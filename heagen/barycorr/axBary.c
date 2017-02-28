/* $Id: axBary.c,v 1.10 2013/12/02 20:28:26 craigm Exp $ */
/*--------------------------------------------------------------------
 *
 *    File:        axBary.c
 *    Programmer:  Arnold Rots, ASC
 *    Date:        1 September 1998
 *
 *  axBary is a multi-mission function that applies barycenter
 *  corrections to HFWG-compliant FITS files.
 *
 *  Function call:
 *    int axBary (char *inFile, char *orbitFile, char *outfile,
 *                double ra, double dec, char *refFrame, int debug)
 *
 *  Input:
 *    inFile    char*   Path of file to be corrected
 *    orbitFile char*   Path of orbit ephemeris file
 *    outFile   char*   Path of output file to be created
 *    ra        double  RA to be used for barycenter corrections
 *    dec       double  Dec to be used for barycenter corrections
 *                      If either ra or dec is out of bounds (0 -> 360,
 *                      -90 -> +90, respectively), RA_PNT and DEC_PNT
 *                      are used; if either is not available, RA_NOM
 *                      and DEC_NOM are used; if either of those is not
 *                      available, an error condition results
 *                      J2000 is required
 *    refFrame  char*   Reference frame to be used (FK5 or ICRS)
 *    debug     int     Debug message switch
 *
 *  Return:
 *    0: success; otherwise: error
 *
 *  Description:
 *    It copies inFile into outFile, applying barycenter corrections to all
 *    times, using orbit ephemeris file orbitFile, ra, dec, and refFrame "FK5"
 *    or "ICRS" (default).  If outFile = inFile or *outFile=0 or *inFile=0,
 *    no copying will take place and all correction will be made in situ.
 *    Specifically:
 *    
 *    In all headers in the file, the following keywords will be updated:
 *    
 *      TIMESYS   'TDB'
 *      TIMEREF   'SOLARSYSTEM'
 *      RA_NOM    ra
 *      DEC_NOM   dec
 *      RADECSYS  refFrame
 *      PLEPHEM   JPL ephemeris used
 *      TSTART    barycenter-corrected 
 *      TSTOP     barycenter-corrected 
 *      DATE-OBS  barycenter-corrected 
 *      DATE-END  barycenter-corrected 
 *      TIERRELA  relative clock error
 *      TIERABSO  absolute clock error
 *      TIMEZERO  set to 0.0; previous value will be incorporated in times
 *      CREATOR
 *      DATE
 *    
 *    In all tables, the values in columns named 'time' (case-insensitive)
 *    will be barycenter-corrected.
 *    
 *    In all tables with EXTNAME 'GTI', the values in in columns named
 *    'START' and 'STOP' will be barycenter-corrected.
 *
 *    If available, fine clock corrections will be applied.
 *    
 *    It requires the following packages:
 *      cfitsio
 *      bary (mine)
 *    It expects to find reference files (tai-utc.dat, JPLEPH.405,
 *    JPLEPH.200) in at least one of the directories $TIMING_DIR and
 *    $ASC_DATA (in that order).
 *
 *--------------------------------------------------------------------*/

#include "bary.h"
#define NTIMBUF 1000
#define PCACORR -16.0
#define HEXTECORR -1.0

/* KLUDGE!!!!
   This is a heuristic to handle the cases where TIMEZERO is not meant to
   be added to the header keywords.   Such files come out of extractor.
   
   The heuristic is small (less than one tenth of TSTART), then
   assume we should be adding TIMEZERO.  Otherwise, consider
   TIMEZERO to be "big" meaning that it should not be added to
   header keywords (but do always add it to the TIME column).
*/
double timezero_heuristic_function(double timezero, double tstart)
{
  if (timezero < 0.1*tstart) { /* Small TIMEZERO */
    return (timezero);
  }
  /* Large TIMEZERO: we don't want to subtract it */
  return (0);
}


char *xbProgName = "hdaxbary" ;

int axBary (char *inFile, char *orbitFile, char *outFile,
	    double ra, double dec, char *refFrame, int debug,
	    char *clockFile, double dtmaxclock)
{

  char *version = "axBary - $Revision: 1.10 $ $Date: 2013/12/02 20:28:26 $" ;
  char *toTDB = "TDB" ;  /* could give user option of TCB, but be careful,
			  * the radio database assumes TDB */
  fitsfile *fits_file = NULL ;
  char instrument[80] = "" ;
  char timeunit[16], plephem[16], timesys[16], timeref[16], radecsys[16],
    missionk[32], extname[32], dateobs[32], dateend[32], crdate[32] ;
  char frame[16] ;
  char line[1025] ;
  double tstart, tstop, ranom, decnom, time[NTIMBUF], dir[3] ;
  double timezero, timezero_hk;
  double equinox, met2sec ;
  double clockcorr, clockcorr0, t ;
  MJDTime mtstart, mtstop ;
  int i, j, k, l, m, n ;
  int hdu, error=0, htype ;
  int princhdu = 0 ;
  int convdir = 0 ;
  int ephnum=0, denum ;
  int ncol, col[10] ;
  long nrows ;
  char keystrval[80] ;
  char comment[80] ;
  double mjdrefi = 0.0 ;
  double mjdreff = 0.0 ;
  double *nv = NULL ;
  double tierrela = -1.0 ;
  double tierabso = -1.0 ;

  /*  Keyword Comments  */
  char *comtstart = "Elapsed seconds since MJDREF at start of file" ;
  char *comtstop  = "Elapsed seconds since MJDREF at end of file" ;
  char *comdateob = "Date and time (TIMESYS) at start of file" ;
  char *comdatend = "Date and time (TIMESYS) at end of file" ;
  char *comtimref = "Times are pathlength-corrected to barycenter" ;
  char *comtimsys = "All times in this file are TDB" ;
  char *commjdref = "TDB time reference point; Modified Julian Day" ;
  char *commjdrfi = "TDB time reference; Modified Julian Day (int)" ;
  char *commjdrff = "TDB time reference; Modified Julian Day (frac)" ;
  char *comtime   = "Time elapsed since MJDREF" ;
  char *comranom  = "Right Ascension used for barycenter corrections" ;
  char *comdecnom = "Declination used for barycenter corrections" ;
  char *comrefsys = "Coordinate Reference System" ;
  char *compleph  = "Solar system ephemeris used for baryctr corr." ;
  char *comdate   = "Date and time (UTC) of file creation" ;
  char *comtierre = "Short-term clock stability" ;
  char *comtierab = "Absolute precision of clock correction" ;

  void errexit (int, fitsfile*, char*, int) ;

  /*
   *   ----------------------
   * -- Copy input to output --
   *   ----------------------
   */
  if ( ( (*inFile) * (*outFile) ) && strcmp(inFile, outFile) ) {
    sprintf (line, "cp %s %s", inFile, outFile) ;
    if ( system (line) ) {
      fprintf (stderr,"%s: Could not open input file %s or create output file %s\n",
	       xbProgName, inFile, outFile) ;
      exit (1) ;
    }
  }
  else if ( !(*outFile) )
      outFile = inFile ;
  /* Just to make sure ... */
  sprintf (line, "chmod u+w %s", outFile) ;
    if ( system (line) ) {
      fprintf (stderr,"%s: Could not make output file %s writable\n",
	       xbProgName, outFile) ;
      exit (1) ;
    }

  if ( *refFrame )
    strncpy (frame, refFrame, 5) ;
  else
    *frame = 0 ;
  strcpy (timesys, "UTC") ;

  /*
   *   ------------------
   * -- Open output file --
   *   ------------------
   */
  if ( fits_open_file (&fits_file, outFile, READWRITE, &error) ) {
    fprintf (stderr,"%s: Failed to re-open output file %s\n",
	     xbProgName, outFile) ;
    errexit (error, fits_file, outFile, debug) ;
  }

  /*
   *   --------------------
   * -- Find principal HDU --
   *   --------------------
   */
  fits_read_key (fits_file, TINT, "NAXIS", &n, comment, &error) ;
  if ( !n ) {
    princhdu++ ;
    fits_movabs_hdu (fits_file, 2, &htype, &error) ;
  }

  /*
   *   ---------------------------
   * -- Get RA and Dec sorted out --
   *   ---------------------------
   */
  fits_read_key   (fits_file, TSTRING,  "MISSION", missionk, comment, &error) ;
  if ( error ) {
    error = 0 ;
    fits_read_key (fits_file, TSTRING, "TELESCOP", missionk, comment, &error) ;
    if ( error ) {
      strcpy (missionk, "UNKNOWN") ;
      error = 0 ;
    }
  }
  fits_read_key   (fits_file, TSTRING,"INSTRUME",instrument, comment, &error) ;
  if ( error ) {
    strcpy (instrument, "UNKNOWN") ;
    error = 0 ;
  }
  if ( !strcmp(missionk, "XTE") ) {
    mission = XTE ;
    mjdrefi = 49353 ;
    mjdreff = 0.000696574074 ;
    strcpy (timesys, "TT") ;
  }
  else if ( !strcmp(missionk, "AXAF") ) {
    mission = AXAF ;
    mjdrefi = 50814 ;
    mjdreff = 0.0 ;
    strcpy (timesys, "TT") ;
  }
  else if ( !strcmp(missionk, "CHANDRA") ) {
    mission = AXAF ;
    mjdrefi = 50814 ;
    mjdreff = 0.0 ;
    strcpy (timesys, "TT") ;
  }
  else if ( !strcmp(missionk, "SWIFT") ) {
    mission = SWIFT ;
    mjdrefi = 51910 ;
    mjdreff = 0.0007428703704 ;
    strcpy (timesys, "TT") ;
  }
  else if ( !strcmp(missionk, "NuSTAR") ) {
    mission = NuSTAR ;
    mjdrefi = 55197 ;
    mjdreff = 0.00076601852 ;
    strcpy (timesys, "TT") ;
  }

  fits_read_key   (fits_file, TDOUBLE, "TIERRELA",&tierrela, comment, &error) ;
  if ( error ) {
    tierrela = 0.000000001 ;
    error = 0 ;
  }
  fits_read_key   (fits_file, TDOUBLE, "TIERABSO",&tierabso, comment, &error) ;
  if ( error ) {
    tierabso = 4.0 ;
    error = 0 ;
  }

  /*  If RA and Dec have to come from the FITS header ...  */
  if ( ( ra < 0.0 ) || ( ra > 360.0) || ( dec < -90.0 ) || ( dec > 90.0 ) ) {
    fits_read_key   (fits_file, TDOUBLE,  "RA_NOM",  &ranom, comment, &error) ;
    fits_read_key (fits_file, TDOUBLE, "DEC_NOM", &decnom, comment, &error) ;
    if ( error ) {
      error = 0 ;
      mjdrefi = 0.0 ;
      fits_read_key (fits_file, TDOUBLE,  "RA_PNT",  &ranom, comment, &error) ;
      fits_read_key (fits_file, TDOUBLE, "DEC_PNT", &decnom, comment, &error) ;
    }

    /*  ... then we also need EQUINOX and reference frame and check consistency  */
    fits_read_key  (fits_file, TDOUBLE, "EQUINOX", &equinox, comment, &error) ;
    if ( error || ( equinox != 2000.0 ) ) {
      fits_close_file (fits_file, &error) ;
      fprintf (stderr, "===> No barycenter correction applied to file %s\n",
	       inFile) ;
      fprintf (stderr, "     Could not find a sensible RA and Dec\n") ;
      errexit (1, fits_file, outFile, debug) ;
    }

    fits_read_key (fits_file, TSTRING, "RADECSYS", radecsys, comment, &error) ;
    if ( !error && !strcmp(frame,"ICRS") && !strcmp(radecsys,"FK5") )
      convdir = 1 ;
    else if ( error || ( ( strcmp (frame, radecsys) ) && *refFrame ) ) {
      fits_close_file (fits_file, &error) ;
      fprintf (stderr, "===> No barycenter correction applied to file %s\n",
	       inFile) ;
      fprintf (stderr, "     RADECSYS (%s) does not agree with\n", radecsys) ;
      fprintf (stderr, "     requested reference frame (%s)\n", frame) ;
      errexit (1, fits_file, outFile, debug) ;
    }
    else if ( !frame[0] )
      strcpy (frame, radecsys) ;
    ra = ranom ;
    dec = decnom ;
  }
  else {
    if ( !frame[0] )
      strcpy (frame, "ICRS") ;
    strcpy (radecsys, frame) ;
  }

  /*  Set up for correct JPL ephemeris and set direction cosines  */
  if ( !strcmp (frame, "ICRS") )
    ephnum = 405 ;
  else if ( !strcmp (frame, "FK5") )
    ephnum = 200 ;
  else {
    fits_close_file (fits_file, &error) ;
    fprintf (stderr, "===> No barycenter correction applied to file %s\n",
	     inFile) ;
    fprintf (stderr, "     Invalid reference frame: %s\n", frame) ;
    errexit (3, fits_file, outFile, debug) ;
  }
  dir[0] = cos(ra/RADEG) * cos(dec/RADEG) ;
  dir[1] = sin(ra/RADEG) * cos(dec/RADEG) ;
  dir[2] = sin(dec/RADEG) ;
  if ( convdir )
    c200to405 (dir, debug) ;

  /*
   *   ------------------
   * -- Clock correction --
   *   ------------------
   */
  fits_read_key   (fits_file, TDOUBLE,  "TSTART",   &tstart, comment, &error) ;
  fits_read_key   (fits_file, TDOUBLE,   "TSTOP",    &tstop, comment, &error) ;
  if ( error ) {
    fprintf (stderr, "===> No barycenter correction applied to file %s\n",
	     inFile) ;
    fprintf (stderr, "     Error reading TSTART/TSTOP in principal HDU\n") ;
    errexit (error, fits_file, outFile, debug) ;
  }
  t = 0.5 * (tstart + tstop) ;
  if ( debug )
    fprintf (stderr, "%s: Principal HDU  -  TIERRELA = %f, TIERABSO = %f\n",
	     xbProgName, tierrela, tierabso) ;
  fits_read_key   (fits_file, TDOUBLE,"TIMEZERO", &timezero, comment, &error) ;
  if ( error > 0 ) {
    timezero = 0.0 ;
    error = 0 ;
  }
  timezero_hk = timezero_heuristic_function(timezero, tstart);
  if (timezero != 0 && timezero_hk == 0) {
    fprintf(stderr, 
	    "NOTE: TIMEZERO keyword is large; \n"
	    "   ==> not adding it header keyword values\n");
  }

  clockinit (mission, clockFile) ;
  if ( debug )
    fprintf (stderr, "%s: Calling clockCorr for t = %24.15f, timezero = %f\n",
	     xbProgName, t, timezero_hk) ;
  clockcorr = clockcorr0 = clockCorr (t, timezero_hk, &tierabso, instrument) - timezero_hk ;
  if ( debug )
    fprintf (stderr, "%s: Clock correction %f -  TIERABSO = %f\n",
	     xbProgName, clockcorr, tierabso) ;

  /*
   *   ----------------------
   * -- Go back to first HDU --
   *   ----------------------
   */
  hdu = -1 ;

  /*
   *   ----------------
   * -- Loop over HDUs --
   *   ----------------
   */
  while ( !error ) {
    hdu++ ;
    fits_movabs_hdu (fits_file, hdu+1, &htype, &error) ;
    if ( error ) {
      error = 0 ;
      fprintf (stderr, "<=== Finished after %d HDUs\n", hdu) ;
      break ;
    }
    fits_read_key   (fits_file, TSTRING,  "MISSION", missionk, comment, &error) ;
    if ( error ) {
      error = 0 ;
      fits_read_key (fits_file, TSTRING, "TELESCOP", missionk, comment, &error) ;
      if ( error ) {
	strcpy (missionk, "UNKNOWN") ;
	error = 0 ;
      }
    }
    if ( !strcmp(missionk, "XTE") )
      mission = XTE ;
    else if ( !strcmp(missionk, "AXAF") )
      mission = AXAF ;
    else if ( !strcmp(missionk, "SWIFT") ){
      mission = SWIFT ;
    } else if ( !strcmp(missionk, "NuSTAR") ){
      mission = NuSTAR ;
    }

  /*
   *   ----------------------------------
   * -- Get essential timing information --
   *   ----------------------------------
   */

    fits_read_key (fits_file, TSTRING,  "TIMESYS",  timesys, comment, &error) ;
    error = 0 ;
    if ( strcmp(timesys,"TT") * strcmp(timesys,"TDT") * strcmp(timesys,"TAI")
	 * strcmp(timesys,"ET") * strcmp(timesys,"UTC") ) {
      fprintf (stderr, "===> No barycenter correction applied to HDU %d\n",
	       hdu) ;
      fprintf (stderr, "     TIMESYS is %s\n", timesys) ;
      continue ;
    }
    fits_read_key (fits_file, TSTRING, "TIMEUNIT", timeunit, comment, &error) ;
    if ( error ) {
      strcpy (timeunit, "s") ;
      error = 0 ;
    }
    if ( *timeunit == 'd' )
      met2sec = SECDAY ;
    else
      met2sec = 1.0 ;
    fits_read_key   (fits_file, TDOUBLE, "MJDREFI",  &mjdrefi, comment, &error) ;
    if ( error ) {
      error = 0 ;
      fits_read_key (fits_file, TDOUBLE, "MJDREF",   &mjdreff, comment, &error) ;
      if ( error )
	error = 0 ;
      else {
	mjdrefi = (int) mjdreff ;
	mjdreff -= mjdrefi ;
      }
    }
    else {
      fits_read_key (fits_file, TDOUBLE, "MJDREFF",  &mjdreff, comment, &error) ;
      if ( error ) {
	mjdreff = 0.0 ;
	error = 0 ;
      }
    }
    fits_read_key   (fits_file, TDOUBLE,"TIMEZERO", &timezero, comment, &error) ;
    if ( error > 0 ) {
      timezero = 0.0 ;
      error = 0 ;
    }
    timezero_hk = timezero_heuristic_function(timezero, tstart);
    if (debug) 
      fprintf(stderr,"%s: added clockcorr (%f) to timezero => %f\n",
	      xbProgName,clockcorr,timezero+clockcorr0);

  /*
   *   -----------------
   * -- Initialize bary --
   *   -----------------
   */
    denum = baryinit (mission, timesys, toTDB, mjdrefi, mjdreff, timeunit,
		      ephnum) ;
    if ( denum ) {
      printf ("%s: Using JPL Planetary Ephemeris DE-%d\n", xbProgName, denum) ;
      if ( debug )
	fprintf (stderr, "%s: bary stuff initialized in HDU %d\n",
		 xbProgName, hdu) ;
    }
    else {
      fprintf (stderr, "%s: Could not initialize bary stuff in HDU %d\n",
	       xbProgName, hdu) ;
      error = 0 ;
      continue ;
    }
    sprintf (plephem, "JPL-DE%03d", denum) ;

  /*
   *   ------------------------------
   * -- Get other header information --
   *   ------------------------------
   */

    /*  Tstart and Tstop  */
    fits_read_key   (fits_file, TDOUBLE,  "TSTART",   &tstart, comment, &error) ;
    fits_read_key   (fits_file, TDOUBLE,   "TSTOP",    &tstop, comment, &error) ;
    if ( error ) {
      fprintf (stderr, "%s: Warning - error reading TSTART/TSTOP in HDU %d\n",
	       xbProgName, hdu) ;
      error = 0 ;
      /*      continue ; */
    }
    else {
      if ( debug )
	fprintf (stderr, "%s: HDU %d TSTART = %f, TSTOP = %f\n",
		 xbProgName, hdu, tstart, tstop) ;
      if (dtmaxclock == 0) {
	/* Old behavior: use one clock correction calculation for everything */
	tstart += timezero_hk + clockcorr0;
	tstop  += timezero_hk + clockcorr0;
      } else {
	/* Calculate clock offset fresh each time.  Note that
	   clockCorr adds timezero+clockcorr so it is not necessarily
	   to add timezero yet again. */
	tstart += clockCorr(tstart+timezero_hk, timezero_hk, &tierabso, instrument);
	tstop  += clockCorr(tstop +timezero_hk, timezero_hk, &tierabso, instrument);
      }
	
      if ( debug )
	fprintf (stderr, "%s: HDU %d TSTART = %f, TSTOP = %f (corrected for timezero)\n",
		 xbProgName, hdu, tstart, tstop) ;
      tstart += xbarycorr(tstart, dir, xscorbit(orbitFile, tstart, &error)) / met2sec ;
      tstop  += xbarycorr(tstop,  dir, xscorbit(orbitFile, tstop,  &error)) / met2sec ;
      if ( debug )
	fprintf (stderr, "%s: HDU %d TSTART = %f, TSTOP = %f (corrected to barycenter)\n",
		 xbProgName, hdu, tstart, tstop) ;
      if ( error ) {
	fprintf (stderr, "%s: Error %d correcting TSTART/TSTOP in HDU %d\n",
		 xbProgName, error, hdu) ;
	error = 0 ;
	continue ;
      }
      else {
	met2mjd (tstart, &mtstart) ;
	met2mjd (tstop, &mtstop) ;
      }
    }

    /*  Sort out the columns  */
    ncol = 0 ;
    if ( htype == 2 ) {
      fits_read_key (fits_file, TSTRING, "EXTNAME", extname, comment, &error) ;
      fits_read_key (fits_file,   TLONG,  "NAXIS2",  &nrows, comment, &error) ;

      /*  NOT a GTI Binary Table  */
      if ( strcmp(extname, "GTI") ) {
	fits_get_colnum (fits_file, 0,   "time",  col, &error) ;
	if ( error ) {
	  printf ("%s: Could not find a Time column in HDU %d\n", xbProgName, hdu) ;
	  error = 0 ;
	}
	else
	  ncol = 1 ;
	if ( (!strcmp(extname, "BADPIX"))&&(mission == NuSTAR) ) {
	  /* NuSTAR column "TIME_STOP" to be corrected */
	   fits_get_colnum (fits_file, 0,  "time_stop",  col+ncol, &error) ;
	   if ( error ) {
	     printf ("%s: Could not find a TIME_STOP column in HDU %d\n", xbProgName, hdu) ;
	     error = 0 ;
	   }
	   else
	     ncol++ ;
	}
      }

      /*  GTI  */
      else {
	fits_get_colnum (fits_file, 0, "start*",  col, &error) ;
	if ( error ) {
	  printf ("%s: Could not find a Start column in HDU %d\n", xbProgName, hdu) ;
	  error = 0 ;
	}
	else
	  ncol = 1 ;
	fits_get_colnum (fits_file, 0,  "stop*",  col+ncol, &error) ;
	if ( error ) {
	  printf ("%s: Could not find a Stop column in HDU %d\n", xbProgName, hdu) ;
	  error = 0 ;
	}
	else
	  ncol++ ;
      }
    }

  /*
   *   ------------
   * -- Fix header --
   *   ------------
   */
    strcpy (dateobs, convertTime(&mtstart)) ;
    strcpy (dateend, convertTime(&mtstop)) ;
    fits_modify_key_str (fits_file, "TIME-OBS", dateobs+11,         "&", &error) ;
    error = 0 ;
    fits_modify_key_str (fits_file, "TIME-END", dateend+11,         "&", &error) ;
    error = 0 ;
    fits_modify_key_dbl (fits_file,   "TSTART",  tstart,15,   comtstart, &error) ;
    error = 0 ;
    fits_modify_key_dbl (fits_file,    "TSTOP",   tstop,15,    comtstop, &error) ;
    error = 0 ;
    fits_update_key_str (fits_file, "DATE-OBS", dateobs,      comdateob, &error) ;
    fits_update_key_str (fits_file, "DATE-END", dateend,      comdatend, &error) ;
    fits_update_key_dbl (fits_file, "TIMEZERO",     0.0, 6,         "&", &error) ;
    fits_update_key_str (fits_file,  "TIMESYS",   toTDB,      comtimsys, &error) ;
    fits_update_key_dbl (fits_file,   "RA_OBJ",      ra, 8,    comranom, &error) ;
    fits_update_key_dbl (fits_file,  "DEC_OBJ",     dec, 8,   comdecnom, &error) ;
    fits_update_key_str (fits_file,  "TIMEREF","SOLARSYSTEM", comtimref, &error) ;
    fits_update_key_str (fits_file,  "TREFPOS","BARYCENTER",  
			 "Time reference position", &error) ;
    fits_update_key_str (fits_file,  "TREFDIR","RA_OBJ,DEC_OBJ",
			 "Keywords of reference direction", &error);
    fits_update_key_str (fits_file, "RADECSYS",radecsys,      comrefsys, &error) ;
    fits_update_key_str (fits_file,  "PLEPHEM", plephem,       compleph, &error) ;
    if ( tierrela > 0.0 )
      fits_update_key_dbl(fits_file,"TIERRELA",tierrela, 6,   comtierre, &error) ;
    if ( tierabso > 0.0 )
      fits_update_key_dbl(fits_file,"TIERABSO",tierabso, 6,   comtierab, &error) ;
    fits_modify_comment (fits_file,   "MJDREF",               commjdref, &error) ;
    error = 0 ;
    fits_modify_comment (fits_file,  "MJDREFI",               commjdrfi, &error) ;
    error = 0 ;
    fits_modify_comment (fits_file,  "MJDREFF",               commjdrff, &error) ;
    error = 0 ;
    if ( ncol == 1 ) {
      sprintf (keystrval, "TTYPE%d", col[0]) ;
      fits_modify_comment (fits_file, keystrval,                comtime, &error) ;
      error = 0 ;
    }
    if ( error ) {
      fprintf (stderr, "===> No barycenter correction applied to HDU %d\n",
	       hdu) ;
      fprintf (stderr, "     Error occurred while updating header\n", timesys) ;
      error = 0 ;
      continue ;
    }

  /*
   *   --------------------
   * -- Fix time column(s) --
   *   --------------------
   */
    for (n=0; n<ncol; n++) {
      double old_time = TIMENULLVAL;
      double old_clockcorr = clockcorr0 + timezero;
      j = 0 ;
      while ( j < nrows ) {
	k = j + NTIMBUF ;
	if ( k > nrows )
	  k = nrows ;
	m = k - j ;
	j++ ;

	/* Zero values for debugging sanity */
	for (i=0; i<NTIMBUF; i++) time[i] = 0;

	/*  Read  */
	fits_read_col  (fits_file, TDOUBLE, col[n], j, 1, m, nv, time, &i, &error) ;
	if ( error ) {
	  fprintf (stderr, "===> Error while reading rows %d - %d of HDU %d (column %d)\n",
		   j, k, hdu, col[n]) ;
	  errexit (error, fits_file, outFile, debug) ;
	}

	/*  Apply barycenter correction */

	for (i=0; i<m; i++) {
	  /* Decide whether to recalculate clock offset value.
	     If dtmaxclock is non-zero AND:
   	       1. First time through (oldtime == TIMENULLVAL) OR
	       2. Time values differ by more than dtmaxclock OR
	       3. Time value crosses day boundary 
	          (recalculate to capture leapsecond rollover)

	     If dtmaxclock is zero, then use value computed at 
	     center time, which was the default for previous versions
	     of axBary.
	  */

	  if ( dtmaxclock == 0 ) {
	    /* Old behavior: If dtmaxclock == 0 then we use the clock
	       correction from the center of the observation (for
	       backward compatibility) */
	    time[i] += timezero + clockcorr0;

	  } else if ((old_time == TIMENULLVAL) ||
		     (fabs(time[i] - old_time) >= dtmaxclock) ||
		     (floor(time[i]/86400) != floor(old_time/86400))) {
	    /* Criteria met, calculate new clock offset.  Note that
	       clockCorr returns timezero+clockcorr so there is no
	       need to add timezero in again */

	    /* Reset the first down line */
	    old_clockcorr = clockCorr(time[i]+timezero, timezero, &tierabso, instrument);
	    /* printf("clockCorr(%f) = %f (TIMEZERO=%f)\n", time[i]+timezero, old_clockcorr, timezero); */
	    old_time = time[i];

	    /* printf("  Time[%d] BEFORE = %f    AFTER = %f\n",
	       i, time[i], time[i] + old_clockcorr); */
	    time[i] += old_clockcorr;
	  } else {
	    /* Criteria not met, so use old correction */
	    time[i] += old_clockcorr;
	  }

	  /* Calculate barycenter component */
	  time[i] += xbarycorr(time[i], dir, xscorbit(orbitFile, time[i], &error))
	             / met2sec ;
	  if ( error ) {
	    fprintf (stderr, "===> Error while correcting row %d of HDU %d (column %d)\n",
		     j+i, hdu, col[n]) ;
	    errexit (error, fits_file, outFile, debug) ;
	  }
	}

	/*  Write  */
	fits_write_col (fits_file, TDOUBLE, col[n], j, 1, m,     time,     &error) ;
	if ( error ) {
	  fprintf (stderr, "===> Error while writing rows %d - %d of HDU %d\n",
		   j, k, hdu) ;
	  errexit (error, fits_file, outFile, debug) ;
	}
	j = k ;
      }
    }

  /*
   *   --------------------------------------
   * -- Fix creator, date, history, checksum --
   *   --------------------------------------
   */
    strcpy (crdate, fitsdate()) ;
    fits_update_key_str (fits_file,  "CREATOR", version,            "&", &error) ;
    fits_update_key_str (fits_file,     "DATE",  crdate,        comdate, &error) ;
    if ( hdu == princhdu ) {
      i = 0 ;
      sprintf (line, "TOOL :%-56.56sASC%04d", version, i) ;
      fits_write_history (fits_file, line, &error) ;
      i++ ;
      if ( strlen (version) > 56 ) {
	sprintf (line, "CONT :%-56.56sASC%04d", version+56, i) ;
	fits_write_history (fits_file, line, &error) ;
	i++ ;
      }
      sprintf (line, "PARM :infile=%-49.49sASC%04d", inFile, i) ;
      fits_write_history (fits_file, line, &error) ;
      i++ ;
      if ( strlen (inFile) > 49 ) {
	sprintf (line, "CONT :%-56.56sASC%04d", inFile+49, i) ;
	fits_write_history (fits_file, line, &error) ;
	i++ ;
      }
      sprintf (line, "PARM :outfile=%-48.48sASC%04d", outFile, i) ;
      fits_write_history (fits_file, line, &error) ;
      i++ ;
      if ( strlen (inFile) > 49 ) {
	sprintf (line, "CONT :%-56.56sASC%04d", outFile+48, i) ;
	fits_write_history (fits_file, line, &error) ;
	i++ ;
      }
    }
    fits_write_chksum   (fits_file,                                      &error) ;
    if ( error ) {
      fprintf (stderr, "===> Barycenter correction applied to HDU %d\n",
	       hdu) ;
      fprintf (stderr, "     Error occurred while closing header\n", timesys) ;
      error = 0 ;
    }

  /*
   *   --------------------------
   * -- End HDU loop, close shop --
   *   --------------------------
   */
  }
  fits_close_file (fits_file, &error) ;

  return (error) ;
}

/*           Error handling on exit    */
void errexit (int error, fitsfile *fits_file, char *outFile,
	      int debug)
{
  int ierr ;
  char line[1024] ;

  if ( fits_file )
    fits_close_file (fits_file, &ierr) ;
  if ( debug )
    fprintf (stderr, "%s: Error exit (error %d); output file %s NOT deleted\n",
	     xbProgName, error, outFile) ;
  else {
    sprintf (line, "rm %s", outFile) ;
    if ( system (line) )
      fprintf (stderr, "%s: Error exit (error %d); error while deleting output file %s\n",
	       xbProgName, error, outFile) ;
    else
    fprintf (stderr, "%s: Error exit (error %d); output file %s deleted\n",
	     xbProgName, error, outFile) ;
  }
  exit (error) ;
}
