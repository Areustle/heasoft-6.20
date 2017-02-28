/* $Id: scorbit.c,v 1.10 2012/10/17 19:46:45 craigm Exp $ */
/*------------------------------------------------------------------
 *
 *  File:        scorbit.c
 *  Programmer:  Arnold Rots, SAO/ASC
 *  Date:        2 September 1998
 *
 *  A set of functions that provides orbit positions for the
 *  the bary package.
 *
 *  baryinit will call scorbitinit.  After that, spacecraft positions
 *  will be returned by a call to scorbit or xscorbit:
 *    double *scorbit (char *filename, MJDTime *time, int *oerror)
 *    double *xscorbit (char *filename, double time, int *oerror)
 *
 *  To add support for another mission:
 *    Add the mission to the switch statement in clockCorr.
 *    Add a function to calculate its clock correction.
 *    Don't forget to provide support for it in bary.h and clock.c!
 *
 *-----------------------------------------------------------------*/

#include "bary.h"

void scorbitinit (enum Observatory obs)
/*------------------------------------------------------------------
 *
 *  A function that to initialize the spacecraft orbit package.
 *  It sets the mission parameter.
 *
 *  Input:
 *    mission      Observatory Mission, observatory, telescope
 *
 *-----------------------------------------------------------------*/
{
  mission = obs ;
  return ;
}

/*-----------------------------------------------------------------------
 *
 *  scorbit reads the orbit ephemeris from file OE and interpolates the
 *  spacecraft position at time t; the pointer to an array with these
 *  coordinates is returned.
 *  If t falls outside the range covered by the file, a non-zero value
 *  is returned in oerror.
 *
 *  This is the generic MJDTime version that passes control to the version
 *  appropriate for the value of mission.
 *
 *----------------------------------------------------------------------*/

double *scorbit (char *filename, MJDTime *time, int *oerror)
{
  static double scposn0[3] = {0.0, 0.0, 0.0} ;

  if ( *oerror )
    return NULL ;

  switch ( mission ) {
  case Geocenter:
    return scposn0 ;
  case XTE:
  case AXAF:   /* Same format as XTE */
    return xtescorbit (filename, mjd2met(time), oerror) ;
  case NuSTAR: 
    return nustarscorbit (filename, mjd2met(time), oerror) ;
  case SWIFT:
    return swiftscorbit (filename, mjd2met(time), oerror) ;
  case Unknown:
    *oerror = -11 ;
    return NULL ;
  default:
    *oerror = -12 ;
    return NULL ;
  }
}

/*-----------------------------------------------------------------------
 *
 *  xscorbit reads the orbit ephemeris from file OE and interpolates the
 *  spacecraft position at time t; the pointer to an array with these
 *  coordinates is returned.
 *  If t falls outside the range covered by the file, a non-zero value
 *  is returned in oerror.
 *
 *  This is the generic MET version that passes control to the version
 *  appropriate for the value of mission.
 *
 *----------------------------------------------------------------------*/

double *xscorbit (char *filename, double time, int *oerror)
{
  static double scposn0[3] = {0.0, 0.0, 0.0} ;

  if ( *oerror )
    return NULL ;

  switch ( mission ) {
  case Geocenter:
    return scposn0 ;
  case XTE:
  case AXAF:   /* Same format as XTE */
    return xtescorbit (filename, time, oerror) ;
  case NuSTAR: 
    return nustarscorbit (filename, time, oerror) ;
  case SWIFT:
    return swiftscorbit (filename, time, oerror) ;
  case Unknown:
    *oerror = -11 ;
    return NULL ;
  default:
    *oerror = -12 ;
    return NULL ;
  }
}

/*-----------------------------------------------------------------------
 *
 *  xtescorbit reads the orbit ephemeris from file OE and interpolates the
 *  spacecraft position at time t; the pointer to an array with these
 *  coordinates is returned.
 *  If t falls outside the range covered by the file, a non-zero value
 *  is returned in oerror.
 *
 *  XTE and AXAF version
 *
 *----------------------------------------------------------------------*/

double *xtescorbit (char *filename, double t, int *oerror)
{
  static double sctime[NOEROWS] ;
  static double scposn[3][NOEROWS] ;
  static double scvel[3][NOEROWS] ;
  int htype, any ;
  static double intposn[3] ;
  static fitsfile *OE ;
  static char savefile[256]= " " ;
  int i, j ;
  double scacc, dt, dt2 ;
  static int k ;
  char line[256] ;
  static int startrow ;
  static long numrows ;
  static int nbrows ;
  int startinx, relinx ;
  static double deltat, tstart, tstop ;
/*
 *  Initialize ----------------------------------------------------------
 */
  if ( *oerror )
    return NULL ;

  *oerror = 0 ;
  for (j=0; j<3; j++)
    intposn[j] = 0.0 ;

  if ( strcmp (savefile, filename) ) {
    startrow = 0 ;
    nbrows = 0 ;
    if ( *savefile != ' ' )
      fits_close_file (OE, oerror) ;
    strcpy (savefile, " ") ;
    if ( fits_open_file (&OE, filename, 0, oerror) )
      fprintf(stderr, "xscorbit: Cannot open file %s\n", filename) ;
    else {
      fits_movabs_hdu (OE, 2, &htype, oerror) ;
      if ( !(*oerror) ) {
	fits_read_key (OE, TDOUBLE, "DELTAT", &deltat, line, oerror) ;
	if ( *oerror ) {
	  *oerror = 0 ;
	  fits_read_key (OE, TDOUBLE, "TIMEDEL", &deltat, line, oerror) ;
	}
      }
      fits_read_key (OE, TDOUBLE, "TSTART", &tstart, line, oerror) ;
      fits_read_key (OE, TDOUBLE, "TSTOP", &tstop, line, oerror) ;
      fits_read_key (OE, TLONG, "NAXIS2", &numrows, line, oerror) ;
      if ( *oerror )
	fits_close_file (OE, oerror) ;
      else
	strcpy (savefile, filename) ;
    }
    if ( *oerror )
      return NULL ;
  }

/*
 *  See whether orbit file is appropriate --------------------------------
 */
  startinx = (t - tstart) / deltat + 0.5 ;
  if ( ( startinx <= 0 ) || ( startinx > numrows-2 ) ) {
    fprintf(stderr, "xscorbit: Time %.9g not covered by file %s\n", t, filename) ;
    *oerror = -2 ;
    return NULL ;
  }

/*
 *  Read records from orbit file -----------------------------------------
 */
  relinx = startinx - startrow ;
  if ( !nbrows || !startrow || ( relinx > nbrows-3 ) || (relinx < 0 ) ) {
    startrow = startinx ;
    nbrows = ((startrow + NOEROWS) <= numrows) ? NOEROWS : (numrows - startrow) ;
    fits_read_col (OE, TDOUBLE, 1, startrow, 1, nbrows, 0,
		   sctime, &any, oerror) ;
    for (i=0; i<3; i++) {
      fits_read_col (OE, TDOUBLE, i+2, startrow, 1, nbrows, 0,
		     &scposn[i][0], &any, oerror) ;
      fits_read_col (OE, TDOUBLE, i+5, startrow, 1, nbrows, 0,
		     &scvel[i][0], &any, oerror) ;
    }
    if ( *oerror ) {
      fprintf (stderr, "xscorbit: error while reading rows starting at %d\n",
	       startrow) ;
      return NULL ;
    }
    relinx = startinx - startrow ;
  }

/*
 *  Interpolate position ------------------------------------------------
 */
    dt = t - sctime[relinx+1] ;
    dt2 = 0.5 * dt * dt ;
    for (j=0; j<3; j++) {
      scacc = (scvel[j][relinx+2] - scvel[j][relinx]) /
	(sctime[relinx+2] - sctime[relinx]) ;
      intposn[j] = scposn[j][relinx+1] + dt * scvel[j][relinx+1] + dt2 * scacc ;
    }

/*
 *  Return position -----------------------------------------------------
 */
  return intposn ;
}

/*
 *  swiftscorbit reads the orbit ephemeris from file OE and interpolates the
 *  spacecraft position at time t; the pointer to an array with these
 *  coordinates is returned.
 *  If t falls outside the range covered by the file, a non-zero value
 *  is returned in oerror.
 *
 *  Swift orbit files always have TIMEZERO = 0.0 and so timezero is ignored
 *
 *  Swift orbit files (prefilter output) have position and velocity written
 *  as vector columns with units of km and km/s, respectively. We convert the
 *  position vector to meters just before returning it (after interpolation).
 *
 *  Swift version by M.Tripicco 13May2004
 *  Revised 16March2005 to generalize finding the POSITION/VELOCITY columns
 *
 *----------------------------------------------------------------------*/

double *swiftscorbit (char *filename, double t, int *oerror)
{
  static double sctime[NOEROWS] ;
  static double scposn[3][NOEROWS] ;
  static double scvel[3][NOEROWS] ;
  static double vectmp[3*NOEROWS]; /* used to deal with vector cols */
  int htype, any ;
  static double intposn[3] ;
  static fitsfile *OE ;
  static char savefile[256]= " " ;
  int i, j ;
  double scacc, dt, dt2 ;
/*  static int k ;*/
  char line[256] ;
  static int startrow ;
  static long numrows ;
  static int nbrows ;
  static int nhdus, pcol=0, vcol=0, typecode;
  static long repeat, width;
  static int relinx = -1;
  static double deltat, tstart, tstop ;
  /* Begin modification CM */
  static int gapwarn = 0;
  double diff1 = 0, diff2 = 0, diff3 = 0;
  /* End modification CM */
  /*
   *  Initialize ----------------------------------------------------------
   */
  *oerror = 0 ;
  for (j=0; j<3; j++)
    intposn[j] = 0.0 ;

  if ( strcmp (savefile, filename) ) {
    relinx = -1;  /* Reset relative pointer */
    startrow = 0 ;
    nbrows = 0 ;
    if ( *savefile != ' ' )
      fits_close_file (OE, oerror) ;
    strcpy (savefile, " ") ;
    if ( fits_open_file (&OE, filename, 0, oerror) ){
      fprintf(stderr, "swiftscorbit: Cannot open file %s\n", filename) ;
      return intposn;
    }
    else {
      fits_get_num_hdus (OE, &nhdus, oerror);
      for (j=1; j<=nhdus; j++){
	fits_movabs_hdu (OE, j, &htype, oerror); 
	if (htype != BINARY_TBL) continue;
	fits_get_colnum (OE, CASEINSEN, "POSITION", &pcol, oerror);
	if ( *oerror ) {
	  *oerror = 0;
	  continue;
	}
	fits_get_coltype (OE, pcol, &typecode, &repeat, &width, oerror);
	if (repeat != 3){
	  fprintf(stderr,"swiftscorbit: found POSITION col but not 3-vector..returning intposn\n");
	  return intposn ;
	}
	fits_get_colnum (OE, CASEINSEN, "VELOCITY", &vcol, oerror);
	if ( *oerror == 0) break;
	fits_get_coltype (OE, vcol, &typecode, &repeat, &width, oerror);
	if (repeat != 3){
	  fprintf(stderr,"swiftscorbit: found VELOCITY col but not 3-vector..returning intposn\n");
	  return intposn ;
	}
      }
      /*printf("HDU %d, POSITION is col %d, VELOCITY is col %d\n",j,pcol,vcol);*/
      if ( !(*oerror) ) {
	fits_read_key (OE, TDOUBLE, "DELTAT", &deltat, line, oerror) ;
	if ( *oerror ) {
	  *oerror = 0 ;
	  fits_read_key (OE, TDOUBLE, "TIMEDEL", &deltat, line, oerror) ;
	}
      }
      fits_read_key (OE, TDOUBLE, "TSTART", &tstart, line, oerror) ;
      fits_read_key (OE, TDOUBLE, "TSTOP", &tstop, line, oerror) ;
      fits_read_key (OE, TLONG, "NAXIS2", &numrows, line, oerror) ;
      if ( *oerror )
	fits_close_file (OE, oerror) ;
      else
	strcpy (savefile, filename) ;
    }
    if ( *oerror ){
      fprintf(stderr,"swiftscorbit: problems with %s..returning intposn\n",filename);
      return intposn ;
    }
  }
  if (pcol == 0 || vcol == 0){
    fprintf(stderr,"swiftscorbit: could not find POSITION/VELOCITY 3-vectors in %s\n",filename);
    *oerror = -2;
    return intposn;
  }
  
  /*
   *  Read records from orbit file -----------------------------------------
   */

  /* Begin modification CM */
  if ( !nbrows || !startrow || ( t < sctime[0] ) || (t > sctime[nbrows-1])) {
    /* If we just opened the file, the start scanning from the first row */
    if (startrow == 0) startrow = 1;

    /* Scan the file for a matching time.  Since Swift orbit files can
       have multiple gaps, we have to do a scan */
    while (1) {
      nbrows = ((startrow + NOEROWS) <= numrows) ? NOEROWS : (numrows - startrow) ;
      if (nbrows <= 3) {
	fprintf(stderr, "swiftscorbit: Time %.9g not covered by file %s\n", t, filename) ;
	*oerror = -2 ;
	return intposn ;
      }

      /* fprintf(stdout, "Reading rows %d to %d\n", startrow, startrow+nbrows-1); */
      fits_read_col (OE, TDOUBLE, 1, startrow, 1, nbrows, 0,
		     sctime, &any, oerror) ;

      if (t < sctime[0]) {
	startrow -= (NOEROWS-2);
	if (startrow < 1) { startrow = 1; }
      } else if (t > sctime[nbrows-2]) {
	startrow += (NOEROWS-2);
	if (startrow >= numrows) { startrow = numrows; }
      } else {
	/* We are in the right time range */
	/* fprintf(stdout, "Found (met=%18.2f; torb=%18.2f)!\n",
	   t, sctime[0]); */
	break;
      }

    }
    /* End modification CM */

    relinx = -1;  /* Reset relative pointer */
    for (i=0; i<3; i++) {
      fits_read_col (OE, TDOUBLE, pcol, startrow, i+1, 3*nbrows, 0,
		     &vectmp[0], &any, oerror) ; /* stored as vector columns */
      for (j=0; j<nbrows; j++) scposn[i][j] = vectmp[j*3];
      fits_read_col (OE, TDOUBLE, vcol, startrow, i+1, 3*nbrows, 0,
		     &vectmp[0], &any, oerror) ; /* stored as vector columns */
      for (j=0; j<nbrows; j++) scvel[i][j] = vectmp[j*3];
    }
    if ( *oerror ) {
      fprintf (stderr, "swiftscorbit: error while reading rows starting at %d\n",startrow) ;
      return intposn ;
    }
  }

  /* Begin modification CM */
  /* Locate the proper interpolation position */
  /* First see if old rows are still OK */
  if (relinx >= 0) {
    diff1 = fabs(t-sctime[relinx]);
    diff2 = fabs(t-sctime[relinx+1]);
    diff3 = fabs(t-sctime[relinx+2]);
    if ( ! (diff2 < diff1 && diff2 < diff3) ) { 
      relinx = -1;
    }
  }
  /* If we don't already have a good position, search for it now */
  if (relinx < 0) {
    for (relinx=0; relinx<(nbrows-2); relinx++) {
      diff1 = fabs(t-sctime[relinx]);
      diff2 = fabs(t-sctime[relinx+1]);
      diff3 = fabs(t-sctime[relinx+2]);
      if (diff2 < diff1 && diff2 < diff3) break;
    }
  }

#define MAXGAP 5.0   /* Maximum gap, (data time) - (orbit time), in seconds */
  if (diff2 > MAXGAP) {
    if (gapwarn == 0) {
      fprintf (stderr, "swiftscorbit: warning there is an orbit ephem gap near MET %f\n", t);
    }
    gapwarn = 1;
  } else {
    gapwarn = 0;
  }
  /* End modification CM */

  /*
   *  Interpolate position ------------------------------------------------
   */
  dt = t - sctime[relinx+1] ;
  dt2 = 0.5 * dt * dt ;
  for (j=0; j<3; j++) {
    scacc = (scvel[j][relinx+2] - scvel[j][relinx]) /
      (sctime[relinx+2] - sctime[relinx]) ;
    intposn[j] = scposn[j][relinx+1] + dt * scvel[j][relinx+1] + dt2 * scacc ;
    intposn[j] *= 1000.0; /* convert position vector to meters from km */
  }
  
  /*
   *  Return position -----------------------------------------------------
   */
  return intposn ;
}

/*
 *  nustarscorbit reads the orbit ephemeris from file OE and finds the
 *  spacecraft position nearest to time t; the pointer to an array with these
 *  coordinates is returned.
 *  If t falls outside the range covered by the file, a non-zero value
 *  is returned in oerror.
 *
 *  NuSTAR orbit files always have TIMEZERO = 0.0 and so timezero is ignored
 *
 *  NuSTAR orbit files have position and velocity written
 *  as vector columns with units of km and km/s, respectively. We convert the
 *  position vector to meters just before returning it.
 *
 *  NuSTAR orbit files have no gaps, and DELTAT = 1sec, so to hell with
 *  the interpolation ...
 *
 *  NuSTAR version by A. Davis May 2012
 *
 *----------------------------------------------------------------------*/

double *nustarscorbit (char *filename, double t, int *oerror)
{
  static double sctime[NOEROWS] ;
  static double scposn[3][NOEROWS] ;
  static double scvel[3][NOEROWS] ;
  static double vectmp[3*NOEROWS]; /* used to deal with vector cols */
  int htype, any ;
  static double intposn[3] ;
  static fitsfile *OE ;
  static char savefile[256]= " " ;
  int i, j ;
  double dt ;
  char line[256] ;
  static int startrow ;
  static long numrows ;
  static int nbrows ;
  int startinx, relinx ;
  static int pcol=0, vcol=0, typecode;
  static long repeat, width;
  static double deltat, tstart, tstop ;
/*
 *  Initialize ----------------------------------------------------------
 */
  for (j=0; j<3; j++)
    intposn[j] = 0.0 ;

  if ( *oerror )
    return intposn ;

  *oerror = 0 ;

  if ( strcmp (savefile, filename) ) {
    startrow = 0 ;
    nbrows = 0 ;
    if ( *savefile != ' ' )
      fits_close_file (OE, oerror) ;
    strcpy (savefile, " ") ;
    if ( fits_open_file (&OE, filename, 0, oerror) )
      fprintf(stderr, "nustarscorbit: Cannot open file %s\n", filename) ;
    else {
      fits_movabs_hdu (OE, 2, &htype, oerror) ;
      fits_read_key (OE, TDOUBLE, "DELTAT", &deltat, line, oerror) ;
      fits_read_key (OE, TDOUBLE, "TSTART", &tstart, line, oerror) ;
      fits_read_key (OE, TDOUBLE, "TSTOP", &tstop, line, oerror) ;
      fits_read_key (OE, TLONG, "NAXIS2", &numrows, line, oerror) ;

      /* Find POSITION and VELOCITY columns in input orbit file */
      fits_get_colnum (OE, CASEINSEN, "POSITION", &pcol, oerror);
      if ( *oerror ){
	fprintf(stderr,"nustarscorbit: did not find POSITION col\n");
	return intposn ;
      }
      fits_get_coltype (OE, pcol, &typecode, &repeat, &width, oerror);
      if (repeat != 3){
	fprintf(stderr,"nustarscorbit: found POSITION col but not 3-vector\n");
	return intposn ;
      }
      fits_get_colnum (OE, CASEINSEN, "VELOCITY", &vcol, oerror);
      if ( *oerror ){
	fprintf(stderr,"nustarscorbit: did not find VELOCITY col\n");
	return intposn ;
      }
      fits_get_coltype (OE, vcol, &typecode, &repeat, &width, oerror);
      if (repeat != 3){
	fprintf(stderr,"nustarscorbit: found VELOCITY col but not 3-vector..returning intposn\n");
	return intposn ;
      }

      if ( *oerror )
	fits_close_file (OE, oerror) ;
      else
	strcpy (savefile, filename) ;
    }
    if ( *oerror )
      return intposn ;
  }

/*
 *  See whether orbit file is appropriate --------------------------------
 */
  startinx = (t - tstart) / deltat + 0.5 ;
  if ( ( startinx < -2 ) || ( startinx > (numrows +1) ) ) {
    fprintf(stderr, "nustarscorbit: Time %.9g not covered by file %s\n", t, filename) ;
    *oerror = -2 ;
    return intposn ;
  }
  startinx++;
  if (startinx < 0) startinx = 0; 
  if (startinx > numrows) startinx = numrows; 
/*  fprintf (stderr, "nustarscorbit: startinx is %d\n",startinx) ; */
  

/*
 *  Read records from orbit file -----------------------------------------
 */
  relinx = startinx - startrow ;
/*  fprintf (stderr, "nustarscorbit: relinx is %d\n",relinx) ; */
  if ( !nbrows || !startrow || ( relinx > nbrows-3 ) || (relinx < 0 ) ) {
    startrow = startinx ;
    nbrows = ((startrow + NOEROWS) <= (numrows+1)) ? NOEROWS : (numrows - startrow +1) ;
/*    fprintf (stderr, "nustarscorbit: reading recs: relinx is %d   startinx is %d nbrows is %d numrows is %d\n",relinx,startinx,nbrows,numrows) ; */


    fits_read_col (OE, TDOUBLE, 1, startrow, 1, nbrows, 0,
		   sctime, &any, oerror) ;
    if ( *oerror ) {
      fprintf (stderr, "nustarscorbit: error while reading time starting at %d\n",
	       startrow) ;
      return intposn ;
    }
    for (i=0; i<nbrows; i++) {
      fits_read_col (OE, TDOUBLE, pcol, i+startrow, 1, 3, 0,
		     &vectmp[0], &any, oerror) ; /* stored as vector columns */
      for (j=0; j<3; j++) scposn[j][i] = vectmp[j];
      fits_read_col (OE, TDOUBLE, vcol, i+startrow, 1, 3, 0,
		     &vectmp[0], &any, oerror) ; /* stored as vector columns */
      for (j=0; j<3; j++) scvel[j][i] = vectmp[j];
    }

    if ( *oerror ) {
      fprintf (stderr, "nustarscorbit: error while reading pos/vel starting at %d\n",
	       startrow) ;
      return intposn ;
    }
    relinx = startinx - startrow ;	/* isn't this just 0?? AD */
/*    fprintf (stderr, "nustarscorbit: finished reading recs: relinx is %d\n",relinx) ; */
  }

    dt = t - sctime[relinx] ;
/*    fprintf (stderr, "nustarscorbit: dt = %.2f t = %.2f  OE = %.2f\n",dt,t,sctime[relinx]) ; */

    if ((dt<-2.0)||(dt>2.0)) {
      fprintf (stderr, "nustarscorbit: error: dt is too large: t is %.2f  dt is %.2f\n",t,dt) ;
      return intposn ;
    }

    for (j=0; j<3; j++) {
      intposn[j] = scposn[j][relinx] + dt * scvel[j][relinx] ;
      intposn[j] *= 1000.0; /* convert position vector to meters from km */
    }
/*    fprintf (stderr, "nustarscorbit: %.2f % 8.2f % 8.2f % 8.2f\n",t,intposn[0]/1000,intposn[1]/1000,intposn[2]/1000) ; */

/*
 *  Return position -----------------------------------------------------
 */
  return intposn ;
}

