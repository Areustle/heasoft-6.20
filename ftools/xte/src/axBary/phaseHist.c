char *phrcsid = "RCS: $Id: phaseHist.c,v 1.1 2001/02/21 21:32:25 miket Exp $" ;
/*-----------------------------------------------------------------------
 *
 *  phaseHist.c
 *
 *  Date: 6 November 1997
 *
 *  Arnold Rots, USRA
 *
 *  phaseHist is a C functions that processes pulsar observations,
 *  calculating the phase and binning the events in phase/energy space.
 *  It requires bary.h, bary.c, and dpleph.c.
 *  The main interface is through phaseHist:
 *    long **phaseHist     Returns a two dimensional histogram
 *                           long image[*nchan][*nphase]
 *                           Upon entering phaseHist:
 *                             If the histogram exists :
 *                               If !accum :
 *                                 deallocate the histogram
 *                                 allocate a new histogram
 *                                 zero the histogram
 *                             Else :
 *                               allocate a new histogram
 *                               zero the histogram
 *                           If an error occurs, a NULL pointer is returned
 *      char *orbitFile    Path for orbit ephemeris file
 *      char *dataFile     Path for data file; supported data modes:
 *                           PCA GoodXenon
 *                           PCA Single-EA event mode, including CE_
 *                           PCA Pulsar fold mode
 *                           PCA Binned mode, including CB_, but
 *                                            excepting multi-PCU/layer
 *                           PCA Single-bit mode
 *                           HEXTE Event mode
 *    + char *telescope    Mission/telescope used for collecting data
 *    + char *instrument   Instrument used for collecting data
 *    + char *radecsys     FK5 or ICRS
 *    + char *ephem        JPL-DE200 or JPL-DE405
 *      char *source       Pulsar database source name; may be B or J name
 *                           It expects the radio pulsar database in file
 *                           $TIMING_DIR/psrtime.dat
 *    + double *ra         RA used for calculating the phase
 *    + double *dec        Dec used for calculating the phase
 *      double roffset     Additional radio offset (in s)
 *                           This is the dispersion measure correction
 *                           for Crab and Vela pulsars
 *      double *dt0        Additional clock correction (in s); return full corr.
 *      int binary         Binary pulsar?
 *                           It expects the binary orbit database in file
 *                           $TIMING_DIR/psrbin.dat.
 *      PsrTimPar *inptp   Input PTP, if radio database is not to be used
 *      PsrBinPar *inpbp   Input PBP, if radio database is not to be used
 *                           If radio DB is to be used, provide pointers
 *                           to struct with (inptp->ra < 0) or NULL pointers
 *                           These structs are defined in bary.h
 *      int l1only         Layer 1 only? (only for GoodXenon)
 *                           -1: only propane layer
 *                            0: all xenon layer
 *                            1: only layer 1
 *      int nTR            Number of time ranges to apply
 *      double *starts     double starts[nTR]: GTI start times
 *      double *stops      double stops[nTR]: GTI stop times
 *    + double *tstart     double tstart: start of data in MJD
 *    + double *tstop      double tstop: End of data in MJD
 *    + double **exposure  Total exposure time (always in s) for phase bin i
 *                           is in *exposure[i]
 *      int accum          Do not zero the histogram, but accumulate
 *    + int *gainapp       Value of GAINAPP keyword
 *    + char *datobs       Value of DATE-OBS keyword; set to TSTART
 *   ++ int *nphase        Number of phase bins per period (default: 100)
 *   ++ int *nchan         Number of channel bins (default: 256)
 *   ++ char *cpix         Channel coordinate descriptor (default: none)
 *    + double *f0         Current frequency
 *      int nobcorr        Force no barycenter correction
 *    + int *nevts         Number of events accumulated
 *      int debug          Debug mode
 *
 *    + output only
 *   ++ input/output
 *
 *  phaseHist makes a number of assumptions:
 *    Data files are XTE data files of the type described above.
 *    PCA GoodXenon data have been processed by xenon2fits.
 *    It will find the data in column 2 for PCA, column 4 for HEXTE.
 *    HEXTE cluster information is found in column 3.
 *    The only allowed values for TIMESYS are: TT, TAI, ET, TDT, TDB, TB,
 *    TCB, UTC.
 *  It performs a number of services:
 *    Barycenter correction is applied if TIMESYS is not TDB or TCB.
 *    In PCA event mode, time markers are automatically screened out.
 *    In PCA GoodXenon data, there is an option to only select Layer 1 events.
 *    For HEXTE data the off-positions are screened out.
 *  Note:
 *    For PCA multi-PCU/LLD Binned configurations, only column 2 is used;
 *      this could be changed, but I am not sure whether it's worth the effort.
 *    Exposure time does not take missing partitions into account (unless
 *      included in the GTI table provided), nor does it correct for HEXTE
 *      off-source time.
 *    The phase values for the histogram refer to the CENTERS of the bins.
 *                                                    ^^^^^^^^^^^^^^^^^^^
 *  Incorporating other missions:
 *    - Define a new Observatory.
 *    - Add the new mission to the switch statements in clockCorr,
 *      dataConfig, dataSetup.
 *    - Define new data_type(s).
 *    - Add the mission-specific versions of clockCorr, dataConfig, dataSetup.
 *    - Add switches for the new data types.
 *
 *----------------------------------------------------------------------*/

#include "bary.h"
#define NBUFROW 1000
#define PHASPIXR 0.5
#define AMIN(A,B) (A<B ? A : B)
#define AMAX(A,B) (A>B ? A : B)

char *toTDB = "TDB" ;  /* could give user option of TCB, but be careful,
                        * the radio database assumes TDB */
/*
 * All parameters are global, so we can parcel some tasks out to
 * functions that may be mission-specific
 */
char *phProgName = {"phaseHist $Revision: 1.1 $"} ;
fitsfile *data_file;
int htype ;
char timesys[80] = "UTC" ;
char filetype[80] ;
char config[80] ;
char ddroot[80] ;
char newcpix[4096] ;
char *dd2 = NULL ;
char *evtb2 = NULL ;
int bcorr ;
int denum ;
unsigned long mask ;
enum data_type {INVALID, XTE_SE, XTE_SA, PCA_SE, PCA_GX, PCA_PF,
		PCA_B, PCA_SB, HEXTE_SE, AXAF_SE, ACIS_SE, HRC_SE} ;
enum data_type dtype ;
double dir[3] ;
char line[256] ;
char *subline ;
double *start ;
double *stop ;
double mjdrefi = 0.0 ;
double mjdreff = 0.0 ;
int datacol, clstrcol, timecol ;
int ngti ;
int igti ;
int row = 0 ;
int nrows ;
int nbufrow ;
long nax[2] ;
int anynul ;
void *nulval = NULL ;
int pfnchbin, pfnphbin, pfnbin ;
int nevtbyt=0 ;
int nevtint=0 ;
int numchan ;
unsigned long chanmask, chanshift, chan ;
double *Time ;
unsigned char *clstrpos ;
unsigned char *evtbuf ;
long *evtbufint ;
long *ievent ;
unsigned short *pfhist ;
unsigned char *event ;
unsigned short *pfold ;
unsigned long nevents ;
double t1, t2 ;
static long **hist = NULL ;
static double *expo = NULL ;
int channel ;
double dnphase ;
double dpfnphase ;
MJDTime mt ;
unsigned long it ;
double t ;
double tt ;
double pfper, deltapf ;
int ipfper ;
double timedel = 0.0 ;
double deltat = 0.0 ;
double deltaexp ;
double totexp ;
double timepixr = 0.5 ;
int simevent ;
double binscale ;
int error = 0 ;
double tZero, tCorr ;
double tierabso = 4.0, timezero = 0.0 ;
int i, n, j, k, l, m, jj1, jj2 ;
double yy1, yy2, w ;
char *str ;
int gotparms = 0 ;
double phase, ddp, phase2 ;
PsrTimPar ptp ;
PsrBinPar pbp ;
char datareffrm[80] = "FK5" ;
char timeunit[80] = "s" ;
char plephem[80] = "JPL-DE200" ;
char missionkey[80] = "" ;
int ephnum ;
double met2sec ;
double sec2met ;

int procGTI (double **, double **, int, double *, double *, double, double) ;
int readhead (char *, char *, char *, int *, char *) ;
void dataConfig (int, int, char*) ;
void xteDataConfig (int, int, char*) ;
void axafDataConfig (int, int, char*) ;
void dataSetup (int, int *, char *, int) ;
void xteDataSetup (int, int *, char *, int) ;
void axafDataSetup (int, int *, char *, int) ;

long** phaseHist (char *orbitFile, char *dataFile,
		  char *telescope, char *instrument,
		  char *radecsys, char *ephem,
		  char *source, double *ra, double *dec,
		  double roffset, double *dt0, int binary,
		  PsrTimPar *inptp, PsrBinPar *inpbp, int l1only,
		  int nTR, double *starts, double *stops,
		  double *tstart, double *tstop, double **exposure,
		  int accum, int *gainapp, char *datobs,
		  int *nphase, int *nchan, char *cpix, double *f0,
		  int nobcorr, int *nevts, int debug)
{

/*
 *  Start ---------------------------------------------------------------
 */
  if ( *nphase <= 0 )
    *nphase = 100 ;
  dnphase = *nphase ;
  ddp = PHASPIXR / dnphase ;

/*
 *  Open the data file --------------------------------------------------
 */
  if ( fits_open_file (&data_file, dataFile, 0, &error) ) {
    fprintf(stderr, "%s: cannot open file %s\n", phProgName, dataFile) ;
    return hist ;
  }
  fits_movabs_hdu (data_file, 2, &htype, &error) ;
  printf ("Data file: %s, htype: %d, error: %d\n", dataFile, htype, error) ;

/*
 *  Read some header items ----------------------------------------------
 */
  error = readhead (telescope, instrument, datobs, gainapp, dataFile) ;
  if ( error ) {
    fprintf (stderr, "%s: Error reading header of %s; status %d\n",
	     phProgName, dataFile, error) ;
    return hist ;
  }

/*
 *  Determine the mission -----------------------------------------------
 */
  if ( *missionkey )
    strcpy (telescope, missionkey) ;
  if ( !strcmp(telescope, "XTE") )
    mission = XTE ;
  else if ( !strcmp(telescope, "AXAF") )
    mission = AXAF ;
  else if ( !strcmp(telescope, "CHANDRA") )
    mission = AXAF ;

/*
 *  Clock corrections ---------------------------------------------------
 */
  clockinit (mission) ;
  tZero = clockCorr (0.5*(t1+t2), timezero, &tierabso, instrument) ;
  tZero += *dt0 ;
  t1 += tZero ;
  t2 += tZero ;

/*
 *  Determine the data configuration ------------------------------------
 */
  dataConfig (l1only, debug, instrument) ;

/*
 *  Set things up depending on the data configuration ------------------
 */
  dataSetup (l1only, nchan, cpix, debug) ;

/*
 *  Memory (de)allocation -----------------------------------------------
 */
  expo = *exposure ;
  if ( !accum )
    if ( hist ) {
      while ( hist[i] )
	free (hist[i++]) ;
      free (hist) ;
      hist = NULL ;
      free (expo) ;
      expo = NULL ;
    }
  if ( !hist ) {
    if ( hist = (long**) malloc (((*nchan)+1)*sizeof(long*)) ) {
      for (i=0; i<*nchan; i++)
	if ( !(hist[i] = (long*) malloc ((*nphase)*sizeof(long)) ) )
	  error = 1 ;
      hist[*nchan] = NULL ;
      if ( !(expo = (double*) malloc ((*nphase)*sizeof(double)) ) )
	error = 1 ;
      if ( error ) {
	while ( hist[i] )
	  free (hist[i++]) ;
	free (hist) ;
	hist = NULL ;
	free (expo) ;
	expo = NULL ;
      }
    }
    else
      error = 1 ;
    accum = 0 ;
  }
  if ( error ) {
    *exposure = expo ;
    return hist ;
  }
/*
 *  Initialize arrays ---------------------------------------------------
 */

  if ( !accum ) {
    for (j=0; j<*nchan; j++)
      for (i=0; i<*nphase; i++)
	hist[j][i] = 0 ;
    for (i=0; i<*nphase; i++)
      expo[i] = 0.0 ;
  }
  if ( debug )
    fprintf (stderr, "%s: initialized - %d phase and %d channel bins\n",
	     phProgName, *nphase, *nchan) ;

/*
 *  Time system --------------------------------------------------------
 */
  if ( !error ) {
    if ( !strcmp(timesys, "TT") )
      bcorr = 1 ;
    else if ( !strcmp(timesys, "TDT") )
      bcorr = 1 ;
    else if ( !strcmp(timesys, "TAI") )
      bcorr = 1 ;
    else if ( !strcmp(timesys, "ET") )
      bcorr = 1 ;
    else if ( !strcmp(timesys, "UTC") )
      bcorr = 1 ;
    else if ( !strcmp(timesys, "TB") )
      bcorr = 0 ;
    else if ( !strcmp(timesys, "TDB") )
      bcorr = 0 ;
    else if ( !strcmp(timesys, "TCB") )
      bcorr = 0 ;
    else {
      fprintf (stderr, "%s: unrecognized TIMESYS: %s; will assume it is UTC\n",
	       phProgName, timesys) ;
      bcorr = 1 ;
    }
    if ( debug )
      fprintf (stderr, "%s: timesystem: %s\n", phProgName, timesys) ;
    if ( !bcorr ) {
      if ( !strcmp (datareffrm, "ICRS") || !strcmp (plephem, "JPL-DE405") )
	ephnum = 405 ;
      else
	ephnum = 200 ;
    }
    else
      ephnum = 0 ;
    if ( nobcorr )
      bcorr = 0 ;

    /*  If no barycenter correction is required, we allow orbitFile "none"
     *  and then set mission to Geocenter, so wherever barycorr is called
     *  we get a sensible answer
     */
    if ( !bcorr &&
	 ( !strcmp(orbitFile, "none") || !strcmp(orbitFile, "None") || !strcmp(orbitFile, "NONE") ) )
      mission = Geocenter ;
    denum = baryinit (mission, timesys, toTDB, mjdrefi, mjdreff, timeunit,
		      ephnum) ;
    if ( denum ) {
      printf ("%s: Using JPL Planetary Ephemeris DE-%d\n", phProgName, denum) ;
      if ( debug )
	fprintf (stderr, "%s: bary stuff initialized\n", phProgName) ;
    }
    else {
      fprintf (stderr, "%s: Could not initialize bary stuff\n", phProgName) ;
      error = -20 ;
    }
    sprintf (plephem, "JPL-DE%d", denum) ;
  }

/*
 *  Get timing parameters -----------------------------------------------
 */
  simevent = 1 ;
  binscale = 1.0 ;
  met2mjd (t1, &mt) ;
  if ( !error ) {
    if ( inptp && ( inptp->ra >= 0.0 ) ) {
      ptp = *inptp ;
      if ( binary && inpbp && ( inpbp->pb > 0.0 ) )
	pbp = *inpbp ;
      else
	binary = 0 ;
    }
    else {
      if ( ( error =
	    timeparms (source, &mt, roffset, binary, &ptp, &pbp, 1) ) > 0 )
	fprintf (stderr, "%s: Could not find source %s for MET %d\n",
		 phProgName, source, t1) ;
      else if ( error == -3 ) {
	error = 0 ;
	binary = 1 ;
      }
      else if ( error ) {
	fprintf (stdout, "%s: Could not find source %s in binary pulsar file\n",
		 phProgName, source) ;
	fprintf (stdout, "%s: Proceeding without binary orbit corrections",
		 phProgName) ;
	error = 0 ;
	binary = 0 ;
      }
      if ( !error ) {
	if ( debug )
	  fprintf (stderr, "%s: ra = %f, dec = %f\n",
		   phProgName, ptp.ra, ptp.dec) ;
	*ra = ptp.ra ;
	*dec = ptp.dec ;
	ptp.ra /= RADEG ;
	ptp.dec /= RADEG ;
	dir[0] = cos(ptp.ra) * cos(ptp.dec) ;
	dir[1] = sin(ptp.ra) * cos(ptp.dec) ;
	dir[2] = sin(ptp.dec) ;
	if ( ( denum == 405 ) && ( ptp.denum == 200 ) )
	  c200to405 (dir, debug) ;
	met2sec = ( *timeunit == 'd' ) ? SECDAY : 1.0 ;
	sec2met = 1.0 / met2sec ;
	tt = met2sec * (t1 - mjd2met (&(ptp.t0))) ;
	*f0 = ptp.f + ptp.df * tt + 0.5 * ptp.ddf * tt * tt + ptp.dddf * tt * tt * tt / 6.0 ;
	if ( debug )
	  fprintf (stderr, "%s: f0 = %f\n",
		   phProgName, *f0) ;
	if ( timedel > 0.0 ) {
	  if ( timedel < (0.5 / ((*f0) * (*nphase))) ) {
	    simevent = 1 ;
	  }
	  else {
	    simevent = 0 ;
	    binscale = timedel * (*f0) * (*nphase) ;
	    if ( binscale <= 1.0 || ( dtype == PCA_PF ) )
	      binscale = 1.0 ;
	    else
	      if ( accum )
		for (j=0; j<*nchan; j++)
		  for (i=0; i<*nphase; i++)
		    hist[j][i] *= binscale ;
	  }
	}
      }
    }
  }

/*
 *  Straighten out time ranges ------------------------------------------
 */
  ngti = 0 ;
  totexp = 0.0 ;
  if ( !error ) {
    ngti = procGTI (&start, &stop, nTR, starts, stops, t1, t2) ;
    if ( debug ) {
      fprintf (stderr, "%s: TSTART = %f, TSTOP = %f\n",
	       phProgName, t1, t2) ;
      for (i=0; i<nTR; i++) {
	fprintf (stderr, "%s: %d start %f stop %f",
		 phProgName, i, starts[i], stops[i]) ;
	if ( i < ngti )
	  fprintf (stderr, "  newstart %f newstop %f\n",
		   start[i], stop[i]) ;
	else
	  fprintf (stderr, "\n") ;
      }
    }

    if ( !ngti )
      fprintf (stdout, "%s: no valid time ranges from %g to %g\n",
	       phProgName, t1, t2) ;
    else {
      if ( bcorr )
	for (i=0; i<ngti; i++)
	  totexp += met2sec * (stop[i] - start[i])
	    + xbarycorr(stop[i], dir, xscorbit(orbitFile, stop[i], &error))
	      - xbarycorr(start[i], dir, xscorbit(orbitFile, start[i], &error)) ;
      else
	for (i=0; i<ngti; i++)
	  totexp += met2sec * (stop[i] - start[i]) ;
      *tstart = start[0] ;
      *tstop = stop[ngti-1] ;
      if ( bcorr ) {
	*tstart += xbarycorr(*tstart, dir, xscorbit(orbitFile, *tstart, &error))
	            / met2sec ;
	*tstop += xbarycorr(*tstop, dir, xscorbit(orbitFile, *tstop, &error))
	           / met2sec ;
      }
      if ( debug ) {
	fprintf (stderr, "%s: ngti = %d, tstart = %f, tstop = %f, exposure = %f\n",
		 phProgName, ngti, start[0], stop[ngti-1], totexp) ;
	fprintf (stderr, "%s: in TDB: tstart = %f, tstop = %f\n",
		 phProgName, *tstart, *tstop) ;
      }
      met2mjd (*tstart, &mt) ;
      *tstart = mt.MJDint + mt.MJDfr ;
      strcpy (datobs, convertTime(&mt)) ;
      met2mjd (*tstop, &mt) ;
      *tstop = mt.MJDint + mt.MJDfr ;
    }
  }
/*
 *  Loop on the data ----------------------------------------------------
 */
  igti = 0 ;
  row = 0 ;
  nevents = 0 ;
  if ( nevtbyt )
    evtbuf = (unsigned char *) malloc (nevtbyt*nbufrow*sizeof(unsigned char)) ;
  if ( nevtint ) {
    evtbufint = (long *) malloc (nbufrow*sizeof(long)) ;
    if ( !datacol )
      for (i=0; i<nbufrow; i++)
	evtbufint[i] = 0 ;
  }
  Time = (double *) malloc (nbufrow * sizeof(double)) ;
  if ( clstrcol )
    clstrpos = (unsigned char *) malloc (nbufrow * sizeof(unsigned char)) ;
  if ( pfnbin )
    pfhist = (unsigned short *) malloc(nbufrow*pfnbin*sizeof(unsigned short)) ;

/*
 *  Read the data -------------------------------------------------------
 */
  while ( !error && ( row < nrows ) && ( igti < ngti ) ) {
/*        Time column first; add tZero; if outside range, forget it  */
    if ( (row + nbufrow) > nrows )
      nbufrow = nrows - row ;
    fits_read_col (data_file, TDOUBLE, timecol, row+1, 1, nbufrow,
		   nulval, Time, &anynul, &error) ;
    for (i=0; i<nbufrow; i++)
      Time[i] += tZero ;
    if ( Time[nbufrow-1] < start[igti] ) {
      row += nbufrow ;
      if ( debug ) {
	fprintf (stderr, "%s: read %d rows, starting at %d, skipping\n",
		 phProgName, nbufrow, row+1) ;
	fprintf (stderr, "               start = %f, stop = %f\n",
		 Time[0], Time[nbufrow-1]) ;
      }
      continue ;
    }
    if ( Time[0] > stop[ngti-1] ) {
      row += nbufrow ;
      if ( debug ) {
	fprintf (stderr, "%s: read %d rows, starting at %d, stopping\n",
		 phProgName, nbufrow, row+1) ;
	fprintf (stderr, "               start = %f, stop = %f\n",
		 Time[0], Time[nbufrow-1]) ;
      }
      break ;
    }
/*        The other columns - by mission  */
    switch (mission) {
    case XTE:
      if ( pfnbin )
	fits_read_col (data_file, TSHORT, datacol, row+1, 1,
		       nbufrow*pfnbin, nulval, pfhist, &anynul,
		       &error) ;
      else {
	if ( clstrcol )
	  fits_read_col (data_file, TBYTE, clstrcol, row+1, 1, nbufrow,
			 nulval, clstrpos, &anynul, &error) ;
	fits_read_col (data_file, TBYTE, datacol, row+1, 1, nbufrow*nevtbyt,
		       nulval, evtbuf, &anynul, &error) ;
      }
      break ;
    case AXAF:
      fits_read_col (data_file, TINT, datacol, row+1, 1, nbufrow,
		     nulval, evtbufint, &anynul, &error) ;
      break ;
    }
    if ( debug )
      fprintf (stderr, "%s: read %d rows, starting at %d after %d events\n",
	       phProgName, nbufrow, row+1, nevents) ;

/*
 *  Loop through the rows; check the time first --------------------------
 */
    k = 0 ;
    event = evtbuf ;
    ievent = evtbufint ;
    pfold = pfhist ;
    while ( k<nbufrow ) {
      t = Time[k] ;
      if ( t < start[igti] ) {
	k++ ;
	pfold += pfnbin ;
	event += nevtbyt ;
	ievent++ ;
	continue ;
      }
      else if ( t > stop[igti] ) {
	igti++ ;
	if ( igti < ngti )
	  continue ;
	else {
	  igti-- ;
	  break ;
	}
      }

/*
 *  Switch on dtype; event mode first; HEXTE's cluster position ----------
 */
      switch ( dtype ) {
      case HEXTE_SE:
	if ( clstrpos[k] & 0x36 )
	  break ;

/*
 *  All XTE event modes; get the channel information ----------------------
 */
      case PCA_SE:
      case PCA_GX:
	chan = (unsigned long) event[nevtbyt-1] + event[nevtbyt-2] * 256 ;
	if ( nevtbyt > 2 )
	  chan += (unsigned long) event[nevtbyt-3] * 65536 ;
	if  ( !(chan & mask) )
	  break ;
	chan = ( chan >> chanshift ) & chanmask ;
	if ( chan >= numchan )
	  chan = numchan - 1 ;

/*
 *  All XTE event modes; calculate phase and add to histogram -------------
 */
	phase = xabsphase (t,  &ptp, binary, &pbp, bcorr, dir, orbitFile,
			  &error) + ddp ;
	phase -= (int) phase ;
	if ( debug > 7 ) {
	  fprintf (stderr, "TB = %18.17g   ", t) ;
	  fprintf (stderr, "Phase = %6.3g\n", phase) ;
	}
	j = phase * dnphase ;
	hist[chan][j]++ ;
	nevents++ ;
	break ;

/*
 *  All AXAF event modes; get the channel information ---------------------
 */
      case HRC_SE:
      case ACIS_SE:
	chan = *ievent ;

/*
 *  All AXAF event modes; calculate phase and add to histogram ------------
 */
	phase = xabsphase (t,  &ptp, binary, &pbp, bcorr, dir, orbitFile,
			  &error) + ddp ;
	phase -= (int) phase ;
	if ( debug > 7 ) {
	  fprintf (stderr, "TB = %18.17g   ", t) ;
	  fprintf (stderr, "Phase = %6.3g\n", phase) ;
	}
	j = phase * dnphase ;
	hist[chan][j]++ ;
	nevents++ ;
	break ;

/*
 *  Pulsar fold mode; calculate exposure ---------------------------------
 */
      case PCA_PF:
	if ( bcorr )
	  tt = t ;
	else
	  tt = t - sec2met * xbarycorr(t, dir, xscorbit(orbitFile, t, &error)) ;
	deltaexp = (pfper * met2sec
	    + xbarycorr(tt+pfper, dir, xscorbit(orbitFile, tt+pfper, &error))
	    - xbarycorr(tt, dir, xscorbit(orbitFile, tt, &error)) )
	  / dnphase ;
	for (i=0; i<(*nphase); i++)
	  expo[i] += deltaexp ;

/*
 *  Pulsar fold mode; calculate phase ------------------------------------
 */
	tt = t + sec2met * pfper ;
	phase = xabsphase (t,  &ptp, binary, &pbp, bcorr, dir, orbitFile,
			  &error) + ddp ;
	phase2 = xabsphase (tt,  &ptp, binary, &pbp, bcorr, dir, orbitFile,
			  &error) + ddp ;
	phase2 -= phase ;
	while ( phase2 > 0.5 )
	  phase2-- ;
	while ( phase2 < -0.5 )
	  phase2++ ;
	phase += 0.5 * phase2 ;
	phase -= (int) phase ;

	if ( debug ) {
	  fprintf (stderr, "TB = %18.17g   ", t) ;
	  fprintf (stderr, "Phase = %6.3g\n", phase) ;
	}

/*
 *  Pulsar fold mode; distribute the counts ------------------------------
 */
	for (i=0; i<pfnphbin; i++) {
	  yy1 = (double) i * dpfnphase + phase * dnphase ;
	  yy2 = yy1 + dpfnphase ;
	  jj1 = (int) yy1 ;
	  jj2 = (int) yy2 ;
	  for (j=jj1; j<=jj2; j++) {
	    w = (AMIN(j+1, yy2) - AMAX(j, yy1)) / dpfnphase ;
	    m = (int) j - (j / (*nphase)) * (*nphase) ;
	    if ( m < 0 )
	      m += *nphase ;
	    for (l=0; l<numchan; l++) {
	      n = (int) ( (double) w * pfold[l*pfnphbin + i] + 0.5 ) ;
	      hist[l][m] += n ;
	      nevents += n ;
	    }
	  }
	}
	break ;

/*
 *  Binned modes; small bins ---------------------------------------------
 */
      case PCA_B:
      case PCA_SB:
	if ( bcorr )
	  tt = t ;
	else
	  tt = t - sec2met * xbarycorr(t, dir, xscorbit(orbitFile, t, &error)) ;
	timedel = (deltat + sec2met *
	    ( xbarycorr(tt+deltat, dir, xscorbit(orbitFile, tt+deltat, &error))
	    - xbarycorr(tt, dir, xscorbit(orbitFile, tt, &error)) ) )
	  / pfnphbin ;
	if ( simevent ) {
	  for (i=0; i<pfnphbin; i++) {
	    tt = t + (0.5 +i) * timedel ;
	    phase = xabsphase (tt,  &ptp, binary, &pbp, bcorr, dir, orbitFile,
			      &error) + ddp ;
	    phase -= (int) phase ;
	    if ( debug && !i ) {
	      fprintf (stderr, "TB = %18.17g   ", tt) ;
	      fprintf (stderr, "First bin phase = %6.3g\n", phase) ;
	    }
	    j = phase * dnphase ;
	    expo[j] += met2sec * timedel ;
	    for (l=0; l<numchan; l++) {
	      n =  pfold[l*pfnphbin + i] ;
	      hist[l][j] += n ;
	      nevents += n ;
	    }
	  }
	}

/*
 *  Binned modes; large bins ---------------------------------------------
 */
	else {
	  tt = t ;
	  phase = xabsphase (tt,  &ptp, binary, &pbp, bcorr, dir, orbitFile,
			    &error) + ddp ;
	  phase -= (int) phase ;
	  yy2 = phase * dnphase ;
	  for (i=0; i<pfnphbin; i++) {
	    yy1 = yy2 ;
	    if ( dnphase <= yy1 )
	      yy1 -= dnphase ;
	    tt = t + (i+1) * timedel ;
	    phase = xabsphase (tt,  &ptp, binary, &pbp, bcorr, dir, orbitFile,
			      &error) + ddp ;
	    phase -= (int) phase ;
	    yy2 = phase * dnphase ;
	    if ( yy2 < yy1 )
	      yy2 += dnphase ;
	    if ( debug && !i ) {
	      fprintf (stderr, "TB = %18.17g   ", tt) ;
	      fprintf (stderr, "First bin phases = %6.3g to %6.3g\n", yy1, yy2) ;
	    }
	    jj1 = (int) yy1 ;
	    jj2 = (int) yy2 + 1 ;
	    for (j=jj1; j<jj2; j++) {
	      w = (AMIN((j+1), yy2) - AMAX(j, yy1)) / ( yy2 - yy1) ;
	      m = (int) j - (j / (*nphase)) * (*nphase) ;
	      if ( m < 0 )
		m += *nphase ;
	      expo[m] += met2sec * w * timedel ;
	      w *= binscale ;
	      for (l=0; l<numchan; l++) {
		n = (int) ( (double) w * pfold[l*pfnphbin + i] + 0.5 ) ;
		hist[l][m] += n ;
		nevents += n ;
	      }
	    }
	  }
	}
	break ;

/*
 *  Default; end of switch -----------------------------------------------
 */
      default:
	break ;
      }

/*
 *  End of buffer loop ---------------------------------------------------
 */
	k++ ;
	pfold += pfnbin ;
	event += nevtbyt ;
	ievent++ ;
    }

/*
 *  End of data read loop ------------------------------------------------
 */
    row += nbufrow ;
  }

/*
 *  Finish things up -----------------------------------------------------
 */
  if ( debug )
    fprintf (stderr, "%s: before finishing up, error status %d\n",
	     phProgName, error) ;
  switch (dtype) {
  case PCA_GX:
  case PCA_SE:
  case HEXTE_SE:
  case HRC_SE:
  case ACIS_SE:
/*
 *      We need to calculate the exposure properly for event mode --------
 *          Each good time interval
 */
    for (igti=0; igti<ngti; igti++) {
      /*  The phases of start and stop  */
      phase = xabsphase (start[igti], &ptp, binary, &pbp, bcorr, dir, orbitFile,
			  &error) + ddp ;
      phase -= (int) phase ;
      phase2 = xabsphase (stop[igti], &ptp, binary, &pbp, bcorr, dir, orbitFile,
			  &error) + ddp ;
      phase2 -= (int) phase2 ;
      /*  The number of complete periods  */
      deltaexp = 1.0 / (*f0 * dnphase) ;
      k = (stop[igti] - start[igti]) * (*f0) ;
      deltaexp *= k * met2sec ;
      for (i=0; i<(*nphase); i++)
	expo[i] += deltaexp ;
      /*  The bins that get one period extra  */
      phase *= dnphase ;
      phase2 *= dnphase ;
      jj1 = phase ;
      jj2 = phase2 ;
      deltaexp = met2sec / (*f0 * dnphase) ;
      w = phase2 - phase ;
      for (i=0; i<(*nphase); i++)
	if ( w * (i-jj1) * (jj2 - i) >= 0.0 )
	  expo[i] += deltaexp ;
      /*  Correct the boundary bins  */
      expo[jj1] -= deltaexp * (phase - jj1) ;
      expo[jj2] -= deltaexp * (jj2 + 1 -phase2) ;
    }
    break ;
  case PCA_B:
  case PCA_SB:
    if ( (!simevent) && ( binscale > 1.0 ) )
      for (j=0; j<*nchan; j++)
	for (i=0; i<*nphase; i++)
	  hist[j][i] = (int) ((double) hist[j][i] / binscale + 0.5 ) ;
    break ;
  default:
    break ;
  }
  if ( debug )
    fprintf (stderr, "%s: finished; accepted %d events\n",
	     phProgName, nevents) ;

/*
 *  Close up and exit ---------------------------------------------------
 */
  if ( nevtbyt )
    free (evtbuf) ;
  if ( nevtint )
    free (evtbufint) ;
  free (Time) ;
  if ( clstrcol )
    free (clstrpos) ;
  if ( pfnbin )
    free (pfhist) ;
  if ( start ) {
    free (start) ;
    free (stop) ;
    start = stop = NULL ;
  }

  if ( error && !accum ) {
    if ( debug )
      fprintf (stderr, "%s: error %d occurred, freeing hist\n",
	     phProgName, error) ;
    while ( hist[i] )
      free (hist[i++]) ;
    free (hist) ;
    hist = NULL ;
    free (expo) ;
    expo = NULL ;
  }

  if ( dd2 )
    free (dd2) ;
  if ( evtb2 )
    free (evtb2) ;
  i = 0 ;
  fits_close_file (data_file, &i) ;

  *nevts += nevents ;
  *exposure = expo ;
  *dt0 = tZero ;
  strcpy (radecsys, datareffrm) ;
  strcpy (ephem, plephem) ;
  return hist ;
}

int procGTI (double **startp, double **stopp,
	     int nTR, double *starts, double *stops,
	     double t1, double t2)
/*------------------------------------------------------------------
 *
 *  A function that ANDs nTR time ranges (starts - stops) with
 *  a global time interval t1 - t2, sorts the intervals, and
 *  removes overlaps.
 *  Processed arrays of start and stop times are returned in
 *  startp and stopp, while the function returns the number
 *  of resulting intervals.
 *
 *-----------------------------------------------------------------*/
{

  double *start = NULL ;
  double *stop = NULL ;
  int ngti = 0 ;
  int i, j ;
  double t1t, t2t ;

/*      Trivial case               */
  if ( nTR == 1 ) {
    if ( ( *starts == 0.0 ) && ( *stops == 0.0 ) )
      nTR = 0 ;
    else if ( ( *starts < t1 ) && ( *stops > t2 ) )
      nTR = 0 ;
  }

/*      AND with start and stop    */
  if ( nTR ) {
    start = (double*) malloc ( nTR * sizeof (double) ) ;
    stop = (double*) malloc ( nTR * sizeof (double) ) ;
    for (i=0; i<nTR; i++) {
      if ( ( starts[i] < t2 ) && ( stops[i] > t1 ) ) {
	if ( starts[i] > t1 )
	  start[ngti] = starts[i] ;
	else
	  start[ngti] = t1 ;
	if ( stops[i] < t2 )
	  stop[ngti] = stops[i] ;
	else
	  stop[ngti] = t2 ;
	ngti++ ;
      }
    }
/*      Bubble sort                */
    if ( ngti > 1 ) {
      for (i=ngti-1; i>0; i--)
	for (j=0; j<i; j++)
	  if ( start[j] > start[j+1] ) {
	    t1t = start[j] ;
	    t2t = stop[j] ;
	    start[j] = start[j+1] ;
	    stop[j] = stop[j+1] ;
	    start[j+1] = t1t ;
	    stop[j+1] = t2t ;
	  }
/*      Combine overlaps           */
      for (i=0; i<ngti-1; i++)
	if ( stop[i] >= start[i+1] ) {
	  if ( stop[i] < stop[i+1] )
	    stop[i] = stop[i+1] ;
	  ngti-- ;
	  for (j=i+1; j<ngti ; j++) {
	    start[j] = start[j+1] ;
	    stop[j] = stop[j+1] ;
	  }
	  i-- ;
	}
    }
  }
/*      No GTIs specified: take start and stop */
  else {
    start = (double*) malloc ( sizeof (double) ) ;
    stop = (double*) malloc ( sizeof (double) ) ;
    ngti = 1 ;
    start[0] = t1 ;
    stop[0] = t2 ;
  }

  *startp = start ;
  *stopp = stop ;

  return ngti ;

}

int readhead (char *telescope, char *instrument, char *datobs,
	      int *gainapp, char *dataFile)
/*------------------------------------------------------------------
 *
 *  A function that reads header items from the FITS data file.
 *
 *-----------------------------------------------------------------*/
{
  int stat = 0 ;

/*
 *  Read some header items ----------------------------------------------
 */
  fits_read_key   (data_file, TDOUBLE, "TIMEZERO", &timezero,  line, &stat) ;
  if ( stat ) {
    timezero = 0.0 ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TDOUBLE, "TIERABSO", &tierabso,  line, &stat) ;
  if ( stat ) {
    tierabso = 4.0 ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TDOUBLE, "MJDREFI",  &mjdrefi,   line, &stat) ;
  if ( stat ) {
    stat = 0 ;
    mjdrefi = 0.0 ;
    fits_read_key (data_file, TDOUBLE, "MJDREF",   &mjdreff,   line, &stat) ;
  }
  else {
    fits_read_key (data_file, TDOUBLE, "MJDREFF",  &mjdreff,   line, &stat) ;
    if ( stat ) {
      mjdreff = 0.0 ;
      stat = 0 ;
    }
  }
  fits_read_key   (data_file, TDOUBLE, "DELTAT",   &deltat,    line, &stat) ;
  if ( stat ) {
    deltat = 0.0 ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TDOUBLE, "TIMEDEL",  &timedel,   line, &stat) ;
  if ( stat ) {
    timedel = 0.0 ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TDOUBLE, "TIMEPIXR", &timepixr,  line, &stat) ;
  if ( stat ) {
    timepixr = 0.5 ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TSTRING, "TIMESYS",  timesys,    line, &stat) ;
  if ( stat ) {
    strcpy (timesys, "UTC") ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TSTRING, "REFFRAME", datareffrm, line, &stat) ;
  if ( stat ) {
    strcpy (datareffrm, "FK5") ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TSTRING, "TIMEUNIT", timeunit,   line, &stat) ;
  if ( stat ) {
    strcpy (timeunit, "s") ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TSTRING, "PLEPHEM",  plephem,    line, &stat) ;
  if ( stat ) {
    strcpy (plephem, "JPL-DE200") ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TSTRING, "DATAMODE", config,     line, &stat) ;
  if ( stat ) {
    strcpy (config, "UNKNOWN") ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TSTRING, "MISSION",  missionkey, line, &stat) ;
  if ( stat ) {
    *missionkey = 0 ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TSTRING, "TELESCOP", telescope,  line, &stat) ;
  if ( stat ) {
    strcpy (telescope, "UNKNOWN") ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TSTRING, "INSTRUME", instrument, line, &stat) ;
  if ( stat ) {
    strcpy (instrument, "UNKNOWN") ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TSTRING, "TDDES",    ddroot,     line, &stat) ;
  stat = 0 ;
  if ( !(*datobs) ) {
    fits_read_key   (data_file, TSTRING, "DATE-OBS", datobs,   line, &stat) ;
    fits_read_key   (data_file, TLOGICAL, "GAINAPP", gainapp,  line, &stat) ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TSTRING, "EXTNAME",  filetype,   line, &stat) ;
  printf ("EXTNAME: %s, status: %d\n", filetype, stat) ;
  if ( stat ) {
    strcpy (filetype, "UNKNOWN") ;
    stat = 0 ;
  }
  fits_read_key   (data_file, TDOUBLE, "TSTART",   &t1,        line, &stat) ;
  printf ("TSTART: %f, status: %d\n", t1, stat) ;
  fits_read_key   (data_file, TDOUBLE, "TSTOP",    &t2,        line, &stat) ;
  printf ("TSTOP:  %f, status: %d\n", t2, stat) ;
  fits_read_key   (data_file, TINT,    "NAXIS2",   &nrows,     line, &stat) ;
  printf ("NAXIS2: %d, status: %d\n", nrows, stat) ;
  printf ("Working on data file %s\n", dataFile) ;
  printf ("  Configuration %s\n", config) ;

  return stat ;
}

void dataConfig (int l1only, int debug, char *instrument)
/*------------------------------------------------------------------
 *
 *  A function that determines the data configuration depending on mission.
 *  It sets or adjusts:
 *    dtype
 *    nbufrow
 *    tZero
 *    nevtbyt
 *    pfnchbin
 *    pfnphbin
 *    pfnbin
 *    error
 *    mask
 *
 *-----------------------------------------------------------------*/
{

/*
 *  Determine the data configuration ------------------------------------
 */
  if ( !error ) {
    switch (mission) {
    case XTE:
      xteDataConfig (l1only, debug, instrument) ;
      break ;
    case AXAF:
      axafDataConfig (l1only, debug, instrument) ;
      break ;
    default:
      break ;
    }
  }
  return ;
}
void xteDataConfig (int l1only, int debug, char *instrument)
/*------------------------------------------------------------------
 *
 *  A function that determines the data configuration for XTE.
 *
 *-----------------------------------------------------------------*/
{

/*
 *  Determine the data configuration ------------------------------------
 */
  if ( !strcmp(filetype, "XTE_SE") ) {
    dtype = XTE_SE ;
    pfnchbin = pfnphbin = pfnbin = 0 ;
    nbufrow = NBUFROW ;
    tZero += (0.5 - timepixr) * timedel ;  /* since time bits are truncated */
  }
  else if ( !strcmp(filetype, "XTE_SA") ) {
    dtype = XTE_SA ;
    nevtbyt = 0 ;
    nbufrow = 1 ;
    tZero -= timepixr * timedel ;  /* correct to TIMEPIXR=0.0 */
  }
  else {
    fprintf (stderr, "%s: unrecognized EXTNAME: %s\n", phProgName, filetype) ;
    dtype = INVALID ;
    error = -1 ;
  }

  if ( !error ) {
    if ( strstr(ddroot, "I[PCA]") ) {
      if ( dtype == XTE_SA ) {
	if ( !strncmp(config, "PF_", 3) )
	  dtype = PCA_PF ;
	else if ( !strncmp(config, "SB", 2) )
	  dtype = PCA_SB ;
	else if ( strstr(config, "B_") )
	  dtype = PCA_B ;
	else {
	  fprintf (stderr, "%s: invalid configuration/EXTNAME pair:\n%s  %s\n",
		   phProgName, config, filetype) ;
	  error = -3 ;
	}
      }
      else if ( dtype == XTE_SE ) {
	if ( strstr(config, "E_") )
	  dtype = PCA_SE ;
	else if ( strstr(config, "GoodXenon") )
	  dtype = PCA_GX ;
	else {
	  fprintf (stderr, "%s: invalid configuration/EXTNAME pair:\n%s  %s\n",
		   phProgName, config, filetype) ;
	  error = -3 ;
	}
      }
    }
    else if ( strstr(ddroot, "I[HEXTE]") ) {
      if ( dtype == XTE_SE ) {
	if ( strstr(config, "E_") )
	  dtype = HEXTE_SE ;
	else {
	  fprintf (stderr, "%s: invalid configuration/EXTNAME pair:\n%s  %s\n",
		   phProgName, config, filetype) ;
	  error = -3 ;
	}
      }
    }
    else {
      error = -4 ;
      fprintf (stderr, "%s: invalid instrument: %s\n",
		   phProgName, ddroot) ;
    }
  }

  if ( dtype == PCA_GX ) {
    if ( l1only > 0 )
      mask = 0x00000300 ;
    else if ( l1only < 0 )
      mask = 0x00020000 ;
    else
      mask = 0x00003f00 ;
  }
  else if ( dtype == PCA_SE )
    mask = 0x00008000 ;
  else
    mask = 0x000fffff ;
  if ( debug )
    switch ( dtype ) {
    case PCA_PF:
      fprintf (stderr, "%s: dtype PCA_PF\n", phProgName) ;
      break ;
    case PCA_B:
      fprintf (stderr, "%s: dtype PCA_B\n", phProgName) ;
      break ;
    case PCA_SB:
      fprintf (stderr, "%s: dtype PCA_SB\n", phProgName) ;
      break ;
    case PCA_GX:
      fprintf (stderr, "%s: dtype PCA_GX\n", phProgName) ;
      break ;
    case PCA_SE:
      fprintf (stderr, "%s: dtype PCA_SE\n", phProgName) ;
      break ;
    case HEXTE_SE:
      fprintf (stderr, "%s: dtype HEXTE_SE\n", phProgName) ;
      break ;
    default:
      fprintf (stderr, "%s: illegal dtype %d\n", phProgName, dtype) ;
      break ;
    }
  return ;
}

void axafDataConfig (int l1only, int debug, char *instrument)
/*------------------------------------------------------------------
 *
 *  A function that determines the data configuration for AXAF/CHANDRA.
 *
 *-----------------------------------------------------------------*/
{

/*
 *  Determine the data configuration ------------------------------------
 */
  if ( !strcmp(filetype, "EVENTS") ) {
    dtype = AXAF_SE ;
    pfnchbin = pfnphbin = pfnbin = 0 ;
    nbufrow = NBUFROW ;
    nevtbyt = 0 ;
    nevtint = 1 ;
    tZero += (0.5 - timepixr) * timedel ;  /* since time bits may be truncated */
  }
  else {
    fprintf (stderr, "%s: unrecognized EXTNAME: %s\n", phProgName, filetype) ;
    dtype = INVALID ;
    error = -1 ;
  }

  if ( !error ) {
    if ( strstr(instrument, "ACIS") ) {
      dtype = ACIS_SE ;
    }
    else if ( strstr(instrument, "HRC") ) {
      dtype = HRC_SE ;
    }
    else {
      error = -4 ;
      fprintf (stderr, "%s: invalid instrument: %s\n",
		   phProgName, instrument) ;
    }
  }

  mask = 0xffffffff ;
  if ( debug )
    switch ( dtype ) {
    case ACIS_SE:
      fprintf (stderr, "%s: dtype ACIS_SE\n", phProgName) ;
      break ;
    case HRC_SE:
      fprintf (stderr, "%s: dtype HRC_SE\n", phProgName) ;
      break ;
    case AXAF_SE:
      fprintf (stderr, "%s: dtype AXAF_SE\n", phProgName) ;
      break ;
    default:
      fprintf (stderr, "%s: illegal dtype %d\n", phProgName, dtype) ;
      break ;
    }
  return ;
}

void dataSetup (int l1only, int *nchan, char *cpix, int debug)
/*------------------------------------------------------------------
 *
 *  A function that sets up the data structures depending on mission.
 *  It sets or adjusts:
 *    timecol     chanmask    chanshift   datacol
 *    clstrcol    dd2         timedel     n
 *    nax         pfnchbin    pfnphbin    pfnbin
 *    numchan     newpix      dpfnphase   deltat
 *    pfper       deltapf     evtb2       nevtbyt
 *    nchan       error
 *
 *-----------------------------------------------------------------*/
{

/*
 *  Determine the data configuration ------------------------------------
 */
  if ( !error ) {
    switch (mission) {
    case XTE:
      xteDataSetup (l1only, nchan, cpix, debug) ;
      break ;
    case AXAF:
      axafDataSetup (l1only, nchan, cpix, debug) ;
      break ;
    default:
      break ;
    }
  }
  return ;
}
void xteDataSetup (int l1only, int *nchan, char *cpix, int debug)
/*------------------------------------------------------------------
 *
 *  A function that sets up the data structures for XTE.
 *
 *-----------------------------------------------------------------*/
{

/*
 *  Set things up depending on the data configuration ------------------
 */
  timecol = 1 ;
  chanmask = 0x000000ff ;
  chanshift = 0 ;
  datacol = 2 ;
  clstrcol = 0 ;
  fits_read_key_longstr  (data_file,   "TDDES2", &dd2,    line, &error) ;
  switch ( dtype ) {
  case PCA_SB:
  case PCA_B:
    timedel = -1.0 ;
    strcpy (line, strstr(dd2, "T[") + 2 ) ;
    sscanf (line, "%lg;%lg", &t, &timedel) ;
  case PCA_PF:
    fits_read_tdim (data_file,       2,        2,  &n,     nax, &error) ;
    if ( n < 2 )
      nax[1] = 1 ;
    pfnphbin = nax[0] ;
    pfnchbin = nax[1] ;
    numchan = pfnchbin ;
    strcpy (newcpix, strstr(dd2, "C[") + 2 ) ;
    *(strstr(newcpix, "]")) = 0 ;
    pfnbin = pfnphbin * pfnchbin ;
    dpfnphase = (double) dnphase / pfnphbin ;
    if ( deltat <= 0.0 )
      deltat = timedel * pfnphbin ;
/*  Adjust for frequency errors  */
    if ( dtype == PCA_PF ) {
      strcpy (line, strstr(dd2, "P[") + 2 ) ;
      *(strstr(line, "]")) = 0 ;
      subline = line - 1 ;
      pfper = 0.0 ;
      while ( subline ) {
	subline++ ;
	sscanf (subline, "%lg;%lg;%d", &t, &deltapf, &i) ;
	if ( debug )
	  fprintf (stderr, "subline: %s, t = %f, deltapf = %f, i = %d\n",
		   subline, t, deltapf, i) ;
	pfper += i * deltapf ;
	subline = strstr (subline, ",") ;
      }
      ipfper = timedel / pfper ;
      if ( debug )
	fprintf (stderr, "pfper = %f, ipfper = %d, timedel = %f; ",
		 pfper, ipfper, timedel) ;
      pfper *= ipfper ;
      if ( debug )
	fprintf (stderr, "pfper = %f\n", pfper) ;
    }
    break ;
  case PCA_GX:
    if ( l1only > 0 ) {
      printf ("===>Note: Only layer 1 good xenon events will be selected\n") ;
      printf ("          from configuration %s\n", config) ;
    }
    else if ( l1only == 0 ) {
      printf ("===>Note: All good xenon events will be selected\n") ;
      printf ("          from configuration %s\n", config) ;
    }
    else {
      if ( strstr (config, "Prop") ) {
	printf ("===>Note: Only PROPANE events will be selected\n") ;
	printf ("          from configuration %s\n", config) ;
      }
      else {
	printf (">>>>Note: You asked for PROPANE events to be selected\n") ;
	printf (">>>>      from configuration %s\n", config) ;
	printf (">>>>      Hence, this data file will be skipped\n") ;
	error = -10 ;
      }
    }
    fits_read_key_longstr (data_file,  "TEVTB2", &evtb2,  line, &error) ;
    nevtbyt = 3 ;
    numchan = 256 ;
    strcpy (newcpix, "0:255") ;
    break ;
  case PCA_SE:
    fits_read_key_longstr (data_file,  "TEVTB2", &evtb2,  line, &error) ;
    while ( str = strchr (evtb2, ' ') ) {
      if ( debug )
	fprintf (stderr, "%s:%d characters left in str\n", phProgName, strlen(str)) ;
      for (i=0; str[i]; i++)
	str[i] = str[i+1] ;
      if ( debug )
	fprintf (stderr, "%s: stopped at character %d in str\n", phProgName, i) ;
    }
    n = k = 0 ;
    if ( debug )
      fprintf (stderr, "%s: TEVTB2: %s\n",
	       phProgName, evtb2) ;
    str = evtb2 ;
    if ( strstr(evtb2,"C[") ) {
      while ( (str = strchr (str, '{')) < strstr(evtb2, "C[") ) {
	sscanf (++str, "%d", &k) ;
	n += k ;
      }
      sscanf (++str, "%d", &k) ;
      n += k ;
      chanshift = 16 - n ;
      chanmask = 1 ;
      for (i=1; i<k; i++)
	chanmask = 2 * chanmask + 1 ;
      if ( debug )
	fprintf (stderr, "%s: PCA_SE: found %d bits before T, %d pha bits, chmask %d\n",
		 phProgName, n, k, chanmask) ;
      numchan = chanmask + 1 ;
      strcpy (newcpix, strstr(evtb2,"C[")+2) ;
      *(strstr (newcpix, "]")) = 0 ;
    }
    else {
      chanmask = 0 ;
      numchan = 1 ;
      strcpy (newcpix, strstr(dd2,"C[")+2) ;
      *(strstr (newcpix, "]")) = 0 ;
    }
    nevtbyt = 2 ;
    break ;
  case HEXTE_SE:
    fits_read_key_longstr  (data_file, "TDDES4", &dd2,    line, &error) ;
    fits_read_key_longstr (data_file,  "TEVTB4", &evtb2,  line, &error) ;
    datacol = 4 ;
    clstrcol = 3 ;
    sscanf (config + strlen(config) - 2, "%2X", &j) ;
    for (i=0, k=1, nevtbyt=0; i<7 ;i++, (k=k<<1))
      if ( j&k )
	nevtbyt++ ;
    if ( j&1 ) {
      numchan = 256 ;
      strcpy (newcpix, "0:255") ;
    }
    else {
      numchan = 1 ;
      strcpy (newcpix, "0~255") ;
    }
    break ;
  default:
    error = -5 ;
    strcpy (newcpix, "none") ;
    break ;
  }
  if ( *nchan <= 0 )
    *nchan = numchan ;
  else if ( numchan > *nchan )
    numchan = *nchan ;
  if ( strcmp (cpix, "none") ) {
    if ( strcmp (newcpix, "none") ) {
      if ( strcmp (cpix, newcpix) ) {
	error = -5 ;
	fprintf (stderr, "%s: new CPIX (%s) differs from previous CPIX (%s)\n",
	       phProgName, newcpix, cpix) ;
      }
    }
  }
  else if ( strcmp (newcpix, "none") )
    strcpy (cpix, newcpix) ;
  if ( debug )
    fprintf (stderr, "%s: CPIX = %s\n",
	     phProgName, cpix) ;
	
  if ( debug )
    fprintf (stderr, "%s: numchan = %d\n",
	     phProgName, numchan) ;
  if ( debug && pfnbin )
    fprintf (stderr, "%s: pfnbin = %d, pfnphbin = %d, pfnchbin = %d\n",
	     phProgName, pfnbin, pfnphbin, pfnchbin) ;

  return ;
}

void axafDataSetup (int l1only, int *nchan, char *cpix, int debug)
/*------------------------------------------------------------------
 *
 *  A function that sets up the data structures for AXAF/CHANDRA.
 *
 *-----------------------------------------------------------------*/
{

/*
 *  Set things up depending on the data configuration ------------------
 */
  char keyword[32] ;
  int i, j ;

  timecol = 0 ;
  chanmask = 0x000000ff ;
  chanshift = 0 ;
  datacol = 0 ;
  clstrcol = 0 ;

  strcpy (newcpix, "none") ;
  switch ( dtype ) {
  case ACIS_SE:
    *nchan = 1024 ;
    break ;
  case HRC_SE:
    *nchan = 256 ;
    break ;
  default:
    error = -5 ;
    break ;
  }

  if ( !error ) {
    fits_get_colnum (data_file, CASEINSEN,   "pha", &datacol, &error) ;
    if ( error ) {
      fprintf (stderr, "%s: cannot find PHA column\n", phProgName) ;
      error = 0 ;
      fits_get_colnum (data_file, CASEINSEN,   "pi", &datacol, &error) ;
      if ( error ) {
	fprintf (stderr, "%s: cannot find PI column\n", phProgName) ;
	*nchan = 1 ;
	error = 0 ;
      }
    }
    fits_get_colnum (data_file, CASEINSEN, "time", &timecol, &error) ;
    if ( error )
      fprintf (stderr, "%s: cannot find Time column\n", phProgName) ;
  }
  if ( error )
    fprintf (stderr, "%s: cannot operate on this file\n", phProgName) ;
  else {
    if ( datacol ) {
      sprintf (keyword, "TLMIN%d", datacol) ;
      fits_read_key   (data_file, TINT,    keyword,   &i,     line, &error) ;
      sprintf (keyword, "TLMAX%d", datacol) ;
      fits_read_key   (data_file, TINT,    keyword,   &j,     line, &error) ;
      numchan = error ? *nchan : j+1 ;
      error = 0 ;
    }
    else
      numchan = 1 ;
  }

  *nchan = numchan ;

  if ( debug )
    fprintf (stderr, "%s: numchan = %d\n",
	     phProgName, numchan) ;

  return ;
}
