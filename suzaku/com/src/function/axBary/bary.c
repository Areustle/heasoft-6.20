char *bcrcsid = "RCS: $Id: bary.c,v 1.4 2007/05/15 02:27:54 ishisaki Exp $" ;
/*-----------------------------------------------------------------------
 *
 *  bary.c
 *
 *  Date: 20 November 1997
 *
 *  Arnold Rots, USRA
 *
 *  bary contains a set of functions around barycorr, calculating barycenter
 *  corrections for, in principle, any observations.
 *  Required:
 *    bary.h
 *    dpleph.c
 *    cfitsio
 *
 *  Externally referenced:
 *    int baryinit (enum Observatory, char *, char *, double, double, char *, int) ;
 *    double barycorr (MJDTime *, double *, double *) ;
 *    int timeparms (char *, MJDTime *, double, int, PsrTimPar *, PsrBinPar *, int) ;
 *    double absphase (MJDTime *,  PsrTimPar *, int, PsrBinPar *,
 *                     int, double *, char *, int *) ;
 *    double xabsphase (double, PsrTimPar *, int, PsrBinPar *,
 *                     int, double *, char *, int *) ;
 *    fitsfile *openFFile (const char *name)
 *    double *scorbit (char *, MJDTime *, int *oerror)
 *    void met2mjd (double, MJDTime *) ;
 *    double mjd2met (MJDTime *) ;
 *    const char *convertTime (MJDTime *) ;
 *    const char *fitsdate ()
 *    double ctatv (long, double) ;
 *    void c200to405 (double *, int) ;
 *
 *  MET equivalents:
 *    double xbarycorr (double, double *, double *) ;
 *    double *xscorbit (char *, double, int *) ;
 *
 *  Internal:
 *    double TTtoTDB (MJDTime *) ;
 *    double TTtoTCG (MJDTime *) ;
 *    double TDBtoTCB (MJDTime *) ;
 *    double UTCtoTAI (MJDTime *) ;
 *    double toTT (MJDTime *) ;
 *    double toUTC (MJDTime *) ;
 *    double binorbit (MJDTime *, PsrBinPar *) ;
 *
 *  It is assumed that the environment variable $TIMING_DIR is defined
 *  and that the ephemeris file $TIMING_DIR/ephem.fits exists.
 *  It is also assumed that $TIMING_DIR/psrtime.dat and
 *  $TIMING_DIR/psrbin.dat exist.
 *  However, all these names are defined as mocros in bary.h.
 *
 *  Certain parts are adapted from software
 *  by Joseph M. Fierro, Stanford University, EGRET project,
 *  and from software provided with the JPL ephemeris.
 *
 *----------------------------------------------------------------------*/

#define BARY_CONST 1
#include "bary.h"
#undef BARY_CONST

static char ephemname[128] ;
static double c, msol, radsol ;
static int denum ;
static double met2day, day2met, met2sec  ;

/*-----------------------------------------------------------------------
 *
 *  met2mjd converts MET (seconds or days) to MJD day and fractional day.
 *  void met2mjd (double t, MJDTime *mjdTT)
 *   t:        MET time (s or d)
 *   mjdTT:    MJD (TT) day
 *
 *----------------------------------------------------------------------*/

void met2mjd (double t, MJDTime *mjdTT)
{
  long mjdTTint ;
  double mjdTTfr ;

  mjdTTint = (long) t * met2day ;
  mjdTTfr = t * met2day - mjdTTint + mjdRef.MJDfr ;
  mjdTTint += mjdRef.MJDint ;
  if ( mjdTTfr >= 1.0 ) {
    (mjdTTint)++ ;
    mjdTTfr-- ;
  }
  else if ( mjdTTfr < 0.0 ) {
    (mjdTTint)-- ;
    mjdTTfr++ ;
  }
  mjdTT->ts = mjdRef.ts ;
  mjdTT->MJDint = mjdTTint ;
  mjdTT->MJDfr = mjdTTfr ;
  return ;
}

/*-----------------------------------------------------------------------
 *
 *  mjd2met converts MJD day and fractional day to MET (seconds or days).
 *  double mjd2met (MJDTime *mjdTT)
 *    mjdTT:    MJD (TT) day
 *    return:   MET time (s or d)
 *
 *----------------------------------------------------------------------*/

double mjd2met (MJDTime *mjdTT)
{
  switch (mjdRef.ts ) {
  case TT:
    toTT (mjdTT) ;
    break ;
  case UTC:
    toUTC (mjdTT) ;
    break ;
  default:
    break ;
  }
  return ( (mjdTT->MJDint - mjdRef.MJDint +
	    mjdTT->MJDfr - mjdRef.MJDfr) * day2met ) ;
}

/*-----------------------------------------------------------------------
 *
 *  UTCtoTAI returns TAI-UTC
 *  double UTCtoTAI (long jdUTCint, double jdUTCfr)
 *   mjdUTC:   MJD (UTC) day
 *   return:   TAI - UTC (s)
 *  Only works for dates after 1 Jan 1961 (JD2437300.5)
 *
 *----------------------------------------------------------------------*/

double UTCtoTAI (MJDTime *mjdUTC)
{
  int i ;
  double dt = 0 ;

  while ( mjdUTC->MJDfr >= 1.0 ) {
    mjdUTC->MJDint++ ;
    mjdUTC->MJDfr-- ;
  }
  while ( mjdUTC->MJDfr < 0.0 ) {
    mjdUTC->MJDint-- ;
    mjdUTC->MJDfr++ ;
  }

  i = numleaps - 1 ;
  while ( ( mjdUTC->MJDint < leapsMJD[i] ) && i )
    i-- ;
  if ( i > 12 )
    dt = leapsecs[i] ;
  else
    dt = leapsecs[i] +
      (mjdUTC->MJDint - MJDoffset[i] + mjdUTC->MJDfr) * leapcoeff[i] ;

  return (double) dt ;
}

/*-----------------------------------------------------------------------
 *
 *  TTtoTDB calculates TDB-TT at time TT.
 *  double TTtoTDB (long jdTTint, double jdTTfr)
 *   mjdTT:   MJD (TT) day
 *   return:  TDB - TT (s)
 *
 *  It uses the coefficients from Fairhead & Bretagnon 1990,
 *  A&A 229, 240, as provided by ctatv.
 *  The accuracy is better than 100 ns.
 *  
 *  The proper way to do all this is to abandon TDB and use TCB.
 *
 *  The way this is done is as follows: TDB-TT and its derivative are
 *  calculated for the integer part of the Julian Day (when needed).
 *  The precise value is derived from fractional part and derivative.
 *
 *----------------------------------------------------------------------*/

double TTtoTDB (MJDTime *mjdTT) {
  static double tdbtdt ;
  static double tdbtdtdot ;
  static long oldmjd = 0 ;
  long l ;

  while ( mjdTT->MJDfr >= 1.0 ) {
    mjdTT->MJDint++ ;
    mjdTT->MJDfr-- ;
  }
  while ( mjdTT->MJDfr < 0.0 ) {
    mjdTT->MJDint-- ;
    mjdTT->MJDfr++ ;
  }

  if ( mjdTT->MJDint != oldmjd ) {
    oldmjd = mjdTT->MJDint ;
    l = oldmjd + 2400001 ;

    tdbtdt = ctatv (l, 0.0) ;
    tdbtdtdot = ctatv (l, 0.5) - ctatv (l, -0.5) ;
  }

  return ( tdbtdt + (mjdTT->MJDfr - 0.5) * tdbtdtdot ) ;
}

/*-----------------------------------------------------------------------
 *
 *  TDBtoTCB calculates TCB-TDB at time TDB.
 *  double TDBtoTCB (MJDTime *mjdTB)
 *   mjdTB:   MJD (TB) day
 *   return:  TCB - TDB (s)
 *
 *----------------------------------------------------------------------*/

double TDBtoTCB (MJDTime *mjdTB) {

  return LB * ( (mjdTB->MJDint - 43144) + mjdTB->MJDfr ) * SECDAY ;
}

/*-----------------------------------------------------------------------
 *
 *  TTtoTCG calculates TCG-TT at time TT.
 *  double TTtoTCG (MJDTime *mjdTB)
 *   mjdTB:   MJD (TB) day
 *   return:  TCG - TT (s)
 *
 *----------------------------------------------------------------------*/

double TTtoTCG (MJDTime *mjdTB) {

  return LG * ( (mjdTB->MJDint - 43144) + mjdTB->MJDfr ) * SECDAY ;
}

/*-----------------------------------------------------------------------
 *
 *  toTT converts mjd to TT
 *  double toTT (MJDTime *mjd)
 *   mjd:     MJD (?) day; converted
 *   return:  Change in seconds
 *
 *----------------------------------------------------------------------*/

double toTT (MJDTime *mjd) {

  double total = 0.0 ;

  switch ( mjd->ts ) {
  case TT:
    break ;
/*  from TDB and from TCB are not (yet) implemented  */
  case TDB:
  case TCB:
    break ;
  case UTC:
    total += UTCtoTAI (mjd) ;
  case TAI:
    total += TAItoTT ;
    mjd->MJDfr += total / SECDAY ;
    mjd->ts = TT ;
    break ;
  default:
    break ;
  }
  return total ;
}

/*-----------------------------------------------------------------------
 *
 *  toUTC converts mjd to UTC
 *  double toUTC (MJDTime *mjd)
 *   mjd:     MJD (?) day; converted
 *   return:  Change in seconds
 *
 *----------------------------------------------------------------------*/

double toUTC (MJDTime *mjd) {

  double total = 0.0 ;

  switch ( mjd->ts ) {
  case UTC:
    break ;
/*  from TDB and from TCB are not (yet) implemented  */
  case TDB:
  case TCB:
    break ;
  case TT:
    total -= TAItoTT ;
  case TAI:
    total -= UTCtoTAI (mjd) ;
    mjd->MJDfr += total / SECDAY ;
    mjd->ts = UTC ;
    break ;
  default:
    break ;
  }
  return total ;
}

const char *convertTime (MJDTime *mjd)
/*----------------------------------------------------------------------
 *  Convert MJD time to a FITS-type date and time string.
 *
 *  WARNING: ONLY GUARANTEED TILL ARNOLD'S RETIREMENT !!!
 *
 *  Input:  mjd            time to be converted (MJD)
 *
 *  Return:                string containing date and time as
 *                           yyyy-mm-ddThh:mm:ss or yy/mm/dd
 *
 ----------------------------------------------------------------------*/
{
  long mjdi ;
  double mjdf, t ;
  int day, mon, yr;
  int hr, min, sec;
  const int md[]={31,28,31,30,31,30,31,31,30,31,30,31} ;
  static char date[24] ;


/*     ---------------------
 *   - First, the day number -
 *   - All relative to 1902  -
 *     ---------------------
 */
  mjdi = mjd->MJDint ;
  mjdf = mjd->MJDfr ;
  while ( mjdf >= 1.0 ) {
    mjdf-- ;
    mjdi++ ;
  }
  while ( mjdf < 0.0 ) {
    mjdf++ ;
    mjdi-- ;
  }
  t = mjdf * SECDAY ;
  day = mjdi - 15750 ;

/*     ---------------------
 *   - Next, the time of day -
 *     ---------------------
 */
  hr = (int) (t / 3600.0);
  t -= hr * 3600;
  min = (int) (t / 60.0);
  sec = (int) (t - min * 60) ;

/*     -----------------------
 *   - Deal with the leap days -
 *     -----------------------
 */
  yr = 2 + (4*(day/1461));
  day %= 1461;
  if ( day == 789 ) {
    mon = 2;
    day = 29;
    yr += 2;
  }
  else {

/*     ---------------------
 *   - Correct for leap days -
 *     ---------------------
 */
    if ( day > 789 ) day--;

/*     ------------
 *   - Get the year -
 *     ------------
 */
    yr += (day/365);
    day %= 365;
    day++;

/*     -----------------
 *   - Get month and day -
 *     -----------------
 */
    mon = 0;
    while ( day > md[mon] ) {
      day -= md[mon];
      mon++;
    }
    mon++;
  }

/*     ------------------
 *   - Format the strings -
 *     ------------------
 */
  if ( yr < 96 )  /*  AXAF uses an early change-over date */
    sprintf (date, "%02d/%02d/%02d\0", day, mon, yr);

/*     ------------
 *   - Next century -
 *     ------------
 */
  else
    sprintf (date, "%04d-%02d-%02dT%02d:%02d:%02d\0",
	     yr+1900, mon, day, hr, min, sec) ;

/*     ----
 *   - Done -
 *     ----
 */
  return date ;
}

  const char *fitsdate ()
/*----------------------------------------------------------------------
 *  Convert current UTC date and time to a FITS-type date and time string.
 *
 *  Return:                string containing current UTC date and time as
 *                           yyyy-mm-ddThh:mm:ss
 *
 ----------------------------------------------------------------------*/
{
  time_t tt ;
  struct tm *tmptr ;
  static char str[48] ;

  time (&tt) ;
  tmptr = gmtime (&tt) ;
  strftime (str, 40, "%Y-%m-%dT%H:%M:%S", tmptr) ;

  return str ;
}

/*-----------------------------------------------------------------------
 *
 *  openAFile finds one of the ASCII system files and opens it for reading.
 *  FILE *openAFile (const char *name)
 *   name:     Name of the file
 *  openAFile first tries to find the file in TIMINGDIR, then in LHEADIR.
 *   
 *   return:   file pointer, NULL if not found
 *
 *----------------------------------------------------------------------*/
FILE *openAFile (const char *name)
{
  char fileName[1024] ;
  char *filepath = NULL ;
  FILE *FF = NULL ;

  if ( filepath = getenv (TIMINGDIR) ) {
    sprintf (fileName, "%s/%s", filepath, name) ;
    FF = fopen (fileName, "r") ;
  }

  if ( FF == NULL )
    if ( filepath = getenv (LHEADIR) ) {
      sprintf (fileName, "%s/%s", filepath, name) ;
      FF = fopen (fileName, "r") ;
    }

  if ( filepath == NULL )
    fprintf (stderr, "Neither $%s, nor $%s is defined\n", TIMINGDIR, LHEADIR) ;
  else if ( FF == NULL )
    fprintf (stderr, "File %s could not be found in %s\n", fileName, filepath) ;

  return FF ;
}

/*-----------------------------------------------------------------------
 *
 *  openFFile finds one of the FITS system files and opens it for reading.
 *  fitsfile *openFFile (const char *name)
 *   name:     Name of the file
 *  openFFile first tries to find the file in TIMINGDIR, then in LHEADIR.
 *   
 *   return:   FITS file pointer, NULL if not found
 *
 *----------------------------------------------------------------------*/
fitsfile *openFFile (const char *name)
{
  char fileName[1024] ;
  char *filepath = NULL  ;
  fitsfile *FF = NULL ;
  int error = 0 ;

  if ( filepath = getenv (TIMINGDIR) ) {
    sprintf (fileName, "%s/%s", filepath, name) ;
    fits_open_file (&FF, fileName, READONLY, &error) ;
  }

  if ( FF == NULL )
    if ( filepath = getenv (LHEADIR) ) {
      error = 0 ;
      sprintf (fileName, "%s/%s", filepath, name) ;
      fits_open_file (&FF, fileName, READONLY, &error) ;
    }

  if ( filepath == NULL )
    fprintf (stderr, "Neither $%s, nor $%s is defined\n", TIMINGDIR, LHEADIR) ;
  else if ( FF == NULL )
    fprintf (stderr, "File %s could not be found in %s\n", fileName, filepath) ;

  return FF ;
}

/*-----------------------------------------------------------------------
 *
 *  baryinit sets up the path to the ephemeris file and gets the leap
 *  seconds information.
 *  int baryinit (enum Observatory obs, char *fromts, char *tots,
 *                double mjdi, double mjdf, char *timeunit, int ephnum)
 *   obs:      Observatory (Unknown, Geocenter, XTE, AXAF)
 *   fromts:   Time system of input
 *   tots:     Time system to convert to
 *   mjdi:     MJDREFI; use mission default if =-1.0
 *   mjdf:     MJDREFF
 *   timeunit: Unit of time (s or d)
 *   ephnum:   Ephemeris number requested (200, 405; 0: default)
 *               if < 0: do not initialize ephemeris
 *   return:   denum; 0 upon error
 *
 *----------------------------------------------------------------------*/
int baryinit (enum Observatory obs, char *fromts, char *tots, double mjdi,
	      double mjdf, char *timeunit, int ephnum) {
  char from[5], to[5] ;
  int i ;
  FILE *FF ;
  char mon[16], dum1[16], dum2[16] ;

  mission = obs ;
/*
 *  Sort out time systems -----------------------------------------------
 */
  strncpy (from, fromts, 3) ;
  strncpy (to, tots, 3) ;
  for (i=0; i<3; i++) {
    if ( ( from[i] > 96 ) && ( from[i] < 123 ) )
      from[i] -= 32 ;
    if ( ( to[i] > 96 ) && ( to[i] < 123 ) )
      to[i] -= 32 ;
  }

  if ( !( strcmp(from, "TT") ) )
    fromTS = TT ;
  else if ( !( strcmp(from, "TDT") ) )
    fromTS = TT ;
  else if ( !( strcmp(from, "ET") ) )
    fromTS = TT ;
  else if ( !( strcmp(from, "TDB") ) )
    fromTS = TDB ;
  else if ( !( strcmp(from, "TB") ) )
    fromTS = TDB ;
  else if ( !( strcmp(from, "TCB") ) )
    fromTS = TCB ;
  else if ( !( strcmp(from, "TAI") ) )
    fromTS = TAI ;
  else if ( !( strcmp(from, "IAT") ) )
    fromTS = TAI ;
  else
    fromTS = UTC ;

  if ( !( strcmp(to, "TT") ) )
    toTS = TT ;
  else if ( !( strcmp(to, "TDT") ) )
    toTS = TT ;
  else if ( !( strcmp(to, "ET") ) )
    toTS = TT ;
  else if ( !( strcmp(to, "TDB") ) )
    toTS = TDB ;
  else if ( !( strcmp(to, "TB") ) )
    toTS = TDB ;
  else if ( !( strcmp(to, "TCB") ) )
    toTS = TCB ;
  else if ( !( strcmp(to, "TAI") ) )
    toTS = TAI ;
  else if ( !( strcmp(to, "IAT") ) )
    toTS = TAI ;
  else
    toTS = UTC ;

/*
 *  Set scale factors ---------------------------------------------------
 */
  day2met = ( *timeunit == 'd' ) ? 1.0 : SECDAY ;
  met2day = 1.0 / day2met ;
  met2sec = SECDAY / day2met ;

/*
 *  Get leap seconds ----------------------------------------------------
 *  19May2005 This block rewritten by Ingo.Kreykenbohm@obs.unige.ch (ISDC)
 *            to eliminate repeated reading of leapsecs file which could lead
 *            to buffer overruns
 */
  if (numleaps == 0) {
    FF = openAFile (TAIUTC) ;
    while ( fscanf (FF, "%d %s  1 =JD 24%d.5 %s %lg S + (MJD - %lg) X %lg %s",
		    &i, mon, leapsMJD+numleaps, dum1, leapsecs+numleaps,
		    MJDoffset+numleaps, leapcoeff+numleaps, dum2) == 8 )
      {
	numleaps++ ;
	if (numleaps == MAX_LEAPS) {
	  fprintf(stderr, "Baryinit: cannot handle more than %i leapseconds. Please adapt MAX_LEAP in bary.h and recompile.",MAX_LEAPS);
	  exit(-5);
	}
      }
    fclose (FF) ;
  }

/*
 *  Set the MJD Reference -----------------------------------------------
 */
  if ( ( obs == AXAF ) && ( mjdi == -1.0) ) {
    mjdRefi = 50814 ;
    mjdReff = 0.0 ;
  }
  else if ( ( obs != XTE ) || ( mjdi != -1.0) ) {
    mjdRefi = mjdi + mjdf ;
    mjdReff = mjdi - mjdRefi ;
    mjdReff += mjdf ;
  }

  mjdRef.MJDint = mjdRefi ;
  mjdRef.MJDfr = mjdReff ;
  mjdRef.ts = fromTS ;
/*
 *  If fromTS is ET or TAI, or (UTC and TIMEUNIT='s'),
 *  change it to TT and do the conversion in MJDRef.
 */
  switch ( fromTS ) {
  case UTC:
    if  ( day2met < 10.0 )
      break ;
  case ET:
  case TAI:
    fromTS = TT ;
    toTT (&mjdRef) ;
    break ;
  default:
    break ;
  }

/*
 *  Initialize clock and orbit ------------------------------------------
 */
  scorbitinit (mission) ;
  clockinit (mission) ;

/*
 *  Initialize ephemeris ------------------------------------------------
 */
  if ( ephnum >= 0 ) {
    if ( i = initephem (ephnum, &denum, &c, &radsol, &msol) ) {
      fprintf (stderr, "Error while initializing ephemeris; status: %d\n",
	       i) ;
      denum = 0 ;
    }
  }
  else
    denum = 1 ;

  return denum ;
}

/*-----------------------------------------------------------------------
 *
 *  timeinit sets things up for time conversions and gets the leap
 *  seconds information; no ephemeris initialization.
 *  int timeinit (enum Observatory obs, char *fromts, char *tots,
 *                double mjdi, double mjdf, char *timeunit)
 *   obs:      Observatory (Unknown, Geocenter, XTE, AXAF)
 *   fromts:   Time system of input
 *   tots:     Time system to convert to
 *   mjdi:     MJDREFI
 *   mjdf:     MJDREFF
 *   timeunit: Unit of time (s or d)
 *   return:   0 upon error
 *
 *----------------------------------------------------------------------*/
int timeinit (enum Observatory obs, char *fromts, char *tots, double mjdi,
	      double mjdf, char *timeunit) {
  return baryinit (obs, fromts, tots, mjdi, mjdf, timeunit, -1) ;
}

/*-----------------------------------------------------------------------
 *
 *  MET time interface to main function barycorr:
 *  double xbarycorr (double t, double *sourcedir, double *scposn)
 *   t:         MET time of photon/pulse arrival
 *   sourcedir: Direction cosines of source position
 *   scposn:    Spacecraft position vector in meters
 *
 *
 *  Method:
 *   Convert to JD and call barycorr.
 *   Return calculated delay (in s)
 *
 *----------------------------------------------------------------------*/

double xbarycorr (double t, double *sourcedir, double *scposn)
{
  MJDTime mjdTT ;

/*
 *  Convert to MJD ------------------------------------------------------
 */
  met2mjd (t, &mjdTT) ;

/*
 *  Get ephemeris data --------------------------------------------------
 */
  return barycorr (&mjdTT, sourcedir, scposn) ;
}

/*-----------------------------------------------------------------------
 *
 *  Main function:
 *  double barycorr (MJDTime *mjdTT, double *sourcedir, double *scposn)
 *   mjdXT:     Time of photon/pulse arrival in MJD(??)
 *   sourcedir: Direction cosines of source position
 *   scposn:    (Spacecraft) position vector in meters wrt geocenter
 *              Note: it usually does not matter if provided in FK5
 *                    or DE200 frame when using DE405
 *
 *
 *  Method:
 *   Compute index of ephemeris memory corresponding to input date
 *   If (input date is not in memory) then
 *    Load next 32 days of ephemeris data into memory
 *   Calculate position of Sun, Earth, and velocity of Earth
 *   Calculate SSBC-to-S/C vector and Sun-to-S/C vector
 *   Calculate distance and angular size of sun
 *  [ If (object was blocked by sun)             ]  commented out
 *  [   Return (no delay calculated)             ]
 *   Add all propagation effects   
 *   Return calculated delay (in s)
 *
 *  All distances are in light seconds, velocities in ls/s.
 *
 *----------------------------------------------------------------------*/

double barycorr (MJDTime *mjdXT, double *sourcedir, double *scposn)
{
  const double *rce, *rcs, *vce;
  double rca[3], rsa[3];
  const double *eposn ;
  double total, sundis, sunsiz, cth ;
  MJDTime mjdXX ;
  MJDTime *mjdTT ;
  int i;
  int iearth = 3 ;
  int isun = 11 ;
  double jdt[2] ;
  MJDTime mjdTDB ;

/*
 *  Sort out time systems -----------------------------------------------
 */
  total = 0.0 ;
  switch ( mjdXT->ts ) {
  case TT:
    mjdTT = mjdXT ;
    break ;
  case TDB:
  case TCB:
    return 0.0 ;
  case UTC:
  case TAI:
  default:
    mjdTT = &mjdXX ;
    mjdTT->MJDint = mjdXT->MJDint ;
    mjdTT->MJDfr = mjdXT->MJDfr ;
    mjdTT->ts = mjdXT->ts ;
    total += toTT (mjdTT) ;
    break ;
  }

/*
 *  Check observatory position ------------------------------------------
 */
  if ( scposn == NULL ) {
    fprintf (stderr, "barycorr: Invalid Observatory/Spacecraft position vector\n") ;
    return 0.0 ;
  }

/*
 *  Get all positions ---------------------------------------------------
 */
  jdt[0] = mjdTT->MJDint + 2400001 ;
  jdt[1] = mjdTT->MJDfr - 0.5 ;
  if ( (eposn = dpleph (jdt, iearth, isun)) == NULL ) {
    fprintf (stderr, "barycorr: Could not find solar system ephemeris for MJD %f\n",
	     mjdTT->MJDint) ;
    return 0.0 ;
  }
  rce = eposn ;
  vce = eposn + 3 ;
  rcs = eposn + 6 ;
  for (i = 0; i < 3; i++) {
    rca[i] = rce[i] + scposn[i]/c;         /* SSBC-to-S/C vector */
    rsa[i] = rca[i] - rcs[i];              /* Sun-to-S/C vector */
  }

/*
 *  Calculate the time delay due to the gravitational field of the Sun --
 *  (I.I. Shapiro, Phys. Rev. Lett. 13, 789 (1964)). --------------------
 */ 
  sundis = sqrt(DOT(rsa, rsa));
  cth = DOT(sourcedir, rsa)/sundis;

/*
 *  Eclipsed by the sun
 *  sunsiz = radsol/sundis;
 *  if ((cth + 1) < 0.5*sunsiz*sunsiz)
 *    return (0);
 */

/*
 *  Sum all propagation effects -----------------------------------------
 */
  total += TTtoTDB (mjdTT)
           + DOT(sourcedir, rca)
	     + DOT(scposn, vce)/c
	       + 2*msol*log(1+cth);

/*
 *  Sort out time systems -----------------------------------------------
 */
  switch ( toTS ) {
  case TCB:
    mjdTDB.MJDint = mjdTT->MJDint ;
    mjdTDB.MJDfr = mjdTT->MJDfr + total/SECDAY ;
    mjdTDB.ts = TDB ;
    total += TDBtoTCB (&mjdTDB) ;
    break ;
  default:
    break ;
  }

  return total ;
}

/*-----------------------------------------------------------------------
 *
 *  timeparms gets the timing parameters from the pulsar database.
 *  
 *  char *insource:  source name (J1234-5643, B1234-56, or 1234-56)
 *  MJDTime *time:   MJD(TB) time for which the parameters are requested
 *  double roffset:  extra offset to be applied to radio zero phase (in s)
 *  int bin:         binary pulsar?
 *  PsrTimPar *ptp:  pulsar timing parameters struct
 *  PsrBinPar *pbp:  pulsar binary orbit parameters struct
 *  int debug:       chatty
 * 
 *----------------------------------------------------------------------*/

int timeparms (char *insource, MJDTime *time, double roffset, int bin,
	       PsrTimPar *ptp, PsrBinPar *pbp, int debug)
{
  FILE *PD ;
  int i, j ;
  int error ;
  char filename[256] ;
  char line[256] ;
  char line2[256] ;
  double mjd[2] ;
  long dt ;
  char bsource[32] ;
  char jsource[32] ;
  char *dsource ;
  char *source ;
  char binsource[32] ;
  int rah, ram, decd, decm ;
  double ras, decs, mjdf, fd, dfd, ddfd, dddfd, rms, lastrms, mjdftrun ;
  double tr, dtt, dir[3] ;
  MJDTime mtime ;
  double scposn0[3] = {0.0, 0.0, 0.0} ;
  long mjd1, mjd2, mjdi, mjdt, lastmjd2 ;
  char sdec[12] ;
  int nocover = 0 ;
  int tdenum, fbin ;
  double ra, dec ;

  int timeline (char*, char*, double*, double*, long*, long*, long*, double*,
		double*, double*, double*, double*, double*, int*, int*, char*) ;

/*
 *  Sort out the name  ---------------------------------------------------
 */
  if ( ( *insource == 'J' ) || ( *insource == 'j' ) ) {
    source = insource + 1 ;
    dsource = jsource ;
  }
  else if ( ( *insource == 'B' ) || ( *insource == 'b' ) ) {
    source = insource + 1 ;
    dsource = bsource ;
  }
  else {
    source = insource ;
    dsource = bsource ;
  }

/*
 *  Open the database ----------------------------------------------------
 */
  error = 0 ;
  if ( ( PD = openAFile (PSRTIME) ) == NULL ) {
    fprintf (stderr, "Could not open file %s\n", PSRTIME) ;
    error = 1 ;
  }

/*
 *  Get integer part of MJD ----------------------------------------------
 */
  mjdt = time->MJDint ;

/*
 *  Search the database --------------------------------------------------
 */
  fgets (line, 256, PD) ;
  fgets (line, 256, PD) ;
  strcpy (dsource, "  ") ;
  while ( strcmp (dsource, source) && !error ) {
    fgets (line, 256, PD) ;
    if ( feof(PD) )
      error = 2 ;
    timeline (line, bsource, &ra, &dec, &mjd1, &mjd2, &mjdi, &mjdf,
	      &fd, &dfd, &ddfd, &dddfd, &rms, &fbin, &tdenum, jsource) ;
  }
  if ( error > 1 )
    fprintf (stderr, "Could not find %s in file %s\n", insource, PSRTIME) ;

/*
 *  Locate the right record ----------------------------------------------
 */
  if ( !error ) {
    timeline (line, bsource, &ra, &dec, &mjd1, &mjd2, &mjdi, &mjdf,
	      &fd, &dfd, &ddfd, &dddfd, &rms, &fbin, &tdenum, jsource) ;
    strcpy (binsource, bsource) ;
    if ( mjdt < mjd1 )
      nocover = 1 ;
  }
  while ( ( mjdt > mjd2 ) && !error ) {
    lastmjd2 = mjd2 ;
    fgets (line2, 256, PD) ;
    if ( feof(PD) ) {
      nocover = 1 ;
      break ;
    }
    timeline (line2, bsource, &ra, &dec, &mjd1, &mjd2, &mjdi, &mjdf,
	      &fd, &dfd, &ddfd, &dddfd, &rms, &fbin, &tdenum, jsource) ;
    if ( strcmp (dsource, source) ) {
      break ;
      nocover = 1 ;
    }
    if ( mjdt < mjd1 ) {
      nocover = 1 ;
      if ( (mjdt - lastmjd2) < (mjd1 - mjdt) )
	break ;
    }
    strncpy (line, line2, 256) ;
  }

/*
 *  Get the data ---------------------------------------------------------
 */
  if ( !error ) {

/*
 *  Find entry with lowest rms  ------------------------------------------
 */
    if ( !nocover ) {
      timeline (line, bsource, &ra, &dec, &mjd1, &mjd2, &mjdi, &mjdf,
		&fd, &dfd, &ddfd, &dddfd, &rms, &fbin, &tdenum, jsource) ;
      lastrms = rms ;
/*      printf ("First valid entry %d to %d at %f with rms %f\n",
 *	      mjd1, mjd2, (double) mjdi+mjdf, rms) ; 
 */
      while ( 1 ) {
	fgets (line2, 256, PD) ;
	if ( feof(PD) ) {
	  break ;
	}
	timeline (line2, bsource, &ra, &dec, &mjd1, &mjd2, &mjdi, &mjdf,
		  &fd, &dfd, &ddfd, &dddfd, &rms, &fbin, &tdenum, jsource) ;
	if ( strcmp (dsource, source) )
	  break ;
	if ( ( mjdt >= mjd1 ) && ( mjdt <= mjd2 ) && ( rms < lastrms ) ) {
	  strncpy (line, line2, 256) ;
	  lastrms = rms ;
/*	  printf ("Changed to entry %d to %d at %f with rms %f\n",
 *		  mjd1, mjd2, (double) mjdi+mjdf, rms) ; 
 */
	}
      }
    }
/*
 *  Really get the data --------------------------------------------------
 */
    timeline (line, bsource, &ra, &dec, &mjd1, &mjd2, &mjdi, &mjdf,
	      &fd, &dfd, &ddfd, &dddfd, &rms, &fbin, &tdenum, jsource) ;
    ptp->ra = ra ;
    ptp->dec = dec ;
    ptp->denum = tdenum ;
    if ( debug )
      printf ("Source: %s, RA = %.8f,  Dec = %.8f,  Frame = DE%d\n",
	      dsource, ptp->ra, ptp->dec, ptp->denum) ;
    dir[0] = cos(ptp->ra/RADEG) * cos(ptp->dec/RADEG) ;
    dir[1] = sin(ptp->ra/RADEG) * cos(ptp->dec/RADEG) ;
    dir[2] = sin(ptp->dec/RADEG) ;
    if ( denum != ptp->denum ) {
      fprintf (stderr, "Warning: You are using Solar System Ephemeris DE%d\n",
	       denum) ;
      fprintf (stderr, "         with a Timing Ephemeris produced with DE%d\n",
	       ptp->denum) ;
      if ( ( denum == 405 ) && ( ptp->denum == 200 ) ) {
	c200to405 (dir, debug) ;
      fprintf (stderr, "         The coordinates will be transformed to DE405\n") ;
      }
    }
    ptp->f = fd ;
    ptp->df = dfd ;
    ptp->ddf = ddfd ;
    ptp->dddf = dddfd ;
    i = mjdf * 100.0 + 0.5 ;
    mjdftrun = (double) i / 100.0 ;
    ptp->t0.MJDint = mjdi ;
    ptp->t0.MJDfr = mjdftrun ;
    ptp->t0.ts = TDB ;

/*
 *  Convert UTC -> MET ----------------------------------------------------
 */
    mtime.MJDint = mjdi ;
    mtime.MJDfr = mjdf + roffset / SECDAY ;
    mtime.ts = UTC ;
    toTT (&mtime) ;
    if ( debug ) {
      printf ("Data MJD(TDB): %d\n", mjdt) ;
      printf ("Used radio pulsar database entry covering the period:\n") ;
      printf ("  MJD(TDB) %d to %d; rms: %f milliperiod\n", mjd1, mjd2, rms) ;
      if ( nocover )
	printf ("===> Note that no entry could be found covering the observations.\n") ;
      printf ("Frequency/period ephemeris (T0):\n") ;
      printf ("  MJD(TDB) = %d + %g\n", mjdi, mjdftrun) ;
      printf ("  f = %.14f  df/dt = %g  d2fdt2 = %g  d3fdt3 = %g\n", fd, dfd, ddfd, dddfd) ;
      printf ("Radio pulse TOA ephemeris (Tr):\n  MJD(TT)  = %d + %g\n",
	      mtime.MJDint, mtime.MJDfr) ;
    }
    tr = barycorr (&mtime, dir, scposn0) ;
    dtt = tr + ( (mtime.MJDint - ptp->t0.MJDint) + (mtime.MJDfr - ptp->t0.MJDfr) ) * SECDAY ;
    ptp->radioph = -fd*dtt - dfd*dtt*dtt*0.5 - ddfd*dtt*dtt*dtt/6.0 - dddfd*dtt*dtt*dtt*dtt/24.0 ;
    if ( debug )
      printf ("  TDB-TT = %18.17g\n  DT (=Tr-T0) =%10.9g  Phase(T0) = %10.9g\n",
	      tr, dtt, ptp->radioph) ;
  }
  fclose (PD) ;

/*
 *  Binary orbits --------------------------------------------------------
 */
  if ( fbin ) {
    if ( bin )
      fbin = 0 ;
    else
      bin = 1 ;
  }
  if ( bin && !error ) {
    if ( ( PD = openAFile (PSRBIN) ) == NULL ) {
      fprintf (stderr, "Could not open file %s\n", PSRBIN) ;
      error = -1 ;
    }

/*
 *  Search the database --------------------------------------------------
 */
    fgets (line, 256, PD) ;
    fgets (line, 256, PD) ;
    strcpy (dsource, "  ") ;
    while ( strcmp (dsource, binsource) && !error ) {
      fgets (line, 256, PD) ;
      if ( feof(PD) )
	error = -2 ;
      sscanf (line, "%s", dsource) ;
    }
    if ( error < -1 )
      fprintf (stderr, "Could not find %s in file %s\n", insource, PSRBIN) ;

/*
 *  Locate and read the right record -------------------------------------
 */
    if ( !error ) {
      while ( 1 ) {
	fgets (line2, 256, PD) ;
	if ( feof (PD) || strncmp (line2, binsource, 7) )
	  break ;
	strncpy (line, line2, 256) ;
      }
      for (i=0; i<strlen(line); i++)
	if ( line[i] == 'D' )
	  line[i] = 'E' ;
      sscanf (line, "%s %lg %lg %lg %ld%lg %lg %lg %lg %lg",
	      dsource, &(pbp->pb), &(pbp->a1sini), &(pbp->e), &mjdi, &mjdftrun,
	      &(pbp->omega), &(pbp->omdot), &(pbp->gamma), &(pbp->pbdot)) ;
      pbp->t0.MJDint = mjdi ;
      pbp->t0.MJDfr = mjdftrun ;
      pbp->t0.ts = TDB ;
      pbp->omega /= RADEG ;
      pbp->omdot /= RADEG * 365.25 * SECDAY ;
      if ( debug ) {
	printf (" Binary orbit (%s):\n", binsource) ;
	printf ("  MJD = %d + %g\n",
		pbp->t0.MJDint, pbp->t0.MJDfr) ;
	printf ("  pb = %g  a1sini = %g  e = %g\n",
		pbp->pb, pbp->a1sini, pbp->e) ;
      }
    }
    fclose (PD) ;
  }

/*
 *  Return error ---------------------------------------------------------
 */
  if ( error > 0 || nocover ) {
    fprintf (stderr, "===>  Please note that a suitable timing ephemeris entry\n") ;
    fprintf (stderr, "      could not be found in the pulsar timing database.\n") ;
    fprintf (stderr, "      You may want to get a fresh copy of:\n") ;
    fprintf (stderr, "        ftp://pulsar.princeton.edu/gro/jcat\n") ;
    fprintf (stderr, "      and deposit it in $TIMING_DIR/psrtime.dat, \n") ;
    fprintf (stderr, "      or you may want to make your own timing ephemeris and\n") ;
    fprintf (stderr, "      put it in the proper format  into that file.\n") ;
  }
  else if ( error ) {
    fprintf (stderr, "===>  Please note that a suitable binary orbit ephemeris entry\n") ;
    fprintf (stderr, "      could not be found in the pulsar timing database.\n") ;
    fprintf (stderr, "      You may want to get a fresh copy of:\n") ;
    fprintf (stderr, "        ftp://pulsar.princeton.edu/gro/psrbin.dat\n") ;
    fprintf (stderr, "      and deposit it in $TIMING_DIR/psrbin.dat, or you may want\n") ;
    fprintf (stderr, "      to make your own binary orbit ephemeris and put it in\n") ;
    fprintf (stderr, "      the proper format into that file.\n") ;
  }
  if ( fbin ) {
    fprintf (stderr, "===>  Please note that the pulsar database indicated that this pulsar\n") ;
    fprintf (stderr, "      is a binary pulsar and faseBin will try to treat it as such.\n") ;
    error = - 3 ;
  }
  return error ;
}

/*-----------------------------------------------------------------------
 *
 *  timeline reads an entry from psrtime.dat.
 * 
 *----------------------------------------------------------------------*/

timeline (char *line, char *bsource, double *ra, double *dec,
	  long *mjd1, long *mjd2, long *mjdi, double *mjdf,
	  double *fd, double *dfd, double *ddfd, double *dddfd,
	  double *rms, int *fbin, int *denum, char *jsource) {

  int i ;
  int rah, ram, decd, decm ;
  double ras, decs ;
  char sdec[12], obs[12], bino, dum1[16], dum2[16] ;
  int dfexp, ddfexp, dddfexp, nitem ;

  /*  Remove trailing blanks  */
  while ( line[strlen(line) - 1] == ' ' )
    line[strlen(line) - 1] = 0 ;

  /*  Read most of the items  */
  nitem = sscanf (line,
		  "%s %d %d %lg %s %d %lg %ld %ld %ld%lg %lg %lgD%3d%lgD%3d%lg %c %c %s %s %lgD%3d",
		  bsource, &rah, &ram, &ras, &sdec, &decm, &decs,
		  mjd1, mjd2, mjdi, mjdf, fd, dfd, &dfexp, ddfd, &ddfexp, rms,
		  obs, &bino, dum1, dum2, dddfd, &dddfexp) ;
  obs[1] = 0 ;

  *dfd *= pow (10.0, dfexp) ;
  *ddfd *= pow (10.0, ddfexp) ;
  /*  J source name and/or DE number, if present (backward compatibility) */
  /*  added quartic term, as well  */
  *jsource = 0 ;
  *denum = 200 ;
  if ( nitem > 19 ) {
    if ( strncmp (dum1, "DE", 2) )
      strcpy (jsource, dum1) ;
    else {
      sscanf (dum1, "DE%d", denum) ;
      strcpy (jsource, dum2) ;
    }
    if ( nitem < 22 )
      *dddfd = 0.0 ;
    else if ( nitem > 22 )
      *dddfd *= pow (10.0, dddfexp) ;
  }

  /*  Marked as binary?  */
  *fbin = ( bino == '*' ) ? 1 : 0 ;

  /*  RA and Dec  */
  decd = abs ( atoi (sdec) ) ;
  *ra = ( (double) rah + ram / 60.0 + ras / 3600.0 ) * 15.0 ;
  *dec = (double) decd + decm / 60.0 + decs / 3600.0 ;
  if ( sdec[0] == '-' )
    *dec = -(*dec) ;

  return 0 ;

}

/*-----------------------------------------------------------------------
 *
 *  c200to405 converts a direction cosine vector from the DE200 frame
 *  (nominally FK5, but not quite) to the DE405 frame (ICRS).
 *  
 *  double dir[3]    direction cosine vector (in and out)
 *  int debug        Whether or not to print the conversion
 *  
 *  The formula, according to Standish, is:
 *  
 *    dir(DE405) = dir(DE200) + eps x dir(DE200)
 *  
 *  where eps' = (0.002, 0.012, 0.006) / 206265
 * 
 *----------------------------------------------------------------------*/

void c200to405 (double *dir, int debug) {
  double dir200[3] ;
  double x=0.002/206265.0, y=0.012/206265.0, z=0.006/206265.0 ;
  int i ;

  for (i=0; i<3; i++)
    dir200[i] = dir[i] ;
  dir[0] += y * dir200[2] - z * dir200[1] ;
  dir[1] += z * dir200[0] - x * dir200[2] ;
  dir[2] += x * dir200[1] - y * dir200[0] ;

  if ( debug ) {
    printf ("Conversion      DE200     to    ICRS:\n") ;
    printf ("   X         %12.9f    %12.9f\n", dir200[0], dir[0]) ;
    printf ("   Y         %12.9f    %12.9f\n", dir200[1], dir[1]) ;
    printf ("   Z         %12.9f    %12.9f\n", dir200[2], dir[2]) ;
  }
  return ;
}

/*-----------------------------------------------------------------------
 *
 *  xabsphase calculates absolute phase
 *  It is the MET interface to absphase.
 *  double time:     MET time for which the parameters are requested
 *  PsrTimPar *ptp:  pulsar timing parameters struct
 *  int binary:      binary pulsar?
 *  PsrBinPar *pbp:  pulsar binary orbit parameters struct
 *  int bcorr:       apply barycenter correction?
 *  double dir[3]:   source position vector
 *  char *oefile:    orbit ephemeris filename
 *  int *error:      error (status) flag
 * 
 *----------------------------------------------------------------------*/

double xabsphase (double time,  PsrTimPar *ptp, int binary, PsrBinPar *pbp,
		 int bcorr, double *dir, char *oefile, int *error)
{
  MJDTime mt ;
  
  met2mjd (time, &mt) ;
  return absphase (&mt, ptp, binary, pbp, bcorr, dir, oefile, error) ;
}

/*-----------------------------------------------------------------------
 *
 *  absphase calculates absolute phase
 *  
 *  MJDTime *time:   MJD(TDB) time for which the parameters are requested
 *  PsrTimPar *ptp:  pulsar timing parameters struct
 *  int binary:      binary pulsar?
 *  PsrBinPar *pbp:  pulsar binary orbit parameters struct
 *  int bcorr:       apply barycenter correction?
 *  double dir[3]:   source position vector
 *  char *oefile:    orbit ephemeris filename
 *  int *error:      error (status) flag
 * 
 *----------------------------------------------------------------------*/

double absphase (MJDTime *time, PsrTimPar *ptp, int binary, PsrBinPar *pbp,
		 int bcorr, double *dir, char *oefile, int *error)
{
  double dt ;
  double dt2 ;
  double dt3 ;
  double dt4 ;
  double dtf ;
  int fi ;
  double ff ;
  double phase=0.0 ;
  double binphase ;

  if ( bcorr ) {
    time->MJDfr += barycorr (time, dir, scorbit(oefile,time,error)) / SECDAY ;
    time->ts = TDB ;
  }

  if ( !(*error) ) {

    if ( binary )
      time->MJDfr += binorbit (time, pbp) / SECDAY ;

    dt = time->MJDint - ptp->t0.MJDint ;
    dt += time->MJDfr - ptp->t0.MJDfr ;
    dt *= SECDAY ;
    dt2 = 0.5 * dt * dt ;
    dt3 = dt * dt2 / 3.0 ;
    dt4 = dt2 * dt2 / 6.0 ;
    dtf = dt - (int) dt ;
    fi = ptp->f ;
    ff = ptp->f - fi ;
    phase = fi*dtf + ff*dt + ptp->df*dt2 + ptp->ddf*dt3 + ptp->dddf*dt4 + ptp->radioph ;
    phase -= (int) phase ;
    if ( phase < 0.0 )
      phase++ ;
  }

  return phase ;
}

/*-----------------------------------------------------------------------
 *
 *  binorbit calculates binary orbit delay.
 *  
 *  MJDTime *time:   MJD(TDB) time for which the correction is requested
 *  PsrBinPar *pbp:  pulsar binary orbit parameters struct
 * 
 *----------------------------------------------------------------------*/

double binorbit (MJDTime *time, PsrBinPar *pbp)
{
  double torb ;
  double orbph ;
  double t ;
  double dt ;
  double dt2 ;
  double e ;
  double ep, dep, denom ;
  double alpha, beta, omega, sbe, cbe, q ;

  e = pbp->e ;
  t = time->MJDint - pbp->t0.MJDint ;
  t += time->MJDfr - pbp->t0.MJDfr ;
  t *= SECDAY ;
  dt = t / pbp->pb ;
  dt2 = 0.5 * dt * dt ;
  orbph = dt - dt2 * pbp->pbdot ;
  orbph -= (int) orbph ;
  if ( orbph < 0.0 )
    orbph++ ;
  orbph *= TWOPI ;
  ep = orbph + e*sin(orbph)*(1 + e*cos(orbph)) ;
  denom = 1.0 - e*cos(ep) ;
 
  dep = 1.0 ;
  while ( fabs(dep) > 1.0e-12 ) {
     dep = ( orbph - ( ep - e * sin(ep) ) ) / denom ;
     ep += dep ;
  }

  omega = pbp->omega + pbp->omdot * t ;
  alpha = pbp->a1sini * sin(omega) ;
  beta = pbp->a1sini * cos(omega) * sqrt(1 - e * e) ;
  sbe = sin(ep) ;
  cbe = cos(ep) ;
  q = alpha * (cbe - e) + (beta + pbp->gamma) * sbe ;
  torb = -q + (TWOPI / pbp->pb) * q * (beta * cbe - alpha * sbe) / (1 - e * cbe) ;

  return torb ;
}
