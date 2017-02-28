char *dplrcsid = "RCS: $Id: dpleph.c,v 1.1 2001/02/21 21:32:25 miket Exp $" ;
/*-----------------------------------------------------------------------
 *
 *  dpleph.c
 *
 *  Date: 20 November 1997
 *
 *  Arnold Rots, USRA
 *
 *  dpleph.c contains a collection of C functions to read and interpolate
 *  the JPL ephemeris.
 *
 *  Requires:
 *    bary.h
 *    bary.c
 *    cfitsio
 *
 *  Externally referenced:
 *    int initephem (int ephnum, int *denum, double *c, double *radsol,
 *                   double *msol)
 *    const double *dpleph (double *jd, int ntarg, int ncent)
 *
 *  Internal:
 *    int state (double *jd, int ntarg, int ncent, double *posn)
 *    int getstate (int ntarg, double t1, double *posn)
 *    int interp (double *buf, double t1, int ncf, int na, double *pv)
 *    int readephem (long recnum)
 *    double findcval (char **cnam, double *cval, long n, char *name)
 *
 *  To find and open the ephemeris file, the function openFFile from
 *  bary.c is used.  The environment variables used, and the name of
 *  the ephemeris file (macro EPHEM), are defined in bary.h.
 *  It is assumed that the environment variables $TIMING_DIR or $LHEA_DATA
 *  are defined and that the ephemeris files exist in at least one of the
 *  two.  The ephemeris files are JPLEPH.200 and JPLEPH.405, while JPLEPH
 *  should point to the default.
 *
 *  Largely adapted from the standard Fortran JPL ephemeris software.
 *
 *----------------------------------------------------------------------*/

#include "bary.h"
static char ephfile[1024] ;
static long irecsz ;   /* length of ephemeris records (no. of doubles)  */
static double ss1, ss2, ss3, ss3inv ;
static long currec = 0 ;
static double emratinv ; /* emratinv = 1.0 / (1.0 + emrat) */
static double *buffer=NULL ;
static long iptr[13], ncf[13], na[13] ;
static long buflen ;
static long nrecs ;
static double clight ;
static double au ;
static double aufac, velfac ;

int state (double *, int, int, double *) ;
int getstate (int, double, double *) ;
int interp (double *, double, int, int, double *) ;
int readephem (long) ;
double findcval (char **, double *, long, char *) ;

/*-----------------------------------------------------------------------
 *
 *  const double *dpleph (double *jd, int ntarg, int ncent)
 *
 *    double[2] jd      JD time for which ephemeris is requested
 *    int       ntarg   Target for which position is requested
 *    int       ncent   Center for which position is requested
 *
 *  dpleph returns a pointer to an array containing the concatenated state
 *  vectors (X, Y, Z, VX, VY, VZ) of the target and the center with respect
 *  to the barycenter of the solar system at time jd[0]+jd[1] (in JD(TT)).
 *
 *  The numbering convention for ntarg and ncent is:
 *    1 = Mercury            8 = Neptune
 *    2 = Venus              9 = Pluto
 *    3 = Earth             10 = Moon
 *    4 = Mars              11 = Sun
 *    5 = Jupiter           12 = Solar system barycenter
 *    6 = Saturn            13 = Earth-Moon barycenter
 *    7 = Uranus            14 = Nutations (longitude and obliquity; untested)
 *                          15 = Librations 
 *  This numbering scheme is 1-relative, to be consistent with the
 *  Fortran version.
 *
 *  posn[12]   Interpolated quantities requested:
 *               posn[0:2] ntarg position    posn[3:5]  ntarg velocity
 *               posn[6:8] ncent position    posn[9:11] ncent velocity
 *               units are lightseconds and lightseconds/s
 *
 *               posn[0:1] ntarg long/lat    posn[2:3]  ntarg rates
 *               posn[0:2] ntarg Euler angs  posn[3:5]  ntarg rates
 *               units are radians and radians/s (librations)
 *
 *----------------------------------------------------------------------*/

const double *dpleph (double *jd, int ntarg, int ncent)
{
  static double posn[12] ;
  long jdint ;
  double jdtmp ;
  int i ;

/*
 *     Make sure jd is in proper range
 */
  jdint = (long) jd[0] ;
  jdtmp = (double) jd[0] - jdint ;
  jd[0] = (double) jdint ;
  jd[1] += jdtmp ;
  while ( jd[1] >= 0.5 ) {
    jd[1]-- ;
    jd[0]++ ;
  }
  while ( jd[1] < -0.5 ) {
    jd[1]++ ;
    jd[0]-- ;
  }

/*
 *      Get the positions and return
 */
  if ( state (jd, ntarg, ncent, posn) )
    return (const double *) NULL ;
  else {
    return (const double *) posn ;
  }
}

/*-----------------------------------------------------------------------
 *
 *  int state (double *jd, int ntarg, int ncent, double *posn)
 *     This function reads and interpolates the JPL ephemeris,
 *     returning position and velocity of the bodies ntarg and ncent
 *     with respect to the solar system barycenter at JD (TT) time
 *     jdint + jdfr.
 *
 *     Arguments:
 *       Input:
 *           jd[0]   JD (TT) - integer part
 *           jd[1]   JD (TT) - fractional part (-0.5 <= jdfr < +0.5)
 *           ntarg   Target number
 *           ncent   Center number
 *       Output:
 *        posn[12]   Interpolated quantities requested:
 *                     posn[0:2] ntarg position  posn[3:5]  ntarg velocity
 *                     posn[6:8] ncent position  posn[9:11] ncent velocity
 *                     units are lightseconds and lightseconds/s
 *
 *                     posn[0:1] ntarg long/lat  posn[2:3]  ntarg rates
 *                     posn[0:2] ntarg Euler angs posn[3:5] ntarg rates
 *                     units are radians and radians/s (librations)
 *
 *----------------------------------------------------------------------*/

int state (double *jd, int ntarg, int ncent, double *posn)
{
  double t, t1, t2 ;
  int i ;
  long recnum ;

/*
 *      Reference to most recent midnight
 */
  t1 = jd[0] - 0.5 ;
  t2 = jd[1] + 0.5 ;
  t = t1 + t2 ;

/*
 *       Error return for epoch out of range
 */
  if ( ( t < ss1 ) || ( t > ss2 ) ) {
    fprintf (stderr, "dpleph[state]: Time %f outside range of ephemeris\n",
	     t) ;
    return 1 ;
  }

/*
 *       Calculate record # and relative time in interval
 */
  recnum = (int) ((double) (t1 - ss1) * ss3inv) + 1 ;
  if ( t1 == ss2 )
    recnum-- ;
  t1 = ((t1 - ((double) (recnum - 1) * ss3 + ss1)) + t2) * ss3inv ;

/*
 *       Read correct record if not in memory
 */
  if ( recnum != currec ) {
    currec = recnum ;
    if ( readephem (recnum) ) {
      fprintf (stderr, "dpleph[state]: Read failure in ephemeris file, record %d\n",
	       recnum) ;
      return 2 ;
    }
  }

/*
 *       Get state vector for target
 */
  if ( getstate (ntarg, t1, posn) )
    return -1 ;

/*
 *       Get state vector for center
 */
  if ( ncent > 0 )
   if ( getstate (ncent, t1, posn+6) )
     return -2 ;

/*
 *   Return
 */
  return 0 ;
}

/*-----------------------------------------------------------------------
 *
 *  int getstate (int ntarg, double t1, double *posn)
 *     This function reads and interpolates the JPL ephemeris,
 *     returning position and velocity of body ntarg
 *     with respect to the solar system barycenter at Chebyshev time t1.
 *     It does some shuffling of indices: Adding earth vector
 *     to moon's state vector, converting EM state vector to earth,
 *     juggling the indices for nutation and libration, and adding
 *     barycenter, as well as scaling.
 *
 *     Arguments:
 *       Input:
 *           ntarg   Target number
 *              t1   Chebyshev time
 *       Output:
 *        posn[6]   Interpolated quantities requested:
 *                     posn[0:2] ntarg position  posn[3:5] ntarg velocity
 *                     units are lightseconds and lightseconds/s
 *
 *----------------------------------------------------------------------*/

int getstate (int ntarg, double t1, double *posn) {
  int mtarg, mcent, i ;
  double st[6], scale ;

  if ( ( ntarg <= 0 ) || ( ntarg > 15 ) ) {
    fprintf (stderr, "dpleph[getstate]: Illegal solar system body: %d\n", ntarg) ;
    return 1 ;
  }
/*
 *       Figure out correct translation for objects
 */
  mtarg = ntarg - 1 ;
  mcent = -1 ;
  scale = 1.0 ;
  switch ( ntarg ) {
  case 3:               /* Earth: from EM barycenter and Moon */
    mcent = 9 ;
    scale = -emratinv ;
    break ;
  case 10:              /* Moon: need to add EM barycenter */
    mtarg = 2 ;
    mcent = 9 ;
    scale = 1.0 - emratinv ;
    break ;
  case 13:              /* EM barycenter */
    mtarg = 2 ;
    break ;
  case 14:              /* Nutations */
  case 15:              /* Librations */
    mtarg -= 2 ;
    break ;
  case 12:              /* Solar system barycenter */
    mtarg = -1 ;
    for (i=0; i<6 ; i++)
      posn[i] = 0.0 ;
    return 0 ;
  default:
    break ;
  }

/*
 *   Interpolate mtarg
 */
  if ( ncf[mtarg] <= 0 ) {
    fprintf (stderr, "dpleph[getstate]: No data for solar system body %d\n", ntarg) ;
    return -1 ;
  }

  if ( mtarg >= 0 )
    interp (buffer+iptr[mtarg]-1, t1, ncf[mtarg], na[mtarg], posn) ;

  if ( mcent >= 0 ) {
    interp (buffer+iptr[mcent]-1, t1, ncf[mcent], na[mcent], st) ;
    for (i=0; i<6; i++)
      posn[i] += st[i] * scale ;
  }

/*
 *   Scale
 */
  if ( mtarg < 12 )
    for (i=0; i<6; i++)
      posn[i] *= aufac ;
  else
    for (i=0; i<3; i++)
      posn[i] /= SECDAY ;

/*
 *   Return
 */
  return 0 ;
}

/*-----------------------------------------------------------------------
 *
 *  int interp (double *buf, double t1, int ncf, int na, double *pv)
 *
 *     This function differentiates and interpolates a
 *     set of Chebyshev coefficients to give position and velocity
 *
 *     Arguments:
 *       Input:
 *         buf   1st location of array of Chebyshev coefficients of position
 *          t1   t1 is fractional time in interval covered by
 *               coefficients at which interpolation is wanted
 *               (0 <= t1 <= 1).  T(2) IS LENGTH OF WHOLE
 *               INTERVAL IN INPUT TIME UNITS.
 *         ncf   # of coefficients per component
 *          na   # of sets of coefficients in full array
 *               (i.e., # of sub-intervals in full interval)
 *       Output:
 *       pv[6]   interpolated quantities requested.
 *
 *----------------------------------------------------------------------*/

int interp (double *buf, double t1, int ncf, int na, double *pv)
{

  static double pc[18], vc[18] ;
  int i, j, k, l ;
  static int np = 2 ;
  static int nv = 3 ;
  static double twot = 0.0 ;
  double dna, temp, dt1, tc ;
  double *bufptr ;
  double *pvptr ;
  double vfac ;

  pc[0] = 1.0 ;
  pc[1] = 0.0 ;
  vc[1] = 1.0 ;

/*
 *       Get correct sub-interval number for this set of coefficients
 *       and then get normalized Chebyshev time within that subinterval
 */
  dna = (double) na ;
  dt1 = (int) t1 ;
  temp = dna * t1 ;
  l = (int) ((double) temp - dt1) ;

/*
 *         tc is the normalized chebyshev time (-1 <= tc <= 1)
 */
  tc = 2.0 * (temp - floor(temp) + dt1) -1.0 ;

/*
 *       Check to see whether Chebyshev time has changed,
 *       and compute new polynomial values if it has.
 *       (the element pc[1] is the value of t1(tc) and hence
 *       contains the value of tc on the previous call.)
 *       This option was removed since it caused more grief than savings.
 */
  np = 2 ;
  nv = 3 ;
  pc[1] = tc ;
  twot = tc + tc ;

/*
 *       Be sure that at least 'ncf' polynomials have been evaluated
 *       and are stored in the array 'pc'.
 */
  if ( np < ncf ) {
    for (i=np; i<ncf; i++)
      pc[i] = twot * pc[i-1] - pc[i-2] ;
    np = ncf ;
  }

/*
 *       Interpolate to get position for each component
 */
  bufptr = buf + l * ncf * 3 ;
  pvptr = pv ;
  for (i=0; i<3; i++, pvptr++) {
    *pvptr = 0.0 ;
    for (j=0; j<ncf; j++)
      *pvptr += pc[j] * *(bufptr++) ;
  }

/*
 *       If velocity interpolation is wanted, be sure enough
 *       derivative polynomials have been generated and stored.
 */
  vfac = dna * velfac ;
  vc[2] = twot + twot ;
  if ( nv < ncf ) {
    for (i=nv; i<ncf; i++)
      vc[i] = twot * vc[i-1] + 2.0 * pc[i-1] - vc[i-2] ;
    nv = ncf ;
  }

/*
 *       Interpolate to get velocity for each component
 */
  bufptr = buf + l * ncf * 3 ;
  pvptr = pv + 3 ;
  for (i=0; i<3; i++, pvptr++) {
    *pvptr = 0.0 ;
    bufptr++ ;
    for (j=1; j<ncf; j++)
      *pvptr += vc[j] * *(bufptr++) ;
    *pvptr *= vfac ;
  }

/*
 *     Return
 */
  return 0 ;
}

/*-----------------------------------------------------------------------
 *
 *  int readephem (long recnum)
 *
 *     This function opens the ephemeris file and reads record
 *     <recnum> from the third extension.
 *
 *     Arguments:
 *       Input:
 *         recnum   record number to be read
 *
 *----------------------------------------------------------------------*/

int readephem (long recnum)
{
  fitsfile *ephem_file;
  int status=0 ;
  int htype, any ;
  void *dum = NULL ;

  if ( (ephem_file = openFFile (ephfile) ) == NULL ) {
    status = 104 ;
    fprintf(stderr, "dpleph[readephem]: Cannot open file %s\n", EPHEM) ;
  }

  fits_movabs_hdu (ephem_file, 4, &htype, &status) ;
  fits_read_col (ephem_file, TDOUBLE, 1, recnum, 1, buflen, dum,
		   buffer, &any, &status) ;

  if ( !status )
    currec = recnum ;

  fits_close_file (ephem_file, &status) ;

  return status ;
}

/*-----------------------------------------------------------------------
 *
 *  int initephem (int ephnum, int *denum, double *c, double *radsol,
 *                 double *msol)
 *
 *     This function initializes parameters for using the JPL
 *     planetary ephemeris and returns some constants.
 *
 *     Arguments:
 *       Input
 *      ephnum   requested DE number of ephemeris (0: default)
 *       Output:
 *       denum   DE number of ephemeris sed
 *           c   speed of light (m/s)
 *      radsol   solar radius (light secs)
 *        msol   GM(solar), using light second as unit of length
 *
 *----------------------------------------------------------------------*/

int initephem (int ephnum, int *denum, double *c, double *radsol, double *msol)
{
  fitsfile *ephem_file;
  int status=0 ;
  char comment[80] ;
  int htype, any ;
  char *cnam[200] ;
  char cnamchar[1400] ;
  double cval[200] ;
  char extname[80] ;
  double x ;
  void *dum = NULL ;
  int i ;

/*
 *    --------------------------
 *  - Set up constant name array -
 *    --------------------------
 */
  *denum = 0 ;
  for (i=0; i<200; i++)
    cnam[i] = cnamchar + 7 * i ;
  currec = 0 ;
  if ( buffer ) free (buffer) ;

/*
 *    -------------------
 *  - Open Ephemeris File -
 *    -------------------
 */
  if ( ephnum )
    sprintf (ephfile, "%s.%d", EPHEM, ephnum ) ;
  else
    strcpy (ephfile, EPHEM) ;
  if ( (ephem_file = openFFile (ephfile) ) == NULL ) {
    status = 104 ;
    fprintf(stderr, "dpleph[initephem]: Cannot open file %s\n", ephfile) ;
    return status ;
  }

/*
 *    ---------------------
 *  - Check first extension -
 *    ---------------------
 */
  fits_movabs_hdu (ephem_file, 2, &htype, &status) ;
  fits_read_key (ephem_file, TSTRING, "EXTNAME", extname, comment, &status) ;
  if ( strcmp(extname, "DE1") ) {
    fprintf(stderr, "First extension of ephemeris file is wrong type: >%s<\n",
	    extname);
    status = -11 ;
  }
  fits_read_key (ephem_file, TLONG, "NAXIS2", &nrecs, comment, &status) ;
  if ( nrecs > 200 )
    nrecs = 200 ;
  fits_read_col_str (ephem_file, 1, 1, 1, nrecs, "",
		   cnam, &any, &status) ;
  fits_read_col (ephem_file, TDOUBLE, 2, 1, 1, nrecs, dum,
		   cval, &any, &status) ;
  *denum = (int) ( findcval (cnam, cval, nrecs, "DENUM") + 0.5 ) ;
  clight = findcval (cnam, cval, nrecs, "CLIGHT") ;
  emratinv = findcval (cnam, cval, nrecs, "EMRAT") ;
  if ( emratinv > 0.0 )
    emratinv = 1.0 / (1.0 + emratinv) ;
  au = findcval (cnam, cval, nrecs, "AU") ;
  x = au / clight ;
  *msol = findcval (cnam, cval, nrecs, "GMS") ;
  *msol = *msol * x * x * x / (86400.0 * 86400.0) ;
  *radsol = findcval (cnam, cval, nrecs, "RADS") ;
  if ( *radsol < 0.0 )
    *radsol = findcval (cnam, cval, nrecs, "ASUN") ;
  *radsol /= clight ;
  *c = clight * 1000.0 ;

/*
 *    ----------------------
 *  - Check second extension -
 *    ----------------------
 */
  fits_movabs_hdu (ephem_file, 3, &htype, &status) ;
  fits_read_key (ephem_file, TSTRING, "EXTNAME", extname, comment, &status) ;
  if ( strcmp(extname, "DE2") ) {
    fprintf(stderr, "Second extension of ephemeris file is wrong type\n");
    status = -12 ;
  }
  fits_read_key (ephem_file, TLONG, "NAXIS2", &nrecs, comment, &status) ;
  if ( nrecs > 13 )
    nrecs = 13 ;
  fits_read_col (ephem_file, TLONG, 2, 1, 1, nrecs, dum,
		   iptr, &any, &status) ;
  fits_read_col (ephem_file, TLONG, 3, 1, 1, nrecs, dum,
		   ncf, &any, &status) ;
  fits_read_col (ephem_file, TLONG, 4, 1, 1, nrecs, dum,
		   na, &any, &status) ;

/*
 *    ---------------------
 *  - Check third extension -
 *    ---------------------
 */
  fits_movabs_hdu (ephem_file, 4, &htype, &status) ;
  fits_read_key (ephem_file, TSTRING, "EXTNAME", extname, comment, &status) ;
  if ( strcmp(extname, "DE3") ) {
    fprintf(stderr, "Third extension of ephemeris file is wrong type\n");
    status = -13 ;
  }
  fits_read_key (ephem_file, TDOUBLE, "TSTART", &ss1, comment, &status) ;
  fits_read_key (ephem_file, TDOUBLE, "TSTOP", &ss2, comment, &status) ;
  fits_read_key (ephem_file, TDOUBLE, "TIMEDEL", &ss3, comment, &status) ;
  ss3inv = 1.0 / ss3 ;
  fits_read_key (ephem_file, TLONG, "NAXIS1", &irecsz, comment, &status) ;
  buflen = irecsz / 8 ;
  buffer = (double *) malloc (buflen * sizeof (double)) ;
  fits_read_key (ephem_file, TLONG, "NAXIS2", &nrecs, comment, &status) ;
  if ( nrecs != ((long) ((ss2 - ss1 + 0.5 ) * ss3inv)) ) {
    fprintf(stderr, "Ephemeris file is wrong length\n");
    status = -10 ;
  }
  aufac = 1.0 / clight ;
  velfac = 2.0 / (ss3 * 86400.0) ;

  if ( status )
    fprintf(stderr, "dpleph[initephem]: Failed to open %s properly; status: %d\n",
	    ephfile, status) ;

  fits_close_file (ephem_file, &status) ;

  if ( ephnum && ( ephnum != *denum ) ) {
    fprintf(stderr, "dpleph[initephem]: DENUM of %s (%d) disagrees with request  (%d)\n",
	    ephfile, *denum, ephnum) ;
    status = -1 ;
  }

/*
 *  if ( !status )
 *    printf ("Using JPL Planetary Ephemeris DE-%d\n", *denum) ;
 */

  return status ;
}

/*-----------------------------------------------------------------------
 *
 *  double findcval (char **cnam, double *cval, long n, char *name)
 *
 *     This function returns the value from <cval> that corresponds
 *     to the name <name> in <cnam>.
 *     <recnum> from the third extension.
 *
 *     Arguments:
 *       Input:
 *        cnam[n]   list of name strings
 *        cval[n]   list of values
 *              n   number of entries in cbam and cval
 *           name   name string to be matched
 *       Return value:
 *         value from cval that matches name in cnam
 *
 *----------------------------------------------------------------------*/

double findcval (char **cnam, double *cval, long n, char *name) {
  int i ;
  double t ;

  t = -1.0 ;
  for (i=0; i<n; i++)
    if ( !(strcmp(cnam[i], name)) ) {
      t = cval[i] ;
      break ;
    }

  return t ;
}
