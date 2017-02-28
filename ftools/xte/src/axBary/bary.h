/* RCS: $Id: bary.h,v 1.2 2005/06/03 19:11:17 miket Exp $ */
/*-----------------------------------------------------------------------
 *
 *  bary.h
 *
 *  Date: 19 November 1997
 *  Unified FITS version
 *
 *  Arnold Rots, USRA
 *
 *  This is the header file for bary.c and related source files.
 *
 *  Do not ever use DE405 on a FITS file that has:
 *      TIMESYS='TDB'
 *    but not:
 *      RADECSYS='ICRS' or PLEPHEM='JPL-DE405'
 *  TIMESYS='TDB' _with_    RADECSYS='ICRS' and/or PLEPHEM='JPL-DE405'
 *                             should use denum=405.
 *  TIMESYS='TDB' _without_ RADECSYS='ICRS' or     PLEPHEM='JPL-DE405'
 *                             should use denum=200.
 *  
 *  Time is kept in three possible ways:
 *  The basic convention is:
 *    MJDTime t ;  The MJDTime struct is defined below;
 *                   a C++ class would be better.
 *  The JPL ephemeris functions use:
 *    double time[2] ;  Where:
 *      time[0]   Integer part of JD
 *      time[1]   Fractional part of JD; -0.5 <= time[1] < 0.5
 *  MET uses a single double:
 *    double time ;  Where:
 *      time      Seconds or days since MJDREF
 *
 * We shall use the following convention:
 *
 *   If the FITS file has   Then we shall adopt
 *   TIMESYS  TIMEUNIT      refTS  fromTS  mjdRef
 *
 *     TT        s           TT     TT     MJDREF
 *     TT        d           TT     TT     MJDREF
 *     UTC       s           UTC    TT     toTT(MJDREF)
 *     UTC       d           UTC    UTC    MJDREF
 *     TDB       s           TDB    TDB    MJDREF
 *     TDB       d           TDB    TDB    MJDREF
 *
 * System files (JPLEPH, JPLEPH.200, JPLEPH.405, psrtime.dat, psrbin.dat,
 * tai-utc.dat, tdc.dat) are first searched for in $TIMING_DIR,
 * then $LHEA_DATA ($ASC_DATA, depending on what LHEADIR is set to).
 *
 *----------------------------------------------------------------------*/

#ifndef BARY_H
#define BARY_H

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <fitsio.h>
#include <longnam.h>

#define NOEROWS	100		/* Number of Orbit Ephemeris rows in memory */
#define SECDAY	86400			/* Seconds per day */
#define RADEG   57.2957795130823
#define TWOPI   6.283185307179586
#define LB      1.550519748E-8          /* TT and TDB to TCB rate */
#define LC      1.4808268457E-8         /* TCG to TCB rate */
#define LG      6.96929019E-10          /* TT to TCG rate */
#define TAItoTT 32.184	        	/* TT - TAI (s) */
#define DOT(A, B)	(A[0]*B[0] + A[1]*B[1] + A[2]*B[2])

#define TIMINGDIR "TIMING_DIR"
#define LHEADIR   "LHEA_DATA"
/* #define LHEADIR   "ASC_DATA" */
#define EPHEM     "JPLEPH"
#define PSRTIME   "psrtime.dat"
#define PSRBIN    "psrbin.dat"
#define XTECC     "tdc.dat"
#define TAIUTC    "tai-utc.dat"


static enum TimeSys {TT, TDB, TCG, TCB, TAI, ET, UTC} fromTS, toTS ;

static enum Observatory {Unknown, Geocenter, XTE, AXAF} mission=Unknown ;

typedef struct {
  enum TimeSys ts ;
  long MJDint ;
  double MJDfr ;
} MJDTime ;

typedef struct {
  double ra ;           /* RA */
  double dec ;          /* Dec */
  int denum ;           /* JPL ephemeris number */
  MJDTime t0 ;          /* reference epoch (in MJD(TDB)(at barycenter)) */
  double f ;            /* pulsar frequency */
  double df ;           /* pulsar frequency derivative */
  double ddf ;          /* pulsar frequency second derivative */
  double dddf ;         /* pulsar frequency third derivative */
  double radioph ;      /* radio phase at reference epoch */
} PsrTimPar ;

typedef struct {
  double pb ;           /* Orbital period (s) */
  double a1sini ;       /* Projected semi-major axis (light seconds) */
  double e ;            /* orbital eccentricity */
  MJDTime t0 ;          /* Barycentric time (in MJD(TDB)) of periastron */
  double omega ;        /* Longitude of periastron (degrees) */
  double omdot ;        /* First derivative of omdot (degrees per Julian year) */
  double gamma ;        /* Time-dilation and gravitational redshift parameter (s) */
  double pbdot ;        /* First derivative of pb */
} PsrBinPar ;

/*  Externally referenced functions  */
void met2mjd (double, MJDTime *) ;
double mjd2met (MJDTime *) ;
int baryinit (enum Observatory, char *, char *, double, double, char *, int) ;
void scorbitinit (enum Observatory) ;
void clockinit (enum Observatory) ;
double barycorr (MJDTime *, double *, double *) ;
double *scorbit (char *, MJDTime *, int *) ;
double xbarycorr (double, double *, double *) ;
double *xscorbit (char *, double, int *) ;
double *xtescorbit (char *, double, int *) ;       /* XTE- and AXAF-specific */
double clockCorr (double, double, double*, char*) ;
int timeinit (enum Observatory, char *, char *, double, double, char *) ;
double toTT (MJDTime *) ;
double toUTC (MJDTime *) ;
const char *convertTime (MJDTime *) ;
const char *fitsdate () ;
int timeparms (char *, MJDTime *, double, int, PsrTimPar *, PsrBinPar *, int) ;
void c200to405 (double *, int) ;
double absphase (MJDTime *, PsrTimPar *, int, PsrBinPar *,
		 int, double *, char *, int *) ;
double xabsphase (double, PsrTimPar *, int, PsrBinPar *,
		 int, double *, char *, int *) ;
double binorbit (MJDTime *, PsrBinPar *) ;
long** phaseHist (char *, char *, char *, char *, char *, char *, char *,
		  double *, double *, double, double *, int,
		  PsrTimPar *, PsrBinPar *, int, int, double *, double *,
		  double *, double *, double **, int, int *, char *,
		  int *, int *, char *, double *, int, int *, int) ;

/*  Internally referenced functions  */
double TTtoTDB (MJDTime *) ;
double TDBtoTCB (MJDTime *) ;
double TTtoTCG (MJDTime *) ;
double UTCtoTAI (MJDTime *) ;
double ctatv (long, double) ;
int initephem (int, int *, double *, double *, double *) ;
const double *dpleph (double *, int, int) ;
FILE *openAFile (const char *) ;
fitsfile *openFFile (const char *) ;

#endif


#ifdef BARY_CONST
#ifndef BARY_CONST_DEF
#define BARY_CONST_DEF

static long mjdRefi = 49353 ;
static double mjdReff = 6.965740740000E-04 ;
static MJDTime mjdRef ;

static long numleaps = 0 ; /* Number of leap seconds, including 1 Jan 1961 */
/* fix to repeated leapsec reads from Ingo.Kreykenbohm@obs.unige.ch 19May2005 */
#define MAX_LEAPS 5000
static long leapsMJD[MAX_LEAPS] ;
static double leapsecs[MAX_LEAPS] ;
static double MJDoffset[MAX_LEAPS] ;
static double leapcoeff[MAX_LEAPS] ;
char *bhrcsid = "RCS: $Id: bary.h,v 1.2 2005/06/03 19:11:17 miket Exp $" ;

#endif
#endif
