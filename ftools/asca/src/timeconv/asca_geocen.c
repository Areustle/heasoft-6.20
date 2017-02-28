/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/timeconv/asca_geocen.c,v 3.8 1999/03/01 17:02:56 pwilson Exp $   */
/*                   */
/*
 *   asca_geocen.c version 1.3
 *
 *   geocentric correction for ASCA using atFuctions
 *   this subroutine is called by fortran program
 *     Yutaro Sekimoto 93/09/4
 *     mail address: ysekimoto@tkyvax.phys.s.u-tokyo.ac.jp
 *     Masaharu Hirayama 99/03/01
 *     mail address: hirayama@scipp.ucsc.edu
 *
 *   Modification:
 *
 *     M.Hirayama 94/06/20 version 1.1
 *     Change type (pointer -> value) of input arguments, i.e.,
 *        asca_time, ra_deg, and dec_deg
 *     Change order of arguments (infile <-> asca_geotime)
 *     Remove needless presession correction
 *     Change LIGHTSPEED (2.99792e+5 -> 2.99792458e+5)
 *
 *     M.Hirayama 94/09/16 version 1.2
 *     Divide into two functions asca_geocen() and asca_geocen_init()
 *
 *     P.Wilson 98/12/31 version 1.3
 *     Modify asca_geocen_asca2mjd to take leapseconds into account
 *     
 */
#include "timeconv.h"
#include "atFunctions.h"
#include "atError.h"
#include <stdio.h>
#include <string.h>
#include <math.h>
#define LIGHTSPEED 2.99792458e+05 /* in km/s */
#define ASCA2MJD 48988.0000000
#define DAYSEC  86400.0
#define FILENAMELENGTH 160

static AtVect targ_vec;  /* Vector of target from geocenter */
static char file[FILENAMELENGTH]; /* orbit file */

/* CFORTRAN macros for 2 ftoolslib subroutines */
/* First one obtains the path to the leap seconds file in refdata */
/* Second one calculates the number of leapseconds between a      */
/*    given time and the start of a reference year (1993 here).   */

PROTOCCALLSFSUB3(FGFCAL,fgfcal,PSTRING,STRING,PINT)
PROTOCCALLSFFUN4(INT,LEAPSEC,leapsec,DOUBLE,INT,STRING,PINT)
#define Fgfcal(infile,calfile,status) \
           CCALLSFSUB3(FGFCAL,fgfcal,PSTRING,STRING,PINT,infile,calfile,status)
#define Leapsec(time,refyear,leapfile,status) \
           CCALLSFFUN4(LEAPSEC,leapsec,DOUBLE,INT,STRING,PINT,time,refyear,leapfile,status)

static void
asca_geocen_asca2mjd(ascatime, mjd)
     double ascatime;
     double *mjd;        /* output: converted MJD */
     /* COMMENT by M.Hirayama                                            */
     /*  This "mjd" is used to get a satellite position.                 */
     /*  Leap seconds do not effect geocentric correction time very much */
     /*  all over ASCA's mission life. Satellite position is much more   */
     /*  ambiguous compared with effect of leap seconds                  */
     /*  1998: To improve timing of millisecond pulsars... take leap     */
     /*        seconds into account (P.Wilson-RITSS/GSFC)                */
{
  int status=0;
  static int  refyear=1993;
  static int  init_path=1;
  static char leapfile[256];

  /*  Search for leapfile path only once  */
  if( init_path ) {
     Fgfcal(leapfile,"leapsec.fits",status);
     init_path = 0;
  }
  ascatime -= Leapsec(ascatime, refyear, leapfile, status);
  *mjd = ASCA2MJD + ascatime/DAYSEC ;
}

void
asca_geocen_init(mjd, ra_deg, dec_deg, infile, epoch, elements, status)
     double mjd;
     double ra_deg;
     double dec_deg;
     char *infile;
     double *epoch;
     double *elements;
     int *status;          /* output: status */
{
  int i, kchk = 0;  /* for atSetElement2 */

/* calc target vector from ra , dec. length of vector is arb. */
  atPolDegToVect( 1.0, ra_deg, dec_deg, targ_vec);

/* set orbital element of the satellite */
  strncpy(file, infile, FILENAMELENGTH);
  *status = atSetElement2(file, mjd, kchk);
  if(*status != 0){
    printf("geocen:Error:Can't get orbital element.\n");
    return;
  }

/* return orbital elements (unit of angle is radian) */
  *epoch = atElement.mjdz;
  elements[0]  = atElement.semiax;      /* Semi-major Axis in km */
  elements[1]  = atElement.eccent;      /* Eccentricity */
  elements[2]  = atElement.aincln;      /* Inclination */
  elements[3]  = atElement.ragome;      /* Right Ascension of Ascending Node */
  elements[4]  = atElement.smaome;      /* Argument of Perigee
                                           (angle from ascending node) */
  elements[5]  = atElement.omean0;      /* Mean Anomaly at the Epoch */
  elements[6]  = atElement.adot;        /* Time Derivative of "semiax" 
                                           in km/day */
  elements[7]  = atElement.eccdot;      /* Time Derivative of "eccent"
                                           in /day (v1.6)*/
  elements[8]  = atElement.aindot;      /* Time Derivative of "aincln"
                                           in radian/day (v1.6)*/
  elements[9]  = atElement.ragdot;      /* Time Derivative of "ragome"
                                           in radian/day */
  elements[10] = atElement.smodot;      /* Time Derivative of "smaome"
                                           in radian/day */

  elements[11] = atElement.znbar;       /* Time Derivative of Mean Anomaly
                                           in radian/day, i.e. Mean Motion */
  elements[12] = atElement.znbadt;      /* 2nd Derivative of Mean Anomaly
                                           in radian/day**2 */
  elements[13] = atElement.perige;      /* Perigee */
  elements[14] = atElement.apoge;       /* Apogee */
}

void
asca_geocen(ascatime, asca_geotime)
     double ascatime;
     double *asca_geotime;  /* output: geocentric time of photon */ 
{
  double mjd;
  AtVect x;         /* Vector of satellite from geocenter */
  double rad;       /* separation angle between satellite and target */
  double geo_dtime; /* correction time in sec for geocenter */
  
/* convert ASCATIME to MJD */
  asca_geocen_asca2mjd(ascatime, &mjd);

/* get satellite position at mjd */
  atSatPos(mjd, x);

/* calc separation angle */
  atAngDistance(x ,targ_vec, &rad);

/* calc geocentric correction */
  geo_dtime =  atNorm(x)*cos(rad) / LIGHTSPEED ;
  *asca_geotime = ascatime + geo_dtime;
}

FCALLSCSUB7(asca_geocen_init,ASCA_GEOCEN_INIT,asca_geocen_init,DOUBLE,DOUBLE,DOUBLE,STRING,PDOUBLE,PDOUBLE,PINT)
FCALLSCSUB2(asca_geocen,ASCA_GEOCEN,asca_geocen,DOUBLE,PDOUBLE)
