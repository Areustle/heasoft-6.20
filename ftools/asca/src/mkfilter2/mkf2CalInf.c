/*
 * mkf2CalInf Ver.1.0                       Mar.17, 1994  T.Takeshima
 *
 * function
 * To get attitude and to calculate elevation, COR, and SAA for given time
 *
 * Ver.1.1 by Jeff Guerber, RSTX/GSFC, 1998-08-26.  Check Euler angles for
 *         undefined flag (-99.999), if so set elv, fov, dye to undefined
 *         (rest don't depend on the Eulers).
 * Ver.1.2 by Jeff Guerber, RSTX/GSFC, 1998-09-01.  atEarthOccult target arg
 *         must be normalized, so normalize Sun_pos (in AU) in sunshine calc.
 *         (Tag_pos is normalized by its definition.)
 *
 */
#include	<stdio.h>
#include	<string.h>

#include        "mkfilter.h"
#include        "mkf2_recd.h"
#include        "faio.h"
#include        "ascatime.h"

#define DEBUG1

int mkf2CalInf ( double         ascatime,
                 int            iatt,
                 AtEulerAng     *euler,
                 double         *elv,
                 double         *cor,
                 int            *saa,
	         unsigned char  *fov,
                 double         *dye,
                 AtPolarVect    *satvect,
                 unsigned char  *sunshine ){

    double        delta, mjd, dyearth;
    AtVect        Sat_pos, Sat_posg, Sun_pos, Tag_pos, Sun_norm;
                             /* typedef double AtVect[3]; */
    AtPolarVect   Sat_pol;   /* typedef struct
                                { double r;    radial component
                                  double lon;  longitude or R.A. (radian)
                                  double lat;} latitude or declination (radian)
	                         AtPolarVect; */
    float         rig;
    int           flag, iret;
    AtTime        ti;
    double        angdist;

    iret = 0;

    asca2attime( ascatime, &ti );
    atMJulian( &ti, &mjd );
/*
 *  get values
 */
    atSatPos( mjd, Sat_pos );                    /* satellite position xyz */
    atGeodetic( mjd, Sat_pos, Sat_posg );
    atSun( mjd, Sun_pos );                       /* get Sun position */
    atVectToPol( Sat_posg, &Sat_pol );           /* satellite position xyz
                                                    --> polar coordinate */
    atBrazil( Sat_pol.lon, Sat_pol.lat, saa );   /* check SAA */
    atRigidity( &Sat_pol, &rig );                /* get rigidity value */
    *cor = (double)rig;
    satvect->r   = Sat_pol.r;
    satvect->lon = Sat_pol.lon * RAD2DEG;
    satvect->lat = Sat_pol.lat * RAD2DEG;

#ifdef DEBUG
    fprintf ( stderr, "Sat_pol ( %6.3f, %6.3f, %6.3f ), ",
                   Sat_pol.r, Sat_pol.lon, Sat_pol.lat );
    fprintf ( stderr, "satvect ( %6.3f, %6.3f, %6.3f )\n",
                   satvect->r, satvect->lon, satvect->lat );
#endif

    if ( euler->phi > -99. ) {              /* undefined is -99.999 */

        atPolDegToVect( 1.0, euler->phi, euler->theta, Tag_pos );
        /* Satellite Z-axis --> Target direction (normalized) */
        iret = atEarthOccult( Sat_pos, Tag_pos, Sun_pos, &flag, elv );
        /* get source earth occultation and elevation */
        *elv *= RAD2DEG;
        if      ( flag == 0 ) *fov = 0;
        else if ( flag == 1 ) *fov = 1;
        else if ( flag == 2 ) *fov = 2;
        else                  *fov = 8;

        *dye = dp10DyeElv( Sat_pos, Sun_pos ,Tag_pos ) * RAD2DEG;
    }
    else {
          *elv = -99.999;
          *fov = 99;
          *dye = -99.999;
    }

#ifdef DEBUG
    fprintf ( stderr, "Sat_pos ( %7.3lf, %7.3lf, %7.3lf )\n", Sat_pos[0], Sat_pos[1], Sat_pos[2] );
    fprintf ( stderr, "Sun_pos ( %7.3lf, %7.3lf, %7.3lf )\n", Sun_pos[0], Sun_pos[1], Sun_pos[2] );
    fprintf ( stderr, "Sat_pos ( %7.3lf, %7.3lf, %7.3lf )\n", Tag_pos[0], Tag_pos[1], Tag_pos[2] );
    fprintf ( stderr, "dye = %lf\n", *dye );
#endif

    iret = atNormVect( Sun_pos, Sun_norm );
    iret = atEarthOccult( Sat_pos, Sun_norm, Sun_pos, &flag, &angdist );
                        /* get source earth occultation and elevation */
    if ( flag == 0 ) *sunshine = 1;
    else             *sunshine = 0;

#ifdef DEBUG
    fprintf( stderr, "%9.3lf ",  Sat_pol.lon*RAD2DEG );
    fprintf( stderr, "%9.3lf: ", Sat_pol.lat*RAD2DEG );
    fprintf( stderr, "SAA=%d, elv=%6.2lf, ", *saa, *elv );
    fprintf( stderr, "rig=%6.2lf, fov=%d, dye=%6.2lf, iret=%d\n",
                      *cor, *fov, *dye, iret );
    fprintf( stderr, "Euler : %9.3lf %9.3lf %9.3lf: ",
             euler->phi, euler->theta, euler->psi );
    fprintf( stderr,
             "SAA=%d, elv=%6.2lf, rig=%6.2lf, fov=%d, dye=%6.2lf, iret=%d\n",
             *saa, *elv, *cor, *fov, *dye, iret );
#endif

    return(iret);
}
