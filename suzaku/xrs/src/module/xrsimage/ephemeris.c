/***************************************************************************
* This is a set of routines for calculating the positions, etc. of 
* celestial bodies. These can be replaced with something more accurate
* if desired at a later date.
****************************************************************************/
#include <stdlib.h>
#include <math.h>

/*
#define DEBUG
*/

#ifdef ATFUNCVALUES
#define BETA_EARTH 9.936508e-5 /* mean velocity of earth in units of c */

#define SIN_ECLIPTIC 0.397796946 /* sin and cosine of the inclination */
#define COS_ECLIPTIC 0.917473481 /* of the ecliptic                   */
#endif /* ATFUNCVALUES */


#define BETA_EARTH (20.496/3600.0*M_PI/180.) /* mean v of earth in units of c */

#define SIN_ECLIPTIC 0.3979486 /* sin and cosine of the inclination */
#define COS_ECLIPTIC 0.9174076 /* of the ecliptic                   */



/***********************************************************************
************************************************************************
* Give the position of the sun in radians along the ecliptic.
************************************************************************/
double earth_longitude(double mjd) {

double mjd0=51545.5; 
double twopi=2.*M_PI;

double g,l;
double lon;
double deltamjd;
            
deltamjd=mjd-mjd0;

g=0.109662+fmod(6.24004 + 0.01720197*deltamjd, twopi);
l=twopi   +fmod(4.89495 + 0.01720279*deltamjd, twopi);
      
lon=l+0.03342*sin(g)+0.0003491*sin(2.0*g);

#ifdef DEBUG
printf("earth_longitude: lon=%g\n",lon);
#endif

return(lon);

} /* end of sun_longitude function */



/***********************************************************************
************************************************************************
* Give the velocity of the earth in cartesian coordinates.
* vhat is a unit vector giving the direction of motion and the return value
* is the magnitide in units of the speed of light.
* This routine is useful for calculating abberation.
************************************************************************/
double earth_velocity(double vhat[3], double earth_lon) {

double sinlon, coslon;

/*************************************************
* get the position of the Sun along the ecliptic *
*************************************************/
sinlon=sin(earth_lon);
coslon=cos(earth_lon);

/*******************************
* direction of motion of earth *
*******************************/
vhat[0]=-sinlon;
vhat[1]= coslon*COS_ECLIPTIC;
vhat[2]= coslon*SIN_ECLIPTIC;

return(BETA_EARTH);

} /* end of earth_velocity function */



/***********************************************************************
************************************************************************
* give the position of the sun in the sky as a cartesian unit vector
* in J2000 coordinates. The Z axis points toward the north pole and the
* X axis points toward the vernal equinox.
************************************************************************/
void sun_position(double sun_dir[3], double earth_lon) {

double sinlon,coslon;

coslon=cos(earth_lon);
sinlon=sin(earth_lon);

sun_dir[0]=coslon;
sun_dir[1]=COS_ECLIPTIC*sinlon;
sun_dir[2]=SIN_ECLIPTIC*sinlon;

} /* end of sun_position function */



