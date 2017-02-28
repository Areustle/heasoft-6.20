/***************************************************************************
* This is a set of routines for calculating the positions, etc. of 
* celestial bodies. These can be replaced with something more accurate
* if desired at a later date.
****************************************************************************/

/***********************************************************************
************************************************************************
* Give the position of the sun in radians along the ecliptic.
************************************************************************/
double earth_longitude(double mjd);

/***********************************************************************
************************************************************************
* Give the velocity of the earth in cartesian coordinates.
* vhat is a unit vector giving the direction of motion and the return value
* is the magnitide in units of the speed of light.
* This routine is useful for calculating abberation.
************************************************************************/
double earth_velocity(double vhat[3], double earth_lon);


/***********************************************************************
************************************************************************
* give the position of the sun in the sky as a cartesian unit vector
* in J2000 coordinates. The Z axis points toward the north pole and the
* X axis points toward the vernal equinox.
************************************************************************/
void sun_position(double sun_dir[3], double earth_lon);
