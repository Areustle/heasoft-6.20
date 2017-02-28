/*
 * $Source: /headas/headas/attitude/tasks/coordinator/earthvel.c,v $
 * $Revision: 1.1 $
 * $Date: 2005/02/01 17:02:59 $
 *
 *
 * $Log: earthvel.c,v $
 * Revision 1.1  2005/02/01 17:02:59  rwiegand
 * Updated the earth about sun velocity model.
 *
 */


#include <math.h>

#include "earthvel.h"


#define USE_ATFUNCTIONS 1

#ifdef USE_ATFUNCTIONS
#include "atFunctions.h"
#endif

#ifdef USE_SLALIB
#include "slalib.h"
#endif



const double c_km_per_s = 299792.458;


static void
earth_vel_at_mjd_c (double mjd, double vel_c[3])
{
#if defined(USE_ATFUNCTIONS)

	const int EARTH = 2;
	const double km_per_au = 149597870;  
	const double delta_s = 60; /* estimate velocity over 2 * 30s */
	const double delta_mjd = delta_s / 86400;

	int i;
	AtVect p1[10], p2[10];
	double size[10];
	double mag[10];
	double pos_km[3];
	double vel_km_per_s[3];

	atPlanet(mjd - delta_mjd / 2, p1, size, mag);
	atPlanet(mjd + delta_mjd / 2, p2, size, mag);

	for (i = 0; i < 3; ++i)
		pos_km[i] = -p1[EARTH][i] * km_per_au;

	for (i = 0; i < 3; ++i)
		vel_km_per_s[i] = -(p2[EARTH][i] - p1[EARTH][i]) * km_per_au / delta_s;

	for (i = 0; i < 3; ++i)
		vel_c[i] = vel_km_per_s[i] / c_km_per_s;

#elif defined(USE_SLALIB)

	const double equinox = 2000;
	double evb[3], epb[3], evh[3], eph[3];

	slaEvp(mjd, equinox, evb, epb, evh, eph);

	for (i = 0; i < 3; ++i)
		vel_c[i] = evb[i] / c_km_per_s;

#else
	/* attitude/libephemeris */

	double v, vhat[3];
	v = earth_velocity(vhat, earth_longitude(mjd));

	/*
	 * Note that v*vhat is the velocity of the sun about the
	 * earth in units of c.
	 * I am suspicious of the accuracy of this model.
	 * It differs by several percent on my test cases from GTDS and AST
	*/

	for (i = 0; i < 3; ++i)
		vel_c[i] = -v * vhat[i];

#endif
}



double compat_earthvel_at_mjd (double vhat[3], double mjd)
{
	int i;
	double vc[3];
	double norm;

	earth_vel_at_mjd_c(mjd, vc);

	norm = sqrt(vc[0] * vc[0] + vc[1] * vc[1] + vc[2] * vc[2]);

	for (i = 0; i < 3; ++i)
		vhat[i] = -vc[i] / norm;

	return norm;
}

