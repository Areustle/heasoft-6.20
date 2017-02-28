/*
 * $Source: /headas/headas/attitude/tasks/aberrator/aberrator.c,v $
 * $Revision: 1.3 $
 * $Date: 2012/08/22 19:21:28 $
 *
 *
 * $Log: aberrator.c,v $
 * Revision 1.3  2012/08/22 19:21:28  craigm
 * Update to task version 1.1.  Add 'orbext' parameter to support NuSTAR orbit extension named 'ORBIT'.  Add 'attcol' parameter to support either POINTING or QPARAM as input (previously was only POINTING).  Unit test still passes. --CM
 *
 * Revision 1.2  2005/02/01 00:07:18  rwiegand
 * Use atFunctions instead of attitude/libephemeris for earth velocity.
 *
 * Revision 1.1  2005/01/29 23:55:11  rwiegand
 * Tool to adjust an attitude file for velocity aberration.
 *
 */


#include <math.h>

#include "report.h"
#include "orbfile.h"
#include "align.h"
#include "aberrator.h"


#define USE_ATFUNCTIONS 1

#ifdef USE_ATFUNCTIONS
#include "atFunctions.h"
#endif

#ifdef USE_SLALIB
#include "slalib.h"
#endif



const double c_km_per_s = 299792.458;


static double
degrees_to_radians (double degrees)
{
	double radians = M_PI * degrees / 180;
	return radians;
}


static double
radians_to_degrees (double radians)
{
	double degrees = 180 * radians / M_PI;
	return degrees;
}


static int
vector_to_pointing_deg (double v[3], double * ra_d, double * dec_d)
{
	double rho2, norm, rho;
	double ra_r, dec_r;

#define EPSILON 1e-12

	rho2 = v[0] * v[0] + v[1] * v[1];
	norm = sqrt(rho2 + v[2] * v[2]);

	if (norm == 0)
		{
			*ra_d = *dec_d = 0;
			report_error("cannot convert null vector to pointing\n");
			return 1;
		}

	rho = sqrt(rho2);
	dec_r = asin(v[2] / norm);

	if (rho < EPSILON)
		ra_r = 0;
	else
		{
			double c, s;

			c = v[0] / rho;
			s = v[1] / rho;

			if (fabs(s) < EPSILON)
				ra_r = (1 - c / fabs(c)) * M_PI / 2;
			else
				ra_r = 2 * atan((1 - c) / s);
		}

	if (ra_r > M_PI * 2)
		ra_r -= M_PI * 2;

	else if (ra_r < 0)
		ra_r += M_PI * 2;

	*ra_d = radians_to_degrees(ra_r);
	*dec_d = radians_to_degrees(dec_r);

	return 0;
}


static void
convert_pointing_deg_to_unit (double ra_d, double dec_d, double unit[3])
{
	double ra_r, dec_r;

	ra_r = degrees_to_radians(ra_d);
	dec_r = degrees_to_radians(dec_d);

	unit[0] = cos(ra_r) * cos(dec_r);
	unit[1] = sin(ra_r) * cos(dec_r);
	unit[2] = sin(dec_r);
}


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



static int
get_correction_at_met (Aberrator * tool, double met, double * corr)
{
	int code = 0;
	int i;

	for (i = 0; i < 3; ++i)
		corr[i] = 0;

	if (tool->move_earth)
		{
			const double seconds_per_day = 86400;
			double mjd = tool->mjdref + met / seconds_per_day;
			double vel_c[3];

			earth_vel_at_mjd_c(mjd, vel_c);

			for (i = 0; i < 3; ++i)
				corr[i] -= vel_c[i];
		}

	if (tool->move_satellite)
		{
			if (!isInExtrapolatedOrbitFile(tool->orbfile, met))
				{
					code = 1;
					report_error("time %g not available in extrapolated orbit file\n",
									met);
				}
			else
				{
					PVrecord satellite;
					findRecordInOrbitFile(tool->orbfile, &satellite, met);
					for (i = 0; i < 3; ++i)
						corr[i] -= satellite.vel[i] / c_km_per_s;
				}
		}

	return code;
}



int
correct_attitude_record (Aberrator * tool, AttitudeRecord * record)
{
	int code = 0;
	double unit[3];
	double corr[3];
	double ra, dec, roll;

	if (!code)
		code = get_correction_at_met(tool, record->time, corr);

	if (!code)
		{
			if (! tool -> from_quat) { 

				ra = record->inertial[0];
				dec = record->inertial[1];
				roll = record->inertial[2];

			} else {
				int i;

				for (i = 0; i < 4; ++i)
				  tool->quat->p[i] = record->quaternion[i];

				convertQuatToRADecRoll(tool->align, 
						       tool->quat,
						       &ra, &dec, &roll);
			}

			convert_pointing_deg_to_unit(ra, dec, unit);
		}

	if (!code)
		{
			int i;
			double sum[3];

			for (i = 0; i < 3; ++i)
				sum[i] = unit[i] + corr[i];

			vector_to_pointing_deg(sum, &ra, &dec);


#ifdef DEBUG
printf("inertial ra=%f dec=%f => unit %f %f %f\n",
				ra, dec, unit[0], unit[1], unit[2]);
printf("corr vector [ %f %f %f ]\n",
				corr[0], corr[1], corr[2]);
printf("sum vector [ %f %f %f ] => ra=%f dec=%f\n",
				sum[0], sum[1], sum[2], ra, dec);
#endif


			convertRADecRollToQuat(tool->align, tool->quat, ra, dec, roll);

			roll = record->inertial[2];

			record->apparent[0] = ra;
			record->apparent[1] = dec;
			record->apparent[2] = roll;

			for (i = 0; i < 4; ++i)
				record->quaternion[i] = tool->quat->p[i];
		}

	return code;
}


