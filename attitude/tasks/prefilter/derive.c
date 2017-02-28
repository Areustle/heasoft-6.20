/*
 * $Source: /headas/headas/attitude/tasks/prefilter/derive.c,v $
 * $Revision: 1.26 $
 * $Date: 2016/10/25 20:00:22 $
 *
 * $Log: derive.c,v $
 * Revision 1.26  2016/10/25 20:00:22  rwiegand
 * Add timeadj parameter. Swift is the instigater: timeadj is a mechanism to address its clock drift. Satellite positions in particular are sensitive to propagating the right amount of time.
 *
 * Revision 1.25  2009/10/14 20:37:55  rwiegand
 * Update to use latest atEarthElev from atFunctions library.
 *
 * Revision 1.24  2005/09/14 21:41:12  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.23  2005/01/10 21:39:00  rwiegand
 * Removed PNT_ prefix from RA, DEC, ROLL outputs; renamed POINTING to PNTUNIT
 * to avoid conflict with attitude history file.  Derive SUNSHINE flag
 * before earth angles to avoid unnecessary NULLs.
 *
 * Revision 1.22  2004/12/30 22:56:26  rwiegand
 * Implemented writing NULLs for parameters which can not be calculated
 * at particular timestamps because of missing dependencies (e.g., pointing
 * vector cannot be found without attitude).  Require an attitude record
 * within attextrap of output record time or set attitude (and attitude
 * dependent parameters) to NULL.
 *
 * Revision 1.21  2004/12/24 18:48:52  rwiegand
 * Added sun/moon/earth RA/Dec to set of available output parameters.
 *
 * Revision 1.20  2004/11/29 14:58:27  rwiegand
 * Pruned dead code.
 *
 * Revision 1.19  2004/05/27 15:27:22  rwiegand
 * Use modified version of atEarthElev for determining bright earth angle.
 *
 * Revision 1.18  2004/02/02 15:53:57  rwiegand
 * Now there is a single vector defining the current pointing derived from
 * the alignment file parameter.
 * There used to be fields defined in terms of the primary spacecraft axis
 * and others in terms of the instrument boresight.
 *
 * Revision 1.17  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 * Revision 1.16  2002/11/26 20:13:19  rwiegand
 * Corrected problem with duplicate final record in output.  Made TLE input
 * more robust.
 *
 * Revision 1.15  2002/05/14 17:42:49  rwiegand
 * Fixed bug related to splitting space delimited columns parameter.
 *
 * Revision 1.14  2002/05/14 14:51:57  rwiegand
 * Reworked parameter interface with Ed Pier's suggestions.
 *
 * Revision 1.13  2002/05/07 13:57:45  miket
 * modified to use native F77 versions of geocal/geomag/starksubs via cfortran
 *
 * Revision 1.12  2002/03/29 14:36:06  rwiegand
 * Renamed flag variable to avoid conflict with f2c.h
 *
 * Revision 1.11  2002/03/25 13:46:55  rwiegand
 * Maintain boresight vector (instead of quaternion).  For XTE comparison,
 * calculate boresight separation from nominal instead of spacecraft axis.
 *
 * Revision 1.10  2002/03/21 19:34:39  rwiegand
 * Added keywords for FITS output file.  Derive offset from nominal pointing.
 *
 * Revision 1.9  2002/03/15 18:16:33  rwiegand
 * Updated FITS column comments and units.  Derive cut off rigidity using SAX's
 * code.  Derive McIlwain L using XTE's code.  Both the SAX and XTE code were
 * FORTRAN so converted using f2c.  Implemented a mode for comparing prefilter
 * results to XTE xtefilt output.
 *
 * Revision 1.1  2002/02/20 16:20:51  wiegand
 * Initial revision
 *
 * Revision 1.8  2002/02/01 23:33:20  rwiegand
 * Context now contains modified julian day of timestamp.  Polar position
 * was being derived twice (inconsistently)- eliminated the wrong one.
 *
 * Revision 1.7  2002/02/01 15:05:30  rwiegand
 * Correct term is mission time (not mission elapsed time).  Fixed calculation
 * of pointing vector.
 *
 * Revision 1.6  2002/01/31 21:57:18  rwiegand
 * Added option to use FRF orbit file and atElement for position
 *
 * Revision 1.5  2002/01/31 19:34:42  rwiegand
 * Fixed right ascension; made roll look like coord libs
 *
 * Revision 1.4  2002/01/31 13:52:15  rwiegand
 * Renamed tool to prefilter.  Added mission elapsed time parameter.  Added
 * parameter file argument.
 *
 * Revision 1.3  2002/01/30 19:48:28  rwiegand
 * Converted sun, moon, ram angles to degrees
 *
 * Revision 1.2  2002/01/28 19:28:20  rwiegand
 * Base MET on swiftime.c instead of allowing it to be set here
 *
 * Revision 1.1  2002/01/28 16:08:05  rwiegand
 * Initial revision
 */


#include "prefilter.h"
#include "derive.h"
#include "atFunctions.h"
#include "report.h"
#include "convert.h"
#include "align.h"


static const double km_per_au = 149597870.691;



int derive_mission_time (const Context * context, Derived * derived)
{
	derived->time_raw = context->time_raw;
	derived->time_adj = context->time_adj;

	if (context->args->verbose)
		report_verbose("time.raw %.3f .adj %.3f\n",
				derived->time_raw, derived->time_adj);

	return derived->code;
}


int derive_context (const Context * context, Derived * derived)
{
	AtVect_to_AtVect(context->position, derived->position);
	AtVect_to_AtVect(context->velocity, derived->velocity);
	AtQuat_to_AtQuat(context->quaternion, derived->quaternion);

	return derived->code;
}


int derive_polar_position (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	AtVect position;
	AtPolarVect * polar = &derived->polarPosition;

	derived->altitude = constants->nulle;
	derived->latitude = constants->nulle;
	derived->longitude = constants->nulle;

	polar->r = constants->nulle;
	polar->lat = constants->nulle;
	polar->lon = constants->nulle;

	AtVect_to_AtVect(context->position, position);

	if (atGeodetic(context->mjd, position, derived->geodetic))
		derive_error(derived, __func__, "atGeodetic");
	else if (atVectToPol(derived->geodetic, polar))
		derive_error(derived, __func__, "atVectToPol");
	else
		{
			derived->altitude = polar->r;
			derived->latitude = radians_to_degrees(constants, polar->lat);
			derived->longitude = radians_to_degrees(constants, polar->lon);
		}

	if (context->args->verbose)
		report_verbose("altitude %.3f, latitude %.3f, longitude %.3f\n",
					derived->altitude, derived->latitude,
					derived->longitude);

	return derived->code;
}


int derive_pointing_vector (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	AtRotMat rm;
	AtRotMat aib; /* body to inertial */
	AtVect axis = { 0, 0, 1 };
	AtQuat q;
	const Arguments * args = context->args;

	derived->pointing[0] = constants->nulle;
	derived->pointing[1] = constants->nulle;
	derived->pointing[2] = constants->nulle;

	if (context->haveQuaternion)
		{
			AtQuat_to_QUAT(context->quaternion, derived->coordquat);
			productOfQuats(derived->alignquat, derived->coordquat, args->align->q_inverse);

			QUAT_to_AtQuat(derived->alignquat, q);
		}

	if (!context->haveQuaternion)
		;
	else if (atQuatToRM(q, rm))
		derive_error(derived, __func__, "atQuatToRM");
	else if (atInvRotMat(rm, aib))
		derive_error(derived, __func__, "atInvRotMat");
	else if (atRotVect(aib, axis, derived->pointing))
		derive_error(derived, __func__, "atRotVect");

	if (context->args->verbose)
		{
			double * p = derived->pointing;
			report_verbose("pointing %.3f %.3f %.3f\n",
					p[0], p[1], p[2]);
		}

	return derived->code;
}


int derive_ra_dec_roll (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	double ra, dec, roll;

	derived->rightAscension = constants->nulle;
	derived->declination = constants->nulle;
	derived->roll = constants->nulle;

	if (context->haveQuaternion)
		{
			AtQuat_to_QUAT(context->quaternion, derived->coordquat);

			convertQuatToRADecRoll(context->args->align, derived->coordquat,
						&ra, &dec, &roll);

			derived->rightAscension   = ra;
			derived->declination      = dec;
			derived->roll             = roll;
		}

	if (context->args->verbose)
		report_verbose("RA %.3f, Dec %.3f, roll %.3f\n",
					derived->rightAscension,
					derived->declination,
					derived->roll);

	return derived->code;
}

int
atEarthElev2( /* atEarthElev replacement */
	AtVect vSat,	/*  (in) sidereal   */
	AtVect nvTgt,	/*  (in) sidereal   */
	AtVect vSun,	/*  (in) sidereal   */
	int *earth_occult,	/* (out) earth occultation */
	double elevation[3]	/* (out) from earth, DE, NE */
);

int derive_earth_angles (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;

	AtVect position;
	AtVect target;
	AtVect sunPosition;
	AtVect sunNormal;
	double angle[3];
	int xflag;
	double dummy;

	derived->earthLimbAngle = constants->nulle;
	derived->brightEarthAngle = constants->nulle;
	derived->sunshine = constants->nulli;
	derived->fovFlag = constants->nulli;

	AtVect_to_AtVect(context->position, position);
	atCopyVect(derived->pointing, target);

	if (atSun(context->mjd, sunPosition))
		derive_error(derived, __func__, "atSun");
	else if (atNormVect(sunPosition, sunNormal))
		derive_error(derived, __func__, "atNormVect");
	else if (atEarthOccult(position, sunNormal, sunPosition, &xflag, &dummy))
		derive_error(derived, __func__, "atEarthOccult");
	else
		{
			derived->sunshine = (xflag == 0) ? 1 : 0;

			if (!context->haveQuaternion)
				;
			else if (atEarthElev(position, target, sunPosition, &xflag, angle))
				derive_error(derived, __func__, "atEarthElev2");
			else
				{
					double brightAngle;

					derived->earthLimbAngle = radians_to_degrees(constants, angle[0]);

					brightAngle = angle[1];
					derived->brightEarthAngle = radians_to_degrees(constants, brightAngle);

					derived->fovFlag = xflag;
				}
		}

	if (context->args->verbose)
		report_verbose("earth limb %.3f, bright earth %.3f\n"
					"\tsunshine %d, fovFlag %d\n",
					derived->earthLimbAngle,
					derived->brightEarthAngle,
					derived->sunshine,
					derived->fovFlag);

	return derived->code;
}



int derive_sun_angle (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	double angle;
	AtVect sunPosition;
	AtVect satToSun;

	derived->sunAngle = constants->nulle;

	if (!context->haveQuaternion)
		;
	else if (atSun(context->mjd, sunPosition))
		derive_error(derived, __func__, "atSun");
	else if (atMulAddVect(km_per_au, sunPosition, -1, derived->position,
				satToSun))
		derive_error(derived, __func__, "atMulAddVect");
	else if (atAngDistance(satToSun, derived->pointing, &angle))
		derive_error(derived, __func__, "atAngDistance");
	else
		derived->sunAngle = radians_to_degrees(constants, angle);

	if (context->args->verbose)
		report_verbose("sun angle %.3f\n",
					derived->sunAngle);

	return derived->code;
}


int derive_moon_angle (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	double angle;
	double size, phase, distance;
	AtVect moonPosition;
	AtVect satToMoon;

	derived->moonAngle = constants->nulle;

	if (!context->haveQuaternion)
		;
	else if (atMoon(context->mjd, moonPosition, &size, &phase, &distance))
		derive_error(derived, __func__, "atMoon");
	else if (atMulAddVect(1, moonPosition, -1, derived->position, satToMoon))
		derive_error(derived, __func__, "atMulAddVect");
	else if (atAngDistance(satToMoon, derived->pointing, &angle))
		derive_error(derived, __func__, "atAngDistance");
	else
		derived->moonAngle = radians_to_degrees(constants, angle);

	if (context->args->verbose)
		report_verbose("moon angle %.3f\n",
					derived->moonAngle);

	return derived->code;
}


int derive_ram_angle (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	double angle;
	AtVect velocity;

	derived->ramAngle = constants->nulle;

	AtVect_to_AtVect(context->velocity, velocity);

	if (!context->haveVelocity)
		;
	else if (!context->haveQuaternion)
		;
	else if (atAngDistance(velocity, derived->pointing, &angle))
		derive_error(derived, __func__, "atAngDistance");
	else
		derived->ramAngle = radians_to_degrees(constants, angle);

	if (context->args->verbose)
		report_verbose("ram angle %.3f\n",
					derived->ramAngle);

	return derived->code;
}


int derive_nominal_pointing_separation (
		const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	double angle;
	AtVect nominal;
	double nomra;
	double nomdec;

	derived->pointingSeparation = constants->nulle;

	nomra = context->args->nominalRightAscension;
	nomdec = context->args->nominalDeclination;

	if (!context->haveQuaternion)
		;
	else if (atPolDegToVect(1, nomra, nomdec, nominal))
		derive_error(derived, __func__, "atPolDegToVect");
	else if (atAngDistance(nominal, derived->pointing, &angle))
		derive_error(derived, __func__, "atAngDistance");
	else
		derived->pointingSeparation = radians_to_degrees(constants, angle);

	if (context->args->verbose)
		report_verbose("nominal pointing separation %.3f\n",
					derived->pointingSeparation);

	return derived->code;
}


int asca_derive_cut_off_rigidity (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	float rigidity;

	derived->cutOffRigidityASCA = constants->nulle;

	if (atRigidity(&derived->polarPosition, &rigidity))
		derive_error(derived, __func__, "atRigidity");
	else
		derived->cutOffRigidityASCA = rigidity;

	if (context->args->verbose)
		report_verbose("ASCA cut off rigidity %.3f\n",
					derived->cutOffRigidityASCA);

	return derived->code;
}


int derive_saa (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	AtPolarVect * polar = &derived->polarPosition;
	int xflag;

	derived->inSAA = constants->nulli;
	derived->timeSinceSAA = constants->nulle;

	/* base time since entered/exited SAA on change of inSAA flag? */

	if (atBrazil(polar->lon, polar->lat, &xflag))
		derive_error(derived, __func__, "atBrazil");
	else
		{
			if ((derived->lastSAA != constants->nulli)
						&& (xflag != derived->lastSAA))
				derived->timeChangeSAA = context->time_adj;

			derived->inSAA = xflag;

			if (derived->timeChangeSAA != constants->nulle)
				derived->timeSinceSAA = context->time_adj - derived->timeChangeSAA;
		}

	derived->lastSAA = derived->inSAA;

	if (context->args->verbose)
		report_verbose("inSAA %d, lastSAA %d, timeSinceSAA %.3f, timeChangeSAA %.3f\n",
					(int) derived->inSAA, (int) derived->lastSAA,
					derived->timeSinceSAA, derived->timeChangeSAA);

	return derived->code;
}


int derive_earth_radec (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	AtVect satToEarth;
	double distance, ra, dec;

	derived->earthRA = constants->nulle;
	derived->earthDec = constants->nulle;

	if (atInvVect(derived->position, satToEarth))
		derive_error(derived, __func__, "atInvVect");
	else if (atVectToPolDeg(satToEarth, &distance, &ra, &dec))
		derive_error(derived, __func__, "atVectToPolDeg");
	else
		{
			derived->earthRA = ra;
			derived->earthDec = dec;
		}

	if (context->args->verbose)
		report_verbose("earth ra=%.3f, dec=%.3f\n",
					derived->earthRA, derived->earthDec);

	return derived->code;
}


int derive_sun_radec (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	double distance, ra, dec;
	AtVect sunPosition;
	AtVect satToSun;

	derived->sunRA = constants->nulle;
	derived->sunDec = constants->nulle;

	if (atSun(context->mjd, sunPosition))
		derive_error(derived, __func__, "atSun");
	else if (atMulAddVect(km_per_au, sunPosition, -1, derived->position,
				satToSun))
		derive_error(derived, __func__, "atMulAddVect");
	else if (atVectToPolDeg(satToSun, &distance, &ra, &dec))
		derive_error(derived, __func__, "atVectToPolDeg");
	else
		{
			derived->sunRA = ra;
			derived->sunDec = dec;
		}

	if (context->args->verbose)
		report_verbose("sun ra=%.3f, dec=%.3f\n",
					derived->sunRA, derived->sunDec);

	return derived->code;
}


int derive_moon_radec (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	double size, phase, distance;
	double ra, dec;
	AtVect moonPosition;
	AtVect satToMoon;

	derived->moonRA = constants->nulle;
	derived->moonDec = constants->nulle;

	if (atMoon(context->mjd, moonPosition, &size, &phase, &distance))
		derive_error(derived, __func__, "atMoon");
	else if (atMulAddVect(1, moonPosition, -1, derived->position, satToMoon))
		derive_error(derived, __func__, "atMulAddVect");
	else if (atVectToPolDeg(satToMoon, &distance, &ra, &dec))
		derive_error(derived, __func__, "atVectToPolDeg");
	else
		{
			derived->moonRA = ra;
			derived->moonDec = dec;
		}

	if (context->args->verbose)
		report_verbose("moon ra=%.3f, dec=%.3f\n",
					derived->moonRA, derived->moonDec);

	return derived->code;
}


