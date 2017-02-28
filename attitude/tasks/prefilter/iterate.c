/*
 * $Source: /headas/headas/attitude/tasks/prefilter/iterate.c,v $
 * $Revision: 1.9 $
 * $Date: 2016/10/25 20:00:22 $
 *
 * $Log: iterate.c,v $
 * Revision 1.9  2016/10/25 20:00:22  rwiegand
 * Add timeadj parameter. Swift is the instigater: timeadj is a mechanism to address its clock drift. Satellite positions in particular are sensitive to propagating the right amount of time.
 *
 * Revision 1.8  2005/09/14 21:41:12  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.7  2004/12/30 22:56:26  rwiegand
 * Implemented writing NULLs for parameters which can not be calculated
 * at particular timestamps because of missing dependencies (e.g., pointing
 * vector cannot be found without attitude).  Require an attitude record
 * within attextrap of output record time or set attitude (and attitude
 * dependent parameters) to NULL.
 *
 * Revision 1.6  2004/02/02 15:53:57  rwiegand
 * Now there is a single vector defining the current pointing derived from
 * the alignment file parameter.
 * There used to be fields defined in terms of the primary spacecraft axis
 * and others in terms of the instrument boresight.
 *
 * Revision 1.5  2003/06/23 22:57:37  rwiegand
 * Added iteration code that indicates that current iteration should be skipped.
 *
 * Revision 1.4  2003/06/23 22:23:03  rwiegand
 * Orbit and attitude out of bounds errors were not being corrected logged.
 *
 * Revision 1.3  2003/01/09 22:10:05  rwiegand
 * Corrected a formatting string.
 *
 * Revision 1.2  2003/01/09 21:25:40  rwiegand
 * Put errors on stderr instead of stdout.  Fixed conversion of 2 digit to 4
 * digit years (thanks Ed).  More informative error message when satellite
 * is below minimum altitude.
 *
 * Revision 1.1  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 */

#include <stdlib.h>

#include "prefilter.h"
#include "derive.h"
#include "iterate.h"
#include "misstime.h"
#include "report.h"
#include "convert.h"



int iterate_timestamp_delta (const Arguments * args, Context * context)
{
	if (context->first)
		{
			context->first = 0;
			context->time_raw = args->start;
		}
	else
			context->time_raw += args->interval;

	context->time_adj = context->time_raw + context->time_offset;

	context->mjd = met_to_mjd_utc(args, context->time_raw);

	atMJDate(context->mjd, &context->timestamp);
	atCTime2(&context->timestamp, context->timestampString);

	if (!context->code)
		if (context->time_raw > args->end)
			context->code = PREFILTER_DONE;

	return context->code;
}


int iterate_timestamp_report (const Arguments * args, Context * context)
{
	report_verbose("time %f corr %f [%s] [code %d]\n",
			context->time_raw, context->time_adj, context->timestampString, context->code);

	return context->code;
}


/* propagate TLE to timestamp */
int iterate_position_tle (const Arguments * args, Context * context)
{
	/* generate error for deep space TLE */
	int flags = TLE_SHALLOW;
	int status;

	vector_t position;
	vector_t velocity;
	datetime_t timestamp;

	datetime_set_mjd(&timestamp, context->mjd);

	status = tle_to_position(&context->tle, &timestamp,
			&position, &velocity, flags);

	if (status)
		{
			context->code = PREFILTER_ERROR;
			report_error("unable to determine position from TLE\n");
			iterate_position_nullify(args, context);
		}
	else
		{
			vector_to_AtVect(&position, context->position);
			vector_to_AtVect(&velocity, context->velocity);
			context->havePosition = 1;
			context->haveVelocity = 1;
		}

	return context->code;
}


int iterate_position_atSatPos (const Arguments * args, Context * context)
{
	if (atSatPos(context->mjd, context->position))
		iterate_error(args, context, __func__, "atSatPos");
	else
		context->havePosition = 1;

	return context->code;
}


int iterate_position_nullify (const Arguments * args, Context * context)
{
	const Constants * constants = args->constants;

	double * p = context->position;
	double * v = context->velocity;

	context->havePosition = 0;
	context->haveVelocity = 0;

	p[0] = constants->nulle;
	p[1] = constants->nulle;
	p[2] = constants->nulle;

	v[0] = constants->nulle;
	v[1] = constants->nulle;
	v[2] = constants->nulle;

	return context->code;
}


int iterate_position_validate (const Arguments * args, Context * context)
{
	/*
	 * minimal altitude validation
	 * was added because the FORTRAN SHELLC/SHELLG functions
	 * could enter an infinite loop when given a bad altitude
	 */

	double d = atNorm(context->position);
	double minAltitude = 100;
	if (d < args->constants->earthRadius + minAltitude)
		{
			context->code = PREFILTER_ERROR;
			report_error("satellite has re-entered at %s [altitude < %f km]\n",
				context->timestampString, minAltitude);
		}

	return context->code;
}


int iterate_position_report (const Arguments * args, Context * context)
{
	double * p = context->position;
	double * v = context->velocity;

	report_verbose("position %.3f %.3f %.3f\n",
			p[0], p[1], p[2]);
	report_verbose("velocity %f %f %f\n",
			v[0], v[1], v[2]);

	return context->code;
}


int iterate_attitude_attfile (const Arguments * args, Context * context)
{
	if (!context->code)
		{
			if (!isInExtrapolatedAttFile(args->attfile, context->time_raw))
				{
					context->code = PREFILTER_SKIP;
					report_verbose("mission time %.3f is not available in the attitude file\n",
							context->time_raw);
				}
		}

	if (!context->code)
		{
			if (findQuatInAttFile(args->attfile,
							context->coordquat, context->time_raw))
				{
					QUAT_to_AtQuat(context->coordquat, context->quaternion);
					context->haveQuaternion = 1;
				}
			else
				iterate_attitude_nullify(args, context);
		}

	return context->code;
}


int iterate_attitude_nullify (const Arguments * args, Context * context)
{
	const Constants * constants = args->constants;

	double * q = context->quaternion;

	context->haveQuaternion = 0;

	q[0] = constants->nulle;
	q[1] = constants->nulle;
	q[2] = constants->nulle;
	q[3] = constants->nulle;

	return context->code;
}


int iterate_attitude_report (const Arguments * args, Context * context)
{
	double * q = context->quaternion;

	report_verbose("quaternion %.3f %.3f %.3f %.3f\n",
			q[0], q[1], q[2], q[3]);

	return context->code;
}

