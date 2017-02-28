
/*
 * $Source: /headas/headas/attitude/tasks/prefilter/convert.c,v $
 * $Revision: 1.5 $
 * $Date: 2016/10/25 20:00:22 $
 *
 * $Log: convert.c,v $
 * Revision 1.5  2016/10/25 20:00:22  rwiegand
 * Add timeadj parameter. Swift is the instigater: timeadj is a mechanism to address its clock drift. Satellite positions in particular are sensitive to propagating the right amount of time.
 *
 * Revision 1.4  2005/09/14 21:41:12  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.3  2004/02/02 15:53:57  rwiegand
 * Now there is a single vector defining the current pointing derived from
 * the alignment file parameter.
 * There used to be fields defined in terms of the primary spacecraft axis
 * and others in terms of the instrument boresight.
 *
 * Revision 1.2  2003/01/22 17:32:43  rwiegand
 * Added parameter for writing HISTORY keywords to output.  Fixed conversion
 * of string to AtTime.
 *
 * Revision 1.1  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 */

#include <string.h>
#include <ctype.h>

#include "derive.h"
#include "convert.h"
#include "report.h"


double radians_to_degrees (const Constants * constants, double radians)
{
	double degrees = radians * 180 / constants->pi;
	return degrees;
}


double degrees_to_radians (const Constants * constants, double degrees)
{
	double radians = degrees * constants->pi / 180;
	return radians;
}


int vector_to_AtVect (const vector_t * vector, AtVect atvect)
{
	int code = 0;

	vector_t tmp = *vector;
	vector_denormalize(&tmp);

	atvect[0] = tmp.x;
	atvect[1] = tmp.y;
	atvect[2] = tmp.z;

	return code;
}


int AtVect_to_AtVect (const AtVect in, AtVect out)
{
	int code = 0;

	out[0] = in[0];
	out[1] = in[1];
	out[2] = in[2];

	return code;
}


int QUAT_to_AtQuat (const QUAT * coordquat, AtQuat atquat)
{
	int code = 0;

	/* see coord/quat.h for QUAT declaration */
	/* see atFunctions/atFunctions.h for AtQuat declaration */

	atquat[0] = coordquat->p[0];
	atquat[1] = coordquat->p[1];
	atquat[2] = coordquat->p[2];
	atquat[3] = coordquat->p[3];

	return code;
}


int AtQuat_to_QUAT (const AtQuat atquat, QUAT * coordquat)
{
	int code = 0;

	/* see coord/quat.h for QUAT declaration */
	/* see atFunctions/atFunctions.h for AtQuat declaration */

	coordquat->p[0] = atquat[0];
	coordquat->p[1] = atquat[1];
	coordquat->p[2] = atquat[2];
	coordquat->p[3] = atquat[3];

	return code;
}


int AtQuat_to_AtQuat (const AtQuat in, AtQuat out)
{
	int code = 0;

	out[0] = in[0];
	out[1] = in[1];
	out[2] = in[2];
	out[3] = in[3];

	return code;
}


double AtTime_to_yyyyddd_fraction (const AtTime * t)
{
	int doy;
	double out = 0;
	if (!year_month_day_to_doy(t->yr, t->mo, t->dy, &doy))
		{
			double sod = seconds_from_hours_minutes_seconds(
					t->hr, t->mn, t->sc + t->ms / 1000);
			out = 1000 * t->yr + doy + sod / 86400;
		}
	else
		report_error("invalid year/month/day [%d/%d/%d]\n",
					t->yr, t->mo, t->dy);

	return out;
}



int AtTime_compare (const AtTime * t1, const AtTime * t2)
{
	int compare = 0;

	long yyyymmdd1 = (t1->yr * 100 + t1->mo) * 100 + t1->dy;
	long yyyymmdd2 = (t2->yr * 100 + t2->mo) * 100 + t2->dy;

	double hhmmssmmm1 = ((t1->hr * 100 + t1->mn) * 100 + t1->sc) * 1000
			+ (double) t1->ms;
	double hhmmssmmm2 = ((t2->hr * 100 + t2->mn) * 100 + t2->sc) * 1000
			+ (double) t2->ms;

	if (yyyymmdd1 > yyyymmdd2)
		compare = 1;
	else if (yyyymmdd1 < yyyymmdd2)
		compare = -1;
	else if (hhmmssmmm1 > hhmmssmmm2)
		compare = 1;
	else if (hhmmssmmm1 < hhmmssmmm2)
		compare = -1;

	return compare;
}


/* convert from yyyy-mm-ddThh:mm:ss.sss to AtTime */
int string_to_AtTime (const char * s, AtTime * t)
{
	int code = 0;

	const char * proto1 = "dddd-dd-ddTdd:dd:dd.ddd";
	const char * proto2 = "dddd-dd-ddTdd:dd:dd";
	const char * proto = "";

	int i, length;

	length = strlen(s);
	if (length == strlen(proto1))
		proto = proto1;
	else if (length == strlen(proto2))
		proto = proto2;
	else
		{
			code = 1;
			report_warning("invalid timestamp %s, expecting %s or %s\n",
					s, proto1, proto2);
		}

	for (i = 0; !code && i < length; ++i)
		{
			char f = proto[i];

			if (f == 'd')
				{
					if (!isdigit(s[i]))
						{
							code = 2;
							report_warning("expecting digit at offset %d [got %c]\n",
									i, s[i]);
						}
				}
			else if (f != s[i])
				{
					code = 2;
					report_warning("expecting %c at offset %d [got %c]\n",
							f, i, s[i]);
				}
		}

	if (!code)
		{
				t->yr = atoi(s + 0);
				t->mo = atoi(s + 5);
				t->dy = atoi(s + 8);
				t->hr = atoi(s + 11);
				t->mn = atoi(s + 14);
				t->sc = atoi(s + 17);
				t->ms = atoi(s + 20) / 1000.;
		}

	return code;
}


double met_to_mjd_utc(const Arguments *args, double met)
{
	double mjd = args->mjd0_utc + (met + args->context->time_offset) / 86400.;
	return mjd;
}


int met_to_AtTime(const Arguments *args, double met, AtTime *t)
{
	double mjd = met_to_mjd_utc(args, met);
	atMJDate(mjd, t);
	return 0;
}


double met_to_yyyyddd_fraction(const Arguments *args, double met)
{
	AtTime t;
	met_to_AtTime(args, met, &t);
	return AtTime_to_yyyyddd_fraction(&t);
}
