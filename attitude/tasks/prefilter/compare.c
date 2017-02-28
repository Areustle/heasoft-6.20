/*
 * $Source: /headas/headas/attitude/tasks/prefilter/compare.c,v $
 * $Revision: 1.4 $
 * $Date: 2016/10/25 20:00:22 $
 *
 * $Log: compare.c,v $
 * Revision 1.4  2016/10/25 20:00:22  rwiegand
 * Add timeadj parameter. Swift is the instigater: timeadj is a mechanism to address its clock drift. Satellite positions in particular are sensitive to propagating the right amount of time.
 *
 * Revision 1.3  2005/09/14 21:41:12  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.2  2004/12/30 22:56:26  rwiegand
 * Implemented writing NULLs for parameters which can not be calculated
 * at particular timestamps because of missing dependencies (e.g., pointing
 * vector cannot be found without attitude).  Require an attitude record
 * within attextrap of output record time or set attitude (and attitude
 * dependent parameters) to NULL.
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

#include "prefilter.h"
#include "derive.h"
#include "report.h"
#include "fitsio.h"
#include "convert.h"



int initialize_compare (Arguments * args)
{
	Compare * compare = args->compare;
	int status = 0;

	if (!args->code)
		{
			fitsfile * fptr = 0;
			long rows;

			/* open other FITS file */
			fits_open_file(&fptr, args->orbname, READONLY, &status);

			if (!status)
				fits_get_num_rows(fptr, &rows, &status);

			if (!status)
				{
					compare->fptr = fptr;
					compare->last = rows;
					compare->row = 1; /* cleverly skip row 1 (XTE kludge) */
					compare->row = 0;
				}
			else
				{
					args->code = PREFILTER_SETUP_ERROR;
					report_error("unable to initialize compare FITS file %s\n",
							args->orbname);
				}
		}

	if (!args->code)
		{
			/* initialize pcolumn array */
			compare->pcolumn[COMPARE_TIME] = &compare->time;

			compare->pcolumn[COMPARE_PX] = &compare->px;
			compare->pcolumn[COMPARE_PY] = &compare->py;
			compare->pcolumn[COMPARE_PZ] = &compare->pz;

			compare->pcolumn[COMPARE_VX] = &compare->vx;
			compare->pcolumn[COMPARE_VY] = &compare->vy;
			compare->pcolumn[COMPARE_VZ] = &compare->vz;

			compare->pcolumn[COMPARE_Q1] = &compare->q1;
			compare->pcolumn[COMPARE_Q2] = &compare->q2;
			compare->pcolumn[COMPARE_Q3] = &compare->q3;
			compare->pcolumn[COMPARE_Q4] = &compare->q4;
		}

	if (!args->code)
		{
			int status = 0;
			int i;

			/* locate columns */

			for (i = 0; i < COMPARE_COLUMNS; ++i)
				{
					char buffer[FLEN_KEYWORD];
					CompareColumn * p = compare->pcolumn[i];

					strcpy(buffer, p->name);

					fits_get_colnum(compare->fptr, CASEINSEN, buffer,
							&p->index, &status);

					if (status)
						{
							args->code = PREFILTER_SETUP_ERROR;
							report_error("unable to get column number of '%s'\n", p->name);
						}

#ifdef OTHER_CHECK_COLUMN_TYPE
					{
						int type;
						fits_get_coltype(compare->fptr, p->index,
								&type, 0, 0, &status);

						if (status || (type != p->fitsType))
							{
								args->code = PREFILTER_SETUP_ERROR;
								report_error("invalid column type [%d != %d] for '%s'\n",
										type, p->fitsType, p->name);
							}
					}
#endif

				}
		}

	return args->code;
}


int iterate_context_compare (const Arguments * args, Context * context)
{
	Compare * compare = args->compare;

	/* take timestamp, position, quaternion from FITS file */
	if (!context->code)
		{
			if (compare->row < compare->last)
				++compare->row;
			else
				context->code = PREFILTER_DONE;
		}

	if (!context->code)
		{
			int status = 0;
			long i;
			int nulls = 0;

			for (i = 0; i < COMPARE_COLUMNS; ++i)
				{
					CompareColumn * column = compare->pcolumn[i];

					fits_read_col_dbl(compare->fptr, column->index,
								compare->row, 1, 1, args->constants->nulle,
								&column->value, &nulls, &status);
				}

			if (status)
				{
					context->code = PREFILTER_ERROR;
					report_error("error reading compare data at row %d\n",
							compare->row);
				}
		}

	/* update Context state using values */
	if (!context->code)
		{
			AtVect pin;
			AtVect vin;
			AtQuat qin;
			int i;

			/* snag values */
			context->time_raw = compare->time.value;
			for (i = 0; i < 3; ++i)
				pin[i] = compare->pcolumn[COMPARE_PX + i]->value;
			for (i = 0; i < 3; ++i)
				vin[i] = compare->pcolumn[COMPARE_VX + i]->value;
			for (i = 0; i < 3; ++i)
				qin[i] = compare->pcolumn[COMPARE_Q1 + i]->value;

			/* convert time */
			met_to_AtTime(args, context->time_raw, &context->timestamp);
			atCTime2(&context->timestamp, context->timestampString);
			atMJulian(&context->timestamp, &context->mjd);

			if (args->compareApplyQuaternion)
				{
					AtRotMat rm;
					AtRotMat aib; /* body to inertial */

					AtQuat_to_AtQuat(qin, context->quaternion);

					if (atQuatToRM(qin, rm))
						context_error(context, __func__, "atQuatToRM");
					else if (atInvRotMat(rm, aib))
						context_error(context, __func__, "atInvRotMat");
					else if (atRotVect(aib, pin, context->position))
						context_error(context, __func__, "atRotVect/position");
					else if (atRotVect(aib, vin, context->velocity))
						context_error(context, __func__, "atRotVect/velocity");
				}
			else
				{
					AtVect_to_AtVect(pin, context->position);
					AtVect_to_AtVect(vin, context->velocity);
				}

			context->havePosition = 1;
			context->haveVelocity = 1;
			context->haveQuaternion = 1;
		}

	return context->code;
}

