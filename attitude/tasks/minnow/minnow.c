#include <math.h>
#include <string.h>

#include "pil.h"
#include "report.h"
#include "headas_utils.h" /* for headas_clobberfile */
#include "longnam.h"
#include "quat.h"
#include "align.h"
#include "rewmath.h"
#include "caldbquery.h"


char **string_split(const char *in, char delimiter, int *count);

#define TOOLSUB minnow
#include "headas_main.c"


typedef enum
{
	OP_AND, /* default operator */
	OP_OR,
	OP_FINAL
} Operator;


typedef struct
{
	int chatter;
	int history;

	int outfileNone;

	char infile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];

	char alignfile[PIL_LINESIZE];

	char operation[PIL_LINESIZE];

	char options[PIL_LINESIZE];

	double time_s;

	double ra_d;
	double dec_d;
	double roll_d;

} Parameters;


typedef struct
{
	const Parameters * par;

	fitsfile * fptr;

	double t1;
	double t2;

	QUAT * q1;
	QUAT * q2;

	double unominal[3];
	double usampled[3];

	double pointing[3]; /* RA, Dec, roll */

	double max_angle_d;
	double max_offset_s;
	Operator operator;
	int onlyOutliers;

	ALIGN * align;

	QUAT * qref;
	QUAT * qsample;
	QUAT * qtmp;
	ROTMATRIX * rm;

	QUAT * qout;

} Task;



static int compare_doubles(double d1, double d2)
{
	if (d1 < d2)
		return -1;
	if (d1 > d2)
		return 1;
	return 0;
}


static int write_attitude (Task * task)
{
	int status = 0;
	int i;
	fitsfile * fptr = 0;
	char *ttype[] = { "TIME", "QPARAM", "POINTING" };
	char *tform[] = { "1D",   "4D",     "3D" };
	char *tunit[] = { "s",    " ",      "deg" };
	char extname[] = "ATTITUDE";
	int nrows = 2;
	int nfields = sizeof(ttype) / sizeof(ttype[0]);
	int colnum, firstrow, firstelem, nelements;
	double data[8];
	char * attmode = "NONE";

	fits_create_file(&fptr, task->par->outfile, &status);
	if (!fptr || status)
	{
		report_error("unable to create %s [%d]\n", task->par->outfile, status);
		return status;
	}

	fits_create_tbl(fptr, BINARY_TBL, nrows, nfields, ttype, tform, tunit, extname, &status);
	if (status)
	{
		report_error("unable to create %s table [%d]\n", extname, status);
		return status;
	}

	if (task->qout == task->qref)
		attmode = "REFERENCE";
	else
		attmode = "SAMPLE";
	fits_write_key(fptr, TSTRING, "MINNOW", attmode, "Minnow attitude choice", &status);
	if (status)
	{
		report_error("unable to write MINNOW keyword [%d]\n", status);
		return status;
	}

	colnum = 1;
	firstrow = 1;
	firstelem = 1;
	nelements = 2;
	data[0] = task->par->time_s - 1;
	data[1] = task->par->time_s + 1;
	fits_write_col_dbl(fptr, colnum, firstrow, firstelem, nelements, data, &status);
	if (status)
	{
		report_error("unable to write TIME column [%d]\n", status);
		return status;
	}

	colnum = 2;
	nelements = 8;
	for (i = 0; i < 4; ++i)
		data[i] = data[i+4] = task->qout->p[i];
	fits_write_col_dbl(fptr, colnum, firstrow, firstelem, nelements, data, &status);
	if (status)
	{
		report_error("unable to write TIME column [%d]\n", status);
		return status;
	}

	colnum = 3;
	nelements = 6;
	for (i = 0; i < 3; ++i)
		data[i] = data[i+3] = task->pointing[i];
	fits_write_col_dbl(fptr, colnum, firstrow, firstelem, nelements, data, &status);
	if (status)
	{
		report_error("unable to write TIME column [%d]\n", status);
		return status;
	}

	if (task->par->history)
		HDpar_stamp(fptr, 1, &status);

	fits_close_file(fptr, &status);
	if (status)
	{
		report_error("unable to close %s [%d]\n", task->par->outfile, status);
		return status;
	}

	return status;
}


double choose_row(double value, double *array, int count)
{
	int posmin = 0;
	int posmax = count - 1;

	if (value < array[0])
		return -0.5;

	if (value > array[count-1])
		return count - 0.5;

	while (1)
	{
		int mid = (posmin + posmax) / 2;
		int test = compare_doubles(array[mid], value);

		if (test < 0)
		{
			if (mid == posmin && posmax != posmin)
			{
				posmin = posmax;
				continue;
			}
			if (mid == posmin)
				return mid + 0.5;
			posmin = mid;
		}
		else if (test > 0)
		{
			if (mid == posmax && posmax != posmin)
			{
				posmax = posmin;
				continue;
			}
			if (mid == posmax)
				return mid - 0.5;
			posmax = mid;
		}
		else
			return mid;
	}
}


static int loadAttitudeData (Task * task, double t)
{
	int status = 0;
	int nulls = 0;
	double * data = 0;
	long count = 0;
	double q[8];
	int colnum = 0;
	int selected;

	fits_get_num_rows(task->fptr, &count, &status);
	if (status) return status;

	if (count < 2)
	{
		report_warning("insufficient rows [%d]\n", count);
		return 1;
	}

	fits_get_colnum(task->fptr, CASEINSEN, "TIME", &colnum, &status);
	if (status) return status;

	data = (double *) malloc(count * sizeof(double));
	fits_read_col_dbl(task->fptr, colnum, 1, 1, count, -999, data, &nulls, &status);
	if (status) return status;

	{
		double tmp = choose_row(t, data, count);
		if (tmp < 0)
			selected = 0;
		else if (tmp > count - 1)
			selected = count - 2;
		else
			selected = (int) tmp;
	}

	fits_get_colnum(task->fptr, CASEINSEN, "QPARAM", &colnum, &status);
	if (status) return status;

	task->t1 = data[selected];
	task->t2 = data[selected+1];
	report_verbose("t=%f selected=%d t1=%f t2=%f\n",
			t, selected, task->t1, task->t2);

	fits_read_col_dbl(task->fptr, colnum, selected+1, 1, 8, -999, &q[0], &nulls, &status);
	if (status) return status;

	setQuat(task->q1, q[0], q[1], q[2], q[3]);
	setQuat(task->q2, q[4], q[5], q[6], q[7]);

	report_verbose("q1 = { %.15f, %.15f, %.15f, %.15f }\n",
			q[0], q[1], q[2], q[3]);
	report_verbose("q2 = { %.15f, %.15f, %.15f, %.15f }\n",
			q[4], q[5], q[6], q[7]);

	free(data);

	return status;
}


static int initialize (Task * task, const Parameters * par)
{
	int code = 0;
	int status = 0;

	task->par = par;

	/* attitude file */
	fits_open_table(&task->fptr, par->infile, READONLY, &status);
	if (!task->fptr || status)
	{
		code = 1; /* choose good CFITSIO code */
		report_error("unable to open %s [%d]\n", par->infile, status);
	}

	if (!code)
	{
		char path[QUERY_MAXPATH];
		if (!strncasecmp(par->alignfile, "CALDB", 5))
		{
			CALDBQuery query = { 0 };
			strcpy(query.codename, "ALIGNMENT");
			strcpy(query.instrument, "SC");
			set_caldb_query_qualifiers(&query, par->alignfile);
			if (simple_caldb_query(&query, task->fptr, path))
				report_warning("unable to resolve alignfile=%s\n", par->alignfile);
			else
				HDpar_note("alignfile", path);
		}
		else
			strcpy(path, par->alignfile);

		/* open alignment file */
		task->align = readAlign(path);
		if (!task->align)
		{
			report_error("unable to open alignment file '%s'\n", path);
			code = FILE_NOT_OPENED;
		}

		if (code)
			return code;
	}

	task->qref = allocateQuat();
	task->qsample = allocateQuat();
	task->qtmp = allocateQuat();
	task->q1 = allocateQuat();
	task->q2 = allocateQuat();
	task->rm = allocateRotMatrix();

	return code;
}


static int parse_double(const char *in, double *out)
{
	int code = 0;
	char *end = 0;
	*out = strtod(in, &end);
	if (end == in)
		code = 1;
	else if (end && *end)
		code = 2;
	return code;
}


static int parseOptions(Task *task, const char *options)
{
	int code = 0;
	int i, count;
	char **elements;
	char *p, *e;
	double value;

	task->max_angle_d = -1;
	task->max_offset_s = -1;

	elements = string_split(options, ',', &count);
	for (i = 0; i < count; ++i)
	{
		p = elements[i];
		e = strchr(p, '=');
		if (e)
		{
			*e = 0;
			if (parse_double(e+1, &value))
			{
				code = 1;
				report_warning("invalid %s value '%s'\n", p, e+1);
			}
			else if (!strcasecmp(p, "ANGLE_d"))
				task->max_angle_d = value;
			else if (!strcasecmp(p, "OFFSET_s"))
				task->max_offset_s = value;
			else
				report_warning("unknown option '%s'\n", p);
		}
		else
		{
			if (!strcasecmp(p, "OUTLIER"))
				task->onlyOutliers = 1;
			else if (!strcasecmp(p, "AND"))
				task->operator = OP_AND;
			else if (!strcasecmp(p, "OR"))
				task->operator = OP_OR;
			else
				report_warning("unknown option '%s'\n", p);
		}
	}

	free(elements);

	return code;
}


static void makeUnit (const char *tag, double ra_d, double dec_d, double *unit)
{
	Math_rd2unit(Math_toRadians(ra_d), Math_toRadians(dec_d), unit);
	report_monitor("%s: RA=%f, Dec=%f\n",
			tag, ra_d, dec_d);
	report_verbose("%s: unit { %f, %f, %f }\n",
			tag, unit[0], unit[1], unit[2]);
}


static int run (Task * task)
{
	int code = 0;
	const Parameters * par = task->par;

	code = parseOptions(task, par->options);

	makeUnit("reference", par->ra_d, par->dec_d, task->unominal);

	convertRADecRollToQuat(task->align, task->qref, par->ra_d, par->dec_d, par->roll_d);
	convertQuatToRotMatrix(task->rm, task->qref);
	{
		double axis[3] = { 1, 0, 0 };
		double w[3];
		invertQuat(task->qtmp, task->qref);
		convertQuatToRotMatrix(task->rm, task->qtmp);
		applyRotMatrixToVector(task->rm, w, axis);
		report_verbose("q * { 1, 0, 0 } = { %f, %f, %f }\n", w[0], w[1], w[2]);
	}

	if (!code)
		code = loadAttitudeData(task, par->time_s);

	if (!code)
	{
		/* interpolate */
		double dt0, dt;
		dt0 = par->time_s - task->t1;
		dt = task->t2 - task->t1;
		if (dt > 0)
		{
			QUAT * qdelta;
			qdelta = allocateQuat();
			getQuatOfChange(qdelta, task->q1, task->q2);
			multiplyQuatByScalar(task->qtmp, qdelta, dt0 / dt);
			productOfQuats(task->qsample, task->q1, task->qtmp);
			destroyQuat(qdelta);
		}
	}

	if (!code)
	{
		int checks = 0;
		int outliers = 0;
		int outlier = 0;
		double ra_d, dec_d, roll_d;
		double angle_d, offset_s;
		double *p = task->pointing;

		offset_s = Math_min(fabs(par->time_s - task->t1),
				fabs(par->time_s - task->t2));

		convertQuatToRADecRoll(task->align, task->qsample, &ra_d, &dec_d, &roll_d);
		makeUnit("sampled", ra_d, dec_d, task->usampled);

		angle_d = Math_toDegrees(Math_u3angle(task->unominal, task->usampled));
		report_monitor("offset: %.3f [s]\n", offset_s);
		report_monitor("angle: %.3f [deg]\n", angle_d);
		report_monitor("angle: %.3f [arcsec]\n", 3600 * angle_d);

		if (task->max_angle_d > 0)
		{
			++checks;
			if (angle_d > task->max_angle_d)
				++outliers;
		}

		if (task->max_offset_s > 0)
		{
			++checks;
			if (offset_s > task->max_offset_s)
				++outliers;
		}

		if (outliers > 0)
		{
			if (task->operator == OP_AND)
			{
				if (outliers == checks)
					outlier = 1;
			}
			else if (task->operator == OP_OR)
			{
				outlier = 1;
			}
		}

		if (outlier)
		{
			task->qout = task->qref;

			p[0] = par->ra_d;
			p[1] = par->dec_d;
			p[2] = par->roll_d;
		}
		else
		{
			task->qout = task->qsample;

			p[0] = ra_d;
			p[1] = dec_d;
			p[2] = roll_d;
		}

		report_monitor("selected %s attitude\n", outlier ? "reference" : "sampled");

		if (!par->outfileNone)
			if (!task->onlyOutliers || outlier)
			{
				report_verbose("writing attitude file\n");
				code = write_attitude(task);
			}
	}

	return code;
}


static int get_parameters (Parameters * par)
{
	int code = 0;

	if (code == PIL_OK)
		code = PILGetFname("infile", par->infile);

	if (code == PIL_OK)
		code = PILGetFname("outfile", par->outfile);

	if (code == PIL_OK)
		code = PILGetString("operation", par->operation);

	if (code == PIL_OK)
		code = PILGetFname("alignfile", par->alignfile);

	if (code == PIL_OK)
		code = PILGetReal("time", &par->time_s);

	if (code == PIL_OK)
		code = PILGetString("options", par->options);

	if (code == PIL_OK)
		code = PILGetReal("ra", &par->ra_d);

	if (code == PIL_OK)
		code = PILGetReal("dec", &par->dec_d);

	if (code == PIL_OK)
		code = PILGetReal("roll", &par->roll_d);

	if (code == PIL_OK)
		code = PILGetBool("history", &par->history);

	par->outfileNone = !strcasecmp(par->outfile, "NONE");
	if (!par->outfileNone)
		headas_clobberfile(par->outfile);

	return code;
}


/****************************************************************************
* This is the main FTOOL function
****************************************************************************/
int minnow(void)
{
	int code = 0;
	Parameters par = { 0 };
	Task task = { 0 };

	add_report_function(&report_headas);

	set_toolname("minnow");
	set_toolversion("0.2");

	code = get_parameters(&par);

	if (!code)
		code = initialize(&task, &par);

	if (!code)
		code = run(&task);

	remove_report_function(&report_headas);

	return code;
}



