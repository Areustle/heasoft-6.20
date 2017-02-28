/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotstarid/listgsc/listgsc.c,v $
 * $Revision: 1.2 $
 * $Date: 2005/08/05 15:04:47 $
 *
 *		Lists contents of guide star catalog files in a region of the
 *		sky defined by a ra,dec (center) and radius [all in degrees].
 *		An epoch parameter in years can be passed to apply proper motion
 *		relative to 2000.
 *
 *
 * $Log: listgsc.c,v $
 * Revision 1.2  2005/08/05 15:04:47  rwiegand
 * Include HTM trixel in ID.
 *
 * Revision 1.1  2004/06/17 21:56:33  rwiegand
 * This tool accepts a list of FITS paths (to tables of star catalog information),
 * a circular region on the sky (given by RA, DEC, and radius in degrees), and
 * an epoch, and displays objects in those tables with are in that region at that
 * epoch (i.e. this tool applies the proper motion).
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "fitsio.h"


enum Status
{
	SUCCESS,
	MATCHED_ARGUMENT,
	BAD_ARGUMENT,
	BAD_INPUT,
	ENUM_DUMMY
};


enum IteratorColumns
{
	COL_ID,
	COL_RA,
	COL_DEC,
	COL_MAG,
	COL_TYPE,
	COL_RA_PROPER,
	COL_DEC_PROPER,
	COL_COUNT
};


typedef struct
{
	/* all degrees */
	double ra;
	double dec;

	double radius;

	double epoch;

} Parameters;


typedef struct
{
	int code;

	fitsfile * fptr;

	int columns;
	iteratorCol itercol[COL_COUNT];

	double years;

	/* radians */
	double ra;
	double dec;
	double radius;

	double x0, y0, z0; /* unit vector */
	double mincos;

	char prefix[32];

} Tool;


static const char * TYPE[] = {
		"star",
		"galaxy",
		"blend",
		"non-star",
		"unclassified",
		"defect"
};

#define NUMTYPE (sizeof(TYPE) / sizeof(TYPE[0]))


int getDouble (int code, const char * arg, const char * key, double * out)
{
	int len = strlen(key);
	if (!code && !strncmp(arg+1, key, len) && arg[len+1] == '=')
		{
			double tmp;
			char * tail;
			code = MATCHED_ARGUMENT;
			tmp = strtod(arg + len + 2, &tail);
			if (tail && *tail)
				code = BAD_ARGUMENT;
			else
				*out = tmp;
		}

	return code;
}


double degrees_to_radians (double degrees)
{
	double radians = degrees * M_PI / 180;
	return radians;
}


double mas_to_radians (double mas)
{
	double radians = degrees_to_radians(mas / 3600 / 1000);
	return radians;
}


void rd2xyz (double ra, double dec, double scale,
		double * x, double * y, double * z)
{
	*x = cos(ra * scale) * cos(dec * scale);
	*y = sin(ra * scale) * cos(dec * scale);
	*z = sin(dec * scale);
}


int iterate_catalog (long totaln, long offset, long firstn, long nvalues,
			int narrays, iteratorCol *data, void * user)
{
	int status = 0;
	Tool * tool = (Tool *) user;
	long i;
	long * id;
	double * ra, raNull, * dec, decNull;
	float * mag, magNull, * rap, rapNull, * decp, decpNull;
	long * type, typeNull;

	id    = (long *)   fits_iter_get_array(&tool->itercol[COL_ID]);
	ra    = (double *) fits_iter_get_array(&tool->itercol[COL_RA]);
	dec   = (double *) fits_iter_get_array(&tool->itercol[COL_DEC]);
	mag   = (float *)  fits_iter_get_array(&tool->itercol[COL_MAG]);
	type  = (long *)   fits_iter_get_array(&tool->itercol[COL_TYPE]);
	rap   = (float *)  fits_iter_get_array(&tool->itercol[COL_RA_PROPER]);
	decp  = (float *)  fits_iter_get_array(&tool->itercol[COL_DEC_PROPER]);

	raNull = ra[0];
	decNull = dec[0];
	magNull = mag[0];
	typeNull = type[0];
	rapNull = rap[0];
	decpNull = decp[0];

	for (i = 1; i <= nvalues; ++i)
		{
			double rai, deci;
			double xi, yi, zi;
			double cosalpha;

			/* apply proper motions [milliarcsec / year] */
			rai = ra[i] + mas_to_radians(rap[i] * tool->years);
			deci = dec[i] + mas_to_radians(decp[i] * tool->years);

			rd2xyz(rai, deci, 1, &xi, &yi, &zi);

			cosalpha = tool->x0 * xi + tool->y0 * yi + tool->z0 * zi;

			if (cosalpha > tool->mincos)
				{
					const char * pc;
					printf("ID=%s:%ld, RA=%g, DEC=%g, MAG=",
							tool->prefix, id[i], rai, deci);

					if (magNull && mag[i] == magNull)
						fputs("NULL", stdout);
					else
						printf("%f", mag[i]);

					fputs(", TYPE=", stdout);
					if (typeNull && type[i] == typeNull)
						pc = "NULL";
					else if (type[i] < 0 || type[i] >= NUMTYPE)
						pc = "invalid";
					else
						pc = TYPE[type[i]];
					fputs(pc, stdout);

					putchar('\n');
				}
		}

	return status;
}


void addColumn (Tool * tool, int index, char * name, int datatype)
{
	int status;
	iteratorCol * col = &tool->itercol[index];

	status = fits_iter_set_by_name(col, tool->fptr, name, datatype, InputCol);

	if (status)
		{
			tool->code = BAD_INPUT;
			printf("unable to initialize iterator column '%s' [%d]\n",
						name, status);
		}
}


int listStars (const Parameters * par, const char * path)
{
	int status = 0;
	Tool tool = { 0 };

	tool.years = par->epoch - 2000;

	tool.ra = degrees_to_radians(par->ra);
	tool.dec = degrees_to_radians(par->dec);
	tool.radius = degrees_to_radians(par->radius);
	tool.mincos = cos(tool.radius);

	rd2xyz(par->ra, par->dec, degrees_to_radians(1), &tool.x0, &tool.y0, &tool.z0);
	{
		char * slash = rindex(path, '/');
		tool.prefix[0] = 0;
		if (slash)
			strcpy(tool.prefix, slash + 1);
		tool.prefix[8] = 0;
	}

	fits_open_file(&tool.fptr, path, READONLY, &status);
	if (status)
		printf("unable to open '%s' [%d]\n", path, status);

	if (!status)
		{
			addColumn(&tool, COL_ID, "gscID2", TLONG);
			addColumn(&tool, COL_RA, "RA", TDOUBLE);
			addColumn(&tool, COL_DEC, "DEC", TDOUBLE);
			addColumn(&tool, COL_MAG, "JpgMag", TFLOAT);
			addColumn(&tool, COL_TYPE, "classification", TLONG);
			addColumn(&tool, COL_RA_PROPER, "RA_PROPER", TFLOAT);
			addColumn(&tool, COL_DEC_PROPER, "DEC_PROPER", TFLOAT);
		}

	if (!status)
		{
			if (fits_iterate_data(COL_COUNT, tool.itercol,
						0 /* offset */, 0 /* default rows per iteration */,
						&iterate_catalog, &tool, &status))
				printf("error iterating data [%d]\n", status);
		}

	if (tool.fptr)
		{
			int tmp = 0;
			fits_close_file(tool.fptr, &tmp);
		}

	return status;
}


int main (int argc, char ** argv)
{
	Parameters par = { 0 };
	int code = 0;
	int i;

	par.epoch = 2000;
	par.radius = 1. / 60;

	for (i = 1; !code && i < argc; ++i)
		{
			const char * arg = argv[i];
			if (*arg == '-')
				{
					code = getDouble(code, arg, "ra", &par.ra);
					code = getDouble(code, arg, "dec", &par.dec);
					code = getDouble(code, arg, "radius", &par.radius);
					code = getDouble(code, arg, "epoch", &par.epoch);

					if (code == MATCHED_ARGUMENT)
						code = 0;
					else if (!code)
						printf("unknown argument '%s'\n", arg);
					else
						printf("argument syntax '%s'\n", arg);
				}
			else
				code = listStars(&par, arg);
		}

	return code;
}


#if 0
	/* converting to a unit vector and back to RA/DEC takes care of this */
					/* normalize RA, DEC */
					if (deci > M_PI / 2)
						{
							rai += M_PI;
							deci = M_PI - deci;
						}
					else if (deci < -M_PI / 2)
						{
							rai += M_PI;
							deci = -M_PI - deci;
						}

					if (rai < 0)
						rai += 2 * M_PI;
					else if (rai >= 2 * M_PI)
						rai -= 2 * M_PI;
#endif
