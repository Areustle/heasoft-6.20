/*
 * $Source: /headas/headas/attitude/tasks/aberrator/io.c,v $
 * $Revision: 1.5 $
 * $Date: 2012/08/22 19:21:28 $
 *
 *
 * $Log: io.c,v $
 * Revision 1.5  2012/08/22 19:21:28  craigm
 * Update to task version 1.1.  Add 'orbext' parameter to support NuSTAR orbit extension named 'ORBIT'.  Add 'attcol' parameter to support either POINTING or QPARAM as input (previously was only POINTING).  Unit test still passes. --CM
 *
 * Revision 1.4  2005/09/16 12:12:33  rwiegand
 * Allow user to qualify CALDB query.
 *
 * Revision 1.3  2005/03/04 19:34:48  rwiegand
 * Made fits_copy_col column numbers distinct to avoid losing T*n keywords.
 *
 * Revision 1.2  2005/01/30 00:15:03  rwiegand
 * Followed some HEAdas practices.
 *
 * Revision 1.1  2005/01/29 23:55:11  rwiegand
 * Tool to adjust an attitude file for velocity aberration.
 *
 */


#include <stdio.h>	/* pil.h uses but does not include */
#include <math.h>
#include <string.h>

#include "headas.h"
#include "pil.h"
#include "report.h"
#include "tool.h"
#include "fitsio.h"
#include "orbfile.h"
#include "align.h"
#include "aberrator.h"
#include "headas_gti.h"
#include "caldbquery.h"


#define TOOLSUB attaberr
#define VERSION 1.1
#include "headas_main.c"


#define EXTNAME_ATTITUDE "ATTITUDE"

#define COLNAME_TIME         "TIME"
#define COLNAME_INPUT        "POINTING"
#define COLNAME_INERTIAL     "POINTING_INERTIAL"
#define COLNAME_APPARENT     "POINTING"
#define COLNAME_QUATERNION   "QPARAM"


/* CFITSIO attitude iterator columns */
enum
{
	ITERATOR_TIME,
	ITERATOR_INERTIAL,
	ITERATOR_APPARENT,
	ITERATOR_QUATERNION,
	ITERATOR_COLUMNS
};



typedef struct
{
	int chatter;
	int history;
	int checksum;

	char attfile[PIL_LINESIZE];
	char orbfile[PIL_LINESIZE];
	char alignfile[PIL_LINESIZE];
	char orbfile_extname[PIL_LINESIZE];

	int move_earth;
	int move_satellite;
	int from_quat;

} Parameters;



static int
get_parameters (Parameters * p)
{
	int code = 0;
	int pill = 0;
	

	if (!pill)
		pill = PILGetFname("infile", p->attfile);

	if (!pill)
		pill = PILGetFname("orbfile", p->orbfile);

	if (!pill)
		pill = PILGetFname("alignfile", p->alignfile);

	if (!pill)
		pill = PILGetBool("earthvel", &p->move_earth);

	if (!pill)
		pill = PILGetBool("satvel", &p->move_satellite);

	if (!pill) {
		char attcolname[PIL_LINESIZE];

		pill = PILGetString("attcol", attcolname);
		if (strcasecmp(attcolname,"QPARAM") == 0) {
		  p->from_quat = 1;
		} else if (strcasecmp(attcolname,"POINTING") == 0) {
		  p->from_quat = 0;
		} else {
		  pill = 1;
		  report_error("attcol must be either POINTING or QPARAM");
		}
	}

	if (!pill) {
		pill = PILGetString("orbext", p->orbfile_extname);
	}

	p->chatter = headas_chatpar;
	p->checksum = 1;
	get_history(&p->history);

	if (pill)
		{
			code = TASK_SETUP_ERROR;
			report_error("unable to load parameters\n");
		}
	else
		report_verbose("parameters loaded\n");

	return code;
}



static int
iterate_attitude_aux (long totaln, long offset, long firstn,
						long nvalues, int narrays, iteratorCol * icols,
						void * user)
{
	int code = 0;

	int i;
	Aberrator * tool = (Aberrator *) user;

	double * ptime;
	double * pinertial;
	double * papparent;
	double * pquaternion;

	report_verbose("iterate_events_aux(totaln %ld, offset %ld, firstn %ld, nvalues %ld, narrays %d, ...)\n", totaln, offset, firstn, nvalues, narrays);

	ptime = (double *) fits_iter_get_array(icols + ITERATOR_TIME);
	pinertial = (double *) fits_iter_get_array(icols + ITERATOR_INERTIAL);
	papparent = (double *) fits_iter_get_array(icols + ITERATOR_APPARENT);
	pquaternion = (double *) fits_iter_get_array(icols + ITERATOR_QUATERNION);

	/* 0th value indicates null */
#if 0
	nulltime = time[0];
	nullinertial = pinertial[0];
	...
#endif

	++ptime;
	++pinertial;
	++papparent;
	++pquaternion;

	for (i = 0; !code && i < nvalues; ++i)
		{
			AttitudeRecord record;

			record.time = ptime[i];
			record.inertial = pinertial + 3 * i;
			record.apparent = papparent + 3 * i;
			record.quaternion = pquaternion + 4 * i;

			code = correct_attitude_record(tool, &record);
		}

	return code;
}


static void
initialize_iterator_column (iteratorCol * icol, char * name, fitsfile * fptr,
							int datatype, int coltype, int * status)
{
	int tmp = fits_iter_set_by_name(icol, fptr, name, datatype, coltype);

	if (tmp)
		{
			if (!*status)
				*status = tmp;
			report_error("unable to initialize iterator column '%s' [%d]\n",
							name, status);
		}
}



static int
iterate_attitude (Aberrator * tool)
{
	int code = 0;
	int status = 0;

	iteratorCol icols[ITERATOR_COLUMNS] = { { 0 } };

	initialize_iterator_column(&icols[ITERATOR_TIME], COLNAME_TIME,
						tool->fptr, TDOUBLE, InputCol, &status);
	initialize_iterator_column(&icols[ITERATOR_INERTIAL], COLNAME_INERTIAL,
						tool->fptr, TDOUBLE, InputCol, &status);
	initialize_iterator_column(&icols[ITERATOR_APPARENT], COLNAME_APPARENT,
						tool->fptr, TDOUBLE, OutputCol, &status);
	initialize_iterator_column(&icols[ITERATOR_QUATERNION], COLNAME_QUATERNION,
						tool->fptr, TDOUBLE, InputOutputCol, &status);

	if (status)
		{
			code = status;
			report_error("unable to initialize attitude iteration [%d]\n", status);
		}

	fits_iterate_data(ITERATOR_COLUMNS, icols, 0, 0,
						iterate_attitude_aux, tool, &status);

	if (status)
		{
			code = status;
			report_error("attitude iteration failed [%d]\n", status);
		}

	return code;
}



static int
run_parameters (Parameters * p)
{
	int code = 0;
	int * pstatus = &code;
	Aberrator tool = { 0 };
	int col_inertial;
	int col_apparent;

	{
		tool.move_earth = p->move_earth;
		tool.move_satellite = p->move_satellite;
		tool.quat = allocateQuat();
		tool.from_quat = p->from_quat;
	}

	if (!code)
		{
			/* open input */
			fits_open_file(&tool.fptr, p->attfile, READWRITE, pstatus);
			if (code)
				report_error("unable to open '%s' [%d]\n", p->attfile, code);
		}

	if (!code && p->move_earth)
		{
			tool.mjdref = HDget_frac_time(tool.fptr, "MJDREF", 0, 0, pstatus);
			if (code)
				report_error("unable to determine mjdref\n");
		}

	if (!code && p->move_satellite)
		{
			/* open orbit file */
			tool.orbfile = openOrbitFile(p->orbfile, p->orbfile_extname);
			if (!tool.orbfile)
				{
					report_error("unable to open orbit file '%s'\n", p->orbfile);
					code = TASK_INPUT_ERROR;
				}
		}

	if (!code)
		{
			char path[QUERY_MAXPATH];
			if (!strncasecmp(p->alignfile, "CALDB", 5)) {
				CALDBQuery query = { 0 };
				strcpy(query.codename, "ALIGNMENT");
				strcpy(query.instrument, "SC");
				set_caldb_query_qualifiers(&query, p->alignfile);
				if (simple_caldb_query(&query, tool.fptr, path))
					report_warning("unable to resolve alignfile=%s\n", p->alignfile);
				else
					HDpar_note("alignfile", p->alignfile);
			}
			else
				strcpy(path, p->alignfile);

			/* open alignment file */
			tool.align = readAlign(path);
			if (!tool.align)
				{
					report_error("unable to open alignment file '%s'\n", path);
					code = TASK_INPUT_ERROR;
				}
		}

	if (!code)
		{
			fits_movnam_hdu(tool.fptr, BINARY_TBL, EXTNAME_ATTITUDE, 0, pstatus);
			if (code)
				report_error("unable to move to %s table [%d]\n",
							EXTNAME_ATTITUDE, code);
		}

	if (!code)
		{
			/* check if it looks like this conversion has already been done
				should have a keyword... */
			int status = 0;
			fits_write_errmark();
			fits_get_colnum(tool.fptr, CASEINSEN, COLNAME_INERTIAL,
						&col_inertial, &status);
			fits_clear_errmark();
			if (!status)
				{
					code = TASK_INPUT_ERROR;
					report_error("%s[%s] already has a %s column\n",
								p->attfile, EXTNAME_ATTITUDE, COLNAME_INERTIAL);
				}
		}

	if (!code)
		{
			/* copy POINTING column */
			/* we will call the first one POINTING, the second POINTING_INERTIAL */
			int create = TRUE;
			char key[16];
			int fauxpas = 0;

			if (fits_get_colnum(tool.fptr, CASEINSEN, COLNAME_INPUT,
								&col_apparent, pstatus))
				report_error("unable to locate %s column [%d]\n",
									COLNAME_INPUT, code);

			else if (!(col_inertial = col_apparent + 1))
				fauxpas = 1;		/* always true [columns count from 1] */

			else if (fits_copy_col(tool.fptr, tool.fptr,
							col_apparent, col_inertial, create, pstatus))
				report_error("unable to copy %s column [%d]\n",
									COLNAME_INPUT, code);

			else if (!sprintf(key, "TTYPE%d", col_inertial))
				fauxpas = 1;    /* C library failure */

			else if (fits_update_key_str(tool.fptr, key, COLNAME_INERTIAL, 0, pstatus))
				report_error("unable to rename %s column [%d]\n",
										COLNAME_INERTIAL, code);

			else
				report_status("initialized %s column\n", COLNAME_INERTIAL);

			if (fauxpas)
				report_error("whoa, logic or C library error\n");
		}

	if (!code && fits_flush_file(tool.fptr, pstatus))
		report_error("unable to flush FITS file [%d]\n", code);

	if (!code)
		code = iterate_attitude(&tool);

	if (!code)
		{
			HDpar_stamp(tool.fptr, 0, pstatus);
			fits_write_chksum(tool.fptr, pstatus);
			if (code)
				report_error("error writing history/checksums [%d]\n", code);
		}

	if (tool.fptr)
		{
			int tmp = 0;
			if (!code)
				fits_close_file(tool.fptr, &tmp);
			else
				; /* leave changes uncommited */
			if (tmp)
				{
					code = TASK_OUTPUT_ERROR;
					report_error("unable to close file [%d]\n", tmp);
				}
		}

	return code;
}


int
TOOLSUB ()
{
	int code = 0;

	Parameters p = { 0 };

	set_toolname(_STRINGIFY(TOOLSUB));
	set_toolversion(_STRINGIFY(VERSION));

	add_report_function(&report_headas);

	if (!code)
		code = get_parameters(&p);

	if (!code)
		code = run_parameters(&p);

	remove_report_function(&report_headas);

	return code;
}

