/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotflatfield/uvotflatfield.c,v $
 * $Revision: 1.12 $
 * $Date: 2008/04/08 13:48:45 $
 *
 *
 * $Log: uvotflatfield.c,v $
 * Revision 1.12  2008/04/08 13:48:45  rwiegand
 * Report warning if flat field correction is unity everywhere.
 *
 * Revision 1.11  2007/06/21 15:49:31  rwiegand
 * Write FLATCORR keyword.
 *
 * Revision 1.10  2005/09/16 12:16:39  rwiegand
 * Allow user to qualify CALDB queries.
 *
 * Revision 1.9  2005/05/24 20:53:21  rwiegand
 * Moved CALDB interface from UVOT specific to attitude library.
 *
 * Revision 1.8  2005/05/20 19:16:11  rwiegand
 * Record calibration file(s) retrieved from CALDB in output.
 *
 * Revision 1.7  2004/12/02 22:13:36  rwiegand
 * Changed processing when user specifies primary extension.
 *
 * Revision 1.6  2004/11/18 17:45:22  rwiegand
 * Corrected codename for CALDB pixel to pixel sensitivity.
 *
 * Revision 1.5  2004/10/17 11:33:57  rwiegand
 * Updated name of calibration pixel to pixel sensitivity extension.
 *
 * Revision 1.4  2004/10/04 18:08:17  rwiegand
 * Implemented CALDB interface.
 *
 * Revision 1.3  2004/08/05 19:52:59  rwiegand
 * Calibration file stores flat field in PIXSENS extension.
 *
 * Revision 1.2  2004/05/14 17:55:44  rwiegand
 * For a binned image, divide by the *average* binned flatfield.  Propagate
 * any NULL or zero in flatfield to the output as NULL.
 *
 * Revision 1.1  2004/05/13 23:07:44  rwiegand
 * Tool for flat fielding UVOT data.
 *
 */


#include <stdio.h>			/* pil.h uses but does not include */
#include <math.h>
#include <string.h>

#include "headas.h"
#include "pil.h"
#include "report.h"
#include "genimage.h"
#include "keyutil.h"

#include "uvottool.h"
#include "uvotfile.h"
#include "caldbquery.h"


#define TOOLSUB uvotflatfield
#define VERSION 0.2
#include "headas_main.c"


#define PAR_INFILE   "infile"
#define PAR_OUTFILE  "outfile"
#define PAR_FLATFILE "flatfile"


#define CREATOR _STRINGIFY(TOOLSUB) " " _STRINGIFY(VERSION)
#define ORIGIN "GSFC"



typedef struct
{
	int chatter;
	int clobber;
	int history;
	int checksum;

	char infile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];

	char flatfile[PIL_LINESIZE];

} Parameters;




typedef struct
{
	/* Parameters * par; */

	fitsfile * iptr;
	fitsfile * optr;

	FImage flatfield;
	FImage * flattened;

	RawWindow * window;

} FlatFielder;



static int
get_parameters (Parameters * p)
{
	int code = 0;
	int pill = 0;

	if (!pill)
		pill = PILGetFname(PAR_INFILE, p->infile);

	if (!pill)
		pill = PILGetFname(PAR_FLATFILE, p->flatfile);

	if (!pill)
		pill = PILGetFname(PAR_OUTFILE, p->outfile);

	p->clobber = headas_clobpar;
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
load_flatfield (FImage * image, const char * path)
{
	int code = 0;
	ImageIO io = { 0 };
	char pathext[1024];

	strcpy(pathext, path);
	strcat(pathext, "[PIXTOPIXSENS]");

	code = fimage_read(image, pathext, &io);

	if (code)
		report_error("unable to load flat field calibration file\n");
	else {
		int unity = 1;
		float * p = image->oned;
		float * end = image->oned + image->width * image->height;
		report_status("flat field is %d by %d\n", image->width, image->height);
		while (unity && p != end)
			if (*p++ != 1.0f)
				unity = 0;
		if (unity)
			report_warning("flat field is unity\n");
	}

	return code;
}


/* note we use absolute coordinates throughout */
static int
iterate_flatfield (FImage * image, IIState * state)
{
	int code;
	float raw;
	FlatFielder * tool = (FlatFielder *) state->user;
	FImage * flat = tool->flattened;

	code = fimage_get(image, state->x0, state->y0, &raw);
	if (code)
		report_error("unable to get raw pixel at %d,%d [%d]\n",
					state->x0, state->y0, code);
	else if (raw == image->null)
		{
			code = fimage_set(flat, state->x0, state->y0, flat->null);
			if (code)
				report_error("unable to set null flattened pixel at %d,%d [%d]\n",
							state->x0, state->y0, code);
		}
	else
		{
			/* bin up the corresponding flat field calibration pixels */
			int i, j;
			int null = 0;
			float sum = 0;
			RawWindow * window = tool->window;
			for (i = 0; i < window->binx; ++i)
				for (j = 0; j < window->biny; ++j)
					{
						int cc;
						float zf;
						int fullx = window->x0 + state->x0 * window->binx + i;
						int fully = window->y0 + state->y0 * window->biny + j;
						cc = fimage_get(&tool->flatfield, fullx, fully, &zf);
						if (cc)
							{
								code = cc;
								report_error("unable to get flat field pixel at %d,%d [%d]\n",
											fullx, fully, cc);
							}
						else if (zf == flat->null)
							null = 1;
						else
							sum += zf;
					}

			if (!code)
				{
					sum /= window->binx * window->biny;

					if (null || sum == 0)
						code = fimage_set(flat, state->x0, state->y0, flat->null);
					else
						code = fimage_set(flat, state->x0, state->y0, raw / sum);

					if (code)
						report_error("unable to set flattened pixel at %d,%d [%d]\n",
									state->x0, state->y0, code);
				}
		}
		
	return code;
}



static int
exclude_keywords (const HeaderRecord * record, void * user)
{
	int accept = 0;
	accept = strcmp(record->keyname, "BLANK");
	return accept;
}


static int
flatfield_hdu (FlatFielder * tool)
{
	int code = 0;
	FImage raw = { 0 };
	FImage flat = { 0 };
	ImageIO io = { 0 };
	FITSHeader header = { 0 };

	tool->flattened = &flat;
	header.accept = accept_nonstructural_records;
	io.header = &header;

	raw.null = flat.null = -1;

	if (!code)
		code = fimage_read_chdu(&raw, tool->iptr, &io);

	if (!code)
		code = add_header_record(&header, "FLATCORR",
					"FLATCORR=                    T / Flatfield applied?");

	if (!code)
		code = fimage_allocate(&flat, raw.width, raw.height);

	if (!code)
		{
			code = load_raw_window(tool->iptr, tool->window);
			if (code)
				{
					code = TASK_INPUT_ERROR;
					report_error("unable to load raw window\n");
				}
		}

	if (!code)
		{
			IIState state = { 0 };
			state.user = tool;
			code = fimage_iterate(&raw, iterate_flatfield, &state);
		}

	header.accept = exclude_keywords;
	if (!code)
		code = fimage_append(&flat, tool->optr, &io);

	if (!code && tool->window->wcs)
		save_raw_window(tool->optr, tool->window);

	fimage_release(&raw);
	fimage_release(&flat);
	tool->flattened = 0;

	release_header(&header);

	return code;
}


static int
run_parameters (Parameters * p)
{
	int code = 0;
	int status = 0;

	FlatFielder tool = { 0 };
	FileSpec spec = { 0 };
	RawWindow window = { 0 };

	tool.window = &window;
	tool.flatfield.null = -1;

	if (!code)
		{
			/* open input */
			fits_open_file(&tool.iptr, p->infile, READONLY, &status);
			if (status)
				{
					code = status;
					report_error("unable to open %s [%d]\n", p->infile, status);
				}
		}

	if (!code)
		{
			if (file_parse_path(p->infile, &spec))
				{
					code = TASK_INPUT_ERROR;
					report_error("invalid input '%s'\n", p->infile);
				}
			else if (spec.hdu1)
				{
					if (file_resolve_hdu(&spec, tool.iptr))
						{
							code = TASK_INPUT_ERROR;
							report_error("invalid input HDU specified '%s'\n", p->infile);
						}
					else
						report_verbose("user specified extension '%s' => %d\n",
								spec.ext, spec.hdu1);
				}
		}

	if (!code)
		{
			char path[QUERY_MAXPATH];
			if (!strncasecmp(p->flatfile, "CALDB", 5))
				{
					CALDBQuery query = { 0 };
					strcpy(query.codename, "CAL_FLAT");
					set_caldb_query_qualifiers(&query, p->flatfile);
					code = simple_caldb_query(&query, tool.iptr, path);
					if (!code)
						HDpar_note(PAR_FLATFILE, path);
				}
			else
				strcpy(path, p->flatfile);

			if (!code)
				code = load_flatfield(&tool.flatfield, path);
		}

	if (!code && p->clobber)
		headas_clobberfile(p->outfile);

	if (!code)
		{
			/* create output */
			fits_create_file(&tool.optr, p->outfile, &status);
			if (status)
				{
					code = status;
					report_error("unable to create %s [%d]\n", p->outfile, status);
				}
		}

	if (!code)
		{
			file_create_primary(tool.iptr, tool.optr, &spec);

			/*
			 * modify DATE and CREATOR keywords in the primary header
			 */
			fits_write_date(tool.optr, &status);
			fits_update_key(tool.optr, TSTRING, "CREATOR", CREATOR,
											"Software that created this file", &status);
			fits_update_key(tool.optr, TSTRING, "ORIGIN", ORIGIN,
											"Processing Site", &status);

			if (p->history)
				HDpar_stamp(tool.optr, 1, &status);
			if (status)
				{
					code = TASK_OUTPUT_ERROR;
					report_error("unable to write history\n");
				}

			if (p->checksum && fits_write_chksum(tool.optr, &status))
				{
					code = TASK_OUTPUT_ERROR;
					report_error("unable to write checksums\n");
				}
		}

	if (!code)
		{
			if (spec.hdu1)
				{
					/*
					 * just process the specified HDU
					 */
					code = flatfield_hdu(&tool);
				}
			else
				{
					/*
					 * process all extensions
					 */
					int i;
					int hdus = 0;
					fits_get_num_hdus(tool.iptr, &hdus, &status);

					if (hdus < 2)
						report_warning("no extensions to flat field\n");

					for (i = 2; i <= hdus; ++i)
						{
							fits_movabs_hdu(tool.iptr, i, IMAGE_HDU, &status);

							if (status)
								{
									code = status;
									report_error("unable to move to HDU %d [%d]\n",
													i, status);
								}
							else
								code = flatfield_hdu(&tool);
						}
				}
		}

	if (tool.optr)
		{
			/* close output */
			int tmp = 0;
			if (code)
				fits_delete_file(tool.optr, &tmp);
			else
				fits_close_file(tool.optr, &tmp);
		}

	if (tool.iptr)
		{
			/* close input */
			int tmp = 0;
			fits_close_file(tool.iptr, &tmp);
		}

	/* cleanup */
	fimage_release(&tool.flatfield);

	return code;
}


int
uvotflatfield ()
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

