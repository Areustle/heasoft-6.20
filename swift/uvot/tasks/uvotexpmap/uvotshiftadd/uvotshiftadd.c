/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotexpmap/uvotshiftadd/uvotshiftadd.c,v $
 * $Revision: 1.7 $
 * $Date: 2015/05/29 15:27:56 $
 *
 * $Log: uvotshiftadd.c,v $
 * Revision 1.7  2015/05/29 15:27:56  rwiegand
 * Fixed UVOT exposure maps created with method=SHIFTADD to match EXPOSURE (previously missing dead time correction).
 *
 * Revision 1.6  2009/10/07 20:54:53  rwiegand
 * Reimplemented the squashed 1.4 revision to propagate most keywords from
 * sky image to output.
 *
 * Revision 1.5  2008/05/09 19:05:27  irby
 * Call HDfile_system_check with mode "e" instead of 0.  Was exiting
 * with an error (for either value of clobber) when the output file
 * existed.
 *
 * Revision 1.4  2008/03/03 22:25:52  rwiegand
 * Write RAW WCS keywords to shift/add map.
 *
 * Revision 1.4  2008/03/03 20:09:48  rwiegand
 * Added masktrim parameter and pass sky images to uvotshiftadd so all keywords
 * are propagated.
 *
 * Revision 1.3  2007/09/14 15:52:46  rwiegand
 * Removed dependency on TIME column.
 *
 * Revision 1.2  2007/09/06 15:56:27  rwiegand
 * Implemented support for binned images.
 *
 * Revision 1.1  2007/09/05 18:34:14  rwiegand
 * First attempt at supporting onboard shift and add when calculating
 * exposure maps.  Not yet capable of handling binned data.
 *
 */

#include "report.h"
#include "headas.h"
#include "pil.h"
#include "uvottool.h"
#include "uvotfile.h"
#include "genimage.h"

#define EXTERN
#include "exposureMapShift_Add.h"

#define TOOLSUB uvotshiftadd
#include "headas_main.c"


#define PAR_INFILE      "infile"
#define PAR_BADPIXFILE  "badpixfile"
#define PAR_TRACKFILE   "trackfile"
#define PAR_OUTFILE     "outfile"
#define PAR_FRAMETIME   "frametime"
#define PAR_EXPOSURE    "exposure"


typedef struct
{
	int unused;

	char infile[PIL_LINESIZE];
	char badpixfile[PIL_LINESIZE];
	char trackfile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];

	float frametime;
	float exposure;

	int clobber;
	int chatter;
	int history;

} Parameters;



typedef struct
{
	IImage * quality;
	IImage * map;
	RawWindow * window;

	fitsfile * fptr;

	int nframes;

} Task;



enum
{
	TRACK_RAWX,
	TRACK_RAWY,
	TRACK_NFRAMES,
	TRACK_COLUMNS
};


static int
iterate_tracking_aux (long totaln, long offset, long firstn,
								long nvalues, int narrays, iteratorCol * icols,
								void * user)
{
	int code = 0;
	int i;

	int * prawx;
	int * prawy;
	int * pframes;

	Task * task = (Task *) user;

	report_verbose("iterate_tracking_aux(totaln %ld, offset %ld, firstn %ld, "
			"nvalues %ld, narrays %d, ...)\n",
			totaln, offset, firstn, nvalues, narrays);

	prawx = (int *) fits_iter_get_array(icols + TRACK_RAWX);
	prawy = (int *) fits_iter_get_array(icols + TRACK_RAWY);
	pframes = (int *) fits_iter_get_array(icols + TRACK_NFRAMES);

	for (i = 1; i <= nvalues; ++i)
		{
			TRACKING_POSITION tp;

			tp.duration = pframes[i];
			tp.trackingX = prawx[i];
			tp.trackingY = prawy[i];

			task->nframes += pframes[i];

			exposureMapSA_Track(&tp);
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
iterate_tracking_records (Task * task)
{
	int status = 0;

	iteratorCol icols[TRACK_COLUMNS] = { { 0 } };

	initialize_iterator_column(&icols[TRACK_RAWX], "D_RAWX",
									task->fptr, TINT, InputCol, &status);
	initialize_iterator_column(&icols[TRACK_RAWY], "D_RAWY",
									task->fptr, TINT, InputCol, &status);
	initialize_iterator_column(&icols[TRACK_NFRAMES], "N_FRAMES",
									task->fptr, TINT, InputCol, &status);

	if (status)
		report_error("unable to initialize event iteration [%d]\n", status);

	else if (fits_iterate_data(TRACK_COLUMNS, icols, 0, 0,
			iterate_tracking_aux, task, &status))
		report_error("tracking iteration failed [%d]\n", status);

	return status;
}



static int build_exposure_map (Task * task)
{
	int code = 0;
	EXPOSURE_ARGS shiftadd = { 0 };
	SUBMATRIX quality = { 0 };
	SUBMATRIX telemetered = { 0 };
	int xmin, xmax, ymin, ymax;

	xmin = task->window->x0;
	ymin = task->window->y0;
	xmax = xmin + task->quality->width - 1;
	ymax = ymin + task->quality->height - 1;

	shiftadd.null = task->map->null = -1;
	shiftadd.detectorXSize = 2048;
	shiftadd.detectorYSize = 2048;

	shiftadd.quality = &quality;
	shiftadd.telemetered = &telemetered;

	telemetered.xMinIndex = xmin;
	telemetered.yMinIndex = ymin;
	telemetered.xMaxIndex = xmax;
	telemetered.yMaxIndex = ymax;

	quality.xMinIndex = xmin;
	quality.yMinIndex = ymin;
	quality.xMaxIndex = xmax;
	quality.yMaxIndex = ymax;
	quality.data = task->quality->oned;

	if (!code)
		code = exposureMapSA_Init(&shiftadd);

	if (!code)
		code = iterate_tracking_records(task);

	if (!code)
		exposureMapSA_EndTrack();

	if (!code) {
		SUBMATRIX * bobo = shiftadd.exposure;
		IImage * rew = task->map;
		rew->width = bobo->xSize;
		rew->height = bobo->ySize;
		iimage_set_data_oned(rew, bobo->data, 1);
	}

	return code;
}


static int unbin_quality_image (const IImage * binned, IImage * unbinned,
		const RawWindow * window)
{
	int code = 0;
	int i, j, u, v;
	int x, y, z;

	code = iimage_allocate(unbinned,
			binned->width * window->binx,
			binned->height * window->biny);
	unbinned->null = binned->null;

	for (j = 0; j < binned->height; ++j)
		for (i = 0; i < binned->width; ++i) {
			z = iimage_get_relative(binned, i, j);
			for (v = 0; v < window->biny; ++v)
				for (u = 0; u < window->binx; ++u) {
					x = i * window->binx + u;
					y = j * window->biny + v;
					iimage_set_relative(unbinned, x, y, z);
				}
		}

	return code;
}


static int bin_and_scale_exposure_map (const IImage * unbinned,
		FImage * binned, const RawWindow * window, double scale)
{
	int code = 0;
	int i, j, u, v;
	int x, y, null;
	float z, z0, sum;

	code = fimage_allocate(binned,
			unbinned->width / window->binx,
			unbinned->height / window->biny);
	binned->null = unbinned->null;

	for (j = 0; j < binned->height; ++j)
		for (i = 0; i < binned->width; ++i) {
			null = 0;
			sum = 0;
			for (v = 0; v < window->biny; ++v)
				for (u = 0; u < window->binx; ++u) {
					x = i * window->binx + u;
					y = j * window->biny + v;
					z0 = iimage_get_relative(unbinned, x, y);
					if (z0 == unbinned->null)
						null = 1;
					else
						sum += z0;
				}

			if (null)
				z = binned->null;
			else
				z = scale * sum / (window->binx * window->biny);

			fimage_set_relative(binned, i, j, z);
		}

	return code;
}


/* accept all keycard types *except* those in user list */
static int exclude_keycard_types (const HeaderRecord * record, void * user)
{
	HeaderRecord * h = (HeaderRecord *) record;
	const int * p = (int *) user;
	int type = fits_get_keyclass(h->card);

	while (*p) {
		if (*p == type)
			return 0;
		else
			++p;
	}

	return 1;
}


static int run_parameters (const Parameters * p)
{
	int code = 0;
	Task task = { 0 };
	IImage qualityIn = { 0 };
	IImage quality = { 0 };
	IImage map = { 0 };
	FImage mapOut = { 0 };
	ImageIO io = { 0 };
	FITSHeader header = { 0 };
	RawWindow window = { 0 };


	if (!code)
		if (HDfile_system_check(p->outfile, "e")) {
			if (!p->clobber) {
				code = FILE_NOT_CREATED;
				report_error("%s exists and clobber not set\n", p->outfile);
			}
			else if (headas_clobberfile((char*) p->outfile)) {
				code = FILE_NOT_CREATED;
				report_error("unable to clobber %s\n", p->outfile);
			}
		}

	if (!code) {
		int types[] = { TYP_STRUC_KEY, TYP_WCS_KEY, 0 };
		header.user = types;
		header.accept = exclude_keycard_types;
		code = fetch_header_records_path(&header, p->infile);
	}

	if (!code) {
		io.header = 0;

		code = iimage_read(&qualityIn, p->badpixfile, &io);
		if (code) {
			report_error("unable to load quality image %s [%d]\n",
					p->badpixfile, code);
			code = READ_ERROR;
		}
		else
			report_verbose("loaded %s\n", p->badpixfile);
	}

	if (!code) {
		code = parse_raw_window(&header, &window);
		/* ought to ensure quality has same raw window... */
		if (!code) {
			report_verbose("raw window is x0=%d, y0=%d, dx=%d, dy=%d\n",
					window.x0, window.y0, window.dx, window.dy);
			set_header_key_string(&header, "CTYPE1", "RAWX");
			set_header_key_string(&header, "CUNIT1", "pixel");
			set_header_key_double(&header, "CRPIX1", window.x0 + window.binx/2. - 0.5);
			set_header_key_double(&header, "CRVAL1", 1.0);
			set_header_key_double(&header, "CDELT1", window.binx);

			set_header_key_string(&header, "CTYPE2", "RAWY");
			set_header_key_string(&header, "CUNIT2", "pixel");
			set_header_key_double(&header, "CRPIX2", window.y0 + window.biny/2. - 0.5);
			set_header_key_double(&header, "CRVAL2", 1.0);
			set_header_key_double(&header, "CDELT1", window.biny);
		}
	}

	if (!code)
		code = unbin_quality_image(&qualityIn, &quality, &window);

	if (!code) {
		/* open the tracking file */
		if (fits_open_file(&task.fptr, p->trackfile, READONLY, &code)) {
			report_error("unable to open tracking file %s [%d]\n",
					p->trackfile, code);
		}
		else
			report_verbose("opened %s\n", p->trackfile);
	}

	if (!code) {
		task.quality = &quality;
		task.window = &window;
		task.map = &map;

		code = build_exposure_map(&task);

		if (code)
			report_error("unable to build exposure map\n");
		else
			report_verbose("exposure map built\n");
	}

	if (!code) {
		double scale = 0;
		if (p->frametime > 0)
			scale = p->frametime;
		else
			scale = p->exposure / task.nframes;

		code = bin_and_scale_exposure_map(&map, &mapOut, &window, scale);
	}

	if (!code) {
		io.history = p->history;
		io.header = &header;
		header.accept = 0;
		code = fimage_write(&mapOut, p->outfile, &io);
		if (code) {
			report_error("unable to write shift add map %s [%d]\n",
					p->outfile, code);
			code = WRITE_ERROR;
		}
	}

	if (task.fptr) {
		int tmp = 0;
		fits_close_file(task.fptr, &tmp);
	}

	iimage_release(&qualityIn);
	iimage_release(&quality);
	iimage_release(&map);
	fimage_release(&mapOut);

	return code;
}



static int get_parameters (Parameters * p)
{
	int code = 0;

	if (!code)
		code = PILGetFname(PAR_INFILE, p->infile);

	if (!code)
		code = PILGetFname(PAR_BADPIXFILE, p->badpixfile);

	if (!code)
		code = PILGetFname(PAR_TRACKFILE, p->trackfile);

	if (!code)
		code = PILGetFname(PAR_OUTFILE, p->outfile);

	if (!code)
		code = PILGetReal4(PAR_FRAMETIME, &p->frametime);

	if (!code)
		code = PILGetReal4(PAR_EXPOSURE, &p->exposure);

	p->chatter = headas_chatpar;
	p->clobber = headas_clobpar;
	get_history(&p->history);

	if (code)
		report_error("unable to load parameters\n");
	else
		report_status("parameters loaded\n");

	return code;
}


int TOOLSUB ()
{
	int code = 0;
	Parameters par = { 0 };

	add_report_function(&report_headas);

	if (!code)
		code = get_parameters(&par);

	if (!code)
		code = run_parameters(&par);

	remove_report_function(&report_headas);

	return code;
}

