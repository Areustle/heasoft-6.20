/*
 * $Source: /headas/headas/swift/uvot/tasks/uvottfc/uvottfc1.c,v $
 * $Revision: 1.3 $
 * $Date: 2006/04/06 15:09:18 $
 *
 * $Log: uvottfc1.c,v $
 * Revision 1.3  2006/04/06 15:09:18  rwiegand
 * Use larger primitive (int instead of short) since pixel counts are actually
 * 16 unsigned bits.
 *
 * Revision 1.2  2005/09/12 23:30:22  rwiegand
 * Null terminate list of keycard types to propagate.
 *
 * Revision 1.1  2005/03/04 19:03:59  rwiegand
 * Generate sparse image in C sub-process.
 *
 */


#include <stdio.h>	/* pil.h uses but does not include */

#include "headas.h"
#include "pil.h"
#include "genimage.h"
#include "report.h"
#include "keyutil.h"
#include "uvottool.h"
#include "uvotfile.h"
#include "uvotimage.h"
#include "uvotquality.h"


#define TOOLSUB uvottfc1
#define TOOLVER 1.0
#include "headas_main.c"

#define CREATOR _STRINGIFY(TOOLSUB) " " _STRINGIFY(TOOLVER)

#define EXTNAME "TDRSS_FC"


enum
{
	ITERATOR_RAWX,
	ITERATOR_RAWY,
	ITERATOR_HIGH,
	ITERATOR_MEDIUM,
	ITERATOR_LOW,
	ITERATOR_COLUMNS
};



typedef struct
{
	int chatter;
	int clobber;
	int history;

	char infile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];
	char badpixfile[PIL_LINESIZE];

} Parameters;



typedef struct
{
	const Parameters * par;

	double met;
	IImage * sparse;
	SImage * quality;

	int xmin, xmax, ymin, ymax;

	int null;
	int null_high;
	int null_medium;
	int null_low;

} Task;


#define NMEDIUM   4
#define NLOW     16

typedef struct
{
	int rawx;
	int rawy;
	int high;
	int medium[NMEDIUM];
	int low[NLOW];

} PostageStamp;



/*
	pixels are numbered from lower left hand corner for consistency with
	FITS and UVOT definitions
*/


/* indices into XXX_DELTA arrays */
const int DELTA_Y = 0;
const int DELTA_X = 1;

/*
	[ deltaRow = deltaY, deltaCol = deltaX ]
	for pixels b c d e (relative to pixel a)
*/
const int MEDIUM_DELTA[][2] = {
	{ -1,  0 },
	{  0, -1 },
	{  0,  1 },
	{  1,  0 }
};


/*
	[ deltaRow = deltaY, deltaCol = deltaX ]
	for pixels f - u (relative to pixel a)
*/
const int LOW_DELTA[][2] = {
	{ -2, -1 }, /* f g h */
	{ -2,  0 },
	{ -2,  1 },
	{ -1, -2 }, /* i j */
	{ -1, -1 },
	{ -1,  1 }, /* k l */
	{ -1,  2 },
	{  0, -2 }, /* m */
	{  0,  2 }, /* n */
	{  1, -2 }, /* o p */
	{  1, -1 },
	{  1,  1 }, /* q r */
	{  1,  2 },
	{  2, -1 }, /* s t u */
	{  2,  0 },
	{  2,  1 }
};



static int
assimilate_pixel (Task * task, int rawx, int rawy, int counts)
{
	int code = 0;
	IImage * sparse = task->sparse;
	int was;

	if (task->null && counts == task->null)
		return code;

	if (rawx < task->xmin || rawx > task->xmax
			|| rawy < task->ymin || rawy > task->ymax)
		report_warning("ignoring bogus pixel at %d, %d\n", rawx, rawy);

	else if (simage_get_relative(task->quality, rawx, rawy))
		report_warning("ignoring bad pixel at %d, %d\n", rawx, rawy);

	else if (iimage_get(sparse, rawx, rawy, &was))
		code = 1;

	else if (was != sparse->null)
		/*
			if the same pixel is downlinked more than once,
			only the first value is valid
		*/
		; /* duplicate pixel */

	else if (iimage_set(sparse, rawx, rawy, counts))
		code = 1;

	return code;
}


static int
assimilate_postage_stamp (Task * task, const PostageStamp * stamp)
{
	int code = 0;
	int i;

	task->null = task->null_high;
	if (!code)
		code = assimilate_pixel(task, stamp->rawx, stamp->rawy, stamp->high);

	task->null = task->null_medium;
	for (i = 0; !code && i < NMEDIUM; ++i) {
		int rawx = stamp->rawx + MEDIUM_DELTA[i][DELTA_X];
		int rawy = stamp->rawy + MEDIUM_DELTA[i][DELTA_Y];
		code = assimilate_pixel(task, rawx, rawy, stamp->medium[i]);
	}

	task->null = task->null_low;
	for (i = 0; !code && i < NLOW; ++i) {
		int rawx = stamp->rawx + LOW_DELTA[i][DELTA_X];
		int rawy = stamp->rawy + LOW_DELTA[i][DELTA_Y];
		code = assimilate_pixel(task, rawx, rawy, stamp->low[i]);
	}

	return code;
}



static int
iterate_postage_stamps_aux (long totaln, long offset, long firstn,
						long nvalues, int narrays, iteratorCol * icols,
						void * user)
{
	int code = 0;
	Task * task = (Task *) user;
	int i, j;

	int * prawx;
	int * prawy;
	int * phigh;
	int * pmedium;
	int * plow;

	report_verbose("iterate_postage_stamps_aux(totaln %ld, offset %ld, firstn %ld, nvalues %ld, narrays %d, ...)\n", totaln, offset, firstn, nvalues, narrays);

	prawx   = (int *) fits_iter_get_array(icols + ITERATOR_RAWX);
	prawy   = (int *) fits_iter_get_array(icols + ITERATOR_RAWY);
	phigh   = (int *) fits_iter_get_array(icols + ITERATOR_HIGH);
	pmedium = (int *) fits_iter_get_array(icols + ITERATOR_MEDIUM);
	plow    = (int *) fits_iter_get_array(icols + ITERATOR_LOW);

	/* 0th value indicates null */
	task->null_high = phigh[0];
	task->null_medium = pmedium[0];
	task->null_low = plow[0];

	++prawx;
	++prawy;
	++phigh;
	++pmedium;
	++plow;

	for (i = 0; !code && i < nvalues; ++i)
		{
			PostageStamp stamp = { 0 };

			stamp.rawx = prawx[i];
			stamp.rawy = prawy[i];
			stamp.high = phigh[i];

			for (j = 0; j < NMEDIUM; ++j)
				stamp.medium[j] = pmedium[NMEDIUM * i + j];

			for (j = 0; j < NLOW; ++j)
				stamp.low[j] = plow[NLOW * i + j];

			code = assimilate_postage_stamp(task, &stamp);
		}

	return code;
}


static void
initialize_iterator_column (iteratorCol * icol, char * name, fitsfile * fptr,
							int datatype, int coltype, int * pstatus)
{
	int tmp = fits_iter_set_by_name(icol, fptr, name, datatype, coltype);

	if (tmp)
		{
			if (!*pstatus)
				*pstatus = tmp;
			report_error("unable to initialize iterator column '%s' [%d]\n",
							name, tmp);
		}
}



static int
iterate_postage_stamps (Task * task, fitsfile * fptr)
{
	int code = 0;
	int *pstatus = &code;

	iteratorCol icols[ITERATOR_COLUMNS] = { { 0 } };

	initialize_iterator_column(&icols[ITERATOR_RAWX], "RAWX",
						fptr, TINT, InputCol, pstatus);
	initialize_iterator_column(&icols[ITERATOR_RAWY], "RAWY",
						fptr, TINT, InputCol, pstatus);
	initialize_iterator_column(&icols[ITERATOR_HIGH], "HIGH",
						fptr, TINT, InputCol, pstatus);
	initialize_iterator_column(&icols[ITERATOR_MEDIUM], "MEDIUM",
						fptr, TINT, InputCol, pstatus);
	initialize_iterator_column(&icols[ITERATOR_LOW], "LOW",
						fptr, TINT, InputCol, pstatus);

	if (code)
		report_error("unable to initialize iteration [%d]\n", code);

	else
		fits_iterate_data(ITERATOR_COLUMNS, icols, 0, 0,
						iterate_postage_stamps_aux, task, pstatus);

	if (code)
		report_error("postage stamp iteration failed [%d]\n", code);

	return code;
}



static int
load_bad_pixels (SImage * quality, const char * path, double met)
{
	int code = 0;
	BadPixelList badpixels = { 0 };

	quality->binx = 1;
	quality->biny = 1;

	if (!code)
		code = simage_allocate(quality, UVOT_RAW_DIMX, UVOT_RAW_DIMY);

	if (!code)
		code = simage_set_constant(quality, 0);

	if (!code)
		code = load_bad_pixel_list(&badpixels, path);

	if (!code)
		code = apply_bad_pixel_list(&badpixels, quality, met);

	release_bad_pixel_list(&badpixels);

	return code;
}


int
run_parameters (Parameters * p)
{
	int code = 0;
	Task task = { 0 };
	RawWindow window = { 0 };
	int * pstatus = &code;
	fitsfile * ifptr = 0;
	fitsfile * ofptr = 0;
	IImage sparse = { 0 };
	SImage quality = { 0 };
	FITSHeader header = { 0 };
	FileSpec spec = { 0 };

	sparse.null = -1;
	sparse.warnings = 10;
	sparse.offset = 1;

	quality.null = -1;
	quality.warnings = 10;

	task.par = p;
	task.sparse = &sparse;
	task.quality = &quality;

	if (!code && p->clobber)
		code = headas_clobberfile(p->outfile);

	if (!code && fits_open_file(&ifptr, p->infile, READONLY, pstatus))
		report_error("unable to open %s [%d]\n", p->infile, code);

	if (!code && fits_create_file(&ofptr, p->outfile, pstatus))
		report_error("unable to create %s [%d]\n", p->outfile, code);

	if (!code && file_create_primary(ifptr, ofptr, &spec))
		report_error("unable to initialize %s primary HDU\n", p->outfile);

	if (!code && fits_movnam_hdu(ifptr, BINARY_TBL, EXTNAME, 0, pstatus))
		report_error("unable to move to %s extension [%d]\n", EXTNAME, code);

	if (!code)
		{
			int types[] = { TYP_REFSYS_KEY, TYP_USER_KEY, 0 };
			header.accept = accept_keycard_types;
			header.user = types;
			fetch_header_records_fits(&header, ifptr);
			header.accept = 0;
			set_header_key_string(&header, "CREATOR", CREATOR);
		}

	if (!code)
		{
			int bin = 1;
			header.warnings = 1;
			if (!code)
				code = get_header_key_double(&header, "TSTOP", &task.met, 0);

			if (!code)
				code = get_header_key_integer(&header, "WINDOWX0", &window.x0, 0);
			if (!code)
				code = get_header_key_integer(&header, "WINDOWY0", &window.y0, 0);
			if (!code)
				code = get_header_key_integer(&header, "WINDOWDX", &window.dx, 0);
			if (!code)
				code = get_header_key_integer(&header, "WINDOWDY", &window.dy, 0);
			header.warnings = 0;
			header.get_or_set = 1;
			if (!code)
				code = get_header_key_integer(&header, "BINX", &window.binx, &bin);
			if (!code)
				code = get_header_key_integer(&header, "BINY", &window.biny, &bin);
			if (code)
				report_error("unable to determine raw window\n");
		}

	if (!code)
		code = load_bad_pixels(&quality, p->badpixfile, task.met);

	if (!code)
		{
			task.xmin = window.x0;
			task.xmax = window.x0 + window.dx - 1;
			task.ymin = window.y0;
			task.ymax = window.y0 + window.dy - 1;

			sparse.x0 = window.x0;
			sparse.y0 = window.y0;
		}

	if (!code)
		code = iimage_allocate(&sparse, window.dx, window.dy);

	if (!code)
		code = iimage_set_constant(&sparse, sparse.null);

	if (!code)
		code = iterate_postage_stamps(&task, ifptr);

	if (!code)
		{
			ImageIO io = { 0 };

			set_header_key_string(&header, "CTYPE1", "RAWX");
			set_header_key_string(&header, "CTYPE2", "RAWY");
			set_header_key_double(&header, "CRVAL1", window.x0);
			set_header_key_double(&header, "CRVAL2", window.y0);
			set_header_key_double(&header, "CRPIX1", 1);
			set_header_key_double(&header, "CRPIX2", 1);
			set_header_key_double(&header, "CDELT1", 1);
			set_header_key_double(&header, "CDELT2", 1);
			set_header_key_string(&header, "CUNIT1", "pixel");
			set_header_key_string(&header, "CUNIT2", "pixel");

			io.header = &header;

			if (p->history)
				io.history = 1;
			io.checksum = 1;
			io.compress = 1;
			code = iimage_append(&sparse, ofptr, &io);
		}

	if (ifptr)
		{
			int tmp = 0;
			fits_close_file(ifptr, &tmp);
		}

	if (ofptr)
		{
			int tmp = 0;
			if (code)
				fits_delete_file(ofptr, &tmp);
			else if (fits_close_file(ofptr, pstatus))
				report_error("unable to close %s [%d]\n", p->outfile, code);
		}

	iimage_release(&sparse);
	simage_release(&quality);
	release_header(&header);

	return code;
}


int
get_parameters (Parameters * p)
{
	int code = 0;
	int pill = 0;

	if (!pill)
		pill = PILGetFname("infile", p->infile);

	if (!pill)
		pill = PILGetFname("outfile", p->outfile);

	if (!pill)
		pill = PILGetFname("badpixlist", p->badpixfile);

	p->clobber = headas_clobpar;
	p->chatter = headas_chatpar;
	get_history(&p->history);

	if (pill)
		{
			code = TASK_SETUP_ERROR;
			report_error("unable to load parameters\n");
		}
	else
		report_status("parameters loaded\n");

	return code;
}



int
uvottfc1 ()
{
	int code = 0;
	Parameters p = { 0 };

	set_toolname(_STRINGIFY(TOOLSUB));
	set_toolversion(_STRINGIFY(TOOLVER));

	add_report_function(&report_headas);

	if (!code)
		code = get_parameters(&p);

	if (!code)
		code = run_parameters(&p);

	remove_report_function(&report_headas);

	return code;
}

