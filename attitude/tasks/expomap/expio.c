/*
 * $Source: /headas/headas/attitude/tasks/expomap/expio.c,v $
 * $Revision: 1.6 $
 * $Date: 2003/11/26 21:02:21 $
 *
 *	Perform input and output for exposure map generation
 *
 * $Log: expio.c,v $
 * Revision 1.6  2003/11/26 21:02:21  rwiegand
 * Updated parameter stamping call.
 *
 * Revision 1.5  2003/09/04 17:36:07  rwiegand
 * Allow creating exposure maps in detector coordinates.
 *
 * Revision 1.4  2003/07/28 21:13:20  rwiegand
 * Added history and checksum flags.
 *
 * Revision 1.3  2003/07/18 21:14:38  rwiegand
 * Pass in binning parameters instead of trying to determine from WCS keywords
 * and TELDEF information.
 *
 * Revision 1.2  2003/07/18 20:11:21  rwiegand
 * Relocated to headas/attitude package.  Use coord/image.c support for
 * windowing/binning based on WCS keywords.
 *
 * Revision 1.1  2003/05/14 18:14:44  rwiegand
 * Tool for creating exposure maps from instrument map, good time intervals,
 * and attitude information.
 *
 * Revision 1.2  2003/05/14 13:35:29  rwiegand
 * Transfer keywords from input to output.
 *
 * Revision 1.1  2003/05/12 14:16:33  rwiegand
 * Generic exposure map generator.
 * Given an instrument map, good time interval(s) and attitude information,
 * creates an exposure map.
 *
 * Revision 1.1  2003/05/02 18:24:17  rwiegand
 * Exposure map generator
 *
 */

#include <string.h>

#include "expomap.h"
#include "expio.h"
#include "fitsio.h"
#include "genimage.h"
#include "coordfits.h"
#include "xform2d.h"
#include "report.h"
#include "headas_utils.h"


/*
 * create a single GTI from the TSTART/TSTOP keywords
 */
static int
load_gtis_image (ExposureMapper * mapper, fitsfile * fptr)
{
	int code = 0;
	double start, stop;
	int status = 0;

	mapper->gtis = 1;

	if (!code)
		{
			char comment[FLEN_COMMENT];

			fits_read_key(fptr, TDOUBLE, "TSTART", &start, comment, &status);
			fits_read_key(fptr, TDOUBLE, "TSTOP", &stop, comment, &status);

			if (status)
				{
					code = EXPOMAP_INPUT_ERROR;
					report_error("unable to read image TSTART/TSTOP keywords\n");
				}
		}

	if (!code)
		{
			GTI * pgti = 0;
			pgti = calloc(1, sizeof(GTI));
			pgti[0].start = start;
			pgti[0].stop = stop;
			mapper->interval = pgti;
		}

	return code;
}


static int
load_gtis_table (ExposureMapper * mapper, fitsfile * fptr)
{
	int code = 0;

	int status = 0;
	long count = -1;
	int startcol = 0;
	int stopcol = 0;
	GTI * interval = 0;
	double * raw = 0;

	if (!code)
		{
			fits_get_num_rows(fptr, &count, &status);

			if (status)
				{
					code = EXPOMAP_INPUT_ERROR;
					report_error("unable to determine GTI rows\n");
				}
			else
				report_verbose("expecting %d GTI rows\n", count);

			mapper->gtis = count;
		}

	if (!code)
		{
			const int ignoreCase = 1;

			fits_get_colnum(fptr, ignoreCase, "START", &startcol, &status);
			fits_get_colnum(fptr, ignoreCase, "STOP", &stopcol, &status);

			if (status)
				{
					code = EXPOMAP_INPUT_ERROR;
					report_error("unable to determine GTI START/STOP columns\n");
				}
			else
				report_verbose("START column %d, STOP column %d\n",
												startcol, stopcol);
		}

	if (!code)
		{
			interval = calloc(count, sizeof(GTI));
			if (!interval)
				{
					code = EXPOMAP_MEMORY_ERROR;
					report_error("unable to allocate space for GTI structs\n");
				}
		}

	if (!code)
		{
			raw = (double *) malloc(count * sizeof(double));
			if (!raw)
				{
					code = EXPOMAP_MEMORY_ERROR;
					report_error("unable to allocate space for raw GTI column\n");
				}
		}

	if (!code)
		{
			double null = 0;
			int nulls = 0;
			fits_read_col(fptr, TDOUBLE, startcol, 1, 1, count,
											&null, raw, &nulls, &status);
			if (status)
				{
					code = EXPOMAP_INPUT_ERROR;
					report_error("unable to read GTI START column\n");
				}
			else
				{
					int i;
					for (i = 0; i < count; ++i)
						interval[i].start = raw[i];
				}
		}

	if (!code)
		{
			double null = 0;
			int nulls = 0;
			fits_read_col(fptr, TDOUBLE, stopcol, 1, 1, count,
											&null, raw, &nulls, &status);
			if (status)
				{
					code = EXPOMAP_INPUT_ERROR;
					report_error("unable to read GTI STOP column\n");
				}
			else
				{
					int i;
					for (i = 0; i < count; ++i)
						interval[i].stop = raw[i];
				}
		}

	if (!code)
		{
			mapper->interval = interval;
			report_status("loaded %d GTI rows\n", count);
		}

	if (raw)
		free(raw);

	return code;
}


int
load_good_time_intervals (ExposureMapper * mapper, const char * path)
{
	int code = 0;
	fitsfile * fptr = 0;
	int status = 0;
	int hduType = -1;

	if (!code)
		{
			fits_open_file(&fptr, path, READONLY, &status);
			if (status)
				{
					code = EXPOMAP_INPUT_ERROR;
					report_error("unable to open %s [%d]\n", path, status);
				}
			else
				report_verbose("opened good time intervals file %s\n", path);
		}

	if (!code)
		{
			/* determine input type */
			fits_get_hdu_type(fptr, &hduType, &status);

			if (status)
				{
					code = EXPOMAP_INPUT_ERROR;
					report_error("unable to determine GTI HDU type\n");
				}
			else
				{
					const char * typeString = "UNKNOWN";

					if (hduType == ASCII_TBL)
						typeString = "ASCII_TBL";
					else if (hduType == BINARY_TBL)
						typeString = "BINARY_TBL";
					else if (hduType == IMAGE_HDU)
						typeString = "IMAGE_HDU";
					else
						typeString = "EVIL";

					report_verbose("GTI HDU is %s\n", typeString);
				}
		}

	if (!code)
		{
			if (hduType == IMAGE_HDU)
				code = load_gtis_image(mapper, fptr);
			else
				code = load_gtis_table(mapper, fptr);
		}

	if (fptr)
		{
			int tmp = 0;
			fits_close_file(fptr, &tmp);
			if (tmp)
				{
					code = EXPOMAP_INPUT_ERROR;
					report_error("unable to close GTI HDU\n");
				}
		}

	return code;
}


static int
accept_nonstructural_nonwcs_records (const HeaderRecord * record, void * user)
{
	int accept = 0;
	HeaderRecord * h = (HeaderRecord *) record;
	int class = fits_get_keyclass(h->card);
	if (class > TYP_CMPRS_KEY && class != TYP_WCS_KEY)
		accept = 1;
	return accept;
}


int
load_instrument_map (ExposureMapper * mapper, const char * path)
{
	int code = 0;
	ImageIO io = { 0 };
	io.header = mapper->header;
	io.header->accept = accept_nonstructural_records;

	code = fimage_read(mapper->instrument, path, &io);
	io.header->accept = 0;
	if (code)
		{
			code = EXPOMAP_INPUT_ERROR;
			report_error("unable to read instrument map [%d]\n", code);
		}

	return code;
}


int
load_telescope_definition (ExposureMapper * mapper, const char * path)
{
	int code = 0;
	TELDEF * teldef;
	char buffer[1024];

	strcpy(buffer, path);
	teldef = readTelDef(buffer);

	if (!teldef)
		{
			code = EXPOMAP_INPUT_ERROR;
			report_error("unable to load teldef file %s\n", path);
		}
	else
		mapper->teldef = teldef;

	return code;
}


int
initialize_attitude (ExposureMapper * mapper, const char * path,
				double attdelta)
{
	int code = 0;
	ATTFILE * attfile;
	char buffer[1024];

	strcpy(buffer, path);

	attfile = openAttFile(buffer);
	if (!attfile)
		{
			code = EXPOMAP_INPUT_ERROR;
			report_error("unable to open attitude file %s\n", path);
		}
	else
			mapper->attfile = attfile;

	if (!code)
		{
			ATTITERATOR * attiter = allocateAttIterator(attfile, attdelta);

			if (!attiter)
				{
					code = EXPOMAP_SETUP_ERROR;
					report_error("unable to initialize attitude iterator\n");
				}
			else
				mapper->attiter = attiter;
		}

	return code;
}


int
write_exposure_map (ExposureMapper * mapper, const char * path)
{
	int code = 0;

	int status = 0;
	fitsfile * fptr = 0;

	fits_create_file(&fptr, path, &status);
	if (status)
		{
			code = EXPOMAP_OUTPUT_ERROR;
			report_error("unable to create exposure map %s\n", path);
		}

	if (!code)
		{
			ImageIO io = { 0 };
			io.header = mapper->header;
			io.header->accept = accept_nonstructural_nonwcs_records;
			image_set_wcs_simple(io.header, mapper->exposure->wcs);
			code = fimage_append(mapper->exposure, fptr, &io);

		}

	if (!code && mapper->history)
		HDpar_stamp(fptr, 0, &status);

	if (!code && mapper->checksum)
		fits_write_chksum(fptr, &status);

	if (fptr)
		{
			int tmp = 0;
			if (code)
				fits_delete_file(fptr, &tmp);
			else
				fits_close_file(fptr, &tmp);

			if (tmp)
				{
					code = EXPOMAP_OUTPUT_ERROR;
					report_error("error writing exposure map\n");
				}
		}

	if (!code)
		report_status("wrote exposure map to %s\n", path);

	return code;
}

