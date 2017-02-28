/*
 * $Source: /headas/headas/swift/uvot/lib/swuvot/uvotfile.c,v $
 * $Revision: 1.6 $
 * $Date: 2007/10/11 13:47:15 $
 *
 * $Log: uvotfile.c,v $
 * Revision 1.6  2007/10/11 13:47:15  rwiegand
 * Write real valued WCS keywords.
 *
 * Revision 1.5  2005/08/29 11:53:26  rwiegand
 * Added function to determine UVOT raw window from FITSHeader object.
 *
 * Revision 1.4  2005/07/18 12:41:58  rwiegand
 * Allow caller to require UVOT raw window to be loaded from WINDOW*
 * keywords.  Allow caller to store UVOT raw window to WCS keywords.
 *
 * Revision 1.3  2004/12/02 22:31:24  rwiegand
 * Corrected derivation of window corner from WCS.
 *
 * Revision 1.2  2004/12/02 22:12:52  rwiegand
 * Pass create_primary_header a FileSpec to take the basic header from.
 *
 * Revision 1.1  2004/12/02 21:29:45  rwiegand
 * A Swift/UVOT tool library.
 *
 */

#include <math.h>
#include <string.h>
#include <stdlib.h>

#include "uvotfile.h"
#include "report.h"
#include "keyutil.h"
#include "fitsio.h"


int file_parse_path (const char * string, FileSpec * spec)
{
	int status = 0;
	char tmp[FLEN_FILENAME];
	strcpy(tmp, string);

	fits_parse_input_url(tmp, spec->type, spec->base, spec->tmp,
				spec->ext, spec->filt, spec->bin, spec->col, &status);

	if (!status && spec->ext[0]) {
		char * end = 0;
		int hdu = strtol(spec->ext, &end, 10);
		if (end && *end)
			spec->hdu1 = -1;
		else
			spec->hdu1 = hdu + 1;
	}

	return status;
}



int file_resolve_hdu (FileSpec * spec, fitsfile * fptr)
{
	int status = 0;
	int hdu, type;

	if (spec->hdu1 > 0 || !spec->ext[0])
		return 0; /* already resolved */

	fits_get_hdu_num(fptr, &hdu);

	if (fits_movnam_hdu(fptr, ANY_HDU, spec->ext, 0, &status))
		report_warning("unable to move to HDU '%s' [%d]\n", spec->ext, status);
	else
		fits_get_hdu_num(fptr, &spec->hdu1);

	fits_movabs_hdu(fptr, hdu, &type, &status);

	return status;
}


static int roundint (double x)
{
	int i = floor(x + 0.5);
	return i;
}


int load_raw_window (fitsfile * fptr, RawWindow * window)
{
	int status = 0;
	long x0, y0, dx, dy, binx, biny;

	fits_read_key_lng(fptr, "WINDOWX0", &x0, 0, &status);
	fits_read_key_lng(fptr, "WINDOWY0", &y0, 0, &status);
	fits_read_key_lng(fptr, "WINDOWDX", &dx, 0, &status);
	fits_read_key_lng(fptr, "WINDOWDY", &dy, 0, &status);
	fits_read_key_lng(fptr, "BINX", &binx, 0, &status);
	fits_read_key_lng(fptr, "BINY", &biny, 0, &status);

	if (!status) {
		window->wcs = 0;

		window->x0 = roundint(x0);
		window->y0 = roundint(y0);
		window->dx = roundint(dx);
		window->dy = roundint(dy);

		window->binx = roundint(binx);
		window->biny = roundint(biny);
	}

	else if (window->ignoreWCS) {
		report_warning("image missing UVOT WINDOW* keywords and WCS not allowed\n");
	}

	else {
		/* fall back on WCS */
		char type[FLEN_VALUE];
		char ctype1[FLEN_VALUE];
		char ctype2[FLEN_VALUE];
		double crval1, crval2, crpix1, crpix2, cdelt1, cdelt2, rot;
		long naxis1, naxis2;
		double x0, y0;

		report_warning("unable to read window keywords, will try WCS\n");
		status = 0;
		fits_read_img_coord(fptr, &crval1, &crval2, &crpix1, &crpix2,
					&cdelt1, &cdelt2, &rot, type, &status);

		fits_read_key_str(fptr, "CTYPE1", ctype1, 0, &status);
		fits_read_key_str(fptr, "CTYPE2", ctype2, 0, &status);

		fits_read_key_lng(fptr, "NAXIS1", &naxis1, 0, &status);
		fits_read_key_lng(fptr, "NAXIS2", &naxis2, 0, &status);

		/* precalculate these, though they might not be used */
		x0 = (0.5 - crpix1) * cdelt1 + crval1 + 0.5;
		y0 = (0.5 - crpix2) * cdelt2 + crval2 + 0.5;

		if (status)
			/* already failed */
			report_warning("unable to read RAW image keywords\n");

		else if (strcmp(ctype1, "RAWX") || strcmp(ctype2, "RAWY")) {
			status = 1;
			report_warning("CTYPEn are not RAW [%s/%s]\n", ctype1, ctype2);
		}

		else if (fmod(cdelt1, 1) > 1e-9 || fmod(cdelt2, 1) > 1e-9) {
			status = 1;
			report_warning("CDELTn not integral [%e/%e]\n", cdelt1, cdelt2);
		}

		else if (fabs(fmod(x0, 1)) > 1e-9 || fabs(fmod(y0, 1)) > 1e-9) {
			status = 1;
			report_warning("WINDOW[XY]0 not integral [%e/%e]\n", x0, y0);
		}

		else if (rot != 0) {
			status = 1;
			report_warning("RAW rotation is non-zero [%e]\n", rot);
		}

		else {
			/* indicate the binning was derived from WCS */
			window->wcs = 1;

			window->x0 = roundint(x0);
			window->y0 = roundint(y0);
			window->binx = roundint(cdelt1);
			window->biny = roundint(cdelt2);
			window->dx = window->binx * naxis1;
			window->dy = window->biny * naxis2;
		}
	}

	if (status)
		report_warning("unable to determine RAW window\n");

	return status;
}


int parse_raw_window (const FITSHeader * header, RawWindow * window)
{
	int code = 0;
	long x0, y0, dx, dy, binx, biny;

	if (!code)
		code = get_header_key_long(header, "WINDOWX0", &x0, 0);
	if (!code)
		code = get_header_key_long(header, "WINDOWY0", &y0, 0);
	if (!code)
		code = get_header_key_long(header, "WINDOWDX", &dx, 0);
	if (!code)
		code = get_header_key_long(header, "WINDOWDY", &dy, 0);
	if (!code)
		code = get_header_key_long(header, "BINX", &binx, 0);
	if (!code)
		code = get_header_key_long(header, "BINY", &biny, 0);

	if (!code) {
		window->wcs = 0;

		window->x0 = roundint(x0);
		window->y0 = roundint(y0);
		window->dx = roundint(dx);
		window->dy = roundint(dy);

		window->binx = roundint(binx);
		window->biny = roundint(biny);
	}

	else if (window->ignoreWCS) {
		report_warning("image missing UVOT WINDOW* keywords and WCS not allowed\n");
	}

	else {
		/* fall back on WCS */
		char ctype1[FLEN_VALUE];
		char ctype2[FLEN_VALUE];
		double crval1, crval2, crpix1, crpix2, cdelt1, cdelt2;
		long naxis1, naxis2;
		double x0, y0;

		report_warning("unable to read window keywords, will try WCS\n");
		code = 0;
		ctype1[0] = ctype2[0] = 0;

		if (!code)
			code = get_header_key_string(header, "CTYPE1", ctype1, 0);
		if (!code)
			code = get_header_key_string(header, "CTYPE2", ctype2, 0);

		if (!code)
			code = get_header_key_double(header, "CRVAL1", &crval1, 0);
		if (!code)
			code = get_header_key_double(header, "CRVAL2", &crval2, 0);

		if (!code)
			code = get_header_key_double(header, "CRPIX1", &crpix1, 0);
		if (!code)
			code = get_header_key_double(header, "CRPIX2", &crpix2, 0);

		if (!code)
			code = get_header_key_double(header, "CDELT1", &cdelt1, 0);
		if (!code)
			code = get_header_key_double(header, "CDELT2", &cdelt2, 0);

		if (!code)
			code = get_header_key_long(header, "NAXIS1", &naxis1, 0);
		if (!code)
			code = get_header_key_long(header, "NAXIS2", &naxis2, 0);

		/* precalculate these, though they might not be used */
		x0 = (0.5 - crpix1) * cdelt1 + crval1 + 0.5;
		y0 = (0.5 - crpix2) * cdelt2 + crval2 + 0.5;

		if (code)
			/* already failed */
			report_warning("unable to read RAW image keywords\n");

		else if (strcmp(ctype1, "RAWX") || strcmp(ctype2, "RAWY")) {
			code = 1;
			report_warning("CTYPEn are not RAW [%s/%s]\n", ctype1, ctype2);
		}

		else if (fmod(cdelt1, 1) > 1e-9 || fmod(cdelt2, 1) > 1e-9) {
			code = 1;
			report_warning("CDELTn not integral [%e/%e]\n", cdelt1, cdelt2);
		}

		else if (fabs(fmod(x0, 1)) > 1e-9 || fabs(fmod(y0, 1)) > 1e-9) {
			code = 1;
			report_warning("WINDOW[XY]0 not integral [%e/%e]\n", x0, y0);
		}

		else {
			/* indicate the binning was derived from WCS */
			window->wcs = 1;

			window->x0 = roundint(x0);
			window->y0 = roundint(y0);
			window->binx = roundint(cdelt1);
			window->biny = roundint(cdelt2);
			window->dx = window->binx * naxis1;
			window->dy = window->biny * naxis2;
		}
	}

	if (code)
		report_warning("unable to determine RAW window\n");

	return code;
}


int save_raw_window (fitsfile * fptr, const RawWindow * window)
{
	int status = 0;

	fits_update_key_lng(fptr, "WINDOWX0", window->x0,
					"Lower left hand RAWX", &status);
	fits_update_key_lng(fptr, "WINDOWY0", window->y0,
					"Lower left hand RAWY", &status);
	fits_update_key_lng(fptr, "WINDOWDX", window->dx,
					"RAW window width", &status);
	fits_update_key_lng(fptr, "WINDOWDY", window->dy,
					"RAW window height", &status);

	fits_update_key_lng(fptr, "BINX", window->binx,
					"RAW window X binning", &status);
	fits_update_key_lng(fptr, "BINY", window->biny,
					"RAW window Y binning", &status);


	if (window->writeWCS) {

		fits_update_key_str(fptr, "CTYPE1", "RAWX",
						"WCS axis type", &status);
		fits_update_key_str(fptr, "CUNIT1", "pixel",
						"WCS axis unit", &status);
		fits_update_key_flt(fptr, "CRVAL1", window->x0 + window->binx/2. - 0.5,
						-3, "WCS reference value", &status);
		fits_update_key_flt(fptr, "CRPIX1", 1.0,
						-3, "WCS reference pixel", &status);
		fits_update_key_flt(fptr, "CDELT1", window->binx,
						-3, "WCS coordinate increment", &status);

		fits_update_key_str(fptr, "CTYPE2", "RAWY",
						"WCS axis type", &status);
		fits_update_key_str(fptr, "CUNIT2", "pixel",
						"WCS axis unit", &status);
		fits_update_key_flt(fptr, "CRVAL2", window->y0 + window->biny/2. - 0.5,
						-3, "WCS reference value", &status);
		fits_update_key_flt(fptr, "CRPIX2", 1.0,
						-3, "WCS reference pixel", &status);
		fits_update_key_flt(fptr, "CDELT2", window->biny,
						-3, "WCS coordinate increment", &status);
	}

	return status;
}


int file_create_primary (fitsfile * ifptr, fitsfile * ofptr, FileSpec * spec)
{
	int code = 0;
	int status = 0;
	int ihdu, hdu0, type;
	FITSHeader header = { 0 };
	int types[] = { TYP_REFSYS_KEY, TYP_USER_KEY, TYP_COMM_KEY, TYP_CONT_KEY };

	header.accept = accept_keycard_types;
	header.user = types;

	ihdu = 1;

	fits_get_hdu_num(ifptr, &hdu0);

	if (ihdu != 0 && hdu0 != ihdu) {
		if (fits_movabs_hdu(ifptr, ihdu, &type, &status)) {
			code = 1;
			report_error("unable to move to HDU %d [%d]\n", ihdu, status);
		}
	}

	if (!code)
		code = fetch_header_records_fits(&header, ifptr);

	if (!code)
		if (fits_create_img(ofptr, BYTE_IMG, 0, 0, &status)) {
			code = 1;
			report_warning("unable to create null primary image [%d]\n",
					status);
		}

	/* further filtering is possible here... */
	header.accept = 0;
	if (!code)
		code = update_header_records_fits(&header, ofptr);

	release_header(&header);

	if (ihdu != 0 && hdu0 != ihdu) {
		if (fits_movabs_hdu(ifptr, hdu0, &type, &status)) {
			code = 1;
			report_warning("unable to reposition input to HDU %d [%d]\n",
					hdu0, status);
		}
	}

	return code;
}


