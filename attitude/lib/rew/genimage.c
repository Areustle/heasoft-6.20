/*
 * $Source: /headas/headas/attitude/lib/rew/genimage.c,v $
 * $Revision: 1.5 $
 * $Date: 2008/07/02 15:32:40 $
 *
 *
 * $Log: genimage.c,v $
 * Revision 1.5  2008/07/02 15:32:40  rwiegand
 * Corrected indices in PCi_ja keywords.
 *
 * Revision 1.4  2008/03/07 21:29:08  rwiegand
 * Updated default handling of header keywords.
 *
 * Revision 1.3  2003/07/28 21:12:20  rwiegand
 * More keyword support.
 *
 * Revision 1.2  2003/07/25 20:02:15  rwiegand
 * Updates for WCS compliance.
 *
 * Revision 1.1  2003/05/14 13:41:39  rwiegand
 * Support for logging and reading/writing FITS images and keywords.
 *
 */


#include <stdlib.h>
#include <string.h>

#include "headas_utils.h" /* headas_parstamp */
#include "report.h"
#define _IMAGE_DEFINITIONS
#include "genimage.h"



int
image_fits_datatype (const char * name)
{
	int datatype = 0;

	if (!strcmp(name, "FImage"))
		datatype = TFLOAT;
	else if (!strcmp(name, "DImage"))
		datatype = TDOUBLE;
	else if (!strcmp(name, "IImage"))
		datatype = TINT;
	else if (!strcmp(name, "SImage"))
		datatype = TSHORT;
	else if (!strcmp(name, "BImage"))
		datatype = TBYTE;
	else
		report_warning("unknown type string %s\n", name);

	return datatype;
}


int
image_fits_bitpix (const char * name)
{
	int bitpix = 0;

	if (!strcmp(name, "FImage"))
		bitpix = FLOAT_IMG;
	else if (!strcmp(name, "DImage"))
		bitpix = DOUBLE_IMG;
	else if (!strcmp(name, "IImage"))
		bitpix = LONG_IMG;
	else if (!strcmp(name, "SImage"))
		bitpix = SHORT_IMG;
	else if (!strcmp(name, "BImage"))
		bitpix = BYTE_IMG;
	else
		report_warning("unknown type string %s\n", name);

	return bitpix;
}


int
image_wcs_to_pix (ImageWCS * wcs, double xwcs, double ywcs,
		double * xpix, double * ypix)
{
	int code = 0;
	fits_world_to_pix(xwcs, ywcs, wcs->xval, wcs->yval,
			wcs->xpix, wcs->ypix, wcs->xinc, wcs->yinc,
			wcs->rot, wcs->type, xpix, ypix, &code);
	return code;
}


int
image_pix_to_wcs (ImageWCS * wcs, double xpix, double ypix,
		double * xwcs, double * ywcs)
{
	int code = 0;
	fits_pix_to_world(xpix, ypix, wcs->xval, wcs->yval,
			wcs->xpix, wcs->ypix, wcs->xinc, wcs->yinc,
			wcs->rot, wcs->type, xwcs, ywcs, &code);
	return code;
}


int
image_set_wcs (FITSHeader * header, int axis, char suffix,
								const char * type, double pix, double val, double inc,
								const char * unit)
{
	int code = 0;

	char ctype[16];
	char crpix[16];
	char crval[16];
	char cdelt[16];
	char cunit[16];

	sprintf(ctype, "CTYPE%d%c", axis, suffix);
	sprintf(crpix, "CRPIX%d%c", axis, suffix);
	sprintf(crval, "CRVAL%d%c", axis, suffix);
	sprintf(cdelt, "CDELT%d%c", axis, suffix);
	sprintf(cunit, "CUNIT%d%c", axis, suffix);

	if (!code)
		code = set_header_key_string(header, ctype, type);

	if (!code)
		code = set_header_key_double(header, crpix, pix);

	if (!code)
		code = set_header_key_double(header, crval, val);

	if (!code)
		code = set_header_key_double(header, cdelt, inc);

	if (!code)
		code = set_header_key_string(header, cunit, unit);

	return code;
}


int
image_set_wcs_simple (FITSHeader * header, ImageWCS * wcs)
{
	int code = 0;

	char xtype[16];
	char ytype[16];

	strcpy(xtype, wcs->xnam);
	if (wcs->type)
		strcat(xtype, wcs->type);

	strcpy(ytype, wcs->ynam);
	if (wcs->type)
		strcat(ytype, wcs->type);

	if (!code)
		code = image_set_wcs(header, 1, 0, xtype,
										wcs->xpix, wcs->xval, wcs->xinc, wcs->unit);

	if (!code)
		code = image_set_wcs(header, 2, 0, ytype,
										wcs->ypix, wcs->yval, wcs->yinc, wcs->unit);

	return code;
}

int
image_get_wcsa (const FITSHeader * header, ImageWCSa * wcsa,
							int axis, char suffix)
{
	int code = 0;

	char crpix[8];
	char crval[8];
	char cdelt[8];
	char ctype[8];
	char cunit[8];
	double crpix0 = 0.0;
	double crval0 = 0.0;
	double cdelt0 = 1.0;

	sprintf(crpix, "CRPIX%d%c", axis, suffix);
	sprintf(crval, "CRVAL%d%c", axis, suffix);
	sprintf(cdelt, "CDELT%d%c", axis, suffix);
	sprintf(ctype, "CTYPE%d%c", axis, suffix);
	sprintf(cunit, "CUNIT%d%c", axis, suffix);

	if (!code)
		code = get_header_key_double(header, crpix, &wcsa->crpix, &crpix0);

	if (!code)
		code = get_header_key_double(header, crval, &wcsa->crval, &crval0);

	if (!code)
		code = get_header_key_double(header, cdelt, &wcsa->cdelt, &cdelt0);

	if (!code)
		code = get_header_key_string(header, ctype, wcsa->ctype, "");

	if (!code)
		get_header_key_string(header, cunit, wcsa->cunit, "");

	if (!code && wcsa->pcij)
		{
			int i, j;
			int any = 0;
			for (i = 0; i < 2; ++i)
				for (j = 0; j < 2; ++j)
					{
						char pcij[8];
						double tmp = i == j ? 1 : 0;
						double * dest = wcsa->pcij + 2 * i + j;
						sprintf(pcij, "PC%d_%d%c", i+1, j+1, suffix);
						if (!get_header_key_double(header, pcij, dest, &tmp))
							any = 1;
					}
		}

	return code;
}

