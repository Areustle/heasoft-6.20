#ifndef GENIMAGE_H
#define GENIMAGE_H

/*
 * $Source: /headas/headas/attitude/lib/rew/genimage.h,v $
 * $Revision: 1.6 $
 * $Date: 2005/03/04 19:32:32 $
 *
 * 	Two dimensional FITS image interface.
 *
 *
 * $Log: genimage.h,v $
 * Revision 1.6  2005/03/04 19:32:32  rwiegand
 * Write integer image BLANK keyword automatically.
 *
 * Revision 1.5  2003/09/04 17:41:21  rwiegand
 * Added polygon utilities.  When iterating over images, allow specifying a
 * region (less than the whole image), or the step size (other than every pixel).
 * Allow resizing a FITSHeader memory buffer.
 *
 * Revision 1.4  2003/07/28 21:12:20  rwiegand
 * More keyword support.
 *
 * Revision 1.3  2003/07/25 20:02:15  rwiegand
 * Updates for WCS compliance.
 *
 * Revision 1.2  2003/05/23 02:27:10  rwiegand
 * Support reading image from current HDU.
 *
 * Revision 1.1  2003/05/14 13:41:39  rwiegand
 * Support for logging and reading/writing FITS images and keywords.
 *
 */


#include "fitsio.h"
#include "keyutil.h"


#define IMAGE_INPUT_ERROR 10000


enum
{
	IMAGE_ERROR = 1,
	IMAGE_X_LOW,
	IMAGE_X_HIGH,
	IMAGE_Y_LOW,
	IMAGE_Y_HIGH,
	IMAGE_DUMMY
};


typedef struct
{
	int x, y;
	int x0, y0;
	void * user;
	int delta, dx, dy;
		/* iterations change by given increment */
	int region, xmin, xmax, ymin, ymax;
		/* iteration limited to given region */
} IIState;



typedef struct
{
	/* for CFITSIO WCS interface */
	double xpix;
	double ypix;
	double xval;
	double yval;
	double xinc;
	double yinc;

	char type[8]; /* -XYZ */
	double rot;

	char xnam[8];
	char ynam[8];
	char unit[8];

} ImageWCS;


typedef struct
{
	int axis;
	char suffix;

	double crpix;
	double crval;
	double cdelt;

	char ctype[16];
	char cunit[16];

	double * pcij;

} ImageWCSa;


typedef struct
{
	FITSHeader * header;

	int history;
	int checksum;
	int compress;

	const char * path;

	int nwcsa;
	ImageWCSa * wcsa;

} ImageIO;



int image_fits_datatype (const char * type);
int image_fits_bitpix (const char * type);

int image_fits_write_wcs (ImageWCS * wcs, fitsfile * fptr);

int image_wcs_to_pix (ImageWCS * wcs, double xwcs, double ywcs,
		double * xpix, double * ypix);
int image_pix_to_wcs (ImageWCS * wcs, double xpix, double ypix,
		double * xwcs, double * ywcs);

int image_set_wcs_simple (FITSHeader * header, ImageWCS * wcs);
int image_get_wcsa (const FITSHeader * header, ImageWCSa * wcsa,
		int axis, char suffix);

int image_combine_wcsa (ImageWCS * wcs, ImageWCSa * xwcs, ImageWCSa * ywcs);


#define _I_TOKEN0(a,b) a ## b
#define _I_TOKEN(a,b) _I_TOKEN0(a,b)

#define _i_prefix _I_TOKEN(_i_space, image_)
#define _i_func(name) _I_TOKEN(_i_prefix, name)
#define _i_iterator _I_TOKEN(_i_type,Iterator)

#define _I_STRINGIFY0(token) # token
#define _I_STRINGIFY(token) _I_STRINGIFY0(token)


#define _c_type float
#define _i_type FImage
#define _i_space f
#include "genimage.h0"
#undef _c_type
#undef _i_type
#undef _i_space

#define _c_type double
#define _i_type DImage
#define _i_space d
#include "genimage.h0"
#undef _c_type
#undef _i_type
#undef _i_space

#define _c_type int
#define _i_type IImage
#define _i_space i
#include "genimage.h0"
#undef _c_type
#undef _i_type
#undef _i_space

#define _c_type short
#define _i_type SImage
#define _i_space s
#include "genimage.h0"
#undef _c_type
#undef _i_type
#undef _i_space

typedef unsigned char byte;
#define _c_type byte
#define _i_type BImage
#define _i_space b
#include "genimage.h0"
#undef _c_type
#undef _i_type
#undef _i_space


#endif

