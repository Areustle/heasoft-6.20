#ifndef UVOTFLAGQUAL_H
#define UVOTFLAGQUAL_H

/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotflagqual/flagqual.h,v $
 * $Revision: 1.8 $
 * $Date: 2010/06/21 20:27:12 $
 *
 * $Log: flagqual.h,v $
 * Revision 1.8  2010/06/21 20:27:12  rwiegand
 * Updated with new version from Vladimir: Source detection algorithm is improved
 * (splitting overlapping sources).
 *
 * Revision 1.7  2010/04/28 21:51:06  rwiegand
 * Vladimir provided a new version which operates on each extension and updates
 * quality flagging (halo rings, filter dependence).
 *
 * Revision 1.3  2008/06/12 18:12:14  wiegand
 * Added flags to indicate point and extended sources.  Use SExtractor
 * CXX, CYY, CXY ellipse definition.  Process raw image using floating point.
 *
 * Revision 1.2  2008/06/11 14:26:50  wiegand
 * Implemented quality flag map.
 *
 * Revision 1.1  2008/06/10 20:24:37  wiegand
 * Initial revision
 *
 */

#include "genimage.h"


typedef enum
{
	FLAG_BAD_PIXEL,                  /* 1 */
	FLAG_READOUT_STREAK,             /* 2 */
	FLAG_SMOKE_RING,                 /* 4 */
	FLAG_DIFFRACTION_SPIKE,          /* 8 */
	FLAG_STRONG_MOD8,                /* 16 */
	FLAG_CENTRAL_REGION,             /* 32 */
	FLAG_NEAR_BRIGHT_SOURCE,         /* 64 */
	FLAG_NEAR_IMAGE_EDGE,            /* 128 */
	FLAG_WITHIN_EXTENDED_SOURCE,     /* 256 */
	FLAG_WEIRD_SOURCE,               /* 512 */
	FLAG_CONFUSION_0,     /* another source within 0.5 aperture radius */
	FLAG_CONFUSION_1,     /* another source within 0.5 - 1.0 aperture radius */
	FLAG_POINT_SOURCE,
	FLAG_EXTENDED_SOURCE,
	FLAG_TERMINATOR
} QualityFlags;


typedef enum
{
	FILTER_UNKNOWN,
	FILTER_U,
	FILTER_B,
	FILTER_V,
	FILTER_WHITE,
	FILTER_UVW1,
	FILTER_UVW2,
	FILTER_UVM2
} FilterCode;


typedef struct
{
	double psf_fwhm_arcsec;
	double psf_fwhm_imagePixels;
	double aperture_imagePixels;
	double aperture_arcsec;
	double arcsec_per_imagePixel;

} Constants;


typedef struct
{
	double xbar, ybar, cxx, cyy, cxy;
	double rr;
} Ellipse;


typedef struct
{
	int id;

	double xImage, yImage;
	double xRaw, yRaw;
	double majorAxis_imagePixels, minorAxis_imagePixels, thetaAxis_degrees;
	Ellipse ellipse_imagePixels;
	int isPointSource;

	double rawRate, rawCounts;
	double backgroundCounts;

	int flags;
} Source;


/*------------------------------------------------------------*/

typedef struct
{
	double xImage;
	double yImage;
	int quality;
} ImageBadPixel;


typedef struct Parameters Parameters;


typedef struct
{
	const Parameters * par;
	const Constants * constants;

	RawWindow * window;

	FImage * rawImage;
	IImage * flagImage;
	IImage * qmapImage; /* Additional quality map for strong scattered light features */
	/*QualityImage * flagImage;*/
	/*--------- source mask output image -------------------------*/
	FImage * smapImage;
	/*--------- readout streaks image -------------------------*/
	FImage * vstreakImage;
	/*------------------------------------------------------------*/
	/*--------- extended source image -------------------------*/
	FImage * extendedImage;
	/*------------------------------------------------------------*/
	int flaggedPixels[FLAG_TERMINATOR];
	int flaggedSources[FLAG_TERMINATOR];

	int nBadPixels;
	ImageBadPixel * badPixel;
	BadPixelList * badPixList;

	int nSources;
	Source * source;

	/*-----------bright source list ------------------------------*/
	/*
	 int nBrightSources;
	 Source * brightSource;
	 */
	/*------------------------------------------------------------*/
	double exposure;

	int x0, y0, dx, dy, binx, biny;
	int smoothSize;

	fitsfile * fptr;
	fitsfile * ofptr;
	fitsfile * smapfptr;
	fitsfile * qmapfptr;

	int reportDuplicates;
	int flagDetections;

	FilterCode filter;
	char filterString[FLEN_CARD];

} Task;


void convert_raw_to_image(Task * task, double rawx, double rawy,
		double * imagex, double * imagey);

int flagAllQuality(Task * task);

int checkBrightSources(Task * task, int hdu);

const char * flagString(int flag);


#endif /* UVOTFLAGQUAL_H */

