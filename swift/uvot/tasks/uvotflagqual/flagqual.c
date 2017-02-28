/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotflagqual/flagqual.c,v $
 * $Revision: 1.10 $
 * $Date: 2010/06/21 20:27:12 $
 *
 * $Log: flagqual.c,v $
 * Revision 1.10  2010/06/21 20:27:12  rwiegand
 * Updated with new version from Vladimir: Source detection algorithm is improved
 * (splitting overlapping sources).
 *
 * Revision 1.9  2010/04/28 21:51:06  rwiegand
 * Vladimir provided a new version which operates on each extension and updates
 * quality flagging (halo rings, filter dependence).
 *
 * Revision 1.8  2010/04/05 21:13:18  rwiegand
 * Use enumerated (rather than integer) constants for filter codes.  Use array
 * of function pointers for flagging routines.
 *
 * Revision 1.7  2010/04/05 20:56:49  rwiegand
 * Purged commented out code.
 *
 * Revision 1.6  2010/04/05 20:20:55  rwiegand
 * Saving formatted version of Vladimir Yershov changes:
 * VNY 2009/11/22 Introduced the subroutine flagHaloRings, which should replace
 * the routine flagCentralEnhancementRegion (using the same bit 5 for flagging)
 * VNY 2009/12/21 The task is now detecting bright sources by itself and does
 * not require the input source list. So, the corresponding input parameter is
 * removed.
 * VNY 2010/03/21 (flagqual2.c) isolated single pixels are removed from the
 * source map used for source detection, which is needed for dealing with
 * short-exposures made with the UVW2-filter.
 *
 * Revision 1.5  2010/04/02 13:31:03  rwiegand
 * Implemented flagging of pixels in diffraction spikes.
 *
 * Revision 1.4  2009/08/26 21:08:02  irby
 * Remove apostrophes in "REQUIREMENTS" ifdef section so as not to choke
 * older compilers.
 *
 * Revision 1.3  2008/06/12 18:14:15  wiegand
 * Use SExtractor CXX, CYY, CXY ellipse definition.  Added point and extended
 * source flags.  When processing bad pixels, only flag the pixels that are
 * directly affected instead of all pixels associated with a source which
 * is affected by a bad pixel.  Changed raw image to floating point.
 *
 * Revision 1.2  2008/06/11 14:28:29  wiegand
 * Implemented quality flag map.
 *
 * Revision 1.1  2008/06/10 20:23:48  wiegand
 * Initial revision
 *
 */

#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "uvotfile.h"
#include "uvotquality.h"
#include "uvotimage.h"

#include "report.h"
#include "flagqual.h"


typedef int (*FlagRoutine)(Task *);


static int IMAX(int a, int b)
{
	if (a > b)
		return a;
	return b;
}

static int IMIN(int a, int b)
{
	if (a < b)
		return a;
	return b;
}

int apply_range(double v, int min, int max)
{
	int i = (int) v;

	if (i < min)
		return min;

	if (i > max)
		return max;

	return i;
}

static double DMAX(double a, double b)
{
	if (a > b)
		return a;
	return b;
}

static double DMIN(double a, double b)
{
	if (a < b)
		return a;
	return b;
}

static float FMAX(float a, float b)
{
	if (a > b)
		return a;
	return b;
}



/* see SExtractor v2.5 manual, section 9.1.6, equation 26 */

int point_in_ellipse(double x, double y, Ellipse * ellipse)
{
	int inside;
	double dx, dy;

	dx = x - ellipse->xbar;
	dy = y - ellipse->ybar;

	inside = ellipse->cxx * dx * dx
			+ ellipse->cyy * dy * dy
			+ ellipse->cxy * dx * dy
			< ellipse->rr;

	return inside;
}

void convert_raw_to_image(Task * task, double rawx, double rawy,
		double * imagex, double * imagey)
{

	*imagex = (rawx - task->x0 + 0.5) / task->binx + 0.5;
	*imagey = (rawy - task->y0 + 0.5) / task->biny + 0.5;
}

const char * flagString(int flag)
{
	static int initialized = 0;
	static const char * FLAG_STRING[FLAG_TERMINATOR];
	static char buffer[64];
	const char * s = 0;

	if (!initialized) {
		initialized = 1;
		FLAG_STRING[FLAG_BAD_PIXEL] = "BAD_PIXEL";
		FLAG_STRING[FLAG_READOUT_STREAK] = "READOUT_STREAK";
		FLAG_STRING[FLAG_SMOKE_RING] = "SMOKE_RING";
		FLAG_STRING[FLAG_DIFFRACTION_SPIKE] = "DIFFRACTION_SPIKE";
		FLAG_STRING[FLAG_STRONG_MOD8] = "STRONG_MOD8";
		FLAG_STRING[FLAG_CENTRAL_REGION] = "CENTRAL_REGION";
		FLAG_STRING[FLAG_NEAR_BRIGHT_SOURCE] = "NEAR_BRIGHT_SOURCE";
		FLAG_STRING[FLAG_NEAR_IMAGE_EDGE] = "NEAR_IMAGE_EDGE";
		FLAG_STRING[FLAG_WITHIN_EXTENDED_SOURCE] = "WITHIN_EXTENDED_SOURCE";
		FLAG_STRING[FLAG_WEIRD_SOURCE] = "WEIRD_SOURCE";
		FLAG_STRING[FLAG_CONFUSION_0] = "CONFUSION_0[<0.5ap]";
		FLAG_STRING[FLAG_CONFUSION_1] = "CONFUSION_1[0.5-1ap]";
		FLAG_STRING[FLAG_POINT_SOURCE] = "POINT_SOURCE";
		FLAG_STRING[FLAG_EXTENDED_SOURCE] = "EXTENDED_SOURCE";
	}

	if (flag >= 0 && flag < FLAG_TERMINATOR) {
		s = FLAG_STRING[flag];
		if (!s) {
			sprintf(buffer, "MISSING_STRING[%d]", flag);
			s = buffer;
			report_warning("flagString: flag %d not handled\n", flag);
		}
	}
	else {
		sprintf(buffer, "UNEXPECTED_FLAG[%d]", flag);
		s = buffer;
	}

	return s;
}

static int hasSourceFlag(Source * source, int flag)
{
	int bit = 1 << flag;
	if (source->flags & bit)
		return 1;
	return 0;
}

static void flagSource(Task * task, Source * source, int flag)
{
	int bit = 1 << flag;
	if (source->flags & bit) {
		if (task->reportDuplicates)
			report_verbose("flagSource: ID %d already has %s flag\n",
					source->id, flagString(flag));
	}
	else {
		++task->flaggedSources[flag];
		source->flags |= bit;
	}
}

static void flagPixel(Task * task, int x, int y, int flag)
{
	int bit = 1 << flag;
	int flags = iimage_get_relative(task->flagImage, x, y);
	if (flag){
	  if (flags & bit) {
	    static int lastFlag = 0;
	    const int LIMIT = 10;
	    static int count;
	    if (flag != lastFlag) {
	      lastFlag = flag;
	      count = LIMIT;
	    }
	    if (count > 0) {
	      --count;
	      if (task->reportDuplicates)
		report_verbose("flagPixel: %d, %d already has %s flag\n", x, y,
			       flagString(flag));
	    }
	  }
	  else {
	    ++task->flaggedPixels[flag];
	    iimage_set_relative(task->flagImage, x, y, flags | bit);
	  }
	}
	else{
	  /* clear the pixels if flag is zero */
	  iimage_set_relative(task->flagImage, x, y, flag);
	}
}

static void flagPixel2(Task * task, int x, int y, int flag)
{
	int bit = 1 << flag;
	int flags = iimage_get_relative(task->qmapImage, x, y);
	if (flag){
	  if (flags & bit) {
	    static int lastFlag = 0;
	    const int LIMIT = 10;
	    static int count;
	    if (flag != lastFlag) {
	      lastFlag = flag;
	      count = LIMIT;
	    }
	    if (count > 0) {
	      --count;
	      if (task->reportDuplicates)
		report_verbose("flagPixel: %d, %d already has %s flag\n", x, y,
			       flagString(flag));
	    }
	  }
	  else {
	    /*++task->flaggedPixels[flag];*/
	    iimage_set_relative(task->qmapImage, x, y, flags | bit);
	  }
	}
	else{
	  /* clear the pixels if flag is zero */
	  iimage_set_relative(task->qmapImage, x, y, flag);
	}
}


static void flagImageRect(Task * task, int x0, int x1, int y0, int y1, int flag)
{
	int x, y;

	if (!task->flagImage)
		return;

	x0 = apply_range(x0, 0, task->flagImage->width - 1);
	x1 = apply_range(x1, 0, task->flagImage->width - 1);

	y0 = apply_range(y0, 0, task->flagImage->height - 1);
	y1 = apply_range(y1, 0, task->flagImage->height - 1);

	for (y = y0; y <= y1; ++y)
		for (x = x0; x <= x1; ++x)
			flagPixel(task, x, y, flag);
}

static void flagImageRect2(Task * task, int x0, int x1, int y0, int y1, int flag)
{
	int x, y;

	if (!task->qmapImage)
		return;

	x0 = apply_range(x0, 0, task->qmapImage->width - 1);
	x1 = apply_range(x1, 0, task->qmapImage->width - 1);

	y0 = apply_range(y0, 0, task->qmapImage->height - 1);
	y1 = apply_range(y1, 0, task->qmapImage->height - 1);

	for (y = y0; y <= y1; ++y)
		for (x = x0; x <= x1; ++x)
			flagPixel2(task, x, y, flag);

}

static void flagImageCircle(Task * task, double xc, double yc, double radius,
		int flag)
{
	int x, y, x0, x1, y0, y1;
	int width, height;
	double d_image;

	if (!task->flagImage)
		return;
	/*printf("L247 xc= %f, yc=%f, flag= %d \n", xc, yc, flag);*/
	width = task->flagImage->width;
	height = task->flagImage->height;

	x0 = apply_range(xc - radius, 0, width - 1);
	x1 = apply_range(xc + radius, 0, width - 1);

	y0 = apply_range(yc - radius, 0, height - 1);
	y1 = apply_range(yc + radius, 0, height - 1);

	for (y = y0; y <= y1; ++y) {
		for (x = x0; x <= x1; ++x) {
			d_image = hypot(xc - x, yc - y);
			if (d_image < radius)
				flagPixel(task, x, y, flag);
		}
	}
}

static void flagImageCircle2(Task * task, double xc, double yc, double radius,
		int flag)
{
	int x, y, x0, x1, y0, y1;
	int width, height;
	double d_image;

	if (!task->qmapImage)
		return;

	width = task->qmapImage->width;
	height = task->qmapImage->height;

	x0 = apply_range(xc - radius, 0, width - 1);
	x1 = apply_range(xc + radius, 0, width - 1);

	y0 = apply_range(yc - radius, 0, height - 1);
	y1 = apply_range(yc + radius, 0, height - 1);

	for (y = y0; y <= y1; ++y) {
		for (x = x0; x <= x1; ++x) {
			d_image = hypot(xc - x, yc - y);
			if (d_image < radius)
				flagPixel2(task, x, y, flag);
		}
	}
}



/*
static void flagImageAnnulus (Task * task, double xc, double yc,
		double radius, double innerRadius, int flag)
{
	int x, y, x0, x1, y0, y1;
	int width, height;
	double d_image;

	if (!task->flagImage)
		return;

	width = task->flagImage->width;
	height = task->flagImage->height;

	x0 = apply_range(xc - radius, 0, width-1);
	x1 = apply_range(xc + radius, 0, width-1);

	y0 = apply_range(yc - radius, 0, height-1);
	y1 = apply_range(yc + radius, 0, height-1);

	for (y = y0; y <= y1; ++y) {
		for (x = x0; x <= x1; ++x) {
			d_image = hypot(xc - x, yc - y);
			if (d_image < radius  && d_image > innerRadius)
				flagPixel(task, x, y, flag);
		}
	}
}
*/
/* Flag image annulus*/

static void flagImageAnnulusZone(Task * task, double xc, double yc,
		double radius, double innerRadius, double scatr1, double scatr2,
		double scatr3, double scatxc, double scatyc, int zone, int flag)
{
	int x, y, x0, x1, y0, y1;
	int width, height;
	double d_image, d_zone;

	if (!task->flagImage)
		return;

	width = task->flagImage->width;
	height = task->flagImage->height;

	x0 = apply_range(xc - radius, 0, width - 1);
	x1 = apply_range(xc + radius, 0, width - 1);

	y0 = apply_range(yc - radius, 0, height - 1);
	y1 = apply_range(yc + radius, 0, height - 1);

	/*printf("L300 zone= %d \n",zone);*/
	for (y = y0; y <= y1; ++y) {
		for (x = x0; x <= x1; ++x) {
			d_image = hypot(xc - x, yc - y);
			d_zone = hypot(scatxc - x, scatyc - y);
			if (zone == 2) {
				/*if (y==y0 && x==x0){
				 printf("L307 zone=%d d_image= %5.1f d_zone=%5.1f\n",zone, d_image, d_zone);
				 }*/
				if (((d_image < radius && d_image > innerRadius) && (d_zone
						> scatr1 && d_zone < scatr2)))
					flagPixel(task, x, y, flag);
			}
			if (zone == 1) {
				/*if (y==y0 && x==x0){
				 printf("L307 zone=%d d_image= %5.1f d_zone=%5.1f\n",zone, d_image, d_zone);
				 }*/
				if (((d_image < radius && d_image > innerRadius) && (d_zone
						> scatr2 && d_zone < scatr3)))
					flagPixel(task, x, y, flag);
			}
			if (zone == 0) {
				/*if (y==y0 && x==x0){
				 printf("L307 zone=%d d_image= %5.1f d_zone=%5.1f\n",zone, d_image, d_zone);
				 }*/
				if (d_image < radius && d_image > innerRadius)
					flagPixel(task, x, y, flag);
			}
		}
	}
} /* Flag image annulus zone */


static void flagImageAnnulusZone2(Task * task, double xc, double yc,
		double radius, double innerRadius, double scatr1, double scatr2,
		double scatr3, double scatxc, double scatyc, int zone, int flag)
{
	int x, y, x0, x1, y0, y1;
	int width, height;
	double d_image, d_zone;

	if (!task->qmapImage)
		return;

	width = task->qmapImage->width;
	height = task->qmapImage->height;

	x0 = apply_range(xc - radius, 0, width - 1);
	x1 = apply_range(xc + radius, 0, width - 1);

	y0 = apply_range(yc - radius, 0, height - 1);
	y1 = apply_range(yc + radius, 0, height - 1);

	for (y = y0; y <= y1; ++y) {
		for (x = x0; x <= x1; ++x) {
			d_image = hypot(xc - x, yc - y);
			d_zone = hypot(scatxc - x, scatyc - y);
			if (zone == 2) {
				if (((d_image < radius && d_image > innerRadius) && (d_zone
						> scatr1 && d_zone < scatr2)))
					flagPixel2(task, x, y, flag);
			}
			if (zone == 1) {
				if (((d_image < radius && d_image > innerRadius) && (d_zone
						> scatr2 && d_zone < scatr3)))
					flagPixel2(task, x, y, flag);
			}
			if (zone == 0) {
				if (d_image < radius && d_image > innerRadius)
					flagPixel2(task, x, y, flag);
			}
		}
	}
} /* Flag image annulus zone 2 */


static void flagRawCircle(Task * task, double xraw, double yraw, double rraw,
		int flag)
{
	double ximage, yimage, rimage;

	if (!task->flagImage)
		return;

	convert_raw_to_image(task, xraw, yraw, &ximage, &yimage);
	rimage = rraw / task->binx;

	flagImageCircle(task, ximage, yimage, rimage, flag);
}

static void flagRawCircle2(Task * task, double xraw, double yraw, double rraw,
		int flag)
{
	double ximage, yimage, rimage;

	if (!task->qmapImage)
		return;

	convert_raw_to_image(task, xraw, yraw, &ximage, &yimage);
	rimage = rraw / task->binx;

	flagImageCircle2(task, ximage, yimage, rimage, flag);
}


/*
static void flagRawAnnulus (Task * task, double xraw, double yraw,
		double rraw, double irraw, int flag)
{
	double ximage, yimage, rimage, irimage;

	if (!task->flagImage)
		return;

	convert_raw_to_image(task, xraw, yraw, &ximage, &yimage);
	rimage = rraw / task->binx;
	irimage = irraw / task->binx;

	flagImageAnnulus(task, ximage, yimage, rimage, irimage, flag);
}
*/

static void flagRawAnnulusZone(Task * task, double xraw, double yraw, int zone,
		int flag)
{
	double ximage, yimage, rimage, irimage;
	const double rraw = 290.0;
	const double irraw = 90.0;
	const double scatr1 = 733;
	const double scatr2 = 858;
	const double scatr3 = 983;
	const double scatxc = 1013.0;
	const double scatyc = 1064.0;
	double scatri1, scatri2, scatri3, scatxci, scatyci;

	if (!task->flagImage)
		return;

	convert_raw_to_image(task, xraw, yraw, &ximage, &yimage);
	rimage = rraw / task->binx;
	irimage = irraw / task->binx;
	scatri1 = (scatr1 + 20.0) / task->binx;
	scatri2 = scatr2 / task->binx;
	scatri3 = (scatr3 - 20.0) / task->binx;
	scatxci = scatxc / task->binx;
	scatyci = scatyc / task->binx;

	/*printf("L375 raw flagging zone= %d\n", zone);*/

	flagImageAnnulusZone(task, ximage, yimage, rimage, irimage, scatri1,
			scatri2, scatri3, scatxci, scatyci, zone, flag);

}

static void flagRawAnnulusZone2(Task * task, double xraw, double yraw, int zone,
		int flag)
{
	double ximage, yimage, rimage, irimage;
	const double rraw = 290.0;
	const double irraw = 90.0;
	const double scatr1 = 733;
	const double scatr2 = 858;
	const double scatr3 = 983;
	const double scatxc = 1013.0;
	const double scatyc = 1064.0;
	double scatri1, scatri2, scatri3, scatxci, scatyci;

	if (!task->qmapImage)
		return;

	convert_raw_to_image(task, xraw, yraw, &ximage, &yimage);
	rimage = rraw / task->binx;
	irimage = irraw / task->binx;
	scatri1 = (scatr1 + 20.0) / task->binx;
	scatri2 = scatr2 / task->binx;
	scatri3 = (scatr3 - 20.0) / task->binx;
	scatxci = scatxc / task->binx;
	scatyci = scatyc / task->binx;


	flagImageAnnulusZone2(task, ximage, yimage, rimage, irimage, scatri1,
			scatri2, scatri3, scatxci, scatyci, zone, flag);

}


static void flagSourceEllipse(Task * task, Source * source, int flag)
{
	int x, y, x0, x1, y0, y1;
	int width, height;
	double a, b, majorAxis_imagePixels;
	Ellipse * e = &source->ellipse_imagePixels;

	if (!task->flagImage)
		return;

	width = task->flagImage->width;
	height = task->flagImage->height;

	a = sqrt(e->rr / e->cxx);
	b = sqrt(e->rr / e->cyy);
	majorAxis_imagePixels = DMAX(a, b);

	x0 = apply_range(source->xImage - majorAxis_imagePixels, 0, width - 1);
	x1 = apply_range(source->xImage + majorAxis_imagePixels, 0, width - 1);

	y0 = apply_range(source->yImage - majorAxis_imagePixels, 0, height - 1);
	y1 = apply_range(source->yImage + majorAxis_imagePixels, 0, height - 1);

	for (y = y0; y <= y1; ++y) {
		for (x = x0; x <= x1; ++x) {
			if (point_in_ellipse(x, y, &source->ellipse_imagePixels))
				flagPixel(task, x, y, flag);
		}
	}
}

static void flagSourceEllipse2(Task * task, Source * source, int flag)
{
	int x, y, x0, x1, y0, y1;
	int width, height;
	double a, b, majorAxis_imagePixels;
	Ellipse * e = &source->ellipse_imagePixels;

	if (!task->qmapImage)
		return;

	width = task->qmapImage->width;
	height = task->qmapImage->height;

	a = sqrt(e->rr / e->cxx);
	b = sqrt(e->rr / e->cyy);
	majorAxis_imagePixels = DMAX(a, b);

	x0 = apply_range(source->xImage - majorAxis_imagePixels, 0, width - 1);
	x1 = apply_range(source->xImage + majorAxis_imagePixels, 0, width - 1);

	y0 = apply_range(source->yImage - majorAxis_imagePixels, 0, height - 1);
	y1 = apply_range(source->yImage + majorAxis_imagePixels, 0, height - 1);

	for (y = y0; y <= y1; ++y) {
		for (x = x0; x <= x1; ++x) {
			if (point_in_ellipse(x, y, &source->ellipse_imagePixels))
				flagPixel2(task, x, y, flag);
		}
	}
}


static void flagSourcePixels(Task * task, Source * source, int flag)
{
	if (!task->flagImage)
		return;

	if (source->isPointSource) {
		/* flag each pixel in aperture */
	  /*printf("L450 xImage= % 2f yImage= % 2f aperture_imagePixels=% 2f flag= %d \n",
		 source->xImage, source->yImage,
		 task->constants->aperture_imagePixels, flag);*/
		flagImageCircle(task, source->xImage, source->yImage,
				task->constants->aperture_imagePixels, flag);
	}
	else {
		/* flag each pixel in ellipse */

		flagSourceEllipse(task, source, flag);
	}
}

static void flagSourcePixels2(Task * task, Source * source, int flag)
{
	if (!task->qmapImage)
		return;

	if (source->isPointSource) {
		/* flag each pixel in aperture */
		flagImageCircle2(task, source->xImage, source->yImage,
				task->constants->aperture_imagePixels, flag);
	}
	else {
		/* flag each pixel in ellipse */

		flagSourceEllipse2(task, source, flag);
	}
}



/*
static void flagSourceAndPixels (Task * task, Source * source, int flag)
{
	flagSource(task, source, flag);

	if (task->flagImage)
		flagSourcePixels(task, source, flag);
}
*/


#ifdef REQUIREMENTS
>
> ************************************************************************************
> * Bad Pixel (Bit 0)
> *
> *************************************************************************************
>
> This flag is set when omdetect does the photometry on each source. For "point" sources,
> if the aperture includes a "bad" pixel, then the flag is set. For an "extended" source,
> if any pixel assigned to the source is "bad", then the flag is set. A "bad" pixel is
> ascertained by checking the "quality" array- a non-zero value indicates a bad pixel.
>
> The quality array is added to the image by omcosflag, by utilising the calibration
> bad pixel map. This program also looks for blank columns in the image, and if any found
> the corresponding pixels in the quality array are set to bad. Omcosflag also
> does a search for isolated very bright pixels, which are also set to bad.
>
>
> --- bob ----------------------------------------------------------------
> 1. We do not have omdetect[1]. But we can pass a quality array to uvotdetect.
> 2. What will determine what are "point" and "extended" sources?

sextractor should calculate semi-major and semi-minor axis dimensions in arcseconds. I cannot remember whether these axes are related to FWHM or Gaussian sigma or something
else. But if the semi-minor axis exceeds some value, related to the calibrated PSF then the source is considered extended. Stephen can help you with sextractor definitions and provide a sensible criterion.

> 3. I assume aperture is a new parameter in arcsec or pixels and that we measure from
> source centroid to center of bad pixel.

The standard UVOT source aperture is 5"". It seems prudent to use a circle of this radius to search for bad pixels.

> 4. I do not know how we can determine the set of pixels assigned to sources. It
> may be possible to use the SExtractor SEGMENTATION map.

This goes back to the major and minor axes above. Once you understand how they are related to the source extent then it should be relatively easy to assign pixels to extended sources.
#endif /* REQUIREMENTS */

int flagBadPixels(Task * task)
{
	int code = 0;
	int i;
	ImageBadPixel * bp;

	report_verbose("flagging bad pixels\n");


	/* flag the pixels */

	for (i = 0; task->flagImage && i < task->nBadPixels; ++i) {
		int x, y;

		bp = &task->badPixel[i];

		x = (int) (bp->xImage + 0.5);
		y = (int) (bp->yImage + 0.5);
		/* checking */

		if (x >= 0 && x < task->rawImage->width && y >= 0 && y
		    < task->rawImage->height){
			flagPixel(task, x, y, FLAG_BAD_PIXEL);
			flagPixel2(task, x, y, FLAG_BAD_PIXEL);
		}
	}

	return code;
}

#ifdef REQUIREMENTS
> ***********************************************************************************
> READ-OUT STREAKS (bit 1) (Algorithm 1)
> ***********************************************************************************
>
> Any point-source with either a raw count-rate >=70 or flagged as having a mod-8 pattern
> is considered to have produced a read-out streak.
>
> If the raw-count rate is greater than 220 then the width of the streak is taken to be 19 pixels.
> otherwise 16 pixels.
>
> Flag any source whose x distance away from the streak-producing source is no greater than the streak width.
>
>
> ***********************************************************************************
> READ-OUT STREAKS (bit 1) (Algorithm 2- only used by omdetect)
> ***********************************************************************************
>
> Since the object producing a read-out streak may lie outside the image, another algorithm is used
> by omdetect.
>
> The algorithm works as follows (mainly derived by trial and error):
>
> [... bunch of code]
>
>
> --- bob ---------------------------------------------------------------------
> 1. do you want to use algorithm 1 or 2?

Both. The second algorithm is only useful if the image is windowed.

> 2. algorithm 1 defines something as the streak width then uses it as if it is
> the half streak width

Use it as the half width. There was debate last week that UVOT images do not have readout streaks. Can you guys look out for examples?

--- bob again -------------------
1. It must be that the streak width given is in raw pixels(?)
---------------------------------
>------------------vny------------------------
>
> Increasing the threshold to 110 counts/s (thresh1) for the new source list
> Removing the criterium of stron mod8 noise
>
#endif /* REQUIREMENTS */

int flagReadoutStreaksAlgo1(Task * task)
{
	int code = 0;
	int s;
	Source * source;
	double streakWidth;
	double thresh0 = 110.0;
	double thresh1 = 300.0;

	report_verbose("flagging readout streaks, algorithm-1 \n");

	switch (task->filter) {

	case FILTER_U:
		thresh0 = 92.0;
		thresh1 = 250.0;
		break;

	case FILTER_B:
	        thresh0 = 90.0; /*490.0;*/
		thresh1 = 500.0;
		break;

	case FILTER_V:
		thresh0 = 110.0;
		thresh1 = 350.0;
		break;

	case FILTER_WHITE:
		thresh0 = 110.0;
		thresh1 = 450.0;
		break;

	case FILTER_UVW1:
		thresh0 = 200.0;
		thresh1 = 500.0;
		break;

	case FILTER_UVW2:
		thresh0 = 150.0;
		thresh1 = 300.0;
		break;

	case FILTER_UVM2:
		thresh0 = 90.0;
		thresh1 = 350.0;
		break;

	default:
		thresh0 = 110.0;
		thresh1 = 300.0;
		break;
	}

	for (s = 0; s < task->nSources; ++s) {

		source = &task->source[s];

		if (!(source->rawRate >= thresh0))
			continue;

		if (!((source->isPointSource) || (source->rawRate >= thresh1)))
			continue; /* only consider point sources or very bright extended*/


		/* source has not produced a readout streak */

		if (source->rawRate > thresh1) {
			streakWidth = 19 / task->binx;
		}
		else {
		  streakWidth = 8 / task->binx; /* 10 */
			/*printf("L686 streakWidth= %5.1f rate= %5.1f \n",streakWidth, source->rawRate);*/
		}

		/*printf("L686 streakWidth= %5.1f rate= %5.1f \n",streakWidth,source->rawRate);*/

		if (task->flagImage) {
			/* flag every pixel in the streak */
			int x0, x1, imageWidth1;
			imageWidth1 = task->flagImage->width - 1;
			x0 = apply_range(source->xImage - streakWidth, 0, imageWidth1);
			x1 = apply_range(source->xImage + streakWidth, 0, imageWidth1);
			flagImageRect(task, x0, x1, 0, task->flagImage->height - 1,
					FLAG_READOUT_STREAK);
			/***************************************************************/
			/* In order to keep the bright source detectable to uvotdetect */
			/* we have to blank out the flagged rectangle in the centre    */
			/* by setting the central pixlels of the flagged region to zero*/
			/*flagSourcePixels(task, source, 0);*/
		}

		if (task->qmapImage) {
			/* flag every pixel in the streak */
			int x0, x1, imageWidth1;
			imageWidth1 = task->qmapImage->width - 1;
			x0 = apply_range(source->xImage - streakWidth, 0, imageWidth1);
			x1 = apply_range(source->xImage + streakWidth, 0, imageWidth1);
			flagImageRect2(task, x0, x1, 0, task->qmapImage->height - 1,
					FLAG_READOUT_STREAK);
			/***************************************************************/
			/* In order to keep the bright source detectable to uvotdetect */
			/* we have to blank out the flagged rectangle in the centre    */
			/* by setting the central pixlels of the flagged region to zero*/
			flagSourcePixels2(task, source, 0);
		}

	}

	return code;
}

static int
writeFloatImage (const char *path, float image[2048][2048])
{
	int code = 0;
	int i,j, nx, ny;
	float z;
	FImage smapImage = { 0 };
	ImageIO io = { 0 };

	smapImage.null = -1;
	smapImage.width=2048;
	smapImage.height=2048;
	nx = 2047;
	ny = 2047;
	/* Allocate smapImage */
	if (!code) {
		report_verbose("L737 allocating the smap image \n");
		code = fimage_allocate(&smapImage, smapImage.width, smapImage.height);
		if (code)
		  report_error("Unable to allocate smapImage image; code=[%d]\n",
			       code);
		else {
		  report_verbose("allocated smap image\n");
		}
	}
	if (!code) {
	  report_verbose("L747 copying the smap image to zero \n");
	  code = fimage_set_constant(&smapImage, 0);
	}
	for (i = 0; i <= nx; ++i) {
	  for (j = 0; j <= ny; ++j) {
	    z = image[i][j];
	    fimage_set_relative(&smapImage, i, j, z);
	  }
	}

	code = fimage_write(&smapImage, path, &io);
	

	return code;
} /* end writeFloatImage */


int flagReadoutStreaksAlgo2(Task * task)
{
  int code = 0;
  int nx, ny;
  int i, il, iu;
  int j, jl, ju;
  int i1, jaux;
  int ileft, iright;
  float z, fmax;
  float faver, naver;
  
  static float auxImage[2048][2048];
  float sig1;
  
  static float vslice[2048];
  static float hslice[2048];
  
  float saver, nsaver;
  
  double streakWidth;
  double streakPosition;
  int biny1=0;


  biny1=task->biny;
  if (biny1<1) biny1=1;
  /*
  printf("L997 biny1=%d dy=%d , xo=%d yo=%d \n", biny1, task->dy, task->x0, task->y0);
  */

  if (!(task->dy==2048)){

    /* The window is not full-frame: continue apllying the Algo2*/
    report_verbose("flagging readout streaks, algorithm-2 \n");
    nx = task->rawImage->width - 1;
    ny = task->rawImage->height - 1;
    il = 0;
    iu = nx;
    jl = 0;
    ju = ny;
    
    /* Fill in the readout streak-map image */
    fmax = -1000;
    saver=0.0;
    nsaver=0.0;

    for (j = jl; j <= ju; ++j) {
      faver=0.0;
      naver=0.0;
      vslice[j]=0;
      
      for (i = il; i <= iu; ++i) {
	z = fimage_get_relative(task->vstreakImage, i, j);
	auxImage[i][j] = z;
	if (z > fmax)
	  fmax = z;
	if (z>0.0){
	  faver+=z;
	  naver+=1.0;
	}
      }/* for i */
      
      if (naver>1.0) faver/=naver;
      vslice[j]=faver;
      saver+=faver;
      nsaver+=1.0;
    }/* for j*/
    
    saver/=nsaver;
    /*printf("L818 slice abverage saver= %f \n",saver);*/
    /*
      printf("L831 dumping the vstreak image .......\n");
      code = writeFloatImage("rdout_image2map.fits", auxImage);
      printf("L833 ......... auxImage .\n");
    */
    /************************************/
    /* Test: fill in the test image */
    /*
      for (j = jl; j <= ju; ++j){
      for (i = il; i <= iu; ++i) {
      auxImage[i][j]=vslice[j];
      } 
      }
    */
    /*
      printf("L831 dumping the vstreak image .......\n");
      code = writeFloatImage("rdout_image2map.fits", auxImage);
      printf("L833 ......... auxImage .\n");
    */
    /************************************/
    
    
    /* Remove the vertical slice values below threshold*/
    for (j = jl; j <= ju; ++j){
      z=vslice[j];
      if (z>saver){
	vslice[j]=-1.0;
      }
      /*
	if (j<100)
	printf(" j= %d z=%f  v= %f \n", j, z, vslice[j]);
      */
    }

    /* Horizontal cross-section */
    for (i = il; i <= iu; ++i) {
      /* sum the vertical velues excluding those that flagged negative in vslice */
      faver=0.0;
      naver=0.0;
      hslice[i]=0.0;
      for (j = jl; j <= ju; ++j){
	if (vslice[j]>0.0 && auxImage[i][j]>0.0){
	  faver+=auxImage[i][j];
	  naver+=1.0;
	}
      } /* for j */
      if (naver>1.0) faver/=naver;
      hslice[i]=faver;
    }/* for i*/
    
    /************************************/
    /* Test: fill in the test image */
    /*
      for (i = il; i <= iu; ++i) {
      for (j = jl; j <= ju; ++j){
      if (vslice[j]>0.0)
      auxImage[i][j]=hslice[i];
      else
      auxImage[i][j]=0.0;
      } 
      }
      printf("L858 dumping the vstreak image .......\n");
      code = writeFloatImage("rdout_image2map.fits", auxImage);
      printf("L860 ......... auxImage .\n");
    */
    /************************************/
    
    /* Compute the average of the streak image */
    j = jl;
    faver = 0.0;
    naver = 0.0;
    for (i = il; i <= iu; ++i) {
      faver += hslice[i];
      naver++;
    }
    if (naver > 1)
      faver /= naver;
    sig1 = sqrt(faver);
    printf("L900 faver= %5.1f sig1= %5.1f\n",faver, sig1);
    
    /*Find the streaks*/
    jaux = jl;
    for (i = il; i <= iu; ++i) {
      if (hslice[i] > (faver + sig1 / 2.0)) { /* streak is found*/
	ileft = i;
	iright = -1;
	hslice[i] = 0.0;
	for (i1 = ileft + 1; i1 <= iu; ++i1) {
	  if (iright >= ileft)
	    continue;
	  if (hslice[i1] < (faver + sig1 / 2.0)) { 
	    /* right edge of the streak is found */
	    iright = i1 - 1;
	    hslice[i1] = 0.0;
	  }
	  else {
	    hslice[i1] = 0.0;
	  }
	}
	streakWidth = (double) (iright - ileft);
	streakPosition = ((double) (iright + ileft)) / 2.0;
	printf("L925 streakPosition= %5.1f  streakWidth= %5.1f \n", streakPosition, streakWidth);
	
	if (streakWidth < 5.0)
	  streakWidth = 5.0;
	
	if (task->flagImage) {
	  /* flag every pixel in the streak in the main quality map */
	  int x0, x1, imageWidth1;
	  /*printf("L933 streal Position= &5.1f   streakWidth= %5.1f \n", streakPosition, streakWidth);*/
	  imageWidth1 = task->flagImage->width - 1;
	  x0 = apply_range(streakPosition - streakWidth, 0, imageWidth1);
	  x1 = apply_range(streakPosition + streakWidth, 0, imageWidth1);
	  flagImageRect(task, x0, x1, 0, task->flagImage->height - 1,
			FLAG_READOUT_STREAK);
	}
	if (task->qmapImage) {
	  /* flag every pixel in the streak in the additional quality map */
	  int x0, x1, imageWidth1;
	  imageWidth1 = task->qmapImage->width - 1;
	  x0 = apply_range(streakPosition - streakWidth, 0, imageWidth1);
	  x1 = apply_range(streakPosition + streakWidth, 0, imageWidth1);
	  flagImageRect2(task, x0, x1, 0, task->qmapImage->height - 1,
			FLAG_READOUT_STREAK);
	}


      }
    }
  }
  else{
    report_warning("flagReadoutStreaksAlgo2: Full-frame image; No need in running Algo2\n");
  } 
  return code;
}

#ifdef REQUIREMENTS
*************************************************
SMOKE RINGS - bit 2
*************************************************

Any object with either a raw count-rate >=60 or flagged as having a mod-8 pattern is considered
to have produced a smoke-ring - any other source within or near to the smoke-ring is flagged.

The predicted position of a smoke-ring is obtained from the following:

1) Compute the radial distance of the star (xstar, ystar) from the centre of the
OM field of view (xCentre=1024.5, yCentre=1024.5)

2) Compute the position of the smoke-ring using:

x (smoke ring) = real(a1 + b1 * x + c1 * y + d1 * x * x + &
e1 * y * y + f1 * x * y) + xCentre

where x = xStar - xCentre, y = yStar - yCentre
and
a1 = 9.6950, b1 = 1.2052,
c1 = 1.1027d-03, d1 = 2.9117d-06
e1 = 1.8790d-06, f1 = 4.2347d-07

y (smoke ring) = real(a2 + b2 * x + c2 * y + d2 * x * x + &
e2 * y * y + f2 * x * y) + yCentre
and
a2 = -4.8165, b2 = 5.0884
c2 = 1.2071, d2 = -2.8810d-07,
e2 = 1.549d-06, f2 = -1.0783d-06

These coefficients were obtained by Simon Rosen from a least-squares fit to about 100
measured positions of stars and their associated smoke-ring.

Any source within a radius of 38 pixels is then flagged (the 38 was derived from an
average radius of 26 + 12 photometry radius).

#endif /* REQUIREMENTS */

int flagSmokeRings(Task * task)
{

	int code = 0;
	int s;
	Source * source;
	double dx, dy;
        double rwx, rwy;
	double xSmoke, ySmoke;
	double xSmokeIn, ySmokeIn, xSmokeOut, ySmokeOut;

	const double xCenter = 1024.5;
	const double yCenter = 1024.5;

	const double a1 = 9.6950;
	const double b1 = 1.2052;
	const double c1 = 1.1027e-3;
	const double d1 = 2.9117e-6;
	const double e1 = 1.8790e-6;
	const double f1 = 4.2347e-7;

	const double a2 = -4.8165;
	const double b2 = 5.0884;
	const double c2 = 1.2071;
	const double d2 = -2.8810e-7;
	const double e2 = 1.549e-6;
	const double f2 = -1.0783e-6;

	const double radius_rawPixels = 38.0;

	const double a1out = 0.02239078318646;
	const double b1out = 0.0004816756256664;
	const double c1out = -2.745742171497e-7;

	const double a1in = -0.0510805;
	const double b1in = -5.39749e-6;

	double rawR, deltaRout, deltaRin, fraction;
	double thresh0 = 150.0; /*V-filter*/

	report_verbose("flagging smoke rings \n");

	switch (task->filter) {

	case FILTER_U:
		thresh0 = 90.0;
		break;

	case FILTER_B:
		thresh0 = 65.0;
		break;

	case FILTER_V:
		thresh0 = 90.0;
		break;

	case FILTER_WHITE:
		thresh0 = 92.0;
		break;

	case FILTER_UVW1:
		thresh0 = 180.0;
		break;

	case FILTER_UVW2:
		thresh0 = 90.0;
		break;

	case FILTER_UVM2:
		thresh0 = 80.0;
		break;

	default:
		thresh0 = 150.0;
		break;
	}


	for (s = 0; s < task->nSources; ++s) {

            source = &task->source[s];
            /*
	    printf("L924 filte=%d  thresh0= %f  \n", task->filter, thresh0);
	    printf("L925 s=%d rawRate= %5.1f rawCounts= %5.1f ", s, source->rawRate, source->rawCounts);
            printf(" x=%f1  y=%f1 \n",source->xRaw, source->yRaw);
            */

		if (!(source->rawRate >= thresh0
		|| hasSourceFlag(source, FLAG_STRONG_MOD8)))
			continue; /* source has not produced a smoke ring */
		rwx=source->xRaw*task->binx+task->x0;
		rwy=source->yRaw*task->biny+task->y0;
		dx = rwx - xCenter;
		dy = rwy - yCenter;

		rawR = sqrt(dx * dx + dy * dy);
		deltaRout = a1out * rawR + b1out * rawR * rawR
				+ c1out * rawR * rawR * rawR;
		deltaRin = a1in * rawR + b1in * rawR * rawR;

		xSmokeOut = rwx;
		ySmokeOut = rwy;
		xSmokeIn = rwx;
		ySmokeIn = rwy;
		if (rawR > 0.0) {
			fraction = (rawR + deltaRout) / rawR;
			xSmokeOut = dx * fraction + xCenter;
			ySmokeOut = dy * fraction + yCenter;
			fraction = (rawR + deltaRin) / rawR;
			xSmokeIn = dx * fraction + xCenter;
			ySmokeIn = dy * fraction + yCenter;
		}

 

		xSmoke = a1 + b1 * dx + c1 * dy
				+ d1 * dx * dx + e1 * dy * dy + f1 * dx * dy;
		ySmoke = a2 + b2 * dx + c2 * dy
				+ d2 * dx * dx + e2 * dy * dy + f2 * dx * dy;

		xSmoke = xSmoke + xCenter;
		ySmoke = ySmoke + xCenter;


                if (task->flagImage){
                  flagRawCircle(task, xSmokeOut, ySmokeOut, radius_rawPixels,
				FLAG_SMOKE_RING);
		  flagRawCircle(task, xSmokeIn, ySmokeIn, radius_rawPixels,
				FLAG_SMOKE_RING);

                  /***************************************************************/
                  /* In order to keep the bright source detectable to uvotdetect */
		  /* we have to blank out the flagged rectangle in the centre    */
		  /* by setting the central pixlels of the flagged region to zero*/
		  /*flagSourcePixels(task, source, 0);*/
                }

                if (task->qmapImage){
                  flagRawCircle2(task, xSmokeOut, ySmokeOut, radius_rawPixels,
				FLAG_SMOKE_RING);
		  flagRawCircle2(task, xSmokeIn, ySmokeIn, radius_rawPixels,
				FLAG_SMOKE_RING);

                  /***************************************************************/
                  /* In order to keep the bright source detectable to uvotdetect */
		  /* we have to blank out the flagged rectangle in the centre    */
		  /* by setting the central pixlels of the flagged region to zero*/
		  flagSourcePixels2(task, source, 0);
                }


	}

	return code;
}

#ifdef REQUIREMENTS
*************************************************
HALO RINGS - bit 5 (borrowing this bit number from the central enhancement)
*************************************************

This algorithm is similar to flagging smoke-rings

Any object with a raw count-rate >=200
AND flagged as having a mod-8 pattern
AND having the position withing the radius of 960 pixels
from the centre of the image
is considered to have produced a halo-ring - any other source within or
near to the halo-ring is flagged.

The predicted position of a halo-ring is obtained from the following:

1) Get the x- and y- coordinates of the source

2) Compute the radial distance of the star (xradial, yradial) from the
center of the image (1024.5, 1024.5)
3) Check the conditions for the halo-ring production;
4) Compute the position of the halo-ring using:

x (halo ring) = real(a1 +
a2 * x +
a3 * x*x +
a4 * x*x*x +
a5 * y +
a6 * x*y +
a7 * x*x * y +
a8 * y*y +
a9 * x* y*y +
a10* y*y*y)

where
a1 = +97.5175,
a2 = -0.218701
a3 = +0.000363801
a4 = -1.4018E-7
a5 = -0.103481
a6 = -2.68774E-6
a7 = +5.32936E-8
a8 = +6.37087E-5
a9 = -6.24458E-8
a10= +4.22387E-9

y (halo ring) = real(b1 +
b2 * x +
b3 * x*x +
b4 * x*x*x +
b5 * y +
b6 * x*y +
b7 * x*x * y +
b8 * y*y +
b9 * x* y*y +
b10* y*y*y)

where
b1 = +78.3445
b2 = -0.0399515
b3 = +1.81649E-5
b4 = -4.02579E-9
b5 = -0.172434
b6 = +2.64199E-5
b7 = -1.83517E-8
b8 = +2.07679E-4
b9 = +1.97041E-8
b10= -6.5617E-8

These coefficients were obtained by VNY from a least-squares fit to about 200
measured positions of stars and their associated halo-rings.

Any source within a radius of 290 unbinned pixels is then flagged (the actual radii of the
halo rings vary from 270 to 290 unbinned pixels, depending on the source position within the OSW).

> 2009-12-20: not all of the sources having the mod-8 pattern are producing the halo-rings, so
> the condition for checking the mod-8 noise pattern is removed
>
> However, fainter sources (120 counts/s) can produce halos when intersected with the
> large scattered light annulus
>
#endif /* REQUIREMENTS */

int flagHaloRings(Task * task)
{

	int code = 0;
	int s;
	Source * source;
	double x, y;
	double dx, xHalo, dy, yHalo;
	double dxCenter, dyCenter, distanceFromCenter;

	double a1 = +97.5175;
	double a2 = -0.218701;
	double a3 = +0.000363801;
	double a4 = -1.4018E-7;
	double a5 = -0.103481;
	double a6 = -2.68774E-6;
	double a7 = +5.32936E-8;
	double a8 = +6.37087E-5;
	double a9 = -5.24458E-8;
	double a10 = +4.22387E-9;

	double b1 = +78.3445;
	double b2 = -0.0399515;
	double b3 = +1.81649E-5;
	double b4 = -4.02579E-9;
	double b5 = -0.172434;
	double b6 = +2.64199E-5;
	double b7 = -1.83517E-8;
	double b8 = +2.07679E-4;
	double b9 = +1.97041E-8;
	double b10 = -6.5617E-8;
	/* The above polynomial coefficients correspond to the filter V*/

	double scatr1 = 733;
	double scatr2 = 858;
	double scatr3 = 983;
	double scatxc = 1013.0;
	double scatyc = 1064.0;

	double rawLim1 = 80.0;
	double rawLim2 = 200.0;
	double rawLim3 = 110.0;

	int zoneNo = -1;

	report_verbose("flagging halo rings \n");

	/* Changing the polynomials according to the filter name */
	switch (task->filter) {

	case FILTER_U:

		a1 = 47.1799;
		a2 = +0.0188393;
		a3 = +0.0000526472;
		a4 = -1.9488E-8;
		a5 = -0.00332491;
		a6 = -2.03551E-5;
		a7 = +8.78338E-9;
		a8 = 2.2195E-5;
		a9 = 7.14078E-10;
		a10 = -9.36718E-9;

		b1 = -59.3986;
		b2 = -0.0141095;
		b3 = 2.35993E-5;
		b4 = -6.71875E-9;
		b5 = 0.080184;
		b6 = 3.4611E-6;
		b7 = -1.64948E-8;
		b8 = -1.9253E-5;
		b9 = +1.34659E-8;
		b10 = 3.28282E-9;

		scatr1 = 733;
		scatr2 = 858;
		scatr3 = 983;
		scatxc = 1013.0;
		scatyc = 1064.0;

		rawLim1 = 93.0;
		rawLim2 = 93.0;
		rawLim3 = 93.0;

		break;

	case FILTER_B:

		a1 = -1.12132;
		a2 = +0.0208541;
		a3 = +0.0000591124;
		a4 = -3.6106E-8;
		a5 = +0.0652544;
		a6 = -1.79795E-5;
		a7 = +3.27489E-8;
		a8 = -7.16385E-5;
		a9 = -1.68482E-8;
		a10 = +2.61619E-8;

		b1 = -119.618;
		b2 = +0.132086;
		b3 = -7.3483E-5;
		b4 = 1.92349E-8;
		b5 = 0.220985;
		b6 = -1.36318E-4;
		b7 = 1.44053E-8;
		b8 = -9.69827E-5;
		b9 = +4.86035E-8;
		b10 = 1.53434E-8;

		scatr1 = 748;
		scatr2 = 862;
		scatr3 = 976;
		scatxc = 972.0;
		scatyc = 1039.0;

		rawLim1 = 400.0;
		rawLim2 = 480.0;
		rawLim3 = 480.0;

		break;

	case FILTER_V:

		a1 = +97.5175;
		a2 = -0.218701;
		a3 = +0.000363801;
		a4 = -1.4018E-7;
		a5 = -0.103481;
		a6 = -2.68774E-6;
		a7 = +5.32936E-8;
		a8 = +6.37087E-5;
		a9 = -5.24458E-8;
		a10 = +4.22387E-9;

		b1 = +78.3445;
		b2 = -0.0399515;
		b3 = +1.81649E-5;
		b4 = -4.02579E-9;
		b5 = -0.172434;
		b6 = +2.64199E-5;
		b7 = -1.83517E-8;
		b8 = +2.07679E-4;
		b9 = +1.97041E-8;
		b10 = -6.5617E-8;

		scatr1 = 733;
		scatr2 = 858;
		scatr3 = 983;
		scatxc = 1013.0;
		scatyc = 1064.0;

		rawLim1 = 290.0;
		rawLim2 = 200.0;
		rawLim3 = 110.0;
		break;

	case FILTER_WHITE:

		a1 = +19.6703;
		a2 = -0.310509;
		a3 = +0.000300269;
		a4 = -6.56017E-8;
		a5 = +0.370038;
		a6 = +1.57495E-4;
		a7 = -9.67163E-8;
		a8 = -4.57782E-4;
		a9 = +2.32014E-8;
		a10 = +1.33996E-7;

		b1 = -66.5966;
		b2 = -0.0932105;
		b3 = +1.76824E-5;
		b4 = +7.96286E-9;
		b5 = +0.159772;
		b6 = +9.19484E-5;
		b7 = -3.65776E-8;
		b8 = -9.47743E-5;
		b9 = +4.32388E-9;
		b10 = +2.04734E-8;

		/*  to check: */
		scatr1 = 733;
		scatr2 = 858;
		scatr3 = 983;
		scatxc = 1013.0;
		scatyc = 1064.0;

		rawLim1 = 350.0;
		rawLim2 = 350.0;
		rawLim3 = 350.0;

		break;

	case FILTER_UVW1:

		a1 = +8.99784;
		a2 = -0.0995284;
		a3 = +1.89676E-4;
		a4 = -4.80143E-8;
		a5 = +0.0319411;
		a6 = +1.76311E-5;
		a7 = -5.19138E-8;
		a8 = -2.70982E-5;
		a9 = +2.93707E-8;
		a10 = -6.53152E-10;

		b1 = -57.7982;
		b2 = -0.18892;
		b3 = +1.89325E-4;
		b4 = -4.88131E-8;
		b5 = +0.247303;
		b6 = +4.08491E-5;
		b7 = -4.98235E-8;
		b8 = -1.72479E-4;
		b9 = +2.79903E-8;
		b10 = +4.11668E-8;

		/*  to check: */
		scatr1 = 733;
		scatr2 = 858;
		scatr3 = 983;
		scatxc = 1013.0;
		scatyc = 1064.0;

		rawLim1 = 250.0;
		rawLim2 = 250.0;
		rawLim3 = 250.0;

		break;

	case FILTER_UVW2:

		a1 = +40.2368;
		a2 = +0.131276;
		a3 = -1.68589E-4;
		a4 = +5.73473E-8;
		a5 = -0.0492335;
		a6 = +1.15889E-4;
		a7 = -1.12056E-8;
		a8 = +9.59962E-7;
		a9 = -4.51813E-8;
		a10 = +1.11661E-8;


		b1 = -12.9135;
		b2 = -0.1204;
		b3 = +9.29085E-5;
		b4 = -2.31722E-8;
		b5 = +0.268428;
		b6 = -1.19346E-5;
		b7 = -6.93897E-9;
		b8 = -2.29412E-4;
		b9 = +4.17225E-8;
		b10 = +5.23563E-8;


		/*  to check: */
                scatr1 = 830;/*733;*/
		scatr2 = 858;
		scatr3 = 983;
		scatxc = 1013.0;
		scatyc = 1064.0;

		rawLim1 = 250.0;
		rawLim2 = 250.0;
		rawLim3 = 200.0;

		break;

	case FILTER_UVM2:

		a1 = -2.63017;
		a2 = -0.0341356;
		a3 = +7.50918E-5;
		a4 = -1.86133E-8;
		a5 = +0.0320265;
		a6 = +7.43639E-5;
		a7 = +1.07984E-9;
		a8 = -4.83027E-5;
		a9 = -4.3003E-8;
		a10 = +2.3074E-8;

		b1 = -6.19901;
		b2 = +0.00108907;
		b3 = +4.75763E-5;
		b4 = -2.36222E-8;
		b5 = -0.274445;
		b6 = -9.03625E-5;
		b7 = +2.86881E-8;
		b8 = +4.37027E-4;
		b9 = +2.80825E-8;
		b10 = -1.36766E-7;

		/*  to check: */
		scatr1 = 733;
		scatr2 = 858;
		scatr3 = 983;
		scatxc = 1013.0;
		scatyc = 1064.0;

		rawLim1 = 250.0;
		rawLim2 = 250.0;
		rawLim3 = 200.0;

		break;

	default:

		a1 = +97.5175;
		a2 = -0.218701;
		a3 = +0.000363801;
		a4 = -1.4018E-7;
		a5 = -0.103481;
		a6 = -2.68774E-6;
		a7 = +5.32936E-8;
		a8 = +6.37087E-5;
		a9 = -5.24458E-8;
		a10 = +4.22387E-9;

		b1 = +78.3445;
		b2 = -0.0399515;
		b3 = +1.81649E-5;
		b4 = -4.02579E-9;
		b5 = -0.172434;
		b6 = +2.64199E-5;
		b7 = -1.83517E-8;
		b8 = +2.07679E-4;
		b9 = +1.97041E-8;
		b10 = -6.5617E-8;

		scatr1 = 733;
		scatr2 = 858;
		scatr3 = 983;
		scatxc = 1013.0;
		scatyc = 1064.0;

		rawLim1 = 80.0;
		rawLim2 = 200.0;
		rawLim3 = 110.0;

		break;
	}

	for (s = 0; s < task->nSources; ++s) {

		source = &task->source[s];

		x = source->xRaw*task->binx+task->x0;
		y = source->yRaw*task->biny+task->y0;

		dxCenter = x - scatxc;
		dyCenter = y - scatyc;

		distanceFromCenter = sqrt(dxCenter * dxCenter + dyCenter * dyCenter);
/*printf("L1805 distanceFromCenter=%f \n", distanceFromCenter);*/
		if (!(source->rawRate >= rawLim1))
			continue;

		if (distanceFromCenter <= scatr1)
			zoneNo = 0;
		if (distanceFromCenter > scatr1 && distanceFromCenter <= scatr2)
			zoneNo = 1;
		if (distanceFromCenter > scatr2 && distanceFromCenter <= scatr3)
			zoneNo = 2;
			/*
			printf("L1429 s= %d x=%5.1f  y=%5.1f rate= %5.1f zone=%d \n",
		            s, source->xRaw, source->yRaw,source->rawRate,zoneNo);
			*/

		/*
		 if (!((source->rawRate >= 200.0
		 || hasSourceFlag(source, FLAG_STRONG_MOD8))
		 && distanceFromCenter < radiusSL ))
		 */
		if (!((source->rawRate >= rawLim2 && distanceFromCenter < scatr1)
				|| (source->rawRate >= rawLim3 && zoneNo > 0)))
			continue; /* source has not produced a halo ring */
		/*
		printf("L872 --------------------- (flagging) -----\n");
		printf("L873 s=%d rawrate= %f1 zone= %d\n", s, source->rawRate, zoneNo);
		printf("L874 xraw=%5.1f  yraw=%5.1f  x=%5.1f y=%5.1f  \n",
		  source->xRaw, source->yRaw, x, y);
		*/

                report_verbose("flagging halo: source= %d x=%5.1f  y=%5.1f rate= %5.1f zone=%d \n",
		            s, source->xRaw, source->yRaw,source->rawRate,zoneNo);

		dx = a1 + a2*x + a3*x*x + a4*x*x*x + a5*y + a6*x*y
				+ a7*x*x*y + a8*y*y + a9*x*y*y + a10*y*y*y;
		dy = b1 + b2*x + b3*x*x + b4*x*x*x + b5*y + b6*x*y
				+ b7*x*x*y + b8*y*y + b9*x*y*y + b10*y*y*y;
		xHalo = x + dx;
		yHalo = y + dy;
/*
		 printf("L886          dx=%5.1f     dy=%5.1f \n",dx, dy);
		 printf("L887       xHalo=%5.1f  yHalo=%5.1f \n",xHalo, yHalo);
*/                	 

		/* Using the flag FLAG_CENTRAL_REGION for these halo-rings */
		/*
		flagRawCircle(task, xHalo, yHalo, radius_rawPixels, FLAG_CENTRAL_REGION);

		flagRawAnnulus(task, xHalo, yHalo, radius_rawPixels,
				inner_radius_rawPixels, FLAG_CENTRAL_REGION);
		 */
                 if (task->flagImage){
		     flagRawAnnulusZone(task, xHalo, yHalo, zoneNo, FLAG_CENTRAL_REGION);

		    /***************************************************************/
		    /* In order to keep the bright source detectable to uvotdetect */
		    /* we have to blank out the flagged rectangle in the centre    */
		    /* by setting the central pixlels of the flagged region to zero*/
		    /*flagSourcePixels(task, source, 0);*/
                 }

                 if (task->qmapImage){
		     flagRawAnnulusZone2(task, xHalo, yHalo, zoneNo, FLAG_CENTRAL_REGION);

		    /***************************************************************/
		    /* In order to keep the bright source detectable to uvotdetect */
		    /* we have to blank out the flagged rectangle in the centre    */
		    /* by setting the central pixlels of the flagged region to zero*/
		    flagSourcePixels2(task, source, 0);
                 }

	}

	return code;
} /* Flag Halo Rings*/

#ifdef REQUIREMENTS
> *************************************************************************
> Source on diffraction spike - bit 3
> *************************************************************************
>
> Any object with either a raw count-rate >=70 or flagged as having
> a mod-8 pattern is checked to see
> if it might have diffraction spikes.
>
> The routine for "detecting" diffraction spikes is a bit crude, and probably not very reliable,
> and probably needs overhauling. The algorithm works as follows:
>
> 1) Count the number of objects along position angles of 45 and 135 degrees from the centre
> of the star that lie within a radius of 10 and 120 pixels from the centre and a distance
> of no more than 5 pixels from the radius line.
>
> 2) Compute the search area from:
>
> SearchArea = (maxRadius - minradius) * 4.0 * maxDistance
>
> 3) Compute the number of objects within a box of width and height 150 pixels centred
> on the star centre, ignoring objects with a raw count-rate greater than 5 (n1).
>
> 4) Compute the number of stars expected within the search area, again ignoring objects
> with a raw count-rate greater than 5 (n2).
>
> 5) If the number of star along the spikes (n1) is > 3 x n2 then conclude that there
> are diffraction spikes and flag sources along the spikes. Stars between a radii of 10 and 120
> and not more than 10 pixels away from the spike line are flagged.
>
>
>
> --- bob -----------------------------------------------------------------------
> 1. I take (3) to be the count of objects detected in that box (which may extend
> outside the bounds of the image)
>
>Agreed.
>
> 2. I take (4) to be the result of (3) times the factor (SearchArea / boxArea)
>
>Agreed.
>
> ---- vny ---------------------------------------------------------------------
> With the new source list the diffraction spike flagging threshold
> is increased from 70 to 200 counts/s for the V-filter. This value is filter-dependent,
> so the variable thresh1 will be used instead;
> The parameteres of the flagged region are slightly modified: length from 120 to 200,
> width from 10 to 5; for the sources between 110 counts/c (thresh0) and 200 counts/s (thresh1)
> the length is left to be 120 pixels
>
#endif /* REQUIREMENTS */

int flagDiffractionSpikes(Task * task)
{
	int code = 0;
	int s;
	Source * source;
	const double r0 = 5;
	const double r1high = 200;
	const double r1low = 120;

	const double wide = 5;
	const double cos45d = cos(M_PI / 4);
	const double sin45d = sin(M_PI / 4);
	double dx, dy, dxhat, dyhat;
	int x, y;
	double r1;
	double thresh0 = 110.0;
	double thresh1 = 200.0;

	report_verbose("flagging diffraction spikes \n");

	/*
	 printf("L920 Flagging diffraction spikes for the sources: \n");
	 printf("------------------------------------------------ \n");
	 */

	switch (task->filter) {

	case FILTER_U:
		thresh0 = 92.0;
		thresh1 = 250.0;
		break;

	case FILTER_B:
		thresh0 = 100.0;
		thresh1 = 500.0;
		break;

	case FILTER_V:
		thresh0 = 100.0;
		thresh1 = 300.0;
		break;

	case FILTER_WHITE:
		thresh0 = 100.0;
		thresh1 = 350.0;
		break;

	case FILTER_UVW1:
		thresh0 = 250.0;
		thresh1 = 500.0;
		break;

	case FILTER_UVW2:
		thresh0 = 300.0;
		thresh1 = 400.0;
		break;

	case FILTER_UVM2:
		thresh0 = 300.0;
		thresh1 = 700.0;
		break;

	default:
		thresh0 = 110.0;
		thresh1 = 200.0;
		break;
	}

	for (s = 0; s < task->nSources; ++s) {
		source = &task->source[s];

		r1 = r1high;
		if (!(source->rawRate >= thresh0 /*110.0*/
		|| hasSourceFlag(source, FLAG_STRONG_MOD8)))
			continue; /* source not checked for diffraction spikes */

		/* For not very bright sources reduce the length of the flagged region*/
		if (source->rawRate < thresh1 /*200.0*/)
			r1 = r1low;

		/* update quality map (by Bob Weigang 2009-11-22) */
		if (task->flagImage) {
			/* disable duplicate reporting during diffraction spike flagging */
			int tmp = task->reportDuplicates;
			task->reportDuplicates = 0;
			/*
			 printf("L932 s= %d rawrate= %f1  x=%f1  y=%f1 \n",s,
			 source->rawRate, source->xRaw, source->yRaw);
			 */
			for (dx = -(r1 + 0.5); dx <= (r1 + 0.5); dx += 0.5)
				for (dy = -(wide + 0.5); dy <= (wide + 0.5); dy += 0.5) {
					if (fabs(dx) < r0)
						continue;
					dxhat = cos45d * dx - sin45d * dy;
					dyhat = sin45d * dx + cos45d * dy;
					x = (int) floor(dxhat + 0.5 + source->xImage);
					y = (int) floor(dyhat + 0.5 + source->yImage);
					if (x >= 0 && x < task->flagImage->width && y >= 0 && y
							< task->flagImage->height)
						flagPixel(task, x, y, FLAG_DIFFRACTION_SPIKE);
				}

			for (dy = -(r1 + 0.5); dy <= (r1 + 0.5); dy += 0.5)
				for (dx = -(wide + 0.5); dx <= (wide + 0.5); dx += 0.5) {
					if (fabs(dy) < r0)
						continue;
					dxhat = cos45d * dx - sin45d * dy;
					dyhat = sin45d * dx + cos45d * dy;
					x = (int) floor(dxhat + 0.5 + source->xImage);
					y = (int) floor(dyhat + 0.5 + source->yImage);
					if (x >= 0 && x < task->flagImage->width && y >= 0 && y
							< task->flagImage->height)
						flagPixel(task, x, y, FLAG_DIFFRACTION_SPIKE);
				}

			task->reportDuplicates = tmp;

                        /***************************************************************/
		        /* In order to keep the bright source detectable to uvotdetect */
		        /* we have to blank out the flagged rectangle in the centre    */
		        /* by setting the central pixlels of the flagged region to zero*/
		        /*flagSourcePixels(task, source, 0);*/

		}

		if (task->qmapImage) {
			/* disable duplicate reporting during diffraction spike flagging */
			int tmp = task->reportDuplicates;
			task->reportDuplicates = 0;

			for (dx = -(r1 + 0.5); dx <= (r1 + 0.5); dx += 0.5)
				for (dy = -(wide + 0.5); dy <= (wide + 0.5); dy += 0.5) {
					if (fabs(dx) < r0)
						continue;
					dxhat = cos45d * dx - sin45d * dy;
					dyhat = sin45d * dx + cos45d * dy;
					x = (int) floor(dxhat + 0.5 + source->xImage);
					y = (int) floor(dyhat + 0.5 + source->yImage);
					if (x >= 0 && x < task->qmapImage->width && y >= 0 && y
							< task->qmapImage->height)
						flagPixel2(task, x, y, FLAG_DIFFRACTION_SPIKE);
				}

			for (dy = -(r1 + 0.5); dy <= (r1 + 0.5); dy += 0.5)
				for (dx = -(wide + 0.5); dx <= (wide + 0.5); dx += 0.5) {
					if (fabs(dy) < r0)
						continue;
					dxhat = cos45d * dx - sin45d * dy;
					dyhat = sin45d * dx + cos45d * dy;
					x = (int) floor(dxhat + 0.5 + source->xImage);
					y = (int) floor(dyhat + 0.5 + source->yImage);
					if (x >= 0 && x < task->qmapImage->width && y >= 0 && y
							< task->qmapImage->height)
						flagPixel2(task, x, y, FLAG_DIFFRACTION_SPIKE);
				}

			task->reportDuplicates = tmp;

                        /***************************************************************/
		        /* In order to keep the bright source detectable to uvotdetect */
		        /* we have to blank out the flagged rectangle in the centre    */
		        /* by setting the central pixlels of the flagged region to zero*/
		        flagSourcePixels2(task, source, 0);

		}



	} /* for s=0 */

	return code;
}

/* Compute the size of the flagging square for flagging the mod8-pattern */
int computeMod8square(Task * task, float rawRate)
{

	int mod8size = 0;

	float a1 = -9.95358;
	float a2 = 0.55208;

	float mod8sizeMax=90.0;
	 

	/* The above polynomial coefficients correspond to the filter B*/

	/*report_verbose("computing the size of the mod8-corruption square  \n");*/

	/* Changing the coefficients according to the filter name */
	switch (task->filter) {

	case FILTER_U:

		a1 = 1.56231;
		a2 = 0.46366;
		mod8sizeMax=120.0;

		break;

	case FILTER_B:

		a1 = 23.7363;
		a2 = 0.17352;
		mod8sizeMax=90.0;
		break;

	case FILTER_V:
		a1 = 21.0585;
		a2 = 0.262103;
		mod8sizeMax=90.0;

		break;

	case FILTER_WHITE:
		a1 = 51.0923;
		a2 = 0.153499;
		mod8sizeMax=120.0;
		break;

	case FILTER_UVW1:

		a1 = 18.9862;
		a2 = 0.164676;
		mod8sizeMax=120.0;

		break;

	case FILTER_UVW2:

		a1 = 12.5963;
		a2 = 0.151341;
		mod8sizeMax=120.0;

		break;

	case FILTER_UVM2:
		a1 = 33.084;
		a2 = 0.106789;
		mod8sizeMax=120.0;


		break;

	default:

		a1 = 23.7363;
		a2 = 0.17352;
		mod8sizeMax=90.0;

		break;
	}

	mod8size = (int) a1 + a2 * rawRate + 0.5;
	if (mod8size<4) mod8size=4;
	if (mod8size>mod8sizeMax) mod8size=mod8sizeMax;

	return mod8size;
} /* computeMod8square*/

#ifdef REQUIREMENTS
>****************************************************************************************
>Source surrounded by strong mod-8 pattern - bit 4
>****************************************************************************************
>
>A number of checks on the pixels surrouding the central pixel of the source are performed
>to determine if there is a "strong" mod-8 pattern.
>
>[bunch of code]
> VNY 2009-11-29 if the value for dx were initially increased, say, to 8 pixels (dx=8/binx), then the mod-8
> corrupted sources would be found faster
>
> VNY 2010-05-10 it seems to be easies to use the source count rates and a linear relashionship
  > between them and the side of the masking square (calibrated for each filter individually)
>
#endif /* REQUIREMENTS */

int flagStrongMod8Pattern(Task * task)
{
	int code = 0;
	int s;
	int binx, dx, nx1, ny1, nx, ny;
	int i, i0, i1, i2, il, iu, il1, iu1;
	int j, j0, j1, j2, jl, ju, jl1, ju1;
	int n, nmin, n1, n3a, n3b, n3;
	int iMinus1, iMinus2, iMinus3, iPlus1, iPlus2, iPlus3;
	int jMinus1, jMinus2, jMinus3, jPlus1, jPlus2, jPlus3;
	int imin, imax, jmin, jmax;
	float z, z1, z2, z3, fmax, lmax, null;
	int kmin, k1, k2, k3, k4, k5;
	int maxWidth;
	const double nsigma = 2;
	double thresh0 = 30.0;
	int mod8size = 0;
	int x0,x1,y0,y1;

	report_verbose("flagging strong Mod-8 pattern; filter=%s\n", task->filterString);

	switch (task->filter) {

	case FILTER_U:
		thresh0 = 50.0;
		break;

	case FILTER_B:
		thresh0 = 45.0;
		break;

	case FILTER_V:
		thresh0 = 60.0;
		break;

	case FILTER_WHITE:
		thresh0 = 60.0;
		break;

	case FILTER_UVW1:
		thresh0 = 60.0;
		break;

	case FILTER_UVW2:
		thresh0 = 80.0;
		break;

	case FILTER_UVM2:
		thresh0 = 20.0;
		break;

	default:
		thresh0 = 30.0;
		break;
	}

	binx = task->binx;
	if (binx<1) binx=1;
	dx = 4 / binx;

	nx1 = task->rawImage->width;
	ny1 = task->rawImage->height;
	nx = task->rawImage->width - 1;
	ny = task->rawImage->height - 1;
	null = task->rawImage->null;

	for (s = 0; s < task->nSources; ++s) {

		Source * source = &task->source[s];

		i0 = (int) floor(source->xImage + 0.5);
		j0 = (int) floor(source->yImage + 0.5);

		il = IMAX(0, i0 - dx);
		iu = IMIN(nx, i0 + dx);
		jl = IMAX(0, j0 - dx);
		ju = IMIN(ny, j0 + dx);

		fmax = -1000;
		for (i = il; i <= iu; ++i) {
			for (j = jl; j <= ju; ++j) {
				z = fimage_get_relative(task->rawImage, i, j);
				if (z > fmax)
					fmax = z;
			}
		}

		{
			static int warned = 0;
			if (!warned) {
				warned = 1;
				report_warning(
						"flagStrongMod8: lowered fmax from 1000 to force check\n");
			}
		}
		/*printf("L1643 source=%d  i=%d  j=%d fmax=%5.1f \n", s, i0, j0, fmax);*/
		if (fmax < thresh0 /*30.0*/)
#if CORRECT_VALUE
			(fmax < 1000.0)
#endif
			;
		else if (fmax > 400.0) {
		  mod8size=computeMod8square(task, source->rawRate);
		  mod8size/=binx;

		  x0=i0-mod8size/2;
		  if (x0 < 0) x0=0;
		  x1=i0+mod8size/2;
		  if (x1 > nx) x1=nx;
		  y0=j0-mod8size/2;
		  if (y0 < 0) y0=0;
		  y1=j0+mod8size/2;
		  if (y1 > ny) y1=ny;

		  if (task->flagImage) {
		    flagImageRect(task,x0,x1,y0,y1,FLAG_STRONG_MOD8);
		    /***************************************************************/
		    /* In order to keep the bright source detectable to uvotdetect */
		    /* we have to blank out the flagged rectangle in the centre    */
		    /* by setting the central pixlels of the flagged region to zero*/
		    /*flagSourcePixels(task, source, 0);*/

		    /*printf("L1651 fmax= % 5.1f  rate=% 5.1f  source =%d  i=%d  j=%d mod8size= %d \n", 
		      fmax, source->rawRate, s, i0, j0, mod8size);*/
		  }

		  if (task->qmapImage) {
		    flagImageRect2(task,x0,x1,y0,y1,FLAG_STRONG_MOD8);
		    /***************************************************************/
		    /* In order to keep the bright source detectable to uvotdetect */
		    /* we have to blank out the flagged rectangle in the centre    */
		    /* by setting the central pixlels of the flagged region to zero*/
		    flagSourcePixels2(task, source, 0);
		  }
			
		}
		else { /* need to work */
			maxWidth = 8 / binx;
			dx = 30 / binx; /* changed dx */

			imin = IMAX(0, i0 - maxWidth);
			imax = IMAX(nx, i0 + maxWidth);

			jmin = IMAX(0, j0 - maxWidth);
			jmax = IMAX(ny, j0 + maxWidth);

			il1 = IMAX(0, i0 - dx);
			iu1 = IMIN(nx, i0 + dx);
			jl1 = IMAX(0, j0 - dx);
			ju1 = IMIN(ny, j0 + dx);

			dx = 10 / binx; /* changed dx again! */

			il = IMAX(0, i0 - dx);
			iu = IMAX(nx, i0 + dx);
			jl = IMAX(0, j0 - dx);
			ju = IMAX(ny, j0 + dx);

			k1 = k2 = k3 = k4 = 0;

			nmin = 2 * dx; /* never used! */

			n = ju - jl + 1;
			if (n >= 10) {
				nmin = n - 3;

				/* iLoop1 */
				for (i1 = i0 - 10 / binx; i1 >= il1; --i1) {
					iMinus1 = i1 - 1;
					iMinus2 = i1 - 2;
					iMinus3 = i1 - 3;

					if (iMinus3 < 0)
						break;

					lmax = -1000;
					for (j = jl; j <= ju; ++j) {
						z = fimage_get_relative(task->rawImage, i1, j);
						if (z > lmax)
							lmax = z;
					}
					if (lmax > fmax)
						break;

					n1 = 0;
					for (j = jl; j <= ju; ++j) {
						z = fimage_get_relative(task->rawImage, i1, j);
						z1 = fimage_get_relative(task->rawImage, iMinus1, j);
						if (z != null && z1 != null)
							if (z > z1 + DMAX(2.0, 2.0 * sqrt(z1)))
								++n1;
					}

					if (n1 < nmin)
						continue;

					n3a = 0;
					n3b = 0;
					for (j = jl; j <= ju; ++j) {
						z1 = fimage_get_relative(task->rawImage, iMinus1, j);
						z2 = fimage_get_relative(task->rawImage, iMinus2, j);
						z3 = fimage_get_relative(task->rawImage, iMinus3, j);
						if (z1 != null && z2 != null && z3 != null) {
							if (z2 > z1)
								++n3a;
							if (z3 > z1)
								++n3b;
						}
					}

					n3 = IMAX(n3a, n3b);

					if (n3 >= nmin) {
						imin = IMIN(imin, iMinus3);
						k1 = 1;
					}
				}

				/* iLoop2 */
				for (i2 = i0 + 10 / binx; i2 <= iu1; ++i2) {
					iPlus1 = i2 + 1;
					iPlus2 = i2 + 2;
					iPlus3 = i2 + 3;

					if (iPlus3 > nx)
						break;

					lmax = -1000;
					for (j = jl; j <= ju; ++j) {
						z = fimage_get_relative(task->rawImage, i2, j);
						if (z > lmax)
							lmax = z;
					}

					if (lmax > fmax)
						break;

					n1 = 0;
					for (j = jl; j <= ju; ++j) {
						z = fimage_get_relative(task->rawImage, i1, j);
						z1 = fimage_get_relative(task->rawImage, iPlus1, j);
						if (z != null && z1 != null)
							if (z > z1 + DMAX(2.0, 2.0 * sqrt(z1)))
								++n1;
					}
					if (n1 < nmin)
						continue;

					n3a = 0;
					n3b = 0;
					for (j = jl; j <= ju; ++j) {
						z1 = fimage_get_relative(task->rawImage, iPlus1, j);
						z2 = fimage_get_relative(task->rawImage, iPlus2, j);
						z3 = fimage_get_relative(task->rawImage, iPlus3, j);
						if (z1 != null && z2 != null && z3 != null) {
							if (z2 > z1)
								++n3a;
							if (z3 > z1)
								++n3b;
						}
					}

					n3 = IMAX(n3a, n3b);

					if (n3 >= nmin) {
						imax = IMAX(imax, iPlus3);
						k2 = 1;
					}
				}

			}

			n = iu - il + 1;

			if (n >= 10) {
				nmin = n - 3;
				/* jLoop1: */
				for (j1 = j0 + 10 / binx; j1 <= ju1; ++j1) {
					k5 = k5 + 1; /* not used */
					jPlus1 = j1 + 1;
					jPlus2 = j1 + 2;
					jPlus3 = j1 + 3;

					if (jPlus3 > ny)
						break;

					lmax = -1000;
					for (i = il; i <= iu; ++i) {
						z = fimage_get_relative(task->rawImage, i, j1);
						if (z != null && z > lmax)
							lmax = z;
					}
					if (lmax > fmax)
						break;

					n1 = 0;
					for (i = il; i <= iu; ++i) {
						z = fimage_get_relative(task->rawImage, i, j1);
						z1 = fimage_get_relative(task->rawImage, i, jPlus1);
						if (z != null && z1 != null)
							if (z > z1 + DMAX(2.0, nsigma * sqrt(z1)))
								++n1;
					}
					if (n1 < nmin)
						continue;

					n3a = 0;
					n3b = 0;
					for (i = il; i <= iu; ++i) {
						z1 = fimage_get_relative(task->rawImage, i, jPlus1);
						z2 = fimage_get_relative(task->rawImage, i, jPlus2);
						z3 = fimage_get_relative(task->rawImage, i, jPlus3);
						if (z1 != null && z2 != null && z3 != null) {
							if (z2 > z1)
								++n3a;
							if (z3 > z1)
								++n3b;
						}

						n3 = IMAX(n3a, n3b);
						if (n3 >= nmin) {
							jmax = IMAX(jmax, jPlus3);
							k3 = 1;
						}
					}
				}

				/* jLoop2: */
				for (j2 = j0 - 10 / binx; j1 >= jl1; --j1) {
					jMinus1 = j1 - 1;
					jMinus2 = j1 - 2;
					jMinus3 = j1 - 3;

					if (jMinus3 < 0)
						break;

					lmax = -1000;
					for (i = il; i <= iu; ++i) {
						z = fimage_get_relative(task->rawImage, i, j2);
						if (z != null && z > lmax)
							lmax = z;
					}
					if (lmax > fmax)
						break;

					n1 = 0;
					for (i = il; i <= iu; ++i) {
						z = fimage_get_relative(task->rawImage, i, j2);
						z1 = fimage_get_relative(task->rawImage, i, jMinus1);
						if (z != null && z1 != null)
							if (z > z1 + DMAX(2.0, nsigma * sqrt(z1)))
								++n1;
					}
					if (n1 < nmin)
						continue;

					n3a = 0;
					n3b = 0;
					for (i = il; i <= iu; ++i) {
						z1 = fimage_get_relative(task->rawImage, i, jMinus1);
						z2 = fimage_get_relative(task->rawImage, i, jMinus2);
						z3 = fimage_get_relative(task->rawImage, i, jMinus3);
						if (z1 != null && z2 != null && z3 != null) {
							if (z2 > z1)
								++n3a;
							if (z3 > z1)
								++n3b;
						}

						n3 = IMAX(n3a, n3b);
						if (n3 >= nmin) {
							jmin = IMAX(jmin, jMinus3);
							k4 = 1;
						}
					}
				}
			}

			kmin = 3;

			/* REW: somewhat odd in that the lower left and upper right corners
			 are handled differently than the other two */
			if (source->xImage <= 12.0 / binx || source->yImage <= 12.0 / binx)
				--kmin;

			if (source->xImage >= nx1 - 12.0 / binx || source->yImage >= ny1
					- 12.0 / binx)
				--kmin;

			if (k1 + k2 + k3 + k4 >= kmin) {
				/* REW: the set of pixels flagged for STRONG_MOD8 probably
				 should be more than just the detection ellipse... */

				if (task->flagImage)
					flagSourcePixels(task, source, FLAG_STRONG_MOD8);
				if (task->qmapImage)
					flagSourcePixels2(task, source, FLAG_STRONG_MOD8);
			}
		}

	}

	return code;
}

#ifdef REQUIREMENTS
>****************************************************************************************
>Source within central-enhancement region - bit 5
>***************************************************************************************
>
>If any source is within the circle of radius 160 centred on x=992, y=1063 is quality flag is set.

#endif /* REQUIREMENTS */

int flagCentralEnhancementRegion(Task * task)
{
	int code = 0;
	int s;
	double d_center;

	const double x0 = 992.0;
	const double y0 = 1063.0;
	const double r0 = 160.0;

	for (s = 0; s < task->nSources; ++s) {

		Source * source = &task->source[s];

		d_center = hypot(source->xRaw - x0, source->yRaw - y0);

		if (d_center < r0)
			flagSource(task, source, FLAG_CENTRAL_REGION);
	}

	flagRawCircle(task, x0, y0, r0, FLAG_CENTRAL_REGION);

	return code;
}

#ifdef REQUIREMENTS
> ****************************************************************************************
> Source lies near to a bright source - bit 6
> ***************************************************************************************
>
> Any source either having a raw count-rate >= 80, or with quality flag bit 4 set (mod-8 pattern)
> is checked for "close" neighbours, which may be flagged.
>
> The algorithm is as follows
>
> Check each source in the vicinity of the bright source
>
> 1) Ignore any neighbour with quality bits 4 or 6 set or has a greater count rate.
>
> 3) Compute the maximum distance for flagging the source from
>
> maxRad = max(exclusionradius, min(70.0, 35.0 * countTable(i) / 80.0) )
>
> If the flagging is done by omdetect, the exclusion radius is determined from the algorithm at the end
>
> 4) If the neighbour lies within this radius, flag the star.
>
>
> --- bob -------------------------------------------------------
> 1. missing step 2?
>
>Not in reality. There is no step 2.
>
> 2. does countTable(i) refer to the raw count rate of the bright source?
> ---------------------------------------------------------------
>
>It is raw counts.
>--- bob again --------------------
>No, I looked at the OM code and its rate
>----------------------------------

>
> Exclusion radius algorithm (omdetect)
>
> The algorithm is as follows:
>
> 1) Determine the mean pixel value around the centre of the source, for a series of increasing
> radii from the centre.
>
> 2) Fit a polynomial to these values.
>
> 3) Compute the radius at which the profile reaches the background value, and set the
> exclusionradius equal to this value.
>
>
> --- bob --------------------------------------------------------
> 1. is an appropriate series of increasing radii 5, 10, 15 pixels?
>
>We create a series sampled at 1 unbinned pixel separation.
>
> 2. fit a quadratic polynomial?
>
>Yes.
>
> 3. are we to use the current background value coming out of uvotdetect?
>
>I currently can not think of a reason not to use the uvotdetect background.
>
>--- bob again --------------------------------------------------
>I do not intend to try to implement the complicated exclusion radius
>algorith until GSL or equivalent is in HEADAS

#endif /* REQUIREMENTS */

int flagNearBrightSource(Task * task){
        int code = 0;
        int s;
	Source * source;
	double radius_image;
        float thresh0=80.0; /* below this threshold there is no light from the source nearby */
        
        
	float a1 = 9.90472;
	float a2 = 0.275299;
        float rawCountMax=150.0;
        float nearBrightMax=50.0;

	report_verbose("flagging the vicinity of bright sources \n");

	switch (task->filter) {

	case FILTER_U:
		thresh0 = 50.0;
                a1=-1.30755;
                a2=0.543449;
                rawCountMax=350.0;
                nearBrightMax=80.0;
		break;

	case FILTER_B:
		thresh0 = 60.0;
                a1=9.90472;
                a2=0.275299;
                rawCountMax=150.0;
                nearBrightMax=50.0;
		break;

	case FILTER_V:
		thresh0 = 60.0;
                a1=29.4015;
                a2=0.0931704;
                rawCountMax=300.0;
                nearBrightMax=60.0;		
		break;

	case FILTER_WHITE:
		thresh0 = 60.0;
                a1=44.9826;
                a2=0.0160221;
                rawCountMax=500.0;
                nearBrightMax=120.0;
		break;

	case FILTER_UVW1:
		thresh0 = 60.0;
                a1=30.7885;
                a2=0.0567005;
                rawCountMax=700.0;
                nearBrightMax=60.0;		
		break;

	case FILTER_UVW2:
		thresh0 = 30.0;
                a1=25.73;
                a2=0.04348;
                rawCountMax=700.0;
                nearBrightMax=60.0;		
		break;

	case FILTER_UVM2:
		thresh0 = 20.0;
                a1=29.5876;
                a2=0.0580443;
                rawCountMax=700.0;
                nearBrightMax=100.0;	
		break;

	default:
	  thresh0 = 80.0;
          a1=9.90472;
          a2=0.275299;
          rawCountMax=150.0;
          nearBrightMax=50.0;		
	  break;
	}

	for (s = 0; s < task->nSources; ++s) {

		source = &task->source[s];

		if (!(source->rawRate >= thresh0
				|| hasSourceFlag(source, FLAG_STRONG_MOD8)))
			continue; /* do not need to check for close neighbors */

		radius_image = DMIN(nearBrightMax, a1+a2 * source->rawRate) / task->binx;
			/*
			printf("L2749 s=%d x=%f y=%f rate=%f radius=%f \n", 
	  s, source->xImage, source->yImage, source->rawRate, radius_image); 
			*/
		if (task->flagImage) {
                        flagImageCircle(task, source->xImage, source->yImage, radius_image,
				FLAG_NEAR_BRIGHT_SOURCE);

		        /***************************************************************/
		        /* In order to keep the bright source detectable to uvotdetect */
		        /* we have to blank out the flagged rectangle in the centre    */
		        /* by setting the central pixlels of the flagged region to zero*/
		        /*flagSourcePixels(task, source, 0);*/
               }
		if (task->qmapImage) {
                        flagImageCircle2(task, source->xImage, source->yImage, radius_image,
				FLAG_NEAR_BRIGHT_SOURCE);

		        /***************************************************************/
		        /* In order to keep the bright source detectable to uvotdetect */
		        /* we have to blank out the flagged rectangle in the centre    */
		        /* by setting the central pixlels of the flagged region to zero*/
		        flagSourcePixels2(task, source, 0);
               }

	}

	return code;
}

#ifdef REQUIREMENTS

>**************************************************************************
>Source near to an image edge. (Bit 7)
>***************************************************************************
>
>Any point-source whose centre lies within 12/binx pixels from an image edge (non sky-images) is flagged
>as as being close to an image edge.
>
>Any extended source that has a pixel adjacent to an edge (as determined by the source pixel map that
		>omdetect constructs for each source) is also flagged as being close to an edge.
>
>Additionally, any source that is close to one of the four corners of the OM field of view is also
>flagged as being close to an edge.
>
>The criterion for doing this is-
>
>If any source lies outside a radius of 1300 pixels from a point centred on x=979.2 and y=1016.0 then
>is is flagged. This point and radius was determined from visual inspection of many OM full-frame images.

#endif /* REQUIREMENTS */

int flagNearImageEdge(Task * task)
{
	int code = 0;

	int width = task->rawImage->width;
	int height = task->rawImage->height;

	const int min_pix = 12 / task->binx;

	report_verbose("flagging the vicinity of the image edge \n");
	if (task->flagImage) {
           flagImageRect(task, 0, min_pix - 1, 0, height - 1, FLAG_NEAR_IMAGE_EDGE);
	   flagImageRect(task, width - min_pix, width - 1, 0, height - 1,
			FLAG_NEAR_IMAGE_EDGE);
	   flagImageRect(task, min_pix, width - 1 - min_pix, 0, min_pix - 1,
			FLAG_NEAR_IMAGE_EDGE);
	   flagImageRect(task, min_pix, width - 1 - min_pix, height - min_pix, height
			- 1, FLAG_NEAR_IMAGE_EDGE);
        }
	if (task->qmapImage) {
           flagImageRect2(task, 0, min_pix - 1, 0, height - 1, FLAG_NEAR_IMAGE_EDGE);
	   flagImageRect2(task, width - min_pix, width - 1, 0, height - 1,
			FLAG_NEAR_IMAGE_EDGE);
	   flagImageRect2(task, min_pix, width - 1 - min_pix, 0, min_pix - 1,
			FLAG_NEAR_IMAGE_EDGE);
	   flagImageRect2(task, min_pix, width - 1 - min_pix, height - min_pix, height
			- 1, FLAG_NEAR_IMAGE_EDGE);
        }

	return code;
}

#if ALTERNATIVE
> In flagNearImageEdge, it is only necessary to iterate over pixels along
> the raw image boundary pixels. Here is code, but I do not think it will
> run faster in practice and it is significantly more code.

i = 0;
if (!flag && i0 <= i && i1 >= i)
for (j = j0; !flag && j <= j1; ++j)
if (point_in_ellipse(i, j, &source->ellipse_imagePixels))
flag = 1;

i = task->rawImage->width-1;
if (!flag && i0 <= i && i1 >= i)
for (j = j0; !flag && j <= j1; ++j)
if (point_in_ellipse(i, j, &source->ellipse_imagePixels))
flag = 1;

j = 0;
if (!flag && j0 <= j && j1 >= j)
for (i = i0; !flag && i <= i1; ++i)
if (point_in_ellipse(i, j, &source->ellipse_imagePixels))
flag = 1;

j = task->rawImage->height-1;
if (!flag && j0 <= j && j1 >= j)
for (i = i0; !flag && i <= i1; ++i)
if (point_in_ellipse(i, j, &source->ellipse_imagePixels))
flag = 1;
#endif

#ifdef REQUIREMENTS
> **************************************************************************
> Part of a point-source lies within an extended source. (Bit 8)
> ***************************************************************************
>
> The algorithm checks to see if any pixels assigned to the point source
> overlap with any pixels assigned to an extened source- if so the flag is set.
>
> This check is not carried out on sources with quality bit 4 set (mod-8 source).
>
>
> --- bob ------------------------------------------------------------
> 1. again assumes we know what pixels are associated with extended sources
> 2. for point sources, what is their radius assumed to be?
> --- bob again ------------------------------------------------------
> 3. Let S be a point source and T be an extended source. Confirm that the
> statement "This check is not carried out on mod-8 sources" means that
> if T is a mod-8 source, then S should not be checked whether it is within T?
> --------------------------------------------------------------------
>5 arcsec radius circle.
>
> ----- vladimir 2010-05-23 --------------------------------------
> changing the algorithm to just create a map of extended source regions
>
>
#endif /* REQUIREMENTS */

int flagWithinExtendedSource(Task * task)
{
	int code = 0;
	int nx, ny;
	int i, il, iu;
	int j, jl, ju;
	int i1,j1;
	int ii, jj, jj1;
	
	static float auxImage[2048][2048];
	static float auxImage2[2048][2048];
	static float auxImage3[2048][2048];
	static float auxImage4[2048][2048];
	static float auxImageExt[2048][2048];
	static float auxImagePoint[2048][2048];
	float z,z1;
	float faver, naver;
	float gridAver, gridAver2, naver2;
	int ishift, ishift1,ishift2;
	int pointSourceSize;
	int size1, size2;
	int flag0, flag1, flag2;
	float dx, dy, dist;
	float fshift;
	float alpha, r;
	float threshold1, threshold2;
	float pixPerSource;
	int cleaningRadius;

	int s;
	Source * source;

	int gridStep;


	report_verbose("flagging extended source regions ... \n");

	ishift1=task->smoothSize;
	ishift2=2*ishift1;
	ishift=ishift1/2;
	fshift=(float)ishift;

	pointSourceSize=2*ishift1*ishift2;
	/*printf("L2927 ishift1=%d pointSourceSize=%d \n", ishift1, pointSourceSize);*/

	/*gridStep=200/task->binx;*/
	gridStep=ishift1;

	cleaningRadius=120/task->binx;

	/*printf("L2934 gridStep=%d cleaningradius=%d \n", gridStep, cleaningRadius);*/

	nx = task->rawImage->width - 1;
	ny = task->rawImage->height - 1;
	il = 0;
	iu = nx;
	jl = 0;
	ju = ny;

	faver=0.0;
	naver=0.0;
	for (j = jl; j <= ju; ++j) {      
	  for (i = il; i <= iu; ++i) {
	    z = fimage_get_relative(task->extendedImage, i, j);
	    auxImage2[i][j] = z;
	    auxImage3[i][j]=0.0;
	    auxImage4[i][j]=1.0; /* for the regions around very bright sources */
	    if (z>0.0){
	      faver+=z;
	      naver+=1.0;
	    }
	  }/* for i */
	}/* for j*/

	/*
	printf("L2937 dumping the extended (enhanced) image .......\n");
	code = writeFloatImage("extenh_image2map.fits", auxImage2);
	printf("L2939 ......... auxImage2 .\n");
	*/

	for (s = 0; s < task->nSources; ++s) {
	  source = &task->source[s];
	  /*
	  printf("L3028 id=%d x=%f y=%f counts=%f rate=%f \n", 
		 source->id, source->xImage, source->yImage, source->rawCounts, source->rawRate);
	  */
	  if (source->rawRate >60){
	    /*report_verbose("  cleaning the vicinity of the bright source .... \n");*/
	    ii=(int)source->xImage;
	    jj=(int)source->yImage;
	    for (i=ii-cleaningRadius; i<=ii+cleaningRadius;i++){
	      if (i>=0 && i<=nx){
		for (j=jj-cleaningRadius;j<=jj+cleaningRadius;j++){
		  if (j>=0 && j<= ny){
		    dx=(float)(i-ii);
		    dy=(float)(j-jj);
		    dist=sqrt(dx*dx+dy*dy);
		    if (dist<=cleaningRadius){
		      auxImage4[i][j]=-1;
		    }
		  }
		}/* for j */
	      }
	    } /* for i */
	  } /* if rawrate>60 */
	}
       
	/*
       	printf("L3051 dumping the very bright point source image .......\n");
	code = writeFloatImage("pnt0_image2map.fits", auxImage4);
	printf("L3053 ......... auxImage4 .\n");
	*/

	if (naver>1.0) faver/=naver;
	threshold1=faver+3.0*sqrt(faver);
	/*
	printf("L2945 faver=%f 075faver=%f threshold1=%f \n",faver, 0.75*faver, threshold1);
	*/

	gridAver2=0.0;
	naver2=0.0;
	/* Compute averages for the grid with the separation of gridStep pix */
	for (j = jl; j < ju; j+=gridStep) {      
	  for (i = il; i < iu; i+=gridStep) {
	    gridAver=0.0;
	    naver=0.0;
	    for (i1=i-ishift2*2;i1<=i+ishift2*2;i1+=2){
	      if (i1>=0 && i1<=nx){
		for (j1=j-ishift2*2;j1<=j+ishift2*2;j1+=2){
		  if (j1>=0 && j1<=ny){
		    if (auxImage2[i1][j1]>0.0){
		      gridAver+=auxImage2[i1][j1];
		      naver+=1.0;
		    }
		  }
		}
	      }
	    }
	    if (naver>1.0) gridAver/=naver;
	    /*printf("L2978 i=%d j=%d gridAver=%f \n", i,j,gridAver);*/

	    for (i1=i-gridStep/2;i1<=i+gridStep/2;i1++){
	      if (i1>=0 && i1<=nx){
		for (j1=j-gridStep/2;j1<=j+gridStep/2;j1++){
		  if (j1>=0 && j1<=ny){
		    if (gridAver>0.0)
		      auxImage3[i1][j1]=gridAver;
		    else 
		      auxImage3[i1][j1]=faver;
		  }
		}
	      }
	    }
	    
	    if (gridAver<threshold1){
	      gridAver2+=gridAver;
	      naver2+=1.0;
	    }
	  }/* for i */
	}/* for j */
	if (naver2>1.0) gridAver2/=naver2;
	threshold2=gridAver2+1.5*sqrt(gridAver2);

	/*
	printf("L3010 gridAver2=%f threshold2=%f \n", gridAver2, threshold2);
	*/

	/*
	printf("L3002 dumping the grid aver image .......\n");
	code = writeFloatImage("extenh3_image2map.fits", auxImage3);
	printf("L3004 ......... auxImage3 .\n");
	*/

	/* Create the point source map and remove point sources from auxImage */
	for (j = jl; j <= ju; ++j) {      
	  for (i = il; i <= iu; ++i) {
	    auxImagePoint[i][j]=faver;
	    auxImage[i][j]=auxImage2[i][j];
	    if (auxImage2[i][j]>0.0 && auxImage3[i][j]>0.0){
	      if ((auxImage2[i][j]>auxImage3[i][j]+6.0*sqrt(auxImage3[i][j]))
		  && auxImage4[i][j]>0){
		auxImagePoint[i][j]=-3.0;
		auxImage2[i][j]=faver;
		auxImage[i][j]=faver;
	      }
	    }
	  }/* for i */
	}/* for j*/

	/* auxImage now contains the enhanced image without point sources */
	/* create the extended source map*/
	for (j = jl; j <= ju; ++j) {      
	  for (i = il; i <= iu; ++i) {
	    if (auxImage2[i][j]>threshold2)
	      auxImage[i][j]=-3.0;	    
	  }
	}

	/*
	printf("L2968 dumping the extended (enhanced) image .......\n");
	code = writeFloatImage("extenh2_image2map.fits", auxImage);
	printf("L2970 ......... auxImage .\n");
	*/

	flag0=1;
	pixPerSource=(float)nx*(float)ny/(float)task->nSources;
	if (pixPerSource<1.0E5) flag0=2;
	/*
	printf("L2975 nSources=%d pixPerSource=%f \n", task->nSources, pixPerSource);
	*/

	/* Distinguish between extended and point sources */
	report_verbose("distinguishing between extended and point sources  ... \n");
	for (i = il; i <= iu; i++) {
	  for (j = jl; j <= ju; j++) {      
	    auxImageExt[i][j]=0.0;
	    /*auxImagePoint[i][j]=0.0;*/
	    if (auxImage[i][j]<0.0){

	      size1=1;
	      flag1=1;
	      /* vertical direction */
	      for (i1=i-ishift1*2;i1<=i+ishift1*2;i1+=2){
		if (i1>=0 && i1<=nx){
		  for (j1=j-ishift2*2;j1<=j+ishift2*2;j1+=2){
		    if (j1>=0 && j1<=ny){
		      if (auxImage[i1][j1]<0.0)
			size1+=1;
		    }
		    else
		      flag1=2;
		  }
		}
		else
		  flag1=2;
	      }

	      /* horizontal direction */
	      size2=1;
	      flag2=1;
	      for (i1=i-ishift2*2;i1<=i+ishift2*2;i1+=2){
		if (i1>=0 && i1<=nx){
		  for (j1=j-ishift1*2;j1<=j+ishift1*2;j1+=2){
		    if (j1>=0 && j1<=ny){
		      if (auxImage[i1][j1]<0.0)
			size2+=1;
		    }
		    else
		      flag2=2;
		    /* the threshold size is reduced at the edge of the image */
		  }
		}
		else
		  flag2=2;
		/* the threshold size is reduced at the edge of the image */
	      }
	      

	      if (size1>pointSourceSize/flag1 || size2>pointSourceSize/flag2){
		/* then extended source */
		auxImageExt[i][j]=auxImage[i][j];
		/*auxImagePoint[i][j]=faver;*/	
	      } 
	      else{
		/* point source */
		auxImageExt[i][j]=faver;
		/*auxImagePoint[i][j]=auxImage[i][j];*/
	      }
	    }
	    else{
	      /* nither point nor extended source */
	      auxImageExt[i][j]=auxImage[i][j];
	      /*auxImagePoint[i][j]=auxImage[i][j];*/
	    }
	  }/* for i */
	}/* for j*/

	/*
	printf("L3038 dumping the extended source image .......\n");
	code = writeFloatImage("ext_image2map.fits", auxImageExt);
	printf("L3040 ......... auxImageExt .\n");
	*/

	/*
	printf("L3042 nSources=%d \n", task->nSources);
	*/

	/* Clean the regions corresponding to the known point sources */
	/*
	for (s = 0; s < task->nSources; ++s) {
	  source = &task->source[s];
	  i=source->xImage;
	  j=source->yImage;
	  for (i1=i-2*ishift2;i1<=i+2*ishift2;i1++){
	    if (i1>=0 && i1<=nx){
	      for (j1=j-2*ishift2; j1<=j+2*ishift2;j1++){
		if (j1>=0 && j1<=ny){
		  dx=i-i1; dy=j-j1;
		  dist=sqrt(dx*dx+dy*dy);
		  if (dist<((float)2*ishift2)){
		    auxImageExt[i1][j1]=faver;
		  }
		}
	      }
	    }
	  }
	}
	*/
	/*
	printf("L3057 dumping the cleaned extended source image .......\n");
	code = writeFloatImage("ext2_image2map.fits", auxImageExt);
	printf("L3059 ......... auxImageExt .\n");
	*/

	report_verbose("connecting isolated pixels in the extended source map \n");
	/* Check the neighbour extended pixels */
	for (i = il; i <= iu; i++) {
	  for (j = jl; j <= ju; j++) {
	    auxImage[i][j]=auxImageExt[i][j];
	    if (auxImageExt[i][j]<0.0){

	      flag1=0;

	      /* Check whether the pixel is isolated */
      	      for (i1=i-1;i1<=i+ishift;i1++){
		if (i1>=0 && i1<=nx){
		  for (j1=j-1;j1<=j+1;j1++){
		    if (j1>=0 && j1<=ny){
		      if (i1 != i && j1 !=j){
			if (auxImageExt[i1][j1]>=0.0){
			  flag1+=1;
			}
		      }
		    }
		  }
		}
	      }
	      if (flag1>6){
		/* then the pixels is asolated */
		/* connect it with the neighbour negative pixels */
		for (ii=0;ii<360;ii+=5){
		  alpha=(float)ii;
		  flag2=0;
		  jj1=0;
		  for (jj=1;jj<ishift;jj++){
		    r=(float)jj;
		    dx=r*cos((alpha/2.0)/M_PI)+0.5;
		    dy=r*sin((alpha/2.0)/M_PI)+0.5;
		    i1=i+(int)dx;
		    j1=j+(int)dy;
		    if (i1>=0 && i1<=nx && j1>=0 && j1<=ny){
		      if (auxImageExt[i1][j1]<0.0){
			flag2=1;
			jj1=jj;
		      }
		    }
		  } /* for jj */

		  if (flag2>0 && jj1>0){
		    /* fill in the radius */
		    for (jj=1;jj<=jj1;jj++){
		      r=(float)jj;
		      dx=r*cos((alpha/2.0)/M_PI)+0.5;
		      dy=r*sin((alpha/2.0)/M_PI)+0.5;
		      i1=i+(int)dx;
		      j1=j+(int)dy;
		      if (i1>=0 && i1<=nx && j1>=0 && j1<=ny)
			auxImage[i1][j1]=-3.0;
		    } /* for jj */
		  }

		}/* for ii */
	      }/* if flag1>6 */


	    }
	  }/* for i */
	}/* for j*/


	/*
	printf("L2994 dumping the point source image .......\n");
	code = writeFloatImage("pnt1_image2map.fits", auxImagePoint);
	printf("L2996 ......... auxImagePoint .\n");
	*/


	/* Pass the original-image pixels within the point-source map to the output smap */
	report_verbose("Ammending the output point-source map \n");
	for (i = il; i <= iu; ++i) {
	  for (j = jl; j <= ju; ++j) {
	    if (auxImagePoint[i][j]<0.0){
	      z = fimage_get_relative(task->rawImage, i, j);
	      z1 = fimage_get_relative(task->smapImage, i, j);
	      z=FMAX(z,z1);
	      fimage_set_relative(task->smapImage, i, j, z);
	    }
	  }
	}


	/* place the extended flag into the quality map */
	report_verbose("transferring the extended flag to the quality map  ... \n");
	for (i = il; i <= iu; i++) {
	  for (j = jl; j <= ju; j++) {
	    if (auxImage[i][j]<0){
	      flagPixel(task, i, j,FLAG_WITHIN_EXTENDED_SOURCE);
	      flagPixel2(task, i, j,FLAG_WITHIN_EXTENDED_SOURCE);
	    }
	  }
	}


	/* clean the source regions in the second quality map */
	for (s = 0; s < task->nSources; ++s) {
	  source = &task->source[s];
	  flagSourcePixels2(task, source, 0);
	}
	report_verbose("finished flagging extended sources  \n");
	return code;
}

#ifdef REQUIREMENTS
>**************************************************
>Weird source (bit 9)
>*************************************************
>
>A check is carried out in omdetect for a source centred
>on a very few bright (hot) pixels
>
>The following algorithm basically just checks the pixels
>in a box centred on the centre of the source
>
>Initialise

dx = 20 / binx (binx is the binning factor)

il = i0 - dx
jl = j0 - dx

il = max(0_int16, il)
jl = max(0_int16, jl)

il = min(nx1, il)
jl = min(ny1, jl)

iu = i0 + dx
ju = j0 + dx

iu = max(0_int16, iu)
ju = max(0_int16, ju)

iu = min(nx1, iu)
ju = min(ny1, ju)

> Count how many pixels within the box have a value > 65000 (count1)

> Count how many pixels within the box have a value > 5000 (count2)

!**************************************************************************
! If one or two pixels with a value greater than 65000 , and no others
! have a value > 5000 then conclude anomalous pixels and flag the source
!***************************************************************************

if ( count1 > 0 .and. count1 < 3 .and. count2 == 0 ) then

set the source flag

end if

> --- bob ------------------------------------------------------------
> count2 is poorly described since value > 65000 implies value > 5000.
> The subsequent comment provides a meaningful requirement.
> --------------------------------------------------------------------

#endif /* REQUIREMENTS */

int flagWeirdSources(Task * task)
{
	int code = 0;
	int s, i, j, il, iu, jl, ju;
	int count1, count2;
	Source * source;

	const int dx = 20 / task->binx;

	report_verbose("flagging weird sources \n");

	for (s = 0; s < task->nSources; ++s) {
		source = &task->source[s];

		il = apply_range(source->xImage - dx, 0, task->rawImage->width - 1);
		jl = apply_range(source->yImage - dx, 0, task->rawImage->height - 1);

		iu = apply_range(source->xImage + dx, 0, task->rawImage->width - 1);
		ju = apply_range(source->yImage + dx, 0, task->rawImage->height - 1);

		count1 = 0;
		count2 = 0;

		for (i = il; i <= iu; ++i) {
			for (j = jl; j <= ju; ++j) {
				int z;
				z = fimage_get_relative(task->rawImage, i, j);
				if (z > 65000)
					++count1;
				else if (z > 5000)
					++count2;
			}
		}

		if (count1 > 0 && count1 < 3 && count2 == 0)
		{
			if (task->flagImage)
				flagSourcePixels(task, source, FLAG_WEIRD_SOURCE);
			if (task->qmapImage)
				flagSourcePixels2(task, source, FLAG_WEIRD_SOURCE);
		}
	}

	return code;
}

#ifdef REQUIREMENTS
************************************************************************************
CONFUSION FLAGS
***********************************************************************************

>Each source can have up to 2 confusion flag settings, which are stored in
>an integer 8 variable. The flags are as follows:

!**********************************************************************************
! CFLAG source confusion flags convention
! BIT NO FLAGGING Integer
!
! 0 one or more source within 0.5-1.0 aperture radius 1
! 1 one or more source within 0.5 aperture radius 2
!************************************************************************************

>These flags are set for "point" sources by omdetect when the photometry is performed.
>
>Basically, for each source the distance to its nearest neighbour point source is computed.
>If this distance is less than 0.5 aperture radius flag 1 is set.
>
>If this distance is 0.5 to 1 aperture radii, bit 0 is set.

#endif /* REQUIREMENTS */

int flagConfusion(Task * task)
{
	int code = 0;
	int s;
	Source * source;

	report_verbose("flagging confusing sources \n");

	for (s = 0; s < task->nSources; ++s) {
		source = &task->source[s];

		if (task->flagDetections) {
			if (source->isPointSource)
			/*
			 flagSourceAndPixels(task, source, FLAG_POINT_SOURCE);
			 */
			{
				if (task->flagImage)
					flagSourcePixels(task, source, FLAG_POINT_SOURCE);
			}
			else
			/*
			 flagSourceAndPixels(task, source, FLAG_EXTENDED_SOURCE);
			 */
			{
				if (task->flagImage)
					flagSourcePixels(task, source, FLAG_EXTENDED_SOURCE);
			}
		}
	}

	return code;
}


int flagAllQuality(Task * task)
{
	int code = 0;
	FlagRoutine *routine;
	FlagRoutine routines[] = {
		&flagBadPixels,
		&flagReadoutStreaksAlgo2,
		&flagReadoutStreaksAlgo1,
		&flagSmokeRings,
		&flagDiffractionSpikes,
		&flagHaloRings,
		&flagStrongMod8Pattern,
		&flagNearBrightSource,
		&flagNearImageEdge,
		/* vny 2010/05/09: flagging "within extended sources" will not work here */
		/* because the source list does not containg extended sources */
		/* This flagging is better to postpone to after uvotdetect */
		&flagWithinExtendedSource,
		&flagWeirdSources,
		/*&flagConfusion,*/
		0
	};

	routine = &routines[0];
	while (*routine) {
		code = (*routine)(task);
		++routine;
	}

	return code;
}

