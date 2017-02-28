/*
 * $Source: /headas/headas/attitude/tasks/expomap/mapper.c,v $
 * $Revision: 1.7 $
 * $Date: 2003/09/11 18:46:12 $
 *
 *	based on Chandra mkexpomap.c
 *
 * $Log: mapper.c,v $
 * Revision 1.7  2003/09/11 18:46:12  rwiegand
 * Bug fix: failing to initialize exposure map.
 *
 * Revision 1.6  2003/09/04 18:30:03  rwiegand
 * Had index wrong on one of the offsets.
 *
 * Revision 1.5  2003/09/04 17:36:07  rwiegand
 * Allow creating exposure maps in detector coordinates.
 *
 * Revision 1.4  2003/07/18 21:24:27  rwiegand
 * Transform from SKY_FROM DET level to SKY instead of DET level 0.  Ripped out
 * a bunch of printf statements.
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
 * Revision 1.1  2003/05/12 14:16:33  rwiegand
 * Generic exposure map generator.
 * Given an instrument map, good time interval(s) and attitude information,
 * creates an exposure map.
 *
 * Revision 1.1  2003/05/02 18:24:17  rwiegand
 * Exposure map generator
 *
 */


#include <math.h>

#include "mapper.h"
#include "teldef.h"
#include "quat.h"
#include "expio.h"
#include "expomap.h"
#include "report.h"



int
apply_delta_to_exposure (ExposureMapper * mapper, ExposureDelta * delta)
{
	int code = 0;
	int i, j;

	for (j = delta->ymin; j <= delta->ymax; ++j)
		{
			for (i = delta->xmin; i <= delta->xmax; ++i)
				{
					float value;
					double xinst, yinst;
					int xi, yi;
					int test;

					/* applyXform2dToDiscreteCoords with random dx, dy?  */
					applyXform2dToContinuousCoords(mapper->exp2inst, &xinst, &yinst,
													i, j);
					xi = (int) floor(xinst + 0.5);
					yi = (int) floor(yinst + 0.5);

					test = fimage_get(mapper->instrument, xi, yi, &value);
					if (!test && (value > 0.0))
						{
							double z = fimage_get_absolute(mapper->exposure, i, j);
							z += delta->duration * value;
							fimage_set_absolute(mapper->exposure, i, j, z);
						}
				}
		}

	return code;
}


/*
 * Find the extreme exposure map pixels affected at the current rotation.
 * Requires inst2exp.
 */
int
get_sky_limits (ExposureMapper * mapper, ExposureDelta * delta)
{
	int code = 0;

	int i;
	double xmin, xmax, ymin, ymax;
	double offset[4][2] = { { 0 } };
	FImage * image = mapper->instrument;

	xmin = 1e36;
	xmax = -1e36;
	ymin = 1e36;
	ymax = -1e36;

	offset[0][0] = 1;               /* detector lower left */
	offset[0][1] = 1;
	offset[1][0] = 1;               /* detector upper left */
	offset[1][1] = image->height;
	offset[2][0] = image->width;    /* detector upper right */
	offset[2][1] = image->height;
	offset[3][0] = image->width;    /* detector lower right */
	offset[3][1] = 1;

	for (i = 0; i < 4; ++i)
		{
			double xsky, ysky;

			applyXform2dToContinuousCoords(mapper->inst2exp, &xsky, &ysky,
											offset[i][0], offset[i][1]);

			if (xsky < xmin)
				xmin = xsky;
			if (xsky > xmax)
				xmax = xsky;
			if (ysky < ymin)
				ymin = ysky;
			if (ysky > ymax)
				ymax = ysky;
		}

	delta->xmin = (int) xmin;
	delta->xmax = (int) xmax;
	delta->ymin = (int) ymin;
	delta->ymax = (int) ymax;

	report_verbose("get_sky_limits => %d < x < %d; %d < y < %d\n",
								delta->xmin, delta->xmax, delta->ymin, delta->ymax);

	return code;
}


static void
determine_transform (ExposureMapper * mapper, ExposureDelta * delta)
{
	double tmp;
	double v, vhat[3];

	TELDEF * teldef = mapper->teldef;
	FImage * image;
	ImageWCS * wcs;
	COORDDEF * coord = teldef->det[0];

	if (mapper->aberration)
		{
			mapper->aberration = 0;
			report_warning("velocity aberration not implemented\n");
		}

	v = 0;
	/* prime teldef for quaternion, det2sky (add flag for aberration?) */
	convertDetectorToSkyUsingTeldef(mapper->teldef, &tmp, &tmp,
								coord->center_x, coord->center_y, delta->quat, v, vhat);

	/*
		* XFORM2D inst2tel goes from instrument map pixels to teldef DET pixels,
		* XFORM2D tel2exp goes from teldef SKY pixels to exposure map pixels.
		* (center/crval corresponds to boresight)
		*/
 	image = mapper->instrument;
 	wcs = image->wcs;
 	coord = teldef->det[teldef->sky_from];

	setImageToCoordDefXform2D(mapper->inst2tel, coord, image->binx, image->biny,
			coord->npixels_x / 2. + 0.5 - image->binx * (wcs->xpix + 0.5),
			coord->npixels_y / 2. + 0.5 - image->biny * (wcs->ypix + 0.5));

 	image = mapper->exposure;
	wcs = image->wcs;
	coord = teldef->sky;
	setCoordDefToImageXform2D(mapper->tel2exp, coord, image->binx, image->biny,
			coord->npixels_x / 2. + 0.5 - image->binx * (wcs->xpix + 0.5),
			coord->npixels_y / 2. + 0.5 - image->biny * (wcs->ypix + 0.5));

	combineXform2ds(mapper->alpha, mapper->inst2tel, teldef->det2sky);
	combineXform2ds(mapper->inst2exp, mapper->alpha, mapper->tel2exp);
	invertXform2d(mapper->exp2inst, mapper->inst2exp);
}


int
apply_gti_to_exposure (ExposureMapper * mapper, GTI * gti,
				ExposureDelta * delta)
{
	int code = 0;
	ATTITERATOR * attiter = mapper->attiter;

	setAttIteratorInterval(attiter, gti->start, gti->stop);

	while (!code && moreAttIteratorSteps(attiter))
		{
			delta->duration = getAttIteratorValues(attiter, delta->quat);

			nextAttIteratorStep(attiter);

			determine_transform(mapper, delta);

			code = get_sky_limits(mapper, delta);

			if (!code)
				code = apply_delta_to_exposure(mapper, delta);
		}

	return code;
}


int
create_sky_exposure_map (ExposureMapper * mapper)
{
	int code = 0;
	int i;
	ExposureDelta delta = { 0 };

	delta.quat = allocateQuat();

	for (i = 0; !code && (i < mapper->gtis); ++i)
		code = apply_gti_to_exposure(mapper, mapper->interval + i, &delta);

	destroyQuat(delta.quat);

	return code;
}


int
initialize_exposure_map (ExposureMapper * mapper, double cx, double cy,
		int width, int height)
{
	int code = 0;

	mapper->exposure->offset = 1;
	mapper->instrument->offset = 1;

	if (!code)
		{
			FImage * image = mapper->exposure;
			code = fimage_allocate(image, width, height);
			report_verbose("allocated exposure width=%d, height=%d\n",
							width, height);

			fimage_set_constant(image, 0);
		}

	if (!code)
		{
			TELDEF * teldef = mapper->teldef;
			FImage * image = mapper->exposure;
			ImageWCS * wcs = image->wcs;

 			if (mapper->is_det)
 				{
					COORDDEF * coord = teldef->det[0];

 					wcs->xpix = (width + 1) / 2. - cx / coord->scale_x;
 					wcs->ypix = (height + 1) / 2. - cy / coord->scale_y;
 					wcs->xval = 0; /* boresight */
 					wcs->yval = 0;
 					wcs->xinc = image->binx * coord->scale_x;
 					wcs->yinc = image->biny * coord->scale_y;
 					wcs->rot = 0;
 					sprintf(wcs->xnam, "DETX");
 					sprintf(wcs->ynam, "DETY");
 					sprintf(wcs->unit, "mm");
 				}
 			else
 				{
					COORDDEF * coord = teldef->sky;

 					setSkyCoordCenterInTeldef(mapper->teldef, cx, cy);
 
 					wcs->xpix = (width + 1) / 2.;
 					wcs->ypix = (height + 1) / 2.;
 					wcs->xval = cx;
 					wcs->yval = cy;
 					wcs->xinc = -image->binx * coord->scale_x;
 					wcs->yinc = image->biny * coord->scale_y;
 					wcs->rot = 0;
 					sprintf(wcs->xnam, "RA--");
 					sprintf(wcs->ynam, "DEC-");
 					sprintf(wcs->type, "-TAN");
 				}

		}

	if (!code)
		{
			mapper->inst2exp = allocateXform2d();
			mapper->exp2inst = allocateXform2d();
			mapper->inst2tel = allocateXform2d();
			mapper->tel2exp = allocateXform2d();
			mapper->alpha = allocateXform2d();
		}

	return code;
}


int
release_mapper (ExposureMapper * mapper)
{
	int code = 0;

	if (mapper->teldef)
		destroyTelDef(mapper->teldef);

	if (mapper->attiter)
		destroyAttIterator(mapper->attiter);

	if (mapper->attfile)
		closeAttFile(mapper->attfile);

	if (mapper->exposure)
		fimage_release(mapper->exposure);

	if (mapper->instrument)
		fimage_release(mapper->instrument);

	if (mapper->interval)
		free(mapper->interval);

	if (mapper->inst2exp)
		destroyXform2d(mapper->inst2exp);

	if (mapper->exp2inst)
		destroyXform2d(mapper->exp2inst);

	if (mapper->inst2tel)
		destroyXform2d(mapper->inst2tel);

	if (mapper->tel2exp)
		destroyXform2d(mapper->tel2exp);

	if (mapper->alpha)
		destroyXform2d(mapper->alpha);

	return code;
}


int
set_total_exposure_time (ExposureMapper * mapper)
{
	int code = 0;
	int i;
	double exptime = 0;

	/* find the total exposure time */
	for (i = 0; i < mapper->gtis; ++i)
		{
			const GTI * gti = mapper->interval + i;
			exptime += gti->stop - gti->start;
		}

	mapper->exptime = exptime;

	return code;
}

