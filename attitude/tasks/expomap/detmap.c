/*
 * $Source: /headas/headas/attitude/tasks/expomap/detmap.c,v $
 * $Revision: 1.2 $
 * $Date: 2004/10/17 13:07:41 $
 *
 *
 * $Log: detmap.c,v $
 * Revision 1.2  2004/10/17 13:07:41  rwiegand
 * Use quadrilaterals instead of polygons for computing overlaps.
 *
 * Revision 1.1  2003/09/04 17:36:07  rwiegand
 * Allow creating exposure maps in detector coordinates.
 *
 */

#include <math.h>

#include "mapper.h"
#include "teldef.h"
#include "expomap.h"
#include "genimage.h"
#include "overlap.h"
#include "report.h"


#define ARRAY_ELEMENTS(v) (sizeof(v) / sizeof(v[0]))



typedef struct
{
	ExposureMapper * mapper;
	int x, y;
} ExposureIncrement;



static void
increment_det_exposure (int x, int y, OverlapState * state)
{
	int code = 0;
	ExposureIncrement * increment = (ExposureIncrement *) state->user;
	ExposureMapper * mapper = increment->mapper;
	FImage * instmap = mapper->instrument;
	FImage * expmap = mapper->exposure;
	float instz, expz;

	code = fimage_get(instmap, x, y, &instz);
	if (!code)
		{
			float delta = instz * state->weight;
			code = fimage_get(expmap, increment->x, increment->y, &expz);
			if (!code)
				fimage_set(expmap, increment->x, increment->y, expz + delta);
		}
}


	int code = 0;
static int
iterate_det_exposure (FImage * expmap, IIState * state)
{
	int code = 0;

	/* determine all of the instmap pixels which affect this exposure pixel */
	ExposureMapper * mapper = (ExposureMapper *) state->user;
	Quad qexp, qinst;

	/* corners of current pixel (in exposure map) */
	qexp.a.x = state->x - 0.5;   /* lower left */
	qexp.a.y = state->y - 0.5;
	qexp.b.x = state->x + 0.5;   /* lower right */
	qexp.b.y = state->y - 0.5;
	qexp.c.x = state->x + 0.5;   /* upper right */
	qexp.c.y = state->y + 0.5;
	qexp.d.x = state->x - 0.5;   /* upper left */
	qexp.d.y = state->y + 0.5;

	/* quadrilateral qinst gets corners of current pixel transformed from
	 * exposure map to instrument map */
	applyXform2dToContinuousCoords(mapper->exp2inst,
				&qinst.a.x, &qinst.a.y, qexp.a.x, qexp.a.y);

	applyXform2dToContinuousCoords(mapper->exp2inst,
				&qinst.b.x, &qinst.b.y, qexp.b.x, qexp.b.y);

	applyXform2dToContinuousCoords(mapper->exp2inst,
				&qinst.c.x, &qinst.c.y, qexp.c.x, qexp.c.y);

	applyXform2dToContinuousCoords(mapper->exp2inst,
				&qinst.d.x, &qinst.d.y, qexp.d.x, qexp.d.y);

	{
		/* apply their contributions to this exposure pixel */
		ExposureIncrement tmp = { 0 };
		tmp.mapper = mapper;
		tmp.x = state->x;
		tmp.y = state->y;
		iterate_overlap(&qinst, increment_det_exposure, &tmp);
	}

	return code;
}


int
create_det_exposure_map (ExposureMapper * mapper)
{
	int code = 0;
	FImage * image;
	COORDDEF * coord;
	ImageWCS * wcs;

	/*
	* XFORM2D inst2tel goes from instrument map pixels to teldef DET pixels,
	* XFORM2D tel2exp goes from teldef DET pixels to exposure map pixels.
	* About any COORDDEF would work, we just need some intermediate system.
	*/
	image = mapper->instrument;
	wcs = image->wcs;
	coord = mapper->teldef->det[mapper->teldef->sky_from];

	setImageToCoordDefXform2D(mapper->inst2tel, coord, image->binx, image->biny,
			coord->npixels_x / 2. + 0.5 - image->binx * (wcs->xpix + 0.5),
			coord->npixels_y / 2. + 0.5 - image->biny * (wcs->ypix + 0.5));

	image = mapper->exposure;
	wcs = image->wcs;
	setCoordDefToImageXform2D(mapper->tel2exp, coord, image->binx, image->biny,
			coord->npixels_x / 2. + 0.5 - image->binx * (wcs->xpix + 0.5),
			coord->npixels_y / 2. + 0.5 - image->biny * (wcs->ypix + 0.5));

	combineXform2ds(mapper->inst2exp, mapper->inst2tel, mapper->tel2exp);
	invertXform2d(mapper->exp2inst, mapper->inst2exp);

	{
		IIState state = { 0 };
		state.user = mapper;
		code = fimage_iterate(image, iterate_det_exposure, &state);
	}

	return code;
}


