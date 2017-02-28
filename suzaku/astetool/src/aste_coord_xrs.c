/* $Id: aste_coord_xrs.c,v 1.12 2007/05/07 16:45:32 ishisaki Exp $ */
/************************************************************************
  aste_coord_xrs.c

  xrs_pixel2det		convert XRS PIXEL to DETX/Y[ch]
  xrs_det2pixel		convert XRS DETX/Y[ch] to PIXEL

  1999-12-19	Y.ISHISAKI
************************************************************************/

#include <stdio.h>
#include <math.h>
#include "atFunctions.h"
#include "aste_coord.h"

/* Bilinear Array Pixel Coordinates
(http://heasarc.gsfc.nasa.gov/docs/frames/astroe_prop_tools.html)

Here are the pixel coordinates of the flight bilinear array
determined with a coordinate measurement microscope. The
data are "raw" and have not been corrected in any way. To a
very good approximation the x and y axes used for these
measurements will correspond to the Astro-E spacecraft axes. It
will take a lot of additional work to get the true orientation of the
array with respect to the spacecraft. We have to determine the
transformation matrices between the detector and the detector
assembly, the detector assembly and the He Insert, the He
Insert and the Neon dewar, and finally the the Neon dewar and
the spacecraft. It will thus not be until mid-late next year that we
have all of this information. Nonetheless, I think the machining
tolerances on all of the components are sufficiently good that
these numbers can be used for essentially all mission planning
activities. The ordering of the pixels below corresponds to the
numbering determined by the flight electronics and their location
on the array. It will become obvious once the data are plotted.

 Pixel   Corner       x(mm)     y(mm)

 15       1          0.000     0.000
          2          1.221     0.002
          3          1.223     0.316
          4          0.000     0.316

 14       1          0.000     0.336
          2          1.225     0.332
          3          1.225     0.648
          4          0.002     0.644

 13       1         -0.012     0.670
          2          1.221     0.668
          3          1.221     0.982
          4         -0.010     0.986

 12       1         -0.010     1.000
          2          1.225     1.000
          3          1.227     1.322
          4         -0.010     1.322

 11       1         -0.010     1.340
          2          1.223     1.338
          3          1.223     1.656
          4          0.000     1.650

 10       1         -0.010     1.678
          2          1.221     1.674
          3          1.223     1.994
          4         -0.008     1.992

 9        1         -0.004     2.010
          2          1.225     2.008
          3          1.227     2.324
          4         -0.008     2.332

 8        1         -0.004     2.340
          2          1.225     2.344
          3          1.225     2.664
          4         -0.006     2.658

 16       1         -0.006     2.678
          2          1.225     2.678
          3          1.229     2.994
          4         -0.006     2.998

 17       1         -0.004     3.016
          2          1.231     3.012
          3          1.235     3.330
          4          0.000     3.330

 18       1          0.006     3.348
          2          1.233     3.348
          3          1.231     3.663
          4          0.006     3.662

 19       1          0.004     3.678
          2          1.231     3.674
          3          1.233     3.991
          4          0.004     3.997

 20       1         -0.006     4.003
          2          1.231     4.006
          3          1.231     4.325
          4         -0.012     4.324

 21       1          0.004     4.341
          2          1.229     4.347
          3          1.235     4.670
          4         -0.002     4.665

 22       1          0.006     4.679
          2          1.231     4.686
          3          1.231     5.005
          4          0.002     5.005

 23       1          0.004     5.015
          2          1.235     5.021
          3          1.235     5.343
          4          0.004     5.337

 7        1          1.235    -0.122
          2          2.467    -0.120
          3          2.469     0.198
          4          1.237     0.192

 6        1          1.237     0.212
          2          2.469     0.212
          3          2.469     0.538
          4          1.245     0.532

 5        1          1.237     0.546
          2          2.475     0.546
          3          2.475     0.864
          4          1.237     0.864

 4        1          1.237     0.884
          2          2.473     0.884
          3          2.473     1.200
          4          1.239     1.198

 3        1          1.239     1.216
          2          2.467     1.216
          3          2.467     1.536
          4          1.239     1.536

 2        1          1.239     1.548
          2          2.473     1.552
          3          2.473     1.870
          4          1.235     1.870

 1        1          1.235     1.888
          2          2.471     1.888
          3          2.471     2.204
          4          1.239     2.204

 0        1          1.241     2.220
          2          2.473     2.220
          3          2.471     2.540
          4          1.239     2.540

 24       1          1.239     2.554
          2          2.465     2.564
          3          2.465     2.874
          4          1.243     2.872

 25       1          1.243     2.892
          2          2.483     2.892
          3          2.487     3.211
          4          1.247     3.211

 26       1          1.247     3.223
          2          2.479     3.232
          3          2.479     3.543
          4          1.241     3.543

 27       1          1.241     3.566
          2          2.473     3.566
          3          2.473     3.885
          4          1.241     3.885

 28       1          1.241     3.898
          2          2.475     3.898
          3          2.473     4.217
          4          1.243     4.212

 29       1          1.243     4.224
          2          2.477     4.231
          3          2.477     4.546
          4          1.251     4.546

 30       1          1.251     4.567
          2          2.473     4.567
          3          2.473     4.887
          4          1.249     4.885

 31       1          1.251     4.903
          2          2.477     4.903
          3          2.477     5.225
          4          1.247     5.225
 _______________________________________
 Richard L. Kelley
 Code 662
 Laboratory for High Energy Astrophysics
 NASA/Goddard Space Flight Center
 Greenbelt, MD  20771   USA
 301.286.7266
 1.888.200.2597 (Pager)

 Richard.L.Kelley.1@gsfc.nasa.gov

 Go XRS!
*/


/************************************************************************
  xrs_pixel2det		convert XRS PIXEL to DETX/Y[ch]

  INPUT
	corner 0:center, 1:left-low, 2:right-low, 3:right-up, 4:left-up

  RETURN
	ASTE_COORD_INVALID_PIXEL: invalid pixel number
************************************************************************/
int
xrs_pixel2det(
	TELDEF *teldef,		/* input: teldef file of XRS */
	int pixel,			/* input: pixel number [0-31] */
	int corner,			/* input: position in pixel 0:center, 1-4:corner */
	double *detx_ch,	/* output: DETX [ch] */
	double *dety_ch		/* output: DETY [ch] */
)
{
	static char pname[] = "xrs_pixel2det";
	double pixelx, pixely;

	TELDEF_ASTROE *p = teldef->mission.aste;
	struct xrs_pixel_map *p_map = &p->pixel_map[pixel];

	if ( ASTE_XRS_SENSOR != teldef->id ) {
		fprintf(stderr, "\
%s: TELDEF is not XRS (%d)\n", pname, teldef->id);
		return ASTE_COORD_INVALID_SENSOR;
	}

	if ( pixel < 0 || p->seg_num <= pixel ) {
		fprintf(stderr, "\
%s: invalid pixel number of PIXEL=%d\n", pname, pixel);
		return ASTE_COORD_INVALID_PIXEL;
	}

	switch (corner) {
	case 1: case 2: case 3: case 4:	/* pixel corner */
		pixelx = p_map->x[corner-1];
		pixely = p_map->y[corner-1];
		break;
	default:						/* pixel center */
		pixelx = p_map->xc;
		pixely = p_map->yc;
	}

	*detx_ch = p->det.xcen
			+ p->detxflip * (pixelx - p->int_xcen - p->det_xoff) / p->det_scal;
	*dety_ch = p->det.ycen
			+ p->detyflip * (pixely - p->int_ycen - p->det_yoff) / p->det_scal;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  xrs_det2pixel		convert XRS DETX/Y[ch] to PIXEL

  OUTPUT
	int *pixel: XRS pixel number, but -1 outside of the pixels
************************************************************************/
int
xrs_det2pixel(
	TELDEF *teldef,		/* input: teldef file of XRS */
	double detx_ch,		/* input: DETX [ch] */
	double dety_ch,		/* input: DETY [ch] */
	int *pixel			/* output: pixel number [0-31], or -1: out of pixel */
)
{
	static char pname[] = "xrs_det2pixel";
	int i, j;
	double pixelx, pixely;

	TELDEF_ASTROE *p = teldef->mission.aste;
	int seg_num = p->seg_num;

	if ( ASTE_XRS_SENSOR != teldef->id ) {
		fprintf(stderr, "\
%s: TELDEF is not XRS (%d)\n", pname, teldef->id);
		return ASTE_COORD_INVALID_SENSOR;
	}

	pixelx = p->int_xcen + p->det_xoff
		+ p->detxflip * p->det_scal * (detx_ch - p->det.xcen);
	pixely = p->int_ycen + p->det_yoff
		+ p->detyflip * p->det_scal * (dety_ch - p->det.ycen);

	for (i = 0; i < seg_num; i++) {
		for (j = 0; j < 4; j++) {
			struct xrs_pixel_map *p_map = &p->pixel_map[i];
			double x0 = p_map->x[j];
			double y0 = p_map->y[j];
			double x1 = p_map->x[(j+1)%4];
			double y1 = p_map->y[(j+1)%4];
			double flag = (x1-x0)*(pixely-y0) - (y1-y0)*(pixelx-x0);

			if ( flag < 0.0 ) {
				goto outside;
			}
		}

	inside:
		*pixel = i;
		return ASTE_COORD_NORMAL_END;

	outside:
		;
	}

	*pixel = -1;
	return ASTE_COORD_NORMAL_END;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
