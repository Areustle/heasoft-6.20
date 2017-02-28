#ifndef XRSIMAGE_XRS_CORNERCOORD_INCLUDED

#include "atFunctions.h"
#include "teldef.h"

/******************************************************************
* Xrsimage assumes that xrscoord has been applied to the input event
* file; X (RA), Y (Dec), ROLL are read from fits table, which should be
* calculated by xrscoord.  You should re-run xrscoord when you need to
* change the center of image, pointing, etc.  Note that xrsimage just
* make image fits according to the information in the input event file.
* In the current version of xrsimage, calibration pixel or dead pixel
* is not treated appropreately.  This problem should be fixed in future
* version.
******************************************************************/

/* enum { DET = 0, FOC = 1, SKY = 2 }; */

/******************************************************************
* xrs_cornercoord is used when iamge_type is DET or FOC.
* This routine calculates the corner pixels with teldef file,
******************************************************************/

int xrs_cornercoord (TELDEF *teldef, int pixel, int type, 
			  double *x, double *y);


/******************************************************************
* xrs_skycornercoord is used for image_type is SKY.
* In this routine, corner pixels are calculated
* with X, Y and ROLL parameters written in the input event file:
* 
* The output image in SKY coordinate may NOT be accurate.  One reason is
* xrsimage uses X, Y and ROLL values, which were truncated to INTEGER.
* Xrsimage assumes image of each XRS pixel is square (or rectangle)
* similar to FOC pixel, which is not correct if detector is located far
* from the center of a whole image.
******************************************************************/

int xrs_skycornercoord (TELDEF *teldef, int pixel, double centx, 
			double centy, double roll, double x[], double y[]);

#define XRSIMAGE_XRS_CORNERCOORD_INCLUDED
#endif /* XRSIMAGE_XRS_CORNERCOORD_INCLUDED */

