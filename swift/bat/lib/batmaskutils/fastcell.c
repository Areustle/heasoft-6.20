#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "batdet.h"
#include "headas.h"

/* 
 * Fast conversion of polygon to weights 
 *
 * Quadrilateral defined by (x[i],y[i]) is rendered into array
 * detplane, which is an nx column by ny row image array.  The pixels
 * are assigned a weight.
 * 
 * The coordinates of the image origin are in cell0, and the sizes of
 * the pixel in X and Y are in cellsize.
 *
 * NOTE: this routine, which much faster than detbresimg(), which
 * operates by subpixelation, is potentially of lower fidelity.  It
 * assumes that there are no detector-to-detector variations, no
 * imperfections like high voltage grids or glue dots, and perfectly
 * parallel geometry.  The last assumption means that there are no
 * twists or tilts in the mask-to-DAP alignment (for which there is
 * presently no evidence).
 *
 * double x[4], y[4] - X and Y corners of quadrilateral;
 * DETFLOAT *detplane - image, nx rows by ny columns;
 * int nx, ny - rows and columns of detplane;
 * double cellsize[2] - size of pixel in X and Y;
 * double cell0[0] - coordinates of corner of [0][0] pixel of detplane
 * DETFLOAT weight - weight to assign polygon
 *
 * RETURNS: 0 for success
 *
 * $Id: fastcell.c,v 1.1 2004/05/24 02:04:21 craigm Exp $ */

int fastcell(double x[4], double y[4], 
	     DETFLOAT *detimg, int nx, int ny, 
	     struct batdetplane_struct *detplane,
	     double cell0[2], DETFLOAT weight)
{
  double xmin = x[0], xmax = x[0], ymin = y[0], ymax = y[0];
  int imin, imax, jmin, jmax;
  double xdetsize, ydetsize;
  double xcellsize, ycellsize;
  double pwt;
  int i, j;

  xdetsize = detplane->detsize[0];
  ydetsize = detplane->detsize[1];
  xcellsize = detplane->cellsize[0];
  ycellsize = detplane->cellsize[1];

  for (i=1; i<4; i++) {
    if (x[i] < xmin) { xmin = x[i]; }
    if (x[i] > xmax) { xmax = x[i]; }
    if (y[i] < ymin) { ymin = y[i]; }
    if (y[i] > ymax) { ymax = y[i]; }
  }

  /* Check to see if polygon is even inside the field of view */
  if (xmin >= (cell0[0]+xcellsize*nx)) return 0;
  if (xmax <   cell0[0]) return 0;
  if (ymin >= (cell0[1]+ycellsize*ny)) return 0;
  if (ymax <   cell0[1]) return 0;

  /* Convert to cell numbers in the output array */
  imin = floor((xmin-cell0[0]) / xcellsize);
  imax = ceil ((xmax-cell0[0]) / xcellsize);
  jmin = floor((ymin-cell0[1]) / ycellsize);
  jmax = ceil ((ymax-cell0[1]) / ycellsize);
  imin = ((xmin-cell0[0]) / xcellsize);
  imax = ((xmax-cell0[0]) / xcellsize);
  jmin = ((ymin-cell0[1]) / ycellsize);
  jmax = ((ymax-cell0[1]) / ycellsize);

  /* Apply boundary conditions */
  if (imin < 0)   { imin = 0; }
  if (imax >= nx) { imax = nx-1; }
  if (jmin < 0)   { jmin = 0; }
  if (jmax >= ny) { jmax = ny-1; }

  /* Loop through each Y detector cell */
  for (j=jmin; j<=jmax; j++) {
    double ybmin = cell0[1] + j*ycellsize;  /* Detector y bounds */
    double ybmax = ybmin + ydetsize;

    /* Loop through each X detector cell */
    for (i=imin; i<=imax; i++) {
      /* NOTE: detector origin is in corner of detector cell */
      double xbmin = cell0[0] + i*xcellsize; /* Detector x bounds */
      double xbmax = xbmin + xdetsize;
      
      /* Initial overlap region = detector size */
      double x0 = xbmin, x1 = xbmax, y0 = ybmin, y1 = ybmax;

      /* Narrow overlap region according to projected mask cell shadow */
      if (xmin > x0) { x0 = xmin; }
      if (xmax < x1) { x1 = xmax; }
      if (ymin > y0) { y0 = ymin; }
      if (ymax < y1) { y1 = ymax; }

      /* If there is an overlap ... */
      if ( (x1 > x0) && (y1 > y0) ) {
	/* Partial weight is area of overlap */
	pwt = (x1-x0)*(y1-y0);

	/* ... then we compute the normalized weight */
	pwt = pwt * weight / ((xbmax - xbmin)*(ybmax-ybmin)) ;

	/* ... and add that weight to the output array */
	detimg[i + j*nx] += (DETMAX*pwt);
      }
      
    }
  }
  
  
  return 0;
}
