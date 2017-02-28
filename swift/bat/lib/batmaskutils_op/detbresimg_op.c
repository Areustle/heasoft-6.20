#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "batdet_op.h"
#include "headas.h"

/* 
 * Basic scan-conversion of polygon to image
 *
 * Quadrilateral defined by (x[i],y[i]) is rendered into array
 * detplane, which is an nx column by ny row image array.  The pixels
 * are assigned a weight.
 * 
 * The coordinates of the image origin are in cell0, and the sizes of
 * the pixel in X and Y are in cellsize.
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
 * $Id: detbresimg_op.c,v 1.1 2003/10/28 19:14:08 krimm Exp $
 */

int detbresimg(double x[4], double y[4], 
	      DETFLOAT *detplane, int nx, int ny, 
	      double cellsize[2], double cell0[2], DETFLOAT weight)
{
  double x2[8], y2[8];
  double yc, yb;
  double xp0, yp0, xp1, yp1;
  double xm0, ym0, xm1, ym1;
  double xpx, xmx;
  double xmax, xmin;
  double ratp, ratm;
  float fpx, fmx, fx0, fx1;
  float fby, fey;
  DETFLOAT *px, fy;
  int jb, je;
  int i, j, ip, im, ipx, imx, np, i0, i1;
  
  DETFLOAT wmax = DETMAX*weight;

  /* Cycle this square polygon */
  for(i=0; i<4; i++) {
    x2[i] = x[i]; x2[4+i] = x[i]; 
    y2[i] = y[i]; y2[4+i] = y[i]; 
  }

  /* headas_chat(5, "  wmax=%f\n", (float)wmax); */

  /* Find smallest y-value vertex */
  ip = 2;
  xmin = x2[ip]; xmax = x2[ip];
  for(i=2; i<6; i++) {
    if (y2[i] < y2[ip]) ip = i;
    if (x2[i] < xmin)   xmin = x2[i];
    if (x2[i] > xmax)   xmax = x2[i];
  }
  im = ip;
  /* im and ip now point to the vertex with the smallest y value */

  /* Special case: if there are two vertices that share the bottom,
     then we want to be sure that ip and im span those two vertices */
  if (y2[ip] == y2[ip+1]) { 
    ip ++;
  } else if (y2[im] == y2[im-1]) { 
    im --;
  }

  /* Check to see if polygon is even inside the field of view */
  if (xmin >= (cell0[0]+cellsize[0]*nx)) return 0;
  if (xmax <   cell0[0]) return 0;

  /* IP and IM refer to the position in the X2 and Y2 arrays, one
     walking around in the "plus" direction and the other in the
     "minus" direction. */

  /* Start and stop position, in scan lines */
  yb = y2[ip];
  jb = floor((y2[ip]  -cell0[1])/cellsize[1]);
  je = floor((y2[ip+2]-cell0[1])/cellsize[1]);

  /* Reset to the beginning of the scan line */
  yb = jb*cellsize[1] + cell0[1];

  /* 
  fprintf(stderr, "jb = %d\n", jb);
  fprintf(stderr, "je = [%d]%d [%d]<%d> [%d]%d [%d]%d\n", 
	  ip+1, (int) floor((y2[ip+1]-cell0[1])/cellsize[1]),
	  ip+2, je,
	  ip+3, (int) floor((y2[ip+3]-cell0[1])/cellsize[1]),
	  ip+4, (int) floor((y2[ip+4]-cell0[1])/cellsize[1]));
  */

  fby = -(jb*cellsize[1]+cell0[1]-y2[ip]  )/cellsize[1];
  fey = -(je*cellsize[1]+cell0[1]-y2[ip+2])/cellsize[1];

  /* Initialize vertices that are kept in local */
  xp0 = x2[ip] ; yp0 = y2[ip] ; xp1 = x2[ip+1] ; yp1 = y2[ip+1];
  xm0 = x2[im] ; ym0 = y2[im] ; xm1 = x2[ip-1] ; ym1 = y2[ip-1];
  if (yp1 != yp0) ratp = (xp1-xp0)/(yp1-yp0); else ratp = 0;
  if (ym1 != ym0) ratm = (xm1-xm0)/(ym1-ym0); else ratm = 0;

  if (jb >= ny || je < 0) return 0;

  /* Loop through each scan line */
  for (j=jb; j<=je; j++) {
    /*  fprintf(stderr, "start j = %d\n", j); */
    if (j < 0)   continue;
    if (j >= ny) break;

    /* Find position at bottom of scanline */
    yc = yb + (j-jb)*cellsize[1];

    /* fprintf(stderr, "cont1 j = %d yc = %f yp1 = %f ym1 = %f\n",
       j, yc, yp1, ym1); */

    /* Check if we have advanced to the next vertex, either from the
       "plus" side or the "minus" side. */
    while (yc >= yp1) {
      ip = ip + 1;                 /* Plus side: advance */
      /* fprintf(stderr, "ip=%d\n", ip); */
      if (ip == 8) goto DONE_BRES; /* Graceful termination */
      xp0 = x2[ip] ; yp0 = y2[ip] ; xp1 = x2[ip+1] ; yp1 = y2[ip+1];
      if (yp1 != yp0) ratp = (xp1-xp0)/(yp1-yp0); else ratp = 0;
    }
    while (yc >= ym1) {
      im = im - 1;                 /* Minus side */
      /* fprintf(stderr, "im=%d\n", im); */
      if (im == -1) goto DONE_BRES;
      xm0 = x2[im] ; ym0 = y2[im] ; xm1 = x2[im-1] ; ym1 = y2[im-1];
      if (ym1 != ym0) ratm = (xm1-xm0)/(ym1-ym0); else ratm = 0;
    }
    
    /* fprintf(stderr, "cont2 j = %d\n", j); */
    /* Find ideal position of points on polygon using simple linear 
       extrapolation */
    xpx = xp0 + ratp*(yc-yp0);
    xmx = xm0 + ratm*(yc-ym0);
    /* Convert to scan column positions */
    ipx = floor((xpx-cell0[0])/cellsize[0]);
    imx = floor((xmx-cell0[0])/cellsize[0]);
    /* Fractional coverage of "plus" and "minus" side pixels */
    fpx = -(ipx*cellsize[0]+cell0[0]-xpx)/cellsize[0];
    fmx = -(imx*cellsize[0]+cell0[0]-xmx)/cellsize[0];

    /* Reorder scanline */
    if (ipx < imx) {
      i0 = ipx; i1 = imx;    /* Scan column start/stop */
      fx0 = fpx; fx1 = fmx;  /* Fractional coverage at start/stop columns */
    } else {
      i0 = imx; i1 = ipx;
      fx0 = fmx; fx1 = fpx;
    }
    /* Check for misses */
    if (i0 >= nx || i1 < 0) {
      /* fprintf(stderr, " -- continue i0 = %d i1 = %d nx = %d\n", i0, i1, nx); */
      continue;
    }
    px = &detplane[j*nx+ipx];
    np = i1-i0+1-2;

    /* Figure the row-related fractional coverage */
    fy = wmax;
    if (j == jb) { 
      fy = wmax*(1-fby);
    }
    if (j == je) { 
      fy = wmax*fey; 
    }

    /* Is start column < 0? */
    if (i0 >= 0) { 
      /* No - insert fractionally covered pixel */
      *px++ += (DETFLOAT) ((1-fx0)*fy);
      i0++;
    } else {
      /* Yes - reset start column to column 0, no fractional pixel */
      px = &detplane[j*nx];
      i0 = 0;
      np = i1-i0+1-1;
    }
    if (i0+np >= nx) {
      /* Case of stop column running past end of output array */
      np = nx-i0;
    }

    /* Fill scan columns - main inner loop */
    for (i=0; i<np; i++) *px++ += fy;
    /* fprintf(stderr, "j = %d fract = %f\n", j, fy); */

    /* Check other side of scanline for full or partial coverage */
    if (i1 < nx) { /* Full - add half-pixel */
      *px++ += (DETFLOAT) (fx1*fy);
    }
    
  }
 DONE_BRES:
    
  return 0;
}

