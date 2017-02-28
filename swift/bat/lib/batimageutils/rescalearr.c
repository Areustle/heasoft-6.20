#include <math.h>
#include <stdio.h>
#include "imageutils.h"

/* $Id: rescalearr.c,v 1.2 2002/10/24 14:40:37 craigm Exp $ */

/* 
 * rescalearr()
 * 
 * Rescale an input image into an output image, conserving pixel flux.
 * This routine distinguishes between the size of the image, and the
 * size of the storage used to keep the image.  These two quantities
 * will be different if the image is padded.
 * 
 * The user is responsible for allocating and freeing arrays.
 * rescalearr() zeroes the output array before writing to it.
 * 
 *   arr1 - input image array
 *   nx1  - number of X image pixel columns
 *   ny1  - number of Y image pixel rows
 *   nstride1 - number of storage columns in arr1 (nstride1 >= nx1)
 *   nrow1    - number of storage rows in arr1 (nrow1 >= ny1)
 *
 *   arr2 - output image 
 *   nstride2 - number of storage columns in arr2
 *   nrow2    - number of storage rows in arr2
 *
 *   xscale - expansion factor in X direction. 
 *            xscale == 1.0 means no expansion
 *            xscale > 1.0 means output image will be larger than input
 *   yscale - expansion factor in Y direction.
 * 
 *   xphase - phase offset for output image in X direction.  The input
 *            image is written to the output image starting at output pixel
 *            coordinates (xphase,yphase), where (0.0, 0.0) corresponds to the
 *            corner of the image.
 * 
 * RETURNS: 0 on success
 * 
 */
int rescalearr(FLOAT *arr1, int nx1, int ny1, int nstride1, int nrow1,
	       FLOAT *arr2,                   int nstride2, int nrow2,
	       FLOAT xscale, FLOAT yscale, 
	       FLOAT xphase, FLOAT yphase)
{
  int i, j;
  FLOAT kx = xscale, ky = yscale;
  FLOAT xl, xr, yl, yr;
  int il, ir, jl, jr;
  FLOAT z1;

  /* Clear out destination array with zeroes */
  for (i=0; i<(nstride2*nrow2); i++) {
    arr2[i] = 0.0;
  }

  /* Optimization.  If no rescaling, but just translation, then do a
     direct copy of the data rather than a rescale */
  if ((xscale == 1) && (yscale == 1)) {

    /* No translation either, pure copy */
    if ((xphase == 0) && (yphase == 0)) {
      for (j=0; j<ny1; j++)
	for (i=0; i<nx1; i++) 
	  arr2[j*nstride2+i] = arr1[j*nstride1+i];
      return 0;
    } else if ((floor(xphase) == xphase) && (floor(yphase) == yphase)) {
      
      /* Translation involved */
      int off2 = floor(yphase)*nstride2 + floor(xphase);
      for (j=0; j<ny1; j++)
	for (i=0; i<nx1; i++)
	  arr2[off2+j*nstride2+i] = arr1[j*nstride1+i];
      return 0;
    }      
  }

  /* Loop through rows, then columns of the input image */
  for(j=0; j<ny1; j++) {
    /* Compute Y corners of row */
    yl = ky*j + yphase;
    yr = ky*(j+1) + yphase;
    jl = floor(yl); jr = floor(yr);

    for (i=0; i<nx1; i++) {
      z1 = arr1[j*nstride1+i];
      if (z1 != 0) {
	/* Compute X corners of column */
	xl = kx*i + xphase;
	xr = ky*(i+1) + xphase;

	/* Column index numbers */
	il = floor(xl); ir = floor(xr);


	/* Case 1: an input pixel spans two or more output pixels */
	if ((ir > il+1) || (jr > jl+1)) {
	  int ii, jj;
	  FLOAT xxl, xxr, yyl, yyr;
	  yyl = yl; yyr = jl+1;

	  /* Loop over output pixels, again row and column */
	  for (jj = jl; jj <= jr; jj++) {
	    if (jj == jr) yyr = yr;  /* Last output row */
	    
	    xxl = xl; xxr = il+1;
	    for (ii = il; ii<=ir; ii++) {
	      if (ii == ir) xxr = xr; /* Last output column */
	      /* Linear expansion of this pixel */
	      arr2[jj*nstride2+ii] += z1*(yyr-yyl)*(xxr-xxl);
	      xxl = xxr;
	      xxr += 1;
	    }
	    
	    yyl = yyr;
	    yyr += 1;
	  }
	} else if (il == ir) {

	  /* Case 2: Fall into the same output column */

	  if (jl == jr) {
	    /* Case 2a: ... and same output row */
	    arr2[jl*nstride2+il] += z1*(yr-yl)*(xr-xl);
	  } else if (jl == (jr-1)) {
	    /* Case 2b: ... and two output rows */
	    arr2[jl*nstride2+il] += z1*(jr-yl)*(xr-xl);
	    arr2[jr*nstride2+il] += z1*(yr-jr)*(xr-xl);
	  }
	} else if (il == (ir-1)) {

	  /* Case 3: Fall into two output columns */
	  if (jl == jr) {
	    /* Case 3a: ... and the same output row */
	    arr2[jl*nstride2+il] += z1*(yr-yl)*(ir-xl);
	    arr2[jl*nstride2+ir] += z1*(yr-yl)*(xr-ir);
	  } else if (jl == (jr-1)) {
	    /* Case 3b: ... and two output rows */
	    arr2[jl*nstride2+il] += z1*(jr-yl)*(ir-xl);
	    arr2[jr*nstride2+il] += z1*(yr-jr)*(ir-xl);
	    arr2[jl*nstride2+ir] += z1*(jr-yl)*(xr-ir);
	    arr2[jr*nstride2+ir] += z1*(yr-jr)*(xr-ir);
	  }
	}

      } /* if (z1 != 0) */

    } /* for (i) */
    
  } /* for (j) */

  return 0;
}
