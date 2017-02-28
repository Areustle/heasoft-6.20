#include <fitsio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "imageutils.h"


/*
 * imageinterp_init() - initialize an interpolation scheme
 * 
 * Initializes an interpolation scheme.  The desired 
 * interpolations must be given, as well as the size of the
 * tabulated image.
 *
 * int nx, ny - size of the tabulated image in pixels;
 *              the image would be declared as img[ny][nx];
 * double pixcrd[ncoord][2] - the interpolation positions;
 *              pixcrd[i][0/1] is the X/Y position in pixel units,
 *              where 1.0 is the center of the first pixel;
 * int ncoord - the size of the pixcrd[][] array;
 * int method - interpolation method; one of: 
 *                INTERP_NEARN    - nearest neighbor
 *                INTERP_BILINEAR - bilinear
 *                INTERP_BICUBICB - bicubic B-spline
 *                INTERP_BICUBICH - bicubic hi-resolution spline
 * int flags - interpolation flags (currently ignored)
 * double param - interpolation parameter value (currently ignored)
 * struct interp_struct *interp - a pointer to an existing interpolation
 *              structure; upon return, this structure is filled with
 *              intermediate data;  the user must free it with
 *              imageinterp_free();
 * RETURNS: 0 (success); non-zero error code (CFITSIO code)
 */ 
int imageinterp_init(long int nx, long int ny,
		     double pixcrd[][2], long int ncoord,
		     int method, int flags, double param,
		     struct interp_struct *interp)
{
  long int i, j;
  long int ix, iy, ii;
  double x, y, dx, dy;
  long int npix = nx*ny;
  long int *ind, *igood;
  FLOAT *coef;
  long int nsamp;
  long int minind, maxind, ngood;

  if (interp == 0) return NULL_INPUT_PTR;
  memset(interp, 0, sizeof(struct interp_struct));

  switch (method) {
  case INTERP_NEARN:    nsamp = 1; break;
  case INTERP_BILINEAR: nsamp = 4; break;
  default: return -1;
  }

  ind = (long int *) malloc(sizeof(long int)*(nsamp+1)*ncoord);
  if (ind == 0) return MEMORY_ALLOCATION;
  coef = (FLOAT *) malloc(sizeof(FLOAT)*nsamp*ncoord);
  if (coef == 0) {
    free(ind);
    return MEMORY_ALLOCATION;
  }
  igood = ind + (nsamp*ncoord);
  
  j = 0;
  if (method == INTERP_NEARN) {
    /*  ===== Nearest neighbor interpolation */

    for (i=0; i<ncoord; i++) {
      /* Convert from FITS pixel coordinates to C array offsets */
      /* Floating point pixel number ... */
      x = pixcrd[i][0] - 1;
      y = pixcrd[i][1] - 1;
      
      /* ... and nearest neighbor */
      ix = rint(x);
      iy = rint(y);
      
      /* Short circuit if the position is out of bounds */
      if (ix < 0 || ix >= (nx-1) ||
	  iy < 0 || iy >= (ny-1)) {
	ind[j] = -1;
	j += 4;
	continue;
      }

      /* Ind points to the nearest neighbor position, and
	 the coefficient is 1.0 */
      ind[j] = ix + iy*nx; coef[j] = 1; j++;
    }
  } else {
    /*  ===== Bilinear interpolation */

    for (i=0; i<ncoord; i++) {
      /* Convert from FITS pixel coordinates to C array offsets */
      /* Floating point pixel number ... */
      x = pixcrd[i][0] - 1;
      y = pixcrd[i][1] - 1;
      
      /* ... and the lower neighbor */
      ix = floor(x);
      iy = floor(y);
      
      /* Short circuit if the position is out of bounds */
      if (ix < 0 || ix >= (nx-1) ||
	  iy < 0 || iy >= (ny-1)) {
	ind[j] = -1;
	j += 4;
	continue;
      }

      /* Offset from lower pixel in X and Y */
      dx = (x-ix);
      dy = (y-iy);

      /* Pixel index for lower pixel */
      ii = ix + iy*nx;

      /* Coefficients for ... */
      ind[j] = ii; coef[j] = (1-dx)*(1-dy); /* IX, IY     */ j++; ii++;
      ind[j] = ii; coef[j] = dx*(1-dy);     /* IX+1, IY   */ j++; ii += (nx-1);
      ind[j] = ii; coef[j] = (1-dx)*dy;     /* IX, IY+1   */ j++; ii++;
      ind[j] = ii; coef[j] = dx*dy;         /* IX+1, IY+1 */ j++;
    }
  }

  /* Record the min/max pixel numbers */
  minind = npix;
  maxind = -1;
  ngood = 0;
  for (i=0; i<(ncoord*nsamp); i++) {
    if (ind[i] < 0 || ind[i] >= npix) { 
      /* If some of the input pixels for an output point are out of
	 bounds then set the first pixel for that output point to out
	 of bounds as well, for optimization's sake.  */
      ind[(i/nsamp)*nsamp] = -1;
    } else {
      if      (ind[i] > maxind) { maxind = ind[i]; }
      else if (ind[i] < minind) { minind = ind[i]; }
    }
  }

  /* Record which output pixels are "good" */
  for (i=0; i<ncoord; i++) {
    if (ind[i*nsamp] >= 0) {
      igood[ngood ++] = i;
    }
  }

  interp->method = method;
  interp->nsamp = nsamp;
  interp->ind = ind;
  interp->coef = coef;
  interp->minind = minind;
  interp->maxind = maxind;
  interp->ncoord = ncoord;
  interp->ngood = ngood;
  interp->igood = igood;
  return 0;
}

/*
 * imageinterp_apply() - apply interpolation scheme to an image
 *
 * Apply an interpolation scheme, created with imageinterp_init(),
 * to a tabulated image.  Interpolation positions which fall outside
 * of the tabulated image, or encounter a null value, have their
 * result set to the null value.
 *
 * FLOAT *from - pointer to tabulated image; 
 *               must be an nx by ny image defined as img[ny][nx];
 * FLOAT *to - pointer to an existing array of ncoord values;
 *             upon return, the interpolants are placed here;
 * FLOAT nulval - null value for the tabulated image and undefined
 *             output values;
 * struct interp_struct *interp - pointer to an existing interpolation
 *             structure, initialized by imageinterp_init();
 *
 * RETURNS: 0 (success) or non-zero (failure)
 */
int imageinterp_apply(FLOAT *from, FLOAT *to, FLOAT nulval,
		      struct interp_struct *interp)
{
  long int i, j;
  long int ii, nsamp, ngood, ncoord;
  long int *ind, indi, *igood, ig;
  FLOAT *coef;
  FLOAT v, toi;

  if (interp == 0) return NULL_INPUT_PTR;
  if (interp->ind == 0) return NULL_INPUT_PTR;
  if (interp->coef == 0) return NULL_INPUT_PTR;
  ind = interp->ind;
  coef = interp->coef;
  nsamp = interp->nsamp;
  ncoord = interp->ncoord;
  ngood = interp->ngood;
  igood = interp->igood;

  /* Handle case of no good values */
  if (ngood == 0) {
    for (i=0; i<ncoord; i++) to[i] = nulval;
    return 0;
  }

  /* Loop through output pixels... */
  ii = 0;   /* ii = index to input pixel coefficients */
  ig = 0;
  for (i = 0; i < ncoord; i++, ii+=nsamp) {
    to[i] = nulval;
    if (i != igood[ig]) continue;
    ig ++;

    toi = 0;
    v = nulval;
    /* Total all of the coefficients x input pixel values */
    for (j = ii; j < (ii+nsamp); j++) {
      indi = ind[j];
      v = from[indi];
      if (v == nulval) break;
      toi += coef[j]*v;
    }

    if (v != nulval) to[i] = toi;
  }

  return 0;
}

/*
 * imageinterp_free() - free an interpolation scheme structure
 *
 * De-allocates memory associated with an interpolation scheme.
 * The argument must point to a structure previously initialized
 * by imageinterp_init().
 *
 * struct interp_struct *interp - pointer to an existing interpolation
 *          structure, already initialized by imageinterp_init();
 *
 * RETURNS: 0 (success); non-zero (failure)
 */
int imageinterp_free(struct interp_struct *interp)
{
  if (interp == 0) return 0;
  if (interp->ind) free(interp->ind);
  /* Note: field igood is allocated with ind; no need to deallocate */
  if (interp->coef) free(interp->coef);
  memset(interp, 0, sizeof(struct interp_struct));
  return 0;
}
