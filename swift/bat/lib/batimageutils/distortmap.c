#include <fitsio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "imageutils.h"



/*
 * read_distortmap - read BAT distortion map from disk
 * 
 * char *filename - name of input file
 * struct distortmap_struct **d - upon return, *d contains a 
 *            distortion map structure;
 *            NOTE: user is responsible to free(*d) when done
 * int *status - CFITSIO status variable, set upon return
 *
 * RETURNS: 0 (success) or non-zero upon failure
 */
int read_distortmap(char *filename, struct distortmap_struct **d, int *status)
{
  fitsfile *infile = 0;
  int bitpix, naxis;
  long naxes[10] = {1,1,1,1,1,1,1,1,1,1};
  long fpixel[10] = {1,1,1,1,1,1,1,1,1,1};
  long npix = 0;
  int i, anynul = 0;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status) return (*status);
  if (d == 0) return (*status = NULL_INPUT_PTR);
  *d = 0;

  if (fits_open_image(&infile, filename, READONLY, status)) {
    return (*status);
  }

  if (fits_get_img_param(infile, 10, &bitpix, &naxis, naxes, status)) {
    goto DONE;
  }
  if (naxis != 3 || naxes[2] != 2) {
    *status = BAD_DIMEN;
    goto DONE;
  }

  npix = naxes[0]*naxes[1]*naxes[2];
  *d = (struct distortmap_struct *) malloc(sizeof(struct distortmap_struct) +
					   sizeof(FLOAT)*(npix-1));
  if (*d == 0) {
    *status = MEMORY_ALLOCATION;
    goto DONE;
  }
  (*d)->naxis = naxis;
  for (i = 0; i<naxis; i++) { (*d)->naxes[i] = naxes[i]; } 
  (*d)->dimx = (*d)->data;
  (*d)->dimy = (*d)->data + naxes[0]*naxes[1];
  (*d)->nulval = -1e37;
    
  if (fits_read_pix(infile, TFLTTYPE, fpixel, npix,
		    &(*d)->nulval, (*d)->data, &anynul, status)) {
    goto DONE;
  }

  /* Read WCS coordinates of the map */
  read_wcsaxis(infile, 1, "T", 0, 0, (*d)->ctype1, 
	       &(*d)->crpix1,&(*d)->cdelt1, &(*d)->crval1,status);
  read_wcsaxis(infile, 2, "T", 0, 0, (*d)->ctype2, 
	       &(*d)->crpix2,&(*d)->cdelt2, &(*d)->crval2,status);

 DONE:
  if (infile) {
    int safestatus = 0;
    fits_close_file(infile, &safestatus);
  }
  if (d && *status) { free (*d); *d = 0; }
  return *status;

}

/*
 * distortmap_coco1 - convert between distorted and "true" BAT coordinates
 * 
 * struct distortmap_struct *d - distortion map data (from read_distortmap)
 * double imx_in, imy_in - input BAT tangent plane coordinates
 * double imx_out, imy_out - output BAT tangent plane coordinates
 * int direction - direction of transformation;
 *                 APP_TO_TRUE - distorted to "true" conversion
 *                 TRUE_TO_APP - "true" to distorted conversion
 * 
 * RETURNS: 0 if successful; non-zero if an error occurred
 */
int distortmap_coco1(struct distortmap_struct *d,
		     double imx_in,  double imy_in,
		     double *imx_out, double *imy_out,
		     int direction)
{
  double indices[1][2];
  struct interp_struct interp;
  FLOAT dimx = 0.0, dimy = 0.0;
  int status = 0;

  if (d == 0) return -1;
  indices[0][0] = ((imx_in - d->crval1)/d->cdelt1 + d->crpix1);
  indices[0][1] = ((imy_in - d->crval2)/d->cdelt2 + d->crpix2);

  status = imageinterp_init(d->naxes[0], d->naxes[1], indices, 1, 
			    INTERP_BILINEAR, 0, 0.0, &interp);
  if (status) return status;

  status = imageinterp_apply(d->dimx, &dimx, d->nulval, &interp);
  if (!status) status = imageinterp_apply(d->dimy, &dimy, d->nulval, &interp);

  imageinterp_free(&interp);

  if (direction == APP_TO_TRUE) {
    *imx_out = imx_in + dimx;
    *imy_out = imy_in + dimy;
  } else {
    *imx_out = imx_in - dimx;
    *imy_out = imy_in - dimy;
  }

  return status;
}
		    
		    
