#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "batmask_op.h"
#include "batdet_op.h"
#include "headas.h"

/* 
 * Make sure there are detector gaps in detector mask (arbitrary value)
 *
 * double *detimg - pointer to existing 2D array of detector image.
 * int nx, ny - number of columns and rows in image.
 * float val - value to place in cells which are gaps.
 * RETURNS: nothing.  Image is modified in place 
 */
void mkdetgaps_val(double *detimg, int nx, int ny, float val)
{
  /* Positions of gaps in X (columns) ... */
  int xgaps[] = {
    16,  17,  34,  35,  52,  53,  70,  71,  88,  89, 106, 107, 124, 125, 142,
    143, 160, 161, 178, 179, 196, 197, 214, 215, 232, 233, 250, 251, 268, 269};
  /* ... and Y (rows) */
  int ygaps[] = {
    8,   9,  10,  19,  20,  21,  30,  31,  32,  41,  42,  43,  52,  53,  54,
    63,  64,  65,  74,  75,  76,  85,  86,  87,  96,  97,  98, 107, 108, 109,
    118, 119, 120, 129, 130, 131, 140, 141, 142, 151, 152, 153, 162, 163, 164};
  int nxgaps, nygaps;
  int i, j;

  nxgaps = sizeof(xgaps)/sizeof(xgaps[0]);
  nygaps = sizeof(ygaps)/sizeof(ygaps[0]);
  
  /* Carve out the detector gaps, along DETX ... */
  for (i=0; i<nxgaps; i++) {
    if (xgaps[i] < nx) for (j=0; j<ny; j++) 
      detimg[j*nx+xgaps[i]] = val;
  }
  /* ... and DETY */
  for (j=0; j<nygaps; j++) {
    if (ygaps[j] < ny) for (i=0; i<nx; i++) 
      detimg[ygaps[j]*nx + i] = val;
  }

  return;
}

/* 
 * Make sure there are detector gaps in detector mask (arbitrary value)
 *
 * int *detmask - pointer to existing 2D array of detector image.
 * int nx, ny - number of columns and rows in image.
 * int val - value to place in cells which are gaps.
 * RETURNS: nothing.  Image is modified in place 
 */
void mkdetgaps_intval(int *detmask, int nx, int ny, int val)
{
  /* Positions of gaps in X (columns) ... */
  int xgaps[] = {
    16,  17,  34,  35,  52,  53,  70,  71,  88,  89, 106, 107, 124, 125, 142,
    143, 160, 161, 178, 179, 196, 197, 214, 215, 232, 233, 250, 251, 268, 269};
  /* ... and Y (rows) */
  int ygaps[] = {
    8,   9,  10,  19,  20,  21,  30,  31,  32,  41,  42,  43,  52,  53,  54,
    63,  64,  65,  74,  75,  76,  85,  86,  87,  96,  97,  98, 107, 108, 109,
    118, 119, 120, 129, 130, 131, 140, 141, 142, 151, 152, 153, 162, 163, 164};
  int nxgaps, nygaps;
  int i, j;

  nxgaps = sizeof(xgaps)/sizeof(xgaps[0]);
  nygaps = sizeof(ygaps)/sizeof(ygaps[0]);
  
  /* Carve out the detector gaps, along DETX ... */
  for (i=0; i<nxgaps; i++) {
    if (xgaps[i] < nx) for (j=0; j<ny; j++) 
      detmask[j*nx+xgaps[i]] = val;
  }
  /* ... and DETY */
  for (j=0; j<nygaps; j++) {
    if (ygaps[j] < ny) for (i=0; i<nx; i++) 
      detmask[ygaps[j]*nx + i] = val;
  }

  return;
}

/* ----------------------------------------------------------------- */
/* Read detector quality mask file
 * char *filename - name of detector quality mask file
 * long int axes[2] - upon return, the dimensions of the image
 * int goodval - default "good" value, if not provided in map file
 *               by GOODVAL keyword
 * int *status - upon return, the status.  0=good, else CFITSIO error status
 *
 * RETURNS: point to array of quality map (int *).  User is
 * responsible for freeing.
 *
 */
int *read_detmask(char *filename, long int axes[2], int goodval, int *status)
{
  int naxis, bitpix, i;
  int *image = 0;
  fitsfile *fptr = 0;
  long int fpixel[] = {1,1};
  int safestatus = 0;
  
  if (status == 0) return 0;
  if (*status != 0) return 0;
  if (filename == 0) {
    *status = NULL_INPUT_PTR;
    return 0;
  }

  fits_open_image(&fptr, filename, READONLY, status);
  if (*status) return 0;

  fits_get_img_param(fptr, 2, &bitpix, &naxis, axes, status);
  if (*status) goto FAILED;

  image = (int *) malloc (sizeof(int) * axes[0] * axes[1]);
  if (image == 0) {
    *status = MEMORY_ALLOCATION;
    goto FAILED;
  }

  fits_read_pix(fptr, TINT, fpixel, axes[0]*axes[1], 0, 
		image, 0, status);
  if (*status) goto FAILED;

  fits_read_key(fptr, TINT, "GOODVAL", &goodval, 0, status);
  *status = 0;

  for (i=0; i< (axes[0]*axes[1]); i++) {
    image[i] = (image[i] == goodval);
  }

  fits_close_file(fptr, status);
  return image;


 FAILED:
  /* Free the image data if it was allocated */
  if (image) free(image);
  image = 0;

  /* Close the file if it was open */
  safestatus = 0;
  if (fptr) fits_close_file(fptr, &safestatus);
  fptr = 0;

  return 0;
}

