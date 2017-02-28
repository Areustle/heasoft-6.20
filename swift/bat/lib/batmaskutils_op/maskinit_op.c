#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "fitsio.h"
#include "batmask_op.h"
#include "batdet_op.h"

/* 
 * Mask and detector plane initialization and I/O functions
 * 
 * C. Markwardt
 *
 * $Id: maskinit_op.c,v 1.1 2003/10/28 19:14:08 krimm Exp $ 
 */

/* 
 * Read mask image from FITS file.
 *
 * fitsfile *maskfile - aperture FITS file opened for reading
 * struct batmaskplane_struct *mask - mask orientation structure
 *                      image data is filled into structure
 * int *status - CFITSIO status variable
 *
 * RETURNS: CFITSIO status
 */

int mask_image(fitsfile *maskfile, struct batmaskplane_struct *mask, 
	       int *status)
{
  int *aperture = 0;
  int nfilled = 0, nopen = 0;
  int ndim = 0, bitpix;
  long naxes[2] = {0, 0};
  long fpixel[2] = {1, 1};

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if (maskfile == 0) return (*status = NULL_INPUT_PTR);

  fits_get_img_param(maskfile, 2, &bitpix, &ndim, naxes, status);
  if ((*status) || (ndim != 2)) {
    if (*status == 0) *status = BAD_DIMEN;
    fprintf(stderr, "ERROR: aperture file must be a 2D image "
	    "(found %d dimensions)\n", ndim);
    return (*status);
  }

  aperture = (int *) malloc(sizeof(int)*naxes[0]*naxes[1]);
  if (aperture == 0) {
    fprintf(stderr, "ERROR: could not allocate memory for aperture\n");
    return MEMORY_ALLOCATION;
  }

  fits_read_pix(maskfile, TINT, fpixel, naxes[0]*naxes[1], 0, 
		aperture, 0, status);
  if (*status) { 
    fprintf(stderr, "ERROR: error reading aperture pixel data\n");
    return (*status);
  }
  mask->ncells[0] = naxes[0];
  mask->ncells[1] = naxes[1];
  mask->nfilled   = nfilled;
  mask->nopen     = nopen;
  mask->aperture  = (int *) aperture;

  return (*status);
}

/*
 * Read mask parameters from aperture FITS keywords
 *
 * fitsfile *file - aperture FITS file opened for reading;
 * struct batmaskplane_struct *mask - mask orientation structure;
 *                  keyword values are filled into mask;
 * int *status - CFITSIO status variable
 *
 * RETURNS: CFITSIO status
 */

int mask_readkey(fitsfile *file, struct batmaskplane_struct *mask,
		 int *status)
{
  double crpix1, crpix2;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if ((mask == 0) || (file == 0)) return (*status = NULL_INPUT_PTR);

  /* Default values */
  mask->centpos[0] = 0;
  mask->centpos[1] = 0;
  mask->centpos[2] = 100.1;
  mask->psi[0] = 0;
  mask->psi[1] = 0;
  mask->psi[2] = 0;
  mask->cellsize[0] = 0.5;
  mask->cellsize[1] = 0.5;
  mask->cellsize[2] = 0.1;    /* Thickness in cm */
  mask->meanpos[0] = +0.0;
  mask->meanpos[1] = +0.143;  /* Default offset in emergency */
  mask->meanpos[2] = 0.0;   /* Default offset in emergency */
  mask->cell0[0] = -121.75;
  mask->cell0[1] = -60.75;
  crpix1 = 0.5;
  crpix2 = 0.5;

  if (fits_read_key(file, TDOUBLE, "MASKBATX", &mask->centpos[0], 0, status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "MASKBATY", &mask->centpos[1], 0, status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "MASKBATZ", &mask->centpos[2], 0, status))
    *status = 0;

  /* Orientation of mean mask plane about design plane */
  if (fits_read_key(file, TDOUBLE, "MASKPSI0", &mask->psi[0], 0, status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "MASKPSI1", &mask->psi[1], 0, status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "MASKPSI2", &mask->psi[2], 0, status))
    *status = 0;

  /* Mask warp coefficients */
  mask->awarp = 0; mask->bwarp = 0; mask->cwarp = 0;

  /* Size of mask cell in cm */
  if (fits_read_key(file, TDOUBLE, "MASKCELX", &mask->cellsize[0], 0, status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "MASKCELY", &mask->cellsize[1], 0, status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "MASKCELZ", &mask->cellsize[2], 0, status))
    *status = 0;

  /* Mean offset of center of mask, w.r.t. design position */
  if (fits_read_key(file, TDOUBLE, "MASKOFFX", &mask->meanpos[0], 0, status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "MASKOFFY", &mask->meanpos[1], 0, status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "MASKOFFZ", &mask->meanpos[2], 0, status))
    *status = 0;

  /* Position of mask cell in aperture array (corner of tile) */
  if (fits_read_key(file, TDOUBLE, "CRVAL1", &mask->cell0[0], 0, status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "CRPIX1", &crpix1, 0,status))
    *status = 0;

  /* Adjust if the reference pixel is not the lower corner */
  if (fits_read_key(file, TDOUBLE, "CRVAL2", &mask->cell0[1], 0,status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "CRPIX2", &crpix2, 0,status))
    *status = 0;

  /* Adjust if the reference pixel is not the lower corner */
  mask->cell0[0] -= (crpix1-0.5)*mask->cellsize[0];
  mask->cell0[1] -= (crpix2-0.5)*mask->cellsize[1];

  return (*status);
}

/*
 * Read detector plane parameters from aperture FITS keywords
 *
 * fitsfile *file - aperture FITS file opened for reading;
 * struct batdetlane_struct *detplane - detector plane orientation structure;
 *                  keyword values are filled into detplane;
 * int *status - CFITSIO status variable
 *
 * RETURNS: CFITSIO status
 */
int detplane_readkey(fitsfile *file, struct batdetplane_struct *detplane, 
		     int *status)
{

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if ((detplane == 0) || (file == 0)) return (*status = NULL_INPUT_PTR);

  /* Default values */
  detplane->centpos[0] = 0;
  detplane->centpos[1] = 0;
  detplane->centpos[2] = 0.35;

  detplane->meanpos[0] = 0;
  detplane->meanpos[1] = 0;
  detplane->meanpos[2] = 0;

  /* Now uses numbers found in IDL code (corner of tile) */
  detplane->cell0[0] = -60.05;   /* Origin of det plane in CM (BAT-X) */
  detplane->cell0[1] = -36.32;   /* Origin of det plane in CM (BAT-Y) */

  detplane->ncells[0] = 288;     /* Number of det plane cells in BAT-X */
  detplane->ncells[1] = 176;     /* Number of det plane cells in BAT-Y */

  detplane->cellsize[0] = 0.42;
  detplane->cellsize[1] = 0.42;
  detplane->cellsize[2] = 0.20;
  detplane->detsize[0] = 0.40;
  detplane->detsize[1] = 0.40;
  detplane->detsize[2] = 0.20;

  /* Read designed detector plane position */
  if (fits_read_key(file, TDOUBLE, "DETBATX", &detplane->centpos[0], 0,status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "DETBATY", &detplane->centpos[1], 0,status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "DETBATZ", &detplane->centpos[2], 0,status))
    *status = 0;

  /* Mean offset of center of detplane, w.r.t. design position */
  if (fits_read_key(file, TDOUBLE, "DETOFFX", &detplane->meanpos[0], 0,status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "DETOFFY", &detplane->meanpos[1], 0,status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "DETOFFZ", &detplane->meanpos[2], 0,status))
    *status = 0;

  /* Size of det cell in cm */
  if (fits_read_key(file, TDOUBLE, "DETCELX", &detplane->cellsize[0], 0,status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "DETCELY", &detplane->cellsize[1], 0,status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "DETCELZ", &detplane->cellsize[2], 0,status))
    *status = 0;

  /* Size of detector in cm */
  if (fits_read_key(file, TDOUBLE, "DETSIZEX", &detplane->detsize[0], 0,status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "DETSIZEY", &detplane->detsize[1], 0,status))
    *status = 0;
  if (fits_read_key(file, TDOUBLE, "DETSIZEZ", &detplane->detsize[2], 0,status))
    *status = 0;

  return *status;
}

/* 
 * Write mask orientation parameters to FITS keywords
 *
 * fitsfile *file - FITS file opened for writing
 * struct batmaskplane_struct *mask - mask parameters to be written
 * int *status - CFITSIO status variable
 *
 * RETURNS: CFITSIO status
 */

int mask_writekey(fitsfile *file, struct batmaskplane_struct *mask, 
		  int *status)
{
  if ((status == 0) || (*status != 0)) return (*status);

  fits_write_comment(file, 
    "---------------------------------- BAT Mask Orientation ", status);
  fits_update_key(file, TDOUBLE, "MASKBATX", &mask->centpos[0], 
		  "Center of mask tile plane in BAT_X (centimeters)", status);
  fits_update_key(file, TDOUBLE, "MASKBATY", &mask->centpos[1], 
		  "Center of mask tile plane in BAT_Y (centimeters)", status);
  fits_update_key(file, TDOUBLE, "MASKBATZ", &mask->centpos[2], 
		  "Top of mask tile plane in BAT_Z (centimeters)", status);

  fits_update_key(file, TDOUBLE, "MASKOFFX", &mask->meanpos[0], 
		  "Offset of mask in BAT_X (centimeters)", status);
  fits_update_key(file, TDOUBLE, "MASKOFFY", &mask->meanpos[1], 
		  "Offset of mask in BAT_Y (centimeters)", status);
  fits_update_key(file, TDOUBLE, "MASKOFFZ", &mask->meanpos[2], 
		  "Offset of mask in BAT_Z (centimeters)", status);

  fits_update_key(file, TDOUBLE, "MASKX0", &mask->cell0[0], 
		  "Position of mask corner in BAT_X (centimeters)", status);
  fits_update_key(file, TDOUBLE, "MASKY0", &mask->cell0[1], 
		  "Position of mask corner in BAT_Y (centimeters)", status);

  fits_update_key(file, TDOUBLE, "MASKPSI0", &mask->psi[0], 
		  "Mask Euler rotation about X-axis (degree)", status);
  fits_update_key(file, TDOUBLE, "MASKPSI1", &mask->psi[1], 
		  "Mask Euler rotation about Y-axis (degree)", status);
  fits_update_key(file, TDOUBLE, "MASKPSI2", &mask->psi[2], 
		  "Mask Euler rotation about Z-axis (degree)", status);
  
  fits_update_key(file, TDOUBLE, "MASKCELX", &mask->cellsize[0],
		  "Size of mask cell in BAT_X (centimeters)", status);
  fits_update_key(file, TDOUBLE, "MASKCELY", &mask->cellsize[1],
		  "Size of mask cell in BAT_Y (centimeters)", status);
  fits_update_key(file, TDOUBLE, "MASKCELZ", &mask->cellsize[2],
		  "Size of mask cell in BAT_Z (centimeters)", status);

  return (*status);
}

/* 
 * Write detector plane orientation parameters to FITS keywords
 *
 * fitsfile *file - FITS file opened for writing
 * struct batmaskplane_struct *mask - detector plane parameters to be written
 * int *status - CFITSIO status variable
 *
 * RETURNS: CFITSIO status
 */
int detplane_writekey(fitsfile *file, struct batdetplane_struct *detplane, 
		  int *status)
{
  if ((status == 0) || (*status != 0)) return (*status);

  fits_write_comment(file, 
    "---------------------------------- BAT Detector Orientation ", status);
  fits_update_key(file, TDOUBLE, "DETBATX", &detplane->centpos[0], 
		  "Center of detector plane in BAT_X (centimeters)", status);
  fits_update_key(file, TDOUBLE, "DETBATY", &detplane->centpos[1], 
		  "Center of detector plane in BAT_Y (centimeters)", status);
  fits_update_key(file, TDOUBLE, "DETBATZ", &detplane->centpos[2], 
		  "Top of detector plane in BAT_Z (centimeters)", status);

  fits_update_key(file, TDOUBLE, "DETOFFX", &detplane->meanpos[0], 
		  "Offset of detector plane in BAT_X (centimeters)", status);
  fits_update_key(file, TDOUBLE, "DETOFFY", &detplane->meanpos[1], 
		  "Offset of detector plane in BAT_Y (centimeters)", status);
  fits_update_key(file, TDOUBLE, "DETOFFZ", &detplane->meanpos[2], 
		  "Offset of detector plane in BAT_Z (centimeters)", status);

  fits_update_key(file, TDOUBLE, "DETCELX", &detplane->cellsize[0],
		  "Size of detector pitch cell in BAT_X (centimeters)", status);
  fits_update_key(file, TDOUBLE, "DETCELY", &detplane->cellsize[1],
		  "Size of detector pitch cell in BAT_Y (centimeters)", status);
  fits_update_key(file, TDOUBLE, "DETCELZ", &detplane->cellsize[2],
		  "Size of detector pitch cell in BAT_Z (centimeters)", status);

  fits_update_key(file, TDOUBLE, "DETSIZEX", &detplane->detsize[0],
		  "Size of detector in BAT_X (centimeters)", status);
  fits_update_key(file, TDOUBLE, "DETSIZEY", &detplane->detsize[1],
		  "Size of detector in BAT_Y (centimeters)", status);
  fits_update_key(file, TDOUBLE, "DETSIZEZ", &detplane->detsize[2],
		  "Size of detector in BAT_Z (centimeters)", status);

  return (*status);
}
