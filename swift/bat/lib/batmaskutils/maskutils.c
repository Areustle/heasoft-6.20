#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "batmask.h"
#include "batdet.h"

/* 
 * Mask and forward projection utility functions
 * 
 * C. Markwardt
 *
 * $Id: maskutils.c,v 1.10 2004/05/24 01:59:26 craigm Exp $ 
 */

/* 
 * Estimate number of mask sublayers to compute in forward projection.
 *
 * double *srcpos - 3-vector with source position in BAT_X/Y/Z (cm);
 * double *cellsize - 3-vector with mask tile sizes in BAT_X/Y/Z (cm);
 *
 * RETURNS: estimated number of sublayers
 */

int est_masklayers(double *srcpos, double *cellsize, 
		   struct batmaskcorrections_struct *corrs)
{
  double r, tan_th;
  /* Following values do not have to be exact */
  double mask_w = 100;  /* Mask width in cm */
  double subpix = corrs->subpixsize; /* Subpixel size in cm */
  double nmasklayers = 0;
  
  r = sqrt(srcpos[0]*srcpos[0] + srcpos[1]*srcpos[1]);
  /* Extra w for source at finite dist */
  tan_th = (r + mask_w/2)/(srcpos[2] - 100); 

  nmasklayers = tan_th*cellsize[2] / subpix + 1;
  if (nmasklayers < 1) nmasklayers = 1;
  if (nmasklayers > 8) nmasklayers = 8;

  corrs->nmasklayers = (int) nmasklayers;
  return (int) nmasklayers;
}

/*
 * Add detector subpixels to make one detector pixel
 *
 * struct batdetplane_struct *detplane - detector plane orientation;
 * double subpixsize - size of subpixel in cm
 * DETFLOAT *arr - 2D array of subpixels
 * int stride - number of columns in arr
 *
 * RETURNS: appropriately summed subpixels 
 */
double maskaddsub(struct batdetplane_struct *detplane, 
		  struct batmaskcorrections_struct *corrs,
		  DETFLOAT *arr, int stride, double prevsum)
{
  int k, l;
  DETFLOAT sum = 0, *arrptr;
  int subpitchx, subpitchy;  /* Number of subpixels per det pitch element */
  int subdetx,   subdety;    /* Number of subpixels per detector */
  double subpixsize;
  int maskedgeflag = 0;

  subpixsize = corrs->subpixsize;
  subpitchx = rint(detplane->cellsize[0]/subpixsize);
  subpitchy = rint(detplane->cellsize[1]/subpixsize);

  subdetx  = rint(detplane->detsize[0]/subpixsize);
  subdety  = rint(detplane->detsize[1]/subpixsize);

  arrptr = arr;
  for (k=0; k<subdety; k++) {

    for (l=0; l<subdetx; l++) {
      if(*arrptr == 0) maskedgeflag = 1;
      sum += *arrptr++;
    }
    arrptr += (stride-subdetx);
  }

  /* if any subpixels are uncoded, label the detector uncoded */
  if((prevsum == 0) && (maskedgeflag == 1) && (corrs->cunbalanced == 0)) {
    sum = 0;
  }
  
  return (double) sum / (subdetx*subdety) / DETMAX;
}
