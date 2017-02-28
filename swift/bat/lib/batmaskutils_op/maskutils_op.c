#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "batmask_op.h"
#include "batdet_op.h"

/* 
 * Mask and forward projection utility functions
 * 
 * C. Markwardt
 *
 * $Id: maskutils_op.c,v 1.1 2003/10/28 19:14:08 krimm Exp $ 
 */


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
  int subcellx,  subcelly;   /* Number of subpixels per detector */
  double subpixsize;
  int maskedgeflag = 0;

  subpixsize = corrs->subpixsize;
  subpitchx = rint(detplane->cellsize[0]/subpixsize);
  subpitchy = rint(detplane->cellsize[1]/subpixsize);

  subcellx  = rint(detplane->detsize[0]/subpixsize);
  subcelly  = rint(detplane->detsize[1]/subpixsize);

  arrptr = arr;
  for (k=0; k<subcelly; k++) {

    for (l=0; l<subcellx; l++) {
      if(*arrptr == 0) maskedgeflag = 1;
      sum += *arrptr++;
    }
    arrptr += (stride-subcellx);
  }
  
  /* if any subpixels are uncoded, label the detector uncoded */
  if(prevsum == 0 && maskedgeflag == 1) {
	  sum = 0;
  }
  return (double) sum / (subcellx*subcelly) / DETMAX;
}
