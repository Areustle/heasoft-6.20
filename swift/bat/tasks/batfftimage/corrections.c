#include <fitsio.h>
#include <math.h>
#include <string.h>   /* Only source detection */
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "batmask.h"
#include "batdet.h"
#include "bat_gswdev.h"

#include "coordfits.h"
#include "batfftimage.h"

/* 
 * Task to reconstruct sky image from BAT detector plane image
 *   - routines for image transformations and corrections
 *
 * $Id: corrections.c,v 1.3 2005/05/02 06:31:58 craigm Exp $
 *
 * 27 Mar 2005 - split from batfftimage.c
 * 
 */

/* Do post-processing on image to make it ready for writing to the output 
 *
 * struct parm_struct *parms - task parameters
 * struct image_struct *outimg - output image (raw upon input, corrected upon output)
 * int pcodemap - partial coding correction flag
 * struct image_struct *focal - focal plane image
 * struct image_struct *aperture - aperture map
 * struct batmaskplane_struct *mask - BAT mask alignment
 * struct batdetplane_struct *detplane - BAT detector alignment
 * struct wcs_coord *wcs - WCS coordinate system structure
 * FLOAT nullval - pixel null value
 * int ndet - number of enabled detectors
 * double *pmwfcorr - mask weighting correction factor (upon return)
 *
 * RETURNS: status variable (0=success)
 */
 
int image_finalize(struct parm_struct *parms,
		   struct image_struct *outimg, 
		   struct image_struct *pcodeimg,
		   int imgtype,
		   struct image_struct *focal,
		   struct image_struct *aperture,
		   struct batmaskplane_struct *mask,
		   struct batdetplane_struct *detplane,
		   struct wcs_coord *wcs, FLOAT nullval, int ndet,
		   double *pmwfcorr)
		   
{
  int i, j;
  double x, y, z, mwfcorr;
  int pcodecorr, cflatfield, ccosine;
  int pcodemap = imgtype;
  int is_pcodemap = ((pcodemap == PCODEMAP_FILE) || (pcodemap == PCODEMAP_APPEND) ||
		     (pcodemap == PCODEMAP_LAST));

#define min(x,y) ((x<y)?(x):(y))
  /* Source height */
  z = parms->srcpos[2];
  if (z < 100) { z = 1e7; }

  pcodecorr = parms->pcodecorr;
  cflatfield = parms->cflatfield;
  ccosine = parms->ccosine;

  /* No rebalancing occurs if the partial coding map is being computed */
  if (is_pcodemap) {
    pcodecorr = 0;  /* We don't correct the correction! */

    /* These corrections are inverted */
    cflatfield = -cflatfield;  /* XXX problem */
    ccosine = -ccosine;
  }


  /* -------------- */
  /* Correct image for "flat fielding" effect */
  if ((cflatfield || ccosine) && (imgtype != SIGNIFMAP)) {
    double imx, imy, r;
    double cos_edge;
    
    headas_chat(5, "...correcting image for flat fielding...\n");

    /* NOTE: **ALWAYS** right handed at this point, since the image
       flip to left-handed (when appropriate) doesn't occur until below */
    compute_crpixdelt(outimg->axes, parms, focal, aperture, mask, detplane, 
		      HANDEDNESS_RIGHT, wcs);


    for (j=0; j<outimg->axes[1]; j++) {
      imy = xform2_ph(*wcs,(j+1));         /* Tangent plane position: Y */
      for (i=0; i<outimg->axes[0]; i++) {
	imx = xform1_ph(*wcs,(i+1));       /* Tangent plane position: X */

	x = imx * z;  if (x == 0) { x = 1e-20; }
	y = imy * z;  if (y == 0) { y = 1e-20; }

	r = sqrt(x*x + y*y + z*z);
	cos_edge = z/r;
	
	if (cflatfield) {
	  cos_edge += (min(0.15,0.05*fabs(z/x))*fabs(x/r) +
		       min(0.15,0.05*fabs(z/y))*fabs(y/r));
	}

	if ((cflatfield < 0) || (ccosine < 0)) {
	  cos_edge = 1./cos_edge;
	}
	outimg->datap[j][i] /= cos_edge;
	
      }
    }
  }

  /* -------------- */
  /* Correct image for partial coding fraction */
  if (pcodecorr && pcodeimg && (imgtype == FLUXMAP || imgtype == BKGVARMAP)) {
    headas_chat(5, "...correcting image for partial coding...\n");
    foreach_pixel(outimg, i, 
      { if (pcodeimg->data[i] > 0) outimg->data[i] /= pcodeimg->data[i]; \
        else outimg->data[i] = nullval;});
  }

  if (pcodecorr && pcodeimg && (imgtype == SIGNIFMAP)) {
    headas_chat(5, "...masking significance image...\n");
    foreach_pixel(outimg, i, 
      {if (pcodeimg->data[i] < parms->pcodethresh) outimg->data[i]=nullval;});
  }

  /* Correct image for number of detectors */
  if (parms->cndet && (imgtype == FLUXMAP || imgtype == BKGVARMAP)) {
    headas_chat(5, "...normalizing image by number of detectors...\n");
    foreach_pixel(outimg, i, 
         { if (outimg->data[i] != nullval) outimg->data[i] /= ndet; });
  }

  /* -------------- */
  /* Correct image for mask weighting technique */
  {
    double srcpos1[3] = {0,0,0};
    srcpos1[2] = z;
    mwfcorr = maskwtnorm(srcpos1,mask,detplane)*(1+parms->maskwtswgain);
  }

  if (parms->cmaskwt && (imgtype == FLUXMAP || imgtype == BKGVARMAP)) {
    headas_chat(5, "...correcting image for mask weighting technique (factor=%f)...\n",
		mwfcorr);
    foreach_pixel(outimg, i, 
         { if (outimg->data[i] != nullval) outimg->data[i] /= mwfcorr; });
  }

  /* -------------- */
  /* Flip image if needed */
  if (parms->handedness == HANDEDNESS_LEFT) {
    int nx = outimg->axes[0], ny = outimg->axes[1];
    FLOAT temp;

    for (j=0; j<ny; j++) {
      for (i=0; i<(nx+1)/2; i++) {
	/* Perform the swap here */
	temp = outimg->datap[j][i];
	outimg->datap[j][i] = outimg->datap[j][nx-1-i];
	outimg->datap[j][nx-1-i] = temp;
      }
    }
  }


  if (pmwfcorr) *pmwfcorr = mwfcorr;
  return 0;
}
