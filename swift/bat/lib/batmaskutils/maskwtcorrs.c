#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "batmask.h"
#include "batdet.h"
#include "headas.h"

/* 
 * Mask weighting corrections
 * 
 * C. Markwardt
 *
 * $Id: maskwtcorrs.c,v 1.7 2004/10/03 10:37:04 craigm Exp $
 */

#define min(x,y) ((x<y)?(x):(y))

/* 
 * maskwtcorrs - compute corrections to mask weighting
 *
 * double detpos[3] - BAT_X/Y/Z position at detector plane (cm) 
 * double srcpos[3] - BAT_X/Y/Z position of source (cm)
 * struct batmaskcorrections_struct *corrs - corrections to apply
 *
 * RETURNS: correction to be applied to mask weight.  
 *     correction = maskwtcorrs(detpos, srcpos, corrs);
 *     corrected_weight = uncorrected_weight * correction;
 */
double maskwtcorrs(double detpos[3], double srcpos[3],
		   struct batmaskcorrections_struct *corrs)
{
  double x, y, z, x0, y0;
  double rtot2, rcent2, rtot, rcent;
  double wtcorr, costh;

  wtcorr = 1;
  if (corrs == 0) return wtcorr;

  x = (srcpos[0]-detpos[0]);
  y = (srcpos[1]-detpos[1]);
  z = (srcpos[2]-detpos[2]);
  rcent2 = srcpos[0]*srcpos[0] + srcpos[1]*srcpos[1] + srcpos[2]*srcpos[2];
  rcent = sqrt(rcent2);

  rtot2  = x*x + y*y + z*z;
  rtot   = sqrt(rtot2);
  costh  = z / rtot;
 
  if (corrs->ccosine) {
    wtcorr *= costh;
    /* headas_chat(5, " cosine=%f\n", costh); */
  }

  if (corrs->crsquare) {
    wtcorr *= rcent2/rtot2;
    /* headas_chat(5, " rsquare=%f\n", rcent2/rtot2); */
  }
  
  /* Tueller/Hullinger flat fielding correction */
  if (corrs->cnearfield || corrs->cflatfield) {
    double cos_edge, cos_edge_cent;
    double costh_cent;
    double xc = srcpos[0], yc = srcpos[1];

    x0 = x;  if (x0 == 0) x0 = 1e-20;  /* Guard against divide-by-zero */
    y0 = y;  if (y0 == 0) y0 = 1e-20;

    cos_edge = (costh +
		min(0.15,0.05*fabs(z/x0))*fabs(x/rtot) +
		min(0.15,0.05*fabs(z/y0))*fabs(y/rtot));

    if (corrs->cnearfield) {
      /* Near field, desire to correct cosine effect to center of array */

      costh_cent = z / rcent;
      cos_edge_cent = costh_cent +
	min(0.15,0.05*fabs(z/x0))*fabs(xc/rcent) +
	min(0.15,0.05*fabs(z/y0))*fabs(yc/rcent);

      wtcorr *= (cos_edge / cos_edge_cent) * (rcent2/rtot2); 
    } else {

      /* Far field - only cosine-type corrections */
      wtcorr *= cos_edge;
    }

  }

  /* If the direction is forward, we return the weight so that we can
     deweight bright parts of the detector array... */
  if (corrs->direction == RAYTRACE_FORW) return wtcorr;

  /* ... for back projection, we return 1/weight, so that we can
     enhance the weight of the bright parts of the detector array */
  if (wtcorr == 0) return 0;
  return (1.0/wtcorr);
}

/*
 * maskwtnorm - compute mask weight normalization
 *
 * The mask weighting technique (i.e. mask-detector cross correlation)
 * produces an image with units of "counts" but with a more or less
 * arbitrary intensity scale.  This routine computes the normalization
 * based on the mask-detector geometry, and assuming the distribution
 * of illumination fractions is known.
 *
 * double srcpos[3] - source position in BAT_X/Y/Z (cm)
 * struct batmaskplane_struct *mask - mask orientation
 * struct batdetplane_struct *detplane - detector plane orientation
 *
 * RETURNS: mask weight technique normalization.
 *    Users are meant to take (maskwtval)/(maskwtnorm) to arrive at
 *    the true sky counts, where maskwtval is the weighted sum
 *    measured from the detector data.
 *
 */
double maskwtnorm(double srcpos[3],
		  struct batmaskplane_struct *mask, 
		  struct batdetplane_struct *detplane)
{
  double z;      /* Source height, BAT_Z, cm */
  double h;      /* Focal length, cm */
  double lx, ly; /* Detector sizes, x, y, cm */
  double dx, dy; /* Mask cell sizes, x, y, cm */
  double ldx, ldy; /* Ratios, lx/dx, ly/dy */
  double fopen = 0.5;  /* Open fraction */
  double pfull;

  if ((mask == 0) || (detplane == 0) || (srcpos == 0)) {
    return 0;
  }

  z = srcpos[2];

  /* Focal length */
  h = (mask->centpos[2] + mask->meanpos[2] - mask->cellsize[2]/2)
    - (detplane->centpos[2] + detplane->meanpos[2]);

  /* Detector sizes (not the cell sizes) */
  lx = detplane->detsize[0]; ly = detplane->detsize[1];

  /* Mask cell sizes */
  dx = mask->cellsize[0];    dy = mask->cellsize[1];

  /* Ratio of detector size to projected mask cell size */
  ldx = (lx/dx) * (z-h)/z;
  ldy = (ly/dy) * (z-h)/z;

  /* Probability of a fully illuminated detector */
  pfull = fopen*((1-ldx)*(1-ldy)   /* Exact single-cell det/mask overlap */
		 + fopen*((1-ldx)*(ldy) + (1-ldy)*(ldx)) /* Adjacent open cell */
		 + fopen*fopen*fopen*ldx*ldy);           /* Four open cells */

  /* Expression for cross correlation normalization of random grid of
     rectangular mask cells */
  return (1.0 + 4.0*pfull)/6.0;
  
}
		  
