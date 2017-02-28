#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "batmask_op.h"
#include "batdet_op.h"
#include "headas.h"

/* 
 * Mask weighting corrections
 * 
 * C. Markwardt
 *
 * $Id: maskwtcorrs_op.c,v 1.1 2003/10/28 19:14:08 krimm Exp $
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
  double rtot2, rcent2;
  double wtcorr, costh;

  wtcorr = 1;
  if (corrs == 0) return wtcorr;

  x = (srcpos[0]-detpos[0]);
  y = (srcpos[1]-detpos[1]);
  z = (srcpos[2]-detpos[2]);
  rcent2 = srcpos[0]*srcpos[0] + srcpos[1]*srcpos[1] + srcpos[2]*srcpos[2];

  rtot2  = x*x + y*y + z*z;
  costh = z / sqrt(rtot2);
 
  if (corrs->ccosine) {
    wtcorr *= costh;
     /* headas_chat(5, " cosine=%f\n", costh);  */
  }

  if (corrs->crsquare) {
    wtcorr *= rcent2/rtot2;
    wtcorr *= exp(-0.2562*0.25/costh); /* 0.2562 cm2/g constant carbon@30keV, about 0.25 g/cm2 in mask support structure */
    /* headas_chat(5, " rsquare=%f\n", rcent2/rtot2); */
  }
  
  /* Tueller/Hullinger flat fielding correction */
  if (corrs->cflatfield) {
    double wt1;
    x0 = x;  if (x0 == 0) x0 = 1e-20;  /* Guard against divide-by-zero */
    y0 = y;  if (y0 == 0) y0 = 1e-20;
    wt1 = (costh + 
	   min(0.15,0.05*fabs(z/x0))*cos(atan(sqrt(y*y+z*z)/x0)) +
	   min(0.15,0.05*fabs(z/y0))*cos(atan(sqrt(x*x+z*z)/y0)));
    wtcorr *= wt1*rcent2/rtot2;
    wtcorr *= exp(-0.2562*0.25/costh); /* 0.2562 cm2/g constant, carbon@30keV, about 0.25 g/cm2 in mask support structure */
    /* headas_chat(5, " flatfield=%f\n", wt1*rcent2/rtot2); */
  }

  /* If the direction is forward, we return the weight so that we can
     deweight bright parts of the detector array... */
  if (corrs->direction == 1) return wtcorr;

  /* ... for back projection, we return 1/weight, so that we can
     enhance the weight of the bright parts of the detector array */
  if (wtcorr == 0) return 0;
  return (1.0/wtcorr);
}
