/*
C XYZAD: THIS ROUTINE WILL CONVERT CARTESIAN COORINATES X ,Y,Z TO
C           LONG,LAT W/+X-AXIS @ O DEGREES LONGITUDE, +Z-AXIS @
C           90 DEGREES LAT ; X,Y,Z ARE ACTUALLY THE DIRECTION COSINES
C
C           RETURN ANGLES IN DEGREES
C
C         REVISED 8/8/87; (X,Y,Z) NEED NOT BE NORMALIZED
C
*/

#include "groview.h"

void getXyzad(float **x, float **y, float **z,
              float **ra, float **dec, long *npt)
{
  int i;
  double theta ,delta,r, alpha;

  *dec = (float *) malloc((long)(*npt) * sizeof(float));
  *ra  = (float *) malloc((long)(*npt) * sizeof(float));

  for (i=0; i<*npt; i++)
    {
      theta = (*z)[i]/ sqrt((*x)[i]*(*x)[i] + (*y)[i]*(*y)[i] + 
			    (*z)[i]*(*z)[i]);
      delta = asin(theta);

      r = sqrt((*x)[i]*(*x)[i] + (*y)[i]*(*y)[i] );
      if( r <= 0.0) return;
      if( r > 0.0)  alpha = acos((*x)[i]/r);

   /* REMOVE AMBIGUITY*/

    if (((*x)[i]<0.) && ((*y)[i]<0.)) alpha = TWPI - alpha;
    if( ((*x)[i]>0.) && ((*y)[i]<0.)) alpha = TWPI - alpha;
    alpha = alpha/RAD;  /* convert RAD to Deg */
    delta = delta/RAD;
    (*dec)[i] = delta;
    (*ra)[i]  = alpha;
    }
   
}
