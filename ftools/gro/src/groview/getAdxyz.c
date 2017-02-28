/*
C ADXYZ: THIS ROUTINE EXPRESSES CARTESIAN DIRECTION COSINES
C CORRESPONDING TO THE CELESTIAL COORDINATES ALPHA,DELTA ,
C WHERE +X-AXIS IS @ 0 DEGREES AZIMUTH, +Z-AXIS @ 90 DEGREES ALT :
C
C           INPUT ANGLES IN DEGREES; CONVERT INTERNALLY
*/

#include "groview.h"

void getAdxyz(float **ra, float **dec, float **x,
              float **y,float **z, long *npt)
{
  double a,d;
  int i;

  *x = (float *) malloc((long)(*npt) * sizeof(float));
  *y = (float *) malloc((long)(*npt) * sizeof(float));
  *z = (float *) malloc((long)(*npt) * sizeof(float));
  
  for (i=0; i<*npt;i++)
    {
     a = (*ra)[i] * RAD;
     d = (*dec)[i] * RAD ;    /* dec BETWEEN +/- 90 DEGREES */  
     (*x)[i] = cos(a) * cos(d);
     (*y)[i] = sin(a) * cos(d);
     (*z)[i] = sin(d);
    }

}
