/*
C ADXYZ: THIS ROUTINE EXPRESSES CARTESIAN DIRECTION COSINES
C CORRESPONDING TO THE CELESTIAL COORDINATES ALPHA,DELTA ,
C WHERE +X-AXIS IS @ 0 DEGREES AZIMUTH, +Z-AXIS @ 90 DEGREES ALT :
C
C           INPUT ANGLES IN DEGREES; CONVERT INTERNALLY
*/

#include "groview.h"

void getSpaceCor(float **ra, float **dec, float **sx,
              float **sy,float **sz, long *npt)
{
  double a,d;
  int i;
 
  *sx = (float *) malloc((long)(*npt) * sizeof(float));
  *sy = (float *) malloc((long)(*npt) * sizeof(float));
  *sz = (float *) malloc((long)(*npt) * sizeof(float));
 
  for (i=0;i<*npt;i++)
    {
     a = (*ra)[i] * RAD;
     d = (*dec)[i] * RAD;    /* dec BETWEEN +/- 90 DEGREES */  
     (*sx)[i] = cos(a) * cos(d);
     (*sy)[i] = sin(a) * cos(d);
     (*sz)[i] = sin(d);
     
    }
}
