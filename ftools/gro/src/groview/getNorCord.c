/*
C ADXYZ: THIS ROUTINE EXPRESSES CARTESIAN DIRECTION COSINES
C CORRESPONDING TO THE CELESTIAL COORDINATES ALPHA,DELTA ,
C WHERE +X-AXIS IS @ 0 DEGREES AZIMUTH, +Z-AXIS @ 90 DEGREES ALT :
C
C           INPUT ANGLES IN DEGREES; CONVERT INTERNALLY
*/

#include "groview.h"

void getNorCord(float *ra, float *dec, float *x,
              float *y,float *z)
{
  double a,d;

     a = *ra * RAD;
     d = *dec * RAD ;    /* dec BETWEEN +/- 90 DEGREES */  
     *x = cos(a) * cos(d);
     *y = sin(a) * cos(d);
     *z = sin(d);
   
}
