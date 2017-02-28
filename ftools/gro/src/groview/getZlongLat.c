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
void getZlongLat(float **raY, float **decY, float **zLong, 
     float **zLat, long *npt)
{
  int i;

  *zLong = (float *) malloc((long)(*npt) * sizeof(float));
  *zLat  = (float *) malloc((long)(*npt) * sizeof(float));

  for (i=0; i<*npt; i++)
    {
    (*zLong)[i] = (*raY)[i] *RAD; 
    (*zLat)[i] = (*decY)[i]*RAD;
    }
}
