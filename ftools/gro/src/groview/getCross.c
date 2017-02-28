/*
C CROSS.FOR: SUBROUTINE TO COMPUTE THE VECTOR CROSS PRODUCT A X B = C
C              DEFINED AS
C
C              | i  j  k  |
C         C =  | Ax Ay Az |
C              | Bx By Bz |
*/

#include "groview.h"

void getCross(float **scZx, float **scZy, float **scZz,
                 float **scXx, float **scXy, float **scXz,
                 float **scYx, float **scYy, float **scYz, long *nrows)

{
  int i;

  *scYx = (float *) malloc((long)(*nrows) * sizeof(float));
  *scYy = (float *) malloc((long)(*nrows) * sizeof(float));
  *scYz = (float *) malloc((long)(*nrows) * sizeof(float));

  for (i=0;i<*nrows;i++)
    {
    (*scYx)[i] = (*scZy)[i]*(*scXz)[i] - (*scZz)[i]*(*scXy)[i];
    (*scYy)[i] = (*scZz)[i]*(*scXx)[i] - (*scZx)[i]*(*scXz)[i];
    (*scYz)[i] = (*scZx)[i]*(*scXy)[i] - (*scZy)[i]*(*scXx)[i];
    }
}
      
