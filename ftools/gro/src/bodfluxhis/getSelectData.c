#include <string.h>
#include <stdio.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"

 void getSelectData(long *noutrows, long *firstrow, long *lastrow, 
      float **startTime, float **Flux,float **FluxErr, float **FluxFits, 
      float **FluxErrFits, float **startTimeFits)
{
      int  ii, j=0;
   
       /* allocate arrays */
      *startTime = (float *) malloc((int)(*noutrows) * sizeof(float));
      *Flux = (float *) malloc((int)(*noutrows) * sizeof(float));
      *FluxErr = (float *) malloc((int)(*noutrows) * sizeof(float));
     
      for (ii = *firstrow-1; ii < *lastrow+1; ii++)
      {
        (*startTime)[j] =  (*startTimeFits)[ii];
        (*Flux)[j] =  (*FluxFits)[ii];
        (*FluxErr)[j] = (*FluxErrFits)[ii];
        j++;
      }
}
