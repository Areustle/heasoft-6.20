#include <string.h>
#include <stdio.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"

 void getBindata(int *binsz, long *noutrows,float **bstartTime,float **bFlux,float **bFluxErr,float *startTime,float *Flux,float *FluxErr)
{
     float  xsum = 0, ysum = 0, zsum = 0;
     int   nbin = 0, status = 0, ii, j=0;
      
      nbin = (*noutrows)/(*binsz);
      
      /* Allocate arrays */
      *bstartTime = (float *) malloc((int)(nbin) * sizeof(float));
      *bFlux = (float *) malloc((int)(nbin) * sizeof(float));
      *bFluxErr = (float *) malloc((int)(nbin) * sizeof(float));

      xsum = *startTime;
      ysum = *Flux;
      zsum = *FluxErr;
      ii = 0;
      for (ii = 1; ii<*noutrows+1; ii++)
       {
	 if ((ii % (*binsz)) != 0 ) 
	    {
	    xsum = xsum + startTime[ii];
            ysum = ysum + Flux[ii];
            zsum = zsum + FluxErr[ii];
	    }
	  else
	   {
             (*bstartTime)[j] = xsum/(*binsz);
	     (*bFlux)[j]    = ysum/(*binsz);
             (*bFluxErr)[j] = zsum/(*binsz);
	     xsum = startTime[ii];
             ysum = Flux[ii];
             zsum = FluxErr[ii]; 
             j = j+1;
	     }
       }
        
     
}
