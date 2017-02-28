#include <string.h>
#include <stdio.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"

 void getPhasData(long *nrows, double *tjdsta, double *tjdstp, 
      double **startTime, double **stopTime, int *phsta, int *phstp,
      int ***binCts, double ***binPhas,double ***binCtsErr,
       double ***binPhasErr)
{
      int  i = 0, ii=0, jj=0, irow=0, nph, ntime;
      double sum=0, tmp;
      double sumerr=0;

      nph = *phstp - *phsta +1;
      *binPhas = (double **) malloc(1024*sizeof(double *));
      *binPhasErr  = (double **) malloc(1024*sizeof(double *));
      
      for (irow=0;irow<1024;irow++)	
        {  
          (*binPhas)[0] = (double *) malloc(64*sizeof(double));
          (*binPhasErr)[0] = (double *) malloc(64*sizeof(double));
             ntime = 0;
             sum =0.;
             sumerr=0.0; 
             for (i=0;i<*nrows;i++)
	       {
                if (((*startTime)[i]-(*tjdsta))>=-0.0000001 &&
                      ((*tjdstp)-(*stopTime)[i])>=-0.0000001)
		  {
		    tmp = (*stopTime)[i] - (*startTime)[i];
		   sum = sum + (*binCts)[irow][i]/(tmp*86400.0);
                   sumerr  = sumerr + (*binCts)[irow][i]/(tmp*86400.0
                             *tmp*86400.0);
                   ntime = ntime + 1;
		  }
		}
             (*binPhas)[ii][jj] = sum;
	     (*binPhasErr)[ii][jj] = sqrt(sumerr);/*/ntime);*/
             jj++;
            
            if  ((irow%64) == 63)
	      {                  
               ii++;
               jj = 0;
               sum= 0.0;
               sumerr = 0.0; 
              (*binPhas)[ii] = (double *) malloc(64*sizeof(double));
              (*binPhasErr)[ii] = (double *) malloc(64*sizeof(double));   
	      }
	}

   
}
