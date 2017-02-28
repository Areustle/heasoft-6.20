#include <string.h>
#include <stdio.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"

 void getSelectPhas(long *nrows, double *tjdsta, double *tjdstp, 
      double **startTime, double **stopTime, int *phsta, int *phstp,
      int ***binCts, double ***sbinCtPhas,long *nPhas,
      double ***binCtsErr, double ***sbinCtsErrPhas)
{
      int  i = 0, ii=0, jj=0, irow=0, nph, ntime;
      double sum=0;
      double sumerr=0;

      nph = *phstp - *phsta +1;
      *sbinCtPhas = (double **) malloc(1024*sizeof(double *));
      *sbinCtsErrPhas  = (double **) malloc(1024*sizeof(double *));
      
      for (irow=0;irow<1024;irow++)	
        {  
          (*sbinCtPhas)[0] = (double *) malloc(64*sizeof(double));
          (*sbinCtsErrPhas)[0] = (double *) malloc(64*sizeof(double));
          if (((irow%64)-(*phsta-1))>=-0.0000001 && 
              ((*phstp-1)-(irow%64))>=-0.0000001)
	    {
             ntime = 0;
             sum =0.;
             sumerr=0.0; 
             for (i=0;i<*nrows;i++)
	       {
                if (((*startTime)[i]-(*tjdsta))>=-0.0000001 &&
                      ((*tjdstp)-(*stopTime)[i])>=-0.0000001)
		  {
		   sum = sum + (*binCts)[irow][i];
                   sumerr  = sumerr + (*binCtsErr)[irow][i];
                   ntime = ntime + 1;
		  }
		}
             (*sbinCtPhas)[ii][jj] = sum;
	     (*sbinCtsErrPhas)[ii][jj] = sqrt(sumerr);/*/ntime;*/
             jj++;
	    }
            
            if  ((irow%64) == 63)
	      {                  
               ii++;
               jj = 0;
               sum= 0.0;
               sumerr = 0.0; 
              (*sbinCtPhas)[ii] = (double *) malloc(64*sizeof(double));
              (*sbinCtsErrPhas)[ii] = (double *) malloc(64*sizeof(double));   
	      }
	}
	 *nPhas = nph; 

   
}
