#include <string.h>
#include <stdio.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"
#include "bpulsarspec.h"

 void getSelectData(long *nrows, double *tjdsta, double *tjdstp, 
      double **startTime, double **stopTime, int *phsta, int *phstp,
      int ***binCts, double **stime, double ***sbinCts, long *snrows,
      double ***binCtsErr, double ***sbinCtsErr, double **sdt)
{
      int  i = 0, ii=0, jj=0, irow=0, nph;
      double sum=0, tmpdt1=0, tmpdt2=0;
      double sumerr=0;

      *sdt = (double *) malloc((int)(*nrows) * sizeof(double));
      *stime = (double *) malloc((int)(*nrows) * sizeof(double));
      *sbinCts = (double **) malloc(*nrows*1024*sizeof(double *));
      *sbinCtsErr = (double **) malloc(*nrows*1024*sizeof(double *));
     
       for (i=0;i<*nrows;i++)
          {
	    (*sbinCts)[i] = (double *) malloc(1024*sizeof(double));
            (*sbinCtsErr)[i] = (double *) malloc(1024*sizeof(double));
            sum= 0.0;
            sumerr = 0.0; 
            nph = 0;
            tmpdt1 =  (*startTime)[i] - (*tjdsta);
            tmpdt2 =  (*tjdstp) - (*stopTime)[i];

	    if ( (tmpdt1 >= -0.000000001) && (tmpdt2 >= -0.000000001) )  
             
	    {  
              for (irow=0;irow<1024;irow++)	
	        {  
                  if (((irow%64)-(*phsta-1))>=-0.0000001 && 
                       ((*phstp-1)-(irow%64))>=-0.00000001)
		    {
		      sum = sum + (*binCts)[irow][i];
                      sumerr  = sumerr + (*binCtsErr)[irow][i]
                                *(*binCtsErr)[irow][i];
                      nph = nph+1;
		    }          
		     if  ((irow%64) == 63)
		       {
                         (*sbinCts)[ii][jj] = sum;
                         (*sbinCtsErr)[ii][jj] = sqrt(sumerr)/*/(nph+1))*/; 
                         (*stime)[ii] = (*startTime)[i];
                         (*sdt)[ii] = (*stopTime)[i] - (*startTime)[i];
                         jj++;
                         sum= 0.0;
                         sumerr = 0.0;
                         nph = 0;
		       }
		}
	     ii++;
             jj = 0.0;
	    }
	  }
      *snrows = ii;
}
