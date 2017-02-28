#include <string.h>
#include <stdio.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"
#include "bcont.h"
 
 void getSelectData(long *binrows, double *tjdsta, double *tjdstp,
        double **binTime, double **sbinTime, int *chansta, int *chanstp,
        double ***binCts, double ***sbinCts, long *snrows, double ***binCtsErr,
        double ***sbinCtsErr,int *nchan, long *ncol, float **binTimeRes,
        float **sTimeRes)
{
      long  i = 0, ii=0, jj=0, irow=0, nph;
      double tmpdt1=0, tmpdt2=0;
      double sumerr=0;
      double sum=0;

      *sTimeRes = (float *) malloc((long)(*binrows) * sizeof(float));
      *sbinTime = (double *) malloc((long)(*binrows) * sizeof(double));
      *sbinCts = (double **) malloc(*binrows*8*sizeof(double *));
      *sbinCtsErr = (double **) malloc(*binrows*8*sizeof(double *));
     
       for (i=0;i<*binrows;i++)
          {
	    (*sbinCts)[i] = (double *) malloc(8*sizeof(double));
            (*sbinCtsErr)[i] = (double *) malloc(8*sizeof(double));
            sum= 0.0;
            sumerr = 0.0; 
            nph = 0;
            tmpdt1 =  (*binTime)[i] - (*tjdsta);
            tmpdt2 =  (*tjdstp) - (*binTime)[i];

	    if ( (tmpdt1 >= -0.00000000001) && (tmpdt2 >= -0.00000000001) )  
             
	    {  
              for (irow=0;irow<(*ncol);irow++)	
	        {  
                  if (((irow%(*nchan))-(*chansta-1))>=-0.0000001 && 
                       ((*chanstp-1)-(irow%(*nchan)))>=-0.00000001)
		    {
		      sum = sum + (*binCts)[irow][i]*1.0;
                      sumerr  = sumerr + (*binCtsErr)[irow][i]
                                *(*binCtsErr)[irow][i];
                      nph = nph+1;
		    }          
		     if  ((irow%(*nchan)) == (*nchan-1))
		       {
                         (*sbinCts)[ii][jj] = sum;
                         (*sbinCtsErr)[ii][jj] = sqrt(sumerr); 
                         (*sbinTime)[ii] = (*binTime)[i];
                         (*sTimeRes)[ii] = (*binTimeRes)[i];
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










