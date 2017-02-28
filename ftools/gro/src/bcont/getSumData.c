
#include <string.h>
#include <stdio.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"
#include "bcont.h"
 
 void getSumData(long *snrows, double **sbinTime, double ***sbinCts,
      double ***sbinCtsErr,double **sumCts, double **sumCtsErr,
      int *ndet, int **det)
{
      int  i = 0,k=0, idet=0;
      double sumerr=0;
      double sum=0;
     
      *sumCts = (double *) malloc(*snrows*sizeof(double));
      *sumCtsErr = (double *) malloc(*snrows*sizeof(double));
       for (i=0;i<*snrows;i++)
          {
            sum= 0.0;
            sumerr = 0.0; 
            for (idet=0;idet<*ndet;idet++)	
	      {
                k= (*det)[idet];
		sum = sum + (*sbinCts)[i][k];
                sumerr  = sumerr + (*sbinCtsErr)[i][k]
                                *(*sbinCtsErr)[i][k];
	      }
             (*sumCts)[i] = sum;
             (*sumCtsErr)[i] = sqrt(sumerr); 
	  }

}










