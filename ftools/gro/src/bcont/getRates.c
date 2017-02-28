#include <string.h>
#include <stdio.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"
#include "bcont.h"
 
void getRates(long *snrows, double **sumCts, double **sumCtsErr,
               double **Rate, double **RateErr, float **sTimeRes)
{
  long  i =0;
  float timexp=0;
  double sum=0, sumerr=0;

  *Rate = (double *) malloc((long)(*snrows)*sizeof(double));
  *RateErr = (double *) malloc((long)(*snrows)*sizeof(double));
    
         for (i=0;i<(*snrows);i++)
          {
            timexp = (*sTimeRes)[i];
	    (*Rate)[i] = (*sumCts)[i]/timexp;
	    (*RateErr)[i] = sqrt((*sumCts)[i]/(timexp*timexp));
	  }
      
}










