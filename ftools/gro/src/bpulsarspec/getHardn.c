#include "bpulsarspec.h"

void getHardn(long *npt, double ***sbinCts, double **hardn, 
              double ***sbinCtsErr, double **hardnErr, int *chani,
               int *chanj, int *chanm, int *chann)
{
     int i=0, j=0;
     double sum,yo[2000],yn[2000];
     *hardn = (double *) malloc((long)(*npt) * sizeof(double));
     *hardnErr = (double *) malloc((long)(*npt) * sizeof(double));

     for (i=0;i<*npt;i++)
       {
         sum = 0.;
         for (j=*chani-1;j<*chanj;j++)
	   {
	   sum = sum+ (*sbinCts)[i][j];
	   }
         yo[i] = sum;
       }

     for (i=0;i<*npt;i++)
       {
         sum = 0.;
         for (j=*chanm-1;j<*chann;j++)
	   {
	   sum = sum+ (*sbinCts)[i][j];
	   }
          yn[i] = sum;
       }
  
      for (i=0;i<*npt;i++)
	{
	(*hardn)[i] = yn[i]/ yo[i];
        (*hardnErr)[i] = (*hardn)[i] * sqrt(1./3.0*(1./yn[i]+1./yo[i])); 
	}
}
