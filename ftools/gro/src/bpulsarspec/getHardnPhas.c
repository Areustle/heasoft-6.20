#include "bpulsarspec.h"

void getHardnPhas(long *npt, double ***sbinCtPhas, double **hardnPhas, 
              double ***sbinCtsErrPhas, double **hardnErrPhas,
              int *chani, int *chanj, int *chanm, int *chann)
{
     int i=0, j=0;
     double sum,yo[2000],yn[2000];
     *hardnPhas = (double *) malloc((long)(*npt) * sizeof(double));
     *hardnErrPhas = (double *) malloc((long)(*npt) * sizeof(double));

     for (i=0;i<*npt;i++)
       {
         sum = 0.;
         for (j=*chani-1;j<*chanj;j++)
	   {
	   sum = sum+ (*sbinCtPhas)[j][i];
	   }
         yo[i] = sum;
       }

     for (i=0;i<*npt;i++)
       {
         sum = 0.;
         for (j=*chanm-1;j<*chann;j++)
	   {
	   sum = sum+ (*sbinCtPhas)[j][i];
       	   }
          yn[i] = sum;
       }
  
      for (i=0;i<*npt;i++)
	{
	(*hardnPhas)[i] = yn[i]/ yo[i];
        (*hardnErrPhas)[i] = (*hardnPhas)[i] * sqrt(1./3.*(1./yn[i]+1./yo[i]));
	}
}
