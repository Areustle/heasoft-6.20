#include "bpulsarspec.h"

void getSumData(long *snrows, double ***sbinCts, double ***sbinCtsErr, 
     double **sumCts, double **sumCtsErr, double **sdt, double **sumRate,
     double **sumRateErr)
{
     int i=0, j=0;
     double sum,sumrate,yo[2000],yn[2000];
     double sumerr=0.,sumraterr=0.;

     *sumCts = (double *) malloc(16 * sizeof(double));
     *sumCtsErr = (double *) malloc(16 * sizeof(double));
     *sumRate = (double *) malloc(16 * sizeof(double));
     *sumRateErr = (double *) malloc(16 * sizeof(double));

     for (j=0;j<16;j++)
       {
         sumrate = 0.0;
         sum = 0.0;
         sumerr=0.0;
         sumraterr = 0.0;
         for(i=0;i<*snrows;i++)
	   {
	   sum = sum+ (*sbinCts)[i][j];
           sumerr = sumerr + (*sbinCtsErr)[i][j] * (*sbinCtsErr)[i][j];
           sumrate = sumrate + (*sbinCts)[i][j]/((*sdt)[i]*86400.0); 
	   sumraterr = sumraterr + (*sbinCts)[i][j]/((*sdt)[i]*86400.0
                      *(*sdt)[i]*86400.0); 
	   }
         (*sumCts)[j] = sum;
         (*sumCtsErr)[j]= sqrt(sumerr);/*/(*snrows+1));*/
         (*sumRate)[j] = sumrate; 
         (*sumRateErr)[j] = sqrt(sumraterr);/*/(*snrows+1));*/
       }

}
