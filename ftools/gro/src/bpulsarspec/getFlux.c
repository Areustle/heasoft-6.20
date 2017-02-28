#include "bpulsarspec.h"
void getFlux(double ***binPhas, double ***binPhasErr,
		double **fluxphs, double **fluxphsErr, int *chansta, 
                int *chanstp, int *nphs)

{
  int i=0, j=0, k=0, binszPh,status=0;
     double sum1, sum2, fluxphstmp[64],fluxphsErrtmp[64];
     char msg[100]="";

     /* get bin size */
      strcpy(msg,"binszPh");
      Uclgsi(msg, &binszPh, &status);
      if (status != 0 )
        {
         strcpy(msg,"Problem getting phase bin size!");
         Fcerr (msg);
         exit (1);
         }

     for (i=0;i<64;i++)
       {
         sum1 = 0.;
         sum2 = 0.;
            for (j=*chansta-1;j<*chanstp;j++)
	       {
	        sum1 = sum1 + (*binPhas)[j][i];
                sum2 = sum2 + (*binPhasErr)[j][i]* (*binPhasErr)[j][i];
	       }
                fluxphstmp[i] = sum1;
                fluxphsErrtmp[i] = sqrt(sum2/16.);
                
       }


     *fluxphs = (double *) malloc((long)(64) * sizeof(double));
     *fluxphsErr = (double *) malloc((long)(64) * sizeof(double));
     k = 0;
     sum1 = 0.;
     sum2 = 0.;
     for (i=0;i<64;i++)
       {
	 sum1 = sum1 + fluxphstmp[i];
	 sum2 = sum2 + fluxphsErrtmp[i]*fluxphsErrtmp[i];
        if ((i % (binszPh)) == (binszPh-1))
	  {
	    (*fluxphs)[k] = sum1 ;
	    (*fluxphsErr)[k] = sqrt(sum2);
            sum1 = 0;
            sum2 = 0;
            k = k+1;
	  }
       }
        *nphs = k;
}
