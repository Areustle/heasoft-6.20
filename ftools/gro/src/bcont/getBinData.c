#include "bcont.h"

void getBinData(long *nrows, double **midTime, 
           long ***Cts, double ***CtsErr, long *binrows, 
           double **binTime, double ***binCts,
           double ***binCtsErr, int *binsz, long *ncol,float **timeRes,
           float **binTimeRes)
{
 double xsum = 0, wsum = 0, ysum = 0;
 double zsum = 0, count=0;
 float tsum = 0.;
 long  nbin = 0, ii,j=0,k=0;
 char msg[100]="";     
 int status=0;


      nbin = (*nrows)/(*binsz);
    
      /* Allocate arrays */
      *binTimeRes = (float *) malloc((long)(*nrows) * sizeof(float));
      *binTime = (double *) malloc((long)(*nrows) * sizeof(double));
      *binCts = (double **) malloc((long)(*nrows)*128 * sizeof(double *));
      *binCtsErr = (double **) malloc((long)(*nrows)*128 * sizeof(double *));

      xsum = 0;
      tsum = 0;
      wsum = 0; 
      ysum = 0;
      zsum = 0;
       k = 0;
       count = 0;
       for (ii = 0; ii<*nrows; ii++)
	 {
           xsum = xsum + (*midTime)[ii];
           tsum = tsum + (*timeRes)[ii];
           count = count+1; 
           if ((ii % (*binsz)) == ((*binsz)-1) )
             {
              (*binTime)[k]   = xsum/count;
              (*binTimeRes)[k] = tsum/count;
               xsum = 0;
               tsum = 0;
               wsum = 0;
               k = k+1;
               count = 0;
	     }
	 }
     
      for (j=0;j<*ncol;j++)
          {
           k=0;
           ysum = 0;
           zsum =0;
           count = 0;          
	   (*binCts)[j] = (double *) malloc((long)(*nrows)*sizeof(double));
	   (*binCtsErr)[j] = (double *) malloc((long)(*nrows)*sizeof(double));
         for (ii = 0; ii<*nrows; ii++)
           {   
               ysum = ysum + (*Cts)[ii][j];
               zsum = zsum + (*CtsErr)[ii][j]*(*CtsErr)[ii][j]; 
               count = count+1;      
            if ((ii % (*binsz)) == 0 )
             {
	       (*binCts)[j][k] = ysum;
	       (*binCtsErr)[j][k] = sqrt(zsum);
               ysum = 0;
               zsum =0;
               k=k+1; 
               count = 0;    
             }	   
	   }
	  }
      *binrows = k;
 
}
