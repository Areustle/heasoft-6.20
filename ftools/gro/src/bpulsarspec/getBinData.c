#include "bpulsarspec.h"

void getBinData(long *nrows, double **startTime, double **stopTime, 
           int ***Cts, double ***CtsErr, long *bnrows, 
           double **bstartTime, double **bstopTime, int ***binCts,
           double ***binCtsErr, int *binsz) 
{
 double xsum = 0, wsum = 0;
 double zsum = 0, count=0;
 int  ysum = 0, nbin = 0, status=0,ii,j=0,k=0;
 char msg[100]="";     

     /* get bin size */
      strcpy(msg,"binsz");
      Uclgsi(msg, binsz, &status);
      if (status != 0 )
        {
         strcpy(msg,"Problem getting bin size!");
         Fcerr (msg);
         exit (1);
         }
      nbin = (*nrows)/(*binsz);
  
      /* Allocate arrays */
      *bstartTime = (double *) malloc((int)(*nrows) * sizeof(double));
      *bstopTime = (double *) malloc((int)(*nrows) * sizeof(double));
      *binCts = (int **) malloc((int)(*nrows)*1024 * sizeof(int));
      *binCtsErr = (double **) malloc((int)(*nrows)*1024 * sizeof(double));

      xsum = 0;
      wsum = 0; 
      ysum = 0;
      zsum = 0;
       k = 0;
       count = 0;
       for (ii = 0; ii<*nrows; ii++)
	 {
           xsum = xsum + (*startTime)[ii];
           wsum = wsum + (*stopTime)[ii]; 
           count = count+1; 
           if ((ii % (*binsz)) == ((*binsz)-1) )
             {
              (*bstartTime)[k]   = xsum/count;
	      (*bstopTime)[k]    = wsum/count;
               xsum = 0;
               wsum = 0;
               k = k+1;
               count = 0;
	     }
	 }
     
      for (j=0;j<1024;j++)
          {
           k=0;
           ysum = 0;
           zsum =0;
           count = 0;          
	   (*binCts)[j] = (int *) malloc((int)(*nrows)*sizeof(int));
	   (*binCtsErr)[j] = (double *) malloc((int)(*nrows)*sizeof(double));
         for (ii = 0; ii<*nrows; ii++)
           {   
               ysum = ysum + (*Cts)[ii][j];
               zsum = zsum + (*CtsErr)[ii][j]*(*CtsErr)[ii][j]; 
               count = count+1;      
            if ((ii % (*binsz)) == 0 )
             {
	       (*binCts)[j][k] = ysum;
	       (*binCtsErr)[j][k] = sqrt(zsum); /*/(count+1));*/
               ysum = 0;
               zsum =0;
               k=k+1; 
               count = 0;    
             }	   
	   }
	  }
      *bnrows = k;
 
}
