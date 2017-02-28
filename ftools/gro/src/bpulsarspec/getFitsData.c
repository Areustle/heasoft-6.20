
#include "bpulsarspec.h"
void getFitsData(char *fn,long *nrows, double **startTime, 
		 double **stopTime, int ***Cts, double ***CtsErr)

{
  fitsfile *infptr; /* pointer to the FITS file*/
  int status = 0, hdunum, hdutype,i,irow,k=0;
  int anynull = 0, intnull = 0;
  double doublnull=0;
  long  rows=0;
  long   *tmp=NULL;
  double  CtsNum[16], sum = 0., tmpConst = 1.0;
  
      if (fits_open_file(&infptr,fn, READONLY, &status) )
	{
         printf("error opening bodfil\n ");
         exit(1);
	}
    
      /* move to the 3rd HDU to read table */
     hdunum = 4;  
     if ( fits_movabs_hdu(infptr, hdunum, &hdutype, &status) ) 
           printf("error moves to 3rd HDU\n");
     
     /* check if it is a binary table */ 
    if (hdutype != BINARY_TBL)
    {
          printf("Error: Table is not a BINARY TABLE\n");
          
    }
    /* get number of rows in the table*/
    status = fits_get_num_rows(infptr, &rows, &status);
    if (status != 0 )
	{
         printf ("Problem getting number of rows!\n");
         exit (1);
	 }
    *nrows = rows;
   
   /* allocate arrays */
    *startTime = (double *) malloc((int)rows * sizeof(double));
    *stopTime = (double *) malloc((int)rows * sizeof(double));
 
    /*  read the columns of the table */  
     fits_read_col(infptr, TDOUBLE, 1, 1L, 1L, rows, &doublnull,*startTime,
                  &anynull, &status);
       if (status != 0 )
	 {
          printf ("Problem getting start time!\n");
          exit (1);
	 }

      fits_read_col(infptr, TDOUBLE, 3, 1L, 1L, rows, &doublnull, *stopTime,
                  &anynull, &status);
      if (status != 0 )
	{
         printf ("Problem getting stop time!\n");
         exit (1);
	 }

      *Cts = (int **) malloc(*nrows*sizeof(int *));     
      for (i=0;i<*nrows;i++)
      {
	(*Cts)[i] = (int *) malloc(1024*sizeof(int));
        tmp = (long *) malloc((long)1024 * sizeof(long));  
        ffgcvj(infptr, 12, i+1, 1, 1024, intnull, tmp,
	  &anynull, &status);
       	 
        for (irow=0;irow<1024;irow++)	
	  {  
            (*Cts)[i][irow] = tmp[irow];
	    }
       }
         
      for (i=0;i<16;i++) CtsNum[i]=0.0;

      /* get total number of Cts */
       sum = 0.;
       for (irow=0;irow<1024;irow++)	
	 {
         for (i=0;i<*nrows;i++)	
              sum = sum + (*Cts)[i][irow];
              if  ((irow%64) ==  63)
		{
                 CtsNum[k] = sum;
                 k = k +1;
                 sum = 0.;
		}
	  }
     

       /* get counts error */
        *CtsErr = (double **) malloc(*nrows*sizeof(double *));
       for (i=0;i<*nrows;i++)
	 {
          k = 0;
          (*CtsErr)[i] = (double *) malloc(1024*sizeof(double));
          for (irow=0;irow<1024;irow++)
            {
	      tmpConst = (*Cts)[i][irow]   /*/CtsNum[k]*/;
	     (*CtsErr)[i][irow] = sqrt(tmpConst);
             if  ((irow%64) ==  63) k=k+1;
            }
            
	 }
}
  




