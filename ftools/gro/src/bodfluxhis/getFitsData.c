
#include "bodfluxhis.h"
void getFitsData(char **listfn,long *nrows, int *nfile, float **startTime, 
 float **stopTime, float **FluxFits,float **FluxErrFits, float **startTimeFits)

{
  fitsfile *infptr; /* pointer to the FITS file*/
  int status = 0, hdunum, hdutype,i,j=0,ifile;
  int   anynull = 0, intnull = 0, fltnull=0;
  long  flen,rows=0, irow;
  float  *iFlux = NULL, *iFluxErr = NULL, *istartTime = NULL,*istopTime = NULL;
  float  *istartSec = NULL;
  float  epsilon=0.0001, xx, yy;
  char   *fn="";
  long   test16=0, test18=0, longnull=0,  *flag=NULL;
  long   flag18=262144, flag16=65536;


  irow = 0;  
    for (ifile = 0; ifile<*nfile; ifile++)
    {

    /* open Fits file */ 
      fn = (char*) malloc(50);  
      strcpy(fn,listfn[ifile]);
   
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
         printf ("Problem getting number of rows!");
         exit (1);
	 }

    /* close the file */
    if ( fits_close_file(infptr, &status) )             
        {
         printf ("Problem close fits file!");
         exit (1);  
	}  
    irow = rows+irow;
   }
    *nrows = irow;
    
   /* allocate arrays */
    *startTime     = (float *) malloc((int)irow * sizeof(float));
    *stopTime      = (float *) malloc((int)irow * sizeof(float));
    *FluxFits      = (float *) malloc((int)irow * sizeof(float));
    *FluxErrFits   = (float *) malloc((int)irow * sizeof(float));
    *startTimeFits = (float *) malloc((int)irow * sizeof(float));
    
    j = 0;
    for (ifile = 0; ifile<*nfile; ifile++)
    {
      /* open Fits file */
      strncpy(fn,listfn[ifile],(strlen(listfn[ifile])-1));
      if (*nfile == 1) strncpy(fn,listfn[ifile],(strlen(listfn[ifile])));  
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
         printf ("Problem getting number of rows!");
         exit (1);
	 }
   
        /* allocate arrays */
    istartTime = (float *) malloc((int)rows * sizeof(float));
    istopTime  = (float *) malloc((int)rows * sizeof(float));
    iFlux      = (float *) malloc((int)rows * sizeof(float));
    iFluxErr   = (float *) malloc((int)rows * sizeof(float));
    istartSec  = (float *) malloc((int)rows * sizeof(float));
    flag       = (long *)  malloc((int)rows * sizeof(long));

    /*  read the columns of the table */  
    fits_read_col(infptr, TFLOAT, 1, 1L, 1L, rows, &intnull, istartTime,
                  &anynull, &status);
    fits_read_col(infptr, TFLOAT, 3, 1L, 1L, rows, &intnull, istopTime,
                  &anynull, &status);
    fits_read_col(infptr, TFLOAT, 5, 1L, 1L, rows, &fltnull, iFlux,
                  &anynull, &status);
    fits_read_col(infptr, TFLOAT, 6, 1L, 1L, rows, &fltnull, iFluxErr,
                  &anynull, &status);
    fits_read_col(infptr, TFLOAT, 2, 1L, 1L, rows, &intnull, istartSec,
                  &anynull, &status);
    fits_read_col(infptr, TLONG, 21, 1L, 1L, rows, &longnull, flag,
                  &anynull, &status);

    /* accumulate time, flux & sigma when bits 16 & 18 of the column 21 long-int are not set */

    for (i=0;i<rows;i++) 
      {

	test16 = (flag[i] & BIT16) - flag16;   /* this nonsense is necessary to screen out unwanted */
	test18 = (flag[i] & BIT18) - flag18;   /* rows of the flux history data table; crs,05/2004  */

        if ( (test16 !=0) && (test18==0) ) {
         (*startTime)[j]     = istartTime[i]+istartSec[i]/86400.;    /*if times are distinct */
         (*stopTime)[j]      = istopTime[i];                         /*pack into arrays & increment */
         (*FluxFits)[j]      = iFlux[i];                             /* indicies  */
         (*FluxErrFits)[j]   = iFluxErr[i]; 
         (*startTimeFits)[j] = istartTime[i]+istartSec[i]/86400.;
         j++; 
	}

    }

    /* close the file */
       if ( fits_close_file(infptr, &status) )                
        {
         printf ("Problem close fits file!");
         exit (1);  
	} 
   }
    *nrows=j;
}
