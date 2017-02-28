#include "bcont.h"
#include <stdio.h>
void getFitsData(char **listfn,long *nrows, int *nfile, double **midTime, 
     long ***cts, double ***ctsErr, long *ncol, float **timeRes)

{
  fitsfile *infptr; /* pointer to the FITS file*/
  int status = 0, hdunum, hdutype,ifile;
  int ctscol=0,tjdcol=0;
  int  anynull = 0;
  long flen,rows=0, irow=0,i,j,longnull = 0;
  double  *imidTime = NULL,doublenull=0;
  char *fn="", comm[100];
  long *tmp=NULL;
  float val,*tmptime=NULL;
  
    tmptime = malloc((int)(*nfile) * sizeof(float));
    for (ifile = 0; ifile<*nfile; ifile++)
    {

    /* open Fits file */ 
      fn = (char*) malloc(500);  
      strcpy(fn,listfn[ifile]);
   
      if (fits_open_file(&infptr,fn, READONLY, &status) )
	{
         printf("Error opening bcontfil\n ");
         exit(1);
	}
      
      if ( ffmahd(infptr, 1, &hdutype, &status) )
     Printerror( status );
     status = 0;
     if (ffgky(infptr,TFLOAT,"TIME_RES", &val, comm, &status) == 0 )
      tmptime[ifile] = val; 
   
      /* move to the 3rd HDU to read table */
     hdunum = 3;  
     if ( fits_movabs_hdu(infptr, hdunum, &hdutype, &status) ) 
           printf("\n first Error moves to 3rd HDU!\n");
     
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
         printf ("first Problem close fits file!\n");
         exit (1);  
	}  
    irow = rows+irow;
   }
    *nrows = irow;
   
   /* allocate arrays */
    *midTime = (double *) malloc((long)irow * sizeof(double));
    *timeRes = (float *) malloc((long)irow * sizeof(float));    
    *cts = (long **) malloc((long)irow * sizeof(long *));
    *ctsErr = (double **) malloc((long)irow * sizeof(double *));
    
    j=0;
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
     hdunum = 3;  
     if ( fits_movabs_hdu(infptr, hdunum, &hdutype, &status) ) 
       printf("\n!!!!Error moves to 3rd HDU\n");
     
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
   
    /* get number of column */

    if (ffgcno(infptr, CASEINSEN, "COUNTS",&ctscol, &status) > 0)
    {
       printf ("column containing 'COUNTS' in table cell does not exist!");
       exit (1);
    }
    

    if (ffgcno(infptr, CASEINSEN,"MID_TIME",&tjdcol, &status) > 0)
	{
         printf ("Problem getting the column number of MID_TIME!");
         exit (1);
	 }

    /* allocate arrays */
    imidTime =  (double *)malloc((long)rows * sizeof(double));

    /*  read the columns of the table */  
    fits_read_col(infptr, TDOUBLE, tjdcol, 1L, 1L, rows, &doublenull, imidTime,
                  &anynull, &status); 
       
    for (i=0;i<rows;i++) 
      {
        (*midTime)[j] = imidTime[i];
	(*timeRes)[j] = tmptime[ifile];
	(*cts)[j] = (long *) malloc((long)(*ncol) * sizeof(long));
	(*ctsErr)[j] = (double *) malloc((long)(*ncol) * sizeof(double));
        tmp = (long *) malloc((long)128 * sizeof(long));
        ffgcvj(infptr, ctscol, i+1, 1, *ncol, longnull, tmp,
          &anynull, &status);
 
        for (irow=0;irow<*ncol;irow++)
          {
	    (*cts)[j][irow] = tmp[irow];
	    (*ctsErr)[j][irow] = sqrt(tmp[irow]);
	    }
        j++;
      }

    /* close the file */
    status = 0;
       if ( fits_close_file(infptr, &status) )                
        {
         printf ("Problem close fits file!\n");
         exit (1);  
	}  
   
    }

}
