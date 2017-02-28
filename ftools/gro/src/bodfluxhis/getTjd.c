#include <string.h>
#include <stdio.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"

 
 int getTjd( float *tjdsta, float *tjdstp, float *tjdFitsta, float *tjdFitstp,
 float *tjdsta1, float *tjdstp1, float *tjdsta2, float *tjdstp2, float **tjdb,
 float **tjde, int *nvp, char *data_dir)
 {
  fitsfile  *fp;
  int   hdunum,hdutype,i, j=0, anynull=0, vp,vp2;
  int   status=0;
  long  nrows, longnull = 0 ,*vp1= NULL;
  float *tjd1= NULL, *tjd2 = NULL, tjde_tmp = 0, tjdb_tmp = 0;
  float fltnull = 0;
  char  start[10],stop[10], msg[50]="", fname[100];
  
  /* get vp ID number */
  strcpy(msg,"vp");
  Uclgsi(msg, &vp, &status);
      if (status != 0 )
        {
         printf ("Problem getting view period #vp!");
         exit (1);
	}

  /* get vp2 ID number */
  strcpy(msg,"vp2");
  Uclgsi(msg, &vp2, &status);
      if (status != 0 )
        {
         printf ("Problem getting view period #vp2!");
         exit (1);
	}

  /* Check vp ID and TJD within  the file "vp_list.fits" */
     for (i=0;i<100;i++) fname[i]='\0';
     /*     strcat(strcat(fname,"./"),data_dir); */

     strcat(fname,getenv("LHEA_DATA"));
	    strcat(fname,"/vp_list.fits"); 
   
  /* open Fits file */
      if (fits_open_file(&fp, fname, READONLY, &status) )
	{
         printf("error opening viewing period file: $LHEA_DATA/vp_list.fits\n ");
         status = 1; 
        }
    /* move to the 1st HDU to read table */
     hdunum = 2;  
     if ( fits_movabs_hdu(fp, hdunum, &hdutype, &status) ) 
       {
           printf("error moves to 1st HDU\n");
           status = 1;
	}

     /* check if it is a binary table */ 
    if (hdutype != BINARY_TBL)
       {
         printf("Error: Table is not a BINARY TABLE\n");
         status = 1; 
       }
    
    /* get number of rows in the table*/
    status = fits_get_num_rows(fp, &nrows, &status);
    if (status != 0 )
	{
         printf ("Problem getting row number!");
         exit (1);
	 }
 
     /* allocate arrays */
    tjd1 = (float *) malloc((int)nrows * sizeof(float));
    tjd2 = (float *) malloc((int)nrows * sizeof(float));
    vp1  = (long *) malloc((long)nrows * sizeof(long));

    /*  read the columns of the table */  
    fits_read_col(fp, TLONG, 1, 1L, 1L, nrows, &longnull, vp1,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 4, 1L, 1L, nrows, &fltnull, tjd1,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 7, 1L, 1L, nrows, &fltnull, tjd2,
                  &anynull, &status); 
   
    for (i=0;i<nrows;i++)
      {	
    if  ( abs(vp - vp1[i]) == 0) 
    {
      if (tjd1[i] <= *tjdFitsta)
	{ 
         printf("\nStart time %7.1f is out of Fits range %7.1f!\n", 
                tjd1[i],*tjdFitsta);
         printf("\nre-enter VP1 ID!\n");
         exit (1);
        }
      if (tjd2[i] >= *tjdFitstp)
	{
         printf("\nStop time %6.1f is out of Fits range %6.1f!\n", 
                tjd2[i], *tjdFitstp);
         printf("\nre-enter VP1 ID!\n");
         exit (1);
	} 
      *tjdsta1 = tjd1[i];
      *tjdstp1 = tjd2[i];

      if (tjd1[i] >= tjd2[i])
        {
         printf("\nstop time %6.1f < start time %6.1f!\n", 
                tjd2[i],tjd1[i]);
         printf("\nre-enter VP1 ID\n");
         exit (1);
	}
      goto step1;
    }      
      }
          printf("\nNo VP numbered %d! Re-enter VP1 ID!\n", vp); 
          exit (1);

step1: for (i=0;i<nrows;i++)
      {	
    if  (vp2 == vp1[i]) 
    {
      if (tjd1[i] <= *tjdFitsta)
	{
         printf("\nStart time %7.1f is out of Fits range %7.1f!\n", 
         tjd1[i],*tjdFitsta);
         printf("\nre-enter VP2 ID!\n");
         exit (1);
        }
      if (tjd2[i] >= *tjdFitstp)
	{
         printf("\nStop time %6.1f is out of Fits range %6.1f!\n", 
         tjd2[i], *tjdFitstp);
         printf("\nre-enter VP2 ID!\n");
         exit (1);
	} 
      *tjdsta2 = tjd1[i];
      *tjdstp2 = tjd2[i];

      if (tjd1[i] >= tjd2[i])
        {
         printf("\nStop time %6.1f < start time %6.1f!\n", tjd2[i],tjd1[i]);
         printf("\nre-enter VP2 ID!\n");
         exit (1);
	}
      goto step2;
    }      
      }
          printf("\nNo VP numbered %d! Re-enter VP2 ID!\n", vp2); 
          exit (1);

 step2: if (*tjdsta1 > *tjdsta2) 
         {
           *tjdsta = *tjdsta2;
           tjdb_tmp = *tjdstp2;
          }
        else 
	  {
           *tjdsta = *tjdsta1;
           tjdb_tmp = *tjdstp1;
          }

        if (*tjdstp1 < *tjdstp2)
          { 
           *tjdstp = *tjdstp2;
           tjde_tmp = *tjdsta2;
          }
        else 
	  {
           *tjdstp = *tjdstp1;
           tjde_tmp = *tjdsta1;
	  }

 /* find intermediate VPs */
  *tjde = (float *) malloc((int)nrows * sizeof(float));
  *tjdb = (float *) malloc((int)nrows * sizeof(float));
  j = 0;
  for (i=0;i<nrows;i++)
    {
      if ((tjd1[i] >= tjdb_tmp) && (tjd2[i] <= tjde_tmp))
	{
	  (*tjdb)[j] = tjd1[i];
          (*tjde)[j] = tjd2[i];
          j=j+1;
	}
    }
  *nvp = j;
        return status;   
}

