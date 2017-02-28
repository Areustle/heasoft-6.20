#include "groview.h"
void getData( float **tjd, float **raZ, float **decZ,
            float **raX, float **decX, long *nrows, char *data_dir, long **vp,
            char *egretFlg[], char *primTarget[], char *secTarget[], 
            char *all_VPs)
 {
  fitsfile  *fp;
  int   hdunum,hdutype,i, j=0, vp1, vp2;
  int   status=0, BufLen_2=80, anynull=0;
  long  rows, longnull = 0 ,*vptmp= NULL;
  float *tjd1= NULL, *tjd2 = NULL, *raZtmp = NULL, *decZtmp  = NULL;
  float fltnull = 0, *raXtmp = NULL, *decXtmp  = NULL;
  char  start[10],stop[10], msg[50]="", fname[100];
  char  strnull[400], *egretFlgtmp[400];

  /* all_vps? */
  status = 0;
  strcpy(msg,"all_VPs");
  strcpy(all_VPs, " ");
  Uclgst(msg, all_VPs, &status);
      if (status != 0 )
        {
         printf ("Problem getting all_VPs!\n");
         exit (1);
        }

      if (strncmp(all_VPs,"y",1)==0 ||strncmp(all_VPs,"Y",1)==0 )
	{
	  vp1 = 2;
          vp2 = 9195;
	}
      else
	{
	  /* get vp ID number */
	  strcpy(msg,"vp1");
	  Uclgsi(msg, &vp1, &status);
	  if (status != 0 )
	    {
	      printf ("Problem getting view period #vp1!");
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
	}


  /* Check if #vp1 and #vp2  is within  the file "vp_list.fits" */
   /* get name of director where vp_list.fits is*/
        strcpy(msg,"data_dir");
        Uclgst(msg, data_dir, &status);
        if (status != 0)
          {
           printf ("Problem getting name of data directory!\n");
           exit (1);
          }

     for (i=0;i<100;i++) fname[i]='\0';
     strcat(strcat(fname,"./"),data_dir);
     strcat(strcat(fname,"/"),"vp_list.fits");
     printf("fname=%s\n",fname);
  /* open Fits file */
      if (fits_open_file(&fp,fname, READONLY, &status) )
        {
         printf("error opening vp_list.fits\n ");
          exit (1);
        }
    /* move to the 1st HDU to read table */
     hdunum = 2;
     if ( fits_movabs_hdu(fp, hdunum, &hdutype, &status) )
       {
           printf("error moves to 1st HDU\n");
           exit (1);
        }


     /* check if it is a binary table */
    if (hdutype != BINARY_TBL)
       {
         printf("Error: Table is not a BINARY TABLE\n");
         exit (1);
       }

   /* get number of rows in the table*/
    status = fits_get_num_rows(fp, &rows, &status);
    if (status != 0 )
        {
         printf ("Problem getting number of rows!\n");
         exit (1);
         }
  
    /* allocate arrays */
    tjd1 = (float *) malloc((int)rows * sizeof(float));
    tjd2 = (float *) malloc((int)rows * sizeof(float));
    vptmp   = (long *) malloc((long)rows * sizeof(long));
    *vp   = (long *) malloc((long)rows * sizeof(long));
    *raZ  = (float *) malloc((int)rows * sizeof(float));
    *decZ = (float *) malloc((int)rows * sizeof(float));
    *raX  = (float *) malloc((int)rows * sizeof(float));
    *decX = (float *) malloc((int)rows * sizeof(float));
    *tjd = (float *) malloc((int)rows * sizeof(float));
    raZtmp  = (float *) malloc((int)rows * sizeof(float));
    decZtmp = (float *) malloc((int)rows * sizeof(float));
    raXtmp  = (float *) malloc((int)rows * sizeof(float));
    decXtmp = (float *) malloc((int)rows * sizeof(float));
   
    for (i = 0; i < 400; i++)    /* allocate space for string column value */
      {
       egretFlgtmp[i] = (char *) malloc(4);  
       secTarget[i]   = (char *) malloc(23); 
       primTarget[i]   = (char *) malloc(23);  
      }
    strcpy(strnull, " ");

   
     /*  read the columns of the table */
/*

 original version, ordering got screwed up some how?

    fits_read_col(fp, TLONG, 1, 1L, 1L, rows, &longnull, vptmp,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 4, 1L, 1L, rows, &fltnull, tjd1,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 7, 1L, 1L, rows, &fltnull, tjd2,
                  &anynull, &status);
    fits_read_col(fp, TSTRING, 8, 1, 1, rows, strnull, egretFlgtmp,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 9, 1L, 1L, rows, &fltnull, raZtmp,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 10, 1L, 1L, rows, &fltnull, decZtmp,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 11, 1L, 1L, rows, &fltnull, raXtmp,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 12, 1L, 1L, rows, &fltnull, decXtmp,
                  &anynull, &status);
    fits_read_col(fp, TSTRING, 13, 1, 1, rows, strnull, primTarget,
                  &anynull, &status);
    fits_read_col(fp, TSTRING, 16, 1, 1, rows, strnull, secTarget,
                  &anynull, &status);
*/
    fits_read_col(fp, TLONG, 1, 1L, 1L, rows, &longnull, vptmp,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 4, 1L, 1L, rows, &fltnull, tjd1,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 7, 1L, 1L, rows, &fltnull, tjd2,
                  &anynull, &status);
    fits_read_col(fp, TSTRING, 12, 1, 1, rows, strnull, egretFlgtmp,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 8, 1L, 1L, rows, &fltnull, raZtmp,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 9, 1L, 1L, rows, &fltnull, decZtmp,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 10, 1L, 1L, rows, &fltnull, raXtmp,
                  &anynull, &status);
    fits_read_col(fp, TFLOAT, 11, 1L, 1L, rows, &fltnull, decXtmp,
                  &anynull, &status);
    fits_read_col(fp, TSTRING, 13, 1, 1, rows, strnull, primTarget,
                  &anynull, &status);
    fits_read_col(fp, TSTRING, 16, 1, 1, rows, strnull, secTarget,
                  &anynull, &status);   
   

    /* close the file */
    status = 0;
       if ( fits_close_file(fp, &status) )
        {
         printf ("Problem close fits file!\n");
         exit (1);
        }

       if ( (vp1-vptmp[0]) < 0 || (vp1-vptmp[rows-1]) >0) 
	 {
          printf("vp1 must be within %d - %d",vptmp[0],vptmp[rows-1]);
          exit (1);
        }

       if ( (vp2-vptmp[0]) < 0 || (vp2-vptmp[rows-1]) >0) 
	 {
          printf("vp2 must be within %d - %d",vptmp[0],vptmp[rows-1]);
          exit (1);
        }

       if ( (vp1-vp2) > 0 )
	 {
          printf("vp1 must be smaller than %d",vp2); 
          exit (1);
	 } 

       j = 0;
       for (i=0; i<rows; i++)
	 {
          
          if ( ((vptmp[i] - vp1) >= -0.0000001) && 
               ((vp2 - vptmp[i]) >= -0.0000001))
	    {
	      (*tjd)[j] = (tjd1[i]+tjd2[i])/2.;
	      (*raZ)[j]    = raZtmp[i];
	      (*decZ)[j]   = decZtmp[i];
	      egretFlg[j] = (char*) malloc(4);
	      strcpy(egretFlg[j],egretFlgtmp[i]);
	      strcpy(primTarget[j],primTarget[i]);
	      strcpy(secTarget[j],secTarget[i]);
	      (*raX)[j]    = raXtmp[i];
	      (*decX)[j]   = decXtmp[i];
	      (*vp)[j]   = vptmp[i];
	      j = j + 1 ;
	    }
	 }
       *nrows = j;

  
       /* free memory */
       free(raZtmp);
       free(decZtmp);
       free(raXtmp);
       free(decXtmp);  
       free(vptmp);
       free(tjd1);
       free(tjd2);
 }
