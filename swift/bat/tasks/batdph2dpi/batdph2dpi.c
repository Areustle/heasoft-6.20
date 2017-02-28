#include <stdio.h>
#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"


/*
HISTORY
-------
  Version 1.0 written by Susima Abeyagunawardene, SSAI, November 2002
*/

#define TOOLSUB batdph2dpi 
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int batdph2dpi(void);
int batdph2dpi_getpar(char *infile, char *outfile, int *rows,
                      char *levels);
int batdph2dpi_work(char *infile, char *outfile, int rows,
                    char *levels); 
                    
/*---------------------------------------------------------------------------*/
int batdph2dpi (void)
{
/* This batdph2dpi tool flattens a three dimensional array using
   specified ranges of planes in a BAT_DPH data cube to a
   two dimesional array   */

    char infile[PIL_LINESIZE], outfile[PIL_LINESIZE];
 
    /* PIL_LINE_SIZE is defined in pil.h as 2000 */

    int status = 0;
    int rownumber;       /* Number of data cube to be flattened */
    char levels[200];  /* Level ranges to be used in the flattening */ 
    static char taskname[80] = "batdph2dpi";
    static char version[8] = "1.1";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);


    headas_chat(1,"*************************************************\n");
    headas_chat(1,"   WARNING: This task is not supported for scientific analysis.\n");
    headas_chat(1,"   Please use the 'batbinevt' task to convert a survey DPH to a detector image.\n");
    headas_chat(1,"*************************************************\n");
    
    /*  get input parameters */

    status = batdph2dpi_getpar(infile, outfile, &rownumber,
                               levels); 
    headas_chat(5, "levels is %s \n", levels);
    /* Call batdph2dpi_work to open the input and output files 
       and do the required processing */
    if (!status)
       status = batdph2dpi_work(infile, outfile, rownumber,
                                levels); 
 
    return(status);
}
/*---------------------------------------------------------------------------*/
int batdph2dpi_getpar(
    char *infile,      /* O - Input file name without the BAT_DPH 
                              extension */
    char *outfile,     /* O - Output file name */
    int *rownumber,    /* O - The number of the data cube which is to
                              be flattened */  
    char *levels)  /* O - Level ranges to be used in the flattening */ 

/*  read input parameters for the batdph2dpi task from the .par file */
{
    int status = 0;

    if ((status = PILGetFname("infile", infile)))
      fprintf(stderr, "Error reading the 'infile' parameter.\n");

    else if ((status = PILGetFname("outfile", outfile)))
      fprintf(stderr, "Error reading the 'outfile' parameter.\n");

    else if ((status = PILGetInt("rows", rownumber)))
      fprintf(stderr, "Error reading the 'rows' parameter.\n");

    else if ((status = PILGetString("levels",levels )))
      fprintf(stderr, "Error reading the 'levels' parameter.\n");

    return(status);
}
/*  ------------------------------------------------------------------------  */ 

 int batdph2dpi_work(
     char *infile,        /* I - Input file name  */
     char *outfile,       /* I - Output file name */
     int rownumber,       /* I - Number of row (data cube to be
                                 flattened */
     char *levels)  /* I - Number of energy level ranges to
                                 be used in the  processing */
{
  /* Define the local variables     */
  long nrows = 0l;        /* Number of rows in the binary table */
  int numranges ;          /* Number of ranges in
                             level_ranges */
  long rangemin[50];       /* Minimum values in the ranges */
  long rangemax[50];       /* Maximum values in the ranges */
  long last_rangemin;      /* Minimum value in the last range in the
			      input string "1-3,5-7,8-12 last_rangemin
			      is 8. If the last range is just 12
			      last_rangemin is 12 */
  long last_rangemax;      /* Maximum value in the last range for example
			      if the input string is "1-3,5-7,8-12" 
			      last_rangemax is 12. If last range is 
			      just 12, last_rangemax is 12 */
  int last_numranges;      /* Number of ranges in a range string to be
			      parsed */
  float *new_counts;       /* Array containing the sum of DPH_COUNTS
                              from user specified energy levels 
                              (layers) */
  float counts[100];       /* Value of DPH_COUNTS from user
                              specified energy levels (layers) */ 
  float sum_counts;        /* Sum of the counts from the user
                              specified range of layers */
  int layer_range;         /* Range of layers from which the counts
                              are summed */
  long firstelem;          /* First element in the range of layers
                              from which the DPH_COUNTS summing starts */
  long row_number;         /* Row number of the binary table from
                              which the DPH_COUNTS are taken for
                              processing. This value is specified 
                              by the user */ 
                                 
  char *value ;            /* Value stored in the  keyname
                              TDIM3 in the header of the BAT_DPH
                              extension HDU of the FITS file. */
 char tdim3_value[80];     /* Character array to store TDIM3
                              string */
 char *comment;            /* Comment in header of an HDU
                              corresponding to a key */
 char *new_string;         /* Stores first part of the value
                              corresponding to TDIM3 and the number
                              of energy levels will be extracted
                              from this variable */
 char *new_string2;        /* Stores the second part of the value
                              corresponding to TDIM3 from which the
                              number of elements in the second 
                              dimension could be determined */
 char *new_string3;        /* Stores the third part of the value
                              corresponding to TDIM3 from which the
                              number of elements in the third 
                              dimension could be determined */
 int naxis;                /* Number of dimensions in fits array */
 long dim_size[2];         /* Size of each dimension of the fits
                              array */ 
 int nkeys;                /* The number of existing keys in the
                              header */
 char card[FLEN_CARD];     /* Stores the contents of each line
                              in the header of HDU */
 int colnum;               /* Number of the column in BAT_DPH
                              extension from which DPH_COUNTS
                              are obtained */
 int skip_values;          /* Number of entries in binary table to be
                              skipped in order to access the correct
                              entry in the table */
 long product_rowcol;      /* Product of row and col*/
 char infile1[PIL_LINESIZE]; /* Name of BAT_DPH extension of file */
 char infile2[PIL_LINESIZE]; /* Name of EBOUNDS extension of file */
 int colnum_emin;         /* Column number in the binary table
                              containing E_MIN values */
 int colnum_emax;         /* Column number in the binary table
                             containing E_MAX values */
 char comment_emin[70];   /* Comment corresponding to the E_MIN
                             to be added to existing output file */
 char comment_emax[70];   /* Comment corresponding to the E_MAX
                             to be added to existing output file */
 char keyname[8];         /* keyname to be added to header of
                             output file */
 int key_value;           /* Value of the key */
 float *emin_values;      /* Pointer to minimum energy level values */
 float *emax_values;      /* Pointer to maximum energy level values */
 int range_count;         /* Energy level range count. For example
                             if the user inputs the enegy level ranges
                             1-3,5-5 there are two energy level ranges
                             and range_count varies from 0 to 1 */ 
 char *last_range;        /* Character string storing the last range
                             in the user input energy level ranges */
 char *saved_range;       /* value of last_range saved */ 
 char kw_string[20];      /* Name of keyword to be searched */
 int last_one;            /* Integer value corresponding to last_rangemax */
 fitsfile *infptr1 = 0, *infptr2 = 0, *outfptr = 0;
 int status = 0;
 int i, j, k, m;
 int layers = 0, cols = 0, rows = 0;
 float emin, emax;

 strcpy(infile1, infile);
 strcpy(infile2, infile);

 /* Concatenate BAT_DPH and EBOUNDS extensions to infile1 and
    infile2 respectively */
 strcat(infile1,"[BAT_DPH]");
 strcat(infile2,"[EBOUNDS]");
 
 row_number = 0;
 /* Open the BAT_DPH extension */
 fits_open_file(&infptr1, infile1, READONLY , &status);
 /* If this doesn't work, try a [SCALED] extension */
 if (status) {
   status=0;
    strcpy(infile1, infile);
    strcat(infile1,"[SCALED]");
    fits_open_file(&infptr1, infile1, READONLY , &status);
    if (status) {
       fprintf(stderr, "ERROR: Can't open input file %s\n",infile1);
       exit(-1);
    }     
 }
 
 /* Open the EBOUNDS extension */
 fits_open_file(&infptr2, infile2, READONLY , &status);

 /* Delete existing outfile if clobber=YES */
 headas_clobberfile(outfile);

/* Get the number of existing keys in the header of BAT_DPH
    extension */
 fits_get_hdrspace(infptr1, &nkeys, NULL, &status);

/* Get the number of rows in the binary table of BAT_DPH
    extension    */
 fits_get_num_rows(infptr1, &nrows, &status);

 /* Check if the user input row number is greater than the number of
    rows in the binary table of the BAT_DPH extension. If so
    set rownumber to 1 */
 if(rownumber > nrows){
     /*printf("User input row number %d > number of rows %d in HDU \n",
              rownumber, nrows);
     printf("Using row number 1 \n");
     rownumber = 1;*/
     headas_chat(5,"User input row# %d > # of rows %d in HDU \n",
	                         rownumber, nrows);
     fprintf(stderr, "ERROR: user input row %d # > # of rows %ld in HDU \n",
	                         rownumber, nrows);
     exit(-1);     
  }
  comment = malloc(80*sizeof(char));
 
  /* Get the column number corresponding to the name DPH_COUNTS */
  fits_get_colnum(infptr1, CASEINSEN, "DPH_COUNTS", &colnum, &status);
  /* If this doesn't work look for a "SCALED_COUNTS" column */
  if (status) {
    status=0;
     fits_get_colnum(infptr1, CASEINSEN, "SCALED_COUNTS", &colnum, &status);
     if (status) {
        fprintf(stderr, "ERROR: DPH-type column not found \n");
        exit(-1);
     }     
  }
  
  /* Form the keyword to be searched */
  strcpy(kw_string,"");
  sprintf(kw_string, "TDIM%d", colnum);
  headas_chat(5, "keyword to be searched is %s \n", kw_string);
  /* Get the character string stored in keyword TDIM3. This will have
     the number of energy levels in the data cube, the number of rows
     and columns. For example the string will be of the form "(5-286-173)" */  
  /*fits_read_keyword(infptr1, "TDIM3", tdim3_value, comment,
    &status);*/
  fits_read_keyword(infptr1, kw_string, tdim3_value, comment, &status);
  if (status) {
     fprintf(stderr, "ERROR: keyword %s not found \n",kw_string);
     exit(-1);
  }     
  value = tdim3_value;
  headas_chat(5,"tdim3_value is %s \n", value);

  /* Get the part of the string that stores the number of
      energy levels */
  new_string = strtok(value,"(");
  new_string = strtok('\0', ",");

 /* Get the number of columns in the data cube */
  new_string2 = strtok('\0', ",");

 /* Get the number of rows in the data cube */
  new_string3 = strtok('\0', ")");
 
  /* Convert the character values to integer values */ 
  layers = atoi(new_string);
  cols = atoi(new_string2);
  rows = atoi(new_string3);

  /* Parse the level_ranges string and get the minimum and maximum
     energy values for each range    */
  headas_chat(5,"levels string is %s \n", levels);
  fits_parse_range(levels, layers, layers, &numranges,
                   rangemin, rangemax, &status);

  /* Get the character string corresponding to the last enegy
     range */
  last_range = strtok(levels,",");
  headas_chat(5,"last_range is %s \n", last_range);
  if ((strcmp(last_range,"-") != 0) && (numranges > 1)){
    for(m = 0; m < (numranges -1); m++){
       last_range = strtok('\0', ",");
    }
    /*printf("last_range inside if statement is %s\n",last_range);*/
  }
  /*printf("This is outside if statement last_range is%s\n",
            last_range);*/
  saved_range = last_range;
  headas_chat(5,"saved_range is %s \n", saved_range);
  fits_parse_range(last_range, layers, layers, &last_numranges,
        	        &last_rangemin, &last_rangemax, &status); 
  headas_chat(5,"last_rangemin and last_rangemax values are %d %d \n",
		        last_rangemin, last_rangemax);
  last_one = (int) last_rangemax;
  if(last_one > layers){
       printf("Last value in the input ranges %d > #energy layers %d\n",
                           last_one, layers);
       printf("Using value of the last energy layer %d\n",layers);
  }

  skip_values = 0;
  product_rowcol = (long)rows*(long)cols;
  new_counts = malloc(product_rowcol*sizeof(float));
  for (j = 0; j < rows ; j++) {
    for (k = 0; k < cols; k++) {
       sum_counts = 0.0;
       row_number = (long) rownumber;
       for (i = 0; i < numranges; i++){
          layer_range = rangemax[i] - rangemin[i] + 1;
          /* Read the FITS binary table */
          firstelem = rangemin[i] + skip_values;
          fits_read_col(infptr1,TFLOAT,colnum,row_number,
                        firstelem, layer_range, NULL, counts,
                        NULL, &status);
          /* Get the total counts for the number of energy 
             levels given by layer_range starting with the 
             counts corresponding to the energy level rangemin[i] */
          for (m = 0; m < layer_range; m++){
             sum_counts = sum_counts + counts[m];
          }
          new_counts[j*cols+k] = sum_counts;         
       }  /* End of processing i values */
       /* Skip to the next set of values to be used in
          the flattening process */
       skip_values = skip_values + layers;
     }  /* End of processing k values */  
   }  /* End of processing j values */        

   
   /* Create the image HDU   */
   if (fits_create_file(&outfptr, outfile, &status)){
     headas_chat(0,"Unable to open output file. \n");
     goto cleanup;
   }
   
   headas_chat(5, "Created output file: \n %s\n", outfile);
   naxis = 2;
   dim_size[0] = cols;
   dim_size[1] = rows;
 
   /* Create new primary image HDU             */
    if (fits_create_img(outfptr,FLOAT_IMG,naxis,dim_size,&status))
        goto cleanup;
    headas_chat(5,"Created the image HDU. \n");

  /* Copy the keywords from the input file to output file */
    for(i = 0; i < nkeys; i++){
       fits_read_record(infptr1, i, card, &status);
       /* *** Copy HISTORY keywords and any user keywords, but not
         COMMENTS */ 
       if ((fits_get_keyclass(card) >= TYP_REFSYS_KEY) &&
            (strncmp(card, "COMMENT", 7) != 0))  
	  fits_write_record(outfptr, card, &status);     
    }
    /* Write the WCS keywords. If they already exist in the output
       file the values will be updated */
    fits_update_key(outfptr,TSTRING,"CTYPE1","DETX",
		    "Name of axis 1", &status);

    key_value = 1;
    fits_update_key(outfptr,TINT,"CRPIX1",&key_value,
		    "Image reference pixel",&status);

    key_value = 0;
    fits_update_key(outfptr,TINT,"CRVAL1",&key_value,
		    "Image reference value", &status);

    key_value = 1;
    fits_update_key(outfptr,TINT,"CDELT1",&key_value,
		    "Image pixel size", &status);
    

    fits_update_key(outfptr,TSTRING,"CTYPE2","DETY",
		    "Name of axis 2", &status);

    key_value = 1;
    fits_update_key(outfptr,TINT,"CRPIX2",&key_value,
		    "Image reference pixel",&status);

    key_value = 0;
    fits_update_key(outfptr,TINT,"CRVAL2",&key_value,
		    "Image reference value", &status);

    key_value = 1;
    fits_update_key(outfptr,TINT,"CDELT2",&key_value,
		    "Image pixel size", &status);
    /* Write history keywords */
    /* Replaced headas_parstamp with HDpar_stamp  14-Nov-2003  */
    status=HDpar_stamp(outfptr,0,&status); 
    /* status = headas_parstamp(outfptr, 0); */
    if (status) {
      fprintf(stderr,"ERROR:error writing history keywords \n");
    }        
    /* Write the image information stored in new_counts to the
       output file */
    /*if (fits_write_img(outfptr,TFLOAT,row_number,product_rowcol,*/
    if (fits_write_img(outfptr,TFLOAT,1,product_rowcol,
                       new_counts,&status))  
       goto cleanup; 

/*    ***        Processing EBOUNDS extension      ***     */
/* Get the number of rows in the EBOUNDS extension binary table */
    fits_get_num_rows(infptr2, &nrows, &status);
    headas_chat(5, "Number of rows in EBOUNDS table is %d \n", nrows);
 
/* Get the column number containing the E_MIN values */
    fits_get_colnum(infptr2, CASESEN, "E_MIN", &colnum_emin, &status);
    headas_chat(5, "E_MIN colnum is %d \n", colnum_emin);

/* Get the column number containing the E_MAX values */
    fits_get_colnum(infptr2, CASESEN, "E_MAX", &colnum_emax, &status);
    headas_chat(5, "E_MAX colnum is %d \n", colnum_emax);
 
/* Allocate memory to store the energy values from the EBOUNDS
   binary table */
    emin_values = malloc(nrows*sizeof(float));
    emax_values = malloc(nrows*sizeof(float));
 
/* Extract the emin_values and emax_values from the EBOUNDS extension */
/* HAK 23-May-2003  Replace the code below with the more efficient single
   calls to fits_read_col (per Bill Pence's suggestions) */
    /* for(i = 1; i <= nrows; i++) { */
      /* Read one float value starting from 1st element of column number
          colnum_emin in row number i of the EBOUNDS binary table
          into the variable energy_value */
      /* fits_read_col(infptr2,TFLOAT,colnum_emin,i,1,1,NULL,
                     &energy_value,NULL,&status);
       emin_values[i-1] = energy_value;
       fits_read_col(infptr2,TFLOAT,colnum_emax,i,1,1,NULL,
                     &energy_value2,NULL,&status);
		     emax_values[i-1] = energy_value2; 
		     } */
    fits_read_col(infptr2,TFLOAT,colnum_emin,1,1,nrows,NULL,
                    emin_values,NULL,&status);    
    fits_read_col(infptr2,TFLOAT,colnum_emax,1,1,nrows,NULL,
                   emax_values,NULL,&status);
    headas_chat(5, "E_MIN         E_MAX \n");
    /* Check whether correct E_MIN and E_MAX values have been
       extracted from the EBOUNDS extension */
    for(i = 0; i < nrows; i++){
      headas_chat(5,"%9.7E   %9.7E \n",emin_values[i],emax_values[i]);
    }

    /* Get the minimum and maximum energy values for the
     user specified layers */
    headas_chat(5,"Level   E_MIN          E_MAX \n");
    headas_chat(5,"         (kev)          (kev) \n");
    range_count = 0;
    for(j = 0; j < numranges; j++){
       for(k = rangemin[j]; k <= rangemax[j]; k++){
          /*printf( "%d   %9.7E    %9.7E \n",
                        k, emin_values[k-1], emax_values[k-1]);*/
          if(j == range_count && k== rangemin[range_count]){
             sprintf(keyname,"E_MIN%d", (range_count+1));
             keyname[8]='\0';
             sprintf(comment_emin, "Minimum energy(keV) of range #%d",
                                                   (range_count+1));
             comment_emin[70] = '\0';
            /* Write the keyname and the corresponding values to the
               output file */
             emin = emin_values[k-1];
             if(fits_update_key(outfptr,TFLOAT,keyname,&emin,
                             comment_emin,&status)){
                headas_chat(5, "Unable to update keyname %s \n",keyname);
		fprintf(stderr,"ERROR:unable to update %s \n",keyname);
		exit(-1);
             }
             else{
                  headas_chat(5,"Updated keyname %s \n", keyname);
             }
          }
         /* Get the keyname for the maximum energy value for the
            current level */
         if(j == range_count && k == rangemax[range_count]){
             sprintf(keyname,"E_MAX%d", (range_count+1));
             keyname[8]='\0';
             sprintf(comment_emax, "Maximum energy(keV) of range #%d",
                                              (range_count+1));
             comment_emax[70] = '\0';
             emax = emax_values[k-1];
             if(fits_update_key(outfptr,TFLOAT,keyname,&emax,
                                comment_emax,&status)){
                headas_chat(5,"Unable to update keyname %s \n",keyname);
		fprintf(stderr,"ERROR: unable to update %s \n",keyname);
		exit(-1);
             }
             else{
                 headas_chat(5,"Updated keyname %s \n", keyname);  
             }
         }
     } /* End of processing one range */
     range_count++;   /* Process next range */
   }

cleanup:
    if (outfptr) fits_close_file(outfptr, &status);
    if (infptr1)  fits_close_file(infptr1, &status);
    if (infptr2)  fits_close_file(infptr2, &status);
    headas_chat(5,"Number of keys in the input file is %d \n",nkeys);

  free(new_counts);   
  free(comment);
  return(status);
}
