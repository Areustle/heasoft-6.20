#include <stdio.h>
#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"


/*
HISTORY
-------
  Version 1.0 written by Susima Abeyagunawardene, SSAI, February 2003

  CBM 29-Nov-2003 Change to new HDpar_stamp() function.

  HAK 17-Dec-2003
     Added code to pass through TNULLs from the input DPH
     Also update the TSTART, TSTOP and related keywords to correctly
        reflect the output DPH
*/

#define TOOLSUB batsumdph 
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int batsumdph(void);
int batsumdph_getpar(char *infile, char *outfile, char *rows);
int batsumdph_work(char *infile, char *outfile, char *rows);
int update_keys(fitsfile *,double *,double *,int);

/*---------------------------------------------------------------------------*/
int batsumdph(void)
{
/* This bat tool adds the values in the specified columns of the 
   specified rows of the BAT_DPH extension of the file and generates
   a new file with a BAT_DPH extension. Also a new GTI extension is also
   generated using time values of the used rows. */  

  char infile[PIL_LINESIZE], outfile[PIL_LINESIZE];
  
  char rows[200]; /* Array of rows for e.g. 1-3, 6-9 etc. */
  int status = 0;       /* Status returned from calls to subroutines */
  static char taskname[80] = "batsumdph";
  static char version[8] = "1.1";

  /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    headas_chat(1,"*************************************************\n");
    headas_chat(1,"   WARNING: This task is not supported for scientific analysis.\n");
    headas_chat(1,"   Please use the 'batbinevt' task to sum survey DPHs\n");
    headas_chat(1,"*************************************************\n");

  /*  get input parameters */

    status = batsumdph_getpar(infile, outfile, rows);

  /* Call batsumdph_work to open open the input and output
     files and do the necessary processing.The rows are specified
     in increasing order with no overlaps */

     if(!status) {   
      status = batsumdph_work(infile, outfile, rows);
    }

     /* HAK 22-May-2003  Added return */
     return(status);

}  /* End of batsumdph */

/*------------------------------------------------------------------------*/

int batsumdph_getpar(
	     char *infile,        /* O - Input file name with out BAT_DPH
				         extension */
             char *outfile,       /* O - Output file name */
             char *rows)    /* 0 - Rows required for summing */
 /* Read the input parameters for batsumdph task from the .par file */
 
{
  int status = 0;
  if ((status = PILGetFname("infile", infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");

  
 else if ((status = PILGetFname("outfile", outfile)))
      fprintf(stderr, "Error reading the 'outfile' parameter.\n");


 else if ((status = PILGetString("rows",rows )))
      fprintf(stderr, "Error reading the 'rows' parameter.\n");
   

  return(status);
}

/*------------------------------------------------------------------------*/


int batsumdph_work(
	       char *infile,     /* I - Input file name */
               char *outfile,    /* I - Output file name */
               char *rows) /* I - Ranges of rows to be used in
				    the summing */

{
  /* Define the local variables */
  char card[FLEN_CARD];          /* Contents of each line in header of
				    HDU */
  int colnum;                    /* Number of the column containing
				    DPH_COUNTS */
  int colnum_data_flags;         /* Number of DATA_FLAGS column in
				    BAT_DPH extension */
  int colnum_dph_counts;         /* Number of DPH_COUNTS column in
				    BAT_DPH extension */
  int colnum_exposure;           /* Number of EXPOSURE column in
				    BAT_DPH extension */ 
  int colnum_gain_index;         /* Number of GAIN_INDEX column in
				    BAT_DPH extension */
  int colnum_offset_index;       /* Number of OFFSET_INDEX column in
				    BAT_DPH extension */
  int colnum_time;               /* Number of TIME column in
				    BAT_DPH extension */
  int counts_per_row;            /* Number of DPH_COUNTS per row of
				    the binary BAT_DPH table. It is
				    equal to levels*286*173 */
  char comment[80];              /* comment in a keyword */ 
  int data_flags[80];            /*  Array containing data_flags in 
				    all the rows of the BAT_DPH
				    binary table */
  long *dph_counts;
  double exposure;               /* Exposure value from current row */
  long first_gain_index;         /* Gain index of the first selected row */
  long first_offset_index;       /* Offset index of the first selected
				    row */
  double first_row_time;         /* Time corresponding to first row of
                                    BAT_DPH binary table */ 
  double *gti_start;             /* GTI start value for each of the rows */
  double *gti_end;               /* GTI stop value for each of the rows */ 
  char infile1[PIL_LINESIZE];    /* BAT_DPH extension file name */
  char infile2[PIL_LINESIZE];    /* GTI extension file name */
  char infile3[PIL_LINESIZE];    /* EBOUNDS extension file name */
  long maxranges;                /* Maximum number of ranges */ 
  long maxrows;                  /* Maximum number of rows input to
                                    fits_parse_range */
  int new_data_flags=0;          /* New data flags value obtained
				    by 'or-ing' the data flag values
				    in the selected rows */
  int nkeys;                     /* Number of existing keys in header */
  int num_gti_rows;              /* Number of rows in GTI extension
				    binary table */
  int num_of_ranges;             /* Total number of ranges input */
  long nrows;                    /* Total number of rows in BAT_DPH
                                    extension binary table */
  int num_levels;                /* Number of energy levels in the data
				    cube represented by this binary
				    file */
  long rangemax[160];            /* Array of maximum row value in
                                    each range */ 
  long rangemin[160];            /* Array of minimum row value in
                                    each range */
  int row_data_flags;            /* data_flags value in current row */
  long row_gain_index;           /* Gain index from current rows */
  long row_offset_index;         /* Offset index from current row */
  int sum_data_flags;            /* Sum of the data_flags from the
				    selected rows */ 
  long  *sum_dph_counts;         /* Sum of each of possible 80*286*173
				    dph_counts from selected rows 
				    a maximum of 80 energy levels 
				    is assumed */
  double sum_exposure;           /* Sum of exposures of selected rows */
  char tdim_str[80];             /* Key word to be searched */
  char tdim_value[FLEN_CARD];    

  int naxis = 0;                 /* For reading dimensions of DPH_COUNTS column */
  long naxes[3] = {0,0,0};       /* For reading dimensions of DPH_COUNTS column */

  int ndone = 0;                 /* Number of rows analyzed */
  
/*--------------------------------------------------------------------*/
  fitsfile *infptr1 = 0, *infptr2 = 0, *outfptr = 0;
  fitsfile *infptr3 = 0;
  int status = 0;
  int i, j, m, mm;
  int warning=0;
  long tnull3=-32768;
  int tfields = 6;  /* Number of fields in the new binary table */
  char *ttype[6];   /* Names of the columns in the new binary table */
  char *tform[6];   /* Format of each column */
  char *tunit[6];   /*Physical unit of table column */
  char tform_str[20]; /* Character string used to get correct value 
			 of ttype[2] */
  int total_counts;   /* Number of count (DPH_COUNTS) in each row of the
			 binary table */
  char ext_name[15];  /* Extension name */


  headas_chat(4,"Entered batsumdph_work \n");
  strcpy(infile1,infile);
  strcpy(infile2,infile);
  strcpy(infile3,infile);
  /* Concatenate the BAT_DPH, GTI and EBOUNDS extensions to infile1,
     infile2 and infile3 */ 
      
  strcat(infile1,"[BAT_DPH]");
  strcat(infile2,"[GTI]");
  strcat(infile3,"[EBOUNDS]");
  strcpy(ext_name,"BAT_DPH");
  ext_name[7] = '\0';
  headas_chat(4, "ext_name is %s \n", ext_name);
  headas_chat(4,"infile1 and outfile1 are %s %s\n",infile1, infile2);

  ttype[0]= "TIME";
  ttype[1]= "EXPOSURE";
  ttype[2]= "DPH_COUNTS";
  ttype[3]= "DATA_FLAGS";
  ttype[4]= "GAIN_INDEX";
  ttype[5]= "OFFSET_INDEX";
  tform[0]= "1D";
  tform[1]= "1D";
  /*tform[2] = "1I"*/;         /* This will be modified later once
				  the number of energy levels in the
				  file is known */
  tform[2]= "3958240I";       
  tform[3]= "1I";
  tform[4]= "1J";
  tform[5]= "1J";
  tunit[0]= "s";  
  tunit[1]= "s";  
  tunit[2]= "COUNT";  
  tunit[3]= " ";
  tunit[4]= " ";
  tunit[5]= " ";

  /* Open the BAT_DPH extension */
  fits_open_file(&infptr1, infile1, READONLY, &status);
  if (status) {
     fprintf(stderr,"ERROR:Unable to open %s \n",infile1);
     return status;
  }
  headas_chat(4,"Opened file %s \n", infile1);
  
  /* Open the GTI extension */
  fits_open_file(&infptr2, infile2, READONLY, &status);
  if (status) {
     fprintf(stderr,"ERROR:Unable to open %s \n",infile2);
     return status;
  }
  headas_chat(4,"Opened file %s \n", infile2);
  
  /* Open the EBOUNDS extension */
  fits_open_file(&infptr3, infile3, READONLY, &status);
  if (status) {
     fprintf(stderr,"ERROR:Unable to open %s \n",infile3);
     return status;
  }
  headas_chat(4,"Opened file %s \n", infile3);
  
  /* Delete the existing outfile if clobber = YES */
  headas_clobberfile(outfile);

  /* Get the column number correspoinding to DPH_COUNTS */
  fits_get_colnum(infptr1, CASEINSEN, "DPH_COUNTS", &colnum, &status);

  if (fits_read_tdim(infptr1, colnum, 3, &naxis, naxes, &status)) {
    fprintf(stderr, "ERROR: could not read dimensions of column %d\n",
	    colnum);
    return status;
  }
  sprintf(tdim_str, "TDIM%d", colnum);
  sprintf(tdim_value, "(%ld,%ld,%ld)", naxes[0], naxes[1], naxes[2]);

  num_levels = naxes[0];

  /* Get the total number of DPH_COUNTS in a row */
  counts_per_row = num_levels*286*173; 
  headas_chat(4,"num_levels is %d \n",num_levels);

  /* Get the number of rows in BAT_DPH binary table */
  if (fits_get_num_rows(infptr1, &nrows, &status)) {
    fprintf(stderr, "ERROR: could not get number of table rows\n");
    return status;
  }

  headas_chat(4,"Number of rows in BAT_DPH bin table is %d\n",
		  nrows);

  /* Parse the row ranges string */
  maxrows = (long) nrows; 
  maxranges = (long) nrows;
  fits_parse_range(rows,maxrows,maxranges,&num_of_ranges,
                   rangemin,rangemax,&status);
  if(status) {
    fprintf(stderr, "ERROR: Unable to parse input rows %s \n",
	    rows);
    return status;
  }
  headas_chat(4,"Number of row ranges is %d \n",num_of_ranges);

  /* Create output file */
  if (fits_create_file(&outfptr, outfile, &status)) {
    headas_chat(5, "Unable to create outputfile \n", outfile);
    return status;
  }
  headas_chat(4,"Created output file \n", outfile);
  sum_exposure = 0.0;  

  ndone = 0;

  sum_dph_counts = malloc(3958240*sizeof(long)); /*3958240=80*286*173*/
  if (!sum_dph_counts) {
    fprintf(stderr,"ERROR:allocating memory to sum_dph_counts failed \n");
    return status;
  }    
  /* Get column numbers corresponding to ttype[0] through ttype[5]
     in BAT_DPH binary table */
  fits_get_colnum(infptr1, CASESEN, ttype[0], &colnum_time,
		                           &status);
  fits_get_colnum(infptr1, CASESEN, ttype[1], &colnum_exposure,
		                           &status);  
  fits_get_colnum(infptr1, CASESEN, ttype[2], &colnum_dph_counts,
		                           &status);  
  fits_get_colnum(infptr1, CASESEN, ttype[3], &colnum_data_flags,
		                           &status);  
  fits_get_colnum(infptr1, CASESEN, ttype[4], &colnum_gain_index,
		                           &status);  
  fits_get_colnum(infptr1, CASESEN, ttype[5], &colnum_offset_index,
		                           &status);  
  if (status) {
    fprintf(stderr, "ERROR:cannot get column numbers in binary table \n");
    return status;
  }
  headas_chat(4,"Column numbers are %d %d %d %d %d %d \n",
		colnum_time, colnum_exposure, colnum_dph_counts,
	        colnum_data_flags, colnum_gain_index, colnum_offset_index);	
  /* Initialize the sum_dph_counts */
  for (m = 0; m < counts_per_row; m++) {
    sum_dph_counts[m] = 0;
  }       
  dph_counts = malloc(80*286*173*sizeof(long));
  if (!dph_counts) {
    fprintf(stderr,"ERROR:allocating memory to dph_counts failed \n");
    return MEMORY_ALLOCATION;
  }    

  gti_start=malloc(nrows*sizeof(double));
  if(!gti_start) {
     fprintf(stderr,"ERROR:allocating memory to gti_start failed \n");	 
     return MEMORY_ALLOCATION;
  }   
  
  gti_end=malloc(nrows*sizeof(double));
  if(!gti_end) {
     fprintf(stderr,"ERROR:allocating memory to gti_end failed \n");	 
     return MEMORY_ALLOCATION;
  }   
  
  for (i = 0; i < num_of_ranges; i++) {
       headas_chat(4, "i = %d row min = %d, row max = %d \n",
	                 i, rangemin[i], rangemax[i]);
       for (j = rangemin[i]; j <= rangemax[i]; j++) {

	/* Get the counts_per_row of dph counts from each
	   selected row */
         fits_read_col(infptr1, TLONG, colnum_dph_counts , j, 1, 
		   counts_per_row, NULL, dph_counts, NULL, &status);
	 if (status) {
	   fprintf(stderr,"ERROR:Cannot get counts from row %d \n",j);
	   goto cleanup;
	 }  

         /* HAK 17-Dec-2003  Added code to write TNULL to the output DPH
            for each DPH cell in which any of the inputs are TNULL.
            Print a warning if TNULLs are written to the output */
         for (m = 0; m < counts_per_row; m++) {
	   if ((dph_counts[m] == tnull3) || (sum_dph_counts[m] == tnull3)) {
	     sum_dph_counts[m] = tnull3;
	     warning++;
	   } else { 
	     sum_dph_counts[m] += dph_counts[m];
	   }
         }	      
        /* read the EXPOSURE column which is column #2 
	   from the BAT_DPH extension */
	 fits_read_col(infptr1, TDOUBLE, colnum_exposure, j, 1, 1, NULL, 
		            &exposure, NULL, &status);
	 if (status) {
	   fprintf(stderr,"Error:Cannot get exposure from row %d \n",j);
           goto cleanup;
         }	   
         headas_chat(4,"Row %d exposure is %f \n", j, exposure);
         sum_exposure = sum_exposure + exposure;	
         headas_chat(4,"Row %d sum_exposure is %f \n",
		       	j, sum_exposure);
	 /* Get the GTI start time for row j from infile2 */
	 fits_read_col(infptr2, TDOUBLE, 1, j, 1, 1, NULL,
			 &gti_start[ndone], NULL , &status);
	 if (status) {
           fprintf(stderr,"Error:Cannot get GTI start from row %d \n",j);
           goto cleanup;
         }
	 gti_end[ndone] = gti_start[ndone] + exposure;
	 headas_chat(4,"For row %d, gti_start and end are %f %f \n",
		     ndone, gti_start[ndone], gti_end[ndone]);
	 
         if ((i == 0) && (ndone == 0)) {
         /* read the TIME column from BAT_DPH extension */
	    fits_read_col(infptr1, TDOUBLE, colnum_time, rangemin[i],   
		       1, 1, NULL, &first_row_time, NULL, &status);
            if (status) {
	      fprintf(stderr,"Unable to read col 1 from 1st selected row\n");
	      goto cleanup;
            }
	    headas_chat(4, "first_row_time is %f \n", 
			  first_row_time);
	  
	 
         } /* End of processing the first selected row */
	 /* Get columns of row j for data_flags,
	    gain_index and offset_index values */
	 fits_read_col(infptr1, TINT, colnum_data_flags, j,
		1, 1, NULL, &row_data_flags, NULL, &status);
	 data_flags[ndone] = row_data_flags;
	 fits_read_col(infptr1, TLONG, colnum_gain_index, j, 
		1, 1, NULL, &row_gain_index, NULL, &status);
	 fits_read_col(infptr1, TLONG, colnum_offset_index, j,
		1, 1, NULL, &row_offset_index, NULL, &status);
        	
	 if (status) {
	  fprintf(stderr,"Warning:unable to get data_flags in row %d\n",
		 ndone+1);
	  goto cleanup;
         }
       	 if ((i == 0) && (ndone == 0)) {
	  first_gain_index = row_gain_index;
          first_offset_index = row_offset_index;
	  headas_chat(4,"data_flags in first selected row is %d\n",
			       data_flags[ndone]);
	  headas_chat(4,"first_gain_index is %d \n",first_gain_index);
	  headas_chat(4,"first_gain_index is %d \n",first_gain_index);
         }
	 else{
	  if (row_gain_index != first_gain_index){
	    fprintf(stderr,"Warning:row_gain_index %ld in row %d ",
			      row_gain_index, j);
	    fprintf(stderr,"different to first row value %ld \n",
	               first_gain_index);
          }
         }
	
	 ndone ++;
       } /* End of processing each row range */	
  } /* End of processing all the row ranges */      
  headas_chat(4,"Warning level = %d \n",warning);
  headas_chat(4,"Total number of rows = %d \n",ndone);
  /* Print out the first 10 sums of the dph_counts from
     the selected rows */
  for (m = 0; m < 10; m++) {
       headas_chat(5, "Sum of count #%d is %d \n",
		   m, sum_dph_counts[m]);
  }
  /* Sum the data_flag values of the selected rows. If the sum > 0
     data flag for the new row is 1. If sum = 0 data flag = 0 - this is
     the same as the value obtained by 'or ing' all zero values */
  /*   *********** Enter code here ***************  */  
  sum_data_flags = 0;
  for (i = 0; i < ndone; i++) {
	sum_data_flags = sum_data_flags + data_flags[i];
  }  
  headas_chat(4,"sum_data_flags is %d\n",sum_data_flags);
  if (sum_data_flags > 0) new_data_flags = 1;
  if (sum_data_flags == 0) new_data_flags = 0;
  headas_chat(4, "new_data_flags is %d \n", new_data_flags);  
   /*Create binary table*/  
  strcpy(tform_str, "");
  total_counts = num_levels*286*173;
  sprintf(tform_str, "%dI", total_counts);
  tform[2] = tform_str; 
  headas_chat(4, "tform[2] is %s \n", tform[2]);  
  fits_create_tbl(outfptr, 2, 0, tfields, ttype,
		  tform, tunit, "BAT_DPH", &status);
  if (status){
    fprintf(stderr,"ERROR:unable to create binary table \n");
    goto cleanup;
  }
  
  headas_chat(4,"Created binary table \n");  
  /*  ********************  NEW **************** */        
  /* Copy keywords from header of bat extension to new output file */
  fits_get_hdrspace(infptr1, &nkeys, NULL, &status);
  if (status) {
    fprintf(stderr,"WARNING:Unable to get number of keys from %s\n",
	                  infile1);
  }
  else {
	headas_chat(4, "*** nkeys is %d ***\n", nkeys);	
	for (mm = 0; mm < nkeys; mm++) {
	    fits_read_record(infptr1, mm, card, &status);
            /* Copy HISTORY keywords and any user keywords, but not
	       COMMENTS */
            if ((fits_get_keyclass(card) >= TYP_REFSYS_KEY) &&
		  (strncmp(card, "COMMENT", 7) != 0)){
	       fits_write_record(outfptr, card, &status);
	       headas_chat(5,"keyclass value is %s \n", card);
	    }   
       }
       headas_chat(4,"tdim_value is %s \n", tdim_value);   
       /* HAK 22-May-2003  Added TNULL keyword to DPH extension */
       fits_update_key(outfptr, TSTRING, tdim_str, tdim_value,
		          "Array dimensions", &status);
       if (status) {
	   fprintf(stderr,"ERROR: Could not add TDIMn to BAT_DPH header\n");
           goto cleanup;
       }
		    
  }	
  /* ****************** END OF NEW ****************** */
  /* Enter values in the columns of the binary table */
  fits_write_col(outfptr,TDOUBLE,1,1,1,1,&first_row_time,
		      &status);
  if (status) {
     fprintf(stderr, "ERROR: writing time to first row \n");
     goto cleanup;
  }
  
  /* Write EXPOSURE column values */
  fits_write_col(outfptr,TDOUBLE,2,1,1,1,&sum_exposure,
		      &status);
  if (status) {
     fprintf(stderr, "ERROR: writing exposure to first row \n");
     goto cleanup;
  }
  
  /* Write DPH_COUNTS column */
  fits_write_col(outfptr,TLONG,3,1,1,total_counts,
		      sum_dph_counts,&status);
  if (status) {
     fprintf(stderr, "ERROR: writing dph_counts to first row \n");
     goto cleanup;
  }
  fits_update_key(outfptr,TLONG,"TNULL3",&tnull3,
		  "data null value", &status);
  
  /* Write DATA_FLAGS column */
  fits_write_col(outfptr,TINT,4,1,1,1,
		      &sum_data_flags,&status);
  if (status) {
     fprintf(stderr, "ERROR: writing data_flags to first row \n");
     goto cleanup;
  }
  /* Write GAIN_INDEX column */
  fits_write_col(outfptr,TLONG,5,1,1,1,&first_gain_index,
		                   &status);
  if (status) {
     fprintf(stderr, "ERROR: writing gain index to first row \n");
     goto cleanup;
  }
  /* Write OFFSET_INDEX column */
  fits_write_col(outfptr,TLONG,6,1,1,1,&first_offset_index,
		                   &status);
  if (status) {
     fprintf(stderr, "ERROR: writing offset index to first row \n");
     goto cleanup;
  }

  /* HAK 17-Dec-2003  
     Added code to overwrite the TSTART, TSTOP, TELAPSE, ONTIME,
     LIVETIME, EXPOSURE keywords to reflect what is truly in the output 
     file -- Note that the current code doesn't read or use the DEADC
     keyword; this needs to be fixed in the next revision. */
 
     status=update_keys(outfptr,gti_start,gti_end,ndone);
     if (status) {
        fprintf(stderr,"ERROR:unable to update keywords\n");
     goto cleanup;
   }    
     
    /* HAK 22-May-2003 Added this call */
    /* Write optional history keywords */
    /* CBM 29 Nov 2003 Change to HDpar_stamp */
    status = HDpar_stamp(outfptr, 0, &status);

    if (status) goto cleanup;

  /* Copy the EBOUNDS extension from the input file to
     the output file */
  fits_copy_hdu(infptr3, outfptr, 0, &status);
  if (status) {
    fprintf(stderr,"ERROR:unable to copy EBOUNDS hdu \n");
    goto cleanup;
  }    

  headas_chat(3, "Updating keywords...\n");
  status=update_keys(outfptr,gti_start,gti_end,ndone);
  if (status) {
    fprintf(stderr,"ERROR:unable to update keywords\n");
    goto cleanup;
  }    
  
  headas_chat(3, "Copying GTI extension...\n");
  /* Copy GTI HDU to output file */
  fits_copy_hdu(infptr2, outfptr, 0, &status);
  if (status) {
    fprintf(stderr,"ERROR:Unable to copy GTI HDU to output file \n");
    goto cleanup;
  }
  status=update_keys(outfptr,gti_start,gti_end,ndone);
  if (status) {
    fprintf(stderr,"ERROR:unable to update keywords\n");
    goto cleanup;
  }    
  
  /* Get the number of rows in the GTI binary table */
  fits_read_key(infptr2,TINT,"NAXIS2",&num_gti_rows,
		     comment, &status);
  if (status) {
    fprintf(stderr,"ERROR:cannot get GTI extension rows \n");
    goto cleanup;
  }
  /* Delete num_gti_rows in GTI binary table */
  num_gti_rows = (long)num_gti_rows;  
  fits_delete_rows(outfptr, 1, num_gti_rows, &status);
  if (status) {
    fprintf(stderr,"ERROR:cannot delete existing GTI rows \n");
    goto cleanup;
  }
  /* Copy the new gti_start and gti_end values to
     the output file GTI extension */
  fits_write_col(outfptr, TDOUBLE, 1, 1, 1, ndone, gti_start, &status);
  fits_write_col(outfptr, TDOUBLE, 2, 1, 1, ndone, gti_end, &status);
 
  cleanup:
  headas_chat(4, "Returning from batsumdph \n");
  if (warning) 
     headas_chat(0,"Warning -- %d TNULLs written to output\n",warning);
  if (outfptr) fits_close_file(outfptr, &status);
  if (infptr1) fits_close_file(infptr1, &status);
  if (infptr2) fits_close_file(infptr2, &status);
  return(status);
}
   
/*--------------------------------------------------------------------*/

/* HAK 17-Dec-2003
   New routine to update the keywords:  TSTART, TSTOP, TELAPSE, ONTIME,
   LIVETIME, EXPOSURE */

int update_keys (
   fitsfile *outfptr,
   double *gti_start, 
   double *gti_end,
   int n)

{
  int i,status=0;
  double write_time;
  
  fits_update_key(outfptr,TDOUBLE,"TSTART",&gti_start[0],NULL,&status); 
  
  fits_update_key(outfptr,TDOUBLE,"TSTOP",&gti_end[n-1],NULL,&status); 
  
  write_time = gti_end[n-1] - gti_start[0];
  fits_update_key(outfptr,TDOUBLE,"TELAPSE",&write_time,NULL,&status); 
  
  write_time = 0;
  for (i=0; i<n; i++) 
    write_time += (gti_end[i] - gti_start[i]);
  fits_update_key(outfptr,TDOUBLE,"ONTIME",&write_time,NULL,&status); 
  
  fits_update_key(outfptr,TDOUBLE,"LIVETIME",&write_time,NULL,&status); 
  
  fits_update_key(outfptr,TDOUBLE,"EXPOSURE",&write_time,NULL,&status); 


  if (status) { 
    fprintf(stderr, "ERROR status %d writing timing keywords\n",
	    status);
  }
  return(status);

}


