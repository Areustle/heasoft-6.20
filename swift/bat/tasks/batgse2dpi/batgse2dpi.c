#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "bat_gswdev.h"

/*
 * HISTORY
 * -------
 *   Version 1.0 written by Susima Abeyagunawardene, SSAI, January 2003
 *   */

/* HAK 22-July-2003  Cleaned up code by removing three unnecessary arrays. */

/* HAK 23-July-2003  Fixed bug whereby the window high and low arrays were 
   not being indexed properly if there were missing rows in the windows files.
   The code now uses the block/dm/side/det values from the windows rows to
   index the windows arrays. 
   Also changed several more arrays to malloc-ed pointer arrays. */
 

#define TOOLSUB batgse2dpi
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"


/* Function Prototypes */
int batgse2dpi(void);
int batgse2dpi_getpar(char *infile, char *outfile, char *histmode,
		       char *windows, char *maskfile,
		       double *deadpercount, int *deadapp );
int batgse2dpi_builddpi(char *infile, char *outfile, char *histmode,
	                 char *windows, char *maskfile,
	                 double *deadpercount, int *deadapp );	

/* ------------------------------------------------------------------ */
int batgse2dpi(void)
{
 char infile[PIL_LINESIZE], outfile[PIL_LINESIZE], maskfile[PIL_LINESIZE];
 char windows[PIL_LINESIZE];
 char histmode[10];
 int status = 0;
 
 static char taskname[80] = "batgse2dpi";
 static char version[8] = "1.00";
 int deadapp = 1;
 double deadpercount = 0.0;
 
 /* Register taskname and version */
 set_toolname(taskname);
 set_toolversion(version);

/* printf("batgse2dpi-Modified April 1 2003 - has Craig's updates\n");*/
 /* Get input parameters */
 status = batgse2dpi_getpar(infile,  outfile, histmode, windows,
		             maskfile, &deadpercount, &deadapp);

   headas_chat(4,"Name of infile is %s \n", infile);
   headas_chat(4,"Name of outfile is %s \n", outfile);
   headas_chat(4,"Name of histmode is %s \n", histmode);
   headas_chat(4,"Name of windows is %s \n", windows);
   headas_chat(4,"Name of maskfile is %s \n", maskfile);
   headas_chat(4,"Name of deadpercount is %f \n", deadpercount);
   headas_chat(4,"Name of deadapp is %d \n", deadapp);

if (!status)
     status = batgse2dpi_builddpi(infile, outfile, histmode, windows,
     maskfile, &deadpercount, &deadapp);
 return (status);
}


/* ---------------------------------------------------------------- */
int batgse2dpi_getpar(
    char *infile,     /* O - Input file name containing a list of
			     files to be read by batgse2dpi.c */
    char *outfile,    /* O - Output file to which information from
			     the list of files is written */
    char *histmode,   /* O - total or window indicating whether to
			     use all the counts for each detector as 
			     in HITSMAP extension or a window of
			     counts as in the SPECTRUM extension
			     which has 4096 counts values per detector */
    char *windows,    /* O - Name of file containing the window 
			     for processing SPECTRUM extension */ 
    char *maskfile,   /* O - output file to which detector "mask"
			     image is written */
    double *deadpercount, /* O - Dead time per count (s) */
    int *deadapp)         /* O - Boolean 1=apply dead time correction
		                         0= do not apply dead time
			                    correction */		 
{ 
   int status = 0;
   
   if (( status = PILGetFname("infile", infile)))
         fprintf(stderr, "Error reading the 'infile' parameter.\n");

   else if (( status = PILGetFname("outfile", outfile)))
	      fprintf(stderr, "Error reading the 'outfile' parameter.\n");
   
   else if (( status = PILGetString("histmode", histmode)))
	      fprintf(stderr, "Error reading the 'histmode' parameter.\n");

   else if (( status = PILGetString("windows", windows)))
	      fprintf(stderr, "Error reading the 'windows' parameter.\n");
   
   else if (( status = PILGetString("detmask", maskfile)))
	      fprintf(stderr, "Error reading the 'maskfile' parameter.\n");
   
   else if (( status = PILGetReal("deadpercount", deadpercount)))
	      fprintf(stderr, "Error reading 'deadpercount' parameter.\n");

   else if (( status = PILGetBool("deadapp", deadapp)))
	      fprintf(stderr, "Error reading 'deadapp' parameter.\n");
   
   if (status) return(status);
   
   /* Case where filename is NONE, change to empty string */ 
   if (strcasecmp(maskfile, "none") == 0) {
       maskfile[0] = 0;
   }    
   headas_chat(4,"Name of infile is %s \n", infile);
   headas_chat(4,"Name of outfile is %s \n", outfile);
   headas_chat(4,"Name of histmode is %s \n", histmode);
   headas_chat(4,"Name of windows is %s \n", windows);
   headas_chat(4,"Name of maskfile is %s \n", maskfile);
   headas_chat(4,"Name of deadpercount is %f \n",*deadpercount);
   headas_chat(4,"Name of deadapp is %d \n", *deadapp);
   
   /* Error checking on deadtime */
   if (*deadpercount < 0) {
       *deadpercount = 0;
   }
   (*deadpercount) = 1e-6 * (*deadpercount);   
   return(status);
}   

         
/* ----------------------------------------------------------------- */

static float image_array[DAP_ROWS][DAP_COLS];
static int detmask[DAP_ROWS][DAP_COLS];

int batgse2dpi_builddpi(
    char *infile,         /* I - Name of file containing a list of
			         gse file names */
    char *outfile,        /* I - Output dpi file name */
    char *histmode,       /* I - Mode indicating which extension
			         HITSMAP or SPECTRUM to be used in
				 the processing. */
    char *windows,        /* I - File containing windows for processing
			         SPECTRUM extensions */
    char *maskfile,       /* I - Output detector mask file name. */
    double *deadpercount, /* I - Dead time per count (s) */
    int *deadapp)         /* I - Boolean 1 = apply dead time correction
			                 0 = do not apply dead time
					     correction */
{
  char filenames[64][FLEN_FILENAME];    /* Names of 64 files each upto
					   FLEN_FILENAME characters long  */
  char key_name[25], comment[80];
  int status = 0;
  int i, j, k, m, n, file_count = 0; 
  int det_ct;
  int hdu_type;
  int blockid, dmid, sideid;
  int nkeys;                  /* Number of keys in the first HITSMAP or
		                 SPECTRUM extension of the first of 16
				 files read */
  int naxis;                  /* Number of dimensions in fits array */
  /* HAK 23-July-2003  Change these definitions to pointer arrays to allow the
     code to run under Mac OSX */
  /* float image_array[173][286];
     int detmask[173][286]; */
  /* float **image_array;
  int **detmask; */

  long dim_size[2];           /* Size of each dimension in fits array */
  int colnum;                 /* Number of the column corresponding 
		                 to COUNTS column */
  int col_counts[128];        /* Counts from each detector of a
				 HITSMAP extension of FITS file
			         or total counts in specified window
			         per detector of a SPECTRUM extension  */
  int total4096_cts[128];     /* Array containing total of 4096 count
				 values for each detector */ 
  int spectrum_cts[4096];     /* Array of counts for each detector of a
		                 SPECTRUM extension of a FITS file */ 
  int sum_spectrum_cts;       /* Sum of the counts in specified number of 
			         array elements for each detector */ 
  int sum4096_spec_cts;       /* Sum of all 4096 SPECTRUM extension counts
				 for each detector */ 
  /* These variable values are generated by batidconvert */
  short int block[128], dm[128], det[128];
  short int row[128], col[128];
  /* End of batidconvert output values */
  
  long total_elements;     /* Total number of elements written to
			      image array */
  unsigned short int detid[128];
  char card[256];
  int *w_block;       /* Array of block ids from windows file */
  int new_block[256];      /* Every 128th value of block id */
  int *w_dm;        /* Array of dim values from windows file */
  int new_dm[256];         /* Every 128th value of dm */
  int *w_side;       /* Array of side id values from windows file */
  int new_side[256];       /* Every 128th value of side id */
  int *w_det;        /* A value of 0 to 127 contained in each array
			      element. This value along with corresponding
			      window_block, window_dim, window_side values
			      is used to compute the window_det_id */
  int *w_det_id;    /* Value computed using the formula
			       w_det_id[i] = w_det[i] + 128*w_side[i] +
				        256*w_dm[i] + 2048*w_block[i] */
  int *temp_w_low = 0;
  int *temp_w_high = 0;
  int *w_low = 0;        /* Low end of 4096 array of SPECTRUM 
			       counts to be used */
  int *w_high = 0;       /* High end of 4096 array of SPECTRUM 
			       counts to be used */
  int nhdus;               /* Number of HDUs in the FITS file
			      being processed */  
  int winhdus;             /* Number of HDUs in the windows FITS file */
  int w_count;             /* Counter of window table rows */
  int f_count;             /* Count of block, dm and side ids read from
			      each file */
  /* HAK 22-July-2003.  Removed these unneeded arrays. */
  /* int f_block[32768]; */     /* Array containing block ids obtained from
			      files */
  /* int f_dm[32768]; */        /* Array of dm values obtained from files */
  /* int f_side[32768]; */     /* Array of side ids obtained from files */
  int num_cols;            /* Number of columns in the windows file */
  /*int num_rows;             Number of rows in the windows file */
  long  num_rows;          /* Number of rows in the windows file */
  int hdutype;             /* Type of HDU */
  char *btbl_cols[6];      /* The binary table column names */
  char *tform[6];
  char *tunit[6];
  int firstfile = 1;       /* Reset to 0 after creating output file */
  FILE *fp;
  fitsfile *infptr = 0, *outfptr = 0, *winfptr = 0;
  fitsfile *maskfptr = 0;

  /* Used in deadtime correction */
  float exposure, deadtime, livetime, liverat;
  float exposure_vals[256], deadtime_vals[256], livetime_vals[256];
  int exposure_ct = -1;
  int totcounts = 0;      /* Total of all the counts for all the detectors
			     in HITSMAP extensions or total of counts 
			     within the windows of all the detectors in a
			     SPECTRUM extension */
  int tot4096cts = 0;     /* Total of all 4096 values for all detectors
			     in a SPECTRUM extension */
  int badval = 1, goodval = 0;

  /* Initialize the image array */
  for(i = 0; i < 173; i++){
     for(k = 0; k < 286; k++){
       /* image_array[i][k] = (float)(i*286)+(float)(k); */
         image_array[i][k] = 0.0;
	 detmask[i][k] = badval; 
         if (i < 3) {
	    headas_chat(5,"%3.0f ",image_array[i][k]);
         }
     }
  }     
  for (i=0; i < 800; i++) headas_chat(5,"%3.0f ",image_array[0][i]);


  f_count = -1;
  /* Open the windows file and get the required information
     if histmode is window */
  if (strcmp(histmode,"window") == 0 ) {
     fits_open_file(&winfptr, windows, READONLY, &status);
     if (status != 0){
         fprintf(stderr,"Unable to open FITS file %s \n", windows);
         goto cleanup;
     }      
     headas_chat(3,"Opened FITS file %s \n", windows);
     fits_get_num_hdus(winfptr, &winhdus, &status);
     headas_chat(3,"Number of hdus in %s is %d \n",
			  windows, winhdus);
     /* Move to first extension which is the second HDU */
     fits_movabs_hdu(winfptr, 2, &hdutype, &status);
     headas_chat(3,"hdutype is %d \n",hdutype);
     /* get the number of columns in the windows file */
     fits_get_num_cols(winfptr, &num_cols, &status);
     headas_chat(3,"Number of columns in windows file %d\n",
					     num_cols);
     fits_get_num_rows(winfptr, &num_rows, &status);
     headas_chat(3,"Number of rows in windows file %d\n",
					     num_cols);

     /* HAK 22-July-2003  Initialize the windows arrays */
     w_high = (int*)(malloc(NUM_ELEMENTS*sizeof(int)));
     w_low = (int*)(malloc(NUM_ELEMENTS*sizeof(int)));
     for (w_count = 0; w_count < NUM_ELEMENTS; w_count++) {
        w_low[w_count] = 0;
        w_high[w_count] = 4095;
     }

     /* Read the binary table and get the values for
        for BLOCK, DM SIDE DET, WINDOW_LOW,WINDOW_HIGH */ 
         w_block = (int*)(malloc(num_rows*sizeof(int)));
         w_dm = (int*)(malloc(num_rows*sizeof(int)));
         w_side = (int*)(malloc(num_rows*sizeof(int)));
         w_det = (int*)(malloc(num_rows*sizeof(int)));
         w_det_id = (int*)(malloc(num_rows*sizeof(int))); 
         temp_w_high = (int*)(malloc(num_rows*sizeof(int)));
         temp_w_low = (int*)(malloc(num_rows*sizeof(int)));

	 fits_read_col(winfptr, TINT, 1, 1, 1, num_rows, NULL,
           	       w_block, NULL, &status);
         fits_read_col(winfptr, TINT, 2, 1, 1, num_rows, NULL,
				    w_dm, NULL, &status);
         fits_read_col(winfptr, TINT, 3, 1, 1, num_rows, NULL,
				    w_side , NULL,&status);
         fits_read_col(winfptr, TINT, 4, 1, 1, num_rows, NULL,
	                            w_det, NULL,&status); 
         fits_read_col(winfptr, TINT, 5, 1, 1, num_rows, NULL,
				    temp_w_low, NULL, &status);
         /*if(w_low[w_count] < 0) w_low[w_count] = 0; 
         headas_chat(3,"In row %d w_low is %d \n",
			   (w_count+1), w_low[w_count]);*/
         fits_read_col(winfptr, TINT, 6, 1, 1, num_rows, NULL,
				    temp_w_high, NULL,&status);

	 for (w_count = 0; w_count < num_rows; w_count++) {
	    /* Compute the detector id */
	    w_det_id[w_count] = w_det[w_count] +
	       128*w_side[w_count] + 256*w_dm[w_count] +
	       2048*w_block[w_count]; 
           headas_chat(4,"For count %d w_det_id, w_low, w_high are %d %d %d \n",
	    w_count,w_det_id[w_count],w_low[w_count],w_high[w_count]); 	
	     /*End of computing the detector ids */		     	
         } 

	 /* HAK 23-July-2003  Add code to put the windows in the correct 
	    location if there are not numbers for every detector in the
	    windows file. */
         for (w_count = 0; w_count < num_rows; w_count++) {
	    w_low[w_det_id[w_count]] = temp_w_low[w_count]; 
	    w_high[w_det_id[w_count]] = temp_w_high[w_count]; 
         }
         free(w_block);
         free(w_dm);
         free(w_side);
         free(w_det); 
         free(temp_w_low);
         free(temp_w_high); 
         free(w_det_id); 

	 /* Check the w_high and w_low values for each row
	    and reset the values if necessary */
	 for (w_count = 0; w_count < NUM_ELEMENTS; w_count++) {
	     if(w_low[w_count] < 0) {
	        headas_chat(5,"In row %d w_low is %d \n",
		               (w_count + 1), w_low[w_count]);
		w_low[w_count] = 0;
             }		
	     if(w_high[w_count] > 4095) w_high[w_count]=4095;
         /*headas_chat(3,"In row %d w_high is %d \n",
			   (w_count+1), w_high[w_count]);*/
             if (w_low[w_count] > w_high[w_count]){
	        if (w_high[w_count] != -1) {
                   fprintf(stderr,"ERROR: low window > high window\n");
                   fprintf(stderr,"       defaulting to full range\n");
                }
	        w_low[w_count] = 0;
	        w_high[w_count] = 4095;
	     }
	 } /* End of checking the counts */     

   /*  }  End of reading windows file */	
  } /* End of reading windows file when histmode = "window" */  
  /* Open the ascii file containing FITS file names for reading
     only */    
  if (( fp = fopen(infile ,"r")) == NULL) {
      headas_chat(5,"Unable to open ascii file %s \n",infile);
      status = 1;
  }      
  else{
       headas_chat(5,"Successfully opened ascii file %s \n",infile);
       /* Get the number of files listed in infile and the number
	  of characters in file name of each file in the list */
       i = 0;
       while (feof(fp) == 0) {
	       if (fgets(filenames[i], 255, fp) == 0) break;
	       filenames[i][255] = 0;   /* Null terminate */
	       for (j = 0; j < 255; j++)        /* Remove C/Rs */
                  if (filenames[i][j] == '\n') filenames[i][j] = 0;
               if (strlen(filenames[i]) == 0) continue; /* Skip the blanks */
	       for (j=0; filenames[i][j] && isspace(filenames[i][j]); j++);
	       if (strlen(filenames[i]) == j) continue;
               headas_chat(5,"  Filename %d: '%s'\n",i,filenames[i]);
	       i++;
        }	       
       file_count = i;
       headas_chat(5,"Number of files listed is %d \n",file_count);   
       /* Delete existing outfile if outfile already exists */
       headas_clobberfile(outfile);
       /* Print out the names of the files in file_list */
       for (i = 0; i < file_count; i++) {
	   headas_chat(5, "Name of file is %s \n",filenames[i]);    
	   /* Open the FITS file */
	   status = 0;
	   fits_open_file(&infptr, filenames[i], READONLY,
			   &status);
	   if(status == 0){
              headas_chat(5," Opened file %s \n",filenames[i]);
           }else {
	    fprintf(stderr,"WARNING: could not open %s \n",filenames[i]);
            continue;
           }	   
	   /* Get number of hdus in the FITS file */
	   fits_get_num_hdus(infptr, &nhdus, &status); 
	   if (status || (nhdus <= 1)) {
	      fprintf(stderr, "WARNING: %s has incorrect extensions \n",
		       filenames[i]);
              status = 0;
              fits_close_file(infptr, &status); 
              continue; /* Process the next file in the loop */
           }
           headas_chat(5,"Found %d extensions in %s\n",
         		          nhdus, filenames[i]);
           if ((strcmp(histmode,"total")) == 0) { /* Process HITSMAP */
             for(m = 1; m <= nhdus; m++) {
	      /* Move to the HDU whose number is given by m
	         the primary HDU is 1 */
	        headas_chat(5, "Moving to HDU %d \n",m);
                fits_movabs_hdu(infptr, m, &hdu_type, &status);
                if (status) {
	        /* Could not get hdu_type, try the next extension */
	          status = 0;
                  continue;  /* Go to the next extension */
                }
                /* This extension is ok get the key value */
                fits_read_key(infptr,TSTRING,"EXTNAME",key_name,0,&status);
                if (status) {
	           /* No EXTNAME keyword, so advance to next extension */
	           status = 0;
                   continue;
                }
                /* Found EXTNAME keyword check whether it is HITSMAP */
                headas_chat(5, "EXTNAME = %s \n", key_name);
                if (strncmp(key_name, "HITSMAP", 7) != 0) {
		   /* Wrong extension name, so advance to next extension */
		   continue;
                }
  	        /* Found HITSMAP extension. */

		/* Be sure the exposure is >= 0.  Less than zero
                   appears to mean, "this histogram is empty" */
		/* **** exposure = 0.0;
		fits_read_key(infptr,TFLOAT,"EXPOSURE",
			      &exposure,comment,&status);
		if ((status != 0) || (exposure <= 0)) {
		  status = 0;
		  headas_chat(5, "Extension has < 0 exposure.  Skipping.\n");
		  continue;
		}
		exposure_ct++;    *****  */ 

		/* Check whether this is the
	           first HITSMAP extension of first of 16 files opened */
		/*  ****  New  *** */
		/* Found HITSMAP extension. Check whether this is the
		   first HITSMAP extension of first of 16 files
		   opened */
		exposure_ct++;
	        exposure = 0.0;
	        fits_read_key(infptr,TFLOAT,"EXPOSURE",
			      &exposure,comment,&status);	
	        if (firstfile) {
		   firstfile = 0;  /* Reset to indicate next is not the 1st
		  		      HITSMAP extension */
		   /* Get the number of keys in this HDU */
                   fits_get_hdrspace(infptr,&nkeys, NULL, &status);
                   headas_chat(5, "Copying keywords\n");
                   headas_chat(5, "Number of keys is %d \n", nkeys);
		   /* Open the output file and write all the
		    key information to the above file */
		   if(fits_create_file(&outfptr,outfile,&status)){
		      fprintf(stderr,"Unable to open %s\n",outfile);
	              goto cleanup;
	            } else { /* Opened output file */
	              headas_chat(5,"Opened file %s \n",outfile);
		      naxis = 2;
		      dim_size[0] = DAP_COLS;
		      dim_size[1] = DAP_ROWS;
		      /* Create new primary image HDU */
		      if(fits_create_img(outfptr,FLOAT_IMG,
		     	  naxis,dim_size,&status)) {
		       fprintf(stderr, "ERROR: could not make output image\n");
	               goto cleanup;
            	      }	  
		      headas_chat(5,"Created image HDU \n");
             	      /*  Create detector mask  */
		      if (maskfile[0]) {
			if (fits_create_file(&maskfptr,maskfile,&status)) {
			  fprintf(stderr,"ERROR: could not create mask %s\n",
				        maskfile);
           		    goto cleanup;
			}
		        if (fits_create_img(maskfptr, FLOAT_IMG,
					     naxis,dim_size,&status)) {
		           fprintf(stderr,"ERROR: could not make output mask \n");
	                   goto cleanup;
	                }
	                headas_chat(5,"Created detmask HDU \n");
        	      }	  
			         	     
	              /* Copy the keywords */
                      for(n = 0; n < nkeys; n++){
	                fits_read_record(infptr,n,card,&status);                               
        	        if((fits_get_keyclass(card) >= TYP_REFSYS_KEY)){ 
                           fits_write_record(outfptr,card,&status);
			   if (maskfile[0])
		             fits_write_record(maskfptr,card,
						          &status);	
		        } /* Wrote keyword to output file */  

                      } /* Read all keywords and wrote them 
				     to output file */
                    } /* End of writing HDU to image file */ 	     
	          } /* End of processing 1st HISTMAP in 1st file */	
	    	  /* Get the values of the keywords BLOCK_ID,
		     DM_ID, SIDE_ID */ 
		  fits_read_key(infptr,TINT,"BLOCK_ID",&blockid,0,&status);  
		  fits_read_key(infptr,TINT,"DM_ID",&dmid,0,&status);
		  fits_read_key(infptr,TINT,"SIDE_ID",&sideid,0,&status);
		  headas_chat(5,"blockid is %d \n", blockid);
		  headas_chat(5,"dmid is %d \n", dmid);
	  	  headas_chat(5,"sideid is %d \n", sideid);
	          if(fits_get_colnum(infptr, CASEINSEN, "COUNTS",
	                 &colnum, &status)) goto cleanup;
	          headas_chat(5, "Column of COUNTS is %d \n",colnum);
	          /* Compute the detector id for each channel */ 
                  for(det_ct = 0; det_ct < 128; det_ct++){
                    /* Save the blockid, dmid and sideid for
	       	       all 128 detectors although each will
	               have the same value for all 128 */ 	  
		    /* HAK 22-July-2003 Removed these unnecessary lines of 
		       code */
	            /* f_count++;
                    f_block[f_count] = blockid;
	            f_dm[f_count] = dmid;	             
		    f_side[f_count] = sideid;	       
	            headas_chat(5,"f_count,blockid,dm,sideid are %d %d %d %d  \n", 
				f_count, f_block[f_count],
				f_dm[f_count], f_side[f_count]); */
		    detid[det_ct] = det_ct + 128*sideid + 256*dmid
		                                      + 2048*blockid;
		    headas_chat(5, "detid is %d \n", detid[det_ct]);
                  } /* End of computing detector id for each
				   channel */
	          fits_read_col(infptr,TINT,colnum,1,1,128,
				  NULL, col_counts, NULL, &status);
	          /* Call batidconvert to compute the row and 
		     column number of image_array to store the
		     counts corresponding to each detector array */       		    
   	          batidconvert(128,detid,block,dm,det,row,col);
                  totcounts = 0;
		  for(det_ct = 0; det_ct < 128; det_ct++){
		      headas_chat(5, "row, value is %d \n",row[det_ct]);
		      headas_chat(5, "col, value is %d \n",col[det_ct]);
		      headas_chat(5, "counts=%d \n",col_counts[det_ct]); 
		      /* Copy the contents of col_counts array to
		         the appropriate column and row of image
	                 array */
	              image_array[row[det_ct]][col[det_ct]] = 
			 col_counts[det_ct]; 
		      detmask[row[det_ct]][col[det_ct]] = goodval;

		      totcounts += col_counts[det_ct];

		   } /* End of processing 128 detectors */   	    

	           /* Apply deadtime correction if DEADC is set and
                       EXPOSURE is non-zero */
		   if (exposure != 0) {
		       headas_chat(5, "Applying deadtime correction\n");
		       deadtime = (float) totcounts *
		                   (*deadapp) * (*deadpercount);
		       if (deadtime > exposure) {
			 fprintf(stderr, "ERROR: deadtime was larger than exposure!\n");
			 fprintf(stderr, "       (setting deadtime to 0)\n");
			 deadtime = 0;
		       }
		       livetime = exposure - deadtime;
		       liverat  = exposure / livetime;
		       headas_chat(2, "   EXPOSURE=%f\n", exposure);
		       headas_chat(2, "   LIVETIME=%f\n", livetime);
		       headas_chat(2, "   DEADTIME=%f\n", deadtime);
		  
		       for(det_ct = 0; det_ct < 128; det_ct++){
			 image_array[row[det_ct]][col[det_ct]]
			  *= liverat; 
		       } 
		    } /* End of processing if exposure !=0 */   
		    else{ /* exposure == 0 */
		         deadtime = 0.0;
		         livetime = 0.0;
	            }	  
                    /* save the exposure, deadtime and livetime
			        values */
		    exposure_vals[exposure_ct] = exposure;
        	    deadtime_vals[exposure_ct] = deadtime;
	            livetime_vals[exposure_ct] = livetime;
               } /* End of for loop for m */		
	      } /* End of processing HITSMAP extensions */
              /*  ***** histmode = window processing *****  */
	      else if ((strcmp(histmode, "window")) == 0){
			headas_chat(5,"Processing SPECTRUM extension \n");
			/* HAK 22-July-2003 Moved this stuff to where the
			   data is first read in (so arrays can be freed). */
		        /* for (w_count = 0; w_count < 32768; w_count++){ */
		             /* Compute the detector id */
			     /* w_det_id[w_count] = w_det[w_count] +
				  128*w_side[w_count] + 256*w_dm[w_count] +
			   	  2048*w_block[w_count]; 
                             headas_chat(4,"For count %d w_det_id, w_low, w_high are %d %d %d \n",
					 w_count,
				 w_det_id[w_count],w_low[w_count], 
				                   w_high[w_count]);	 
						   } */ /*End of computing the detector ids */		     	
			for(m = 1; m <= nhdus ; m++) {
			/* Move to HDU whose number is given by m
                           the primary HDU is 1 */
                           headas_chat(5, "Moving to HDU %d \n",m);
			   fits_movabs_hdu(infptr, m, &hdu_type, &status);
			   if (status ){
                              /* Could not get hdu_type, try the next
                                 extension */
                             status = 0;
                             continue; /* Go to next extension */
                           }
                           /* This extension is ok get the key value */
                           fits_read_key(infptr,TSTRING,"EXTNAME",
                                         key_name,0,&status);
                           if (status) {
                             /* No EXTNAME keyword, so advance to next
                                 extension */   
                              status = 0;
                              continue;
                           }  
                           /* Found EXTNAME keyword check whether it is
                              SPECTRUM */
                           headas_chat(5, "EXTNAME = %s \n", key_name);
                           if (strncmp(key_name, "SPECTRUM", 8) != 0) { 
                              /* Wrong extension name, so advance to
                                  next extension */ 
                              continue;
                           }
			   /* ****  Delete this part ****  */
                           /* Found SPECTRUM extension. */

			   /* Be sure the exposure is >= 0.  Less than zero
			      appears to mean, "this histogram is empty" */
			   /* exposure = 0.0;
			   fits_read_key(infptr,TFLOAT,"EXPOSURE",
					 &exposure,comment,&status);
			   if ((status != 0) || (exposure <= 0)) {
			     status = 0;
			     headas_chat(5, "Extension has < 0 exposure.  Skipping.\n");
			     continue;
			   }
			   exposure_ct++; */

			   /* Check whether
                              this is the first SPECTRUM extension of
                              the first of 16 files opened. */
			   /*  **** End of deletion ****   */
			    
			   /*  ****  New part ****   */
			   /* Found SPECTRUM extension check whether
			      this is the first SPECTRUM extension of
			      the first of 16 files opened. */
			   exposure_ct++;
			   exposure = 0.0;
			   fits_read_key(infptr,TFLOAT,"EXPOSURE",
					 &exposure,comment,&status);
			   /* **** End of new part **** */
                           if (firstfile) {
                               firstfile = 0; /* Reset to indicate next   
                                                 HDU is not the first
                                                 SPECTRUM extension */
                               /* Get the number of keys in this HDU */
                               fits_get_hdrspace(infptr,&nkeys, NULL,
                                                             &status); 
                               headas_chat(5, "Copying keywords\n");
                               headas_chat(5, "Number of keys is %d \n",
                                                                 nkeys);
                               /* Open the output file and write all the
                                   key information to the above file */
                               if(fits_create_file(&outfptr,outfile,
                                                          &status)){
                                  fprintf(stderr,"Unable to open %s\n",
                                                             outfile);
                                  goto cleanup;     
                                     
                               } else{ /* Opened output file */
		                 headas_chat(5,"Opened file %s \n",outfile);
	                         naxis = 2;
	                         dim_size[1] = DAP_ROWS;
	                         dim_size[0] = DAP_COLS;
	                         /* Create new primary image HDU */
	                         if(fits_create_img(outfptr,FLOAT_IMG,
				     naxis,dim_size,&status)) {
				    fprintf(stderr, "ERROR: could not make output image \n");
				    goto cleanup;
				 }   
	                         headas_chat(5,"Created image HDU \n");
				 /* Create maskfile */
				 if (maskfile[0]) {
				    if (fits_create_file(&maskfptr,
						maskfile,&status)){
				       fprintf(stderr, "ERROR: could not create mask %s \n", maskfile);
				       goto cleanup;
			            }	       
				    if(fits_create_img(maskfptr,FLOAT_IMG,
					   naxis,dim_size,&status)) {
				      fprintf(stderr,"ERROR: could not make output mask\n");
			              goto cleanup;
			            }
	                            headas_chat(5,"Created detmask HDU \n");
	                         }			    
                                 /* Copy the keywords */
			         for(n = 0; n < nkeys; n++){
	            	            fits_read_record(infptr,n,card,&status);
			            if((fits_get_keyclass(card) >=
		       	               TYP_REFSYS_KEY)){
		                      fits_write_record(outfptr,card,
			                         &status);
			              if (maskfile[0])
					  fits_write_record(maskfptr,card,
							   &status);	
	                            } /* Wrote keyword to output file */
                                 }  /*Read all keywords and wrote them
                                     to output file */
                               } /* End of writing HDU to image file*/

                            }  /*End of processing first SPECTRUM
                                  HDU in first file */
                            /* Get the values of keys BLOCK_ID, 
                               DM_ID, SIDE_ID */  
                            fits_read_key(infptr,TINT,"BLOCK_ID",
                                             &blockid,0,&status);
                            fits_read_key(infptr,TINT,"DM_ID",&dmid,
                                               0,&status);
                            fits_read_key(infptr,TINT,"SIDE_ID", 
                                               &sideid,0,&status);
                            headas_chat(4,"blockid is %d \n", blockid);
                            headas_chat(4,"dmid is %d \n", dmid);
                            headas_chat(4,"sideid is %d \n", sideid);
                            if(fits_get_colnum(infptr, CASEINSEN, "counts",
                                         &colnum, &status)) goto cleanup;
                            headas_chat(5, "Column of COUNTS is \n",
                                              colnum);
                            for(det_ct = 0; det_ct < 128; det_ct++){
                                /* Save blockid, dmid and side id
                                   obtained from the files */
			        /* HAK 22-July-2003 Removed these lines of 
				   unnecessary code. */
                                /* f_count++;
                                f_block[f_count] = blockid;
                                f_dm[f_count] = dmid;
                                f_side[f_count] = sideid;
                                headas_chat(3,"f_count,block,dm,side ids are %d %d %d %d \n",
					    f_count,f_block[f_count],
					    f_dm[f_count], f_side[f_count] ); */
                                detid[det_ct] = det_ct + 128*sideid + 256*dmid
                                                       + 2048*blockid;
                                headas_chat(4,"detid is %d \n",
                                            detid[det_ct]);
                                fits_read_col(infptr,TINT,colnum,
                                      (det_ct+1),1,4096,NULL,
                                  &spectrum_cts[0], NULL,&status);
                                /* Initialize the sum of the counts in
				   window */
                                sum_spectrum_cts = 0;
				/* Initialize sum of all 4096 count
				  values per detector */
				sum4096_spec_cts = 0;
                                headas_chat(5,"For detid %d w_low = %d w_high = %d \n",
					    detid[det_ct],
                                  w_low[detid[det_ct]],w_high[detid[det_ct]]);
                                for(k = w_low[detid[det_ct]];
                                        k <= w_high[detid[det_ct]]; k++){
                                    sum_spectrum_cts = sum_spectrum_cts +
                                               spectrum_cts[k];
                                }
                                col_counts[det_ct] = sum_spectrum_cts;
				/* Total up the 4096 values of counts
				   per detector */
			        for (j = 0; j < 4096; j++) {
				    sum4096_spec_cts = sum4096_spec_cts +
				                         spectrum_cts[j];
			        }
		                /* Save total for this detector */
		                total4096_cts[det_ct] = sum4096_spec_cts;		
  
                           } /* End of computing detid & sum_spectrum_cts
                                    for all 128 detectors */
                           batidconvert(128,detid,block,dm,det,row,col);
                           totcounts = 0;
			   tot4096cts = 0;
                           for(det_ct = 0; det_ct < 128; det_ct++){
			       image_array[row[det_ct]][col[det_ct]] =
			       col_counts[det_ct]; 
                               detmask[row[det_ct]][col[det_ct]] = goodval;
                               totcounts += col_counts[det_ct];
			       tot4096cts += total4096_cts[det_ct];
                           }  /* End of processing 128 detectors*/
                           /* Apply deadtime correction if DEADC is set and
                              EXPOSURE is non-zero */
                           if (exposure != 0) {
                              headas_chat(5, "Applying deadtime correction\n");
                              deadtime = (float) tot4096cts * (*deadapp)
                                                 * (*deadpercount);
			      if (deadtime > exposure) {
				fprintf(stderr, "ERROR: deadtime was larger than exposure!\n");
				fprintf(stderr, "       (setting deadtime to 0)\n");
				deadtime = 0;
			      }
                              livetime = exposure - deadtime;
                              liverat  = exposure / livetime;
                              headas_chat(2, "   EXPOSURE=%f\n",
                                                     exposure);
                              headas_chat(2, "   LIVETIME=%f\n",
                                                livetime);
                              headas_chat(2, "   DEADTIME=%f\n",
                                                deadtime); 
                             for(det_ct = 0; det_ct < 128; det_ct++){
			       image_array[row[det_ct]][col[det_ct]]
				*= liverat; 
                             }
                          } /* End of processing when exposure != 0 */
                          else { /* Exposure is zero */
                                deadtime = 0.0;
                                livetime = 0.0;
                          }
                          /* Save exposure, deadtime and livetime
                                  in arrays */
                          exposure_vals[exposure_ct] = exposure;
                          deadtime_vals[exposure_ct] = deadtime;
                          livetime_vals[exposure_ct] = livetime;
		     }  /*End of for loop m*/ 
	       }  /*End of processing SPECTRUM extensions*/ 	     
	       else{	     
	        /* histmode is something other than total
		         or window */
		    fprintf(stderr,"ERROR:Invalid histmode %s \n",
				     histmode);
		    status = 1;
		    goto cleanup;   
	       } 	       
	     fits_close_file(infptr, &status); /* Close FITS file and open */
					       /* the next in the loop*/  
           /* }  End of processing file with non blank name */
       } /* End of for loop for i*/	    
       fclose(fp);  /* Close ascii file */
       /* Write image data to output file */
       total_elements = 286*173;
       
       if (fits_write_img(outfptr, TFLOAT , 1, total_elements,
	  &(image_array[0][0]), &status)) goto cleanup; 	       
       fits_update_key(outfptr,TLOGICAL,"DEADAPP",deadapp,
		         "Deadtime corrections was applied", &status);
       headas_chat(5, "Wrote image data to %s \n", outfile);
       if (maskfile[0]) {
	   if (fits_write_img(maskfptr,TINT,1,total_elements,
	    &(detmask[0][0]), &status)) goto cleanup; 
	   fits_update_key(maskfptr, TINT, "GOODVAL", &goodval, 
			   "Quality value which indicates good pixels", 
			   &status);
           headas_chat(5, "Wrote mask data to %s \n", maskfile);
       }	   
       /* Set the column names */
        btbl_cols[0] = "BLOCK";
        btbl_cols[1] = "DM   ";
        btbl_cols[2] = "SIDE ";
        btbl_cols[3] = "EXPOSURE";
        btbl_cols[4] = "LIVE_TIME";
        btbl_cols[5] = "DEAD_TIME";
       /* Set data formats */
        tform[0] = "I    ";
	tform[1] = "I    ";
        tform[2] = "I    ";
        tform[3] = "1E   ";
        tform[4] = "1E   ";
        tform[5] = "1E   ";
       /* Set the units */	
        tunit[0] = " ";
        tunit[1] = " ";
        tunit[2] = " ";
        tunit[3] = "s";
        tunit[4] = "s";
        tunit[5] = "s";	
       /* Create binary table  */
       if (fits_create_tbl(outfptr, 2, 0, 6, btbl_cols, tform, 
			          tunit, "LIVETIME", &status)) {
            fprintf(stderr, "ERROR: Unable to create binary table\n");
            goto cleanup;
       }	    
       /* Get every 128th value of w_block, w_dm and w_side */
       for(k = 0; k < 256; k++) {
	 /* HAK 22-July-2003 Changed the way these are calculated to simplify
	    the code */
	 /* new_block[k] = f_block[k + 127*k];
           new_dm[k] = f_dm[k + 127*k];
           new_side[k] = f_side[k + 127*k]; */
	   new_block[k] = (int)(k/16);
           new_dm[k] = ((int)(k/2) % 8);
           new_side[k] = (k % 2);
           headas_chat(2,"k,new_block,new_dm,new_side values are %d,%d,%d,%d,%d,%d,%d\n",
                                  k, new_block[k], new_dm[k], new_side[k],(int)(k/16),((int)(k/2) % 8),(k % 2) );
           
       }	   
       /*Write column  values to output file */
       fits_write_col(outfptr, TINT, 1, 1, 1, 256, &new_block[0],
		                    &status);
       if (status != 0) {
	   fprintf(stderr, "ERROR: Unable to write col 1 of table\n");
	   goto cleanup;
       }	   
       fits_write_col(outfptr, TINT, 2, 1, 1, 256, &new_dm[0],
		                    &status);
       if (status != 0) {
	   fprintf(stderr, "ERROR: Unable to write col 2 of table\n");
	   goto cleanup;
       }	   
       fits_write_col(outfptr, TINT, 3, 1, 1, 256, &new_side[0],
		                    &status);
       if (status != 0) {
	   fprintf(stderr, "ERROR: Unable to write col 3 of table\n");
	   goto cleanup;
       }	   
       fits_write_col(outfptr, TFLOAT, 4, 1, 1, 256, &exposure_vals[0],
		                    &status);
       if (status != 0) {
	   fprintf(stderr, "ERROR: Unable to write col 4 of table\n");
	   goto cleanup;
       }	   
       fits_write_col(outfptr, TFLOAT, 5, 1, 1, 256, &livetime_vals[0],
		                    &status);
       if (status != 0) {
	   fprintf(stderr, "ERROR: Unable to write col 5 of table\n");
	   goto cleanup;
       }	   
       fits_write_col(outfptr, TFLOAT, 6, 1, 1, 256, &deadtime_vals[0],
		                    &status);
       if (status != 0) {
	   fprintf(stderr, "ERROR: Unable to write col 6 of table \n");
	   goto cleanup;
       } 	   

       /* Write HISTORY keywords to HDU */
       status = HDpar_stamp(outfptr, 0, &status);

  } /* successfully opened ascii file containing FITS file names */      
cleanup:
  if (outfptr) fits_close_file(outfptr, &status);
  /*if (infptr) fits_close_file(infptr, &status);*/
  if (winfptr) fits_close_file(winfptr, &status);
  if (maskfptr) fits_close_file(maskfptr, &status);
  
  free(w_low);
  free(w_high);

  
  return(status);
}      
