#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "bat_gswdev.h"

/*
HISTORY
-------
HAK 2-August-2003  Version 1.0  
Basic tool to read in a DPH, organize the table by detector ID and output
a table with one row per detector and columns giving detector ID, row and
column and a vector containing the spectrum for that detector.

*/

#define TOOLSUB batdph2pha
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Structure for passing all PIL parameters to the main routine. */
typedef struct {
   char infile[PIL_LINESIZE];
   char outfile[PIL_LINESIZE];
   int rows;
} BATDPH2PHA_PARAMS;   

/* Global array definitions */

/* Function prototypes */
int batdph2pha(void);
int batdph2pha_getpar (BATDPH2PHA_PARAMS *params);
int batdph2pha_printsummary (BATDPH2PHA_PARAMS *params,char *taskname,char *version);
int batdph2pha_work (float *,float **,int *,int *,long int *);

/*-------------------------------------------------------------*/
int batdph2pha(void)
{
   int status;
   int dphcol,timecol,expcol;
   long int i,rown;
   long int nrows=0; /* Number of rows in input file*/
   long int nchannels; /* Number of energy channels in input DPH*/
   double time,exposure;
   int min,max;
   
   static char taskname[80] = "batdph2pha";
   static char version[8] = "2.1";
   fitsfile *infptr = 0, *outfptr = 0;

   char *ttype[10], *tunit[10];
   char *tform[10];
   char tformn[3] = " ";
   char keyname[FLEN_CARD];

   float *dph;
   float **spectra;
   double *out_time;
   double *out_exposure;
   int *detx;
   int *dety;
   int *detid, *block, *dm, *side, *det;

   char comment[30] = "";
   char value[20]= "";

   BATDPH2PHA_PARAMS params;

   headas_chat(1,"*************************************************\n");
   headas_chat(1,"   WARNING: This task is not supported for scientific analysis.\n");
   headas_chat(1,"   Please use the 'batbinevt' task to convert a survey DPH to a spectrum.\n");
   headas_chat(1,"*************************************************\n");

   headas_chat(5,"In the beginning\n");

   /* Register taskname and version. */
   set_toolname(taskname);
   set_toolversion(version);

   headas_chat(5,"In the beginning\n");

   /*  get input parameters */
   status=batdph2pha_getpar(&params);

   if (!status) {

      /* Open the input DPH and output file */
      if (fits_open_file(&infptr,params.infile,READONLY,&status)) goto cleanup;        /* First figure out the number of energy channels from the EBOUNDS
	 extension. */
      if (fits_movnam_hdu(infptr,BINARY_TBL,"EBOUNDS",0,&status)) goto cleanup;
      if (fits_get_num_rows(infptr,&nchannels,&status)) goto cleanup;
      headas_chat(4,"Number of energy channels is %d\n",nchannels);

      /* Now malloc the array to hold the DPH and output spectra*/
      dph = (float *) malloc((unsigned) DAP_CELLS*nchannels*sizeof(float));
      spectra = (float **) malloc((unsigned) NUM_ELEMENTS*sizeof(float *));
      for (i=0;i<NUM_ELEMENTS;i++)
	 spectra[i] = (float *) malloc((unsigned) nchannels*sizeof(float));
 
      /* Move to the correct HDU in the input file. The file must contain
         a "BAT_DPH" extension.*/
      if (fits_movnam_hdu(infptr,BINARY_TBL,"BAT_DPH",0,&status)) goto cleanup;

      /* Get the number of rows in the input file */
      if (fits_get_num_rows(infptr,&nrows,&status)) goto cleanup;
      headas_chat(4,"Number of rows is %d\n",nrows);

      /* Get the column numbers for the input file.  The column numbers
         that are read are detector ID and PHA.  The PI column is written.*/
      if (fits_get_colnum(infptr,CASEINSEN,"DPH_COUNTS",&dphcol,&status)) goto cleanup;
      if (fits_get_colnum(infptr,CASEINSEN,"TIME",&timecol,&status)) goto cleanup;
      if (fits_get_colnum(infptr,CASEINSEN,"EXPOSURE",&expcol,&status)) goto cleanup;

      if (params.rows > nrows) {
	 fprintf(stderr,"Row number selected greater than highest row in input file.  Exiting.\n");
         goto cleanup;
      } else {
	 /* Read in the input DPH and other associated values.*/
	 fits_read_col(infptr,TFLOAT,dphcol,(long)params.rows,1,
		       DAP_CELLS*nchannels,0,dph,0,&status);   
	 fits_read_col(infptr,TDOUBLE,timecol,(long)params.rows,1,
		       1,0,&time,0,&status);   
	 fits_read_col(infptr,TDOUBLE,expcol,(long)params.rows,1,
		       1,0,&exposure,0,&status);   
         headas_chat(5,"Time and exposure are %f,%f\n",time,exposure);
         
      }

      detx = (int *) malloc((unsigned) NUM_ELEMENTS*sizeof(int));
      dety = (int *) malloc((unsigned) NUM_ELEMENTS*sizeof(int));

      /* Call the main work routine */
      status=batdph2pha_work(dph,spectra,dety,detx,&nchannels);

      /* Set up some of the output arrays */
      out_time = (double *) malloc((unsigned) NUM_ELEMENTS*sizeof(double));
      out_exposure = (double *) malloc((unsigned) NUM_ELEMENTS*sizeof(double));
      detid = (int *) malloc((unsigned) NUM_ELEMENTS*sizeof(int));
      block = (int *) malloc((unsigned) NUM_ELEMENTS*sizeof(int));
      dm = (int *) malloc((unsigned) NUM_ELEMENTS*sizeof(int));
      side = (int *) malloc((unsigned) NUM_ELEMENTS*sizeof(int));
      det = (int *) malloc((unsigned) NUM_ELEMENTS*sizeof(int));
      
      for (i=0;i<NUM_ELEMENTS;i++) {
	 out_time[i] = time;
         out_exposure[i] = exposure;
         detid[i] = (int)i;
         block[i] = (int)(i/2048);
         dm[i] = (int)((i-block[i]*2048)/256);
         side[i] = (int)((i-block[i]*2048-dm[i]*256)/128);
         det[i] = (int)(i-block[i]*2048-dm[i]*256-side[i]*128);
      }
   
      /* Delete existing file if clobber=YES */
      headas_clobberfile(params.outfile); 

      /* Set up the output file for writing */
      ttype[0] = "TIME";
      ttype[1] = "EXPOSURE";
      ttype[2] = "DET_ID";
      ttype[3] = "COUNT";
      ttype[4] = "BLOCK";
      ttype[5] = "DM";
      ttype[6] = "SIDE";
      ttype[7] = "DETECTOR";
      ttype[8] = "DETX";
      ttype[9] = "DETY";

      tform[0] = "1D";
      tform[1] = "1D";
      tform[2] = "1I";
      sprintf(tformn,"%dE",(int)nchannels);
      tform[3] = tformn;
      for (i=4;i<10;i++) {
	 tform[i] = "1I";
         tunit[i] = " ";
      }
  
      tunit[0] = "s";
      tunit[1] = "s";
      tunit[2] = " ";
      tunit[3] = "count";
  
      if (fits_create_file(&outfptr,params.outfile,&status)) goto cleanup;
      if (fits_create_tbl(outfptr,BINARY_TBL,NUM_ELEMENTS,10,
			  ttype,tform,tunit,"BAT_SPECTRUM",&status));

      /* Just for testing purposes */
      /* for (i=0;i<NUM_ELEMENTS;i++) {
	 for (j=0;j<nchannels;j++) spectra[i][j]=(float)j;
	 } */

      for (i=0;i<NUM_ELEMENTS;i++) 
	 fits_write_col(outfptr,TFLOAT,4,i+1,1,(long)nchannels, 
			spectra[i],&status); 

      fits_write_col(outfptr,TDOUBLE,1,1,1,NUM_ELEMENTS, 
			out_time,&status); 
      fits_write_col(outfptr,TDOUBLE,2,1,1,NUM_ELEMENTS, 
			out_exposure,&status); 
      fits_write_col(outfptr,TINT,3,1,1,NUM_ELEMENTS, 
			detid,&status); 
      fits_write_col(outfptr,TINT,5,1,1,NUM_ELEMENTS, 
			block,&status); 
      fits_write_col(outfptr,TINT,6,1,1,NUM_ELEMENTS, 
			dm,&status); 
      fits_write_col(outfptr,TINT,7,1,1,NUM_ELEMENTS, 
			side,&status); 
      fits_write_col(outfptr,TINT,8,1,1,NUM_ELEMENTS, 
			det,&status); 
      fits_write_col(outfptr,TINT,9,1,1,NUM_ELEMENTS, 
			detx,&status); 
      fits_write_col(outfptr,TINT,10,1,1,NUM_ELEMENTS, 
			dety,&status); 

      /* Now write out the comments for each column */
      for (i=1;i<11;i++) {
         sprintf(keyname,"TTYPE%d",(int)i);
         fits_read_key(outfptr,TSTRING,keyname,value,comment,&status);
         switch (i) {
	    case 1:  sprintf(comment,"Time associated with DPH"); break;
	    case 2:  sprintf(comment,"Exposure of the histogram"); break;
	    case 3:  sprintf(comment,"Detector ID"); break;
	    case 4:  sprintf(comment,"Spectrum of the detector"); break;
	    case 5:  sprintf(comment,"Block number"); break;
	    case 6:  sprintf(comment,"DM (detector module)  number"); break;
	    case 7:  sprintf(comment,"DM side number"); break;
	    case 8:  sprintf(comment,"Detector channel number"); break;
	    case 9:  sprintf(comment,"BAT X coordinate"); break;
	    case 10:  sprintf(comment,"BAT Y coordinate"); break;
         }
         fits_update_key(outfptr,TSTRING,keyname,value,comment,&status);
         if ((i == 3) || (i > 4)) { 
            switch (i) {
	       case 3: min=0; max=32767; break;
	       case 5: min=0; max=15; break;
	       case 6: min=0; max=7; break;
	       case 7: min=0; max=1; break;
	       case 8: min=0; max=127; break;
	       case 9: min=0; max=DAP_COLS-1; break;
	       case 10: min=0; max=DAP_ROWS-1; break;
            }
            sprintf(keyname,"TLMIN%d",(int)i);
            fits_write_key(outfptr,TINT,keyname,&min,
			   "Minimum legal value",&status);
            sprintf(keyname,"TLMAX%d",(int)i);
            fits_write_key(outfptr,TINT,keyname,&max,
			   "Maximum legal value",&status);
         } 
        
      }
       
      /* Write optional history keywords */
      /* Replaced headas_parstamp with HDpar_stamp  14-Nov-2003  */
      status=HDpar_stamp(outfptr,0,&status); 
      /* status=headas_parstamp(outfptr,0); */
      if (status) goto cleanup;

      /* Copy the EBOUNDS extension to the output file */
      if (fits_movnam_hdu(infptr,BINARY_TBL,"EBOUNDS",0,&status)) goto cleanup;
      if (fits_copy_hdu(infptr,outfptr,0,&status)) goto cleanup;

      if (status) fits_report_error(stderr,status);

      if (status) goto cleanup;

      /* Copy the GTI extension to the output file */
      if (fits_movnam_hdu(infptr,BINARY_TBL,"GTI",0,&status)) {
	 fits_movnam_hdu(infptr,BINARY_TBL,"STDGTI",0,&status); 
      }
      headas_chat(3,"Status is %d\n",status);
      if (status == 0) {
         if (fits_copy_hdu(infptr,outfptr,0,&status)) goto cleanup;
         if (fits_movnam_hdu(outfptr,BINARY_TBL,"GTI",0,&status)) {
	    fits_movnam_hdu(outfptr,BINARY_TBL,"STDGTI",0,&status); 
         }
         fits_get_num_rows(outfptr,&nrows,&status);
         headas_chat(3,"Number of rows in GTI is %d (%d)\n",nrows,status);
         rown=1;
         for (i=1;i<=nrows;i++) {
            headas_chat(3,"Row to delete in GTI is %d %d\n",i,rown);
            if (i == params.rows) rown = 2;
	    else fits_delete_rows(outfptr,rown,1,&status);
         }
      } else status=0;

      if (status) fits_report_error(stderr,status);

      if (status) goto cleanup;
   }
 
    /* Print out a summary of operations (unless chatter=0) */
    status=batdph2pha_printsummary(&params,taskname,version);
    if (status) goto cleanup;


    headas_chat(5,"Close to the end\n");
    if (fits_close_file(outfptr,&status)) goto cleanup;
    if (fits_close_file(infptr,&status)) goto cleanup;

    return(status);

cleanup:

    if (outfptr) fits_close_file(outfptr,  &status);
    if (infptr) fits_close_file(infptr,  &status);

    if (status) fits_report_error(stderr, status);
    return(status);
}

/*-------------------------------------------------------------*/
int batdph2pha_work(
   float *dph,
   float **spectra,
   int *detx,
   int *dety,
   long int *nchannels)

{
   int status=0;

   long int i;
   int *location;
   short int *block,*dm,*det;
   short int *row,*col;

   unsigned short int detid[NUM_ELEMENTS];
  
   headas_chat(5,"Here in the work routine\n");

   block = (short int*)(malloc(NUM_ELEMENTS*sizeof(short int)));
   dm = (short int*)(malloc(NUM_ELEMENTS*sizeof(short int)));
   det = (short int*)(malloc(NUM_ELEMENTS*sizeof(short int)));
   row = (short int*)(malloc(NUM_ELEMENTS*sizeof(short int)));
   col = (short int*)(malloc(NUM_ELEMENTS*sizeof(short int)));
   location = (int *)(malloc(NUM_ELEMENTS*sizeof(int)));

   /* Set up an array that gives the location within the BAT map of each
      detector */
   for (i=0;i<NUM_ELEMENTS;i++) detid[i]=i;
   batidconvert(NUM_ELEMENTS,detid,block,dm,det,row,col);
   for (i=0;i<NUM_ELEMENTS;i++) {
      location[i]=(col[i]+row[i]*DAP_COLS)*(*nchannels);
      detx[i] = (int)row[i];
      dety[i] = (int)col[i];
   }

   for (i=0;i<NUM_ELEMENTS;i++) spectra[i] = &dph[location[i]];

   free(block);
   free(dm);
   free(det);
   free(row);
   free(col);
   free(location);

   return(status);

}

/*-------------------------------------------------------------*/

/* Routine to make all calls to PIL to fill the structure containing
   all user supplied paramters. */

int batdph2pha_getpar (
    BATDPH2PHA_PARAMS *params)

{
 
   int status=0;

   headas_chat(5,"Got to the parameter subroutine\n");

   if ((status = PILGetFname("infile", params->infile)))
      fprintf(stderr, "Error reading the 'infile' parameter.\n");

   else if ((status = PILGetFname("outfile", params->outfile)))
      fprintf(stderr, "Error reading the 'outfile' parameter.\n");

   else if ((status = PILGetInt("row", &params->rows)))
      fprintf(stderr, "Error reading the 'rows' parameter.\n");

    headas_chat(4,"The input file is called %s\n",params->infile);
    headas_chat(4,"The output file is called %s\n",params->outfile);
    headas_chat(4,"The row number is %d\n",params->rows);

    return(status);

}


/*-------------------------------------------------------------*/

int batdph2pha_printsummary (
   BATDPH2PHA_PARAMS *params,
   char *taskname,
   char *version)

{ 
    headas_chat(1,"%s v%s completed\n",taskname,version);
    headas_chat(1,"Input DPH file is %s\n",params->infile);
    if (*params->outfile) headas_chat(1,"Output PHA file is %s\n",params->outfile);

    return(0);
}
