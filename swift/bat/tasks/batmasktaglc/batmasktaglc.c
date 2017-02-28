#include <stdio.h>
#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_gti.h"
#include "batmask.h"
#include "bat_gswdev.h"

/* Task name and version */
char taskname[] = "batmasktaglc";
char taskver[]  = "2.12";
	   
/*
  HISTORY
  -------
    Version 1.0 written by Susima Abeyagunawardene, SSAI, February 2003

    Version 2.0 by Hans Krimm, August 2003
       Extensive modifications to the structure of the tool
       Extensive changes to the way in which source, background and
          errors are calculated, based on a more correct analysis of
          how to derive these quantities from the mask weighted and
          quadrant rates.
   HAK 26-Aug-2003 Modified code to accept bat2fits3 format:  sums,
      sumsq, etc. in columns instead of keywords.
   CBM 29-Nov-2003 Change to new HDpar_stamp() function.
   HAK 5-Aug-2004 -- Added ability to do partial coding, cosine and
      flatfielding corrections.
      -- Modified code to allow multiple mask weighting maps
      -- Streamlined code to reduce number of FITS I/O calls
      -- Other minor improvements
   HAK 29-Jan-2005 -- Added calculation and writing out of GTI extension 
      -- Corrected code so that no rows are written to the RATE extension
         when either (a) there are counts in one of the input rate files,
         but not the other, or (b) when there is not a valid mask weight
         map covering the time period.  The times in the GTI extension
         indicate the valid times according to these criteria.
      -- Added check to make sure that the catalog number of the mask
         weight map matches that of the rate file.
   HAK 3-Feb-2005 
      -- Modified the way the GTI extension was written; now take advantage
         of the headas utilities to write GTIs.
      -- Added some more keywords indicating what corrections have been
         applied.
      -- Added the ndets correction.
      -- Fixed the code so that either type of detector mask (image or table)
         can be read into the code.
         REALIZED AFTERWARD that this was a mistake.  The code should only
	 read the version of the mask that comes from flight -- anything 
	 generated on the ground will mask out detectors that did contribute
	 to the light curves.
      -- Corrected one mistake in the way the error bars were being 
         calculated; now it looks like I am calculating errors the same
         way that they are being calculated in batbinevt
         NOTE:  There is still an outstanding problem in the calculations
         that is being investigated.  Until it is fixed, use the data with
         caution.
   HAK 8-Feb-2005
      -- Added further corrections to the way that the error bars are
         calculated.  Now divide the variance by the square of the sum
         of the weights (see extensive comments in the code).  This now
         produces correctly sized error bars.
      -- Added code to remove disabled detectors from the calculations of
         the rate and background (and errors).  This correction brings the
         source rate into close correspondence with that calculated using
         batmaskwtevt and batbinevt
   HAK 9-Feb-2005
      -- Added hooks so that in a future version of the code, the errors
         can be calculated on a quadrant-by-quadrant basis.  This code
         is not yet enabled.  The errors are still calculated on on the
         basis of counts in the whole array.
      -- Cleaned up the code for readability and smoothness in a number 
         of places.
      -- Added that the rate/bkg/error are divided by the number of enabled
         detectors if the corrections include "ndets."  Allow the user to
         disable this division using the "detdiv" parameter.
      -- Set the default corrections to be "pcode,flatfield,ndets" which is
         consistent with other tools.
   HAK 11-Feb-2005
      -- Fixed a bug that was causing a mis-match between the data and the 
         correction which is based on the corresponding row in the mask
	 weight map.  Also made sure that bogus data or corrections are not
         allowed to slip through.  Also added some comments in several places.
   HAK 17-Mar-2005 Added the "scale" parameter to allow the user to scale 
         the input raw mask-tagged counts according to a user-derived
         correction.
   HAK 1-Apr-2005  Added lines so that the detector enablement map can be
         named "Det_Enable_Map," which is the extension name in bat2fits6

   HAK 5-Jul-2005 Changed one line of code to fix a serious bug.  We now
         reset the counter of good detectors for each row in the detector
         weight mask file.

   HAK 16-Sep-2005  Modified the application of the "scale" parameter so
         it is really a scaling of the output light curve like the 
         corrections (pcode, flatfield, etc.). Also changed some print
         outputs so that chatter=3 is not so verbose.

   ----------------------------
   revision 1.34
   date: 2006/08/03 21:00:11;  author: craigm;  state: Exp;  lines: +6 -2
   Bump to version 2.6; fix bug in the writing of empty GTIs; unit test still passes --CM

   ----------------------------
   revision 1.35
   date: 2006/08/03 21:20:29;  author: craigm;  state: Exp;  lines: +2 -0
   Add some debugging code for the problem discovered --CM

   CBM 07 Aug 2006 - v2.7; avoid out-of-bounds memory access when dealing with
                     GTIs; debugging messages more descriptive

   CBM 08 Mar 2008 - v2.9; add descriptive column names for output files.

   CBM 05 Dec 2008 - v2.10; correct bug; crashes when trying to write EBOUNDS

   CBM 07 Jun 2009 - v2.11; code clean-up

   CBM 08 Jun 2009 - v2.12 now scale=INDEF is allowed, which means to
      use 1.3 before 2007-03-13, and 1.0 after that time, based on a
      flight software upgrad.e

*/

#define MAX_GTIROWS              1024
#define MAX_WEIGHT               14
#define SHIELDED_WEIGHTS         7

#define NUM_ROWS (DAP_ROWS)
#define NUM_COLS (DAP_COLS)
#define NUM_SPACES (DAP_CELLS)

#define TOOLSUB batmasktaglc
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Structure containing the mask weight parameters */
typedef struct {
  float masked[4];
  float shielded[4];
  float sum[4];
  float sumsq[4];
  float sumsqmk[4];
  float mask_weights[NUM_SPACES];
  double time;
  int num_rows;
  float theta;
  float phi;
} MASK_WEIGHT_STRUCT;

/* Structure for passing all PIL parameters to the main routine. */
typedef struct {
   char infile[PIL_PATH_MAX];
   char outfile[PIL_PATH_MAX];
   char quadfile[PIL_PATH_MAX];
   char maskwt[PIL_PATH_MAX];
   char detfile[PIL_PATH_MAX];
   char ebounds[PIL_PATH_MAX];
   char corrections[PIL_PATH_MAX];
   char detdiv[PIL_PATH_MAX];
   double scale;
} BATMASKTAGLC_PARAMS;   

/* Structure for passing the GTI times */
typedef struct {
   double start;
   double stop;
} GTI_STRUCT;

/* Global declarations */

   static float detmask[NUM_SPACES];         
   struct batmaskcorrections_struct corrs; 

/* Function Prototypes */
int batmasktaglc(void);
int batmasktaglc_getpar (BATMASKTAGLC_PARAMS *params);
long batmasktaglc_getrows(char *maskwt, char *infile);
int batmasktaglc_readarr(char *maskwt, char *detfile, char *infile,
                      float [], MASK_WEIGHT_STRUCT *);
int batmasktaglc_work(char *infile1, char *quadfile, 
	   float *detmask, char *outfile, MASK_WEIGHT_STRUCT *,
           GTI_STRUCT *,int detdiv, double scale);

int write_ebounds(char *ebounds,char *outfile,char *infile);
int write_gti(char *outfile, GTI_STRUCT *);

/*-------------------------------------------------------------*/
int batmasktaglc(void)
{
/* This is a tool to derive light curves from the mask tagged rate
   packets produced by the flight software */
	
   BATMASKTAGLC_PARAMS params;

   char infile1[PIL_PATH_MAX];      /* MASK_TAG_RATES extension of 
			              the input file */	      
   int status = 0; /* Status returned from calls to subroutines */

   MASK_WEIGHT_STRUCT *mkwtmap;
   GTI_STRUCT *gti;

   int m;
   long num_rows;
   int detdiv;

   /* Register taskname and version. */
   set_toolname(taskname);
   set_toolversion(taskver);

   /*  get input parameters */

   status = batmasktaglc_getpar(&params);
   if (status) {
      fprintf(stderr, "Error reading parameters from .par file \n");
      goto cleanup;   
   }

   num_rows=batmasktaglc_getrows(params.maskwt,params.infile);
   
   /* Malloc the maskwt structure */
   mkwtmap = (MASK_WEIGHT_STRUCT *) malloc((unsigned) num_rows*sizeof(MASK_WEIGHT_STRUCT));

   /* Malloc the GTI structure */
  gti = (GTI_STRUCT *) malloc((unsigned) MAX_GTIROWS*sizeof(GTI_STRUCT));
  for (m=0;m<MAX_GTIROWS;m++) {
     gti[m].start=0.0;
     gti[m].stop=0.0;
  }

  /* Get infile1 name */
  sprintf(infile1, "%s[MASK_TAG_RATES]", params.infile);

   /* Call batmasktaglc to open infile, quadfile, outfile and 
      and do the required processing */
   status = batmasktaglc_readarr(params.maskwt,params.detfile,
				 params.infile,detmask,mkwtmap);
   if (status) {
      if (status > 1) fprintf(stderr, "Error reading from the maskwt and/or detmask files \n");
      goto cleanup;   
   }

    if (strcasecmp(params.detdiv,"NO") == 0) {
      detdiv = 0;
    } else {
      detdiv = 1;
    }
   status = batmasktaglc_work(infile1,params.quadfile,
			      detmask,params.outfile, mkwtmap,gti,detdiv,params.scale);
   if (status) {
      fprintf(stderr, "Error in the main batmasktaglc work file. \n");
      goto cleanup;   
   }

   status = write_ebounds(params.ebounds,params.outfile,params.infile);
   if (status) {
      fprintf(stderr, "Error writing the ebounds extension. \n");
      goto cleanup;   
   }

   status = write_gti(params.outfile,gti);
   if (status) {
      fprintf(stderr, "Error writing the GTI extension. \n");
      goto cleanup;   
   }

   /* HAK 22-May-2003 Added this return statement. */
   return(status);

cleanup:
   fprintf(stderr,"Exiting batmasktaglc with an error status %d. \n",status);
   return(status);

} /* End of batmasktaglc */

/* --------------------------------------------------------------- */

int batmasktaglc_getpar(
    BATMASKTAGLC_PARAMS *params)

{
   int status = 0;

   fitsfile *infptr=0;

   headas_chat(5, "...Entering batmasktaglc_getpar...\n");
   /* Read the input parameters for batmasktaglc task from the
      .par file */
   if ((status = PILGetFname("infile", params->infile)))
      fprintf(stderr, "Error reading the 'infile' parameter.\n");
   else if ((status = PILGetFname("quadfile", params->quadfile))) 
	  fprintf(stderr, "Error reading the 'quadfile' parameter.\n"); 
   else if ((status = PILGetString("maskwt", params->maskwt)))
      fprintf(stderr, "status is %d reading %s parameter.\n",
		      status, params->maskwt);
   else if ((status = PILGetFname("outfile", params->outfile)))
      fprintf(stderr, "status is %d reading %s parameter.\n",
		      status, params->outfile );	   
   else if ((status = PILGetFname("detmask", params->detfile)))
      fprintf(stderr, "status is %d reading %s parameter.\n",
		      status, params->detfile );	   
   else if ((status = PILGetString("ebounds", params->ebounds)))
      fprintf(stderr, "Error reading ebounds value from .par file \n");

   else if ((status = PILGetString("corrections", params->corrections)))
      fprintf(stderr, "Error reading the 'corrections' parameter.\n");

   else if ((status = PILGetString("detdiv", params->detdiv)))
      fprintf(stderr, "Error reading the 'detdiv' parameter.\n");

   else if ((status = PILGetReal("scale", &params->scale))) {
     /* Handle the case of INDEF for scale factor */
     if (status == PIL_VALUE_UNDEFINED) {
       status = 0;
       params->scale = -1;
     } else {
       fprintf(stderr, "Error reading the 'scale' parameter.\n");
     }
   }

   /* If the ebounds filename parameter points to caldb, then retrieve
         the ebounds from caldb. */
   if ( strcasecmp(params->ebounds,"CALDB") == 0) {
     struct caldbparms_struct caldb;
     /* CALDB filtering expressions */
     char *expr = "MODE.eq.4";
     char *codenam = "EBOUNDS";
     char *pfile = params->ebounds;

     char online[80], *ponline = online;
     long int extno[1];
     int maxret = 1;
     int nret = 0, nfound = 0;

     /* If we are going to access caldb, then we need to get the
	date and time of the observation from the input events
	file first. */
     
     status = 0;
     fits_open_file(&infptr,params->infile,READONLY,&status);
     if (status == 0) fits_movnam_hdu(infptr,BINARY_TBL,"MASK_TAG_RATES",0,&status);
     if (status == 0) batkw_to_caldb_parms(infptr, &caldb, 1, &status);
     fits_close_file(infptr, &status);
     if (status) {
       fprintf(stderr, "ERROR: could not open '%s' for CALDB information\n",
	       params->infile);
       return status;
     }

     bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
		      &pfile, extno, &ponline, &nret, &nfound, &status);
     if ((status != 0) || (nret == 0) || (nfound == 0)) {
       fprintf(stderr, "ERROR: could not locate the BAT teldef file in CALDB\n");
       return status;
     }
   }

   /* Set or zero out all the correction terms */
   memset((void *)&corrs, 0, sizeof(corrs));  /* <--- Zero the structure */

    if (status == 0) {
       if (strstr(params->corrections, "cosine")) {
         headas_chat(5," ...enabling the cosine correction...\n"); 
         corrs.ccosine = 1;
       }
       if (strstr(params->corrections, "pcode")) {
          headas_chat(5," ...enabling the partial coding correction...\n"); 
          corrs.cpcode = 1;
       }
       if (strstr(params->corrections, "flatfield")) {
          headas_chat(5, " ...enabling the flatfield correction...\n"); 
          corrs.cflatfield = 1;
       }
       if (strstr(params->corrections, "ndets")) {
          headas_chat(5," ...enabling the number of detectors correction...\n"); 
          corrs.cnbatdets = 1;
       }
       if (strstr(params->corrections, "default")) {
          headas_chat(5," ...enabling the default correction...\n"); 
          headas_chat(5,"     (pcode,flatfield,ndets)\n"); 
          corrs.cpcode = 1;
          corrs.cnbatdets = 1;
          corrs.cflatfield = 1;
       }
    }

   /* Sanity check the corrections */
   if (corrs.ccosine && corrs.cflatfield) {
      fprintf(stderr, "ERROR: you can't specify both 'cosine' and 'flatfield' corrections\n");
      return -1;
  }

  return (status);
}  

/* **************************************************************** */

long batmasktaglc_getrows(
		char *maskwt,      /* I - masktagimg file without 
				          extension name */
		char *infile)      /* I - Input mask tagged rates file */

{

  int status = 0;
  long rows=0;
  fitsfile *maskfptr = 0;
  char maskwtext[PIL_PATH_MAX];    /* MASK_TAG_WEIGHT extension of the
				      maskwt file */

  /* Here if the maskwt parameter is file look for a MASK_TAG_WEIGHT HDU 
     in the input file */
  if (strcasecmp(maskwt,"file") == 0 || strcasecmp(maskwt,"infile") == 0)  {
    strcpy(maskwt,infile);
  }

  sprintf(maskwtext, "%s[MASK_TAG_WEIGHT]", maskwt);
  headas_chat(5," ...opening %s...\n",maskwtext);
  fits_open_file(&maskfptr, maskwtext, READONLY, &status);
  /* If unable to open as a table, then try to open as an image */
  if (status) {
     status=0;
     fits_open_image(&maskfptr,maskwt, READONLY, &status);
     if (status) {
         fprintf(stderr, "ERROR: could not open %s\n",maskwt);
         return status;
     } else {
         status=0;
         rows=1;
     }
  } else {
     fits_get_num_rows(maskfptr, &rows,&status);
     if (status) {
       fprintf(stderr,"ERROR: reading number of rows\n");
       goto cleanup; 
     }
  }
  headas_chat(5," ...number of mask weight maps is %d...\n", rows);    

  fits_close_file(maskfptr,&status);

  return(rows);

cleanup:
   if (maskfptr) fits_close_file(maskfptr, &status);
   if (status == 0) status--;
   return(status);

}

/* **************************************************************** */

int batmasktaglc_readarr(
		char *maskwt,      /* I - masktagimg file without 
				          extension name */
		char *detfile,     /* I - Input detector mask file */
		char *infile,      /* I - Input mask tagged rates file */
		float detmask[NUM_SPACES],
		MASK_WEIGHT_STRUCT *mkwtmap)

{
  /* Define the local variables */
  int col_number;                  /* Number of the MASK_WEIGHT column */
  long num_rows;              /* Number of rows in the MASK_WEIGHT extension*/

  char kw_string[20];              /* Key word to be searched in HDU */ 
  char infileext[PIL_PATH_MAX]; 

  fitsfile *maskfptr = 0;
  fitsfile *detfptr =0;
  int status = 0;
  int hdu_type;
  int i, j;   
  int row;
  int goodval;
  double time=0.0;
  long obs_idrt,obs_idmw;

  long fpixel[2] = {1,1};
  float sum4[4],sumsq4[4],sumsqmk4[4],masked4[4],shielded4[4];
  float xobj,yobj,zobj;


  /* Here if the maskwt parameter is file look for a MASK_TAG_WEIGHT HDU 
     in the input file */
  if (strcasecmp(maskwt,"FILE") == 0 || strcasecmp(maskwt,"INFILE") == 0) {
    strcpy(maskwt,infile);
  }

  /* First open the input file and check the observation ID */
  sprintf(infileext, "%s[MASK_TAG_RATES]", infile);
  fits_open_file(&maskfptr, infileext, READONLY, &status);
  if (status) {
    fprintf(stderr,"ERROR: unable to open %s \n", infile);
    goto cleanup;
  }    
  fits_read_key(maskfptr, TLONG, "CATNUM", &obs_idrt, NULL, &status);
  if (status) {
     status=0;
     /* To be compatible with older versions of bat2fits */
     fits_read_key(maskfptr, TLONG, "SOURCEID", &obs_idrt, NULL, &status);
  }
  fits_close_file(maskfptr,&status);
  headas_chat(5," ...light curve CATNUM=%d...\n", obs_idrt);

  status=0;
  /* Open the masktagimg file and read the primary HDU */
  fits_open_data(&maskfptr, maskwt, READONLY, &status);
  if (status) {
    fprintf(stderr,"ERROR: unable to open mask weight file '%s'.\n", maskwt);
    goto cleanup;
  }    
  headas_chat(5," ...opened mask weight file '%s'\n", maskwt);

  fits_get_hdu_type(maskfptr, &hdu_type, &status);

  if (hdu_type == IMAGE_HDU) {
    /* ===== This is an image extension ===== */
     headas_chat(0,"You are opening a mask weighted map which was not produced by the flight code.\n");
     headas_chat(0,"Proceed with caution\n");

     row = 0; /* Pretend we are reading first row of a table */
     mkwtmap[row].num_rows = 1;

     for (i=0;i<4;i++) {
         sum4[i]=0.0;
	 sumsq4[i]=0.0;
	 sumsqmk4[i]=0.0;
     }	     

     headas_chat(5, " ...reading mask weights from primary array...\n");
     fits_read_pix(maskfptr, TFLOAT, fpixel, NUM_SPACES, 0,
		   &(mkwtmap[row].mask_weights[0]), 0, &status); 
     if (status) {
       fprintf(stderr, "ERROR: unable to read the mask weight image from '%s'.\n",
	       maskwt);
       goto cleanup;
     }

     /* Read target position and time */
     fits_read_key(maskfptr, TFLOAT,"BAT_XOBJ", &xobj, NULL,&status);
     fits_read_key(maskfptr, TFLOAT,"BAT_YOBJ", &yobj, NULL,&status);
     fits_read_key(maskfptr, TFLOAT,"BAT_ZOBJ", &zobj, NULL,&status);
     fits_read_key(maskfptr, TDOUBLE,"TSTART", &time, NULL,&status);
     mkwtmap[row].theta = atan(sqrt((xobj*xobj)+(yobj*yobj))/(zobj*zobj)); 
     mkwtmap[row].phi = atan2(yobj,xobj);
     mkwtmap[row].time = time;
     if (status) {
       fprintf(stderr, "ERROR: could not read BAT_{X,Y,Z}OBJ or TSTART keywords from '%s'.\n",
	       maskwt);
       goto cleanup;
     }

     /* Re-scale the ground-generated map to be similar to the scaled flight map */
     for (i=0;i<NUM_SPACES;i++) {
       float w = mkwtmap[row].mask_weights[i];

        /* simple */
        /* mkwtmap[row].mask_weights[i] = (float)((int)(mask_weight_vals[i]*7.0)+7.0); */
	/* more complicated, but more correct */     

       w = (float)((int)(0.5*(15.0*w+14.0)));
       if (w < 0.0) w = 0.0;
       mkwtmap[row].mask_weights[i] = w;

       sum4[0]     += w;
       sumsq4[0]   += w*w;
       sumsqmk4[0] += w*w;
       
     }

     /* Since we are adding the quadrants at this point anyway, just 
      * do it all in one quadrant */
     for (i=0;i<4;i++) mkwtmap[row].sum[i]     = sum4[i];
     for (i=0;i<4;i++) mkwtmap[row].sumsq[i]   = sumsq4[i];
     for (i=0;i<4;i++) mkwtmap[row].sumsqmk[i] = sumsqmk4[i];
     for (i=0;i<4;i++) masked4[i] = 8192;
     for (i=0;i<4;i++) mkwtmap[row].masked[i] = masked4[i];
     for (i=0;i<4;i++) shielded4[i] = 0;
     for (i=0;i<4;i++) mkwtmap[row].shielded[i] = shielded4[i];

  } else {
    /* ===== This is a table extension ===== */
    
    char extname[FLEN_CARD] = "";
    int sumcol, sumsqcol, sumsqmkcol, maskcol, shieldcol, thetacol, phicol, timecol;

    /* This was not an image HDU, so make sure it is a table HDU of the right type */
    fits_read_key(maskfptr, TSTRING, "EXTNAME", extname, NULL, &status);
    headas_chat(5, " ...EXTNAME='%s'...\n", extname);
    if (status || strcasecmp(extname,"MASK_TAG_WEIGHT") != 0) {
      status = 0;
      headas_chat(5, " ...moving to MASK_TAG_WEIGHT extension...\n", extname);
      fits_movnam_hdu(maskfptr, BINARY_TBL, "MASK_TAG_WEIGHT", 0, &status);
      if (status) {
	fprintf(stderr, "ERROR: could not find the MASK_TAG_WEIGHT extension in '%s'\n",
		maskwt);
	goto cleanup;
      }
    }
      
    fits_read_key(maskfptr, TLONG, "CATNUM", &obs_idmw, NULL, &status);
    headas_chat(5," ...mask weight CATNUM=%d...\n",obs_idmw);
    if (obs_idrt != obs_idmw) {
      fprintf(stderr, "ERROR: Mask weight map does not match mask weight rates file.\n");
      fprintf(stderr,"   Rate file CATNUM  is %d\n",(int)obs_idrt);
      fprintf(stderr,"   Mask weight file CATNUM is %d\n",(int)obs_idmw);
      status = -1;
      goto cleanup;
    }

    fits_get_num_rows(maskfptr, &num_rows, &status);
    if (status) {
      fprintf(stderr,"ERROR: reading MASK_WEIGHT number of rows\n");
      goto cleanup;
    }
    fits_get_colnum(maskfptr, CASESEN, "MASK_WEIGHT", &col_number,
		    &status);
    if (status) {
      fprintf(stderr,"ERROR: could not find MASK_WEIGHT column\n");
      goto cleanup;
    }
    headas_chat(5, " ...found %d rows; MASK_WEIGHT column=%d...\n",
		num_rows, col_number);
    
    for (row=0; row<num_rows; row++) mkwtmap[row].num_rows = num_rows;
    
    /* Get the values in MASK_WEIGHT column */
    /* Read each of the rows in the MASK_WEIGHT table */
    for (row=0;row<num_rows;row++) {
      
      fits_read_col(maskfptr, TFLOAT, col_number, row+1, 1, NUM_SPACES,
		    NULL, &(mkwtmap[row].mask_weights[0]), NULL, &status);
      
      if (status) {
	fprintf(stderr,"ERROR: unable to read MASK_WEIGHT column for row %d\n",row);
	goto cleanup;
      }
      
    }
    
    /* Now read the columns that give the sums, sums of squares, etc.
       for the four quadrants */
    /* The code must support both bat2fits2 in which these are keywords
       and bat2fits3 in which they are in columns.  The default will be
       the new format */
    
    /* First check to see if the columns exist */
    fits_get_colnum(maskfptr, CASESEN, "SUM",   &sumcol,   &status);
    fits_get_colnum(maskfptr, CASESEN, "SUMSQ", &sumsqcol, &status);
    fits_get_colnum(maskfptr, CASESEN, "SUMSQMK", &sumsqmkcol,&status);
    fits_get_colnum(maskfptr, CASESEN, "MASK", &maskcol,   &status);
    fits_get_colnum(maskfptr, CASESEN, "SHIELD", &shieldcol,&status);
    fits_get_colnum(maskfptr, CASESEN, "THETA", &thetacol, &status);
    fits_get_colnum(maskfptr, CASESEN, "PHI", &phicol,     &status);
    fits_get_colnum(maskfptr, CASESEN, "TIME", &timecol,   &status); 
    
    if (status == 0) {
      /* Standard columns found! */
      headas_chat(5, " ...found standard format MASK_WEIGHT columns...\n");
      
      /* Loop through the rows of the MASK_WEIGHT extension */
      for (row=0; row<num_rows; row++) {
	
	fits_read_col(maskfptr, TFLOAT, sumcol, row+1, 1, 4,
		      NULL, &(mkwtmap[row].sum[0]), NULL, &status);
	
	fits_read_col(maskfptr, TFLOAT, sumsqcol, row+1, 1, 4,
		      NULL, &(mkwtmap[row].sumsq[0]), NULL, &status);
	
	fits_read_col(maskfptr, TFLOAT, sumsqmkcol, row+1, 1, 4,
		      NULL, &(mkwtmap[row].sumsqmk[0]), NULL, &status);
	
	fits_read_col(maskfptr, TFLOAT, maskcol, row+1, 1, 4,
		      NULL, &(mkwtmap[row].masked[0]), NULL, &status);
	
	fits_read_col(maskfptr, TFLOAT, shieldcol, row+1, 1, 4,
		      NULL, &(mkwtmap[row].shielded[0]), NULL, &status);
	
	fits_read_col(maskfptr, TFLOAT, thetacol, row+1, 1, 1,
		      NULL, &(mkwtmap[row].theta), NULL, &status);
	
	fits_read_col(maskfptr, TFLOAT, phicol, row+1, 1, 1,
		      NULL, &(mkwtmap[row].phi), NULL, &status);
	
	fits_read_col(maskfptr, TDOUBLE, timecol, row+1, 1, 1,
		      NULL, &(mkwtmap[row].time), NULL, &status); 
	
	if (status) {
	  fprintf(stderr, "ERROR: could not read row %d from mask weight file '%s'\n",
		  row+1, maskwt);
	  goto cleanup;
	}
      }
      
    } else {

      /* Standard columns not found... */
      /* This branch is archaic and based on bat2fits2.  It is kept in 
	 the code for back compatibility, but should raise an alert for
	 the user */     
      status=0;
      headas_chat(0,"Couldn't find column -- looking for keywords\n");
      headas_chat(0,"WARNING -- This indicates old (unsupported) FITS format\n");
      
      for (i=0;i<4;i++) {
	
	sprintf(kw_string, "SUM%d", i);
	fits_read_key(maskfptr, TFLOAT, kw_string, &(mkwtmap[0].sum[i]), NULL, &status);
	
	sprintf(kw_string, "SUMSQ%d", i);
	fits_read_key(maskfptr, TFLOAT, kw_string, &(mkwtmap[0].sumsq[i]), NULL,&status);
	
	sprintf(kw_string, "SUMSQMK%d", i);
	fits_read_key(maskfptr, TFLOAT, kw_string, &(mkwtmap[0].sumsqmk[i]), NULL, &status);
	
	sprintf(kw_string, "MASK%d", i);
	fits_read_key(maskfptr, TFLOAT, kw_string, &(mkwtmap[0].masked[i]), NULL,&status);
		      
	sprintf(kw_string, "SHIELD%d", i);
	fits_read_key(maskfptr, TFLOAT, kw_string, &(mkwtmap[0].shielded[i]), NULL,&status);
      }
      if (status) {
	fprintf(stderr, "ERROR: could not read MASK_WEIGHT keywords from '%s'.\n",
		maskwt);
	goto cleanup;
      }

    }


      /* End of processing binary table branch */
  }
  fits_close_file(maskfptr, &status);
  maskfptr = 0;

  /* If no detector mask file is specified, fill 
     a default array with all zeros (all good) */
  for (i=0;i<NUM_SPACES;i++) detmask[i]=0.0;

  /* Read detector quality map file */
  if (strcasecmp(detfile, "NONE") != 0) {
    int safestatus = 0;
    headas_chat(5, "...reading detector mask image...\n");
    
    if (fits_open_file(&detfptr, detfile, READONLY, &status)) {
      fprintf(stderr, "ERROR: could not open '%s'\n", detfile);
      goto cleanup;
    }

    /* Move to the correct HDU in the input file. The file must contain
       a "BAT_FLAGS" or "Det_Enable_Map" extension.*/
    fits_movnam_hdu(detfptr,BINARY_TBL,"BAT_FLAGS",0,&status) ;
    if (status) {
      status=0;
      fits_movnam_hdu(detfptr,BINARY_TBL,"Det_Enable_Map",0,&status) ;
    }
    if (status) {
      fprintf(stderr, "ERROR: could not find Det_Enable_Map extension in '%s'.\n",
	      detfile);
      goto cleanup;
    }
    
    /* Read GOODVAL keyword.  If the keyword is not found, then assume
       that 0=good, 1=bad.  If the keyword is found, then use that
       value for good, and anything else for bad. */
    fits_read_key(detfptr, TINT, "GOODVAL", &goodval, 0, &status);
    headas_chat(5, " ...GOODVAL=%d...\n", goodval);
    if (status) goodval = 0;  /* Default = 0 */
    status = 0;
    fits_get_colnum(detfptr, CASEINSEN, "FLAG", &col_number, &status);
    fits_read_col(detfptr, TFLOAT, col_number, 1, 1,
		  NUM_ROWS*NUM_COLS, 0, detmask, 0, &status);   
    fits_close_file(detfptr, &safestatus);
    detfptr = 0;

     if (status) {
       fprintf(stderr, "ERROR: could not read detector quality map data from\n");
       fprintf(stderr, "    %s\n", detfile);
       fprintf(stderr, "NOTE: One must use the detector flag table produced in flight\n");
       fprintf(stderr, "  It is NOT appropriate to use a detector mask produced by bathotpix\n");
       fprintf(stderr, "  because the hot pixels DO contribute to the mask tagged light curve\n");
     } else if (goodval) {
 
       /* If goodval is 1 (old format), then switch everything over */
       for (j=0;j<NUM_SPACES;j++) {
	 if (detmask[j]) { 
	   detmask[j]=0;
	 } else {
	   detmask[j]=1;
	 }
       }
       
     }
  }
  
  return(status);

cleanup:
  { 
    int safestatus = 0;
    if (maskfptr) fits_close_file(maskfptr, &safestatus);
    safestatus = 0;
    if (detfptr) fits_close_file(detfptr, &safestatus);
  }
  return(status);

}  


/*
 * Routine to estimate fudge factor based on MET of when the data was taken
 *
 */
double batmasktaglc_scale_factor(double met)
{
  /* Before 2007-03-13, there was an error in the flight software
     ray-tracing algorithm, which caused the masktagged counts to be
     attenuated from their peak value. Based on analysis by H. Krimm,
     a scale factor was found that increases the flux to the proper
     level (and also the noise unfortunately).
  */
  if (met < 195436800) { return (1.3); }

  /* For later dates, the fluxes are more or less correct. */
  /*   H Krimm: "0.9898 rounded up to 1.0" */
  return (1.0);
}


/*
 * Utility routine to compute "location" and "quadid"
 */
int batmasktaglc_detid(int location[NUM_ELEMENTS], int quadid[NUM_SPACES])
{
  unsigned short int detid[NUM_ELEMENTS];
  short int block[NUM_ELEMENTS], dm[NUM_ELEMENTS], det[NUM_ELEMENTS], 
    rrow[NUM_ELEMENTS], col[NUM_ELEMENTS];
  int i;

  /* Set up an array that gives the location within the BAT map of each
     detector */
  for (i=0;i<NUM_ELEMENTS;i++) detid[i]=i;
  batidconvert(NUM_ELEMENTS,detid,block,dm,det,rrow,col);

  for (i=0;i<NUM_ELEMENTS;i++) location[i]=col[i]+rrow[i]*NUM_COLS;
  for (i=0;i<NUM_SPACES;i++) quadid[i]=-1;
  for (i=0;i<NUM_ELEMENTS/4;i++) quadid[location[i]]=0;
  for (i=NUM_ELEMENTS/4;i<NUM_ELEMENTS/2;i++) quadid[location[i]]=1;
  for (i=NUM_ELEMENTS/2;i<3*NUM_ELEMENTS/4;i++) quadid[location[i]]=2;
  for (i=3*NUM_ELEMENTS/4;i<NUM_ELEMENTS;i++) quadid[location[i]]=3;

  return 0;
}


/* **************************************************************** */
#define FUDGE_14  0.77
#define FUDGE_0   -0.77

int batmasktaglc_work(
	char *infile1,         /*I - MASK_TAG_RATES extension of 
			                 input file */
	/* fitsfile *infptr2, */        /*I - Place from which the energy
				         level values are obtained */
        char *quadfile,        /*I - B1600MS_RATES extension of 
			                 quad file */
        float detmask[NUM_SPACES],        /*I - Detector enablement map */
	char *outfile,             /*I - Name of output file to be
				         created */
	MASK_WEIGHT_STRUCT *mkwtmap,
        GTI_STRUCT *gti,
        int detdiv,
        double scale)

{
  int status = 0;	
  float background[4];             /* Background counts per row */
  float correct_cts[4];            /* Fully corrected count value per
				      energy level for current row */
  float elvl_cts[4];                /* Four energy level values
				      from counts column in 
				      infile */ 
  float *err3_lvl0 = 0;           /* Corrected error for energy level 0
				      per row */
  float *err3_lvl1 = 0;           /* Same as above for energy level 1 */
  float *err3_lvl2 = 0;           /* Same as above for energy level 2 */
  float *err3_lvl3 = 0;           /* Same as above for energy level 3 */
  float quad0_cts[4];              /* quad0 counts from the 4 energy 
			            levels in a row of B1600MS_RATES
				    ext. binary table*/
  float quad1_cts[4];              /* quad1 counts from the 4 energy
				    levels */
  float quad2_cts[4];              /* quad2 counts from the 4 energy
				    levels */ 
  float quad3_cts[4];              /* quad3 counts from the 4 energy
				    levels */
  float rate_errors[4];         /* Rate errors for the 4 energy levels */
  int row_count = 0;             /* Count of rows that have equal
				    TIME values is in the infile
				    and the quad file */
  float *row_l0_adj_incts = 0;  /* Energy level 0 adjusted infile
				    counts for each relevant row in
				    binary table */  
  float *row_l1_adj_incts = 0;  /* Same as above for energy lvl 1 */  
  float *row_l2_adj_incts = 0;  /* Same as above for energy lvl 2 */
  float *row_l3_adj_incts = 0;  /* Same as above for energy lvl 3 */
  float *row_l0_bkg = 0;        /* Energy level 0 background */
  float *row_l1_bkg = 0;        /* Same as above for energy lvl 1 */  
  float *row_l2_bkg = 0;        /* Same as above for energy lvl 2 */
  float *row_l3_bkg = 0;        /* Same as above for energy lvl 3 */
  float sum_quad_cts[4];           /* Sum of quad0, quad1, quad2
		      	            quad3 values for each energy
				    level */   
  double time_infile;             /* TIME from current row of
				     MASK_TAG_RATES extension  
				     binary table */
  double last_time;
  double *time_infile_rows = 0;   /* TIME saved from each of the
				     rows in MASK_TAG_RATES extension
				     of binary table */
  double time_quadfile;           /* TIME from current row of 
				     B1600MS_RATES extension
				     binary table */ 
  double *time_quadfile_rows = 0; /* TIME saved from each of the
				     rows in B1600MS_RATES extension
				     binary table */
  double *time_outfile_rows = 0; /* TIME in output file */
  long *row_infile = 0;   /* Row counter for MASK_TAG_RATES */
  long *row_quadfile = 0; /* Row counter for quad RATE */
  long *row_mkwtmap = 0;  /* Row counter for mask weight map */
  long *outfile_row_mkwtmap = 0;  /* Another row counter for mask weight map */

  int tfields_rate = 4;           /* Number of fields in HDU RATE
				     binary table */
  char *ttype_rate[4];            /* Names of the columns in the
				     binary table in HDU RATE */
  char *tform_rate[4];            /* Format of each column */
  char *tunit_rate[4];            /* Physical unit of table column */
  char *comments[4];              /* Descriptive column comments */
  
  int quadfile_time_col;          /* Number of TIME col in bin table
                                  of B1600MS_RATES extension of quad file */
  long num_infile_rows;	   /* Number of rows in bin table
				         of MASK_TAG_RATES extension */
  long num_quadfile_rows;    /* Number of rows in bin table
				         of B1600MS_RATES extension */
  int infile_time_col;       /* Number of TIME col in bin table
                                         of MASK_TAG_RATES extension of
                                         input file */
  int infile_count_col;      /* Number of COUNT col in bin table
                                         of MASK_TAG_RATES extension of
                                         input file */    
  int quadfile_quad0_col;    /* Number of quad0 count in bin
                                         table of B1600MS_RATES
                                         extension of quad file */
  int quadfile_quad1_col;    /* Number of quad1 count in bin
                                         table of B1600MS_RATES
                                         extension of quad file */
  int quadfile_quad2_col;    /* Number of quad2 count in bin
                                         table of B1600MS_RATES
                                         extension of quad file */
  int quadfile_quad3_col;    /* Number of quad3 count in bin
                                         table of B1600MS_RATES
                                         extension of quad file */
  int i, j, k=0, m ;
  int done=0;
  long ii, jj,kk;  
  fitsfile *infptr1 = 0, *quadfptr = 0;
  int infile_count_1_col,infile_count_2_col;
  char quadfile1[PIL_PATH_MAX];
  fitsfile *outfptr = 0;         /* - Output file pointer */

  double w[4], w2[4] , ns[4], n14[4], n0[4];
  double e[4], we[4], detm[4], ngood[4];
  double wtot=0.0, w2tot=0.0 , nstot=0.0, n14tot=0.0;
  double n0tot = 0.0, etot=0.0, wetot=0.0, detmtot=0.0;
  double wm = (double)MAX_WEIGHT;
  double f0 = (double)FUDGE_0;
  double f14 = (double)FUDGE_14;
  
  /* Variables for correcting counts based on mask weights */
  double *a = 0, *b = 0, *c = 0, *d = 0; 
  double *u1 = 0, *u2 = 0, *u3 = 0;
  double *atot = 0, *btot = 0, *ctot = 0, *dtot = 0;
  double *u1tot = 0, *u2tot = 0, *u3tot = 0;
  double *cos_edge = 0, *pcode_corr = 0;
  double correction_applied;

  float bkg[4],src[4],var[4];
  float bquad[4],squad[4];
  double u = 0.0;
 
  int row,max_row,gti_row;
  double x,y,z,r;
  int nbaddets=0,ngooddets=NUM_ELEMENTS;
  float timedel,quad_timedel;

  /* Arrays for figuring out detector locations */
  int location[NUM_ELEMENTS], quadid[NUM_SPACES];

  headas_chat(5,"...Entering batmasktaglc_work...\n");
  max_row=mkwtmap[0].num_rows;  
  headas_chat(5," ...mask weight nrows=%d...\n", max_row);

  batmasktaglc_detid(location, quadid);

  /* One giant allocation */
  a = malloc((7*4 + 9*1)*max_row*sizeof(double));
  if (a == 0) {
    fprintf(stderr, "ERROR: memory allocation 1\n");
    status = MEMORY_ALLOCATION;
    goto cleanup;
  }
  b = a + 4*max_row;
  c = b + 4*max_row; 
  d = c + 4*max_row;
  atot = d + 4*max_row;
  btot = atot + max_row;
  ctot = btot + max_row;
  dtot = ctot + max_row;
  u1 = dtot + max_row;
  u2 = u1 + 4*max_row;
  u3 = u2 + 4*max_row;
  u1tot = u3 + 4*max_row;
  u2tot = u1tot + max_row;
  u3tot = u2tot + max_row;
  cos_edge = u3tot + max_row;
  pcode_corr = cos_edge + max_row;

  /* Method for calculating the source, background and errors:
     There are two observables, C (quad rates) and W (mask weights), 
     which are the total counts and total weighted counts:
    [  C  ]   =  [  1  1  ...   1 ]   [ c1 ]         (eqn 1)
    [  W  ]      [ w1 w2  ...  wN ]   [ .  ]  
                                      [ .  ]
                                      [ .  ]
                                      [ cN ]
   where wi is the mask weight for a particular detector, and ci is the
   counts in that detector.  Also, one assumes that the counts in each
   detector can be broken down into:

   ci  = B + ei S + di                           (eqn 2)

   We have to make two corrections to eqn2.  First is that the wi's are
   on the interval [0,14] and the source flux S is actually multiplied
   by a weighting ei on the interval [0,1].  The relationship is: 

   ei = [ (wi*2) + 1 + fi ] / 30.
   
   where fi = -0.77 for wi=0 
         fi = 0.77 for wi=14
         fi = 0 otherwise

   For detectors where fi=0, ei = [ (wi*2) + 1 ] / 30.
   This just maps wi=1 -> ei=3/30,  wi=7 -> ei=1/2, etc.
   The fi's are there because the actual distribution of weights is not 
   even.  There are "spikes" in the distribution at wi=0 and wi=14 due to
   the mask cells being larger than the detectors (an overabundance of 
   fully blocked and fully open detectors).  Empirically, I determined that
   about 20% of the detectors have wi=0 and 20% have wi=14.  The remaining
   weights are evenly distributed, with about 4.6% at each integral weight
   from 1 to 13.  This is shown to be the case both for fully coded and
   partially coded sources. Some algebra tells us that the correct 
   relationship between ei and wi in the lowest and highest bins should be:

   lowest:
   ei = (0*15.4 + (1/30)*4.6)/20 = 0.007666... 
      with wi=0, we have ei = 0.007666 = [1 + fi] / 30  -> fi=-0.77

   highest:
   ei = (1*15.4 + (29/30)*4.6)/20 = 0.992333...
      with wi=0, we have ei = 0.99233 = [28 + 1 + fi] / 30  -> fi=0.77

   and also, in the wi's, shielded (uncoded) detectors get a value wi=7. 
   However, these detectors will have no contribution from the source, 
   so I should only count coded detectors in the sum 

   Defining B as the background counts, S is the source counts, and di is 
   a random variable (iid).  

   Working out eqn (2) gives:

   W = Sum[wici] = B*Sum[wi] + S*(Sum[wi*ei] - 3.5Ns)
   W = B*w + S*(we - 3.5Ns)                                 (eqn 3)

   where we have defined w = Sum[wi],  we = Sum[wi*ei] 
   and Ns is the number of shielded detectors, each of which have wi=7
   and ei=0.5, hence the multiplier 3.5

   C = Sum[ci] = B*N + S*(Sum[ei] - 0.5Ns)
   C = B*N + S*(e - 0.5Ns)                                  (eqn 4)

   where we define e = Sum[ei], and N is the total number of detectors.
   Here each of the Ns shielded detectors has ei=0.5.

   Combining eqns (1), (3) and (4)  gives:
    [ C ]  =  [ N   e - 0.5Ns  ] [ B ]   +   [ sum(di)    ]   (eqn 5)
    [ W ]     [ w   we-3.5(Ns) ] [ S ]       [ sum(wi di) ]

   a little more algebra will eliminate the e's:

   e = Sum[ei] = (1/30)*Sum[wi*2 + 1 + fi]
   e = (1/30) * ( 2*Sum[wi] + N + Sum[fi] )
   e = (1/30) * ( 2*w + N + 0.77*(N14-N0))

   where N14 is the number of detectors with wi=14 and N0 the number 
   with wi=0.

   define e' = e - 0.5Ns

   we = Sum[wi*ei] = (1/30)*Sum[wi*wi*2 + wi + wifi]
   we = (1/30) * ( 2*Sum[wi*wi] + Sum[wi] + Sum[wifi] )
   we = (1/30) * ( 2*w2 + w + 10.78*N14)

   where w2 = Sum[wi*wi] and 
   Sum[wifi] = N14*(14)*(0.77) + N0*(0)*(-0.77) = 10.78*N14

   define (we)' = we - 3.5Ns

   Eqn 5 can be rewritten as:
     O  = M x  +  d                                (eqn 6)
   where O is the observable vector [ C W ], M is the 2x2 matrix in eqn(3), 
   x is the unknown source & background vector, and d is the random
   noise term.

   Okay, the goal is to estimate S and its variance as well as B.  
   Thus, we compute the expected values of x and get:
      x  = M^{-1}  ( O - d )                          (eqns 7)
      E(x) = M^{-1} E( O - d )
           = M^{-1} O
   since E(d) = zero.
   Now, to invert the matrix, I take
   detM =  N*(we)' - (w)e'
      M^{-1} becomes:
      [ (we)' /detM     -e'/detM ]
      [ -w /detM         N/detM  ]

   For the case of fully coded sources, Ns = 0, so this simplifies to:
      M^{-1} =
      [ we /detM     -e/detM ]
      [  - w/detM     N/detM ]
   multiplying out the equation
      M^{-1} O
      [ B ] = [ we / detM     -e/detM ] [ C ]
      [ S ] = [ - w/detM      N/detM  ] [ W ]              (eqn8)

   Once we have B and S we have enough information to go back and calculate 
   the variance in the source flux.  We will use the same method used in 
   calculating the errors in the light curve derived from the event files 
   (batbinevt):

   --------
   For the case of Ns=0, we also have N0=N14=0.2N
  
   w = 7N   w2 = 77N (derived from the same weighting used to find fi) 
   e = (1/30) * (2w + N) = (1/30) * (15N)
   e = N/2  , which is expected for this case.

   we = (1/30) * (2w2 + w * 10.78*(0.2N))
   we = (1/30) * (2*77N + 7N + 2N) = (1/30) * (163N)
   we = 5.5N

   detM = N*we - (w)*(e) = 5.5N^2 - 3.5 N^2 
   detM = 2N^2

   Eqn 8 becomes:

      [ NB ] = [ 2.7    -0.25] [ C ]
      [ NS ] = [ -3.5    0.5 ] [ W ]              (eqn9)

   Solving eq8 for NB and NS gives:

   NS = -3.5 C + 0.5 W
   NS = 7 * [ (1/14) W - (1/2) C ]

   NB = 2.7 C - 0.25 W
   -------

   Remember that all of the above (between the -----'s) is for the 
   simple case of Ns=0. 

   HAK 1-Feb-2005  Found an error in the way I was calculating the
   errors;  thanks to JT and CM for pointing this out.

   Now to find the errors.

   OLD Here for each detector, we calculate the weighted counts:
   OLD ci = B + S ui + di,
   OLD where now the weights are ui's on the interval [-1,1].  And here we 

   Here for each detector, we have calculated the counts:
   ci = B + S ei + di,
   where the weights are ei's on the interval [0,1].  And here we 
   don't have to disregard the shielded detectors, since they don't 
   contribute to the weighted counts.

   The variance in the mask weighted counts is
     sig2 = sum(ui^2 ci)
 
     where the ui's are weights on the interval [-1,1].

   OLD  sig2 = B sum (ui^2) + S sum (ui^3) + sum (ui^2 di)
   OLD Assuming the ui's and di's are uncorrelated, the last term is zero.
   OLD So sig2 = B sum (ui^2) + S sum (ui^3) 

     sig2 = B sum (ui^2) + S sum (ui^2 * ei) + sum (ui^2 * di)
   Assuming the ui's and di's are uncorrelated, the last term is zero.
   So sig2 = B sum (ui^2) + S sum (ui^2 * ei) 
    
   Now ei = (ui +1)/2, so
   sum (ui^2 * ei) = (1/2)(sum (ui^3) + sum (ui^2))

   thus,
   sig2 = B sum (ui^2) + S/2 sum (ui^2) + S/2 sum (ui^3)
   sig2 = B sum (ui^2) + (S/2) ( sum (ui^2) + sum (ui^3))

   or, alternatively,
   sig2 = (B + S/2) sum (ui^2) + S/2 sum (ui^3) 

   Now there is one further step.  The Source and Background I have
   calculated are for an unmasked detector array (the calculations have
   taken out the dimunition effects of the mask), but the variance is 
   based on the rates for a masked detector array.  Therefore, I must
   divide the final variance by a factor:

   [ sum (ei * ui) ]^2  = [ sum ( 0.5(ui +1) * ui) ]^2
                 = 0.25 * [ sum (ui^2 + ui) ]^2
                 = 0.25 * [ ( sum (ui^2) + sum (ui)) ]^2

   so 
   sig2 = 2 * [ 2*B sum (ui^2) + S ( sum (ui^2) + sum (ui^3)) ] /
      ( sum (ui^2) + sum (ui) )^2
   
   since sum (ui^2) ~ 0.55 and sum (ui) ~ 0, the denominator is ~ 0.27,
   so the multiplicative factor is ~ 3.7.

   */

  /* Loop through all possible rows in the mask weight map table */
  for (row=0;row<max_row;row++) {  

     /* Zero out all the counters */
     for (i=0;i<4;i++) {
        n0[i] = 0.0;  
        n14[i] = 0.0;  
        w[i] = 0.0;  
        w2[i] = 0.0;  
        ns[i] = 0.0;
        ngood[i] = (double)(NUM_ELEMENTS/4);
     }
     n0tot=0.0; n14tot=0.0; wtot=0.0, w2tot=0.0; nstot=0.0;
     nbaddets=0;
     ngooddets=NUM_ELEMENTS;

     for (i=0;i<NUM_SPACES;i++) {
        if ((detmask[i]) && (quadid[i] != -1)) {
           nbaddets++;
           ngood[quadid[i]]--;
           ngooddets--;
        }
     }
     headas_chat(5," ...number of bad detectors = %d...\n",nbaddets);
     headas_chat(5," ...number of good detectors = %d,%d,%d,%d (total %d)\n",
		 (int)ngood[0], (int)ngood[1], (int)ngood[2], (int)ngood[3], 
		 (int)ngooddets);


     for (i=0;i < NUM_SPACES;i++) {
        if ( (mkwtmap[row].mask_weights[i] == 0) && 
           (!(detmask[i])) && (quadid[i] != -1) ) {
           n0[quadid[i]] += 1.0;
           n0tot += 1.0;
        }
        if ( (mkwtmap[row].mask_weights[i] == 14) && 
           (!(detmask[i])) && (quadid[i] != -1) ) {
           n14[quadid[i]] += 1.0;
           n14tot += 1.0;
        }
     }

     headas_chat(5,"      ROW %4d %-16s   %11s %11s %11s %11s\n",
		 row, "STATS", "QUAD0", "QUAD1", "QUAD2", "QUAD3");
     headas_chat(5," %30s   %11.1f %11.1f %11.1f %11.1f\n",
		 "SUM", mkwtmap[row].sum[0], mkwtmap[row].sum[1],
		 mkwtmap[row].sum[2], mkwtmap[row].sum[3]);
     headas_chat(5," %30s   %11.1f %11.1f %11.1f %11.1f\n",
		 "SUMSQ", mkwtmap[row].sumsq[0], mkwtmap[row].sumsq[1],
		 mkwtmap[row].sumsq[2], mkwtmap[row].sumsq[3]);
     headas_chat(5," %30s   %11.1f %11.1f %11.1f %11.1f\n",
		 "SUMSQMK", mkwtmap[row].sumsqmk[0], mkwtmap[row].sumsqmk[1],
		 mkwtmap[row].sumsqmk[2], mkwtmap[row].sumsqmk[3]);
     headas_chat(5," %30s   %11.1f %11.1f %11.1f %11.1f\n",
		 "MASKED", mkwtmap[row].masked[0], mkwtmap[row].masked[1],
		 mkwtmap[row].masked[2], mkwtmap[row].masked[3]);
     headas_chat(5," %30s   %11.1f %11.1f %11.1f %11.1f\n",
		 "SHIELDED", mkwtmap[row].shielded[0], mkwtmap[row].shielded[1],
		 mkwtmap[row].shielded[2], mkwtmap[row].shielded[3]);


     /* Now calculate all the parameters for deriving the source and
	background */
     for (i=0;i<4;i++) {
        w[i] = mkwtmap[row].sum[i];      /* Sum of the weights */
        wtot += mkwtmap[row].sum[i];    
        w2[i] = mkwtmap[row].sumsq[i]; /* Sum of the squares of the weights */
        w2tot += mkwtmap[row].sumsq[i];
        ns[i] = mkwtmap[row].shielded[i];  /* Number of shielded detectors */
        nstot += mkwtmap[row].shielded[i];
     }
   
     /* Now need to correct the sums to remove the disabled detectors. */
     /* Don't need to do this correction on ns */
     for (i=0;i<NUM_SPACES;i++) {
        if ((detmask[i]) && (quadid[i] != -1)) {
           w[quadid[i]] -= mkwtmap[row].mask_weights[i]; 
           wtot -= mkwtmap[row].mask_weights[i];
           w2[quadid[i]] -= 
              (mkwtmap[row].mask_weights[i])*(mkwtmap[row].mask_weights[i]);
           w2tot -= 
              (mkwtmap[row].mask_weights[i])*(mkwtmap[row].mask_weights[i]);
        }
     }

     for (i=0;i<4;i++) {
        /* Next calculate some derived quantities from the equations above */
        e[i] = (1.0/30.0) * (2.0*w[i] + ngood[i] + f14*n14[i] + f0*n0[i]); 
        e[i] -= 0.5*ns[i];
        we[i] = (1.0/30.0) * (2.0*w2[i] + w[i] + wm*f14*n14[i]);
        we[i] -= 0.5*0.5*wm*ns[i];  /* 3.5 Ns */
     }

     etot = (1.0/30.0) * (2.0*wtot + ngooddets + f14*n14tot + f0*n0tot); 
     etot -= 0.5*nstot;
     wetot = (1.0/30.0) * (2.0*w2tot + wtot + wm*f14*n14tot);
     wetot -= 0.5*0.5*wm*nstot;

     for (i=0;i<4;i++) {
        /* detm[i] = ngood[i]*we[i] - w[i]*e[i]; */
        detm[i] = ngood[i]*wetot - wtot*e[i];
        detm[i] /= ngood[i];
       /* Determinant of matrix relating observables to source & background */

        /* a[row*4+i] = (float) (we[i]/detm[i]); */
        a[row*4+i] = (float) (wetot/detm[i]);
        b[row*4+i] = (float) (-e[i]/detm[i]);
        /* c[row*4+i] = (float) (-w[i]/detm[i]); */
        c[row*4+i] = (float) (-wtot/detm[i]);
        d[row*4+i] = (float) (ngood[i]/detm[i]);
   /* Four elements of inverse matrix for deriving S and B from observables */
     }

     detmtot = ngooddets*wetot - wtot*etot;
     detmtot /= ngooddets;
     atot[row] = (float) (wetot/detmtot);
     btot[row] = (float) (-etot/detmtot);
     ctot[row] = (float) (-wtot/detmtot);
     dtot[row] = (float) (ngooddets/detmtot);

     /* Now for deriving the errors we also need to know the sum of weights
     squared and cubed on the interval [-1,1] */
     u1tot[row] = 0.0;
     u2tot[row] = 0.0;
     u3tot[row] = 0.0;
     for (i=0;i<4;i++) {
        u1[row*4+i]=0.0;
        u2[row*4+i]=0.0;
        u3[row*4+i]=0.0;
     }
     for (i=0;i<NUM_SPACES;i++) {
        u = (mkwtmap[row].mask_weights[i]-7.0)/7.0;
        if ((!(detmask[i])) && (quadid[i] != -1)) { 
           u1[row*4+quadid[i]] += u;
           u2[row*4+quadid[i]] += u*u;
           u3[row*4+quadid[i]] += u*u*u;
           u1tot[row] += u;
           u2tot[row] += u*u;
           u3tot[row] += u*u*u;
        }
     }
  /* To correct for the fact that the gaps have contributed an additional 
     +1 for u^2 and -1 for u and u^3 for each gap cell. Only do this if 
     there is no detector mask -- otherwise it will have already been 
     taken care of above.*/
  /* if (!(nbaddets)) { 
        u1[row] += (NUM_SPACES - NUM_ELEMENTS);
        u2[row] -= (NUM_SPACES - NUM_ELEMENTS);
        u3[row] += (NUM_SPACES - NUM_ELEMENTS);
	} */
     for (i=0;i<4;i++) {
        u1[row*4+i] /= ngood[i];
        u2[row*4+i] /= ngood[i];
        u3[row*4+i] /= ngood[i];  
     }
     u1tot[row] /= ngooddets;
     u2tot[row] /= ngooddets;
     u3tot[row] /= ngooddets;

  }  /* End of calculating all the mask weight parameters */ 

  /* Open MASK_TAG_RATES extension of infile */
  headas_chat(5," ...opening file %s...\n",infile1);
  fits_open_file(&infptr1, infile1, READONLY, &status);
  if (status) {
     fprintf(stderr,"ERROR: unable to open %s \n", infile1);
     goto cleanup;
  }    
  /* Get infile1 TIME and COUNT columns */
  fits_get_colnum(infptr1, CASESEN, "TIME", &infile_time_col,
		    &status);
  if (status) {
     fprintf(stderr,"ERROR: cannot get TIME column number \n");
     goto cleanup;
  }    
  
  fits_get_colnum(infptr1, CASESEN, "WEIGHTED_COUNTS", &infile_count_1_col,
		    &status);
  if (status) {
    infile_count_1_col = -1;	  
    headas_chat(5, "WEIGHTED_COUNTS column not found \n");
      
    /* Try getting the column number for COUNTS */
    status = 0; 
    fits_get_colnum(infptr1, CASESEN, "COUNTS", &infile_count_2_col,
		    &status);
    if (status) {

      /* Try getting the column number for COUNT */
      status = 0; 
      fits_get_colnum(infptr1, CASESEN, "COUNT", &infile_count_2_col,
		      &status);
      if (status) {
	fprintf(stderr,"ERROR: cannot get WEIGHTED_COUNTS or COUNTS column number \n");
	goto cleanup;
      }
    }
    headas_chat(5, " ...WEIGHTED_COUNTS column = %d...\n", infile_count_2_col);
  }      
  if (infile_count_1_col == -1){
	infile_count_col = infile_count_2_col;
  }else{
     infile_count_col = infile_count_1_col;
  }      
  /* Get the number of rows in the MASK_TAG_RATES extension of
     the infile */
  fits_get_num_rows(infptr1, &num_infile_rows, &status);
  if (status) {
    fprintf(stderr,"ERROR: unable to number of rows in infile\n");
    goto cleanup;
  }
  headas_chat(5," ...masktag rates rows = %d...\n", num_infile_rows);  

  /* Find the time base (should be 1.6 seconds) */
  fits_read_key(infptr1, TFLOAT, "TIMEDEL", &timedel, 0, &status);

  /* Get quadfile1 name */
  sprintf(quadfile1, "%s[RATE]", quadfile);

  /* Open RATE extension of quadfile      */
  headas_chat(5, " ...opening file %s...\n", quadfile1);
  fits_open_file(&quadfptr, quadfile1, READONLY, &status);
  if (status) {
     status=0;
     headas_chat(5," ...trying B1600MS_RATES extension...\n");
     sprintf(quadfile1, "%s[B1600MS_RATES]", quadfile);
     fits_open_file(&quadfptr, quadfile1, READONLY, &status);
     if (status) {
        fprintf(stderr,"ERROR: unable to open %s \n", quadfile1);
        goto cleanup;
     }
  }    

  /* Find the time base (should be 1.6 seconds) */
  fits_read_key(quadfptr, TFLOAT, "TIMEDEL", &quad_timedel, 0, &status);
  /* Compare to make sure that it is the same as that in the mask weight 
     light curve file */
  if (timedel != quad_timedel) {
       fprintf(stderr, "ERROR: Time base in mask weight rates file does not match that in quad rates files.\n");
       fprintf(stderr, "   Rate file TIMDEL is %f\n", timedel);
       fprintf(stderr, "   Quad file TIMEDEL is %f\n", quad_timedel);
       status = -1;
       goto cleanup;
    }
  headas_chat(5, " ...time bin size=%f...\n", timedel);


  /* Get the colunm number for TIME */
  fits_get_colnum(quadfptr, CASESEN, "TIME", &quadfile_time_col,
		    &status);
  /* Get the column numbers for QUAD0_COUNTS, QUAD1_COUNTS,
     QUAD2_COUNTS and QUAD3_COUNTS */
  fits_get_colnum(quadfptr, CASESEN, "QUAD0_COUNTS", 
		  &quadfile_quad0_col, &status);
  fits_get_colnum(quadfptr, CASESEN, "QUAD1_COUNTS", 
		  &quadfile_quad1_col, &status);
  fits_get_colnum(quadfptr, CASESEN, "QUAD2_COUNTS", 
		  &quadfile_quad2_col, &status);
  fits_get_colnum(quadfptr, CASESEN, "QUAD3_COUNTS", 
		  &quadfile_quad3_col, &status);
  if (status) {
    fprintf(stderr,"ERROR: unable to get column numbers in binary table \n");
    goto cleanup;
  }     
  
  /* Get number of rows in B1600MS_RATES extension of quadfile */
  fits_get_num_rows(quadfptr, &num_quadfile_rows, &status);
  if (status) {
    fprintf(stderr,"ERROR: unable to get number of rows in quad file \n");
    goto cleanup;
  }
  headas_chat(5," ...quad rates rows = %d...\n", num_quadfile_rows);  

  /*              Allocate memory          */
  row_l0_adj_incts = malloc((num_infile_rows+1)*12*sizeof(float));
  row_l1_adj_incts = row_l0_adj_incts + (num_infile_rows+1);
  row_l2_adj_incts = row_l1_adj_incts + (num_infile_rows+1);
  row_l3_adj_incts = row_l2_adj_incts + (num_infile_rows+1);
  row_l0_bkg       = row_l3_adj_incts + (num_infile_rows+1);
  row_l1_bkg       = row_l0_bkg       + (num_infile_rows+1);
  row_l2_bkg       = row_l1_bkg       + (num_infile_rows+1);
  row_l3_bkg       = row_l2_bkg       + (num_infile_rows+1);
  err3_lvl0        = row_l3_bkg       + (num_infile_rows+1);
  err3_lvl1        = err3_lvl0        + (num_infile_rows+1);
  err3_lvl2        = err3_lvl1        + (num_infile_rows+1);
  err3_lvl3        = err3_lvl2        + (num_infile_rows+1);

  time_infile_rows   = malloc( (2*(num_infile_rows+1) + (num_quadfile_rows+1))*sizeof(double));
  time_outfile_rows  = time_infile_rows  + (num_infile_rows+1);
  time_quadfile_rows = time_outfile_rows + (num_infile_rows+1);

  row_infile   = malloc((num_infile_rows+1)*4*sizeof(long));
  row_quadfile =        row_infile    + (num_infile_rows+1);
  row_mkwtmap  = 	row_quadfile  + (num_infile_rows+1);
  outfile_row_mkwtmap = row_mkwtmap   + (num_infile_rows+1);
  if ((row_l0_adj_incts == 0) || (time_infile_rows == 0) ||
      (row_infile == 0)) {
    fprintf(stderr, "ERROR: Memory allocation 2\n");
    goto cleanup;
  }

  /*          End allocating memory          */

  for (i = 0; i < (num_infile_rows ); i++) {
     /* Get the TIME for row i+1 of binary table of 
        MASK_TAG_RATES extension of infile*/
     ii = (long) (i + 1);
     fits_read_col(infptr1,TDOUBLE,infile_time_col,ii,1,1,NULL,
                   &time_infile,NULL,&status); 
     time_infile_rows[i] = time_infile;
     /* All set to -1 initially to indicate unmatched rows */
     row_infile[i] = -1;
     row_quadfile[i] = -1;
     row_mkwtmap[i] = -1;
  }

  for (j = 0; j < (num_quadfile_rows); j++) {
     /* Get the TIME for row j+1 of binary table of
        the RATE extension of quad file */
     jj = (long) (j + 1);
     fits_read_col(quadfptr,TDOUBLE,quadfile_time_col,jj,
		1,1,NULL,&time_quadfile,NULL,&status);
     time_quadfile_rows[j] = time_quadfile;
  }

  /* Now go through the infile and quadfile times and match times */
  ii=0;
  for (i = 0; i < (num_infile_rows ); i++) {
     done=num_quadfile_rows;
     for (j = 0; j < done; j++) {
        if (time_infile_rows[i] == time_quadfile_rows[j]) {
	   /* The ii-th row in the array of valid, matched raw mask tagged values
              is the i-th row in the array of all raw mask tagged values */
	   row_infile[ii] = i;
           /* And the ii-th row in the array of valid, matched quad rate values
              is the j-th row in the array of all quad rate values.  Note that
              i and j can be different if there are offsets between the two
              input arrays */
	   row_quadfile[ii] = j;
	   if (i < 20) headas_chat(10,"Tin=%f  Tqd=%f  Row=%d,%d\n",
	      time_infile_rows[i],time_quadfile_rows[j],
              row_infile[ii],row_quadfile[ii]);
	   done=j;  /* Need look no farther */
           ii++;
       } 
     }
     /* We do the same matching to the rows (indexed here by j) in the mask
        weight map table.  The -1 three lines below is needed because the
        index ii has already been incremented above. */
     for (j = 0; j < max_row; j++) {
        if (time_infile_rows[i] >= mkwtmap[j].time) 
	   row_mkwtmap[ii-1]=j;
     }
  }  

  for (i = 0; i < num_infile_rows; i++) {
     /* Get the TIME for row i+1 of binary table of 
        MASK_TAG_RATES extension of infile*/
     ii=row_infile[i]+1;
     jj=row_quadfile[i]+1;
     kk=row_mkwtmap[i];
     /* HAK 27-Jan-2005 Added test to make sure that there was a valid
      * row in the mask weight map as well (kk > -1) */
     if ((ii > 0) && (jj > 0) && (kk > -1)) { 

	/* Read the input file and get the counts for
	 * the 4 energy levels. Each energy level has
	 * only one value of counts */
	fits_read_col(infptr1,TFLOAT,infile_count_col,ii,
           1,4,NULL,elvl_cts,NULL,&status);
	if (status) {
	   fprintf(stderr,"Unable to read infile row %d (status=%d)\n",
	      (int)ii,status);
           goto cleanup;
	}
	/* HAK 27-Jan-2004 Add check to make sure input counts are not
	 * zero*/
	if ((elvl_cts[0] > 0) && (elvl_cts[1] > 0) &&
	   (elvl_cts[2] > 0) && (elvl_cts[3] > 0)) { /* Continue */		
	
	row_count++;	

        /* Fill the appropriate output file time row New HAK 27-Jan-2005 */
	time_outfile_rows[row_count] = time_infile_rows[ii-1];

        /* Make a proper table of which mask weight map and hence set
           of corrections. */
        outfile_row_mkwtmap[row_count] = kk;

	/* Get the quad count values for the 4 energy levels
	   for row number jj */
	fits_read_col(quadfptr,TFLOAT,quadfile_quad0_col,jj,
	   1,4,NULL,quad0_cts,NULL,&status);
	fits_read_col(quadfptr,TFLOAT,quadfile_quad1_col,jj,
	   1,4,NULL,quad1_cts,NULL,&status);
	fits_read_col(quadfptr,TFLOAT,quadfile_quad2_col,jj,
	   1,4,NULL,quad2_cts,NULL,&status);
	fits_read_col(quadfptr,TFLOAT,quadfile_quad3_col,jj,
	   1,4,NULL,quad3_cts,NULL,&status);
        if (status) {
	   fprintf(stderr,"Unable to read quadfile row %d (status=%d)\n",
	      j,status); 
           goto cleanup;
        }
	/* Sum quad0, quad1, quad2, quad3 count values for
	   each of the energy levels */
	  
        for (k = 0; k < 4; k++) {
	   sum_quad_cts[k] = quad0_cts[k] + quad1_cts[k] +
		             quad2_cts[k] + quad3_cts[k];
        }	      
	      	   
        /* Here we calculate the source counts.  There are four separate
           calculations for the four energy bands.  We add together the
           contributions from the four quadrants at this point. */
        /* HAK 17-Mar-2004  Added the ability to scale the input counts
           by a user-supplied scale factor */
        /* HAK 15-Sep-2005 Removed the scale factor from here and put
           it where it should be in the corrections at the end */
        for (k=0;k<4;k++) {
	   /* The hooks are there for doing it by quadrant; this will
              be enabled when I figure out how to do it */        
           /* src[k] = (dtot*elvl_cts[k] + 
              c[kk*4]*quad0_cts[k] + c[kk*4+1]*quad1_cts[k] + 
	      c[kk*4+2]*quad2_cts[k] + c[kk*4+3]*quad3_cts[k]); */
	   src[k] = (dtot[kk]*elvl_cts[k] + ctot[kk]*sum_quad_cts[k]);
        }

        row_l0_adj_incts[row_count] = src[0];
        row_l1_adj_incts[row_count] = src[1];
        row_l2_adj_incts[row_count] = src[2];
        row_l3_adj_incts[row_count] = src[3];

        /* Here we calculate the background counts.  There are four separate
           calculations for the four energy bands.  We add together the
           contributions from the four quadrants at this point. */
        for (k=0;k<4;k++) { 
	   /* The hooks are there for doing it by quadrant; this will
              be enabled when I figure out how to do it */        
           /* bkg[k] = (btot*elvl_cts[k] + 
              a[kk]*quad0_cts[k] + a[kk*4+1]*quad1_cts[k] +
              a[kk*4+2]*quad2_cts[k] + a[kk*4+3]*quad3_cts[k] ); */	
           bkg[k] = (btot[kk]*elvl_cts[k] + atot[kk]*sum_quad_cts[k]);
        }

        row_l0_bkg[row_count] = bkg[0];
        row_l1_bkg[row_count] = bkg[1];
        row_l2_bkg[row_count] = bkg[2];
        row_l3_bkg[row_count] = bkg[3];

	/* Compute err3 for each row */
        /* HAK 1-Feb-2005 Corrected the way this is calculated */
        /* HAK 7-Feb-2005 Added correction of 1/sqrt(timedel) which
           accounts for the fact that the argument of the error should
           be multiplied by 1.6 <- base errors on total counts, not 
           counts per second -- and then the final error should be
           divided by 1.6, the net result being to divide by sqrt(1.6) */
        /* HAK 7-Feb-2005  Add correction described in comments above:
           sig2 = B sum (ui^2) + (S/2) ( sum (ui^2) + sum (ui^3)) 
           becomes
           sig2 = 2 *[ 2*B sum (ui^2) + S ( sum (ui^2) + sum (ui^3)) ] /
	   ( sum (ui^2) + sum (ui) )^2 */
        for (k=0;k<4;k++) { 

	   /* The hooks are there for doing it by quadrant; this will
              be enabled when I figure out how to do it */        
           squad[0]=(d[kk*4]*elvl_cts[k] + c[kk*4]*quad0_cts[k]);
           squad[1]=(d[kk*4+1]*elvl_cts[k] + c[kk*4+1]*quad1_cts[k]);
           squad[2]=(d[kk*4+2]*elvl_cts[k] + c[kk*4+2]*quad2_cts[k]);
           squad[3]=(d[kk*4+3]*elvl_cts[k] + c[kk*4+3]*quad3_cts[k]);
           bquad[0]=(b[kk*4]*elvl_cts[k] + a[kk*4]*quad0_cts[k]);
           bquad[1]=(b[kk*4+1]*elvl_cts[k] + a[kk*4+1]*quad1_cts[k]);
           bquad[2]=(b[kk*4+2]*elvl_cts[k] + a[kk*4+2]*quad2_cts[k]);
           bquad[3]=(b[kk*4+3]*elvl_cts[k] + a[kk*4+3]*quad3_cts[k]);

	   /*  var[k] = 0.0;
           for (j=0;j<4;j++) {
              var[k] += ( 2.0 * (2.0*bquad[j]*u2[kk*4+j] + 
	         squad[j]*(u2[kk*4+j]+u3[kk*4+j])) ) / 
		( (u2[kk*4+j]+u1[kk*4+j])*(u2[kk*4+j]+u1[kk*4+j]));  
		} */
	  var[k] = (2.0 * (2.0*bkg[k]*u2tot[kk] + 
	     src[k]*(u2tot[kk]+u3tot[kk]))) /
	    ((u2tot[kk]+u1tot[kk])*(u2tot[kk]+u1tot[kk]));
           /* To avoid taking the square root of a negative number
              below -- the variance should never be negative. */
           if (var[k] < 0.0) var[k]=0.0;
           
        }

	err3_lvl0[row_count] = sqrt(var[0]);
	err3_lvl1[row_count] = sqrt(var[1]);
	err3_lvl2[row_count] = sqrt(var[2]);
	err3_lvl3[row_count] = sqrt(var[3]);

        } /* End conditional */
     }
  } /* for i */      	  
  headas_chat(5," ...row_count = %d...\n", row_count);

  /* Create the outfile */
  headas_clobberfile(outfile);
  status=0;
  headas_chat(5, " ...creating output file %s...\n", outfile);
  if (fits_create_file(&outfptr, outfile, &status)) {
     fprintf(stderr,"ERROR: unable to create outfile %s (status %d)\n",
        outfile,status);
     goto cleanup;
  }
  
  ttype_rate[0] = "TIME";
  ttype_rate[1] = "RATE";
  ttype_rate[2] = "ERROR";
  ttype_rate[3] = "BACKV";  /* CM - change to BACKV according to OGIP std */
  
  tform_rate[0] = "1D";
  tform_rate[1] = "4E";
  tform_rate[2] = "4E";
  tform_rate[3] = "4E";

  tunit_rate[0] = "s";
  tunit_rate[1] = "count/s";  /* CM - change to lower case according to OGIP */
  tunit_rate[2] = "count/s";
  tunit_rate[3] = "count/s";

  comments[0]   = "Time of light curve time bin";
  comments[1]   = "Mask tagged rate";
  comments[2]   = "Statistical error";
  comments[3]   = "Background rate";

  /* Create binary table in HDU RATE */
  fits_create_tbl(outfptr, BINARY_TBL, 0, tfields_rate, ttype_rate,
		    tform_rate, tunit_rate, "RATE", &status);
  if (status) {
     fprintf(stderr,"ERROR: unable to create bin tbl in HDU RATE \n");
     goto cleanup;
  }

  /* Write descriptive column comments */
  {
    int i;
    char ttypen[FLEN_CARD];
    
    for (i=0; i<tfields_rate; i++) {
      if (comments && comments[i] && comments[i][0]) {
	fits_make_keyn("TTYPE", i+1, ttypen, &status);
	fits_modify_comment(outfptr, ttypen, comments[i], &status);
      }
    }
  }


  /* ========== CM Copy input file keywords to output */
  {
    int i;
    int nkeys = 0;
    char card[FLEN_CARD];

    fits_get_hdrspace(infptr1, &nkeys, NULL, &status);
    
    for (i = 1; i <= nkeys; i++) {
      fits_read_record(infptr1, i, card, &status);
      /* Copy HISTORY keywords and any user keywords, but not COMMENTs */
      if ((fits_get_keyclass(card) >= TYP_REFSYS_KEY) &&
	  (strncmp(card, "COMMENT ", 7) != 0))
	fits_write_record(outfptr, card, &status);
    }
  }

  /* ========== Write required OGIP keywords */
  { 
    char creator[FLEN_CARD];
    int backapp = 1;

    /* OGIP required columns:
       TIME
       RATE   - nE - count/s - source rate
       ERROR  - nE - count/s - source error
       BACKV  - nE - count/s - background rate
       BACKE  - nE - count/s - background error
       
       OGIP required keywords
               TIMEDEL (if needed), TSTART*, TSTOP*, BACKAPP=T, TIMEPIXR*, 
               ONTIME*, EXPOSURE*, HDUCLASS=OGIP, HDUCLAS1=LIGHTCURVE, 
	       HDUCLAS2=TOTAL/NET/BKG, HDUCLAS3=COUNT/RATE
	       TIMVERSN=OGIP/93-003, EXTNAME=RATE

       Keywords marked by "*" are already set in input mask tag rate data.
    */

    sprintf(creator, "%s %s", taskname, taskver);
    fits_update_key(outfptr, TSTRING, "CREATOR", creator,
		    "Program that created this FITS file", &status);

    fits_update_key(outfptr, TLOGICAL,"BACKAPP", &backapp, 
		    "Was background correction applied?", &status);
    fits_update_key(outfptr, TSTRING, "HDUCLASS", "OGIP", 
		    "Conforms to OGIP/GSFC standards", &status);
    fits_update_key(outfptr, TSTRING, "HDUCLAS1", "LIGHTCURVE", 
		    "Contains light curve", &status);
    fits_update_key(outfptr, TSTRING, "HDUCLAS2", "NET", 
		    "Light curve is background subtracted", &status);
    fits_update_key(outfptr, TSTRING, "HDUCLAS3", "RATE", 
		    "Light curve is count/s", &status);
    /* Added this keyword HAK 2-Feb-2005 */
    fits_update_key(outfptr, TSTRING, "FLUXMETH", "WEIGHTED",
                    "Flux extraction method", &status);
    /* Also possible to have: */
    /* fits_update_key(outfptr, TSTRING, "HDUCLAS3", "COUNT",  */
    /*                 "Light curve is units of count", &status); */

    fits_update_key(outfptr, TSTRING, "TIMVERSN", "OGIP/93-003", 
		    "Version of light curve format", &status);

    /* MAY want to add this since Fink's current code puts TIMEDEL=0 */
    fits_update_key(outfptr, TFLOAT, "TIMEDEL", &timedel, 
		    "[s] Light curve sampling period", &status);
  }

#define min(x,y) ((x<y)?(x):(y))

  /* Apply corrections to the rate (cosine and/or partial coding) */
  if ((corrs.ccosine) || (corrs.cflatfield)) {   
     headas_chat(5, " ...applying cosine correction...\n");
     z = 1e7;  /* Put z at infinity */
     /* Convert theta and phi to X,Y for correction */
     for (i=0;i<max_row;i++) {
        x = z * sqrt((tan(mkwtmap[i].theta)*(tan(mkwtmap[i].theta)))/
		     (1.0 + tan(mkwtmap[i].phi)*tan(mkwtmap[i].phi)));
	y = z * sqrt((((tan(mkwtmap[i].theta)*(tan(mkwtmap[i].theta))) *
	    (tan(mkwtmap[i].phi)*tan(mkwtmap[i].phi)))) /
	    (1.0 + tan(mkwtmap[i].phi)*tan(mkwtmap[i].phi)));
        r = sqrt(x*x + y*y + z*z);

        if (corrs.cflatfield) {
           cos_edge[i] = z/r;        
	   cos_edge[i] += (min(0.15,0.05*fabs(z/x))*fabs(x/r) +
		           min(0.15,0.05*fabs(z/y))*fabs(y/r));
        } else {             
            cos_edge[i] = sqrt( 1.0 / (1.0 + (tan(mkwtmap[i].theta)*(tan(mkwtmap[i].theta)))));
        }
     }
  } else {
     for (i=0;i<max_row;i++) cos_edge[i] = 1.0;
  }

  /* Partial coding correction */
  if (corrs.cpcode) {   
     headas_chat(5, " ...applying partial coding correction...\n");
     for (i=0;i<max_row;i++) {
        pcode_corr[i] = NUM_ELEMENTS;
        for (j=0;j<4;j++) pcode_corr[i] -= mkwtmap[i].shielded[j];
        pcode_corr[i] = NUM_ELEMENTS / pcode_corr[i];
     }
  } else { 
     for (i=0;i<max_row;i++) pcode_corr[i] = 1.0;
  }

  /* Disabled detector correction */
  if (corrs.cnbatdets) {
     headas_chat(5," ...applying disabled detectors correction...\n");
     if (nbaddets == 0) { 
        headas_chat(1,"WARNING:  You have selected the ndets correction without supplying a detector mask:\n");
        headas_chat(1,"   No correction will be applied\n");      
     }
     headas_chat(5, " ...number of disabled detectors = %d...\n",nbaddets);
     for (i=0;i<max_row;i++) {
       /* Calculate the correction for the disabled detectors: */
       pcode_corr[i] *= ((double)(NUM_ELEMENTS)) /
	  ((double)(NUM_ELEMENTS - nbaddets));
     }
  }

  ngooddets = NUM_ELEMENTS - nbaddets;
  fits_update_key(outfptr, TINT, "NBATDETS", &ngooddets,
                  "Number of enabled detectors", &status);
  fits_update_key(outfptr, TINT, "NGOODPIX", &ngooddets,
                  "Number of enabled detectors", &status);

  /* Now figure out the start and stop times of the GTI intervals */
  gti_row=0;
  if (row_count > 0) {
    time_infile = time_outfile_rows[1];
    gti[gti_row].start = time_infile; 
    last_time = time_infile;
    
    for (m = 2; m < row_count; m++) {
      
      time_infile = time_outfile_rows[m];
      if ((time_infile - last_time) > (timedel*1.1)) { 
        gti[gti_row].stop = last_time;
        gti_row++;
        gti[gti_row].start = time_infile;
      } 
      
      last_time = time_infile;
      
    }
    time_infile = time_outfile_rows[row_count];
    gti[gti_row].stop = time_infile;
  }

  /* If the scale factor was INDEF, then we need to compute it based
     on the MET time when this data was taken. */
  if (scale == -1) {
    scale = batmasktaglc_scale_factor(time_infile_rows[0]);
  }
  headas_chat(5, " ...scale factor = %f...\n", scale);

  /* Now loop only over rows that will actually be output */
  for (m = 1; m <= row_count; m++) {
     kk = outfile_row_mkwtmap[m];
     if ((kk < 0) || (kk >=max_row)) {
        correction_applied = 1.0 * scale;
     } else {
       /* HAK 15-Sep-2005  Put the user-supplied scale parameter in
	  here as an overall correction. */
        correction_applied = (pcode_corr[kk]/cos_edge[kk]) * scale;
     }
     time_infile = time_outfile_rows[m];
     fits_write_col(outfptr,TDOUBLE,1,m,1,1, &time_infile,
	                    &status);
     if (status) {
       fprintf(stderr,"ERROR: unable to write time in row %d\n",m);
       goto cleanup;
     }

     /* Get the scale rate for each energy level in each row
        from saved values */
     correct_cts[0] = row_l0_adj_incts[m] * correction_applied;
     correct_cts[1] = row_l1_adj_incts[m] * correction_applied;
     correct_cts[2] = row_l2_adj_incts[m] * correction_applied;
     correct_cts[3] = row_l3_adj_incts[m] * correction_applied;
     for (i=0;i<4;i++) {
        correct_cts[i] /= timedel;
        if ((corrs.cnbatdets) && (detdiv)) correct_cts[i] /= (float)NUM_ELEMENTS;
     }

     /* Write the scaled rate to column 2 of outfile */
     fits_write_col(outfptr,TFLOAT,2,m,1,4,correct_cts,
		                  &status);
    if (status) {
       fprintf(stderr,"ERROR: unable to write RATE values in row %d\n",m); 
       goto cleanup;
    }	   

    /* Get the 4 rate errors for each row from saved values */
    rate_errors[0] = err3_lvl0[m] * correction_applied;
    rate_errors[1] = err3_lvl1[m] * correction_applied;
    rate_errors[2] = err3_lvl2[m] * correction_applied;
    rate_errors[3] = err3_lvl3[m] * correction_applied;
    for (i=0;i<4;i++) {
       rate_errors[i] /= timedel;
       if ((corrs.cnbatdets) && (detdiv)) rate_errors[i] /= (float)NUM_ELEMENTS;
    } 

    /* Write rate errors to column 3 of outfile */
    fits_write_col(outfptr,TFLOAT,3,m,1,4,rate_errors,&status);
    if (status) {
       fprintf(stderr,"ERROR: unable to write RATE errors in row %d\n",m); 
       goto cleanup;
    }	   
    
    /* Get the 4 backgrounds for each row from saved values */
    background[0] = row_l0_bkg[m] * correction_applied;
    background[1] = row_l1_bkg[m] * correction_applied;
    background[2] = row_l2_bkg[m] * correction_applied;
    background[3] = row_l3_bkg[m] * correction_applied;
    for (i=0;i<4;i++) {
       background[i] /= timedel;
       if ((corrs.cnbatdets) && (detdiv)) background[i] /= (float)NUM_ELEMENTS;
    }

    /* Write background to column 4 of outfile */
    fits_write_col(outfptr,TFLOAT,4,m,1,4,background,&status);
    if (status) {
       fprintf(stderr,"ERROR: unable to write BACKGROUND in row %d\n",m); 
       goto cleanup;
    }	   
     
  }  /* End of processing m rows */   

    /* HAK 22-May-2003 Added this call */
    /* Write optional history keywords */
    /* CBM 29 Nov 2003 - changed to new parstamp */
    status = HDpar_stamp(outfptr, 0, &status);

    if (status) goto cleanup;

    fits_update_key(outfptr, TLOGICAL, "PCODEAPP", &(corrs.cpcode),
		  "Partial coding correction applied?", &status);
    fits_update_key(outfptr, TLOGICAL, "FFAPP", &(corrs.cflatfield),
		  "Projection correction applied?", &status);
    if (corrs.ccosine) 
       fits_update_key(outfptr, TLOGICAL, "COSAPP", &(corrs.ccosine),
		    "Cosine correction applied?", &status);
    fits_update_key(outfptr, TLOGICAL, "NGPIXAPP", &(corrs.cnbatdets),
                  "Normalized by number of detectors?", &status);
   
    if (scale != 1.0)
       fits_update_key(outfptr, TDOUBLE, "SCALEAPP", &(scale),
                  "Scale factor correction applied", &status);

    if (outfptr) fits_close_file(outfptr, &status); 
    outfptr = 0;


cleanup:
   if (a) free(a);
   if (row_l0_adj_incts) free(row_l0_adj_incts);
   if (time_infile_rows) free(time_infile_rows);
   if (row_infile) free(row_infile);
   {
     int safestatus = 0;
     if (outfptr) fits_close_file(outfptr, &safestatus);
     safestatus = 0;
     if (infptr1) fits_close_file(infptr1, &safestatus);
     safestatus = 0;
     if (quadfptr) fits_close_file(quadfptr, &safestatus);
   }

   return(status);
}  

/* **************************************************************** */

int write_ebounds
   (char *ebounds,
    char *outfile,
    char *infile)

{

  int status=0;
  fitsfile *infptr=0, *outfptr=0;
  char ebounds_file[PIL_PATH_MAX];

  headas_chat(5,"...Entering write_ebounds...\n");

  fits_open_file(&outfptr,outfile,READWRITE,&status);

  if (strcasecmp(ebounds,"file") == 0 || strcasecmp(ebounds,"infile") == 0) {
    headas_chat(5, " ...copying EBOUNDS from infile...\n");
    strcpy(ebounds_file, infile);
  } else {
    headas_chat(5, " ...copying EBOUNDS from %s...\n", ebounds);
    strcpy(ebounds_file, ebounds);
  }
    
  /* Open the EBOUNDS extension of infile  */
  if (fits_open_file(&infptr,ebounds_file,READONLY,&status) == 0) {
    fits_movnam_hdu(infptr,BINARY_TBL,"EBOUNDS",0,&status);
  }

  if (status) {
    fprintf(stderr,"ERROR: unable to open %s EBOUNDS extension\n",infile);
  } else {
    headas_chat(5," ...copying EBOUNDS HDU...\n");
    /* Copy EBOUNDS ext from infile to outfile */
    fits_copy_hdu(infptr, outfptr, 0, &status); 
  }    

  if (status) {
    fprintf(stderr, "ERROR: Unable to copy EBOUNDS ext to output file. (status=%d)\n",
		status);
  }

  if (outfptr) fits_close_file(outfptr, &status); 
  if (infptr) fits_close_file(infptr, &status); 
  return(status);
}
/* **************************************************************** */

int write_gti
   (char *outfile,
   GTI_STRUCT *gti)

{

  int m,nrow=0,status=0;
  fitsfile *outfptr=0;
  double start,stop;
 
  char *ttype_gti[2];            /* Names of the columns in the
				     binary table in HDU GTI */
  struct gti_struct gti_write;

  headas_chat(5,"...Entering write_gti...\n");

  HDgti_init(&gti_write);
  fits_open_file(&outfptr,outfile,READWRITE,&status); 

  ttype_gti[0] = "START";
  ttype_gti[1] = "STOP";
  
  /* First figure out how many rows to write out */
  for (m=0;m<MAX_GTIROWS;m++) {
     start=gti[m].start;
     stop=gti[m].stop;
     if ((start > 0.0) && (stop > 0.0)) nrow++;
  }

   /* Grow the gti to the number of entries you need */
   HDgti_grow(&gti_write, nrow, &status);
   gti_write.ngti = nrow;

   nrow=0;
   for (m=0;m<MAX_GTIROWS;m++) {
      start=gti[m].start;
      stop=gti[m].stop;
      if ((start > 0.0) && (stop > 0.0)) {
         gti_write.start[nrow]=start;
         gti_write.stop[nrow]=stop;
         nrow++;
      }
   }

   /* Write the GTI */
   headas_chat(5, " ...GTI ngti=%d maxgti=%d start=%d...\n",
	       gti_write.ngti, gti_write.maxgti, gti_write.start ? 1 : 0);
   if (nrow > 0) {
     headas_chat(5, " ...writing GTI extension...\n");
     HDgti_write(outfptr, &gti_write, "GTI",0,0 , &status);
     fits_modify_comment(outfptr, "TTYPE1", "GTI start time", &status);
     fits_modify_comment(outfptr, "TTYPE2", "GTI stop  time", &status);
   } else {
     fprintf(stderr, "WARNING: There were no GTIs.  Is that right?\n");
   }

  if (outfptr) fits_close_file(outfptr, &status); 

  return(status);

}

