#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include "fitsio.h"
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "batmask.h"
#include "batdet.h"
#include "bat_gswdev.h"

static char taskname[] = "batclean";
static char taskver[]  = "3.26";

/*
HISTORY
---------
  Version 1.0 written by Hans Krimm, USRA/NASA-GSFC  21-Mar-2003

  Version 2.0 21-May-2003  This version includes a different singular
  value decomposition code to avoid license problems with Numerical
  Recipes.

  Version 3.0 16-August-2003  Substantially rewritten to include source
  cleaning in addition to background cleaning. Also cleaned up the 
  allocation of large arrays to allow running on systems with smaller
  stack sizes.  Many other changes and bug fixes throughout.

  4-December-2003  Added parameters to support the new "opaque" version 
  of maskwtimg.  Changed the weighting for the detector plane according
  to the Mighell prescription for small data values.

  5-August-2004 Added the ability to read in an error map to be used in
  cases where image statistics are not Poissonian.
  Also use uniform weights if the image has been cleaned and no error map
  is provided.

  8-December-2004 Fixed a bug that was preventing reading detector mask
  tables (as opposed to images).  Also modified code so that it would 
  accept a situation in which half the array was masked out.

  9-December-2004 Added capability to read in and process more than one
  image extension.

  13-December-2004 Fixed a bug so that uncalculated parameters are no longer
  written as keywords.

  29-January-2005 HAK Fixed a bug that was causing the code to crash when 
  there were catalog sources below the threshold for cleaning (cleansnr
  parameter)

  4-Feb-2005  HAK Added support for catalog extension to be named
  TRIG_SOURCE as well as BAT_CATALOG

  24-Feb-2005 HAK Changed response of the code for the case in which an input 
  catalog is provided, but no sources have SNR>cleansnr.  The code now sets
  (internally) the parameter srcclean=0 and proceeds as if no input catalog
  had been provided.  A warning message is ouput to STDERR. Formerly the 
  code exited at that point and produced no output file. 
  -- Fixed bug in code that was handling sources with CATNUM=0 improperly.
  Sometimes CATNUM=0 is valid.
  -- Added "ignore" parameter to allow the user to request that a particular
  source be ignored in the cleaning process (i.e not cleaned regardless of 
  SNR).  The default is "UNKNOWN."  Other options are "NONE" or a particular
  source name.  In a future modification, the user will be able to specify a
  file listing sources to be ignored.

  6-Mar-2005 HAK Added the expand_item_list() capability to the "ignore" 
  parameter so that one can now specify a comma-separated list or input
  table preceded by @.
  -- Reduced the number of standard background parameters from 18 to 14
  This corrects an old error in which the edges of the top and bottom 
  parts of the array were fit separately.  

  8-Mar-2005 HAK Added the ability for a user to supply one or more
  background models to be fit.  The default is for these to be fit along
  with the simple model, but the user can choose to have only his model fit.
  -- Added balancing to the code.  There is a new "balance" parameter which
  causes the code to set the average of all detectors or selected subsets
  to zero.  If this is applied, then the BALAPP keyword is set to true.
  -- Cleaned up various other things in the code.

  10-Mar-2005 HAK Fixed code so that it doesn't do the balancing if the
  user requests that the model be output (instead of the cleaned focal
  plane.
  -- Supress error messages if the user selects bkgmodel="none"
  -- Allow user to do either clean first (default) or balance first using
  the "balfirst" parameter
  -- Fixed bug so that code will clean all images in the input file 
  regardless of their order in the file (skips EBOUNDS and GTI) extensions.
  -- Fixed bug that was not cleaning and balancing multiple images
  correctly
  -- Moved some pieces of code around and streamlined some sections
  -- Added lines of code to free malloc-ed arrays.
  -- Removed or changed to chatter=5 some debugging lines.

  14-Mar-2005 HAK Fixed a few bugs that arose due to the complicating
  branching now allowed with cleaning and balancing.
  -- Made the chatter=3 or 4 more concise and less verbose.
  -- Fixed bug in applying the "leadedge" fit correction

  17-Mar-2005 HAK Added CLEANLEV keyword to keep track of how many times 
  the DPI has been cleaned.  The keyword value is incremented each time the
  code is run.
  -- Added code to update the  HDUCLAS2 keyword to indicate which 
  outversion is requested.  Details in help file.

  11-Apr-2005 HAK Modified code so that balancing can be done even when
  the outversion="fit".  See comments in balance() routine for details.
  An inverse of the balancing done to the "cleaned" DPI is done to the
  "fit" DPI.

  11-May-2005 CBM - Version 3.7.  Two bug fixes:

  1. A vector 'SNR' column in the input caused batclean to silently
     produce the wrong results, since it is not expecting a vector.
     (Bugzilla #175)

     The fix checks for a vector column and condenses the
     vector values down to a single value per row.

  2. batclean now handles rate images.  The fixed code checks
     for the proper keywords for a rate map instead of a counts map,
     (the BUNIT keyword) and scales the error bars according to 
     the exposure.

  23 Jun 2005 CBM - Version 3.8. Check for null values in SNR.

  24 Jun 2005 CBM - Version 3.9. 
    * fix references to uninitialized memory
    * fix memory overrun (in svd())
    * deallocate memory at the end of use

  28 Jun 2005 CBM - Version 3.10. 
    * Increase memory allocations for strings
    * Defend against null pointers

  28 Jul 2005 CBM - Version 3.11.
    * add 'snrcol' parameter so user can choose which
      column to use for S/N thresholding

  06 Aug 2005 CBM  - Version 3.12.
   * enlarge the name string and make it automatic, to avoid memory
     corruption when the input table string is too large.
   * some clean-ups of simple string manipulations
   * make focal_placeholder[][] static, in parallel with focal[][]

  25 Aug 2005 CBM - Version 3.13.
   * do not blindly skip the last two extensions (assumed to be GTI
     and EBOUNDS extensions); instead, check each extension and ignore
     the ones that are not image HDUs.

  3 Oct 2005 HAK - Version 3.14.
   * modified the calls to fits_read_pix when reading in user-supplied
     background models.  These are now read in as DOUBLE to be compatible
     with the overall model structure.  Also NULL values are assigned a
     value of zero.
   * Fixed a bug in the way that chi-squared in the SVD fit was being
     calculated

   12 Dec 2006 HAK 
   * Fixed a bug -- the code now zeroes out masked detectors for user-
     supplied models and leading edge detectors.  It was already doing 
     this for the standard background and source models.   

   13 Dec 2006 HAK - Version 3.15
   * Added code so that the input catalog can be named "INPUT_CATALOG"
     in addition to "BAT_CATALOG" and "TRIG_SOURCE"
   * Added a new parameter "maskfit" which causes the code to output the
     model fit for masked detectors.  The default remains that the is output
     only for non-masked detectors.

   03 Apr 2007 CBM - Version 3.16
   * Added calls to fits_set_hdustruct() for safety against CFITSIO 
     taking a dump on the file structure.

   27 Aug 2007 CBM - Version 3.17 
   * Internal modifications to make task more robust regarding the
     capitalization of strings; simplify using strcasecmp() instead of
     strcmp().

   20 Oct 2008 CBM - Version 3.18
   * Correct BUG in handling of 'Edges' and 'flight' balancing; Edges
     was meant to do ShortEdges+LongEdges, but instead did just outer
     sandwich edges.  This also affected 'flight' since it used the
     'Edges' method.  Now the correct method is used explicitly.

   31 Oct 2008 CBM - Version 3.19
   * No longer require any special extension name for the input catalog.

   21 Aug 2009 CBM - Version 3.20
   * Removed bug when incatalog source at CATNUM=-999
   * Significant internal clean-ups

   12 Jan 2010 CBM - Version 3.21
   * Resolved bugs when bkgmodel=@file and file contains useronly, simple, etc

   15 Jan 2010 CBM - Version 3.22

   * Raise hard-coded limit from 20 sources to 200 sources.  This only
     applies to 'backexp' file, so a bug is rarely triggered. 

   26 Feb 2010 HAK - Version 3.23
   * Fixed two bugs in the code.  First is to correct the behavior when the 
     parameter balfirst=YES -- fixed code so that balancing is actually done
     in the correct order (previously no balancing was done in this scenario)
     Second is to correct the behavior when the parameter maskfit=YES -- 
     fixed code so that the source part of the model is output for masked
     detectors
   * Added two new options to the outversion parameter:  srcfit and 
     srccleaned for the source-only parts of the model 

   20 Jan 2011 CBM - Version 3.24
   * Major restructuring to replace repetitive code with function calls
   * Be very explicit about the preprocessing of data/errors before fitting
     (before some cases were mixed together)
   * This version still has two bugs: (1) a bug relating to how
     statistical weighting of Poisson data is performed; it is done
     incorrectly for rate data; and (2) a bug relating to how
     balancing is done: it's done on the weighted data which is
     incorrect and leads to an incorrect bias in the output maps.
   * The purpose of checking in this version is to clear out the
     structural changes first; I have verified that in every case this
     code produces the same result as the previous version.

   20 Jan 2011 CBM - Version 3.25
   * Additional restructuring so that less use of confusing variable names:
     now focal counts, fit map, model map and residual map are kept separate.
     This also simplifies the internal logic of the code.
   * fix bug in "original" output which was being balanced when it shouldn't
     be according to documentation.
   * full test suite continues to test OK.
   * previously mentioned bugs from version 3.24 still exist.

   22 Mar 2011 CBM - Version 3.26
   * internal logic bug fix release, for two bugs noted in version 3.24 
     release notes.
   * Poisson data was not weighted correctly for rate data, now fixed
   * Balancing was done incorrectly resulting in a bias of ~1 count per
     pixel in the output maps, now fixed
   * Analysis was done on approximately 3 months of BAT survey data,
     which showed a small improvement in noise levels, potentially
     10-25% depending on the energy band.  Significances for bright
     sources increased slightly (~1-2%).  The total improvement, if
     any, will depend on the spectrum of the source, the total background
     rate, any other nearby bright sources.
   * Regardless of the presence of an improvement or not, no large
       *problems* in the data were noted with this change.
   * Unit tests were revised to account for the new task behavior.

 */

/* HAK 6-Mar-2005 Reduced this number from 14 to 18 (see batclean code) */
#define NUM_BACKGROUND_ELEMENTS  14
#define DETECTOR_SIZE            0.42  /* cm */
#define MAX_SOURCES              500

#define NUM_ROWS (DAP_ROWS)
#define NUM_COLS (DAP_COLS)
#define NUM_SPACES (DAP_CELLS)

#define TOOLSUB batclean
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Structure for passing all PIL parameters to the main routine. */
typedef struct {
   char infile[PIL_PATH_MAX];
   char outfile[PIL_PATH_MAX];
   char incatalog[PIL_PATH_MAX];
   char aperture[PIL_PATH_MAX];
   char detmask[PIL_PATH_MAX];
   char wtmapin[PIL_PATH_MAX];
   char wtmapout[PIL_PATH_MAX];
   char backexp[PIL_PATH_MAX];
   char bkgmodel[PIL_PATH_MAX];
   char **bkgmodels;
   int nbkgmodel;
   int model_simple, model_test, model_useronly, model_none;
   char corrections[PIL_LINESIZE];
   char balance[PIL_LINESIZE];
   char ignore[PIL_PATH_MAX];
   char **ignores;
   int nignore;
   char outversion[PIL_PATH_MAX];
   double eff_edge;
   int srcclean;
   int leadedge;
   int balfirst;
   int maskfit;
   double cleansnr;
   double bat_z;
   char snrcol[FLEN_CARD];
} BATCLEAN_PARAMS;   

typedef struct {
   int id;
   int rows;
   float imx,imy;
   double snr;
   int enable;
   char name[PIL_PATH_MAX];
   double mskimg[NUM_SPACES];
} SOURCE_STRUCT;

typedef struct {
   int eBTAll;     /* Balance full DAP (enabled detectors only).  Useful if you do no other balanacing or cleaning */
   int eBTModule;
   int eBTInOut;
   int eBTEdges;
   int eBTLongEdges;
   int eBTShortEdges;
} eBalanceTypes;


/* Global variable declarations */
static double backexp[NUM_BACKGROUND_ELEMENTS][NUM_SPACES]; 
static double backexp_save[NUM_BACKGROUND_ELEMENTS][NUM_SPACES]; 
static float focal[NUM_ROWS][NUM_COLS];
static float modelmap[NUM_ROWS][NUM_COLS];
static float fitmap[NUM_ROWS][NUM_COLS];
static float residmap[NUM_ROWS][NUM_COLS];
static float detmask[NUM_ROWS][NUM_COLS];
static float weights[NUM_SPACES];

/* Function prototypes */
int batclean(void);
int batclean_getpar (BATCLEAN_PARAMS *params);
int batclean_printsummary (BATCLEAN_PARAMS *params,long int *nevt,char *taskname,char *version);
int classbydet(double [NUM_BACKGROUND_ELEMENTS][NUM_SPACES]); 
int forward_project(char *, SOURCE_STRUCT *,double *, double *, char *);
void run_fitting_test(void);
int write_backexposure(fitsfile *,double **,float [],float [], int, int, int, char *);
int write_fit_params(fitsfile *,int *, int ,int,int,float *,float *, SOURCE_STRUCT *, SOURCE_STRUCT *);
void fit_model(double **, float [], float[], int, int, float [],float *,float *);
int balance(float [], float [], float [], char *);
int is_units_rate(char *unitstr);
int mighell_1999(int is_rate, float expo, float cr, float *y, float *ey);
int linsum(int n, float *img, 
	   int addsub, int ncoeff, float *coeff, double **templ);

/*-------------------------------------------------------------*/
int batclean(void)
{
  int status = 0,safestatus = 0;
  
  long int i,k;
  long int nevt=0; /* Number of events in input file*/
  int j,jj;
  int topc=0,botc=0;
  int numhdus,hducnt,hdutype,imghducnt;
  int goodval,cleanlev=0;
  int backapp=0, balapp=0; 
  int flgcol;
  
  fitsfile *infptr = 0, *detptr = 0, *wrtptr = 0, *modptr = 0;
  fitsfile *bkexptr = 0, *wtmapoptr = 0, *wtmapiptr = 0;
  char creator[FLEN_CARD];
  
  float *cvm;
  float *pfocal = 0;
  float *pmodelmap = 0, *pfitmap = 0, *presidmap = 0;
  float *pdetmask;
  char write_map = 'X'; /* Decides output image type */
  
  long fpixel[2] = {1,1};
  
  BATCLEAN_PARAMS params;
  
  float chisq;
  float *afoc;
  int ndata,ma;
  double **xstar;
  double **xstar_save;
  double **ptemplate;
  
  int xkey,ykey;
  long int nrows;
  int nsources = 0;
  int nbkgterms = 0, nfitterms = 0;

  SOURCE_STRUCT *incat = 0;
  SOURCE_STRUCT *source = 0;
  SOURCE_STRUCT *leadedge = 0;
  SOURCE_STRUCT *source_save = 0;
  SOURCE_STRUCT *leadedge_save = 0;
  
  SOURCE_STRUCT *model_dpi = 0;
  SOURCE_STRUCT *model_dpi_save = 0;
  
  char *model;

  int nbaddets = 0;
  int cleaned=0, nusermodels=0, model_flag=0;
  int nosimple=0,dobalance=1,simplemodel=0;
  long dim_size[2];
  
  int typecode;
  long int n_snr, width;
  char imageunits[FLEN_CARD] = "";
  double expo = 1.0;
  int is_rate = 0;
  
  int zeronullval=0; 
  int zeroanynull;
  
  /* ================================================================ */
  /* Register taskname and version. */
  set_toolname(taskname);
  set_toolversion(taskver);
  /* CREATOR keyword */
  sprintf(creator, "%s %s", taskname, taskver);
  
  /*  get input parameters */
  headas_chat(5, "...reading task parameters...\n");
  status=batclean_getpar(&params);
  if (status) {
    fprintf(stderr,"Error in reading parameters\n");
    return status;
  }
  
  
  /* ================================================================ */
  /* CREATE THE OUTPUT FILE */
  headas_chat(5, "...creating output file...\n");
  headas_clobberfile(params.outfile); 
  if (fits_create_file(&wrtptr,params.outfile,&status)) goto cleanup;
  
  /* Fill the backexp array -- See description in the subroutine */
  headas_chat(5, "...calling classbydet...\n");
  status = classbydet(backexp);   
  if (params.maskfit == 1) status = classbydet(backexp_save);   
  
  /* ================================================================ */
  /* OPEN AND PROCESS THE INPUT CATALOG FILE */
  if (params.srcclean == 1) {
    int namcol, imxcol, imycol, catcol, snrcol;
    
    /* Read input source catalog file */
    
    headas_chat(5, "...opening catalog file...\n");
    if (fits_open_data(&infptr, params.incatalog, READONLY, &status)) {
      fprintf(stderr, "ERROR: srcclean=YES, but could not open input catalog '%s'\n",
	      params.incatalog);
      goto cleanup;
    }
    
    /* Get the column numbers and read in the data from the catalog */
    if (fits_get_num_rows(infptr,&nrows,&status)) goto cleanup;
    headas_chat(5,"    (%d rows)\n", nrows);
    
    incat = (SOURCE_STRUCT *) malloc( nrows*sizeof(SOURCE_STRUCT) );
    if (incat == 0) {
      fprintf(stderr, "ERROR: could not allocate storage for incat\n");
      return MEMORY_ALLOCATION;
    }
    
    /* Look for a BAT_ZOBJ keyword.  This tells the source Z-distance
       if the source is at a finite distance. */
    fits_write_errmark();
    fits_read_key(infptr, TDOUBLE, "BAT_ZOBJ", &(params.bat_z),
		  NULL, &status);
    headas_chat(5,"   (BAT_Z = %f; status=%d)\n", params.bat_z, status);
    status = 0;
    fits_clear_errmark();
    
    /* Read in the input catalog */
    fits_get_colnum(infptr,CASESEN,"NAME",&namcol,&status);
    fits_get_colnum(infptr,CASESEN,"IMX",&imxcol,&status);
    fits_get_colnum(infptr,CASESEN,"IMY",&imycol,&status);
    fits_get_colnum(infptr,CASESEN,"CATNUM",&catcol,&status);
    if (status) {
      status = 0;
      fits_get_colnum(infptr,CASESEN,"SOURCE_ID",&catcol,&status);
    }   
    fits_get_colnum(infptr,CASESEN,params.snrcol,&snrcol,&status);
    fits_get_coltype(infptr,snrcol,&typecode,&n_snr, &width, &status);
    
    if (status) {
      fprintf(stderr, "ERROR: could not find required columns NAME, IMX, IMY, CATNUM, %s\n",
	      params.snrcol);
      return status;
    }
    
    /* Read complete input catalog */
    headas_chat(2,"\n\n");
    headas_chat(2,"    %36s  %1s %5s  %8s   %8s %8s \n",
		"NAME", " ", "CAT#", "SNR", "IMX", "IMY");
    headas_chat(2,"    %36s  %1s %5s  %8s   %8s %8s \n",
		"-----------------------------------", " ", "-----", 
		"--------", "--------", "--------");
    
    nsources = 0;
    for (i=0; i<nrows; i++) {
      char *pname = incat[i].name;
      fits_read_col(infptr, TFLOAT, imxcol, i+1, 1, 1, NULL, &(incat[i].imx), 0, &status);
      fits_read_col(infptr, TFLOAT, imycol, i+1, 1, 1, NULL, &(incat[i].imy), 0, &status);
      fits_read_col(infptr, TINT,   catcol, i+1, 1, 1, NULL, &(incat[i].id),  0, &status);
      fits_read_col(infptr, TSTRING,namcol, i+1, 1, 1, 0,    &(pname),        0, &status);
      
      /* Read SNR column which potentially is a vector: find maximum SNR for each
	 row. */
      incat[i].snr = 0;
      for (j = 0; j<n_snr; j++) {
	double snrj = -1e10;
	fits_read_col(infptr, TDOUBLE,snrcol, i+1, j+1, 1, NULL, &(snrj), 0, &status);
	if (snrj > incat[i].snr) { incat[i].snr = snrj; }
      }
      
      /* Enable or disable this entry... */
      incat[i].enable = 1;
      /* ... according to name (using the 'ignore' parameter) ... */
      for (j=0; j<params.nignore; j++) {
	if (strcmp(params.ignores[j],incat[i].name) == 0) incat[i].enable = 0;
      }
      /* ... according to SNR ... */
      if (params.cleansnr != 0 && incat[i].snr < params.cleansnr) incat[i].enable = 0;
      
      if (incat[i].enable) nsources ++;
      
      headas_chat(2,"SRC:%36s  %1s %5d  %8.2f   %8.4f %8.4f \n",
		  incat[i].name, 
		  incat[i].enable ? "+" : " ", incat[i].id, incat[i].snr,
		  incat[i].imx, incat[i].imy);
    }
    headas_chat(2,"   -- Keeping %d clean sources (marked with '+') --\n",
		nsources);
    
    
    if (nsources == 0) {
      headas_chat(1,
		  "NOTE: no valid catalog sources.\n"
		  "      Proceeding to clean background only\n");
      params.srcclean = 0;
    }
    
    if (params.srcclean == 1) {
      source = (SOURCE_STRUCT *) malloc(nsources*sizeof(SOURCE_STRUCT));      
      if (params.maskfit == 1) {
	source_save = (SOURCE_STRUCT *) malloc(nsources*sizeof(SOURCE_STRUCT));            
      }
      j = 0;
      for (i=0; i<nrows; i++) {
	if (incat[i].enable) { 
	  source[j] = incat[i];
	  source[j].rows = nsources;  /* ??? */
	  j++;
	}
      }
      
      /* Now fill the source forward projection array */
      headas_chat(5,"...forward_projecting...\n");
      status = forward_project(params.aperture, source, &params.bat_z,
			       &params.eff_edge, params.corrections);
      if (params.maskfit) {
	for (i=0; i<nsources; i++) {
	  source_save[i] = source[i];
	}
      }
    }
    
    fits_close_file(infptr, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not read image data from %s\n", 
	      params.infile);
      return status;
    }
    
    if (params.leadedge) {
      leadedge = (SOURCE_STRUCT *) malloc(nsources*sizeof(SOURCE_STRUCT));  	  
      if (params.maskfit == 1) {
	leadedge_save = (SOURCE_STRUCT *) malloc(nsources*sizeof(SOURCE_STRUCT));
      }
      
      for (i=0;i<nsources;i++) {
	leadedge[i] = source[i];
	
	/* Now figure out which are the leading edge detectors.
	   This is based simply on which quadrant the source is
	   in. The "key" numbers are which components of the 
	   background represent the leading edge for each 
	   quadrant.  Note that there are two separate background
	   components related to y-leading edges.*/
	if (leadedge[i].imx >= 0.0) xkey=8; else xkey=6;
	if (leadedge[i].imy >= 0.0) ykey=10; else ykey=12;
	for (k=0;k<NUM_SPACES;k++) {
	  leadedge[i].mskimg[k] = 
	    source[i].mskimg[k]*backexp[xkey][k] +
	    source[i].mskimg[k]*backexp[ykey][k];
	  /* Don't double count the corners */
	  if (leadedge[i].mskimg[k] == source[i].mskimg[k]*2.0) 
	    leadedge[i].mskimg[k] /= 2.0;
	  if (leadedge[i].mskimg[k]) source[i].mskimg[k] = 0.0; 
	} 
	
	if (params.maskfit) {
	  leadedge_save[i] = leadedge[i];
	}
      }
      
    } /* end leadedge processing */
    
  }
  /* END PROCESS INPUT CATALOG */
  
  
  /* ================================================================ */
  /* BEGIN PROCESS INPUT USER BACKGROUND MODELS */
  /* Now if there are user supplied background models, open the files 
     containing them.  If the file doesn't exist we need to be able to 
     ignore it, but we don't want the code to crash or the program to stop */
  /* First have to cycle through and add up all the real user-supplied models */
  params.model_simple = 0;
  params.model_test = 0;
  params.model_useronly = 0;
  params.model_none = 0;
  headas_chat(5, "...checking models...\n");
  for (i=0;i<params.nbkgmodel;i++) {
    model = params.bkgmodels[i];
    
    /* These are reserved model names and should not be parsed as files */
    if      (strcasecmp(model,"SIMPLE") == 0) { params.model_simple = 1; } 
    else if (strcasecmp(model,"TEST") == 0)   { params.model_test   = 1; }
    else if (strcasecmp(model,"USERONLY") == 0){params.model_useronly = 1; }
    else if (strcasecmp(model,"NONE") == 0)   { params.model_none   = 1; }
    else {
      
      /* Otherwise, it must be a file name */
      long naxes[2];
      headas_chat(5, "...file=%s\n", model);
      fits_open_image(&modptr, model, READONLY, &status);
      if (status) {
	fprintf(stderr, "ERROR: could not open user model image %s\n", model);
	return status;
      }
      fits_get_img_size(modptr, 2, naxes, &status);
      if (status == 0) {
	if (naxes[0]*naxes[1] < NUM_ROWS*NUM_COLS) {
	  fprintf(stderr, 
		  "ERROR: user model image does not have proper dimensions\n"
		  "       (expected %dx%d; found %ldx%ld)\n"
		  " file=%s\n",
		  NUM_COLS, NUM_ROWS, naxes[0], naxes[1],
		  model);
	  return BAD_DIMEN;
	}
      }
      fits_close_file(modptr,&status); 
      
      /* OK, passed the test, count it */
      nusermodels ++;
    }
  }
  headas_chat(5, "...%d user model templates...\n", nusermodels);
  
  if (nusermodels > 0) {
    model_flag++;
  } else {
    simplemodel = 1;
  }
  
  if (model_flag) {
    /* Now we can allocate space for the user supplied models */
    model_dpi = (SOURCE_STRUCT *) malloc(nusermodels*sizeof(SOURCE_STRUCT));
    if (params.maskfit) {
      model_dpi_save = (SOURCE_STRUCT *) malloc(nusermodels*sizeof(SOURCE_STRUCT));
    }
    
    headas_chat(2,"\n\n");
    headas_chat(2,"    %3s  %40s\n",
		"#", "Model Name");
    headas_chat(2,"    %3s  %40s\n",
		"---", "----------------------------------------");
    
    /* Next actually fill the appropriate arrays */
    nusermodels = 0;
    for (i=0; i<params.nbkgmodel; i++) {
      model = params.bkgmodels[i];
      
      status = 0;
      
      
      /* These are reserved model names and should not be parsed as files */
      if ((strcasecmp(model,"SIMPLE") == 0) ||
	  (strcasecmp(model,"TEST") == 0) ||
	  (strcasecmp(model,"USERONLY") == 0) ||
	  (strcasecmp(model,"NONE") == 0)) {
	
	headas_chat(2,
		    "MOD:%3s  %s\n",
		    "---", model);
	
      } else {
	
	/* Otherwise it is a file name */
	int safestatus = 0;
	fits_open_image(&modptr, model, READONLY, &status);
	if (status == 0) {
	  fits_read_pix(modptr, TDOUBLE, fpixel, NUM_ROWS*NUM_COLS,
			&zeronullval, model_dpi[nusermodels].mskimg, 
			&zeroanynull, &status);
	}
	if (status == 0) {
	  /* Figure out the model name.  
	     There are two variants here:  One is to take the file name
	     (after stripping off the path) and the other is to search
	     for the MODNAME keyword.  The keyword version overrides. */
	  fits_read_key(modptr, TSTRING, "MODNAME", model_dpi[nusermodels].name, 0, &status);
	  if (status != 0) {
	    char *p;
	    
	    /* Keyword not found, use filename */
	    p = rindex(model, '/');
	    if (p == 0) p = model; else p++;
	    strcpy(model_dpi[nusermodels].name, p);
	    status = 0;
	  }
	}
	
	if (params.maskfit && status == 0) {
	  model_dpi_save[nusermodels] = model_dpi[nusermodels];
	}
	
	if (status == 0) {
	  headas_chat(2,"MOD:%3d  %s\n",
		      nusermodels+1, model_dpi[nusermodels].name);
	  nusermodels ++;
	}
	
	
	/* Close the file safely */
	fits_close_file(modptr, &safestatus);
      }
    }
    
    headas_chat(2,"   -- Using %d user model templates --\n", 
		nusermodels);
    
  }
  /* END PROCESS INPUT USER BACKGROUND MODELS */
  
  
  /* ================================================================ */
  /* Read input error map file */
  if (params.wtmapin[0] != 0) {
    headas_chat(5, "...reading input error map...\n");
    fits_open_image(&wtmapiptr, params.wtmapin, READONLY, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not open %s\n", params.wtmapin);
      return status;
    }
    
    fits_read_pix(wtmapiptr, TFLOAT, fpixel, NUM_SPACES,0,weights,0, &status);
    fits_close_file(wtmapiptr,&status); 
  }
  /* End read input error map file */
  
  
  /* ================================================================ */
  /* BEGIN READ DETECTOR MASK FILE */
  if (params.detmask[0] != 0) {
    headas_chat(5, "...reading detector mask image...\n");
    fits_open_image(&detptr, params.detmask, READONLY, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not open %s\n", params.detmask);
      return status;
    }
    
    fits_read_pix(detptr, TFLOAT, fpixel, NUM_ROWS*NUM_COLS,0,detmask,0, &status);
    /* If it is not an image file, then we need to open it as a table cell. 
       Check for this here. */
    if (status) {
      headas_chat(5, "...reopen as table...\n");
      status = 0;
      fits_close_file(detptr,&safestatus);
      if (fits_open_file(&detptr,params.detmask,READONLY,&status)) goto cleanup;           
      /* Move to the correct HDU in the input file. The file must contain
	 a "BAT_FLAGS" extension.*/
      if (fits_movnam_hdu(detptr,BINARY_TBL,"BAT_FLAGS",0,&status)) goto cleanup;
      if (fits_get_colnum(detptr,CASEINSEN,"FLAG",&flgcol,&status)) goto cleanup;
      fits_read_col(detptr,TFLOAT,flgcol,1,1,
		    NUM_ROWS*NUM_COLS,0,detmask,0,&status);   
      
      if (status) {
	fprintf(stderr, "Unable to open the detector mask file: %s\n", params.detmask);
	fits_close_file(detptr,&safestatus);
      }
    }
    
    /* Read GOODVAL keyword.  If the keyword is not found, then assume
       that 1=good, 0=bad.  If the keyword is found, then use that
       value for good, and anything else for bad. */
    safestatus = 0;
    fits_read_key(detptr, TINT, "GOODVAL", &goodval, 0, &safestatus);
    if (safestatus) goodval = 0;
    headas_chat(5,"...GOODVAL=%d...\n", goodval);
    
    /* If goodval is 1 (old format), then switch everything over */
    if (goodval) {
      for (j=0;j<NUM_ROWS;j++) {
	for (i=0;i<NUM_COLS;i++) {
	  if (detmask[j][i]) detmask[j][i]=0;
	  else detmask[j][i]=1;
	}
      } 
    }
    
    safestatus = 0;
    fits_close_file(detptr, &safestatus);
    if (status) {
      fprintf(stderr, "ERROR: could not read detector mask data from %s\n", 
	      params.detmask);
      return status;
    }
  } else {   /* If no detector mask file is specified, fill 
		a default array with all zeros (all good)
		.  Gaps should already
		be zero in the input DPI */
    for (j=0;j<NUM_ROWS;j++) {
      for (i=0;i<NUM_COLS;i++) detmask[j][i]=0.0;
    } 
  }
  /* To put it all into one long array rather than a square array */ 
  pdetmask = &detmask[0][0];
  /* END READ DETECTOR MASK FILE */
  
  
  /* Open the output background exposure map file if requested */
  if (*params.backexp) {
    headas_chat(5, "...back exposure map file...\n"
		"   (%s)\n", params.backexp);
    /* Delete existing file if clobber=YES */
    headas_clobberfile(params.backexp); 
    if (fits_create_file(&bkexptr,params.backexp,&status)) {
      fprintf(stderr,"ERROR: unable to create back exposure file %s (status=%d)\n",
	      params.backexp, status);
    }
  }
  /* End opening the output background exposure map */
  
  
  /* If the user has specified an output error map, then create it here */
  if (params.wtmapout[0]) {
    headas_chat(5,
		"...output weight map...\n"
		"   (%s)\n", params.wtmapout);
    headas_clobberfile(params.wtmapout);
    if (fits_create_file(&wtmapoptr,params.wtmapout,&status)) goto cleanup;
    dim_size[0] = NUM_COLS;
    dim_size[1] = NUM_ROWS;
  }
  /* End creating the output error map */
  


  /* ================================================================ */
  /* Create the focal plane array for use in fitting and balancing */
  pfocal = &(focal[0][0]);
  pfitmap = &(fitmap[0][0]);
  pmodelmap = &(modelmap[0][0]);
  presidmap = &(residmap[0][0]);

  /* Read input detector plane image file */
  headas_chat(5, "...reading input image...\n");
  fits_open_image(&infptr, params.infile, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s\n", params.infile);
    return status;
  }
  fits_get_num_hdus(infptr,&numhdus,&status);
  headas_chat(5,"...Found %d HDUs...\n", numhdus);
  
  /* If there are more than three HDUs in the input (BAT_DPI, EBOUNDS,
     GTI, then we need to read and clean all the input DPIs */
  
  if (numhdus <= 1) numhdus=1; /*If there are no EBOUNDS or GTI */
  hducnt=0;  imghducnt=0;


  
  /* ================================================================ */
  /* THIS IS THE MAIN LOOP -- LOOP THROUGH ALL HDUs, IGNORING NON-IMAGE
     HDUs, CLEANING EACH ONE SEPARATELY */
  while (hducnt < numhdus) { 

    /* Initialize the map-specific arrays */
    for (i = 0; i<NUM_SPACES; i++) {
      pfocal[i] = pfitmap[i] = pmodelmap[i] = presidmap[i] = 0;
    }
  
    headas_chat(2,"\n-------------------\n");
    headas_chat(2,"  Image %d\n", hducnt);
    fits_open_image(&infptr, params.infile, READONLY, &status);
    if (fits_movabs_hdu(infptr,hducnt+1,&hdutype,&status)) goto cleanup; 
    /* Skip non-image HDUs */
    if (hdutype != IMAGE_HDU) {
      headas_chat(2,"  (skipping non-image HDU)\n");
      safestatus = 0;
      fits_close_file(infptr, &safestatus);
      hducnt++;
      continue;
    }
    
    fits_read_key(infptr, TLOGICAL, "BACKAPP", &cleaned, 0, &status);
    if (status) {
      cleaned = 0;
    }
    headas_chat(5, "   (BACKAPP=%d; status=%d)\n", cleaned, status);
    status = 0;
    
    fits_read_key(infptr, TSTRING, "BUNIT", imageunits, 0, &status);
    /* CM -- read BUNIT keyword to decide whether to exposure correct */
    if (status) {
      fprintf(stderr, "WARNING: Assuming image units are counts\n");
      strcpy(imageunits,"count");
      status = 0;
    }
    headas_chat(2, "   BUNIT=%s\n", imageunits);
    
    /* CM -- read exposure keyword to get statistics right */
    if (is_units_rate(imageunits)) {
      /* Rate data, read exposure */
      is_rate = 1;

      fits_read_key(infptr, TDOUBLE, "EXPOSURE", &expo, 0, &status);
      if (status) {
	fprintf(stderr, "WARNING: Assuming exposure is 1.0 seconds\n");
	expo = 1.0;
	status = 0;
      }
    } else {
      /* Counts data, use default exposure of 1.0 seconds */
      expo = 1.0;
      is_rate = 0;
    }
    headas_chat(2, "   EXPOSURE=%f %s\n", expo,
		is_rate ? "(rate data)" : "(ignored for count data)");
    
    headas_chat(5, "...reading image pixel data...\n");
    fits_read_pix(infptr, TFLOAT, fpixel, NUM_ROWS*NUM_COLS,0,focal,0, &status);
    headas_chat(5, "   (done; status=%d)\n", status);
    fits_copy_hdu(infptr,wrtptr,0,&status); 
    
    fits_close_file(infptr, &safestatus);
    if (status) {
      fprintf(stderr, "ERROR: could not read image data from %s (%d)\n", 
	      params.infile,status);
      return status;
    } else {
      hducnt++;
      imghducnt++;
    }
    
    /* Update the CREATOR keyword for this task */
    fits_update_key(wrtptr, TSTRING, "CREATOR", creator, 
		    "Name of creating task", &status);
    
    
    /* ================================================================ */
    /* Preparation of counts/rate array for fitting */
    
    /* Now zero out bad detectors in the focal plane */
    /* Also need to zero the model detector planes */
    /* While we are looping fill the weights array */
    if (params.wtmapin[0] != 0) {
      headas_chat(3,"...using weights from file...\n"
		  "   (%s)\n",params.wtmapin);
    }
    nbaddets = NUM_ELEMENTS-NUM_SPACES;
    if (params.detmask[0] == 0) nbaddets = 0;

    for (i=0;i<NUM_COLS*NUM_ROWS;i++) {

      /* Apply mask to all detectors and templates */
      if (pdetmask[i] > 0) {
	nbaddets ++;
	pfocal[i] = 0;

	for (k=0;k<NUM_BACKGROUND_ELEMENTS;k++) backexp[k][i] = 0;
	if (params.srcclean ==1) {
	  for (k=0;k<nsources;k++) source[k].mskimg[i] = 0;
	}
	if (params.leadedge ==1) {
	  for (k=0;k<nsources;k++) leadedge[k].mskimg[i] = 0;
	}
	if (model_flag) {
	  for (k=0;k<nusermodels;k++) model_dpi[k].mskimg[i] = 0;
	}
      }
    }	
    headas_chat(2,"   Number of ignored pixels:     %d\n", nbaddets);

    /* Default focal plane values and weights */
    for (i=0; i<NUM_COLS*NUM_ROWS; i++) {
      pfitmap[i] = pfocal[i];
      weights[i] = 1.0;
    }

    /* If no input weight map was supplied, then compute the 
       weights based on Poisson statistics and the Mighell 1999
       prescription */
    
    /* No input weight map and input map has already been cleaned:
       --> uniform weighting of 1.0 for all pixels. */
    if (params.wtmapin[0] == 0 && cleaned) {
      fprintf(stderr, 
	      "WARNING: Cleaning a pre-cleaned image using uniform weights\n");
      
      /* No input weight map and input is counts / rate
	 --> Use Mighell 1999 prescription for Poisson data */
    } else if (params.wtmapin[0] == 0) {


      for (i=0; i<NUM_COLS*NUM_ROWS; i++) {
	float cr = pfocal[i];
	mighell_1999(is_rate, expo, cr, &(pfitmap[i]), &(weights[i]));
      }

      /* Weight map supplied  */
    } else if (params.wtmapin[i]) {
      /* --> Do nothing: we already have an input weight map */
      for (i=0; i<NUM_COLS*NUM_ROWS; i++) {
	pfitmap[i] = pfocal[i];
      }
    } 
    
    /* Write the output error map if requested */
    if (params.wtmapout[0]) {
      fits_create_img(wtmapoptr,FLOAT_IMG,2,dim_size,&status);
      if (fits_movabs_hdu(wtmapoptr,imghducnt,&hdutype,&status)) goto cleanup; 
      if (status) {
	fprintf(stderr,"ERROR:  Could not write output error map %s\n",params.wtmapout);
	goto cleanup;
      }
      
      fits_write_pix(wtmapoptr, TFLOAT, fpixel, NUM_SPACES,weights, &status);
      if (status) {
	fprintf(stderr,"ERROR:  Could not write output error map %s\n",params.wtmapout);
	goto cleanup;
      }
      fits_update_key(wtmapoptr,TSTRING, "EXTNAME", "ERRMAP", NULL, &status);
    }
    
    
    /* ================================================================ */
    /* Pre-fit balancing */
    /* If requested that balancing happen before cleaning, do the
       balancing here, unless balance="none" */
    /* HAK 9-Mar-2005  Also add that we don't balance if the user has 
       requested that the output file be the fit file */
    if (strstr(params.balance,"none")) dobalance=0;
    if (strstr(params.balance,"NONE")) dobalance=0;
    headas_chat(5,"   (dobalance=%d)\n", dobalance);
    
    /* Fixed this conditional so that it is met in the proper situation -- HAK 25-Feb-2010 */
    if ( (dobalance) && (params.balfirst) &&
	 ( strcasecmp(params.outversion,"fit") ||
	 strcasecmp(params.outversion,"bkgfit") ||
         strcasecmp(params.outversion,"srcfit")) ) {
      headas_chat(5,"...balancing before fit...\n");
      status = balance(pfitmap,pmodelmap,pdetmask,params.balance);
      headas_chat(5,"   (done; status=%d)\n", status);
      
      /* Write a keyword indicating that the image has been balanced */
      if (!(status)) {
	balapp=1; 
      }
      status=0;
      fits_update_key(wrtptr, TLOGICAL, "BALAPP", &balapp,
		      "Array balancing applied",&status);
    }
    
    /* If bkgmodel = "test" then go to a simple debugging test of the code */ 
    if (params.model_test) {
      strcpy(params.outversion,"original"); 
      run_fitting_test();
    }
    
    if (params.model_useronly) {
      nosimple=1;
      simplemodel=0;
    }
    
    if (params.model_none) {
      nusermodels=0;
      simplemodel=0;
    }
    headas_chat(5,"   (nosimple=%d; simplemodel=%d; nusermodels=%d)\n",
		nosimple, simplemodel, nusermodels);
    

    /* ================================================================ */
    /* START MAIN CLEANING PROCESS */
    /* Here is where we do the real work.  Set up and call the fitting routine. */
    if (params.model_simple || nusermodels || simplemodel) {
      
      ndata = NUM_SPACES;
      if (params.srcclean != 1) nsources=0;

      /* Add up the total number of fitted parameters */
      nfitterms = 0;

      if (!nosimple) nfitterms += NUM_BACKGROUND_ELEMENTS; /* ... + std bkg terms ... */
      nfitterms += nusermodels;                            /* ... + user templates ... */
      nbkgterms = nfitterms;                               /* ... = total "bkg" terms */

      if (params.srcclean) nfitterms += nsources;          /* ... + source terms */
      /* Add an extra parameters for each source if the flag is set
	 to fit leading edge detectors separately. */
      if (params.leadedge) nfitterms += nsources;          /* ... + leading edge terms */

      ma = nfitterms;

      headas_chat(5,"   (NDATA=%d; MAX=%d; NSOURCE=%d)\n", ndata, ma, nsources);
      afoc = (float *) malloc((unsigned) ma*sizeof(float));
      xstar      = (double **) malloc((unsigned) ma*sizeof(double*));
      xstar_save = (double **) malloc((unsigned) ma*sizeof(double*));
      if (nosimple) {
	j=0;
	jj=0;
      } else {

	/* ----- Standard background terms */
	for (i=0,j=0;i<NUM_BACKGROUND_ELEMENTS;i++,j++) 
	  xstar[j]=&backexp[0][0]+ndata*i;
	if (params.maskfit) {
	  for (i=0,jj=0;i<NUM_BACKGROUND_ELEMENTS;i++,jj++) 
	    xstar_save[jj]=&backexp_save[0][0]+ndata*i;
	}
	j=NUM_BACKGROUND_ELEMENTS;
	jj=NUM_BACKGROUND_ELEMENTS;
      }

      /* ----- User requested templates */
      for (i=0;i<nusermodels;i++) {
	xstar[j++] = model_dpi[i].mskimg;
      }
      if (params.maskfit) {
	for (i=0;i<nusermodels;i++) {
	  xstar_save[jj++] = model_dpi_save[i].mskimg;
	}
      } 

      /* ----- Source templates */
      if (params.srcclean == 1) {
	for (i=0;i<nsources;i++) {
	  xstar[j++] = source[i].mskimg;
	  if (params.leadedge) xstar[j++] = leadedge[i].mskimg;
	}
	if (params.maskfit) {
	  for (i=0;i<nsources;i++) {
	    xstar_save [jj++] = source_save[i].mskimg;
	    if (params.leadedge) xstar_save[jj++] = leadedge_save[i].mskimg;
	  }
	} 
      }
      

      /* ================================================================ */
      /* Fitting of data templates with linear coefficients using SVD */
      /* Set up a dummy code here until we write code to find
	 covariance matrix */
      cvm = (float *) malloc((unsigned) ma*ma*sizeof(float));
      
      /* This is the Golub and Reinsch formula */
      headas_chat(5, "...fitting model...\n");
      fit_model(xstar,pfitmap,weights,ma,ndata,afoc,&chisq,cvm);
      headas_chat(5, "   (done)\n");
      headas_chat(2, "   Chi squared (SVD): %.4f\n", chisq);
      
      /* Write the fit parameters as keywords */
      headas_chat(5,"...writing fit parameters...\n");
      status=write_fit_params(wrtptr,&params.leadedge,ma,
			      nosimple ? NUM_BACKGROUND_ELEMENTS : 0,
			      nusermodels,afoc,&chisq,
			      source, model_dpi);
      if (status) {
	fprintf(stderr,"ERROR: Unable to write keywords to %s\n",params.outfile);
	goto cleanup;
      }


      /* ================================================================ */
      /*  Compute Output Values (except for balancing) */
      
      /* The fit template we are using depends on whether the user
	 requests every pixel (including disabled ones (params.maskfit
	 == 1) or just the ones being fitted (params.maskfit == 0) */
      ptemplate = params.maskfit ? (xstar_save) : (xstar);

      /* Initialize the residual map so that we can subtract model components */
      for (i=0;i<NUM_COLS*NUM_ROWS;i++) presidmap[i] = pfocal[i];
      for (i=0;i<NUM_COLS*NUM_ROWS;i++) pmodelmap[i] = 0;


      /* ------ "original" - no change at all */
      if (strcasecmp(params.outversion,"original") == 0) {
	/* If the user wants the output file to be the same as the input,
	   do nothing here. */
	write_map = 'O';  /* Write 'O'riginal map */


	/* ------ "cleaned" - data-model */
      } else if (strcasecmp(params.outversion,"cleaned") == 0) {
	backapp=1;
	headas_chat(5,"...calculating cleaned focal map...\n");
	
	linsum(NUM_COLS*NUM_ROWS, presidmap, -1, nfitterms, afoc, xstar);
	write_map = 'R';  /* Default to write 'R'esidual map */


	/* ------ "fit" = model(bkg+src+user) */
      } else if (strcasecmp(params.outversion,"fit") == 0) {
	headas_chat(5,"...calculating best-fit focal map...\n");

	linsum(NUM_COLS*NUM_ROWS, pmodelmap, +1, nfitterms, afoc, ptemplate);
	linsum(NUM_COLS*NUM_ROWS, presidmap, -1, nfitterms, afoc, ptemplate);
	
	write_map = 'P';  /* Write 'P'redicted map */


	/* ------ "bkgfit" = model(bkg+user) without sources */
      } else if (strcasecmp(params.outversion,"bkgfit") == 0) {
	headas_chat(3,"...calculating background fit only...\n");

	linsum(NUM_COLS*NUM_ROWS, pmodelmap, +1, nbkgterms, afoc, ptemplate);
	linsum(NUM_COLS*NUM_ROWS, presidmap, -1, nbkgterms, afoc, ptemplate);
	
	write_map = 'P';  /* Write 'P'redicted map */


	/* ------ "bkgcleaned" = data - model(bkg+user) */
      } else if (strcasecmp(params.outversion,"bkgcleaned") == 0) {
	headas_chat(5,"...removing only background from focal map...\n");
	
	linsum(NUM_COLS*NUM_ROWS, presidmap, -1, nbkgterms, afoc, xstar);
	write_map = 'R';  /* Default to wzbrite 'R'esidual map */
	
	
	/* ------ "srcfit" = model(src_only) */
      } else if (strcasecmp(params.outversion,"srcfit") == 0) {
	int nterms;
	headas_chat(5,"...calculating source fit only...\n");

	/* Starting position for template values is first source term,
	   which is just beyond last background term */
	ptemplate = params.maskfit ? (&xstar_save[nbkgterms]) : (&xstar[nbkgterms]);
	nterms = nfitterms - nbkgterms;

	linsum(NUM_COLS*NUM_ROWS, pmodelmap, +1, nterms, afoc, ptemplate);
	linsum(NUM_COLS*NUM_ROWS, presidmap, -1, nterms, afoc, ptemplate);

	write_map = 'P';  /* Write 'P'redicted map */


	/* ------ "srccleaned" = data - model(src) */
      } else if (strcasecmp(params.outversion,"srccleaned") == 0) {
	int nterms;
	headas_chat(5,"...removing only sources from focal map...\n");
	nterms = nfitterms - nbkgterms;

	backapp=1;
	/* Starting position for template values is first source term,
	   which is just beyond last background term */
	ptemplate = &xstar[nbkgterms];

	linsum(NUM_COLS*NUM_ROWS, presidmap, -1, nterms, afoc, ptemplate);
	write_map = 'R';  /* Default to write 'R'esidual map */
      }


      
      /* ================================================================ */
      /*  Only remaining analysis step is BALANCING */

      /* HAK 9-Mar-2005  Also add that we don't balance if the user has 
         requested that the output file be the fit file */
      /* HAK 10-Mar-2005 And also do not balance here if the user had 
         requested balancing before cleaning. */
      if (strstr(params.balance,"none")) dobalance=0;
      if (strstr(params.balance,"NONE")) dobalance=0;
      /* With outversion=sourcefit, we want only the source template models
         with no apparent balancing */
      if (strcasecmp(params.outversion,"srcfit") == 0) dobalance = 0;
      headas_chat(5,"   (dobalance=%d)\n", dobalance);
      
      if ( (dobalance) && !(params.balfirst) ) {
	
	headas_chat(5, "...balancing...\n");
	status=balance(presidmap,pmodelmap,pdetmask,params.balance);
	headas_chat(5,"   (done; status=%d)\n", status);
	
	/* Write a keyword indicating that the image has been balanced */
        if (!(status)) {
	  balapp=1; 
	}
	status=0;
	fits_update_key(wrtptr,TLOGICAL,"BALAPP",&balapp,
			"Array balancing applied",&status);
      }

      /* Write the output file and the
	 HDUCLAS2 keyword which indicates the type of output map */
      switch(write_map) {
      case 'R': /* 'R'esidual */
	fits_update_key(wrtptr, TSTRING, "HDUCLAS2", "RESIDUAL",
			" array contains clean residuals", &status);
	fits_write_pix(wrtptr, TFLOAT, fpixel, NUM_COLS*NUM_ROWS, presidmap, &status);
	break;
      case 'P': /* 'P'redicted */
	fits_update_key(wrtptr, TSTRING, "HDUCLAS2", "PREDICTED",
			" array contains fitted model", &status);
	fits_write_pix(wrtptr, TFLOAT, fpixel, NUM_COLS*NUM_ROWS, pmodelmap, &status);
	break;
      case 'O': /* 'O'riginal */
	fits_write_pix(wrtptr, TFLOAT, fpixel, NUM_COLS*NUM_ROWS, pfocal, &status);
	break;
      }
      
      if (status) {
	fprintf(stderr,"ERROR:  Could not write output image %s\n",params.outfile);
	goto cleanup;
      }
      /* Write a keyword indicating that the image has been cleaned */
      fits_update_key(wrtptr,TLOGICAL,"BACKAPP",&backapp,
		      "Background cleaning applied",&status);
      
      headas_chat(5,"...updating CLEANLEV keyword...\n");
      if (backapp) {
	safestatus=0;
	fits_read_key(wrtptr, TINT, "CLEANLEV", &cleanlev, 0, &safestatus);
	if (safestatus) {
	  headas_chat(5,"  (creating CLEANLEV=1 keyword)\n");
	  safestatus=1;  
	  cleanlev=1;
	} else {
	  headas_chat(5,"  (updating CLEANLEV=%d keyword)\n", cleanlev+1);
	  cleanlev++;
	}
	fits_update_key(wrtptr,TINT,"CLEANLEV",&cleanlev,
			"Number of cleanings",&status);
      }
      
      /* Write optional history keywords */
      /* Replaced headas_parstamp with HDpar_stamp  14-Nov-2003  */
      status=HDpar_stamp(wrtptr,0,&status); 
      if (status) goto cleanup;
      
      /* Write the output background exposure map file if requested */
      if (*params.backexp) {
	headas_chat(5,"...writing background exposure map...\n");
	if (fits_movabs_hdu(bkexptr,imghducnt,&hdutype,&status)) goto cleanup; 
	status=write_backexposure(bkexptr,xstar,afoc,
				  cvm,ma,(topc+botc),params.leadedge, creator);
	if (status) {
	  fprintf(stderr,"ERROR: Unable to write backexp map %s\n",params.backexp);
	  goto cleanup;
	}
	headas_chat(5,"   (done)\n");
      }
      free(cvm);
      free(afoc);
      free(xstar);
      free(xstar_save);
      
      /* END MAIN CLEANING PROCESS */
    } else {  
      /* THIS BRANCH IS ONLY IF NO CLEANING IS DONE (BALANCE ONLY OR ERROR) */
      headas_chat(5,"...balance only...\n");
      if ( ! params.model_none ) {
	fprintf(stderr,"WARNING: You must either supply a valid input model or use the SIMPLE background model.\n");
      }
      /* If we are to balance the DAP, then do it, otherwise, just exit. */
      if (strstr(params.balance,"none") || strstr(params.balance,"NONE")) {
	dobalance = 0;
      }
      if ( (strcasecmp(params.outversion,"fit") == 0) ||
	   (strcasecmp(params.outversion,"bkgfit") == 0) ) {
	dobalance = 0;
      }
      headas_chat(5,"   (dobalance=%d)\n", dobalance);
      
      if ( (dobalance) && !(params.balfirst) ) {
	if ( ! params.model_none ) {
	  fprintf(stderr,"  But since you have requested it, batclean will still balance the focal plane\n");
	}
	
	headas_chat(5,"...balancing...\n");
	status=balance(pfocal,pmodelmap,pdetmask,params.balance);
	headas_chat(5,"   (done; status=%d)\n", status);
	
	/* Write a keyword indicating that the image has been balanced */
        if (!(status)) {
	  balapp=1; 
	}
	status=0;
	fits_update_key(wrtptr,TLOGICAL,"BALAPP",&balapp,
			"Array balancing applied",&status);
	
	/* Write the output file*/ 
	fits_write_pix(wrtptr, TFLOAT, fpixel, 
		       NUM_COLS*NUM_ROWS, pfocal, &status);
	if (status) {
	  fprintf(stderr,"ERROR:  Could not write output image %s\n",
		  params.outfile);
	  goto cleanup;
	}
      }
    }
    /* END NO-CLEANING BRANCH */
    
  }
  /* END MAIN LOOP THROUGH THE INPUT IMAGES */
  
  headas_chat(5,"...closing files...\n");
  if (bkexptr) fits_close_file(bkexptr,  &status);
  
  if (params.wtmapout[0]) {
    if (fits_close_file(wtmapoptr,&status)) goto cleanup;
  }
  
  headas_chat(5,"...copying EBOUNDS extension...\n");
  if (fits_open_file(&infptr,params.infile,READONLY,&status)) return(status);           
  /* Copy the EBOUNDS extension to the output file */
  if (fits_movnam_hdu(infptr,BINARY_TBL,"EBOUNDS",0,&status)) {
    headas_chat(5,"   (No EBOUNDS extension)\n");
    status=0;
  } else {
    if (fits_copy_hdu(infptr,wrtptr,0,&status)) goto cleanup;
  }
  headas_chat(5,"   (done)\n");
  
  /* Copy the GTI extension to the output file */
  headas_chat(5,"...copying GTI extension...\n");
  if (fits_movnam_hdu(infptr,BINARY_TBL,"STDGTI",0,&status)) {
    headas_chat(5,"   (No GTI extension)\n");
    status=0;
  } else {   
    if (fits_copy_hdu(infptr,wrtptr,0,&status)) goto cleanup;
  }
  if (fits_close_file(infptr,&status)) goto cleanup;
  
  /* Print out a summary of operations (unless chatter=0) */
  status=batclean_printsummary(&params,&nevt,taskname,taskver);
  headas_chat(5, "...cleaning up...\n");
  if (status) goto cleanup;
  
  /* Free up malloc-ed arrays */
  if (params.srcclean == 1) {
    if (source) free(source);
    if (source_save) free(source_save);
  }
  if (model_flag) {
    if (model_dpi) free(model_dpi);
    if (model_dpi_save) free(model_dpi_save);
  }
  
  goto cleanup;
  headas_chat(5, "...closing output file...\n");
  if (fits_close_file(wrtptr,&status)) goto cleanup;
  wrtptr = 0;
  
  headas_chat(5, "...finishing batclean_work...\n");
  return(status);
  
 cleanup:
  
  headas_chat(5, "...batclean_work cleanup...\n");
  {
    int safestatus = 0;
    if ( wrtptr) fits_close_file(wrtptr, &safestatus);
    wrtptr = 0;
  }
  if (leadedge) free(leadedge);
  if (leadedge_save) free(leadedge_save);
  leadedge = 0;
  
  if (status) fits_report_error(stderr, status);
  return(status);
}


/*-------------------------------------------------------------*/

/* Routine to call the Golub and Reinsch
   SVD program and solve the matrix equation. */

void fit_model  (
		 double **x,
		 float y[],
		 float sig[],
		 int ma,
		 int ndata,
		 float afoc[],
		 float *chisq,
		 float *cvm)

{
  int i,j,k,status;
  double *xx, *yy, *u, *v, *w, *temp, eps=0.0000001;
  double *tempa;
  int nmax=10,mmax=10;
  float sum,tmp;
  int number=5;

  if (ndata < nmax) nmax = ndata;
  if (ma < mmax) mmax = ma;

  if (ma == 3) number=4;

  xx = (double *) malloc((unsigned) (ndata)*(ma)*sizeof(double));
  yy = (double *) malloc((unsigned) (ndata)*sizeof(double));
  for (i=0;i<ndata;i++) {
    yy[i] = y[i] / sig[i];
    for (j=0;j<ma;j++) {   
      xx[i*ma+j]=x[j][i]/sig[i]; 
    }
  }

  u = (double *) malloc((unsigned) (ndata*ma)*sizeof(double));
  v = (double *) malloc((ndata)*(ma)*sizeof(double));
  w = (double *) malloc((ma)*sizeof(double));
  temp = (double *) malloc((ndata)*sizeof(double));
  tempa = (double *) malloc((ma)*sizeof(double));

  status = svd(xx,ndata,ma,u,w,v,eps,3,temp); 
  headas_chat(5,"   (svd status=%d)\n", status);

  /* Need to guard against NaN's:  if an element of W is too small,
     replace it with a very small number */
  /* for (i=0;i<ma;i++) {
     if (w[i] < MIN_VAL) w[i] = MIN_VAL;
     } */

  /* Now do the fitting. */

  for (i=0;i<ma;i++) {
    tempa[i] = 0.0;
    for (j=0;j<ndata;j++) {   
      tempa[i] += u[i+j*ma]*yy[j]; 
    }
  }
    
  for (i=0;i<ma;i++) {
    tempa[i] /= w[i];
  }

  for (i=0;i<ma;i++) {
    afoc[i] = 0.0;
    for (j=0;j<ma;j++) {   
      afoc[i] += v[i*ma+j]*tempa[j]; 
    }
  }

  /* Now find chi-squared */
  *chisq=0.0;
  for (i=0;i<ndata;i++) {
    for (sum=0.0,j=0;j<ma;j++) sum += afoc[j]*x[j][i]; 
    /* HAK 3-Oct-2005  fixed this bug in how chisq was being
       calculated.  Had been using yy[i] which is already 
       divided by sig[i] */
    tmp=(y[i]-sum)/sig[i];
    *chisq += tmp*tmp;
  }

  /* Now find the covariance matrix */
  for (j=0;j<ma;j++) {
    for (k=0;k<ma;k++) {
      cvm[j*ma+k] = 0.0;
      for (i=0;i<ma;i++) {
	cvm[j*ma+k] += (v[j+i*ma]*v[k+i*ma])/(w[i]*w[i]);
      }
    }
  }

  free(xx);
  free(yy);
  free(u);
  free(v);
  free(w);
  free(temp);

}

/*-------------------------------------------------------------*/

/* Routine to make all calls to PIL to fill the structure containing
   all user supplied paramters. */

int batclean_getpar (
		     BATCLEAN_PARAMS *params)
{
 
  int i,len,status=0;
  char *cptr, *ignore, *model;

  fitsfile *infptr=0;
 
  params->incatalog[0] = 0;
  params->aperture[0] = 0;

  if ((status = PILGetFname("infile", params->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");

  else if ((status = PILGetFname("outfile", params->outfile)))
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");

  else if ((status = PILGetFname("detmask", params->detmask)))
    fprintf(stderr, "Error reading the 'detmask' parameter.\n");

  else if ((status = PILGetFname("wtmapin", params->wtmapin)))
    fprintf(stderr, "Error reading the 'wtmapin' parameter.\n");

  else if ((status = PILGetFname("wtmapout", params->wtmapout)))
    fprintf(stderr, "Error reading the 'wtmapout' parameter.\n");

  else if ((status = PILGetFname("backexp", params->backexp)))
    fprintf(stderr, "Error reading the 'backexp' parameter.\n");

  else if ((status = PILGetString("bkgmodel", params->bkgmodel)))
    fprintf(stderr, "Error reading the 'bkgmodel' parameter.\n");

  else if ((status = PILGetString("corrections", params->corrections)))
    fprintf(stderr, "Error reading the 'corrections' parameter.\n");

  else if ((status = PILGetString("balance", params->balance)))
    fprintf(stderr, "Error reading the 'balance' parameter.\n");
   
  else if ((status = PILGetBool("balfirst", &params->balfirst)))
    fprintf(stderr, "Error reading the 'balfirst' parameter.\n");

  else if ((status = PILGetBool("maskfit", &params->maskfit)))
    fprintf(stderr, "Error reading the 'maskfit' parameter.\n");

  else if ((status = PILGetString("ignore", params->ignore)))
    fprintf(stderr, "Error reading the 'ignore' parameter.\n");
   
  else if ((status = PILGetString("outversion", params->outversion)))
    fprintf(stderr, "Error reading the 'outversion' parameter.\n");

  else if ((status = PILGetBool("srcclean", &params->srcclean)))
    fprintf(stderr, "Error reading the 'srcclean' parameter.\n");

  else { 

    if (params->srcclean == 1) {

      if ((status = PILGetFname("incatalog", params->incatalog)))
	fprintf(stderr, "Error reading the 'incatalog' parameter.\n");

      if ((status = PILGetString("aperture", params->aperture)))
	fprintf(stderr, "Error reading the 'aperture' parameter.\n");

      if ((status = PILGetReal("bat_z", &params->bat_z)))
	fprintf(stderr, "Error reading the 'bat_z' parameter.\n");

      if ((status = PILGetReal("eff_edge", &params->eff_edge)))
	fprintf(stderr, "Error reading the 'eff_edge' parameter.\n");

      if ((status = PILGetReal("cleansnr", &params->cleansnr)))
	fprintf(stderr, "Error reading the 'cleansnr' parameter.\n");

      if ((status = PILGetString("snrcol", params->snrcol)))
	fprintf(stderr, "Error reading the 'snrcol' parameter.\n");

      if ((status = PILGetBool("leadedge", &params->leadedge)))
	fprintf(stderr, "Error reading the 'leadedge' parameter.\n");
    } else {
      /* Set to zero parameters that are not relevant without source
	 cleaning */
      params->leadedge=0;
      params->bat_z=0.0;
      params->cleansnr=0.0;

    }

    /* remove leading blanks in output file string */
    cptr = params->outfile;
    while (*cptr == ' ') cptr++;  
    if (cptr != params->outfile) {
      len = strlen(cptr);
      memmove(params->outfile, cptr, len + 1);
    }

    /* test for special strings */
    if (!strcasecmp(params->outfile, "none")  ) params->outfile[0]   = '\0';
    if (!strcasecmp(params->incatalog, "none")) params->incatalog[0] = '\0';
    if (!strcasecmp(params->aperture, "none") ) params->aperture[0]  = '\0';
    if (!strcasecmp(params->detmask, "none")  ) params->detmask[0]   = '\0';
    if (!strcasecmp(params->wtmapin, "none")  ) params->wtmapin[0]   = '\0';
    if (!strcasecmp(params->wtmapout, "none") ) params->wtmapout[0]  = '\0';
    if (!strcasecmp(params->backexp, "none")  ) params->backexp[0]   = '\0';
	
  }

  if (strcasecmp(params->outversion,"original") &&
      strcasecmp(params->outversion,"fit") &&
      strcasecmp(params->outversion,"cleaned") &&
      strcasecmp(params->outversion,"bkgcleaned") &&
      strcasecmp(params->outversion,"bkgfit") &&
      strcasecmp(params->outversion,"srccleaned") &&
      strcasecmp(params->outversion,"srcfit")) {
    fprintf(stderr, "ERROR: outversion must be one of: original, fit, bkgcleaned, bkgfit, srccleaned or srcfit\n");
    return -1;
  }

  /* Parse the input list for the ignore parameter */
  params->ignores = expand_item_list(params->ignore, &params->nignore, ',', 
				     1, 1, 1, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not parse source ignore list\n");
    if (params->ignores) free(params->ignores);
    return status;
  }
  if (params->nignore == 0) {
    fprintf(stderr, "ERROR: source ignore list was empty\n");
    if (params->ignores) free(params->ignores);
    return -1;
  }

  /* If first element is "NONE" then do not use 'ignore' */
  if (strcasecmp(params->ignores[0], "NONE") == 0) {
    params->nignore = 0;
  }
  
  for (i=0;i<params->nignore;i++) {
    ignore = params->ignores[i];
    if (!strcasecmp(ignore, "none")) strcpy(params->ignores[i],"NONE");
  }

  /* Parse the input list for the bkgmodel parameter */
  params->bkgmodels = expand_item_list(params->bkgmodel, &params->nbkgmodel, 
				       ',', 1, 1, 1, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not parse background model (bkgmodel) list\n");
    if (params->bkgmodels) free(params->bkgmodels);
    return status;
  }
  if (params->nbkgmodel == 0) {
    fprintf(stderr, "ERROR: background model (bkgmodel) list was empty\n");
    if (params->bkgmodels) free(params->bkgmodels);
    return -1;
  }

  for (i=0;i<params->nbkgmodel;i++) {
    model = params->bkgmodels[i];
    if (!strcasecmp(model, "simple")   ) strcpy(params->bkgmodels[i],"SIMPLE");
    if (!strcasecmp(model, "useronly") ) strcpy(params->bkgmodels[i],"USERONLY");
    if (!strcasecmp(model, "test") ) {
      strcpy(params->bkgmodels[i],"TEST");
      params->srcclean=0;
    }
    if (!strcasecmp(model, "none") ) {
      strcpy(params->bkgmodels[i],"NONE");
      params->srcclean=0;
    }
  }

  /* If the aperture filename parameter points to caldb, then retrieve
     the aperture from caldb. */


  /* Fill in parameters given by CALDB */
  if (strncasecmp(params->aperture, "CALDB",5) == 0) {
    struct caldbparms_struct caldb;
    char expr[80];
    char *codenam = "CODED_MASK";
    char *pfile = params->aperture;
    char online[80], *ponline = online;
    long int extno[1];
    int maxret = 1;
    int nret = 0, nfound = 0;

    if (fits_open_data(&infptr, params->infile, READONLY,&status)) return(status);

    batkw_to_caldb_parms(infptr, &caldb, 1, &status);
    if ( infptr) fits_close_file(infptr,  &status);
    
    /* Query CALDB database for aperture */
    if (strcasecmp(params->aperture, "CALDB") == 0) {
      headas_chat(1, "NOTE: Using the 'FLUX' aperture type\n");
      strcpy(expr, "APERTYPE.eq.\"FLUX\"");
    } else if (strncasecmp(params->aperture, "CALDB:",6) == 0) {
      sprintf(expr, "APERTYPE.eq.\"%s\"", params->aperture+6);
    } else {
      fprintf(stderr, "ERROR: aperture must be either CALDB or CALDB:apertype\n");
      return -1;
    }
      
    bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
		     &pfile, extno, &ponline, &nret, &nfound, &status);
    if ((status != 0) || (nret == 0) || (nfound == 0)) {
      fprintf(stderr, "ERROR: could not locate aperture CALDB file\n");
      return status;
    }
  }

  headas_chat(4,"\nThe input file is called %s\n",params->infile);
  headas_chat(4,"The output file is called %s\n",params->outfile);
  headas_chat(4,"The input catalog file is called %s\n",params->incatalog);
  headas_chat(4,"The aperture file is called %s\n",params->aperture);
  headas_chat(4,"The detector mask file is called %s\n",params->detmask);
  headas_chat(4,"The input error file is called %s\n",params->wtmapin);
  headas_chat(4,"The output error file is called %s\n",params->wtmapout);
  headas_chat(4,"The back exposure map file is called %s\n",params->backexp);
  headas_chat(4,"    (%d background models to fit)\n", params->nbkgmodel);
  headas_chat(4,"The background model(s) are called %s\n",params->bkgmodel);
  headas_chat(4,"    (%d sources to ingore)\n", params->nignore);  
  headas_chat(4,"Source(s) to be ignored are %s\n",params->ignore);    
  headas_chat(4,"Will sources be cleaned? ");
  if (params->srcclean)  headas_chat(4,"YES\n"); 
  else headas_chat(4,"NO\n");
  headas_chat(4,"Fit leading edge detectors separately? ");
  if (params->leadedge)  headas_chat(4,"YES\n"); 
  else headas_chat(4,"NO\n");
  headas_chat(4,"The S/N threshold for source cleaning is %.4f\n",params->cleansnr);
  headas_chat(4,"The value of bat_z is %.4f\n",params->bat_z);
  headas_chat(4,"The output file will be the %s file\n",params->outversion);
  headas_chat(4,"Corrections to be applied are %s\n",params->corrections);
  headas_chat(4,"Balancing to be applied is %s\n",params->balance);
  headas_chat(4,"Which will be applied first? (clean or balance) ");
  if (params->balfirst)  headas_chat(4,"BALANCE\n"); 
  else headas_chat(4,"CLEAN\n");
  headas_chat(4,"Will masked detectors be fit? ");
  if (params->maskfit)  headas_chat(4,"YES\n\n"); 
  else headas_chat(4,"NO\n\n");
    
  return(status);

}


/*-------------------------------------------------------------*/

int batclean_printsummary (
			   BATCLEAN_PARAMS *params,
			   long int *nevt,
			   char *taskname,
			   char *version)

{ 
  headas_chat(1,"%s v%s completed\n",taskname,version);
  headas_chat(1,"Input DPI file is %s\n",params->infile);
  headas_chat(1,"Output file is %s\n",params->outfile);

  return(0);
}

/*-------------------------------------------------------------*/

int classbydet (
		double backexp[NUM_BACKGROUND_ELEMENTS][NUM_SPACES])
{

  /* Comments taken from original David Palmer IDL code */
  /* ; fill an array containing the detector class
     ; -2 = non-functional detector
     ; -1 = no detector
     ; 0 = interior
     ; 1 = front edge
     ; 2 = back edge
     ; 3 = left edge
     ; 6 = right edge
     ; 1+3=4 for front left corner etc.
     ; classbydet / 3 =  frontness       (0, 0, 1, 2)
     ; classbydet() mod 3 = leftness     (-1,0,1,2)
     ; (16 + 2) in the left-right direction, (8+3) in the front direction */

  int i,j,ii,jj;
  int cbd[NUM_ROWS][NUM_COLS];
  
  for (j=0;j<NUM_ROWS;j++) {
    for (i=0;i<NUM_COLS;i++) {
      cbd[j][i]=0;
      if (!(i % 18)) cbd[j][i] += 3;
      if ((i % 18) == 15)  cbd[j][i] += 6;
      if (!(j % 11)) cbd[j][i] += 2;
      if ((j % 11) == 7)  cbd[j][i] += 1;
      /* HAK 21-May-2003  Took out this line since the code wasn't 
	 working with it in.  The backexp does include turned off
	 and masked out detectors -- they are set to zero in the data
	 array */
      /* if (detmask[j][i]) cbd[j][i]=-2; */
      if (((i % 18) == 16) || ((i % 18) == 17))  cbd[j][i]=-1;
      if (((j % 11) == 8) || ((j % 11) == 9) || ((j % 11) == 10))  
	cbd[j][i]=-1;
    }
  } 
  
  for (i=0;i<NUM_SPACES;i++) {
    ii=(i % 286); /* X-value or column */
    jj=(int)(i / 286); /* Y-value or row */
    for (j=0;j<NUM_BACKGROUND_ELEMENTS;j++) backexp[j][i]=0.0;
    backexp[0][i]=1.0;   /* DC Offset */
    /* backexp[0][i]=cbd[jj][ii]; */  /* For now just make it the value of cbd */
    backexp[1][i]=DETECTOR_SIZE*((double)(ii)-((double)(NUM_COLS/2)-0.5)); /* X */
    
    backexp[2][i]=DETECTOR_SIZE*((double)(jj)-(double)(NUM_ROWS-1)/2); /* Y */
    backexp[3][i]=backexp[1][i]*backexp[1][i];  /* X^2 */
    backexp[4][i]=backexp[2][i]*backexp[2][i];  /* Y^2 */
    backexp[5][i]=backexp[1][i]*backexp[2][i];  /* Quadratic Cross Term */

    if (cbd[jj][ii] >= 0) {  /* Ignore bad detectors */

      if ((cbd[jj][ii] / 3) == 1) {
	backexp[6][i]=backexp[0][i]; /* Left detectors */
	backexp[7][i]=backexp[1][i];
      } 
      if ((cbd[jj][ii] / 3) == 2) {
	backexp[8][i]=backexp[0][i];  /* Right detectors */
	backexp[9][i]=backexp[1][i];
      }
      /* HAK 6-Mar-2005 Took out the differentiation between the top
       * and bottom halves of the array.  This is vestigal to the
       * ancient design in which the sandwiches are shingled. */
      if ((cbd[jj][ii] % 3) == 1) {
	backexp[10][i]=backexp[0][i]; /* Front edges */
	backexp[11][i]=backexp[2][i]; 
      }
      if ((cbd[jj][ii] % 3) == 2) {
	backexp[12][i]=backexp[0][i]; /* Back edges */
	backexp[13][i]=backexp[2][i]; 
      }
    }

    /* Now set the non-existent and bad detector spaces back to zero */
    if (cbd[jj][ii] < 0) 
      for (j=0;j<NUM_BACKGROUND_ELEMENTS;j++) backexp[j][i]=0.0;

  }

  return(0);
  
}

/*-------------------------------------------------------------*/

double maskimg[BLOCKYCELLS*2][BLOCKXCELLS*8];

int forward_project(
		    char *aperture,
		    SOURCE_STRUCT *source,
		    double *bat_z,
		    double *eff_edge,
		    char *corrections)

{
  struct batmaskplane_struct mask;
  struct batdetplane_struct detplane;
  fitsfile *infptr;

  int status=0;
  int i,j,k;
  double srcpos[3];
  /* Output array of full mask */
  struct batmaskcorrections_struct corrs; 

  double x=0.0,y=0.0,z=0.0;
  double distance=10000000.0;

  /* Set or zero out all the correction terms */
  memset((void *)&corrs, 0, sizeof(corrs));  /* <--- Zero the structure */
  corrs.subpixsize = 0.02;
  corrs.nmasklayers = 0;
  corrs.ccosine = 0;
  corrs.cside = 0;
  corrs.copaque = 0;
  corrs.opaquethresh = 0.98;
  corrs.nmasklayers = 0;
  /* HAK 21-Aug-2003  Changed these next 
     two lines to change the mask weighting
     to run from [0,1] instead of [-1,1] */
  corrs.rebalance = 0;
  corrs.cunbalanced = 1;
  corrs.cpcode = 0;
  corrs.crsquare = 0;
  corrs.cflatfield = 0;   
  corrs.cnbatdets = 0;
  /* Correct for flat fielding of Tueller/Hullinger */   
  corrs.cflatfield = 0;
  /* Apply mask weighting corrections in a forward sense */
  corrs.direction = RAYTRACE_FORW;
  /* Add JayC fields to corrs structure */
  corrs.cinc_edge=0;
  corrs.copaque=0;
  corrs.eff_edge=*eff_edge;

  /* Only apply rsquare or flatfield correction for source at a finite 
     distance */
  if (status == 0) {
    headas_chat(5, "...corrections: ");
    if (strstr(corrections, "flatfield")) {
      headas_chat(5,"flatfield, ");
      corrs.cflatfield = 1;
    }
    if (strstr(corrections, "cosine")) {
      headas_chat(5,"cosine, ");
      corrs.ccosine = 1;
    }
    if ((strstr(corrections, "nearfield")) && (*bat_z != 0.0)) {
      headas_chat(5,"nearfield, ");
      corrs.crsquare = 1;
    }
    if ((strstr(corrections, "rsquare")) && (*bat_z != 0.0)) {
      headas_chat(5,"rsquare, ");
      corrs.crsquare = 1;
    }
    if ((strstr(corrections, "inc_edge")) && (*bat_z != 0.0)) {
      headas_chat(5,"inc_edge, ");
      corrs.cinc_edge = 1;
    }
    if (strstr(corrections, "opaque")) {
      headas_chat(5,"opaque, ");
      corrs.copaque = 1;
    }
    headas_chat(5, "...\n");
  }



  /* Read aperture image */
  headas_chat(5,"...reading aperture file...\n"
	      "   (%s)\n", aperture);
  fits_open_image(&infptr,aperture, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s\n", aperture);
    return status;
  }
  mask_image(infptr, &mask, &status);             /* Image */
  mask_readkey(infptr, &mask, &status);           /* Mask keywords */
  detplane_readkey(infptr, &detplane, &status);   /* Detector keywords */
  headas_chat(5, "   (done reading aperture; status=%d)\n", status);
  fits_close_file(infptr, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not read data from %s\n", aperture);
    return status;
  }
    
    
  /* Initialize the mask weighted image to zero */
  for (i=0;i<BLOCKYCELLS*2;i++) {
    for (j=0;j<BLOCKXCELLS*8;j++) maskimg[i][j] = 0.0; 
  }   
    
  for (i=0;i<source[0].rows;i++) {
    /* HAK 28-July-2003 Modified code to handle case of finite bat_z */
    /* case BCTANGENT:  Tangent plane coordinates from Craig's code*/
    x = source[i].imx;
    y = source[i].imy;
    z = *bat_z;
    if (z != 0) {
      /* If z is known, then compute the source position directly... */
      srcpos[0] = x*z;
      srcpos[1] = y*z;
      srcpos[2] = z;
    } else {
      /* ... otherwise, only the unit vector */
      z = 1.0 / sqrt(1.0 + x*x + y*y);
      srcpos[0] = distance*(x*z);
      srcpos[1] = distance*(y*z);
      srcpos[2] = distance*z;
    }
      
    /* Initialize the mask weighted image to zero */
    /* This must be done before every call to maskwtimg */
    for (j=0;j<BLOCKYCELLS*2;j++) {
      for (k=0;k<BLOCKXCELLS*8;k++) maskimg[j][k] = 0.0; 
    }   
      
    maskwtimg(srcpos, &mask, &detplane, &corrs, &(maskimg[0][0]),
	      BLOCKXCELLS*8, BLOCKYCELLS*2);
      
    for (j=0;j<NUM_ROWS;j++) {
      for (k=0;k<NUM_COLS;k++) 
	source[i].mskimg[k+j*NUM_COLS] = maskimg[j][k]; 
    }   

  }

  return(status);

}




/*-------------------------------------------------------------*/

void run_fitting_test()
{
  double x[3][6];
  float y[6],sig[6];
  float chisq=0;

  int i,j;
  int ma=3;        /* Number of fit parameters */
  int ndata=6;     /* Number of data points */

  float resb[3];
  double **xstar;
  float *cvm;

  for (i=0;i<ndata;i++) {
    x[0][i]=1.0;
    sig[i]=1.0;
  }
  /* IDL> x=[[0.0,0.0],[2.0,1.0],[2.5,2.0],[1.0,3.0],[4.0,6.0],[7.0,2.0]] */
  x[1][0]= 0.0;
  x[2][0]= 0.0;
  x[1][1]= 2.0;
  x[2][1]= 1.0;
  x[1][2]= 2.5;
  x[2][2]= 2.0;
  x[1][3]= 1.0;
  x[2][3]= 3.0;
  x[1][4]= 4.0;
  x[2][4]= 6.0;
  x[1][5]= 7.0;
  x[2][5]= 2.0;
  y[0] = 6.0;
  y[1] = 12.0;
  y[2] = 9.3;
  y[3] = -1.0;
  y[4] = 3.5;
  y[5] = 29.0;
    
  headas_chat(4,"Input Matrix X\n");
  for (i=0;i<ndata;i++) {
    headas_chat(4,"%d %.2f  %.2f %.2f %.2f %.2f\n",i,x[0][i],x[1][i],x[2][i],y[i],sig[i]);
  }

  cvm = (float *) malloc((unsigned) ma*ma*sizeof(float));

  xstar = (double **) malloc((unsigned) (ma)*sizeof(double*));
  /* for (i=0,j=0;i<=ma-1;i++,j++) xstar[j]=(double *)&x[0][0]+ndata*i; */
  for (i=0,j=0;i<=ma-1;i++,j++) xstar[j]=&x[0][0]+ndata*i;

  headas_chat(4,"Input Matrix X\n");
  for (i=0;i<ndata;i++) {
    headas_chat(4,"%d (%.2f  %.2f %.2f)\n",i,xstar[0][i],xstar[1][i],xstar[2][i]);
  }


  /* This is the Golub and Reinsch formula */
  fit_model(xstar,y,sig,ma,ndata,resb,&chisq,cvm);

  headas_chat(4,"Returned from the inner sanctum\n");

  headas_chat(4,"Fit Matrix A\n");
  for (i=0;i<ma;i++) {
    headas_chat(4,"%d %.4f\n",i,resb[i]);
  }

  headas_chat(4,"Covariance Matrix\n");
  for (i=0;i<ma;i++) {
    headas_chat(4,"%d %.4f  %.4f %.4f\n",
		i,cvm[i*ma],cvm[i*ma+1],cvm[i*ma+2]);
  }


}

/*-------------------------------------------------------------*/

int write_backexposure(
		       fitsfile *bkexptr,
		       double **xstar,
		       float afoc[],
		       float cvm[],
		       int ma,
		       int reduc,
		       int leadedge,
		       char *creator)

{
  int i,j,status=0,ncols,cvmdim;
  int start,debug=5;
 
  char *ttype[NUM_BACKGROUND_ELEMENTS+MAX_SOURCES+1];   
  /* Names of the columns in the background exposure table */
  char *tform[NUM_BACKGROUND_ELEMENTS+MAX_SOURCES+1];   /* Format of each column */
  char *tunit[NUM_BACKGROUND_ELEMENTS+MAX_SOURCES+1];   /*Physical unit of table column */
  char keyname[FLEN_CARD] = "";
  char tdim[FLEN_CARD] = "(286,173)";
  char tformn[FLEN_CARD] = "";
  char tdimn[FLEN_CARD] = "";

  char comment[FLEN_CARD] = "";
  char value[FLEN_CARD]= "";
  int nsources = 0;

  double *expmap = 0;

  expmap = (double *) malloc((unsigned) NUM_SPACES*sizeof(double));

  for (i=0;i<ma;i++) {
    tform[i] = "49478E"; 
    tunit[i] = " ";
  }
  cvmdim=ma*ma;
  sprintf(tformn,"%dE",cvmdim);
  sprintf(tdimn,"(%d,%d)",ma,ma);
  tform[ma] = tformn; 
  tunit[ma] = " ";

  ttype[0]="CONSTANT";
  ttype[1]="X";
  ttype[2]="Y";
  ttype[3]="XSQR";
  ttype[4]="YSQR";
  ttype[5]="X_TIMES_Y";
  ttype[6]="LEFT_DETS_CONST";
  ttype[7]="LEFT_DETS_X";
  ttype[8]="RIGHT_DETS_CONST";
  ttype[9]="RIGHT_DETS_X";
  if (reduc == 0) {
    ttype[10]="FRONT_BOTTOM_CONST";
    ttype[11]="FRONT_BOTTOM_Y";
    ttype[12]="FRONT_TOP_CONST";
    ttype[13]="FRONT_TOP_Y";
    ttype[14]="BACK_BOTTOM_CONST";
    ttype[15]="BACK_BOTTOM_Y";
    ttype[16]="BACK_TOP_CONST";
    ttype[17]="BACK_TOP_Y";
  } else {
    ttype[10]="FRONT_CONST";
    ttype[11]="FRONT_Y";
    ttype[12]="BACK_CONST";
    ttype[13]="BACK_Y";
  }

  for (i=NUM_BACKGROUND_ELEMENTS-reduc;i<ma;i++) {
    ttype[i] = "SOURCE";
    nsources ++;
    if (nsources > MAX_SOURCES) {
      fprintf(stderr, 
	      "WARNING: number of sources (%d) exceeds the maximum (%d)\n"
	      "         truncating the 'backexp' output file.\n",
	      ma-NUM_BACKGROUND_ELEMENTS+reduc,
	      MAX_SOURCES);
      /* KLUDGE: truncate the list at 'ma' so that we don't overflow internal
	 buffers. */
      ma = i;
    }
  }
  ttype[ma]="COVAR_MATRIX";

  /* If the output background exposure map file is specified, then open
     this file for readout.  This is really only for diagnostics, so it
     is not expected to be used very often. */

  fits_create_tbl(bkexptr, BINARY_TBL, 1, ma+1, 
		  ttype, tform, tunit, "BAT_BKEXP", &status); 
  if (status) {
    fprintf(stderr,"ERROR:unable to create binary table status=%d\n",status);
    goto cleanup;
  } 
  /* Update the CREATOR keyword for this task */
  fits_update_key(bkexptr, TSTRING, "CREATOR", creator, 
		  "Name of creating task", &status);
      
  for (i=0;i<ma;i++) {
    for (j=0;j<NUM_SPACES;j++) expmap[j]=xstar[i][j]*afoc[i];
    fits_write_col(bkexptr,TDOUBLE,i+1,1,1,NUM_SPACES,&expmap[0],&status);
    if (status) goto cleanup;
  }
  if (status) {
    fprintf(stderr, "ERROR: writing to background exposure file. \n");
    goto cleanup;
  } 
  fits_write_col(bkexptr,TFLOAT,i+1,1,1,ma*ma,
		 &cvm[0],&status);      
  fits_set_hdustruc(bkexptr, &status);
  fits_get_num_cols(bkexptr,&ncols,&status);
  for (i=0;i<ma;i++) {
    sprintf(keyname,"TDIM%d",i+1);
    fits_update_key(bkexptr,TSTRING,keyname,tdim,
		    "Array dimensions",&status);
    if (status) {
      fprintf(stderr, "ERROR: writing keywords (%d) \n",status);
      goto cleanup;
    }
  } 
  sprintf(keyname,"TDIM%d",i+1);
  fits_update_key(bkexptr,TSTRING,keyname,tdimn,
		  "Array dimensions",&status);

  /* Rewrite column names and comments */
  start=NUM_BACKGROUND_ELEMENTS-reduc+1;
  for (i=start;i<ma+1;i++) {
    sprintf(keyname,"TTYPE%d",i);
    if (leadedge) {
      j = ((i-start)/2) + 1;
      if ((i - start) % 2) {
	sprintf(value,"SOURCE_%d_LEADEDGE",j);
	sprintf(comment,"Leading edge DPI for Source %d",j);
      } else {
	sprintf(value,"SOURCE_%d",j);
	sprintf(comment,"Detector plane for Source %d",j);
      }
    } else {
      sprintf(value,"SOURCE_%d",i-(NUM_BACKGROUND_ELEMENTS-reduc));
      sprintf(comment,"Detector plane for Source %d",
	      i-(NUM_BACKGROUND_ELEMENTS-reduc));
    }
    headas_chat(debug,"   (%s = %s / %s)\n",keyname,value,comment);
    fits_update_key(bkexptr,TSTRING,keyname,value,comment,&status); 
  }

  fits_set_hdustruc(bkexptr, &status);
  /* Write optional history keywords */
  /* Replaced headas_parstamp with HDpar_stamp  14-Nov-2003  */
  status=HDpar_stamp(bkexptr,0,&status); 
  /* status=headas_parstamp(bkexptr,0); */
  if (status) goto cleanup;

  if (expmap) free(expmap);
  return status;

 cleanup:

  if (expmap) free(expmap);
  if (bkexptr) fits_close_file(bkexptr,  &status);
  return status;

}

/*-------------------------------------------------------------*/

int write_fit_params(
		     fitsfile *wrtptr,
		     int *leadedge, 
		     int ma,
		     int reduc,
		     int nmodel,
		     float *afoc,
		     float *chisq,
		     SOURCE_STRUCT *source,
		     SOURCE_STRUCT *model_dpi)

{
  int i,j,status=0;
  int start=NUM_BACKGROUND_ELEMENTS;
  char keyname[FLEN_CARD] = "";
  char comment[FLEN_CARD] = "";
  int debug=5;

  fits_update_key(wrtptr, TFLOAT, "CHISQ", chisq,
		  "Chi Squared from background fit",&status);

  for (i=0; i<NUM_BACKGROUND_ELEMENTS-reduc; i++) {
    sprintf(keyname,"BGPAR%d",i);
    sprintf(comment,"Background fit parameter %d",i);
    fits_update_key(wrtptr, TFLOAT, keyname, &afoc[i], comment, &status); 
    headas_chat(debug,"   (%s = %g / %s)\n",
		keyname,afoc[i],comment);
  }

  start -= reduc;
  for (i=start;i<start+nmodel;i++) {
    sprintf(keyname,"BGPAR%d",i);
    sprintf(comment,"User bkg fit parameter %d: %s",
	    i-start, model_dpi[i-start].name);
    fits_update_key(wrtptr, TFLOAT, keyname, &afoc[i], comment, &status); 
    headas_chat(debug,"   (%s = %g / %s)\n",
		keyname,afoc[i],comment);
  }

  start += nmodel;
  for (i=start; i<ma; i++) {
    sprintf(keyname,"SRPAR%d", i-start);
    if (*leadedge) {
      j = ((i-start)/2) + 1;
      if ((i - start) % 2) {
	sprintf(comment,"Leading edge Src fit param %d: %s",
		j,source[j-1].name);
      } else {
	sprintf(comment,"Source fit parameter %d: %s",
		j,source[j-1].name);
      }
    } else {
      sprintf(comment,"Source fit parameter %d: %s",
	      i-start,source[i-start].name);
    }
    fits_update_key(wrtptr, TFLOAT, keyname, &afoc[i], comment, &status); 
    headas_chat(debug,"   (%s = %g / %s)\n",
		keyname,afoc[i],comment);
  }

  fits_set_hdustruc(wrtptr, &status);
  return status;

}

/*-------------------------------------------------------------*/

int balance(
   float pfocal[],
   float fit[],
   float pdetmask[],
   char *balance)
{
  int i,j,status=0;
  eBalanceTypes baltype;
  char **balances;
  int nbalance_list = 0, nbalance = 0;
  char *balcheck;
  
  int ii,jj;
  int debug=5;  /* Reset this variable to temporarily decrease chatter
		   level for this task */
  int cbd[NUM_ROWS][NUM_COLS];
  int sandwich[NUM_ROWS][NUM_COLS];
  float means[2*NUM_BLOCKS*NUM_SANDWICHES];
  float dets[2*NUM_BLOCKS*NUM_SANDWICHES];
  
  /* Parse the input list */
  balances = expand_item_list(balance, &nbalance_list, ',', 1, 1, 1, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not parse balance list\n");
    if (balances) free(balances);
    return status;
  }
  if (nbalance_list == 0) {
    fprintf(stderr, "ERROR: balance list was empty\n");
    if (balances) free(balances);
    return -1;
  }
  
  /* Set or zero out all the correction terms */
  memset((void *)&baltype, 0, sizeof(baltype));  /* <--- Zero the structure */
  
  headas_chat(4,"In the balancing routine\n");
  
  j = 1;  /* Count of balancings */
  for (i=0; i<nbalance_list; i++) {
    balcheck = balances[i];
    if (!(strcasecmp(balcheck,"flight"))) {
      /* Flight is Edges+InOut */
      baltype.eBTShortEdges = j++;
      baltype.eBTLongEdges = j++;
      baltype.eBTInOut = j++;
    }		      
    if (!strcasecmp(balcheck,"All")) {
      baltype.eBTAll = j++;
    }		      
    if (!strcasecmp(balcheck,"Module")) {
      baltype.eBTModule = j++;
    }    
    if (!strcasecmp(balcheck,"InOut")) {
      baltype.eBTInOut = j++;
    }    
    if (!strcasecmp(balcheck,"Edges")) {
      /* Edges is ShortEdges+LongEdges */
      baltype.eBTShortEdges = j++;
      baltype.eBTLongEdges = j++;
    } 
    if (!strcasecmp(balcheck,"LongEdges")) {
      baltype.eBTLongEdges = j++;
    }     
    if (!strcasecmp(balcheck,"ShortEdges")) {
      baltype.eBTShortEdges = j++;
    }     
  }
  nbalance = j-1;
  
  headas_chat(4," (%d balancings to perform)\n", nbalance);

  /* Comments taken from original David Palmer IDL code */
  /* ; fill an array containing the detector class
     ; -2 = non-functional detector
     ; -1 = no detector
     ; 0 = interior
     ; 1 = front edge
     ; 2 = back edge
     ; 3 = left edge
     ; 6 = right edge
     ; 1+3=4 for front left corner etc.
     ; classbydet / 3 =  frontness       (0, 0, 1, 2)
     ; classbydet() mod 3 = leftness     (-1,0,1,2)
     ; (16 + 2) in the left-right direction, (8+3) in the front direction */
  
   /* Module:

      5 2 2 2 2 2 2 2 2 2 2 2 2 2 2 8
      3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
      3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
      3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
      3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
      3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
      3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
      4 1 1 1 1 1 1 1 1 1 1 1 1 1 1 7  */

   /* Use the same routine as in classbydet to get the classification of
      each detector spot */  
  for (j=0;j<NUM_ROWS;j++) {
    for (i=0;i<NUM_COLS;i++) {
      cbd[j][i]=0;
      if (!(i % 18)) cbd[j][i] += 3;
      if ((i % 18) == 15)  cbd[j][i] += 6;
      if (!(j % 11)) cbd[j][i] += 2;
      if ((j % 11) == 7)  cbd[j][i] += 1;
      if (((i % 18) == 16) || ((i % 18) == 17))  cbd[j][i]=-1;
      if (((j % 11) == 8) || ((j % 11) == 9) || ((j % 11) == 10))  
	cbd[j][i]=-1;
    }
  } 
  /* But since we are balancing by sandwich, we need another array to
     tell us what sandwich we are in */
  for (j=0;j<NUM_ROWS;j++) {
    for (i=0;i<NUM_COLS;i++) {
      sandwich[j][i]=-1;
      if (cbd[j][i] > -1) {
	sandwich[j][i]++;
	sandwich[j][i] += (i / 18);
	sandwich[j][i] += 16 * (j / 11);
      }
    }
  }   
  /* Also need to mask out the disabled detectors from the detector enable
     map, so that they are not included in the averages for balancing. 
     But this needs to be done inside the balancing loops so that pfocal
     is not corrupted. */
  
  /* Details of balancing:
     
   The balancing is the same as that performed in the BAT flight code:

   The flight code contains the various options:

   typedef enum
   {
     eBTNone = 0,
     eBTAll = 1,     Balance full DAP (enabled detectors only). 
        Useful if you do no other balanacing or cleaning 
     eBTClean = 2,
     eBTModule = 3,
     eBTInOut = 4,
     eBTEdges = 5,
     eBTLongEdges = 6,
     eBTShortEdges = 7
   } eBalanceTypes;

   eBTModule: each of the 256 modules is balanced separately.    (256 
      balancings, each of 128 detectors)

   eBTInOut:  each module has its inner dets balanced and its outer dets 
      balanced.  2x256 balancings: 256 balancings of 84 inner detectors, 
      256 balancings of 44 outer detectors

   eBTEdges:  This is 256 balancings of the 44 outer (edge) detectors.

   eBTLongEdges:  32 balancings of (16 sandwiches x 16 detectors)

      In other words, if the array looks like this, then you balance LONG 
      ROW 0 (256 dets), then LONG ROW 1 (256 dets) on down to LONG ROW 31.

      (LONG ROW 0)
       LLLLLLLLLLLLLLLL   LLLLLLLLLLLLLLLL    ... to 16 ... LLLLLLLLLLLLLLLL

      Balance those.

      1
      2
      3
      4
      5
      6
      LLLLLLLLLLLLLLLL   LLLLLLLLLLLLLLLL    ... to 16 ... LLLLLLLLLLLLLLLL

      And Balance those.

      (LONG ROW 1)

      (LONG ROW 2)
      LLLLLLLLLLLLLLLL   LLLLLLLLLLLLLLLL    ... to 16 ... LLLLLLLLLLLLLLLL

      And Balance those.

      1
      2
      3
      4
      5
      6
      LLLLLLLLLLLLLLLL   LLLLLLLLLLLLLLLL    ... to 16 ... LLLLLLLLLLLLLLLL
      (LONG ROW 3)

      And Balance those.

      ... to 32 ...

   eBTShortEdges:  32 balancings of (16 sandwiches x 8 detectors)
>
   */

   /* HAK 11-Apr-2005 Added code to allow balancing of the fit file 
      (if that version is selected with the "outversion" parameter)
      Since the -fit- is related to the -cleaned- by the equation:

      Cleaned = Original - Fit

      balancing makes the equation:

      (Cleaned - balanced) = Original - Fit
      (Cleaned - balanced) = Original - (Fit + balanced)
      
      so whenever the balancing is subtracted from the Cleaned DPI, the
      same quantity is added to the Fit DPI.  

      (Note that the main code just passes a placeholder to this routine
      when the cleaned DPI is to be output)  */

  status=1;
  /* Now loop through and do the balancing in the order specified */
  for (i=1; i<=nbalance; i++) {
    
    /* Balance the entire array */	   
    if (baltype.eBTAll == i) {
      status=0;
      headas_chat(4,"Applying the All balancing (order is %d)\n",i); 
      for (j=0;j<2*NUM_BLOCKS*NUM_SANDWICHES;j++) {
	means[j]=0.0;
	dets[j]=0.0;
      }
      for (j=0;j<NUM_SPACES;j++) {
	ii=(j % 286); /* X-value or column */
	jj=(int)(j / 286); /* Y-value or row */
	if (!(pdetmask[j])) {
	  means[0] += pfocal[j];
	  dets[0]++;
	}
      }
      for (j=0;j<1;j++) { 
	if (dets[j]) means[j] /= dets[j];
	headas_chat(debug,"SW %d mean is %.4f for %.1f dets\n",j,means[j],dets[j]);
      } 
      for (j=0;j<NUM_SPACES;j++) {
	ii=(j % 286); /* X-value or column */
	jj=(int)(j / 286); /* Y-value or row */
	if (!(pdetmask[j])) {
	  pfocal[j] -= means[0];
	  fit[j] += means[0];
	}
      }
    }
    
    /* Balance each module separately */       
    if (baltype.eBTModule == i) {
      status=0;
      headas_chat(4,"Applying the Module balancing (order is %d)\n",i);
      for (j=0;j<2*NUM_BLOCKS*NUM_SANDWICHES;j++) {
	means[j]=0.0;
	dets[j]=0.0;
      }
      for (j=0;j<NUM_SPACES;j++) {
	ii=(j % 286); /* X-value or column */
	jj=(int)(j / 286); /* Y-value or row */
	if ((!(pdetmask[j])) && (sandwich[jj][ii] > -1)) {
	  means[sandwich[jj][ii]] += pfocal[j];
	  dets[sandwich[jj][ii]]++;
	}
      }
      for (j=0;j<NUM_BLOCKS*NUM_SANDWICHES;j++) { 
	if (dets[j]) means[j] /= dets[j];
	headas_chat(debug,"SW %d mean is %.4f for %.1f dets\n",j,means[j],dets[j]);
      } 
      for (j=0;j<NUM_SPACES;j++) {
	ii=(j % 286); /* X-value or column */
	jj=(int)(j / 286); /* Y-value or row */
	if ((!(pdetmask[j])) && (sandwich[jj][ii] > -1)) {
	  pfocal[j] -= means[sandwich[jj][ii]];
	  fit[j] += means[sandwich[jj][ii]];
	}
      }
    }
    
    /* Balance inner and outer detectors separately */
    if (baltype.eBTInOut == i) {
      status=0;
      headas_chat(4,"Applying the InOut balancing (order is %d)\n",i);
      for (j=0;j<2*NUM_BLOCKS*NUM_SANDWICHES;j++) {
	means[j]=0.0;
	dets[j]=0.0;
      }
      for (j=0;j<NUM_SPACES;j++) {
	ii=(j % 286); /* X-value or column */
	jj=(int)(j / 286); /* Y-value or row */
	if ((!(pdetmask[j])) && (sandwich[jj][ii] > -1)) {
	  if (cbd[jj][ii]) {   /* Outs */
	    means[sandwich[jj][ii]] += pfocal[j];
	    dets[sandwich[jj][ii]]++;
	  } else {  /* Ins */
	    means[sandwich[jj][ii]+256] += pfocal[j];
	    dets[sandwich[jj][ii]+256]++;
	  }
	}
      }
      for (j=0;j<2*NUM_BLOCKS*NUM_SANDWICHES;j++) { 
	if (dets[j]) means[j] /= dets[j];
	headas_chat(debug,"SW %d mean is %.4f for %.1f dets\n",j,means[j],dets[j]);
      } 
      for (j=0;j<NUM_SPACES;j++) {
	ii=(j % 286); /* X-value or column */
	jj=(int)(j / 286); /* Y-value or row */
	if ((!(pdetmask[j])) && (sandwich[jj][ii] > -1)) {
	  if (cbd[jj][ii]) pfocal[j] -= means[sandwich[jj][ii]];
	  else pfocal[j] -= means[sandwich[jj][ii]+256];
	  if (cbd[jj][ii]) fit[j] += means[sandwich[jj][ii]];
	  else fit[j] += means[sandwich[jj][ii]+256];
	}
      }
    } 
    
    /* Balance edge detectors separately */
    if (baltype.eBTEdges == i) {
      status=0;
      headas_chat(4,"Applying the Edges balancing (order is %d)\n",i);
      for (j=0;j<2*NUM_BLOCKS*NUM_SANDWICHES;j++) {
	means[j]=0.0;
	dets[j]=0.0;
      }
      for (j=0;j<NUM_SPACES;j++) {
	ii=(j % 286); /* X-value or column */
	jj=(int)(j / 286); /* Y-value or row */
	if ((!(pdetmask[j])) && (sandwich[jj][ii] > -1) && (cbd[jj][ii]) ) {
	  means[sandwich[jj][ii]] += pfocal[j];
	  dets[sandwich[jj][ii]]++;
	}
      }
      for (j=0;j<NUM_BLOCKS*NUM_SANDWICHES;j++) { 
	if (dets[j]) means[j] /= dets[j];
	headas_chat(debug,"SW %d mean is %.4f for %.1f dets\n",j,means[j],dets[j]);
      } 
      for (j=0;j<NUM_SPACES;j++) {
	ii=(j % 286); /* X-value or column */
	jj=(int)(j / 286); /* Y-value or row */
	if ((!(pdetmask[j])) && (sandwich[jj][ii] > -1) && (cbd[jj][ii])) {
	  pfocal[j] -= means[sandwich[jj][ii]];
	  fit[j] += means[sandwich[jj][ii]];
	}
      }
    }
    
    /* Balance long edge detectors separately */
    if (baltype.eBTLongEdges == i) {
      status=0;
      headas_chat(4,"Applying the LongEdges balancing (order is %d)\n",i);
      for (j=0;j<2*NUM_BLOCKS*NUM_SANDWICHES;j++) {
	means[j]=0.0;
	dets[j]=0.0;
      }
      for (j=0;j<NUM_SPACES;j++) {
	ii=(j % 286); /* X-value or column */
	jj=(int)(j / 286); /* Y-value or row */
	if ((!(pdetmask[j])) && (sandwich[jj][ii] > -1)) { 
	  if (!(((cbd[jj][ii])-1) % 3)) {
	    /* Bottom long edges */
	    means[sandwich[jj][ii]/16] += pfocal[j];
	    dets[sandwich[jj][ii]/16]++;
	  }
	  if (!(((cbd[jj][ii])-2) % 3)) {
	    /* Top long edges */
	    means[sandwich[jj][ii]/16+16] += pfocal[j];
	    dets[sandwich[jj][ii]/16+16]++;
	  }
	}
      }
      for (j=0;j<2*NUM_BLOCKS;j++) { 
	if (dets[j]) means[j] /= dets[j];
	headas_chat(debug,"Edge %d mean is %.4f for %.1f dets\n",j,means[j],dets[j]);
      } 
      for (j=0;j<NUM_SPACES;j++) {
	ii=(j % 286); /* X-value or column */
	jj=(int)(j / 286); /* Y-value or row */
	if ((!(pdetmask[j])) && (sandwich[jj][ii] > -1)) {
	  if (!(((cbd[jj][ii])-1) % 3)) {
	    pfocal[j] -= means[sandwich[jj][ii]/16];
	    fit[j] += means[sandwich[jj][ii]/16];
	  }
	  if (!(((cbd[jj][ii])-2) % 3)) {
	    pfocal[j] -= means[sandwich[jj][ii]/16+16];
	    fit[j] += means[sandwich[jj][ii]/16+16];
	  }
	}
      }
    } 
    
    /* Balance short edge detectors separately */
    if (baltype.eBTShortEdges == i) {
      status=0;
      headas_chat(4,"Applying the ShortEdges balancing (order is %d)\n",i);
      for (j=0;j<2*NUM_BLOCKS*NUM_SANDWICHES;j++) {
	means[j]=0.0;
	dets[j]=0.0;
      }
      for (j=0;j<NUM_SPACES;j++) {
	ii=(j % 286); /* X-value or column */
	jj=(int)(j / 286); /* Y-value or row */
	if ((!(pdetmask[j])) && (sandwich[jj][ii] > -1)) { 
	  if (cbd[jj][ii]/6) {
	    /* Right long edges */
	    means[sandwich[jj][ii] % 16] += pfocal[j];
	    dets[sandwich[jj][ii] % 16]++;
	  } else {
	    if (cbd[jj][ii]/3) {
	      /* Left long edges */
	      means[sandwich[jj][ii] % 16 + 16] += pfocal[j];
	      dets[sandwich[jj][ii] % 16 + 16]++;
	    }
	  }
	}
      }
      for (j=0;j<2*NUM_BLOCKS;j++) { 
	if (dets[j]) means[j] /= dets[j];
	headas_chat(debug,"Edge %d mean is %.4f for %.1f dets\n",j,means[j],dets[j]);
      } 
      for (j=0;j<NUM_SPACES;j++) {
	ii=(j % 286); /* X-value or column */
	jj=(int)(j / 286); /* Y-value or row */
	if ((!(pdetmask[j])) && (sandwich[jj][ii] > -1)) {
	  if (cbd[jj][ii]/6) {
	    pfocal[j] -= means[sandwich[jj][ii] % 16];
	    fit[j] += means[sandwich[jj][ii] % 16];
	  } else {
	    if (cbd[jj][ii]/3) {
	      pfocal[j] -= means[sandwich[jj][ii] % 16 + 16];
	      fit[j] += means[sandwich[jj][ii] % 16 + 16];
	    }
	  }
	}
      }
    }
    
  } /* End loop through balance parameters */   
  
  return status;
}

/* Determine whether this is a 'rate' unit string or not */
int is_units_rate(char *unitstr)
{
  char compstr[100];
  char *p, *q;
  
  /* Bounds checking */
  if (unitstr == 0) return 0;
  if (unitstr[0] == 0) return 0;
  if (strlen(unitstr) > (100-1)) return 0;

  /* Compress all the spaces out */
  for(p=unitstr, q=compstr; *p; p++) {
    if (! isspace(*p)) *q++ = tolower(*p);
  }
  *q = 0;

  /* Possible units: count/s or count s**(-1) */
  if (strcmp(compstr, "count/s") == 0) return 1;
  if (strcmp(compstr, "counts**(-1)") == 0) return 1;
  
  return 0;
}

/*
 * mighell_1999 - compute Mighell parameters Y and EY for input rate/counts
 * 
 * int is_rate - cr is rate value? (1=rate; 0=counts)
 * float expo - exposure in seconds
 * float cr - either counts or rate (determined by is_rate)
 * float *y - upon return, Mighell 1999 modified counts/rate
 * float *ey - upon return, Mighell 1999 modified error(counts/rate)
 *
 * Returns: 0
 */
int mighell_1999(int is_rate, float expo, float cr, float *y, float *ey)
{
  float counts;
  /* Convert rate to counts if necessary */
  if (is_rate) { 
    counts = cr*expo;
  } else {
    counts = cr;
  }

  /* For non-zero counts, we set "data" value to counts+1 and error
     value to sqrt(counts+1) */
  if (counts > 0) {
    *y = counts + 1;
    *ey = sqrt(counts+1);
  } else {
    /* For zero counts, we set the "data" value to 0, error to 1 */
    *y = 0;
    *ey = 1;
  }

  /* Convert back to rate if that was what was passed in */
  if (is_rate) {
    *y  /= expo;
    *ey /= expo;
  }

  return 0;
}

int linsum(int n, float *img, 
	   int addsub, int ncoeff, float *coeff, double **templ)
{
  int icoeff, jpix;

  /* Loop through coefficients */
  for (icoeff=0; icoeff<ncoeff; icoeff++) {
    double *ptempl = templ[icoeff];    /* Current template */
    float   acoeff = coeff[icoeff];    /* Current coefficient */

    if (addsub < 0) { acoeff *= -1; } /* If subtracting */

    for (jpix=0; jpix<n; jpix++) {
      img[jpix] += acoeff*ptempl[jpix];
    }
  }

  return 0;
}

