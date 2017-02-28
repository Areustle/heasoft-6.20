/* -------------batdrmgen.c response generator tool----------------- */
/* ----------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_utils.h"
#include "bat_gswdev.h"
#include "batdrmgen.h"



/* 
 * batdrmgen
 *
 * Compute BAT detector response matrix and write to OGIP-standard
 * .rsp file.
 *
 * See also: 
 *   respio.c - input/output routines
 *   calc.c   - response computation routines
 *   mutau_model.c - spectral response function
 *   lookup_params.c - goes to a FITS table file and extracts parameters

 * This module contains primarily CFITSIO and HEADAS setup code.
 *
 * A. Parsons / C. Markwardt / D. Hullinger
 * 
 * version 1.2: Build 5 version of batdrmgen.c - AMP 5/26/03
 * version 1.3: This version has the changes needed to accommodate 
 *              parms and resp structure changes made to allow user 
 *              to enter her own incident photon energy scale 
 *              parameters  AMP 7/8/03
 * version 1.4: this version has the user defined energy scale feature working.
 *              AMP 8/11/03
 * version 1.5: Cleared up variable name ambiguity between the actual incident
 *              energy scale and the Nbins and energy range parameters. Now the
 *              resp and parms structures are truly isolated in terms of the 
 *              kind of information they contain.  Also fixed column names 
 *              in the output response file so that XSPEC could recognise it
 *              -AMP 8/14/03
 * version 1.6: implementing the version of lookup_parms.c that can read in 
 *              the calibration parameter files.  AMP 8/19/03
 * version 1.7: Implementation of CALDB to get the correct files for the cal 
 *              parameters as well as  depth distribution table,  -AMP 8/20/03
 * version 1.9: Made INDEF escale simply a logarithmic scale 10 - 200 keV, 160
 *              bins with adjustments to line up with bin edges at 
 *              23.172, 26.093,27.471,30.993  - the bin edge values are hard 
 *              coded inside internal_escale.  Made INDEF the default for 
 *              escale in the param file and made escale a hidden parameter. 
 *              -AMP 8/24/03
 *
 * version 1.10 Build 6 version
 * version 1.11 reading in new keywords
 * version 1.12 Build 7 version
 * version 2.0  Build 9 version:
 *              Now properly accounts for unmodulated counts from higher
 *              energy photons (change in "trans" function).
 * version 2.3  lead absorption coefficient modeled as a broken power law
 *              above 88 keV
 *              RESPFILE keyword now written to PHA file
 * version 2.4  hkfile parameter added
 * version 2.5  Build 12 version:
 *              batdrmgen check hv from hkfile and makes sure it is 200 V
 *              for every sandwich
 * version 2.6  Added functionality to deal with a passive material transmission
 *              model that has breaks at at Ag and Au k edges
 * version 2.9  Added a better gain correction method so that peaks generated
 *              by batdrmgen line up with calibration line peaks
 * version 3.0  Added a linear function to determine energy resolution sigma
 * version 3.1  Added PB_EDGEN normalization factor for scaling the lead tile
 *              effect.
 * version 3.3  Added calls to fits_set_hdustruct() for safety against CFITSIO 
 *              taking a dump on the file structure. (CBM)
 * version 3.4  When the PHA file is updated with the response file name,
 *              now the RESPFILE *column* will be updated if it exists,
 *              otherwise it will udpate the RESPFILE keyword.
 * version 3.5  Changed the names of some WARNINGS and NOTEs (no code change)
 *              (CBM)
 * version 3.6  Allow SIGMA parameters to be zero, indicating no gaussian
 *              smoothing.
 ---------------------------- */

/* ============ This is the overall task version
                bump it up when there is a functionality change */
char taskname[] = "batdrmgen";
char taskver[] = "3.6";

#define TOOLSUB batdrmgen
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* ----------------------------------------------------------------- */
/* Get tool parameters from command line or parameter file */
int batdrmgen_getpar(struct parm_struct *parms) 
{
  int status = 0;

  /* Default values */
  parms->infile[0] = 0;
  parms->outfile[0] = 0;
  parms->hkfile[0] = 0;
  parms->calfile[0] = 0;
  parms->depthfile[0] = 0;
  parms->detmask[0] = 0;
  parms->efile[0] = 0;
  parms->row=1;
  strcpy(parms->coltype, "PI");    /* Filled in when reading PHA file */
  strcpy(parms->method, "");
  strcpy(parms->escale, "CALDB");
  parms->use_mean=1;

  /* Parameter values */
  if ((status = PILGetFname("infile", parms->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");

  else if ((status = PILGetFname("outfile", parms->outfile))) 
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");

  else if ((status = PILGetString("hkfile", parms->hkfile)))
    fprintf(stderr, "Error reading the 'hkfile' parameter.\n");

  else if ((status = PILGetString("calfile", parms->calfile)))
    fprintf(stderr, "Error reading the 'calfile' parameter.\n");
  
  else if ((status = PILGetString("depthfile", parms->depthfile)))
    fprintf(stderr, "Error reading the 'depthfile' parameter.\n");
 
  else if ((status = PILGetString("escale", parms->escale)))
    fprintf(stderr, "Error reading the 'escale' parameter.\n");

  else if ((status = PILGetString("detmask", parms->detmask)))
    fprintf(stderr, "Error reading the 'detmask' parameter.\n");

  else if ((status = PILGetString("fudge", parms->fudge)))
    fprintf(stderr, "Error reading the 'fudge' parameter.\n");

  else if ((status = PILGetString("method", parms->method)))
    fprintf(stderr, "Error reading the 'method' parameter.\n");

  else if ((status = PILGetBool("flight_data", &parms->flight_data)))
    fprintf(stderr, "Error reading the 'flight_data' parameter.\n");

  else if ((status = PILGetReal("hv_def",&parms->hv_def)))
    fprintf(stderr, "Error reading the 'hv_def' parameter.\n");

  else if ((status = PILGetInt("row",&parms->row)))
    fprintf(stderr, "Error reading the 'row' parameter.\n");

  else if ((status = PILGetReal("vthr_def",&parms->vthr_def)))
    fprintf(stderr, "Error reading the 'vthr_def' parameter.\n");

  else if ((strcasecmp(parms->escale, "LIN") == 0) || 
	   (strcasecmp(parms->escale, "LOG") == 0 )){ 

       if  ((status = PILGetInt("nphoton_bins", &parms->Nphoton_bins)))
       fprintf(stderr, "Error reading the 'nphoton_bins' parameter.\n");

       else if ((status = PILGetReal("elimit_lo",&parms->Elimit_lo)))
	     fprintf(stderr, "Error reading the 'elimit_lo' parameter.\n");

       else if ((status = PILGetReal("elimit_hi", &parms->Elimit_hi)))
	     fprintf(stderr, "Error reading the 'elimit_hi' parameter.\n");
  }
       
  else if (strcasecmp(parms->escale, "FILE") == 0) { 
    if ((status = PILGetString("efile", parms->efile)))
       fprintf(stderr, "Error reading the 'efile' parameter.\n");
  }

  if (status) {
    return status;
  }

  /* Use the method parameter to set use_mean */

  if (strstr(parms->method, "table")) parms->use_mean=0;

  headas_chat(4,"  use_mean: %d\n", parms->use_mean); 

  /* Check for default value of detmask file */

  if (strcasecmp(parms->detmask,"none") == 0) {
    parms->detmask[0] = 0;
  }

  if (strcasecmp(parms->hkfile,"caldb") == 0) {
    fprintf(stderr, 
	    "ERROR: hkfile=CALDB is not allowed.  Did you mean to set hkfile=NONE?\n");
    return 1;
  }

  /* Check and see if escale is an allowed value */

  headas_chat(4,"escale = %s\n",parms->escale); 

  if (strcasecmp(parms->escale, "LIN") != 0  &&
      strcasecmp(parms->escale, "LOG") != 0 &&
      strcasecmp(parms->escale, "FILE") != 0 ) {
       fprintf(stderr, "ERROR: your entry is not allowed for escale\n");
       return 1;
  }

  /* Check and see if fudge is an allowed value 
   * If it is INDEF, change it to CRAB */

  headas_chat(4,"fudge = %s\n",parms->fudge);

  if ((strcasecmp(parms->fudge, "NONE")!=0)&&
      (strcasecmp(parms->fudge, "INDEF")!=0)&&
      (strcasecmp(parms->fudge, "CRAB")!=0)) {
    fprintf(stderr,"ERROR: your entry is not allowed for fudge\n");
    return 1;
  }

  if (strcasecmp(parms->fudge, "INDEF")==0)
    strcpy(parms->fudge,"CRAB");

  /* Check Elimit_lo and Elimit_hi */

  if ((strcasecmp(parms->escale,"LIN")==0) && 
      (parms->Elimit_lo>parms->Elimit_hi)) {
    fprintf(stderr,"ERROR: elimit_lo > elimit_hi\n");
    return 1;
  }
  if ((strcasecmp(parms->escale,"LOG")==0) && 
      (parms->Elimit_lo>parms->Elimit_hi)) {
    fprintf(stderr,"ERROR: elimit_lo > elimit_hi\n");
    return 1;
  }

  /* change vthr from millivolts to volts */

  parms->vthr_def/=1000;

  return status;
}

/* ----------------------------------------------------------------- */
/* Read detector mask file 
 *
 * char *filename - name of detector mask file
 * long axes[2] - upon return, the dimensions of the image
 * int goodval - value which indicates "good" detectors.  Set to 0
 * int *status - status code up on return
 *
 * RETURNS: status code
 
 */
int *read_detmask(char *filename, long axes[2], int goodval, int *status)
{
  int naxis, bitpix, i;
  int *image = 0;
  fitsfile *fptr = 0;
  long int fpixel[] = {1,1};
  int safestatus = 0;
  
  if (status == 0) return 0;
  if (*status != 0) return 0;
  if (filename == 0) {
    *status = NULL_INPUT_PTR;
    return 0;
  }
  headas_chat(4,"Using fits_open_image\n");
  fits_open_image(&fptr, filename, READONLY, status);
  if (*status) return 0;

  headas_chat(4,"Using fits_get_image_param\n");
  fits_get_img_param(fptr, 2, &bitpix, &naxis, axes, status);
  if (*status) goto FAILED;

  image = (int *) malloc (sizeof(int) * axes[0] * axes[1]);
  if (image == 0) {
    *status = MEMORY_ALLOCATION;
    goto FAILED;
  }
  headas_chat(4,"Using fits_read_pix\n");
  fits_read_pix(fptr, TINT, fpixel, axes[0]*axes[1], 0, 
		image, 0, status);
  if (*status) goto FAILED;

  headas_chat(4,"Using fits_read_key\n");
  fits_read_key(fptr, TINT, "GOODVAL", &goodval, 0, status);
  *status = 0;

  for (i=0; i< (axes[0]*axes[1]); i++) {
    image[i] = (image[i] == goodval);
  }

  fits_close_file(fptr, status);
  return image;


 FAILED:
  /* Free the image data if it was allocated */
  if (image) free(image);
  image = 0;

  /* Close the file if it was open */
  safestatus = 0;
  if (fptr) fits_close_file(fptr, &safestatus);
  fptr = 0;

  return 0;
}


/* ----------------------------------------------------------------- */
/* Print opening banner for user */

void banner(struct parm_struct *parms)
{
  headas_chat(2, "*********************************************\n");
  headas_chat(1, "         %s v%s\n", parms->taskname, parms->taskver);
  headas_chat(2, "---------------------------------------------\n");
  headas_chat(2, "               PHA File: %s\n", parms->infile);
  headas_chat(2, "                PHA Row: %d\n", parms->row);
  headas_chat(2, "            Output File: %s\n", parms->outfile);
  headas_chat(2, "      Housekeeping File: %s\n", parms->hkfile);
  headas_chat(2, "       Calibration File: %s\n", parms->calfile);
  headas_chat(2, "Depth Distribution File: %s\n", parms->depthfile);
  headas_chat(2, "    Energy Scale Method: %s\n", parms->escale);
  /*if (parms->detmask[0])
    headas_chat(2,"           Detector Mask: %s\n", parms->detmask);
  else
    headas_chat(2, "          Detector Mask: NONE\n");*/
  if (parms->use_mean)
    headas_chat(2, "   Interpolation Method: MEAN\n");
  else
    headas_chat(2, "   Interpolation Method: METHOD\n");
  headas_chat(2, "      Correction Method: %s\n", parms->fudge);
  headas_chat(2, "\n");
  if (strcasecmp(parms->escale,"FILE") == 0) 
    headas_chat(2,"      Energy Scale File: %s\n", parms->efile); 
  else if 
    ((strcasecmp(parms->escale,"LIN") == 0)||
     (strcasecmp(parms->escale,"LOG") == 0)) {
      headas_chat(2,"  Number of Photon Bins: %d\n", parms->Nphoton_bins); 
      headas_chat(2,"      Lowest Photon Bin: %g keV\n", parms->Elimit_lo); 
      headas_chat(2,"     Highest Photon Bin: %g keV\n", parms->Elimit_hi); 
    }
  if (strcasecmp(parms->hkfile,"none") == 0) {
    headas_chat(2,"             Default HV: %g V\n", parms->hv_def); 
    /*headas_chat(2,"      Default Threshold: %g V\n", parms->vthr_def);*/
  }
  if (!parms->flight_data)
    headas_chat(2, "           Flight Data?: NO\n");
  headas_chat(1, "---------------------------------------------\n");
}

/* ----------------------------------------------------------------- */
/* Print feedback summary so user knows tool succeeded */
void summary(struct parm_struct *parms)
{
  headas_chat(2, "----------batdrmgen task complete------------\n");
}



/* ----------------------------------------------------------------- */
/* Do main work of tool */
int batdrmgen_work(struct parm_struct *parms)
{
  char *infile, *outfilename;
  int status = 0;
/*int *detmask = 0;*/ /* Detector quality mask data */
  struct resp_struct resp;
  long int nmatrixcells;

 

  if (parms == 0) {
    return NULL_INPUT_PTR;
  }

  infile = parms->infile;
  outfilename = parms->outfile;
  resp.matrix = 0;
  resp.emin = 0;      /* pulse height binning */
  resp.emax = 0;
  resp.nebins = parms->Nphoton_bins;  /* photon energy binning */
 
  banner(parms);

  /* Read the detector mask (if it exists) */
/*
  if (parms->detmask[0]) {
       headas_chat(4, "  ...Reading detector mask file...\n");
       detmask = read_detmask(parms->detmask, axesd, 0, &status);
       if (status != 0) {
	    fprintf(stderr, "ERROR: could not read quality mask %s\n", 
	    parms->detmask);
	    goto CLEANUP;
       }
  }
*/

  /* Read information from the PHA file */

  headas_chat(4,"Use readphainfo to get info from PHA spectrum file\n");

  if ((status = readphainfo(parms, &resp)) != 0) {
    fprintf(stderr, "ERROR: could not read PHA information\n");
    return status;
  }

  /* Harded values used for testing.

  resp.srcpos[0]=180.;
  resp.srcpos[1]=180.;
  resp.srcpos[2]=300.;

  resp.nphabins = 80;
 
  resp.nfapp=1;
  resp.pcodeapp=1;
  resp.pcodefr=0.5;
  resp.ngpixapp=1;
  resp.ngoodpix=32678.;

  */

  headas_chat(4, "Determine incident photon energy bin scale\n");
  if (internal_escale(parms, &resp)) {
    if (resp.energ_lo) free(resp.energ_lo);
    if (resp.energ_hi) free(resp.energ_hi);
    fprintf(stderr, "ERROR: internal_escale failed\n");
    return -1;
  }
 
  if ((resp.nebins == 0) || (resp.nphabins == 0)) {
    fprintf(stderr, "ERROR: response matrix has zero dimensions!\n");
    return -1;
  }

  /* ==== CM Find the CALDB file names if they are requested === */

  if (strcasecmp(parms->depthfile,"CALDB") == 0) {
    /* Assume the CALDB-related keywords have already been read
       (they have, in respio.c:readphainfo() */
    char *codenam = "DEPTH_DIST";     /* Code name of CALDB dataset */
    char *expr = "-";               /* Selection expression */
    int maxret = 1;                 /* Maximum desired results */
    char *pfile = parms->depthfile; /* String to receive query result */
    char online[80], *ponline = online; /* Dummy string to hold online */
    long int extno[1];              /* Dummy array to hold extension */
    int nret = 0, nfound = 0;       /* Number actually found */
    
    bat_caldb_search(&(resp.caldb), codenam, expr, maxret, PIL_PATH_MAX, 
		     &pfile,extno, &ponline, &nret, &nfound, &status);
    if ((status != 0) || (nret == 0) || (nfound == 0)) {
      fprintf(stderr, "ERROR: could not locate CALDB depth dist file\n");
      if (status != 0) return status;
      return -1;
    }
    
  }
  headas_chat(3,"  Depth dist file = %s\n", parms->depthfile);

  strcpy(parms->mtvalfile, parms->calfile);
  if (strcasecmp(parms->calfile,"CALDB") == 0) {
    /* Assume the CALDB-related keywords have already been read
       (they have, in respio.c:readphainfo() */
    char *codenam = "MTFUNC_PARMS";   /* Code name of CALDB dataset */
    char *expr = "-";               /* Selection expression */
    int maxret = 1;                 /* Maximum desired results */
    char *pfile = parms->calfile; /* String to receive query result */
    char online[80], *ponline = online; /* Dummy string to hold online */
    long int extno[1];              /* Dummy array to hold extension */
    int nret = 0, nfound = 0;       /* Number actually found */
    char extstr[PIL_PATH_MAX] = "";

    switch (resp.fluxmeth) {
      case 0:  expr = "FLUXMETH.EQ.\"UNKNOWN\"";  break;
      case 1:  expr = "FLUXMETH.EQ.\"RAW\"";      break;
      case 2:  expr = "FLUXMETH.EQ.\"WEIGHTED\""; break;
      case 3:  expr = "FLUXMETH.EQ.\"FITTED\"";   break;
      default: expr = "FLUXMETH.EQ.\"UNKNOWN\"";  break;
    }
    bat_caldb_search(&(resp.caldb), codenam, expr, maxret, PIL_PATH_MAX, 
		     &pfile,extno, &ponline, &nret, &nfound, &status);
    if ((status != 0) || (nret == 0) || (nfound == 0)) {
      fprintf(stderr, "ERROR: could not locate CALDB Mu-Tau Function Parameters\n");
      fprintf(stderr, "       for the spectrum flux type '%s'.\n", expr);
      fprintf(stderr, "       Batdrmgen may not support this spectrum flux type.\n");
      if (status != 0) return status;
      return -1;
    }

    /* Check for the extension number */
    sprintf(extstr,"[%ld]",extno[0]);
    strcat(parms->calfile,extstr);

    /* Now check for the Mu-tau values extension, which may be in the
       same or different file */
    expr = "-";
    codenam = "MT_VALUES";
    pfile = parms->mtvalfile;
    
    bat_caldb_search(&(resp.caldb), codenam, expr, maxret, PIL_PATH_MAX, 
		     &pfile,extno, &ponline, &nret, &nfound, &status);
    if ((status != 0) || (nret == 0) || (nfound == 0)) {
      fprintf(stderr, "ERROR: could not locate CALDB MT_VALUES file\n");
      if (status != 0) return status;
      return -1;
    }
    
    /* Check for the extension number */
    sprintf(extstr,"[%ld]",extno[0]);
    strcat(parms->mtvalfile,extstr);
    
  } else {

    /* ==== Not a CALDB file, tack on the extension number */
    strcat(parms->calfile,   "[1]");
    strcat(parms->mtvalfile, "[2]");
  }


  headas_chat(3,"  MTFUNC_PARMS file = %s\n", parms->calfile);
  headas_chat(3,"  MT_VALUES    file = %s\n", parms->mtvalfile);

  headas_chat(4,"Allocate memory for matrix itself\n");

  nmatrixcells = resp.nebins*resp.nphabins;
  resp.matrix = (double *) malloc(sizeof(double)*nmatrixcells);
  if (resp.matrix == 0) {
    fprintf(stderr, "ERROR: Could not allocate response matrix memory.\n");
    return MEMORY_ALLOCATION;
  }

  headas_chat(4,"Call calc_response to generate response matrix\n");

  if ((status = calc_response(parms, &resp)) != 0) {
    fprintf(stderr, "ERROR: Response matrix generation failed.\n");
    free(resp.matrix);
    return status;
  }


  headas_chat(4,"Call writeallresp to write response matrix to disk\n");

  if ((status = writeallresp(parms, &resp)) != 0) {
    fprintf(stderr, "ERROR: Could not write response matrix.\n");
    free(resp.matrix);
    return status;
  }

  headas_chat(4,"update RESPFILE keyword in PHA file\n");

  if ((status = update_respfile(infile,outfilename,parms->row)) != 0) {
    fprintf(stderr,"ERROR: update_respfile failed.\n");
    goto CLEANUP;
  }

 CLEANUP:
  /* Deallocate memory, and other cleanup operations */
  if (resp.matrix) free(resp.matrix);
  if (resp.emin) free(resp.emin);
  if (resp.emax) free(resp.emax);
  if (resp.emin_pre) free(resp.emin_pre);
  if (resp.emax_pre) free(resp.emax_pre);
  if (resp.energ_lo) free(resp.energ_lo);
  if (resp.energ_hi) free(resp.energ_hi);
  summary(parms);

  return status;
}


/* ----------------------------------------------------------------- */
/* Main point of entry for tool */
int batdrmgen(void)
{
  int status = 0;

  struct parm_struct parms;

  /* Register taskname and version. */

  set_toolname(taskname);
  set_toolversion(taskver);

  headas_chat(2, "----------Begin task batdrmgen -------------- \n");
  headas_chat(4, "First get the parameters using batdrmgen_getpar\n");

  if ((status = batdrmgen_getpar(&parms)) != 0) {
    fprintf(stderr, "Invalid task parameters were found\n");
    return status;
  }

  parms.taskname = &taskname[0];
  parms.taskver  = &taskver[0];

  return batdrmgen_work(&parms);

}

