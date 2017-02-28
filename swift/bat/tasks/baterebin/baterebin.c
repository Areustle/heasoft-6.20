#include <string.h>
#include <fitsio.h>
#include <ctype.h>
#include "pil.h"
#include "headas.h"
#include "imageutils.h"
#include "baterebin.h"

/* XXX kludge for calculation of residuals; 
   0 = use gpulseTokeV (i.e. ground calculated energy scale) 
   1 = use fpulseTokeV (i.e. flight calculated energy scale) */
#define RESID_ESCALE 0

static char taskname[] = "baterebin";
static char taskver[]  = "8.3";


/**************************** HISTORY **********************************
 Version 2.0 ---  Louis Barbier, NASA-GSFC  2003-Aug-20
 Version 2.1 ---  Dec. 5, 2003
 Version 3.0 --- Jay Cummings October 13, 2004
 	* Now does more than one row at a time
        * Added ebins paramter: Filename of text file containing number of energy
          bins and a list of the lower edges of the bins in the output file.
	* Does things in image order instead of detector number order
 Version 4.0 --- Jay Cummings February 18, 2005
        * Added a cubic correction, now the default.
 Version 5.0 --- Jay Cummings March 11, 2005
        * Added fixed-DAC method, now the default.
 Version 6.0 --- Jay Cummings March 30, 2005
        * Now copies the GTI extension of the input DPH to the output DPH
	* 
	* Moved the bin edge energy calculations to outside the row loop. This should
	* slightly speed up processing of multi-row DPHs.
	*
	* Changed which keywords are checked in the calfile to determine what the
	* gain and offset indices are. In some cases GAININD != LDPGAIN and/or
	* OFFINDEX != LDPOFFST, and LDPGAIN or LDPOFFST is not equal to the
	* numbers in the name of the file. This should make the warning about
	* the indices not matching appear less often.
	*
	* Changed the wording of warnings to make them seem less dire.
	* 
	* Now uses the row of the calfile that is closest in time to the
	* first DPH row instead of the first calfile row
	*
	*
Version 6.1 --- Jay Cummings April 28, 2005
        * Fixed a bug that would delete the incorrect number of rows from the
	* output EBOUNDS extension compared to the input EBOUNDS extension.
	* Previously the number of input energy bins was figured from the 
	* NAXIS2 keyword of the input DPH extension rather than the input EBOUNDS
	* extension. If the number of output energy bins was different, there
	* would be extra rows in the EBOUNDS extension filled with the bounds
	* from the input file (since this extension was originally copied from
	* the input file, then the new values were written in). This error
	* was seen when the number of output energy bins was less than the
	* number of DPH rows. Apparently somehow, I don't know how, the right
	* number of rows were written when the number of input DPH rows was less
	* than the number of output energy bins.
	*
	* Used to compare the low end of the highest input bin to the high end of
	* the highest output bin in order to determine whether to print a warning
	* message that they were different. Now compares the high end of both
	* bins. This change has no effect on the function of the program, just
	* whether the warning message is printed.
	*
	*
Version 6.2 -- C. Markwardt 24 Jun 2005
        * Make gain_offset structure static so that it doesn't clobber the
        * stack and confuse a lot of architectures. (it was ~2.5 MB)
        *
        * Also, free the large memory chunks at the end of the task.
Version 7.0 -- Jay Cummings July 2005
        * Added the ability to include an input detmask
	* Reduced the number of error messages
	* Reduced the sound of urgency of error messages
	* Added more information on in what way and how much pixels are not good
Version 7.1 -- Jay Cummings Aug 2005
        * Counts from the integral bins will be placed into the end output bins
	  if and only if the extreme energy of the corresponding output bin
	  is the minimum energy (0) or maximum energy (6553.6)
	* Counts from the integral bins were not placed in other than the
	  end output bins in previous versions and will not be in this version.
Version 7.2 -- Jay Cummings Nov 2005
	* Added the optional output detmask with values indicating various
	  potential criteria for masking detectors
	* Added two other parameters for output detmask that determine the
	  energy range checked for two potential exclusion criteria
Version 7.3 -- CBM 03 Apr 2007 
        * Added calls to fits_set_hdustruct() for safety against CFITSIO 
          taking a dump on the file structure.
Version 7.4 -- CBM 20 Apr 2007
        * Use more high-level functions to change the size of the EBOUNDS ext.
Version 7.5 -- CBM 03 Feb 2009
        * Move ebins parsing code to batutils, to reduce duplication.
Version 8.0 -- CBM 22 Jun 2009
        * Beginning of major overhaul, addition of calls to 
        * bat_read_cal_coeff() and bat_pha_to_keV()
        * but old code is currently still present
Version 8.2 -- CBM 23 Jun 2009
        * Removal of OLD_ECALC code, leaving only library energy conversion code

 **********************************************************************/

/************************ BRIEF DESCRIPTION ***************************
 This tool reads in a detector plane histogram; extracts the energy
  spectra (histograms) for all detectors; rebins those histograms
  based on quadratic corrections; outputs another detector plane histogram

 It is to be used on survey data only (not bursts).

 Some of this code is taken from other routines, such as bateconvert,
  batdph2dpi, etc.
 **********************************************************************/

#define TOOLSUB baterebin
#include "headas_main.c"

#define NUM_PIXELS (DAP_CELLS)


/* ----------------------------------------------------------------- */
/* ----------------------------------------------------------------- */
/* 
 * prepfile - prepare output file for writing
 *
 */
int prepfile(fitsfile *infile, fitsfile *outfile, 
	     long int ntimes, long int nchannels,
	     float * outemin, float * outemax, 
	     struct parm_struct *parms, int calmode)
{
  int status = 0;
  int dphndim = 3;
  long int dph_tdim[3];
  char tdimstr[FLEN_CARD];
  char tdimname[FLEN_CARD];
  char creator[FLEN_CARD];
  char tformstr[FLEN_CARD];
  long int tform;
  char comment[FLEN_CARD];
  long int oldtabwidth;
  char oldtformstr[FLEN_CARD];
  char outputdatatype;
  char dphtformname[FLEN_CARD];
  char history[FLEN_CARD];
  int dphcol;
  long int oldntimes;
  long oldnaxis2;
  int ncols;
  long int nrows;
  long int i;
  int emin_colnum, emax_colnum;
  char *p = 0;


    if(fits_movnam_hdu(infile, BINARY_TBL, "BAT_DPH", 0, &status)) {
        fprintf(stderr, "ERROR: Could not move input pointer to BAT_DPH HDU, status %d\n", status);
	return status;
    }

    if(fits_movnam_hdu(outfile, BINARY_TBL, "BAT_DPH", 0, &status)) {
        fprintf(stderr, "ERROR: Could not move output pointer to BAT_DPH HDU, status %d\n", status);
	return status;
    }

    if(fits_get_colnum(infile, CASEINSEN, "DPH_COUNTS", &dphcol, &status)) {
        fprintf(stderr, "ERROR: Could not find DPH_COUNTS colunm in input, status %d\n", status);
	return status;
    }

    if(fits_delete_col(outfile, dphcol, &status)) {
        fprintf(stderr, "ERROR: Could not delete original DPH colunm in output, status %d\n", status);
	return status;
    }

    dphndim = 3;
    dph_tdim[0] = nchannels;
    dph_tdim[1] = 286;
    dph_tdim[2] = 173;

    sprintf(tdimstr,"(%ld,%ld,%ld)",dph_tdim[0], dph_tdim[1], dph_tdim[2]);

    if(strcasecmp(parms->outdatatype, "SHORT") == 0) {
        outputdatatype = 'I';
	parms->wholecount = 1;
    }
    else if(strcasecmp(parms->outdatatype, "INT") == 0) {
        outputdatatype = 'J';
	parms->wholecount = 1;
    }
    else if(strcasecmp(parms->outdatatype, "FLOAT") == 0)
        outputdatatype = 'E';
    else if(strcasecmp(parms->outdatatype, "DOUBLE") == 0)
        outputdatatype = 'D';
    else {
	    fprintf(stderr,"ERROR: What kind of data is %s?\n", parms->outdatatype);
	    outputdatatype = 'X';
	    goto TFORMPROB;
    }

  headas_chat(4,"NUM_PIXELS = %d \n", NUM_PIXELS);
    tform = dph_tdim[0]*dph_tdim[1]*dph_tdim[2];
    sprintf(tformstr, "%ld%c", tform, outputdatatype);
  headas_chat(3, "New tform %s  \n", tformstr);

    if(fits_insert_col(outfile, dphcol, "DPH_COUNTS", tformstr, &status)) {
        fprintf(stderr, "ERROR: Could not insert new DPH colunm in output, status %d\n", status);
	return status;
    }
    fits_set_hdustruc(outfile, &status);
   
    if(fits_read_key(infile, TLONG, "NAXIS1", &oldtabwidth, comment, &status)) {
        fprintf(stderr, "ERROR: reading NAXIS1, status %d \n", status);
	    goto DPHHEADPROB;
    }

    sprintf(history, "Old table width (NAXIS1) was %ld", oldtabwidth);
    if(fits_write_history(outfile, history, &status)) {
        fprintf(stderr, "ERROR: writing history, status %d\n", status);
	    goto DPHHEADPROB;
    }

    if(fits_read_key(infile, TLONG, "NAXIS2", &oldntimes, comment, &status)) {
        fprintf(stderr, "ERROR: reading NAXIS2, status %d\n", status);
	    goto DPHHEADPROB;
    }

    sprintf(history, "Old number of rows was %ld", oldntimes);
    if(fits_write_history(outfile, history, &status)) {
        fprintf(stderr, "ERROR: writing history, status %d\n", status);
	    goto DPHHEADPROB;
    }

    sprintf(dphtformname, "TFORM%d", dphcol);
    if(fits_read_key(infile, TSTRING, dphtformname, oldtformstr, comment, &status)) {
        fprintf(stderr, "ERROR: reading %s, status %d\n", dphtformname, status);
	    goto DPHHEADPROB;
    }

    sprintf(history, "Old DPH tform (%s) was %s", dphtformname, oldtformstr);
    if(fits_write_history(outfile, history, &status)) {
        fprintf(stderr, "ERROR: writing history, status %d\n", status);
	    goto DPHHEADPROB;
    }
    fits_set_hdustruc(outfile, &status);

    if(fits_read_key(infile, TSTRING, "CREATOR", creator, comment, &status)) {
        fprintf(stderr, "ERROR: reading CREATOR, status %d\n", status);
	    goto DPHHEADPROB;
    }

    sprintf(history, "Old creator was %s", creator);
    if(fits_write_history(outfile, history, &status)) {
        fprintf(stderr, "ERROR: writing history, status %d\n", status);
	    goto DPHHEADPROB;
    }

    if(calmode == QUAD_METH) if(fits_update_key(outfile, TSTRING, "GAINMETH", "QUAD", "Quadratic ADU to energy correction applied by baterebin", &status)) {
        fprintf(stderr, "ERROR: writing GAINMETH keyword, status %d\n", status);
	goto DPHHEADPROB;
    }

    if(calmode == CUBIC_METH) if(fits_update_key(outfile, TSTRING, "GAINMETH", "CUBIC", "Cubic ADU to energy correction at fixed ADU applied by baterebin", &status)) {
        fprintf(stderr, "ERROR: writing GAINMETH keyword, status %d\n", status);
	goto DPHHEADPROB;
    }

    if(calmode == FIXEDDAC_METH) if(fits_update_key(outfile, TSTRING, "GAINMETH", "FIXEDDAC", "Cubic ADU to energy correction at fixed DAC applied by baterebin", &status)) {
        fprintf(stderr, "ERROR: writing GAINMETH keyword, status %d\n", status);
	goto DPHHEADPROB;
    }

    /* if(fits_write_tdim(outfile, dphcol, dphndim, dph_tdim, &status)) { */
    /* For some reason, there is no corresponding "fits_update_tdim" function */
    sprintf(tdimname,"TDIM%d",dphcol);
    if(fits_update_key(outfile, TSTRING, tdimname, tdimstr, "", &status)) {
        fprintf(stderr, "ERROR: writing tdim keys, status %d\n", status);
	goto DPHHEADPROB;
    }
    fits_set_hdustruc(outfile, &status);

    sprintf(creator, "%s %s", parms->taskname, parms->taskver);
    if(fits_update_key(outfile, TSTRING, "CREATOR", creator,
		  "Program that created this FITS file", &status)) {
        fprintf(stderr, "ERROR: writing CREATOR, status %d\n", status);
	goto DPHHEADPROB;
    }

    /* Write keywords to provide tracability for calibration coefficients */
    p = rindex(parms->calfile,'/');
    if (p == 0) p = parms->calfile; else p++;
    fits_update_key(outfile, TSTRING, "BCALFILE", p,
		    "BAT total linear gain/offset file name", &status);

    p = rindex(parms->residfile,'/');
    if (p == 0) p = parms->residfile; else p++;
    fits_update_key(outfile, TSTRING, "BRESFILE", p,
		    "BAT Residual from linear gain file name", &status);

    p = rindex(parms->pulserfile,'/');
    if (p == 0) p = parms->pulserfile; else p++;
    fits_update_key(outfile, TSTRING, "BPULFILE", p,
		    "BAT Ground Pulser DAC to keV file name", &status);

    p = rindex(parms->fltpulserfile,'/');
    if (p == 0) p = parms->fltpulserfile; else p++;
    fits_update_key(outfile, TSTRING, "BFLTFILE", p,
		    "BAT Flight Pulser DAC to keV file name", &status);

    if(fits_get_num_cols(infile, &ncols, &status)) {
	fprintf(stderr, "Couldn't get number of columns in input DPH extension\n");
	return status;
    }

    /* Copy all the other columns besides the DPH itself */

    for(i = 1; i <= ncols; i++) {
        if(i != dphcol) {
	    if(fits_copy_col(infile, outfile, i, i, 0, &status)) {
	        fprintf(stderr, "Couldn't copy column %ld of the DPH extension\n", i);
	        return status;
	    }
        }
    }

    /* Create EBOUNDS HDU */
    if(fits_movnam_hdu(infile, BINARY_TBL, "EBOUNDS", 0, &status)) {
        fprintf(stderr, "ERROR: Could not move input pointer to EBOUNDS HDU, status %d\n", status);
	return status;
    }

    if(fits_movnam_hdu(outfile, BINARY_TBL, "EBOUNDS", 0, &status)) {
        fprintf(stderr, "ERROR: Could not move output pointer to EBOUNDS HDU (status=%d)\n", status);
	return status;
    }

    /* How many rows in original EBOUNDS extension? */
    if (fits_get_num_rows(infile, &oldnaxis2, &status)) {
      fprintf(stderr, "ERROR: reading number of EBOUNDS rows (status=%d)\n", status);
      goto EBHEADPROB;
    }

    /* Shrink the extension if necessary */
    if(oldnaxis2 > nchannels) {
        nrows = oldnaxis2 - nchannels;
        if(fits_delete_rows(outfile, 1, nrows, &status)) {
            fprintf(stderr, "Couldn't delete %ld rows from EBOUNDS extension\n",
		    nrows);
	    return status;
        }
    }

    /* Update header structure to be safe */
    fits_set_hdustruc(outfile, &status);
    
    sprintf(history, "Old number of bins was %ld", oldnaxis2);
    if(fits_write_history(outfile, history, &status)) {
        fprintf(stderr, "ERROR: writing history, status %d\n", status);
	goto EBHEADPROB;
    }

    /* Write EBOUNDS extension */
    headas_chat(4,"Writing ebounds data\n");

    fits_get_colnum(outfile, CASEINSEN,"E_MIN", &emin_colnum, &status); 
    fits_get_colnum(outfile, CASEINSEN,"E_MAX", &emax_colnum, &status);
    
    status = fits_write_col(outfile, TFLOAT, emin_colnum, 1, 1, nchannels, outemin, &status);
    if (status != 0) {
        fprintf(stderr, "ERROR: Could not write EBOUNDS emin to output\n");
        return status;
    }
    status = fits_write_col(outfile, TFLOAT, emax_colnum, 1, 1, nchannels, outemax, &status);
    if (status != 0) {
        fprintf(stderr, "ERROR: Could not write EBOUNDS emax to output\n");
        return status;
    }
    fits_set_hdustruc(outfile, &status);

    return status;

DPHHEADPROB:

    if (status != 0) {
      fprintf(stderr, "ERROR: The output DPH header could not be modified: status %d\n", status);
    }
    return status;

EBHEADPROB:

    if (status != 0) {
      fprintf(stderr, "ERROR: The output EBOUNDS header could not be modified: status %d\n", status);
    }
    return status;

TFORMPROB:

    fprintf(stderr, "Data types not as expected\n");
    return status;

}



/* ----------------------------------------------------------------- */
/* ----------------------------------------------------------------- */

int baterebin (void)
{

  fitsfile *infptr = 0;
  fitsfile *outfptr = 0;
  fitsfile *outmapptr = 0;
  fitsfile *maskfile = 0;
  struct parm_struct parms;
  FILE *efptr = 0;
  int i, j, status;
  long int nchannels, ntimes, nusetimes;
  int dphcol,timecol,expcol;
  int emin_colnum, emax_colnum;
  int gainindcol, offindcol;
  int gainindex[NMAX_ROWS], offindex[NMAX_ROWS];
  float *dphrow = 0;        /* the input energy spectra       */
  /* Current DPHs are integers */
  float *newdphrow = 0;         /* the rebinned spectra           */
  float *emin = 0;
  float *emax = 0;               /* input energy ranges, min and max        */
  float *corremin = 0;
  long int nebins;
  float outemin[NMAX_EBINS];
  float outemax[NMAX_EBINS];
  double time,exposure;
  long int row = 0;                  /* Which input row we are on */
  long int ncurrrow = 0;             /* Which output row we are on */
  int numrowranges; 
  long int rowrangemin[NMAX_ROWS], rowrangemax[NMAX_ROWS];
  float loweminout;
  float higheminout;
  float lowemin;
  float highemin;
  int lowdet = -1, highdet = -1, nlow = 0, nhigh = 0;
  static struct det_qual_struct detqual;
  static bat_ecal_data caldata;
  char gainmeth[FLEN_CARD];
  char comment[FLEN_CARD];
  int n_hdu;
  int safestatus;
  struct image_struct *detmask = 0; /* Input mask */
  struct image_struct *outmap = 0; /* Output mask */
  float *outimg = 0;
  int gmeth;
    
 
  /* register taskname and tool version */
  set_toolname(taskname);
  set_toolversion(taskver);
  /* CREATOR keyword */
  
  headas_chat(4, "baterebin -  declarations done  \n");

  if ((status=baterebin_getpar(&parms, taskname, taskver)) != 0) {  /* get input parameters */
      fprintf(stderr, "ERROR: could not parse parameter list\n");
      return status;
  }

  /* -------------- */


  /* Read detector mask file */
  if (parms.detmask[0] != 0) {
    headas_chat(4, "...reading detector mask image...\n");
    fits_open_image(&maskfile, parms.detmask, READONLY, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not open %s\n", parms.detmask);
      return status;
    }

    detmask  = image_read(maskfile, &status);

    /* Read GOODVAL keyword.  If the keyword is not found, then assume
       that 1=good, 0=bad.  If the keyword is found, then use that
       value for good, and anything else for bad. */
    safestatus = 0;
    fits_read_key(maskfile, TINT, "GOODVAL", &(parms.goodval), 0, &safestatus);
    if (safestatus) parms.goodval = 0;

    fits_close_file(maskfile, &status);
    headas_chat(4, "    (%dx%d image)\n", detmask->axes[0], detmask->axes[1]);
    if (status) {
      fprintf(stderr, "ERROR: could not read detector mask data from %s\n", 
	      parms.detmask);
      return status;
    }

    headas_chat(4, "...checking detector mask image...\n");
    if (detmask->axes[0] != 286 || 
		  (detmask->axes[1] != 173) ) {
      fprintf(stderr, 
	 "ERROR: Detector mask must have same dimensions as detector image\n");
      return BAD_DIMEN;
    }

  }

  outmap = image_init(286, 173, outimg);

  headas_chat(4,"Opening DPH file %s\n",parms.infile);
  /* Open the input DPH file */
  if (fits_open_file(&infptr,parms.infile,READONLY,&status)) {
	  fprintf(stderr, "Error opening infile %s\n", parms.infile);
	  goto CLEANUP;        
  }
  if (fits_movnam_hdu(infptr,BINARY_TBL,"BAT_DPH",0,&status)) {
	  fprintf(stderr, "ERROR: Could not find table 'BAT_DPH' in infile\n");
	  goto CLEANUP;
  }

  if(fits_read_key(infptr, TSTRING, "GAINMETH", gainmeth, comment, &status)) {
	  fprintf(stderr, "ERROR: Could not read the GAINMETH keyword in the input file\n");
	  fprintf(stderr, "so we don't know if the file has already been corrected.\n");
	  goto CLEANUP;
  }
  if(strcmp(gainmeth, "FLIGHT") != 0) {
	  fprintf(stderr, "ERROR: Existing DPH GAINMETH keyword is not 'FLIGHT', it is %s\n", gainmeth);
          fprintf(stderr, "Start from the flight file, rather than this one.\n");
	  goto CLEANUP;
  }

  /* Get the number of rows in the input file */
  headas_chat(4,"Getting number of rows\n");
  if (fits_get_num_rows(infptr,&ntimes,&status)) {
	  fprintf(stderr, "Error getting number of rows (times) in infile\n");
	  goto CLEANUP;
  }
  headas_chat(3,"Number of rows (time intervals) in input is %d\n",ntimes);
  headas_chat(4," Request to read rows: %s\n",parms.rowstr);

  /* Parse the 'rows' string */
  headas_chat(4, "...parsing 'rows'...\n");
  if (fits_parse_range(parms.rowstr, ntimes, NMAX_ROWS,
                     &numrowranges, rowrangemin, rowrangemax, &status)) {
      fprintf(stderr, "ERROR: could not parse 'rows' parameter ('%s')\n",
	             parms.rowstr);
      return status;
  }
  headas_chat(4, "   (found %d ranges, starting at %ld, ending at %ld)\n",
                     numrowranges, rowrangemin[0], rowrangemax[numrowranges-1]);
  
  nusetimes = 0;
  for (j=0; j<numrowranges; j++)   {
	  if((rowrangemax[j] > 0) && (rowrangemin[j] > 0)) {
  	       nusetimes += rowrangemax[j]-rowrangemin[j]+1;
	  }
  }
  headas_chat(3, "Number of rows (time intervals) expected in output %ld\n", nusetimes);
	
  if (!status && (nusetimes > 0)) {

 /* create a new file for the output */
    headas_clobberfile(parms.outfile);     /* delete if already existing */
    fits_create_file(&outfptr, parms.outfile, &status);
    if (status) {
      fprintf(stderr, "ERROR: Could not open output file %s for writing\n",
		                  parms.outfile);
      goto CLEANUP;
    }

    /* Copy the original file to the output file. */
    /* This is easier than writing from scratch. */
    fits_copy_file(infptr, outfptr, 1, 1, 1, &status);  

    if (status) {
        fprintf(stderr, "ERROR: Could not copy input file to output file, status %d\n", status);
	return status;
    }

    fits_get_num_hdus(outfptr, &n_hdu, &status);
    headas_chat(5, "%d HDUs in outfile resulting from copy of infile\n",n_hdu);

    /* Figure out the number of energy channels from the EBOUNDS extension. */
    if (fits_movnam_hdu(infptr,BINARY_TBL,"EBOUNDS",0,&status)) {
        fprintf(stderr, "Error moving to EBOUNDS\n");
	goto CLEANUP;
    }
    if (fits_get_num_rows(infptr,&nchannels,&status)) {
        fprintf(stderr, "Error getting number of rows (energy bins) in infile\n");
	goto CLEANUP;
    }
    headas_chat(3,"Number of input energy channels is %d\n",nchannels);

/* get the column number of the columns containing EMIN and EMAX values */
    fits_get_colnum(infptr, CASEINSEN,"E_MIN", &emin_colnum, &status); 
    fits_get_colnum(infptr, CASEINSEN,"E_MAX", &emax_colnum, &status);
    headas_chat(4,"emin/ emax column numbers are: %d  %d \n", emin_colnum, emax_colnum);
    
 /* Space for the emin and emax values            */
    emin = malloc(nchannels*sizeof(float));
    emax = malloc(nchannels*sizeof(float));
    
 /* Move to the correct HDU in the input file. The file must contain
     a "BAT_DPH" extension.*/
    if (fits_movnam_hdu(infptr,BINARY_TBL,"BAT_DPH",0,&status)) {
        fprintf(stderr, "Error moving to BAT_DPH in infile\n");
	goto CLEANUP;
    }

 /* Get the column numbers for the input file.  The column numbers
    that are read are detector dph, time and exposure.            */
    if (fits_get_colnum(infptr,CASEINSEN,"DPH_COUNTS",&dphcol,&status)) {
        fprintf(stderr, "Error getting dph counts column\n");
	goto CLEANUP;
    }
    if (fits_get_colnum(infptr,CASEINSEN,"TIME",&timecol,&status)) {
        fprintf(stderr, "Error getting times column\n");
	goto CLEANUP;
    }
    if (fits_get_colnum(infptr,CASEINSEN,"EXPOSURE",&expcol,&status)) {
        fprintf(stderr, "Error getting exposure column\n");
	goto CLEANUP;
    }

    row = rowrangemin[0];  /* get the time of the first row to be used */
    /* This will be used to choose the row of the calfile to use */

    fits_read_col(infptr,TDOUBLE,timecol,row,1, 1,0,&time,0,&status);

    /* Optionally read the gain/offset indices (but don't die if they aren't there) */
    gainindex[0] = -1; offindex[0] = -1;
    fits_write_errmark();
    fits_get_colnum(infptr,CASEINSEN,"GAIN_INDEX",&gainindcol,&status);
    fits_get_colnum(infptr,CASEINSEN,"OFFSET_INDEX",&offindcol,&status);
    fits_read_col(infptr,TINT,gainindcol,1,1,ntimes,0,gainindex,0,&status);
    fits_read_col(infptr,TINT,offindcol,1,1,ntimes,0,offindex,0,&status);
    fits_clear_errmark();
    status = 0;

    headas_chat(4,"The time is in column number %d \n",timecol);
    headas_chat(4,"The exposure is in column number %d \n",expcol);
    headas_chat(4,"The DPH is in column number %d \n",dphcol);

 /*  Read in the emin and emax values now    */
    if (fits_movnam_hdu(infptr,BINARY_TBL,"EBOUNDS",0,&status)) {
        fprintf(stderr, "Error moving to EBOUNDS in infile\n");
	goto CLEANUP;
    }
    headas_chat(4,"finding input ebounds columns\n");
    fits_read_col(infptr,TFLOAT,emin_colnum,1,1,nchannels, NULL, emin, NULL, &status);
    fits_read_col(infptr,TFLOAT,emax_colnum,1,1,nchannels, NULL, emax, NULL, &status);
    for (i=0;i< nchannels; i++) {
	headas_chat(5,"Input Emin bin %d = %9.5e Emax= %9.5e \n",i, emin[i],emax[i]);
	if((i > 0) && (emax[i-1] != emin[i])) {
		fprintf(stderr,"WARNING: Input energy bins are not contiguous: ");
		fprintf(stderr,"emax[%d] = %f != emin[%d] = %f\n", i-1, emax[i-1], i, emin[i]);
	}
    }

    if((strcasecmp(parms.ebins, "FILEBINS")) != 0) {

        nebins = read_ebins(parms.ebins, outemin, outemax, 4096, "PHA");

        if (nebins <= 0) {
            fprintf(stderr, "ERROR: could not parse energy bins\n");
            status = RANGE_PARSE_ERROR;
	    goto CLEANUP;
        }

        for(i = 0;i < nebins; i++) {
            if((i > 0) && (outemax[i-1] != outemin[i])) {
                  fprintf(stderr,"WARNING: Input energy bins are not contiguous: ");
                  fprintf(stderr,"emax[%d] = %f != emin[%d] = %f\n", i-1, emax[i-1], i, emin[i]);
            }
            if(i > 0) if(outemin[i] < outemin[i-1]) {
                  fprintf(stderr,"ERROR: Output energy bin %d minimum is less than energy bin %d:\n", i, i-1);
                  fprintf(stderr,"outemin[%d] = %f, outemin[%d] = %f\n", i-1, outemin[i-1], i, outemin[i]);
		  goto CLEANUP;
            }
        }
        if(outemin[0] != emin[0]) {
              headas_chat(2, "Lower bound of lowest energy output bin is not equal to lower bound of lowest input bin.\n");
              headas_chat(2, "Some counts may be left out. Contents of the low integral bin will be ignored.\n");
        }
	else {
              headas_chat(1, "Contents of the low integral bin will be placed in the lowest energy output bin.\n");
              headas_chat(2, "Their energy is not well defined, but it may be that their real energy was in the range\n");
              headas_chat(2, "of higher output bins for some detectors.\n");
	}
        if(outemax[nebins-1] != emax[nchannels-1]) {
              headas_chat(2, "Upper bound of highest energy output bin is not equal to upper bound of highest input bin.\n");
              headas_chat(2, "Some counts may be left out. Contents of the high integral bin will be ignored.\n");
        }
	else {
              headas_chat(1, "Contents of the high integral bin will be placed in the highest energy output bin.\n");
              headas_chat(2, "Their energy is not well defined, but it may be that their real energy was in the range\n");
              headas_chat(2, "of lower output bins for some detectors.\n");
	}

    } else {      /* Set output ebounds equal to input ebounds */
	    nebins = nchannels;
	    for(i=0;i< nchannels;i++) {
		    outemin[i] = emin[i];
		    outemax[i] = emax[i];
	    }
            headas_chat(2, "Output energy bins are the same as input.\n");
            headas_chat(1, "Contents of the low integral bin will be placed in the lowest energy output bin.\n");
            headas_chat(2, "Their energy is not well defined, but it may be that their real energy was in the range\n");
            headas_chat(2, "of higher output bins for some detectors.\n");
            headas_chat(1, "Contents of the high integral bin will be placed in the highest energy output bin.\n");
            headas_chat(2, "Their energy is not well defined, but it may be that their real energy was in the range\n");
            headas_chat(2, "of lower output bins for some detectors.\n");
    }
    least_E = emin[0];
    greatest_E = emax[nchannels-1];

   /* read the gain/offset tables and fill the gain/offset structure */
    lowemin = emin[1];
    highemin = emin[nchannels-1];
    loweminout = parms.lowecare;
    higheminout = parms.highecare;
    if(loweminout == 10.0) loweminout = outemin[1];
    if(higheminout == 194.9) higheminout = outemin[nebins-1];

    memset(&detqual, 0, sizeof(detqual));
    { 
      int calmode = INDEF_METH;
      bat_read_cal_coeff(time, parms.calfile, parms.residfile, parms.residext, 
			 parms.pulserfile, parms.pulserext, 
			 parms.fltpulserfile, parms.fltpulserext, 
			 &calmode, &caldata);
      bat_caldata_adjust(&caldata, 2);
    }

    if(status) {
      fprintf(stderr,"ERROR: Calibration files not read correctly, status %d\n",status);
      goto CLEANUP;
    }

    gmeth = caldata.gain_meth;

    if (gmeth == QUAD_METH) {
      headas_chat(2,"Quadratic gain correction method used\n");
    } else if (gmeth == CUBIC_METH) {
      headas_chat(2,"Cubic gain correction at fixed ADU (old) method used\n");
    } else if (gmeth == FIXEDDAC_METH) {
      headas_chat(2,"Cubic gain correction at fixed DAC method used\n");
    }

    headas_chat(2,
		"Pulser file gain index number        = %d\n"
		"Input file gain index number (row 1) = %d\n",
		caldata.gainindex, gainindex[0]);
    headas_chat(2,
		"Pulser file offset index number        = %d\n"
		"Input file offset index number (row 1) = %d\n",
		caldata.offsetindex, offindex[0]);
    if((caldata.gainindex != gainindex[0]) || (caldata.offsetindex != offindex[0])) {
	    headas_chat(1, "WARNING: Gain/offset index numbers in DPH row and in calfile keywords don't match. \n");
	    headas_chat(1, "         This is not a serious problem, under most circumstances. \n");
    }

    /* Sanity checking of calibration values */
    status = cal_value_check(&caldata, 
			     &detqual, &lowemin, &highemin, 
			     loweminout, higheminout, &parms, 
			     &nlow, &nhigh, &lowdet, &highdet, detmask, outmap);

    if(status) {
      fprintf(stderr,"ERROR: Calibration values not set correctly, status %d\n",status);
      goto CLEANUP;
    }

    /* No output Emin should be less than the highest input Emin; because the size of the
     * last energy bin is really indeterminate, we shouldn't redistribute any counts in it.
     * Actually, it is worse than that, the highest output Emin should be less than the
     * CORRECTED highest input Emin, but we don't know that a priori. Hence, the last 2
     * bins in the output may be wrong if the corrected highest input Emin goes lower than
     * the highest output Emin. 
     * The same thing goes for the first energy bin. */

    if(highemin < outemin[nebins-1]) {
      headas_chat(1, "Notice: Output highest energy bin has higher minimum energy (%f keV) than the\n", outemin[nebins-1]);
      headas_chat(1, "minimum energy of the highest input bin for %d detectors. \n", nhigh);
      headas_chat(2, "For such detectors, the output in the highest energy bins may include\n");
      headas_chat(2, "events of higher energy than the nominal maximum for those bins.\n");
      headas_chat(2, "You could make the minimum of the highest energy bin lower to avoid this.\n");
      headas_chat(2, "Corrected low end of highest input bin for pixel %d was %f keV\n", highdet, highemin);
    }
    if(lowemin > outemin[1]) {
      headas_chat(1, "Notice: Output lowest energy bin has lower maximum energy (%f keV) than the\n", outemin[1]);
      headas_chat(1, "maximum energy of the lowest input bin for %d detectors. \n", nlow);
      headas_chat(2, "For such detectors, the output in the lowest energy bins may include\n");
      headas_chat(2, "events of lower energy than the nominal minimum for those bins.\n");
      headas_chat(2, "You could make the maximum of the lowest energy bin higher to avoid this.\n");
      headas_chat(2, "Corrected high end of lowest input bin for pixel %d was %f keV\n", lowdet, lowemin);
    }
    headas_chat(2, "Number of output energy bins is %d\n", nebins);
    if(nebins < 1) {
      fprintf(stderr, "ERROR: number of output energy bins is now < 1, %ld\n", nebins);
      goto CLEANUP;
    }
    
    for (i=0;i< nebins; i++) {
      headas_chat(5,"Output Emin bin %d = %9.5e Emax= %9.5e \n", i, outemin[i],outemax[i]);
    }   	
    
    /* Now allocate the array to hold the DPH and output spectra    */
    dphrow    = (float *) malloc((unsigned) NUM_PIXELS*nchannels*sizeof(float));
    newdphrow = (float *) malloc((unsigned) NUM_PIXELS*nebins*sizeof(float));

    /* Now allocate the array to hold the corrected energies of the input spectra    */
    corremin = (float *) malloc((unsigned) NUM_PIXELS*(nchannels+1)*sizeof(float));
    
    headas_chat(4,"Finished allocating arrays for spectra\n");
    
    /* Calculate the "true" energies corresponding to the nominal flight energy */
    status = calc_true_e(emin, emax, corremin, nchannels,
			 nebins, &caldata, &detqual);
    if(status != 0) {
      fprintf(stderr, "Error calculating corrected energies\n");
      goto CLEANUP;
    }
    
    if (prepfile(infptr, outfptr, nusetimes, nebins, 
		 outemin, outemax, &parms, gmeth)) {
	fprintf(stderr, "ERROR: prepfile returned error\n");
	goto CLEANUP;
    }

    headas_chat(4, "Back from prepfile(), starting for(row) loop\n");

    nrowsin = ntimes;
   
    for(row = 1; row <= ntimes; row++) {
      
        /* Check to see if this row is in the requested row ranges */
      for (j=0; j<numrowranges; j++) { 
	if ((row >= rowrangemin[j]) && (row <= rowrangemax[j])) break;
      }
      /* This row was not found */
      if (j == numrowranges) {
	headas_chat(4, "   (skipping row %d)\n", row);
	if (fits_movnam_hdu(outfptr,BINARY_TBL,"BAT_DPH",0,&status)) {
	  fprintf(stderr, "Error moving to BAT_DPH in outfile\n");
	  goto CLEANUP;
	}
	if(fits_delete_rows(outfptr, ncurrrow+1, 1, &status)) {
	  fprintf(stderr, "Couldn't delete row in DPH extension: input row %ld, current output row %ld\n", row, ncurrrow+1);
	  goto CLEANUP;
	}
	continue;
      }
      
      ncurrrow++;
      
      headas_chat(2, "   (reading row %d)\n", row);
      
      headas_chat(4, "ncurrrow = %ld\n", ncurrrow);
      
      if (fits_movnam_hdu(infptr,BINARY_TBL,"BAT_DPH",0,&status)) {
	fprintf(stderr, "Error moving to BAT_DPH in outfile\n");
	goto CLEANUP;
      }

      /* Read in the input DPH and other associated values.*/
      fits_read_col(infptr,TFLOAT,dphcol,row,1,
		    NUM_PIXELS*nchannels,0,dphrow,0,&status);
      /* Input DPHs are integers, but fits_read_col is supposed to do coversions for numerical types */
      headas_chat(4,"read dphrow, status returned =  %d \n",status);
      fits_read_col(infptr,TDOUBLE,timecol,row,1, 1,0,&time,0,&status);
      
      fits_read_col(infptr,TDOUBLE,expcol, row,1, 1,0,&exposure,0,&status);
      headas_chat(3,"Time and exposure are %lf %lf\n",time,exposure);
      
      totalexp += exposure;
      
      /* Do the corrections to the spectra                                              */
      
      status = baterebin_work( corremin, outemin, outemax, dphrow, newdphrow, nchannels,
			       nebins, parms.wholecount, &caldata, &detqual, outmap); 
      
      headas_chat(4,"Finished baterebin_work, in row %ld, out row %ld\n", row, ncurrrow);
      
      /* write new DPH column here   */
      
      if (fits_movnam_hdu(outfptr,BINARY_TBL,"BAT_DPH",0,&status)) {     
	fprintf(stderr, "Error moving to BAT_DPH in outfile\n");
	goto CLEANUP;
      }
      fits_write_col(outfptr, TFLOAT, dphcol, ncurrrow, 1, nebins*NUM_PIXELS,
		     newdphrow, &status);
      /* DPH_COUNTS */
      if (status != 0) {
	fprintf(stderr, "ERROR: Could not write DPH data to output, row %ld\n", ncurrrow);
	goto CLEANUP;
      }
      
      /* ------------------- */
      
    }   /* for(row) */
    
    if(ncurrrow != nusetimes) {
      fprintf(stderr, "ERROR: Number of rows found not the total number in row ranges, %ld != %ld\n",
	      ncurrrow, nusetimes);
      goto CLEANUP;
    }
    nrowsout = ncurrrow;
    
  }   /* if(!status) */
  
  /* ------------------- */
  
  status = HDpar_stamp(outfptr, 0, &status);
  if (status) {
    fprintf(stderr, "ERROR: Can't stamp outfile with history, status %d\n", status);
    goto CLEANUP;
  }

  fits_close_file(outfptr, &status);
  outfptr = 0;


  /* ------------------- */
  /* Write output quality map */
  if(parms.outmap[0] != 0) {
    headas_clobberfile(parms.outmap);     /* delete if already existing */
    fits_create_file(&outmapptr, parms.outmap, &status);
    if (status) {
      fprintf(stderr, "ERROR: Could not open outmap file %s for writing\n",
	      parms.outmap);
      goto CLEANUP;
    }
    
    outmap->nullval = -1e37;
    image_write(outmapptr, outmap, &status);
    image_writeinfo(outmapptr, outmap, &status);  /* Write time keywords */
    
    fits_write_comment(outmapptr, "  ========================================================", &status);
    fits_write_comment(outmapptr, "  bit 1 indicates high edge of input integral bin is more than ", &status);
    fits_write_comment(outmapptr, "        the low edge of the lowest energy non integral output bin", &status);
    fits_write_comment(outmapptr, "        that you care about (default 14.0 keV)", &status);
    fits_write_comment(outmapptr, "  bit 2 indicates low edge of input integral bin is less than ", &status);
    fits_write_comment(outmapptr, "        the high edge of the highest non integral output bin", &status);
    fits_write_comment(outmapptr, "        that you care about (default 151.4 keV)", &status);
    fits_write_comment(outmapptr, "  bit 3 (4) indicates correction > 50 keV in magnitude", &status);
    fits_write_comment(outmapptr, "  bit 4 (8) indicates wacked gain/offset values", &status);
    fits_write_comment(outmapptr, "  bit 5 (16) indicates no V -> energy value available", &status);
    fits_write_comment(outmapptr, "  bit 6 (32) indicates detector masked by input detmask", &status);
    fits_write_comment(outmapptr, "  ========================================================", &status);

    status = HDpar_stamp(outmapptr, 0, &status);
    if (status) {
	  fprintf(stderr, "ERROR: Can't stamp outmap with history, status %d\n", status);
	  goto CLEANUP;
    }
  }

  if(parms.outmap[0] != 0) {
    fits_close_file(outmapptr, &status);
    outmapptr = 0;
  }

  /* Print out summary information if we get here successfully    */
  status=baterebin_printsummary(&parms,taskname,taskver);
  if (status) goto CLEANUP;

  headas_chat(4,"Done, just close files and exit!\n");
  if (outfptr) fits_close_file(outfptr,&status);
  headas_chat(4,"closed output file \n");
  if (infptr) fits_close_file(infptr, &status);
  headas_chat(4,"closed input file \n");
 
  headas_chat(3,"Ending, status = %d\n", status);
  
  if (dphrow) free(dphrow);
  if (newdphrow) free(newdphrow);
  if (corremin) free(corremin);
  return(status); 
 
CLEANUP:
  headas_chat(4,"At CLEANUP, status %d\n", status);
  if (status)  fits_report_error(stderr, status);
  status = 0;
  if (efptr) status = fclose(efptr);
  if (status)  {
	  fprintf(stderr, "Error at CLEANUP while closing efptr\n");
	  fits_report_error(stderr, status);
  }
  status = 0;
  if (outfptr) fits_close_file(outfptr, &status);
  if (status)  {
	  fprintf(stderr, "Error at CLEANUP while closing outfptr\n");
	  fits_report_error(stderr, status);
  }
  status = 0;
  if (infptr)  fits_close_file(infptr, &status);
  if (status)  {
	  fprintf(stderr, "Error at CLEANUP while closing infptr\n");
	  fits_report_error(stderr, status);
  }
  status = 0;
  if (dphrow) free(dphrow);
  if (newdphrow) free(newdphrow);
  if (corremin) free(corremin);
  return (status); 

}



/* ----------------------------------------------------------------- */
/* ----------------------------------------------------------------- */
/* Interpolate the nominal flight energy bins to "true" energy bins */
int calc_true_e(
	float *min_e, /* Nominal values of the low edge of the input energy bins */
	float *max_e, /* Nominal values of the high edge of the input energy bins */
	float *corremin, /* Correct values of the input energy bins */
	long int nchannels, /* Number of input energy bins */
	long int out_nchannels, /* Number of output energy bins */
	bat_ecal_data *caldata, 
	struct det_qual_struct *detqual
	)
{
  int status = 0;
  int i,j;
  float xpha[256];    /* Intermediate pulse-heights corresponding to bin edge energies */
  float *xenergy;     /* Calculated "true" energies for each nominal bin edge energy */


  /* convert energy bins to new bins using gain / offset parameters */
  for(j=0; j<NUM_PIXELS; j++) {
    if(detqual->flag[j] != GOOD_GAINS) continue;
    xenergy = &(corremin[j*(nchannels+1)]);

    bat_flight_energy_to_pha(min_e, xpha, nchannels, 
			     caldata->ftotgain[j], caldata->ftotoffset[j], 1);
    bat_pha_to_energy(xpha, xenergy, nchannels, 
		      caldata->gain_meth, 
		      caldata->ftotgain[j], caldata->ftotoffset[j], 
		      caldata->fpulseTokeV[j], caldata->fpulse0keV[j], 
		      caldata->gpulseTokeV[j], caldata->gpulse0keV[j], 
		      caldata->DAClow, 
		      caldata->gpulresid0[j], caldata->gpulresid1[j], 
		      caldata->gpulresid2[j], caldata->gpulresid3[j], 
		      caldata->gpul_nom_offset[j], caldata->gpul_nom_gain[j], 
		      RESID_ESCALE, 
		      &status);
    if (status) {
      fprintf(stderr, "ERROR: fatal energy conversion error in findecorr()\n");
      return status;
    }

    /* Last bin is i+1 */
    xenergy[nchannels] = max_e[nchannels-1];

    /* Check for non-linear correction: if successive channels no longer increase */
    for (i=0; i<nchannels; i++) {
      if (xenergy[i+1] < xenergy[i]) {
	detqual->flag[j] = GAINOFF_OUTOFRNG;
      }
    }
  }

#if (RESID_ESCALE == 1)
  fprintf(stderr, "WARNING: Using obsolete fpulseTokeV energy scale\n");
#endif

  return (status);

}

/* ----------------------------------------------------------------- */
/* ----------------------------------------------------------------- */
 
int baterebin_work(
	float *cmine, /* Correct values of the low edge of the input energy bins */
	float *out_min_e, /* Values of the low edge of the output energy bins */
	float *out_max_e, /* Values of the high edge of the output energy bins */
	float *spectra, /* Numbers in the input energy bins */
	float *respectra, /* Numbers in the output energy bins */
	long int nchannels, /* Number of input energy bins */
	long int out_nchannels, /* Number of output energy bins */
	int  whole, /* Move only whole counts? */
	bat_ecal_data *caldata, 
	struct det_qual_struct *detqual,
	struct image_struct *outmap
	)
{
  int status = 0;
  int i,j,k,lowest;
  int m, n, q, r;

  /* Actual values of the low edge of the input energy bins */
  float corrmin_e[NMAX_EBINS+1]; 

  float moven;
  long int odet, idet;
  float e_width,e_fract;
  float idetsum;
  float odetsum;
  float epsilon;
  int ks_i_adds_to[20];
  float frac_of_i_in_k[20];
  float leftover;
  float accounti;
  float fracleft[20];
  int fracorder[20];
  int imoven;
  int nwarns = 0;
  int nwarns2 = 0;
  int nnulls = 0;
  int nullval = 0;
  int ngainerr = 0;
  int notdis = 0;

  epsilon = 0.01;
 /* do the re-binning here */
 /* assumption is that the energy bins are in ascending order - 
  * later may want to modify this to allow for descending order */
 /* also assume there are no gaps in energy bins, i.e. they are contiguous        */   
 /* find which old bins (indexed by i) contribute to the new bins (indexed by k)  */

 /* initialize new spectra first */
  for(j=0; j<NUM_PIXELS*out_nchannels; j++)
		 respectra[j] = 0.0;

  for(j=0; j<NUM_PIXELS; j++) {

      odet = j*out_nchannels;
      idet = j*nchannels;

      if(detqual->flag[j] != GOOD_GAINS) {
	notdis = 0;
	if(outmap->data[j] == 0) outmap->data[j] = 8;
	if(detqual->flag[j] != MASKED_OUT) {
	  for(i = 0; i<nchannels;i++) {
	    if(spectra[idet+i] > 0) {
	      notdis = 1;
	      break;
	    }
	  }
        }
	if(notdis == 1) {
          if( ngainerr < 10) {
	    headas_chat(4,"Pixel %d has bad gain values; skipping this detector\n",j);
          } else if(ngainerr == 10) {
	    headas_chat(3,"Number of pixels with bad gain values > 10, further error messages skipped.\n");
          } else if(ngainerr == 1000) {
	    headas_chat(1,"Warning, you have a lot of bad gain values: \n     Check that your calfile (gain/offset file) is not missing data or is otherwise weird.\n");
	  }

	  ngainerr  ++;
	}
	continue;
      } else {

	  idetsum = 0;
	  odetsum = 0;
          nullval = 0;

          for (i=0; i<nchannels; i++) {
	      if(spectra[idet+i] < 0)
		 nullval = 1;
	      idetsum += spectra[idet+i];
              corrmin_e[i] = cmine[j*(nchannels+1) + i];
          }
	  if(nullval == 1)  { 
	    nnulls++;
	    if(nnulls < 10) {
              headas_chat(2,"Detector %ld has null values in at least some of its energy bins\n",j);
	    } else if(nnulls == 10) headas_chat(2,"At least 10 detectors have null values, further messages skipped\n");
	  }
          corrmin_e[nchannels] = cmine[(j+1)*(nchannels+1)-1];

	  if(whole == 0) {
	      /* first output bin always includes all of first input bin */
/* NO, IT DOESN'T!! * This has led to a bug of always putting integral bin counts in the end bins,
 * even if they do not contain all energies */
	      if(spectra[idet+0] > 0) {
      /* Instead, include the integral bin counts only if the bottom energy is equal to the input
       * bottom energy */
		if(out_min_e[0] == least_E) {
		  respectra[odet+0] += spectra[idet+0];
                  odetsum += spectra[idet+0];
	        }
	      }
              lowest = 1;           /* start at low edge of second bin in old histogram */
	      while(corrmin_e[lowest+1] <= out_min_e[1]) {
		      /* While the lowest bins in the input are all below the second bin's 
		       * energy in the new histogram, add them into the lowest bin.
		       * This includes "negative" energies, of which there are quite a
		       * few using the cubic corrections, it seems. */
		      if(spectra[idet+lowest] > 0) respectra[odet+0] += spectra[idet+lowest];
                      if(spectra[idet+lowest] > 0) odetsum += spectra[idet+lowest];
		      lowest++;
	      }
	      for(k=0; k < out_nchannels; k++) {
                  for(i=lowest;(i < nchannels - 1) && (corrmin_e[i] < out_max_e[k]);i++) {

		    if(spectra[idet+i] < 0) {
		      nwarns ++;
		      if (nwarns < 6) {
			fprintf(stderr,"WARNING: Negative count number in pixel %d, bin %d\n",j,i);
		      } 
		      if (nwarns == 5) {
			fprintf(stderr,"         (suppressing further warnings)\n");
		      } 
		    }
		      
		      /* skip bins where complete range is less than the minimum energy of kth bin */
	              while((corrmin_e[i+1] <= out_min_e[k]) && (i < nchannels-1)) i++; 
		      if((i < nchannels-1) && (corrmin_e[i] < out_max_e[k])) {
    
		          if((corrmin_e[i] >= out_min_e[k]) && (corrmin_e[i+1] <= out_max_e[k])) {
	                      if(spectra[idet+i] > 0) respectra[odet+k] += spectra[idet+i];  /* Add counts of all input bins completely within kth output bin */
                              if(spectra[idet+i] > 0) odetsum += spectra[idet+i];
		          }
		          else {
    
                              e_width = corrmin_e[i+1] - corrmin_e[i];  /* input bin width - kev*/ 
    
			      /* input bin is lower energy but overlaps output bin */
		              if((corrmin_e[i] < out_min_e[k]) && (corrmin_e[i+1] <= out_max_e[k]))
                                  e_fract = corrmin_e[i+1] - out_min_e[k]; /* width of overlap - kev*/
    
			      /* input bin includes all of output bin */
			      else if((corrmin_e[i] < out_min_e[k]) && (corrmin_e[i+1] > out_max_e[k]))
                                  e_fract = out_max_e[k] - out_min_e[k]; /* width of overlap - kev*/
    
			      /* input bin is higher energy but overlaps output bin */
			      else if((corrmin_e[i] >= out_min_e[k]) && (corrmin_e[i+1] > out_max_e[k]))
                                  e_fract = out_max_e[k] - corrmin_e[i]; /* width of overlap - kev*/
    
			      else {
				      fprintf(stderr, "Seems to be an unexpected bin overlap configuration\n");
				      e_fract = 0.0;
				      return 1;
			      }
    
                              /* move (number of events in old bin/e_width)*(e_fract) events into new histogram */
                              if(e_width!=0.0){
                                   if (whole)  { /* only move a whole number of counts */       
	                               if(spectra[idet+i] > 0) moven = (float)((int)(spectra[idet+i]* e_fract/e_width + 0.5));
				       else moven = 0;
			           /* THIS DOESN"T DO THE RIGHT THING--- work on it */
    /* Since we are not "moving some and leaving some behind" as originally conceived, some counts could get lost in some circumstances. */
			           }
                                   else              
		                     if(spectra[idet+i] > 0) moven = spectra[idet+i] * (e_fract/e_width); 
				     else moven = 0;
                                   respectra[odet+k] += moven;
                                   odetsum += moven;
                              } else return( 1 );
		          }
		      }   /* End if i still in range */
	          }   /* End for(i) */
    
	      }    /* End for(k)  */
    
    
	      /* last output bin always includes all of last input bin */
/* NO, IT DOESN'T!! * This has led to a bug of always putting integral bin counts in the end bins,
 * even if they do not contain all energies */
	      if(spectra[idet+nchannels-1] > 0) {
      /* Instead, include the integral bin counts only if the top energy is equal to the maximum */
		if(out_max_e[nchannels-1] == greatest_E) {
		  respectra[odet+out_nchannels-1] += spectra[idet+nchannels-1];
                  odetsum += spectra[idet+nchannels-1];
		}
	      }

	  }  /* End if fractional counts are allowed */

	  /* Do it the other way around for whole counts just for fun  */
	  else {

	      /* first output bin always includes all of first input bin */
/* NO, IT DOESN'T!! * This has led to a bug of always putting integral bin counts in the end bins,
 * even if they do not contain all energies */
	      if(spectra[idet+0] > 0) {
      /* Instead, include the integral bin counts only if the bottom energy is equal to the input
       * bottom energy */
		if(out_min_e[0] == least_E) {
		  respectra[odet+0] += spectra[idet+0];
	        }
	      }
              lowest = 1;           /* start at low edge of second bin in old histogram */
              for(i=lowest;i < nchannels - 1;i++) {
	          for(k=0,m=0;( k < out_nchannels) && (out_min_e[k] < corrmin_e[i+1]); k++) {
	          /* skip bins where complete range is less than the minimum energy of ith bin */
                      while(out_max_e[k] < corrmin_e[i]) k++;


	              if((k < out_nchannels) && (out_min_e[k] < corrmin_e[i+1])) {

	                  ks_i_adds_to[m] = k;

	                  if((corrmin_e[i] >= out_min_e[k]) && (corrmin_e[i+1] <= out_max_e[k])) {
                              frac_of_i_in_k[m] = 1.0;
	                  }
                          else {

                              e_width = corrmin_e[i+1] - corrmin_e[i];  /* input bin width - kev*/ 

    			      /* input bin is lower energy but overlaps output bin */
			      if((corrmin_e[i] < out_min_e[k]) && (corrmin_e[i+1] <= out_max_e[k]))
				      e_fract = corrmin_e[i+1] - out_min_e[k]; /* width of overlap - kev*/
        
			      /* input bin includes all of output bin */
			      else if((corrmin_e[i] < out_min_e[k]) && (corrmin_e[i+1] > out_max_e[k]))
				      e_fract = out_max_e[k] - out_min_e[k]; /* width of overlap - kev*/
        
			      /* input bin is higher energy but overlaps output bin */
			      else if((corrmin_e[i] >= out_min_e[k]) && (corrmin_e[i+1] > out_max_e[k]))
				      e_fract = out_max_e[k] - corrmin_e[i]; /* width of overlap - kev*/
        
			      else {
				      fprintf(stderr, "Seems to be an unexpected bin overlap configuration\n");
				      e_fract = 0.0;
				      return 1;
			      }
        
			      /* move (number of events in old bin/e_width)*(e_fract) events into new histogram */
			      if(e_width!=0.0){
				      frac_of_i_in_k[m] = e_fract/e_width; 
			      } else return( 1 );
			  }

			  m++;

		      }   /* End if k still in range */

		  }   /* End for(k) */

		  for(n=0,accounti=0;n<m;n++) {
 			  if(spectra[idet+i] > 0) moven = spectra[idet+i] * frac_of_i_in_k[n];
			  else moven = 0;
 			  imoven = (int) (moven);
 			  fracleft[n] = moven - imoven;
 			  respectra[odet+ks_i_adds_to[n]] += imoven;
 			  accounti += imoven;
		  }
		  if(spectra[idet+i] > 0) if(accounti != spectra[idet+i]) {
			  for(n=0;n<m;n++) {
    				  for(r=0,q=0;r<m;r++) if(fracleft[n] < fracleft[r]) q++;
    				  fracorder[n] = q;
			  }
		          
			  leftover = spectra[idet+i] - accounti;
			  while(leftover > 0) {
				  for(q=0;(q<m) && (leftover > 0);q++) {
					  for(n=0;(n<m) && (q != fracorder[n]);n++);
					  if(q == fracorder[n]) {
    						  respectra[odet+ks_i_adds_to[n]] ++;
    						  leftover --;
					  }
				  }
			  }
		  }
			      
	      }    /* End for(i)  */


	      /* last output bin always includes all of last input bin */
/* NO, IT DOESN'T!! * This has led to a bug of always putting integral bin counts in the end bins,
 * even if they do not contain all energies */
	      if(spectra[idet+nchannels-1] > 0) {
      /* Instead, include the integral bin counts only if the top energy is equal to the maximum */
		if(out_max_e[nchannels-1] == greatest_E) {
		  respectra[odet+out_nchannels-1] += spectra[idet+nchannels-1];
		}
	      }


	      for(k=0,m=0;k < out_nchannels; k++)
		  if(spectra[idet+nchannels-1] > 0) odetsum += respectra[odet+k];

	  }

          /* Check that no counts went missing */
	  if((odetsum < idetsum - epsilon) || (odetsum > idetsum + epsilon)) {
            nwarns2++;
            if(nwarns2 < 10 && out_max_e[nchannels-1] == greatest_E && out_min_e[0] == least_E)
              headas_chat(3,"Pixel %d Sum of events in input (%f) != to sum of events in output (%f) within epsilon (%f)\n",j, idetsum, odetsum, epsilon);
	    if(nwarns2 == 11) {
              headas_chat(2,"Sum of events in at least 10 detectors != to sum of events in output within epsilon (%f)\n", epsilon);
	      headas_chat(2,"Suppressing further event count warnings. Note that you expect different counts\n");
	      headas_chat(2,"if your output energy range does not include all the input energy range.\n");
	    }
              for(i=0;i<nchannels-1;i++) if (nwarns2 == 0)
		  headas_chat(5,"det %d input bin %d counts %f mine %f maxe %f\n", 
				  j, i, spectra[idet+i], corrmin_e[i], corrmin_e[i+1]);
	      headas_chat(5,"det %d input bin %d counts %f mine %f\n", 
			      j, i, spectra[idet+i], corrmin_e[nchannels-1]);
	      for(k=0; k < out_nchannels; k++) if (nwarns2 == 0)
		  headas_chat(5,"det %d output bin %d counts %f mine %f maxe %f\n", 
				  j, k, respectra[odet+k], out_min_e[k], out_max_e[k]);

	  }

      }  /* end else ok gains */


  } /* j */

  if(nnulls > 0) headas_chat(2,"%d detectors with null values in input DPH found.\n",nnulls);
  if(ngainerr > 0) headas_chat(2,"%d detectors with with events in them had bad gain/offset values, and were zeroed out.\n",ngainerr);
  if(ngainerr > 0) headas_chat(2,"(Gain/offset files typically have some (tens) bad values due to pulser errors.)\n");

 return(status);

}

/* ----------------------------------------------------------------- */
/*-------------------------------------------------------------*/

/* Routine to make all calls to PIL to fill the structure containing
 *    all user supplied paramters. */

int baterebin_getpar(struct parm_struct *parms, char *taskname, char *version)

{

   int status=0;
   fitsfile *infptr=0;
   struct caldbparms_struct caldb;


     memset(&caldb, 0, sizeof(caldb));
     parms->taskname = taskname;
     parms->taskver = version;
     parms->infile[0] = 0;
     parms->outfile[0] = 0;
     parms->calfile[0] = 0;
     parms->residfile[0] = 0;
     parms->pulserfile[0] = 0;
     parms->fltpulserfile[0] = 0;
     parms->detmask[0] = 0;
     parms->outmap[0] = 0;
     parms->lowecare = 10.0;
     parms->highecare = 194.9;
     parms->wholecount = 0;
     sprintf(parms->ebins,"FILEBINS");
     sprintf(parms->outdatatype,"FLOAT");

     /* Default extension numbers */     
     parms->pulserext = -1;    
     parms->fltpulserext = -1;
     parms->residext = -1;

     headas_chat(4,"Got to the parameter subroutine\n");

     if ((status = PILGetFname("infile", parms->infile)))
         fprintf(stderr, "Error reading the 'infile' parameter.\n");
 
     else { 
	     if ((status = PILGetFname("outfile", parms->outfile)))
         fprintf(stderr, "Error reading the 'outfile' filename parameter.\n");
 
     else { 
	     if ((status = PILGetFname("calfile", parms->calfile)))
         fprintf(stderr, "error reading 'calfile' filename parameter.\n");
     
     else { 
	     if ((status = PILGetString("residfile", parms->residfile)))
         fprintf(stderr,"Error reading the 'residfile' filename. \n");
 
     else { 
	     if ((status = PILGetString("pulserfile", parms->pulserfile)))
         fprintf(stderr,"Error reading the 'pulserfile' filename. \n");
     
     else { 
	     if ((status = PILGetString("fltpulserfile", parms->fltpulserfile)))
         fprintf(stderr,"Error reading the 'fltpulserfile' filename. \n");

     else { 
	     if ((status = PILGetString("ebins", parms->ebins)))
         fprintf(stderr,"Error reading the 'ebins' output ebounds filename. \n");
 
     else { 
	     if ((status = PILGetString("detmask", parms->detmask)))
         fprintf(stderr, "Error reading the 'detmask' parameter.\n");
 
     else { 
	     if ((status = PILGetString("outmap", parms->outmap)))
         fprintf(stderr, "Error reading the 'outmap' parameter.\n");
 
     else { 
	     if ((status = PILGetReal("highecare", &parms->highecare)))
         fprintf(stderr, "Error reading the 'highecare' parameter.\n");
 
     else { 
	     if ((status = PILGetReal("lowecare", &parms->lowecare)))
         fprintf(stderr, "Error reading the 'lowecare' parameter.\n");
 
     else { 
	     if ((status = PILGetBool("wholecount", &parms->wholecount)))
         fprintf(stderr, "Error reading the 'wholecount' parameter.\n");
 
     else { 
	     if ((status = PILGetString("rows", parms->rowstr)))
         fprintf(stderr, "Error reading the 'rows' parameter.\n");

     else { 
	     if ((status = PILGetString("outdatatype", parms->outdatatype)))
         fprintf(stderr, "Error reading the 'outdatatype' parameter. Should be SHORT, INT, FLOAT, or DOUBLE\n");
     }}}}}}}}}}}}}

     /* Default value of parms->detmask file name */
     if (strcasecmp(parms->detmask, "none") == 0) {
           parms->detmask[0] = 0;
     }

     if (strcasecmp(parms->outmap, "none") == 0) {
           parms->outmap[0] = 0;
     }

     if ( (strcasecmp(parms->residfile,"CALDB") == 0) ||
	  (strcasecmp(parms->pulserfile,"CALDB") == 0) ||
	  (strcasecmp(parms->fltpulserfile,"CALDB") == 0) ||
	  (strncasecmp(parms->ebins,"CALDB",5) == 0) ) {
       
       fits_open_data(&infptr, parms->infile, READONLY, &status);
       batkw_to_caldb_parms(infptr, &caldb, 1, &status);
       fits_close_file(infptr, &status);
       if (status) {
	 fprintf(stderr, "ERROR: could not determine CALDB parameters from %s\n",
		 parms->infile);
	 return status;
       }
     }


     if (strcasecmp(parms->residfile,"CALDB") == 0) {
       char *expr = "-";
       char *codenam = "DET_GAIN";
       char *pfile = parms->residfile;
       char online[80], *ponline = online;
       long int extno[1];
       int maxret = 1;
       int nret = 0, nfound = 0;

       bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
			&pfile, extno, &ponline, &nret, &nfound, &status);
       if ((status != 0) || (nret == 0) || (nfound == 0)) return status;
       parms->residext = extno[0];
     }

     /* Ground-derived pulser DAC to keV conversion */
     if (strcasecmp(parms->pulserfile,"CALDB") == 0) {
       char *expr = "SOURCE.EQ.\"GROUND\"";
       char *codenam = "PULSER_GAIN";
       char *pfile = parms->pulserfile;
       char online[80], *ponline = online;
       long int extno[1];
       int maxret = 1;
       int nret = 0, nfound = 0;
       
       bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
			&pfile, extno, &ponline, &nret, &nfound, &status);
       if ((status != 0) || (nret == 0) || (nfound == 0)) return status;
       parms->pulserext = extno[0];
     }
     
     /* Flight pulser DAC to keV conversion */
     if (strcasecmp(parms->fltpulserfile,"CALDB") == 0) {
       char *expr = "SOURCE.EQ.\"FLIGHT\"";
       char *codenam = "PULSER_GAIN";
       char *pfile = parms->fltpulserfile;
       char online[80], *ponline = online;
       long int extno[1];
       int maxret = 1;
       int nret = 0, nfound = 0;
       
       bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
			&pfile, extno, &ponline, &nret, &nfound, &status);
       if ((status != 0) || (nret == 0) || (nfound == 0)) return status;
       parms->fltpulserext = extno[0];
       headas_chat(5, "   (pfile=%s)\n", pfile);
     }

     /* Read CALDB stuff, namely the energy bins */
     if (strncasecmp(parms->ebins,"CALDB",5) == 0) {
       char expr[80];
       char *codenam = "EBOUNDS";
       char *pfile = parms->ebins;
       char online[80], *ponline = online;
       long int extno[1];
       int maxret = 1;
       int nret = 0, nfound = 0;
       int nebins;

       /* Attempt to read number of bins from CALDB expression, otherwise
	  default to 4 bins for light curves and 80 bins for spectra */
       status = sscanf(parms->ebins+5, ":%d", &nebins);
       if (status <= 0) {
	   nebins = 80;
       }
       status = 0;

       /* Create expression for number of energy bins */
       sprintf(expr, "MODE.eq.%d", nebins);
       
       /* Actually query the CALDB */
       bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
			&pfile, extno, &ponline, &nret, &nfound, &status);
       if ((status != 0) || (nret == 0) || (nfound == 0)) return status;
     }

     headas_chat(3,"The input file is called %s\n",parms->infile);
     headas_chat(3,"The output file is called %s\n",parms->outfile);
     headas_chat(3,"The inflight calfile is called %s\n",parms->calfile);
     if (strcasecmp(parms->ebins, "FILEBINS") != 0) {
	     headas_chat(3,"The output ebounds file is called %s\n", parms->ebins);
     } else {
         headas_chat(3,"The output ebounds will be the same as in the input file\n");
     }
     if(parms->wholecount != 0) headas_chat(3,"Only whole counts will be moved\n");
     else headas_chat(3,"Partial counts will be moved\n");
     headas_chat(3,"The residual file is called %s\n",parms->residfile);
     headas_chat(3,"The pulser DAC to energy conversion file is called %s\n", parms->pulserfile);
     headas_chat(3,"The desired rows are %s\n",parms->rowstr);
     if((strcasecmp(parms->rowstr, "-") == 0)) {
	     headas_chat(3,"All rows (time intervals) will be used.\n");
     }

     return(status);

}


/* ----------------------------------------------------------------- */
/* ----------------------------------------------------------------- */

int baterebin_printsummary (
   struct parm_struct *parms,
   char *taskname,
   char *version)

{
    headas_chat(1,"%s v%s completed\n",taskname,version);
    headas_chat(1,"Input DPH file is %s\n",parms->infile);
    if (*parms->outfile) headas_chat(1,"Output DPH file is %s\n",parms->outfile);
    if (*parms->detmask) headas_chat(1,"Input detector mask is %s\n",parms->detmask);
    if (*parms->outmap) {
	    headas_chat(1,"Output imperfect conversion map is %s (set chatter to 3+ for value description)\n",parms->outmap);
            headas_chat(3,"Values in map:\n");
			   headas_chat(3, "        1 bit is integral bin overlaps lowest energy of interest\n"); 
			   headas_chat(3, "        2 bit is integral bin overlaps highest energy of interest\n"); 
			   headas_chat(3, "        6 bit is too large a correction\n"); 
			   headas_chat(3, "        4 bit is gain values out of range\n"); 
			   headas_chat(3, "        5 bit is gain values not available\n"); 
			   headas_chat(3, "        6 bit is detector masked out by input mask\n"); 
    }
    headas_chat(1,"Total exposure time in output %ld\n", totalexp);
    headas_chat(1,"Number of time intervals in input %d\n", nrowsin);
    headas_chat(1,"Number of time intervals in output %d\n", nrowsout);

    return(0);
}


/* ----------------------------------------------------------------- */
/* ----------------------------------------------------------------- */
/*
 * cal_value_check - sanity check of calibration coefficients 
 *
 */
int cal_value_check(
   bat_ecal_data *caldata,
   struct det_qual_struct *detqual,
   float * highestlowcorremin,
   float * lowesthighcorremin,
   float loweminout,
   float higheminout,
   struct parm_struct *parms,
   int *nlow,
   int *nhigh,
   int *lowdet,
   int *highdet,
   struct image_struct *detmask,
   struct image_struct *outmap)
{

  int status=0;
  long int i;
  float highcorremin;
  float lowcorremin;
  float low_emin;
  float high_emin;
  int det_stat[N_GAIN_STATES];
  int pixsandrow=0, pixsandcol=0;
  int row=0, col=0;
  int maskok = 0;
  int gmeth;

  for(i=0;i<N_GAIN_STATES;i++) det_stat[i] = 0;

  high_emin = *lowesthighcorremin;
  low_emin = *highestlowcorremin;

  headas_chat(4, "High end of lowest energy bin in input is %f\n", low_emin);
  headas_chat(4, "Low end of highest energy bin in input is %f\n", high_emin);
  
  *lowesthighcorremin = 10000.0;
  *highestlowcorremin = -10000.0;

    /* Check all the gain/offset values to see if they fall in reasonable ranges */

#if (RESID_ESCALE == 1)
  fprintf(stderr, "WARNING: Using obsolete fpulseTokeV energy scale\n");
#endif

  gmeth = caldata->gain_meth;

  if ((gmeth != QUAD_METH) && (gmeth != CUBIC_METH) && (gmeth != FIXEDDAC_METH)) {
    status = -1;
    fprintf(stderr, 
	    "ERROR: The value of GAINMETH in residual file is\n"
	    "       not recognized.  You may need to update your software.\n");
    return(status);
  }

  for (i=0;i<NUM_PIXELS;i++) {
    
    outmap->data[i] = 0;
    col = i % 286; 
    row = (i - col) / 286;
    pixsandcol = col % 18;
    pixsandrow = row % 11;
    
    if(detmask != 0) {
      if(detmask->data[i] == parms->goodval) {
	maskok = 1;
      } else {
	maskok = 0;
      }
      if (parms->outmap[0] != 0) {
	if(maskok == 0) outmap->data[i] = 32;
      }
    }

    /* Now apply quality flags to calibration data */
    if((pixsandcol > 15) || (pixsandrow > 7)) {

      /* Falls in detector gaps */
      detqual->flag[i] = GAP;
      
    } else if (detmask && maskok == 0) {
      
      /* Masked as bad */
      detqual->flag[i] = MASKED_OUT;

    } else if ((caldata->gpulresid2[i] == 0) &&
	       (caldata->gpulresid1[i] == 0) &&
	       (caldata->gpulresid0[i] == 0)) {
      
      /* No "residual" coefficients */
      if (caldata->fpulseTokeV[i] == 0) {
	detqual->flag[i] = NO_GAIN_REC;
	headas_chat(5, "DET=(%3d,%3d) -- NO_GAIN_REC\n", row, col);
      } else {
	detqual->flag[i] = NO_QUAD_FIT;
	headas_chat(5, "DET=(%3d,%3d) -- NO_QUAD_FIT\n", row, col);
      }


    /* Global calibration value sanity checks */
    } else if (caldata->ftotgain[i] < 0.08 ||  /* [keV/ADU] total linear gain */
	       caldata->ftotgain[i] > 0.39 ||
	       caldata->ftotoffset[i] < 1500 || /* [ADU] total linear offset */
	       caldata->ftotoffset[i] > 3750 ||
	       caldata->fpulseTokeV[i] < 0.05 || /* [keV/DACU] flight DAC-keV linear gain */
	       caldata->fpulseTokeV[i] > 0.15 ||
	       caldata->fpulse0keV[i] < (-80) || /* [DACU] flight DAC-keV linear offset */
	       caldata->fpulse0keV[i] > (30) || 
	       caldata->gpulseTokeV[i] < 0.05 || /* [keV/DACU] flight DAC-keV linear gain */
	       caldata->gpulseTokeV[i] > 0.15 ||
	       caldata->gpulse0keV[i] < (-80) || /* [DACU] flight DAC-keV linear offset */
	       caldata->gpulse0keV[i] > (30)) {

      detqual->flag[i] = GAINOFF_OUTOFRNG;
      if(parms->outmap[0] != 0) outmap->data[i] = outmap->data[i] + 8;
      headas_chat(5, "DET=(%3d,%3d) -- RANGE -- ftotgain/offset=%f/%f fpulseTo/0keV=%f/%f gpulseTo/0keV=%f/%f\n",
		  row, col, caldata->ftotgain[i], caldata->ftotoffset[i], 
		  caldata->fpulseTokeV[i], caldata->fpulse0keV[i],
		  caldata->gpulseTokeV[i], caldata->gpulse0keV[i]);


    /* Values specific to the quadratic residuals */
    } else if (gmeth == QUAD_METH && 
	       (caldata->gpulresid0[i] < 1000.0 ||
		caldata->gpulresid0[i] > 9500.0 ||
		caldata->gpulresid1[i] < (-12.0) ||
		caldata->gpulresid1[i] > (2.0) ||
		caldata->gpulresid2[i] < 0.0001 ||
		caldata->gpulresid2[i] > 0.0018)) {
      
      detqual->flag[i] = GAINOFF_OUTOFRNG;
      if(parms->outmap[0] != 0) outmap->data[i] = outmap->data[i] + 8;

      headas_chat(5, "DET=(%3d,%3d) -- RESRANGE:QUAD -- gpulresid0/1/2=%f/%f/%f\n",
		  row, col, 
		  caldata->gpulresid0[i],
		  caldata->gpulresid1[i],
		  caldata->gpulresid2[i]);


    /* Values specific to the cubic residuals */
    } else if (gmeth == CUBIC_METH && 
	       (caldata->gpulresid0[i] <  1000.0 ||
		caldata->gpulresid0[i] > 13000.0 ||
		caldata->gpulresid1[i] < (-15.0) ||
		caldata->gpulresid1[i] > ( -0.2) ||
		caldata->gpulresid2[i] < (-0.002) ||
		caldata->gpulresid2[i] > ( 0.008) ||
		caldata->gpulresid3[i] < (-0.000002) ||
		caldata->gpulresid3[i] > ( 0.000002))) {
      
      detqual->flag[i] = GAINOFF_OUTOFRNG;
      if(parms->outmap[0] != 0) outmap->data[i] = outmap->data[i] + 8;

      headas_chat(5, "DET=(%3d,%3d) -- RESRANGE:CUBIC -- gpulresid0/1/2/3=%f/%f/%f/%f\n",
		  row, col, 
		  caldata->gpulresid0[i],
		  caldata->gpulresid1[i],
		  caldata->gpulresid2[i],
		  caldata->gpulresid3[i]);


    /* Values specific to the cubic FIXEDDAC residuals */
    } else if (gmeth == FIXEDDAC_METH && 
	       (caldata->gpulresid0[i] <  5000.0 ||
		caldata->gpulresid0[i] > 25000.0 ||
		caldata->gpulresid1[i] < (-30.0) ||
		caldata->gpulresid1[i] > ( -0.0) ||
		caldata->gpulresid2[i] < (-0.002) ||
		caldata->gpulresid2[i] > ( 0.010) ||
		caldata->gpulresid3[i] < (-0.000001) ||
		caldata->gpulresid3[i] > ( 0.0000005))) {
      
      detqual->flag[i] = GAINOFF_OUTOFRNG;
      if(parms->outmap[0] != 0) outmap->data[i] = outmap->data[i] + 8;

      headas_chat(5, "DET=(%3d,%3d) -- RESRANGE:FIXEDDAC -- gpulresid0/1/2/3=%f/%f/%f/%f\n",
		  row, col, 
		  caldata->gpulresid0[i],
		  caldata->gpulresid1[i],
		  caldata->gpulresid2[i],
		  caldata->gpulresid3[i]);
    } else {
      float ehighlow[2], xenergy[2];
      float xpha[2];
      int nchannels = 2;

      detqual->flag[i] = GOOD_GAINS;
      
      ehighlow[0] = high_emin;
      ehighlow[1] = low_emin;

      bat_flight_energy_to_pha(ehighlow, xpha, nchannels,
			       caldata->ftotgain[i], caldata->ftotoffset[i], 1);
      bat_pha_to_energy(xpha, xenergy, nchannels, 
			caldata->gain_meth, 
			caldata->ftotgain[i], caldata->ftotoffset[i], 
			caldata->fpulseTokeV[i], caldata->fpulse0keV[i], 
			caldata->gpulseTokeV[i], caldata->gpulse0keV[i], 
			caldata->DAClow, 
			caldata->gpulresid0[i], caldata->gpulresid1[i], 
			caldata->gpulresid2[i], caldata->gpulresid3[i], 
			caldata->gpul_nom_offset[i], caldata->gpul_nom_gain[i], 
			RESID_ESCALE,
			&status);

      /* highcorremin is the lowest value for the corrected location
       of the minimum of the highest energy bin.  This will be used to
       make sure the highest output energy bin includes all of the
       counts from the highest input energy bin. */


      /* real low edge of energy bin */
      highcorremin = xenergy[0];
      if(highcorremin < higheminout) {
	(*nhigh)++;
	if(parms->outmap[0] != 0) outmap->data[i] = outmap->data[i] + 2;
      }
      if(highcorremin < *lowesthighcorremin) {
	*highdet = i;
	*lowesthighcorremin = highcorremin;
      }

      /* lowcorremin is the highest value for the corrected location
      of the maximum of the lowest energy bin.  This will be used to
      make sure the lowest output energy bin includes all of the
      counts from the lowest input energy bin. */

      /* real high edge of energy bin */
      lowcorremin  = xenergy[1] + 1;
      if(lowcorremin > loweminout) {
	(*nlow)++;
	if(parms->outmap[0] != 0) outmap->data[i] = outmap->data[i] + 1;
      }
      if((lowcorremin - low_emin > BIGGEST_CORR) || 
	 (low_emin - lowcorremin > BIGGEST_CORR)){
	/* current detectors that stick out as having high (by quite a bit) lowcorremin */
	/* The gains and offsets don't look unusual though, so this
	   should be checked, if we care. */
	detqual->flag[i] = TOO_BIG_CORR;
	if(parms->outmap[0] != 0) outmap->data[i] = outmap->data[i] + 4;
      } else if(lowcorremin > *highestlowcorremin) {
	*lowdet = i;
	*highestlowcorremin = lowcorremin;
      }

    } /* End giant if() */

    /* Accumulate statistics on each kind of flagged pixel */
    det_stat[detqual->flag[i]]++;
  }

      
    high_emin = *lowesthighcorremin;
    low_emin = *highestlowcorremin;
      
    headas_chat(4, "Maximum corrected high end of lowest energy bin in input is %f\n", low_emin);
    headas_chat(4, "Minimum corrected low end of highest energy bin in input is %f\n", high_emin);
    headas_chat(2, "\n");
    headas_chat(2, "%d detectors with acceptable gain/offsets\n", det_stat[GOOD_GAINS]);
    headas_chat(2, "%d detectors masked out with detmask file\n", det_stat[MASKED_OUT]);
    headas_chat(2, "%d detectors with too large a correction at low energy end\n", det_stat[TOO_BIG_CORR]);
    headas_chat(2, "%d detectors with anomalously large or small gain/offsets \n", det_stat[GAINOFF_OUTOFRNG]);
    headas_chat(2, "%d detectors with no gains recorded \n", det_stat[NO_GAIN_REC]);
    headas_chat(2, "%d detectors with no quadratic fit \n", det_stat[NO_QUAD_FIT]);
    headas_chat(2, "%d gap pixels\n", det_stat[GAP]);
    headas_chat(2, "\n");

    return(status);


}
