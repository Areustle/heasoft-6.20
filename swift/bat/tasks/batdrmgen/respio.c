/* ----------respio.c FITS input & output functions----------------- */
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
#include "headas_gti.h"
#include "bat_gswdev.h"
#include "batdrmgen.h"

/* 
 * Routines for performing output to FITS response matrix files in
 * more or less OGIP standard format
 *
 * prepfile() - utility routine to create FITS table
 * writecalkeys() - writes calibration file keywords to a header
 * writeresp() - writes response extension only
 * writeebounds() - writes EBOUNDS extension only
 * writeallresp() - writes both response and EBOUNDS extensions
 * readphainfo() - reads response-related info from PHA file 
 * read_col_or_keyword - read column value, but retry as a keyword
 *
 * A. Parsons / C. Markwardt / D. Hullinger
 *
 */

/* made alterations to accomodate the changes made to structure variables 
 * resp, parms so that the user can input the parameters needed to define 
 * her own incident photon energy scale AMP 7/8/03*/



/* ----------------------------------------------------------------- */
/* Prepare the file for output (performs generic-type copying and setup) */
int prepfile(fitsfile *infile, fitsfile *outfile, 
	     int ncols, int nrows, 
	     char *colnames[], char *colforms[], char *colunits[], 
	     char *comments[], 
	     char *extname, int append)
{
  int status = 0;
  int i;
  int nkeys = 0;
  char card[FLEN_CARD];
  char ttypen[FLEN_CARD];

  headas_chat(4, "  ...creating output table using fits_create_tbl...\n");
  fits_create_tbl(outfile, BINARY_TBL, nrows, ncols, 
		  colnames, colforms, colunits, extname, &status);
  if (status) {
    fprintf(stderr, "ERROR: Could not create output table\n");
    return status;
  }    
  for (i=0; i<ncols; i++) {
    if (comments && comments[i] && comments[i][0]) {
      fits_make_keyn("TTYPE", i+1, ttypen, &status);
      fits_modify_comment(outfile, ttypen, comments[i], &status);
    }
  }
  
  /* Copy all the user keywords (not the structural keywords) */
  headas_chat(4, "Using fits_get_hdrspace\n");
  fits_get_hdrspace(infile, &nkeys, NULL, &status);

  headas_chat(4, "Copying keywords with fits_read_record and  fits_write_record (also calls fits_get_keyclass) \n");
  for (i = 1; i <= nkeys; i++) {
    fits_read_record(infile, i, card, &status);
    /* Copy HISTORY keywords and any user keywords, but not COMMENTs */
    if ((fits_get_keyclass(card) >= TYP_REFSYS_KEY) &&
	(strncmp(card, "COMMENT ", 7) != 0))
      fits_write_record(outfile, card, &status);
  }

  return status;
}

/*
 * writecalkeys - Write calibration file keywords
 *
 * This routine writes the names of the calibration files used to 
 * keywords in the headers of the EBOUNDS and SPECRESP MATRIX extensions
 *
 * outfile - FITS file pointer for RSP file (opened for writing)
 * parms - general tool parameters
 *   ->calfile  - (char *) name of calibration parameter file
 *   ->distfile  - (char *) name of depth distribution file
 *   ->efile - (char *) name of incident energy bin file
 *
 * RETURNS: 0 for success, error code for failure
 */

int writecalkeys(fitsfile *outfile, struct parm_struct *parms)
{
  
  char *p;
  int status=0;

  p = rindex(parms->calfile,'/');
  if (p == 0) p = parms->calfile; else p++;
  fits_update_key(outfile, TSTRING, "PARMFILE", p,
      "BAT calibration parameter file", &status);

  p = rindex(parms->depthfile,'/');
  if (p == 0) p = parms->depthfile; else p++;
  fits_update_key(outfile, TSTRING, "DPTHFILE", p,
      "BAT interaction depth profiles", &status);

  p = rindex(parms->efile,'/');
  if (p == 0) p = parms->efile; else p++;
  fits_update_key(outfile, TSTRING, "ENRGFILE", p,
      "BAT incident energy bin file", &status);

  if (status) {
    fits_report_error(stderr,status);
    return status;
  }
  else {
    return 0;
  }
}

/* 
 * writeresp - Write OGIP response extension only
 *
 * This routine writes an OGIP-standard response matrix extension.
 * The parms and resp structures must be appropriately populated.
 *
 * infile - FITS file pointer for PHA file (opened to table extension)
 * outfile - FITS file pointer for RSP file (opened for writing)
 * parms - general tool parameters
 *   ->infile   - (char *) name of input PHA file
 *   ->outfile  - (char *) name of output RSP file
 *   ->calfile  - (char *) name of calibration parameter file
 *   ->distfile  - (char *) name of depth distribution file

 *   ->taskname - (char *) name of task
 *   ->taskver  - (char *) version string for task
 *   ->coltype  - (char *) type of pulse height column (probably "PI")
 * resp - response matrix data and descriptors
 *        (set batdrmgen.h for definitions)
 * 
 * RETURNS: 0 for success, error code for failure
 *  
 */

int writeresp(fitsfile *infile, fitsfile *outfile, 
	      struct parm_struct *parms,
	      struct resp_struct *resp)
{
  int status = 0;
  int ncols;
  char *colnames[] = { "ENERG_LO", "ENERG_HI", "N_GRP", "F_CHAN", "N_CHAN", 
		       "MATRIX" };
  char *colforms[] = { "E",        "E",        "I",     "I",      "I",
		       "E"       };
  char *colunits[] = { "keV",      "keV",      "",      "",       "",
		       ""        };
  char *comments[] = {"Incident energy lower bin edge",
		      "Incident energy upper bin edge",
		      "Number of response groups",
		      "Group first response channel",
		      "Group number of channels",
		      "Response matrix elements"};
  char *extname    = "SPECRESP MATRIX";
  char tform_matrix[10] = "";
  char creator[FLEN_CARD];
  int i;
  int *work;
  int workint = 0;

  /* Response matrix:
     ESTART - lower bound incident photon bin
     ESTOP - upper bound incident photon bin
     N_GRP    - 1 for BAT
     F_CHAN   - 0 for BAT
     N_CHAN   - nphabins for BAT
     MATRIX   - response information

     keywords: TELESCOP, INSTRUME, FILTER, CHANTYPE, DETCHANS,
               HDUCLASS = 'OGIP', HDUCLAS1 = 'RESPONSE', 
	       HDUCLAS2 = 'RSP_MATRIX', HDUVERS = '1.3.0', 
	       TLMIN{F_CHAN}, NUMGRP = {nebins}, NUMELT = {nebins*nphabins}, 
	       PHAFILE = {infile}, HDUCLAS3 = 'FULL', RMFVERSN = '1992a',
	       HDUVERS1 = '1.1.0', HDUVERS2 = '1.2.0'
  */

  headas_chat(5, "...Inside writeresp...\n");
  ncols = sizeof(colnames)/sizeof(colnames[0]);

  /* Fill in actual number of PHA bins into TFORM descriptor */
  if (resp->nphabins > 1) {
    sprintf(tform_matrix, "%dE", resp->nphabins);
    colforms[5] = tform_matrix;
  }
    
  headas_chat(4,"..Calling prepfile.. \n");
  status = prepfile(infile, outfile, ncols, resp->nebins,  
		    colnames, colforms, colunits, comments, extname, 0);

  sprintf(creator, "%s %s", parms->taskname, parms->taskver);
  headas_chat(4,"..Calling fits_update_key.. \n");
  fits_update_key(outfile, TSTRING, "CREATOR", creator,
		 "Program that created this FITS file", &status);
  if (status != 0) {
    fprintf(stderr, "ERROR: The output header could not be created\n");
    return status;
  }

  /* Write the calibration file names as keywords */

  headas_chat(4,"..Calling writecalkeys.. \n");
  status=writecalkeys(outfile,parms);
  if (status != 0) {
    fprintf(stderr, "ERROR: writecalkeys failed\n");
    return status;
  }

  /* Write OGIP-standard version keywords */

  headas_chat(4,"Using fits_update_key to write required keywords in rsp file.. \n");
  fits_update_key(outfile, TSTRING, "HDUCLASS", "OGIP", 
		  "Conforms to OGIP/GSFC standards", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS1", "RESPONSE", 
		  "Dataset relates to spectral response", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS2", "RSP_MATRIX", 
		  "Dataset is a spectral response matrix", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS3", "FULL", 
		  "Convolved with det. effects and mask", &status);
  fits_update_key(outfile, TSTRING, "HDUVERS", "1.3.0",
		  "Format (OGIP memo CAL/GEN/92-002a)", &status);
  fits_update_key(outfile, TSTRING, "HDUVERS1", "1.1.0",
		  "Format (OGIP memo CAL/GEN/92-002a)", &status);
  fits_update_key(outfile, TSTRING, "HDUVERS1", "1.2.0",
		  "Format (OGIP memo CAL/GEN/92-002a)", &status);
  fits_update_key(outfile, TSTRING, "RMFVERSN", "1992a",
		  "Format (OGIP memo CAL/GEN/92-002a)", &status);

  /* Write response matrix dimension information */
  headas_chat(4,"Using fits_update_key to write response matrix dimension info.. \n");

  fits_update_key(outfile, TSTRING, "CHANTYPE", parms->coltype,
		  "Pulse height channel type", &status);
  fits_update_key(outfile, TINT, "DETCHANS", &(resp->nphabins),
		  "Total number of PHA channels available", &status);
  workint = 0;
  fits_update_key(outfile, TINT, "TLMIN4", &workint,
		  "Minimum legal value", &status);
  workint = resp->nphabins - 1;
  fits_update_key(outfile, TINT, "TLMAX4", &workint,
		  "Maximum legal value", &status);
  fits_update_key(outfile, TINT, "NUMGRP", &(resp->nebins),
		  "Number of channel subsets", &status);
  workint = resp->nebins*resp->nphabins;
  fits_update_key(outfile, TINT, "NUMELT", &workint,
		  "Total number of stored response matrix elements", &status);
  
  fits_update_key(outfile, TSTRING, "PHAFILE", parms->infile,
		  "Corresponding PHA file", &status);

  if (status) {
    fprintf(stderr, "ERROR: Could write keywords to output table\n");
    return status;
  }    

  /* Remove RESPFILE keyword if it exists */
  {
    int mystatus = 0;
    fits_write_errmark();
    fits_delete_key(outfile, "RESPFILE", &mystatus);
    fits_clear_errmark();
  }

  /* Allocate some temporary work space */
  work = (int *) malloc(sizeof(int) * resp->nebins);
  if (work == 0) {
    fprintf(stderr, "ERROR: Could not allocate working space memory\n");
    return MEMORY_ALLOCATION;
  }

  /* Write data to table */
  headas_chat(4,"Using fits_write_col to write the data to the table.. \n");


  fits_write_col(outfile, TDOUBLE, 1, 1, 1, resp->nebins,
		 resp->energ_lo, &status);   /* E_LOW */

  fits_write_col(outfile, TDOUBLE, 2, 1, 1, resp->nebins,
		 resp->energ_hi, &status);   /* E_HIGH */

  /* N_GRP is always 1, for a single channel group */
  for(i=0; i<resp->nebins; i++) work[i] = 1;
  fits_write_col(outfile, TINT, 3, 1, 1, resp->nebins,
		 work, &status);             /* N_GRP */

  /* F_CHAN is always 0, for the first PHA channel */
  for(i=0; i<resp->nebins; i++) work[i] = 0;
  fits_write_col(outfile, TINT, 4, 1, 1, resp->nebins,
		 work, &status);             /* F_CHAN */

  /* N_CHAN is always nphabins */
  for(i=0; i<resp->nebins; i++) work[i] = resp->nphabins;
  fits_write_col(outfile, TINT, 5, 1, 1, resp->nebins,
		 work, &status);             /* N_CHAN */

  /* The matrix is already stored in the desired format in memory, so
     it is simple to stuff it out to the FITS file */
  fits_write_col(outfile, TDOUBLE, 6, 1, 1, resp->nebins*resp->nphabins,
		 resp->matrix, &status);

  /* Done.  De-allocate the work space */
  free(work);

  status = HDpar_stamp(outfile, 0, &status);
  fits_set_hdustruc(outfile, &status);

  if (status == 0)
    headas_chat(1, "---\nMATRIX written to %s\n", parms->outfile); 
  else
    fprintf(stderr, "ERROR: could not write data to %s\n", parms->outfile);

  return status;
}

/* 
 * writeebounds - Write OGIP EBOUNDS extension only
 *
 * This routine writes an OGIP-standard EBOUNDS extension.  The
 * response matrix itself should already have been written by
 * writeresp().  The parms and resp structures must be appropriately
 * populated.
 *
 * infile - FITS file pointer for PHA file (opened to table extension)
 * outfile - FITS file pointer for RSP file (opened for writing)
 * parms - general tool parameters
 *   ->infile   - (char *) name of input PHA file
 *   ->outfile  - (char *) name of output RSP file
 *   ->taskname - (char *) name of task
 *   ->taskver  - (char *) version string for task
 *   ->coltype  - (char *) type of pulse height column (probably "PI")
 * resp - response matrix data and descriptors
 *        (set batdrmgen.h for definitions)
 * 
 * RETURNS: 0 for success, error code for failure
 *   
 */
int writeebounds(fitsfile *infile, fitsfile *outfile, 
		 struct parm_struct *parms, 
		 struct resp_struct *resp)
{
  int status = 0;
  int i;
  int ncols;
  char *colnames[] = { "CHANNEL",  "E_MIN",    "E_MAX"};
  char *colforms[] = { "I",        "E",        "E"    };
  char *colunits[] = { "",         "keV",      "keV"  };
  char *comments[] = { "Spectrum channel number",
		       "Channel lower energy bin edge",
		       "Channel upper energy bin edge" };
  char *extname    = "EBOUNDS";
  char creator[FLEN_CARD];
  int *work;

  /* 
     EBOUNDS:
     CHANNEL
     E_MIN
     E_MAX

     keywords: EXTNAME=EBOUNDS, CHANTYPE=PHA/PI, DETCHANS=NE, 
               HDUCLASS=OGIP, HDUCLAS1=RESPONSE, HDUCLAS2=EBOUNDS,
	       HDUVERS=1.2.0, PHAFILE=filename, RMFVERSN=1992a (obsolete)
               HDUVERS1=1.0.0, HDUVERS2=1.1.0
  */

  headas_chat(5, "...Inside writeebounds...\n");
  ncols = sizeof(colnames)/sizeof(colnames[0]);
  /* NOTE: append, not create */
  status = prepfile(infile, outfile, ncols, resp->nphabins,
		    colnames, colforms, colunits, comments, extname, 1);
  sprintf(creator, "%s %s", parms->taskname, parms->taskver);
  fits_update_key(outfile, TSTRING, "CREATOR", creator,
		  "Program that created this FITS file", &status);
  
  if (status != 0) {
    fprintf(stderr, "ERROR: The output file could not be appended to\n");
    return status;
  }

  /* Write the calibration file names as keywords */

  headas_chat(4,"..Calling writecalkeys.. \n");
  status=writecalkeys(outfile,parms);
  if (status != 0) {
    fprintf(stderr, "ERROR: writecalkeys failed\n");
    return status;
  }
 
  headas_chat(4,
      "Using fits_update_key to write more required keywords in rsp file..\n");
  fits_update_key(outfile, TSTRING, "HDUCLASS", "OGIP", 
		  "Conforms to OGIP/GSFC standards", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS1", "RESPONSE", 
		  "Contains spectrum", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS2", "EBOUNDS", 
		  "Spectrum is background subtracted", &status);
  fits_update_key(outfile, TSTRING, "RMFVERSN", "1992a",
		  "Version of EBOUNDS format (OBSOLETE)", &status);
  fits_update_key(outfile, TSTRING, "HDUVERS", "1.2.0",
		  "Version of EBOUNDS header", &status);
  fits_update_key(outfile, TSTRING, "HDUVERS1", "1.0.0",
		  "Version of EBOUNDS header", &status);
  fits_update_key(outfile, TSTRING, "HDUVERS2", "1.1.0",
		  "Version of EBOUNDS header", &status);

  fits_update_key(outfile, TINT, "DETCHANS", &(resp->nphabins),
		  "Total number of detector channels available", &status);
  fits_update_key(outfile, TSTRING, "CHANTYPE", parms->coltype,
		  "Pulse height channel type", &status);
  fits_update_key(outfile, TSTRING, "PHAFILE", parms->infile,
		  "Spectrum that this EBOUNDS extension applies to", &status);

  if (status) {
    fprintf(stderr, "ERROR: Could not write keywords to output table\n");
    return status;
  }    

  /* Allocate some temporary work space */
  work = (int *) malloc(sizeof(int) * resp->nphabins);
  if (work == 0) {
    fprintf(stderr, "ERROR: Could not allocate working space memory\n");
    return MEMORY_ALLOCATION;
  }

  for (i=0; i<resp->nphabins; i++) work[i] = i;
  fits_write_col(outfile, TINT,    1, 1, 1, resp->nphabins, work, &status);
  fits_write_col(outfile, TDOUBLE, 2, 1, 1, resp->nphabins,resp->emin,&status);
  fits_write_col(outfile, TDOUBLE, 3, 1, 1, resp->nphabins,resp->emax,&status);
  free(work);

  status = HDpar_stamp(outfile, 0, &status);
  fits_set_hdustruc(outfile, &status);

  if (status == 0)
    headas_chat(1, "EBOUNDS written to %s\n", parms->outfile); 
  else
    fprintf(stderr, "ERROR: could not write data to %s\n", parms->outfile);

  return status;
}

/* 
 * writeallresp - Write OGIP-compliant response and EBOUNDS combined
 *
 * This routine writes an OGIP-standard response file, including the
 * matrix and EBOUNDS extensions.  This routine creates the response
 * file from scratch.  It also must open the input PHA file
 * (read-only) in order to copy the mission-level keywords into the
 * RSP file.  The parms and resp structures must be appropriately
 * populated.
 *
 * parms - general tool parameters
 *   ->infile   - (char *) name of input PHA file
 *   ->outfile  - (char *) name of output RSP file
 *   ->taskname - (char *) name of task
 *   ->taskver  - (char *) version string for task
 *   ->coltype  - (char *) type of pulse height column (probably "PI")
 * resp - response matrix data and descriptors
 *        (set batdrmgen.h for definitions)
 * 
 * RETURNS: 0 for success, error code for failure
 *    
 */
int writeallresp(struct parm_struct *parms,
		 struct resp_struct *resp)
{
  int status = 0;
  fitsfile *infile;
  fitsfile *outfile;

  /* Create file and copy primary array of input FITS file */

  headas_chat(4,"  ...writing output file...\n");

  fits_open_table(&infile, parms->infile, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s for reading\n",
	    parms->infile);
    goto CLEANUP;
  }

  headas_clobberfile(parms->outfile);
  fits_create_file(&outfile, parms->outfile, &status);
  if (status) {
    fprintf(stderr, "ERROR: Could not open output file %s for writing\n",
	    parms->outfile);
    goto CLEANUP;
  }

  headas_chat(4,"  ...writing response extension...\n");
  status = writeresp(infile, outfile, parms, resp);

  if (status != 0) {
    fprintf(stderr, "ERROR: Could not write response to output\n");
    goto CLEANUP;
  }
  
  /* Write EBOUNDS extension */
  headas_chat(4,"  ...writing ebounds extension...\n");
  status = writeebounds(infile, outfile, parms, resp);

  fits_close_file(outfile, &status);
  outfile = 0;


 CLEANUP:
  /* Close input file no matter what */
  {
    int mystatus = 0;
    if (infile) fits_close_file(infile, &mystatus);
    infile = 0;
  }

  if (status != 0) {
    fprintf(stderr, "ERROR: Could not write output file\n");
    if (outfile != 0) {
      int mystatus = 0;
      fits_close_file(outfile, &mystatus);
      outfile = 0;
    }
  }

  return status;
}


/* 
 * readphainfo - read info from PHA file, relevant to response matrix
 *
 * This routine reads data from parms->infile, which should be a
 * standard OGIP spectral file from the BAT instrument.  It reads:
 *    BAT_[XYZ]OBJ keywords - position of object in BAT coordinates
 *    TIMEZERO  - Offset to the time column
 *    TIME      - Start time of observation (MET seconds)
 *    TIME_STOP - (not used) Stop time of observation (MET seconds)
 *    MJDREF    - (not used) Reference time (MJD) for MET = 0
 *    FLUXMETH  - method used to normalize mask-weighting factors
 *    GAINAPP   - whether gain correction ahs been applied
 *    GAINMETH  - method used to make gain correction 
 *    NFAPP     - near-fielding correction applied
 *    MSKWTSQF  - Half-variance of mask weight map
 *  And from EBOUNDS extension, reads E_MIN and E_MAX columns
 *
 * The values are populated into the resp structure.  Storage is
 * allocated for E_MIN and E_MAX automatically.  The user is
 * responsible for freeing these arrays when done.
 *
 * Values modified by this function are listed below.
 *
 * parms - task parameter info
 *         ->infile - (char *) name of input PHA file
 *         ->coltype - (char *) PI or PHA, based on binning of file (modified)
 * resp - response matrix information
 *        ->srcpos[] - (double) BAT X/Y/Z position (modified)
 *        ->mjdref - (double) MJD epoch of MET=0 (modified)
 *        ->time - (double) start time of observation
 *        ->time_stop - (double) stop time of observation
 *        ->timezero - (double) offset time
 *        ->chantype - (char *) type of channel ("PI")
 *        ->gainapp - (int) whether gain correction was applied (1=yes; 0=no)
 *        ->gainmeth - (char *) method of gain correction
 *
 *        ->emin/emax - (double *) lower/upper edges of PHA bins
 *                      (allocated by this routine)
 *        ->fluxmeth - (int) flux extraction method (1=raw; 2=weighted...)
 *
 * RETURNS: 0 for success, otherwise returns errorcode upon failure
 *

 */
int readphainfo(struct parm_struct *parms,
		struct resp_struct *resp)
{
  int status = 0;
  int colnum;
  int emincol, emaxcol, i, j;
  double def_mskwtsqf = 1.0; /* default mskwtsqf */
/*double def_pcodefr = 1.0;*/
/*double def_ngoodpix = 32768.0;*/
  fitsfile *infile;
  long int nbins;
  char fluxmeth[FLEN_CARD];

  headas_chat(4,"Opening the PHA file with readphainfo\n");
  fits_open_file(&infile, parms->infile, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s for reading\n", 
	    parms->infile);
    return status;
  }
  headas_chat(4,"Moving to SPECTRUM extension\n");
  fits_movnam_hdu(infile,BINARY_TBL,"SPECTRUM",0,&status);
  if (status) {
    int mystatus = 0;
    fprintf(stderr, "ERROR: could not move to SPECTRUM extension\n");
    fits_close_file(infile, &mystatus);
    return status;
  }

  batkw_to_caldb_parms(infile, &(resp->caldb), 1, &status);
  if (status) {
    int mystatus = 0;
    fprintf(stderr, "ERROR: could not read CALDB-type keywords\n");
    fits_close_file(infile, &mystatus);
    return status;
  }
  if (strcmp(resp->caldb.instrume,"BAT") != 0) {
    int mystatus = 0;
    fprintf(stderr, "ERROR: instrument was not set to BAT\n");
    fits_close_file(infile, &mystatus);
    return 1;
  }

  /* Read in the source position keywords */

  headas_chat(5,
      "Using read_col_or_keyword to read the source position keywords\n");

  read_col_or_keyword(infile, TDOUBLE, "BAT_XOBJ", parms->row, 0, 
      &(resp->srcpos[0]), 0, &status);
  read_col_or_keyword(infile, TDOUBLE, "BAT_YOBJ", parms->row, 0, 
      &(resp->srcpos[1]), 0, &status);
  read_col_or_keyword(infile, TDOUBLE, "BAT_ZOBJ", parms->row, 0, 
      &(resp->srcpos[2]), 0, &status);

  if (status) {
    int mystatus = 0;
    fprintf(stderr, "ERROR: could not read source position keywords\n");
    fits_close_file(infile, &mystatus);
    return status;
  }

  /* Read in required time-related keywords */

  headas_chat(5,"Reading the time-related keywords \n");

  fits_write_errmark();
  fits_get_colnum(infile, CASEINSEN, "TIME", &colnum, &status);
  fits_read_col(infile, TDOUBLE, colnum, parms->row, 1, 1, 0, 
      &(resp->time), 0, &status);
  fits_clear_errmark();
  if (status) {
    status=0;
    fits_read_key(infile, TDOUBLE, "TSTART", &(resp->time), 0, &status);
  }

  if (status) {
    int mystatus = 0;
    fprintf(stderr, "ERROR: could not read TIME column or TSTART keyword\n");
    fits_close_file(infile, &mystatus);
    return status;
  }

  fits_write_errmark();
  fits_get_colnum(infile, CASEINSEN, "TIME_STOP", &colnum, &status);
  fits_read_col(infile, TDOUBLE, colnum, parms->row, 1, 1, 0, 
      &(resp->time_stop), 0, &status);
  fits_clear_errmark();
  if (status) {
    status=0;
    fits_read_key(infile, TDOUBLE, "TSTOP", &(resp->time_stop), 0, &status);
  }
    
  if (status) {
    int mystatus = 0;
    fprintf(stderr, 
	"ERROR: could not read TIME_STOP column or TSTOP keyword\n");
    fits_close_file(infile, &mystatus);
    return status;
  }

  /* Read in optional time-related keywords */

  resp->mjdref = HDget_frac_time(infile, "MJDREF", 0, 0, &status);
  if (status) {
    headas_chat(3,"---\n");
    headas_chat(3,"WARNING: MJDREF keyword not found\n");
    headas_chat(3,"  Assuming it is zero\n");
    resp->mjdref=0;
    status=0;
  }

  read_col_or_keyword(infile, TDOUBLE, "TIMEZERO", parms->row, 0, 
      &(resp->timezero), 0, &status);
  if (status) {
    headas_chat(3,"---\n");
    headas_chat(3,"WARNING: TIMEZERO keyword not found\n");
    headas_chat(3,"  Assuming it is zero\n");
    resp->timezero=0;
    status=0;
  }

  /* Read in the CHANTYPE keyword, warn the user if it isn't PI,
   * and change it to PI (for when it is written to the RSP file) */

  fits_read_key(infile, TSTRING, "CHANTYPE", &(parms->coltype),  0, &status);

  if (strcmp(parms->coltype,"PI")) {
    headas_chat(3,"---\n");
    headas_chat(1,"WARNING: CHANTYPE is not 'PI'\n");
    headas_chat(1,"  BATDRMGEN will produce inaccurate results\n");
  }

  strcpy(parms->coltype, "PI");

  /* Read the FLUXMETH keyword (default is 2: "weighted") */

  resp->fluxmeth=0;
  fits_read_key(infile, TSTRING, "FLUXMETH", fluxmeth,  0, &status);
  if (status) {
    resp->fluxmeth = 2;  /* Default 2=weighted; for backward compat */
  } else {
    if (strcasecmp(fluxmeth,"RAW")==0) {
      resp->fluxmeth = 1;
    } else if (strcasecmp(fluxmeth,"WEIGHTED")==0) {
      resp->fluxmeth = 2;
    } else if (strcasecmp(fluxmeth,"FITTED")==0) {
      resp->fluxmeth = 3;
    }
  }

  if (resp->fluxmeth == 0) {
    int mystatus = 0;
    fprintf(stderr, "ERROR: unrecognized FLUXMETH keyword %s\n",fluxmeth);
    fits_close_file(infile, &mystatus);
    return -1;
  }
  if (resp->fluxmeth != 2 && resp->fluxmeth != 3) {
    int mystatus = 0;
    fprintf(stderr, 
	"ERROR: batdrmgen only works with FLUXMETH='WEIGHTED' files\n");
    fits_close_file(infile, &mystatus);
    return -1;
  }

  /* Read keywords which may not be in the file -- apply a sane default */

  /* GAINAPP (Default is 'T': gain correction is applied) */

  fits_read_key(infile, TLOGICAL, "GAINAPP", &(resp->gainapp),  0, &status);
  if (status) {
    status = 0;
    resp->gainapp = 1;
    headas_chat(2, "NOTE: GAINAPP keyword not found, assuming TRUE\n");
  }

  /* Warn the user if GAINAPP = 'F' */
  /* This is the only use of of the gainapp variable */

  if (resp->gainapp == 0) {
    headas_chat(3,"---\n");
    headas_chat(1, 
      "WARNING: GAINAPP = F.  Gain-correction should be applied to the data\n");
    headas_chat(1, 
      "         or BATDRMGEN will produce innacurate results\n");
  }

  /* GAINMETH (Default is 'CUBIC': cubic gain correction was applied) */

  fits_read_key(infile, TSTRING, "GAINMETH", &(resp->gainmeth), 0, &status);
  if (status) {
    status = 0;
    resp->gainmeth[0] = 0;
    strcpy(resp->gainmeth, "FULLCUBIC");
    headas_chat(2, "NOTE: GAINMETH keyword not found, assuming FULLCUBIC\n");
  }

  /* Warn the user if GAINMETH = 'LINEAR' */
  /* This is the only use of of the gainmeth variable */

  if (! strcmp(resp->gainmeth,"LINEAR")) {
    headas_chat(3,"---\n");
    headas_chat(1,"WARNING: GAINMETH = 'LINEAR'\n");
    headas_chat(1,"         BATDRMGEN may produce inaccurate results\n");
  }

  /* THESE KEYWORDS ARE NO LONGER USED BY BATDRMGEN: 
   *    PCODEAPP
   *    PCODEFR
   *    FFAPP
   *    NGPIXAPP
   *    NGOODPIX
   */
  
  /* ========================== COMMENTED OUT ========================= */
  
  /* Default: PCODEAPP = 'T' (correction for partial coding was not applied) */
  /* This variable is not actually used anywhere  */
/*fits_read_key(infile, TLOGICAL, "PCODEAPP",  &(resp->pcodeapp),  0, &status);
  if (status) {
    status = 0;
    resp->pcodeapp = 1;*/
    /* headas_chat(2, "NOTE: PCODEAPP keyword not found, assuming FALSE\n");*/
/*}*/

  /* Default: PCODEFR = def_pcodefr */
  /* This variable is not actually used anywhere  */
/*fits_read_key(infile, TDOUBLE, "PCODEFR",  &(resp->pcodefr),  0, &status);
  if (status) {
    status = 0;
    resp->pcodefr= def_pcodefr;*/
    /* headas_chat(2, 
     * "NOTE: PCODEFR keyword not found, using default value = %f\n",
     * def_pcodefr);
     * if (resp->pcodeapp)
     *  headas_chat(1, 
     * "WARNING: PCODEAPP = T but PCODEFR keyword not found\n");
     */
/*}*/
  
  /* Default: FFAPP = 'T' (correction for flatfielding was applied) */
  /* This variable is not actually used anywhere  */
/*fits_read_key(infile, TLOGICAL, "FFAPP",  &(resp->ffapp),  0, &status);
  if (status) {
    status = 0;
    resp->ffapp= 1;*/
  /* headas_chat(2, "NOTE: FFAPP keyword not found, assuming FALSE\n"); */
/*}*/

  /* Default: NGPIXAPP = 'T' (correction for num. of dets. was applied) */
  /* This variable is not actually used anywhere  */
/*fits_read_key(infile, TLOGICAL, "NGPIXAPP",  &(resp->ngpixapp),  0, &status);
  if (status) {
    status = 0;
    resp->ngpixapp = 1;*/
    /* headas_chat(2, "NOTE: NGPIXAPP keyword not found, assuming FALSE\n");*/
/*}*/

  /* Default: NGOODPIX = def_ngoodpix */
  /* This variable is not actually used anywhere  */
/*fits_read_key(infile, TDOUBLE, "NGOODPIX",  &(resp->ngoodpix),  0, &status);
  if (status) {
    status = 0;
    resp->ngoodpix= def_ngoodpix;*/
    /* headas_chat(2, 
     * "NOTE: NGOODPIX keyword not found, using default value = %f\n",
     * def_ngoodpix);
     * if (resp->ngpixapp)
     *  headas_chat(1, 
     * "WARNING: NGPIXAPP = T but NGOODPIX keyword not found\n");
     */
/*}*/

  /* ================================================================== */

  /* NFAPP (Default is 'F': correction for nearfielding was not applied) */

  read_col_or_keyword(infile, TLOGICAL, "NFAPP", parms->row, 0, 
      &(resp->nfapp), 0, &status);
  if (status) {
    status = 0;
    resp->nfapp = 0;
    headas_chat(2, "NOTE: NFAPP keyword not found, assuming FALSE\n");
  }

  /* Give a warning if FLIGHT_DATA is FALSE and NFAPP is FALSE */

  if ((!parms->flight_data)&&(!resp->nfapp)) {
    headas_chat(3,"---\n");
    headas_chat(1, "WARNING: flight_data = NO and NFAPP = F\n");
    headas_chat(1, "   batdrmgen will not produce properly normalized\n");
    headas_chat(1, "   response for ground data unless near-fielding\n");
    headas_chat(1, "   correction is applied\n");
  }

  read_col_or_keyword(infile, TDOUBLE, "MSKWTSQF", parms->row, 0, 
      &(resp->mskwtsqf), 0, &status);
  if (status) {
    status = 0;
    resp->mskwtsqf=def_mskwtsqf;
    /* resp->mskwtsqf=def_mskwtsqf*resp->ngoodpix*resp->pcodefr; */
    headas_chat(3,"---\n");
    headas_chat(1, "WARNING: MSKWTSQF keyword not found\n");
    headas_chat(1, "    using default value = %f\n",resp->mskwtsqf);
    /* headas_chat(1, "    (= 0.274 * NGOODPIX * PCODEFR)\n"); */
    headas_chat(1, "    response may not be properly normalized\n");
  }

  /* Now move to EBOUNDS extension and read energy bins */

  fits_movnam_hdu(infile, BINARY_TBL, "EBOUNDS", 0, &status);
  resp->nphabins = 0;
  headas_chat(5,"Using fits_get_num_rows to read the EBOUNDS extension\n");
  fits_get_num_rows(infile, &nbins, &status);

  /* Error condition */

  if (status) {
    int mystatus = 0;
    resp->nphabins = 0;
    fprintf(stderr, "ERROR: could not move to EBOUNDS extension\n");
    fits_close_file(infile, &mystatus);
    return status;
  }

  resp->nphabins = nbins;
  resp->nphabins_pre = nbins*SUB_BINS;

  /* Allocate memory for emin and emax */

  resp->emin = (double *)malloc(sizeof(double)*nbins);
  resp->emax = (double *)malloc(sizeof(double)*nbins);

  /* Error condition */

  if ((resp->emin == 0) || (resp->emax == 0)) {
    int mystatus = 0;
    resp->nphabins = 0;
    if (resp->emin) free(resp->emin);
    if (resp->emax) free(resp->emax);
    fprintf(stderr, "ERROR: could not allocate EBOUNDS storage\n");
    fits_close_file(infile, &mystatus);
    return MEMORY_ALLOCATION;
  }

  /* Read E_MIN and E_MAX from the fits file */

  headas_chat(5,"Using fits_get_colnum to determine the columns\n");
  headas_chat(5,"for the energy bin edges\n");

  fits_get_colnum(infile, CASEINSEN, "E_MIN", &emincol, &status);
  fits_get_colnum(infile, CASEINSEN, "E_MAX", &emaxcol, &status);

  headas_chat(5,"Using fits_read_col to read the energy bin edges\n");

  fits_read_col(infile, TDOUBLE, emincol, 1, 1, nbins, 0, 
		resp->emin, 0, &status);
  fits_read_col(infile, TDOUBLE, emaxcol, 1, 1, nbins, 0, 
		resp->emax, 0, &status);

  /* Error condition */

  if (status) {
    int mystatus = 0;
    resp->nphabins = 0;
    if (resp->emin) free(resp->emin);
    if (resp->emax) free(resp->emax);
    fprintf(stderr, "ERROR: could not read EBOUNDS data\n");
    fits_close_file(infile, &mystatus);
    return status;
  }

  /* Allocate memory for emin_pre and emax_pre */

  resp->emin_pre = (double *)malloc(sizeof(double)*nbins*SUB_BINS);
  resp->emax_pre = (double *)malloc(sizeof(double)*nbins*SUB_BINS);

  /* Error condition */

  if ((resp->emin_pre == 0) || (resp->emax_pre == 0)) {
    int mystatus = 0;
    resp->nphabins = 0;
    if (resp->emin_pre) free(resp->emin_pre);
    if (resp->emax_pre) free(resp->emax_pre);
    fprintf
      (stderr, "ERROR: could not allocate pre-gaussian bin edge storage\n");
    fits_close_file(infile, &mystatus);
    return MEMORY_ALLOCATION;
  }

  /* Calculate the emin_pre and emax_pre arrays */

  for (i=0;i<resp->nphabins;i++) {
    for (j=0;j<SUB_BINS;j++)
      resp->emin_pre[i*SUB_BINS+j]=
	resp->emin[i]+(resp->emax[i]-resp->emin[i])/SUB_BINS*j;
  }

  for (i=0;i<resp->nphabins_pre-1;i++) 
    resp->emax_pre[i]=resp->emin_pre[i+1];

  resp->emax_pre[resp->nphabins_pre-1]=resp->emax[resp->nphabins-1];

  /* Report the contents of the resp structure 
   * (except for the arrays) */

  headas_chat(5,"resp.mskwtsqf= %e\n",resp->mskwtsqf);
  headas_chat(5,"resp.mjdref= %f\n",resp->mjdref);
  headas_chat(5,"resp.time= %f\n",resp->time);
  headas_chat(5,"resp.time_stop= %f\n",resp->time_stop);
  headas_chat(5,"resp.timezero= %f\n",resp->timezero);
  headas_chat(5,"resp.srcpos[0]= %f\n",resp->srcpos[0]);
  headas_chat(5,"resp.srcpos[1]= %f\n",resp->srcpos[1]);
  headas_chat(5,"resp.srcpos[2]= %f\n",resp->srcpos[2]);
  headas_chat(5,"resp.gainmeth= %s\n",resp->gainmeth);
  headas_chat(5,"resp.gainapp= %d\n",resp->gainapp);
/*headas_chat(5,"resp.pcodeapp= %d\n",resp->pcodeapp);*/
/*headas_chat(5,"resp.pcodefr= %f\n",resp->pcodefr);*/
/*headas_chat(5,"resp.ffapp= %d\n",resp->ffapp);*/
  headas_chat(5,"resp.nfapp= %d\n",resp->nfapp);
/*headas_chat(5,"resp.ngpixapp= %d\n",resp->ngpixapp);*/
/*headas_chat(5,"resp.ngoodpix= %f\n",resp->ngoodpix);*/
  headas_chat(5,"resp.nphabins = %d\n",resp->nphabins);
  headas_chat(5,"resp.nphabins_pre = %d\n",resp->nphabins_pre);
  headas_chat(5,"resp.emin and resp.emax:\n");
  for (i=0;i<resp->nphabins;i++)
    headas_chat(5,"  %d  %f  %f\n",i,resp->emin[i],resp->emax[i]);
  headas_chat(5,"resp.emin_pre and resp.emax_pre:\n");
  for (i=0;i<resp->nphabins_pre;i++)
    headas_chat(5,"  %d  %f  %f\n",i,resp->emin_pre[i],resp->emax_pre[i]);

  /* Close file if nothing went wrong */
  fits_close_file(infile, &status);
  return status;
}

/*
 * get_mutau
 *
 * inputs:
 *   fits_name
 *
 * outputs:
 *  fills mutau tables in multi_cal_parms
 */

int get_mutau(char *fits_name,long extno, struct multi_mt_struct *cal_parms) 
{

  fitsfile *fits_file;

  int status=0;
  int hdutype;
  int nulval=0;
  int anynul;

  long n_rows;
  int n_cols;
 
  char file_type_string[20];
  char comment[100];

  headas_chat(5,"...Inside get_mutau...\n ");
 
  /* open the fits file (extension 1) */

  if (fits_open_file(&fits_file, fits_name, READONLY, &status)) {
    return status;
  }

  if ((extno >= 0) && fits_movabs_hdu(fits_file, extno, &hdutype, &status)) {
    return status;
  }

  /* make sure it is the right extension */

  if (fits_read_key(fits_file, TSTRING, "CCNM0001", file_type_string, 
	comment, &status)) {
    fprintf(stderr,"ERROR:  searching for CCNM0001 keyword in calfile\n");
    fprintf(stderr,"        extension %ld:\n",extno);
    return status;
  }
  if (strcmp(file_type_string,"MT_VALUES")) {
    fprintf(stderr,
	"ERROR:  calfile extension %ld CCNM0001 keyword != MT_VALUES\n",
	extno);
    return -1;
  }

  /* get n_rows */
 
  if (fits_get_num_rows(fits_file, &n_rows, &status)) {
    return status;
  }

  headas_chat(5,"number of rows in MT_VALUES table: %d\n",n_rows);

  /* get n_cols */

  if (fits_get_num_cols(fits_file, &n_cols, &status)) {
    return status;
  }

  headas_chat(5,"number of columns in MT_VALUES table: %d\n",n_cols);
 
  /* read in the mutau_e,mutau_h and fraction vectors */

  if (fits_read_col(fits_file,TFLOAT,1,1,1,n_rows,&nulval,
	cal_parms->mutau_e,&anynul,&status)) {
    return status;
  }
  if (fits_read_col(fits_file,TFLOAT,2,1,1,n_rows,&nulval,
	cal_parms->mutau_h,&anynul,&status)) {
    return status;
  }
  if (fits_read_col(fits_file,TFLOAT,3,1,1,n_rows,&nulval,
	cal_parms->fraction,&anynul,&status)) {
    return status;
  }

  /* close the fits file */

  fits_close_file(fits_file, &status);

  /* return */

  headas_chat(5,"...Leaving get_mutau...\n ");

  return 0;

}
 
int get_cal_parms(char *fits_name, int use_mean, long extno, 
    struct multi_mt_struct *cal_parms,struct resp_struct *resp) 
{

  fitsfile *cal_fits_file;
  Parm_record  *param_table;
 
  int status=0;
  int hdutype;
  int nulval=0;
  int anynul;
  float *dummy,*theta,*phi,*gamma;
  float theta_o, phi_o, xo, yo, zo, N;
  long n_rows;
  int n_cols;
  int i,k;
  float temp, temp1, temp2, temp3;

  char file_type_string[20];
  char comment[100];

  headas_chat(5,"...Inside get_cal_parms...\n ");
  if (use_mean)
    headas_chat(4,"The method is MEAN\n");
  else headas_chat(4,"The method is TABLE\n");
 
  /* guard against divide-by-zero */
  xo =resp->srcpos[0]; if (xo == 0) xo = 1e-20; 
  yo =resp->srcpos[1]; if (yo == 0) yo = 1e-20;
  zo =resp->srcpos[2]; 

  /* open the fits file (extension extno) */

  if (fits_open_file(&cal_fits_file, fits_name, READONLY, &status)) {
    return status;
  }

  if ((extno >= 0) && 
      fits_movabs_hdu(cal_fits_file, extno, &hdutype, &status)) {
    return status;
  }

  /* make sure it is the right extension */

  if (fits_read_key(cal_fits_file, TSTRING, "CCNM0001", file_type_string, 
	comment, &status)) {
    fprintf(stderr,"ERROR:  searching for CCNM0001 keyword in calfile\n");
    fprintf(stderr,"        extension %ld:\n",extno);
    return status;
  }
  if (strcmp(file_type_string,"MTFUNC_PARMS")) {
    fprintf(stderr,
	"ERROR:  calfile extension %ld CCNM0001 keyword != MTFUNC_PARMS\n",
	extno);
    return -1;
  }

  /* allocate memory for gain_adj and sigma_coeffs vectors */

  cal_parms->gain_adj = (float *)malloc(sizeof(float)*6);
  cal_parms->sigma_coeffs = (float *)malloc(sizeof(float)*7);

  /* read in the parameter file format version number 
   * (set it to 1 if the keyword cannot be found) */

  if (fits_read_key(cal_fits_file, TINT, "BATFILEV", &(cal_parms->batfilev),
      0, &status)) {
    cal_parms->batfilev=1;
    status=0;
  }

  /* allocate memory for psv vector 
   * (how much depends on batfilev)
   * and set use_gain_adj
   * and set use_sigma */

  switch (cal_parms->batfilev) {
    case 1:
      cal_parms->psv= (float *)malloc(sizeof(float)*5);
      cal_parms->use_gain_adj=0;
      cal_parms->use_sigma=0;
      break;
    case 2:
      cal_parms->psv= (float *)malloc(sizeof(float)*14);
      cal_parms->use_gain_adj=0;
      cal_parms->use_sigma=0;
      break;
    case 3:
      cal_parms->psv= (float *)malloc(sizeof(float)*14);
      cal_parms->use_gain_adj=1;
      cal_parms->use_sigma=0;
      break;
    case 4:
      cal_parms->psv= (float *)malloc(sizeof(float)*14);
      cal_parms->use_gain_adj=1;
      cal_parms->use_sigma=1;
      break;
    default:
      fprintf(stderr, 
	  "ERROR: batdrmgen does not recognize format version %d\n",
	  cal_parms->batfilev);
      return 1;
      break;
  }

  /* read in the CFUNNCOF, CFUNEMIN, and CFUNEMAX keywords */

  if (fits_read_key(cal_fits_file, TINT, "CFUNNCOF", 
	&(cal_parms->cfunncof), 0, &status)) {
    cal_parms->cfunncof=0;
    status=0;
  }
  
  if (cal_parms->cfunncof> 999) {
    fprintf(stderr,
	"ERROR: value of keyword CFUNNCOF (%d) is too large\n",
	cal_parms->cfunncof);
    status=1;
  }

  if (fits_read_key(cal_fits_file, TFLOAT, "CFUNEMIN", 
	&(cal_parms->cfunemin), 0, &status)) {
    cal_parms->cfunemin=14.;
    status=0;
  }

  if (fits_read_key(cal_fits_file, TFLOAT, "CFUNEMAX", 
	&(cal_parms->cfunemax), 0, &status)) {
    cal_parms->cfunemax=130.;
    status=0;
  }

  /* allocate memory for cfunp vector */

  cal_parms->cfunp=
    (float *)malloc(cal_parms->cfunncof*sizeof(float));

  /* read in the fit parameters */

  if (!use_mean)
  {
  
  /* method = TABLE */

    /* get n_rows */
   
    if (fits_get_num_rows(cal_fits_file, &n_rows, &status)) {
      return status;
    }
  
    headas_chat(4,"number of rows in MTFUNC_PARMS table: %ld\n",n_rows);
    
    /* get n_cols */
  
    if (fits_get_num_cols(cal_fits_file, &n_cols, &status)) {
      return status;
    }
  
    headas_chat(4,"number of columns in MTFUNC_PARMS table: %ld\n",n_cols);
    
    /* read in the fit parameter arrays */
  
    theta = (float *) malloc (n_rows*sizeof(float));
    phi = (float *) malloc (n_rows*sizeof(float));
    gamma= (float *) malloc (n_rows*sizeof(float));
    dummy = (float *) malloc (n_rows*sizeof(float));
    param_table = (Parm_record *) malloc (n_rows*sizeof(Parm_record));
  
    /* read in theta values */

    if (fits_read_col(cal_fits_file,TFLOAT,3,1,1,n_rows,&nulval,
  	theta,&anynul,&status)) {
      return status;
    }
  
    /* read in phi values */
  
    if (fits_read_col(cal_fits_file,TFLOAT,4,1,1,n_rows,&nulval,
  	phi,&anynul,&status)) {
      return status;
    }
  
    /* calculate gamma */
   
    theta_o = atan(sqrt(xo*xo + yo*yo)/zo);
    phi_o = atan(yo/xo);
  
    for (k=0;k<n_rows;k++) {
      gamma[k] = acos(cos(theta[k])*cos(theta_o) + 
  				sin(theta[k])*sin(theta_o)*cos(phi[k]-phi_o));
      param_table[k].gamma = gamma[k];
    }
  
    /* read in the rest of the parameters 
     * and store them in the param_table array of structures*/
    
    if (fits_read_col(cal_fits_file,TFLOAT,5,1,1,n_rows,&nulval,
       dummy,&anynul,&status)) {
      return status;
    }
    for (i = 0;i<n_rows;i++) param_table[i].voltage = dummy[i];
  
    if (fits_read_col(cal_fits_file,TFLOAT,6,1,1,n_rows,&nulval,
  	dummy,&anynul,&status)) {
      return status;
    }
  
    for (i = 0;i<n_rows;i++) param_table[i].sigma = dummy[i];
  
    if (fits_read_col(cal_fits_file,TFLOAT,7,1,1,n_rows,&nulval,
  	dummy,&anynul,&status)) {
      return status;
    }
  
    for (i = 0;i<n_rows;i++) param_table[i].gain_coeff = dummy[i];
  
    if (fits_read_col(cal_fits_file,TFLOAT,8,1,1,n_rows,&nulval,
  	dummy,&anynul,&status)) {
      return status;
    }
  
    for (i = 0;i<n_rows;i++) param_table[i].gain_index = dummy[i];
  
    if (fits_read_col(cal_fits_file,TFLOAT,9,1,1,n_rows,&nulval,
  	dummy,&anynul,&status)) {
      return status;
    }
  
    for (i = 0;i<n_rows;i++) param_table[i].exp_lambda = dummy[i];
  
    if (fits_read_col(cal_fits_file,TFLOAT,10,1,1,n_rows,&nulval,
  	dummy,&anynul,&status)) {
      return status;
    }
  
    for (i = 0;i<n_rows;i++) param_table[i].exp_coeff = dummy[i];
  
    if (fits_read_col(cal_fits_file,TFLOAT,11,1,1,n_rows,&nulval,
  	dummy,&anynul,&status)) {
      return status;
    }
  
    for (i = 0;i<n_rows;i++) param_table[i].exp_index = dummy[i];
  
    if (fits_read_col(cal_fits_file,TFLOAT,12,1,1,n_rows,&nulval,
  	dummy,&anynul,&status)) {
      return status;
    }
  
    for (i = 0;i<n_rows;i++) param_table[i].norm = dummy[i];
  
    /* sort by gamma */
  
    qsort(param_table,n_rows,sizeof(Parm_record),record_compare);
    
    for (k=0;k<n_rows;k++) 
      headas_chat(4,"param_table[%d].gamma = %f, param_table[%d].norm = %f\n",
          k,param_table[k].gamma,k,param_table[k].norm);
  
    cal_parms->voltage = 0.0;
    cal_parms->sigma = 0.0;
    cal_parms->gain_coeff= 0.0;
    cal_parms->gain_index= 0.0;
    cal_parms->exp_coeff= 0.0;
    cal_parms->exp_index = 0.0;
    cal_parms->exp_lambda= 0.0;
    cal_parms->norm = 0.0; 
  
    N=0;
    for (i=0;i<3;i++) N += 1/param_table[i].gamma;
       
    for (i=0;i<3;i++) {
      cal_parms->voltage += param_table[i].voltage/(N*param_table[i].gamma);
      cal_parms->sigma += param_table[i].sigma/(N*param_table[i].gamma);
      cal_parms->gain_coeff += 
	param_table[i].gain_coeff/(N*param_table[i].gamma);
      cal_parms->gain_index += 
	param_table[i].gain_index/(N*param_table[i].gamma);
      cal_parms->exp_coeff += param_table[i].exp_coeff/(N*param_table[i].gamma);
      cal_parms->exp_index += param_table[i].exp_index/(N*param_table[i].gamma);
      cal_parms->exp_lambda += 
	param_table[i].exp_lambda/(N*param_table[i].gamma);
      cal_parms->norm  +=  param_table[i].norm/(N*param_table[i].gamma);
    }
   
     headas_chat(4,"1st closest cal_parms values:\n");
     headas_chat(4,"     voltage = %f\n",param_table[0].voltage);
     headas_chat(4,"     sigma = %f\n",param_table[0].sigma);
     headas_chat(4,"     gain_coeff = %f\n",param_table[0].gain_coeff);
     headas_chat(4,"     gain_index = %f\n",param_table[0].gain_index);
     headas_chat(4,"     exp_coeff = %f\n",param_table[0].exp_coeff);
     headas_chat(4,"     exp_index = %f\n",param_table[0].exp_index);
     headas_chat(4,"     exp_lambda = %f\n",param_table[0].exp_lambda);
     headas_chat(4,"     norm = %f\n",param_table[0].norm);
     headas_chat(4,"2nd closest cal_parms values:\n");
     headas_chat(4,"     voltage = %f\n",param_table[1].voltage);
     headas_chat(4,"     sigma = %f\n",param_table[1].sigma);
     headas_chat(4,"     gain_coeff = %f\n",param_table[1].gain_coeff);
     headas_chat(4,"     gain_index = %f\n",param_table[1].gain_index);
     headas_chat(4,"     exp_coeff = %f\n",param_table[1].exp_coeff);
     headas_chat(4,"     exp_index = %f\n",param_table[1].exp_index);
     headas_chat(4,"     exp_lambda = %f\n",param_table[1].exp_lambda);
     headas_chat(4,"     norm = %f\n",param_table[1].norm);
     headas_chat(4,"3rd closest cal_parms values:\n");
     headas_chat(4,"     voltage = %f\n",param_table[2].voltage);
     headas_chat(4,"     sigma = %f\n",param_table[2].sigma);
     headas_chat(4,"     gain_coeff = %f\n",param_table[2].gain_coeff);
     headas_chat(4,"     gain_index = %f\n",param_table[2].gain_index);
     headas_chat(4,"     exp_coeff = %f\n",param_table[2].exp_coeff);
     headas_chat(4,"     exp_index = %f\n",param_table[2].exp_index);
     headas_chat(4,"     exp_lambda = %f\n",param_table[2].exp_lambda);
     headas_chat(4,"     norm = %f\n",param_table[2].norm);
  
     headas_chat(5,"checking weighted gamma average:  N = %f\n",N);
  
    /* free memory used by dummy, theta, phi and gamma arrays */
  
     free (dummy);
     free (theta);
     free (phi);
     free (gamma);
     free (param_table);

  }
  else 
  {

    /* method = MEAN */

    fits_read_key(cal_fits_file, TFLOAT, "VOLTAGE", &(cal_parms->voltage),
	0, &status);
    fits_read_key(cal_fits_file, TFLOAT, "SIGMA", &(cal_parms->sigma),
	0, &status);
    fits_read_key(cal_fits_file, TFLOAT, "GAIN_CFF", &(cal_parms->gain_coeff), 
	0, &status);
    fits_read_key(cal_fits_file, TFLOAT, "GAIN_IND", &(cal_parms->gain_index), 
	0, &status);
    fits_read_key(cal_fits_file, TFLOAT, "EXP_CFF", &(cal_parms->exp_coeff),
	0, &status);
    fits_read_key(cal_fits_file, TFLOAT, "EXP_IND", &(cal_parms->exp_index),
	0, &status);
    fits_read_key(cal_fits_file, TFLOAT, "EXP_LAMB", &(cal_parms->exp_lambda),
	0, &status);
    fits_read_key(cal_fits_file, TFLOAT, "NORM_ADJ", &(cal_parms->norm),
	0, &status);
  
    if (status) { 
      return status;
    }

  }

  /* gain_adj parameters */

  fits_read_key(cal_fits_file, TFLOAT, "GAIN_0", 
      &temp, 0, &status);
  if (!status) cal_parms->gain_adj[0]=temp;
  else status=0;

  fits_read_key(cal_fits_file, TFLOAT, "GAIN_1", 
      &temp, 0, &status);
  if (!status) cal_parms->gain_adj[1]=temp;
  else status=0;
  
  fits_read_key(cal_fits_file, TFLOAT, "GAIN_2", 
      &temp, 0, &status);
  if (!status) cal_parms->gain_adj[2]=temp;
  else status=0;
  
  fits_read_key(cal_fits_file, TFLOAT, "GAIN_3", 
      &temp, 0, &status);
  if (!status) cal_parms->gain_adj[3]=temp;
  else status=0;
  
  fits_read_key(cal_fits_file, TFLOAT, "GAIN_4", 
      &temp, 0, &status);
  if (!status) cal_parms->gain_adj[4]=temp;
  else status=0;
  
  fits_read_key(cal_fits_file, TFLOAT, "GAIN_5", 
      &temp, 0, &status);
  if (!status) cal_parms->gain_adj[5]=temp;
  else status=0;

  /* sigma_coeffs parameters */

  fits_read_key(cal_fits_file, TFLOAT, "SIGMA_0", 
      &temp, 0, &status);
  if (!status) cal_parms->sigma_coeffs[0]=temp;
  else status=0;

  fits_read_key(cal_fits_file, TFLOAT, "SIGMA_1", 
      &temp, 0, &status);
  if (!status) cal_parms->sigma_coeffs[1]=temp;
  else status=0;
  
  fits_read_key(cal_fits_file, TFLOAT, "SIGMA_2", 
      &temp, 0, &status);
  if (!status) cal_parms->sigma_coeffs[2]=temp;
  else status=0;
  
  fits_read_key(cal_fits_file, TFLOAT, "SIGMA_3", 
      &temp, 0, &status);
  if (!status) cal_parms->sigma_coeffs[3]=temp;
  else status=0;
  
  fits_read_key(cal_fits_file, TFLOAT, "SIGMA_4", 
      &temp, 0, &status);
  if (!status) cal_parms->sigma_coeffs[4]=temp;
  else status=0;
  
  fits_read_key(cal_fits_file, TFLOAT, "SIGMA_5", 
      &temp, 0, &status);
  if (!status) cal_parms->sigma_coeffs[5]=temp;
  else status=0;
  
  fits_read_key(cal_fits_file, TFLOAT, "SIGMA_6", 
      &temp, 0, &status);
  if (!status) cal_parms->sigma_coeffs[6]=temp;
  else status=0;
  
  /* transmission parameters */

  fits_read_key(cal_fits_file, TFLOAT, "SRC_RHO", &(cal_parms->src_density),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "SRC_LCFF", &(cal_parms->src_low_coeff),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "SRC_LIND", &(cal_parms->src_low_index),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "SRC_HCFF", &(cal_parms->src_high_coeff),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "SRC_HIND", &(cal_parms->src_high_index),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "SRC_SMTH", &(cal_parms->src_smooth),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "SRC_THCK", &(cal_parms->src_thickness),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "AIR_RHO", &(cal_parms->air_density),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "AIR_LCFF", &(cal_parms->air_low_coeff),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "AIR_LIND", &(cal_parms->air_low_index),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "AIR_HCFF", &(cal_parms->air_high_coeff),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "AIR_HIND", &(cal_parms->air_high_index),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "AIR_SMTH", &(cal_parms->air_smooth),
      0, &status);
  switch (cal_parms->batfilev) {
    float temp;
    case 1:
      fits_read_key(cal_fits_file, TFLOAT, "PSV_LCFF", 
          &temp, 0, &status);
      cal_parms->psv[0]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_LIND", 
          &temp, 0, &status);
      cal_parms->psv[1]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_HCFF", 
          &temp, 0, &status);
      cal_parms->psv[2]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_HIND", 
          &temp, 0, &status);
      cal_parms->psv[3]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_SMTH", 
          &temp, 0, &status);
      cal_parms->psv[4]=temp;
      break;
    case 2:
      fits_read_key(cal_fits_file, TFLOAT, "PSV_0", 
          &temp, 0, &status);
      cal_parms->psv[0]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_1", 
          &temp, 0, &status);
      cal_parms->psv[1]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_2", 
          &temp, 0, &status);
      cal_parms->psv[2]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_3", 
          &temp, 0, &status);
      cal_parms->psv[3]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_4", 
          &temp, 0, &status);
      cal_parms->psv[4]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_5", 
          &temp, 0, &status);
      cal_parms->psv[5]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_6", 
          &temp, 0, &status);
      cal_parms->psv[6]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_7", 
          &temp, 0, &status);
      cal_parms->psv[7]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_8", 
          &temp, 0, &status);
      cal_parms->psv[8]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_9", 
          &temp, 0, &status);
      cal_parms->psv[9]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_10", 
          &temp, 0, &status);
      cal_parms->psv[10]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_11", 
          &temp, 0, &status);
      cal_parms->psv[11]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_12", 
          &temp, 0, &status);
      cal_parms->psv[12]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_13", 
          &temp, 0, &status);
      cal_parms->psv[13]=temp;
      break;
    case 3:
      fits_read_key(cal_fits_file, TFLOAT, "PSV_0", 
          &temp, 0, &status);
      cal_parms->psv[0]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_1", 
          &temp, 0, &status);
      cal_parms->psv[1]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_2", 
          &temp, 0, &status);
      cal_parms->psv[2]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_3", 
          &temp, 0, &status);
      cal_parms->psv[3]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_4", 
          &temp, 0, &status);
      cal_parms->psv[4]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_5", 
          &temp, 0, &status);
      cal_parms->psv[5]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_6", 
          &temp, 0, &status);
      cal_parms->psv[6]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_7", 
          &temp, 0, &status);
      cal_parms->psv[7]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_8", 
          &temp, 0, &status);
      cal_parms->psv[8]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_9", 
          &temp, 0, &status);
      cal_parms->psv[9]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_10", 
          &temp, 0, &status);
      cal_parms->psv[10]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_11", 
          &temp, 0, &status);
      cal_parms->psv[11]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_12", 
          &temp, 0, &status);
      cal_parms->psv[12]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_13", 
          &temp, 0, &status);
      cal_parms->psv[13]=temp;
      break;
    case 4:
      fits_read_key(cal_fits_file, TFLOAT, "PSV_0", 
          &temp, 0, &status);
      cal_parms->psv[0]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_1", 
          &temp, 0, &status);
      cal_parms->psv[1]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_2", 
          &temp, 0, &status);
      cal_parms->psv[2]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_3", 
          &temp, 0, &status);
      cal_parms->psv[3]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_4", 
          &temp, 0, &status);
      cal_parms->psv[4]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_5", 
          &temp, 0, &status);
      cal_parms->psv[5]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_6", 
          &temp, 0, &status);
      cal_parms->psv[6]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_7", 
          &temp, 0, &status);
      cal_parms->psv[7]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_8", 
          &temp, 0, &status);
      cal_parms->psv[8]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_9", 
          &temp, 0, &status);
      cal_parms->psv[9]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_10", 
          &temp, 0, &status);
      cal_parms->psv[10]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_11", 
          &temp, 0, &status);
      cal_parms->psv[11]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_12", 
          &temp, 0, &status);
      cal_parms->psv[12]=temp;
      fits_read_key(cal_fits_file, TFLOAT, "PSV_13", 
          &temp, 0, &status);
      cal_parms->psv[13]=temp;
      break;
    default:
      fprintf(stderr, 
	  "ERROR: batdrmgen does not recognize format version %d\n",
	  cal_parms->batfilev);
      return 1;
      break;
  }

  fits_read_key(cal_fits_file, TFLOAT, "PB_RHO", &(cal_parms->pb_density),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "PB_LCFF", &(cal_parms->pb_low_coeff),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "PB_LIND", &(cal_parms->pb_low_index),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "PB_HCFF", &(cal_parms->pb_mid_coeff),
      0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "PB_HIND", &(cal_parms->pb_mid_index),
      0, &status);

  /* correction coefficients */

  for (i=0;i<cal_parms->cfunncof;i++) {
    char coeff_name[8];
    char coeff_num[3];
    float temp;
    strcpy(coeff_name,"CFUNP");
    sprintf(coeff_num,"%d",i);
    strncat(coeff_name,coeff_num,3);
    fits_read_key(cal_fits_file, TFLOAT, coeff_name, &temp, 0, &status);
    cal_parms->cfunp[i]=temp;
  }

  if (status) {
    return status;
  }

  /* Lead tile edge effect normalization */
  fits_write_errmark();
  fits_read_key(cal_fits_file, TFLOAT, "PB_EDGEN", &(cal_parms->pb_edge_norm),
      0, &status);
  fits_clear_errmark();

  /* Fall-back normalization */
  if (status) {
    status = 0;
    cal_parms->pb_edge_norm = 1.0;
  }


  /* assume the pre-smooth calibration file is being used */

  cal_parms->pb_smooth=1;
  cal_parms->pb_high_coeff=cal_parms->pb_mid_coeff;
  cal_parms->pb_high_index=cal_parms->pb_mid_index;

  /* Try to read in PB_SMTH, PB_VIND, PB_VCFF */

  fits_read_key(cal_fits_file, TFLOAT, "PB_SMTH", &temp1, 0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "PB_VCFF", &temp2, 0, &status);
  fits_read_key(cal_fits_file, TFLOAT, "PB_VIND", &temp3, 0, &status);

  /* If they are present, use them */

  if (!status) {
    cal_parms->pb_smooth=temp1;
    cal_parms->pb_high_coeff=temp2;
    cal_parms->pb_high_index=temp3;
  }
  else {

    /* If not, try to read in PB_SMTH, PB_MIND, PB_MCFF */

    int status2=0;

    fits_read_key(cal_fits_file, TFLOAT, "PB_SMTH", &temp1, 0, &status2);
    fits_read_key(cal_fits_file, TFLOAT, "PB_MCFF", &temp2, 0, &status2);
    fits_read_key(cal_fits_file, TFLOAT, "PB_MIND", &temp3, 0, &status2);

    /* If they are present, use them */

    if (!status2) {

      cal_parms->pb_smooth=temp1;
      cal_parms->pb_mid_coeff=temp2;
      cal_parms->pb_mid_index=temp3;

    }
  }

  status=0;
  fits_close_file(cal_fits_file, &status);

  headas_chat(5,"...Leaving get_cal_parms...\n ");
  return 0;

}



int record_compare (void const *p1, void const *p2)
{
  Parm_record *r1 = (Parm_record *)p1;
  Parm_record *r2 = (Parm_record *)p2;

  if (r1->gamma < r2->gamma) return -1;
  else if (r1->gamma > r2->gamma) return 1;
  return 0;
}


int update_respfile (char *pha_name, char *resp_name, int row)
  /* update the RESPFILE keyword in the pha file with the name of the
   * response file (row is ignored).  If a RESPFILE *column* exists,
   * then fill the corresponding cell instead (using the row
   * specified).
   */
{
  fitsfile *pha_file;
  int status=0, closestatus = 0;
  int colnum;
  char *dest = 0;
  char *extname="SPECTRUM";

  fits_write_errmark();
  fits_open_file(&pha_file,pha_name,READWRITE,&status);
  if (status) {
    headas_chat(3,"---\n");
    /* This is not an error. User can still specify the response matrix
       manually. */
    headas_chat(1, "WARNING: could not open file '%s' for update\n",pha_name);
    fits_clear_errmark();
    return 0;
  }    
  fits_movnam_hdu(pha_file,BINARY_TBL,extname,0,&status);
  if (status) {
    headas_chat(3,"---\n");
    headas_chat(1, 
		"WARNING: could not find extension '%s' in \n"
		"   file '%s'\n",extname,pha_name);
    fits_clear_errmark();
    return 0;
  }    
  fits_get_colnum(pha_file, CASEINSEN, "RESPFILE", &colnum, &status);
  if (status) {
    /* No column found.  Modify the RESPFILE keyword. */
    status = 0;
    fits_update_key(pha_file, TSTRING, "RESPFILE", resp_name,
		    "Redistribution Matrix file (RMF)", &status);
    dest = "keyword";
  } else {
    /* Column found. First delete any existing RESPFILE keyword... */
    fits_delete_key(pha_file, "RESPFILE", &status);
    status = 0; /* ... without regard to whether it exists ... */

    /* ... and then Modify the RESPFILE table cell. */
    fits_write_col(pha_file, TSTRING, colnum, row, 1, 1, 
		   &resp_name, &status);
    dest = "column";
  }
    
  fits_close_file(pha_file,&closestatus);
  fits_clear_errmark();

  if (status) {
    headas_chat(3,"---\n");
    headas_chat(1, "WARNING: could not update RESPFILE %s in %s\n",
		dest, pha_name);
  }

  return 0;
}


int get_hk (char *hk_name, struct parm_struct *parms, 
    struct multi_mt_struct *cal_parms, int *dm_enable, double time)

  /* get_hk: read the dap housekeeping values pertinent to batdrmgen
   *
   * input:  hk_name:
   *            name of the DAP housekeeping file
   *         parms:
   *            structure giving batdrmgen parameter values
   *         cal_parms:
   *            structure giving values needed by batdrmgen to create
   *            the response
   *         time:
   *            the start time of the pha data
   *
   * output: dm_enable:
   *            a vector of 32768 integers, each specifying whether
   *            a particular detector is enabled (0: yes, 1: no)
   *         cal_parms->hv:
   *            a vector of 128 floats, each specifying the high voltage
   *            of a particular detector module
   *         cal_parms->vthr:
   *            a vector of 256 floats, each specifying the XA1 setpoint
   *            threshold of a particular sandwich
   */

{
  
  int status=0;
  fitsfile *hk_file;
  /* int enable_def=0; */
  long num_rows;
  long hk_row;
  int column_number;
  float nulval_f=-1;
  /* int nulval_i=-1; */
  double nulval_d=-1;
  int anynul;
  int i=0;
  int time_colnum;
  double *time_array;
  /* long enable_size=32768; */
  long hv_size=128;
  long vthr_size=256;

  /*
  fprintf(stdout,"enable_size: %ld\n",enable_size);
  */
  headas_chat(4,"hv_size: %ld\n",hv_size);
  headas_chat(4,"vthr_size: %ld\n",vthr_size);

  /* If the name was NONE or unspecified,
   * use default values */

  if (strcasecmp(hk_name,"none") == 0) {
    headas_chat(3,"---\n");
    headas_chat(1,"NOTE: no housekeeping file specified:\n");
    headas_chat(1,"  high voltage assumed to be %g V\n",parms->hv_def);
    for (i=0;i<vthr_size;i++) cal_parms->vthr[i]=parms->vthr_def;
    for (i=0;i<hv_size;i++) cal_parms->hv[i]=parms->hv_def;
    return 0;
  }

  /* open the housekeeping file */

  if (fits_open_file(&hk_file, hk_name, READONLY, &status)) {
    fits_report_error(stderr,status);
    return status;
  }

  /* move to the DAP_HK extension */

  if (fits_movnam_hdu(hk_file,BINARY_TBL,"DAP_HK",0,&status)) {
    fprintf(stderr,
	"ERROR: the hkfile does not have a DAP_HK extension\n");
    fits_report_error(stderr,status);
    return status;
  }

  /* find the TIME column */

  if (fits_get_colnum(hk_file,CASEINSEN,"TIME",&time_colnum,&status)) {
    fprintf(stderr,
	"ERROR: the hkfile does not have a TIME column\n");
    fits_report_error(stderr,status);
    return status;
  }
  headas_chat(4,"hkfile TIME column: %d\n",time_colnum);

  /* find the number of rows in the table */

  if (fits_get_num_rows(hk_file,&num_rows,&status)) {
    fits_report_error(stderr,status);
    return status;
  }
  headas_chat(4,"Number of rows in hkfile: %ld\n",num_rows);
  
  /* allocate memory for time_array */

  time_array=(double *)malloc(sizeof(double)*num_rows);

  /* read the time column into an array */

  if (fits_read_col(hk_file,TDOUBLE,time_colnum,1,1,num_rows,&nulval_d,
	time_array,&anynul,&status)) {
    fits_report_error(stderr,status);
    return status;
  }
  headas_chat(5,"hkfile TIME column values:\n");
  for (i=0;i<num_rows;i++) headas_chat(5,"%d   %f\n",i,time_array[i]);
  
  /* search time_array to find the proper element */

  hk_row=search_array(time_array,num_rows,time);
  hk_row++;
  headas_chat(4,"Row number to be used: %d\n",hk_row);
  headas_chat(4,"Time of this row: %f\n",time_array[i]);

  /* if hk_row is 0, it means time was earlier than the earliest time 
   * in this case, warn the user and use the row with the earliest time */

  if (hk_row==0) {
    headas_chat(3,"---\n");
    headas_chat(1,
	"WARNING: the start time of the pha data is earlier than\n");
    headas_chat(1,"  the earliest hkfile time.\n");
    headas_chat(1,
	"  Using hkfile values from the earliest time available: %f\n",
	time_array[0]);
    hk_row=1;
  }

  /* if hk_row is num_rows+1, it means time was later than the latest time
   * in this case, warn the user and use the row with the latest time */

  if (hk_row==num_rows+1) {
    headas_chat(3,"---\n");
    headas_chat(1,"WARNING: the start time of the pha data is later than\n");
    headas_chat(1,"  the latest hkfile time.\n");
    headas_chat(1,
	"  Using hkfile values from the latest time available: %f\n",
	time_array[num_rows-1]);
    hk_row=num_rows;
  }

  /* if there is only one row, warn the user */

  if (num_rows==3) {
    headas_chat(3,"---\n");
    headas_chat(1,"NOTE: there is only one row in the hkfile\n");
    headas_chat(1,"  Using values from this time: %f\n",time_array[0]);
  }

  /* Read in the channel enable (dm_enable) values */

  /*
  if (fits_get_colnum(hk_file,CASEINSEN,"DM_enable",&column_number,&status)) {
  */
    /* if the dm_enable column wasn't present, warn the user and
     * use default value */
  /*
    headas_chat(3,"WARNING: DM_enable column not found in hkfile./n");
    headas_chat(3,"  using default value: %d\n",enable_def);
    for (i=0;i<enable_size;i++) dm_enable[i]=enable_def;
    status=0;
  }
  else {

    if (fits_read_col(hk_file,TINT,column_number,hk_row,1,enable_size,&nulval_i,
	  dm_enable,&anynul,&status)) {
      fits_report_error(stderr,status);
      return status;
    
    }
  }
  */

  /* Read in the high voltage (hv) values */

  if (fits_get_colnum(hk_file,CASEINSEN,"BHVMon_Level",&column_number,
	&status)) {

    /* if the BHVMon_Level column wasn't present, warn the user and
     * use default value */

    headas_chat(3,"---\n");
    headas_chat(1,"WARNING: BHVMon_Level column not found in hkfile./n");
    headas_chat(1,"  using default value: %f\n",parms->hv_def);
    for (i=0;i<hv_size;i++) cal_parms->hv[i]=parms->hv_def;
    status=0;
  }

  else {

    headas_chat(5,"BHVMon_Level column: %f\n",column_number);
    if (fits_read_col(hk_file,TFLOAT,column_number,hk_row,1,hv_size,&nulval_f,
	  cal_parms->hv,&anynul,&status)) {
      fits_report_error(stderr,status);
      return status;
    }

    /* fill nulls with the default value */

    if (anynul) {
      headas_chat(3,"---\n");
      headas_chat(1,
	  "WARNING: One or more NULL values found in BHVMon_Level column.\n");
      headas_chat(1,"  Using default value: %f\n",parms->hv_def);
      for (i=0;i<hv_size;i++) {
	if (cal_parms->hv[i]==nulval_f) cal_parms->hv[i]=parms->hv_def;
      }
    }
  }

  /* Read in the voltage threshold (vthr) values */

  if (fits_get_colnum(hk_file,CASEINSEN,"DM_HK_Vthr",&column_number,&status)) {

    /* if the DM_HK_Vthr column wasn't present, warn the user and
     * use default value */

    headas_chat(2,"WARNING: DM_HK_Vthr column not found in hkfile./n");
    headas_chat(2,"  using default value: %f\n",parms->vthr_def);
    for (i=0;i<vthr_size;i++) cal_parms->vthr[i]=parms->vthr_def;
    status=0;
  }

  else {

    headas_chat(5,"DM_HK_Vthr column: %f\n",column_number);
    anynul=0;
    if (fits_read_col(hk_file,TFLOAT,column_number,hk_row,1,vthr_size,&nulval_f,
	  cal_parms->vthr,&anynul,&status)) {
      fits_report_error(stderr,status);
      return status;
    }

    /* fill nulls with the default value */

    if (anynul) {
      headas_chat(3,"---\n");
      headas_chat(1,
	  "WARNING: One or more NULL values found in DM_HK_Vthr column.\n");
      headas_chat(1,"  Using default value: %f\n",parms->vthr_def);
      for (i=0;i<vthr_size;i++) {
	if (cal_parms->vthr[i]==nulval_f) cal_parms->vthr[i]=parms->vthr_def;
      }
    }
  }

  /* That's all, folks! */

  free(time_array);
  return 0;

}


int sum_detmask (int *detmask, int *san_ngood)

  /* sum_detmask: find the number of "good" detectors in each sandwich
   *              from the detmask
   *
   * input:  detmask:
   *           an array of detector mask values (1 is "good", 0 is "bad")
   *
   * output: san_ngood:
   *           an array of 256 integers, each specifying the number of
   *           "good" detectors in a particular sandwich
   *
   * san_ngood must be allocated before the function call */

{

  int san;
  int san_det;
  unsigned short int detid[NUM_DETECTORS];
  short int block[NUM_DETECTORS];
  short int dm[NUM_DETECTORS];
  short int det[NUM_DETECTORS];
  short int row[NUM_DETECTORS];
  short int col[NUM_DETECTORS];

  /* loop through each sandwich */

  for (san=0;san<NUM_SANDWICHES*NUM_BLOCKS;san++) {

    /* find the detector id for all of the detectors in this sandwich */

    for (san_det=0;san_det<NUM_DETECTORS;san_det++)
      detid[san_det]=san*NUM_DETECTORS+san_det;

    /* find rows and columns for all of the detectors in this sandwich */

    batidconvert(NUM_DETECTORS,detid,block,dm,det,row,col);

    /* initialize the san-th element */

    san_ngood[san]=0;

    /* add up the detmask values for these detectors */

    for (san_det=0;san_det<NUM_DETECTORS;san_det++)
      san_ngood[san]+=detmask[col[san_det]+DAP_COLS*row[san_det]];
  }
  return 0;
}


int sum_dm_enable (int *dm_enable, int *san_ngood)

  /* sum_dm_enable: find the number of "good" detectors in each sandwich
   *                from the dm_enable array
   *
   * input:  dm_enable:
   *           an array of 32768 integers, each specifying whether a
   *           particular detector was enabled or not
   *           (0 is "good", 1 is "bad")
   *
   * output: san_ngood:
   *           an array of 256 integers, each specifying the number of
   *           "good" detectors in a particular sandwich
   *
   * san_ngood must be allocated before the function call */

{

  int san;
  int san_det;

  /* loop through each sandwich */

  for (san=0;san<NUM_SANDWICHES*NUM_BLOCKS;san++) {

    /* initialize the san-th element */

    san_ngood[san]=0;

    /* find the number of enabled detectors in this sandwich */

    for (san_det=0;san_det<NUM_DETECTORS;san_det++)
      san_ngood[san]+=(dm_enable[san*NUM_DETECTORS+san_det]==0);

  }
  return 0;
}


int search_array (double *array, long array_size, double value)
  /* returns the largest index of array for which the value is less
   * than or equal to value
   *
   * if value < array[0], returns -1
   * if value > array[array_size-1], returns array_size
   */
{
  int low, high, mid;

  headas_chat(5,"Inside search_array\n");

  /* if the value is smaller than the first entry, return -1 */

  if (value<array[0]) return -1;

  /* if the value is larger than the last entry, return array_size */

  if (value>array[array_size-1]) return array_size;

  /* if there is only one entry, return 0 */

  if (array_size==1) return 0;

  /* if none of the above conditions were met, we have to search */

  low=0;
  high=array_size-1;

  while (high>low+1) {
    headas_chat(5,"low index: %d  high index: %d\n",low,high);
    mid=(high+low)/2;
    if (value<array[mid]) high=mid;
    else low=mid;
  }

  return low;
}


/* 
 *
 * read_col_or_keyword - read column value, but retry as a keyword
 *
 * This routine attempts to read a FITS column value (a single value
 * from a single row of the table).  If that fails, it will attempt to
 * read a keyword of the same name instead.
 *
 * fitsfile *fptr - pointer to FITS file, opened for reading;
 * int datatype - type code of data to read (see CFITSIO documentation);
 * char *colname - name of column / keyword to read;
 * int row - FITS row number to read (ignored for keyword);
 * void *nulval - substitute value, if column contains NULL value;
 *                 nulval = 0 indicates to do no special NULL processing.
 * void *anynul - upon return, *anynul is non-zero if NULL substitution occurred
 *
 * RETURNS: CFITSIO error status.
 *
 */

int read_col_or_keyword(fitsfile *fptr, int datatype, char *colname, 
                        int row, void *nulval, void *value, 
                        int *anynul, int *status)
{
   int colnum;

   /* Parameter error checking */
   if (status == 0) return NULL_INPUT_PTR;
   if (*status) return *status;
   if ((fptr == 0) || (colname == 0) || (*colname == 0) ||
       (value == 0)) return (*status = NULL_INPUT_PTR);

   /* Check to see if the column name is present in the FITS file */
   fits_write_errmark();
   fits_get_colnum(fptr, CASEINSEN, colname, &colnum, status);
   fits_clear_errmark();

   if (*status == 0) {

     /* Yes, the column was found ! */
     fits_read_col(fptr, datatype, colnum, row, 1, 1, nulval, 
                   value, anynul, status);
   } else if (*status == COL_NOT_FOUND) {

     /* No, the column was not found... try as a keyword */
     *status = 0;  /* Reset error status */
 
     if (anynul) *anynul = 0;  /* Reset to say no null values */
     /* Read as a keyword */
     fits_read_key(fptr, datatype, colname, value, 0, status);
   } 
     
   return *status;
}
