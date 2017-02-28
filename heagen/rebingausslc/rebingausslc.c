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

static char taskname[80] = "rebingausslc";
static char version[8] = "1.5";

/*
 * $Id: rebingausslc.c,v 1.6 2011/02/15 22:28:15 irby Exp $
 * C. Markwardt 
 *
 * rebingausslc - rebin light curves with gaussian statistics
 *
 * VERSION HISTORY
 *
 * 1.0 - CBM - initial functional version
 *
 * 1.1 - CBM - add more options for tstart/tstop parameters (now accept
 *    INDEF, or #KEYWORD)
 * 
 * 1.2 - CBM - fix memory allocation bug which caused seg-fault.
 *
 * 1.3 - CBM - fix bug which prevents writing TIMEDEL column.
 *
 * 1.4 - CBM - fix bug when user specifies INDEF for only one of tstart/tstop
 *
 */


#define TOOLSUB rebingausslc
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"


/* ----------------------------------------------------------------- */
/* Task parameters */
struct parm_struct 
{
  char *taskname;               /* Name of this task */
  char *taskver;                /* Version number string */
  char infile[PIL_PATH_MAX];    /* Input light curve file name */
  char outfile[PIL_PATH_MAX];   /* Output light curve file name */
  char gtifile[PIL_PATH_MAX];   /* Name of GTI/bin file */
  fitsfile *inptr;              /* FITS file for input */
  char ratecolumn[80], errorcolumn[80], timecolumn[80], expocolumn[80];
  double tstart, tstop;         /* Start and stop time of light curve */
  double tbinsize;              /* Time bin width */
  double timepixr;              /* Time bin alignment */
  int tbinmethod;               /* Time binning method */
};

/* ----------------------------------------------------------------- */
/* Constants used in this program */ 
#define UNIFORM  2  /* Uniform time bins */
#define USERBINS 0  /* User-defined time bins via GTI */
#define CONSTSNR 3  /* Constant signal to noise */
#define BAYESIAN 1  /* Bayesian (not used) */
#define INFBINS  4  /* Use time bins from infile */
#define MATCHBINS 5 /* Use time bins from light curve */

/* Default time */
#define TDEFAULT (-1e307)

/* ----------------------------------------------------------------- */
/* 
 * rebingausslc_getpar - get tool parameters from parameter file
 *
 * struct parm_struct *parms - parameters; upon return, this 
 *                             structure is filled
 *
 * RETURNS - 0 upon success, or a failure code.
 */
int rebingausslc_getpar(struct parm_struct *parms) 
{
  int status = 0;
  char method[FLEN_CARD];
  char tstartstr[FLEN_CARD], tstopstr[FLEN_CARD];
  fitsfile *inptr = 0;

  /* Default values */
  parms->infile[0] = 0;
  parms->outfile[0] = 0;
  parms->gtifile[0] = 0;
  parms->tstart = TDEFAULT;
  parms->tstop  = TDEFAULT;
  parms->tbinsize = 0;
  parms->inptr = 0;

  parms->timecolumn[0] = 0;
  parms->ratecolumn[0] = 0;
  parms->errorcolumn[0] = 0;
  parms->expocolumn[0] = 0;

  /* Parameter values */
  if ((status = PILGetString("infile", parms->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");

  else if ((status = PILGetFname("outfile", parms->outfile)))
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");

  else if ((status = PILGetReal("timedel", &parms->tbinsize)))
    fprintf(stderr, "Error reading the 'timedel' parameter.\n");

  else if ((status = PILGetString("timebinalg", method)))
    fprintf(stderr, "Error reading the 'timebinalg' parameter.\n");

  else if ((status = PILGetString("gtifile", parms->gtifile)))
    fprintf(stderr, "Error reading the 'gtifile' parameter.\n");

  else if ((status = PILGetString("tstart", (tstartstr))))
    fprintf(stderr, "Error reading the 'tstart' parameter.\n");

  else if ((status = PILGetString("tstop", (tstopstr))))
    fprintf(stderr, "Error reading the 'tstop' parameter.\n");
  
  else if ((status = PILGetReal("timepixr", &parms->timepixr)))
    fprintf(stderr, "Error reading the 'timepixr' parameter.\n");
  
  else if ((status = PILGetString("tcol", parms->timecolumn)))
    fprintf(stderr, "Error reading the 'tcol' parameter.\n");
  else if ((status = PILGetString("ratecol", parms->ratecolumn)))
    fprintf(stderr, "Error reading the 'ratecol' parameter.\n");
  else if ((status = PILGetString("errcol", parms->errorcolumn)))
    fprintf(stderr, "Error reading the 'errcol' parameter.\n");
  else if ((status = PILGetString("expocol", parms->expocolumn)))
    fprintf(stderr, "Error reading the 'expocol' parameter.\n");


  if (status) {
    return status;
  }

  /* ------------------- */
  /* Open input file */
  fits_open_data(&inptr, parms->infile, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s\n", parms->infile);
    return status;
  }
  parms->inptr = inptr;

  /* Parse the time binning algorithm */
  if (strncasecmp(method,"u",1) == 0) {
    parms->tbinmethod = UNIFORM;
  } else if (strncasecmp(method,"g",1) == 0) {
    parms->tbinmethod = USERBINS;
/*
  } else if (strncasecmp(method,"s",1) == 0) {
    parms->tbinmethod = CONSTSNR;
  } else if (strncasecmp(method,"m",1) == 0) {
    parms->tbinmethod = MATCHBINS;
*/
  } else {
    fprintf(stderr, "ERROR: time binning algorithm must be one of:\n"
            "      (u)niform, (g)ti, (s)nr or (m)atchlc\n");
    return -1;
  }

  /* Check for default value of gti file */
  if (strcasecmp(parms->gtifile,"none") == 0) {
    parms->gtifile[0] = 0;
  }

  /* Check for timedel == 0 */
  if ((parms->tbinmethod == UNIFORM) && (parms->tbinsize <= 0)) {
    fprintf(stderr, "ERROR: 'timedel' must be positive\n");
    return -1;
  }

  /* User specified the explicit time; NOTE that this may be
     overwritten by the later processing of tstartstr="INDEF" or
     tstartstr="#KEYNAME", and same for tstopstr */
  parms->tstart = atof(tstartstr);
  parms->tstop  = atof(tstopstr);

  /* If the user requests INDEF or #KEYWORD, then we need to read the
     input file. */
  if ((tstartstr[0] == '#') || (tstopstr[0] == '#') || 
      (strcasecmp(tstartstr,"INDEF") == 0) ||
      (strcasecmp(tstopstr,"INDEF") == 0)) {
    long int nrows;
    double *t = 0;
    double nulval = TDEFAULT;
    int anynul = 0;
    double tmin, tmax;
    int timecol, iin;

    headas_chat(5,"...user requested custom tstart/tstop...\n");
    fits_get_num_rows(inptr, &nrows, &status);
    fits_get_colnum(inptr, CASEINSEN, parms->timecolumn, &timecol, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not get information about %s column\n",
	      parms->timecolumn);
      return status;
    }

    /* Allocate and read the TIME column */
    t = (double *) malloc(sizeof(double)*nrows);
    if (t == 0) {
      fprintf(stderr, "ERROR: could not allocate memory for %s column\n",
	      parms->timecolumn);
      return status;
    }
    fits_read_col(inptr, TDOUBLE, timecol, 1, 1, nrows, &nulval, t, 
		  &anynul, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not read %s column\n",
	      parms->timecolumn);
      free(t);
      return status;
    }

    /* Compute the min/max */
    tmin = t[0]; tmax = t[0];
    for (iin = 0; iin<nrows; iin++) {
      if (t[iin] < tmin) { tmin = t[iin]; }
      if (t[iin] > tmax) { tmax = t[iin]; }
    }
    headas_chat(5, "...min(%s)=%f, max(%s)=%f...\n",
		parms->timecolumn, tmin,
		parms->timecolumn, tmax);

    /* Now actually fill in the tstart/tstop values */
    free(t);
    if (strcasecmp(tstartstr, "INDEF") == 0) {
      parms->tstart = tmin;
      headas_chat(5, "...setting tstart=%f...\n", tmin);
    }
    if (strcasecmp(tstopstr, "INDEF") == 0) {
      parms->tstop = tmax;
      headas_chat(5, "...setting tstop=%f...\n", tmax);
    }
    if (tstartstr[0] == '#') {
      fits_read_key(inptr, TDOUBLE, tstartstr+1, &(parms->tstart), 0, &status);
      if (status) {
	fprintf(stderr, "ERROR: could not read the %s keyword\n", 
		tstartstr+1);
	return status;
      }
      headas_chat(5, "...setting tstart=%s=%f...\n", tstartstr+1, parms->tstart);
    }
    if (tstopstr[0] == '#') {
      fits_read_key(inptr, TDOUBLE, tstopstr+1, &(parms->tstop), 0, &status);
      if (status) {
	fprintf(stderr, "ERROR: could not read the %s keyword\n", 
		tstopstr+1);
	return status;
      }
      headas_chat(5, "...setting tstop=%s=%f...\n", tstopstr+1, parms->tstop);
    }
    
  }

  /* Sanity checking on tstart/tstop */
  if ((parms->tstart == 0 || parms->tstop == 0) && (parms->tbinmethod == UNIFORM)) {
    fprintf(stderr, "ERROR: 'tstart' and 'tstop' must be non-zero.\n");
    return -1;
  }
  if (parms->tstop <= parms->tstart) {
    fprintf(stderr, "ERROR: 'tstop' must be greater than 'tstart'.\n");
    return -1;
  }

  /* If "timebinalg=gti" was given, then gtifile must also be set */
  if ((parms->tbinmethod == USERBINS) && (parms->gtifile[0] == 0)) {
    fprintf(stderr, "ERROR: when using timebinalg=gti, gtifile must also be specified\n");
    return -1;
  }

  /* If "timebinalg=match" was given, then gtifile must also be set */
  if ((parms->tbinmethod == MATCHBINS) && (parms->gtifile[0] == 0)) {
    fprintf(stderr, "ERROR: when using timebinalg=matchlc, gtifile must also be specified\n");
    return -1;
  }

  if (parms->timepixr < 0 && parms->timepixr != (-1)) {
    fprintf(stderr, "ERROR: timepixr must be between 0 and 1, or equal to -1\n");
    return -1;
  }

  /* Default value of timepixr */
  if (parms->timepixr == -1) {
    parms->timepixr = 0.5;
  }

  return status;
}


/* ----------------------------------------------------------------- */
/*
 * banner - Print opening banner for user
 * 
 * struct parm_struct *parms - task parameters
 *
 * RETURNS - void
 */
void banner(struct parm_struct *parms)
{


  headas_chat(2, "******************************************\n");
  headas_chat(1, "         %s v%s\n", parms->taskname, parms->taskver);
  headas_chat(2, "------------------------------------------\n");
  headas_chat(2, "      Input File: %s\n", parms->infile);
  headas_chat(2, "     Output File: %s\n",parms->outfile);
  headas_chat(2, "      Time Range: %f to %f\n",
	 parms->tstart, parms->tstop);
  switch(parms->tbinmethod) {
  case UNIFORM:
    headas_chat(2, "  Binning Method: u = UNIFORM\n");
    headas_chat(2, "   Time Bin Size: %f (s)\n", parms->tbinsize);
    break;
/*
  case CONSTSNR:
    headas_chat(2, "  Binning Method: s = CONSTSNR\n");
    headas_chat(2, "    Signal/Noise: %f\n", parms->snr);
    if (parms->tbinsize > 0)
      headas_chat(2, "   Max. Time Bin: %f\n", parms->tbinsize);
    break;
*/
  case USERBINS:
    headas_chat(2, "  Binning Method: g = USERBINS\n");
    headas_chat(2, "  Time Bins From: %s\n", parms->gtifile);
    break;
  case INFBINS:
    headas_chat(2, "  Binning Method: i = INFILE\n");
    headas_chat(2, "  Time Bins From: input time binning\n");
    break;
  case MATCHBINS:
    headas_chat(2, "  Binning Method: m = MATCHLC\n");
    headas_chat(2, "  Time Bins From: %s (light curve)\n", parms->gtifile);
    break;
  case BAYESIAN:
    /* NOT USED */
    headas_chat(2, "  Binning Method: BAYESIAN\n");
    break;
  }
  headas_chat(2, "------------------------------------------\n");
}

/* ----------------------------------------------------------------- */
/* 
 * summary - Print feedback summary so user knows tool succeeded
 * 
 * struct parm_struct *parms - task parameters
 * long int nrows - number of input rows
 * long int ntbins - number of output rows
 * long int ngood - number of good output rows
 *
 * RETURNS - void
 */
void summary(struct parm_struct *parms,
	     long int nrows, long int ntbins, long int ngood)
{
  headas_chat(2, "  Rebinned %d input rows to %d output rows (%ld total good samples)\n",
	      nrows, ntbins, ngood);
  headas_chat(2, "------------------------------------------\n");
}


/* ----------------------------------------------------------------- */
/* 
 * prepfile - Prepare the file for output (LIGHTCURVE or SPECTRUM)
 *
 * A new table extension will be created in the output file.
 *
 * fitsfile *infile - input file, opened for reading at LC ext
 * fitsfile *outfile - output file, opened for writing
 * int ncols - number of columns to be created
 * int nrows - number of rows to be created
 * char *colnames[] - names of output columns (TTYPEn)
 * char *colforms[] - formats of output columns (TFORMn)
 * char *colunits[] - units of output columns (TUNITn)
 * char *extname - name of extension to be created
 * int append - reserved, set to zero
 *
 * RETURNS - CFITSIO status code
 */
int prepfile(fitsfile *infile, fitsfile *outfile, 
	     int ncols, int nrows, 
	     char *colnames[], char *colforms[], char *colunits[], 
	     char *extname, int append)
{
  int status = 0;
  int i;
  int nkeys = 0;
  char card[FLEN_CARD];

  fits_create_tbl(outfile, BINARY_TBL, nrows, ncols, 
		  colnames, colforms, colunits, extname, &status);
  if (status) {
    fprintf(stderr, "ERROR: Could not create output table\n");
    return status;
  }    

  /* True if a keyword is a WCS keyword */
#define is_wcs_keyword(card) \
         ((isdigit(card[0]) && isdigit(card[5]) && \
	    (!strncmp(&card[1],"CUNI",4) || !strncmp(&card[1],"CTYP",4) || \
	     !strncmp(&card[1],"CRVL",4) || !strncmp(&card[1],"CDLT",4) || \
	     !strncmp(&card[1],"CRPX",4))) || \
          (isdigit(card[0]) && isdigit(card[1]) && isdigit(card[2]) && \
	   (!strncmp(&card[2],"PC",2) || !strncmp(&card[2],"CD",2)))) 

  /* True if a keyword is a CIAO data-model keyword.  Keep
     DTYPE/DVAL/DUNIT since they are not column descriptors */
#define is_dm_keyword(card) \
         (!strncmp(card,"MTYPE",5) || !strncmp(card,"MFORM",5) || \
	  !strncmp(card,"METYP",5) || !strncmp(card,"DSTYP",5) || \
	  !strncmp(card,"DSVAL",5) || !strncmp(card,"DSREF",5))


  /* Copy all the user keywords (not the structural keywords) */
  fits_get_hdrspace(infile, &nkeys, NULL, &status);
  
  for (i = 1; i <= nkeys; i++) {
    fits_read_record(infile, i, card, &status);
    
    /* Copy HISTORY keywords and any user keywords, but not COMMENTs */
    if (! (fits_get_keyclass(card) >= TYP_REFSYS_KEY) ) continue;
    if (strncmp(card, "COMMENT ", 7) == 0) continue;
    if (is_wcs_keyword(card)) continue;
    if (is_dm_keyword(card)) continue;

    fits_write_record(outfile, card, &status);
  }
    
  fits_set_hdustruc(outfile, &status);
  return status;
}


/* ----------------------------------------------------------------- */
/* 
 * writelc - Write the FITS light curve file
 * 
 * fitsfile *infile - input file, opened for reading at LC ext
 * fitsfile *outfile - output file, opened for writing
 * struct parm_struct *parms - task parameters
 * double *time - output TIME column values
 * double *twidth - output TIMEDEL column values (if not UNIFORM sampling)
 * double *rate - output RATE column values
 * double *error - output ERROR column values
 * int *nsamp - output number of samples per row
 * double tstart, tstop - start/stop time of light curve
 * double timepixr - output TIMEPIXR value
 * double timedel - output TIMEDEL value (if UNIFORM samp)
 * int nrows - number of rows to be created
 * int nebins - number of vector samples per row
 * double ontime - total exposure time
 * double nulval - TNULLn value
 * 
 * RETURNS - CFITSIO status code
 */
 
int writelc(fitsfile *infile, fitsfile *outfile, struct parm_struct *parms, 
	    double *time, double *twidth, 
	    double *rate, double *error, int *nsamp, 
	    double tstart, double tstop, double timepixr, double timedel,
	    int nrows, int nebins, double ontime, double nulval)
{
  int status = 0;
  int ncols;
  char *colnames[] = { "TIME",   "RATE",  "ERROR", "NSAMP",
		       "TIMEDEL"};
  char *colforms[] = { "D",      "D",     "D",     "J",
		       "D"       };
  char *colunits[] = { "s",      "count/s", "count/s", "",
		       "s"       };
  char *extname    = "RATE";
  char tform_rate[10] = "";
  char tform_nsamp[10] = "";
  char creator[FLEN_CARD];


  /* Light curve:
     Time
     TIMEDEL - if non-uniform bins
     RATE
     ERROR
     keywords: TIMEDEL (if needed), TSTART, TSTOP, BACKAPP=T, TIMEPIXR, 
               ONTIME, HDUCLASS=OGIP, HDUCLAS1=LIGHTCURVE, 
	       HDUCLAS2=TOTAL/NET/BKG, HDUCLAS3=COUNT/RATE
	       TIMVERSN=OGIP/93-003, EXTNAME=RATE
  */

  ncols = sizeof(colnames)/sizeof(colnames[0]);
  if (parms->tbinmethod == UNIFORM) ncols--;
  if (nebins > 1) {
    sprintf(tform_rate, "%dD", nebins);
    colforms[1] = tform_rate;
    colforms[2] = tform_rate;
    sprintf(tform_nsamp, "%dJ", nebins);
    colforms[3] = tform_nsamp;
  }

  status = 0;
  status = prepfile(infile, outfile, ncols, nrows,
		    colnames, colforms, colunits, extname, 0);
  sprintf(creator, "%s %s", parms->taskname, parms->taskver);
  fits_update_key(outfile, TSTRING, "CREATOR", creator,
		 "Program that created this FITS file", &status);
  if (status != 0) {
    fprintf(stderr, "ERROR: The output header could not be created\n");
    return status;
  }

  fits_update_key(outfile, TDOUBLE, "TSTART", &tstart, 0, &status);
  fits_update_key(outfile, TDOUBLE, "TSTOP", &tstop, 0, &status);
  fits_update_key(outfile, TDOUBLE, "TIMEPIXR", &timepixr, 
		  "Time bin alignment", &status);
  fits_update_key(outfile, TDOUBLE, "ONTIME", &ontime,
		  "[s] Accumulated on-time", &status);
  fits_update_key(outfile, TDOUBLE, "LIVETIME", &ontime,
		  "[s] ONTIME multiplied by DEADC", &status);
  fits_update_key(outfile, TDOUBLE, "EXPOSURE", &ontime,
		  "[s] Accumulated on-time", &status);
  fits_update_key(outfile, TSTRING, "HDUCLASS", "OGIP", 
		  "Conforms to OGIP/GSFC standards", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS1", "LIGHTCURVE", 
		  "Contains light curve", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS3", "RATE", 
		  "Light curve is count/s", &status);
  fits_update_key(outfile, TSTRING, "TIMVERSN", "OGIP/93-003", 
		  "Version of light curve format", &status);

  if (parms->tbinmethod == UNIFORM) {
    fits_update_key(outfile, TDOUBLE, "TIMEDEL", &timedel,
		    "[s] Light curve sampling period", &status);
  } else {
    /* Make sure this keyword is removed, because it could confuse the
       downstream software to have both a keyword and column.  A
       TIMEDEL keywod could be present if one was in the original
       input file, and the keywords were copied over. */
    int mystatus = 0;
    fits_write_errmark();
    fits_delete_key(outfile, "TIMEDEL", &mystatus);
    fits_clear_errmark();
  }

  if (status) {
    fprintf(stderr, "ERROR: Could write keywords to output table\n");
    return status;
  }    

  fits_write_col(outfile, TDOUBLE, 1, 1, 1, nrows, time, &status); /* TIME */

  fits_write_colnull(outfile, TDOUBLE, 2, 1, 1, nrows*nebins,
		     rate, &nulval, &status);    /* RATE */
  fits_write_colnull(outfile, TDOUBLE, 3, 1, 1, nrows*nebins,
		     error, &nulval, &status);   /* ERROR */
  fits_write_col(outfile, TINT,    4, 1, 1, nrows*nebins,
		     nsamp, &status);            /* NSAMP */

  /* Special case: variable bin width */
  if (parms->tbinmethod != UNIFORM) {
    fits_write_col(outfile, TDOUBLE, 5, 1, 1, nrows,
		   twidth, &status);            /* TIMEDEL */
  }

  status = HDpar_stamp(outfile, 0, &status);

  fits_set_hdustruc(outfile, &status);
  if (status) {
    fprintf(stderr, "WARNING: an error occured while writing to %s\n",
	    parms->outfile);
  } else {
    headas_chat(1, "  Light curve written to %s\n", parms->outfile);
  }
  return status;

}



/* ----------------------------------------------------------------- */
/* Do main work of tool */
int rebingausslc_work(struct parm_struct *parms)
{
  char *infile, *outfile;
  fitsfile *inptr = 0, *outptr = 0;
  struct gti_struct gti;
  int tbinmethod;
  int ncols;
  long int nrows, ntbins = 0, nvect, width, nvect2, nsamp, ngood = 0;
  double tstart, tstop, timedel, timepixr;
  double in_timepixr = 0.5, in_timedel = 0.0;
  double ontime = 0.0;   /* Accumulated on-time */
  double binexpo;
  int timecol, ratecol, errcol, expocol;
  int typecode;
  
  double *time = 0, *rate = 0, *error = 0;
  int *iseg = 0;
  double *rw = 0, *ww = 0, *tw = 0, *td = 0;
  int *nw = 0;

  double nulval = TDEFAULT;
  int anynul = 0;
  int j;
  int iin, iout;

  int status = 0, safestatus = 0;

  if (parms == 0) {
    return NULL_INPUT_PTR;
  }

  banner(parms);
  /* Use some local storage */
  infile = parms->infile;
  outfile = parms->outfile;
  tbinmethod = parms->tbinmethod;
  tstart = parms->tstart;
  tstop = parms->tstop;
  timedel = parms->tbinsize;
  timepixr = parms->timepixr;
  inptr = parms->inptr;

  /* ------------------- */
  /* Read input GTI */
  HDgti_init(&gti);
  if (parms->gtifile[0]) {
    headas_chat(5, "...opening GTI file...\n");
    HDgti_read(parms->gtifile, &gti, 0, 0, 0, 0, 0, &status);
    if (status) { 
      fprintf(stderr, "ERROR: could not open and read GTI file %s\n", 
	      parms->gtifile);
      goto CLEANUP;
    } else if (gti.ngti == 0) {
      fprintf(stderr, "ERROR: the input GTI was empty\n");
      goto CLEANUP;
    }
  }

  /* ------------------- */
  /* Number of output bins */
  if (tbinmethod == UNIFORM) {
    ntbins = ceil( (tstop - tstart)/timedel );
  } else {
    ntbins = gti.ngti;
  }
  headas_chat(5, "...number of output time bins = %d...\n", ntbins);

  /* ------------------- */
  /* Assume the file has already been opened by _getpar() */

  /* Read column info */
  fits_get_num_cols(inptr, &ncols, &status);
  fits_get_num_rows(inptr, &nrows, &status);
  fits_get_colnum(inptr, CASEINSEN, parms->timecolumn, &timecol, &status);
  fits_get_colnum(inptr, CASEINSEN, parms->ratecolumn, &ratecol, &status);
  fits_get_colnum(inptr, CASEINSEN, parms->errorcolumn, &errcol, &status);

  fits_get_coltype(inptr, ratecol, &typecode, &nvect, &width, &status);
  fits_get_coltype(inptr, errcol,  &typecode, &nvect2, &width, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not read column information of %s\n",
	    infile);
    goto CLEANUP;
  }

  if (nvect != nvect2) {
    status = BAD_DIMEN;
    fprintf(stderr, 
	    "ERROR: dimensions of the %s and %s columns do not agree.\n"
	    "       (%s is a %ld-vector and %s is a %ld-vector)\n",
	    parms->ratecolumn, parms->errorcolumn,
	    parms->ratecolumn, nvect, parms->errorcolumn, nvect2);
    goto CLEANUP;
  }
  headas_chat(5, "...rows=%ld cols=%d nvect=%d...\n",
	      nrows, ncols, nvect);
  headas_chat(2, "  Input file has %ld rows with %ld samples per row\n",
	      nrows, nvect);

  /* ------------ */
  /* Determine TIMEPIXR value (or default to 0.5) */
  fits_read_key(inptr, TDOUBLE, "TIMEPIXR", &in_timepixr, 0, &status);
  if (status) {
    status = 0;
    in_timepixr = 0.5;
  }

  fits_get_colnum(inptr, CASEINSEN, parms->expocolumn, &expocol, &status);
  if (status) {
    status = 0;
    expocol = -1;
    fits_read_key(inptr, TDOUBLE, "TIMEDEL", &in_timedel, 0, &status);
    if (status) {
      status = 0;
      in_timedel = 0;
    }
  }
  headas_chat(5, "...input timedel=%f timepixr=%f expocol=%d...\n",
	      in_timedel, in_timepixr, expocol);

  /* Allocate memory */
  headas_chat(5, "...allocating memory...\n");
  /* - computed time segment */
  iseg = (int *)malloc(sizeof(int)*nrows);

  /* - input data */
  time = (double *)malloc(sizeof(double)*nrows*(1+nvect+nvect)); /* TIME */
  rate = time + nrows;         /* RATE */
  error = rate + nrows*nvect;  /* ERROR */

  /* - output data */
  rw = (double *)malloc(sizeof(double)*ntbins*(nvect+nvect+2));  
  ww = rw + ntbins*nvect;                          /* Summed weights */
  tw = ww + ntbins*nvect;                          /* Output time value */
  td = tw + ntbins;                                /* Output bin width */

  /* - number of samples in output bin */
  nw = (int *)malloc(sizeof(int)*ntbins*nvect);

  if (time == 0 || iseg == 0 || rw == 0 || nw == 0) {
    status = MEMORY_ALLOCATION;
    fprintf(stderr, "ERROR: could not allocate memory for light curve data.\n");
    goto CLEANUP;
  }

  /* ------------ */
  /* Read light curve data */
  headas_chat(5, "...reading light curve data...\n");
  fits_read_col(inptr, TDOUBLE, timecol, 1, 1, nrows, &nulval, time, 
		&anynul, &status);
  if (in_timepixr != 0.5) {
    if (expocol > 0) {
      headas_chat(5, "...shifting to center of bin by variable bin size...\n");
      /* XXX: note abusing the rate[] variable temporarily */
      fits_read_col(inptr, TDOUBLE, expocol, 1, 1, nrows, &nulval, rate, 
		    &anynul, &status);
      for (iin=0; iin<nrows; iin++) time[iin] += rate[iin]*(0.5-in_timepixr);
    } else {
      double toff = in_timedel*(0.5-in_timepixr);
      headas_chat(5, "...shifting to center of bin by %f...\n", toff);
      for (iin=0; iin<nrows; iin++) time[iin] += toff;
    }
  }
  fits_read_col(inptr, TDOUBLE, ratecol, 1, 1, nrows*nvect, &nulval, rate,
		&anynul, &status);
  fits_read_col(inptr, TDOUBLE, errcol, 1, 1, nrows*nvect, &nulval, error,
		&anynul, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not read data from %s\n", infile);
    goto CLEANUP;
  }

  /* ------------ */
  /* Compute center-time of each output bin */
  headas_chat(5, "...computing output bin center-times...\n");
  if (tbinmethod == UNIFORM) {
    for (iout=0; iout<ntbins; iout++) {
      tw[iout] = tstart + timedel*iout + timedel*timepixr;
      td[iout] = timedel;
    }
  } else {
    for (iout=0; iout<ntbins; iout++) {
      tw[iout] = gti.start[iout] + (gti.stop[iout]-gti.start[iout])*timepixr;
      td[iout] = gti.stop[iout] - gti.start[iout];
    }
  }  

  /* ------------ */
  /* Determine which output bin each sample belongs to */
  headas_chat(5, "...determining the output bin numbers...\n");
  if (tbinmethod == UNIFORM) {
    for (iin=0; iin<nrows; iin++) {
      if (time[iin] == nulval) {
	iseg[iin] = -1;
      } else {
	iseg[iin] = floor( (time[iin]-tstart)/timedel );
	/* if (iseg[iin] == 6) { printf("%f %g %g\n", time[iin], rate[iin], error[iin]); } */
      }
    }
  } else if (tbinmethod == USERBINS) {
    status = HDgti_where(&gti, nrows, time, iseg, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not perform GTI time binning\n");
      goto CLEANUP;
    }
  } else {
    /* XXX */
    fprintf(stderr, "ERROR: Ackkk!!!!\n");
    goto CLEANUP;
  }

  /* ------------ */
  /* Initialize output values */
  nsamp = ntbins*nvect;      /* NSAMP is total number of time bins x energy bins */
  for (iout=0; iout<nsamp; iout++) {
    rw[iout] = ww[iout] = 0;
    nw[iout] = 0;
  }

  /* ------------ */
  /* Master loop over every input time sample */
  headas_chat(5, "...entering master loop...\n");
  for (iin=0; iin<nrows; iin++) {
    int iouti, ioutbase, iinbase;

    /* Locate the output bin that this input bin belongs to */
    iouti = iseg[iin];

    /* Exclude out-of-bounds samples */
    if (iouti < 0 || iouti >= ntbins) continue;
    
    /* Compute base bin number based on number of "energy" bins per time sample */
    ioutbase = iouti * nvect;  /* Output base bin number */
    iinbase  = iin   * nvect;  /* Input base bin number */

    /* XXX: handle too-wide time bins */
    
    for (j=0; j<nvect; j++) {
      double r, e, w;
      r = rate [iinbase + j];
      e = error[iinbase + j];
      if (r == nulval || e == nulval || e == 0 || iouti < 0) continue;

      /* Compute gaussian weight */
      w = 1.0/(e*e);

      rw[ioutbase+j] += r*w;  /* Weighted rate */
      ww[ioutbase+j] +=   w;  /* Summed weights */
      nw[ioutbase+j] ++;      /* Number of samples */
      /* printf("%d %d %d %f %f %f %f %f %f\n", iin, ioutbase, j, time[i], r, e, w, rw[ioutbase+j], ww[ioutbase+j]); */
    }
  }
    
  /* ------------ */
  /* Final step: compute weighted means and uncertainties */
  headas_chat(5, "...computing means...\n");
  ngood = 0;
  ontime = 0.0;
  binexpo = 0.0;
  for (iout=0; iout<nsamp; iout++) {
    /* Initialize the exposure per bin to zero */
    if (iout % nvect == 0) { binexpo = 0; }

    if (nw[iout] > 0 && ww[iout] > 0) {
      rw[iout] /= ww[iout];          /* Mean rate (weighted) */
      ww[iout] = 1.0/sqrt(ww[iout]); /* Standard error of mean */
      ngood ++;

      /* Compute exposure for this bin if we have a good data point. */
      if (binexpo == 0) {
	if (tbinmethod == USERBINS) {
	  binexpo = gti.stop[iout/nvect] - gti.start[iout/nvect];
	} else { 
	  binexpo = timedel;
	}
      }
    } else {
      rw[iout] = nulval;
      ww[iout] = nulval;
    }
    
    if ((iout % nvect) == (nvect - 1)) ontime += binexpo;
  }
  
  headas_chat(5,"...creating output file...\n");
  headas_clobberfile(outfile);
  fits_create_file(&outptr, outfile, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not create %s\n", parms->outfile);
    goto CLEANUP;
  }
  fits_copy_file(inptr, outptr, 1, 0, 0, &status);
  if (status == 0) {
    status = writelc(inptr, outptr, parms, 
		     tw, td, rw, ww, nw, tstart, tstop, timepixr, timedel, 
		     ntbins, nvect, ontime, nulval);
  }
  if (status) {
    fprintf(stderr, "ERROR: could not write output light curve %s\n", outfile);
    goto CLEANUP;
  }
  fits_copy_file(inptr, outptr, 0, 0, 1, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not copy trailing extensions of %s\n", outfile);
    goto CLEANUP;
  }

CLEANUP:    
  /* ------------------- */
  safestatus = 0;
  if (inptr) fits_close_file(inptr, &safestatus);
  safestatus = 0;
  if (outptr) fits_close_file(outptr, &safestatus);

  HDgti_free(&gti);
  if (iseg) free(iseg);
  if (time) free(time);
  if (rw) free(rw);
  if (nw) free(nw);

  /* ------------------- */
  summary(parms, nrows, ntbins, ngood);

  return status;
}

/* ----------------------------------------------------------------- */
/* Main point of entry for tool */
int rebingausslc(void)
{
  int status = 0;

  struct parm_struct parms;

  /* Register taskname and version. */

  set_toolname(taskname);
  set_toolversion(version);

  if ((status = rebingausslc_getpar(&parms)) != 0) {
    fprintf(stderr, "Could not read parameter file\n");
    return status;
  }

  parms.taskname = &taskname[0];
  parms.taskver  = &version[0];

  return rebingausslc_work(&parms);

}
