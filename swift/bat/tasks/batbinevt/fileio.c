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

#include "batbinevt.h"

/* 
 * Routines for performing output to FITS files in more or less OGIP 
 *   standard format
 *
 *  11 Apr 2003 - split out from batbinevt.c
 *
 * $Id: fileio.c,v 1.49 2010/12/16 06:26:42 craigm Exp $
 * C. Markwardt 
 */

/* ----------------------------------------------------------------- */
/* Prepare the file for output (LIGHTCURVE or SPECTRUM) */
int prepfile(fitsfile *infile, fitsfile *outfile, 
	     int ncols, int nrows, 
	     char *colnames[], char *colforms[], char *colunits[], 
	     char *comments[],
	     char *extname, int append, char *maskwt)
{
  int status = 0;
  int i;
  int nkeys = 0;
  char card[FLEN_CARD];
  char ttypen[FLEN_CARD];

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


  /* True if a keyword is a WCS keyword */
#define is_wcs_keyword(card) \
         ((isdigit(card[0]) && isdigit(card[5]) && \
	    (!strncmp(&card[1],"CUNI",4) || !strncmp(&card[1],"CTYP",4) || \
	     !strncmp(&card[1],"CRVL",4) || !strncmp(&card[1],"CDLT",4) || \
	     !strncmp(&card[1],"CRPX",4))) || \
          (isdigit(card[0]) && isdigit(card[1]) && \
	   (!strncmp(&card[2],"PC",2) || !strncmp(&card[2],"CD",2)))) 

  /* True if a keyword is a CIAO data-model keyword.  Keep
     DTYPE/DVAL/DUNIT since they are not column descriptors */
#define is_dm_keyword(card) \
         (!strncmp(card,"MTYPE",5) || !strncmp(card,"MFORM",5) || \
	  !strncmp(card,"METYP",5) || !strncmp(card,"DSTYP",5) || \
	  !strncmp(card,"DSVAL",5) || !strncmp(card,"DSREF",5))


  /* NOTE: copying of keywords depends on whether the maskwt parameter
		  was given or not.  If not, then the keywords are
		  copied from the input file.  If yes, then the
		  keywords are copied from the maskwt file */
		  
  /* Copy all the user keywords (not the structural keywords) */
  fits_get_hdrspace(infile, &nkeys, NULL, &status);
  
  for (i = 1; i <= nkeys; i++) {
    fits_read_record(infile, i, card, &status);
    /* headas_chat(5,"  %4d/%4d %s\n", fits_get_keyclass(card),TYP_REFSYS_KEY,card); */
    
    /* Copy HISTORY keywords and any user keywords, but not COMMENTs */
    if (! (fits_get_keyclass(card) >= TYP_REFSYS_KEY) ) continue;
    if (strncmp(card, "COMMENT ", 7) == 0) continue;
    if (is_wcs_keyword(card)) continue;
    if (is_dm_keyword(card)) continue;

    fits_write_record(outfile, card, &status);
  }
    
  if (maskwt && maskwt[0]) {
    fitsfile *maskfile;
    fits_open_file(&maskfile, maskwt, READONLY, &status);
    if (status == 0) {
      int safestatus = 0, keylength;
      char keyname[FLEN_CARD];
      
      fits_get_hdrspace(maskfile, &nkeys, NULL, &status);
      if (status == 0) for (i = 1; i <= nkeys; i++) {
	fits_read_record(maskfile, i, card, &status);
	/* Copy HISTORY keywords and any user keywords, but not COMMENTs */
	if (fits_get_keyclass(card) < TYP_REFSYS_KEY) continue;
	if (strncmp(card, "COMMENT ",8) == 0) continue;
	if (strncmp(card, "TSTART  ",8) == 0) continue;

	/* Handle more obscure WCS structural keywords */
	if (is_wcs_keyword(card)) continue;
	if (is_dm_keyword(card)) continue;

	fits_get_keyname(card, keyname, &keylength, &status);
	if (status == 0) { keyname[keylength] = 0; }
	fits_update_card(outfile, keyname, card, &status);
      }
      
      fits_close_file(maskfile, &safestatus);
    }
    
  }

  fits_set_hdustruc(outfile, &status);
  return status;
}

/* ----------------------------------------------------------------- */
int write_app_keys(fitsfile *outfile, struct parm_struct *parms, 
		   int *status)
{
  int i;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status) return *status;
  if (outfile == 0 || parms == 0) {
    return  (*status = NULL_INPUT_PTR);
  }

  for (i=0; i<parms->nappkeys; i++) {
    fits_update_key(outfile, TLOGICAL, 
		    parms->appkeynames[i],&(parms->appkeyvals[i]),
		    parms->appkeycomms[i], status);
  }

  return *status;
}


/* ----------------------------------------------------------------- */
/* Write the FITS light curve file */
int writelc(fitsfile *infile, fitsfile *outfile, struct parm_struct *parms, 
	    struct spectrum_struct *spect,
	    int nebins, float *emin, float *emax)
{
  int status = 0;
  int ncols;
  char *colnames[] = { "TIME",   "RATE",  "ERROR", "TOTCOUNTS", "FRACEXP",
		       "TIMEDEL"};
  char *colforms[] = { "D",      "D",     "D",     "J",         "D", 
		       "D"       };
  char *colunits[] = { "s",      "count/s", "count/s", "count", "",
		       "s"       };
  char *comments[] = { "Time of light curve bin",
		       "Light curve rate",
		       "Statistical error",
		       "Total counts in time bin",
		       "Fractional exposure",
		       "Time bin width" };
  char *extname    = "RATE";
  char tform_rate[10] = "";
  double ontime, deadc = 1.0;
  char keyname[FLEN_CARD];  /* Name of keyword */
  char eunit[FLEN_CARD];    /* Energy units */
  int picol;
  char creator[FLEN_CARD];
  int backapp = 1;
  int i;
  double *work;
  double nulval = NULL_COUNTS;

  if (spect->nsplits != 1) {
    fprintf(stderr, "INTERNAL ERROR: writelc can only write one split\n");
    return -1;
  }

  work = (double *) malloc(sizeof(double)*spect->ntbins);
  if (work == 0) return MEMORY_ALLOCATION;

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

  /* Unweighted data are not background subtracted */
  backapp = (parms->weighted)?1:0;
  ncols = sizeof(colnames)/sizeof(colnames[0]);
  if (parms->tbinmethod == UNIFORM) ncols--;
  if (spect->nebins > 1) {
    sprintf(tform_rate, "%dD", spect->nebins);
    colforms[1] = tform_rate;
    colforms[2] = tform_rate;
  }

  /* Handle case of counts, not rate */
  if (parms->rate == 0) {
    colnames[1] = "COUNTS";
    colunits[1] = "count";
    comments[1] = "Light curve counts";
    colunits[2] = "count";
  }

  /* Read energy unit from input file, default to keV */
  status = 0;
  fits_get_colnum(infile, CASEINSEN, parms->coltype, &picol, &status);
  if (status == 0) {
    sprintf(keyname, "TUNIT%d", picol);
    fits_read_key(infile, TSTRING, keyname, eunit, 0, &status);
  }
  if (status) strcpy(eunit, "keV");

  status = 0;
  status = prepfile(infile, outfile, ncols, spect->ntbins,  
		    colnames, colforms, colunits, comments, extname, 0, 
		    parms->maskwt);
  sprintf(creator, "%s %s", parms->taskname, parms->taskver);
  fits_update_key(outfile, TSTRING, "CREATOR", creator,
		 "Program that created this FITS file", &status);
  write_app_keys(outfile, parms, &status);
  if (status != 0) {
    fprintf(stderr, "ERROR: The output header could not be created\n");
    free(work);
    return status;
  }

  ontime = 0;
  for (i=0; i<spect->ntbins; i++) {
    ontime += spect->exposures[i];
  }

  fits_update_key(outfile, TDOUBLE, "TSTART", &spect->tstart, 0, &status);
  fits_update_key(outfile, TDOUBLE, "TSTOP", &spect->tstop, 0, &status);
  fits_update_key(outfile, TLOGICAL,"BACKAPP", &backapp, 
		  "Was background correction applied?", &status);
  fits_update_key(outfile, TDOUBLE, "TIMEPIXR", &parms->timepixr, 
		  "Time bin alignment", &status);
  fits_update_key(outfile, TDOUBLE, "ONTIME", &ontime,
		  "[s] Accumulated on-time", &status);
  fits_update_key(outfile, TDOUBLE, "LIVETIME", &ontime,
		  "[s] ONTIME multiplied by DEADC", &status);
  fits_update_key(outfile, TDOUBLE, "DEADC", &deadc,
		  "Dead time correction factor", &status);
  fits_update_key(outfile, TDOUBLE, "EXPOSURE", &ontime,
		  "[s] Accumulated on-time", &status);
  fits_update_key(outfile, TSTRING, "HDUCLASS", "OGIP", 
		  "Conforms to OGIP/GSFC standards", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS1", "LIGHTCURVE", 
		  "Contains light curve", &status);
  if (parms->weighted) {
    fits_update_key(outfile, TSTRING, "HDUCLAS2", "NET", 
		    "Light curve is background subtracted", &status);
  } else {
    fits_update_key(outfile, TSTRING, "HDUCLAS2", "TOTAL", 
		    "Light curve is source+background", &status);
  }
  if (parms->rate) {
    fits_update_key(outfile, TSTRING, "HDUCLAS3", "RATE", 
		    "Light curve is count/s", &status);
  } else {
    fits_update_key(outfile, TSTRING, "HDUCLAS3", "COUNT", 
		    "Light curve is units of count", &status);
  }
  fits_update_key(outfile, TSTRING, "TIMVERSN", "OGIP/93-003", 
		  "Version of light curve format", &status);
  fits_update_key(outfile, TSTRING, "FLUXMETH", 
		  (parms->weighted)?("WEIGHTED"):("RAW"),
		  "Flux extraction method", &status);

  /* Write out energy information */
  if (emin && emax && (nebins > 0)) {
    if (nebins == 1) {
      /* Single energy band */
      fits_update_key(outfile, TFLOAT, "E_MIN", &emin[0], 
		      "[keV] Minimum energy in light curve", &status);
      fits_update_key(outfile, TFLOAT, "E_MAX", &emax[nebins-1], 
		      "[keV] Maximum energy in light curve", &status);
      fits_update_key(outfile, TSTRING, "EUNIT", eunit, 
		      "Energy units of E_MIN and E_MAX", &status);
    } else {

      /* Multiple energy bands */
      for (i=0; i<nebins; i++) {

	sprintf(keyname, "E_MIN%d", i+1);
	fits_update_key(outfile, TFLOAT, keyname, &emin[i], 
			"[keV] Minimum band energy in light curve", &status);
	sprintf(keyname, "E_MAX%d", i+1);
	fits_update_key(outfile, TFLOAT, keyname, &emax[i],
			"[keV] Maximum band energy in light curve", &status);
      }
      fits_update_key(outfile, TSTRING, "EUNIT", eunit, 
		      "Energy units of E_MINn and E_MAXn", &status);
      fits_update_key(outfile, TINT, "NUMBAND", &nebins, 
		      "Number of energy bands", &status);
    }
  }

  if (parms->tbinmethod == UNIFORM) {
    fits_update_key(outfile, TDOUBLE, "TIMEDEL", &spect->tbinsize,
		    "[s] Light curve sampling period", &status);
  } else {
    /* Make sure this keyword is removed, because it could confuse the
       downstream software to have both a keyword and column.  A
       TIMEDEL keywod could be present if one was in the original
       input file, and the keywords were copied over. */
    int safestatus = 0;
    fits_write_errmark();
    fits_delete_key(outfile, "TIMEDEL", &safestatus);
    fits_clear_errmark();
  }
    
  if (status) {
    fprintf(stderr, "ERROR: Could write keywords to output table\n");
    free(work);
    return status;
  }    


  if (parms->timepixr != 0) {
    int i;
    fits_update_key(outfile, TDOUBLE, "TIMEPIXR", &parms->timepixr, 
		    "Time bin alignment", &status);
    for (i=0; i<spect->ntbins; i++) {
      work[i]  = spect->times[i];
      work[i] += (spect->tends[i]-spect->times[i])*parms->timepixr;
    }
    fits_write_col(outfile, TDOUBLE, 1, 1, 1, spect->ntbins, 
		   work, &status);            /* TIME */
  } else {
    fits_write_col(outfile, TDOUBLE, 1, 1, 1, spect->ntbins, 
		   spect->times, &status);    /* TIME */
  }

  fits_write_colnull(outfile, TDOUBLE, 2, 1, 1, spect->ntbins*spect->nebins, 
		     spect->weights, &nulval, &status);    /* RATE */
  if (spect->accum_wt2 && spect->weights2) {
    fits_write_colnull(outfile, TDOUBLE, 3, 1, 1, spect->ntbins*spect->nebins, 
		       spect->weights2, &nulval, &status);   /* ERROR */
  }
  fits_write_col(outfile, TLONG, 4, 1, 1, spect->ntbins, 
		 spect->counts, &status);     /* TOTCOUNTS */
  for (i=0; i<spect->ntbins; i++) {
    work[i] = (spect->tends[i]-spect->times[i]);
  }

  if (parms->tbinmethod != UNIFORM) {
    fits_write_col(outfile, TDOUBLE, 6, 1, 1, spect->ntbins, 
		   work, &status);            /* TIMEDEL */
  }

  /* Compute fractional exposure */
  for (i=0; i<spect->ntbins; i++) if (work[i] > 0) {
    work[i] = spect->exposures[i]/work[i];
  }
  fits_write_col(outfile, TDOUBLE, 5, 1, 1, spect->ntbins, 
		 work, &status);              /* FRACEXP */
  
  free(work);
  status = HDpar_stamp(outfile, 0, &status);
  headas_chat(1, "  Light curve written to %s\n", parms->outfile);

  fits_set_hdustruc(outfile, &status);
  return status;
}

/* ----------------------------------------------------------------- */
/* Write the OGIP spectral file header (both type I and type II) */
int writephahead(fitsfile *infile, fitsfile *outfile, struct parm_struct *parms, 
	    struct spectrum_struct *spect, int phatype, int *status)
{
  char creator[FLEN_CARD];
  char *chantype = parms->coltype;
  double ontime, timepixr = 0, deadc = 1.0;
  double telapse = 0;
  int backapp = 1;
  int quality = 0, grouping = 0, poiserr = 0;
  double sys_err = 0; /* , tnull_m1 = -1.0; */
  double areascal = 1.0, backscal = 1.0, corrscal = 0.0;
  int i;

  backapp = (parms->weighted)?1:0;

  ontime = 0;
  for (i=0; i<spect->ntbins; i++) {
    ontime += spect->exposures[i];
  }
  telapse = spect->tstop - spect->tstart;

  sprintf(creator, "%s %s", parms->taskname, parms->taskver);
  fits_update_key(outfile, TSTRING, "CREATOR", creator,
		 "Program that created this FITS file", status);
  fits_update_key(outfile, TDOUBLE, "TSTART", &spect->tstart, 0, status);
  fits_update_key(outfile, TDOUBLE, "TSTOP", &spect->tstop, 0, status);
  fits_update_key(outfile, TLOGICAL,"BACKAPP", &backapp, 
		  "Was background correction applied?", status);
  fits_update_key(outfile, TDOUBLE, "TIMEPIXR", &timepixr, 
		  "Time bin alignment", status);
  fits_update_key(outfile, TDOUBLE, "ONTIME", &ontime,
		  "[s] Accumulated on-time", status);
  fits_update_key(outfile, TDOUBLE, "LIVETIME", &ontime,
		  "[s] ONTIME multiplied by DEADC", status);
  fits_update_key(outfile, TDOUBLE, "DEADC", &deadc,
		  "Dead time correction factor", status);
  fits_update_key(outfile, TDOUBLE, "EXPOSURE", &ontime,
		  "[s] Accumulated exposure", status);
  fits_update_key(outfile, TDOUBLE, "TELAPSE", &telapse,
		  "[s] Total elapsed time from start to stop", status);
  fits_update_key(outfile, TSTRING, "HDUCLASS", "OGIP", 
		  "Conforms to OGIP/GSFC standards", status);
  fits_update_key(outfile, TSTRING, "HDUCLAS1", "SPECTRUM", 
		  "Contains spectrum", status);
  if (parms->weighted) {
    fits_update_key(outfile, TSTRING, "HDUCLAS2", "NET", 
		    "Spectrum is background subtracted", status);
  } else {
    fits_update_key(outfile, TSTRING, "HDUCLAS2", "TOTAL", 
		    "Spectrum is source+background", status);
  }
  if (parms->rate) {
    fits_update_key(outfile, TSTRING, "HDUCLAS3", "RATE",
		    "Spectrum is count/s", status);
  } else {
    fits_update_key(outfile, TSTRING, "HDUCLAS3", "COUNT", 
		    "Spectrum is units of count", status);
  }
  if (phatype == 1) {
    /* Do nothing,
       Type I spectrum does not use HDUCLAS4 */
    int safestatus = 0;
    fits_write_errmark();
    fits_delete_key(outfile, "HDUCLAS4", &safestatus);
    fits_clear_errmark();
  } else {
    /* Type II spectrum is officially designated TYPE:II */
    fits_update_key(outfile, TSTRING, "HDUCLAS4", "TYPE:II", 
		    "Multiple PHAs in this HDU", status);
  }
  fits_update_key(outfile, TSTRING, "PHAVERSN", "1992a",
		  "Version of spectrum format", status);
  fits_update_key(outfile, TSTRING, "HDUVERS", "1.2.0",
		  "Version of spectrum header", status);
  fits_update_key(outfile, TSTRING, "FLUXMETH", 
		  (parms->weighted)?("WEIGHTED"):("RAW"),
		  "Flux extraction method", status);

  /* Keywords required for XSPEC */
  fits_update_key(outfile, TDOUBLE, "AREASCAL", &areascal, 
		  "Nominal effective area", status);
  fits_update_key(outfile, TDOUBLE, "BACKSCAL", &backscal, 
		  "Background scale factor", status);
  fits_update_key(outfile, TDOUBLE, "CORRSCAL", &corrscal, 
		  "Correction scale factor", status);
  fits_update_key(outfile, TSTRING, "BACKFILE", "none", 
		  "Background FITS file", status);
  fits_update_key(outfile, TSTRING, "CORRFILE", "none", 
		  "Correction FITS file", status);
  fits_update_key(outfile, TSTRING, "RESPFILE", "none", 
		  "Redistribution Matrix file (RMF)", status);
  fits_update_key(outfile, TSTRING, "ANCRFILE", "none", 
		  "Effective Area file (ARF)", status);
  fits_update_key(outfile, TSTRING, "XFLT0001", "none", 
		  "XSPEC selection filter description", status);

  fits_update_key(outfile, TINT, "QUALITY", &quality,
		  "Data quality flag", status);
  fits_update_key(outfile, TINT, "GROUPING", &grouping, 
		  "Spectra are not grouped", status);
  fits_update_key(outfile, TLOGICAL, "POISSERR", &poiserr,
		  "Poisson errors do not apply", status);
  fits_update_key(outfile, TDOUBLE, "SYS_ERR", &sys_err,
		  "Systematic error value", status);

  fits_update_key(outfile, TINT, "DETCHANS", &(spect->nebins), 
		  "Total number of detector channels available", status);


  if (strcasecmp(chantype,"PHA") != 0) chantype = "PI";

  fits_update_key(outfile, TSTRING, "CHANTYPE", chantype,
		  "Pulse height channel type", status);

  return *status;
}

/* ----------------------------------------------------------------- */
/* Write the FITS type II spectrum file */
int writepha2(fitsfile *infile, fitsfile *outfile, struct parm_struct *parms, 
	    struct spectrum_struct *spect)
{
  int status = 0;
  int i;
  int ncols;
  char *colnames[] = { "TIME",    "TIME_STOP", "SPEC_NUM",   "EXPOSURE", 
		       "TELAPSE", "CHANNEL",   "RATE",       "STAT_ERR",
		       "TOTCOUNTS"};
  char *colforms[] = { "D",       "D",         "J",          "D",
		       "D",       "I",         "D",          "D", 
		       "J"};
  char *colunits[] = { "s",       "s",         "",           "s", 
		       "s",       "",          "count/s",    "count/s", 
		       "count"};
  char *comments[] = { "Spectrum start time",
		       "Spectrum stop time",
		       "Spectrum number",
		       "Spectrum exposure",
		       "Spectrum TSTOP-TSTART",
		       "Spectrum channel number",
		       "Spectrum rate",
		       "Spectrum error",
		       "Spectrum total counts" };
  char *extname    = "SPECTRUM";
  char tform_rate[10] = "";
  char tform_chan[10] = "";
  int *channel = 0;
  double *work;
  int tlmin = 0, tlmax = 0;
  double nulval = NULL_COUNTS;

  if (spect->nsplits != 1) {
    fprintf(stderr, "INTERNAL ERROR: writepha can only write one split\n");
    return -1;
  }

  work = (double *) malloc(sizeof(double)*spect->ntbins);
  if (work == 0) return MEMORY_ALLOCATION;

  /* 
     Spectrum:
     Time
     Time_STOP
     SPEC_NUM
     EXPOSURE
     CHANNEL
     RATE
     STAT_ERR

     keywords: TSTART, TSTOP, QUALITY=0, GROUPING=0, POISERR=F, SYS_ERR=0, 
               TLMIN=0, TLMAX=NE-1, CHANTYPE=PHA/PI
               EXTNAME=SPECTRUM, HDUCLASS=OGIP, HDUCLAS1=SPECTRUM, 
	       HDUCLAS2=TOTAL/NET/BKG, HDUCLAS3=TYPE:II, HDUCLAS4=COUNT/RATE
	       HDUVERS=1.2.0, PHAVERSN=1992a, AREASCAL=1, CORRSCAL=0, 
	       BACKSCAL=1, BACKFILE=none, CORRFILE=none, RESPFILE=none,
	       ANCRFILE=none, FILTER=none, XLT0001=none, DETCHANS=NE

               FILTER taken from input file
  */
  
  /* Unweighted data are not background subtracted */
  ncols = sizeof(colnames)/sizeof(colnames[0]);
  sprintf(tform_chan, "%dI", spect->nebins);
  sprintf(tform_rate, "%dD", spect->nebins);
  colforms[5] = tform_chan;
  colforms[6] = tform_rate;
  colforms[7] = tform_rate;

  /* Handle case of counts, not rate */
  if (parms->rate == 0) {
    colnames[6] = "COUNTS";
    colunits[6] = "count";
    comments[6] = "Spectrum counts";
    colunits[7] = "count";
  }

  status = prepfile(infile, outfile, ncols, spect->ntbins,  
		    colnames, colforms, colunits, comments, extname, 0,
		    parms->maskwt);

  write_app_keys(outfile, parms, &status);
  if (status != 0) {
    fprintf(stderr, "ERROR: The output file could not be created\n");
    free(work);
    return status;
  }

  writephahead(infile, outfile, parms, spect, 2,  &status);

  tlmax = spect->nebins-1;
  fits_update_key(outfile, TINT, "TLMIN6", &tlmin,
		  "Minimum legal value", &status);
  fits_update_key(outfile, TINT, "TLMAX6", &tlmax,
		  "Maximum legal value", &status);


  if (status) {
    fprintf(stderr, "ERROR: Could write keywords to output table\n");
    free(work);
    return status;
  }    

  {
    int safestatus = 0;
    fits_write_errmark();
    fits_delete_key(outfile, "EXPOSURE", &safestatus); safestatus = 0;
    fits_delete_key(outfile, "TELAPSE", &safestatus); safestatus = 0;
    fits_delete_key(outfile, "TOTCOUNT", &safestatus); safestatus = 0;
    fits_delete_key(outfile, "TOTCOUNTS", &safestatus); safestatus = 0;
    fits_delete_key(outfile, "SPEC_NUM", &safestatus); safestatus = 0;
    fits_delete_key(outfile, "CHANNEL", &safestatus); safestatus = 0;
    fits_delete_key(outfile, "LIVETIME", &safestatus); safestatus = 0;
    fits_clear_errmark();
  }


  fits_write_col(outfile, TDOUBLE, 1, 1, 1, spect->ntbins, 
		 spect->times, &status);     /* TIME */
  fits_write_col(outfile, TDOUBLE, 2, 1, 1, spect->ntbins, 
		 spect->tends, &status);     /* TIME_STOP */
  fits_write_col(outfile, TDOUBLE, 4, 1, 1, spect->ntbins, 
		 spect->exposures, &status); /* EXPOSURE */
  for (i=0; i<spect->ntbins; i++) {
    work[i] = (spect->tends[i]-spect->times[i]);
  }
  fits_write_col(outfile, TDOUBLE, 5, 1, 1, spect->ntbins, 
		 work, &status);             /* TELAPSE */
  fits_write_colnull(outfile, TDOUBLE, 7, 1, 1, spect->ntbins*spect->nebins, 
		     spect->weights, &nulval, &status);   /* RATE */
  if (spect->accum_wt2 && spect->weights2) {
    fits_write_colnull(outfile, TDOUBLE, 8, 1, 1, spect->ntbins*spect->nebins, 
		       spect->weights2, &nulval, &status);  /* ERROR */
  }
  fits_write_col(outfile, TLONG, 9, 1, 1, spect->ntbins, 
		 spect->counts, &status);    /* TOTCOUNTS */
  for (i=0; i<spect->ntbins; i++) {  /* Compute SPEC_NUM */
    work[i] = i+1;
  }
  fits_write_col(outfile, TDOUBLE, 3, 1, 1, spect->ntbins, 
		 work, &status);    /* SPEC_NUM */

  channel = (int *) malloc(sizeof(int)*spect->nebins);
  if (channel) {
    for(i=0; i<spect->nebins; i++) channel[i] = i;

    for(i=1; i<=spect->ntbins; i++) {
      fits_write_col(outfile, TINT, 6, i, 1, spect->nebins, 
		     channel, &status);      /* CHANNEL */
    }
    free(channel);
  }

  free(work);
  status = HDpar_stamp(outfile, 0, &status);

  if (status == 0)
    headas_chat(1, "  Type II Spectrum written to %s\n", parms->outfile); 
  else
    fprintf(stderr, "ERROR: could not write data to %s\n", parms->outfile);

  fits_set_hdustruc(outfile, &status);
  return status;
}


/* ----------------------------------------------------------------- */
/* Write the FITS type I spectrum file */
int writepha1(fitsfile *infile, fitsfile *outfile, struct parm_struct *parms, 
	      struct spectrum_struct *spect)
{
  int status = 0;
  int i;
  int ncols;
  char *colnames[] = { "CHANNEL", "RATE",      "STAT_ERR"};
  char *colforms[] = { "I",       "D",         "D" };
  char *colunits[] = { "",        "count/s",   "count/s" };
  char *comments[] = { "Spectrum channel number",
		       "Spectrum rate",
		       "Spectrum statistical error" };
  char *extname    = "SPECTRUM";
  int *channel = 0;
  int tlmin = 0, tlmax = 0;
  double nulval = NULL_COUNTS;

  if (spect->ntbins > 1) {
    fprintf(stderr, "ERROR: Type I spectral file cannot handle more than one output spectrum\n");
    return -1;
  }

  /* Handle case of counts, not rate */
  if (parms->rate == 0) {
    colnames[1] = "COUNTS";
    colunits[1] = "count";
    comments[1] = "Spectrum counts";
    colunits[2] = "count";
  }

  /* Unweighted data are not background subtracted */
  ncols = sizeof(colnames)/sizeof(colnames[0]);

  status = prepfile(infile, outfile, ncols, spect->ntbins,  
		    colnames, colforms, colunits, comments, extname, 0,
		    parms->maskwt);

  write_app_keys(outfile, parms, &status);
  if (status != 0) {
    fprintf(stderr, "ERROR: The output file could not be created\n");
    return status;
  }

  writephahead(infile, outfile, parms, spect, 1,  &status);


  tlmax = spect->nebins-1;
  fits_update_key(outfile, TINT, "TLMIN1", &tlmin,
		  "Minimum legal value", &status);
  fits_update_key(outfile, TINT, "TLMAX1", &tlmax,
		  "Maximum legal value", &status);

  fits_update_key(outfile, TLONG, "TOTCOUNT", &(spect->counts),
		  "Total number of counts", &status);

  if (status) {
    fprintf(stderr, "ERROR: Could write keywords to output table\n");
    return status;
  }    

  {
    int safestatus = 0;
    fits_write_errmark();
    fits_delete_key(outfile, "CHANNEL", &safestatus); safestatus = 0;
    fits_delete_key(outfile, "SPEC_NUM", &safestatus); safestatus = 0;
    fits_clear_errmark();
  }


  channel = (int *) malloc(sizeof(int)*spect->nebins);
  if (channel == 0) { return MEMORY_ALLOCATION; }
  for(i=0; i<spect->nebins; i++) channel[i] = i;

  fits_write_col(outfile, TINT, 1, 1, 1, spect->nebins, 
		 channel, &status);      /* CHANNEL */
  free(channel);

  fits_write_colnull(outfile, TDOUBLE, 2, 1, 1, spect->nebins, 
		     spect->weights, &nulval, &status);   /* RATE */
  if (spect->accum_wt2 && spect->weights2) {
    fits_write_colnull(outfile, TDOUBLE, 3, 1, 1, spect->nebins, 
		       spect->weights2, &nulval, &status);  /* ERROR */
  }

  status = HDpar_stamp(outfile, 0, &status);

  if (status == 0)
    headas_chat(1, "  Type I Spectrum written to %s\n", parms->outfile); 
  else
    fprintf(stderr, "ERROR: could not write data to %s\n", parms->outfile);

  fits_set_hdustruc(outfile, &status);
  return status;
}


/* ----------------------------------------------------------------- */
/* Transpose the array so that the NSPLITS dimension is first */
int dph_transpose(double *dph, int nebins, int nsplits, int ntbins)
{
  int i, j, k;
  double *p1;
  double *temp;

  temp = (double *) malloc(sizeof(double)*nebins*nsplits);
  if (temp == 0) return MEMORY_ALLOCATION;

  headas_chat(5,"  ...dph_transpose(dph,%d,%d,%d)...\n",
	      nebins, nsplits, ntbins);
  for (k = 0; k<ntbins; k++) {
    p1 = dph + nsplits*nebins*k;

    /* Transpose one time sample to temp[] array */
    for (j=0; j<nsplits; j++) {
      for (i=0; i<nebins; i++) {
	temp[j+i*nsplits] = p1[i+j*nebins];
      }
    }

    /* Transfer temp array back to real array */
    for (j=0; j<(nsplits*nebins); j++) {
      p1[j] = temp[j];
    }
  }

  free(temp);
  return 0;
}  

/* ----------------------------------------------------------------- */
/* Write the FITS DPH */
int writedph(fitsfile *infile, fitsfile *outfile, struct parm_struct *parms, 
	     struct spectrum_struct *spect,
	     int nebins, float *emin, float *emax)
{
  int status = 0;
  int ncols;
  char *colnames[] = { "TIME",   "TIME_STOP",  "EXPOSURE", 
		       "DPH_COUNTS", "TOTCOUNTS", "E_MIN", "E_MAX", "DPH_LEVEL"};
  char *colforms[] = { "D",      "D",         "D",
		       "E",          "J",         "D",     "D",     "J"};
  char *colunits[] = { "s",      "s",         "s",
		       "count",    "count",     "keV",   "keV",   ""};
  char *comments[] = { "DPH start time",
		       "DPH stop time",
		       "DPH exposure",
		       "DPH counts",
		       "Total counts",
		       "Lower energy bin edge",
		       "Upper energy bin edge",
		       "DPH slice number" };
  char *extname    = "BAT_DPH";
  double ontime, telapse, timepixr = 0, deadc = 1.0;
  char dph_tform[FLEN_CARD];
  char creator[FLEN_CARD];
  char datamode[FLEN_CARD];
  int i, j;
  long int dph_tdim[3];
  long int dphndim, nrows;
  int backapp = 1;
  int nperrow;
  double one = 1.0, zero = 0.0;
  double nulval = NULL_COUNTS;

  ncols = sizeof(colnames) / sizeof(colnames[0]);
  /* No need to write energy columns for DPH */
  if (parms->outtype != DPITAB) ncols -= 3;

  backapp = (parms->weighted)?1:0;
  /* Fill in the right name, dimensions and units for the DPH column */

  if (parms->outtype == DPH) {
    /* DPH: all energy slices in one row */
    dphndim = 3;
    dph_tdim[0] = spect->nebins;
    dph_tdim[1] = parms->nimgx;
    dph_tdim[2] = parms->nimgy;
    nrows = spect->ntbins;
    nperrow = spect->nbins;
  } else {
    /* DPITAB: one energy slice per row */
    dphndim = 2;
    dph_tdim[0] = parms->nimgx;
    dph_tdim[1] = parms->nimgy;
    nrows = spect->ntbins*spect->nebins;
    nperrow = parms->nimgx*parms->nimgy;
  }

  sprintf(dph_tform, "%dE", nperrow);
  colforms[3] = dph_tform;
  if (parms->dphcolumn[0]) colnames[3] = parms->dphcolumn;
  if (parms->rate == 1)    colunits[3] = "count/s";
    
  status = prepfile(infile, outfile, ncols, nrows,
		    colnames, colforms, colunits, comments, extname, 0,
		    parms->maskwt);
  sprintf(creator, "%s %s", parms->taskname, parms->taskver);
  fits_update_key(outfile, TSTRING, "CREATOR", creator,
		  "Program that created this FITS file", &status);
  fits_write_tdim(outfile, 4, dphndim, dph_tdim, &status);
  write_app_keys(outfile, parms, &status);
  fits_set_hdustruc(outfile, &status);
  
  if (status != 0) {
    fprintf(stderr, "ERROR: The output header could not be created\n");
    return status;
  }

  ontime = 0;
  for (i=0; i<spect->ntbins; i++) {
    ontime += spect->exposures[i];
  }
  telapse = spect->tstop - spect->tstart;

  fits_update_key(outfile, TDOUBLE, "TSTART", &spect->tstart, 0, &status);
  fits_update_key(outfile, TDOUBLE, "TSTOP", &spect->tstop, 0, &status);
  fits_update_key(outfile, TLOGICAL,"BACKAPP", &backapp, 
		  "Was background correction applied?", &status);
  
  fits_update_key(outfile, TDOUBLE, "TIMEPIXR", &timepixr, 
		  "Time bin alignment", &status);
  fits_update_key(outfile, TDOUBLE, "ONTIME", &ontime,
		  "[s] Accumulated on-time", &status);
  fits_update_key(outfile, TDOUBLE, "LIVETIME", &ontime,
		  "[s] ONTIME multiplied by DEADC", &status);
  fits_update_key(outfile, TDOUBLE, "DEADC", &deadc,
		  "Dead time correction factor", &status);
  fits_update_key(outfile, TDOUBLE, "EXPOSURE", &ontime,
		  "[s] Accumulated on-time", &status);
  fits_update_key(outfile, TDOUBLE, "TELAPSE", &telapse,
		  "[s] TSTOP - TSTART", &status);
  fits_update_key(outfile, TSTRING, "HDUCLASS", "OGIP", 
		  "Conforms to OGIP/GSFC standards", &status);
  fits_update_key(outfile, TSTRING, "HDUCLAS1", "ARRAY",
		  "Contains array data", &status);
  if (parms->weighted) {
    fits_update_key(outfile, TSTRING, "HDUCLAS2", "NET", 
		    "Histogram is weighted", &status);
  } else {
    fits_update_key(outfile, TSTRING, "HDUCLAS2", "TOTAL",
		    "Histogram is unweighted", &status);
  }
  fits_update_key(outfile, TSTRING, "FLUXMETH", 
		  (parms->weighted)?("WEIGHTED"):("RAW"),
		  "Flux extraction method", &status);



  /* Remove extraneous keywords */
  fits_write_errmark();
  fits_delete_key(outfile, "TOTCOUNT", &status); status = 0;
  fits_delete_key(outfile, "TOTCOUNTS", &status); status = 0;
  fits_delete_key(outfile, "CHANNEL", &status); status = 0;
  fits_delete_key(outfile, "SPEC_NUM", &status); status = 0;
  fits_clear_errmark();

  if (parms->outtype == DPH) {
    /* DPH: just write out the columns normally */
    sprintf(datamode, "Survey_%d", spect->nebins);
    fits_update_key(outfile, TSTRING, "DATAMODE", datamode,
		    "Instrument data mode", &status);
    fits_write_col(outfile, TDOUBLE, 1, 1, 1, spect->ntbins, 
		   spect->times, &status);                   /* TIME */
    fits_write_col(outfile, TDOUBLE, 2, 1, 1, spect->ntbins, 
		   spect->tends, &status);                   /* TIME_STOP */
    fits_write_col(outfile, TDOUBLE, 3, 1, 1, spect->ntbins, 
		   spect->exposures, &status);               /* EXPOSURE */
    fits_write_col(outfile, TLONG, 5, 1, 1, spect->ntbins,
		   spect->counts, &status);                  /* TOTCOUNTS */
    fits_write_colnull(outfile, TDOUBLE, 4, 1, 1, spect->ntbins*spect->nbins,
		   spect->weights, &nulval, &status);        /* DPH_COUNTS */
  } else {

    /* DPITAB: write out the rows multiply, since there may be more
       than one energy slice */
    sprintf(datamode, "DPITAB_%d", spect->nebins);
    fits_update_key(outfile, TSTRING, "DATAMODE", datamode,
		    "Instrument data mode", &status);

    /* Reference pixels and values */
    fits_update_key(outfile, TDOUBLE, "1CRPX4", &one, 
		    "Reference pixel (axis 1)", &status);
    fits_update_key(outfile, TDOUBLE, "1CRVL4", &zero, 
		    "Reference value (axis 1)", &status);
    fits_update_key(outfile, TDOUBLE, "2CRPX4", &one, 
		    "Reference pixel (axis 2)", &status);
    fits_update_key(outfile, TDOUBLE, "2CRVL4", &zero, 
		    "Reference value (axis 2)", &status);

    for (i=0; i<spect->ntbins; i++) {
      for (j=0; j<spect->nebins; j++) {
	int levelnum;
	fits_write_col(outfile, TDOUBLE, 1, i*spect->nebins+j+1, 1, 1,
		       &(spect->times[i]), &status);           /* TIME */
	fits_write_col(outfile, TDOUBLE, 2, i*spect->nebins+j+1, 1, 1,
		       &(spect->tends[i]), &status);           /* TIME_STOP */
	fits_write_col(outfile, TDOUBLE, 3, i*spect->nebins+j+1, 1, 1,
		       &(spect->exposures[i]), &status);       /* EXPOSURE */
	fits_write_col(outfile, TLONG, 5, i*spect->nebins+j+1, 1, 1,
		       &(spect->counts[i]), &status);          /* TOTCOUNTS */
	fits_write_col(outfile, TFLOAT, 6, i*spect->nebins+j+1, 1, 1,
		       &(emin[j]), &status);                   /* E_MIN */
	fits_write_col(outfile, TFLOAT, 7, i*spect->nebins+j+1, 1, 1,
		       &(emax[j]), &status);                   /* E_MAX */
	levelnum = j+1;
	fits_write_col(outfile, TINT, 8, i*spect->nebins+j+1, 1, 1,
		       &levelnum, &status);                    /* DPH_LEVEL */
	
      }
    }

    /* Transpose data to align with DPI table layout */
    if (status == 0) 
      status = dph_transpose(spect->weights, 
			     spect->nebins, spect->nsplits, spect->ntbins);
    fits_write_colnull(outfile, TDOUBLE, 4, 1, 1, spect->ntbins*spect->nbins,
		   spect->weights, &nulval, &status);    /* DPH_COUNTS */
  }


  status = HDpar_stamp(outfile, 0, &status);
  headas_chat(1, "  %s written to %s\n", 
	      (parms->outtype==DPH)?"DPH":"DPITAB", 
	      parms->outfile);

  fits_set_hdustruc(outfile, &status);
  return status;
}

/* ----------------------------------------------------------------- */
/* Write the FITS DPI */
int writedpi(fitsfile *infile, fitsfile *outfile, struct parm_struct *parms, 
	     struct spectrum_struct *spect,
	     int nebins, float *emin, float *emax)
{
  int status = 0;
  long int dph_tdim[3];
  long int dphndim, next;
  long int fpixel[2];
  int backapp, levelnum;
  int i, k, ie, it;
  int nkeys = 0;
  char card[FLEN_CARD];
  char creator[FLEN_CARD];
  char datamode[FLEN_CARD];
  char bunit[FLEN_CARD] = "count";
  double telapse, *ptr;
  double one = 1.0, zero = 0.0, deadc = 1.0;
  double nulval = NULL_COUNTS;

  backapp = (parms->weighted)?1:0;
  if (parms->rate) {
    strcpy(bunit, "count/s");
  }

  dphndim = 2;
  dph_tdim[0] = parms->nimgx;
  dph_tdim[1] = parms->nimgy;
  next = spect->ntbins*spect->nebins;

  /* Transpose data to align with DPI table layout */
  status = dph_transpose(spect->weights, 
			 spect->nebins, spect->nsplits, spect->ntbins);
  if (status) {
    fprintf(stderr, "ERROR: could not transpose data\n");
    return status;
  }

  /* Determine number of input file keywords */
  fits_get_hdrspace(infile, &nkeys, NULL, &status);
  sprintf(creator, "%s %s", parms->taskname, parms->taskver);

  /* Loop through each extension, which is one slice of a DPH */
  for(k=0; k<next; k++) {
    it = k / nebins;
    ie = k % nebins;

    fits_create_img(outfile, FLOAT_IMG, dphndim, dph_tdim, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not create image extension %d\n",k+1);
      return status;
    }

    /* Copy all the user keywords (not the structural keywords) */
    for (i = 1; i <= nkeys; i++) {
      fits_read_record(infile, i, card, &status);
      /* headas_chat(5,"  %4d/%4d %s\n", fits_get_keyclass(card),TYP_REFSYS_KEY,card); */
      /* Copy HISTORY keywords and any user keywords, but not COMMENTs */
      if (! (fits_get_keyclass(card) >= TYP_REFSYS_KEY) ) continue;
      if (strncmp(card, "COMMENT ", 7) == 0) continue;
      if (is_wcs_keyword(card)) continue;
      if (is_dm_keyword(card)) continue;

      fits_write_record(outfile, card, &status);
    }

    /* FITS standard used to forbid EXTNAME in the primary HDU but now
       allows it (version 3.0; released 2008) */
    {
      char extname[FLEN_CARD] = "BAT_DPI";
      if (k > 0) { sprintf(extname, "BAT_DPI_%d", k+1); }
      fits_update_key(outfile, TSTRING, "EXTNAME", extname,
		      "BAT Detector Plane Image", &status);
    }
    fits_update_key(outfile, TSTRING, "CREATOR", creator,
		    "Program that created this FITS file", &status);
    fits_update_key(outfile, TSTRING, "CTYPE1", parms->detxcolumn,
		    "Name of first axis", &status);
    fits_update_key(outfile, TSTRING, "CTYPE2", parms->detycolumn,
		    "Name of second axis", &status);
    sprintf(datamode, "DPI_%d", spect->nebins);
    fits_update_key(outfile, TSTRING, "DATAMODE", datamode,
		    "Instrument data mode", &status);
    fits_update_key(outfile, TSTRING, "BUNIT", bunit, 
		    "physical unit of image", &status);

    fits_update_key(outfile, TDOUBLE, "TSTART", &spect->times[it], 0, &status);
    fits_update_key(outfile, TDOUBLE, "TSTOP",  &spect->tends[it], 0, &status);
    fits_update_key(outfile, TLOGICAL,"BACKAPP", &backapp, 
		    "Was background correction applied?", &status);
    fits_update_key(outfile, TDOUBLE, "ONTIME", &spect->exposures[it],
		    "[s] Accumulated on-time", &status);
    fits_update_key(outfile, TDOUBLE, "LIVETIME", &spect->exposures[it],
		    "[s] ONTIME multiplied by DEADC", &status);
    fits_update_key(outfile, TDOUBLE, "DEADC", &deadc,
		    "Dead time correction factor", &status);
    fits_update_key(outfile, TDOUBLE, "EXPOSURE", &spect->exposures[it],
		    "[s] Accumulated on-time", &status);
    telapse = spect->tends[it]-spect->times[it];
    fits_update_key(outfile, TDOUBLE, "TELAPSE", &telapse, 
		    "[s] TSTOP - TSTART", &status);
    fits_update_key(outfile, TSTRING, "HDUCLASS", "OGIP", 
		    "Conforms to OGIP/GSFC standards", &status);
    fits_update_key(outfile, TSTRING, "HDUCLAS1", "ARRAY",
		    "Contains array data", &status);
    if (parms->weighted) {
      fits_update_key(outfile, TSTRING, "HDUCLAS2", "NET", 
		      "Histogram is weighted", &status);
    } else {
      fits_update_key(outfile, TSTRING, "HDUCLAS2", "TOTAL",
		      "Histogram is unweighted", &status);
    }
    fits_update_key(outfile, TSTRING, "FLUXMETH", 
		    (parms->weighted)?("WEIGHTED"):("RAW"),
		    "Flux extraction method", &status);
    fits_update_key(outfile, TLONG, "TOTCOUNT", &spect->counts[it],
		    "Total counts per time sample", &status);
    fits_update_key(outfile, TFLOAT, "E_MIN", &emin[ie],
		    "[keV] Lower energy bin edge", &status);
    fits_update_key(outfile, TFLOAT, "E_MAX", &emax[ie],
		    "[keV] Upper energy bin edge", &status);
    levelnum = ie+1;
    fits_update_key(outfile, TINT, "DPHLEVEL", &levelnum,
		    "DPH level number", &status);
    /* Reference pixels and values */
    fits_update_key(outfile, TDOUBLE, "CRPIX1", &one,  "Reference pixel", &status);
    fits_update_key(outfile, TDOUBLE, "CRVAL1", &zero, "Reference value", &status);
    fits_update_key(outfile, TDOUBLE, "CRPIX2", &one,  "Reference pixel", &status);
    fits_update_key(outfile, TDOUBLE, "CRVAL2", &zero, "Reference value", &status);

    if (status) {
      fprintf(stderr, "ERROR: Could not write keywords to output image %d\n",k+1);
      return status;
    }    

    ptr = spect->weights + k*spect->nsplits;
    fpixel[0] = 1;
    fpixel[1] = 1;
    fits_write_pixnull(outfile, TDOUBLE, fpixel, spect->nsplits, ptr, &nulval, &status);
    
    if (status) {
      fprintf(stderr, "ERROR: Could not write data to output image %d\n",k+1);
      return status;
    }    

    status = HDpar_stamp(outfile, 0, &status);

  }


  headas_chat(1, "  DPIs written to %s\n", 
	      parms->outfile);

  fits_set_hdustruc(outfile, &status);
  return status;
}

int writeebounds(fitsfile *infile, fitsfile *outfile, 
		 struct parm_struct *parms, 
		 struct spectrum_struct *spect, 
		 int nebins, float *emin, float *emax)
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

  ncols = sizeof(colnames)/sizeof(colnames[0]);
  /* NOTE: append, not create */
  status = prepfile(infile, outfile, ncols, nebins, 
		    colnames, colforms, colunits, comments, extname, 1, 
		    parms->maskwt);
  sprintf(creator, "%s %s", parms->taskname, parms->taskver);
  fits_update_key(outfile, TSTRING, "CREATOR", creator,
		  "Program that created this FITS file", &status);
  write_app_keys(outfile, parms, &status);
  
  if (status != 0) {
    fprintf(stderr, "ERROR: The output file could not be appended to\n");
    return status;
  }

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

  fits_update_key(outfile, TDOUBLE, "TSTART", &spect->tstart, 0, &status);
  fits_update_key(outfile, TDOUBLE, "TSTOP", &spect->tstop, 0, &status);
  fits_update_key(outfile, TINT, "DETCHANS", &nebins,
		  "Total number of detector channels available", &status);
  {
    char *chantype = parms->coltype;
    if (strcasecmp(chantype,"PHA") != 0) chantype = "PI";

    fits_update_key(outfile, TSTRING, "CHANTYPE", chantype, 
		    "Pulse height channel type", &status);
  }
  if (parms->outtype == PHA || parms->outtype == PHA1 || parms->outtype == PHA2) {
    fits_update_key(outfile, TSTRING, "PHAFILE", parms->outfile,
		   "Spectrum that this EBOUNDS extension applies to", &status);
  }

  if (status) {
    fprintf(stderr, "ERROR: Could not write keywords to output table\n");
    return status;
  }    

  for(i=0; i<nebins; i++) {
    fits_write_col(outfile, TINT,   1, i+1, 1, 1, &i,         &status);
    fits_write_col(outfile, TFLOAT, 2, i+1, 1, 1, &(emin[i]), &status);
    fits_write_col(outfile, TFLOAT, 3, i+1, 1, 1, &(emax[i]), &status);
  }

  status = HDpar_stamp(outfile, 0, &status);

  if (status == 0)
    headas_chat(1, "  EBOUNDS written to %s\n", parms->outfile); 
  else
    fprintf(stderr, "ERROR: could not write data to %s\n", parms->outfile);

  /* Delete any extraneous keywords */
  fits_write_errmark();
  fits_delete_key(outfile, "EXPOSURE", &status); status = 0;
  fits_delete_key(outfile, "TELAPSE", &status); status = 0;
  fits_delete_key(outfile, "ONTIME", &status); status = 0;
  fits_delete_key(outfile, "LIVETIME", &status); status = 0;
  fits_clear_errmark();

  fits_set_hdustruc(outfile, &status);
  return status;
}

int writegti(fitsfile *infile, fitsfile *outfile, 
	     struct parm_struct *parms)
{
  int status = 0;
  char creator[FLEN_CARD];

  int i;
  int nkeys = 0;
  char card[FLEN_CARD];
  char keyname[FLEN_CARD], keyvalue[FLEN_CARD];
  double exposure = 0.0;

  HDgti_write(outfile, &(parms->gtimaster), 0, 0, 0, &status);
  for (i=0; i<parms->gtimaster.ngti; i++) {
    exposure += (parms->gtimaster.stop[i] - parms->gtimaster.start[i]);
  }

  /* Copy all the user keywords (not the structural keywords) */
  fits_get_hdrspace(infile, &nkeys, NULL, &status);

  for (i = 1; i <= nkeys; i++) {
    fits_read_keyn(infile, i, keyname, keyvalue, 0, &status);
    fits_read_record(infile, i, card, &status);
    /* Copy HISTORY keywords and any user keywords, but not COMMENTs */
    if (! (fits_get_keyclass(keyname) >= TYP_REFSYS_KEY) ) continue;
    if ( strncmp(keyname, "COMMENT", 7) == 0) continue;
    if ( strncmp(keyname, "HDU", 3) == 0) continue;
    if ( strncmp(keyname, "TIMEZERO", 8) == 0) continue;
    if ( strncmp(keyname, "MJDREF", 6) == 0) continue;
    if ( strncmp(keyname, "TSTART", 6) == 0) continue;
    if ( strncmp(keyname, "TSTOP", 5) == 0) continue;
    if (is_wcs_keyword(keyname)) continue;
    if (is_dm_keyword(card)) continue;

    fits_update_card(outfile, keyname, card, &status);
  }

  sprintf(creator, "%s %s", parms->taskname, parms->taskver);
  fits_update_key(outfile, TSTRING, "CREATOR", creator,
		  "Program that created this FITS file", &status);

  fits_update_key(outfile, TDOUBLE, "EXPOSURE", &exposure, 
		  "[s] Total exposure, with known corrections", &status);

  fits_modify_comment(outfile, "TTYPE1", "GTI start time", &status);
  fits_modify_comment(outfile, "TTYPE2", "GTI stop  time", &status);

  if (status != 0) {
    fprintf(stderr, "ERROR: The output file could not be appended to\n");
    return status;
  }

  /* Delete any extraneous keywords */
  fits_write_errmark();
  fits_delete_key(outfile, "TELAPSE", &status); status = 0;
  fits_delete_key(outfile, "ONTIME", &status); status = 0;
  fits_delete_key(outfile, "LIVETIME", &status); status = 0;
  fits_clear_errmark();

  fits_set_hdustruc(outfile, &status);
  return status;
}

