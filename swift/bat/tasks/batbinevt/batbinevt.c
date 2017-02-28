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

#include "batbinevt.h"

static char taskname[] = "batbinevt";
static char taskver[]  = "1.48";

/*
  BIN mask weighted event data into a light curve or a spectrum.

  13 Dec 2002 - correct error when TIMEDEL is zero (should reset to
                TSTOP-TSTART)

  $Id: batbinevt.c,v 1.126 2010/05/21 23:43:12 craigm Exp $
  C. Markwardt 
*/


#define HDUCLAS2_DEFAULT 0
#define HDUCLAS2_PREDICTED 1
#define HDUCLAS2_RESIDUALS 2

#define TOOLSUB batbinevt
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"


/* ----------------------------------------------------------------- */
/* Get tool parameters from command line or parameter file */
int batbinevt_getpar(struct parm_struct *parms) 
{
  int status = 0;
  char method[PIL_LINESIZE];
  char outtype[PIL_LINESIZE];
  char tstart[PIL_LINESIZE];
  char tstop[PIL_LINESIZE];
  double default_ebinquant = 0.1;

  /* Default values */
  parms->intype = 0;
  parms->infile[0] = 0;
  parms->outfile[0] = 0;
  parms->ninfiles = 0;
  parms->detmask[0] = 0;
  parms->tbinmethod = UNIFORM;
  parms->tstart = T_INDEF;
  parms->tstop  = T_INDEF;
  parms->tbinsize = 0;
  parms->outtype = LC;
  parms->ftstart = 0;
  parms->ftstop  = 0;
  parms->ftmin = T_INDEF;
  parms->ftmax = T_INDEF;
  parms->minfracexp = 0.1;
  parms->weighted = 1;
  parms->maskwt[0] = 0;
  parms->unstr[0] = 0;

  parms->timecolumn[0] = 0;
  parms->dphcolumn[0] = 0;
  parms->detxcolumn[0] = 0;
  parms->detycolumn[0] = 0;
  parms->maskwtcolumn[0] = 0;

  parms->ebinquant = 0.0;
  parms->default_ebinquant = 1.0;

  parms->nappkeys = 0;  /* Initialize to no *APP correction keywords */

  HDgti_init(&parms->gtimaster);  /* Zero out the master data structure */
  strcpy(parms->coltype, "PI");

  /* Parameter values */
  if ((status = PILGetString("infile", parms->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");

  else if ((status = PILGetFname("outfile", parms->outfile)))
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");

  else if ((status = PILGetString("outtype", outtype)))
    fprintf(stderr, "Error reading the 'outtype' parameter.\n");

  else if ((status = PILGetReal("timedel", &parms->tbinsize)))
    fprintf(stderr, "Error reading the 'timedel' parameter.\n");

  else if ((status = PILGetString("timebinalg", method)))
    fprintf(stderr, "Error reading the 'timebinalg' parameter.\n");

  else if ((status = PILGetString("energybins", parms->ebins)))
    fprintf(stderr, "Error reading the 'timebinalg' parameter.\n");

  else if ((status = PILGetString("gtifile", parms->gtifile)))
    fprintf(stderr, "Error reading the 'gtifile' parameter.\n");

  else if ((status = PILGetString("ecol", parms->coltype)))
    fprintf(stderr, "Error reading the 'ecol' parameter.\n");

  else if ((status = PILGetString("tstart", tstart)))
    fprintf(stderr, "Error reading the 'tstart' parameter.\n");

  else if ((status = PILGetString("tstop", tstop)))
    fprintf(stderr, "Error reading the 'tstop' parameter.\n");
  
  else if ((status = PILGetReal("snrthresh", &parms->snr)))
    fprintf(stderr, "Error reading the 'snrthresh' parameter.\n");

  else if ((status = PILGetInt("buffersize", &parms->buffersize)))
    fprintf(stderr, "Error reading the 'buffersize' parameter.\n");

  else if ((status = PILGetString("detmask", parms->detmask)))
    fprintf(stderr, "Error reading the 'detmask' parameter.\n");

  else if ((status = PILGetReal("timepixr", &parms->timepixr)))
    fprintf(stderr, "Error reading the 'timepixr' parameter.\n");
  
  else if ((status = PILGetString("maskwt", parms->maskwt)))
    fprintf(stderr, "Error reading the 'maskwt' parameter.\n");

  else if ((status = PILGetString("weighted", parms->wtstr)))
    fprintf(stderr, "Error reading the 'weighted' parameter.\n");

  else if ((status = PILGetString("outunits", parms->unstr)))
    fprintf(stderr, "Error reading the 'outunits' parameter.\n");

  else if ((status = PILGetString("tcol", parms->timecolumn)))
    fprintf(stderr, "Error reading the 'tcol' parameter.\n");
  else if ((status = PILGetString("countscol", parms->dphcolumn)))
    fprintf(stderr, "Error reading the 'countscol' parameter.\n");
  else if ((status = PILGetString("xcol", parms->detxcolumn)))
    fprintf(stderr, "Error reading the 'xcol' parameter.\n");
  else if ((status = PILGetString("ycol", parms->detycolumn)))
    fprintf(stderr, "Error reading the 'ycol' parameter.\n");
  else if ((status = PILGetString("maskwtcol", parms->maskwtcolumn)))
    fprintf(stderr, "Error reading the 'maskwtcol' parameter.\n");
  else if ((status = PILGetReal("ebinquant", &default_ebinquant)))
    fprintf(stderr, "Error reading the 'ebinquant' parameter.\n");
  else if ((status = PILGetBool("delzeroes", &parms->delzeroes)))
    fprintf(stderr, "Error reading the 'delzeroes' parameter.\n");
  else if ((status = PILGetReal("minfracexp", &parms->minfracexp)))
    fprintf(stderr, "Error reading the 'minfracexp' parameter.\n");

  else if ((status = PILGetReal("min_dph_frac_overlap", &parms->minfracoverlap)))
    fprintf(stderr, "Error reading the 'min_dph_frac_overlap' parameter.\n");
  else if ((status = PILGetReal("min_dph_time_overlap", &parms->mintimeoverlap)))
    fprintf(stderr, "Error reading the 'mindphtimeoverlap' parameter.\n");
  else if ((status = PILGetReal("max_dph_time_nonoverlap", &parms->maxtimenonoverlap)))
    fprintf(stderr, "Error reading the 'max_dph_time_nonoverlap' parameter.\n");

  if (status) {
    return status;
  }

  parms->default_ebinquant = default_ebinquant;

  /* Parameter validation */
  if (parms->buffersize > 4*1024*1024) {
    parms->buffersize = 4*1024*1024;
  } else if (parms->buffersize < 4) {
    parms->buffersize = 4;
  }

  /* Parse the input list */
  parms->infiles = expand_item_list(parms->infile, &parms->ninfiles, ',', 
				    1, 1, 1, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not parse input file list\n");
    if (parms->infiles) free(parms->infiles);
    return status;
  }
  if (parms->ninfiles == 0) {
    fprintf(stderr, "ERROR: input file list was empty\n");
    if (parms->infiles) free(parms->infiles);
    return -1;
  }
  headas_chat(5,"    (%d input files)\n", parms->ninfiles);

  /* Parse the time binning algorithm */
  if (strncasecmp(method,"u",1) == 0) {
    parms->tbinmethod = UNIFORM;
  } else if (strncasecmp(method,"g",1) == 0) {
    parms->tbinmethod = USERBINS;
  } else if (strncasecmp(method,"s",1) == 0) {
    parms->tbinmethod = CONSTSNR;
  } else if (strncasecmp(method,"b",1) == 0) {
    parms->tbinmethod = BAYESIAN;
  } else if (strncasecmp(method,"i",1) == 0) {
    parms->tbinmethod = INFBINS;
  } else if (strncasecmp(method,"m",1) == 0) {
    parms->tbinmethod = MATCHBINS;
  } else if (strncasecmp(method,"h",1) == 0) {
    parms->tbinmethod = MINSNR;
  } else {
    fprintf(stderr, "ERROR: time binning algorithm must be one of:\n"
	    "      (u)niform, (g)ti, (s)nr, (h)ighsnr, (i)nfile or (m)atchlc\n");
    return -1;
  }

  /* Check for default value of gti file */
  if (strcasecmp(parms->gtifile,"none") == 0) {
    parms->gtifile[0] = 0;
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

  /* Check for default value of detmask file */
  if (strcasecmp(parms->detmask,"none") == 0) {
    parms->detmask[0] = 0;
  }
  /* Check for default value of maskwt file */
  if (strcasecmp(parms->maskwt,"none") == 0) {
    parms->maskwt[0] = 0;
  }



  /* Error checking */
  if (parms->tbinmethod == BAYESIAN) {
    fprintf(stderr, "ERROR: binning method %s not supported yet\n", method);
    return -1;
  }

  /* Parse the column name */
  if (strncasecmp(parms->coltype,"pi",2) == 0) {
    strcpy(parms->coltype, "PI");
  } else if (strncasecmp(parms->coltype,"pha",2) == 0) {
    strcpy(parms->coltype, "PHA");
  }

  /* Parse the output type */
  if (strncasecmp(outtype,"lc",2) == 0) {
    parms->outtype = LC;
    parms->rate = 1;
  } else if (strncasecmp(outtype,"pha1",4) == 0) {
    parms->outtype = PHA1;
    parms->rate = 1;
  } else if (strncasecmp(outtype,"pha2",4) == 0) {
    parms->outtype = PHA2;
    parms->rate = 1;
  } else if (strncasecmp(outtype,"pha",3) == 0) {
    parms->outtype = PHA;
    parms->rate = 1;
  } else if (strncasecmp(outtype,"dpitab",6) == 0) {
    parms->outtype = DPITAB;
    parms->rate = 0;
  } else if (strncasecmp(outtype,"dpi",3) == 0) {
    parms->outtype = DPI;
    parms->rate = 0;
  } else if (strncasecmp(outtype,"dph",3) == 0) {
    parms->outtype = DPH;
    parms->rate = 0;
  } else {
    fprintf(stderr, "ERROR: Output type must be one of: LC, PHA, PHA1, PHA2, DPI, DPH, DPITAB\n");
    return -1;
  }

  /* Check for output units.  Default has already been set. */
  if (strncasecmp(parms->unstr, "INDEF", 5) == 0) {
    /* Do nothing */
  } else if ((strcasecmp(parms->unstr, "RATE") == 0) ||
	     (strcasecmp(parms->unstr, "COUNT/S") == 0) || 
	     (strcasecmp(parms->unstr, "COUNTS/S") == 0)) {
    parms->rate = 1;
    if (type_is_image(parms->intype)) {
      headas_chat(1, "WARNING: Do you really want to request a RATE histogram?");
    }
  } else if ((strcasecmp(parms->unstr, "COUNT") == 0) || 
	     (strcasecmp(parms->unstr, "COUNTS") == 0)) {
    parms->rate = 0;

  } else {
    fprintf(stderr, "ERROR: unrecognized 'outunits' (=%s)\n", 
	    parms->unstr);
    return -1;
  }

  /* Default value for weighted */
  parms->weighted = -1;
  if ((strncasecmp(parms->wtstr,"yes",3) == 0) ||
      (strncasecmp(parms->wtstr,"y",1) == 0)) {
    parms->weighted = 1;
  }
  if ((strncasecmp(parms->wtstr,"no",2) == 0) ||
      (strncasecmp(parms->wtstr,"n",1) == 0)) {
    parms->weighted = 0;
  }
  if ((strncasecmp(parms->wtstr,"indef",5) == 0)) {
    parms->weighted = 2;
  }
  if (parms->weighted == -1) {
    fprintf(stderr, "ERROR: weighted must be one of Yes, No, or INDEF\n");
    return -1;
  }
  if (parms->weighted == 2) {
    /* Default value is based on whether the output product type is a
       "final" background subtracted product, or an intermediate
       product (detector histograms are considered intermediate). */
    switch (parms->outtype) {
    case LC:      parms->weighted = 1; break;
    case PHA:     parms->weighted = 1; break;
    case PHA1:    parms->weighted = 1; break;
    case PHA2:    parms->weighted = 1; break;
    case DPI:     parms->weighted = 0; break;
    case DPITAB:  parms->weighted = 0; break;
    case DPH:     parms->weighted = 0; break;
    }
  }

  /* Parse tstart... */
  if (strcasecmp(tstart, "indef") == 0) {
    parms->tstart = T_INDEF;
  } else {
    char *endptr = 0;
    parms->tstart = strtod(tstart, &endptr);
    if (endptr == tstart) {
      fprintf(stderr, "ERROR: tstart must be a number or INDEF\n");
      return -1;
    }
  }


  /* ...and tstop */
  if (strcasecmp(tstop, "indef") == 0) {
    parms->tstop = T_INDEF;
  } else {
    char *endptr = 0;
    parms->tstop = strtod(tstop, &endptr);
    if (endptr == tstop) {
      fprintf(stderr, "ERROR: tstop must be a number or INDEF\n");
      return -1;
    }
  }


  if (parms->ninfiles > 0) {
    headas_chat(5, "  ...allocating %d GTI structures...\n",
		parms->ninfiles);
    parms->gtis = (struct gti_struct *) malloc(sizeof(struct gti_struct) *
					       parms->ninfiles);
    parms->ftstart = (double *)malloc(sizeof(double)*parms->ninfiles);
    parms->ftstop  = (double *)malloc(sizeof(double)*parms->ninfiles);

    if ((parms->gtis == 0) || (parms->ftstart == 0) || (parms->ftstop == 0)) {
      fprintf(stderr, "ERROR: could not allocate GTI structures\n");
      if (parms->gtis) free(parms->gtis);
      if (parms->ftstart) free(parms->ftstart);
      if (parms->ftstop) free(parms->ftstop);

      status = MEMORY_ALLOCATION;
      return status;
    }
  }    

  if (parms->timepixr < 0 && parms->timepixr != (-1)) {
    fprintf(stderr, "ERROR: timepixr must be between 0 and 1, or equal to -1\n");
    return -1;
  }

  /* Default value of timepixr */
  if (parms->timepixr == -1) {
    if (parms->outtype == LC && parms->tbinmethod != UNIFORM) {
      /* Non-uniform light curve bins: bin-center */
      parms->timepixr = 0.5;
    } else {
      /* Uniform light curve bins: bin-start */
      parms->timepixr = 0.0;
    }
  }


  return status;
}


/* ----------------------------------------------------------------- */
/* Print opening banner for user */
void banner(struct parm_struct *parms)
{
  char *outtype = "";

  switch (parms->outtype) {
  case LC:     outtype = "  (LC)"; break;
  case PHA:    outtype = "  (PHA auto)"; break;
  case PHA1:   outtype = "  (PHA type I)"; break;
  case PHA2:   outtype = "  (PHA type II)"; break;
  case DPI:    outtype = "  (DPI)"; break;
  case DPITAB: outtype = "  (DPITAB)"; break;
  case DPH:    outtype = "  (DPH)"; break;
  }

  headas_chat(2, "******************************************\n");
  headas_chat(1, "         %s v%s\n", parms->taskname, parms->taskver);
  headas_chat(2, "------------------------------------------\n");
  headas_chat(2, "    Input Events: %s\n", parms->infile);
  headas_chat(2, "     Output File: %s%s\n",parms->outfile, outtype);
  if (parms->detmask[0])
    headas_chat(2, "   Detector Mask: %s\n", parms->detmask); 
  else
    headas_chat(2, "   Detector Mask: NONE\n");
  headas_chat(2, "      Time Range: %f to %f  (requested)\n",
	 parms->tstart, parms->tstop);
  headas_chat(2, "      Time Range: %f to %f  (actual)\n",
	 parms->tmin, parms->tmax);
  headas_chat(2, "Apply Weighting?: %s     Energy Column: %s\n", 
	      parms->weighted ? "YES" : "NO ", parms->coltype);
  if (parms->maskwt[0])
    headas_chat(2, " Mask Weight Map: %s\n", parms->maskwt);
  headas_chat(2, "     Energy Bins: %s\n", parms->ebins);
  headas_chat(2, "    Output Units: %s\n", 
	      parms->rate ? "RATE" : "COUNTS");
  switch(parms->tbinmethod) {
  case UNIFORM:
    headas_chat(2, "  Binning Method: u = UNIFORM\n");
    headas_chat(2, "   Time Bin Size: %f (s)\n", parms->tbinsize);
    break;
  case CONSTSNR:
  case MINSNR:
    headas_chat(2, "  Binning Method: %s = %s\n",
		(parms->tbinmethod == CONSTSNR) ? "s"        : "h",
		(parms->tbinmethod == CONSTSNR) ? "CONSTSNR" : "HIGHSNR");
    headas_chat(2, "    Signal/Noise: %f\n", parms->snr);
    if (parms->tbinsize > 0)
      headas_chat(2, "   Max. Time Bin: %f\n", parms->tbinsize);
    break;
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
/* Print feedback summary so user knows tool succeeded */
void summary(struct parm_struct *parms,
	     int nevt, double ngood, double nbad, int ntbins, int nebins) 
{
  headas_chat(1, "    Number of Rows Processed: %d\n", nevt);
  headas_chat(1, "    Number Accepted/Rejected: %g/%g\n", ngood, nbad);
  headas_chat(1, "  Time Bins: %10d      Energy Bins: %4d\n", 
	      ntbins, nebins);
  headas_chat(2, "------------------------------------------\n");
}

/* ----------------------------------------------------------------- */
/* Convert the accumulated weights into rates */
int weights2rates(struct spectrum_struct *spect,
		  struct parm_struct *parms)
{
  int i, j;
  int newtbins = 0;
  int status = 0;
  int *index = 0;
  int ind;
  int zeroflag = 0;
  int classtype = HDUCLAS2_DEFAULT;


  if (spect == 0) {
    return NULL_INPUT_PTR;
  }

  /* Get set to zero the error bars if this is a predicted flux, as
     opposed to a Poisson flux. */
  if (strncasecmp(parms->hduclas2, "PREDICTED", 9) == 0) {
    classtype = HDUCLAS2_PREDICTED;
  } else if (strncasecmp(parms->hduclas2, "RESIDUALS", 9) == 0) {
    classtype = HDUCLAS2_RESIDUALS;
  }

  /* Make sure end of last time bin is set right */
  if (parms->tbinmethod != UNIFORM && 
      parms->tbinmethod != MATCHBINS &&
      parms->tbinmethod != INFBINS &&
      parms->tbinmethod != USERBINS) {
    spect->tends[spect->ntbins-1] = spect->tstop;
  }

  if (classtype == HDUCLAS2_RESIDUALS && parms->weighted) { 
    fprintf(stderr, 
	    "WARNING: the input file had HDUCLAS2 = 'RESIDUALS', which means\n"
	    "         that the Poisson errors will probably be assigned\n"
	    "         incorrectly.  You should typically only maked weighted\n"
	    "         spectra from unsubtracted/uncleaned inputs.\n");
  }

  /* Compute exposure for each bin, and from there, the rate in each
     bin */
  for (i=0; i<spect->ntbins; i++) {
    status = 0;
    spect->exposures[i] = HDgti_exp(spect->times[i], spect->tends[i], 
				    &parms->gtimaster, &status);
    if (status) spect->exposures[i] = 0;
    
    /* headas_chat(5, "  (tstart=%f tstop=%f exposures[i] = %f  status=%d)\n", 
       spect->times[i], spect->tends[i], spect->exposures[i], status); */

    /* Compute standard deviation of number of counts => sqrt(weights^2) */
    if (spect->accum_wt2 && spect->weights2) for (j=0; j<spect->nbins; j++) {
      ind = i*spect->nbins + j;
      spect->weights2[ind] = sqrt(spect->weights2[ind]);
      if (classtype == HDUCLAS2_PREDICTED) {
	spect->weights2[ind] *= XSPEC_ERROR_FUDGE_FACTOR;
      }
    }

    if ((spect->exposures[i] > 0) && (parms->rate)) {
      for (j=0; j<spect->nbins; j++) {
	ind = i*spect->nbins + j;
	spect->weights[ind] /= spect->exposures[i];
      }
      /* If there are accumulated squared weights, divide by the
	 exposure. */
      if (spect->accum_wt2 && spect->weights2) {
	zeroflag = 1;
	for (j=0; j<spect->nbins; j++) {
	  ind = i*spect->nbins + j;
	  spect->weights2[ind] /= spect->exposures[i];
	
	  /* Check for non-zero weights */
	  if (spect->weights2[ind] != 0) { 
	    zeroflag = 0;
	  } else {
	    /* Special NULL value to indicate no error bar */
	    /* XXX remove for now
	       spect->weights2[ind] = -1; */
	  }
	}

	/* All weights were zero, ignore this bin */
	if (zeroflag == 1 && parms->delzeroes && classtype == HDUCLAS2_DEFAULT) { 
	  spect->exposures[i] = 0; 
	}

      } /* end rate error calc */
    }   /* end rate calc */

    /* Place nulls in bins with no exposure */
    if (spect->exposures[i] == 0) {
      for (j=0; j<spect->nbins; j++) {
	ind = i*spect->nbins + j;
	spect->weights[ind]  = NULL_COUNTS;
	if (spect->weights2) spect->weights2[ind] = NULL_COUNTS;
      }
    }

  }

  index = (int *) malloc(sizeof(int)*spect->ntbins);
  if (index == 0) return MEMORY_ALLOCATION;

  /* Scan through and locate those time bins which have non-zero
     exposure, record them in the index[] array */
  j = 0;
  for (i=0; i<spect->ntbins; i++) {
    double telapse = (spect->tends[i]-spect->times[i]);

    index[i] = -1;

    /* Allow only bins with large fractional exposure... */
    if ( (parms->minfracexp <= 0) ||
	 ((telapse > 0) && (spect->exposures[i]/telapse > parms->minfracexp)) ) {
      index[i] = j++;
    } else if (parms->tbinsize == 0) {
      /* ... *OR* if all the bins have been lumped into one */
      index[i] = j++;
    }
  }

  /* Now j contains the new number of time bins */
  newtbins = j;
  if (newtbins == 0) {
    headas_chat(1,"WARNING: all time bins have negligible exposure\n");
    spect->ntbins = 0;
    free(index);
    return 0;
  }

  /* Scan through the index list and compress the time bins so that the
     non-zero bins are removed */
  for (i=0; i<spect->ntbins; i++) {
    if ((index[i] != i) && (index[i] >= 0)) {
      int doff = index[i]*spect->nbins;
      int soff = i*spect->nbins;

      /* Copy the scalar quantities per bin */
      spect->times[index[i]] = spect->times[i];
      spect->tends[index[i]] = spect->tends[i];
      spect->exposures[index[i]] = spect->exposures[i];
      spect->counts[index[i]] = spect->counts[i];
      spect->totweights[index[i]] = spect->totweights[i];
      spect->totweights2[index[i]] = spect->totweights2[i];

      /* Copy the vector quantities per bin */
      for (j=0; j<spect->nbins; j++) {
	spect->weights[doff+j] = spect->weights[soff+j];
      }
      if (spect->accum_wt2 && spect->weights2) for (j=0; j<spect->nbins; j++) {
	spect->weights2[doff+j] = spect->weights2[soff+j];
      }
    }
  }
  spect->ntbins = newtbins;
  free(index);

  return 0;
}


/* ----------------------------------------------------------------- */
/* Do main work of tool */
int batbinevt_work(struct parm_struct *parms)
{
  char *infile, *outfilename;
  fitsfile *inevt = 0, *indph = 0, *outfile = 0;
  int status = 0;
  int remaining = 0, nread = 0;
  int buffersize;
  int ifile, i;
  int tbinmethod;
  double *tev = 0, *tend = 0;
  float *pi = 0, *weights = 0;
  int *ebin = 0, *segs = 0, *split = 0;
  float *ncounts = 0;
  int *detx = 0, *dety = 0;
  int *split1 = 0;
  int nevt = 0;
  struct spectrum_struct spect;
  int nebins = 0;                           /* Energy bins */
  float emin[4096], emax[4096];
  int ebintab[65536], nebintab = 65536;     /* Energy bin lookup table */
  long axesd[2] = {0, 0};
  int *detmask = 0;   /* Detector quality mask data */
  int nimgx = 286, nimgy = 173;             /* Size of image in DETX/Y */
  struct evtfile_struct evtfile;
  struct dphfile_struct dphfile;
  double *maskwt = 0;

  if (parms == 0) {
    return NULL_INPUT_PTR;
  }

  /* ------------------- */
  /* Some info about DPHs, in case they are written.  Really this
     stuff belongs in the parameter file. */
  parms->nimgx = nimgx;
  parms->nimgy = nimgy;

  /* Use some local storage */
  infile = parms->infiles[0];
  outfilename = parms->outfile;
  tbinmethod = parms->tbinmethod;

  /* ------------------- */
  /* Initialize the "spectrum" structure */

  spect.ntmax = spect.ntbins = spect.nebins = spect.nsplits = 0;
  init_spect(&spect);

  /* ------------------- */
  /* Read time limits and GTIs from the input file(s) */
  status = read_tlimits(parms);
  if (status) {
    fprintf(stderr, "ERROR: could not read event TSTART and TSTOP values\n");
    goto CLEANUP;
  }
  if (parms->gtimaster.ngti == 0) {
    fprintf(stderr,"======================================================\n");
    fprintf(stderr,"WARNING: no overlapping good time intervals were found\n");
    fprintf(stderr,"======================================================\n");
    goto CLEANUP;
  }
  if ((parms->intype != DPH) && (parms->intype != EVENTS)) {
    fprintf(stderr,"======================================================\n");
    fprintf(stderr, "ERROR: input data must be either EVENTS or BAT_DPH\n");
    fprintf(stderr,"======================================================\n");
    goto CLEANUP;
  }

  /* ------------------- */
  /* Read CALDB stuff, namely the energy bins */
  if (strncasecmp(parms->ebins,"CALDB",5) == 0) {
    struct caldbparms_struct caldb;
    char expr[80];
    char *codenam = "EBOUNDS";
    char *pfile = parms->ebins;
    char online[80], *ponline = online;
    long int extno[1];
    int maxret = 1;
    int nret = 0, nfound = 0;


    /* Read the CALDB-related keywords from the first input file */
    fits_open_data(&inevt, parms->infiles[0], READONLY, &status);
    batkw_to_caldb_parms(inevt, &caldb, 1, &status);
    fits_close_file(inevt, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not determine CALDB parameters from %s\n",
	      parms->infiles[0]);
      return status;
    }

    /* Attempt to read number of bins from CALDB expression, otherwise
       default to 4 bins for light curves and 80 bins for spectra */
    status = sscanf(parms->ebins+5, ":%d", &nebins);
    if (status <= 0) {
      if (parms->outtype == LC) {
	nebins = 4; 
      } else {
	nebins = 80;
      }
    }
    status = 0;

    /* Create expression for number of energy bins */
    sprintf(expr, "MODE.eq.%d", nebins);

    /* Actually query the CALDB */
    bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
		     &pfile, extno, &ponline, &nret, &nfound, &status);
    if ((status != 0) || (nret == 0) || (nfound == 0)) {
      fprintf(stderr, "ERROR: could not locate CALDB energy bins file\n");
      return status;
    }
      
  }

  /* Now buffersize is known */
  buffersize = parms->buffersize;
  banner(parms);

  /* ------------------- */
  /* Parse energy bins */
  if ((parms->intype == DPH) && 
      (strcasecmp(parms->ebins, "FILEBINS") == 0 ||
       strcasecmp(parms->ebins, "INFILE") == 0)) {
    /* DPH and FILEBINS binning */
    nebins = parms->ndphbins;
    for (i=0; i<nebins; i++) {
      emin[i] = parms->dphemin[i];
      emax[i] = parms->dphemax[i];
    } 
  } else if ((parms->intype == EVENTS) &&
	     (strcasecmp(parms->ebins, "FILEBINS") == 0 ||
	      strcasecmp(parms->ebins, "INFILE") == 0)) {

    /* EVENTS with INFILE binning, if the input file has the right
       structure */

    fits_open_data(&inevt, parms->infiles[0], READONLY, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not open %s\n", parms->infiles[0]);
      return status;
    }
    nebins = events_readebins(inevt, parms->coltype, emin, emax, 4096);
    fits_close_file(inevt, &status);
    if (nebins <= 0) {
      fprintf(stderr, "ERROR: could not determine event energy bins\n");
      return -1;
    }

  } else {
    /* EVENTS or non-FILEBINS binning */
    nebins = read_ebins(parms->ebins, emin, emax, 4096, parms->coltype);
  }

  if (nebins <= 0) {
    fprintf(stderr, "ERROR: could not parse energy bins\n");
    status = RANGE_PARSE_ERROR;
    goto CLEANUP;
  }

  /* ------------------- */
  /* Construct an energy binning lookup table (input file type dependent) */
  if (parms->intype == EVENTS) {
    /* EVENTS */
    status = events_mkebintab(parms, nebins, emin, emax, 65536, ebintab);
  } else {
    /* DPH */
    status = dph_mkebintab(parms, nebins, emin, emax, 65536, ebintab);
  }
  if (status) goto CLEANUP;

  /* ------------------- */
  /* Warn for uncommon output configurations */
  if (type_is_image(parms->outtype) && parms->weighted) {
    char *imgtype = "DPI";
    if (parms->outtype == DPH) imgtype = "DPH";
    fprintf(stderr,"==============================================================\n");
    fprintf(stderr, "WARNING: you are making a %s-type output with weighted=YES.\n",
	    imgtype);
    fprintf(stderr, "         This is an uncommon configuration.  Are you sure?\n");
    fprintf(stderr,"==============================================================\n");
  }


  /* ------------------- */
  /* Read and apply any detector masks */
  if (parms->detmask[0]) {
    headas_chat(5, "  ...Reading detector mask file...\n");
    detmask = read_detmask(parms->detmask, axesd, 0, 0, 1, &status);
    if (status != 0) {
      fprintf(stderr, "ERROR: could not read quality mask %s\n", 
	      parms->detmask);
      goto CLEANUP;
    }
    headas_chat(5, "    (%dx%d image)\n", axesd[0], axesd[1]);
    if ((axesd[0] != nimgx) || (axesd[1] != nimgy)) {
      fprintf(stderr, "ERROR: detector quality mask dimensions do not agree\n");
      fprintf(stderr, "       with output dimensions\n");
      goto CLEANUP;
    }
  }

  /* ------------------- */
  /* Read the mask weight map */
  parms->nappkeys = 0;
  if (parms->maskwt[0]) {
    headas_chat(5, "  ...Reading mask weight map file...\n");
    maskwt = read_maskwt(parms->maskwt, axesd, parms, &status);
    if (status != 0) {
      fprintf(stderr, "ERROR: could not read mask weight map %s\n", 
	      parms->maskwt);
      goto CLEANUP;
    }
    headas_chat(5, "    (%dx%d image)\n", axesd[0], axesd[1]);
    if ((axesd[0] != nimgx) || (axesd[1] != nimgy)) {
      fprintf(stderr, "ERROR: mask weight map dimensions do not agree\n");
      fprintf(stderr, "       with output dimensions\n");
      goto CLEANUP;
    }
  } else if (type_is_image(parms->intype) && parms->weighted) {
    fprintf(stderr, 
	    "WARNING: the input file is binned, but no 'maskwt' parameter was given.\n"
	    "         Did you mean to set weighted=NO?\n");
  }

  /* ------------------- */
  /* Initialize the generic binning parameters */
  headas_chat(5,"  ...initializing binning method...\n");
  spect.snr  = parms->snr;
  spect.snr2 = parms->snr*parms->snr;
  spect.nebins = nebins;
  spect.nsplits = 1;
  spect.accum_wt2 = 1;
  spect.tstart = parms->tmin;
  spect.tstop  = parms->tmax;
  if (type_is_image(parms->outtype)) {    /* Image size */
    spect.nsplits = nimgx * nimgy;
    /* No need to compute weights2 for images & DPHs */
    spect.accum_wt2 = 0;     
  }
  headas_chat(5, "    (nsplits=%d accum_wt2=%d)\n", 
	      spect.nsplits, spect.accum_wt2);

  /* ------------------- */
  /* Initialize the specific binning method */
  switch (tbinmethod) {
  case UNIFORM:
    status = uniform_init(&spect, parms);
    break;
  case USERBINS:
  case INFBINS:
  case MATCHBINS:
    status = userbin_init(&spect, parms);
    break;
  case CONSTSNR:
  case MINSNR:
    status = constsnr_init(&spect, parms);
    break;
  }
  headas_chat(5, "  (initial ntbins=%d)\n", spect.ntbins);

  /* Double check if we are being asked to make a sketchy combination
     of PHA2 and COUNTS. Downstream software doesn't handle this very
     well. */
  if ( ((strcasecmp(parms->unstr, "COUNT") == 0) || 
	(strcasecmp(parms->unstr, "COUNTS") == 0)) &&
       ( (parms->outtype == PHA && spect.ntbins == 1) ||
	 (parms->outtype == PHA2) ) ) {
    fprintf(stderr, 
	    "\n"
	    "WARNING: BAT COUNTS spectra in type II format are not handled\n"
	    "         well by downstream processing tools like CMPPHA or GRPPHA.\n"
	    "         We recommend you switch to RATE spectra.\n\n");
  }

  if (status) {
    fprintf(stderr, "ERROR: could not allocate spectrum structure\n");
    return status;
  }

  /* ------------------- */
  /* Allocate arrays for storage of the column values */
  headas_chat(5,"  ...allocating memory (buffersize=%d)...\n", buffersize);
  tev     = (double *) malloc(sizeof(double)*buffersize);
  pi      = (float *)  malloc(sizeof(float)*buffersize);
  weights = (float *)  malloc(sizeof(float)*buffersize);
  ebin    = (int *)    malloc(sizeof(int)*buffersize);
  segs    = (int *)    malloc(sizeof(int)*buffersize);
  detx    = (int *)    malloc(sizeof(int)*buffersize);
  dety    = (int *)    malloc(sizeof(int)*buffersize);
  split   = (int *)    malloc(sizeof(int)*buffersize);
  ncounts = (float *)  malloc(sizeof(float)*buffersize);
  if (parms->intype != EVENTS) {
    tend = (double *)  malloc(sizeof(double)*buffersize);
  }

  if ((tev == 0) || (pi == 0) || (ebin == 0) || (weights == 0) || 
      (segs == 0) || (detx == 0) || (dety == 0) || (split == 0) ||
      (ncounts == 0)){
    fprintf(stderr, "ERROR: Unable to allocate memory for buffers\n");
    status = MEMORY_ALLOCATION;
    goto CLEANUP;
  }

  /* If input is not DPH, then we don't need NCOUNTS, it's always 1 */
  if (parms->intype != DPH) {
    free(ncounts);
    ncounts = 0;
  }

  /* ------------------- */
  /* Main file loop */

  for (ifile=0; ifile<parms->ninfiles; ifile++) {

    /* Open input events file */
    infile = parms->infiles[ifile];
    if (parms->intype == EVENTS) {
      /* EVENTS */
      evtfile.infile = infile;
      evtfile.inevt = 0;
      evtfile.detmask = detmask;
      evtfile.maskwt  = maskwt;
      events_open(parms, &evtfile, &status);
      inevt = evtfile.inevt;
      
    } else {
      /* DPH */
      dphfile.infile = infile;
      dphfile.indph  = 0;
      dphfile.detmask = detmask;
      dphfile.maskwt  = maskwt;
      dph_open(parms, &dphfile, &status);
      indph = dphfile.indph;
    }

    if (status) {
      fprintf(stderr, "ERROR: could not open %s\n", infile);
      goto CLEANUP;
    }
    
    /* Loop through the input events in chunks */
    headas_chat(5,"  ...entering binning loop...\n");

    /* ------------------- */
    /* Keep reading data until none is remaining.  It is up to the
       input driver (i.e. events_read) to decide when there are no
       more events remaining. */
    remaining = 1;
    while (remaining) {

      nread = 0;
      if (parms->intype == EVENTS) {
	/* EVENTS */
	events_read(&evtfile, parms, nebintab, ebintab,
		    tev, pi, weights, ebin, segs, split, detx, dety,
		    &nread, &remaining, &status);
      } else {
	/* DPH */
	dph_read(&dphfile, parms, nebintab, ebintab,
		 tev, tend, pi, weights, ebin, segs, split, detx, dety, ncounts,
		 &nread, &remaining, &status);
      }

      if (status || (nread == 0)) {
	fprintf(stderr, "Error while reading from input file (status=%d nread=%d)\n",
		status, nread);
	  goto CLEANUP;
      }

      if (spect.nsplits > 1) split1 = split; else split1 = 0;

      /* ------------------- */
      /* Bin the data in time too */
      switch (tbinmethod) {
      case UNIFORM:
	status = uniform_bin(&spect, tev, tend, ebin, weights, nread,
			     segs, detx, dety, split1, ncounts);
	break;
      case MATCHBINS:
      case USERBINS:
      case INFBINS:
	status = userbin_bin(&spect, tev, tend, ebin, weights, nread,
			     segs, detx, dety, split1, ncounts, 
			     &(parms->ugti));
	/* NOTE: XXX detx is overwritten with scratch data!! XXX */
	break;
      case CONSTSNR:
      case MINSNR:
	status = constsnr_bin(&spect, tev, tend, ebin, weights, nread, 
			      segs, detx, dety, split1, ncounts, tbinmethod);
	break;
      }
      /* Check for a binning error */
      if (status == -1) {
	fprintf(stderr, 
		"ERROR: for file %s\n"
		"       the input starting near time MET %f and ending at time MET %f\n"
		"       spans across more than one output time bin.\n"
		"       You must specify an output time binning which does not split\n"
		"       up single DPHs rows.\n",
		infile, tev[0], tend ? tend[nread-1] : tev[nread-1]);
	goto CLEANUP;
      } else if (status != 0) {
	fprintf(stderr, "ERROR: while binning events\n");
	goto CLEANUP;
      }
      
    }

    /* ------------------- */
    /* Close the input file when finished */
    if (parms->intype == EVENTS) {
      /* EVENTS */
      events_close(&evtfile, parms, &status);
      nevt += evtfile.ndone;
    } else {
      /* DPH */
      dph_close(&dphfile, parms, &status);
      nevt += dphfile.ndone*dphfile.ntotbins;
    }

    status = 0;
    inevt = 0;

  }

  /* ------------------- */
  /* Convert the data to rates ... */
  headas_chat(5, "  (final ntbins=%d)\n", spect.ntbins);
  headas_chat(5,"  ...converting weights to rates...\n");
  weights2rates(&spect, parms);
  if (spect.ntbins == 0) {
    /* If the spectrum had no exposure, there could be no time bins left */
    fprintf(stderr, "WARNING: no time bins; not writing output FITS file\n");
    goto CLEANUP;
  }

  /* ------------------- */
  /* ... and write to FITS file */
  /* Create file and copy primary array of input FITS file */
  headas_chat(5,"  ...writing output file...\n");
  headas_clobberfile(outfilename);
  fits_create_file(&outfile, outfilename, &status);
  if (status) {
    fprintf(stderr, "ERROR: Could not open output file %s for writing\n",
	    outfilename);
    goto CLEANUP;
  }

  /* ------------------- */
  /* Copy the initial HDU of the first file to the output file */
  fits_open_file(&inevt, parms->infiles[0], READONLY, &status);
  headas_chat(5,"  ...copying primary header...\n");
  if (parms->outtype != DPI) {
    /* Now do the copying.  Copy from primary HDU of first file ... */

    /* NOTE: don't do this for DPIs, since they will occupy the
       primary HDU themselves. */
    fits_movabs_hdu(inevt, 1, 0, &status);      /* Move to primary HDU... */
    fits_copy_hdu(inevt, outfile, 0, &status);  /* ... copy it ... */
  }
  fits_movnam_hdu(inevt, BINARY_TBL, parms->dataext,
		  0, &status);                /* ... and back to orig. ext.*/

  if (status) {
    fprintf(stderr, "ERROR: Could not copy primary header of input to output\n");
    goto CLEANUP;
  }

  /* ------------------- */
  /* Actually write the output file, according to output type */
  if (parms->outtype == LC) {
    headas_chat(5,"  ...writing light curve extension...\n");
    status = writelc(inevt, outfile, parms, &spect,    /* Light curve */
		     nebins, emin, emax);   
  } else if (parms->outtype == PHA1 || 
	     (parms->outtype == PHA && spect.ntbins == 1)) {
    headas_chat(5,"  ...writing type I spectrum extension...\n");
    status = writepha1(inevt, outfile, parms, &spect);  /* Spectrum */
  } else if (parms->outtype == PHA2 || parms->outtype == PHA) {
    headas_chat(5,"  ...writing type II spectrum extension...\n");
    status = writepha2(inevt, outfile, parms, &spect);  /* Spectrum */
  } else if ((parms->outtype == DPH) || (parms->outtype == DPITAB)) {
    headas_chat(5,"  ...writing DPH/DPITAB extension...\n");
    status = writedph(inevt, outfile, parms, &spect,   /* DPH */
		     nebins, emin, emax);   
  } else if ((parms->outtype == DPI)) {
    headas_chat(5,"  ...writing DPI extension(s)...\n");
    status = writedpi(inevt, outfile, parms, &spect,   /* DPI */
		     nebins, emin, emax);   
  } else {
    fprintf(stderr, "ERROR: don't know how to write this output type yet!\n");
    status = -1;
  }

  if (status != 0) {
    fprintf(stderr, "ERROR: Could not write science data to output\n");
    goto CLEANUP;
  }
    
  /* ------------------- */
  /* Write EBOUNDS extension */
  headas_chat(5,"  ...writing ebounds extension...\n");
  status = writeebounds(inevt, outfile, parms, &spect, nebins, emin, emax);

  /* ------------------- */
  /* Write GTI extension */
  headas_chat(5,"  ...writing GTI extension...\n");
  status = writegti(inevt, outfile, parms);

  fits_close_file(outfile, &status);
  outfile = 0;

  

CLEANUP:    
  /* ------------------- */
  /* Close input event file no matter what */
  {
    int mystatus = 0;
    if (inevt) fits_close_file(inevt, &mystatus);
    inevt = 0;
  }

  if (status != 0) {
    fprintf(stderr, "ERROR: Could not write output file\n");
    if (outfile != 0) {
      int mystatus = 0;
      fits_close_file(outfile, &mystatus);
      outfile = 0;
    }
  }

  /* ------------------- */
  /* Reasonable summary number of "rows" for DPH inputs */
  if (parms->intype == DPH && dphfile.ntotbins > 0) { 
    nevt /= dphfile.ntotbins;
  }
  summary(parms, nevt, spect.totcounts, spect.badcounts, spect.ntbins, nebins);

  /* ------------------- */
  /* Deallocate memory */
  headas_chat(5,"  ...de-allocating memory...\n");
  if (tev) free(tev); 
  if (tend) free(tend);
  if (pi) free(pi);
  if (weights) free(weights);
  if (ebin) free(ebin);
  if (segs) free(segs);
  if (detx) free(detx);
  if (dety) free(dety);
  if (split) free(split);
  if (ncounts) free(ncounts);
  tev = 0; pi = 0; weights = 0; ebin = 0; segs = 0; detx = 0; dety = 0;
  if (detmask) free(detmask);
  detmask = 0;
  if (maskwt) free(maskwt);
  maskwt = 0;
  free_spect(&spect);
  
  if (parms->ftstart) free(parms->ftstart);
  if (parms->ftstop) free(parms->ftstop);
  if (parms->infiles)   free(parms->infiles);
  if (parms->gtimaster.ngti) HDgti_free( & (parms->gtimaster) );
  if (parms->ugti.ngti)      HDgti_free( & (parms->ugti) );
  if (parms->gtis) {
    for (i=0; i<parms->ninfiles; i++) 
      if (parms->gtis[i].ngti) HDgti_free( & (parms->gtis[i]) );
    free(parms->gtis);
    parms->gtis = 0;
  }

  return status;
}

/* ----------------------------------------------------------------- */
/* Main point of entry for tool */
int batbinevt(void)
{
  int status = 0;

  struct parm_struct parms;

  /* Register taskname and version. */

  set_toolname(taskname);
  set_toolversion(taskver);

  if ((status = batbinevt_getpar(&parms)) != 0) {
    fprintf(stderr, "Could not read parameter file\n");
    return status;
  }

  parms.taskname = &taskname[0];
  parms.taskver  = &taskver[0];

  return batbinevt_work(&parms);

}
