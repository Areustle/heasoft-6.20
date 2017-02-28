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
 * Routines for opening, reading, and closing events from events files
 *
 *  26 May 2003 - split out from batbinevt.c
 *
 * $Id: events.c,v 1.14 2010/05/21 23:43:12 craigm Exp $
 * C. Markwardt 
 */

/*
 * events_open - open an events file, and read various column numbers
 *               in preparation for reading
 * events_read - read events from file, and process them partially
 * events_close - close file and do some error checking 
 *
 */

/* Open and prepare an events file.
   Upon input:
    evtfile->infile - (char *) - name of input file
    evtfile->detmask - (int *) - pointer to detmask array, or 0
    everything else in evtfile will be destroyed.
   Returns: status code    
*/
int events_open(struct parm_struct *parms,
		struct evtfile_struct *evtfile,
		int *status)
{
  fitsfile *inevt = 0;
  char *infile;
  long int nrows;
  int timecol = 0, picol = 0, wtcol = 0, xcol = 0, ycol = 0;
  int *detmask = 0;
  double *maskwt = 0;
  int needmaskwtcol = 0;
  double timedel = 0, timepixr = 0.5;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return 0;
  if ((parms == 0) || (evtfile == 0) || (evtfile->infile == 0)) 
    return (*status = NULL_INPUT_PTR);

  infile = evtfile->infile;
  detmask = evtfile->detmask;
  maskwt = evtfile->maskwt;
  needmaskwtcol = (maskwt == 0) && (parms->weighted == 1);
  headas_chat(5, "  ...opening %s...\n", infile);
  memset(evtfile, 0, sizeof(*evtfile));

  inevt = 0;
  fits_open_file(&inevt,infile,READONLY, status); 
  if (*status) {
    fprintf(stderr, "Unable to open %s for read access\n", infile);
    goto CLEANUP;
  }
  evtfile->inevt = inevt;
  evtfile->infile = infile;

  /* Move to the first extension containing BAT events.  */
  fits_movnam_hdu(inevt, BINARY_TBL, "EVENTS", 0, status);
  if (*status != 0) {
    fprintf(stderr, "ERROR: input file does not contain an EVENTS extension\n");
    goto CLEANUP;
  }
    
  /* Read number of rows */
  if (fits_get_num_rows(inevt, &nrows, status)) {
    fprintf(stderr, "The input file is not well-formed\n");
    goto CLEANUP;
  }
  headas_chat(5, "    (%d rows)\n", nrows);
  evtfile->nrows = nrows;

  *status = 0;
  /* Get column numbers */
  fits_get_colnum(inevt,CASEINSEN,parms->timecolumn,&timecol,status);
  if (strcmp(parms->ebins,"-") != 0) {
    /* Only skip the PI column if the energy binning is "-", meaning
       all bins accepted */
    fits_get_colnum(inevt,CASEINSEN,parms->coltype,&picol,status);
  }
  if (needmaskwtcol) {
    fits_get_colnum(inevt,CASEINSEN,parms->maskwtcolumn,&wtcol,status);
  }
  if (maskwt || detmask || type_is_image(parms->outtype)) {
    fits_get_colnum(inevt,CASEINSEN,parms->detxcolumn,&xcol,status);
    fits_get_colnum(inevt,CASEINSEN,parms->detycolumn,&ycol,status);
  }
    
  if (*status) {
    char colnames[100];
    fprintf(stderr, 
	    "The input file does not contain the required columns:\n");
    colnames[0] = 0;
    strcat(colnames, parms->timecolumn); strcat(colnames, ", ");
    if (needmaskwtcol) {
      strcat(colnames, parms->maskwtcolumn); strcat(colnames, ", ");
    }
    if (maskwt || detmask || type_is_image(parms->outtype)) {
      strcat(colnames, parms->detxcolumn); strcat(colnames, ", ");
      strcat(colnames, parms->detycolumn); strcat(colnames, ", ");
    }
    if (strcmp(parms->ebins,"-") != 0) {
      strcat(colnames, " and "); strcat(colnames, parms->coltype);
    }
    fprintf(stderr, "     %s (please check input)\n", colnames);
    goto CLEANUP;
  }

  fits_write_errmark();
  fits_read_key(inevt, TDOUBLE, "TIMEDEL", &timedel, 0, status);
  fits_clear_errmark();
  if (*status) {
    *status = 0;
    headas_chat(5,"  ...assuming TIMEDEL=0...\n");
  } else {
    headas_chat(5,"  ...TIMEDEL=%f...\n", timedel);
  }
  fits_write_errmark();
  fits_read_key(inevt, TDOUBLE, "TIMEPIXR", &timepixr, 0, status);
  fits_clear_errmark();
  if (*status) {
    *status = 0;
    headas_chat(5,"  ...assuming TIMEPIXR=%f...\n");
  } else {
    headas_chat(5,"  ...TIMEPIXR=%f...\n", timepixr);
  }

  /* Copy data to evtfile structure */
  evtfile->timecol = timecol;
  evtfile->picol   = picol;
  evtfile->wtcol   = wtcol;
  evtfile->xcol    = xcol;
  evtfile->ycol    = ycol;

  evtfile->ndone = 0;
  evtfile->blankweights = 1;  /* To see if all weights are 0 */
  evtfile->blankpi      = 1;  /* To see if all pi are 0 */
  evtfile->detmask = detmask;
  evtfile->maskwt = maskwt;
  evtfile->timedel = timedel;
  evtfile->dtcent  = (0.5 - timepixr)*timedel;  /* Correction to get center-of-bin */
  headas_chat(5,"    (time stamp correction of %e s to move to bin center)\n",
	      evtfile->dtcent);

  return *status;

 CLEANUP:
  /* Error-handling case */
  if (inevt) {
    int mystatus = 0;
    fits_close_file(inevt, &mystatus);
  }
  evtfile->inevt = 0;

  return *status;
}

/* 
   Read events from input file indicated by evtfile
   
   nebintab - size of energy binning lookup table
   ebintab  - energy binning lookup table
   tev - times from event file
   pi - energies from event file
   weights - weights from event file
   ebin - energy bin number from event file
   segs - time segment number from event file
   detx - X pixel number from event file
   dety - Y pixel number from event file

   *nread - upon return, number of events read
   *remaining - upon return, remaining events to read? 1=yes 0=no
   *status - status code
*/
int events_read(struct evtfile_struct *evtfile,
		struct parm_struct *parms,
		int nebintab, int *ebintab,
		double *tev, float *pi, float *weights,
		int *ebin, int *segs, int *split, int *detx, int *dety,
		int *nread, int *remaining, int *status)
{
  fitsfile *inevt = 0;
  int nbuf;
  int nimgx, nimgy;
  int off;
  int i;
  int *detmask = 0;
  double *maskwt = 0;
  int needmaskwtcol = 0;
  float ebinquant = 0.1;
  /* Default value for out of bounds data.  NOTE: this should be < 0
     so that it triggers the heV < 0 case below. */
  float tnull_pi = -32768.0;  

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if ((evtfile == 0) || (tev == 0) || (remaining == 0) || (nread == 0)) 
    return (*status = NULL_INPUT_PTR);

  *remaining = 0;
  *nread = 0;
  nbuf = (evtfile->nrows - evtfile->ndone);
  off = evtfile->ndone + 1;         /* FITS file row number */
  if (nbuf > parms->buffersize) {
    nbuf = parms->buffersize;
  }

  inevt = evtfile->inevt;
  detmask = evtfile->detmask;
  maskwt = evtfile->maskwt;
  nimgx = parms->nimgx;
  nimgy = parms->nimgy;

  /* Read energy binning quantization */
  if (parms->ebinquant > 0) ebinquant = parms->ebinquant;

  needmaskwtcol = (maskwt == 0) && (parms->weighted == 1);

  /* Read FITS data */

  /* Column TIME */
  fits_read_col(inevt,TDOUBLE,evtfile->timecol,off,1,nbuf,0,tev, 0,status);
  for (i=0; i<nbuf; i++) { tev[i] += evtfile->dtcent; } /* Correct to bin center */

  /* Column PHA or PI */
  if (evtfile->picol != 0) {
    int anynul = 0;
    fits_read_col(inevt,TFLOAT,evtfile->picol, off,1,nbuf,
		  &tnull_pi,pi, &anynul,status);
  }
  /* Column MASK_WEIGHT */
  if (needmaskwtcol) {
    fits_read_col(inevt,TFLOAT,evtfile->wtcol, off,1,nbuf,0,weights, 0,status);
  }
  /* Column DETX / DETY */
  if (maskwt || detmask || type_is_image(parms->outtype)) {
    fits_read_col(inevt,TINT,evtfile->xcol, off,1,nbuf,0,detx, 0,status);
    fits_read_col(inevt,TINT,evtfile->ycol, off,1,nbuf,0,dety, 0,status);
  }

  if (*status) {
    fprintf(stderr, "Error while reading from input events file\n");
    return *status;
  }

  /* Fill in the mask weights from the weight map, if available.  The
     map always takes priority over the MASK_WEIGHT column.  */
  if (maskwt) {
    for (i=0; i<nbuf; i++) {
      weights[i] = maskwt[dety[i]*nimgx+detx[i]];
    }
  }
    

  /* Check against the possibility that all weights are zero, and
     later warn the user.  Here, just check the weights until a
     non-zero one is found */
  if (evtfile->blankweights) for (i=0; i<nbuf; i++) {
    if (weights[i] != 0) {
      evtfile->blankweights = 0;
      break;
    }
  }
  /* Same for PI value */
  if ((evtfile->picol != 0) && evtfile->blankpi) for (i=0; i<nbuf; i++) {
    if (pi[i] != 0) {
      evtfile->blankpi = 0;
      break;
    }
  }
  
  /* Do good time interval filtering */
  HDgti_where(&parms->gtimaster, nbuf, tev, segs, status);
  
  if (evtfile->picol == 0) {
    /* If there is no PI column, then just set the energy bin to zero */
    for (i=0; i<nbuf; i++) ebin[i] = 0;
  } else for (i=0; i<nbuf; i++) {
    /* Compute the energy bin, nominally in hecto-electron Volts */
    int heV = floor(pi[i]/ebinquant + 0.01);
    /* NOTE: the 0.01 bins is to make sure that we don't put the
       batbinevt quantization boundary right at an existing TSCAL
       quantization boundary. */
    /* NOTE ALSO: that the (pi[i] == TNULL) case is picked up here */
    
    if ((heV < 0) || (heV >= nebintab)) ebin[i] = -1;
    else ebin[i] = ebintab[heV];
  }    
  
  /* Do spatial quality filtering - NOTE: read_detmask modifies
     the detmask[] so that 0=bad, 1=good, regardless of the format
     of the input quality map. */
  if (detmask) {
    for (i=0; i<nbuf; i++) {
      if ((detx[i] < 0) || (dety[i] < 0) ||
	  (detx[i] >= nimgx) || (dety[i] >= nimgy) ||
	  detmask[dety[i]*nimgx+detx[i]] == 0) {
	ebin[i] = -1;
      }
    }
  }
  
  /* Make spatial dimension, which becomes the "split" */
  if (split && type_is_image(parms->outtype)) {
    for (i=0; i<nbuf; i++) 
      split[i] = dety[i]*nimgx + detx[i];
  }
  
  /* If no weighting is being applied, then reset all weights to
     1, as long as they are in-bounds. */
  if (parms->weighted == 0) {
    for (i=0; i<nbuf; i++) {
      if (ebin[i] != -1) weights[i] = 1;
    }
  }

  evtfile->ndone += nbuf;
  if (evtfile->ndone < evtfile->nrows) *remaining = 1;
  *nread = nbuf;

  return *status;
}

/* 
   Close events file and do some error checking
*/
int events_close(struct evtfile_struct *evtfile,
		 struct parm_struct *parms,
		 int *status)
{
  fitsfile *inevt = 0;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if (evtfile == 0) return (*status = NULL_INPUT_PTR);

  inevt = evtfile->inevt;

  /* If we got here and blankweights is still set, then all the
     events had zero weight, which is suspicious... */
  if (evtfile->blankweights && parms->weighted) {
    headas_chat(1, "WARNING: all events in the following file had zero weight:\n");
    headas_chat(1, "         %s\n", evtfile->infile);
    headas_chat(1, "    --> Did you remember to apply mask weighting?\n");
    headas_chat(1, "\n");
  }

  /* If we got here and blankweights is still set, then all the
     events had zero weight, which is suspicious... */
  if ((evtfile->picol != 0) && evtfile->blankpi) {
    headas_chat(1, "WARNING: all events in the following file had zero %s value:\n", parms->coltype);
    headas_chat(1, "         %s\n", evtfile->infile);
    headas_chat(1, "    --> Did you remember to use bateconvert to fill the %s column?\n", parms->coltype);
    headas_chat(1, "\n");
  }

  fits_close_file(inevt, status);
  evtfile->inevt = 0;

  return *status;
}


/* ----------------------------------------------------------------- */
/* Convert a set of energy bins into a look-up table, so that the bin
   number of a photon with energy E can be found quickly.

   The table is initialized with -1 to indicate "no bin"
 */
int events_mkebintab(struct parm_struct *parms,
		     int nbins, float *emin, float *emax, 
		     int nebintab, int *ebintab)
{
  int i, j;
  int imin, imax;
  float ebinquant = 0.1;
  int bad = 0, totbad = 0;

  if ((parms == 0) || (nbins == 0) || (emin == 0) || (emax == 0) ||
      (nebintab == 0) || (ebintab == 0)) {
    return NULL_INPUT_PTR;
  }

  if (parms->default_ebinquant > 0) ebinquant = parms->default_ebinquant;
  if (parms->ebinquant > 0)         ebinquant = parms->ebinquant;

  /* Initialize with -1 */
  for (i=0; i<nebintab; i++) {
    ebintab[i] = -1;
  }

  for (i=0; i<nbins; i++) {
    imin = rint(emin[i]/ebinquant);
    imax = rint(emax[i]/ebinquant);
    
    if (imin < 0)         imin = 0;
    if (imax >= nebintab) imax = nebintab-1;

    /* Handle the case where the upper bound touches the event bin
       edge exactly.  The upper bound is considered open, so it does
       not include the upper bound point itself. I.e. 14-16 keV
       includes 15.999999 keV but not 16 keV. */
    if ((imax > imin) && fabs(imax * ebinquant - emax[i]) < 0.01*ebinquant) {
      imax --;
    }
    
    bad = 0;
    for (j=imin; j<=imax; j++) {
      if (ebintab[j] != -1) bad++;
      ebintab[j] = i;
    }
    if (bad > 1) {
      fprintf(stderr, "ERROR: energy bin number %d (%f-%f) overlaps with another bin\n",
	      i+1, emin[i], emax[i]);
      totbad = 1;
    }

  }    

  return -totbad;
}

int events_readebins(fitsfile *infile, char *coltype,
		     float *emin, float *emax, int nbins)
{
  int status = 0;
  int colnum = 0;
  char typechar[FLEN_CARD];
  double scale, zero;
  int typecode;
  long repeat, width;
  char tlmin[FLEN_CARD], tlmax[FLEN_CARD];
  int i, j;
  int minval, maxval;
  
  fits_get_colnum(infile,CASEINSEN,coltype,&colnum,&status);
  if (status) return -1;

  fits_get_eqcoltype(infile, colnum, &typecode, &repeat, &width, &status);
  fits_get_bcolparms(infile, colnum, 0 /* TTYPE */, 0 /* TUNIT */,
		     typechar, 0 /* REPEAT */, &scale, &zero,
		     0 /* NULVAL */, 0 /* TDISP */, &status);
  if (status) return -1;
  fits_make_keyn("TLMIN", colnum, tlmin, &status);
  fits_make_keyn("TLMAX", colnum, tlmax, &status);
  fits_read_key(infile, TINT, tlmin, &minval, 0, &status);
  fits_read_key(infile, TINT, tlmax, &maxval, 0, &status);
  if (status) {
    fprintf(stderr, "ERROR: input file must have TLMIN/TLMAX keywords\n");
    return -1;
  }
  headas_chat(5, "  ...using INFILE method for event data...\n");
  headas_chat(5, 
	      "  ...column (%s/%d) parameters:\n"
	      "  typecode=%d scale=%f zero=%f tlmin=%d tlmax=%d\n",
	      coltype, colnum,
	      typecode, scale, zero, minval, maxval);

  if (scale != 1 || zero != 0 || typecode == TFLOAT || typecode == TDOUBLE) {
    fprintf(stderr, 
	    "ERROR: input column %s must have integer type and not have scale factors\n",
	    coltype);
    return -1;
  }
  
  if (minval-maxval+1 > nbins) {
    fprintf(stderr, "ERROR: there are too many input bins (max=%d)\n",
	    nbins);
    return -1;
  }
  
  for (j=0, i=minval;   i<=maxval;    i++, j++) {
    emin[j] = i;
    emax[i] = i + 0.9999;
  }
  headas_chat(5, "  ...created %d energy bins from %d to %d...\n",
	      maxval-minval+1, minval, maxval);
  
  return (maxval-minval+1);
}
