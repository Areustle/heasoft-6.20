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
 * Routines for opening, reading, and closing DPH files 
 *
 *  26 May 2003 - split out from batbinevt.c
 *
 * $Id: dphread.c,v 1.12 2010/06/11 19:00:32 craigm Exp $
 * C. Markwardt 
 */

/*
 * dph_open - open an events file, and read various column numbers
 *               in preparation for reading
 * dph_read - read counts from file, and process them partially
 * dph_close - close file and do some error checking 
 *
 */

/* Open and prepare an events file.
   Upon input:
    dphfile->infile - (char *) - name of input file
    dphfile->detmask - (int *) - pointer to detmask array, or 0
    everything else in dphfile will be destroyed.
   Returns: status code    
*/
int dph_open(struct parm_struct *parms,
	     struct dphfile_struct *dphfile,
	     int *status)
{
  fitsfile *indph = 0;
  char *infile;
  long int nrows;
  int timecol, dphcol, expocol;
  int tstopcol = -1, telapsecol = -1;
  int *detmask = 0;
  double *maskwt = 0;
  float *temp = 0;
  int ntotbins;
  int i;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return 0;
  if ((parms == 0) || (dphfile == 0) || (dphfile->infile == 0)) 
    return (*status = NULL_INPUT_PTR);

  infile = dphfile->infile;
  detmask = dphfile->detmask;
  maskwt  = dphfile->maskwt;
  headas_chat(5, "  ...opening DPH %s...\n", infile);
  memset(dphfile, 0, sizeof(*dphfile));

  indph = 0;
  fits_open_file(&indph, infile, READONLY, status); 
  if (*status) {
    fprintf(stderr, "Unable to open %s for read access\n", infile);
    goto CLEANUP;
  }
  dphfile->indph = indph;

  /* Move to the first extension containing BAT events.  */
  fits_movnam_hdu(indph, BINARY_TBL, "BAT_DPH", 0, status);
  if (*status != 0) {
    fprintf(stderr, "ERROR: input file does not contain a BAT_DPH extension\n");
    goto CLEANUP;
  }
    
  /* Read number of rows */
  if (fits_get_num_rows(indph, &nrows, status)) {
    fprintf(stderr, "The input file is not well-formed\n");
    goto CLEANUP;
  }
  headas_chat(5, "    (%d rows)\n", nrows);
  dphfile->nrows = nrows;

  *status = 0;
  /* Get column numbers */
  fits_get_colnum(indph,CASEINSEN,parms->timecolumn,&timecol,status);
  fits_get_colnum(indph,CASEINSEN,parms->dphcolumn,&dphcol,status);
  fits_get_colnum(indph,CASEINSEN,"EXPOSURE",&expocol,status);
  if (*status) {
    fprintf(stderr, 
	    "The input file does not contain the %s and %s columns\n",
	    parms->timecolumn, parms->dphcolumn);
    goto CLEANUP;
  }
  fits_write_errmark();
  fits_get_colnum(indph,CASEINSEN,"TIME_STOP",&tstopcol,status); *status = 0;
  fits_get_colnum(indph,CASEINSEN,"TELAPSE",&telapsecol,status); *status = 0;
  fits_clear_errmark();
  
  ntotbins = 1;
  for (i=0; i<parms->ndphaxes; i++) {
    ntotbins *= parms->dphaxes[i];
  }
  /* Size of both array and nulls */
  /* NOTE: the null array is tacked onto the end of the data array */
  temp = (float *) malloc((sizeof(float)+sizeof(char))*ntotbins);
  if (temp == 0) {
    *status = MEMORY_ALLOCATION;
    goto CLEANUP;
  }

  /* Copy data to dphfile structure */
  dphfile->timecol = timecol;
  dphfile->dphcol  = dphcol;
  dphfile->expocol = expocol;
  dphfile->tstopcol = tstopcol;
  dphfile->telapsecol = telapsecol;

  dphfile->ndone = 0;
  dphfile->detmask = detmask;
  dphfile->maskwt = maskwt;
  dphfile->data = temp;
  dphfile->ntotbins = ntotbins;
  dphfile->curbin = -1;

  return *status;

 CLEANUP:
  /* Error-handling case */
  if (indph) {
    int mystatus = 0;
    fits_close_file(indph, &mystatus);
  }
  dphfile->indph = 0;

  return *status;
}

int dph_overlap_seg(double *time1, double *time2,
		    struct gti_struct *gtimaster,
		    double minfracoverlap, double mintimeoverlap,
		    double maxtimenonoverlap,
		    int *status)
{
  struct gti_struct gti_dph, gti_overlap;
  double tarr[2];
  double tstarti, tstopi;
  int segs1, segs2;

  if (status == 0 || *status) return -1;
  tarr[0] = *time1; tarr[1] = *time2;
  
  /* Fill gti_dph with the time span of the individual DPH */
  gti_dph = *gtimaster;
  gti_dph.ngti = 1; gti_dph.maxgti = 1; gti_dph.dptr = 0;
  gti_dph.start = &(tarr[0]); gti_dph.stop = &(tarr[1]);
  
  /* Compute the overlap GTI with the master GTI */
  HDgti_init(&gti_overlap);
  HDgti_merge(GTI_AND, &gti_overlap, gtimaster, &gti_dph, status);

  /* There *should* be an overlap here, since it was already checked
     in read_tlimits(), but we check anyway.  We still need to do the
     full GTI intersection here since we want to know the start/stop
     times of the overlap period.
   */
  if (*status || gti_overlap.ngti == 0) {
    return -1;
  }

  /* We passed.  Assume that the overlap fraction/time has already
     been checked in read_tlimits() */

  tstarti = gti_overlap.start[0];
  tstopi = gti_overlap.stop[gti_overlap.ngti-1];
  HDgti_free(&gti_overlap);

  /* There is overlap.  Check to be sure that both the start and
     stop are in the same bin. */
  HDgti_where(gtimaster, 1, &tstarti, &segs1, status);
  HDgti_where(gtimaster, 1, &tstopi,  &segs2, status);
  if (segs1 < 0 || segs2 < 0 || *status) {
    return -1;
  }

  *time1 = tstarti;
  *time2 = tstopi;
  return segs1;
}


/* 
   Read DPH data (one row) from input file indicated by dphfile
   
   nebintab - size of energy binning lookup table
   ebintab  - energy binning lookup table
   tev - times from event file
   pi - energies from event file (not filled for DPH data)
   weights - weights from event file
   ebin - energy bin number from event file
   segs - time segment number from event file
   detx - X pixel number from event file
   dety - Y pixel number from event file

   *nread - upon return, number of events read
   *remaining - upon return, remaining events to read? 1=yes 0=no
   *status - status code
*/
int dph_read(struct dphfile_struct *dphfile,
		struct parm_struct *parms,
		int nebintab, int *ebintab,
		double *tev, double *tend, float *pi, float *weights,
		int *ebin, int *segs, int *split, 
	        int *detx, int *dety, float *ncounts,
		int *nread, int *remaining, int *status)
{
  fitsfile *indph = 0;
  int nbuf;
  int nimgx, nimgy, fileebins;
  int off;
  int j, k;
  int *detmask = 0;
  double *maskwt = 0;
  double time1, time2; 
  double expo1 = 0, telapse = 0;
  float *data = 0;
  int segs1;
  int ntotbins, curbin;
  int anynull = 0;
  char *nullptr;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if ((dphfile == 0) || (tev == 0) || (remaining == 0) || (nread == 0)) 
    return (*status = NULL_INPUT_PTR);

  *remaining = 0;
  *nread = 0;
  
  indph = dphfile->indph;
  nbuf = parms->buffersize;
  detmask = dphfile->detmask;
  maskwt = dphfile->maskwt;
  nimgx = parms->nimgx;
  nimgy = parms->nimgy;
  off = dphfile->ndone + 1;         /* FITS file row number */
  fileebins = parms->dphaxes[0];
  data = dphfile->data;
  ntotbins = dphfile->ntotbins;
  curbin = dphfile->curbin;
  /* NOTE: the null array is tacked onto the end of the data array */
  nullptr = (char *) ((int *)dphfile->data + ntotbins);

  /* These reads are inexpensive, I think. */
  fits_read_col(indph,TDOUBLE,dphfile->timecol,off,1,1,0,&time1, 0,status);

  /* time2 is meant to be the end of the histogram time interval.  We
     try several different columns to have a good shot at finding the
     end of the interval. */
  time2 = time1;
  /* printf("XXX time %f status %d curbin %d off %d\n", time1, *status, curbin,off); */
  if (dphfile->tstopcol > 0) {
    fits_read_col(indph,TDOUBLE,dphfile->tstopcol,off,1,1,0,&time2, 0,status);
  } else if (dphfile->telapsecol > 0) {
    fits_read_col(indph,TDOUBLE,dphfile->telapsecol,off,1,1,0,&telapse, 0,status);
    if (*status == 0) time2 += telapse;
  } else if (dphfile->expocol > 0) {
    fits_read_col(indph,TDOUBLE,dphfile->expocol,off,1,1,0,&expo1, 0,status);
    if (*status == 0) time2 += expo1;
  }

  if (time2 <= time1) {
    fprintf(stderr, "ERROR: input DPH has invalid times (row=%d, tstart=%f tstop=%f)\n",
	    off, time1, time2);
    return (*status = -1);
  }

  time2 -= 1e-6;  /* XXX Slight offset to prevent the GTI filter from
		     flagging histogram endpoints which exactly match
		     the end of a GTI. */

  if (curbin < 0) {
    headas_chat(5,"  (reading %d DPH bins)\n", ntotbins);
    fits_read_colnull(indph,TFLOAT,dphfile->dphcol,off,1,ntotbins,
		      data, nullptr, &anynull, status);
    /* printf("XXX data %d status %d off %d\n", data[0], *status, off); */

    if (*status) {
      fprintf(stderr, "Error while reading from input DPH file\n");
      return *status;
    }
    curbin = 0;
  }

  /* Do good time interval filtering, check the start of the interval first
     and then the end of the interval. */

  segs1 = dph_overlap_seg(&time1, &time2, &(parms->gtimaster),
			  parms->minfracoverlap, parms->mintimeoverlap,
			  parms->maxtimenonoverlap,
			  status);

  k = 0;
  j = curbin;
  for (k=0, j=curbin; (j<ntotbins) && (k<nbuf); j++) {

    if (nullptr[j]) continue;  /* Skip empty values */
    
    /* Record start/stop time of each DPH */
    tev[k]  = time1;
    tend[k] = time2;

    segs[k] = segs1;

    ebin[k]  = ebintab[j % fileebins];
    split[k] = j / fileebins;
    detx[k]  = split[k] % nimgx;
    dety[k]  = split[k] / nimgx;
    if ((detx[k] % 18) > 15) {
      /* This spatial column is a gap, skip all these energy bins */
      j += (fileebins-1);
      continue;
    }
    if ((dety[k] % 11) > 7) {
      /* This spatial row is a gap, skip the entire row */
      j += (fileebins*nimgx-1);
      continue;
    }
    if (detmask && (detmask[dety[k]*nimgx+detx[k]] == 0)) {
      /* This data was masked out by detmask, so ignore these energy bins */
      j += (fileebins-1);
      continue;
    }

    ncounts[k] = data[j];

    k++;  /* Advance to next output bin */
  }
  nbuf = k;

  /* printf("final curbin=%d j=%d k=%d\n",curbin,j,k);*/
  /* We finished a row.  Advance the ndone indicator, and reset
     curbin, so that next time around we read a new row (if there is
     one) */
  if (j >= ntotbins) {
    dphfile->ndone += 1;
    curbin = -1;
  } else {
    curbin = j;
  }

  /* Fill in the mask weight if it is present, 1.0 if not */
  if (maskwt) {
    for (j=0; j<nbuf; j++) {
      weights[j] = maskwt[split[j]];
      /* headas_chat(5,"  %d: fileebins=%d split=%d counts=%f weight=%f\n",
	 j, fileebins, split[j], ncounts[j], weights[j]); */
    }
  } else if (parms->weighted == 0) {
    for (j=0; j<nbuf; j++) {
      if (ebin[j] != -1) weights[j] = 1;
    }
  }

  if (dphfile->ndone < dphfile->nrows) *remaining = 1;
  *nread = nbuf;
  dphfile->curbin = curbin;

  return *status;
}

/* 
   Close events file and do some error checking
*/
int dph_close(struct dphfile_struct *dphfile,
		 struct parm_struct *parms,
		 int *status)
{
  fitsfile *indph = 0;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if (dphfile == 0) return (*status = NULL_INPUT_PTR);


  /* Free temporary storage */
  if (dphfile->data) {
    free(dphfile->data);
    dphfile->data = 0;
  }

  /* Free file resources */
  indph = dphfile->indph;
  fits_close_file(indph, status);
  dphfile->indph = 0;

  return *status;
}

int dph_mkebintab(struct parm_struct *parms,
		  int nbins, float *emin, float *emax, 
		  int nebintab, int *ebintab)
{
  int i, j;
  int jmin, jmax;
  int bad = 0, totbad = 0;

  if (emin[0] == 0.0) 
    emin[0] = parms->dphemin[0];
  if (emax[nbins-1] == 65536.0) 
    emax[nbins-1] = parms->dphemax[parms->ndphbins-1];

  /* Initialize with -1 */
  for (i=0; i<nebintab; i++) {
    ebintab[i] = -1;
  }

  /* Check for an (almost) exact match on energies */
  for (i=0; i<nbins; i++) {
    
    for (jmin=0; jmin<(parms->ndphbins); jmin++) {
      if (fabs(emin[i]-parms->dphemin[jmin]) < 0.01) break;
    }
    for (jmax=0; jmax<(parms->ndphbins); jmax++) {
      if (fabs(emax[i]-parms->dphemax[jmax]) < 0.01) break;
    }

    if ((jmin == parms->ndphbins) || (jmax == parms->ndphbins)) {
      fprintf(stderr, "ERROR: the requested bins must exactly match the\n");
      fprintf(stderr, "       bin edges of the input file to within 0.01 keV.\n");
      return -1;
    }

    bad = 0;
    for (j=jmin; j<=jmax; j++) {
      if (ebintab[j] != -1) bad = 1;
      ebintab[j] = i;
    }

    if (bad) {
      fprintf(stderr, "ERROR: energy bin number %d (%f-%f) overlaps with another bin\n",
	      i+1, emin[i], emax[i]);
      totbad = 1;
    }
  }

  return -totbad;
}
