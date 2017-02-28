#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "battblocks.h"


/* 
 * battblocks - task for computing various time blocks from event or
 * light curve data
 * 
 * Subroutines for event data
 *
 *   Cost function from Scargle 1998
 *
 * C. Markwardt
 *
 * $Id: bbevts.c,v 1.10 2004/08/22 00:36:37 craigm Exp $
 *
 */

int logprob_evt(int *cellpops, int *cellsizes, int ncells, 
		double *logprob, double ncp_prior);

/* ============================================================= */
/* Convert a set of events to cells.  To first order, each event forms
   its own cell, but usually for efficiency, several events are
   grouped together into a single cell. */
int evt2cells(double *t, int ntimes, 
	      int **cellsizes, int **cellpops, int *ncells, 
	      double tstart, double tstop, double timedel, int nspill)
{
  int *cpops = 0, *csize = 0;
  int nc, i;

  if ((t == 0) || (ntimes <= 0) || (cellsizes == 0) || (cellpops == 0) ||
      (timedel <= 0) || (nspill <= 0) || (tstop - tstart <= 0)) {
    return 0;
  }

  if (nspill == 1) {
    nc = ntimes;
  } else {
    nc = ceil((double)ntimes / nspill);
  }

  cpops = (int *) malloc(sizeof(int)*nc);
  csize = (int *) malloc(sizeof(int)*nc);
  if ((cpops == 0) || (csize == 0)) {
    if (cpops) free(cpops);
    if (csize) free(csize);
    return 0;
  }

  csize[0] = (t[nspill]-tstart)/timedel;
  cpops[0]  = nspill;
  for (i=1; i<(nc-1); i++) {
    csize[i] = (t[(i+1)*nspill]-t[i*nspill])/timedel;
    cpops[i]  = nspill;
  }
  csize[nc-1] = (tstop-t[(nc-1)*nspill])/timedel;
  cpops[nc-1] = ntimes - (nc-1)*nspill;

  *cellsizes = csize;
  *cellpops  = cpops;
  *ncells    = nc;
  
  return 0;
}

/* ============================================================= */
/* 
 * rebinevts - rebin events with a known set of change points
 */
int rebinevts(double *t, int ntimes, 
	      int **cellsizes, int **cellpops,
	      int *cps, int ncps, double timedel)
{
  int *cpops = 0, *csize = 0;
  int j;
  int nc = ncps-1;

  if (*cellsizes) free(*cellsizes);
  if (*cellpops)  free(*cellpops);
  *cellsizes = 0;
  *cellpops = 0;

  cpops = (int *) malloc(sizeof(int)*nc);
  csize = (int *) malloc(sizeof(int)*nc);
  if ((cpops == 0) || (csize == 0)) {
    if (cpops) free(cpops);
    if (csize) free(csize);
    return 0;
  }

  for (j=0; j<nc; j++) {
    csize[j] = (t[cps[j+1]]-t[cps[j]])/timedel;
    cpops[j] = cps[j+1]-cps[j];
  }
  cpops[nc-1] ++;

#if 0
  { 
    int ntot = 0;
    headas_chat(5,"Rebinned LC\n");
    for (j=0; j<nc; j++) {
      headas_chat(5,"  %5d %10d %10d %f-%f\n", j, csize[j], cpops[j], t[cps[j]], t[cps[j+1]]);
      ntot += cpops[j];
    }
    headas_chat(5,"  TOTAL COUNTS: %d\n", ntot);
  }
#endif

  *cellsizes = csize;
  *cellpops  = cpops;
  
  return 0;
}

/* ============================================================= */
/* Form the cumulative sum of the events.
 *  There is assumed to be one event per time.
 */
double *evtcumsum(int ntimes)
{
  int i;
  double *cumcounts = 0;

  cumcounts = (double *)malloc(sizeof(double)*ntimes);
  if (cumcounts == 0) return 0;

  for (i=0; i<ntimes; i++) {
    cumcounts[i] = (double) i+1;
  }

  return cumcounts;
}

/* ============================================================= */
/* Determine the Bayesian block change points, based on the 
   event-based cost function */
int *evtbayes(int *cellsizes, int *cellpops, int ncells, 
	      double ncp_prior, int *ncparray,
	      double **bestlogprob, int **lastcellstart, int nlag)
{
  int *cumsizes = 0, *cumpops = 0, *last_start;
  int *cparray = 0;
  double *merged = 0, *best = 0;
  double temp;
  int i, j, imaxer, ncp, index, icp;
  int istart = 0, ioldstart = 0;

  if (bestlogprob) *bestlogprob = 0;
  if (lastcellstart) *lastcellstart = 0;

  cumsizes = (int *) malloc(sizeof(int)*ncells);
  cumpops  = (int *) malloc(sizeof(int)*ncells);
  last_start = (int *) malloc(sizeof(int)*ncells);
  merged   = (double *) malloc(sizeof(double)*ncells);
  best     = (double *) malloc(sizeof(double)*ncells);
  if ((cumsizes == 0) || (cumpops == 0) || (merged == 0) || 
      (best == 0) || (last_start == 0)) {
    if (cumsizes) free(cumsizes);
    if (cumpops) free(cumpops);
    if (merged) free(merged);
    if (best) free(best);
    if (last_start) free(last_start);
    return 0;
  }
  
  for (i=0; i<ncells; i++) {
    cumsizes[i] = 0;
    cumpops[i] = 0;
  }

  istart = 0; ioldstart = 0;

  for (i=0; i<ncells; i++) {

    /* Approximation to the "nibble" algorithm */
    if (nlag > 0) {
      istart = i - nlag;
      if (istart < 0) istart = 0;
    }

    /* If we are nibbling, then we must shift the best[] array, so
       that the normalized probability is unity before the starting
       element. */
    if ( (istart > 0) && (istart != ioldstart) ) {
      for (j=istart; j<i; j++) {
	best[j] -= best[istart-1];
      }
    }

    /* Accumulate the parameters */
    for(j=istart; j<i; j++) {
      cumsizes[j] += cellsizes[i];
      cumpops[j]  += cellpops[i];
    }
    cumsizes[i] = cellsizes[i];
    cumpops[i] = cellpops[i];

    /* Compute the cost function for the cumulants */
    logprob_evt(cumpops+istart, cumsizes+istart, i-istart+1, 
		merged+istart, ncp_prior);

    /* Where is the maximum probability in the joint best|merged
       arrays? */
    imaxer = istart;
    best[i] = merged[istart];
    if (i > 0) {
      for(j=istart+1; j<=i; j++) {
	temp = best[j-1]+merged[j];
	if (temp > best[i]) {
	  best[i] = temp;
	  imaxer = j;
	}
      }
    }

    /* Record the new best position */
    last_start[i] = imaxer;

    /* Keep track of the previous nibble starting point */
    ioldstart = istart;
  }

  /* Count number of change points */
  ncp = 2;
  index = last_start[ncells-1];
  while (index > 1) {
    ncp ++;
    index = last_start[index-1];
  }

  /* Create output array of change points */
  cparray = (int *) malloc(sizeof(int)*ncp);
  if (cparray == 0) {
    ncp = 0;
    goto CLEANUP;
  }
  
  icp = ncp-1;
  cparray[icp--] = ncells;
  index = last_start[ncells-1];
  while (index > 1) {
    cparray[icp--] = index;
    index = last_start[index-1];
  }
  cparray[0] = 0;

 CLEANUP:
  if (cumsizes) free(cumsizes);
  if (cumpops) free(cumpops);
  if (merged) free(merged);
  if (last_start) {
    if (lastcellstart) *lastcellstart = last_start;
    else free(last_start);
  }
  if (bestlogprob) {
    if (bestlogprob) *bestlogprob = best;
    else free(best);
  }

  *ncparray = ncp;
  return cparray;
}

/*
 * logprob_evt - Compute log posterior probability for event data
 *
 * int *cellpops - populations of cells (i.e. number of events per cell)
 * int *cellsizes - widths of cells, in units of timedel
 * int ncells - number of cells
 * double *logprob - upon return, the log probability
 * double ncp_prior - log(prob) prior
 *
 * RETURNS: CFITSIO status value 
 *

;-----------------------------------------------------------------
; See:  J.D. Scargle, 1998, ApJ, 504, 405
;
; Log posterior (Bayes factor) for constant-rate Poisson data:
;   * flat prior on Poisson rate parameter (unnormalized)
;   * geometric prior on number of changepoints
;
; Input: cell_sizes -- size (length in 1D) of each cell (array)
;        cell_pops  -- number of events in each cell (array)
;        ncp_prior  -- log parameter for number of changepoints
;
; The first two inputs are computed from make_cells.  The third
; input, ncp_prior, acts like a smoothing parameter, weighting
; against separate blocks.  The optimal value for ncp_prior was
; determined empirically, adjusting its value until virtually
; no "spikes" remained, while retaining significant block
; structure across the time series.
;
; datatype: 1 --> time tagged data; use midpoints of data cells
;           2 --> time tagged data; use intervals of data cells
;-----------------------------------------------------------------
*/

int logprob_evt(int *cellpops, int *cellsizes, int ncells, 
		double *logprob, double ncp_prior)
{
  int i;
  double arg;

  for (i=0; i<ncells; i++) {
    arg = cellsizes[i] - cellpops[i] + 1;
    logprob[i] = 1e-38;
    if (arg > 0) {
      logprob[i] = (lgamma(cellpops[i]+1) + lgamma(arg) - 
		    lgamma(cellsizes[i]+2) - ncp_prior);
    }
  }

  return 0;
}

