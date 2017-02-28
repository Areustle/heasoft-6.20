#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "battblocks.h"


/* 
 * battblocks - task for computing various time blocks from event or
 * light curve data
 * 
 * Subroutines for data with Gaussian errors 
 *
 *   EXPERIMENTAL Cost function from Scargle (priv. comm.)
 *
 * C. Markwardt
 *
 * $Id: bblcgauss.c,v 1.17 2007/10/31 03:14:26 craigm Exp $
 *
 */

int logprob_lcgauss(double *cellparams, int nstride, int ncells, 
		    double *logprob, double ncp_prior, 
		    double data_range);

/* ============================================================= */
/* Convert a light curve to cells, more or less by direct
   transcription, except that special moments must be computed for use
   in the cost function. */
int lcgauss2cells(double *t, double *rate, double *rerror, int ntimes, 
		  double **cellparams, int *ncells, double *data_range, 
		  double tstart, double tstop, double timedel)
{
  double *cparams = 0;
  double maxd = 0, mind = 0;
  int nc, i;

  if ((t == 0) || (rate == 0) || (rerror == 0) || (ntimes <= 0) || 
      (cellparams == 0) || (tstop - tstart <= 0) || (data_range == 0)) {
    return 0;
  }

  *data_range = 0;
  nc = ntimes;

  cparams = (double *) malloc(NGAUSSCELLP*sizeof(double)*nc);
  if (cparams == 0) {
    return 0;
  }

  maxd = rate[0];
  mind = rate[0];

  for (i=0; i<nc; i++) {
    double r = rate[i], er2 = rerror[i];

    er2 = er2*er2;
    if (er2 > 0) {
      cparams[i+  0 ] = 0.5*(r*r/er2);
      cparams[i+  nc] =  -  (r/er2);
      cparams[i+2*nc] = 0.5*(1/er2);
    } else {
      cparams[i+  0 ] = 0;
      cparams[i+  nc] = 0;
      cparams[i+2*nc] = 0;
    }

    if (r > maxd) maxd = r;
    if (r < mind) mind = r;
  }

  *cellparams = cparams;
  *ncells    = nc;
  *data_range = maxd - mind;
  
  return 0;
}

/* ============================================================= */
/* Take an existing set of gaussian distributed bins, and rebin.  This
   involves averaging adjacent bins, weighting by their respective
   uncertainties. */
int rebinlcgauss(double *counts, double *error,
		 double **cellparams, double *data_range, 
		 int *cps, int ncps, double timedel)
{
  double *cparams = 0;
  double maxd = 0, mind = 0;
  int i, j;
  int nc = ncps-1;

  if (*cellparams) free(*cellparams);
  *cellparams = 0;
  *data_range = 0;

  cparams = (double *) malloc(NGAUSSCELLP*sizeof(double)*nc);
  if (cparams == 0) {
    return 0;
  }

  for (j=0; j<nc; j++) {
    double wi, mutot, wtot;
    double r, sig, er2;
    int ntot;

    /* Compute cumulants needed to get unbiased estimators for the
       mean and sigma of the points within the block. */
    mutot = 0; wtot = 0; ntot = 0;
    for (i=cps[j]; i<cps[j+1]; i++) {
      wi = 0;
      if (error[i]) wi = 1/(error[i]*error[i]);
      mutot += wi*counts[i];
      wtot  += wi;
      ntot  += 1;
    }

    /* Actually compute the estimators - counts per original bin */
    if (wtot) {
      r   = mutot / wtot;
      sig = (double) 1.0 / sqrt(wtot);
    } else {
      r   = 0.0;
      sig = 0.0;
    }

    /* Set min/max on first iteration */
    if (j == 0) maxd = mind = r;

    /* Determine min/max range */
    if (r > maxd) maxd = r;
    if (r < mind) mind = r;

    /* sigma^2, used in computing the moments */
    er2 = sig*sig;

    /* Now compute the cost function moments */
    if (er2) {
      cparams[j+  0 ] = 0.5*(r*r/er2);
      cparams[j+  nc] =  -  (r/er2);
      cparams[j+2*nc] = 0.5*(1/er2);
    } else {
      cparams[j+  0 ] = 0;
      cparams[j+  nc] = 0;
      cparams[j+2*nc] = 0;
    }
#if 0
    headas_chat(5, "  %5d %5d %15.5g %15.5g\n", j, ntot, r, sig);
#endif
  }    

  *cellparams = cparams;
  *data_range = maxd - mind;
  
  return 0;
}

/* ============================================================= */
/* Form the cumulative sum of the light curve, between two points.
 */
double *lcgausscumsum(double *dt, double *rate, double *error, 
		      int ntimes, char *hduclas3)
{
  int i;
  double *cumcounts = 0;
  double *cumerror  = 0;
  int is_rate = 0;

  cumcounts = (double *)malloc(sizeof(double)*ntimes * 2);
  if (cumcounts == 0) return 0;

  /* Tack the error bars at the end */
  cumerror = cumcounts + ntimes;

  if (strcasecmp(hduclas3, "COUNT") == 0) {
    is_rate = 0;
  } else {
    is_rate = 1;
  }

  /* Initial value */
  cumcounts[0] = rate[0];
  if (is_rate) { cumcounts[0] *= dt[0]; }  /* .. convert to counts */

  for (i=1; i<ntimes; i++) {
    double r = rate[i], e = error[i];

    if (is_rate) { 
      /* Convert to counts */
      r *= dt[i];
      e *= dt[i];
    }

    cumcounts[i] = cumcounts[i-1] + r;
    cumerror[i]  = cumerror[i-1]  + e*e;
  }    

  return cumcounts;
}



/* ============================================================= */
/* Determine the Bayesian block change points, based on the 
   Gaussian binned cost function */
int *lcgaussbayes(double *cellparams, int ncells, double data_range, 
		  double ncp_prior, int *ncparray, 
		  double **bestlogprob, int **lastcellstart, int nlag)
{
  double *cumparams = 0;
  int *last_start;
  int *cparray = 0;
  double *merged = 0, *best = 0;
  double temp;
  int i, j, imaxer, ncp, index, icp;
  int nc, nc2;
  double *sum_x_0, *sum_x_1, *sum_x_2;
  double *x_0, *x_1, *x_2;
  int istart = 0, ioldstart = 0;


  if (bestlogprob) *bestlogprob = 0;
  if (lastcellstart) *lastcellstart = 0;

  nc = ncells;
  nc2 = nc+nc;
  cumparams = (double *) malloc(NGAUSSCELLP*sizeof(double)*nc);
  last_start = (int *) malloc(sizeof(int)*nc);
  merged   = (double *) malloc(sizeof(double)*nc);
  best     = (double *) malloc(sizeof(double)*nc);
  if ((cumparams == 0) || (merged == 0) || 
      (best == 0) || (last_start == 0)) {
    if (cumparams) free(cumparams);
    if (merged) free(merged);
    if (best) free(best);
    if (last_start) free(last_start);
    return 0;
  }

  /* Declare some convenience pointers to... */
  x_2 = cellparams;      /* ...the data */
  x_1 = x_2 + nc;
  x_0 = x_1 + nc;

  sum_x_2 = cumparams;  /* ... and the accumulated data */
  sum_x_1 = sum_x_2 + nc;
  sum_x_0 = sum_x_1 + nc;

  /* Initialize cumulants to zero */
  for (i=0; i<nc*NGAUSSCELLP; i++) {
    cumparams[i] = 0;
  }

  istart = 0; ioldstart = 0;

  for (i=0; i<nc; i++) {
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
      sum_x_2[j] += x_2[i];
      sum_x_1[j] += x_1[i];
      sum_x_0[j] += x_0[i];
      /* Unfortunately, isinf() is not standard on every system */
      /* if (isinf(sum_x_0[j]) || isinf(sum_x_1[j]) || isinf(sum_x_2[j])) { */
      /*    fprintf(stderr, "ERROR: infinite sum\n"); */
      /* } */
	
    }
    sum_x_2[i] = x_2[i];
    sum_x_1[i] = x_1[i];
    sum_x_0[i] = x_0[i];

    /* Compute the cost function for the cumulants -- NOTE: the offset
       of 'istart' is okay for indexing each series, because the
       stride of 'nc' is used to walk between series. */
    logprob_lcgauss(cumparams+istart, nc, i-istart+1, 
		    merged+istart, ncp_prior, data_range);

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

#if 0
  /* Debugging output to a file */
  { 
    FILE *out;
    out = fopen("test.dat", "w");
    for (i=0; i<nc; i++) {
      fprintf(out, "%d %d %f\n", i, last_start[i], best[i]);
    }
    fclose(out);
  }
#endif

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
  if (cumparams) free(cumparams);
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

/* ============================================================= */
/*
 * logprob_lcgauss - Compute log posterior probability for gaussian data
 *
 * double *cellparams - cell parameters (NGAUSSCELLPxnstride array)
 * int nstride - number of storage cells 
 * int ncells - number of cell to compute (ncells <= nstride)
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
; datatype: 3 --> binned data, binned posterior
;-----------------------------------------------------------------
*/

/* MATLAB version - Scargle, priv. comm.
    log_prob = cc_log + 0.5 * log( pi ) ...
             - 0.5 * log( sum_x_0 ) ...
             + ( ( sum_x_1 ) .^ 2 ) ./ ( 4 * sum_x_0 ) ...
             - sum_x_2;

log_prob = log_prob  - ncp_prior; % prior on number of changepoints
*/

int logprob_lcgauss(double *cellparams, int nstride, int ncells, 
		    double *logprob, double ncp_prior, double data_range)
{
  double *sum_x_0, *sum_x_1, *sum_x_2;
  double log_cc = 0;
  double lp = -ncp_prior;
  static double logpi = 1.1447298858494002;   /* log(M_PI) */
  int i;

  /* log_cc is -log(data range), as suggested by Scargle */
  if (data_range > 0) log_cc = - log(data_range);

  sum_x_2 = cellparams;
  sum_x_1 = sum_x_2 + nstride;
  sum_x_0 = sum_x_1 + nstride;
  /* printf("=====\n"); */

  /* Based on revised Scargle paper, as of 29 Nov 2003 */
  for (i=0; i<ncells; i++) {
    /* printf("%f %f %f\n", sum_x_0[i], sum_x_1[i], sum_x_2[i]); */
    if (sum_x_0[i] > 0) {

      lp = log_cc + 0.5*logpi - 0.5*log(sum_x_0[i]) +
	(sum_x_1[i]*sum_x_1[i]) / (4 * sum_x_0[i]) - sum_x_2[i] - ncp_prior;
      logprob[i] = lp;
    } else {

      /* Fall-back case, when sum_x_0 is zero --> use previous value */
      logprob[i] = lp;
    }
  }

  return 0;
}

