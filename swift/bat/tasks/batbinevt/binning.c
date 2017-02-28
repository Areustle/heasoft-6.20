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
 * Routines for binning events different ways
 *
 *  11 Apr 2003 - split out from batbinevt.c
 *
 * $Id: binning.c,v 1.14 2008/03/14 14:51:01 craigm Exp $
 *
 * C. Markwardt 
 */


/* ----------------------------------------------------------------- */
/* Initialize the UNIFORM binning 
   spect - spectrum to be initialized
   parms - task parameters (uses tbinsize)
   RETURNS - 0 upon success, an error code otherwise
*/
int uniform_init(struct spectrum_struct *spect, 
		 struct parm_struct *parms)
{
  spect->totcounts = 0;
  spect->badcounts = 0;
  spect->tbinsize =  parms->tbinsize;
  if (spect->tbinsize == 0) 
    spect->tbinsize = spect->tstop - spect->tstart;
  spect->ntbins = ceil((spect->tstop - spect->tstart)/spect->tbinsize);
  if (spect->ntbins <= 0) spect->ntbins = 1;
  spect->ntmax = spect->ntbins;

  return init_spect(spect);
}

/* ----------------------------------------------------------------- */
/* Perform the UNIFORM binning 
   spect - spectrum to accept new data
   tev - array of event times
   tend - array of event end times (if applicable, 0 otherwise)
   ebin - array of event energy bin numbers
   weights - array of mask weights
   n - size of arrays (tev, tend, ebin, weights, segs, detx, dety, split, ncounts)
   segs - array of good time interval index numbers
   detx - array of X pixel numbers
   dety - array of Y pixel numbers
   split - array of "split" numbers (if applicable, 0 otherwise)
   ncounts - array of count numbers (if applicable, 0 otherwise)

   RETURNS - 0 upon success, an error code otherwise
*/
int uniform_bin(struct spectrum_struct *spect, 
		double *tev, double *tend, int *ebin, float *weights, int n,
		int *segs, int *detx, int *dety, int *split, float *ncounts)
{
  int i;
  float nn;
  int tbin, tendbin, ind;
  double wt, wt2;
  int nebins, nbins, ntbins;
  int accum_wt2;

  if ((spect == 0) || (tev == 0) || (ebin == 0) || (n == 0) || (segs == 0))
    return 0;

  nebins = spect->nebins;
  ntbins = spect->ntbins;
  nbins = spect->nbins;
  accum_wt2 = spect->accum_wt2;

  for(i=0; i<n; i++) {
    nn = 1;
    if (ncounts) {
      nn = ncounts[i];
      if (nn == 0) continue;
    }

    tbin = floor((tev[i]-spect->tstart)/spect->tbinsize);

    if ((tbin >= 0) && (tbin < ntbins) && 
	(ebin[i] >= 0) && (weights[i] != 0) && (segs[i] >= 0)) {

      /* Check that the start and end bin are identical (this may not
	 be true for a DPH). */
      if (tend) {
	tendbin = floor((tend[i]-spect->tstart)/spect->tbinsize);
	if (tbin != tendbin) return -1;
      }

      ind = tbin*nbins + ebin[i];
      if (split) ind += split[i]*nebins;
      wt = weights[i];
      wt2 = wt*wt;
      if (ncounts) {
	wt *= ncounts[i];
	wt2 *= ncounts[i];
      }

      spect->counts[tbin]  += nn;
      spect->weights[ind]  += wt;
      if (accum_wt2) spect->weights2[ind] += wt2;
      spect->totweights[tbin]  += wt;
      spect->totweights2[tbin] += wt2;
      spect->totcounts += nn;

    } else {
      spect->badcounts += nn;
    }
      
  }

  return 0;
}


/* ----------------------------------------------------------------- */
/* Initialize the USERBIN spectrum 
   spect - spectrum to be initialized
   parms - task parameters (uses ugti)
   RETURNS - 0 upon success, an error code otherwise
*/
int userbin_init(struct spectrum_struct *spect, 
		 struct parm_struct *parms)
{
  int i, status = 0;
  
  spect->totcounts = 0;
  spect->badcounts = 0;
  spect->tbinsize = 0;

  spect->ntbins = parms->ugti.ngti;
  spect->ntmax = spect->ntbins;
  /* headas_chat(5, "  (user spectrum ntbins=%d)\n", spect->ntbins); */
  
  status = init_spect(spect);

  /* Fill the time arrays with user GTI start/stop time */
  if (status == 0) {
    for (i=0; i<parms->ugti.ngti; i++) {
      spect->times[i] = parms->ugti.start[i];
      spect->tends[i] = parms->ugti.stop[i];
    }
  }

  return status;
}

/* ----------------------------------------------------------------- */
/* Perform the USERBIN binning
   spect - spectrum to accept new data
   tev - array of event times
   tend - array of event end times (if applicable, 0 otherwise)
   ebin - array of event energy bin numbers
   weights - array of mask weights
   n - size of arrays (tev, tend, ebin, weights, segs, detx, dety, split, ncounts)
   segs - array of good time interval index numbers
   detx - array of X pixel numbers
   dety - array of Y pixel numbers
   split - array of "split" numbers (if applicable, 0 otherwise)
   ncounts - array of count numbers (if applicable, 0 otherwise)
   ugti - the "user GTI" used for binning

   RETURNS - 0 upon success, an error code otherwise
*/
int userbin_bin(struct spectrum_struct *spect, 
		double *tev, double *tend, int *ebin, float *weights, int n,
		int *segs, int *detx, int *dety, int *split, float *ncounts,
		struct gti_struct *ugti)
{
  int i;
  float nn;
  int tbin, ind;
  int nebins, nbins, ntbins;
  double wt, wt2;
  int accum_wt2;
  int status = 0;

  if ((spect == 0) || (tev == 0) || (ebin == 0) || (n == 0) || (segs == 0) ||
      (ugti == 0)) {
    return 0;
  }

  nebins = spect->nebins;
  ntbins = spect->ntbins;
  nbins = spect->nbins;
  accum_wt2 = spect->accum_wt2;

  /* Decide which *user* bin the data belongs to.  Note that this is
     different that the *master GTI* bin that it belongs to, since the
     master GTI can be merged and otherwise mucked with.  The user-GTI
     is preserved unblemished. 

     NOTE: OVERWRITES detx!!!
  */
  HDgti_where(ugti, n, tev, detx, &status);
  /* Copy over new segment info into old, but only if original segment
     was good. */
  for (i=0; i<n; i++) {
    if (segs[i] >= 0) segs[i] = detx[i];
  }

  /* If there are end-points for the inputs, then check that they
     match the start points.  We error-out if there are any DPHs which
     would span an output bin. */
  if (tend) {
    HDgti_where(ugti, n, tend, detx, &status);
    for (i=0; i<n; i++) {
      if (segs[i] >= 0 && segs[i] != detx[i]) return -1;
    }
  }
  /* headas_chat(5, "   (user gti n=%d tev[0]=%f segs[0]=%d status=%d)\n", 
     n, tev[0], segs[0], status); */
  

  for(i=0; i<n; i++) {
    nn = 1;
    if (ncounts) {
      nn = ncounts[i];
      if (nn == 0) continue;
    }
    tbin = segs[i];

    if ((tbin >= 0) && (tbin < ntbins) && 
	(ebin[i] >= 0) && (weights[i] != 0) && (tbin >= 0)) {

      ind = tbin*nbins + ebin[i];
      if (split) ind += split[i]*nebins;
      wt = weights[i];
      wt2 = wt*wt;
      if (ncounts) {
	wt *= ncounts[i];
	wt2 *= ncounts[i];
      }

      spect->counts[tbin]  += nn;
      spect->weights[ind]  += wt;
      if (accum_wt2) spect->weights2[ind] += wt2;
      spect->totweights[tbin]  += wt;
      spect->totweights2[tbin] += wt2;
      spect->totcounts += nn;
    } else {
      spect->badcounts += nn;
    }
      
  }

  return 0;
}




/* ----------------------------------------------------------------- */
/* Initialize the CONSTSNR binning 
   spect - spectrum to be initialized
   parms - task parameters (uses tbinsize)
   RETURNS - 0 upon success, an error code otherwise
*/
int constsnr_init(struct spectrum_struct *spect, 
		  struct parm_struct *parms)
{
  int status = 0;

  spect->totcounts = 0;
  spect->badcounts = 0;
  spect->tbinsize =  parms->tbinsize;
  spect->ntbins = 1;
  spect->ntmax = 0;

  /* Estimate the number of time bins ... */
  if (spect->tbinsize > 0) {
    spect->ntmax = ceil((spect->tstop - spect->tstart)/spect->tbinsize);
    if (spect->ntmax <= 0) spect->ntmax = 1;
  }
  /* ... but at least 500 time bins */
  if (spect->ntmax < 500) spect->ntmax = 500;

  status = init_spect(spect);
  spect->times[0] = spect->tstart;

  return status;
}

/* ----------------------------------------------------------------- */
/* Perform CONSTSNR binning 
   spect - spectrum to accept new data
   tev - array of event times
   tend - array of event end times (if applicable, 0 otherwise)
   ebin - array of event energy bin numbers
   weights - array of mask weights
   n - size of arrays (tev, tend, ebin, weights, segs, detx, dety, split, ncounts)
   segs - array of good time interval index numbers
   detx - array of X pixel numbers
   dety - array of Y pixel numbers
   split - array of "split" numbers (if applicable, 0 otherwise)
   ncounts - array of count numbers (if applicable, 0 otherwise)
   mode - binning mode (CONSTSNR or MINSNR)

   RETURNS - 0 upon success, an error code otherwise
*/
int constsnr_bin(struct spectrum_struct *spect, 
		 double *tev, double *tend, int *ebin, float *weights, int n,
		 int *segs, int *detx, int *dety, int *split, float *ncounts,
		 int mode)
{
  int i;
  float nn;
  int tbin, ind, seg;
  double tmin, tbinsize;
  double tcut = 0;
  int nebins, nbins;
  double wt, wt2;
  int status = 0;
  int lasttseq;
  int accum_wt2;
  int snr_done, tbin_done;

  if ((spect == 0) || (tev == 0) || (ebin == 0) || (n == 0) || (segs == 0))
    return 0;

  tbin = spect->ntbins-1;        /* Current bin number */
  tmin = spect->times[tbin];     /* Current bin start time */
  nbins = spect->nbins;          /* Number of bins */
  nebins = spect->nebins;        /* Number of energy bins */
  tbinsize = spect->tbinsize;    /* Minimum time bin size */
  accum_wt2 = spect->accum_wt2;  /* Currently accumulated weight */

  for(i=0; i<n; i++) {
    nn = 1;

    /* If an explicit number of counts was specified, then use that as
       a multiplier */
    if (ncounts) {
      nn = ncounts[i];
      /* Short-circuit if no counts found */
      if ((nn == 0) && (i != (n-1))) continue;
    }

    /* Short circuits if the energy or time are out of bounds */
    if (ebin[i] < 0)           goto OUT_OF_BOUNDS;
    if (tev[i] > spect->tstop) goto OUT_OF_BOUNDS;

    /* Add the current event to the bin */
    if ((tev[i] >= tmin) && (weights[i] != 0)) {

      ind = tbin*nbins + ebin[i];        /* Time index */
      if (split) ind += split[i]*nebins; /*  .. add "split" */
      wt = weights[i];
      wt2 = wt*wt;
      
      if (ncounts) {
	/* Multiply by number of counts */
	wt  *= ncounts[i];
	wt2 *= ncounts[i];
      }
      seg = segs[i];

      /* Initialize lastseg when we get our first photon */
      if (spect->totcounts == 0) {
	spect->lastseg = seg;
	spect->lasttime = tev[i];
      }

      /* Make sure, if times are repeated, that this is the last time
         in the sequence of repeats, before closing a bin.  The end of
         a buffer always permits a bin-end. */
      lasttseq = (tev[i] != spect->lasttime);
      /* lasttseq = (i == (n-1)) || (tev[i] != tev[i+1]);
	 lasttseq |= ((i == 0) && (tev[i] != spect->lasttime)); */

      /* Advance to next bin if SNR threshold is met */
      /* Note: the threshold is met based on the mask weighted signal,
         not the total number of counts received */
      /* Also check if we have crossed a GTI boundary, and start a new
         bin if so. */

      /* SNR threshold met */
      snr_done = (spect->totweights[tbin]*spect->totweights[tbin] > 
		  spect->snr2*spect->totweights2[tbin]);

      if (mode == CONSTSNR) {
	if (tbinsize > 0) {
	  tbin_done = (snr_done || (tev[i] - tmin > tbinsize));
	} else {
	  tbin_done = (snr_done);
	}
      } else {
	if (tbinsize > 0) {
	  tbin_done = (snr_done && (tev[i] - tmin > tbinsize));
	} else {
	  tbin_done = (snr_done);
	}
      }

      if ( lasttseq && ( tbin_done || (seg != spect->lastseg) )) {

	/* Compute cut-off point for this bin (depends on whether we
	   know when the input ends or not). */
	if (tend == 0) {
	  tcut = tev[i];
	} else {
	  tcut = tend[i];
	}

	spect->tends[tbin] = tcut;                  /* End old bin ... */
	if (spect->counts[tbin]) spect->ntbins ++;  /* ... and begin new bin */
	tbin = spect->ntbins-1;
	spect->times[tbin] = tcut;
	tmin = tev[i];
	spect->lastseg = seg;

	/* If we have reached the end of the spectrum, we need to add
	   more bins at the end */
	if (spect->ntbins == spect->ntmax) {
	  int newtbins = spect->ntmax; /* Number of bins to *add* */
	  if (newtbins > 100000) newtbins = 100000; /* Don't grow too fast */
	  
	  newtbins += spect->ntmax;    /* Add original num of bins */
	  if ( (status = expand_spect(spect, newtbins)) ) {
	    return status;
	  }
	}
      }

      spect->lasttime = tcut;
      if (seg >= 0){
	/* Increment the weights, if in a valid time segment */
	spect->counts[tbin]  += nn;
	spect->weights[ind]  += wt;
	if (accum_wt2) spect->weights2[ind] += wt2;
	spect->totweights[tbin]  += wt;
	spect->totweights2[tbin] += wt2;
	spect->totcounts += nn;
      }

    } else {
      /* Handle case of out-of-order events */
    OUT_OF_BOUNDS:
      spect->badcounts += nn;
    }
  }

    return 0;
}


