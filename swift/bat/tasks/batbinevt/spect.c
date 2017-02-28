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
 * Routines for allocating, de-allocating and manipulating spectra 
 *
 *  11 Apr 2003 - split out from batbinevt.c
 *
 * $Id: spect.c,v 1.6 2003/08/04 04:20:00 craigm Exp $
 * C. Markwardt 
 */


/* 
   Format of spectral data stored in spect->weights and ->weights2:

     Energies     x      Spatial/Other x      Time
     nebins       x      nsplits       x      ntmax
   = (          nbins               )  x      ntmax

   i.e., energy is the most rapidly varying index, the "split" is the
   next most rapidly varying, and time is the least rapidly varying.

   The "split" is any other binning that might be done, usually
   according to spatial position on the detector plane.

   The value nbins = (nebins * nsplits) is the total number of bins
   per time sample.

*/
   

/* ----------------------------------------------------------------- */
/* Free allocated memory associated with spectrum */
int free_spect(struct spectrum_struct *spect)
{
  headas_chat(5, "  ...freeing spectrum...\n");
  if (spect == 0) return NULL_INPUT_PTR;
  if (spect->times) free(spect->times);
  if (spect->tends) free(spect->tends);
  if (spect->exposures) free(spect->exposures);
  if (spect->counts) free(spect->counts);
  if (spect->totweights) free(spect->totweights);
  if (spect->totweights2) free(spect->totweights2);
  if (spect->weights) free(spect->weights);
  if (spect->weights2) free(spect->weights2);

  /* Reset pointers and status values to 0 */
  memset(spect, 0, sizeof(*spect));

  return 0;
}

/* ----------------------------------------------------------------- */
/* Allocate and initialize a spectrum structure to a pristine state */
int init_spect(struct spectrum_struct *spect)
{
  int i, j;
  int ntmax;
  int nebins;
  int nsplits;
  int nbins;
  int accum_wt2;

  if (spect == 0)
    return NULL_INPUT_PTR;

  ntmax = spect->ntmax;    
  nebins = spect->nebins;  
  nsplits = spect->nsplits;
  accum_wt2 = spect->accum_wt2;

  headas_chat(5, "  ...initializing spectrum w/ %d time and %d energy and %d other bins...\n",
	      ntmax, nebins, nsplits);

  spect->totcounts = 0;
  spect->badcounts = 0;

  spect->times = 0;
  spect->tends = 0;
  spect->exposures = 0;
  spect->counts = 0;
  spect->totweights = 0;
  spect->totweights2 = 0;
  spect->weights = 0;
  spect->weights2 = 0;

  if ((ntmax <= 0)||(nebins <= 0)||(nsplits <= 0)) return 0;
  nbins = nebins*nsplits;

  headas_chat(5, "  ...allocating memory...\n");
  spect->times = (double *) malloc(sizeof(double)*ntmax);
  spect->tends = (double *) malloc(sizeof(double)*ntmax);
  spect->exposures = (double *) malloc(sizeof(double)*ntmax);
  spect->counts = (long *) malloc(sizeof(long)*ntmax);
  spect->totweights = (double *) malloc(sizeof(double)*ntmax);
  spect->totweights2 = (double *) malloc(sizeof(double)*ntmax);
  spect->weights = (double *) malloc(sizeof(double)*ntmax*nbins);
  if (accum_wt2) {
    spect->weights2 = (double *) malloc(sizeof(double)*ntmax*nbins);
  }

  if ((spect->times == 0) || (spect->tends == 0) || 
      (spect->exposures == 0) ||
      (spect->counts == 0) || 
      (spect->totweights == 0) || (spect->totweights2 == 0) ||
      (spect->weights == 0) || (accum_wt2 && (spect->weights2 == 0))) {
    fprintf(stderr, "ERROR: could not allocate memory for spectrum\n");
    free_spect(spect);

    return MEMORY_ALLOCATION;
  }
  spect->nbins = nbins;
  spect->nebins = nebins;
  spect->ntmax = ntmax;
  spect->nsplits = nsplits;
  spect->accum_wt2 = accum_wt2;
  spect->lastseg = -1;
  spect->lasttime = 0;

  headas_chat(5, "  ...zeroing memory...\n");
  for (i=0; i<ntmax; i++) {
    spect->times[i] = spect->tstart + i*spect->tbinsize; /* Start time */
    spect->tends[i] = spect->times[i] + spect->tbinsize; /* End time   */
    spect->counts[i] = 0;
    spect->totweights[i] = 0;
    spect->totweights2[i] = 0;
    for (j=0; j<nbins; j++) {
      spect->weights[j+i*nbins] = 0;
    }
    if (accum_wt2) for (j=0; j<nbins; j++) {
      spect->weights2[j+i*nbins] = 0;
    }
  }
  /* spect->tends[ntmax-1] = spect->tstop; */

  return 0;
}




/* ----------------------------------------------------------------- */
/* Expand memory of spectrum to a new size */
int expand_spect(struct spectrum_struct *spect, int newtbins)
{
  int i, j;
  int ntmax = spect->ntmax;
  int nbins = spect->nbins;
  int accum_wt2;

  headas_chat(5, "  ...expanding spectrum to %d time bins...\n", newtbins);

  if (newtbins <= ntmax) return 0;
  accum_wt2 = spect->accum_wt2;

  /* Reallocate memory for spectrum using new size */
  headas_chat(5, "  ...re-allocating memory...\n");
  spect->times = (double *) realloc(spect->times,sizeof(double)*newtbins);
  spect->tends = (double *) realloc(spect->tends,sizeof(double)*newtbins);
  spect->exposures = (double *) realloc(spect->exposures,sizeof(double)*newtbins);
  spect->counts = (long *) realloc(spect->counts,
				   sizeof(long)*newtbins);
  spect->totweights = (double *) realloc(spect->totweights,
					 sizeof(double)*newtbins);
  spect->totweights2 = (double *) realloc(spect->totweights2,
					  sizeof(double)*newtbins);
  spect->weights = (double *) realloc(spect->weights,
				      sizeof(double)*newtbins*nbins);
  if (accum_wt2) {
    spect->weights2 = (double *) realloc(spect->weights2,
					 sizeof(double)*newtbins*nbins);
  }

  if ((spect->times == 0) || (spect->tends == 0) || 
      (spect->exposures == 0) ||
      (spect->counts == 0) || 
      (spect->totweights == 0) || (spect->totweights2 == 0) ||
      (spect->weights == 0) || (accum_wt2 && (spect->weights2 == 0))) {
    fprintf(stderr, "ERROR: could not dynamically expand spectrum\n");
    free_spect(spect);

    return MEMORY_ALLOCATION;
  }

  headas_chat(5, "  ...zeroing memory...\n");
  for (i=ntmax; i<newtbins; i++) {
    spect->times[i] = spect->tstart + i*spect->tbinsize;
    spect->tends[i] = spect->times[i] + spect->tbinsize;
    spect->counts[i] = 0;
    spect->totweights[i] = 0;
    spect->totweights2[i] = 0;
    for (j=0; j<nbins; j++) {
      spect->weights[j+i*nbins] = 0;
    }
    
    if (accum_wt2) for (j=0; j<nbins; j++) {
      spect->weights2[j+i*nbins] = 0;
    }
  }
  /* spect->tends[newtbins-1] = spect->tstop; */
  spect->ntmax = newtbins;

  return 0;
}
