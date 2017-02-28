#include <fitsio.h>
#include <math.h>
#include <string.h>
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "batcelldetect.h"
#include "mpfit.h"

/* 
 * batcelldetect
 * Source detection routines
 * 
 *
 *  22 May 2003 - split out from batcelldetect.c
 *
 * C. Markwardt 
 *
 * $Id: srcdetect.c,v 1.25 2010/06/22 22:40:52 craigm Exp $
 */

int  floodfill(struct image_struct *found, struct image_struct *image, 
	       struct image_struct *bkg, struct image_struct *bkgvar,
	       struct image_struct *nbkgpix, struct image_struct *mask,
	       int x, int y, int isrc,
	       struct source_struct *source, int nonullborder,
	       int maxsrcpix);
void src_summarize(struct source_struct *src, int status);

/* Add a source to the source detection list */
struct source_struct *addsource(struct source_struct *sources, 
				int *nsrc, int *nmax)
{
  int sz = sizeof(struct source_struct);

  if (*nmax == 0) {
    /* Initial call - allocate 20 spaces */
    *nmax = 20;
    *nsrc = 0;
    sources = (struct source_struct *) malloc(sz*(*nmax));
    if (sources) memset(sources, 0, sz*(*nmax));
  } else {
    /* Add new block of memory to source list */
    int nadd = (*nmax);           /* Number of sources to add = existing srcs*/
    
    if (*nsrc >= *nmax) {
      if (nadd < 20)   nadd = 20;   /* Don't add to few or too many */
      if (nadd > 1000) nadd = 1000;
      (*nmax) += nadd;
      sources = (struct source_struct *) realloc(sources, sz*(*nmax));
      if (sources) memset(sources+(*nsrc), 0, sz*nadd);
    }
  }

  if (sources) {
    /* Initialize with defaults */

    sources[*nsrc].imx = NULL_POS;
    sources[*nsrc].imy = NULL_POS;
    sources[*nsrc].imx_orig = NULL_POS;
    sources[*nsrc].imy_orig = NULL_POS;
    sources[*nsrc].imx_corr = NULL_POS;
    sources[*nsrc].imy_corr = NULL_POS;

    sources[*nsrc].ra_orig  = NULL_POS;
    sources[*nsrc].dec_orig = NULL_POS;
    sources[*nsrc].ra_corr  = NULL_POS;
    sources[*nsrc].dec_corr = NULL_POS;

    sources[*nsrc].first_x = NULL_POS;
    sources[*nsrc].first_y = NULL_POS;

    /* Default is null rather than zero */
    sources[*nsrc].contamflux = NULL_POS;  
    sources[*nsrc].bkgflux = NULL_POS;
    sources[*nsrc].bkgflux_cell = NULL_POS;
    sources[*nsrc].bkgflux_fit  = NULL_POS;

    (*nsrc) ++;
  } else {
    *nmax = 0;
  }

  return sources;
}

/* Delete a source[i] from the source detection list */
int delsource(struct source_struct *sources, int i,
	      int *nsrc, int *nmax)
{
  int j;

  if (i < 0 || i >= (*nsrc)) return -1;
  for (j = i+1; j<(*nsrc); j++) {
    sources[j-1] = sources[j];
  }
  *nsrc = *nsrc - 1;

  /* Zero the old last structure so that it is pristine */
  memset(&(sources[*nsrc]), 0, sizeof(struct source_struct));

  return 0;
}

/* Use the flood fill algorithm to find all pixels connected to this
   source */
int  floodfill(struct image_struct *found,
	       struct image_struct *image, 
	       struct image_struct *bkg,
	       struct image_struct *bkgvar,
	       struct image_struct *nbkgpix,
	       struct image_struct *mask,
	       int x, int y, int isrc,
	       struct source_struct *source,
	       int nonullborder, int maxsrcpix)
{
  FLOAT flux, bkgflux;
  int st = 0;

  bkgflux = bkg  ->datap[y][x];
  flux    = image->datap[y][x];
  /* printf("  image[%d,%d] = %f\n", x, y, flux); */

  /* Check for border cases */
  if (nonullborder) {
    /* If we border a 'null' pixel ... */
    if (flux == image->nullval) return ERR_NULLBORDER;
    if (mask->datap[y][x] == 0) return ERR_NULLBORDER;

    /* ... or if we are on the edge of the image, reject this source */
    if ((x == 0) || (x == (found->axes[0]-1)) ||
	(y == 0) || (y == (found->axes[1]-1))) {
      return ERR_NULLBORDER;
    }
  }
  /* Subtract the background */
  flux -= bkgflux;

  if (found->datap[y][x] == -1) {

    found->datap[y][x] = isrc;     /* Mark as found */
    source->xsum += x*flux;        /* Sum weighted by flux of ... X */
    source->ysum += y*flux;        /*  ... Y */
    source->xsum2 += x*x*flux;     /*  ... X^2 */
    source->ysum2 += y*y*flux;     /*  ... Y^2 */
    source->sumflux += flux;       /*  ... 1, i.e. sum of flux */
    source->sum2flux += flux*flux; /*  ... sum of flux^2 */
    source->npix ++;               /* Number of pixels in source */

    /* Return an error if we have exceeded the maximum number of
       pixels for this source.  Usually this indicates the signal to
       noise threshold is too low, and we are picking up noise. */
    if (source->npix > maxsrcpix) return ERR_MAXSRCPIX;

    if (fabs(flux) > fabs(source->peakflux)) {
      /* If this is the maximum flux so far detected, record it */
      source->xpeak = x;
      source->ypeak = y;
      source->peakflux = flux;
      source->bkgflux = bkgflux;
      source->bkgflux_cell = bkgflux;
      source->bkgvar = bkgvar->datap[y][x];
      source->nbkgpix = 0;
      if (nbkgpix) source->nbkgpix = nbkgpix->datap[y][x];
    }


    /* Now perform simple flood fill algorithm, scan right, left, up, down */
    if (x < (found->axes[0]-1))
      st = floodfill(found, image, bkg, bkgvar, nbkgpix, mask,
		     x+1, y, isrc, source, nonullborder, maxsrcpix);
    if ((st == 0) && (x > 0))
      st = floodfill(found, image, bkg, bkgvar, nbkgpix, mask,
		     x-1, y, isrc, source, nonullborder, maxsrcpix);
    if ((st == 0) && (y < (found->axes[1]-1)))
      st = floodfill(found, image, bkg, bkgvar, nbkgpix, mask,
		     x, y+1, isrc, source, nonullborder, maxsrcpix);
    if ((st == 0) && (y > 0))
      st = floodfill(found, image, bkg, bkgvar, nbkgpix, mask,
		     x, y-1, isrc, source, nonullborder, maxsrcpix);
  }

  return st;
}

/* Finalize computations of the source properties */
void src_summarize(struct source_struct *src, int status)
{
  int npix = src->npix;
  double sumflux = src->sumflux;

  /* Put source status */
  src->status = status;
  if (npix == 0 || src->sumflux <= 0) {
    src->status = ERR_NOPIX;
    return;
  }

  /* Position is weighted by the flux */
  src->xsum  /= sumflux;
  src->ysum  /= sumflux;
  src->xsum2 /= sumflux;
  src->ysum2 /= sumflux;

  /* Sigma of peak in pixels, minimum of 0.5 pixels */
  if (src->xsum2 > src->xsum*src->xsum)
    src->xsum2 = sqrt(src->xsum2 - src->xsum*src->xsum);
  else 
    src->xsum2 = 0.5;
  if (src->ysum2 > src->ysum*src->ysum)
    src->ysum2 = sqrt(src->ysum2 - src->ysum*src->ysum);
  else 
    src->ysum2 = 0.5;

  /* Average flux in peak, and variance */
  src->sumflux  /= npix;
  src->sum2flux /= npix;
  if (src->sum2flux > src->sumflux*src->sumflux)
    src->sum2flux = sqrt(src->sum2flux - src->sumflux*src->sumflux);
  else
    src->sum2flux = 0.0;

  /* Compute signal to noise ratio. Note: no need to perform
     background subtraction because it has already been subtracted in
     floodfill(). */
  if (src->bkgvar > 0) {
    src->snr = src->peakflux/src->bkgvar;
  } else {
    src->snr = 0;
  }

  /* Current best flux is the peak flux */
  src->bestflux = src->peakflux;

  /* Position conversion from C 0-based indices to FITS 1-based indices */
  src->xsum  += 1;   /* flux-weighted peak centroid */
  src->ysum  += 1;
  src->xpeak += 1;   /* pixel with the peak flux */
  src->ypeak += 1;

  return;
}


/* Scan image for new sources */
struct source_struct *flooddetect(struct detect_struct *detect,
				  int srcradius)
{
  struct image_struct *found, *image, *nbkgpix;
  struct image_struct *bkg, *bkgvar, *mask;
  struct source_struct *sources = 0;
  int nsrc = 0, nmax = 0, isrc;
  int status = 0;
  int nonullborder;
  int i, j;
  int maxsrcpix = MAXSRCPIX;

  if ((16 * srcradius * srcradius) > maxsrcpix) {
    maxsrcpix = 16 * srcradius * srcradius;
  }

  if (detect == 0) return 0;
  found = detect->found;
  image = detect->image;
  bkg = detect->bkgmap;
  bkgvar = detect->bkgvarmap;
  nbkgpix = detect->npix;
  sources = detect->sources;
  nsrc = detect->nsrcfound;
  nmax = detect->nsrcmax;
  mask = detect->mask;
  nonullborder = detect->nonullborder;
  headas_chat(5, " ... entering flooddetect with nonullborder=%d null=%f...\n", 
	      nonullborder, image->nullval);

  for (j=0; j<found->axes[1]; j++) {
    for (i=0; i<found->axes[0]; i++) {
      if (found->datap[j][i] == -1) {
	/* New source found! */
	sources = addsource(sources, &nsrc, &nmax);
	if (sources == 0) return 0;
	isrc = nsrc - 1;

	/* Find adjacent pixels */
	status = floodfill(found, image, bkg, bkgvar, nbkgpix, mask,
			   i, j, isrc, &sources[isrc], nonullborder,
			   maxsrcpix);
	/* Compute accumulated quantities */
	src_summarize(&sources[isrc], status);

	/* Require at least 4 adjacent detected pixels, and possibly
	   non-border pixel.  For these sources, we completely
	   obliterate the source. */
	if ((sources[isrc].npix < detect->nadjpix) ||
	    (sources[isrc].status == ERR_NULLBORDER)) {

	  /* First check adjacent pixels */
	  if (sources[isrc].npix < detect->nadjpix) {
	    headas_chat(5, "  (rejected source at (%d,%d) for too few pixels [%d < %d])\n",
		      sources[isrc].xpeak, sources[isrc].ypeak,
		      sources[isrc].npix, detect->nadjpix);
	  } else {
	    headas_chat(5, "  (rejected source at (%d,%d) for being on a border\n",
			sources[isrc].xpeak, sources[isrc].ypeak);
	  }

	  memset(&(sources[isrc]), 0, sizeof(sources[isrc]));
	  nsrc--;
	  continue;
	}

	sources[isrc].name[0] = 0;
	sources[isrc].sourceid = -1;
	sources[isrc].err_rad = -1;  /* Default behavior */

	sources[isrc].method = METH_CELL; /* Detection method: cell detection */
	
	/* If we found a problem, quit */
	if (sources[isrc].status != 0 && sources[isrc].status != ERR_MAXSRCPIX) {
	  detect->nsrcfound = nsrc;
	  detect->sources = sources;
	  detect->nsrcmax = nmax;
	  return sources;
	}

      }
    }
  }
  
  detect->nsrcfound = nsrc;
  detect->sources = sources;
  detect->nsrcmax = nmax;
  return sources;
}
