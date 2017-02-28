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
 * Pixel detection routines
 * 
 *
 *  22 May 2003 - split out from batcelldetect.c
 *
 * C. Markwardt 
 *
 * $Id: pixdetect.c,v 1.26 2010/05/06 01:08:16 craigm Exp $
 */

/* Function prototype */
struct image_struct *bkgwindow(struct parm_struct *parms);
struct image_struct *srcwindow(struct parm_struct *parms);

/*
 * pixdetect - perform pixel detection algorithm
 * 
 * struct parm_struct *parms
 *   ->niter - number of detection iterations to perform (in)
 *   ->npixthresh - minimum number of pixels (in)
 *   ->snrthresh - detection threshold, sigma (in)
 *   ->hduclas2 - from input image, HDUCLAS2 keyword value (in)
 *   ->statistics - image statistics code (in)
 *   ->pospeaks - if true, only find positive excesses (in)
 *   ->{src,bkg}windowrad - source,background window radius, pixels (in)
 *   ->bkgwindowtype - background window type, CIRCLE or SQUARE (in)
 *   ->nonullborder - accept sources that touch the border? (in)
 *
 * struct detect_struct *detect
 *   ->image - flux map (in)
 *   ->bkgmask - mask map for determining background bias+noise (in)
 *             - mask map with detected sources removed (out)
 *   ->mask - mask map for source detection (in)
 *   ->bkgmap - a priori background map (in)
 *              calculated mean background map (out)
 *       (NOTE: if input background is specified then it is not recalculated)
 *   ->bkgvarmap - a priori noise map (in)
 *                 calculated noise map (out)
 *       (NOTE: if input noise is specified then it is not recalculated)
 *   ->npixfound - number of new excess pixels found (out)
 *   ->found - map of found pixels (out)
 *   ->snrmap - map of signal to noise ratio (out)
 *   ->npix - map of number of pixels used at each point for bkg (out)
 *   ->bkgmap - map of mean background level (out)
 *
 * RETURNS: 0
 */
int pixdetect(struct parm_struct *parms,
	      struct detect_struct *detect)
{
  struct image_struct *image = 0;
  struct image_struct *mask = 0, *bkgmask = 0, *sum = 0, *sum2 = 0, *npix = 0;
  struct image_struct *kernel = 0, *fkernel = 0, *skernel = 0, *sfkernel = 0;
  struct image_struct *sky = 0;
  struct image_struct *found = 0;
  struct image_struct *bkgmap = 0, *bkgvarmap = 0;
  int nx, ny, npthresh, iter, nfound = 0, noldfound = 0;
  float snrthresh = 3.0, peakvar = 0.0;
  int compute_signifmap = 0;
  int niter;

  image = detect->image;
  bkgmask = detect->bkgmask;
  mask = detect->mask;
  bkgvarmap = detect->bkgvarmap;
  bkgmap = detect->bkgmap;

  niter = parms->niter;
  /* No need to iterate if the background is fully specified */
  if (bkgvarmap && bkgmap) niter = 1;

  nx = image->axes[0];
  ny = image->axes[1];

  /* Make background window kernel */
  kernel = bkgwindow(parms);
  skernel = srcwindow(parms);
  npthresh = parms->npixthresh;
  snrthresh = parms->snrthresh;

  sky   = image_init(nx, ny, 0); /* Temporary array */
  found = image_init(nx, ny, 0); /* Pixel detection array */
  foreach_pixel(found, i, {found->data[i] = -2;});
  detect->found = found;

  /* Blank out the already-known sources */
  if (strcmp(parms->hduclas2,"SIGNIFICANCE") != 0) {
    compute_signifmap = 1;
  } else {
    /* No need to iterate if the significance map is given already */
    niter = 1;
  }
  compute_signifmap = 1; /* XXX For now, always compute significance map */
  blanksources(parms, detect);
  nfound = detect->npixfound;

  for(iter=0; iter<niter; iter++) {

    headas_chat(2, "    Detection Iteration %d\n", iter+1);
    /* Convolve number of pixels */
    if (npix) image_free(npix);
    npix = 0;

    if (compute_signifmap) {
      /* ---- Must compute a significance map from the flux map */
      if (bkgmap == 0 || bkgvarmap == 0) {
	headas_chat(5, "   (computing npix map)\n");
	npix = image_convolve(bkgmask, kernel, &fkernel);
      }

      /* Convolve sky map to get background sum, then divide by
	 num. pixels to get mean background.  */
      if (bkgmap) { 
	headas_chat(5, "   (using precomputed bkgmap)\n");
	sum = bkgmap;
      } else {
	headas_chat(5, "   (computing bkgmap)\n");
	foreach_pixel(sky, i, {sky->data[i] = image->data[i]*bkgmask->data[i];});
	if (sum) image_free(sum);
	sum = image_convolve(sky, kernel, &fkernel);

	foreach_pixel(npix, i, 
          {if (npix->data[i] > npthresh) sum->data[i] /= npix->data[i]; 
	   else                          sum->data[i] = 0; 
	  });
      }

      /* Compute sky map squared */
      if (bkgvarmap) {
	headas_chat(5, "   (using precomputed bkgvarmap)\n");
	/* Pre-computed variance map */
	sum2 = bkgvarmap;
      } else if (parms->statistics == STATGAUSS) { 
	headas_chat(5, "   (computing bkgvarmap - gaussian statistics)\n");
	/* Gaussian statistics */
	foreach_pixel(sky, i, sky->data[i] = 
		      image->data[i]*image->data[i]*bkgmask->data[i]);
	if (sum2) image_free(sum2);
	sum2 = image_convolve(sky, kernel, &fkernel);
	
	/* Standard deviation of background -> sky */
	foreach_pixel(sum2, i, 
 	  { FLOAT y = 0;
 	    if (npix->data[i] > npthresh) 
	      y = (sum2->data[i]/npix->data[i]) - sum->data[i]*sum->data[i];
	    if (y > 0) sum2->data[i] = sqrt(y);
	    else       sum2->data[i] = 0.0;
	  });
      } else {
	headas_chat(5, "   (computing bkgvarmap - poisson statistics)\n");

	/* Poisson statistics = sqrt(counts)/npix = sqrt(sum/npix); */
	if (sum2) image_free(sum2);
	sum2 = image_copy(sum);
	foreach_pixel(sum2, i, 
   	   { if (npix->data[i] > npthresh) {
	       if (sum->data[i] > 0) { sum2->data[i] = sqrt(sum2->data[i]); }
	       else { sum2->data[i] = 1.0 / sqrt(npix->data[i]); }
	     } else { sum2->data[i] = 0; }
	   });
      }

      /* Find peak background variance */
      peakvar = 0.0;
      foreach_pixel(sky, i, if (sum2->data[i] > peakvar) peakvar=sum2->data[i];);
      headas_chat(5, "   (peak noise level=%f)\n", peakvar);
      /* If there is no variance, the image must be blank.  Bail out. */
      if (peakvar == 0) break;
      
      /* Signal to noise ratio, (data - bkg)/e_bkg
       */
      if (parms->statistics == STATGAUSS) {
	foreach_pixel(sky, i, 
          { 
	    sky->data[i] = 0;
	    if (sum2->data[i] > 0) {
	      sky->data[i] = ((image->data[i] - sum->data[i])/sum2->data[i]);
	    }
	  });
      } else {
	/* Poisson statistics */
	struct image_struct *npixs = 0;

	if (sky) image_free(sky);
	sky = image_convolve(image, skernel, &sfkernel);
	npixs = image_convolve(mask, skernel, &sfkernel);
	
	foreach_pixel(sky, i, 
	{ 
	  FLOAT ss = sky->data[i];
	  FLOAT np = npixs->data[i];
	  FLOAT vs = sum2->data[i];
	  FLOAT sigma2 = ss/np/np+vs*vs;
	  
	  if (np > 0 && sigma2 > 0) {
	    sky->data[i] = (ss/np - sum->data[i])/sqrt(sigma2);
	  } else {
	    sky->data[i] = 0;
	  }
	});
	image_free(npixs);
      }

      /* End of compute_signifmap */
    } else {
      /* ---- Significance map is already made -- copy it */
      if (sum == 0)  sum  = image_copy(sky);
      if (sum2 == 0) sum2 = image_copy(sky);
      foreach_pixel(sky, i, sky->data[i] = image->data[i];);
      foreach_pixel(sum2, i, sum2->data[i] = 1.0;);
      foreach_pixel(sum,  i, sum->data[i]  = 0.0;);
      peakvar = 1.0;

    }

    headas_chat(5,"  ...threshold search...\n");
    /* Perform threshold search */
    if (parms->pospeaks) {
      /* Search for POSITIVE only peaks */
      foreach_pixel(sky, i,
      { if ((bkgmask->data[i] != 0) && (sky->data[i] >= snrthresh) && (mask->data[i]>0)) {
	bkgmask->data[i] = 0;   /* Remove found source from next search */
	found->data[i] = -1; /* Add new source to found image */
	nfound++;
      }});
    } else {
      /* Search for POSITIVE or NEGATIVE peaks */
      foreach_pixel(sky, i,
      { if ((bkgmask->data[i] != 0) && (fabs(sky->data[i]) >= snrthresh) && (mask->data[i]>0)) {
	bkgmask->data[i] = 0;   /* Remove found source from next search */
	found->data[i] = -1; /* Add new source to found image */
	nfound++;
      }});
    }
    headas_chat(2,"        Found %d cumulative pixels\n", nfound);

    /* No need to continue iterations if we haven't found any new
       pixels. */
    if (nfound == noldfound) break;
    noldfound = nfound;
  }

  /* Clean up space (IMAGE mask, kernel, fkernel) */
  image_free(kernel);  kernel = 0;
  image_free(fkernel); fkernel = 0;
  if (skernel) image_free(skernel); skernel = 0;
  if (sfkernel) image_free(sfkernel); sfkernel = 0;
  
  detect->mask   = mask;
  detect->bkgmask= bkgmask;
  detect->snrmap = sky;
  detect->npix   = npix;
  detect->found  = found;
  detect->npixfound = nfound;
  detect->bkgmap = sum;
  detect->bkgvarmap = sum2;
  
  return 0;
}

/* 
 * bkgwindow - Create background window kernel
 * 
 * struct parm_struct *parms 
 *   ->{src,bkg}windowrad - source,background window radius, pixels (in)
 *   ->bkgwindowtype - background window type, CIRCLE or SQUARE (in)
 * 
 * RETURNS: kernel map (caller responsible to free)
 *   size of window will be (2*BKGWINDOWRAD+1) on each side
 *   kernel will be centered at position map->data[br][br]
 *     where br = BKGWINDOWRAD
 * 
 */
struct image_struct *bkgwindow(struct parm_struct *parms)
{
  int nx, ny;    /* Dimensions of image */
  int i, j, r2;
  int br, br2;   /* Background radius, squared */
  int sr, sr2;   /* Source radius, squared */
  int xc, yc;    /* Center of image */
  int wtype;
  struct image_struct *window;
  
  br = parms->bkgwindowrad;
  br2 = br*br;
  sr = parms->srcwindowrad;
  sr2 = sr*sr;
  wtype = parms->bkgwindowtype;

  /* X diameter of background window = 2*bkgwindowrad + 1 */
  nx = 2*br + 1;
  if (nx < 3) nx = 3;      /* Enforce limits */
  if (nx > 201) nx = 201;

  if (wtype == SMOOTH_CIRCLE) {
    nx = 4*br + 1;
    xc = 2*br;
    yc = 2*br;
  } else {
    xc = br;
    yc = br;
  }
    
  ny = nx;
  window = image_init(nx, ny, 0);
  if (window == 0) return 0;

  if (wtype == SMOOTH_CIRCLE) {
    /* ===== Smoothed Circular Region ===== */
    /*   The shape: 
     *    * overall the shape is the same as the circular region, but
     *        with tapering so that the window itself contributes
     *        fewer artifacts to the noise map.
     *     * inner edge is smoothed with tanh() function, and scale-width
     *         of 0.1*(inner radius)
     *     * outer edge is smoothed with tanh() function, and scale-width
     *         of 0.2*(inner radius)
     */
    double maxf = 0;

    for (j=0; j<ny; j++) {
      for (i=0; i<nx; i++) {
	double r, f;
	/* Compute radius from the center at (2*br,2*br)*/
	r2 = (j-2*br)*(j-2*br) + (i-2*br)*(i-2*br);
	r = sqrt(r2);

	/* Smooth decay from peak response */
	f = (1-tanh((r-br)/(br*0.2)))/2.0;
	/* Smooth increase from 0 at source radius (if any) */
	if (sr > 0) { f *= (1+tanh((r-sr)/(sr*.1)))/2.0; }
	window->datap[j][i] = f;
	if (f > maxf) { maxf = f; } /* Keep track of peak value */
      }
    }

    /* Rescale so that peak value is 1.0 */
    if (maxf > 0) {
      for (j=0; j<ny; j++) {
	for (i=0; i<nx; i++) {
	  window->datap[j][i] /= maxf;
	}
      }
    }
    
  } else if (wtype == CIRCLE) { 
    /* ===== Circular Region ===== */
    for (j=0; j<ny; j++) {
      for (i=0; i<nx; i++) {
	/* Compute radius from the center at (br,br) */
	r2 = (j-yc)*(j-yc) + (i-xc)*(i-xc);
	if (r2 <= br2) window->datap[j][i] = 1;
      }
    }
  } else { 
    /* ===== Square Region ===== */
    foreach_pixel(window, i, {window->data[i] = 1;});
  }

  /* Punch out the center of the region where the PSF should sit */
  if (wtype != SMOOTH_CIRCLE && sr > 0) {
    for (j=0; j<ny; j++) {
      for (i=0; i<nx; i++) {
	/* Again, compute the radius */
	r2 = (j-yc)*(j-yc) + (i-xc)*(i-xc);
	if (r2 <= sr2) window->datap[j][i] = 0;
      }
    }
  }

  return window;
}


/* 
 * srcwindow - Create source window kernel
 *
 * struct parm_struct *parms 
 *   ->srcwindowrad - source window radius, pixels (in)
 *
 * RETURNS: kernel map (caller responsible to free)
 *   size of window will be (2*SRCWINDOWRAD+1) on each side
 *   kernel will be centered at position map->data[sr][sr]
 *     where sr = SRCWINDOWRAD
 */
struct image_struct *srcwindow(struct parm_struct *parms)
{
  int nx, ny;
  int i, j;
  int r2, sr, sr2;
  struct image_struct *window;
  
  sr = parms->srcwindowrad;
  sr2 = sr*sr;

  /* X diameter of background window = 2*srcwindowrad + 1 */
  nx = 2*sr + 1;
  if (nx < 3) nx = 3;      /* Enforce limits */
  if (nx > 201) nx = 201;
  ny = nx;
  window = image_init(nx, ny, 0);
  if (window == 0) return 0;

  /* Construct circular source region */
  for (j=0; j<ny; j++) {
    for (i=0; i<nx; i++) {
      /* Compute radius from the center (squared) */
      r2 = (j-sr)*(j-sr) + (i-sr)*(i-sr);
      if (r2 <= sr2) window->datap[j][i] = 1;
    }
  }

  return window;
}


/* 
 * blanksources - blank out known sources from background map
 *
 * struct parm_struct *parms
 *   ->srcwindowrad - source window radius, pixels (in)
 *   ->nonullborder - accept sources that touch the border? (in)
 * 
 * struct detect_struct *detect
 *   ->mask - mask map for source detection (in)
 *   ->bkgmask - mask map for determining background bias+noise (in)
 *             - mask map with detected sources removed (out)
 *   ->found - map of found pixels (out)
 *             each found pixel will be given a sequential number
 *             a value of -1 indicates no excess
 *   ->npixfound - number of new excess pixels found (out)
 *   ->sources - list of known sources (in)
 *      if source touches border, status will be changed to ERR_NULL_BORDER
 *   ->nsrcfound - number of elements in 'sources' (in)
 * 
 * RETURNS: 0
 */
int blanksources(struct parm_struct *parms, struct detect_struct *detect)
{
  struct source_struct *sources;
  struct image_struct *found = 0, *mask = 0, *bkgmask = 0;
  int nx, ny;
  int i, j, k;
  int sr, sr2, r2;
  int ixpix, iypix, ixmin, ixmax, iymin, iymax;
  int npixfound = detect->npixfound;
  int nborder = 0;

  sources = detect->sources;
  found = detect->found;
  bkgmask = detect->bkgmask;
  mask = detect->mask;

  nx = bkgmask->axes[0];
  ny = bkgmask->axes[1];

  /* Blank out the mask around sources which are already "found", say
     from an input catalog. */
  if (detect->nsrcfound > 0) {
    sr = parms->srcwindowrad;
    sr2 = sr*sr;

    for (i=0; i<detect->nsrcfound; i++) {
      int del = 0;
      /* Convert from FITS 1-based coordinates to C 0-based coordinates */
      ixpix = rint(sources[i].xsum-1);
      iypix = rint(sources[i].ysum-1);

      ixmin = ixpix - sr;
      ixmax = ixpix + sr;
      iymin = iypix - sr;
      iymax = iypix + sr;
      /* Flag sources that actually straddle the edge of the image */
      if (parms->nonullborder && sources[i].status >= 0 &&
	  (ixmin < 0 || ixmax >= nx || iymin < 0 || iymax >= ny)) {
	sources[i].status = ERR_NULLBORDER;
      }
      if (ixmin < 0)   ixmin = 0;
      if (ixmax >= nx) ixmax = nx-1;
      if (iymin < 0)   iymin = 0;
      if (iymax >= ny) iymax = ny-1;

      /* First scan this source's region to be sure it doesn't touch
         the border of the image; here "border" includes touching a
         masked region of the image. */
      if (sources[i].status >= 0) for (k=iymin; k<=iymax; k++) {
	for (j=ixmin; j<=ixmax; j++) {
	  /* Check circular region around source ... compute radius */

	  r2 = (k-iypix)*(k-iypix) + (j-ixpix)*(j-ixpix);
	  if (r2 <= sr2) {

	    /* Pixel is within the source window radius
	       ... but is bad if it touches the border of the image */
	    if (parms->nonullborder && mask->datap[k][j] == 0) {
	      sources[i].status = ERR_NULLBORDER;
	    }
	  }
	}
      }

      /* Now mark the pixels as being "bad" for background
	 determination, and if the source is OK, then also mark the
	 pixels as "found" for source analysis */
      for (k=iymin; k<=iymax; k++) {
	for (j=ixmin; j<=ixmax; j++) {
	  /* Check circular region around source ... compute radius */

	  r2 = (k-iypix)*(k-iypix) + (j-ixpix)*(j-ixpix);
	  if (r2 <= sr2) {

	    /* Pixel is within the source window radius; 
	       add source as "found" and remove it from the mask */

	    if (found->datap[k][j] < 0) {

	      if (sources[i].status >= 0) {
		/* Mark pixels as "found" */
		detect->npixfound ++;
		found->datap[k][j] = i;
	      } else {
		nborder ++;
	      }

	      /* ... and "bad" for background analysis */
	      bkgmask->datap[k][j] = 0;
	      del = 1;
	    }
	  }
	}      
      }      

      /* 
      headas_chat(5, "  (source %d blanked=%s)\n",
		  i, del ? "yes" : "no");
      */

    } /* for i */
    
  }
  headas_chat(5, "  ... blanksources (nsrcfound=%d npixblanked=%d nborder=%d)...\n", 
	      detect->nsrcfound, detect->npixfound - npixfound, nborder);

  return 0;
}


#if 0
/* For debugging purposes, to determine if an image has bogus values */
int check_image(struct image_struct *im, char *name)
{
  int nx, ny;
  int ok = 1;

  nx = im->axes[0];
  ny = im->axes[1];
  
  foreach_pixel(im, i, { if (isnan(im->data[i]) || isinf(im->data[i]) || (im->data[i] < -1e20) ) { headas_chat(5,"%s[%d,%d]: %f\n", name, i % nx, i / nx, im->data[i]); ok = 0; } } );

  return ok;
}
#endif
