#include <fitsio.h>
#include <math.h>
#include <string.h>   /* Only source detection */
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "batmask.h"
#include "batdet.h"
#include "bat_gswdev.h"

#include "coordfits.h"
#include "batfftimage.h"

static char taskname[] = "batfftimage";
static char taskver[]  = "1.20";


/* 
 * Task to reconstruct sky image from BAT detector plane image
 *
 *   C. Markwardt
 *   Nov 2002
 *
 * $Id: batfftimage.c,v 1.122 2009/08/21 18:24:42 craigm Exp $
 *
 * 16 Dec 2002 - add near-field imaging code (CM)
 * 17 Dec 2002 - more near-field imaging corrections (plate scale) (CM)
 * 17 Jan 2003 - correct bugs for far-field imaging introduced by
 *               near-field code.  More debugging statements (CM)
 * 02 Mar 2003 - add RA/DEC WCS keywords
 *
 */

#define TOOLSUB batfftimage
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* ------------------------------------------------------------------------- */
/* Read parameter file */
int batfftimage_getpar(struct parm_struct *parms) 
{
  int status = 0;
  char handstr[FLEN_CARD];
  char pcodemapstr[FLEN_CARD];
  char bkgvartypestr[FLEN_CARD];
  char keepbitstr[FLEN_CARD];

  parms->infile[0] = 0;
  parms->attitude[0] = 0;
  parms->aperture[0] = 0;
  parms->detmask[0] = 0;
  parms->outfile[0] = 0;
  parms->bkgfile[0] = 0;

  parms->maskoff[0] = 0;  /* Offset of mask from nominal */
  parms->maskoff[1] = 0;
  parms->maskoff[2] = 0;

  parms->srcpos[0] = 0;   /* Source position */
  parms->srcpos[1] = 0;
  parms->srcpos[2] = 0;

  parms->origin[0] = 0;   /* Origin of angular coordinates w.r.t. BAT origin */
  parms->origin[1] = 0;
  parms->origin[2] = 0;

  parms->oversampx = 2;
  parms->oversampy = 2;
  parms->pcodethresh = 0;
  parms->pcodemap = 0;
  parms->rebalance = 1;
  parms->append = 0;
  parms->corrections[0] = 0;
  parms->cautocollim = 0;
  parms->teldef[0] = 0;
  parms->goodval = 0;
  parms->copyall = 1;
  parms->time = 0;

  if ((status = PILGetString("infile", parms->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");

  else if ((status = PILGetFname("outfile", parms->outfile)))
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");

  else if ((status = PILGetString("attitude", parms->attitude)))
    fprintf(stderr, "Error reading the 'attitude' parameter.\n");

  else if ((status = PILGetString("aperture", parms->aperture)))
    fprintf(stderr, "Error reading the 'aperture' parameter.\n");

  else if ((status = PILGetString("bkgfile", parms->bkgfile)))
    fprintf(stderr, "Error reading the 'bkgfile' parameter.\n");

  else if ((status = PILGetString("detmask", parms->detmask)))
    fprintf(stderr, "Error reading the 'detmask' parameter.\n");
  
  else if ((status = PILGetReal("maskoffx", &parms->maskoff[0])))
    fprintf(stderr, "Error reading the 'maskxoff' parameter.\n");

  else if ((status = PILGetReal("maskoffy", &parms->maskoff[1])))
    fprintf(stderr, "Error reading the 'maskyoff' parameter.\n");

  else if ((status = PILGetReal("maskoffz", &parms->maskoff[2])))
    fprintf(stderr, "Error reading the 'maskzoff' parameter.\n");

  else if ((status = PILGetReal("bat_z", &parms->srcpos[2])))
    fprintf(stderr, "Error reading the 'bat_z' parameter.\n");

  else if ((status = PILGetReal("origin_z", &parms->origin[2])))
    fprintf(stderr, "Error reading the 'origin_z' parameter.\n");

  else if ((status = PILGetInt("oversampx", &parms->oversampx)))
    fprintf(stderr, "Error reading the 'oversampx' parameter.\n");

  else if ((status = PILGetInt("oversampy", &parms->oversampy)))
    fprintf(stderr, "Error reading the 'oversampy' parameter.\n");

  else if ((status = PILGetBool("rebalance", &parms->rebalance)))
    fprintf(stderr, "Error reading the 'rebalance' parameter.\n");

  else if ((status = PILGetReal("pcodethresh", &parms->pcodethresh)))
    fprintf(stderr, "Error reading the 'pcodethresh' parameter.\n");

  else if ((status = PILGetString("pcodemap", pcodemapstr)))
    fprintf(stderr, "Error reading the 'pcodemap' parameter.\n");

  else if ((status = PILGetBool("append", &parms->append)))
    fprintf(stderr, "Error reading the 'append' parameter.\n");

  else if ((status = PILGetString("corrections", parms->corrections)))
    fprintf(stderr, "Error reading the 'corrections' parameter.\n");

  else if ((status = PILGetString("keepbits", keepbitstr)))
    fprintf(stderr, "Error reading the 'keepbits' parameter.\n");

  else if ((status = PILGetString("handedness", handstr)))
    fprintf(stderr, "Error reading the 'handedness' parameter.\n");

  else if ((status = PILGetString("rows", parms->rowstr)))
    fprintf(stderr, "Error reading the 'rows' parameter.\n");

  else if ((status = PILGetBool("copyall", &parms->copyall)))
    fprintf(stderr, "Error reading the 'copyall' parameter.\n");

  else if ((status = PILGetString("countscol", parms->countscol)))
    fprintf(stderr, "Error reading the 'countscol' parameter.\n");

  else if ((status = PILGetString("teldef", parms->teldef)))
    fprintf(stderr, "Error reading the 'teldef' parameter.\n");

  else if ((status = PILGetReal("maskwtswgain", &parms->maskwtswgain)))
    fprintf(stderr, "Error reading the 'maskwtswgain' parameter.\n");
  
  else if ((status = PILGetString("bkgvarmap", parms->bkgvarmap)))
    fprintf(stderr, "Error reading the 'bkgvarmap' parameter.\n");

  else if ((status = PILGetString("signifmap", parms->signifmap)))
    fprintf(stderr, "Error reading the 'signifmap' parameter.\n");

  else if ((status = PILGetString("bkgvartype", bkgvartypestr)))
    fprintf(stderr, "Error reading the 'bkgvartype' parameter.\n");

  else if ((status = PILGetReal("time", &parms->time)))
    fprintf(stderr, "Error reading the 'time' parameter.\n");


  /* Default case for DETMASK parameter */
  if (strcasecmp(parms->detmask, "none") == 0) {
    parms->detmask[0] = 0;
  }

  /* Check pcodemap string */
  if ((strcasecmp(pcodemapstr,"y") == 0)||(strcasecmp(pcodemapstr,"yes") == 0)) {
    parms->pcodemap = PCODEMAP_FILE;  /* Output file is partial coding map only */
  } else if ((strcasecmp(pcodemapstr,"n") == 0)||(strcasecmp(pcodemapstr,"no") == 0)) {
    parms->pcodemap = FLUXMAP;  /* No partial coding map at all */
  } else if (strcasecmp(pcodemapstr,"append_all") == 0) {
    parms->pcodemap = PCODEMAP_APPEND;  /* Append all partial coding maps */
  } else if (strcasecmp(pcodemapstr,"append_last") == 0) {
    parms->pcodemap = PCODEMAP_LAST;  /* Append only last partial coding map */
  } else {
    fprintf(stderr, "ERROR: pcodemap must be YES, NO, APPEND_LAST or APPEND_ALL\n");
    return -1;
  }

  if (strncasecmp(bkgvartypestr,"VAR",3) == 0) {
    parms->bkgvartype = BKGVARMAP;
  } else if (strncasecmp(bkgvartypestr,"STD",3) == 0) {
    parms->bkgvartype = STDDEVMAP;
  } else {
    fprintf(stderr, "ERROR: bkgvartype must be VARIANCE or STDDEV\n");
    return -1;
  }

  parms->keepbits = 0;
  if (strcasecmp(keepbitstr,"ALL") == 0) {
    parms->keepbits = 0;
  } else {
    parms->keepbits = atoi(keepbitstr);
    if (parms->keepbits <= 0 || parms->keepbits > 64) {
      fprintf(stderr, "WARNING: 'keepbits' value of '%s' was not parseable \n",
	      keepbitstr);
      fprintf(stderr, "         into an integer in the range 1-64.  Assuming\n");
      fprintf(stderr, "         keepbits=ALL\n");
      parms->keepbits = 0;
    }
  }


  /* Defaults for the variance and significance maps */
  if (strcasecmp(parms->bkgvarmap, "none") == 0) {
    parms->bkgvarmap[0] = 0;
  }
  if (strcasecmp(parms->signifmap, "none") == 0) {
    parms->signifmap[0] = 0;
  }
  


  /* Check for handedness of image */
  if (strcasecmp(handstr, "left") == 0) {
    parms->handedness = HANDEDNESS_LEFT;
  } else if (strcasecmp(handstr, "right") == 0) {
    parms->handedness = HANDEDNESS_RIGHT;
  } else {
    fprintf(stderr, "ERROR: handedness must be either 'left' or 'right'\n");
    return -1;
  }

  /* Input file, either a DPI file, or NONE in the case of making a
     partial coding map. */
  if (strcasecmp(parms->infile, "none") == 0) {
    if (parms->pcodemap == FLUXMAP) {
      fprintf(stderr, 
	      "ERROR: infile can be 'NONE' only when making a \n"
	      "       partial coding map (pcodemap=YES)\n");
      return -1;
    }
    parms->infile[0] = 0;
  }

  /* Attitude file, or NONE for no RA/DEC info */
  if (strcasecmp(parms->attitude, "none") == 0) {
    parms->attitude[0] = 0;
  }

  /* Background image file, or NONE for no subtraction */
  if (strcasecmp(parms->bkgfile, "none") == 0) {
    parms->bkgfile[0] = 0;
  }

  /* Check for default value of teldef file */
  if (strcasecmp(parms->teldef,"none") == 0) {
    parms->teldef[0] = 0;
  }
  if ((parms->teldef[0] == 0) && (parms->attitude[0] != 0)) {
    fprintf(stderr, "WARNING: cannot assign RA/DEC coordinates unless BOTH\n");
    fprintf(stderr, "         attitude and teldef files are specified.\n");
    fprintf(stderr, "         ---> DISABLING RA/DEC COORDINATES\n");
    parms->attitude[0] = 0;
  }

  /* Parse the corrections parameter */
  parms->cautocollim = 0;
  parms->pcodecorr = 0;
  parms->cflatfield = 0;
  parms->ccosine = 0;
  parms->cmaskwt = 0;
  parms->cndet = 0;
  headas_chat(5, "...corrections='%s'...\n", parms->corrections);
  if ((status == 0) && parms->corrections[0] != 0) {
    if (strstr(parms->corrections, "none")) {
      parms->cautocollim = 0;
    }
    if (strstr(parms->corrections, "default")) {
      parms->cflatfield = 1;
      parms->pcodecorr = 1;
      parms->cautocollim = 1;
      parms->cmaskwt = 1;
      parms->cndet = 1;
    }
    /* If autocollimation correction is to be applied */
    if (strstr(parms->corrections, "autocollim")) {
      parms->cautocollim = 1;
    }
    if (strstr(parms->corrections, "maskwt")) {
      parms->cmaskwt = 1;
    }
    /* If partial coding correction is to be applied */
    if (strstr(parms->corrections, "pcode")) {
      parms->pcodecorr = 1;
    }
    /* If flat fielding correction is to be applied */
    if (strstr(parms->corrections, "flatfield")) {
      parms->cflatfield = 1;
    }
    /* If flat fielding correction is to be applied */
    if (strstr(parms->corrections, "cosine")) {
      parms->ccosine = 1;
    }
    /* If to be normalized by the number of active detectors */
    if (strstr(parms->corrections, "ndets")) {
      parms->cndet = 1;
    }      
  }

  /* Sanity check the corrections */
  if (parms->ccosine && parms->cflatfield) {
    fprintf(stderr, "ERROR: you can't specify both 'cosine' and 'flatfield' corrections\n");
    return -1;
  }



  /* Counts column default */
  if (strcasecmp(parms->countscol, "INDEF") == 0) {
    parms->countscol[0] = 0;
  }

  return status;
}

/* ------------------------------------------------------------------------- */
/* Print a standard banner for the user */
void banner(struct parm_struct *parms)
{
  char *pcodemapstr;
  char keepbitstr[FLEN_CARD];

  headas_chat(2, "******************************************\n");
  headas_chat(1, "         %s v%s\n", parms->taskname, parms->taskver);
  headas_chat(2, "------------------------------------------\n");
  if (parms->infile[0]) 
    headas_chat(2, "     Input Image: %s\n", parms->infile);
  else
    headas_chat(2, "     Input Image: NONE\n");
  if (strcmp(parms->rowstr,"-") != 0)
    headas_chat(2, " Image Selection: %s\n", parms->rowstr);

  if (parms->bkgfile[0]) 
    headas_chat(2, " Background File: %s\n", parms->bkgfile);
  else
    headas_chat(2, " Background File: NONE\n");
  
  if (parms->attitude[0]) 
    headas_chat(2, "   Attitude File: %s\n", parms->attitude);
  else
    headas_chat(2, "   Attitude File: NONE\n");
  if (parms->teldef[0]) 
    headas_chat(2, "     TelDef File: %s\n", parms->teldef);
  else
    headas_chat(2, "     TelDef File: NONE\n");
  
  headas_chat(2, "  Aperture Image: %s\n", parms->aperture);
  headas_chat(2, "    Output Image: %s%s\n", parms->outfile,
	      (parms->append)?"   (append)":"");
  if (parms->detmask[0])
    headas_chat(2, "   Detector Mask: %s\n", parms->detmask); 
  else
    headas_chat(2, "   Detector Mask: NONE\n");
  if ((parms->maskoff[0] != 0) || (parms->maskoff[1] != 0) ||
      (parms->maskoff[2] != 0)) {
    headas_chat(2, "    Mask Offsets: %+7.3f X   %+7.3f Y   %+7.3f Z (cm)\n",
		parms->maskoff[0], parms->maskoff[1], parms->maskoff[2]);
  }
  if (parms->srcpos[2] != 0) 
    headas_chat(2, "    Source BAT_Z: %+7.3f (cm)\n", parms->srcpos[2]);
  if (parms->origin[2] != 0)
    headas_chat(2, "BAT_Z Ang Origin: %+7.3f (cm)\n", parms->origin[2]);
  headas_chat(2, "    Oversampling: %2d X   %2d Y\n", 
	      parms->oversampx, parms->oversampy);
  switch(parms->pcodemap) {
  case FLUXMAP: pcodemapstr = "NO"; break;
  case PCODEMAP_FILE: pcodemapstr = "YES"; break;
  case PCODEMAP_APPEND: pcodemapstr = "APPEND_LAST"; break;
  case PCODEMAP_LAST: pcodemapstr = "APPEND_ALL"; break;
  default: pcodemapstr = "UNKNOWN"; break;
  }
  headas_chat(2, "  Partial Coding: %s    Threshold: %f\n", 
	      pcodemapstr, parms->pcodethresh);
  headas_chat(2, "Rebalance Images: %3s\n", 
	      (parms->rebalance)?"YES":"NO ");
  sprintf(keepbitstr,"keepbits=%d ",parms->keepbits);
  if (parms->keepbits == 0) keepbitstr[0] = 0;
  headas_chat(2, "     Corrections: autocollim=%s handedness=%s pcode=%s flatfield=%s %s\n", 
	      (parms->cautocollim)?"YES":"NO ",
	      (parms->handedness == HANDEDNESS_LEFT)?"left ":"right",
	      (parms->pcodecorr)?"YES":"NO ",
	      (parms->cflatfield)?"YES":"NO ",
	      keepbitstr);
  headas_chat(2, "------------------------------------------\n");
}

/* ------------------------------------------------------------------------- */
/* Final summary */
void summary(struct parm_struct *parms, int nimages)
{
  headas_chat(2, "    Sky images computed: %d\n", nimages);
  headas_chat(2, "------------------------------------------\n");
}

/* ------------------------------------------------------------------------- */

/*
 * fft_convolve - main image deconvolution work routine 
 *
 * struct image_struct *focal - pointer to focal plane image
 * struct image_struct *detmask - pointer to detector quality map
 * struct image_struct *aperture - pointer to aperture (mask) map
 * struct parm_struct *parms - pointer to parameter structure
 *                     parms->oversampx/y - oversampling of image in x/y
 *                     parms->rebalance - rebalance focal & aperture? (1=yes)
 *                                        (ignored when pcodemap == 1)
 * int anynull - are there null values in focal? (1=yes; 0=no)
 * FLOAT nullval - null values in focal must be flagged with this value
 * double x/yrescale - rescaling factors based on image distance
 * int pcodemap - make partial coding map? (1=yes; 0=no)
 * struct image_struct ***aperture_fft - precomputed aperture array pointer
 *   There are three cases:
 *           aperture_fft   *aperture_fft [upon input]
 *      (a)     NULL            N/A         NO precomputations requested
 *      (b)    non-NULL         NULL        Initial computation requested
 *      (c)    non-NULL        non-NULL     Re-use existing computations
 * 
 *   For case (a), the aperture arrays are computed internally and
 *   then discarded.
 *
 *   For case (b), the user is requesting the aperture FFTs be
 *   computed for the first time.  These can be used in subsequent
 *   calls to fft_convolve.  Upon return, *aperture_fft will point to
 *   an array of struct image_struct * pointers, of size
 *   parms->oversampx * parms->oversampy. 
 * 
 *   For case (c), the user is requesting to use an existing set of
 *   precomputed aperture FFTs, as returned by a call to case (b).
 *
 *   After the final usage, the * user is required to free each of
 *   these images using image_free(), and the pointer array.
 *
 * int *ndetson - upon return, *ndetson contains the number of
 *     non-NULL pixel values.
 * int *status - CFITSIO status
 *
 * RETURNS: pointer to image_struct structure, containgin the
 * deconvolved sky image.  The user is responsible for freeing this
 * image with image_free().
 *
 */
struct image_struct *fft_convolve(struct image_struct *focal,
				  struct image_struct *detmask,
				  struct image_struct *aperture,
				  struct parm_struct *parms,
				  int anynull, FLOAT nullval,
				  double xrescale, double yrescale,
				  int pcodemap,
				  struct image_struct ***aperture_fft,
				  int *ndetson, int msksqd, 
				  int *status)
{
  int ndet;
  struct image_struct *detmaski = detmask;
  struct image_struct *focres = 0;
  struct image_struct *result = 0;
  struct image_struct *outimg = 0;
  int copied = 0;
  struct image_struct **apres = 0;
  int ntot = parms->oversampx * parms->oversampy;
  int noverx = parms->oversampx, novery = parms->oversampy;
  int nxx, nyy;
  int nxo, nyo;
  int i, j;
  int ii, jj, kk;

  if (aperture_fft) apres = (*aperture_fft);

  /* Count number of enabled detectors */
  ndet = 0;
  *ndetson = 0;

  foreach_pixel(detmask, i, {if (detmask->data[i] != 0) ndet++;});
  headas_chat(5, "  (non-quality-masked detectors = %d)\n", ndet);

  /* Search the image for any NULL values.  Apply the detector quality
     map to the data.  Rebalance the images if necessary. 

     NOTE: this block allocates a temporary detmask (called detmaski),
     which is necessary because, if many images are processed, the
     NULLs in one image should not affect another image.
  */
  if (anynull) {
    int nnulls = 0;
    
    headas_chat(5, "...masking out null pixels...\n");
    detmaski = image_init(detmask->axes[0], detmask->axes[1], 0);
    if (detmaski == 0) {
      fprintf(stderr, "ERROR: memory allocation failed for detmask\n");
      *status = MEMORY_ALLOCATION;
      return 0;
    }
    foreach_pixel(detmaski, i, {detmaski->data[i] = detmask->data[i];});
    copied = 1;

    /* Remove NULL Pixels */
    foreach_pixel(focal, i, {if (focal->data[i] == nullval) 
      {detmaski->data[i] = 0; nnulls++;}});
    headas_chat(5,"  (found %d nulls in focal-background image)\n", nnulls);
  }

  /* Apply the mask to the data */
  foreach_pixel(focal, i, {focal->data[i] *= detmaski->data[i];});

  /* Re-balance the aperture and the focal plane map (except when
     getting partial coding map or if using the squared mask) */
  if (parms->rebalance && pcodemap == FLUXMAP && !msksqd) {
    headas_chat(5, "...rebalancing images...\n");
    image_balance(aperture, aperture);
    image_balance(focal, detmaski);
  }
  
  /* Count (the revised) number of enabled detectors */
  ndet = 0;
  foreach_pixel(detmaski, i, {if (detmaski->data[i] != 0) ndet++;});
  headas_chat(5, "  (revised non-quality-masked detectors = %d)\n", ndet);
  
  if (pcodemap != FLUXMAP) {
    foreach_pixel(focal, i, {focal->data[i] = detmaski->data[i];});
  }

  /* Free the temporary detmask only if we allocated a new one */
  if (copied) image_free(detmaski);

  /* Find the next highest powers of two, because the FFT demands it */
  nxx = image_next2pown(aperture->axes[0]*xrescale + focal->axes[0]);
  nyy = image_next2pown(aperture->axes[1]*yrescale + focal->axes[1]);

  if (apres == 0) {
    headas_chat(5,"...allocating memory for apertures...\n");
    apres = (struct image_struct **) malloc(sizeof(struct image_struct*)*ntot);
    if (apres == 0) {
      fprintf(stderr, "ERROR: memory allocation failed for apres\n");
      *status = MEMORY_ALLOCATION;
      return 0;
    }
    apres[0] = 0;
  }

  /* Rescale the aperture, with different X and Y offsets */
  /*  Note : this blurs the mask 
      and so its important to do the squaring AFTER this */
  if (apres[0] == 0) {
    int jj, kk, ii;
    headas_chat(5, "...computing shifted aperture images...\n");

    ii = 0;
    for (jj=0; jj<novery; jj++) {
      for (kk=0; kk<noverx; kk++) {
	apres[ii] = image_rescale(aperture, xrescale, yrescale, 
				  (double)kk/noverx, (double)jj/novery, 
				  nxx, nyy);
	if (msksqd) {
	  foreach_pixel(apres[ii], i, { apres[ii]->data[i] *= apres[ii]->data[i]; });
	}
	ii++;
      }
    }

    headas_chat(5, "...computing Fourier transforms of aperture...\n");
    for (ii = 0; ii<ntot; ii++) {
      image_fft(apres[ii]);  /* Transforms of aperture(s)...*/
    }
  }
  /* Save for future calculations... */
  if (aperture_fft) {
    *aperture_fft = apres;
    headas_chat(5, "...saving aperture transforms for later...\n");
  }

  /* Simple way to fill the detector plane image into the 1024x512 image */
  headas_chat(5, "... inserting focal map into padded array ... \n");
  headas_chat(5, "    (%d,%d) image - at insertion point (%d,%d)\n",
	      nxx, nyy, nxx-focal->axes[0], nyy-focal->axes[1]);
  focres = image_rescale(focal, 1.0, 1.0, 
			 nxx-focal->axes[0], nyy-focal->axes[1], nxx, nyy);

  headas_chat(5, "...computing Fourier transforms of focal map...\n");
  image_fft(focres);                        /*  ... and focal plane */

  /* Create master output arrays */
  result = image_init(nxx, nyy, 0);
  nxo = ceil(xrescale*aperture->axes[0]) + focal->axes[0];
  nyo = ceil(yrescale*aperture->axes[1]) + focal->axes[1];

  outimg = image_init(nxo*noverx+4, nyo*novery+4, 0);

  /* Copy metadata of input image to output image */
  image_copyinfo(outimg, focal);

  /* Main deconvolution loop */
  headas_chat(5, "...deconvolving images...\n");
  kk = 0; jj = 0;
  for (ii=0; ii<ntot; ii++) {
    /* Construct result = apres * conj(focres) */
    image_cmult(result, apres[ii], focres);
    if (parms->rebalance && pcodemap == FLUXMAP && !msksqd) {
      /* Force the output to be zero-mean */
      result->data[0] = 0;  
    }

    image_fft(result);  /* Inverse transform back to spatial domain */

    /* Shuffle this subsample into the master output array */
    for (j=0; j<nyo; j++) {
      int off, offi, offj;

      offi = noverx - kk;
      offj = novery - jj;
      
      off = offi + (offj+noverx*j)*outimg->axes[0];
      for (i=0; i<nxo; i++) {
	outimg->data[off+noverx*i] = result->datap[j][i];
      }
    }

    kk ++;
    if (kk == noverx) {
      kk = 0;
      jj++;
    }
  }

  image_free(focres);
  image_free(result);
  if (aperture_fft == 0) {
    for (ii=0; ii<ntot; ii++) {
      image_free(apres[ii]);
    }
  }

  *ndetson = ndet;
  return outimg;
}

/* ------------------------------------------------------------------------- */

/* 
 * Main work routine 
 *
 */
int batfftimage_work(struct parm_struct *parms)
{
  struct image_struct *aperture = 0, *focal = 0, *vardat = 0;
  struct image_struct **apres = 0, **pcode_apres = 0, **msksqd_apres = 0;
  struct image_struct *focres = 0, *detmask = 0;
  struct image_struct *result = 0, *outimg = 0, *pcodeimg = 0;
  struct image_struct *varimg = 0, *signifimg = 0;
  struct batmaskplane_struct mask;
  struct batdetplane_struct detplane;
  struct wcs_coord wcs;
  fitsfile *imgfile, *dpifile = 0, *outfile = 0, *bkgfile = 0;
  fitsfile *varfile = 0, *signiffile = 0;
  int ihdu_out = 0, ihdu_signif = 0, ihdu_var = 0;
  int *phdu_signif, *phdu_var;
  int nimages = 0, nbkgimages = 0, imgnum = 0, ngoodimages = 0;
  int hdutype, bkg_hdutype;
  int msksqd = 0;

  /* Status variables for variance/significance/keepbits calculations */
  int keepbits = 0;
  int varwarned = 0;

  double xrescale, yrescale;
  double mwfcorr; /* Correction for mask weighting technique */
  int j, k;
  int status = 0, safestatus = 0;
  int unshielded = 0;  /* Number of unshielded pixels */
  int ndet = 0;        /* Number of detectors */
  int append = 0;
  int append_varimg = 0, append_signifimg = 0;
  int ntot = 0;        /* Total number of oversampling elements */
  char *countscol = 0;

  /* Row selections */
  int maxrowranges = 512, numrowranges = 0;
  long int rowrangemin[512], rowrangemax[512];

  /* Null values for image */
  FLOAT nullval = 1e38;
  int anynull = 0;

  /* Print banner */
  banner(parms);
  append = parms->append;
  append_varimg = parms->append;
  append_signifimg = parms->append;

  /* Name of counts column, if there is one */
  if (parms->countscol[0] != 0) countscol = parms->countscol;

  /* -------------- */

  /* If either of the auxiliary files are specified to be in CALDB,
     then locate them now */
  if ((strncasecmp(parms->aperture, "CALDB",5) == 0) ||
      (strncasecmp(parms->teldef, "CALDB",5) == 0)) {

    status = locate_caldb_files(parms, &dpifile);
    if (status) {
      fprintf(stderr, "ERROR: could not find CALDB files\n");
      return status;
    }
  }

  /* -------------- */
  /* Open and read mask and detector-related data */
  headas_chat(5, "...reading aperture image...\n");
  fits_open_image(&imgfile, parms->aperture, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s\n", parms->aperture);
    return status;
  }
  mask_readkey(imgfile, &mask, &status);
  detplane_readkey(imgfile, &detplane, &status);
  aperture = image_read(imgfile, &status);
  safestatus = 0;
  fits_close_file(imgfile, &safestatus);
  if (status) {
    fprintf(stderr, "ERROR: could not read aperture data from %s\n", 
	    parms->aperture);
    return status;
  }

  mask.meanpos[0] += parms->maskoff[0];
  mask.meanpos[1] += parms->maskoff[1];
  mask.meanpos[2] += parms->maskoff[2];

  /* -------------- */

  /* Image rescaling factors: this factor determines how much the
     aperture map is scaled by, when mapped to the detector plane.
     For a source at infinity, the only rescaling is between the 5 mm
     pitch size of the mask tiles and the 4.2 mm pitch of the detector
     cells.  For a finite position, this has to be rescaled further,
     by the ratio (det to source) / (mask to source).  Here "det"
     refers to the top plane of the detectors. */

  headas_chat(5, "...computing image rescaling factors...\n");

  compute_crpixdelt(0, parms, focal, aperture, &mask, &detplane, 
		    parms->handedness, &wcs);
  xrescale = wcs.rescale_factor[0];
  yrescale = wcs.rescale_factor[1];
  headas_chat(5, "   RX=%f   RY=%f\n", xrescale, yrescale);


  /* -------------- */
  /* Read detector mask file */
  if (parms->detmask[0] != 0) {
    status = read_qmap(parms, &detmask);
    if (status) return status;
  } else {
    headas_chat(5, "...creating default detector mask image...\n");
    detmask = image_mkdetmask(focal, 286, 173);
  }

  /* -------------- */
  headas_chat(5, "...opening input image %s...\n", parms->infile);
  if ((dpifile == 0) && (parms->infile[0])) {
    
    /* Input image is already open if we checked CALDB */
    fits_open_file(&dpifile, parms->infile, READONLY, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not open %s\n", parms->infile);
      goto CLEANUP;
    }
  }

  /* -------------- */
  nimages = 1;
  if (dpifile) {
    nimages = image_nimages(dpifile, &hdutype, &status);
    if (status) {
      fprintf(stderr, "ERROR: could find images in %s\n", 
	      parms->infile);
      goto CLEANUP;
    }
    headas_chat(5, "   (file contains %d images of type %d)\n", 
		nimages, hdutype);
  }

  /* -------------- */
  nbkgimages = 0;
  if (parms->bkgfile[0]) {
    headas_chat(5, "...opening background file %s...\n", parms->bkgfile);
    fits_open_file(&bkgfile, parms->bkgfile, READONLY, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not open %s\n", parms->bkgfile);
      goto CLEANUP;
    }

    nbkgimages = image_nimages(bkgfile, &bkg_hdutype, &status);
    if (status) {
      fprintf(stderr, "ERROR: could find images in %s\n", 
	      parms->bkgfile);
      goto CLEANUP;
    }
    headas_chat(5, "   (background file contains %d images of type %d)\n", 
		nimages, bkg_hdutype);
    if ((nbkgimages > 1) && (nbkgimages != nimages)) {
      fprintf(stderr, "ERROR: the number of background images must match the input,\n");
      fprintf(stderr, "       or there must be only a single background image.\n");
      goto CLEANUP;
    }
  }

  /* -------------- */
  /* Parse the 'rows' string */
  headas_chat(5, "...parsing 'rows'...\n");
  if (fits_parse_range(parms->rowstr, nimages, maxrowranges,
		       &numrowranges, rowrangemin, rowrangemax, &status)) {
    fprintf(stderr, "ERROR: could not parse 'rows' parameter ('%s')\n",
	    parms->rowstr);
    return status;
  }
  headas_chat(5, "   (found %d ranges, starting at %ld, ending at %ld)\n",
	      numrowranges, rowrangemin[0], rowrangemax[numrowranges-1]);
    

  /* ====================================== MAIN PROCESSING LOOP */
  /*    (loop over each image in the input) */
  for(imgnum = 1; imgnum <= nimages; imgnum++) {

    keepbits = parms->keepbits;

  /* -------------- */
  /* Read input detector plane image file */
  if (parms->infile[0]) {

    /* Check to see if this image is in the requested row ranges */
    for (j=0; j<numrowranges; j++) {
      if ((imgnum >= rowrangemin[j]) && (imgnum <= rowrangemax[j])) break;
    }
    /* This image was not found */
    if (j == numrowranges) continue;

    headas_chat(5, "   (reading image %d)\n", imgnum);

    /* -------------- */
    /* Read background subtracted detector plane image file */

    if (focal) image_free(focal); focal = 0;
    if (vardat) image_free(vardat); vardat = 0;
    focal = read_focal(dpifile, imgnum, bkgfile, nbkgimages, 
		       hdutype, bkg_hdutype,
		       countscol, parms, &vardat, &anynull, &status);
    if (focal == 0 || status) {
      fprintf(stderr, "ERROR: reading background subtracted focal map failed\n");
      goto CLEANUP;
    }

    headas_chat(5, "...checking detector image dimensions...\n");
    if ( ((detmask->axes[0] != focal->axes[0]) || 
	  (detmask->axes[1] != focal->axes[1])) ) {
      fprintf(stderr, 
	 "ERROR: Detector mask must have same dimensions as detector image\n");
      return BAD_DIMEN;
    }

  }

  /* Error checking in case there was no calculable variance map */
  if (vardat == 0) {
    keepbits = 0;
    if (parms->bkgvarmap[0] && varwarned == 0) {
      fprintf(stderr, "WARNING: You asked for a variance map but the input map\n");
      fprintf(stderr, "         is not of the right type to do this.  It will \n");
      fprintf(stderr, "         not be output.\n");
      varwarned = 1;
    } else if (parms->signifmap[0] && varwarned == 0) {
      fprintf(stderr, "WARNING: You asked for a significance map but the input map\n");
      fprintf(stderr, "         is not of the right type to make the intermediate\n");
      fprintf(stderr, "         variance map, which is needed.  The significance map\n");
      fprintf(stderr, "         will not be output.\n");
      varwarned = 1;
    } else if (parms->keepbits > 0 && varwarned == 0) {
      fprintf(stderr, "WARNING: You asked for %d significant bits to be preserved in\n",
	      parms->keepbits);
      fprintf(stderr, "         the output image, but the input map is not of the right\n");
      fprintf(stderr, "         type to make the required intermediate variance map.\n");
      fprintf(stderr, "         The full number of bits will be kept.\n");
      varwarned = 1;
    }
  }

  if (focal) {
    headas_chat(5,  "... dpi  dimensions (%d,%d) ...\n", 
		focal->axes[0], focal->axes[1]);
  }
  headas_chat(5,  "... mask dimensions (%d,%d) ...\n", 
	      detmask->axes[0], detmask->axes[1]);

  /* Partial coding map preparation:  
     1. image is detmask
     2. aperture is truncated

     ... but do not compute it if we have already done it in a
     previous iteration.  We assume that the partial coding map does
     not change, since the detmask is fixed, and any image nulls
     should equally be fixed.
 */
  if (((parms->pcodemap != FLUXMAP) || parms->pcodecorr) && (pcodeimg == 0)) {
    int totpix = 0;
    double factor = 1.0;
    struct image_struct *truncaperture = 0, *dummyfocal = 0;

    /* image is detmask */
    headas_chat(5, "...preparing for partial coding computation...\n");
    dummyfocal = image_init(detmask->axes[0], detmask->axes[1], 0);
    if (dummyfocal == 0) {
      fprintf(stderr, "ERROR: could not allocate memory for pcode focal map (%ldx%ld)\n",
	      detmask->axes[0], detmask->axes[1]);
      status = MEMORY_ALLOCATION;
      goto CLEANUP;
    }
      
    foreach_pixel(dummyfocal, i, {dummyfocal->data[i] = detmask->data[i];});

    /* Tally number of unshielded mask elements and number of detectors */
    unshielded = 0;
    foreach_pixel(aperture, i, {if (aperture->data[i] != 0) unshielded++;});

    /* aperture is truncated */
    truncaperture = image_init(aperture->axes[0], aperture->axes[1], 0);
    if (truncaperture == 0) {
      fprintf(stderr, "ERROR: could not allocate memory for truncated aperture (%ldx%ld)\n",
	      aperture->axes[0], aperture->axes[1]);
      status = MEMORY_ALLOCATION;
      goto CLEANUP;
    }
    foreach_pixel(truncaperture, i, 
      {if (aperture->data[i] < 0) truncaperture->data[i] = 0; \
       else truncaperture->data[i] = aperture->data[i];});

    /* Compute total number of aperture pixels */
    headas_chat(5, "...computing partial coding map...\n");
    foreach_pixel(truncaperture, i, 
      {if (truncaperture->data[i] != 0) totpix++;});

    msksqd = 0;
    if (pcodeimg) image_free(pcodeimg);
    pcodeimg = fft_convolve(dummyfocal, detmask, truncaperture, parms, 
			    anynull, nullval,
			    xrescale, yrescale, 1,
			    &pcode_apres, &ndet, msksqd, &status);
    

    factor = (double) ndet * totpix/unshielded;
    headas_chat(5, "...normalizing by number of factor = %f = %f * %f / %f...\n", 
		factor, (double) ndet, (double)totpix, (double) unshielded);
    foreach_pixel(pcodeimg, i,   {pcodeimg->data[i] /= factor; });

    /* Threshold it */
    headas_chat(5, "...computing partial coding threshold...\n");
    foreach_pixel(pcodeimg, i,   
      {if (pcodeimg->data[i] < parms->pcodethresh) pcodeimg->data[i] = 0;});

    image_free(dummyfocal);
    image_free(truncaperture);
  }

  /* -------------- */
  /* Main work routine */
  headas_chat(5, "   pcodemap=%d\n", parms->pcodemap);
  if (parms->pcodemap == PCODEMAP_FILE) {
    outimg = pcodeimg;
    pcodeimg = 0;
  } else {

    if (outimg) image_free(outimg);
    msksqd = 0;
    outimg = fft_convolve(focal, detmask, aperture, parms, anynull, nullval,
			  xrescale, yrescale, 0,
			  &apres, &ndet, msksqd, &status);
  }

  /* -------------- */
  /* Significance or variance maps */
  if (parms->signifmap[0] || parms->bkgvarmap[0] || (parms->keepbits > 0)) {
    if (vardat == 0) {
      headas_chat(1, "WARNING: skipping significance / variance map calculation.\n");
    } else {
      /* Estimate error is:
	 Sum(wi^2 * ri)
      */
      msksqd = 1;
      if (varimg) image_free(varimg); varimg = 0;
      varimg = fft_convolve(vardat, detmask, aperture, parms, anynull, nullval,
			    xrescale, yrescale, 0,
			    &msksqd_apres, &ndet, msksqd, &status);
      if (varimg == 0) {
	fprintf(stderr, "ERROR: variance image calculation failed\n");
	status = MEMORY_ALLOCATION;
	goto CLEANUP;
      }
      /* -------------- */
      /* Compute standard deviation */
      foreach_pixel(varimg, i, {FLOAT d = varimg->data[i]; if (d != nullval && d >= 0)
	varimg->data[i] = sqrt(d); else varimg->data[i] = nullval; });
	
      if (parms->signifmap[0]) {
	if (parms->pcodemap == PCODEMAP_FILE) {
	  fprintf(stderr, 
		  "WARNING: you are computing the significance with a partial coding map.\n"
		  "         That is not a common operation.  Is this correct?\n");
	}
	if (signifimg) image_free(signifimg); signifimg = 0;
	signifimg = image_copy(outimg);
	if (signifimg == 0) {
	  fprintf(stderr, "ERROR: significance image calculation failed\n");
	  status = MEMORY_ALLOCATION;
	  goto CLEANUP;
	}

	/* -------------- */
	/* Compute significance ( = data / stddev ) */
	foreach_pixel(signifimg, i, {FLOAT d = varimg->data[i]; if (d != nullval && d >= 1e-20)
	  signifimg->data[i] /= d; else signifimg->data[i] = nullval; });

      } /* Signifmap */

      /* Prepare standard deviation map for writing */
      headas_chat(5, "...preparing individual variance map...\n");
      varimg->nullval = nullval;
      image_finalize(parms, varimg, pcodeimg, BKGVARMAP, 
		     focal, aperture, &mask, &detplane, &wcs, nullval, ndet, &mwfcorr);

    }
  }

  /* -------------- */
  /* Finalize the output image */
  outimg->nullval = nullval;
  image_finalize(parms, outimg, pcodeimg, 
		 parms->pcodemap == PCODEMAP_FILE, 
		 focal, aperture, 
		 &mask, &detplane, &wcs, nullval, ndet, &mwfcorr);
  if (parms->pcodemap != PCODEMAP_FILE) {
    /* Stripping for normal images */
    image_stripbits(keepbits, outimg, varimg, STRIP_VAR, nullval);
  } else {
    /* Stripping for partial coding maps */
    image_stripbits(parms->keepbits, outimg, 0, STRIP_REL, nullval);
  }

  /* Write output image */
  headas_chat(5, "...writing output image...\n");
  if (append == 0) headas_clobberfile(parms->outfile);
  if (outfile == 0) {
    image_append(&outfile, parms->outfile, append, &status);
  }
  append = 1;  /* Always append after writing the first image */

  if (status) {
    fprintf(stderr, "ERROR: could not open %s for output\n",
	    parms->outfile);
    return status;
  }

  status = image_out(outfile, dpifile, outimg, parms, 
		     parms->pcodemap == PCODEMAP_FILE,
		     imgnum, mwfcorr, focal, aperture, &mask, &detplane,
		     &wcs, nullval, ndet, &ihdu_out);

  if (status != 0) {
    fprintf(stderr, "ERROR: could not write keywords to %s\n", parms->outfile);
    continue;
  }

  ngoodimages ++;
  if (parms->pcodemap == PCODEMAP_FILE) {
    headas_chat(1, "  Partial coding image %d written to %s\n", imgnum, parms->outfile);
  } else {
    headas_chat(1, "  Sky image %d written to %s\n", imgnum, parms->outfile);
  }

  /* -------------- */
  /* Append each partial coding map to the end of the data */
  if (parms->pcodemap == PCODEMAP_APPEND) {
    struct image_struct *pcodeimg_copy = 0;

    /* Make a copy, so we don't destroy the original partial coding
       image (it is re-used in the next loop iteration). */
    pcodeimg_copy = image_copy(pcodeimg);
    if (pcodeimg_copy == 0) {
      fprintf(stderr, "ERROR: could not allocate memory for partial coding temporary array\n");
      return MEMORY_ALLOCATION;
    }
    pcodeimg_copy->nullval = nullval;
    
    headas_chat(5, "...preparing individual partial coding map...\n");
    image_finalize(parms, pcodeimg_copy, 0, PCODEMAP_FILE, focal, aperture, 
		   &mask, &detplane, &wcs, nullval, ndet, &mwfcorr);
    image_stripbits(parms->keepbits, pcodeimg_copy, 0, STRIP_REL,
		    nullval);

    headas_chat(5, "...writing individual partial coding map...\n");
    status = image_out(outfile, dpifile, pcodeimg_copy, parms, 1,
		       imgnum, mwfcorr, focal, aperture, &mask, &detplane,
		       &wcs, nullval, ndet, &ihdu_out);
    headas_chat(1, "  Partial coding image %d written to %s\n", imgnum, parms->outfile);

    image_free(pcodeimg_copy);
  }

  /* -------------- */
  /* Output the variance map */
  if (vardat && varimg && parms->bkgvarmap[0]) {
    if (strcmp(parms->bkgvarmap,"APPEND") == 0) {
      varfile = outfile;
      phdu_var = &ihdu_out;
    } else {
      if (append_varimg == 0) headas_clobberfile(parms->bkgvarmap);
      if (varfile == 0) {
	image_append(&varfile, parms->bkgvarmap, append_varimg, &status);
      }
      append_varimg = 1;  /* Always append after writing the first image */
      phdu_var = &ihdu_var;
    }

    /* NOTE: Most preparation of the variance map occurs above!!! */
    /* For true variance maps, we must square the standard deviation */
    if (parms->bkgvartype == BKGVARMAP) {
      foreach_pixel(varimg, i, { varimg->data[i] *= varimg->data[i]; });
    }

    image_stripbits(keepbits, varimg, 0, STRIP_REL, nullval);
    headas_chat(5, "...writing individual variance map...\n");
    status = image_out(varfile, dpifile, varimg, parms, BKGVARMAP,
		       imgnum, mwfcorr, focal, aperture, &mask, &detplane,
		       &wcs, nullval, ndet, phdu_var);
    headas_chat(1, "  Variance image %d written to %s\n", 
		imgnum, 
		(varfile == outfile)?(parms->outfile):(parms->bkgvarmap));

    if (varfile == outfile) { varfile = 0; }
  }

  /* -------------- */
  /* Output the significance map */
  if (vardat && signifimg && parms->signifmap[0]) {
    if (strcmp(parms->signifmap,"APPEND") == 0) {
      signiffile = outfile;
      phdu_signif = &ihdu_out;
    } else {
      if (append_signifimg == 0) headas_clobberfile(parms->signifmap);
      if (signiffile == 0) {
	image_append(&signiffile, parms->signifmap, append_signifimg, &status);
      }
      append_signifimg = 1;  /* Always append after writing the first image */
      phdu_signif = &ihdu_signif;
    }
    signifimg->nullval = nullval;
    /* Correct standard deviation map */
    headas_chat(5, "...preparing individual significance map...\n");
    image_finalize(parms, signifimg, pcodeimg, SIGNIFMAP, 
		   focal, aperture, &mask, &detplane, &wcs, nullval, ndet, &mwfcorr);
    image_stripbits(parms->keepbits, signifimg, 0, STRIP_VAR, nullval);

    headas_chat(5, "...writing individual significance map...\n");
    status = image_out(signiffile, dpifile, signifimg, parms, SIGNIFMAP,
		       imgnum, mwfcorr, focal, aperture, &mask, &detplane,
		       &wcs, nullval, ndet, phdu_signif);
    headas_chat(1, "  Significance image %d written to %s\n", 
		imgnum, 
		(signiffile == outfile)?(parms->outfile):(parms->signifmap));
    if (signiffile == outfile) { signiffile = 0; }
  }


  }
  /* ===================== END OF MAIN PROCESSING LOOP */

  /* Append final partial coding map to the end of the data */
  if (parms->pcodemap == PCODEMAP_LAST) {
    pcodeimg->nullval = nullval;
    headas_chat(5, "...preparing final partial coding map...\n");
    image_finalize(parms, pcodeimg, 0, PCODEMAP_LAST, focal, aperture, 
		   &mask, &detplane, &wcs, nullval, ndet, &mwfcorr);
    image_stripbits(parms->keepbits, pcodeimg, 0, STRIP_REL, nullval);

    headas_chat(5, "...writing final partial coding map...\n");
    status = image_out(outfile, dpifile, pcodeimg, parms, 3,
		       imgnum, mwfcorr, focal, aperture, &mask, &detplane,
		       &wcs, nullval, ndet, &ihdu_out);
    headas_chat(1, "  Final partial coding image written to %s\n", 
		parms->outfile);
  }


  /* -------------- */
  /* Copy any remaining extensions */
  if ((status == 0) && parms->copyall) {
    FLOAT nullval = 1e38;
    int anynull = 0;
    struct image_struct *image = 0;

    headas_chat(5," ...copying final extensions...\n");
    /* Poor man's way to move to the end of the image extensions */
    if (hdutype == IMAGE_HDU) {
      headas_chat(5," ...moving to last image extension...\n");
      image = image_read_i(dpifile, nimages, countscol, &nullval, &anynull, &status);
      if (status) {
	fprintf(stderr, "ERROR: could not move to last image during cleanup phase\n");
	goto CLEANUP;
      }
      image_free(image);
    }
    
    /* Step through the remaining extensions until we run out of them */
    while (status == 0) {
      headas_chat(5," ...moving to next HDU...\n");
      fits_write_errmark();
      fits_movrel_hdu(dpifile, +1, 0, &status);
      fits_clear_errmark();
      if (status) {
	/* Couldn't move to the extension means... we're done!! */
	status = 0;
	break;
      }

      fits_copy_hdu(dpifile, outfile, 0, &status);
    }

  }


  /* -------------- */
  /* Task cleanup */
  CLEANUP:
  safestatus = 0;
  if (outfile) fits_close_file(outfile, &safestatus);
  safestatus = 0;
  if (dpifile) fits_close_file(dpifile, &safestatus);
  safestatus = 0;
  if (bkgfile) fits_close_file(bkgfile, &safestatus);
  safestatus = 0;
  if (varfile) fits_close_file(varfile, &safestatus);
  safestatus = 0;
  if (signiffile) fits_close_file(signiffile, &safestatus);
  safestatus = 0;

  /* -------------- */
  /* Free image memory */
  headas_chat(5, "...releasing image memory...\n");
  ntot = parms->oversampx * parms->oversampy;

  if (apres && apres[0])  /* Normal flux maps */
    for (k=0; k<ntot; k++) image_free(apres[k]);
  if (apres) free(apres); 

  if (pcode_apres && pcode_apres[0])  /* Partial coding maps */
    for (k=0; k<ntot; k++) image_free(pcode_apres[k]);
  if (pcode_apres) free(pcode_apres);

  if (msksqd_apres && msksqd_apres[0]) 
    for (k=0; k<ntot; k++) image_free(msksqd_apres[k]);
  if (msksqd_apres) free(msksqd_apres);

  apres = 0; pcode_apres = 0; msksqd_apres = 0;
  if (aperture) image_free(aperture);
  if (focal) image_free(focal);
  if (vardat) image_free(vardat);
  if (focres) image_free(focres);
  if (result) image_free(result);
  if (outimg) image_free(outimg);
  if (pcodeimg) image_free(pcodeimg);
  if (detmask) image_free(detmask);

  summary(parms, ngoodimages);
  return status;
}

int batfftimage(void)
{
  int status = 0;

  struct parm_struct parms;

  /* Register taskname and version. */

  set_toolname(taskname);
  set_toolversion(taskver);

  if ((status = batfftimage_getpar(&parms)) != 0) {
    fprintf(stderr, "Could not read parameter file\n");
    return status;
  }
  
  parms.taskname = &taskname[0];
  parms.taskver  = &taskver[0];

  return batfftimage_work(&parms);

}
