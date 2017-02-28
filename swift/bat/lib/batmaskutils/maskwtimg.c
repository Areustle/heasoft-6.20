#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "batmask.h"
#include "batdet.h"
#include "headas.h"

/* 
 * Forward projection of entire detector array
 * 
 * C. Markwardt
 *
 * $Id: maskwtimg.c,v 1.26 2005/01/21 21:54:08 craigm Exp $ 
 *
 * 02 Jan 2003 - Signature of maskwtimg() changed because of 2D array
 *               problems (CM)
 */

/* Detector block offsets from the corner, in units of blocks */
double blockox[] = {0,1,2,3,4,5,6,7,0,1,2,3,4,5,6,7};
double blockoy[] = {1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0};

/*
 * Compute forward projection of entire detector array
 *
 * double srcpos[3] - source position in BAT_X/Y/Z (cm)
 * struct batmaskplane_struct *mask - mask orientation
 * struct batdetplane_struct *detplane - detector plane orientation
 * double subpixsize - size of subpixels (cm)
 * double maskimg[][] - image of detector plane
 *
 * RETURNS: 0 upon success
 */

int maskwtimg(double srcpos[3], 
	      struct batmaskplane_struct *mask, 
	      struct batdetplane_struct *detplane,
	      struct batmaskcorrections_struct *corrs,
	      double *maskimg, int nimgx, int nimgy)
{
  double dz = 0, zdet = 0;        /* Offsets of mask and detector */
  int npoly;
  double *poly, *p;
  double *weights;
  int i, j, b;
  double detcell[2];
  double detcell0[2];
  double bx, by;
  int nmasklayers;  /* Number of mask layers */
  int ilayer;
  double dzstep;    /* Step of sublayers in cm */
  int nmaxpoly;
  double subpixsize;         /* Subpixel size in cm */
  int    blocknx, blockny;   /* Number of block subpixels (BAT_X/Y) */
  double blockwx, blockwy;   /* Block width in CM (BAT_X/Y) */
  int subpitchx, subpitchy;  /* Number of subpixels per det pitch element */
  int count;
  double sum;
  int dmx = 0, *detmask = 0;
  int cfast = corrs->cfast;
  double mwfcorr, Kcorr;

  int ndetson = 0, ncoded = 0;
  float wtcorr;
  double pcodefr;
  double detpos[3] = {0, 0, 0}; /* Center of detector array */

  DETFLOAT *arr;

  if (corrs->copaque) {
    /* Call into Jay's code if the opaque correction is set.  
       Return immediately! */
    return maskwtimg_op(srcpos, mask, detplane, corrs, maskimg, nimgx, nimgy);
  }

  /* Size of the subpixel */
  subpixsize = corrs->subpixsize;
  detcell[0] = subpixsize;
  detcell[1] = subpixsize;

  /* Number of sub pixels per detector cell */
  if ( cfast == 0 ) {
    subpitchx = rint(detplane->cellsize[0]/subpixsize);
    subpitchy = rint(detplane->cellsize[1]/subpixsize);
  } else {
    /* In "fast" mode the whole detector cell is computed at once
       without subpixelization */
    subpitchx = 1;
    subpitchy = 1;
  }

  zdet = detplane->centpos[2] + detplane->meanpos[2];
  headas_chat(5,"  (Z position of detector plane = %f)\n", 
	      zdet);

  blocknx = BLOCKXCELLS * subpitchx;             /* Number of "x" cells */
  blockny = BLOCKYCELLS * subpitchy;             /* Number of "y" cells */
  blockwx = BLOCKXCELLS * detplane->cellsize[0]; /* Block width in "x" */
  blockwy = BLOCKYCELLS * detplane->cellsize[1]; /* Block width in "y" */

  /* Array for subpixelated mask weights */
  arr = (DETFLOAT *) malloc(sizeof(DETFLOAT)*blocknx*blockny);

  /* Subpixelation of the mask in the Z direction  (CM) */
  nmasklayers = est_masklayers(srcpos, mask->cellsize, corrs);
  dzstep = mask->cellsize[2]/nmasklayers;
  
  nmaxpoly = mask->ncells[0]*mask->ncells[1];
  poly = (double *) malloc(nmaxpoly*sizeof(double)*8);
  weights = (double *) malloc(nmaxpoly*sizeof(double));

  /* Render these polygons onto the subpixelated detector array; 
     
     Do each block separately 
  */
  for (b=0; b<16; b++) {
    double bbx1, bbx2, bby1, bby2;
    int nblockpix = blocknx*blockny;
    int i0, j0;
    int nbres;
    DETFLOAT *arrptr = 0;

    sum = 0;
    nbres = 0;
    for (i=0; i<nblockpix; i++) arr[i] = 0;

    /* Step through each mask sublayer */
    for (ilayer=0; ilayer<nmasklayers; ilayer++) {
      double dx = detplane->cellsize[0];
      double dy = detplane->cellsize[0];
    
      dz = -dzstep * ilayer;
    
      /* Position of block in BAT X/Y */
      bx = detplane->cell0[0]+blockwx*blockox[b];
      by = detplane->cell0[1]+blockwy*blockoy[b];
      detcell0[0] = bx;
      detcell0[1] = by;

      /* Establish bounding box */
      bbx1 = bx-2*dx;
      bbx2 = bx+blockwx+2*dx;
      bby1 = by-2*dy;
      bby2 = by+blockwy+2*dy;
      /*headas_chat(5, "...block %d layer %d bounding box...\n", b, ilayer);*/
      /*headas_chat(5, "   (%f,%f) (%f,%f)\n", bbx1, bby1, bbx2, bby2);*/

      /* Forward project the mask tiles onto the detector array */
      npoly = forwmask(srcpos, mask, 
		       bbx1, bbx2, bby1, bby2,
		       dz, zdet, poly, weights, nmaxpoly, corrs);
      if (npoly <= 0) continue;
      /* headas_chat(5, "...block %d layer %d npoly=%d...\n", b, ilayer, npoly); */

      /* Scan convert the polygons into the subpixelated image array */
      
      p = poly;
      for(i=0; i<npoly; i++) {
	/* Make unbalanced weight map if requested */
	if (corrs->cunbalanced && (weights[i] < 0)) weights[i] = 0;

	if ((p[0] > bbx1) && (p[0] < bbx2) && 
	    (p[4] > bby1) && (p[4] < bby2) && (weights[i] != 0)) {
	  if (cfast == 0) {
	    detbresimg(p, p+4, arr, blocknx, blockny, detcell, detcell0, 
		       (DETFLOAT) weights[i]);
	  } else {
	    fastcell(p, p+4, arr, blocknx, blockny, detplane, detcell0, 
		     (DETFLOAT) weights[i]);
	  }
	  nbres ++;
	}
	p += 8;  /* Skip ahead by 4 points = 8 values */
      }
      /* headas_chat(5, "...block %d layer %d nbres=%d...\n", b, ilayer, nbres); */
    }  /* mask tile sublayer loop */


    /* Rebin the subpixels into standard det-sized pixels */
    /* Note: b below is the block number as described in above list */
    i0 = (b%8)*BLOCKXCELLS;   /* Start position in X-pixels for the block */
    j0 = (1-b/8)*BLOCKYCELLS; /* Start position in Y-pixels for the block */

    if (nbres > 0) for(i=0; i<BLOCKXCELLS; i++) {
      if (i % 18 > 15) continue;    /* Skip over gaps in X */
      for(j=0; j<BLOCKYCELLS; j++) {
	if (j % 11 > 7) continue;   /* Skip over gaps in Y */

	arrptr = arr + i*subpitchx + j*subpitchy*blocknx;
	if (cfast == 0) {
	  sum = maskaddsub(detplane, corrs, arrptr, blocknx, 0);
	} else {
	  sum = *arrptr;
	}

	maskimg[(j0+j)*nimgx + i0+i] += (sum/nmasklayers);
      } /* j loop */
    }   /* i loop */
    
  } /* block loop */


  /* ===================== Apply mask, and corrections, if any */

  /* Apply detector mask if given */
  if (corrs->detmask && (corrs->dmx > 0) && (corrs->dmy > 0)) {
    dmx = corrs->dmx;
    detmask = corrs->detmask;
  }

  /* Mask the image out, and compute the number of detectors enabled,
     and coded */
  if (detmask) {
    for(j=0; j<173; j++) {
      for(i=0; i<286; i++) {
	if (detmask[j*dmx+i]) {
	  ndetson ++;
	  if (maskimg[j*nimgx+i] != 0) ncoded ++;
	} else {
	  maskimg[j*nimgx+i] = 0;
	}
      }
    }
  } else {

    /* Something sane to do if no quality mask was supplied */
    ndetson = 32768;
    for(j=0; j<173; j++) {
      for(i=0; i<286; i++) {
	if (maskimg[j*nimgx+i] != 0) ncoded ++;
      }
    }
  }

  /* Compute mask weighting corrections */
  wtcorr = 1;

  pcodefr = 0.0;
  if (ndetson > 0) pcodefr = (float) ncoded / ndetson;

  /* Store these values for later use */
  corrs->ngoodpix = ndetson;
  corrs->pcodefr  = pcodefr;
  corrs->maskwtsqf = 1.0;

  if (corrs->cpcode && pcodefr > 0) {
    wtcorr *= 1/pcodefr;
  } else {
    corrs->maskwtsqf *= pcodefr;
  }
  if (corrs->cnbatdets && ndetson > 0) {
    wtcorr *= 1 / (float) ndetson;
  } else {
    corrs->maskwtsqf *= ndetson;
  }
  /* Normalize by mask weighting technique */
  mwfcorr = maskwtnorm(srcpos,mask,detplane)*(1+corrs->maskwtswgain);
  if (corrs->cmaskwt) {
    wtcorr *= 1 / mwfcorr;
  } else {
    corrs->maskwtsqf *= mwfcorr;
  }

  /* Pull out any other (cosine-type) corrections, from the center of
     the detector array */
  detpos[0] = 0; detpos[1] = 0; detpos[2] = zdet;  /* Center face of detector array */
  Kcorr = maskwtcorrs(detpos,srcpos,corrs);  /* prop to 1/cos(th) */

  /* These corrections have already been applied to the individual
     weights, so no need to apply them again. However, they also
     need to be applied to the MASKWTSQF. */
  corrs->maskwtsqf *= Kcorr;

  headas_chat(5, " (ndetson=%d ncoded=%d pcodefr=%f mwfcorr=%f Kcorr=%f wtcorr=%f)\n", 
	      ndetson, ncoded, pcodefr, mwfcorr, Kcorr, wtcorr);

  /* Apply mask weighting correction */
  for(j=0; j<173; j++) {
    for(i=0; i<286; i++) {
      maskimg[j*nimgx+i] *= wtcorr;
    }
  }

  /* Rebalance the image if requested */
  if (corrs->rebalance) {
    double Ksum, Ki;

    sum = 0;
    Ksum = 0;
    count = 0;

    /* First find sum of non-zero elements */
    for(j=0; j<173; j++) {
      for(i=0; i<286; i++) {
	if ((maskimg[j*nimgx+i] != 0) && ((!detmask) || detmask[j*dmx+i])) {
	  sum += maskimg[j*nimgx+i];

	  /* Compute the approximate weights for each detector.  The
             computation needs only to be approximate since it will be
             applied to the summed weights, which should be close to
             zero anyway.

	     The concept here is that:

	        Sum[ wi' ] = 0, 
             where wi' = (2*fi - 1 + C) * Ki
                   fi is the illuminated fraction
                   C is the constant we seek
                   Ki is any correction we applied (maskwtcorrs)
             The result of the sum below is actually Sum[ wi ]
             where wi = (2*fi - 1) * Ki, without the C.  To
             estimate C, we need to compute Sum[ Ki ].
	  */
	  detpos[0] = detplane->cell0[0] + (i + 0.5)*detplane->cellsize[0];
	  detpos[1] = detplane->cell0[1] + (j + 0.5)*detplane->cellsize[1];
	  detpos[2] = zdet;
	  
	  Ki = maskwtcorrs(detpos, srcpos, corrs);
	  Ksum += Ki;

	  count += 1;
	}
      }
    }

    headas_chat(5, " (rebalance sum=%f Ksum=%f count=%d)\n", sum, Ksum, count);

    /* Next, subtract the mean value */
    if ((count > 0) && (Ksum != 0)) {
      sum /= Ksum;

      for(j=0; j<173; j++) {
	for(i=0; i<286; i++) {
	  if ((maskimg[j*nimgx+i] != 0) && ((!detmask) || detmask[j*dmx+i])) {

            detpos[0] = detplane->cell0[0] + (i + 0.5)*detplane->cellsize[0];
            detpos[1] = detplane->cell0[1] + (j + 0.5)*detplane->cellsize[1];
            detpos[2] = zdet;

            Ki = maskwtcorrs(detpos, srcpos, corrs);
            maskimg[j*nimgx+i] -= sum*Ki;

	  }
	}
      }
    }
  }

  free(poly);
  free(weights);
  free(arr);

  return 0;
}

