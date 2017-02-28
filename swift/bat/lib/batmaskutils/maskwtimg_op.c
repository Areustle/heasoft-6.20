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
 * $Id: maskwtimg_op.c,v 1.1 2003/11/24 09:05:02 craigm Exp $ 
 *
 * 02 Jan 2003 - Signature of maskwtimg() changed because of 2D array
 *               problems (CM)
 */

/* Detector block offsets from the corner, in units of blocks */
double blockox_op[] = {0,1,2,3,4,5,6,7,0,1,2,3,4,5,6,7};
double blockoy_op[] = {1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0};

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

int maskwtimg_op(double srcpos[3], 
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
  double dzstep;    /* Step of sublayers in cm */
  int nmaxpoly;
  double subpixsize;         /* Subpixel size in cm */
  int    blocknx, blockny;   /* Number of block subpixels (BAT_X/Y) */
  double blockwx, blockwy;   /* Block width in CM (BAT_X/Y) */
  int subpitchx, subpitchy;  /* Number of subpixels per det pitch element */
  int count;
  double sum;
  struct blockdettop_struct dettop, *dettops;

  DETFLOAT *arrb, *arrt;


  /* Size of the subpixel */
  subpixsize = corrs->subpixsize;
  detcell[0] = subpixsize;
  detcell[1] = subpixsize;

  subpitchx = rint(detplane->cellsize[0]/subpixsize);
  subpitchy = rint(detplane->cellsize[1]/subpixsize);

  zdet = detplane->centpos[2] + detplane->meanpos[2];
  /* headas_chat(5,"  (Z position of detector plane = %f)\n", 
	      zdet); */

  blocknx = BLOCKXCELLS * subpitchx;
  blockny = BLOCKYCELLS * subpitchy;
  blockwx = BLOCKXCELLS * detplane->cellsize[0];
  blockwy = BLOCKYCELLS * detplane->cellsize[1];

  arrb = (DETFLOAT *) malloc(sizeof(DETFLOAT)*blocknx*blockny);
  arrt = (DETFLOAT *) malloc(sizeof(DETFLOAT)*blocknx*blockny);

  /* Subpixelation of the mask in the Z direction  (CM) */
  dzstep = mask->cellsize[2];
  
  nmaxpoly = mask->ncells[0]*mask->ncells[1];
  poly = (double *) malloc(nmaxpoly*sizeof(double)*8);
  weights = (double *) malloc(nmaxpoly*sizeof(int));

  /* Render these polygons onto the subpixelated detector array; 
     
     Do each block separately 
  */
  for (b=0; b<16; b++) {
    double bbx1, bbx2, bby1, bby2;
    int nblockpix = blocknx*blockny;
    int i0, j0;
    int nbres;
    DETFLOAT *arrptr = 0;
    double dx = detplane->cellsize[0];
    double dy = detplane->cellsize[1];
   

    sum = 0;
    nbres = 0;
    for (i=0; i<nblockpix; i++) {
	    arrb[i] = 0;
	    arrt[i] = 0;
    }



    dz = -dzstep; /* Bottom of mask */
  
    /* Position of block in BAT X/Y */
    bx = detplane->cell0[0]+blockwx*blockox_op[b];
    by = detplane->cell0[1]+blockwy*blockoy_op[b];
    detcell0[0] = bx;
    detcell0[1] = by;

    /* Establish bounding box */
    bbx1 = bx-2*dx;
    bbx2 = bx+blockwx+2*dx;
    bby1 = by-2*dy;
    bby2 = by+blockwy+2*dy;
    /* headas_chat(5, "...block %d bottom of mask bounding box...\n", b); */
    /* headas_chat(5, "   (%f,%f) (%f,%f)\n", bbx1, bby1, bbx2, bby2); */

    /* Forward project the mask tiles onto the detector array */
    npoly = forwmask(srcpos, mask, 
		     bbx1, bbx2, bby1, bby2,
		     dz, zdet, poly, weights, nmaxpoly, corrs);
    if (npoly <= 0) continue;
    /* headas_chat(5, "...block %d bottom of mask npoly=%d...\n", b, npoly); */

    /* Scan convert the polygons into the subpixelated image array */
    
    p = poly;
    for(i=0; i<npoly; i++) {
	/* Make unbalanced weight map ---- NOT JUST if requested *
	 * The previous scheme does not do weight corrections right. *
	 * The weights should be corrected on the full scale (blocked
	 * to unblocked), rather than on the half scale (0 to ~1 where
	 * blocked is -1. See the correction below in the balancing
	 * section. Making blocked a very small positive number, as
	 * opposed to making it exactly zero (as uncoded is), because
	 * I want to mark it as coded. */ 
	if (weights[i] < 0) weights[i] = 0.0001;
	if ((p[0] > bbx1) && (p[0] < bbx2) && (p[4] > bby1) && (p[4] < bby2)) {
	detbresimg(p, p+4, arrb, blocknx, blockny, detcell, detcell0, 
		  (DETFLOAT) weights[i]);
	nbres ++;

	}


	p += 8;/* Skip ahead by 4 points = 8 values */
    }
    /* headas_chat(5, "...block %d bottom of mask nbres=%d...\n", b, nbres); */


    dz = 0.0; /* Top of mask */
  
    /* Position of block in BAT X/Y */
    bx = detplane->cell0[0]+blockwx*blockox_op[b];
    by = detplane->cell0[1]+blockwy*blockoy_op[b];
    detcell0[0] = bx;
    detcell0[1] = by;

    /* Establish bounding box */
    bbx1 = bx-2*dx;
    bbx2 = bx+blockwx+2*dx;
    bby1 = by-2*dy;
    bby2 = by+blockwy+2*dy;
    /* headas_chat(5, "...block %d top of mask bounding box...\n", b); */
    /* headas_chat(5, "   (%f,%f) (%f,%f)\n", bbx1, bby1, bbx2, bby2); */

    /* Forward project the mask tiles onto the detector array */
    npoly = forwmask(srcpos, mask, 
		     bbx1, bbx2, bby1, bby2,
		     dz, zdet, poly, weights, nmaxpoly, corrs);
    if (npoly <= 0) continue;
    /* headas_chat(5, "...block %d top of mask npoly=%d...\n", br, npoly); */

    /* Scan convert the polygons into the subpixelated image array */
    
    p = poly;
    for(i=0; i<npoly; i++) {

	/* Make unbalanced weight map ---- NOT JUST if requested *
	 * The previous scheme does not do weight corrections right. *
	 * The weights should be corrected on the full scale (blocked
	 * to unblocked), rather than on the half scale (0 to ~1 where
	 * blocked is -1. See the correction below in the balancing
	 * section. Making blocked a very small positive number, as
	 * opposed to making it exactly zero (as uncoded is), because
	 * I want to mark it as coded. */ 
	if (weights[i] < 0) weights[i] = 0.0001;

	if ((p[0] > bbx1) && (p[0] < bbx2) && (p[4] > bby1) && (p[4] < bby2)) {
	detbresimg(p, p+4, arrt, blocknx, blockny, detcell, detcell0, 
		  (DETFLOAT) weights[i]);
	nbres ++;
	}
	p += 8;/* Skip ahead by 4 points = 8 values */
    }
    /* headas_chat(5, "...block %d top of mask nbres=%d...\n", b, nbres); */

  for(i = 0; i< blocknx; i++) 
    for(j = 0; j < blockny; j++)
      if(*(arrb + i + j*blocknx) > *(arrt + i + j*blocknx))
            *(arrb + i + j*blocknx) = *(arrt + i + j*blocknx);


  /* Rebin the subpixels into standard det-sized pixels */
  /* Note: b below is the block number as described in above list */
  i0 = (b%8)*BLOCKXCELLS;   /* Start position in X-pixels for the block */
  j0 = (1-b/8)*BLOCKYCELLS; /* Start position in Y-pixels for the block */
  if (nbres > 0) for(i=0; i<BLOCKXCELLS; i++) {
    if (i % 18 > 15) continue;    /* Skip over gaps in X */
    for(j=0; j<BLOCKYCELLS; j++) {
      if (j % 11 > 7) continue; /* Skip over gaps in Y */
      arrptr = arrb + i*subpitchx + j*subpitchy*blocknx;
      sum = maskaddsub(detplane, corrs, arrptr, blocknx, maskimg[(j0+j)*nimgx + i0+i]);

      maskimg[(j0+j)*nimgx + i0+i] += sum;

    } /* j loop */
  }   /* i loop */

  if(corrs->cinc_edge) {
    float eff_edge;

    eff_edge = corrs->eff_edge;
    sum = 0;
    nbres = 0;
    for (i=0; i<nblockpix; i++) {
	    arrb[i] = 0;
	    arrt[i] = 0;
    }

    dz = -dzstep; /* Bottom of mask */
  
    /* Position of block in BAT X/Y */
    bx = detplane->cell0[0]+blockwx*blockox_op[b];
    by = detplane->cell0[1]+blockwy*blockoy_op[b];
    detcell0[0] = bx;
    detcell0[1] = by;

    /* Establish bounding box */
    bbx1 = bx-2*dx;
    bbx2 = bx+blockwx+2*dx;
    bby1 = by-2*dy;
    bby2 = by+blockwy+2*dy;
    /* headas_chat(5, "...block %d bottom of mask bounding box...\n", b); */
    /* headas_chat(5, "   (%f,%f) (%f,%f)\n", bbx1, bby1, bbx2, bby2); */

    /* Forward project the mask tiles onto the detector array */
    npoly = forwmask(srcpos, mask, 
		     bbx1, bbx2, bby1, bby2,
		     dz, zdet-eff_edge, poly, weights, nmaxpoly, corrs);
    if (npoly <= 0) continue;
    /* headas_chat(5, "...block %d bottom of mask npoly=%d...\n", b, npoly); */

    /* Scan convert the polygons into the subpixelated image array */
    
    p = poly;
    for(i=0; i<npoly; i++) {

	/* Make unbalanced weight map ---- NOT JUST if requested *
	 * The previous scheme does not do weight corrections right. *
	 * The weights should be corrected on the full scale (blocked
	 * to unblocked), rather than on the half scale (0 to ~1 where
	 * blocked is -1. See the correction below in the balancing
	 * section. Making blocked a very small positive number, as
	 * opposed to making it exactly zero (as uncoded is), because
	 * I want to mark it as coded. */ 
	if (weights[i] < 0) weights[i] = 0.0001;

	if ((p[0] > bbx1) && (p[0] < bbx2) && (p[4] > bby1) && (p[4] < bby2)) {
	detbresimg(p, p+4, arrb, blocknx, blockny, detcell, detcell0, 
		  (DETFLOAT) weights[i]);
	nbres ++;
	}
	p += 8;/* Skip ahead by 4 points = 8 values */
    }
    /* headas_chat(5, "...block %d bottom of mask nbres=%d...\n", b, nbres); */


    dz = 0.0; /* Top of mask */
  
    /* Position of block in BAT X/Y */
    bx = detplane->cell0[0]+blockwx*blockox_op[b];
    by = detplane->cell0[1]+blockwy*blockoy_op[b];
    detcell0[0] = bx;
    detcell0[1] = by;

    /* Establish bounding box */
    bbx1 = bx-2*dx;
    bbx2 = bx+blockwx+2*dx;
    bby1 = by-2*dy;
    bby2 = by+blockwy+2*dy;
    /* headas_chat(5, "...block %d top of mask bounding box...\n", b); */
    /* headas_chat(5, "   (%f,%f) (%f,%f)\n", bbx1, bby1, bbx2, bby2); */

    /* Forward project the mask tiles onto the detector array */
    npoly = forwmask(srcpos, mask, 
		     bbx1, bbx2, bby1, bby2,
		     dz, zdet-eff_edge, poly, weights, nmaxpoly, corrs);
    if (npoly <= 0) continue;
    /* headas_chat(5, "...block %d top of mask npoly=%d...\n", br, npoly); */

    /* Scan convert the polygons into the subpixelated image array */
    
    p = poly;
    for(i=0; i<npoly; i++) {

	/* Make unbalanced weight map ---- NOT JUST if requested *
	 * The previous scheme does not do weight corrections right. *
	 * The weights should be corrected on the full scale (blocked
	 * to unblocked), rather than on the half scale (0 to ~1 where
	 * blocked is -1. See the correction below in the balancing
	 * section. Making blocked a very small positive number, as
	 * opposed to making it exactly zero (as uncoded is), because
	 * I want to mark it as coded. */ 
	if (weights[i] < 0) weights[i] = 0.0001;

	if ((p[0] > bbx1) && (p[0] < bbx2) && (p[4] > bby1) && (p[4] < bby2)) {
	detbresimg(p, p+4, arrt, blocknx, blockny, detcell, detcell0, 
		  (DETFLOAT) weights[i]);
	nbres ++;
      }
      p += 8;/* Skip ahead by 4 points = 8 values */
    }
    /* headas_chat(5, "...block %d top of mask nbres=%d...\n", b, nbres); */

    for(i = 0; i< blocknx; i++) 
      for(j = 0; j < blockny; j++)
        if(*(arrb + i + j*blocknx) > *(arrt + i + j*blocknx))
            *(arrb + i + j*blocknx) = *(arrt + i + j*blocknx);


    /* Project detector tops onto plane */

    dettops = &dettop;

    dettops->cell0[0] = detcell0[0];
    dettops->cell0[1] = detcell0[1];
    dettops->cell0[2] = detplane->cell0[2];
    dettops->cellsize[0] = detplane->cellsize[0];
    dettops->cellsize[1] = detplane->cellsize[1];
    dettops->cellsize[2] = detplane->cellsize[2];
    dettops->ncells[0] = 36;
    dettops->ncells[1] = 88;
    dettops->centpos[0] = detplane->centpos[0];
    dettops->centpos[1] = detplane->centpos[1];
    dettops->centpos[2] = detplane->centpos[2];
    dettops->meanpos[0] = detplane->meanpos[0];
    dettops->meanpos[1] = detplane->meanpos[1];
    dettops->meanpos[2] = detplane->meanpos[2];

    npoly = forwdettop(srcpos,dettops,
		    bbx1, bbx2, bby1, bby2,
		    zdet-eff_edge, poly, nmaxpoly, corrs);

    /* headas_chat(5, "dettops ...block %d npoly=%d...nbres=%d\n", b, npoly,nbres); */

    if (npoly <= 0) continue;

    p = poly;
    for(i=0; i<npoly; i++) {

      if ((p[0] > bbx1) && (p[0] < bbx2) && (p[4] > bby1) && (p[4] < bby2)) {
        dettop_shad(p, p+4, arrb, blocknx, blockny, detcell, detcell0);
      }
        p += 8;/* Skip ahead by 4 points = 8 values */
    }
  

    /* Rebin the subpixels into standard det-sized pixels */
    /* Note: b below is the block number as described in above list */
    i0 = (b%8)*BLOCKXCELLS;   /* Start position in X-pixels for the block */
    j0 = (1-b/8)*BLOCKYCELLS; /* Start position in Y-pixels for the block */
    if (nbres > 0) for(i=0; i<BLOCKXCELLS; i++) {
      if (i % 18 > 15) continue;    /* Skip over gaps in X */
      for(j=0; j<BLOCKYCELLS; j++) {
        if (j % 11 > 7) continue; /* Skip over gaps in Y */
  
          arrptr = arrb + i*subpitchx + j*subpitchy*blocknx;
          sum = maskaddsub(detplane, corrs, arrptr, blocknx, maskimg[(j0+j)*nimgx + i0+i]);
	  if(maskimg[(j0+j)*nimgx + i0+i] != 0)  /* if the detector is coded */
            maskimg[(j0+j)*nimgx + i0+i] += sum;
  
    
        } /* j loop */
      }   /* i loop */

    } /* Edge section */
    
  } /* block loop */

  /* Rebalance the image if requested */
  if (corrs->rebalance) {

    sum = 0;
    count = 0;

    /* First find sum of non-zero elements */
    for(j=0; j<173; j++) {
      for(i=0; i<286; i++) {
	if (maskimg[j*nimgx+i] > 0)  {
	  maskimg[j*nimgx+i] *= 2;
	  sum += maskimg[j*nimgx+i];
	  count += 1;
	} 
      }
    }

    /* Next, subtract the mean value */
    if (count > 0) {
      sum /= count;

      for(j=0; j<173; j++) {
	for(i=0; i<286; i++) {
	  if (maskimg[j*nimgx+i] > 0)  {
	/* Make the weight corrected map go from -1 to  ~1 */
            maskimg[j*nimgx+i] -= sum;
	  }
	}
      }
    }
  }

  free(poly);
  free(weights);
  free(arrb);
  free(arrt);

  return 0;
}

