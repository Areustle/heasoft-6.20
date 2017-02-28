#include <stdlib.h>
#include <math.h>
#include "batmask_op.h"
#include "batdet_op.h"
#include "headas.h"

/* 
 * Forward project the corners of an array of mask pixels onto the
 * detector plane.
 *
 * The position of the source is given by srcpos[], and the
 * orientation of the mask by the mask parameter.  A bounding box is
 * given by (xmin,ymin) and (xmax,ymax).
 * 
 * The user is responsible for allocating enough space for polygons
 * and weights.  polygons should be a pointer to double, 8*nmaxpoly;
 * weights a pointer to int, 1*nmaxpoly.
 *
 * The layout of a single polygon is {X0,X1,X2,X3,Y0,Y1,Y2,Y3}.
 * 
 * double srcpos[3] - position of the source in BAT coords (cm);
 * struct batmaskplane_struct *mask - mask position and orientation;
 * double xmin, xmax, ymin, ymax - bounding box of detector plane (cm);
 * double dz - adjustment to mask plane BAT_Z position;
 * double zdet - BAT_Z height of detector plane;
 * poly - pointer to polygon store;
 * weights - pointer to weight store;
 * nmaxpoly - maximum number of allocated polygons.
 *
 * RETURNS: number of converted polygons
 *          -1 upon failure
 *
 * $Id: forwmask_op.c,v 1.1 2003/10/28 19:14:08 krimm Exp $
 *
 * 17 Dec 2002 - Fixed error in computation of bounding box (CM)
 *               (only appears in near-field case)
 *
 */

int forwmask(double srcpos[3], struct batmaskplane_struct *mask, 
	     double xmin, double xmax, double ymin, double ymax,
	     double dz, double zdet, 
	     double *poly, double *weights, int nmaxpoly,
	     struct batmaskcorrections_struct *corrs)
{
  double *p;
  double coffs[4][2];
  double cx, cy,   cellx, celly;
  double xm, ym, zm;     /* Intersection of ray with mask */
  double xsm, ysm, zsm;  /* Vector from mask to source */
  double zdm, rat;       /* zdm = Zdet - Zmask;  rat = zdm/zsm */
  double xmmin, xmmax, ymmin, ymmax;          /* Bounding box at mask */
  int cellimin, cellimax, celljmin, celljmax; /* Bounding mask pixels */
  double detpos[3], wtcorr;
  int ncells, npoly = 0;
  int k;
  int celli, cellj;
  int *ap;
  
  if (nmaxpoly <= 0) return -1;

  cx = mask->cellsize[0];
  cy = mask->cellsize[1];
  /* Positions of all mask cell corners, with respect to one corner,
     MUST be in CLOCKWISE order! */
  coffs[0][0] = 0; coffs[0][1] = 0;
  coffs[1][0] = 0; coffs[1][1] =cy;
  coffs[2][0] =cx; coffs[2][1] =cy;
  coffs[3][0] =cx; coffs[3][1] = 0;

  /* Pre-compute some valuable quantities */
  /* Z Position of cell (cm) with respect to BAT coordinates */
  zm  = mask->centpos[2] + mask->meanpos[2] + dz;
  zsm = srcpos[2]-zm;

  /* Z component from mask to detector */
  zdm = zdet-zm; 
  rat = zdm/zsm;

  /* First compute the bounding box 
     ASSUMES NO mask translation, rotation, etc. */
  xmmin = xmin - (srcpos[0]-xmin)*zdm/(srcpos[2]-zdet);
  ymmin = ymin - (srcpos[1]-ymin)*zdm/(srcpos[2]-zdet);
  xmmax = xmax - (srcpos[0]-xmax)*zdm/(srcpos[2]-zdet);
  ymmax = ymax - (srcpos[1]-ymax)*zdm/(srcpos[2]-zdet);
  cellimin = floor((xmmin-mask->cell0[0])/cx);
  celljmin = floor((ymmin-mask->cell0[1])/cy);
  cellimax = ceil((xmmax-mask->cell0[0])/cx);
  celljmax = ceil((ymmax-mask->cell0[1])/cy);

  /* Range check */
  if (cellimin < 0) cellimin = 0;
  if (celljmin < 0) celljmin = 0;
  if (cellimax >= mask->ncells[0]) cellimax = mask->ncells[0]-1;
  if (celljmax >= mask->ncells[1]) celljmax = mask->ncells[1]-1;

  ncells = (celljmax-celljmin+1)*(cellimax-cellimin+1);
  if (ncells == 0) return 0;

  npoly = 0;
  ap = mask->aperture; /* Mask aperture array */

  p = poly;
  for (cellj = celljmin; cellj <= celljmax; cellj++) {
    for (celli = cellimin; celli <= cellimax; celli++) {
      int weight = ap[celli+cellj*(mask->ncells[0])];
      if (weight != 0) {
	if (npoly >= nmaxpoly) return -1;

	/* X/Y Position of cell (cm) with respect to the mask design center */
	cellx = celli * cx + mask->cell0[0];
	celly = cellj * cy + mask->cell0[1];
	
	/* X/Y Position of cell (cm) with respect to BAT coordinates */
	xm = cellx + mask->centpos[0] + mask->meanpos[0];
	ym = celly + mask->centpos[1] + mask->meanpos[1];
	
	/* Vector which points from mask location to source position (cm) */
	xsm = srcpos[0]-xm;    ysm = srcpos[1]-ym;    
	
	/* Here is where tilt/rot/warp code would go */
	
	/* Loop over every corner - first X then Y */
	for (k=0; k<4; k++){
	  *p++ = xm+coffs[k][0] + (xsm-coffs[k][0])*rat;
	}
	for (k=0; k<4; k++){
	  *p++ = ym+coffs[k][1] + (ysm-coffs[k][1])*rat;
	}

	detpos[0] = 0; detpos[1] = 0;
	detpos[2] = zdet;
	for (k=-8; k<(-4); k++) detpos[0] += p[k]/4;
	for (k=-4; k<(0); k++)  detpos[1] += p[k]/4;

	wtcorr = maskwtcorrs(detpos, srcpos, corrs);
	if(weight > 0) weights[npoly] = (double) weight*wtcorr;
	else weights[npoly] = weight;
	/* headas_chat(5, "  weight[old]=%f wtcorr=%f weight[new]=%f\n", 
	   (double) weight, wtcorr, weights[npoly]); */

	npoly ++;
      }
    }   /* cellj loop */
  }     /* celli loop */


  return npoly;
}

