#include <stdlib.h>
#include <math.h>
#include "batmask_op.h"
#include "headas.h"

/* 
 * Forward project the corners of an array of detector pixels onto the
 * projection plane below the detector plane.
 *
 * The position of the source is given by srcpos[], and the
 * orientation of the shielding detector tops by the dettopplane parameter.
 * A bounding box is given by (xmin,ymin) and (xmax,ymax).
 * 
 * The user is responsible for allocating enough space for polygons
 * polygons should be a pointer to double, 8*nmaxpoly;
 *
 * The layout of a single polygon is {X0,X1,X2,X3,Y0,Y1,Y2,Y3}.
 * 
 * double srcpos[3] - position of the source in BAT coords (cm);
 * struct blockdettop_struct *dettopplane - detector tops position and orientation;
 * double xmin, xmax, ymin, ymax - bounding box of detector plane (cm);
 * double zproj - BAT_Z height of projection plane;
 * poly - pointer to polygon store;
 * nmaxpoly - maximum number of allocated polygons.
 */

/* RETURNS: number of converted polygons
 *          -1 upon failure
 *
 * $Id: forwdettop.c,v 1.1 2003/10/28 19:14:08 krimm Exp $
 *
 *
 */

int forwdettop(double srcpos[3], struct blockdettop_struct *dettopplane, 
	     double xmin, double xmax, double ymin, double ymax,
	     double zproj, 
	     double *poly, int nmaxpoly,
	     struct batmaskcorrections_struct *corrs)
{
  double *p;
  double coffs[4][2];
  double cx, cy,   cellx, celly;
  double xdt, ydt, zdt;     /* Intersection of ray with detector top */
  double xsdt, ysdt, zsdt;  /* Vector from detector top to source */
  double zddt, rat;       /* zddt = Zproj - Zdettop;  rat = zddt/zsdt */
  double xdtmin, xdtmax, ydtmin, ydtmax;          /* Bounding box at detector top */
  int cellimin, cellimax, celljmin, celljmax; /* Bounding detector top pixels */
  /*double detpos[3];*/
  int ncells, npoly = 0;
  int k;
  int celli, cellj;
  
  if (nmaxpoly <= 0) return -1;

  cx = dettopplane->cellsize[0];
  cy = dettopplane->cellsize[1];
  /* Positions of all detector corners, with respect to one corner,
     MUST be in CLOCKWISE order! */
  coffs[0][0] = 0.0; coffs[0][1] = 0.0;
  coffs[1][0] = 0.0; coffs[1][1] =cy-0.02;
  coffs[2][0] =cx-0.02; coffs[2][1] =cy-0.02;
  coffs[3][0] =cx-0.02; coffs[3][1] = 0.0;

  /* Pre-compute some valuable quantities */
  /* Z Position of cell (cdt) with respect to BAT coordinates */
  zdt  = dettopplane->centpos[2] + dettopplane->meanpos[2];
  zsdt = srcpos[2]-zdt;

  /* Z component from detector top to projection plane */
  zddt = zproj-zdt; 
  rat = zddt/zsdt;

  /* First compute the bounding box 
     ASSUMES NO mask translation, rotation, etc. */
  xdtmin = xmin - (srcpos[0]-xmin)*zddt/(srcpos[2]-zproj);
  ydtmin = ymin - (srcpos[1]-ymin)*zddt/(srcpos[2]-zproj);
  xdtmax = xmax - (srcpos[0]-xmax)*zddt/(srcpos[2]-zproj);
  ydtmax = ymax - (srcpos[1]-ymax)*zddt/(srcpos[2]-zproj);
  cellimin = floor((xdtmin-dettopplane->cell0[0])/cx);
  celljmin = floor((ydtmin-dettopplane->cell0[1])/cy);
  cellimax = ceil((xdtmax-dettopplane->cell0[0])/cx);
  celljmax = ceil((ydtmax-dettopplane->cell0[1])/cy);

  /* Range check */
  if (cellimin < 0) cellimin = 0;
  if (celljmin < 0) celljmin = 0;
  if (cellimax >= dettopplane->ncells[0]) cellimax = dettopplane->ncells[0]-1;
  if (celljmax >= dettopplane->ncells[1]) celljmax = dettopplane->ncells[1]-1;

  ncells = (celljmax-celljmin+1)*(cellimax-cellimin+1);
  if (ncells == 0) return 0;

  npoly = 0;

  p = poly;
  for (cellj = celljmin; cellj <= celljmax; cellj++) {
    if (cellj % 11 > 7) continue;    /* Skip over gaps in Y */
    for (celli = cellimin; celli <= cellimax; celli++) {
      if (celli % 18 > 15) continue;    /* Skip over gaps in X */
      if (npoly >= nmaxpoly) return -1;
    
      /* X/Y Position of cell (cdt) with respect to the block corner */
      cellx = celli * cx + dettopplane->cell0[0];
      celly = cellj * cy + dettopplane->cell0[1];
      
      /* X/Y Position of cell (cdt) with respect to BAT coordinates */
      xdt = cellx + dettopplane->centpos[0] + dettopplane->meanpos[0];
      ydt = celly + dettopplane->centpos[1] + dettopplane->meanpos[1];
      
      /* Vector which points from detector top location to source position (cdt) */
      xsdt = srcpos[0]-xdt;    ysdt = srcpos[1]-ydt;    
      
      /* Here is where tilt/rot/warp code would go */
      
      /* Loop over every corner - first X then Y */
      for (k=0; k<4; k++){
        *p++ = xdt+coffs[k][0] + (xsdt-coffs[k][0])*rat;
      }
      for (k=0; k<4; k++){
        *p++ = ydt+coffs[k][1] + (ysdt-coffs[k][1])*rat;
      }
/* ?  This doesn't seem to do anything...
      detpos[0] = 0; detpos[1] = 0;
      detpos[2] = zproj;
      for (k=-8; k<(-4); k++) detpos[0] += p[k]/4;
      for (k=-4; k<(0); k++)  detpos[1] += p[k]/4;
 */   
      npoly ++;
    }   /* celli loop */
  }     /* cellj loop */


  return npoly;
}

