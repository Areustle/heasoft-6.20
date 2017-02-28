#include <string.h>
#include <fitsio.h>
#include <pil.h>
#include "imageutils.h"
#include "batcelldetect.h"

/*
 * coco - coordinate conversion 
 *   
 * It is possible to convert directly from one coord system
 * to another without going through pixels.
 * 
 * wcs - array of pointers to wcsprm structures;
 * altwcs - indices to wcs array, one for each alternate WCS coord sys;
 * from - coord sys to convert from ('@'=primary; 'A'-'Z' alternate;
 *           '.' = pixel; '+' = tangent plane or intermediate);
 * to - coord sys to convert to;
 * (in1,in2) - input coordinates
 * (*out1,*out2) - output coordinates
 * status - conversion status
 */
int coco(struct wcsprm *wcs, int altwcs[27], int from, int to, 
	 double in1, double in2, double *out1, double *out2,
	 int *status)
{
  double pixcrd[2], imgcrd[2], phi[1], theta[1], world[2];
  int statp[1];
  int iwcs;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status) return *status;
  if (wcs == 0 || altwcs == 0 || out1 == 0 || out2 == 0) {
    return (*status = NULL_INPUT_PTR);
  }

  if (from == to) {
    /* Identity transformation */
    *out1 = in1;
    *out2 = in2;
    return 0;
  } else if (from == '.') {
    /* Pixel to world transformation */
    if (to == '+' && altwcs['T'-'@'] == -1) {
      *out1 = (in1 - wcs[0].crpix[0])*wcs[0].cdelt[0] / 57.295780;
      *out2 = (in2 - wcs[0].crpix[1])*wcs[0].cdelt[1] / 57.295780;
    } else {
      pixcrd[0] = in1;      pixcrd[1] = in2;
      iwcs = (to == '+') ? (altwcs['T'-'@']) : (altwcs[to-'@']);
      *status = wcsp2s(&wcs[iwcs], 1, 2, pixcrd, imgcrd, phi, theta, world, statp);
      if (*status == 0) {
	*out1 = world[0];
	*out2 = world[1];
      }
    }
  } else if (to == '.') {
    /* World to pixel transformation */
    if (from == '+' && altwcs['T'-'@'] == -1) {
      *out1 = (in1 * 57.295780 / wcs[0].cdelt[0]) + wcs[0].crpix[0];
      *out2 = (in2 * 57.295780 / wcs[0].cdelt[1]) + wcs[0].crpix[1];
    } else {
      world[0] = in1;      world[1] = in2;
      iwcs = (from == '+') ? (altwcs['T'-'@']) : (altwcs[from-'@']);
      *status = wcss2p(&wcs[iwcs], 1, 2, world, phi, theta, imgcrd, pixcrd, statp);
      if (*status == 0) {
	*out1 = pixcrd[0];
	*out2 = pixcrd[1];
      }
    }
  } else {
  /* Direct world to world transformation */
    coco(wcs, altwcs, from, '.', in1, in2, &(pixcrd[0]), &(pixcrd[1]), status);
    coco(wcs, altwcs, '.',  to,  pixcrd[0], pixcrd[1], out1, out2,     status);
  }
    
  return *status;
}


#if 0
/* OBSOLETE OBSOLETE */
int fits_pix_to_world1(double xpix, double ypix, double xref, double yref,
      double xrefpix, double yrefpix, double xinc, double yinc, double rot,
      char *type, double *xpos, double *ypos, int *status)
{
  struct wcsprm wcs;
  int naxis = 2;
  double pixcrd[2], imgcrd[2], phi, theta, world[2];
  int stat0 = 0, stat[2] = {0,0};

  if (*status) return *status;

  /* Initialize wcs structure */
  memset(&wcs, 0, sizeof(wcs));
  wcs.flag = -1;
  if (wcsini(1, naxis, &wcs)) {
    return (*status = MEMORY_ALLOCATION);
  }

  wcs.flag = 0;
  wcs.naxis = 2;
  wcs.crpix[0] = xrefpix; wcs.crpix[1] = yrefpix;
  wcs.cdelt[0] = xinc;    wcs.cdelt[1] = yinc;
  wcs.crval[0] = xref;    wcs.crval[1] = yref;
  wcs.lonpole = 180;      wcs.latpole  = 90;   /* Default for zenithal */
  wcs.pc[0] = 1; wcs.pc[1] = 0; wcs.pc[2] = 0; wcs.pc[3] = 1;
  wcs.npv = 0;
  wcs.nps = 0;
  wcs.altlin = 4;
  wcs.crota[0] = 0;       wcs.crota[1] = rot;
  sprintf(wcs.ctype[0], "XLON%s    ", type);
  sprintf(wcs.ctype[1], "XLAT%s    ", type);

  pixcrd[0] = xpix; pixcrd[1] = ypix;
  stat0 = wcsp2s(&wcs, 1, naxis, pixcrd, 
		 imgcrd, &phi, &theta, world, stat);
  wcsfree(&wcs);

  if (stat0 || stat[0]) return (*status = 501);
  *xpos = world[0];     *ypos = world[1];

  return 0;
}


int fits_world_to_pix1(double xpos, double ypos, double xref, double yref, 
      double xrefpix, double yrefpix, double xinc, double yinc, double rot,
      char *type, double *xpix, double *ypix, int *status)
{
  struct wcsprm wcs;
  int naxis = 2;
  double pixcrd[2], imgcrd[2], phi, theta, world[2];
  int stat0 = 0, stat[2] = {0,0};

  if (*status) return *status;

  /* Initialize wcs structure */
  memset(&wcs, 0, sizeof(wcs));
  wcs.flag = -1;
  if (wcsini(1, naxis, &wcs)) {
    return (*status = MEMORY_ALLOCATION);
  }

  wcs.flag = 0;
  wcs.naxis = 2;
  wcs.crpix[0] = xrefpix; wcs.crpix[1] = yrefpix;
  wcs.cdelt[0] = xinc;    wcs.cdelt[1] = yinc;
  wcs.crval[0] = xref;    wcs.crval[1] = yref;
  wcs.lonpole = 180;      wcs.latpole  = 90;   /* Default for zenithal */
  wcs.pc[0] = 1; wcs.pc[1] = 0; wcs.pc[2] = 0; wcs.pc[3] = 1;
  wcs.npv = 0;
  wcs.nps = 0;
  wcs.altlin = 4;
  wcs.crota[0] = 0;       wcs.crota[1] = rot;
  sprintf(wcs.ctype[0], "XLON%s    ", type);
  sprintf(wcs.ctype[1], "XLAT%s    ", type);

  world[0] = xpos; world[1] = ypos;
  stat0 = wcss2p(&wcs, 1, naxis, world,
		 &phi, &theta, imgcrd, pixcrd, stat);
  wcsfree(&wcs);

  if (stat0 || stat[0]) return (*status = 501);
  *xpix = pixcrd[0];     *ypix = pixcrd[1];

  return 0;
}

int fits_pix_to_world2(double xpix, double ypix, 
		       struct wcsprm *wcs,
		       double *xpos, double *ypos, int *status)
{
  double pixcrd[2], imgcrd[2], phi[1], theta[1], world[2];
  int statp[1];

  if (*status) return (*status);

  pixcrd[0] = xpix;
  pixcrd[1] = ypix;
  *status = wcsp2s(wcs, 1, 2, pixcrd, imgcrd, phi, theta, world, statp);
  *xpos = world[0]; 
  *ypos = world[1];

  return *status;
}

int fits_world_to_pix2(double xpos, double ypos, 
		       struct wcsprm *wcs,
		       double *xpix, double *ypix, int *status)
{
  double pixcrd[2], imgcrd[2], phi[1], theta[1], world[2];
  int statp[1];

  if (*status) return (*status);

  world[0] = xpos;
  world[1] = ypos;
  *status = wcss2p(wcs, 1, 2, world, phi, theta, imgcrd, pixcrd, statp);
  *xpix = pixcrd[0];
  *ypix = pixcrd[1];

  return *status;
}
#endif
