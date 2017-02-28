/*
 * contour.c
 *
 * contour-based algorithm for deriving exposure maps 
 *
 * $Id: contour.c,v 1.5 2006/05/24 19:15:47 craigm Exp $
 *
 */

#include <fitsio.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "headas_gti.h"

#include "batoccultmap.h"

/* ----------------------------------------------------------------- */
/* poscircle - make a circle of radius theta degrees, centered on
   (ra,dec) with nsegs line segments */
int poscircle(double ra, double dec, double theta, int nsegs,
	      double *radeccirc)
{
  double phi, cos_ra, sin_ra, cos_dec, sin_dec;
  double ux, uy, uz;
  int i;

  /* headas_chat(5,"RA=%f DEC=%f THETA=%f\n", ra, dec, theta);*/
  cos_ra = cos(ra/deg2rad);
  sin_ra = sin(ra/deg2rad);
  cos_dec = cos(dec/deg2rad);
  sin_dec = sin(dec/deg2rad);
  theta = theta / deg2rad;
  for(i=0; i<nsegs; i++) {
    phi = 360.0 * i / ((double)nsegs-1) / deg2rad;
    
    ux =  cos(phi)*sin(theta)*sin_dec*cos_ra+cos_dec*cos(theta)*cos_ra-sin_ra*sin(phi)*sin(theta);
    uy =  cos(phi)*sin(theta)*sin_dec*sin_ra+cos_dec*cos(theta)*sin_ra+cos_ra*sin(phi)*sin(theta);
    uz = -cos(phi)*sin(theta)*cos_dec       +sin_dec*cos(theta);
  
    radeccirc[2*i+0] = atan2(uy,ux)/rad2deg;
    radeccirc[2*i+1] = asin(uz)/rad2deg;
    /* headas_chat(5," %d %f %f\n", i, radeccirc[2*i+0], radeccirc[2*i+1]); */
  }

  return 0;
}
      

/* ----------------------------------------------------------------- */
/* Simple Bresenham line algorithm */
/* Render a line in image using pixel value, from (x0,y0) to (x1,y1) */
/* Upon return, number of pixels is *npix, and mean {x,y} values are
 * {*isum,*jsum} */
void bresline(struct image_struct *image, FLOAT value,
	      int x0, int y0, int x1, int y1, 
	      long int *npix, long int *isum, long int *jsum)
{
  int dy = y1 - y0;
  int dx = x1 - x0;
  int stepx, stepy;
  int nx, ny;

  if (dy < 0) { dy = -dy;  stepy = -1; } else { stepy = 1; }
  if (dx < 0) { dx = -dx;  stepx = -1; } else { stepx = 1; }
  dy <<= 1;                                               /* dy is now 2*dy*/
  dx <<= 1;                                               /* dx is now 2*dx*/
  nx = image->axes[0];
  ny = image->axes[1];

  if((x0 >=0) && (x0 < nx) && (y0 >= 0) && (y0 < ny)) {
    image->datap[y0][x0] = value;
    (*isum) += x0;
    (*jsum) += y0;
    (*npix) ++;
  }
  if (dx > dy) {
    int fraction = dy - (dx >> 1);               /* same as 2*dy - dx */
    while (x0 != x1) {
      if (fraction >= 0) {
	y0 += stepy;
	fraction -= dx;                          /* same as fraction -= 2*dx */
      }
      x0 += stepx;
      fraction += dy;                            /* same as fraction -= 2*dy */
      if((x0 >=0) && (x0 < nx) && (y0 >= 0) && (y0 < ny)) {
	image->datap[y0][x0] = value;
	(*isum) += x0;
	(*jsum) += y0;
	(*npix) ++;
      }

    }
  } else {
    int fraction = dx - (dy >> 1);
    while (y0 != y1) {
      if (fraction >= 0) {
	x0 += stepx;
	fraction -= dy;
      }
      y0 += stepy;
      fraction += dx;
      if((x0 >=0) && (x0 < nx) && (y0 >= 0) && (y0 < ny)) {
	image->datap[y0][x0] = value;
	(*isum) += x0;
	(*jsum) += y0;
	(*npix) ++;
      }

    }
  }
}


/* ----------------------------------------------------------------- */
/* Use the flood fill algorithm to find all pixels connected to this
   source */
int  floodfill(struct image_struct *image,
	       int x, int y, 
	       FLOAT srcval, FLOAT destval, int depth,
	       long int *npix)
{
  int xstart;
  int nx = image->axes[0], ny = image->axes[1];

  if (image->datap[y][x] == srcval) {
    while (y>0 && image->datap[y-1][x] == srcval) y--;
    if (image->datap[y][x] != srcval) y++;
  }
  
  if (image->datap[y][x] == srcval) {
    while (x>0 && image->datap[y][x-1] == srcval) x--;
    if (image->datap[y][x] != srcval) x++;
  }

  for (xstart=x; x < nx && image->datap[y][x] == srcval; x++) {
    image->datap[y][x] = destval;
    *npix += 1;
  }
  for (x--;x>=xstart;x--) {
    if (y > 0 &&      image->datap[y-1][x] == srcval) floodfill(image,x,y-1,srcval,destval,depth+1, npix);
    if (y < (ny-1) && image->datap[y+1][x] == srcval) floodfill(image,x,y+1,srcval,destval,depth+1, npix);
  }

  return 0;
}

/* ----------------------------------------------------------------- */
/* Pre-compute the celestial positions of the border pixels */
int bordcalc(struct image_struct *image,
	     struct wcsprm *wcsdata,
	     int *nborderpix0, double **bradec, double **bijpix)
{
  double *pixcrd, *imgcrd, *world, *theta, *phi;
  int *statp;
  int nborderpix;
  int i, j, ii;
  int status;

  nborderpix = 2*(image->axes[0]+image->axes[1]);

  world  = (double *) malloc(sizeof(double)*nborderpix*2);
  pixcrd = (double *) malloc(sizeof(double)*nborderpix*2);
  imgcrd = (double *) malloc(sizeof(double)*nborderpix*2);
  theta  = (double *) malloc(sizeof(double)*nborderpix);
  phi    = (double *) malloc(sizeof(double)*nborderpix);
  statp   = (int *)    malloc(sizeof(int)*nborderpix);

  /* Fill in the border around the image.  Remember to use the FITS
     convention where pixel centers start at 1.0 */
  ii = 0;
  for (i=0; i<image->axes[0]; i++) {
    pixcrd[ii++] = i + 1;
    pixcrd[ii++] = 0 + 1;
  }
  for (j=0; j<image->axes[1]; j++) {
    pixcrd[ii++] = image->axes[0];
    pixcrd[ii++] = j + 1;
  }
  for (i=image->axes[0]-1; i>= 0; i--) {
    pixcrd[ii++] = i + 1;
    pixcrd[ii++] = image->axes[1];
  }
  for (j=image->axes[1]-1; j>= 0; j--) {
    pixcrd[ii++] = 0 + 1;
    pixcrd[ii++] = j + 1;
  }

  /* Convert to celestial coordinates */
  status = wcsp2s(&wcsdata[0], nborderpix, 2, pixcrd, 
		  imgcrd, phi, theta, world, statp);

  /* Check for bogus values, and also convert to radians */
  for (i=0; i<nborderpix; i++) {
    if (statp[i]) {
      world[2*i+0] = world[2*i+1] = -1e38;
    } else {
      world[2*i+0] /= deg2rad;
      world[2*i+1] /= deg2rad;
    }
  }

  /* Fill in the return values */
  *nborderpix0 = nborderpix;
  *bradec = world;
  *bijpix = pixcrd;

  free(imgcrd);
  free(theta);
  free(phi);
  free(statp);

  return 0;
}
	     

/* ----------------------------------------------------------------- */
int contcalc(struct image_struct *result, FLOAT value,
	    struct wcsprm *wcsdata,
	    double ra_body, double dec_body, double radius, 
	    int nsegs,
	    int nborderpix, double *bradec, double *bijpix)
{
  double *pixcrd, *imgcrd, *world, *theta, *phi;
  int *statp;
  double cos_theta;
  int i, status = 0;
  int i0, i1, j0, j1;
  long int isum = 0, jsum = 0, nsum = 0;
  int icent, jcent;
  volatile int debug = 0;

  pixcrd = (double *) malloc(sizeof(double)*nsegs*2);
  world  = (double *) malloc(sizeof(double)*nsegs*2);
  imgcrd = (double *) malloc(sizeof(double)*nsegs*2);
  theta  = (double *) malloc(sizeof(double)*nsegs);
  phi    = (double *) malloc(sizeof(double)*nsegs);
  statp   = (int *)    malloc(sizeof(int)*nsegs);

  poscircle(ra_body, dec_body, radius, nsegs, world);
  
  /* Actual pixel transformation routine */
  status = wcss2p(&wcsdata[0], nsegs, 2, world, phi, theta, imgcrd, pixcrd, statp);
  
  for (i=1; i<nsegs; i++) {
    if (statp[i]   != 0) continue;  /* Exclude the out-of-bounds points */
    if (statp[i-1] != 0) continue;  /* Exclude the out-of-bounds points */
    /* Convert from FITS convention to C convention for pixel labeling */
    i0 = rint(pixcrd[2*i-2])-1;
    i1 = rint(pixcrd[2*i+0])-1;
    j0 = rint(pixcrd[2*i-1])-1;
    j1 = rint(pixcrd[2*i+1])-1;

    if((j0 < 0 && j1 < 0) || (j0 >= result->axes[1] && j1 >= result->axes[1]) ||
       (i0 < 0 && i1 < 0) || (i0 >= result->axes[0] && i1 >= result->axes[0]))
      continue;
	
    /* headas_chat(5, "line (%d,%d) -> (%d,%d)   [%f,%f]  [%f,%f]\n", i0, j0, i1, j1,
       world[2*i+0], world[2*i+1], ra_body, dec_body);  */
    bresline(result, value, i0, j0, i1, j1, &nsum, &isum, &jsum);
  }
  free(pixcrd);
  free(world);
  free(imgcrd);
  free(theta);
  free(phi);
  free(statp);

  /* Draw border if needed */
  cos_theta = cos(radius/deg2rad);
  for (i=0; i<nborderpix; i++) {
    double bordra, borddec, cd;

    bordra  = bradec[2*i+0];
    borddec = bradec[2*i+1];
    if (bordra < -1e37 || borddec < -1e37) continue;
    
    cd = sin(borddec)*sin(dec_body/deg2rad) +
      cos(borddec)*cos(dec_body/deg2rad)*cos(bordra-ra_body/deg2rad);

    if (cd >= cos_theta) {
      i0 = (int) bijpix[2*i+0] - 1;
      j0 = (int) bijpix[2*i+1] - 1;
      
      result->datap[j0][i0] = value;
      isum += i0;
      jsum += j0;
      nsum ++;
    }
  }
  
  /* Find the center of mass of the set of points we have selected */
  if (nsum == 0) {
    /* headas_chat(5, "no occultation\n"); */
    return 0;
  }
  icent = isum / nsum;
  jcent = jsum / nsum;
  /* Fill the array with good values */
  /* headas_chat(5, "flood %4d %4d %6d\n", icent, jcent, nsum); */
  if (debug) {
    fitsfile *dbgfile = 0;
    int status = 0;
    fits_create_file(&dbgfile, "!debug.img", &status);
    image_write(dbgfile, result, &status);
    fits_close_file(dbgfile, &status);
  }
  floodfill(result, icent, jcent, 0.0, value, 1, &nsum);
  
  return nsum;
}

int docontourmap(int ntseg, struct image_struct *sky,
		 int nborderpix, double *bradec, double *bijpix,
		 struct wcsprm *wcsdata,
		 double *dt, double *alt, 
		 double *earth_ra, double *earth_dec,
		 double *moon_ra, double *moon_dec,
		 double *sun_ra, double *sun_dec,
		 double earth_radius, double sun_radius, double moon_radius,
		 double atmosphere,
		 int do_earth, int do_moon, int do_sun
		 )
{
  int k;
  double radius;
  int npix;
  struct image_struct *scratch;

  /* Initialize the scratch image */
  scratch = image_copy(sky);
  foreach_pixel(scratch,i,{scratch->data[i] = 0;});

  for (k=0; k<ntseg; k++) {

    npix = 0;
    
    /* Earth */
    if (do_earth) {
      radius = asin((earth_radius+atmosphere)/alt[k])/rad2deg;
      npix += contcalc(scratch, -dt[k], wcsdata, earth_ra[k], earth_dec[k], radius, 200,
		       nborderpix, bradec, bijpix);
    }
    
    /* Moon */
    if (do_moon) {
      radius = moon_radius/60;
      npix += contcalc(scratch, -dt[k], wcsdata, moon_ra[k], moon_dec[k], radius, 50,
		       nborderpix, bradec, bijpix);
    }
    
    /* Sun */
    if (do_sun) {
      radius = sun_radius/60;
      npix += contcalc(scratch, -dt[k], wcsdata, sun_ra[k], sun_dec[k], radius, 50,
		       nborderpix, bradec, bijpix);
    }
    
    if (npix > 0) {
      /* Only add this image if there was an occultation */
      foreach_pixel(sky,     i, { sky->data[i] += scratch->data[i]; });
      foreach_pixel(scratch,i,{scratch->data[i] = 0;});
    }
    headas_chat(5,"   (number of pixels occulted in time step = %d)\n",
		npix);
    
  } /* End of time loop */

  image_free(scratch);
  return 0;
}
