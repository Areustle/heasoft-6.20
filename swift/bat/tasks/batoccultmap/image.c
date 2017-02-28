/*
 * image.c
 *
 * image-based algorithm for deriving exposure maps 
 *
 * $Id: image.c,v 1.3 2006/02/24 18:35:18 craigm Exp $
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
/* Convert pixel coordinates to RA/DEC */
int allpixconvert(int nonulls, int nx, int ny,
		  struct image_struct *image, FLOAT nullval,
		  double *img,
		  struct wcsprm *wcsdata,
		  double *sin_rapix, double *cos_rapix,
		  double *sin_decpix, double *cos_decpix)
{
  /* ------------------------------- */
  /* Precompute the RA/Dec for each image pixel */
  double *pixcrd, *imgcrd, *world, *theta, *phi;
  double rapix, decpix;
  int *statp;
  int i, j, k, kk;
  int status = 0;

  pixcrd = (double *) malloc(sizeof(double)*nonulls*2);
  world  = (double *) malloc(sizeof(double)*nonulls*2);
  imgcrd = (double *) malloc(sizeof(double)*nonulls*2);
  theta  = (double *) malloc(sizeof(double)*nonulls);
  phi    = (double *) malloc(sizeof(double)*nonulls);
  statp   = (int *)    malloc(sizeof(int)*nonulls);
  
  /* Remove nulls, transfer to the format that wcsp2s wants */
  k = 0;
  kk = 0;
  for (j=0; j<ny; j++) {
    for (i=0; i<nx; i++) {
      if (image->datap[j][i] != nullval) {
	pixcrd[kk] = i+1;
	pixcrd[kk+1] = j+1;
	kk+=2;
	k++;
      }
    }
  }
  
  /* Actual pixel transformation routine */
  status = wcsp2s(&wcsdata[0], nonulls, 2, pixcrd, imgcrd, theta, phi, world, statp);
  if (status) {
    fprintf(stderr, "ERROR: WCS transformation failed with status %d\n",
	    status);
    status = WCS_ERROR;
    return status;
  }
	
  /* Extract the values into arrays that this routine wants*/
  for (kk=0,k=0; k<nonulls; k++) {
    rapix = world[kk+wcsdata[0].lng]*rad2deg;
    decpix = world[kk+wcsdata[0].lat]*rad2deg;
    sin_rapix[k]=sin(rapix);
    cos_rapix[k]=cos(rapix);
    sin_decpix[k]=sin(decpix);
    cos_decpix[k]=cos(decpix);
    kk += 2;
  }
  free(pixcrd);
  free(world);
  free(imgcrd);
  free(theta);
  free(phi);
  free(statp);

  return 0;
}


int doimagemap(int nonulls, int ntseg, double *img, 
	       double *sin_rapix, double *cos_rapix,
	       double *sin_decpix, double *cos_decpix,
	       double *dt, double *alt, 
	       double *earth_ra, double *earth_dec,
	       double *moon_ra, double *moon_dec,
	       double *sun_ra, double *sun_dec,
	       double earth_radius, double sun_radius, double moon_radius,
	       double atmosphere,
	       int do_earth, int do_moon, int do_sun
	       )
{
  double cos_earth, cos_sun, cos_moon;
  double sin_earth_ra, cos_earth_ra, sin_earth_dec, cos_earth_dec;
  double sin_moon_ra, cos_moon_ra, sin_moon_dec, cos_moon_dec;
  double sin_sun_ra, cos_sun_ra, sin_sun_dec, cos_sun_dec;
  double cos_pix;
  int i, k;
  int sub = 0;

  cos_sun=cos(sun_radius/60*rad2deg);    /* sun_radius in arcmin */
  cos_moon=cos(moon_radius/60*rad2deg);  /* moon_radius in arcmin */

  /* ------------------------------- */
  /* Main heavy-duty processing loop */
  /*    - loop over every pixel, and compute cos(angle) between the pixel 
	position and the body.  Do this for each body.
  */
  for (k=0; k<ntseg; k++) {

    cos_earth = cos(asin((earth_radius+atmosphere)/alt[k]));
    sin_earth_ra = sin(earth_ra[k]*rad2deg);
    cos_earth_ra = cos(earth_ra[k]*rad2deg);
    sin_earth_dec = sin(earth_dec[k]*rad2deg);
    cos_earth_dec = cos(earth_dec[k]*rad2deg);   
    sin_moon_ra = sin(moon_ra[k]*rad2deg);
    cos_moon_ra = cos(moon_ra[k]*rad2deg);
    sin_moon_dec = sin(moon_dec[k]*rad2deg);
    cos_moon_dec = cos(moon_dec[k]*rad2deg);
    sin_sun_ra = sin(sun_ra[k]*rad2deg);
    cos_sun_ra = cos(sun_ra[k]*rad2deg);
    sin_sun_dec = sin(sun_dec[k]*rad2deg);
    cos_sun_dec = cos(sun_dec[k]*rad2deg);
    
    for (i=0; i<nonulls; i++)  {
      sub = 0;
      
      if (do_earth) {
	cos_pix=sin_decpix[i]*sin_earth_dec+cos_decpix[i]*cos_earth_dec
	  *(cos_rapix[i]*cos_earth_ra+sin_rapix[i]*sin_earth_ra);
	if (cos_pix>=cos_earth) { 
	  sub = 1;
	}
      }
      
      if (!sub && do_moon) {
	cos_pix=sin_decpix[i]*sin_moon_dec+cos_decpix[i]*cos_moon_dec
	  *(cos_rapix[i]*cos_moon_ra+sin_rapix[i]*sin_moon_ra);
	if (cos_pix>=cos_moon) {
	  sub = 1;
	}
      }
      
      if (!sub && do_sun) {
	cos_pix=sin_decpix[i]*sin_sun_dec+cos_decpix[i]*cos_sun_dec
	  *(cos_rapix[i]*cos_sun_ra+sin_rapix[i]*sin_sun_ra);
	if (cos_pix>=cos_sun) {
	  sub = 1;
	}
      }
      
      if (sub) {
	img[i]-=dt[k];
      }
    }
  } /* End of time loop */

  return 0;
}

