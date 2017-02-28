#include <fitsio.h>
#include <string.h>
#include <math.h>
#include "imageutils.h"
#include "batfftsg.h"

/*
 *   * FFT and convolution
 *
 * $Id: imagefft.c,v 1.1 2003/01/17 17:37:35 craigm Exp $
 * C. Markwardt
 *
 * 17 Jan 2003 - separated from imageutils.c (CM)
 *
 */

/* Use correct FFT, single or double precision */
#if ISFLOAT
#define rdft2d srdft2d
#endif

/* 
 * Perform Ooura FFT
 *
 * FFT is performed in place (i.e. destructively).  Depending on the
 * image flags, the image is either taken from the spatial to Fourier
 * domain, or the Fourier to spatial domain.
 *
 * The result is normalized such that two successive applications of
 * image_fft() should result in the original image.
 * 
 * struct image_struct *image - image to be transformed
 */
void image_fft(struct image_struct *image)
{
  int i, j;
  int nxx, nyy;

  if (image == 0) return;

  nxx = image->axes[0]; 
  nyy = image->axes[1]; 
  
  if (image->flags & IMGFLAG_FOURIER) {
    /* In Fourier domain, perform inverse */
    rdft2d(nyy, nxx, -1, image->datap, image->t, image->ip, image->w);
    /* Renormalize */
    for (j=0; j<nyy; j++) {
      for (i=0; i<nxx; i++) {
	image->datap[j][i] *= 2.0 / (nxx*nyy);
      }
    }
    image->flags &= ~IMGFLAG_FOURIER;  /* Remove Fourier flag */
  } else {
    /* In spatial domain, perform Fourier transform to Fourier domain */
    rdft2d(nyy, nxx, 1, image->datap, image->t, image->ip, image->w);
    image->flags |= IMGFLAG_FOURIER;   /* Add Fourier flag */
  }    

  return;
}
