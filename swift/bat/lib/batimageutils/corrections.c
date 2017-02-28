#include <fitsio.h>
#include <math.h>
#include <string.h>   /* Only source detection */
#include "imageutils.h"

/* ============================================================= */
/* 
 * image_stripbits - remove low-order bits from image
 *
 * This routine removes low-order (insignificant) bits from the image.
 * The number of bits stripped is primarily defined by striptype.
 * This function is used to remove irrelevant bits, to aid in the
 * compression of output images.  It must be the last transformation
 * applied before writing the data to disk.
 *
 * When striptype is set to STRIP_VAR, then varimg is consulted.  At
 * least keepbits bits of the image noise are preserved.  If a pixel
 * value is larger than the noise, those bits are preserved, plus the
 * noise bits.  Thus the same absolute level of quantization noise
 * should be present throughout the whole image, including bright
 * point sources.
 *
 * If STRIP_VAR is set, but varimg is null, then the variance is
 * assume to be 1.
 *
 * If STRIP_REL is set, then varimg is ignored.  keepbits significant
 * bits are kept everywhere, which means that the image will have the
 * same relative precision everywhere, but bright point sources will
 * have lower absolute precision.  [ However, this option is good for
 * smoothly varying maps with no point sources. ]
 *
 * int keepbits - number of bits of original image to keep
 * struct image_struct *outimg - image to be stripped
 * struct image_struct *varimg - variance map, used to 
 *   determine the minimum number of bits to preserve.
 *   If striptype == STRIP_VAR, but varimg is null, then
 *   the "variance" is assumed to be 1.
 * int striptype - type of stripping to do (see above)
 * 
 * RETURNS: 0
 */

int image_stripbits(int keepbits,
		    struct image_struct *outimg,
		    struct image_struct *varimg,
		    int striptype, 
		    FLOAT nullval)
{		    
  FLOAT bitfact;
  FLOAT im, vim, nim, fr;
  int ex0, ex1;
  int i;

  /* Determine the right bit-extraction functions, depending on the 
     image data type */
#if (FLOAT == double)
#define frexpF(value,exp) frexp(value,exp)
#define ldexpF(value,exp) ldexp(value,exp)
#else
#define frexpF(value,exp) frexpf(value,exp)
#define ldexpF(value,exp) ldexpf(value,exp)
#endif

  if (keepbits <= 0) return 0;

  /* ------------------ */
  /* Keep the same number of bits everwhere */
  if (striptype == STRIP_REL) {
    bitfact = (1 << keepbits);

    for(i=0; i < (outimg->axes[0]*outimg->axes[1]); i++) {
      im = outimg->data[i];
      if (im == nullval) continue;
      fr = (FLOAT) rint(frexpF(im,&ex0) * bitfact) / bitfact;
      outimg->data[i] = ldexpF(fr,ex0);
    }
    return 0;
  }

  /* ------------------ */
  /* The number of bits relative to the local variance */
  if (striptype == STRIP_VAR) {
    for(i=0; i < (outimg->axes[0]*outimg->axes[1]); i++) {
      im = outimg->data[i];

      /* Default variance is 1.0 */
      vim = (varimg)?(varimg->data[i]):(1.0);
      if (vim == nullval || im == nullval || vim == 0) continue;
      nim = im/vim;

      fr = frexpF(nim,&ex0);
      ex0 += keepbits;
      if (ex0 < 0) ex0 = 0;
      bitfact = (1 << ex0);

      fr = (FLOAT) rint(frexpF(im,&ex1) * bitfact) / bitfact;
      outimg->data[i] = ldexpF(fr,ex1);
    }
    return 0;
  }

  return -1;
}
