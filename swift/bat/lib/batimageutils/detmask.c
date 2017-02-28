#include "imageutils.h"

/* 
 * Make sure there are detector gaps in detector mask (arbitrary value)
 *
 * struct image_struct *image - pre-allocated image, which will be modified
 *            so that positions of BAT detector gaps are filled with val.
 *            User is responsible for allocating and freeing.
 * float val - value to place in cells which are gaps.
 * RETURNS: nothing.  Image is modified in place 
 */
void image_mkdetgaps_val(struct image_struct *image, float val)
{
  /* Positions of gaps in X (columns) ... */
  int xgaps[] = {
    16,  17,  34,  35,  52,  53,  70,  71,  88,  89, 106, 107, 124, 125, 142,
    143, 160, 161, 178, 179, 196, 197, 214, 215, 232, 233, 250, 251, 268, 269};
  /* ... and Y (rows) */
  int ygaps[] = {
    8,   9,  10,  19,  20,  21,  30,  31,  32,  41,  42,  43,  52,  53,  54,
    63,  64,  65,  74,  75,  76,  85,  86,  87,  96,  97,  98, 107, 108, 109,
    118, 119, 120, 129, 130, 131, 140, 141, 142, 151, 152, 153, 162, 163, 164};
  int nxgaps, nygaps;
  int nx, ny;
  int i, j;

  nxgaps = sizeof(xgaps)/sizeof(xgaps[0]);
  nygaps = sizeof(ygaps)/sizeof(ygaps[0]);
  nx = image->axes[0];
  ny = image->axes[1];
  
  /* Carve out the detector gaps, along DETX ... */
  for (i=0; i<nxgaps; i++) {
    if (xgaps[i] < nx) for (j=0; j<ny; j++) 
      image->datap[j][xgaps[i]] = val;
  }
  /* ... and DETY */
  for (j=0; j<nygaps; j++) {
    if (ygaps[j] < ny) for (i=0; i<nx; i++) 
      image->datap[ygaps[j]][i] = val;
  }

  return;
}

/* 
 * Make sure there are detector gaps in detector mask (0 fill)
 *
 * struct image_struct *image - pre-allocated image, which will be modified
 *            so that positions of BAT detector gaps are filled with 0s.
 *            User is responsible for allocating and freeing.
 * RETURNS: nothing.  Image is modified in place 
 */
void image_mkdetgaps(struct image_struct *image)
{
  image_mkdetgaps_val(image, 0);
  return;
}


/* 
 * Create a mask array from the known gap positions.  Assume all else is good
 *
 * struct image_struct *base - image on which dimensions of the mask
 *         are based, or 0.
 * int nx - number of columns of image, only if base == 0
 * int ny - number of rows of image, only if base == 0
 *
 * RETURNS: mask image.  User is responsible to free
 */
struct image_struct *image_mkdetmask(struct image_struct *base, 
				     int nx, int ny)
{
  struct image_struct *detmask = 0;

  if (base != 0) {
    nx = base->axes[0];
    ny = base->axes[1];
  }
  
  detmask = image_init(nx, ny, 0);
  /* Default is enabled */
  foreach_pixel(detmask, i, {detmask->data[i] = 1;});
  image_mkdetgaps(detmask);

  return detmask;
}
