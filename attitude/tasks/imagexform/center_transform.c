#include <math.h>

#include "methods.h"
#include "image.h"
#include "att_fatal.h"
#include "coord.h"
#include "overlap.h"
#include "headas.h"


typedef struct
{
  IMAGE * image;
  PARAM * param;
  double blank;
  char * bitvec;
} CenterTransform;


static void
apply_transform (PARAM * param, double * x, double * y, double i, double j)
{
  applyXform2dToContinuousCoords(param->trans, x, y, i, j);

  if (param->nonlinear)
    {
        /********************************
	* apply a non-linear correction *
	********************************/
      double tmp_x, tmp_y;

      applyMapXform(param->nonlinear, &tmp_x, &tmp_y, *x, *y);

      *x = tmp_x;
      *y = tmp_y;
    }
}


static void
setTransformedPixel (CenterTransform * transform, int x, int y, double z)
{
  IMAGE * image = transform->image;
  if (x>=0 && x< image->dimenx && y>=0 && y< image->dimeny)
    {
      int pos, byte, bit;
      pos = y * image->dimenx + x;
      byte = pos / 8;
      bit = pos % 8;
      setImagePixel(image, x, y, z);
      transform->bitvec[byte] |= 1 << bit;
    }
}


#ifdef BLANK_UNSET_PIXELS
static void
blank_unset_pixels (CenterTransform * transform, int byte)
{
  int bit;
  int flags = transform->bitvec[byte];
  PARAM * param = transform->param;

  for (bit = 0; bit < 8; ++bit)
    if (!(flags & (1 << bit)))
      {
        int pos, x, y;
        pos = byte * 8 + bit;
        y = pos / param->dimenx;
        x = pos % param->dimenx;
        setImagePixel(transform->image, x, y, transform->blank);
      }
}
#endif


static void
overlap_callback (int x, int y, OverlapState * state)
{
  CenterTransform * transform = (CenterTransform *) state->user;
  setTransformedPixel(transform, x, y, transform->blank);
}


static void
propagate_blanks (CenterTransform * transform, int i, int j)
{
  Quad q;   /* corners of input pixel i,j transformed */
  PARAM * param = transform->param;

  apply_transform(param, &q.a.x, &q.a.y, i - 0.5, j - 0.5);

  apply_transform(param, &q.b.x, &q.b.y, i + 0.5, j - 0.5);

  apply_transform(param, &q.c.x, &q.c.y, i + 0.5, j + 0.5);

  apply_transform(param, &q.d.x, &q.d.y, i - 0.5, j + 0.5);

  iterate_overlap(&q, overlap_callback, transform);
}


/******************************************************************************
* The "center" transform method moves all of the counts associated with a
* given input pixel to the output pixel containing the transformed input
* pixel center.
******************************************************************************/
void
transform_by_center (fitsfile * fpin, fitsfile * fpout, PARAM * param)
{
  CenterTransform transform = { 0 };
  IMAGE *original;
  IMAGE *transformed;

  int i, j;
  int bitveclen;

  transform.param = param;
  transform.blank = BLANKVALUE;

/**************************
* read the original image *
**************************/
  headas_chat(1, "Reading image\n");
  original = readFITSImage(fpin, transform.blank);
  if (original == NULL)
    {
      fprintf(stderr, "Could not read input image\n");
      att_fatal(1);
    }

/*********************************
* allocate the transformed image *
*********************************/
  transformed = allocateSimilarImage(param->dimenx, param->dimeny, original);
  if (transformed  == NULL)
    {
      fprintf(stderr, "Could not allocate transformed image\n");
      att_fatal(1);
    }
  transform.image = transformed;

  bitveclen = param->dimenx * param->dimeny / 8 + 1;
  transform.bitvec = calloc(1, bitveclen);

/***************************
* transform all the pixels *
***************************/
  headas_chat(1, "Transforming image\n");

  for (j = 0; j < original->dimeny; ++j)
    {
      headas_chat(4, "Transforming row %d", j);

      for (i = 0; i < original->dimenx; ++i)
        {

          double x, y;
          double origval;

          origval = getImagePixel(original, i, j);

          if (isBlank(origval))
            {
              if (param->zero_nulls)
                {
                  /* nothing to do */
                }
              else
                {
                  /* BLANK out all output pixels which are affected
                   * by this input pixel */
                  propagate_blanks(&transform, i, j);
                }
            }
          else
            {
              int xi, yi;
              double transval;
              apply_transform(param, &x, &y, i, j);
              xi = floor(x + 0.5);
              yi = floor(y + 0.5);
              transval = getImagePixel(transformed, xi, yi);
              if (!isBlank(transval))
                {
                  /* add to current value (usually zero?) */
                  setTransformedPixel(&transform, xi, yi, transval + origval);
                  headas_chat(5, "from %d %d to %g %g value=%g\n",
                              i, j, x, y, origval);
                }
              else
                {
                  /* landed on top of a blank pixel */
                }
            }
        }
    }

#ifdef BLANK_UNSET_PIXELS
  if (!param->zero_nulls)
    {
      int i;
      for (i = 0; i < bitveclen; ++i)
        if (transform.bitvec[i] != 0xff)
          blank_unset_pixels(&transform, i);
    }
#endif

/******************************
* write the transformed image *
******************************/
  headas_chat(1, "Writing transformed image\n");
  writeFITSImage(fpout, transformed);

  destroyImage(original);
  destroyImage(transformed);

}  /* end of transform_by_center function */

