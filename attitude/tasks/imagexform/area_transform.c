#include <math.h>

#include "methods.h"
#include "headas.h"
#include "att_fatal.h"
#include "overlap.h"
#include "image.h"


typedef struct
{
  PARAM * param;
  IMAGE * image;
  long input_width;
  double value;
  double blank;
} AreaTransform;


/*******************************************************************************
*
*******************************************************************************/
static void
apply_transform (PARAM * param, double * x, double * y, double ix, double iy)
{
  applyXform2dToContinuousCoords(param->trans, x, y, ix, iy);

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
}                               /* end of transform_single_event function */


static void
blank_overlap_callback (int x, int y, OverlapState * state)
{
  AreaTransform * transform = (AreaTransform *) state->user;
  setImagePixel(transform->image, x, y, transform->blank);
}


static void
distribute_overlap_callback (int x, int y, OverlapState * overlap)
{
  AreaTransform * transform = (AreaTransform *) overlap->user;
  double z = getImagePixel(transform->image, x, y);
  if (!isBlank(z) && transform->value != 0)
    {
      double zhat = z + transform->value * overlap->weight;
      setImagePixel(transform->image, x, y, zhat);
    }
}


/*******************************************************************************
* This can be used to paint all the output pixels touched by a given input
* pixel with the blank value, or to distribute the value of the input pixel
* among the output pixels it is transformed onto.
*******************************************************************************/
static void
distribute_value_or_null (AreaTransform * transform, int i, int j, int valnul)
{
  Quad q;            /* corners of input pixel i,j transformed */
  PARAM * param = transform->param;

  apply_transform(param, &q.a.x, &q.a.y, i - 0.5, j - 0.5);

  apply_transform(param, &q.b.x, &q.b.y, i + 0.5, j - 0.5);

  apply_transform(param, &q.c.x, &q.c.y, i + 0.5, j + 0.5);

  apply_transform(param, &q.d.x, &q.d.y, i - 0.5, j + 0.5);

  if (valnul)
    iterate_overlap(&q, distribute_overlap_callback, transform);
  else
    iterate_overlap(&q, blank_overlap_callback, transform);
}



/****************************************************************************
* iterator work function for transforming a real valued image using the area
* method.
* The transformation is done by looping over each pixel, transforming the
* corners of that pixel to the new coordinate system, then distributing the
* input value in proportion to the fractional overlap with output pixels.
****************************************************************************/
int
transform_by_area_work (long total_rows, long offset,
                       long first_pixel, long npixels,
                       int ncols, iteratorCol * column, void * info)
{
  long pix;
  double * pixels;
  int noblanks;
  AreaTransform * transform = (AreaTransform *) info;
  PARAM * param = transform->param;

  headas_chat(4, "Work function called first_pixel=%ld\n", first_pixel);

/********************************
* get the array of pixel values *
********************************/
  pixels = (double *) fits_iter_get_array(column);
  if (transform->blank != pixels[0])
    headas_chat(4, "received null value %e\n", pixels[0]);
  noblanks = pixels[0] == 0;

#ifdef DEBUG
    printXform2d(param->trans, stdout);
#endif

/************************************************
* loop over all the pixels in the current chunk *
************************************************/
  for (pix = 1; pix <= npixels; ++pix)
    {
      int i, j;

      i = (pix + first_pixel - 2) % transform->input_width;
      j = (pix + first_pixel - 2) / transform->input_width;

      transform->value = pixels[pix];
      if (!noblanks && (pixels[pix] == pixels[0]))
        {
          /********************************
          * the input pixel is null/blank *
          ********************************/
          if (param->zero_nulls)
            {
              /* no affect on output */
            }
          else
            {
              /*************************************************************
              * set everything to null that could be tainted by this pixel *
              *************************************************************/
              distribute_value_or_null(transform, i, j, 0);
            }
        }  /* end if this is a null/blank pixel */
      else if (transform->value != 0)
        {
           /* distribute this pixel's value among overlapped pixels */
           distribute_value_or_null(transform, i, j, 1);
        }
    }

/************************************
* have to return 0 to show we're OK *
************************************/
  return 0;
}  /* end of transform_by_area iterator work function */


void
transform_by_area(fitsfile * fpin, fitsfile * fpout, PARAM * param)
{
  int status = 0;
  AreaTransform transform = { 0 };
  int bitpix;
  iteratorCol column;

  transform.param = param;
  transform.blank = BLANKVALUE;

  fits_get_img_type(fpin, &bitpix, &status);
  if (status)
    {
      fprintf(stderr, "Error while reading BITPIX from %s\n", param->infile);
      fits_report_error(stderr, status);
      att_fatal(status);
    }

/*******************************************************
* base the output data type on the input image         *
* (always floating point, but only double if input is) *
*******************************************************/

  if (bitpix != DOUBLE_IMG)
    bitpix = FLOAT_IMG;

  transform.image = allocateImage(param->dimenx, param->dimeny, bitpix);

/***************************************************************
* determine the width of the image in pixels, which we will
* use to convert pixel offsets to coordinates in the iterator
* work function
***************************************************************/
  fits_read_key_lng(fpin, "NAXIS1", &transform.input_width, NULL, &status);
  if (status)
    {
      fprintf(stderr, "Error while reading NAXIS1 from %s\n", param->infile);
      fits_report_error(stderr, status);
      att_fatal(status);
    }

/******************************************************
* initialize the "column" data structure.
* The CFITSIO iterator treats image data as a column
******************************************************/
  fits_iter_set_by_num(&column, fpin, 0, TDOUBLE, InputCol);

/****************************************
* call the iterator to do the real work *
****************************************/
  fits_iterate_data(1, &column, 0l /* don't skip any rows */ ,
                    0l /* read optimum columns per iteration */ ,
                    transform_by_area_work, &transform, &status);
  if (status)
    {
      fprintf(stderr, "Error from iterator\n");
      fits_report_error(stderr, status);
      att_fatal(status);
    }

  writeFITSImage(fpout, transform.image);

/* deallocate memory */
  destroyImage(transform.image);
}                               /* end of transform_by_events function */

