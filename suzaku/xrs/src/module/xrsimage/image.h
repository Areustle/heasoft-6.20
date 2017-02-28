#ifndef XRSIMAGE_IMAGE_INCLUDED

#include "fitsio.h"
#include "longnam.h"
#include "param.h"

#define IMAGE_DATA_TYPE int
#define IMAGE_FITS_DATA_TYPE TINT

typedef struct {

int dimenx;
int dimeny;

int npixels;

IMAGE_DATA_TYPE** data;

} IMAGE;


/*****************************************************************************
******************************************************************************
* alloctaer storage for a new image with given dimensions 
*****************************************************************************/
IMAGE* allocateImage(int dimenx, int dimeny);

/*****************************************************************************
******************************************************************************
* set all pixels in an image to a  given value (typically 0)
*****************************************************************************/
void blankImage(IMAGE* im, int value);

/*****************************************************************************
******************************************************************************
* increment by 'increment' all the pixels contained within a polygon
* defined by the  corners whose coordinates are given by
* xcorners,ycorners. This routine assumes that any horizontal line
* (y=const) will pass through the sides of this polygon at 
* most twice. The routine also assumes that the coordinates of the
* first corner (x[0],y[0]) are repeated in the last corner (x[n-1],y[n-1]).
* Note "n" gives the number of corners in the polygon and is one less
* than the dimendion of the coordinate arrays.
* the corners must be listed in order either clockwise or counter-clockwise
* around the polygon.
*****************************************************************************/
void incrementConvexPolygonInImage(IMAGE* im, double* x, double* y, int n,
                                   int increment);

/*****************************************************************************
******************************************************************************
* Write image to a new FITS file.
* Also copy keywords from the input event file and copy the entire GTI 
* extension to the image.
*****************************************************************************/
int writeImageToFITSfile(IMAGE* im, char* filename, 
                          fitsfile* eventfp, PARAM* param);


#define XRSIMAGE_IMAGE_INCLUDED
#endif /* XRSIMAGE_IMAGE_INCLUDED */
