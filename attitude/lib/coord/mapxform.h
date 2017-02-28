#ifndef MAPXFORM_INCLUDED

#include "xform2d.h"

#define MAPXFORM_TYPE float /* data type of map array elements */

/****************************************************************************
* A MAPXFORM structure contains the information needed to specify an
* arbitrary transform between one set of two dimensional coordinates to 
* another. The transform is specified by a map of deltax and deltay adjustments.
*
* The map arrays are stored with X cycling fastest. There is a mechanism for
* arbitrary scaling and coordinate offsets between the transform coordinates
* and the pixel coordinates. However, we assume that the pixel values
* apply to the lower left hand corner of the pixel. i.e. the value
* of the first pixel would apply at pixel coordinate (0,0).
* The map arrays are interpolated linearly and extrapolated linearly outside
* their boundaries.
*******************************************************************************/
typedef struct {

int dimenx;
int dimeny;

MAPXFORM_TYPE**deltax;
MAPXFORM_TYPE**deltay;

XFORM2D* to_pixels;


} MAPXFORM;

/****************************************************************************
*****************************************************************************
* allocate memory for a MAPXFORM structure whose map has given dimensions
****************************************************************************/
MAPXFORM* allocateMapXform(int dimenx, int dimeny);

/****************************************************************************
*****************************************************************************
* allocate a new MAPXFORM structure with the same dimensions and
* axis scaling and offsets as an existing one
****************************************************************************/
MAPXFORM* allocateMapXformWithSameAxes(MAPXFORM* old);

/****************************************************************************
*****************************************************************************
* Create a new MAPXFORM which specified the same transform as the original
* but which occupies completely separate memory
****************************************************************************/
MAPXFORM* cloneMapXform(MAPXFORM* map);

/****************************************************************************
*****************************************************************************
* free memory for a MAPXFORM structure
****************************************************************************/
void destroyMapXform(MAPXFORM* map);

/* Print a MAPXFORM structure to a stream. Since the data dimensions
   can be large, set max_rows = 0 to display all rows, or set max_rows
   >= 1 to display only a certain number of rows. Set max_columns
   analogously to display some or all columns. */
void printMapXform(MAPXFORM* map, FILE* stream, int max_columns, int max_rows);

/***************************************************************************
****************************************************************************
* returns a pointer to the first pixel in the deltax array.
* this is useful for, say reading the array from a FITS file
***************************************************************************/
MAPXFORM_TYPE* getMapXformDeltaxBlock(MAPXFORM* map);

/***************************************************************************
****************************************************************************
* returns a pointer to the first pixel in the deltay array.
* this is useful for, say reading the array from a FITS file
***************************************************************************/
MAPXFORM_TYPE* getMapXformDeltayBlock(MAPXFORM* map);

/***************************************************************************
****************************************************************************
* set transformation from coordinate to be transformed to map pixel
* coordinates
***************************************************************************/
void setMapXformAxes(MAPXFORM* map, double origin_x, double scale_x,
                                    double origin_y, double scale_y );



/***************************************************************************
****************************************************************************
* apply the map transform to (xo,y0) to produce (x1,y1).
* Returns 1 if the point was outside the map array boundaries and
* returns 0 otherwise.
***************************************************************************/
int applyMapXform(MAPXFORM* map, double *x1, double *y1,
                                 double  x0, double y0  );


/***************************************************************************
****************************************************************************
* Take a mapped transform "old" and apply a linear transform "trans"
* and produce a mapped transform "new", such that
* trans(old(x,y)) = new(trans(x,y)). In otherwords, "new" applies
* to the linear transformed space instead of to the pre-linear transform space.
* The transform is done in place - i.e. the original MAPXFORM is modified
******************************************************************************/
void applyXform2dToMapXform(MAPXFORM* map, XFORM2D* trans);

/****************************************************************************
* Given a set of offsets at randomly sampled points, this function fills
* in the deltax and deltay arrays for a MAPXFORM so as to approximate as set
* of randomly spaced offsets. Inverse distance weighting is used to interpolate
* the unevenly spaced input data.
****************************************************************************/
void setMapXformFromListOfPoints(MAPXFORM* map, double* x, double* y,
                                           double* deltax, double* deltay,
                                           int npoints );


/********************************************************************************
* Given a MAPXform set another MAPXFORm to be the inverse. Note that this
* inversion introduces innaccuracy from interpolation.
********************************************************************************/
void invertMapXform(MAPXFORM* inverse, MAPXFORM* map);



#define MAPXFORM_INCLUDED
#endif /* MAPXFORM_INCLUDED */
