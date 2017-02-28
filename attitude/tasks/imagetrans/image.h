#ifndef IMAGE_INCLUDED

#include <math.h>
#include "fitsio.h"
#include "overlap.h"

#define NaN (getNaN())

#define IMAGEDATATYPE double

typedef struct ImageStruct IMAGE;

struct ImageStruct {

    int bitpix;

    IMAGEDATATYPE** data;
    char** is_null;
    
    int has_nulls;

    long dimenx;
    long dimeny;

    double zero;
    double scale;

    IMAGEDATATYPE out_of_bounds;
    
    double utility;
	long flags;
    
    int has_preferred_blank;
    int preferred_blank;

    IMAGE* weight;

};

/*************************************************************************
* produce an IEEE NaN value
*************************************************************************/
double getNaN();

/***********************************************************************************
* Allocate storage for an image. This function is mostly for internal use
* instead call allocateSimilarImage or readFITSImage
***********************************************************************************/
IMAGE* allocateImage(long dimenx, long dimeny, int bitpix);

/*****************************************************************************
* create a new image will all pixels initialized to zero.
* It will have the same scaling as the gievn image.
* if bitpix is non-zero, then the new image will have that bitpix value,
* otherwise it defaults to the same value as the given image.
*****************************************************************************/
IMAGE* allocateSimilarImage(long dimenx, long dimeny, IMAGE* image, int bitpix);

/*****************************************************************************
* frees data storage for an image structure
*****************************************************************************/
void destroyImage(IMAGE* image);

/********************************************************************************
* returns true if the given coordinates are within the image
********************************************************************************/
int isInImage(IMAGE* image, int i, int j);

/********************************************************************************
* returns true if the given pixel of the image has a null value.
* if the coordinates are out of bounds then it returns true only if the default
* out of bounds value is set to NaN
********************************************************************************/
int imageIsNull(IMAGE* image, int i, int j);

/********************************************************************************
* returns the pixel value at the given coordinates, or the default out of
* bounds value if the coordinates are not in the image. Returns NaN if the
* particular pixel is null.
********************************************************************************/
double getImagePixel(IMAGE* image, int i, int j);

/********************************************************************************
* Mark a particular pixel in the image as being null. Does nothing if the given
* coordinates are not in the image.
********************************************************************************/
void setImageNull(IMAGE* image, int i, int j);

/********************************************************************************
* Set the value of a given pixel. If the value is NaN, then the pixel will
* be marked as null. Does nothing if the coordinates are not in the image.
********************************************************************************/
void setImagePixel(IMAGE* image, int i, int j, double value);

/********************************************************************************
* adds the given value to the current value in the specified pixel.
* Does nothing if the coordinates are out of bounds or if the pixel is
* currently null. If the specified value is null, sets the current
* pixel to null.
********************************************************************************/
void incrementImagePixel(IMAGE* image, int i, int j, double value);

/*********************************************************************************
* Returns the CFITSIO data code for the variable used by this structure.
*********************************************************************************/
int getFITSImageDataType(IMAGE* image);

/**********************************************************************************
*
**********************************************************************************/
IMAGE* readFITSImage(fitsfile* fp, int zero_nulls);

/**********************************************************************************
*
**********************************************************************************/
void writeFITSImage(fitsfile* fp, IMAGE* image);

/*********************************************************************************
* return an interpolated pixel value. Outside the image we extrapolate,
* or really interpolate using the out_of_bounds values.
*********************************************************************************/
double interpolateImagePixel(IMAGE* image, double x, double y);


/***************************************************************************
* increment all the pixels under a quadrilateral by a given value
***************************************************************************/
void incrementQuadOnImage(IMAGE* image, const Quad* quad, double value);


/***************************************************************************
* Set all the pixels under a quadrilateral to a given value.
***************************************************************************/
void setQuadOnImage(IMAGE* image, const Quad* quad, double value);

/***************************************************************************
* Apply flags to all the pixels under a quadrilateral.
***************************************************************************/
void applyFlagsToImage (IMAGE* image, const Quad* quad, long flags);

/***************************************************************************
* Set all the pixels touched by the given quadrilateral to null.
***************************************************************************/
void setQuadOnImageNull(IMAGE* image, const Quad* quad);

/***************************************************************************
* suggest a value for the BLANK keyword to use when writing the image.
* Th eimage is not obligated to use this value, and will ignore it if it
* is not appropriate for the data storage, or if the value is used by
* valid pixels.
***************************************************************************/
void suggestBlankForImage(IMAGE* image, int blank);

#define IMAGE_INCLUDED
#endif /* IMAGE_INCLUDED */
