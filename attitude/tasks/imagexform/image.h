#ifndef IMAGE_INCLUDED

#include "fitsio.h"

#define IMAGEDATATYPE double

#define BLANKVALUE DOUBLENULLVALUE
#define isBlank(x) (x == BLANKVALUE)


typedef struct {

int bitpix;

IMAGEDATATYPE** data;

long dimenx;
long dimeny;

double zero;
double scale;

long blank;
int has_blank;

} IMAGE;

/***********************************************************************************
*
***********************************************************************************/
IMAGE* allocateImage(long dimenx, long dimeny, int bitpix);

/*****************************************************************************
*
*****************************************************************************/
IMAGE* allocateSimilarImage(long dimenx, long dimeny, IMAGE* image);

/*****************************************************************************
*
*****************************************************************************/
void destroyImage(IMAGE* image);

/*********************************************************************************
*
*********************************************************************************/
void* getImageDataPointer(IMAGE* image);

/*********************************************************************************
*
*********************************************************************************/
int getFITSImageDataType(IMAGE* image);

/**********************************************************************************
*
**********************************************************************************/
IMAGE* readFITSImage(fitsfile* fp, double null);

/**********************************************************************************
*
**********************************************************************************/
void writeFITSImage(fitsfile* fp, IMAGE* image);

/*********************************************************************************
*
*********************************************************************************/
void setImagePixel(IMAGE* image, int i, int j, double value);

/*********************************************************************************
*
*********************************************************************************/
double getImagePixel(IMAGE* image, int i, int j);

/*********************************************************************************
*
*********************************************************************************/
double interpolateImagePixel(IMAGE* image, double x, double y);


#define IMAGE_INCLUDED
#endif /* IMAGE_INCLUDED */
