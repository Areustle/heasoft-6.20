typedef struct {

float** data;
int dimenx;
int dimeny;

} IMAGE;


/****************************************************************************
* constructor
****************************************************************************/
IMAGE* allocateImage(int dimenx, int dimeny );


/****************************************************************************
* read an image from the given FITS file
****************************************************************************/
IMAGE* readImage(char* filename, int status);


/****************************************************************************
* write an image to the given FITS file
****************************************************************************/
int writeImage(IMAGE* im, char* filename);


/****************************************************************************
* returns the closest x coordinate which is withing the image
****************************************************************************/
int xWithinImage(IMAGE* im, int x);


/****************************************************************************
* returns the closest y coordinate which is withing the image
****************************************************************************/
int yWithinImage(IMAGE* im, int y);


/****************************************************************************
* smooth the image over a square box sized just large enough so that the sum
* of the pixels it covers is >= min.
* Pixels with zero or negative exposure are set to zero
* returns the result in a new image
****************************************************************************/
IMAGE* adaptivelySmoothImageWithExposure(IMAGE* im, IMAGE* expo, 
                            float min, float min_expo, int status);
