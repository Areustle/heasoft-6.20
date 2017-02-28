#include "image.h"



/***********************************************************************************
* Allocate storage for an image. This function is mostly for internal use
* instead call allocateSimilarImage or readFITSImage
***********************************************************************************/
IMAGE* allocateImage(long dimenx, long dimeny, int bitpix) {

IMAGE* image;
int i;

image = (IMAGE*)malloc(sizeof(IMAGE));

image->dimenx = dimenx;
image->dimeny = dimeny;

image->bitpix = bitpix;



image->data    = (double**)malloc(sizeof(double*)*dimeny);
*(image->data) = (double* )calloc(dimenx*dimeny, sizeof(double )); /*init to 0*/
for(i=1; i<dimeny; ++i) {
    image->data[i] = image->data[i-1] + dimenx;
}


return image;
} /* end of allocateImage function */

/*****************************************************************************
*
*****************************************************************************/
IMAGE* allocateSimilarImage(long dimenx, long dimeny, IMAGE* image) {

IMAGE* copy;

copy = allocateImage(dimenx, dimeny, image->bitpix);

copy->zero  = image->zero;
copy->scale = image->scale;

copy->blank     = image->blank;
copy->has_blank = image->has_blank;

return copy;

} /* end of allocateSimilarImage function */

/*****************************************************************************
*
*****************************************************************************/
void destroyImage(IMAGE* image) {

    free(*(image->data));
    free(image->data);
    free(image);
}

/*********************************************************************************
*
*********************************************************************************/
void* getImageDataPointer(IMAGE* image) {
  
    return (void*)*(image->data);
}

/*********************************************************************************
*
*********************************************************************************/
int getFITSImageDataType(IMAGE* image) {

    return TDOUBLE;
}

/**********************************************************************************
*
**********************************************************************************/
IMAGE* readFITSImage(fitsfile* fp, double null) {

int status=0;
int bitpix;
long dimen[2];

int anynull=0;

IMAGE* image;

/************************************
* read the image size and data type *
************************************/
fits_get_img_type(fp, &bitpix, &status);
fits_get_img_size(fp, 2, dimen, &status);

if(status) {
    fits_report_error(stderr, status);
    return NULL;
}

/*****************************
* create the IMAGE structure *
*****************************/
image = allocateImage(dimen[0], dimen[1], bitpix);

/**************************************
* read the scaling for integer images *
**************************************/
if(bitpix>0) {
    int tmp = status;

    /* set error mark since these are optional */
    fits_write_errmark();

    /********
    * BZERO *
    ********/
    fits_read_key_dbl(fp, "BZERO", &(image->zero), NULL, &status);
    if(status==KEY_NO_EXIST) {
        image->zero=0.0;
        status=0;
    }

    /*********
    * BSCALE *
    *********/
    fits_read_key_dbl(fp, "BSCALE", &(image->scale), NULL, &status);
    if(status==KEY_NO_EXIST) {
        image->scale=1.0;
        status=0;
    }

    /********
    * BLANK *
    ********/
    image->has_blank = 1;
    fits_read_key_lng(fp, "BLANK", &(image->blank), NULL, &status);
    if(status==KEY_NO_EXIST) {
        image->has_blank=0;
        status=0;
    }

    /* restore previous state */
    status = tmp;
    fits_clear_errmark();
} /* end if this is an integer image */


/****************
* read the data *
****************/
fits_read_img(fp, getFITSImageDataType(image), 1l, dimen[0]*dimen[1],
              &null, getImageDataPointer(image), &anynull, &status);

if(status) {
    fits_report_error(stderr, status);
    return NULL;
}

return image;

} /* end of readFITSImage method */


/**********************************************************************************
*
**********************************************************************************/
void writeFITSImage(fitsfile* fp, IMAGE* image) {

int status=0;
long dimen[2];

dimen[0] = image->dimenx;
dimen[1] = image->dimeny;

fits_create_img(fp, image->bitpix, 2, dimen, &status);

/**************************************
* write the scaling for integer images *
**************************************/
if(image->bitpix>0 && (image->zero != 0.0 || image->scale != 1.0) ) {

    fits_update_key(fp, TDOUBLE, "BZERO", &(image->zero),
                   "pixel scaling zero point", &status);

    fits_update_key(fp, TDOUBLE, "BSCALE", &(image->scale),
                   "pixel scaling", &status);

} /* end if this is an integer image */

/*******************************
* blank keyword if appropriate *
*******************************/
if(image->bitpix>0 && image->has_blank) {
    fits_update_key(fp, TLONG, "BLANK", &(image->blank),
                   "Pixel value used for nulls", &status);
}

/*****************
* write the data *
*****************/
{
    IMAGEDATATYPE blank = BLANKVALUE;

    fits_write_imgnull(fp, getFITSImageDataType(image), 1l, dimen[0]*dimen[1],
                       getImageDataPointer(image), &blank, &status);
}

fits_report_error(stderr, status);
       

} /* end of readFITSImage method */



/*********************************************************************************
*
*********************************************************************************/
void setImagePixel(IMAGE* image, int i, int j, double value) {

if(i>=0 && i< image->dimenx && j>=0 && j< image->dimeny)
		image->data[j][i]=value;

} /* end of setImagePixel method */


/*********************************************************************************
*
*********************************************************************************/
double getImagePixel(IMAGE* image, int i, int j) {

if(i>=0 && i< image->dimenx && j>=0 && j< image->dimeny)
	return image->data[j][i];
else
	return 0.0;

} /* end of getImagePixel method */

/*********************************************************************************
*
*********************************************************************************/
double interpolateImagePixel(IMAGE* image, double x, double y) {

int i,j;
double xhat, yhat;
double a00, a01, a10, a11;
double value;

if(x<0.0 || y<0.0 ) return 0.0;

i=(int)x;
j=(int)y;

if(i>=image->dimenx || j>=image->dimeny) return 0.0;

xhat = x-i;
yhat = y-j;

a00 = getImagePixel(image, i  , j  );
a10 = getImagePixel(image, i+1, j  );
a01 = getImagePixel(image, i  , j+1);
a11 = getImagePixel(image, i+1, j+1);

if (isBlank(a00) || isBlank(a10) || isBlank(a01) || isBlank(a11))
    value = BLANKVALUE;
else
    value = xhat*yhat*(a11 - a01 - a10 + a00)
         +   xhat*(a10 - a00 )
         +   yhat*(a01 - a00 )
         +         a00;

return value;

} /* end of interpolateImagePixel function */

