#define BUFSIZE	255

#include <math.h>
#include <fitsio.h>
#include <longnam.h>
#include <cftools.h>	/* Required to use c_fcerr, c_fcecho, etc. */
#include "image.h"

/****************************************************************************
* constructor
****************************************************************************/
IMAGE* allocateImage(int dimenx, int dimeny) {

IMAGE* im;
int i;

im = (IMAGE*)malloc(sizeof(IMAGE));

im->dimenx = dimenx;
im->dimeny = dimeny;

im->data   =(float**)malloc(sizeof(float*)*dimeny);
im->data[0]=(float* )malloc(sizeof(float )*dimenx*dimeny);

for(i=1;i<dimeny;++i) {
    im->data[i] = im->data[i-1]+dimenx;
}

return im;

} /* end of allocateImage function */


/****************************************************************************
* read an image from the given FITS file
****************************************************************************/
IMAGE* readImage(char* filename, int status) {

IMAGE* im;
int anynull=0;
fitsfile* fp;
long dimenx, dimeny;
char context[BUFSIZE];


/****************************
* open file and read header *
****************************/
fits_open_file(&fp, filename, READONLY, &status);
if(status != OK)
{
	strcpy(context,"Failed to open FITS file.");
	c_fcerr(context);
}

fits_read_key_lng(fp, "NAXIS1", &dimenx, NULL, &status);
fits_read_key_lng(fp, "NAXIS2", &dimeny, NULL, &status);
if(status != OK)
{
	strcpy(context,"Failed read NAXIS keywords.");
	c_fcerr(context);
}

/***********************************
* allocate image and read the data *
***********************************/
im=allocateImage((int)dimenx, (int)dimeny);

fits_read_img_flt(fp, 0l, 1l, dimenx*dimeny, 0., im->data[0], &anynull, 
                  &status);
if(status != OK)
{
	strcpy(context,"Failed to read primary array.");
	c_fcerr(context);
}


fits_close_file(fp,&status);
if(status != OK)
{
	strcpy(context,"Failed to close FITS file.");
	c_fcerr(context);
}

return im;

} /* end of readImage function */

/****************************************************************************
* write an image to the given FITS file
****************************************************************************/
int writeImage(IMAGE* im, char* filename) {

int status=0;
fitsfile* fp;
long naxes[2];
char context[BUFSIZE];

/*******************************
* create file and write header *
*******************************/
fits_create_file(&fp, filename, &status);
if(status != OK)
{
	strcpy(context,"Failed to open output file.");
	c_fcerr(context);
	return status;
}

naxes[0] = (long)(im->dimenx);
naxes[1] = (long)(im->dimeny);
fits_create_img(fp, -32, 2, naxes, &status);
if(status != OK)
{
	strcpy(context,"Failed to create primary array.");
	c_fcerr(context);
	return status;
}

/***********************
* write the image data *
***********************/
fits_write_img_flt(fp, 0l, 1l, (long)(im->dimenx*im->dimeny), 
                   im->data[0], &status);
if(status != OK)
{
	strcpy(context,"Failed to write primary array.");
	c_fcerr(context);
	return status;
}

fits_close_file(fp,&status);
if(status != OK)
{
	strcpy(context,"Failed to close output file.");
	c_fcerr(context);
}

return status;

} /* end of writeImage function */


/****************************************************************************
* returns the closest x coordinate which is withing the image
****************************************************************************/
int xWithinImage(IMAGE* im, int x) {

if(x<0) return 0;
if(x>=im->dimenx) return im->dimenx-1;
return x;

} /* end of xWithinImage function */

/****************************************************************************
* returns the closest y coordinate which is withing the image
****************************************************************************/
int yWithinImage(IMAGE* im, int y) {

if(y<0) return 0;
if(y>=im->dimeny) return im->dimeny-1;
return y;

} /* end of yWithinImage function */




/****************************************************************************
* smooth the image over a square box sized just large enough so that the sum
* of the pixels it covers is >= min.
* Pixels with zero or negative exposure are set to zero
* returns the result in a new image
****************************************************************************/
IMAGE* adaptivelySmoothImageWithExposure(IMAGE* im, IMAGE* expo, 
                               float min, float min_expo, int status) {

IMAGE* smoothed;
int dimenx, dimeny;

int i,j;
int ii,jj;
int radius;

float total, norm;

char context[BUFSIZE];

/*********************************
* make sure the dimensions match *
*********************************/
if(im->dimenx != expo->dimenx || 
   im->dimeny != expo->dimeny   ) {
	strcpy(context,"Image and exposure mapd simensions differ.");
	c_fcerr(context);
	status = -10001;
	return status;
}

/******************************
* allocate the smoothed image *
******************************/
smoothed = allocateImage(im->dimenx, im->dimeny);

/***********************
* loop over all pixels *
***********************/
for(j=0; j<im->dimeny; ++j) {
    for(i=0; i<im->dimenx; ++i) {

        /**************************************
        * set pixels with no exposure to zero *
        **************************************/
        if(expo->data[j][i]<=min_expo ) {
            smoothed->data[j][i]=0.0;
            continue;
        }

        total =   im->data[j][i];
        norm  = expo->data[j][i];
        for(radius=1; total<min && radius<im->dimenx+im->dimeny; ++radius) {

            /*********
            * bottom *
            *********/
            jj=j-radius;
            if(jj==yWithinImage(im,jj) ) {
                for(ii = xWithinImage(im,i-radius); 
                    ii<= xWithinImage(im,i+radius); ++ii) {

                    total +=   im->data[jj][ii];
                    norm  += expo->data[jj][ii];
                }
            }

            /******
            * top *
            ******/
            jj=j+radius;
            if(jj==yWithinImage(im,jj) ) {
                for(ii = xWithinImage(im,i-radius); 
                    ii<= xWithinImage(im,i+radius); ++ii) {

                    total +=   im->data[jj][ii];
                    norm  += expo->data[jj][ii];
                }
            }

            /*******
            * left *
            *******/
            ii=i-radius;
            if(ii==xWithinImage(im,ii) ) {
                for(jj = yWithinImage(im,j-radius); 
                    jj<= yWithinImage(im,j+radius); ++jj) {

                    total +=   im->data[jj][ii];
                    norm  += expo->data[jj][ii];
                }
            }

            /********
            * right *
            ********/
            ii=i+radius;
            if(ii==xWithinImage(im,ii) ) {
                for(jj = yWithinImage(im,j-radius); 
                    jj<= yWithinImage(im,j+radius); ++jj) {

                    total +=   im->data[jj][ii];
                    norm  += expo->data[jj][ii];
                }
            }
        } /* end of loop over expanding radius */

        /****************************************************
        * exposure correct and set the smoothed pixel value *
        ****************************************************/
        smoothed->data[j][i] = total/norm;

    } 
} /* end of loop over pixels */


return smoothed;

} /* end of adaptivelySmoothImageWithExposure function */
