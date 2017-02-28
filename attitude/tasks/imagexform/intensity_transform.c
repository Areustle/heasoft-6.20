#include "methods.h"
#include "image.h"
#include "att_fatal.h"
#include "coord.h"
#include "headas.h"

/********************************************************************************
*
********************************************************************************/
void transform_by_intensity(fitsfile* fpin, fitsfile* fpout, PARAM* param) {

IMAGE* original;
IMAGE* transformed;
 
int i,j;
double x,y;
double value;
double null;

/***********************************************
* determine the value to use for nulls in the
* input image
***********************************************/
if(param->zero_nulls) null=0.0;
else                  null=BLANKVALUE;

/**************************
* read the original image *
**************************/
headas_chat(1, "Reading image\n");
original = readFITSImage(fpin, null);
if(original==NULL) {
    fprintf(stderr, "Could not read input image\n");
    att_fatal(1);
}

/*********************************
* allocate the transformed image *
*********************************/
transformed = allocateSimilarImage(param->dimenx, param->dimeny, original);


/***************************
* transform all the pixels *
***************************/
headas_chat(1, "Transforming image\n");

for(j=0; j<transformed->dimeny;  ++j) {

    headas_chat(4, "Transforming row %d", j);
    for(i=0; i<transformed->dimenx; ++i) {

        /********************************************
        * get the coordinates in the original image *
	    ********************************************/
        applyXform2dToContinuousCoords(param->trans, &x, &y,
	                                    (double)i, (double)j);
	if(param->nonlinear) {
	    /********************************
	    * apply a non-linear correction *
	    ********************************/
	    double tmp_x, tmp_y;

	    applyMapXform(param->nonlinear, &tmp_x, &tmp_y, x, y);

	    x=tmp_x;
	    y=tmp_y;
	}

	/************************************************
	* write the interpolated value to the new image *
	************************************************/
        value = interpolateImagePixel(original, x,y);
	setImagePixel(transformed, i,j, value);
	
	if(value != 0.0)
	headas_chat(5, "from %g %g to %d %d value=%g\n", x,y, i, j, value);
    }
}

/******************************
* write the transformed image *
******************************/
headas_chat(1, "Writing transformed image\n");
writeFITSImage(fpout, transformed);


} /* end of transform_by_intensity function */
