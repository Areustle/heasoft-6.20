#include "methods.h"
#include "info.h"
#include "headas.h"
#include "random.h"
#include "att_fatal.h"
#include "overlap.h"

/*******************************************************************************
*
*******************************************************************************/
void transform_single_event(INFO* info, double* x, double* y,
                            int i, int j, double dx, double dy ) {


    applyXform2dToDiscreteCoords(info->trans, x, y, i, j, dx, dy);

	if(info->nonlinear) {
	    /********************************
	    * apply a non-linear correction *
	    ********************************/
	    double tmp_x, tmp_y;

	    applyMapXform(info->nonlinear, &tmp_x, &tmp_y, *x, *y);

	    *x=tmp_x;
	    *y=tmp_y;
	}

} /* end of transform_single_event function */

/*******************************************************************************
*
*******************************************************************************/
void events_set_null_callback(int i, int j, OverlapState * state) {

    INFO* info;

    info = (INFO*) state->user;

    if(i>=0 && info->dimenx && j>=0 && j<info->dimeny) {
        info->image[j][i] = info->blank;
    }

} /* end of events_set_null_callback function */


/*******************************************************************************
* paint all the output pixels touched by a given input pixel with the null
* value.
*******************************************************************************/
void transform_null_events(INFO* info, int i, int j) {

    double top_left_x , top_left_y;
    double top_right_x, top_right_y;
    double bottom_left_x , bottom_left_y;
    double bottom_right_x, bottom_right_y;
    
    Quad q;  /* corners of input pixel i,j transformed */


    /***************************************************************************
    * transform the corners of the input pixel to the output image coordinates *
    ***************************************************************************/
    transform_single_event(info, &top_left_x,      &top_left_y,    i, j, -0.5,  0.5);
    transform_single_event(info, &top_right_x,    &top_right_y,    i, j,  0.5,  0.5);
    transform_single_event(info, &bottom_left_x,  &bottom_left_y,  i, j, -0.5, -0.5);
    transform_single_event(info, &bottom_right_x, &bottom_right_y, i, j,  0.5, -0.5);

	q.a.x = top_left_x;       q.a.y = top_left_y;
	q.b.x = top_right_x;      q.b.y = top_left_y;
	q.c.x = bottom_right_x;   q.c.y = bottom_right_y;
	q.d.x = bottom_left_x;    q.d.y = bottom_left_y;

    iterate_overlap(&q, events_set_null_callback, info);

} /* end of transform_events_null function */

/****************************************************************************
*****************************************************************************
* iterator work function for transforming a counts image.
* The transformation is done by looping over each event, calculating
* a random position for that event within its pixel, transforming
* that position to the new coordinate system and then rebinning.
* This way the transformation is done the same way as a transformation
* applied to an event file. In particular the total number of counts is
* preserved in the transformed image.
****************************************************************************/
int transform_by_events_work(long total_rows, long offset,
                             long first_pixel, long npixels,
                             int ncols, iteratorCol *column,  void* void_info ) {


int pix;
int event;
int i,j;
int* pixels;
INFO* info;

double dx, dy;
double x, y;

int xcoord, ycoord;

int counts;
int blank;


/****************************
* recast the info structure *
****************************/
info = (INFO*)void_info;

headas_chat(4, "Work function called first_pixel=%ld\n", first_pixel);


/********************************
* get the array of pixel values *
********************************/
pixels = (int*)fits_iter_get_array(column);
blank = pixels[0];

/*
printXform2d(info->trans, stdout);
*/

/************************************************
* loop over all the pixels in the current chunk *
************************************************/
for(pix=1; pix<=npixels; ++pix) {

    i = (pix + first_pixel - 2) % info->input_width;
    j = (pix + first_pixel - 2) / info->input_width;

    /****************************************************
    * null pixels are treated as if they have no counts *
    * pixels[0] always has the null flag value in it    *
    ****************************************************/
    counts=pixels[pix];
    if(blank != 0 && counts == blank) {
        /**************************
        * the input pixel is null *
        **************************/
        counts=0;
        if(!info->zero_nulls) {
            /*************************************************************
            * set everything to null that could be tainted by this pixel *
            *************************************************************/
            transform_null_events(info, i, j);
        }
    } /* end if this is a null pixel */

    /*****************************************
    * loop over the events within this pixel *
    *****************************************/
    for(event=0; event<counts; ++event) {

        /************************************************************
        * note you can't put these "in-line" in the applyXform...
        * function call since the order in which they would be
        * evaluated is machine dependant
        ***********************************************************/
        dx = get_random();
        dy = get_random();

        applyXform2dToDiscreteCoords(info->trans, &x, &y, i, j, dx, dy);

	if(info->nonlinear) {
	    /********************************
	    * apply a non-linear correction *
	    ********************************/
	    double tmp_x, tmp_y;

	    applyMapXform(info->nonlinear, &tmp_x, &tmp_y, x, y);

	    x=tmp_x;
	    y=tmp_y;
	}

        /******************************************
        * make sure we are inside the image array *
        ******************************************/
        xcoord = (int)(x + 0.5);
        ycoord = (int)(y + 0.5);
        if (xcoord >= 0 && xcoord < info->dimenx &&
            ycoord >= 0 && ycoord < info->dimeny   ) {
            /*************************************************
            * transformed pixel is in bounds, so increment
            * it by one photon
            *************************************************/
            if(info->image[ycoord][xcoord] != info->blank) {
                ++(info->image[ycoord][xcoord]);
            }

            headas_chat(5, "%d %d x=%g y=%g counts=%d\n",
	                i,j,x,y, info->image[ycoord][xcoord]);
        } else {
            headas_chat(5, "%d %d x=%g y=%g outside of output image\n",
	                i,j,x,y);
        }
    }


} /* end of loop over pixels */

/************************************
* have to return 0 to show we're OK *
************************************/
return 0;


} /* end of transform_by_events iterator work function */

/********************************************************************************
*
********************************************************************************/
void write_events_image(fitsfile* fpin, fitsfile* fpout, INFO* info) {

int status=0;

int bitpix;
long naxes[2];

naxes[0]=info->dimenx;
naxes[1]=info->dimeny;

headas_chat(1, "Writing transformed image\n");

/*****************************************
* match the data type to the input image *
*****************************************/
fits_get_img_type(fpin, &bitpix, &status);
fits_create_img(fpout, bitpix, 2, naxes, &status);

if(bitpix>0) {
    /****************************************
    * BLANK - only legal for integer images *
    ****************************************/
    fits_write_key_lng(fpout, "BLANK", (long)(info->blank),
                       "Pixel value used for null", &status);
}

/*******************
* write the pixels *
*******************/
fits_write_img_int(fpout, 0l/*not grouped*/, 1l /* writing from first pixel*/,
                   (long)(info->dimenx * info->dimeny),
                   info->image[0], &status);



if(status) {
    fprintf(stderr, "Error writing image\n");
    fits_report_error(stderr,status);
    att_fatal(status);
}

} /* end of write_events_image function */

/********************************************************************************
*
********************************************************************************/
void transform_by_events(fitsfile* fpin, fitsfile* fpout, PARAM* param ) {

int status=0;
long input_width;

INFO* info;
iteratorCol column;


/******************************************************************
* issue a warning if we are reading an image which might have
* non-integer values.
******************************************************************/
{ /* block of local variables */
long bitpix;
int is_real=0;


fits_read_key_lng(fpin, "BITPIX", &bitpix, NULL, &status);
if(bitpix<0) {
    /*******************************************************************
    * negative BITPIX means the image has floating point numbers in it *
    *******************************************************************/
    is_real=1;

} else {
    /**********************************************************************
    * even if the image is stored in integers, the pixel values
    * make have some scaling applied which could make them non-integer
    * we check for this here
    *********************************************************************/
    double bscale;
    double bzero;
    fits_write_errmark();
    fits_read_key_dbl(fpin, "BSCALE", &bscale, NULL, &status);
    if(status == KEY_NO_EXIST) {
        status=0;
	bscale=1.0;
	fits_clear_errmark();
    }

    fits_write_errmark();
    fits_read_key_dbl(fpin, "BZERO" , &bzero, NULL, &status);
    if(status == KEY_NO_EXIST) {
        status=0;
	bzero=0.0;
	fits_clear_errmark();
    }


    if( (double)((int)bscale) != bscale ||
        (double)((int)bzero ) != bzero    ) {
        is_real=1;
    }
}

if(is_real) {
    /************************************************************************
    * issue a warning. The image could actually contain only integer
    * values so this isn't a fatal error. But there are no particularly
    * good reasons to store a counts image like this, so this warants
    * a message to stderr
    ***********************************************************************/
   fprintf(stderr,
            "Warning: The events method is only appropriate for integer images.\n");
}

} /* end of block of local variables */

/***************************************************************
* determine the width of the image in pixels, which we will
* use to convert pixel offsets to coordinates in the iterator
* work function
***************************************************************/
fits_read_key_lng(fpin, "NAXIS1", &input_width, NULL, &status);
if(status) {
    fprintf(stderr, "Error while reading NAXIS1 from %s\n", param->infile);
    fits_report_error(stderr,status);
    att_fatal(status);
}




/************************
* create info structure *
************************/
info = createInfo((int)input_width, param->dimenx, param->dimeny,
                  param->trans, param->nonlinear, param->zero_nulls);
if(info==NULL) {
    fprintf(stderr, "Could not create information structure\n");
    att_fatal(1);
}

/******************************************************
* initialize the "column" data structure.
* The CFITSIO iterator treats image data as a column
******************************************************/
fits_iter_set_by_num(&column, fpin, 0, TINT,  InputCol);


/****************************************
* call the iterator to do the real work *
****************************************/
fits_iterate_data(1,&column,
                  0l/*don't skip any rows*/,
                  0l/*read optimum columns per iteration*/,
                  transform_by_events_work, info, &status);
if(status) {
    fprintf(stderr, "Error from iterator\n");
    fits_report_error(stderr,status);
    att_fatal(status);
}


write_events_image(fpin, fpout, info);

} /* end of transform_by_events function */




