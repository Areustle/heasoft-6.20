#include "image.h"
#include "att_fatal.h"
#include "overlap.h"
#include "red_black_tree.h"
#include "headas.h"

#include "longnam.h"

/*************************************************************************
* produce an IEEE NaN value
*************************************************************************/
double getNaN() {

    static int initialized;
    static double value=0;
    
if (!initialized) {
    int i;
    initialized = 1;
    for(i=0; i< 8; ++i) {
        *(((unsigned char*)(&value))+i) = 0xff;
    }
    
    if(! isnan(value) ) {
        fprintf(stderr, "Warning: This machine does not use IEEE floating point numbers.\n");
        fprintf(stderr, "Null pixel values will not be handled correctly\n");
    }
}

    return value;
} /* end of getNaN function */



/***********************************************************************************
* Allocate storage for an image. This function is mostly for internal use
* instead call allocateSimilarImage or readFITSImage
***********************************************************************************/
IMAGE* allocateImage(long dimenx, long dimeny, int bitpix) {

    IMAGE* image;
    int i;

    /************************
    * allocate some storage *
    ************************/
    image = (IMAGE*)malloc(sizeof(IMAGE));

    /***********************
    * remember some values *
    ***********************/
    image->dimenx = dimenx;
    image->dimeny = dimeny;

    image->bitpix = bitpix;

    /*******************************
    * set these things to defaults *
    *******************************/
    image->zero=0.0;
    image->scale=1.0;

    image->out_of_bounds = 0.0;
    
    image->has_nulls=0;
    
    image->has_preferred_blank = 0;


    /***************************
    * allocate the image array *
    ***************************/
    image->data    = (IMAGEDATATYPE**)malloc(sizeof(IMAGEDATATYPE*)*dimeny);
    *(image->data) = (IMAGEDATATYPE* )calloc(dimenx*dimeny, sizeof(IMAGEDATATYPE)); /*init to 0*/
    for(i=1; i<dimeny; ++i) {
        image->data[i] = image->data[i-1] + dimenx;
    }

    /*************************************
    * allocate the null pixel flag array *
    *************************************/
    image->is_null =(char**)malloc(sizeof(char*)*dimeny);
    *(image->is_null) = (char* )calloc(dimenx*dimeny, sizeof(char)); /*init to 0*/
    for(i=1; i<dimeny; ++i) {
        image->is_null[i] = image->is_null[i-1] + dimenx;
    }

    image->weight = 0;


    return image;

} /* end of allocateImage function */

/*****************************************************************************
* create a new image with all pixels initialized to zero.
* It will have the same scaling as the given image.
* if bitpix is non-zero, then the new image will have that bitpix value,
* otherwise it defaults to the same value as the given image.
*****************************************************************************/
IMAGE* allocateSimilarImage(long dimenx, long dimeny, IMAGE* image, int bitpix) {

    IMAGE* copy;

    copy = allocateImage(dimenx, dimeny, image->bitpix);

    copy->zero  = image->zero;
    copy->scale = image->scale;

    if(bitpix!= 0) copy->bitpix = bitpix;
    
    if(image->has_preferred_blank) {
        suggestBlankForImage(copy, image->preferred_blank);
    }

    return copy;

} /* end of allocateSimilarImage function */


/*****************************************************************************
* frees data storage for an image structure
*****************************************************************************/
void destroyImage(IMAGE* image) {

    free(*(image->data));
    free(image->data);

    free(*(image->is_null));
    free(image->is_null);

    free(image);


} /* end of destroyImage function */

/********************************************************************************
* returns true if the given coordinates are within the image
********************************************************************************/
int isInImage(IMAGE* image, int i, int j) {

    return i>=0 && i< image->dimenx &&
           j>=0 && j< image->dimeny;
}

/********************************************************************************
* returns true if the given pixel of the image has a null value.
* if the coordinates are out of bounds then it returns true only if the default
* out of bounds value is set to NaN
********************************************************************************/
int imageIsNull(IMAGE* image, int i, int j) {

    if(isInImage(image,i,j)) return (int)(image->is_null[j][i]);
    else               return isnan(image->out_of_bounds);
}

/********************************************************************************
* returns the pixel value at the given coordinates, or the default out of
* bounds value if the coordinates are not in the image. Returns NaN if the
* particular pixel is null.
********************************************************************************/
double getImagePixel(IMAGE* image, int i, int j) {

    if(isInImage(image,i,j))  {
        /*****************************************
        * in bounds - check if the pixel is null *
        *****************************************/
        if(imageIsNull(image,i,j)) return NaN;
        else                      return image->data[j][i];
    } else  {
        /****************
        * out of bounds *
        ****************/
        return image->out_of_bounds;
    }

}



/********************************************************************************
* Mark a particular pixel in the image as being null. Does nothing if the given
* coordinates are not in the image.
********************************************************************************/
void setImageNull(IMAGE* image, int i, int j) {

    if(isInImage(image,i,j)) {

        image->is_null[j][i]=(char)1;
        image->has_nulls=1;
    }
}

/********************************************************************************
* Set the value of a given pixel. If the value is NaN, then the pixel will
* be marked as null. Does nothing if the coordinates are not in the image.
********************************************************************************/
void setImagePixel(IMAGE* image, int i, int j, double value) {

    if(isInImage(image, i,j)) {

        if(isnan(value) ) setImageNull(image, i,j);
        else              image->data[j][i] = value;
    }
}


/********************************************************************************
* adds the given value to the current value in the specified pixel.
* Does nothing if the coordinates are out of bounds or if the pixel is
* currently null. If the specified value is null, sets the current
* pixel to null.
********************************************************************************/
void incrementImagePixel(IMAGE* image, int i, int j, double value) {

    if(isInImage(image, i,j) && !imageIsNull(image,i,j) ) {

        if(isnan(value) ) setImageNull(image, i,j);
        else              image->data[j][i] += value;
    }
}


/******************************************************************************
* applies (bit ORs) the input flags to the current value in the specified pixel.
* Does nothing if the coordinates are out of bounds or if the pixel is
* currently null.
******************************************************************************/
void flagImagePixel(IMAGE* image, int i, int j, long flags) {

    if(isInImage(image, i,j) && !imageIsNull(image,i,j) ) {

		long oldFlags = (long) image->data[j][i];
		flags |= oldFlags;
        image->data[j][i] = flags;
    }
}


/*********************************************************************************
* Returns the CFITSIO data code for the variable used by this structure.
*********************************************************************************/
int getFITSImageDataType(IMAGE* image) {

    int size = sizeof(IMAGEDATATYPE);

    if(     size == 8 ) return TDOUBLE;
    else if(size == 4 ) return TFLOAT;
    else                 return -1;
}

/**********************************************************************************
*
**********************************************************************************/
IMAGE* readFITSImage(fitsfile* fp, int zero_nulls) {

    int status=0;
    int bitpix;
    long dimen[2];
    long start[] = {1l, 1l};
    long npixels;

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
    
    npixels = dimen[0]*dimen[1];

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

        /* restore previous state */
        status = tmp;
        fits_clear_errmark();
    } /* end if this is an integer image */

    /***************************************
    * try reading a preferred BLANK value *
    **************************************/
    if(bitpix>0) {
        long blank;
        fits_read_key_lng(fp, "BLANK", &blank, NULL, &status);
        if(status==KEY_NO_EXIST) {
            /**********************************
            * no blank, but we won't sweat it *
            **********************************/
            status=0;
        } else {
            /****************************
            * There was a BLANK keyword *
            ****************************/
            suggestBlankForImage(image, (int)blank);
        }
    } /* end of this is an integer image */


    /****************
    * read the data *
    ****************/
    fits_read_pixnull(fp, getFITSImageDataType(image), start, npixels,
                      *(image->data), *(image->is_null),
                      &(image->has_nulls), &status);

    /*************************************************************
    * if requested, set all the null pixels to zero. We have to
    * do this by hand, since CFITSIO won't do it for us
    **************************************************************/
    if(zero_nulls && image->has_nulls) {
        long pixel;
        
        for(pixel=0l; pixel < npixels; ++pixel) {
            if( (*(image->is_null))[pixel] ) {
                /***********************************************
                * set the pixel to zero and mark it as un-null *
                ***********************************************/
                (*(image->is_null))[pixel] = (char)0;
                (*(image->data   ))[pixel] = 0.0;
            }
        }
        
        image->has_nulls = 0;
    }



    /*******************
    * check for errors *
    *******************/
    if(status) {
        fits_report_error(stderr, status);
        return NULL;
    }

    return image;

} /* end of readFITSImage method */

/****************************************************************************
* This is an internal utility function for reading the raw pixel values
* from a FITS image. It is used when searching for a BLANK value
****************************************************************************/
int** read_unscaled_fits_pixels(fitsfile* fp) {

    double bzero, bscale;

    int** values;
    int null=0;
    long start[] = {1,1};
    long dimen[2];

    int status=0;
    int anynull=0;

    int i;

    long npixels;

    /***************************
    * get the image dimensions *
    ***************************/
    fits_get_img_size(fp, 2, dimen, &status);
    npixels = dimen[0] * dimen[1];

    /****************************************
    * allocate storage for the pixel values *
    ****************************************/
    values = (int**)malloc(sizeof(int*)*dimen[1]);
    *values = (int*)malloc(sizeof(int)*npixels);
    for(i=1; i<dimen[1]; ++i) {
        values[i] = values[i-1]+dimen[0];
    }

    /************************************************************************
    * temporarily reset bscale and bzero so that we can read the raw values *
    ************************************************************************/
    fits_set_bscale(fp, 1.0, 0.0, &status);

    /****************
    * read the data *
    ****************/
    fits_read_pix(fp, TINT, start, npixels, &null, *values, &anynull, &status);

    /****************************************************************
    * reset the CFITSIO internal scaling back to the keyword values *
    ****************************************************************/
    fits_read_key_dbl(fp, "BZERO", &bzero, NULL, &status);
    if(status==KEY_NO_EXIST) {
        bzero = 0.0;
        status=0;
    }

    fits_read_key_dbl(fp, "BSCALE", &bscale, NULL, &status);
    if(status==KEY_NO_EXIST) {
        bscale = 1.0;
        status=0;
    }

    fits_set_bscale(fp, bscale, bzero, &status);

    return values;

} /* end of findBlankForImage function */

/****************************************************************************
*
*****************************************************************************/
void writeFITSImage(fitsfile* fp, IMAGE* image) {

    int status=0;
    long dimen[2];
    long start[] = {1l,1l};
    long npixels;
    int i,j;

    dimen[0] = image->dimenx;
    dimen[1] = image->dimeny;

    npixels = dimen[0] * dimen[1];

    fits_create_img(fp, image->bitpix, 2, dimen, &status);


    /**********************************************************************
    * write the scaling for integer images
    ********************************************************************/
    if(image->bitpix>0 ) {

        if(image->zero != 0.0 ) {
            fits_update_key(fp, TDOUBLE, "BZERO", &(image->zero),
                        "pixel scaling zero point", &status);
        }

        if(image->scale != 1.0 ) {
            fits_update_key(fp, TDOUBLE, "BSCALE", &(image->scale),
                        "pixel scaling", &status);
        }

    } /* end if this is an integer image */

    /*****************
    * write the data *
    *****************/
    fits_write_pix(fp, getFITSImageDataType(image), start, npixels,
                *(image->data), &status);

    /************************************************
    * now we deal with null pixels if there are any *
    ************************************************/
    if(image->has_nulls) {

        long pixel;
        int no_blank=0;
        int blank=0;

        if(image->bitpix>0) {
            /**********************************************************
            * have to come up with a good value for the BLANK keyword *
            **********************************************************/
            NODE* tree=NULL;

            /******************************************************
            * get the unscaled integer values which were written
            * into the FITS file above 
            ******************************************************/
            int** values = read_unscaled_fits_pixels(fp);
            
            /*******************************************************
            * load the non-null pixel values into a tree structure *
            *******************************************************/
            for(j=0; j< image->dimeny; ++j) {
                for(i=0; i<image->dimenx; ++i) {
                    if(!imageIsNull(image,i,j)) {

                        addValueToNode(&tree, values[j][i]);
                    }
                }
            }
            
            /*********************************************
            * we're done with the raw integer values now *
            *********************************************/
            free(*values);
            free(values);
            
            /*******************************************
            * now pick the best available unused value *
            *******************************************/
            no_blank=1;
            
            /*******************************************
            * try using the preferred BLANK value *
            **************************************/
            if(image->has_preferred_blank &&
               !nodeHasValue(tree, image->preferred_blank) ) {
                /********************************
                * the preferred value is unused *
                ********************************/
                blank = image->preferred_blank;
                no_blank=0;
            }


            if(no_blank && image->bitpix == 8) {
                /*************************************************
                * unsigned bytes - pick the largest unused value *
                *************************************************/
                for(i=255; i>=0; --i) {

                    if(! nodeHasValue(tree, i) ) {
                        /************************
                        * found an unused value *
                        ************************/
                        blank = i;
                        no_blank=0;
                        break;
                    }
                }
            } else if(no_blank) {
                /***********************
                * signed integer image *
                ***********************/
                int max =  (1l << (image->bitpix-1))-1;
                int min = -(1l << (image->bitpix-1));

                /***********************
                * a quick sanity check *
                ***********************/
                if(sizeof(int)*8 < image->bitpix) {
                    fprintf(stderr, 
                            "WARNING: This code assumes int has at least %d bytes",
                            image->bitpix/8);
                    fprintf(stderr,
                            "but on this machine, ints are %d bytes\n",
                            sizeof(int) );
                }
                
                /***********************
                * try different values *
                ***********************/
                if(! nodeHasValue(tree, -1) ) {
                    /***************
                    * first try -1 *
                    ***************/
                    blank = -1;
                    no_blank = 0;

                } else if(! nodeHasValue(tree, min) ) {
                    /***********************************
                    * then try the most negative value *
                    ***********************************/
                    blank = min;
                    no_blank = 0;

                } else if(! nodeHasValue(tree, max) ) {
                    /*************************************
                    * then try the larget possible value *
                    *************************************/
                    blank = max;
                    no_blank = 0;

                } else {
                    /**************************************
                    * now were desperate, so try anything *
                    **************************************/
                    for(i=min+1; i< max; ++i) {
                        if(!nodeHasValue(tree, i) ) {
                            blank = i;
                            no_blank = 0;
                            break;
                        }
                    }
                }
            } /* end if image has signed integers */
            
            /*******************************************************
            * by the time we get here we have found a BLANK value
            * or else we haven't
            *******************************************************/
            if(!no_blank) {
                /******************************************
                * found one, so write it to the FITS file *
                ******************************************/
                headas_chat(1, "Setting BLANK=%d\n", blank);

                fits_write_key_lng(fp, "BLANK", (long)blank,
                                   "Pixel value indicating NULL", &status);

                /******************************************************
                * The code didn't wqork until I added this.
                * apparently setting the BLANK keyword is not enough
                * to tell CFITSIO what the BLANK value is. 
                * And then for some reason the preprocessor didn't
                * have the long name
                * fits_set_imgnul(fp, (long)blank, &status);
                * but using the short name version appears to work
                * -ED 2004-04-20
                *****************************************************/
                ffpnul(fp, (long)blank, &status);
            }

        } /* end if the image has integer pixels */

        if(no_blank) {
            /*********************************************************
            * we were unable to come up with a value for the BLANK
            * keyword, so we will set all the nulls to zero, and
            * report the null pixels to stderr
            *********************************************************/
            int zero=0;

            fprintf(stderr, "Couldn't find an unused pixel value for BLANK\n");
            fprintf(stderr, "The following null pixels will be set to zero:\n");

            for(pixel=0; pixel < npixels; ++pixel) {

                if( (*(image->is_null))[pixel]) {
                    /****************************************
                    * the pixel is null, so mark it as such *
                    ****************************************/
                    fprintf(stderr, "%ld %ld\n", pixel%(image->dimenx)+1l,
                                                pixel/(image->dimenx)+1l );

                    fits_write_img(fp, TINT, pixel, 1l, &zero, &status);

                }

            }

        } else {
            /***************************
            * mark all the null pixels *
            ***************************/
            for(pixel=0; pixel < npixels; ++pixel) {

                if( (*(image->is_null))[pixel]) {
                    /****************************************
                    * the pixel is null, so mark it as such *
                    ****************************************/
                    fits_write_null_img(fp, pixel+1l, 1l, &status);
                }

            }
            
        } /* end if we have a BLANK value */


    } /* end if image has null pixel in it */

    /************************
    * check for FITS errors *
    ************************/
    if(status) {
        fprintf(stderr, "Error writing image status=%d\n", status);
        fits_report_error(stderr, status);
        att_fatal(1);
    }




} /* end of readFITSImage method */




/*********************************************************************************
* return an interpolated pixel value. Outside the image we extrapolate,
* or really interpolate using the out_of_bounds values.
*********************************************************************************/
double interpolateImagePixel(IMAGE* image, double x, double y) {

    int i,j;
    double xhat, yhat;
    double a00, a01, a10, a11;
    double value;

    /***********************************************
    * get the integer coordinates
    * and the fractional position within the pixel
    ***********************************************/
    i=floor(x);
    j=floor(y);

    xhat = x-(double)i;
    yhat = y-(double)j;

    /**********************************************
    * get the pixel values to interpolate between *
    **********************************************/
    a00 = getImagePixel(image, i  , j  );
    a10 = getImagePixel(image, i+1, j  );
    a01 = getImagePixel(image, i  , j+1);
    a11 = getImagePixel(image, i+1, j+1);
    
    /**********************************************
    * special cases - we need to separate these out
    * in order to properly handle NaNs
    ************************************************/
    if(xhat==0.0 && yhat==0.0) return a00;
    else if(xhat==0.0) {
        /***************
        * on left edge *
        ***************/
        if(isnan(a01) || isnan(a00)) return NaN;
        else                         return yhat*(a01 - a00 ) + a00;

    } else if(yhat==0.0) {
        /******************
        * on bottom edge *
        *****************/
        if(isnan(a10) || isnan(a00)) return NaN;
        else                         return xhat*(a10 - a00 ) + a00;

    } else {
        /**************************
        * in the middle somewhere *
        **************************/
        if(isnan(a00) || isnan(a01) || isnan(a10) || isnan(a11) ) return NaN;
        else {
            /**************
            * interpolate *
            **************/
            value = xhat*yhat*(a11 - a01 - a10 + a00)
                     +   xhat*(a10 - a00 )
                     +   yhat*(a01 - a00 )
                     +         a00;

            return value;
        }
    }


} /* end of interpolateImagePixel function */

/***************************************************************************
* callback function used for iterating over a quadrilateral
***************************************************************************/
void increment_image_callback (int i, int j, OverlapState * state) {

    IMAGE* image = (IMAGE*)state->user;
    double delta;

    /***************************
    * determine the contribution
    ***************************/

    delta = image->utility * state->weight;
    if (delta < 0) {
static int limit = 10;
		if (limit > 0) {
			printf("warning: delta at %d, %d => %e\n", i, j, delta);
			--limit;
		}
	}

    incrementImagePixel(image, i, j, delta);
    if (image->weight)
        incrementImagePixel(image->weight, i, j, state->weight);

} /* end of increment_callback */


/***************************************************************************
* callback function used for iterating over a quadrilateral with flags
***************************************************************************/
void apply_flags_callback (int i, int j, OverlapState * state) {

    IMAGE* image = (IMAGE*)state->user;

	if (state->weight > 0) {
		flagImagePixel(image, i, j, image->flags);
	}
	else {
static int limit = 10;
		if (limit > 0) {
			printf("warning: weight at %d, %d => %e\n", i, j, state->weight);
			--limit;
		}
	}

} /* end of increment_callback */

/***************************************************************************
* callback function used for iterating over a quadrilateral
***************************************************************************/
void set_image_callback (int i, int j, OverlapState * state) {

    IMAGE* image = (IMAGE*)state->user;

    setImagePixel(image, i,j, image->utility);

} /* end of increment_callback */

/***************************************************************************
* callback function used for iterating over a quadrilateral
***************************************************************************/
void null_image_callback (int i, int j, OverlapState * state) {

    IMAGE* image = (IMAGE*) state->user;

    setImageNull(image, i,j);

} /* end of increment_callback */

/***************************************************************************
* increment all the pixels under a quadrilateral by a given value
***************************************************************************/
void incrementQuadOnImage(IMAGE* image, const Quad * quad, double value) {

    image->utility = value;

    iterate_overlap(quad, increment_image_callback, image);

}

/***************************************************************************
* apply flags to all the pixels under a quadrilateral
***************************************************************************/
void applyFlagsToImage (IMAGE* image, const Quad * quad, long flags) {

    image->flags = flags;

    iterate_overlap(quad, apply_flags_callback, image);

}

/***************************************************************************
* Set all the pixels under a quadrilateral to a given value.
***************************************************************************/
void setQuadOnImage(IMAGE* image, const Quad* quad, double value) {

    image->utility = value;

    iterate_overlap(quad, set_image_callback, image);

}

/***************************************************************************
* Set all the pixels touched by the given quadrilateral to null.
***************************************************************************/
void setQuadOnImageNull(IMAGE* image, const Quad* quad) {

    iterate_overlap(quad, null_image_callback, image);

}


/***************************************************************************
* suggest a value for the BLANK keyword to use when writing the image.
* Th eimage is not obligated to use this value, and will ignore it if it
* is not appropriate for the data storage, or if the value is used by
* valid pixels.
***************************************************************************/
void suggestBlankForImage(IMAGE* image, int blank) {

    int min;
    int max;

    /***************************************************
    * a new suggestion invalidates old ones, even if
    * this suggestion is not valid
    **************************************************/
    image->has_preferred_blank=0;
    
    /****************************************************
    * floating point images do not have a BLANK keyword *
    ****************************************************/
    if(image->bitpix<0) return;
    
    /*************************************************
    * make sure the bitpix is in range for the image *
    *************************************************/
    if(image->bitpix==8) {
        /****************************
        * 8 bit images are unsigned *
        ****************************/
        min=0; 
        max = 255;
    } else {
        /*************************************
        * other kinds of integers are signed *
        *************************************/
        max =  (1l << (image->bitpix-1))-1;
        min = -(1l << (image->bitpix-1));
    }
    
    if(blank < min || blank > max ) return;
    
    /*****************************************************************
    * if we get here we will tentatively accept this blank value
    * we will still check whether this value is used by a non-null
    * pixel when we write the image
    *****************************************************************/
    image->has_preferred_blank = 1;
    image->preferred_blank = blank;

} /* end of suggestBlankForImage function */

