#include <stdlib.h>
#include <string.h>
#include <math.h>

#define TOOLSUB imagexform
/* fatal.h / headas_main() require that TOOLSUB be defined first */


#include "headas.h"
#include "coordfits.h"
#include "param.h"
#include "att_fatal.h"
#include "info.h"
#include "methods.h"
#include "random.h"

#include "headas_main.c"

#define COPY_HDUS


/*****************************************************************************
* Copies all the non-required keywords in one file to another.
* returns the CFITSIO status value
*****************************************************************************/
int copy_keywords(fitsfile* fpin, fitsfile* fpout) {

int status=0;

char card[FLEN_CARD];
int ncards;
int morekeys;
int i;
int type;
int hdu;
int good_hdu_id;

/**************************************************
* find the total number of keywords in the infile *
**************************************************/
fits_get_hdrspace(fpin, &ncards, &morekeys, &status);

/*********************************************************
* determine what HDU we are in. There are some keywords
* That we don't want to copy into the primary HDU
*********************************************************/

if (!status)
    fits_get_hdu_num(fpout, &hdu);

/*****************************
* loop over all the keywords *
*****************************/
for(i=1; i<=ncards; ++i) {

    fits_read_record(fpin, i, card, &status);

    /***************************************
    * determine the type of keyword *
    ********************************/
    type = fits_get_keyclass(card);
    
    /*****************************
    * special handling for these *
    *****************************/
    good_hdu_id = 0;
    if(type == TYP_HDUID_KEY) {
        good_hdu_id = 1;
        
        if(hdu==1 && !strncmp(card, "EXT", 3) ) {
            /*************************************************
            * we can't copy any of the EXT... keywords into 
            * the primary HDU 
            *************************************************/
            good_hdu_id = 0;
        }
    }
    
    /**********************************************
    * write the keyword if it has the right class *
    **********************************************/
    if(type == TYP_HDUID_KEY ||
					  type == TYP_UNIT_KEY ||
                      type == TYP_COMM_KEY ||
                      type == TYP_CONT_KEY ||
                      type == TYP_USER_KEY ||
                      type == TYP_REFSYS_KEY ) {
        /***********************************
        * This is a keyword we should copy *
        ***********************************/
        fits_write_record(fpout,  card, &status);
    }
} /* end of loop over keywords */
        
return status;

} /* end of copy_keywords function */




/****************************************************************************
*****************************************************************************
* transform to a FITS image from one coordinate system to another.
****************************************************************************/
void TOOLSUBAUX(void) {

PARAM* param;

int status=0;
fitsfile* fpin, *fpout;

set_toolversion("1.1.0");

/**********************
* read the parameters *
**********************/
param=readParam();

if(param==NULL) {
    fprintf(stderr, "Could not read parameters\n");
    att_fatal(1);
}

if(param->trans == NULL) {
    fprintf(stderr, "Could not determine transform\n");
    att_fatal(1);
}


/****************************
* open the input image file *
****************************/
fits_open_file(&fpin, param->infile, READONLY, &status);

if(status) {
    fprintf(stderr, "Error while opening event file %s\n", param->infile);
    fits_report_error(stderr,status);
    att_fatal(status);
}

/***********************************************************
* if the method is "bbox", just calculate the bounding box
* of the output image and quit
***********************************************************/
if(param->method == BBOX_METHOD) {

    double xmin, ymin, xmax, ymax;

    transform_bbox(&xmin, &ymin, &xmax, &ymax, fpin, param);
    report_bbox(xmin, ymin, xmax, ymax);

    return;
}

/********************************************
* check if we are clobbering the input file *
********************************************/
if(param->outfile[0] == '!' ) {
    char in_root[FLEN_FILENAME];
    char out_root[FLEN_FILENAME];

    fits_parse_rootname(param->outfile, out_root, &status);
    fits_parse_rootname(param->infile ,  in_root, &status);

    if(status) {
        fits_report_error(stderr, status);
        att_fatal(1);
    }
    
    if(!strncmp(in_root, out_root+1,FLEN_FILENAME) ) {
        fprintf(stderr, "Clobbering the input file is not allowed\n");
        att_fatal(1);
    }
} /* end if the outfile is to be clobbered */

/***************************
* now open the output file *
***************************/
fits_create_file(&fpout, param->outfile, &status);

if(status==FILE_NOT_CREATED) {
    /*********************************************
    * could not create the file, so try opening
    * it as an existing file
    *********************************************/
    headas_chat(1, "Appending Extension to an existing output file\n");
    status=0;
    fits_open_file(&fpout, param->outfile, READWRITE, &status);
}

/***********************************************************
* copy preceeding HDUs to the output file if there are any *
***********************************************************/
if(param->copy_hdus) {
    int hdu;

    fits_get_hdu_num(fpin, &hdu);
    if(hdu>1) {

        headas_chat(1, "Copying preceeding HDUs\n");
        fits_copy_file(fpin, fpout, 1/*previous*/, 0/*current*/,
                    0/*following*/, &status);
    }
}/* end if copying HDUs */

/*********************************************
* do the transform by the appropriate method *
*********************************************/
headas_chat(1, "Transforming\n");

if(param->method == EVENTS_METHOD) {
    transform_by_events(fpin, fpout, param);

} else if(param->method == INTENSITY_METHOD) {
    transform_by_intensity(fpin, fpout, param);

} else if(param->method == CENTER_METHOD) {
    transform_by_center(fpin, fpout, param);

} else if(param->method == AREA_METHOD) {
    transform_by_area(fpin, fpout, param);

} else {
    fprintf(stderr, "Unknown tranform method %d\n", param->method);
    att_fatal(1);
}


/***************************************************************
* copy all the keywords from the input file to the output file *
***************************************************************/
status=copy_keywords(fpin,fpout);
if(status) {
    fprintf(stderr, "Error copying keywords\n");
    fits_report_error(stderr,status);
    att_fatal(status);
}


/***************************************
* now write the new WCS keyword values *
***************************************/
status=writeWCS(param->wcs, fpout);
if(status) {
    fprintf(stderr, "Error updating WCS keywords\n");
    fits_report_error(stderr,status);
    att_fatal(status);
}

/************************************
* write HISTORY keywords to outfile *
************************************/
if (param->history) {
    HDpar_stamp(fpout, 0, &status);
}

/**********************
* update the checksum *
**********************/
fits_write_date(fpout, &status);

fits_update_key_str(fpout, "CREATOR", "imagexform",
                   "Program which created this HDU", &status);

fits_write_chksum(fpout, &status);


/*********************************************************
* copy all the extensions from the infile to the outfile *
*********************************************************/
if(param->copy_hdus) {
    headas_chat(1, "Copying other HDUs\n");
    fits_copy_file(fpin, fpout, 0/*previous*/, 0/*current*/,
                   1/*following*/, &status);
}

/******************
* close the files *
******************/
fits_close_file(fpin ,&status);
fits_close_file(fpout,&status);
if(status) {
    fprintf(stderr, "Error closing files\n");
    fits_report_error(stderr,status);
    att_fatal(status);
}


} /* end of imagexform function */

