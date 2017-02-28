#include <stdlib.h>
#include <string.h>
#include <math.h>

#define TOOLSUB transform_wcs
/* fatal.h / headas_main() require that TOOLSUB be defined first */


#include "headas.h"
#include "coordfits.h"
#include "param.h"
#include "att_fatal.h"

#include "headas_main.c"

/****************************************************************************
*****************************************************************************
* transform to a FITS image from one coordinate system to another.
****************************************************************************/
void TOOLSUBAUX(void) {

PARAM* param;

int status=0;
fitsfile* fp;


set_toolversion("0.0.0");

/**********************
* read the parameters *
**********************/
param=readParam();

if(param==NULL) {
    fprintf(stderr, "Could not read parameters\n");
    att_fatal(1);
}

/***********************
* open the output file *
***********************/
fits_create_file(&fp, param->outfile, &status);

if(status==FILE_NOT_CREATED) {
    /*********************************************
    * could not create the file, so try opening
    * it as an existing file
    *********************************************/
    status=0;
    fits_open_file(&fp, param->outfile, READWRITE, &status);
} else {
    /***********************
    * make an empty header *
    ***********************/
    fits_create_img(fp, 8, 0, NULL, &status);
}

if(status) {
    fprintf(stderr, "Could not open %s\n", param->outfile);
    fits_report_error(stderr, status);
    att_fatal(status);
}

/**********************
* apply the transform *
**********************/
if(param->combo->map) {
    fprintf(stderr, "Warning: ignoring non-linear part of transform\n");
}

transformWCSPixels(param->wcs, param->combo->trans);

/*************************
* write the WCS keywords *
*************************/
status=writeWCS(param->wcs, fp, param->out_suffix);

/***************************
* write the parameter list *
***************************/ 
HDpar_stamp(fp, 0, &status);

/***********************
* update the checksums *
***********************/ 
fits_write_chksum(fp, &status);

/**********************
* close the FITS file *
**********************/
fits_close_file(fp, &status);

if(status) {
    fprintf(stderr, "Could not write WCS to %s\n", param->outfile);
    fits_report_error(stderr, status);
    att_fatal(status);
}

} /* end of main function */

