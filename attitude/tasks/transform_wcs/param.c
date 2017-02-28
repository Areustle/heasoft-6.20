#include <stdlib.h>
#include <string.h>

#include "headas.h"
#include "param.h"

#include "param_wrappers.h"

#include "att_fatal.h"

#include "fitsio.h"


#define UNKNOWN -3
#define SKY -2
#define RAW -1

/**************************************************************************
* read the input parameters
**************************************************************************/
PARAM* readParam(void) {

char filename[FILENAME_LENGTH];
fitsfile* fp;
int status=0;

PARAM* param;

/*********************************
* create the parameter structure *
*********************************/
param=(PARAM*)malloc(sizeof(PARAM));

read_string_param("infile", filename, FILENAME_LENGTH);
fits_open_file(&fp, filename, READONLY, &status);
if(status) {
    fprintf(stderr, "Could not open %s\n", filename);
    fits_report_error(stderr, status);
    att_fatal(1);
}

/*******************
* keyword suffixes *
*******************/
read_string_param( "insuffix", param-> in_suffix, 2);
read_string_param("outsuffix", param->out_suffix, 2);

/************************
* read the WCS keywords *
************************/
param->wcs = readWCS(fp, param->in_suffix);
if(param->wcs == NULL) {

    fprintf(stderr, "Could not read %s\n", filename);
    att_fatal(1);
}

fits_close_file(fp, &status);
if(status) {
    fprintf(stderr, "Error closing %s\n", filename);
    fits_report_error(stderr, status);
    att_fatal(1);
}

/*********************
* read the transform *
*********************/
read_string_param("transform", filename, FILENAME_LENGTH);
param->combo = readComboXform(filename);
if(param->combo == NULL) {

    fprintf(stderr, "Could not read %s\n", filename);
    att_fatal(1);
}


/**********
* outfile *
**********/
read_string_param("outfile", param->outfile, FILENAME_LENGTH);

return param;

} /* end of readParam */

