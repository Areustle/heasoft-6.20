#include <stdlib.h>
#include <string.h>

#include "headas.h"
#include "param.h"
#include "param_wrappers.h"
#include "att_fatal.h"
#include "random.h"



/**************************************************************************
**************************************************************************
* read the input parameters
**************************************************************************/
PARAM* readParam(void) {

XFORM2D* temp;
char transform_file[FILENAME_LENGTH];

PARAM* param;
param=(PARAM*)malloc(sizeof(PARAM));

/*************************
* input and output files *
*************************/
read_string_param( "infile",  param->infile, FILENAME_LENGTH);
read_string_param("outfile", param->outfile, FILENAME_LENGTH);

param->seed = read_int_param("seed");
seed_random(param->seed);

param->history = read_boolean_param("history");


/******************************************
* should null pixels be treated as zeros? *
******************************************/
param->zero_nulls = read_boolean_param("zeronulls");

/**************************************
* force a BITPIX for the output image *
**************************************/
param->bitpix = read_int_param("bitpix");

/*******************
* transform method *
*******************/
param->method = determine_method();

/**************************
* output image dimensions *
**************************/
param->dimenx = read_int_param("dimenx");
param->dimeny = read_int_param("dimeny");

/************************************************
* determine the transform to apply to the image *
************************************************/
if (param->method == INTERPOLATE_METHOD) {
    /***************************************************
    * the interpolate method needs the inverse transform *
    ***************************************************/
    read_string_param("inverse", transform_file, FILENAME_LENGTH);
    if (!strcasecmp(transform_file, "NONE"))
        param->method = INTERALPHA_METHOD;
}

if (param->method != INTERPOLATE_METHOD) {
    /*****************************************************
    * the rest of the methods need the forward transform *
    *****************************************************/
    read_string_param("transform", transform_file, FILENAME_LENGTH);
}

param->combo = readComboXform(transform_file);
if(param->combo == NULL) {

    fprintf(stderr, "Could not read transform from %s\n", transform_file);
    att_fatal(1);
}

param->interAlpha = read_double_param("alpha");
param->interEpsilon = 1e-6;

/*****************************************************************************
* from the user's point of view, the transforms follow the FITS convention
* of having the center of the first pixels be (1,1). However internally
* we put the center of the first pixel at (0,0). This is C after all.
* so we need to modify the transform to follow the internal convention
*****************************************************************************/
temp = allocateXform2d();

setXform2dToTranslation(temp, 1.0, 1.0); /* from C to FITS */
applyXform2dBeforeComboXform(temp, param->combo);

setXform2dToTranslation(temp, -1.0, -1.0); /* from FITS back to C */
applyXform2dAfterComboXform(param->combo, temp);


return param;

} /* end of readParam function */

/******************************************************************************
* read and interpret the transform method from the parameter file
******************************************************************************/
int determine_method() {

char method[FILENAME_LENGTH];


read_string_param("method", method, FILENAME_LENGTH);

if(!strncasecmp(method, "events",      FILENAME_LENGTH) ) return      EVENTS_METHOD;
if(!strncasecmp(method, "interpolate", FILENAME_LENGTH) ) return INTERPOLATE_METHOD;
if(!strncasecmp(method, "bbox",        FILENAME_LENGTH) ) return        BBOX_METHOD;
if(!strncasecmp(method, "center",      FILENAME_LENGTH) ) return      CENTER_METHOD;
if(!strncasecmp(method, "area",        FILENAME_LENGTH) ) return        AREA_METHOD;
if(!strncasecmp(method, "flat",        FILENAME_LENGTH) ) return        FLAT_METHOD;
if(!strncasecmp(method, "flag",        FILENAME_LENGTH) ) return        FLAG_METHOD;

fprintf(stderr, "Unknown transform method %s\n", method);
return UNKNOWN_METHOD;

} /* end of determine_method function */

