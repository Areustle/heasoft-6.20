#include <stdlib.h>
#include <string.h>

#include "headas.h"
#include "param.h"

#include "xform2d.h"

#include "param_wrappers.h"

#include "att_fatal.h"


#define UNKNOWN -3
#define SKY -2
#define RAW -1

/**************************************************************************
**************************************************************************
* determine the coordinate integer code corresponding to a coordinate name 
**************************************************************************/
int coord_type(TELDEF* teldef, char* name) {

    int i;

    if(!strncmp(name, "RAW", COORD_NAME_LENGTH) ) return RAW;
    if(!strncmp(name, "SKY", COORD_NAME_LENGTH) ) return SKY;

    /********************************************************
    * figure out what level of detector cordinates is named *
    ********************************************************/
    for(i=0; i< teldef->n_det_levels; ++i) {

        if(!strncmp(name, teldef->det[i]->name, COORD_NAME_LENGTH) ) {
            return i;
        }

    }

    return UNKNOWN;

} /* end of coord_type function */

/**************************************************************************
* read the input parameters
**************************************************************************/
PARAM* readParam(void) {


COORDDEF* coord;
TELDEF* teldef;

double center_x, center_y;

PARAM* param;

/*********************************
* create the parameter structure *
*********************************/
param=(PARAM*)malloc(sizeof(PARAM));


/**************
* teldef file *
**************/
{
char teldef_name[FILENAME_LENGTH];
read_string_param("teldef", teldef_name, FILENAME_LENGTH);
teldef = readTelDef(teldef_name);

}

/**************
* output file *
**************/
read_string_param("outfile", param->outfile, FILENAME_LENGTH);

/*********************
* WCS keyword suffix *
*********************/
read_string_param("suffix", param->suffix, 2);

/******************
* coordinate name *
******************/
read_string_param( "coord", param->coord_name, COORD_NAME_LENGTH);

coord = get_coorddef(teldef, coord_type(teldef, param->coord_name));

if(!strcmp(coord->name, "SKY") ){
    /******************************************************
    * for sky coordinates we need to get the aspect point *
    ******************************************************/
    center_x = read_double_param("ra");
    center_y = read_double_param("dec");
} else if(!strcmp(coord->name, "RAW") ){
    /********************************************
    * for raw coordinates give the pixel number *
    ********************************************/
    center_x = coord->center_x * coord->scale_x;
    center_y = coord->center_y * coord->scale_y;


} else {
    /********************************************
    * otherwise we put the origin at the center *
    ********************************************/
    center_x = 0.0;
    center_y = 0.0;
}

/***************************
* create the WCS structure *
***************************/
param->wcs =createWCSForCoordDef(coord, center_x, center_y );

/***************************************************
* see if we need to translate to image coordinates *
***************************************************/
if(coord->first_pixel_x != 1.0 || coord->first_pixel_y != 1.0 ) {

    XFORM2D* to_image = allocateXform2d();

    setXform2dToTranslation(to_image, 1.0-coord->first_pixel_x, 1.0-coord->first_pixel_y);
    transformWCSPixels(param->wcs, to_image);

    destroyXform2d(to_image);


} /* end if we needed to transform to image coordinates */


return param;


} /* end of  readParam function */

/******************************************************************************
*
******************************************************************************/
int get_segment(TELDEF* teldef) {

    static int segment=-1;

    /***********************************************************
    * if we have already determined the segment number, just
    * return what we found
    ***********************************************************/
    if(segment >=0) return segment;

    /******************************************************
    * if there's only one segment, just return its number *
    ******************************************************/
    if(teldef->nsegments <=1) {
        segment = teldef->min_segment;
        return segment;
    }


    /*****************************
    * read the segment parameter *
    *****************************/
    segment = read_int_param("segment");

    headas_chat(1, "Assuming RAW coordinate segment %d for all data\n",
                segment);


    /*********************************
    * make sure the segment is valid *
    *********************************/
    if(segment<teldef->min_segment || segment >= teldef->nsegments) {
        fprintf(stderr,
                "Invalid segment number %d, must be between %d and %d\n",
                segment, teldef->min_segment, teldef->nsegments-1);
        att_fatal(1);
    }

    return segment;

} /* end of get_segment function */

/***************************************************************************
* Get the COORDDEF structure for a given coordinate integer code
***************************************************************************/
COORDDEF* get_coorddef(TELDEF* teldef, int coord) {

if(coord == SKY ) return teldef->sky;
else if(coord == RAW ) return teldef->raw[get_segment(teldef)];
else return teldef->det[coord];


} /* end of get_coorddef function */

