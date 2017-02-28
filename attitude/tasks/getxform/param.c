#include <stdlib.h>
#include <string.h>

#include "headas.h"
#include "param.h"

#include "ephemeris.h"

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
**************************************************************************
* returns true if the first coordinate index is "lower" than the second.
**************************************************************************/
int is_upward(int from, int to) {

return (from !=SKY && to != RAW && from<to) || to == SKY || from == RAW;

} /* end of is_upward function */



/**************************************************************************
* read the input parameters
**************************************************************************/
PARAM* readParam(void) {

MAPXFORM* nonlinear=NULL;

XFORM2D* temp;

TELDEF* teldef;

int use_image_coordinates;

int from, to;
int lo, hi;

int first_det, last_det;
int level;

PARAM* param;

/*********************************
* create the parameter structure *
*********************************/
param=(PARAM*)malloc(sizeof(PARAM));
param->quat = NULL;


/**************
* teldef file *
**************/
{
char teldef_name[FILENAME_LENGTH];
read_string_param("teldef", teldef_name, FILENAME_LENGTH);
teldef = readTelDef(teldef_name);
param->teldef = teldef;
}

/**************
* output file *
**************/
read_string_param("outfile", param->outfile, FILENAME_LENGTH);

/***********************
* create the transform *
***********************/
param->combo = allocateComboXform();

/*************************
* initialize some things *
*************************/
temp = allocateXform2d();

/*******************
* coordinate names *
*******************/
read_string_param( "from", param->from_coord, COORD_NAME_LENGTH);
read_string_param(   "to", param->  to_coord, COORD_NAME_LENGTH);

/*****************************
* parse the coordinate names *
*****************************/
from = coord_type(teldef, param->from_coord);
  to = coord_type(teldef, param->to_coord);


if(from==UNKNOWN) {
    headas_chat(1, "Unknown coordinate type %s\n", param->from_coord);
    att_fatal(1);
}

if(to==UNKNOWN) {
    headas_chat(1, "Unknown coordinate type %s\n", param->to_coord);
    att_fatal(1);
}

if(from==to) {
    /*********************
    * identity transform *
    *********************/
    return param;
}

/*****************************************************
* determine the bottom-most and top-most coordinates *
*****************************************************/
if( is_upward(from,to) ) {
    lo=from;
    hi=to;
} else {
    lo=to;
    hi=from;
}

/*****************************************************
* first, are we transforming from pixel coordinates? *
*****************************************************/
use_image_coordinates = read_boolean_param("image");
if(use_image_coordinates) {
    /******************************************************
    * need to apply the transform from image coordinates
    * to teldef coordinates
    ******************************************************/
    COORDDEF* coord;

    coord = get_coorddef(param->teldef, lo);

    setXform2dToTranslation(temp, coord->first_pixel_x -1.0,
                                    coord->first_pixel_y -1.0);

    applyXform2dAfterComboXform(param->combo, temp);

} /* end if we are translating from image to teldef coordinates */


/************************************************************************
* we need to handle the transform from RAW to detector in a special way *
************************************************************************/
if(lo==RAW) {
    /**************************************************
    * check if we have to apply nonlinear corrections *
    **************************************************/
    if(teldef->raw_corrections_needed) {
        nonlinear = cloneMapXform(teldef->raw_map);
        param->combo->map = nonlinear;
    }

    /**************************************************
    * do the raw to bottom level detector coordinate
    **************************************************/
    combineXform2ds(temp, param->combo->trans,
                          teldef->raw[get_segment(teldef)]->trans);
    copyXform2d(param->combo->trans, temp);

} /* end if transforming from raw coordinates */

/*********************************************
* transform through the detector coordinates *
*********************************************/
if(lo==RAW) first_det=0;
else        first_det=lo;

if(hi==SKY) last_det = teldef->sky_from;
else        last_det = hi;


for(level=first_det; level<last_det; ++level) {
    /****************************************
    * do the detector to detector transform *
    ****************************************/
    applyXform2dAfterComboXform(param->combo, teldef->det[level]->trans );
}


if(hi==SKY) {
    /**********************************
    * transforming to sky coordinates *
    **********************************/
    double ra_nom=0.0;
    double dec_nom=0.0;

    double time  =0.0;
    double margin =0.0;

    QUAT* q=NULL;

    double v=0.0;
    double vhat[3] = {1.0, 0.0, 0.0};

    /******************************************
    * read nominal RA and Dec from parameters *
    ******************************************/
    ra_nom = read_double_param("ra");
    dec_nom = read_double_param("dec");

    /************************************************
    * determine the time so we can get the attitude *
    ************************************************/
    time = read_double_param("time");

    /*************************************
    * get the attitude at the image time *
    *************************************/
    { /* block of local variables */
    char att_name[FILENAME_LENGTH];
    ATTFILE* att;

    read_string_param("attfile", att_name, FILENAME_LENGTH);
    att = openAttFile(att_name);
    if(att==NULL) {
        headas_chat(1, "Could not open attitude file %s\n", att_name);
        att_fatal(1);
    }
    
    /********************************************
    * check if the time is in the attitude file *
    ********************************************/
    margin = read_double_param("timemargin");
    resetAttFileExtrapolationLimits(att, margin);
    if(! isInExtrapolatedAttFile(att, time) ) {
        fprintf(stdout, "Warning: Time %.14g not covered by attitude file %s\n",
                time, att_name);
    }


    /*************************
    * read the attitude file *
    *************************/
    param->quat = q = allocateQuat();
    param->time = time;
    findQuatInAttFile(att,q,time);
    closeAttFile(att);

    /***************************
    * report the attitude used *
    ***************************/
    {
    double ra, dec, roll;
    convertQuatToRADecRoll(teldef->alignment, q, &ra, &dec, &roll);
    
    headas_chat(1, "Using %s %s pointing RA=%f Dec=%f Roll=%f at time %.14g\n",
                teldef->mission, teldef->instrument,
                ra,dec,roll,time);
    }

    } /* end of block of local variables */

    /*************
    * aberration *
    *************/
    if (read_boolean_param("aberration")) {
        double mjd, mjdref;

        mjdref = read_double_param("mjdref");
        
        mjd=mjdref+time/86400.;
        v=earth_velocity(vhat,earth_longitude(mjd));
    }



    /**************************************************
    * we finally have everything we need to calculate 
    * the detector to sky transformation.
    * Because of a quirk in the coordfits library
    * We have to pretend to do an actual transform, and then
    * get the transform used. At some point the library
    * should be fixed to make this less cludgey
    **************************************************/
    {
    double dumx, dumy;

    setSkyCoordCenterInTeldef(teldef, ra_nom, dec_nom);
    convertDetectorToSkyUsingTeldef(teldef, &dumx, &dumy, 0.0, 0.0,
                                    q, v, vhat);
                                    
    applyXform2dAfterComboXform(param->combo, teldef->det2sky);

    } /* end of block of local variables */



} /* end if transforming to sky coordinates */

/*********************************************************
* finally, check if we are doing the transform in terms
* of image coordinates
*********************************************************/
if(use_image_coordinates) {

    COORDDEF* coord;

    coord = get_coorddef(param->teldef, hi);

    setXform2dToTranslation(temp, 1.0 - coord->first_pixel_x ,
                                  1.0 - coord->first_pixel_y  );

    applyXform2dAfterComboXform(param->combo, temp);

} /* end if we are translating from image to teldef coordinates */


/************************************************************************
* We might have to invert the transform. The intensity method requires
* the inverse transform, or we may be transforming "downward".
************************************************************************/
if( !is_upward(from,to) ) {

    COMBOXFORM* inverse;

    inverse = invertComboXform(param->combo);
    destroyComboXform(param->combo);
    
    param->combo = inverse;

} /* end if we need to invert the transform */


/***************************************
* free up temporary storage and return *
***************************************/
destroyXform2d(temp);

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



int parstamp_path (const char * path)
{
   fitsfile * fptr = 0;
   int status = 0;

   fits_open_file(&fptr, path, READWRITE, &status);

   HDpar_stamp(fptr, 0, &status);
   fits_write_chksum(fptr, &status);

   if (fptr) {
      int tmp = 0;
      fits_close_file(fptr, &tmp);
   }

   fits_report_error(heaout, status);

   return status;
}

