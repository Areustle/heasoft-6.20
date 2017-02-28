#include <stdlib.h>
#include <string.h>

#include "headas.h"
#include "param.h"
#include "info.h"
#include "ephemeris.h"
#include "random.h"
#include "param_wrappers.h"

#include "att_fatal.h"



/**************************************************************************
**************************************************************************
* read the input parameters
**************************************************************************/
PARAM* readParam(void) {

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

/**************
* teldef file *
**************/
{
char teldef_name[FILENAME_LENGTH];
read_string_param("teldef", teldef_name, FILENAME_LENGTH);
param->teldef = readTelDef(teldef_name);
}

/****************************************************
* should the other extensions in the file be copied *
****************************************************/
param->copy_hdus = read_boolean_param("copy_hdus");

/****************************************************
* should the other extensions in the file be copied *
****************************************************/
param->zero_nulls = read_boolean_param("zeronulls");

/*******************
* transform method *
*******************/
param->method = determineMethod();


/************************************************
* determine the transform to apply to the image *
************************************************/
param->nonlinear=NULL;
determineTransform(param);

/***********************************
* print the transform for the user *
***********************************/
headas_chat(1, "Applying the following transform to the data:\n");
headas_chat(1, "x' = %g * x + %g * y + %g\n",
            param->trans->rot[0][0], param->trans->rot[0][1], param->trans->xshift);

headas_chat(1, "y' = %g * x + %g * y + %g\n",
            param->trans->rot[1][0], param->trans->rot[1][1], param->trans->yshift);

if(param->nonlinear) {
    headas_chat(1, "...with non-linear corrections\n");
}

return param;

} /* end of readParam function */


/******************************************************************************
* read and interpret the transform method from the parameter file
******************************************************************************/
int determineMethod() {

char method[FILENAME_LENGTH];


read_string_param("method", method, FILENAME_LENGTH);

if(!strncasecmp(method, "events",    FILENAME_LENGTH) ) return    EVENTS_METHOD;
if(!strncasecmp(method, "intensity", FILENAME_LENGTH) ) return INTENSITY_METHOD;
if(!strncasecmp(method, "bbox",      FILENAME_LENGTH) ) return      BBOX_METHOD;
if(!strncasecmp(method, "center",    FILENAME_LENGTH) ) return    CENTER_METHOD;
if(!strncasecmp(method, "area",      FILENAME_LENGTH) ) return      AREA_METHOD;

fprintf(stderr, "Unknown transform method %s\n", method);
return UNKNOWN_METHOD;

} /* end of determineMethod function */

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
**************************************************************************
* Set then transform, plus dimensions and WCS keywords for the
* transformed image 
**************************************************************************/
void determineTransform(PARAM* param) {

XFORM2D* trans;
XFORM2D* new;
XFORM2D* dum;
XFORM2D* teldef2output;
TELDEF* teldef;

int status=0;
fitsfile* fp=NULL;

double binx;
double biny;

int from, to;
int lo, hi;

int first_det, last_det;
int level;

double crval1=0.0;
double crval2=0.0;

COORDDEF* from_coord=NULL;
COORDDEF*   to_coord=NULL;

trans = allocateXform2d();
new   = allocateXform2d();
setXform2dToTranslation(trans, 0.0, 0.0 ); /* initilize to identity */

/***************************
* get the teldef structure *
***************************/
teldef = param->teldef;

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
    headas_chat(1, "Identity transform not supported\n");
    param->trans = NULL;
    param->wcs = NULL;
    att_fatal(1);
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


/******************************************************
* open the original image so we can get some things
* from its keywords
******************************************************/
fits_open_file(&fp, param->infile, READONLY, &status);
if(status) {
    fits_report_error(stderr,status);
    att_fatal(1);
}

/*************************************************
* check to be sure there is an image in that HDU *
*************************************************/
{ /* block of local variables */
int hdutype;
long naxis;
fits_get_hdu_type(fp, &hdutype, &status);
fits_read_key_lng(fp, "NAXIS", &naxis, NULL, &status);

if(status) {
    fits_report_error(stderr,status);
    att_fatal(1);
}

if(hdutype != IMAGE_HDU) {
    fprintf(stderr, "Input HDU is not an image\n");
    att_fatal(1);
}

if(naxis != 2) {
    fprintf(stderr, "Input image has %ld axes\n", naxis);
    att_fatal(1);
}

} /* end of block of local variables */



/******************************************************
* determine the image binning factors with respect to
* The coordinates as defined in the teldef file
******************************************************/
binx = read_double_param("binx");
biny = read_double_param("biny");

if(binx<=0.0) {
    /******************************************************
    * default selected for X binning factor. Try reading
    * the IMGBIN keyword
    ******************************************************/
    fits_read_key_dbl(fp, "IMGBIN", &binx, NULL, &status);
    if(status== KEY_NO_EXIST) {
        binx=1;
        status=0;
    }

    if(status) {
        fits_report_error(stderr,status);
        att_fatal(1);
    }

}

/*****************************************
* biny defaults to be the same and binx *
*****************************************/
if(biny<=0.0) biny = binx;

/*********************************
* report the binning factor used *
*********************************/
if(binx==biny) {
    headas_chat(1, "The images are binned by a factor of %g\n", binx);
} else {
    headas_chat(1, 
                "The images are binned by a factor of %g in X and %g in Y\n",
                binx, biny);
}


/*************************
* build up the transform *
*************************/
if(lo==RAW) {
    /************************************
    * transforming from RAW coordinates *
    ************************************/
    int segment=0;

    /**************************************************
    * check if we have to apply nonlinear corrections *
    **************************************************/
    if(teldef->raw_corrections_needed) {
        param->nonlinear = cloneMapXform(teldef->raw_map);
    }


    if(teldef->nsegments >1 ) {
        /*************************
        * get the segment number.*
        *************************/
        char key[FLEN_KEYWORD];
        read_string_param("segment", key, FLEN_KEYWORD);
        if(sscanf(key, "%d", &segment) != 1 ) {
            /****************************************************
            * could not interpret the segment value as a number
            * so assume it is a keyword
            ****************************************************/
            long value;
            fits_read_key_lng(fp, key, &value, NULL, &status);
            if(status) {
                fprintf(stderr, "Could not read segment keyword %s\n", key);
                fits_report_error(stderr, status);
                att_fatal(status);
            } else {
                /**********************************
                * we sucessfully read the keyword *
                **********************************/
                segment = (int)value;
            }

        } /* end if the parameter was not a number */

        headas_chat(1, "Assuming RAW coordinate segment %d for all data\n",
                    segment);
    }

    /*********************************
    * make sure the segment is valid *
    *********************************/
    if(segment<teldef->min_segment || segment >= teldef->nsegments) {
        fprintf(stderr,
                "Invalid segment number %d, must be between %d and %d\n",
                segment, teldef->min_segment, teldef->nsegments-1);
        att_fatal(1);
    }



    /**************************************************
    * do the raw to bottom level detector coordinate
    **************************************************/ 
    combineXform2ds(new, trans, teldef->raw[segment]->trans);
    dum=new;
    new=trans;
    trans=dum;
    
    /*********************************************************
    * save the coordef structure now while we still know the
    * segment number
    *********************************************************/
    if(from==RAW) from_coord =teldef->raw[segment];
    else            to_coord =teldef->raw[segment];


    /*********************************************************
    * if we are ending up in RAW coordinates, then we need
    * to set the WCS keywords now, while we still know the
    * segment number
    *********************************************************/
    if(to==RAW) {
        COORDDEF * coord = teldef->raw[segment];
        param->wcs = createWCS(coord, coord->center_x, coord->center_y, binx, biny);
        param->dimenx = coord->npixels_x / binx;
        param->dimeny = coord->npixels_y / biny;
    }

} /* end if transforming from raw coordinates */

/*********************************************
* transform through the detector coordinates *
*********************************************/
if(lo==RAW) first_det=0;
else        first_det=lo;

if(hi==SKY) last_det = teldef->sky_from;
else        last_det = hi;


for (level=first_det; level<last_det; ++level) {
    /**************************************************
    * do the detector to detector transform *
    **************************************************/ 
    combineXform2ds(new, trans, teldef->det[level]->trans);
    dum=new;
    new=trans;
    trans=dum;
    
    if(param->nonlinear) {
        applyXform2dToMapXform(param->nonlinear, teldef->det[level]->trans );
    }

}


if(hi==SKY) {
    /**********************************
    * transforming to sky coordinates *
    **********************************/
    double ra_nom=0.0;
    double dec_nom=0.0;

    double tstart=0.0;
    double tstop =0.0;
    double time  =0.0;

    QUAT* q=NULL;

    double v=0.0;
    double vhat[3] = {1.0, 0.0, 0.0};


    if(from==SKY) {
        /*************************************************
        * determine nominal RA and Dec from WCS keywords *
        *************************************************/
        fits_read_key_dbl(fp, "CRVAL1",  &ra_nom, NULL, &status);
        fits_read_key_dbl(fp, "CRVAL2", &dec_nom, NULL, &status);
        if(status) {
            fits_report_error(stderr,status);
            att_fatal(1);
        }
        headas_chat(1, "Using nominal RA=%f and Dec=%f from WCS keywords\n",
               ra_nom, dec_nom);
    } else {
        /******************************************
        * read nominal RA and Dec from parameters *
        ******************************************/
         ra_nom = read_double_param("ra");
        dec_nom = read_double_param("dec");
    }

    /***********************************************
    * save the nominal RA/Dec for the WCS keywords *
    ***********************************************/
    crval1 = ra_nom;
    crval2 = dec_nom;

    /*****************************************
    * determine the time the image was taken *
    *****************************************/
    fits_read_key_dbl(fp, "TSTART", &tstart, NULL, &status);
    fits_read_key_dbl(fp, "TSTOP" , &tstop , NULL, &status);
        if(status) {
            fits_report_error(stderr,status);
            att_fatal(1);
        }

    time = 0.5*(tstart + tstop);

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

    /*************************
    * read the attitude file *
    *************************/
    q = allocateQuat();
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

        fits_read_key_dbl(fp, "MJDREF", &mjdref, NULL, &status);
        if(status) {
            fits_report_error(stderr,status);
            att_fatal(1);
        }
        
        mjd=mjdref+time/86400.;
        v=earth_velocity(vhat,earth_longitude(mjd));
    }



    /**************************************************
    * we finally have everything we need to calculate 
    * the detector to sky transformation.
    * because of a quirk in the coordfits library
    * We have to pretend to do an actual transform, and then
    * get the transform used. At some point the library
    * should be fixed to make this less cludgey
    **************************************************/
    {
    double dumx, dumy;

    setSkyCoordCenterInTeldef(teldef, ra_nom, dec_nom);
    convertDetectorToSkyUsingTeldef(teldef, &dumx, &dumy, 0.0, 0.0,
                                    q, v, vhat);

    combineXform2ds(new, trans, teldef->det2sky);
    dum=new;
    new=trans;
    trans=dum;

    if(param->nonlinear) {
        applyXform2dToMapXform(param->nonlinear, teldef->det2sky );
    }

    } /* end of block of local variables */

    /*********************************************************
    * if we are ending up in SKY coordinates, then we need
    * to set the WCS keywords now, while we still know the
    * nominal RA and Dec
    *********************************************************/
    if(to==SKY) {
        COORDDEF * coord = teldef->sky;
        param->wcs = createWCS(coord, ra_nom, dec_nom, binx, biny);
        param->dimenx = teldef->sky->npixels_x / binx;
        param->dimeny = teldef->sky->npixels_y / biny;
    }

    /*****************************
    * save the coordef structrue *
    *****************************/
    if(from==SKY) from_coord = teldef->sky;
    else            to_coord = teldef->sky;

 
} /* end if transforming to sky coordinates */


/*****************************************************************************
* at this point we have set set from_coord and/or to_coord only if they are
* RAW or SKY coordinates. Otherwise they are NULL. So we need to fill them
* in for detector coordinates
******************************************************************************/
if(from_coord==NULL) from_coord = teldef->det[from];
if(  to_coord==NULL)   to_coord = teldef->det[to  ];

/***************************************************************************
* check that the from coord is consistent with the CTYPE keywords in the
* input image if those keywords exist.
***************************************************************************/
{ /* block of local variables */
char ctype1[FLEN_VALUE];
char ctype2[FLEN_VALUE];

fits_read_key_str(fp, "CTYPE1", ctype1, NULL, &status);
fits_read_key_str(fp, "CTYPE2", ctype2, NULL, &status);

if(status != KEY_NO_EXIST) {
    /*******************************************
    * compare the keywords to the column names *
    *******************************************/
    if(from == SKY) {
        /*************************************
        * sky coordinates are a special case *
        *************************************/
        if(strncmp(ctype1, "RA---TAN", FLEN_VALUE) ||
           strncmp(ctype2, "DEC--TAN", FLEN_VALUE)  ) {
            fprintf(stderr, 
                    "Input image does not appear to be in sky coordinates\n");
        }
    } else {
        /*****************************************************
        * for most coordinates we expect the CTYPE keywords
        * to be the column names
        *****************************************************/
        if(strncmp(ctype1, from_coord->name_x, FLEN_VALUE) ||
           strncmp(ctype2, from_coord->name_y, FLEN_VALUE)  ) {
            fprintf(stderr,
                    "%s coordinates not consistent with CTYPE1=%s and CTYPE2=%s\n",
                    param->from_coord, ctype1, ctype2);
        }
    } /* end if we are not in sky coordinates */
} else {
    /*****************************************************
    * No CTYPE keywords, so we just won't worry about it *
    *****************************************************/
    status=0;
}

} /* end of block of local variables */

/**************************************************
* report the transform between teldef coordinates *
**************************************************/
headas_chat(4, "transform between teldef coordinates:\n");
headas_chat(4, "x' = %g * x + %g * y + %g\n",
            trans->rot[0][0], trans->rot[0][1], trans->xshift);

headas_chat(4, "y' = %g * x + %g * y + %g\n",
            trans->rot[1][0], trans->rot[1][1], trans->yshift);

if(param->nonlinear) {
    headas_chat(4, "...with non-linear corrections\n");
}

/*******************************************************************************
* at this point we have a transform between coordinate systems, but we still
* need to transform between the pixel coordinates and the transform coordinates
* Pixel coordinates have a first pixel at (0,0) and may be binned.
*******************************************************************************/
{ /* start of block of local variables */

double offset_x;
double offset_y;

XFORM2D* before;
XFORM2D* after;

XFORM2D* temp;

/*******************************************
* allocate the before and after transforms *
*******************************************/
before = allocateXform2d();
after  = allocateXform2d();

/*******************************************************************
* now we handle the transform from the original pixel coordinates
* to the coordinates as defined in the teldef file.
*******************************************************************/
if(is_upward(from,to)) temp = before;
else                   temp = after; /* things are flipped */

offset_x = read_double_param("from_offx");
offset_y = read_double_param("from_offy");

setImageToCoordDefXform2D(temp, from_coord, binx, biny, offset_x, offset_y);

/*******************************************************************
* do the same thing for the "to" coordinats
*******************************************************************/
if(is_upward(from,to)) temp = after;
else                   temp = before; /* things are flipped */

offset_x = read_double_param("to_offx");
offset_y = read_double_param("to_offy");

setCoordDefToImageXform2D(temp, to_coord, binx, biny, offset_x, offset_y);

teldef2output = allocateXform2d();
copyXform2d(teldef2output, temp);


/**************************************************
* report the transform between teldef coordinates *
**************************************************/
headas_chat(4, "before transform:\n");
headas_chat(4, "x' = %g * x + %g * y + %g\n",
            before->rot[0][0], before->rot[0][1], before->xshift);

headas_chat(4, "y' = %g * x + %g * y + %g\n",
            before->rot[1][0], before->rot[1][1], before->yshift);
            

headas_chat(4, "\nafter transform:\n");
headas_chat(4, "x' = %g * x + %g * y + %g\n",
            after->rot[0][0], after->rot[0][1], after->xshift);

headas_chat(4, "y' = %g * x + %g * y + %g\n",
            after->rot[1][0], after->rot[1][1], after->yshift);

/**************************************
* now put all the transforms together *
**************************************/
temp  = allocateXform2d();

combineXform2ds(temp, before, trans);
combineXform2ds(trans, temp, after);


destroyXform2d(before);
destroyXform2d(after);
destroyXform2d(temp);

/********************************************************************
* ...and take care of the nonlinear part if there is one
* Note that we don't have to do anything about the transform which
* happen before the coordinate to coordinate transform. I *think*
* this is right.
*******************************************************************/
if(param->nonlinear) {
    applyXform2dToMapXform(param->nonlinear, after );
}


} /* end of block of local variables */

/***************************************
* we can close the image FITS file now *
***************************************/
fits_close_file(fp,&status);
if(status) {
    fits_report_error(stderr,status);
    att_fatal(1);
}

/************************************************************************
* We might have to invert the transform. The intensity method requires
* the inverse transform, or we may be transforming "downward".
************************************************************************/
if( (param->method != INTENSITY_METHOD && !is_upward(from,to)) ||
    (param->method == INTENSITY_METHOD &&  is_upward(from,to))   ) {

    invertXform2d(new, trans);
    dum=new;
    new=trans;
    trans=dum;
    
    if(param->nonlinear) {
        /*********************************************
	* we have to invert the non-linear transform *
        *********************************************/
	MAPXFORM* inverse;
	headas_chat(1, "Warning: have to invert a non-linear distortion map ");
        headas_chat(1, "to do this transform.\n This may not be 100%% accurate\n");

	inverse = allocateMapXformWithSameAxes(param->nonlinear);
	invertMapXform(inverse, param->nonlinear);

	destroyMapXform(param->nonlinear);
	param->nonlinear = inverse;

        /******************************************************************
	* and then swap the order of the non-linear and linear transforms *
	******************************************************************/
        applyXform2dToMapXform(param->nonlinear, trans );

    }
} /* end if we need to invert the transform */





/*************************
* set the parameters now *
*************************/
param->trans = trans;


/***********************************************************
* if we are ending up in detector coordinates, then we
* haven't set the WCS keywords yet. For RAW and SKY
* coords, we had to set them above since we needed some
* local variables to do so
***********************************************************/
if(to!=RAW && to!=SKY) {
    COORDDEF * coord = teldef->det[to];
    param->wcs = createWCS(coord, 0.0, 0.0, binx, biny);
    param->dimenx = coord->npixels_x / binx;
    param->dimeny = coord->npixels_y / biny;
}

{
/***************************************************************************
* transform the WCS information
***************************************************************************/
WCS * wcs;

wcs = param->wcs;

applyXform2dToContinuousCoords(teldef2output, &wcs->crpix[0], &wcs->crpix[1],
								to_coord->center_x, to_coord->center_y);

headas_chat(3, "WCSpix: teldef %e, %e => out %e, %e\n",
		to_coord->center_x, to_coord->center_y, wcs->crpix[0], wcs->crpix[1]);

++wcs->crpix[0];
++wcs->crpix[1];

headas_chat(3, "\tWCSpix: => crpix %e, %e\n", wcs->crpix[0], wcs->crpix[1]);

destroyXform2d(teldef2output);
}

/***************************************************************************
* check if we need to override the default dimensions of the output image
***************************************************************************/
{
int dimenx;
int dimeny;

dimenx = read_int_param("to_sizex");
dimeny = read_int_param("to_sizey");

if(dimenx > 0 ) param->dimenx = dimenx;
if(dimeny > 0 ) param->dimeny = dimeny;

} /* end of block of local variables */

} /* end of  determineTransform function */

