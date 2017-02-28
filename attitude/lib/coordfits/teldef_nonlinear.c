#include <math.h>
#include <string.h>
#include "teldef.h"

/*
#define DEBUG
*/

/***************************************************************************
****************************************************************************
* read the non-linear corrections required for the raw to bottom level
* detector coordinates transformation if any are required.
* This routine is specific to the ASCA GIS
***************************************************************************/
void readGISNonLinearCorrectionsInTeldef(TELDEF* teldef, fitsfile* fp) {

int status=0;
int hdutype;
int anynull;
MAPXFORM_TYPE null_value = 0.;
int original_hdu;
int mapxform_fits_type;
long dimen1, dimen2;

MAPXFORM* map;

int i,j;

double angle;
XFORM2D* rotation;
XFORM2D* inversion;
XFORM2D* int2det;

COORDDEF* det;

/********************************************************
* we assume this is an ASCA GIS teldef file
* and requires non-linear corrections
********************************************************/
teldef->raw_corrections_needed=1;

/********************************************************************
* remember the original HDU so we can return there when we are done *
********************************************************************/
fits_get_hdu_num(fp,&original_hdu);

/******************************************************
* get FITS data type code for MAPXFORM arraydata type *
******************************************************/
if(     sizeof(MAPXFORM_TYPE)==sizeof(float ) ) mapxform_fits_type=TFLOAT;
else if(sizeof(MAPXFORM_TYPE)==sizeof(double) ) mapxform_fits_type=TDOUBLE;
else {
    fprintf(stderr,"Error: MAPXFORM_TYPE not float or double\n");
    exit(1);
}

#ifdef DEBUG
printf("readGISNonLinearCorrectionsInTeldef: about to read maps\n");
#endif


/**************************************************************************
* read deltax array - note this is hard-wired to HDU 5 (primary=1) since
* that's the way ascalin did things. A more general scheme would have
* descriptive EXTNAME keywords
*************************************************************************/
fits_movabs_hdu(fp,5/*hdu number*/, &hdutype, &status);

fits_read_key_lng(fp,"NAXIS1",&dimen1,NULL,&status);
fits_read_key_lng(fp,"NAXIS2",&dimen2,NULL,&status);

teldef->raw_map =allocateMapXform(dimen1,dimen2);
map = teldef->raw_map;

fits_read_img(fp,mapxform_fits_type, 1l, dimen1*dimen2,
              &null_value,
              getMapXformDeltaxBlock(map), 
              &anynull, &status);

checkTelDefFITSerrors(status,"reading DeltaX raw correction array from",teldef);

/***********************************************************************
* read deltay array  - hard-wired to HDU 7 (primary=1). See note above *
***********************************************************************/
fits_movabs_hdu(fp,7/*hdu number*/, &hdutype, &status);

fits_read_key_lng(fp,"NAXIS1",&dimen1,NULL,&status);
fits_read_key_lng(fp,"NAXIS2",&dimen2,NULL,&status);

if(dimen1 != map->dimenx ||
   dimen2 != map->dimeny   ) {
    fprintf(stderr,
            "DeltaX map dimensions, %d x %d, differ from DeltaY, %ld x %ld\n",
            map->dimenx, map->dimeny, dimen1, dimen2 );
    exit(1);
}

fits_read_img(fp,mapxform_fits_type, 1l, dimen1*dimen2,
              &null_value, 
              getMapXformDeltayBlock(map),
              &anynull, &status);

checkTelDefFITSerrors(status,"reading DeltaY raw correction array from",teldef);

/**************************************
* set the coordinate to pixel mapping *
**************************************/
setMapXformAxes(map, 1., 1., 1., 1.);

/***********************************************************************
* the map deltax and deltay are in milimeters, so scale them to pixels *
***********************************************************************/
det=teldef->det[0];
for(j=0;j<map->dimeny;++j) {
    for(i=0;i<map->dimenx;++i) {
        map->deltax[j][i] /= det->scale_x;
        map->deltay[j][i] /= det->scale_y;
    }
}


/*****************************************************************************
* The maps in the GIS teldef files are in terms of an internal coordinate
* system which is rotated and inverted with respect to the DET coordinates.
* so we have to transform the map so that it applies to the DET
* coordinates.
*****************************************************************************/

/************************
* return to primary HDU *
************************/
fits_movabs_hdu(fp,original_hdu, &hdutype, &status);

#ifdef DEBUG
printf("readGISNonLinearCorrectionsInTeldef: about to determine int2det\n");
#endif


/**********************
* sat -> det rotation *
**********************/
fits_read_key_dbl(fp,"DET_ROTD",&angle,NULL,&status);
checkTelDefFITSerrors(status,"reading DET_ROTD from",teldef);
angle*=-M_PI/180.; /* convert to radians and take negative */



rotation=allocateXform2d();
setXform2dToRotation(rotation, sin(angle), cos(angle),
                     det->center_x, det->center_y);

/******************************************
* sat -> det inversion  - flip the Y axis *
******************************************/
inversion=allocateXform2d();
setXform2dToScaling(inversion, 1.0, -1.0, det->center_x, det->center_y);


/*****************************
* combine all the transforms *
*****************************/
int2det=allocateXform2d();
combineXform2ds(int2det,rotation,inversion);

destroyXform2d(rotation);
destroyXform2d(inversion);

/********************
* transform the map *
********************/
#ifdef DEBUG
printf("readGISNonLinearCorrectionsInTeldef: about to transform map\n");
#endif

applyXform2dToMapXform(map, int2det);
destroyXform2d(int2det);

#ifdef DEBUG
printf("readGISNonLinearCorrectionsInTeldef: done transforming map\n");
#endif



/****************************************************************
* check for any stray FITS errors before we leave this function *
****************************************************************/
checkTelDefFITSerrors(status,"reading raw corrections from",teldef);

} /* end of readGISNonLinearCorrectionsInTeldef function */


/***************************************************************************
****************************************************************************
* read the non-linear corrections required for the raw to bottom level
* detector coordinates transformation if any are required.
***************************************************************************/
void readDetToMapPixelScaling(double* scale, double* origin, int axis,
                              fitsfile* fp) {

int status=0;
double crpix, crval, cdelt;

char key[FLEN_KEYWORD];

#ifdef DEBUG
printf("readDetToMapPixelScaling: start\n");
#endif

if(axis != 1 && axis != 2) {
    fprintf(stderr,"Illegal image axis %d\n",axis);
    return;
}

sprintf(key, "CRPIX%d", axis);
fits_read_key_dbl(fp, key, &crpix, NULL, &status);

sprintf(key, "CRVAL%d", axis);
fits_read_key_dbl(fp, key, &crval, NULL, &status);

sprintf(key, "CDELT%d", axis);
fits_read_key_dbl(fp, key, &cdelt, NULL, &status);

if(status) {
    /***********************************************
    * default values if we can't read the keywords *
    ***********************************************/
    crpix=1.;
    crval=1.;
    cdelt=1.;
}

*origin = crval - crpix*cdelt;
*scale = 1./cdelt;

} /* end of readDetToMapPixelScaling */


/***************************************************************************
* Are the corrections expressed in raw or detector coordinates?
* If they are in raw coordinates, then we need to transform the
* map to detector coordinates, since the corrections are applied after the
* linear part of the raw -> detector transform
***************************************************************************/
int isTeldefDistortionInRawCoords(TELDEF* teldef, fitsfile* fp) {

int status=0;
int in_raw;

fits_read_key_log(fp, "CORINRAW", &in_raw, NULL, &status);
checkTelDefFITSerrors(status,"reading CORINRAW keyword",teldef);
if(in_raw) {

    /****************************************************
    * check if there is more than one segment. If so
    * this is probably a mistake, so give a warning
    ***************************************************/
    if(getTelDefSegmentCount(teldef) > 1) {
        fprintf(stderr,
        "Warning: Non-linear corrections applied in segment %d coordinates\n",
		teldef->min_segment);
    }

} /* end if the map is in raw coordinates */

return in_raw;

} /* end of isTeldefDistortionInRawCoords */


/****************************************************************************
*
****************************************************************************/
void readTableMapXformFromTeldef(TELDEF* teldef, fitsfile* fp) {

int status=0;

long npoints;
double* x;
double* y;
double* x1;
double* y1;
double* deltax;
double* deltay;
int col;
int anynull=0;
int i;

char method[FLEN_VALUE];
long binx;
long biny;
COORDDEF* det;

XFORM2D* linear_approx;

/**********************
* read the table data *
**********************/
fits_read_key_lng(fp, "NAXIS2", &npoints, NULL, &status);

x      = (double*)malloc(sizeof(double)*npoints);
y      = (double*)malloc(sizeof(double)*npoints);
x1     = (double*)malloc(sizeof(double)*npoints);
y1     = (double*)malloc(sizeof(double)*npoints);
deltax = (double*)malloc(sizeof(double)*npoints);
deltay = (double*)malloc(sizeof(double)*npoints);

fits_get_colnum(fp, CASESEN, "X", &col, &status);
fits_read_col_dbl(fp, col, 1l, 1l, npoints, 0.0, x, &anynull, &status);

fits_get_colnum(fp, CASESEN, "Y", &col, &status);
fits_read_col_dbl(fp, col, 1l, 1l, npoints, 0.0, y, &anynull, &status);

fits_get_colnum(fp, CASESEN, "NEWX", &col, &status);
fits_read_col_dbl(fp, col, 1l, 1l, npoints, 0.0, x1, &anynull, &status);

fits_get_colnum(fp, CASESEN, "NEWY", &col, &status);
fits_read_col_dbl(fp, col, 1l, 1l, npoints, 0.0, y1, &anynull, &status);

checkTelDefFITSerrors(status,"reading nonlinear correction table",teldef);


/*******************************************************************
* get the best fit linear transform so that we are left with only
* residuals
*******************************************************************/
linear_approx = allocateXform2d();
findBestXform2d(linear_approx, x, y, x1, y1, NULL, (int)npoints);

for(i=0; i<npoints; ++i) {

    double x2, y2;
    
    applyXform2dToContinuousCoords(linear_approx, &x2, &y2, x[i], y[i]);

    deltax[i] = x1[i] -x2;
    deltay[i] = y1[i] -y2;
}



/*************
* set up map *
*************/
fits_read_key_lng(fp, "SAMPLEX", &binx, NULL, &status);
fits_read_key_lng(fp, "SAMPLEY", &biny, NULL, &status);

det = teldef->det[0];
teldef->raw_map=allocateMapXform(det->npixels_x/binx, det->npixels_y/biny);

setMapXformAxes(teldef->raw_map, det->first_pixel_x, (double)binx,
			     det->first_pixel_y, (double)biny);


/**********************
* interpolate the map *
**********************/
fits_read_key_str(fp, "METHOD", method, NULL, &status);
checkTelDefFITSerrors(status,"reading METHOD keyword",teldef);

if(strcasecmp(method, "distance")) {
    fprintf(stderr, "Unknown distortion map interpolation method %s\n",
            method);
}

setMapXformFromListOfPoints(teldef->raw_map, x1, y1, deltax, deltay, (int)npoints );

/*********************************************************
* now merge these transforms with the raw->det transform *
*********************************************************/
if(isTeldefDistortionInRawCoords(teldef, fp) ) {
    /*********************************************************
    * the non-linear part comes before the raw2det transform *
    *********************************************************/

    XFORM2D* raw2det;
    XFORM2D* combined;

    raw2det = teldef->raw[0]->trans;

    /************************************************
    * we start with linear_approx nonlinear raw2det *
    ************************************************/
    applyXform2dToMapXform(teldef->raw_map, raw2det);

    /**********************************************
    * now we have linear_approx raw2det nonlinear *
    **********************************************/
    combined = allocateXform2d();
    combineXform2ds(combined, linear_approx, raw2det);

    /*********************************
    * now we have combined nonlinear *
    *********************************/
    destroyXform2d(raw2det);
    teldef->raw[0]->trans = combined;

} else {
    /********************************************************
    * the non-linear part comes after the raw2det transform *
    ********************************************************/

    XFORM2D* raw2det;
    XFORM2D* combined;

    raw2det = teldef->raw[0]->trans;

    /************************************************
    * we start with raw2det linear_approx nonlinear *
    ************************************************/
    combined = allocateXform2d();
    combineXform2ds(combined, raw2det, linear_approx);

    /*********************************
    * now we have combined nonlinear *
    *********************************/
    destroyXform2d(raw2det);
    teldef->raw[0]->trans = combined;

} /* end if the map is specified in detector coordinates */

/****************************************************************
* check for any stray FITS errors before we leave this function *
****************************************************************/
checkTelDefFITSerrors(status,"reading distortion map table",teldef);

} /* end of readTableMapXformFromTeldef function */

/****************************************************************************
*
****************************************************************************/
void readImageMapXformFromTeldef(TELDEF* teldef, fitsfile* fp) {

int status=0;
int anynull=0;

long dimen1;
long dimen2;
long npixels;

int mapxform_fits_type;
MAPXFORM_TYPE null_value = 0.;

double scale_x, scale_y;
double origin_x, origin_y;

/*********************************************
* determine the dimensions of the map arrays
* and allocate the map transform
*********************************************/
fits_read_key_lng(fp,"NAXIS1",&dimen1,NULL,&status);
fits_read_key_lng(fp,"NAXIS2",&dimen2,NULL,&status);

teldef->raw_map=allocateMapXform(dimen1,dimen2);


/*********************************************************************
* read the WCS keywords. These give the mapping from map pixels
* to detector coordinates,
**********************************************************************/
readDetToMapPixelScaling(&scale_x, &origin_x, 1, fp);
readDetToMapPixelScaling(&scale_y, &origin_y, 2, fp);

setMapXformAxes(teldef->raw_map, origin_x, scale_x, origin_y, scale_y);

/******************************************************
* get FITS data type code for MAPXFORM arraydata type *
******************************************************/
if(     sizeof(MAPXFORM_TYPE)==sizeof(float ) ) mapxform_fits_type=TFLOAT;
else if(sizeof(MAPXFORM_TYPE)==sizeof(double) ) mapxform_fits_type=TDOUBLE;
else {
    fprintf(stderr,"Error: MAPXFORM_TYPE not float or double\n");
    exit(1);
}
/****************************************************************
* Now read the correction values from the image
* the image has two planes, the first is the deltax values and
* the second is the deltay values
****************************************************************/
npixels = dimen1*dimen2;

fits_read_img(fp,mapxform_fits_type, 1l, npixels,
	&null_value,
	getMapXformDeltaxBlock(teldef->raw_map),
	&anynull, &status);

fits_read_img(fp,mapxform_fits_type, 1l+npixels, npixels,
	&null_value,
	getMapXformDeltayBlock(teldef->raw_map),
	&anynull, &status);

checkTelDefFITSerrors(status,"reading nonlinear correction array from",teldef);

/*********************************************************
* now merge these transforms with the raw->det transform *
*********************************************************/
if(isTeldefDistortionInRawCoords(teldef, fp) ) {

    /******************************************************
    * we have nonlinear raw2det, so we need to flip them *
    ******************************************************/
    applyXform2dToMapXform(teldef->raw_map, teldef->raw[0]->trans);
    
} /* end if the map is in RAW coordinates */

} /* end of readImageMapXformFromTeldef method */



/***************************************************************************
****************************************************************************
* read the non-linear corrections required for the raw to bottom level
* detector coordinates transformation (if any).
***************************************************************************/
void readNonLinearCorrectionsInTeldef(TELDEF* teldef, fitsfile* fp) {

int status=0;
int hdutype;
int original_hdu;





#ifdef DEBUG
printf("readNonLinearCorrectionsInTeldef: start\n");
#endif

/**********************************************************************
* The ASCA GIS has a mission-dependant format for its non-linear
* corrections. So we call a different routine to read them.
**********************************************************************/
if(!strncmp(teldef->mission,"ASCA",FLEN_VALUE) &&
      (!strcmp(teldef->instrument,"GIS2") ||
       !strcmp(teldef->instrument,"GIS3")   )  ) {
    /****************************************************
    * ASCA GIS, so call the instrument specific routine *
    ****************************************************/
    readGISNonLinearCorrectionsInTeldef(teldef, fp);
    return;
}

/********************************************************************
* remember the original HDU so we can return there when we are done *
********************************************************************/
fits_get_hdu_num(fp,&original_hdu);


/*********************************
* Look for a NONLINEAR extension *
*********************************/
fits_movnam_hdu(fp, ANY_HDU, "NONLINEAR", 0/*any version*/, &status);

if(status == BAD_HDU_NUM) {
    /******************************************************
    * no such HDU, so there are no non-linear corrections *
    ******************************************************/
    teldef->raw_corrections_needed=0;
    status=0;
    return;
}

#ifdef DEBUG
printf("readNonLinearCorrectionsInTeldef: found NONLINEAR extension\n");
#endif


/********************************************************
* if we get here, then we have standard-format non-linear
* corrections
********************************************************/
teldef->raw_corrections_needed=1;

/**********************************************************************
* the non-linear corrections can be stored as an image or as a list
* of points in a table
*********************************************************************/
fits_get_hdu_type(fp, &hdutype, &status);

if(hdutype == IMAGE_HDU) readImageMapXformFromTeldef(teldef, fp);
else                     readTableMapXformFromTeldef(teldef, fp);


/************************
* return to primary HDU *
************************/
fits_movabs_hdu(fp,original_hdu, &hdutype, &status);

/****************************************************************
* check for any stray FITS errors before we leave this function *
****************************************************************/
checkTelDefFITSerrors(status,"reading raw corrections from",teldef);

} /* end of readNonLinearCorrectionsInTeldef function */
