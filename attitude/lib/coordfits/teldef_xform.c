#include <string.h>

#include "teldef.h"

/*
#define DEBUG
*/

/************************************************************************
*************************************************************************
* set the orientation of the sky coordinate tanmgent plane 
* This is used for detector to sky conversions
* R.A. and Dec. are the "nominal pointing" in decimal degrees.
* the tangent plane will have zhat pointing toward (ra,dec)
* xhat will point along -RA and yhat will point along +Dec.
************************************************************************/
void setSkyCoordCenterInTeldef(TELDEF* teldef, double ra, double dec ) {

ALIGN* align;

align = allocateDefaultAlign();
convertRADecRollToQuat(align, teldef->q0, ra, dec, 0.0);
destroyAlign(align);

convertQuatToRotMatrix(teldef->rot0,teldef->q0);


} /* end of setSkyCoordCenterInTeldef function */


/****************************************************************************
******************************************************************************
* This function converts raw coordinates to bottom level detector coordinates
* Note that it takes three arguments for the segment, rawx, and rawy, all
* integers. 
* dx, and dy give the location of the event with respect to the pixel
* center. typically these are random numbers between -.5 and +.5 
* (see applyXform2dToDiscreteCoords).
* If you want to transform real valued raw coordinates, 
* use convertContRawToDetectorUsingTeldef
*****************************************************************************/
void convertRawToDetectorUsingTeldef(TELDEF* teldef, double* detx, double* dety,
                                     int seg, int rawx, int rawy, 
                                     double dx, double dy) {

convertContRawToDetectorUsingTeldef(teldef, detx, dety,
                                    seg, (double)rawx+dx, (double)rawy+dy );


} /* end of convertRawToDetectorUsingTeldef function */

/****************************************************************************
******************************************************************************
* This function converts raw coordinates to bottom level detector coordinates
* Note that rawx and rawy are given as doubles. 
* See also convertRawToDetectorUsingTeldef
******************************************************************************/
void convertContRawToDetectorUsingTeldef(TELDEF* teldef, 
                                         double* detx, double* dety,
                                         int seg, double rawx, double rawy) {

    /********************************
    * check the segment for validity *
    *********************************/
    if(seg<teldef->min_segment || seg >=teldef->nsegments) {
        fprintf(stderr, "Invalid segment number %d\n", seg);
        exit(1);
    }

    /***************************
    * do linear transformation *
    ***************************/
    applyXform2dToContinuousCoords(teldef->raw[seg]->trans,
                                   detx,dety,rawx,rawy);

    /**********************************
    * apply map corrections if needed *
    **********************************/
    if(teldef->raw_corrections_needed) {
        double tmpx, tmpy;

        tmpx=*detx;
        tmpy=*dety;
        applyMapXform(teldef->raw_map, detx, dety, tmpx, tmpy);

        #ifdef DEBUG
        printf("convertRawToDetectorUsingTeldef tmp=(%g %g) det=(%g %g)\n",
               tmpx, tmpy, *detx, *dety);
        #endif

    }


} /* end of convertContRawToDetectorUsingTeldef function */


/******************************************************************************
******************************************************************************
* this function converts bottom level detector coordinates to raw coordinates.
* the function returns the number of the segment containing the coordinates.
* If the point does not lie on any segment the function will return -1,
* and the values of rawx and rawy will be undefined.
* If the point is on more than one overlapping segment, the results are 
* undefined.
******************************************************************************/
int convertDetectorToRawUsingTelDef(TELDEF* teldef, double* rawx, double* rawy,
                                    double detx, double dety) {

int seg;
COORDDEF* raw;

#ifdef DEBUG
printf("convertDetectorToRawUsingTelDef: start\n");
printf("convertDetectorToRawUsingTelDef: det0x_min=%g\n",teldef->det0x_min);
printf("convertDetectorToRawUsingTelDef: det0x_max=%g\n",teldef->det0x_max);
printf("convertDetectorToRawUsingTelDef: det0y_min=%g\n",teldef->det0y_min);
printf("convertDetectorToRawUsingTelDef: det0y_max=%g\n",teldef->det0y_max);
#endif

/***************************************************************************
* if non-linear corrections are needed we just bail - until some later 
* date when we perhaps write code to handle this 
***************************************************************************/
if(teldef->raw_corrections_needed) {
    fprintf(stderr,
      "Can't convert detector to raw since nonlinear corrections are needed\n");
    exit(1);
}


if(detx<teldef->det0x_min || detx>teldef->det0x_max ||
   dety<teldef->det0y_min || dety>teldef->det0y_max   ) {
   /******************************
   * outside of FOV bounding box *
   ******************************/
    return(-1);
}

/***************************************************************
* loop over all segments, checking if the point is on each one *
***************************************************************/
for(seg=teldef->min_segment;seg<teldef->nsegments;++seg) {

    raw=teldef->raw[seg];

    if(detx>=raw->trans_min_x && detx<=raw->trans_max_x &&
       dety>=raw->trans_min_y && dety<=raw->trans_max_y    ) {
        /****************************************************
        * point is within the bounding box for this segment *
        ****************************************************/
        applyXform2dToContinuousCoords(raw->inverse_trans,rawx,rawy,detx,dety);
       
        if(*rawx >= raw->min_x && *rawx <= raw->max_x && 
           *rawy >= raw->min_y && *rawy <= raw->max_y    ) {
            /***************************************
            * found a segment containing the point *
            ***************************************/
            return(seg);
        }
    } /* end if within bounding box for this segment */

} /* end of loop over segments */

/********************************************************
* if the point was on a segment we would have returned
*  already, so we must not be on any segment 
********************************************************/
return(-1);


}/* end of convertDetectorToRawUsingTelDef */



/****************************************************************************
*****************************************************************************
* Convert one level of detector coordinates to the next.
* Note this function assumes the input coordinates are continuous.
* if you want to transform from discrete coordinates (adding a random
* location within the pixel) you will have to do this "by hand" using
* applyXform2dToDiscreteCoords. 
* Perhaps this is not the most efficient implimentation, particularly if
* you are transforming a large number of coordinates over more
* than two levels of coordinates.
****************************************************************************/
void convertDetectorToDetectorUsingTelDef(TELDEF* teldef, int from, int to,
                                          double* x1, double* y1,
                                          double x0, double y0) {
int level;

if(to>from) {
    /********************
    * transforming "up" *
    ********************/
    applyXform2dToContinuousCoords(teldef->det[from]->trans,x1,y1,x0,y0);

    for(level=from+1;level<to;++level) {
        applyXform2dToContinuousCoords(teldef->det[level]->trans,x1,y1,*x1,*y1);
    }

} else if (from>to) {
    /**********************
    * transforming "down" *
    **********************/
    applyXform2dToContinuousCoords(teldef->det[from-1]->inverse_trans,
                                   x1,y1,x0,y0);

    for(level=from-2;level>=to;--level) {
        applyXform2dToContinuousCoords(teldef->det[level]->trans,x1,y1,*x1,*y1);
    }

} else {
    /***********************************
    * same level - kind of stupid, ya? *
    ***********************************/
    *x1=x0;
    *y1=y0;
} /* end of cases for to and from */

} /* end of convertDetectorToDetectorUsingTelDef function */



/****************************************************************************
*****************************************************************************
* Convert detector coordinates to sky coordinates.
* q is the current spacecraft pointing
* v and vhat are the scalar and vector components of the earth's velocity
*            these two quantities are used for abberation correction.
*            aberation will not be applied if v=0.;
* Note this routine is not efficient if you have to convert many different
* coordinates which have the same pointing and aberration
* (see repeatDetectorToSkyTeldefConversion).
* Note also that you must first set the sky tangent plane using
* setSkyTangetPlaneInTeldef before the first call to this routine.
*****************************************************************************/
void convertDetectorToSkyUsingTeldef(TELDEF* teldef, 
                                     double* skyx, double* skyy,
                                     double  detx, double  dety, 
                                     QUAT* q, 
                                     double v, double vhat[3]) {

COORDDEF* det;

det = teldef->det[teldef->sky_from];

/*********************************************************************
* adjust for misalignment between the telescope and spacecraft axies *
*********************************************************************/
productOfQuats(teldef->xrt,q,teldef->alignment->q_inverse);

/******************************************************
* find rotation from nominal pointing to XRT pointing *
******************************************************/
getQuatOfChange(teldef->delta_q,teldef->q0,teldef->xrt);

/**************************************
* find the basic det -> sky transform *
**************************************/
convertQuatToXform2d(teldef->det2sky, teldef->delta_q,
                     det->center_x, det->center_y,
                     teldef->sky->center_x, teldef->sky->center_y, 
                     teldef->sky_pix_per_radian);

/**************************************
* correct for abberation if necessary *
**************************************/
if(v!=0.) addAberrationToXform2d(teldef->det2sky,teldef->rot0,v,vhat,
                                 teldef->sky_pix_per_radian);

/***********************************
* do the DET -> SKY transformation *
***********************************/
applyXform2dToContinuousCoords(teldef->det2sky, skyx, skyy, detx, dety);


} /* end of convertDetectorToSkyUsingTeldef function */


/*****************************************************************************
******************************************************************************
* do a bottom level detector to sky coordinate conversion using the same
* pointing and aberration as in the most recent call to 
* convertDetectorToSkyUsingTeldef. This is useful if you are converting
* the coordinate for a number of events which all happened at the same time
*****************************************************************************/
void repeatDetectorToSkyTeldefConversion(TELDEF* teldef, 
                                         double* skyx, double* skyy,
                                         double  detx, double  dety ) {

/*************************************************
* simply reapply the last det2sky transformation *
*************************************************/
applyXform2dToContinuousCoords(teldef->det2sky, skyx, skyy, detx, dety);

}/* end of repeatDetectorToSkyTeldefConversion function */


/*****************************************************************************
******************************************************************************
* find the detector coordinate level corresponding to a given 
* 3 character coordinate type name
* returns -1 if there is no matching detector coordinate
*****************************************************************************/
int getTelDefDetLevelFromName(TELDEF* teldef, char* name) {

int level;

for(level=0; level<teldef->n_det_levels && 
             strcmp(teldef->det[level]->name, name); 
    ++level);

if(level >= teldef->n_det_levels) level=-1;

return(level);

} /* end of getTelDefDetLevelFromName function */
