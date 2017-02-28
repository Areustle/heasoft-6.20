#include "comboxform.h"

#include <string.h>
#include <math.h>

#include "coord.h"
#include "coordwcs.h"

#define NEARLY_ZERO 1e-15

/******************************************************************************
* create a new comboxform set to the identity transform
******************************************************************************/
COMBOXFORM* allocateComboXform() {

COMBOXFORM* combo;

combo = (COMBOXFORM*)malloc(sizeof(COMBOXFORM));

combo->trans = allocateXform2d();
setXform2dToTranslation(combo->trans, 0.0, 0.0);

combo->map=NULL;

return combo;

} /* end of allocateComboXform function */

/****************************************************************************
*
****************************************************************************/
void destroyComboXform(COMBOXFORM* combo) {

    destroyXform2d(combo->trans);
    if(combo->map) destroyMapXform(combo->map);
    
    free(combo);
    
} /* end of destroyComboXform function */

/****************************************************************************
*
****************************************************************************/
void applyComboXform(COMBOXFORM* combo, double* newx, double* newy,
                                        double  oldx, double  oldy ) {
                                        
    applyXform2dToContinuousCoords(combo->trans, newx, newy, oldx, oldy);

	if(combo->map) {
	    /********************************
	    * apply a non-linear correction *
	    ********************************/
	    double tmp_x, tmp_y;

	    applyMapXform(combo->map, &tmp_x, &tmp_y, *newx, *newy);

	    *newx=tmp_x;
	    *newy=tmp_y;
	}

} /* end of applyComboXform function */


/****************************************************************************
*
****************************************************************************/
void writeComboXform(COMBOXFORM* combo, char* filename) {

fitsfile* fp=NULL;
int status=0;

XFORM2D* xform;
MAPXFORM* map;

xform = combo->trans;
map   = combo->map;


/******************************************
* create the FITS file as an empty header *
******************************************/
fits_create_file(&fp, filename, &status);

/**********************************************************************
* if there is a nonlinear part, then we encode it as an image in the 
* primary header
**********************************************************************/
if(map) {

    long naxes[3];
    int naxis=3;
    int bitpix;
    int type;
    long npixels;
    long start[] = { 1l, 1l, 1l};


    /*******************
    * image dimensions *
    *******************/
    bitpix = -8*sizeof(MAPXFORM_TYPE);
    
    naxes[0] = map->dimenx;
    naxes[1] = map->dimeny;
    naxes[2] = 2;

    fits_create_img(fp, bitpix, naxis, naxes, &status);

    /***********************
    * write the map pixels *
    ***********************/
    npixels = naxes[0]*naxes[1];

    if(sizeof(MAPXFORM_TYPE) == 4) type = TFLOAT;
    else                           type = TDOUBLE;

    fits_write_pix(fp, type, start, npixels, map->deltax[0], &status);
    start[2]=2l;
    fits_write_pix(fp, type, start, npixels, map->deltay[0], &status);

    /****************************************************************
    * now write the WCS keywords to transform from the pixels to 
    * the coordinates
    ***************************************************************/
    { /* start of block of local variables */
    WCS* wcs;

    wcs = allocateWCS();
    invertXform2d(wcs->trans, map->to_pixels);
    writeWCS(wcs, fp,"");
    
    destroyWCS(wcs);

    } /* end of block of local variables */


} else {
    /**************************************************
    * no non-linear part, so just make an empty image *
    **************************************************/
    fits_create_img(fp, 8, 0, NULL, &status);
}

/*************************************************************
* clip the matrix elements to zero to make the file prettier *
*************************************************************/
{ /* block of local variables */
double rot00, rot01, rot10, rot11;
double xshift, yshift;

rot00 = xform->rot[0][0];
rot01 = xform->rot[0][1];
rot10 = xform->rot[1][0];
rot11 = xform->rot[1][1];

xshift = xform->xshift;
yshift = xform->yshift;

if(fabs(rot00)<NEARLY_ZERO) rot00=0.0;
if(fabs(rot01)<NEARLY_ZERO) rot01=0.0;
if(fabs(rot10)<NEARLY_ZERO) rot10=0.0;
if(fabs(rot11)<NEARLY_ZERO) rot11=0.0;

if(fabs(xshift)<NEARLY_ZERO) xshift=0.0;
if(fabs(yshift)<NEARLY_ZERO) yshift=0.0;

/******************************************
* now write the linear transform keywords *
******************************************/
fits_write_key(fp, TDOUBLE, "ROT00", &rot00, "transform matrix component", &status);
fits_write_key(fp, TDOUBLE, "ROT01", &rot01, "transform matrix component", &status);
fits_write_key(fp, TDOUBLE, "ROT10", &rot10, "transform matrix component", &status);
fits_write_key(fp, TDOUBLE, "ROT11", &rot11, "transform matrix component", &status);


fits_write_key(fp, TDOUBLE, "SHIFTX", &xshift, "translation in X", &status);
fits_write_key(fp, TDOUBLE, "SHIFTY", &yshift, "translation in Y", &status);

} /* end of block of local variables */

fits_close_file(fp, &status);

fits_report_error(stderr, status);

} /* end of writeTransformToFits function */

/******************************************************************************
*
******************************************************************************/
COMBOXFORM*  readComboXform(char* filename) {

int status=0;
fitsfile* fp;

long naxis;

COMBOXFORM* combo;

combo = allocateComboXform();

/****************
* open the file *
****************/
fits_open_file(&fp, filename, READONLY, &status);

/****************************
* read the linear transform *
****************************/
fits_read_key_dbl(fp, "ROT00", &(combo->trans->rot[0][0]) , NULL, &status);
fits_read_key_dbl(fp, "ROT01", &(combo->trans->rot[0][1]) , NULL, &status);
fits_read_key_dbl(fp, "ROT10", &(combo->trans->rot[1][0]) , NULL, &status);
fits_read_key_dbl(fp, "ROT11", &(combo->trans->rot[1][1]) , NULL, &status);

fits_read_key_dbl(fp, "SHIFTX", &(combo->trans->xshift) , NULL, &status);
fits_read_key_dbl(fp, "SHIFTY", &(combo->trans->yshift) , NULL, &status);

/**************************************************
* now see if there is a non-linear distortion map *
**************************************************/
fits_read_key_lng(fp, "NAXIS", &naxis, NULL, &status);

if(status) {
    fits_report_error(stderr, status);
    return NULL;
}

if(naxis == 0) return combo;
if(naxis != 3) return NULL;

{ /* start of block of local variables */
    int type;
    long start[] = {1l, 1l, 1l};
    long naxes[3];
    int nfound;
    long npixels;
    double null=0.0;
    int anynull;
    WCS* wcs;
    
    /********************************
    * get the dimensions of the map *
    ********************************/
    fits_read_keys_lng(fp, "NAXIS", 1, 3, naxes, &nfound, &status);
    if(naxes[2] != 2l ) return NULL;
    
    npixels = naxes[0] * naxes[1];

    /*******************
    * allocate the map *
    *******************/
    combo->map = allocateMapXform((int)naxes[0], (int)naxes[1]);


    /*****************************
    * determine the storage type *
    *****************************/
    if(sizeof(MAPXFORM_TYPE) == 4) type = TFLOAT;
    else                           type = TDOUBLE;

    fits_read_pix(fp, type, start, npixels, &null,
                  combo->map->deltax[0], &anynull, &status);

    start[2]=2l;
    fits_read_pix(fp, type, start, npixels, &null,
                  combo->map->deltay[0], &anynull, &status);
                  
    /***********************************************************
    * now read the WCS keywords to get the to_pixels transform *
    ***********************************************************/
    wcs = readWCS(fp,"");
    invertXform2d(combo->map->to_pixels, wcs->trans);
    destroyWCS(wcs);

} /* end of block of local variables */

/*******************************************
* close the fits file and check for errors *
*******************************************/
fits_close_file(fp, &status);
if(status) {
    fits_report_error(stderr, status);
    return NULL;
}
return combo;


} /* end of readComboXform function */

/**************************************************************************
*
**************************************************************************/
void applyXform2dBeforeComboXform(XFORM2D* trans, COMBOXFORM* combo) {


XFORM2D* temp;

/************************************************************
* transform the linear part
* note we don;'t do anyting to the nonlinear part, since
* by convention it is applied after the linear part
***********************************************************/
temp = allocateXform2d();
combineXform2ds(temp, trans, combo->trans);
copyXform2d(combo->trans, temp);
destroyXform2d(temp);

} /* end of applyXform2dBeforeComboXform function */

/**************************************************************************
*
**************************************************************************/
void applyXform2dAfterComboXform(COMBOXFORM* combo, XFORM2D* trans) {

XFORM2D* temp;

/****************************
* transform the linear part *
****************************/
temp = allocateXform2d();
combineXform2ds(temp, combo->trans, trans);
copyXform2d(combo->trans, temp);
destroyXform2d(temp);

/*******************************
* transform the nonlinear part *
*******************************/
if(combo->map) applyXform2dToMapXform(combo->map, trans);


} /* end of applyXform2dAfterComboXform function */

/**************************************************************************
* return a new COMBOXFORM which is the inverse of this one.
* Note that if there is a non-linear part to the transform, the
* inverse is not reversable. i.e. two inverses do not give the original
* transform.
**************************************************************************/
COMBOXFORM* invertComboXform(COMBOXFORM* combo) {

COMBOXFORM* inverse;

inverse = allocateComboXform();

/****************************
* first invert the linear part *
*******************************/
invertXform2d(inverse->trans, combo->trans);

if(combo->map) {
    /*************************************
    * need to invert the non-linear part *
    *************************************/
    inverse->map = allocateMapXformWithSameAxes(combo->map);
    invertMapXform(inverse->map, combo->map);

    /******************************************************************
    * and then swap the order of the non-linear and linear transforms *
    ******************************************************************/
    applyXform2dToMapXform(inverse->map, inverse->trans );
    
}

return inverse;


} /* end of invertComboXform function */
