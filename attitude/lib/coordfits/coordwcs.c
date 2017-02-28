#include "coordwcs.h"

#include <math.h>
#include <string.h>

#define  WCS_MATRIX_TOLERANCE 1e-10

/****************************************************************************
* create a set of WCS keywords with default values. The defaults are
* an identity transform with the reference pixel at (0,0).
****************************************************************************/
WCS* allocateWCS() {

WCS* wcs;

wcs = (WCS*)malloc(sizeof(WCS));
if(wcs==NULL) return NULL;

wcs->trans = allocateXform2d();
if(wcs->trans==NULL) return NULL;

setXform2dToTranslation(wcs->trans, 0.0, 0.0);
wcs->xref=0.0;
wcs->yref=0.0;

wcs->xunit[0]='\0';
wcs->yunit[0]='\0';

wcs->xtype[0]='\0';
wcs->ytype[0]='\0';

return wcs;

} /* end of allocateWCS function */

/**************************************************************************
*
**************************************************************************/
void destroyWCS(WCS* wcs) {

    destroyXform2d(wcs->trans);
    free(wcs);
}

/**************************************************************************
* Create a new WCS structure which describes the world coordinates for
* a given set of coordinates. The center_x, center_y arguments give the
* world coordinate at the center of the coordinate space. Note that this function
* does not necessarily give the WCS keywords which would be appropriate 
* for an image, since an image is translated to put the first pixel at (1,1)
**************************************************************************/
WCS* createWCSForCoordDef(COORDDEF* coord, double center_x, double center_y ) {

WCS* wcs;

XFORM2D* translate;
XFORM2D* scale;
double scale_x, scale_y;


wcs = allocateWCS();

wcs->xref = coord->center_x;
wcs->yref = coord->center_y;

/***********************************************
* first translate the reference pixel to (0,0) *
***********************************************/
translate = allocateXform2d();
setXform2dToTranslation(translate, -coord->center_x, -coord->center_y);

/*************
* then scale *
*************/
scale_x = coord->scale_x;
scale_y = coord->scale_y;

if(!strcmp(coord->name, "SKY") ) {
    /******************************************
    * by convention, RA increases to the left *
    ******************************************/
    scale_x = -scale_x;
}


scale = allocateXform2d();
setXform2dToScaling(scale, scale_x, scale_y, 0.0, 0.0);

combineXform2ds(wcs->trans, translate, scale);

/******************************************************************
* then translate so the reference value is at the reference pixel *
******************************************************************/
applyTranslationToXform2d(wcs->trans, center_x, center_y);

/****************
* set the units *
****************/
strncpy(wcs->xunit, coord->scale_unit, FLEN_VALUE);
strncpy(wcs->yunit, coord->scale_unit, FLEN_VALUE);

/**************************
* set the coordinate types *
***************************/
if(!strcmp(coord->name, "SKY") ) {
    /******************************
    * sky coordinates are special *
    ******************************/
    strcpy(wcs->xtype, "RA---TAN");
    strcpy(wcs->ytype, "DEC--TAN");

} else {
    /*************************************
    * regular coordinates get their name *
    *************************************/
    strncpy(wcs->xtype, coord->name_x, FLEN_VALUE);
    strncpy(wcs->ytype, coord->name_y, FLEN_VALUE);
}


return wcs;


} /* of createWCSForCoordDef function */

/******************************************************************************
* Modify the WCS in order to account for a transform applied to the pixels.
* This transforms the reference point as well as the transform. The units
* and coordinate types remain unchanged
******************************************************************************/
void transformWCSPixels(WCS* wcs, XFORM2D* trans) {

double xref, yref;
XFORM2D* inverse;
XFORM2D* combined;

/***************************************************************
* transform the reference point into the new pixel coordinates *
***************************************************************/
applyXform2dToContinuousCoords(trans, &xref, &yref, wcs->xref, wcs->yref);
wcs->xref = xref;
wcs->yref = yref;

/*************************************************************************
* put the inverse transform before the WCS transform
* The idea is to transform the new coordinates back to the old and then
* apply the original WCS transform
*************************************************************************/
inverse = allocateXform2d();
invertXform2d(inverse, trans);

combined = allocateXform2d();
combineXform2ds(combined, inverse, wcs->trans);

copyXform2d(wcs->trans, combined);

destroyXform2d(inverse);
destroyXform2d(combined);


} /* end of transformWCSPixels function */

/******************************************************************************
* Read a two-dimensional set of WCS keywords. This method can read
* either the PC or CD matrix conventions, or the plain old
* CDELT. This function ignores the CROTA keywords.
******************************************************************************/
WCS* readWCS(fitsfile* fp, char* suffix) {

WCS* wcs;
int status=0;

double crpix1;
double crpix2;
double crval1;
double crval2;

char* template[] = {"CD1_1", "CD1_2", "CD2_1", "CD2_2"};
int ntemplate=4;
char card[FLEN_CARD];

char key[FLEN_KEYWORD];

/**********************************************************
* make sure the suffix is no more than one character long *
**********************************************************/
if(suffix[0] != '\0' && suffix[1] != '\0' ) suffix[1] = '\0';


/*******************************************
* allocate the WCS structure and save the
* reference pixel values
*******************************************/
wcs = allocateWCS();
if(wcs==NULL) return NULL;

/*************************
* read the string values *
*************************/
sprintf(key, "CTYPE1%s", suffix);
fits_read_key_str(fp, key, wcs->xtype, NULL, &status);
if(status == KEY_NO_EXIST) {
    status=0;
    wcs->xtype[0] = '\0';
}

sprintf(key, "CTYPE2%s", suffix);
fits_read_key_str(fp, key, wcs->ytype, NULL, &status);
if(status == KEY_NO_EXIST) {
    status=0;
    wcs->ytype[0] = '\0';
}

sprintf(key, "CUNIT1%s", suffix);
fits_read_key_str(fp, key, wcs->xunit, NULL, &status);
if(status == KEY_NO_EXIST) {
    status=0;
    wcs->xunit[0] = '\0';
}

sprintf(key, "CUNIT2%s", suffix);
fits_read_key_str(fp, key, wcs->yunit, NULL, &status);
if(status == KEY_NO_EXIST) {
    status=0;
    wcs->yunit[0] = '\0';
}

/************************************************************
* all the method use CRPIX and CRVAL, so lets read them now *
************************************************************/
sprintf(key, "CRPIX1%s", suffix);
fits_read_key_dbl(fp, key, &crpix1, NULL, &status);
if(status == KEY_NO_EXIST) {
    status=0;
    crpix1=0.0;
}

sprintf(key, "CRPIX2%s", suffix);
fits_read_key_dbl(fp, key, &crpix2, NULL, &status);
if(status == KEY_NO_EXIST) {
    status=0;
    crpix2=0.0;
}

sprintf(key, "CRVAL1%s", suffix);
fits_read_key_dbl(fp, key, &crval1, NULL, &status);
if(status == KEY_NO_EXIST) {
    status=0;
    crval1=0.0;
}

sprintf(key, "CRVAL2%s", suffix);
fits_read_key_dbl(fp, key, &crval2, NULL, &status);
if(status == KEY_NO_EXIST) {
    status=0;
    crval2=0.0;
}

wcs->xref = crpix1;
wcs->yref = crpix2;

/******************************************************
* see if the header has any of the CD matrix keywords *
******************************************************/
fits_read_record(fp, 0, card, &status);
fits_find_nextkey(fp, template, ntemplate, NULL, 0, card, &status);
if(status==KEY_NO_EXIST) {
    /*******************************************************
    * no CD keywords, so assume PC keywords for the matrix *
    *******************************************************/
    double pc11;
    double pc12;
    double pc21;
    double pc22;
    double cdelt1;
    double cdelt2;

    status=0;

    sprintf(key, "PC1_1%s", suffix);
    fits_read_key_dbl(fp, key, &pc11, NULL, &status);
    if(status == KEY_NO_EXIST) {
        status=0;
        pc11=1.0;
    }

    sprintf(key, "PC1_2%s", suffix);
    fits_read_key_dbl(fp, key, &pc12, NULL, &status);
    if(status == KEY_NO_EXIST) {
        status=0;
        pc12=0.0;
    }

    sprintf(key, "PC2_1%s", suffix);
    fits_read_key_dbl(fp, key, &pc21, NULL, &status);
    if(status == KEY_NO_EXIST) {
        status=0;
        pc21=0.0;
    }

    sprintf(key, "PC2_2%s", suffix);
    fits_read_key_dbl(fp, key, &pc22, NULL, &status);
    if(status == KEY_NO_EXIST) {
        status=0;
        pc22=1.0;
    }

    /**************************************
    * this method uses the CDELT keywords *
    **************************************/
    sprintf(key, "CDELT1%s", suffix);
    fits_read_key_dbl(fp, key, &cdelt1, NULL, &status);
    if(status == KEY_NO_EXIST) {
        status=0;
        cdelt1=1.0;
    }

    sprintf(key, "CDELT2%s", suffix);
    fits_read_key_dbl(fp, key, &cdelt2, NULL, &status);
    if(status == KEY_NO_EXIST) {
        status=0;
        cdelt2=1.0;
    }


    /****************************
    * fill in the matrix values *
    ****************************/
    wcs->trans->rot[0][0] = pc11*cdelt1;
    wcs->trans->rot[0][1] = pc12*cdelt1;
    wcs->trans->rot[1][0] = pc21*cdelt2;
    wcs->trans->rot[1][1] = pc22*cdelt2;

    /*************************
    * now fill in the shifts *
    *************************/
    wcs->trans->xshift=crval1 - cdelt1*pc11*crpix1 - cdelt1*pc12*crpix2;
    wcs->trans->yshift=crval2 - cdelt2*pc21*crpix1 - cdelt2*pc22*crpix2;

} else if(status == 0) {
    /**************************************************
    * there are CD matrix keywords, so lets read them *
    **************************************************/
    double cd11;
    double cd12;
    double cd21;
    double cd22;

    sprintf(key, "CD1_1%s", suffix);
    fits_read_key_dbl(fp, key, &cd11, NULL, &status);
    if(status == KEY_NO_EXIST) {
        status=0;
        cd11=0.0;
    }

    sprintf(key, "CD1_2%s", suffix);
    fits_read_key_dbl(fp, key, &cd12, NULL, &status);
    if(status == KEY_NO_EXIST) {
        status=0;
        cd12=0.0;
    }

    sprintf(key, "CD2_1%s", suffix);
    fits_read_key_dbl(fp, key, &cd21, NULL, &status);
    if(status == KEY_NO_EXIST) {
        status=0;
        cd21=0.0;
    }

    sprintf(key, "CD2_2%s", suffix);
    fits_read_key_dbl(fp, key, &cd22, NULL, &status);
    if(status == KEY_NO_EXIST) {
        status=0;
        cd22=0.0;
    }
    
    /****************************
    * fill in the matrix values *
    ****************************/
    wcs->trans->rot[0][0] = cd11;
    wcs->trans->rot[0][1] = cd12;
    wcs->trans->rot[1][0] = cd21;
    wcs->trans->rot[1][1] = cd22;

    /*************************
    * now fill in the shifts *
    *************************/
    wcs->trans->xshift=crval1 - cd11*crpix1 - cd12*crpix2;
    wcs->trans->xshift=crval2 - cd21*crpix1 - cd22*crpix2;


}

/******************************
* final check for FITS errors *
******************************/
if(status) {
    fits_report_error(stderr, status);
    return NULL;
}


return wcs;


} /* end of readWCS function */

/************************************************************************
* Write a set of WCS keywords. This function uses the PC matrix convention.
* However, it will not write a matrix value if it is more than
* WCS_MATRIX_TOLERANCE different from the default identity matrix.
************************************************************************/
int writeWCS(WCS* wcs, fitsfile* fp, char* suffix) {

int status=0;

double cdelt1;
double cdelt2;

double pc11;
double pc12;
double pc21;
double pc22;

double crval1;
double crval2;

char key[FLEN_KEYWORD];
double (*rm)[2];
double det;


/**********************************************************
* make sure the suffix is no more than one character long *
**********************************************************/
if(suffix[0] != '\0' && suffix[1] != '\0' ) suffix[1] = '\0';

/***************************************************************
* pull a reasonable scaling factor out of the transform matrix *
***************************************************************/
rm = &wcs->trans->rot[0];

det = rm[0][0] * rm[1][1] - rm[0][1] * rm[1][0];
cdelt1 = sqrt(fabs(det));
cdelt2 = cdelt1;

if (!strcmp(wcs->xtype, "RA---TAN"))
   cdelt1 *= -1;

pc11 = rm[0][0] / cdelt1;
pc12 = rm[0][1] / cdelt1;
pc21 = rm[1][0] / cdelt2;
pc22 = rm[1][1] / cdelt2;


/*********************************************
* determine the value at the reference pixel *
*********************************************/
applyXform2dToContinuousCoords(wcs->trans, &crval1, &crval2,
                               wcs->xref, wcs->yref);

if(fabs(crval1)<WCS_MATRIX_TOLERANCE) crval1=0.0;
if(fabs(crval2)<WCS_MATRIX_TOLERANCE) crval2=0.0;

/**************************************
* now we can write the basic keywords *
**************************************/
sprintf(key,"CRPIX1%s", suffix);
fits_update_key(fp, TDOUBLE, key, &(wcs->xref), NULL, &status);

sprintf(key,"CRPIX2%s", suffix);
fits_update_key(fp, TDOUBLE, key, &(wcs->yref), NULL, &status);

sprintf(key,"CRVAL1%s", suffix);
fits_update_key(fp, TDOUBLE, key, &crval1, NULL, &status);

sprintf(key,"CRVAL2%s", suffix);
fits_update_key(fp, TDOUBLE, key, &crval2, NULL, &status);

sprintf(key,"CDELT1%s", suffix);
fits_update_key(fp, TDOUBLE, key, &cdelt1, NULL, &status);

sprintf(key,"CDELT2%s", suffix);
fits_update_key(fp, TDOUBLE, key, &cdelt2, NULL, &status);

/************************************************************
* now we check the matrix values. If they are above the
* threshold, we will write them, otherwise not
************************************************************/
if(fabs(pc11-1) > WCS_MATRIX_TOLERANCE) {
    sprintf(key,"PC1_1%s", suffix);
    fits_update_key(fp, TDOUBLE, key, &pc11, NULL, &status);
}

if(fabs(pc12) > WCS_MATRIX_TOLERANCE) {
    sprintf(key,"PC1_2%s", suffix);
    fits_update_key(fp, TDOUBLE, key, &pc12, NULL, &status);
}

if(fabs(pc21) > WCS_MATRIX_TOLERANCE) {
    sprintf(key,"PC2_1%s", suffix);
    fits_update_key(fp, TDOUBLE, key, &pc21, NULL, &status);
}

if(fabs(pc22-1) > WCS_MATRIX_TOLERANCE) {
    sprintf(key,"PC2_2%s", suffix);
    fits_update_key(fp, TDOUBLE, key, &pc22, NULL, &status);
}

if(status) {
    fprintf(stderr, "Error writing WCS keywords\n");
    fits_report_error(stderr, status);
}

/********************************************
* finally write the strings if they are set *
********************************************/
if(wcs->xtype[0] != '\0' ) {
    sprintf(key,"CTYPE1%s", suffix);
    fits_update_key_str(fp, key, wcs->xtype, "X coordinate type", &status);
}

if(wcs->ytype[0] != '\0' ) {
    sprintf(key,"CTYPE2%s", suffix);
    fits_update_key_str(fp, key, wcs->ytype, "Y coordinate type", &status);
}

if(wcs->xunit[0] != '\0' ) {
    sprintf(key,"CUNIT1%s", suffix);
    fits_update_key_str(fp, key, wcs->xunit, "X coordinate units", &status);
}

if(wcs->yunit[0] != '\0' ) {
    sprintf(key,"CUNIT2%s", suffix);
    fits_update_key_str(fp, key, wcs->yunit, "Y coordinate units", &status);
}

return status;

} /* end of writeWCS function */
