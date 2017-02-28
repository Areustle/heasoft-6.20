#include <math.h>
#include <string.h>
#include <fitsio.h>
#include <longnam.h>

#include "teldef.h"

/*
#define DEBUG
*/

/***********************************************************************
*************************************************************************
* set the raw COORDDEFs assuming that each segment contains only a single 
* pixel given a table of pixel corner values. This is how XRS teldef files
* work. The fits file pointer must point to the extension containing 
* a table with one row per pixel. There must be at least three columns,
* one giving the segment (pixel) number (counting from zero).
* and two giving the x and y corner positions as four element vectors.
* Note teldef->det[0]->scale_x and teldef->det[0]->scale_y must be defined
* before calling this function.
* Although this function searches the file for other HDUs it returns to the
* original HDU before exiting.
*
* returns 1 if the raw coordinates were set by this method and 
* returns 0 if there is no PIXEL_MAP extension
************************************************************************/
int setSinglePixelRawCoordsInTelDef(TELDEF* teldef, fitsfile* fp) {

int status=0;
int anynul;
long naxis2;
long row;

int original_hdu;
int hdutype;

int seg_col,x_col,y_col;
int seg;
double xcorner[4];
double ycorner[4];

XFORM2D* pre;
XFORM2D* middle;
XFORM2D* post;
XFORM2D* dummy;

COORDDEF* raw0;
COORDDEF* raw;
COORDDEF* det;

char name_seg[FLEN_VALUE];

#ifdef DEBUG
printf("setSinglePixelRawCoordsInTelDef: start\n");
#endif

/*************************************************************
* try to move to a PIXEL_MAP extension
* If there is no such extension, return having done nothing
* remember the original_hdu before the move.
*************************************************************/
fits_get_hdu_num(fp,&original_hdu);

fits_movnam_hdu(fp,ANY_HDU,"PIXEL_MAP",0/* any version*/,&status);
if(status==BAD_HDU_NUM) return(0);

checkTelDefFITSerrors(status,"finding PIXEL_MAP extension in",teldef);

/*******************************************************
* allocate a dummy XFORM2D to hold intermediate values *
*******************************************************/
dummy =allocateXform2d();
middle=allocateXform2d();


/*****************************************************************************
* read the number of table rows and set the number of segments to this value
* Note that for now we assume the first segment is zero. This should
* modified to check the actual lowest pixel number.
*****************************************************************************/
fits_read_key_lng(fp,"NAXIS2",&naxis2,NULL,&status);
checkTelDefFITSerrors(status,"reading NAXIS2 from",teldef);
setNsegmentsInTelDef(teldef, 0, (int)naxis2);


/**************************************************************************
* OK, now that we have allocated the raw COORDDEF structures, we go back
* to original HDU for a minute to read some address space and 
* transformation info. Note that as far as this info is concerned
* all of the segments are the same, so we read the first one
* from the teldef file and then just copy the rest.
**************************************************************************/
fits_movabs_hdu(fp,original_hdu,&hdutype,&status);
checkTelDefFITSerrors(status,"going back to previous HDU in",teldef);

raw0=teldef->raw[teldef->min_segment];
setStringInCoordDef(&(raw0->name),"RAW");
setCoordinatesFromKeywordsInTeldef(teldef,raw0, fp );

for(seg=teldef->min_segment+1;seg<teldef->nsegments;++seg) {
    raw=teldef->raw[seg];

    setCoordDefLimits(raw,raw0->first_pixel_x,raw0->npixels_x,
                          raw0->first_pixel_y,raw0->npixels_y  );

    raw->scale_x=raw0->scale_x;
    raw->scale_y=raw0->scale_y;
    setStringInCoordDef(&(raw->scale_unit),raw0->scale_unit);

    setStringInCoordDef(&(raw->name  ),raw0->name  );
    setStringInCoordDef(&(raw->name_x),raw0->name_x);
    setStringInCoordDef(&(raw->name_y),raw0->name_y);

}





/**************************************************************************
* set up a translatiuon from the raw address space specified in the
* original HDU to coordinates with a single pixel whose center
* is at (.5, .5). This is because setXform2dFromCornerPixels returns
* a transform from the latter coordinates.
**************************************************************************/
pre=allocateXform2d();
setXform2dToScaling(pre,1./(double)raw0->npixels_x, 
                        1./(double)raw0->npixels_y, 
                    raw0->center_x, raw0->center_y );

applyTranslationToXform2d(pre, 0.5 - raw0->center_x, 
                               0.5 - raw0->center_x );

#ifdef DEBUG
printf("pre:\n");
printXform2d(pre,stdout);
#endif

/***********************************************************************
* and then read the transformation from the coordinate system
* of the pixel corner measurements
* to the detector coordinate system - also from the original HDU
***********************************************************************/
det=teldef->det[0];
post=allocateXform2d();
setXform2dFromTelDefKeywords(teldef,post,fp, raw0, det);

#ifdef DEBUG
printf("post:\n");
printXform2d(post,stdout);
#endif


/*******************************
* read the segment column name *
*******************************/
fits_read_key_str(fp,"SEG_COL",name_seg,NULL,&status);
if(status==KEY_NO_EXIST && teldef->nsegments==1) {
    status=0;
    strcpy(name_seg,"");
}
checkTelDefFITSerrors(status,"read SEG_COL from",teldef);

teldef->seg_col_name=(char*)malloc(sizeof(char)*(strlen(name_seg)+1));
strcpy(teldef->seg_col_name,name_seg);



/****************************************************************************
* now go back to the PIXEL_MAP HDU to read the pixel corners from the table *
****************************************************************************/
fits_movnam_hdu(fp,ANY_HDU,"PIXEL_MAP",0/* any version*/,&status);
checkTelDefFITSerrors(status,"returning to PIXEL_MAP extension in",teldef);


/*************************
* find the named columns *
*************************/
fits_get_colnum(fp,CASESEN,name_seg,&seg_col,&status);
fits_get_colnum(fp,CASESEN,"PIXELX",&x_col  ,&status);
fits_get_colnum(fp,CASESEN,"PIXELY",&y_col  ,&status);

checkTelDefFITSerrors(status,"finding columns in",teldef);


/*********************************************************************
* loop over the rows in the table, filling in the COORDDEF values
* for each segment
*********************************************************************/
{double sumx=0.,sumy=0;
for(row=1;row<=naxis2;++row) {


    /************************
    * read values from file *
    ************************/
    fits_read_col_int(fp,seg_col,row,1l,1l, 0  , &seg,    &anynul,&status);
    fits_read_col_dbl(fp,  x_col,row,1l,4l, 0.0, xcorner, &anynul,&status);
    fits_read_col_dbl(fp,  y_col,row,1l,4l, 0.0, ycorner, &anynul,&status);

    sumx+=xcorner[0]+xcorner[1]+xcorner[2]+xcorner[3];
    sumy+=ycorner[0]+ycorner[1]+ycorner[2]+ycorner[3];

    /*********************************************
    * fill in address space and column name info *
    *********************************************/
    raw=teldef->raw[seg];


    /*************************
    * set best fit transform *
    *************************/
    setXform2dFromCornerPixels(middle,xcorner,ycorner);
    combineXform2ds(dummy,pre,middle);

    combineXform2ds(raw->trans,dummy,post);
    setCoordDefTransformInfo(raw);

    #ifdef DEBUG
    printf("setSinglePixelRawCoordsInTelDef: row=%ld seg=%d transform:\n",
           row,seg);
    printXform2d(raw->trans,stdout);
    #endif


} /* end of loop over table rows */


#ifdef DEBUG
printf("meanx=%g meany=%g\n",.25*sumx/naxis2,.25*sumy/naxis2);
printf("detcenter=%g detcentery=%g\n",det->center_x*.024, -det->center_y*.024);
#endif

} /* end of scope of local variables sumx and sumy */

/*******************
* check for errors *
*******************/
checkTelDefFITSerrors(status,"reading columns from",teldef);


/********************************
* free the allocated structures *
********************************/
destroyXform2d(pre);
destroyXform2d(middle);
destroyXform2d(post);
destroyXform2d(dummy);

/**************************************************************
* return to the original HDU before we came to this extension *
**************************************************************/
fits_movabs_hdu(fp,original_hdu,&hdutype,&status);
checkTelDefFITSerrors(status,"returning to previous HDU in",teldef);

return(1);


} /* end of setSinglePixelRawCoordsInTelDef function */

/****************************************************************************
*****************************************************************************
* read the raw coordinate information from "COE_[X/Y]n_[A/B/C]" keywords in the 
* primary header. This is the way things are done for the ASCA SIS and GIS.
* Note that this method is limited to 10 segments at most, otherwise the
* "COE" keywords would be too long. An alternative format could
* lift this restriction if needed, but large numbers of COE keywords
* can be cumbersome in a file, so perhaps it is best to define a new
* table-based format for such cases.
* 
* This format works in the following way:
* a set of COE_[X/Y]n_[A/B/C] keywords define the transformation from
* raw coordinates (for segment "n") to an intermediate coordinate system.
* Then the keywords
* DET_ROTD
* RAWFLIPX
* RAWFLIPY
* define the translation from the internal cooridnate system to the bottom
* level detector coordinates.
* Note that these three keywords are optional with defaults
* DET_ROTD=0.
* RAWFLIPX=1.
* RAWFLIPY=-1.
* Note also that the intermediate coordinates will not be identical to the
* DET coordinate if all the defaults are used. This requires RAWFLIPY=1.
****************************************************************************/
int setRawFromCoefInTelDef(TELDEF* teldef, fitsfile* fp) {

int status=0;
char key[FLEN_KEYWORD];
double value;

int seg;
int min_segment=0;
int nsegments=-1; /* is initialized below, but compiler doesn't recognize it */
int include_seg;


char name_seg[FLEN_VALUE];

COORDDEF* det;
COORDDEF* raw0;
COORDDEF* raw;

XFORM2D* raw2int;
XFORM2D* int2det;

#ifdef DEBUG
printf("setRawFromCoefInTelDef: start\n");
#endif


/*******************************************************************
* get the bottom level coordinate system from the teldef structure *
*******************************************************************/
det=teldef->det[0];

/**************************************************************************
* determine the number of segments by counting the COE keywords
* There are two possible formats for these keywords -
* COE_[XY]_[ABC] or COE_[XY]n_[ABC], where n is the segment number.
* The first case is used only if nsegments=1
* We first check for keywords without segment numbers. If one
* exists, then we don't bother to check if there are numbered keywords.
*************************************************************************/

/*****************************************************
* try reading the COE keyword without the seg number *
*****************************************************/
fits_read_key_dbl(fp,"COE_X_A",&value,NULL,&status);
if(status!=KEY_NO_EXIST) {
    /*************************************************
    * found it, so the is only one segment.
    * flag that we will be handling things this way
    *************************************************/
    include_seg=0;
    nsegments=1;

} else {
    /*****************************************************
    * didn't find the keyword, so we will go on to look
    * for the kind with segment numbers 
    *****************************************************/
    status=0;
    include_seg=1;
}

checkTelDefFITSerrors(status,"reading COE_X_A from",teldef);

if(include_seg) {
    /************************************************
    * didn't find a keyword without segment numbers,
    * so now count the keywords with segment numbers
    * Note we limit seg<10 since otherwise  
    * the keyword name would be too long
    *************************************************/
    status=KEY_NO_EXIST;
    for(seg=0;status==KEY_NO_EXIST && seg<10;++seg) {

        status=0;
        sprintf(key,"COE_X%d_A",seg);
        fits_read_key_dbl(fp,key,&value,NULL,&status);
        min_segment=seg;
    }



    nsegments=0;
    for(seg=min_segment; !status && seg<10;++seg) {

        sprintf(key,"COE_X%d_A",seg);
        fits_read_key_dbl(fp,key,&value,NULL,&status);
	#ifdef DEBUG
	printf("just read %s status=%d nsegments=%d\n", key, status, nsegments);
	#endif
        ++nsegments;
    }

    /*******************
    * check for errors *
    *******************/
    if(status==KEY_NO_EXIST) status=0;
    checkTelDefFITSerrors(status,"reading detector keywords from",teldef);

    /*********************************************************
    * there is one extra iteration when status==KEY_NO_EXIST *
    *********************************************************/
    --nsegments;

} /* end if we need to include segment numbers in COE keyword names */




/**************************************
* check if this method is appropriate *
**************************************/
if(nsegments==0) {
    /**********************************************************
    * there are no COE keywords so return having done nothing
    **********************************************************/
    return(0);
}

/**************************************************************
* Set the number of segments
**************************************************************/
setNsegmentsInTelDef(teldef, min_segment, nsegments);

/*********************************************************************
* set the raw coordinate address space, scaling, and column names
* note that this info is the same for all segments, so we read
* from the teldef file for the first segment and then just copy this
* info to all the other segments
*********************************************************************/
raw0=teldef->raw[teldef->min_segment];
setStringInCoordDef(&(raw0->name),"RAW");
setCoordinatesFromKeywordsInTeldef(teldef,raw0, fp );

for(seg=teldef->min_segment+1; seg<teldef->nsegments; ++seg) {
    raw=teldef->raw[seg];

    setCoordDefLimits(raw,raw0->first_pixel_x,raw0->npixels_x,
                          raw0->first_pixel_y,raw0->npixels_y  );

    raw->scale_x=raw0->scale_x;
    raw->scale_y=raw0->scale_y;
    setStringInCoordDef(&(raw->scale_unit),raw0->scale_unit);

    setStringInCoordDef(&(raw->name  ),raw0->name  );
    setStringInCoordDef(&(raw->name_x),raw0->name_x);
    setStringInCoordDef(&(raw->name_y),raw0->name_y);

} /* end of loop over segments */


/************************************************************************
* the raw to detector transform is a combination of the 
* alignment transforms given by the COE_[X/Y]n_[A/B/C] keywords.
* followed by a transformation specified by the bottom level detector
* coordinate transformation keywords
* we read this second transformation now.
*************************************************************************/
det=teldef->det[0];
int2det=allocateXform2d();
setXform2dFromTelDefKeywords(teldef, int2det, fp, raw0, det );

#ifdef DEBUG
printf("setRawFromCoefInTelDef: int2det:\n");
printXform2d(int2det,stdout);
printf("nsegments=%d\n", teldef->nsegments);
#endif



/********************************
* determine segment column name *
********************************/
if(nsegments>1) {
    fits_read_key_str(fp,"SEG_COL",name_seg,NULL,&status);

    if(status==KEY_NO_EXIST && !strcmp(teldef->mission,"ASCA") &&  
       (!strcmp(teldef->instrument,"SIS0") ||
        !strcmp(teldef->instrument,"SIS1"))   ){
        /**************************************************************
        * this keyword is left out of the original ASCA teldef files
        * so we have to supply a special default
        **************************************************************/
        status=0;
        strcpy(name_seg,"CCDID");
    }

    checkTelDefFITSerrors(status,"reading SEG_COL from",teldef);


} else {
    /******************************************************************
    * there is no segment column name since there is only one segment *
    ******************************************************************/
    strcpy(name_seg,"");
}

teldef->seg_col_name=(char*)malloc(sizeof(char)*(strlen(name_seg)+1) );
strcpy(teldef->seg_col_name,name_seg);



 
/*************************************************************************
* allocate a structure to hold the transforms read from the COE keywords *
*************************************************************************/
raw2int=allocateXform2d();

/*******************************************
* determine the transform for each segment *
*******************************************/
for(seg=teldef->min_segment; seg<teldef->nsegments; ++seg) {

    /*********************************************
    * set address space limits and column names *
    *********************************************/
    raw=teldef->raw[seg];

    /*******************
    * "X" coefficients *
    *******************/
    if(include_seg) sprintf(key,"COE_X%d_A",seg);
    else            sprintf(key,"COE_X_A"      );
    fits_read_key_dbl(fp,key,&(raw2int->xshift   ),NULL,&status);

    #ifdef DEBUG
    printf("key=|%s| value=%g status=%d\n",key,raw2int->xshift,status);
    #endif

    if(include_seg) sprintf(key,"COE_X%d_B",seg);
    else            sprintf(key,"COE_X_B"      );
    fits_read_key_dbl(fp,key,&(raw2int->rot[0][0]),NULL,&status);

    if(include_seg) sprintf(key,"COE_X%d_C",seg);
    else            sprintf(key,"COE_X_C"      );
    fits_read_key_dbl(fp,key,&(raw2int->rot[0][1]),NULL,&status);

    /*******************
    * "Y" coefficients *
    *******************/
    if(include_seg) sprintf(key,"COE_Y%d_A",seg);
    else            sprintf(key,"COE_Y_A"      );
    fits_read_key_dbl(fp,key,&(raw2int->yshift   ),NULL,&status);

    if(include_seg) sprintf(key,"COE_Y%d_B",seg);
    else            sprintf(key,"COE_Y_B"      );
    fits_read_key_dbl(fp,key,&(raw2int->rot[1][0]),NULL,&status);

    if(include_seg) sprintf(key,"COE_Y%d_C",seg);
    else            sprintf(key,"COE_Y_C"      );
    fits_read_key_dbl(fp,key,&(raw2int->rot[1][1]),NULL,&status);

    #ifdef DEBUG
    printf("setRawFromCoefInTelDef: seg=%d\n",seg);
    printf("setRawFromCoefInTelDef: raw2int:\n");
    printXform2d(raw2int,stdout);
    #endif



    /*********************************************
    * check for errors in any of the above reads *
    *********************************************/
    checkTelDefFITSerrors(status,"reading raw to det coefficients from",teldef);

    /***************************************
    * combine transforms to get RAW -> DET *
    ***************************************/
    combineXform2ds(raw->trans,raw2int,int2det);
    setCoordDefTransformInfo(raw);

    #ifdef DEBUG
    printf("setRawFromCoefInTelDef: seg=%d\n",seg);
    printf("setRawFromCoefInTelDef: raw->trans @ %d:\n",(int)(raw->trans));
    printXform2d(raw->trans,stdout);
    #endif


} /* end of loop over segments */

/*************************
* free temporary storage *
*************************/
destroyXform2d(raw2int);
destroyXform2d(int2det);

return(1);

} /* end of setRawFromCoefInTelDef function */

/****************************************************************************
*****************************************************************************
* This is a variant of setRawFromCoefInTelDef which handles some
* unique quirks in ASCA GIS teldef files.
****************************************************************************/
int setRawForASCAGISTelDef(TELDEF* teldef, fitsfile* fp) {

int status=0;

char key[FLEN_KEYWORD];

COORDDEF* raw;
COORDDEF* det;

char pos_det[FLEN_VALUE];
int pos_det_code;

double angle;

XFORM2D* half_pixel_shift;
XFORM2D* coef;
XFORM2D* raw2int;
XFORM2D* rotation;
XFORM2D* scaling;
XFORM2D* int2det;

#ifdef DEBUG
printf("setRawForASCAGISTelDef: start\n");
#endif

/*******************************
* this only works for ASCA GIS *
*******************************/
if( !(!strncmp(teldef->mission,"ASCA",FLEN_VALUE) &&
      (!strcmp(teldef->instrument,"GIS2") || 
       !strcmp(teldef->instrument,"GIS3")   )        ) ) {
    /********************************
    * not ASCA GIS so just quit now *
    ********************************/
    return(0);
}

#ifdef DEBUG
printf("setRawForASCAGISTelDef: this is GIS\n");
#endif


/***************************
* GIS has only one segment *
***************************/
setNsegmentsInTelDef(teldef,0,1);
raw=teldef->raw[0];
det=teldef->det[0];

/****************************
* blank segment column name *
****************************/
teldef->seg_col_name=(char*)malloc(sizeof(char));
strcpy(teldef->seg_col_name,"");


/**********************************************************
* GIS is 256x256 pixels in standard resolution with the
* first pixel at (0,0)
*********************************************************/
setCoordDefLimits(raw,0.,256,0.,256);

raw->scale_x=1.;
raw->scale_y=1.;
setStringInCoordDef(&(raw->scale_unit),"pixels");

setStringInCoordDef(&(raw->name_x),"RAWX");
setStringInCoordDef(&(raw->name_y),"RAWY");


/*******************************************************************************
* The GIS has two commandable poaition determination methods: "FLF" and "POW2".
* Each method has its own teldef file, with the method specified in the
* "POS_DET" keyword. However, the coeficients for *both* methods are in all 
* teldef files (this is what ascalin expects). The coeficients for the POW2
* method are given with the "seg=1" coeficients and the FLF coeficients
* are given in the "seg=2" coeficients.
*******************************************************************************/
fits_read_key_str(fp,"POS_DET",pos_det,NULL,&status);
checkTelDefFITSerrors(status,"reading POS_DET from",teldef);

pos_det_code=0;
if(!strncmp(pos_det,"POW2",FLEN_VALUE)) pos_det_code=1;
if(!strncmp(pos_det,"FLF" ,FLEN_VALUE)) pos_det_code=2;

if(pos_det_code==0) {
    fprintf(stderr,"Unknown POS_DET value %s in %s\n",pos_det,teldef->filename);
    exit(1);
}

#ifdef DEBUG
printf("setRawForASCAGISTelDef: pos_det=%s pos_det_code=%d\n",
       pos_det,pos_det_code );
#endif

/***************************************************************************
* read the raw to detector coordinate transform. This is nearly the same as
* setRawFromCoefInTelDef except that the COE keywords give det
* coordinates in milimeters with the origin at the DET coordinate center.
* also we first need to arbitrarily shift the raw coordinates by a half a pixel * to mimic what ascalin did. Ascalin added ransom numbers raw+random
* instead of raw+random-.5
***************************************************************************/
half_pixel_shift=allocateXform2d();
setXform2dToTranslation(half_pixel_shift, 0.5, 0.5);

coef=allocateXform2d();


/*******************
* "X" coefficients *
*******************/
sprintf(key,"COE_X%d_A",pos_det_code);
fits_read_key_dbl(fp,key,&(coef->xshift   ),NULL,&status);

sprintf(key,"COE_X%d_B",pos_det_code);
fits_read_key_dbl(fp,key,&(coef->rot[0][0]),NULL,&status);

sprintf(key,"COE_X%d_C",pos_det_code);
fits_read_key_dbl(fp,key,&(coef->rot[0][1]),NULL,&status);

/*******************
* "Y" coefficients *
*******************/
sprintf(key,"COE_Y%d_A",pos_det_code);
fits_read_key_dbl(fp,key,&(coef->yshift   ),NULL,&status);

sprintf(key,"COE_Y%d_B",pos_det_code);
fits_read_key_dbl(fp,key,&(coef->rot[1][0]),NULL,&status);

sprintf(key,"COE_Y%d_C",pos_det_code);
fits_read_key_dbl(fp,key,&(coef->rot[1][1]),NULL,&status);

checkTelDefFITSerrors(status,"reading COE keywords from",teldef);

/************************************************************
* combine the half pixel shift and the coeficient transform *
************************************************************/
raw2int=allocateXform2d();
combineXform2ds(raw2int, half_pixel_shift, coef);
destroyXform2d(half_pixel_shift);
destroyXform2d(coef);


/**********************
* sat -> det rotation *
**********************/
fits_read_key_dbl(fp,"DET_ROTD",&angle,NULL,&status);
checkTelDefFITSerrors(status,"reading DET_ROTD from",teldef);
angle*=-M_PI/180.; /* convert to radians and take negative */

rotation=allocateXform2d();
setXform2dToRotation(rotation,sin(angle),cos(angle),0.,0.);

/******************************************
* sat -> det inversion  - flip the Y axis *
******************************************/
scaling=allocateXform2d();
setXform2dToScaling(scaling, 1./det->scale_x, -1./det->scale_y, 0. ,0.);


/*****************************
* combine all the transforms *
*****************************/
int2det=allocateXform2d();
combineXform2ds(int2det,rotation,scaling);
combineXform2ds(raw->trans,raw2int,int2det);


/************************************************************************
* and finally translate the origin to the center of the DET coordinates *
************************************************************************/
applyTranslationToXform2d(raw->trans, det->center_x, det->center_y);

setCoordDefTransformInfo(raw);

/*****************************
* free the temporary storage *
*****************************/
destroyXform2d(rotation);
destroyXform2d(scaling);
destroyXform2d(raw2int);
destroyXform2d(int2det);

/***************************************************
* we have sucessfully read the GIS raw coordinates *
***************************************************/
return(1);

} /* end of setRawForASCAGISTelDef function */

/************************************************************************
*************************************************************************
* read the standard detector coordinate keywords into a TELDEF structure
*
************************************************************************/
void readTeldefDetectorCoordinates(TELDEF* teldef, fitsfile* fp) {

int status=0;
int level;
long nlevels;
char** names;
int nfound;

/*
int *found;
char value[FLEN_VALUE];
char* indx_include_template[]={"*_INDX"};
char* xsiz_include_template[]={"*_XSIZ"};
char* xsiz_exclude_template[]={"SKY_XSIZ","RAW_XSIZ"};
char card[FLEN_CARD];
*/

COORDDEF* orig;
COORDDEF* dest;

#ifdef DEBUG
printf("readTeldefDetectorCoordinates: start\n");
#endif

/****************************************************
* determine the number of detector coordinate types *
****************************************************/
fits_read_key_lng(fp,"NCOORDS",&nlevels,NULL,&status);
if(status==KEY_NO_EXIST) {
    /**********************************************
    * default is one type of detector coordinates *
    **********************************************/
    status=0;
    nlevels=1;
} else {
    /**********************************
    * make sure there are no problems *
    **********************************/
    checkTelDefFITSerrors(status,"reading NCOORDS from",teldef);
    nlevels -=2; /* subtract raw and sky coords */
    if(nlevels<=0) {
        fprintf(stderr,"Invalid number of detector coordinate types %ld in %s\n",
                nlevels, teldef->filename);
        exit(1);
    }

}

#ifdef DEBUG
printf("readTeldefDetectorCoordinates: about to set nlevels=%ld\n", nlevels);
#endif


setDetLevelsInTelDef(teldef,nlevels);

#ifdef DEBUG
printf("readTeldefDetectorCoordinates: about to find root names\n");
#endif

/*********************************************
* find the root names of all the coordinates *
*********************************************/
names=(char**)malloc(sizeof(char*)*nlevels);
*names=(char*)malloc(sizeof(char)*nlevels*FLEN_VALUE);
for(level=1;level<nlevels;++level) names[level]=names[level-1]+FLEN_VALUE;

fits_read_keys_str(fp, "COORD", 1, nlevels, names, &nfound, &status);
checkTelDefFITSerrors(status,"reading COORDn keywords from",teldef);

if(nfound==0 && nlevels==1) {
    /********************************************************************
    * The COORD keywords are optional if there is only one type of DET
    * coordinate. The default name is "DET"
    ********************************************************************/
    strcpy(names[0],"DET");
} else if(nfound <nlevels) {
    fprintf(stderr,"Expected %ld coord keywords, but only found %d\n",
            nlevels, nfound);
    exit(1);
}

/******************************************************
* set the coordinate names in the coorddef structures *
******************************************************/
for(level=0;level<nlevels;++level) {

    #ifdef DEBUG
    printf("readTeldefDetectorCoordinates: %d name=|%s|\n",level,names[level]);
    #endif

    setStringInCoordDef(&(teldef->det[level]->name),names[level]);
}

/*****************************
* destroy the array of names *
*****************************/
free(*names);
free( names);        

/********************************************************************
* now loop over keywords again and set the COORDDEF values 
* for each level of detector coordinates
********************************************************************/
for(level=0;level<teldef->n_det_levels;++level) {

    setCoordinatesFromKeywordsInTeldef(teldef, teldef->det[level], fp );

} 



/**********************************************************************
* set transformations from one detector coordinate system to another
**********************************************************************/
for(level=0;level<teldef->n_det_levels-1;++level) {

    orig=teldef->det[level  ];
    dest=teldef->det[level+1];

    setXform2dFromTelDefKeywords(teldef, orig->trans, fp, orig, dest );
    
    setCoordDefTransformInfo(orig);

} /* end of loop over levels to set transformations */


#ifdef DEBUG
printf("readTeldefDetectorCoordinates: done\n");
#endif



} /* end of readDetectorCoordinatesIntoTeldef function */


/***********************************************************************
*************************************************************************
* set the alignment matrix which gives the transformation between
* spacecraft coordinates and bottom level detector coordinates
* The matrix is given as a rotation matrix encoded as a set of
* ALGNMnm keywords. This is stored in the TELDEF structure as
* a quaternion. We also calculate the inverse of this quaternion.
************************************************************************/
void readTelDefAlignment(TELDEF* teldef, fitsfile* fp) {


#ifdef DEBUG
printf("readTelDefAlignment: start\n");
#endif

teldef->alignment = readAlignFromFITSfile(fp);
if(teldef->alignment == NULL) exit(1);

#ifdef DEBUG
printf("readTelDefAlignment: done (sort of)\n");
#endif



} /* end of readTelDefAlignment function */




/************************************************************************
*************************************************************************
* read Sky coordinate information from a teldef file
************************************************************************/
void readTelDefSkyCoordinates(TELDEF* teldef, fitsfile* fp) {

int status=0;

char sky_from[FLEN_VALUE];

COORDDEF* sky;
COORDDEF* det; /* origin detector coordinates */

/*
double first_pixel_x, first_pixel_y;
long npixels_x, npixels_y;

double arcmin_per_mm;
*/
  

/*****************************************
* allocate the sky coordinate definition *
*****************************************/
sky=teldef->sky=allocateCoordDef();
setStringInCoordDef(&(sky->name),"SKY");

/*************************************************
* determine the detector coordinates which will
*  get transformed into sky coordinates
*************************************************/
fits_read_key_str(fp,"SKY_FROM",sky_from,NULL,&status);
if(status==KEY_NO_EXIST) {
    /***************************************************
    * default is the highest level detector coordinate *
    ***************************************************/
    status=0;
    teldef->sky_from=teldef->n_det_levels - 1;
} else if(!status) {
    /**************************************************
    * get the detector coordinate level from the name *
    **************************************************/
    teldef->sky_from=getTelDefDetLevelFromName(teldef,sky_from);
    if(teldef->sky_from<0) {
        fprintf(stderr,"Unknown SKY_FROM detector coordinate type %s",sky_from);
        exit(1);
    }

} else {
    /*****************************
    * some mysterious FITS error *
    *****************************/
    checkTelDefFITSerrors(status,"reading SKY_FROM keyword from",teldef);
}

det=teldef->det[teldef->sky_from];

/**********************************
* read the telescope focal length *
**********************************/
fits_read_key_dbl(fp,"FOCALLEN",&(teldef->focal_length),NULL,&status);
checkTelDefFITSerrors(status,"reading FOCALLEN from",teldef);

/***************************************************************
* determine the address space limits, scaling and column names
* for the sky coordinates
***************************************************************/
setCoordinatesFromKeywordsInTeldef(teldef, sky, fp );


} /* end of readTelDefSkyCoordinates function */


