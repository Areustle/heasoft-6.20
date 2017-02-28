#include <math.h>
#include <string.h>
#include "fitsio.h"
#include "longnam.h"

#include "teldef.h"


/*
#define DEBUG
*/



/****************************************************************************
*****************************************************************************
* create a new TELDEF structure and read its contents from a file.
* The actual reading is usually done by other mission-dependant routines
****************************************************************************/
TELDEF* readTelDef(char* filename) {

TELDEF* teldef;
int length;
fitsfile *fp;
int status=0;

int raw_set;

#ifdef DEBUG
printf("readTelDef: start\n");
#endif


/***********************************
* allocate space for the structure *
***********************************/
teldef=(TELDEF*)malloc(sizeof(TELDEF));


/************************************
* set the filename in the structure *
************************************/
length=strlen(filename)+1;
teldef->filename=(char*)malloc(sizeof(char)*length);
strncpy(teldef->filename,filename,length);


/***********************
* open the teldef file *
***********************/
fits_open_file(&fp,filename,READONLY,&status);
checkTelDefFITSerrors(status,"opening",teldef);

/*********************************************
* read the mission from the TELESCOP keyword *
*********************************************/
fits_read_key_str(fp,"TELESCOP",teldef->mission,NULL,&status);
checkTelDefFITSerrors(status,"reading TELESCOP from",teldef);

/************************************************
* read the instrument from the INSTRUME keyword *
************************************************/
fits_read_key_str(fp,"INSTRUME",teldef->instrument,NULL,&status);
checkTelDefFITSerrors(status,"reading INSTRUME from",teldef);

#ifdef DEBUG
printf("readTelDef:    mission=|%s|\n",teldef->mission   );
printf("readTelDef: instrument=|%s|\n",teldef->instrument);
#endif


/************************
* detector coordinates  *
************************/
readTeldefDetectorCoordinates(teldef,fp);

/********************************************************************
* Try a number of different methods for defining the raw coordinates
* each of the following functions tries a different method. They all
* return 1 if successful and 0 if the method is not applicable.
* if the method is not applicable, the function leaves no side effects.
*******************************************************************/
raw_set=0;
if(!raw_set) raw_set=setSinglePixelRawCoordsInTelDef(teldef, fp);
if(!raw_set) raw_set=setRawForASCAGISTelDef(teldef,fp);
if(!raw_set) raw_set=setRawFromCoefInTelDef(teldef,fp);
if(!raw_set) {
    fprintf(stderr,"Error: no raw coordinate definition found in %s\n",
            teldef->filename);
    exit(1);
}

/*******************************************************************
* now see if we need to make non-linear corrections in going from 
* raw -> detector coordinates. As is done for the ASCA GIS
*******************************************************************/
readNonLinearCorrectionsInTeldef(teldef,fp);

/*******************************************************************
* calculate RAW coordinate bounding box in bottom level detector
* coordinates. This bounding box is used mostly for doing
* detector -> raw transformations
*******************************************************************/
calculateBoundingBoxInTeldef(teldef);


/***************************************************************************
* read the alignment matrix giving the transformation from spacecraft to
* bottom level detector coordinates
**************************************************************************/
readTelDefSkyCoordinates(teldef,fp);
readTelDefAlignment(teldef, fp);

#ifdef DEBUG
printf("done reading sky coordinates\n");
#endif


/**********************************
* close the FITS calibration file *
**********************************/
fits_close_file(fp,&status);
checkTelDefFITSerrors(status,"closing",teldef);


/*******************************
* initialize the setable items *
*******************************/
teldef->q0=allocateQuat();
teldef->rot0=allocateRotMatrix();
setSkyCoordCenterInTeldef(teldef,0.,0.);

teldef->delta_q=allocateQuat();
teldef->xrt=allocateQuat();
teldef->det2sky=allocateXform2d();


#ifdef DEBUG
printf("readTelDef: done\n");
#endif


return(teldef);

} /* end of readTelDef function */



/***********************************************************************
*************************************************************************
* set nsegments and allocate the array of raw COORDDEF structures
* Assumes the raw COORDDEFs have not been previously allocated
************************************************************************/
void setNsegmentsInTelDef(TELDEF* teldef, int min_segment, int nsegments) {
int seg;

#ifdef DEBUG
printf("setNsegmentsInTelDef: %d segments starting at %d\n", 
       nsegments, min_segment);
#endif


nsegments += min_segment;

teldef->min_segment = min_segment;
teldef->nsegments    = nsegments;

teldef->raw=(COORDDEF**)malloc(sizeof(COORDDEF*)*nsegments);

/************************************************
* set all the unused "padding" segments to zero *
************************************************/
for(seg=0; seg<min_segment; ++seg) {

    teldef->raw[seg]=NULL;
}

/*********************************
* allocate all the used segments *
*********************************/
for(seg=min_segment; seg<nsegments;++seg) {

    teldef->raw[seg]=allocateCoordDef();
}

} /* end of setNsegmentsInTelDef function */

/****************************************************************************
* returns the number of segments - i.e. the number of different raw coordinates.
* Note that this is not the same as the nsegments field if min_segment > 0
*****************************************************************************/
int getTelDefSegmentCount(TELDEF* teldef) {
    return teldef->nsegments - teldef->min_segment;
}

/***********************************************************************
*************************************************************************
* set n_det_levels and allocate the array of det COORDDEF structures
* Assumes the det COORDDEFs have not been previously allocated.
* Note n_det_levels is the number of types of detector coordinates
* (e.g. 2 for DET + FOC coordinates).
************************************************************************/
void setDetLevelsInTelDef(TELDEF* teldef, int n_det_levels) {
int level;

/****************************************************************
* check that the number of levels is valid -
* if there are no detector coordinates, then there's not much 
* point in having a teldef file,. is there?
****************************************************************/
if(n_det_levels<=0) {
    fprintf(stderr,"Error: no detector coordinates found in %s\n",
            teldef->filename);
    exit(1);
}

/***************************
* set the number of levels *
***************************/
teldef->n_det_levels=n_det_levels;

/******************************************************
* allocate space for the detector coorddef structures *
******************************************************/
teldef->det=(COORDDEF**)malloc(sizeof(COORDDEF*)*n_det_levels);

for(level=0; level<n_det_levels;++level) {

    teldef->det[level]=allocateCoordDef();
}


} /* end of setDetLevelsInTelDef function */


/***********************************************************************
*************************************************************************
* after the raw COORDDEF structures in a TELDEF structure have been
* set, you should call this routine to calculate the bounding box
* of the raw coordinate system in bottom level detector coordinates 
************************************************************************/
void calculateBoundingBoxInTeldef(TELDEF* teldef) {

int seg;
double xmin,xmax,ymin,ymax;

COORDDEF* raw;

raw=teldef->raw[teldef->min_segment];
xmin=raw->trans_min_x;
xmax=raw->trans_max_x;
ymin=raw->trans_min_y;
ymax=raw->trans_max_y;


for(seg=teldef->min_segment+1; seg<teldef->nsegments;++seg) {

    raw=teldef->raw[seg];

    if(xmin>raw->trans_min_x) xmin=raw->trans_min_x;
    if(xmax<raw->trans_max_x) xmax=raw->trans_max_x;
    if(ymin>raw->trans_min_y) ymin=raw->trans_min_y;
    if(ymax<raw->trans_max_y) ymax=raw->trans_max_y;


} /* end of loop over segments */

/*****************************************
* set the values in the teldef structure *
*****************************************/
teldef->det0x_min=xmin;
teldef->det0x_max=xmax;
teldef->det0y_min=ymin;
teldef->det0y_max=ymax;



} /* end of calculateBoundingBoxInTeldef function */


/********************************************************************
*********************************************************************
* This function 
* tries to read a double valued keyword with a given prefix and
* returns a default value if the keyword is not found.
********************************************************************/
double readTelDefKey(TELDEF* teldef, fitsfile* fp,
                     char* key, char* key_end, char* suffix, 
                     double def) {
int status=0;
double value;

/*****************************************************
* copy the key suffix onto the end of the key prefix *
*****************************************************/
strcpy(key_end,suffix);
fits_read_key_dbl(fp,key,&value,NULL,&status);

#ifdef DEBUG
printf("key=|%s| value=%g status=%d\n",key,value,status);
#endif

if(status==KEY_NO_EXIST) {
    /********************
    * use default value *
    ********************/
    status=0;
    value=def;
} /* end if we use default value */

/*******************
* check for errors *
*******************/
checkTelDefFITSerrors(status, "reading keyword from",teldef);

return(value);

 } /* end of readTelDefKey function */




/*****************************************************************************
******************************************************************************
* This function reads a linear transformation from one coordinate system
* to another as defined by a set of keywords as follows:
*
* xxx_XOFF - x offset between centers default 0.
* xxx_YOFF - y offset between centers default 0.
* xxxXFLIP - x inversion (-1 or 1)    default 1.
* xxxYFLIP - y inversion (-1 or 1)    default -1. for RAW, otherwise 1. 
* xxx_SCAL - x and y scaling          default 1.
* xxx_ROTD - rotation in degrees      default 0.
*
* Where "xxx" is the keyword name of the destination coordinate system.
*
* These define an offset between the centers of the two coordinate systems,
* a scaling and possible inversion applied such that the center of the
* destination coordinate system remains fixed, and a rotation about the center
* of the destination coordinate system counterclockwise in degrees.
* 
* If any of these keywords is missing, the default is no transformation
* except in the case of YFLIP. Here a transformation from raw 
* coordinates to detector coordinates flips the y coordiinates by default,
* but a transformation from detector to detector coordinates
* does not flip by default.
*******************************************************************************/
void setXform2dFromTelDefKeywords(TELDEF* teldef, XFORM2D* trans, fitsfile* fp, 
                                  COORDDEF* orig, COORDDEF* dest ) {

char key[FLEN_KEYWORD];
char* key_end;

double centerx, centery;
double xoff, yoff;
XFORM2D* translation;

double default_yflip;
double scale,xflip,yflip;
XFORM2D* scaling;

double angle;
XFORM2D* rotation;

XFORM2D* temp;

#ifdef DEBUG
printf("setXform2dFromTelDefKeywords: start orig->name=%s\n", orig->name);
#endif

/**************************************************************
* copy the root name into the beginning of the keyword string *
**************************************************************/
strncpy(key,dest->name,FLEN_KEYWORD);
key_end=key+strlen(dest->name);

/*********************************************************************
* first translate the center of the center of the orig coordinates
* (0., 0.) then apply an offset
********************************************************************/
translation=allocateXform2d();

/*****************************************************************************
* We have to do some gymnastics to find the center of the 
* internal coordinate system if we are transforming from RAW coordinates.
****************************************************************************/
if(!strcmp(orig->name,"RAW") ) {
    /*****************************************************************
    * the internal, intermediate coordinates between RAW and bottom
    * level detector coordinates are not explicitly defined
    * in the teldef file. If INT_XCEN and INT_YCEN keywords exist
    * then we use these to define the center. If they are missing
    * then the center is assumed to be the same as the detector
    * coordinate center
    ****************************************************************/
    int status=0;

    fits_read_key_dbl(fp,"INT_XCEN",&centerx,NULL,&status);
    fits_read_key_dbl(fp,"INT_YCEN",&centery,NULL,&status);

    if(status==KEY_NO_EXIST) {
        status=0;
        centerx = dest->center_x;
        centery = dest->center_y;
    }
    checkTelDefFITSerrors(status,"reading INT_XCEN and INT_YCEN from",teldef);

} else {
    centerx = orig->center_x;
    centery = orig->center_y;
}

/**************************************************
* read the offset keywords - default is no offset *
**************************************************/
xoff=readTelDefKey(teldef,fp,key,key_end,"_XOFF",0.);
yoff=readTelDefKey(teldef,fp,key,key_end,"_YOFF",0.);

/**********************
* set the translation *
**********************/
setXform2dToTranslation(translation, -centerx - xoff, 
                                     -centery - yoff );



/*************************
* scaling  and inversion *
*************************/
if(!strcmp(orig->name,"RAW") ) default_yflip=-1.; /* flip RAW by deafult */
else                           default_yflip= 1.;

scale=readTelDefKey(teldef,fp,key,key_end,"_SCAL",1.);
xflip=readTelDefKey(teldef,fp,key,key_end,"XFLIP",1.);
yflip=readTelDefKey(teldef,fp,key,key_end,"YFLIP",default_yflip);

scale=1./scale; /* by convention the scaling is 1./SCAL keyword */

scaling=allocateXform2d();
setXform2dToScaling(scaling, scale*xflip, scale*yflip, 0., 0. );

/***********
* rotation *
***********/
angle=readTelDefKey(teldef,fp,key,key_end,"_ROTD",0.);
angle*=M_PI/180.;

rotation=allocateXform2d();
setXform2dToRotation(rotation, sin(angle), cos(angle), 0., 0.);


/******************************************************************
* now combine the translation, scaling and rotation in that order *
******************************************************************/
temp=allocateXform2d();
combineXform2ds(temp,translation,scaling);
combineXform2ds(trans,temp,rotation);

/******************************************************************
* finally translate the origin to the center of the destination
* coordinate system
******************************************************************/
applyTranslationToXform2d(trans, dest->center_x, dest->center_y);

/***********
* clean up *
***********/
destroyXform2d(temp);
destroyXform2d(translation);
destroyXform2d(scaling);
destroyXform2d(rotation);

#ifdef DEBUG
printf("setXform2dFromTelDefKeywords: result:\n");
printXform2d(trans,stdout);
#endif

} /* end of setXform2dFromTelDefKeywords function */



/****************************************************************************
*****************************************************************************
* Read the address space limits, pixel scale and column names
* for a given set of coordinates. The "name" element of the 
* given COORDDEF structure must be set before calling this function
****************************************************************************/
void setCoordinatesFromKeywordsInTeldef(TELDEF* teldef, COORDDEF* coord,
                                        fitsfile* fp ) {

int status=0;

char key[FLEN_KEYWORD];
char* key_end;

double first_pixel_x, first_pixel_y;
long npixels_x, npixels_y;

char units[FLEN_VALUE];
char colname[FLEN_VALUE];

#ifdef DEBUG
printf("setCoordinatesFromKeywordsInTeldef name=%s\n",coord->name);
#endif

/**************************************************************
* copy the root name into the beginning of the keyword string *
**************************************************************/
strncpy(key,coord->name,FLEN_KEYWORD);
key_end=key+strlen(coord->name);



/*****************************************
* read the address space limits keywords *
*****************************************/
strcpy(key_end,"XPIX1");
fits_read_key_dbl(fp,key,&first_pixel_x,NULL,&status);

strcpy(key_end,"YPIX1");
fits_read_key_dbl(fp,key,&first_pixel_y,NULL,&status);
    
strcpy(key_end,"_XSIZ");
fits_read_key_lng(fp,key,&npixels_x,NULL,&status);

strcpy(key_end,"_YSIZ");
fits_read_key_lng(fp,key,&npixels_y,NULL,&status);

if(status==KEY_NO_EXIST && !strcmp(coord->name,"RAW") &&   
                           !strcmp(teldef->mission,"ASCA") ) {
    /**************************************************************
    * we have some special defaults because the original ASCA 
    * teldef files omitted these keywords for the raw coordinates
    **************************************************************/
    status=0;

    if(!strcmp(teldef->instrument,"SIS0") ||
       !strcmp(teldef->instrument,"SIS1")) {
        /***********
        * ASCA SIS *
        ***********/
        first_pixel_x=6.;
        first_pixel_y=2.;
        
        npixels_x=419;
        npixels_y=416;

    } else if(!strcmp(teldef->instrument,"GIS2") ||
              !strcmp(teldef->instrument,"GIS3")) {
        /***********
        * ASCA GIS *
        ***********/
    } else {
        /**********
        * unknown *
        **********/
        fprintf(stderr,"Unknown ASCA instrument %s\n",teldef->instrument);
        exit(1);
    }

} /*end of ASCA defaults */

if(status==KEY_NO_EXIST && !strcmp(coord->name,"SKY") ) {
    /*******************************************************
    * sky coordinate dimensions default to the originating
    * detector coordinate dimensions
    *******************************************************/
    status=0;

    first_pixel_x = teldef->det[teldef->sky_from]->first_pixel_x;
    first_pixel_y = teldef->det[teldef->sky_from]->first_pixel_y;
    npixels_x     = teldef->det[teldef->sky_from]->npixels_x;
    npixels_y     = teldef->det[teldef->sky_from]->npixels_y;

}

    

#ifdef DEBUG
printf("first_pixel_x=%g first_pixel_y=%g\n",first_pixel_x,first_pixel_y);
printf("npixels_x=%ld npixels_y=%ld\n",npixels_x,npixels_y);
printf("status=%d\n",status);
#endif

/*******************
* check for errors *
*******************/
checkTelDefFITSerrors(status,"Reading coordinate information from",teldef);

/*******************************************
* set the values in the COORDDEF structure *
*******************************************/
setCoordDefLimits(coord,first_pixel_x,(int)npixels_x,
                        first_pixel_y,(int)npixels_y  );


/*************************
* determine scale values *
*************************/
if(!strcmp(coord->name,"SKY") ) {
    /**********************************************************************
    * SKY coordinates have their scale determined from the focal length
    * and the scale of the originating detector coordinates
    * note that the physical pixel size of the sky coordinates must
    * be the same as the size of the originating detector coordinates
    * Note that the FOCALLEN value must be in the same units as
    * the scale of the originating detector coordinates. 
    **********************************************************************/
    double from_radians;
    COORDDEF* det;

    /*************************************
    * determine preferred physical units *
    *************************************/
    strcpy(key_end,"_UNIT");
    fits_read_key_str(fp,key,units,NULL,&status);
    if(status==KEY_NO_EXIST) {
        /***********************************
        * sky units default to arc minutes *
        ***********************************/
        status=0;
        strcpy(units,"arcmin");
    }

    setStringInCoordDef(&(coord->scale_unit),units);

    /**********************************
    * interpret the name of the units *
    **********************************/
    if(     !strcmp(units,"arcmin")     ) from_radians=180.*60./M_PI;
    else if(!strcmp(units,"arc minutes")) from_radians=180.*60./M_PI;
    else if(!strcmp(units,"arcsec")     ) from_radians=180.*3600./M_PI;
    else if(!strcmp(units,"arc seconds")) from_radians=180.*3600./M_PI;
    else if(!strcmp(units,"deg")        ) from_radians=180./M_PI;
    else if(!strcmp(units,"degrees")    ) from_radians=180./M_PI;
    else if(!strcmp(units,"rad"    )    ) from_radians=1.;
    else if(!strcmp(units,"radians")    ) from_radians=1.;
    else {
        fprintf(stderr,"Unsupported SKY units %s\n",units);
        exit(1);
    }


    /**************************************************************
    * the X and Y scales of the originating detector coordinates
    * must be the same - perhaps some day we will lift this
    * restriction
    **************************************************************/
    det=teldef->det[teldef->sky_from];
    if(det->scale_x != det->scale_y) {
        fprintf(stderr,"Sky pixels are not square since:\n");
        fprintf(stderr,"%s scale=%g %s/pixel and %s scale=%g %s/pixel\n",
                det->name_x, det->scale_x, det->scale_unit,
                det->name_y, det->scale_y, det->scale_unit );
        exit(1);
    }

    /****************
    * set the scale *
    ****************/
    coord->scale_x=det->scale_x * from_radians/teldef->focal_length;
    coord->scale_y=det->scale_y * from_radians/teldef->focal_length;

    /**************************************************************
    * det->sky transformations require the pixel scale in radians.
    * Note that the X and Y scales are required to be the same
    * so we arbitrarily pick the X scale here.
    **************************************************************/
    teldef->sky_pix_per_radian = teldef->focal_length / det->scale_x;


} else {
    /**************************************************************************
    * for raw or detector coordinates we try to read the scale keywords
    * though in the usual case the raw coordinates default to being unscaled
    **************************************************************************/
    strcpy(key_end,"_XSCL");
    fits_read_key_dbl(fp,key,&(coord->scale_x),NULL,&status);
    
    strcpy(key_end,"_YSCL");
    fits_read_key_dbl(fp,key,&(coord->scale_y),NULL,&status);

    if(status==KEY_NO_EXIST && !strcmp(coord->name,"RAW") ) {
        /*************************************
        * by default raw pixels are unscaled *
        *************************************/
        status=0;

        coord->scale_x=1.;
        coord->scale_y=1.;

        setStringInCoordDef(&(coord->scale_unit),"pixels"  );
    } else {
        /*************************************************
        * This is not the RAW scale default case, so
        * determine the scale unit.
        *************************************************/
    
        strcpy(key_end,"_UNIT");
        fits_read_key_str(fp,key,units,NULL,&status);

        if(status==KEY_NO_EXIST) {
            /************************************************
            * reset status and set default value milimeters *
            ************************************************/
            status=0;
            strcpy(units,"mm");
        }

        setStringInCoordDef(&(coord->scale_unit),units);

    } /* end if not raw default scale */

} /* end if setting raw or detector scales */

#ifdef DEBUG
printf("scale_x=%g scale_y=%g units=%s status=%d\n",
       coord->scale_x,coord->scale_y,coord->scale_unit,status);
#endif


/***************
* column names *
***************/
    

/****************
* X column name *
****************/
strcpy(key_end,"_XCOL");
fits_read_key_str(fp,key,colname,NULL,&status);
if(status==KEY_NO_EXIST) {
    /***********************
    * use the default name *
    ***********************/
    status=0;
    if(!strcmp(coord->name,"SKY")) {
        /***********************************************
        * default name for sky coordinates is just "X" *
        ***********************************************/
        strcpy(colname,"X");
    } else {
        /*****************************************
        * for everything else default is "nameX" *
        *****************************************/
        strcpy(colname,coord->name);
        strcat(colname,"X");
    }

} 
setStringInCoordDef(&(coord->name_x),colname);


/****************
* Y column name *
****************/
strcpy(key_end,"_YCOL");
fits_read_key_str(fp,key,colname,NULL,&status);
if(status==KEY_NO_EXIST) {
    /***********************
    * use the default name *
    ***********************/
    status=0;
    if(!strcmp(coord->name,"SKY")) {
        /***********************************************
        * default name for sky coordinates is just "Y" *
        ***********************************************/
        strcpy(colname,"Y");
    } else {
        /*****************************************
        * for everything else default is "nameY" *
        *****************************************/
        strcpy(colname,coord->name);
        strcat(colname,"Y");
    }
} 
setStringInCoordDef(&(coord->name_y),colname);


#ifdef DEBUG
printf("name_X=|%s| name_y=|%s| status=%d\n",
       coord->name_x, coord->name_y,status);
#endif

/******************************
* check for stray FITS errors *
******************************/
checkTelDefFITSerrors(status,"reading address space keywords from",teldef);

} /* end of setCoordinatesFromKeywordsInTeldef function */


/***********************************************************************
*************************************************************************
* handle FITSIO errors while reading teldef files
* prints error messages and exits if there is an error
************************************************************************/
void checkTelDefFITSerrors(int status, char* doing, TELDEF* teldef) {
 
   if(!status) return;
 
   fprintf(stderr,"FITSIO error while %s file %s:\n",doing,teldef->filename);
   fits_report_error(stderr,status);

   exit(status);
 
}

/*****************************************************************************
******************************************************************************
* free all memory associated with a TELDEF structure
*****************************************************************************/
void destroyTelDef(TELDEF* teldef) {

int i;

free(teldef->filename);

/************
* raw stuff *
************/
for(i=teldef->min_segment; i<teldef->nsegments;++i) {
    destroyCoordDef(teldef->raw[i]);
}
free(teldef->raw);

free(teldef->seg_col_name);

if(teldef->raw_corrections_needed) destroyMapXform(teldef->raw_map);

/*****************
* detector stuff *
*****************/
for(i=0;i<teldef->n_det_levels;++i) {
    destroyCoordDef(teldef->det[i]);
}
free(teldef->det);

/************
* sky stuff *
************/
destroyCoordDef(teldef->sky);
destroyAlign(teldef->alignment);

destroyQuat(teldef->q0);
destroyRotMatrix(teldef->rot0);

destroyQuat(teldef->delta_q);
destroyQuat(teldef->xrt);
destroyXform2d(teldef->det2sky);

/***********************************
* and destroy the structure itself *
***********************************/
free(teldef);

} /* end of destroyTeldef function */

