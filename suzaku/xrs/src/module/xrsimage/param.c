#include <string.h>

#include "param.h"

#define DEBUG

/*****************************************************************************
******************************************************************************
* create a new param structure and read parameters, mostly from the parfile
*****************************************************************************/
PARAM* readParam(int* iserror) {

PARAM* param;

param=(PARAM*)malloc(sizeof(PARAM));

/****************************************************
* what type of image should we produce?
* options are DET, FOC and SKY (case insensitive)
****************************************************/
read_string_param("image_type",param->image_type,FILENAME_LENGTH);

param->isDetImage=!strncasecmp(param->image_type,"DET",FILENAME_LENGTH);
param->isFocImage=!strncasecmp(param->image_type,"FOC",FILENAME_LENGTH);
param->isSkyImage=!strncasecmp(param->image_type,"SKY",FILENAME_LENGTH);

if( !(param->isDetImage || param->isFocImage || param->isSkyImage) ) {
    /*********************
    * unknown image type *
    *********************/
    fprintf(stderr,"Unknown image type %s requested\n",param->image_type);
    *iserror=(UNKNOWN_IMAGE_TYPE);
    return NULL;
} 


read_string_param("eventext",param->event_ext ,FLEN_VALUE);
read_string_param("eventfile",param->event_file ,FILENAME_LENGTH);

read_string_param("teldef"   ,param->teldef_file,FILENAME_LENGTH);
read_string_param("outfile"  ,param->out_file   ,FILENAME_LENGTH);

/*****************************************************************
* This is an xrs-specific task, so we can hard-wire column names *
*****************************************************************/
strncpy(param->time_col_name,"TIME",FLEN_VALUE);

return(param);
}

/***************************************************************************
****************************************************************************
* determine the WCS image scaling parameters. First try reading these from the 
* event file header. If they aren't there, then redetermine them from the
* teldef file values.
* Note that for sky images this
*****************************************************************************/
int readImageScaleParams(PARAM* param, fitsfile* fp, TELDEF* cal) {

int status=0;
int col[2];
char key[FLEN_KEYWORD];
int i;
int seg;

COORDDEF* coord=0;

#ifdef DEBUG
printf("readImageScaleParams: start\n");
#endif /* DEBUG */

 if(param->isDetImage) coord= &cal->mission.aste->det; /* det */
 if(param->isFocImage) coord= &cal->mission.aste->foc; 
 if(param->isSkyImage) coord= &cal->mission.aste->sky; 

#ifdef DEBUG
printf("readImageScaleParams: about to get column numbers\n");
printf("readImageScaleParams: n_det_levels=%d\n",cal->ncoords);
printf("readImageScaleParams: coord@%d\n",(int)coord);
printf("readImageScaleParams: name_x@%d name_y@%d\n",
       (int)(coord->xcol),(int)(coord->ycol));
#endif /* DEBUG */


/**************************************
* determine coordinate column numbers *
**************************************/
fits_get_colnum(fp,CASESEN,coord->xcol,col  ,&status);
fits_get_colnum(fp,CASESEN,coord->ycol,col+1,&status);

#ifdef DEBUG
printf("readImageScaleParams: col[0]=%d col[1]=%d\n",col[0],col[1]);
#endif /* DEBUG */


if(status) {
    fprintf(stderr,"Error finding coordinate column numbers in %s\n",
            param->event_file);

    fits_report_error(stderr,status);
    return status;
}

#ifdef DEBUG
printf("readImageScaleParams: about to read TC values\n");
#endif /* DEBUG */


/************************
* try to read TC values *
************************/
for(i=0;i<2;++i) {

sprintf(key,"TCRPX%d",col[i]);
fits_read_key_dbl(fp,key,&(param->crpix[i]),NULL,&status);

sprintf(key,"TCRVL%d",col[i]);
fits_read_key_dbl(fp,key,&(param->crval[i]),NULL,&status);

sprintf(key,"TCDLT%d",col[i]);
fits_read_key_dbl(fp,key,&(param->cdelt[i]),NULL,&status);

} /* end of loop over x and y columns */

#ifdef DEBUG
printf("readImageScaleParams: done reading TC status=%d\n",status);
#endif /* DEBUG */

if(status) {
    fprintf(stderr,"Error reading image scale values from %s\n",
            param->event_file);
    fits_report_error(stderr,status);
    return status;
}


/******************************************************************
* determine the RAW pixel coordinates. 
* Each XRS pixel should be a segment containing a single pixel.
* we need to get the coordinates of this pixel
* we make a number of assumptions here and check the teldef file
* to be sure they are all true. Why not be careful?
******************************************************************/
param->rawx=(int)cal->mission.aste->raw.xpix1;
param->rawy=(int)cal->mission.aste->raw.ypix1;

#ifdef DEBUG
printf("readImageScaleParams:checking rawx=%d rawy=%d\n",
       param->rawx, param->rawy);
#endif /* DEBUG */


for(seg=0;seg<cal->mission.aste->seg_num;++seg) {

#ifdef DEBUG
printf("readImageScaleParams:checking seg=%d\n",seg);
#endif /* DEBUG */


    /********************************************************
    * the coordinates of the first pixel must be an integer *
    ********************************************************/
    if((double)param->rawx != cal->mission.aste->raw.xpix1 ||
       (double)param->rawy != cal->mission.aste->raw.ypix1   ) {
        
        fprintf(stderr,"Warning: First pixel of segment %d is at (%g, %g)\n",
                seg,(double)cal->mission.aste->raw.xpix1,
                    (double)cal->mission.aste->raw.ypix1 );
    }

} /* end of loop checking each segment */

 return 0;
} /* end of readImageScaleParams function */
