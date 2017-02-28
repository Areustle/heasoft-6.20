#include <string.h>

#include "keywords.h"
#include "version.h"
#include "att_fatal.h"

/*
#define DEBUG
*/

/**************************************************************************
**************************************************************************
* update the keywords for a single set of coordinates 
**************************************************************************/
void update_coord_keywords(COORDDEF* coord,
                            iteratorCol* colx, iteratorCol* coly,
                            double crvalx, double crvaly,
                            int nullx, int nully ) {

int status=0;

int colnum;
fitsfile* fp;
char* colname;

char key[FLEN_KEYWORD];
char comment[FLEN_COMMENT];

#ifdef DEBUG
printf("update_coord_keywords: start\n");
printf("update_coord_keywords: crvalx=%g crvaly=%g\n", crvalx, crvaly);
printf("update_coord_keywords: coord@%d\n", (int)coord);
printf("update_coord_keywords: coord->name=%s\n", coord->name );
printf("update_coord_keywords: coord->first_pixel_x=%g\n",coord->first_pixel_x);
printf("update_coord_keywords: nullx=%d, nully=%d\n", nullx, nully);
#endif


/*********************
* first the X column *
*********************/
colnum =fits_iter_get_colnum (colx);
#ifdef DEBUG
printf("update_coord_keywords: colnum=%d\n", colnum);
#endif

fp     =fits_iter_get_file   (colx);
colname=fits_iter_get_colname(colx);

/********
* TLMIN *
********/
sprintf(key    ,"TLMIN%d"                    ,colnum );
#ifdef DEBUG
printf("update_coord_keywords: key=%s\n", key);
#endif

sprintf(comment,"Minimum value for %s column",colname);

#ifdef DEBUG
printf("update_coord_keywords: key=%s\n", key);
printf("update_coord_keywords: comment=%s\n", comment);
printf("update_coord_keywords: status=%d\n", status);
#endif

fits_update_key_lng(fp,key,coord->first_pixel_x,comment,&status);

#ifdef DEBUG
printf("update_coord_keywords: did TLMIN\n");
#endif



/********
* TLMAX *
********/
sprintf(key    ,"TLMAX%d"                    ,colnum );
sprintf(comment,"Maximum value for %s column",colname);
fits_update_key_lng(fp,key,coord->last_pixel_x,comment,&status);


/********
* TCRPX *
********/
sprintf(key    ,"TCRPX%d"                 ,colnum );
sprintf(comment,"%s image reference pixel",colname);
fits_update_key_dbl(fp,key,coord->center_x,6, comment,&status);


/********
* TCRVL *
********/
sprintf(key    ,"TCRVL%d"                                   ,colnum );
sprintf(comment,"%s image reference pixel coordinate (%s)",
        colname,coord->scale_unit);
fits_update_key_dbl(fp,key,crvalx,6,comment,&status);


/********
* TCDLT *
********/
sprintf(key    ,"TCDLT%d"                  ,colnum );
sprintf(comment,"%s image scale (%s/pixel)",colname,coord->scale_unit);

if(!strcmp(coord->name, "SKY") ) {
    /*************************************************
    * flip the X axis for sky coordinates to follow 
    * the convention that RA increases to the left
    *************************************************/
    fits_update_key_dbl(fp,key,-coord->scale_x,6,comment,&status);
    
} else {
    /**************************************
    * everything else is treated normally *
    **************************************/
    fits_update_key_dbl(fp,key,coord->scale_x,6,comment,&status);
}

/********
* TCTYP *
********/
sprintf(key,"TCTYP%d",colnum );
sprintf(comment,"%s coordinate type", colname);
fits_update_key_dbl(fp,key,crvalx,6,comment,&status);

if(!strcmp(coord->name, "SKY") ) {
    /******************************
    * sky coordinates are special *
    ******************************/
    fits_update_key_str(fp, key, "RA---TAN", comment, &status);
} else {
    /**************************
    * use the coordinate name *
    **************************/
    char value[FLEN_VALUE];
    sprintf(value, "%sX", coord->name);
    fits_update_key_str(fp, key, value, comment, &status);
}

/********
* TCUNI *
********/
sprintf(key    ,"TCUNI%d", colnum );
sprintf(comment,"%s units", colname);
fits_update_key_str(fp,key,coord->scale_unit,comment,&status);


/********
* TNULL *
********/
sprintf(key, "TNULL%d", colnum);
sprintf(comment,"%s null value", colname);

if(!strcmp(coord->name, "SKY") ) {
    /******************************
    * sky coordinates are special *
    ******************************/
    fits_update_key_lng(fp, key, nullx, comment, &status);
}


#ifdef DEBUG
printf("update_coord_keywords: did the X column\n");
#endif


/************************
* check for FITS errors *
************************/
if(status) {
    fprintf(stderr,"Error writing keywords for %s column\n",colname);
    fits_report_error(stderr,status);
    att_fatal(status);
}


/********************
* Then the Y column *
********************/
colnum =fits_iter_get_colnum (coly);
fp     =fits_iter_get_file   (coly);
colname=fits_iter_get_colname(coly);

/********
* TLMIN *
********/
sprintf(key    ,"TLMIN%d"                    ,colnum );
sprintf(comment,"Minimum value for %s column",colname);
fits_update_key_lng(fp,key,coord->first_pixel_y,comment,&status);


/********
* TLMAX *
********/
sprintf(key    ,"TLMAX%d"                    ,colnum );
sprintf(comment,"Maximum value for %s column",colname);
fits_update_key_lng(fp,key,coord->last_pixel_y,comment,&status);


/********
* TCRPX *
********/
sprintf(key    ,"TCRPX%d"                 ,colnum );
sprintf(comment,"%s image reference pixel",colname);
fits_update_key_dbl(fp,key,coord->center_y,6, comment,&status);


/********
* TCRVL *
********/
sprintf(key    ,"TCRVL%d"                                   ,colnum );
sprintf(comment,"%s image reference pixel coordinate (%s)",
        colname,coord->scale_unit);
fits_update_key_dbl(fp,key,crvaly,6,comment,&status);


/********
* TCDLT *
********/
sprintf(key    ,"TCDLT%d"                  ,colnum );
sprintf(comment,"%s image scale (%s/pixel)",colname,coord->scale_unit);
fits_update_key_dbl(fp,key,coord->scale_y,6,comment,&status);

/********
* TCTYP *
********/
sprintf(key,"TCTYP%d",colnum );
sprintf(comment,"%s coordinate type", colname);
fits_update_key_dbl(fp,key,crvalx,6,comment,&status);

if(!strcmp(coord->name, "SKY") ) {
    /******************************
    * sky coordinates are special *
    ******************************/
    fits_update_key_str(fp, key, "DEC--TAN", comment, &status);
} else {
    /**************************
    * use the coordinate name *
    **************************/
    char value[FLEN_VALUE];
    sprintf(value, "%sY", coord->name);
    fits_update_key_str(fp, key, value, comment, &status);
}

/********
* TCUNI *
********/
sprintf(key    ,"TCUNI%d", colnum );
sprintf(comment,"%s units", colname);
fits_update_key_str(fp,key,coord->scale_unit,comment,&status);


/********
* TNULL *
********/
sprintf(key, "TNULL%d", colnum);
sprintf(comment,"%s null value", colname);

if(!strcmp(coord->name, "SKY") ) {
    /******************************
    * sky coordinates are special *
    ******************************/
    fits_update_key_lng(fp, key, nully, comment, &status);
}


/************************
* check for FITS errors *
************************/
if(status) {
    fprintf(stderr,"Error writing keywords for %s column\n",colname);
    fits_report_error(stderr,status);
    att_fatal(status);
}


} /* end of update_coord_keywords */



/****************************************************************************
***************************************************************************** 
* Write keywords to document aspecting calculation
****************************************************************************/
void writeAspectingKeywords(fitsfile* fp, PARAM* param) {

int status=0;


fits_update_key_dbl(fp,"RA_NOM",param->ra,6/* decimals*/,
                    "R. A. of nominal aspect point",&status);

fits_update_key_dbl(fp,"DEC_NOM",param->dec,6/* decimals*/,
                    "Dec. of nominal aspect point",&status);

fits_update_key_log(fp,"ABERRAT",param->do_aberration,
                    "Has aberration been corrected for in sky coords?",&status);

fits_update_key_log(fp,"FOLOWSUN",param->follow_sun,
                    "Has the Sun position been recalculated for each event?",
                    &status);

if(status) {
    fprintf(stderr,"Error updating aspecting keywords\n");
    fits_report_error(stderr,status);
    att_fatal(status);
}

} /* end of writeAspectingkeywords function */




/****************************************************************************
***************************************************************************** 
* add HISTORY comments to header
****************************************************************************/
#define HISTORY_SIZE 512
#define append_safely(history,string) strncat((history),\
                                              (string),\
                                              HISTORY_SIZE-strlen(history)-1 )

void add_history_comments(fitsfile* fp, INFO* info) {

PARAM* param;

int status=0;
char history[HISTORY_SIZE]="";
int level;

char date[FLEN_VALUE];
int local_time;



param=info->param;

/*******************
* modified columns *
*******************/
append_safely(history,"The ");
for(level=0;level<info->cal->n_det_levels; ++level) {

    append_safely(history,info->cal->det[level]->name_x);
    append_safely(history," ");

    append_safely(history,info->cal->det[level]->name_y);
    append_safely(history," ");

} /* end of loop over detector coordinate levels */


if(param->do_sky) {

    append_safely(history,info->cal->sky->name_x);
    append_safely(history," ");

    append_safely(history,info->cal->sky->name_y);
    append_safely(history," ");

}

/**********
* version *
**********/
append_safely(history,"columns were updated by ");
append_safely(history,VERSION);

/*******
* date *
*******/
append_safely(history," on ");

fits_get_system_time(date,&local_time,&status);
append_safely(history,date);
if(!local_time) append_safely(history," UTC");

/**************
* teldef file *
**************/
append_safely(history," using teldef file ");
append_safely(history,info->cal->filename);

/****************
* attitude file *
****************/
if(info->param->do_sky) {
    append_safely(history," and attitude file ");
    append_safely(history,info->att->name);
}

/*********
* period *
*********/
append_safely(history,".");


/***************************
* write the history string *
***************************/
fits_write_history(fp,history,&status);

/************************
* check for FITS errors *
************************/
if(status) {
    fprintf(stderr,"Error writing HISTORY keywords\n");
    fits_report_error(stderr,status);
    att_fatal(status);
}


} /* end of add_history_comments function */




/****************************************************************************
***************************************************************************** 
* Update column keywords and add HISTORY comments 
****************************************************************************/
void update_keywords(INFO* info, iteratorCol* fits_col) {

TELDEF* cal;
PARAM* param;

int level;


cal=info->cal;
param=info->param;


/******************************************************
* Write RA_NOM, DEC_NOM, ABBERAT, and FOLOWSUN keywords *
******************************************************/
writeAspectingKeywords(fits_iter_get_file(fits_col),param);

/*********************************
* write Detector column keywords *
*********************************/
for(level=0;level<cal->n_det_levels;++level) {

    update_coord_keywords(cal->det[level],
                          info->detx_col[level],info->dety_col[level],
                          0., 0., 0, 0 );

} /* end of loop over Detector coordinates */

/***************************************
* write Sky coordinate column keywords *
***************************************/
#ifdef DEBUG
printf("update_keywords: updating sky keywords\n");
#endif
if(param->do_sky) {
    update_coord_keywords(cal->sky,info->skyx_col,info->skyy_col,
                          param->ra, param->dec,
                          param->skyx_null_value, param->skyy_null_value );
}



/***********************
* add history keywords *
***********************/
#ifdef DEBUG
printf("update_keywords: updating history\n");
#endif
add_history_comments(fits_iter_get_file(fits_col),info);

#ifdef DEBUG
printf("update_keywords: done\n");
#endif



} /* end of update_keywords function */


