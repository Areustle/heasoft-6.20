#include <stdlib.h>
#include <string.h>

#include "headas.h"
#include "headas_gti.h"

#include "fitsio.h"
#include "longnam.h"
#include "info.h"
#include "earthvel.h"
#include "att_fatal.h"



/*
#define READ_COLUMNS
*/



/******************************************************************************
*******************************************************************************
* set up misc info structure to hand to the iterator 
* the fitsfile pointer argument is for the event file
******************************************************************************/
INFO* createInfo(PARAM* param, fitsfile* fp) {

INFO* info;

double mjd;

int status=0;
char comment[FLEN_COMMENT]="";
char mission[FLEN_VALUE]="UNKNOWN";
char instrument[FLEN_VALUE]="UNKNOWN";

/********************************
* allocate memory for structure *
********************************/
info = (INFO*) calloc(1, sizeof(INFO));

/***********************************
* remember the parameter structure *
***********************************/
info->param=param;

/**********************************************
* read mission and instrument from event file *
**********************************************/
fits_read_key_str(fp,"TELESCOP",mission   ,comment,&status);
fits_read_key_str(fp,"INSTRUME",instrument,comment,&status);
if(status) {
    printf("Warning: can't read TELESCOP and INSTRUME keywords from %s\n",
           param->event_file);
    status=0;
}

/******************************************************
* open attitude file and make some consistency checks *
******************************************************/
if(param->do_sky) {
    info->att=openAttFile(param->att_file);
    resetAttFileExtrapolationLimits(info->att,param->time_margin);

    if(strncmp(mission,info->att->mission, FLEN_VALUE)) {
        fprintf(stderr,"Warning TELESCOP=%s in %s but =%s in %s\n",
                mission,param->event_file,info->att->mission,info->att->name);
    }

    setAttFileInterpolation(info->att, param->interpolation);
}

/****************************************************
* open teldef file and make some consistency checks *
****************************************************/
info->cal=readTelDef(param->cal_file);

if(strncmp(mission, info->cal->mission, FLEN_VALUE)) {
    fprintf(stderr,"Warning TELESCOP=%s in %s but =%s in %s\n",
            mission,param->event_file,info->cal->mission,info->cal->filename);
}

if(strncmp(instrument, info->cal->instrument, FLEN_VALUE)) {
    fprintf(stderr,"Warning INSTRUME=%s in %s but =%s in %s\n",
            instrument,param->event_file,
            info->cal->instrument,
            info->cal->filename);
}


/*******************************************************
* allocate storage for the current pointing quaternion *
*******************************************************/
info->q=allocateQuat();

/********************************************
* set the tangent plane coordinate system  
* corresponding to the nominal pointing
********************************************/
setSkyCoordCenterInTeldef(info->cal, param->ra,param->dec);



if(param->do_aberration) {
    /********************************************
    * set up for applying aberration correction *
    ********************************************/
    if(param->follow_sun) {
        /**************************************************************
        * we will be recalculating the earth's velocity for
        * each event. All we need to do here is read the
        * MJD reference time used to calculate MJD from mission time
        **************************************************************/
        info->mjdref = HDget_frac_time(fp, "MJDREF", 0, 0, &status);
        /*
        fits_read_key_dbl(fp,"MJDREF",&(info->mjdref),comment,&status);
        */
        if(status) {
            fprintf(stderr,"Can't read MJDREF from %s\n",param->event_file);
            fits_report_error(stderr,status);
            att_fatal(status);
        }

    } else {
        /******************************************************
        * we won't be recalculating the earth's velocity for
        * each event, so get the earth's velocity at MJD-OBS
        * and use that throughout the observation 
        ******************************************************/
        fits_read_key_dbl(fp,"MJD-OBS",&mjd,comment,&status);
        if(status) {
            fprintf(stderr,"Can't read MJD-OBS from %s\n",param->event_file);
            att_fatal(status);
        }
        info->v = compat_earthvel_at_mjd(info->vhat, mjd);

    } /* end if we will not be recalculating sun posistion for each event */
} else {
    /***********************************************
    * no aberation - indicate this by setting v=0. 
    * note in this case follow_sun will have been  
    * set to "NO" in readParam
    ***********************************************/
    info->v=0.;
}



return(info);

} /* end of createInfo function */




/**************************************************************************
***************************************************************************
* function to set iterator FITS column strucutres
**************************************************************************/
iteratorCol* setIteratorColumns(INFO* info, fitsfile* fp, int* ncols) {

iteratorCol* fits_col; /* array of columns */
iteratorCol* col;      /* pointer to a single column */
PARAM* param;
TELDEF* cal;

int seg;
int readrawx, readrawy;

int level;

#ifdef DEBUG 
printf("setIteratorColumns: start\n");
#endif

/**************************************************************
* get the param and teldef structures from the info structure *
**************************************************************/
param=info->param;
cal=info->cal;

/************************************************
* count the number of FITS columns we will need *
************************************************/
*ncols=0;

if(param->do_sky)    ++(*ncols); /* TIME    */
if(cal->nsegments>1) ++(*ncols); /* Segment */

readrawx=0;
readrawy=0;
for(seg=cal->min_segment; seg<cal->nsegments;++seg) {
    readrawx = readrawx || cal->raw[seg]->npixels_x>1;
    readrawy = readrawy || cal->raw[seg]->npixels_y>1;
}

if(readrawx) ++(*ncols); /* Raw X */
if(readrawy) ++(*ncols); /* Raw Y */

*ncols += 2*cal->n_det_levels; /* Detector */

if(param->do_sky) (*ncols) += 2; /* Sky */

#ifdef DEBUG
printf("setIteratorColumns: about to allocate %d columns\n",*ncols);
#endif /* DEBUG */

/**************************************
* allocate space for the fits columns *
**************************************/
fits_col=(iteratorCol*)malloc(sizeof(iteratorCol)*(*ncols) );

/*********************
* assign the columns *
*********************/
col=fits_col;

/**************
* Time column *
**************/
if(param->do_sky) {

    fits_iter_set_by_name(col,fp,param->time_col_name,TDOUBLE,InputCol);
    info->time_col=col;
    ++col;

} else info->time_col=NULL;

/*****************
* Segment column *
*****************/
if(cal->nsegments>1) {

    fits_iter_set_by_name(col,fp,cal->seg_col_name,TINT,InputCol);
    info->seg_col=col;
    ++col;

} else {
    info->seg_col=NULL;
    info->seg_value=0;
}

/***************
* Raw X column *
***************/
if(readrawx) {

    fits_iter_set_by_name(col,fp,cal->raw[cal->min_segment]->name_x,TINT,InputCol);
    info->rawx_col=col;
    ++col;

} else {
    info->rawx_col=NULL;
    info->rawx_value=(int)cal->raw[cal->min_segment]->first_pixel_x;
}

/***************
* Raw Y column *
***************/
if(readrawy) {

    fits_iter_set_by_name(col,fp,cal->raw[cal->min_segment]->name_y,TINT,InputCol);
    info->rawy_col=col;
    ++col;

} else {
    info->rawy_col=NULL;
    info->rawy_value=(int)cal->raw[cal->min_segment]->first_pixel_y;
}


/*******************
* detector columns *
*******************/
info->detx_col=(iteratorCol**)malloc(sizeof(iteratorCol*)*cal->n_det_levels);
info->dety_col=(iteratorCol**)malloc(sizeof(iteratorCol*)*cal->n_det_levels);

for(level=0;level<cal->n_det_levels;++level) {

#ifdef READ_COLUMNS

    /*************
    * Detector X *
    *************/
    fits_iter_set_by_name(col,fp,cal->det[level]->name_x,TINT,InputCol);
    info->detx_col[level]=col;
    ++col;

    /*************
    * Detector Y *
    *************/
    fits_iter_set_by_name(col,fp,cal->det[level]->name_y,TINT,InputCol);
    info->dety_col[level]=col;
    ++col;

#else

    /*************
    * Detector X *
    *************/
    fits_iter_set_by_name(col,fp,cal->det[level]->name_x,TINT,OutputCol);
    info->detx_col[level]=col;
    ++col;

    /*************
    * Detector Y *
    *************/
    fits_iter_set_by_name(col,fp,cal->det[level]->name_y,TINT,OutputCol);
    info->dety_col[level]=col;
    ++col;

#endif /* DEBUG */

} /* end of loop over Detector levels */

/*************************************************
* allocate array of pointers which will be used   
* to point to the actual detector data arrays
*************************************************/
info->detx=(int**)malloc(sizeof(int*)*cal->n_det_levels);
info->dety=(int**)malloc(sizeof(int*)*cal->n_det_levels);


/**************
* Sky Columns *
**************/
if(param->do_sky) {


#ifdef READ_COLUMNS

    /********
    * Sky X *
    ********/
    fits_iter_set_by_name(col,fp,cal->sky->name_x,TINT,InputCol);
    info->skyx_col=col;
    ++col;


    /********
    * Sky Y *
    ********/
    fits_iter_set_by_name(col,fp,cal->sky->name_y,TINT,InputCol);
    info->skyy_col=col;
    ++col;

#else

    /********
    * Sky X *
    ********/
    fits_iter_set_by_name(col,fp,cal->sky->name_x,TINT,OutputCol);
    info->skyx_col=col;
    ++col;


    /********
    * Sky Y *
    ********/
    fits_iter_set_by_name(col,fp,cal->sky->name_y,TINT,OutputCol);
    info->skyy_col=col;
    ++col;

#endif /* DEBUG */



} else {
    info->skyx_col=NULL;
    info->skyy_col=NULL;
}
    
#ifdef DEBUG
printf("setIteratorColumns: done\n");
#endif /* DEBUG */


return(fits_col);

} /* end of setIteratorColumns function */

/******************************************************************************
*******************************************************************************
* free all the memory associated with an INFO structure
* Note this even destroys the associated PARAM structure even though
* that structure was created separately.
******************************************************************************/
void destroyInfo(INFO* info) {

if(info->param->do_sky) closeAttFile(info->att);

destroyTelDef(info->cal);

destroyQuat(info->q);

free(info->detx_col);
free(info->dety_col);

free(info->detx);
free(info->dety);

destroyParam(info->param);

free(info);

}
