#include <stdlib.h>
#include <math.h>

#define TOOLSUB coordinator
/* att_fatal.h / headas_main() require that TOOLSUB be defined first */

#include "headas.h"
#include "coordfits.h"
#include "param.h"
#include "info.h"
#include "earthvel.h"
#include "keywords.h"
#include "version.h"
#include "random.h"

#include "headas_main.c"
#include "att_fatal.h"


/*
#define DEBUG
*/



/****************************************************************************
*****************************************************************************
* iterator work function 
****************************************************************************/
int update_coordinates(long total_rows, long offset, long first_row, long nrows,
               int ncols, iteratorCol *fits_col,  void* void_info ) 
{
INFO* info;

int percent_done;

double* time=0;
int* seg=0;
int* rawx=0;
int* rawy=0;

int level;
int** detx;
int** dety;

int* skyx=0;
int* skyy=0;

int seg_value;
int rawx_value;
int rawy_value;

PARAM* param;
ATTFILE* att;
TELDEF* cal;

double v=0.0;
double vhat[3];

double mjd;
double mjdref=0.0;

int row;

double old_detx, old_dety;
double new_detx, new_dety;

double cont_detx, cont_dety;
double cont_skyx, cont_skyy;


#ifdef DEBUG
printf("first_row=%ld nrows=%ld\n",first_row,nrows);
#endif /* DEBUG */

/********************************************
* get various stuff from the info structure *
********************************************/
info=(INFO*)void_info;

att=info->att;
cal=info->cal;

param=info->param;

if(param->follow_sun) {
    mjdref=info->mjdref;
} else {
    v=info->v;

    vhat[0]=info->vhat[0];
    vhat[1]=info->vhat[1];
    vhat[2]=info->vhat[2];

}


/***************************************************
* get the column arrays from the column structures *
***************************************************/
if(info->time_col!=NULL) time=(double*)fits_iter_get_array(info->time_col);

if(info->seg_col!=NULL ) seg =(int*)fits_iter_get_array(info->seg_col );
if(info->rawx_col!=NULL) rawx=(int*)fits_iter_get_array(info->rawx_col);
if(info->rawy_col!=NULL) rawy=(int*)fits_iter_get_array(info->rawy_col);

detx=info->detx;
dety=info->dety;

for(level=0;level<cal->n_det_levels;++level) {
    detx[level]=(int*)fits_iter_get_array(info->detx_col[level]);
    dety[level]=(int*)fits_iter_get_array(info->dety_col[level]);
}

if(info->skyx_col!=NULL) skyx=(int*)fits_iter_get_array(info->skyx_col);
if(info->skyy_col!=NULL) skyy=(int*)fits_iter_get_array(info->skyy_col);


/******************************************
* loop over all rows in the current chunk *
******************************************/
for(row=1;row<=nrows;++row) {

    /*****************************************************
    * set the input quantities to either the values
    * from the event file or to constant values 
    *****************************************************/
    if(info->seg_col!=NULL) seg_value=seg[row];
    else                    seg_value=info->seg_value;

    if(info->rawx_col!=NULL) rawx_value=rawx[row];
    else                     rawx_value=info->rawx_value;

    if(info->rawy_col!=NULL) rawy_value=rawy[row];
    else                     rawy_value=info->rawy_value;

    /*********************************
    * check the segment for validity *
    *********************************/
    if(seg_value <cal->min_segment || seg_value >= cal->nsegments) {
        fprintf(stderr, "Invalid segment %d row %d\n", 
                seg_value, (int)(row+first_row) );

        /*************************
        * set everything to null *
        *************************/
        for(level=0;level<cal->n_det_levels;++level) {
            
            detx[level][row]=param->skyx_null_value;
            dety[level][row]=param->skyy_null_value;
        }

        skyx[row]=param->skyx_null_value;
        skyy[row]=param->skyy_null_value;
    }


    /*****************
    * debugging info *
    *****************/
    headas_chat(5,"%d seg=%d rawx=%d rawy=%d ",
                row, seg_value, rawx_value, rawy_value);
        

    /*****************************************************
    * transform Raw to bottom level Detector coordinates *
    *****************************************************/
    if(param->randomize) {
        /**************************************************************
        * assign a random location for the event within the pixel
        * Note we can't embed the calls to get_random in the argument
        * list of convertRawToDetectorUsingTeldef, since the order
        * in which the arguments are evaluated is machine-dependant
        **************************************************************/
        double dx, dy;
        dx = get_random();
        dy = get_random();
        
        headas_chat(5, "dx=%g dy=%g ", dx, dy);

        convertRawToDetectorUsingTeldef(cal,&cont_detx, &cont_dety, seg_value,
                                        rawx_value,rawy_value, 
                                        dx, dy);

    } else {
        /*************************************************
        * assume the event is in the center of the pixel *
        *************************************************/
        convertRawToDetectorUsingTeldef(cal,&cont_detx, &cont_dety, seg_value,
                                        rawx_value,rawy_value, 0.0, 0.0 );
    }

    /*****************
    * debugging info *
    *****************/
    headas_chat(5, "%s=%g %s=%g ",
                cal->det[0]->name_x, cont_detx,
                cal->det[0]->name_y, cont_dety);


    /*******************************************
    * round the detector values to the nearest  
    * integer and write to the I/O array.
    *******************************************/
    detx[0][row]=(int)(cont_detx+.5);
    dety[0][row]=(int)(cont_dety+.5);

    /*****************************************************
    * transform to the higher level detector coordinates *
    *****************************************************/
    old_detx=cont_detx;
    old_dety=cont_dety;
    for(level=1;level<cal->n_det_levels;++level) {

        /***************************************************
        * transform and put the results in the column data *
        ***************************************************/
        convertDetectorToDetectorUsingTelDef(cal,level-1,level,
                                             &new_detx,&new_dety,
                                              old_detx, old_dety );
        detx[level][row]=(int)(new_detx+.5);
        dety[level][row]=(int)(new_dety+.5);
        

        headas_chat(5, "%s=%g %s=%g ",
                    cal->det[level]->name_x, new_detx,
                    cal->det[level]->name_y, new_dety);


        if(level==cal->sky_from) {
            /***************************************************
            * these are the originating coordinates for the
            * detector to sky transformation, so save them
            * in the "cont" variables for later use
            ***************************************************/
            cont_detx = new_detx;
            cont_dety = new_dety;
        }


        /*************************************
        * the new cooridnate become the old 
        * coordinates for the next iteration
        *************************************/
        old_detx=new_detx;
        old_dety=new_dety;


    } /* end of loop over higher detector levels */


    /****************************
    * DET -> SKY transformation *
    ****************************/
    if(param->do_sky) {

        /********************************************
        * check if time is covered by attitude file *
        ********************************************/
        if(!isInExtrapolatedAttFile(att,time[row]) ) {
            static int max_warnings = 10;

            ++info->missing_attitude_count;

            if (max_warnings > 0) {
               --max_warnings;
               fprintf(stdout,
                    "Event %d %s=%.14g not covered by attitude file\n",
                    (int)(row+first_row-1), param->time_col_name, time[row]);
               if (!max_warnings)
                  fprintf(stdout, "Further events outside attitude file"
							" will be reported in aggregate\n");
            }

            skyx[row]=param->skyx_null_value;
            skyy[row]=param->skyy_null_value;

        } else {
            /***************************************************
            * the current time is covered by the attitude file *
            ***************************************************/

            if(row>1 && time[row]==time[row-1]) {
                /********************************************************
                * the event occured at the same time as the last event
                * so no need to reread the attitude file or
                * recalculate the aberration
                ********************************************************/
                repeatDetectorToSkyTeldefConversion(cal, 
                                                    &cont_skyx, &cont_skyy,
                                                    cont_detx, cont_dety);
            } else {
                /*********************************************
                * this is a new time so we have to read the 
                * attitude file and calculate aberration
                *********************************************/
                findQuatInAttFile(att,info->q,time[row]);

                if(param->follow_sun) {
                    /*********************************************************
                    * recalculate the Earth's velocity vector for this event *
                    *********************************************************/
                    mjd=mjdref+time[row]/86400.;
                    v = compat_earthvel_at_mjd(vhat, mjd);
#if 0
	printf("v=%f, vhat=[ %f, %f, %f ]\n", v, vhat[0], vhat[1], vhat[2]);
#endif
                }
    
                /***********************************
                * do the DET -> SKY transformation *
                ***********************************/
                convertDetectorToSkyUsingTeldef(cal, &cont_skyx, &cont_skyy,
                                                      cont_detx, cont_dety, 
                                                info->q, v, vhat);

            } /* end if current time is different from previous */

            /*********************************************
            * check if the sky coordinates are in bounds *
            *********************************************/
            if(isnan(cont_skyx) || isnan(cont_skyy) ||
               cont_skyx<cal->sky->first_pixel_x ||
               cont_skyy<cal->sky->first_pixel_y || 
               cont_skyx>cal->sky->last_pixel_x  ||
               cont_skyy>cal->sky->last_pixel_y    ) {
                /**************************************************************
                * This event is outside the SKY address space area, so set the 
                * values to some null value.
                * We need to do this check so that we don't get SKY coordinates 
                * which overflow the data type of the FITS column.
                **************************************************************/
                skyx[row]=param->skyx_null_value;
                skyy[row]=param->skyy_null_value;

            } else {
                /************************************************************
                * sky coordinate are within the allowed range, so convert
                * them to integers and store them in the array to be written 
                * to the event file
                ************************************************************/
                headas_chat(5, "skyx=%g skyy=%g", cont_skyx,cont_skyy);

                skyx[row]=(int)(cont_skyx+.5);
                skyy[row]=(int)(cont_skyy+.5);



            } /* end if sky coordinates are in bounds */

        } /* end if time is covered by attfile */

    } /* end if we are doing DET -> SKY transformation */
    
    /********************************
    * end of line of debugging info *
    ********************************/
    headas_chat(5, "\n");


} /* end of loop over rows (events) */


/********************************************************************
* calculate the fraction of the file completed after this iteration *
********************************************************************/
percent_done=(int)(100. * (double)(first_row-offset+nrows-1)/
                          (double)(total_rows-offset)      );
                          
headas_chat(2, "%d%% done\n", percent_done);


return(0);

} /* end of update_coordinates function */


/******************************************************************************
*******************************************************************************
******************************************************************************/
void coordinator_aux (void) 
{
fitsfile* fp;
PARAM* param;
int status=0;

int ncols;
iteratorCol* fits_col;
INFO* info;

#ifdef DEBUG
printf("coordinator: start\n");
#endif /* DEBUG */

set_toolversion(VERSION_ID);

/****************************
* read the input parameters *
****************************/
param=readParam();
if (!param) {
   headas_chat(0, "unable to load parameters\n");
   att_fatal(1);
}

#ifdef DEBUG
printf("coordinator: about to open event file\n");
#endif /* DEBUG */


/*********************************************
* open FITS file and move to event extension *
*********************************************/
fits_open_file(&fp,param->event_file,READWRITE,&status);
fits_movnam_hdu(fp,BINARY_TBL,param->event_extension,0/* any version*/,&status);
if(status) {
    fprintf(stderr,"Error while opening event file %s\n",param->event_file);
    fits_report_error(stderr,status);
    att_fatal(status);
}

#ifdef DEBUG
printf("coordinator: about to create info structure\n");
#endif /* DEBUG */



/************************************************************************
* create the INFO structure and fill it with information.
* The info structure is used to communicate with the work function
* and to provide persistent state between work function calls
************************************************************************/
info=createInfo(param,fp);

#ifdef DEBUG
printf("coordinator: about to set FITS columns\n");
#endif /* DEBUG */


/********************************************
* set up column structures for the iterator *
********************************************/
fits_col=setIteratorColumns(info,fp,&ncols);

#ifdef DEBUG
printf("coordinator: about to call iterator\n");
#endif /* DEBUG */


/****************************************
* call the iterator to do the real work *
****************************************/
fits_iterate_data(ncols,fits_col,
                  0l/*don't skip any rows*/,
                  0l/*read optimum columns per iteration*/,
                  update_coordinates,info,&status);
if(status) {
    fprintf(stderr,"Error from iterator\n");
    fits_report_error(stderr,status);
    att_fatal(status);
}

if (info->missing_attitude_count)
    fprintf(stdout, "Warning: %d events were not covered by the attitude file\n",
                info->missing_attitude_count);

#ifdef DEBUG
printf("coordinator: about to update keywords\n");
#endif /* DEBUG */



/*****************************************
* update the appropriate header keywords *
*****************************************/
update_keywords(info,fits_col);

#ifdef DEBUG
printf("coordinator: about to close events file\n");
#endif /* DEBUG */


/*******************
* close event file *
*******************/
HDpar_stamp(fp, 0, &status);
fits_close_file(fp,&status);

if(status) {
    fprintf(stderr,"Error while closing event file\n");
    fits_report_error(stderr,status);
    att_fatal(status);
}


/********************************
* clean up the allocated memory *
********************************/
#ifdef DEBUG
printf("coordinator: destroying info structure\n");
#endif /* DEBUG */

destroyInfo(info);


}
