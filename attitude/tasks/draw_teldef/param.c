#include <stdlib.h>
#include <string.h>

#include "param.h"
#include "param_wrappers.h"
#include "att_fatal.h"


/***************************************************************************
****************************************************************************
* create a new PARAM structure and read the parfile
***************************************************************************/
PARAM* readParam(void) {

PARAM* param;

char detname[FILENAME_DIMEN];
int i;

COORDDEF* det;
double scalex, scaley;
double x, y;

param=(PARAM*)malloc(sizeof(PARAM));

/**************
* teldef file *
**************/
read_string_param("teldef",param->filename,FILENAME_DIMEN);
param->teldef=readTelDef(param->filename);

/**************
* output file *
**************/
read_string_param("outfile",param->outfile,FILENAME_DIMEN);
if(!strncmp(param->outfile,"stdout",FILENAME_DIMEN)) param->fp = stdout;
else param->fp = fopen(param->outfile,"w");

if(param->fp == NULL) {
    fprintf(stderr,"Could not open output file %s\n",param->outfile);
    att_fatal(1);
}

/******************************************
* detector coordinates to align with page *
******************************************/
if(param->teldef->n_det_levels>1) {
    read_string_param("detname",detname,FILENAME_DIMEN);

    /************************************************************
    * find the coordinate level corresponding to the given name *
    ************************************************************/
    for(i=0;
        i<param->teldef->n_det_levels && 
        strncmp(detname,param->teldef->det[i]->name,FILENAME_DIMEN);
        ++i);

    if(i==param->teldef->n_det_levels) {
        /**********************************
        * no match - try reading a number *
        **********************************/
        if(sscanf(detname,"%d",&(param->frame_level))!=1 &&
           param->frame_level>=0 && 
           param->frame_level < param->teldef->n_det_levels) {
            /*****************************************************
            * even reading as a number failed, so give a warning
            * and use the highest level coordinates
            *****************************************************/
            param->frame_level = param->teldef->n_det_levels -1;
            fprintf(stderr,"Warning: Invalid detector coordinate name %s\n",
                    detname);
            fprintf(stderr,"         Aligning %s coordinates with page\n",
                    param->teldef->det[param->frame_level]->name );
        }
    } else {
        /********************************
        * found a coordinate name match *
        ********************************/
        param->frame_level=i;
    } 
} else {
    /*******************************************************
    * only one type of detector coordinate so use that one *
    *******************************************************/
    param->frame_level=0;
}

/*****************************
* label the raw coordinates? *
*****************************/
param->label_raw=read_boolean_param("labelraw");


/***************
* bounding box *
***************/
param->bbox_x0=read_double_param("bbox_x0");
param->bbox_y0=read_double_param("bbox_y0");
param->width   =read_double_param("width"  );
param->margin =read_double_param("margin" );

det=param->teldef->det[param->frame_level];

param->bbox_x1 = param->bbox_x0 + param->width;
param->bbox_y1 = param->bbox_y0 + param->width * (double)(det->npixels_y)/
                                                 (double)(det->npixels_x) ;

/***************************************************
* detector to PostScript coordinate transformation *
***************************************************/
scalex=(param->bbox_x1 - param->bbox_x0 - 2.0*param->margin )/
       (double)(det->npixels_x);

scaley=(param->bbox_y1 - param->bbox_y0 - 2.0*param->margin )/
       (double)(det->npixels_y);

param->det2ps=allocateXform2d();
setXform2dToScaling(param->det2ps,scalex,scaley, 0.0, 0.0 );


applyXform2dToContinuousCoords(param->det2ps, &x, &y, det->min_x, det->min_y );

applyTranslationToXform2d(param->det2ps, param->bbox_x0 + param->margin - x,
                                         param->bbox_y0 + param->margin - y );
                    

/***************
* grid spacing *
***************/
param->max_grid_lines=read_int_param("maxngrid");

/************
* font info *
************/
read_string_param("font",param->font,FILENAME_DIMEN);
param->font_leeway=read_double_param("fontborder");

param->arrow_length=read_double_param("arrowlength");

/******************
* spacecraft axes *
******************/
param->draw_scx = read_boolean_param("draw_scx");
param->draw_scy = read_boolean_param("draw_scy");
param->draw_scz = read_boolean_param("draw_scz");

param->sc_length = read_double_param("sc_length");
param->sc_length *= param->teldef->det[param->teldef->sky_from]->npixels_x;

param->sc_originx = read_double_param("sc_originx");
param->sc_originy = read_double_param("sc_originy");

param->min_length=5.0; /* points */

param->history = read_boolean_param("history");

return(param);
} /* end of readParam function */

/******************************************************************************
*******************************************************************************
* free all the memory associated with the PARAM structure.
* note this also closes the outfile 
******************************************************************************/
void destroyParam(PARAM* param) {

    destroyXform2d(param->det2ps);
    destroyTelDef(param->teldef);
    fclose(param->fp);

    free(param);

} /* end of destroyParam function */
