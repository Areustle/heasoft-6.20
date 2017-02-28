#include <math.h>

#include "headas.h"
#define TOOLSUB draw_teldef
#include "headas_main.c"
#include "att_fatal.h"

#include "coordfits.h"
#include "postscript.h"
#include "param.h"

#define LABEL_DIMEN 256

/* clip any drawing outside the selected detector corodinates */

#define CLIP


/*
#define DEBUG
*/



/************************************************************************
* this helper function makes it easy to convert any detector
* coordinates to the frame detector coordinates
************************************************************************/
void transform_det(PARAM* param, int level,  double* x1, double* y1,
                   double x0, double y0 ) {

    convertDetectorToDetectorUsingTelDef(param->teldef, 
                                         level, param->frame_level, 
                                         x1, y1, x0,y0 );

}

/**************************************************************************
* This is a another helper function to make it easy to convert 
* raw coordinates to the frame detector coordinates
**************************************************************************/
void transform_raw(PARAM* param, double* x1, double* y1, 
               int seg, double x0, double y0) {

    double xtmp, ytmp;

    convertContRawToDetectorUsingTeldef(param->teldef, &xtmp, &ytmp,
                                        seg, x0, y0);

    convertDetectorToDetectorUsingTelDef(param->teldef, 0, param->frame_level, 
                                         x1, y1, xtmp,ytmp );

}

/***************************************************************************
* draw a spacecraft axis 
***************************************************************************/
void draw_spacecraft_axis(PARAM* param, ROTMATRIX* rot,
                          double originx, double originy,
                          int index, char* label) {
                     
    double sc_vector[3];
    double det_vector[3];
    TELDEF* cal;
    
    double endx, endy;
    double length;
    
    cal = param->teldef;

    sc_vector[0] =0.0;
    sc_vector[1] =0.0;
    sc_vector[2] =0.0;
    
    sc_vector[index] = param->sc_length;


    applyRotMatrixToVector(rot, det_vector, sc_vector );

    transform_det(param, cal->sky_from, &endx, &endy,
                det_vector[0], det_vector[1]);

    endx -= originx;
    endy -= originy;

    length = sqrt(endx*endx + endy*endy);

    endx += param->sc_originx;
    endy += param->sc_originy;


    if(length >= param->min_length) {
        draw_line(param, param->sc_originx, param->sc_originy, endx, endy);
        generic_label(param, endx, endy, "ontop", label);
    }

}


/*****************************************************************************
* makes a PostScript drawing of the coordinate systems in a teldef file
*****************************************************************************/
void draw_teldef_aux(void) {

PARAM* param;
TELDEF* cal;

char label[LABEL_DIMEN];
char* plural[]={"","s"};
double scalex, scaley;

int seg;
int i, j;
int nx, ny;

int level;

COORDDEF* raw;
COORDDEF* det;

double rawx0, rawy0;
double rawx1, rawy1;
double detx0, dety0;
double detx1, dety1;
double detx2, dety2;

/*****************
* initialization *
*****************/
set_toolversion("0.1");
param=readParam();
cal=param->teldef;
det=cal->det[param->frame_level];

postscript_preamble(param);

/******************
* raw coordinates *
******************/

#ifdef CLIP
begin_clipping(param,det->min_x, det->min_y, det->max_y, det->max_y);
#endif /* CLIP */

for(seg=cal->min_segment; seg<cal->nsegments; ++seg) {

    raw=cal->raw[seg];
    write_comment(param,"segment");

    #ifdef DEBUG
    printf("read_teldef: seg=%d min_x=%g min_y=%g max_x=%g max_y=%g\n",
           seg,raw->min_x, raw->min_y, raw->max_x, raw->max_y);
    printf("    first_pixel_x=%g first_pixel_y=%g\n",
           raw->first_pixel_x, raw->first_pixel_y);
    #endif /* DEBUG */

    if(cal->raw_corrections_needed) {
        /******************
        * draw each pixel *
        ******************/
        nx=raw->npixels_x + 1;
        ny=raw->npixels_y + 1;
        while(nx > param->max_grid_lines || ny > param->max_grid_lines) {
            nx=(nx-1)/2+1;
            ny=(ny-1)/2+1;
        }
    } else {
        /********************
        * just draw outline *
        ********************/
        nx=2;
        ny=2;
    }


    /*******************
    * horizontal lines *
    *******************/
    for(j=0;j<ny;++j) {


        rawy1=rawy0 = (double)j/(double)(ny-1) * (double)raw->npixels_y  
                      + raw->min_y;
        rawx1=raw->min_x;

        #ifdef DEBUG
        printf("horizontal line j=%d rawx1=%g, rawy1=%g\n",j, rawx1, rawy1);
        #endif /* DEBUG */


        for(i=1;i<nx;++i) {


            rawx0=rawx1;
            rawx1=(double)i/(double)(nx-1) * (double)raw->npixels_x  
                   + raw->min_x;

            transform_raw(param, &detx0, &dety0, seg, rawx0, rawy0);
            transform_raw(param, &detx1, &dety1, seg, rawx1, rawy1);


            #ifdef DEBUG
            printf("draw_teldef: seg=%d i=%d j=%d raw0 (%g %g) raw1 (%g %g)\n",
                   seg,i,j, rawx0,rawy0, rawx1,rawy1);

            printf("draw_teldef: seg=%d i=%d j=%d det0 (%g %g) det1 (%g %g)\n",
                   seg,i,j, detx0,dety0, detx1,dety1);
            #endif /* DEBUG */


            draw_line(param,detx0, dety0, detx1, dety1);

        }
    } /* end of loop over horizontal lines */

    /*****************
    * vertical lines *
    *****************/
    for(i=0;i<nx;++i) {

        rawx0=rawx1=(double)i/(double)(nx-1) * (double)raw->npixels_x  
              + raw->min_x;


        rawy1=raw->min_y;

        for(j=1;j<nx;++j) {

            rawy0=rawy1;
            rawy1=(double)j/(double)(ny-1) * (double)raw->npixels_y  
                          + raw->min_y;

            transform_raw(param, &detx0, &dety0, seg, rawx0, rawy0);
            transform_raw(param, &detx1, &dety1, seg, rawx1, rawy1);

            draw_line(param,detx0, dety0, detx1, dety1);

        }
    } /* end of loop over horizontal lines */

    /******************
    * label the axies *
    ******************/
    if(param->label_raw) {
        if(cal->raw_corrections_needed) {
            /************************************************
            * coordinate are non-linear, so just give scale *
            ************************************************/
            scalex=(double)(raw->npixels_x)/(double)(nx-1);
            scaley=(double)(raw->npixels_y)/(double)(ny-1);

            if(scalex==scaley) {
                sprintf(label,"%s and %s, %g pixel%s per line",
                        raw->name_x, raw->name_y, scalex, plural[scalex>1] );
            } else {
                sprintf(label,"%s, %g pixel%s per line and %s, %g pixel%s/line",
                        raw->name_x, scalex, plural[scalex>1],
                        raw->name_y, scaley, plural[scaley>1] );
            }

            generic_label(param,det->min_x, det->max_y, "below", label);

        } else {
            /*********************
            * label linear axies *
            *********************/
            if(raw->npixels_x >1 && raw->npixels_y >1) {

                transform_raw(param, &detx0,&dety0, seg, raw->min_x,raw->min_y);
                transform_raw(param, &detx1,&dety1, seg, raw->max_x,raw->min_y);
                transform_raw(param, &detx2,&dety2, seg, raw->min_x,raw->max_y);

/*
                if(detx0 >= det->min_x && detx0 <= det->max_x &&
                   dety0 >= det->min_y && dety0 <= det->max_y   ) {
*/
                    /**************************************
                    * raw origin is inside detector axies *
                    **************************************/
                    label_axes(param, detx0,dety0, detx1,dety1, detx2,dety2,
                               raw->name_x, raw->name_y, 1/*inside*/);

              /*      } */ /* end if origin is inside detector space */

            } /* end if there is more than one pixel */
        } /* end if axes are linear */
    } /* end if we are labeling the raw coordinates */

    /***************************
    * label the segment number *
    ***************************/
    if(param->label_raw && cal->nsegments>1) {

        #ifdef DEBUG
        printf("seg=%d raw center at (%g, %g)\n",
               seg, raw->center_x, raw->center_y);
        #endif /* DEBUG */
        
        transform_raw(param, &detx0,&dety0, seg, raw->center_x, raw->center_y);

        sprintf(label,"%s %d",cal->seg_col_name, seg);
        generic_label(param, detx0, dety0, "ontop center", label);
    } /* end if we need to label segments */

} /* end of loop over segments */

#ifdef CLIP
end_clipping(param);
#endif /* CLIP */

/****************************
* draw detector coordinates *
****************************/
for(level=0;level<cal->n_det_levels;++level) {
    det=cal->det[level];

    write_comment(param,"detector bounding box");

    #ifdef DEBUG
    printf("detector level %d\n", level);
    #endif /* DEBUG */


    /*********
    * bottom *
    *********/
    transform_det(param, level, &detx0, &dety0, det->min_x, det->min_y);
    transform_det(param, level, &detx1, &dety1, det->max_x, det->min_y);

    draw_line(param, detx0,dety0, detx1,dety1);

    /********
    * right *
    ********/
    transform_det(param, level, &detx0, &dety0, det->max_x, det->min_y);
    transform_det(param, level, &detx1, &dety1, det->max_x, det->max_y);

    draw_line(param, detx0,dety0, detx1,dety1);

    /******
    * top *
    ******/
    transform_det(param, level, &detx0, &dety0, det->max_x, det->max_y);
    transform_det(param, level, &detx1, &dety1, det->min_x, det->max_y);

    draw_line(param, detx0,dety0, detx1,dety1);

    /*******
    * left *
    *******/
    transform_det(param, level, &detx0, &dety0, det->min_x, det->max_y);
    transform_det(param, level, &detx1, &dety1, det->min_x, det->min_y);

    draw_line(param, detx0,dety0, detx1,dety1);


    /*******
    * axes *
    *******/
    transform_det(param, level, &detx0, &dety0, det->min_x, det->min_y);
    transform_det(param, level, &detx1, &dety1, det->max_x, det->min_y);
    transform_det(param, level, &detx2, &dety2, det->min_x, det->max_y);

    label_axes(param, detx0,dety0, detx1,dety1, detx2,dety2, 
               det->name_x, det->name_y, 0/*outside*/);

} /* end of loop over detector coordinates */

#ifdef DEBUG
printf("about to add title\n");
#endif /* DEBUG */


/******************
* spacecraft axes *
******************/
if(param->draw_scx || param->draw_scy || param->draw_scz) {

    double originx;
    double originy;

    ROTMATRIX* rot;
    /* unused
    double endx, endy;
    double  sc_vector[3];
    double det_vector[3];
    
    double length;
    */
    
    rot = allocateRotMatrix();
    convertQuatToRotMatrix(rot, cal->alignment->q_inverse);

    transform_det(param, cal->sky_from, &originx, &originy, 0.0, 0.0);


    if(param->draw_scx) {
        draw_spacecraft_axis(param, rot, originx, originy, 0, "SC_X");
    }

    if(param->draw_scy) {
         draw_spacecraft_axis(param, rot, originx, originy, 1, "SC_Y");
    }

    if(param->draw_scz) {
         draw_spacecraft_axis(param, rot, originx, originy, 2, "SC_Z");
    }

    destroyRotMatrix(rot);
} /* end if we are drawing spacecraft axes */



/**************
* add a title *
**************/
sprintf(label,"%s %s Teldef File %s\n",
        cal->mission, cal->instrument, cal->filename);
add_title(param, label);

#ifdef DEBUG
printf("about to clean up\n");
#endif /* DEBUG */


/***********
* clean up *
***********/
postscript_trailer(param);
destroyParam(param);

#ifdef DEBUG
printf("done\n");
#endif /* DEBUG */


} /* end of draw_teldef function */
