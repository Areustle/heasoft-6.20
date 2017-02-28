#include "xrsimage.h"
#include "info.h"
#include "param.h"
#include "ephemeris.h"
#include "xrs_cornercoord.h"

#define DEBUG

/*****************************************************************************
******************************************************************************
* iterator work function to generate sky coordinate images
*****************************************************************************/
int paint_sky_pixels(long total_rows, long offset, long first_row, 
                     long nrows, int ncols, iteratorCol *fits_col,  
                     void* void_info ) 
{

INFO* info;
PARAM* param;
TELDEF* cal;

int row;
int* seg;
double* time; 
double* centx;
double*  centy;
double*  roll;

 int corner;
double delta_rawx[4]={-0.5,  0.5, 0.5, -0.5};
double delta_rawy[4]={-0.5, -0.5, 0.5,  0.5};

double detx, dety;

double xpoly[5];
double ypoly[5];

/*  #ifdef DEBUG */
/*  printf("paint_sky_pixels: start\n"); */
/*  #endif */ /* DEBUG */ 


/********************************************
* get useful things from the info structure *
********************************************/
info=(INFO*)void_info;

param=info->param;
cal=info->cal;


/*  #ifdef DEBUG */
/*  printf("paint_sky_pixels: got quats and xforms from info\n"); */
/*  #endif */ /* DEBUG */ 

/*************************
* get column data arrays *
*************************/
time=( double*)fits_iter_get_array(fits_col  );
seg=(     int*)fits_iter_get_array(fits_col+1);
centx=(double*)fits_iter_get_array(fits_col+2);
centy=(double*)fits_iter_get_array(fits_col+3);
roll=( double*)fits_iter_get_array(fits_col+4);

/*  #ifdef DEBUG */
/*  printf("paint_sky_pixels: about to loop\n"); */
/*  #endif */ /* DEBUG */ 

/*******************************************
* loop over all rows in the current column *
*******************************************/
for(row=1;row<=nrows;++row) {

        /***********************************
        * calculate pixel corner positions *
        ***********************************/

  xrs_skycornercoord(cal, seg[row], centx[row], centy[row], roll[row], 
		     xpoly, ypoly);

            /****************************************
            * The image array indices start at zero *
            ****************************************/
/*              xpoly[corner] -= cal->mission.aste->sky.xpix1; */
/*              ypoly[corner] -= cal->mission.aste->sky.ypix1; */

/*          } */ /* end of loop over polygon corners */ 

        /********************
        * paint the polygon *
        ********************/
        incrementConvexPolygonInImage(info->im,xpoly,ypoly,4/* corners*/,
                                                           1/*one event*/);


} /* end of loop over rows (events) */

return(0);

} /* end of paint_sky_pixels iterator work function */





/*****************************************************************************
******************************************************************************
* generate a smoothed image in sky coordinates 
*****************************************************************************/
IMAGE* xrs_sky_image(PARAM* param, fitsfile* fp, int *iserror) {

int status=0;
iteratorCol* fits_col;
int ncols;

INFO* info;

/**********************************************************************
* create INFO structure to comunicate with the iterator work function *
**********************************************************************/
 *iserror=0;
info=createInfo(param,fp,iserror);
 if (NULL == info || *iserror) {
   return NULL;
 }

/*********************************************
* get image scaling and aspecting parameters *
*********************************************/
*iserror=readImageScaleParams(param,fp,info->cal);
 if(*iserror) {return NULL;}

/**********************
* set up FITS columns *
**********************/
ncols=5; /* Nuber of columns to use in iteration */
fits_col=(iteratorCol*)malloc(sizeof(iteratorCol)*ncols);

fits_iter_set_by_name(fits_col  ,fp,   param->time_col_name,TDOUBLE,InputCol);
fits_iter_set_by_name(fits_col+1,fp,info->cal->mission.aste->seg_col,TINT   ,InputCol);
fits_iter_set_by_name(fits_col+2,fp,   "X",TDOUBLE,InputCol);
fits_iter_set_by_name(fits_col+3,fp,   "Y",TDOUBLE,InputCol);
fits_iter_set_by_name(fits_col+4,fp,   "ROLL",TDOUBLE,InputCol);


#ifdef DEBUG
printf("xrs_sky_image:about to iterate\n");
#endif /* DEBUG */

/*************************************************
* call the iterator to paint pixels on the image *
*************************************************/
fits_iterate_data(ncols,fits_col,
                  0l/*don't skip any rows*/,
                  0l/*read optimum columns per iteration*/,
                  paint_sky_pixels,info,&status);

if(status) {
  fprintf(stderr,"Error iterating to make image\n");

    fits_report_error(stderr,status);
    *iserror=status;
    return NULL;
}

#ifdef DEBUG
printf("xrs_sky_image: done iterating\n");
#endif /* DEBUG */

return(info->im);

}
