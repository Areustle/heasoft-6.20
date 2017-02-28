#include "fitsio.h"
#include "longnam.h"
#include "atFunctions.h"
#include "teldef.h"
#include "aste_coord.h"

#include "xrsimage.h"
#include "image.h"
#include "xrs_cornercoord.h"

#define HISTOGRAM_TYPE int

#define DEBUG

/****************************************************************************
*****************************************************************************
* iterator work function to histogram counts per pixel
****************************************************************************/
int accumulate_histogram(long total_rows, long offset, long first_row, 
                         long nrows, int ncols, iteratorCol *fits_col,  
                         void* void_info ) 
{

int* counts;
HISTOGRAM_TYPE* pixel;
int row;

counts=(int*)void_info;
pixel=(int*)fits_iter_get_array(fits_col);

for(row=1;row<nrows;++row) {
    ++counts[pixel[row]];
}

return(0);

} /* end of accumulate_histogram iterator work function */


/*****************************************************************************
******************************************************************************
* generate a smoothed image in detector coordinates 
*****************************************************************************/
IMAGE* xrs_det_image(PARAM* param, fitsfile* fp, int* iserror) {

int status=0;
iteratorCol* fits_col;
int ncols;

TELDEF* cal;
COORDDEF* coord; 

HISTOGRAM_TYPE* counts;
int seg;

IMAGE* im;

int corner;
double delta_rawx[4]={-0.5,  0.5, 0.5, -0.5};
double delta_rawy[4]={-0.5, -0.5, 0.5,  0.5};

double detx, dety;

double xpoly[5];
double ypoly[5];

#ifdef DEBUG
printf("xrs_det_image: start\n");
#endif /* DEBUG */


/********************************************************************
* read the teldef file and get the coordinates we are interested in *
********************************************************************/
/*   cal=aste_coord_init(cal->telescop,cal->instrume,param->teldef_file); */
cal=aste_coord_init("","",param->teldef_file);
 if (NULL == cal) {
   fprintf(stderr,
	   "Error; Cannot allocate TELDEF struct.\n%s does not exist?\n",
	   param->teldef_file);
   *iserror=TELDEF_NOT_EXIST;
   return NULL;
 }

#ifdef DEBUG
 printf("xrs_det_image: teldef intialized\n");
#endif /* DEBUG */

 if(param->isDetImage) coord=&cal->mission.aste->det; /* DET */ 
 else                  coord=&cal->mission.aste->foc; /* FOC */
 
#ifdef DEBUG
printf("xrs_det_image: about to read image scale parameters\n");
#endif /* DEBUG */


/*******************************
* get image scaling parameters *
*******************************/
*iserror=readImageScaleParams(param,fp,cal);
 if(*iserror) { return NULL;}

#ifdef DEBUG
printf("xrs_det_image: about to allocate histogram\n");
#endif /* DEBUG */


/*****************************************************************
* allocate space for pixel histogram  and set all counts to zero *
*****************************************************************/
counts=(int*)malloc(sizeof(int)*cal->mission.aste->seg_num);
/* for(seg=0;seg<cal->nsegments;++seg) { */
for(seg=0;seg<cal->mission.aste->seg_num;++seg) {
    counts[seg]=0;
}

#ifdef DEBUG
printf("xrs_det_image: about to set up columns\n");
#endif /* DEBUG */


/***************************************
* set up FITS columns for the iterator *
***************************************/
ncols=1;
fits_col=(iteratorCol*)malloc(sizeof(iteratorCol)*ncols);

fits_iter_set_by_name(fits_col,fp,cal->mission.aste->seg_col,TINT,InputCol);

#ifdef DEBUG
printf("xrs_det_image: about to iterate\n");
#endif /* DEBUG */


/******************************************************
* call the iterator to histogram the counts per pixel *
******************************************************/
fits_iterate_data(ncols,fits_col,
                  0l/*don't skip any rows*/,
                  0l/*read optimum columns per iteration*/,
                  accumulate_histogram,counts,&status);

if(status) {
  fprintf(stderr,"Error accumulating histogram\n");

    fits_report_error(stderr,status);
    *iserror=status;
    return NULL;
}


#ifdef DEBUG
printf("done iterating mission.aste->seg_num=%d\n",cal->mission.aste->seg_num);
#endif /* DEBUG */

/***********************
* create a blank image *
***********************/
im=allocateImage(coord->xsiz, coord->ysiz);
blankImage(im,0);

for(seg=0;seg<cal->mission.aste->seg_num;++seg) {

        /****************************************
        * The image array indices start at zero *
        ****************************************/
/*            xpoly[corner] -= coord->xpix1;  */
/*            ypoly[corner] -= coord->ypix1;  */

    /* ******************** NEW ******************** */
  if(param->isDetImage) {
    xrs_cornercoord(cal, seg, 0, xpoly, ypoly);
  }
  if(param->isFocImage) {
    xrs_cornercoord(cal, seg, 1, xpoly, ypoly);
  }
  /* ******************** NEW  end ******************** */

    /********************
    * paint the polygon *
    ********************/
    incrementConvexPolygonInImage(im,xpoly,ypoly,4/* corners*/,counts[seg]);

} /* end of loop over segments */

 aste_coord_free(cal);
 return(im);

} /* end of xrs_det_image function */
