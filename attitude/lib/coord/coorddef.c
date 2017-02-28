#include <string.h>
#include "coorddef.h"

/*
#define DEBUG
*/

/****************************************************************************
****************************************************************************
* create a new COORDDEF structure
* strings are initialized to null pointers.
****************************************************************************/
COORDDEF* allocateCoordDef(void) {

COORDDEF* coord;

coord=(COORDDEF*)malloc(sizeof(COORDDEF));

coord->scale_unit=(char*)NULL;
coord->name_x    =(char*)NULL;
coord->name_y    =(char*)NULL;
coord->name      =(char*)NULL;

coord->trans         =allocateXform2d();
coord->inverse_trans=allocateXform2d();

return(coord);

} /* end of allocateCoordDef function */


/****************************************************************************
****************************************************************************
* free memory for a COORDDEF structure
****************************************************************************/
void destroyCoordDef(COORDDEF* coord) {


/***********************************************
* free the strings if they have been allocated *
***********************************************/
if(coord->scale_unit!=NULL) free(coord->scale_unit);
if(coord->name_x    !=NULL) free(coord->name_x    );
if(coord->name_y    !=NULL) free(coord->name_y    );

free(coord->trans);
free(coord->inverse_trans);


/*********************
* free the structure *
*********************/
free(coord);

} /* end of destroyCoordDef function */



/****************************************************************************
****************************************************************************
* sets an arbitrary string in a CoordDef structure. The first argument is
* a pointer to the string in the structure (e.g. coord->scale_unit),
* and the second argument is the string to be copied there.
* The only thing which makes this function specific to COORDDEF strings is 
* that it assumes un-allocated strings are null pointers.
****************************************************************************/
void setStringInCoordDef(char** coord_string, char* string) {
int length;

#ifdef DEBUG
printf("setStringInCoordDef: start\n");
printf("setStringInCoordDef: *coord_string@%d\n",(int)(*coord_string));
#endif /* DEBUG */

/*************************************************************
* free the string memory if it has been previously allocated *
*************************************************************/
if(*coord_string!=NULL) free(*coord_string);

#ifdef DEBUG
printf("setStringInCoordDef: freed\n");
#endif /* DEBUG */


/********************************************
* allocate the string to the correct length *
********************************************/
length=strlen(string)+1;
*coord_string=(char*) malloc(sizeof(char)*length);

#ifdef DEBUG
printf("setStringInCoordDef: allocated\n");
#endif /* DEBUG */


/******************
* copy the string *
******************/
strncpy(*coord_string,string,length);

#ifdef DEBUG
printf("setStringInCoordDef: done\n");
#endif /* DEBUG */


} /* end of setStringInCoordDef function */

/****************************************************************************
*****************************************************************************
* set first_pixel_[x/y] and npixels_[x/y] and calculate the [min/max]_[x/y]
* values in a COORDDEF structure
*****************************************************************************/
void setCoordDefLimits(COORDDEF* coord, double first_pixel_x, int npixels_x,
                                        double first_pixel_y, int npixels_y  ) {

/***************************
* set the dimension values *
***************************/
coord->first_pixel_x=first_pixel_x;
coord->first_pixel_y=first_pixel_y;

coord->npixels_x=npixels_x;
coord->npixels_y=npixels_y;

/*************************
* last pixel coordinates *
*************************/
coord->last_pixel_x = coord->first_pixel_x + coord->npixels_x - 1.;
coord->last_pixel_y = coord->first_pixel_y + coord->npixels_y - 1.;

/**************************
* set the absolute limits *
**************************/
coord->min_x=first_pixel_x-.5;
coord->min_y=first_pixel_y-.5;

coord->max_x=first_pixel_x+(double)npixels_x-.5;
coord->max_y=first_pixel_y+(double)npixels_y-.5;

/***********************
* address space center *
***********************/
coord->center_x = coord->first_pixel_x + 0.5*(double)coord->npixels_x - 0.5;
coord->center_y = coord->first_pixel_y + 0.5*(double)coord->npixels_y - 0.5;


} /* end of setCoordDefLimits function */



/****************************************************************************
*****************************************************************************
* You should call this function after setting the "trans" element of
* a COORDDEF structure and calling setCoordDefLimits.
* This function sets the inverse transform
* and the bounding box for the transformed coordinates.
****************************************************************************/
void setCoordDefTransformInfo(COORDDEF* coord) {

double x,y;
double xmin,xmax,ymin,ymax;

/**********************************
* calculate the inverse transform *
**********************************/
invertXform2d(coord->inverse_trans,coord->trans);

/****************************************
* calculate the bounding box quantities *
****************************************/
applyXform2dToContinuousCoords(coord->trans,&x,&y,coord->min_x,coord->min_y);
xmin=x;
xmax=x;
ymin=y;
ymax=y;

applyXform2dToContinuousCoords(coord->trans,&x,&y,coord->max_x,coord->min_y);
if(x<xmin) xmin=x;
if(x>xmax) xmax=x;
if(y<ymin) ymin=y;
if(y>ymax) ymax=y;

applyXform2dToContinuousCoords(coord->trans,&x,&y,coord->max_x,coord->max_y);
if(x<xmin) xmin=x;
if(x>xmax) xmax=x;
if(y<ymin) ymin=y;
if(y>ymax) ymax=y;

applyXform2dToContinuousCoords(coord->trans,&x,&y,coord->min_x,coord->max_y);
if(x<xmin) xmin=x;
if(x>xmax) xmax=x;
if(y<ymin) ymin=y;
if(y>ymax) ymax=y;

/****************************************
* set the bounding box in the structure *
****************************************/
coord->trans_min_x=xmin;
coord->trans_max_x=xmax;
coord->trans_min_y=ymin;
coord->trans_max_y=ymax;

} /* end of setCoordDefTransformInfo function */


/****************************************************************************
*****************************************************************************
* Adjust the "window" for an existing coordinate system. This changes the 
* number of pixels and offsets the physical location of the first pixel.
* Note that the coordinates of the first pixel in the new coordinate system
* are kept the same (typically 0,0 or 1,1), but the translations are changed
* so that an event which occured at (offset_x, offset_y) in the original
* coordinates will appear in the first pixel of the windowed coordinates.
* if offset_x/y = first_pixel_x/y of the original coordinates then the new
* and old coordinate systems will have the same origin.
****************************************************************************/
void rewindowCoordDef(COORDDEF* coord, double offset_x, double offset_y,
                      int size_x,  int size_y) {

XFORM2D* translation;
XFORM2D* new_xform;

/*****************************
* adjust the coordinate size *
*****************************/
setCoordDefLimits(coord, coord->first_pixel_x, size_x,
                         coord->first_pixel_y, size_y );

/*****************************************************************
* adjust the transform to the next coordinates by preceeding it
* by a translation. I;'m sure there's a more efficient way to
* code this, but it's probably not worth the effort.
****************************************************************/
translation = allocateXform2d();
setXform2dToTranslation(translation, offset_x - coord->first_pixel_x,
                                     offset_y - coord->first_pixel_y );

new_xform=allocateXform2d();
combineXform2ds(new_xform, translation, coord->trans);

destroyXform2d(coord->trans);
coord->trans = new_xform;

destroyXform2d(translation);

/*********************************************************
* set up the rest of the stuff internal to the transform *
*********************************************************/
setCoordDefTransformInfo(coord);


} /* end of rewindowCoordDef function */


void printCoordDef(COORDDEF* coord, FILE* stream)
{
  fprintf(stream, "      first_pixel_x: %g\n", coord->first_pixel_x);
  fprintf(stream, "      first_pixel_y: %g\n", coord->first_pixel_y);
  fprintf(stream, "      npixels_x: %d\n", coord->npixels_x);
  fprintf(stream, "      npixels_y: %d\n", coord->npixels_y);
  fprintf(stream, "      last_pixel_x: %g\n", coord->last_pixel_x);
  fprintf(stream, "      last_pixel_y: %g\n", coord->last_pixel_y);
  fprintf(stream, "      min_x: %g\n", coord->min_x);
  fprintf(stream, "      max_x: %g\n", coord->max_x);
  fprintf(stream, "      min_y: %g\n", coord->min_y);
  fprintf(stream, "      max_y: %g\n", coord->max_y);
  fprintf(stream, "      center_x: %g\n", coord->center_x);
  fprintf(stream, "      center_y: %g\n", coord->center_y);
  fprintf(stream, "      scale_x: %g\n", coord->scale_x);
  fprintf(stream, "      scale_y: %g\n", coord->scale_y);
  fprintf(stream, "      scale_unit: %s\n", coord->scale_unit);
  fprintf(stream, "      name_x: %s\n", coord->name_x);
  fprintf(stream, "      name_y: %s\n", coord->name_y);
  fprintf(stream, "      name: %s\n", coord->name);
  fprintf(stream, "      trans:\n");
  printXform2d(coord->trans, stream);
  fprintf(stream, "      inverse_trans:\n");
  printXform2d(coord->inverse_trans, stream);
  fprintf(stream, "      trans_min_x: %g\n", coord->trans_min_x);
  fprintf(stream, "      trans_max_x: %g\n", coord->trans_max_x);
  fprintf(stream, "      trans_min_y: %g\n", coord->trans_min_y);
  fprintf(stream, "      trans_max_y: %g\n", coord->trans_max_y);
}
