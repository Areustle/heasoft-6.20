#ifndef COORDDEF_INCLUDED

#include "xform2d.h"

/***************************************************************************
****************************************************************************
* The COORDDEF structure holds information defining a single coordinate
* system. In general several of these are included in a teldef structure
***************************************************************************/
typedef struct {

/***********************************************
* coordinates of the center of the first pixel *
***********************************************/
double first_pixel_x;
double first_pixel_y;

/*********************************************
* dimensions of the coordinate address space *
*********************************************/
int npixels_x;
int npixels_y;

/***********************************************
* coordinate of the centers of the last pixels
* last_pixel_x=first_pixel_x+npixels_x-1.
***********************************************/
double last_pixel_x;
double last_pixel_y;

/*************************************************************
* minimum and maximum coordinates - note that these are the
* pixel edges not the centers, so 
* min_x=first_pixel_x-.5 and
* max_x=first_pixel_x+(double)npixels_x-.5
* and similarly for y.
**************************************************************/
double min_x;
double max_x;
double min_y;
double max_y;

/*****************************************************************
* coordinates of the center of the address space
* center_x = first_pixel_x + 0.5*npixels_x - 0.5
*****************************************************************/
double center_x;
double center_y;

/***********************************************
* physical size of given pixel (if meaningful) 
* and a string naming the units of that scale.
***********************************************/
double scale_x;
double scale_y;
char* scale_unit;

/**************************
* event file column names *
**************************/
char* name_x;
char* name_y;

/*********************************************************************
* reference name - usually the three letter designation used in the
* teldef file
*********************************************************************/
char* name;

/**********************************************************************
* the following items deal with the transformation to the
* "next" coordinate system. What "next" means depends on how
* this structure is used.
* Trans is the linear transformation to the next coordinate
* and the other variables give the bounding box of the transformed 
* coordinates
*********************************************************************/

XFORM2D* trans;
XFORM2D* inverse_trans;

double trans_min_x;
double trans_max_x;
double trans_min_y;
double trans_max_y;



} COORDDEF;

/****************************************************************************
****************************************************************************
* create a new COORDDEF structure
* strings are initialized to null pointers.
****************************************************************************/
COORDDEF* allocateCoordDef(void);

/****************************************************************************
****************************************************************************
* free memory for a COORDDEF structure
****************************************************************************/
void destroyCoordDef(COORDDEF* coord);

/****************************************************************************
****************************************************************************
* sets an arbitrary string in a CoordDef structure. The first argument is
* a pointer to the string in the structure (e.g. coord->scale_unit),
* and the second argument is the string to be copied there.
* The only thing which makes this function specific to COORDDEF strings is 
* that it assumes un-allocated strings are null pointers.
****************************************************************************/
void setStringInCoordDef(char** coord_string, char* string);

/****************************************************************************
*****************************************************************************
* set first_pixel_[x/y] and npixels_[x/y] and calculate the [min/max]_[x/y]
* values in a COORDDEF structure
*****************************************************************************/
void setCoordDefLimits(COORDDEF* coord, double first_pixel_x, int npixels_x,
                                        double first_pixel_y, int npixels_y  );


/****************************************************************************
*****************************************************************************
* You should call this function after setting the "trans" element of
* a COORDDEF structure and calling setCoordDefLimits. 
* This function sets the inverse transform
* and the bounding box for the transformed coordinates.
****************************************************************************/
void setCoordDefTransformInfo(COORDDEF* coord);

/**********************************************************************************
* returns the transform from image coordinates to coordef space.
*********************************************************************************/
void setImageToCoordDefXform2D(XFORM2D* trans, COORDDEF* coord,
                               double binx, double biny,
			       double offset_x, double offset_y);

/**********************************************************************************
* returns the transform from coorddef space to pixel coordinates
*********************************************************************************/
void setCoordDefToImageXform2D(XFORM2D* trans, COORDDEF* coord,
                            double binx, double biny,
			    double offset_x, double offset_y);

/*******************************************
 * Print a COORDDEF structure to a stream. 
 *******************************************/
void printCoordDef(COORDDEF* coord, FILE* stream);




#define COORDDEF_INCLUDED
#endif /* COORDDEF_INCLUDED */
