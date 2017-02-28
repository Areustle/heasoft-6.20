#ifndef XIMAGEXFORM_PARAM_INCLUDED

#define FILENAME_LENGTH 128
#define COORD_NAME_LENGTH 4

#include "coordfits.h"
#include "wcs.h"

/***************
* method codes *
***************/
#define   UNKNOWN_METHOD 0
#define    EVENTS_METHOD 1
#define INTENSITY_METHOD 2
#define      BBOX_METHOD 3
#define    CENTER_METHOD 4
#define      AREA_METHOD 5

typedef struct {

char  infile[FILENAME_LENGTH];
char outfile[FILENAME_LENGTH];

TELDEF* teldef;

char from_coord[COORD_NAME_LENGTH];
char   to_coord[COORD_NAME_LENGTH];

int method;

int dimenx; /* dimensions of */
int dimeny; /* the new image */
WCS* wcs;
XFORM2D* trans;
MAPXFORM* nonlinear;

int seed;   /* random number generator seed */
int history;

int copy_hdus;

int zero_nulls;

} PARAM;

/**************************************************************************
**************************************************************************
* read the input parameters 
**************************************************************************/
PARAM* readParam(void);

/******************************************************************************
* read and interpret the transform method from the parameter file
******************************************************************************/
int determineMethod();


/**************************************************************************
**************************************************************************
* determine what transform we should use 
**************************************************************************/
int coord_type(TELDEF* teldef, char* name);

/**************************************************************************
**************************************************************************
* returns >0 if the first coordinate index is "lower" than the second.
**************************************************************************/
int is_upward(int from, int to);

/**************************************************************************
**************************************************************************
* determine what transform we should use 
**************************************************************************/
void determineTransform(PARAM* param);



#define XIMAGEXFORM_PARAM_INCLUDED
#endif /* XIMAGEXFORM_PARAM_INCLUDED */
