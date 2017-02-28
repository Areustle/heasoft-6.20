#ifndef XIMAGEXFORM_PARAM_INCLUDED
#define XIMAGEXFORM_PARAM_INCLUDED

#define FILENAME_LENGTH 512


#include "coordfits.h"
#include "comboxform.h"


/***************
* method codes *
***************/
#define     UNKNOWN_METHOD 0
#define      EVENTS_METHOD 1
#define INTERPOLATE_METHOD 2
#define        BBOX_METHOD 3
#define      CENTER_METHOD 4
#define        AREA_METHOD 5
#define        FLAT_METHOD 6
#define        FLAG_METHOD 7
#define  INTERALPHA_METHOD 8


typedef struct {

char  infile[FILENAME_LENGTH];
char outfile[FILENAME_LENGTH];

int method;

int dimenx; /* dimensions of */
int dimeny; /* the new image */

COMBOXFORM* combo;


int seed;   /* random number generator seed */
int history;

int zero_nulls;
int bitpix;

double interAlpha; /* exponent for distance weighting interpolation */
double interEpsilon;

} PARAM;

/**************************************************************************
**************************************************************************
* read the input parameters 
**************************************************************************/
PARAM* readParam(void);

/******************************************************************************
* read and interpret the transform method from the parameter file
******************************************************************************/
int determine_method();

#endif /* XIMAGEXFORM_PARAM_INCLUDED */
