#ifndef XRSIMAGE_PARAM_INCLUDED

#include "atFunctions.h"
#include "teldef.h" 
/* #include "xform2d.h" */
#include "fitsio.h" 

#define FILENAME_LENGTH 128
#define UNKNOWN_IMAGE_TYPE 101

typedef struct {

char image_type[FILENAME_LENGTH];
int isDetImage;
int isFocImage;
int isSkyImage;

char event_ext[FLEN_VALUE];

char  event_file[FILENAME_LENGTH];
char teldef_file[FILENAME_LENGTH];
char    out_file[FILENAME_LENGTH];

char time_col_name[FLEN_VALUE];

/***************
* WCS keywords *
***************/
double crpix[2];
double crval[2];
double cdelt[2];

/********************************************
* raw coordinates of the single XRS pixels 
* This info taken from the teldef file 
*******************************************/
int rawx;
int rawy;

} PARAM;


/*****************************************************************************
******************************************************************************
* create a new param structure and read parameters, mostly from the parfile
*****************************************************************************/
PARAM* readParam(int *iserror);

/***************************************************************************
****************************************************************************
* determine the WCS image scaling parameters. First try reading these from the 
* event file header. If they aren't there, then redetermine them from the
* teldef file values.
* Note that for sky images this
*****************************************************************************/
int readImageScaleParams(PARAM* param, fitsfile* fp, TELDEF* cal);

#define XRSIMAGE_PARAM_INCLUDED
#endif /* XRSIMAGE_PARAM_INCLUDED */
