#ifndef XRSIMAGE_INFO_INCLUDED

#include "param.h"
#include "image.h"

#define TELDEF_NOT_EXIST 100

/***************************************************************************
* this is a structure used to communicate with the iterator work function
* when making sky images
****************************************************************************/
typedef struct {

PARAM* param;
TELDEF* cal;

/****************************
* the image we are painting *
****************************/
IMAGE* im;

} INFO;

/*************************************************************************
**************************************************************************
* create a new INFO structure 
*************************************************************************/
INFO* createInfo(PARAM* param, fitsfile* fp, int* iserror);

#define XRSIMAGE_INFO_INCLUDED
#endif /* XRSIMAGE_INFO_INCLUDED */
