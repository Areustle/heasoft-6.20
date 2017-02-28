#ifndef IMAGEXFORM_INFO_INCLUDED

#include "coord.h"

typedef struct {

int input_width;

int** image;
int dimenx;
int dimeny;

int blank;
int zero_nulls;

XFORM2D* trans;
MAPXFORM* nonlinear;

} INFO;

/**************************************************************************
***************************************************************************
* allocate the info structure 
**************************************************************************/
INFO* createInfo(int input_width, int dimenx, int dimeny, 
                 XFORM2D* trans, MAPXFORM* nonlinear, int zero_nulls);

#define IMAGEXFORM_INFO_INCLUDED
#endif /* IMAGEXFORM_INFO_INCLUDED */
