
#include <stdlib.h>
#include "info.h"

/**************************************************************************
***************************************************************************
* allocate the info structure 
**************************************************************************/
INFO* createInfo(int input_width, int dimenx, int dimeny, 
                 XFORM2D* trans, MAPXFORM* nonlinear, int zero_nulls) {

int i;
INFO* info;
info = (INFO*)malloc(sizeof(INFO));

info->input_width = input_width;

/**********************
* allocate image data *
**********************/
info->dimenx = dimenx;
info->dimeny = dimeny;

info->image = (int**)malloc(sizeof(int*)*dimeny);
info->image[0] = (int*)calloc(dimenx*dimeny,sizeof(int)); /* inits to 0 */
for(i=1; i<dimeny; ++i) {
    info->image[i] = info->image[i-1] + dimenx;
}

/********************
* save the transform *
*********************/
info->trans = trans;
info->nonlinear = nonlinear;

info->blank = -1;
info->zero_nulls = zero_nulls;

return info;

} /* end of createInfo function */
