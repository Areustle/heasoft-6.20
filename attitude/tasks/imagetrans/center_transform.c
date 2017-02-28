#include <math.h>

#include "methods.h"
#include "image.h"
#include "att_fatal.h"
#include "coord.h"
#include "headas.h"


/******************************************************************************
* The "center" transform method moves all of the counts associated with a
* given input pixel to the output pixel containing the transformed input
* pixel center.
******************************************************************************/
void transform_by_center (IMAGE* original, IMAGE* transformed, PARAM * param) {

int i, j;
double x, y;
int new_i, new_j;
double value;
/***************************
* transform all the pixels *
***************************/
for (j = 0; j < original->dimeny; ++j) {
    headas_chat(4, "Transforming row %d", j);

    for (i = 0; i < original->dimenx; ++i) {
        /*******************************
        * get the transformed position *
        *******************************/
        applyComboXform(param->combo, &x, &y, i, j);

        new_i = floor(x + 0.5);
        new_j = floor(y + 0.5);

        /****************************
        * transform the pixel value *
        ****************************/
        value = getImagePixel(original, i, j);
        incrementImagePixel(transformed, new_i, new_j, value);

        /*************************
        * report what's going on *
        *************************/
        headas_chat(5, "from %d %d to %g %g value=%g\n", i, j, x, y, value);

    }
}


}  /* end of transform_by_center function */

