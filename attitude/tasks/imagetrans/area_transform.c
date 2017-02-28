#include <math.h>

#include "methods.h"
#include "headas.h"
#include "att_fatal.h"

/*****************************************************************************
*
******************************************************************************/
void transform_by_area(IMAGE* original, IMAGE*transformed, PARAM * param) {

    int i,j;
    double value;


    /*****************************************
    * loop over pixels in the original image *
    *****************************************/
    for (j = 0; j < original->dimeny; ++j) {
        headas_chat(4, "Transforming row %d", j);

        for (i = 0; i < original->dimenx; ++i) {

            value = getImagePixel(original, i,j);
            distribute_value(transformed, param->combo, i,j, value);

            /*********************
            * report what we did *
            *********************/
            headas_chat(5, "%d %d value=%g\n", i,j, value);

        } /* end of loop over pixels */

    } /* end of loop over rows */

} /* end of transform_by_area function */


