#include <math.h>

#include "methods.h"
#include "headas.h"
#include "att_fatal.h"

/*****************************************************************************
*
******************************************************************************/
void transform_flattened_area(IMAGE* original, IMAGE*transformed, PARAM * param) {

    int i,j;
    double value;
    double weight;

    transformed->weight = allocateSimilarImage(transformed->dimenx,
			transformed->dimeny, transformed, TDOUBLE);


    /*****************************************
    * loop over pixels in the original image *
    *****************************************/
    for (j = 0; j < original->dimeny; ++j) {
        headas_chat(4, "Transforming row %d", j);

        for (i = 0; i < original->dimenx; ++i) {

            value = getImagePixel(original, i,j);
            distribute_value(transformed, param->combo, i, j, value);

            /*********************
            * report what we did *
            *********************/
            headas_chat(5, "%d %d value=%g\n", i, j, value);

        } /* end of loop over pixels */

    } /* end of loop over rows */


    /* loop over outside boundary to update weights of outside pixels */
    for (i = 0; i < transformed->dimenx; ++i) {
        distribute_value(transformed, param->combo, i, -1, 0);
        distribute_value(transformed, param->combo, i, original->dimeny, 0);
    }
    for (j = 0; j < transformed->dimeny; ++j) {
        distribute_value(transformed, param->combo, -1, j, 0);
        distribute_value(transformed, param->combo, original->dimenx, j, 0);
    }


    /*****************************************
    * divide transformed image by weight
    *****************************************/
    headas_chat(3, "dividing through by weight image\n");

    for (j = 0; j < transformed->dimeny; ++j) {

        for (i = 0; i < transformed->dimenx; ++i) {

            value = getImagePixel(transformed, i, j);
            weight = getImagePixel(transformed->weight, i, j);

            if (!isnan(value) && weight != 0) {
                setImagePixel(transformed, i, j, value / weight);
            }
        }
    }

} /* end of transform_by_area function */


