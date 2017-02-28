
#include "methods.h"
#include "headas.h"
#include "att_fatal.h"

/*****************************************************************************
*
******************************************************************************/
void transform_flag_area (IMAGE* original, IMAGE*transformed, PARAM * param)
{
    int i,j;
    long value;


    /*****************************************
    * loop over pixels in the original image *
    *****************************************/
    for (j = 0; j < original->dimeny; ++j) {
        headas_chat(4, "Transforming row %d", j);

        for (i = 0; i < original->dimenx; ++i) {

            value = (long) getImagePixel(original, i,j);
            distribute_flags(transformed, param->combo, i,j, value);

            /*********************
            * report what we did *
            *********************/
			if (headas_chatpar >= 5)
            	headas_chat(5, "%d %d value=%g\n", i,j, value);

        } /* end of loop over pixels */

    } /* end of loop over rows */

} /* end of transform_flag_area function */


