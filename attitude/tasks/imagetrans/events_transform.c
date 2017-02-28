#include <math.h>

#include "methods.h"
#include "headas.h"
#include "random.h"
#include "att_fatal.h"


/*******************************************************************************
*
*******************************************************************************/
void transform_by_events(IMAGE* original, IMAGE* transformed, PARAM* param) {

int counts;
int event;

int i,j;
double dx,dy;
double x,y;
int xcoord, ycoord;


/***************************
* transform all the pixels *
***************************/
for (j = 0; j < original->dimeny; ++j) {
    headas_chat(4, "Transforming row %d", j);

    for (i = 0; i < original->dimenx; ++i) {

        /****************************************
        * loop over all the counts in the pixel *
        ****************************************/
        if(imageIsNull(original,i,j) ){
            /*************
            * null pixel *
            *************/
            distribute_value(transformed, param->combo, i,j, NaN);
            headas_chat(5, "%d %d is null\n", i,j);

        } else {
            /****************************************************
            * non-null pixel, transform each event individually *
            ****************************************************/
            counts = (int)getImagePixel(original,i,j);
            for(event=0; event<counts; ++event) {

                /************************************************************
                * note you can't put these "in-line" in the transform
                * function call since the order in which they would be
                * evaluated is machine dependant
                ***********************************************************/
                dx = get_random();
                dy = get_random();

                applyComboXform(param->combo, &x, &y, (double)i+dx, (double)j+dy);

                /******************************************
                * make sure we are inside the image array *
                ******************************************/
                xcoord = floor(x + 0.5);
                ycoord = floor(y + 0.5);

                /***********************************
                * add one to the transformed pixel *
                ***********************************/
                incrementImagePixel(transformed, xcoord, ycoord, 1.0);

                /*********************
                * report what we did *
                *********************/
                headas_chat(5, "%d %d x=%g y=%g counts=%g\n",
                            i,j,x,y, 
                            getImagePixel(transformed,xcoord, ycoord));
            } /* end of loop over events */

        } /* end if the pixel is not null */

    } /* end of loop over pixels */

} /* end of loop over rows */

} /* end of transform_by_events function */
