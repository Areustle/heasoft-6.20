#include "methods.h"
#include "image.h"
#include "att_fatal.h"
#include "coord.h"
#include "headas.h"

/********************************************************************************
*
********************************************************************************/
void transform_by_interpolate(IMAGE* original, IMAGE* transformed, PARAM* param) {

    int i,j;
    double x,y;
    double value;

    /***************************
    * transform all the pixels *
    ***************************/
    for(j=0; j<transformed->dimeny;  ++j) {

        headas_chat(4, "Transforming row %d\n", j);
        for(i=0; i<transformed->dimenx; ++i) {

            applyComboXform(param->combo, &x, &y, (double)i, (double)j );

            /************************************************
            * write the interpolated value to the new image *
            ************************************************/
            value = interpolateImagePixel(original, x,y);
            setImagePixel(transformed, i,j, value);

            headas_chat(5, "from %g %g to %d %d value=%g\n", x,y, i, j, value);
        }
    }

} /* end of transform_by_interpolate function */
