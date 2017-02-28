#include <stdlib.h>
#include <string.h>
#include <math.h>

#define TOOLSUB applyxform
/* fatal.h / headas_main() require that TOOLSUB be defined first */


#include "headas.h"
#include "coordfits.h"
#include "param.h"
#include "att_fatal.h"

#include "headas_main.c"

/****************************************************************************
*****************************************************************************
* transform to a FITS image from one coordinate system to another.
****************************************************************************/
void TOOLSUBAUX(void) {

PARAM* param;

double oldx, oldy;
double newx, newy;


set_toolversion("0.0.0");

/**********************
* read the parameters *
**********************/
param=readParam();

if(param==NULL) {
    fprintf(stderr, "Could not read parameters\n");
    att_fatal(1);
}

while(fscanf(param->fpin, "%lf %lf", &oldx, &oldy) == 2) {

    applyComboXform(param->combo, &newx, &newy, oldx, oldy);
    
    fprintf(param->fpout, param->format, oldx, oldy, newx, newy);

} /* end of loop over coordinates */

fflush(param->fpout);


} /* end of main function */

