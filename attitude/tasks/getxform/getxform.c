#include <stdlib.h>
#include <string.h>
#include <math.h>

#define TOOLSUB getxform
/* fatal.h / headas_main() require that TOOLSUB be defined first */


#include "headas.h"
#include "coordfits.h"
#include "param.h"
#include "att_fatal.h"

#include "headas_main.c"

void writeSkyQuaternion (PARAM * param);

/****************************************************************************
*****************************************************************************
* transform to a FITS image from one coordinate system to another.
****************************************************************************/
void TOOLSUBAUX(void) {

PARAM* param;


set_toolversion("0.0.0");

/**********************
* read the parameters *
**********************/
param=readParam();

if(param==NULL) {
    fprintf(stderr, "Could not read parameters\n");
    att_fatal(1);
}

if(param->combo == NULL) {
    fprintf(stderr, "Could not determine transform\n");
    att_fatal(1);
}
/***********************************
* print the transform for the user *
***********************************/
headas_chat(1, "Extracted the following transform:\n");
headas_chat(1, "x' = %g * x + %g * y + %g\n",
            param->combo->trans->rot[0][0],
            param->combo->trans->rot[0][1],
            param->combo->trans->xshift);

headas_chat(1, "y' = %g * x + %g * y + %g\n",
            param->combo->trans->rot[1][0], 
            param->combo->trans->rot[1][1], 
            param->combo->trans->yshift);

if(param->combo->map) {
    headas_chat(1, "...with non-linear corrections\n");
}


/*************************************
* write the transform to a FITS file *
*************************************/
writeComboXform(param->combo, param->outfile);


/*************************************
* write the transform to a FITS file *
*************************************/
if (param->quat)
	writeSkyQuaternion(param);


/********************************************
* write the parameter list to the primary+0 *
********************************************/
parstamp_path(param->outfile);


} /* end of main function */


void writeSkyQuaternion (PARAM * param)
{
	fitsfile * fptr;
	int status = 0;

	fits_open_file(&fptr, param->outfile, READWRITE, &status);
	if (status) {
		fprintf(stderr, "Unable to open %s to write sky quaternion [%d]\n",
				param->outfile, status);
		att_fatal(1);
	}
	fits_update_key_dbl(fptr, "SKYTIME", param->time, 15, "Time of SWXQn", &status);
	fits_update_key_dbl(fptr, "SWXQ0", param->quat->p[0], 15, "Attitude at SKYTIME", &status);
	fits_update_key_dbl(fptr, "SWXQ1", param->quat->p[1], 15, "Attitude at SKYTIME", &status);
	fits_update_key_dbl(fptr, "SWXQ2", param->quat->p[2], 15, "Attitude at SKYTIME", &status);
	fits_update_key_dbl(fptr, "SWXQ3", param->quat->p[3], 15, "Attitude at SKYTIME", &status);

	if (status) {
		fprintf(stderr, "Unable to write sky quaternion [%d]\n", status);
		att_fatal(1);
	}
}

