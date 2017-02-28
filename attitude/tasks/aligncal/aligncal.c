#include "headas.h"
#define TOOLSUB aligncal
#include "att_fatal.h"
#include "headas_main.c"

#include "param.h"
#include "points.h"
#include "coordfits.h"

#include <math.h>




void TOOLSUBAUX(void) {

PARAM* param;
TELDEF* teldef;

XFORM2D* xform;
QUAT* q;
QUAT* new_q;
ROTMATRIX* rot;

POINTS* in;

double error;

int status=0;
fitsfile* fpin;
fitsfile* fpout;
int hdu_type;

int i,j;
char key[FLEN_KEYWORD];


/****************************
* read the input paramaters *
****************************/
param = readParam();
teldef = param->teldef;
in     = param->points;

/************************************************
* get the correction to the misalignment matrix *
************************************************/
xform = allocateXform2d();
q     = allocateQuat();

error = findBestQuatXform2d(q, xform, in->x0, in->y0,
                                      in->x1, in->y1,
                                      in->wgt, in->npoints,
                            teldef->sky->center_x, teldef->sky->center_y,
                            teldef->sky->center_x, teldef->sky->center_y,
                            teldef->sky_pix_per_radian,
                            param->tolerance, param->max_iterations);

headas_chat(1, "Residual RMS error %g pixels\n", error);


/*************************************
* calculate the new alignment matrix *
*************************************/
new_q = allocateQuat();
productOfQuats(new_q, teldef->alignment->q_inverse, q);

invertQuatInPlace(new_q);

rot = allocateRotMatrix();
convertQuatToRotMatrix(rot, new_q);


/******************************************
* copy the teldef file to the output file *
******************************************/
fits_open_file(&fpin, teldef->filename, READONLY, &status);
fits_create_file(&fpout, param->outfile, &status);

fits_copy_file(fpin, fpout, 1, 1, 1, &status);

fits_close_file(fpin, &status);
fits_movabs_hdu(fpout, 1, &hdu_type, &status);

if(status) {
    fprintf(stderr, "Error copying teldef file\n");
    fits_report_error(stderr, status);
    return;
}


/******************************
* update the alignment matrix *
******************************/
for(j=0;j<3;++j) {
    for(i=0;i<3;++i) {

        sprintf(key,"ALIGNM%d%d",i+1,j+1);

        fits_update_key_dbl(fpout, key, rot->m[j][i] ,param->decimals,
                            NULL, &status);
    }
}

/************************************
* write HISTORY keywords to outfile *
************************************/
HDpar_stamp(fpout, 0, &status);

fits_close_file(fpout, &status);

if(status) {
    fprintf(stderr, "Error writing updated alignment keywords\n");
    fits_report_error(stderr, status);
    return;
}





} /* end of attdump function */
