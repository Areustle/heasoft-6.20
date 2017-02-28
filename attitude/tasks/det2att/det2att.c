/*
 * $Source: /headas/headas/attitude/tasks/det2att/det2att.c,v $
 * $Revision: 1.5 $
 * $Date: 2005/10/11 20:32:17 $
 *
 * 	Tool for translating detector coordinate shifts into attitude
 * 	file corrections.
 *
 * $Log: det2att.c,v $
 * Revision 1.5  2005/10/11 20:32:17  rwiegand
 * Updated to produce quaternions consistent with the telescope definition.
 *
 */

#include "headas.h"
#define TOOLSUB det2att
#include "att_fatal.h"
#include "headas_main.c"

#include "param.h"
#include "infile.h"
#include "coordfits.h"



static void applyQuatToVector (QUAT * quat,
				double out[3], double in[3], ROTMATRIX * rm)
{
	convertQuatToRotMatrix(rm, quat);
	applyRotMatrixToVector(rm, out, in);
}




void det2att_aux (void) {

PARAM* param;

INFILE* infile;
double time;
double deltax, deltay;

TELDEF* teldef;

double centerx, centery;
XFORM2D* trans;
QUAT* delta_q;

ATTOUT* out;

/****************************
* read the input paramaters *
****************************/
param = readParam();

/**********************
* open the input file *
**********************/
infile = openInfile(param->infile);
if(infile==NULL) {
    fprintf(stderr, "Can't open infile %s\n", param->infile);
    att_fatal(1);
}

/***********************
* read the teldef file *
***********************/
teldef = readTelDef(param->teldef);
if(teldef==NULL) {
    fprintf(stderr, "Can't open teldef %s\n", param->teldef);
    att_fatal(1);
}


/***********************
* open the output file *
***********************/
out = createAttOut(param->outfile);
if(out==NULL) {
    fprintf(stderr, "Can't open output file %s\n", param->outfile);
    att_fatal(1);
}

/***********************
* allocate some things *
***********************/
trans = allocateXform2d();
delta_q = allocateQuat();

centerx = teldef->det[teldef->sky_from]->center_x;
centery = teldef->det[teldef->sky_from]->center_y;

/***************************************
* loop over the rows of the input file *
****************************************/
while(readInfileValues(infile, &time, &deltax, &deltay) ) {

    headas_chat(5,"%g %g %g\n", time, deltax, deltay);

    setXform2dToTranslation(trans, -deltax, -deltay);

    convertXform2dToQuat(delta_q, trans, centerx, centery,
                                         centerx, centery,
                         teldef->sky_pix_per_radian);
                         
#ifdef DEBUG
    {
    EULER* e;
    e = allocateEuler();
    convertQuatToEuler(e, delta_q);
    printf("phi=%g theta=%g psi=%g\n", e->phi, e->theta, e->psi);
    destroyEuler(e);
    }
#endif

	{
		ROTMATRIX rm;
		double out[3];
		applyQuatToVector(teldef->alignment->q, out, delta_q->p, &rm);
		delta_q->p[0] = out[0];
		delta_q->p[1] = out[1];
		delta_q->p[2] = out[2];
	}

    addAttOutRow(out, time, delta_q);

}

/*************************
* HEADAS parameter stamp *
*************************/
{
int status=0;
HDpar_stamp(out->fp, 0, &status);
if(status) fits_report_error(stderr, status);
}

closeInfile(infile);
closeAttOut(out);



} /* end of attdump function */
