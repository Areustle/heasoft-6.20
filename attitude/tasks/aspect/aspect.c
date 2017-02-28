/****************************************************************************
* This program calculates the nominal aspect point for an  attitude file.
****************************************************************************/
#include <math.h>
#include "param.h"
#include "aspectbins.h"

#include "headas.h"
#define TOOLSUB aspect
#include "att_fatal.h"
#include "headas_main.c"


/*
#define DEBUG
*/

extern int writeNewAttitude(fitsfile *, char *, GTIS *, 
			    double, double, double, double q[4]);


/****************************************************************************
* Calculate the mean pointing
****************************************************************************/
void calculate_mean_pointing(ATTFILE* file, QUAT* q, PARAM* param) {

ASPECTBINS* bin;

int iteration=0;
int isGood;
double size;

long row;


/**********************************************************
* pick a random quaternion half way through the file to be
* the initial guess at the center of the bin structure.
* REW: this can be troublesome if the selected record is
* at a time outside the stable pointing time.  Therefore,
* when GTIs are provided, initialize the quaternion to a time
* within the first GTI.
***********************************************************/
if (param->gti->current < 0) {
	param->gti->current = 0;
	row=file->nrows/2+1;
	readQuatFromAttFile(file,q,row);
}
else {
	int which = param->gti->n / 2;
	double t = (param->gti->start[which] + param->gti->stop[which]) / 2;
	findQuatInAttFile(file, q, t);
}

/***********************
* set up bin strucutre *
***********************/
bin=allocateAspectBins(param->nbins);

/******************************************************************************
* start with the requested bin size and the initial guess of the nominal
* pointing
* If this doesn't work, increase the bin size until it encompases the peak
* of the attitude solution. This has to occur some time since the bin structure
* can grow to cover the entire sphere
******************************************************************************/
isGood=0;
size=param->bin_size;
iteration=0;
while(!isGood && iteration < param->max_iterations) {

    headas_chat(2, "Iteration %d bin size=%g\n", iteration, size);

    resetAspectBins(bin,q,size);
    fillAspectBins(bin, file, param->max_rotation, param->gti);
    isGood=findPeakOfAspectBins(q,bin);

    if(!isGood) {
        /************************
        * increase the bin size *
        ************************/
        size*=bin->dimen;
        if(size*bin->dimen>M_PI) {
            /********************************************************
            * limit the bin size to half the sky 
            * anything larger is wasted, since that's all we can
            * fit into a single tangent plane projection
            *******************************************************/
            size=M_PI/bin->dimen;
            ++iteration;
        }

    }

} /* end of increasing bin size iterations */


/*************************************************************************
* give a warning message if we exceeded the maximum number of iterations *
*************************************************************************/
if(iteration >= param->max_iterations) {
    fprintf(stderr, "\nWarning: Exceeded the maximum number of iterations (%d) at the maximum\n",
            param->max_iterations);

    fprintf(stderr,"bin size (%g). This can happen if the attitude pauses at multiple\n", size*180./M_PI);

    fprintf(stderr,  "points widely distributed over the sky.\n\n");
    fprintf(stderr,"The following results are unreliable:\n\n");
}

/*****************************************************************
* now reduce the bin size and zero in on the aspect peak
* Note if we found the solution in the first try above this 
* loop will not be executed
*****************************************************************/
while(param->bin_size != bin->size ) {

    /************************
    * decrease the bin size *
    ************************/
    size /= bin->dimen/2;
    if(size < param->bin_size ) size = param->bin_size;


    resetAspectBins(bin,q,size);
    fillAspectBins(bin,file,param->max_rotation, param->gti);
    findPeakOfAspectBins(q,bin);

}



} /* end of calculate_mean_pointing function */


/****************************************************************************
* This is the main FTOOL function
****************************************************************************/
void aspect_aux(void) {

PARAM* param;
ATTFILE* file;



QUAT* q;
EULER* e;
double ra,dec,roll;
int status = 0;


/**************************************************
* allocate space for quaternians and euler angles *
**************************************************/
q=allocateQuat();
e=allocateEuler();

/***************************
* get the input parameters *
***************************/
param=readParam();

/************************************************
* get the ATTFILE structure from the parameters *
************************************************/
file=param->att;

/***************************************************************
* calculate the mean pointing, catching special cases
* like if there is only one row in the attitude file
***************************************************************/
if(file->nrows>1) calculate_mean_pointing(file, q, param);
else if(file->nrows==1) readQuatFromAttFile(file, q, 1l);
else {
    fprintf(stderr, "Attitude file has no rows\n");
    att_fatal(1);
}

/*************************************
* output the spacecraft Euler angles *
*************************************/
convertQuatToEuler(e,q);
writeEulerToParfile(e);
headas_chat(1, "Spacecraft Euler Angles: Phi=%.5f Theta=%.5f Psi=%.5f\n",
       e->phi  *180./M_PI,
       e->theta*180./M_PI,
       e->psi  *180./M_PI);
       
/*****************************************
* ... and the realigned RA, Dec and Roll *
*****************************************/
convertQuatToRADecRoll(param->align, q, &ra, &dec, &roll);

writeRaDecRollToParfile(ra,dec,roll);
headas_chat(1, "Telescope pointing: R.A.=%.5f Dec=%.5f Roll=%.5f degrees\n",
       ra,dec,roll);

status = writeNewAttitude(file->fp, param->newattfile, param->gti, 
			  ra, dec, roll, q->p);
if (status) att_fatal(status);


} /* end of aspect "main" function */
