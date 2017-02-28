#include "headas.h"
#define TOOLSUB attdump
#include "att_fatal.h"
#include "headas_main.c"

#include "coordfits.h"
#include "param.h"



void attdump_aux(void) {

PARAM* param;

ATTFILE* att;

double time;

QUAT* q;
EULER* e;
double ra, dec, roll;

long row;

/***************
* read parfile *
***************/
param=readParam();

/*************************
* open the attitude file *
*************************/
att=openAttFile(param->infile);

/***************************************
* allocate quaternion and Euler angles *
***************************************/
q=allocateQuat();
e=allocateEuler();


for(row=1l; row<=att->nrows; ++row ) {

    time=readTimeFromAttFile(att,row);
    readQuatFromAttFile(att,q,row);

    convertQuatToRADecRoll(param->align, q, &ra, &dec, &roll);
    fprintf(param->fpout, param->format, time, ra, dec, roll);
    

} /* end of loop over attitude file rows */


/***********
* clean up *
***********/
closeAttFile(att);

destroyQuat(q);
destroyEuler(e);


} /* end of attdump function */
