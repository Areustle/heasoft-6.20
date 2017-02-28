#include "headas.h"
#define TOOLSUB attcombine
#include "att_fatal.h"
#include "headas_main.c"

#include "param.h"
#include "coordfits.h"



static int
initialize_output_pointing (ATTFILE * first, ATTOUT * combined, int * colnum)
{
    int status = 0;
    int create = TRUE;
    int outcols;
    int in_pointing;
    int out_pointing;
    char key[FLEN_KEYWORD];

    if (fits_get_colnum(first->fp, CASEINSEN, "POINTING",
			&in_pointing, &status))
        headas_chat(0, "unable to locate POINTING [%d]\n", status);

    else if (fits_get_num_cols(combined->fp, &outcols, &status))
        headas_chat(0, "unable to get number of columns [%d]\n", status);

    else if (!(out_pointing = outcols + 1)) {
        headas_chat(0, "non-positive column number\n");
        return 1;
    }

    else if (fits_insert_rows(combined->fp, 0, first->nrows, &status))
        headas_chat(0, "unable to insert output rows [%d]\n", status);

    else if (fits_copy_col(first->fp, combined->fp, in_pointing,
                  out_pointing, create, &status))
        headas_chat(0, "unable to create POINTING [%d]\n", status);

    else if (!sprintf(key, "TTYPE%d", out_pointing))
        headas_chat(0, "unable to format TTYPEn\n");

    else if (fits_copy_col(first->fp, combined->fp, in_pointing,
                  out_pointing + 1, create, &status))
        headas_chat(0, "unable to create POINTING_ORIG [%d]\n", status);

    else if (!sprintf(key, "TTYPE%d", out_pointing + 1))
        headas_chat(0, "unable to format TTYPEn\n");

    else if (fits_update_key_str(combined->fp, key, "POINTING_ORIG", 0, &status))
        headas_chat(0, "unable to name POINTING_ORIG [%d]\n", status);

    else
        headas_chat(1, "initialized POINTING and POINTING_ORIG columns\n");


    *colnum = out_pointing;

    return status;
}




void attcombine_aux (void)
{

PARAM* param;

ATTFILE* first;
ATTFILE* second;
ATTOUT* combined;

QUAT* q1;
QUAT* q2;
QUAT* q;

long row;
double time;
int out_pointing = 0;

/****************************
* read the input parameters *
****************************/
param = readParam();
if (!param) {
    headas_chat(0, "unable to load parameters\n");
    att_fatal(1);
}

/*****************
* open the files *
*****************/
first = openAttFile(param->first);
if(first==NULL) {
    headas_chat(0, "could not open %s\n", param->first);
    att_fatal(1);
}


second = openAttFile(param->second);
if(second==NULL) {
    headas_chat(0, "could not open %s\n", param->second);
    att_fatal(1);
}

combined = createAttOut(param->combined);
if(combined==NULL) {
    headas_chat(0, "could not create %s\n", param->combined);
    att_fatal(1);
}

setAttFileInterpolation(second, param->interpolation);


/* initialize bonus output columns if alignment provided */
if (param->align)
    if (initialize_output_pointing(first, combined, &out_pointing)) {
        headas_chat(0, "unable to initialize output POINTING\n");
        att_fatal(1);
    }


/***********************************************
* loop over the rows in the first attitude file
* applying rotations from the second
***********************************************/
q1 = allocateQuat();
q2 = allocateQuat();
q  = allocateQuat();

for (row = 1; row <= first->nrows; ++row) {

    time = readTimeFromAttFile(first, row);
    readQuatFromAttFile(first, q1, row);

    findQuatInAttFile(second, q2, time);

    /************************************************
    * combine the quaternions and write the results *
    ************************************************/
    productOfQuats(q, q1, q2);
    addAttOutRow(combined, time, q);

    if (out_pointing) {
        int status = 0;
        double ra, dec, roll, pointing[3];
        convertQuatToRADecRoll(param->align, q, &ra, &dec, &roll);
        pointing[0] = ra;
        pointing[1] = dec;
        pointing[2] = roll;
        fits_write_col_dbl(combined->fp, out_pointing, row, 1, 3,
                 pointing, &status);
        if (status)
            headas_chat(0, "unable to write updated row %ld pointing [%d]\n",
                       row, status);
    }
}

/*************************
* HEADAS parameter stamp *
*************************/
{
int status=0;
HDpar_stamp(combined->fp, 0, &status);
if(status) fits_report_error(stdout, status);
}

/**************
* close files *
**************/
closeAttFile(first);
closeAttFile(second);
closeAttOut(combined);


destroyQuat(q1);
destroyQuat(q2);
destroyQuat(q);


} /* end of attdump function */

