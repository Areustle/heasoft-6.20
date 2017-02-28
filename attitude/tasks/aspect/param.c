#include <math.h>
#include <string.h>
#include "param_wrappers.h"
#include "param.h"
#include "att_fatal.h"
#include "caldbquery.h"
#include "headas.h"



/********************************************************
* read the parameters 
********************************************************/
PARAM* readParam(void) 
{
PARAM* param;

char filename[FILENAME_DIMEN];

double first_att_time;
double  last_att_time;
double time_margin;

/*****************************************
* allocate space for parameter structure *
*****************************************/
param=(PARAM*)malloc(sizeof(PARAM));

/****************
* attitude file *
****************/
read_string_param("attfile",filename,FILENAME_DIMEN);
param->att=openAttFile(filename);

first_att_time = readTimeFromAttFile(param->att, 1l);
last_att_time  = readTimeFromAttFile(param->att, param->att->nrows);

/*******
* GTIs *
*******/
read_string_param("gtis",filename,FILENAME_DIMEN);
param->gti = NULL;
param->bound_gtis = 0;

if(strncasecmp(filename,"none",FILENAME_DIMEN) && filename[0]!='\0' ) {
    /**************************************************
    * we have a valid GTI file name so try to read it *
    **************************************************/
    param->gti = readFITSGTIS(filename);

    if(param->gti == NULL) {
        fprintf(stderr, "Could not read GTI table - proceeding without GTIs\n");
    }
	else
		param->bound_gtis = read_boolean_param("boundgtis");
}

if(param->gti == NULL ) {
    /********************************************************
    * no GTI file, so use the entire attitude file interval *
    ********************************************************/
    param->gti = allocateGTIS(1);
    param->gti->start[0] = first_att_time;
    param->gti->stop[0]  =  last_att_time;
	param->gti->current = -1;


} /* end if we have to make up GTIs */



if (param->bound_gtis)
    restrictGTIs(param->gti, first_att_time, last_att_time);

else {
/************************************
* check for excessive extrapolation *
************************************/
time_margin = read_double_param("timemargin");

if(param->gti->start[0] < first_att_time - time_margin) {
    /***************************
    * too far before the start *
    ***************************/
    fprintf(stderr,
            "Warning: Extrapolating %g seconds before start of attitude file\n",
            first_att_time - param->gti->start[0] );
}


if(last_att_time + time_margin < param->gti->stop[param->gti->n-1] ) {
    /************************
    * too far after the end *
    ************************/
    fprintf(stderr,
            "Warning: Extrapolating %g seconds after end of attitude file\n",
            param->gti->stop[param->gti->n-1] - last_att_time );
}
}

/*************************
* New attitude file name *
**************************/
read_string_param("newattfile",param->newattfile,FILENAME_DIMEN);


/*********************
* binning parameters *
*********************/
param->nbins=read_int_param("nbins");
param->bin_size=M_PI/180.*read_double_param("binsize");
param->max_rotation=M_PI/180.*read_double_param("maxrot");

param->max_iterations = read_int_param("iterations");

/*******************
* alignment matrix *
*******************/
read_string_param("alignfile",param->alignfile,FILENAME_DIMEN);
if (!strncasecmp(param->alignfile, "CALDB", 5)) {
    CALDBQuery query = { 0 };
    strcpy(query.codename, "ALIGNMENT");
    strcpy(query.instrument, "SC");
    set_caldb_query_qualifiers(&query, param->alignfile);
    if (simple_caldb_query(&query, param->att->fp, param->alignfile))
        headas_chat(0, "unable to resolve alignfile=CALDB\n");
    else
        HDpar_note("alignfile", param->alignfile);
}

param->align = readAlign(param->alignfile);
if(param->align==NULL) {
    att_fatal(1);
}


return(param);

} /* end of readParam function */


/****************************************************************************
*****************************************************************************
* The following functions write the results to the parfile.
* the ra, dec and roll parameters are the mean telescope pointing
* and the Euler angles give the mean position of the spacecraft axes. 
* These functions don't use the PARAM structure but are in this file because
* they use the xpi wrappers.
****************************************************************************/
void writeRaDecRollToParfile(double ra, double dec, double roll) {

    write_double_param("ra",ra);
    write_double_param("dec",dec);
    write_double_param("roll",roll);

} /* end of writeRaDecRollToParfile function */


void writeEulerToParfile(EULER* e) {

    write_double_param("euler1",e->phi  *180./M_PI);
    write_double_param("euler2",e->theta*180./M_PI);
    write_double_param("euler3",e->psi  *180./M_PI);

} /* end of writeEulerToParfile function */
