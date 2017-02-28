#include <string.h>
#include <stdlib.h>

#include "param.h"
#include "param_wrappers.h"
#include "random.h"
#include "headas.h"
#include "caldbquery.h"
#include "attfile.h"


/**************************************************************************
**************************************************************************
* read the input parameters 
**************************************************************************/
PARAM* readParam(void) 
{
PARAM* param;
char interpolation[FLEN_VALUE];
param = (PARAM*) calloc(1, sizeof(PARAM));

read_string_param("eventfile",param->event_file     ,FILENAME_LENGTH);
read_string_param("eventext" ,param->event_extension,FLEN_VALUE     );

read_string_param("timecol"  ,param->time_col_name  ,FLEN_VALUE     );

read_string_param("teldef"   ,param->cal_file       ,FILENAME_LENGTH);

read_string_param("attfile"  ,param->att_file       ,FILENAME_LENGTH);

/****************************************************************
* the user can turn off aspecting by giving "none" or a blank 
* string for the attitude file name 
****************************************************************/
if(strncasecmp(param->att_file,"none",FILENAME_LENGTH) || 
   param->att_file[0]=='\0' ) {
    /***************************************************
    * valid attfile name given so we will do aspecting *
    ***************************************************/
    param->do_sky=1;
} else {
    /**********************************************************
    * null attfile name given, so we won't be doing aspecting *
    **********************************************************/
    param->do_sky=0;
}

/****************************************************************
* the user can pass CALDB for the TELDEF file name
****************************************************************/
if(!strncasecmp(param->cal_file, "CALDB", 5)) {
	CALDBQuery query = { 0 };
	sprintf(query.codename, "TELDEF");
	query.infile = param->event_file;
	set_caldb_query_qualifiers(&query, param->cal_file);
	if (simple_caldb_query(&query, 0, param->cal_file)) {
		headas_chat(0, "unable to resolve teldef=CALDB\n");
		free(param);
		return 0;
	}
	HDpar_note("teldef", param->cal_file);
}

/*******************************
* aspecting-related parameters *
*******************************/
if(param->do_sky) {
   
    /******************************
    * should we apply aberration? *
    ******************************/
    param->do_aberration=read_boolean_param("aberration");
    if(param->do_aberration) param->follow_sun=read_boolean_param("follow_sun");
    else                     param->follow_sun=0;

    /*******************************************************
    * nominal pointing entered in decimal R.A. and Dec but
    * converted immediately to Euler angles
    ******************************************************/
    param->ra =read_double_param("ra");
    param->dec=read_double_param("dec");

    /******************************************************
    * value to write if SKY coordinates are out of bounds *
    ******************************************************/
    param->skyx_null_value=read_int_param("skyxnull");
    param->skyy_null_value=read_int_param("skyynull");

    read_string_param("interpolation", interpolation ,FLEN_VALUE);
    if (!strcasecmp(interpolation, "CONSTANT")) {
        param->interpolation = ATTFILE_CONSTANT;
        headas_chat(2, "using CONSTANT interpolation for aspecting\n");
    }
    else {
        param->interpolation = ATTFILE_LINEAR;
        headas_chat(2, "using LINEAR interpolation for aspecting\n");
    }

} else {
   /*************************************************
   * we will not be doing DET -> SKY transformation *
   *************************************************/
   param->do_aberration=0;
   param->follow_sun=0;
}

/*************************************
* random number generator parameters *
*************************************/
param->randomize=read_boolean_param("randomize");
if(param->randomize) {
    param->seed=read_int_param("seed");
    seed_random(param->seed);
}

/********************************************************
* how far outside the attitude file can we extrapolate? *
********************************************************/
param->time_margin=read_double_param("timemargin");


return(param);
}

/****************************************************************************
*****************************************************************************
* free all the memory associated with a param structure
****************************************************************************/
void destroyParam(PARAM* param) {

free(param);

} /* end of destroyParam function */

