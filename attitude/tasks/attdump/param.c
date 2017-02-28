#include <stdlib.h>
#include <string.h>
#include "param_wrappers.h"
#include "param.h"

/***************************************************************************
****************************************************************************
* create a new PARAM structure and read the parfile
***************************************************************************/
PARAM* readParam(void) {

PARAM* param;
char alignfile[FILENAME_DIMEN];

param=(PARAM*)malloc(sizeof(PARAM));

read_string_param("infile"   , param->infile , FILENAME_DIMEN);
read_string_param("outfile"  , param->outfile, FILENAME_DIMEN);
read_string_param("alignfile", alignfile     , FILENAME_DIMEN);

param->align = readAlign(alignfile);

/*******************
* open the outfile *
*******************/
if(!strncasecmp(param->outfile,"stdout",FILENAME_DIMEN) ||
   !strncmp(param->outfile,"-",FILENAME_DIMEN)             ) {
    /***************************
    * write to standard output *
    ***************************/
    param->fpout=stdout;

} else {
    /**************************
    * write to an actual file *
    **************************/
    param->fpout=fopen(param->outfile,"w");
}

/*************************
* read the output format *
*************************/
read_string_param("format",param->format,FILENAME_DIMEN-1);
strcat(param->format,"\n");

param->history = read_boolean_param("history");

return(param);
}
