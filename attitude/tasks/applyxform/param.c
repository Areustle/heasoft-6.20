#include <stdlib.h>
#include <string.h>

#include "headas.h"
#include "param.h"

#include "param_wrappers.h"

#include "att_fatal.h"


/**************************************************************************
* read the input parameters
**************************************************************************/
PARAM* readParam(void) {

char filename[FILENAME_LENGTH];

PARAM* param;

/*********************************
* create the parameter structure *
*********************************/
param=(PARAM*)malloc(sizeof(PARAM));

/**********
* in file *
**********/
read_string_param("infile", filename, FILENAME_LENGTH);
if(!strcmp(filename, "STDIN")) {
    param->fpin = stdin;
} else {
    param->fpin = fopen(filename, "r");
    if(param->fpin == NULL) {
        fprintf(stderr, "Could not open %s\n", filename);
        att_fatal(1);
    }
}

/************
* transform *
************/
read_string_param("transform", filename, FILENAME_LENGTH);
param->combo = readComboXform(filename);
if(param->combo == NULL) {

    fprintf(stderr, "Could not read %s\n", filename);
    att_fatal(1);
}

/***********
* out file *
***********/
read_string_param("outfile", filename, FILENAME_LENGTH);
if(!strcmp(filename, "STDOUT")) {
    param->fpout = stdout;
} else {
    param->fpout = fopen(filename, "w");
    if(param->fpout == NULL) {
        fprintf(stderr, "Could not open %s\n", filename);
        att_fatal(1);
    }
}

/*********
* format *
*********/
{
	char inform[16];
	char outform[16];
	int precision;
    precision = read_int_param("inprec");
	sprintf(inform, "%%.%df", precision);
    precision = read_int_param("outprec");
	sprintf(outform, "%%.%df", precision);
	sprintf(param->format, "%s %s %s %s\n", inform, inform, outform, outform);
}

return param;

} /* end of readParam */

