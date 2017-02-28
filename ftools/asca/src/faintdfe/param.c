#include <string.h>
#include <stdlib.h>
#include "xpi_wrappers.h"
#include "faintdfe.h"
#include "param.h"

/*************************************************************************
**************************************************************************
* read the parfile into a new PARAM structure
*************************************************************************/
PARAM* readParam(void) {

PARAM* param;
char gradelist[PARAM_STRING_DIMEN];
char* token;
int grade;

char specdump[PARAM_STRING_DIMEN];

/*************************************
* allocate space for param structure *
*************************************/
param=(PARAM*)malloc(sizeof(PARAM));

/******************
* read parameters *
******************/
read_string_param("infile" , param->infile , PARAM_STRING_DIMEN );
read_string_param("extname", param->extname, PARAM_STRING_DIMEN );
read_string_param("tblfile", param->tblfile, PARAM_STRING_DIMEN );
read_string_param("outfile", param->outfile, PARAM_STRING_DIMEN );

if(!strncmp(param->outfile, "STDOUT", PARAM_STRING_DIMEN) ) {
    param->fpout=stdout;
} else {
    param->fpout=fopen(param->outfile,"w");
    if(param->fpout == NULL) {
        fprintf(stderr,"Can't open output file %s\n",param->outfile);
        exit(1);
    }
}

param->split  =read_int_param("split"  );
param->zerodef=read_int_param("zerodef");

param->binsec=read_double_param("binsec");

param->mincounts=read_int_param("mincounts");

/*******************************
* decode the grade list string *
*******************************/
read_string_param("grades", gradelist, PARAM_STRING_DIMEN );

for(grade=0;grade<NGRADES;++grade) param->use_grade[grade]=0;

token=strtok(gradelist," ");
while(token!=NULL) {
    
    if(sscanf(token,"%d",&grade)==1 && grade>=0 && grade <NGRADES ) {
        /**************
        * valid grade *
        **************/
        param->use_grade[grade]=1;
    } else {
        /****************
        * invalid grade *
        ****************/
        fprintf(stderr,"Invalid grade \"%s\" ignored\n",token);
    }

    /*********************
    * get the next token *
    *********************/
    token=strtok(NULL," ");
    
}

/***********
* accuracy *
***********/
param->accuracy=read_double_param("accuracy");


/*************************************************
* spectral dump file mostly useful for debugging *
*************************************************/
read_string_param("specdump", specdump, PARAM_STRING_DIMEN );
param->specdump=openSpecDump(specdump,-MAX_PHA, DIST_DIMEN);

return(param);

} /* end of readParam function */
