#include <math.h>
#include <string.h>
#include "info.h"

/*************************************************************************
**************************************************************************
* create a new INFO structure 
* This structure is used to communicate with the FITSIO iterator
* when making SKY images. Note that it is not used when 
* making DET or FOC images
*************************************************************************/

INFO* createInfo(PARAM* param, fitsfile* fp, int* iserror) {

INFO* info;

int status=0;
char comment[FLEN_COMMENT]="";
char mission[FLEN_VALUE]="UNKNOWN";
char instrument[FLEN_VALUE]="UNKNOWN";
 TELDEF* cal;

/********************************
* allocate memory for structure *
********************************/
info=(INFO*)malloc(sizeof(INFO));

/***********************************
* remember the parameter structure *
***********************************/
info->param=param;

/**********************************************
* read mission and instrument from event file *
**********************************************/
fits_read_key_str(fp,"TELESCOP",mission   ,comment,&status);
fits_read_key_str(fp,"INSTRUME",instrument,comment,&status);
if(status) {
    printf("Warning: can't read TELESCOP and INSTRUME keywords from %s\n",
           param->event_file);
    status=0;
}
/****************************************************
* open teldef file and make some consistency checks *
****************************************************/
info->cal=aste_coord_init(mission,instrument,param->teldef_file); 
 if (NULL == info->cal) {
   fprintf(stderr,
	   "Error; Cannot allocate TELDEF struct.\n%s does not exist?\n",
	   param->teldef_file);
   *iserror=TELDEF_NOT_EXIST;
   return NULL;
 }

if(strncmp(mission, info->cal->telescop, FLEN_VALUE)) {
    fprintf(stderr,"Warning TELESCOP=%s in %s but =%s in %s\nn",
            mission,param->event_file,info->cal->telescop,info->cal->filename);
}

if(strncmp(instrument, info->cal->instrume, FLEN_VALUE)) {
    fprintf(stderr,"Warning INSTRUME=%s in %s but =%s in %s\nn",
                       instrument,param->event_file,
            info->cal->instrume,  info->cal->filename);
}

/******************************
* create and initialize image *
******************************/
info->im=allocateImage(info->cal->mission.aste->sky.xsiz, info->cal->mission.aste->sky.ysiz );
blankImage(info->im,0);





return(info);

} /* end of createInfo function */
