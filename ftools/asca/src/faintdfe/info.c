#include <stdlib.h>
#include <math.h>
#include "fitsio.h"
#include "longnam.h"
#include "rdd.h"

#include "param.h"
#include "function.h"
#include "info.h"


/***************************************************************************
****************************************************************************
* create a new INFO structure 
* This routine also reads a bunch of things from the FITS header and
* initializes the model RDD function 
***************************************************************************/
INFO* allocateInfo(PARAM* param, fitsfile* fp) {

INFO* info;

int status=0;

char key[FLEN_KEYWORD];
char instrument[FLEN_VALUE];
long long_value;
int sis, ccdmode;
char datamode[FLEN_VALUE];
double tstart;

int hdu;
int hdutype;

char ccdlist[FLEN_VALUE];
int chip0,chip1,chip2,chip3;


/***********************************
* allocate space for the structure *
***********************************/
info=(INFO*)malloc(sizeof(INFO));
if(info==NULL) {
    fprintf(stderr,"Can't allocate memory for INFO structure\n");
    exit(1);
}

/***************************
* save the param structure *
***************************/
info->param=param;

/******************
* instrument name *
******************/
instrument[0]='\0'; /* just in case*/
fits_read_key_str(fp,"INSTRUME",instrument,NULL,&status);
if(sscanf(instrument,"SIS%d",&sis)==0 || !(sis==0 || sis==1) ) {
    fprintf(stderr, "Invalid instrument %s\n",instrument);
    exit(1);
}

/************
* data mode *
************/
fits_read_key_str(fp,"DATAMODE",datamode,NULL,&status);
if(strncmp(datamode,"FAINT",FLEN_VALUE) ){
    fprintf(stderr,"invalid mode %s, only FAINT allowed\n",datamode);
    exit(1);
}

/***********************************************
* CCD mode and CCD list
* OK, this is stupid, but we have to go back 
* to the primary HDU to read the CCD keywords
***********************************************/
fits_get_hdu_num(fp,&hdu);
fits_movabs_hdu(fp,1/*primary HDU*/,&hdutype,&status);

/************
* SnCCDMODE *
************/
sprintf(key,"S%dCCDMOD",sis);
fits_read_key_lng(fp,key,&long_value,NULL,&status);
ccdmode=(int)long_value;

/***********
* SnCCDLST *
***********/
sprintf(key,"S%dCCDLST",sis);
fits_read_key_str(fp,key,ccdlist,NULL,&status);
sscanf(ccdlist,"%d %d %d %d",&chip0,&chip1,&chip2,&chip3);
info->ccd_on[0]=0;
info->ccd_on[1]=0;
info->ccd_on[2]=0;
info->ccd_on[3]=0;

++(info->ccd_on[chip0]);
++(info->ccd_on[chip1]);
++(info->ccd_on[chip2]);
++(info->ccd_on[chip3]);

/**************************
* go back to original HDU *
**************************/
fits_movabs_hdu(fp,hdu,&hdutype,&status);

/*********
* TSTART *
*********/
fits_read_key_dbl(fp,"TSTART",&tstart,NULL,&status);



/**************************
* check for FITSIO errors *
**************************/
if(status) {
    fprintf(stderr,"Error reading header of FITS file %s\n",param->infile);
    fprintf(stderr,"INSTRUMENT=%s (SIS%d)\n",instrument,sis);
    fprintf(stderr,"DATAMODE=%s\n",datamode);
    fprintf(stderr,"S%dCCDMODE=%d\n",sis,ccdmode);
    fprintf(stderr,"S%dCCDLST=%s\n",sis,ccdlist);
    fprintf(stderr,"TSTART=%g\n",tstart);

    fits_report_error(stderr,status);
    exit(status);
}

/******************
* Rasmussen model *
******************/
setRDDmodelInfo(info,tstart,sis,ccdmode);


/**********************************************************************
* OK , maybe this is dumb, but lets just make sure that 1<<9 means
* what we think it does on this machine, so that the grading routine
* will work OK
**********************************************************************/
if(1<<9 != 512 ) {
    fprintf(stderr,"Woops! 1<<9 != 512 on this machine\n");
    exit(1);
}



return(info);

} /* end of allocateInfo function */


/****************************************************************************
*****************************************************************************
* Initialize the RDD model 
****************************************************************************/
void setRDDmodelInfo(INFO* info, double time, int sis, int ccdmode) {

PARAM* param;
RDDPARAM* rdd;
double ave=0.;
double sig=0;
int ccd;
int i;
double n;

/******************************************
* get the param structure for convenience *
******************************************/
param=info->param;

/********************************
* initialize model RDD function *
********************************/
info->max_pha=MAX_MODEL_PHA;
info->model_dimen=2*MAX_MODEL_PHA+1;

rdd=allocateRDDparam();

for(ccd=0;ccd<4;++ccd) {

    if(info->ccd_on[ccd]) {
        
        /****************************
        * read the model parameters *
        ****************************/
        readRDDparam(rdd,param->tblfile,param->accuracy,sis,ccd,ccdmode,time);

        /************************
        * adjust the zero point *
        ************************/
        adjustZeroOfRDDmodel(rdd,param->zerodef );

        /******************
        * calculate model *
        ******************/
        for(i=-info->max_pha; i<=info->max_pha; i++){

            info->model[ccd][i+info->max_pha] = 
                                              calculateRDDmodel(rdd, (double)i);
        }

        /************
        * normalize *
        ************/
        normalize_distribution(info->model[ccd],info->model_dimen);

    } /* end if the current CCD is on */
} /* end of loop over chips */

/**********
* cleanup *
**********/
destroyRDDparam(rdd);

} /* end of setRDDmodelInfo function */

