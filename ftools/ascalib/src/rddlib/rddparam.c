/*****************************************************************************
* This file contains a set of routines for reading, setting, etc
* the RDD model function parameters
*****************************************************************************/

#include <stdlib.h>
#include <math.h>
#include <fitsio.h>
#include <longnam.h>
#include "rdd.h"

/****************************************************************************
*****************************************************************************
* allocate memory for a set of RDD parameters and initialize a few things
****************************************************************************/
RDDPARAM* allocateRDDparam(void) {

RDDPARAM* rdd;

rdd=(RDDPARAM*)malloc(sizeof(RDDPARAM));

rdd->model_type=UNKNOWN_RDD_MODEL;

return(rdd);

} /* end of allocateRDDparam function */

/****************************************************************************
*****************************************************************************
* free memory for a set of RDD parameters
****************************************************************************/
void destroyRDDparam(RDDPARAM* rdd) {

free(rdd);

} /* end of destroyRDDparam function */


/***************************************************************************
****************************************************************************
* set the parameters for an exponential tail (Rasmussen) RDD model function
****************************************************************************/
void setExpTailRDDparam(RDDPARAM* rdd, 
                        double f,  double sig, double Q, double q0, 
                        double accuracy) {


rdd->f=f;
rdd->sig=sig;
rdd->Q=Q;
rdd->q0=q0;

rdd->accuracy=accuracy;

rdd->sig_over_Q  = sig/Q;
rdd->sig_over_Q2 = (rdd->sig_over_Q)*(rdd->sig_over_Q);

rdd->gauss_norm=1.0/(sqrt(2.0*M_PI)*rdd->sig);
rdd->exp_norm=0.5/Q;
rdd->erfc_arg_norm=1.0/sqrt(2.0);


rdd->model_type=EXPTAIL_RDD_MODEL;

} /* end of setExpTailRDDparam function */



/****************************************************************
*****************************************************************
* wrapper around rdd_poisson needed for integration
****************************************************************/
double rdd_poisson_wrapper(double x, void* param) {
double* lambda;
lambda=(double*)param;
return(rdd_poisson(x,*lambda));
} /* end of rdd_poisson_wrapper function */

/***************************************************************************
****************************************************************************
* set the parameters for the Poisson RDD model function 
***************************************************************************/
void setPoissonRDDparam(RDDPARAM* rdd, 
                        double f,  double sig, double Q, double q0,
                        double lambda, double J, double accuracy ) {

double min_steps=3.;

setExpTailRDDparam(rdd,f,sig,Q,q0,accuracy);

rdd->lambda=lambda;
rdd->J=J;

rdd->model_type=POISSON_RDD_MODEL;

/*********************
* store the accuracy *
*********************/
rdd->accuracy=accuracy;

/*************************
* determine Poisson norm *
*************************/
rdd->poisson_norm=1./(rdd_integral_to_infinity(rdd_poisson_wrapper, &lambda,
                                               0.,accuracy) * rdd->J);


} /* end of setPoissonRDDparam function */


/*****************************************************************************
******************************************************************************
* reset the q0 param - this will translate the RDD model 
*****************************************************************************/
void resetq0RDDparam(RDDPARAM* rdd, double newq0) {

rdd->q0=newq0;

} /* end of resetq0RDDparam function */

/*****************************************************************************
******************************************************************************
* reset the sig param  
*****************************************************************************/
void resetsigRDDparam(RDDPARAM* rdd, double sig) {

rdd->sig=sig;

rdd->sig_over_Q  = sig/rdd->Q;
rdd->sig_over_Q2 = (rdd->sig_over_Q)*(rdd->sig_over_Q);

rdd->gauss_norm=1.0/(sqrt(2.0*M_PI)*rdd->sig);


} /* end of resetsigRDDparam function */



/*****************************************************************************
******************************************************************************
* read RDD model parameters from a calibration file for a given 
* instrument, CCD chip, clocking mode and ascatime.
* Calibration files may be of three types:
* (1) ASCII ExpTail cal file
* (2) FITS  ExpTail cal file
* (3) FITS  Poisson cal file
* This function determines the file format and calls the approproate routine
* to read the file of the correct type.
*
* Note the two ExpTail cal files give similar but not exactly the same results.
* Apart from small interpolation errors on f, The FITS table repeats the 
* same values for f at time 0.0 and 4.327199E+06 (an error)?
* Furthermore, the definition of q0 is a bit nebulous. However, this
* doesn't really matter since q0 has no physical significance.
* For the FITS version we take q0 such that the peak is at zero.
* This is similar to the linear-with-time values in the ascii version.
******************************************************************************/
void readRDDparam(RDDPARAM* rdd, char* file, double accuracy,
                  int sis, int chip, int mode, double time) {

int status=0;
fitsfile* fits_fp;
FILE* ascii_fp;


/*********************************
* open the file - try FITS first *
*********************************/
fits_open_file(&fits_fp,file,READONLY,&status);
if(status) {
    /********************************************************
    * couldn't open as a FITS file, so try the ASCII method *
    ********************************************************/
    ascii_fp=fopen(file,"r");
    if(ascii_fp==NULL) {
        fprintf(stderr,"Can't open RDD calibration file %s\n",file);
        exit(1);
    } 
 
    /********************************************************
    * read the model parameters from the ASCII ExpTail file
    * Note that all chips are assumed to be the same
    ********************************************************/
    readASCIIExpTailRDDparam(rdd,ascii_fp,accuracy,sis,mode,time);

} else {
    /***************************************************
    * this is  FITS file, first try old ExpTail format *
    ***************************************************/
    char model[FLEN_VALUE]={""};
    fits_read_key_str(fits_fp, "RDDMODEL", model, NULL,&status);

    if(status==KEY_NO_EXIST || !strncasecmp(model,"EXPTAIL",FLEN_COMMENT) ) {
        /**********************
        * FITS ExpTail format *
        **********************/
        status=0;
        readFITSExpTailRDDparam(rdd, fits_fp,accuracy,sis,mode,time);
        
    } else if(!strncasecmp(model,"POISSON",FLEN_COMMENT)) {
        /**********************
        * FITS Poisson format *
        **********************/
        readFITSPoissonRDDparam(rdd, fits_fp,accuracy,sis,chip,mode,time);
    } else {
        /********
        * error *
        ********/
        fprintf(stderr,"Error opening FITS RDD cal file %s\n",file);
        fits_report_error(stderr,status);
        exit(status);
    }

} /* end if it's a FITS file */


} /* end of readRDDparam function */

/******************************************************************************
*******************************************************************************
* Read ExpTail RDD parameters using the ASCII file method
* This is what faintdfe used to use in FTOOLS 4.2 and earlier.
* The file format gives coeficients for linear fits to the model parameters
* as a function of time
******************************************************************************/
void readASCIIExpTailRDDparam(RDDPARAM* rdd, FILE* fp, double accuracy,
                              int sis, int  ccdmode, double time ) {
char str[1024];
int tmp_sis=-1;
int tmp_ccdmode=-1;

double tf0, Tf, ss1, ss0, QQ1, QQ0, qq1, qq0;
double time0, ASCA_t0=0.0;
double tt;
int found_param=0;
int found_t0=0;

/*******************************
* read the time fit parameters *
*******************************/
while ( fgets(str,4096,fp) != NULL && !(sis==tmp_sis && ccdmode==tmp_ccdmode)){

    if ( strncmp(str,"!",1) ){
        /*********************
        * not a comment line *
        *********************/
        if ( sscanf(str,"%d%d%lf%lf%lf%lf%lf%lf%lf%lf",
                    &tmp_sis,&tmp_ccdmode,
		    &tf0, &Tf,  &ss1, &ss0, &QQ1, &QQ0, &qq1, &qq0) == 10 ) {
            /*******************
            * normal data line *
            *******************/
            found_param=1;

        } else if( sscanf(str,"%lf",&time0) == 1 ){
            /********************************
            * time zero reference time line *
            ********************************/
	    ASCA_t0 = time0;
            found_t0=1;
        } else{
            /*************
            * bogus line *
            *************/
            fprintf(stderr,
                    "Invalid Rasmussen ASCII RDD calibration file format\n");
            exit(1);
        }

    } /* end if not a comment line */

} /* end of loop over lines */

fclose(fp);

/*************************************************
* check to be sure we found the time coeficients *
*************************************************/
if(!found_param || ! found_t0) {
    fprintf(stderr,
            "Did not find parameters for SIS%d %d CCD mode in RDD cal file\n",
            sis, ccdmode);
    exit(1);
}

/***************************
* calculate RDD parameters *
***************************/
tt = time - ASCA_t0;

setExpTailRDDparam(rdd, 1.0 - exp(-pow((tt-tf0)/Tf,4.0)),
                        ss1*tt + ss0,
                        QQ1*tt + QQ0,
                        qq1*tt + qq0,
                   accuracy);

 
} /* end of readASCIIExpTailRDDparam function */


/*****************************************************************************
******************************************************************************
* read FITS ExpTail RDD model parameters - this format was used by sisrmg
* in FTOOLS 4.2 and earlier.
* This format gives a time history of all parameters in a FITS bintable.
*****************************************************************************/
void readFITSExpTailRDDparam(RDDPARAM* rdd, fitsfile* fp, double accuracy,
                             int sis, int ccdmode, double time ) {
int status=0;
int anynull;

long row;
long nrows;

int time_col;

char colname[FLEN_VALUE];
int f_col;
int sig_col;
int Q_col;
int q0_col;

double f[2];
double sig[2];
double Q[2];
double q0[2];

double hat;

double* timetable;

/******************************************
* make sure we are in the right extension *
******************************************/
fits_movnam_hdu(fp, BINARY_TBL, "RDD_T1", 0/*any version*/, &status);

/**************************************************************************
* read all the time values - there shouldn't ever be enough of these to 
* cause memory trouble 
**************************************************************************/
fits_read_key_lng(fp,"NAXIS2",&nrows,NULL,&status);

timetable=(double*)malloc(sizeof(double)*nrows);

fits_get_colnum(fp,CASESEN,"TIME",&time_col,&status);
fits_read_col_dbl(fp,time_col,1l,1l,nrows,0.,timetable,&anynull,&status);

/***********************************************************************
* find the table rows bracketing the desired time 
* Tricky index stuff - the following loop leaves row pointing
* to the row immediately after the desired time.
* But them FITSIO row numbers start at 1 instead of zero,
* so row is the row *before* the desired time as far
* as FITSIO is concerned.
* Note also the following loop starts at 1 so we don't read
* before the array if the time is before the first entry in the table.
***********************************************************************/
for(row=1; row<nrows-1 && time>=timetable[row]; ++row);

hat=(time-timetable[row-1])/(timetable[row]-timetable[row-1]);

/*
printf("\nrow=%d nrows=%d hat=%g\n",row,nrows,hat);
*/

/******************************************************
* find the fits columns for the desired combination 
* of instrument and clocking mode
******************************************************/
sprintf(colname,"SIS%dM%dFR",sis,ccdmode);
fits_get_colnum(fp,CASESEN,colname,&f_col,&status);

sprintf(colname,"SIS%dM%dSG",sis,ccdmode);
fits_get_colnum(fp,CASESEN,colname,&sig_col,&status);


sprintf(colname,"SIS%dM%dX1",sis,ccdmode);
fits_get_colnum(fp,CASESEN,colname,&Q_col,&status);


sprintf(colname,"SIS%dM%dQP",sis,ccdmode);
fits_get_colnum(fp,CASESEN,colname,&q0_col,&status);

/**************************************************
* read the parameters bracketing the desired time *
**************************************************/
fits_read_col_dbl(fp,f_col  ,row,1l,2l,0.,f  ,&anynull,&status);
fits_read_col_dbl(fp,sig_col,row,1l,2l,0.,sig,&anynull,&status);
fits_read_col_dbl(fp,Q_col  ,row,1l,2l,0.,Q  ,&anynull,&status);
fits_read_col_dbl(fp,q0_col ,row,1l,2l,0.,q0 ,&anynull,&status);

/*************************************
* close up and check for FITS errors *
*************************************/
fits_close_file(fp,&status);
if(status) {
    fprintf(stderr,"Error reading FITS ExpTail RDD cal file\n");
    fits_report_error(stderr,status);
    exit(status);
}

/*****************************
* interpolate the parameters *
*****************************/
setExpTailRDDparam(rdd, hat*(  f[1]-  f[0])+  f[0],
                        hat*(sig[1]-sig[0])+sig[0],
                        hat*(  Q[1]-  Q[0])+  Q[0],
                        hat*( q0[1]- q0[0])+ q0[0],
                   accuracy);

} /* end of readASCIIExpTailRDDparam function */


/*****************************************************************************
******************************************************************************
* read FITS Poisson model parameters. 
*****************************************************************************/
void readFITSPoissonRDDparam(RDDPARAM* rdd, fitsfile* fp, double accuracy,
                             int sis, int chip, int mode, double time) {
int status=0;
int anynull;

long row;
long nrows;

int time_col;

char colname[FLEN_VALUE];
int f_col;
int sig_col;
int Q_col;
int q0_col;
int lambda_col;
int J_col;

double f[2];
double sig[2];
double Q[2];
double q0[2];
double lambda[2];
double J[2];

double hat;

double* timetable;

char extname[FLEN_VALUE];
char key[FLEN_KEYWORD];
long default_chip;

/***************************
* find the right extension *
***************************/
sprintf(extname,"RDD_S%dC%dM%d",sis,chip,mode);
fits_movnam_hdu(fp, BINARY_TBL, extname, 0/*any version*/, &status);
if(status==BAD_HDU_NUM) {
    /***************************
    * try the default CCD chip *
    ***************************/
    status=0;
    sprintf(key,"S%dDEFCCD",sis);
    fits_read_key_lng(fp, key, &default_chip, NULL, &status);
    sprintf(extname,"RDD_S%dC%dM%d",sis,default_chip,mode);
    fits_movnam_hdu(fp, BINARY_TBL, extname, 0/*any version*/, &status);

    if(status==BAD_HDU_NUM) {
        fprintf(stderr,
                "Can't fits RDD param extension for SIS%d, %d CCD mode\n",
                sis, mode);
        exit(status);
    } else {
        fprintf(stdout,"Using default CCD %d instead of CCD %d for RDD model\n",
                default_chip, chip);
    }
} /* end if did not find extension for requested chip */

/**************************************************************************
* read all the time values - there shouldn't ever be enough of these to 
* cause memory trouble 
**************************************************************************/
fits_read_key_lng(fp,"NAXIS2",&nrows,NULL,&status);

timetable=(double*)malloc(sizeof(double)*nrows);

fits_get_colnum(fp,CASESEN,"TIME",&time_col,&status);
fits_read_col_dbl(fp,time_col,1l,1l,nrows,0.,timetable,&anynull,&status);


/***********************************************************************
* find the table rows bracketing the desired time 
* Tricky index stuff - the following loop leaves row pointing
* to the row immediately after the desired time.
* But them FITSIO row numbers start at 1 instead of zero,
* so row is the row *before* the desired time as far
* as FITSIO is concerned.
* Note also the following loop starts at 1 so we don't read
* before the array if the time is before the first entry in the table.
***********************************************************************/
for(row=1; row<nrows-1 && time>=timetable[row]; ++row);

hat=(time-timetable[row-1])/(timetable[row]-timetable[row-1]);

/******************************************************
* find the fits columns for the desired combination 
* of instrument and clocking mode
******************************************************/

fits_get_colnum(fp,CASESEN,"F"     ,&f_col     ,&status);
fits_get_colnum(fp,CASESEN,"SIGMA" ,&sig_col   ,&status);
fits_get_colnum(fp,CASESEN,"Q"     ,&Q_col     ,&status);
fits_get_colnum(fp,CASESEN,"Q0"    ,&q0_col    ,&status);
fits_get_colnum(fp,CASESEN,"LAMBDA",&lambda_col,&status);
fits_get_colnum(fp,CASESEN,"J"     ,&J_col     ,&status);

/**************************************************
* read the parameters bracketing the desired time *
**************************************************/
fits_read_col_dbl(fp,f_col     ,row,1l,2l,0.,f     ,&anynull,&status);
fits_read_col_dbl(fp,sig_col   ,row,1l,2l,0.,sig   ,&anynull,&status);
fits_read_col_dbl(fp,Q_col     ,row,1l,2l,0.,Q     ,&anynull,&status);
fits_read_col_dbl(fp,q0_col    ,row,1l,2l,0.,q0    ,&anynull,&status);
fits_read_col_dbl(fp,lambda_col,row,1l,2l,0.,lambda,&anynull,&status);
fits_read_col_dbl(fp,J_col     ,row,1l,2l,0.,J     ,&anynull,&status);

/*************************************
* close up and check for FITS errors *
*************************************/
fits_close_file(fp,&status);
if(status) {
    fprintf(stderr,"Error reading FITS Poisson RDD cal file\n");
    fits_report_error(stderr,status);
    exit(status);
}

/*****************************
* interpolate the parameters *
*****************************/
setPoissonRDDparam(rdd, hat*(     f[1]-      f[0])+      f[0],
                        hat*(   sig[1]-    sig[0])+    sig[0],
                        hat*(     Q[1]-      Q[0])+      Q[0],
                        hat*(    q0[1]-     q0[0])+     q0[0],
                        hat*(lambda[1]- lambda[0])+ lambda[0],
                        hat*(     J[1]-      J[0])+      J[0],
                   accuracy);


} /* end of readFITSPoissonRDDparam */


