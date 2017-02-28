#include <math.h>
#include "rdd.h"

/******************************************************************************
*******************************************************************************
* calculate the RDD function of the type specified when the function
* parameters were set 
******************************************************************************/
double calculateRDDmodel(RDDPARAM* rdd, double q) {

if(     rdd->model_type==EXPTAIL_RDD_MODEL) return ( expTailRDDmodel(rdd,q) );
else if(rdd->model_type==POISSON_RDD_MODEL) return ( poissonRDDmodel(rdd,q) );
else {
    fprintf(stderr,"Error: RDD model parameters not set\n");
    exit(1);
}

} /* end of calculateRDDmodel function */

/******************************************************************************
*******************************************************************************
* calculate the Exponential tail (Rassmussen) RDD function at an arbitrary 
* PHA value q.
* This function is a delta-function plus a powerlaw both convolved
* with gaussian readout noise. The convolutions are done analytically.
******************************************************************************/
double expTailRDDmodel(RDDPARAM* rdd, double q) {

double G, P;
double deltaq,qhat;
double erf_factor;

deltaq=q-rdd->q0;
qhat=deltaq/rdd->sig;

/**************************************
* delta function (gaussian) component *
**************************************/
G = rdd->gauss_norm * exp(-0.5*qhat*qhat);

/*******************
* power law factor *
*******************/
erf_factor=rdd_two_minus_erfc( rdd->erfc_arg_norm *(qhat - rdd->sig_over_Q) );

if(erf_factor!=0.) {
    /**************
    * normal case *
    **************/
    P = rdd->exp_norm * exp(0.5*rdd->sig_over_Q2 - qhat*rdd->sig_over_Q )
        *erf_factor;
} else {
    /**********************************************************************
    * this is to avoid P=NaN when q is a large negative number.
    * Note thaty the erf_factor goes to zero faster that the exponential
    * goes to infinity
    **********************************************************************/
    P=0.;
}

/*
printf("expTailRDDmodel: q=%g G=%g P=%g\n",q,G,P);
*/

return ((1.0 - rdd->f)*G + (rdd->f)*P);

} /* end of funcRasmussen function */

/****************************************************
*****************************************************
* wrapper function needed for integral
****************************************************/
double rdd_poisson_integrand(double x, void* param) {
void** param_array;
RDDPARAM* rdd;
double* q;

param_array=(void**)param;
rdd=(RDDPARAM*)param_array[0];
q  =(double*  )param_array[1];

return(rdd_poisson((x-rdd->q0)/rdd->J, rdd->lambda ) *
       expTailRDDmodel(rdd, *q-x+rdd->q0) );
} /* end of rdd_poisson_integrand */



/******************************************************************************
*******************************************************************************
* calculate the Poisson RDD function at an arbitrary PHA value q
* Note that this function is the convolution of a poisson distribution
* with the exponential tail RDD model function. The convolution integral
* is done numerically
******************************************************************************/
double poissonRDDmodel(RDDPARAM* rdd, double q) {

double result;

double min_exptail, max_exptail;
double min_poisson, max_poisson;
double min,max;

void** param;
param=(void**)malloc(sizeof(void*)*2);
param[0]=rdd;
param[1]=&q;


/*****************************************************************
* Note we integrate from q0 to infinity since the Poisson is 
* defined as zero left of the origin
*****************************************************************/
result = rdd_integral_to_infinity(rdd_poisson_integrand,param,
                                  rdd->q0,rdd->accuracy) 
         * rdd->poisson_norm;

free(param);

return result;

} /* end of poissonRDDmodel function */


/***********************************************
************************************************
* wrapper function needed for integration
***********************************************/
double rdd_add_multi_pixel_wrapper(double x, void* param) {
void** param_array;
RDDPARAM* rdd;
double* q;
int* n;

param_array=(void**)param;
rdd=(RDDPARAM*)param_array[0];
q  =(double*  )param_array[1];
n  =(int*     )param_array[2];

return calculateRDDmodel(rdd,x)*multiPixelRDDmodel(rdd,*q-x,*n-1);

} /* end of rdd_add_multi_pixel_wrapper function */

/*************************************************************************
**************************************************************************
* This function gives the RDD model convolved with itself n times.
* This is the RDD distribution for n combined pixels.
* The ExpTail model has an analytical form when n=2, but it is long and
* complicated and I was too lazy to type it in and debug it.
*************************************************************************/
double multiPixelRDDmodel(RDDPARAM* rdd, double q, int n) {

double result;

void** param;
param=(void**)malloc(sizeof(void*)*3);
param[0]=rdd;
param[1]=&q;
param[2]=&n;



if(n==1) {
    result = calculateRDDmodel(rdd,q);
} else {
    result = rdd_infinite_integral(rdd_add_multi_pixel_wrapper,param,
                                 rdd->accuracy);
}

free(param);

return result;


} /* end of multiPixelRDDmodel function */


/**********************************
***********************************
* two integrand wrapper functions 
**********************************/
double rdd_model_wrapper(double x, void* param) {
RDDPARAM* rdd;

rdd=(RDDPARAM*)param;
return calculateRDDmodel(rdd,x);
}

double x_rdd_model_wrapper(double x, void* param) {
RDDPARAM* rdd;

rdd=(RDDPARAM*)param;
return x*calculateRDDmodel(rdd,x);
}



/******************************************************************************
*******************************************************************************
* Returns the mean of the RDD model within an interval
******************************************************************************/
double intervalMeanOfRDDmodel(RDDPARAM* rdd, double min, double max) {

return (rdd_romberg(x_rdd_model_wrapper,rdd,min,max,rdd->accuracy)/
        rdd_romberg(  rdd_model_wrapper,rdd,min,max,rdd->accuracy)  );

}/* end of intervalMeanOfRDDmodel function */


/******************************************************************************
*******************************************************************************
* Returns the mean of the RDD model between -Inf and +Inf
******************************************************************************/
double trueMeanOfRDDmodel(RDDPARAM* rdd) {

if(rdd->model_type==EXPTAIL_RDD_MODEL) {
    /*******************************************************
    * we can calculate this analytically for Exptail model *
    *******************************************************/
    return(rdd->q0+rdd->f*rdd->Q);

} else if(rdd->model_type==POISSON_RDD_MODEL) {
    /************************************************************
    * we do a numerical integral 
    ************************************************************/
    return rdd_infinite_integral(x_rdd_model_wrapper,rdd,rdd->accuracy);

} else {
    fprintf(stderr,"RDD model parameters do not appear to be set\n");
    exit(1);
}


}/* end of trueMeanofRDDmodel function*/

/***********************************
*************************************
* wrapper functions
************************************/
double rdd_neg_exptail_wrapper(double x, void* param) {

RDDPARAM* rdd;
rdd=(RDDPARAM*)param;

return -expTailRDDmodel(rdd,x);

} /* end of rdd_neg_exptail_wrapper */

double rdd_neg_poisson_wrapper(double x, void* param) {

RDDPARAM* rdd;
rdd=(RDDPARAM*)param;

return -poissonRDDmodel(rdd,x);

} /* end of rdd_neg_poisson_wrapper */


/******************************************************************************
*******************************************************************************
* Returns the location of the peak of the RDD model
******************************************************************************/
double peakOfRDDmodel(RDDPARAM* rdd) {

double peak;


if(rdd->model_type==EXPTAIL_RDD_MODEL) {

    peak=rdd_brent_minimum(rdd_neg_exptail_wrapper,rdd,rdd->q0-rdd->sig,
                           rdd->q0, rdd->q0+rdd->sig+rdd->Q,
                           rdd->accuracy );


} else if(rdd->model_type==POISSON_RDD_MODEL) {
    double min,max,mid;

    min=rdd->q0+rdd->lambda;
    max=min+rdd->sig+rdd->Q+rdd->J;

    rdd_bracket_minimum(rdd_neg_poisson_wrapper,rdd,&min,&mid,&max);

    peak=rdd_brent_minimum(rdd_neg_poisson_wrapper,rdd, 
                           min,mid,max, rdd->accuracy );


} else {
    fprintf(stderr,"RDD model parameters do not appear to be set\n");
    exit(1);
}

return(peak);

}/* end of peakOfRDDmodel function */



/******************************************************************************
*******************************************************************************
* Adjusts the q0 parameter in order to put the one of three points at q=0
* The zerotype variable determines the point shifted to the origin:
* RDD_PEAK_ZERO the peak of the RDD distribution 
* RDD_MEAN_ZERO the true mean of the distribution between -Inf and + Inf
* RDD_4040_ZERO the mean of the distribution betwen -40 and +40
* 
* RDD_4040_ZERO mimics the onboard dark current correction algorithm
* in the absence of DFE. Note that it must be calculated iteratively.
*
******************************************************************************/
void adjustZeroOfRDDmodel(RDDPARAM* rdd, int zerotype ) {

if(zerotype==RDD_PEAK_ZERO) {
    /******************************************************************
    * peak at zero - kind of sloppy, but we do this twice to be sure
    * we have as much accuracy as we could want
    ******************************************************************/
    resetq0RDDparam(rdd, rdd->q0-peakOfRDDmodel(rdd));
    resetq0RDDparam(rdd, rdd->q0-peakOfRDDmodel(rdd));

} else if(zerotype==RDD_MEAN_ZERO) {
    /***************
    * mean at zero *
    ***************/
    resetq0RDDparam(rdd, rdd->q0-trueMeanOfRDDmodel(rdd));
    resetq0RDDparam(rdd, rdd->q0-trueMeanOfRDDmodel(rdd));

} else if(zerotype==RDD_4040_ZERO) {
    /**********************************************************
    * mimic onboard zero - note this must be done iteratively *
    **********************************************************/
    double mean;
    int i;
    int max_iterations=100;

    mean=0.0;
    resetq0RDDparam(rdd, rdd->q0-trueMeanOfRDDmodel(rdd));

    for(i=0;i<max_iterations &&
            fabs(mean=intervalMeanOfRDDmodel(rdd,-40.,40.))>rdd->accuracy; ++i){

        resetq0RDDparam(rdd, rdd->q0-mean);
    }

    if(i==max_iterations && fabs(mean)>rdd->accuracy) {
        fprintf(stderr,"Warning zero adjustment only accurate to %g\n",
                fabs(mean) );
    }

} else {
    /*****************
    * unknown option *
    *****************/
    fprintf(stderr,"Unknown method for adjusting distribution zero\n");
}/* end of zerodef options */

} /* end of adjustZeroOfRDDmodel function */
        
        
    

