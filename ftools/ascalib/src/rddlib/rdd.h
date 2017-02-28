#ifndef RDD_INCLUDED
/*****************************************************************************
*
* AUTHOR: Edward A. Pier <pier@ssvs.gsfc.nasa.gov>
*
* DATE:   1999-06-24
*
* DESCRIPTION:
* This is a set of routines for calculating RDD model functions
* This library will handle both the old Exponential Tail RDD model
* and the newer Poisson RDD model in a manner transparent to the user
* and programmer. The type of model used is determined by the 
* calibration file used.
*
* See http://www.astro.isas.ac.jp/~dotani/rddModel.html for more information
* about current RDD models.
*
* The general programmer will only need to worry about a few of the functionss
* in this library. Most are only used internally. See the file main.c
* for a simple example program showing how to use these functions.
*
* A note on calibration files:
* As of this writing there are three calibration files, each with a different
* format. This library can read any of these files and will automatically use
* the model corresponding to the calibration file.
*
* faintdfe.tbl is an ASCII file used by the faintdfe FTOOL. It gives parameters
*              for the Exponential Tail model.
*
* sisrddis_290296.fits is a FITS calibration files used by the sisrmg FTOOL.
*                      It also gives parameters for the Exponential Tail 
*                      model. Note that these parameters are nearly the same 
*                      as faintdfe.tbl, but not identical. The biggest 
*                      difference is in q0, which should be considered to be 
*                      arbitrary.
*
* sis_rdd_1999-06-24.fits gives parameters for the Poisson RDD model.
*                         it was created from ASCII files given by T. Dotani.
*
*
* 
*****************************************************************************/
#include <fitsio.h>

#define UNKNOWN_RDD_MODEL 0
#define EXPTAIL_RDD_MODEL 1
#define POISSON_RDD_MODEL 2

#define RDD_PEAK_ZERO 0
#define RDD_MEAN_ZERO 1
#define RDD_4040_ZERO 2

typedef struct {

int model_type; /* either EXPONENTIAL_RDD_MODEL or POISSON_RDD_MODEL */

/********************************************************
* parameters for exponential tail model by A. Rasmussen *
********************************************************/
double f;   /* mixing param between gaussian and exponential */
double sig; /* with of readout noise gaussian */
double Q;   /* slope of exponential component */
double q0;  /* center of gaussian and curoff of exponential */

double sig_over_Q;    /*=sig/Q */
double sig_over_Q2;   /*=sig*sig/(Q*Q) */
double gauss_norm;     /*=1.0/(2.*M_PI)/sig */
double exp_norm;       /*=0.5/Q */
double erfc_arg_norm; /*=1./sqrt(2.);


/****************************************************************
* additional parameters for Poisson model by Dotani & Yamashita *
****************************************************************/
double lambda; /* number of defects per pixel */
double J;      /* dark current per defect */

double accuracy;

double poisson_norm;

} RDDPARAM;


/**************************************
* function prototypes from rddparam.c *
**************************************/
RDDPARAM* allocateRDDparam(void);
void destroyRDDparam(RDDPARAM* rdd);

void setExpTailRDDparam(RDDPARAM* rdd, 
                        double f,  double sig, double Q, double q0,
                        double accuracy);

void setPoissonRDDparam(RDDPARAM* rdd, 
                        double f,  double sig, double Q, double q0,
                        double lambda, double J, double accuracy);

void resetq0RDDparam(RDDPARAM* rdd, double newq0);
void resetsigRDDparam(RDDPARAM* rdd, double sig);

void readRDDparam(RDDPARAM* rdd, char* file, double accuracy,
                  int sis, int chip, int mode, double time);

void readASCIIExpTailRDDparam(RDDPARAM* rdd, FILE* fp, double accuracy,
                              int sis, int  ccdmode, double time );

void readFITSExpTailRDDparam(RDDPARAM* rdd, fitsfile* fp, double accuracy,
                             int sis, int  ccdmode, double time );

void readFITSPoissonRDDparam(RDDPARAM* rdd, fitsfile* fp, double accuracy,
                             int sis, int chip, int mode, double time);

/***************************************
* function prototypes from functionc.c *
***************************************/
double rdd_lngamma(double x);
double rdd_two_minus_erfc(double x);
double rdd_poisson(double x, double lambda);

/*****************************************
* function prototypes from integration.c *
*****************************************/
double rdd_integral_to_infinity(double func(double, void*), void* param, 
                                double a, double accuracy );

double rdd_infinite_integral(double func(double, void*), void* param,
                             double accuracy );

double rdd_romberg(double func(double, void*), void*,
                    double a, double b, double toll);

double rdd_open_romberg(double func(double, void*), void* param,
                        double a, double b, double toll);

double rdd_trapezoid(double func(double, void*), void* param,
                     double a,  double b,  int n, double* remember);

double rdd_midpoint(double func(double, void*), void* param,
                    double a,  double b,  int n, double* remember);

void rdd_poly_interpolate(double* xa, double* ya,  int n,  double x,  
                double* y,  double* dy);

void rdd_bracket_minimum(double func(double, void*), void* param,
                         double* a, double* c, double* b );

double rdd_brent_minimum(double func(double, void*), void* param,
                         double min, double mid, double max, 
                         double accuracy);

/*********************************
* function prototypes from rdd.c *
*********************************/
double calculateRDDmodel(RDDPARAM* rdd, double q);
double expTailRDDmodel(RDDPARAM* rdd, double q);
double poissonRDDmodel(RDDPARAM* rdd, double q);
double multiPixelRDDmodel(RDDPARAM* rdd, double q, int n);
double intervalMeanOfRDDmodel(RDDPARAM* rdd, double min, double max);
double trueMeanOfRDDmodel(RDDPARAM* rdd);
double peakOfRDDmodel(RDDPARAM* rdd);

void adjustZeroOfRDDmodel(RDDPARAM* rdd, int zerotype );


#define RDD_INCLUDED
#endif
