#ifndef HXDBSTJUDGEUTIL_H
#define HXDBSTJUDGEUTIL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <anl.h>
#include <fitsio.h>
#include <mpfit.h>

/*
** burst detection algorithms
*/
typedef enum {
    GINGA,
    HETE2,
    STEP
} hxdBstJudge_DetAlg;

/*
** structure to hold a basic light curve
*/
typedef struct hxdBstJudge_LC {
    double* time;
    double* rate;
    double* error;
    double* fracexp;
    double* deadc;
    double  tpixr;
    double  tzero;
    double  timedel;
    double  mean;
    double  stddev;
    double  variance;
    long    npts;
    int     has_deadc_col;
} hxdBstJudge_LC;

/*
** trigger data structure
*/
typedef struct hxdBstJudge_Trigger {
    double  b1start;      /* early background start time */
    double  b1stop;       /* early background stop time */
    long    ifstart;      /* light curve index for fstart */
    double  fstart;       /* foreground start time */
    double  fstop;        /* foreground stop time */
    double  b2start;      /* late background start time */
    double  b2stop;       /* late background stop time */
    double  sigma;        /* sigma of detection, if any, 0.0 otherwise */
    double  fore_cnts;    /* foreground interval counts */
    double  fore_rate;    /* foreground interval rate */
    double  fore_rateerr; /* error on foreground interval rate */
    double  back_cnts;    /* est. background counts in foreground interval */
    double  back_rate;    /* background interval rate */
    double  back_rateerr; /* error on background interval rate */
    double  net_cnts;     /* background subtracted foreground counts */
    double  net_rate;     /* background subtracted foreground rate */
    double  net_rateerr;  /* error on background subtracted foreground rate */
    double* bgcoeffs;     /* background polynomial fit coeffs */
    int     bgorder;      /* order of polynomial fit */
    double  t50;          /* T50 */
    double  t50err;       /* T50 error */
    double  t50start;     /* T50 start */
    double  t50stop;      /* T50 stop */
    double  t90;          /* T90 */
    double  t90err;       /* T90 error */
    double  t90start;     /* T90 start */
    double  t90stop;      /* T90 stop */
} hxdBstJudge_Trigger;

/*
** hxdBstJudge_LC_new -
**
**      allocates a new light curve data structure
*/
int
hxdBstJudge_LC_new( hxdBstJudge_LC* lc, int num_row );

/*
** hxdBstJudge_LC_free -
**
**      frees memory allocated for a light curve
*/
void
hxdBstJudge_LC_free( hxdBstJudge_LC* lc );

/*
** hxdBstJudge_LC_read -
**
**      reads a fits light curve
**      must have TIME, RATE, ERROR and FRACEXP columns
*/
int
hxdBstJudge_LC_read( char filename[ ], hxdBstJudge_LC* lc );

/*
** hxdBstJudge_LC_stats_basic -
**
**      calculates basic light curve statistics and stores them
**      in the input light curve data structure
*/
void
hxdBstJudge_LC_stats_basic( hxdBstJudge_LC* lc, double minthresh,
                            double maxthresh );

/*
** hxdBstJudge_LC_bincenter -
**
**      calculates the center of a light curve time bin
*/
double hxdBstJudge_LC_bincenter( hxdBstJudge_LC* lc, long i );

/*
** hxdBstJudge_LC_bst_judge -
**
**      detects bursts
*/
void
hxdBstJudge_LC_bst_judge( hxdBstJudge_LC* lc, hxdBstJudge_Trigger** intrigs,
                          int nintrigs, hxdBstJudge_Trigger*** outtrigs,
                          int* ntrigs,  hxdBstJudge_DetAlg det_alg,
                          double gaptol, double overlaptol, double maxdur,
                          int durest, int* status );

/*
** hxdBstJudge_Trigger_new -
**
**      allocates a new trigger data structure and initializes it
*/
hxdBstJudge_Trigger*
hxdBstJudge_Trigger_new( double fstart, double fdur, double b1dur,
                         double b1offset, double b2dur, double b2offset,
                         double sigma, int bgorder );

/*
** hxdBstJudge_Trigger_free -
**
**      frees memory allocated to a trigger
*/
void hxdBstJudge_Trigger_free( hxdBstJudge_Trigger** trig );

/*
** hxdBstJudge_Trigger_zero -
**
**      zeroes a trigger by shifting the foreground start time to zero, and
**      all other times relative to it.
*/
void hxdBstJudge_Trigger_zero( hxdBstJudge_Trigger* trig );

/*
** hxdBstJudge_Trigger_deepcopy -
**
**      performs a deep copy of 1 trigger data structure to another
*/
hxdBstJudge_Trigger*
hxdBstJudge_Trigger_deepcopy( hxdBstJudge_Trigger* trig );

/*
** HXDbstJudge_Trigger_get_best -
**
**      calculates the best trigger for a light curve at a given time
**      choosing among all input triggers
*/
hxdBstJudge_Trigger*
hxdBstJudge_Trigger_get_best( hxdBstJudge_Trigger** intrigs, long ntrigs,
                              hxdBstJudge_LC* lc, long itrig, int bgorder,
                              double gaptol, int* status );
/*
** hxdBstJudge_Trigger_calcbest -
**
**      calculates the best trigger for a light curve at a given time
*/
hxdBstJudge_Trigger*
hxdBstJudge_Trigger_calcbest( hxdBstJudge_Trigger* intrig,
                              hxdBstJudge_LC* lc, long itrig,
                              long bgorder, double gaptol, double overlaptol,
                              double maxdur, int* status );

/*
** hxdBstJudge_Trigger_calc -
**
**      calculates a trigger on a light curve
*/
hxdBstJudge_Trigger*
hxdBstJudge_Trigger_calc( hxdBstJudge_Trigger* intrig,
                          hxdBstJudge_LC* lc, long itrig,
                          int bgorder, double gaptol, int* status );

/*
** hxdBstJudge_Trigger_sigma -
**
**      estimates the gaussian sigma detection level for a given
**      trigger and an input light curve
*/
void hxdBstJudge_Trigger_sigma( hxdBstJudge_LC* lc, long itrig,
                                hxdBstJudge_Trigger* intrig, int bgorder,
                                int* status );

/*
** hxdBstJudge_Trigger_polyfit_func -
** 
**      polynomial fit function for SV decomp
*/
void hxdBstJudge_Trigger_polyfit_func( double x, double* afunc, unsigned int m );

/*
** hxdBstJudge_Trigger_polyfit -
**
**      fits a degree=degree polynomial to x/y/yerr
*/
void hxdBstJudge_Trigger_polyfit( double* x, double* y, double* yerr,
                                  double* a, int n, int degree );

/*
** hxdBstJudge_Trigger_linleast -
**
**      does linear least squares fit
*/
void hxdBstJudge_Trigger_linleast( double* x, double* y, double* yerr,
                                   double* a, double* chisq, int n );

/*
** hxdBstJudge_Trigger_icrosstime -
*/
int
hxdBstJudge_Trigger_icrosstime( double *cumsum, double cummin, double maxcts,
                                double thresh, int istart, int istop,
                                int durerrmeth, double rms, int *ifirst0,
                                int *ilast0 );

/*
** hxdBstJudge_Trigger_burstdur -
**
**      Compute burst duration measures - the method is to scan the
**      "cumulative" counts array, starting from the left and right hand side
**      until X% of the counts have been scanned over.  The algorithm accounts
**      for the possibility that the threshold may be crossed several times,
**      and uses an estimate of the light curve uncertainties to establish a
**      threshold "band" rather than a hard threshold number.
**
**      This code is adapted from the Swift task battblocks. Thanks to
**      Craig M. for the original code.
*/
int
hxdBstJudge_Trigger_burstdur( hxdBstJudge_Trigger* trig,
                              hxdBstJudge_LC* lc );

/*
** hxdBstJudge_Step_calc -
** 
**      calculates a step
*/
hxdBstJudge_Trigger*
hxdBstJudge_Step_calc( hxdBstJudge_LC* lc, long istep, double winsz, double sigma,
                       double maxchi, double width_frac, double delchi, int* status );

/*
** hxdBstJudge_Step_fit_func -
** 
**      step fit function for mpfit fit, basically constant plus linear
**      plus hyperbolic tangent.
*/
int hxdBstJudge_Step_fit_func( int m, int n, double *p, double *deviates,
                               double **derivs, void *private );

/*
** hxdBstJudge_Step_stepfunc -
**
*/
double hxdBstJudge_Step_stepfunc( double x, double* p );

/*
** hxdBstJudge_Step_ftest -
**
**      computes f-statistic and related probability given an old
**      chi-squared value and its related dof, and a new chi-squared
**      and related dof.
*/
void hxdBstJudge_Step_ftest( double chisqold, int dof_old, double chisqnew, 
                             int dof_new, double* fstat, double* fprob );

/*
** hxdBstJudge_Step_incbeta -
**
**      Incomplete beta function I_x(a, b). based on NR routine.
*/
double hxdBstJudge_Step_incbeta( double a, double b, double x );

/*
** hxdBstJudge_Step_cfbeta -
**
**      Incomplete Beta function, computed using continuous fraction.
**      Based on NR routine.
*/
double hxdBstJudge_Step_cfbeta( double a, double b, double x );

#endif
