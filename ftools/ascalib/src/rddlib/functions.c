/*****************************************************************************
* This file contains a number of routines for calculating special functions 
* needed to calculate the RDD models.
* For the most part these were taken from Numerical recipies.
*****************************************************************************/

#include <math.h>
#include "rdd.h"

#define DEBUG

/***************************************************************************
****************************************************************************
* Natural log of the Gamma function. This routine was swiped from numerical
* recipies and is needed to calculate the Poisson distribution at 
* an arbitrary real values point adn needed for convolution.
* gammln(x) = ln ((x-1)! for integer x.
****************************************************************************/
double rdd_lngamma(double x) {
double y,tmp,ser;
static double cof[6]={76.18009172947146,-86.50532032941677,
        24.01409824083091,-1.231739572450155,
                0.1208650973866179e-2,-0.5395239384953e-5};
int j;

y=x;
tmp=x+5.5;
tmp -= (x+0.5)*log(tmp);
ser=1.000000000190015;
for (j=0;j<=5;j++) ser += cof[j]/++y;

return -tmp+log(2.5066282746310005*ser/x);

} /* end of rdd_lngamma function */

/***************************************************************************
****************************************************************************
* two minus error function "erfc". This is required by the
* exponential part of the Rasmussen distribution
* This routine is a modified version of one swiped from
* Numerical Recipies
**************************************************************************/
double rdd_two_minus_erfc(double x) {
double t,z,ans;

z=fabs(x);
t=1.0/(1.0+0.5*z);

ans=t*exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196+t*(0.09678418+
          t*(-0.18628806+t*(0.27886807+t*(-1.13520398+t*(1.48851587+
          t*(-0.82215223+t*0.17087277)))))))));

/*
printf("rdd_two_minus_erfc: x=%g ans=%g\n",x,ans);
*/

return x >= 0.0 ? 2.0-ans : ans;

} /* end of rdd_two_minus_erfc function */

/***************************************************************************
****************************************************************************
* Possion distribution of a real valued quantity.
* This gives the probability of x Poisson events given a mean number
* of lambda. This is exp(-lambda)*lambda^x/x!
* note we use the gamma function to calculate foctorials of real numbers
* and we code the product of factors as a sum of logarithms to avoid
* overflow.
* Note this function is not normalized. See rdd_poisson_norm
***************************************************************************/
double rdd_poisson(double x, double lambda) {

static double old_lambda=1.;
static double old_loglambda=0.;
double loglambda;

if(lambda==old_lambda) loglambda=old_loglambda;
else {
    loglambda=log(lambda);
    old_lambda=lambda;
    old_loglambda=loglambda;
}

return( exp(-lambda + x*loglambda - rdd_lngamma(x+1.0) ) );

} /* end of rdd_poisson */
