#include <stdio.h>
#include <math.h>
#include "cfortran.h"

static struct arffilter_para {
	int gauss_num;	/* number of Gaussian */
	double cons;	/* constant */
	double center[4];	/* Gaussian center */
	double sigma[4];	/* Gaussian sigma */
	double norm[4];	/* Gaussian normalization */
} arffp[2] = {
	{ 4, 1.0, 0.800, 5.389, 6.172, 15.00,
	  1.143, 2.096, 1.000, 8.296,
	  8.301e-3, -3.644e-2, 3.846e-2, 2.000e-2 },
	{ 3, 1.0, 0.800, 1.706, 10.00, 0.00,
	  1.561, 0.7843, 1.806, 0.00,
	  -6.835e-2, 6.867e-2, 7.075e-2, 0.00 }
};

/*
static double arf_norm[4] = { 0.885, 0.906, 1.039, 0.979 };
static double arf_norm[4] = { 1.130, 1.104, 0.962, 1.021 };
static double arf_norm[4] = { 1.031, 1.007, 0.878, 0.932 };
*/

static double arf_norm[4] = { 1.057, 1.032, 0.900, 0.909 };

/*
Flux level
    S0, S1, S2 : -2.5%
    s3         : +2.5%
*/


double
gaussF(x, a, b, c)
double x, a, b, c;
{
	double dum;
	dum = (a-x)/b;
	return c * exp(-dum*dum);
}

double
get_arf_correction_factor(x, sensor_ID)
double x;
int sensor_ID;
{
	int i,sid;
	double sig=0.0;
	switch ( sensor_ID ) {
	case 0:
	case 1:
		sig = 0.0;
		sig += arffp[0].cons;
		for(i=0;i<arffp[0].gauss_num;i++) {
			sig += gaussF(x,arffp[0].center[i],arffp[0].sigma[i],arffp[0].norm[i]);
		}
		sig += arffp[0].cons;
		for(i=0;i<arffp[1].gauss_num;i++) {
			sig += gaussF(x,arffp[1].center[i],arffp[1].sigma[i],arffp[1].norm[i]);
		}
		sig /= 2.0;
		break;
	case 2:
	case 3:
		sid = sensor_ID - 2;
		sig = 0.0;
		sig += arffp[sid].cons;
		for(i=0;i<arffp[sid].gauss_num;i++) {
			sig += gaussF(x,arffp[sid].center[i],arffp[sid].sigma[i],arffp[sid].norm[i]);
		}
		break;
	}
	return sig * arf_norm[sensor_ID];
}

#if defined(vms)
#define get_arf_corr get_arf_corr_
#endif

double
get_arf_corr(energy, sensor_ID)
double energy;
int sensor_ID;
{
        return get_arf_correction_factor(energy, sensor_ID);
}

FCALLSCFUN2(DOUBLE,get_arf_corr,GET_ARF_CORR,get_arf_corr,DOUBLE,INT)

