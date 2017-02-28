#include <stdio.h>
#include <math.h>
#include "cfortran.h"

static struct noRTpara {
	double dE;		/* delta Energy (keV) when integration */
	double layer;	/* additional Xe layer */
	double sigma;	/* gaussian sigma for smoothing [%] */
	double Acons;	/* normalization constant */
	double Ejump;	/* PH jump at Xe-L edge */
	double recap;	/* escape recapture rate */
} GISnoRTpara[2] = {
	{ 0.04, 0.122, 3.0, 1.010, 1.20, 0.0 },
	{ 0.04, 0.101, 3.0, 1.010, 1.20, 0.0 }
};

double
get_correction_factor(x, p)
double x;
struct noRTpara *p;
{
#define FWHM	2.35482004503094938202313865291939927549477137877164107704
	static double Drift_Region = 1.0;	/* cm */
	double sig, energy, sigmakeV, sum[2];
	
	sig = p->Acons;
	if ( x < 0.40 ) return sig;
	
	sigmakeV = p->sigma / 100.0 / FWHM * 5.0;
	
	sum[0] = sum[1] = 0.0;
	for (energy = x - 3.0; energy <= x + 3.0; energy += p->dE) {
		double plasma, be, gas, lcm, jump, mu, trans[2];
		if ( energy < 0.40 ) continue;
		gis_elements_trans(energy, &plasma, &be, &gas);
/* gas : He + Xe 1mm transmission */
		lcm = Drift_Region;
		trans[0] = 1.0 - pow(gas, 10*lcm);
		lcm = Drift_Region + p->layer;
		trans[1] = 1.0 - pow(gas, 10*lcm);
		jump = 0.0;
		if ( 4.785 < energy ) {
			jump = p->Ejump;
			trans[1] += p->recap;
		}
		mu = ( x - (energy-jump) ) / sigmakeV;
		sum[0] += trans[0] * exp(-mu*mu/2);
		sum[1] += trans[1] * exp(-mu*mu/2);
	}
	if( 0.0 != sum[0] ) {
		sig = p->Acons * sum[1] / sum[0];
	}
	return sig;
}

#if defined(vms)
#define get_gis_norti get_gis_norti_
#endif

double
get_gis_norti(energy, instrum)
double energy;
int instrum;
{
        return get_correction_factor(energy, &GISnoRTpara[instrum-2]);
}

FCALLSCFUN2(DOUBLE,get_gis_norti,GET_GIS_NORTI,get_gis_norti,DOUBLE,INT)
