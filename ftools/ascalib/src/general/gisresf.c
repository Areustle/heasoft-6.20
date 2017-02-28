#include <stdio.h>
#include <math.h>
#include "cfortran.h"

#ifdef vms
#define Get_gis_psf Get_gis_psf_
#define Get_gis_eff Get_gis_eff_
#endif

double Get_gis_psf(xmm, ymm, Ekev, xoff, yoff)
double xmm, ymm, Ekev, xoff, yoff;
{
        extern double gis_psf();
        return gis_psf(xmm, ymm, Ekev, xoff, yoff);
}

FCALLSCFUN5(DOUBLE,Get_gis_psf,GET_GIS_PSF,get_gis_psf,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE)

double
Get_gis_eff(lo, hi, be, correct)
double lo, hi, be;
int correct;
{
	extern double Be_thick;
	extern double gis_eff();
	
	static struct {
		double lo, hi, eff;
	} sav = { 0.0, 0.0, -1.0 };
	
	be *= 1.0e-4;	/* um -> cm */
	if ( sav.lo != lo || sav.hi != hi || be != Be_thick || sav.eff < 0.0 ) {
		int n;
		double energy, eff_lo, eff_mi, eff_hi, step;
		
		Be_thick = be;
		eff_lo = gis_eff(lo, correct);
		eff_mi = gis_eff((lo+hi)/2, correct);
		eff_hi = gis_eff(hi, correct);
		if ( 0.05 < fabs(eff_lo-eff_hi)/eff_mi ) {
			step = ( hi - lo ) / 20;
			n = 0;
			eff_mi = 0.0;
			for (energy = lo + step/2; energy < hi; energy += step) {
				eff_mi += gis_eff(energy, correct);
				n++;
			}
			eff_mi /= n;
		}
		sav.lo = lo;
		sav.hi = hi;
		sav.eff = eff_mi;
	}
	
	return sav.eff;
}

FCALLSCFUN4(DOUBLE,Get_gis_eff,GET_GIS_EFF,get_gis_eff,DOUBLE,DOUBLE,DOUBLE,INT)



