#ifndef _HXD_PI_UTIL_OLD_GSO_H_
#define _HXD_PI_UTIL_OLD_GSO_H_
#include "hxdeventFitsUtil.h"
#include "hxdcaldbUtil.h"

typedef struct {
  double peak_slow[HXD_WEL_N_UNIT][HXDGSOGHF_PMT_N_LINE];
  double peak_slow_err[HXD_WEL_N_UNIT][HXDGSOGHF_PMT_N_LINE];
  double peak_fast[HXD_WEL_N_UNIT][HXDGSOGHF_PMT_N_LINE];
  double peak_fast_err[HXD_WEL_N_UNIT][HXDGSOGHF_PMT_N_LINE];
} hxdpiUtil_old_ghfdata;

void hxdpiUtil_old_gso_init(char* hxd_gsoght_fname, char* hxd_gsolin_fname, 
			double tstart, double tstop, int* istat);

void hxdpiUtil_old_gso_correct(HxdEventFits02* eventdata, 
			   hxdpiUtil_old_HK*  hkdata,
			   hxdpiUtil_old_EHK* ehkdata,
			   double* upi_fast, 
			   double* upi_slow,
			   int *pi_fast,
			   int *pi_slow );

void hxdpiUtil_old_gso_correct_DNL(int unit,
			       int pha_fast,
			       int pha_slow,
			       double *adcdnl_pi_fast,
			       double *adcdnl_pi_slow);

void hxdpiUtil_old_gso_correct_INL(int unit, 
			       double adcdnl_pi_fast, double adcdnl_pi_slow,
			       double *adcinl_pi_fast,double *adcinl_pi_slow);

void hxdpiUtil_old_gso_correct_gain(int unit,int time, 
				double adcinl_pi_fast, double adcinl_pi_slow,
				double *gain_pi_fast, double *gain_pi_slow);

void hxdpiUtil_old_gso_correct_nonlinear(int unit,
				     double gain_pi_fast,double gain_pi_slow,
				     double *gso_pi_fast,double *gso_pi_slow);

void hxdpiUtil_old_gso_digitalize_upi(int unit, 
				  double gso_pi_fast,double gso_pi_slow,
				  double *upi_fast, double *upi_slow);

void hxdpiUtil_old_gso_digitalize_pi( double *upi_fast, double *upi_slow,
				  int *pi_fast,	   int *pi_slow);

void hxdpiUtil_old_gso_ght_calcghf(int unit, double time, 
			       float t_saa_hxd, double *temp_well,
			       hxdpiUtil_old_ghfdata *ghfdata );

void hxdpiUtil_old_gso_ghf_fit(int unit,  double time, 
			   hxdpiUtil_old_ghfdata* ghfdata );


#endif
