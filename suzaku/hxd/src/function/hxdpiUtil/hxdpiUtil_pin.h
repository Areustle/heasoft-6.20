#ifndef _HXDPIUTIL_PIN_H_
#define _HXDPIUTIL_PIN_H_


void   hxdpiUtil_pin_init(char* hxd_pinghf_fname, 
			  char* hxd_pinlin_fname, 
			  int* istat);

void   hxdpiUtil_pin_correct(HxdEventFits02 *data,
			     double *upi_pin0, double *upi_pin1,
			     double *upi_pin2, double *upi_pin3,
			     int *pi_pin0,     int *pi_pin1,
			     int *pi_pin2,     int *pi_pin3);

void hxdpiUtil_pin_AE_correct(int unit, int pin0, int pin1, int pin2, int pin3,
			      double *ae_pi_pin0, 
			      double *ae_pi_pin1,
			      double *ae_pi_pin2,
			      double *ae_pi_pin3);

void hxdpiUtil_pin_Gain_correct(int unit, 
				double ae_pi_pin0, double ae_pi_pin1,
				double ae_pi_pin2, double ae_pi_pin3,
				double *double_pi_pin0, double *double_pi_pin1,
				double *double_pi_pin2, double *double_pi_pin3);

void hxdpiUtil_pin_digitalyze_upi(double double_pi_pin0, double double_pi_pin1,
				  double double_pi_pin2, double double_pi_pin3,
				  double *upi_pin0, double *upi_pin1,
				  double *upi_pin2, double *upi_pin3);

void hxdpiUtil_pin_digitalyze_pi( double *upi_pin0, double *upi_pin1,
				  double *upi_pin2, double *upi_pin3,
				  int *pi_pin0, int *pi_pin1,
				  int *pi_pin2, int *pi_pin3 );
#endif
