/*
 * hxdpiUtil_pin_correct.c
 *      created by T.Kishishita@RIKEN, 2005-01-19  (hxdpiUtil_ver 0.2.0)
 *
 *      modified by T.Kishishita@RIKEN, 2005-01-25 (hxdpiUtil_ver 0.2.1)
 *
 *      support FITS I/O, T.Kishishita, 2005-02-02 (hxdpiUtil_ver 0.2.2)
 *
 *      read pin gain history, Y.Terada, 2005-02-05 v0.2.3
 *
 *      modified digitalize section, T.Kishishita, 2005-05-10 v.0.2.6
 *   
 *      PI(PIN) int -> double, T.Kishishita, 2005-06-03, v0.2.9
 *
 *      debug, Y.Terada, 2005-06-03, v0.3.0
 *
 *	change function hxdpiUtil_pin_AE_correct, 
 *                             T.Kishishita, 2005-08-29, v.3.6
 *	change invalid definition, 2005-09-17 (Y.Terada)
 *      modify hxdpiUtil_pin_AE_correct (in case of 0&255 pha chaanel), 
 *                             T.Kishishita, 2005-09-30, v0.3.8
 *      delete C++ Style coding. Y.Terada, 2005-10-27
 *
 *      0.7.0: 2006-05-26  PI=0 for PHA=255 (M.K)
 *      0.7.2: 2006-08-18, Memory save, by Y.Terada
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aste_rand.h"
#include "hxdcaldbUtil.h"
#include "hxdpiUtil_old.h"

static char tool_name[] = "hxdpiUtil_old_pin";

#define HXDPIUTIL_PIN_INIT      0xff
#define HXDPIUTIL_PIN_NO_INIT   0x00
#define HXD_PIN_MAX_PI_CH 256
/*
static HxdPinLin_ADCINL pin_adcinl;
static HxdPinLin_GAIN   pin_gain;
static PIN_GHF          pin_ghf;
*/

static HxdPinLin_ADCINL* pin_adcinl = NULL;
static HxdPinLin_GAIN*   pin_gain   = NULL;
static PIN_GHF*          pin_ghf    = NULL;

static int init = HXDPIUTIL_PIN_NO_INIT;

const  double  hxdpiUtil_old_pi_Invalid = 0.0;

/* Definition of Functions */
void hxdpiUtil_old_pin_init(char* hxdpin_gainhistory_filename,
			char* hxdpinlin_fname,
			int* istat){		
  int status, ch, unit;

  /****** Memory Allocation **********/
/*fprintf(stdout, "pin %d+%d+%d Bytes\n",
	  sizeof(HxdPinLin_ADCINL), sizeof(HxdPinLin_GAIN),
	  sizeof(PIN_GHF));*/

  if(pin_adcinl == NULL){
    pin_adcinl = (HxdPinLin_ADCINL*) malloc( sizeof(HxdPinLin_ADCINL) );
  } else {
    fprintf(stderr, "hxdpiUtil_old: Memory, pin_adcinl, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if(pin_gain == NULL){
    pin_gain = (HxdPinLin_GAIN*) malloc( sizeof(HxdPinLin_GAIN) );
  } else {
    fprintf(stderr, "hxdpiUtil_old: Memory, pin_gain, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if(pin_ghf == NULL){
    pin_ghf = (PIN_GHF*) malloc( sizeof(PIN_GHF) );
  } else {
    fprintf(stderr, "hxdpiUtil_old: Memory, pin_ghf, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  /****** read FITS **********/
  status = hxdcaldbUtil_hxdpinlin_open_FITS(hxdpinlin_fname);
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDPINLIN FITS file open error\n", tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }
  status = hxdcaldbUtil_hxdpinlin_adcinl_read_FITS(pin_adcinl);
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDPINLIN FITS file read error (ADC INL)\n", tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }
  status = hxdcaldbUtil_hxdpinlin_gain_read_FITS(pin_gain);
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDPINLIN FITS file read error (GAIN)\n", tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }
  
  status = hxdcaldbUtil_hxdpinlin_close_FITS();
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDPINLIN FITS file close error\n", tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }

  /** read pin gain history file **/
  status = hxdcaldbUtil_pingainhist_open_FITS(hxdpin_gainhistory_filename);
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDPIN GHF FITS file open error\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }
  status = hxdcaldbUtil_pingainhist_read_FITS(pin_ghf);
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDPIN GHF FITS file read error (GAIN)\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }
  status = hxdcaldbUtil_pingainhist_close_FITS();
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDPIN GHF FITS file close error\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }

  init = HXDPIUTIL_PIN_INIT;
  return;
}									   

void 
hxdpiUtil_old_pin_correct(HxdEventFits02 *data,
		      double *upi_pin0,  double *upi_pin1,
		      double *upi_pin2,  double *upi_pin3,
		      int *pi_pin0,      int *pi_pin1,
		      int *pi_pin2,      int *pi_pin3){
  int pin0, pin1, pin2, pin3;
  int unit;
  double ae_pi_pin0,ae_pi_pin1,ae_pi_pin2,ae_pi_pin3;
  double double_pi_pin0, double_pi_pin1, double_pi_pin2, double_pi_pin3;

  if (init != HXDPIUTIL_PIN_INIT){
    fprintf(stderr, "%s: pin_correct: Please Init First!\n", tool_name);
    return;
  }

  pin0 = (int)data->pha_pin0;
  pin1 = (int)data->pha_pin1;
  pin2 = (int)data->pha_pin2;
  pin3 = (int)data->pha_pin3;
  unit = (int)data->unitid;

  hxdpiUtil_old_pin_AE_correct(unit, pin0, pin1, pin2, pin3,
			   &ae_pi_pin0, &ae_pi_pin1,
			   &ae_pi_pin2, &ae_pi_pin3);
  
  hxdpiUtil_old_pin_Gain_correct(unit, 
			     ae_pi_pin0, ae_pi_pin1,
			     ae_pi_pin2, ae_pi_pin3,
			     &double_pi_pin0, &double_pi_pin1,
			     &double_pi_pin2, &double_pi_pin3);
  
  hxdpiUtil_old_pin_digitalyze_upi(double_pi_pin0, double_pi_pin1,
			       double_pi_pin2, double_pi_pin3,
			       upi_pin0, upi_pin1, upi_pin2, upi_pin3);

  hxdpiUtil_old_pin_digitalyze_pi(upi_pin0, upi_pin1, upi_pin2, upi_pin3,
			       pi_pin0, pi_pin1, pi_pin2, pi_pin3 );

  if ( pin0 == 255 ) *upi_pin0 = hxdpiUtil_old_pi_Invalid ;
  if ( pin1 == 255 ) *upi_pin1 = hxdpiUtil_old_pi_Invalid ;
  if ( pin2 == 255 ) *upi_pin2 = hxdpiUtil_old_pi_Invalid ;
  if ( pin3 == 255 ) *upi_pin3 = hxdpiUtil_old_pi_Invalid ;


}			

/* Inner Functions */
void hxdpiUtil_old_pin_AE_correct(int unit, int pin0, int pin1, int pin2, int pin3,
			      double *ae_pi_pin0, 
			      double *ae_pi_pin1,
			      double *ae_pi_pin2,
			      double *ae_pi_pin3){

  double rnd_width_high_pin0, rnd_width_low_pin0;
  double rnd_width_high_pin1, rnd_width_low_pin1;
  double rnd_width_high_pin2, rnd_width_low_pin2;
  double rnd_width_high_pin3, rnd_width_low_pin3;

  if (init != HXDPIUTIL_PIN_INIT){
    fprintf(stderr, "%s: pin_AE_correct: Please Init First!\n", tool_name);
    return;
  }

  if(pin0==0){
    rnd_width_low_pin0 = 0.0;
  }else{
    rnd_width_low_pin0 = (pin_adcinl->ae_pi_pin0[unit][pin0] 
			  - pin_adcinl->ae_pi_pin0[unit][pin0 - 1])/2.;
  }

  if(pin1==0){
    rnd_width_low_pin1 = 0.0;
  }else{
    rnd_width_low_pin1 = (pin_adcinl->ae_pi_pin1[unit][pin1] 
			  - pin_adcinl->ae_pi_pin1[unit][pin1 - 1])/2.;
  }
  if(pin2==0){
    rnd_width_low_pin2 = 0.0;
  }else{
    rnd_width_low_pin2 = (pin_adcinl->ae_pi_pin2[unit][pin2] 
			  - pin_adcinl->ae_pi_pin2[unit][pin2 - 1])/2.;
  }
  if(pin3==0){
    rnd_width_low_pin3 = 0.0;
  }else{
    rnd_width_low_pin3 = (pin_adcinl->ae_pi_pin3[unit][pin3] 
			  - pin_adcinl->ae_pi_pin3[unit][pin3 - 1])/2.;
  }

  if(pin0==255){
    rnd_width_high_pin0 = 0.0;
  }else{
    rnd_width_high_pin0 = (pin_adcinl->ae_pi_pin0[unit][pin0 + 1] 
			   - pin_adcinl->ae_pi_pin0[unit][pin0])/2.;
  }
  if(pin1==255){
    rnd_width_high_pin1 = 0.0;
  }else{
    rnd_width_high_pin1 = (pin_adcinl->ae_pi_pin1[unit][pin1 + 1] 
			   - pin_adcinl->ae_pi_pin1[unit][pin1])/2.;
  }
  if(pin2==255){
    rnd_width_high_pin2 = 0.0;
  }else{
    rnd_width_high_pin2 = (pin_adcinl->ae_pi_pin2[unit][pin2 + 1] 
			   - pin_adcinl->ae_pi_pin2[unit][pin2])/2.;
  }
  if(pin3==255){
    rnd_width_high_pin3 = 0.0;
  }else{
    rnd_width_high_pin3 = (pin_adcinl->ae_pi_pin3[unit][pin3 + 1] 
			   - pin_adcinl->ae_pi_pin3[unit][pin3])/2.;
  }


  if(aste_drndts()>0.5){
    *ae_pi_pin0 = pin_adcinl->ae_pi_pin0[unit][pin0] + aste_drndts()*rnd_width_high_pin0;
  } else {
    *ae_pi_pin0 = pin_adcinl->ae_pi_pin0[unit][pin0] - aste_drndts()*rnd_width_low_pin0;
  }

  if(aste_drndts()>0.5){
    *ae_pi_pin1 = pin_adcinl->ae_pi_pin1[unit][pin1] + aste_drndts()*rnd_width_high_pin1;
  } else {
    *ae_pi_pin1 = pin_adcinl->ae_pi_pin1[unit][pin1] - aste_drndts()*rnd_width_low_pin1;
  }

  if(aste_drndts()>0.5){
    *ae_pi_pin2 = pin_adcinl->ae_pi_pin2[unit][pin2] + aste_drndts()*rnd_width_high_pin2;
  } else {
    *ae_pi_pin2 = pin_adcinl->ae_pi_pin2[unit][pin2] - aste_drndts()*rnd_width_low_pin2;
  }

  if(aste_drndts()>0.5){
    *ae_pi_pin3 = pin_adcinl->ae_pi_pin3[unit][pin3] + aste_drndts()*rnd_width_high_pin3;
  } else {
    *ae_pi_pin3 = pin_adcinl->ae_pi_pin3[unit][pin3] - aste_drndts()*rnd_width_low_pin3;
  }

}

void hxdpiUtil_old_pin_Gain_correct(int unit, 
				double ae_pi_pin0, double ae_pi_pin1,
				double ae_pi_pin2, double ae_pi_pin3,
				double *double_pi_pin0, double *double_pi_pin1,
				double *double_pi_pin2, double *double_pi_pin3){
  if (init != HXDPIUTIL_PIN_INIT){
    fprintf(stderr, "%s: pin_Gain_correct: Please Init First!\n", tool_name);
    return;
  }

  *double_pi_pin0 = ((ae_pi_pin0 - pin_gain->pin0_offset[unit]) 
		     / pin_gain->pin0_gain[unit]);
  *double_pi_pin1 = ((ae_pi_pin1 - pin_gain->pin1_offset[unit]) 
		     / pin_gain->pin1_gain[unit]);
  *double_pi_pin2 = ((ae_pi_pin2 - pin_gain->pin2_offset[unit]) 
		     / pin_gain->pin2_gain[unit]);
  *double_pi_pin3 = ((ae_pi_pin3 - pin_gain->pin3_offset[unit]) 
		     / pin_gain->pin3_gain[unit]);
}

void hxdpiUtil_old_pin_digitalyze_upi(double double_pi_pin0, double double_pi_pin1,
				  double double_pi_pin2, double double_pi_pin3,
				  double *upi_pin0, double *upi_pin1,
				  double *upi_pin2, double *upi_pin3){
  
  const  double ENE_MIN = 0.375;
  const  double ENE_STEP = 0.375;
  
  if( (  ((double_pi_pin0 - ENE_MIN)/ENE_STEP) < 0.0) || 
     (((double_pi_pin0 - ENE_MIN)/ENE_STEP) >= (double)HXD_PIN_MAX_PI_CH)){
    *upi_pin0 = hxdpiUtil_old_pi_Invalid;
  } else {
    *upi_pin0 = ((double_pi_pin0 - ENE_MIN)/ENE_STEP);
  }
  
  if( (  ((double_pi_pin1 - ENE_MIN)/ENE_STEP) < 0.0)  || 
     (((double_pi_pin1 - ENE_MIN)/ENE_STEP) >= (double)HXD_PIN_MAX_PI_CH)){
    *upi_pin1 = hxdpiUtil_old_pi_Invalid;
  } else {
    *upi_pin1 = ((double_pi_pin1 - ENE_MIN)/ENE_STEP);
  }
  
  if( (  ((double_pi_pin2 - ENE_MIN)/ENE_STEP) < 0.0)  ||  
     (((double_pi_pin2 - ENE_MIN)/ENE_STEP) >= (double)HXD_PIN_MAX_PI_CH)){
    *upi_pin2 = hxdpiUtil_old_pi_Invalid;
  } else {
    *upi_pin2 = ((double_pi_pin2 - ENE_MIN)/ENE_STEP);
  }
  
  if( (  ((double_pi_pin3 - ENE_MIN)/ENE_STEP)  < 0.0)  ||  
     (((double_pi_pin3 - ENE_MIN)/ENE_STEP) >= (double)HXD_PIN_MAX_PI_CH)){
    *upi_pin3 = hxdpiUtil_old_pi_Invalid;
  } else {
    *upi_pin3 = ((double_pi_pin3 - ENE_MIN)/ENE_STEP);
  }
  
}

void hxdpiUtil_old_pin_digitalyze_pi( double *upi_pin0, double *upi_pin1,
				  double *upi_pin2, double *upi_pin3,
				  int *pi_pin0, int *pi_pin1,
				  int *pi_pin2, int *pi_pin3 ){

  *pi_pin0 = hxdpiUtil_old_half_adjust( upi_pin0 );
  *pi_pin1 = hxdpiUtil_old_half_adjust( upi_pin1 );
  *pi_pin2 = hxdpiUtil_old_half_adjust( upi_pin2 );
  *pi_pin3 = hxdpiUtil_old_half_adjust( upi_pin3 );

}
