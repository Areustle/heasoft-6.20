/*
 *   hxdpiUtil_gso
 *        v0.2.0; 2005-01-18, created by S.Hirakuri
 *        v0.2.2; 2005-02-02, support FITS I/O and debug by T.Kishishita
 *        v0.2.3; 2005-02-05, read gain history by Y.Terada
 *        v0.2.4; 2005-02-07, by Y.Terada
 *        v0.2.7; 2005-05-13, add gain correction by S.Hirakuri
 *                            pi(int -> double)
 *        v0.2.8; 2005-06-03, read ghf FITS, by Y.Tearda
 *        v0.3.2; 2005-06-13, selection fit model of GHF by S.Hirakuri
 *        v0.3.3; 2005-06-13, debug
 *        v0.3.4; 2005-06-13, debug gso_correct_INL by S.Hirakuri
 *        v0.3.5; 2005-06-13, redebug gso_correct_INL by S.Hirakuri 
 *        v0.3.7; 2005-09-10, C++ style comment-out -> C style by M.K
 *        v0.3.8; 2005-09-17, change invalid channel to 0.0
 *        v0.4.0: 2005-10-12, change gain correction method by S.Hirakuri
 *        v0.4.3: 2005-11-03, 153Gd energy 151 -> 146 keV by M.K
 *        v0.4.4: 2005-11-18, Fast/Slow combined fit by M.K
 *        v0.4.5: 2005-11-21, Err*3 for 153Gd(148keV) peak by M.K
 *        v0.4.6: 2005-11-21, 153Gd peak fitted with PL func by M.K
 *        v0.4.7: 2005-11-22, Iteration fit with the "scaled" intercept by M.K
 *        v0.4.8: 2005-12-06, INL separated into AE and S parts by T.K & M.K
 *        v0.4.9: 2005-12-20, New 152Gd peak (dgc), 2dim func by M.K
 *                            (based on check with 0.4.5.1-0.4.7.1)
 *                            Separated INL for GSO and AE are introduced
 *        v0.5.0: 2005-12-21, artificial INL correction by M.K
 *        v0.6.0: 2006-01-05, artificial INL (array for 16units), 
 *                            gso_correct_nonlinear() added  by M.K
 *        v0.6.1: 2006-01-10, different random number for Slow/Fast in DNL
 *        v0.6.2: 2006-01-31, debug read GHF, by Y.Terada
 *        v0.6.3: 2006-06-21, artificial INL modified (time variation) by M.K
 *        v0.7.0: 2006-05-26, for short term trend GHF (M.K)
 *        v0.7.2: 2006-08-18, Memory save, by Y.Terada
 *        v0.7.3: 2006-08-24, for invalid quality in ghf, by Y.Terada
 *        v0.7.4: 2006-08-24, debug, pi nan value, by Y.Terada
 *        v0.7.5: 2006-09-06, debug, pi nan -> invaled valud (0.0), by T.K
 *        v2.1.0: 2007-05-10, GHF -> GHT (M.K)
 *        v2.1.1: 2007-05-18, debug (M.K)
 *        v2.1.2: 2007-05-27, ght_numrow[3] (M.K)
 *        v2.1.3: 2007-06-12, debug info (M.K)
 *        v2.1.4: 2007-10-11, debug in hxdpiUtil_gso_ghf_fit by H.T.
 *        v2.2.0: 2008-02-28, remove artificial INL correction, instead,
 *                            convert PHA into LightYeild, and then into Energy
 *                            including small effect of pedestal change by S.Yamada
 *        v2.2.1: 2009-02-18, using a 511keV-fit result over one obs, in place of GHT.          
 *        v2.2.2: 2009-02-18, modify Fast offset so as to adjust FAST to SLOW. (S. Yamada)           
 *        v2.3.0: 2009-07-19, inplement saapass to evaluate gain,  by S.Yamada			      
 *        v2.3.1: 2009-08-10, modifiy gain calc,  by S.Yamada
 *        v2.3.2: 2009-10-19, delete // mark for ftools release, by Y.Terada
 *        v2.4.0: 2009-10-20, use gsoghp file by Y.Terada
 *        v2.4.1: 2009-10-20, gsoghp --> gsogpt by Y.Terada
 *        v2.4.2: 2010-01-05, read version of gsogpt by Y.Terada
 *        v2.4.3: 2013-10-09, delete unused variables by Y.Terada
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "aste_rand.h"
#include "hxdpiUtil.h"

#define HXD_GSO_MAX_PI_CH     512
#define HXD_GSO_MAX_EN       1025
#define HXD_GSO_MIN_EN          1
#define HXDPIUTIL_PI_INVALID    0

#define INHI_ENERGY           511 
#define HXDPIUTIL_GSO_SWITCH_TIME 195000000.0

/* static char version[] = "version 2.4.3"; */
static char tool_name[] = "hxdpiUtil_gso";


/* Ground(Tantai 200406) - In orbit pedestal */
static double wpu_diffpedestal_slow[16] = {
  7.1, 7.1, 7.2, 7.6, 
  7.4, 6.4, 7.4, 7.3, 
  7.9, 7.2, 7.0, 6.5, 
  7.6, 7.0, 6.5, 6.8
};

static double wpu_diffpedestal_fast[16] = {
  0., 0., 0., 0., 
  0., 0., 0., 0., 
  0., 0., 0., 0., 
  0., 0., 0., 0.
};

/* Ground(Tantai 200406) */
static double wpu_offset_slow[16] = {
 30.5, 31.5, 32.9, 30.9,
 30.3, 31.7, 28.6, 32.6,
 31.6, 30.7, 29.5, 33.5,
 31.8, 33.2, 32.1, 30.0
};
static double wpu_offset_fast[16] = {
 -4.4, 0.1, -0.9, 1.7,
 -2.1, -1.8, -1.8, -0.1,
 -1.1, -0.5, 0.1, 0.6,
 -0.4, 5.1, -0.7, -1.5
};

/* Ground (GSO lightyeild exp by Kitaguchi 2004 ) */
static double ly_offset_energy[16] = {
  8., 8., 8., 8.,
  8., 8., 8., 8.,
  8., 8., 8., 8.,
  8., 8., 8., 8.
};

#define HXDPI_NUM_SAA_PASS 8
#define HXDPI_GSO_ID_FAST 0
#define HXDPI_GSO_ID_SLOW 1
double *longterm_a;
double *longterm_b;
double *longterm_c;
double *longterm_d;
double *longterm_e;
double *temp_a;
double *temp_b;
double *tsaa_a;
double *tsaa_b;
double *tsaa_c;    
double *mod_ga_1a;
double *mod_ga_1b;
double *mod_ga_1c;
double *mod_ga_2a;
double *mod_ga_2b;
double *mod_ga_2c;
double *mod_ga_3a;
double *mod_ga_3b;
double *mod_ga_3c;
double *mod_c;

/* copy ver 0.4.9
   Non-linearity of GSO light yield
   Function : Ene_mod = Ene * (1.0-1.0/exp(photo_inl[0]*Ene+photo_inl[1]))
*/
static double gso_photo_inl_fast[2] = { 0.0535402, 0.500384 };
static double gso_photo_inl_slow[2] = { 0.0516020, 0.270684 };


#define HXDPIUTIL_GSO_INIT      0xff
#define HXDPIUTIL_GSO_NO_INIT   0x00

static HxdGsoLin_ADCDNL* dnl_data = NULL;
static HxdGsoLin_ADCINL* inl_data = NULL;
/*
GSO_GHT* gdalpha_ght_data = NULL;
GSO_GHT* annihi_ght_data  = NULL;
GSO_GHT* gd153_ght_data   = NULL;
static GSO_GHT* gso_ght_data[HXDGSOGHT_PMT_N_LINE];
*/

static int      gpt_ver = HXDGSOGPT_VERSION_UNDEFINED;
static GSO_GPT* gpt_data;

static double *ghf_slow_slope     = NULL;
static double *ghf_fast_slope     = NULL;
static double *ghf_slow_intercept = NULL;
static double *ghf_fast_intercept = NULL;

static int init = HXDPIUTIL_GSO_NO_INIT;
static int gpt_numrow = 0;

static double time20050710 = 174268800;

#define DEBUG 0

void hxdpiUtil_gso_init(char* hxd_gsogpt_fname, char* hxd_gsolin_fname, 
			double tstart, double tstop, int* istat){
  int status;
  /* int i,j; */
  int irow;

  *istat = HXDPIUTIL_OK;

  /** Memory Allocation of CALDB Data **/
  if(dnl_data == NULL){
    dnl_data = (HxdGsoLin_ADCDNL*) malloc( sizeof(HxdGsoLin_ADCDNL) );
  } else {
    fprintf(stderr, "hxdpiUtil: Memory, dnl_data, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if(inl_data == NULL){
    inl_data = (HxdGsoLin_ADCINL*) malloc( sizeof(HxdGsoLin_ADCINL) );
  } else {
    fprintf(stderr, "hxdpiUtil: Memory, inl_data, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if(gpt_data == NULL){
     gpt_data = (GSO_GPT*) malloc( sizeof(GSO_GPT) );
  } else {
    fprintf(stderr,"hxdpiUtil: Memory, gpt_data, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if(ghf_slow_slope == NULL){
    ghf_slow_slope = malloc( sizeof(double)*HXD_WEL_N_UNIT );
  } else {
    fprintf(stderr,"hxdpiUtil: Memory, ghf_slow_slope, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if( ghf_fast_slope == NULL){
    ghf_fast_slope = malloc( sizeof(double)*HXD_WEL_N_UNIT );
  } else {
    fprintf(stderr,"hxdpiUtil: Memory, ghf_fast_slope, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if( ghf_slow_intercept == NULL){
    ghf_slow_intercept = malloc( sizeof(double)*HXD_WEL_N_UNIT );
  } else {
    fprintf(stderr,"hxdpiUtil: Memory, ghf_slow_intercept, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if( ghf_fast_intercept == NULL){
    ghf_fast_intercept = malloc( sizeof(double)*HXD_WEL_N_UNIT );
  } else {
    fprintf(stderr,"hxdpiUtil: Memory, ghf_fast_intercept, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  /** read gso calibration file **/
  status = hxdcaldbUtil_hxdgsolin_open_FITS(hxd_gsolin_fname);
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDGSOLIN FITS file open error\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }
  status = hxdcaldbUtil_hxdgsolin_adcdnl_read_FITS(dnl_data);
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDGSOLIN FITS file read error (ADC DNL)\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }
  status = hxdcaldbUtil_hxdgsolin_adcinl_read_FITS(inl_data);
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDGSOLIN FITS file read error (ADC INL)\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }
  status = hxdcaldbUtil_hxdgsolin_close_FITS();
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDGSOLIN FITS file close error\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }

  /** read gso gain history table **/
  status = hxdcaldbUtil_gsogpt_open_FITS(hxd_gsogpt_fname);
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDGSO GPT FITS file open error\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }

  status = 
    hxdcaldbUtil_gsogpt_read_VERSION_FITS(&gpt_ver);
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDGSO GPT FITS file read error (VERSION)\n",
	    tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }

  status = 
    hxdcaldbUtil_gsogpt_read_FITS(tstart, tstop, gpt_data, gpt_numrow );
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDGSO GPT FITS file read error\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }

  status = hxdcaldbUtil_gsogpt_close_FITS();
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDGSO GPT FITS file close error\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }
  init = HXDPIUTIL_GSO_INIT;
  /* printf("Opening GSO calibration files OK\n");
     fflush(stdout); */

  /** assignment **/
  for(irow=0; irow < gpt_data->nrow; irow++){
    if( (gpt_data->data[irow].start_time > tstart) )  break;
  }
  printf("nrow = %d, irow = %d\n\n", gpt_data->nrow, irow);
  longterm_a = (double *) gpt_data->data[irow-1].longterm_a;
  longterm_b = (double *) gpt_data->data[irow-1].longterm_b;
  longterm_c = (double *) gpt_data->data[irow-1].longterm_c;
  longterm_d = (double *) gpt_data->data[irow-1].longterm_d;
  longterm_e = (double *) gpt_data->data[irow-1].longterm_e;
  temp_a     = (double *) gpt_data->data[irow-1].temp_a;
  temp_b     = (double *) gpt_data->data[irow-1].temp_b;
  tsaa_a     = (double *) gpt_data->data[irow-1].tsaa_a;
  tsaa_b     = (double *) gpt_data->data[irow-1].tsaa_b;
  tsaa_c     = (double *) gpt_data->data[irow-1].tsaa_c;
  mod_ga_1a  = (double *) gpt_data->data[irow-1].mod_ga_1a;
  mod_ga_1b  = (double *) gpt_data->data[irow-1].mod_ga_1b;
  mod_ga_1c  = (double *) gpt_data->data[irow-1].mod_ga_1c;
  mod_ga_2a  = (double *) gpt_data->data[irow-1].mod_ga_2a;
  mod_ga_2b  = (double *) gpt_data->data[irow-1].mod_ga_2b;
  mod_ga_2c  = (double *) gpt_data->data[irow-1].mod_ga_2c;
  mod_ga_3a  = (double *) gpt_data->data[irow-1].mod_ga_3a;
  mod_ga_3b  = (double *) gpt_data->data[irow-1].mod_ga_3b;
  mod_ga_3c  = (double *) gpt_data->data[irow-1].mod_ga_3c;
  mod_c      = (double *) gpt_data->data[irow-1].mod_c;
  return;
}

void hxdpiUtil_gso_read_gsogpt_version(int *hxd_gsogpt_version, 
				       int *istat){
  *hxd_gsogpt_version = gpt_ver;

  if (gpt_ver == HXDGSOGPT_VERSION_UNDEFINED) {
    *istat = HXDPIUTIL_NG;
  } else {
    *istat = HXDPIUTIL_OK;
  }

  return;
}


void hxdpiUtil_gso_correct(HxdEventFits02 *eventdata, hxdpiUtil_HK* hkdata,
			   hxdpiUtil_EHK* ehkdata,
			   double* upi_fast,  double* upi_slow,
			   int *pi_fast,  int *pi_slow,int *saapass){
  int pha_fast, pha_slow;
  int unit;
  double time;
  float t_saa_hxd;
  double temp_well;
  /* double local_saapass; */

  double adcdnl_pi_fast, adcinl_pi_fast, gain_pi_fast, gso_pi_fast; 
  double adcdnl_pi_slow, adcinl_pi_slow, gain_pi_slow, gso_pi_slow;
  /* hxdpiUtil_ghfdata ghfdata; */

  /* int i; */

  if (init != HXDPIUTIL_GSO_INIT){
    fprintf(stderr, "%s: gso_correct: Please Init First!\n", tool_name);
    return;
  }

  pha_fast = eventdata->pha_fast;
  pha_slow = eventdata->pha_slow;
  time = eventdata->time;
  unit = eventdata->unitid;

  t_saa_hxd = ehkdata->t_saa_hxd;
  temp_well = hkdata->hxd_temp_body3_cal;


  /* hxdpiUtil_gso_ght_calcghf( unit, time, t_saa_hxd, temp_well, &ghfdata );*/

  /* hxdpiUtil_gso_ghf_fit( unit, time, &ghfdata ); */
  
  hxdpiUtil_gso_calcgain( unit, time, t_saa_hxd, temp_well, *saapass );

  hxdpiUtil_gso_correct_DNL(unit,pha_fast, pha_slow,
			    &adcdnl_pi_fast, &adcdnl_pi_slow);

  hxdpiUtil_gso_correct_INL(unit,adcdnl_pi_fast, adcdnl_pi_slow,
			    &adcinl_pi_fast, &adcinl_pi_slow);

  hxdpiUtil_gso_correct_gain(unit, time,
			     adcinl_pi_fast, adcinl_pi_slow,
			     &gain_pi_fast, &gain_pi_slow);

  hxdpiUtil_gso_correct_nonlinear(unit, gain_pi_fast, gain_pi_slow,
				  &gso_pi_fast, &gso_pi_slow);

  hxdpiUtil_gso_digitalize_upi(unit, gso_pi_fast, gso_pi_slow, 
			       upi_fast, upi_slow );

  hxdpiUtil_gso_digitalize_pi(upi_fast, upi_slow, pi_fast, pi_slow );

}


void hxdpiUtil_gso_correct_DNL(int unit,int pha_fast,int pha_slow,
			       double *adcdnl_pi_fast,double *adcdnl_pi_slow){
  double rnd_number;

  if (init != HXDPIUTIL_GSO_INIT){
    fprintf(stderr, "%s: gso_correct DNL: Please Init First!\n", tool_name);
    return;
  }

  rnd_number = aste_drndts();
  *adcdnl_pi_fast = dnl_data->adc_fast_start[unit][pha_fast] +
                   (dnl_data->adc_fast_width[unit][pha_fast] * rnd_number);

  rnd_number = aste_drndts();
  *adcdnl_pi_slow = dnl_data->adc_slow_start[unit][pha_slow] +
                   (dnl_data->adc_slow_width[unit][pha_slow] * rnd_number);

}

void hxdpiUtil_gso_correct_INL(int unit,
			       double adcdnl_pi_fast, double adcdnl_pi_slow,
			       double *adcinl_pi_fast,double *adcinl_pi_slow){

  int i=0;
  int j=0;
  double rx,lx,ry,ly,dy;

  if (init != HXDPIUTIL_GSO_INIT){
    fprintf(stderr, "%s: gso_correct INL : Please Init First!\n", tool_name);
    return;
  }

  while(adcdnl_pi_fast > inl_data->adc_pi_fast[unit][i]
	&& i < HXDGSOLIN_ADCINL_N_CH - 1) i++;

  rx = inl_data->adc_pi_fast[unit][i];
  lx = inl_data->adc_pi_fast[unit][i-1];
  ry = inl_data->ae_pi_fast[unit][i];
  ly = inl_data->ae_pi_fast[unit][i-1];
  dy = (ry-ly)/(rx-lx)*(adcdnl_pi_fast - rx) + ry;
  *adcinl_pi_fast = adcdnl_pi_fast - dy;

  while(adcdnl_pi_slow > inl_data->adc_pi_slow[unit][j]
	&& j < HXDGSOLIN_ADCINL_N_CH - 1) j++;
  rx = inl_data->adc_pi_slow[unit][j];
  lx = inl_data->adc_pi_slow[unit][j-1];
  ry = inl_data->ae_pi_slow[unit][j];
  ly = inl_data->ae_pi_slow[unit][j-1];
  dy = (ry-ly)/(rx-lx)*(adcdnl_pi_slow - rx) + ry;
  *adcinl_pi_slow = adcdnl_pi_slow - dy;

}

void hxdpiUtil_gso_correct_gain(int unit, int time,
				double adcdnl_pi_fast, double adcdnl_pi_slow,
				double *gain_pi_fast, double *gain_pi_slow){
  /* double gain_fast, gain_slow; */
  double ly_fast,ly_slow;     

  if (init != HXDPIUTIL_GSO_INIT){
    fprintf(stderr, "%s: gso_correct Gain : Please Init First!\n", tool_name);
    return;
  }

  ly_fast = (adcdnl_pi_fast - ghf_fast_intercept[unit]) / ghf_fast_slope[unit];
  *gain_pi_fast = ((ly_fast * (INHI_ENERGY - ly_offset_energy[unit])) / INHI_ENERGY) + ly_offset_energy[unit];
  ly_slow = (adcdnl_pi_slow - ghf_slow_intercept[unit]) / ghf_slow_slope[unit];
*gain_pi_slow = ((ly_slow * (INHI_ENERGY - ly_offset_energy[unit])) / INHI_ENERGY) + ly_offset_energy[unit];

}


void hxdpiUtil_gso_correct_nonlinear(int unit, 
				     double gain_pi_fast, double gain_pi_slow,
				     double *gso_pi_fast, double *gso_pi_slow){
  *gso_pi_fast = gain_pi_fast * (1.0-1.0/exp(gso_photo_inl_fast[0]*gain_pi_fast+gso_photo_inl_fast[1]));
  *gso_pi_slow = gain_pi_slow * (1.0-1.0/exp(gso_photo_inl_slow[0]*gain_pi_slow+gso_photo_inl_slow[1]));
   }


  
void hxdpiUtil_gso_digitalize_upi(int unit,
				  double gso_pi_fast, double gso_pi_slow,
				  double *upi_fast,    double *upi_slow){

  /* Energy to PI conversion */
  *upi_fast = ((gso_pi_fast - HXD_GSO_MIN_EN)/(HXD_GSO_MAX_EN - HXD_GSO_MIN_EN)
		   * HXD_GSO_MAX_PI_CH);
  *upi_slow = ((gso_pi_slow - HXD_GSO_MIN_EN)/(HXD_GSO_MAX_EN - HXD_GSO_MIN_EN)
		   * HXD_GSO_MAX_PI_CH);
  if(*upi_fast < 0) *upi_fast = HXDPIUTIL_PI_INVALID;
  if(*upi_slow < 0) *upi_slow = HXDPIUTIL_PI_INVALID;
  if(*upi_fast > HXD_GSO_MAX_PI_CH) *upi_fast = HXDPIUTIL_PI_INVALID;
  if(*upi_slow > HXD_GSO_MAX_PI_CH) *upi_slow = HXDPIUTIL_PI_INVALID;
  if( isnan(*upi_fast) ) *upi_fast = HXDPIUTIL_PI_INVALID;
  if( isnan(*upi_slow) ) *upi_slow = HXDPIUTIL_PI_INVALID;

}

void hxdpiUtil_gso_digitalize_pi( double *upi_fast, double *upi_slow,
				  int *pi_fast,	   int *pi_slow){
  *pi_fast = hxdpiUtil_half_adjust( upi_fast );
  *pi_slow = hxdpiUtil_half_adjust( upi_slow );
}


void hxdpiUtil_gso_calcgain( int unit, double time,
				float t_saa_hxd, double temp_well, int saapass ){

  double fit_energy[HXDGSOGHF_PMT_N_LINE];
  double slow511pha;
  double fast511pha;

  double oslow511pha;
  double ofast511pha;
  
  fit_energy[1] = INHI_ENERGY;

  if(saapass > 7) saapass = 7;

  /* double testmodf = modterm(time,unit,HXDPI_GSO_ID_FAST); */
  /* double testmods = modterm(time,unit,HXDPI_GSO_ID_SLOW); */

  fast511pha = (longterm(time,unit,0)) * ( 1. + shortterm(time,unit,0,t_saa_hxd,temp_well,saapass)) * ( 1. + modterm(time,unit,0));
  slow511pha = (longterm(time,unit,1)) * ( 1. + shortterm(time,unit,1,t_saa_hxd,temp_well,saapass)) * ( 1. + modterm(time,unit,1));

  ofast511pha = longterm(time,unit,HXDPI_GSO_ID_FAST) * ( 1. + shortterm(time,unit,0,t_saa_hxd,temp_well,saapass)) ;
  oslow511pha = longterm(time,unit,HXDPI_GSO_ID_SLOW) * ( 1. + shortterm(time,unit,1,t_saa_hxd,temp_well,saapass)) ;


/* Slow */
  ghf_slow_intercept[unit] = wpu_offset_slow[unit] - wpu_diffpedestal_slow[unit];
  ghf_slow_slope[unit] = (slow511pha - ghf_slow_intercept[unit] ) / INHI_ENERGY;

/* Fast */
 ghf_fast_intercept[unit] = wpu_offset_fast[unit] - wpu_diffpedestal_fast[unit];
 ghf_fast_slope[unit] = (fast511pha - ghf_fast_intercept[unit] ) / INHI_ENERGY;

 /* printf("calc_gain(unit,time,fast,ofast,slow,oslow,testf,tests) %d %f %f %f %f %f %f %f \n",unit,time,fast511pha,ofast511pha,slow511pha,oslow511pha,testmodf,testmods);
    printf("calc_gain(unit,time,fast,slow,saapass,t_saa_hxd,temp) %d %f %f %f %d %f %f \n",unit,time,fast511pha,slow511pha,saapass,t_saa_hxd,temp_well); */
 
}


double longterm(double time, int unit, int fastslow){
  int tmpunit=unit + HXD_WEL_N_UNIT * fastslow;  
  double timeday = (time - time20050710) / 86400.;
  double vlongterm = longterm_a[tmpunit] + longterm_b[tmpunit] * exp ( ((-1.)*timeday)/longterm_c[tmpunit]) + longterm_d[tmpunit] * exp ( ((-1.0)*timeday)/longterm_e[tmpunit]);
  /* printf("timeday %f tmpunit %d long % \n",timeday,tmpunit,vlongterm); */
  return vlongterm;
}


double shortterm (double time, int unit, int fastslow, float t_saa_hxd, double temp_well,int saapass){
  int tmpunit=unit + HXD_WEL_N_UNIT * fastslow;
  int saanum= tmpunit * HXDPI_NUM_SAA_PASS + saapass;
  double ftemp  = (temp_a[tmpunit] + temp_b[tmpunit] * temp_well)/100.;
  double fsaa = (tsaa_a[saanum] + tsaa_b[tmpunit] * exp ( (-1) * t_saa_hxd / tsaa_c[tmpunit]))/100.;
  double sum =ftemp + fsaa;
  /*printf("saanum %d tmpunit %d ftemp %f fsaa %f sum %f temp_well %f \n",saanum,tmpunit,ftemp,fsaa,sum,temp_well); */
  return sum;			      
}


double modterm(double time, int unit, int fastslow){
  int tmpunit=unit + HXD_WEL_N_UNIT * fastslow;  
  double timeday = (time - time20050710) / 86400.;
  double vmod_c = mod_c[tmpunit];
  double vmod_ga_a = mod_ga_1a[tmpunit] * exp ( ((-1.)* ( timeday - mod_ga_1b[tmpunit]) * ( timeday - mod_ga_1b[tmpunit]) / mod_ga_1c[tmpunit]));
  double vmod_ga_b = mod_ga_2a[tmpunit] * exp ( ((-1.)* ( timeday - mod_ga_2b[tmpunit]) * ( timeday - mod_ga_2b[tmpunit]) / mod_ga_2c[tmpunit]));
  double vmod_ga_c = mod_ga_3a[tmpunit] * exp ( ((-1.)* ( timeday - mod_ga_3b[tmpunit]) * ( timeday - mod_ga_3b[tmpunit]) / mod_ga_3c[tmpunit]));
  double vmodterm = ( vmod_c + vmod_ga_a + vmod_ga_b + vmod_ga_c ) / 100.0;
  /*  printf("timeday %f tmpunit %d modterm %f (c,a,b,c) = ( %f, %f, %f, %f ) \n",timeday,tmpunit,vmodterm,vmod_c,vmod_ga_a,vmod_ga_b,vmod_ga_c); */
  return vmodterm;
}

