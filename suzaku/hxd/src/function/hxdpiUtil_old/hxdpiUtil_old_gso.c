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
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "aste_rand.h"
#include "hxdpiUtil_old.h"

#define HXD_GSO_MAX_PI_CH     512
#define HXD_GSO_MAX_EN       1025
#define HXD_GSO_MIN_EN          1
#define HXDPIUTIL_PI_INVALID    0

#define INHI_ENERGY           511 
#define GD_153_ENERGY        148.00
/* Fitting parameter of 153Gd peak energy */
/* T0 = 2005-07-10 03:00:00.0, a*(T-T0)**b+c  */
#define GD_153_START_TIME    174279600.0
#define GD_153_NORM          224.668
#define GD_153_INDEX        -0.00516931
#define GD_153_CONST         -65.8768

#define HXDPIUTIL_GSO_SWITCH_TIME 195000000.0

/*  152Gd Peak Energy based upon in-orbit cal (with double peak fit)   */
static double gd_152_energy_slow[16] = {
  356.935,  354.061,  359.295,  355.644,
  356.619,  359.639,  360.513,  357.003,
  355.969,  360.813,  357.532,  360.997,
  353.972,  360.081,  359.95,   360.612
};

static double gd_152_energy_fast[16] = {
  354.89,   354.086,  357.331,  354.838,
  354.438,  357.226,  358.305,  355.5,
  353.473,  358.797,  355.755,  357.613,
  352.103,  358.023,  357.185,  357.843
};

/* Fast vs. Slow relation based upon in-orbit data */
static double fastslow_slope[16] = {
  0.964642,  0.961226,  0.969175,  0.96009,
  0.951802,  0.977671,  0.954175,  0.92116,
  0.960962,  0.966626,  0.983164,  0.928079,
  0.960525,  0.963515,  0.956244,  0.96248
};

static double fastslow_intercept[16] = {
  16.9026,  14.8264,  14.3981,  13.7513,
  22.6993,  22.7783,  21.6699,  16.0641,
  22.7139,  20.3288,  17.6359,  26.994,
  22.1067,  12.8374,  23.8211,  21.475
};

/* Gain slope vs intercept relation based upon in-orbit data */
static double slope_cept_a[16] = {
  -39.6387,  -46.3213,  -26.5571,  -51.6858,
  -120.626,  -51.9854,  -96.9556,  -31.886,
  -44.0661,  -33.0878,  -47.305,   -82.0904,
  -102.554,  -182.604,  -33.146,   -43.6245
};

static double slope_cept_b[16] = {
  103.308,  134.196,  46.3168,  147.99,
  457.613,  152.583,  338.38,   72.2977,
  117.435,  79.1222,  133.767,  264.222,
  371.881,  721.921,  79.53,    124.464
};

/*
  Artificial non-linearity of GSO (for both of fast/slow)
  Function : Ene_mod = Ene - a*exp(-Ene/b)
*/
static double gso_ene_inl_a[16] = {
  68.5218,  74.0741,  71.6650,  70.1791,
  54.0215,  62.7370,  61.0102,  73.7479,
  74.4508,  63.8047,  60.4962,  86.2019,
  62.1099,  55.2332,  65.0722,  55.9573
};

static double gso_ene_inl_b[16] = {
  32.5190,  28.2824,  36.9328,  34.7108,
  37.6698,  37.4792,  34.4425,  30.5533,
  32.7890,  34.9334,  41.5842,  29.5312,
  36.3973,  38.0169,  36.5912,  35.6999
};

/* Pedestal channel of WPU */
static int wpu_pedestal_slow[16] = {
  79, 77, 78, 79,
  73, 76, 73, 77,
  79, 77, 73, 79,
  77, 79, 76, 74
};

static int wpu_pedestal_fast[16] = {
  63, 63, 63, 69,
  61, 61, 62, 65,
  63, 63, 61, 69,
  63, 63, 63, 61
};

static char version[] = "version 2.1.4";
static char tool_name[] = "hxdpiUtil_old_old_gso";

#define HXDPIUTIL_GSO_INIT      0xff
#define HXDPIUTIL_GSO_NO_INIT   0x00

static HxdGsoLin_ADCDNL* dnl_data = NULL;
static HxdGsoLin_ADCINL* inl_data = NULL;

GSO_GHT* gdalpha_ght_data = NULL;
GSO_GHT* annihi_ght_data  = NULL;
GSO_GHT* gd153_ght_data   = NULL;
static GSO_GHT* gso_ght_data[HXDGSOGHT_PMT_N_LINE];

static double *ghf_slow_slope     = NULL;
static double *ghf_fast_slope     = NULL;
static double *ghf_slow_intercept = NULL;
static double *ghf_fast_intercept = NULL;

static int init = HXDPIUTIL_GSO_NO_INIT;
static int ght_numrow[HXDGSOGHT_PMT_N_LINE] = {0,0,0};

#define DEBUG 0

void hxdpiUtil_old_gso_init(char* hxd_gsoght_fname, char* hxd_gsolin_fname, 
			double tstart, double tstop, int* istat){
  int status;
  int i,j;

  *istat = HXDPIUTIL_OK;

  /** Memory Allocation of CALDB Data **/
  if(dnl_data == NULL){
    dnl_data = (HxdGsoLin_ADCDNL*) malloc( sizeof(HxdGsoLin_ADCDNL) );
  } else {
    fprintf(stderr, "hxdpiUtil_old: Memory, dnl_data, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if(inl_data == NULL){
    inl_data = (HxdGsoLin_ADCINL*) malloc( sizeof(HxdGsoLin_ADCINL) );
  } else {
    fprintf(stderr, "hxdpiUtil_old: Memory, inl_data, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if(gdalpha_ght_data == NULL){
     gdalpha_ght_data = (GSO_GHT*) malloc( sizeof(GSO_GHT) );
  } else {
    fprintf(stderr,"hxdpiUtil_old: Memory, gdalpha_ght_data, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if(annihi_ght_data == NULL){
    annihi_ght_data  = (GSO_GHT*) malloc( sizeof(GSO_GHT) );
  } else {
    fprintf(stderr,"hxdpiUtil_old: Memory, annihi_ght_data , is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if( gd153_ght_data == NULL){
    gd153_ght_data = (GSO_GHT*) malloc( sizeof(GSO_GHT) );
  } else {
    fprintf(stderr,"hxdpiUtil_old: Memory, gd153_ght_data, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if(ghf_slow_slope == NULL){
    ghf_slow_slope = malloc( sizeof(double)*HXD_WEL_N_UNIT );
  } else {
    fprintf(stderr,"hxdpiUtil_old: Memory, ghf_slow_slope, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if( ghf_fast_slope == NULL){
    ghf_fast_slope = malloc( sizeof(double)*HXD_WEL_N_UNIT );
  } else {
    fprintf(stderr,"hxdpiUtil_old: Memory, ghf_fast_slope, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if( ghf_slow_intercept == NULL){
    ghf_slow_intercept = malloc( sizeof(double)*HXD_WEL_N_UNIT );
  } else {
    fprintf(stderr,"hxdpiUtil_old: Memory, ghf_slow_intercept, is already allocated\n");
    *istat = HXDPIUTIL_NG;
    return;
  }

  if( ghf_fast_intercept == NULL){
    ghf_fast_intercept = malloc( sizeof(double)*HXD_WEL_N_UNIT );
  } else {
    fprintf(stderr,"hxdpiUtil_old: Memory, ghf_fast_intercept, is already allocated\n");
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
  status = hxdcaldbUtil_gsoght_open_FITS(hxd_gsoght_fname);
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDGSO GHT FITS file open error\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }

  status = 
    hxdcaldbUtil_gsoght_read_FITS(tstart, tstop, gdalpha_ght_data, 
			       annihi_ght_data,  gd153_ght_data, ght_numrow );
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDGSO GHT FITS file read error\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }
  /** assign lines **/
  gso_ght_data[0] = gdalpha_ght_data;
  gso_ght_data[1] = annihi_ght_data;
  gso_ght_data[2] = gd153_ght_data;

  if (DEBUG){
    printf("ght_numrow[0] = %d, ght_numrow[1] = %d, ght_numrow[2] = %d\n",
	   ght_numrow[0], ght_numrow[1], ght_numrow[2] );
  }

  status = hxdcaldbUtil_gsoght_close_FITS();
  if (status != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s:HXDGSO GHT FITS file close error\n",tool_name);
    *istat = HXDPIUTIL_NG;
    return;
  }

  init = HXDPIUTIL_GSO_INIT;
  printf("Opening GSO calibration files OK\n");
  fflush(stdout);

  return;
}



void hxdpiUtil_old_gso_correct(HxdEventFits02 *eventdata, hxdpiUtil_old_HK* hkdata,
			   hxdpiUtil_old_EHK* ehkdata,
			   double* upi_fast,  double* upi_slow,
			   int *pi_fast,  int *pi_slow){
  int pha_fast, pha_slow;
  int unit;
  double time;
  float t_saa_hxd;
  double temp_well[16];

  double adcdnl_pi_fast, adcinl_pi_fast, gain_pi_fast, gso_pi_fast; 
  double adcdnl_pi_slow, adcinl_pi_slow, gain_pi_slow, gso_pi_slow;
  hxdpiUtil_old_ghfdata ghfdata;

  int i;

  if (init != HXDPIUTIL_GSO_INIT){
    fprintf(stderr, "%s: gso_correct: Please Init First!\n", tool_name);
    return;
  }

  pha_fast = eventdata->pha_fast;
  pha_slow = eventdata->pha_slow;
  time = eventdata->time;
  unit = eventdata->unitid;

  t_saa_hxd = ehkdata->t_saa_hxd;
  /* Current method use the same temperature for all unit */
  for ( i=0; i<16; i++){
    temp_well[i] = hkdata->hxd_temp_body3_cal;
  }

  hxdpiUtil_old_gso_ght_calcghf( unit, time, t_saa_hxd, temp_well, &ghfdata );

  hxdpiUtil_old_gso_ghf_fit( unit, time, &ghfdata );

  hxdpiUtil_old_gso_correct_DNL(unit,pha_fast, pha_slow,
			    &adcdnl_pi_fast, &adcdnl_pi_slow);

  hxdpiUtil_old_gso_correct_INL(unit,adcdnl_pi_fast, adcdnl_pi_slow,
			    &adcinl_pi_fast, &adcinl_pi_slow);

  hxdpiUtil_old_gso_correct_gain(unit, time,
			     adcinl_pi_fast, adcinl_pi_slow,
			     &gain_pi_fast, &gain_pi_slow);

  hxdpiUtil_old_gso_correct_nonlinear(unit, gain_pi_fast, gain_pi_slow,
				  &gso_pi_fast, &gso_pi_slow);

  hxdpiUtil_old_gso_digitalize_upi(unit, gso_pi_fast, gso_pi_slow, 
			       upi_fast, upi_slow );

  hxdpiUtil_old_gso_digitalize_pi(upi_fast, upi_slow, pi_fast, pi_slow );

}


void hxdpiUtil_old_gso_correct_DNL(int unit,int pha_fast,int pha_slow,
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

void hxdpiUtil_old_gso_correct_INL(int unit,
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

void hxdpiUtil_old_gso_correct_gain(int unit, int time,
				double adcdnl_pi_fast, double adcdnl_pi_slow,
				double *gain_pi_fast, double *gain_pi_slow){
  double gain_fast, gain_slow;

  if (init != HXDPIUTIL_GSO_INIT){
    fprintf(stderr, "%s: gso_correct Gain : Please Init First!\n", tool_name);
    return;
  }

  *gain_pi_fast = 
    (adcdnl_pi_fast - ghf_fast_intercept[unit]) / ghf_fast_slope[unit];
  *gain_pi_slow = 
    (adcdnl_pi_slow - ghf_slow_intercept[unit]) / ghf_slow_slope[unit];

}


void hxdpiUtil_old_gso_correct_nonlinear(int unit, 
				     double gain_pi_fast, double gain_pi_slow,
				     double *gso_pi_fast, double *gso_pi_slow){
  double gain_pedestal_fast, gain_pedestal_slow;
  double gso_ene_inl_par ;

  /*
   * Use gso_ene_inl_par instead of gso_ene_inl_a[unit]
   * to take the time variation of gain into account.
   * (always should be zero-energy at pedestal)
   */
  gain_pedestal_slow =
    (wpu_pedestal_slow[unit]-ghf_slow_intercept[unit]) / ghf_slow_slope[unit];
  gso_ene_inl_par =
    gain_pedestal_slow / exp( -1.0*gain_pedestal_slow/gso_ene_inl_b[unit] );

  *gso_pi_fast = gain_pi_fast
    - gso_ene_inl_par * exp( -1.0 * gain_pi_fast / gso_ene_inl_b[unit] );
  *gso_pi_slow = gain_pi_slow
    - gso_ene_inl_par * exp( -1.0 * gain_pi_slow / gso_ene_inl_b[unit] );

}

  
void hxdpiUtil_old_gso_digitalize_upi(int unit,
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

void hxdpiUtil_old_gso_digitalize_pi( double *upi_fast, double *upi_slow,
				  int *pi_fast,	   int *pi_slow){
  *pi_fast = hxdpiUtil_old_half_adjust( upi_fast );
  *pi_slow = hxdpiUtil_old_half_adjust( upi_slow );
}


void hxdpiUtil_old_gso_ght_calcghf( int unit, double time,
				float t_saa_hxd, double *temp_well,
				hxdpiUtil_old_ghfdata *ghfdata ){

  static int gso_correct_first = 1;
  static int ght_irow[HXDGSOGHT_PMT_N_LINE] = {0,0,0};
  static double ght_tstart_now[HXDGSOGHT_PMT_N_LINE]  = {0.0,0.0,0.0};
  static double ght_tstart_next[HXDGSOGHT_PMT_N_LINE] = {0.0,0.0,0.0};

  double inter_factor[HXDGSOGHT_PMT_N_LINE];
  double peak_slow, peak_fast, err_slow, err_fast;
  int i;
  double slow_a, slow_b, slow_c, slow_long;
  double fast_a, fast_b, fast_c, fast_long;
  double slow_d_now, slow_d_next, fast_d_now, fast_d_next;

  /* Row judgement */ 
  if ( gso_correct_first ){
    for ( i=0; i<HXDGSOGHT_PMT_N_LINE; i++ ){
      ght_tstart_now[i]  = gso_ght_data[0]->data[i].start_time;
      ght_tstart_next[i] = gso_ght_data[0]->data[i].start_time;
    }
    gso_correct_first = 0;
  }
  for ( i=0; i<HXDGSOGHT_PMT_N_LINE; i++ ){
    while ( (time > ght_tstart_now[i])  && (ght_irow[i] < ght_numrow[i]-1 ) ){
      if ( time <= ght_tstart_next[i] ) {
	break;
      }
      ght_irow[i]++;
      if ( ght_irow[i] < ght_numrow[i]-1 ){
	ght_tstart_now[i]  = gso_ght_data[i]->data[ght_irow[i]].start_time;
	ght_tstart_next[i] = gso_ght_data[i]->data[ght_irow[i]+1].start_time;
      }else{ /* ght_irow == ght_numwor-1 */
	ght_tstart_now[i]  = gso_ght_data[i]->data[ght_irow[i]].start_time;
	ght_tstart_next[i] = gso_ght_data[i]->data[ght_irow[i]].start_time;
      }
    }
  }

  /* peak channel calculation */
  for ( i=0; i<HXDGSOGHF_PMT_N_LINE; i++){

    /* Interporation factor */
    if ( ght_tstart_now[i] < ght_tstart_next[i] ){
      inter_factor[i] = 
	(time - ght_tstart_now[i])/(ght_tstart_next[i] - ght_tstart_now[i]);
    }else{
      inter_factor[i] = 0.0;
    }

    if (DEBUG){
      printf("hxdpiUtil_old: irow[%d]=%d, Ts_now[%d]=%12.2lf, Ts_next[%d]=%12.2lf\n", i, ght_irow[i], i, ght_tstart_now[i], i, ght_tstart_next[i], i);
      printf("hxdpiUtil_old: Time=%12.2lf, If[%d]=%6.4lf\n", time, i, inter_factor[i] );
      fflush(stdout);
    }

    slow_a = gso_ght_data[i]->data[ght_irow[i]].slow_param_a[unit];
    slow_b = gso_ght_data[i]->data[ght_irow[i]].slow_param_b[unit];
    slow_c = gso_ght_data[i]->data[ght_irow[i]].slow_param_c[unit];
    slow_d_now  = gso_ght_data[i]->data[ght_irow[i]].slow_longterm[unit];
    slow_d_next = gso_ght_data[i]->data[ght_irow[i]+1].slow_longterm[unit];
    slow_long = (slow_d_next - slow_d_now)*inter_factor[i] + slow_d_now;

    fast_a = gso_ght_data[i]->data[ght_irow[i]].fast_param_a[unit];
    fast_b = gso_ght_data[i]->data[ght_irow[i]].fast_param_b[unit];
    fast_c = gso_ght_data[i]->data[ght_irow[i]].fast_param_c[unit];
    fast_d_now  = gso_ght_data[i]->data[ght_irow[i]].fast_longterm[unit];
    fast_d_next = gso_ght_data[i]->data[ght_irow[i]+1].fast_longterm[unit];
    fast_long = (fast_d_next - fast_d_now)*inter_factor[i] + fast_d_now;
    
    if (DEBUG){
      printf("hxdpiUtil_old: Sa=%7.2e, Sb=%7.2e, Sc=%7.2lf, Sl=%7.2f\n",
	     slow_a, slow_b, slow_c, slow_long);
      printf("hxdpiUtil_old: Sd_now=%7.2f, Sd_next=%7.2f, temp[%d]=%7.2lf, t_saa=%7.2f\n", slow_d_now, slow_d_next, unit, temp_well[unit], t_saa_hxd);
      printf("hxdpiUtil_old: Fa=%7.2e, Fb=%7.2e, Fc=%7.2lf, Fl=%7.2f\n",
	     fast_a, fast_b, fast_c, fast_long);
      printf("hxdpiUtil_old: Fd_now=%7.2f, Fd_next=%7.2f, temp[%d]=%7.2lf, t_saa=%7.2f\n", fast_d_now, fast_d_next, unit, temp_well[unit], t_saa_hxd);
      fflush(stdout);
    }

    if ( time < HXDPIUTIL_GSO_SWITCH_TIME ){ /* ghistdump.cc */
      peak_slow = 
	slow_long * (1+slow_a*(temp_well[unit]+14.0)) * 
	(1+slow_b*exp(-1.0*t_saa_hxd/slow_c));
      peak_fast = 
	fast_long * (1+fast_a*(temp_well[unit]+14.0)) * 
	(1+fast_b*exp(-1.0*t_saa_hxd/fast_c));
    }else{ /* ghistdump2.cc */
      peak_slow = 
	slow_long * (1+slow_a*(temp_well[unit]+14.0)) * 
	(1+slow_b*exp(-1.0*t_saa_hxd/slow_c))/(1+slow_b);
      peak_fast = 
	fast_long * (1+fast_a*(temp_well[unit]+14.0)) * 
	(1+fast_b*exp(-1.0*t_saa_hxd/fast_c))/(1+fast_b);
    }
    err_slow = peak_slow * 0.03;
    err_fast = peak_fast * 0.03;

    if (DEBUG){
      printf("hxdpiUtil_old:[%2d] P_s=%7.2lf, Pe_s=%7.2lf, P_f=%7.2lf, Pe_ft=%7.2lf\n", 
	     unit, peak_slow, err_slow, peak_fast, err_fast);
      fflush(stdout);
    }

    ghfdata->peak_slow[unit][i] = peak_slow;
    ghfdata->peak_slow_err[unit][i] = err_slow;
    ghfdata->peak_fast[unit][i] = peak_fast;
    ghfdata->peak_fast_err[unit][i] = err_fast;
  }
}


void hxdpiUtil_old_gso_ghf_fit(int unit, double time, hxdpiUtil_old_ghfdata* ghfdata ){
  int j,k,row;
  double weight ;
  double weight_x_sum ;
  double weight_y_sum ;
  double weight_x2_sum ;
  double weight_xy_sum ;
  double weight_sum ;
  double delta ;
  double err,peak;
  double fit_energy[HXDGSOGHF_PMT_N_LINE];

  fit_energy[1] = INHI_ENERGY;
  fit_energy[2] = GD_153_CONST +
    GD_153_NORM * pow( (time - GD_153_START_TIME)/86400, GD_153_INDEX );
  /*
  if(DEBUG) printf("hxdpiUtil_old : 153Gd peak energy = %f\n", fit_energy[2] );
  */

  /* Fitting with the least squre method  */
  weight_x_sum  = 0;
  weight_y_sum  = 0;
  weight_x2_sum = 0;
  weight_xy_sum = 0;
  weight_sum    = 0;

  /* Slow */
  fit_energy[0] = gd_152_energy_slow[unit];
  for(k=0; k<HXDGSOGHF_PMT_N_LINE; k++){
    peak = ghfdata->peak_slow[unit][k];
    err  = ghfdata->peak_slow_err[unit][k];
    weight = pow(err, -2);
    weight_x_sum   += weight * fit_energy[k];
    weight_y_sum   += weight * peak;
    weight_x2_sum  += weight * pow(fit_energy[k], 2);
    weight_xy_sum  += weight * fit_energy[k] * peak;
    weight_sum     += weight ;
  }

  /* Fast */
  fit_energy[0] = gd_152_energy_fast[unit];
  for(k=0; k<HXDGSOGHF_PMT_N_LINE; k++){
    peak = ghfdata->peak_fast[unit][k] * fastslow_slope[unit] + fastslow_intercept[unit];
    err  = ghfdata->peak_fast_err[unit][k] * fastslow_slope[unit];
    weight = pow(err, -2);
    weight_x_sum   += weight * fit_energy[k];
    weight_y_sum   += weight * peak;
    weight_x2_sum  += weight * pow(fit_energy[k], 2);
    weight_xy_sum  += weight * fit_energy[k] * peak;
    weight_sum     += weight ;
  }

  /* result of three-points fitting */
  delta = weight_sum * weight_x2_sum - pow(weight_x_sum, 2);

  /* Slow */
  ghf_slow_slope[unit] =
    (weight_sum * weight_xy_sum - weight_x_sum * weight_y_sum) / delta;
  ghf_slow_intercept[unit] =
    (weight_x2_sum * weight_y_sum - weight_x_sum * weight_xy_sum) / delta;
  /* Fast */
  ghf_fast_intercept[unit] =
    (ghf_slow_intercept[unit]-fastslow_intercept[unit])/fastslow_slope[unit];
  ghf_fast_slope[unit] =  ghf_slow_slope[unit] / fastslow_slope[unit];

  if (DEBUG){
    printf("hxdpiUtil_old:[%2d] Sl_s=%7.2lf Is_s=%7.2lf Sl_f=%7.2lf Is_f=%7.2lf\n", 
	   unit, ghf_slow_slope[unit], ghf_slow_intercept[unit], 
	   ghf_fast_slope[unit], ghf_fast_intercept[unit]);
    fflush(stdout);
  }

}
