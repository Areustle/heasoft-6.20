/*
 *   hxdcaldbUtil_gsogainhist.c
 *         2005-02-02, created by T.Kishishita
 *         2005-02-05, debug   by Y.Terada
 *         2005-02-07, debug,  by Y.Terada
 *         v0.2.4; 2005-05-17,
 *                 change INSTRUME and DETNAM by Y.Terada
 *                 INSTRUME is fixed to be a value of 'HXD'
 *                 DETNAM is, 'WELL_GSO' 'WELL_PIN' 'WAM_ANTI'
 *         v0.3.0; 2005-05-25  by Y.Terada
 *                 change structure of Gain History, GSO_GHF.
 *                 change format of gain history FITS.
 *         v0.3.1; 2005-0531   by S.hirakuri
 *                 debug
 *         v0.3.5; 2005-06-09  by Y.Terada
 *                 add exposure information in ASCII I/O
 *                 add QUALITY column in gain gistory FITS.
 *         v 0.4.0; 2005-06-13 Y.Terada
 *                 GSFC comment
 *         v 0.4.1; 2005-06-25 Y.Terada,  add Primary Header 
 *         v 0.4.3; 2005-09-28 Y.Terada,  add FILENAME
 *         v 0.4.4; 2005-10-19..22 Y.Terada,  New FITS Format
 *         v 0.4.5; 2005-10-24 Y.Terada,  backward compatibility
 *         v 0.4.6; 2005-11-11 Y.Terada,  read quality
 *         v 0.4.8; 2005-11-26 Y.Terada, H.Takahashi add CBD10001
 *         v 0.4.9; 2005-11-29 Y.Terada, add comments on CBD10001
 *         v 0.5.0; 2006-01-31 Y.Terada, read GHF debug
 *         v 0.5.2; 2006-05-26 M.K add TSTOP in gsogainhist_read_FITS()
 *         v 0.5.3; 2006-06-26 M.K "ghf_start_time >= tstart"
 *         v 0.5.4; 2006-07-29 M.K changed the case of 'nrow=1'
 *         v 0.6.2; 2006-08-31 Y.Terada
 *                 format changed (as version 1.2.2.3 caldb)
 *         v 0.6.5; 2006-10-25 M.K int -> double in time check of ASCII GHF
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "hxdcaldbUtil.h"
#include "hxdFitsCommentUtil.h"
#include "atFunctions.h"
#include "aste_time.h"

static FILE*     hxd_gsogainhist_ascii_fp;
static fitsfile* hxd_gsogainhist_fits_fp;
static char tool_name[] = "hxdcaldbUtil_gsoghf";
/*static int hxd_gsogainhist_irow[HXDGSOGHF_PMT_N_LINE];*/
/* static int N_row; */
static void reformat_GHTBL_to_FitsParam(int unit, GSO_GH_TBL *tbl, 
					double *fitsparam);
static void reformat_FitsParam_to_GHTBL(int unit, double *fitsparam, 
					GSO_GH_TBL *tbl);
#define DEBUG 0

/* ========================================
 *   ASCII File I/O
 * ========================================*/
int hxdcaldbUtil_gsogainhist_open_ASCII (char* gsogainhist_ascii_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  hxd_gsogainhist_ascii_fp = fopen(gsogainhist_ascii_fname, "r");
  if (hxd_gsogainhist_ascii_fp == NULL){
    fprintf(stderr, 
            "%s: Cannot open ASCII file(%s)\n",
            tool_name, gsogainhist_ascii_fname);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

int 
hxdcaldbUtil_gsogainhist_read_ASCII (GSO_GHF* gso_slow_ghf_data,
				     GSO_GHF* gso_fast_ghf_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int N_row;

  int the_unit_id;
  int start_yyyymmdd;
  int start_hhmmss;
  double start_time;
  double end_time;
  int unit_id;
  double exposure;
  int fit_model_id;
  /** slow **/
  double slow_fit_start_ch;
  double slow_fit_end_ch;
  double slow_fit_gauss_peak;
  double slow_fit_gauss_peak_err;
  double slow_fit_gauss_norm;
  double slow_fit_gauss_norm_err;
  double slow_fit_gauss_sigma;
  double slow_fit_gauss_sigma_err;
  double slow_fit_pow_norm;
  double slow_fit_pow_norm_err;
  double slow_fit_pow_index;
  double slow_fit_pow_index_err;
  double slow_area_flux;
  /** fast **/
  double fast_fit_start_ch;
  double fast_fit_end_ch;
  double fast_fit_gauss_peak;
  double fast_fit_gauss_peak_err;
  double fast_fit_gauss_norm;
  double fast_fit_gauss_norm_err;
  double fast_fit_gauss_sigma;
  double fast_fit_gauss_sigma_err;
  double fast_fit_pow_norm;
  double fast_fit_pow_norm_err;
  double fast_fit_pow_index;
  double fast_fit_pow_index_err;
  double fast_area_flux;

  double quality;
  double redu_chi_sq;
  AtTimeD attime;

  double prev_start_time = 0.0;
  double prev_end_time = 0.0;
  int    prev_unit_id  = 0xFFFF;

  if (hxd_gsogainhist_ascii_fp == NULL){
    fprintf(stderr, "hxdcaldbUtil: Invalid file pointer(hxd_gsogainhist)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  N_row = -1;
  while(!feof(hxd_gsogainhist_ascii_fp))   {
    fscanf(hxd_gsogainhist_ascii_fp, 
	   "%lf %lf %lf %d %d %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf\n",
	   &start_time, &end_time, &exposure, &unit_id, &fit_model_id, 
	   &slow_fit_start_ch,    &slow_fit_end_ch, 
	   &slow_fit_gauss_peak,  &slow_fit_gauss_peak_err,
	   &slow_fit_gauss_norm,  &slow_fit_gauss_norm_err, 
	   &slow_fit_gauss_sigma, &slow_fit_gauss_sigma_err, 
	   &slow_fit_pow_norm,    &slow_fit_pow_norm_err, 
	   &slow_fit_pow_index,   &slow_fit_pow_index_err, 
	   &slow_area_flux,       
	   &fast_fit_start_ch,    &fast_fit_end_ch, 
	   &fast_fit_gauss_peak,  &fast_fit_gauss_peak_err,
	   &fast_fit_gauss_norm,  &fast_fit_gauss_norm_err, 
	   &fast_fit_gauss_sigma, &fast_fit_gauss_sigma_err, 
	   &fast_fit_pow_norm,    &fast_fit_pow_norm_err, 
	   &fast_fit_pow_index,   &fast_fit_pow_index_err, 
	   &fast_area_flux,       &redu_chi_sq, &quality);

    /*** (1) check the format ***/
    if ( (start_time) != (prev_start_time) ) {
      /** (1-1) end of the time zone **/
      if (prev_unit_id != 0xFFFF){
	fprintf(stderr, "%s:No.%d: Not All Unit ID defined. 0x%04X\n",
		tool_name, N_row, prev_unit_id);
      }
      prev_unit_id = 0;
      prev_start_time = start_time;
      prev_end_time   = end_time;
      N_row ++;
    } else {
      /** (1-2) same time zone: check **/
      if ( (end_time) != (prev_end_time) ){
	fprintf(stderr, 
		"%s:No.%d: same start time (%f), diffrent end time (%f->%f)\n",
		tool_name, N_row, start_time, prev_end_time, end_time);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      if ( prev_unit_id & (0x0001<<unit_id) ){
	fprintf(stderr, "%s:No.%d: same unit_id=%d\n",
		tool_name, N_row, unit_id);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
    }
    prev_unit_id |= (0x0001<<unit_id);

    /** (1-3) check **/
    if (unit_id > HXD_WEL_N_UNIT){
	fprintf(stderr, "%s:No.%d: undefined unit_id=%d\n",
		tool_name, N_row, unit_id);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
    }

    /*** (2) write the data into the structure ***/
    aste2attimeD(start_time,&attime);
    start_yyyymmdd = attime.yr*10000 + attime.mo*100 + attime.dy;
    start_hhmmss = attime.hr*10000 + attime.mn*100 + attime.sc;
    
    gso_slow_ghf_data->data[N_row].start_time = start_time;
    gso_slow_ghf_data->data[N_row].start_yyyymmdd = start_yyyymmdd;
    gso_slow_ghf_data->data[N_row].start_hhmmss = start_hhmmss;
    gso_slow_ghf_data->data[N_row].start_time = start_time;
    gso_slow_ghf_data->data[N_row].end_time = end_time;
/*  gso_slow_ghf_data->data[N_row].exposure = end_time - start_time;*/
    gso_slow_ghf_data->data[N_row].exposure = exposure;
    gso_slow_ghf_data->data[N_row].fit_model_id = fit_model_id;
    gso_slow_ghf_data->data[N_row].fit_start_ch[unit_id] 
      = slow_fit_start_ch;
    gso_slow_ghf_data->data[N_row].fit_end_ch[unit_id] 
      = slow_fit_end_ch;
    gso_slow_ghf_data->data[N_row].fit_gauss_peak[unit_id] 
      = slow_fit_gauss_peak;
    gso_slow_ghf_data->data[N_row].fit_gauss_peak_err[unit_id] 
      = slow_fit_gauss_peak_err;
    gso_slow_ghf_data->data[N_row].fit_gauss_norm[unit_id] 
      = slow_fit_gauss_norm;
    gso_slow_ghf_data->data[N_row].fit_gauss_norm_err[unit_id] 
      = slow_fit_gauss_norm_err;
    gso_slow_ghf_data->data[N_row].fit_gauss_sigma[unit_id] 
      = slow_fit_gauss_sigma;
    gso_slow_ghf_data->data[N_row].fit_gauss_sigma_err[unit_id] 
      = slow_fit_gauss_sigma_err;
    gso_slow_ghf_data->data[N_row].fit_pow_norm[unit_id] 
      = slow_fit_pow_norm;
    gso_slow_ghf_data->data[N_row].fit_pow_norm_err[unit_id] 
      = slow_fit_pow_norm_err;
    gso_slow_ghf_data->data[N_row].fit_pow_index[unit_id] 
      = slow_fit_pow_index;
    gso_slow_ghf_data->data[N_row].fit_pow_index_err[unit_id] 
      = slow_fit_pow_index_err;
    gso_slow_ghf_data->data[N_row].area_flux[unit_id] 
      = slow_area_flux;
    gso_slow_ghf_data->data[N_row].redu_chi_sq[unit_id] 
      = redu_chi_sq;
    gso_slow_ghf_data->data[N_row].quality[unit_id] = quality;

    gso_fast_ghf_data->data[N_row].start_time = start_time;
    gso_fast_ghf_data->data[N_row].start_yyyymmdd = start_yyyymmdd;
    gso_fast_ghf_data->data[N_row].start_hhmmss = start_hhmmss;
    gso_fast_ghf_data->data[N_row].start_time = start_time;
    gso_fast_ghf_data->data[N_row].end_time = end_time;
/*  gso_fast_ghf_data->data[N_row].exposure = end_time - start_time;*/
    gso_fast_ghf_data->data[N_row].exposure = exposure;
    gso_fast_ghf_data->data[N_row].fit_model_id = fit_model_id;
    gso_fast_ghf_data->data[N_row].fit_start_ch[unit_id] 
      = fast_fit_start_ch;
    gso_fast_ghf_data->data[N_row].fit_end_ch[unit_id] 
      = fast_fit_end_ch;
    gso_fast_ghf_data->data[N_row].fit_gauss_peak[unit_id] 
      = fast_fit_gauss_peak;
    gso_fast_ghf_data->data[N_row].fit_gauss_peak_err[unit_id] 
      = fast_fit_gauss_peak_err;
    gso_fast_ghf_data->data[N_row].fit_gauss_norm[unit_id] 
      = fast_fit_gauss_norm;
    gso_fast_ghf_data->data[N_row].fit_gauss_norm_err[unit_id] 
      = fast_fit_gauss_norm_err;
    gso_fast_ghf_data->data[N_row].fit_gauss_sigma[unit_id] 
      = fast_fit_gauss_sigma;
    gso_fast_ghf_data->data[N_row].fit_gauss_sigma_err[unit_id] 
      = fast_fit_gauss_sigma_err;
    gso_fast_ghf_data->data[N_row].fit_pow_norm[unit_id] 
      = fast_fit_pow_norm;
    gso_fast_ghf_data->data[N_row].fit_pow_norm_err[unit_id] 
      = fast_fit_pow_norm_err;
    gso_fast_ghf_data->data[N_row].fit_pow_index[unit_id] 
      = fast_fit_pow_index;
    gso_fast_ghf_data->data[N_row].fit_pow_index_err[unit_id] 
      = fast_fit_pow_index_err;
    gso_fast_ghf_data->data[N_row].area_flux[unit_id] 
      = fast_area_flux;
    gso_fast_ghf_data->data[N_row].redu_chi_sq[unit_id] 
      = redu_chi_sq;
    gso_fast_ghf_data->data[N_row].quality[unit_id] = quality;
  }

  gso_slow_ghf_data->nrow = N_row+1;
  gso_fast_ghf_data->nrow = N_row+1;

  return status;
}

int 
hxdcaldbUtil_gsogainhist_close_ASCII(void){
  int status = HXDCALDBUTIL_STATUS_OK;
  
  if (fclose(hxd_gsogainhist_ascii_fp) ){
    fprintf(stderr, 
            "%s: Cannot close ASCII file\n",
	    tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

/* ========================================
 *   Fits File  Create
 * ========================================*/
int 
hxdcaldbUtil_gsogainhist_create_FITS(char* gsogainhist_fits_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
  int bitpix = 8;
  long naxis2 = 1;
  long nrow=0;
  char detname[16] = "WELL_GSO";
  char code_name[16] = "GAIN_HISTORY";
  char *extension_name[HXDGSOGHF_PMT_N_LINE]={
    "GSO_152GD_350KEV",
    "GSO_ANNIHILATION_511KEV",
    "GSO_153GD_150KEV"
  };
  long naxes[1];
  int unit_id;
  
#define HXDPMT_GSOGAINHIST_NKEYWORD 38
  
  static char *ttype[HXDPMT_GSOGAINHIST_NKEYWORD] = {
    "START_TIME", "YYYYMMDD", 
    "HHMMSS", "END_TIME", 
    "EXPOSURE",     "FIT_MODEL_ID", 
    /** Fit Results **/
    "W00_SLOW", "W00_FAST",
    "W01_SLOW", "W01_FAST",
    "W02_SLOW", "W02_FAST",
    "W03_SLOW", "W03_FAST",
    "W10_SLOW", "W10_FAST",
    "W11_SLOW", "W11_FAST",
    "W12_SLOW", "W12_FAST",
    "W13_SLOW", "W13_FAST",
    "W20_SLOW", "W20_FAST",
    "W21_SLOW", "W21_FAST",
    "W22_SLOW", "W22_FAST",
    "W23_SLOW", "W23_FAST",
    "W30_SLOW", "W30_FAST",
    "W31_SLOW", "W31_FAST",
    "W32_SLOW", "W32_FAST",
    "W33_SLOW", "W33_FAST"
  };
  static char *tform[HXDPMT_GSOGAINHIST_NKEYWORD] = {
    "1D", "1J", 
    "1J", "1D", 
    "1D", "1I", 
    /** Fit Results **/
    "15D", "15D", "15D", "15D", 
    "15D", "15D", "15D", "15D", 
    "15D", "15D", "15D", "15D", 
    "15D", "15D", "15D", "15D",
    "15D", "15D", "15D", "15D", 
    "15D", "15D", "15D", "15D", 
    "15D", "15D", "15D", "15D", 
    "15D", "15D", "15D", "15D"
  };
  /**HXDGSOGHF_N_FITPARAM is 15 **/

  static char *tunit[HXDPMT_GSOGAINHIST_NKEYWORD] = {
    "s", "",
    "",  "s", 
    "s", "",
    /** Fit Results **/
    "","","","",
    "","","","",
    "","","","",
    "","","","",
    "","","","",
    "","","","",
    "","","","",
    "","","",""
  };
  char *keyword[HXDPMT_GSOGAINHIST_NKEYWORD]={
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  ", 
    "TTYPE6  ", "TTYPE7  ", "TTYPE8  ","TTYPE9  ", "TTYPE10  ", 
    "TTYPE11  ", "TTYPE12  ", "TTYPE13  ", "TTYPE14  ", "TTYPE15  ", 
    "TTYPE16  ", "TTYPE17  ", "TTYPE18  ", "TTYPE19  ", "TTYPE20  ",
    "TTYPE21  ", "TTYPE22  ", "TTYPE23  ", "TTYPE24  ", "TTYPE25  ", 
    "TTYPE26  ", "TTYPE27  ", "TTYPE28  ", "TTYPE29  ", "TTYPE30  ",
    "TTYPE31  ", "TTYPE32  ", "TTYPE33  ", "TTYPE34  ", "TTYPE35  ", 
    "TTYPE36  ", "TTYPE37  ", "TTYPE38  "
  };
  char *comment[HXDPMT_GSOGAINHIST_NKEYWORD]={
    "GTO start time", "DATE", 
    "TIME", "GTI end time", 
    "exposure time", "FIT MODEL", 
    /** Fit Results **/
    "Fit Results for W00 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W00 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W01 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W01 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W02 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W02 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W03 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W03 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W10 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W10 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W11 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W11 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W12 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W12 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W13 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W13 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W20 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W20 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W21 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W21 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W22 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W22 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W23 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W23 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W30 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W30 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W31 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W31 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W32 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W32 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W33 PI_SLOW           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  .",
    "Fit Results for W33 PI_FAST           (TUNIT)                         01:Start channel in fitting           (chan )                         02:End channel in fitting             (chan )                         03:Peak Channel by Gaussian fitting   (chan )                         04:Error of Peak Channel, Gaussian    (chan )                         05:Normalization by Gaussian fitting  (count)                         06:Error of Normalization, Gaussian   (count)                         07:Sigma obtained by Gaussian fitting (chan )                         08:Error of Sigma, Gaussian fitting   (chan )                         09:Index of POWLAW component                                          10:Error of Index, POWLAW component                                   11:Normalization of POWLAW component  (chan )                         12:Error of Normalization, POWLAW     (count)                         13:Reduced chi square in simultaneous                                    SLOW/FAST fitting                                                  14:Area Flux                  (count/cm**2/s)                         15:Quality flag           (0: good / 1: bad )                         Number is from MSB. b1000000000000000 = 01  ."
  };
  int ncol = sizeof(ttype)/sizeof(*ttype);
  char *description[HXDGSOGHF_PMT_N_LINE] ={
    "Fit Results of the intrinsic Gd line on 348 keV.",
    "Fit Results of the Annihilation line on 511 keV.",
    "Fit Results of the line from 153Gd on 152 keV."
  };
  int  use_boundary = 1;
  char *ext_meanning[HXDGSOGHF_PMT_N_LINE] = {
    "ENERG(348,348)",
    "ENERG(511,511)",
    "ENERG(148,148)"
  };
  int hdu_num;


  /**** create Fits file *****/
  if (fits_create_file(&hxd_gsogainhist_fits_fp, gsogainhist_fits_fname,
		       &istat)) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n", tool_name, 
	    gsogainhist_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } 

  if( fits_write_grphdr(hxd_gsogainhist_fits_fp,
                        1, 8, 0, naxes, 0, 1, 1, &istat) ){
    fprintf(stderr, "%s:fits_write_grphdr failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  for (hdu_num=0; hdu_num < HXDGSOGHF_PMT_N_LINE; hdu_num++){
    if( fits_create_hdu(hxd_gsogainhist_fits_fp, &istat) ){
      fprintf(stderr, "%s:fits_create_hdu failed (%d)\n", tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    if ( fits_write_btblhdr(hxd_gsogainhist_fits_fp, naxis2, ncol, 
			    ttype, tform, tunit, 
			    extension_name[hdu_num], 0, &istat) ) {
      fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    if (hxdcaldbUtil_write_fits_header(hxd_gsogainhist_fits_fp, 
				       detname, code_name, 
				       description[hdu_num],
				       gsogainhist_fits_fname,
				       use_boundary,ext_meanning[hdu_num])
	!= HXDCALDBUTIL_STATUS_OK){
      fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    
    hxdFitsComment_write(hxd_gsogainhist_fits_fp,HXDPMT_GSOGAINHIST_NKEYWORD,
			 keyword, comment, &istat);
    if (istat){
      fprintf(stderr, "%s: fits write comment failed.\n", tool_name);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    /* hxd_gsogainhist_irow[hdu_num] = 0; */
  }

  if(fits_flush_file(hxd_gsogainhist_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int 
hxdcaldbUtil_gsogainhist_write_FITS(int n_line, 
				    GSO_GHF* gdalpha_slow_ghf_data,
				    GSO_GHF* gdalpha_fast_ghf_data,
				    GSO_GHF* annihi_slow_ghf_data,
				    GSO_GHF* annihi_fast_ghf_data,
				    GSO_GHF* gd153_slow_ghf_data,
				    GSO_GHF* gd153_fast_ghf_data){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  int colnum;
  int row, irow;
  long naxis2;
  long firstelem = 1;
  long nelements = 1;
  unsigned char uchar_unit;
  int hdutype;
  
  double double_val;
  int    int_val;
  unsigned char uchar_val;
  
  char detname[16] = "WELL_GSO"; /** pri header **/

  enum{
    GSO_GHF_DUMMY_C_FORTRUN,
      GSO_GHF_START_TIME, GSO_GHF_YYYYMMDD, 
      GSO_GHF_HHMMSS, GSO_GHF_END_TIME, 
      GSO_GHF_EXPOSURE, GSO_GHF_FIT_MODEL_ID, 
      W00_SLOW, W00_FAST, W01_SLOW, W01_FAST,
      W02_SLOW, W02_FAST, W03_SLOW, W03_FAST,
      W10_SLOW, W10_FAST, W11_SLOW, W11_FAST,
      W12_SLOW, W12_FAST, W13_SLOW, W13_FAST,
      W20_SLOW, W20_FAST, W21_SLOW, W21_FAST,
      W22_SLOW, W22_FAST, W23_SLOW, W23_FAST,
      W30_SLOW, W30_FAST, W31_SLOW, W31_FAST,
      W32_SLOW, W32_FAST, W33_SLOW, W33_FAST
      }; /** for column number **/
  
  char *param_boundaries[HXDGSOGHF_PMT_N_LINE]={
    "line (1)",
    "line (2)",
    "line (3)"
  };
  int hdu_num;
  GSO_GHF *slow_ghf_data[HXDGSOGHF_PMT_N_LINE];
  GSO_GHF *fast_ghf_data[HXDGSOGHF_PMT_N_LINE];
  double param[HXDGSOGHF_N_FITPARAM];
  int unit_id;

  long param_fbit = 1;
  long param_nbit = HXDGSOGHF_N_FITPARAM;
  
  /** line number check **/
  if (n_line > HXDGSOGHF_PMT_N_LINE){
    fprintf(stderr, "%s: line nuber (%d) is over maximum (%d)\n",
	    n_line, HXDGSOGHF_PMT_N_LINE);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** assign lines **/
  slow_ghf_data[0] = (GSO_GHF *) gdalpha_slow_ghf_data;
  fast_ghf_data[0] = (GSO_GHF *) gdalpha_fast_ghf_data;
  slow_ghf_data[1] = (GSO_GHF *) annihi_slow_ghf_data;
  fast_ghf_data[1] = (GSO_GHF *) annihi_fast_ghf_data;
  slow_ghf_data[2] = (GSO_GHF *) gd153_slow_ghf_data;
  fast_ghf_data[2] = (GSO_GHF *) gd153_fast_ghf_data;
  
  for (hdu_num=0; hdu_num < n_line; hdu_num++){
    /*** move to the corresponding extension ***/
    fits_movabs_hdu(hxd_gsogainhist_fits_fp, (hdu_num+2), &hdutype, &istat);
    if (istat){
      fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
	      tool_name, hdu_num+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    
    /** check data **/
    if ( slow_ghf_data[hdu_num]->nrow != fast_ghf_data[hdu_num]->nrow){
      fprintf(stderr, 
	      "%s: slow and fast data, nrow mismatched HDU%d (%d and %d)\n", 
	      tool_name, hdu_num+1, slow_ghf_data[hdu_num]->nrow,
	      fast_ghf_data[hdu_num]->nrow);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    } 
    /** update fits header (1) **/
    naxis2= (long) slow_ghf_data[hdu_num]->nrow;
    if (fits_modify_key_lng(hxd_gsogainhist_fits_fp, "NAXIS2", 
			    naxis2, "&", &istat)){
      fprintf(stderr, "%s:fits_update_key NAXIS2=%d failed HDU%d (%d)\n", 
	      tool_name, slow_ghf_data[hdu_num]->nrow, hdu_num+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    /** update fits header (2) **/
    /*
      if(fits_write_key_str(hxd_gsogainhist_fits_fp, "CBD10001", 
      param_boundaries[hdu_num],
      "Parameter boundaries", &istat)){
      fprintf(stderr, "%s: fits_write_key_str failed HDU%d (%d)\n", 
      tool_name, hdu_num+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
      }
    */
    
    /** write data **/
    for(row=0; row<slow_ghf_data[hdu_num]->nrow; row++){
      irow = row+1; /** fitsio is FORTRAN **/
      colnum = GSO_GHF_START_TIME;
      double_val = slow_ghf_data[hdu_num]->data[row].start_time;
      fits_write_col_dbl(hxd_gsogainhist_fits_fp, colnum, irow, 
			 firstelem, nelements, &double_val, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_dbl start_time failed HDU%d (%d)\n",
		tool_name, hdu_num+1, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }

      colnum = GSO_GHF_YYYYMMDD;
      int_val = slow_ghf_data[hdu_num]->data[row].start_yyyymmdd;
      fits_write_col_int(hxd_gsogainhist_fits_fp, colnum, irow,
			 firstelem, nelements, &int_val, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_int yyyymmdd failed HDU%d (%d)\n",
		tool_name, hdu_num+1, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }

      colnum = GSO_GHF_HHMMSS;
      int_val = slow_ghf_data[hdu_num]->data[row].start_hhmmss;
      fits_write_col_int(hxd_gsogainhist_fits_fp, colnum, irow, 
			 firstelem, nelements, &int_val, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_int hhmmss failed HDU%d (%d)\n",
		tool_name, hdu_num+1, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      
      colnum = GSO_GHF_END_TIME;
      double_val = slow_ghf_data[hdu_num]->data[row].end_time;
      fits_write_col_dbl(hxd_gsogainhist_fits_fp, colnum, irow, 
			 firstelem, nelements, &double_val, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_dbl end time failed HDU%d (%d)\n",
		tool_name, hdu_num+1, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      
      colnum = GSO_GHF_EXPOSURE;
      double_val = slow_ghf_data[hdu_num]->data[row].exposure;
      fits_write_col_dbl(hxd_gsogainhist_fits_fp, colnum, irow, 
			 firstelem, nelements, &double_val, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_dbl end time failed HDU%d (%d)\n",
		tool_name, hdu_num+1, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }

      colnum = GSO_GHF_FIT_MODEL_ID;
      uchar_val 
	= (unsigned char) slow_ghf_data[hdu_num]->data[row].fit_model_id;
      
      fits_write_col_byt(hxd_gsogainhist_fits_fp, colnum, irow, 
			 firstelem, nelements, &uchar_val, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_byt MODEL_ID failed HDU%d (%d)\n", 
		tool_name, hdu_num+1, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      /** write fit results **/
      for (unit_id=0; unit_id<HXD_WEL_N_UNIT; unit_id++ ){
	/** slow **/
	reformat_GHTBL_to_FitsParam(unit_id,
				    &slow_ghf_data[hdu_num]->data[row], 
				    param);
	colnum = W00_SLOW + unit_id*2;
	fits_write_col_dbl(hxd_gsogainhist_fits_fp, colnum, irow,
			   param_fbit, param_nbit, param, &istat);
	if(istat){
	  fprintf(stderr, 
		  "%s:fits_write_col_dbl W%d%d_SLOW failed HDU%d(%d)\n", 
		  tool_name, unit_id>>2, unit_id&0x3, hdu_num+1, istat);
	  status = HXDCALDBUTIL_STATUS_NG;
	  return status;
	}

	/** fast **/
	reformat_GHTBL_to_FitsParam(unit_id, 
				    &fast_ghf_data[hdu_num]->data[row], 
				    param);
	colnum = W00_SLOW + unit_id*2 + 1;
	fits_write_col_dbl(hxd_gsogainhist_fits_fp, colnum, irow,
			   param_fbit, param_nbit, param, &istat);
	if(istat){
	  fprintf(stderr, 
		  "%s:fits_write_col_dbl W%d%d_FAST failed HDU%d(%d)\n", 
		  tool_name, unit_id>>2, unit_id&0x3, hdu_num+1, istat);
	  status = HXDCALDBUTIL_STATUS_NG;
	  return status;
	}

      }

    } /** end of row **/

    if (fits_write_date(hxd_gsogainhist_fits_fp, &istat)) {
      fprintf(stderr,"%s: fits_write_date failed (%d)\n", tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    
    if (fits_write_chksum(hxd_gsogainhist_fits_fp, &istat)) {
      fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", tool_name, 
	      istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
  } /** end of hdu **/

  if(fits_flush_file(hxd_gsogainhist_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /******** Add Primary Header keywords ********/
  if (hxdcaldbUtil_write_fits_priheader(hxd_gsogainhist_fits_fp, detname)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}


/* ========================================
 *   Fits File  Read
 * ========================================*/

int 
hxdcaldbUtil_gsogainhist_open_FITS(char* gsogainhist_fits_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  if (fits_open_file(&hxd_gsogainhist_fits_fp,gsogainhist_fits_fname,
		     READONLY, &istat)) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
	    gsogainhist_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  return status;
}

/*
 *  INPUT:  tstart/tstop
 *  OUTPUT: GSO_GHF[HXDGSOGHF_PMT_N_LINE]    table for each line (slow)
 *          GSO_GHF[HXDGSOGHF_PMT_N_LINE]    table for each line (fast)
 *          numrow                           number of valid row 
 */
int 
hxdcaldbUtil_gsogainhist_read_FITS(double tstart, double tstop,
				   GSO_GHF* gdalpha_slow_ghf_data,
				   GSO_GHF* gdalpha_fast_ghf_data,
				   GSO_GHF* annihi_slow_ghf_data,
				   GSO_GHF* annihi_fast_ghf_data,
				   GSO_GHF* gd153_slow_ghf_data,
				   GSO_GHF* gd153_fast_ghf_data,
				   int* numrow){

  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  int hdu_num, hdutype;
  int colnum;
  long irow, nrow;
  int startrow, stoprow;
  char comment[80];
  long firstelem = 1;
  long nelements = 1;
  double        double_val;
  signed int    int_val;
  unsigned char uchar_val;
  int anynul;
  signed int    int_nulval = 0;
  double        double_nulval = 0.0;
  unsigned char uchar_nulval = 0;
  int unit_id;
  int i;

  GSO_GHF* slow_ghf_data[HXDGSOGHF_PMT_N_LINE];
  GSO_GHF* fast_ghf_data[HXDGSOGHF_PMT_N_LINE];

  double ghf_start_time;
  double param_slow[HXDGSOGHF_N_FITPARAM];
  double param_fast[HXDGSOGHF_N_FITPARAM];
  long param_fbit = 1;
  long param_nbit = HXDGSOGHF_N_FITPARAM;

  enum{
    GSO_GHF_DUMMY_C_FORTRUN,
      GSO_GHF_START_TIME, GSO_GHF_YYYYMMDD, 
      GSO_GHF_HHMMSS, GSO_GHF_END_TIME, 
      GSO_GHF_EXPOSURE, GSO_GHF_FIT_MODEL_ID, 
      W00_SLOW, W00_FAST, W01_SLOW, W01_FAST,
      W02_SLOW, W02_FAST, W03_SLOW, W03_FAST,
      W10_SLOW, W10_FAST, W11_SLOW, W11_FAST,
      W12_SLOW, W12_FAST, W13_SLOW, W13_FAST,
      W20_SLOW, W20_FAST, W21_SLOW, W21_FAST,
      W22_SLOW, W22_FAST, W23_SLOW, W23_FAST,
      W30_SLOW, W30_FAST, W31_SLOW, W31_FAST,
      W32_SLOW, W32_FAST, W33_SLOW, W33_FAST
      }; /** for column number **/

  /** assign lines **/
  slow_ghf_data[0] = (GSO_GHF *) gdalpha_slow_ghf_data;
  fast_ghf_data[0] = (GSO_GHF *) gdalpha_fast_ghf_data;
  slow_ghf_data[1] = (GSO_GHF *) annihi_slow_ghf_data;
  fast_ghf_data[1] = (GSO_GHF *) annihi_fast_ghf_data;
  slow_ghf_data[2] = (GSO_GHF *) gd153_slow_ghf_data;
  fast_ghf_data[2] = (GSO_GHF *) gd153_fast_ghf_data;
  
  for (hdu_num=0; hdu_num < HXDGSOGHF_PMT_N_LINE; hdu_num++){

    /** move to the HDU **/
    fits_movabs_hdu(hxd_gsogainhist_fits_fp, (hdu_num+2), &hdutype, &istat);
    if (istat){
      fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
	      tool_name, hdu_num+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    /** read number of rows **/
    fits_read_key_lng(hxd_gsogainhist_fits_fp, "NAXIS2", &nrow, 
                      comment, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_key NAXIS2 failed (%d)\n", 
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    /** search row region, using tstart and tstop (only hdu_num==0) **/
    if ( hdu_num == 0 ){
      ghf_start_time = 0.0;
      startrow = 0; stoprow = 0; 

      if ( nrow==1 ){
	startrow = 1; stoprow = 1; *numrow = 1 ;
      }else{
	for(irow=1;irow<=nrow;irow++){
	  colnum = GSO_GHF_START_TIME;
	  fits_read_col_dbl(hxd_gsogainhist_fits_fp, colnum, 
			    irow, firstelem, nelements, double_nulval,
			    &ghf_start_time, &anynul, &istat);
	  if(istat){
	    fprintf(stderr, "%s:fits_read_col_dbl start_time failed (%d)\n", 
		    tool_name, istat);
	    status = HXDCALDBUTIL_STATUS_NG;
	    return status;
	  }
	  if ( (startrow==0) && (ghf_start_time>=tstart) ){
	    startrow = (irow>1) ? (irow-1):1 ;
	  }
	  if ( (stoprow==0) && (ghf_start_time>tstop) ){
	    stoprow = irow ;
	    break;
	  }
	}
	if ( startrow==0 ) { /* no valid row */
	  status = HXDCALDBUTIL_STATUS_NG;
	  return status;
	}
	if ( stoprow==0) stoprow = nrow;
	*numrow = ( stoprow - startrow ) + 1 ;
      }
      if(DEBUG)
	fprintf(stderr, "HDU-%d, startrow=%d, stoprow=%d\n",
		hdu_num+1, startrow, stoprow);
    } /* end of if (hdu_num == 0) */

    /** read the data **/
    for(irow=startrow;irow<=stoprow;irow++){
      i = irow - startrow ;

      /* 1. time */
      colnum = GSO_GHF_START_TIME;
      fits_read_col_dbl(hxd_gsogainhist_fits_fp, colnum, 
                        irow, firstelem, nelements, double_nulval,
                        &ghf_start_time, &anynul, &istat);
      slow_ghf_data[hdu_num]->data[i].start_time = ghf_start_time;
      fast_ghf_data[hdu_num]->data[i].start_time = ghf_start_time;

      /* 2. yyyymmdd */
      colnum = GSO_GHF_YYYYMMDD;
      fits_read_col_int(hxd_gsogainhist_fits_fp, colnum, 
			irow, firstelem, nelements, int_nulval,
			&int_val, &anynul, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_read_col_int yyyymmdd failed (%d)\n", 
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      slow_ghf_data[hdu_num]->data[i].start_yyyymmdd = int_val;
      fast_ghf_data[hdu_num]->data[i].start_yyyymmdd = int_val;

      /* 3. hhmmss */
      colnum = GSO_GHF_HHMMSS;
      fits_read_col_int(hxd_gsogainhist_fits_fp, colnum, 
			irow, firstelem, nelements, int_nulval,
			&int_val, &anynul, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_read_col_int yyyymmdd failed (%d)\n", 
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      slow_ghf_data[hdu_num]->data[i].start_hhmmss = int_val;
      fast_ghf_data[hdu_num]->data[i].start_hhmmss = int_val;

      /* 4. end time*/
      colnum = GSO_GHF_END_TIME;
      fits_read_col_dbl(hxd_gsogainhist_fits_fp, colnum, 
			irow, firstelem, nelements, double_nulval,
			&double_val, &anynul, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_read_col_dbl end_time failed (%d)\n", 
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      slow_ghf_data[hdu_num]->data[i].end_time = double_val;
      fast_ghf_data[hdu_num]->data[i].end_time = double_val;
      
      /* 5. exposure */
      colnum = GSO_GHF_EXPOSURE;
      fits_read_col_dbl(hxd_gsogainhist_fits_fp, colnum, 
			irow, firstelem, nelements, double_nulval,
			&double_val, &anynul, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_read_col_dbl exposure failed (%d)\n", 
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      slow_ghf_data[hdu_num]->data[i].exposure = double_val;
      fast_ghf_data[hdu_num]->data[i].exposure = double_val;
      
      /* 6. model id */
      colnum = GSO_GHF_FIT_MODEL_ID;
      fits_read_col_byt(hxd_gsogainhist_fits_fp, colnum, 
			irow, firstelem, nelements, double_nulval,
			&uchar_val, &anynul, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_read_col_byt model id failed (%d)\n", 
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      slow_ghf_data[hdu_num]->data[i].fit_model_id = (int) uchar_val;
      fast_ghf_data[hdu_num]->data[i].fit_model_id = (int) uchar_val;
    
      /* 7. fit results */
      for (unit_id=0; unit_id<HXD_WEL_N_UNIT; unit_id++ ){
	/** slow **/
	colnum = W00_SLOW + unit_id*2 ;
	fits_read_col_dbl(hxd_gsogainhist_fits_fp, colnum, irow,
			  param_fbit, param_nbit, double_nulval,
			  param_slow, &anynul, &istat);
	if(DEBUG) fprintf(stderr, 
		  "col=%d row=%d bit-%d .. %dbits, null=%f param[2]=%f\n", 
			  colnum, irow,
			  param_fbit, param_nbit, double_nulval,
			  param_slow[2]);
	if(istat){
	  fprintf(stderr, 
	  "%s:fits_read_col_dbl W%d%d_FAST failed HDU%d col=%d (%d)\n", 
	  tool_name, unit_id>>2, unit_id&0x3, hdu_num+1, colnum, istat);
	  status = HXDCALDBUTIL_STATUS_NG;
	  return status;
	}
	reformat_FitsParam_to_GHTBL(unit_id, param_slow, 
				    &slow_ghf_data[hdu_num]->data[i]);

	/** fast **/
	colnum = W00_SLOW + unit_id*2 +1;
	fits_read_col_dbl(hxd_gsogainhist_fits_fp, colnum, irow,
			  param_fbit, param_nbit, double_nulval,
			  param_fast, &anynul, &istat);
	if(DEBUG) fprintf(stderr, 
		  "col=%d row=%d bit-%d .. %dbits, null=%f param[2]=%f\n", 
			  colnum, irow,
			  param_fbit, param_nbit, double_nulval,
			  param_fast[2]);
	if(istat){
	  fprintf(stderr, 
		"%s:fits_read_col_dbl W%d%d_FAST failed HDU%d col=%d (%d)\n", 
		tool_name, unit_id>>2, unit_id&0x3, hdu_num+1, colnum, istat);
	  status = HXDCALDBUTIL_STATUS_NG;
	  return status;
	}
	
	reformat_FitsParam_to_GHTBL(unit_id, param_fast,
				    &fast_ghf_data[hdu_num]->data[i]);
	
      } /** end of unit **/

    } /** end of startrow-stoprow **/

  } /** end of HDU **/
  return status;
}

int 
hxdcaldbUtil_gsogainhist_close_FITS(void){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  
  if(fits_close_file(hxd_gsogainhist_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  return status;
}


static void
reformat_GHTBL_to_FitsParam(int unit, GSO_GH_TBL *tbl, double *fitsparam){
  fitsparam[ 0] = tbl->fit_start_ch[unit];
  fitsparam[ 1] = tbl->fit_end_ch[unit];
  fitsparam[ 2] = tbl->fit_gauss_peak[unit];
  fitsparam[ 3] = tbl->fit_gauss_peak_err[unit];
  fitsparam[ 4] = tbl->fit_gauss_norm[unit];
  fitsparam[ 5] = tbl->fit_gauss_norm_err[unit];
  fitsparam[ 6] = tbl->fit_gauss_sigma[unit];
  fitsparam[ 7] = tbl->fit_gauss_sigma_err[unit];
  fitsparam[ 8] = tbl->fit_pow_norm[unit];
  fitsparam[ 9] = tbl->fit_pow_norm_err[unit];
  fitsparam[10] = tbl->fit_pow_index[unit];
  fitsparam[11] = tbl->fit_pow_index_err[unit];
  fitsparam[12] = tbl->redu_chi_sq[unit];
  fitsparam[13] = tbl->area_flux[unit];
  fitsparam[14] = tbl->quality[unit];
}

static void
reformat_FitsParam_to_GHTBL(int unit, double *fitsparam, GSO_GH_TBL *tbl){
  tbl->fit_start_ch[unit]       = fitsparam[ 0];
  tbl->fit_end_ch[unit]         = fitsparam[ 1];
  tbl->fit_gauss_peak[unit]     = fitsparam[ 2];
  tbl->fit_gauss_peak_err[unit] = fitsparam[ 3];
  tbl->fit_gauss_norm[unit]     = fitsparam[ 4];
  tbl->fit_gauss_norm_err[unit] = fitsparam[ 5];
  tbl->fit_gauss_sigma[unit]    = fitsparam[ 6];
  tbl->fit_gauss_sigma_err[unit]= fitsparam[ 7];
  tbl->fit_pow_norm[unit]       = fitsparam[ 8];
  tbl->fit_pow_norm_err[unit]   = fitsparam[ 9];
  tbl->fit_pow_index[unit]      = fitsparam[10];
  tbl->fit_pow_index_err[unit]  = fitsparam[11];
  tbl->redu_chi_sq[unit]        = fitsparam[12];
  tbl->area_flux[unit]          = fitsparam[13];
  tbl->quality[unit]            = fitsparam[14];
}
