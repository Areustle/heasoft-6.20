#ifndef _HXD_CALDB_UTIL_H_
#define _HXD_CALDB_UTIL_H_
/*
 *   hxdcaldbUtil
 *         v0.0.1; 2005-01-14, created by Y.Terada
 *                 define the file format, only support ASCII Files.
 *         v0.0.2; 2005-01-25, 
 *                 change HXDGSOLIN_ADCINL_N_CH
 *         v0.1.0; 2005-01-28, 
 *                 support fitsio for hxdgso.fits, by Y.Terada, T.Kishishita
 *         v0.1.1; 2005-02-02,
 *                 support fitsio for hxdpin.fits, by T.Kishishita
 *         v0.1.2; 2005-02-04,
 *                 support GSO gainhistory fits file, by T.Kishishita
 *                 support trn_ph_tbl,            by Y.Terada
 *         v0.1.3; 2005-02-05,
 *                 support PIN gainhistory fits file, by Y.Terada
 *                 debug   GSO gainhistory fits file, by Y.Terada
 *         v0.1.4; 2005-02-07,
 *                 debug, in reading PIN/GSO gainhistories, by Y.Terada
 *                 support PIN LD Threshold, for hxdgrade,  by Y.Terada
 *         v0.2.0; 2005-05-06,
 *                 create arfdb I/O   by Y.Terada
 *         v0.2.1; 2005-05-09,
 *                 add arfdb read     by Y.Terada
 *         v0.2.2; 2005-05-09,
 *                 add teldef read    by Y.Terada
 *         v0.2.3; 2005-05-10,
 *                 update teldef inf  by Y.Terada
 *         v0.2.4; 2005-05-17         by Y.Terada
 *                 change INSTRUME and DETNAM 
 *                 INSTRUME is fixed to be a value of 'HXD'
 *                 DETNAM is, 'WELL_GSO' 'WELL_PIN' 'WAM_ANTI'
 *         v0.2.5; 2005-05-19         by Y.Terada
 *                 support gcc 2.95.x
 *         v0.3.0; 2005-05-25         by Y.Terada
 *                 change structure of Gain History, GSO_GHF.
 *                 change format of gain history FITS.
 *         v0.3.1; 2005-05-28         by T.Kitaguchi
 *                 support PSD selection ASCII file
 *         v0.3.3; 2005-06-03         by T.Kitaguchi
 *                 support PSD selection FITS file
 *         v0.3.5; 2005-06-09  by Y.Terada
 *                 add exposure information in ASCII I/O
 *                 add QUALITY column in gain gistory FITS.
 *         v0.4.0; 2005-06-13  by Y.Terada
 *                 include GSFC comments (2005-06-08).
 *         v0.4.1; 2005-06-25  by Y.Terada
 *                 add Primary Header
 *         v0.4.2; 2005-08-30, by T.Kitaguchi
 *                 support ae_hxd_pinthr.fits
 *         v0.4.3; 2005-09-28 Y.Terada
 *                 include GSFC comment
 *         v0.4.4; 2005-10-22 Y.Terada
 *                 New Gain History Format,
 *                     (suggest by Lorella-san and Ebisawa-san) 
 *         v0.4.5; 2005-10-24 Y.Terada
 *                 backward Compatibility
 *         v0.4.7; 2005-11-16 T.Kitaguchi
 *                 change format of psdsel FITS
 *         v0.4.9; 2005-11-29 Y.Terada,
 *                 write BOUNDARY in a FITS header
 *         v0.5.1; 2006-05-01 T.Kitaguchi
 *                 expand HXDPSDSEL_NROW (5121 -> 8192)
 *         v0.5.2; 2006-05-26 M.K
 *                 add tstop in hxdcaldbUtil_gsogainhist_read_FITS()
 *                 GSO_GHF_ROW_MAX (1024 -> 16384)
 *         v0.6.0; 2006-08-29 Y.Terada
 *                 add New File: bstidt I/O, rename files
 *         v0.6.1; 2006-08-30 Y.Terada
 *                 add New File: gsoght I/O
 *         v0.6.2; 2006-08-31 Y.Terada
 *                 format changed (as version 1.2.2.3 caldb)
 *         v0.6.3; 2006-09-15 Y.Terada
 *                 change format of bstidt
 *                 add wamghf, new format
 *         v0.6.4; 2006-09-29 Y.Terada
 *                 change format of bstidt
 *         v0.6.5; 2006-10-26 M.K
 *                 GSO_GHF_ROW_MAX (16384 -> 32768)
 *         v0.6.6; 2007-04-24, Y.Terada
 *                 GSOGHT: add reserve space
 *                 BSTIDT: change format
 *         v0.6.7; 2007-05-01, Y.Terada
 *                 BSTIDT: add functions
 *         v0.6.8; 2007-05-10, M.K
 *                 GSO_GHT_ROW_MAX (16384 -> 4096)
 *         v0.7.2; 2008-07-29, Y.Terada
 *                 BSTIDT: add function for hxdbsttime
 *         v0.7.3; 2008-11-02, Y.Terada
 *                 update checksum in BSTIDT.
 *         v0.7.4; 2009-10-20, Y.Terada
 *                 add gsoghp file for hxdpi.
 *         v0.7.5; 2009-10-21, Y.Terada
 *                 gsoghp --> gsogpt 
 *         v0.7.6; 2009-12-29, K.Yamaoka
 *                 wamghf release
 *         v0.7.7; 2010-01-05, Y.Terada
 *                 read version of gsogpt
 */

#include "fitsio.h"

#define HXDCALDBUTIL_STATUS_OK 1
#define HXDCALDBUTIL_STATUS_NG 0
#define HXD_WEL_N_UNIT 16


/* ===================================================================
 *                       Util
 * =================================================================== */
int hxdcaldbUtil_ask_date( void );
int hxdcaldbUtil_set_date( char * fits_date, char *fits_time, int version );
int hxdcaldbUtil_write_fits_header(fitsfile *fits_fp, char *detname,
				   char *code_name, char *description,
				   char *file_name,
				   int use_boundary, char *boundary);

int hxdcaldbUtil_write_fits_priheader(fitsfile *fits_fp, char *detname);

/* ===================================================================
 *                       ae_hxd_gsolin_YYYYMMDD.fits
 * =================================================================== */
#define HXDGSOLIN_ADCDNL_N_CH 4096
#define HXDGSOLIN_ADCINL_N_CH 64
/*  First Extension 
 *         UNIT_ID, (PHA_SLOW, ADC_SLOW_WIDTH, ADC_SLOW_START) x 4096,
 *         (PHA_FAST, ADC_FAST_WIDTH, ADC_FAST_START) x 4096
 */
typedef struct {
  double pha_slow      [HXD_WEL_N_UNIT][HXDGSOLIN_ADCDNL_N_CH];
  double adc_slow_width[HXD_WEL_N_UNIT][HXDGSOLIN_ADCDNL_N_CH];
  double adc_slow_start[HXD_WEL_N_UNIT][HXDGSOLIN_ADCDNL_N_CH];
  double pha_fast      [HXD_WEL_N_UNIT][HXDGSOLIN_ADCDNL_N_CH];
  double adc_fast_width[HXD_WEL_N_UNIT][HXDGSOLIN_ADCDNL_N_CH];
  double adc_fast_start[HXD_WEL_N_UNIT][HXDGSOLIN_ADCDNL_N_CH];
} HxdGsoLin_ADCDNL;

/*  Second Extension 
 *          UNIT_ID, (ADC_PI_SLOW, AE_PI_SLOW) x 4096?,
 *         (ADC_PI_FAST, AE_PI_FAST) x 4096?
 */
typedef struct {
  double adc_pi_slow[HXD_WEL_N_UNIT][HXDGSOLIN_ADCINL_N_CH];
  double ae_pi_slow [HXD_WEL_N_UNIT][HXDGSOLIN_ADCINL_N_CH];
  double adc_pi_fast[HXD_WEL_N_UNIT][HXDGSOLIN_ADCINL_N_CH];
  double ae_pi_fast [HXD_WEL_N_UNIT][HXDGSOLIN_ADCINL_N_CH];
} HxdGsoLin_ADCINL;

int hxdcaldbUtil_hxdgsolin_adcdnl_open_ASCII (char* hxdgsolin_adcdnl_fname);
int hxdcaldbUtil_hxdgsolin_adcdnl_read_ASCII (HxdGsoLin_ADCDNL* data);
int hxdcaldbUtil_hxdgsolin_adcdnl_close_ASCII(void);
int hxdcaldbUtil_hxdgsolin_adcinl_open_ASCII (char* hxdgsolin_adcinl_fname);
int hxdcaldbUtil_hxdgsolin_adcinl_read_ASCII (HxdGsoLin_ADCINL* data);
int hxdcaldbUtil_hxdgsolin_adcinl_close_ASCII(void);

int hxdcaldbUtil_hxdgsolin_create_FITS(char* hxdgsolin_fname);
int hxdcaldbUtil_hxdgsolin_adcdnl_create_FITS( char* hxdgsolin_fname );
int hxdcaldbUtil_hxdgsolin_adcinl_create_FITS( char* hxdgsolin_fname );
int hxdcaldbUtil_hxdgsolin_adcdnl_write_FITS(HxdGsoLin_ADCDNL* dnl_data);
int hxdcaldbUtil_hxdgsolin_adcinl_write_FITS(HxdGsoLin_ADCINL* inl_data);
int hxdcaldbUtil_hxdgsolin_merge_FITS(void);

#define HXDGSOLIN_PRIMARY_HDU_ID 1
#define HXDGSOLIN_ADCDNL_HDU_ID  2
#define HXDGSOLIN_ADCINL_HDU_ID  3
int hxdcaldbUtil_hxdgsolin_open_FITS(char* hxdgsolin_fname);
int hxdcaldbUtil_hxdgsolin_adcdnl_read_FITS(HxdGsoLin_ADCDNL* dnl_data);
int hxdcaldbUtil_hxdgsolin_adcinl_read_FITS(HxdGsoLin_ADCINL* inl_data);
int hxdcaldbUtil_hxdgsolin_close_FITS(void);

/* ===================================================================
 *                       ae_hxd_pinlin_YYYYMMDD.fits
 * =================================================================== */
#define HXDPINLIN_ADCINL_N_CH 256
#define HXDPINLIN_GAIN_N_CH 64
/*  First Extension 
 *        UNIT_ID, (PHA_PIN0, AE_PI_PIN0) x 256,
 *        (PHA_PIN1, AE_PI_PIN1) x 256, (PHA_PIN2, AE_PI_PIN2) x 256,
 *         (PHA_PIN3, AE_PI_PIN3) x 256
 */
typedef struct {
  double pha_pin0  [HXD_WEL_N_UNIT][HXDPINLIN_ADCINL_N_CH];
  double ae_pi_pin0[HXD_WEL_N_UNIT][HXDPINLIN_ADCINL_N_CH];
  double pha_pin1  [HXD_WEL_N_UNIT][HXDPINLIN_ADCINL_N_CH];
  double ae_pi_pin1[HXD_WEL_N_UNIT][HXDPINLIN_ADCINL_N_CH];
  double pha_pin2  [HXD_WEL_N_UNIT][HXDPINLIN_ADCINL_N_CH];
  double ae_pi_pin2[HXD_WEL_N_UNIT][HXDPINLIN_ADCINL_N_CH];
  double pha_pin3  [HXD_WEL_N_UNIT][HXDPINLIN_ADCINL_N_CH];
  double ae_pi_pin3[HXD_WEL_N_UNIT][HXDPINLIN_ADCINL_N_CH];
} HxdPinLin_ADCINL;

/*  Second Extension 
 *        UNIT_ID, PIN0_GAIN, PIN0_OFFSET, PIN1_GAIN, PIN1_OFFSET,
 *         PIN2_GAIN, PIN2_OFFSET, PIN3_GAIN, PIN3_OFFSET
 */
typedef struct {
  double pin0_gain  [HXD_WEL_N_UNIT];
  double pin0_offset[HXD_WEL_N_UNIT];
  double pin1_gain  [HXD_WEL_N_UNIT];
  double pin1_offset[HXD_WEL_N_UNIT];
  double pin2_gain  [HXD_WEL_N_UNIT];
  double pin2_offset[HXD_WEL_N_UNIT];
  double pin3_gain  [HXD_WEL_N_UNIT];
  double pin3_offset[HXD_WEL_N_UNIT];
} HxdPinLin_GAIN;

int hxdcaldbUtil_hxdpinlin_adcinl_open_ASCII (char* hxdpinlin_adcinl_fname);
int hxdcaldbUtil_hxdpinlin_adcinl_read_ASCII (HxdPinLin_ADCINL* data);
int hxdcaldbUtil_hxdpinlin_adcinl_close_ASCII(void);
int hxdcaldbUtil_hxdpinlin_gain_open_ASCII (char* hxdpinlin_gain_fname);
int hxdcaldbUtil_hxdpinlin_gain_read_ASCII (HxdPinLin_GAIN* data);
int hxdcaldbUtil_hxdpinlin_gain_close_ASCII(void);

int hxdcaldbUtil_hxdpinlin_create_FITS(char* hxdpinlin_fname);
int hxdcaldbUtil_hxdpinlin_adcinl_create_FITS( char* hxdpinlin_fname );
int hxdcaldbUtil_hxdpinlin_gain_create_FITS( char* hxdpinlin_fname );
int hxdcaldbUtil_hxdpinlin_adcinl_write_FITS(HxdPinLin_ADCINL* inl_data);
int hxdcaldbUtil_hxdpinlin_gain_write_FITS(HxdPinLin_GAIN* gain_data);

#define HXDPINLIN_PRIMARY_HDU_ID 1
#define HXDPINLIN_ADCINL_HDU_ID  2
#define HXDPINLIN_GAIN_HDU_ID  3
int hxdcaldbUtil_hxdpinlin_merge_FITS(void);
int hxdcaldbUtil_hxdpinlin_open_FITS(char* hxdpinlin_fname);
int hxdcaldbUtil_hxdpinlin_adcinl_read_FITS(HxdPinLin_ADCINL* inl_data);
int hxdcaldbUtil_hxdpinlin_gain_read_FITS(HxdPinLin_GAIN* gain_data);
int hxdcaldbUtil_hxdpinlin_close_FITS(void);

/* ===================================================================
 *                    GSO gain history ****.ghf
 *                    ae_hxd_gsoghf_YYYYMMDD.fits
 * =================================================================== */
typedef struct{
  double start_time;
  double end_time;
  int start_yyyymmdd;
  int start_hhmmss;
  double exposure;
  int fit_model_id;
  double fit_start_ch       [HXD_WEL_N_UNIT];
  double fit_end_ch         [HXD_WEL_N_UNIT];
  double fit_gauss_peak     [HXD_WEL_N_UNIT];
  double fit_gauss_peak_err [HXD_WEL_N_UNIT];
  double fit_gauss_norm     [HXD_WEL_N_UNIT];
  double fit_gauss_norm_err [HXD_WEL_N_UNIT];
  double fit_gauss_sigma    [HXD_WEL_N_UNIT];
  double fit_gauss_sigma_err[HXD_WEL_N_UNIT];
  double fit_pow_norm       [HXD_WEL_N_UNIT];
  double fit_pow_norm_err   [HXD_WEL_N_UNIT];
  double fit_pow_index      [HXD_WEL_N_UNIT];
  double fit_pow_index_err  [HXD_WEL_N_UNIT];
  double redu_chi_sq        [HXD_WEL_N_UNIT];
  double area_flux          [HXD_WEL_N_UNIT];
  int    quality            [HXD_WEL_N_UNIT];
} GSO_GH_TBL;

#define HXDGSOGHF_PMT_N_LINE 3
#define HXDGSOGHF_N_FITPARAM 15
#define GSO_GHF_ROW_MAX 32768

typedef struct {
  int nrow;
  GSO_GH_TBL data[GSO_GHF_ROW_MAX];
} GSO_GHF;

#define HXDGSOGHF_PRIMARY_HDU_ID  1
#define HXDGSOGHF_PMTGDFIT_HDU_ID_OFFSET 2


int hxdcaldbUtil_gsogainhist_open_ASCII (char* gsogainhist_ascii_fname);
int hxdcaldbUtil_gsogainhist_read_ASCII (GSO_GHF* gso_slow_ghf_data,
					 GSO_GHF* gso_fast_ghf_data);
int hxdcaldbUtil_gsogainhist_close_ASCII(void);

int hxdcaldbUtil_gsogainhist_create_FITS(char* gsogainhist_fits_fname);
int hxdcaldbUtil_gsogainhist_write_FITS(int n_line, 
					GSO_GHF* gdalpha_slow_ghf_data,
					GSO_GHF* gdalpha_fast_ghf_data,
					GSO_GHF* annihi_slow_ghf_data,
					GSO_GHF* annihi_fast_ghf_data,
					GSO_GHF* gd153_slow_ghf_data,
					GSO_GHF* gd153_fast_ghf_data);

int hxdcaldbUtil_gsogainhist_open_FITS(char* gsogainhist_fits_fname);
int hxdcaldbUtil_gsogainhist_read_FITS(double tstart, double tstop,
				       GSO_GHF* gdalpha_slow_ghf_data,
				       GSO_GHF* gdalpha_fast_ghf_data,
				       GSO_GHF* annihi_slow_ghf_data,
				       GSO_GHF* annihi_fast_ghf_data,
				       GSO_GHF* gd153_slow_ghf_data,
				       GSO_GHF* gd153_fast_ghf_data,
				       int* numrow);
/*
 *  INPUT:  tstart, tstop
 *  OUTPUT: GSO_GHF[HXDGSOGHF_PMT_N_LINE]    table for each line (slow)
 *          GSO_GHF[HXDGSOGHF_PMT_N_LINE]    table for each line (fast)
 *          numrow                           number of valid row 
 */

int hxdcaldbUtil_gsogainhist_close_FITS(void);


/* ===================================================================
 *                    GSO gain history parameter table
 *                    ae_hxd_gsoght_YYYYMMDD.fits
 * =================================================================== */
typedef struct{
  double start_time;
  double end_time;
  int start_yyyymmdd;
  int start_hhmmss;
  int model_id;
  double slow_param_a  [HXD_WEL_N_UNIT];
  double slow_param_b  [HXD_WEL_N_UNIT];
  double slow_param_c  [HXD_WEL_N_UNIT];
  double slow_longterm [HXD_WEL_N_UNIT];
  double fast_param_a  [HXD_WEL_N_UNIT];
  double fast_param_b  [HXD_WEL_N_UNIT];
  double fast_param_c  [HXD_WEL_N_UNIT];
  double fast_longterm [HXD_WEL_N_UNIT];
} GSO_GHT_TBL;

#define HXDGSOGHT_PMT_N_LINE 3
#define HXDGSOGHT_N_PARAM 8
#define GSO_GHT_ROW_MAX 4096

typedef struct {
  int nrow;
  GSO_GHT_TBL data[GSO_GHT_ROW_MAX];
} GSO_GHT;

#define HXDGSOGHT_PRIMARY_HDU_ID  1
#define HXDGSOGHT_PMTGDFIT_HDU_ID_OFFSET 2

int hxdcaldbUtil_gsoght_open_ASCII (char* gsoght_ascii_fname);
int hxdcaldbUtil_gsoght_read_ASCII (GSO_GHT* gsoght_data);
int hxdcaldbUtil_gsoght_close_ASCII(void);

int hxdcaldbUtil_gsoght_create_FITS(char* gsoght_fits_fname);
int hxdcaldbUtil_gsoght_write_row_FITS(int irow, GSO_GHT_TBL* ght_tbl);
int hxdcaldbUtil_gsoght_write_FITS(GSO_GHT* gdalpha_ght_data,
				   GSO_GHT* annihi_ght_data,
				   GSO_GHT* gd153_ght_data);

int hxdcaldbUtil_gsoght_open_FITS(char* gsoght_fits_fname);
int hxdcaldbUtil_gsoght_read_row_FITS(int irow, GSO_GHT_TBL* ght_tbl);
int hxdcaldbUtil_gsoght_read_FITS(double tstart, double tstop,
				  GSO_GHT* gdalpha_ght_data,
				  GSO_GHT* annihi_ght_data,
				  GSO_GHT* gd153_ght_data, 
				  int* numrow);
int hxdcaldbUtil_gsoght_close_FITS(void);


/* ===================================================================
 *                    GSO gain history parameter
 *                    ae_hxd_gsogpt_YYYYMMDD.fits
 * =================================================================== */
#define HXDPI_NUM_SAA_PASS 8
typedef struct{
  double start_time;
  double end_time;
  int start_yyyymmdd;
  int start_hhmmss;
  int model_id;
  double longterm_a [HXD_WEL_N_UNIT * 2]; /* 01 */
  double longterm_b [HXD_WEL_N_UNIT * 2]; /* 02 */
  double longterm_c [HXD_WEL_N_UNIT * 2]; /* 03 */
  double longterm_d [HXD_WEL_N_UNIT * 2]; /* 04 */
  double longterm_e [HXD_WEL_N_UNIT * 2]; /* 05 */
  double temp_a     [HXD_WEL_N_UNIT * 2]; /* 06 */
  double temp_b     [HXD_WEL_N_UNIT * 2]; /* 07 */
  double tsaa_a     [HXD_WEL_N_UNIT * 2 * HXDPI_NUM_SAA_PASS];  /* 08-15 */
  double tsaa_b     [HXD_WEL_N_UNIT * 2]; /* 16 */
  double tsaa_c     [HXD_WEL_N_UNIT * 2]; /* 17 */
  double mod_ga_1a  [HXD_WEL_N_UNIT * 2]; /* 18 */
  double mod_ga_1b  [HXD_WEL_N_UNIT * 2]; /* 19 */
  double mod_ga_1c  [HXD_WEL_N_UNIT * 2]; /* 20 */
  double mod_ga_2a  [HXD_WEL_N_UNIT * 2]; /* 21 */
  double mod_ga_2b  [HXD_WEL_N_UNIT * 2]; /* 22 */
  double mod_ga_2c  [HXD_WEL_N_UNIT * 2]; /* 23 */
  double mod_ga_3a  [HXD_WEL_N_UNIT * 2]; /* 24 */
  double mod_ga_3b  [HXD_WEL_N_UNIT * 2]; /* 25 */
  double mod_ga_3c  [HXD_WEL_N_UNIT * 2]; /* 26 */
  double mod_c      [HXD_WEL_N_UNIT * 2]; /* 27 */
} GSO_GPT_TBL;

#define HXDGSOGPT_PMT_N_LINE 1
#define HXDGSOGPT_N_PARAM 32
#define GSO_GPT_ROW_MAX 1024

typedef struct {
  int nrow;
  GSO_GPT_TBL data[GSO_GPT_ROW_MAX];
} GSO_GPT;

#define HXDGSOGPT_PRIMARY_HDU_ID  1
#define HXDGSOGPT_PMTGDFIT_HDU_ID_OFFSET 2
#define HXDGSOGPT_VERSION_UNDEFINED -1

int hxdcaldbUtil_gsogpt_open_ASCII (char* gsogpt_ascii_fname);
int hxdcaldbUtil_gsogpt_read_ASCII (GSO_GPT* gsogpt_data);
int hxdcaldbUtil_gsogpt_close_ASCII(void);

int hxdcaldbUtil_gsogpt_create_FITS(char* gsogpt_fits_fname);
int hxdcaldbUtil_gsogpt_write_row_FITS(int irow, GSO_GPT_TBL* gpt_tbl);
int hxdcaldbUtil_gsogpt_write_FITS(GSO_GPT* gpt_data);

int hxdcaldbUtil_gsogpt_open_FITS(char* gsogpt_fits_fname);
int hxdcaldbUtil_gsogpt_read_row_FITS(int irow, GSO_GPT_TBL* gpt_tbl);
int hxdcaldbUtil_gsogpt_read_FITS(double tstart, double tstop,
				  GSO_GPT* gpt_data,  int numrow);
int hxdcaldbUtil_gsogpt_read_VERSION_FITS(int *version);
int hxdcaldbUtil_gsogpt_close_FITS(void);

/* ===================================================================
 *                    PIN gain history ****.ghf
 *                    ae_hxd_pinghf_YYYYMMDD.fits
 * =================================================================== */
typedef struct {
  double start_time;
  double end_time;
  int start_yyyymmdd;
  int start_hhmmss;
  double exposure;
  int pin_id; /** 0 -- 64 **/
  double pin_gain;
  double pin_gain_error;
  double pin_offset;
  double pin_offset_error;
} PIN_GH_TBL;

#define PIN_GHF_ROW_MAX 256
typedef struct {
  int nrow;
  PIN_GH_TBL data[PIN_GHF_ROW_MAX];
} PIN_GHF;

#define HXDPINGHF_PRIMARY_HDU_ID  1
#define HXDPINGHF_PINGAIN_HDU_ID  2
int hxdcaldbUtil_pingainhist_open_ASCII (char* pingainhist_ascii_fname);
int hxdcaldbUtil_pingainhist_read_ASCII (PIN_GHF* pin_ghf_data);
int hxdcaldbUtil_pingainhist_close_ASCII(void);

int hxdcaldbUtil_pingainhist_create_FITS(char* pingainhist_fits_fname);
int hxdcaldbUtil_pingainhist_write_FITS(PIN_GHF* pin_ghf_data);

int hxdcaldbUtil_pingainhist_open_FITS(char* pingainhist_fits_fname);
int hxdcaldbUtil_pingainhist_read_FITS(PIN_GHF* pin_ghf_data);
int hxdcaldbUtil_pingainhist_close_FITS(void);

/* ===================================================================
 *             ae_hxd_wampht_YYYYMMDD.fits
 *                  (compression setting by PI Prog)
 * =================================================================== */
#define HXD_TRNTBL_NBIN     7
#define HXD_TRNTBL_PH_NBIN 54
#define HXD_TRNTBL_NTBL_MAX  256
#define HXD_TRNTBL_NKEYWORD  62

typedef struct {
  int table_id;
  int trn_bin[HXD_TRNTBL_NBIN];    /** trn bin setting  **/
  int add_flg[HXD_TRNTBL_PH_NBIN]; /** 1:add, 0: no add **/
} HxdTrnPh;

typedef struct {
  int      ntable;
  HxdTrnPh trnphtbl[HXD_TRNTBL_NTBL_MAX];
} HxdTrnPhTbl;

int hxdcaldbUtil_trnphtbl_open_ASCII (char* trnphtbl_ascii_fname);
int hxdcaldbUtil_trnphtbl_read_ASCII (HxdTrnPhTbl *trnphtbl_data);
int hxdcaldbUtil_trnphtbl_close_ASCII(void);

int hxdcaldbUtil_trnphtbl_create_FITS(char* trnphtbl_fits_fname);
int hxdcaldbUtil_trnphtbl_write_FITS(HxdTrnPhTbl *trnphtbl_data);

int hxdcaldbUtil_trnphtbl_open_FITS(char* trnphtbl_fits_fname);
int hxdcaldbUtil_trnphtbl_read_FITS(HxdTrnPhTbl *trnphtbl_data);
int hxdcaldbUtil_trnphtbl_close_FITS(void);


/* ===================================================================
 *             PIN THRESHOLD 
 *              ae_hxd_pinthr_YYYYMMDD.fits
 * =================================================================== */
#define HXDPINTHRES_PIN_NUM_OF_1UNIT 4
#define HXDPINTHRES_UNIT_NUM 16

#define HXDPINTHRES_PRIMARY_HDU_ID    1
#define HXDPINTHRES_THRESHOLD_HDU_ID  2

int hxdcaldbUtil_pinthres_open_ASCII (char* pinthres_ascii_fname);
int hxdcaldbUtil_pinthres_read_ASCII (double *pinthres_data);
int hxdcaldbUtil_pinthres_close_ASCII(void);

int hxdcaldbUtil_pinthres_create_FITS(char* pinthres_fits_fname);
int hxdcaldbUtil_pinthres_write_FITS(double *pinthres_data);

int hxdcaldbUtil_pinthres_open_FITS(char* pinthres_fits_fname);
int hxdcaldbUtil_pinthres_read_FITS(double *pinthres_data);
int hxdcaldbUtil_pinthres_close_FITS(void);

/* ===================================================================
 *             Database for hxdarfgen
 *              ae_hxd_pinart_YYYYMMDD.fits
 *              ae_hxd_gsoart_YYYYMMDD.fits
 * =================================================================== */
#define HXD_ARFDB_U_N_EBIN_MAX  512
#define HXD_ARFDB_U_N_ANGL_MAX  512

typedef struct {
  /*** 1st Extension ***/
  double angle      [HXD_ARFDB_U_N_ANGL_MAX];              /** arcmin **/
  double specrsp    [HXD_ARFDB_U_N_ANGL_MAX][HXD_ARFDB_U_N_EBIN_MAX];/* arf */
  /*** 2nd Extension ***/
  double energy_low [HXD_ARFDB_U_N_EBIN_MAX];
  double energy_high[HXD_ARFDB_U_N_EBIN_MAX];
  int    energy_bin [HXD_ARFDB_U_N_EBIN_MAX];
  /*** Information ***/
  int n_angle;
  int n_energy_bin;
} HxdArfdbUnit;

#define HXDARFDBUNIT_PRIMARY_EXTENSION   1
#define HXDARFDBUNIT_ARFMATRIX_EXTENSION 2
#define HXDARFDBUNIT_EBOUNDS_EXTENSION   3

int hxdcaldbUtil_arfdbUnit_open_ASCII (char* arfdbUnit_ascii_list_fname,
				       char* arfdbUnit_ascii_edef_fname);
int hxdcaldbUtil_arfdbUnit_read_ASCII (HxdArfdbUnit *arfdbunit_data);
int hxdcaldbUtil_arfdbUnit_close_ASCII(void);

int hxdcaldbUtil_arfdbUnit_create_FITS(char *arfdbunit_fits_fname,
       				       HxdArfdbUnit *arfdbunit_data,
				       char *detname);
int hxdcaldbUtil_arfdbUnit_write_FITS (HxdArfdbUnit *arfdbunit_data,
				       char *detname);
int hxdcaldbUtil_arfdbUnit_close_FITS (void);

int hxdcaldbUtil_arfdbUnit_open_FITS(char *arfdbunit_fits_fname);
int hxdcaldbUtil_arfdbUnit_read_FITS(HxdArfdbUnit *arfdbunit_data);

/* ===================================================================
 *             TelDef File, read only
 *               ae_hxd_teldef_YYYYMMDD.fits
 * =================================================================== */

#define HXDTELDEF_PRIMARY_EXTENSION   1
#define HXDTELDEF_ALIGNMENT_EXTENSION 2
#define HXDTELDEF_ALIGNMENT_NROW_PIN 64
#define HXDTELDEF_ALIGNMENT_NROW_GSO 16
#define HXDTELDEF_ALIGNMENT_NROW  (HXDTELDEF_ALIGNMENT_NROW_PIN+HXDTELDEF_ALIGNMENT_NROW_GSO)

typedef struct {
  /** HXD ALIGNMENT extension **/
  char det_name[HXDTELDEF_ALIGNMENT_NROW][64];
  int  det_id[HXDTELDEF_ALIGNMENT_NROW]; /** PIN: 0 -- 63, GSO: 64-79**/
  float int_x[HXDTELDEF_ALIGNMENT_NROW]; /** arcmin **/
  float int_y[HXDTELDEF_ALIGNMENT_NROW]; /** arcmin **/
  double hxd_offset_x; /** offset angle from the XRS aim point **/
  double hxd_offset_y;
}HxdTelDef;

int hxdcaldbUtil_teldef_open_FITS(char *teldef_fits_fname);
int hxdcaldbUtil_teldef_read_FITS(HxdTelDef *teldef_data);
int hxdcaldbUtil_teldef_close_FITS (void);

/* ===================================================================
 *             ae_hxd_gsopsd_YYYYMMDD.fits
 *             PSD SELECTION
 * =================================================================== */
#define HXDPSDSEL_NROW          8192
#define HXDPSDSEL_UNIT_NUM        16

#define HXDPSDSEL_PRIMARY_HDU_ID   1
#define HXDPSDSEL_CRITERIA_HDU_ID  2

typedef struct {
  int    unit_id;
  double rpi_fast;
  double rpi_slow_center;
  double rpi_slow_1sigma_upper;
  double rpi_slow_1sigma_lower;
} Hxdgrade_Psdsel_Tbl;

typedef struct {
  int n_row;
  char ccnm[71];
  Hxdgrade_Psdsel_Tbl data[HXDPSDSEL_NROW*HXDPSDSEL_UNIT_NUM];
} Hxdgrade_Psdsel;

int hxdcaldbUtil_psdsel_open_ASCII( char *hxdgrade_psdsel_ascii_fname );
int hxdcaldbUtil_psdsel_read_ASCII( Hxdgrade_Psdsel *psdsel_data );
int hxdcaldbUtil_psdsel_close_ASCII( void);

int hxdcaldbUtil_psdsel_create_FITS( char* psdsel_fits_fname );
int hxdcaldbUtil_psdsel_write_FITS( Hxdgrade_Psdsel *psdsel_data );

int hxdcaldbUtil_psdsel_open_FITS( char* psdsel_fits_fname );
int hxdcaldbUtil_psdsel_read_FITS( Hxdgrade_Psdsel *psdsel_data );
int hxdcaldbUtil_psdsel_close_FITS( void );

/* ===================================================================
 *             ae_hxd_bstidt_YYYYMMDD.fits
 *                BST ID Table
 * =================================================================== */
#define HXDBSTIDT_NROW          8192
#define HXDBSTIDT_TPU_N_BOARD     4
#define HXDBSTIDT_MAX_CHAR_LNGTH  64
#define HXDBSTIDT_PRIMARY_HDU_ID  1
#define HXDBSTIDT_IDTABLE_HDU_ID  2

#define HXDBSTIDT_N_EULER 3

#define HXDBSTIDT_UNDEF_TRGID     9999999
#define HXDBSTIDT_UNDEF_YYYYMMDD  20000101
#define HXDBSTIDT_UNDEF_HHMMSS    000000
#define HXDBSTIDT_UNDEF_AETIME    0.000
#define HXDBSYIDT_UNDEF_INTVAL    0xFFFFFF
#define HXDBSYIDT_UNDEF_DBLVAL    99999.999

#define HXDBSTTIME_ASGN_VALID   1
#define HXDBSTTIME_ASGN_INVALID 0
#define TIME_EPSILON 0.0001

typedef struct {
  int trg_id;          /** (w) trigger id **/
  char burst_id[HXDBSTIDT_MAX_CHAR_LNGTH]; /**   (w) burst_id   **/
  char trn_id[HXDBSTIDT_MAX_CHAR_LNGTH];   /**   (w) trn_id     **/
  int yyyymmdd;        /** (w) event date, from FRZD_DATE **/
  int hhmmss;          /** (w) event time, from FRZD_TIME **/
  int classification;  /** (w) event classification **/
  int trg_src;         /** (t) trigger source **/
  int trg_src_soft;    /** (b) trigger source, soft **/
  int frzd_yyyymmdd[HXDBSTIDT_TPU_N_BOARD];  /** (b) frzd time(yyyy/mm/dd) **/
  int frzd_hhmmss[HXDBSTIDT_TPU_N_BOARD];    /** (b) frzd time (hh:mm:ss)  **/
  double frzd_aetime[HXDBSTIDT_TPU_N_BOARD]; /** (b) frzd time (aetime)    **/
  int simul_sa;        /** (w) satellites simultaneous observation **/
  int peak_countrate[HXDBSTIDT_TPU_N_BOARD];  /** (w) raw peak count rate**/
  double euler_angle[HXDBSTIDT_N_EULER];     /** (w) Euler angle of Suzaku **/
  double inci_theta;   /** (w) incident theta **/
  double inci_phi;     /** (w) incident phi   **/
  double inci_ra;      /** (w) incident R.A.  **/
  double inci_dec;     /** (w) incident DEC   **/
  char   inci_origin[HXDBSTIDT_MAX_CHAR_LNGTH]; /** (w) INCI_ORIGIN */
  double sat_alt;      /** (w) Satellite altitude   **/
  double sat_lon;      /** (w) Satellite longitude **/
  double sat_lat;      /** (w) Satellite latitude   **/
  int time_mode[HXDBSTIDT_TPU_N_BOARD];      /** (t) time mode **/
  int frzon_gbread;                          /** (t) GB READ count **/
  double frzon_aetime[HXDBSTIDT_TPU_N_BOARD];/** (t) aetime    @ freeze on **/
  int frzon_ti[HXDBSTIDT_TPU_N_BOARD];       /** (t) TI        @ freeze on **/
  int frzon_t_latch_ti;                      /** (t) T_LATCH_TI@ freeze on **/
  int frzon_acu_t_latch;                     /** (t) T_LATCH   @ freeze on **/
  int frzof_gbread;                          /** (t) GB READ count **/
  double frzof_aetime[HXDBSTIDT_TPU_N_BOARD];/** (t) aetime    @ freeze off **/
  int valid_yyyymmdd;  /** date on final access (yyyy/mm/dd) **/
  int valid_hhmmss;    /** time on final access (hh:mm:ss)   **/
  int  bstid_revision; /** revision of this row (0=created, 1,2,3,...) **/
  /** accessed by t: hxdwambstid b: hxdbstime w: hxdeditbstidt **/
} HxdBstidt_Tbl;

typedef struct {
  int n_row;
  HxdBstidt_Tbl data[HXDBSTIDT_NROW];
} HxdBstidt;

int hxdcaldbUtil_bstidt_open_ASCII ( char *bstidt_ascii_fname );
int hxdcaldbUtil_bstidt_read_ASCII ( HxdBstidt *bstidt_data );
int hxdcaldbUtil_bstidt_close_ASCII( void);

int hxdcaldbUtil_bstidt_create_FITS( char* bstidt_fits_fname );
int hxdcaldbUtil_bstidt_write_row_FITS (int rowid, HxdBstidt_Tbl *bstidt_tbl);
int hxdcaldbUtil_bstidt_write_FITS ( HxdBstidt *bstidt_data );

int hxdcaldbUtil_bstidt_open_FITS ( char* bstidt_fits_fname );
int hxdcaldbUtil_bstidt_read_FITS ( HxdBstidt *bstidt_data );
int hxdcaldbUtil_bstidt_read_row_FITS (int rowid, HxdBstidt_Tbl *bstidt_tbl );
int hxdcaldbUtil_bstidt_close_FITS( void );

int hxdcaldbUtil_bstidt_init_a_bstidtbl(HxdBstidt_Tbl *bstidt);

/** for hxdbsttime **/
int hxdcaldbUtil_bstidt_read_frzonTimeInfo(HxdBstidt *bstidt_data, 
					   int board, double tlm_aetime,
					   double *frzon_aetime, 
					   unsigned int *frzon_telm_time,
					   unsigned int *latch_AE_time,
					   unsigned int *latch_DE_time,
					   int *bsttime_valid,
					   int *bstidt_rowid);
int hxdcaldbUtil_bstidt_update_FRZN_AETIME ( char* bstidt_fits_fname,
					     double *bst_frzd_tm);

/* ===================================================================
 *                    WAM gain history file
 *                    ae_hxd_wamghf_YYYYMMDD.fits
 * =================================================================== */
#define HXDCALDBUTIL_ANTI_N_UNIT 20
typedef struct{
  double start_time [HXDCALDBUTIL_ANTI_N_UNIT];
  double end_time [HXDCALDBUTIL_ANTI_N_UNIT];
  int start_yyyymmdd [HXDCALDBUTIL_ANTI_N_UNIT];
  int start_hhmmss [HXDCALDBUTIL_ANTI_N_UNIT];
  double exposure [HXDCALDBUTIL_ANTI_N_UNIT];
  int model_id  [HXDCALDBUTIL_ANTI_N_UNIT];
  double peak  [HXDCALDBUTIL_ANTI_N_UNIT];
  double peak_err  [HXDCALDBUTIL_ANTI_N_UNIT];
  double sigma  [HXDCALDBUTIL_ANTI_N_UNIT];
  double sigma_err [HXDCALDBUTIL_ANTI_N_UNIT];
  double area [HXDCALDBUTIL_ANTI_N_UNIT];
  double area_err [HXDCALDBUTIL_ANTI_N_UNIT];
  double redu_chisq [HXDCALDBUTIL_ANTI_N_UNIT];
} WAM_GHF_TBL;

#define HXDWAMGHF_N_PARAM 6
#define WAM_GHF_ROW_MAX 16384

typedef struct {
  int nrow [HXDCALDBUTIL_ANTI_N_UNIT];
  WAM_GHF_TBL data[WAM_GHF_ROW_MAX];
} WAM_GHF;

#define HXDWAMGHF_PRIMARY_HDU_ID  1
#define HXDWAMGHF_PMTFIT_HDU_ID_OFFSET 2

int hxdcaldbUtil_wamghf_open_ASCII (char* wamghf_ascii_fname);
int hxdcaldbUtil_wamghf_read_ASCII (WAM_GHF* wamghf_data);
int hxdcaldbUtil_wamghf_close_ASCII(void);

int hxdcaldbUtil_wamghf_create_FITS(char* wamghf_fits_fname);
int hxdcaldbUtil_wamghf_write_row_FITS(int irow, WAM_GHF_TBL* ght_tbl);
int hxdcaldbUtil_wamghf_write_FITS(WAM_GHF* wamghf_data);

int hxdcaldbUtil_wamghf_open_FITS(char* wamghf_fits_fname);
int hxdcaldbUtil_wamghf_read_row_FITS(int irow, WAM_GHF_TBL* ght_tbl);
int hxdcaldbUtil_wamghf_read_FITS(WAM_GHF* wamghf_data);
int hxdcaldbUtil_wamghf_close_FITS(void);
/******************** EOF ********************/
#endif
