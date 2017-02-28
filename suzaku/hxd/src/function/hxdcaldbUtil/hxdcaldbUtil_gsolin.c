/*
 *   hxdcaldbUtil, hxd_gso_lin_xxxx.fits
 *         v0.0.1; 2005-01-14, created by Y.Terada
 *                 define the file format, only support ASCII Files.
 *         v0.0.2; 2005-01-25, 
 *                 change HXDGSOLIN_ADCINL_N_CH by Y.Terada
 *         v0.1.0; 2005-01-28, 
 *                 support fitsio(hxdcaldbUtil_hxdgsolin_adcdnl_read_FITS),
 *                                by Y.Terada, T.Kishishita
 *         v0.1.1; 2005-02-02,
 *                 support fitsio(hxdcaldbUtil_hxdgsolin_adcinl_read_FITS)
 *                                by T.Kishishita
 *         v0.1.4; 2005-02-07,
 *                 delete needless print, by Y.Terada
 *         v0.2.4; 2005-05-17,
 *                 change INSTRUME and DETNAM by Y.Terada
 *                 INSTRUME is fixed to be a value of 'HXD'
 *                 DETNAM is, 'WELL_GSO' 'WELL_PIN' 'WAM_ANTI'
 *         v0.3.2; 2005-0531,
 *                 DEBUG @ hxdcaldbUtil_hxdgsolin_adcinl_read_FITS
 *                 BY S.Hirakuri
 *         v 0.4.0; 2005-06-13 Y.Terada
 *                 GSFC comment
 *         v 0.4.1; 2005-06-25 Y.Terada,  add Primary Header 
 *         v 0.4.3; 2005-09-28 Y.Terada,  add FILENAME
 *         v 0.4.9; 2005-09-28 Y.Terada,  boundary
 *         v 0.6.2; 2006-08-31 Y.Terada
 *                 format changed (as version 1.2.2.3 caldb)
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "hxdcaldbUtil.h"
#include "hxdFitsCommentUtil.h"

static char tool_name[] = "hxdcaldbUtil_gsolin";

static FILE*     hxdgsolin_adcdnl_ascii_fp;
static FILE*     hxdgsolin_adcinl_ascii_fp;
static fitsfile* hxdgsolin_fits_fp;
static fitsfile* hxdgsolin_adcdnl_fits_fp;
static fitsfile* hxdgsolin_adcinl_fits_fp;
static int hxdgsolin_adcdnl_irow;
static int hxdgsolin_adcinl_irow;

/* ========================================
 *   ASCII File I/O
 * ========================================*/
int 
hxdcaldbUtil_hxdgsolin_adcdnl_open_ASCII (char* hxdgsolin_adcdnl_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  hxdgsolin_adcdnl_ascii_fp = fopen(hxdgsolin_adcdnl_fname, "r");
  if (hxdgsolin_adcdnl_ascii_fp == NULL){
    fprintf(stderr, 
	    "hxdcaldbUtil: Cannot open hxdgsolin_adcdnl ASCII file(%s)\n",
	    hxdgsolin_adcdnl_fname);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

int
hxdcaldbUtil_hxdgsolin_adcdnl_read_ASCII (HxdGsoLin_ADCDNL* data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int unit_id, channel;
  int the_unit_id;
  double pha_slow, adc_slow_width, adc_slow_start;
  double pha_fast, adc_fast_width, adc_fast_start;

  if (hxdgsolin_adcdnl_ascii_fp == NULL){
    fprintf(stderr, "hxdcaldbUtil: Invalid file pointer (hxdgsolin_adcdnl)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*  format:
   *         UNIT_ID, (PHA_SLOW, ADC_SLOW_WIDTH, ADC_SLOW_START) x 4096,
   *         (PHA_FAST, ADC_FAST_WIDTH, ADC_FAST_START) x 4096
   */
  for (unit_id=0; unit_id<HXD_WEL_N_UNIT; unit_id++){
    for(channel=0; channel<HXDGSOLIN_ADCDNL_N_CH; channel++){
      fscanf(hxdgsolin_adcdnl_ascii_fp, "%d %lf %lf %lf %lf %lf %lf\n",
	     &the_unit_id, &pha_slow, &adc_slow_width, &adc_slow_start,
	     &pha_fast, &adc_fast_width, &adc_fast_start);
      if( unit_id != the_unit_id ){
	fprintf(stderr, 
		"hxdcaldbUtil; Err hxdgsolin ascii format (unit=%d, ch=%d)\n",
		unit_id, channel);
	fprintf(stderr,"hxdcaldbUtil: %d %f %f %f %f %f %f\n",
		the_unit_id, pha_slow, adc_slow_width, adc_slow_start,
		pha_fast, adc_fast_width, adc_fast_start);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      data->pha_slow[unit_id][channel]       = pha_slow;
      data->adc_slow_width[unit_id][channel] = adc_slow_width;
      data->adc_slow_start[unit_id][channel] = adc_slow_start;
      data->pha_fast[unit_id][channel]       = pha_fast;
      data->adc_fast_width[unit_id][channel] = adc_fast_width;
      data->adc_fast_start[unit_id][channel] = adc_fast_start;
    }
  }
  return status;
}

int
hxdcaldbUtil_hxdgsolin_adcdnl_close_ASCII( void ){
  int status = HXDCALDBUTIL_STATUS_OK;
  if (fclose(hxdgsolin_adcdnl_ascii_fp) ){
    fprintf(stderr, 
	    "hxdcaldbUtil: hxdgsolin_adcdnl_ascii close failed\n");
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}


int
hxdcaldbUtil_hxdgsolin_adcinl_open_ASCII (char* hxdgsolin_adcinl_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  hxdgsolin_adcinl_ascii_fp = fopen(hxdgsolin_adcinl_fname, "r");
  if (hxdgsolin_adcinl_ascii_fp == NULL){
    fprintf(stderr, 
	    "hxdcaldbUtil: Cannot open hxdgsolin_adcinl ASCII file(%s)\n",
	    hxdgsolin_adcinl_fname);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

int
hxdcaldbUtil_hxdgsolin_adcinl_read_ASCII (HxdGsoLin_ADCINL* data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int unit_id, channel;
  int the_unit_id;
  double adc_pi_slow, ae_pi_slow, adc_pi_fast, ae_pi_fast; 

  if (hxdgsolin_adcinl_ascii_fp == NULL){
    fprintf(stderr, "hxdcaldbUtil: Invalid file pointer(hxdgsolin_adcinl)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*  format
   *          UNIT_ID, (ADC_PI_SLOW, AE_PI_SLOW) x 4096?,
   *         (ADC_PI_FAST, AE_PI_FAST) x 4096?
   */

  for (unit_id=0; unit_id<HXD_WEL_N_UNIT; unit_id++){
    for(channel=0; channel<HXDGSOLIN_ADCINL_N_CH; channel++){
      fscanf(hxdgsolin_adcinl_ascii_fp, "%d %lf %lf %lf %lf\n",&the_unit_id,
	     &adc_pi_slow, &ae_pi_slow, &adc_pi_fast, &ae_pi_fast);
      if( unit_id != the_unit_id ){
	fprintf(stderr, 
		"hxdcaldbUtil; Err hxdgsolin ascii format (unit=%d, ch=%d)\n",
		unit_id, channel);
	fprintf(stderr,"hxdcaldbUtil: %d %f %f %f %f\n",the_unit_id,
		adc_pi_slow, ae_pi_slow, adc_pi_fast, ae_pi_fast);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      data->adc_pi_slow[unit_id][channel] = adc_pi_slow;
      data->ae_pi_slow[unit_id][channel]  = ae_pi_slow;
      data->adc_pi_fast[unit_id][channel] = adc_pi_fast;
      data->ae_pi_fast[unit_id][channel]  = ae_pi_fast; 
    }
  }
  return status;
}

int
hxdcaldbUtil_hxdgsolin_adcinl_close_ASCII(void){
  int status = HXDCALDBUTIL_STATUS_OK;
  if (fclose(hxdgsolin_adcinl_ascii_fp) ){
    fprintf(stderr, 
	    "hxdcaldbUtil: hxdgsolin_adcinl_ascii close failed\n");
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

/* ========================================
 *   Fits File Write
 * ========================================*/
int hxdcaldbUtil_hxdgsolin_create_FITS(char* hxdgsolin_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
  int bitpix = 8;
  int naxis = 0;
  long nrow=0;
  char detname[16] = "WELL_GSO";

  if (fits_create_file(&hxdgsolin_fits_fp, hxdgsolin_fname, &istat)) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n", tool_name, 
	    hxdgsolin_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if(fits_create_img(hxdgsolin_fits_fp, bitpix, naxis, &nrow, 
		     &istat)) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (hxdcaldbUtil_hxdgsolin_adcdnl_create_FITS(hxdgsolin_fname) 
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s:adcdnl_create_FITS failed \n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (hxdcaldbUtil_hxdgsolin_adcinl_create_FITS(hxdgsolin_fname) 
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s:adcinl_create_FITS failed \n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;

}

int hxdcaldbUtil_hxdgsolin_adcdnl_create_FITS( char* hxdgsolin_mother_fname ){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  char hxdgsolin_adcdnl_fits_name[256] = "hxdgsolin_adcdnl_tmp.fits";
#define HXDGSOLIN_ADCDNL_NKEYWORD 7
  static char *ttype[HXDGSOLIN_ADCDNL_NKEYWORD] = {
    "UNIT_ID",
    "PHA_SLOW", "ADC_SLOW_WIDTH", "ADC_SLOW_START",
    "PHA_FAST", "ADC_FAST_WIDTH", "ADC_FAST_START"
  };

  static char *tform[HXDGSOLIN_ADCDNL_NKEYWORD] = {
    "1B",
    "1I", "1D", "1D",
    "1I", "1D", "1D"
  };

  static char *tunit[HXDGSOLIN_ADCDNL_NKEYWORD] = {
    "",
    "chan", "chan", "chan",
    "chan", "chan", "chan"
  };

  /*
  static char *minmax_card[] = {
    "TLMIN2  =                    0 / minimum legal value for TABLE_ID",
    "TLMAX2  =                 4096 / maximum legal value for TABLE_ID",
    "TLMIN5  =                    0 / minimum legal value for TABLE_ID",
    "TLMAX5  =                 4096 / maximum legal value for TABLE_ID"
  }
  */

  char *keyword[HXDGSOLIN_ADCDNL_NKEYWORD]={
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  ", "TTYPE6  ",
    "TTYPE7  "
  };

  char *comment[HXDGSOLIN_ADCDNL_NKEYWORD]={
    "Well unit ID from 0 to 15",
    "Slow PHA channel", 
    "WPU Slow ADC channel width", 
    "WPU Slow ADC start channel", 
    "Fast PHA channel", 
    "WPU Fast ADC channel width", 
    "WPU Fast ADC start channel"
  };

  char detname[16] = "WELL_GSO";
  char code_name[16] = "ADCDNL";
  char extention_name[]="ADCDNL";
  char description[81] = "GSO differential non-linearity correction table (for hxdpi)";

  int bitpix = 8;
  int naxis = 0;
  long nrow=0;
  int ncol = sizeof(ttype)/sizeof(*ttype);
  int use_boundary = 0;
  char *boundary = "";

  if (fits_create_file(&hxdgsolin_adcdnl_fits_fp, hxdgsolin_adcdnl_fits_name,
		       &istat)) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n", tool_name, 
	    hxdgsolin_adcdnl_fits_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if(fits_create_img(hxdgsolin_adcdnl_fits_fp, bitpix, naxis, &nrow, 
		     &istat)) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if( fits_create_tbl( hxdgsolin_adcdnl_fits_fp, BINARY_TBL, nrow, ncol,
                       ttype, tform, tunit, extention_name, &istat) ){
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (hxdcaldbUtil_write_fits_header(hxdgsolin_adcdnl_fits_fp, 
				     detname, code_name, description,
				     hxdgsolin_mother_fname,
				     use_boundary, boundary)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxdFitsComment_write(hxdgsolin_adcdnl_fits_fp, HXDGSOLIN_ADCDNL_NKEYWORD,
		       keyword, comment, &istat);
  if (istat){
    fprintf(stderr, "%s: fits write comment failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if(fits_flush_file(hxdgsolin_adcdnl_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxdgsolin_adcdnl_irow = 0;
  return status;

}


int hxdcaldbUtil_hxdgsolin_adcinl_create_FITS( char* hxdgsolin_mother_fname ){
  int istat  = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  char hxdgsolin_adcinl_fits_name[256] = "hxdgsolin_adcinl_tmp.fits";
#define HXDGSOLIN_ADCINL_NKEYWORD 5
  static char *ttype[HXDGSOLIN_ADCINL_NKEYWORD] = {
    "UNIT_ID",
    "ADC_PI_SLOW", "AE_PI_SLOW",
    "ADC_PI_FAST", "AE_PI_FAST"
  };

  static char *tform[HXDGSOLIN_ADCINL_NKEYWORD] = {
    "1B",
    "1D", "1D",
    "1D", "1D"
  };

  static char *tunit[HXDGSOLIN_ADCINL_NKEYWORD] = {
    "",
    "chan", "chan", 
    "chan", "chan"
  };

  char *keyword[HXDGSOLIN_ADCINL_NKEYWORD]={
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  "
  };

  char *comment[HXDGSOLIN_ADCINL_NKEYWORD]={
    "Well unit ID from 0 to 15",
    "WPU Slow ADC channel", "Slow PI Chennel",
    "WPU Fast ADC channel", "Fast PI Channel"
  };

  char detname[16]   = "WELL_GSO";
  char code_name[16] = "ADCINL";
  char extention_name[]="ADCINL";
  char description[81] = "GSO integrated non-linearity correction table (for hxdpi)";

  int bitpix = 8;
  int naxis = 0;
  long nrow=0;
  int ncol = sizeof(ttype)/sizeof(*ttype);
  int use_boundary = 0;
  char *boundary = "";

  if (fits_create_file(&hxdgsolin_adcinl_fits_fp, hxdgsolin_adcinl_fits_name, 
		       &istat)) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n", tool_name, 
	    hxdgsolin_adcinl_fits_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if( fits_create_tbl( hxdgsolin_adcinl_fits_fp, BINARY_TBL, nrow, ncol,
                       ttype, tform, tunit, extention_name, &istat) ){
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (hxdcaldbUtil_write_fits_header(hxdgsolin_adcinl_fits_fp, 
				     detname, code_name, description,
				     hxdgsolin_mother_fname,
				     use_boundary, boundary)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxdFitsComment_write(hxdgsolin_adcinl_fits_fp, HXDGSOLIN_ADCINL_NKEYWORD,
		       keyword, comment, &istat);
  if (istat){
    fprintf(stderr, "%s: fits write comment failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if(fits_flush_file(hxdgsolin_adcinl_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxdgsolin_adcinl_irow = 0;
  return status;
}


int hxdcaldbUtil_hxdgsolin_adcdnl_write_FITS(HxdGsoLin_ADCDNL* dnl_data){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  int unit, ch;
  unsigned char uchar_unit;
  short short_pha;
  long firstelem = 1;
  long nelements = 1;
  enum{
    GSODNL_DUMMY_C_FORTRUN,
    GSODNL_UNIT_ID,
    GSODNL_PHA_SLOW, GSODNL_ADC_SLOW_WIDTH, GSODNL_ADC_SLOW_START,
    GSODNL_PHA_FAST, GSODNL_ADC_FAST_WIDTH, GSODNL_ADC_FAST_START
  }; /** for column number **/
  int colnum;

  for(unit=0;unit<HXD_WEL_N_UNIT;unit++){
    for(ch=0;ch<HXDGSOLIN_ADCDNL_N_CH;ch++){
      hxdgsolin_adcdnl_irow ++;      

      colnum = GSODNL_UNIT_ID;
      uchar_unit = (unsigned char) unit;
      fits_write_col_byt(hxdgsolin_adcdnl_fits_fp, colnum, 
			 hxdgsolin_adcdnl_irow, firstelem, nelements, 
			 &uchar_unit, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_byt UNIT_ID failed (%d)\n", 
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }

      colnum = GSODNL_PHA_SLOW;
      short_pha = (short) dnl_data->pha_slow[unit][ch];
      fits_write_col_sht(hxdgsolin_adcdnl_fits_fp, colnum, 
			 hxdgsolin_adcdnl_irow, firstelem, nelements, 
			 &short_pha, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_sht PHA_SLOW failed (%d)\n",
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }

      colnum = GSODNL_ADC_SLOW_WIDTH;
      fits_write_col_dbl(hxdgsolin_adcdnl_fits_fp, colnum, 
			 hxdgsolin_adcdnl_irow, firstelem, nelements, 
			 &dnl_data->adc_slow_width[unit][ch], &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_sht ADC_SLOW_WIDTH failed (%d)\n",
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }

      colnum = GSODNL_ADC_SLOW_START;
      fits_write_col_dbl(hxdgsolin_adcdnl_fits_fp, colnum, 
			 hxdgsolin_adcdnl_irow, firstelem, nelements, 
			 &dnl_data->adc_slow_start[unit][ch], &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_sht ADC_SLOW_START failed (%d)\n",
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }

      colnum = GSODNL_PHA_FAST;
      short_pha = (short) dnl_data->pha_fast[unit][ch];
      fits_write_col_sht(hxdgsolin_adcdnl_fits_fp, colnum, 
			 hxdgsolin_adcdnl_irow, firstelem, nelements, 
			 &short_pha, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_sht PHA_FAST failed (%d)\n",
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }

      colnum = GSODNL_ADC_FAST_WIDTH;
      fits_write_col_dbl(hxdgsolin_adcdnl_fits_fp, colnum, 
			 hxdgsolin_adcdnl_irow, firstelem, nelements, 
			 &dnl_data->adc_fast_width[unit][ch], &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_sht ADC_FAST_WIDTH failed (%d)\n",
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      
      colnum = GSODNL_ADC_FAST_START;
      fits_write_col_dbl(hxdgsolin_adcdnl_fits_fp, colnum, 
			 hxdgsolin_adcdnl_irow, firstelem, nelements, 
			 &dnl_data->adc_fast_start[unit][ch], &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_sht ADC_FAST_START failed (%d)\n",
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }

    } /** end of ch **/
  } /** end of unit **/


  if (fits_modify_key_lng(hxdgsolin_adcdnl_fits_fp, "NAXIS2", 
			  hxdgsolin_adcdnl_irow, "&", &istat)){
    fprintf(stderr, "%s:fits_update_key NAXIS2 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if(fits_flush_file(hxdgsolin_adcdnl_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int hxdcaldbUtil_hxdgsolin_adcinl_write_FITS(HxdGsoLin_ADCINL* inl_data){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  int unit, ch;
  unsigned char uchar_unit;
  long firstelem = 1;
  long nelements = 1;
  enum{
    GSOINL_DUMMY_C_FORTRUN,
    GSOINL_UNIT_ID,
    GSOINL_ADC_PI_SLOW, GSOINL_AE_PI_SLOW,
    GSOINL_ADC_PI_FAST, GSOINL_AE_PI_FAST
  };
  int colnum;

  for(unit=0;unit<HXD_WEL_N_UNIT;unit++){
    for(ch=0;ch<HXDGSOLIN_ADCINL_N_CH;ch++){
      hxdgsolin_adcinl_irow ++;      

      colnum = GSOINL_UNIT_ID;
      uchar_unit = (unsigned char) unit;
      fits_write_col_byt(hxdgsolin_adcinl_fits_fp, colnum, 
                         hxdgsolin_adcinl_irow, firstelem, nelements, 
                         &uchar_unit, &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_byt UNIT_ID failed (%d)\n", 
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = GSOINL_ADC_PI_SLOW;
      fits_write_col_dbl(hxdgsolin_adcinl_fits_fp, colnum, 
                         hxdgsolin_adcinl_irow, firstelem, nelements, 
                         &inl_data->adc_pi_slow[unit][ch], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_sht ADC_PI_SLOW failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = GSOINL_AE_PI_SLOW;
      fits_write_col_dbl(hxdgsolin_adcinl_fits_fp, colnum, 
                         hxdgsolin_adcinl_irow, firstelem, nelements, 
                         &inl_data->ae_pi_slow[unit][ch], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_sht AE_PI_SLOW failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = GSOINL_ADC_PI_FAST;
      fits_write_col_dbl(hxdgsolin_adcinl_fits_fp, colnum, 
                         hxdgsolin_adcinl_irow, firstelem, nelements, 
                         &inl_data->adc_pi_fast[unit][ch], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_sht ADC_PI_FAST failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = GSOINL_AE_PI_FAST;
      fits_write_col_dbl(hxdgsolin_adcinl_fits_fp, colnum, 
                         hxdgsolin_adcinl_irow, firstelem, nelements, 
                         &inl_data->ae_pi_fast[unit][ch], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_sht AE_PI_FAST failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

    } /** end of ch **/
  } /** end of unit **/

  if (fits_modify_key_lng(hxdgsolin_adcinl_fits_fp, "NAXIS2", 
                          hxdgsolin_adcinl_irow, "&", &istat)){
    fprintf(stderr, "%s:fits_update_key NAXIS2 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if(fits_flush_file(hxdgsolin_adcinl_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;

}

int hxdcaldbUtil_hxdgsolin_merge_FITS(void){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  int morekeys = 0;
  char detname[16]   = "WELL_GSO"; /** pri herder **/

  /** copy ADC-DNL into hxdgsolin.fits  **/
  if (fits_copy_hdu(hxdgsolin_adcdnl_fits_fp, hxdgsolin_fits_fp, 
		    morekeys, &istat) ){
    fprintf(stderr, "%s:fits_copy_file gso adcdnl failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (fits_delete_file(hxdgsolin_adcdnl_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_delete gso adcdnl failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (fits_write_date(hxdgsolin_fits_fp, &istat)) {
    fprintf(stderr,"%s: fits_write_date failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if (fits_write_chksum(hxdgsolin_fits_fp, &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** copy ADC-INL into hxdgsolin.fits  **/
  if (fits_copy_hdu(hxdgsolin_adcinl_fits_fp, hxdgsolin_fits_fp, 
		    morekeys, &istat) ){
    fprintf(stderr, "%s:fits_copy_hdu gso adcdnl failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (fits_delete_file(hxdgsolin_adcinl_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_delete gso adcinl failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (fits_write_date(hxdgsolin_fits_fp, &istat)) {
    fprintf(stderr,"%s: fits_write_date failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if (fits_write_chksum(hxdgsolin_fits_fp, &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /******** Add Primary Header keywords ********/
  if (hxdcaldbUtil_write_fits_priheader(hxdgsolin_fits_fp, detname)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int hxdcaldbUtil_hxdgsolin_close_FITS(void){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;

  if(fits_close_file(hxdgsolin_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file hxdgsolin.fits failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}


/* ========================================
 *   Fits File Read
 * ========================================*/
int hxdcaldbUtil_hxdgsolin_open_FITS(char* hxdgsolin_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  if (fits_open_file(&hxdgsolin_fits_fp, hxdgsolin_fname, READONLY, &istat)) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
	    hxdgsolin_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int hxdcaldbUtil_hxdgsolin_adcdnl_read_FITS(HxdGsoLin_ADCDNL* dnl_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  enum{
    GSODNL_DUMMY_C_FORTRUN,
    GSODNL_UNIT_ID,
    GSODNL_PHA_SLOW, GSODNL_ADC_SLOW_WIDTH, GSODNL_ADC_SLOW_START,
    GSODNL_PHA_FAST, GSODNL_ADC_FAST_WIDTH, GSODNL_ADC_FAST_START
  }; /** for column number **/
  int colnum;
  long irow;
  long nrow;

  int unit, ch;
  int format_unit, format_ch;
  unsigned char uchar_unit;
  short short_pha;

  long firstelem = 1;
  long nelements = 1;
  char comment[80];
  unsigned char uchar_nulval = 0;
  short         short_nulval = 0;
  double        double_nulval = 0.0;
  int anynul;
  int hdutype;
 
  /*** move to ADCDNL extension ***/
  fits_movabs_hdu(hxdgsolin_fits_fp, HXDGSOLIN_ADCDNL_HDU_ID, &hdutype,
		  &istat);
  if(istat){
    fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
	    tool_name, HXDGSOLIN_ADCDNL_HDU_ID, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** read nrow (number of rows) ***/
  fits_read_key_lng(hxdgsolin_fits_fp, "NAXIS2", &nrow, 
		    comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key NAXIS2 failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** read columns ***/
  for (irow = 1; irow <= nrow; irow ++){
    /**** Unit ID ****/
    colnum = GSODNL_UNIT_ID;
    fits_read_col_byt(hxdgsolin_fits_fp, colnum, 
		      irow, firstelem, nelements, uchar_nulval,
		      &uchar_unit, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt UNIT_ID failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    unit = (int) uchar_unit;

    /**** PHA SLOW ****/
    colnum = GSODNL_PHA_SLOW;
    fits_read_col_sht(hxdgsolin_fits_fp, colnum, 
		      irow, firstelem, nelements, short_nulval,
		      &short_pha, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PHA_SLOW failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    ch = (int) short_pha;
    { /* --- check unit and ch format --- */
      format_unit = (irow-1) / HXDGSOLIN_ADCDNL_N_CH;
      format_ch   = (irow-1) % HXDGSOLIN_ADCDNL_N_CH;
      if (unit != format_unit || ch != format_ch ){
	fprintf(stderr, "%s: Invarid format\n", tool_name);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
    }

    dnl_data->pha_slow[unit][ch] = (double) short_pha;

    /**** SLOW WIDTH ****/
    colnum = GSODNL_ADC_SLOW_WIDTH;
    fits_read_col_dbl(hxdgsolin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &dnl_data->adc_slow_width[unit][ch], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt SLOW WIDTH failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    /**** SLOW START ****/
    colnum = GSODNL_ADC_SLOW_START;
    fits_read_col_dbl(hxdgsolin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &dnl_data->adc_slow_start[unit][ch], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt SLOW START failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    /**** PHA FAST ****/
    colnum = GSODNL_PHA_FAST;
    fits_read_col_sht(hxdgsolin_fits_fp, colnum, 
		      irow, firstelem, nelements, short_nulval,
		      &short_pha, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PHA FAST failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    { /** --- check ch --- **/
      if((int)short_pha != ch){
	fprintf(stderr, "%s: Invarid PHA_FAST\n", tool_name);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
    }
    dnl_data->pha_fast[unit][ch] = (double) short_pha;

    /**** FAST WIDTH ****/
    colnum = GSODNL_ADC_FAST_WIDTH;
    fits_read_col_dbl(hxdgsolin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &dnl_data->adc_fast_width[unit][ch], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt FAST WIDTH failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    
    /**** FAST START ****/
    colnum = GSODNL_ADC_FAST_START;
    fits_read_col_dbl(hxdgsolin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &dnl_data->adc_fast_start[unit][ch], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt FAST START failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

  } /** end of irow **/

  return status;
}

int hxdcaldbUtil_hxdgsolin_adcinl_read_FITS(HxdGsoLin_ADCINL* inl_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  int i,j;    
  enum{
    GSOINL_DUMMY_C_FORTRUN,
      GSOINL_UNIT_ID,
      GSOINL_ADC_PI_SLOW, GSOINL_AE_PI_SLOW,
      GSOINL_ADC_PI_FAST, GSOINL_AE_PI_FAST
      }; /** for column number **/
  int colnum;
  long irow;
  long nrow;
  int unit;
  int ch[HXD_WEL_N_UNIT] = {0};
  int format_unit, format_ch;
  unsigned char uchar_unit;
  short short_pha;
  long firstelem = 1;
  long nelements = 1;
  char comment[80];
  
  unsigned char uchar_nulval = 0;
  double        double_nulval = 0.0;
  
  int anynul;
  int hdutype;
  /*** move to ADCINL extension ***/
  fits_movabs_hdu(hxdgsolin_fits_fp, HXDGSOLIN_ADCINL_HDU_ID , &hdutype,
		  &istat);
  if(istat){
    fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
	    tool_name, HXDPINLIN_ADCINL_HDU_ID, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } 
  /* fprintf(stderr,"move to ADCINL extension\n"); */

  /*** read nrow (number of rows) ***/
  fits_read_key_lng(hxdgsolin_fits_fp, "NAXIS2", &nrow, 
		    comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key NAXIS2 failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } 
  /*** read columns ***/
  for (irow = 1; irow <= nrow; irow ++){

    /**** Unit ID ****/
    
    colnum = GSOINL_UNIT_ID;
    fits_read_col_byt(hxdgsolin_fits_fp, colnum, 
		      irow, firstelem, nelements, uchar_nulval,
		      &uchar_unit, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt UNIT_ID failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    unit = (int) uchar_unit;


    /** SLOW PHA **/
    colnum = GSOINL_ADC_PI_SLOW;
    fits_read_col_dbl(hxdgsolin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &inl_data->adc_pi_slow[unit][ch[unit]], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl ADC_PI_SLOW failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = GSOINL_AE_PI_SLOW;
    fits_read_col_dbl(hxdgsolin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &inl_data->ae_pi_slow[unit][ch[unit]], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl AE_PI_SLOW failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }


    /** FAST PHA **/
    colnum = GSOINL_ADC_PI_FAST;
    fits_read_col_dbl(hxdgsolin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &inl_data->adc_pi_fast[unit][ch[unit]], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl ADC_PI_FAST failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    colnum = GSOINL_AE_PI_FAST;
    fits_read_col_dbl(hxdgsolin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &inl_data->ae_pi_fast[unit][ch[unit]], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl AE_PI_FAST failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    ch[unit] ++;   
  } /** end of irow **/


  /*fprintf(stdout,"hxdcaldbUtil_hxdgsolin_adcinl_read_FITS done ...\n"); */
  return status;
}
