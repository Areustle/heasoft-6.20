/*
 *   hxdcaldbUtil, hxd_pin_lin_xxxx.fits
 *         v0.0.1; 2005-01-14, created by Y.Terada
 *                 define the file format, only support ASCII Files.
 *         v0.1.1; 2005-01-28, 
 *                 support fitsio, by Y.Terada, T.Kishishita
 *         v0.2.4; 2005-05-17,
 *                 change INSTRUME and DETNAM by Y.Terada
 *                 INSTRUME is fixed to be a value of 'HXD'
 *                 DETNAM is, 'WELL_GSO' 'WELL_PIN' 'WAM_ANTI'
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

static char tool_name[] = "hxdcaldbUtil_pinlin";

static FILE*     hxdpinlin_adcinl_ascii_fp;
static FILE*     hxdpinlin_gain_ascii_fp;
static fitsfile* hxdpinlin_fits_fp;
static fitsfile* hxdpinlin_adcinl_fits_fp;
static fitsfile* hxdpinlin_gain_fits_fp;
static int hxdpinlin_adcinl_irow;
static int hxdpinlin_gain_irow;

/* ========================================
 *   ASCII File I/O
 * ========================================*/
int
hxdcaldbUtil_hxdpinlin_adcinl_open_ASCII (char* hxdpinlin_adcinl_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  hxdpinlin_adcinl_ascii_fp = fopen(hxdpinlin_adcinl_fname, "r");
  if (hxdpinlin_adcinl_ascii_fp == NULL){
    fprintf(stderr, 
	    "hxdcaldbUtil: Cannot open hxdpinlin_adcinl ASCII file(%s)\n",
	    hxdpinlin_adcinl_fname);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

int
hxdcaldbUtil_hxdpinlin_adcinl_read_ASCII (HxdPinLin_ADCINL* data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int unit_id, channel;
  int the_unit_id;
  double pha_pin0,ae_pi_pin0,pha_pin1,ae_pi_pin1;
  double pha_pin2,ae_pi_pin2,pha_pin3,ae_pi_pin3;

  if (hxdpinlin_adcinl_ascii_fp == NULL){
    fprintf(stderr, "hxdcaldbUtil: Invalid file pointer(hxdpinlin_adcinl)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*  format
   *        UNIT_ID, (PHA_PIN0, AE_PI_PIN0) x 256,
   *        (PHA_PIN1, AE_PI_PIN1) x 256, (PHA_PIN2, AE_PI_PIN2) x 256,
   *         (PHA_PIN3, AE_PI_PIN3) x 256
   */

  for (unit_id=0; unit_id<HXD_WEL_N_UNIT; unit_id++){
    for(channel=0; channel<HXDPINLIN_ADCINL_N_CH; channel++){
      fscanf(hxdpinlin_adcinl_ascii_fp, "%d %lf %lf %lf %lf %lf %lf %lf %lf\n",
	     &the_unit_id,&pha_pin0,&ae_pi_pin0,&pha_pin1,&ae_pi_pin1,
	     &pha_pin2,&ae_pi_pin2,&pha_pin3,&ae_pi_pin3);
      if( unit_id != the_unit_id ){
	fprintf(stderr, 
		"hxdcaldbUtil; Err hxdpinlin ascii format (unit=%d, ch=%d)\n",
		unit_id, channel);
	fprintf(stderr,"hxdcaldbUtil: %d %f %f %f %f %f %f %f %f\n",
		the_unit_id,pha_pin0,ae_pi_pin0,pha_pin1,ae_pi_pin1,
		pha_pin2,ae_pi_pin2,pha_pin3,ae_pi_pin3);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      data->pha_pin0[unit_id][channel]    = pha_pin0; 
      data->ae_pi_pin0[unit_id][channel]  = ae_pi_pin0; 
      data->pha_pin1[unit_id][channel]    = pha_pin1; 
      data->ae_pi_pin1[unit_id][channel]  = ae_pi_pin1; 
      data->pha_pin2[unit_id][channel]    = pha_pin2; 
      data->ae_pi_pin2[unit_id][channel]  = ae_pi_pin2; 
      data->pha_pin3[unit_id][channel]    = pha_pin3; 
      data->ae_pi_pin3[unit_id][channel]  = ae_pi_pin3; 
    }
  }
  return status;
}

int
hxdcaldbUtil_hxdpinlin_adcinl_close_ASCII(void){
  int status = HXDCALDBUTIL_STATUS_OK;
  if (fclose(hxdpinlin_adcinl_ascii_fp) ){
    fprintf(stderr, 
	    "hxdcaldbUtil: hxdpinlin_adcinl_ascii close failed\n");
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

/* -------------- ------------- */
int 
hxdcaldbUtil_hxdpinlin_gain_open_ASCII (char* hxdpinlin_gain_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  hxdpinlin_gain_ascii_fp = fopen(hxdpinlin_gain_fname, "r");
  if (hxdpinlin_gain_ascii_fp == NULL){
    fprintf(stderr, 
	    "hxdcaldbUtil: Cannot open hxdpinlin_gain ASCII file(%s)\n",
	    hxdpinlin_gain_fname);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

int
hxdcaldbUtil_hxdpinlin_gain_read_ASCII (HxdPinLin_GAIN* data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int unit_id;
  int the_unit_id;
  double pin0_gain, pin0_offset, pin1_gain, pin1_offset;
  double pin2_gain, pin2_offset, pin3_gain, pin3_offset;

  if (hxdpinlin_gain_ascii_fp == NULL){
    fprintf(stderr, "hxdcaldbUtil: Invalid file pointer (hxdpinlin_gain)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*  format:
   *         UNIT_ID, (PHA_SLOW, ADC_SLOW_WIDTH, ADC_SLOW_START) x 4096,
   *         (PHA_FAST, ADC_FAST_WIDTH, ADC_FAST_START) x 4096
   */
  for (unit_id=0; unit_id<HXD_WEL_N_UNIT; unit_id++){
      fscanf(hxdpinlin_gain_ascii_fp, "%d %lf %lf %lf %lf %lf %lf %lf %lf\n",
	     &the_unit_id, &pin0_gain, &pin0_offset, &pin1_gain, &pin1_offset,
	     &pin2_gain, &pin2_offset, &pin3_gain, &pin3_offset);
      if( unit_id != the_unit_id ){
	fprintf(stderr, 
		"hxdcaldbUtil; Err hxdpinlin ascii format (unit=%d)\n",
		unit_id);
	fprintf(stderr,"hxdcaldbUtil: %d %f %f %f %f %f %f %f %f\n",
		the_unit_id, pin0_gain, pin0_offset, pin1_gain, pin1_offset,
		pin2_gain, pin2_offset, pin3_gain, pin3_offset);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
      data->pin0_gain[unit_id]       = pin0_gain;
      data->pin0_offset[unit_id]     = pin0_offset;
      data->pin1_gain[unit_id]       = pin1_gain;
      data->pin1_offset[unit_id]     = pin1_offset;
      data->pin2_gain[unit_id]       = pin2_gain;
      data->pin2_offset[unit_id]     = pin2_offset;
      data->pin3_gain[unit_id]       = pin3_gain;
      data->pin3_offset[unit_id]     = pin3_offset;
    }
  return status;
}

int
hxdcaldbUtil_hxdpinlin_gain_close_ASCII( void ){
  int status = HXDCALDBUTIL_STATUS_OK;
  if (fclose(hxdpinlin_gain_ascii_fp) ){
    fprintf(stderr, 
	    "hxdcaldbUtil: hxdpinlin_gain_ascii close failed\n");
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

/* ========================================
 *   Fits File Write
 * ========================================*/
int 
hxdcaldbUtil_hxdpinlin_create_FITS(char* hxdpinlin_fname){

  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
  int bitpix = 8;
  int naxis = 0;
  long nrow=0;
  char detname[16] = "WELL_PIN";

  if (fits_create_file(&hxdpinlin_fits_fp, hxdpinlin_fname, &istat)) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n", tool_name, 
	    hxdpinlin_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } else 
    fprintf(stderr,"ok\n");

  if(fits_create_img(hxdpinlin_fits_fp, bitpix, naxis, &nrow, 
		     &istat)) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } else
    fprintf(stderr,"ok\n");

  if (hxdcaldbUtil_hxdpinlin_adcinl_create_FITS(hxdpinlin_fname) 
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s:adcINL_create_FITS failed \n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } else
    fprintf(stderr,"ok\n");

  if (hxdcaldbUtil_hxdpinlin_gain_create_FITS(hxdpinlin_fname) 
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s:adcGAIN_create_FITS failed \n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } else
    fprintf(stderr,"ok\n");
  return status;
}

int hxdcaldbUtil_hxdpinlin_adcinl_create_FITS( char* hxdpinlin_mother_fname ){
  int istat  = 0;
  int status = HXDCALDBUTIL_STATUS_OK; 
  int bitpix = 8;
  int naxis = 0;
  long nrow=0;
  char hxdpinlin_adcinl_fits_name[256] = "hxdpinlin_adcinl_tmp.fits";
  char detname[16] = "WELL_PIN";
  char code_name[16] = "ADCINL";
  char extention_name[]="ADCINL";
  char description[81] = "Integrated Non Linearity for PIN ADC.";

#define HXDPINLIN_ADCINL_NKEYWORD 9

  static char *ttype[HXDPINLIN_ADCINL_NKEYWORD] = {
    "UNIT_ID",
    "PHA_PIN0", "AE_PI_PIN0", "PHA_PIN1", "AE_PI_PIN1",
    "PHA_PIN2", "AE_PI_PIN2", "PHA_PIN3", "AE_PI_PIN3"
  };
  static char *tform[HXDPINLIN_ADCINL_NKEYWORD] = {
    "1B",
    "1I", "1D", "1I", "1D",
    "1I", "1D", "1I", "1D"
  };
  static char *tunit[HXDPINLIN_ADCINL_NKEYWORD] = {
    "",
    "chan", "chan", "chan", "chan",
    "chan", "chan", "chan", "chan"
  };
  char *keyword[HXDPINLIN_ADCINL_NKEYWORD]={
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  ", "TTYPE6  ",
    "TTYPE7  ", "TTYPE8  ","TTYPE9  ",
  };
  char *comment[HXDPINLIN_ADCINL_NKEYWORD]={
    "Well unit ID from 0 to 15",
    "PIN0 PHA channel", "PIN0 PI",
    "PIN1 PHA channel", "PIN1 PI",
    "PIN2 PHA channel", "PIN2 PI",
    "PIN3 PHA channel", "PIN3 PI"
  };
  int ncol = sizeof(ttype)/sizeof(*ttype);
  int use_boundary = 0;
  char *boundary = "";

  if (fits_create_file(&hxdpinlin_adcinl_fits_fp, hxdpinlin_adcinl_fits_name,
		       &istat)) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n", tool_name, 
	    hxdpinlin_adcinl_fits_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  else 
    fprintf(stderr,"create INL FITS file is succeeded.\n");
 

  if(fits_create_img(hxdpinlin_adcinl_fits_fp, bitpix, naxis, &nrow, 
		     &istat)) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  else
    fprintf(stderr, "create INL FITS Image is succeeded.\n");

  if( fits_create_tbl(hxdpinlin_adcinl_fits_fp, BINARY_TBL, nrow, ncol,
                       ttype, tform, tunit, extention_name, &istat) ){
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  else
    fprintf(stderr, "create INL FITS table is succeeded.\n");

  if (hxdcaldbUtil_write_fits_header(hxdpinlin_adcinl_fits_fp, 
				     detname, code_name, description,
				     hxdpinlin_mother_fname,
				     use_boundary, boundary)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  else
    fprintf(stderr, "write INL FITS header is succeeded.\n");

  hxdFitsComment_write(hxdpinlin_adcinl_fits_fp,HXDPINLIN_ADCINL_NKEYWORD,
		       keyword, comment, &istat);
  if (istat){
    fprintf(stderr, "%s: fits write comment failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  else
    fprintf(stderr, "write INL FITS comment is succeeded.\n");


  if(fits_flush_file(hxdpinlin_adcinl_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  else
    fprintf(stderr, "flush INL FITS file is succeeded.\n");


  hxdpinlin_adcinl_irow = 0;
  return status;

}

int hxdcaldbUtil_hxdpinlin_gain_create_FITS( char* hxdpinlin_mother_fname){
  int istat  = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  char hxdpinlin_gain_fits_name[256] = "hxdpinlin_gain_tmp.fits";
#define HXDPINLIN_GAIN_NKEYWORD 9

  static char *ttype[HXDPINLIN_GAIN_NKEYWORD] = {
    "UNIT_ID",
    "PIN0_GAIN", "PIN0_OFFSET",
    "PIN1_GAIN", "PIN1_OFFSET",
    "PIN2_GAIN", "PIN2_OFFSET",
    "PIN3_GAIN", "PIN3_OFFSET"
  };
  static char *tform[HXDPINLIN_GAIN_NKEYWORD] = {
    "1B",
    "1D", "1D",
    "1D", "1D",
    "1D", "1D",
    "1D", "1D"
  };
  static char *tunit[HXDPINLIN_GAIN_NKEYWORD] = {
    "",
    "", "", 
    "", "",
    "", "",
    "", ""
  };
  char *keyword[HXDPINLIN_GAIN_NKEYWORD]={
    "TTYPE1  ", 
    "TTYPE2  ", "TTYPE3  ", 
    "TTYPE4  ", "TTYPE5  ",
    "TTYPE6  ", "TTYPE7  ",
    "TTYPE8  ", "TTYPE9  "
  };
  char *comment[HXDPINLIN_GAIN_NKEYWORD]={
    "Well unit ID from 0 to 15",
    "WPU PIN0 channel gain", "WPU PIN0 channel offset",
    "WPU PIN1 channel gain", "WPU PIN1 channel offset",
    "WPU PIN2 channel gain", "WPU PIN2 channel offset",
    "WPU PIN3 channel gain", "WPU PIN3 channel offset"
  };

  char detname[16]   = "WELL_PIN";
  char code_name[16] = "GAIN";
  char extention_name[]="GAIN";
  char description[81] = "Gain factor for each PIN diode.";
  int bitpix = 8;
  int naxis = 0;
  long nrow=0;
  int ncol = sizeof(ttype)/sizeof(*ttype);
  int use_boundary = 0;
  char *boundary = "";

  if (fits_create_file(&hxdpinlin_gain_fits_fp, hxdpinlin_gain_fits_name, 
		       &istat)) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n", tool_name, 
	    hxdpinlin_gain_fits_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  if( fits_create_tbl( hxdpinlin_gain_fits_fp, BINARY_TBL, nrow, ncol,
                       ttype, tform, tunit, extention_name, &istat) ){
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  if (hxdcaldbUtil_write_fits_header(hxdpinlin_gain_fits_fp, 
				     detname, code_name, description,
				     hxdpinlin_mother_fname,
				     use_boundary, boundary)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  hxdFitsComment_write(hxdpinlin_gain_fits_fp, HXDPINLIN_GAIN_NKEYWORD,
		       keyword, comment, &istat);
  if (istat){
    fprintf(stderr, "%s: fits write comment failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  if(fits_flush_file(hxdpinlin_gain_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  hxdpinlin_gain_irow = 0;
  return status;
}

int hxdcaldbUtil_hxdpinlin_adcinl_write_FITS(HxdPinLin_ADCINL* inl_data){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  int unit, ch;
  int colnum;
  unsigned char uchar_unit;
  long firstelem = 1;
  long nelements = 1;
  short short_pha;
  enum{
    PININL_DUMMY_C_FORTRUN,
      PININL_UNIT_ID,
      PININL_PHA_PIN0, PININL_AE_PI_PIN0,
      PININL_PHA_PIN1, PININL_AE_PI_PIN1,
      PININL_PHA_PIN2, PININL_AE_PI_PIN2,
      PININL_PHA_PIN3, PININL_AE_PI_PIN3
      }; /** for column number **/
  
  for(unit=0;unit<HXD_WEL_N_UNIT;unit++){
    for(ch=0;ch<HXDPINLIN_ADCINL_N_CH;ch++){
      hxdpinlin_adcinl_irow ++;      

      colnum = PININL_UNIT_ID;
      uchar_unit = (unsigned char) unit;

      fits_write_col_byt(hxdpinlin_adcinl_fits_fp, colnum, 
                         hxdpinlin_adcinl_irow, firstelem, nelements, 
                         &uchar_unit, &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_byt UNIT_ID failed (%d)\n", 
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = PININL_PHA_PIN0;
      short_pha = (short) inl_data->pha_pin0[unit][ch];
      fits_write_col_sht(hxdpinlin_adcinl_fits_fp, colnum, 
                         hxdpinlin_adcinl_irow, firstelem, nelements, 
                         &short_pha, &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_sht PIN0_PHA failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = PININL_AE_PI_PIN0;
      fits_write_col_dbl(hxdpinlin_adcinl_fits_fp, colnum, 
                         hxdpinlin_adcinl_irow, firstelem, nelements, 
                         &inl_data->ae_pi_pin0[unit][ch], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_dbl PIN0_AE_PI failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = PININL_PHA_PIN1;
      short_pha = (short) inl_data->pha_pin1[unit][ch];
      fits_write_col_sht(hxdpinlin_adcinl_fits_fp, colnum, 
                         hxdpinlin_adcinl_irow, firstelem, nelements, 
                         &short_pha, &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_sht PIN1_PHA failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = PININL_AE_PI_PIN1;
      fits_write_col_dbl(hxdpinlin_adcinl_fits_fp, colnum, 
                         hxdpinlin_adcinl_irow, firstelem, nelements, 
                         &inl_data->ae_pi_pin1[unit][ch], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_dbl PIN1_AE_PI failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = PININL_PHA_PIN2;
      short_pha = (short) inl_data->pha_pin2[unit][ch];
      fits_write_col_sht(hxdpinlin_adcinl_fits_fp, colnum, 
                         hxdpinlin_adcinl_irow, firstelem, nelements, 
                         &short_pha, &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_sht PIN2_PHA failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = PININL_AE_PI_PIN2;
      fits_write_col_dbl(hxdpinlin_adcinl_fits_fp, colnum, 
                         hxdpinlin_adcinl_irow, firstelem, nelements, 
                         &inl_data->ae_pi_pin2[unit][ch], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_dbl PIN2_AE_PI failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = PININL_PHA_PIN3;
      short_pha = (short) inl_data->pha_pin3[unit][ch];
      fits_write_col_sht(hxdpinlin_adcinl_fits_fp, colnum, 
                         hxdpinlin_adcinl_irow, firstelem, nelements, 
                         &short_pha, &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_sht PIN3_PHA failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = PININL_AE_PI_PIN3;
      fits_write_col_dbl(hxdpinlin_adcinl_fits_fp, colnum, 
                         hxdpinlin_adcinl_irow, firstelem, nelements, 
                         &inl_data->ae_pi_pin3[unit][ch], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_dbl PIN3_AE_PI failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }
    } /** end of ch **/
  } /** end of unit **/
  if (fits_modify_key_lng(hxdpinlin_adcinl_fits_fp, "NAXIS2", 
                          hxdpinlin_adcinl_irow, "&", &istat)){
    fprintf(stderr, "%s:fits_update_key NAXIS2 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  if(fits_flush_file(hxdpinlin_adcinl_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
   return status;
}


int hxdcaldbUtil_hxdpinlin_gain_write_FITS(HxdPinLin_GAIN* gain_data){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  int unit, ch;
  int colnum;
  unsigned char uchar_unit;
  long firstelem = 1;
  long nelements = 1;
  short short_pha;
  enum{
    PININL_DUMMY_C_FORTRUN,
      PIN_UNIT_ID,
      GAIN_PIN0, OFFSET_PIN0,
      GAIN_PIN1, OFFSET_PIN1,
      GAIN_PIN2, OFFSET_PIN2,
      GAIN_PIN3, OFFSET_PIN3
      }; /** for column number **/

  for(unit=0;unit<HXD_WEL_N_UNIT;unit++){
    for(ch=0;ch<HXDPINLIN_GAIN_N_CH;ch++){
      hxdpinlin_gain_irow ++;      

      colnum = PIN_UNIT_ID;
      uchar_unit = (unsigned char) unit;
      fits_write_col_byt(hxdpinlin_gain_fits_fp, colnum, 
                         hxdpinlin_gain_irow, firstelem, nelements, 
                         &uchar_unit, &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_byt UNIT_ID failed (%d)\n", 
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = GAIN_PIN0;
      fits_write_col_dbl(hxdpinlin_gain_fits_fp, colnum, 
                         hxdpinlin_gain_irow, firstelem, nelements, 
                         &gain_data->pin0_gain[unit], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_sht PIN0_GAIN failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = OFFSET_PIN0;
      fits_write_col_dbl(hxdpinlin_gain_fits_fp, colnum, 
                         hxdpinlin_gain_irow, firstelem, nelements, 
                         &gain_data->pin0_offset[unit], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_dbl PIN0_OFFSET failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }
      
      colnum = GAIN_PIN1;
      fits_write_col_dbl(hxdpinlin_gain_fits_fp, colnum, 
                         hxdpinlin_gain_irow, firstelem, nelements, 
                         &gain_data->pin1_gain[unit], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_sht PIN1_GAIN failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = OFFSET_PIN1;
      fits_write_col_dbl(hxdpinlin_gain_fits_fp, colnum, 
                         hxdpinlin_gain_irow, firstelem, nelements, 
                         &gain_data->pin1_offset[unit], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_dbl PIN1_OFFSET failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = GAIN_PIN2;
      fits_write_col_dbl(hxdpinlin_gain_fits_fp, colnum, 
                         hxdpinlin_gain_irow, firstelem, nelements, 
                         &gain_data->pin2_gain[unit], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_sht PIN2_GAIN failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = OFFSET_PIN2;
      fits_write_col_dbl(hxdpinlin_gain_fits_fp, colnum, 
                         hxdpinlin_gain_irow, firstelem, nelements, 
                         &gain_data->pin2_offset[unit], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_dbl PIN2_OFFSET failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = GAIN_PIN3;
      fits_write_col_dbl(hxdpinlin_gain_fits_fp, colnum, 
                         hxdpinlin_gain_irow, firstelem, nelements, 
                         &gain_data->pin3_gain[unit], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_sht PIN3_GAIN failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

      colnum = OFFSET_PIN3;
      fits_write_col_dbl(hxdpinlin_gain_fits_fp, colnum, 
                         hxdpinlin_gain_irow, firstelem, nelements, 
                         &gain_data->pin3_offset[unit], &istat);
      if(istat){
        fprintf(stderr, "%s:fits_write_col_dbl PIN3_OFFSET failed (%d)\n",
                tool_name, istat);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }

    } /** end of ch **/
  } /** end of unit **/
  if (fits_modify_key_lng(hxdpinlin_gain_fits_fp, "NAXIS2", 
                          hxdpinlin_gain_irow, "&", &istat)){
    fprintf(stderr, "%s:fits_update_key NAXIS2 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  if(fits_flush_file(hxdpinlin_gain_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  return status;
}

int hxdcaldbUtil_hxdpinlin_merge_FITS(void){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  int morekeys = 0;
  char detname[16] = "WELL_PIN";

  /** copy ADC-INL into hxdpinlin.fits  **/
  if (fits_copy_hdu(hxdpinlin_adcinl_fits_fp, hxdpinlin_fits_fp, 
		    morekeys, &istat) ){
    fprintf(stderr, "%s:fits_copy_file pin adcdnl failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  if (fits_delete_file(hxdpinlin_adcinl_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_delete pin adcdnl failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  if (fits_write_date(hxdpinlin_fits_fp, &istat)) {
    fprintf(stderr,"%s: fits_write_date failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  if (fits_write_chksum(hxdpinlin_fits_fp, &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  /** copy GAIN into hxdpinlin.fits  **/
  if (fits_copy_hdu(hxdpinlin_gain_fits_fp, hxdpinlin_fits_fp, 
		    morekeys, &istat) ){
    fprintf(stderr, "%s:fits_copy_hdu pin adcdnl failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  if (fits_delete_file(hxdpinlin_gain_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_delete pin adcinl failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  if (fits_write_date(hxdpinlin_fits_fp, &istat)) {
    fprintf(stderr,"%s: fits_write_date failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  if (fits_write_chksum(hxdpinlin_fits_fp, &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }


  /******** Add Primary Header keywords ********/
  if (hxdcaldbUtil_write_fits_priheader(hxdpinlin_fits_fp, detname)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int hxdcaldbUtil_hxdpinlin_close_FITS(void){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  
  if(fits_close_file(hxdpinlin_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file hxdpinlin.fits failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  return status;
}

int hxdcaldbUtil_hxdpinlin_open_FITS(char* hxdpinlin_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  if (fits_open_file(&hxdpinlin_fits_fp, hxdpinlin_fname, READONLY, &istat)) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
	    hxdpinlin_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  return status;
}

int hxdcaldbUtil_hxdpinlin_adcinl_read_FITS(HxdPinLin_ADCINL* inl_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  enum{
    PININL_DUMMY_C_FORTRUN,
      PININL_UNIT_ID,
      PININL_PHA_PIN0, PININL_AE_PI_PIN0,
      PININL_PHA_PIN1, PININL_AE_PI_PIN1,
      PININL_PHA_PIN2, PININL_AE_PI_PIN2,
      PININL_PHA_PIN3, PININL_AE_PI_PIN3
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
  /*** move to ADCINL extension ***/
  fits_movabs_hdu(hxdpinlin_fits_fp, HXDPINLIN_ADCINL_HDU_ID, &hdutype,
		  &istat);
  if(istat){
    fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
	    tool_name, HXDPINLIN_ADCINL_HDU_ID, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } else {}
  /*fprintf(stderr,"move to ADCINL extension\n"); */

  /*** read nrow (number of rows) ***/
  fits_read_key_lng(hxdpinlin_fits_fp, "NAXIS2", &nrow, 
		    comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key NAXIS2 failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } else {}
  /*
    fprintf(stderr,"read nrow (number of rows)\n");
    fprintf(stderr,"nrow:%u\n",nrow);
  */

  /*** read columns ***/
  for (irow = 1; irow <= nrow; irow ++){
    /*fprintf(stderr,"irow:%u",irow);*/
    /**** Unit ID ****/
    
    colnum = PININL_UNIT_ID;
    fits_read_col_byt(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, uchar_nulval,
		      &uchar_unit, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt UNIT_ID failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    unit = (int) uchar_unit;

    /**** PIN0 ****/
    colnum = PININL_PHA_PIN0;
    fits_read_col_sht(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, short_nulval,
		      &short_pha, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN0 PHA failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
 
    ch = (int)short_pha;

    inl_data->pha_pin0[unit][ch] = (double) short_pha;
    
    colnum = PININL_AE_PI_PIN0;
    fits_read_col_dbl(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &inl_data->ae_pi_pin0[unit][ch], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN0 PI failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    /**** PIN1 ****/
    colnum = PININL_PHA_PIN1;
    fits_read_col_sht(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, short_nulval,
		      &short_pha, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN0 PHA failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    } 
    inl_data->pha_pin1[unit][ch] = (double) short_pha;
    
    colnum = PININL_AE_PI_PIN1;
    fits_read_col_dbl(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &inl_data->ae_pi_pin1[unit][ch], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN0 PI failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    /**** PIN2 ****/
    colnum = PININL_PHA_PIN2;
    fits_read_col_sht(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, short_nulval,
		      &short_pha, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN0 PHA failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
 
    inl_data->pha_pin2[unit][ch] = (double) short_pha;
    
    colnum = PININL_AE_PI_PIN2;
    fits_read_col_dbl(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &inl_data->ae_pi_pin2[unit][ch], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN0 PI failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    /**** PIN3 ****/
    colnum = PININL_PHA_PIN3;
    fits_read_col_sht(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, short_nulval,
		      &short_pha, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN0 PHA failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    ch = (int) short_pha;
 
    inl_data->pha_pin3[unit][ch] = (double) short_pha;
    
    colnum = PININL_AE_PI_PIN3;
    fits_read_col_dbl(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &inl_data->ae_pi_pin3[unit][ch], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN0 PI failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
  } /** end of irow **/

  /* fprintf(stderr,"hxdcaldbUtil_hxdpinlin_adcinl_read_FITS done ...\n"); */
  return status;
}

int hxdcaldbUtil_hxdpinlin_gain_read_FITS(HxdPinLin_GAIN* gain_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  enum{
    PININL_DUMMY_C_FORTRUN,
      PIN_UNIT_ID,
      GAIN_PIN0, OFFSET_PIN0,
      GAIN_PIN1, OFFSET_PIN1,
      GAIN_PIN2, OFFSET_PIN2,
      GAIN_PIN3, OFFSET_PIN3
      }; /** for column number **/
  int colnum;
  long irow;
  long nrow;
  int unit, ch;
  unsigned char uchar_unit;

  long firstelem = 1;
  long nelements = 1;
  double        double_nulval = 0.0;
  unsigned char uchar_nulval = 0;
  int anynul;
  int hdutype;
  char comment[256];
  /*** move to ADCINL extension ***/
  fits_movabs_hdu(hxdpinlin_fits_fp, HXDPINLIN_GAIN_HDU_ID, &hdutype,
		  &istat);
  if(istat){
    fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
	    tool_name, HXDPINLIN_GAIN_HDU_ID, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** read nrow (number of rows) ***/
  fits_read_key_lng(hxdpinlin_fits_fp, "NAXIS2", &nrow, 
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
    
    colnum = PIN_UNIT_ID;
    fits_read_col_byt(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, uchar_nulval,
		      &uchar_unit, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt UNIT_ID failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    unit = (int) uchar_unit;

    colnum = GAIN_PIN0;
    fits_read_col_dbl(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &gain_data->pin0_gain[unit], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN0 GAIN failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
 
    colnum = OFFSET_PIN0;
    fits_read_col_dbl(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &gain_data->pin0_offset[unit], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN0 OFFSET failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = GAIN_PIN1;
    fits_read_col_dbl(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &gain_data->pin1_gain[unit], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN1 GAIN failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
 
    colnum = OFFSET_PIN1;
    fits_read_col_dbl(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &gain_data->pin1_offset[unit], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN1 OFFSET failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
 
    colnum = GAIN_PIN2;
    fits_read_col_dbl(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &gain_data->pin2_gain[unit], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN2 GAIN failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
 
    colnum = OFFSET_PIN2;
    fits_read_col_dbl(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &gain_data->pin2_offset[unit], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN2 OFFSET failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = GAIN_PIN3;
    fits_read_col_dbl(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &gain_data->pin3_gain[unit], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN3 GAIN failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
 
    colnum = OFFSET_PIN3;
    fits_read_col_dbl(hxdpinlin_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &gain_data->pin3_offset[unit], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt PIN3 OFFSET failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
  }

  return status;
}

