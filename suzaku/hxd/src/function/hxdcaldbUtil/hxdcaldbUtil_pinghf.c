/*
 *   hxdcaldbUtil_gsogainhist.c
 *         2005-02-05, created by Y.Terada,
 *                     based on hxdgainhistUtil v0.0.3
 *         v0.2.4; 2005-05-17,
 *                 change INSTRUME and DETNAM by Y.Terada
 *                 INSTRUME is fixed to be a value of 'HXD'
 *                 DETNAM is, 'WELL_GSO' 'WELL_PIN' 'WAM_ANTI'
 *         v 0.4.0; 2005-06-13 Y.Terada
 *                 GSFC comment
 *         v 0.4.1; 2005-06-25 Y.Terada
 *         v 0.4.3; 2005-09-28 Y.Terada,  add FILENAME
 *         v 0.4.9; 2005-09-28 Y.Terada,  boundary
 *         v 0.5.5; 2006-08-19 Y.Terada,  debug
 *         v 0.6.2; 2006-08-31 Y.Terada
 *                 format changed (as version 1.2.2.3 caldb)
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "hxdcaldbUtil.h"
#include "hxdFitsCommentUtil.h"
#include "atFunctions.h"
#include "aste_time.h"

static char tool_name[] = "hxdcaldbUtil_pinghf";

static FILE*     hxd_pingainhist_ascii_fp;
static fitsfile* hxd_pingainhist_fits_fp;

/* ===============================================
 *       read information from an ASCII file.
 * ===============================================*/
int 
hxdcaldbUtil_pingainhist_open_ASCII (char* pingainhist_ascii_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  
  hxd_pingainhist_ascii_fp = fopen(pingainhist_ascii_fname, "r");
  if (hxd_pingainhist_ascii_fp == NULL){
    fprintf(stderr, "%s: Cannot open ASCII file(%s)\n",
            tool_name, pingainhist_ascii_fname);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

int 
hxdcaldbUtil_pingainhist_read_ASCII (PIN_GHF* pin_ghf_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int nrow  = 0;
  static double prev_start_time = 0.00;
  AtTimeD attime;

  if (hxd_pingainhist_ascii_fp == NULL){
    fprintf(stderr, "hxdcaldbUtil: Invalid file pointer(hxd_pingainhist)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  while(!feof(hxd_pingainhist_ascii_fp)){
    if (! fscanf (hxd_pingainhist_ascii_fp, "%lf %lf %d %lf %lf %lf %lf\n",
		  &pin_ghf_data->data[nrow].start_time,
		  &pin_ghf_data->data[nrow].end_time,
		  &pin_ghf_data->data[nrow].pin_id,
		  &pin_ghf_data->data[nrow].pin_gain,
		  &pin_ghf_data->data[nrow].pin_gain_error,
		  &pin_ghf_data->data[nrow].pin_offset,
		  &pin_ghf_data->data[nrow].pin_offset_error)) {
      status = HXDCALDBUTIL_STATUS_NG;
      fprintf(stderr, "%s: error in scanning ASCII file, hxd_pingainhist\n",
	      tool_name);
      return status;
    }
    pin_ghf_data->data[nrow].exposure 
      = pin_ghf_data->data[nrow].end_time 
      - pin_ghf_data->data[nrow].start_time;

    aste2attimeD(pin_ghf_data->data[nrow].start_time, &attime);
    pin_ghf_data->data[nrow].start_yyyymmdd 
      = attime.yr*10000 + attime.mo*100 + attime.dy;
    pin_ghf_data->data[nrow].start_hhmmss 
      = attime.hr*10000 + attime.mn*100 + attime.sc;

    /** format check **/
    if (prev_start_time > pin_ghf_data->data[nrow].start_time){
      status = HXDCALDBUTIL_STATUS_NG;
      fprintf(stderr, "%s: Invalid Time order %d (%f -> %f)\n", 
	      tool_name, nrow, prev_start_time, 
	      pin_ghf_data->data[nrow].start_time);
      return status;
    }

    prev_start_time = pin_ghf_data->data[nrow].start_time;

    nrow ++;

  } /** end of file **/

  pin_ghf_data->nrow = nrow;
  return status;
}

int 
hxdcaldbUtil_pingainhist_close_ASCII(void){
  int status = HXDCALDBUTIL_STATUS_OK;
  
  if (fclose(hxd_pingainhist_ascii_fp) ){
    fprintf(stderr, "%s: Cannot close ASCII file\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

/* ===============================================
 *       Write FITS File
 * ===============================================*/

int 
hxdcaldbUtil_pingainhist_create_FITS(char* pingainhist_fits_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;

#define HXDPIN_GAINHIST_NKEYWORD 10
  char *ttype[HXDPIN_GAINHIST_NKEYWORD] = {
    "START_TIME", "YYYYMMDD" ,"HHMMSS", "END_TIME", "EXPOSURE", "PIN_ID",
    "PIN_GAIN", "PIN_GAIN_ERROR", "PIN_OFFSET", "PIN_OFFSET_ERROR"
  };
  char *tform[HXDPIN_GAINHIST_NKEYWORD] = {
    "1D", "1J", "1J", "1D","1D", "1B",
    "1D", "1D", "1D", "1D"
  };
  char *tunit[HXDPIN_GAINHIST_NKEYWORD] = {
    "s", "", "", "s","s","",
    "", "", "", ""
  };
  char *keyword[HXDPIN_GAINHIST_NKEYWORD] = {
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  ", 
    "TTYPE6  ", "TTYPE7  ", "TTYPE8  ", "TTYPE9  ", "TTYPE10 "
  };
  char *comment[HXDPIN_GAINHIST_NKEYWORD] = {
    "Start Time", "DATE of start time", "TIME of start time", "End Time",
    "Exposure from start time to end time", "PIN ID (0 -- 63)", 
    "PIN Gain value", "error of PIN GAIN",
    "PIN Offset value ", "error of PIN OFFSET"
  };
  int ncol = sizeof(ttype)/sizeof(*ttype);
  int bitpix = 8;
  int naxis = 0;
  long nrow=0;
  char detname[16] = "WELL_PIN";
  char code_name[16] = "GAIN_HISTORY";
  char extention_name[]="PIN_GHF";
  char description[81] = "PIN gain history table";
  int use_boundary = 0;
  char *boundary = "";

  if (fits_create_file(&hxd_pingainhist_fits_fp, pingainhist_fits_fname, 
		       &istat)) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n", tool_name, 
            pingainhist_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } 

  if(fits_create_img(hxd_pingainhist_fits_fp, bitpix, naxis, &nrow, 
                     &istat)) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if( fits_create_tbl(hxd_pingainhist_fits_fp, BINARY_TBL, nrow, ncol,
                      ttype, tform, tunit, extention_name, &istat) ){
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (hxdcaldbUtil_write_fits_header(hxd_pingainhist_fits_fp, 
                                     detname, code_name, description,
				     pingainhist_fits_fname,
				     use_boundary, boundary)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxdFitsComment_write(hxd_pingainhist_fits_fp,HXDPIN_GAINHIST_NKEYWORD,
                       keyword, comment, &istat);
  if (istat){
    fprintf(stderr, "%s: fits write comment failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if(fits_flush_file(hxd_pingainhist_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int 
hxdcaldbUtil_pingainhist_write_FITS(PIN_GHF* pin_ghf_data){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  int colnum;
  int irow;
  long firstelem = 1;
  long nelements = 1;
  char detname[16] = "WELL_PIN"; /** pri header **/
  enum{
    PINGHF_START_TIME=1, 
    PINGHF_YYYYMMDD,
    PINGHF_HHMMSS, 
    PINGHF_END_TIME, 
    PINGHF_EXPOSURE, 
    PINGHF_PIN_ID,
    PINGHF_PIN_GAIN, 
    PINGHF_PIN_GAIN_ERROR, 
    PINGHF_PIN_OFFSET, 
    PINGHF_PIN_OFFSET_ERROR
  }; /** FOR COLUMN NUMBER**/

  /** reformat **/
  unsigned char uchar_pin_id;

  for(irow=1; irow<=pin_ghf_data->nrow; irow++){

    /*** write ***/
    colnum = PINGHF_START_TIME;
    fits_write_col_dbl(hxd_pingainhist_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &pin_ghf_data->data[irow-1].start_time, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl start time failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_YYYYMMDD;
    fits_write_col_int(hxd_pingainhist_fits_fp, colnum, 
		       irow, firstelem, nelements, 
		       &pin_ghf_data->data[irow-1].start_yyyymmdd, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_int yyyymmdd failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_HHMMSS;
    fits_write_col_int(hxd_pingainhist_fits_fp, colnum, 
		       irow, firstelem, nelements, 
		       &pin_ghf_data->data[irow-1].start_hhmmss, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_int hhmmss failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_END_TIME;
    fits_write_col_dbl(hxd_pingainhist_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &pin_ghf_data->data[irow-1].end_time, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl end time failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_PIN_ID;
    uchar_pin_id = (unsigned char) pin_ghf_data->data[irow-1].pin_id;
    fits_write_col_byt(hxd_pingainhist_fits_fp, colnum, 
                       irow, firstelem, nelements, &uchar_pin_id, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_byt pin id failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }


    colnum = PINGHF_EXPOSURE;
    fits_write_col_dbl(hxd_pingainhist_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &pin_ghf_data->data[irow-1].exposure, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl exposure failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_PIN_GAIN;
    fits_write_col_dbl(hxd_pingainhist_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &pin_ghf_data->data[irow-1].pin_gain, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl pingain failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_PIN_GAIN_ERROR;
    fits_write_col_dbl(hxd_pingainhist_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &pin_ghf_data->data[irow-1].pin_gain_error, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl pingain err failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_PIN_OFFSET;
    fits_write_col_dbl(hxd_pingainhist_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &pin_ghf_data->data[irow-1].pin_offset, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl pinoffset failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_PIN_OFFSET_ERROR;
    fits_write_col_dbl(hxd_pingainhist_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &pin_ghf_data->data[irow-1].pin_offset_error, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl pinoffset err failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

  } /** end of irow **/

  if (fits_modify_key_lng(hxd_pingainhist_fits_fp, "NAXIS2", 
			  pin_ghf_data->nrow, "&", &istat)){
    fprintf(stderr, "%s:fits_update_key NAXIS2 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (fits_write_date(hxd_pingainhist_fits_fp, &istat)) {
    fprintf(stderr,"%s: fits_write_date failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if (fits_write_chksum(hxd_pingainhist_fits_fp, &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if(fits_flush_file(hxd_pingainhist_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /******** Add Primary Header keywords ********/
  if (hxdcaldbUtil_write_fits_priheader(hxd_pingainhist_fits_fp, detname)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;

}


/* ===============================================
 *       Read FITS File
 * ===============================================*/
int 
hxdcaldbUtil_pingainhist_open_FITS(char* pingainhist_fits_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  if (fits_open_file(&hxd_pingainhist_fits_fp,pingainhist_fits_fname,READONLY,
		     &istat)) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
            pingainhist_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  return status;
}

int 
hxdcaldbUtil_pingainhist_read_FITS(PIN_GHF* pin_ghf_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  int colnum;
  long irow;
  long nrow;
  long firstelem = 1;
  long nelements = 1;
  char comment[80];
  unsigned char uchar_nulval = 0;
  double        double_nulval = 0.0;
  unsigned char uchar_pin_id;
  int anynul;
  enum{
    PINGHF_START_TIME=1, 
    PINGHF_YYYYMMDD,
    PINGHF_HHMMSS, 
    PINGHF_END_TIME, 
    PINGHF_EXPOSURE, 
    PINGHF_PIN_ID, 
    PINGHF_PIN_GAIN, 
    PINGHF_PIN_GAIN_ERROR, 
    PINGHF_PIN_OFFSET, 
    PINGHF_PIN_OFFSET_ERROR
  }; /** FOR COLUMN NUMBER**/
  int hdutype;

  /*** move to PIN GAIN extension ***/
  fits_movabs_hdu(hxd_pingainhist_fits_fp, HXDPINGHF_PINGAIN_HDU_ID,
		  &hdutype,&istat);
  if(istat){
    fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
            tool_name, HXDPINGHF_PINGAIN_HDU_ID, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** read nrow (number of rows) ***/
  fits_read_key_lng(hxd_pingainhist_fits_fp, "NAXIS2", &nrow, 
                    comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key NAXIS2 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  for (irow = 1; irow <= nrow; irow ++){

    colnum = PINGHF_START_TIME;
    fits_read_col_dbl(hxd_pingainhist_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &pin_ghf_data->data[irow-1].start_time, &anynul, 
		      &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl start time failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_END_TIME;
    fits_read_col_dbl(hxd_pingainhist_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &pin_ghf_data->data[irow-1].end_time, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl end time failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_EXPOSURE;
    fits_read_col_dbl(hxd_pingainhist_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &pin_ghf_data->data[irow-1].exposure, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl exposure failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_PIN_ID;
    fits_read_col_byt(hxd_pingainhist_fits_fp, colnum, 
		      irow, firstelem, nelements, uchar_nulval,
		      &uchar_pin_id, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_byt pin id failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    pin_ghf_data->data[irow-1].pin_id = (int) uchar_pin_id;

    colnum = PINGHF_PIN_GAIN;
    fits_read_col_dbl(hxd_pingainhist_fits_fp, colnum, 
                       irow, firstelem, nelements, double_nulval,
                       &pin_ghf_data->data[irow-1].pin_gain, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl pingain failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_PIN_GAIN_ERROR;
    fits_read_col_dbl(hxd_pingainhist_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &pin_ghf_data->data[irow-1].pin_gain_error, &anynul, 
		      &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl pingain err failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_PIN_OFFSET;
    fits_read_col_dbl(hxd_pingainhist_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &pin_ghf_data->data[irow-1].pin_offset, &anynul, 
		      &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl pinoffset failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINGHF_PIN_OFFSET_ERROR;
    fits_read_col_dbl(hxd_pingainhist_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &pin_ghf_data->data[irow-1].pin_offset_error, &anynul, 
		      &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl pinoffset err failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

  } /** end of irow **/

  return status;

}

int 
hxdcaldbUtil_pingainhist_close_FITS(void){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  
  if(fits_close_file(hxd_pingainhist_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

/****** EOF ********/
