/*
 *   hxdcaldbUtil_wamghf.c
 *         2009-10-03, created by K.Yamaoka,
 *                     based on hxdcaldbUtil_pinghf.c
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

static char tool_name[] = "hxdcaldbUtil_wamghf";

static FILE*     hxd_wamghf_ascii_fp;
static fitsfile* hxd_wamghf_fits_fp;
static int hxd_wamghf_irow;

/* ===============================================
 *       read information from an ASCII file.
 * ===============================================*/
int 
hxdcaldbUtil_wamghf_open_ASCII (char* wamghf_ascii_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  
  hxd_wamghf_ascii_fp = fopen(wamghf_ascii_fname, "r");
  if (hxd_wamghf_ascii_fp == NULL){
    fprintf(stderr, "%s: Cannot open ASCII file(%s)\n",
            tool_name, wamghf_ascii_fname);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

int 
hxdcaldbUtil_wamghf_read_ASCII (WAM_GHF* wam_ghf_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int nrow[HXDCALDBUTIL_ANTI_N_UNIT]  = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  double start_time, end_time, exposure;
  int unit_id, model_id;
  int unit;
  double peak, peak_err, sigma, sigma_err, area, area_err, redu_chisq;
  static double prev_start_time = 0.00;
  AtTimeD attime;

  if (hxd_wamghf_ascii_fp == NULL){
    fprintf(stderr, "hxdcaldbUtil: Invalid file pointer(hxd_wamghf)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  while(!feof(hxd_wamghf_ascii_fp)){
    if (! fscanf (hxd_wamghf_ascii_fp, "%lf %lf %lf %d %d %lf %lf %lf %lf %lf %lf %lf\n",
		  &start_time, &end_time, &exposure, &unit_id, 
		  &model_id, &peak, &peak_err, &sigma, &sigma_err,
		  &area, &area_err, &redu_chisq)) {
      status = HXDCALDBUTIL_STATUS_NG;
      fprintf(stderr, "%s: error in scanning ASCII file, hxd_wamghf\n",
	      tool_name);
      return status;
    }

    /*
    wam_ghf_data->data[nrow[unit_id]].exposure 
      = wam_ghf_data->data[nrow[unit_id]].end_time 
      - wam_ghf_data->data[nrow[unit_id]].start_time;
    */

    wam_ghf_data->data[nrow[unit_id]].start_time[unit_id] = start_time;
    aste2attimeD(wam_ghf_data->data[nrow[unit_id]].start_time[unit_id], &attime);
    wam_ghf_data->data[nrow[unit_id]].start_yyyymmdd[unit_id] 
      = attime.yr*10000 + attime.mo*100 + attime.dy;
    wam_ghf_data->data[nrow[unit_id]].start_hhmmss[unit_id] 
      = attime.hr*10000 + attime.mn*100 + attime.sc;

    wam_ghf_data->data[nrow[unit_id]].end_time[unit_id] = end_time;
    wam_ghf_data->data[nrow[unit_id]].exposure[unit_id] = exposure;
    wam_ghf_data->data[nrow[unit_id]].model_id[unit_id] = model_id;
    wam_ghf_data->data[nrow[unit_id]].peak[unit_id] = peak;
    wam_ghf_data->data[nrow[unit_id]].peak_err[unit_id] = peak_err;
    wam_ghf_data->data[nrow[unit_id]].sigma[unit_id] = sigma;
    wam_ghf_data->data[nrow[unit_id]].sigma_err[unit_id] = sigma_err;
    wam_ghf_data->data[nrow[unit_id]].area[unit_id] = area;
    wam_ghf_data->data[nrow[unit_id]].area_err[unit_id] = area_err;
    wam_ghf_data->data[nrow[unit_id]].redu_chisq[unit_id] = redu_chisq;

    /** format check **/
    if (prev_start_time > wam_ghf_data->data[nrow[unit_id]].start_time[unit_id]){
      status = HXDCALDBUTIL_STATUS_NG;
      fprintf(stderr, "%s: Invalid Time order %d (%f -> %f)\n", 
	      tool_name, nrow[unit_id], prev_start_time, 
	      wam_ghf_data->data[nrow[unit_id]].start_time[unit_id]);
      return status;
    }

    prev_start_time = wam_ghf_data->data[nrow[unit_id]].start_time[unit_id];

    nrow[unit_id] ++;

  } /** end of file **/

  for (unit=0; unit<HXDCALDBUTIL_ANTI_N_UNIT; unit++  ){
  wam_ghf_data->nrow[unit] = nrow[unit];
  }

  return status;
}

int 
hxdcaldbUtil_wamghf_close_ASCII(void){
  int status = HXDCALDBUTIL_STATUS_OK;
  
  if (fclose(hxd_wamghf_ascii_fp) ){
    fprintf(stderr, "%s: Cannot close ASCII file\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

/* ===============================================
 *       Write FITS File
 * ===============================================*/

int 
hxdcaldbUtil_wamghf_create_FITS(char* wamghf_fits_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;

#define HXDWAM_GAINHIST_NKEYWORD 13
  char *ttype[HXDWAM_GAINHIST_NKEYWORD] = {
    "START_TIME", "START_YYMMDD", "START_HHMMSS", "END_TIME", "EXPOSURE", "MODEL_ID", "PEAK", "PEAK_ERR", "SIGMA", "SIGMA_ERR", 
    "AREA", "AREA_ERR", "REDU_CHISQ"
  };
  char *tform[HXDWAM_GAINHIST_NKEYWORD] = {
    "1D", "1J", "1J", "1D","1D", "1B",
    "1D", "1D", "1D", "1D", "1D", "1D", "1D"
  };
  char *tunit[HXDWAM_GAINHIST_NKEYWORD] = {
    "s", "", "", "s","s","",
    "channel", "channel", "channel", "channel", "photons/s", "photons/s", ""
  };
  char *keyword[HXDWAM_GAINHIST_NKEYWORD] = {
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  ", 
    "TTYPE6  ", "TTYPE7  ", "TTYPE8  ", "TTYPE9  ", "TTYPE10 ",
    "TTYPE11 ", "TTYPE12 ", "TTYPE13 "
  };
  char *comment[HXDWAM_GAINHIST_NKEYWORD] = {
    "Start Time", "DATE of start time", "TIME of start time", "End Time",
    "Exposure from start time to end time", "Model ID (0:Single Gaussian, 1:Double Gaussian)", 
    "Center of the 511 keV line", "Error of the center",
    "Width of the 511 keV line", "Error of the width", 
    "Total flux of the 511 keV line", "Error of the total flux", 
    "Reduced chis-square of the 511 keV line fitting"
  };

  int ncol = sizeof(ttype)/sizeof(*ttype);
  int bitpix = 8;
  int naxis2 = 0;
  long nrow = 0;
  char detname[16] = "HXD_WAM";
  char code_name[16] = "GAIN_HISTORY";
  char extention_name_base[]="WAM_GHF";
  char extention_name[16];
  /*
  char description[81] = "WAM gain history table";
  */
  char description[81];
  int use_boundary = 0;
  char *boundary = "";
  long naxes[1];
  int unit;

  if (fits_create_file(&hxd_wamghf_fits_fp, wamghf_fits_fname, 
		       &istat)) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n", tool_name, 
            wamghf_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } 

  if (fits_write_grphdr(hxd_wamghf_fits_fp,
                        1, 8, 0, naxes, 0, 1, 1, &istat) ){
    fprintf(stderr, "%s:fits_write_grphdr failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /**** create Extension *****/
  for (unit=0; unit<HXDCALDBUTIL_ANTI_N_UNIT; unit++  ){
    if( fits_create_hdu(hxd_wamghf_fits_fp, &istat) ){
      fprintf(stderr, "%s:fits_create_hdu failed (%d)\n", tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

  sprintf(extention_name, "%s_T%01d%01d", 
	  extention_name_base, unit/5, unit%5);

  if ( fits_write_btblhdr(hxd_wamghf_fits_fp, naxis2, ncol, 
			  ttype, tform, tunit, 
			  extention_name, 0, &istat) ) {
    fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  sprintf(description, "Fit results of the 511 keV line for WAM unit T%01d%01d.", (int)unit/5, (int)unit%5);

  if (hxdcaldbUtil_write_fits_header(hxd_wamghf_fits_fp, 
				     detname, code_name, description, 
				     wamghf_fits_fname,
				     use_boundary, boundary) 
!= HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
    
  hxdFitsComment_write(hxd_wamghf_fits_fp,HXDWAM_GAINHIST_NKEYWORD,
		       keyword, comment, &istat);
  if (istat){
    fprintf(stderr, "%s: fits write comment failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
    
  }

  if(fits_flush_file(hxd_wamghf_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxd_wamghf_irow = 0;
  return status;
}

int 
hxdcaldbUtil_wamghf_write_FITS(WAM_GHF* wam_ghf_data){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  int colnum;
  int irow;
  int unit;
  long firstelem = 1;
  long nelements = 1;
  int hdutype; 
  int extension_id;
  char detname[16] = "HXD_WAM"; /** pri header **/
  enum{
    WAMGHF_DUMMY_C_FORTRUN,
    WAMGHF_START_TIME, 
    WAMGHF_YYYYMMDD,
    WAMGHF_HHMMSS, 
    WAMGHF_END_TIME, 
    WAMGHF_EXPOSURE, 
    WAMGHF_MODEL_ID,
    WAMGHF_PEAK, 
    WAMGHF_PEAK_ERR, 
    WAMGHF_SIGMA, 
    WAMGHF_SIGMA_ERR,
    WAMGHF_AREA, 
    WAMGHF_AREA_ERR,
    WAMGHF_REDU_CHISQ
  }; /** FOR COLUMN NUMBER**/

  /** reformat **/
  unsigned char uchar_val;

  for (unit=0; unit < HXDCALDBUTIL_ANTI_N_UNIT; unit++){
    /*** move to the corresponding extension ***/
    extension_id = HXDWAMGHF_PMTFIT_HDU_ID_OFFSET+unit;
    fits_movabs_hdu(hxd_wamghf_fits_fp, extension_id,
                    &hdutype, &istat);
    if (istat){
      fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
              tool_name, extension_id, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

  for(irow=1; irow<=wam_ghf_data->nrow[unit]; irow++){

    /*** write ***/
    colnum = WAMGHF_START_TIME;
    fits_write_col_dbl(hxd_wamghf_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &wam_ghf_data->data[irow-1].start_time[unit], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl start time failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_YYYYMMDD;
    fits_write_col_int(hxd_wamghf_fits_fp, colnum, 
		       irow, firstelem, nelements, 
		       &wam_ghf_data->data[irow-1].start_yyyymmdd[unit], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_int yyyymmdd failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_HHMMSS;
    fits_write_col_int(hxd_wamghf_fits_fp, colnum, 
		       irow, firstelem, nelements, 
		       &wam_ghf_data->data[irow-1].start_hhmmss[unit], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_int hhmmss failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_END_TIME;
    fits_write_col_dbl(hxd_wamghf_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &wam_ghf_data->data[irow-1].end_time[unit], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl end time failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_MODEL_ID;
    uchar_val = (unsigned char) wam_ghf_data->data[irow-1].model_id[unit];

    fits_write_col_byt(hxd_wamghf_fits_fp, colnum, 
                       irow, firstelem, nelements,
		       &uchar_val, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_byt model id failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }


    colnum = WAMGHF_EXPOSURE;
    fits_write_col_dbl(hxd_wamghf_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &wam_ghf_data->data[irow-1].exposure[unit], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl exposure failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_PEAK;
    fits_write_col_dbl(hxd_wamghf_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &wam_ghf_data->data[irow-1].peak[unit], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl peak failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_PEAK_ERR;
    fits_write_col_dbl(hxd_wamghf_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &wam_ghf_data->data[irow-1].peak_err[unit], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl peak err failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_SIGMA;
    fits_write_col_dbl(hxd_wamghf_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &wam_ghf_data->data[irow-1].sigma[unit], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl sigma failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_SIGMA_ERR;
    fits_write_col_dbl(hxd_wamghf_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &wam_ghf_data->data[irow-1].sigma_err[unit], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl sigma err failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_AREA;
    fits_write_col_dbl(hxd_wamghf_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &wam_ghf_data->data[irow-1].area[unit], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl area failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_AREA_ERR;
    fits_write_col_dbl(hxd_wamghf_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &wam_ghf_data->data[irow-1].area_err[unit], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl area err failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_REDU_CHISQ;
    fits_write_col_dbl(hxd_wamghf_fits_fp, colnum, 
                       irow, firstelem, nelements, 
                       &wam_ghf_data->data[irow-1].redu_chisq[unit], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl redu chisq failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

  } /** end of irow **/


  if (fits_modify_key_lng(hxd_wamghf_fits_fp, "NAXIS2", 
			  wam_ghf_data->nrow[unit], "&", &istat)){
    fprintf(stderr, "%s:fits_update_key NAXIS2 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (fits_write_date(hxd_wamghf_fits_fp, &istat)) {
    fprintf(stderr,"%s: fits_write_date failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if (fits_write_chksum(hxd_wamghf_fits_fp, &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  } /** end of unit **/

  if(fits_flush_file(hxd_wamghf_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /******** Add Primary Header keywords ********/
  if (hxdcaldbUtil_write_fits_priheader(hxd_wamghf_fits_fp, detname)
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
hxdcaldbUtil_wamghf_open_FITS(char* wamghf_fits_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  if (fits_open_file(&hxd_wamghf_fits_fp,wamghf_fits_fname,READONLY,
		     &istat)) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
            wamghf_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  return status;
}

int 
hxdcaldbUtil_wamghf_read_FITS(WAM_GHF* wam_ghf_data){
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
  unsigned char uchar_val;
  int anynul;
  enum{
    WAMGHF_DUMMY_C_FORTRUN,
    WAMGHF_START_TIME, 
    WAMGHF_YYYYMMDD,
    WAMGHF_HHMMSS, 
    WAMGHF_END_TIME, 
    WAMGHF_EXPOSURE, 
    WAMGHF_MODEL_ID, 
    WAMGHF_PEAK, 
    WAMGHF_PEAK_ERR, 
    WAMGHF_SIGMA, 
    WAMGHF_SIGMA_ERR,
    WAMGHF_AREA, 
    WAMGHF_AREA_ERR,
    WAMGHF_REDU_CHISQ
  }; /** FOR COLUMN NUMBER**/
  int hdutype;
  int row;
  int extension_id;
  int unit;

  for (unit=0; unit < HXDCALDBUTIL_ANTI_N_UNIT; unit++){
    /*** move to the corresponding extension ***/
    extension_id = HXDWAMGHF_PMTFIT_HDU_ID_OFFSET+unit;
    fits_movabs_hdu(hxd_wamghf_fits_fp, extension_id,
                    &hdutype, &istat);
    if (istat){
      fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
              tool_name, extension_id, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

  /*** read nrow (number of rows) ***/
  fits_read_key_lng(hxd_wamghf_fits_fp, "NAXIS2", &nrow, 
                    comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key NAXIS2 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  for (irow = 1; irow <= nrow; irow ++){

    colnum = WAMGHF_START_TIME;
    fits_read_col_dbl(hxd_wamghf_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &wam_ghf_data->data[irow-1].start_time[unit], &anynul, 
		      &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl start time failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_END_TIME;
    fits_read_col_dbl(hxd_wamghf_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &wam_ghf_data->data[irow-1].end_time[unit], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl end time failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_EXPOSURE;
    fits_read_col_dbl(hxd_wamghf_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &wam_ghf_data->data[irow-1].exposure[unit], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl exposure failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_MODEL_ID;
    fits_read_col_byt(hxd_wamghf_fits_fp, colnum, 
		      irow, firstelem, nelements, uchar_nulval,
		      &uchar_val, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_byt model id failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    wam_ghf_data->data[irow-1].model_id[unit] = (int) uchar_val;

    colnum = WAMGHF_PEAK;
    fits_read_col_dbl(hxd_wamghf_fits_fp, colnum, 
                       irow, firstelem, nelements, double_nulval,
                       &wam_ghf_data->data[irow-1].peak[unit], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl peak failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_PEAK_ERR;
    fits_read_col_dbl(hxd_wamghf_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &wam_ghf_data->data[irow-1].peak_err[unit], &anynul, 
		      &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl peak err failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_SIGMA;
    fits_read_col_dbl(hxd_wamghf_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &wam_ghf_data->data[irow-1].sigma[unit], &anynul, 
		      &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl sigma failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_SIGMA_ERR;
    fits_read_col_dbl(hxd_wamghf_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &wam_ghf_data->data[irow-1].sigma_err[unit], &anynul, 
		      &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl sigma err failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_AREA;
    fits_read_col_dbl(hxd_wamghf_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &wam_ghf_data->data[irow-1].area[unit], &anynul, 
		      &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl area failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_AREA_ERR;
    fits_read_col_dbl(hxd_wamghf_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &wam_ghf_data->data[irow-1].area_err[unit], &anynul, 
		      &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl area err failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = WAMGHF_REDU_CHISQ;
    fits_read_col_dbl(hxd_wamghf_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &wam_ghf_data->data[irow-1].redu_chisq[unit], &anynul, 
		      &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl redu chisq failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

  } /** end of irow **/

  } /** end of unit **/

  return status;

}

int 
hxdcaldbUtil_wamghf_close_FITS(void){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  
  if(fits_close_file(hxd_wamghf_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

/****** EOF ********/
