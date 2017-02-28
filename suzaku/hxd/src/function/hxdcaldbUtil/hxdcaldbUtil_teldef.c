/*
 *  hxdcaldbUtil_teldef.c
 *
 *  version 0.2.2,  created by Y.Terada, 2005-05-09
 *        (known_bugs)  cannot read DETECTOR column
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <sys/types.h>
#include <math.h>
#include "fitsio.h"
#include "hxdcaldbUtil.h"
#include "hxdFitsCommentUtil.h"

static char tool_name[] = "hxdcaldbUtil_teldef";

#define DEBUG 0

static fitsfile* hxdteldef_fits_fp;

static double hxdcaldbUtil_calc_hxdoffset(double foc_off, double foc_siz,
					  double focpixl, double foc_scl,
					  double focallen);
int 
hxdcaldbUtil_teldef_open_FITS(char *teldef_fits_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  if (fits_open_file(&hxdteldef_fits_fp, teldef_fits_fname, 
                     READONLY, &istat)) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
            teldef_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int 
hxdcaldbUtil_teldef_read_FITS(HxdTelDef *teldef_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  int anynul;
  int hdutype;
  long nrow;
  long firstelem = 1;
  long nelements = 1;
  char comment[80];

  int colnum;
  int irow;
  float float_nulval = 0.0;
  char char_nulval[64];

  /** keywords in primary extension**/
  double foc_xoff, foc_yoff; /** pixel **/
  double foc_xsiz, foc_ysiz; /** pixel **/
  double focxpixl, focypixl; /** number of pixels **/
  double foc_xscl, foc_yscl; /** mm/pixel **/
  double focallen;           /** focal length =  FC length **/
  
  char_nulval[0] = '\0';

  /************ 1st Extenstion *************/
  fits_movabs_hdu(hxdteldef_fits_fp, HXDTELDEF_ALIGNMENT_EXTENSION, 
                  &hdutype, &istat);
  if (istat){
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            tool_name, HXDTELDEF_ALIGNMENT_EXTENSION, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_lng(hxdteldef_fits_fp, "NAXIS2", &nrow, comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key NAXIS2 1st Ext failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (nrow != HXDTELDEF_ALIGNMENT_NROW){
    fprintf(stderr, "%s: Invalid row number, %d != %d\n", 
	    tool_name,  nrow, HXDTELDEF_ALIGNMENT_NROW);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  for (irow = 0; irow < HXDTELDEF_ALIGNMENT_NROW; irow ++){
    colnum = 1;  /** DETECTOR **/
    /*
    fits_read_col_str(hxdteldef_fits_fp, colnum, irow+1,
		      firstelem, nelements, char_nulval, 
		      teldef_data->det_name[irow], &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_str DETECTOR[%d] failed (%d)\n",
              tool_name, irow+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    */
    /****---------------------------------------------*****/
    if (irow < HXDTELDEF_ALIGNMENT_NROW_PIN){
      sprintf(teldef_data->det_name[irow], "PIN%02d", irow);
    } else {
      sprintf(teldef_data->det_name[irow], "GSO%02d", 
	      irow-HXDTELDEF_ALIGNMENT_NROW_PIN);
    }
    /****---------------------------------------------*****/

    colnum = 2;  /** INT_X **/
    fits_read_col_flt(hxdteldef_fits_fp, colnum, irow+1,
                      firstelem, nelements, float_nulval, 
                      &teldef_data->int_x[irow], &anynul, &istat );
    if(istat){
      fprintf(stderr, "%s:fits_read_col_flt INT_X[%d] failed (%d)\n",
              tool_name, irow+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = 3;  /** INT_Y **/
    fits_read_col_flt(hxdteldef_fits_fp, colnum, irow+1,
                      firstelem, nelements, float_nulval, 
                      &teldef_data->int_y[irow], &anynul, &istat );
    if(istat){
      fprintf(stderr, "%s:fits_read_col_flt INT_Y[%d] failed (%d)\n",
              tool_name, irow+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    teldef_data->det_id[irow] = irow;
  }


  /************ Primary Extenstion *************/
  fits_movabs_hdu(hxdteldef_fits_fp, HXDTELDEF_PRIMARY_EXTENSION, 
                  &hdutype, &istat);
  if (istat){
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            tool_name, HXDTELDEF_PRIMARY_EXTENSION, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdteldef_fits_fp, "FOC_XOFF", &foc_xoff, comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key FOC_XOFF failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdteldef_fits_fp, "FOC_YOFF", &foc_yoff, comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key FOC_YOFF failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdteldef_fits_fp, "FOC_XSIZ", &foc_xsiz, comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key FOC_XSIZ failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdteldef_fits_fp, "FOC_YSIZ", &foc_ysiz, comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key FOC_YSIZ failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdteldef_fits_fp, "FOCXPIX1", &focxpixl, comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key FOCXPIX1 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdteldef_fits_fp, "FOCYPIX1", &focypixl, comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key FOCYPIX1 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdteldef_fits_fp, "FOC_XSCL", &foc_xscl, comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key FOC_XSCL failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdteldef_fits_fp, "FOC_YSCL", &foc_yscl, comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key FOC_YSCL failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdteldef_fits_fp, "FOCALLEN", &focallen, comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key FOCALLEN failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  teldef_data->hxd_offset_x = hxdcaldbUtil_calc_hxdoffset(foc_xoff, 
							  foc_xsiz, focxpixl, 
							  foc_xscl, focallen);

  teldef_data->hxd_offset_y = hxdcaldbUtil_calc_hxdoffset(foc_yoff, 
							  foc_ysiz, focypixl, 
							  foc_yscl, focallen);

  return status;
}

int 
hxdcaldbUtil_teldef_close_FITS (void){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
 
  if(fits_close_file(hxdteldef_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

static double 
hxdcaldbUtil_calc_hxdoffset(double foc_off, /** pixel **/
			    double foc_siz, /** pixel **/
			    double focpixl, /** number of pixels **/
			    double foc_scl, /** mm/pixel **/
			    double focallen){/** focal length =  FC length **/
#define PI 3.14159265

  double offset_angle;

  offset_angle = atan ( (foc_scl*foc_off/focallen) ) * (180.0/PI) * 60.0;

  if (DEBUG) 
    fprintf(stdout, 
	    "foc_scl=%f, foc_off=%3.2f, focallen=%3.2f, %3.2farcmin\n",
	    foc_scl, foc_off, focallen, offset_angle);

  return offset_angle;
}

/****** EOF *******/
