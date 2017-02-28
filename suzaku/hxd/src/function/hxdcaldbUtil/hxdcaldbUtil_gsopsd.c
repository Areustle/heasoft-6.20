/*
 *   hxdcaldbUtil_psdsel.c
 *         v0.0.1; 2005-05-23, created by T.Kitaguchi
 *         v0.0.2; 2005-06-03, by T.Kitaguchi
 *                 support FITS file I/O
 *         v 0.4.0; 2005-06-13 Y.Terada
 *                 GSFC comment
 *         v 0.4.1; 2005-06-25 Y.Terada,    add pri herder
 *         v 0.4.2; 2005-08-30 T.Kitaguchi, debug
 *         v 0.4.3; 2005-09-28 Y.Terada,    add FILENAME
 *         v 0.4.7; 2005-11-16 T.Kitaguchi, rot_pi -> rpi
 *         v 0.4.9; 2005-09-28 Y.Terada,    boundary
 *         v 0.6.2; 2006-08-31 Y.Terada
 *                  format changed (as version 1.2.2.3 caldb)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "hxdcaldbUtil.h"
#include "hxdFitsCommentUtil.h"

static char  tool_name[] = "hxdcaldbUtil_gsopsd";

static FILE*     hxd_psdsel_ascii_fp;
static fitsfile* hxd_psdsel_fits_fp;
static int hxd_psdsel_irow;


/* ======================================== *
 *   ASCII File I/O                         *
 * ======================================== */
int hxdcaldbUtil_psdsel_open_ASCII ( char* psdsel_ascii_fname ) {
  int status = HXDCALDBUTIL_STATUS_OK;

  hxd_psdsel_ascii_fp = fopen(psdsel_ascii_fname, "r");
  if (hxd_psdsel_ascii_fp == NULL) {
    fprintf(stderr, 
            "%s: Cannot open ASCII file(%s)\n",
            tool_name, psdsel_ascii_fname);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

int hxdcaldbUtil_psdsel_read_ASCII ( Hxdgrade_Psdsel *psdsel_data ) {
  int status = HXDCALDBUTIL_STATUS_OK;
  int N_row;

  int    unit_id;
  double rpi_fast;
  double rpi_slow_center;
  double rpi_slow_1sigma_upper;
  double rpi_slow_1sigma_lower;

  if ( hxd_psdsel_ascii_fp == NULL ) {
    fprintf(stderr, "hxdcaldbUtil: Invalid file pointer(hxd_psdsel)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  N_row = 0;

  while ( !feof(hxd_psdsel_ascii_fp) ) {
    fscanf(hxd_psdsel_ascii_fp, "%d %lf %lf %lf %lf\n",
	   &unit_id, &rpi_fast, &rpi_slow_center,
	   &rpi_slow_1sigma_upper, &rpi_slow_1sigma_lower);
      
    psdsel_data->data[N_row].unit_id               = unit_id;
    psdsel_data->data[N_row].rpi_fast              = rpi_fast;
    psdsel_data->data[N_row].rpi_slow_center       = rpi_slow_center;
    psdsel_data->data[N_row].rpi_slow_1sigma_upper = rpi_slow_1sigma_upper;
    psdsel_data->data[N_row].rpi_slow_1sigma_lower = rpi_slow_1sigma_lower;

    N_row ++;
  }

  psdsel_data->n_row = N_row;
  return status;
}

int hxdcaldbUtil_psdsel_close_ASCII( void ) {
  int status = HXDCALDBUTIL_STATUS_OK;
  
  if ( fclose(hxd_psdsel_ascii_fp) ) {
    fprintf(stderr, "%s: Cannot close ASCII file\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}



/* ======================================== *
 *   FITS File Create                       *
 * ======================================== */
int  hxdcaldbUtil_psdsel_create_FITS( char *psdsel_fits_fname ) {
  int  status = HXDCALDBUTIL_STATUS_OK;
  int  istat  = 0;
  int  bitpix = 8;
  int  naxis  = 0;
  long nrow   = 0;
  long ncol   = 0;

  char detname[16]   = "WELL_GSO";
  char code_name[16] = "PSDSEL";
  char extention_name[]="PSDSEL";
  char description[81] = "PSD selection criteria in hxdgrade";

#define HXD_PSDSEL_NKEYWORD 5

  static char *ttype[HXD_PSDSEL_NKEYWORD] = {
    "UNIT_ID", "RPI_F", "RPI_S_CEN",
    "RPI_S_UP", "RPI_S_LOW"
  };

  static char *tform[HXD_PSDSEL_NKEYWORD] = {
    "1B", "1D", "1D",
    "1D", "1D"
  };

  static char *tunit[HXD_PSDSEL_NKEYWORD] = {
    "", "chan", "chan",
    "chan", "chan"
  };

  static char *minmax_card[] = {
    "TLMIN1  =                    0 / minimum legal value for UNIT_ID",
    "TLMAX1  =                   15 / maximum legal value for UNIT_ID"
  };

  static char *keyword[HXD_PSDSEL_NKEYWORD] = {
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ",
    "TTYPE4  ", "TTYPE5  "
  };
  
  static char *comment[HXD_PSDSEL_NKEYWORD] = {
    "Well unit ID from 0 to 15",
    "Fast PI channel                                                     / in 45-degree counterclockwise rotation",
    "center of Slow PI channel width                                     / in 45-degree counterclockwise rotation",
    "1 sigma upper width of Slow PI channel                              / in 45-degree counterclockwise rotation",
    "1 sigma lower width of Slow PI channel                              / in 45-degree counterclockwise rotation"
  };
  int use_boundary = 0;
  char *boundary = "";

  ncol = sizeof(ttype)/sizeof(*ttype);

  if ( fits_create_file(&hxd_psdsel_fits_fp, psdsel_fits_fname, &istat) ) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n",
	    tool_name, psdsel_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if ( fits_create_img(hxd_psdsel_fits_fp, bitpix, naxis, &nrow, &istat) ) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if ( fits_create_tbl(hxd_psdsel_fits_fp, BINARY_TBL, nrow, ncol,
                       ttype, tform, tunit, extention_name, &istat) ) {
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if ( hxdcaldbUtil_write_fits_header(hxd_psdsel_fits_fp, detname, 
				      code_name, description,
				      psdsel_fits_fname,
				      use_boundary, boundary)
       != HXDCALDBUTIL_STATUS_OK ) {
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxdFitsComment_write(hxd_psdsel_fits_fp, HXD_PSDSEL_NKEYWORD,
                       keyword, comment, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits write comment failed (%d)\n", tool_name,istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if ( fits_flush_file(hxd_psdsel_fits_fp, &istat) ) {
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxd_psdsel_irow = 0;
  return status;
}

/* ======================================== *
 *   FITS File Write                        *
 * ======================================== */
int hxdcaldbUtil_psdsel_write_FITS( Hxdgrade_Psdsel *psdsel_data ) {
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  int colnum;
  int row, irow;
  long firstelem = 1;
  long nelements = 1;
  char detname[16] = "WELL_GSO";

  int unit, ch;
  unsigned char uchar_unit;

  enum {
    PSDSEL_UNIT_ID = 1,
    PSDSEL_RPI_FAST,
    PSDSEL_RPI_SLOW_CENTER,
    PSDSEL_RPI_SLOW_1SIGMA_UPPER,
    PSDSEL_RPI_SLOW_1SIGMA_LOWER
  }; /** FOR COLUMN NUMBER**/
  
  for ( row=0; row<psdsel_data->n_row; row++ ) {
    irow = row + 1; /** fitsio is FORTRAN **/
    
    colnum = PSDSEL_UNIT_ID;
    uchar_unit = (unsigned char) psdsel_data->data[row].unit_id;
    fits_write_col_byt(hxd_psdsel_fits_fp, colnum, irow, firstelem, nelements, 
		       &uchar_unit, &istat);
    if ( istat ) {
      fprintf(stderr,
	      "%s:fits_write_col_byt UNIT_ID failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
      
    colnum = PSDSEL_RPI_FAST;
    fits_write_col_dbl(hxd_psdsel_fits_fp, colnum, irow, firstelem, nelements, 
		       &psdsel_data->data[row].rpi_fast,
		       &istat);
    if ( istat ) {
      fprintf(stderr,
	      "%s:fits_write_col_dbl RPI_FAST failed (%d)\n",
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PSDSEL_RPI_SLOW_CENTER;
    fits_write_col_dbl(hxd_psdsel_fits_fp, colnum, irow, firstelem, nelements, 
		       &psdsel_data->data[row].rpi_slow_center,
		       &istat);
    if ( istat ) {
      fprintf(stderr,
	      "%s:fits_write_col_dbl RPI_SLOW_CENTER failed (%d)\n",
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PSDSEL_RPI_SLOW_1SIGMA_UPPER;
    fits_write_col_dbl(hxd_psdsel_fits_fp, colnum, irow, firstelem, nelements, 
		       &psdsel_data->data[row].rpi_slow_1sigma_upper,
		       &istat);
    if ( istat ) {
      fprintf(stderr,
	      "%s:fits_write_col_dbl RPI_SLOW_1SIGMA_UPPER failed (%d)\n",
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PSDSEL_RPI_SLOW_1SIGMA_LOWER;
    fits_write_col_dbl(hxd_psdsel_fits_fp, colnum, irow, firstelem, nelements, 
		       &psdsel_data->data[row].rpi_slow_1sigma_lower,
		       &istat);
    if ( istat ) {
      fprintf(stderr,
	      "%s:fits_write_col_dbl RPI_SLOW_1SIGMA_LOWER failed (%d)\n",
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

  }


  if (fits_write_date(hxd_psdsel_fits_fp, &istat)) {
    fprintf(stderr,"%s: fits_write_date failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if (fits_write_chksum(hxd_psdsel_fits_fp, &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if(fits_flush_file(hxd_psdsel_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /******** Add Primary Header keywords ********/
  if (hxdcaldbUtil_write_fits_priheader(hxd_psdsel_fits_fp, detname)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;

}
      
/* ========================================
 *   Fits File Read
 * ========================================*/
int hxdcaldbUtil_psdsel_open_FITS( char* psdsel_fits_fname ) {
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  if ( fits_open_file(&hxd_psdsel_fits_fp, psdsel_fits_fname,
		      READONLY, &istat) ) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
	    psdsel_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  return status;
}

int hxdcaldbUtil_psdsel_read_FITS( Hxdgrade_Psdsel* psdsel_data ) {
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  int colnum;
  long irow;
  long nrow;
  long firstelem = 1;
  long nelements = 1;
  char comment[80];
  double        double_val;
  signed int    int_val;
  unsigned char uchar_val;
  signed int    int_nulval = 0;
  double        double_nulval = 0.0;
  unsigned char uchar_nulval = 0;
  int anynul;
  int extension_id;
  int row;
  unsigned char uchar_unit_id;

  enum {
    PSDSEL_UNIT_ID = 1,
    PSDSEL_RPI_FAST,
    PSDSEL_RPI_SLOW_CENTER,
    PSDSEL_RPI_SLOW_1SIGMA_UPPER,
    PSDSEL_RPI_SLOW_1SIGMA_LOWER
  }; /** FOR COLUMN NUMBER **/
  
  int hdutype;

  /*** move to PSD SELECTION CRITERIA extension ***/
  fits_movabs_hdu(hxd_psdsel_fits_fp, HXDPSDSEL_CRITERIA_HDU_ID,
		  &hdutype, &istat);
  if(istat){
    fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
	    tool_name, HXDPSDSEL_CRITERIA_HDU_ID, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** read CCNM0001 keyword ***/
  fits_read_key_str(hxd_psdsel_fits_fp, "CCNM0001", &psdsel_data->ccnm[0],
                    comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key CCNM0001 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** read nrow (number of rows) ***/
  fits_read_key_lng(hxd_psdsel_fits_fp, "NAXIS2", &nrow, 
		    comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key NAXIS2 failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  for ( irow = 1; irow <= nrow; irow ++ ) {

    colnum = PSDSEL_UNIT_ID;
    fits_read_col_byt(hxd_psdsel_fits_fp, colnum,
		      irow, firstelem, nelements, uchar_nulval,
		      &uchar_unit_id,
		      &anynul, &istat);
    if ( istat ) {
      fprintf(stderr, "%s:fits_read_col_byt UNIT ID failed (%d)\n",
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    psdsel_data->data[irow-1].unit_id = (int) uchar_unit_id;

    colnum = PSDSEL_RPI_FAST;
    fits_read_col_dbl(hxd_psdsel_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &psdsel_data->data[irow-1].rpi_fast,
		      &anynul, &istat);
    if ( istat ) {
      fprintf(stderr,
	      "%s:fits_read_col_dbl PI FAST in 45-degree rotation failed (%d)\n",
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PSDSEL_RPI_SLOW_CENTER;
    fits_read_col_dbl(hxd_psdsel_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &psdsel_data->data[irow-1].rpi_slow_center,
		      &anynul, &istat);
    if ( istat ) {
      fprintf(stderr,
	      "%s:fits_read_col_dbl center of PI SLOW width in 45-degree rotation failed (%d)\n",
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PSDSEL_RPI_SLOW_1SIGMA_UPPER;
    fits_read_col_dbl(hxd_psdsel_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &psdsel_data->data[irow-1].rpi_slow_1sigma_upper,
		      &anynul, &istat);
    if ( istat ) {
      fprintf(stderr,
	      "%s:fits_read_col_dbl 1 sigma upper width of PI SLOW in 45-degree rotation failed (%d)\n",
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PSDSEL_RPI_SLOW_1SIGMA_LOWER;
    fits_read_col_dbl(hxd_psdsel_fits_fp, colnum, 
		      irow, firstelem, nelements, double_nulval,
		      &psdsel_data->data[irow-1].rpi_slow_1sigma_lower,
		      &anynul, &istat);
    if ( istat ) {
      fprintf(stderr,
	      "%s:fits_read_col_dbl 1 sigma lower width of PI SLOW in 45-degree rotation failed (%d)\n",
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    psdsel_data->n_row = nrow;
  }

  return status;
}
 
int hxdcaldbUtil_psdsel_close_FITS( void ) {
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;

  if ( fits_close_file(hxd_psdsel_fits_fp, &istat) ) {
    fprintf(stderr, "%s:fits_close_file hxdpsdsel.fits failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}
