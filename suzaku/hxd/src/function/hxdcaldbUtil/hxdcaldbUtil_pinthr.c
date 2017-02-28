/*
 *  pin threshold, for hxdgrade,
 *          null version, 2005-02-07, by Y.Terada
 *          v0.4.1; 2005-08-30, by T.Kitaguchi
 *                  support ASCII, FITS I/O
 *         v 0.4.3; 2005-09-28 Y.Terada,  add FILENAME
 *         v 0.4.9; 2005-09-28 Y.Terada,  boundary
 *         v 0.6.2; 2006-08-31 Y.Terada
 *                 format changed (as version 1.2.2.3 caldb)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hxdcaldbUtil.h"
#include "hxdFitsCommentUtil.h"

static char  tool_name[] = "hxdcaldbUtil_pinthr";

static FILE*     hxd_pinthres_ascii_fp;
static fitsfile* hxd_pinthres_fits_fp;
static int hxd_pinthres_irow;


/* ======================================== *
 *   ASCII File I/O                         *
 * ======================================== */
int hxdcaldbUtil_pinthres_open_ASCII ( char* pinthres_ascii_fname ) {
  int status = HXDCALDBUTIL_STATUS_OK;

  hxd_pinthres_ascii_fp = fopen(pinthres_ascii_fname, "r");
  if (hxd_pinthres_ascii_fp == NULL) {
    fprintf(stderr, 
            "%s: Cannot open ASCII file(%s)\n",
            tool_name, pinthres_ascii_fname);
    status = HXDCALDBUTIL_STATUS_NG;
  }

  return status;
}


int hxdcaldbUtil_pinthres_read_ASCII ( double *pinthres_data ) {
  int status = HXDCALDBUTIL_STATUS_OK;

  int    unit_id, pin_id;
  double pin_threshold[HXDPINTHRES_PIN_NUM_OF_1UNIT];

  if ( hxd_pinthres_ascii_fp == NULL ) {
    fprintf(stderr, "hxdcaldbUtil: Invalid file pointer(hxd_pinthres)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  while ( !feof(hxd_pinthres_ascii_fp) ) {
    fscanf(hxd_pinthres_ascii_fp, "%d %lf %lf %lf %lf\n",
           &unit_id, &pin_threshold[0], &pin_threshold[1],
           &pin_threshold[2], &pin_threshold[3]);
      
    for ( pin_id=0; pin_id<HXDPINTHRES_PIN_NUM_OF_1UNIT; pin_id++ ) {
      *(pinthres_data + unit_id * HXDPINTHRES_PIN_NUM_OF_1UNIT + pin_id)
	= pin_threshold[pin_id];
    }
  }

  return status;
}

int hxdcaldbUtil_pinthres_close_ASCII ( void ) {
  int status = HXDCALDBUTIL_STATUS_OK;

  if ( fclose(hxd_pinthres_ascii_fp) ) {
    fprintf(stderr, "%s: Cannot close ASCII file\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
  }

  return status;
}



/* ======================================== *
 *   FITS File Create                       *
 * ======================================== */
int hxdcaldbUtil_pinthres_create_FITS ( char* pinthres_fits_fname ) {
  int status = HXDCALDBUTIL_STATUS_OK;
  
  int  istat  = 0;
  int  bitpix = 8;
  int  naxis  = 0;
  long nrow   = 0;
  long ncol   = 0;

  char detname[16]   = "WELL_PIN";
  char code_name[16] = "PINTHRES";
  char extention_name[]="PINTHRES";
  char description[81] = "PIN lower threshold table (for hxdgrade)";

#define HXDPINTHRES_NKEYWORD 5

    static char *ttype[HXDPINTHRES_NKEYWORD] = {
    "UNIT_ID",
    "THRES_PIN0", "THRES_PIN1",
    "THRES_PIN2", "THRES_PIN3"
  };

  static char *tform[HXDPINTHRES_NKEYWORD] = {
    "1B",
    "1D", "1D",
    "1D", "1D"
  };

  static char *tunit[HXDPINTHRES_NKEYWORD] = {
    "",
    "chan", "chan",
    "chan", "chan"
  };

  static char *minmax_card[] = {
    "TLMIN1  =                    0 / minimum legal value for UNIT_ID",
    "TLMAX1  =                   15 / maximum legal value for UNIT_ID"
  };

  static char *keyword[HXDPINTHRES_NKEYWORD] = {
    "TTYPE1  ",
    "TTYPE2  ", "TTYPE3  ",
    "TTYPE4  ", "TTYPE5  "
  };

  static char *comment[HXDPINTHRES_NKEYWORD] = {
    "Well unit ID from 0 to 15",
    "PIN0 PI lower threshold", "PIN1 PI lower threshold",
    "PIN2 PI lower threshold", "PIN3 PI lower threshold",
  };
  int use_boundary = 0;
  char *boundary = "";

  ncol = sizeof(ttype)/sizeof(*ttype);

  if ( fits_create_file(&hxd_pinthres_fits_fp, pinthres_fits_fname, &istat) ) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n",
            tool_name, pinthres_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if ( fits_create_img(hxd_pinthres_fits_fp,
		       bitpix, naxis, &nrow, &istat) ) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if ( fits_create_tbl(hxd_pinthres_fits_fp, BINARY_TBL, nrow, ncol,
                       ttype, tform, tunit, extention_name, &istat) ) {
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if ( hxdcaldbUtil_write_fits_header(hxd_pinthres_fits_fp, detname, 
                                      code_name, description,
				      pinthres_fits_fname,
				      use_boundary, boundary)
       != HXDCALDBUTIL_STATUS_OK ) {
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;

    return status;
  }
  
  hxdFitsComment_write(hxd_pinthres_fits_fp, HXDPINTHRES_NKEYWORD,
                       keyword, comment, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits write comment failed (%d)\n", tool_name,istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if ( fits_flush_file(hxd_pinthres_fits_fp, &istat) ) {
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  return status;
}
  
  

/* ======================================== *
 *   FITS File Write                        *
 * ======================================== */
int hxdcaldbUtil_pinthres_write_FITS ( double *pinthres_data ) {
  int status = HXDCALDBUTIL_STATUS_OK;

  int istat = 0;
  int colnum;
  int row, irow;
  long firstelem = 1;
  long nelements = 1;
  char detname[16] = "WELL_PIN";

  int unit_id, ch;
  unsigned char uchar_unit;

  enum {
    PINTHRES_UNIT_ID = 1,
    PINTHRES_THRES_PIN0,
    PINTHRES_THRES_PIN1,
    PINTHRES_THRES_PIN2,
    PINTHRES_THRES_PIN3
  }; /** FOR COLUMN NUMBER**/


  for ( row=0; row<HXDPINTHRES_UNIT_NUM; row++ ) {
    irow = row + 1; /** fitsio is FORTRAN **/
    
    colnum = PINTHRES_UNIT_ID;
    unit_id = row;
    uchar_unit = (unsigned char) unit_id;
    fits_write_col_byt(hxd_pinthres_fits_fp, colnum, irow,
		       firstelem, nelements,
		       &uchar_unit, &istat);
    if ( istat ) {
      fprintf(stderr,
              "%s:fits_write_col_byt UNIT_ID failed (%d)\n", 
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINTHRES_THRES_PIN0;
    fits_write_col_dbl(hxd_pinthres_fits_fp, colnum, irow,
		       firstelem, nelements, 
                       &pinthres_data[HXDPINTHRES_PIN_NUM_OF_1UNIT*unit_id+0],
                       &istat);
    if ( istat ) {
      fprintf(stderr,
              "%s:fits_write_col_dbl THRES_PIN0 failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINTHRES_THRES_PIN1;
    fits_write_col_dbl(hxd_pinthres_fits_fp, colnum, irow,
		       firstelem, nelements, 
                       &pinthres_data[HXDPINTHRES_PIN_NUM_OF_1UNIT*unit_id+1],
                       &istat);
    if ( istat ) {
      fprintf(stderr,
              "%s:fits_write_col_dbl THRES_PIN1 failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINTHRES_THRES_PIN2;
    fits_write_col_dbl(hxd_pinthres_fits_fp, colnum, irow,
		       firstelem, nelements, 
                       &pinthres_data[HXDPINTHRES_PIN_NUM_OF_1UNIT*unit_id+2],
                       &istat);
    if ( istat ) {
      fprintf(stderr,
              "%s:fits_write_col_dbl THRES_PIN2 failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    
    colnum = PINTHRES_THRES_PIN3;
    fits_write_col_dbl(hxd_pinthres_fits_fp, colnum, irow,
		       firstelem, nelements, 
                       &pinthres_data[HXDPINTHRES_PIN_NUM_OF_1UNIT*unit_id+3],
                       &istat);
    if ( istat ) {
      fprintf(stderr,
              "%s:fits_write_col_dbl THRES_PIN3 failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
  }    
    
  if (fits_write_date(hxd_pinthres_fits_fp, &istat)) {
    fprintf(stderr,"%s: fits_write_date failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
    }
  
  if (fits_write_chksum(hxd_pinthres_fits_fp, &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if(fits_flush_file(hxd_pinthres_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /******** Add Primary Header keywords ********/
  if ( hxdcaldbUtil_write_fits_priheader(hxd_pinthres_fits_fp, detname)
       != HXDCALDBUTIL_STATUS_OK ) {
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  return status;
}



/* ========================================
 *   Fits File Read
 * ========================================*/
int hxdcaldbUtil_pinthres_open_FITS ( char* pinthres_fits_fname ) {
  int status = HXDCALDBUTIL_STATUS_OK;
  
  int istat  = 0;
  
  if ( fits_open_file(&hxd_pinthres_fits_fp, pinthres_fits_fname,
                      READONLY, &istat) ) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
            pinthres_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}
 
int hxdcaldbUtil_pinthres_read_FITS ( double *pinthres_data ) {
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
  int hdutype;
  int unit_id;
  
  enum {
    PINTHRES_UNIT_ID = 1,
    PINTHRES_THRES_PIN0,
    PINTHRES_THRES_PIN1,
    PINTHRES_THRES_PIN2,
    PINTHRES_THRES_PIN3
  }; /** FOR COLUMN NUMBER**/

  /*** move to PSD SELECTION CRITERIA extension ***/
  fits_movabs_hdu(hxd_pinthres_fits_fp, HXDPINTHRES_THRESHOLD_HDU_ID,
                  &hdutype, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
            tool_name, HXDPINTHRES_THRESHOLD_HDU_ID, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /*** read nrow (number of rows) ***/
  fits_read_key_lng(hxd_pinthres_fits_fp, "NAXIS2", &nrow, 
                    comment, &istat);
  if ( istat ) {
    fprintf(stderr, "%s:fits_read_key NAXIS2 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  for ( irow = 1; irow <= nrow; irow ++ ){
    colnum = PINTHRES_UNIT_ID;
    fits_read_col_byt(hxd_pinthres_fits_fp, colnum,
                      irow, firstelem, nelements, uchar_nulval,
                      &uchar_unit_id,
                      &anynul, &istat);
    if ( istat ) {
      fprintf(stderr, "%s:fits_read_col_byt UNIT ID failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    unit_id = (int) uchar_unit_id;

    colnum = PINTHRES_THRES_PIN0;
    fits_read_col_dbl(hxd_pinthres_fits_fp, colnum, 
                      irow, firstelem, nelements, double_nulval,
                      &pinthres_data[HXDPINTHRES_PIN_NUM_OF_1UNIT*unit_id+0],
                      &anynul, &istat);
    if ( istat ) {
      fprintf(stderr,
              "%s:fits_read_col_dbl PIN0 THRESHOLD failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINTHRES_THRES_PIN1;
    fits_read_col_dbl(hxd_pinthres_fits_fp, colnum, 
                      irow, firstelem, nelements, double_nulval,
                      &pinthres_data[HXDPINTHRES_PIN_NUM_OF_1UNIT*unit_id+1],
                      &anynul, &istat);
    if ( istat ) {
      fprintf(stderr,
              "%s:fits_read_col_dbl PIN1 THRESHOLD failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINTHRES_THRES_PIN2;
    fits_read_col_dbl(hxd_pinthres_fits_fp, colnum, 
                      irow, firstelem, nelements, double_nulval,
                      &pinthres_data[HXDPINTHRES_PIN_NUM_OF_1UNIT*unit_id+2],
                      &anynul, &istat);
    if ( istat ) {
      fprintf(stderr,
              "%s:fits_read_col_dbl PIN2 THRESHOLD failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = PINTHRES_THRES_PIN3;
    fits_read_col_dbl(hxd_pinthres_fits_fp, colnum, 
                      irow, firstelem, nelements, double_nulval,
                      &pinthres_data[HXDPINTHRES_PIN_NUM_OF_1UNIT*unit_id+3],
                      &anynul, &istat);
    if ( istat ) {
      fprintf(stderr,
              "%s:fits_read_col_dbl PIN3 THRESHOLD failed (%d)\n",
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
  }
  
  return status;
}

int hxdcaldbUtil_pinthres_close_FITS ( void ) {
  int status = HXDCALDBUTIL_STATUS_OK;
  
  int istat = 0;
    
  if ( fits_close_file(hxd_pinthres_fits_fp, &istat) ) {
    fprintf(stderr, "%s:fits_close_file hxdpinthres.fits failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  return status;
}

