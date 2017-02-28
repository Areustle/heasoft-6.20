/*
 *  hxdcaldbUtil_arfdb_unit.c
 *
 *  version 0.2.0,  created by Y.Terada, 2005-05-06
 *  version 0.2.1,  add FITSread and debug by Y.Terada, 2005-05-09
 *  version 0.2.3   expand the energy bin size Y.Terada, 2005-05-10
 *  version 0.2.5   miner bug fixed, for gcc 2.95.x Y.Terada, 2005-05-18
 *  version 0.4.0; 2005-06-13 Y.Terada,  GSFC comment
 *  version 0.4.1; 2005-06-25 Y.Terada,  add Primary Header
 *  version 0.4.3; 2005-09-28 Y.Terada,  add FILENAME
 *  version 0.4.8; 2005-11-26 Y.Terada, H.Takahashi,  change EXT NAME
 *  version 0.4.9; 2005-11-29 Y.Terada,  use BOUNDARY
 *  version 0.6.2; 2006-08-31 Y.Terada
 *                 format changed (as version 1.2.2.3 caldb)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <sys/types.h>
#include "fitsio.h"
#include "hxdcaldbUtil.h"
#include "hxdFitsCommentUtil.h"

static char tool_name[] = "hxdcaldbUtil_xxxart";

static FILE*     hxdarfdbunit_ascii_list_fp;
static FILE*     hxdarfdbunit_ascii_edef_fp;
static fitsfile* hxdarfdbunit_fits_fp;

static struct {
  double angle   [HXD_ARFDB_U_N_ANGL_MAX];
  char   fname   [HXD_ARFDB_U_N_ANGL_MAX][256];
  int    n_angle;
  int    n_ebin;
} hxdarfdb_com;

/* ========================================
 *   ASCII File I/O
 * ========================================*/
int 
hxdcaldbUtil_arfdbUnit_open_ASCII (char* arfdbUnit_ascii_list_fname,
				   char* arfdbUnit_ascii_edef_fname){
  
  int status = HXDCALDBUTIL_STATUS_OK;

  hxdarfdbunit_ascii_list_fp = fopen(arfdbUnit_ascii_list_fname, "r");
  if (hxdarfdbunit_ascii_list_fp == NULL){
    fprintf(stderr, 
            "hxdcaldbUtil: Cannot open hxdarfdbunit LIST ASCII file(%s)\n",
            arfdbUnit_ascii_list_fname);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxdarfdbunit_ascii_edef_fp = fopen(arfdbUnit_ascii_edef_fname, "r");
  if (hxdarfdbunit_ascii_edef_fp == NULL){
    fprintf(stderr, 
            "hxdcaldbUtil: Cannot open hxdarfdbunit E DEF ASCII file(%s)\n",
            arfdbUnit_ascii_edef_fname);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int 
hxdcaldbUtil_arfdbUnit_read_ASCII (HxdArfdbUnit *arfdbunit_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int angle_id, energy_id;
  FILE*  arfdbunit_ascii_fp;
  
  /** --- (1) read list file --- **/
  hxdarfdb_com.n_angle = 0;
  while (1){
    if (fscanf(hxdarfdbunit_ascii_list_fp, "%lf %s\n",
	       &hxdarfdb_com.angle[hxdarfdb_com.n_angle],
	       &hxdarfdb_com.fname[hxdarfdb_com.n_angle]) == EOF) break;
    hxdarfdb_com.n_angle ++;
  }
  arfdbunit_data->n_angle = hxdarfdb_com.n_angle;

  /** --- (2) read edef file --- **/
  hxdarfdb_com.n_ebin = 0;
  while (1){
    if (fscanf(hxdarfdbunit_ascii_edef_fp, "%lf %lf %d\n",
	       &arfdbunit_data->energy_low [hxdarfdb_com.n_ebin],
	       &arfdbunit_data->energy_high[hxdarfdb_com.n_ebin],
	       &arfdbunit_data->energy_bin [hxdarfdb_com.n_ebin]) == EOF){
      break;
    }

    hxdarfdb_com.n_ebin ++;
  }

  arfdbunit_data->n_energy_bin = hxdarfdb_com.n_ebin;

  /** --- (3) read arf files --- **/
  for (angle_id=0; angle_id<hxdarfdb_com.n_angle; angle_id++){
    /** (3-1) open file **/
    arfdbunit_ascii_fp = fopen(hxdarfdb_com.fname[angle_id], "r");
    if (arfdbunit_ascii_fp == NULL){
      fprintf(stderr, 
	      "hxdcaldbUtil: Cannot open ANGLE ASCII file No. %d (%s)\n",
	      angle_id, hxdarfdb_com.fname[angle_id]);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    /** (3-2) read file **/
    arfdbunit_data->angle[angle_id] = hxdarfdb_com.angle[angle_id];

    for (energy_id = 0; energy_id < hxdarfdb_com.n_ebin; energy_id++ ){
      int temporal_energy_id;
      fscanf (arfdbunit_ascii_fp, "%d %lf\n", &temporal_energy_id, 
	      &arfdbunit_data->specrsp[angle_id][energy_id]);
      if (temporal_energy_id != energy_id){
	fprintf(stderr, "hxdcaldbUtil: %s, Energy ID (%d -> %d) is invalid\n",
		hxdarfdb_com.fname[angle_id], energy_id, temporal_energy_id);
	status = HXDCALDBUTIL_STATUS_NG;
	/* return status; */ /** the file must be closed **/
      }
    }

    /** (3-3) close file **/
    if (fclose(arfdbunit_ascii_fp) ){
      fprintf(stderr, 
	      "hxdcaldbUtil: ANGLE ASCII file No. %d (%s) close failed\n",
	      angle_id, hxdarfdb_com.fname[angle_id]);
      status = HXDCALDBUTIL_STATUS_NG;
      /* return status; */
    }
    
    if (status == HXDCALDBUTIL_STATUS_NG) return status;
    
  } /** --- end of arf files --- **/

  return status;
}

int 
hxdcaldbUtil_arfdbUnit_close_ASCII(void){
  int status = HXDCALDBUTIL_STATUS_OK;
  if (fclose(hxdarfdbunit_ascii_list_fp) ){
    fprintf(stderr, 
            "hxdcaldbUtil: hxdarfdbunit_ascii_list_fp close failed\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if (fclose(hxdarfdbunit_ascii_edef_fp) ){
    fprintf(stderr, 
            "hxdcaldbUtil: hxdarfdbunit_ascii_edef_fp close failed\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  return status;
}

/* ========================================
 *   FITS write
 * ========================================*/
int 
hxdcaldbUtil_arfdbUnit_create_FITS(char *arfdbunit_fits_fname,
				   HxdArfdbUnit *arfdbunit_data,
				   char *detname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;

  /** 1st Extension **/
#define HXDARFDBUNIT_MATRIX_NKEYWORD 2
  static char *ttype1[HXDARFDBUNIT_MATRIX_NKEYWORD] = {
    "ANGLE", "SPECRESP"
  };
  static char matrix_form[] = "1024D"; /* default val */
  static char *tform1[HXDARFDBUNIT_MATRIX_NKEYWORD] = {
    "1D", matrix_form
  }; 
  static char *tunit1[HXDARFDBUNIT_MATRIX_NKEYWORD] = {
    "arcmin", "cm**2"
  };
  char *keyword1[HXDARFDBUNIT_MATRIX_NKEYWORD]={
    "TTYPE1  ", "TTYPE2  "
  };
  char *comment1[HXDARFDBUNIT_MATRIX_NKEYWORD]={
    "Angle from an optical axis of Well Unit",
    "Arf matrix for the angle.                                             The definition of the energy is in EBOUNDS"
  };
  char code_name1[16] = "ARFMATRIX";
  char description1[81] = "Database for hxdarfgen. AEFs for incident angles";

  /** 2nd Extension **/
#define HXDARFDBUNIT_EBOUNDS_NKEYWORD 3
  static char *ttype2[HXDARFDBUNIT_EBOUNDS_NKEYWORD] = {
    "CHANNEL", "E_MIN", "E_MAX"
  };
  static char *tform2[HXDARFDBUNIT_EBOUNDS_NKEYWORD] = {
    "1I", "1D", "1D"
  };
  static char *tunit2[HXDARFDBUNIT_EBOUNDS_NKEYWORD] = {
    "chan", "keV", "keV"
  };
  char *keyword2[HXDARFDBUNIT_EBOUNDS_NKEYWORD]={
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  "
  };
  char *comment2[HXDARFDBUNIT_EBOUNDS_NKEYWORD]={
    "Channel of SPECRESP in the first extension.",
    "Lower bounds of the energy",
    "Upper bounds of the energy"
  };
  char code_name2[16] = "ART_ENERGIES";
  char description2[81] = "Definition table of the Energy channel";

  int bitpix = 8;
  int naxis = 0;
  long nrow=0;
  long naxes[1];
  int ncol;

  int use_boundary = 0;
  char *boundary = "";

  /**** create Fits file *****/
  if( fits_create_file(&hxdarfdbunit_fits_fp, arfdbunit_fits_fname, &istat) ) {
    fprintf(stderr, "%s:fits_create_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if( fits_write_grphdr(hxdarfdbunit_fits_fp, 
			1, 8, 0, naxes, 0, 1, 1, &istat) ){
    fprintf(stderr, "%s:fits_write_grphdr failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /**** create First Extenstion *****/
  if( fits_create_hdu(hxdarfdbunit_fits_fp, &istat) ){
    fprintf(stderr, "%s:fits_create_hdu failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  sprintf(matrix_form,"%d",arfdbunit_data->n_energy_bin);
  strcat(matrix_form,"D");
  *(tform1+1)=matrix_form;

  ncol = sizeof(ttype1)/sizeof(*ttype1);
  if ( fits_write_btblhdr(hxdarfdbunit_fits_fp, arfdbunit_data->n_angle, ncol, 
                          ttype1, tform1, tunit1, "ARFMATRIX", 0, &istat) ) {
    fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (hxdcaldbUtil_write_fits_header(hxdarfdbunit_fits_fp, detname, 
				     code_name1, description1,
				     arfdbunit_fits_fname,
				     use_boundary, boundary)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxdFitsComment_write(hxdarfdbunit_fits_fp, HXDARFDBUNIT_MATRIX_NKEYWORD,
                       keyword1, comment1, &istat);
  if (istat){
    fprintf(stderr, "%s: fits write comment failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /**** create Secound Extenstion *****/
  if( fits_create_hdu(hxdarfdbunit_fits_fp, &istat) ){
    fprintf(stderr, "%s:fits_create_hdu failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  ncol = sizeof(ttype2)/sizeof(*ttype2);
  if ( fits_write_btblhdr(hxdarfdbunit_fits_fp, arfdbunit_data->n_energy_bin, 
			  ncol, ttype2, tform2, tunit2, "ART_ENERGIES",
			  0, &istat)){
    fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (hxdcaldbUtil_write_fits_header(hxdarfdbunit_fits_fp, detname, 
				     code_name2, description2,
				     arfdbunit_fits_fname,
				     use_boundary, boundary)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxdFitsComment_write(hxdarfdbunit_fits_fp, HXDARFDBUNIT_EBOUNDS_NKEYWORD,
                       keyword2, comment2, &istat);
  if (istat){
    fprintf(stderr, "%s: fits write comment failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /**** flush the file ****/
  if(fits_flush_file(hxdarfdbunit_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int 
hxdcaldbUtil_arfdbUnit_write_FITS (HxdArfdbUnit *arfdbunit_data,
				   char *detname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
  int hdutype;

  int colnum;
  long firstelem = 1;
  long nelements = 1;
  int angle_irow;
  int ebin_irow;

  /************ 1st Extenstion *************/
  fits_movabs_hdu(hxdarfdbunit_fits_fp, HXDARFDBUNIT_ARFMATRIX_EXTENSION, 
		  &hdutype, &istat);
  if (istat){
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            tool_name, HXDARFDBUNIT_ARFMATRIX_EXTENSION, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  for (angle_irow = 0; angle_irow <arfdbunit_data->n_angle; angle_irow ++ ){
    colnum = 1; /** ANGLE **/
    fits_write_col_dbl(hxdarfdbunit_fits_fp, colnum, angle_irow+1,
		       firstelem, nelements, 
		       &arfdbunit_data->angle[angle_irow], &istat );
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl ANGLE[%d]=%f failed (%d)\n",
	      tool_name, angle_irow+1, arfdbunit_data->angle[angle_irow],
	      istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status; 
    }

    colnum = 2; /** SPECRESP **/
    fits_write_col_dbl(hxdarfdbunit_fits_fp, colnum, angle_irow+1,
		       firstelem, arfdbunit_data->n_energy_bin,
		       &arfdbunit_data->specrsp[angle_irow][0], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl SPECRESP[%d][%d] failed (%d)\n",
	      tool_name, angle_irow+1, arfdbunit_data->n_energy_bin, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
  } /** End Of Angle_IROW loop**/

  /************ 2nd Extenstion *************/
  fits_movabs_hdu(hxdarfdbunit_fits_fp, HXDARFDBUNIT_EBOUNDS_EXTENSION, 
		  &hdutype, &istat);
  if (istat){
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            tool_name, HXDARFDBUNIT_EBOUNDS_EXTENSION, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  for (ebin_irow = 0; ebin_irow <arfdbunit_data->n_energy_bin; ebin_irow ++ ){
    short e_ch = arfdbunit_data->energy_bin[ebin_irow] ;
    colnum = 1; /** CHANNEL **/
    fits_write_col_sht(hxdarfdbunit_fits_fp, colnum, ebin_irow+1,
		       firstelem, nelements,
                       &e_ch, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_sht CHANNEL[%d] failed (%d)\n",
	      tool_name, ebin_irow+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = 2; /** E_MIN **/
    fits_write_col_dbl(hxdarfdbunit_fits_fp, colnum, ebin_irow+1, 
		       firstelem, nelements,
                       &arfdbunit_data->energy_low[ebin_irow], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl E_MIN[%d] failed (%d)\n",
	      tool_name, ebin_irow+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = 3; /** E_MAX **/
    fits_write_col_dbl(hxdarfdbunit_fits_fp, colnum, ebin_irow+1,
		       firstelem, nelements,
                       &arfdbunit_data->energy_high[ebin_irow], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl E_MAX[%d] failed (%d)\n",
	      tool_name, ebin_irow+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

  } /** End Of Ebin_IROW loop**/


  /*** Write Data and CHECKSUM ***/
  if (fits_movabs_hdu(hxdarfdbunit_fits_fp, HXDARFDBUNIT_ARFMATRIX_EXTENSION, 
                      &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            tool_name, HXDARFDBUNIT_ARFMATRIX_EXTENSION, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if( fits_write_date(hxdarfdbunit_fits_fp, &istat) ){
    fprintf(stderr, "%s:fits_write_date failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (fits_write_chksum(hxdarfdbunit_fits_fp, &istat)) {
    fprintf(stderr, "%s:fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
  }


  if (fits_movabs_hdu(hxdarfdbunit_fits_fp, HXDARFDBUNIT_EBOUNDS_EXTENSION,
                      &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            tool_name, HXDARFDBUNIT_ARFMATRIX_EXTENSION, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if( fits_write_date(hxdarfdbunit_fits_fp, &istat) ){
    fprintf(stderr, "%s:fits_write_date failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (fits_write_chksum(hxdarfdbunit_fits_fp, &istat)) {
    fprintf(stderr, "%s:fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
  }

  /******** Add Primary Header keywords ********/
  if (hxdcaldbUtil_write_fits_priheader(hxdarfdbunit_fits_fp, detname)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int
hxdcaldbUtil_arfdbUnit_close_FITS (void){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
 
  if(fits_close_file(hxdarfdbunit_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

/* ========================================
 *   FITS read
 * ========================================*/
int
hxdcaldbUtil_arfdbUnit_open_FITS(char *arfdbunit_fits_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  if (fits_open_file(&hxdarfdbunit_fits_fp, arfdbunit_fits_fname, 
		     READONLY, &istat)) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
            arfdbunit_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int
hxdcaldbUtil_arfdbUnit_read_FITS(HxdArfdbUnit *arfdbunit_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  int anynul;
  int hdutype;
  long nrow;
  long firstelem = 1;
  long nelements = 1;
  char comment[80];

  int colnum;
  int angle_irow, ebin_irow;

  unsigned char uchar_nulval = 0;
  short         short_nulval = 0;
  double        double_nulval = 0.0;

  /************ 1st Extenstion *************/
  fits_movabs_hdu(hxdarfdbunit_fits_fp, HXDARFDBUNIT_ARFMATRIX_EXTENSION, 
		  &hdutype, &istat);
  if (istat){
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            tool_name, HXDARFDBUNIT_ARFMATRIX_EXTENSION, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_lng(hxdarfdbunit_fits_fp, "NAXIS2", &nrow, comment, &istat);
  arfdbunit_data->n_angle = (int) nrow;
  if(istat){
    fprintf(stderr, "%s:fits_read_key NAXIS2 1st Ext failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /************ 2nd Extenstion *************/
  fits_movabs_hdu(hxdarfdbunit_fits_fp, HXDARFDBUNIT_EBOUNDS_EXTENSION, 
		  &hdutype, &istat);
  if (istat){
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            tool_name, HXDARFDBUNIT_EBOUNDS_EXTENSION, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_lng(hxdarfdbunit_fits_fp, "NAXIS2", &nrow, comment, &istat);
  arfdbunit_data->n_energy_bin = (int) nrow;
  if(istat){
    fprintf(stderr, "%s:fits_read_key NAXIS2 2nd Ext failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /* ----------------
      Read Data Body
   * --------------- */
  /************ 1st Extenstion *************/
  fits_movabs_hdu(hxdarfdbunit_fits_fp, HXDARFDBUNIT_ARFMATRIX_EXTENSION, 
		  &hdutype, &istat);
  if (istat){
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            tool_name, HXDARFDBUNIT_ARFMATRIX_EXTENSION, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  for (angle_irow=0; angle_irow< arfdbunit_data->n_angle; angle_irow++){
    colnum = 1; /** ANGLE **/
    fits_read_col_dbl(hxdarfdbunit_fits_fp, colnum, angle_irow+1,
		      firstelem, nelements, double_nulval, 
		      &arfdbunit_data->angle[angle_irow], &anynul, &istat );
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl ANGLE[%d]=%f failed (%d)\n",
              tool_name, angle_irow+1, arfdbunit_data->angle[angle_irow],
              istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status; 
    }
    
    colnum = 2; /** SPECRESP **/
    fits_read_col_dbl(hxdarfdbunit_fits_fp, colnum, angle_irow+1,
		      firstelem, arfdbunit_data->n_energy_bin,
		      double_nulval, 
		      &arfdbunit_data->specrsp[angle_irow][0], 
		      &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl SPECRESP[%d][%d] failed (%d)\n",
              tool_name, angle_irow+1, arfdbunit_data->n_energy_bin, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
  }

  /************ 2nd Extenstion *************/
  fits_movabs_hdu(hxdarfdbunit_fits_fp, HXDARFDBUNIT_EBOUNDS_EXTENSION, 
		  &hdutype, &istat);
  if (istat){
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            tool_name, HXDARFDBUNIT_EBOUNDS_EXTENSION, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  for (ebin_irow = 0; ebin_irow <arfdbunit_data->n_energy_bin; ebin_irow ++ ){

    short e_ch;
    colnum = 1; /** CHANNEL **/
    fits_read_col_sht(hxdarfdbunit_fits_fp, colnum, ebin_irow+1,
		      firstelem, nelements, double_nulval, 
		      &e_ch, &anynul, &istat);
    arfdbunit_data->energy_bin[ebin_irow] = e_ch;
    if(istat){
      fprintf(stderr, "%s:fits_read_col_sht CHANNEL[%d] failed (%d)\n",
              tool_name, ebin_irow+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = 2; /** E_MIN **/
    fits_read_col_dbl(hxdarfdbunit_fits_fp, colnum, ebin_irow+1, 
		      firstelem, nelements, double_nulval, 
		      &arfdbunit_data->energy_low[ebin_irow], 
		      &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl E_MIN[%d] failed (%d)\n",
              tool_name, ebin_irow+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    colnum = 3; /** E_MAX **/
    fits_read_col_dbl(hxdarfdbunit_fits_fp, colnum, ebin_irow+1,
		      firstelem, nelements, double_nulval, 
		      &arfdbunit_data->energy_high[ebin_irow], 
		      &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl E_MAX[%d] failed (%d)\n",
              tool_name, ebin_irow+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
  }

  return status;
}

/****** EOF *******/
