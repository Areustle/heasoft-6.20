/*
 * hxdrspUtil_arf.c
 *      create arf fits file
 *         version 0.0.1 2003-10-15,  created by Y.Terada
 *         version 0.0.2 2003-10-18, modefied by Y.Terada
 *         version 0.0.3 2003-11-23, modefied by Y.Terada
 *                 for NeXT SGD dummy response
 *         version 0.0.4 2004-08-06, by Y.Terada
 *                 for HXD team, included into astroe_dir (hxd home)
 *         version 0.1.0 2005-04-12, by Y.Terada
 *                 included into hxdrspUtil
 *         version 0.1.2 2005-06-06, By Y.Terada
 *                 delete unused printf, add comments and credits.
 *         version 0.1.3 2005-06-13, By Y.Terada
 *                 debug
 *         version 0.1.4 2005-06-22, By Y.Terada
 *                 enlarged the ARF buffer
 *         version 0.2.0 2005-11-04, By Y.Terada
 *                 get file pointer
 *         version 0.2.3 2013-10-10, By Y.Terada
 *                 add return value, etc.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/types.h>
#include "fitsio.h"
#include "hxdrspUtil.h"
#include "hxdFitsHeaderUtil.h"
#include "anl.h"

static char *pname = "hxdrspUtil_arf";
static char version[]   = "version 0.2.3";

static fitsfile* hxdrspUtil_arf_fp = NULL;
#define DEBUG 0 

/* -----------------------------------------------------------
 *          Initialization
 * -----------------------------------------------------------*/
int 
hxdrspUtil_arf_init( HxdArfInfo* com, HxdArfData *data,
		     char *telescop, char *instrume, char *detnam){
  int status = HXDRSPUTIL_OK;
  int i;
  
  for (i=0;i<MKDMYARF_MAX_NAXIS;i++){
    data->row_num[i]     = 0;
    data->energy_low[i]  = 0;
    data->energy_high[i] = 0;
    data->specresp[i]    = 0;
    data->irow           = 0;
  }

  sprintf(com->telescop, telescop);
  sprintf(com->instrume, instrume);
  sprintf(com->detnam,   detnam);

  return status;
}

/* -----------------------------------------------------------
 *          Read data from an ASCII file
 * -----------------------------------------------------------*/
int 
hxdrspUtil_arf_read_ascii ( char* asciiname, HxdArfData* data){
  int status = HXDRSPUTIL_OK;
  int row_id = 0;
  int dmpnum = 20;
  FILE *ascii_fp;
  double e_low, e_high, rsp;

  /*** File Open ***/
  ascii_fp = fopen(asciiname, "r");
  if (ascii_fp == NULL) {
    fprintf(stderr, "file (%s) open error\n", asciiname);
    status = HXDRSPUTIL_NG; 
    return status;
  }

  /*** File Read ***/
  if (DEBUG) fprintf (stderr, "Reading  ASCII FILE (%s) ", asciiname);
  while (1) {
    row_id ++; /** row_id starts 1 **/
    fscanf(ascii_fp, "%lf %lf %lf\n", &e_low, &e_high, &rsp);
    data->energy_low [row_id] = e_low;
    data->energy_high[row_id] = e_high;
    data->specresp   [row_id] = rsp;
    data->row_num    [row_id] = row_id;
    if (feof(ascii_fp)) {
      data->irow = row_id;
      break;
    }
    if (row_id > MKDMYARF_MAX_NAXIS){
      fprintf(stderr, "Warning: %s, line is over %d\n", 
              asciiname, MKDMYARF_MAX_NAXIS);
      break;
    }
    if(dmpnum!=0) if(row_id%dmpnum) continue;
    if (DEBUG) fprintf(stderr, ".");
  }
  if (DEBUG) fprintf (stderr, "done\n");

  /*** File Close ***/
  if ( fclose(ascii_fp) ) {
    fprintf(stderr, "file (%s) close error\n", asciiname);
    status = HXDRSPUTIL_NG; 
    return status;
  }

  /*** Normal End ***/
  return status;
}

/* -----------------------------------------------------------
 *          Dump arf body
 * -----------------------------------------------------------*/
int 
hxdrspUtil_arf_dump_data  ( HxdArfData* data){
  int status = HXDRSPUTIL_OK;
  int row_id;

  fprintf (stdout, "ID E_LOW E_HIGH SPECRSP\n");

  for (row_id=1; row_id<=data->irow; row_id++)
    fprintf (stdout, "%d %f %f %f\n", data->row_num[row_id],
             data->energy_low[row_id], data->energy_high[row_id],
             data->specresp[row_id]);
  fprintf (stdout, "Total ROW Number = %ld\n", data->irow);
  return status;
}

/* -----------------------------------------------------------
 *          Check the arf body
 * -----------------------------------------------------------*/
int 
hxdrspUtil_arf_check_data( HxdArfData* data){
  int status = HXDRSPUTIL_OK;
  int row_id;
  int dmpnum = (data->irow/20);

  for (row_id=1; row_id<=data->irow; row_id++){
    if (data->energy_low[row_id] < 0.0){
      fprintf (stderr, "%s line=%d invalid value ENERG_LO (%f)\n", 
               pname, row_id, data->energy_low[row_id]);
      status = HXDRSPUTIL_NG;
    }
    if (data->energy_high[row_id] < 0.0){
      fprintf (stderr, "%s line=%d invalid value ENERG_HI (%f)\n", 
               pname, row_id, data->energy_high[row_id]);
      status = HXDRSPUTIL_NG;
    }
    if (data->specresp[row_id] < 0.0){
      fprintf (stderr, "%s line=%d invalid value SPECRSP (%f)\n", 
               pname, row_id, data->specresp[row_id]);
      status = HXDRSPUTIL_NG;
    }
    if (data->energy_low[row_id] > data->energy_high[row_id]){
      fprintf (stderr, 
               "%s line=%d ENERG_LO is larger than ENERG_HI (%f -> %f)\n", 
               pname, row_id, 
               data->energy_low[row_id], data->energy_high[row_id]);
      status = HXDRSPUTIL_NG;
    }
    if (data->energy_low[row_id] < data->energy_high[row_id-1]){
      fprintf (stderr, "%s line=%d Energy Range is overlapped (%f -> %f)\n", 
               pname, row_id, 
               data->energy_low[row_id], data->energy_high[row_id-1]);
      status = HXDRSPUTIL_NG;
    }
    if (data->energy_low[row_id-1] > data->energy_low[row_id]){
      fprintf (stderr, 
               "%s line=%d the order of ENERG_LO is swapped (%f -> %f)\n", 
               pname, row_id, 
               data->energy_low[row_id], data->energy_low[row_id-1]);
      status = HXDRSPUTIL_NG;
    }
    if(dmpnum!=0) if(row_id%dmpnum) continue;
    if (DEBUG) fprintf(stderr, ".");
  }

  if (status == HXDRSPUTIL_OK) {
    fprintf(stderr, " OK\n");
  } else {
    fprintf(stderr, "\n   %s: Please Check the INPUT FILE\n", pname );
  }

  return status;
}

/* ===============================================================
 *              Create the ARF Fits file
 * ===============================================================*/
int 
hxdrspUtil_arf_create_fits( char* arfname, HxdArfInfo* com){
  int status = HXDRSPUTIL_OK;
  int istat = 0;
  
  static char *ttype[] = {
    "ENERG_LO", "ENERG_HI", "SPECRESP"
  };
  static char *tform[] = {
    "1E", "1E", "1E"
  };
  static char *tunit[] = {
    "keV", "keV", "cm**2"
  };
  
  static char telecomment[80] = "mission/satellite name";
  static char instcomment[80] = "instrument name";  
  static char detcomment[80]  = "detector name";

  int bitpix = 8;
  int naxis = 0;
  long nrow=0;
  int ncol;
  char creater[80];
  char origin[80];

  char taskname[FITS_KEY_MAX_LENGTH];
  char taskver[FITS_KEY_MAX_LENGTH];
  char credits[FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES];
  int create_mode;
  char *buf;

  if (DEBUG) fprintf(stderr, "Creating ARF Fits (%s)", arfname);

  if (fits_create_file(&hxdrspUtil_arf_fp, arfname, &istat)) {
    fprintf(stderr, "%s:fits_create_file failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  
  if(fits_create_img(hxdrspUtil_arf_fp, bitpix, naxis, &nrow, &istat)) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  ncol = sizeof(ttype)/sizeof(*ttype);
  if( fits_create_tbl( hxdrspUtil_arf_fp, BINARY_TBL, nrow, ncol,
                       ttype, tform, tunit, "SPECRESP", &istat) ){
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  sprintf(creater, "%s @", getpwuid(getuid())->pw_gecos);
  gethostname(creater+strlen(creater), sizeof(creater)-strlen(creater));
  sprintf(origin, "%s: %s", pname, version);

  fits_write_key_str(hxdrspUtil_arf_fp, "CREATOR", creater, 
		     "Creater of this fits",
                     &istat);
  fits_write_key_str(hxdrspUtil_arf_fp, "ORIGIN", origin, 
		     "Tool name and version", 
                     &istat);
  fits_write_key_str(hxdrspUtil_arf_fp, "TELESCOP", 
                     com->telescop, telecomment, &istat);
  fits_write_key_str(hxdrspUtil_arf_fp, "INSTRUME", 
                     com->instrume, instcomment, &istat);
  fits_write_key_str(hxdrspUtil_arf_fp, "DETNAM",
                     com->detnam, detcomment, &istat);
  fits_write_key_str(hxdrspUtil_arf_fp, 
                     "HDUCLASS", "OGIP", "format devised by the OGIP",
                     &istat);
  fits_write_key_str(hxdrspUtil_arf_fp, "HDUCLAS1", "RESPONSE",
                     "dataset associated with instrument response", &istat);
  fits_write_key_str(hxdrspUtil_arf_fp, 
                     "HDUVERS1", "1.0.0", "version of HDUCLAS1 format",
                     &istat);
  fits_write_key_str(hxdrspUtil_arf_fp, "HDUCLAS2", "SPECRESP",
                     "dataset is a Response Matrix", &istat);
  fits_write_key_str(hxdrspUtil_arf_fp, 
                     "HDUVERS2", "1.1.0", "version of HDUCLAS2 format",
                     &istat);
  
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  buf = (char *)strdup(anl_task_name());
  if(strlen(buf) > FITS_KEY_MAX_LENGTH) buf[FITS_KEY_MAX_LENGTH] = '\0';
  sprintf( taskname, "%s", buf);

  buf = (char *)strdup(anl_task_version());
  if(strlen(buf) > FITS_KEY_MAX_LENGTH) buf[FITS_KEY_MAX_LENGTH] = '\0';
  sprintf( taskver, "%s", buf);

  buf = (char *)strdup(anl_task_credits());
  if(strlen(buf) > FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES) 
    buf[FITS_KEY_MAX_LENGTH] = '\0';
  sprintf( credits, "%s", buf);
  create_mode = HXD_FITS_HEADER_UTIL_CREDIT_CREATE;

  hxdFitsHeader_writeCredits(hxdrspUtil_arf_fp, 
                             taskname, taskver, credits, create_mode, 
                             &istat);

  /*
  hxdFitsHeader_writeCredits(hxdrspUtil_arf_fp, anl_task_name(),
 			     anl_task_version(), anl_task_credits(), 
			     HXD_FITS_HEADER_UTIL_CREDIT_CREATE, &istat);
  */

  if (istat){
    fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (%d)\n", 
	    pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  if (DEBUG) fprintf(stderr, " done\n");
  return status;
}

/* ===============================================================
 *              Write the table into the ARF Fits file
 * ===============================================================*/
int
hxdrspUtil_arf_write_fits ( HxdArfData* data ){
  int status = HXDRSPUTIL_OK;

  int istat = 0;
  /* float energy_low, energy_high, specresp; */
  long firstelem = 1;
  long nelements = 1;
  int row_id;
  int dmpnum = (data->irow/20);
  int hdutype;
  int colnum;  float write_val;

  for (row_id=1; row_id<=data->irow; row_id++){
    colnum = 1;
    write_val = (float) data->energy_low[row_id];
    fits_write_col_flt(hxdrspUtil_arf_fp, 
		       colnum, row_id, firstelem, nelements, 
                       &write_val, &istat);
    colnum = 2;
    write_val = (float) data->energy_high[row_id];
    fits_write_col_flt(hxdrspUtil_arf_fp, 
		       colnum, row_id, firstelem, nelements, 
                       &write_val,&istat);
    colnum = 3;
    write_val = (float) data->specresp[row_id];
    fits_write_col_flt(hxdrspUtil_arf_fp,     
		       colnum, row_id, firstelem, nelements, 
                       &write_val, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_flt failed (%d)\n", pname, istat);
      status = HXDRSPUTIL_NG;
      return status;
    }
    if(dmpnum!=0) if(row_id%dmpnum) continue;
    if (DEBUG) fprintf(stderr, ".");
  }

  /**** change header value ***/
  if (fits_movabs_hdu(hxdrspUtil_arf_fp, ARF_FITS_BINTABLE_HDU, 
                      &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            pname, ARF_FITS_BINTABLE_HDU, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  if (fits_modify_key_lng(hxdrspUtil_arf_fp, 
			  "NAXIS2", data->irow, "&", &istat)){
    fprintf(stderr, "%s:fits_update_key NAXIS2 failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  return status;
}

/* ===============================================================
 *                        Close the ARF File
 * ===============================================================*/
int 
hxdrspUtil_arf_close_fits ( void ){
  int status = HXDRSPUTIL_OK;
  int istat = 0;
  int hdutype;

  if (fits_movabs_hdu(hxdrspUtil_arf_fp, ARF_FITS_PRIMARY_HDU, 
                      &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            pname, ARF_FITS_PRIMARY_HDU, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  
  if( fits_write_date(hxdrspUtil_arf_fp, &istat) ){
    fprintf(stderr, "%s:fits_write_date failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  if (fits_write_chksum(hxdrspUtil_arf_fp, &istat)) {
    fprintf(stderr, "%s:fits_write_chksum failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
 
  /*  
  if (fits_movabs_hdu(hxdrspUtil_arf_fp, ARF_FITS_BINTABLE_HDU, 
                      &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
            pname, ARF_FITS_BINTABLE_HDU, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  if (fits_modify_key_lng(hxdrspUtil_arf_fp, 
			  "NAXIS2", data->irow, "&", &istat)){
    fprintf(stderr, "%s:fits_update_key NAXIS2 failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  */
  if(fits_close_file(hxdrspUtil_arf_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", pname, istat);
    status = HXDRSPUTIL_NG;
    return status;
  }
  
  if (DEBUG) fprintf(stderr, "done\n");
  return status;
}


int 
hxdrspUtil_arf_fits_add_comment ( char *comment ){
  int status = HXDRSPUTIL_OK;
  int stat = 0;

  fits_write_comment(hxdrspUtil_arf_fp, comment, &stat);
  if(stat){
    fprintf(stderr, "%s: fits_write_comment failed (%d)\n",
	    pname, stat);
    status = HXDRSPUTIL_NG;
    return status;
  }

  return status;

}

