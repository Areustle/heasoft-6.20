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
 *                 add hxdcaldbUtil_set_date          by Y.Terada
 *         v0.1.4; 2005-02-07,
 *                 debug by Y.Terada
 *         v0.2.4; 2005-05-17,
 *                 change INSTRUME and DETNAM by Y.Terada
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
 *         v0.4.0; 2005-06-13 Y.Terada
 *                 GSFC comment
 *         v0.4.1; 2005-06-25 Y.Terada    
 *                 add Primary Header (TELESCOP/INSTRUME/CHECKSUM)
 *         v0.4.2; 2005-08-30, by T.Kitaguchi
 *                 support ae_hxd_pinthr.fits
 *         v0.4.3; 2005-09-28 Y.Terada    
 *                 include GSFC comment
 *         v0.4.8; 2005-11-28 Y.Terada
 *                 include GSFC comment
 *         v0.4.9; 2005-11-29 Y.Terada
 *                 include GSFC comment, use boundary
 *                 caldb on ftp://legacy.gsfc.nasa.gov/caldb/data/suzaku
 *         v0.5.2; 2006-05-26 M.K
 *                 hxdcaldbUtil_gsogainhist.c changed
 *         v0.5.3; 2006-06-26 M.K
 *                 hxdcaldbUtil_gsogainhist.c minor updated
 *         v0.5.4; 2006-07-29 M.K
 *                 hxdcaldbUtil_gsogainhist.c debugged
 *         v0.6.0; 2006-08-29 Y.Terada
 *                 add New File: bstidt I/O, rename files
 *         v0.6.1; 2006-08-30 Y.Terada
 *                 add New File: gsoght I/O
 *         v0.6.2; 2006-08-31 Y.Terada
 *                 format changed (as version 1.2.2.3 caldb)
 *         v0.6.8; 2007-05-11 M.K
 *                 hxdcaldbUtil_gso.c (GHF -> GHT)
 *         v0.6.9; 2007-05-12 K.Yamaoka
 *                 hxdcaldbUtil_bstidt.c changed 
 *         v0.7.0; 2007-05-27 M.K
 *                 hxdcaldbUtil_gsoght.c changed 
 *         v0.7.1; 2007-06-12 M.K
 *                 version string updated
 *         v0.7.6; 2009-12-29 K.Yamaoka
 *                 hxdcaldbUtil_wamghf.c added 
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "hxdcaldbUtil.h"
#include "atFunctions.h"
#include "aste_coord.h"

static char tool_name[] = "hxdcaldbUtil";
static char version[]   = "version 0.7.6";
static char hxdcaldbUtil_fits_date[256];
static char hxdcaldbUtil_fits_time[256];
static long hxdcaldbUtil_version = 0;

int hxdcaldbUtil_ask_date( void ){
  int status = HXDCALDBUTIL_STATUS_OK;
  fprintf(stderr, " Date when DATASET becoms valid (UT) >> ");
  scanf("%s", hxdcaldbUtil_fits_date);
  fprintf(stderr, " Time when DATASET becoms valid (UT) >> ");
  scanf("%s", hxdcaldbUtil_fits_time);
  fprintf(stderr, " Version Number of the file          >> ");
  scanf("%ld", &hxdcaldbUtil_version);
  return status;
}

int hxdcaldbUtil_set_date( char * fits_date, char *fits_time, int the_version){
  int status = HXDCALDBUTIL_STATUS_OK;
  sprintf(hxdcaldbUtil_fits_date,fits_date);
  sprintf(hxdcaldbUtil_fits_time,fits_time);
  hxdcaldbUtil_version = the_version;
  return status;
}

int hxdcaldbUtil_write_fits_header(fitsfile *fits_fp, char *detname, 
				   char *code_name, char *description,
				   char *file_name, 
				   int use_boundary, char *boundary){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  char telecomment[80] = "Telescope (mission) name";
  char instcomment[80] = "Instrument name";  
  char detcomment[80]  = "Detector name";
  char telescop[64]    ;
  char instrume[64]    = "HXD";
  char creator[64]     ; 
  char origin[64]      = "HXD-II team";

  sprintf(telescop, "%s", aste_telescop() );
  sprintf(creator, "hxdcaldbUtil %s", version);

  fits_write_key_str(fits_fp, "CREATOR", creator, "by HXD team", &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_str(fits_fp, "ORIGIN", origin, "Source of the FITS file", 
		     &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_str(fits_fp, "TELESCOP", telescop, telecomment, &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_str(fits_fp, "INSTRUME", instrume, instcomment, &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_str(fits_fp, "DETNAM", detname, "detector name", &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  
  fits_write_key_str(fits_fp, "CCLS0001",
                     "BCF", "Basic Calibration File", &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_str(fits_fp, "CCNM0001", code_name, 
		     "Type of calibration data", &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_str(fits_fp, "CDTP0001",
                     "DATA", "Dataset type (DATA or TASK)", &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_str(fits_fp, "CVSD0001", hxdcaldbUtil_fits_date,
                     "UTC date when file should be first used", &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_str(fits_fp, "CVST0001", hxdcaldbUtil_fits_time,
                     "UTC time when file should be first used", &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }


  fits_write_key_str(fits_fp, "CDES0001", description, "Description", &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (use_boundary){
    fits_write_key_str(fits_fp, "CBD10001",
		       boundary, "Parameter boundaries", &istat);
    if(istat){
      fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", 
	      tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
  }

  fits_write_key_str(fits_fp, "FILENAME", file_name, "Filename", &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_lng(fits_fp, "VERSION", hxdcaldbUtil_version, "", &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_int failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}


int hxdcaldbUtil_write_fits_priheader(fitsfile *fits_fp, char *detname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  char telecomment[80] = "Telescope (mission) name";
  char instcomment[80] = "Instrument name";  
  char detcomment[80]  = "Detector name";
  char telescop[64]    ;
  char instrume[64]    = "HXD";
  int hdutype;

  fits_movabs_hdu(fits_fp, 1, &hdutype, &istat);
  if (istat) {
    fprintf(stderr, "%s: fits_movabs_hdu failed To HDU 1 (status=%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  sprintf(telescop, "%s", aste_telescop() );
  fits_write_key_str(fits_fp, "TELESCOP", telescop, telecomment, &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_str(fits_fp, "INSTRUME", instrume, instcomment, &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_str(fits_fp, "DETNAM", detname, "detector name", &istat);
  if(istat){
    fprintf(stderr, "%s: fits_write_key_str failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_chksum(fits_fp, &istat);
  if (istat) {
    fprintf(stderr, "%s:fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
  }

  return status;
}


