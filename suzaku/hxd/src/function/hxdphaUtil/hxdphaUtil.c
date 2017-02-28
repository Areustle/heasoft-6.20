/*
 * hxdphaUtil
 *      to make PHA and PI spectrum FITS.
 *      v0.0.1, created by Y.Terada
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "fitsio.h"
#include "hxdphaUtil.h"
#include "hxdFitsHeaderUtil.h"
#include "anl.h"

static fitsfile* fits_fp;
static char tool_name[] = "hxdphaUtil";
static int hxdphaUtil_add_PRIM_header_keywords(double exposure); 
static int hxdphaUtil_add_SPEC_header_keywords(double exposure, 
					       char* chantype, int detchans);


/** ------------------------------------------
 **                  Create FITS
 ** ------------------------------------------*/
int hxdphaUtil_create_FITS( char * fitsname, HXD_STD_KEYS header,
			    double exposure, char* chantype,
			    int detchans){
  int status = HXDPHAUTIL_OK;
  int istat  = 0;

  static char *ttype[2] = {"CHANNEL", "COUNTS"};
  static char *tform[2] = {"J", "J"};
  static char *tunit[2] = {"chan", "count"};
  char *keyword[2]      = {"TTYPE1  ", "TTYPE2  "};
  char *comment[2]     = {"Detector channel (type unknown)",
                          "Counts per channel"};
  int  bitpix = -32;
  int  naxis  = 0;
  long naxis2 = 0;
  long nrow   = 0;
  int  ncol   = sizeof(ttype)/sizeof(*ttype);
  static char extention_name[]="SPECTRUM";

  /**** create Fits file *****/
  if (fits_create_file(&fits_fp, fitsname, &istat)) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n", tool_name, 
            fitsname, istat);
    status = HXDPHAUTIL_NG;
    return status;
  } 

  if(fits_create_img(fits_fp, bitpix, naxis, &nrow, &istat)) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", tool_name, istat);
    status = HXDPHAUTIL_NG;
    return status;
  }

  if( fits_create_tbl( fits_fp, BINARY_TBL, nrow, ncol,
                       ttype, tform, tunit, extention_name, &istat) ){
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", tool_name, istat);
    status = HXDPHAUTIL_NG;
    return status;
  }

  /**** write fits header ****/
  header.hduclass_ev_hk = HXD_FITS_HEADER_UTIL_HDUCLASS_PHA_PRI;
  if (hxdFitsHeader_writeHXDStdKeys(fits_fp, header, HXDPHAUTIL_PRIMARY_HDU,
				    &istat)  != ANL_OK) {
    fprintf(stderr,"%s: Error in writeHXDStdKey() (status=%d)\n",
            tool_name,istat);
    return ANL_NG;
  }

  hxdphaUtil_add_PRIM_header_keywords(exposure);

  header.hduclass_ev_hk = HXD_FITS_HEADER_UTIL_HDUCLASS_PHA;
  if (hxdFitsHeader_writeHXDStdKeys(fits_fp, header, HXDPHAUTIL_SPECTRU_HDU,
				    &istat)  != ANL_OK) {
    fprintf(stderr,"%s: Error in writeHXDStdKey() (status=%d)\n",
            tool_name,istat);
    return ANL_NG;
  }

  hxdphaUtil_add_SPEC_header_keywords(exposure, chantype, detchans);

  return status;
}

/** ------------------------------------------
 **                  Write FITS
 ** ------------------------------------------*/
int hxdphaUtil_write_FITS ( HxdPha *hxdpha, HXD_STD_KEYS header, 
			    HXD_GTI *gti){
  int status = HXDPHAUTIL_OK;
  int istat  = 0;

  int irow;
  int chan;
  long firstelem = 1;
  long nelements = 1;
  int hdutype;
  int colnum;

  fits_movabs_hdu(fits_fp, HXDPHAUTIL_SPECTRU_HDU,
                  &hdutype,&istat);
  if(istat){
    fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
            tool_name, HXDPHAUTIL_SPECTRU_HDU, istat);
    status = HXDPHAUTIL_NG;
    return status;
  }

  for (chan = 0; chan < hxdpha->detchans; chan ++){
    irow = chan + 1;

    colnum = 1;
    fits_write_col_int(fits_fp, colnum, irow, firstelem, nelements,
		       &chan, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_int CHANNEL failed (%d)\n", 
	      tool_name, istat);
      status = HXDPHAUTIL_NG;
      return status;
    }

    colnum = 2;
    fits_write_col_int(fits_fp, colnum, irow, firstelem, nelements,
		       &hxdpha->counts[chan], &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_int COUNT failed (%d)\n", 
	      tool_name, istat);
      status = HXDPHAUTIL_NG;
      return status;
    }

  } /** end of irow **/

  if (hxdgtiFits_createGTIxtention(fits_fp, HXDPHAUTIL_GTI_HDU,
                                   *gti, header, &istat)
      != ANL_OK) {
    fprintf(stderr,"%s: error in creatin GTI extention (%d)\n",tool_name,
            istat);
    status = HXDPHAUTIL_NG;
    return status;
  }

  return status;
}

/** ------------------------------------------
 **                  Close FITS
 ** ------------------------------------------*/
int hxdphaUtil_close_FITS ( void ){
  int status = HXDPHAUTIL_OK;
  int istat  = 0;
  if(fits_close_file(fits_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", tool_name, istat);
    status = HXDPHAUTIL_NG;
    return status;
  }

  return status;
}

/** ------------------------------------------
 **             Local Functions
 ** ------------------------------------------*/
static int
hxdphaUtil_add_PRIM_header_keywords(double exposure){
  int status = HXDPHAUTIL_OK;
  int istat=0;
  int hdutype;


  fits_movabs_hdu(fits_fp, HXDPHAUTIL_PRIMARY_HDU,
                  &hdutype, &istat);
  if(istat){
    fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
            tool_name, HXDPHAUTIL_SPECTRU_HDU, istat);
    status = HXDPHAUTIL_NG;
    return status;
  }

  if (fits_write_key_dbl(fits_fp, "EXPOSURE",   exposure, 8, 
                         "Exposure time", &istat)){
    fprintf(stderr,"Error in writing keyword: EXPOSURE (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  if (fits_write_key_dbl(fits_fp, "LIVETIME",   exposure, 8, 
                         "On-source time", &istat)){
    fprintf(stderr,"Error in writing keyword: EXPOSURE (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

}

static int
hxdphaUtil_add_SPEC_header_keywords(double exposure, char* chantype, 
				    int detchans){
  int status = HXDPHAUTIL_OK;
  int hdutype;
  int istat=0;
  double dbl_val;
  long   lng_val;

  char tmp_card[81];
  int i;

  fits_movabs_hdu(fits_fp, HXDPHAUTIL_SPECTRU_HDU,
                  &hdutype, &istat);
  if(istat){
    fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
            tool_name, HXDPHAUTIL_SPECTRU_HDU, istat);
    status = HXDPHAUTIL_NG;
    return status;
  }

  /********** Insert MAX Channel *********/
  /** search pos **/
  for(i=1;;i++){
    if (fits_flush_file(fits_fp, &istat)){
      fprintf(stderr,
	      "Error doing fits_flush_file() (status=%d)\n", istat);
    }

    if (fits_read_record(fits_fp, i, tmp_card, &istat)) {
      istat = 0;
      break;
    }
    /** Insert TLMIN/TLMAX **/
    if(0 == strncmp("TUNIT1 ", tmp_card, 7)){
      lng_val = 0;
      if (fits_insert_key_lng(fits_fp, "TLMIN1",   lng_val, 
			     "Lowest legal channel number", &istat)){
	fprintf(stderr,"Error in writing keyword: TLMIN1 (status=%d)\n",
		istat);
	return HXDPHAUTIL_NG;
      }

      lng_val = (long) (detchans-1);
      if (fits_insert_key_lng(fits_fp, "TLMAX1",   lng_val, 
			     "Highest legal channel number", &istat)){
	fprintf(stderr,"Error in writing keyword: TLMAX1 (status=%d)\n",
		istat);
	return HXDPHAUTIL_NG;
      }
      break;
    } /** end of insert **/

  } /** end of search **/



  /** search pos **/
  for(i=1;;i++){
    if (fits_read_record(fits_fp, i, tmp_card, &istat)) {
      istat = 0;
      break;
    }
    if(0 == strncmp("DETNAM", tmp_card, 6)){
      break;
    }
  }/** end of search **/

  /**** add keywords ****/
  if (fits_insert_key_str(fits_fp, "FILTER",   "NONE", 
                         "no filter in use", &istat)){
    fprintf(stderr,"Error in writing keyword: FILTER (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  if (fits_insert_key_dbl(fits_fp, "EXPOSURE",   exposure, 8, 
                         "Exposure time", &istat)){
    fprintf(stderr,"Error in writing keyword: EXPOSURE (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  dbl_val = 1.00;
  if (fits_insert_key_dbl(fits_fp, "AREASCAL",   dbl_val, 8, 
                         "area scaling factor", &istat)){
    fprintf(stderr,"Error in writing keyword: AREASCAL (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  if (fits_insert_key_str(fits_fp, "BACKFILE",   "none", 
                         "associated background filename", &istat)){
    fprintf(stderr,"Error in writing keyword: BACKFILE (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  dbl_val = 1.00;
  if (fits_insert_key_dbl(fits_fp, "BACKSCAL",   dbl_val, 8, 
                         "background file scaling factor", &istat)){
    fprintf(stderr,"Error in writing keyword: BACKSCAL (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  if (fits_insert_key_str(fits_fp, "CORRFILE",   "none", 
                         "associated correction filename", &istat)){
    fprintf(stderr,"Error in writing keyword: CORRFILE (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  dbl_val = 1.00;
  if (fits_insert_key_dbl(fits_fp, "CORRSCAL",   dbl_val, 8, 
                         "correction file scaling factor", &istat)){
    fprintf(stderr,"Error in writing keyword: CORRSCAL (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  if (fits_insert_key_str(fits_fp, "RESPFILE",   "none", 
                         "associated redistrib matrix filename", &istat)){
    fprintf(stderr,"Error in writing keyword: RESPFILE (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  if (fits_insert_key_str(fits_fp, "ANCRFILE",   "none", 
                         "associated ancillary response filename", &istat)){
    fprintf(stderr,"Error in writing keyword: ANCRFILE (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  if (fits_insert_key_str(fits_fp, "PHAVERSN",   "1992a", 
                         "obsolate", &istat)){
    fprintf(stderr,"Error in writing keyword: (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  lng_val = (long) detchans;
  if (fits_insert_key_lng(fits_fp, "DETCHANS",   lng_val, 
                         "total number possible channels", &istat)){
    fprintf(stderr,"Error in writing keyword: DETCHANS (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  if (fits_insert_key_str(fits_fp, "CHANTYPE",   chantype,
                         "channel type (PHA, PI etc)", &istat)){
    fprintf(stderr,"Error in writing keyword: (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  if (fits_insert_key_log(fits_fp, "POISSERR", 1, 
                         "Poissonian errors to be assumed", &istat)){
    fprintf(stderr,"Error in writing keyword: POISSERR (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  lng_val = 0;
  if (fits_insert_key_lng(fits_fp, "STAT_ERR",   lng_val, 
                         "no statistical error specified", &istat)){
    fprintf(stderr,"Error in writing keyword: STAT_ERR(status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  lng_val = 0;
  if (fits_insert_key_lng(fits_fp, "SYS_ERR",   lng_val, 
                         "no systematic error specified", &istat)){
    fprintf(stderr,"Error in writing keyword: SYS_ERR(status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  lng_val = 0;
  if (fits_insert_key_lng(fits_fp, "GROUPING",   lng_val, 
                         "no grouping of the data has been defined", &istat)){
    fprintf(stderr,"Error in writing keyword: GROUPING(status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  lng_val = 0;
  if (fits_insert_key_lng(fits_fp, "QUALITY",   lng_val, 
                         "no data quality information specified", &istat)){
    fprintf(stderr,"Error in writing keyword: QUALITY(status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  lng_val = 0;
  if (fits_insert_key_lng(fits_fp, "TOTCTS",   lng_val, 
                         "Total counts in spectrum", &istat)){
    fprintf(stderr,"Error in writing keyword: TOTCTS(status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  lng_val = 1;
  if (fits_insert_key_lng(fits_fp, "SPECDELT",   lng_val, 
                         "Binning factor for spectrum", &istat)){
    fprintf(stderr,"Error in writing keyword: TOTCTS(status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  lng_val = 0;
  if (fits_insert_key_lng(fits_fp, "SPECPIX",   lng_val, 
                         "The rebinned channel corresponding to SPECVAL",
			  &istat)){
    fprintf(stderr,"Error in writing keyword: SPECPIX (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }

  dbl_val = 0.00;
  if (fits_insert_key_dbl(fits_fp, "SPECVAL",   dbl_val, 8, 
                         "Original channel value at center of SPECPIX",
			  &istat)){
    fprintf(stderr,"Error in writing keyword: SPECVAL (status=%d)\n",istat);
    return HXDPHAUTIL_NG;
  }
 
  return status;
}
