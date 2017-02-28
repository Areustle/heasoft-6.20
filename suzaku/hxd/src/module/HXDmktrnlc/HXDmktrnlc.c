/**********************************************
HXDmktrnlc: light curve ftools for the TRN data

ver 0.0.1 K.Yamaoka  2005 Oct.18 
   first version to make fits light curve
ver 0.0.2 K.Yamaoka  2005 Oct.18
   modified keyword
ver 0.0.3 K,Yamaoka 2005 Oct 24
   Excluded ev_time<=0 event 
ver 0.0.4 K.Yamaoka 2005 Nov. 8
   change header --> stdkeys
ver 0.0.5 K.Yamaoka 2005 Nov. 8
   support deadtime correction
ver 0.0.6 Y.Terada  2006 Jun. 27
   debug 
ver 0.0.7 Y.Terada  2006 Jul. 10
   avoid time parameter for Solaris (Bryan's comment)
ver 0.0.8 K.Yamaoka 2006 Jul. 12
   support DE compression
ver 0.0.9 Y.Terada  2006 Jul. 14
   debug; core dump in HEADAS env.
ver 0.1.0 K.Yamaoka 2006 Jul. 26
   modify the comment in cread_fits_key 
ver 0.1.1 Y.Terada  2006 Jul. 27
   write par values in FITS header
ver 0.1.2 Y.Terada  2006 Aug. 18
   save memory
ver 0.1.3 Y.Terada  2006 Aug. 24
  --> ver1.0.0
ver 2.0.0 Y.Terada 2007 Jun. 18
   write date and checksum
   debug in initialize of HxdMkTrnLC_lc.
ver 2.0.1 C. Padgett 2011 Mar. 24
   Add support for GTI in input event file
   and output FRACEXP/DEADC columns
   Fixed DEADAPP keyword in output file
   Added TIMEPIXR keyword to output file
**********************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cli.h>
#include <com.h>
#include <bnk.h>
#include <evs.h>
#include <anl.h>
#include <atFunctions.h>
#include <aste_time.h>
#include <fitsio.h>
#include <cfortran.h>
#include <hbook.h>
#include "hxd/HXD.h"
#include <math.h>

#include "hxdtrnFitsUtil.h"
#include "hxdtrnFitsToBnkUtil.h"
#include "hxdFitsHeaderUtil.h"
#include "hxdgtiFitsUtil.h"
#include "pil.h"

#define HXD_TRN_BOARD_NUM 4
#define HXD_TRN_PH_NUM 55
#define TIMEBIN_MAX_NUM 1000000
#define FILENAME_MAX_LENGTH 256

HxdTrnFits data;
char HXDmktrnlc_version[] = "version 2.0.1";

static char pname[] = "HXDmktrnlc";

static char *ttype[] = {
  "TIME", "RATE", "ERROR", "FRACEXP", "DEADC"
};

static char *tform[] = {
  "1D", "1D", "1D", "1D", "1D"
};

static char *tunit[] = {
  "s", "count/s", "count/s", "", ""
};
static char *keyword[] = {
  "TTYPE1", "TTYPE2", "TTYPE3", "TTYPE4", "TTYPE5"
};

static struct {
  char filename[FILENAME_MAX_LENGTH];
  char newfilename[FILENAME_MAX_LENGTH];
  fitsfile *fp;
  int irow;
  int iomode;
  char origin[FILENAME_MAX_LENGTH];
}com;

typedef struct{
  double integ_ph_time[HXD_TRN_BOARD_NUM]; /* in unit of sec */
  int delta_trn_ph[HXD_TRN_BOARD_NUM][TIMEBIN_MAX_NUM];
  double trn_ph_fexp[HXD_TRN_BOARD_NUM][TIMEBIN_MAX_NUM];
  double trn_ph_deadc[HXD_TRN_BOARD_NUM][TIMEBIN_MAX_NUM];
  double trn_ph_rate[HXD_TRN_BOARD_NUM][TIMEBIN_MAX_NUM];
  double trn_ph_rate_err[HXD_TRN_BOARD_NUM][TIMEBIN_MAX_NUM];
  double pre_dtcnt[HXD_TRN_BOARD_NUM];
  int data_num[HXD_TRN_BOARD_NUM]; /** cleared on init **/
} HxdMkTrnLC_lc;

static HxdMkTrnLC_lc* lc = NULL;

typedef struct {
  fitsfile *fits_fp;
  int tpu_board, ph_mode, chmin, chmax, dtcor;
  double dt_clk;
  double time[TIMEBIN_MAX_NUM];
  int ph_time_bin[HXD_TRN_BOARD_NUM];/** cleared on init **/
  HXD_GTI gti;
  HXD_STD_KEYS stdkeys;
} HxdMkTrnLC_trnfits;

static HxdMkTrnLC_trnfits* trnfits = NULL;

static struct{
  int colnum[HXD_TRN_FITS_KEY_NUM];
  long nrow;
} fits;

static int writeHistory_wamlc ( void ){
  int istat = 0;
  int status = ANL_OK;
  char history [PIL_LINESIZE];

  sprintf(history, "    ------- parameters in %s -------   ", pname);
  fits_write_history(trnfits->fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  sprintf(history, "         origin = %s", com.origin);
  fits_write_history(trnfits->fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  sprintf(history, "         outroot = %s", com.newfilename);
  fits_write_history(trnfits->fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  if(trnfits->tpu_board==-1){
    sprintf(history, "         tpu_board = all (0,1,2,3)");
  } else {
    sprintf(history, "         tpu_board = %d", trnfits->tpu_board);
  }
  fits_write_history(trnfits->fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  if (trnfits->ph_mode == 0){
    sprintf(history, "         ph_mode = others");
  } else if (trnfits->ph_mode == 1){
    sprintf(history, "         ph_mode = PH");
  } else {
    sprintf(history, "         ph_mode = unknown");
  }
  fits_write_history(trnfits->fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  sprintf(history, "         channel = (%d ch -- %d ch)", 
	  trnfits->chmin, trnfits->chmax);
  fits_write_history(trnfits->fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  if(trnfits->dtcor){
    sprintf(history, "         dt_cor = Perform dead time correction");
  } else {
    sprintf(history, "         dt_cor = No dead time correction");
  }
  fits_write_history(trnfits->fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  if(trnfits->dtcor){
    sprintf(history, "         dt_clk = %f (sec) dead time clock", trnfits->dt_clk);
    fits_write_history(trnfits->fits_fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history failed (%d)\n", istat);
      status = ANL_NG; return status;
    }
  }

  return status;
}

static int createlcFITS(char *fitsname, int board, int chmin,
                        int chmax, double timedel, int timebin){
  int istat = 0;
  int simple = 1;
  int bitpix = 8;
  int naxis = 0;
  long naxes[1];
  long pcount = 0;
  long gcount = 1;
  int err_flag = 1;
  int extend = 1; /** Extension is included**/
  int nc = 5;
  int hdutype;
  int log_val;
  int tmp_card[81];
  int i, size;
  char buff[80];
  char comment[80];
  char keywordvalue[80];

  fits_create_file(&trnfits->fits_fp, fitsname, &istat);

  if ( istat ) {
    fprintf(stderr, "%s:fits_create_file failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_grphdr(trnfits->fits_fp, simple, bitpix, naxis, naxes,
		      pcount, gcount, extend, &istat);
  } 

  if ( istat ) {
    fprintf(stderr, "%s:fits_write_grphdr failed (%d)\n", pname, istat);
    return ANL_NG;
  } else { 

    hxdFitsHeader_mallocSTDKEYS(&trnfits->stdkeys);
    hxdFitsHeader_setDefaultKeywordValues(&trnfits->stdkeys);
    trnfits->stdkeys.hduclass_ev_hk = HXD_FITS_HEADER_UTIL_HDUCLASS_LC;
    sprintf(trnfits->stdkeys.datamode, "TRANSIENT"); 
    sprintf(buff, "ANL: %s: %s", pname, HXDmktrnlc_version);
    sprintf(trnfits->stdkeys.creator, buff);
    /* sprintf(trnfits->stdkeys.origin, "ISAS/JAXA"); */
    sprintf(trnfits->stdkeys.instrument, "HXD");
    trnfits->stdkeys.use_detnam = 1; 
    sprintf(trnfits->stdkeys.detnam, "WAM_ANTI%d",board);
    trnfits->stdkeys.timepixr = 0.5; /** latch values at the end of the TIME clock **/
    trnfits->stdkeys.timedel = timedel;
    trnfits->stdkeys.chanmin = chmin;
    trnfits->stdkeys.chanmax = chmax;
    
    /* Credits, from ASTE_ANL (later than version 1.40)*/
    sprintf(trnfits->stdkeys.credits,     anl_task_credits());
    sprintf(trnfits->stdkeys.task_name,   anl_task_name()   );
    sprintf(trnfits->stdkeys.task_version,anl_task_version());

    fits_read_key_flt(com.fp, "TIERRELA", &trnfits->stdkeys.tierrela, 
                      comment, &istat);
    fits_read_key_flt(com.fp, "TIERABSO",  &trnfits->stdkeys.tierabso,  
                      comment, &istat);

    fits_read_key_str(com.fp, "TLM_FILE", trnfits->stdkeys.tlmfile, 
                      comment, &istat);
    if(istat){  sprintf(trnfits->stdkeys.tlmfile, "  "); istat=0; }
    fits_read_key_str(com.fp, "TIM_FILE", trnfits->stdkeys.timfile, 
                      comment, &istat);
    if(istat){  sprintf(trnfits->stdkeys.timfile, "  "); istat=0; }
    fits_read_key_str(com.fp, "ATT_FILE", trnfits->stdkeys.attfile, 
                      comment, &istat);
    if(istat){  sprintf(trnfits->stdkeys.attfile, "  "); istat=0; }
    fits_read_key_str(com.fp, "ORB_FILE", trnfits->stdkeys.orbfile, 
                      comment, &istat);
    if(istat){  sprintf(trnfits->stdkeys.orbfile, "  "); istat=0; }
    fits_read_key_str(com.fp, "LEAPFILE", trnfits->stdkeys.leapfile, 
                      comment, &istat);
    if(istat){  sprintf(trnfits->stdkeys.leapfile, "  "); istat=0; }
    fits_read_key_str(com.fp, "TELDEF", trnfits->stdkeys.teldef, 
                      comment, &istat);
    if(istat){  sprintf(trnfits->stdkeys.teldef, "  "); istat=0; }

    fits_read_key_dbl(com.fp, "TSTART", &trnfits->stdkeys.tstart, 
		      comment, &istat);
    fits_read_key_dbl(com.fp, "TSTOP",  &trnfits->stdkeys.tstop,  
		      comment, &istat);
    fits_read_key_dbl(com.fp, "TELAPSE", &trnfits->stdkeys.telapse, 
		      comment, &istat);
    fits_read_key_dbl(com.fp, "ONTIME",  &trnfits->stdkeys.ontime,  
		      comment, &istat);

    fits_read_key_str(com.fp, "OBJECT", trnfits->stdkeys.object, 
                      comment, &istat);
    fits_read_key_str(com.fp, "OBSERVER", trnfits->stdkeys.observer, 
                      comment, &istat);
    fits_read_key_str(com.fp, "OBS_MODE", trnfits->stdkeys.obs_mode, 
                      comment, &istat);
    fits_read_key_str(com.fp, "OBS_ID", trnfits->stdkeys.obs_id, 
                      comment, &istat);
    fits_read_key_str(com.fp, "DATE-OBS", trnfits->stdkeys.date_obs, 
                      comment, &istat);
    fits_read_key_str(com.fp, "TIME-OBS", trnfits->stdkeys.time_obs, 
                      comment, &istat);
    fits_read_key_str(com.fp, "DATE-END", trnfits->stdkeys.date_end, 
                      comment, &istat);
    fits_read_key_str(com.fp, "TIME-END", trnfits->stdkeys.time_end, 
                      comment, &istat);

    trnfits->stdkeys.write_radec = 1;
    fits_read_key_dbl(com.fp, "RA_OBJ", &trnfits->stdkeys.ra_obj, 
                      comment, &istat);
    if(istat) { trnfits->stdkeys.ra_obj = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "DEC_OBJ", &trnfits->stdkeys.dec_obj, 
                      comment, &istat);
    if(istat) { trnfits->stdkeys.dec_obj = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "RA_PNT", &trnfits->stdkeys.ra_pnt, 
                      comment, &istat);
    if(istat) { trnfits->stdkeys.ra_pnt = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "DEC_PNT", &trnfits->stdkeys.dec_pnt, 
                      comment, &istat);
    if(istat) { trnfits->stdkeys.dec_pnt = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "RA_NOM", &trnfits->stdkeys.ra_nom, 
                      comment, &istat);
    if(istat) { trnfits->stdkeys.ra_nom = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "DEC_NOM", &trnfits->stdkeys.dec_nom, 
                      comment, &istat);
    if(istat) { trnfits->stdkeys.dec_nom = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "PA_NOM", &trnfits->stdkeys.roll_nom, 
                      comment, &istat);
    if(istat) { trnfits->stdkeys.roll_nom = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "MEAN_EA1", &trnfits->stdkeys.mean_ea1, 
                      comment, &istat);
    if(istat) { trnfits->stdkeys.mean_ea1 = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "MEAN_EA2", &trnfits->stdkeys.mean_ea2, 
                      comment, &istat);
    if(istat) { trnfits->stdkeys.mean_ea2 = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "MEAN_EA3", &trnfits->stdkeys.mean_ea3, 
                      comment, &istat);
    if(istat) { trnfits->stdkeys.mean_ea3 = 0.0; istat=0; }

    hxdFitsHeader_writeHXDStdKeys(trnfits->fits_fp, trnfits->stdkeys, 1, &istat);
    if(istat){
      fprintf(stderr, "%s:hxdFitsHeader_writeHXDStdKeys failed (%d)\n ",
	      pname, istat);
      return ANL_NG;
    }

    /* add a CHANBIN keyword indicating 'PH' channel binning */
    sprintf( buff, "PH" );
    if ( fits_update_key_str( trnfits->fits_fp, "CHANBIN", buff,
                             "WAM channel binning type (PH or TH)", &istat ) ) {
        fprintf( stderr, "%s:fits_update_key_str( CHANBIN ) failed (%d)\n",
                 pname, istat );
        return ANL_NG;
    }

    istat = writeHistory_wamlc();
    if(istat != ANL_OK) { /** error **/
      fprintf(stderr, "%s: Error in writing history\n", pname);
      return istat;
    } else {              /** continue **/
      istat = 0;
    }

    hxdFitsHeader_updateStdTimeKeys(trnfits->fits_fp, trnfits->stdkeys, 1, &istat);
    if(istat){
      fprintf(stderr, "%s:hxdFitsHeader_updateStdTimeKeys failed (%d)\n ",
	      pname, istat);
      return ANL_NG;
    }

    /*hxdFitsHeader_writeTIMEDEL(trnfits->fits_fp, 1, timedel);
    if(istat){
      fprintf(stderr, "%s:hxdFitsHeader_writeTIMEDEL failed \n ", pname);
      return ANL_NG;
    }*/

    /* Remove TIMEDEL keyword from first (primary) extension if it exists. */

    fits_movabs_hdu(trnfits->fits_fp, 1, &hdutype, &istat);
    if (istat) { 
      fprintf(stderr, "%s:(fits_movabs_hdu) primary array not found (%d)\n ", pname, istat);
      return ANL_NG;
   }

    fits_read_keyword(trnfits->fits_fp, "TIMEDEL", keywordvalue, comment, &istat);
    if (istat == KEY_NO_EXIST) { /* Do nothing if TIMEDEL keyword isn't present. */
      istat = 0;
    } else if (istat != 0) { /* Report error if other error status is found. */
      fprintf(stderr, "%s:fits_read_keyword TIMEDEL failed (%d)\n ", pname, istat);
      return ANL_NG;
    } else { /* Delete TIMEDEL keyword. */
      fits_delete_key(trnfits->fits_fp, "TIMEDEL", &istat);
      
      if (istat) { 
      fprintf(stderr, "%s:fits_delete key TIMEDEL failed (%d)\n ", pname, istat);
      }
    }
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_update_key_str failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_date(trnfits->fits_fp,&istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_date for header failed (%d)\n", pname, istat);
    return ANL_NG;
  } /* else {
    fits_write_chksum(trnfits->fits_fp,&istat);    
  } */

  /** Create 1st extention **/

/*  if ( istat ){
    fprintf(stderr, "%s:fits_write_chksum for header failed (%d)\n", pname, istat);
    return ANL_NG;
  } */ else {
    fits_create_hdu(trnfits->fits_fp, &istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_creat_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_btblhdr(trnfits->fits_fp, timebin, nc, ttype, tform, tunit, 
		     "RATE", pcount, &istat); 
  }

  /** Write keywords in 1st extention **/

  if ( istat ){
    fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    hxdFitsHeader_writeHXDStdKeys(trnfits->fits_fp, trnfits->stdkeys, 2, &istat);
    if(istat){
      fprintf(stderr, "%s:hxdFitsHeader_writeHXDStdKeys failed (%d)\n ",
	      pname, istat);
      return ANL_NG;
    }

    /* add a CHANBIN keyword indicating 'PH' channel binning */
    sprintf( buff, "PH" );
    if ( fits_update_key_str( trnfits->fits_fp, "CHANBIN", buff,
                             "WAM channel binning type (PH or TH)", &istat ) ) {
        fprintf( stderr, "%s:fits_update_key_str( CHANBIN ) failed (%d)\n",
                 pname, istat );
        return ANL_NG;
    }

    istat = writeHistory_wamlc();
    if(istat != ANL_OK) { /** error **/
      fprintf(stderr, "%s: Error in writing history\n", pname);
      return istat;
    } else {              /** continue **/
      istat = 0;
    }

    hxdFitsHeader_updateStdTimeKeys(trnfits->fits_fp, trnfits->stdkeys, 2, &istat);
    if(istat){
      fprintf(stderr, "%s:hxdFitsHeader_updateStdTimeKeys failed (%d)\n ",
	      pname, istat);
      return ANL_NG;
    }

    hxdFitsHeader_writeTIMEDEL(trnfits->fits_fp, 2, timedel);
    if(istat){
      fprintf(stderr, "%s:hxdFitsHeader_writeTIMEDEL failed \n ", pname);
      return ANL_NG;
    }
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {

    fits_update_key_dbl(trnfits->fits_fp, "TIMEPIXR", 0.5, 10,
                       "Timestamps give center of bin", &istat);

    log_val = 0;
    fits_update_key_log(trnfits->fits_fp, "BACKAPP", log_val, 
		       "background is subtracted", &istat);
    if ( trnfits->dtcor ) {
        log_val = 1;
    }
    fits_update_key_log(trnfits->fits_fp, "DEADAPP", log_val, 
		       "deadtime correction applied", &istat);
    log_val = 0;
    fits_update_key_log(trnfits->fits_fp, "VIGNAPP", log_val, 
		       "vignetting or collimator applied", &istat);
    fits_update_key_log(trnfits->fits_fp, "GAINAPP", log_val, 
		       "Gain all ready subracted", &istat);
    log_val = 1;
    fits_update_key_log(trnfits->fits_fp,"CLOCKAPP", log_val, 
		       "Clock correction applied", &istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_update_key_str failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_date(trnfits->fits_fp,&istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_date for 1st extension failed (%d)\n", pname, istat);
    return ANL_NG;
  } /* else {
    fits_write_chksum(trnfits->fits_fp,&istat);    
  } */

  /** Write keywords in 2nd extention **/
/*if ( istat ){
    fprintf(stderr, "%s:fits_write_chksum for 1st extention failed (%d)\n", pname, istat);
    return ANL_NG;
  } */ else {
    fits_create_hdu(trnfits->fits_fp, &istat);
  }

  trnfits->stdkeys.hduclass_ev_hk = HXD_FITS_HEADER_UTIL_HDUCLASS_HK;

  if ( istat ){
    fprintf(stderr, "%s:fits_creat_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    hxdgtiFits_finalizeGTI(&trnfits->gti, trnfits->stdkeys.tstart, trnfits->stdkeys.tstop);
  }
  if ( istat ){
    fprintf(stderr, "%s:hxdgtiFits_FinalizeGTI for 2nd extension failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    hxdgtiFits_createGTIxtention(trnfits->fits_fp, 3, trnfits->gti, trnfits->stdkeys, &istat);
  }
 
  
  if ( istat ){
    fprintf(stderr, "%s:hxdgtiFits_createGTIxtention for 2nd extension failed (%d)\n", pname, istat);
    return ANL_NG;
  }   

  istat = writeHistory_wamlc();
  if(istat != ANL_OK) { /** error **/
    fprintf(stderr, "%s: Error in writing history\n", pname);
    return istat;
  } else {              /** continue **/
    istat = 0;
  }

  fits_write_date(trnfits->fits_fp,&istat);
  if ( istat ){
    fprintf(stderr, "%s:fits_write_date for 2nd extension failed (%d)\n", 
	    pname, istat);
    return ANL_NG;
  } /* else {
    fits_write_chksum(trnfits->fits_fp,&istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_chksum for 2nd extension failed (%d)\n", 
	    pname, istat);
    return ANL_NG;
  }  */
  
  return ANL_OK;
}

static int closelcFITS(){

  int istat = 0;
  fits_close_file(trnfits->fits_fp, &istat);
  return ANL_OK;

}

static int mktrnlcFITS(int board, int ph_mode, int datanum){
  int istat = 0;
  int status;
  int hdutype;
  int i;
  char lcname[FILENAME_MAX_LENGTH*2];
  int hduid;

  if(ph_mode == 1){ 

    /** Decision of the file name **/
	sprintf(lcname, "%s_wam%d_ph%d_%d.lc", 
		com.newfilename, board, trnfits->chmin, trnfits->chmax);

	/** Write the header of fits **/
        if( createlcFITS(lcname, board, trnfits->chmin, trnfits->chmax,
                         lc->integ_ph_time[board], datanum) ){
	  fprintf(stderr,"%s: fits creation failed (%d)\n", pname, istat);
          status = ANL_QUIT;
          return status;
        }

	/*Fill Light Curve Column */
	fits_movabs_hdu(trnfits->fits_fp,2,&hdutype,&istat);
	if(istat){
	  fprintf(stderr,"%s: movabs hdu failed (%d)\n", pname, istat);
	  status = ANL_QUIT;
	  return status;
	}
	fits_write_col_dbl(trnfits->fits_fp,1,1,1,datanum, trnfits->time,
                           &istat);
	if(istat){
	  fprintf(stderr,"%s: write_col_dbl TIME failed (%d)\n", pname, istat);
	  status = ANL_QUIT;
	  return status;
	}
	fits_write_col_dbl(trnfits->fits_fp,2,1,1,datanum, 
			   lc->trn_ph_rate[board], &istat);
	if(istat){
	  fprintf(stderr,"%s: write_col_dbl RATE failed (%d)\n", pname, istat);
	  status = ANL_QUIT;
	  return status;
	}

	fits_write_col_dbl(trnfits->fits_fp,3,1,1,datanum, 
			   lc->trn_ph_rate_err[board], &istat);
	if(istat){
	  fprintf(stderr,"%s: write_col_dbl ERROR failed (%d)\n", pname, istat);
	  status = ANL_QUIT;
	  return status;
	}

	fits_write_col_dbl(trnfits->fits_fp,4,1,1,datanum,
                           lc->trn_ph_fexp[board], &istat);
	if(istat){
	  fprintf(stderr,"%s: write_col_dbl FRACEXP failed (%d)\n", pname,
                  istat);
	  status = ANL_QUIT;
	  return status;
	}

	fits_write_col_dbl(trnfits->fits_fp,5,1,1,datanum,
                           lc->trn_ph_deadc[board], &istat);
	if(istat){
	  fprintf(stderr,"%s: write_col_dbl DEADC failed (%d)\n", pname,
                  istat);
	  status = ANL_QUIT;
	  return status;
	}

	/** Write checksum **/
	for(hduid=1;hduid<=3;hduid++){
	  fits_movabs_hdu(trnfits->fits_fp,hduid,&hdutype,&istat);
	  if(istat){
	    fprintf(stderr,"%s: movabs hdu failed (%d)\n", pname, istat);
	    status = ANL_QUIT;
	    return status;
	  }
	  fits_write_chksum(trnfits->fits_fp, &istat);
	  if(istat){
	    fprintf(stderr,"%s: update checksum failed (%d)\n", pname, istat);
	    status = ANL_QUIT;
	    return status;
	  }
	}

	/** Close the Fits **/
	if( closelcFITS() ){
	  fprintf(stderr,"%s: fits close failed (%d)\n", pname, istat);
	  status = ANL_QUIT;
          return status;
	}
  } else {
	  status = ANL_QUIT;
	  return status;
  }

  status = ANL_OK;
  return status;
}

double calc_fracexpo( double start, double stop, double binexpo, HXD_GTI gti ) {

    int i = 0;
    double expo = 0.0;
    double fexp = 0.0;

    /* get the total exposure between start and stop covered by the GTI */
    while ( i < gti.row ) {
        /* overlap */
        if ( gti.stop[ i ] > start && gti.start[ i ] < stop ) {
            if ( gti.start[ i ] >= start && gti.stop[ i ] <= stop ) {
                expo += gti.stop[ i ] - gti.start[ i ];
            } else if ( gti.start[ i ] >= start ) {
                expo += stop - gti.start[ i ];
            } else if ( gti.stop[ i ] <= stop ) {
                expo += gti.stop[ i ] - start;
            } else {
                expo += binexpo;
                break;
            }
        }
        i++;
    }

    /* get the fractional exposure */
    if ( binexpo > 0.0 && binexpo >= expo ) {
        fexp = expo / binexpo;
    } else {
        fexp = 1.0;
    }
    return fexp;
}

int time_in_gti( double time, HXD_GTI gti ) {
    int i = 0;
    while ( i < gti.row ) {
        if ( time >= gti.start[ i ] && time <= gti.stop[ i ] ) {
            return 1;
        }
        i++;
    }
    return 0;
}
    
void
HXDmktrnlc_startup(int *status)
{

  /*
    printf("%d+%d bytes", sizeof(HxdMkTrnLC_lc), sizeof(HxdMkTrnLC_trnfits));
  */

  if( lc == NULL){
    lc = (HxdMkTrnLC_lc*) malloc( sizeof(HxdMkTrnLC_lc) );
  } else {
    fprintf(stderr, "%s: memory, lc, is alread allocated\n", pname);
    *status = ANL_NG;
    return;
  }

  if( trnfits == NULL){
    trnfits = (HxdMkTrnLC_trnfits*) malloc( sizeof(HxdMkTrnLC_trnfits) );
  } else {
    fprintf(stderr, "%s: memory, trnfits, is alread allocated\n", pname);
    *status = ANL_NG;
    return;
  }

  *status = ANL_OK;
}

void
HXDmktrnlc_com(int *status)
{

  *status = PILGetString("origin", com.origin);
  if (*status) {
    fprintf(stderr, "%s: PILGetString origin error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetString("outroot", com.newfilename);
  if (*status) {
    fprintf(stderr, "%s: PILGetString Output root error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetInt("tpu_board", &trnfits->tpu_board);
  if ( *status || trnfits->tpu_board<-1 || trnfits->tpu_board>3 ) {
    fprintf(stderr, "%s: PILGetInt TPU board error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  /** ph_mode (PH or other counters)**/
 *status = PILGetInt("ph_mode", &trnfits->ph_mode);
  if ( *status ) {
    fprintf(stderr, "%s: PILGetInt ph_mode error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  /** trnfits->chmin =< ch <= trnfits->chmax **/
  *status = PILGetInt("min_channel", &trnfits->chmin);
  if (*status || trnfits->chmin<0 || trnfits->chmin>54) {
    fprintf(stderr, "%s: PILGetInt min_channel error (%d)\n",
            pname, *status );
	    *status = ANL_QUIT;
    exit(-1);
	}	

  *status = PILGetInt("max_channel", &trnfits->chmax);
  if (*status || trnfits->chmax<0 || trnfits->chmax>54){
    fprintf(stderr, "%s: PILGetInt min_channel error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

   *status = PILGetBool("dt_cor", &trnfits->dtcor);
  if ( *status ) {
    fprintf(stderr, "%s: PILGetBool dt_cor error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetReal("dt_clk", &trnfits->dt_clk);
  if ( *status ) {
    fprintf(stderr, "%s: PILGetReal dt_clk error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  if (trnfits->chmin > trnfits->chmax ){
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = ANL_OK;

}    
 
void
HXDmktrnlc_init(int *status)
{
  int board, tbin;
  for(board=0;board<HXD_TRN_BOARD_NUM;board++){
    lc->data_num[board] = 0;
    trnfits->ph_time_bin[board] = 0;
    for(tbin=0;tbin<TIMEBIN_MAX_NUM; tbin++){
      lc->delta_trn_ph[board][tbin] = 0;
    }
  }

    *status = ANL_OK;
}

void
HXDmktrnlc_his(int *status)
{
    *status = ANL_OK;
}

void
HXDmktrnlc_bgnrun(int *status)
{
  int istat = 0;
  int size;
  char filename[256];
  fitsfile *fp;

  BnkfGetM("HXDtrnFitsRead:FILE_P", sizeof(com.fp), &size, &com.fp);
  BnkfGetM("HXDtrnFitsRead:GTI", sizeof(HXD_GTI), &size, &trnfits->gti);

  BnkDef("ASTE:FFF_ORIGIN", sizeof(com.origin));
  BnkPut("ASTE:FFF_ORIGIN", strlen(com.origin)+1, com.origin);

  *status = ANL_OK;       
}

void
HXDmktrnlc_ana(int nevent, int eventid, int *status)
{
  int size;
  int istat = 0;
  double ev_time;
  double s_time;
  int trn_iblock;  
  int trn_ae_de_length_chk;
  int trn_de_module;
  int trn_de_block;
  int trn_rdbin;
  int trn_tblid;        /** add **/
  int trn_sys_err_code; /** add **/
  int trn_data_size;
  int trn_ae_block;
  int trn_gb_flg;
  int trn_time_mode;
  int trn_rbm;
  int trn_gb_frz;
  int trn_dt_mode;
  int trn_sumld_mode;
  int trn_ae_module;
  int trn_gb_trg;
  int trn_ph[HXD_TRN_MAX_ENE_BIN+1];
  int trn_pi[HXD_TRN_MAX_ENE_BIN+1];
  int trn_over_flow;
  int trn_pseudo;
  int trn_t_ant[5];
  int trn_ud;
  int trn_dead_time;
  int trn_sum_ld;
  int trn_w_ant[4];
  int board,i;
  double dtcnt[HXD_TRN_BOARD_NUM], delta_dtcnt[HXD_TRN_BOARD_NUM];
  double lt_frac[HXD_TRN_BOARD_NUM], exposure[HXD_TRN_BOARD_NUM];
  double fracexpo;
  int carry_dtcnt_flg[HXD_TRN_BOARD_NUM];

  static int first_in_gti[HXD_TRN_BOARD_NUM];

  hxdtrnFitsToBnk_get( &data );

  BnkfGetM("HXD:TRN:EV_TIME", sizeof(double), &size, &ev_time);
  BnkfGetM("HXD:TRN:PACKET_S_TIME", sizeof(double), &size, &s_time);
  BnkfGetM("HXD:TRB:IBLOCK", sizeof(int), &size, &trn_iblock );
  BnkfGetM("HXD:TRN:BOARD", sizeof(int), &size, &trn_de_module );
  BnkfGetM("HXD:TRN:BLOCK", sizeof(int), &size, &trn_de_block );
  BnkfGetM("HXD:TRN:RDBIN", sizeof(int), &size, &trn_rdbin );
  BnkfGetM("HXD:TRN:TBLID", sizeof(int), &size, &trn_tblid );

  BnkfGetM("HXD:TRN:DATA_SIZE",sizeof(int), &size, &trn_data_size );
  BnkfGetM("HXD:TRN:SYS_ERR_CODE",sizeof(int), &size, &trn_sys_err_code );

  BnkfGetM("HXD:TRH:BLOCK", sizeof(int), &size, &trn_ae_block );
  BnkfGetM("HXD:TRH:GB_FLG", sizeof(int), &size, &trn_gb_flg );
  BnkfGetM("HXD:TRH:TIME_MODE", sizeof(int), &size, &trn_time_mode );
  BnkfGetM("HXD:TRH:RBM", sizeof(int), &size, &trn_rbm);
  BnkfGetM("HXD:TRH:GB_FRZ", sizeof(int), &size, &trn_gb_frz );
  BnkfGetM("HXD:TRH:DT_MODE", sizeof(int), &size, &trn_dt_mode );
  BnkfGetM("HXD:TRH:SUMLD_MODE", sizeof(int), &size, &trn_sumld_mode );
  BnkfGetM("HXD:TRH:BOARD", sizeof(int), &size, &trn_ae_module );
  BnkfGetM("HXD:TRH:GB_TRG", sizeof(int), &size, &trn_gb_trg );

  BnkfGetM("HXD:TRB:PH", sizeof(int)*HXD_TRN_MAX_ENE_BIN, &size, trn_ph);
  BnkfGetM("HXD:TRB:OVER_FLOW", sizeof(int), &size, &trn_over_flow );
  BnkfGetM("HXD:TRB:PSEUDO", sizeof(int), &size, &trn_pseudo );
  BnkfGetM("HXD:TRB:TRN_ANT", sizeof(int)*5, &size, trn_t_ant );
  BnkfGetM("HXD:TRB:UD", sizeof(int), &size, &trn_ud );
  BnkfGetM("HXD:TRB:DEAD_TIME", sizeof(int), &size, &trn_dead_time );
  BnkfGetM("HXD:TRB:SUM_LD", sizeof(int), &size, &trn_sum_ld );
  BnkfGetM("HXD:TRB:WELL_ANT", sizeof(int)*4, &size, trn_w_ant );
 
  if(trn_de_module == trn_ae_module){
    if(trn_de_module>HXD_TRN_BOARD_NUM){
      fprintf(stderr, "%s: TPU module id error %d\n", pname, trn_de_module);
      *status=ANL_QUIT;    return;
    }
    board = (trn_ae_module)&0x03;
  } else {
    fprintf(stderr, "%s: TPU module id mismatch (DE:%d, AE:%d)\n",
	    pname, trn_de_module, trn_ae_module);
    *status=ANL_QUIT;    return;
  }

  lc->integ_ph_time[board] = 0.5 * pow(2.0,(double)trn_time_mode)* (trn_ae_block/trn_de_block);; 
  dtcnt[board] = trn_dead_time;

  if ( lc->data_num[board] == 0 ){
    /*The first time bin */
    lc->pre_dtcnt[board] = trn_dead_time;
    delta_dtcnt[board]  = 0.0;
    first_in_gti[board]  = 1;
    
  } else {
    /* Judge whether TRN Deadtime counter is carried or not */
    if( dtcnt[board] < lc->pre_dtcnt[board] ){
      carry_dtcnt_flg[board] = 1;
    } else {
      carry_dtcnt_flg[board] = 0;
    }
   /* Substract the counter value from previous one*/
    delta_dtcnt[board] = dtcnt[board]
      + (carry_dtcnt_flg[board]<<16) - lc->pre_dtcnt[board];
   
    lc->pre_dtcnt[board] = dtcnt[board];

  }

  fracexpo = calc_fracexpo( ev_time - lc->integ_ph_time[board], ev_time,
                            lc->integ_ph_time[board], trnfits->gti );
  if (trnfits->dtcor) {
      lt_frac[board] = 1.0 - delta_dtcnt[board]*trnfits->dt_clk;
  } else if (!trnfits->dtcor){ 
      lt_frac[board] = 1.0; 
  }
  exposure[board] = lt_frac[board] * lc->integ_ph_time[board] * fracexpo;

  /*
  ** Calculate whether this bin is the 1st in a GTI - This is required for dead
  ** time correction. Bins that are at the leading edge of a GTI are not
  ** counted, but they are used to account for the dead time.
  */
  first_in_gti[board] = 0;
  i = 0;
  while ( i < trnfits->gti.row ) {
      if ( ev_time>=1.0 &&
           ev_time-1.5*lc->integ_ph_time[board]<trnfits->gti.start[i] &&
           ev_time-0.5*lc->integ_ph_time[board]>=trnfits->gti.start[i] ) {
          first_in_gti[board] = 1;
          break;
      }
      i++;
  }

  if (ev_time>=1.0 && lc->data_num[board]>0 && exposure[board]>0.0 && !first_in_gti[board] &&
      time_in_gti(ev_time-0.5*lc->integ_ph_time[board], trnfits->gti)) {

      trnfits->time[trnfits->ph_time_bin[board]]= ev_time - 0.5*lc->integ_ph_time[board];

      lc->trn_ph_fexp[board][trnfits->ph_time_bin[board]] = fracexpo;
      lc->trn_ph_deadc[board][trnfits->ph_time_bin[board]] = 1.0-delta_dtcnt[board]*trnfits->dt_clk;

      if(trnfits->ph_time_bin[board] >= TIMEBIN_MAX_NUM){
          fprintf(stderr, "%s: time bin exceeds %d\n", pname, TIMEBIN_MAX_NUM);
          *status = ANL_OK;
          return;
      }

      /** overflow bin is taken as PH 54 channel data **/
      trn_ph[HXD_TRN_MAX_ENE_BIN] = trn_over_flow;

      /** added counts for trnfits->chmin <ch< trnfits->chmax **/
      for ( i=trnfits->chmin; i<trnfits->chmax+1; i++){
          lc->delta_trn_ph[board][trnfits->ph_time_bin[board]] += trn_ph[i];
      }				
      /** Calculate the count rate **/
      lc->trn_ph_rate[board][trnfits->ph_time_bin[board]] 
          = (double)lc->delta_trn_ph[board][trnfits->ph_time_bin[board]]/exposure[board];

      /** Calculate the statistical error assuming Poisson distribution **/    
      lc->trn_ph_rate_err[board][trnfits->ph_time_bin[board]] = 
          sqrt((double)lc->delta_trn_ph[board][trnfits->ph_time_bin[board]])/exposure[board];

      trnfits->ph_time_bin[board] ++;/** Number of time bins **/

  }

  lc->data_num[board]++;
  *status = ANL_OK;
}

void
HXDmktrnlc_endrun(int *status)
{    
  int istat = 0;
  int board;
  int th;
  int stat;
  
  if (trnfits->tpu_board == -1){
    for (board=0; board<HXD_TRN_BOARD_NUM; board++){      
      stat = mktrnlcFITS(board,trnfits->ph_mode,trnfits->ph_time_bin[board]);
      if(stat != ANL_OK){
	fprintf(stderr,"%s: error in making trnlcFITS TPU-%d\n", pname, board);
	*status = ANL_NG;
	return;
      }
    }
  } else if((trnfits->tpu_board>=0)&&(trnfits->tpu_board<=3)){
    board = trnfits->tpu_board;
    stat =  mktrnlcFITS(board,trnfits->ph_mode,trnfits->ph_time_bin[board]);
    if(stat != ANL_OK){
      fprintf(stderr,"%s: error in making trnlcFITS TPU-%d\n", pname, board);
      *status = ANL_NG;
      return;
    }
  } else{
    fprintf(stderr, "board id mismuched.(%d)\n", trnfits->tpu_board);
  }
  
  *status = ANL_OK;
}

void
HXDmktrnlc_exit(int *status)
{
    *status = ANL_OK;
}
