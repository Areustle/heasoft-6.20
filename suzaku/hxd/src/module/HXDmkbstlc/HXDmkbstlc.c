/**********************************************
HXDmkbstlc: light curve ftools for the BST data

ver 0.1.4 K.Yamaoka 2006 Jul. 26
   modify the comment in the fits_read_key
ver 0.1.5 Y.Terada  2006 Jul. 27
   write parameters in FITS headers
ver 2.0.0 Y.Terada  2006 Sep. 15
   version 2.0 format, with backword compatibility
ver 2.0.1 Y.Terada  2008 Nov. 1
   write checksum
ver 2.1.0 K.Yamaoka 2009 May 14
   Support deadtime-correction
ver 2.2.0 K.Yamaoka 2009 May 14
   Support PH data with given energies
ver 2.2.1 K.Yamaoka 2009 June 14
   bug fix for PH data light curve
ver 2.2.2 C.Padgett 2011 March 24
   Added keywords to output file
   Changed fits_write to fits_update
ver 2.2.2wam K.Yamaoka 2010 Jan 30
   printf (from ver 2.2.1 independently) 
ver 2.2.3 Y.Terada 2011 August 11 
   Debug checksum of 1st extension (from ver 2.2.2)
ver 2.2.4 Y.Terada 2013 Oct 21
   Merge v2.2.2wam, updated from ver 2.2.3
   Add return values
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

#include "hxdbstFitsUtil.h"
#include "hxdbstFitsToBnkUtil.h"
#include "hxdFitsHeaderUtil.h"
#include "hxdgtiFitsUtil.h"
#include "pil.h"
#include "hxdeventFitsUtil.h"

#define HXD_TRN_BOARD_NUM 4
#define HXD_BST_TH_NUM 4
#define HXD_BST_PH_NUM 55
#define HXD_BST_TOTAL_PH_TIME_BIN 128
#define HXD_BST_TOTAL_TH_TIME_BIN 4096
#define FILENAME_MAX_LENGTH 256
#define DEBUG 0

HxdBstFits data;
char HXDmkbstlc_version[] = "2.2.3";
                                
static char pname[] = "HXDmkbstlc";

static char *ttype[] = {
  "TIME", "RATE", "ERROR", "FRACEXP"
};

static char *tform[] = {
  "1D", "1D", "1D", "1D"
};

static char *tunit[] = {
  "s", "counts/s", "counts/s", ""
};
/*
static char *keyword[] = {
  "TTYPE1", "TTYPE2", "TTYPE3", "TTYPE4"
};
*/

/*static double bst_time[HXD_BST_TOTAL_TH_TIME_BIN];*/
static double integ_ph_time[HXD_TRN_BOARD_NUM]; /* in unit of sec */
static double integ_th_time[HXD_TRN_BOARD_NUM]; /* in unit of sec */

static int delta_dtcnt[HXD_TRN_BOARD_NUM], pre_dtcnt[HXD_TRN_BOARD_NUM];
static int delta_bst_ph[HXD_TRN_BOARD_NUM][HXD_BST_PH_NUM][HXD_BST_TOTAL_PH_TIME_BIN];
static double bst_ph_rate[HXD_TRN_BOARD_NUM][HXD_BST_PH_NUM][HXD_BST_TOTAL_PH_TIME_BIN];
static double bst_ph_rate_err[HXD_TRN_BOARD_NUM][HXD_BST_PH_NUM][HXD_BST_TOTAL_PH_TIME_BIN];
static double bst_sum_ph_rate[HXD_TRN_BOARD_NUM][HXD_BST_TOTAL_PH_TIME_BIN];
static double bst_sum_ph_rate_err[HXD_TRN_BOARD_NUM][HXD_BST_TOTAL_PH_TIME_BIN];

static int delta_bst_th[HXD_TRN_BOARD_NUM][HXD_BST_TH_NUM][HXD_BST_TOTAL_TH_TIME_BIN];
static int delta_bst_th_all[HXD_TRN_BOARD_NUM][HXD_BST_TOTAL_TH_TIME_BIN];
static double bst_th_rate[HXD_TRN_BOARD_NUM][HXD_BST_TH_NUM][HXD_BST_TOTAL_TH_TIME_BIN];
static double bst_th_rate_err[HXD_TRN_BOARD_NUM][HXD_BST_TH_NUM][HXD_BST_TOTAL_TH_TIME_BIN];
static double bst_sum_th_rate[HXD_TRN_BOARD_NUM][HXD_BST_TOTAL_TH_TIME_BIN];
static double bst_sum_th_rate_err[HXD_TRN_BOARD_NUM][HXD_BST_TOTAL_TH_TIME_BIN];
static double bst_th_all_rate[HXD_TRN_BOARD_NUM][HXD_BST_TOTAL_TH_TIME_BIN];
static double bst_th_all_rate_err[HXD_TRN_BOARD_NUM][HXD_BST_TOTAL_TH_TIME_BIN];
static int pre_bst_th[HXD_TRN_BOARD_NUM][HXD_BST_TH_NUM];
static int carry_bst_th_flg[HXD_TRN_BOARD_NUM][HXD_BST_TH_NUM];

static double pseudo_th_time[HXD_BST_TOTAL_TH_TIME_BIN];
static double pseudo_ph_time[HXD_BST_TOTAL_PH_TIME_BIN];

static struct {
  char filename[FILENAME_MAX_LENGTH];
  char newfilename[FILENAME_MAX_LENGTH];
  fitsfile *fp;
  int irow;
  int iomode;
  char origin[256];
  int format_version;
}com;

static fitsfile *fits_fp;
static int tpu_board, th_mode, ene_mode, ch, chmin, chmax, dtcor;
static double dt_clk, dt_ph;
static double frz_tm[HXD_TRN_BOARD_NUM];
static double on_time[HXD_TRN_BOARD_NUM] = {0.0,0.0,0.0,0.0};
static HXD_GTI gti;
static HXD_STD_KEYS stdkeys;

/*
static struct{
  int colnum[HXD_BST_FITS_KEY_NUM];
  long nrow;
} fits;
*/

static int writeHistory_bstlc ( void ){
  int istat = 0;
  int status = ANL_OK;
  char history [PIL_LINESIZE];

  sprintf(history, "    ------- parameters in %s -------   ", pname);
  fits_write_history(fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  sprintf(history, "         outroot = %s", com.newfilename);
  fits_write_history(fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  if(tpu_board==-1){
    sprintf(history, "         tpu_board = all (0,1,2,3)");
  } else {
    sprintf(history, "         tpu_board = %d", tpu_board);
  }
  fits_write_history(fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }


  if(th_mode==1){
    sprintf(history, "         th_mode = TH data");
  } else if(th_mode==0){
    sprintf(history, "         th_mode = PH data");
  } else {
    sprintf(history, "         th_mode = unknown");
  }
  fits_write_history(fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  sprintf(history, "         origin = %s", com.origin);
  fits_write_history(fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  return status;
}

static int createlcFITS(char *fitsname, int board, int chmin, int chmax,
                        double timedel, int timebin, int th_mode){
  int istat = 0;
  int simple = 1;
  int bitpix = 8;
  int naxis = 0;
  long naxes[1];
  long pcount = 0;
  long gcount = 1;
/*int err_flag = 1;*/
  int extend = 1; /** Extension is included**/
  int nc = 4;
  int log_val;
  /*
  int hdutype, hdunum;
  int i, size; 
  int morekey  = 5; 
  */
  
  char buff[80];
  char tmp_card[81];
  /*
  char *tim_file;
  char tlm_name[FILENAME_MAX_LENGTH];
  char filename[FILENAME_MAX_LENGTH];
  char leapfile[1024];
  */
  char comment[80];

  fits_create_file(&fits_fp, fitsname, &istat);

  if ( istat ) {
    fprintf(stderr, "%s:fits_create_file failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_grphdr(fits_fp, simple, bitpix, naxis, naxes,
		      pcount, gcount, extend, &istat);
  } 

  if ( istat ) {
    fprintf(stderr, "%s:fits_write_grphdr failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {          

    hxdFitsHeader_mallocSTDKEYS(&stdkeys);
    hxdFitsHeader_setDefaultKeywordValues(&stdkeys);
    stdkeys.hduclass_ev_hk = HXD_FITS_HEADER_UTIL_HDUCLASS_LC;

    sprintf(stdkeys.datamode, "BURST"); 
    sprintf(buff, "ANL: %s: %s", pname, HXDmkbstlc_version);
    sprintf(stdkeys.creator, buff);
    /*    sprintf(stdkeys.origin, "%s", com.origin); */
    sprintf(stdkeys.instrument, "HXD");
    stdkeys.use_detnam=1; 
    sprintf(stdkeys.detnam, "WAM_ANTI%d", board);

    stdkeys.timepixr=0.5; /** latch values at the end of the TIME clock **/
    stdkeys.chanmin=chmin;
    stdkeys.chanmax=chmax;
    
    /* Credits, from ASTE_ANL (later than version 1.40)*/
    sprintf(stdkeys.credits,     anl_task_credits());
    sprintf(stdkeys.task_name,   anl_task_name()   );
    sprintf(stdkeys.task_version,anl_task_version());

    fits_read_key_flt(com.fp, "TIERRELA", &stdkeys.tierrela, 
		      comment, &istat);
    fits_read_key_flt(com.fp, "TIERABSO",  &stdkeys.tierabso,  
		      comment, &istat);

    fits_read_key_str(com.fp, "TLM_FILE", stdkeys.tlmfile, 
                      comment, &istat);
    if(istat){  sprintf(stdkeys.tlmfile, "  "); istat=0; }
    fits_read_key_str(com.fp, "TIM_FILE", stdkeys.timfile, 
                      comment, &istat);
    if(istat){  sprintf(stdkeys.timfile, "  "); istat=0; }
    fits_read_key_str(com.fp, "ATT_FILE", stdkeys.attfile, 
                      comment, &istat);
    if(istat){  sprintf(stdkeys.attfile, "  "); istat=0; }
    fits_read_key_str(com.fp, "ORB_FILE", stdkeys.orbfile, 
                      comment, &istat);
    if(istat){  sprintf(stdkeys.orbfile, "  "); istat=0; }
    fits_read_key_str(com.fp, "LEAPFILE", stdkeys.leapfile, 
                      comment, &istat);
    if(istat){  sprintf(stdkeys.leapfile, "  "); istat=0; }
    fits_read_key_str(com.fp, "TELDEF", stdkeys.teldef, 
                      comment, &istat);
    if(istat){  sprintf(stdkeys.teldef, "  "); istat=0; }

    /*
    fits_read_key_dbl(com.fp, "TSTART", &stdkeys.tstart, 
                      comment, &istat);
    fits_read_key_dbl(com.fp, "TSTOP",  &stdkeys.tstop,  
                      comment, &istat);
    fits_read_key_dbl(com.fp, "TELAPSE", &stdkeys.telapse, 
                      comment, &istat);
    fits_read_key_dbl(com.fp, "ONTIME",  &stdkeys.ontime,  
                      comment, &istat);
    */
    fits_read_key_str(com.fp, "OBJECT", stdkeys.object, 
                      comment, &istat);
    fits_read_key_str(com.fp, "OBSERVER", stdkeys.observer, 
                      comment, &istat);
    fits_read_key_str(com.fp, "OBS_MODE", stdkeys.obs_mode, 
                      comment, &istat);
    fits_read_key_str(com.fp, "OBS_ID", stdkeys.obs_id, 
                      comment, &istat);
    fits_read_key_str(com.fp, "DATE-OBS", stdkeys.date_obs, 
                      comment, &istat);
    fits_read_key_str(com.fp, "TIME-OBS", stdkeys.time_obs, 
                      comment, &istat);
    fits_read_key_str(com.fp, "DATE-END", stdkeys.date_end, 
                      comment, &istat);
    fits_read_key_str(com.fp, "TIME-END", stdkeys.time_end, 
                      comment, &istat);

    stdkeys.write_radec = 1;
    fits_read_key_dbl(com.fp, "RA_OBJ", &stdkeys.ra_obj, 
                      comment, &istat);
    if(istat) { stdkeys.ra_obj = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "DEC_OBJ", &stdkeys.dec_obj, 
                      comment, &istat);
    if(istat) { stdkeys.dec_obj = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "RA_PNT", &stdkeys.ra_pnt, 
                      comment, &istat);
    if(istat) { stdkeys.ra_pnt = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "DEC_PNT", &stdkeys.dec_pnt, 
                      comment, &istat);
    if(istat) { stdkeys.dec_pnt = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "RA_NOM", &stdkeys.ra_nom, 
                      comment, &istat);
    if(istat) { stdkeys.ra_nom = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "DEC_NOM", &stdkeys.dec_nom, 
                      comment, &istat);
    if(istat) { stdkeys.dec_nom = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "PA_NOM", &stdkeys.roll_nom, 
                      comment, &istat);
    if(istat) { stdkeys.roll_nom = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "MEAN_EA1", &stdkeys.mean_ea1, 
                      comment, &istat);
    if(istat) { stdkeys.mean_ea1 = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "MEAN_EA2", &stdkeys.mean_ea2, 
                      comment, &istat);
    if(istat) { stdkeys.mean_ea2 = 0.0; istat=0; }
    fits_read_key_dbl(com.fp, "MEAN_EA3", &stdkeys.mean_ea3, 
                      comment, &istat);
    if(istat) { stdkeys.mean_ea3 = 0.0; istat=0; }

    if (th_mode==1) {
    stdkeys.tstart = frz_tm[board] - on_time[board] + timedel;
    stdkeys.tstop = frz_tm[board];
    stdkeys.ontime = on_time[board] - timedel;
    } else if (th_mode==0){
    stdkeys.tstart = frz_tm[board] - on_time[board];
    stdkeys.tstop = frz_tm[board];
    stdkeys.ontime = on_time[board];
    }

    printf("%.8f %.8f\n",stdkeys.tstart, stdkeys.tstop);

    stdkeys.timedel = timedel;     

    hxdFitsHeader_writeHXDStdKeys(fits_fp, stdkeys, 1, &istat);    
    /* write CHANBIN keyword to indicate WAM binning type */
    if ( th_mode==1 ) {
        sprintf( tmp_card, "TH" );
        if ( fits_update_key_str( fits_fp, "CHANBIN", tmp_card,
                                  "WAM channel binning type (TH or PH)",
                                  &istat ) ) {
            fprintf(stderr, "%s: fits_update_key_str('CHANBIN') failed (%d)\n",
                    pname, istat );
            return istat;
        }
    } else if ( th_mode==0 ) {
        sprintf( tmp_card, "TH" );
        if ( fits_update_key_str( fits_fp, "CHANBIN", tmp_card,
                                  "WAM channel binning type (TH or PH)",
                                  &istat ) ) {
            fprintf(stderr, "%s: fits_update_key_str('CHANBIN') failed (%d)\n",
                    pname, istat );
            return istat;
        }
    }
    istat = writeHistory_bstlc();
    if(istat != ANL_OK) { /** error **/
      fprintf(stderr, "%s: Error in writing history\n", pname);
      return istat;
    } else {              /** continue **/
      istat = 0;
    }
    hxdFitsHeader_updateStdTimeKeys(fits_fp, stdkeys, 1, &istat);
    hxdFitsHeader_writeTIMEDEL(fits_fp, 1, timedel);

  }

  if ( istat ){
    fprintf(stderr, "%s:fits_update_key_str failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_date(fits_fp,&istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_date for 1st extension failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_chksum(fits_fp,&istat);    
  }

  /** Create 1st extention **/

  if ( istat ){
    fprintf(stderr, "%s:fits_write_date failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_create_hdu(fits_fp, &istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_creat_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_btblhdr(fits_fp, timebin, nc, ttype, tform, tunit, 
		     "RATE", pcount, &istat); 
  }

  /** Write the keyword in 1st extention **/

  if ( istat ){
    fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {

    hxdFitsHeader_writeHXDStdKeys(fits_fp, stdkeys, 2, &istat);
    /* write CHANBIN keyword to indicate WAM binning type */
    if ( th_mode==1 ) {
        sprintf( tmp_card, "TH" );
        if ( fits_update_key_str( fits_fp, "CHANBIN", tmp_card,
                                  "WAM channel binning type (TH or PH)",
                                  &istat ) ) {
            fprintf(stderr, "%s: fits_update_key_str('CHANBIN') failed (%d)\n",
                    pname, istat );
            return istat;
        }
    } else if ( th_mode==0 ) {
        sprintf( tmp_card, "TH" );
        if ( fits_update_key_str( fits_fp, "CHANBIN", tmp_card,
                                  "WAM channel binning type (TH or PH)",
                                  &istat ) ) {
            fprintf(stderr, "%s: fits_update_key_str('CHANBIN') failed (%d)\n",
                    pname, istat );
            return istat;
        }
    }
    istat = writeHistory_bstlc();
    if(istat != ANL_OK) { /** error **/
      fprintf(stderr, "%s: Error in writing history\n", pname);
      return istat;
    } else {              /** continue **/
      istat = 0;
    }
    hxdFitsHeader_updateStdTimeKeys(fits_fp, stdkeys, 2, &istat);
    hxdFitsHeader_writeTIMEDEL(fits_fp, 2, timedel);

  }

  if ( istat ){
    fprintf(stderr, "%s:fits_delete_key failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {

    log_val = 0;
    fits_update_key_log(fits_fp, "BACKAPP", log_val, 
		       "background is subtracted", &istat);

    if (dtcor == 1) {
      log_val = 1;
    }

    fits_update_key_log(fits_fp, "DEADAPP", log_val, 
		       "deadtime correction applied", &istat);

    log_val = 0;
    fits_update_key_log(fits_fp, "VIGNAPP", log_val, 
		       "vignetting or collimator applied", &istat);
    fits_update_key_log(fits_fp, "GAINAPP", log_val, 
		       "Gain all ready subracted", &istat);
    log_val = 1;
    fits_update_key_log(fits_fp,"CLOCKAPP", log_val, 
		       "Clock correction applied", &istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_update_key_str failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_date(fits_fp,&istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_date for 1st extension failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_chksum(fits_fp,&istat);    
  }

  /** Write the keywords in 2nd extention **/
   
  if ( istat ){
    fprintf(stderr, "%s:fits_update_key failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_create_hdu(fits_fp, &istat);
  }

  stdkeys.hduclass_ev_hk = HXD_FITS_HEADER_UTIL_HDUCLASS_HK;

  if ( istat ){
    fprintf(stderr, "%s:fits_creat_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    hxdgtiFits_finalizeGTI(&gti, stdkeys.tstart, stdkeys.tstop);
  }
    if ( istat ){
      fprintf(stderr, "%s:hxdgtiFits_FinalizeGTI for 2nd extension failed (%d)\n", pname, istat);
      return ANL_NG;
    } else {
      hxdgtiFits_createGTIxtention(fits_fp, 3, gti, stdkeys, &istat);
    }

  if ( istat ){
    fprintf(stderr, "%s:hxdgtiFits_createGTIxtention for 2nd extension failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
  fits_write_date(fits_fp,&istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_date for 2nd extension failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_chksum(fits_fp,&istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_chksum for 2nd extension failed (%d)\n", 
	    pname, istat);
    return ANL_NG;
  } 
  
  return ANL_OK;
}

static int closeFITS(){

  int istat = 0;
  int hdutype;
  int ext_id;


  for (ext_id=1; ext_id<=2; ext_id++){
    fits_movabs_hdu(fits_fp,ext_id,&hdutype,&istat);
    if ( istat ) {
      fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
	      pname, ext_id, istat);
      return ANL_NG;
    }
    
    fits_write_chksum(fits_fp, &istat);
    if ( istat ) {
      fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
      return ANL_NG;
    }
  }

  fits_close_file(fits_fp, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_close_file failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  return ANL_OK;

}

static void mkbstlcFITS(int board, int th_mode, int ene_mode, int *status){

  int istat = 0;
/*int *status; */
  int hdutype;
  int th, ph, i, j;
/*int chanmin = -1; */
/*int chanmax = -1; */
  char lcname[256];
  double frac[HXD_BST_TOTAL_TH_TIME_BIN];
  double ph_time[HXD_BST_TOTAL_PH_TIME_BIN];
  double th_time[HXD_BST_TOTAL_TH_TIME_BIN];

  for (ph=0; ph<HXD_BST_TOTAL_PH_TIME_BIN; ph++){
  ph_time[ph] = frz_tm[board] - on_time[board] + pseudo_ph_time[ph];
  }

  for (th=0; th<HXD_BST_TOTAL_TH_TIME_BIN; th++){
  th_time[th] = frz_tm[board] - on_time[board] + pseudo_th_time[th];
  }

  for (i=0;i<HXD_BST_TOTAL_TH_TIME_BIN;i++){
    frac[i] = 1.0;
  }

  if (th_mode==1) {

    if (ene_mode == -1){ /** All Energy Channels **/

    /** TH data light curve **/
      for (th=0; th<HXD_BST_TH_NUM; th++){	
		
	sprintf(lcname, "%s_wam%d_th%d.lc", com.newfilename, board, th);
        if( createlcFITS(lcname, board, th, th, integ_th_time[board],
                         HXD_BST_TOTAL_TH_TIME_BIN-1, th_mode) ){
	  *status = ANL_QUIT;
	  return;      
	}

	/*Fill Light Curve Column */
	/*** TH first bin is removed ***/
	fits_movabs_hdu(fits_fp,2,&hdutype,&istat);
	fits_write_col_dbl(fits_fp,1,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			   th_time+1, &istat);
	fits_write_col_dbl(fits_fp,2,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			   bst_th_rate[board][th]+1, &istat);
	fits_write_col_dbl(fits_fp,3,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			   bst_th_rate_err[board][th]+1, &istat);

	fits_write_col_dbl(fits_fp,4,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			   frac+1, &istat);
	
	if( closeFITS() ){
	  *status = ANL_QUIT;
	  return;
	}
	
      }

      /** TH data light curve for TH summed event **/

	sprintf(lcname, "%s_wam%d_thall.lc", com.newfilename, board);
	if( createlcFITS(lcname, board, 0, HXD_BST_TH_NUM-1,
		       integ_th_time[board],
		       HXD_BST_TOTAL_TH_TIME_BIN-1, th_mode) ){
	  *status = ANL_QUIT;
	  return;      
	}


	fits_movabs_hdu(fits_fp,2,&hdutype,&istat);
	fits_write_col_dbl(fits_fp,1,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			   th_time+1, &istat);
	fits_write_col_dbl(fits_fp,2,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			   bst_th_all_rate[board]+1, &istat);
	fits_write_col_dbl(fits_fp,3,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			   bst_th_all_rate_err[board]+1, &istat);

	fits_write_col_dbl(fits_fp,4,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			   frac+1, &istat);
	
	if( closeFITS() ){
	  *status = ANL_QUIT;
	  return;
	}

    } else if (ene_mode == 0){ /** One Energy Channel **/

      /** TH data light curve for TH single cahnnel **/

      sprintf(lcname, "%s_wam%d_th%d.lc", com.newfilename, board, ch);
      if( createlcFITS(lcname, board, ch, ch, integ_th_time[board], 
                       HXD_BST_TOTAL_TH_TIME_BIN-1, th_mode) ){
	*status = ANL_QUIT;
	return;      
      }

      fits_movabs_hdu(fits_fp,2,&hdutype,&istat);
      fits_write_col_dbl(fits_fp,1,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			 th_time+1, &istat);
      fits_write_col_dbl(fits_fp,2,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			 bst_th_rate[board][ch]+1, &istat);
      fits_write_col_dbl(fits_fp,3,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			 bst_th_rate_err[board][ch]+1, &istat);

      fits_write_col_dbl(fits_fp,4,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			 frac+1, &istat);
        
      if( closeFITS() ){
	*status = ANL_QUIT;
	return;
      }

    } else if (ene_mode == 1) { /** Energy summed mode **/

      sprintf(lcname, "%s_wam%d_th%d_%d.lc", com.newfilename, board, chmin, chmax);
      if( createlcFITS(lcname, board, chmin, chmax, integ_th_time[board], 
                       HXD_BST_TOTAL_TH_TIME_BIN-1, th_mode) ){
	*status = ANL_QUIT;
	return;      
      }

      for (i=0;i<HXD_BST_TOTAL_TH_TIME_BIN;i++){
	for (j=chmin; j<chmax+1; j++){
	  bst_sum_th_rate[board][i] = 0.0;
	}
      }

      for (i=0;i<HXD_BST_TOTAL_TH_TIME_BIN;i++){
	for (j=chmin; j<chmax+1; j++){
	  bst_sum_th_rate[board][i] += bst_th_rate[board][j][i];
	}
          bst_sum_th_rate_err[board][i] =
            sqrt(bst_sum_th_rate[board][i]*integ_th_time[board])/integ_th_time[board];
      }


      fits_movabs_hdu(fits_fp,2,&hdutype,&istat);
      fits_write_col_dbl(fits_fp,1,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			 th_time+1, &istat);
      fits_write_col_dbl(fits_fp,2,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			 bst_sum_th_rate[board]+1, &istat);
      fits_write_col_dbl(fits_fp,3,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			 bst_sum_th_rate_err[board]+1, &istat);

      fits_write_col_dbl(fits_fp,4,1,1,HXD_BST_TOTAL_TH_TIME_BIN-1, 
			 frac+1, &istat);
        
      if( closeFITS() ){
	*status = ANL_QUIT;
	return;
      }

    }

    } else if (th_mode==0){ /** PH data **/

    if(ene_mode == -1){ /** All Energy Channels **/

    /** PH data Light curve **/
      for (ph=0; ph<HXD_BST_PH_NUM; ph++){	
	sprintf(lcname, "%s_wam%d_ph%d.lc", com.newfilename, board, ph);
	if( createlcFITS(lcname, board, ph, ph, integ_ph_time[board], 
		       HXD_BST_TOTAL_PH_TIME_BIN, th_mode) ){
	  *status = ANL_QUIT;
	  return;      
	}

	/*Fill Light Curve Column */
	fits_movabs_hdu(fits_fp,2,&hdutype,&istat);
	fits_write_col_dbl(fits_fp,1,1,1, HXD_BST_TOTAL_PH_TIME_BIN, 
			   ph_time, &istat);
	fits_write_col_dbl(fits_fp,2,1,1, HXD_BST_TOTAL_PH_TIME_BIN, 
			   bst_ph_rate[board][ph], &istat);
	fits_write_col_dbl(fits_fp,3,1,1, HXD_BST_TOTAL_PH_TIME_BIN, 
			   bst_ph_rate_err[board][ph], &istat);

	for (i=0;i<HXD_BST_TOTAL_PH_TIME_BIN;i++){
	  frac[i] = 1.0;
	}

	fits_write_col_dbl(fits_fp,4,1,1,HXD_BST_TOTAL_PH_TIME_BIN, frac, &istat);
	
	if( closeFITS() ){
	  *status = ANL_QUIT;
	  return;
	}
	
      }

    } else if ( ene_mode == 0 ) {

      sprintf(lcname, "%s_wam%d_ph%d.lc", com.newfilename, board, ch);
      if( createlcFITS(lcname, board, ch, ch, integ_ph_time[board], 
                       HXD_BST_TOTAL_PH_TIME_BIN, th_mode) ){
	*status = ANL_QUIT;
	return;      
      }

      /*Fill Light Curve Column */
      fits_movabs_hdu(fits_fp,2,&hdutype,&istat);
      fits_write_col_dbl(fits_fp,1,1,1, HXD_BST_TOTAL_PH_TIME_BIN, 
			 ph_time, &istat);
      fits_write_col_dbl(fits_fp,2,1,1, HXD_BST_TOTAL_PH_TIME_BIN, 
			 bst_ph_rate[board][ph], &istat);
      fits_write_col_dbl(fits_fp,3,1,1, HXD_BST_TOTAL_PH_TIME_BIN, 
			 bst_ph_rate_err[board][ph], &istat);

      for (i=0;i<HXD_BST_TOTAL_PH_TIME_BIN;i++){
	frac[i] = 1.0;
      }

      fits_write_col_dbl(fits_fp,4,1,1,HXD_BST_TOTAL_PH_TIME_BIN, frac, &istat
			 );
        
      if( closeFITS() ){
	*status = ANL_QUIT;
	return;
      }

    } else if ( ene_mode == 1){ /** Energy Summed mode **/

      sprintf(lcname, "%s_wam%d_ph%d_%d.lc", com.newfilename, board, chmin, chmax);
      if( createlcFITS(lcname, board, chmin, chmax, integ_ph_time[board],
		       HXD_BST_TOTAL_PH_TIME_BIN, th_mode) ){
        *status = ANL_QUIT;
        return;
      }

      for (i=0;i<HXD_BST_TOTAL_PH_TIME_BIN;i++){
	for (j=chmin; j<chmax+1; j++){
	  bst_sum_ph_rate[board][i] = 0.0;
	}         
      }

      /*Fill Light Curve Column */

      for (i=0;i<HXD_BST_TOTAL_PH_TIME_BIN;i++){
	for (j=chmin; j<chmax+1; j++){
	  bst_sum_ph_rate[board][i] += bst_ph_rate[board][j][i];
	}         
      bst_sum_ph_rate_err[board][i] = 
        sqrt(bst_sum_ph_rate[board][i]*integ_ph_time[board])/integ_ph_time[board];
      }

      fits_movabs_hdu(fits_fp,2,&hdutype,&istat);
      fits_write_col_dbl(fits_fp,1,1,1, HXD_BST_TOTAL_PH_TIME_BIN,
                         ph_time, &istat);
      fits_write_col_dbl(fits_fp,2,1,1, HXD_BST_TOTAL_PH_TIME_BIN,
                         bst_sum_ph_rate[board], &istat);
      fits_write_col_dbl(fits_fp,3,1,1, HXD_BST_TOTAL_PH_TIME_BIN,
                         bst_sum_ph_rate_err[board], &istat);

      for (i=0;i<HXD_BST_TOTAL_PH_TIME_BIN;i++){
        frac[i] = 1.0;
      }

      fits_write_col_dbl(fits_fp,4,1,1,HXD_BST_TOTAL_PH_TIME_BIN, frac, &istat);


      if( closeFITS() ){
	*status = ANL_QUIT;
	return;
      }

    }
  }     
}

void
HXDmkbstlc_startup(int *status)
{
    *status = ANL_OK;
}

void
HXDmkbstlc_com(int *status)
{

  *status = PILGetString("outroot", com.newfilename);
  if (*status) {
    fprintf(stderr, "%s: PILGetInt Output root error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  /* printf("FILENAME = %s\n", com.newfilename); */

  *status = PILGetInt("tpu_board", &tpu_board);
  if ( *status || tpu_board<-1 || tpu_board>3 ) {
    fprintf(stderr, "%s: PILGetInt TPU board error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetInt("th_mode", &th_mode);
  if (*status) {
    fprintf(stderr, "%s: PILGetInt TH mode error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetInt("energy_mode", &ene_mode);
  if (*status || (ene_mode !=-1 && ene_mode !=0 && ene_mode !=1 )){
    fprintf(stderr, "%s: PILGetInt energy mode error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  if (ene_mode == 0) {
    /** Each channel mode **/
    *status = PILGetInt("energy_channel", &ch);
    if (*status || (ch<0 || ch>54)) {
      fprintf(stderr, "%s: PILGetInt energy_channel error (%d)\n",
	      pname, *status );
      *status = ANL_QUIT;
      exit(-1);
    }
  } else if ( ene_mode == 1){

    /** Summed mode **/
    /** chmin =< ch <= chmax **/
    *status = PILGetInt("min_channel", &chmin);
    if (*status || chmin<0 || chmin>54) {
      fprintf(stderr, "%s: PILGetInt min_channel error (%d)\n",
	      pname, *status );
      *status = ANL_QUIT;
      exit(-1);
    }       

    *status = PILGetInt("max_channel", &chmax);
    if (*status || chmax<0 || chmax>54){
      fprintf(stderr, "%s: PILGetInt max_channel error (%d)\n",
	      pname, *status );
      *status = ANL_QUIT;
      exit(-1);
    }

    if (chmin > chmax ){
      *status = ANL_QUIT;
      exit(-1);
    }

  }

  *status = PILGetString("origin", com.origin);
  if (*status) {
    fprintf(stderr, "%s: PILGetString origin error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetBool("dt_cor", &dtcor);
  if ( *status ) {
    fprintf(stderr, "%s: PILGetBool dt_cor error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetReal("dt_clk", &dt_clk);
  if ( *status ) {
    fprintf(stderr, "%s: PILGetReal dt_clk error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetReal("dt_ph", &dt_ph);
  if ( *status ) {
    fprintf(stderr, "%s: PILGetReal dt_ph error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = ANL_OK;
}    
 
void
HXDmkbstlc_init(int *status)
{
    *status = ANL_OK;
}

void
HXDmkbstlc_his(int *status)
{
    *status = ANL_OK;
}

void
HXDmkbstlc_bgnrun(int *status)
{
  int istat = 0;
  int size;
/*char filename[256]; */
  char comment[80];

  BnkfGetM("HXDbstFitsRead:FILE_P", sizeof(com.fp), &size, &com.fp);

  BnkDef("ASTE:FFF_ORIGIN", sizeof(com.origin));
  BnkPut("ASTE:FFF_ORIGIN", strlen(com.origin)+1, com.origin);

  BnkGet("HXD:BST:format_version", sizeof(int), &size, &com.format_version);

  if( com.format_version == 1 ){
    fits_read_key_dbl(com.fp, "FRZD_TM0",  &frz_tm[0], comment, &istat);
    fits_read_key_dbl(com.fp, "FRZD_TM1",  &frz_tm[1], comment, &istat);
    fits_read_key_dbl(com.fp, "FRZD_TM2",  &frz_tm[2], comment, &istat);
    fits_read_key_dbl(com.fp, "FRZD_TM3",  &frz_tm[3], comment, &istat);

    printf("%.8f %.8f %.8f %.8f\n",frz_tm[0],frz_tm[1],frz_tm[2],frz_tm[3]);

    if(istat){
      fprintf(stderr, "%s:fits_read_key_dbl FRZD_TM failed (%d)", 
	      pname, istat);
      *status = ANL_NG;       
      return;
    }
  } else if (com.format_version == HXD_EVENT_FITS_FORMAT_VERSION){
    fits_read_key_dbl(com.fp, "FRZN_TM0",  &frz_tm[0], comment, &istat);
    fits_read_key_dbl(com.fp, "FRZN_TM1",  &frz_tm[1], comment, &istat);
    fits_read_key_dbl(com.fp, "FRZN_TM2",  &frz_tm[2], comment, &istat);
    fits_read_key_dbl(com.fp, "FRZN_TM3",  &frz_tm[3], comment, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_key_dbl FRZN_TM failed (%d)", 
	      pname, istat);
      *status = ANL_NG;       
      return;
    }
  }

  *status = ANL_OK;       
  return;
}

void
HXDmkbstlc_ana(int nevent, int eventid, int *status)
{  
  int size;
  double s_time, ae_time, bst_frz_time[HXD_TRN_BOARD_NUM];
  int bst_iblock;
  int bst_sys_err_cod;
  int bst_soft_flg;
  int bst_current_tpu;
  int bst_rest_req;
  int bst_data_size;
  int bst_read_cnt;
  int bst_de_board;
  int bst_ae_board;
  int bst_data_seq;
  int bst_th0[HXD_BST_TH_TIME_BIN];
  int bst_th1[HXD_BST_TH_TIME_BIN];
  int bst_th2[HXD_BST_TH_TIME_BIN];
  int bst_th3[HXD_BST_TH_TIME_BIN];
  int bst_th[HXD_BST_TH_NUM][HXD_BST_TH_TIME_BIN];
  int bst_ph[HXD_BST_PH_ENE_BIN+1];
  int bst_over_flow;
  int bst_dead_time;
  int bst_ud;
  int bst_sum_ld;
  int board = 0x00;
  int th, ph;
  int i, ph_time_bin, th_time_bin;
  int tmmode[HXD_TRN_BOARD_NUM];
  int delta_bst_ph0_1[HXD_TRN_BOARD_NUM][HXD_BST_TOTAL_PH_TIME_BIN], 
    delta_bst_ph2_55[HXD_TRN_BOARD_NUM][HXD_BST_TOTAL_PH_TIME_BIN],
    delta_bst_ph0_55[HXD_TRN_BOARD_NUM][HXD_BST_TOTAL_PH_TIME_BIN];

  double live_time_ph[HXD_TRN_BOARD_NUM], live_time_th[HXD_TRN_BOARD_NUM];
/*int istat = 0;*/
  int dtcnt[HXD_TRN_BOARD_NUM];
  int carry_dtcnt_flg[HXD_TRN_BOARD_NUM];

  hxdbstFitsToBnk_get( &data );

  BnkfGetM("HXD:BST:PACKET_S_TIME", sizeof(double), &size, &s_time);
  BnkfGetM("HXD:BST:PACKET_AETIME", sizeof(double), &size, &ae_time);
  BnkfGetM("HXD:BST:IBLOCK", sizeof(int), &size, &bst_iblock );
  BnkfGetM("HXD:BST:ERR_COD", sizeof(int), &size, &bst_sys_err_cod );
  BnkfGetM("HXD:BST:SOFT_FLG", sizeof(int), &size, &bst_soft_flg );
  BnkfGetM("HXD:BST:CURRENT_TPU", sizeof(int), &size, &bst_current_tpu );
  BnkfGetM("HXD:BST:REST_REQ", sizeof(int), &size, &bst_rest_req );
  BnkfGetM("HXD:BST:DATA_SIZE", sizeof(int), &size, &bst_data_size );
  BnkfGetM("HXD:BST:READ_CNT", sizeof(int), &size, &bst_read_cnt );
  BnkfGetM("HXD:BST:DE_BOARD", sizeof(int), &size, &bst_de_board );
  BnkfGetM("HXD:BST:BOARD", sizeof(int), &size, &bst_ae_board );
  BnkfGetM("HXD:BST:DATA_SEQ", sizeof(int), &size, &bst_data_seq );
  BnkfGetM("HXD:BST:TH0", sizeof(int)*HXD_BST_TH_TIME_BIN, &size, bst_th0 );
  BnkfGetM("HXD:BST:TH1", sizeof(int)*HXD_BST_TH_TIME_BIN, &size, bst_th1 );
  BnkfGetM("HXD:BST:TH2", sizeof(int)*HXD_BST_TH_TIME_BIN, &size, bst_th2 );
  BnkfGetM("HXD:BST:TH3", sizeof(int)*HXD_BST_TH_TIME_BIN, &size, bst_th3 );
  BnkfGetM("HXD:BST:PH", sizeof(int)*HXD_BST_PH_ENE_BIN, &size, bst_ph );
  BnkfGetM("HXD:BST:OVER_FLOW", sizeof(int), &size, &bst_over_flow );
  BnkfGetM("HXD:BST:DEAD_TIME", sizeof(int), &size, &bst_dead_time );
  BnkfGetM("HXD:BST:UD", sizeof(int), &size, &bst_ud );
  BnkfGetM("HXD:BST:SUM_LD", sizeof(int), &size, &bst_sum_ld );

  BnkfGetM("HXD:BST:FRZD_TM", sizeof(int)*4, &size, bst_frz_time);
  BnkfGetM("HXD:BST:TM_MODE", sizeof(int)*4, &size, tmmode);

  if(bst_de_board == bst_ae_board){
    board = bst_ae_board;
  } else {
    *status=ANL_QUIT;
  }

  integ_ph_time[board] = 1.0/2.0*pow(2.0,(double)tmmode[board]); 
  integ_th_time[board] = 1.0/64.0*pow(2.0,(double)tmmode[board]); 

  /** overflow bin is taken as PH 55 channel data **/
  bst_ph[HXD_BST_PH_ENE_BIN] = bst_over_flow; 

  /** ph_time_bin 0-127 **/
  ph_time_bin = bst_data_seq*2 + bst_iblock;  
  pseudo_ph_time[ph_time_bin] = integ_ph_time[board]*0.5 
    + integ_ph_time[board]*ph_time_bin;
  /** center of the time bin **/   
  dtcnt[board] = bst_dead_time;

  if ( ph_time_bin == 0 ){
    /*The first time bin */
    pre_dtcnt[board] = dtcnt[board];
    delta_dtcnt[board]  = 0.0;

  } else {
    /* Judge whether BST TH counter is carried or not */
    if( dtcnt[board] < pre_dtcnt[board] ){
      carry_dtcnt_flg[board] = 1;
    } else {
      carry_dtcnt_flg[board] = 0;
    }

    /* Substract the counter value from previous one*/
   delta_dtcnt[board] = dtcnt[board]
     + (carry_dtcnt_flg[board]<<16) - pre_dtcnt[board];

   pre_dtcnt[board] = dtcnt[board];
  }
  on_time[board] += integ_ph_time[board];

  delta_bst_ph0_1[board][ph_time_bin] = 0;
  delta_bst_ph2_55[board][ph_time_bin] = 0;

  for (ph=0;ph<HXD_BST_PH_ENE_BIN+1;ph++){
    delta_bst_ph[board][ph][ph_time_bin] = bst_ph[ph];

    if(ph>=0 && ph<=1){
      delta_bst_ph0_1[board][ph_time_bin] += bst_ph[ph];
	}else if(ph>=2 && ph<=HXD_BST_PH_ENE_BIN){
      delta_bst_ph2_55[board][ph_time_bin] += bst_ph[ph];
    }

  }

    delta_bst_ph0_55[board][ph_time_bin] 
      = delta_bst_ph0_1[board][ph_time_bin]+ delta_bst_ph2_55[board][ph_time_bin];

    if (th_mode == 0){
    /** Deadtime correction is made using the deadtime counter for PH **/
  if(dtcor == 1) {
    live_time_ph[board] = integ_ph_time[board] - dt_clk*delta_dtcnt[board];
  }else{
    live_time_ph[board] = integ_ph_time[board];
  }

    /** Calculate the count rate **/
    for (ph=0;ph<HXD_BST_PH_ENE_BIN+1;ph++){

    bst_ph_rate[board][ph][ph_time_bin] = 
      (double)delta_bst_ph[board][ph][ph_time_bin]/live_time_ph[board];

    /** Calculate the statistical error assuming Poisson distribution **/    
    bst_ph_rate_err[board][ph][ph_time_bin] = 
      sqrt((double)delta_bst_ph[board][ph][ph_time_bin])/live_time_ph[board];
    }

    } else if (th_mode == 1){

      for(i=0; i<HXD_BST_TH_TIME_BIN; i++ ){
    
	th_time_bin = ph_time_bin*HXD_BST_TH_TIME_BIN + i ;
	/** 0--128[s]: 0 point is _time**/
	pseudo_th_time[th_time_bin] = integ_th_time[board]*0.5
	  + integ_th_time[board]*th_time_bin;

	bst_th[0][i] = bst_th0[i];
	bst_th[1][i] = bst_th1[i];
	bst_th[2][i] = bst_th2[i];
	bst_th[3][i] = bst_th3[i];
	
	for (th=0; th<HXD_BST_TH_NUM; th++){
      
	  if ( (bst_data_seq == 0) && (bst_iblock == 0) && (i == 0) ){
	    /*The first time bin */
	    pre_bst_th[board][th] = bst_th[th][i];
	    delta_bst_th_all[board][th_time_bin]  = 0;
	  } else {
	    /* Judge whether BST TH counter is carried or not */
	    if( bst_th[th][i] < pre_bst_th[board][th] ){
	      carry_bst_th_flg[board][th] = 1;
	    } else {
	      carry_bst_th_flg[board][th] = 0;
	    }
	    /* Substract the counter value from previous one*/
	    delta_bst_th[board][th][th_time_bin] = bst_th[th][i] 
	      + (carry_bst_th_flg[board][th]<<16) - pre_bst_th[board][th];

	    delta_bst_th_all[board][th_time_bin] 
	      += delta_bst_th[board][th][th_time_bin] ;
	    
	    /* Prev counter is set at the current counter value. */
	    pre_bst_th[board][th] = bst_th[th][i];      
	  }

	}

	/** Deadtime correction is made using the deadtime counter and TH data.
	    The TH data have no information about 0-1 PH channels and UD event, 
	    so these information is estimated using the averaged rate over 0.5 or 1 sec.  
	**/

	if(dtcor == 1){
	  live_time_th[board] = integ_th_time[board] 
	    - (integ_th_time[board]/integ_ph_time[board])*(dt_clk*(double)delta_dtcnt[board] - dt_ph*(double)delta_bst_ph0_55[board][ph_time_bin]) 
	    - dt_ph*(double)delta_bst_th_all[board][th_time_bin]*(double)delta_bst_ph0_55[board][ph_time_bin]/(double)delta_bst_ph2_55[board][ph_time_bin];
	} else if(dtcor == 0){
	  live_time_th[board] = integ_th_time[board];
	}

	for (th=0; th<HXD_BST_TH_NUM; th++){
	  /* Calculate the count rate*/
	  bst_th_rate[board][th][th_time_bin] = 
	    (double)delta_bst_th[board][th][th_time_bin]/live_time_th[board];
    
	  /* Calculate the statistical error assuming the Gaussian distributions */
	  bst_th_rate_err[board][th][th_time_bin] = 
	    sqrt((double)delta_bst_th[board][th][th_time_bin])/live_time_th[board];
	}

	bst_th_all_rate[board][th_time_bin] 
	  = delta_bst_th_all[board][th_time_bin]/integ_th_time[board];
	
	bst_th_all_rate_err[board][th_time_bin] 
	  = sqrt((double)delta_bst_th_all[board][th_time_bin])/integ_th_time[board];
	
      }
    }

    *status = ANL_OK;
}
  

void
HXDmkbstlc_endrun(int *status)
{    

/*int istat = 0; */
  int board;
  
  if (tpu_board == -1){
    for (board=0;board<HXD_TRN_BOARD_NUM;board++){      
      mkbstlcFITS(board, th_mode, ene_mode, status);
    }
  } else {
    mkbstlcFITS(tpu_board, th_mode, ene_mode, status);
  }

  if (*status == ANL_NG){
    fprintf(stderr,"HXDmkbstlc_endrun: error\n");
    return;
  } else {
    *status = ANL_OK;
    return;
  }
}

void
HXDmkbstlc_exit(int *status)
{
    *status = ANL_OK;
}
