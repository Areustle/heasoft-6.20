/*******************************************************
HXDmkbstspec: Spectrum ftools for the WAM BST data
 ver. 0.0.1 K.Yamaoka 2006 July 29
   first original version
 ver. 0.0.2 K.Yamaoka 2007 Apr. 5
   correct for a bug in spectrum accumulation
 ver. 0.0.3 K.Yamaoka 2007 Apr. 30
   modify and add the key word in the header of the fits
 ver. 0.0.4 K.Yamaoka 2007 May 1
   add history 
 ver. 0.0.5 K.Yamaoka 2007 May 12
   add DETUNIT keyword, checksum
 ver. 0.1.0 K.Yamaoka 2008 Mar 12
   support TH 4band spectra
 ver. 2.0.0 K.Yamaoka 2008 Sep 25
   support ver 2.0 process 
 ver. 2.0.1 K.Yamaoka 2008 Nov 24
   bug fix in exposure of TH spectrum
 ver. 2.0.2 K.Yamaoka 2009 Jan 6
   bug fix in board selection
 ver. 2.0.3 K.Yamaoka 2009 Jan 22
   bug fix in accumuration of TH spectrum
*******************************************************/

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
#include "math.h"

#include "hxdbstFitsUtil.h"
#include "hxdbstFitsToBnkUtil.h"
#include "hxdFitsHeaderUtil.h"
#include "pil.h"
#include "hxdeventFitsUtil.h"

#define HXD_TRN_BOARD_NUM 4
#define HXD_BST_TH_NUM 4
#define HXD_BST_PH_NUM 54
#define HXD_BST_TH_ENE_BIN 4
#define HXD_BST_TOTAL_PH_TIME_BIN 128
#define HXD_BST_TOTAL_TH_TIME_BIN 4096
#define FILENAME_MAX_LENGTH 256
#define DEBUG 0
HxdBstFits data;

char HXDmkbstspec_version[] = "version 2.0.3";

static char pname[] = "HXDmkbstspec";

static fitsfile *fits_fp;
static int bst_ph_sum[HXD_TRN_BOARD_NUM][HXD_BST_PH_ENE_BIN+1];
static int bst_th_sum[HXD_TRN_BOARD_NUM][HXD_BST_TH_ENE_BIN];
static int channel[HXD_TRN_BOARD_NUM][HXD_BST_PH_ENE_BIN+1];
static double bst_ph_sum_err[HXD_TRN_BOARD_NUM][HXD_BST_PH_ENE_BIN+1];
static double bst_th_sum_err[HXD_TRN_BOARD_NUM][HXD_BST_TH_ENE_BIN];
static double delta_dtcnt[HXD_TRN_BOARD_NUM], pre_dtcnt[HXD_TRN_BOARD_NUM];
static double delta_bst_th[HXD_TRN_BOARD_NUM][HXD_BST_TH_ENE_BIN][HXD_BST_TOTAL_TH_TIME_BIN];
static double pre_bst_th[HXD_TRN_BOARD_NUM][HXD_BST_TH_ENE_BIN];
static double ontime[HXD_TRN_BOARD_NUM] = {0.0,0.0,0.0,0.0};
static double exposure[HXD_TRN_BOARD_NUM] = {0.0,0.0,0.0,0.0};
static double dtc[HXD_TRN_BOARD_NUM];
static double time_min, time_max, dt_clk;
static int th_mode, dtcor;
static double frz_tm[HXD_TRN_BOARD_NUM];
static double tstart[HXD_TRN_BOARD_NUM], tstop[HXD_TRN_BOARD_NUM];

static char *ttype[] = {
  "CHANNEL", "COUNTS", "STAT_ERR"
};

static char *tform[] = {
  "1J", "1J", "1D"
};

static char *tunit[] = {
  " ", "counts", "counts"
};

static struct {
  fitsfile *fp;
  long irow;
  long nrow;
  int iomode;  int gtimode;
  int time_colnum;  int pi_colnum;  int quality_colnum;
  int time_change;  int pi_change;  int quality_change;
  fitsfile *new_fp;
  char newfilename[FILENAME_MAX_LENGTH];
  int trn_board;
  char origin[256];
  int format_version;
}com;

static int writeHistory_bstspec ( void ){
  int istat = 0;
  int status = ANL_OK;
  char history [PIL_LINESIZE];

  sprintf(history, "    ------- parameters in %s -------   ", pname);
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

  sprintf(history, "         outroot = %s", com.newfilename);
  fits_write_history(fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  if(com.trn_board == -1){
    sprintf(history, "         tpu_board = all (0,1,2,3)");
  } else {
    sprintf(history, "         tpu_board = %d", com.trn_board);
  }
  fits_write_history(fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  if (th_mode == 0){
    sprintf(history, "         th_mode = PH");
  } else if (th_mode == 1){
    sprintf(history, "         th_mode = TH");
  } else {
    sprintf(history, "         th_mode = unknown");
  }
  fits_write_history(fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  sprintf(history, "         time_interval = %.8lf -- %.8lf (sec)", 
          time_min, time_max);
  fits_write_history(fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  if(dtcor){
    sprintf(history, "         dt_cor = Perform dead time correction");
  } else {
    sprintf(history, "         dt_cor = No dead time correction");
  }
  fits_write_history(fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  if(dtcor){
    sprintf(history, "         dt_clk = %e (sec) dead time clock", dt_clk);
    fits_write_history(fits_fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history failed (%d)\n", istat);
      status = ANL_NG; return status;
    }
  }
  return status;
}

static int createphaFITS(char *fitsname, int board, double tstart, double tstop, double ontime, double dtc, double exposure){
  int istat = 0;
  int hdutype, hdunum;
  int simple = 1;
  int bitpix = 8;
  int naxis = 0;
  long naxes[1];
  long pcount = 0;
  long gcount = 1;
  int extend = 1; /** Extension is included**/
  int err_flag = 1;
  int nc = 3; /** Number of column **/
  char date_obs[16], time_obs[16], date_end[16], time_end[16];
  AtTimeD tstart_attime, tstop_attime;

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
    fits_write_key_str(fits_fp,"TELESCOP","SUZAKU","Telescope (mission) name", &istat);
    fits_write_key_str(fits_fp,"INSTRUME","HXD","Instrument name", &istat);
    fits_write_key_str(fits_fp,"DETNAM",  "WAM_ANTI","Detector name", &istat);
    fits_write_key_str(fits_fp,"OBJECT",  "DEFAULT","Name of observed object", &istat);
    fits_write_key_str(fits_fp,"ORIGIN",  "ISAS/JAXA","Origin of FITS file", &istat);
    fits_write_key_str(fits_fp,"CREATOR", "hxdmkbstspec",
                       "Program name that produced this file", &istat);
  }
  
  if ( istat ){
    fprintf(stderr, "%s:fits_write_key_str failed (%d)\n", pname, istat);
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

  /** Write the keywords in 1st extention **/
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

    if(th_mode == 0){
      fits_write_btblhdr(fits_fp, HXD_BST_PH_ENE_BIN+1, nc, 
			 ttype, tform, tunit, "SPECTRUM", pcount, &istat); 
    } else if (th_mode == 1){
      fits_write_btblhdr(fits_fp, HXD_BST_TH_ENE_BIN, nc, 
			 ttype, tform, tunit, "SPECTRUM", pcount, &istat); 
    }
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {

  aste2attimeD(tstart, &tstart_attime);
  aste2attimeD(tstop, &tstop_attime);

  sprintf(date_obs, "%04d-%02d-%02d",  tstart_attime.yr, tstart_attime.mo, tstart_attime.dy);
  sprintf(time_obs, "%02d:%02d:%.6lf", tstart_attime.hr, tstart_attime.mn, tstart_attime.sc+tstart_attime.ss);
  sprintf(date_end, "%04d-%02d-%02d",  tstop_attime.yr, tstop_attime.mo, tstop_attime.dy);
  sprintf(time_end, "%02d:%02d:%.6lf", tstop_attime.hr, tstop_attime.mn, tstop_attime.sc+tstop_attime.ss);

  fits_write_key_str(fits_fp, "TELESCOP", "SUZAKU","Telescope (mission) name", &istat);
  fits_write_key_str(fits_fp, "INSTRUME", "HXD","Instrument name", &istat);
  fits_write_key_str(fits_fp, "DETNAM",   "WAM_ANTI","Detector name", &istat);
  fits_write_key_lng(fits_fp, "DETUNIT",  board,"WAM detector unit (0,1,2,3)", &istat);
  fits_write_key_str(fits_fp, "OBJECT",   "DEFAULT","Name of observed object", &istat);
  fits_write_key_str(fits_fp, "FILTER",   "NONE", "redistribution matrix file(RMF)", &istat);
  fits_write_key_dbl(fits_fp, "TSTART",   tstart, 14,  "start time", &istat);
  fits_write_key_dbl(fits_fp, "TSTOP",    tstop, 14,   "stop time", &istat);
  fits_write_key_str(fits_fp, "DATE_OBS", date_obs, "Date of observation start", &istat);
  fits_write_key_str(fits_fp, "TIME_OBS", time_obs, "Time of observation start", &istat);
  fits_write_key_str(fits_fp, "DATE_END", date_end, "Date of observation stop", &istat);
  fits_write_key_str(fits_fp, "TIME_END", time_end, "Time of observation stop", &istat);
  fits_write_key_dbl(fits_fp, "ONTIME",   ontime, 10,   "Total spectrum accumulation time", &istat);
  fits_write_key_dbl(fits_fp, "DEADC",    dtc, 10, "Deadtime correction factor", &istat);
  fits_write_key_dbl(fits_fp, "LIVETIME", exposure, 10, "ONTIME multiplied by deadtime correction factor", &istat);
  fits_write_key_dbl(fits_fp, "EXPOSURE", exposure, 10, "exposure time", &istat);
  fits_write_key_flt(fits_fp, "AREASCAL", 1.0, 10, "nominal effective area",&istat);
  fits_write_key_str(fits_fp, "BACKFILE", "NONE","background FITS file for this object", &istat);
  fits_write_key_flt(fits_fp, "BACKSCAL", 1.0, 10, "background scaling factor", &istat);
  fits_write_key_str(fits_fp, "CORRFILE", "NONE","correlation FITS file for this object", &istat);
  fits_write_key_flt(fits_fp, "CORRSCAL", 1.0, 10, "correlation scaling factor", &istat);
  fits_write_key_str(fits_fp, "RESPFILE", "NONE", "redistribution matrix file(RMF)", &istat);
  fits_write_key_str(fits_fp, "ANCRFILE", "NONE", "ancillary response file (ARF)", &istat);
  fits_write_key_str(fits_fp, "PHAVERSN", "1992a", "OGIP classification of FITS format", &istat);

  if (th_mode == 0){
  fits_write_key_lng(fits_fp, "DETCHANS", HXD_BST_PH_ENE_BIN+1, "Total no. detector channels available", &istat);
  } else if (th_mode == 1){
  fits_write_key_lng(fits_fp, "DETCHANS", HXD_BST_TH_ENE_BIN, "Total no. detector channels available", &istat);
  }

  fits_write_key_log(fits_fp, "STATERR" , 0, "no statisical error specified", &istat);
  fits_write_key_log(fits_fp, "SYSERR"  , 0, "no systematic error", &istat);
  fits_write_key_log(fits_fp, "POISSERR", 0, "Poissonian statistical errors to be assumed", &istat);
  fits_write_key_lng(fits_fp, "GROUPING", 0, "grouping of the data has been defined", &istat);
  fits_write_key_lng(fits_fp, "QUALITY",  0, "data quality information specified", &istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_key failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  istat = writeHistory_bstspec();
  if(istat != ANL_OK) { /** error **/
    fprintf(stderr, "%s: Error in writing history\n", pname);
    return istat;
  } else {              /** continue **/
    istat = 0;
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_key for 1st extensionB failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  return ANL_OK;
}

static int closephaFITS(){
  int istat = 0;
  int hdutype;
  fits_close_file(fits_fp, &istat);

  if(istat){
    fprintf(stderr, "%s:fits_close_file in closephaFITS failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  return ANL_OK;
}

static int mkbstphaFits(int board, int th_mode){
  int istat = 0;
  int i;
  char phaname[256];

    if(th_mode == 0){  
      sprintf(phaname, "%s_bst%d_ph.pha", com.newfilename, board);
    for(i=0;i<HXD_BST_PH_ENE_BIN+1;i++){
    bst_ph_sum_err[board][i] = sqrt(bst_ph_sum[board][i]);
    channel[board][i] = i;
    }
  } else if(th_mode == 1){  
    sprintf(phaname, "%s_bst%d_th.pha", com.newfilename, board);
    for(i=0;i<HXD_BST_TH_ENE_BIN;i++){
    bst_th_sum_err[board][i] = sqrt(bst_th_sum[board][i]);
    channel[board][i] = i;
    }
  }

    istat = createphaFITS(phaname, board, tstart[board], tstop[board], ontime[board], dtc[board],exposure[board]);
    if(istat != ANL_OK){
      fprintf(stderr, "%s:createphaFITS failed (%d)\n", pname, istat);
      return ANL_NG;
    } 

  /*
  fits_write_key_dbl(fits_fp, "EXPOSURE", exposure[board], 10, "exposure time", &istat);
  */
  if (th_mode == 0){
  fits_write_col_int(fits_fp, 1, 1, 1, 
		     HXD_BST_PH_ENE_BIN+1, channel[board], &istat);
  fits_write_col_int(fits_fp, 2, 1, 1, 
		     HXD_BST_PH_ENE_BIN+1, bst_ph_sum[board], &istat);
  fits_write_col_dbl(fits_fp, 3, 1, 1, 
		     HXD_BST_PH_ENE_BIN+1, bst_ph_sum_err[board], &istat);
  } else if (th_mode == 1){
  fits_write_col_int(fits_fp, 1, 1, 1, 
		     HXD_BST_TH_ENE_BIN, channel[board], &istat);
  fits_write_col_int(fits_fp, 2, 1, 1, 
		     HXD_BST_TH_ENE_BIN, bst_th_sum[board], &istat);
  fits_write_col_dbl(fits_fp, 3, 1, 1, 
		     HXD_BST_TH_ENE_BIN, bst_th_sum_err[board], &istat);
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_col_int/dbl for 2nd extension failed (%d)\n", pname, istat);
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
    fprintf(stderr, "%s:fits_write_chksum for 2nd extension failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  istat = closephaFITS();
  if(istat != ANL_OK){
    fprintf(stderr, "%s:closephaFITS failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  return ANL_OK;

}


void
HXDmkbstspec_startup(int *status)
{
    *status = ANL_OK;
}

void
HXDmkbstspec_com(int *status)
{

  *status = PILGetString("origin", com.origin);
  if (*status) {
    fprintf(stderr, "%s: PILGetString origin error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetString("outroot", com.newfilename);
  if ( *status ) {
    fprintf(stderr, "%s: PILGetString Output root error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetInt("tpu_board", &com.trn_board);
  if ( *status || com.trn_board<-1 || com.trn_board>3 ) {
    fprintf(stderr, "%s: PILGetInt trn_board error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetInt("th_mode", &th_mode);
  if ( *status || th_mode<0 || th_mode>1) {
    fprintf(stderr, "%s: PILGetInt th_mode error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetReal("time_min", &time_min);
  if ( *status ) {
    fprintf(stderr, "%s: PILGetReal time_min error (%d)\n",
            pname, *status );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = PILGetReal("time_max", &time_max);
  if ( *status ) {
    fprintf(stderr, "%s: PILGetReal time_max error (%d)\n",
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

  *status = ANL_OK;
}    
 
void
HXDmkbstspec_init(int *status)
{
    *status = ANL_OK;
}

void
HXDmkbstspec_his(int *status)
{
    *status = ANL_OK;
}

void
HXDmkbstspec_bgnrun(int *status)
{

  int istat = 0;
  int size;
  char filename[256];
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
HXDmkbstspec_ana(int nevent, int eventid, int *status)
{
  int size;
  double s_time, ae_time, bst_frz_time[4];
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
  int board, th, ph;
  int i, j, ph_time_bin, th_time_bin;
  int tmmode[HXD_TRN_BOARD_NUM];
  int istat = 0;
  int dt_mode, dead_time, pseudo;
  double ph_time[HXD_TRN_BOARD_NUM][HXD_BST_TOTAL_PH_TIME_BIN];
  double th_time[HXD_TRN_BOARD_NUM][HXD_BST_TOTAL_TH_TIME_BIN];
  double integ_ph_time[HXD_TRN_BOARD_NUM], integ_th_time[HXD_TRN_BOARD_NUM];
  double dtcnt[HXD_TRN_BOARD_NUM];
  int carry_dtcnt_flg[HXD_TRN_BOARD_NUM];
  int carry_bst_th_flg[HXD_TRN_BOARD_NUM][HXD_BST_TH_ENE_BIN];
  static int first_accum_flg[HXD_TRN_BOARD_NUM] = {1,1,1,1};
  static int last_accum_flg[HXD_TRN_BOARD_NUM] = {1,1,1,1};

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
  ph_time[board][ph_time_bin] = frz_tm[board]  
    - integ_ph_time[board]*(HXD_BST_TOTAL_PH_TIME_BIN - ph_time_bin - 0.5);

  /** center of the time bin **/   
  dtcnt[board] = bst_dead_time;

  if ( ph_time_bin == 0 ){
    /*The first time bin */
    pre_dtcnt[board] = dtcnt[board];
    delta_dtcnt[board]  = 0.0;

    /** initialize the counter **/
    if (th_mode == 0){
    for(i=0;i<HXD_BST_PH_ENE_BIN+1;i++){
      bst_ph_sum[board][i] = 0;
    }}

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

  if (th_mode == 0){

  if (( ph_time[board][ph_time_bin] > time_min) 
       && (ph_time[board][ph_time_bin] < time_max) && ph_time_bin !=0){

    if(first_accum_flg[board]){
      tstart[board] = ph_time[board][ph_time_bin]
	- 0.5*integ_ph_time[board];
      first_accum_flg[board] = 0; 
    }

     if (dtcor) {
       ontime[board] += integ_ph_time[board];
       exposure[board] += integ_ph_time[board] - delta_dtcnt[board]*dt_clk;
     } else if (!dtcor){ 
       ontime[board] += integ_ph_time[board];
       exposure[board] = ontime[board];
    }

     for(i=0;i<HXD_BST_PH_ENE_BIN+1;i++){
       bst_ph_sum[board][i] += bst_ph[i];
     }

  } else if(time_max<=ph_time[board][ph_time_bin]){
  
    if (last_accum_flg[board] ==1){
      tstop[board] = ph_time[board][ph_time_bin]
	- 0.5*integ_ph_time[board];
      last_accum_flg[board] = 0; 
    }

  }
  } else if (th_mode == 1){

  /** th_time_bin 0-4095**/
  for (i=0;i<HXD_BST_TH_TIME_BIN;i++){
    th_time_bin = ph_time_bin*HXD_BST_TH_TIME_BIN + i;
    th_time[board][th_time_bin] = frz_tm[board]
    - integ_th_time[board]*(HXD_BST_TOTAL_TH_TIME_BIN - th_time_bin -0.5 );

    bst_th[0][i] = bst_th0[i];
    bst_th[1][i] = bst_th1[i];
    bst_th[2][i] = bst_th2[i];
    bst_th[3][i] = bst_th3[i];

      if ( th_time_bin == 0 ){
        /*The first time bin */
	for(j=0;j<HXD_BST_TH_ENE_BIN;j++){
	  bst_th_sum[board][j] = 0;
	}

	for (th=0; th<HXD_BST_TH_NUM; th++){
	  pre_bst_th[board][th] = bst_th[th][i];
	  delta_bst_th[board][th][th_time_bin]  = 0.0;
	}

      } else {

        /* Judge whether BST TH counter is carried or not */
	for (th=0; th<HXD_BST_TH_NUM; th++){
	  if( bst_th[th][i] < pre_bst_th[board][th] ){
	    carry_bst_th_flg[board][th] = 1;
	  } else {
	    carry_bst_th_flg[board][th] = 0;
	  }
	  /* Substract the counter value from previous one*/
	  delta_bst_th[board][th][th_time_bin] = bst_th[th][i] 
	    + (carry_bst_th_flg[board][th]<<16) - pre_bst_th[board][th];
       
	  /* Prev counter is set at the current counter value. */
	  pre_bst_th[board][th] = bst_th[th][i];      
	}

	if (( th_time[board][th_time_bin] > time_min) 
	    && (th_time[board][th_time_bin] < time_max) && th_time_bin !=0){

	  if(first_accum_flg[board]){
	    tstart[board] = th_time[board][th_time_bin]
	      - 0.5*integ_th_time[board];
	    first_accum_flg[board] = 0; 
	  }
	  	  
	  for(j=0;j<HXD_BST_TH_ENE_BIN;j++){
	    bst_th_sum[board][j] += delta_bst_th[board][j][th_time_bin];
	  }

	  if (dtcor) {
	    ontime[board] += integ_th_time[board];
	    /** Note that deadtime correction is performed 
		by averaged rate of the deadtime couter. **/
	    exposure[board] += integ_th_time[board] - 
	      (double)1/HXD_BST_TH_TIME_BIN * delta_dtcnt[board]*dt_clk;
	  } else if (!dtcor){ 
	    ontime[board] += integ_th_time[board];
	    exposure[board] = ontime[board];
	  }
	  
	} else if(time_max<=th_time[board][th_time_bin]){
	  
	  if (last_accum_flg[board] ==1){
	    tstop[board] = th_time[board][th_time_bin]
	      - 0.5*integ_th_time[board];
	    last_accum_flg[board] = 0; 
	  }
	}
      }
  }
  }

 *status = ANL_OK;
  
}

void
HXDmkbstspec_endrun(int *status)
{   
  int board;
  int istat = 0;


  if (com.trn_board == -1){
    for (board=0; board<HXD_TRN_BOARD_NUM; board++){     
      dtc[board] = exposure[board]/ontime[board];
      mkbstphaFits(board, th_mode);
    }
  } else if (com.trn_board==0 || com.trn_board==1 || com.trn_board==2 || com.trn_board==3 ){
    dtc[com.trn_board] = exposure[com.trn_board]/ontime[com.trn_board];
    mkbstphaFits(com.trn_board, th_mode);
  }

  *status = ANL_OK;
}

void
HXDmkbstspec_exit(int *status)
{
    *status = ANL_OK;
}
