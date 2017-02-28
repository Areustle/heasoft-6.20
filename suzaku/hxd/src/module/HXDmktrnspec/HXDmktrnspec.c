/*******************************************************
HXDmktrnspec: Spectrum ftools for the WAM TRN data
 ver. 0.0.1 K.Yamaoka 2005 May 3
   first original version
 ver. 0.0.2 K.Yamaoka 2005 Oct.18
   modify the keyword and etc..    
 ver. 0.0.3 K.Yamaoka 2005 Nov.12
   support GTI   
 ver. 0.0.4 K.Yamaoka 2006 May 3
   support WAM time mode and DE compression
 ver. 0.0.5 K.Yamaoka 2007 Apr. 30
   modify and add keyword in the header of the fits
 ver. 0.0.6 K.Yamaoka 2007 Apr. 30
   add history
 ver. 0.0.7 K.Yamaoka 2007 May. 12
   add DETUNIT keyword, checks
 ver. 0.0.8 K.Yamaoka 2007 Sep. 25
   initialize trn_ph_sum[][]
 ver. 0.0.9 Y.Terada 2013 Oct. 21
   add return value
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

#include "hxdtrnFitsUtil.h"
#include "hxdtrnFitsToBnkUtil.h"
#include "hxdFitsHeaderUtil.h"
#include "pil.h"

#define HXD_TRN_BOARD_NUM 4
#define FILENAME_MAX_LENGTH 256
#define DEBUG 0

char HXDmktrnspec_version[] = "version 0.0.9";

static char pname[] = "HXDmktrnspec";

static fitsfile *fits_fp;
static int trn_ph_sum[HXD_TRN_BOARD_NUM][HXD_TRN_MAX_ENE_BIN+1]; /** 54PH channel + overflow**/
static int channel[HXD_TRN_BOARD_NUM][HXD_TRN_MAX_ENE_BIN+1];
static double trn_ph_sum_err[HXD_TRN_BOARD_NUM][HXD_TRN_MAX_ENE_BIN+1];
static double delta_dtcnt[HXD_TRN_BOARD_NUM], pre_dtcnt[HXD_TRN_BOARD_NUM];
static double exposure[HXD_TRN_BOARD_NUM] = {0.0,0.0,0.0,0.0};
static double ontime[HXD_TRN_BOARD_NUM] = {0.0,0.0,0.0,0.0};
static double dtc[HXD_TRN_BOARD_NUM];
static int data_num[HXD_TRN_BOARD_NUM] = {0,0,0,0};
static double tstart[HXD_TRN_BOARD_NUM], tstop[HXD_TRN_BOARD_NUM];
static int trn_board = 0;
static double time_min, time_max, dt_clk;
static int dtcor;

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
  fitsfile *new_fp;
  long irow;
  long nrow;
  int iomode;  int gtimode;
  int time_colnum;  int pi_colnum;  int quality_colnum;
  int time_change;  int pi_change;  int quality_change;
  int trn_board;
  char newfilename[FILENAME_MAX_LENGTH];
  char origin[FILENAME_MAX_LENGTH];
}com;

static int writeHistory_wamspec ( void ){
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

  if(trn_board==-1){
    sprintf(history, "         tpu_board = all (0,1,2,3)");
  } else {
    sprintf(history, "         tpu_board = %d", trn_board);
  }
  fits_write_history(fits_fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    status = ANL_NG; return status;
  }

  sprintf(history, "         time_interval = %.8f -- %.8f (sec)", 
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
/*int hdutype, hdunum;*/
  int simple = 1;
  int bitpix = 8;
  int naxis = 0;
  long naxes[1];
  long pcount = 0;
  long gcount = 1;
  int extend = 1; /** Extension is included**/
/*int err_flag = 1; */
  int nc = 3;
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
    fits_write_key_str(fits_fp,"DETNAM","WAM_ANTI","Detector name", &istat);
    fits_write_key_str(fits_fp,"OBJECT","DEFAULT","Name of observed object", &istat);
    fits_write_key_str(fits_fp,"ORIGIN","ISAS/JAXA","Origin of FITS file", &istat);
    fits_write_key_str(fits_fp,"CREATOR","hxdmkwamspec",
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
  fits_write_btblhdr(fits_fp, HXD_TRN_MAX_ENE_BIN+1, nc, ttype, tform, tunit, 
		     "SPECTRUM", pcount, &istat); 
  }

  if ( istat ){
    fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    
    aste2attimeD(tstart, &tstart_attime);
    aste2attimeD(tstop, &tstop_attime);

    sprintf(date_obs, "%04d-%02d-%02d",  tstart_attime.yr, tstart_attime.mo, tstart_attime.dy);
    sprintf(time_obs, "%02d:%02d:%.6f", tstart_attime.hr, tstart_attime.mn, tstart_attime.sc+tstart_attime.ss);
    sprintf(date_end, "%04d-%02d-%02d",  tstop_attime.yr, tstop_attime.mo, tstop_attime.dy);
    sprintf(time_end, "%02d:%02d:%.6f", tstop_attime.hr, tstop_attime.mn, tstop_attime.sc+tstop_attime.ss);

  fits_write_key_str(fits_fp, "TELESCOP",  "SUZAKU","Telescope (mission) name", &istat);
  fits_write_key_str(fits_fp, "INSTRUME",  "HXD","Instrument name", &istat);
  fits_write_key_str(fits_fp, "DETNAM",    "WAM_ANTI","Detector name", &istat);
  fits_write_key_lng(fits_fp,"DETUNIT", board,"WAM detector unit (0,1,2,3)", &istat);
  fits_write_key_str(fits_fp, "OBJECT",    "DEFAULT","Name of observed object", &istat);
  fits_write_key_str(fits_fp, "FILTER",   "NONE", "Instrument filter in use", &istat);
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
  fits_write_key_str(fits_fp, "PHAVERSN", "1992a", "OGIP classificationof FITS format", &istat);
  fits_write_key_lng(fits_fp, "DETCHANS", HXD_TRN_MAX_ENE_BIN+1, "Total no. detector channels available", &istat);
  fits_write_key_log(fits_fp, "STATERR" , 0, "no statisical error specified", &istat);
  fits_write_key_log(fits_fp, "SYSERR"  ,  0, "no systematic error", &istat);
  fits_write_key_log(fits_fp, "POISSERR", 0, "Poissonian statistical errors to be assumed", &istat);
  fits_write_key_lng(fits_fp, "GROUPING", 0, "grouping of the data has been defined", &istat);
  fits_write_key_lng(fits_fp, "QUALITY" ,  0, "data quality information specified", &istat);

  }
  if ( istat ){
    fprintf(stderr, "%s:fits_write_key for 1st extension failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  istat = writeHistory_wamspec();
  if(istat != ANL_OK) { /** error **/
    fprintf(stderr, "%s: Error in writing history\n", pname);
    return istat;
  } else {              /** continue **/
    istat = 0;
  }

  /*
  fits_write_date(fits_fp,&istat);
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
  */

  return ANL_OK;
}

static int closephaFITS(){
  int istat = 0;
/*int hdutype;*/
  fits_close_file(fits_fp, &istat);
  return ANL_OK;
}

static void mktrnphaFits(int board, int* status){
  int istat = 0;
  int i;
/*int *status; */
  char phaname[256];

  
  for(i=0;i<HXD_TRN_MAX_ENE_BIN+1;i++){
    trn_ph_sum_err[board][i] = sqrt(trn_ph_sum[board][i]);
    channel[board][i] = i;
  }

  sprintf(phaname, "%s_wam%d.pha", com.newfilename, board);

  if( createphaFITS(phaname, board, tstart[board], tstop[board], ontime[board], dtc[board], exposure[board]) ){
    *status = ANL_QUIT;
    return;
  }
  /*
  fits_write_key_dbl(fits_fp, "EXPOSURE", exposure[board], 10, "exposure time", &istat);
  */
  fits_write_col_int(fits_fp, 1, 1, 1, 
		     HXD_TRN_MAX_ENE_BIN+1, channel[board], &istat);
  fits_write_col_int(fits_fp, 2, 1, 1, 
		     HXD_TRN_MAX_ENE_BIN+1, trn_ph_sum[board], &istat);
  fits_write_col_dbl(fits_fp, 3, 1, 1, 
		     HXD_TRN_MAX_ENE_BIN+1, trn_ph_sum_err[board], &istat);

  if ( istat ){
  fprintf(stderr, "%s:fits_write_col_int/dbl for 2nd extension failed (%d)\n", pname, istat);
  }else{
  fits_write_date(fits_fp,&istat);
  }
  if ( istat ){
    fprintf(stderr, "%s:fits_write_date for 2nd extension failed (%d)\n", pname, istat);
    *status = ANL_NG;
    return;
  } else {
    fits_write_chksum(fits_fp,&istat);
  }

  if( closephaFITS() ){
    *status = ANL_QUIT;
    return;
  }
}

void
HXDmktrnspec_startup(int *status)
{
    *status = ANL_OK;
}

void
HXDmktrnspec_com(int *status)
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

  *status = PILGetInt("tpu_board", &trn_board);
  if ( *status ) {
    fprintf(stderr, "%s: PILGetInt trn_board error (%d)\n",
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

  com.trn_board = trn_board;
    
  *status = ANL_OK;
}    
 
void
HXDmktrnspec_init(int *status)
{
    *status = ANL_OK;
}

void
HXDmktrnspec_his(int *status)
{
    *status = ANL_OK;
}

void
HXDmktrnspec_bgnrun(int *status)
{
    *status = ANL_OK;       
}

void
HXDmktrnspec_ana(int nevent, int eventid, int *status)
{
  HxdTrnFits data;
  int i;
/*int j;*/
  int size;
  int trn_ae_module, trn_ae_block;
  int trn_de_module, trn_de_block;
  double ev_time;
  int trn_time_mode;
  int trn_ph[HXD_TRN_MAX_ENE_BIN+1];
  int trn_pi[HXD_TRN_MAX_ENE_BIN+1];
  int over_flow;
  int dt_mode, dead_time, pseudo;
  int trn_quality;
/*int istat;*/
  double compress_factor;
  double integ_time[HXD_TRN_BOARD_NUM];
  int board = -1;
  double dtcnt[HXD_TRN_BOARD_NUM];
  int carry_dtcnt_flg[HXD_TRN_BOARD_NUM];
  static int first_accum_flg[HXD_TRN_BOARD_NUM] = {1,1,1,1};
  static int last_accum_flg[HXD_TRN_BOARD_NUM]  = {1,1,1,1};

  hxdtrnFitsToBnk_get( &data );
  BnkfGetM("HXD:TRN:EV_TIME", sizeof(double), &size, &ev_time);

  BnkfGetM("HXD:TRN:BOARD", sizeof(int), &size, &trn_de_module);
  BnkfGetM("HXD:TRN:BLOCK", sizeof(int), &size, &trn_de_block);

  BnkfGetM("HXD:TRH:BOARD", sizeof(int), &size, &trn_ae_module);
  BnkfGetM("HXD:TRH:BLOCK", sizeof(int), &size, &trn_ae_block);
  BnkfGetM("HXD:TRH:TIME_MODE", sizeof(int), &size, &trn_time_mode);

  BnkfGetM("HXD:TRB:PI", sizeof(int)*HXD_TRN_MAX_ENE_BIN, &size, trn_pi);
  BnkfGetM("HXD:TRB:PH", sizeof(int)*HXD_TRN_MAX_ENE_BIN, &size, trn_ph);
  BnkfGetM("HXD:TRB:OVER_FLOW", sizeof(int), &size, &over_flow);
  BnkfGetM("HXD:TRH:DT_MODE", sizeof(int), &size, &dt_mode);
  BnkfGetM("HXD:TRB:DEAD_TIME", sizeof(int), &size, &dead_time);
  BnkfGetM("HXD:TRB:PSEUDO", sizeof(int), &size, &pseudo);
  BnkfGetM("HXD:TRN:TRN_QUALITY", sizeof(int), &size, &trn_quality );

 if(trn_de_module == trn_ae_module){
    board = trn_ae_module;
  } else {
    *status=ANL_QUIT;
  }

 compress_factor = (double)trn_ae_block/trn_de_block;
 integ_time[board] = 0.5 * pow(2.0,trn_time_mode) * compress_factor;

 trn_ph[HXD_TRN_MAX_ENE_BIN] = over_flow;

 dtcnt[board] = dead_time;

 if ( data_num[board] == 0 ){
   /*The first time bin */
   pre_dtcnt[board] = dead_time;
   delta_dtcnt[board]  = 0.0;
   /** initialize trn_ph_sum[][] **/
   for(i=0;i<HXD_TRN_MAX_ENE_BIN+1;i++){
     trn_ph_sum[board][i] += 0;
   }

 } else {
      /* Judge whether Deadtime counter is carried or not */
   if( dtcnt[board] < pre_dtcnt[board] ){
     carry_dtcnt_flg[board] = 1;
   } else {
     carry_dtcnt_flg[board] = 0;
   }
   /* Substract the current counter value from previous one*/
   delta_dtcnt[board] = dtcnt[board]
     + (carry_dtcnt_flg[board]<<16) - pre_dtcnt[board];
   
   pre_dtcnt[board] = dtcnt[board];

   if ((ev_time-0.5*integ_time[board] > time_min) 
       && (ev_time-0.5*integ_time[board] < time_max)){

     if(first_accum_flg[board]){
      tstart[board] = ev_time - integ_time[board];
      first_accum_flg[board] = 0; 
     }

     /** exposure accumulation and deadtime correction **/
     if (dtcor) {
       ontime[board]   += integ_time[board];
       /*
       exposure[board] += (1.0 - delta_dtcnt[board]*dt_clk) * integ_time[board];
       */
       exposure[board] += (integ_time[board] - delta_dtcnt[board]*dt_clk);

     } else if (!dtcor){ 
       ontime[board]   += integ_time[board]; 
       exposure[board] = ontime[board];
    }
     /** Spectrum accumultation **/
     for(i=0;i<HXD_TRN_MAX_ENE_BIN+1;i++){
       trn_ph_sum[board][i] += trn_ph[i];
     }

   } else if (ev_time-0.5*integ_time[board]>=time_max){

     if (last_accum_flg[board] ==1){
      tstop[board] = ev_time - integ_time[board];
      last_accum_flg[board] = 0; 
     }

   }
   
 }

 data_num[board]++;

 *status = ANL_OK;
}

void
HXDmktrnspec_endrun(int *status)
{   
  int board;
/*int istat = 0;*/

  if (trn_board == -1){
    for (board=0; board<HXD_TRN_BOARD_NUM; board++){     
      dtc[board] = exposure[board]/ontime[board];
      mktrnphaFits(board, status);
    }
  }else {
    dtc[trn_board] = exposure[trn_board]/ontime[trn_board];
    mktrnphaFits(trn_board, status);
  }

  if (*status != ANL_OK){
    fprintf(stderr,"HXDmktrnspec_endrun: error\n");
    return;
  } else {
    *status = ANL_OK;
    return;
  }

}

void
HXDmktrnspec_exit(int *status)
{
    *status = ANL_OK;
}
