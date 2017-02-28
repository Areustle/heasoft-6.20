/*
 *  HXDfbstTime.c
 *       version 0.0.1              Y.Terada 1999.10.08
 *       + hxdTimeUtil version 0.6.2
 *       version 0.0.5              Y.Terada 1999.12.23
 *       + hxdTimeUtil version 0.6.4
 *       add tlm_time/telm_time/tlm_aetime
 *       Change param name for the third release of HXD-FTOOLS.
 *       versino 0.1.0              Y.Terada 2003.05.03
 *       for HXD-II format, no need to search TRN
 *       version 0.2.0             Y.Terada  2003.07.23 
 *         for HEADAS, PIL
 *       version 0.2.1             Y.Terada  2004-03-13
 *         Quit (FITS)
 *        version 0.2.3             Y.Terada  2005-02-23
 *         hxdTimeUtil, version 0.8.1, astetool v1.31
 *        version 0.2.4             R.Miyawaki 2005.05.28
 *                 Change PACKET_AETIME -> PACKET_S_TIME
 *                 hxdTimeUtil, version 0.8.2, Read tim_file
 *        version 0.2.5             Y.Terada  2005-06-03
 *                 add time resolution, TIMEDEL.
 *        version 0.2.6             Y.Terada  2005-06-08
 *                 debug TIMEDEL
 *        version 0.2.7             ?.??????  2005-06-09
 *                 
 *        version 0.2.8             M.Kokubun 2005-09-12
 *                 default conversion mode from II-I to III
 *        version 0.2.9             Y.Terada  2005-09-17
 *                 debug (delete needless BnkGet)
 *                 Cheange TIME source to HXD:BST:TIME
 *        version 0.3.0             Y.Terada  2005-10-24
 *                 define ACU ZERO, before 2005/10/25
 *        version 0.3.1             Y.Terada  2005-10-28
 *                 define ACU ZERO, before 2005/10/30 21:40:00
 *        version 0.3.2             Y.Terada  2005-11-05
 *                 write PIL value in FFF header
 *        version 0.3.3             Y.Terada  2005-11-08
 *        version 0.3.4             Y.Terada  2006-02-06
 *                 debug, leapsec init before aste function
 *        ---> version 1.0.0
 *        version 2.0.0             Y.Terada  2007-05-06
 *                 Read ae_hxd_bstidt.fits for ver2.0 process.
 *        version 2.0.1             Y.Terada  2008-07-29
 *                 use bstidt for time assignment
 *        version 2.0.2             Y.Terada  2008-10-23
 *                 debug (thanks to M.Ozaki)
 *        version 2.0.3             Y.Terada  2008-10-30
 *                 disable bstidt access when file is in CALDB
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "fitsio.h"
#include "cfortran.h"
#include "hbook.h"
#include "hxd/HXD.h"

#include "atFunctions.h"
#include "aste_time.h"

/* #include "uclpario.h" */
#include "pil.h" 
#include "headas.h"

#include "hxdTime.h"
#include "hxdTimeUtil.h"
#include "HXDfbstTime.h"
#include "aste_gethk.h"
#include "hxdFitsHeaderUtil.h"
#include "hxdcaldbUtil.h"

char HXDfbstTime_version[] = "version 2.0.3";
static char pname[] = "HXDfbstTime";

static double aetime_border;
static HxdTimeStatus timstat;
static int HxdTime2aetime_mode = 0;
#define TI_AETIME_MODE_I         1
#define TI_AETIME_MODE_II_I      2
#define TI_AETIME_MODE_II_II     3
#define TI_AETIME_MODE_III       4

#define TIME_VALID   0
#define TIME_INVALID 1

#define GET_KEYWORD_NUM 4
#define FILELIST_MAX_LENGTH PIL_LINESIZE

static double bst_frzd_tm[4] = {0.0, 0.0, 0.0, 0.0};

/** 
    In Suzaku Operetion before 2005/10/25, we did not reset 
    the time counter, so we have to give the explicit value
    in the PV phase. After that, we can calculate the ACU_
    Zero value with the telemetrized data. (comment Y.Terada)
 **/

static HxdTime ACU_zero_time_PVphase = {17,1618542592}; /* 2005-08-22 02:26:31.3 */
static double  aetime_valid_ACUZero;
static AtTimeD attime_valid_ACUZero = {2005, 10, 30, 21, 40, 00, 0.00};
static HxdBstidt bstidt_data;

static int bsttime_valid = HXDBSTTIME_ASGN_INVALID;
static int bstidt_rowid;

#define DEBUG 0

enum{
    TRN_TIME_MODE,
    TRN_TIME,
    TRN_GB_FLG,
    TRN_GB_FRZ
};

void
HXDfbstTime_startup(int *status){
  int used = 1;
  HxdTime2aetime_mode = TI_AETIME_MODE_III ; 
/*aetime_valid_ACUZero = attimeD2aste(&attime_valid_ACUZero); */
  BnkPut( "HXD:ftools:hxdbsttime_yn",       sizeof(int), &used);

  BnkDef("HXD:PIL:time_convert_mode",sizeof(int));
  
  *status = ANL_OK;
}

void
HXDfbstTime_com(int *status) {
  /*
    static char mode_tbl[4][256]={
        "MODE-(1)","MODE-(2-1)","MODE-(2-2)", "EXIT"
    };
    static char mode_help[4][256]={
        "use ASTE FUNCTION (TI to aetime, with time table)",
        "use telemetry aetime, and simply add sub_aetime"  ,
        "use telemetry aetime, and interpolate event aetime" ,
        "EXIT"
    };
    int answer[4];
  */    
    /************ ftools *************/
    if ( *status ) {
        
        *status = 0;
	/*
        UCLGSI("time_convert_mode", HxdTime2aetime_mode, *status);
	*/
	*status = PILGetInt("time_convert_mode", &HxdTime2aetime_mode);
        if ( *status ) {
	  fprintf(stderr, "%s: param time_convert_mode read failed (%d)\n",
		  pname, *status);
            *status = ANL_QUIT;
            exit(-1);
        } else {
	  BnkPut("HXD:PIL:time_convert_mode", sizeof(int),
		 &HxdTime2aetime_mode);
	}
        
        *status = ANL_OK;
        return;
    }

    *status = ANL_OK;
}

void
HXDfbstTime_init(int *status) {
    int board;
    int istat;
    int stat, size;
    char tim_filename[FILELIST_MAX_LENGTH];
    char bstidt_filename[FILELIST_MAX_LENGTH];

    /**** no need to BnkDef BST_EVTIME ****/
    
    /**** TIME is valid or not ****/
    BnkDef("HXDfbstTime:TIME_CORR", sizeof(int));

    /**** for TIMEDEL, time resolution ****/
    BnkDef("HXDfbstTime:TIME_RESOLUTION", sizeof(double));

    /**** status clear ****/
    timstat.before.board = TPU_NBOARD + 1;
    timstat.before.latch_DE_time = 0x0;
    timstat.before.latch_AE_time = 0x0;
    for(board=0;board<TPU_NBOARD;board++) {
	timstat.before.ti[board] = 0x0;
	timstat.before.trn_time[board] = 0x0;
	timstat.begin.trn      [board] = FIRST;
    }
    timstat.begin.hk  = FIRST;
    timstat.begin.sys = FIRST;

   BnkfGetM("HXDfbstTime:TIM_FILE_NAME",
            sizeof(tim_filename), &size, tim_filename);

    stat = hxdTime_init(tim_filename);
    if (stat != HXDTIMEUTIL_OK){
      fprintf(stderr, "%s: hxdTime Init error\n", pname);
      *status = ANL_QUIT;
      return;
    }

    aetime_valid_ACUZero = attimeD2aste(&attime_valid_ACUZero);


    BnkfGetM("HXDfbstTime:BSTIDT_FILE_NAME",
	     sizeof(bstidt_filename), &size, bstidt_filename);
    stat = hxdcaldbUtil_bstidt_open_FITS(bstidt_filename);
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: file open error\n", pname);
      *status = ANL_QUIT;
      return;
    }
    stat = hxdcaldbUtil_bstidt_read_FITS(&bstidt_data);
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: file read error\n", pname);
      *status = ANL_QUIT;
      return;
    }
    stat = hxdcaldbUtil_bstidt_close_FITS();
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: file close error\n", pname);
      *status = ANL_QUIT;
      return;
    }


    *status = ANL_OK;
    return;
}

void
HXDfbstTime_his(int *status){ *status = ANL_OK; }

void
HXDfbstTime_bgnrun(int *status){ *status = ANL_OK; }

void
HXDfbstTime_ana(int nevent, int eventid, int *status){
    int size, eventtype, length, istat;
    static HxdTime ACU_zero_time ={0,0};
/*  HxdTime tlm_time             ={0,0};*/
    HxdTime frzon_tlm_time             ={0,0};
    HxdTime bst_time             ={0,0};
    static unsigned int latch_DE_time = 0x00; /* 20 bit counter */
    static unsigned int latch_AE_time = 0x00; /* 24 bit counter */
    unsigned int telm_time=0x0;     /* Secondary Header (32-bit) TI  */
    unsigned int frzon_telm_time=0x0;
    unsigned int bst_freezed_time, extended_bst_freezed_time,TRN_time_counter;
    double bst_evtime, tlm_aetime, frzon_aetime;
    int board;
    int stat;

    /* ======= (I) Get Time_Latch_Time, BST time  ======= */
    BnkGet( "HXD:BST:BOARD",   sizeof(int),   &size, &board);
    BnkGet( "HXD:BST:PACKET_S_TIME",     sizeof(double),&size, 
	    &tlm_aetime);
    BnkGet( "HXD:BST:PACKET_SEC_HEADER", sizeof(int),   &size, 
	    &telm_time );
    timstat.board = board;

    /**
     ** Version 2.0.0 to 2.0.1 changes (using bstidt information).
     **   tlm_aetime ---> frzon_aetime
     **   telm_time  ---> frzon_telm_time
     **   tlm_time   ---> frzon_tlm_time
     **   latch_AE_time (telemetorized time --> Freeze ON time)
     **   latch_DE_time (telemetorized time --> Freeze ON time)
     **/
    /*
    BnkGet( "HXDfbstTime:HXD_AE_TM_LATCH_TM",  sizeof(int), &size, 
	    &latch_AE_time );
    BnkGet( "HXDfbstTime:HXD_SYS_LATCH_TI",    sizeof(int), &size, 
	    &latch_DE_time  );
    */
    stat = hxdcaldbUtil_bstidt_read_frzonTimeInfo(&bstidt_data, 
						  board, tlm_aetime, 
						  &frzon_aetime, 
						  &frzon_telm_time,
						  &latch_AE_time,
						  &latch_DE_time,
						  &bsttime_valid,
						  &bstidt_rowid);
    if (stat != HXDCALDBUTIL_STATUS_OK){
      fprintf(stderr, "%s: hxdbstTime read frzon time information failed\n", 
	      pname);
      *status = ANL_QUIT;
      return;
    }

    if(bsttime_valid == HXDBSTTIME_ASGN_INVALID){
      bst_frzd_tm[board] = 0.00000;
      *status = ANL_OK;
      return;
    } 

    /*
    tlm_time 
      = hxdTime_ti_to_HxdTime(&telm_time, &timstat);
    */
    frzon_tlm_time 
      = hxdTime_ti_to_HxdTime(&frzon_telm_time, &timstat);

    if (aetime_valid_ACUZero >= tlm_aetime) {
      /** PV Phase **/
      if(DEBUG) fprintf(stderr, "PV phase\n");
      ACU_zero_time = ACU_zero_time_PVphase;
    } else {
      /** after 2005/10/26 **/
      Calc_ACU_zero_time(&latch_AE_time, &latch_DE_time, &ACU_zero_time);
    }
    
    BnkGet( "HXD:BST:TIME",   sizeof(int), &size, &bst_freezed_time);

    
    /* ========== (II) burst time determination ======================== */
    /*** (2-1) pre time assignment (bst_freezed_time -> bst_time)***/
    timstat.BST_extended  = BST_EXTENDED_NO;
    timstat.TPU_time_mode = HXD_TPU_TIME_MODE_DEFAULT; /** must read **/
    /*
    hxdTime_bst_time_determination(bst_freezed_time, &timstat,
                                   ACU_zero_time, tlm_time, &bst_time);*/
    hxdTime_bst_time_determination(bst_freezed_time, &timstat,
				   ACU_zero_time, frzon_tlm_time, &bst_time);

    switch ( HxdTime2aetime_mode ) {
    case TI_AETIME_MODE_I:
	bst_evtime = hxdTime_HxdTime2aetime_I(bst_time);
        break;
    case TI_AETIME_MODE_II_I:
      /*
        bst_evtime = hxdTime_HxdTime2aetime_II_i (bst_time,
                                                  tlm_time, tlm_aetime);*/
        bst_evtime = hxdTime_HxdTime2aetime_II_i (bst_time,
                                                  frzon_tlm_time, frzon_aetime);
        break;
    case TI_AETIME_MODE_II_II:
      /*
	bst_evtime = hxdTime_HxdTime2aetime_II_ii(bst_time,
                                                  tlm_time, tlm_aetime);*/
	bst_evtime = hxdTime_HxdTime2aetime_II_ii(bst_time,
                                                  frzon_tlm_time, frzon_aetime);
						  
        break;
    case TI_AETIME_MODE_III:
      /*bst_evtime = hxdTime_HxdTime2aetime_III(bst_time, tlm_aetime);*/
        bst_evtime = hxdTime_HxdTime2aetime_III(bst_time, frzon_aetime);
        break;
    default:
        fprintf(stderr,"HXDfbstTime: TI to aetime, no mode selected!\n");
	/*
        bst_evtime = hxdTime_HxdTime2aetime_II_i (bst_time,
	                                          tlm_time, tlm_aetime);*/
        bst_evtime = hxdTime_HxdTime2aetime_II_i (bst_time,
						  frzon_tlm_time, frzon_aetime);
        break;
    }

    /*** Time assign failure in hxdTime_HxdTime2aetime(). ***/
    if(bst_evtime == HXD_TIME_INVALID_AETIME){
      bsttime_valid = HXDBSTTIME_ASGN_INVALID;
    }

    /* ============ (III) output files or hbook ( or fits ) =============== */
    bst_frzd_tm[board] = bst_evtime;

    *status = ANL_OK;
    return;
}

void
HXDfbstTime_endrun(int *status){ 
  double timedel;
  double timedel_tpu[4];
  double timedel_sum_us = 0.0;
  int board;
  int alive_board_num = 0;
  char bstidt_filename[FILELIST_MAX_LENGTH];
  int stat, size;
  int access_caldb;

  BnkPut("HXD:BST:FRZD_TM", sizeof(double)*4, &bst_frzd_tm);
  BnkPut("HXDfbstTime:TIME_CORR", sizeof(int), &bsttime_valid);
  BnkfGetM("HXD:PIL:access_caldb",sizeof(int), &size, &access_caldb);
  BnkfGetM("HXDfbstTime:BSTIDT_FILE_NAME",
	   sizeof(bstidt_filename), &size, bstidt_filename);
 
  if(!access_caldb){
    if(bsttime_valid == HXDBSTTIME_ASGN_VALID){
      stat = hxdcaldbUtil_bstidt_update_by_hxdbsttime(bstidt_filename,
						      bstidt_rowid,
						      bst_frzd_tm);
      if (stat != HXDCALDBUTIL_STATUS_OK){
	fprintf(stderr, "%s: hxdbstTime update bstidt failed\n", pname);
	*status = ANL_QUIT;
	return;
      }
      fprintf(stdout,
	      "[INFO]%s: BSTIDT(%s) is updated.\n", pname, bstidt_filename);
    } else{
      fprintf(stdout,
	      "[INFO]%s: BSTIDT(%s) is not updated, because of invalid time assignment.\n", 
	      pname, bstidt_filename);
    }
  } else {
      fprintf(stdout,
	      "[INFO]%s: BSTIDT(%s) is not updated, because the file is in CALDB.\n", 
	      pname, bstidt_filename);
  }

  for (board=0; board<4; board++) {
    timedel_tpu[board] 
      = hxdTime_Resolution_calc_TIMEDEL(timstat.time_shift, board);
    if (timedel_tpu[board] > 0.0){
      timedel_sum_us += timedel_tpu[board]*1000000;
      alive_board_num ++;
    }
  }
  timedel = timedel_sum_us / (double) alive_board_num / 1000000;

  BnkPut("HXDfbstTime:TIME_RESOLUTION", sizeof(double), &timedel);

  *status = ANL_OK; 
}

void
HXDfbstTime_exit(int *status){ *status = ANL_OK; }


/*********************  Static Functions *************************************/
/*** the same function: HXDfwelTime (version 0.1.2) ***/
static void
Calc_ACU_zero_time(unsigned int *latch_AE_time,
		   unsigned int *latch_DE_time,
		   HxdTime *ACU_zero_time       ){
    HxdTime ACU_zero_time_dummy;   /* 32 bit counter : upper 15 bit (sec)*/

    if (*latch_AE_time == timstat.before.latch_AE_time &&
	*latch_DE_time == timstat.before.latch_DE_time &&
	timstat.begin.hk == NOT_FIRST ){
	; /* do nothing: no change ACU_zero_time */
    } else {
	/***  calc ACU counter Zero_Reset Time ***/
	ACU_zero_time_dummy.lower =
	    ( (*latch_DE_time << HXD_TIME_COUNTER_Sec_SHIFT) & 0x7fffffff )
          - ( (*latch_AE_time << HXD_TIME_COUNTER_030_SHIFT) & 0x7fffffff );
	ACU_zero_time_dummy.upper =
          *latch_DE_time>>
	      (HXD_TIME_COUNTER_Up5_SHIFT - HXD_TIME_COUNTER_Sec_SHIFT);
	/*** ACU zero_time correction ************/
	*ACU_zero_time = hxdTime_ACU_zero_time_correct( &ACU_zero_time_dummy );
	if (DEBUG) fprintf(stderr,  "HXDfbstTime: LATCH DE:%x AE:%x\n",
			   (*latch_DE_time),(*latch_AE_time));
	timstat.before.latch_AE_time = *latch_AE_time;
	timstat.before.latch_DE_time = *latch_DE_time;
    }
}
/* ======================= End of HXDfbstTime.c =========================== */

