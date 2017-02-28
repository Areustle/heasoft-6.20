/*
 *  HXDftrnTime.c
 *       version 0.0.1              Y.Terada 1999.10.08
 *       + hxdTimeUtil version 0.6.2
 *       version 0.0.3              Y.Terada 1999.12.23
 *       + hxdTimeUtil version 0.6.4
 *       version 0.2.0             Y.Terada  2003-07-23 
 *         for HEADAS, PIL
 *       version 0.2.1             Y.Terada  2004-03-13
 *         Quit (FITS)
 *       version 0.2.2             Y.Terada  2005-02-23
 *         hxdTimeUtil, version 0.8.1, astetool v1.31
 *       version 0.2.3             R.Miyawaki 2005.05.28
 *                 Change PACKET_AETIME -> PACKET_S_TIME
 *                 hxdTimeUtil, version 0.8.2, Read tim_file
 *       version 0.2.4             Y.Terada 2005.06.03
 *                 calc TIMEDEL (time resolution).
 *       version 0.2.5             Y.Terada 2005.06.08
 *                 debug TIMEDEL
 *       version 0.2.7             Y.Terada 2005.06.13
 *                 write TRTIME unit
 *       version 0.2.8             Y.Terada 2005.06.14
 *                 update TRN, even when time is invalid.
 *       version 0.2.9           R Miyawaki 2005.08.25
 *                 delete if-sentence for HK&SYS BnkGet          
 *       version 0.3.0             M.Kokubun 2005.09.12
 *                 default conversion mode from II-I to III
 *       version 0.3.1             Y.Terada 2005.09.26
 *                 pre-search in HK FITS, before _ana process.
 *       version 0.3.2             Y.Terada 2005.11.05
 *                 put PIL value into FITS header
 *       version 0.3.3             Y.Terada 2005.11.08
 *                 shorten Bnk name
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

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"

#include "hxdTime.h"
#include "hxdTimeUtil.h"
#include "HXDftrnTime.h"
#include "hxdFitsHeaderUtil.h"

char HXDftrnTime_version[] = "version 0.3.3";
static char pname[] = "HXDftrnTime";

static HxdTimeStatus timstat;

static int HxdTime2aetime_mode = 0;
#define TI_AETIME_MODE_I         1
#define TI_AETIME_MODE_II_I      2
#define TI_AETIME_MODE_II_II     3
#define TI_AETIME_MODE_III       4

#define TIME_VALID   0
#define TIME_INVALID 1

#define FILELIST_MAX_LENGTH 256

#define DEBUG 0

void
HXDftrnTime_startup(int *status){
  int used = 1;
  HxdTime2aetime_mode = TI_AETIME_MODE_III ;
  BnkPut( "HXD:ftools:hxdwamtime_yn",       sizeof(int), &used);
  BnkDef("HXD:PIL:time_convert_mode",sizeof(int));
  *status = ANL_OK;
}

void
HXDftrnTime_com(int *status) {
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
    if ( *status ) {	/************ ftools *************/
        
        *status = 0;
	/*
        UCLGSI("time_convert_mode", HxdTime2aetime_mode, *status);
	*/
	*status = PILGetInt("time_convert_mode", &HxdTime2aetime_mode);
        if ( *status ) {
	  fprintf(stderr, "%s:PILGetInt time_convert_mode err (%d)\n",
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
    /************ ql *************/

    *status = ANL_OK;
}

void
HXDftrnTime_init(int *status) {
    int board;
    int stat, size;
    char tim_filename[FILELIST_MAX_LENGTH];
    /********* Define at hxdtrnFitsToBnkUtil *********/
/*  BnkDef("HXD:TRN:EV_TIME", sizeof(double));*/
    
    BnkDef("HXDftrnTime:TIME_INVALID", sizeof(int));
/*  BnkDef("HXDftrnTime:EV_TIME_TLM", sizeof(double));*/ /* debug */

    /**** for TIMEDEL, time resolution ****/
    BnkDef("HXDftrnTime:TIME_RESOLUTION", sizeof(double));
    
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
    
   BnkfGetM("HXDftrnTime:TIM_FILE_NAME",
            sizeof(tim_filename), &size, tim_filename);
   
   stat = hxdTime_init(tim_filename);
    if (stat != HXDTIMEUTIL_OK){
      fprintf(stderr, "%s: hxdTime Init error\n", pname);
      *status = ANL_QUIT;
      return;
    }

    *status = ANL_OK;
}

void
HXDftrnTime_his(int *status){ *status = ANL_OK; }

void
HXDftrnTime_bgnrun(int *status){ *status = ANL_OK; }

void
HXDftrnTime_ana(int nevent, int eventid, int *status){
    int size;
/*  int eventtype, length; */
    int time_invalid,hxd_packet_update_flag;
    static HxdTime ACU_zero_time ={0,0};
    HxdTime tlm_time             ={0,0};
    HxdTime trn_time             ={0,0};
    static unsigned int latch_DE_time = 0x00; /* 20 bit counter */
    static unsigned int latch_AE_time = 0x00; /* 24 bit counter */
    unsigned int telm_time=0x0;/* Secondary Header (32-bit) TI  */
    unsigned int TPU_time_counter;
    int board,block_num, block_cnt;
    double trn_evtime, tlm_aetime;
    
    /* ======= Get Time_Latch_Time, TRN_time_mode TRN packet Time ======= */
    BnkGet("HXD:ALL:UPDATE",sizeof(int), &size, &hxd_packet_update_flag);
    
    if ( hxd_packet_update_flag & HXD_UPDATE_HK ) {
	BnkGet( "HXDftrnTime:HXD_AE_TM_LATCH_TM", sizeof(int),&size,&latch_AE_time);
	Calc_ACU_zero_time(&latch_AE_time, &latch_DE_time, &ACU_zero_time);
	if( timstat.begin.hk == FIRST ) timstat.begin.hk = NOT_FIRST;
    }
    
    if ( hxd_packet_update_flag & HXD_UPDATE_SYS ) {
	BnkGet( "HXDftrnTime:HXD_SYS_LATCH_TI", sizeof(int), &size, &latch_DE_time);
	Calc_ACU_zero_time(&latch_AE_time, &latch_DE_time, &ACU_zero_time);
	if( timstat.begin.sys == FIRST ) timstat.begin.sys = NOT_FIRST;
    }

    if ( !(hxd_packet_update_flag & HXD_UPDATE_TRN)||  /* 1 = GoodTimeRegion */
	timstat.begin.hk == FIRST || timstat.begin.sys == FIRST ) {
	/***** poor information for time determination *****/
	time_invalid = TIME_INVALID;
	trn_evtime=0.0;
	hxd_packet_update_flag |= HXD_UPDATE_TRN;
	BnkPut("HXD:ALL:UPDATE",sizeof(int), &hxd_packet_update_flag);
	goto END_PROCESS; 
    } else {
	time_invalid = TIME_VALID;
    }

    BnkGet( "HXD:TRN:PACKET_S_TIME",     sizeof(double),&size, &tlm_aetime);
    BnkGet( "HXD:TRN:PACKET_SEC_HEADER", sizeof(int),   &size, &telm_time );
    tlm_time = hxdTime_ti_to_HxdTime(&telm_time, &timstat); /*int -> hxdTime */
    BnkGet("HXD:TRH:TIME",     sizeof(int),   &size, &TPU_time_counter  );
    BnkGet("HXD:TRH:TIME_MODE",sizeof(int),   &size, &timstat.TPU_time_mode);
    BnkGet("HXD:TRN:BOARD",    sizeof(int),   &size, &timstat.board);
    BnkGet("HXD:TRN:BLOCK",    sizeof(int),   &size, &block_cnt );
    BnkGet("HXD:TRB:IBLOCK",   sizeof(int),   &size, &block_num );

    if(DEBUG) fprintf(stderr,"tlm_aetime=%f,TI=0x%X ",tlm_aetime,telm_time);
    if(DEBUG) fprintf(stderr,"time=0x%X, mode=%d, board=%d, blk=%d, iblk=%d\n",
		      TPU_time_counter,timstat.TPU_time_mode,
		      timstat.board,block_cnt,block_num);
    
    /* ==========  transient time determination ======================== */
    
    hxdTime_trn_time_determination(TPU_time_counter, &timstat,
				   ACU_zero_time, tlm_time, &trn_time);
    
    hxdTime_trn_time_block_correction(block_cnt, block_num, &timstat, &trn_time);

    switch ( HxdTime2aetime_mode ) {
    case TI_AETIME_MODE_I:
	trn_evtime = hxdTime_HxdTime2aetime_I(trn_time);
        break;
    case TI_AETIME_MODE_II_I:
        trn_evtime = hxdTime_HxdTime2aetime_II_i (trn_time,
                                                   tlm_time, tlm_aetime);
        break;
    case TI_AETIME_MODE_II_II:
	trn_evtime = hxdTime_HxdTime2aetime_II_ii(trn_time,
						  tlm_time, tlm_aetime);
        break;
    case TI_AETIME_MODE_III:
	trn_evtime = hxdTime_HxdTime2aetime_III(trn_time, tlm_aetime);
        break;
    default:
        fprintf(stderr,"HXDftrnTime: TI to aetime, no mode selected!\n");
        trn_evtime = hxdTime_HxdTime2aetime_II_i (trn_time,
						  tlm_time, tlm_aetime);
        break;
    }

    /* ============ output files or hbook ( or fits ) ================= */
    /*** prepare the next ***/
    board = timstat.board;
    timstat.before.tlm_time [board] = tlm_time;
    timstat.before.ti       [board] = telm_time;
    timstat.before.trn_time[board]  = TPU_time_counter;
    timstat.before.board            = board;
    if ( timstat.begin.trn[board] == FIRST ) timstat.begin.trn[board] = NOT_FIRST;
    if (latch_AE_time >0)
      hxdTime_Resolution_record(&trn_time, &trn_evtime, board);

    /*** OUTPUT ***/
END_PROCESS:
    BnkPut("HXD:TRN:EV_TIME", sizeof(double), &trn_evtime);
    BnkPut("HXDftrnTime:TIME_INVALID", sizeof(int), &time_invalid);

    if(DEBUG) fprintf(stderr,
		      "HXDftrnTime:debug:aetime = %f, EV_TIME=%f VALID=%d\n",
		      tlm_aetime, trn_evtime, time_invalid);
    *status = ANL_OK;
    return;
}

void
HXDftrnTime_endrun(int *status){ 
  double timedel;
  double timedel_tpu[4];
  double timedel_sum_us = 0.0;
  int board;
  int alive_board_num = 0;

  for (board=0; board<4; board++) {
    timedel_tpu[board] 
      = hxdTime_Resolution_calc_TIMEDEL(timstat.time_shift, board);
    if (timedel_tpu[board] > 0.0){
      timedel_sum_us += timedel_tpu[board]*1000000;
      alive_board_num ++;
    }
  }
  timedel = timedel_sum_us / (double) alive_board_num / 1000000;

  BnkPut("HXDftrnTime:TIME_RESOLUTION", sizeof(double), &timedel);
  
  BnkDef("HXDftrnTime:TIME_MODE", sizeof(int));
  BnkPut("HXDftrnTime:TIME_MODE", sizeof(int), &timstat.TPU_time_mode);

  *status = ANL_OK; 
}

void
HXDftrnTime_exit(int *status){ *status = ANL_OK; }


/*********************  Static Functions *************************************/
/*** HXDfwelTime (version 0.1.2) ***/
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
/*	fprintf(stderr,
		"HXDftrnTime: LATCH DE:%x AE:%x\n",
		(*latch_DE_time),(*latch_AE_time));*/
	timstat.before.latch_AE_time = *latch_AE_time;
	timstat.before.latch_DE_time = *latch_DE_time;
    }
    
}
/* ======================= End of HXDftrnTime.c =========================== */
