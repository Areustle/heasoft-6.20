/*
 *  HXDfsclTime.c
 *       version 0.0.1              Y.Terada 1999.12.13
 *       + hxdTimeUtil version 0.6.3
 *       version 0.0.2              Y.Terada 1999.12.23
 *       + hxdTimeUtil version 0.6.4
 *       version 0.0.4              Y.Fukazawa 2000.02.03
 *       + hxdTimeUtil version 0.6.5    add EV_TIME_TLM
 *       version 0.2.0              Y.Terada 2003.07.23
 *         for HEADas and PIL
 *       version 0.2.1              Y.Terada 2004.03.11
 *         Add warning when time is out_of_range.
 *       version 0.2.4             Y.Terada  2005-02-23
 *         hxdTimeUtil, version 0.8.1, astetool v1.31
 *       version 0.2.5             R.Miyawaki 2005.05.28
 *                 Change PACKET_AETIME -> PACKET_S_TIME
 *                 hxdTimeUtil, version 0.8.2, Read tim_file
 *       version 0.2.6             Y.Terada  2005-06-03
 *                 add TIMEDEL.
 *       version 0.2.7             Y.Terada  2005-06-08
 *                 debug TIMEDEL
 *       version 0.2.8             Y.Terada  2005-06-14
 *                 update SCL, even when time is invalid
 *       version 0.3.0            R Miyawaki 2005.08.25
 *                 delete if-sentence for HK&SYS BnkGet          
 *       version 0.3.1            M.Kokubun 2005.09.12
 *                 default conversion mode from II-I to III
 *       version 0.3.2            Y.Terada  2005.11.05
 *                 write PIL parameters
 *       version 0.3.3            Y.Terada  2005.11.08
 *                 shorten Bnk name
 *       version 0.3.7            Y.Terada  2006.08.23
 *       version 0.3.8            Y.Terada  2006.08.25
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
#include "HXDfsclTime.h"
#define DEBUG 0
#include "hxdFitsHeaderUtil.h"

char HXDfsclTime_version[] = "version 0.3.8";
static char pname[] = "HXDfsclTime";

static HxdTimeStatus timstat;

static int HxdTime2aetime_mode = 0;
#define TI_AETIME_MODE_I         1
#define TI_AETIME_MODE_II_I      2
#define TI_AETIME_MODE_II_II     3
#define TI_AETIME_MODE_III       4

#define TIME_VALID   0
#define TIME_INVALID 1

#define FILELIST_MAX_LENGTH 256

void
HXDfsclTime_startup(int *status){
  int used = 1;
  HxdTime2aetime_mode = TI_AETIME_MODE_III ; /** default **/
  BnkPut( "HXD:ftools:hxdscltime_yn",       sizeof(int), &used);
  BnkDef("HXD:PIL:time_convert_mode",sizeof(int));
  *status = ANL_OK;
}

void
HXDfsclTime_com(int *status) {
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
HXDfsclTime_init(int *status) {
    int board;
    int stat, size;
    char tim_filename[FILELIST_MAX_LENGTH];

    /********* Define at HXDsclFitsRead *********/
/*  BnkDef("HXD:SCL:EV_TIME", sizeof(double));*/
    
    BnkDef("HXDfsclTime:TIME_INVALID", sizeof(int));
    BnkDef("HXDfsclTime:EV_TIME_TLM", sizeof(double));

    /**** for TIMEDEL, time resolution ****/
    BnkDef("HXDfsclTime:TIME_RESOLUTION", sizeof(double));

    /**** status clear ****/
    timstat.before.board = WPU_NBOARD + 1;
    timstat.before.latch_DE_time = 0x0;
    timstat.before.latch_AE_time = 0x0;
    for(board=0;board<WPU_NBOARD;board++) {
		timstat.before.ti[board] = 0x0;
		timstat.before.scl_time[board] = 0x0;
		timstat.begin.scl      [board] = FIRST;
    }
    timstat.begin.hk  = FIRST;
    timstat.begin.sys = FIRST;

   BnkfGetM("HXDfsclTime:TIM_FILE_NAME",
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
HXDfsclTime_his(int *status){ *status = ANL_OK; }

void
HXDfsclTime_bgnrun(int *status){ *status = ANL_OK; }

void
HXDfsclTime_ana(int nevent, int eventid, int *status){
    int size, eventtype, length;
    int time_invalid,hxd_packet_update_flag;
    static HxdTime ACU_zero_time ={0,0};
    HxdTime tlm_time             ={0,0};
    HxdTime scl_time             ={0,0};
    static unsigned int latch_DE_time = 0x00; /* 20 bit counter */
    static unsigned int latch_AE_time = 0x00; /* 24 bit counter */
    unsigned int telm_time=0x0;/* Secondary Header (32-bit) TI  */
    double scl_evtime, tlm_aetime;
    unsigned int WPU_time_counter;
    int board;
    
    if (DEBUG) fprintf(stdout, "%s_ana: ANA Begin-\n",  pname);
    
    /* ======= Get Time_Latch_Time, SCL packet Time ======= */
    BnkGet("HXD:ALL:UPDATE",sizeof(int), &size, &hxd_packet_update_flag);
    
    if ( hxd_packet_update_flag & HXD_UPDATE_HK ) {
	BnkGet("HXDfsclTime:HXD_WPU_CLK_RATE", sizeof(int), &size, &timstat.WPU_clock_rate);
	BnkGet( "HXDfsclTime:HXD_AE_TM_LATCH_TM", sizeof(int),&size,&latch_AE_time);
	Calc_ACU_zero_time(&latch_AE_time, &latch_DE_time, &ACU_zero_time);
	if( timstat.begin.hk == FIRST ) timstat.begin.hk = NOT_FIRST;
    }
    
    if ( hxd_packet_update_flag & HXD_UPDATE_SYS ) {
	BnkGet( "HXDfsclTime:HXD_SYS_LATCH_TI", sizeof(int), &size, &latch_DE_time);
	Calc_ACU_zero_time(&latch_AE_time, &latch_DE_time, &ACU_zero_time);
	if( timstat.begin.sys == FIRST ) timstat.begin.sys = NOT_FIRST;
    }
    
    if ( !(hxd_packet_update_flag & HXD_UPDATE_SCL)||  /* 1 = GoodTimeRegion */
	timstat.begin.hk == FIRST || timstat.begin.sys == FIRST ) {
	/***** poor information for time determination *****/
	time_invalid = TIME_INVALID;	
	scl_evtime=0.0;
	hxd_packet_update_flag |= HXD_UPDATE_SCL;
	BnkPut("HXD:ALL:UPDATE",sizeof(int), &hxd_packet_update_flag);
	goto END_PROCESS; 
    } else {
	time_invalid = TIME_VALID;
    }
    
    if (DEBUG) fprintf(stdout, "%s_ana: ANA read zerotime.\n",  pname);
    
    BnkGet( "HXD:SCL:PACKET_S_TIME",     sizeof(double),&size, &tlm_aetime);
    BnkGet( "HXD:SCL:PACKET_SEC_HEADER", sizeof(int),   &size, &telm_time );
    BnkGet("HXD:SCL:TIME",     sizeof(int),   &size, &WPU_time_counter  );
    BnkGet("HXD:SCL:BOARD",    sizeof(int),   &size, &board);
    timstat.board = board;
    if (timstat.board<0 || WPU_NBOARD<timstat.board){
      fprintf(stderr, "%s: invalid board id = %d\n", pname, timstat.board);
      *status = ANL_NG; return;
    }

    tlm_time = hxdTime_ti_to_HxdTime(&telm_time, &timstat); /*int -> hxdTime */
    
    /* ==========  transient time determination ======================== */
    
    hxdTime_scl_time_determination(WPU_time_counter, &timstat,
				   ACU_zero_time, tlm_time, &scl_time);
    scl_evtime = hxdTime_HxdTime2DoubleTime(scl_time);
    BnkPut("HXDfsclTime:EV_TIME_TLM", sizeof(double), &scl_evtime);

    switch ( HxdTime2aetime_mode ) {
    case TI_AETIME_MODE_I:
	scl_evtime = hxdTime_HxdTime2aetime_I(scl_time);
        break;
    case TI_AETIME_MODE_II_I:
        scl_evtime = hxdTime_HxdTime2aetime_II_i (scl_time,
						  tlm_time, tlm_aetime);
        break;
    case TI_AETIME_MODE_II_II:
	scl_evtime = hxdTime_HxdTime2aetime_II_ii(scl_time,
						  tlm_time, tlm_aetime);
        break;
    case TI_AETIME_MODE_III:
        scl_evtime = hxdTime_HxdTime2aetime_III(scl_time, tlm_aetime);
        break;
    default:
        fprintf(stderr,"HXDfsclTime: TI to aetime, no mode selected!\n");
        scl_evtime = hxdTime_HxdTime2aetime_II_i (scl_time,
						  tlm_time, tlm_aetime);
        break;
    }
    
    /* ============ output files or hbook ( or fits ) ================= */
    /*** prepare the next ***/
    timstat.before.tlm_time [board] = tlm_time;
    timstat.before.ti       [board] = telm_time;
    timstat.before.scl_time[board]  = WPU_time_counter;
    timstat.before.board            = board;
    if ( timstat.begin.scl[board] == FIRST ) timstat.begin.scl[board] = NOT_FIRST;

    if(latch_AE_time > 0)
      hxdTime_Resolution_record(&scl_time, &scl_evtime, board);

    /*** OUTPUT ***/
 END_PROCESS:
    BnkPut("HXD:SCL:EV_TIME", sizeof(double), &scl_evtime);
    BnkPut("HXDfsclTime:TIME_INVALID", sizeof(int), &time_invalid);
    
    if (DEBUG) fprintf(stdout, "%s_ana: ANA End\n",  pname);
    *status = ANL_OK;
    return;
}

void
HXDfsclTime_endrun(int *status){
  double timedel;
  double timedel_wpu[4];
  double timedel_sum_us = 0.0;
  int board;
  int alive_board_num = 0;

  for (board=0; board<4; board++) {
    timedel_wpu[board] 
      = hxdTime_Resolution_calc_TIMEDEL(timstat.time_shift, board);
    if (timedel_wpu[board] > 0.0){
      timedel_sum_us += timedel_wpu[board]*1000000;
      alive_board_num ++;
    }
  }

  if(alive_board_num == 0) {
    timedel = 3.051757E-5; /** default value **/
  } else {
    timedel = timedel_sum_us / (double) alive_board_num / 1000000;
  }

  BnkPut("HXDfsclTime:TIME_RESOLUTION", sizeof(double), &timedel);

  *status = ANL_OK; 
}

void
HXDfsclTime_exit(int *status){ *status = ANL_OK; }

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
		timstat.before.latch_AE_time = *latch_AE_time;
		timstat.before.latch_DE_time = *latch_DE_time;
    }
    
}
/* ======================= End of HXDfsclTime.c =========================== */
