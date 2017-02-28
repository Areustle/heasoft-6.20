/*
 *  HXDfwelTime.c
 *       version 0.0.3              Y.Terada 1999.08.03
 *  [function ]    hxdTimeUtil version 0.3
 *       version 0.0.4              Y.Terada 1999.08.09
 *  [function ]    hxdTimeUtil version 0.5
 *                 ANL_SKIP for time invaliant event.
 *       version 0.0.5              Y.Terada 1999.08.14
 *  [function ]    hxdTimeUtil version 0.6
 *                 rename functions.
 *       version 0.1.0              Y.Terada 1999.08.15
 *  [function ]    hxdTimeUtil version 0.6
 *                 support aste_get_hk();
 *                 read "WEL event UPDATE." (read GTI)
 *       version 0.1.1              Y.Fukazawa 1999.09.23
 *  [function ]    hxdTimeUtil version 0.6.1
 *                 BUG fix 
 *                 bnkdef HXDfwelTime:EV_TIME_TLM (Origin is the first tlmtime)
 *       version 0.1.4              Y.Terada 1999.12.23
 *                 change param name in hxdtime.par for the third-release.
 *  [function ]    hxdTimeUtil version 0.6.4
 *                 add 1.94 usec (HXD-DE and DP time latch correction)
 *        version 0.1.5             Y.Terada 2003.04.15
 *                 pre HXD-II format 
 *        version 0.1.6             Y.Terada 2003.05.03
 *                 HXD-II I/O, read PWH.
 *        version 0.2.0             Y.Terada, H Takahasi, M Suzuki 2003.07.23
 *                 for HEADAS
 *                     debug thanks to Y. Ishisaki!!
 *        version 0.2.1             Y.Terada 2004.03.13  
 *                 Quit, only when time is over HK time range. (FITS)
 *        version 0.2.3             Y.Terada 2005.02.07
 *                 Delete needless debug-codes.
 *        version 0.2.4             Y.Terada 2005.02.23
 *                 hxdTimeUtil, version 0.8.1, astetool v1.31
 *        version 0.2.5             R.Miyawaki 2005.05.28
 *                 Change PACKET_AETIME -> PACKET_S_TIME
 *                 hxdTimeUtil, version 0.8.2, Read tim_file
 *        version 0.2.6             Y.Terada 2005.06.03
 *                 calc TIMEDEL (time resolution).
 *        version 0.2.7             Y.Terada 2005.06.03
 *                 debug TIMEDEL, record in the same WPU.
 *        version 0.2.8  Y.Terada,R Miyawaki 2005.06.14
 *                 update WELL, even when time is invalid.
 *        version 0.2.9           R Miyawaki 2005.08.25
 *                 delete if-sentence for HK&SYS BnkGet
 *        version 0.3.0           M.Kokubun 2005.09.10
 *                 update event.time for later modules
 *        version 0.3.1           M.Kokubun 2005.09.12
 *                 set default conversion mode from II_I to III
 *        version 0.3.2           Y.Terada  2005.11.04
 *                 Put PIL parameters into Bnk.
 *        version 0.3.3           Y.Terada  2005.11.05
 *                 Put PIL parameters into Bnk, debug
 *        version 0.3.4           Y.Terada  2005.11.08
 *        version 0.3.5           H.Takahashi 2006.02.04
 *                 Invarid value 0.0 -> define
 *        version 0.3.6           Y.Terada  2006.08.18
 *                 Save Memory
 *        version 0.3.7           Y.Terada  2006.08.23
 *                 bug fixed
 *        version 0.3.8           Y.Terada  2006.08.24
 *          ---> v1.0.0
 *        version 2.0.0           Y.Terada  2006-09-10 
 *                 Version 2.0 format
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
/*#include <pfile.h>*/

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"

#include "hxdTime.h"
#include "hxdTimeUtil.h"
#include "HXDfwelTime.h"

char HXDfwelTime_version[] = "version 2.0.0";
static char pname[] = "HXDfwelTime";

static double aetime_border;
/* static HxdTimeStatus timstat;*/
static HxdTimeStatus *timstat = NULL;
static int HxdTime2aetime_mode = 0;
#define TI_AETIME_MODE_I         1
#define TI_AETIME_MODE_II_I      2
#define TI_AETIME_MODE_II_II     3
#define TI_AETIME_MODE_III       4

#define TIME_VALID   0
#define TIME_INVALID 1

#define FILELIST_MAX_LENGTH 256

#define HXD_TIME_INVALID 0.0

static int hxdfweltime_use_pwh;

void
HXDfwelTime_startup(int *status){
  int use = 1;
  HxdTime2aetime_mode = TI_AETIME_MODE_III ; 
  BnkDef("HXD:PIL:time_convert_mode", sizeof(int));
  BnkPut("HXD:ftools:hxdtime_yn",       sizeof(int), &use);

  if (timstat == NULL){
    timstat = (HxdTimeStatus*) malloc(sizeof(HxdTimeStatus));
  } else {
    fprintf(stderr, "%s: Error in memory allocation of timstat\n", pname);
    *status = ANL_NG;
    return;
  }

  *status = ANL_OK;
}

void
HXDfwelTime_com(int *status) {
    static char mode_tbl[4][256]={
        "MODE-(1)","MODE-(2-1)","MODE-(2-2)", "EXIT"
    };
    static char mode_help[4][256]={
        "use ASTE FUCTION (TI to aetime, with time table)",
        "use telemetry aetime, and simply add sub_aetime"  ,
        "use telemetry aetime, and superpose event aetime" ,
        "EXIT"
    };
    int answer[4];
        
    /************ ftools *************/
    if ( *status ) {
        
        *status = 0;
	/*
        UCLGSI("time_convert_mode", HxdTime2aetime_mode, *status);
	*/
	*status = PILGetInt("time_convert_mode", &HxdTime2aetime_mode);
        if ( *status ) {
	  fprintf(stderr, "%s: error in PILGetInt time_convert_mode (%d)n",
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
    
    /************ hxdql *************/
/*
   INQUIRE(" HxdTime2aetime mode?", 4, mode_tbl, mode_help, 1, answer);
    switch( answer[1] ){
    case 1:  HxdTime2aetime_mode = TI_AETIME_MODE_I    ; break;
    case 2:  HxdTime2aetime_mode = TI_AETIME_MODE_II_I ; break;
    case 3:  HxdTime2aetime_mode = TI_AETIME_MODE_II_II; break;
    case 4:  break;
    }
*/
    *status = ANL_OK;
}

void
HXDfwelTime_init(int *status) {
    int board;
    int stat, size;
    char tim_filename[FILELIST_MAX_LENGTH];

    /**** no need to BnkDef ****/
    BnkDef("HXDfwelTime:TIME_INVALID", sizeof(int));
    BnkDef("HXDfwelTime:EV_TIME_TLM", sizeof(double));

    /**** for TIMEDEL, time resolution ****/
    BnkDef("HXDfwelTime:TIME_RESOLUTION", sizeof(double));

    /**** CLK_RATE time border is 1999 3/1 ****/
    aetime_border = hxdTime_get_aetime("990301000000");

    /**** status clear ****/
    timstat->before.board = WPU_NBOARD + 1;
    timstat->before.latch_DE_time = 0x0;
    timstat->before.latch_AE_time = 0x0;
    for(board=0;board<WPU_NBOARD;board++) {
	timstat->before.ti[board] = 0x0;
	timstat->before.wel_time[board] = 0x0;
	timstat->begin.wel      [board] = FIRST;
    }
    timstat->begin.hk  = FIRST;
    timstat->begin.sys = FIRST;

   BnkfGetM("HXDfwelTime:TIM_FILE_NAME",
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
HXDfwelTime_his(int *status){ *status = ANL_OK; }

void
HXDfwelTime_bgnrun(int *status){ 
  int size;
  
  BnkfGetM("HXDeventFitsRead:USE_PWH", sizeof(int), 
	   &size, &hxdfweltime_use_pwh);
  
  *status = ANL_OK; 
}

void
HXDfwelTime_ana(int nevent, int eventid, int *status){
    int size, eventtype, length, time_invalid;
    static HxdTime ACU_zero_time ={0,0};
    HxdTime tlm_time             ={0,0};
    HxdTime well_event_time      ={0,0};
    static unsigned int latch_DE_time = 0x00; /* 20 bit counter :      all bit (sec)*/
    static unsigned int latch_AE_time = 0x00; /* 24 bit counter : upper  7 bit (sec)*/
    unsigned int WPU_time_counter;  /* 19 bit counter : mode changes */
    unsigned int telm_time=0x0;     /* Secondary Header (32-bit) TI  */
    int unit, board;
    double well_evtime, tlm_aetime;
    int hxd_packet_update_flag;
    int WPU_clock_rate;

    int ti, mti;
    int pwh[HXD_PWH_DATANUM];
    HxdEventFits02 event_data;

    /* ======= Get Time_Latch_Time, WPU_counter, Well packet Time ======= */
    BnkGet("HXD:ALL:UPDATE",sizeof(int), &size, &hxd_packet_update_flag);
    if ( hxd_packet_update_flag & HXD_UPDATE_HK ) {
	BnkGet( "HXDfwelTime:HXD_AE_TM_LATCH_TM", sizeof(int), &size, &latch_AE_time);
	Calc_ACU_zero_time(&latch_AE_time, &latch_DE_time, &ACU_zero_time);
	if( timstat->begin.hk == FIRST ) timstat->begin.hk = NOT_FIRST;
    }
    
    if ( hxd_packet_update_flag & HXD_UPDATE_SYS ) {
	BnkGet( "HXDfwelTime:HXD_SYS_LATCH_TI",    sizeof(int), &size, &latch_DE_time);
	Calc_ACU_zero_time(&latch_AE_time, &latch_DE_time, &ACU_zero_time);
	if( timstat->begin.sys == FIRST ) timstat->begin.sys = NOT_FIRST;
    }

    if ( !(hxd_packet_update_flag & HXD_UPDATE_WEL)||  /* 1 = GoodTimeRegion */
	timstat->begin.hk == FIRST || timstat->begin.sys == FIRST ) {
	/***** poor information for time determination *****/
	time_invalid = TIME_INVALID;
	well_evtime= HXD_TIME_INVALID; 
	mti=0;
	hxd_packet_update_flag |= HXD_UPDATE_WEL;
	BnkPut("HXD:ALL:UPDATE",  sizeof(int), &hxd_packet_update_flag);
	BnkPut("HXDfwelTime:EV_TIME_TLM", sizeof(double), &well_evtime);
	BnkPut("HXD:WEL:EV_TIME", sizeof(double), &well_evtime);
	BnkPut("HXD:WEL:MTI",     sizeof(int), &mti);
	goto END_PROCESS; 
    } else {
	time_invalid = TIME_VALID;
    }

    /* Event data access */
    BnkfGetM("HXD:WEL:EVENT", sizeof(HxdEventFits02), &size, &event_data);

    BnkGet( "HXD:WEL:UNITID" ,sizeof(int), &size, &unit);
    timstat->board = (unit>>2);

    BnkGet( "HXDfwelTime:HXD_WPU_CLK_RATE", sizeof(int),&size,&WPU_clock_rate);
    timstat->WPU_clock_rate = WPU_clock_rate;
    if (hxdfweltime_use_pwh == TRUE) {
      BnkGet("HXDfwelTime:PWH", sizeof(int)*HXD_PWH_DATANUM, &size, &pwh);
      BnkGet( "HXD:WEL:PACKET_SEC_HEADER", sizeof(int), &size, &ti);
      mti = hxdTime_mti_gen( pwh, ti, timstat->board);
      telm_time = mti;
    } else if (hxdfweltime_use_pwh == FALSE) {
      BnkGet( "HXD:WEL:PACKET_S_TIME",  sizeof(double), &size, &tlm_aetime);
      BnkGet( "HXD:WEL:PACKET_SEC_HEADER", sizeof(int), &size, &ti);
      mti = ti;
      telm_time = ti;
    }
    tlm_time = hxdTime_ti_to_HxdTime(&telm_time, timstat);    

    BnkGet( "HXD:WEL:WELTIME",sizeof(int), &size, &WPU_time_counter);

    /* ================ well event time determination ================= */
    hxdTime_wel_time_determination(WPU_time_counter, timstat,
				   ACU_zero_time, tlm_time, &well_event_time);
    
    well_evtime   = hxdTime_HxdTime2DoubleTime(well_event_time);
    BnkPut("HXDfwelTime:EV_TIME_TLM", sizeof(double), &well_evtime);

    switch ( HxdTime2aetime_mode ) {
    case TI_AETIME_MODE_I:
        well_evtime = hxdTime_HxdTime2aetime_I (well_event_time);
        break;
    case TI_AETIME_MODE_II_I:
        well_evtime = hxdTime_HxdTime2aetime_II_i (well_event_time,
                                                   tlm_time, tlm_aetime);
        break;
    case TI_AETIME_MODE_II_II:
	well_evtime = hxdTime_HxdTime2aetime_II_ii(well_event_time,
						   tlm_time, tlm_aetime);
        break;
    case TI_AETIME_MODE_III:
	well_evtime = hxdTime_HxdTime2aetime_III(well_event_time, tlm_aetime);
        break;
    default:
        fprintf(stderr,"HXDwelTime: TI to aetime, no mode selected!\n");
        well_evtime = hxdTime_HxdTime2aetime_II_i (well_event_time,
                                                   tlm_time, tlm_aetime);
        break;
    }

    /* ============ output files or hbook ( or fits ) ================= */    
    BnkPut("HXD:WEL:EV_TIME", sizeof(double), &well_evtime);
    BnkPut("HXD:WEL:MTI", sizeof(int), &mti);

    /* update event data register */
    hxdTimeUtil_modify_eventdata( well_evtime, mti, &event_data );  
    BnkfPutM("HXD:WEL:EVENT", sizeof(HxdEventFits02), &event_data);

    /* =================== record counter (for time resolution) ====== */
    board = timstat->board;
    if (latch_AE_time>0){
      hxdTime_Resolution_record(&well_event_time, &well_evtime, board);
      /* printf("time latch = 0x%X CLK_RATE=%d\n", latch_AE_time, 
	        timstat->WPU_clock_rate); */
    } else {
      /* printf("%X%d, ", latch_AE_time, 	     timstat->WPU_clock_rate); */
    }

    /* =================== prepare the next ======================== */
    timstat->before.ti[board]       = telm_time;
    timstat->before.wel_time[board] = WPU_time_counter;
    timstat->before.board           = board;
    if ( timstat->begin.wel[board] == FIRST ) timstat->begin.wel[board] = NOT_FIRST;

 END_PROCESS:
    BnkPut("HXDfwelTime:TIME_INVALID", sizeof(int), &time_invalid);
    *status = ANL_OK;
    return;
}

void
HXDfwelTime_endrun(int *status){ 
  double timedel;
  double timedel_wpu[4];
  double timedel_sum_us = 0.0;
  int board;
  int alive_board_num = 0;

  for (board=0; board<4; board++) {
    timedel_wpu[board] 
      = hxdTime_Resolution_calc_TIMEDEL(timstat->time_shift, board);
    if (timedel_wpu[board] > 0.0){
      timedel_sum_us += timedel_wpu[board]*1000000;
      alive_board_num ++;
    }
  }
  timedel = timedel_sum_us / (double) alive_board_num / 1000000;
  /*
  fprintf(stdout, "average timedel = %f(us)/%d = %f(us)\n", 
	  timedel_sum_us, alive_board_num, timedel*1000000);
  */
  BnkPut("HXDfwelTime:TIME_RESOLUTION", sizeof(double), &timedel);

  *status = ANL_OK; 
}

void
HXDfwelTime_exit(int *status){ *status = ANL_OK; }


/*********************  Static Functions *************************************/
static void
Calc_ACU_zero_time(unsigned int *latch_AE_time,
		   unsigned int *latch_DE_time,
		   HxdTime *ACU_zero_time       ){
    HxdTime ACU_zero_time_dummy;   /* 32 bit counter : upper 15 bit (sec)*/
    
    if (*latch_AE_time == timstat->before.latch_AE_time &&
	*latch_DE_time == timstat->before.latch_DE_time &&
	timstat->begin.hk == NOT_FIRST ){
	; /* do nothing: no change ACU_zero_time */
    } else {
	/***  calc ACU counter Zero_Reset Time ***/
	ACU_zero_time_dummy.lower =
	    ( (*latch_DE_time << HXD_TIME_COUNTER_Sec_SHIFT) & 0x7fffffff )
		- ( (*latch_AE_time << HXD_TIME_COUNTER_030_SHIFT) & 0x7fffffff );
	ACU_zero_time_dummy.upper =
	    *latch_DE_time>>(HXD_TIME_COUNTER_Up5_SHIFT - HXD_TIME_COUNTER_Sec_SHIFT);
	/*** ACU zero_time correction ************/
	*ACU_zero_time = hxdTime_ACU_zero_time_correct( &ACU_zero_time_dummy );
	/* fprintf(stderr,"HXDfwelTime: LATCH DE:%x AE:%x\n",(*latch_DE_time),(*latch_AE_time)); */
	timstat->before.latch_AE_time = *latch_AE_time;
	timstat->before.latch_DE_time = *latch_DE_time;
    }
    
}
/* ======================= End of HXDfwelTime.c =========================== */
