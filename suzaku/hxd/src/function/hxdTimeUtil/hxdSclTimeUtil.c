/* ------------------------------------------------------------------------
 *   scl_time_determination
 *                         Time Determination function for Scl data.
 * ------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "atFunctions.h"
#include "aste_time.h"
#include "hxd/HXD.h"
#include "hxdTime.h"
#include "hxdTimeUtil.h"

static void
debug_sclt_dump(unsigned int *WPU_time_counter, 
	   HxdTimeStatus *tstatus,
	   HxdTime *ACU_zero_time, HxdTime *tlm_time, HxdTime *well_time);

void
hxdTime_scl_time_determination(unsigned int WPU_time_counter, 
			       HxdTimeStatus *tstatus,
			       HxdTime ACU_zero_time, HxdTime tlm_time,
			       HxdTime *well_time){
    static unsigned int WPU_clock_number[WPU_NBOARD];
    static unsigned int WPU_clock_number_TLM; /* number of WPU_Counter_Reset */
    static unsigned int WPU_time_counter_TLM;
    unsigned int WPU_clock_full_time; /* WPU_Counter_Reset to Rest */
    /* expected WPU_time_counter at tlm_time */
    unsigned int WPU_zero_time; /* WPU_counter zero_time of wel_event_time */
    unsigned int WPU_shift;     /* counter shift */
    int shift, negative_flag=0;
    unsigned int mask;
    HxdTime dummy;
    int board;
    HxdTime before_ti;
    unsigned int beforeti;

    /* ======================= change setting ============================= */
    /* -OLD--- Clock Rate ("0"=30.5us / "1"=15.26 us / "2 or 3"=7.63 us)--- */
    /* -NEW--- Clock Rate ("0"=61.0us / "1"=30.1  us / "2 or 3"=15.3 us)--- */
    switch ( tstatus->WPU_clock_rate ){
    case HXD_WPU_CLOCK_RATE_NORMAL:
	WPU_clock_full_time = HXD_SCL_CLK_NORMAL_FULL_TIME_COUNT;
	WPU_shift = HXD_TIME_COUNTER_061_SHIFT; /* 61.0 us counter */
	shift = HXD_SCL_TIME_FULL_BIT_NUMBER + HXD_TIME_COUNTER_061_SHIFT;
	mask = HXD_SCL_CLK_NORMAL_MASK;
	break;
    case HXD_WPU_CLOCK_RATE_FINE:
    case HXD_WPU_CLOCK_RATE_NORMAL_OLD:
	WPU_clock_full_time = HXD_SCL_CLK_FINE_FULL_TIME_COUNT;
	WPU_shift = HXD_TIME_COUNTER_030_SHIFT; /* 30.5us counter */
	shift = HXD_SCL_TIME_FULL_BIT_NUMBER + HXD_TIME_COUNTER_030_SHIFT;
	mask = HXD_SCL_CLK_FINE_MASK;
	break;
    case HXD_WPU_CLOCK_RATE_SUPER_FINE:
    case HXD_WPU_CLOCK_RATE_SUPER_FINE2:
    case HXD_WPU_CLOCK_RATE_FINE_OLD:
	WPU_clock_full_time = HXD_SCL_CLK_S_FINE_FULL_TIME_COUNT;
	WPU_shift = HXD_TIME_COUNTER_015_SHIFT; /* 15.7us counter */
	shift = HXD_SCL_TIME_FULL_BIT_NUMBER + HXD_TIME_COUNTER_015_SHIFT;
	mask = HXD_SCL_CLK_S_FINE_MASK;
	break;
    case HXD_WPU_CLOCK_RATE_SUPER_FINE_OLD:
    case HXD_WPU_CLOCK_RATE_SUPER_FINE2_OLD:
fprintf(stderr,"====================================================\n");
fprintf(stderr,"Time determination of scaler time counter is invalid for old superfine time mode\n");
fprintf(stderr,"\n");
	WPU_clock_full_time = HXD_SCL_CLK_SS_FINE_FULL_TIME_COUNT;
	WPU_shift = HXD_TIME_COUNTER_015_SHIFT; /* 15.7us counter */
	shift = HXD_SCL_TIME_FULL_BIT_NUMBER + HXD_TIME_COUNTER_015_SHIFT;
	mask = HXD_SCL_CLK_SS_FINE_MASK;
	break;
    default:
	WPU_clock_full_time = HXD_SCL_CLK_NORMAL_FULL_TIME_COUNT;
	WPU_shift = HXD_TIME_COUNTER_030_SHIFT; /* default = fine */
	shift = HXD_SCL_TIME_FULL_BIT_NUMBER + HXD_TIME_COUNTER_030_SHIFT;
	mask = HXD_SCL_CLK_FINE_MASK;
	break;
    }

    tstatus->time_shift = WPU_shift;

    /* ====================== calc WPU_zero_time ======================== */
    board = tstatus->board;
    beforeti = tstatus->before.ti[board];

    before_ti = hxdTime_ti_to_HxdTime(&beforeti, tstatus);

    if (    tstatus->before.board      != board
/*	||  ! HxdTime_compare( tstatus->before.tlm_time[board], tlm_time ) */
            ||  before_ti.lower != tlm_time.lower
	    ||  tstatus->begin.wel[board] == FIRST                        ){
	
	/****** dummy ******/
	dummy = hxdTime_Sub_HxdTime(&tlm_time, &ACU_zero_time);
	
	/**** TLM counter *****/
	WPU_clock_number_TLM  = dummy.upper <<(HXD_TIME_COUNTER_Up5_SHIFT-shift);
	WPU_clock_number_TLM += dummy.lower >> shift;
	WPU_time_counter_TLM  = ( (dummy.lower&mask) >> WPU_shift ) & 0xffffff;
    }

    
    if ( WPU_time_counter_TLM < WPU_time_counter ){
        if ( WPU_clock_number_TLM == 0 ) {
	  WPU_clock_number[board] = WPU_clock_number_TLM;
	  negative_flag = 1;
	} else {
	  WPU_clock_number[board] = WPU_clock_number_TLM - 1;	
	}
    } else {
	WPU_clock_number[board] = WPU_clock_number_TLM;
    }
    if(TIME_DEBUG) {
      if(board==1)fprintf(stderr,"%x %x %x %x %x\n",WPU_clock_number[board],WPU_clock_number_TLM,WPU_time_counter_TLM,WPU_time_counter,tlm_time.lower);
    }
    /* ================ calc well_time ================================= */
    /****** HXD-AE ACU zero time ********/
    if ( negative_flag ) {
      dummy.upper = 0;
      dummy.lower = WPU_clock_full_time;
      *well_time = hxdTime_Sub_HxdTime(&ACU_zero_time, &dummy);
    } else {
      well_time->lower  =  ACU_zero_time.lower;
      well_time->upper  =  ACU_zero_time.upper;
    }

    /****** + HXD-AE WPU zero time ********/
    well_time->lower += (WPU_clock_full_time*WPU_clock_number[board])&0x7fffffff;
    well_time->upper += ((WPU_clock_full_time>>22)*(WPU_clock_number[board])) >>9;
    
    /****** + HXD-AE WPU Well time ********/
    well_time->lower += (WPU_time_counter << WPU_shift )&0x7fffffff;
    
    /***** HxdTime format correction ******/
    well_time->upper += ( well_time->lower)>>31;
    well_time->lower -= ((well_time->lower)>>31)*0x80000000;

    if (TIME_DEBUG) debug_sclt_dump(&WPU_time_counter, tstatus, &ACU_zero_time,
			       &tlm_time, well_time);

}

/* ======================= EOF hxdWelTimeUtil.c ============================= */

static void
debug_sclt_dump(unsigned int *WPU_time_counter, 
	   HxdTimeStatus *tstatus,
	   HxdTime *ACU_zero_time, HxdTime *tlm_time, HxdTime *well_time){
    
    /**** for DEBUG ****/
    fprintf(stderr,"WEL[%d]:0x%05X ", tstatus->board, *WPU_time_counter);
    fprintf(stderr,"AE0:%02X.%08X " , ACU_zero_time->upper, ACU_zero_time->lower);
    fprintf(stderr,"TLM:%02X.%08X " , tlm_time->upper, tlm_time->lower);
    fprintf(stderr,"STAT:%d "       , tstatus->WPU_clock_rate);
    fprintf(stderr,"-->wel:%02X.%08X ", well_time->upper, well_time->lower);
    
}











