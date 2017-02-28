/****************************************************
 *  hxdTimeUtil.c
 *  version 1.0 = HXDwelTime(ANL) version 5.2
 *                1999/07/26  Y.Terada
 *  version 0.3   1999/08/03 by Y.Terada
 *  version 0.6   1999/08/12 by Y.Terada
 *                 HxdTime2astime()
 *  version 0.6   2003/05/03 by Y.Terada
 *                 HXD-II format: add hxdTime_mti_gen()
 *  version 0.7 2003/05/03 by Y.Terada
 *                 HXD-II format: add hxdTime_mti_gen()
 *                 Change BST Time format.
 *  version 0.8.0 2004/03/13 by Y.Terada
 *                 support atFunction v2.2 (AtTimeD)
 *  version 0.8.1 2005/02/23 by Y.Terada
 *                 support ti2aetime (astetool v1.31)
 *  version 0.8.2 2005/05/28 by R.Miyawaki
 *                 Read tim_file (aste_ti2time v2.7)
 *  version 0.8.3 2005/06/03 by Y.Terada
 *                 get time resolution, TIMEDEL
 *  version 0.8.4 2005/06/08 by Y.Terada
 *                 support long time exposure, in calc time resolution.
 *  version 0.8.5 2005/06/08 by R.Miyawaki
 *                 add before_ti for WPU_clock_number BUG
 *  version 0.8.6 2005/06/13 by Y.Terada
 *                 debug before_ti / delete needless printf.
 *  version 0.8.7 2005/08/26 by R.Miyawaki
 *                 debug calc_Timedel
 *  version 0.8.9 2005/09/17 by Y.Terada
 *                 debug hxdTime_Sum_HxdTime.
 *  version 0.9.0 2005/09/26 by Y.Terada
 *                 debug hxdTime_AddInt_HxdTime,
 *                 debyg hxdTrnTimeUtil
 *  version 0.9.1 2005/09/27 by Y.Terada
 *                 debyg hxdTrnTimeUtil, new algorithm
 *  version 0.9.2 2005/10/14 by R.Miyawaki
 *                 hxdBstTimeUtil, set ACU_zero_time = reset_time
 *                 hxdBstTimeUtil, debug frz_time.upper
 *                 hxdTimeUtil, move hxdTime_Sum_HxdTime to the old version
 *  version 0.9.3 2005/10/15 by R.Miyawaki
 *                 hxdBstTimeUtil, remove ACU_zero_time = reset_time
 *  version 0.9.5 2006/02/24, by Y.Terada, thanks to Y.Ishisaki,
 *                 check TIMEDEL value
 *  version 2.0.0 for pipe line process ver2.x (same as ver 0.9.5)
 *  version 2.0.1 2008/10/23 by Y.Terada and M.Ozaki
 *  version 2.0.2 2013/10/15 by Y.Terada
 ****************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "atFunctions.h"
#include "aste_time.h"
#include "hxd/HXD.h"
#include "hxdTime.h"
#include "hxdTimeUtil.h"
#include "aste_ti2time.h"

static char *
hxdTime_scan_yymmdd(char *p, AtTimeD *ji);

static double
ASTE_ti2aetime(unsigned int ti);

static double
hxdTime_Get_TI_base_tick(HxdTime *current_time,     double *current_aetime);

static TI2TIME *ti2time;
static int initial_done = 0;

#define HXDTIME_RESOLUTION_USE_RECORD 999
static struct {
  int use_record;
  HxdTime first_hxdtime[4];
  double  first_aetime [4];
  HxdTime current_hxdtime[4];
  double  current_aetime [4];
  int recorded[4];
} hxdTime_resolution;


/**************************************************************************
 *********************** (0) HXD TIME INIT ********************************
 **************************************************************************/
int hxdTime_init( char *tim_file ){
  int status = HXDTIMEUTIL_OK;
  int stat;

/*  char time_pkt_file[16] = "none";
  char dp_timc_file[16]  = "none";
  
  stat = aste_ti2time_init(&ti2time, time_pkt_file, dp_timc_file); */

  stat = aste_ti2time_init(&ti2time, tim_file);

  if (stat != 0){
    fprintf(stderr, "hxdTimeUtil: Err in aste_ti2time_init (%d)n", stat);
    status = HXDTIMEUTIL_NG;
    return status;
  }
  initial_done = 1;
  return status;
}

/**************************************************************************
 *********************** (I) TIME DETERMINATIN UTILITIES ******************
 **************************************************************************/
/* ------------------------------------------------------------------------
 *   (I-1) HxdTime +- HxdTime, HxdTime +- Unsigned Int
 * ------------------------------------------------------------------------*/
HxdTime
hxdTime_Sum_HxdTime(HxdTime *time0, HxdTime *time1){
    HxdTime add_time;
    
    add_time.lower  = ( time0->lower + time1->lower )&0x7fffffff;
    add_time.upper  = ( time0->lower + time1->lower )>>HXD_TIME_COUNTER_Up5_SHIFT;
    /*    add_time.upper  = (time0->lower >>HXD_TIME_COUNTER_Up5_SHIFT);
	  add_time.upper += (time1->lower >>HXD_TIME_COUNTER_Up5_SHIFT); */
    add_time.upper += ( time0->upper + time1->upper );

    return add_time;
}

HxdTime
hxdTime_Sub_HxdTime(HxdTime *time0, HxdTime *time1){
    HxdTime sub_time;
    unsigned int dummy_lower;

    dummy_lower = time0->lower - time1->lower;
    sub_time.lower = dummy_lower & 0x7FFFFFFF ;
    
    if ( dummy_lower & 0x80000000 ) {
	sub_time.upper = time0->upper - time1->upper - 1;
    } else {
	sub_time.upper = time0->upper - time1->upper;
    }
    
    return sub_time;
}

HxdTime
hxdTime_AddInt_HxdTime(HxdTime *time0, unsigned int *number){
    HxdTime add_time;
    /* (caution!) unsigned int number: HxdTime lower format */

    add_time.lower  = ( time0->lower + *number )&0x7fffffff;
    /*
    add_time.upper  = ( time0->lower + *number )>>HXD_TIME_COUNTER_Up5_SHIFT;
    */
    add_time.upper  = ( time0->lower)>>HXD_TIME_COUNTER_Up5_SHIFT;
    add_time.upper += ( *number )    >>HXD_TIME_COUNTER_Up5_SHIFT;
    add_time.upper += time0->upper;

    return add_time;

}

HxdTime
hxdTime_SubInt_HxdTime(HxdTime *time0, unsigned int *number){
    HxdTime sub_time;
    unsigned int dummy_lower;
    /* (caution!) unsigned int number: HxdTime lower format */
    
    dummy_lower  = time0->lower - *number ;
    sub_time.lower = dummy_lower & 0x7FFFFFFF ;
    
   if ( dummy_lower & 0x80000000 ) {
	sub_time.upper = time0->upper - 1;
    } else {
	sub_time.upper = time0->upper;
    }

    return sub_time;

}

/* ------------------------------------------------------------------------
 *   (I-2) HxdTime == HxdTime ?
 * ------------------------------------------------------------------------*/
int
hxdTime_HxdTime_compare(HxdTime time0, HxdTime time1 ){
#define SAME   1
#define DIFFER 0
    
    if(  (time0.upper == time1.upper)
       &&(time0.lower == time1.lower) ) {
	return SAME;
    } else {
	return DIFFER;
    }
}

/* ------------------------------------------------------------------------
 *   (I-3) HxdTime <---> TI
 * ------------------------------------------------------------------------*/
void
hxdTime_Extract_Ti_from_HxdTime( HxdTime time,
				unsigned int *ti, int *under_ti ){
    
    *ti  = time.upper<<(HXD_TIME_COUNTER_Up5_SHIFT-HXD_TIME_COUNTER_244_SHIFT);
    *ti += time.lower>> HXD_TIME_COUNTER_244_SHIFT;
    *under_ti = time.lower & 0xF;

}

HxdTime
hxdTime_ti_to_HxdTime(unsigned int *ti, HxdTimeStatus *stat){
    HxdTime time;
    static unsigned int ti_inc[4] = {0,0,0,0};
    int board;

    board = stat->board;
    if ( *ti < stat->before.ti[board] ) ti_inc[board]++;
    
    time.lower = (*ti << HXD_TIME_COUNTER_244_SHIFT) & 0x7fffffff;
    time.upper =
	( *ti >> (HXD_TIME_COUNTER_Up5_SHIFT-HXD_TIME_COUNTER_244_SHIFT) )
	& 0x1F ;
    
    time.upper += (ti_inc[board] << 5); /* ETI bit */

    return time;
}

/* ------------------------------------------------------------------------
 *   scan_yymmdd, get_aetime
 * ------------------------------------------------------------------------*/
static char *
hxdTime_scan_yymmdd(char *p, AtTimeD *ji){
/*if ( 6 == sscanf(p, "%02u%02u%02u%02u%02u%02u",*/
  if ( 6 == sscanf(p, "%02d%02d%02d%02d%02d%02d",
		   &ji->yr, &ji->mo, &ji->dy, &ji->hr, &ji->mn, &ji->sc) ) {
    p += 12;
/*  if ( '.' == *p && 1 == sscanf(p+1, "%03f", &ji->ss) ) { */
    if ( '.' == *p && 1 == sscanf(p+1, "%03lf", &ji->ss) ) { 
      p += 4;
    } else {
      ji->ss = 0.0;
    }
  }
  return p;
}

double
hxdTime_get_aetime(char *yymmdd){
  AtTimeD ji;

  hxdTime_scan_yymmdd(yymmdd,&ji);
  return attimeD2aste(&ji);
}

/**************************************************************************
 *********************** (II) HXD TIME DETERMINATIN ***********************
 **************************************************************************/
/* ------------------------------------------------------------------------
 *   ACU_zero_time_correct
 *                         time correction for AE-ACU time zero.
 * ------------------------------------------------------------------------*/
HxdTime
hxdTime_ACU_zero_time_correct(HxdTime *time){
    HxdTime Zero_time_correction       = {0,0};
    HxdTime Integrated_time_correction = {0,0};
    HxdTime corrected_time;
    
    /* ==== TIME_LATCH_TIME Reset time correction ( 122 us )==== */
    Zero_time_correction.lower  = ( 0x1 << (HXD_TIME_COUNTER_122_SHIFT) );
    corrected_time = hxdTime_Sum_HxdTime (time, &Zero_time_correction);
    
    /* ============= Integrated error correction =============== */
    Integrated_time_correction.lower = 0x0;     /* not supported */
    corrected_time = hxdTime_Sum_HxdTime (&corrected_time,
					  &Integrated_time_correction);
    return corrected_time;
}

/**************************************************************************
 **************** (III) HXD TIME DETERMINATIN (TI to aetime) **************
 **************************************************************************/
/* ------------------------------------------------------------------------
 *   (III-1) HxdTime2DoubleTime
 *                         change value HxdTime to double.
 * ------------------------------------------------------------------------*/
double
hxdTime_HxdTime2DoubleTime( HxdTime int_time ){
    double double_time;
    double time_sec;
    double time_us;

    time_sec  = (double)((int_time.lower >> 16 ) & 0x7fff );
    time_sec += (double)( int_time.upper * 32768.0);
    time_us   = (double)((int_time.lower & 0x0000ffff) * (1.0/65536.0) );

    double_time = time_sec + time_us + HXD_ZERO_CORRECTION_TIME;
    
    return double_time; 
}

/* ========================================================================
 *   (III-2) HxdTime2ASTETime
 *                         change value HxdTime to aetime.
 *  (i)    using ASTE FUNCTION (TI to aetime, with time table.)
 *  (ii-1) use telemetry aetime, and simply add sub_aetime.
 *  (ii-2) use telemetry aetime, and interpolate event aetime.
 * ======================================================================== */
double
hxdTime_HxdTime2aetime_I(HxdTime hxdtime){
    double aetime;
    unsigned int ti;       /* TI (S.H.) format */
    int under_ti;          /* fine time bit    */

    hxdTime_Extract_Ti_from_HxdTime( hxdtime, &ti, &under_ti );

    aetime  = (double)(under_ti * ASTE_DP_BASE_TICK );
    aetime += ASTE_ti2aetime(ti); /** HXD temporal version **/
    aetime += HXD_ZERO_CORRECTION_TIME;
    return aetime;
}

double
hxdTime_HxdTime2aetime_II_i(HxdTime hxdtime,
			    HxdTime tlmtime, double tlm_aetime){
    double aetime;
    HxdTime process_time;
    double base_tick = ASTE_DP_BASE_TICK; /* nominal value */
    
    process_time = hxdTime_Sub_HxdTime(&tlmtime, &hxdtime);
    if(process_time.upper) {
	if (TIME_DEBUG)
	    fprintf(stderr,
            "hxdTime_HxdTime2aetime: tlmtime to eventtime is too Long!\n");
	if (TIME_DEBUG)
	    fprintf(stderr,"tlmtime:%x %x  eventtime:%x %x @astetime=%f\n",
		tlmtime.upper,tlmtime.lower,hxdtime.upper,hxdtime.lower,
		tlm_aetime);
/*	exit(1);*/
    }
    
    aetime  = - (process_time.lower * base_tick);
    aetime += tlm_aetime;
    aetime += HXD_ZERO_CORRECTION_TIME;

/*  fprintf(stderr, "TLM=%12.8f, EVT=%12.8f\n", tlm_aetime, aetime);*/
    return aetime;
}

double
hxdTime_HxdTime2aetime_II_ii(HxdTime hxdtime,
			     HxdTime tlmtime, double tlm_aetime){
    double aetime;
    HxdTime process_time;
    double base_tick ;     /* running avarage */
    
    process_time = hxdTime_Sub_HxdTime(&tlmtime, &hxdtime);
    if(process_time.upper)
	if (TIME_DEBUG)
	    fprintf(stderr,
	    "hxdTime_HxdTime2aetime2-2: tlmtime to eventtime is too Long!\n");
    
    base_tick = hxdTime_Get_TI_base_tick(&tlmtime, &tlm_aetime);
    
    aetime  = - (process_time.lower * base_tick);
    aetime += tlm_aetime;
    aetime += HXD_ZERO_CORRECTION_TIME;
    
    return aetime;
}


double hxdTime_HxdTime2aetime_III  (HxdTime hxdtime, double tlm_aetime){
  double aetime;
  double aetime_at_ti;
  unsigned int ti;       /* TI (S.H.) format */
  int under_ti;          /* fine time bit    */
  int stat;

  if (!initial_done){
    fprintf(stderr, "hxdTimeUtil: Please Initialize aste_ti2time\n");
    aetime = 0.00;
    return aetime;
  }

  hxdTime_Extract_Ti_from_HxdTime( hxdtime, &ti, &under_ti );

  aetime  = (double)(under_ti * ASTE_DP_BASE_TICK );

  stat = aste_ti2time_dp(ti2time, ti, tlm_aetime, &aetime_at_ti);
  if(stat == 0){ /** success **/
    aetime += aetime_at_ti;
  } else {
    aetime = HXD_TIME_INVALID_AETIME;
  }

  return aetime;
}

/* ------------------------------------------------------------------------
 *   (III-1) GET ASTRO-E TIME (temporal version)
 * ------------------------------------------------------------------------*/
static double
ASTE_ti2aetime(unsigned int ti){
    double time_sec, time_us;
    double aetime;
    
    /* Only change time format: int to double */
    time_us  = (double)((ti&0x00000FFF) * (1.0/4096.0));
    time_sec = (double)((ti&0xFFFFF000) >> 12);

    aetime = time_us + time_sec;

    return aetime;
}

/* ------------------------------------------------------------------------
 *   (III-1) get base tick of DP counter.
 * ------------------------------------------------------------------------*/
static double
hxdTime_Get_TI_base_tick(HxdTime *current_time,  double *current_aetime){
#define TIMEBUF_SIZE 64
#define TIMEBUF_MASK 0x3F
    
    /*** FIFO: Ring Buffer ***/
    static struct {
	HxdTime time;
	double aetime;
    } TimeBuffer[TIMEBUF_SIZE];
    static int buffer_init = 0;          /* flag for initialization */
    static int timebuf_current_pos = 0 ; /* pointer */
    int timebuf_last_pos;                /* pointer */

    static HxdTime pre_time ={0,0};
    
    double  base_tick ;
    HxdTime last_time  ;
    double  last_aetime;
    HxdTime sub_time   ;
    double  sub_aetime ;

    /** =========(1)== Write to FIFO ============== **/
    if( ! hxdTime_HxdTime_compare(pre_time, *current_time) ){
	/**** forward the write pointer ****/
	timebuf_current_pos = (timebuf_current_pos+1) & TIMEBUF_MASK;
	
	/**** Input current value to FIFO ****/
	TimeBuffer[timebuf_current_pos].time   = *current_time   ;
	TimeBuffer[timebuf_current_pos].aetime = *current_aetime;
	
	if(buffer_init < TIMEBUF_SIZE)  buffer_init ++;
    } 	
    pre_time = *current_time; /* prepare the next */

    
    /** =========(2)== Calc Base Tick  ============== **/
    if(buffer_init != TIMEBUF_SIZE) {
	/**** FIFO init ****/
	base_tick = ASTE_DP_BASE_TICK; /* nominal value */
    } else {
	/*** read from FIFO ***/
	timebuf_last_pos    = (timebuf_current_pos-TIMEBUF_SIZE+1)&TIMEBUF_MASK;
	last_time   = TimeBuffer[timebuf_last_pos].time  ;
	last_aetime = TimeBuffer[timebuf_last_pos].aetime;
	
	/*** calc ***/
	sub_time   = hxdTime_Sub_HxdTime(current_time, &last_time);    
	sub_aetime = *current_aetime - last_aetime;
	
	if( sub_time.lower != 0 ){
	    base_tick = (sub_aetime * 16.0) / (double) (sub_time.lower << 4);
	} else {
	    base_tick = ASTE_DP_BASE_TICK; /* nominal value */
	}
    }
    
    return base_tick;
}

#define HXD_N_WPU 4
#define HXDTIME_MTI_FIRST 1
#define HXDTIME_MTI_NOT_FIRST 0
#define HXDTIME_PWH_SEC_MAX 16
#define TI_ONE_SEC  0x1000
int
hxdTime_mti_gen( int *pwh, int ti, int board ){
  static int event_num[HXD_N_WPU]= {0,0,0,0};
  static int prev_ti[HXD_N_WPU] = {0,0,0,0};
  static int first_flg[HXD_N_WPU] = {HXDTIME_MTI_FIRST,HXDTIME_MTI_FIRST,
				     HXDTIME_MTI_FIRST,HXDTIME_MTI_FIRST};
  int mti;
  int seconds;
  int counts;

  if (first_flg[board] == HXDTIME_MTI_FIRST){
    /** === First Call === **/
    mti = ti;
    first_flg[board] = HXDTIME_MTI_NOT_FIRST;

    return mti;
  } else {

    /**** search PWH ****/
    counts = event_num[board];
    for(seconds=0; seconds < HXDTIME_PWH_SEC_MAX; seconds ++){
      if (counts > pwh[seconds]){
	counts -= pwh[seconds];
      } else {
	break;
      }
    }
    
    /*** change packet ***/
    if (ti != prev_ti[board]) {
      event_num[board] = 0;
/*    printf("W%d: -------------\n", board); */
    }
    /*** create mti ***/

    mti = ti + TI_ONE_SEC * seconds;
/*  printf("W%d: TI=0x%x MTI=0x%x\n", board, ti, mti); */
    
  }

  event_num[board] ++;
  prev_ti  [board] = ti;

  return mti;  

}

/* ------------------------------------------------------------------------
 *   (III-4) others,
 *                         record and measure the time resolution.
 * ------------------------------------------------------------------------*/

void hxdTime_Resolution_record(HxdTime *hxdtime, double *aetime, int board){
  static int first[4] = {1,1,1,1};

  if (first[board]) {
    hxdTime_resolution.use_record = HXDTIME_RESOLUTION_USE_RECORD;
    hxdTime_resolution.first_hxdtime[board].upper = hxdtime->upper;
    hxdTime_resolution.first_hxdtime[board].lower = hxdtime->lower;
    hxdTime_resolution.first_aetime[board]        = *aetime;
    hxdTime_resolution.recorded[board]            = 0;
    first[board] = 0;
  } else {
    hxdTime_resolution.current_hxdtime[board].upper = hxdtime->upper;
    hxdTime_resolution.current_hxdtime[board].lower = hxdtime->lower;
    hxdTime_resolution.current_aetime[board]        = *aetime;
    hxdTime_resolution.recorded[board]              = 1;
  }

  return;

}

double hxdTime_Resolution_calc_TIMEDEL( int time_shift, int board ){
  double resolution;
  double n_clocks_par_sec;
  HxdTime diff_hxdTime;
  double  diff_aetime;
  int n_clocks_lower;
  int n_clocks_upper;

  if (hxdTime_resolution.use_record != HXDTIME_RESOLUTION_USE_RECORD) {
    resolution = 1.0 / pow(2, (16-time_shift) ); 
                 /* base tick in hxdTime is 1/65536 = 1/2^16 (sec) */
    /* fprintf(stdout, "not recorded?\n"); */
    return resolution;
  }
  if (! hxdTime_resolution.recorded[board]){
    resolution = 0.0;
    return resolution;
  }

  diff_hxdTime 
    = hxdTime_Sub_HxdTime( &hxdTime_resolution.current_hxdtime[board],
			   &hxdTime_resolution.first_hxdtime[board]);

  diff_aetime = hxdTime_resolution.current_aetime[board] 
                - hxdTime_resolution.first_aetime[board];

  n_clocks_lower  = (diff_hxdTime.lower >> time_shift  );
  n_clocks_lower += (diff_hxdTime.upper << (31-time_shift) ) & 0x7FFFFFFF;
  n_clocks_upper  = (diff_hxdTime.upper >> time_shift  );

  /*
  resolution = diff_aetime / ( (double) n_clocks_lower );
  */

  if (diff_aetime <= 0.000){
    resolution = 0.0; return resolution;
  } else {
    n_clocks_par_sec  = ( (double)n_clocks_lower ) / diff_aetime;
    n_clocks_par_sec += ( (double)n_clocks_upper )*2147483648.0 / diff_aetime;
    resolution = 1.0 / n_clocks_par_sec;
    return resolution;
  }

}

/* ======================= End of hxdTimeUtil.c =========================== */


