/* ========================================================================
 *   hxdTimeUtil.h
 *          version 0.1 1999.7.26 created by Y. Terada
 *               first version for hxd time determinatin (well only)
 *          version 0.2 1999.8.2  modefied by Y. Terada
 *               HxdTime reformat
 *          version 0.3 1999.8.3  modefied by Y. Terada
 *               share HxdTimeStatus for WEL, TRN, BST
 *          version 0.5 1999.8.12 modefied by Y. Terada
 *               support HxdTime2ASTETime. Three mode.
 *          version 0.6 1999.8.14 modefied by Y. Terada
 *               rename functions. hxdTime_***();
 *          version 0.64 1999.12.14 modefied by Y. Terada
 *               support time assignment for SCL Fits (or SCL Packet).
 *          version 0.7 2003/05/03 by Y.Terada
 *                 HXD-II format: add hxdTime_mti_gen()
 *                 Change BST Time format.
 *          version 0.8.0 2004/03/13 by Y.Terada
 *                 support atFunction v2.2 (AtTimeD)
 *          version 0.8.1 2005/02/23 by Y.Terada
 *                 support ti2aetime (astetool v1.31)
 *          version 0.8.2 2005/05/28 by R.Miyawaki
 *                 Read tim_file (aste_ti2time v2.7)
 *          version 0.8.3 2005/06/03 by Y.Terada
 *                 get time resolution, TIMEDEL
 *          version 0.8.4 2005/06/08 by Y.Terada
 *                 support long time exposure, in calc time resolution.
 *                 access in pointer type.
 *          version 0.8.5 2005/06/08 by R.Miyawaki
 *                 add before_ti for WPU_clock_number BUG
 *          version 0.8.6 2005/06/13 by Y.Terada
 *                 debug before_ti / delete needless printf.
 *          version 0.8.8 2005/09/10 by M.Kokubun
 *                  #include "hxdeventFitsUtil.h"
 *          version 0.8.9 2005/09/17 by Y.Terada
 *                 update hxdBstTimeUtil for HXD-II version.
 *          version 0.9.0 2005/09/26 by Y.Terada
 *                 debug hxdTime_AddInt_HxdTime,
 *                 debyg hxdTrnTimeUtil
 *          version 0.9.1 2005/09/27 by Y.Terada
 *                 debyg hxdTrnTimeUtil, new algorithm
 * ========================================================================*/
#ifndef _HXD_TIME_UTIL_H_
#define _HXD_TIME_UTIL_H_
/**************************************************************************
 *********************** (0) HXD TIME INIT ********************************
 **************************************************************************/
#define HXDTIMEUTIL_OK 1
#define HXDTIMEUTIL_NG 0
#include "hxdeventFitsUtil.h"
int hxdTime_init( char *tim_file );

/**************************************************************************
 *********************** (I) HXD TIME DETERMINATIN ************************
 **************************************************************************/
/*
 *   ----  hxd time format ----
 */
typedef struct {
    unsigned int upper;  /*  5-bit LSB :2^16   sec */
    unsigned int lower;  /* 31-bit LSB :1/2^16 sec */
} HxdTime;

/*
 *   ----  process status for time determination ----
 */
typedef struct {
    int WPU_clock_rate;
    int TPU_time_mode;
    int BST_extended; /* Time resolution (use/no_use TRN_TIME) */
    int time_shift;   /* updated in **_determination() */
    int board;
    struct {
	unsigned int latch_DE_time;/*20 bit counter : all bit(sec)*/
	unsigned int latch_AE_time;/*24 bit counter : upper 7bit(sec)*/
	unsigned int wel_time  [WPU_NBOARD]; /* 19-bit counter    */
	unsigned int trn_time  [TPU_NBOARD]; /* 19-bit counter    */
	unsigned int scl_time  [WPU_NBOARD]; /* 24-bit counter    */
	unsigned int ti        [WPU_NBOARD]; /* 32-bit counter TI */
	HxdTime      tlm_time  [WPU_NBOARD]; /* 63-bit counter ETI*/
	int board;
    } before;
    struct {
	int wel[WPU_NBOARD];
	int trn[TPU_NBOARD];
	int scl[WPU_NBOARD];
	int hk;
	int sys;
    } begin;
} HxdTimeStatus;

/*
 *    ---- HXD-AE time zero correction ----                 [hxdTimeUtil.c]
 */
HxdTime hxdTime_ACU_zero_time_correct(HxdTime *time);

/**************************************************************************
 ******************* (II) TIME DETERMINATIN (HXD to TI ) ******************
 **************************************************************************/
/*
 *    ==== HXD WEL event ====                            [hxdWelTimeUtil.c]
 */
void hxdTime_wel_time_determination(unsigned int WPU_time_counter,
				    HxdTimeStatus *tstatus,
				    HxdTime ACU_zero_time, HxdTime tlm_time,
				    HxdTime *well_time);
/*
 *    ==== HXD TRN data ====                             [hxdTrnTimeUtil.c]
 */
void hxdTime_trn_time_determination(unsigned int TPU_time_counter,
				    HxdTimeStatus *tstatus,
				    HxdTime ACU_zero_time, HxdTime tlm_time,
				    HxdTime *trn_time);
/* correction: AE-DE SEND TIMEING to PH DATA LATCH TIMING */
void hxdTime_trn_time_block_correction(int block_cnt, int block_num,
				       HxdTimeStatus *tstatus,
				       HxdTime *trn_time);
/*
 *    ==== HXD BST data ====                            [hxdBstTimeUtil.c]
 */
void hxdTime_bst_time_determination(unsigned int bst_freezed_time,
				    HxdTimeStatus *tstatus,
				    HxdTime ACU_zero_time, HxdTime tlm_time,
				    HxdTime *bst_time);

/* correction: FREEZED TIMEING to FIRST TH BIN TIMING */
void hxdTime_bst_time_block_correction(HxdTimeStatus *tstatus,
				       HxdTime *bst_time);

/* Add fine Time resolution using TRN_TIME at the FREEZED TIMING */
int hxdTime_bst_add_trntime(unsigned int bst_freezed_time,
			    unsigned int TRN_time_counter,
			    unsigned int *extended_bst_freezed_time);

/**************************************************************************
 ****************** (III) TIME DETERMINATIN (TI to aetime) ****************
 *********************************************** [hxdTimeUtil.c] **********
 **************************************************************************/
/*
 * hxdtime --> double time (hxdtime2aetime: Simple version)
 */  
double hxdTime_HxdTime2DoubleTime  (HxdTime int_time);

/* ========================================================================
 *   HxdTime2ASTETime
 *                         change value HxdTime to aetime.
 *  (1)   using ASTE FUNCTION (TI to aetime, with time table.)
 *  (2-1) use telemetry aetime, and simply add sub_aetime.
 *  (2-2) use telemetry aetime, and interpolate event aetime.
 *  (3)   use astetool version 1.31
 * ======================================================================== */
double hxdTime_HxdTime2aetime_I    (HxdTime hxdtime);
double hxdTime_HxdTime2aetime_II_i (HxdTime hxdtime,
				    HxdTime tlmtime,     double tlm_aetime);
double hxdTime_HxdTime2aetime_II_ii(HxdTime hxdtime,
				    HxdTime tlmtime,     double tlm_aetime);
double hxdTime_HxdTime2aetime_III  (HxdTime hxdtime,     double tlm_aetime);

int    hxdTime_mti_gen( int *pwh, int ti, int board );

void hxdTime_Resolution_record(HxdTime* hxdtime, double* aetime, int board_id);

double hxdTime_Resolution_calc_TIMEDEL( int time_shift, int board );


/**************************************************************************
 ****************** (IV) TOOLS for hxdTimeUtil ****************************
 *********************************************** [hxdTimeUtil.c] **********
 **************************************************************************/
/*
 *   ---- calculation tools ----
 */
/* SUM */
HxdTime hxdTime_Sum_HxdTime(HxdTime *time0, HxdTime *time1);

/* SUB */
HxdTime hxdTime_Sub_HxdTime(HxdTime *time0, HxdTime *time1);

/* COMPARE */
int hxdTime_HxdTime_compare(HxdTime time0, HxdTime time1 );

/* ADD INTEGER */
HxdTime hxdTime_AddInt_HxdTime(HxdTime *time0, unsigned int *number);

/* SUB INTEGER */
HxdTime hxdTime_SubInt_HxdTime(HxdTime *time0, unsigned int *number);

/* EXTRACT INTEGER */
void hxdTime_Extract_Ti_from_HxdTime(HxdTime time,
				     unsigned int *ti, int *under_ti);
/* change format TLM-TI to HxdTime */
HxdTime hxdTime_ti_to_HxdTime(unsigned int *ti, HxdTimeStatus *stat);

/*
 *   ---- aetime tools ----
 */
double hxdTime_get_aetime(char *yymmdd);

#endif
