/* ------------------------------------------------------------------------
 *   trn_time_determination
 *                        Time Determination function for Transient data.
 *                        version 0.2 created by Y.Terada 1999.7.27
 *                        version 0.9 reviced by Y.Terada 2005.9.26
 *                        version 2.0.2 reviced by Y.Terada 2013.10.15
 * ------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "atFunctions.h"
#include "aste_time.h"
#include "hxd/HXD.h"
#include "hxdTime.h"
#include "hxdTimeUtil.h"
#define DEBUG 0

void
hxdTime_trn_time_determination(unsigned int TPU_time_counter,
		       HxdTimeStatus *tstatus,
		       HxdTime ACU_zero_time, HxdTime tlm_time,
		       HxdTime *trn_time){
    unsigned int int_tran_time; 
 /* unsigned int int_tlm_time, int_acu_zero_time;*/
    HxdTime trn_time_tmp, tlm_time2, tlm_time3;
    unsigned int trn_lower_25bit; /** lower 25bit in nominal**/
    unsigned int tlm_lower_25bit; /** lower 25bit in nominal**/
    HxdTime trn_carry;

    switch ( tstatus->TPU_time_mode ){
    case HXD_TPU_TIME_MODE0:
      /*
	tstatus->time_shift = HXD_TIME_COUNTER_015_SHIFT;
      */
	tstatus->time_shift = HXD_TIME_COUNTER_030_SHIFT;
	break;
    case HXD_TPU_TIME_MODE_DEFAULT:
	tstatus->time_shift = HXD_TIME_COUNTER_030_SHIFT;
	break;
    case HXD_TPU_TIME_MODE2:
	tstatus->time_shift = HXD_TIME_COUNTER_061_SHIFT;
	break;
    case HXD_TPU_TIME_MODE3:
	tstatus->time_shift = HXD_TIME_COUNTER_122_SHIFT;
	break;
    default:
	fprintf(stderr,"hxdTrnTimeUtil.c: Can not get TPU_time_mode.\n");
	tstatus->time_shift = HXD_TIME_COUNTER_030_SHIFT;
	break;
    }

    /** 1. make lower bit **/
    int_tran_time = TPU_time_counter   << tstatus->time_shift;
    trn_time_tmp  = hxdTime_AddInt_HxdTime(&ACU_zero_time, &int_tran_time);
    trn_lower_25bit = trn_time_tmp.lower & (0x00FFFFFF<<tstatus->time_shift);

    /** 2. make upper bit **/
    tlm_time2.lower = tlm_time.lower & (0x7F000000<<tstatus->time_shift);
    tlm_time2.upper = tlm_time.upper;

    /** 3. check carry up **/
    tlm_lower_25bit = tlm_time.lower & (0x00FFFFFF<<tstatus->time_shift);
    if (trn_lower_25bit > tlm_lower_25bit){
      /** carry **/
      if(DEBUG) fprintf(stderr, "carried\n");
      trn_carry.lower  = (0x1000000<<tstatus->time_shift);
      trn_carry.upper  = 0x00;
    } else {
      /** no carry **/
      trn_carry.lower  = 0x00;
      trn_carry.upper  = 0x00;
    }
    tlm_time3 = hxdTime_Sub_HxdTime(&tlm_time2, &trn_carry);

    /** 4. merge upper/lower bit **/
    *trn_time     = hxdTime_AddInt_HxdTime(&tlm_time3, &trn_lower_25bit);

    if(DEBUG)
      fprintf(stderr,
	      "TRN=%08X, ACU=%04X.%08X, TLM=%04X.%08X, TRN_TIME=%04X.%08X\n",
	      TPU_time_counter,
	      ACU_zero_time.upper,ACU_zero_time.lower,
	      tlm_time.upper,tlm_time.lower,
	      trn_time->upper,trn_time->lower
	      );
    
    return;
}

void
hxdTime_trn_time_block_correction(int block_cnt, int block_num,
			  HxdTimeStatus *tstatus,
			  HxdTime *trn_time){
    
    unsigned int trn_edit_time = 0x00; /* format: HxdTime lower */
    
    switch ( tstatus->TPU_time_mode ){
    case HXD_TPU_TIME_MODE0:
      if(block_cnt ==2){
	trn_edit_time = TIME_STEP_05SEC;
      } else if (block_cnt==1){
        trn_edit_time = TIME_STEP_1SEC;
      } 
	break;
    case HXD_TPU_TIME_MODE_DEFAULT:
	trn_edit_time = TIME_STEP_1SEC;
	break;
    case HXD_TPU_TIME_MODE2:
	trn_edit_time = TIME_STEP_2SEC;
	break;
    case HXD_TPU_TIME_MODE3:
	trn_edit_time = TIME_STEP_4SEC;
	break;
    default:
	fprintf(stderr,"hxdTrnTimeUtil.c: Can not get TPU_time_mode.\n");
	trn_edit_time = TIME_STEP_1SEC;
	break;
    }
    
    /*
     *  Example:
     *    block cnt    3 (TRN packet; value 3 means 3-block data)
     *    block num  = 0,        1,        2,      
     *                 (-2sec)   (-1sec)   (exact) 
     */

    trn_time->lower -= trn_edit_time * ( block_cnt - 1 - block_num );
    
}
/* ==================== end of hxdTrnTimeUtil.c ====================== */


