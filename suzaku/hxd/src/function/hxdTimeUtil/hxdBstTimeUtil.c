/* -----------------------------------------------------------------------
 *   burst_time_determination
 *                        Time Determination function for BST data.
 *                        created  1999.10.08  By Y.Terada
 *                        modefied 2005.09.17  By Y.Terada for Suzaku
 *                        modefied 2005.09.27  By Y.Terada 
 *                        modefied 2005.10.14  By R.Miyawaki
 *                        modefied 2005.10.15  By R.Miyawaki
 *                        modefied 2013.10.27  By Y.Terada 
 * ----------------------------------------------------------------------
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "atFunctions.h"
#include "aste_time.h"
#include "anl.h"
#include "hxd/HXD.h"
#include "hxdTime.h"
#include "hxdTimeUtil.h"

#define DEBUG 0
void
hxdTime_bst_time_determination(unsigned int bst_freezed_time,
			       HxdTimeStatus *tstatus,
			       HxdTime ACU_zero_time, HxdTime tlm_time,
			       HxdTime *bst_time){

  HxdTime frz_time, bst_time_tmp;
  HxdTime tlm_time2, tlm_time3;
  unsigned int bst_upper_2bit;
  unsigned int tlm_upper_2bit;
  HxdTime bst_carry;

  switch ( tstatus->TPU_time_mode ){
  case HXD_TPU_TIME_MODE0:
    tstatus->time_shift = HXD_TIME_COUNTER_015_SHIFT;
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

  /** 1. make lower bit, (after conv. bst_freezed_time into HxdTime format) */
  frz_time.lower =(bst_freezed_time << tstatus->time_shift) & 0x7FFFFFFF;
  frz_time.upper =(bst_freezed_time)>>(31 - tstatus->time_shift);
  bst_time_tmp = hxdTime_Sum_HxdTime(&ACU_zero_time, &frz_time);

  /** 2. make upper bit **/
  tlm_time2.lower = 0x00;
  tlm_time2.upper = tlm_time.upper & (0x0E<<tstatus->time_shift);

  /** 3. check carry up **/
  bst_upper_2bit = bst_time_tmp.upper & (0x07>>tstatus->time_shift);
  tlm_upper_2bit = tlm_time.upper     & (0x07>>tstatus->time_shift);
  if (bst_upper_2bit > tlm_upper_2bit){
    /** carry **/
    if (DEBUG)fprintf(stderr, "carry (bst 0x0%X , tlm 0x%1X)\n",
		      bst_upper_2bit, tlm_upper_2bit);
    bst_carry.lower = 0x00;
    bst_carry.upper = (0x02<<tstatus->time_shift);
  } else {
    /** no carry **/
    bst_carry.lower = 0x00;
    bst_carry.upper = 0x00;
  }
  tlm_time3 = hxdTime_Sub_HxdTime(&tlm_time2, &bst_carry);

  /** 4. merge upper/lower bit **/
  bst_time->lower  = bst_time_tmp.lower ;
  bst_time->lower += tlm_time3.lower;
  bst_time->upper  = bst_time_tmp.upper & (0x07>>tstatus->time_shift);
  bst_time->upper += tlm_time3.upper;

  /* timing: FREEZED TIME */
  if (DEBUG){
    fprintf(stderr,
	    "FRZ=%08X(%04X.%08X) + ACU=%04X.%08X,->%04X.%08X, TLM=%04X.%08X, BST=%04X.%08X\n",
	    bst_freezed_time,frz_time.upper, frz_time.lower,
	    ACU_zero_time.upper,ACU_zero_time.lower,
	    bst_time_tmp.upper, bst_time_tmp.lower,
	    tlm_time.upper,tlm_time.lower,
	    bst_time->upper,bst_time->lower
	    );
  }

/*  hxdTime_bst_time_block_correction( tstatus, bst_time); */
    /* timing TRANSIENT TIME */
    
    return;
   
}

void
hxdTime_bst_time_block_correction(HxdTimeStatus *tstatus, HxdTime *bst_time){
    
/* ***************** HXD TPU Timing chart (burst/transient data) ******************
 *                                                    [memo: 1999/4/26 Y.Terada]
 * (1) TRANSIENT TIME          ( transient packet: 3 Byte ) every 1 sec (30.5usec)
 * (2) GAMMA BURST TIME        ( transient packet: 3 Byte ) every 1 sec (15.6msec) 
 * (3) GAMMA BURST SEND TIME   ( burst     packet: 2 Byte ) every BST   (2.0  sec)
 * (4) GAMMA BURST FREEZED TIME( burst     packet: 3 Byte ) every BST   (15.6msec) 
 *
 * DP Time  ========|==============|=========//===|==============|===========>
 *                    -1             1             112            113
 * PH latch ------------|--------------|-----//-------|--------------|------->
 * (TPU)                      ^         ^              ^
 *   TRN                      !         !              !
 *   BST                burst judge     |             time(4)          ...time(3)
 *                        time(2)    time(1)           !
 * BST data ------------|--------------|-----//-------|- - - - - - - - - - - ..
 *                                      _____//______________________________..
 * BST_Flg  ___________________________|
 *                                                 <<Freeze!>>        _______..
 * BST_Frz  _________________________________//______________________|
 * (Trn data)
 *
 *                                                    [memo: 2005/09/17 Y.Terada]
 * (1) TRANSIENT TIME          ( transient packet: 3 Byte ) every 1 sec (30.5usec)
 * (2) GAMMA BURST TIME        ( transient packet: 3 Byte ) every 1 sec (15.6msec) 
 * (3) GAMMA BURST SEND TIME   ( burst     packet: 2 Byte ) every BST   (2.0  sec)
 * (4) GAMMA BURST FREEZED TIME( burst     packet: 3 Byte ) every BST   (30.5usec) 
 * **************************************************************************** */
#define BST_N_BIN 128
    unsigned int trn_edit_time; /* format:   HxdTime lower */
    unsigned int delta_time;    /* BST coverage time range */
    
    switch ( tstatus->TPU_time_mode ){
    case HXD_TPU_TIME_MODE0:
        trn_edit_time = TIME_STEP_05SEC;
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
        fprintf(stderr,"hxdBstTimeUtil.c: Can not get TPU_time_mode.\n");
        trn_edit_time = TIME_STEP_1SEC;
        break;
    }

    delta_time = trn_edit_time * BST_N_BIN;
    
    *bst_time = hxdTime_SubInt_HxdTime ( bst_time, &delta_time );
    return;
}    

/** This function is not used for HXD-II. */
int
hxdTime_bst_add_trntime(unsigned int bst_freezed_time, unsigned int TRN_time_counter,
			unsigned int *extended_bst_freezed_time){
    
    /* ********************* TIME COVERAGE ************************
     *                |   256s|     1s|    4ms|   15us|
     *          --+---+---+---+---+---+---+---+---+---+-
     *            +============= TI ==============+   +
     *            +   +   +  ========TRN_TIME======== +
     *            +  =======FRZ_TIME========  +   +   +
     *            +  ----------EXTEND_FRZ_TIME------- +
     *            +  +---+---+---+---+---+---+---+---++
     * ************************************************************/
   int istat = ANL_OK;

   *extended_bst_freezed_time = bst_freezed_time << 9;

   /* ========== (I) Check Input value ============ */
   if( (bst_freezed_time & 0x007FFF) != (TRN_time_counter>>9) ) {
       fprintf(stderr,
	       "hxdTimeUtil: BST: bst_freezed_time and TRN_time_counter mismatch\n");
       istat = ANL_QUIT;
       return istat;
   }
   
   /* ====== (II) ADD FINE TIME RESOLUTION BITS ====== */
   *extended_bst_freezed_time += TRN_time_counter & 0x000001FF;
   
   return istat;
}
