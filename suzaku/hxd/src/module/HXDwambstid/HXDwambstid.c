/**************************************
*
*  HXDwambstid
*
*    2007/05/01 created by Y.Terada
*        Pick-up Time information from WAM FITS
*    2007/05/06 by Y.Terada
*        Support HK access
*    2007/05/14 by K.Yamaoka
*        a bug fix for the FRZ TIME(HHMMSS)
*    2008/12/10 by K.Yamaoka
*        Pick-up trg_src 
*    2009/03/02 by Y.Terada
*        write undefined value(s) when it is undefined.
**************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "anl.h"
#include "cfortran.h"
#include "fitsio.h"

#include "pil.h"
#include "aste_gethk.h"
#include "hxd/HXD.h"
#include "hxdcaldbUtil.h"
#include "atFunctions.h"
#include "aste_time.h"

char HXDwambstid_version[] = "version 0.0.5";
static char pname[] = "HXDwambstid";

/**** hk access ****/
#define GET_KEYWORD_NUM 3

enum{
  HXD_SYS_LATCH_TI,
  HXD_AE_TM_LATCH_TM,
  HXD_GB_RD_CNT
};

/** common param **/
static struct {
  char hxdbstidt_fname[PIL_LINESIZE];
  HxdBstidt bstidt_data;
  ASTE_HK *aste_hk;
  int hk_id[GET_KEYWORD_NUM];
} com;

#define HXD_TPU_N_BOARD 4

/************** start up *****************/
void
HXDwambstid_startup(int *status){
  *status = ANL_OK;
  return;
}

/**************  init    *****************/
void
HXDwambstid_init(int *status){

  /**--initialize bstidt_data--**/
  com.bstidt_data.n_row = -1;

  *status = ANL_OK;
  return;
}

/**************   com    *****************/
void
HXDwambstid_com(int *status){

  if ( *status ) { /* ftools */
    
    *status = 0;

    *status = PILGetFname("create_bstidt_name", com.hxdbstidt_fname);

    if ( *status ) {
        fprintf(stderr, "%s: PILGetFname create_bstidt_name error (%d)\n",
		pname, *status);
        *status = ANL_NG;
	return;
    } 

    *status = ANL_OK;
    return;
  }
  
  com.hxdbstidt_fname[0]='\0';
  CLtxtrd("HXD BST-ID-TABLE (bstidt) FITS name ?", 
	  com.hxdbstidt_fname,sizeof(com.hxdbstidt_fname));
  
  *status = ANL_OK;
  return;
}

/**************  histogram ***************/
void
HXDwambstid_his(int *status){
  *status = ANL_OK;
  return;
}

/************** begin run ****************/
void
HXDwambstid_bgnrun(int *status){
  int stat;
  int size;

  /**--create ae_bstidt FITS--**/
  stat = hxdcaldbUtil_bstidt_create_FITS(com.hxdbstidt_fname);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: file create error\n", pname);
    *status = ANL_NG;
    return;
  }
	
  /**--access HK--**/
  BnkfGetM("HXDgethkInit:ASTE_HK", sizeof(ASTE_HK *), &size, &com.aste_hk);
  if( com.aste_hk == NULL ){
    *status = ANL_QUIT; return;
  } else {
    com.hk_id[HXD_SYS_LATCH_TI]   
      = aste_gethk_id(com.aste_hk, "HXD_TLATCH_TIME");
    com.hk_id[HXD_AE_TM_LATCH_TM] 
      = aste_gethk_id(com.aste_hk, "HXD_AE_TM_LATCH_TM");
    com.hk_id[HXD_GB_RD_CNT] 
      = aste_gethk_id(com.aste_hk, "HXD_GB_RD_CNT");
  } 

  *status = ANL_OK;
  return;
}

/*************** analysis ****************/
void
HXDwambstid_ana(int nevent, int eventid, int *status){
  int istat;
  int size;
  int hxd_packet_update_flag;
  int board, gb_frz, gb_trg, ti;
  int  time_mode[HXD_TPU_N_BOARD] = {0,0,0,0};
  double aetime;
  AtTimeD attime;
  int yyyymmdd, hhmmss;
  
  static int prev_gb_frz[HXD_TPU_N_BOARD] = {0,0,0,0};
  static int prev_ti[HXD_TPU_N_BOARD]     = {0,0,0,0};
  static double prev_aetime[HXD_TPU_N_BOARD] = {0.0,0.0,0.0,0.0};
  static int first_trn_packet[HXD_TPU_N_BOARD] = {1,1,1,1};

  static int status_freeze = 0x0000; /** Freeze TPU-0,1,2,3 **/
  static int status_clear  = 0x0000; /** Clear  TPU-0,1,2,3 **/
  static int status_already_frz = 0;
  static int  frz_ti[HXD_TPU_N_BOARD] = {HXDBSYIDT_UNDEF_INTVAL,
					 HXDBSYIDT_UNDEF_INTVAL,
					 HXDBSYIDT_UNDEF_INTVAL,
					 HXDBSYIDT_UNDEF_INTVAL};
  static int  clr_ti[HXD_TPU_N_BOARD] = {HXDBSYIDT_UNDEF_INTVAL,
					 HXDBSYIDT_UNDEF_INTVAL,
					 HXDBSYIDT_UNDEF_INTVAL,
					 HXDBSYIDT_UNDEF_INTVAL};
  static double frz_aetime[HXD_TPU_N_BOARD] = {HXDBSYIDT_UNDEF_DBLVAL,
					       HXDBSYIDT_UNDEF_DBLVAL,
					       HXDBSYIDT_UNDEF_DBLVAL,
					       HXDBSYIDT_UNDEF_DBLVAL};
  static double clr_aetime[HXD_TPU_N_BOARD] = {HXDBSYIDT_UNDEF_DBLVAL,
					       HXDBSYIDT_UNDEF_DBLVAL,
					       HXDBSYIDT_UNDEF_DBLVAL,
					       HXDBSYIDT_UNDEF_DBLVAL};

  int tpuid;

/* 0:latch time 1:pre time (not suport) 2:next time */
  static double gethk_time_info_sys[3] = {0.0, 0.0, 0.0};
  static double gethk_time_info_hk [3] = {0.0, 0.0, 0.0}; 
  int hxd_tlatch_time    =HXDBSYIDT_UNDEF_INTVAL;
  int hxd_ae_tm_latch_tm =HXDBSYIDT_UNDEF_INTVAL;
  int gb_read            =HXDBSYIDT_UNDEF_INTVAL;


  BnkGet("HXD:ALL:UPDATE",sizeof(int), &size, &hxd_packet_update_flag);
  if ( !(hxd_packet_update_flag & HXD_UPDATE_TRN) ){
    /** out of GTI time region **/
    *status = ANL_OK;  return;
  }
  
  /**-- read data --**/
  BnkGet( "HXD:TRH:BOARD",  sizeof(int), &size, &board );
  BnkGet( "HXD:TRH:GB_TRG", sizeof(int), &size, &gb_trg);
  BnkGet( "HXD:TRH:GB_FRZ", sizeof(int), &size, &gb_frz);
  BnkGet( "HXD:TRN:PACKET_SEC_HEADER",  sizeof(int), &size, &ti );
  BnkGet( "HXD:TRN:PACKET_AETIME", sizeof(double), &size, &aetime);

 /**-- check board id --**/
  if (board <0 || HXD_TPU_N_BOARD <= board){
    anl_msg_error("%s: illegal HXD-AE TPU module id (TPU-%d)\n", pname, board);
    *status = ANL_NG;   return;
  }

  BnkGet( "HXD:TRH:TIME_MODE",  sizeof(int), &size, &time_mode[board] );

  /**==ana body==**/
  if (first_trn_packet[board]){
    /**--first data--**/
    first_trn_packet[board] = 0;
  } else {
    /**===Ana Loop Body==**/

    /** (1) GB Freeze ------------------------------------------------ **/
    if ((prev_gb_frz[board] == 0) && (gb_frz == 1) ){
      frz_ti[board]     = ti;
      frz_aetime[board] = aetime;
      status_freeze |= (0x01 << board);
      
      /** all TPU, gb freeze **/
      if(status_freeze == 0x0F){ 
	com.bstidt_data.n_row ++;
	hxdcaldbUtil_bstidt_init_a_bstidtbl(
		    &com.bstidt_data.data[com.bstidt_data.n_row] );
	for(tpuid = 0; tpuid<HXD_TPU_N_BOARD; tpuid ++){
	  com.bstidt_data.data[com.bstidt_data.n_row].frzon_aetime[tpuid] 
	    = frz_aetime[tpuid];
	  com.bstidt_data.data[com.bstidt_data.n_row].frzon_ti[tpuid]     
	    = frz_ti[tpuid];
	  com.bstidt_data.data[com.bstidt_data.n_row].time_mode[tpuid]     
	    = time_mode[tpuid];
	  aste2attimeD(frz_aetime[tpuid], &attime);
	  com.bstidt_data.data[com.bstidt_data.n_row].frzd_yyyymmdd[tpuid]
	    = 10000*attime.yr + 100*attime.mo + attime.dy;
	  com.bstidt_data.data[com.bstidt_data.n_row].frzd_hhmmss[tpuid]
	    = 10000*attime.hr + 100*attime.mn + attime.sc;
	  if(tpuid == 0){
	    com.bstidt_data.data[com.bstidt_data.n_row].trg_src
              = gb_trg;
	    com.bstidt_data.data[com.bstidt_data.n_row].yyyymmdd
	      = 10000*attime.yr + 100*attime.mo + attime.dy;
	    com.bstidt_data.data[com.bstidt_data.n_row].hhmmss
	      = 10000*attime.hr + 100*attime.mn + attime.sc;
	  }
	}

	/**HK-SYS extension**/
	if(aetime < gethk_time_info_sys[1] || gethk_time_info_sys[2] < aetime){
	  istat = aste_gethk(com.hk_id[HXD_SYS_LATCH_TI], aetime, TINT, 1, 
			     &hxd_tlatch_time, gethk_time_info_sys);
	  if ( istat == ANL_NG ){
	    anl_msg_warning("%s: get HXD_TLATCH_TIME faild.\n",pname);
	  } else {
	    com.bstidt_data.data[com.bstidt_data.n_row].frzon_t_latch_ti
	      = hxd_tlatch_time;
	  }

	  istat = aste_gethk(com.hk_id[HXD_GB_RD_CNT], aetime, TINT, 1, 
			     &gb_read, gethk_time_info_sys);
	  if ( istat == ANL_NG ){
	    anl_msg_warning("%s: get GB_RD_CNT faild.\n",pname);
	  } else {
	    com.bstidt_data.data[com.bstidt_data.n_row].frzon_gbread
	      = gb_read;
	  }
	}

	/**HK-HK extension**/
	if(aetime < gethk_time_info_hk[1] || gethk_time_info_hk[2] < aetime){
	  istat = aste_gethk(com.hk_id[HXD_AE_TM_LATCH_TM], aetime, TINT, 1, 
			     &hxd_ae_tm_latch_tm, gethk_time_info_hk);
	  if ( istat == ANL_NG ){
	    anl_msg_warning("%s: get HXD_AE_TM_LATCH_TM faild.\n",pname);
	  } else {
	    com.bstidt_data.data[com.bstidt_data.n_row].frzon_acu_t_latch
	      = hxd_ae_tm_latch_tm;
	  }
	}
	
	/** for the next bst **/
	status_freeze = 0x00;
	status_already_frz = 1;
      }
    }


    /** (2) Clear Freeze: check ----------------------------------------- **/
    if ((prev_gb_frz[board] == 1) && (gb_frz == 0) ){
      clr_ti[board]     = ti;
      clr_aetime[board] = aetime;
      status_clear |= (0x01 << board);

      /** all TPU, gb clear **/
      if(status_clear == 0x0F){
	if(! status_already_frz) {
	  com.bstidt_data.n_row ++; /* first writing */
	}
	for(tpuid = 0; tpuid<HXD_TPU_N_BOARD; tpuid ++){
	  com.bstidt_data.data[com.bstidt_data.n_row].frzof_aetime[tpuid] 
	    = clr_aetime[tpuid];
	  com.bstidt_data.data[com.bstidt_data.n_row].time_mode[tpuid]     
	    = time_mode[tpuid];
	}

	/**HK-SYS extension**/
	if(aetime < gethk_time_info_sys[1] || gethk_time_info_sys[2] < aetime){
	  istat = aste_gethk(com.hk_id[HXD_GB_RD_CNT], aetime, TINT, 1, 
			     &gb_read, gethk_time_info_sys);
	  if ( istat == ANL_NG ){
	    anl_msg_warning("%s: get GB_RD_CNT faild.\n",pname);
	  } else {
	    com.bstidt_data.data[com.bstidt_data.n_row].frzon_gbread
	      = gb_read;
	  }
	}

	/** for the next bst **/
	status_clear = 0x00;
	status_already_frz = 0; /* clear */
      }
    } /* ------------------------------------------------------------------ */

  } /*===end of body===*/
  
  /**-- copy values to static area --**/
  prev_gb_frz[board] = gb_frz;
  prev_ti    [board] = ti;
  prev_aetime[board] = aetime;
  
  *status = ANL_OK;
  return;
}

/************** end run  *****************/
void
HXDwambstid_endrun(int *status){
  int stat;

  /**--write ae_bstidt FITS--**/
  com.bstidt_data.n_row ++;

  stat = hxdcaldbUtil_bstidt_write_FITS(&com.bstidt_data);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: file write error GHF\n", pname);
    *status = ANL_NG;
    return;
  }

  *status = ANL_OK;
  return;
}

/**************** exit *******************/
void
HXDwambstid_exit(int *status){
  int stat;

  /**--close ae_bstidt FITS--**/
  stat = hxdcaldbUtil_bstidt_close_FITS();
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: file close error\n", pname);
    return;
  }

  *status = ANL_OK;
  return;
}

/**************** static ******************/

/* EOF */
