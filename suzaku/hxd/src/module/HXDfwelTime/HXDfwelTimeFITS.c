/*
 *   HXDfwelTimeFITS v0.0.1 test version for ftools
 *                   v0.0.2 first fix version
 *                   v0.0.3 for well time determination
 *                   develop aste_gethk
 *                   v 0.1.5 Y.Terada 2003.04.15, pre HXD-II format 
 *                   v 0.1.6 Y.Terada 2003.05.03  HXD-II I/O, read PWH.
 *                   v 0.2.1 Y.Terada 2004.03.13  
 *                        Quit, only when time is over HK time range.
 *                   v 0.2.5 R.Miyawaki 2005.05.28, read timfile  
 *                   v 0.2.7 Y.Terada 2005.06.08, use S_TIME.
 *                           new version of aste_gethk (v2.x)
 *                   v 0.2.8 Y.Terada 2005.06.13,
 *                           don't QUIT even when time is over-range.
 *                   v 0.3.3 Y.Terada  2005.11.05
 *                           Put PIL parameters into Bnk
 *                   v 0.3.4 Y.Terada  2005.11.08
 *                           shorten BNK name
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "fitsio.h"
#include "cfortran.h"
#include "hxd/HXD.h"
#include "pil.h"
/*#include <pfile.h>*/
/*#include "uclpario.h"*/

#include "aste_gethk.h"
#include "hxdFitsHeaderUtil.h"

#define GET_KEYWORD_NUM 19
/*#define FILELIST_MAX_LENGTH 1024*/
#define FILELIST_MAX_LENGTH PIL_LINESIZE

char HXDfwelTimeFITS_version[] = "version 0.3.8";

static char pname[] = "HXDfwelTimeFITS";

static struct{
  char tim_filename[FILELIST_MAX_LENGTH];
} com;

/*static char hk_filelist[FILELIST_MAX_LENGTH];*/
static ASTE_HK *aste_hk;
static int hk_id[GET_KEYWORD_NUM];
static int hxdfweltime_use_pwh;

enum{
  HXD_SYS_LATCH_TI,
  HXD_AE_TM_LATCH_TM,
  HXD_WPU_CLK_RATE,
  HXD_WEL_PWH1,    HXD_WEL_PWH2,    HXD_WEL_PWH3,    HXD_WEL_PWH4,
  HXD_WEL_PWH5,    HXD_WEL_PWH6,    HXD_WEL_PWH7,    HXD_WEL_PWH8,
  HXD_WEL_PWH9,    HXD_WEL_PWH10,   HXD_WEL_PWH11,   HXD_WEL_PWH12,
  HXD_WEL_PWH13,   HXD_WEL_PWH14,   HXD_WEL_PWH15,    HXD_WEL_PWH16
};

void
HXDfwelTimeFITS_startup(int *status)
{
  BnkDef("HXD:PIL:tim_filename", sizeof(char)*HXDFITSHEADER_LINESIZE);
  *status = ANL_OK;
}

void
HXDfwelTimeFITS_com(int *status)
{
    
  if ( *status ) { /* ftools */
    
    *status = 0;

    *status = PILGetFname("tim_filename", com.tim_filename);

    if ( *status ) {
      fprintf(stderr, "%s: PILGetFname TIMFILE error (%d)\n", pname, *status);
      *status = ANL_QUIT;
      exit(-1);
    } else {
      int string_size = strlen(com.tim_filename);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning tim_filename is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:tim_filename", string_size, com.tim_filename);
    }

    
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }
    
    *status = ANL_OK;
    return;
  }

  com.tim_filename[0]='\0';

  CLtxtrd("input .tim FITS file name (HXDtimeSet)",
          com.tim_filename,
          sizeof(com.tim_filename));
  
  /*
     hk_filelist[0] = '\0';
     
     CLtxtrd("HXD (none).hk fits file name",
     hk_filelist, sizeof(hk_filelist) );
     */
  
  *status = ANL_OK;
  
}

void
HXDfwelTimeFITS_init(int *status)
{
    
  BnkDef( "HXDfwelTime:HXD_SYS_LATCH_TI" , sizeof(int) );
  /* "HXD:SYS:TLTIME" */
  BnkDef( "HXDfwelTime:HXD_AE_TM_LATCH_TM", sizeof(int) );
  /* "HXD:HKA:LATCH_TIM" */
  BnkDef( "HXDfwelTime:HXD_WPU_CLK_RATE", sizeof(int) );
  /* "HXD:HKA:WPU_CLK_RATE" */
  
  /*
     at HXDeventFitsRead
     BnkDef( "HXD:WEL:PACKET_SEC_HEADER", sizeof(int));
     BnkDef( "HXD:WEL:WELTIME", sizeof(int));
     BnkDef( "HXD:WEL:UNITID", sizeof(int));
     BnkDef( "HXD:WEL:EV_TIME", sizeof(double));
     */
  
  BnkDef( "HXDfwelTime:HXD_SYS_TIME", sizeof(double) );
  BnkDef( "HXDfwelTime:HXD_HK_TIME", sizeof(double) );
    
  BnkDef( "HXDfwelTime:PWH", sizeof(int)*HXD_PWH_DATANUM);

  BnkDef("HXDfwelTime:TIM_FILE_NAME", sizeof(com.tim_filename));

  BnkfPutM("HXDfwelTime:TIM_FILE_NAME",
	   sizeof(com.tim_filename), com.tim_filename);

  *status = ANL_OK;
}

void
HXDfwelTimeFITS_his(int *status)
{
    *status = ANL_OK;
}

void
HXDfwelTimeFITS_bgnrun(int *status)
{
  
  int istat;
  int size;
  
  /*aste_hk = aste_gethk_init ( hk_filelist );*/
  BnkfGetM("HXDgethkInit:ASTE_HK", sizeof(ASTE_HK *), &size, &aste_hk);

  BnkfGetM("HXDeventFitsRead:USE_PWH", sizeof(int),   
	   &size, &hxdfweltime_use_pwh);
  
  if( aste_hk == NULL ){
    *status = ANL_QUIT;
    return;
  } else {

    hk_id[HXD_SYS_LATCH_TI]   = aste_gethk_id(aste_hk, "HXD_TLATCH_TIME");
    hk_id[HXD_AE_TM_LATCH_TM] = aste_gethk_id(aste_hk, "HXD_AE_TM_LATCH_TM");
    hk_id[HXD_WPU_CLK_RATE]   = aste_gethk_id(aste_hk, "HXD_WPU_CLK_RATE");

    if (hxdfweltime_use_pwh == FALSE) {  *status = ANL_OK;  return; }
    hk_id[HXD_WEL_PWH1]  = aste_gethk_id(aste_hk, "HXD_WEL_PWH1");
    hk_id[HXD_WEL_PWH2]  = aste_gethk_id(aste_hk, "HXD_WEL_PWH2");
    hk_id[HXD_WEL_PWH3]  = aste_gethk_id(aste_hk, "HXD_WEL_PWH3");
    hk_id[HXD_WEL_PWH4]  = aste_gethk_id(aste_hk, "HXD_WEL_PWH4");
    hk_id[HXD_WEL_PWH5]  = aste_gethk_id(aste_hk, "HXD_WEL_PWH5");
    hk_id[HXD_WEL_PWH6]  = aste_gethk_id(aste_hk, "HXD_WEL_PWH6");
    hk_id[HXD_WEL_PWH7]  = aste_gethk_id(aste_hk, "HXD_WEL_PWH7");
    hk_id[HXD_WEL_PWH8]  = aste_gethk_id(aste_hk, "HXD_WEL_PWH8");
    hk_id[HXD_WEL_PWH9]  = aste_gethk_id(aste_hk, "HXD_WEL_PWH9");
    hk_id[HXD_WEL_PWH10] = aste_gethk_id(aste_hk, "HXD_WEL_PWH10");
    hk_id[HXD_WEL_PWH11] = aste_gethk_id(aste_hk, "HXD_WEL_PWH11");
    hk_id[HXD_WEL_PWH12] = aste_gethk_id(aste_hk, "HXD_WEL_PWH12");
    hk_id[HXD_WEL_PWH13] = aste_gethk_id(aste_hk, "HXD_WEL_PWH13");
    hk_id[HXD_WEL_PWH14] = aste_gethk_id(aste_hk, "HXD_WEL_PWH14");
    hk_id[HXD_WEL_PWH15] = aste_gethk_id(aste_hk, "HXD_WEL_PWH15");
    hk_id[HXD_WEL_PWH16] = aste_gethk_id(aste_hk, "HXD_WEL_PWH16");

  }

  *status = ANL_OK;
  
}


void
HXDfwelTimeFITS_ana(int nevent, int eventid, int *status)
{

  int size;
  int istat = ANL_OK;
  
  double time;

  int hxd_tlatch_time;
  int hxd_ae_tm_latch_tm;
  int hxd_wpu_clk_rate;

  /* 0:latch time 1:pre time (not suport) 2:next time */
  static double gethk_time_info_sys[3] = {0.0, 0.0, 0.0};
  static double gethk_time_info_hk [3] = {0.0, 0.0, 0.0}; 
  static double gethk_time_info_pwh[3] = {0.0, 0.0, 0.0};
  int pwh[HXD_PWH_DATANUM];
  
  int update;
  
  BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
  
  if( !(update & HXD_UPDATE_WEL) ){
    *status = ANL_OK;
    return;
  }

  BnkfGetM("HXD:WEL:PACKET_S_TIME", sizeof(double), &size, &time );

  /*************** SYS Extension **************/
  if(time < gethk_time_info_sys[1] || gethk_time_info_sys[2] < time){

    istat = aste_gethk(hk_id[HXD_SYS_LATCH_TI], time, TINT, 1, 
		       &hxd_tlatch_time, gethk_time_info_sys);
    if ( istat == ANL_NG ){
      if(time > gethk_time_info_sys[2]){
	fprintf(stderr, "%s_ana: get HXD_TLATCH_TIME faild: time over range. \n", pname);
	/*
	*status = ANL_QUIT;
	return;
	*/
      }  else {
	fprintf(stderr, "%s_ana: get HXD_TLATCH_TIME faild: Initial mode. continue.\n", pname);
	istat = ANL_OK; 
      }
    } else {
      BnkfPutM( "HXDfwelTime:HXD_SYS_LATCH_TI", sizeof(int),
		&hxd_tlatch_time );
    }

    if (istat == ANL_OK) {
      update |= HXD_UPDATE_SYS;
      BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update);
      BnkfPutM( "HXDfwelTime:HXD_SYS_TIME" , sizeof(double),
		&gethk_time_info_sys[0] );
    }
  }

  /*************** HK Extension ***************/
  if(time < gethk_time_info_hk[1] || gethk_time_info_hk[2] < time){
    
    istat = aste_gethk ( hk_id[HXD_AE_TM_LATCH_TM], time, TINT, 1, 
			 &hxd_ae_tm_latch_tm, gethk_time_info_hk );
/*    fprintf(stderr, "val=%d (access=%14.13fE+8, stime=%14.13fE+8,%14.13fE+8,%14.13fE+8) \n", 
	    hxd_ae_tm_latch_tm, time/100000000, gethk_time_info_hk[0]/100000000,
	    gethk_time_info_hk[1]/100000000,gethk_time_info_hk[2]/100000000);*/
    if( istat == ANL_NG ){
      if(time > gethk_time_info_hk[2]){
	fprintf(stderr, "%s_ana: get HXD_AE_TM_LATCH_TM faild: time over range.\n", pname);
	/*
	*status = ANL_QUIT;
	return;
	*/
      }  else {
	fprintf(stderr, "%s_ana: get HXD_AE_TM_LATCH_TM faild: Initial mode. continue.\n", pname);
	istat = ANL_OK; 
      }
    } else {
      BnkfPutM( "HXDfwelTime:HXD_AE_TM_LATCH_TM", sizeof(int), 
		&hxd_ae_tm_latch_tm );
    }

    istat = aste_gethk ( hk_id[HXD_WPU_CLK_RATE], time, TINT, 1, 
			 &hxd_wpu_clk_rate, gethk_time_info_hk );
    if( istat == ANL_NG ){
      if(time > gethk_time_info_hk[2]){
	fprintf(stderr, "%s_ana: get HXD_WPU_CLK_RATE faild: time over range. \n", pname);
	/*
	*status = ANL_QUIT;
	return;
	*/
      }  else {
	fprintf(stderr, "%s_ana: get HXD_WPU_CLK_RATE faild: Initial mode. continue.\n", pname);
	istat = ANL_OK; 
      }
    } else {
      BnkfPutM( "HXDfwelTime:HXD_WPU_CLK_RATE", sizeof(int),
		&hxd_wpu_clk_rate );	    
    }
    
    if (istat == ANL_OK){
      update |= HXD_UPDATE_HK;
      BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update);
      BnkfPutM( "HXDfwelTime:HXD_HK_TIME" , sizeof(double),
		&gethk_time_info_hk[0] );	    
    }
  }

  /*************** PWH Extension ***************/
  /*** USE PWH or not ***/
  if (hxdfweltime_use_pwh == FALSE) {  
    *status = ANL_OK;
    return;
  }

  if(time < gethk_time_info_pwh[1] || gethk_time_info_pwh[2] < time){

    istat = aste_gethk ( hk_id[HXD_WEL_PWH1], time, TINT, 1, 
			 &pwh[0], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH1 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH2], time, TINT, 1, 
			 &pwh[1], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH2 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH3], time, TINT, 1, 
			 &pwh[2], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH3 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH4], time, TINT, 1, 
			 &pwh[3], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH4 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH5], time, TINT, 1, 
			 &pwh[4], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH5 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH6], time, TINT, 1, 
			 &pwh[5], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH6 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH7], time, TINT, 1, 
			 &pwh[6], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH7 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH8], time, TINT, 1, 
			 &pwh[7], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH8 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH9], time, TINT, 1, 
			 &pwh[8], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH9 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH10], time, TINT, 1, 
			 &pwh[9], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH10 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH11], time, TINT, 1, 
			 &pwh[10], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH11 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH12], time, TINT, 1, 
			 &pwh[11], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH12 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH13], time, TINT, 1, 
			 &pwh[12], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH13 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH14], time, TINT, 1, 
			 &pwh[13], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH14 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH15], time, TINT, 1, 
			 &pwh[14], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH15 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    istat = aste_gethk ( hk_id[HXD_WEL_PWH16], time, TINT, 1, 
			 &pwh[15], gethk_time_info_pwh );
    if ( istat == ANL_NG ){
      fprintf(stderr, "%s_ana: get HXD_WEL_PWH16 faild.\n", pname);
      *status = ANL_QUIT;
      return;
    } 

    BnkfPutM ("HXDfwelTime:PWH", sizeof(int)*HXD_PWH_DATANUM, &pwh);

    update |= HXD_UPDATE_PWH;
    BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update);
    
  }
  
  *status = ANL_OK;
    
}

void
HXDfwelTimeFITS_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDfwelTimeFITS_exit(int *status)
{
    *status = ANL_OK;
}
