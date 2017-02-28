/*
 *   HXDftrnTimeFITS v0.0.1 1999-10-15 created  by Y.Terada
 *                   v0.1.0 2003-05-03 for HXD-II  Y.Terada
 *                   v0.2.1 2004-03-13 modefied by Y.Terada 
 *                    not to quit, when time-before.
 *                   v0.2.3 2005-05-28 read timfile by R.Miyawaki 
 *                   v0.2.6 2005-06-09 use S_TIME. debug for BNK name
 *                    new version of aste_gethk (v2.x) by R.Miyawaki 
 *                   v0.2.8 2005-06-14 by Y.Terada
 *                    don't Quit, even when time is over-range
 *                   v 0.3.1             Y.Terada 2005.09.26
 *                    pre-search in HK FITS, before _ana process.
 *                   v 0.3.2             Y.Terada 2005.11.05
 *                    put PIL value into FITS header
 *                   v 0.3.3             Y.Terada 2005.11.08
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

#include "aste_gethk.h"
#include "hxdFitsHeaderUtil.h"

#define GET_KEYWORD_NUM 3
/*#define FILELIST_MAX_LENGTH 1024*/
#define FILELIST_MAX_LENGTH PIL_LINESIZE

char HXDftrnTimeFITS_version[] = "version 0.3.3";

static char pname[] = "HXDftrnTimeFITS";

static struct{
    char tim_filename[FILELIST_MAX_LENGTH];
} com;

/*static char hk_filelist[FILELIST_MAX_LENGTH];*/
static ASTE_HK *aste_hk;
static int hk_id[GET_KEYWORD_NUM];

enum{
    HXD_SYS_LATCH_TI,
    HXD_AE_TM_LATCH_TM,
    HXD_WPU_CLK_RATE
};

void
HXDftrnTimeFITS_startup(int *status)
{
  BnkDef("HXD:PIL:tim_filename", sizeof(char)*HXDFITSHEADER_LINESIZE);
  *status = ANL_OK;
}

void
HXDftrnTimeFITS_com(int *status)
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
HXDftrnTimeFITS_init(int *status)
{
    
  BnkDef( "HXDftrnTime:HXD_SYS_LATCH_TI" , sizeof(int) );
  /* "HXD:SYS:TLTIME" */
  BnkDef( "HXDftrnTime:HXD_AE_TM_LATCH_TM", sizeof(int) );
  /* "HXD:HKA:LATCH_TIM" */
  
  BnkDef( "HXDftrnTime:HXD_SYS_TIME", sizeof(double) );
  BnkDef( "HXDftrnTime:HXD_HK_TIME", sizeof(double) );

  BnkDef("HXDftrnTime:TIM_FILE_NAME", sizeof(com.tim_filename));

  BnkfPutM("HXDftrnTime:TIM_FILE_NAME",
	   sizeof(com.tim_filename), com.tim_filename);

  *status = ANL_OK;
  
}

void
HXDftrnTimeFITS_his(int *status)
{
    *status = ANL_OK;
}

void
HXDftrnTimeFITS_bgnrun(int *status)
{
  
  int istat;
  int size;
  
  /*aste_hk = aste_gethk_init ( hk_filelist );*/
  BnkfGetM("HXDgethkInit:ASTE_HK", sizeof(ASTE_HK *), &size, &aste_hk);
  
  if( aste_hk == NULL ){
    *status = ANL_QUIT;
    return;
  } else {

      hk_id[HXD_SYS_LATCH_TI]   = aste_gethk_id(aste_hk, "HXD_TLATCH_TIME");
      hk_id[HXD_AE_TM_LATCH_TM] = aste_gethk_id(aste_hk, "HXD_AE_TM_LATCH_TM");

  } 
  
  *status = ANL_OK;
  
}


void
HXDftrnTimeFITS_ana(int nevent, int eventid, int *status)
{

  int size;
  int istat = ANL_OK;
  
  double time;
  
  int hxd_tlatch_time;
  int hxd_ae_tm_latch_tm;
  
/* 0:latch time 1:pre time (not suport) 2:next time */
  static double gethk_time_info_sys[3] = {0.0, 0.0, 0.0};
  static double gethk_time_info_hk [3] = {0.0, 0.0, 0.0}; 
  
  int update;
  
  BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
/*fprintf(stderr,"READ UPDATE = 0x%x\n",update);*/
  
  if( !(update & HXD_UPDATE_TRN) ){
    *status = ANL_OK;
    return;
  }
  
  BnkfGetM("HXD:TRN:PACKET_S_TIME", sizeof(double), &size, &time );
  
  /*************** SYS Extension **************/
  if(time < gethk_time_info_sys[1] || gethk_time_info_sys[2] < time){

      istat = aste_gethk(hk_id[HXD_SYS_LATCH_TI], time, TINT, 1, 
			 &hxd_tlatch_time, gethk_time_info_sys);
      if ( istat == ANL_NG ){
	  if(time > gethk_time_info_sys[2]){
	      fprintf(stderr, "%s_ana: get HXD_TLATCH_TIME faild: time over range.\n", pname);
	      /*
	      *status = ANL_QUIT;
	      return;
	      */
	  }  else {
	      fprintf(stderr, "%s_ana: get HXD_TLATCH_TIME faild: Initial mode. continue.\n", pname);
	      istat = ANL_OK; 
	  }
      } else {
	  BnkfPutM( "HXDftrnTime:HXD_SYS_LATCH_TI", sizeof(int),
		    &hxd_tlatch_time );
      }

      if (istat == ANL_OK) {
	  update |= HXD_UPDATE_SYS;
	  BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update);
	  BnkfPutM( "HXDftrnTime:HXD_SYS_TIME" , sizeof(double),
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
	      fprintf(stderr, "%s_ana: get HXD_AE_TM_LATCH_TM faild: time over range. \n", pname);
	      /*
	      *status = ANL_QUIT;
	      return;
	      */
	  }  else {
	      fprintf(stderr, "%s_ana: get HXD_AE_TM_LATCH_TM faild: Initial mode. continue.\n", pname);
	      istat = ANL_OK; 
	  }
      } else {
	  BnkfPutM( "HXDftrnTime:HXD_AE_TM_LATCH_TM", sizeof(int), 
		    &hxd_ae_tm_latch_tm );
      }

      if (istat == ANL_OK){
	  update |= HXD_UPDATE_HK;
	  BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update);
	  BnkfPutM( "HXDftrnTime:HXD_HK_TIME" , sizeof(double),
		    &gethk_time_info_hk[0] );           
      }
  }

    *status = ANL_OK;
    
}

void
HXDftrnTimeFITS_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDftrnTimeFITS_exit(int *status)
{
    *status = ANL_OK;
}
