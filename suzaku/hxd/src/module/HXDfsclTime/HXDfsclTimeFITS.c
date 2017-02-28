/*
 *   HXDfsclTimeFITS:
 *      v0.0.1         1999-12-13 Y.Terada created
 *      v0.0.2         1999-12-13 Y.Terada
 *      v0.0.3         2000-01-21 Y.Terada
 *      v0.1.0         2003-05-03 Y.Terada
 *      v 0.2.1              Y.Terada 2004.03.11
 *         Add warning when time is out_of_range.
 *      v 0.2.2              Y.Terada 2004.03.12
 *         not to QUIT, when time is not yet coming.
 *         Quit, when time is over HK time range.
 *      v 0.2.5 R.Miyawaki 2005.05.28, read timfile  
 *      v 0.2.8 R.Miyawaki 2005.06.09, use S_TIME. debug for BNK name
 *         new version of aste_gethk (v2.x)
 *      v 0.3.2 Y.Terada   2005.11.05, write PIL parameters
 *      v 0.3.3 Y.Terada   2005.11.08, shorten Bnk name
 *      v 0.3.4 H.Takahashi 2006.02.12, introduce gethk_time_info_hk_pre
 *      v 0.3.5 H.Takahashi 2006.02.13, correct position of aste_gethk
 *                                        for HK data (HXD_AE_TM_LATCH_TM)
 *      v 0.3.6 Y.Terada   2006.08/23,  check TSTART/TSTOP
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
/* #include "uclpario.h" */

#include "aste_gethk.h"
#include "hxdFitsHeaderUtil.h"

#define GET_KEYWORD_NUM 3
/*#define FILELIST_MAX_LENGTH 1024*/
#define FILELIST_MAX_LENGTH PIL_LINESIZE

char HXDfsclTimeFITS_version[] = "version 0.3.6";

static char pname[] = "HXDfsclTimeFITS";

static struct{
  char tim_filename[FILELIST_MAX_LENGTH];
} com;

/*static char hk_filelist[FILELIST_MAX_LENGTH];*/
static ASTE_HK *aste_hk;
static int hk_id[GET_KEYWORD_NUM];
static double hxdscl_tstart, hxdscl_tstop;
static double hxdscl_tbegin_hk, hxdscl_tend_hk;
static double hxdscl_tbegin_sys, hxdscl_tend_sys;

enum{
    HXD_SYS_LATCH_TI,
      HXD_AE_TM_LATCH_TM,
      HXD_WPU_CLK_RATE
};

void
HXDfsclTimeFITS_startup(int *status)
{
  BnkDef("HXD:PIL:tim_filename", sizeof(char)*HXDFITSHEADER_LINESIZE);
    *status = ANL_OK;
}

void
HXDfsclTimeFITS_com(int *status)
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
HXDfsclTimeFITS_init(int *status)
{
    
  BnkDef( "HXDfsclTime:HXD_SYS_LATCH_TI" , sizeof(int) );
  /* "HXD:SYS:TLTIME" */
  BnkDef( "HXDfsclTime:HXD_AE_TM_LATCH_TM", sizeof(int) );
  /* "HXD:HKA:LATCH_TIM" */
  
  BnkDef( "HXDfsclTime:HXD_SYS_TIME", sizeof(double) );
  BnkDef( "HXDfsclTime:HXD_HK_TIME", sizeof(double) );
    
  BnkDef( "HXDfsclTime:HXD_WPU_CLK_RATE", sizeof(int) );

  BnkDef("HXDfsclTime:TIM_FILE_NAME", sizeof(com.tim_filename));

  BnkfPutM("HXDfsclTime:TIM_FILE_NAME",
           sizeof(com.tim_filename), com.tim_filename);

  *status = ANL_OK;
  
}

void
HXDfsclTimeFITS_his(int *status)
{
    *status = ANL_OK;
}

void
HXDfsclTimeFITS_bgnrun(int *status)
{
  
  int istat;
  int size;
  double tstart,tstop;

/* aste_hk = aste_gethk_init ( hk_filelist );*/
  BnkfGetM("HXDgethkInit:ASTE_HK", sizeof(ASTE_HK *), &size, &aste_hk);
  
  if( aste_hk == NULL ){
    *status = ANL_QUIT;
    return;
  } else {

      hk_id[HXD_SYS_LATCH_TI]   = aste_gethk_id(aste_hk, "HXD_TLATCH_TIME");
      hk_id[HXD_AE_TM_LATCH_TM] = aste_gethk_id(aste_hk, "HXD_AE_TM_LATCH_TM");
      hk_id[HXD_WPU_CLK_RATE]   = aste_gethk_id(aste_hk, "HXD_WPU_CLK_RATE");

  } 
  
  BnkfGetM("HXDsclFitsRead:TSTART", sizeof(double), &size, &hxdscl_tstart);
  BnkfGetM("HXDsclFitsRead:TSTOP",  sizeof(double), &size, &hxdscl_tstop);
  BnkfGetM("HXDsclFitsRead:HK:TBEGIN", sizeof(double), &size, 
	   &hxdscl_tbegin_hk);
  BnkfGetM("HXDsclFitsRead:HK:TEND", sizeof(double), &size, 
	   &hxdscl_tend_hk);
  BnkfGetM("HXDsclFitsRead:SYS:TBEGIN", sizeof(double), &size, 
	   &hxdscl_tbegin_sys);
  BnkfGetM("HXDsclFitsRead:SYS:TEND", sizeof(double), &size, 
	   &hxdscl_tend_sys);


  *status = ANL_OK;
  
}

void
HXDfsclTimeFITS_ana(int nevent, int eventid, int *status){
    int size;
    int istat = ANL_OK;
    
    double time;
    
    static int hxd_tlatch_time    = 0;
    static int hxd_ae_tm_latch_tm = 0;
    
/* 0:latch time 1:pre time (not suport) 2:next time */
    static double gethk_time_info_sys[3] = {0.0, 0.0, 0.0};
    static double gethk_time_info_hk [3] = {0.0, 0.0, 0.0}; 

    int update;
    
    int hxd_wpu_clk_rate;

    BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
    
    if( !(update & HXD_UPDATE_SCL) ){
	*status = ANL_OK;
	return;
    }

    BnkfGetM("HXD:SCL:PACKET_S_TIME", sizeof(double), &size, &time );
    if ( (time< hxdscl_tstart) || (hxdscl_tstop < time) ){
      /** out of time range **/
      *status = ANL_OK;
      return;
    }

    /*************** HK Extension ***************/

    if(time < gethk_time_info_hk[1] || gethk_time_info_hk[2] < time
       && (hxdscl_tbegin_hk<=time && time<=hxdscl_tend_hk)){

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
		 return; */
	    }  else {
		fprintf(stderr, "%s_ana: get HXD_AE_TM_LATCH_TM faild: Initial mode. continue.\n", pname);
		istat = ANL_OK; 
	    }
	} else {
	    BnkfPutM( "HXDfsclTime:HXD_AE_TM_LATCH_TM", sizeof(int), 
		      &hxd_ae_tm_latch_tm );
	}
	
	istat = aste_gethk ( hk_id[HXD_WPU_CLK_RATE], time, TINT, 1, 
			     &hxd_wpu_clk_rate, gethk_time_info_hk );
	if( istat == ANL_NG ){
	    if(time > gethk_time_info_hk[2]){
		fprintf(stderr, "%s_ana: get HXD_WPU_CLK_RATE faild: time over range.\n", pname);
		/* *status = ANL_QUIT;
		   return; */
	    }  else {
		fprintf(stderr, "%s_ana: get HXD_WPU_CLK_RATE faild: Initial mode. continue.\n", pname);
		istat = ANL_OK; 
	    }
	} else {
	    BnkfPutM( "HXDfsclTime:HXD_WPU_CLK_RATE", sizeof(int),
		      &hxd_wpu_clk_rate );        
	}
    
	if (istat == ANL_OK){
	    update |= HXD_UPDATE_HK;
	    BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update);
	    BnkfPutM( "HXDfsclTime:HXD_HK_TIME" , sizeof(double),
		      &gethk_time_info_hk[0] );           
	}
    }
    
    /*************** SCL Extension ***************/
    if(time < gethk_time_info_sys[1] || gethk_time_info_sys[2] < time
       && (hxdscl_tbegin_sys<=time && time<=hxdscl_tend_sys)){

	istat = aste_gethk(hk_id[HXD_SYS_LATCH_TI], time, TINT, 1, 
			   &hxd_tlatch_time, gethk_time_info_sys);
	if ( istat == ANL_NG ){
	    if(time > gethk_time_info_sys[2]){
		fprintf(stderr, "%s_ana: get HXD_TLATCH_TIME faild: time over range. \n", pname);
		/*
		*status = ANL_QUIT;
		return; */
	    }  else {
		fprintf(stderr, "%s_ana: get HXD_TLATCH_TIME faild: Initial mode. continue.\n", pname);
		istat = ANL_OK; 
	    }
	} else {
	    BnkfPutM( "HXDfsclTime:HXD_SYS_LATCH_TI", sizeof(int),
		      &hxd_tlatch_time );
	}

	if (istat == ANL_OK) {
	    update |= HXD_UPDATE_SYS;
	    BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update);
	    BnkfPutM( "HXDfsclTime:HXD_SYS_TIME" , sizeof(double),
		      &gethk_time_info_sys[0] );
	}
    }
    
    *status = ANL_OK;   
}

void
HXDfsclTimeFITS_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDfsclTimeFITS_exit(int *status)
{
    *status = ANL_OK;
}
