/*
 *   HXDfbstTimeFITS v0.0.1 1999.10.13 Y.Terada
 *                   v0.1.0 2003-05-03 for HXD-II  Y.Terada
 *                   v0.2.0 2003-07-23 HEADAS   by Y.Terada 
 *                   v0.2.1 2004-03-13 modefied by Y.Terada 
 *                    not to quit, when time-before.
 *                   v0.2.4 2005-05-28 read timfile by R.Miyawaki
 *                   v0.2.7 2005-06-09 use S_TIME. debug for BNK name
 *                    new version of aste_gethk (v2.x) by R.Miyawaki
 *                   v0.3.2 2005-11-05          by Y.Terada
 *                    write PIL value in FFF header
 *                   v0.3.3 2005-11-08          by Y.Terada
 *                   --> v1.0.0
 *                   v2.0.0 2007-05-06          by Y.Terada
 *                   v2.0.3 2008-10-30          by Y.Terada
 *                    disable bstidt access when file is in CALDB
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
#include "aste_caldb.h"

#define GET_KEYWORD_NUM 3

#define FILELIST_MAX_LENGTH PIL_LINESIZE

char HXDfbstTimeFITS_version[] = "version 2.0.3";

static char pname[] = "HXDfbstTimeFITS";

static struct{
  char tim_filename[FILELIST_MAX_LENGTH];
  char bstidt_filename[FILELIST_MAX_LENGTH];
  char o_bstidt_filename[FILELIST_MAX_LENGTH];
  int  caldb_type_bstidt;
} com;

static ASTE_HK *aste_hk;
static int hk_id[GET_KEYWORD_NUM];

enum{
    HXD_SYS_LATCH_TI,
    HXD_AE_TM_LATCH_TM
};

void
HXDfbstTimeFITS_startup(int *status)
{
  BnkDef("HXD:PIL:tim_filename", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef("HXD:PIL:bstidt_filename", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef("HXD:PIL:CALDB_TYPE:bstidt",sizeof(int));
  BnkDef("HXD:PIL:access_caldb",sizeof(int));
  com.caldb_type_bstidt = 0;

    *status = ANL_OK;
}

void
HXDfbstTimeFITS_com(int *status)
{
  CALDB_INFO caldb;
  int string_size;
  char *k;
  int access_caldb = 0;
    
  if ( *status ) { /* ftools */
    
    *status = 0;
    
    /***************** tim file name ***********************/
    *status = PILGetFname(k="tim_filename", com.tim_filename);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetFname TIMFILE error (%d)\n", pname, *status);
      *status = ANL_QUIT;
      exit(-1);
    } else {
      string_size = strlen(com.tim_filename);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning tim_filename is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:tim_filename", string_size, com.tim_filename);
    }
    
    /***************** bstidt name ***********************/
    aste_caldb_init(&caldb);
    *status = PILGetFname(k="bstidt_fname", com.o_bstidt_filename);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetFname BSTIDT error (%d)\n", pname, *status);
      *status = ANL_QUIT;
      return; /*exit(-1);*/
    } else {
      /*-- caldb access --*/
      if (0==CLstricmp("CALDB", com.o_bstidt_filename)){
	com.caldb_type_bstidt = 1;
        caldb.telescop = "SUZAKU";        /* TELESCOP */
        caldb.instrume = "HXD";           /* INSTRUME */
        caldb.detnam   = "WAM_ANTI";      /* DETNAM   */
        caldb.codename = "BURST_ID_TABLE";  /* CCNM0001 */
        aste_caldb_get(&caldb);
        if (caldb.status != 0 || caldb.nfound == 0){
          anl_msg_error("%s: no CALDB entry for '%s-%s' (status=%d)\n",
                        pname, caldb.detnam, caldb.codename, caldb.status);
          *status = ANL_QUIT;
          return;
        }
        if (caldb.nfound != 1){
          anl_msg_warning("%s: multiple CALDB entry for '%s' (nfound=%d)\n",
                        pname, caldb.codename, caldb.nfound);
        }
        strcpy(com.bstidt_filename, caldb.filename);
	access_caldb = 1;
      } else{
        strcpy(com.bstidt_filename, com.o_bstidt_filename);
	access_caldb = 0;
      }

      /*-- Put filename --*/
      string_size = strlen(com.bstidt_filename);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning bstidt_filenam is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }

      BnkPut("HXD:PIL:bstidt_filename", string_size, com.bstidt_filename);
      BnkPut("HXD:PIL:CALDB_TYPE:bstidt",sizeof(int),&com.caldb_type_bstidt);
      BnkPut("HXD:PIL:access_caldb",sizeof(int), &access_caldb);
      
      *status = ANL_OK;
      return;
    }
  } /** anl **/
  com.tim_filename[0]='\0';
  
  CLtxtrd("input .tim FITS file name (HXDtimeSet)",
	  com.tim_filename,
	  sizeof(com.tim_filename));
  
  *status = ANL_OK;
  
}

void
HXDfbstTimeFITS_init(int *status)
{
    
  BnkDef( "HXDfbstTime:HXD_SYS_LATCH_TI" , sizeof(int) );
  /* "HXD:SYS:TLTIME" */
  BnkDef( "HXDfbstTime:HXD_AE_TM_LATCH_TM", sizeof(int) );
  /* "HXD:HKA:LATCH_TIM" */
  
  BnkDef( "HXDfbstTime:HXD_SYS_TIME", sizeof(double) );
  BnkDef( "HXDfbstTime:HXD_HK_TIME", sizeof(double) );
    
  BnkDef("HXDfbstTime:TIM_FILE_NAME", sizeof(char)*FILELIST_MAX_LENGTH);
  BnkDef("HXDfbstTime:BSTIDT_FILE_NAME", sizeof(char)*FILELIST_MAX_LENGTH);

  BnkfPutM("HXDfbstTime:TIM_FILE_NAME",
           sizeof(char)*FILELIST_MAX_LENGTH, com.tim_filename);
  BnkfPutM("HXDfbstTime:BSTIDT_FILE_NAME",
           sizeof(char)*FILELIST_MAX_LENGTH, com.bstidt_filename);

  *status = ANL_OK;
  
}

void
HXDfbstTimeFITS_his(int *status)
{
    *status = ANL_OK;
}

void
HXDfbstTimeFITS_bgnrun(int *status)
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
HXDfbstTimeFITS_ana(int nevent, int eventid, int *status)
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
  
  if( !(update & HXD_UPDATE_BST) ){
    *status = ANL_OK;
    return;
  }
  
  BnkfGetM("HXD:BST:PACKET_S_TIME", sizeof(double), &size, &time );
  
  /*************** SYS Extension **************/
  if(time < gethk_time_info_sys[1] || gethk_time_info_sys[2] < time){

      istat = aste_gethk(hk_id[HXD_SYS_LATCH_TI], time, TINT, 1, 
                         &hxd_tlatch_time, gethk_time_info_sys);
      if ( istat == ANL_NG ){
          if(time > gethk_time_info_sys[2]){
              fprintf(stderr, "%s_ana: get HXD_TLATCH_TIME faild: time over range. Quit\n", pname);
              *status = ANL_QUIT;
              return;
          }  else {
              fprintf(stderr, "%s_ana: get HXD_TLATCH_TIME faild: Initial mode. continue.\n", pname);
              istat = ANL_OK; 
          }
      } else {
          BnkfPutM( "HXDfbstTime:HXD_SYS_LATCH_TI", sizeof(int),
                    &hxd_tlatch_time );
      }

      if (istat == ANL_OK) {
          update |= HXD_UPDATE_SYS;
          BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update);
          BnkfPutM( "HXDfbstTime:HXD_SYS_TIME" , sizeof(double),
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
              fprintf(stderr, "%s_ana: get HXD_AE_TM_LATCH_TM faild: time over range. Quit\n", pname);
              *status = ANL_QUIT;
              return;
          }  else {
              fprintf(stderr, "%s_ana: get HXD_AE_TM_LATCH_TM faild: Initial mode. continue.\n", pname);
              istat = ANL_OK; 
          }
      } else {
          BnkfPutM( "HXDfbstTime:HXD_AE_TM_LATCH_TM", sizeof(int), 
                    &hxd_ae_tm_latch_tm );
      }

      if (istat == ANL_OK){
          update |= HXD_UPDATE_HK;
          BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update);
          BnkfPutM( "HXDfbstTime:HXD_HK_TIME" , sizeof(double),
                    &gethk_time_info_hk[0] );           
      }
  }

    *status = ANL_OK;

}

void
HXDfbstTimeFITS_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDfbstTimeFITS_exit(int *status)
{
    *status = ANL_OK;
}
