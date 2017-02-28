/*
   HXDbstFitsRead
     v0.0.1 1999.10.13 created Y.Terada
     v0.0.4 2003.04.14 modefied by Y.Terada
            For HXD-II format (hxdbstFitsUtil v0.1.0).
     v0.0.5 2003.05.03 modefied by Y.Terada
     v0.1.0 2004.03.10 modefied by Y.Terada, write credits
     v0.1.0 2004.03.11 modefied by Y.Terada, debug
     v0.1.2 2004.03.12 modefied by Y.Terada, for hxdFitsHeaderUtil v1.3.3
     v0.1.3 2004.03.12 modefied by Y.Terada, add ETI
     v0.1.4 2004.03.13 modefied by Y.Terada, credits
     v0.2.0 2005.05.23 modefied by Y.Terada, delete ETI
     v0.2.1 2005.06.03 modefied by Y.Terada, support TIMEDEL
     v0.2.2 2005.11.02 modefied by Y.Terada, memory allocation in Credits
     v0.2.3 2005.11.04 modefied by Y.Terada, put pIL value in header
     v0.2.4 2005.11.05 modefied by Y.Terada, delete strndup.
     v0.2.5 2005.11.11 modefied by Y.Terada, delete needless Bnk access
     ---> v1.0.0
     v2.0.0 2006.09.15 modefied by Y.Terada, for v2 format
     v2.0.1 2008.07.29 modefied by Y.Terada, update TIMECORR
     v2.0.2 2008.10.23 modefied by Y.Terada, delete Credits for temporal
     v2.0.3 2008.10.25 modefied by Y.Terada, temporal 2
     v2.0.4 2008.10.30 modefied by Y.Terada, bring back Credits.
     v2.0.5 2013.10.16 modefied by Y.Terada, value for return
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "aste_coord.h"
#include "aste_rand.h"
#include "fitsio.h"
#include "cfortran.h"
#include "hbook.h"
#include "hxd/HXD.h"

#include "hxdbstFitsUtil.h"
#include "hxdbstFitsToBnkUtil.h"
#include "hxdFitsHeaderUtil.h"
#include "hxdeventFitsUtil.h"

#define FILENAME_MAX_LENGTH 256
#define HXD_BST_MAX_ENE_BIN  54

char HXD2ndbstFitsWrite_version[] = "version 2.0.5";

static char pname[] = "HXD2ndbstFitsWrite";

enum{
  HXD_READONLY,
  HXD_OVERWRITE,
  HXD_CREATE
};

static struct {
  fitsfile *fp;
  long irow;
  long nrow;
  int iomode;
  fitsfile *new_fp;
  char newfilename[FILENAME_MAX_LENGTH];
  int new_colnum[HXD_BST_FITS_KEY_NUM];
  long new_irow;
}com;

static int createFITS(){

  int istat = 0;
    
  int hdutype;
  int morekey = 5;
  
  int hdunum;
  int form_ver, size;
  
  fits_create_file(&com.new_fp, com.newfilename, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_create_file('%s') failed (%d)\n",
	    pname, com.newfilename, istat);
    exit(-1);
  } else {
    fits_get_hdu_num(com.fp, &hdunum);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_hdu_num failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {    
    fits_movabs_hdu(com.fp, HXD_BST_FITS_PRIMARY_HDU, &hdutype, &istat);
  }
  if ( istat ) {        
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_copy_hdu(com.fp, com.new_fp, morekey, &istat);
  }
  if ( istat ) {        
    fprintf(stderr, "%s: fits_copy_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_movabs_hdu(com.fp, HXD_BST_FITS_EVENT_EXTENTION, &hdutype, &istat);
  }
  if ( istat ) {        
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_copy_header(com.fp, com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_copy_header failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    BnkGet("HXD:BST:format_version", sizeof(int), &size, &form_ver);
    hxdbstFits_col_num( com.new_fp, com.new_colnum, form_ver, &istat );
  }
  if( istat ){
    return ANL_NG;
  } else {
    fits_movabs_hdu(com.new_fp, hdunum, &hdutype, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  }
  
  return ANL_OK;
  
}

static int closeFITS(){
    
  int istat = 0;
  int hdutype;
  
/*int hdunum;*/

  char taskname[FITS_KEY_MAX_LENGTH];
  char taskver[FITS_KEY_MAX_LENGTH];
  char credits[FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES];
  char *buf;

  fits_movabs_hdu(com.new_fp, HXD_BST_FITS_PRIMARY_HDU, &hdutype, &istat);
  if( istat ){
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } 
  /*  
  sprintf( taskname, "%s", 
	   strndup(anl_task_name()   , FITS_KEY_MAX_LENGTH));
  sprintf( taskver, "%s",  
	   strndup(anl_task_version(), FITS_KEY_MAX_LENGTH));
  sprintf( credits, "%s",  
	   strndup(anl_task_credits(), FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES));
  */
  buf = (char *)strdup(anl_task_name());
  if(strlen(buf) > FITS_KEY_MAX_LENGTH) buf[FITS_KEY_MAX_LENGTH] = '\0';
  sprintf( taskname, "%s", buf);

  buf = (char *)strdup(anl_task_version());
  if(strlen(buf) > FITS_KEY_MAX_LENGTH) buf[FITS_KEY_MAX_LENGTH] = '\0';
  sprintf( taskver, "%s", buf);

  buf = (char *)strdup(anl_task_credits());
  if(strlen(buf) > FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES) 
    buf[FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES] = '\0';
  sprintf( credits, "%s", buf);

  if(com.iomode == HXD_CREATE) {
    hxdFitsHeader_writeCredits(com.new_fp, taskname, taskver, credits,
                               HXD_FITS_HEADER_UTIL_CREDIT_DUPLICATE, &istat);
  } else {
    hxdFitsHeader_writeCredits(com.new_fp, taskname, taskver, credits,
                               HXD_FITS_HEADER_UTIL_CREDIT_OVERWRITE, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } 

  if (hxdFitsHeader_writeParamer_bst( com.new_fp, HXD_BST_FITS_PRIMARY_HDU)
      != HXD_FITS_HEADER_UTIL_OK){
    fprintf(stderr, "%s: error in writing parameters\n", pname);
    return ANL_NG;
  } else {
    fits_write_chksum(com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  
  fits_movabs_hdu(com.new_fp, HXD_BST_FITS_EVENT_EXTENTION, &hdutype, &istat);
  if( istat ){
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } 
  
  if(com.iomode == HXD_CREATE) {
    hxdFitsHeader_writeCredits(com.new_fp, taskname, taskver, credits,
                               HXD_FITS_HEADER_UTIL_CREDIT_DUPLICATE, &istat);
  } else {
    hxdFitsHeader_writeCredits(com.new_fp, taskname, taskver, credits,
                               HXD_FITS_HEADER_UTIL_CREDIT_OVERWRITE, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } 

  if (hxdFitsHeader_writeParamer_bst( com.new_fp, HXD_BST_FITS_EVENT_EXTENTION)
      != HXD_FITS_HEADER_UTIL_OK){
    fprintf(stderr, "%s: error in writing parameters\n", pname);
    return ANL_NG;
  } else {
    fits_write_chksum(com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
    return ANL_NG;
  }
  
  fits_close_file(com.new_fp, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_close_file ('%s') failed (%d)\n",
	    pname, com.newfilename, istat);
    return ANL_NG;
  }
  
  return ANL_OK;
  
}

void
HXD2ndbstFitsWrite_startup(int *status)
{
  *status = ANL_OK;
}

void
HXD2ndbstFitsWrite_init(int *status)
{  
  *status = ANL_OK;
}

void
HXD2ndbstFitsWrite_com(int *status)
{
  
  if ( *status ) { /* ftools */
    
    *status = 0;
    
    if ( *status ) {
      *status = ANL_QUIT;
      return;
    }
    
    *status = ANL_OK;
    
	return;
  }
  
  *status = ANL_OK;
}

void
HXD2ndbstFitsWrite_his(int *status)
{
  *status = ANL_OK;
}

void
HXD2ndbstFitsWrite_bgnrun(int *status)
{
  
  int used;
  
  BnkfGetM("HXDbstFitsRead:IOMODE", sizeof(int), &used,
	   &com.iomode);  
  BnkfGetM("HXDbstFitsRead:FILE_P", sizeof(com.fp), &used,
	   &com.fp);
  BnkfGetM("HXDbstFitsRead:NROW", sizeof(long), &used, &com.nrow);
  
  if(com.iomode == HXD_CREATE){
    
    BnkfGetM("HXDbstFitsRead:NEW_FILE_NAME",sizeof(com.newfilename),
	     &used, com.newfilename );
    
    if( createFITS() ){
      *status = ANL_QUIT;
      return;
    }
    
    com.new_irow = 0;
    
  }
  
  *status = ANL_OK;
  
}

void
HXD2ndbstFitsWrite_ana(int nevent, int eventid, int *status)
{
  
  int istat = 0;
  int used;    
  
  if( com.iomode == HXD_READONLY ){
    *status = ANL_OK;
    return;
  }
  
  if( com.iomode == HXD_OVERWRITE ){

    /*
       long firstelem = 1;
       long nelements = 1;
    
       BnkfGetM("HXDbstFitsRead:IROW", sizeof(long), &used, &com.irow);
    */
    
  } else if(com.iomode == HXD_CREATE){
    
/*  int i;     */
    HxdBstFits data;

    com.new_irow++;
    
    hxdbstFitsToBnk_get( &data );
    
    BnkfGetM("HXD:BST:PACKET_SEC_HEADER", sizeof(int), &used, &data.ti);
/*  BnkfGetM("HXD:BST:ETI", sizeof(int)*2, &used, data.eti); */
    
    hxdbstFits_col_write( com.new_fp, com.new_irow, com.new_colnum,
			 &data, &istat );
    
    if( istat ){
      *status = ANL_QUIT;
      return;
    }
    
  }
  
  *status = ANL_OK;
  
}

void
HXD2ndbstFitsWrite_endrun(int *status)
{

  int size;
  int i;
  int istat = 0;
  
  unsigned long bst_freezed_time_ct[4];
/*long bst_freezed_time_ct[4];*/
  long tpu_time_mode[4];
  double bst_freezed_time[4];
  int bst_freezed_time_ct_int[4];
  int tpu_time_mode_int[4];
  double timedel;
  int time_valid;
  int stat;

  int hxdbsttime_yes;
  int form_ver;

  BnkfGetM("HXD:ftools:hxdbsttime_yn", sizeof(int), &size, &hxdbsttime_yes);
  BnkGet("HXD:BST:format_version", sizeof(int), &size, &form_ver);

  if(hxdbsttime_yes) {
    BnkfGetM("HXD:BST:FRZD_TM", sizeof(double)*4, &size, bst_freezed_time);
    BnkfGetM("HXD:BST:FRTM_CT", sizeof(int)*4, &size, bst_freezed_time_ct_int);
    BnkfGetM("HXD:BST:TM_MODE", sizeof(int)*4, &size, tpu_time_mode_int);
    BnkfGetM("HXDfbstTime:TIME_RESOLUTION", sizeof(double), &size, &timedel);
    BnkfGetM("HXDfbstTime:TIME_CORR", sizeof(int), &size, &time_valid);

    for(i=0;i<4;i++){
      bst_freezed_time_ct[i] = (long)bst_freezed_time_ct_int[i];
      tpu_time_mode[i] = (long)tpu_time_mode_int[i];    
    }
    
    if( com.iomode == HXD_OVERWRITE ){    
      hxdbstFits_modify_key(com.fp, bst_freezed_time_ct,
			    bst_freezed_time, tpu_time_mode, form_ver,&istat );
      if(istat){
	*status = ANL_QUIT;
	return;
      }
      
      stat = hxdFitsHeader_writeTIMEDEL(com.fp, HXD_BST_FITS_PRIMARY_HDU, 
					timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_BST_FITS_PRIMARY_HDU);
	*status = ANL_QUIT;
	return;
      }
      
      stat = hxdFitsHeader_writeTIMEDEL(com.fp, HXD_BST_FITS_EVENT_EXTENTION,
					timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_BST_FITS_EVENT_EXTENTION);
	*status = ANL_QUIT;
	return;
      }
      

      hxdbstFits_modify_timecorr(com.fp, &time_valid, &istat);
      if (istat) {
	fprintf(stderr, "%s: update TIMECORR failed.", pname);
	*status = ANL_QUIT;
	return;
      }

      /* end of overwrite */
    } else if( com.iomode == HXD_CREATE ){
      hxdbstFits_modify_key(com.new_fp, bst_freezed_time_ct,
			    bst_freezed_time, tpu_time_mode, form_ver, 
			    &istat );
      if(istat){
	*status = ANL_QUIT;
	return;
      }
      
      stat = hxdFitsHeader_writeTIMEDEL(com.new_fp, HXD_BST_FITS_PRIMARY_HDU, 
					timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_BST_FITS_PRIMARY_HDU);
	*status = ANL_QUIT;
	return;
      }
      
      stat = hxdFitsHeader_writeTIMEDEL(com.new_fp,
					HXD_BST_FITS_EVENT_EXTENTION,
					timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_BST_FITS_EVENT_EXTENTION);
	*status = ANL_QUIT;
	return;
      }

      hxdbstFits_modify_timecorr(com.new_fp, &time_valid, &istat);
      if (istat) {
	fprintf(stderr, "%s: update TIMECORR failed.", pname);
	*status = ANL_QUIT;
	return;
      }

    } /* end of create */

  } /** end of hxdbsttime **/

    
  if(com.iomode == HXD_CREATE){
    if( closeFITS() ){
      *status = ANL_QUIT;
      return;
    }	
  }
  
  *status = ANL_OK;
  
}

void
HXD2ndbstFitsWrite_exit(int *status)
{
  *status = ANL_OK;
}
