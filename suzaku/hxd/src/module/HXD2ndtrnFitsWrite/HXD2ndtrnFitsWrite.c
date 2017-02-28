/*
   HXDeventFitsRead
     v0.0.1 created
     v0.0.2
     v0.0.3 see update
     v0.1.0 for HXD-II 2003-05-03 Y.Terada
     v0.2.0 Write credits 2004-03-10 Y.Terada
     v0.2.1 2004-03-11 H.Taka Y.Terada, debug and skip delete_row..
     v0.2.2 2004-03-12 Y.Terada, for hxdFitsHeaderUtil v1.3.3
     v0.2.3 2004-03-12 Y.Terada, add ETI
     v0.2.3 2005-05-23 Y.Terada, delete ETI
     v0.2.5 2005-06-03 Y.Terada, add TIMEDEL
     v0.2.6 2005-06-13 Y.Terada, write TRNTIME unit.
     v0.2.7 2005-11-02 Y.Terada, memory allocate in Credits
     v0.2.8 2005-11-04 Y.Terada, put PIL value
     v0.2.9 2005-11-05 Y.Terada, delete strndup
     v0.3.0 2005-11-09 Y.Terada, delete strndup
     v0.3.1 2005-11-11 Y.Terada, delete needless Bnk access
     --> v1.0.0
     v2.0.0 2007-05-10 Y.Terada, at CA, debug for create mode, TIMEDEL
     v2.0.1 2013-10-16 Y.Terada, add return value
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

#include "hxdtrnFitsUtil.h"
#include "hxdtrnFitsToBnkUtil.h"
#include "hxdFitsHeaderUtil.h"

#define FILENAME_MAX_LENGTH 256

char HXD2ndtrnFitsWrite_version[] = "version 2.0.1";

static char pname[] = "HXD2ndtrnFitsWrite";

enum{
  HXD_READONLY,
  HXD_OVERWRITE,
  HXD_CREATE
};

static struct {
  fitsfile *fp;
  long irow;
  long nrow;
  int iomode;  int gtimode;
  int time_colnum;  int pi_colnum;  int quality_colnum;
  int time_change;  int pi_change;  int quality_change;
  fitsfile *new_fp;
  char newfilename[FILENAME_MAX_LENGTH];
  int new_colnum[HXD_TRN_FITS_KEY_NUM];
  long new_irow;
}com;

static int createFITS(){

  int istat = 0;
    
  int hdutype;
  int morekey = 5;
  
  int hdunum;
  
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
    fits_movabs_hdu(com.fp, HXD_TRN_FITS_PRIMARY_HDU, &hdutype, &istat);
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
    fits_movabs_hdu(com.fp, HXD_TRN_FITS_EVENT_EXTENTION, &hdutype, &istat);
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
    hxdtrnFits_col_num( com.new_fp, com.new_colnum, &istat );
  }
  if( istat ){
    return ANL_NG;
  } else {
    fits_movabs_hdu(com.fp, HXD_TRN_FITS_GTI_EXTENTION, &hdutype, &istat);
  }
  if ( istat ) {        
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {    
    fits_copy_hdu(com.fp, com.new_fp, morekey, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_copy_header failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {	
    fits_movabs_hdu(com.fp, hdunum , &hdutype, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {	
    fits_movabs_hdu(com.new_fp, HXD_TRN_FITS_EVENT_EXTENTION,
		    &hdutype, &istat);
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
  
/*int hdunum; */
  char taskname[FITS_KEY_MAX_LENGTH];
  char taskver[FITS_KEY_MAX_LENGTH];
  char credits[FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES];

  /** 1st extension **/
  fits_movabs_hdu(com.new_fp, HXD_TRN_FITS_PRIMARY_HDU, &hdutype, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } /* else {
    fits_write_date(com.new_fp, &istat);
  }  
  if ( istat ) {
    fprintf(stderr,"%s: fits_write_date() failed (%d)\n", pname, istat);
    return ANL_NG;
  } */ else {
    /** ------------ write credits into HISTORY ------------- **/
    char *buf;
    /*
    sprintf( taskname, "%s", 
	     strndup(anl_task_name()   , FITS_KEY_MAX_LENGTH));
    sprintf( taskver, "%s",  
	     strndup(anl_task_version(), FITS_KEY_MAX_LENGTH));
    sprintf( credits, "%s",  
	     strndup(anl_task_credits(), 
		     FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES));
    */
    buf = (char *)strdup(anl_task_name());
    if(strlen(buf) > FITS_KEY_MAX_LENGTH) buf[FITS_KEY_MAX_LENGTH] = '\0';
    sprintf( taskname, "%s", buf);
    
    buf = (char *)strdup(anl_task_version());
    if(strlen(buf) > FITS_KEY_MAX_LENGTH) buf[FITS_KEY_MAX_LENGTH] = '\0';
    sprintf( taskver, "%s", buf);
    
    buf = (char *)strdup(anl_task_credits());
    if(strlen(buf) > FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES) 
      buf[FITS_KEY_MAX_LENGTH] = '\0';
    sprintf( credits, "%s", buf);
    
    hxdFitsHeader_writeCredits(com.new_fp, taskname, taskver, credits, 
			       HXD_FITS_HEADER_UTIL_CREDIT_DUPLICATE, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (primary: %d)\n",
	    pname, istat);
    return ANL_NG;
  } 
  if (hxdFitsHeader_writeParamer_trn( com.new_fp, HXD_TRN_FITS_PRIMARY_HDU)
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

  /** 2nd extension **/
  fits_movabs_hdu(com.new_fp, HXD_TRN_FITS_EVENT_EXTENTION, &hdutype, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } /* else {
    fits_write_date(com.new_fp, &istat);
  }  
  if ( istat ) {
    fprintf(stderr,"%s: fits_write_date() failed (%d)\n", pname, istat);
    return ANL_NG;
  } */ /* else {
    fits_delete_rows(com.new_fp, com.new_irow+1,
		     com.nrow - com.new_irow, &istat );
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_delete_rows failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } */ else {/** write credits into HISTORY **/
    char *buf;
    /*
    sprintf( taskname, "%s", 
	     strndup(anl_task_name()   , FITS_KEY_MAX_LENGTH));
    sprintf( taskver, "%s",  
	     strndup(anl_task_version(), FITS_KEY_MAX_LENGTH));
    sprintf( credits, "%s",  
	     strndup(anl_task_credits(), 
		     FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES));
    */
    buf = (char *)strdup(anl_task_name());
    if(strlen(buf) > FITS_KEY_MAX_LENGTH) buf[FITS_KEY_MAX_LENGTH] = '\0';
    sprintf( taskname, "%s", buf);
    
    buf = (char *)strdup(anl_task_version());
    if(strlen(buf) > FITS_KEY_MAX_LENGTH) buf[FITS_KEY_MAX_LENGTH] = '\0';
    sprintf( taskver, "%s", buf);
    
    buf = (char *)strdup(anl_task_credits());
    if(strlen(buf) > FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES) 
      buf[FITS_KEY_MAX_LENGTH] = '\0';
    sprintf( credits, "%s", buf);

    hxdFitsHeader_writeCredits(com.new_fp, taskname, taskver, credits,
			       HXD_FITS_HEADER_UTIL_CREDIT_DUPLICATE, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (secondary: %d)\n",
	    pname, istat);
    return ANL_NG;
  } 
  if (hxdFitsHeader_writeParamer_trn( com.new_fp, HXD_TRN_FITS_EVENT_EXTENTION)
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
  
  /** 3rd extension **/
  fits_movabs_hdu(com.new_fp, HXD_TRN_FITS_GTI_EXTENTION, &hdutype, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } /* else {
    fits_write_date(com.new_fp, &istat);
  }  
  if ( istat ) {
    fprintf(stderr,"%s: fits_write_date() failed (%d)\n", pname, istat);
    return ANL_NG;
  } */ else {
    char *buf;
    /*
    sprintf( taskname, "%s", 
	     strndup(anl_task_name()   , FITS_KEY_MAX_LENGTH));
    sprintf( taskver, "%s",  
	     strndup(anl_task_version(), FITS_KEY_MAX_LENGTH));
    sprintf( credits, "%s",  
	     strndup(anl_task_credits(), 
		     FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES));
    */

    buf = (char *)strdup(anl_task_name());
    if(strlen(buf) > FITS_KEY_MAX_LENGTH) buf[FITS_KEY_MAX_LENGTH] = '\0';
    sprintf( taskname, "%s", buf);
    
    buf = (char *)strdup(anl_task_version());
    if(strlen(buf) > FITS_KEY_MAX_LENGTH) buf[FITS_KEY_MAX_LENGTH] = '\0';
    sprintf( taskver, "%s", buf);
    
    buf = (char *)strdup(anl_task_credits());
    if(strlen(buf) > FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES) 
      buf[FITS_KEY_MAX_LENGTH] = '\0';
    sprintf( credits, "%s", buf);

    hxdFitsHeader_writeCredits(com.new_fp, taskname, taskver, credits,
			       HXD_FITS_HEADER_UTIL_CREDIT_DUPLICATE, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (gti: %d)\n", 
	    pname, istat);
    return ANL_NG;
  } 
  if (hxdFitsHeader_writeParamer_trn( com.new_fp, HXD_TRN_FITS_GTI_EXTENTION)
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

  /** close **/
  fits_close_file(com.new_fp, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_close_file ('%s') failed (%d)\n",
	    pname, com.newfilename, istat);
    return ANL_NG;
  }

  return ANL_OK;
  
}

static int get_colnum(){
  
  int istat = 0;
  int casesen = TRUE;
  
  fits_get_colnum( com.fp, casesen, "TIME", &com.time_colnum, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('TIME') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "TRN_PI",
		    &com.pi_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('TRN_PI') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "TRN_QUALITY",
		    &com.quality_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('TRN_QUALITY') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  }
  
  return ANL_OK;
  
}


void
HXD2ndtrnFitsWrite_startup(int *status)
{
  *status = ANL_OK;
}

void
HXD2ndtrnFitsWrite_init(int *status)
{  
  *status = ANL_OK;
}

void
HXD2ndtrnFitsWrite_com(int *status)
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
HXD2ndtrnFitsWrite_his(int *status)
{
  *status = ANL_OK;
}

void
HXD2ndtrnFitsWrite_bgnrun(int *status)
{
  
  int used;
  
  BnkfGetM("HXDtrnFitsRead:IOMODE", sizeof(int), &used,
	   &com.iomode);
  BnkfGetM("HXDtrnFitsRead:GTIMODE", sizeof(int), &used,
	   &com.gtimode);
  
  BnkfGetM("HXDtrnFitsRead:EV_TIME:CHANGE", sizeof(int), &used,
	   &com.time_change );
  BnkfGetM("HXDtrnFitsRead:PI:CHANGE", sizeof(int), &used,
	   &com.pi_change );
  BnkfGetM("HXDtrnFitsRead:QUALITY:CHANGE", sizeof(int), &used,
	   &com.quality_change );
  
  BnkfGetM("HXDtrnFitsRead:FILE_P", sizeof(com.fp), &used,
	   &com.fp);
  BnkfGetM("HXDtrnFitsRead:NROW", sizeof(long), &used, &com.nrow);
  
  if( get_colnum() ){
    *status = ANL_QUIT;
    return;
  }
  
  else if(com.iomode == HXD_CREATE){
    
    BnkfGetM("HXDtrnFitsRead:NEW_FILE_NAME",sizeof(com.newfilename),
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
HXD2ndtrnFitsWrite_ana(int nevent, int eventid, int *status)
{
  
  int istat = 0;
  int used;    
  
  if( com.iomode == HXD_READONLY ){
    *status = ANL_OK;
    return;
  }
  
  if( com.iomode == HXD_OVERWRITE ){
    
    long firstelem = 1;
    long nelements = 1;
    
    BnkfGetM("HXDtrnFitsRead:IROW", sizeof(long), &used, &com.irow);
    
    if( com.time_change == TRUE ){
      
      double time;
      
      BnkfGetM("HXD:TRN:EV_TIME", sizeof(double), &used, &time);
      
      fits_write_col_dbl(com.fp, com.time_colnum, com.irow,
			 firstelem, nelements, &time, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('TIME') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }
    }
    
    if( com.pi_change == TRUE ){

      int i;
      int trn_pi[HXD_TRN_MAX_ENE_BIN];
      unsigned short usht_trn_pi[HXD_TRN_MAX_ENE_BIN];
      
      BnkfGetM("HXD:TRB:PI", sizeof(int)*HXD_TRN_MAX_ENE_BIN, &used, trn_pi );
      
      for(i=0;i<HXD_TRN_MAX_ENE_BIN;i++){
	usht_trn_pi[i] = (unsigned short) trn_pi[i];
      }

      fits_write_col_usht(com.fp, com.pi_colnum, com.irow,
			  firstelem, HXD_TRN_MAX_ENE_BIN, usht_trn_pi, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('TIME') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }
      
    }
    
    if( com.quality_change == TRUE ){

      int trn_quality;
      unsigned short trn_quality_shr;
      
      BnkfGetM("HXD:TRN:TRN_QUALITY", sizeof(int), &used, &trn_quality );
      
      trn_quality_shr = (unsigned short) trn_quality;

      fits_write_col_usht(com.fp, com.quality_colnum, com.irow,
			  firstelem, nelements,
			  &trn_quality_shr, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('TRN_QUALITY') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }
      
    }
    
  }else if(com.iomode == HXD_CREATE){
    
    int i;    
    HxdTrnFits data;
    int trn_pi[HXD_TRN_MAX_ENE_BIN];
    int trn_quality;

    int update;
    
    BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &used, &update );
    
    if( !(update & HXD_UPDATE_TRN) ){
      *status = ANL_OK;
      return;
    }
    
    com.new_irow++;
    
    hxdtrnFitsToBnk_get( &data );
    
    BnkfGetM("HXD:TRN:PACKET_SEC_HEADER", sizeof(int), &used, &data.ti);
    BnkfGetM("HXD:TRN:EV_TIME", sizeof(double), &used, &data.time);
 /* BnkfGetM("HXD:TRN:ETI", sizeof(int)*2, &used, data.eti); */
    BnkfGetM("HXD:TRB:PI", sizeof(int)*HXD_TRN_MAX_ENE_BIN, &used, trn_pi );
    BnkfGetM("HXD:TRN:TRN_QUALITY", sizeof(int), &used, &trn_quality );
    
    for(i=0;i<HXD_TRN_MAX_ENE_BIN;i++){
      data.trn_pi[i] = (unsigned short) trn_pi[i];
    }
    data.trn_quality = (unsigned short) trn_quality;
    
    hxdtrnFits_col_write( com.new_fp, com.new_irow, com.new_colnum,
			 &data, &istat );
    
    if( istat ){
      *status = ANL_QUIT;
      return;
    }
    
  }
  
  *status = ANL_OK;
  
}

void
HXD2ndtrnFitsWrite_endrun(int *status)
{
  double timedel;
  int  time_mode;
  int used;    
  int stat = 0;
  int hxdwamtime_yn;


  if(com.iomode == HXD_CREATE){ 

    BnkfGetM("HXD:ftools:hxdwamtime_yn", sizeof(int), &used, &hxdwamtime_yn);
    
    if (hxdwamtime_yn){
/*  if( com.time_change == TRUE ){*/
      BnkfGetM("HXDftrnTime:TIME_MODE", sizeof(int), &used, &time_mode);
      hxdtrnFits_modify_trntime_unit (com.new_fp, time_mode, &stat);
      if(stat){
	fprintf(stderr, "%s: update TRNTIME unit failed (%d)\n", pname, stat);
	*status = ANL_QUIT;
	return;
      }
      
      
      BnkfGetM("HXDftrnTime:TIME_RESOLUTION", sizeof(double), &used, &timedel);
      stat = hxdFitsHeader_writeTIMEDEL(com.new_fp, HXD_TRN_FITS_PRIMARY_HDU,
					timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_TRN_FITS_PRIMARY_HDU);
	*status = ANL_QUIT;
	return;
      }
      
      stat = hxdFitsHeader_writeTIMEDEL(com.new_fp, 
					HXD_TRN_FITS_EVENT_EXTENTION,
					timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_TRN_FITS_EVENT_EXTENTION);
	*status = ANL_QUIT;
	return;
      }
      
      stat =  hxdFitsHeader_writeTIMEDEL(com.new_fp, 
					 HXD_TRN_FITS_GTI_EXTENTION,
					 timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_TRN_FITS_GTI_EXTENTION);
	*status = ANL_QUIT;
	return;
      }
    }

    if( closeFITS() ){
      *status = ANL_QUIT;
      return;
    }
  } else if( com.iomode == HXD_OVERWRITE ){
    
    if( com.time_change == TRUE ){
      BnkfGetM("HXDftrnTime:TIME_MODE", sizeof(int), &used, &time_mode);
      hxdtrnFits_modify_trntime_unit (com.fp, time_mode, &stat);
      if(stat){
	fprintf(stderr, "%s: update TRNTIME unit failed (%d)\n", pname, stat);
	*status = ANL_QUIT;
	return;
      }

      BnkfGetM("HXDftrnTime:TIME_RESOLUTION", sizeof(double), &used, &timedel);
      stat = hxdFitsHeader_writeTIMEDEL(com.fp, HXD_TRN_FITS_PRIMARY_HDU,
                                        timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
        fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
                HXD_TRN_FITS_PRIMARY_HDU);
        *status = ANL_QUIT;
        return;
      }
      
      stat = hxdFitsHeader_writeTIMEDEL(com.fp, 
                                        HXD_TRN_FITS_EVENT_EXTENTION,
                                        timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
        fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
                HXD_TRN_FITS_EVENT_EXTENTION);
        *status = ANL_QUIT;
        return;
      }
      
      stat =  hxdFitsHeader_writeTIMEDEL(com.fp, 
                                         HXD_TRN_FITS_GTI_EXTENTION,
                                         timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
        fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
                HXD_TRN_FITS_GTI_EXTENTION);
        *status = ANL_QUIT;
        return;
      }
      
      /** closeFITS() in HXDeventFitsRead, exit(); **/
      
    }
  } 
  
  *status = ANL_OK;
  
}

void
HXD2ndtrnFitsWrite_exit(int *status)
{
  *status = ANL_OK;
}
