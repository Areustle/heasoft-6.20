/*
   HXD2ndsclFitsWrite:
   
   v0.0.1 created by Y.Terada 1999-12-13
          *  Only for scl time assignment. (Fill TIME)
          *  Only OVERWRITE Mode is supported.
             (READONLY, CREATE Mode is not supported.)
   v0.0.2 modefied by Y.Terada 1999-12-23
          delete fits close. (colse at HXDsclFitsRead.c)
   v0.1.0 modefied by Y.Terada 2004-03-10
          write credits
   v0.1.1 modefied by Y.Terada 2004-03-11 debug
   v0.1.2 modefied by Y.Terada 2004-03-12 for hxdFitsHeaderUtil v1.3.3
   v0.2.0 modefied by Y.Tearda 2005-06-03, add TIMEDEL
   v0.2.1 modefied by Y.Tearda 2005-06-03, delete needless Bnk access
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

#include "hxdFitsHeaderUtil.h"
#include "HXD2ndsclFitsWrite.h"

#include "aste_gethk.h"
#define DEBUG 0

#define FILENAME_MAX_LENGTH 256

char HXD2ndsclFitsWrite_version[] = "version 0.2.1";

static char pname[] = "HXD2ndsclFitsWrite";

static struct {
	fitsfile *fp;
/*	int gtimode;*/
	int time_colnum;
	long irow;
	long nrow;
} com;

void
HXD2ndsclFitsWrite_startup(int *status){  *status = ANL_OK; }

void
HXD2ndsclFitsWrite_init(int *status){ *status = ANL_OK; }

void
HXD2ndsclFitsWrite_com(int *status) {
  if ( *status ) { /* ftools */
    *status = 0;
    if ( *status ) {
      *status = ANL_QUIT;
      return;
    }
    *status = ANL_OK;
	return;
  }
  /* ANL-QL */
  *status = ANL_OK;
}

void
HXD2ndsclFitsWrite_his(int *status){  *status = ANL_OK; }

void
HXD2ndsclFitsWrite_bgnrun(int *status) {
  int used;
  int istat = 0;
  int casesen = TRUE;
    
/*BnkfGetM("HXDsclFitsRead:GTIMODE", sizeof(int),    &used, &com.gtimode);*/
  BnkfGetM("HXDsclFitsRead:FILE_P",  sizeof(com.fp), &used, &com.fp     );
  BnkfGetM("HXDsclFitsRead:NROW",    sizeof(long),   &used, &com.nrow   );
  
  fits_get_colnum( com.fp, casesen, "TIME", &com.time_colnum, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('TIME') failed (%d)\n",
	    pname, istat);
    *status = ANL_QUIT;
    return ;
  }
 
  *status = ANL_OK;
}

void
HXD2ndsclFitsWrite_ana(int nevent, int eventid, int *status){
  int istat = 0;
  int used;    
  
  double time;
  long firstelem = 1;
  long nelements = 1;
  
  BnkfGetM("HXDsclFitsRead:IROW", sizeof(long),   &used, &com.irow);
  BnkfGetM("HXD:SCL:EV_TIME",     sizeof(double), &used, &time    );
  if (DEBUG) fprintf(stdout, "%s_ana: irow=%d, time=%f\n",  pname, com.irow, time);
  
  fits_write_col_dbl(com.fp, com.time_colnum, com.irow,
					 firstelem, nelements, &time, &istat);
  if ( istat ) {
	  fprintf(stderr,"%s: fits_write_col('TIME') failed (%d)\n", pname, istat);
	  *status = ANL_QUIT;
	  return;
  }
  if (DEBUG) fprintf(stdout, "%s_ana: write TIME...\n",  pname);

  *status = ANL_OK;
}

void
HXD2ndsclFitsWrite_endrun(int *status){
  /** in HXDsclFitsRead **/
  /*
  int istat = 0;
  hxdFitsHeader_writeCredits(com.fp, anl_task_name(), 
                             anl_task_version(), anl_task_credits(), 
			     HXD_FITS_HEADER_UTIL_CREDIT_OVERWRITE, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (%d)\n",
	    pname, istat);
    *status = ANL_NG;
    return;
  }
  */

  double scl_timedel;
  double timedel;
  int used;    
  int stat;

  int hxdscltime_yes;

  BnkfGetM( "HXD:ftools:hxdscltime_yn",  sizeof(int), &used, &hxdscltime_yes);

  if (hxdscltime_yes) {
    BnkfGetM("HXDfsclTime:TIME_RESOLUTION", sizeof(double), 
	     &used, &scl_timedel);
    stat = hxdFitsHeader_writeTIMEDEL(com.fp, HXD_HK_FITS_SCL_EXTENSTION,
				      scl_timedel);
    if (stat != HXD_FITS_HEADER_UTIL_OK) {
      fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
	      HXD_HK_FITS_SCL_EXTENSTION);
      *status = ANL_QUIT;
      return;
    }
  
    /** change TIMEDEL in other extensions **/
    timedel = (1.0/4096.0);
    
    stat = hxdFitsHeader_writeTIMEDEL(com.fp, HXD_HK_FITS_PRIMARY_HDU,
				      timedel);
    if (stat != HXD_FITS_HEADER_UTIL_OK) {
      fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
	      HXD_HK_FITS_PRIMARY_HDU);
      *status = ANL_QUIT;
      return;
    }

    stat = hxdFitsHeader_writeTIMEDEL(com.fp, HXD_HK_FITS_HK_EXTENSTION,
				      timedel);
    if (stat != HXD_FITS_HEADER_UTIL_OK) {
      fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
	      HXD_HK_FITS_HK_EXTENSTION);
      *status = ANL_QUIT;
      return;
    }
    
    stat = hxdFitsHeader_writeTIMEDEL(com.fp, HXD_HK_FITS_SYS_EXTENSTION,
				      timedel);
    if (stat != HXD_FITS_HEADER_UTIL_OK) {
      fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
	      HXD_HK_FITS_SYS_EXTENSTION);
      *status = ANL_QUIT;
      return;
    }

    stat = hxdFitsHeader_writeTIMEDEL(com.fp, HXD_HK_FITS_ACU_EXTENSTION,
				      timedel);
    if (stat != HXD_FITS_HEADER_UTIL_OK) {
      fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
	      HXD_HK_FITS_ACU_EXTENSTION);
      *status = ANL_QUIT;
      return;
    }

    stat = hxdFitsHeader_writeTIMEDEL(com.fp, HXD_HK_FITS_PWH_EXTENSTION,
				      timedel);
    if (stat != HXD_FITS_HEADER_UTIL_OK) {
      fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
	      HXD_HK_FITS_PWH_EXTENSTION);
      *status = ANL_QUIT;
      return;
    }
  } /** end of hxdscltime **/

  *status = ANL_OK;
  return;
}

void
HXD2ndsclFitsWrite_exit(int *status){
/*    int istat = 0;
    
    fits_close_file(com.fp, &istat);
    if ( istat ) {
	fprintf(stderr, "%s: fits_close_file failed (%d)\n", pname, istat);
	*status = ANL_QUIT;
	return;
    }
*/
    *status = ANL_OK;
}
/*********************** EOF *************************************/

