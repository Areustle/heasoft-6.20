/*
   HXDeventFitsRead

   v0.0.1 test version created by M.Sugiho
   v0.0.2 for HXDeventFitsRead v0.0.6
   v0.0.3 for HXDeventFitsRead v0.0.7
   v0.0.4 for HXDeventFitsRead v0.0.9
   v0.0.5 for HXDeventFitsRead v0.1.2
   v0.0.6 for HXDeventFitsRead v0.1.3
   v0.0.7 for HXDeventFitsRead v0.1.5
   v0.0.8 include bug
   v0.0.9 for HXDeventFitsRead v0.1.7
   v0.1.0 for HXDeventFitsRead v0.2.0
          CREATE support ( not support clock_rate )
   v0.1.1 for HXDeventFitsRead v0.2.0
          corresponding to HXDeventFitsWrite 0.6.6
	                   hxdeventFitsUtil 0.0.2
	  temporaly wpu_clock_rate = 0;
   v0.1.2 corresponding to HXDeventFitsRread 0.2.1
   v0.1.3 aste_gethk.h
          pi_slow debug!!
	  LNGTH_OK => LENGTH_CHK corresponding to HXDeventFitsRread 0.2.2
   v0.1.4 hxd_eventFits -> hxdeventFits : hxdeventFitsUtil v0.0.4
   v0.1.5 for hxdeventFitsUtil 0.0.5
   v0.1.6 delete Evs
   v0.1.7 chksum
   v0.1.8 see update
   v0.1.9 1999-10-31 change include "", for ftools-release     by Y.Terada
   v0.2.0 1999-12-24 new_fp for CREATE_MODE (only add comment) by Y.Terada
   v0.2.1 2000-01-22 Fill DET_TYPE, PI_PIN                     by Y.Terada
   v0.2.2 2003-04-15 debug fits_delete_rows in close fits.     by Y.Terada
   v0.2.3 2003-05-02 add MTI in Time assignment          .     by Y.Terada
   v0.3.0 2004-03-10 add CREDITS for HISTORY                   by Y.Terada
   v0.3.1 2004-03-11 debug (skip delete_rows)      by Y.Terada,H.Takahashi
   v0.3.2 2004-03-12 closeFITS                     by Y.Terada
   v0.3.3 2004-03-12 add ETI                       by Y.Terada
   v0.3.4 2004-03-16 GRADE bit format              by Y.Terada,H.Takahashi
   v0.4.0 2005-01-25, new GRADE format (hxdeventFitsUtil v0.3.0)
                                                   by Y.Terada, T.Kisisita
   v0.4.1 2005-02-07, change data flow of DET_TYPE (just from hxdgrade)
                                                   by Y.Terada
   v0.4.2 2005-02-14, delete undefined rows,       by Y.Terada
   v0.4.3 2005-05-20, add PIN_ID                   by T.Kitaguchi
   v0.4.4 2005-05-26, double PI_**                 by Y.Terada
   v0.4.5 2005-05-26, debug PIN_ID                 by Y.Terada
   v0.4.6 2005-06-03, add TIMEDEL(time resolution) by Y.Terada
   v0.4.7 2005-06-11, change format GRADE_PSDSEL   by Y.Terada
   v0.4.8 2005-06-13                               by Y.Terada
   v0.4.9 2005-06-14  debug                        by Y.Terada
   v0.4.9 2005-09-11  Fill DET_TYPE when com.iomode = HXD_CREATE
                                                   by T.Kitaguchi
   v0.5.0 (same as 0.4.9)
   v0.5.1 2005-11-02  debug in writeCredits        by Y.Terada
   v0.5.2 2005-11-04  write PIL parameters         by Y.Terada
   v0.5.3 2005-11-05  delete stdndup               by Y.Terada
                      grade_quality (unsigned--> signed)
   v0.5.4 2005-11-08  type check of WRT parameters by Y.Terada
   v0.5.5 2005-11-11  delete needless Bnk access   by Y.Terada
   v0.5.6 2005-12-02  type check, change Q_FLAGS   by Y.Terada
   ---> v1.0.0
   v2.0.0 2006-09-10 Version 2.0 format                 by Y.Terada
   v2.0.1 2007-05-08 debug (comment out if statement about
                      time_change when iomode == HXD_CREATE)
                                                   by T.Kitaguchi
   v2.0.2 2007-05-08 overwrite NEVENTS keyword in 3rd ext.
                                                   by T.Kitaguchi
   v2.0.3 2007-05-09 check hxdtime_yn when write TIMEDEL keyword
                                                   by T.Kitaguchi
   v2.0.4 2007-09-27 not overwrite NEVENTS keyword in 3rd ext.
                     (since NEVENTS in 3rd ext. is removed after xelect)
                                                   by T.Kitaguchi
   v2.0.5 2009-03-02 write checksum                by Y.Terada
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
/*#include "pfile.h"*/
/* #include "uclpario.h" */

#include "aste_gethk.h"
#include "hxdeventFitsUtil.h"
#include "hxdFitsHeaderUtil_old.h"

#define FILENAME_MAX_LENGTH 256
#define DEBUG 0
#define DEBUG2 0
#define HISTORY_MAX_LENGTH  256

char HXD2ndeventFitsWrite_old_version[] = "version 2.0.4";

static char pname[] = "HXD2ndeventFitsWrite";

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
  int gtimode;
  int time_colnum;
  int mti_colnum;
  /* int grade_colnum; */
  int grade_qualty_colnum; /** new, v0.4.0 **/
  int grade_pmttrg_colnum; /** new, v0.4.0 **/
  int grade_pintrg_colnum; /** new, v0.4.0 **/
  int grade_psdsel_colnum; /** new, v0.4.0 **/
  int grade_hitpat_colnum; /** new, v0.4.0 **/
  int grade_reserv_colnum; /** new, v0.4.0 **/
  int det_type_colnum;
  int pin_id_colnum;       /** new, v0.4.3 **/
  int pi_fast_colnum;
  int pi_slow_colnum;
  int pi_pin_colnum;
  int pi_pin0_colnum;
  int pi_pin1_colnum;
  int pi_pin2_colnum;
  int pi_pin3_colnum;
  int upi_fast_colnum;     /** new, v2.0.0 **/
  int upi_slow_colnum;     /** new, v2.0.0 **/
  int upi_pin_colnum;      /** new, v2.0.0 **/
  int upi_pin0_colnum;     /** new, v2.0.0 **/
  int upi_pin1_colnum;     /** new, v2.0.0 **/
  int upi_pin2_colnum;     /** new, v2.0.0 **/
  int upi_pin3_colnum;     /** new, v2.0.0 **/
  int time_change;
  int grade_change;
  int pi_pmt_change;
  int pi_pin_change;
  fitsfile *new_fp;
  char newfilename[FILENAME_MAX_LENGTH];
  int new_colnum[HXD_EVENT_FITS_KEY_NUM];
  long new_irow;
}com;

static int createFITS(){

  int istat = 0;

  int hdutype;
  int morekey = 10;

  int hdunum;

  fits_create_file(&com.new_fp, com.newfilename, &istat);
/*
   BnkfGetM("HXDeventFitsRead:NEW_FILE_P", sizeof(com.new_fp),
            &used, &com.new_fp);                             */
/**** (!!) com.new_fp is set to NULL in HXDeventFitsRead *****/
  if ( istat ) {
    fprintf(stderr, "%s: fits_create_file('%s') failed (%d)\n",
	    pname, com.newfilename, istat);
    exit(-1);
  } else {
    fits_get_hdu_num(com.fp, &hdunum);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_hdu_num failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_movabs_hdu(com.fp, HXD_EVENT_FITS_PRIMARY_HDU, &hdutype, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_copy_hdu(com.fp, com.new_fp, morekey, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_copy_hdu failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_movabs_hdu(com.fp, HXD_EVENT_FITS_EVENT_EXTENTION, &hdutype,
		    &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, istat);
        return ANL_NG;
  } else {
    fits_copy_header(com.fp, com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_copy_header failed (%d)\n",
		pname, istat);
    return ANL_NG;
  } else {
    hxdeventFits_col_num( com.new_fp, com.new_colnum, &istat );
  }
  if( istat ){
    return ANL_NG;
  } else {
    fits_movabs_hdu(com.fp, HXD_EVENT_FITS_GTI_EXTENTION, &hdutype,
		    &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_copy_hdu(com.fp, com.new_fp, morekey, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_copy_header failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_movabs_hdu(com.fp, hdunum , &hdutype, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
		pname, istat);
    return ANL_NG;
  } else {
    fits_movabs_hdu(com.new_fp, HXD_EVENT_FITS_EVENT_EXTENTION,
		    &hdutype, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, istat);
        return ANL_NG;
  }

  return ANL_OK;

}

static int closeFITS(){

  int istat = 0;
  int hdutype;

  char taskname[FITS_KEY_MAX_LENGTH];
  char taskver[FITS_KEY_MAX_LENGTH];
  char credits[FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES];
  char *buf;

  /*** ------------------------------------------------ ***/
  /*** Primary Extension                                ***/
  /*** ------------------------------------------------ ***/
  fits_movabs_hdu(com.new_fp, HXD_EVENT_FITS_PRIMARY_HDU, &hdutype, &istat);
  if( istat ){
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_modify_key_lng(com.new_fp, "NEVENTS", com.new_irow,
			"&", &istat);
  }
  if( istat ){
    fprintf(stderr, "%s: fits_modify_key NEVENTS failed (%d)\n", pname, istat);
    return ANL_NG;
  } /* else {
    fits_write_date(com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr,"%s: fits_write_date() failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_chksum(com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
    return ANL_NG;
  } */

  /** write credits into HISTORY **/
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
    buf[FITS_KEY_MAX_LENGTH] = '\0';
  sprintf( credits, "%s", buf);

  hxdFitsHeader_old_writeCredits(com.new_fp, taskname, taskver, credits,
			     HXD_FITS_HEADER_UTIL_CREDIT_DUPLICATE, &istat);

  if ( istat ) {
    fprintf(stderr, "%s: hxdFitsHeader_old_writeCredits failed (%d)\n",
	      pname, istat);
    return ANL_NG;
  }

  /* 
  ** flush the CFITSIO buffer to avoid 
  ** compiler optimization problems on FC[789] 
  */
  fits_flush_buffer( com.new_fp, 0, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_flush_buffer failed (%d)\n",
              pname, istat);
    return ANL_NG;
  }

  /** write param **/
  if (hxdFitsHeader_old_writeParamer_wel( com.new_fp, HXD_EVENT_FITS_PRIMARY_HDU)
      != HXD_FITS_HEADER_UTIL_OK){
    fprintf(stderr, "%s: error in writing parameters\n", pname);
    return ANL_NG;
  }

  /** write date and checksum **/
  fits_write_date(com.new_fp, &istat);
  if ( istat ) {
    fprintf(stderr,"%s: fits_write_date() failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_chksum(com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  /*** ------------------------------------------------ ***/
  /*** Secondary Extension                              ***/
  /*** ------------------------------------------------ ***/
  fits_movabs_hdu(com.new_fp, HXD_EVENT_FITS_EVENT_EXTENTION, &hdutype,
		  &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_modify_key_lng(com.new_fp, "NEVENTS", com.new_irow,
			"&", &istat);
  }
  if( istat ){
    fprintf(stderr, "%s: fits_modify_key NEVENTS failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    if (com.nrow > com.new_irow)
      fits_delete_rows(com.new_fp, com.new_irow,
		       com.nrow - com.new_irow+1, &istat );
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_delete_rows failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    char history[HISTORY_MAX_LENGTH];
    sprintf(history, "   %s: Delete %d rows by task (%s).",
	    pname, com.nrow - com.new_irow, anl_task_name());
    fits_write_history(com.new_fp, history, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_history failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } /* else {
    fits_write_date(com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr,"%s: fits_write_date() failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_chksum(com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
    return ANL_NG;
  } */

  /** write credits into HISTORY **/
  hxdFitsHeader_old_writeCredits(com.new_fp, taskname, taskver, credits,
			     HXD_FITS_HEADER_UTIL_CREDIT_DUPLICATE, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: hxdFitsHeader_old_writeCredits failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  }

  /** write param **/
  if (hxdFitsHeader_old_writeParamer_wel( com.new_fp,
				      HXD_EVENT_FITS_EVENT_EXTENTION )
      != HXD_FITS_HEADER_UTIL_OK){
    fprintf(stderr, "%s: error in writing parameters\n", pname);
    return ANL_NG;
  }

  /** write date and checksum **/
  fits_write_date(com.new_fp, &istat);
  if ( istat ) {
    fprintf(stderr,"%s: fits_write_date() failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_chksum(com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  /*** ------------------------------------------------ ***/
  /*** Third Extension                                  ***/
  /*** ------------------------------------------------ ***/
  fits_movabs_hdu(com.new_fp, HXD_EVENT_FITS_GTI_EXTENTION, &hdutype,
		  &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } /* else {
    fits_write_date(com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr,"%s: fits_write_date() failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_chksum(com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
    return ANL_NG;
  } */ /* else {
    fits_modify_key_lng(com.new_fp, "NEVENTS", com.new_irow,
			"&", &istat);
  } if( istat ){
    fprintf(stderr, "%s: fits_modify_key NEVENTS failed (%d)\n", pname, istat);
    return ANL_NG;
    } */

  /** write credits into HISTORY **/
  hxdFitsHeader_old_writeCredits(com.new_fp, taskname, taskver, credits,
			     HXD_FITS_HEADER_UTIL_CREDIT_DUPLICATE, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: hxdFitsHeader_old_writeCredits failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  }

  /** write param **/
  if (hxdFitsHeader_old_writeParamer_wel( com.new_fp,
				      HXD_EVENT_FITS_GTI_EXTENTION)
      != HXD_FITS_HEADER_UTIL_OK){
    fprintf(stderr, "%s: error in writing parameters\n", pname);
    return ANL_NG;
  }

  /** write date and checksum **/
  fits_write_date(com.new_fp, &istat);
  if ( istat ) {
    fprintf(stderr,"%s: fits_write_date() failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_write_chksum(com.new_fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  /** CLOSE FITS **/
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
    fits_get_colnum( com.fp, casesen, "MTI", &com.mti_colnum, &istat);
  }
  if ( istat ) {
      fprintf(stderr, "%s: fits_get_colnum('MTI') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "GRADE_QUALTY",
		     &com.grade_qualty_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('GRADE_QUALTY') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "GRADE_PMTTRG",
		     &com.grade_pmttrg_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('GRADE_PMTTRG') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "GRADE_PINTRG",
		     &com.grade_pintrg_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('GRADE_PINTRG') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "GRADE_PSDSEL",
		     &com.grade_psdsel_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('GRADE_PSDSEL') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "GRADE_HITPAT",
		     &com.grade_hitpat_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('GRADE_HITPAT') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "GRADE_RESERV",
		     &com.grade_reserv_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('GRADE_RESERV') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "DET_TYPE", &com.det_type_colnum,
		    &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('PIN_ID') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "PIN_ID", &com.pin_id_colnum,
		    &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('DET_TYPE') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "PI_FAST",
		    &com.pi_fast_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('PI_FAST') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "PI_SLOW",
		    &com.pi_slow_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('PI_SLOW') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "PI_PIN", &com.pi_pin_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('PI_PIN') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "PI_PIN0",
		    &com.pi_pin0_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('PI_PIN0') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "PI_PIN1",
		    &com.pi_pin1_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('PI_PIN1') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "PI_PIN2",
		    &com.pi_pin2_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('PI_PIN2') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "PI_PIN3",
			&com.pi_pin3_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('PI_PIN3') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "UPI_FAST",
		    &com.upi_fast_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('UPI_FAST') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "UPI_SLOW",
		    &com.upi_slow_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('UPI_SLOW') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "UPI_PIN", &com.upi_pin_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('UPI_PIN') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "UPI_PIN0",
		    &com.upi_pin0_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('UPI_PIN0') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "UPI_PIN1",
		    &com.upi_pin1_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('UPI_PIN1') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "UPI_PIN2",
		    &com.upi_pin2_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('UPI_PIN2') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    fits_get_colnum( com.fp, casesen, "UPI_PIN3",
			&com.upi_pin3_colnum, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_get_colnum('UPI_PIN3') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  }

  return ANL_OK;

}

void
HXD2ndeventFitsWrite_old_startup(int *status)
{
  *status = ANL_OK;
}

void
HXD2ndeventFitsWrite_old_init(int *status)
{
  *status = ANL_OK;
}

void
HXD2ndeventFitsWrite_old_com(int *status)
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
HXD2ndeventFitsWrite_old_his(int *status)
{
  *status = ANL_OK;
}

void
HXD2ndeventFitsWrite_old_bgnrun(int *status)
{

  int used;

  BnkfGetM("HXDeventFitsRead:IOMODE", sizeof(int), &used,
	   &com.iomode);
  BnkfGetM("HXDeventFitsRead:GTIMODE", sizeof(int), &used,
	   &com.gtimode);

  BnkfGetM("HXDeventFitsRead:EV_TIME:CHANGE", sizeof(int), &used,
	   &com.time_change );
  BnkfGetM("HXDeventFitsRead:GRADE:CHANGE", sizeof(int), &used,
	   &com.grade_change );
  BnkfGetM("HXDeventFitsRead:PI_PMT:CHANGE", sizeof(int), &used,
	   &com.pi_pmt_change );
  BnkfGetM("HXDeventFitsRead:PI_PIN:CHANGE", sizeof(int), &used,
	   &com.pi_pin_change );

  BnkfGetM("HXDeventFitsRead:FILE_P", sizeof(com.fp), &used,
	   &com.fp);
  BnkfGetM("HXDeventFitsRead:NROW", sizeof(long), &used, &com.nrow);

  if( get_colnum() ){
    *status = ANL_QUIT;
    return;
  }

  else if(com.iomode == HXD_CREATE){

    BnkfGetM("HXDeventFitsRead:NEW_FILE_NAME",sizeof(com.newfilename),
	     &used, com.newfilename );

    if( createFITS() ){
      *status = ANL_QUIT;
      return;
    }

    BnkfPutM("HXDeventFitsRead:NEW_FILE_P",sizeof(com.new_fp), &com.new_fp );

    com.new_irow = 0;

  }

  *status = ANL_OK;

}

void
HXD2ndeventFitsWrite_old_ana(int nevent, int eventid, int *status)
{

  int istat = 0;
  int used;

  if( com.iomode == HXD_READONLY ){
    *status = ANL_OK;
    return;
  }

  if( com.iomode == HXD_OVERWRITE ){

    double time;
    unsigned int mti;
    /* short grade; */
    unsigned char det_type;
    unsigned char pin_id; /** new, v0.4.3 **/
    int pi_fast;
    int pi_slow;
    int pi_pin[4];
    double upi_fast;
    double upi_slow;
    double upi_pin[4];
    /* int grade; */
    int grade_temp;/** new, v0.4.0 **/
    short grade_qualty; /** new, v0.4.0 **/
    unsigned char  grade_pmttrg; /** new, v0.4.0 **/
    unsigned char  grade_pintrg; /** new, v0.4.0 **/
/*  unsigned short grade_psdsel;  */
    double grade_psdsel;
    unsigned int   grade_hitpat; /** new, v0.4.0 **/
    unsigned int   grade_reserv; /** new, v0.4.0 **/
    int grade_pi_pin;
    double grade_upi_pin;

    /* int int_grade; */
    int int_det_type;
    int int_pin_id;              /** new, v0.4.3 **/
/*    int int_pi_fast;
    int int_pi_slow;
    int int_pi_pin[4];
    int int_pi_pin_val; */

    long firstelem = 1;
    long nelements = 1;

    BnkfGetM("HXDeventFitsRead:IROW", sizeof(long), &used,
	     &com.irow);

    /** ================= for hxdtime ================ **/
    if( com.time_change == TRUE ){

      BnkfGetM("HXD:WEL:EV_TIME", sizeof(double), &used, &time);

      fits_write_col_dbl(com.fp, com.time_colnum, com.irow,
			 firstelem, nelements, &time, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('TIME') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

      BnkfGetM("HXD:WEL:MTI", sizeof(int), &used, &mti);

      fits_write_col_uint(com.fp, com.mti_colnum, com.irow,
			 firstelem, nelements, &mti, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('MTI') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

    }

    /** ================= for hxdgrade ================ **/
    if( com.grade_change == TRUE ){
      unsigned char char_val;

      /* BnkfGetM("HXD:WEL:GRADE", sizeof(int), &used, &grade); */
      BnkfGetM("HXD:WEL:GRADE_QUALTY", sizeof(int), &used,&grade_temp);
      grade_qualty = (short) grade_temp;
      BnkfGetM("HXD:WEL:GRADE_PMTTRG", sizeof(int), &used,&grade_temp);
      grade_pmttrg = (unsigned char)  grade_temp;
      BnkfGetM("HXD:WEL:GRADE_PINTRG", sizeof(int), &used,&grade_temp);
      grade_pintrg = (unsigned char)  grade_temp;
      BnkfGetM("HXD:WEL:GRADE_PSDSEL", sizeof(double), &used,&grade_psdsel);

      BnkfGetM("HXD:WEL:GRADE_HITPAT", sizeof(int), &used,&grade_temp);
      grade_hitpat = (unsigned int)   grade_temp;
      BnkfGetM("HXD:WEL:GRADE_RESERV", sizeof(int), &used,&grade_temp);
      grade_reserv = (unsigned int)   grade_temp;

      BnkfGetM("HXD:WEL:DET_TYPE", sizeof(int), &used, &int_det_type);
      det_type = (unsigned char) int_det_type;

      BnkfGetM("HXD:WEL:GRADE_PI_PIN",  sizeof(int), &used, &grade_pi_pin);
      BnkfGetM("HXD:WEL:GRADE_UPI_PIN", sizeof(double), &used, &grade_upi_pin);

      BnkfGetM("HXD:WEL:PIN_ID", sizeof(int), &used, &int_pin_id);
      pin_id = (unsigned char) int_pin_id;

      fits_write_col_sht(com.fp, com.grade_qualty_colnum, com.irow,
			 firstelem, nelements, &grade_qualty, &istat);
      if ( istat ) {
	fprintf(stderr,"%s:fits_write_col GRADE_QUALTY=0x%x failed (%d)\n",
		pname, grade_qualty, istat);
	return;
      } else {
	fits_write_col_byt(com.fp, com.grade_pmttrg_colnum, com.irow,
			   firstelem, nelements, &grade_pmttrg, &istat);
      } if ( istat ) {
	fprintf(stderr,"%s:fits_write_col GRADE_PMTTRG=%d failed (%d)\n",
		pname, grade_pmttrg, istat);
	return;
      } else {
	fits_write_col_byt(com.fp,com.grade_pintrg_colnum, com.irow,
			   firstelem, nelements, &grade_pintrg, &istat);
      } if ( istat ) {
	fprintf(stderr,"%s:fits_write_col GRADE_PINTRG=%d failed (%d)\n",
		pname, grade_pintrg, istat);
	return;
      } else {
	fits_write_col_dbl(com.fp, com.grade_psdsel_colnum, com.irow,
			   firstelem, nelements,&grade_psdsel, &istat);
      } if ( istat ) {
	fprintf(stderr,"%s:fits_write_col GRADE_PSDSEL=%d failed (%d)\n",
		pname, grade_psdsel, istat);
	return;
      } else {
	fits_write_col_uint(com.fp, com.grade_hitpat_colnum, com.irow,
			   firstelem, nelements, &grade_hitpat, &istat);
      } if ( istat ) {
	fprintf(stderr,"%s:fits_write_col GRADE_HITPAT=%d failed (%d)\n",
		pname, grade_hitpat, istat);
	return;
      } else {
	fits_write_col_uint(com.fp, com.grade_reserv_colnum, com.irow,
			   firstelem, nelements, &grade_reserv, &istat);
      } if ( istat ) {
	fprintf(stderr,"%s:fits_write_col GRADE_RESERV=%d failed (%d)\n",
		pname, grade_reserv, istat);
	return;
      }

      fits_write_col_byt(com.fp, com.det_type_colnum, com.irow,
			 firstelem, nelements, &det_type, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('DET_TYPE') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

      char_val = (unsigned char) grade_pi_pin;
      fits_write_col_byt(com.fp, com.pi_pin_colnum, com.irow,
			 firstelem, nelements, &char_val, &istat);
      if(DEBUG) fprintf(stderr,"%s: Fill PI_PIN=%d\n", pname, grade_pi_pin);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('PI_PIN') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

      fits_write_col_dbl(com.fp, com.upi_pin_colnum, com.irow,
			 firstelem, nelements, &grade_upi_pin, &istat);
      if(DEBUG) fprintf(stderr,"%s: Fill UPI_PIN=%d\n", pname, grade_pi_pin);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('PI_PIN') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

      fits_write_col_byt(com.fp, com.pin_id_colnum, com.irow,
			 firstelem, nelements, &pin_id, &istat);
      if(DEBUG) fprintf(stderr,"%s: Fill PIN_ID=%d\n", pname, pin_id);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('PIN_ID') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

    }

    /** ================= for hxdpi ================ **/
    if( com.pi_pmt_change == TRUE ){
      short short_val;

      BnkfGetM("HXD:WEL:PI_FAST", sizeof(int), &used, &pi_fast);
      BnkfGetM("HXD:WEL:PI_SLOW", sizeof(int), &used, &pi_slow);

      short_val = (short) pi_fast;
      fits_write_col_sht(com.fp, com.pi_fast_colnum, com.irow,
			 firstelem, nelements, &short_val, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('PI_FAST') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

      short_val = (short) pi_slow;
      fits_write_col_sht(com.fp, com.pi_slow_colnum, com.irow,
			 firstelem, nelements, &short_val, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('PI_SLOW') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

      BnkfGetM("HXD:WEL:UPI_FAST", sizeof(double), &used, &upi_fast);
      BnkfGetM("HXD:WEL:UPI_SLOW", sizeof(double), &used, &upi_slow);

      fits_write_col_dbl(com.fp, com.upi_fast_colnum, com.irow,
			 firstelem, nelements, &upi_fast, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('UPI_FAST') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

      fits_write_col_dbl(com.fp, com.upi_slow_colnum, com.irow,
			 firstelem, nelements, &upi_slow, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('UPI_SLOW') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

    }

    if( com.pi_pin_change == TRUE ){
      unsigned char char_val;

      BnkfGetM("HXD:WEL:PI_PIN", sizeof(int)*4, &used, pi_pin);

      char_val = (unsigned char) pi_pin[0];
      fits_write_col_byt(com.fp, com.pi_pin0_colnum, com.irow,
			 firstelem, nelements, &char_val, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('PI_PIN0') failed (%d)\n",
			pname, istat);
	*status = ANL_QUIT;
	return;
      }

      char_val = (unsigned char) pi_pin[1];
      fits_write_col_byt(com.fp, com.pi_pin1_colnum, com.irow,
			 firstelem, nelements, &char_val, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('PI_PIN1') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

      char_val = (unsigned char) pi_pin[2];
      fits_write_col_byt(com.fp, com.pi_pin2_colnum, com.irow,
			 firstelem, nelements, &char_val, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('PI_PIN2') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

      char_val = (unsigned char) pi_pin[3];
      fits_write_col_byt(com.fp, com.pi_pin3_colnum, com.irow,
			 firstelem, nelements, &char_val, &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('PI_PIN3') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

      BnkfGetM("HXD:WEL:UPI_PIN", sizeof(double)*4, &used, upi_pin);

      fits_write_col_dbl(com.fp, com.upi_pin0_colnum, com.irow,
			 firstelem, nelements, &upi_pin[0], &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('UPI_PIN0') failed (%d)\n",
			pname, istat);
	*status = ANL_QUIT;
	return;
      }

      fits_write_col_dbl(com.fp, com.upi_pin1_colnum, com.irow,
			 firstelem, nelements, &upi_pin[1], &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('UPI_PIN1') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

      fits_write_col_dbl(com.fp, com.upi_pin2_colnum, com.irow,
			 firstelem, nelements, &upi_pin[2], &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('UPI_PIN2') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

      fits_write_col_dbl(com.fp, com.upi_pin3_colnum, com.irow,
			 firstelem, nelements, &upi_pin[3], &istat);
      if ( istat ) {
	fprintf(stderr,"%s: fits_write_col('UPI_PIN3') failed (%d)\n",
		pname, istat);
	*status = ANL_QUIT;
	return;
      }

    }

  }else if(com.iomode == HXD_CREATE){

    HxdEventFits02 data;

    int int_trig;
    int int_unitid;
    /* int int_grade; */
    int int_grade_qualty; /** new, v0.4.0 **/
    int int_grade_pmttrg; /** new, v0.4.0 **/
    int int_grade_pintrg; /** new, v0.4.0 **/
/*  int int_grade_psdsel;  */
    int int_grade_hitpat; /** new, v0.4.0 **/
    int int_grade_reserv; /** new, v0.4.0 **/
    int int_det_type;
    int int_length_chk;
/*  int int_quality_flags; */
/*  int int_pi_fast;
    int int_pi_slow; */
    int int_pha_fast;
    int int_pha_slow;
/*  int int_pi_pin[4]; */
    int int_pha_pin[4];
    int pi_fast;
    int pi_slow;
    int pi_pin[4];
    int grade_pi_pin;
    double upi_fast;
    double upi_slow;
    double upi_pin[4];
    double grade_upi_pin;
    int int_pin_id;
    double s_time;

    int update;

    BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &used, &update );

    if( !(update & HXD_UPDATE_WEL) ){
      /*
      fprintf(stderr, "%s: WARNING: update no column \n", pname);
      */
      /** write no-update rows **/
      /*
      BnkfGetM( "HXD:WEL:EVENT", sizeof(HxdEventFits02), &used, &data );
      hxdeventFits_col_write( com.new_fp, com.new_irow, com.new_colnum,
			      &data, &istat );
      if( istat ){
	*status = ANL_QUIT;
	return;
      }
      */
      *status = ANL_OK;
      return;
    }

    com.new_irow++;

    BnkfGetM( "HXD:WEL:PACKET_S_TIME", sizeof(double), &used, &data.s_time);
    BnkfGetM( "HXD:WEL:EV_TIME", sizeof(double), &used, &data.time);
    BnkfGetM( "HXD:WEL:MTI", sizeof(int), &used, &data.mti);
    BnkfGetM( "HXD:WEL:GRADE_QUALTY", sizeof(int), &used,
	      &int_grade_qualty);
    BnkfGetM( "HXD:WEL:GRADE_PMTTRG", sizeof(int), &used,
	      &int_grade_pmttrg);
    BnkfGetM( "HXD:WEL:GRADE_PINTRG", sizeof(int), &used,
	      &int_grade_pintrg);
    BnkfGetM( "HXD:WEL:GRADE_PSDSEL", sizeof(double), &used,
	      &data.grade_psdsel);
    BnkfGetM( "HXD:WEL:GRADE_HITPAT", sizeof(int), &used,
	      &int_grade_hitpat);
    BnkfGetM( "HXD:WEL:GRADE_RESERV", sizeof(int), &used,
	      &int_grade_reserv);
    BnkfGetM( "HXD:WEL:DET_TYPE", sizeof(int), &used, &int_det_type);
    BnkfGetM( "HXD:WEL:PI_FAST", sizeof(int), &used, &pi_fast );
    BnkfGetM( "HXD:WEL:PI_SLOW", sizeof(int), &used, &pi_slow );
    BnkfGetM( "HXD:WEL:PI_PIN", sizeof(int)*4, &used, pi_pin );
    BnkfGetM( "HXD:WEL:GRADE_PI_PIN", sizeof(int), &used, &grade_pi_pin );
    BnkfGetM( "HXD:WEL:UPI_FAST", sizeof(double), &used, &upi_fast );
    BnkfGetM( "HXD:WEL:UPI_SLOW", sizeof(double), &used, &upi_slow );
    BnkfGetM( "HXD:WEL:UPI_PIN", sizeof(double)*4, &used, upi_pin );
    BnkfGetM( "HXD:WEL:GRADE_UPI_PIN", sizeof(double), &used, &grade_upi_pin );
    BnkfGetM( "HXD:WEL:PIN_ID", sizeof(int), &used, &int_pin_id);

    BnkfGetM( "HXD:WEL:UNITID", sizeof(int), &used, &int_unitid );
    BnkfGetM( "HXD:WEL:LENGTH_CHK", sizeof(int), &used, &int_length_chk );
    BnkfGetM( "HXD:WEL:WELTIME", sizeof(int), &used, &data.weltime );
    BnkfGetM( "HXD:WEL:QUALITY_FLAGS", sizeof(int), &used,
	     &data.quality_flags );
    BnkfGetM( "HXD:WEL:TRIG", sizeof(int), &used, &int_trig );
    BnkfGetM( "HXD:WEL:HIT_PATTERN_WELL", sizeof(int), &used,
	     &data.hit_pattern_well );
    BnkfGetM( "HXD:WEL:HIT_PATTERN_ANTI", sizeof(int), &used,
	     &data.hit_pattern_anti );
    BnkfGetM( "HXD:WEL:PHA_FAST", sizeof(int), &used, &int_pha_fast );
    BnkfGetM( "HXD:WEL:PHA_SLOW", sizeof(int), &used, &int_pha_slow );
    BnkfGetM( "HXD:WEL:PHA_PIN", sizeof(int)*4, &used, &int_pha_pin[0] );

    /*
      BnkfGetM( "HXD:WEL:PACKET_AETIME", sizeof(double), &used,
      &data.aetime);
    */
    BnkfGetM( "HXD:WEL:PACKET_SEC_HEADER", sizeof(int), &used, &data.ti );

    data.trig = (unsigned char) int_trig;
    data.unitid = (unsigned char) int_unitid;

    data.grade_qualty = (short)          int_grade_qualty;
    data.grade_pmttrg = (unsigned char ) int_grade_pmttrg;
    data.grade_pintrg = (unsigned char ) int_grade_pintrg;
/*  data.grade_psdsel = (unsigned short) int_grade_psdsel; */
    data.grade_hitpat = (unsigned int)   int_grade_hitpat;
    data.grade_reserv = (unsigned int)   int_grade_reserv;
    data.det_type   = (unsigned char) int_det_type;
    data.length_chk = (unsigned char) int_length_chk;
/*  data.quality_flags = (unsigned char) int_quality_flags; */
    data.pi_fast = pi_fast;
    data.pi_slow = pi_slow;
    data.pha_fast = (short) int_pha_fast;
    data.pha_slow = (short) int_pha_slow;
    data.pi_pin0 = pi_pin[0];
    data.pha_pin0 = (unsigned char) int_pha_pin[0];
    data.pi_pin1 = pi_pin[1];
    data.pha_pin1 = (unsigned char) int_pha_pin[1];
    data.pi_pin2 = pi_pin[2];
    data.pha_pin2 = (unsigned char) int_pha_pin[2];
    data.pi_pin3 = pi_pin[3];
    data.pha_pin3 = (unsigned char) int_pha_pin[3];
    data.pi_pin   = grade_pi_pin;
    data.pin_id = (unsigned char) int_pin_id;

    data.upi_fast = upi_fast;
    data.upi_slow = upi_slow;
    data.upi_pin0 = upi_pin[0];
    data.upi_pin1 = upi_pin[1];
    data.upi_pin2 = upi_pin[2];
    data.upi_pin3 = upi_pin[3];
    data.upi_pin  = grade_upi_pin;

    hxdeventFits_col_write( com.new_fp, com.new_irow, com.new_colnum,
			   &data, &istat );

    if( istat ){
      *status = ANL_QUIT;
      return;
    }

  }

  *status = ANL_OK;

}

#define HXD2NDEVENTFITSWRITE_YES 1
#define HXD2NDEVENTFITSWRITE_NO  0

void
HXD2ndeventFitsWrite_old_endrun(int *status)
{
  double timedel;
  int clock_rate;
  int used;
  int stat = 0;
  int size;
  int hxdtime_yn;

  BnkGet( "HXD:ftools:hxdtime_yn", sizeof(int), &size, &hxdtime_yn);

  if(com.iomode == HXD_CREATE){
    /* if( com.time_change == TRUE ){ */
    if( hxdtime_yn == HXD2NDEVENTFITSWRITE_YES ){
      BnkGet( "HXDfwelTime:HXD_WPU_CLK_RATE", sizeof(int), &used, &clock_rate);
      hxdeventFits_modify_weltime_unit(com.new_fp, clock_rate, &stat);
      if(stat){
	fprintf(stderr, "%s: update WELTIME unit failed (%d)\n", pname, stat);
	*status = ANL_QUIT;
	return;
      }

      BnkfGetM("HXDfwelTime:TIME_RESOLUTION", sizeof(double), &used, &timedel);
      stat = hxdFitsHeader_old_writeTIMEDEL(com.new_fp, HXD_EVENT_FITS_PRIMARY_HDU,
					timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_EVENT_FITS_PRIMARY_HDU);
	*status = ANL_QUIT;
	return;
      }

      stat = hxdFitsHeader_old_writeTIMEDEL(com.new_fp,
					HXD_EVENT_FITS_EVENT_EXTENTION,
					timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_EVENT_FITS_EVENT_EXTENTION);
	*status = ANL_QUIT;
	return;
      }

      stat =  hxdFitsHeader_old_writeTIMEDEL(com.new_fp,
					 HXD_EVENT_FITS_GTI_EXTENTION,
					 timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_EVENT_FITS_GTI_EXTENTION);
	*status = ANL_QUIT;
	return;
      }
    } 
    if( closeFITS() ){
      *status = ANL_QUIT;
      return;
    }

  } else if( com.iomode == HXD_OVERWRITE ){

    if( com.time_change == TRUE && hxdtime_yn == HXD2NDEVENTFITSWRITE_YES ){
      BnkGet( "HXDfwelTime:HXD_WPU_CLK_RATE", sizeof(int), &used, &clock_rate);
      hxdeventFits_modify_weltime_unit(com.fp, clock_rate, &stat);
      if(stat){
	fprintf(stderr, "%s: update WELTIME unit failed (%d)\n", pname, stat);
	*status = ANL_QUIT;
	return;
      }

      BnkfGetM("HXDfwelTime:TIME_RESOLUTION", sizeof(double), &used, &timedel);
      stat = hxdFitsHeader_old_writeTIMEDEL(com.fp, HXD_EVENT_FITS_PRIMARY_HDU,
					timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_EVENT_FITS_PRIMARY_HDU);
	*status = ANL_QUIT;
	return;
      }

      stat = hxdFitsHeader_old_writeTIMEDEL(com.fp,
					HXD_EVENT_FITS_EVENT_EXTENTION,
					timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_EVENT_FITS_EVENT_EXTENTION);
	*status = ANL_QUIT;
	return;
      }

      stat =  hxdFitsHeader_old_writeTIMEDEL(com.fp,
					 HXD_EVENT_FITS_GTI_EXTENTION,
					 timedel);
      if (stat != HXD_FITS_HEADER_UTIL_OK) {
	fprintf(stderr, "%s: update TIMEDEL failed in %d extention.", pname,
		HXD_EVENT_FITS_GTI_EXTENTION);
	*status = ANL_QUIT;
	return;
      }

      /** closeFITS() in HXDeventFitsRead, exit(); **/

    }
  }


  *status = ANL_OK;

}

void
HXD2ndeventFitsWrite_old_exit(int *status)
{
  *status = ANL_OK;
}

