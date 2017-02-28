/*
  XISreadExp

	XIS Fits-format exposure extension を読んでバンクに流すモジュール

	version 0.0	2000.01.16	Emi Miyata

		frame-----exp-----event----end
		  ↑       ↑       ↑      ↓
		  ↑       ↑       ↑------←	(event 単位)
		  ↑	   ↑		    ↓
		  ↑       ↑---------------←	(exposure 単位)
		  ↑			    ↓
		  ↑------------------------←	(frame 単位)

	というように FITS の構造をそのまま保存したような形で
	ループをまわすこととする。

      version 0.3     2005.08.22      Hiroshi Nakajima

      Change Bnk Name
         READ_OUT_TIME => EXPTIME
      window_num = 1<<(window_option+1);

      version 1.00    2005.09.13      Kiyoshi Hayashida
        largely modified from v0.3 to be compatible with window mode
      version 1.01    2005.09.16      Kiyoshi Hayashida
        fits_movnam_hdu is used instead of fits_movabs_hdu
      version 1.10    2005.10.03      Kiyoshi Hayashida
        BNK "XIS:READEVENT:ALLPROCESSED",
             "XIS:READEVENT:NOTYETFETCHED" are introduced.
        lastrowid is set just after exposure information is fetched.
      version 1.20    2005.12.05      Hiroshi Nakajima
        transparent when DarkInit, DarkUpdate, DarkFrame, and Frame mode
      version 1.3    2006.08.24      Y.ISHISAKI
	TLONG -> TINT in _init()
	unsigned long -> unsigned int time1_exp, time2_exp in check_time()
      version 1.4    2006.08.31      Y.ISHISAKI
        bug fix introduced in version 1.3, return status correctly in _ana()
      version 1.5    2007.01.30      Y.ISHISAKI
        add BNK XIS:EXPOSURES
	run without FRAMES and EXPOSURES extensions
      version 1.6    2007.04.30      Y.ISHISAKI
	change time counter wrap around message from WARNING -> INFO
	TIMEMATCH moved from xisread.h
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "xisTelemFormat.h"
#include "xisEventFitsUtil.h"
#include "xisEditEventFitsUtil.h"

/* #define _PRINT_OUT_ */

static char pname[] = "XISreadExp";
char XISreadExp_version[] = "version 1.6";

static int flag_expname_nos = ANL_FALSE;
static char BNKhead_exp[] = "EXPOSURES:";
static char BNKhead_nos[] = "EXPOSURE:";

/* fits column information */
static fitsfile *infitsd = NULL;
static COLUMN_INF *column_inf;
static int column_num;
static int max_value_size;

/* record row number of EXPOSURE extension (1st exp in the latest frame)*/
static long irow;
/* record row number of EXPOSURE extension (last exp in the latest frame) */
static long last_rowid;
/* flag valid BNK data from EXPOSURE extension are fetched*/
static int expbnk_fetched;
/* flag valid BNK data from EXPOSURE extension are fetched*/
static long expnum;

/* XIS DE Edit Mode */
static int edit_mode;

/* macros used for check_time */
#define TIME_1_EXCEED_2         0
#define TIME_EQUAL              1
#define TIME_2_EXCEED_1         2
#define EXPTIME_PER_DAY         353894400UL  /* EXPTIME counter SUZAKU XIS */

typedef struct {
  int read_out_time_h;
  int read_out_time_l;
  unsigned int read_out_time;
  int exptime;
  int aedate;
} TIMEMATCH;

/*******************************************************
 consistency check for aedate & exptime
********************************************************/
static int
check_time(TIMEMATCH *time1, TIMEMATCH *time2)
{
  unsigned int time1_exp, time2_exp;
  time1_exp = (unsigned int)time1->exptime;
  time2_exp = (unsigned int)time2->exptime;

  if ( time1->aedate == time2->aedate ) {
    if ( time1_exp == time2_exp) {
      return TIME_EQUAL;
    } else if ( time1_exp < time2_exp ) {
#ifdef _PRINT_OUT_
      printf("time1 %u < time2 %u \n", time1_exp, time2_exp);
#endif
      return TIME_2_EXCEED_1;
    } else if ( time1_exp > time2_exp + EXPTIME_PER_DAY ) {
      /* Exptime counter wrap round. It occurs every about 2weeks.
	 Margin in the inequality should be longer than one XIS exposure
	 and shorter than 2weeks. One day is adopted here for it. */
      anl_msg_info("\
%s: INFO: time1 (%u) > time2 (%u) counter wrap around\n",
               pname, time1_exp, time2_exp);
      return TIME_2_EXCEED_1;
    } else {
      anl_msg_error("\
%s: ERROR: time1 (%u) > time2 (%u)\n", pname, time1_exp, time2_exp);
      return TIME_1_EXCEED_2;
    }
  } else if ( time1->aedate < time2->aedate ) {
#ifdef _PRINT_OUT_
    printf("time1 aedate %d < time2 aedate %d\n",
	   time1->aedate, time2->aedate);
#endif
    return TIME_2_EXCEED_1;
  } else {
    anl_msg_error("\
%s: ERROR: time1 aedate (%d) > time2 aedate (%d)\n",
             pname, time1->aedate, time2->aedate);
    return TIME_1_EXCEED_2;
  }
}

void
XISreadExp_startup(int *status)
{
  *status = ANL_OK;
}

void
XISreadExp_com(int *status)
{
  *status = ANL_OK;
}

void
XISreadExp_init(int *status)
{
  fitsfile *_infitsd;
  int size, hdutype, skip;
  int extver;
  long frames;
  int fitsStatus = 0;
  char *bnkhead;

  FITS_GET_MEMORY;

  EvsDef("XISreadExp:BEGIN");
  EvsDef("XISreadExp:ENTRY");
  EvsDef("XISreadExp:OK");

  EvsDef("XIS:frame first exp");/* 最初の exposure の時に立つ */
  EvsDef("XIS:frame last exp"); /* 最後の exposure の時に立つ */

  BnkDef("XIS:EXPNAMENOS", sizeof(int));
  BnkDef("XIS:EXPOSURES", sizeof(long));

  /* Get Event mode */
  BnkGet("XIS:EDITMODE", sizeof(int), &size, &edit_mode);

  BnkGet("XIS:FRAMES", sizeof(frames), &size, &frames);
  if ( 0 == frames ) {	/* NO FRAMES EXTENSION */
    expnum = 0;
    goto no_exposures;
  }

  /* open input event fits file */
  BnkGet("XIS:FITS:PTR", sizeof(_infitsd), &size, &_infitsd);
  FITS_CHECK_ERROR_ANL(
fits_reopen_file(_infitsd, &infitsd, &fitsStatus));

  hdutype = BINARY_TBL;
  extver = 0;
  /* Check EXPOSURES extension or EXPOSURE extension */
  flag_expname_nos = ANL_FALSE;
  bnkhead = BNKhead_exp;
fits_movnam_hdu(infitsd, hdutype, "EXPOSURES", extver, &fitsStatus);
  if ( fitsStatus ) {
    fitsStatus = 0;
    flag_expname_nos = ANL_TRUE;
    bnkhead = BNKhead_nos;
    fits_movnam_hdu(infitsd, hdutype, "EXPOSURE", extver, &fitsStatus);
  }
  if ( fitsStatus ) {
    anl_msg_warning("\
%s: WARNING: No EXPOSURE(S) EXTENSION in input fff, ignore it\n", pname);
    expnum = 0;	/* NO EXPOSURE(S) EXTENSION */
    goto no_exposures;
  }
  /* check total EXP number -> expnum */
  FITS_CHECK_ERROR_ANL(
fits_read_key_lng(infitsd, "NAXIS2", &expnum, NULL, &fitsStatus));

  /* list up columns  */
  if ( list_up_column(infitsd, bnkhead, &column_inf, &column_num,
		      &max_value_size) != ANL_TRUE) {
    *status = ANL_QUIT;
    return;
  }

  skip = ANL_TRUE;
  if ( fits2bank_keyword(infitsd, skip) != ANL_TRUE ) {
    *status = ANL_QUIT;
    return;
  }

  if ( flag_expname_nos == ANL_FALSE ) {
    BnkDef("XIS:EXPOSURES:LIST:PTR", sizeof(char *));
    BnkDef("XIS:EXPOSURES:LIST:NUM", sizeof(int));
    BnkDef("XIS:EXPOSURES:LIST:MAXSIZE", sizeof(int));
    BnkPut("XIS:EXPOSURES:LIST:PTR", sizeof(char *), &column_inf);
    BnkPut("XIS:EXPOSURES:LIST:NUM", sizeof(int), &column_num);
    BnkPut("XIS:EXPOSURES:LIST:MAXSIZE", sizeof(int), &max_value_size);
  } else {
    BnkDef("XIS:EXPOSURE:LIST:PTR", sizeof(char *));
    BnkDef("XIS:EXPOSURE:LIST:NUM", sizeof(int));
    BnkDef("XIS:EXPOSURE:LIST:MAXSIZE", sizeof(int));
    BnkPut("XIS:EXPOSURE:LIST:PTR", sizeof(char *), &column_inf);
    BnkPut("XIS:EXPOSURE:LIST:NUM", sizeof(int), &column_num);
    BnkPut("XIS:EXPOSURE:LIST:MAXSIZE", sizeof(int), &max_value_size);
  }

  irow = 0L;
  last_rowid = 0L;
  expbnk_fetched = ANL_FALSE;

 no_exposures:

  BnkPut("XIS:EXPNAMENOS", sizeof(flag_expname_nos), &flag_expname_nos);
  BnkPut("XIS:EXPOSURES", sizeof(expnum), &expnum);

  *status = ANL_OK;
}

void
XISreadExp_his(int *status)
{
  *status = ANL_OK;
}

void
XISreadExp_bgnrun(int *status)
{
  EvsfSetM("XISreadExp:BEGIN");
  *status = ANL_OK;
}

void
XISreadExp_ana(int nevent, int eventid, int *status)
{
  static long rowid[XISactiveFrameVsize];
  /* maximum window_no is 16 in XIS operation, but allocate 1024 here*/

  TIMEMATCH time_frame;
  TIMEMATCH time_event;
  TIMEMATCH time_exp;

  int used;
  int window_num;
  int window_option, exp_seq_num, seq_num;
  int ith_window;
  int time_status_frame_exp, time_status_exp_event = -1;
  int allprocessed;
  int notyetfetched;

  EvsfSetM ("XISreadExp:ENTRY");

  if ( 0 == expnum ) {	/* NO EXPOSURES EXTENSION */
    *status = ANL_OK;
    return;
  }

  allprocessed = ANL_FALSE;
  BnkfGetM("XIS:READEVENT:ALLPROCESSED", sizeof(int), &used, &allprocessed);
  if ( allprocessed == ANL_TRUE ) {
    *status = ANL_OK;
    return;
  }

  notyetfetched = ANL_FALSE;
  BnkfGetM("XIS:READEVENT:NOTYETFETCHED", sizeof(int), &used, &notyetfetched);

  if ( edit_mode == XISeditDarkInit || edit_mode == XISeditDarkUpdate ) {
	*status = ANL_OK;
	return;
  }

  /* bank get window option from FRAMES extension */
  BnkfGetM("XIS:FRAMES:WINDOW_OPTION", sizeof(int), &used, &window_option);
  if ( window_option == XISwindowOff ) {
    *status = ANL_OK;
    return;
  }

  if (window_option < XISwindow4  || window_option > XISwindow16) {
    anl_msg_warning("\
%s: unknown window option -- %d\n", pname, window_option);
    *status = ANL_SKIP;
    return;
  }
  window_num = 1<<(window_option+1);

  /* bank get for time in frame extension */
  BnkfGetM("XIS:FRAMES:AEDATE", sizeof(int), &used, &time_frame.aedate);
  BnkfGetM("XIS:FRAMES:EXPTIME", sizeof(int), &used, &time_frame.exptime);
#ifdef _PRINT_OUT_
  fprintf(stderr,"\
In %s : FRAME aedate=%d exptime=%lu \n",
	  pname,time_frame.aedate, time_frame.exptime);
#endif

  /* Read exposure extension and bank it if valid bank is not set.
     only 1st exposure in a frame is fetched here.  */
  if ( expbnk_fetched == ANL_FALSE ) {
    if ( last_rowid == 0 ) {
      irow = 1;
    } else {
      irow = last_rowid + window_num;
    }
#ifdef _PRINT_OUT_
    fprintf(stderr, "\
In %s %dth row is going to be fetched.\n", pname, row);
#endif
    if ( irow > expnum ) {
      *status = ANL_OK;
      return;
    } else {
      if ( fits2bank(infitsd, column_inf, column_num, irow, max_value_size)
	   != ANL_TRUE) {
	*status = ANL_QUIT;
	return;
      } else {
	expbnk_fetched = ANL_TRUE;
	last_rowid = irow;
      }
    }
  } /* end of if expbnk_fetched */

  /* bank get for time in exposure extension */
  if ( flag_expname_nos == ANL_FALSE ) {
    BnkfGetM("XIS:EXPOSURES:EXPOSURE_SEQ_NO",sizeof(int), &used, &exp_seq_num);
    BnkfGetM("XIS:EXPOSURES:AEDATE",sizeof(int), &used, &time_exp.aedate);
    BnkfGetM("XIS:EXPOSURES:EXPTIME",sizeof(int), &used, &time_exp.exptime);
  } else {
    BnkfGetM("XIS:EXPOSURE:EXPOSURE_SEQ_NO", sizeof(int), &used, &exp_seq_num);
    BnkfGetM("XIS:EXPOSURE:AEDATE", sizeof(int), &used, &time_exp.aedate);
    BnkfGetM("XIS:EXPOSURE:EXPTIME", sizeof(int), &used, &time_exp.exptime);
  }
#ifdef _PRINT_OUT_
  fprintf(stderr, "\
In %s EXPOSURES: aedate=%d exptime=%lu seq_num=%d\n",
	   pname,time_exp.aedate, time_exp.exptime, exp_seq_num);
#endif

  /* bank get time in event extension */
  if ( notyetfetched != ANL_TRUE ) {
    BnkfGetM("XIS:EVENTS:AEDATE", sizeof(int), &used, &time_event.aedate);
    BnkfGetM("XIS:EVENTS:EXPTIME", sizeof(int), &used, &time_event.exptime);
    BnkfGetM("XIS:EVENTS:EXPOSURE_SEQ_NO", sizeof(int), &used, &seq_num);
    time_status_exp_event = check_time(&time_exp, &time_event);
  } else {
    seq_num = 0;
  }
  time_status_frame_exp = check_time(&time_frame, &time_exp);

#ifdef _PRINT_OUT_
  fprintf(stderr,"\
In %s: Frame:%lu Exp:%lu Event:%lu %d %d\n",
	  pname, time_frame.exptime,
	  time_exp.exptime, time_event.exptime,
	  notyetfetched, expbnk_fetched);
#endif

  switch ( time_status_frame_exp ) {
  case TIME_EQUAL:
    if ( time_status_exp_event == TIME_EQUAL || notyetfetched == ANL_TRUE ) {
      /* frame, exposure, event time information are consistent */
      if ( exp_seq_num == 0 )  {
	EvsfSetM("XIS:frame first exp");
#ifdef _PRINT_OUT_
	fprintf(stderr, "\
In %s exp_seq_num=%d seq_num=%d row=%d\n",
		pname,exp_seq_num,seq_num,row);
#endif
	rowid[0] = irow;
	for (ith_window = 1; ith_window < window_num; ith_window++) {
	  rowid[ith_window] = irow + ith_window;
	}
      } else if ( exp_seq_num == window_num - 1 ) {
	EvsfSetM ("XIS:frame last exp");
      }	 /* end of if exp_seq_num */
      if ( seq_num < window_num ) {
	irow = rowid[seq_num];
	if ( fits2bank(infitsd, column_inf, column_num, irow, max_value_size)
	     != ANL_TRUE ) {
	  *status = ANL_QUIT;
	  return;
	}
	EvsfSetM ("XISreadExp:OK");
	*status = ANL_LOOP;
	return;
      }
      anl_msg_error("\
%s: strange seqnum %d %d\n",pname, exp_seq_num,seq_num);
      *status = ANL_QUIT;
      return;
    }

    if ( time_status_exp_event == TIME_2_EXCEED_1 ) {
      /* new FRAME & EXPOSURE extension must be read */
	expbnk_fetched = ANL_FALSE;
	*status = ANL_SKIP;
	return;
    }

    break;

  case TIME_2_EXCEED_1:
    /* new FRAME extension must be read again */
    expbnk_fetched = ANL_FALSE;
    *status = ANL_SKIP;
    return;
    break;

  default:
    ;
  }

  anl_msg_error("\
%s: %ldth exposure, returned with %d\n", pname, irow, time_status_frame_exp);
  anl_msg_error("\
strange time assignment exp:(%d,%u)-frame:(%d,%u)\n",
             time_exp.aedate, (unsigned int)time_exp.exptime,
             time_frame.aedate, (unsigned int)time_frame.exptime);

  *status = ANL_QUIT;
}

void
XISreadExp_endrun(int *status)
{
  *status = ANL_OK;
}

void
XISreadExp_exit(int *status)
{
  int fitsStatus = 0;

  if ( NULL != infitsd ) {
    fits_close_file(infitsd, &fitsStatus);
    if ( fitsStatus ) {
      anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, fitsStatus);
      *status = ANL_QUIT;
      return;
    }
  }

  *status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; End: ***
*/
