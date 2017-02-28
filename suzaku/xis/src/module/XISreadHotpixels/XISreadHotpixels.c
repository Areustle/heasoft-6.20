/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:50:18 1999 by E. Miyata*/
/* xis_module/XISreadEvent/0.7_fast/XISreadEvent_v0.7.c: modified by M.Ozaki on Fri Aug 13 21:04:55 1999:
 *   - replace BnkGet, BnkPut, EvsGet, ...
 *     to BnkfGetM, BnkfPutM, EvsfGetM, ... .
 */
/*******************************************************************
*
*  XISreadHotpixels
*
*       XIS Fits-format event file を読んでバンクに流すモジュール
*       XISreadEvent v1.2を利用して作成
*
*       version 0.0     2005.06.13      Aya Bamba
*
*       version 0.1     2005.06.16      Aya Bamba
*
*         editmodeをxisTelemFormat.h内のマクロに書き換え
*         darkinit, darkupdateで使用できるようにする
*         editmode checkをinitからanaに移動
*
*       version 0.2     2005.06.20      Aya Bamba
*
*         editmodeがdarkinit, darkupdate以外の時はスルーするようにする
*
*       version 0.3     2005.06.21      Aya Bamba
*
*         editmodeがdarkinit, darkupdate以外の時は
*         _init, _ana共にスルーするようにする
*
*       version 0.3     2005.07.23      Aya Bamba
*
*         XIS:AEDATE -> XIS:EVENTS:AEDATE
*         XIS:EXPTIME -> XIS:EVENTS:EXPTIME
*
*       version 0.4     2005.07.23      Aya Bamba
*
*         XIS:EVENTS:AEDATE -> XIS:HOTPIXELS:AEDATE
*         XIS:EVENTS:EXPDATE -> XIS:HOTPIXELS:EXPDATE
*
*       version 0.41    2005.07.29      Aya Bamba
*
*         fixed some bugs according to the BNKhead
*
*       version 0.5     2005.10.03      Aya Bamba
*
*         largely changed: following XISreadEvent v2.10
*
*       version 0.6	2006.08.24     Y.ISHISAKI
*          unsigned long -> unsigned int event_seq_no in _ana()
*	   unsigned long -> unsigned int time1_exp, time2_exp in check_time()
*******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "xisTelemFormat.h"
#include "fitsio.h"
#include "xisEventFitsUtil.h"
#include "xisread.h"
#include "xisEditEventFitsUtil.h"
#include "xisNamingFunc.h"

static char pname[] = "XISreadHotPixels";
char XISreadHotpixels_version[] = "version 0.6";

/* #define BNKhead		"EVENTS:" */
static char BNKhead[] = "";

#define	elsif		else if
#define HOTPIXEL_EXTENSION_NAME "HOTPIXELS"
/* #define	_PRINT_OUT_  */

/* fits column information */
static fitsfile *infitsd;
static COLUMN_INF *column_inf;
static int column_num;
static int max_value_size;

static int eventbnk_fetched;
static int total_event_number;
static int iev;

/* macros used for check_time */
#define	TIME_1_EXCEED_2		0
#define	TIME_EQUAL		1
#define	TIME_2_EXCEED_1         2
#define EXPTIME_PER_DAY         353894400UL

/*******************************************************
 consistency check for aedate & exptime
********************************************************/
/* TIMEMATCH is defined in xisread.h */
static int
check_time(TIMEMATCH *time1, TIMEMATCH *time2)
{
  unsigned int time1_exp, time2_exp;
  time1_exp = (unsigned int)time1->exptime;
  time2_exp = (unsigned int)time2->exptime;

  if (time1->aedate == time2->aedate) {
/*    if ( time1_exp == time2_exp) {*/
      return TIME_EQUAL;
/*    } elsif  ( time1_exp < time2_exp ) {
#ifdef _PRINT_OUT_
      printf ("time1 %lu < time2 %lu \n", time1_exp, time2_exp);
#endif
      return TIME_2_EXCEED_1;
    } elsif ( time1_exp > ( time2_exp + EXPTIME_PER_DAY) ) {
*/      /* exptime counter round around */
/*      fprintf (stderr,
	       "%s: WARNING:  time1 (%lu) > time2 (%lu) counter round around \n",
               pname, time1_exp, time2_exp);
      return TIME_2_EXCEED_1;
    } else {
      fprintf (stderr,
               "%s: ERROR: time1 (%lu) > time2 (%lu)\n",
               pname, time1_exp, time2_exp);
      return TIME_1_EXCEED_2;
    }*/
  } elsif (time1->aedate < time2->aedate) {
#ifdef _PRINT_OUT_
    printf ("time1 aedate %d < time2 aedate %d\n",
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

#define SEQNUM_MATCH 1
#define SEQNUM_MISMATCH 2

static int
check_seqnum(int seqnum1, int seqnum2)
{
  if(seqnum1 == seqnum2 ) {
    return SEQNUM_MATCH;
  } else {
    return SEQNUM_MISMATCH;
  }

}

void
XISreadHotpixels_startup(int *status)
{
  *status = ANL_OK;
}

void
XISreadHotpixels_com(int *status)
{
  *status = ANL_OK;
}

void
XISreadHotpixels_init(int *status)
{
  int size, skip;
  /* edit mode number; see xisTelemFormat.h */
  int editmode;

  int fitsStatus=0, hdutype, extver;
  fitsfile *_infitsd;
  int flag_allprocessed;
  int flag_notyetfetched;

  FITS_GET_MEMORY;

  EvsDef("XISreadHotpixels:BEGIN");
  EvsDef("XISreadHotpixels:ENTRY");
  EvsDef("XISreadHotpixels:OK");

  /* editmode が darkinit/update以外の場合は, スルーするようにしておく */
  BnkGet("XIS:EDITMODE", sizeof(int), &size, &editmode);

  if ((editmode != XISeditDarkInit) && (editmode != XISeditDarkUpdate)){
    *status = ANL_OK;
    return;
  }

  EvsDef("XIS:frame first event");	/* frame の最初のイベントの時に立つ */
  EvsDef("XIS:frame last event");	/* frame の最後のイベントの時に立つ */
  /* 最後のイベントは自分が最後だと分からないから内容は信用しない事 */
  EvsDef("XIS:skip event");
  /* event fits の最後で event はないけど、frame はある時に立つ */

  /* open input event fits file */
  BnkGet("XIS:FITS:PTR", sizeof(_infitsd), &size, &_infitsd);
  FITS_CHECK_ERROR_ANL(fits_reopen_file(_infitsd,&infitsd,&fitsStatus));
  /* FITS_CHECK_ERROR_ANL(fits_movabs_hdu(infitsd, EVENT_EXTENSION_ID+1,
  &hdutype,&fitsStatus)); */
  hdutype = BINARY_TBL;
  extver = 0 ; /* extver check is ignored */
  FITS_CHECK_ERROR_ANL(fits_movnam_hdu(infitsd, hdutype, HOTPIXEL_EXTENSION_NAME,
				       extver, &fitsStatus));

  /* check total event number */
  FITS_CHECK_ERROR_ANL(fits_read_key(infitsd, TINT, "NAXIS2",
			     &total_event_number, NULL, &fitsStatus));

  /* list up columns  */
  if ( list_up_column(infitsd, BNKhead, &column_inf, &column_num,
		      &max_value_size)
      != ANL_TRUE) {
    *status = ANL_QUIT;
    return;
  }

  skip=ANL_TRUE;
  if ( fits2bank_keyword(infitsd, skip) != ANL_TRUE ) {
    *status = ANL_QUIT;
    return;
  }

  /* bank for lists */
  BnkDef("XIS:EVENTS:LIST:PTR", sizeof(char *));
  BnkDef("XIS:EVENTS:LIST:NUM", sizeof(int));
  BnkDef("XIS:EVENTS:LIST:MAXSIZE", sizeof(int));
  BnkPut("XIS:EVENTS:LIST:PTR", sizeof(char *), &column_inf);
  BnkPut("XIS:EVENTS:LIST:NUM", sizeof(int), &column_num);
  BnkPut("XIS:EVENTS:LIST:MAXSIZE", sizeof(int), &max_value_size);

#ifdef _NOT_USE
  BnkEqv("XIS:SEG", sizeof(int), "XIS:SEGMENT", 1);
  BnkEqv("XIS:PH", sizeof(int)*XISoneEventPixelTotNo5x5, "XIS:PHAS", 1);
#endif
  BnkEqv("XIS:EVENTS:AEDATE", sizeof(int), "XIS:AEDATE", 1);
  BnkEqv("XIS:EVENTS:EXPTIME", sizeof(int), "XIS:EXPTIME", 1);
  /* Size of the Bnk is 4 byte for EXPTIME. Defined in
                                            xisEditEventFitsUtil.c */
  BnkEqv("XIS:EVENTS:EXPOSURE_SEQ_NO", sizeof(int), "XIS:EXPOSURE_SEQ_NO", 1);

  eventbnk_fetched = ANL_FALSE;
  iev=0;

  flag_allprocessed = ANL_FALSE;
  BnkDef("XIS:READEVENT:ALLPROCESSED", sizeof(int));
  BnkPut("XIS:READEVENT:ALLPROCESSED", sizeof(int), &flag_allprocessed);

  flag_notyetfetched = ANL_TRUE;
  BnkDef("XIS:READEVENT:NOTYETFETCHED", sizeof(int));
  BnkPut("XIS:READEVENT:NOTYETFETCHED", sizeof(int), &flag_notyetfetched);

  *status = ANL_OK;
}

void
XISreadHotpixels_his(int *status)
{
  *status = ANL_OK;
}

void
XISreadHotpixels_bgnrun(int *status)
{
  EvsfSetM("XISreadHotpixels:BEGIN");
  *status = ANL_OK;
}

void
XISreadHotpixels_ana(int nevent, int eventid, int *status)
{
  /* edit mode number; see xisTelemFormat.h */
  int editmode;

  int size;
  TIMEMATCH time_frame, time_exp;
  static TIMEMATCH time_event;
  static TIMEMATCH time_prevevent;

  int time_status_frame_event, time_status_exp_event;
  int time_status_event_prevevent;
  int seqnum_status;
  unsigned int event_seq_no;
  /* for exposure extension */
  int seqnum_exposure, seqnum_event, window_option;
  int flag_expname_nos;
  int flag_allprocessed;
  int flag_notyetfetched;

  EvsfSetM ("XISreadHotpixels:ENTRY");

  /* editmode が darkinit,darkupdate以外の場合は, スルーするようにしておく */
  BnkfGetM("XIS:EDITMODE", sizeof(int), &size, &editmode);
  if ((editmode != XISeditDarkInit) && (editmode != XISeditDarkUpdate)){
    *status = ANL_OK;
    return;
  }

  /* check event number */
  if (total_event_number == 0) {
    *status = ANL_SKIP;
    flag_allprocessed = ANL_TRUE;
    BnkfPutM("XIS:READEVENT:ALLPROCESSED", sizeof(int), &flag_allprocessed);
    return;
  }

  /* bank get for time in frame extension */
  /* FFFヘッダのキーワード名変更に伴い、以下のように変更
     BNK  : READ_OUT_TIME_H(L) --->  EXPTIME
     変数 :read_out_time  ---> exptime                 */
  BnkfGetM("XIS:FRAMES:AEDATE", sizeof(int), &size, &time_frame.aedate);
  BnkfGetM("XIS:FRAMES:EXPTIME", sizeof(int), &size, &time_frame.exptime);
  BnkfGetM("XIS:FRAMES:WINDOW_OPTION", sizeof(int), &size, &window_option);

  if ( eventbnk_fetched == ANL_FALSE ) {
    /* read event and fill them to bank */
    iev++;
    if (iev > total_event_number) {
      EvsfSetM("XIS:skip event");
      flag_allprocessed = ANL_TRUE;
      BnkfPutM("XIS:READEVENT:ALLPROCESSED", sizeof(int), &flag_allprocessed);
      *status = ANL_SKIP;
      return;
    }
    if (fits2bank (infitsd, column_inf, column_num, iev, max_value_size)
	!= ANL_TRUE) {
      *status = ANL_QUIT;
      return;
    } else {
      eventbnk_fetched = ANL_TRUE;
      if( iev == 1) {
	flag_notyetfetched = ANL_FALSE;
	BnkfPutM("XIS:READEVENT:NOTYETFETCHED", sizeof(int),
		 &flag_notyetfetched);
      }
    }
#ifdef _PRINT_OUT_
  fprintf (stderr,"iev=%d eventbnk_fetched=%d\n", iev, eventbnk_fetched);
#endif
    /* save time of the previous event */
    time_prevevent = time_event;
  }
  /* bank get for time in event extension */
  /* FFFヘッダのキーワード名変更に伴い、以下のように変更
     BNK  : READ_OUT_TIME_H(L) --->  EXPTIME
     変数 :read_out_time  ---> exptime                 */
  BnkfGetM("XIS:AEDATE", sizeof(int), &size, &time_event.aedate);
  BnkfGetM("XIS:EXPTIME", sizeof(int), &size, &time_event.exptime);
  BnkfGetM("XIS:EXPOSURE_SEQ_NO", sizeof(int), &size, &seqnum_event);
  BnkfGetM("XIS:EVENT_SEQ_NO", sizeof(unsigned int), &size, &event_seq_no);

  time_status_frame_event = check_time(&time_frame, &time_event);
  if( iev == 1 ) {
    time_status_event_prevevent = TIME_2_EXCEED_1;
  } else {
    time_status_event_prevevent = check_time(&time_prevevent, &time_event);
  }

  if ( window_option != XISwindowOff ) {
    /* WINDOW mode */
    BnkfGetM("XIS:EXPNAMENOS", sizeof(int), &size, &flag_expname_nos);
    /* bank get in exposure extension */
    /* bank is valid only for window mode */
    if( flag_expname_nos == ANL_FALSE ) {
      BnkfGetM("XIS:EXPOSURES:AEDATE", sizeof(int), &size, &time_exp.aedate);
      BnkfGetM("XIS:EXPOSURES:EXPTIME", sizeof(int), &size, &time_exp.exptime);
      BnkfGetM("XIS:EXPOSURES:EXPOSURE_SEQ_NO", sizeof(int), &size,
	       &seqnum_exposure);
    } else {
      BnkfGetM("XIS:EXPOSURE:AEDATE", sizeof(int), &size, &time_exp.aedate);
      BnkfGetM("XIS:EXPOSURE:EXPTIME", sizeof(int), &size, &time_exp.exptime);
      BnkfGetM("XIS:EXPOSURE:EXPOSURE_SEQ_NO", sizeof(int), &size,
	       &seqnum_exposure);
    }
#ifdef _PRINT_OUT_
  fprintf (stderr,"In %s seq_num=%d exp_seq_num=%d \n",
	   pname,seqnum_event, seqnum_exposure);
#endif
#ifdef _PRINT_OUT_
    fprintf(stderr,"In %s %u %u %u\n", pname, time_frame.exptime,
	  time_exp.exptime, time_event.exptime);
#endif

    time_status_exp_event = check_time(&time_exp, &time_event);
    seqnum_status = check_seqnum(seqnum_exposure, seqnum_event);

    if( (time_status_exp_event == TIME_EQUAL) &&
	(time_status_frame_event == TIME_EQUAL) &&
	( seqnum_status == SEQNUM_MATCH) ) {
      /* FRAME, EXPOSURE, and EVENT information are consistent */
#ifdef _PRINT_OUT_
      fprintf (stderr,
       "In %s window option seq_num=%d exp_seq_num=%d event_seqnum=%u\n",
		pname,seqnum_event, seqnum_exposure, event_seq_no);
#endif
      if( (time_status_event_prevevent != TIME_EQUAL)
	  && (event_seq_no ==0) ) {
	/* 1st event in this FRAME */
	EvsfSetM("XIS:frame first event");
#ifdef _PRINT_OUT_
	fprintf (stderr,"%s: matched\n", pname);
	fprintf (stderr,"EVS: first event \n");
#endif
      } else if( ( (time_status_event_prevevent == TIME_EQUAL)
		 &&(event_seq_no ==0)) ||
		 ( (time_status_event_prevevent != TIME_EQUAL)
		 &&(event_seq_no !=0)) ) {
	/* strange */
	anl_msg_warning("\
%s: event_seq_no and event inconsistent for %dth event\n", pname, iev);
      } /* if time jump */
      eventbnk_fetched = ANL_FALSE;
      *status = ANL_LOOP;
    } else if( (time_status_exp_event != TIME_EQUAL) ||
		(seqnum_status != SEQNUM_MATCH) ) {
      /* EXPOSURES extenstion should be read again */
#ifdef _PRINT_OUT_
      fprintf(stderr, "\
%s %dth event and exp inconsistent\n", pname, iev);
#endif
      eventbnk_fetched = ANL_TRUE;
      *status = ANL_SKIP;
    } else if( time_status_frame_event != TIME_EQUAL ) {
      /* FRAMES extenstion should be read again */
#ifdef _PRINT_OUT_
      fprintf(stderr, "\
%s %dth event and frame inconsistent\n", pname, iev);
#endif
      eventbnk_fetched = ANL_TRUE;
      *status = ANL_SKIP;
    } else {
      /* strange combination */
      anl_msg_error("\
%s: strange time and seqnum info %dth event\n", pname, iev);
      *status = ANL_QUIT;
      return;
    }
  } else {
    /* not WINDOW mode */
    if( time_status_frame_event == TIME_EQUAL) {
      /* FRAME and EVENT information are consistent */
      if( event_seq_no==0) {
	/* Frame 1st event is identified with event_seq_no==0 */
	if( (time_status_event_prevevent == TIME_EQUAL)
	    && (editmode != XISeditTiming) ){
	  anl_msg_warning("\
%s: %d-th event event_seq_no=%u but no exptime jump\n",
		   pname, iev, event_seq_no);
	}
	/* 1st event in this Frame */
	EvsfSetM("XIS:frame first event");
#ifdef _PRINT_OUT_
	fprintf (stderr,"%s: matched\n", pname);
	fprintf (stderr,"EVS: first event \n");
#endif
      }
      if( (seqnum_event != 0) && (editmode != XISeditTiming) ){
	anl_msg_warning("\
%s: strange seqnum %d for %d th event\n", pname, seqnum_event, iev);
      }
      eventbnk_fetched = ANL_FALSE;
      *status = ANL_LOOP;
    } else if( time_status_frame_event == TIME_2_EXCEED_1 ){
      /* FRAME extenstion should be read again */
      eventbnk_fetched = ANL_TRUE;
      *status = ANL_SKIP;
    } else {
      /* strange combination */
      anl_msg_error("\
%s: %dth event, returned with %d\n", pname, iev-1, time_status_frame_event);
      anl_msg_error("\
strange time assignment event:(%d,%u)-frame:(%d,%u)\n",
	       time_event.aedate, (unsigned int)time_event.exptime,
	       time_frame.aedate, (unsigned int)time_frame.exptime);
      *status = ANL_QUIT;
      return;
    }
  } /* end of if window option */

  EvsfSetM ("XISreadEvent:OK");

}

void
XISreadHotpixels_endrun(int *status)
{
  *status = ANL_OK;
}

void
XISreadHotpixels_exit(int *status)
{
  *status = ANL_OK;
}
