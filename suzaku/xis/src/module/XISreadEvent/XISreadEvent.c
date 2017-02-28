/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:50:18 1999 by E. Miyata*/
/* xis_module/XISreadEvent/0.7_fast/XISreadEvent_v0.7.c: modified by M.Ozaki on Fri Aug 13 21:04:55 1999:
 *   - replace BnkGet, BnkPut, EvsGet, ...
 *     to BnkfGetM, BnkfPutM, EvsfGetM, ... .
 */
/*

  XISreadEvent

	XIS Fits-format event file を読んでバンクに流すモジュール

    version 0.0	98.04.01	Emi Miyata
	従来の event だけの流れをやめて

		frame-----exp-----event----end
		  ↑       ↑       ↑      ↓
		  ↑       ↑       ↑------←	(event 単位)
		  ↑	   ↑		    ↓
		  ↑       ↑---------------←	(exposure 単位)
		  ↑			    ↓
		  ↑------------------------←	(frame 単位)

	というように FITS の構造をそのまま保存したような形で
	ループをまわすこととする。

    version 0.1	98.10.21	Emi Miyata

    version 0.2	1999.07.16	Emi Miyata

    version 0.3	1999.07.17	Emi Miyata
	ftools 化にむけて xisread 対応とした。

    version 0.4	1999.07.23	Emi Miyata
	bank name の定義を変えた ("XIS:..." -> "XIS:EVENT:..")

    version 0.5	1999.07.23	Emi Miyata
	bank name をもとに戻した。

    version 0.6	1999.07.25	Emi Miyata
	read keywords in event extension

    version 0.7	1999.07.29	Emi Miyata
	check consistency AEDATE/READ_OUTTIME

    version 0.9	2000.02.05	Emi Miyata
	support exposure extension

    version ?.?	2004.03.10	Hiroshi Murakami
	EVENT_SEQ_NO = 0 のeventが2回出力されるbugを修正

    version 1.2     2005.06.13      Atsushi Senda
	FFF のヘッダキーワード名変更(READ_OUT_TIME --> EXPTIME)
	に伴い、参照する BNK を変更
	  XIS:READ_OUT_TIME --> XIS:EXPTIME
	  XIS:FRAMES:READ_OUT_TIME --> XIS:FRAMES:EXPTIME
	値を格納する変数 exptime を追加 (xisread.h)

    version 1.3     2005.06.20      Aya Bamba
	editmode = darkinit or darkupdateの時はスルーするようにする。

    version 1.4     2005.07.21      Hiroshi Nakajima
	BNKhead = EVENTS:

    version 1.5     2005.07.22      Hiroshi Nakajima
	XIS:AEDATE -> XIS:EVENTS:AEDATE
	XIS:EXPTIME -> XIS:EVENTS:EXPTIME

    version 1.51     2005.08.29     Kiyoshi Hayashida
	1) BNKhead = "EVENTS" -> ""
	2) BnkEqv AEDATE,EXPTIME introduced
	3) (unsigned long) for exptime

    version 2.00     2005.09.13     Kiyoshi Hayashida
	Largely modified from 1.51 to be compatible with Window mode
    version 2.01     2005.09.16     Kiyoshi Hayashida
	fits_movnam_hdu is used instead of fits_movabs_hdu
    version 2.10     2005.10.03     Kiyoshi Hayashida
	Bnk "XIS:READEVENT:ALLPROCESSED"
            "XIS:READEVENT:NOTYETFETCHED" are introduced
	seqnum consistency check is skipped for timing mode
    version 2.20     2005.12.05     Hiroshi Nakajima
	transparent when the edit mode is Frame, DarkInit,
	DarkUpdate, and DarkFrame mode
    version 2.30     2005.12.09     Hiroshi Nakajima
	Bnkput XIS:EVENTS:LIST:*** when Frame, DarkInit,
	DarkUpdate, and DarkFrame mode
    version 2.4	2006.08.24     Y.ISHISAKI
	unsigned long -> unsigned int event_seq_no in _ana()
	unsigned long -> unsigned int time1_exp, time2_exp in check_time()
    version 2.41	2006.09.06     K.Hayashida
	WARNING message is replaced with INFO
    version 2.5	2007.01.30     Y.ISHISAKI
	run without FRAMES and EXPOSURES extensions
    version 2.6	2007.04.30     Y.ISHISAKI
	run without FRAMES and EXPOSURES extensions
	TIMEMATCH moved from xisread.h
	handle event by event, when no FRAMES nor EXPOSURES extensions
	show processing percentage in _ana()
    version 2.7 2007.05.14  Y.ISHISAKI
	treat HOSPIXELS extension as EVENTS when DarkInit, DarkUpdate
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "fitsio.h"
#include "xisTelemFormat.h"
#include "xisEventFitsUtil.h"
#include "xisEditEventFitsUtil.h"
#include "xisNamingFunc.h"

static char pname[] = "XISreadEvent";
char XISreadEvent_version[] = "version 2.7";

/* #define BNKhead		"EVENTS:" */
static char BNKhead[] = "";

/* #define	_PRINT_OUT_  */

/* fits column information */
static int editmode;
static long frames;
static long exposures;
static int flag_expname_nos;
static fitsfile *infitsd;
static COLUMN_INF *column_inf;
static int column_num;
static int max_value_size;

static int eventbnk_fetched;
static int total_event_num;
static int total_event_num_div_10;
static int total_event_digits;
static int iev;

/* macros used for check_time */
#define	TIME_1_EXCEED_2		0
#define	TIME_EQUAL		1
#define	TIME_2_EXCEED_1         2
#define EXPTIME_PER_DAY         353894400UL

typedef struct {
  unsigned int exptime;
  int aedate;
} TIMEMATCH;

/*******************************************************
 consistency check for aedate & exptime
    compare time1 and time2; which is earlier ?
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

#define SEQNUM_MATCH 1
#define SEQNUM_MISMATCH 2

static int
check_seqnum(int seqnum1, int seqnum2)
{
  if ( seqnum1 == seqnum2 ) {
    return SEQNUM_MATCH;
  } else {
    return SEQNUM_MISMATCH;
  }

}

void
XISreadEvent_startup(int *status)
{
  *status = ANL_OK;
}

void
XISreadEvent_com(int *status)
{
  *status = ANL_OK;
}

void
XISreadEvent_init(int *status)
{
  char *extname;
  int used, skip;
  int hdutype, extver;
  fitsfile *_infitsd;
  int allprocessed;
  int notyetfetched;
  int fitsStatus = 0;

  FITS_GET_MEMORY;

  EvsDef("XISreadEvent:BEGIN");
  EvsDef("XISreadEvent:ENTRY");
  EvsDef("XISreadEvent:OK");

  /* editmode が darkinit/update/frame の場合はスルーするようにしておく */
  BnkGet("XIS:EDITMODE", sizeof(editmode), &used, &editmode);
  BnkGet("XIS:FRAMES", sizeof(frames), &used, &frames);
  BnkGet("XIS:EXPOSURES", sizeof(exposures), &used, &exposures);
  BnkGet("XIS:EXPNAMENOS", sizeof(flag_expname_nos), &used, &flag_expname_nos);

  switch ( editmode ) {
  case XISedit5x5:
  case XISedit3x3:
  case XISedit2x2:
  case XISeditTiming:
    extname = EVENT_EXTENSION_NAME;
    break;
  case XISeditDarkInit:
  case XISeditDarkUpdate:
    extname = HOTPIXEL_EXTENSION_NAME;
    break;
  default:
    /* bank for lists */
    BnkDef("XIS:EVENTS:LIST:PTR", sizeof(char *));
    BnkDef("XIS:EVENTS:LIST:NUM", sizeof(int));
    BnkDef("XIS:EVENTS:LIST:MAXSIZE", sizeof(int));
    BnkPut("XIS:EVENTS:LIST:PTR", sizeof(char *), &column_inf);
    BnkPut("XIS:EVENTS:LIST:NUM", sizeof(int), &column_num);
    BnkPut("XIS:EVENTS:LIST:MAXSIZE", sizeof(int), &max_value_size);

    /* 最後のイベントは自分が最後だと分からないから内容は信用しない事 */
    EvsDef("XIS:skip event");

    allprocessed = ANL_FALSE;
    BnkDef("XIS:READEVENT:ALLPROCESSED", sizeof(int));
    BnkPut("XIS:READEVENT:ALLPROCESSED", sizeof(int), &allprocessed);

    notyetfetched = ANL_TRUE;
    BnkDef("XIS:READEVENT:NOTYETFETCHED", sizeof(int));
    BnkPut("XIS:READEVENT:NOTYETFETCHED", sizeof(int), &notyetfetched);

    *status = ANL_OK;
    return;
  }

  EvsDef("XIS:frame first event");	/* frame の最初のイベントの時に立つ */
  EvsDef("XIS:frame last event");	/* frame の最後のイベントの時に立つ */
  /* 最後のイベントは自分が最後だと分からないから内容は信用しない事 */
  EvsDef("XIS:skip event");
  /* event fits の最後で event はないけど、frame はある時に立つ */

  /* open input event fits file */
  BnkGet("XIS:FITS:PTR", sizeof(_infitsd), &used, &_infitsd);
  FITS_CHECK_ERROR_ANL(
fits_reopen_file(_infitsd, &infitsd, &fitsStatus));
  hdutype = BINARY_TBL;
  extver = 0;	/* extver check is ignored */
  FITS_CHECK_ERROR_ANL(
fits_movnam_hdu(infitsd, hdutype, extname, extver, &fitsStatus));

  /* check total event number */
  FITS_CHECK_ERROR_ANL(
fits_read_key(infitsd, TINT, "NAXIS2", &total_event_num, NULL, &fitsStatus));
  total_event_num_div_10 = total_event_num / 10;
  total_event_digits = (int)floor(log10(total_event_num)) + 1;

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
/* Size of the Bnk is 4 byte for EXPTIME. Defined in xisEditEventFitsUtil.c */
  BnkEqv("XIS:EVENTS:EXPOSURE_SEQ_NO", sizeof(int), "XIS:EXPOSURE_SEQ_NO", 1);

  eventbnk_fetched = ANL_FALSE;
  iev = 0;

  allprocessed = ANL_FALSE;
  BnkDef("XIS:READEVENT:ALLPROCESSED", sizeof(int));
  BnkPut("XIS:READEVENT:ALLPROCESSED", sizeof(int), &allprocessed);

  notyetfetched = ANL_TRUE;
  BnkDef("XIS:READEVENT:NOTYETFETCHED", sizeof(int));
  BnkPut("XIS:READEVENT:NOTYETFETCHED", sizeof(int), &notyetfetched);

  *status = ANL_OK;
}

void
XISreadEvent_his(int *status)
{
  *status = ANL_OK;
}

void
XISreadEvent_bgnrun(int *status)
{
  EvsfSetM("XISreadEvent:BEGIN");

  *status = ANL_OK;
}

void
XISreadEvent_ana(int nevent, int eventid, int *status)
{
  static TIMEMATCH time_event;
  static TIMEMATCH time_prevevent;

  int used;
  int time_status_frame_event, time_status_exp_event;
  int time_status_event_prevevent;
  int seqnum_status;
  unsigned int event_seq_no;

/* for FRAMES extension */
  int window_option;
  TIMEMATCH time_frame;

/* for EXPOSURES extension */
  int seqnum_exposure, seqnum_event;
  TIMEMATCH time_exp;

  int allprocessed;
  int notyetfetched;

  EvsfSetM("XISreadEvent:ENTRY");

  if ( editmode == XISeditDarkFrame || editmode == XISeditFrame ) {
    *status = ANL_OK;
    return;
  }

  /* check event number */
  if ( total_event_num == 0 ) {
    *status = ANL_SKIP;
    allprocessed = ANL_TRUE;
    BnkfPutM("XIS:READEVENT:ALLPROCESSED", sizeof(int), &allprocessed);
    return;
  }

  /* bank get for time in frame extension */
  /* FFFヘッダのキーワード名変更に伴い、以下のように変更
     BNK  : READ_OUT_TIME_H(L) --->  EXPTIME
     変数 :read_out_time  ---> exptime                 */
  if ( frames ) {
    if ( XISeditTiming == editmode ) {
      BnkfGetM("XIS:FRAMES:PIXBUF_FLIPTIME", sizeof(int), &used,
	       &time_frame.exptime);
    } else {
      BnkfGetM("XIS:FRAMES:EXPTIME", sizeof(int), &used, &time_frame.exptime);
    }
    BnkfGetM("XIS:FRAMES:AEDATE", sizeof(int), &used, &time_frame.aedate);
    BnkfGetM("XIS:FRAMES:WINDOW_OPTION", sizeof(int), &used, &window_option);
  }

  if ( eventbnk_fetched == ANL_FALSE ) {

    /* show processing percentage */
    if ( 0 < total_event_num_div_10 &&
	 0 == iev % total_event_num_div_10 &&
	 iev <= total_event_num - total_event_num_div_10 ) {
      anl_msg_info("\
  ... %3d%% ( %*d / %*d events )\n",
		10*(iev/total_event_num_div_10),
		total_event_digits, iev, total_event_digits, total_event_num);
    } else if ( iev == total_event_num ) {
      anl_msg_info("\
  ... 100%% ( %*d / %*d events )\n\n",
		total_event_digits, iev, total_event_digits, total_event_num);
    }

    /* read event and fill them to bank */
    iev++;
    if ( total_event_num < iev ) {
      EvsfSetM("XIS:skip event");
      allprocessed = ANL_TRUE;
      BnkfPutM("XIS:READEVENT:ALLPROCESSED", sizeof(int), &allprocessed);
      *status = ANL_SKIP;
      return;
    }
    if ( fits2bank(infitsd, column_inf, column_num, iev, max_value_size)
	 != ANL_TRUE) {
      *status = ANL_QUIT;
      return;
    }
    eventbnk_fetched = ANL_TRUE;
    if ( iev == 1 ) {
      notyetfetched = ANL_FALSE;
      BnkfPutM("XIS:READEVENT:NOTYETFETCHED", sizeof(int), &notyetfetched);
    }
#ifdef _PRINT_OUT_
    fprintf (stderr,"iev=%d eventbnk_fetched=%d\n", iev, eventbnk_fetched);
#endif
    /* save time of the previous event */
    time_prevevent = time_event;
  }

  /* handle event by event, when no FRAMES nor EXPOSURES extensions */
  if ( 0 == frames && 0 == exposures ) {
    eventbnk_fetched = ANL_FALSE;
    *status = ANL_OK;
    return;
  }

  /* bank get for time in event extension */
  /* FFFヘッダのキーワード名変更に伴い、以下のように変更
     BNK  : READ_OUT_TIME_H(L) --->  EXPTIME
     変数 :read_out_time  ---> exptime                 */
  if ( XISeditTiming == editmode ) {
    BnkfGetM("XIS:PIXBUF_FLIPTIME", sizeof(int), &used, &time_event.exptime);
  } else {
    BnkfGetM("XIS:EXPTIME", sizeof(int), &used, &time_event.exptime);
  }
  BnkfGetM("XIS:AEDATE", sizeof(int), &used, &time_event.aedate);
  BnkfGetM("XIS:EXPOSURE_SEQ_NO", sizeof(int), &used, &seqnum_event);
  BnkfGetM("XIS:EVENT_SEQ_NO", sizeof(unsigned int), &used, &event_seq_no);

  if ( frames ) {
    time_status_frame_event = check_time(&time_frame, &time_event);
  } else {	/* NO FRAMES EXTENSION */
    time_status_frame_event = TIME_EQUAL;
  }
  if ( iev == 1 ) {
    time_status_event_prevevent = TIME_2_EXCEED_1;
  } else {
    time_status_event_prevevent = check_time(&time_prevevent, &time_event);
  }

  if ( window_option != XISwindowOff ) {
    /* WINDOW mode */
    /* bank get in exposure extension */
    /* bank is valid only for window mode */
    if ( exposures ) {
      if( flag_expname_nos == ANL_FALSE ) {
	BnkfGetM("XIS:EXPOSURES:AEDATE", sizeof(int), &used, &time_exp.aedate);
	BnkfGetM("XIS:EXPOSURES:EXPTIME", sizeof(int), &used, &time_exp.exptime);
	BnkfGetM("XIS:EXPOSURES:EXPOSURE_SEQ_NO", sizeof(int), &used, &seqnum_exposure);
      } else {
	BnkfGetM("XIS:EXPOSURE:AEDATE", sizeof(int), &used, &time_exp.aedate);
	BnkfGetM("XIS:EXPOSURE:EXPTIME", sizeof(int), &used, &time_exp.exptime);
	BnkfGetM("XIS:EXPOSURE:EXPOSURE_SEQ_NO", sizeof(int), &used, &seqnum_exposure);
      }
    }
#ifdef _PRINT_OUT_
  fprintf (stderr,"In %s seq_num=%d exp_seq_num=%d \n",
	   pname, seqnum_event, seqnum_exposure);
#endif
#ifdef _PRINT_OUT_
    fprintf(stderr,"In %s %u %u %u\n", pname, time_frame.exptime,
	  time_exp.exptime, time_event.exptime);
#endif

    if ( exposures ) {
      time_status_exp_event = check_time(&time_exp, &time_event);
      seqnum_status = check_seqnum(seqnum_exposure, seqnum_event);
    } else {	/* NO EXPOSURES EXTENSION */
      time_status_exp_event = TIME_EQUAL;
      seqnum_status = SEQNUM_MATCH;
    }

    if ( time_status_exp_event == TIME_EQUAL &&
	 time_status_frame_event == TIME_EQUAL &&
	 seqnum_status == SEQNUM_MATCH ) {
      /* FRAME, EXPOSURE, and EVENT information are consistent */
#ifdef _PRINT_OUT_
      fprintf (stderr,
       "In %s window option seq_num=%d exp_seq_num=%d event_seqnum=%u\n",
		pname, seqnum_event, seqnum_exposure, event_seq_no);
#endif
      if ( time_status_event_prevevent != TIME_EQUAL && event_seq_no == 0 ) {
	/* 1st event in this FRAME */
	EvsfSetM("XIS:frame first event");
#ifdef _PRINT_OUT_
	fprintf (stderr,"%s: matched\n", pname);
	fprintf (stderr,"EVS: first event \n");
#endif
      } else if (
	( time_status_event_prevevent == TIME_EQUAL && event_seq_no == 0 ) ||
	( time_status_event_prevevent != TIME_EQUAL && event_seq_no != 0 ) ) {
	/* strange */
	if ( frames ) {
	  anl_msg_warning("\
%s: WARNING: event_seq_no and event inconsistent for %dth event\n",
		pname, iev);
	}
      } /* if time jump */
      eventbnk_fetched = ANL_FALSE;
      *status = ANL_LOOP;
    } else if ( time_status_exp_event != TIME_EQUAL ||
	        seqnum_status != SEQNUM_MATCH ) {
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
    if ( time_status_frame_event == TIME_EQUAL ) {
      /* FRAME and EVENT information are consistent */
      if ( event_seq_no == 0 ) {
	/* Frame 1st event is identified with event_seq_no==0 */
	if ( time_status_event_prevevent == TIME_EQUAL &&
	    editmode != XISeditTiming ) {
	  anl_msg_warning("\
%s: WARNING: %d-th event event_seq_no=%u but no exptime jump\n",
		   pname, iev, event_seq_no);
	}
	/* 1st event in this Frame */
	EvsfSetM("XIS:frame first event");
#ifdef _PRINT_OUT_
	fprintf (stderr,"%s: matched\n", pname);
	fprintf (stderr,"EVS: first event \n");
#endif
      }
      if ( seqnum_event != 0 && editmode != XISeditTiming ) {
	anl_msg_warning("\
%s: WARNING: strange seqnum %d for %d th event\n", pname, seqnum_event, iev);
      }
      eventbnk_fetched = ANL_FALSE;
      *status = ANL_LOOP;
    } else if ( time_status_frame_event == TIME_2_EXCEED_1 ) {
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

  EvsfSetM("XISreadEvent:OK");

  /* status is set above, ANL_LOOP or ANL_SKIP */
}

void
XISreadEvent_endrun(int *status)
{
  *status = ANL_OK;
}

void
XISreadEvent_exit(int *status)
{
  *status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; End: ***
*/
