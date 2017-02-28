/*
  XIStime

	2005/06/10 created by tkohmura

    v0.0  6.14  use xisRdTimeATtime to convert EXPTIME to AETIME
    v0.1  6.15  use ae_ti2time_dp  to convert EXPTIME to AETIME
    v0.2  6.16  use ae_ti2time_dp and aste_ti2time_init
                  to convert EXPTIME to AETIME
    v.0.4 10.12  use tim file to initialize time stamp
    v.0.5 10.24  bug fix for gcc-2.95
    v.0.5a 2006-07-20 M.Ozaki
	A short-life bug-fix branch for HEADAS 2007-July version.
	Use xisutility to compute times.
	Add HISTORY to the output.
    v.0.5b 2006-07-22 H. Matsumoto
	The v0.5a cannot be compiled due to a bug about `bogus_prevtime'.
	The XIStime_ana section is completely revised.
    v.0.5c 2006-08-22 Y.ISHISAKI
	EVSDEF -> EvsDef, EVSSET -> EvsfSetM
	remove unused #include "cfortran.h", "hbook.h"
    v.0.6 2007-01-30 Y.ISHISAKI
	move initialization in _bgnrun() to _init()
	use BnkGet() to get SNAPTIn, DELAYn in _init()
	fix history format in _bgnrun()
    v.0.7 2007-01-31 Y.ISHISAKI
	use aefits_write_module_history() in XISeditEventFits
    v.0.8 2007-04-25 H. Matsumoto, Y.ISHISAKI
        Taking into account Naik's report saying that
          the XIS event time precedes the HXD/PIN event time;
            1/4 win: T_PIN = T_XIS + 6s
            1/8 win: T_PIN = T_XIS + 7s
        We don't know the cause of the difference. But we correct
          it by interpreting EXPTIME as the start of exposure and
          subtracting exposure time of one window.
	Use aste_ti2time_dp() to convert TI to ASTETIME instead of
          xisRdTime2AETime.
	Return ANL_ERROR (new from aste_anl-1.80) if aste_ti2time_dp() fails.
    v.1.0 2007-05-06 Y.ISHISAKI
	fill zero time, if aste_ti2time_dp() failed
	update GTI, FRAMES, EXPOSURES, LOSTAREAS extensions
	update TSTART, TSTOP, TELAPSE, ONTIME, LIVETIME, EXPOSURE keywords
    v.1.1 2007-05-14 Y.ISHISAKI
	use PIXBUF_FLIPTIME for EXPTIME_AETIME in timing mode
	support timing mode, using xis_psum_time()
    v.1.2 2007-05-30 Y.ISHISAKI
	bug fix in compare_f_st(), p2->en -> p2->st
    v.1.3 2007-07-22 Y.ISHISAKI
	bug fix in xis_psum_time(), multiply 8s to calculate aetime
    v.1.4 2007-12-26 Y.ISHISAKI
	accept timfile=none to re-generate GTI
    v.1.5 2009-10-26 Y.ISHISAKI
	add bstgti option for burst mode
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "com.h"
#include "cli.h"
#include "pil.h"
#include "fitsio.h"
#include "aste_gti.h"
#include "aste_ti2time.h"
#include "xisTelemFormat.h"
#include "xisFitsHeaderUtil.h"
#include "xisEventFitsUtil.h"
#include "aetimeUtil.h"

static char pname[] = "XIStime";
char XIStime_version[] = "version 1.5";

#define PSUM_ACTY_DEFAULT	((XISactiveSegmentVsize - 1) / 2)

/* #define _PRINT_OUT_ */

static int editmode;
static int windowoption;
static int psum_l;

/* 1 frame に含まれる window の数。すなわち、
   full window --> windownumber = 1
    1/4 window --> windownumber = 4
    1/8 window --> windownumber = 8
   1/16 window --> windownumber = 16 */
static int windownumber;

/* SNAPTI1-16, DELAY1-16 を格納する変数
     これらは、キーワードが SNAPTI1, DELAY1 と番号 1 から始まっているので、
      配列要素は 1 ずれることに注意。つまり
      snapti[0] = SNAPTI1 -- snapti[15] = SNAPTI16
      delay[0]  = DELAY1  -- delay[15]  = DELAY16
      に数値が代入される。 */
static double snapti[16];
static double delay[16];

static TI2TIME *ttp = NULL;	/* TI --> aetime 変換用 */

typedef struct {
  int nf;
  int nalloc;
  double last_f_st;
  struct f_st_en {
    double st;
    double en;
  } *f;
  GTI_DATA gti;
} FRAME_TIMES;

FRAME_TIMES frame_times;

static struct {
  char timfile[PIL_LINESIZE];
  double gapsec;
  int bstgti;
} com;

/* EXP_CENT_AETIME の計算

     Naik's report: ver0.7 までの xistime では、
        1/4 window   T_PIN = T_XIS + 6s
        1/8 window   T_PIN = T_XIS + 7s

     セック長瀬さんは EXPTIME はframe終了時刻だと主張するが、
     この解釈にはこれまでも紆余曲折があった。
     また Naik さんのレポートによれば、EXPTIME がframe開始時刻だと
     思った方が, より HXD/PIN との時刻のずれが小さくなる。
     そこで、EXPTIME はframe開始時刻だと思ってコーディング。

     そう思うと一律に T_XIS は 8s 増えるので、今度は
          1/4 win T_PIN = T_XIS' - 2s
          1/8 win T_PIN = T_XIS' - 1s
     になる。原因はわからんが、どうも window 1 幅分だけずれるようだ。

     そこで、わざと 1 frame 分の時間を差し引く事により Naik's report
     にあうようにする。

     従って次のように計算。例えば 1/4 window mode の場合

      delay[0]snap[0]delay[1]snap[1]delay[2]snap[2]delay[3]snap[3]
     |--------|+++++|--------|+++++|--------|+++++|--------|+++++|
  EXPOSURE_SEQ_NO 0 |   SEQ_NO 1   |  SEQ_NO 2    | SEQ_NO 3

     になっている。

     したがって, EXPOSURE_SEQ_NO = x の window の撮像中心時刻は、

     exp_cent_aetime
     = exptime_aetime + Sum(i=0 to x) (delay[i]+snapti[i])
          - 0.5*snapti[x]  ... 露光中心を出す為
          - 8s/2s/1s/0.5s (for full, 1/4, 1/8, 1/16win) ... Naik's correction

     と計算する。
*/

static double
xis_calc_time(double exptime_aetime, int exposure_seq_no)
{
  int i;
  double exp_cent_aetime;

  exp_cent_aetime = exptime_aetime;
  for (i = 0; i <= exposure_seq_no; i++) {
    exp_cent_aetime += delay[i] + snapti[i];
  }
  exp_cent_aetime -= 0.5 * snapti[exposure_seq_no];
  exp_cent_aetime -= 8.0 / windownumber;	/* Naik's correction */

  return exp_cent_aetime;
}

static double
xis_inv_calc_time(double exp_cent_aetime, int exposure_seq_no)
{
  int i;

  exp_cent_aetime += 8.0 / windownumber;	/* Naik's correction */
  exp_cent_aetime += 0.5 * snapti[exposure_seq_no];
  for (i = 0; i <= exposure_seq_no; i++) {
    exp_cent_aetime -= delay[i] + snapti[i];
  }

  return exp_cent_aetime;
}

static double
xis_frame_time(double exptime_aetime, double *f_st_ret, double *f_en_ret)
{
  double f_st, f_en, aetime;

  f_st = exptime_aetime - 8.0 / windownumber;
  f_en = f_st + 8.0;
  aetime = f_st + 4.0;

  *f_st_ret = f_st;
  *f_en_ret = f_en;

  return aetime;
}

static int
xis_psum_time(unsigned fliptime, double fliptime_aetime, double s_time,
	      int rawy, int acty,
	      double *aetime, double *f_st, double *f_en)
{
#if 1
  *f_st = fliptime_aetime - 8.0;
  *f_en = fliptime_aetime;
  *aetime = *f_st + 8.0 * (rawy + 0.5) / XISactiveSegmentVsize;
#else
  /* 1 frame = 8 s, TI = 1/4096 s, PIXBUF = 1024 lines */
  /* deltaTI is TI [1/4096 s] required to shift 1 line */
  static unsigned int deltaTI = 8 * 4096 / 1024;	/* 0.0078125 s resol */
  static double delta_sec = 8 * 4096 / 1024 / 4096.0;
  static double half_delta_sec = 0.5 * (8 * 4096 / 1024) / 4096.0;
  static unsigned int extra_delayTI = 8 * 4096;		/* 8 sec delay */
  static double extra_delay_sec = 8;

  unsigned int num_extra, num_shift, detectTI;
  int istat;

  num_extra = acty / psum_l;	/* extra shift needed in the CCD */
  num_shift = 1024 - rawy + num_extra;
  detectTI = fliptime - num_shift * deltaTI - extra_delayTI;
/*  printf("num_extra=%u, num_shift=%u\n", num_extra, num_shift);
  printf("pixbuf_fpti=%u, detectTI=%u\n", pixbuf_fpti, detectTI);
  printf("num_shift=%u, delayTI=%u, extra_delayTI=%u\n", num_shift, deltaTI, extra_delayTI);*/

  istat = aste_ti2time_dp(ttp, detectTI, s_time, aetime);
  if ( istat ) {
    return istat;
  }

  *aetime += half_delta_sec;
  *f_st = fliptime_aetime - ((1024 + num_extra) * delta_sec + extra_delay_sec);
  *f_en = *f_st + 8.0;
#endif

  return 0;
}

static int
init_frame_times(FRAME_TIMES *p)
{
  int istat;

  p->nf = 0;
  p->nalloc = 1000;
  p->last_f_st = 0.0;
  p->f = malloc( p->nalloc * sizeof(*p->f) );
  if ( NULL == p->f ) {
    anl_msg_error("\
%s: frame_times malloc(nalloc=%d) error\n", pname, p->nalloc);
    p->nalloc = 0;
    return -1;
  }
  istat = aste_gti_zero(&p->gti);

  return istat;
}

static void
free_frame_times(FRAME_TIMES *p)
{
  if ( NULL != p->f ) {
    free(p->f);
  }
  aste_gti_free(&p->gti);
}

static int
add_to_gti(FRAME_TIMES *p, double f_st, double f_en)
{
  int i, nf;
  struct f_st_en *f = p->f;

  if ( f_st == p->last_f_st ) {
    return 0;
  }
  p->last_f_st = f_st;

  nf = p->nf;
  for (i = 0; i < nf; i++) {
    if ( f[i].st == f_st ) {	/* already existing frame */
      return 0;
    }
  }

  if ( p->nalloc <= nf ) {
    p->nalloc += 1000;
    p->f = f = realloc(f, p->nalloc * sizeof(*f));
    if ( NULL == f ) {
      anl_msg_error("\
%s: frame_times realloc(nalloc=%d) error\n", pname, p->nalloc);
      p->nalloc = p->nf = 0;
      return -1;
    }
  }

  f[i].st = f_st;
  f[i].en = f_en;
  p->nf++;

  return 0;
}

static int
compare_f_st(const void *p1, const void *p2)
{
  double f_st1 = ((struct f_st_en *)p1)->st;
  double f_st2 = ((struct f_st_en *)p2)->st;

  if ( f_st1 < f_st2 ) {
    return -1;
  } else if ( f_st1 > f_st2 ) {
    return +1;
  }

  return 0;
}

static int
finalize_gti(FRAME_TIMES *p)
{
  int i, iwin, ipos, igti, nf, fix_ontime;
  struct f_st_en *f;
  double st, en, ontime, *start, *stop;

  nf = p->nf;
  if ( 0 == nf ) {
    return 0;
  }

  p->nalloc = nf;
  f = p->f = realloc(p->f, nf * sizeof(*p->f));
  if ( NULL == f ) {
    anl_msg_error("\
%s: frame_times realloc(nalloc=%d) error\n", pname, p->nalloc);
    p->nalloc = p->nf = 0;
    return -1;
  }

  qsort(f, nf, sizeof(*p->f), compare_f_st);

  if ( com.bstgti ) {
    start = malloc(2 * nf * windownumber * sizeof(*start));
  } else {
    start = malloc(2 * nf * sizeof(*start));
  }
  if ( NULL == start ) {
    anl_msg_error("\
%s: gti.start malloc(nalloc=%d) error\n", pname, 2*nf);
    return -1;
  }

  if ( com.bstgti ) {

    fix_ontime = 0;
    igti = nf * windownumber;
    ontime = 0.0;
    stop = &start[igti];
    for (i = 0; i < nf; i++) {
      en = f[i].st;
      for (iwin = 0; iwin < windownumber; iwin++) {
	ipos = i*windownumber + iwin;
	st = en + delay[iwin];
	en = st + snapti[iwin];
	start[ipos] = st;
	stop[ipos] = en;
	ontime += snapti[iwin];
	if ( 0 < ipos && st <= stop[ipos-1] + com.gapsec ) {
	  stop[ipos-1] = st;
	  fix_ontime = 1;
	}
      }
    }
    if ( fix_ontime ) { /* 桁落ちが発生しやすいので fix が必要な時のみ */
      ontime = 0.0;
      for (ipos = 0; ipos < igti; ipos++) {
	ontime += stop[ipos] - start[ipos];
      }
    }

  } else {

    igti = 0;
    ontime = 0.0;
    stop = &start[nf];
    start[0] = f[0].st;
    for (i = 1; i < nf; i++) {
      if ( f[i-1].en + com.gapsec < f[i].st ) {	/* frame time jump */
	stop[igti] = f[i-1].en;
	ontime += stop[igti] - start[igti];
	igti++;
	start[igti] = f[i].st;
      }
    }
    stop[igti] = f[nf-1].en;
    ontime += stop[igti] - start[igti];
    igti++;
  }

  p->gti.ngti = igti;
  p->gti.tstart = start[0];
  p->gti.tstop = stop[igti-1];
  p->gti.telapse = stop[igti-1] - start[0];
  p->gti.ontime = ontime;
  p->gti.start = start;
  p->gti.stop = &start[igti];

  if ( p->gti.stop != stop ) {		/* number of GTI has been changed */
    for (i = 0; i < igti; i++) {
      p->gti.stop[i] = stop[i];
    }
    p->gti.start = start = realloc(start, 2 * igti * sizeof(*start));
    if ( NULL == start ) {
      anl_msg_error("\
%s: gti.start realloc(nalloc=%d) error\n", pname, 2*igti);
      p->gti.ngti = 0;
      return -1;
    }
  }

  return 0;
}

static int
update_gti(fitsfile *fp, FRAME_TIMES *p)
{
  char *k;
  int i, col_start, col_stop;
  long nrow;

  int istat = 0;

  if (
fits_get_colnum(fp, CASESEN, k="START", &col_start, &istat) ||
fits_get_colnum(fp, CASESEN, k="STOP", &col_stop, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }

  if (
fits_get_num_rows(fp, &nrow, &istat) ) {
    anl_msg_error("\
%s: fits_get_num_rows() failed (%d)\n", pname, istat);
    return istat;
  }

  if (
fits_delete_rows(fp, 1, nrow, &istat) ) {
    anl_msg_error("\
%s: fits_delete_rows(nrow=%ld) failed (%d)\n", pname, nrow, istat);
    return istat;
  }

  nrow  = p->gti.ngti;
  if (
fits_write_col_dbl(fp, i=col_start, 1, 1, nrow, p->gti.start, &istat) ||
fits_write_col_dbl(fp, i=col_stop,  1, 1, nrow, p->gti.stop,  &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_write_col(icol=%d, nrow=%ld) failed (%d)\n", pname, i, nrow, istat);
    return istat;
  }

  return 0;
}

static int
update_frames(fitsfile *fp)
{
  static int acty = PSUM_ACTY_DEFAULT;

  int col_exptime, col_flip, col_s_time;	/* input */
  int col_aedate, col_exp_aetime, col_f_st, col_f_en, col_time;	/* output */
  char *k;
  long irow, nrow;
  int i, a, aedate;
  unsigned int last_exptime, exptime, fliptime;
  double s_time, exp_aetime, f_st, f_en, aetime, flip_aetime;

  int istat = 0;

  last_exptime = 0;	/* dummy initialization for gcc warning */

  if (
fits_get_colnum(fp, CASESEN, k="EXPTIME", &col_exptime, &istat) ||
fits_get_colnum(fp, CASESEN, k="S_TIME", &col_s_time, &istat) ||
fits_get_colnum(fp, CASESEN, k="PIXBUF_FLIPTIME", &col_flip, &istat) ||
fits_get_colnum(fp, CASESEN, k="AEDATE", &col_aedate, &istat) ||
fits_get_colnum(fp, CASESEN, k="EXPTIME_AETIME", &col_exp_aetime, &istat) ||
fits_get_colnum(fp, CASESEN, k="FRAME_ST_AETIME", &col_f_st, &istat) ||
fits_get_colnum(fp, CASESEN, k="FRAME_END_AETIME", &col_f_en, &istat) ||
fits_get_colnum(fp, CASESEN, k="TIME", &col_time, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }

  if (
fits_get_num_rows(fp, &nrow, &istat) ) {
    anl_msg_error("\
%s: fits_get_num_rows() failed (%d)\n", pname, istat);
    return istat;
  }

  for (irow = 1; irow <= nrow; irow++) {
    if (
fits_read_col_uint(fp, i=col_exptime, irow, 1, 1, 0, &exptime, &a, &istat) ||
fits_read_col_uint(fp, i=col_flip, irow, 1, 1, 0, &fliptime, &a, &istat) ||
fits_read_col_dbl(fp, i=col_s_time, irow, 1, 1, 0, &s_time, &a, &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n", pname, i, irow, istat);
      return istat;
    }
    if ( 1 == irow || last_exptime != exptime ) {
      last_exptime = exptime;
      istat = aste_ti2time_dp(ttp, exptime, s_time, &exp_aetime);
      if ( istat ) {
	anl_msg_error("\
%s: aste_ti2time_dp() failed at irow=%ld (%d)\n", pname, irow, istat);
	exp_aetime = f_st = f_en = aetime = 0.0;
	aedate = 0;
      } else {
	aedate = aetime2aedate(exp_aetime);
	aetime = xis_frame_time(exp_aetime, &f_st, &f_en);
      }
    }

    if ( XISeditTiming == editmode ) {
      if (
aste_ti2time_dp(ttp, fliptime, s_time, &flip_aetime) ||
xis_psum_time(fliptime, flip_aetime, s_time, 0, acty, &aetime, &f_st, &f_en) ||
	   0 ) {
	anl_msg_error("\
%s: aste_ti2time_dp() failed at irow=%ld (%d)\n", pname, irow, istat);
	aetime = 0.0;
	aedate = 0;
      } else {
	aetime = (f_st + f_en) / 2;
	aedate = aetime2aedate(flip_aetime);
	/* use PIXBUF_FLIPTIME for EXPTIME_AETIME in timing mode */
	exp_aetime = flip_aetime;
      }
    }

    if (
fits_write_col_int(fp, i=col_aedate, irow, 1, 1, &aedate, &istat) ||
fits_write_col_dbl(fp, i=col_exp_aetime, irow, 1, 1, &exp_aetime, &istat) ||
fits_write_col_dbl(fp, i=col_f_st, irow, 1, 1, &f_st, &istat) ||
fits_write_col_dbl(fp, i=col_f_en, irow, 1, 1, &f_en, &istat) ||
fits_write_col_dbl(fp, i=col_time, irow, 1, 1, &aetime, &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_write_col(icol=%d) failed at irow=%ld (%d)\n", pname, i, irow, istat);
      return istat;
    }
  }

  return 0;
}

static int
update_exposures(fitsfile *fp)
{
  int col_exptime, col_s_time, col_seq_no;			/* input */
  int col_aedate, col_exp_aetime, col_exp_cent, col_expo, col_time;/* output */
  char *k;
  long irow, nrow;
  int i, a, aedate, seq_no;
  unsigned int exptime;
  double s_time, exp_aetime, exp_cent, expo;

  int istat = 0;

  if (
fits_get_colnum(fp, CASESEN, k="EXPTIME", &col_exptime, &istat) ||
fits_get_colnum(fp, CASESEN, k="S_TIME", &col_s_time, &istat) ||
fits_get_colnum(fp, CASESEN, k="EXPOSURE_SEQ_NO", &col_seq_no, &istat) ||
fits_get_colnum(fp, CASESEN, k="AEDATE", &col_aedate, &istat) ||
fits_get_colnum(fp, CASESEN, k="EXPTIME_AETIME", &col_exp_aetime, &istat) ||
fits_get_colnum(fp, CASESEN, k="EXP_CENT_AETIME", &col_exp_cent, &istat) ||
fits_get_colnum(fp, CASESEN, k="EXPOSURE", &col_expo, &istat) ||
fits_get_colnum(fp, CASESEN, k="TIME", &col_time, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }

  if (
fits_get_num_rows(fp, &nrow, &istat) ) {
    anl_msg_error("\
%s: fits_get_num_rows() failed (%d)\n", pname, istat);
    return istat;
  }

  for (irow = 1; irow <= nrow; irow++) {
    if (
fits_read_col_uint(fp, i=col_exptime, irow, 1, 1, 0, &exptime, &a, &istat) ||
fits_read_col_dbl(fp, i=col_s_time, irow, 1, 1, 0, &s_time, &a, &istat) ||
fits_read_col_int(fp, i=col_seq_no, irow, 1, 1, 0, &seq_no, &a, &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n", pname, i, irow, istat);
      return istat;
    }
    istat = aste_ti2time_dp(ttp, exptime, s_time, &exp_aetime);
    if ( istat ) {
      anl_msg_error("\
%s: aste_ti2time_dp() failed at irow=%ld (%d)\n", pname, irow, istat);
      exp_aetime = exp_cent = expo = 0.0;
      aedate = 0;
    } else {
      aedate = aetime2aedate(exp_aetime);
      exp_cent = xis_calc_time(exp_aetime, seq_no);
      expo = snapti[seq_no];
    }
    if (
fits_write_col_int(fp, i=col_aedate, irow, 1, 1, &aedate, &istat) ||
fits_write_col_dbl(fp, i=col_exp_aetime, irow, 1, 1, &exp_aetime, &istat) ||
fits_write_col_dbl(fp, i=col_exp_cent, irow, 1, 1, &exp_cent, &istat) ||
fits_write_col_dbl(fp, i=col_expo, irow, 1, 1, &expo, &istat) ||
fits_write_col_dbl(fp, i=col_time, irow, 1, 1, &exp_cent, &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_write_col(icol=%d) failed at irow=%ld (%d)\n", pname, i, irow, istat);
      return istat;
    }
  }

  return 0;
}

static int
update_lostareas(fitsfile *fp)
{
  int col_exptime, col_s_time;	/* input */
  int col_aedate, col_time;	/* output */
  char *k;
  long irow, nrow;
  int i, a, aedate;
  unsigned int exptime;
  double s_time, exp_aetime, f_st, f_en, aetime;

  int istat = 0;

  if (
fits_get_colnum(fp, CASESEN, k="EXPTIME", &col_exptime, &istat) ||
fits_get_colnum(fp, CASESEN, k="S_TIME", &col_s_time, &istat) ||
fits_get_colnum(fp, CASESEN, k="AEDATE", &col_aedate, &istat) ||
fits_get_colnum(fp, CASESEN, k="TIME", &col_time, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }

  if (
fits_get_num_rows(fp, &nrow, &istat) ) {
    anl_msg_error("\
%s: fits_get_num_rows() failed (%d)\n", pname, istat);
    return istat;
  }

  for (irow = 1; irow <= nrow; irow++) {
    if (
fits_read_col_uint(fp, i=col_exptime, irow, 1, 1, 0, &exptime, &a, &istat) ||
fits_read_col_dbl(fp, i=col_s_time, irow, 1, 1, 0, &s_time, &a, &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n", pname, i, irow, istat);
      return istat;
    }
    istat = aste_ti2time_dp(ttp, exptime, s_time, &exp_aetime);
    if ( istat ) {
      anl_msg_error("\
%s: aste_ti2time_dp() failed at irow=%ld (%d)\n", pname, irow, istat);
      exp_aetime = f_st = aetime = 0.0;
      aedate = 0;
    } else {
      aedate = aetime2aedate(exp_aetime);
      aetime = xis_frame_time(exp_aetime, &f_st, &f_en);
    }
    if (
fits_write_col_int(fp, i=col_aedate, irow, 1, 1, &aedate, &istat) ||
fits_write_col_dbl(fp, i=col_time, irow, 1, 1, &aetime, &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_write_col(icol=%d) failed at irow=%ld (%d)\n", pname, i, irow, istat);
      return istat;
    }
  }

  return 0;
}

static int
update_keys(int num_ext, FITS_EXT_STRUCT *fitsExt, FRAME_TIMES *p)
{
  static struct {
    char *TSTART;
    char *TSTOP;
    char *TELAPSE;
    char *ONTIME;
    char *LIVETIME;
    char *EXPOSURE;
  } c = {
    "time start",
    "time stop",
    "elapsed time = TSTOP - TSTART",
    "on time = sum of all GTIs",
    "on-source time corrected for CCD exposure",
    "exposure time",
  };

  int i;
  char *k;
  double livetime, exposure, livefrac;
  fitsfile *fp;

  int istat = 0;

  if ( com.bstgti ) {
    livefrac = 1.0;
    livetime = p->gti.ontime;
  } else {
    livefrac = 0.0;
    for (i = 0; i < windownumber; i++) {
      livefrac += snapti[i];
    }
    livefrac /= 8.0;
    livetime = livefrac * p->gti.ontime;
  }
  exposure = livetime;

  anl_msg_info("\
Updating header keywords ...\n\
\n\
%-8s = %20.6f / %s\n\
%-8s = %20.6f / %s\n\
%-8s = %20.6f / %s\n\
%-8s = %20.6f / %s\n\
%-8s = %20.6f / %s\n\
%-8s = %20.6f / %s\n\
\n",		"TSTART",   p->gti.tstart,  c.TSTART,
		"TSTOP",    p->gti.tstop,   c.TSTOP,
		"TELAPASE", p->gti.telapse, c.TELAPSE,
		"ONTIME",   p->gti.ontime,  c.ONTIME,
		"LIVETIME", livetime,       c.LIVETIME,
		"EXPOSURE", exposure,       c.EXPOSURE);

  if ( NULL == fitsExt ) {
    return 0;
  }

  for (i = 0; i < num_ext; i++) {
    fp = fitsExt[i].fitsd;
    if ( NULL == fp ) {
      continue;
    }
    if (
fits_delete_key(fp, k="LIVETIME", &istat) ||
fits_delete_key(fp, k="EXPOSURE", &istat) ||
	 0 ) {
      istat = 0;	/* ignore error */
    }
    if (
fits_update_key_fixdbl(fp, k="TSTART",  p->gti.tstart,  6, c.TSTART,  &istat)||
fits_update_key_fixdbl(fp, k="TSTOP",   p->gti.tstop,   6, c.TSTOP,   &istat)||
fits_update_key_fixdbl(fp, k="TELAPSE", p->gti.telapse, 6, c.TELAPSE, &istat)||
fits_update_key_fixdbl(fp, k="ONTIME",  p->gti.ontime,  6, c.ONTIME,  &istat)||
	 0 ) {
      anl_msg_error("\
%s: fits_update_key('%s') failed (%d)\n", pname, k, istat);
      return istat;
    }
    if (
fits_insert_key_fixdbl(fp, k="LIVETIME", livetime, 6, c.LIVETIME, &istat) ||
fits_insert_key_fixdbl(fp, k="EXPOSURE", exposure, 6, c.EXPOSURE, &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_update_key('%s') failed (%d)\n", pname, k, istat);
      return istat;
    }
  }

  return istat;
}

static int
do_psum_time(void)
{
  static int first_time = 1;

  static unsigned int last_fliptime;
  static int aedate;
  static double fliptime_aetime;

  unsigned int fliptime;
  double s_time, aetime, f_st, f_en;
  int used, istat, rawy, acty;

/* すべての時刻情報の元である、PIXBUF_FPTI と S_TIME を読み込む */
  BnkfGetM("XIS:PIXBUF_FLIPTIME", sizeof(unsigned int), &used, &fliptime);
  BnkfGetM("XIS:S_TIME", sizeof(double), &used, &s_time);
  BnkfGetM("XIS:RAWY", sizeof(int), &used, &rawy);
  BnkfGetM("XIS:ACTY", sizeof(int), &used, &acty);
  if ( -999 == acty ) {
    acty = PSUM_ACTY_DEFAULT;
  }

  if ( first_time ) {
    istat = aste_ti2time_dp(ttp, fliptime, s_time, &fliptime_aetime);
    if ( istat ) {
      aetime = f_st = f_en = 0.0;
      aedate = 0;
      goto skip;
    }
    last_fliptime = fliptime;
    aedate = aetime2aedate(fliptime_aetime);
    first_time = 0;
  }

  if ( fliptime != last_fliptime ) {
    istat = aste_ti2time_dp(ttp, fliptime, s_time, &fliptime_aetime);
    if ( istat ) {
      aetime = f_st = f_en = 0.0;
      aedate = 0;
      goto skip;
    }
    last_fliptime = fliptime;
    aedate = aetime2aedate(fliptime_aetime);
  }

  istat = xis_psum_time(fliptime, fliptime_aetime, s_time, rawy, acty,
			&aetime, &f_st, &f_en);

  if ( istat ) {
    aetime = f_st = f_en = 0.0;
    goto skip;
  }

  add_to_gti(&frame_times, f_st, f_en);

 skip:

  BnkfPutM("XIS:EXPTIME_AETIME", sizeof(double), &fliptime_aetime);
  BnkfPutM("XIS:AEDATE", sizeof(int), &aedate);
  BnkfPutM("XIS:EXP_CENT_AETIME", sizeof(double), &aetime);
  BnkfPutM("XIS:TIME", sizeof(double), &aetime);

  return istat;
}

static void
show_parameter(void)
{
  printf("\nANL:  *** %s show parameter ***\n\n", pname);
  printf("%20s   %s\n", "TIMFILE", com.timfile);
  printf("%20s   %.6f (s)\n", "GAPSEC", com.gapsec);
  printf("%20s   %s\n", "BSTGTI", com.bstgti ? "YES" : "NO");
  printf("\n");
}

void
XIStime_startup(int *status)
{
  *status = ANL_OK;
}

void
XIStime_com(int *status)
{
  static char *keytbl[] = {
    "TIMFILE",
    "GAPSEC",
    "BSTGTI",
    "SHOW",
    "EXIT"
  };
  static char *help[] = {
    "tim file list in ascii",
    "Allowed gap between frames in second",
    "Generate GTI for the burst option without approximation",
    "Show current setting",
    "Exit from this menu"
  };
  static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
  char *k, *key;
  int ans[2];

  if ( *status ) {	/* ftools */
    if (
PILGetFname(k="timfile", com.timfile) ||
PILGetReal (k="gapsec", &com.gapsec) ||
PILGetBool (k="bstgti", &com.bstgti) ||
	 0 ) {
      anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
      *status = ANL_QUIT;
      return;
    }
    *status = ANL_OK;
    return;
  }

  for (;;) {
    CMinquir(pname, nkey, keytbl, help, 1, ans);
    key = keytbl[ans[1]-1];
    if ( 0 == strcmp("TIMFILE", key) ) {
      CLtxtrd(key, com.timfile, sizeof(com.timfile));
    } else if ( 0 == strcmp("GAPSEC", key) ) {
      CLfdprd(key, &com.gapsec);
    } else if ( 0 == strcmp("BSTGTI", key) ) {
      CLlogrd(key, &com.bstgti);
    } else if ( 0 == strcmp("SHOW", key) ) {
      show_parameter();
    } else if ( 0 == strcmp("EXIT", key) ) {
      break;
    }
  }

  *status = ANL_OK;
}

void
XIStime_init(int *status)
{
  int i; 		/* loop 用 */
  int used;		/* BnkGet 用 */
  char bnkname[32];	/* SNAPTI と DELAY の BNKNAME 用 */
  int istat;		/* return value from aste_ti2time_*() functions */

  show_parameter();

  EvsDef("XIStime:BEGIN");
  EvsDef("XIStime:ENTRY");
  EvsDef("XIStime:OK");

  BnkDef("ASTE:TI2TIME:PTR", sizeof(ttp));

/* edit mode を取得 */
  BnkGet("XIS:EDITMODE", sizeof(editmode), &used, &editmode);
  anl_msg_info("\
EDITMODE = %d\n", editmode);

/* psum line を取得 */
  BnkGet("XIS:PSUM_L", sizeof(psum_l), &used, &psum_l);
  anl_msg_info("\
PSUM_L   = %d\n", psum_l);

/* window option を取得 */
  BnkGet("XIS:WINOPT", sizeof(windowoption), &used, &windowoption);
  anl_msg_info("\
WINOPT   = %d\n", windowoption);

/* window option に対応した windownumber を設定 */
  switch (windowoption) {
  case 0: /* full window */
    windownumber = 1;
    break;
  case 1: /* 1/4 window */
    windownumber = 4;
    break;
  case 2: /* 1/8 window */
    windownumber = 8;
    break;
  case 3: /* 1/16 window */
    windownumber = 16;
    break;
  default: /* no such mode */
    anl_msg_error("\
%s: ERROR: invalid window option %d!!!\n", pname, windowoption);
    goto quit;
  }

  for (i = 0; i < windownumber; i++) {
    sprintf(bnkname, "XIS:SNAPTI%d", i+1);
    BnkGet(bnkname, sizeof(double), &used, &snapti[i]);
    sprintf(bnkname, "XIS:DELAY%d", i+1);
    BnkGet(bnkname, sizeof(double), &used, &delay[i]);
    anl_msg_info("\
SNAPTI%d  = %f\n", i+1, snapti[i]);
    anl_msg_info("\
DELAY%d   = %f\n", i+1, delay[i]);
  }
  anl_msg_info("\n");

/* TI --> aetime 変換のための構造体を初期化する */
  if ( 0 == CLstricmp("none", com.timfile) ) {
    anl_msg_info("\
%s: INFO: timfile=none, only calculating GTI\n", pname);
  } else {
    istat = aste_ti2time_init(&ttp, com.timfile);
    if ( istat ) {
      anl_msg_error("\
%s: aste_ti2time_init() failed (%d)\n", pname, istat);
      goto quit;
    }
  }
  anl_msg_info("\n");

  istat = init_frame_times(&frame_times);
  if ( istat ) {
    goto quit;
  }

/* ttp は ASTE:TI2TIME:PTR という BNK に入れる */
  BnkPut("ASTE:TI2TIME:PTR", sizeof(ttp), &ttp);

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XIStime_his(int *status)
{
  *status = ANL_OK;
}

void
XIStime_bgnrun(int *status)
{
  EvsfSetM("XIStime:BEGIN");

  *status = ANL_OK;
}

void
XIStime_ana(int nevent, int eventid, int *status)
{
  unsigned int last_exptime_failed = 0;

  int used, istat;
  int aedate, seq_no;
  unsigned int exptime;
  double s_time, exp_aetime, exp_cent, f_st, f_en;

  EvsfSetM("XIStime:ENTRY");

  if ( NULL == ttp ) {	/* only re-generate GTI */
    BnkfGetM("XIS:TIME", sizeof(double), &used, &exp_cent);
    BnkfGetM("XIS:EXPOSURE_SEQ_NO", sizeof(int), &used, &seq_no);
    exp_aetime = xis_inv_calc_time(exp_cent, seq_no);
    xis_frame_time(exp_aetime, &f_st, &f_en);
    add_to_gti(&frame_times, f_st, f_en);
    goto end;
  }

/* timing mode の場合は、PIXBUF_FPTI, ACTY, RAWY を使って時刻計算 */
  if ( XISeditTiming == editmode ) {
    do_psum_time();	/* ignore time assignment errors */
    goto end;
  }

/* すべての時刻情報の元である、EXPTIME と S_TIME を読み込む */
  BnkfGetM("XIS:EXPTIME", sizeof(unsigned int), &used, &exptime);
  BnkfGetM("XIS:S_TIME", sizeof(double), &used, &s_time);
/* EXP_SEQ_NO が何番目の window で撮像されたかを表す (0から始まる)*/
  BnkfGetM("XIS:EXPOSURE_SEQ_NO", sizeof(int), &used, &seq_no);

#ifdef _PRINT_OUT_
  printf("EXPTIME = %u\n", exptime);
  printf("S_TIME = %f\n", s_time);
  printf("EXPOSURE_SEQ_NO = %d\n", seq_no);
#endif

/* S_TIME を利用して EXPTIME_AETIME を計算 */
  istat = aste_ti2time_dp(ttp, exptime, s_time, &exp_aetime);
  if ( istat ) {
    if ( last_exptime_failed != exptime ) {
      anl_msg_error("\
%s: aste_ti2time_dp() failed at exptime=%u (%d)\n", pname, exptime, istat);
      last_exptime_failed = exptime;
    }
    exp_aetime = exp_cent = 0.0;
    aedate = 0;
    goto skip;
  }

/* AEDATE を計算 */
  aedate = aetime2aedate(exp_aetime);

/* EXP_CENT_AETIME を計算 */
  exp_cent = xis_calc_time(exp_aetime, seq_no);

/* FRAME_ST_AETIME, FRAME_END_AETIME を計算 */
  xis_frame_time(exp_aetime, &f_st, &f_en);
  add_to_gti(&frame_times, f_st, f_en);

#ifdef _PRINT_OUT_
  printf("EXPTIME_AETIME = %f\n", exp_aetime);
  printf("aedate = %d\n", aedate);
  printf("evt_exp_cent_aetime = %f\n", exp_cent);
  {
    double test_evttime;
    BnkfGetM("XIS:EXP_CENT_AETIME", sizeof(double), &used, &test_evttime);
    printf("XIS:EXP_CENT_AETIME = %f\n", test_evttime);
  }
#endif

 skip:

/* 新たに計算した情報で bnk を更新 */
  BnkfPutM("XIS:EXPTIME_AETIME", sizeof(double), &exp_aetime);
  BnkfPutM("XIS:AEDATE", sizeof(int), &aedate);
  BnkfPutM("XIS:EXP_CENT_AETIME", sizeof(double), &exp_cent);
  BnkfPutM("XIS:TIME", sizeof(double), &exp_cent);

 end:
  EvsfSetM("XIStime:OK");
  *status = ANL_OK;
}

void
XIStime_endrun(int *status)
{
  fitsfile *fp;
  char history[2*PIL_LINESIZE];
  int num_ext = 0;
  FITS_EXT_STRUCT *fitsExt = NULL;

  int used = 0, istat = 0;

  finalize_gti(&frame_times);

  BnkGet("XIS:OUTFITS:GTI:PTR", sizeof(fp), &used, &fp);
  if ( used == sizeof(fp) && NULL != fp ) {
    anl_msg_info("\
Updating GTI extension ...\n");
    update_gti(fp, &frame_times);
  }

  BnkGet("XIS:OUTFITS:FRAMES:PTR", sizeof(fp), &used, &fp);
  if ( used == sizeof(fp) && NULL != fp && NULL != ttp ) {
    anl_msg_info("\
Updating FRAMES extension ...\n");
    update_frames(fp);
  }

  BnkGet("XIS:OUTFITS:EXPOSURES:PTR", sizeof(fp), &used, &fp);
  if ( used == sizeof(fp) && NULL != fp && NULL != ttp ) {
    anl_msg_info("\
Updating EXPOSURES extension ...\n");
    update_exposures(fp);
  }

  BnkGet("XIS:OUTFITS:LOSTAREAS:PTR", sizeof(fp), &used, &fp);
  if ( used == sizeof(fp) && NULL != fp && NULL != ttp ) {
    anl_msg_info("\
Updating LOSTAREAS extension ...\n");
    update_lostareas(fp);
  }

  BnkGet("XIS:OUTFITS:NUM_EXT", sizeof(int), &used, &num_ext);
  BnkGet("XIS:OUTFITS:FITS_EXT:PTR", sizeof(fitsExt), &used, &fitsExt);
  update_keys(num_ext, fitsExt, &frame_times);

  BnkGet("XIS:OUTFITS:EVENTS:PTR", sizeof(fp), &used, &fp);
  if ( used != sizeof(fp) || NULL == fp ) {
    *status = ANL_OK;
    return;
  }

  if ( NULL == ttp ) {
    sprintf(history, "\
  timfile=none");
  } else {
    sprintf(history, "\
  timfile='%s'", com.timfile);
  }
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  gapsec=%.6f  bstgti=%s", com.gapsec, com.bstgti ? "yes" : "no");
  fits_write_history(fp, history, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

void
XIStime_exit(int *status)
{
  free_frame_times(&frame_times);

  *status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; c-basic-offset:2  ***
;;; End: ***
*/
