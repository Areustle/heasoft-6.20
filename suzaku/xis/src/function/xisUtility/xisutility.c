/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:58 1999 by E. Miyata*/
/********************************************************

  xis utility
	ver 0.1   96/09/02 created  by awaki
         xisRdTime2AEtime
         xisGetFrameStTime
         xisGetExpStTime
         xisGetExpCentTime
         xisrawx2actx
         xisrawy2acty
      ver 0.1a  96/09/05 modified by awaki
      ver 0.2   96/09/10 modified by awaki
        xisrawx2actx & xisrawy2acty -> xisraw2act
      ver 0.3   96/09/26 modified by awaki
      ver 0.3a  97/07/10 modified by awaki
                 xisRdTime2AETime (rdoutime/4096.0)
      ver 0.4  97/12/05 modified by E. Miyata
		不必要な pointer 渡しをやめた
		返り値をつけた。エラー処理の強化。
	ver 0.5  98/04/02 modified by M. Ozaki
		時刻構造体の定義と導入
		座標変換関数を別ライブラリ (xisJCoord) に
	ver 0.6  98/07/28 modified by M. Ozaki
		PSum モードの撮像周期を 8 秒に固定
	ver 0.7  99/05/13 modified by H. Awaki
		2x2 modeに対応
	ver 0.8  99/08/12 modified by dE. Miyata
		xisRdTime2AETime で CCSDS から
		読むように変更した
	ver 0.9  00/01/27 modified by M. Ozaki
		xisSetPramInfo で必ず explength を埋める
		xisSetFrameTimes で 1 画面目の特殊扱い
	ver 0.10 00/01/28 modified by M. Ozaki
		xisSetFrameTimes で NormalClock の
		exposure start time を決め打ち
	ver 0.92 2000/01/28 modified by E. Miyata
		fix 恥ずかしい bug
	ver 0.93 2000/02/05 modified by H. Awaki
		xisRdTime2AETimeの修正
	ver 0.94 2000/02/09 modified by M. Ozaki
		xisRdTime2AETimeの修正
		(TI counter の一回りに対応)
	ver 0.95 2000/02/09 modified by M. Ozaki
		xisRdTime2AETimeの修正
		(計算の原点に負の offset を付ける)
	ver 0.96 2004/02/06 modified by M. Ozaki
		xisRdTime2AETimeの修正
	ver 0.97 ??? by M. Ozaki
		(include missing math.h)
	ver 0.98 2005/02/23 modified by H. Matsumoto
		xisRdTime2AETimeの修正
	ver 0.99 2005/02/23 modified by M. Ozaki
		xisRdTime2AETimeの修正
	ver 1.0  2005/05/25 modified by H. Matsumoto
              aste_ti2time の 変更に対応
              xisRdTime2AETime の変更
	ver 1.1 2005/6/14 H. Matsumoto
	      xisSetFrameTimes の変更
	ver 1.2 2005/7/8 H. Matsumoto
	      xisutility.h をこのディレクトリに移動
	ver 1.3 2005/10/31 Y.ISHISAKI
	      exit() 未定義のため #include <stdlib.h> を追加
	      initialize aetime = 0.0 in xisRdTime2AETime()
        ver 1.4 2006/09/15 Hironori Matsumoto
              xisFrameTime の、exptime, buftime, exptime_prev, 
              buftime_prev の定義を unsigned int に変更。

********************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include "anl.h"
#include "bnk.h"
#include "xisTelemFormat.h"
#include "xisutility.h"
#include "aste_ti2time.h"

#define EXPOSURE_DEFAULT	8.0	/* 1 画面のデフォルト exposure time */

#define TI_PRECISION		32	/* TI counter の桁数 */
#define TI_SUBSEC_PREC		12	/* TI counter の sub second の桁数 */

/***********************************************************************
  << FUNCTION >>
  MODE INFで出力された時刻(TIのcounter値)をAstro-E timeに変更する
  ただし、ASTRO-E timeとは、2000/01/01 00:00:00 (TBD)の差の秒数
  このような機能は aste_ti2time で実現されるようになった。

  特に、上流に AEpacketTimeSet が組み込まれている場合には、
  BNK ASTE:PACKET_AETIME に入っている時間は、すでに高精度の
  計算を通した時刻付けが行なわれた後の値である。

  本当はこの関数は必要ない。しかし、この関数を必要としている
  モジュールがあったときに困るので、一応残しておく。

  << VERSION >>
  version 0.0          H. Awaki    96/09/02
  version 0.1          M. Ozaki	 98/04/01  戻り値が astroe_time
  version 0.2          E. Miyata	 99/08/12  CCSDS2nd を使う
  version 0.3          H. Awaki	 00/02/05  CCSDS2nd を本当に使う
  version 0.4          M. Ozaki	 00/02/09  TI counter roundup
  version 0.5          M. Ozaki	 00/02/09  TI counter roundup
  version 0.6          M. Ozaki	 03/02/17  TI counter roundup
  for IA32 architecture
  version 0.7          H. Matsumoto 05/02/23  TI counter roundup
  version 0.99         M. Ozaki    05/02/23  using astetool
  version 1.0          H. Matsumoto 05/05/25
  新しい aste_ti2time に適応させる。

  ***********************************************************************/
double
xisRdTime2AETime(
	         unsigned rdouttime /* IN : TI counter from MODEINF */
		 )
{
  static int       get_flag=0;	/* 一度だけ初期化するためのフラグ */
  static TI2TIME*  ttpp = 0;
  double aetime = 0.0;		/* return value */
  double s_time;		/* sirius time used by aste_ti2time_dp() */
  int    size;			/* parameter for BnkfGetM: unused in this function, however */
  int    r;			/* return value from aste_ti2time_*() functions */

  /* TI --> aetime 変換のための構造体を初期化する。変数 get_flag を利用
     して一回だけ行なう。 */

  if(!get_flag) {
    char* tim_file = 0;		/* time packet FITS file name.
				   とりあえず NULL にしておく。*/
    int index;			/* BnkKey 用 */

    /* もしすでに "ASTE:TI2TIME:PTR" という BNK が定義されていれば、
       すでに初期化は行なわれているので、その情報を使う */
    if (ANL_OK == BnkKey("ASTE:TI2TIME:PTR", &index)) {
      BnkfGetM("ASTE:TI2TIME:PTR", sizeof(ttpp), &size, &ttpp);
    }
    else {
      /* まだ初期化が行なわれていなければ、ここで行なう。
	 ただしこの場合、timfile は読み込まないので、正しい変換は無理 */
      while ((r = aste_ti2time_init(&ttpp, tim_file)) != 0){
	if (r == -1){		/* malloc() error */
	  fprintf(stderr, "XISUTILITY: xisRdTime2AETime: malloc failed. trying again...\n");
	  sleep (1);
	} else {
	  fprintf(stderr, "XISUTILITY: xisRdTime2AETime: aste_ti2time_init() returned %d.  Aborting.\n", r);
	  exit (1);
	}
      }
    }
    get_flag=1;			/* 初期化終了 */

  }


  /* TI --> aetime 変換には、TI の繰り上がりに対処するため、S_TIME も必要 */
  BnkfGetM("ASTE:PACKET_S_TIME", sizeof(s_time), &size, &s_time);


  /* s_time を利用して、TI を AETIME に変換 */
  if ((r = aste_ti2time_dp(ttpp, rdouttime, s_time, &aetime)) != 0){
    fprintf(stderr, "XISUTILITY: xisRdTime2AETime: aste_ti2time_dp() returned %d\n", r);
  }

  return aetime;
}


int
xisSetPramInfo (
		int           clkMode, /* IN: Clock mode */
		int           burstExpTime, /* IN: B_EXP_T in PRAM header */
		xisFrameTimes *ft
		)
/************************************************************************
   << FUNCTION >>
      PRAM Header 情報を元に、各種設定を行なう。
      とりあえずのところ、burst mode での露出時間を読んでるだけ。
	(注意: 下位 5 bit が小数点以下を表すと勝手に仮定している。)
	Normal clock では EXPOSURE_DEFAULT を入れている。

   << VERSION >>
      version 0.0          M. Ozaki	 98/04/02
      version 0.1          M. Ozaki	 98/07/28
      version 0.2          M. Ozaki	 00/01/27
************************************************************************/
{
  switch (clkMode) {
#if 0
    /* burstExpTime は HK 項目なので、メインデータと 8 秒のずれが生じる。*/
    /* これによる出力の混乱を防ぐため、とりあえず常に 8 秒の決め打ちにする。*/
  case XISclockBurst:
    ft->explength = ((double)burstExpTime) / (1<<5);
    break;
#endif
  case XISclockPsum:
    ft->explength = EXPOSURE_DEFAULT / 1024.0; /* 撮像周期を 8 秒にきめ打ち */
    break;
  default:
    ft->explength = EXPOSURE_DEFAULT;
    break;
  }
  return ANL_TRUE;
}


int
xisSetFrameTimes (
		  int    clockmode, /* IN : CLOCK MODE    */
		  xisFrameTimes *ft /* frameTimes: 各種時刻保持構造体 */
		  )
/*********************************************************************
   << FUNCTION >>
      Frameの各種時刻を計算する。
    （複数のexposureが入っている時は、最初のeposureの開始時刻と
      最後のexposureの終了時刻を返す。）
	構造体には、.exptime, .biftime, .exptime_prev, .buftime_prev,
	expdelay, explength をセットしておく事。

   << VERSION >>
	version 0.0          M. Ozaki    98/03/31
	version 0.1          M. Ozaki    98/07/28
	version 0.2          M. Ozaki    00/01/27
	version 0.3          M. Ozaki    00/01/28
	version 0.4          H. Matsumoto   05/06/01
	version 0.5          H. Matsumoto   05/06/14
***********************************************************************/
{
  int bogus_prevtime = 0;
  double exptime_prevD, buftime_prevD;

  /* ft->exptime_prev に 0 が入っていた時の特別処理 */
  /* これは最初の exposure に関してのみ起こる */
#if 0				/* Ver 0.2 での変更点 */
  bogus_prevtime = (ft->exptime_prev == 0) ? 1 : 0;
#else
  bogus_prevtime = (ft->first_frame_flag) ? 1 : 0;
#endif

  /* 最初のフレームは exptime = 0 になっているので、それを aetime に変更
     しても意味がない。そこで、その場合は aetime = -99 を返すようにする。*/

  if (ft->first_frame_flag == 1 && ft->exptime == 0) {
    ft->exptimeD      = -99;
  }
  else {
    ft->exptimeD      = xisRdTime2AETime (ft->exptime);
  }

  /* bogus_prevtime == 1 になっているときは、exptime_prev には意味がない
     値 (0) が入っている。これをそのまま aetime に変換しても意味がないので、
     この場合は -99 を返すようにする */

  if (bogus_prevtime == 1) {
    ft->exptime_prevD = -99;
  }
  else {
    ft->exptime_prevD = xisRdTime2AETime (ft->exptime_prev);
  }


  /* pixel buffer time には、timing mode 以外の時は通常 0 が入っている。
そのときは aetime としても -99 を返すように設定 */
  if(clockmode == XISclockPsum && ft->buftime > 0) {
    ft->buftimeD      = xisRdTime2AETime (ft->buftime);
  }
  else {
    ft->buftimeD = -99;
  }

  if(clockmode == XISclockPsum && ft->buftime_prev > 0) {
    ft->buftime_prevD = xisRdTime2AETime (ft->buftime_prev);
  }
  else {
    ft->buftime_prevD = -99;
  }

  exptime_prevD = (bogus_prevtime)
    ? ft->exptimeD - EXPOSURE_DEFAULT
      : ft->exptime_prevD;
  buftime_prevD = (bogus_prevtime)
    ? ft->buftimeD - EXPOSURE_DEFAULT
      : ft->buftime_prevD;

  /** ↓↓ とりあえずの暫定処置。呼出時に与える環境が整ったら削除 **/
  ft->expdelay = 0;
  /** ↑↑ とりあえずの暫定処置。呼出時に与える環境が整ったら削除 **/
  switch ( clockmode ) {
  case XISclockNormal:

    ft->framesttime  = ft->exptimeD;
    ft->frameendtime = ft->exptimeD + ft->explength;
    break;
  case XISclockBurst:
    ft->framesttime  = exptime_prevD + ft->expdelay;
    ft->frameendtime = ft->framesttime + ft->explength;
    break;
  case XISclockPsum:
    ft->framesttime  = buftime_prevD - ft->explength;
    ft->frameendtime = ft->buftimeD - ft->explength;
    break;
  default:
    fprintf (stderr, "XISUTILITY: Unknown clock mode -- %d\n", clockmode);
    return ANL_FALSE;
  }

  return ANL_TRUE;
}


int
xisGetExpStTime (
		 int           clockmode,
		 int           windowoption,
		 int           rawy,
		 xisFrameTimes *ft, /* frameTimes: 各種時刻保持構造体  */
		 double        *expsttime, /* OUT: EXPOSURE START TIME */
		 double        *exposure /* OUT: EXPOSURE            */
		 )
/************************************************************************
   << FUNCTION >>
      frameの撮像開始時刻他を使って、撮像開始時刻と露光時間を
      計算する。

   << VERSION >>
      version 0.0          H. Awaki    96/09/02
      version 0.1          H. Awaki    96/09/03   bug fix
************************************************************************/
{
  register int bogusExposure;

  switch ( clockmode ) {
  case XISclockBurst:
    *exposure = ft->explength;
    goto WindowCase;		/* ……すぐ下へ jump */
  case XISclockNormal:
    *exposure = ft->frameendtime - ft->framesttime;
  WindowCase:			/* ……XISclockNormal からの jump 先 */
    bogusExposure = ((*exposure > (EXPOSURE_DEFAULT + EXPOSURE_DEFAULT*2)/2) || (*exposure <= 0))
      ? 1 : 0;
    /* 露出時間が長過ぎる/短過ぎるようだと間違ってる可能性あり */
    switch ( windowoption ) {
    case XISwindowOff:
      *expsttime = ft->framesttime;
      break;
    case XISwindow4:
      *exposure /= 4;
      *expsttime = (rawy/(1024/4))*(*exposure) + ft->framesttime;
      break;
    case XISwindow8:
      *exposure /= 8;
      *expsttime = (rawy/(1024/8))*(*exposure) + ft->framesttime;
      break;
    case XISwindow16:
      *exposure /= 16;
      *expsttime = (rawy/(1024/16))*(*exposure) + ft->framesttime;
      break;
    default:
      fprintf (stderr, "XISUTILITY: Unknown window option -- %d\n",
	       windowoption);
      return ANL_FALSE;
    }
    break;

  case XISclockPsum:
    *exposure  = ft->explength;
    *expsttime = ft->framesttime + (*exposure * rawy);
    bogusExposure = 0;		/* 露出時間は常に正しいとする */
    break;

  default:
    fprintf (stderr, "XISUTILITY: Unknown clock mode -- %d\n", clockmode);
    bogusExposure = 1;		/* 露出時間は正しくない */
  }

  return (bogusExposure ? ANL_FALSE : ANL_TRUE);
}


double
xisGetExpCentTime(
		  double framesttime, /* IN : AE TIME from MODEINF */
		  double exposuretime /* IN : AE TIME convereted   */
		  )
/************************************************************************
   << FUNCTION >>
      撮像開始時刻とexposure timeを使って、exposure center timeを
      計算する。

   << VERSION >>
      version 0.0          H. Awaki    96/09/02
      version 0.1          M. Ozaki    98/04/02 結果を返り値とする
************************************************************************/
{
  return (framesttime + (exposuretime * 0.5));
}


int
xisGetPH(
	 int editmode,		/* IN:  XISedit3x3 とか。又の名を Observation Mode */
	 int *pix,		/* IN:  モードに応じた数のピクセルデータ (配列)    */
	 int *pha,		/* OUT: 入力に対応した PHA 値                      */
	 int grade		/* OUT: 入力に対応したイベント・グレード           */
	 )
/************************************************************************
   << FUNCTION >>
      ピクセル値の集合 (25 とか 9 とか、観測モードで異なる) から
      PHA 値とグレードを計算する。

   << VERSION >>
      version 0.0          M. Ozaki    98/04/06
************************************************************************/
{
  int *pixtmp;

  switch (editmode){
  case XISedit3x3:		/* とりあえず全 pixel を加算して PHA とする */
    *pha = 0;
    for (pixtmp = pix + 3*3 - 1; pixtmp >= pix; pixtmp--){
      *pha += *pixtmp;
    }
    break;
  case XISedit5x5:		/* とりあえず全 pixel を加算して PHA とする */
    *pha = 0;
    for (pixtmp = pix + 5*5 - 1; pixtmp >= pix; pixtmp--){
      *pha += *pixtmp;
    }
    break;
  case XISedit2x2:		/* とりあえず全 pixel を加算して PHA とする */
    *pha = 0;
    for (pixtmp = pix + 2*2 - 1; pixtmp >= pix; pixtmp--){
      *pha += *pixtmp;
    }
    break;
  case XISeditTiming:
    *pha = *pix;
    break;
  default:
    fprintf (stderr, "XISUTILITY: xisGetPH: illegal obs mode -- %d\n", editmode);
    return ANL_FALSE;
  }

  return ANL_TRUE;
}


double
xispha2pi(int sensor, int segment, int pha)
/************************************************************************
   << FUNCTION >>
      PHA 値を受けとって、センサーとセグメントに応じた演算をして
      PI 値を返す。
	値が信用できない時はとりあえず 65536 を返す事とする。

   << VERSION >>
      version 0.0          M. Ozaki    98/04/06
************************************************************************/
{
  double pi;

  pi = (double)pha;

  return (pi);
}
