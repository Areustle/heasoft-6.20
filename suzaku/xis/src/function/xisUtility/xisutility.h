/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:58 1999 by E. Miyata*/
/************************************************************************
  xisutility ver1.4
************************************************************************/

/*
 * 露出関連時刻取扱用構造体
 */

/*
 *   --+---+-------------+-------------------+------
 *     |   蓄積開始     (終了: Burst mode)   |
 *     |    ←----------→ explength         |
 *     |←→ expdelay                        |
 *     |                                     |
 *    exptime_prev                          exptime
 */

typedef struct xisFrameTimes {
  unsigned int exptime;		/* EXPTIME in MODINF      */
  unsigned int buftime;		/* BIFTIME in MODINF      */
  double exptimeD;	/* EXPTIME in astroe_time */
  double buftimeD;	/* BIFTIME in astroe_time */
  unsigned int exptime_prev;	/* 一画面前の exptime     */
  unsigned int buftime_prev;	/* 一画面前の buftime     */
  double exptime_prevD;	/* 一画面前の exptimeD    */
  double buftime_prevD;	/* 一画面前の buftimeD    */
  double expdelay;	/* seq.start から蓄積開始までの遅れ (Burst mode) */
  double explength;	/* Burst/Psum mode での frame/line 当たりの露出時間 */
  double framesttime;	/* フレーム開始時刻 */
  double frameendtime;	/* フレーム終了時刻 */
  int first_frame_flag; /* このクロックの最初の 1 フレーム目の時 non-0 */
} xisFrameTimes;

/* ↑
 * こいつらに関わる関数は、
 *  ・PRAM header を元に explength を埋める奴。
 *  ・exptime, buftime, expdelay, explength を元に各種時刻を埋める奴。
 *  ・フレーム開始・終了時刻を埋める奴
 *  ・撮像開始・終了時刻を返す奴
 *  ※expdelay はどうする？
 * かな、とりあえずは。
 */


/*
 * 以下、関数
 */

/************************************************************************/
/*   << FUNCTION >>                                                     */
/*      MODE INFで出力された時刻(TIのcounter値)をATRO-E timeに変更する  */
/*      ただし、ASTRO-E timeとは、2000/01/01 00:00:00 (TBD)の差の秒数   */
/*      そのうち FJT 提供の関数に取って代わられる運命…… (;_;)         */
/************************************************************************/
double
xisRdTime2AETime(
	         unsigned rdouttime	/* IN : TI counter from MODEINF */
);


/************************************************************************/
/*   << FUNCTION >>                                                     */
/*	PRAM Header 情報を元に、各種設定を行なう。			*/
/************************************************************************/
int
xisSetPramInfo (
                int           clkMode,       /* IN: Clock mode */
                int           burstExpTime,  /* IN: B_EXP_T in PRAM header */
                xisFrameTimes *fT
);

/************************************************************************/
/*   << FUNCTION >>                                                     */
/*      Frameの各種時刻を計算する。                                     */
/*    （複数のexposureが入っている時は、最初のeposureの開始時刻と       */
/*      最後のexposureの終了時刻を返す。）                              */
/*      構造体には、.exptime, .biftime, .exptime_prev, .buftime_prev,   */
/*      expdelay, explength をセットしておく事。                        */
/************************************************************************/
int
xisSetFrameTimes (
                   int    clockmode,    /* IN : CLOCK MODE    */
                   xisFrameTimes *fT    /* frameTimes: 各種時刻保持構造体 */
);

/************************************************************************/
/*   << FUNCTION >>                                                     */
/*      frameの撮像開始時刻他を使って、撮像開始時刻と露光時間を         */
/*      計算する。                                                      */
/************************************************************************/
int
xisGetExpStTime (
                   int           clockmode,
                   int           windowoption,
                   int           rawy,
                   xisFrameTimes *fT,   /* frameTimes: 各種時刻保持構造体  */
                   double        *expsttime,   /* OUT: EXPOSURE START TIME */
                   double        *exposure     /* OUT: EXPOSURE            */
);

/************************************************************************/
/*   << FUNCTION >>                                                     */
/*      撮像開始時刻とexposure timeを使って、exposure center timeを     */
/*      計算する。                                                      */
/************************************************************************/
double
xisGetExpCentTime(
	      double framesttime,    /* IN : AE TIME from MODEINF */
              double exposuretime    /* IN : AE TIME convereted   */
);


/************************************************************************/
/*   << FUNCTION >>                                                     */
/*      ピクセル値の集合 (25 とか 9 とか、観測モードで異なる) から	*/
/*      PHA 値とグレードを計算する。					*/
/************************************************************************/
int
xisGetPH(
	int editmode,	/* IN:  XISobs3x3 とか。又の名を Observation Mode */
	int *pix,	/* IN:  モードに応じた数のピクセルデータ (配列)   */
	int *pha,	/* OUT: 入力に対応した PHA 値                     */
	int grade	/* OUT: 入力に対応したイベント・グレード          */
);


/************************************************************************/
/*   << FUNCTION >>                                                     */
/*      PHA 値を受けとって、センサーとセグメントに応じた演算をして	*/
/*      PI 値を返す。							*/
/*	値が信用できない時はとりあえず 65536 を返す事とする。		*/
/************************************************************************/
double
xispha2pi(int sensor, int segment, int pha);
