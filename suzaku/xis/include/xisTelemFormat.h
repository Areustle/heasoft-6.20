/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 21:33:25 1999 by E. Miyata*/

/*************************************************************************
  
  Astro-E XIS   E. Miyata@OsakaUniv.		 

  ver 2.32  2006/4/26 H. Matsumoto
  構造体 XISeventDataTiming に pixBufFlipTime_aetime のフィールドを追加

  ver 2.31  2005/8/31 H. Matsumoto
  構造体 XISeventDataDark に detx,dety のフィールドを追加

  ver 2.30  2005/6/14 H. Matsumoto
  XISmodeInf で exptime のフィールド追加
  XISeventData*** の stime_of_exptime を stime_of_ccsdspacket に名前変更

  ver 2.20 2005/06/07    H. Matsumoto			 
  XISeventData*** に stime_of_exptime のフィールドを追加	 
  XISloasArea に stime_of_exptime のフィールドを追加
  XISeventDataDark に ppux, ppuy のフィールドを追加
  
  ver 2.10 updated 2000/01/17     T.Tsuru                  
  struct XISexpInfDark に procon                   
  tupdateSeqNo,trejectPixel,treplacePixel を追加   
  
  ver 2.00 updated 1999/05/19     T.Tsuru                  
  完全に 2x2 mode に移行                           
  ver 1.9 updated 1999/03/08      H.Awaki                  
  2x2 modeを追加                                   
  ver 1.83 updated 1998/10/21     T. Tsuru                 
  各Edit Mode、Clock Mode に3文字の短縮名を定義。  
  
  ver 1.82 updated 1998/09/07     T. Tsuru                 
  XISenergyPhChSizeDFr                             
  ver 1.81 updated 1998/08/31     T. Tsuru                 
  Main Data のAPIDを追加                           
  ver 1.8 updated 1998/08/01     E. Miyata                 
  compact-type struct 削除			 
  detx/dety/x/y を double -> int に変更		 
  ver 1.75 updated 1998/04/03     H. Awaki                 
  ExpInfの内容も従来のものから変更があったので     
  これを反映させる。                               
  参照した文献                                     
  「ASTRO-E XIS-DEテレメトリ・コマンド仕様書」v1.4  
  ver 1.74 updated 1998/04/01     H. Awaki                 
  ModeInfの内容が従来のものから変更があったので    
  これを反映させる。                               
  ver 1.73 updated 1998/04/01	H. Awaki		 
  XISbitUncompress後のpixel dataの構造体に         
  segmentを追加しました。                          
  ver 1.72 updated 1997/12/03	E. Miyata		 
  #ifndef & #define _XIS_TELEMETRY_FORMAT_	 
  XISeditModeNum XISclockModeNum			 
  XISwindowOptionNum 追加				 
  window option に 1/8 を追加			 
  ver 1.71 updated 1997/06/30	E. Miyata		 
  #define XIScopiedPixelSizeNormal 		 
  #define XIScopiedPixelSizePsum			 
  を追加した					 
  ver 1.7  updated 1997/06/15	T. Tsuru		 
  #define  XISenergyPhChSize 4096 を加えた                
  ver 1.6  updated 1996/10/11	H. Awaki		 
  ver 1.52 updated 1996/10/05	T. Tsuru		 
  ver 1.51 updated 1996/09/18	T. Tsuru		 
  ver 1.5  updated 1996/09/12	T. Tsuru		 
  ver 1.41 updated 1996/09/12	T. Tsuru		 
  ver 1.4  updated 1996/09/10	T. Tsuru		 
  ver 1.3  updated 1996/09/10	T. Tsuru		 
  ver 1.2  updated 1996/09/09	T. Tsuru		 
  ver 1.1  updated 1996/08/29	T. Tsuru		 
  ver 1.0  updated 1996/08/29	T. Tsuru		 
  ver 0.5  updated 1996/08/28	T. Tsuru		 
  ver 0.4  updated 1996/08/20	H. Awaki		 
  ver 0.3  updated 1996/08/14	T. Tsuru		 
  ver 0.2  updated 1996/08/14	H. Awaki		 
  ver 0.1  updated 1996/08/14	T. Tsuru		 
  ver 0.0  updated 1996/08/14	Emi Miyata		 
  
  **********************************************************************/

#ifndef _XIS_TELEMETRY_FORMAT_
#define _XIS_TELEMETRY_FORMAT_

/****************************************************************
 *	                   APID                                 *
 *****************************************************************/
#define XIS_DT_0A  0x100       /* XIS0 Segment A */
#define XIS_DT_0B  0x101       /* XIS0 Segment B */
#define XIS_DT_0C  0x102       /* XIS0 Segment C */
#define XIS_DT_0D  0x103       /* XIS0 Segment D */

#define XIS_DT_1A  0x104       /* XIS1 Segment A */
#define XIS_DT_1B  0x105       /* XIS1 Segment B */
#define XIS_DT_1C  0x106       /* XIS1 Segment C */
#define XIS_DT_1D  0x107       /* XIS1 Segment D */

#define XIS_DT_2A  0x108       /* XIS2 Segment A */
#define XIS_DT_2B  0x109       /* XIS2 Segment B */
#define XIS_DT_2C  0x10A       /* XIS2 Segment C */
#define XIS_DT_2D  0x10B       /* XIS2 Segment D */

#define XIS_DT_3A  0x10C       /* XIS3 Segment A */
#define XIS_DT_3B  0x10D       /* XIS3 Segment B */
#define XIS_DT_3C  0x10E       /* XIS3 Segment C */
#define XIS_DT_3D  0x10F       /* XIS3 Segment D */

/****************************************************************
 *		No. of XISs and Segements			*
 *****************************************************************/
/*		Total XIS No in ASTRO-E				*/
#define XIStotalXISno	4	/* ASTRO-Eには4つXISがある	*/

/*		Total XIS No in ASTRO-E				*/
#define XIStotalPPUno	2	/* ASTRO-Eには2つPPUがある PPU01, PPU12	*/

/*		Total XIS No in ASTRO-E				*/
#define XIStotalAETCEno	2	/* ASTRO-Eには2つAETCEがあるAETCE01,AETCE12 */

/*		Total Segemnt No in 1 XIS			*/
#define XIStotalSegNo	4	/* 1CCDには4つSegementがある	*/

/****************************************************************
 *		BIN No. of ADC in AE/TCE			*
 *****************************************************************/
/*       AE/TCEエネルギー出力のチャンネルサイズ 12bits=4096ch */
#define XISenergyPhChSize               4096
/*       Dark Frame Mode の時のみ 16bitになる。               */
#define XISenergyPhChSizeDFr            65536

/****************************************************************
 *		Coordinate systems				*
 *****************************************************************/
/*		active frame					*/
#define	XISactiveFrameVsize		1024
#define	XISactiveFrameHsize		1024

/*		active segment					*/
#define	XISactiveSegmentVsize		1024
#define	XISactiveSegmentHsize		256

/*		Pixel RAM 上での座標				*/
#define	XISpixelRamVsize		1024
#define	XISpixelRamHsize		276

/*		Copied Pixel					*/
#define XIScopiedPixelSize		2

/*	added in ver1.71					*/
#define XIScopiedPixelSizeNormal	XIScopiedPixelSize
#define XIScopiedPixelSizePsum		1

/*		[Over Under]-clocked area			*/
#define	XISoverclockSize		16
#define	XISunderclockSize		4

/****************************************************************
 *		Sensor & segment ID for XIS			*
 *****************************************************************/
#define	XIS_S0A			0x0
#define	XIS_S0B			0x1
#define	XIS_S0C			0x2
#define	XIS_S0D			0x3

#define	XIS_S1A			0x4
#define	XIS_S1B			0x5
#define	XIS_S1C			0x6
#define	XIS_S1D			0x7

#define	XIS_S2A			0x8
#define	XIS_S2B			0x9
#define	XIS_S2C			0xa
#define	XIS_S2D			0xb

#define	XIS_S3A			0xc
#define	XIS_S3B			0xd
#define	XIS_S3C	       		0xe
#define	XIS_S3D			0xf

/****************************************************************
 *		Pixel No in one event				*
 *****************************************************************/

#define XISoneEventPixelTotNo5x5   25
#define XISoneEventPixelTotNo3x3   9
#define XISoneEventPixelTotNo2x2   4

/****************************************************************
 *		Light Leak Partition				*
 *****************************************************************/
#define XISsegLightLeakPerX        4    /* 1segment でのLight Leak */
#define XISsegLightLeakPerY       16    /* Partition は 4x16       */

#define XISframeLightLeakPerX     16    /* 1 frame でのLight Leak */
#define XISframeLightLeakPerX     16    /* Partition は 16x16     */

/****************************************************************
 *		ModeINF						*
 *            1998/04/01 変更        Awaki                       *
 *         「ASTRO-E XIS-DE テレメトリ・コマンド仕様書 V1.4」    *
 *****************************************************************/
#define	XISmodeInfCSize		11

typedef struct {
  int editMode;			/* edit mode */
  int clockMode;		/* clocking mode */
  int windowOption;		/* window option */
  int lineSeqNo;		/* packet の先頭 #line */

  /* MODINF EXPTIME
     normal mode なら露光開始時刻に相当 */
  unsigned int exptime; 
  double exptime_aetime;	/* exptime を ASTRO-E time に変換したもの。*/

  /* 以前は MODINF EXPTIME をイベント読み出し時刻と誤解しており、その
     ため readOutTime という、全く間違った名前のフィールドに代入さ
     れていた。他のソフトウェアへのコンパチビリティのために残すが、今
     後は MODINF EXPTIME の意味では使用しないこと */
  unsigned int readOutTime;	
  double readOutAetime;		/* EXPTIME の ASTRO-E Time */

  unsigned int pixBufFlipTime;	/* PPUのpixel bufferが反転した時刻を表す */
} XISmodeInf;


/*		Offset from base address			*/
/*          以下は変更しません。  Awaki                         */
#define	XISmodeInfOffsetEdit	68
#define	XISmodeInfOffsetClock	66
#define	XISmodeInfOffsetWindow	64
#define	XISmodeInfOffsetLine	48
#define	XISmodeInfOffsetFrame	32
#define	XISmodeInfOffsetTime	0

/*		Edit Mode					*/
/*    modified by H.Awaki (1996/10/11)                          */
/*    Add Short Name by T.Tsuru (1998/10/21)                    */
/*    Add 2x2 mode   by H.Awaki (1999/03/08)                    */
#define	XISeditModeNum		9
#define	XISedit5x5		0x0
#define XISedit3x3		0x1
#define	XISedit2x2		0x2
#define	XISeditTiming		0x3
#define	XISeditDarkInit		0x4
#define	XISeditDarkUpdate	0x5
#define	XISeditFrame		0x6
#define	XISeditDarkFrame	0x7
#define	XISeditStandBy          0x8

#define XISedit5x5SName         "5x5"
#define XISedit3x3SName		"3x3"
#define	XISedit2x2SName 	"2x2"
#define	XISeditTimingSName	"Tim"
#define	XISeditDarkInitSName	"DIn"
#define	XISeditDarkUpdateSName	"DUp"
#define	XISeditFrameSName	"Frm"
#define	XISeditDarkFrameSName	"DFr"
#define	XISeditStandBySName     "Sby"
#define XISeditUnknownSName     "???"

/*		Clocking Mode					*/
#define	XISclockModeNum		3
#define	XISclockNormal		0x0
#define	XISclockPsum		0x1
#define	XISclockBurst		0x2

#define	XISclockNormalSName	"Nrm"
#define	XISclockPsumSName	"Psm"
#define	XISclockBurstSName	"Bst"
#define XISclockUnknownSName    "???"

/*		Window option					*/
#define	XISwindowOptionNum	4
#define	XISwindowOff		0x0
#define	XISwindow4		0x1
#define	XISwindow8		0x2
#define	XISwindow16		0x3

#define	XISwindowOffSName	"Off"
#define	XISwindow4SName		"1/4"
#define	XISwindow8SName		"1/8"
#define	XISwindow16SName	"1/16"
#define	XISwindowUnknownSName	"????"

/****************************************************************
 *		EXPINF						*
 *****************************************************************/
/*		5x5, 3x3, 2x2					*/
#define	lightLeakAreaNumber	64
#define	XISexpInfMajorCSize	144

typedef struct {
  int areaDiscriEna[XIStotalSegNo]; /* area discri ena	*/
  int gradeDiscriEna;		/* grade discri ena	*/
  int eventBufferID[XIStotalSegNo]; /* event buffer ID	*/
  int pixelBufferID[XIStotalSegNo]; /* pixel buffer ID	*/
  int hotPOvf[XIStotalSegNo];	/* hot pixel over flow	*/
  int timeOut[XIStotalSegNo];	/* PPU time OUt 	*/
  int evramFull[XIStotalSegNo];	/* event ram full	*/
  int otherErr[XIStotalSegNo];	/* other error	*/
  int aChkErr[XIStotalSegNo];	/* A-Chk Err	*/
  int fSyncErr[XIStotalSegNo];	/* Frame Sync Err	*/
  int lSyncErr[XIStotalSegNo];	/* Line Sync Err	*/
  int pixCpErr[XIStotalSegNo];	/* Pixel Code Parity Err   */
  int insideEvThPixel[XIStotalSegNo]; /* ev.th. の範囲内の #event*/
  int eventTotNo[XIStotalSegNo]; /* 検出されたイベント数	*/
  int aboveEvThUpper[XIStotalSegNo]; /* ev.th.up.を越えた#event*/
  int bias[XIStotalSegNo];	/* H-overcl.pixels の平均値 */
  int lastLine[XIStotalSegNo];	/* 処理した最後のライン番号 */
  int lightLeak[XIStotalSegNo][XISsegLightLeakPerY][XISsegLightLeakPerX];
  /* 光洩れ量 		*/
} XISexpInfMajor;

#define	XISexpInf5x5		XISexpInfMajor
#define	XISexpInf3x3		XISexpInfMajor
#define	XISexpInf2x2		XISexpInfMajor

/*		Timing mode					*/
#define	XISexpInfTimingCSize	16

typedef struct {
  int areaDiscriEna[XIStotalSegNo]; /* area discri ena	*/
  int gradeDiscriEna;		/* grade discri ena 	*/
  int eventBufferID[XIStotalSegNo]; /* event buffer ID 	*/
  int pixelBufferID[XIStotalSegNo]; /* pixel buffer ID	*/
  int hotPOvf[XIStotalSegNo];	/* hot pixel over flow	*/
  int timeOut[XIStotalSegNo];	/* PPU time OUt 	*/
  int evramFull[XIStotalSegNo];	/* event ram full	*/
  int otherErr[XIStotalSegNo];	/* other error	*/
  int aChkErr[XIStotalSegNo];	/* A-Chk Err	*/
  int fSyncErr[XIStotalSegNo];	/* Frame Sync Err	*/
  int lSyncErr[XIStotalSegNo];	/* Line Sync Err	*/
  int pixCpErr[XIStotalSegNo];	/* Pixel Code Parity Err   */
  int insideEvThPixel[XIStotalSegNo]; /* ev.th. 内のイベント数*/
  int eventTotNo[XIStotalSegNo]; /* 検出されたイベント数 */
  int aboveEvThUpper[XIStotalSegNo]; /* ev.th.up.を越えた #ev*/
  int bias[XIStotalSegNo];	/* H-overcl.pixels の平均値*/
  int lastLine[XIStotalSegNo];	/* 処理した最後のライン番号*/
} XISexpInfTiming;


/*		Dark Init / Update mode				*/
#define	XISexpInfDarkCSize		9

typedef struct {
  int areaDiscriEna[XIStotalSegNo]; /* area discri ena 	*/
  int gradeDiscriEna;		/* grade discri ena 	*/
  int eventBufferID[XIStotalSegNo]; /* event buffer ID	*/
  int pixelBufferID[XIStotalSegNo]; /* pixel buffer ID	*/
  int hotPOvf[XIStotalSegNo];	/* hot pixel over flow	*/
  int timeOut[XIStotalSegNo];	/* PPU time OUt 	*/
  int evramFull[XIStotalSegNo];	/* event ram full	*/
  int otherErr[XIStotalSegNo];	/* other error	*/
  int aChkErr[XIStotalSegNo];	/* A-Chk Err	*/
  int fSyncErr[XIStotalSegNo];	/* Frame Sync Err	*/
  int lSyncErr[XIStotalSegNo];	/* Line Sync Err	*/
  int pixCpErr[XIStotalSegNo];	/* Pixel Code Parity Err   */
  int procon[XIStotalSegNo];	/* Dark初期化/更新処理対象セグメント */
  int updateSeqNo[XIStotalSegNo]; /* Dark Update残り回数 */
  int rejectPixel[XIStotalSegNo]; /* darkHigh を上回ったピクセル数 */
  int replacePixel[XIStotalSegNo]; /* darkLow を下回ったピクセル数 */
  int tupdateSeqNo[XIStotalSegNo]; /* update を行なった回数   */
  int trejectPixel[XIStotalSegNo]; /* darkHigh を上回ったピクセルの総数 */
  int treplacePixel[XIStotalSegNo]; /* darkLow を下回ったピクセルの総数*/
  int hotPixel[XIStotalSegNo];	/* hot pixel の数	*/
} XISexpInfDark;

#define	XISexpInfDarkInit	XISexpInfDark
#define	XISexpInfDarkUpdate	XISexpInfDark

/*		Frame mode					*/
#define	XISexpInfFrameCSize		2

typedef struct {
  int areaDiscriEna[XIStotalSegNo]; /* area discri ena */
  int gradeDiscriEna;		/* grade discri ena */
  int eventBufferID[XIStotalSegNo]; /* event buffer ID */
  int pixelBufferID[XIStotalSegNo]; /* pixel buffer ID	*/
  int hotPOvf[XIStotalSegNo];	/* hot pixel over flow	*/
  int timeOut[XIStotalSegNo];	/* PPU time OUt 	*/
  int evramFull[XIStotalSegNo];	/* event ram full	*/
  int otherErr[XIStotalSegNo];	/* other error	*/
  int aChkErr[XIStotalSegNo];	/* A-Chk Err	*/
  int fSyncErr[XIStotalSegNo];	/* Frame Sync Err	*/
  int lSyncErr[XIStotalSegNo];	/* Line Sync Err	*/
  int pixCpErr[XIStotalSegNo];	/* Pixel Code Parity Err   */
} XISexpInfFrame;

/*		Dark frame mode					*/
/*typedef struct {
  } XISexpInfDarkFrame;
  */

/*		Discriminator					*/
#define	ENABLE			1
#define	DISABLE			0

/*		Normal-end or Error				*/
#define	XISnormalEnd		1
#define	XISerrorEnd		0


#define	XISdataUnitMaxSize	55	/* byte */

/****************************************************************
 *	Pixel Data						*
 *		これは XISframeMake に入る前まで		*
 *		XISframeMake 後は Event Data の方を用いる	*
 *****************************************************************/

typedef struct {
  int rawx;
  int rawy;
  int phE;
  int phOuter[(XISoneEventPixelTotNo5x5 - 1)];
  int segment;
} XISpixelData5x5;

typedef struct {
  int rawx;
  int pOuterMost;
  int rawy;
  int sumOuterMost;
  int phE;
  int phOuter[(XISoneEventPixelTotNo3x3 - 1)];
  int segment;
} XISpixelData3x3;

typedef struct {
  int rawx;
  int rawy;
  int phE;
  int phOuter[(XISoneEventPixelTotNo2x2-1)];
  int pos2x2;
  int pAdjacent;
  int segment;
} XISpixelData2x2;

typedef struct {
  int rawx;
  int rawy;
  int ph;
  int grade;
  int segment;
} XISpixelDataTiming;

typedef struct {
  int rawx;
  int rawy;
  int ph;
  int segment;
} XISpixelDataDark;

#define XISpixelDataDarkUpdate XISpixelDataDark
#define XISpixelDataDarkInit   XISpixelDataDark

typedef struct {
  int rawy;
  int dummyPixelPh[XIScopiedPixelSize];
  int activePixelPh[XISactiveSegmentHsize];
  int copiedPixelPh[XIScopiedPixelSize];
  int hOverclockedPixelPh[XISoverclockSize];
  int segment;
} XISpixelDataFrame;

#define XISpixelDataDarkFrame XISpixelDataFrame

/****************************************************************
 *	Lost Area						*
 *		パケットロス、テレメトリオーバーフローなどにより*
 *		失われた領域1つに関する情報			*
 *		これは XISbitUncompress で作られる		*
 *****************************************************************/
typedef struct {

  /* 以前は ModeInf EXPTIME (32bitカウンタの値) は読み出し時刻であると
     誤解されていたので、このような名前のフィールドに代入されていた。
     compatibility のために残すが、今後は廃止の方向。*/
  unsigned int readOutTime;	

  /* MODINF EXPTIME */
  unsigned int exptime;

  int aeDate;			/* exptime に対応するaetimeの日日部分 */

  /* exptime が入っていた CCSDS パケットについている S_TIME */  
  double stime_of_ccsdspacket;	

  unsigned int pixBufFlipTime;	/* PPUのpixel bufferが反転した時刻を表す。
				   Clocking Modeが Normal/Burst では常に0。*/
  int segment;
  int lostAreaStRawx;		/* 失われた領域の最初の読みだし座標 */
  int lostAreaStRawy;
  int lostAreaEndRawx;		/* 失われた領域の最後の読みだし座標 */
  int lostAreaEndRawy;
} XISlostArea;



/****************************************************************
 *	Event Data						*
 *		これは XISframeMake で作られる			*
 *****************************************************************/
/* event data の構造体は、 異なるmode毎につくる */
/* mode0  : XISedit3x3   イベントデータ		*/
typedef struct {

  /* ModeInf EXPTIME (32bitカウンタの値) 
     normal mode なら露光開始時刻に相当。
     歴史的経緯でこんなまずい名前になっている。
     compatibility のために残すが今後は廃止の方向*/
  unsigned int readOutTime;	


  /* MODINF EXPTIME */
  unsigned int exptime;	
  int aeDate;			/* exptime に対応するaetimeの日日部分 */
  double stime_of_ccsdspacket;	/* exptime が入っていた CCSDS パケットについている S_TIME */

  unsigned int pixBufFlipTime;	/* PPUのpixel bufferが反転した時刻を表す。
				   Clocking Modeが Normal/Burst では常に0。*/
  int exposureSeqNo;		/* フレームの中での露出番号 (0から数える) */
  int eventSeqNo;		/* 露出の中でのイベント番号 (0から数える) */
  double expCentAetime;		/* 露出時刻(露出のちょうど中間の時刻) (ASTRO-E Time) */
  double exposure;		/* 露出時間 (sec) */
  int segment;
  int rawx;			/* 読みだし座標。 */
  int rawy;			/* 読みだし座標。 */
  int actx;			/* CCD上のピクセル座標。 */
  int acty;			/* CCD上のピクセル座標。 */
  int detx;			/* actxをリニアライズし、XISカメラとしての座標。 */
  int dety;			/* actyをリニアライズし、XISカメラとしての座標。 */
  int x;			/* 天空上の座標 */
  int y;			/* 天空上の座標 */
  int pOuterMost;
  int sumOuterMost;
  int ph[XISoneEventPixelTotNo3x3];
  /* 3x3ピクセルのテレメトリ出力生パルスハイト (channel) */
  int pha;			/* 各種補正をした後の、イベントのパルスハイト。(channel)。 */
  double pi;			/* さらに長時間、場所補正などを行なった結果、
				   最終的に得られるイベントのX線エネルギー。
				   ただしkeVではなく、channelで表現する。 */
  int grade;
} XISeventData3x3;


/* XISedit5x5   イベントデータ			*/
typedef struct {

  /* ModeInf EXPTIME (32bitカウンタの値) 
     露光開始時刻に相当。誤解があってこんなまずい名前になっている。
     compatibility のために残すが、今後は廃止の方向。*/
  unsigned int readOutTime;	

  /* ModeInf EXPTIME */
  unsigned int exptime;

  int aeDate;			/* exptime に対応するaetimeの日日部分 */
  double stime_of_ccsdspacket;	/* exptime が入っていた CCSDS パケットについている S_TIME */

  unsigned int pixBufFlipTime;	/* PPUのpixel bufferが反転した時刻を表す。
				   Clocking Modeが Normal/Burst では常に0。*/
  int exposureSeqNo;		/* フレームの中での露出番号 (0から数える) */
  int eventSeqNo;		/* 露出の中でのイベント番号 (0から数える) */
  double expCentAetime;		/* 露出時刻(露出のちょうど中間の時刻) (ASTRO-E Time) */
  double exposure;		/* 露出時間 (sec) */
  int segment;
  int rawx;			/* 読みだし座標。 */
  int rawy;			/* 読みだし座標。 */
  int actx;			/* CCD上のピクセル座標。 */
  int acty;			/* CCD上のピクセル座標。 */
  int detx;			/* actxをリニアライズし、XISカメラとしての座標。 */
  int dety;			/* actyをリニアライズし、XISカメラとしての座標。 */
  int x;			/* 天空上の座標 */
  int y;			/* 天空上の座標 */
  int ph[XISoneEventPixelTotNo5x5];	
  /* 5x5ピクセルのテレメトリ出力生パルスハイト (channel) */
  int pha;			/* 各種補正をした後の、イベントのパルスハイト。(channel)。 */
  double pi;			/* さらに長時間、場所補正などを行なった結果、
				   最終的に得られるイベントのX線エネルギー。
				   ただしkeVではなく、channelで表現する。 */
  int grade;
} XISeventData5x5;


/* XISedit2x2   イベントデータ			*/
typedef struct {

  /* ModeInfの読みだし時刻 (32bitカウンタの値) 
     normal mode なら露光開始時刻に相当。
     誤解があってこんなまずい名前になっていた。
     compatibility のために残すが、今後は廃止の方向。*/
  unsigned int readOutTime;	

  /* MODINF EXPTIME */
  unsigned int exptime;
  int aeDate;			/* exptime に対応するaetimeの日日部分 */
  double stime_of_ccsdspacket;	/* exptime が入っていた CCSDS パケットについている S_TIME */

  unsigned int pixBufFlipTime;	/* PPUのpixel bufferが反転した時刻を表す。
				   Clocking Modeが Normal/Burst では常に0。*/
  int exposureSeqNo;		/* フレームの中での露出番号 (0から数える) */
  int eventSeqNo;		/* 露出の中でのイベント番号 (0から数える) */
  double expCentAetime;		/* 露出時刻(露出のちょうど中間の時刻) (ASTRO-E Time) */
  double exposure;		/* 露出時間 (sec) */
  int segment;
  int rawx;			/* 読みだし座標。 */
  int rawy;			/* 読みだし座標。 */
  int actx;			/* CCD上のピクセル座標。 */
  int acty;			/* CCD上のピクセル座標。 */
  int detx;			/* actxをリニアライズし、XISカメラとしての座標。 */
  int dety;			/* actyをリニアライズし、XISカメラとしての座標。 */
  int x;			/* 天空上の座標 */
  int y;			/* 天空上の座標 */
  int pos2x2; 
  int pAdjacent;
  int ph[XISoneEventPixelTotNo2x2];
  /* 2x2のテレメトリ出力生パルスハイト (channel) */
  int pha;			/* 各種補正をした後の、イベントのパルスハイト。(channel)。 */
  double pi;			/* さらに長時間、場所補正などを行なった結果、
				   最終的に得られるイベントのX線エネルギー。
				   ただしkeVではなく、channelで表現する。 */
  int grade;
} XISeventData2x2;

/* XISeditTiming   イベントデータ		*/
typedef struct {

  /* ModeInf EXPTIME (32bitカウンタの値)
     しかしTiming/P-sum Modeなので、同じ値しか出力されない 
     compatibility のために残すが、今後は廃止の方向。
     今後は下の exptime を使用すること。*/
  unsigned int readOutTime;	


  /* ModeInf Exptime 
     Timing/P-sum mode のときは、同じ値しか出力されない */
  unsigned int exptime;

  int aeDate;			/* exptime に対応するaetimeの日日部分 */
  double stime_of_ccsdspacket;	/* exptime が入っていた CCSDS パケットについている S_TIME */

  unsigned int pixBufFlipTime;	/* PPUのpixel bufferが反転した時刻を表す。
				   Clocking Modeが Normal/Burst では常に0。*/
  double pixBufFlipTime_aetime;	/* pixel buffer flipping time の Astro-E time 版*/
  int exposureSeqNo;		/* フレームの中での露出番号 (0から数える) */
  int eventSeqNo;		/* 露出の中でのイベント番号 (0から数える) */
  double expCentAetime;		/* 露出時刻(露出のちょうど中間の時刻) (ASTRO-E Time) */
  double exposure;		/* 露出時間 (sec) */
  int segment;
  int rawx;			/* 読みだし座標。 */
  int rawy;			/* 読みだし座標。 */
  int actx;			/* CCD上のピクセル座標。 */
  int acty;			/* CCD上のピクセル座標。 */
  int detx;			/* actxをリニアライズし、XISカメラとしての座標。 */
  int dety;			/* actyをリニアライズし、XISカメラとしての座標。 */
  int x;			/* 天空上の座標 */
  int y;			/* 天空上の座標 */
  int ph;			/* イベントのテレメトリ出力生パルスハイト (channel) */
  int pha;			/* 各種補正をした後の、イベントのパルスハイト。(channel)。 */
  double pi;			/* さらに長時間、場所補正などを行なった結果、
				   最終的に得られるイベントのX線エネルギー。
				   ただしkeVではなく、channelで表現する。 */
  int grade;
} XISeventDataTiming;

/* XISeditFrame, XISeditDarkFrame フレームデータ */
typedef struct {

  /* ModeInf EXPTIME (32bitカウンタの値)
     Clocking Modeが Normal/Burst であれば、カウントアップされていく。
     P-sum Mode では、同じ値しか出力されない 
     compatibility のために残すが、今後は廃止の方向。
     今後は以下の exptime を使用すること */
  unsigned int readOutTime;	


  /* MODINF EXPTIME */
  unsigned int exptime;
  int aeDate;			/* exptime に対応するaetimeの日日部分 */
  double stime_of_ccsdspacket;	/* exptime が入っていた CCSDS パケットについている S_TIME */

  unsigned int pixBufFlipTime;	/* PPUのpixel bufferが反転した時刻を表す。
				   Clocking Modeが Normal/Burst では常に0。*/
  double expCentAetime;		/* 露出時刻(露出のちょうど中間の時刻 */
  double exposure;		/* 露出時間 (sec) */
  int    segment;
  int    rawy;			/* Telemetry ModeINF のLine No から数える */
  int    acty;			/* これは Window Optionや
				   Timingモードを考えるとあった方が良い   */
  int    dummyPixelPh[XIScopiedPixelSize];
  /* 読み出しの最初の 2pixel はセグメントA,Dでは
     ダミーであり、B,Cでは読みだし口側に接する
     セグメントの端2ピクセルのコピー 
     XIScopiedPixelSize = 2 */
  int    activePixelPh[XISactiveSegmentHsize]; 
  /* XISactiveSegmentHsize = 256 */
  int    copiedPixelPh[XIScopiedPixelSize];  
  /* 読みだし口とは反対側のセグメントの端
     2ピクセルのコピー XIScopiedPixelSize = 2 */
  int    hOverclockedPixelPh[XISoverclockSize];	/* XISoverclockSize = 16 */
} XISeventDataFrame;

#define XISeventDataDarkFrame      XISeventDataFrame

/* XISeditDarkUpdate, XISeditDarkInit */
typedef struct {

  /* ModeInf EXPTIME (32bitカウンタの値) 
     通常は露光開始時刻に相当。
     しかし誤解があってこんなまずい名前になっている。
     今後は廃止の方向。代わりに以下の exptime を使うこと。*/
  unsigned int readOutTime;	


  /* MODINF EXPTIME */
  unsigned int exptime;

  int aeDate;			/* exptime に対応するaetimeの日日部分 */
  double stime_of_ccsdspacket;	/* exptime が入っていた CCSDS パケットについている S_TIME */

  unsigned int pixBufFlipTime;	/* PPUのpixel bufferが反転した時刻を表す。
				   Clocking Modeが Normal/Burst では常に0。*/
  int exposureSeqNo;		/* フレームの中での露出番号 (0から数える) */
  int eventSeqNo;		/* 露出の中でのイベント番号 (0から数える)
				   この場合はホットピクセルの番号になる */
  int segment;

  /* Hot Pixel の座標は、copied pixels の機上処理の関係で、通常の RAWX 
     と比較すると 2 pixel ほど大きい座標がテレメトリーに出る。これを 
     PPUX/Yと定義する */

  int ppux; 
  int ppuy;

  int rawx;
  int rawy;
  int actx;
  int acty;
  int detx;
  int dety;
  int ph;
} XISeventDataDark;

#define XISeventDataDarkUpdate XISeventDataDark
#define XISeventDataDarkInit   XISeventDataDark

#endif
