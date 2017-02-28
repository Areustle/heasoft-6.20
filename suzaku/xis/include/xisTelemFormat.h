/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 21:33:25 1999 by E. Miyata*/

/*************************************************************************
  
  Astro-E XIS   E. Miyata@OsakaUniv.		 

  ver 2.32  2006/4/26 H. Matsumoto
  ��¤�� XISeventDataTiming �� pixBufFlipTime_aetime �Υե�����ɤ��ɲ�

  ver 2.31  2005/8/31 H. Matsumoto
  ��¤�� XISeventDataDark �� detx,dety �Υե�����ɤ��ɲ�

  ver 2.30  2005/6/14 H. Matsumoto
  XISmodeInf �� exptime �Υե�������ɲ�
  XISeventData*** �� stime_of_exptime �� stime_of_ccsdspacket ��̾���ѹ�

  ver 2.20 2005/06/07    H. Matsumoto			 
  XISeventData*** �� stime_of_exptime �Υե�����ɤ��ɲ�	 
  XISloasArea �� stime_of_exptime �Υե�����ɤ��ɲ�
  XISeventDataDark �� ppux, ppuy �Υե�����ɤ��ɲ�
  
  ver 2.10 updated 2000/01/17     T.Tsuru                  
  struct XISexpInfDark �� procon                   
  tupdateSeqNo,trejectPixel,treplacePixel ���ɲ�   
  
  ver 2.00 updated 1999/05/19     T.Tsuru                  
  ������ 2x2 mode �˰ܹ�                           
  ver 1.9 updated 1999/03/08      H.Awaki                  
  2x2 mode���ɲ�                                   
  ver 1.83 updated 1998/10/21     T. Tsuru                 
  ��Edit Mode��Clock Mode ��3ʸ����û��̾�������  
  
  ver 1.82 updated 1998/09/07     T. Tsuru                 
  XISenergyPhChSizeDFr                             
  ver 1.81 updated 1998/08/31     T. Tsuru                 
  Main Data ��APID���ɲ�                           
  ver 1.8 updated 1998/08/01     E. Miyata                 
  compact-type struct ���			 
  detx/dety/x/y �� double -> int ���ѹ�		 
  ver 1.75 updated 1998/04/03     H. Awaki                 
  ExpInf�����Ƥ⽾��Τ�Τ����ѹ������ä��Τ�     
  �����ȿ�Ǥ����롣                               
  ���Ȥ���ʸ��                                     
  ��ASTRO-E XIS-DE�ƥ��ȥꡦ���ޥ�ɻ��ͽ��v1.4  
  ver 1.74 updated 1998/04/01     H. Awaki                 
  ModeInf�����Ƥ�����Τ�Τ����ѹ������ä��Τ�    
  �����ȿ�Ǥ����롣                               
  ver 1.73 updated 1998/04/01	H. Awaki		 
  XISbitUncompress���pixel data�ι�¤�Τ�         
  segment���ɲä��ޤ�����                          
  ver 1.72 updated 1997/12/03	E. Miyata		 
  #ifndef & #define _XIS_TELEMETRY_FORMAT_	 
  XISeditModeNum XISclockModeNum			 
  XISwindowOptionNum �ɲ�				 
  window option �� 1/8 ���ɲ�			 
  ver 1.71 updated 1997/06/30	E. Miyata		 
  #define XIScopiedPixelSizeNormal 		 
  #define XIScopiedPixelSizePsum			 
  ���ɲä���					 
  ver 1.7  updated 1997/06/15	T. Tsuru		 
  #define  XISenergyPhChSize 4096 ��ä���                
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
#define XIStotalXISno	4	/* ASTRO-E�ˤ�4��XIS������	*/

/*		Total XIS No in ASTRO-E				*/
#define XIStotalPPUno	2	/* ASTRO-E�ˤ�2��PPU������ PPU01, PPU12	*/

/*		Total XIS No in ASTRO-E				*/
#define XIStotalAETCEno	2	/* ASTRO-E�ˤ�2��AETCE������AETCE01,AETCE12 */

/*		Total Segemnt No in 1 XIS			*/
#define XIStotalSegNo	4	/* 1CCD�ˤ�4��Segement������	*/

/****************************************************************
 *		BIN No. of ADC in AE/TCE			*
 *****************************************************************/
/*       AE/TCE���ͥ륮�����ϤΥ����ͥ륵���� 12bits=4096ch */
#define XISenergyPhChSize               4096
/*       Dark Frame Mode �λ��Τ� 16bit�ˤʤ롣               */
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

/*		Pixel RAM ��Ǥκ�ɸ				*/
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
#define XISsegLightLeakPerX        4    /* 1segment �Ǥ�Light Leak */
#define XISsegLightLeakPerY       16    /* Partition �� 4x16       */

#define XISframeLightLeakPerX     16    /* 1 frame �Ǥ�Light Leak */
#define XISframeLightLeakPerX     16    /* Partition �� 16x16     */

/****************************************************************
 *		ModeINF						*
 *            1998/04/01 �ѹ�        Awaki                       *
 *         ��ASTRO-E XIS-DE �ƥ��ȥꡦ���ޥ�ɻ��ͽ� V1.4��    *
 *****************************************************************/
#define	XISmodeInfCSize		11

typedef struct {
  int editMode;			/* edit mode */
  int clockMode;		/* clocking mode */
  int windowOption;		/* window option */
  int lineSeqNo;		/* packet ����Ƭ #line */

  /* MODINF EXPTIME
     normal mode �ʤ�Ϫ�����ϻ�������� */
  unsigned int exptime; 
  double exptime_aetime;	/* exptime �� ASTRO-E time ���Ѵ�������Ρ�*/

  /* ������ MODINF EXPTIME �򥤥٥���ɤ߽Ф�����ȸ�򤷤Ƥ��ꡢ����
     ���� readOutTime �Ȥ����������ְ�ä�̾���Υե�����ɤ�������
     ��Ƥ�����¾�Υ��եȥ������ؤΥ���ѥ��ӥ�ƥ��Τ���˻Ĥ�������
     ��� MODINF EXPTIME �ΰ�̣�Ǥϻ��Ѥ��ʤ����� */
  unsigned int readOutTime;	
  double readOutAetime;		/* EXPTIME �� ASTRO-E Time */

  unsigned int pixBufFlipTime;	/* PPU��pixel buffer��ȿž���������ɽ�� */
} XISmodeInf;


/*		Offset from base address			*/
/*          �ʲ����ѹ����ޤ���  Awaki                         */
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
  int insideEvThPixel[XIStotalSegNo]; /* ev.th. ���ϰ���� #event*/
  int eventTotNo[XIStotalSegNo]; /* ���Ф��줿���٥�ȿ�	*/
  int aboveEvThUpper[XIStotalSegNo]; /* ev.th.up.��ۤ���#event*/
  int bias[XIStotalSegNo];	/* H-overcl.pixels ��ʿ���� */
  int lastLine[XIStotalSegNo];	/* ���������Ǹ�Υ饤���ֹ� */
  int lightLeak[XIStotalSegNo][XISsegLightLeakPerY][XISsegLightLeakPerX];
  /* ���̤��� 		*/
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
  int insideEvThPixel[XIStotalSegNo]; /* ev.th. ��Υ��٥�ȿ�*/
  int eventTotNo[XIStotalSegNo]; /* ���Ф��줿���٥�ȿ� */
  int aboveEvThUpper[XIStotalSegNo]; /* ev.th.up.��ۤ��� #ev*/
  int bias[XIStotalSegNo];	/* H-overcl.pixels ��ʿ����*/
  int lastLine[XIStotalSegNo];	/* ���������Ǹ�Υ饤���ֹ�*/
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
  int procon[XIStotalSegNo];	/* Dark�����/���������оݥ������� */
  int updateSeqNo[XIStotalSegNo]; /* Dark Update�Ĥ��� */
  int rejectPixel[XIStotalSegNo]; /* darkHigh ����ä��ԥ������ */
  int replacePixel[XIStotalSegNo]; /* darkLow �򲼲�ä��ԥ������ */
  int tupdateSeqNo[XIStotalSegNo]; /* update ��Ԥʤä����   */
  int trejectPixel[XIStotalSegNo]; /* darkHigh ����ä��ԥ��������� */
  int treplacePixel[XIStotalSegNo]; /* darkLow �򲼲�ä��ԥ���������*/
  int hotPixel[XIStotalSegNo];	/* hot pixel �ο�	*/
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
 *		����� XISframeMake ���������ޤ�		*
 *		XISframeMake ��� Event Data �������Ѥ���	*
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
 *		�ѥ��åȥ����ƥ��ȥꥪ���С��ե��ʤɤˤ��*
 *		����줿�ΰ�1�Ĥ˴ؤ������			*
 *		����� XISbitUncompress �Ǻ����		*
 *****************************************************************/
typedef struct {

  /* ������ ModeInf EXPTIME (32bit�����󥿤���) ���ɤ߽Ф�����Ǥ����
     ��򤵤�Ƥ����Τǡ����Τ褦��̾���Υե�����ɤ���������Ƥ�����
     compatibility �Τ���˻Ĥ�����������ѻߤ�������*/
  unsigned int readOutTime;	

  /* MODINF EXPTIME */
  unsigned int exptime;

  int aeDate;			/* exptime ���б�����aetime��������ʬ */

  /* exptime �����äƤ��� CCSDS �ѥ��åȤˤĤ��Ƥ��� S_TIME */  
  double stime_of_ccsdspacket;	

  unsigned int pixBufFlipTime;	/* PPU��pixel buffer��ȿž���������ɽ����
				   Clocking Mode�� Normal/Burst �ǤϾ��0��*/
  int segment;
  int lostAreaStRawx;		/* ����줿�ΰ�κǽ���ɤߤ�����ɸ */
  int lostAreaStRawy;
  int lostAreaEndRawx;		/* ����줿�ΰ�κǸ���ɤߤ�����ɸ */
  int lostAreaEndRawy;
} XISlostArea;



/****************************************************************
 *	Event Data						*
 *		����� XISframeMake �Ǻ����			*
 *****************************************************************/
/* event data �ι�¤�Τϡ� �ۤʤ�mode��ˤĤ��� */
/* mode0  : XISedit3x3   ���٥�ȥǡ���		*/
typedef struct {

  /* ModeInf EXPTIME (32bit�����󥿤���) 
     normal mode �ʤ�Ϫ�����ϻ����������
     ���Ū�аޤǤ���ʤޤ���̾���ˤʤäƤ��롣
     compatibility �Τ���˻Ĥ���������ѻߤ�����*/
  unsigned int readOutTime;	


  /* MODINF EXPTIME */
  unsigned int exptime;	
  int aeDate;			/* exptime ���б�����aetime��������ʬ */
  double stime_of_ccsdspacket;	/* exptime �����äƤ��� CCSDS �ѥ��åȤˤĤ��Ƥ��� S_TIME */

  unsigned int pixBufFlipTime;	/* PPU��pixel buffer��ȿž���������ɽ����
				   Clocking Mode�� Normal/Burst �ǤϾ��0��*/
  int exposureSeqNo;		/* �ե졼�����Ǥ�Ϫ���ֹ� (0���������) */
  int eventSeqNo;		/* Ϫ�Ф���ǤΥ��٥���ֹ� (0���������) */
  double expCentAetime;		/* Ϫ�л���(Ϫ�ФΤ��礦����֤λ���) (ASTRO-E Time) */
  double exposure;		/* Ϫ�л��� (sec) */
  int segment;
  int rawx;			/* �ɤߤ�����ɸ�� */
  int rawy;			/* �ɤߤ�����ɸ�� */
  int actx;			/* CCD��Υԥ������ɸ�� */
  int acty;			/* CCD��Υԥ������ɸ�� */
  int detx;			/* actx���˥��饤������XIS�����Ȥ��Ƥκ�ɸ�� */
  int dety;			/* acty���˥��饤������XIS�����Ȥ��Ƥκ�ɸ�� */
  int x;			/* ŷ����κ�ɸ */
  int y;			/* ŷ����κ�ɸ */
  int pOuterMost;
  int sumOuterMost;
  int ph[XISoneEventPixelTotNo3x3];
  /* 3x3�ԥ�����Υƥ��ȥ�������ѥ륹�ϥ��� (channel) */
  int pha;			/* �Ƽ������򤷤���Ρ����٥�ȤΥѥ륹�ϥ��ȡ�(channel)�� */
  double pi;			/* �����Ĺ���֡���������ʤɤ�Ԥʤä���̡�
				   �ǽ�Ū�������륤�٥�Ȥ�X�����ͥ륮����
				   ������keV�ǤϤʤ���channel��ɽ�����롣 */
  int grade;
} XISeventData3x3;


/* XISedit5x5   ���٥�ȥǡ���			*/
typedef struct {

  /* ModeInf EXPTIME (32bit�����󥿤���) 
     Ϫ�����ϻ������������򤬤��äƤ���ʤޤ���̾���ˤʤäƤ��롣
     compatibility �Τ���˻Ĥ�����������ѻߤ�������*/
  unsigned int readOutTime;	

  /* ModeInf EXPTIME */
  unsigned int exptime;

  int aeDate;			/* exptime ���б�����aetime��������ʬ */
  double stime_of_ccsdspacket;	/* exptime �����äƤ��� CCSDS �ѥ��åȤˤĤ��Ƥ��� S_TIME */

  unsigned int pixBufFlipTime;	/* PPU��pixel buffer��ȿž���������ɽ����
				   Clocking Mode�� Normal/Burst �ǤϾ��0��*/
  int exposureSeqNo;		/* �ե졼�����Ǥ�Ϫ���ֹ� (0���������) */
  int eventSeqNo;		/* Ϫ�Ф���ǤΥ��٥���ֹ� (0���������) */
  double expCentAetime;		/* Ϫ�л���(Ϫ�ФΤ��礦����֤λ���) (ASTRO-E Time) */
  double exposure;		/* Ϫ�л��� (sec) */
  int segment;
  int rawx;			/* �ɤߤ�����ɸ�� */
  int rawy;			/* �ɤߤ�����ɸ�� */
  int actx;			/* CCD��Υԥ������ɸ�� */
  int acty;			/* CCD��Υԥ������ɸ�� */
  int detx;			/* actx���˥��饤������XIS�����Ȥ��Ƥκ�ɸ�� */
  int dety;			/* acty���˥��饤������XIS�����Ȥ��Ƥκ�ɸ�� */
  int x;			/* ŷ����κ�ɸ */
  int y;			/* ŷ����κ�ɸ */
  int ph[XISoneEventPixelTotNo5x5];	
  /* 5x5�ԥ�����Υƥ��ȥ�������ѥ륹�ϥ��� (channel) */
  int pha;			/* �Ƽ������򤷤���Ρ����٥�ȤΥѥ륹�ϥ��ȡ�(channel)�� */
  double pi;			/* �����Ĺ���֡���������ʤɤ�Ԥʤä���̡�
				   �ǽ�Ū�������륤�٥�Ȥ�X�����ͥ륮����
				   ������keV�ǤϤʤ���channel��ɽ�����롣 */
  int grade;
} XISeventData5x5;


/* XISedit2x2   ���٥�ȥǡ���			*/
typedef struct {

  /* ModeInf���ɤߤ������� (32bit�����󥿤���) 
     normal mode �ʤ�Ϫ�����ϻ����������
     ��򤬤��äƤ���ʤޤ���̾���ˤʤäƤ�����
     compatibility �Τ���˻Ĥ�����������ѻߤ�������*/
  unsigned int readOutTime;	

  /* MODINF EXPTIME */
  unsigned int exptime;
  int aeDate;			/* exptime ���б�����aetime��������ʬ */
  double stime_of_ccsdspacket;	/* exptime �����äƤ��� CCSDS �ѥ��åȤˤĤ��Ƥ��� S_TIME */

  unsigned int pixBufFlipTime;	/* PPU��pixel buffer��ȿž���������ɽ����
				   Clocking Mode�� Normal/Burst �ǤϾ��0��*/
  int exposureSeqNo;		/* �ե졼�����Ǥ�Ϫ���ֹ� (0���������) */
  int eventSeqNo;		/* Ϫ�Ф���ǤΥ��٥���ֹ� (0���������) */
  double expCentAetime;		/* Ϫ�л���(Ϫ�ФΤ��礦����֤λ���) (ASTRO-E Time) */
  double exposure;		/* Ϫ�л��� (sec) */
  int segment;
  int rawx;			/* �ɤߤ�����ɸ�� */
  int rawy;			/* �ɤߤ�����ɸ�� */
  int actx;			/* CCD��Υԥ������ɸ�� */
  int acty;			/* CCD��Υԥ������ɸ�� */
  int detx;			/* actx���˥��饤������XIS�����Ȥ��Ƥκ�ɸ�� */
  int dety;			/* acty���˥��饤������XIS�����Ȥ��Ƥκ�ɸ�� */
  int x;			/* ŷ����κ�ɸ */
  int y;			/* ŷ����κ�ɸ */
  int pos2x2; 
  int pAdjacent;
  int ph[XISoneEventPixelTotNo2x2];
  /* 2x2�Υƥ��ȥ�������ѥ륹�ϥ��� (channel) */
  int pha;			/* �Ƽ������򤷤���Ρ����٥�ȤΥѥ륹�ϥ��ȡ�(channel)�� */
  double pi;			/* �����Ĺ���֡���������ʤɤ�Ԥʤä���̡�
				   �ǽ�Ū�������륤�٥�Ȥ�X�����ͥ륮����
				   ������keV�ǤϤʤ���channel��ɽ�����롣 */
  int grade;
} XISeventData2x2;

/* XISeditTiming   ���٥�ȥǡ���		*/
typedef struct {

  /* ModeInf EXPTIME (32bit�����󥿤���)
     ������Timing/P-sum Mode�ʤΤǡ�Ʊ���ͤ������Ϥ���ʤ� 
     compatibility �Τ���˻Ĥ�����������ѻߤ�������
     ����ϲ��� exptime ����Ѥ��뤳�ȡ�*/
  unsigned int readOutTime;	


  /* ModeInf Exptime 
     Timing/P-sum mode �ΤȤ��ϡ�Ʊ���ͤ������Ϥ���ʤ� */
  unsigned int exptime;

  int aeDate;			/* exptime ���б�����aetime��������ʬ */
  double stime_of_ccsdspacket;	/* exptime �����äƤ��� CCSDS �ѥ��åȤˤĤ��Ƥ��� S_TIME */

  unsigned int pixBufFlipTime;	/* PPU��pixel buffer��ȿž���������ɽ����
				   Clocking Mode�� Normal/Burst �ǤϾ��0��*/
  double pixBufFlipTime_aetime;	/* pixel buffer flipping time �� Astro-E time ��*/
  int exposureSeqNo;		/* �ե졼�����Ǥ�Ϫ���ֹ� (0���������) */
  int eventSeqNo;		/* Ϫ�Ф���ǤΥ��٥���ֹ� (0���������) */
  double expCentAetime;		/* Ϫ�л���(Ϫ�ФΤ��礦����֤λ���) (ASTRO-E Time) */
  double exposure;		/* Ϫ�л��� (sec) */
  int segment;
  int rawx;			/* �ɤߤ�����ɸ�� */
  int rawy;			/* �ɤߤ�����ɸ�� */
  int actx;			/* CCD��Υԥ������ɸ�� */
  int acty;			/* CCD��Υԥ������ɸ�� */
  int detx;			/* actx���˥��饤������XIS�����Ȥ��Ƥκ�ɸ�� */
  int dety;			/* acty���˥��饤������XIS�����Ȥ��Ƥκ�ɸ�� */
  int x;			/* ŷ����κ�ɸ */
  int y;			/* ŷ����κ�ɸ */
  int ph;			/* ���٥�ȤΥƥ��ȥ�������ѥ륹�ϥ��� (channel) */
  int pha;			/* �Ƽ������򤷤���Ρ����٥�ȤΥѥ륹�ϥ��ȡ�(channel)�� */
  double pi;			/* �����Ĺ���֡���������ʤɤ�Ԥʤä���̡�
				   �ǽ�Ū�������륤�٥�Ȥ�X�����ͥ륮����
				   ������keV�ǤϤʤ���channel��ɽ�����롣 */
  int grade;
} XISeventDataTiming;

/* XISeditFrame, XISeditDarkFrame �ե졼��ǡ��� */
typedef struct {

  /* ModeInf EXPTIME (32bit�����󥿤���)
     Clocking Mode�� Normal/Burst �Ǥ���С�������ȥ��åפ���Ƥ�����
     P-sum Mode �Ǥϡ�Ʊ���ͤ������Ϥ���ʤ� 
     compatibility �Τ���˻Ĥ�����������ѻߤ�������
     ����ϰʲ��� exptime ����Ѥ��뤳�� */
  unsigned int readOutTime;	


  /* MODINF EXPTIME */
  unsigned int exptime;
  int aeDate;			/* exptime ���б�����aetime��������ʬ */
  double stime_of_ccsdspacket;	/* exptime �����äƤ��� CCSDS �ѥ��åȤˤĤ��Ƥ��� S_TIME */

  unsigned int pixBufFlipTime;	/* PPU��pixel buffer��ȿž���������ɽ����
				   Clocking Mode�� Normal/Burst �ǤϾ��0��*/
  double expCentAetime;		/* Ϫ�л���(Ϫ�ФΤ��礦����֤λ��� */
  double exposure;		/* Ϫ�л��� (sec) */
  int    segment;
  int    rawy;			/* Telemetry ModeINF ��Line No ��������� */
  int    acty;			/* ����� Window Option��
				   Timing�⡼�ɤ�ͤ���Ȥ��ä������ɤ�   */
  int    dummyPixelPh[XIScopiedPixelSize];
  /* �ɤ߽Ф��κǽ�� 2pixel �ϥ�������A,D�Ǥ�
     ���ߡ��Ǥ��ꡢB,C�Ǥ��ɤߤ�����¦���ܤ���
     �������Ȥ�ü2�ԥ�����Υ��ԡ� 
     XIScopiedPixelSize = 2 */
  int    activePixelPh[XISactiveSegmentHsize]; 
  /* XISactiveSegmentHsize = 256 */
  int    copiedPixelPh[XIScopiedPixelSize];  
  /* �ɤߤ������Ȥ�ȿ��¦�Υ������Ȥ�ü
     2�ԥ�����Υ��ԡ� XIScopiedPixelSize = 2 */
  int    hOverclockedPixelPh[XISoverclockSize];	/* XISoverclockSize = 16 */
} XISeventDataFrame;

#define XISeventDataDarkFrame      XISeventDataFrame

/* XISeditDarkUpdate, XISeditDarkInit */
typedef struct {

  /* ModeInf EXPTIME (32bit�����󥿤���) 
     �̾��Ϫ�����ϻ����������
     ��������򤬤��äƤ���ʤޤ���̾���ˤʤäƤ��롣
     ������ѻߤ�����������˰ʲ��� exptime ��Ȥ����ȡ�*/
  unsigned int readOutTime;	


  /* MODINF EXPTIME */
  unsigned int exptime;

  int aeDate;			/* exptime ���б�����aetime��������ʬ */
  double stime_of_ccsdspacket;	/* exptime �����äƤ��� CCSDS �ѥ��åȤˤĤ��Ƥ��� S_TIME */

  unsigned int pixBufFlipTime;	/* PPU��pixel buffer��ȿž���������ɽ����
				   Clocking Mode�� Normal/Burst �ǤϾ��0��*/
  int exposureSeqNo;		/* �ե졼�����Ǥ�Ϫ���ֹ� (0���������) */
  int eventSeqNo;		/* Ϫ�Ф���ǤΥ��٥���ֹ� (0���������)
				   ���ξ��ϥۥåȥԥ�������ֹ�ˤʤ� */
  int segment;

  /* Hot Pixel �κ�ɸ�ϡ�copied pixels �ε�������δط��ǡ��̾�� RAWX 
     ����Ӥ���� 2 pixel �ۤ��礭����ɸ���ƥ��ȥ꡼�˽Ф롣����� 
     PPUX/Y��������� */

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
