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
		��ɬ�פ� pointer �Ϥ����᤿
		�֤��ͤ�Ĥ��������顼�����ζ�����
	ver 0.5  98/04/02 modified by M. Ozaki
		���ﹽ¤�Τ������Ƴ��
		��ɸ�Ѵ��ؿ����̥饤�֥�� (xisJCoord) ��
	ver 0.6  98/07/28 modified by M. Ozaki
		PSum �⡼�ɤλ��������� 8 �ä˸���
	ver 0.7  99/05/13 modified by H. Awaki
		2x2 mode���б�
	ver 0.8  99/08/12 modified by dE. Miyata
		xisRdTime2AETime �� CCSDS ����
		�ɤ�褦���ѹ�����
	ver 0.9  00/01/27 modified by M. Ozaki
		xisSetPramInfo ��ɬ�� explength ������
		xisSetFrameTimes �� 1 �����ܤ��ü찷��
	ver 0.10 00/01/28 modified by M. Ozaki
		xisSetFrameTimes �� NormalClock ��
		exposure start time �����Ǥ�
	ver 0.92 2000/01/28 modified by E. Miyata
		fix �Ѥ������� bug
	ver 0.93 2000/02/05 modified by H. Awaki
		xisRdTime2AETime�ν���
	ver 0.94 2000/02/09 modified by M. Ozaki
		xisRdTime2AETime�ν���
		(TI counter �ΰ�����б�)
	ver 0.95 2000/02/09 modified by M. Ozaki
		xisRdTime2AETime�ν���
		(�׻��θ�������� offset ���դ���)
	ver 0.96 2004/02/06 modified by M. Ozaki
		xisRdTime2AETime�ν���
	ver 0.97 ??? by M. Ozaki
		(include missing math.h)
	ver 0.98 2005/02/23 modified by H. Matsumoto
		xisRdTime2AETime�ν���
	ver 0.99 2005/02/23 modified by M. Ozaki
		xisRdTime2AETime�ν���
	ver 1.0  2005/05/25 modified by H. Matsumoto
              aste_ti2time �� �ѹ����б�
              xisRdTime2AETime ���ѹ�
	ver 1.1 2005/6/14 H. Matsumoto
	      xisSetFrameTimes ���ѹ�
	ver 1.2 2005/7/8 H. Matsumoto
	      xisutility.h �򤳤Υǥ��쥯�ȥ�˰�ư
	ver 1.3 2005/10/31 Y.ISHISAKI
	      exit() ̤����Τ��� #include <stdlib.h> ���ɲ�
	      initialize aetime = 0.0 in xisRdTime2AETime()
        ver 1.4 2006/09/15 Hironori Matsumoto
              xisFrameTime �Ρ�exptime, buftime, exptime_prev, 
              buftime_prev ������� unsigned int ���ѹ���

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

#define EXPOSURE_DEFAULT	8.0	/* 1 ���̤Υǥե���� exposure time */

#define TI_PRECISION		32	/* TI counter �η�� */
#define TI_SUBSEC_PREC		12	/* TI counter �� sub second �η�� */

/***********************************************************************
  << FUNCTION >>
  MODE INF�ǽ��Ϥ��줿����(TI��counter��)��Astro-E time���ѹ�����
  ��������ASTRO-E time�Ȥϡ�2000/01/01 00:00:00 (TBD)�κ����ÿ�
  ���Τ褦�ʵ�ǽ�� aste_ti2time �Ǽ¸������褦�ˤʤä���

  �äˡ���ή�� AEpacketTimeSet ���Ȥ߹��ޤ�Ƥ�����ˤϡ�
  BNK ASTE:PACKET_AETIME �����äƤ�����֤ϡ����Ǥ˹����٤�
  �׻����̤��������դ����Ԥʤ�줿����ͤǤ��롣

  �����Ϥ��δؿ���ɬ�פʤ��������������δؿ���ɬ�פȤ��Ƥ���
  �⥸�塼�뤬���ä��Ȥ��˺���Τǡ�����Ĥ��Ƥ�����

  << VERSION >>
  version 0.0          H. Awaki    96/09/02
  version 0.1          M. Ozaki	 98/04/01  ����ͤ� astroe_time
  version 0.2          E. Miyata	 99/08/12  CCSDS2nd ��Ȥ�
  version 0.3          H. Awaki	 00/02/05  CCSDS2nd �������˻Ȥ�
  version 0.4          M. Ozaki	 00/02/09  TI counter roundup
  version 0.5          M. Ozaki	 00/02/09  TI counter roundup
  version 0.6          M. Ozaki	 03/02/17  TI counter roundup
  for IA32 architecture
  version 0.7          H. Matsumoto 05/02/23  TI counter roundup
  version 0.99         M. Ozaki    05/02/23  using astetool
  version 1.0          H. Matsumoto 05/05/25
  ������ aste_ti2time ��Ŭ�������롣

  ***********************************************************************/
double
xisRdTime2AETime(
	         unsigned rdouttime /* IN : TI counter from MODEINF */
		 )
{
  static int       get_flag=0;	/* ���٤�����������뤿��Υե饰 */
  static TI2TIME*  ttpp = 0;
  double aetime = 0.0;		/* return value */
  double s_time;		/* sirius time used by aste_ti2time_dp() */
  int    size;			/* parameter for BnkfGetM: unused in this function, however */
  int    r;			/* return value from aste_ti2time_*() functions */

  /* TI --> aetime �Ѵ��Τ���ι�¤�Τ��������롣�ѿ� get_flag ������
     ���ư������Ԥʤ��� */

  if(!get_flag) {
    char* tim_file = 0;		/* time packet FITS file name.
				   �Ȥꤢ���� NULL �ˤ��Ƥ�����*/
    int index;			/* BnkKey �� */

    /* �⤷���Ǥ� "ASTE:TI2TIME:PTR" �Ȥ��� BNK ���������Ƥ���С�
       ���Ǥ˽�����ϹԤʤ��Ƥ���Τǡ����ξ����Ȥ� */
    if (ANL_OK == BnkKey("ASTE:TI2TIME:PTR", &index)) {
      BnkfGetM("ASTE:TI2TIME:PTR", sizeof(ttpp), &size, &ttpp);
    }
    else {
      /* �ޤ���������Ԥʤ��Ƥ��ʤ���С������ǹԤʤ���
	 ���������ξ�硢timfile ���ɤ߹��ޤʤ��Τǡ��������Ѵ���̵�� */
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
    get_flag=1;			/* �������λ */

  }


  /* TI --> aetime �Ѵ��ˤϡ�TI �η���夬����н褹�뤿�ᡢS_TIME ��ɬ�� */
  BnkfGetM("ASTE:PACKET_S_TIME", sizeof(s_time), &size, &s_time);


  /* s_time �����Ѥ��ơ�TI �� AETIME ���Ѵ� */
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
      PRAM Header ����򸵤ˡ��Ƽ������Ԥʤ���
      �Ȥꤢ�����ΤȤ���burst mode �Ǥ�Ϫ�л��֤��ɤ�Ǥ������
	(���: ���� 5 bit ���������ʲ���ɽ���Ⱦ���˲��ꤷ�Ƥ��롣)
	Normal clock �Ǥ� EXPOSURE_DEFAULT ������Ƥ��롣

   << VERSION >>
      version 0.0          M. Ozaki	 98/04/02
      version 0.1          M. Ozaki	 98/07/28
      version 0.2          M. Ozaki	 00/01/27
************************************************************************/
{
  switch (clkMode) {
#if 0
    /* burstExpTime �� HK ���ܤʤΤǡ��ᥤ��ǡ����� 8 �äΤ��줬�����롣*/
    /* ����ˤ����Ϥκ�����ɤ����ᡢ�Ȥꤢ������� 8 �äη���Ǥ��ˤ��롣*/
  case XISclockBurst:
    ft->explength = ((double)burstExpTime) / (1<<5);
    break;
#endif
  case XISclockPsum:
    ft->explength = EXPOSURE_DEFAULT / 1024.0; /* ���������� 8 �äˤ����Ǥ� */
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
		  xisFrameTimes *ft /* frameTimes: �Ƽ�����ݻ���¤�� */
		  )
/*********************************************************************
   << FUNCTION >>
      Frame�γƼ�����׻����롣
    ��ʣ����exposure�����äƤ�����ϡ��ǽ��eposure�γ��ϻ����
      �Ǹ��exposure�ν�λ������֤�����
	��¤�Τˤϡ�.exptime, .biftime, .exptime_prev, .buftime_prev,
	expdelay, explength �򥻥åȤ��Ƥ�������

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

  /* ft->exptime_prev �� 0 �����äƤ����������̽��� */
  /* ����Ϻǽ�� exposure �˴ؤ��ƤΤߵ����� */
#if 0				/* Ver 0.2 �Ǥ��ѹ��� */
  bogus_prevtime = (ft->exptime_prev == 0) ? 1 : 0;
#else
  bogus_prevtime = (ft->first_frame_flag) ? 1 : 0;
#endif

  /* �ǽ�Υե졼��� exptime = 0 �ˤʤäƤ���Τǡ������ aetime ���ѹ�
     ���Ƥ��̣���ʤ��������ǡ����ξ��� aetime = -99 ���֤��褦�ˤ��롣*/

  if (ft->first_frame_flag == 1 && ft->exptime == 0) {
    ft->exptimeD      = -99;
  }
  else {
    ft->exptimeD      = xisRdTime2AETime (ft->exptime);
  }

  /* bogus_prevtime == 1 �ˤʤäƤ���Ȥ��ϡ�exptime_prev �ˤϰ�̣���ʤ�
     �� (0) �����äƤ��롣����򤽤Τޤ� aetime ���Ѵ����Ƥ��̣���ʤ��Τǡ�
     ���ξ��� -99 ���֤��褦�ˤ��� */

  if (bogus_prevtime == 1) {
    ft->exptime_prevD = -99;
  }
  else {
    ft->exptime_prevD = xisRdTime2AETime (ft->exptime_prev);
  }


  /* pixel buffer time �ˤϡ�timing mode �ʳ��λ����̾� 0 �����äƤ��롣
���ΤȤ��� aetime �Ȥ��Ƥ� -99 ���֤��褦������ */
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

  /** ���� �Ȥꤢ�����λ�����֡��ƽл���Ϳ����Ķ������ä����� **/
  ft->expdelay = 0;
  /** ���� �Ȥꤢ�����λ�����֡��ƽл���Ϳ����Ķ������ä����� **/
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
		 xisFrameTimes *ft, /* frameTimes: �Ƽ�����ݻ���¤��  */
		 double        *expsttime, /* OUT: EXPOSURE START TIME */
		 double        *exposure /* OUT: EXPOSURE            */
		 )
/************************************************************************
   << FUNCTION >>
      frame�λ������ϻ���¾��Ȥäơ��������ϻ����Ϫ�����֤�
      �׻����롣

   << VERSION >>
      version 0.0          H. Awaki    96/09/02
      version 0.1          H. Awaki    96/09/03   bug fix
************************************************************************/
{
  register int bogusExposure;

  switch ( clockmode ) {
  case XISclockBurst:
    *exposure = ft->explength;
    goto WindowCase;		/* �ġĤ������� jump */
  case XISclockNormal:
    *exposure = ft->frameendtime - ft->framesttime;
  WindowCase:			/* �ġ�XISclockNormal ����� jump �� */
    bogusExposure = ((*exposure > (EXPOSURE_DEFAULT + EXPOSURE_DEFAULT*2)/2) || (*exposure <= 0))
      ? 1 : 0;
    /* Ϫ�л��֤�Ĺ�᤮��/û�᤮��褦���ȴְ�äƤ��ǽ������ */
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
    bogusExposure = 0;		/* Ϫ�л��֤Ͼ���������Ȥ��� */
    break;

  default:
    fprintf (stderr, "XISUTILITY: Unknown clock mode -- %d\n", clockmode);
    bogusExposure = 1;		/* Ϫ�л��֤��������ʤ� */
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
      �������ϻ����exposure time��Ȥäơ�exposure center time��
      �׻����롣

   << VERSION >>
      version 0.0          H. Awaki    96/09/02
      version 0.1          M. Ozaki    98/04/02 ��̤��֤��ͤȤ���
************************************************************************/
{
  return (framesttime + (exposuretime * 0.5));
}


int
xisGetPH(
	 int editmode,		/* IN:  XISedit3x3 �Ȥ�������̾�� Observation Mode */
	 int *pix,		/* IN:  �⡼�ɤ˱��������Υԥ�����ǡ��� (����)    */
	 int *pha,		/* OUT: ���Ϥ��б����� PHA ��                      */
	 int grade		/* OUT: ���Ϥ��б��������٥�ȡ����졼��           */
	 )
/************************************************************************
   << FUNCTION >>
      �ԥ������ͤν��� (25 �Ȥ� 9 �Ȥ�����¬�⡼�ɤǰۤʤ�) ����
      PHA �ͤȥ��졼�ɤ�׻����롣

   << VERSION >>
      version 0.0          M. Ozaki    98/04/06
************************************************************************/
{
  int *pixtmp;

  switch (editmode){
  case XISedit3x3:		/* �Ȥꤢ������ pixel ��û����� PHA �Ȥ��� */
    *pha = 0;
    for (pixtmp = pix + 3*3 - 1; pixtmp >= pix; pixtmp--){
      *pha += *pixtmp;
    }
    break;
  case XISedit5x5:		/* �Ȥꤢ������ pixel ��û����� PHA �Ȥ��� */
    *pha = 0;
    for (pixtmp = pix + 5*5 - 1; pixtmp >= pix; pixtmp--){
      *pha += *pixtmp;
    }
    break;
  case XISedit2x2:		/* �Ȥꤢ������ pixel ��û����� PHA �Ȥ��� */
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
      PHA �ͤ�����Ȥäơ����󥵡��ȥ������Ȥ˱������黻�򤷤�
      PI �ͤ��֤���
	�ͤ����ѤǤ��ʤ����ϤȤꤢ���� 65536 ���֤����Ȥ��롣

   << VERSION >>
      version 0.0          M. Ozaki    98/04/06
************************************************************************/
{
  double pi;

  pi = (double)pha;

  return (pi);
}
