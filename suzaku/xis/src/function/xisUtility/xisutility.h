/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:58 1999 by E. Miyata*/
/************************************************************************
  xisutility ver1.4
************************************************************************/

/*
 * Ϫ�д�Ϣ����谷�ѹ�¤��
 */

/*
 *   --+---+-------------+-------------------+------
 *     |   ���ѳ���     (��λ: Burst mode)   |
 *     |    ��----------�� explength         |
 *     |���� expdelay                        |
 *     |                                     |
 *    exptime_prev                          exptime
 */

typedef struct xisFrameTimes {
  unsigned int exptime;		/* EXPTIME in MODINF      */
  unsigned int buftime;		/* BIFTIME in MODINF      */
  double exptimeD;	/* EXPTIME in astroe_time */
  double buftimeD;	/* BIFTIME in astroe_time */
  unsigned int exptime_prev;	/* ��������� exptime     */
  unsigned int buftime_prev;	/* ��������� buftime     */
  double exptime_prevD;	/* ��������� exptimeD    */
  double buftime_prevD;	/* ��������� buftimeD    */
  double expdelay;	/* seq.start �������ѳ��ϤޤǤ��٤� (Burst mode) */
  double explength;	/* Burst/Psum mode �Ǥ� frame/line �������Ϫ�л��� */
  double framesttime;	/* �ե졼�೫�ϻ��� */
  double frameendtime;	/* �ե졼�ཪλ���� */
  int first_frame_flag; /* ���Υ���å��κǽ�� 1 �ե졼���ܤλ� non-0 */
} xisFrameTimes;

/* ��
 * �����Ĥ�˴ؤ��ؿ��ϡ�
 *  ��PRAM header �򸵤� explength �������ۡ�
 *  ��exptime, buftime, expdelay, explength �򸵤˳Ƽ����������ۡ�
 *  ���ե졼�೫�ϡ���λ�����������
 *  ���������ϡ���λ������֤���
 *  ��expdelay �Ϥɤ����롩
 * ���ʡ��Ȥꤢ�����ϡ�
 */


/*
 * �ʲ����ؿ�
 */

/************************************************************************/
/*   << FUNCTION >>                                                     */
/*      MODE INF�ǽ��Ϥ��줿����(TI��counter��)��ATRO-E time���ѹ�����  */
/*      ��������ASTRO-E time�Ȥϡ�2000/01/01 00:00:00 (TBD)�κ����ÿ�   */
/*      ���Τ��� FJT �󶡤δؿ��˼�ä������뱿̿�ġ� (;_;)         */
/************************************************************************/
double
xisRdTime2AETime(
	         unsigned rdouttime	/* IN : TI counter from MODEINF */
);


/************************************************************************/
/*   << FUNCTION >>                                                     */
/*	PRAM Header ����򸵤ˡ��Ƽ������Ԥʤ���			*/
/************************************************************************/
int
xisSetPramInfo (
                int           clkMode,       /* IN: Clock mode */
                int           burstExpTime,  /* IN: B_EXP_T in PRAM header */
                xisFrameTimes *fT
);

/************************************************************************/
/*   << FUNCTION >>                                                     */
/*      Frame�γƼ�����׻����롣                                     */
/*    ��ʣ����exposure�����äƤ�����ϡ��ǽ��eposure�γ��ϻ����       */
/*      �Ǹ��exposure�ν�λ������֤�����                              */
/*      ��¤�Τˤϡ�.exptime, .biftime, .exptime_prev, .buftime_prev,   */
/*      expdelay, explength �򥻥åȤ��Ƥ�������                        */
/************************************************************************/
int
xisSetFrameTimes (
                   int    clockmode,    /* IN : CLOCK MODE    */
                   xisFrameTimes *fT    /* frameTimes: �Ƽ�����ݻ���¤�� */
);

/************************************************************************/
/*   << FUNCTION >>                                                     */
/*      frame�λ������ϻ���¾��Ȥäơ��������ϻ����Ϫ�����֤�         */
/*      �׻����롣                                                      */
/************************************************************************/
int
xisGetExpStTime (
                   int           clockmode,
                   int           windowoption,
                   int           rawy,
                   xisFrameTimes *fT,   /* frameTimes: �Ƽ�����ݻ���¤��  */
                   double        *expsttime,   /* OUT: EXPOSURE START TIME */
                   double        *exposure     /* OUT: EXPOSURE            */
);

/************************************************************************/
/*   << FUNCTION >>                                                     */
/*      �������ϻ����exposure time��Ȥäơ�exposure center time��     */
/*      �׻����롣                                                      */
/************************************************************************/
double
xisGetExpCentTime(
	      double framesttime,    /* IN : AE TIME from MODEINF */
              double exposuretime    /* IN : AE TIME convereted   */
);


/************************************************************************/
/*   << FUNCTION >>                                                     */
/*      �ԥ������ͤν��� (25 �Ȥ� 9 �Ȥ�����¬�⡼�ɤǰۤʤ�) ����	*/
/*      PHA �ͤȥ��졼�ɤ�׻����롣					*/
/************************************************************************/
int
xisGetPH(
	int editmode,	/* IN:  XISobs3x3 �Ȥ�������̾�� Observation Mode */
	int *pix,	/* IN:  �⡼�ɤ˱��������Υԥ�����ǡ��� (����)   */
	int *pha,	/* OUT: ���Ϥ��б����� PHA ��                     */
	int grade	/* OUT: ���Ϥ��б��������٥�ȡ����졼��          */
);


/************************************************************************/
/*   << FUNCTION >>                                                     */
/*      PHA �ͤ�����Ȥäơ����󥵡��ȥ������Ȥ˱������黻�򤷤�	*/
/*      PI �ͤ��֤���							*/
/*	�ͤ����ѤǤ��ʤ����ϤȤꤢ���� 65536 ���֤����Ȥ��롣		*/
/************************************************************************/
double
xispha2pi(int sensor, int segment, int pha);
