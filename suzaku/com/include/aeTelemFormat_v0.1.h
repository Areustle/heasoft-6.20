/*###############################################################
#								#
#		Astro-E   E. Miyata@OsakaUniv.			#
#	ver 0.1 updated 1996/8/29	Takeshi Go Tsuru	#
#	ver 0.0 updated 1996/8/29	Emi Miyata		#
#								#
################################################################*/
/****************************************************************
*		CCSDS packet					*
*****************************************************************/
typedef struct {
  /* Packet ID */
      /* Astro-E �Ǹ��ꤵ��Ƥ�����ʬ */
  unsigned int version:3;		/* Astro-E �Ǥ� 3 �˸��� */
  unsigned int type:1;			/* Astro-E �Ǥ� 0 �˸��� */
  unsigned int secHeaderFlag:1;		/* Astro-E �Ǥ� 1 �˸��� */
       /* APID */
  unsigned int common:2;		/* common use */
  unsigned int subsystemID:4;		/* ���� ID */
  unsigned int subsystemMisc:5;		/* ����Ǥ�� */

  /* Packet sequence count */
  unsigned int seqFlag:2;		/* sequence flag */
  unsigned int seqCount:14;		/* sequence count */
      /* packet legth */
  unsigned int packetLength:16;		/* secondary header+data-1 */

  /* secondary header */
  unsigned int secHeader:16;		/* �������*/
} AEccsdsHeaderC;

#define	AEccsdsPacketCSize	8	/* CCSDS packet header�Υ�����(byte) */

typedef struct {
  /* Packet ID */
      /* Astro-E �Ǹ��ꤵ��Ƥ�����ʬ */
  int version;				/* Astro-E �Ǥ� 3 �˸��� */
  int type;				/* Astro-E �Ǥ� 0 �˸��� */
  int secHeaderFlag;			/* Astro-E �Ǥ� 1 �˸��� */
       /* APID */
  int common;				/* common use */
  int subsystemID;			/* ���� ID */
  int subsystemMisc;			/* ����Ǥ�� */

  /* Packet sequence count */
  int seqFlag;				/* sequence flag */
  int seqCount;				/* sequence count */
      /* packet legth */
  int packetLength;			/* secondary header+data-1 */

  /* secondary header */
  unsigned int secHeader;		/* �������*/
} AEccsdsHeader;

/****************************************************************
 *		APID Common					*
*****************************************************************/
#define AE_APID_COMMON_RESERVED	3
#define AE_APID_COMMON_HK	2
#define AE_APID_COMMON_MEM	1
#define AE_APID_COMMON_DATA	0

/****************************************************************
 *		APID Subsystem ID				*
*****************************************************************/
#define	AE_APID_DHU_ID	0x01
#define	AE_APID_AOCU_ID	0x02
#define	AE_APID_TCI_ID	0x03
#define	AE_APID_HCE_ID	0x04
#define	AE_APID_DIST_ID	0x05
#define	AE_APID_DP_ID	0x06
#define	AE_APID_XRS_ID	0x07
#define	AE_APID_XIS_ID	0x08
#define	AE_APID_HXD_ID	0x09
#define	AE_APID_HK_ID	0x0a

#define	AE_APID_SPARE0_ID	0x0b
#define	AE_APID_SPARE1_ID	0x0c
#define	AE_APID_SPARE2_ID	0x0d
#define	AE_APID_SPARE3_ID	0x0e
#define	AE_APID_SPARE4_ID	0x0f

/****************************************************************
 *		VCDU						*
*****************************************************************/
/****************************************************************
 *		VCID						*
*****************************************************************/
/* real */
#define AE_VCID_REAL_TIME_DP		0x00
#define AE_VCID_REAL_SATM_COM		0x01
#define AE_VCID_REAL_SATM_DP_OBS	0x02
#define AE_VCID_REAL_OBS_XRS		0x03
#define AE_VCID_REAL_OBS_XIS		0x04
#define AE_VCID_REAL_OBS_HXD		0x05

/* repro */
#define AE_VCID_REPRO_SATM_COM		0x21
#define AE_VCID_REPRO_SATM_DP_OBS	0x22
#define AE_VCID_REPRO_OBS_XRS		0x23
#define AE_VCID_REPRO_OBS_XIS		0x24
#define AE_VCID_REPRO_OBS_HXD		0x25

