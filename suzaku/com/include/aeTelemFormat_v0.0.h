/*###############################################################
#								#
#		Astro-E   E. Miyata@OsakaUniv.			#
#	ver 0.0 updated 1996/8/29	Emi Miyata		#
#								#
################################################################*/
/****************************************************************
*		CCSDS packet					*
*****************************************************************/
typedef struct {
  /* Packet ID */
      /* Astro-E �Ǹ��ꤵ��Ƥ�����ʬ */
  u_int version:3;			/* Astro-E �Ǥ� 3 �˸��� */
  u_int type:1;				/* Astro-E �Ǥ� 0 �˸��� */
  u_int secondaryHeaderFlag:1;		/* Astro-E �Ǥ� 1 �˸��� */
       /* APID */
  u_int common:2;			/* common use */
  u_int subsystemID:4;			/* ���� ID */
  u_int sensorMisc:5;			/* ����Ǥ�� */

  /* Packet sequence count */
  u_int sequenceFlag:2;			/* sequence flag */
  u_int sequenceCount:14;		/* sequence count */
      /* packet legth */
  u_int packetLength:16;		/* secondary header+data-1 */

  /* secondary header */
  u_int secondaryHeader:16;		/* �������*/
} AEccsdsHeaderC;

#define	AEccsdsPacketCSize	8	/* CCSDS packet header�Υ�����(byte) */

typedef struct {
  /* Packet ID */
      /* Astro-E �Ǹ��ꤵ��Ƥ�����ʬ */
  int version;				/* Astro-E �Ǥ� 3 �˸��� */
  int type;				/* Astro-E �Ǥ� 0 �˸��� */
  int secondaryHeaderFlag;		/* Astro-E �Ǥ� 1 �˸��� */
       /* APID */
  int common;				/* common use */
  int subsystemID;			/* ���� ID */
  int sensorMisc;			/* ����Ǥ�� */

  /* Packet sequence count */
  int sequenceFlag;			/* sequence flag */
  int sequenceCount;			/* sequence count */
      /* packet legth */
  int packetLength;			/* secondary header+data-1 */

  /* secondary header */
  unsigned int secondaryHeader;		/* �������*/
} AEccsdsHeader;

