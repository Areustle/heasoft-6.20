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
      /* Astro-E で固定されている部分 */
  u_int version:3;			/* Astro-E では 3 に固定 */
  u_int type:1;				/* Astro-E では 0 に固定 */
  u_int secondaryHeaderFlag:1;		/* Astro-E では 1 に固定 */
       /* APID */
  u_int common:2;			/* common use */
  u_int subsystemID:4;			/* 機器 ID */
  u_int sensorMisc:5;			/* 機器任意 */

  /* Packet sequence count */
  u_int sequenceFlag:2;			/* sequence flag */
  u_int sequenceCount:14;		/* sequence count */
      /* packet legth */
  u_int packetLength:16;		/* secondary header+data-1 */

  /* secondary header */
  u_int secondaryHeader:16;		/* 時刻情報*/
} AEccsdsHeaderC;

#define	AEccsdsPacketCSize	8	/* CCSDS packet headerのサイズ(byte) */

typedef struct {
  /* Packet ID */
      /* Astro-E で固定されている部分 */
  int version;				/* Astro-E では 3 に固定 */
  int type;				/* Astro-E では 0 に固定 */
  int secondaryHeaderFlag;		/* Astro-E では 1 に固定 */
       /* APID */
  int common;				/* common use */
  int subsystemID;			/* 機器 ID */
  int sensorMisc;			/* 機器任意 */

  /* Packet sequence count */
  int sequenceFlag;			/* sequence flag */
  int sequenceCount;			/* sequence count */
      /* packet legth */
  int packetLength;			/* secondary header+data-1 */

  /* secondary header */
  unsigned int secondaryHeader;		/* 時刻情報*/
} AEccsdsHeader;

