/* xrsTelemFormat.h */

#define APID_DP_HK	0x4C0
#define APID_FILL	0x7ff

#define APID_XRS_ACHE_HK	0x4E0
#define APID_XRS_ACHE_ECHO	0x4E2
#define APID_XRS_ACHE_MEM_CODE	0x2E2
#define APID_XRS_ACHE_MEM_DATA	0x2E3
#define APID_XRS_CAPA_HK	0x4E4
#define APID_XRS_CAPA_ECHO	0x4E6
#define APID_XRS_CAPB_HK	0x4E8
#define APID_XRS_CAPB_ECHO	0x4EA
#define APID_XRS_CDPA_HK	0x4EC
#define APID_XRS_CDPA_ECHO	0x4EE
#define APID_XRS_CDPA_MEM	0x2EE
#define APID_XRS_CDPA_MAIN	0x0EC
#define APID_XRS_CDPB_HK	0x4F0
#define APID_XRS_CDPB_ECHO	0x4F2
#define APID_XRS_CDPB_MEM	0x2F2
#define APID_XRS_CDPB_MAIN	0x0F0
#define APID_XRS_FW_MAIN	0x428
#define APID_XRS_FW_HK		0x430
#define APID_XRS_CDE_HK		0x430
#define APID_XRS_DIST		0x428

typedef enum {
	XRS_MAIN_PHA,
	XRS_MAIN_DUMP,
	XRS_MAIN_RATES_0,
	XRS_MAIN_RATES_1,
	XRS_MAIN_RATES_2,
	XRS_MAIN_RATES_3,
	XRS_MAIN_HK_0,
	XRS_MAIN_HK_1,
	XRS_MAIN_HK_2,
	XRS_MAIN_REPLY,
	XRS_MAIN_ERROR,
	XRS_MAIN_UNKNOWN,
	XRS_MAIN_UNDEF
} XRSmainDataType;

typedef union {
	unsigned short w[4];
} XRSmainData;

typedef struct {
	int side;
	int pixel;
	int pulseQuality;
	int secondary;
	int midRes;
	int lowRes;
	int baseline;
	int antiCo;
	int clipped;
	int pulseHeight;
	int riseTime;
	int tickCounter;
	int timeVernier;
	int sampleCount;
	int paritySent;
	int parityError;
} XRSphaData;
