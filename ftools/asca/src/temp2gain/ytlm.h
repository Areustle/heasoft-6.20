/*
 * ytlm.h
 *
 * telemetry file access routine header		'91/12/17	Y.Ishisaki
 *
 */

/*********************  my local defines **************************/

#ifndef SPC
#define SPC		0x20
#endif
typedef enum { gFALSE=0, gTRUE=(!0) } gBOOLEAN;
#ifndef SEEK_SET
#define SEEK_SET	0
#define SEEK_CUR	1
#define SEEK_END	2
#endif

#ifndef elif
#define until(a)	while(!(a))
#define unless(a)	if(!(a))
#define elif		else if
#define null(p)		((p)==NULL)
#endif

/*************** defines and structures for telemetry file ***********/

#ifndef FRWD
#define FRWD		128		/* WORDS/1-FRAME */
#define SFFR		64		/* FRAMES/1-SUB-FRAME */
#define fr_FI(fr)	((fr)[3])	/* Frame Indicater */

typedef unsigned char WD;
typedef WD FR[FRWD];
typedef FR SF[SFFR];

#define ENV_ASTROD_ROOT		"ASTROD_ROOT"	/* environ variable for it */
#endif

/*   kanri-record   */
struct	kanrirec {
	int			recmen;
	int			qlflg;
	int			newdno;
	char			kspace[132];
};

/*   jyoho-record   */
struct	jyohorec {
	int			stpath;
	int			strev;
	int			sttrb;
	int			tono;
	int			datano;
	char			satno[4];
	char			pathno[12];
	char			totday[4];
	char			rlmark[4];
	char			revday[8];
	unsigned	char	strtime[8];
	unsigned	char	endtime[8];
	char			logname[44];
	int			bkupinf;
	int			rlstiu;
	int			pbstiu;
	int			pbcnt;
	char			kuhaku[16];
};

/*   message-data   */
struct	msgdata {
	unsigned	char	hdmark[2];
	unsigned	char	satno[2];
	unsigned	char	pathno[5];
	char			cfiller;
	unsigned	char	startno;
	unsigned	char	unitno;
	unsigned	char	dmode;
	unsigned	char	satwave;
	unsigned	char	today[2];
	unsigned	char	pathday[3];
	unsigned	char	pathtime[5];
	unsigned	char	datamen[3];
	char			yobi[5];
	char			aki[32];
	char			comment[80];
};

/*   frame-data   */
struct	framedata {
	unsigned	char	today[2];
	unsigned	char	ftime[3];
	unsigned	char	fms[2];
	char			space[5];
	unsigned		demst:4;
	unsigned		tmst:4;
	unsigned		bitr:8;
	unsigned	short	sn;
	unsigned	char	tlmdata[128];
};

#ifndef _ADTIME_DEF_
#define _ADTIME_DEF_

typedef enum {
	TIME_IS_ADTIME, TIME_IS_MJD, TIME_IS_ASCATIME, TIME_IS_ATTIME
} TIME_TYPE;

typedef struct {
	int year;
	int month;
	int day;
	int hour;
	int minute;
	int second;
	int msec;
} ADTIME;

typedef double MJD;

typedef double ASCATIME;

#endif

/***************** some proto-typing **********************/

extern unsigned _QLSKIP;	/* skip frame, if delay is greater than this */
extern TIME_TYPE _TIME_TYPE_;	/* 0:ADTIME, 1:MJD */

int ascatool_tlmopen(char *fn);
int ascatool_tlmread(FR *frp, ADTIME *adt);
int ascatool_tlmclose(void);
long ascatool_tlmsfnum(void);
char* ascatool_tlmpass(void);

int sfGet(SF *sfp, int *sfn, ADTIME *adt);
int frameGet(FR *frp, int *sfn, ADTIME *adt);

/* sfGet return values */
#define QL_END		9
#define QL_NODATA	1
#define QL_ERROR	-1
