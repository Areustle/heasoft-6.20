#ifndef MJD_J2000
typedef struct	{
	int yr;		/* year  */
	int mo; 	/* month */
	int dy; 	/* day */
	int hr; 	/* hour */
	int mn; 	/* minuit */
	int sc; 	/* sec */
	float ms; 	/* ms */
} AtTime;
#endif

#define ENV_FRFDIR	"FRFDIR"
#define ENV_FADIR	"FADIR"
#define FTRECSIZE	2880

typedef struct {
	char mtime[8];		/* mission time */
	char eti[4];		/* extended satellite clock */
	SF sf;
} FRFREC;

typedef struct {
	char mtime[8];		/* mission time */
	WD fi[SFFR];		/* FI and SYNC code */
	WD comwd[SFFR][22];	/* common word without SIS */
	WD map[SFFR];		/* GIS data map */
	int gdata[SFFR*8];	/* GIS data */
} GISREC;

typedef struct {
	WD map[SFFR];		/* GIS data map */
	int gdata[SFFR*8];	/* GIS data */
} PACK_METH_1;

typedef struct {
	WD ndata[2];		/* number of data */
	struct { WD pos, body[4]; } gdata[SFFR*8];
} PACK_METH_2;

typedef union {
	PACK_METH_1 m1;	/* for 62 <= n */
	PACK_METH_2 m2;	/* for n < 62 */
} PACK_DATA;

typedef struct {
	WD hk[2];	/* HK temperature */
	WD meth;	/* b0-3:bit-rate, b4-5:FI, method 0:none, 3:sync */
	PACK_DATA pack;
} PACKREC;

typedef struct {
	WD hk[7];
	WD stat[3];
	WD bcos[2*SFFR];
} PACKSTAT;

typedef struct {
	WD meth;	/* b0-3:bit-rate, b4-5:FI, method 0:none, 3:sync */
	WD hk[9];	/* GIS HK */
	WD dpmode;	/* DP mode & RBM status flag */
	WD status[3];	/* GIS status word */
} GGISREC;

typedef struct {
	char mtime[8];
	int pos;
} FRFINDEX;

typedef struct {
	char h[80];
} FitsHead;

struct yfrf_global {
	char *fn;
	FILE *fp;
	long top;
	unsigned long eti, nrec, recsiz, sfn, frn;
	double endtime;		/* changed to ASCA time 93/08/10 */
	FitsHead *h1;
	FitsHead *h2;
	int gisfrf;
	FitsHead *h3;
	FRFINDEX *idx;
	FRFREC frec;
	PACKSTAT pstat;
};

struct yfrf_global* get_yfrf_global(void);
int ascatool_frfopen(char *fn);
int ascatool_frfread(FR *frp, ADTIME *ji);
int ascatool_frfclose(void);
long ascatool_frfsfnum(void);
