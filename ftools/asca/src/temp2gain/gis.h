/*
 * gis.h
 *
 *	GIS information header		Y.Ishisaki
 *					ishisaki@miranda.phys.s.u-tokyo.ac.jp
 * Version 1.0		92/04/06
 * Version 1.1		93/07/08	New GIS format
 */

#define NEW_GIS_FORMAT

#define FRWD	128		/* number of words in a frame */
#define SFFR	64		/* number of frames in a sub-frame */
#define g1SEC	(1L<<14)	/* 1 sec in GIS time step */

typedef enum { gOFF, gON } gOnOff;
typedef enum { gDIS, gENA } gEnaDis;
typedef enum { gPOW2, gFLF } gMETHOD;
typedef enum { gGIS2, gGIS3 } gGISID;
typedef enum { gCPU2, gCPU3 } gCPUID;
typedef enum { gX, gY } gXY;
typedef enum { gCPUin, gCPUout, gLDhit, gL2, gH2, gL1, gH1, gL0, gH0 } gMONI;

typedef unsigned char WD;
typedef WD FR[FRWD];
typedef FR SF[SFFR];

#if defined(mips) || defined(__alpha) || !defined(unix)
#define SWAPHL		/* define for MIPS,8086,... */
#endif

#ifdef __alpha
#define	DWORD	int
#else
#define	DWORD	long
#endif

#ifdef SWAPHL

typedef struct {
	unsigned LDhit	:2;
	unsigned skip1	:2;
	unsigned L1	:2;
	unsigned skip2	:2;
} gScaleDown;

typedef struct {
	unsigned skip		:3;
	unsigned SelfConnect	:1;
	unsigned MPCPHbit	:2;
	unsigned GIS3CPU	:1;
	unsigned GIS2CPU	:1;
} gCPUselect;

typedef struct {
	unsigned SP	:1;
	unsigned RT	:2;
	unsigned YP	:2;
	unsigned XP	:2;
	unsigned PH	:1;
} gTMmode;

typedef struct {
	unsigned LD		:2;
	unsigned FineGainCont	:6;
} gGainCont;

typedef WD gRiseTimeLD;

typedef WD gRiseTimeUD;

typedef struct {
	unsigned GHVL	:3;
	unsigned skip1	:1;
	unsigned GHVH	:3;
	unsigned skip2	:1;
} gHVcontrol;

typedef struct {
	unsigned CPU3norm	:1;
	unsigned CPU3run	:1;
	unsigned GIS3on		:1;
	unsigned CPU2norm	:1;
	unsigned CPU2run	:1;
	unsigned GIS2on		:1;
	unsigned BYPSon		:1;
	unsigned GISAon		:1;
	unsigned BCena		:1;
	unsigned HAMerr		:1;
	unsigned MEMCHKon	:1;
	unsigned MODEmpc	:1;
	unsigned PCALon		:1;
	unsigned RBFGSon	:1;
	unsigned RBFGGon	:1;
	unsigned RBMon		:1;
	unsigned ADR3ena	:1;
	unsigned ADR2ena	:1;
	unsigned GHVREDon	:1;
	unsigned GHVH3on	:1;
	unsigned GHVL3on	:1;
	unsigned GHVH2on	:1;
	unsigned GHVL2on	:1;
	unsigned GHVena		:1;
} gGISstatus;

typedef struct {
	unsigned PHtune		:1;
	unsigned SpreadDiscri	:1;
	unsigned RadiusDiscri	:1;
	unsigned PositionMethod	:1;
	unsigned AnodeGainTune	:1;
	unsigned AnodeSel	:1;
	unsigned RTcondense	:2;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
} gGISgeneral;

typedef struct {
	unsigned reserved	:4;
	unsigned Mask2Region	:1;
	unsigned Mask2Use	:1;
	unsigned Mask1Logic	:1;
	unsigned Mask1Use	:1;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
} gMaskControl;

typedef struct {
	unsigned AllowdXray	:1;
	unsigned Mask2Out	:1;
	unsigned DiscriOut	:1;
	unsigned CalcOut	:1;
	unsigned Mask1Out	:1;
	unsigned Edge		:1;
	unsigned AllAnode0	:1;
	unsigned AnyAnodeFF	:1;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
} gPHeventSelect;

typedef struct {
	unsigned AllowdXray	:4;
	unsigned Mask2Out	:4;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
} gEventScaleDown;

typedef struct {
	unsigned PositionDist	:4;
	unsigned reserved	:4;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
} gDistScaleDown;

typedef DWORD gPositionCenter;

typedef DWORD gRadiusDiscriR;

typedef DWORD gEventCounter;

typedef DWORD gMask2;

typedef DWORD gProgramVersion;

typedef DWORD gAddressMonitor;

typedef struct {
	unsigned a7		:1;
	unsigned a6		:1;
	unsigned a5		:1;
	unsigned a4		:1;
	unsigned a3		:1;
	unsigned a2		:1;
	unsigned a1		:1;
	unsigned a0		:1;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
	unsigned aF		:1;
	unsigned aE		:1;
	unsigned aD		:1;
	unsigned aC		:1;
	unsigned aB		:1;
	unsigned aA		:1;
	unsigned a9		:1;
	unsigned a8		:1;
	unsigned dummy4		:8;
	unsigned dummy5		:8;
	unsigned dummy6		:8;
} gAnodeUse;

typedef struct {
	unsigned a7		:1;
	unsigned a6		:1;
	unsigned a5		:1;
	unsigned a4		:1;
	unsigned a3		:1;
	unsigned a2		:1;
	unsigned a1		:1;
	unsigned a0		:1;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
	unsigned aF		:1;
	unsigned aE		:1;
	unsigned aD		:1;
	unsigned aC		:1;
	unsigned aB		:1;
	unsigned aA		:1;
	unsigned a9		:1;
	unsigned a8		:1;
	unsigned dummy4		:8;
	unsigned dummy5		:8;
	unsigned dummy6		:8;
} gMask1;

#else	/* !SWAPHL */

typedef struct {
	unsigned skip2	:2;
	unsigned L1	:2;
	unsigned skip1	:2;
	unsigned LDhit	:2;
} gScaleDown;

typedef struct {
	unsigned GIS2CPU	:1;
	unsigned GIS3CPU	:1;
	unsigned MPCPHbit	:2;
	unsigned SelfConnect	:1;
	unsigned skip		:3;
} gCPUselect;

typedef struct {
	unsigned PH	:1;
	unsigned XP	:2;
	unsigned YP	:2;
	unsigned RT	:2;
	unsigned SP	:1;
} gTMmode;

typedef struct {
	unsigned FineGainCont	:6;
	unsigned LD		:2;
} gGainCont;

typedef WD gRiseTimeLD;

typedef WD gRiseTimeUD;

typedef struct {
	unsigned skip2	:1;
	unsigned GHVH	:3;
	unsigned skip1	:1;
	unsigned GHVL	:3;
} gHVcontrol;

typedef struct {
	unsigned GISAon		:1;
	unsigned BYPSon		:1;
	unsigned GIS2on		:1;
	unsigned CPU2run	:1;
	unsigned CPU2norm	:1;
	unsigned GIS3on		:1;
	unsigned CPU3run	:1;
	unsigned CPU3norm	:1;
	unsigned RBMon		:1;
	unsigned RBFGGon	:1;
	unsigned RBFGSon	:1;
	unsigned PCALon		:1;
	unsigned MODEmpc	:1;
	unsigned MEMCHKon	:1;
	unsigned HAMerr		:1;
	unsigned BCena		:1;
	unsigned GHVena		:1;
	unsigned GHVL2on	:1;
	unsigned GHVH2on	:1;
	unsigned GHVL3on	:1;
	unsigned GHVH3on	:1;
	unsigned GHVREDon	:1;
	unsigned ADR2ena	:1;
	unsigned ADR3ena	:1;
} gGISstatus;

typedef struct {
	unsigned RTcondense	:2;
	unsigned AnodeSel	:1;
	unsigned AnodeGainTune	:1;
	unsigned PositionMethod	:1;
	unsigned RadiusDiscri	:1;
	unsigned SpreadDiscri	:1;
	unsigned PHtune		:1;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
} gGISgeneral;

typedef struct {
	unsigned Mask1Use	:1;
	unsigned Mask1Logic	:1;
	unsigned Mask2Use	:1;
	unsigned Mask2Region	:1;
	unsigned reserved	:4;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
} gMaskControl;

typedef struct {
	unsigned AnyAnodeFF	:1;
	unsigned AllAnode0	:1;
	unsigned Edge		:1;
	unsigned Mask1Out	:1;
	unsigned CalcOut	:1;
	unsigned DiscriOut	:1;
	unsigned Mask2Out	:1;
	unsigned AllowdXray	:1;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
} gPHeventSelect;

typedef struct {
	unsigned Mask2Out	:4;
	unsigned AllowdXray	:4;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
} gEventScaleDown;

typedef struct {
	unsigned reserved	:4;
	unsigned PositionDist	:4;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
} gDistScaleDown;

typedef DWORD gPositionCenter;

typedef DWORD gRadiusDiscriR;

typedef DWORD gEventCounter;

typedef DWORD gMask2;

typedef DWORD gProgramVersion;

typedef DWORD gAddressMonitor;

typedef struct {
	unsigned a0		:1;
	unsigned a1		:1;
	unsigned a2		:1;
	unsigned a3		:1;
	unsigned a4		:1;
	unsigned a5		:1;
	unsigned a6		:1;
	unsigned a7		:1;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
	unsigned a8		:1;
	unsigned a9		:1;
	unsigned aA		:1;
	unsigned aB		:1;
	unsigned aC		:1;
	unsigned aD		:1;
	unsigned aE		:1;
	unsigned aF		:1;
	unsigned dummy4		:8;
	unsigned dummy5		:8;
	unsigned dummy6		:8;
} gAnodeUse;

typedef struct {
	unsigned a0		:1;
	unsigned a1		:1;
	unsigned a2		:1;
	unsigned a3		:1;
	unsigned a4		:1;
	unsigned a5		:1;
	unsigned a6		:1;
	unsigned a7		:1;
	unsigned dummy1		:8;
	unsigned dummy2		:8;
	unsigned dummy3		:8;
	unsigned a8		:1;
	unsigned a9		:1;
	unsigned aA		:1;
	unsigned aB		:1;
	unsigned aC		:1;
	unsigned aD		:1;
	unsigned aE		:1;
	unsigned aF		:1;
	unsigned dummy4		:8;
	unsigned dummy5		:8;
	unsigned dummy6		:8;
} gMask1;

#endif /* SWAPHL */

typedef struct {
	DWORD LowerA;
	DWORD UpperA;
	DWORD B;
	DWORD UpperC;
	DWORD PHH2;
	DWORD PHL8;
} gSpreadDiscri;

typedef struct {
	WD PH, XP, YP, RT, SP, TIM;	/* (1<<PH) == full scale */
	DWORD dt;			/* time step in 1/2**14 sec */
} gPHbits;

typedef struct {
	DWORD time;		/* 1/2**14 sec from top of SF */
	unsigned short PH;
	WD XP, YP, RT, SP;
	unsigned short TIM;	/* 0 for PCAL mode */
	WD anode[2][16];	/* 0 for PH mode */
} gPHevent;

typedef struct {
	gPHbits bits;
	unsigned short nevents;	/* number of events */
	gPHevent *event;	/* event[nevents] */
} gPHdata;

typedef struct {
	unsigned DWORD time;	/* 1/2**14 sec from top of SF */
	WD *PH;			/* PH[(1<<gPHbits.PH)] */
} gMPCspec;

typedef struct {
	gPHbits bits;
	unsigned short nspec;	/* number of spectre */
	gMPCspec *spec;		/* spec[nspec] */
} gMPCdata;

typedef struct {		/* non reset count, scale-down uncompensated */
	WD bits;		/* (1<<bits)==counter full scale. 8/6(PCAL) */
	WD scale;		/* ordinary 1, pre-scaled 4/8/32 */
	WD n;			/* count index max 0/16/32 */
	DWORD dt; 		/* 1/2**14 sec between count[i] & count[i+1] */
	WD count[32];		/* non-reset-counter value. count[n] */
} gGISmoni_;

typedef struct {
	short XYdist[2][32];	/* if not set, -1 */
	gGISmoni_ *b;		/* points at gGISmoni.CPUin */
	gGISmoni_ CPUin, CPUout, LDhit, L2, H2, L1, H1, L0, H0;
} gGISmoni;

typedef struct {
	struct {
		WD	raw;
		double	cvt;	/* converted to V, uA, C. if not set, -9999 */
	} HVLV, HVHV, HVHI, T;
} gGISHK;

typedef struct {
	DWORD *b;		/* point to gBlockTable.general */
	gGISgeneral		general;
	gMaskControl		maskcont;
	gPHeventSelect		PHevent;
	gAnodeUse		anode[2];	/* Xanode/Yanode */
	gMask1			mask1[2];	/* X/Y */
	gPositionCenter		center[2][2];	/* FLF/POW2, X/Y */
	gRadiusDiscriR		discR[2];	/* FLF/POW2 */
	gMask2			mask2[2][2];	/* X/Y, min/max */
	DWORD			tbl21, tbl22;
	gEventScaleDown		evntsd;
	gProgramVersion		pgver;
	DWORD			tbl25, tbl26, tbl27;
	gSpreadDiscri		SPdisc[2];	/* FLF/POW2 */
	gEventCounter		mon[8];
	gDistScaleDown		distsd;
	gAddressMonitor		addmon;
} gBlockTable;

typedef struct {
	gGISmoni	moni;
	gGainCont	gain;
	gRiseTimeLD	RTLD;
	gRiseTimeUD	RTUD;
	gHVcontrol	HV;
	gScaleDown	scale;
	gTMmode		tmm;
	gBlockTable	tbl;
	gGISHK		hk;
	gPHdata		ph;
	gMPCdata	mpc;
} gGISinfo;

typedef struct {
	gGISstatus	stat;
	gCPUselect	cpu;
	gGISinfo	gis[2];
	WD		work[8*64*sizeof(gPHevent)];
} GISINFO;

void GISget(SF *sfp, GISINFO *ginfo);
void fastGISget(int do_gis2, int do_gis3, SF *sfp, GISINFO *gi);
void showGISINFO(FILE *fp, GISINFO *ginfo);
int SFcheck(SF *sfp);

#undef DWORD
