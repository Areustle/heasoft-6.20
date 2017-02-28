/*
 *	nusplitsc.h: definitions and declarations for nusplitsc
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *
 *
 */
#ifndef NUSPLITSC_H
#define NUSPLITSC_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility, standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h> /* for getpid */
#include <limits.h>
#include <sys/stat.h>   /* needed by 'mkdir' */
#include <sys/types.h>  /* needed by 'mkdir' */
#include <errno.h>


/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>

/* nustar local headers */
#include "nu_highfits.h"
#include "nu_termio.h"
#include "nu_defs.h"
#include "nu_misc.h"
#include "nu_caldb.h"
#include "nustardasversion.h"


#ifndef errno
extern int errno;
#endif

/*******************************
*      defines / typedefs      *
********************************/
#define VECVECACCESS_PT(type, a, b, c)	\
                        ((type *)(a).cols[c]) + (b*(a).Multiplicity[c])

#define VECVEC_ARRAY_READ(type, var, dim, a, b, c) \
                          memcpy(var,VECVECACCESS_PT(type,a,b,c), sizeof(type)*dim)
#define VECVEC_ARRAY_WRITE(type, var, dim, a, b, c) \
                          memcpy(VECVECACCESS_PT(type,a,b,c), var, sizeof(type)*dim)

#define DVECVEC_ARRAY_READ(var, dim, a, b, c) VECVEC_ARRAY_READ(DTYPE, var, dim, a, b, c)
#define DVECVEC_ARRAY_WRITE(var, dim, a, b, c) VECVEC_ARRAY_WRITE(DTYPE, var, dim, a, b, c)


/* input parameter names */
#define PAR_INFILE				"infile"
#define PAR_CHU123HKILE				"chu123hkfile"
#define PAR_HFILE				"hkfile"
#define PAR_CHU123RANGEFILE			"chu123rangefile"
#define PAR_OUTDIR				"outdir"
#define PAR_SPLITMODE				"splitmode"
#define PAR_TIMECUT				"timecut"
#define PAR_STEMOUT				"stemout"
#define PAR_CLEANUP				"cleanup"

/* extension names */
#define EXTNAME_CHU1	"CHU1"
#define EXTNAME_CHU2	"CHU2"
#define EXTNAME_CHU3	"CHU3"

/* extension id */
#define ID_CHU1     	0
#define ID_CHU2     	1
#define ID_CHU3     	2
#define ID_GTI		3
#define ID_STDGTI	2

#define KWNM_NAXIS2		"NAXIS2"
#define CARD_COMM_NAXIS2 	"number of rows in table"
#define KWNM_EXTNAME 		"EXTNAME"
#define CARD_COMM_EXTNAME 	"name of this binary table extension"

#define NUM_EXTENSION			3
#define NUM_COLUMNS			15
#define NUM_COLUMNS_CHU123RANGEFILE	5
#define	NUM_EXPRSCREENING		7

/* columns name CHU file*/
#define CLNM_CHUQ			"CHUQ"
#define CLNM_SEQUENCE		 	"SEQUENCE"
#define CLNM_CORR			"CORR"
#define CLNM_CHUID			"CHUID"
#define CLNM_HIRATE			"HIRATE"
#define CLNM_BBO			"BBO"
#define CLNM_TIMEREF			"TIMEREF"
#define CLNM_VALID			"VALID"
#define CLNM_RESIDUAL			"RESIDUAL"
#define CLNM_LOCKS			"LOCKS"
#define CLNM_OBJECTS			"OBJECTS"
#define CLNM_STARSFAIL			"STARSFAIL"
#define CLNM_AGCFLOOR			"AGCFLOOR"
#define CLNM_AGCCEIL			"AGCCEIL"

/* columns name CHU Merge file*/
#define CLNM_CHUQ1			"CHUQ1"
#define CLNM_SEQUENCE1			"SEQUENCE1"
#define CLNM_CORR1			"CORR1"
#define CLNM_CHUID1			"CHUID1"
#define CLNM_HIRATE1			"HIRATE1"
#define CLNM_BBO1			"BBO1"
#define CLNM_TIMEREF1			"TIMEREF1"
#define CLNM_VALID1			"VALID1"
#define CLNM_RESIDUAL1			"RESIDUAL1"
#define CLNM_LOCKS1			"LOCKS1"
#define CLNM_OBJECTS1			"OBJECTS1"
#define CLNM_STARSFAIL1			"STARSFAIL1"
#define CLNM_AGCFLOOR1			"AGCFLOOR1"
#define CLNM_AGCCEIL1			"AGCCEIL1"
#define CLNM_CHUQ2			"CHUQ2"
#define CLNM_SEQUENCE2			"SEQUENCE2"
#define CLNM_CORR2			"CORR2"
#define CLNM_CHUID2			"CHUID2"
#define CLNM_HIRATE2			"HIRATE2"
#define CLNM_BBO2			"BBO2"
#define CLNM_TIMEREF2			"TIMEREF2"
#define CLNM_VALID2			"VALID2"
#define CLNM_RESIDUAL2			"RESIDUAL2"
#define CLNM_LOCKS2			"LOCKS2"
#define CLNM_OBJECTS2			"OBJECTS2"
#define CLNM_STARSFAIL2			"STARSFAIL2"
#define CLNM_AGCFLOOR2			"AGCFLOOR2"
#define CLNM_AGCCEIL2			"AGCCEIL2"
#define CLNM_CHUQ3			"CHUQ3"
#define CLNM_SEQUENCE3			"SEQUENCE3"
#define CLNM_CORR3			"CORR3"
#define CLNM_CHUID3			"CHUID3"
#define CLNM_HIRATE3			"HIRATE3"
#define CLNM_BBO3			"BBO3"
#define CLNM_TIMEREF3			"TIMEREF3"
#define CLNM_VALID3			"VALID3"
#define CLNM_RESIDUAL3			"RESIDUAL3"
#define CLNM_LOCKS3			"LOCKS3"
#define CLNM_OBJECTS3			"OBJECTS3"
#define CLNM_STARSFAIL3			"STARSFAIL3"
#define CLNM_AGCFLOOR3			"AGCFLOOR3"
#define CLNM_AGCCEIL3			"AGCCEIL3"


/* columns name Chu123 range file file*/
#define KWVL_CHU123RANGE	"chu123range"
#define KWVL_CHU123RANGE_EXT1	"data.eq.chu23"

#define CLNM_PARNAME		"PARNAME"
#define CLNM_RANGE		"RANGE"
#define CLNM_SYNTAX_PAR		"SYNTAX_PAR"
#define CLNM_GROUP		"GROUP"
#define CLNM_SYNTAX_GROUP	"SYNTAX_GROUP"


/* Temporary definitions to put in nu_defs.h file!  */
#define CLNM_DET1X			"DET1X"		/* Event X position Focal Plane Bench Frame */
#define CARD_COMM_DET1X			"Event X position Focal Plane Bench Frame"

#define CLNM_DET1Y			"DET1Y"		/* Event Y position Focal Plane Bench Frame */
#define CARD_COMM_DET1Y			"Event Y position Focal Plane Bench Frame"


/* miscellaneous */
#define BINTAB_ROWS       		1000
#define TIME_EQUAL			-1
#define STR_EQUAL 			0
#define	DF_STEAMOUT			"DEFAULT"
#define	SPLITMODE_NORMAL		"NORMAL"
#define	SPLITMODE_STRICT		"STRICT"
#define LEN_STEMOUT			13
#define CHECKEMPTY			1
#define OUTPUT_IN_WORK_DIR		1

typedef struct {
    SPTYPE dateobs;
    SPTYPE timeobs;
    SPTYPE dateend;
    SPTYPE timeend;
} ObsInfo_t;


typedef struct {
    unsigned TIME;
    unsigned RAWX;
    unsigned RAWY;
    unsigned DET_ID;
    unsigned GRADE;
    unsigned DET1X;
    unsigned DET1Y;
} EvtCol_t;

typedef struct {
    unsigned TIME;
    unsigned CHUQ;
    unsigned SEQUENCE;
    unsigned CORR;
    unsigned CHUID;
    unsigned HIRATE;
    unsigned BBO;
    unsigned TIMEREF;
    unsigned VALID;
    unsigned RESIDUAL;
    unsigned LOCKS;
    unsigned OBJECTS;
    unsigned STARSFAIL;
    unsigned AGCFLOOR;
    unsigned AGCCEIL;
} ChuCol_t;

typedef struct {
    unsigned TIME;
    unsigned CHUQ1;
    unsigned SEQUENCE1;
    unsigned CORR1;
    unsigned CHUID1;
    unsigned HIRATE1;
    unsigned BBO1;
    unsigned TIMEREF1;
    unsigned VALID1;
    unsigned RESIDUAL1;
    unsigned LOCKS1;
    unsigned OBJECTS1;
    unsigned STARSFAIL1;
    unsigned AGCFLOOR1;
    unsigned AGCCEIL1;
    unsigned CHUQ2;
    unsigned SEQUENCE2;
    unsigned CORR2;
    unsigned CHUID2;
    unsigned HIRATE2;
    unsigned BBO2;
    unsigned TIMEREF2;
    unsigned VALID2;
    unsigned RESIDUAL2;
    unsigned LOCKS2;
    unsigned OBJECTS2;
    unsigned STARSFAIL2;
    unsigned AGCFLOOR2;
    unsigned AGCCEIL2;
    unsigned CHUQ3;
    unsigned SEQUENCE3;
    unsigned CORR3;
    unsigned CHUID3;
    unsigned HIRATE3;
    unsigned BBO3;
    unsigned TIMEREF3;
    unsigned VALID3;
    unsigned RESIDUAL3;
    unsigned LOCKS3;
    unsigned OBJECTS3;
    unsigned STARSFAIL3;
    unsigned AGCFLOOR3;
    unsigned AGCCEIL3;
} ChuMergeCol_t;


typedef struct {
    unsigned PARNAME;
    unsigned RANGE;
    unsigned SYNTAX;
    unsigned GROUP;
    unsigned GROUP_SYNTAX;
} Chu123rangefile_T;

typedef struct {
    char instrume[FLEN_VALUE];
} EVTInfo_t;

typedef struct {
    Bintable_t	  	chuTable[NUM_EXTENSION];
    FitsHeader_t	chuHead[NUM_EXTENSION];
    FitsFileUnit_t	chuFileFU[NUM_EXTENSION];
    ChuCol_t    	indxcol;

    Bintable_t	  	chuTableMerge;
    FitsHeader_t	chuHeadMerge;
    FitsFileUnit_t	chuFileMergeFU;
    ChuMergeCol_t	indxcolMerge;

    char 		tempChuFileName[NUM_EXTENSION][BUF_SIZE];
    unsigned int 	rowChu[NUM_EXTENSION];
    unsigned int 	maxRow_chu[NUM_EXTENSION];
    unsigned int 	rowWrite_chu123;
    BOOL		flagRead[NUM_EXTENSION];
    unsigned		nCols[NUM_EXTENSION];
    double 		time[NUM_EXTENSION];
    int			indexMinTime;
    DTYPE		chuq[NUM_EXTENSION][4];
    unsigned		*rowValue[NUM_EXTENSION];
    JTYPE		mjdrefi[NUM_EXTENSION];
    double		mjdreff[NUM_EXTENSION];
} ChuMerge_t;

typedef struct {
    char 		colNameOrig[NUM_COLUMNS][BUF_SIZE];
    char		colComment[NUM_COLUMNS][BUF_SIZE];
    char 		colType[NUM_COLUMNS][BUF_SIZE];
    char 		colUnit[NUM_COLUMNS][BUF_SIZE];
} ChuColDef_t;

typedef struct {
    char 		exprScreening[NUM_EXPRSCREENING][BUF_SIZE];
    char		makeTimeCmd[NUM_EXPRSCREENING][BUF_SIZE];
    char		xselectCmd[NUM_EXPRSCREENING][BUF_SIZE];
    char		nulivetimeCmd[NUM_EXPRSCREENING][BUF_SIZE];
    int 		N[NUM_EXPRSCREENING];

    char		gti_chu123hkfile[NUM_EXPRSCREENING][PIL_LINESIZE];
    char		xselectFile[NUM_EXPRSCREENING][PIL_LINESIZE];
    char		xselectLogFile[PIL_LINESIZE];
    char		xselectTimeFile[PIL_LINESIZE];
    char		output_evtFile[NUM_EXPRSCREENING][PIL_LINESIZE];
} CmdNuSplitSC_t;




typedef struct {
    /*----------   Input Parameters ---------------------*/
    struct {
        /* Asked param */
        char		infile[PIL_LINESIZE];       			/* Name of input SCIENCE_SC FITS(06) Event File  */
        char		chu123hkfile[PIL_LINESIZE];       /* Name of input CHU123 Housekeeping FITS File  */
        char		hkfile[PIL_LINESIZE];							/* Name of input Housekeeping FITS file  */
        char		outdir[PIL_LINESIZE];      			/* Name of output SCIENCE_SC FITS Event File */

        /* Hidden param */
        char		chu123rangefile[PIL_LINESIZE];		/* Name of the input fits CHU123RANGE file or CALDB */
        char		splitmode[PIL_LINESIZE];					/* CHU123 splitting mode (NORMAL,STRICT) */
        BOOL    	timecut;													/* Cut non simultaneous CHU time intervals (yes/no)? */
        char		stemout[PIL_LINESIZE];						/* Stem for the output files or DEFAULT to use the stem of input files[i.e. nu00000000000] */
        BOOL    	cleanup;													/* Delete temporary file? (yes/no)*/
    } par;


    /*----------------------------------------------------------------*/
    char 		taskname[FLEN_FILENAME]; 									/* Name and version of the task */
    Version_t 		nustardas_v;          								/* NuSTARDAS version */
    BOOL 		hist;
    BOOL 		warning;
    char		strhist[256];
    char		date[25];
    char		tmpfile[PIL_LINESIZE];
    char		merge_chu123hkfile[PIL_LINESIZE];					/* Name of output CHU Merged FITS File  */
    char		postfr[20];
    EVTInfo_t		evt;
    ObsInfo_t		obsinfo;


} Global_t;


extern Global_t global;


/* Prototypes */

/* Standard function */
int nusplitsc();
int nusplitsc_work(void);
int nusplitsc_getpar(void);
void nusplitsc_info(void);
int nusplitsc_checkinput(void);
int nusplitsc_checkoutput(CmdNuSplitSC_t *_cmd);
int ComputeDETCoords(FitsFileUnit_t evunit, FitsFileUnit_t ounit);
int GetObsInfo(char *filename, char *extname);
int getInstrument();

/* NuSplitSc specific function */
int splitChuFile();
void subString (const char* input, int offset, int len, char* dest);
void extractNameFile(char *sorg,char *dest);
void preSteam(const char *path,char* dest);
void preSteamOut(char* dest);
void getStemOut(const char *path,char* dest);
void postSteam(const char *path,char* dest);
void refreshStemOut(const char *path,const char* par_stemout,char* dest);

void createNameChuTempFile(const char *nameFileSorg,int num_extension,char *sliptChuFileName);
void createNameChuMergeFile(const char *nameFileSorg,char *sliptChuFileName);
void createNameChuGTIFile(const char *nameFileSorg,int N,char *gtiChuFileName);
void createNameEVTFile(const char *nameFileSorg,int N,char *evtFileName);

int loadCALDBfile(char *ext_name);
int mergeChuFile();
int readRowChuFile(ChuMerge_t *_chuObj,int chuId);
void iGetBintableStructure(ChuMerge_t *_chuObj, int chuId);
void iGetBintableMergeStructure(ChuMerge_t *_chuObj);
void printRow(ChuMerge_t *_chuObj,int chuId);
void printIndex(ChuMerge_t *_chuObj,int flag);
int getIndexCol(ChuMerge_t *_chuObj);
int getIndexMergeCol(ChuMerge_t *_chuObj);
int cleanNuSplitScTempFile(CmdNuSplitSC_t *_cmd);
void initIndex(ChuMerge_t *_chuObj);
double checkTime(ChuMerge_t *_chuObj);
void incAll(ChuMerge_t *_chuObj);
void incMinor(ChuMerge_t *_chuObj,double minore);
void decOthers(ChuMerge_t *_chuObj,double minore);
int checkIndex(ChuMerge_t *_chuObj);
int copyHeader();
int AddKeyPrimatyExtensionChuFile(ChuMerge_t *chuObj);
int updateKeyChuMerge(ChuMerge_t *_chuObj);
int initChuFileTemp(ChuMerge_t *_chuObj);
int writeChuMerge(ChuMerge_t *_chuObj);
void  readChuColType(ChuColDef_t *_chuColDef);

int makeExprScreening(char *name_ExprScreening,char *expr);
int makeOutputEvtFileName(CmdNuSplitSC_t *_cmd);
int makeCMD(CmdNuSplitSC_t *_cmd);
int execMaketime(CmdNuSplitSC_t *_cmd);
int execXselect(CmdNuSplitSC_t *_cmd);
int execNulivetime(CmdNuSplitSC_t *_cmd);

int checkEmptyfile(CmdNuSplitSC_t *_cmd);
int checkOutputEmptyfile(CmdNuSplitSC_t *_cmd);

int nusplitsc_report(CmdNuSplitSC_t *_cmd);
#endif
