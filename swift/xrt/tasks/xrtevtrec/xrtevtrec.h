/*
 *	xrtevtrec.h: definitions and declarations for xrtevtrec
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTEVTREC_H
#define XRTEVTREC_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <unistd.h> /* for getpid */
#include <xrttimefunc.h>
/* xrt local headers */
#include <highfits.h>
#include <xrt_termio.h>
#include <xrtfits.h>
#include <xrtdefs.h>
#include <xrtcaldb.h>
#include <misc.h>
#include <xrtdasversion.h>
#include <xrtbpfunc.h>
/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>


#ifndef errno
extern int errno;
#endif

/*******************************
*      defines / typedefs      *
********************************/

/* input parameter names */

#define PAR_INFILE		   "infile"
#define PAR_HDFILE		   "hdfile"
#define PAR_OUTFILE		   "outfile"
#define PAR_GRADEFILE		   "gradefile"
#define PAR_THRFILE		   "thrfile"
#define PAR_EVENT 		   "event"
#define PAR_SPLIT 		   "split"
#define PAR_ADDCOL 		   "addcol"
#define PAR_DELNULL 		   "delnull"
#define PAR_FLAGNEIGH 		   "flagneigh"

/* miscellaneous */
#define BUFSIZE           1000        /* temporary strings buffer size */
#define CLEN_NAME           81        /* C Columns name length */
#define BINTAB_ROWS       1000

#define STEMLEN             14
#define PHAS7_MOL            7
#define NONREC              17
#define NULLREC             16

#define KWVL_EXTNAME_TMGRADES     "TMGRADES"
#define KWVL_GRADENONREC          -2

typedef struct {
  int amp;
  int gradeid;
  int grade[PHAS7_MOL];
} TMGradeRow_t;

typedef struct {
  unsigned CCDFrame;
  unsigned OFFSET;
  unsigned RAWX;
  unsigned RAWY;
  unsigned PHA;  
  unsigned EVTPHA;  
  unsigned PHAS; 
  unsigned GRADE;  
  unsigned ABOVE;
  unsigned Amp;
  unsigned STATUS;
  unsigned TIME_RO;
} Ev2Col_t;

typedef struct {
  unsigned CCDFrame;
  unsigned XRTMode;
  unsigned Settled;
  unsigned In10Arcm;
  unsigned InSafeM;
  unsigned EvtLLD;
  unsigned FSTS;
  unsigned FSTSub;
  unsigned FETS;
  unsigned FETSub;
  unsigned nPixels;
}HKCol_t;

typedef struct {
  int    xrtvsub;
  char datamode[15];
  SPTYPE dateobs;
  SPTYPE timeobs;
} EVTInfo_t;

typedef struct {
  int nrow;
  int pha;
  int offset;
  int ccdframe;
} Ev2Info_t;

typedef struct {
  int ccdframe;
  double hktime;
  int split;
} WTSplitValue_t;

typedef struct {
  int ccdframe;
  double hktime;
  int split;
  int lastoffset;
} PDSplitValue_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char infile[PIL_LINESIZE];     /* Name of FITS file and [ext#] */
    char hdfile[PIL_LINESIZE]; /* Name of FITS output file */
    char outfile[PIL_LINESIZE]; /* Name of FITS output file */
    char gradefile[PIL_LINESIZE]; /* Name of FITS output file */
    char thrfile[PIL_LINESIZE];
    int split;
    int event;
    BOOL addcol;
    BOOL delnull;
    BOOL flagneigh;
  } par;
/*------------------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  TMGradeRow_t *gradevalues;
  BOOL wt;
  BOOL lr;
  BOOL pu;
  BOOL hist;
  BOOL warning;
  BOOL usethrfile;
  int amp;
  int onboardsplit;
  int onboardevent;
  char strhist[256];
  char date[25];
  char tmpfile[FLEN_FILENAME];
  int maxrows;
  int graderows;
  int totrows;
  int tot_sat;
  double per_tot_sat;
  int grade[TOT_TIM_GRADE];
  double per_grade[TOT_TIM_GRADE];
  JTYPE grademin;
  JTYPE grademax;
  int ret;
  int scount;
  PDSplitValue_t *split_pd;
  WTSplitValue_t *split_wt;
  int lastoffset;
  EVTInfo_t evt;
  
} Global_t;

extern Global_t global;

/* I/O Data structure, windowed timing mode columns from:  
Swift XRT Science FITS Format Design v0.6 June 29,2001) */


/* Prototypes */
void PrintStatistics(void);
int DelCols(FitsFileUnit_t evunit);
int GetGrades(long extno);
int xrtevtrec();
int xrtevtrec_work();
int xrtevtrec_getpar();
void xrtevtrec_info();
int xrtevtrec_checkinput(void);
int WTEventRec(FitsFileUnit_t inevent,FitsFileUnit_t ounit);
int WTAssignGrade(int *, int, Bintable_t , Ev2Col_t, unsigned *OutRows);
int WTReadHK(char *obsmode);
int PDReadHK(char *obsmode);
int PDEventRec(FitsFileUnit_t inevent,FitsFileUnit_t ounit);
int InitBintable(FitsHeader_t *head, Bintable_t *intable, unsigned *nCols, Ev2Col_t *indxcol, int blockn);
int RemoveNullEvents(Bintable_t *table, unsigned *nRows, int colnum);
int AddStatKeywords(FitsFileUnit_t *evunit);
void SetNonRecEvent(Bintable_t , int,  Ev2Col_t);
void FillWT7x1Buff(int count, int stop, int rawx_count, BOOL *first_time, Bintable_t intable, Ev2Col_t indxcol, Ev2Info_t tmp_buff[PHAS7_MOL]);
int TMEvtRec7x1(Ev2Info_t tmp_buff[PHAS7_MOL], Ev2Col_t indxcol, Bintable_t intable, unsigned *OutRows);
void SetTMNullEvent(Bintable_t , int,  Ev2Col_t);
int IsLocalMax(Ev2Info_t tmp_buff[PHAS7_MOL]);
int PDEvtRec(Ev2Info_t tmp_buff[PHAS7_MOL], Ev2Col_t indxcol, Bintable_t intable, unsigned *OutRows);
int faint7x1(int handle[PHAS7_MOL], int *pha, int *grade, int *above);
int FillPD7x1Buff(int count, int offset_count, int ccdframe_count, BOOL *first_time, Bintable_t intable, Ev2Col_t indxcol, Ev2Info_t tmp_buff[PHAS7_MOL]);
int FillPD7x1LastRows(int count, int offset_count, int ccdframe_count, BOOL *first_time, Bintable_t intable, Ev2Col_t indxcol, Ev2Info_t tmp_buff[PHAS7_MOL]);
#endif
