/*
 *	nucalcpha.h: definitions and declarations for nucalcpha
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NUCALCPHA_H
#define NUCALCPHA_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h> /* for getpid */

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
/* input parameter names */

#define PAR_INFILE		 "infile"
#define PAR_OUTFILE		 "outfile"
#define PAR_OFFSETFILE		 "offsetfile"
#define PAR_GRADEFILE		 "gradefile"
#define PAR_PHAPARFILE		 "phaparfile"
#define PAR_EVTTHR		 "evtthr"
#define PAR_TIMERISE		 "timerise"
#define PAR_CLEANCOLS		 "cleancols"

/* miscellaneous */
#define BINTAB_ROWS       1000

#define MAX_EVTTHR        INT_MAX


typedef struct {
  unsigned TIME;
  unsigned NUMRISE;
  unsigned DENRISE;
  unsigned POSTPHAS;
  unsigned PREPHAS;
  unsigned RAWX;
  unsigned RAWY;
  unsigned DET_ID;
  unsigned S_CAP;
  unsigned RAWPHAS;
  unsigned OFFPHAS;
  unsigned TRPHAS;
  unsigned PHAS;
/*   unsigned PHA; */
/*   unsigned SURR; */
  unsigned GRADE;
  unsigned SWTRIG;
  unsigned BADPOS;
  unsigned HOTPOS;
} EvtCol_t;

typedef struct {
  char instrume[FLEN_VALUE];
  SPTYPE dateobs;
  SPTYPE timeobs;
  SPTYPE dateend;
  SPTYPE timeend;
} EVTInfo_t;


typedef struct {
  unsigned GRADEID;
  unsigned GRADE;
} GradeCol_t;

typedef struct {
  int gradeid;
  int grade[PHAS_MOL];
} GradeRow_t;


typedef struct {
  float offset[CAP_DIM];
  unsigned set;
} OffsetInfo_t;


typedef struct {
  unsigned RAWX;
  unsigned RAWY;
  unsigned EVTTHR;
  unsigned TIMERISE;
} PhaParCol_t;

typedef struct {
  int   evtthr;
  float timerise;
} PhaParData_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];       /* Name of input FITS file  */
    char        outfile[PIL_LINESIZE];      /* Name of output FITS file */
    char        offsetfile[PIL_LINESIZE];   /* Name of input OFFSET file */
    char        gradefile[PIL_LINESIZE];    /* Name of input GRADE file */
    char        phaparfile[PIL_LINESIZE];   /* Name of input PHAPAR file */
    int         evtthr;
    double      timerise;
    BOOL        cleancols;
  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  BOOL hist;
  BOOL warning;
  BOOL phaparfile;
  char strhist[256];
  char date[25];
  char tmpfile[PIL_LINESIZE];
  EVTInfo_t evt;
  GradeRow_t *gradevalues;
  int graderows;
  int grade_tot[33];
} Global_t;

extern Global_t global;


/* Prototypes */
int nucalcpha();
int nucalcpha_work(void);
int nucalcpha_getpar(void);
void nucalcpha_info(void);
int nucalcpha_checkinput(void);

int ComputePHAandGRADE(FitsFileUnit_t evunit, FitsFileUnit_t ounit, OffsetInfo_t offset_det0[DET_PIXS][DET_ROWS], OffsetInfo_t offset_det1[DET_PIXS][DET_ROWS], OffsetInfo_t offset_det2[DET_PIXS][DET_ROWS], OffsetInfo_t offset_det3[DET_PIXS][DET_ROWS], PhaParData_t phapardata[4][DET_PIXS][DET_ROWS]);

int GetGrades(long);
int GetOffset(OffsetInfo_t offset_det0[DET_PIXS][DET_ROWS], OffsetInfo_t offset_det1[DET_PIXS][DET_ROWS], OffsetInfo_t offset_det2[DET_PIXS][DET_ROWS], OffsetInfo_t offset_det3[DET_PIXS][DET_ROWS]);

int ReadPhaParFile(PhaParData_t phapardata[4][DET_PIXS][DET_ROWS]);
int ReadPhaParInfo(PhaParData_t phapardata[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam);

#endif
