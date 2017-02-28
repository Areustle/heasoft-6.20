/*
 *	nucalcpos.h: definitions and declarations for nucalcpos
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NUCALCPOS_H
#define NUCALCPOS_H

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
#include <fitsio.h>	 /* Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>
#include <atFunctions.h> /* "At" library */

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
  Macros to put in nu_highfits.h file
********************************/

#define VECVECACCESS_PT(type, a, b, c)	\
                        ((type *)(a).cols[c]) + (b*(a).Multiplicity[c])

#define VECVEC_ARRAY_READ(type, var, dim, a, b, c) \
                          memcpy(var,VECVECACCESS_PT(type,a,b,c), sizeof(type)*dim)
#define VECVEC_ARRAY_WRITE(type, var, dim, a, b, c) \
                          memcpy(VECVECACCESS_PT(type,a,b,c), var, sizeof(type)*dim)

#define DVECVEC_ARRAY_READ(var, dim, a, b, c) VECVEC_ARRAY_READ(DTYPE, var, dim, a, b, c)
#define DVECVEC_ARRAY_WRITE(var, dim, a, b, c) VECVEC_ARRAY_WRITE(DTYPE, var, dim, a, b, c)



/*******************************
*      defines / typedefs      *
********************************/
/* input parameter names */

#define PAR_INFILE		 "infile"
#define PAR_OUTFILE		 "outfile"
#define PAR_PIXPOSFILE		 "pixposfile"
#define PAR_SAVERAW2COORD        "saveraw2coord"
#define PAR_INITSEED     	 "initseed"
#define PAR_ALIGNFILE   	 "alignfile"
#define PAR_MASTASPECTFILE   	 "mastaspectfile"

/* miscellaneous */
#define BINTAB_ROWS           1000
#define MAST_BLOCK_ROWS       1000     /* Must be > 1 */

#define PIXPOS_PDF_WIDTH      7
#define PIXPOS_PDF_HEIGHT     7
#define PIXPOS_PDF_DIM        ( PIXPOS_PDF_WIDTH * PIXPOS_PDF_HEIGHT )

#define CUMPROB_SENS          0.01
#define TIME_SENS             0.0000001


typedef struct {
  unsigned TIME;
  unsigned RAWX;
  unsigned RAWY;
  unsigned DET_ID;
  unsigned GRADE;
  unsigned DET1X;
  unsigned DET1Y;
  unsigned RAW2X;
  unsigned RAW2Y;
  unsigned DET2X;
  unsigned DET2Y;
} EvtCol_t;

typedef struct {
  char instrume[FLEN_VALUE];
  SPTYPE dateobs;
  SPTYPE timeobs;
  SPTYPE dateend;
  SPTYPE timeend;
  DTYPE  tstart;
  DTYPE  tstop;
} EVTInfo_t;


typedef struct {
  unsigned RAWX;
  unsigned RAWY;
  unsigned GRADE;
  unsigned REF_DET1X;
  unsigned REF_DET1Y;
  unsigned PDF;
} PixPosCol_t;

typedef struct {
  int      grade;
  int      ref_det1x;
  int      ref_det1y;
  float    pdf[PIXPOS_PDF_DIM];
} PixPosInfo_t;

typedef struct {
  PixPosInfo_t *info;
  int          ninfo;
} PixPosData_t;


typedef struct {
  unsigned Q_FB_MD0;
  unsigned Q_FB_MD1;
  unsigned V_FB_MD0;
  unsigned V_FB_MD1;
  unsigned Q_OB_ML0;
  unsigned Q_OB_ML1;
  unsigned V_OB_ML0;
  unsigned V_OB_ML1;
  unsigned Q_FB_OB;
  unsigned V_FB_OB;
  unsigned L0_ORIG;
  unsigned L1_ORIG;
  unsigned L0_POINT;
  unsigned L1_POINT;  

  unsigned Q_FPMA_DET1;
  unsigned Q_FPMB_DET1;
  unsigned V_FPMA_DET1;
  unsigned V_FPMB_DET1;

  unsigned Q_FB_FPMA;
  unsigned Q_FB_FPMB;
  unsigned V_FB_FPMA;
  unsigned V_FB_FPMB;

  unsigned Q_DET2A_OB;
  unsigned Q_DET2B_OB;
  unsigned V_DET2A_OB;
  unsigned V_DET2B_OB;

} AlignCol_t;

typedef struct {
  DTYPE Qfbmd0[4];
  DTYPE Qfbmd1[4]; 
  DTYPE Tfbmd0[3];
  DTYPE Tfbmd1[3];
  DTYPE Qobml0[4];
  DTYPE Qobml1[4];
  DTYPE Tobml0[3];
  DTYPE Tobml1[3];
  DTYPE Qfbob[4];
  DTYPE Tfbob[3];
  DTYPE L0ml[3];
  DTYPE L1ml[3];
  DTYPE D0ob[3];
  DTYPE D1ob[3];

  DTYPE Qfpm0det1[4];
  DTYPE Qfpm1det1[4];
  DTYPE Tfpm0det1[3];
  DTYPE Tfpm1det1[3];

  DTYPE Qfbfpm0[4];
  DTYPE Qfbfpm1[4];
  DTYPE Tfbfpm0[3];
  DTYPE Tfbfpm1[3];

  DTYPE Qdet2Aob[4];
  DTYPE Qdet2Bob[4];
  DTYPE Tdet2Aob[3]; 
  DTYPE Tdet2Bob[3];

} AlignInfo_t;


typedef struct {
  double time;
  double Tfbob[3];
  double Qfbob[4];
} MastInfo_t;

typedef struct {
  unsigned TIME;
  unsigned T_FBOB;
  unsigned Q_FBOB;
} MastExtCol_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];       /* Name of input FITS file  */
    char        outfile[PIL_LINESIZE];      /* Name of output FITS file */
    char        pixposfile[PIL_LINESIZE];   /* Name of input pixel location FITS file  */
    char        alignfile[PIL_LINESIZE];
    char        mastaspectfile[PIL_LINESIZE];
    BOOL        saveraw2coord;
    BOOL        initseed;
  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];
  char tmpfile[PIL_LINESIZE];
  EVTInfo_t evt;


} Global_t;

extern Global_t global;


/* Prototypes */
int nucalcpos();
int nucalcpos_work(void);
int nucalcpos_getpar(void);
void nucalcpos_info(void);
int nucalcpos_checkinput(void);

int ReadPixPosFile(PixPosData_t pixpos[4][DET_PIXS][DET_ROWS]);
int ReadPixPosInfo(PixPosData_t pixpos[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam);

int ReadAlignInfo(char *filename, AlignInfo_t *aligninfo);
int ReadMastAspectSolutionInfo(char *filename, MastInfo_t ** mastinfo, const unsigned initRow, unsigned *numRows, BOOL *mastend);

int ComputeDETCoords(FitsFileUnit_t evunit, AlignInfo_t *aligninfo, char *mastfile, FitsFileUnit_t ounit);
int ComputeDET1XY(int rawx, int rawy, int det_id, int grade, PixPosData_t pixpos[DET_PIXS][DET_ROWS], int *det1x, int *det1y, long int *seed);
int ComputeDET2XY(int det1x, int det1y, double time, AlignInfo_t *align, MastInfo_t *mast, int mastcount, long int *seed, char *instr, int *det2x, int *det2y);

int GetFBtoOBinfo(double time, MastInfo_t *mast, int mastcount, AtVect Tfbob, AtRotMat Rfbob);

void InterpolateValues(double down, double up, double this, double valuedown, double valueup, double *value);


#endif
