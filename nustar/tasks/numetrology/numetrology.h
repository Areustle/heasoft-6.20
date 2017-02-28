/*
 *	numetrology.h: definitions and declarations for numetrology
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NUMETROLOGY_H
#define NUMETROLOGY_H

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

#define PAR_METFLAG     	 "metflag"
#define PAR_METROLOGYFILE	 "metrologyfile"
#define PAR_METGRIDFILE 	 "metgridfile"
#define PAR_INPSDFILECOR  	 "inpsdfilecor"
#define PAR_OUTPSDFILE  	 "outpsdfile"
#define PAR_OUTPSDFILECOR  	 "outpsdfilecor"
#define PAR_ALIGNFILE   	 "alignfile"
#define PAR_MASTASPECTFILE   	 "mastaspectfile"
#define PAR_PSDCAL      	 "psdcal"
/* Init test */
/* #define PAR_INPSDFILE   	 "inpsdfile" */
/* End test */

/* miscellaneous */
#define BINTAB_ROWS           1000
#define MET_BLOCK_ROWS        1000     /* Must be > 1 */
#define PSD_BLOCK_ROWS        1000     /* Must be > 1 */

#define TIME_SENS             0.0000001

#define X_PSD0_SCALE          10.0598
#define Y_PSD0_SCALE          10.0598
#define X_PSD1_SCALE          10.1169
#define Y_PSD1_SCALE          10.1169


typedef struct {
  unsigned TIME;
  unsigned XA_PSD0;
  unsigned XB_PSD0;
  unsigned YA_PSD0;
  unsigned YB_PSD0;
  unsigned XA_PSD1;
  unsigned XB_PSD1;
  unsigned YA_PSD1;
  unsigned YB_PSD1;
  unsigned LAS_ON;
  unsigned CLAMP;
  /* Init test */
/*   unsigned X_TMP0; */
/*   unsigned Y_TMP0; */
/*   unsigned X_TMP1; */
/*   unsigned Y_TMP1; */
  /* End test */
} MetrologyCol_t;

typedef struct {
  double   time;
  int      xa_psd0;
  int      xb_psd0;
  int      ya_psd0;
  int      yb_psd0;
  int      xa_psd1;
  int      xb_psd1;
  int      ya_psd1;
  int      yb_psd1;
  unsigned las_on;
  unsigned clamp;
  /* Init test */
/*   float    x_tmp0; */
/*   float    y_tmp0; */
/*   float    x_tmp1; */
/*   float    y_tmp1; */
  /* End test */
} MetrologyRow_t;

typedef struct {
  double   time;
  float    x_psd0;
  float    y_psd0;
  float    x_psd1;
  float    y_psd1;
  float    x0_int;
  float    y0_int;
  float    x1_int;
  float    y1_int;
  BTYPE    metgrid_flag;
} MetrologyInfo_t;

typedef struct {
  unsigned TIME;
  unsigned X_PSD0;
  unsigned Y_PSD0;
  unsigned X_PSD1;
  unsigned Y_PSD1;
  unsigned X0_INT;
  unsigned Y0_INT;
  unsigned X1_INT;
  unsigned Y1_INT;
  unsigned METGRID_FLAG;
} PSDCol_t;

typedef struct {
  char   hduclass[FLEN_VALUE];
  char	 hduclass_comm[FLEN_COMMENT];
  char   hduclas1[FLEN_VALUE];
  char	 hduclas1_comm[FLEN_COMMENT];
  float  timepixr;
  char	 timepixr_comm[FLEN_COMMENT];
  char   obs_id[FLEN_VALUE];
  char	 obs_id_comm[FLEN_COMMENT];
  JTYPE  targ_id;
  char	 targ_id_comm[FLEN_COMMENT];
  char   timesys[FLEN_VALUE];
  char	 timesys_comm[FLEN_COMMENT];
  JTYPE  mjdrefi;
  char	 mjdrefi_comm[FLEN_COMMENT];
  double mjdreff;
  char	 mjdreff_comm[FLEN_COMMENT];
  LTYPE  clockapp;
  char	 clockapp_comm[FLEN_COMMENT];
  char   timeunit[FLEN_VALUE];
  char	 timeunit_comm[FLEN_COMMENT];
  double tstart;
  char	 tstart_comm[FLEN_COMMENT];
  double tstop;
  char	 tstop_comm[FLEN_COMMENT];
  char   dateobs[FLEN_VALUE];
  char	 dateobs_comm[FLEN_COMMENT];
  char   dateend[FLEN_VALUE];
  char	 dateend_comm[FLEN_COMMENT];
} MetrologyKeys_t;


typedef struct {
  unsigned X_STAGE;
  unsigned Y_STAGE;
  unsigned X_PSD;
  unsigned Y_PSD;
  unsigned DELTAX;
  unsigned DELTAY;
} MetGridCol_t;

typedef struct {
  float x_stage;
  float y_stage;
  float x_psd;
  float y_psd;
  float deltax;
  float deltay;
} MetGridInfo_t;


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
  int    ix;
  int    iy;
  double dist;
} PointInfo_t;


typedef struct {
  /* char   instrume[FLEN_VALUE]; */
  SPTYPE dateobs;
  SPTYPE timeobs;
  SPTYPE dateend;
  SPTYPE timeend;
} ObsInfo_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        metrologyfile[PIL_LINESIZE];
    char        metgridfile[PIL_LINESIZE];
    char        inpsdfilecor[PIL_LINESIZE];
    char        outpsdfile[PIL_LINESIZE];
    char        outpsdfilecor[PIL_LINESIZE];
    char        alignfile[PIL_LINESIZE];
    char        mastaspectfile[PIL_LINESIZE];
    /* Init test */
/*     char        inpsdfile[PIL_LINESIZE]; */
    /* End test */
    BOOL        metflag;
    BOOL        psdcal;
  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];
  char metinfofile[PIL_LINESIZE];
  /* Init test */
/*   BOOL inpsdfile; */
  /* End test */
  ObsInfo_t obsinfo;

} Global_t;

extern Global_t global;


/* Prototypes */
int numetrology();
int numetrology_work(void);
int numetrology_getpar(void);
void numetrology_info(void);
int numetrology_checkinput(void);

int ReadMetrologyFile(char *filename, char *extname, MetrologyRow_t **info, const unsigned initRow, unsigned *numRows, BOOL *endfile);

int ReadPSDFile(char *filename, MetrologyInfo_t **info, const unsigned initRow, unsigned *numRows, BOOL *endfile);
int WritePSDFile(char *metfile, MetrologyKeys_t *metkeys, MetGridInfo_t metgrid[2][PSD_ROWS][PSD_PIXS], char *outfile, BOOL psdcal);
int WritePSDExt(char *metfile, FitsFileUnit_t ounit, MetrologyKeys_t *metkeys, MetGridInfo_t metgrid[2][PSD_ROWS][PSD_PIXS], BOOL psdcal);

int ReadMetGridFile(MetGridInfo_t metgrid[2][PSD_ROWS][PSD_PIXS]);
int ReadMetGridInfo(MetGridInfo_t metgrid[PSD_ROWS][PSD_PIXS], char *filename, long extno, char *psdid);

int CorrectPosition(MetGridInfo_t metgrid[PSD_ROWS][PSD_PIXS], float x, float y, float *xcorr, float *ycorr);

int ReadAlignFile(AlignInfo_t *aligninfo);
int ReadAlignInfo(char *filename, AlignInfo_t *aligninfo);

int ComputeMastAspectSolution(MetrologyInfo_t met, AlignInfo_t *align, MastInfo_t *mast);

int WriteMastAspectFile(char *psdfile, MetrologyKeys_t *metkeys, AlignInfo_t *aligninfo, char *outfile);
int WriteMastAspectExt(char *psdfile, FitsFileUnit_t ounit, MetrologyKeys_t *metkeys, AlignInfo_t *aligninfo);

int RotoTranslate(AtQuat Quat, AtVect Tr, AtVect vec, AtVect outvec);
int ComputeOrthonormalBasis(AtVect v1, AtVect v2, AtRotMat M);
int ComputeRotationMatrix(AtRotMat M1, AtRotMat M2, AtRotMat RM);

int GetMetrologyKeys(char *filename, char *extname, MetrologyKeys_t *metkeys);

int GetObsInfo(char *filename, char *extname);

#endif
