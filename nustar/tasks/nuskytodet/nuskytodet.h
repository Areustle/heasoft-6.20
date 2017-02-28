/*
 *	nuskytodet.h: definitions and declarations for nuskytodet
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NUSKYTODET_H
#define NUSKYTODET_H

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
#include <coordfits.h>
#include <teldef.h>
#include <ephemeris.h>
#include <headas_gti.h>
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

#define DVECVEC_ARRAY_READ(var, dim, a, b, c) VECVEC_ARRAY_READ(DTYPE, var, dim, a, b, c)


/*******************************
*      defines / typedefs      *
********************************/
/* input parameter names */

#define PAR_PNTRA		 "pntra"
#define PAR_PNTDEC		 "pntdec"
#define PAR_ATTFILE		 "attfile"
#define PAR_ALIGNFILE		 "alignfile"
#define PAR_TELDEF		 "teldef"
#define PAR_INSTRUMENT		 "instrument"
#define PAR_ABERRATION		 "aberration"
#define PAR_SKYXREF              "skyxref"
#define PAR_SKYYREF              "skyyref"
#define PAR_MASTASPECTFILE       "mastaspectfile"
#define PAR_SKYDETFILE           "skydetfile"
#define PAR_INITSEED     	 "initseed"

/* miscellaneous */
#define BINTAB_ROWS       1000
#define TIME_SENS         0.0000001
#define MAST_BLOCK_ROWS   1000     /* Must be > 1 */

#define KWVL_EXTNAME_DET_COORD    "DET_COORD" 


typedef struct {
  double detx;
  double dety;
  double x;
  double y;
  double v;
  double vhat[3];
} Coord_t;


typedef struct {
  unsigned X_DET2A;
  unsigned Y_DET2A;
  unsigned X_DET2B;
  unsigned Y_DET2B;

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
  DTYPE  x_det2a;
  DTYPE  y_det2a;
  DTYPE  x_det2b;
  DTYPE  y_det2b;

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
  char   object[FLEN_VALUE];
  char	 object_comm[FLEN_COMMENT];
  DTYPE  raobj;
  char	 raobj_comm[FLEN_COMMENT];
  DTYPE  decobj;
  char	 decobj_comm[FLEN_COMMENT];
  DTYPE  ranom;
  char	 ranom_comm[FLEN_COMMENT];
  DTYPE  decnom;
  char	 decnom_comm[FLEN_COMMENT];
  DTYPE  rapnt;
  char	 rapnt_comm[FLEN_COMMENT];
  DTYPE  decpnt;
  char	 decpnt_comm[FLEN_COMMENT];
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
  /* needed by CALDB query */
  /* (used only if dateobs and timeobs not in '2000-01-01T00:00:00' format ) */
  char   timeobs[FLEN_VALUE];
  char   timeend[FLEN_VALUE];

} AttitudeKeys_t;

typedef struct {
  double time;
} AttitudeInfo_t;


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
  unsigned TIME;
  unsigned DET1X;
  unsigned DET1Y;
  unsigned DET2X;
  unsigned DET2Y;
} SkyDetCol_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        attfile[PIL_LINESIZE];
    char        alignfile[PIL_LINESIZE];
    char        teldef[PIL_LINESIZE];
    char        instrument[PIL_LINESIZE];
    char        mastaspectfile[PIL_LINESIZE];
    char        skydetfile[PIL_LINESIZE];
    BOOL        aberration;
    double      pntra;
    double      pntdec;
    double      skyxref;
    double      skyyref;
    BOOL        initseed;

  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];

} Global_t;

extern Global_t global;


/* Prototypes */
int nuskytodet();
int nuskytodet_work(void);
int nuskytodet_getpar(void);
void nuskytodet_info(void);
int nuskytodet_checkinput(void);

int ReadAlignInfo(char *filename, AlignInfo_t *aligninfo);
int GetAttitudeTimes(char *filename, AttitudeInfo_t ** info, int *attcount, double *mjdref, AttitudeKeys_t *attkeys);
int GetAttitudeKeys(FitsHeader_t head, char *filename, AttitudeKeys_t *keys);

int WriteSkyDetFile(AttitudeInfo_t *attinfo, int attcount, double mjdref, AttitudeKeys_t *attkeys, AlignInfo_t *aligninfo, char *infile, char *outfile);
int WriteSkyDetExt(AttitudeInfo_t *attinfo, int attcount, double mjdref, AttitudeKeys_t *attkeys, AlignInfo_t *aligninfo, FitsFileUnit_t ounit);

int ReadMastAspectSolutionInfo(char *filename, MastInfo_t ** mastinfo, const unsigned initRow, unsigned *numRows, BOOL *mastend);

int ConvertSkyToDet2Coords(double skyx, double skyy, int amp, double mjdref, double time, TELDEF *teldef, ATTFILE *attfile, Coord_t *coord);
int ConvertDet2ToDet1Coords(int det2x, int det2y, double time, AlignInfo_t *align, MastInfo_t *mast, int mastcount, long int *seed, char *instr, int *det1x, int *det1y);

int GetFBtoOBinfo2(double time, MastInfo_t *mast, int mastcount, AtVect Tfbob, AtQuat Qfbob);
void InterpolateValues(double down, double up, double this, double valuedown, double valueup, double *value);


#endif
