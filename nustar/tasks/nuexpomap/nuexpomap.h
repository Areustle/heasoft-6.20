/*
 *	nuexpomap.h: definitions and declarations for nuexpomap
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NUEXPOMAP_H
#define NUEXPOMAP_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h> /* for getpid */

#include <sys/stat.h>   /* needed by 'mkdir' */
#include <sys/types.h>  /* needed by 'mkdir' */


/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>
#include <headas_gti.h>
#include <atFunctions.h> /* "At" library */

/* nustar local headers */
#include "nu_highfits.h"
#include "nu_termio.h"
#include "nu_defs.h"
#include "nu_misc.h"
#include "nu_caldb.h"
#include "nu_badpix.h"
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

#define EVECVEC_ARRAY_READ(var, dim, a, b, c) VECVEC_ARRAY_READ(ETYPE, var, dim, a, b, c)
#define EVECVEC_ARRAY_WRITE(var, dim, a, b, c) VECVEC_ARRAY_WRITE(ETYPE, var, dim, a, b, c)


/*******************************
*      defines / typedefs      *
********************************/
/* input parameter names */

#define PAR_INFILE		 "infile"
#define PAR_PIXPOSFILE		 "pixposfile"
#define PAR_ALIGNFILE   	 "alignfile"
#define PAR_MASTASPECTFILE   	 "mastaspectfile"
#define PAR_ATTFILE		 "attfile"
#define PAR_TELDEF		 "teldef"
#define PAR_INSTRPROBMAPFILE	 "instrprobmapfile"
#define PAR_VIGNFILE		 "vignfile"
#define PAR_INDET2INSTRFILE	 "indet2instrfile"
#define PAR_ABERRATION		 "aberration"
#define PAR_VIGNFLAG		 "vignflag"
#define PAR_ENERGY		 "energy"
#define PAR_DET1REFFILE		 "det1reffile"
#define PAR_PIXBIN		 "pixbin"
#define PAR_SKYX		 "skyx"
#define PAR_SKYY		 "skyy"
#define PAR_SKYSIZE		 "skysize"
/* #define PAR_STEMOUT		 "stemout" */
#define PAR_OFFSETFILE		 "offsetfile"
#define PAR_ASPECTHISTOFILE	 "aspecthistofile"
#define PAR_DET1INSTRFILE	 "det1instrfile"
#define PAR_DET2INSTRFILE	 "det2instrfile"
#define PAR_SKYINSTRFILE	 "skyinstrfile"
#define PAR_EXPOMAPFILE		 "expomapfile"
#define PAR_PERCENT		 "percent"
#define PAR_INITSEED     	 "initseed"

/* miscellaneous */
#define BINTAB_ROWS       1000
#define BINTAB_ROWS_SMALL 2
#define STR_LEN           125

#define PIXPOS_MAXGRADE       33
#define PIXPOS_PDF_WIDTH      7
#define PIXPOS_PDF_HEIGHT     7
#define PIXPOS_PDF_DIM        ( PIXPOS_PDF_WIDTH * PIXPOS_PDF_HEIGHT )

#define TIME_SENS         0.0000001
#define DOUBLE_SENS       0.00000000000000001
#define DET1_MAX          1000
#define SKY_MAX           1000
#define MIN_BIN           0
#define MAX_BIN           1000

#define KWNM_CTYPE1       "CTYPE1"
#define KWNM_CUNIT1       "CUNIT1"
#define KWNM_CRPIX1       "CRPIX1"
#define KWNM_CRVAL1       "CRVAL1"
#define KWNM_CDELT1       "CDELT1"
#define KWNM_CTYPE2       "CTYPE2"
#define KWNM_CUNIT2       "CUNIT2"
#define KWNM_CRPIX2       "CRPIX2"
#define KWNM_CRVAL2       "CRVAL2"
#define KWNM_CDELT2       "CDELT2"

#define KWVL_EXTNAME_DET_COORD    "DET_COORD" 
#define KWVL_EXTNAME_INSTRPROBMAP "INSTRPROBMAP" 


typedef struct {
  char   telescop[FLEN_VALUE];
  char	 telescop_comm[FLEN_COMMENT];
  char   instrume[FLEN_VALUE];
  char	 instrume_comm[FLEN_COMMENT];
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
  DTYPE  equinox;
  char	 equinox_comm[FLEN_COMMENT];
  char   timesys[FLEN_VALUE];
  char	 timesys_comm[FLEN_COMMENT];
  JTYPE  mjdrefi;
  char	 mjdrefi_comm[FLEN_COMMENT];
  DTYPE  mjdreff;
  char	 mjdreff_comm[FLEN_COMMENT];
  char   timeunit[FLEN_VALUE];
  char	 timeunit_comm[FLEN_COMMENT];
  DTYPE  tstart;
  char	 tstart_comm[FLEN_COMMENT];
  DTYPE  tstop;
  char	 tstop_comm[FLEN_COMMENT];
  DTYPE  telapse;
  char	 telapse_comm[FLEN_COMMENT];
  char   dateobs[FLEN_VALUE];
  char	 dateobs_comm[FLEN_COMMENT];
  char   dateend[FLEN_VALUE];
  char	 dateend_comm[FLEN_COMMENT];
  SPTYPE timeobs;
  SPTYPE timeend;
  DTYPE  mjdref;
  double livetime;
  double deadc; 

} ObsInfo_t;

typedef struct {
  unsigned TIME;
  unsigned X_DET1;
  unsigned Y_DET1;
} Det1RefCol_t;

typedef struct {
  unsigned TIME;
  unsigned X_OFFSET;
  unsigned Y_OFFSET;
} OffsetCol_t;

typedef struct {
  double time;
  double x_offset;
  double y_offset;
} OffsetInfo_t;


typedef struct {
  int x_bin;
  int y_bin;
  double duration;
  double tstart;
  double tstop;
  double ref_time;
} AspHistoInfo_t;

typedef struct {
  unsigned X_BIN;
  unsigned Y_BIN;
  unsigned DURATION;
  unsigned TSTART;
  unsigned TSTOP;
  unsigned REF_TIME;
} AspHistoCol_t;

typedef struct {
  int x_bin;
  int y_bin;
  double duration;
  double ref_time;
} AspHistoCompInfo_t;


typedef struct {
  unsigned X_BIN;
  unsigned Y_BIN;
  unsigned DURATION;
  unsigned REF_TIME;
} AspHistoCompCol_t;


typedef struct {
  unsigned TIME;
  unsigned DET1X;
  unsigned DET1Y;
} SkyDetCol_t;

typedef struct {
  double time;
  int    det1x;
  int    det1y;
} SkyDetInfo_t;


typedef struct {
  double ref_time;
  double duration;
  int    crval1;
  int    crval2;
} ImageInfo_t;


typedef struct {
  int    det1x;
  int    det1y;
  int    det1size;
  BOOL   isgood;
} Det1InstrMapInfo_t;


typedef struct {
  int    crval1;
  int    crval2;
  int    naxis1;
  int    naxis2;
} Det2InstrMapInfo_t;


typedef struct {
  double ontime;
  double livetime;
  double deadc;
} ExposureInfo_t;

typedef struct {
  unsigned X_DET2A;
  unsigned Y_DET2A;
  unsigned X_DET2B;
  unsigned Y_DET2B;

/*   unsigned Q_FB_MD0; */
/*   unsigned Q_FB_MD1; */
/*   unsigned V_FB_MD0; */
/*   unsigned V_FB_MD1; */
/*   unsigned Q_OB_ML0; */
/*   unsigned Q_OB_ML1; */
/*   unsigned V_OB_ML0; */
/*   unsigned V_OB_ML1; */
/*   unsigned Q_FB_OB; */
/*   unsigned V_FB_OB; */
/*   unsigned L0_ORIG; */
/*   unsigned L1_ORIG; */
/*   unsigned L0_POINT; */
/*   unsigned L1_POINT;   */

/*   unsigned Q_FPMA_DET1; */
/*   unsigned Q_FPMB_DET1; */
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

/*   DTYPE Qfbmd0[4]; */
/*   DTYPE Qfbmd1[4];  */
/*   DTYPE Tfbmd0[3]; */
/*   DTYPE Tfbmd1[3]; */
/*   DTYPE Qobml0[4]; */
/*   DTYPE Qobml1[4]; */
/*   DTYPE Tobml0[3]; */
/*   DTYPE Tobml1[3]; */
/*   DTYPE Qfbob[4]; */
/*   DTYPE Tfbob[3]; */
/*   DTYPE L0ml[3]; */
/*   DTYPE L1ml[3]; */
/*   DTYPE D0ob[3]; */
/*   DTYPE D1ob[3]; */

/*   DTYPE Qfpm0det1[4]; */
/*   DTYPE Qfpm1det1[4]; */
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
  unsigned AZIMUTH;
  unsigned ENERG_LO;
  unsigned THETA;
  unsigned VIGNET;
} VignCol_t;

typedef struct {
  float azimuth;
  float *energ_lo;
  float *theta;
  float *vignet;
} VignRow_t;

typedef struct {
  int       energ_lo_dim;
  int       theta_dim;
  int       vignet_dim;
  int       nrows;
  VignRow_t *row;
} VignInfo_t;


typedef struct {
  unsigned RAWX;
  unsigned RAWY;
  unsigned GRADE;
  unsigned REF_DET1X;
  unsigned REF_DET1Y;
  unsigned PDF;
} PixPosCol_t;

typedef struct {
  int      ref_det1x;
  int      ref_det1y;
  float    *pdf;
} PixPosInfo_t;


typedef struct {
  unsigned rawx;
  unsigned rawy;
} DeltaTimeBPInfo_t;

typedef struct {
  double            tstart;
  double            tstop;
  int               nbp;
  DeltaTimeBPInfo_t *bp;
} DeltaTimeInfo_t;


typedef struct {
  char        det1refgti[PIL_LINESIZE];
  char        combinexform_in[PIL_LINESIZE];
  char        combinexform_out[PIL_LINESIZE];
  char        applyxform_in[PIL_LINESIZE];
  char        applyxform_out[PIL_LINESIZE];
  char        getxform_out[PIL_LINESIZE];
  char        ximage_in[PIL_LINESIZE];
  char        skywcs[PIL_LINESIZE];
  char        dirname[PIL_LINESIZE];
  char        ximage_skyinstrfile[PIL_LINESIZE];
  char        ximage_expomapfile[PIL_LINESIZE];
  char        loc_attfile[PIL_LINESIZE];
  char        skytodetfile[PIL_LINESIZE];
  } TmpFiles_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];       /* Name of input FITS file  */
    char        pixposfile[PIL_LINESIZE];   /* Name of input pixel location FITS file  */
    char        alignfile[PIL_LINESIZE];
    char        mastaspectfile[PIL_LINESIZE];
    char        attfile[PIL_LINESIZE];
    char        teldef[PIL_LINESIZE];
    char        instrprobmapfile[PIL_LINESIZE];
    char        vignfile[PIL_LINESIZE];
    char        indet2instrfile[PIL_LINESIZE];
    char        det1reffile[PIL_LINESIZE];
/*     char        stemout[PIL_LINESIZE]; */
    char        offsetfile[PIL_LINESIZE];
    char        aspecthistofile[PIL_LINESIZE];
    char        det1instrfile[PIL_LINESIZE];
    char        det2instrfile[PIL_LINESIZE];
    char        skyinstrfile[PIL_LINESIZE];
    char        expomapfile[PIL_LINESIZE];
    double      percent;
    BOOL        aberration;
    BOOL        vignflag;
    double      energy;
    int         pixbin;
    double      skyx;
    double      skyy;
    int         skysize;
    BOOL        initseed;

  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  ObsInfo_t  obsinfo;
  TmpFiles_t tmpout;
  BOOL createoffsetfile;
  BOOL createaspecthistofile;
  BOOL createdet1instrfile;
  BOOL getdet2instrfile;
  BOOL createdet2instrfile;
  BOOL createskyinstrfile;
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];

} Global_t;

extern Global_t global;


/* Prototypes */
int nuexpomap();
int nuexpomap_work(void);
int nuexpomap_getpar(void);
void nuexpomap_info(void);
int nuexpomap_checkinput(void);

int GetObsInfo(char *filename, char *extname, ObsInfo_t *obsinfo);

int ComputeOffsets(char *filename, OffsetInfo_t **offinfo, int *offcount);
int ComputeAspHisto(OffsetInfo_t *offinfo, int offcount, struct gti_struct *gti, AspHistoInfo_t **histoinfo, int *histocount);
int ComputeAspHistoComp(AspHistoInfo_t *histoinfo, int histocount, AspHistoCompInfo_t **histocmpinfo, int *histocmpcount);

int WriteOffsetFile(OffsetInfo_t  *offinfo, int offcount, char *infile, char *outfile);
int WriteOffsetExt(OffsetInfo_t  *offinfo, int offcount, FitsFileUnit_t ounit);

int WriteAspectHistoFile(OffsetInfo_t *offinfo, int offcount, char *infile, char *outfile);
int WriteAspectHistoExt(AspHistoInfo_t *histoinfo, int histocount, FitsFileUnit_t ounit);
int WriteAspectHistoCompExt(AspHistoCompInfo_t *histoinfo, int histocount, FitsFileUnit_t ounit);
int ReadAspectHistoCompExt(char *filename, AspHistoCompInfo_t **histocmpinfo, int *histocmpcount);

int ReadInstrProbMapFile(ObsInfo_t *obsinfo, float instrmap[4][DET1_ROWS][DET1_PIXS]);
int ReadInstrProbMapInfo(char *filename, long extno, char *detnam, float instrmap[DET1_ROWS][DET1_PIXS]);

int WriteDET1InstrFile(float instrmap[4][DET1_ROWS][DET1_PIXS], AspHistoCompInfo_t *histocmpinfo, int histocmpcount, char *aspecthistofile, char *evtfile, ObsInfo_t *obsinfo, Det1InstrMapInfo_t *det1info, char *filename);

int GetBinIndex(double binsize, int imin, int imax, double value);

int AddImageKeyword(FitsFileUnit_t inunit, int hducount, ImageInfo_t *imginfo);

int ReadAlignFile(AlignInfo_t *aligninfo);
int ReadAlignInfo(char *filename, AlignInfo_t *aligninfo);

int GetMastInfo(char *filename, double time, MastInfo_t *info);

int WriteCombineXformInFile(char *filename, MastInfo_t *mast, AlignInfo_t *align);
int UpdateCombineXformInFile(char *filename, int det1xx, int det1yy, int det2xx, int det2yy);

int WriteSkyCombineXformInFile(char *filename, int xmin_sky, int ymin_sky, Det2InstrMapInfo_t *det2info, char *getxformfile);

int WriteDet1ToDet2ApplyXformInFile(char *filename, int det1xx, int det1yy);
int ReadDet1ToDet2ApplyXformOutFile(char *filename, int *det2x, int *det2y);

int WriteDet2ToSkyApplyXformInFile(char *filename, Det2InstrMapInfo_t *det2info);
int ReadDet2ToSkyApplyXformOutFile(char *filename, int *xmin_sky, int *ymin_sky);

int WriteXimageInFile(char *skyinstrfile, char *expofile, int imgnum, Det1InstrMapInfo_t *det1info, char *outfile);
int CreateSkyWCS(char *outfile, int xmin_sky, int ymin_sky, int skysize);

int UpdateExposureKeyword(char *filename, ExposureInfo_t *info);

int AddEvtKeywords(FitsFileUnit_t inunit, ObsInfo_t *obsinfo);

int UpdateExposureMapWithDEADC(char *filename, double deadc);

int UpdateInstrMapKeys(char *filename);
int UpdateVignettingKeys(char *filename);
int UpdateDet2InstrMapKeys(char *filename, int extnum, Det2InstrMapInfo_t *info);

int ReadVignFile(char *filename, VignInfo_t *info);

int ApplyVignetting(char *filename, VignInfo_t *info, AlignInfo_t *aligninfo);

void InterpolateValues(double down, double up, double this, double valuedown, double valueup, double *value);

int ReadEvtBPdataAsGTI(char *filename, double mjdref, struct gti_struct gti[4][DET_ROWS][DET_PIXS]);

void SortBadPixExtDatabyTIME(BadPixExtData_t bp[DET_PIXS][DET_ROWS]);

int SetGTIfromBADPIX(BadPixExtData_t inbp[DET_PIXS][DET_ROWS], double mjdref, struct gti_struct gti[DET_ROWS][DET_PIXS]);

int ComputeTimeBad(char *aspecthistofile, int this_xbin, int this_ybin, DeltaTimeInfo_t *deltaT[4], int ndeltaT[4], double *time_bad[4]);

int ReadPixPosFile(ObsInfo_t *obsinfo, PixPosInfo_t pixpos[4][DET_ROWS][DET_PIXS][PIXPOS_MAXGRADE]);
int ReadPixPosInfo(PixPosInfo_t pixpos[DET_ROWS][DET_PIXS][PIXPOS_MAXGRADE], char *filename, long extno, char *detnam);

int CheckAttitude(char *filename);
int CheckDet1Ref(char *filename);

double NormalaziedAt360Angle(double angle);

double ComputeVignValue(VignInfo_t *vigninfo, int vignrow, double dist_arcmin);

int ReadSkyDetFile(char *filename, SkyDetInfo_t **info, int *ninfo);
void FindClosestSkyDetIndex(SkyDetInfo_t *info, int nrows, double time, int *index);

void FindDeltaTimeIndex(DeltaTimeInfo_t *info, int nrows, double time, int *index);
int ComputeDeltaTimeInterval(struct gti_struct bpgti[4][DET_ROWS][DET_PIXS], DeltaTimeInfo_t *deltaT[4], int *ndeltaT);

int ComputeMapBadTot(DeltaTimeInfo_t *deltaT[4], int ndeltaT[4], double *time_bad[4], float probmap[4][DET1_ROWS][DET1_PIXS], PixPosInfo_t pixpos[4][DET_ROWS][DET_PIXS][PIXPOS_MAXGRADE], float map_bad_tot[DET1_ROWS][DET1_PIXS]);


int GetInfoFromDet2InstrMap(char *filename, AspHistoCompInfo_t **histocmpinfo, int *histocmpcount);

int CreateFilteredDet1RefFile(char *infile, char *evtfile, char *outfile);

#endif

