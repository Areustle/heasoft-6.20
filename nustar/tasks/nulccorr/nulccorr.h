/*
 *	nulccorr.h: definitions and declarations for nulccorr
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NULCCORR_H
#define NULCCORR_H

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
#include <headas_gti.h>

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

#define PAR_LCFILE		 "lcfile"
#define PAR_HKFILE		 "hkfile"
#define PAR_OUTLCFILE		 "outlcfile"
#define PAR_INFILE		 "infile"
#define PAR_PHAFILE		 "phafile"
#define PAR_INSKYINSTRFILE	 "inskyinstrfile"
#define PAR_INASPECTHISTOFILE	 "inaspecthistofile"
#define PAR_PIXPOSFILE		 "pixposfile"
#define PAR_ALIGNFILE   	 "alignfile"
#define PAR_MASTASPECTFILE   	 "mastaspectfile"
#define PAR_ATTFILE		 "attfile"
#define PAR_TELDEF		 "teldef"
#define PAR_INSTRPROBMAPFILE	 "instrprobmapfile"
#define PAR_PSFFILE		 "psffile"
#define PAR_VIGNFILE		 "vignfile"
#define PAR_ABERRATION		 "aberration"
#define PAR_VIGNFLAG		 "vignflag"
#define PAR_ENERGY		 "energy"
#define PAR_DET1REFFILE		 "det1reffile"
#define PAR_OPTAXISFILE		 "optaxisfile"
#define PAR_PIXBIN		 "pixbin"
#define PAR_CUTMAPS		 "cutmaps"
#define PAR_BOXSIZE		 "boxsize"
#define PAR_PERCENT		 "percent"
#define PAR_SKYINSTRFILE	 "skyinstrfile"
#define PAR_INITSEED     	 "initseed"
#define PAR_EXTENDED		 "extended"
#define PAR_CORRFILE     	 "corrfile"
#define PAR_PSFFLAG		 "psfflag"
#define PAR_EXPOFLAG		 "expoflag"


/* miscellaneous */
#define BINTAB_ROWS       1000

#define PSF_ROWS                 325
#define PSF_PIXS                 325

#define PSF_CENTERX              162
#define PSF_CENTERY              162


#define KWVL_EXTNAME_FRACTION       "FRACTION"

#define KWNM_CRPIX1P       "CRPIX1P"
#define KWNM_CRVAL1P       "CRVAL1P"
#define KWNM_CRPIX2P       "CRPIX2P"
#define KWNM_CRVAL2P       "CRVAL2P"



typedef struct {
  unsigned TIME;
  unsigned LIVETIME;
} HKCol_t;

typedef struct {
  DTYPE    Time;
  DTYPE    LiveTime;
} HKRow_t;


typedef struct {
  unsigned TIME;
  unsigned RATE;
  unsigned ERROR;
  unsigned RATE_ORIG;
  unsigned ERROR_ORIG;
} LcCol_t;

typedef struct {
  char   instrume[FLEN_VALUE];
  double tstart;
  double timedel;
  double timepixr;
  double timezero;
  LTYPE  nulcco;
} LcInfo_t;


typedef struct {
  unsigned X;
  unsigned Y;
  unsigned SHAPE;
  unsigned R;
  unsigned ROTANG;
  unsigned COMPONENT;
} RegionExtCol_t;

typedef struct {
  char     shape[17];
  double   X[2];
  double   Y[2];
  double   R[2];
  double   R1[2];
  double   rotang[2];
  int      component[2];
} RegionExtInfo_t;


typedef struct {
  char   instrume[FLEN_VALUE];
  SPTYPE dateobs;
  SPTYPE timeobs;
  SPTYPE dateend;
  SPTYPE timeend;
} ObsInfo_t;


typedef struct {
  unsigned TIME;
  unsigned X_OA;
  unsigned Y_OA;
} OptAxisCol_t;

typedef struct {
  double time;
  double x_oa;
  double y_oa;
} OptAxisInfo_t;

typedef struct {
  char instrume[FLEN_VALUE];
  DTYPE  tstart;
  char	 tstart_comm[FLEN_COMMENT];
  DTYPE  tstop;
  char	 tstop_comm[FLEN_COMMENT];
} OptAxisKeys_t;


typedef struct {
  int x_bin;
  int y_bin;
  double tstart;
  double tstop;
} AspHistoInfo_t;

typedef struct {
  unsigned X_BIN;
  unsigned Y_BIN;
  unsigned TSTART;
  unsigned TSTOP;
} AspHistoCol_t;


typedef struct {
  int x_bin;
  int y_bin;
  double theta_median;
  double phi_median;
} AspHistoCompBINInfo_t;


typedef struct {
  unsigned X_BIN;
  unsigned Y_BIN;
} AspHistoCompCol_t;


typedef struct {
  double tstart;
  double tstop;
  double fraction;
  double theta;
  double phi;
} PsfCorrInfo_t;

typedef struct {
  unsigned TSTART;
  unsigned TSTOP;
  unsigned FRACTION;
  unsigned THETA;
  unsigned PHI;
} PsfCorrCol_t;


typedef struct {
/*   double ontime; */
/*   double livetime; */
/*   double deadc; */
  double duration;
  int    xwidth;
  int    ywidth;
  int    crpix1p;
  int    crpix2p;
  int    crval1p;
  int    crval2p;
} ExposureInfo_t;


typedef struct {
  int    psfbin_dim;
  double psfbin;
} PsfBinInfo_t;


typedef struct {
  int x_width;
  int y_width;
  int x_offset;
  int y_offset;
} PhaImgInfo_t;


typedef struct {
  int    x_min;
  int    x_max;
  int    y_min;
  int    y_max;
  float  cx;
  float  cy;

} RegBoxInfo_t;

typedef struct {
  int           nbox;
  RegBoxInfo_t *boxinfo;
} RegBox_t;


typedef struct {
  char        outlcfile[PIL_LINESIZE];
  char        skyinstrfile[PIL_LINESIZE];
  char        corrfile[PIL_LINESIZE];
  char        expomap[PIL_LINESIZE];
  char        aspecthisto[PIL_LINESIZE];
  char        offset[PIL_LINESIZE];
  char        combinexform_in[PIL_LINESIZE];
  char        combinexform_out[PIL_LINESIZE];
  char        rotpsffile[PIL_LINESIZE];
  } TmpFiles_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        lcfile[PIL_LINESIZE];       /* Name of input Light Curve FITS file  */
    char        hkfile[PIL_LINESIZE];       /* Name of input Housekeeping FITS file  */
    char        outlcfile[PIL_LINESIZE];      /* Name of output Corrected Light Curve FITS file */
    char        infile[PIL_LINESIZE];       /* Name of input FITS file  */
    char        phafile[PIL_LINESIZE]; 
    char        inskyinstrfile[PIL_LINESIZE];
    char        inaspecthistofile[PIL_LINESIZE];
    char        pixposfile[PIL_LINESIZE];   /* Name of input pixel location FITS file  */
    char        alignfile[PIL_LINESIZE];
    char        mastaspectfile[PIL_LINESIZE];
    char        attfile[PIL_LINESIZE];
    char        teldef[PIL_LINESIZE];
    char        psffile[PIL_LINESIZE];
    char        vignfile[PIL_LINESIZE];
    char        instrprobmapfile[PIL_LINESIZE];
    char        det1reffile[PIL_LINESIZE];
    char        optaxisfile[PIL_LINESIZE];
    char        skyinstrfile[PIL_LINESIZE];
    char        corrfile[PIL_LINESIZE];
    BOOL        aberration;
    BOOL        vignflag;
    double      energy;
    int         pixbin;
    BOOL        cutmaps;
    int         boxsize;
    double      percent;
    BOOL        initseed;
    BOOL        extended;
    BOOL        psfflag;
    BOOL        expoflag;
  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];

  TmpFiles_t tmpout;
  BOOL       createskyinstrfile;
  BOOL       createcorrfile;
  BOOL       runexpomap;

  ObsInfo_t       obsinfo;
  LcInfo_t        lcinfo;
  RegionExtInfo_t reginfo;

} Global_t;

extern Global_t global;


/* Prototypes */

int nulccorr();
int nulccorr_work(void);
int nulccorr_getpar(void);
void nulccorr_info(void);
int nulccorr_checkinput(void);

int ReadHouseKeeping(char *filename, HKRow_t **hkinfo, int *hknrows);

int ComputeCorrection(FitsFileUnit_t evunit, FitsFileUnit_t ounit, HKRow_t *hkinfo, int hknrows, struct gti_struct *gti, PsfCorrInfo_t *psfcorrinfo, int psfcorrcount);
int ComputeCorrFac(HKRow_t *hkinfo, int hknrows, struct gti_struct *gti, double time, double *corrfac);

void FindClosestHkIndex(HKRow_t *info, int nrows, double time, int *index);
void FindClosestOptAxisIndex(OptAxisInfo_t *info, int nrows, double time, int *index);
void FindClosestPsfCorrInfoIndex(PsfCorrInfo_t *info, int nrows, double time, int *index);

int ReadRegionInfo(char *filename, char *extname, RegionExtInfo_t *reginfo);

int ReadOptAxisFile(char *filename, OptAxisInfo_t ** optinfo, int *optcount, OptAxisKeys_t *optkeys);

int ReadAspectHistoExt(char *filename, AspHistoInfo_t **histoinfo, int *histocount);
int ReadAspectHistoCompExt(char *filename, AspHistoCompBINInfo_t **histocmpinfo, int *histocmpcount);

double NormalaziedAt360Angle(double angle);

int WritePsfCombineXformInFile(char *filename, double phi);

int ReadPSFFile(char *filename, int extnum, float psf[PSF_ROWS][PSF_PIXS], double *sumrcts);
int ReadExpoFile(char *filename, int extnum, float **expomap, ExposureInfo_t *info);

int GetPsfBinInfo(char *filename, PsfBinInfo_t *psfbininfo);

int GetInitBinIndex(double binsize, double offset, int imin, int imax, double value);

int ComputePsfFracOff(char *expofile, int expoextnum, char *psffile, double theta, double phi, RegionExtInfo_t *reginfo, double *psf_frac_off);
int ComputePSFCorrection(char *aspecthistofile, char *skyinstrfile, char *psffile, double skyx, double skyy, RegionExtInfo_t *reginfo, PsfCorrInfo_t ** corrinfo, int *corrcount);

int GetObsInfo(char *filename, char *extname, ObsInfo_t *obsinfo);

int WritePsfCorrFile(PsfCorrInfo_t *psfcorrinfo, int psfcorrcount, char *infile, char *outfile);
int WritePsfCorrExt(PsfCorrInfo_t *psfcorrinfo, int nrows, FitsFileUnit_t ounit);

int ComputePsfCorrFac(PsfCorrInfo_t *psfcorrinfo, int psfcorrcount, struct gti_struct *gti, double time, double *corrfac);

int ReadPhaImg(char *filename, float **img, PhaImgInfo_t *imginfo);

int ComputeExpoCorr(char *expofile, int expoextnum, float *phaimg, PhaImgInfo_t *phaimginfo, RegBox_t *box, double *expocorr);
int ComputeEXPOSURECorrection(char *aspecthistofile, char *skyinstrfile, char *phafile, PsfCorrInfo_t **corrinfo, int *corrcount);

int ComputeBoxFromPhaImg(PhaImgInfo_t *phaimginfo, int boxsize, RegBox_t *box);

int CheckInputSkyInstrFile(char *filename, double energy, BOOL vignflag);


#endif
