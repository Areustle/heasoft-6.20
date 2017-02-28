/*
 *	xrtlccorr.h: definitions and declarations for xrtlccorr
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTLCCORR_H
#define XRTLCCORR_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <unistd.h> /* for getpid */
#include <math.h>
/* xrt local headers */
#include <highfits.h>
#include <xrt_termio.h>
#include <xrtfits.h>
#include <xrtdefs.h>
#include <misc.h>
#include <xrtdasversion.h>
#include <xrtcaldb.h>
#include <xrttimefunc.h>
#include <xrtbpfunc.h>

/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>
#include <hdcal.h>
#include <headas_gti.h>
#include <coordfits.h>
#include <teldef.h>
#include <ephemeris.h>


#ifndef errno
extern int errno;
#endif

/*******************************
*      defines / typedefs      *
********************************/
/* input parameter names */


#define PAR_CREATEINSTRMAP         "createinstrmap"
#define PAR_INFILE		   "infile"
#define PAR_HDFILE		   "hdfile"
#define PAR_FOVFILE		   "fovfile"
#define PAR_VIGFILE		   "vigfile"
#define PAR_WTNFRAME		   "wtnframe"
#define PAR_PCNFRAME		   "pcnframe"
#define PAR_LCFILE		   "lcfile"
#define PAR_INSTRFILE		   "instrfile"
#define PAR_OUTINSTRFILE           "outinstrfile"
#define PAR_PSFFILE		   "psffile"
#define PAR_REGFILE		   "regionfile"
#define PAR_PSFFLAG		   "psfflag"
#define PAR_OUTFILE		   "outfile"
#define PAR_CORRFILE		   "corrfile"
#define PAR_SRCX		   "srcx"
#define PAR_SRCY		   "srcy"
#define PAR_ENERGY		   "energy"
#define PAR_TELDEF                 "teldef"
#define PAR_ATTFILE		   "attfile"
#define PAR_ABERRATION             "aberration"
#define PAR_ATTINTERPOL            "attinterpol"

#define CLNM_SHAPE                 "SHAPE"
#define CLNM_COMPONENT             "COMPONENT"
#define CLNM_ROTANG                "ROTANG"

#define CDETX              299.0 
#define CDETY              299.0

#define WTFRAME_DIFF        0.04

#define RAWMAP_DELTA       4

/* miscellaneous */
#define BINTAB_ROWS       1000
#define DIFF_TIME         100.
#define NCOEFF            4
#define KWVL_EXTNAME_PSF_COEF "PSF_COEF"

typedef struct {
  unsigned X;
  unsigned Y;
  unsigned SHAPE;
  unsigned R;
  unsigned ROTANG;
  unsigned COMPONENT;
} LCCol_t;

typedef struct {
  unsigned TIME;
  unsigned RATE;
  unsigned ERROR;
} RTCol_t;

typedef struct {
  unsigned COEF0;
  unsigned COEF1;
  unsigned COEF2;
  unsigned COEF3;
} PSFCol_t;

typedef struct {
  unsigned CCDFrame;
  unsigned TIME;
  unsigned ENDTIME;
  unsigned XRTMode;
  unsigned Settled;
  unsigned In10Arcm;
  unsigned CCDTEMP;
  unsigned RA;
  unsigned Dec;
} HKCol_t;

typedef struct {
  unsigned RAWX;
  unsigned RAWY;
  unsigned YEXTENT;
  unsigned Amp;
  unsigned TYPE;
  unsigned BADFLAG;
} BPCol_t;

typedef struct {
  unsigned START;
  unsigned STOP;
} GTICol_t;

typedef struct {
  unsigned X;
  unsigned Y;
} EvtCol_t;

typedef struct {
  unsigned PAR0;
  unsigned PAR1;
  unsigned PAR2;
  
} VigCol_t;

typedef struct {
  int hkccdframe;
  double hktime;
  double hkendtime;
  double temp;
  double RA;
  double Dec;
} HKInfo_t;

typedef struct {
  double start;
  double stop;
} GTIInfo_t;

typedef struct {
  char     shape[17];
  double   X[2];
  double   Y[2];
  double   R[2];
  double   R1[2];
  double   rotang[2];
  int      component[2];
} Reg_Info;

typedef struct {
  double par0;
  double par1;
  double par2;
} Vig_Info;

typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char   infile[PIL_LINESIZE];
    char   hdfile[PIL_LINESIZE];
    char   fovfile[PIL_LINESIZE];
    char   vigfile[PIL_LINESIZE];
    char   instrfile[PIL_LINESIZE];
    char   outinstrfile[PIL_LINESIZE];
    int    wtnframe;
    int    pcnframe;
    char   lcfile[PIL_LINESIZE];     /* Name of FITS file and [ext#] */
    char   regfile[PIL_LINESIZE]; 
    char   outfile[PIL_LINESIZE];
    char   psffile[PIL_LINESIZE];
    char   corrfile[PIL_LINESIZE];
    char   teldef[PIL_LINESIZE];
    char   attfile[PIL_LINESIZE];
    double srcx;
    double srcy;
    double energy;
    BOOL   psfflag;
    BOOL   aberration; 
    BOOL   attinterpol;
    BOOL   createinstrmap;
  } par;

  struct {
    int    mjdrefi;
    double mjdreff;
    char   obsid[15];
    int    target;
    int    segnum;
    char   obsmode[15];
    char   datamode[15];
    int    xrtmode;
    double raobj;
    double decobj;
    double rapnt;
    double decpnt;
    double tcrvl_x;
    double tcrvl_y;
    double tstart;
    double tstop;
    double timedel;
    double maxtemp;
    double ontime;
    float  xrtnfram;
    SPTYPE dateobs;
    SPTYPE timeobs;
    char   dateend[20];
    double  pa_pnt;
    BOOL   xrthnflg;

    int       wm1stcol;
    int       wmcolnum;
    int       whalfht;
    int       whalfwd;
    
  }evt_key;

  struct {
    int    mjdrefi;
    double mjdreff;
    char   obsid[15];
    int    target;
    int    segnum;
    char   obsmode[15];
    char   datamode[15];
    double tstart;
    double tstop;
    BOOL   vignapp;
    SPTYPE dateobs;
    SPTYPE timeobs;
    char   dateend[20];
    double energy;
    double  pa_pnt;
    double ranom;
    double decnom;
  }instr_key;
  
  double psf_c0;
  double psf_c1;
  double psf_c2;
  double psf_c3;
  char taskname[PIL_LINESIZE]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  BOOL   warning;
  BOOL   hist;
  double *frac_expo;
  double *time_expo;
  int    frac_index;
  char   date[25];
  char	 attflag[FLEN_KEYWORD];
  char   strhist[256];
  Reg_Info region;

  char   instrmap[PIL_LINESIZE];

  GTIInfo_t *gti;
  HKInfo_t *hkinfo;
  CalSrcInfo_t calfov;
  Vig_Info viginfo;
  int bpcount;
  int gticount;
  int hkcount;
  int *framexorb;
  int nsetframe;

  double tmp_tstart;
  double tmp_tstop;
  double tmp_crpix1;
  double tmp_crpix2;
  int    tmp_crval1;
  int    tmp_crval2;

} Global_t;

extern Global_t global;

/* I/O Data structure, photon counting columns from:  
Swift XRT Science FITS Format Design v0.6 June 29,2001) */

/* Prototypes */

int xrtlccorr(void);
int xrtlccorr_work(void);
int xrtlccorr_getpar(void);
void xrtlccorr_info(void);
int xrtlccorr_checkinput(void);
int ReadLcFile(FitsFileUnit_t lcunit);
int ReadRegionFile();
int CreateFracTable(void);
int GetPSFFileCoeff(double off);
int PsfCorr(int rx, double *wei, double *pcorr);
int CorrectLCCurve(FitsFileUnit_t lcunit);
int AddInstrSwiftXRTKeyword(FitsFileUnit_t inunit);
int ComputeRawCoord(double sky_x, double sky_y, double time, double mjdref, TELDEF* teldef, ATTFILE* attfile, double* raw_x, double* raw_y);
int ReadEventFile(BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS], int *amp);
int ReadHK(void);
int GetCalFOVInfo(void);
int GetVignettingInfo(void);
int CreateInstrMap(BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS], int amp);
int AddEvtSwiftXRTKeyword(FitsFileUnit_t inunit, int bitpix, int amp, int jj);

int StripExtensionWithStem(char *filename, char *stem);

#endif
