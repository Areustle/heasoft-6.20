/*
 *	xrtwtcorr.h: definitions and declarations for xrtwtcorr
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTWTCORR_H
#define XRTWTCORR_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h> /* for getpid */

/* xrt local headers */
#include <highfits.h>
#include <xrt_termio.h>
#include <xrtfits.h>
#include <xrtdefs.h>
#include <misc.h>
#include <xrtdasversion.h>
#include <xrtcaldb.h>
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
#define PAR_OUTFILE		   "outfile"
#define PAR_BIASDIFF               "biasdiff"
#define PAR_HDFILE                 "hdfile"
#define PAR_TRFILE		   "trfile"
#define PAR_COLFILE		   "colfile"
#define PAR_NFRAME		   "nframe"
#define PAR_NEVENTS                "nevents"
#define PAR_BIASMODE               "biasmode"
#define PAR_THRFILE                "thrfile"
#define PAR_BIASTH                 "biasth"
#define PAR_NPIXELS                "npixels"

/* miscellaneous */
#define BINTAB_ROWS       1000
#define SAT_VAL           4095        /* Saturation value */
#define FUNC_POINTS        150
#define STEMLEN             14
#define MAX_WINDOW_LEN     300
#define MAX_NFRAME_TIME    100
#define SP_BIASMODE        "SP"
#define M20P_BIASMODE      "M20P"

typedef struct {
  unsigned PHA; 
  unsigned RAWPHA;
  unsigned CCDFrame;
  unsigned OFFSET;
  unsigned TIME_RO;
  unsigned Amp;
  unsigned RAWX;
} Ev2Col_t;

typedef struct {
  unsigned CCDFrame;
  unsigned XRTMode; 
  unsigned BiasPx;
  unsigned HPIXCT;
  unsigned FSTS;
  unsigned FSTSub;
  unsigned PDBIASLVL;
  unsigned WAVE;
} HKCol_t;

typedef struct {
  double fst;
  double fet;
  int    bias[MAX_WINDOW_LEN];
  int    nbias;
  int    first_rawx;
  int    amp;
  int    lastbiasmedian;
  /*  int    cols;
      int    firstcol; */
} TrailerT_t;

typedef struct {
  unsigned ccdframe;
  double   time;
  double   median; 
  int      npix;
  int      wave;
} BiasMedianFormat_t;

typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];  /* Name of FITS file and [ext#] */
    char        outfile[PIL_LINESIZE];             /* Name of FITS output file */
    char        trfile[PIL_LINESIZE];            /* Name of FITS bias calibration file */
    char        hdfile[PIL_LINESIZE];              /* Name of FITS bias calibration file */
    char        colfile[PIL_LINESIZE];
    char        thrfile[PIL_LINESIZE];
    char        biasmode[PIL_LINESIZE];
    int         biasdiff;
    int         nevents;                           /* pixels threshold  */
    int         nframe; /* number of frames for bias median */
    int         biasth;
    int         npixels;
  } par;
/*------------------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  double tstart;
  double tstop;
  SPTYPE dateobs;
  SPTYPE timeobs;
  int    xrtvsub;
  BOOL hist;
  BOOL warning;
  BOOL rawxcorr;
  BOOL usethrfile;
  char strhist[256];
  char date[25];
  char tmpfile[PIL_LINESIZE];
  BiasMedianFormat_t *bias_median;
  int    totbias;
  unsigned trnrows;
  TrailerT_t *trailer;
  WTOffsetT_t *wtcol;
  int colrows;
} Global_t;

extern Global_t global;


typedef struct {
  unsigned FrstFST;
  unsigned FrstFSTS;
  unsigned LastFST;
  unsigned LastFSTS;
  unsigned BROW1;
  unsigned BROW1CO;
  unsigned BROW1LEN;
  unsigned BROW1AMP;
  unsigned BROW2;
  unsigned BROW2CO;
  unsigned BROW2LEN;
  unsigned BROW2AMP;
  unsigned WM1STCOL;
  unsigned WMCOLNUM;
} TrailerCol_t;




/* I/O Data structure, photon counting columns from:  
Swift XRT Science FITS Format Design v0.6 June 29,2001) */

/* Prototypes */

int xrtwtcorr();
int xrtwtcorr_work(void);
int xrtwtcorr_getpar(void);
void xrtwtcorr_info(void);
int xrtwtcorr_checkinput(void);
int WTPhaReCorrect(FitsFileUnit_t evunit, FitsFileUnit_t ounit);
int ComputeBiasMedian(void);
int ReadTrFile(void);
int CalcMedian(int * bias, int length);

#endif
