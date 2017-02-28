/*
 *	xrtinstrmap.h: definitions and declarations for xrtinstrmap
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTINSTRMAP_H
#define XRTINSTRMAP_H

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
#include <xrtbpfunc.h>

/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>
#include <hdcal.h>

#ifndef errno
extern int errno;
#endif

/*******************************
*      defines / typedefs      *
********************************/
/* input parameter names */


#define PAR_INFILE		   "infile"
#define PAR_OUTFILE		   "outfile"
#define PAR_HDFILE		   "hdfile"
#define PAR_FOVFILE		   "fovfile"
#define PAR_NFRAME		   "nframe"
#define PAR_USENEIGH		   "useneigh"
#define PAR_CHECKATTITUDE	   "checkattitude"

/* miscellaneous */
#define BINTAB_ROWS       1000
#define DIFF_TIME         100.

#define WTFRAME_DIFF        0.04
#define WTFRAME_DIST        1.20

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
  double start;
  double stop;
} GTIInfo_t;



typedef struct {
  int hkccdframe;
  double hktime;
  double hkendtime;
  double temp;
  double RA;
  double Dec;
} HKInfo_t;

typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char infile[PIL_LINESIZE];     /* Name of FITS file and [ext#] */
    char hdfile[PIL_LINESIZE];
    char fovfile[PIL_LINESIZE];
    char outfile[PIL_LINESIZE];               /* Name of FITS output file */
    int nframe;
    BOOL checkattitude;
  } par;

  struct {
    int    mjdrefi;
    double mjdreff;
    char   obsid[15];
    int    target;
    int    segnum;
    char   obsmode[15];
    char   datamode[15];
    double raobj;
    double decobj;
    double rapnt;
    double decpnt;
    double tstart;
    double tstop;
    double timedel;
    double maxtemp;
    double ontime;
    float  xrtnfram;
    char   dateobs[20];
    char   dateend[20];
    double  pa_pnt;
    double tcrvl_x;
    double tcrvl_y;
    BOOL   xrthnflg;
    char   attflag[FLEN_KEYWORD];
    
  }key;
  
  int    xrtmode;
  char taskname[PIL_LINESIZE]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  BOOL hist;
  BOOL warning;
  GTIInfo_t *gti;
  int gticount;
  HKInfo_t *hkinfo;
  double tmp_tstart;
  double tmp_tstop;
  int bpcount;
  int hkcount;
  int *framexorb;
  int nsetframe;
  CalSrcInfo_t calfov;
  int       wm1stcol;
  int       wmcolnum;
  int       whalfht;
  int       whalfwd;
} Global_t;

extern Global_t global;

/* I/O Data structure, photon counting columns from:  
Swift XRT Science FITS Format Design v0.6 June 29,2001) */

/* Prototypes */

int xrtinstrmap(void);
int xrtinstrmap_work(void);
int xrtinstrmap_getpar(void);
void xrtinstrmap_info(void);
int xrtinstrmap_checkinput(void);
int ReadHK(void);
int ReadEventFile(BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS], int *amp);
int CreateInstrMap(BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS],  int amp);
int AddSwiftXRTKeyword(FitsFileUnit_t inunit, int bitpix, int amp, int jj);
int GetCalFOVInfo(void);
double CalcMedian(double * array, int length);
#endif
