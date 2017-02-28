/*
 *	xrtexpocalc.h: definitions and declarations for xrtexpocalc
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTEXPOCALC_H
#define XRTEXPOCALC_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <unistd.h> /* for getpid */

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

/* miscellaneous */
#define BINTAB_ROWS       1000


typedef struct {
  unsigned CCDFrame;
  unsigned TIME;
  unsigned ENDTIME;
  unsigned XRTMode;
  unsigned Settled;
  unsigned In10Arcm;
  unsigned CCDTEMP;
  
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
  int hkccdframe;
  double hktime;
  double hkendtime;
  double temp;
} HKInfo_t;

typedef struct {
  float **data;
  long dimenx;
  long dimeny;
} ExpoMap_t;



typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char infile[PIL_LINESIZE];     /* Name of FITS file and [ext#] */
    char outfile[PIL_LINESIZE];               /* Name of FITS output file */
  } par;

  struct {
    double mjdrefi;
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
  }key;
  
  float data[SIZE_Y][SIZE_X];
  char taskname[PIL_LINESIZE]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  BOOL hist;
} Global_t;

extern Global_t global;

/* I/O Data structure, photon counting columns from:  
Swift XRT Science FITS Format Design v0.6 June 29,2001) */

/* Prototypes */

int xrtexpocalc(void);
int xrtexpocalc_work(void);
int xrtexpocalc_getpar(void);
void xrtexpocalc_info(void);
int xrtexpocalc_checkinput(void);
int ExpoCalc( FitsFileUnit_t inunit);
int ReadInputImage( FitsFileUnit_t  inunit);
#endif
