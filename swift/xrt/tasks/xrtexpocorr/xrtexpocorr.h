/*
 *	xrtexpocorr.h: definitions and declarations for xrtexpocorrc
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTEXPOCORR_H
#define XRTEXPOCORR_H

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
#define PAR_VIGFILE		   "vigfile"
#define PAR_ENERGY		   "energy"

/* miscellaneous */
#define BINTAB_ROWS       1000


typedef struct {
  unsigned PAR0;
  unsigned PAR1;
  unsigned PAR2;
  
} VigCol_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char infile[PIL_LINESIZE];     /* Name of FITS file and [ext#] */
    char outfile[PIL_LINESIZE];    /* Name of FITS output file */
    char vigfile[PIL_LINESIZE];
    double energy;
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
  
  float data[CCD_ROWS][CCD_PIXS];
  float vcorr[CCD_ROWS][CCD_PIXS];
  char taskname[PIL_LINESIZE]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  BOOL hist;
} Global_t;

extern Global_t global;

/* I/O Data structure, photon counting columns from:  
Swift XRT Science FITS Format Design v0.6 June 29,2001) */

/* Prototypes */

int xrtexpocorr(void);
int xrtexpocorr_work(void);
int xrtexpocorr_getpar(void);
void xrtexpocorr_info(void);
int xrtexpocorr_checkinput(void);
int CorrImage(FitsFileUnit_t inunit, FitsFileUnit_t outunit, int ext);
int VignettingCorr(void);
#endif
