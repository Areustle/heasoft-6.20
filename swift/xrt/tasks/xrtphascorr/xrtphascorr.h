/*
 *	xrtphascorr.h: definitions and declarations for xrtphascorr
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTPHASCORR_H
#define XRTPHASCORR_H

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
#define PAR_PHASCONFFILE           "phasconffile"


/* miscellaneous */
#define BINTAB_ROWS       1000


typedef struct {
  unsigned PHAS; 
  unsigned PHASO;
  unsigned TLMPHAS;
} Ev2Col_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];  /* Name of FITS file and [ext#] */
    char        outfile[PIL_LINESIZE]; /* Name of FITS output file */
    char        phasconffile[PIL_LINESIZE]; /* Name of FITS phasconf file */
  } par;
/*------------------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  BOOL hist;
  BOOL warning;
  BOOL correctphas;
  char strhist[256];
  char date[25];
  char tmpfile[PIL_LINESIZE];
  double tstart;
  LTYPE  xphasco;
} Global_t;

extern Global_t global;

/* Prototypes */

int xrtphascorr();
int xrtphascorr_work(void);
int xrtphascorr_getpar(void);
void xrtphascorr_info(void);
int xrtphascorr_checkinput(void);
int PHASCorrect(FitsFileUnit_t evunit, FitsFileUnit_t ounit, int phasconf[9]);
int VerifyPhasConf(int phasconf[9]);

#endif
