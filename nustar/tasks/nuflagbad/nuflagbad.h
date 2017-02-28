/*
 *	nuflagbad.h: definitions and declarations for nuflagbad
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NUFLAGBAD_H
#define NUFLAGBAD_H

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
*      defines / typedefs      *
********************************/
/* input parameter names */

#define PAR_INFILE		 "infile"
#define PAR_OUTFILE		 "outfile"
#define PAR_DISPIXFILE		 "dispixfile"
#define PAR_BPFILE		 "bpfile"
#define PAR_USERBPFILE		 "userbpfile"
#define PAR_OUTBPFILE		 "outbpfile"


/* miscellaneous */
#define BINTAB_ROWS       1000
#define TIME_SENS         0.0000001


typedef struct {
  unsigned TIME;
  unsigned RAWX;
  unsigned RAWY;
  unsigned DET_ID;
  unsigned STATUS;
  unsigned BADPOS;
} EvtCol_t;

typedef struct {
  char instrume[FLEN_VALUE];
  SPTYPE dateobs;
  SPTYPE timeobs;
  double tstart;
  double tstop;
} EVTInfo_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];       /* Name of input FITS file  */
    char        outfile[PIL_LINESIZE];      /* Name of output FITS file */
    char        dispixfile[PIL_LINESIZE];      /* Name of on-board disabled pixel FITS file */
    char        bpfile[PIL_LINESIZE];      /* Name of on-ground bad pixel FITS file */
    char        userbpfile[PIL_LINESIZE];      /* Name of user bad pixel FITS file */
    char        outbpfile[PIL_LINESIZE];      /* Name of output Bad Pixel FITS file */
  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  BOOL hist;
  BOOL warning;
  BOOL userbpfile;
  BOOL bpfile;
  BOOL dispixfile;
  BOOL outbpfile;
  char strhist[256];
  char date[25];
  char tmpfile[PIL_LINESIZE];
  EVTInfo_t evt;


} Global_t;

extern Global_t global;


/* Prototypes */
int nuflagbad();
int nuflagbad_work(void);
int nuflagbad_getpar(void);
void nuflagbad_info(void);
int nuflagbad_checkinput(void);

int ReadEvtBPdata(BadPixExtData_t bp[4][DET_PIXS][DET_ROWS], int *bpExt0, int *bpExt1, int *bpExt2, int *bpExt3);
int ReadBPdata(BadPixExtData_t bp[4][DET_PIXS][DET_ROWS]);
int ReadBPInfo(BadPixExtData_t bp[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam, short int inbadflag);
int ReadDisInfo(BadPixExtData_t bp[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam);

int BadPixFlag(FitsFileUnit_t inunit, FitsFileUnit_t ounit, BadPixExtData_t bp[4][DET_PIXS][DET_ROWS]);

#endif
