/*
 *	nuflagevt.h: definitions and declarations for nuflagevt
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *
 *	CHANGE HISTORY:
 *        0.1.0 - NS 12/07/12 - First version
 *  
 *     
 */
#ifndef NUFLAGEVT_H
#define NUFLAGEVT_H

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
  Macros to put in nu_highfits.h file
********************************/

#define VECVECACCESS_PT(type, a, b, c)	\
                        ((type *)(a).cols[c]) + (b*(a).Multiplicity[c])

#define VECVEC_ARRAY_READ(type, var, dim, a, b, c) \
                          memcpy(var,VECVECACCESS_PT(type,a,b,c), sizeof(type)*dim)
#define VECVEC_ARRAY_WRITE(type, var, dim, a, b, c) \
                          memcpy(VECVECACCESS_PT(type,a,b,c), var, sizeof(type)*dim)

#define JVECVEC_ARRAY_READ(var, dim, a, b, c) VECVEC_ARRAY_READ(JTYPE, var, dim, a, b, c)
#define JVECVEC_ARRAY_WRITE(var, dim, a, b, c) VECVEC_ARRAY_WRITE(JTYPE, var, dim, a, b, c)


/*******************************
*      defines / typedefs      *
********************************/
/* input parameter names */

#define PAR_INFILE		 "infile"
#define PAR_OUTFILE		 "outfile"
#define PAR_DEPTHCUTFILE	 "depthcutfile"
#define PAR_EVTCUTFILE  	 "evtcutfile"

/* miscellaneous */
#define BINTAB_ROWS       1000
#define E_DEPTH_DIM       4096
#define E_DEPTH_NULL      -2147483648


typedef struct {
  unsigned TIME;
  unsigned RAWX;
  unsigned RAWY;
  unsigned DET_ID;
  unsigned GRADE;
  unsigned PI;
  unsigned SURRPI;
  unsigned PRIOR;
  unsigned RESET;
  unsigned PREPHAS;
  unsigned STATUS;
} EvtCol_t;

typedef struct {
  char   instrume[FLEN_VALUE];
  SPTYPE dateobs;
  SPTYPE timeobs;
  SPTYPE dateend;
  SPTYPE timeend;
} EVTInfo_t;

typedef struct {
  unsigned RAWX;
  unsigned RAWY;
  unsigned E1;
  unsigned E2;
  unsigned E3; 
} DepthCol_t;

typedef struct {
  JTYPE  e1[E_DEPTH_DIM];
  JTYPE  e2[E_DEPTH_DIM];
  JTYPE  e3[E_DEPTH_DIM];
} DepthInfo_t;


typedef struct {
  unsigned BASELINE1;
  unsigned PI_BASELINE;
  unsigned BASELINE2;
  unsigned PRIOR1;
  unsigned PRIOR2;
  unsigned PRIOR3;
  unsigned PI_PRIOR;
  unsigned RESET1;
  unsigned RESET2;
  unsigned RESET3;
  unsigned PI1_RESET;
  unsigned PI2_RESET; 
} EvtCutCol_t;

typedef struct {
  JTYPE baseline1;
  JTYPE pi_baseline;
  JTYPE baseline2;
  DTYPE prior1;
  DTYPE prior2;
  DTYPE prior3;
  JTYPE pi_prior;
  DTYPE reset1;
  DTYPE reset2;
  DTYPE reset3;
  JTYPE pi1_reset;
  JTYPE pi2_reset; 
} EvtCutInfo_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];       /* Name of input FITS file  */
    char        outfile[PIL_LINESIZE];      /* Name of output FITS file */
    char        depthcutfile[PIL_LINESIZE];
    char        evtcutfile[PIL_LINESIZE];
  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];
  char tmpfile[PIL_LINESIZE];
  EVTInfo_t evt;


} Global_t;

extern Global_t global;


/* Prototypes */
int nuflagevt();
int nuflagevt_work(void);
int nuflagevt_getpar(void);
void nuflagevt_info(void);
int nuflagevt_checkinput(void);

int ComputeDepthCut(FitsFileUnit_t evunit, FitsFileUnit_t ounit, DepthInfo_t depth[8][DET_PIXS][DET_ROWS], EvtCutInfo_t evtcut[4]);

int ReadDepthCutFile(DepthInfo_t depth[8][DET_PIXS][DET_ROWS], const EVTInfo_t *evt);
int ReadDepthInfo(DepthInfo_t depth[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam);

int ReadEvtCutFile(EvtCutInfo_t evtcut[4], const EVTInfo_t *evt);
int ReadEvtCutInfo(EvtCutInfo_t *evtcut, char *filename, long extno, char *detnam);


#endif
