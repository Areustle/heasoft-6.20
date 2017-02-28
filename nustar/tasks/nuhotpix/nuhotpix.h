/*
 *	nuhotpix.h: definitions and declarations for nuhotpix
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NUHOTPIX_H
#define NUHOTPIX_H

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
#include "nu_gammq.h"
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
#define PAR_OUTHPFILE		 "outhpfile"
#define PAR_BINSIZE		 "binsize"
#define PAR_IMPFAC               "impfac"
#define PAR_LOGPOS               "logpos"
#define PAR_CELLSIZE             "cellsize"
#define PAR_CLEANFLICK           "cleanflick"
#define PAR_BTHRESH              "bthresh"
#define PAR_ITERATE              "iterate"
/* #define PAR_GRADEITERATE         "gradeiterate" */

/* miscellaneous */
#define BINTAB_ROWS       1000
#define TIME_SENS         0.0000001
#define HOTPIX            -1
#define FLICKPIX          -2


typedef struct {
  unsigned TIME;
  unsigned RAWX;
  unsigned RAWY;
/*   unsigned GRADE; */
  unsigned DET_ID;
  unsigned STATUS;
  unsigned HOTPOS;
} EvtCol_t;

typedef struct {
  char instrume[FLEN_VALUE];
  double tstart;
  double tstop;
} EVTInfo_t;


typedef struct {
  int xmax;
  int xmin;
  int ymax;
  int ymin;
} MapBorders_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];       /* Name of input FITS file  */
    char        outfile[PIL_LINESIZE];      /* Name of output FITS file */
    char        outhpfile[PIL_LINESIZE];    /* Name of output Hot Pixel FITS file */
    double      binsize;
    double      impfac;
    double      logpos;
    int         cellsize;
    int         bthresh;
    BOOL        cleanflick;
    BOOL        iterate;
/*     BOOL        gradeiterate; */

  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  BOOL outhpfile;
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];
  char tmpfile[PIL_LINESIZE];
  EVTInfo_t evt;


} Global_t;

extern Global_t global;


/* Prototypes */
int nuhotpix();
int nuhotpix_work(void);
int nuhotpix_getpar(void);
void nuhotpix_info(void);
int nuhotpix_checkinput(void);

int SearchHotPix(FitsFileUnit_t evunit, BadPixExtData_t hp[4][DET_PIXS][DET_ROWS]);

int NewSearch(int count[4][DET_PIXS][DET_ROWS], int gradecount[4][DET_PIXS][DET_ROWS]);

int IdentifyHotPix(int count[4][DET_PIXS][DET_ROWS], MapBorders_t map[4]);
int IdentifyFlickering(int count[4][DET_PIXS][DET_ROWS], MapBorders_t map[4], char *evttype);

int UpdateBPdataWithCountMapsdata(int count[4][DET_PIXS][DET_ROWS], double startime, double stoptime, BadPixExtData_t hp[4][DET_PIXS][DET_ROWS]);

int HotPixFlag(FitsFileUnit_t evunit, FitsFileUnit_t ounit, BadPixExtData_t hp[4][DET_PIXS][DET_ROWS]);

int ReadEvtBPdata(BadPixExtData_t bp[4][DET_PIXS][DET_ROWS], int *bpExt0, int *bpExt1, int *bpExt2, int *bpExt3);

void ComputeMapLimits(int count[4][DET_PIXS][DET_ROWS], MapBorders_t map[4]);

#endif
