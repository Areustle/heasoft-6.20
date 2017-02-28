/*************************************************************************
 *	xrthotpix.h: definitions and declarations with global relevance
 *
 *	AUTHOR:
 *
 *         Italian Swift Archive Center (FRASCATI)
 *     
 *************************************************************************/
#ifndef XRTHOTPIX_H
#define XRTHOTPIX_H

/******************************
 *        header files        *
 ******************************/
#include <math.h>
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>

#include <unistd.h> /* for getpid */ 
#include <ctype.h>
/* xrt local headers */
#include <xrt_termio.h>
#include <highfits.h>
#include <xrtdefs.h>
#include <xrtbpfunc.h>
#include <misc.h>
#include <xrtfits.h>
#include <xrtdasversion.h>
#include <xrtcaldb.h>
#include <xrt_gammq.h>
/* headas headers */
#include <pil.h>


/********************************
 *         definitions          *
 ********************************/


/* input parameter names */
#define PAR_INFILE         "infile"    
#define PAR_OUTBPFILE      "outbpfile"
#define PAR_OUTFILE        "outfile"
#define PAR_OVERSTATUS     "overstatus"
#define PAR_IMPFAC         "impfac"
#define PAR_LOGPOS         "logpos"
#define PAR_CELLSIZE       "cellsize"
#define PAR_CLEANFLICK     "cleanflick"
#define PAR_BTHRESH        "bthresh"
#define PAR_ITERATE        "iterate"
#define PAR_PHAMAX         "phamax"
#define PAR_PHAMIN         "phamin"
#define PAR_USEGOODEVT     "usegoodevt"
#define PAR_GRADEITERATE   "gradeiterate"
#define PAR_FLAGNEIGH      "hotneigh"

/* Block of rows */

#define BINTAB_ROWS  1000
/********************************/
/*           typedefs           */
/********************************/
typedef struct {
  struct {
    char infile[MAX_FILES*FLEN_FILENAME]; 
    char outbpfile[PIL_LINESIZE];
    char outfile[PIL_LINESIZE];
    double impfac;
    double logpos;
    int cellsize;
    int bthresh;
    int phamax;
    int phamin;
    BOOL overstatus;
    BOOL cleanflick;
    BOOL iterate;
    BOOL usegoodevt;
    BOOL gradeiterate;
    BOOL flagneigh;
  } par;
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  BOOL hist;
  char strhist[256];
  char date[25];
  SPTYPE obsid;
  char tmpfile[FLEN_FILENAME];
  int amp;
  BOOL warning;
} Global_t;

extern Global_t global;

typedef struct {
  unsigned TIME;
  unsigned TDHXI;
  unsigned CCDFrame;
  unsigned X;
  unsigned Y;
  unsigned RAWX;
  unsigned RAWY;
  unsigned DETX;
  unsigned DETY;
  unsigned PHAS;
  unsigned PHA;
  unsigned Amp;
  unsigned STATUS; 
  unsigned GRADE;
} EvCol_t;

                  /********************************/
		  /*          prototypes          */
		  /********************************/

int xrthotpix(void);
int xrthotpix_work(void);
int xrthotpix_getpar(void);
void xrthotpix_info(void);
int xrthotpix_checkinput( void ) ;
int FillCountImage(FitsFileUnit_t evunit, int count[CCD_PIXS][CCD_ROWS], int gradecount[CCD_PIXS][CCD_ROWS]);
int CleanHotPixels(int count[CCD_PIXS][CCD_ROWS], int xmin, int xmax, int ymin, int ymax, int *totcounts, int *totpixs);
int FlagBPPixels(FitsFileUnit_t evunit, FitsFileUnit_t ounit, int count[CCD_PIXS][CCD_ROWS]);
int CreateEVFile(FitsFileUnit_t evunit, int count[CCD_PIXS][CCD_ROWS]);
int CleanFlickering(int count[CCD_PIXS][CCD_ROWS], int xmin, int xmax, int ymin, int ymax, int *totcounts, int *totpixs);
int ReadBPExt(FitsFileUnit_t evunit,int hducount, BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS]);
#endif


