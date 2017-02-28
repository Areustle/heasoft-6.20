/*************************************************************************
 *	xrtflagpix.h: definitions and declarations with global relevance
 *
 *	AUTHOR:
 *
 *         Italian Swift Archive Center (FRASCATI)
 *     
 *************************************************************************/

#ifndef XRTFLAGPIX_H
#define XRTFLAGPIX_H

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

/* headas headers */
#include <pil.h>


/********************************
 *         definitions          *
 ********************************/


#define BUFSIZE 81

#define BMFITSEXT    "bm.img"
#define STEMLEN        14


/* input parameter names */
#define PAR_INFILE         "infile"    
#define PAR_BPFILE         "bpfile"
#define PAR_HDFILE         "hdfile"
#define PAR_SRCFILE        "srcfile"
#define PAR_BPTABLE        "bptable"
#define PAR_OUTBPFILE      "outbpfile"
#define PAR_OUTFILE        "outfile"
#define PAR_USERBPFILE     "userbpfile"
#define PAR_OVERSTATUS     "overstatus"
#define PAR_THRFILE        "thrfile"
#define PAR_EVENT          "phas1thr"
#define PAR_MAXTEMP        "maxtemp"


/* Block of rows */
#define BINTAB_ROWS  1000


typedef struct {
  int hkccdframe;
  double hktime;
  int baseline;
  double temp;
} BLine_t;

/********************************/
/*           typedefs           */
/********************************/
typedef struct {

  struct {
    char   infile[PIL_LINESIZE]; 
    char   bpfile[PIL_LINESIZE];
    char   hdfile[PIL_LINESIZE];
    char   userbpfile[PIL_LINESIZE];
    char   bptable[PIL_LINESIZE];
    char   srcfile[PIL_LINESIZE];
    char   outbpfile[PIL_LINESIZE];
    char   outfile[PIL_LINESIZE];
    char   thrfile[PIL_LINESIZE];
    int    event;  
    double maxtemp;
    BOOL overstatus;
  } par;
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  BOOL hist;
  BOOL nosrcflag;
  BOOL nofovflag;
  BOOL warning;
  BOOL usethrfile;
  char strhist[256];
  char date[25];
  SPTYPE dateobs;
  SPTYPE timeobs;
  int xrtvsub;
  double tstart;
  char  datamode[15];
  char obsmode[15];
  char tmpfile[FLEN_FILENAME];
  int amp;
  CalSrcInfo_t calsrc[4]; 
  CalSrcInfo_t calfov;
  double percent;
  BLine_t *bline;
  int count;
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
} EvCol_t;


typedef struct {
  unsigned RAWPHA;
  unsigned STATUS; 
} PDCol_t;
  
typedef struct {
  int xmin;
  int xmax;
  int ymin;
  int ymax;
  short **Map;
} Image_t;

typedef struct {
  unsigned CCDFrame;
  unsigned TIME;
  unsigned BaseLine;
  unsigned XRTMode;
  unsigned Settled;
  unsigned In10Arcm;
  unsigned InSafeM;
  unsigned CCDTEMP;
} HKCol_t;



                  /********************************/
		  /*          prototypes          */
		  /********************************/

int xrtflagpix(void);
int xrtflagpix_work(void);
int GetBiasMap(char *, BadPixel_t *badpix[CCD_PIXS][CCD_ROWS]);
int xrtflagpix_getpar(void);
void xrtflagpix_info(int ret);
int EventBintable(FitsFileUnit_t iunit,FitsFileUnit_t ounit, BadPixel_t *badpix_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *badpix_amp2[CCD_PIXS][CCD_ROWS] );
int WindowedBintable(FitsFileUnit_t iunit,FitsFileUnit_t ounit, BadPixel_t *badpix_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *badpix_amp2[CCD_PIXS][CCD_ROWS]);
int CreateEVFile(FitsFileUnit_t inevent, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS]);
int xrtflagpix_checkinput ( void ) ;
int WriteStatus(FitsFileUnit_t ounit);
int ReadHK(void);
int AddNeighBadPix(FitsFileUnit_t evunit, BadPixel_t * bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2 [CCD_PIXS][CCD_ROWS]);
int GetCalSourcesInfo(void);
int GetCalFovInfo(void);
int ReadBPExt(FitsFileUnit_t evunit, int hducount,BadPixel_t * bp_table_amp[CCD_PIXS][CCD_ROWS]);
#endif


