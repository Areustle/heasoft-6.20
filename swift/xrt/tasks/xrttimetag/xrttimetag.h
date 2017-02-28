/*
 *	xrttimetag.h: definitions and declarations for xrttimetag
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTTIMETAG_H
#define XRTTIMETAG_H

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
#include <xrttimefunc.h>
#include <xrtcaldb.h>
#include <headas_gti.h>

/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>
#include <coordfits.h>
#include <teldef.h>
#include <ephemeris.h>
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
#define PAR_TRFILE		   "trfile"
#define PAR_USEHKKEY		   "usehkkey"
#define PAR_USESRCDET		   "usesrcdet"
#define PAR_ATTFILE		   "attfile"
#define PAR_NPIXELS		   "npixels"

#define PAR_PERCENT                "percent"
#define PAR_TELDEF                 "teldef"
#define PAR_SRCRA                  "srcra"
#define PAR_SRCDEC                 "srcdec"
#define PAR_RANOM                  "ranom"
#define PAR_DECNOM                 "decnom"
#define PAR_ABERRATION             "aberration"
#define PAR_ATTINTERPOL            "attinterpol"
#define PAR_SRCDETX                "srcdetx"
#define PAR_SRCDETY                "srcdety"
#define PAR_COLFILE                "colfile"

/* miscellaneous */
#define BINTAB_ROWS       1000
#define PD_TIMEDEL     0.00014
#define MIN_EXPOTIME   0.05

typedef struct gti_struct GTIInfo_t;

typedef struct {
  int ccdframe;
  double hktime;
  double delta;
  int    amp;
  int    wave;
} DeltaT_t;



typedef struct {
  double fst;
  double fet;
  int    firstcol;
  int    cols;
} TrailerT_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char infile[PIL_LINESIZE];     
    char outfile[PIL_LINESIZE];               
    char hdfile[PIL_LINESIZE];                
    char trfile[PIL_LINESIZE];                
    char attfile[PIL_LINESIZE]; 
    char colfile[PIL_LINESIZE]; 
    int npixels;
    int srcdetx;
    int srcdety;
    double percent;
    char teldef[PIL_LINESIZE];
    double srcra;
    double srcdec;
    double ranom;
    double decnom;
    BOOL usehkkey;
    BOOL usesrcdet;
    BOOL aberration;
    BOOL attinterpol;
  } par;
  char tmpfile[FLEN_FILENAME];    
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  BOOL hist;
  BOOL newquat;
  BOOL slew;
  BOOL pd;
  BOOL biason;
  BOOL telemcorr;
  BOOL warning;
  BOOL rawxcorr;
  SPTYPE dateobs;
  SPTYPE timeobs;
  WTOffsetT_t *wtcol;
  int wtcoloffset;
  int hdrows;
  int colrows;
  char strhist[256];
  char date[25];
  char obsmode[15];
  char datamode[15];
  int  xrtmode;
  GTIInfo_t newgti;
  double    timedel;
  int       wm1stcol;
  int       wmcolnum;
  int       count;
  DeltaT_t *header;
  int trnrows;
  TrailerT_t *trailer;
} Global_t;

extern Global_t global;

typedef struct {
  unsigned CCDFrame;
  unsigned TIME_RO;
  unsigned TIME;
  unsigned RAWX;
  unsigned RAWXTL;
  unsigned RAWY;
  unsigned DETX;
  unsigned DETY;
  unsigned X;
  unsigned Y;
  unsigned OFFSET;
  unsigned Amp;
  unsigned PHA;
} Ev2Col_t;

typedef struct {
  unsigned CCDFrame;
  unsigned FSTS;
  unsigned FSTSub;
  unsigned FETS;
  unsigned FETSub;
  unsigned CCDEXPOS;
  unsigned CCDEXPSB;
  unsigned XRTMode;
  unsigned PixGtULD;
  unsigned Settled;
  unsigned In10Arcm;
  unsigned InSafeM;
  unsigned EVTLOST;
  unsigned Amp;
  unsigned nPixels;
  unsigned PDBIASLVL;
  unsigned WAVE;

} HKCol_t;


typedef struct {
  unsigned FrstFST;
  unsigned FrstFSTS;
  unsigned LastFST;
  unsigned LastFSTS;
  unsigned WM1STCOL;
  unsigned WMCOLNUM;

} TRCol_t;

/* Prototypes */
int xrttimetag(void);
int xrttimetag_work(void);
int xrttimetag_getpar(void);
void xrttimetag_info(void);
int xrttimetag_checkinput(void);
int Timetag(FitsFileUnit_t inevent, FitsFileUnit_t ounit);
int CalcTimeRows(void);
int CalcTimePixs(void);
int AppendGTI(FitsFileUnit_t ounit);
int WTComputeCoord(int rawx, int rawy, double src_x, double src_y, int amp, double time, TELDEF* teldef, ATTFILE* attfile,  Coord_t * coord);
int PDComputeCoord(double src_x, double src_y, int amp, double time, TELDEF* teldef, ATTFILE* attfile,  Coord_t * coord);
int GetRADecKeyVal(char *filename, double *ranom, double *decnom, double *srcra, double *srcdec);
int SlewTimetag(FitsFileUnit_t evunit, FitsFileUnit_t ounit);
int GetGtiValues(struct gti_struct *gti, unsigned XRTmode, char *obs_mode);
int AddGti( struct gti_struct *gti, double start, double stop );
int ComputeDetCoord(double src_x, double src_y, int amp, double time, TELDEF* teldef, ATTFILE* attfile,  Coord_t * coord);
int GetCoordKeyVal(FitsHeader_t	*hkhead);
int ReadTrFile(void);

#endif
