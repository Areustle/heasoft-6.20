/*
 *	xrtpccorr.h: definitions and declarations for xrtpccorr
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTPCCORR_H
#define XRTPCCORR_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <unistd.h> /* for getpid */
#include <math.h>


/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>
#include <headas_gti.h> 
#include <coordfits.h>
#include <teldef.h>
#include <ephemeris.h>


/* xrt local headers */
#include <highfits.h>
#include <xrt_termio.h>
#include <xrtfits.h>
#include <xrtdefs.h>
#include <xrtcaldb.h>
#include <misc.h>
#include <xrtdasversion.h>
#include <xrttimefunc.h>
#include <xrtbpfunc.h>


#ifndef errno
extern int errno;
#endif

/*******************************
*      defines / typedefs      *
********************************/
/* input parameter names */

#define PAR_INFILE		   "infile"
#define PAR_OUTFILE		   "outfile"
#define PAR_ATTFILE		   "attfile"
#define PAR_TELDEF                 "teldef"
#define PAR_MKFFILE                "mkffile"
#define PAR_BIASMETHOD             "biasmethod"
#define PAR_GRADEMIN               "grademin"
#define PAR_GRADEMAX               "grademax"
#define PAR_SRCDETX                "srcdetx"
#define PAR_SRCDETY                "srcdety"
#define PAR_SRCRA                  "srcra"
#define PAR_SRCDEC                 "srcdec"
#define PAR_RANOM                  "ranom"
#define PAR_DECNOM                 "decnom"
#define PAR_ABERRATION             "aberration"
#define PAR_ATTINTERPOL            "attinterpol"
#define PAR_SUBIMGSIZE             "subimgsize"
#define PAR_NEVENTS                "nevents"
#define PAR_SUBIMAGE               "subimage"
#define PAR_LESSEVENTS             "lessevents"
#define PAR_BADPIX                 "badpix"


/* miscellaneous */
#define BINTAB_ROWS       1000
#define SAT_VAL           4095        /* Saturation value */
#define DIFF_TIME         1000.
#define DTIME             1000.

#define V_CENTER            0
#define V_UP                1
#define V_LEFT              2
#define V_RIGHT             3
#define V_DOWN              4

#define BIAS_MEDIAN       "MEDIAN"
#define BIAS_MEAN         "MEAN"


#define KWNM_XPHASCO            "XPHASCO"
#define CARD_COMM_XPHASCO       "Has PHAS been corrected on ground (T/F)?"

#define CLNM_PHASO	        "PHASO"	        /* */
#define CARD_COMM_PHASO         "Array of Pulse Height Analyzer on-board values"


typedef struct {
  unsigned PHAS;
  unsigned PHASO;
  unsigned GRADE;
  unsigned STATUS;
  unsigned CCDFrame;
  unsigned DETX;
  unsigned DETY;
  unsigned TIME;
  unsigned RAWX;
  unsigned RAWY;
  unsigned Amp;
} Ev2Col_t;


typedef struct {
  unsigned TIME;
  unsigned SAFEHOLD;
  unsigned SETTLED;
} MKFCol_t;

typedef struct {
double start_time;
int bias;
int pix2bias;
int pix4bias;
int pix7bias;
int pix9bias;
int nevents;

} SubSetInfo_t;

typedef struct {
  int    x_min;
  int    x_max;
  int    y_min;
  int    y_max;
  int    tot_subset;
  SubSetInfo_t *subset;

} SubImagesInfo_t;

typedef struct {
  double time;
  int    srcdetx;
  int    srcdety;
  int    tot_subimg;
  int    total_img;
  int    central_img;
  SubImagesInfo_t *subimg;
} MKFInfo_t;

typedef struct {
  double tstart;
  double tstop;
  double mjdref;
  LTYPE  xphasco;
  int    whalfwd;
  int    whalfht;
  int    img_xmin;
  int    img_xmax;
  int    img_ymin;
  int    img_ymax;
  SPTYPE dateobs;
  SPTYPE dateend;
  SPTYPE timeobs;
  double mjdrefi;
  double mjdreff;
  char   timesys[15];
  char   timeref[15];
  char   tassign[15];
  char   timeunit[15];
  char   obsid[15];
  char   origin[15];
  int    target;
  char   obsmode[15];
  char   datamode[15];

} EVTInfo_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];  /* Name of FITS file and [ext#] */
    char        outfile[PIL_LINESIZE]; /* Name of FITS output file */
    char        attfile[PIL_LINESIZE];
    char        teldef[PIL_LINESIZE];
    char        mkffile[PIL_LINESIZE];
    char        biasmethod[PIL_LINESIZE];
    int         grademin;
    int         grademax;
    int    srcdetx;
    int    srcdety;
    double srcra;
    double srcdec;
    double ranom;
    double decnom;
    BOOL aberration;  
    BOOL attinterpol; 
    int    subimgsize;
    int    nevents;
    BOOL subimage;
    BOOL lessevents;
    BOOL badpix;

  } par;
/*------------------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];
  char tmpfile[PIL_LINESIZE];
  BOOL use_detxy;
  EVTInfo_t evt;
  MKFInfo_t *orbits;
  int tot_orbits;
  BOOL xbadfram;

} Global_t;

extern Global_t global;


/* I/O Data structure, photon counting columns from:  
Swift XRT Science FITS Format Design v0.6 June 29,2001) */

/* Prototypes */

int xrtpccorr();
int xrtpccorr_work(void);
int xrtpccorr_getpar(void);
void xrpccorr_info(void);
int xrtpccorr_checkinput(void);
int PCPhaReCorrect(FitsFileUnit_t evunit, FitsFileUnit_t ounit, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS]);
int ComputeSrc_XY(FitsFileUnit_t evunit, TELDEF* teldef, double* src_x, double* src_y);
int ComputeDetCoord(double src_x, double src_y, double time, double mjdref, TELDEF* teldef, ATTFILE* attfile,  Coord_t * coord);
int ComputeOrbits(double src_x, double src_y);
int Compute_SubImages();
void ComputeImgRange(int center_x, int center_y, unsigned versus, unsigned n);
int ComputeBiasMedian(FitsFileUnit_t evunit);
int UpdatePCCorrExt(FitsFileUnit_t outunit);
int AddSwiftXRTKeyword(FitsFileUnit_t inunit, int hdunum);
int CalcMedian(int * bias, int length);
int ReadBPExt(FitsFileUnit_t evunit,int hducount, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS]);


#endif
