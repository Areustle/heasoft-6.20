/*
 *	xrtimage.h: definitions and declarations for xrtimage
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTIMAGE_H
#define XRTIMAGE_H

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
#include <xrtbpfunc.h>
#include <xrttimefunc.h>
/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>
#include <headas_gti.h>

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
#define PAR_BIASFILE   		   "biasfile"
#define PAR_SRCFILE   		   "srcfile"
#define PAR_BIAS   		   "bias"
#define PAR_BIASDIFF   		   "biasdiff"
#define PAR_BPFILE                 "bpfile"
#define PAR_BPTABLE                "bptable"
#define PAR_OUTBPFILE              "outbpfile"
#define PAR_USERBPFILE             "userbpfile"
#define PAR_CLEANBP                "cleanbp"
#define PAR_SUBBIAS                "subbias"
#define PAR_GTISCREEN              "gtiscreen"
#define PAR_GTIFILE                "gtifile"
#define PAR_CLEANSRC               "cleansrc"
#define PAR_MAXTEMP                "maxtemp"

#define BINTAB_ROWS                1000
#define NUM_ROUND_POS     5    /* Number of decimal to round tstart, tstop - Should be different to 0 */
typedef struct gti_struct GTIInfo_t;


typedef struct
{
  unsigned CCDFrame;
  unsigned XRTMode;
  unsigned TIME;
  unsigned HPIXCT;
  unsigned BIASPX;
  unsigned IMBLVL;
  unsigned CCDTEMP;
}  HKCol_t;

typedef struct
{
  unsigned ccdframe;
  double   hktime;
  int      xrtmode;
  int      imblvl;
  int      median;
  double   temp;
}  HKInfo_t;


typedef struct {

  unsigned  naxis1;             /* NAXIS1 keyword  */
  unsigned  naxis2;             /* NAXIS2 keyword  */
  int       nullval;            /* BLANK keyword  */
  double    refvalx;            /* CRVAL1 keyword */
  double    refvaly;            /* CRVAL2 keyword */
  double    refx;               /* CRPIX1 keyword */
  double    refy;               /* CRPIX2 keyword */
  double    pixdeltx;           /* CDELT1 keyword */
  double    pixdelty;           /* CDELT2 keyword */
  unsigned  xmax;
  unsigned  ymax;
  unsigned  xmin;
  unsigned  ymin;
  int       type;               /* Coordinate type flag: 0 - raw coordinates 
                                                         1 - det coordinates
                                                         2 - sky coordinates 
				   FitsImageType_t number
				*/
} ImageVars_t;

typedef enum { RAW = 0, DET, SKY }        FitsImageType_t;
char *FitsImageTypeString[] = { "RAW", "DET","SKY" };
int  FitsImageNullvalDef[] = { -1, -1 };
double FitsImageRefvalxDef[] = { 0, 1 };
double FitsImageRefvalyDef[] = { 0, 1 };
double FitsImageRefxDef[] = { 0, 0 };
double FitsImageRefyDef[] = { 0, 0 };
double FitsImagePixdeltxDef[] = { 1, 1 };
double FitsImagePixdeltyDef[] = { 1, 1};


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char    infile[PIL_LINESIZE];     /* Name of FITS file and [ext#] */
    char    outfile[PIL_LINESIZE];               /* Name of FITS output file */
    char    hdfile[PIL_LINESIZE];
    char    biasfile[PIL_LINESIZE];              /* Name of bias FITS input file */
    char    srcfile[PIL_LINESIZE];          
    char    bpfile[PIL_LINESIZE];           /* Name of badpixels CALDB input file */
    char    bptable[PIL_LINESIZE];               /* Name of on-board bad pixel list */
    char    userbpfile[PIL_LINESIZE];            /* Name of user bad pixels input file */
    char    outbpfile[PIL_LINESIZE];             /* Name of output bad pixels file */
    BOOL    cleanbp;
    BOOL    subbias;
    BOOL    cleansrc;
    double  bias;
    double  maxtemp;
    int     biasdiff;
    BOOL    gtiscreen;    
    char    gtifile[MAX_FILES*FLEN_FILENAME];               /* Name of gtifile */
  } par;
  char         taskname[FLEN_FILENAME];              /* Name and version of the task */
  Version_t    swxrtdas_v;                           /* SWXRTDAS version */
  BOOL         hist;
  BOOL         biascal;
  BOOL         phaco;
  BOOL         oldsoft;
  SPTYPE       dateobs;
  SPTYPE       timeobs;
  int          gti_rows;
  double       min_tstart;
  double       max_tstop;
  double       tstart;
  double       tstop; 
  int          bpcount;
  char         strhist[256];
  char         date[25];
  char         tmpfile[FLEN_FILENAME];
  unsigned     caldbrows;
  BiasRow_t    *biasvalues;
  GtiCol_t     *gtivalues;
  CalSrcInfo_t calsrc[4]; 
  ImageVars_t  im;
  HKInfo_t     *hkinfo;
  int          hkrow;
  BOOL createbp;
  
} Global_t;

extern Global_t global;
/* I/O Data structure, photon counting columns from:  
Swift XRT Science FITS Format Design v0.6 June 29,2001) */

/* Prototypes */

int xrtimage();
int xrtimage_work();
int xrtimage_getpar();
void xrtimage_info(void);
int ProcImage (FitsFileUnit_t, FitsFileUnit_t, double *, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS], int, int *);
int GetBiasValues ( void );
int InitImagingVars ( FitsHeader_t );
int GetCoordinateType ( FitsHeader_t );
int JValCust( FitsCard_t *, int *);
int DValCust( FitsCard_t *, double *);
int CreateBPMap(BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *bp_table_amp2[CCD_PIXS][CCD_ROWS]);
int WriteBpKey(FitsFileUnit_t unit, FitsFileUnit_t badunit, char *outbpfile);
int CheckGti(FitsHeader_t   head, BOOL *checkgti);
int LoadGtiTable(double *,double *);
double round_num(double n);
int ComputeBiasValue(FitsHeader_t   head,double * bias);
int WriteHistBlock(FitsFileUnit_t outunit);
int xrtimage_checkinput(void);
int GetCalSourcesInfo(void);
int ReadHK(void);
#endif
