/*
 *	xrtpdcorr.h: definitions and declarations for xrtpdcorr
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTPDCORR_H
#define XRTPDCORR_H

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
#include <slatec.h>

#ifndef errno
extern int errno;
#endif

/*******************************
*      defines / typedefs      *
********************************/
/* input parameter names */

#define PAR_INFILE		   "infile"
#define PAR_OUTFILE		   "outfile"
#define PAR_BIASFILE		   "biasfile"
#define PAR_BIAS                   "bias"
#define PAR_BIASDIFF               "biasdiff"
#define PAR_THRFILE		   "thrfile"
#define PAR_BIASTH                 "biasth"
#define PAR_NEVENTS                "nevents"
#define PAR_HDFILE                 "hdfile"
#define PAR_METHOD                 "method"
#define PAR_FITTOL                 "fittol"
#define PAR_NCLIP                  "nclip"
#define PAR_NSIGMA                 "nsigma"
#define PAR_NFRAME		   "nframe"

/* miscellaneous */
#define BINTAB_ROWS       1000
#define SAT_VAL           4095        /* Saturation value */
#define LRBIASPIX_MUL       20
#define FUNC_POINTS        150
#define STEMLEN             14
/* Gaussian Fit */

#define MAX_FIT_PARM	     4        /* max. # of free fit parameter	*/

#define GUESS_FPARM1      1000
#define GUESS_FPARM3        15
#define GUESS_FPARM2       120

#define KWNM_METHOD           "XRTMETHD"
#define CARD_COMM_METHOD      "Method used to compute bias value"


#define DGAUSS_CONST       0.1

#define DF_CONST          "CONST"
#define DF_SG             "SG"
#define DF_DG             "DG"
#define DF_MN             "MN"

typedef struct {
  unsigned PHA; 
  unsigned RAWPHA;
  unsigned CCDFrame;
  unsigned OFFSET;
  unsigned TIME_RO;
} Ev2Col_t;


typedef struct {
  int level;
  unsigned counts;
} BiasPix_t;



typedef struct {
  unsigned CCDFrame;
  unsigned XRTMode; 
  unsigned BiasPx;
  unsigned HPIXCT;
  unsigned FSTS;
  unsigned FSTSub;
  unsigned PDBIASLVL;
} HKCol_t;

typedef struct {
  unsigned ccdframe;
  double   bias; 
} BiasFormat_t;

typedef struct {
  double   hktime;
  double   pdbiaslvl; 
} BiasSub_t;


typedef struct {
  unsigned ccdframe;
  double   time;
  double      diff; 
} BiasDiffFormat_t;

typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];  /* Name of FITS file and [ext#] */
    char        outfile[PIL_LINESIZE];             /* Name of FITS output file */
    char        biasfile[PIL_LINESIZE];            /* Name of FITS bias calibration file */
    char        hdfile[PIL_LINESIZE];              /* Name of FITS bias calibration file */
    char        thrfile[PIL_LINESIZE];
    double      bias;                              /* Bias value  */
    int         biasdiff;
    int         nevents;                           /* pixels threshold  */
    int         biasth;                               /* Lower level discriminator */
    double      fittol;
    char        method[PIL_LINESIZE];
    int         nclip;
    int         nsigma;
    int         nframe;   
  } par;
/*------------------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  double *xvec;
  double *yvec;
  double tstart;
  double tstop;
  SPTYPE dateobs;
  SPTYPE timeobs;
  int    xrtvsub;
  BOOL hist;
  BOOL lr;
  BOOL pu;
  BOOL first;
  BOOL caldb;
  BOOL oldtlm;
  BOOL biasonbd;
  BOOL warning;
  BOOL usethrfile;
  char method[10];
  char strhist[256];
  char date[25];
  char tmpfile[PIL_LINESIZE];
  unsigned caldbrows;
  BiasRow_t *biasvalues;
  unsigned biasrows;
  BiasFormat_t *outbias;
  BiasDiffFormat_t * diff;
  int    biaslvlrow;
  BiasSub_t *hkbias;
  int    totdiff;
  double FitParm[MAX_FIT_PARM];	/* actual values of fit param.	*/
  BiasPix_t *biashisto;
  int bias_lo;
  int bias_hi;
  int tot_counts;
  int tot_chan;
  int dim_buff;
} Global_t;

extern Global_t global;

/* I/O Data structure, photon counting columns from:  
Swift XRT Science FITS Format Design v0.6 June 29,2001) */

/* Prototypes */

int xrtpdcorr();
int xrtpdcorr_work(void);
int xrtpdcorr_getpar(void);
void xrtpdcorr_info(void);
int xrtpdcorr_checkinput(void);
int PDPhaCorrect(FitsFileUnit_t inevent,FitsFileUnit_t ounit);
int GetBiasValues(void);
int FitBias(unsigned nFitParm,double GaussFitParm[MAX_FIT_PARM], 
	    double *RedChiSq, BOOL UseSingleGaussian, double mean, double sig_mean);
double ReducedChiSquare(double Vec[], int nPoints, int nVars);
BOOL PlotFitSpec(int Spec[LRBIASPIX_MUL], int nChans, int ChanMin, int ChanMax, double FitParm[MAX_FIT_PARM],
		 BOOL UseSingleGaussian, char *PlotDev);
/*  int BiasValue(double * bias, unsigned ccdframe_start, double time, unsigned *ccdframe_stop); */
int BiasValue(double * bias, unsigned ccdframe_start, double time, double *stoptime);
int PDBiasVal( double * bias,unsigned ccdframe_start, double time, double *stoptime);
int CALDBBiasVal(double * bias);
int ComputeMean(double * mean_db, double * sig_mean);
int FillBiasBuff(unsigned ccdframe_start, double time, double *stoptime);
int CopyBiasKeywords(FitsFileUnit_t inunit,FitsFileUnit_t outunit);
int WriteBiasExtension(FitsFileUnit_t inunit, FitsFileUnit_t outunit, int nrows, BiasFormat_t *bias);
BOOL ReadInputFileKeywords(void);
int PhaReCorrect(FitsFileUnit_t evunit, FitsFileUnit_t ounit);
int ComputeBiasDiff(void);
int ComputePUMedian(void);
int ReadHKForPU(void);
#endif
