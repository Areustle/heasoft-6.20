/*
 *	xrtcalcpi.h: definitions and declarations for xrtcalcpi
 *
 *      HISTORY CHANGES:
 *            FT 08/05/2003 added input parameters 'rawx' and 'rawy'
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTCALCPI_H
#define XRTCALCPI_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>

#include <math.h>
#include <xrt_ran2.h>

#include <unistd.h> /* for getpid */

/* xrt local headers */
#include <xrt_termio.h>
#include <highfits.h>
#include <xrtfits.h>
#include <xrtdefs.h>
#include <misc.h>
#include <xrtdasversion.h>
#include <xrtcaldb.h>
#include <xrtbpfunc.h>
/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>
#include <headas_utils.h>

#ifndef errno
extern int errno;
#endif

/*******************************
*      defines / typedefs      *
********************************/


/* input parameter names */

#define PAR_INFILE		   "infile"
#define PAR_OUTFILE		   "outfile"
#define PAR_HDFILE                 "hdfile"
#define PAR_GAINFILE               "gainfile"
#define PAR_SEED                   "seed"
#define PAR_RANDOMFLAG             "randomflag"
#define PAR_GAINNOM                "gainnom"
#define PAR_OFFSET                 "offset"
#define PAR_USERCTCORRPAR          "userctcorrpar"
#define PAR_CORRTYPE               "corrtype"
#define PAR_ALPHA1                 "alpha1"
#define PAR_ALPHA2                 "alpha2"
#define PAR_EBREAK                 "ebreak"
#define PAR_SAVEPINOM              "savepinom"
#define PAR_SAVEPIOFFSET           "savepioffset"
#define PAR_USERCTIPAR             "userctipar"
#define PAR_BETA1                  "beta1"
#define PAR_BETA2                  "beta2"
#define PAR_ECTI                   "ecti"
#define PAR_OFFSETNITER            "offsetniter"

#define TOTAL_MODE                 "TOTAL"
#define NOMINAL_MODE               "NOMINAL"
#define TRAPS_MODE                 "TRAPS"


/* miscellaneous */
#define BINTAB_ROWS       1000
#define NUM_GAIN_COLS     3


/* To be moved in 'libswxrt.so' library  */
#define CLNM_GC0_TRAP         "GC0_TRAP"
#define CLNM_GC1_TRAP         "GC1_TRAP"
#define CLNM_GC2_TRAP         "GC2_TRAP"
#define CLNM_GC3_TRAP         "GC3_TRAP"
#define CLNM_GC4_TRAP         "GC4_TRAP"
#define CLNM_GC5_TRAP         "GC5_TRAP"
#define GAINCOEFF_NUM_NEW           12        /* to overwrite GAINCOEFF_NUM definition */
#define CLNM_PI_OFFSET        "PI_OFFSET"
#define CARD_COMM_PI_OFFSET   "Pulse Invariant Charge Trap Offset"


/* I/O Data structure, photon counting columns from:  
Swift XRT Science FITS Format Design v0.6 June 29,2001) */

typedef struct {
  unsigned TIME;
  unsigned CCDFrame;
  unsigned Amp;
  unsigned X;
  unsigned Y;
  unsigned RAWX;
  unsigned RAWY;
  unsigned DETX;
  unsigned DETY;
  unsigned PHAS; 
  unsigned STATUS; 
  unsigned PHA; 
  unsigned ENERGY; 
  unsigned PI;
  unsigned PI_NOM;
  unsigned GRADE;  
  unsigned ABOVE;
  unsigned TIME_RO;
  unsigned PI_OFFSET;
} Ev2Col_t;


typedef struct {
  unsigned TIME;
  unsigned CCDTEMP;
  unsigned GC0;
  unsigned GC1;
  unsigned GC2;
  unsigned GC3;
  unsigned GC4;
  unsigned GC5;
  unsigned GC0_TRAP;
  unsigned GC1_TRAP;
  unsigned GC2_TRAP;
  unsigned GC3_TRAP;
  unsigned GC4_TRAP;
  unsigned GC5_TRAP;
  unsigned RAWX; 
  unsigned RAWY;
  unsigned YEXTENT;
  unsigned OFFSET;
  unsigned ALPHA1;
  unsigned ALPHA2;
  unsigned EBREAK;
  unsigned BETA1;
  unsigned BETA2;
  unsigned E_CTI;
  unsigned BETA1_TRAP;
  unsigned BETA2_TRAP;
  unsigned E_CTI_TRAP;
} GainCol_t;



typedef struct {
  float ccdtemp[3];
  float gc0[3];
  float gc1[3];
  float gc2[3];
  float gc3[3];
  float gc4[3];
  float gc5[3];
  float gc0_trap[3];
  float gc1_trap[3];
  float gc2_trap[3];
  float gc3_trap[3];
  float gc4_trap[3];
  float gc5_trap[3];
  int   *rawx;
  int   *rawy;
  int   *yextent;
  float *offset2;
  float alpha1;
  float alpha2;
  float ebreak;
  float beta1;
  float beta2;
  float e_cti;
  float beta1_trap;
  float beta2_trap;
  float e_cti_trap;
  double gtime;
} GainRow_t;

typedef struct {
  double hktime;
  int hkccdframe;
  float temp;
} Temp_t;

typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char infile[PIL_LINESIZE];     /* Name of FITS file */
    char outfile[FLEN_FILENAME];    /* Name of FITS output file */
    char hdfile[FLEN_FILENAME];    /* Name of HD FITS output file */
    char gainfile[FLEN_FILENAME];   /* Name of FITS output file */
    char corrtype[PIL_LINESIZE];    /* PI correction type */
    BOOL userctcorrpar;             /* Parameters for charge traps correction input by the user?(yes/no) */
    BOOL userctipar;                /* Parameters for GC1/GC2 energy dependence input by the user?(yes/no) */
    BOOL randomflag;                /* Randomize PHA */
    BOOL savepinom;
    BOOL savepioffset; 
    int seed;
    double gainnom;                 /* Nominal gain */
    double offset;                  /* Offset in channel/energy relationship */
    double alpha1;                  /* powerlaw index for Energy <= break energy for charge traps */
    double alpha2;                  /* powerlaw index for Energy > break energy for charge traps */
    double ebreak;                  /* break energy for charge traps */
    double beta1;                   /* powerlaw index for Energy <= break energy */
    double beta2;                   /* powerlaw index for Energy > break energy */
    double ecti;                    /* break energy */
    int    offsetniter;             /* Number of iterations for charge traps correction */
  } par;
/*----------   Output Parameters ---------------------*/
  char datamode[15];
  char obsmode[15];
/*------------------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  char tmpfile[FLEN_FILENAME];    /* Name of FITS output file */
  BOOL hist;
  BOOL oldgainformat;
  BOOL offset2format;
  BOOL warning;
  char strhist[256];
  SPTYPE dateobs;
  SPTYPE timeobs;
  char date[25];
  double tstart;
  double tstop;
  int xrtvsub;
  GainRow_t *gaincoeff;
  double nomgain;
  unsigned caldbrows;
  int caldbdim;
  Temp_t * temp;
  int xrtmode;
  int count;
  int ct1stcol;
  int ctcolnum;
} Global_t;

typedef struct {
  unsigned CCDFrame;
  unsigned FSTS;
  unsigned FSTSub;
  unsigned XRTMode;
  unsigned PixGtULD;
  unsigned Settled;
  unsigned In10Arcm;
  unsigned InSafeM;
  unsigned TIME;
  unsigned CCDTemp;
} HKCol_t;

extern Global_t global;

/* Prototypes */

int xrtcalcpi(void);
int xrtcalcpi_work(void);
int xrtcalcpi_getpar(void);
int xrtcalcpi_checkinput(void);
void xrtcalcpi_info(void);
int PhotonCountingPha2Pi(FitsFileUnit_t evunit,FitsFileUnit_t ounit);
int WindowedTimingPha2Pi(FitsFileUnit_t evunit,FitsFileUnit_t ounit);
int PhotodiodePha2Pi(FitsFileUnit_t evunit,FitsFileUnit_t ounit);
int GetGainFileCoeff(void);
int ReadTemperatures(void);
int CalculateGainCoeff(double time, double temp, double coeff[GAINCOEFF_NUM_NEW], double coeff_corr[6]);
int CalculateTempCoeff(double temp, int rowsnum, double coeff[GAINCOEFF_NUM_NEW]);
int CalculateGainOffset2(double time, double temp, double en, int rawx, int rawy, double *offset2);
int CalculateTempOffset2(double temp, double en, int rowsnum, int colsnum, double *tempoffset2);
#endif
