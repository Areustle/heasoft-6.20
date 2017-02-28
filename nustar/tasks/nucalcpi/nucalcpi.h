/*
 *	nucalcpi.h: definitions and declarations for nucalcpi
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */

#ifndef NUCALCPI_H
#define NUCALCPI_H

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
#include "nustardasversion.h"



#ifndef errno
extern int errno;
#endif

/*******************************
*      defines / typedefs      *
********************************/
/* input parameter names */

#define PAR_INFILE		 "infile"
#define PAR_HKFILE		 "hkfile"
#define PAR_GAINFILE		 "gainfile"
#define PAR_OUTFILE		 "outfile"
#define PAR_TEMPERATURE		 "temperature"
#define PAR_CLCFILE		 "clcfile"
#define PAR_CLCFILTERFILE	 "clcfilterfile"


/* miscellaneous */
#define TIME_SENS                0.0000001
#define PHAS_SENS                0.0000000001

#define KEV2PICH                 25

#define GAIN_TEMP_DIM            3
#define GAIN_COEFF_DIM           3
#define CLC_GRADES_DIM           13

#define BINTAB_ROWS              1000


typedef struct {
  unsigned TIME;
  unsigned RAWX;
  unsigned RAWY;
  unsigned DET_ID;
  unsigned GRADE;
  unsigned SWTRIG;
  unsigned PHAS;
  unsigned PIS_GAIN;
  unsigned SURRPI;
  unsigned PI_CLC;
  unsigned PI;
} EvtCol_t;

typedef struct {
  char instrume[FLEN_VALUE];
  SPTYPE dateobs;
  SPTYPE timeobs;
  SPTYPE dateend;
  SPTYPE timeend;
  DTYPE  tstart;
  DTYPE  tstop;
} EVTInfo_t;


typedef struct {
  unsigned TIME;
  unsigned CZT0TEMP;
  unsigned CZT1TEMP;
  unsigned CZT2TEMP;
  unsigned CZT3TEMP;
} HKCol_t;

typedef struct {
  double time;
  float  czt0temp;
  float  czt1temp;
  float  czt2temp;
  float  czt3temp;
} HKRow_t;


typedef struct {
  unsigned TIME;
  unsigned RAWX;
  unsigned RAWY;
  unsigned TEMP;
  unsigned SLOPE;
  unsigned OFFSET;
} GainCol_t;

typedef struct {
  double time;
  float  temp[GAIN_TEMP_DIM];
  float  slope[GAIN_COEFF_DIM];
  float  offset[GAIN_COEFF_DIM];
} GainInfo_t;

typedef struct {
  GainInfo_t *info;
  int        ninfo;
} GainData_t;

typedef struct {
  unsigned TIME;
  unsigned RAWX;
  unsigned RAWY;
  unsigned CLC;
  unsigned GR_SLOPE;
  unsigned GR_OFFSET;
} ClcCol_t;

typedef struct {
  double time;
  float  clc[CLC_GRADES_DIM];
  float  gr_slope[CLC_GRADES_DIM];
  float  gr_offset[CLC_GRADES_DIM];
} ClcInfo_t;

typedef struct {
  ClcInfo_t  *info;
  int        ninfo;
} ClcData_t;


typedef struct {
  unsigned ELOW;
  unsigned EHIGH;
} ClcFilterCol_t;

typedef struct {
  float elow;
  float ehigh;
} ClcFilterInfo_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];       /* Name of input FITS file  */
    char        hkfile[PIL_LINESIZE];       /* Name of input Housekeeping Header Packets FITS file*/
    char        gainfile[PIL_LINESIZE];     /* Name of input GAIN file or CALDB */
    char        outfile[PIL_LINESIZE];      /* Name of output FITS file */
    char        clcfile[PIL_LINESIZE]; 
    char        clcfilterfile[PIL_LINESIZE]; 
    double      temperature;
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
int nucalcpi();
int nucalcpi_work(void);
int nucalcpi_getpar(void);
void nucalcpi_info(void);
int nucalcpi_checkinput(void);

int ComputePHAtoPI(FitsFileUnit_t evunit, FitsFileUnit_t ounit);

int ReadTemperatures(HKRow_t **hkinfo, int *hknrows);

int ReadGainFile(GainData_t gaindata[4][DET_PIXS][DET_ROWS]);
int ReadGainInfo(GainData_t gaindata[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam);

int ReadClcFile(ClcData_t clcdata[4][DET_PIXS][DET_ROWS]);
int ReadClcInfo(ClcData_t clcdata[DET_PIXS][DET_ROWS], char *filename, long extno, char *detnam);

int ReadClcFilterFile(ClcFilterInfo_t ** info, int *count);

int GetTemperatures(double time, const HKRow_t *hkinfo, int hknrows, float temp[4]);

int GetGainCoeff(const GainData_t *gaindata, double time, float hktemp, double *slope, double *offset);

void GetGainCoeffbyTemp(const GainInfo_t *info, float hktemp, double *slope, double *offset);

void InterpolateValues(double down, double up, double this, double valueup, double valuedown, double *value);

int GetClcData(const ClcData_t *clcdata, double time, int grade, double *clcval, double *gr_offset, double *gr_slope);

int ApplyEnCorrection(ClcFilterInfo_t *info, int infocount, float en);


#endif
