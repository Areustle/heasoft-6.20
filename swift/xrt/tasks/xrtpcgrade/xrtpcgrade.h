/*
 *	xrtpcgrade.h: definitions and declarations for xrtpcgrade
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTPCGRADE_H
#define XRTPCGRADE_H

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
#include <xrtcaldb.h>
/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>
#include <hdcal.h>

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
#define PAR_GRADEFILE		   "gradefile"
#define PAR_THRFILE		   "thrfile"
#define PAR_SPLIT                  "split"
#define PAR_ASCAGRADE              "ascagrade"

/* miscellaneous */
#define BINTAB_ROWS       1000

typedef struct {
  int amp;
  int gradeid;
  int grade[PHAS_MOL];
} GradeRow_t;

typedef struct {
  int ccdframe;
  double hktime;
  int split;
} SplitValue_t;

typedef struct {
  unsigned CCDFrame;
  unsigned TIME;
  unsigned SplitThr;
  unsigned XRTMode;
  unsigned Settled;
  unsigned In10Arcm;
  unsigned InSafeM;
} HKCol_t;

typedef struct {
  int    xrtvsub;
  SPTYPE dateobs;
  SPTYPE timeobs;
} EVTInfo_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char infile[PIL_LINESIZE];     /* Name of FITS file and [ext#] */
    char hdfile[PIL_LINESIZE];
    char outfile[PIL_LINESIZE];               /* Name of FITS output file */
    char gradefile[PIL_LINESIZE];             /* Name of FITS CALDB grade file */
    char thrfile[PIL_LINESIZE];
    int  split;                                /* Split threshold level   */
    BOOL ascagrade;
  } par;
  char taskname[PIL_LINESIZE]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  GradeRow_t *gradevalues;
  int maxrows;
  BOOL hist;
  BOOL warning;
  BOOL usethrfile;
  char strhist[256];
  char date[25];
  char tmpfile[PIL_LINESIZE];
  int grade_tot[33];
  int grade_asca[8];
  int sat_event;
  int sat_asca;
  JTYPE grademin;
  JTYPE grademax;
  SplitValue_t *split;
  int scount;
  EVTInfo_t evt;

} Global_t;

extern Global_t global;

/* I/O Data structure, photon counting columns from:  
Swift XRT Science FITS Format Design v0.6 June 29,2001) */

typedef struct {
  unsigned PHAS; 
  unsigned PHA; 
  unsigned ASCAPHA; 
  unsigned PI;
  unsigned GRADE;  
  unsigned ASCAGRADE;  
  unsigned ABOVE;
  unsigned Amp;
  unsigned TIME;
  unsigned CCDFrame;
} Ev2Col_t;


/* Prototypes */

int xrtpcgrade();
int xrtpcgrade_work();
int xrtpcgrade_getpar();
void xrtpcgrade_info();
int AddEventBintable(FitsFileUnit_t inevent,FitsFileUnit_t ounit);
int GetGrades(long);
int xrtpcgrade_checkinput(void);
int ReadSplitValues(char *obsmode); 

#endif
