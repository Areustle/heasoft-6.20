/*
 *	nulivetime.h: definitions and declarations for nulivetime
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NULIVETIME_H
#define NULIVETIME_H

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
#include <headas_gti.h>

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
#define PAR_OUTFILE		 "outfile"


/* miscellaneous */
#define BINTAB_ROWS       1000
#define TIME_SENS         0.0000001

typedef struct {
  char   instrume[FLEN_VALUE];
  DTYPE  tstart;
  DTYPE  tstop;
  double ontime;
} ObsInfo_t;

typedef struct {
  double deadc;
  double exposure;
  double livetime;
} ObsInfoUpdate_t;


typedef struct {
  unsigned TIME;
  unsigned LIVETIME;
} HKCol_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];
    char        hkfile[PIL_LINESIZE];
    char        outfile[PIL_LINESIZE];
  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];
  char tmpfile[PIL_LINESIZE];

  ObsInfo_t        obsinfo;
  ObsInfoUpdate_t  obsinfoupdate;

} Global_t;

extern Global_t global;


/* Prototypes */
int nulivetime();
int nulivetime_work(void);
int nulivetime_getpar(void);
void nulivetime_info(void);
int nulivetime_checkinput(void);

int GetObsInfo(char *filename, char *extname, ObsInfo_t *obsinfo);

int ComputeDEADC(char *hkfile, struct gti_struct *gti, double *deadc);

int UpdateEvtTimeKeys(char *filename, ObsInfoUpdate_t *obsupdate);
int AddEvtKeywords(FitsFileUnit_t inunit, ObsInfoUpdate_t *obsupdate);

#endif
