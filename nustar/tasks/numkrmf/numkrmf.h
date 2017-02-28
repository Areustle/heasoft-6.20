/*
 *	numkrmf.h: definitions and declarations for numkrmf
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NUMKRMF_H
#define NUMKRMF_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h> /* for getpid */

#include <sys/stat.h>   /* needed by 'mkdir' */
#include <sys/types.h>  /* needed by 'mkdir' */


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
#define PAR_USRGTIFILE		 "usrgtifile"
#define PAR_SRCREGIONFILE	 "srcregionfile"
#define PAR_OUTFILE		 "outfile"
#define PAR_GRPRMFFILE		 "grprmffile"
#define PAR_CMPRMF		 "cmprmf"
#define PAR_RMFDIR		 "rmfdir"

/* miscellaneous */
#define BINTAB_ROWS       1000


typedef struct {
  unsigned TIME;
  unsigned RAWX;
  unsigned RAWY;
  unsigned DET_ID;
} EvtCol_t;

typedef struct {
  char   instrume[FLEN_VALUE];
  char   depthcut[FLEN_VALUE];
  SPTYPE dateobs;
  SPTYPE timeobs;
  SPTYPE dateend;
  SPTYPE timeend;
} ObsInfo_t;


typedef struct {
  unsigned RAWX;
  unsigned RAWY;
  unsigned XEXTENT;
  unsigned YEXTENT;
  unsigned RMFFILE;
} GrpRmfCol_t;

typedef struct {
  BTYPE  rawx;
  BTYPE  rawy;
  BTYPE  xextent;
  BTYPE  yextent;
  char   *rmffile;
  int    evtcnt;
} GrpRmfRow_t;

typedef struct {
  int          rmffile_dim;
  int          nrows;
  GrpRmfRow_t  *row;
} GrpRmfInfo_t;


typedef struct {
  char        xsel_in[PIL_LINESIZE];
  char        xsel_log[PIL_LINESIZE];
  char        rmflistfile[PIL_LINESIZE];
  char        filtered_evt[PIL_LINESIZE];
  char        dirname[PIL_LINESIZE];
  char        xsel_infile[PIL_LINESIZE];
  char        xsel_usrgtifile[PIL_LINESIZE];
  char        xsel_srcregionfile[PIL_LINESIZE];
  char        addrmf_outfile[PIL_LINESIZE];
  } TmpFiles_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        infile[PIL_LINESIZE];   
    char        usrgtifile[PIL_LINESIZE];  
    char        srcregionfile[PIL_LINESIZE];      
    char        outfile[PIL_LINESIZE];
    char        grprmffile[PIL_LINESIZE];
    char        rmfdir[PIL_LINESIZE];
    BOOL        cmprmf;
  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];
  ObsInfo_t  obsinfo;
  TmpFiles_t tmpout;

} Global_t;

extern Global_t global;


/* Prototypes */
int numkrmf();
int numkrmf_work(void);
int numkrmf_getpar(void);
void numkrmf_info(void);
int numkrmf_checkinput(void);

int GetObsInfo(char *filename, char *extname, ObsInfo_t *obsinfo);

int CreateXselXco(char *evtfile, char *usrgtifile, char *regfile, char *evtfiltered, char *xcofile);

int ReadGrpRmfFile(GrpRmfInfo_t grprmf[4], const ObsInfo_t *evt);
int ReadGrpRmfInfo(GrpRmfInfo_t *info, char *filename, long extno, char *detnam);

int SetGrpRmfEvtCnt(char *filename, GrpRmfInfo_t grprmf[4], int *totevt);
void UpdateEvtCnt(int rawx, int rawy, GrpRmfInfo_t *grprmf);

int WriteRmfListFile(GrpRmfInfo_t grprmf[4], int totevt, char *outfile);

BOOL CheckSingleRmf(GrpRmfInfo_t grprmf[4], char *rmffile);

int UpdateRmfKeys(char *filename);

#endif
