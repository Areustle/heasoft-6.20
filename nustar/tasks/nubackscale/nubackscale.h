/*
 *	nubackscale.h: definitions and declarations for nubackscale
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NUBACKSCALE_H
#define NUBACKSCALE_H

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

#define PAR_SRCPHAFILE		 "srcphafile"
#define PAR_BKGPHAFILE		 "bkgphafile"
#define PAR_SRCOUTFILE		 "srcoutfile"
#define PAR_BKGOUTFILE		 "bkgoutfile"
#define PAR_SRCCORRECT		 "srccorrect"
#define PAR_BKGCORRECT		 "bkgcorrect"

#define PAR_EVTFILE		 "evtfile"
#define PAR_PIXPOSFILE		 "pixposfile"
#define PAR_ALIGNFILE   	 "alignfile"
#define PAR_MASTASPECTFILE   	 "mastaspectfile"
#define PAR_ATTFILE		 "attfile"
#define PAR_TELDEF		 "teldef"
#define PAR_INSTRPROBMAPFILE	 "instrprobmapfile"
/* #define PAR_USRGTIFILE  	 "usrgtifile" */
#define PAR_INEXPOMAPFILE	 "inexpomapfile"
#define PAR_ABERRATION		 "aberration"
#define PAR_DET1REFFILE		 "det1reffile"
#define PAR_PIXBIN		 "pixbin"
#define PAR_PERCENT		 "percent"
#define PAR_INITSEED     	 "initseed"


/* miscellaneous */
#define BINTAB_ROWS       1000

#define KWNM_CRPIX1P       "CRPIX1P"
#define KWNM_CRVAL1P       "CRVAL1P"
#define KWNM_CRPIX2P       "CRPIX2P"
#define KWNM_CRVAL2P       "CRVAL2P"
#define KWNM_NUBACKSC      "NUBACKSC"
#define CARD_COMM_NUBACKSC "Has the BACKSCAL keyword been corrected on-ground (T/F)?"


typedef struct {
  int     xwidth;
  int     ywidth;
  int     x_offset;
  int     y_offset;
  double  crpix1p;
  double  crval1p;
  double  crpix2p;
  double  crval2p;
  double  exposure;
  double  backscal;
  BOOL    nubacksc;
} PhaImgInfo_t;


typedef struct {
  double ontime;
  double livetime;
  double deadc;
  int    xwidth;
  int    ywidth;
  int    crpix1p;
  int    crpix2p;
  int    crval1p;
  int    crval2p;
} ExposureInfo_t;

typedef struct {
/*   double exposure; */
/*   double livetime; */
  double backscal;
} PhaKeysUpdate_t;


typedef struct {
  char        dirname[PIL_LINESIZE];
  char        loc_srcoutfile[PIL_LINESIZE];
  char        loc_bkgoutfile[PIL_LINESIZE];
  char        expo_infile[PIL_LINESIZE];
  char        loc_expomapfile[PIL_LINESIZE];
  char        xsel_in[PIL_LINESIZE];
  char        xsel_infile[PIL_LINESIZE];
  char        xsel_gtifile[PIL_LINESIZE];
  char        filtered_evt[PIL_LINESIZE];
} TmpFiles_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        srcphafile[PIL_LINESIZE];
    char        bkgphafile[PIL_LINESIZE];
    char        srcoutfile[PIL_LINESIZE];
    char        bkgoutfile[PIL_LINESIZE];
    char        evtfile[PIL_LINESIZE];
    char        pixposfile[PIL_LINESIZE];
    char        alignfile[PIL_LINESIZE];
    char        mastaspectfile[PIL_LINESIZE];
    char        attfile[PIL_LINESIZE];
    char        teldef[PIL_LINESIZE];
    char        instrprobmapfile[PIL_LINESIZE];
    char        inexpomapfile[PIL_LINESIZE];
    char        usrgtifile[PIL_LINESIZE];
    char        det1reffile[PIL_LINESIZE];
    BOOL        srccorrect;
    BOOL        bkgcorrect;
    BOOL        aberration;
    int         pixbin;
    double      percent;
    BOOL        initseed;

  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];
  BOOL getexpomapfile;
  TmpFiles_t tmp;

} Global_t;


extern Global_t global;


/* Prototypes */
int nubackscale();
int nubackscale_work(void);
int nubackscale_getpar(void);
void nubackscale_info(void);
int nubackscale_checkinput(void);

int ReadPhaImg(char *filename, float **img, PhaImgInfo_t *imginfo);
int ReadExpoFile(char *filename, float **expomap, ExposureInfo_t *info);

int ComputeExpoMean(float *pha, PhaImgInfo_t *phainfo, float *expo, ExposureInfo_t *expoinfo, double *expomean);

int UpdatePhaKeys(char *filename, PhaKeysUpdate_t *phakeys);

int CreateXselXco(char *evtfile, char *gtifile, char *evtfiltered, char *xcofile);

int CorrectBackSpectrum(char *phafile, char *evtfile);

#endif
