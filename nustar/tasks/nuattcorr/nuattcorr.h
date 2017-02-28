/*
 *	nuattcorr.h: definitions and declarations for nuattcorr
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NUATTCORR_H
#define NUATTCORR_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>    /* for getpid */

#include <sys/stat.h>   /* needed by 'mkdir' */
#include <sys/types.h>  /* needed by 'mkdir' */

/* headas headers */
#include <fitsio.h>    /*Required to use c_fcerr, c_fcecho, etc. */
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
  Macros to put in nu_highfits.h file
********************************/

#define VECVECACCESS_PT(type, a, b, c)	\
                        ((type *)(a).cols[c]) + (b*(a).Multiplicity[c])

#define VECVEC_ARRAY_READ(type, var, dim, a, b, c) \
                          memcpy(var,VECVECACCESS_PT(type,a,b,c), sizeof(type)*dim)
#define VECVEC_ARRAY_WRITE(type, var, dim, a, b, c) \
                          memcpy(VECVECACCESS_PT(type,a,b,c), var, sizeof(type)*dim)

#define DVECVEC_ARRAY_READ(var, dim, a, b, c) VECVEC_ARRAY_READ(DTYPE, var, dim, a, b, c)
#define DVECVEC_ARRAY_WRITE(var, dim, a, b, c) VECVEC_ARRAY_WRITE(DTYPE, var, dim, a, b, c)


/*******************************
*      defines / typedefs      *
********************************/
/* input parameter names */

#define PAR_ATTFILE		 "attfile"
#define PAR_OUTATTFILE		 "outattfile"
#define PAR_CHUOFFSETFILE	 "chuoffsetfile"


/* miscellaneous */
#define BINTAB_ROWS       1000


typedef struct {
  LTYPE  nuattcor;
  SPTYPE dateobs;
  SPTYPE timeobs;
  SPTYPE dateend;
  SPTYPE timeend;
  
} AttitudeInfo_t;


typedef struct {
  unsigned Q_CHU4SC;
} ChuOffsetCol_t;

typedef struct {
  double q_chu4sc[4];
} ChuOffsetInfo_t;


typedef struct {
  char        dirname[PIL_LINESIZE];
  char        loc_attfile[PIL_LINESIZE];
  char        loc_outattfile[PIL_LINESIZE];
  char        loc_colfile[PIL_LINESIZE];
} TmpFiles_t;


typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        attfile[PIL_LINESIZE];         /* Name of input Attitude FITS file  */
    char        outattfile[PIL_LINESIZE];      /* Name of output Corrected Attitude FITS file */
    char        chuoffsetfile[PIL_LINESIZE];   /* Name of the input CHUs Quaternion Offset FITS file */
  } par;


/*----------------------------------------------------------------*/
  char       taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t  nustardas_v;          /* NuSTARDAS version */
  BOOL       hist;
  BOOL       warning;
  char       strhist[256];
  char       date[25];
  TmpFiles_t tmp;

  AttitudeInfo_t   attinfo;
  ChuOffsetInfo_t  chuoffsetinfo;

} Global_t;

extern Global_t global;


/* Prototypes */
int nuattcorr();
int nuattcorr_work(void);
int nuattcorr_getpar(void);
void nuattcorr_info(void);
int nuattcorr_checkinput(void);

int GetAttitudeInfo(char *filename, char *extname, AttitudeInfo_t *attinfo);
int GetChuOffsetInfo(char *filename, ChuOffsetInfo_t *info);

int CreateColFile(char *filename, ChuOffsetInfo_t *chuoffsetinfo);

int UpdateKeywords(char *filename, AttitudeInfo_t *info);

#endif
