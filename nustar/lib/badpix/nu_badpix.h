/*
 *	nu_badpix.h:
 *
 *	DESCRIPTION:
 *
 *        This module provides a collection of definitions and macros
 *        useful for Bad Pixels Files handle. 
 *
 *      CHANGE HISTORY:
 *	 
 * 
 *	AUTHOR:
 *
 *        ASDC - ASI Science Data Center
 */


#ifndef NU_BADPIX_H
#define NU_BADPIX_H


	/********************************/
	/*        header files          */
	/********************************/


/* headas header */
#include <pil.h>
#include <hdcal.h>
#include <headas_stdio.h>

/* highfits header */
#include "nu_basic.h"
#include "nu_defs.h"

/* misc header */
#include "nu_termio.h"




	/********************************/
	/*      defines / typedefs      */
	/********************************/


#define BP_BINTAB_ROWS     1000
#define BP_TIME_SENS       0.0000001


/* Bad pixels types */
#define UBP           0 /* Bad pixel user defined */
#define CBP           1 /* Bad pixel of CALDB */
#define DBP           2 /* On-board disabled pixel */


/* Bad pixels flag ( BADFLAG column ) */
#define CALDB_BP         1     /* Bad pixel from on-ground CALDB Bad Pixel File  */
#define ONBOARD_BP       2     /* Disabled pixel from on-board software  */
#define USER_BP          4     /* Bad pixels in the file provided by the user  */
#define OBS_HOTFLICK_BP  32    /* Hot/flickering pixel */
#define ALL_BP           65535 /* All pixxel */

/* Events Status ( STATUS column ) */
#define GOOD             0                 /* Good event */
#define EV_CAL_BP        CALDB_BP          /* Event falls in a bad pixel from on-ground CALDB Bad Pixel File */
#define EV_ONBOARD_BP    ONBOARD_BP        /* Event falls in a on-board disabled pixel */
#define EV_USER_BP       USER_BP           /* Event falls in a user bad pixel */
#define EV_NEIGH_BP      8                 /* Event has a neighbor bad from bad/disabled pixels list */
#define EV_EDGE_BP       16                /* Event falls in a pixel on a detector edge */
#define EV_HOTFLICK_BP   OBS_HOTFLICK_BP   /* Event falls in a hot/flickering pixel */
#define EV_NEIGH_HF      64                /* Event has a neighbor hot/flickering pixel */
#define EV_DEPTHCUT      128               /* Event fails depth cut */
#define EV_BASELINECUT   256               /* Event fails baseline cut */
#define EV_PRIORRESET    512               /* Event fails prior/reset cut */
#define EV_PRIORCUT      1024              /* Event fails prior cut */
#define EV_RESETCUT      2048              /* Event fails reset cut */
#define EV_OUTRANGE_PI   4096              /* Event with PI out of range */


/* User and CALDB bad pixel data */
typedef struct {
  double    time;
  short int badflag;
 } BadPixData_t ;


typedef struct {
  char   instrume[FLEN_VALUE];
  char   obs_id[FLEN_VALUE];
  char   origin[FLEN_VALUE];
  DTYPE  tstart;
  DTYPE  tstop;
  JTYPE  mjdrefi;
  DTYPE  mjdreff;
} BadPixKeys_t ;


typedef struct {
  unsigned TIME;
  unsigned RAWX;
  unsigned RAWY;
  unsigned BADFLAG;
} BadPixCol_t;


/* On-board disabled pixel data */
typedef struct {
  double    time;
  double    time_stop;
 } DisPixInfo_t ;

typedef struct {
  DisPixInfo_t *info;
  int          ninfo;
 } DisPixData_t ;

typedef struct {
  unsigned TIME;
  unsigned TIME_STOP;
  unsigned RAWX;
  unsigned RAWY;
} DisPixCol_t;


/* Bad pixels data in BADPIX extension */
typedef struct {
  double    time;
  double    time_stop;
  short int badflag;
 } BadPixExtInfo_t ;

typedef struct {
  BadPixExtInfo_t *info;
  int             ninfo;
 } BadPixExtData_t ;

typedef struct {
  unsigned TIME;
  unsigned TIME_STOP;
  unsigned RAWX;
  unsigned RAWY;
  unsigned BADFLAG;
} BadPixExtCol_t;



	/********************************/
	/*          prototypes          */
	/********************************/


int ReadDisPixFile(DisPixData_t bpdata[DET_PIXS][DET_ROWS],char *filename, long extno, char *detnam, char* instr, double tstart, double tstop);
int ReadBadPixFile(BadPixData_t bpdata[DET_PIXS][DET_ROWS],char *filename, long extno, char *detnam, char* instr, double tstop, short int inbadflag);
int ReadEvtBadPixExt(BadPixExtData_t bpdata[DET_PIXS][DET_ROWS],char *filename, char *detnam, short int nobadflag, BOOL *extfound, int *extnum);

int UpdateBPExtWithBadPixData(BadPixData_t inbp[DET_PIXS][DET_ROWS], BadPixExtData_t outbp[DET_PIXS][DET_ROWS], double tstop);
int UpdateBPExtWithDisPixData(DisPixData_t inbp[DET_PIXS][DET_ROWS], BadPixExtData_t outbp[DET_PIXS][DET_ROWS]);
int UpdateBPExtWithBadPixExtData(BadPixExtData_t inbp[DET_PIXS][DET_ROWS], BadPixExtData_t outbp[DET_PIXS][DET_ROWS]);

int CreateBPExt(FitsFileUnit_t ounit, BadPixExtData_t outbp[DET_PIXS][DET_ROWS], int extver, char *detnam, BadPixKeys_t *bpkeys, unsigned short int nobadflag);
int CreateBPFile(BadPixExtData_t bp[4][DET_PIXS][DET_ROWS], char *outbpfile, BadPixKeys_t *bpkeys, char *infile,short int nobadflag);

void SortBadPixExtData(BadPixExtData_t bp[DET_PIXS][DET_ROWS]);

int WriteStatus(FitsFileUnit_t ounit);


#endif
