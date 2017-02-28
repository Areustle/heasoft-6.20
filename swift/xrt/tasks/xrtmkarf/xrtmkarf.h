/*************************************************************************
 *	xrtmkarf.h: definitions and declarations with global relevance
 *
 *	AUTHOR:
 *
 *         Italian Swift Archive Center (FRASCATI)
 *     
 *************************************************************************/
#ifndef XRTMKARF_H
#define XRTMKARF_H
/******************************
 *        header files        *
 ******************************/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h> /* for getpid */

/* XRT local headers */
#include <xrtdefs.h>
#include <xrt_termio.h>
#include <xrtdasversion.h>
#include <highfits.h>
#include <xrtcaldb.h>
#include <xrttimefunc.h>

/* headas headers */
#include <fitsio.h>
#include <pil.h>
#include <coordfits.h>


#ifndef errno
extern int errno;
#endif


/********************************
 *         definitions          *
 ********************************/



/* input parameter names */
#define PAR_OUTFILE       "outfile"
#define PAR_RMFFILE       "rmffile"
#define PAR_PHAFILE       "phafile"
#define PAR_MIRFILE       "mirfile"
#define PAR_INARFFILE     "inarffile"
#define PAR_EXPOFILE      "expofile"
#define PAR_TRANSMFILE    "transmfile"
#define PAR_VIGFILE       "vigfile"
#define PAR_PSFFILE       "psffile"
#define PAR_PSFFLAG       "psfflag"
#define PAR_CNTRDETX      "cntrdetx"
#define PAR_CNTRDETY      "cntrdety"
#define PAR_SRCDETX	  "srcx"
#define PAR_SRCDETY       "srcy"
#define PAR_OFFAXIS       "offaxis"
#define PAR_BOXSIZE       "boxsize"
#define PAR_EXTENDED      "extended"
#define PAR_CLEANUP       "cleanup"


#define CDETX              320.0 
#define CDETY              300.0
#define CSKYX              500.0 
#define CSKYY              500.0
#define SX_MAX             1000 
#define SY_MAX             1000


#define PXL_S              2.36

#define KWVL_DSTYP_GRADE   "GRADE"

#define KWVL_EXTNAME_SPECRESP "SPECRESP" 
#define KWVL_SPECRESP_DSET    "SPECRESP" 
/********************************/
/*           typedefs           */
/********************************/

typedef struct {
  SPTYPE dateobs;
  SPTYPE timeobs;
  char grade[20];
  int xrtvsub;
  double ranom;
  double decnom;
  double crval1;
  double crval2;

} PhaInfo_t;

typedef struct {
  float par0;
  float par1;
  float par2;
} VigInfo_t;


typedef struct {
  int    x_min;
  int    x_max;
  int    y_min;
  int    y_max;
  float  skycx;
  float  skycy;

  float  expocorr;
  float  countsbox;

} WmapBoxInfo_t;

typedef struct {
  int   x_width;
  int   y_width;
  int   x_offset;
  int   y_offset;
  int   nbox;
  WmapBoxInfo_t* box;

} WmapBox_t;


typedef struct {
  double skyx;
  double skyy;

} OptAxisXY_t;


typedef struct {

  struct {
    char   outfile[PIL_LINESIZE]; 
    char   rmf_file[PIL_LINESIZE];
    char   pha_file[PIL_LINESIZE];
    char   inarf_file[PIL_LINESIZE];
    char   expo_file[PIL_LINESIZE];
    char   mir_file[PIL_LINESIZE];
    char   flt_file[PIL_LINESIZE];
    char   vig_file[PIL_LINESIZE];
    char   psf_file[PIL_LINESIZE];
    int    psf_flag; 
    double srcdetx;
    double srcdety;
    double cntr_detx;
    double cntr_dety;
    double offaxis;
    int    boxsize;
    int    extended;
    int    cleanup;
 } par;
  char taskname[FLEN_FILENAME];
  BOOL hist;
  char grade[80];
  Version_t swxrtdas_v;
  char      datamode[20];
  float     srcx;
  float     srcy;
  PhaInfo_t pha;
  OptAxisXY_t optaxis;
} Global_t;

extern Global_t global;


/********************************/
/*          prototypes          */
/********************************/

int   xrtmkarf( void );
int   xrtmkarf_work( void );
int   xrtmkarf_getpar( void );
int   read_rmf(long*,float**,float**);
int   read_mir(long*,float*,float*);
int   read_inarf(long*,float*,float*);
int   read_flt(long*,float*,float*);
int   read_pha_keywords();
int   read_pha(float*,float*,float**,int*,int*,WmapBox_t*);
int   off_calc(float*,float*,float*);
int   opt2sky_calc();
int   vig_corr(float*,float*,long*,float*,VigInfo_t*);
int   psf_corr(float*,long*,float*,int*,float*,float*); 
int   write_arf(int, int, long,float*,float*,float*,char*);
int   update_arf_keywords(char*);
void  printerror(int status);
int   ComputeWmapBox(WmapBox_t * wmapbox);
int   ReadVigInfo(VigInfo_t*);
void  disp_info(void);

#endif
