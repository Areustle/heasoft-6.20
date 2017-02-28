/*
 *	xrthkproc.h: definitions and declarations for xrthkproc
 *
 *	AUTHOR:
 *            Italian Swift Archive Center (FRASCATI)
 *  
 *     
 */
#ifndef XRTHKPROC_H
#define XRTHKPROC_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <unistd.h> /* for getpid */


/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>
#include <headas_gti.h> 
#include <coordfits.h>
#include <teldef.h>
#include <ephemeris.h>


/* xrt local headers */
#include <highfits.h>
#include <xrt_termio.h>
#include <xrtfits.h>
#include <xrtdefs.h>
#include <xrtcaldb.h>
#include <misc.h>
#include <xrtdasversion.h>
#include <xrttimefunc.h>




#ifndef errno
extern int errno;
#endif

/*******************************
*      defines / typedefs      *
********************************/
/* input parameter names */

#define PAR_HDFILE		   "hdfile"
#define PAR_OUTFILE		   "outfile"
#define PAR_ATTFILE		   "attfile"
#define PAR_TELDEF                 "teldef"
#define PAR_SRCRA                  "srcra"
#define PAR_SRCDEC                 "srcdec"
#define PAR_SRCDETX                "srcdetx"
#define PAR_SRCDETY                "srcdety"
#define PAR_RANOM                  "ranom"
#define PAR_DECNOM                 "decnom"
#define PAR_ATTINTERPOL            "attinterpol"
#define PAR_ABERRATION             "aberration"


/* miscellaneous */
#define BINTAB_ROWS       1000
#define CLNM_HKTIME                "HKTIME"
#define CARD_COMM_HKTIME           "Housekeeping  time column"
#define HKFITSEXT         "tc.hk"   
typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char hdfile[PIL_LINESIZE];     /* Name of FITS file and [ext#] */
    char outfile[PIL_LINESIZE];     /* Name of FITS file and [ext#] */
    char attfile[PIL_LINESIZE]; 
    char teldef[PIL_LINESIZE];
    int    srcdetx;
    int    srcdety;
    double srcra;
    double srcdec;
    double ranom;
    double decnom;
    BOOL attinterpol;
    BOOL aberration;
    BOOL follow_sun;

  } par;
  char tmpfile[FLEN_FILENAME];    
  double   tstop;
  double   tstart;

/*------------------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t swxrtdas_v;          /* SWXRTDAS version */
  double mjdref;
  BOOL hist;
  SPTYPE dateobs;
  SPTYPE timeobs;
  char strhist[256];
  char date[25];
} Global_t;

extern Global_t global;

/* I/O Data structure, photon counting columns from:  
Swift XRT Science FITS Format Design v0.6 June 29,2001) */

typedef struct {
unsigned FrameHID ;			 
unsigned CCDFrame ;			 
unsigned TARGET   ;			 
unsigned ObsNum   ;			 
unsigned RA       ;			 
unsigned Dec      ;			 
unsigned Roll     ;			 
unsigned Settled  ;			 
unsigned In10Arcm ;			 
unsigned InSAA    ;			 
unsigned InSafeM  ;			 
unsigned XRTAuto  ;			 
unsigned XRTManul ;			 
unsigned XRTRed   ;			 
unsigned XRTMode  ;			 
unsigned WaveID   ;			 
unsigned ErpCntRt ;			 
unsigned XPosTAM1 ;			 
unsigned YPosTAM1 ;			 
unsigned XPosTAM2 ;			 
unsigned YPosTAM2 ;			 
unsigned DNCCDTemp;
unsigned Vod1        ;
unsigned Vod2        ;
unsigned Vrd1        ;
unsigned Vrd2        ;
unsigned Vog1        ;
unsigned Vog2        ;
unsigned S1Rp1       ;
unsigned S1Rp2       ;
unsigned S1Rp3       ;
unsigned R1pR        ;
unsigned R2pR        ;
unsigned S2Rp1       ;
unsigned S2Rp2       ;
unsigned S2Rp3       ;
unsigned Vgr         ;
unsigned Vsub        ;
unsigned Vbackjun    ;
unsigned Vid         ;
unsigned Ip1         ;
unsigned Ip2         ;
unsigned Ip3         ;
unsigned Sp1         ;
unsigned Sp2         ;
unsigned Sp3         ;
unsigned CpIG        ;
unsigned BaseLin1    ;
unsigned BaseLin2    ;
unsigned FSTS        ;
unsigned FSTSub      ;
unsigned FETS        ;
unsigned FETSub      ;
unsigned CCDExpo     ;
unsigned CCDExpSb    ;
unsigned EvtLLD      ;
unsigned PixGtLLD    ;
unsigned EvtULD      ;
unsigned PixGtULD    ;
unsigned SplitThr    ;
unsigned OuterThr    ;
unsigned SnglePix    ;
unsigned SngleSpl    ;
unsigned ThreePix    ;
unsigned FourPix     ;
unsigned WinHalfW    ;
unsigned WinHalfH    ;
unsigned Amp         ;
unsigned telemRow    ;
unsigned telemCol    ;
unsigned nPixels     ;
unsigned BaseLine    ;
unsigned PixOverF    ;
unsigned PixUnder    ;
unsigned TIME        ;
unsigned ENDTIME     ;
unsigned BiasExpo    ;
unsigned CCDTemp     ;
unsigned LRHPixCt    ;
unsigned LRBPixCt    ;
unsigned LRSumBPx    ;
unsigned LRSoSBPx    ;
unsigned LRBiasPx    ;
unsigned LRBiasLv    ;
unsigned LRNoise     ;
unsigned EVTLOST     ;
unsigned PDBIASTHR;
unsigned PDBIASLVL;
unsigned LDPNUM   ;
unsigned LDPPAGE  ;			 
unsigned HPixCt   ;			 
unsigned BiasPx   ;			 
unsigned IMBLvl   ;			 
unsigned FCWSCol  ;
unsigned FCWSRow  ;
unsigned FCWNCols ;
unsigned FCWNRows ;
unsigned FCWThres ;
unsigned HKTIME   ;			 

} HKCol_t;

/* Prototypes */

int xrthkproc();
int xrthkproc_work();
int xrthkproc_getpar();
int xrthkproc_checkinput(void);
void xrthkproc_info(void);
int Timetag(FitsFileUnit_t inevent, FitsFileUnit_t ounit);
int ComputeDetCoord(double src_x, double src_y, int amp, double time, TELDEF* teldef, ATTFILE* attfile,  Coord_t * coord);
#endif
