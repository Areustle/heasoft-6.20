/*
 *	xrtbpfunc.h: definitions and declarations with 
 *                              global relevance
 *
 *
 *	AUTHOR:
 *        Italian Swift Archive Center (FRASCATI)
 *     
 */

#ifndef XRTBPFUNC_H
#define XRTBPFUNC_H

#include "xrtfits.h"
#include "xrtcaldb.h"
#include "xrtdasversion.h"
#include <headas_utils.h>

#define BPFITSEXT    "bp.fits"
#define HBPFITSEXT    "hp.fits"

/* Bad pixels types */
#define BAD_PIX  1
#define BAD_COL  2

#define UBP           0 /* Bad pixel user defined */
#define CBP           1 /* Bad pixel of CALDB */
#define OBP           2 /* Bad pixel user defined */


#define HOTPIX        -1
#define FLICKPIX      -2

/* Bad pixels flag */
#define CALDB_BP         1
#define ONBOARD_BP       2
#define OBS_BURNED_BP      4
#define OBS_HOT_BP       8
#define USER_BP         16
#define OBS_FLICK_BP    32
#define OBS_TELEM_BP    64 /* Value used to indicate bad pixels found around 3x3 in PC mode */         
#define BAD_BP          2048

/* Events Status */
#define GOOD             0            /* Good event */
#define EV_CAL_BP        CALDB_BP     /* Event falls in bad pixel from caldb */
#define EV_ONBOARD_BP    ONBOARD_BP   /* Event falls in bad pixel from bias map */
#define EV_BURNED_BP     OBS_BURNED_BP  /* Event falls in burned spot */
#define EV_HOT_BP        OBS_HOT_BP   /* Event falls in hot bad pixel */
#define EV_USER_BP       USER_BP      /* Event falls in user bad pixel */
#define EV_FLICK_BP      OBS_FLICK_BP /* Event falls in flickering bad pixels */ 
#define EV_TELEM_BP      OBS_TELEM_BP /* Event falls in telemetred bad pixel */
#define EV_NEIGH_HF      128          /* Event has a neighbor bad from hot pixels list */
#define BP_COL           256          /* Column */
#define EV_BELOW_TH      512          /* Event has PHA[1] < Event Threshold */
#define EV_NEIGH_BP      1024         /* Event has a neighbor bad from bad pixels list */
#define EV_BAD           BAD_BP       /* Bad event*/
#define EV_CALIB_SRC_1   4096         /* Calibration source A */ 
#define EV_CALIB_SRC_2   8192         /* Calibration source B */ 
#define EV_CALIB_SRC_3   16384        /* Calibration source C */ 
#define EV_CALIB_SRC_4   32768        /* Calibration source D */ 



#define EVENT_BAD_PIX   4095    /* Bad pixels flag in events file */
#define BIAS_BAD_PIX    4095    /* Bad pixels flag in bias map */

typedef struct {
  short int bad_type;
  short int xyextent;
  short int bad_flag;
  short int counter;
 } BadPixel_t ;

/* Each variable stores the number associated to column position in badpixel file */
typedef struct {
  unsigned TIME;   /* new element in struct */
  unsigned RAWX;
  unsigned RAWY;
  unsigned Amp;
  unsigned TYPE;
  unsigned XYEXTENT;
  unsigned BADFLAG;
} BadCol_t;


/********************************/
/*          prototypes          */
/********************************/

int GetBadPix(char *, BadPixel_t *badpix_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *badpix_amp2[CCD_PIXS][CCD_ROWS], int type, long extno, double tstart);
int BadpixBintable(FitsFileUnit_t ounit,BadPixel_t *badpix_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *badpix_amp2[CCD_PIXS][CCD_ROWS], char *extname);
int AddBadPix(int x, int y, BadPixel_t pix, BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS]);
int CreateBPFile(FitsFileUnit_t inevent, BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS], BadPixel_t *badpix_amp2[CCD_PIXS][CCD_ROWS], char *outbpfile);
int WriteBPKeyword(FitsFileUnit_t evunit, FitsFileUnit_t badunit, char *outbpfile);
FitsFileUnit_t CreateFile(FitsFileUnit_t evunit, char *filename);
void CutBPFromTable(BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS], int flag);
int HotBintable(FitsFileUnit_t ounit, int amp, BadPixel_t *bp_table[CCD_PIXS][CCD_ROWS], char *extname);
int CreateHotBPFile(FitsFileUnit_t evunit, int amp,BadPixel_t *bp_table_amp1[CCD_PIXS][CCD_ROWS],char *outbpfile);
int WriteBadFlags(FitsFileUnit_t bpunit);
int CreateBPExt(FitsFileUnit_t ounit,BadPixel_t *bp_table_amp[CCD_PIXS][CCD_ROWS],char *extname, int amp);
int UpdateBPExt(FitsFileUnit_t outunit, FitsFileUnit_t evunit, BadPixel_t *bp_table_amp[CCD_PIXS][CCD_ROWS],char *extname, int amp, int hducount);
#endif
