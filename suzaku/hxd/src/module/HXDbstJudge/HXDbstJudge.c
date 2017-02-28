/**************************************************
HXDbstjudge: bst judgement for the light curve fits

ver 0.0.1 K.Yamaoka  2006 Feb. 12
   first version to make fits light curve
ver 0.0.2 K.Yamaoka  2009 May 6
   bug fix in hxdgrb machine
ver 0.0.3 K.Yamaoka  2009 May 7
   further refinement in bst judgment
ver 0.0.4 K.Yamaoka  2009 May 9
   Disable for judgment during the WAM scan

ver 0.0.5 C.Padgett  2009 Oct 6
   * Fixed std deviation calculation for disabling
     during WAM scan:
       - made it a two pass calculation, since NaN's
         were happening with the one-pass version
   * Added burst duration calculation, and eliminated
     numerous bursts being reported for each burst
       - each burst is now integrated until no longer
         detected.
   * Added clobber parameter
   * Added comment header to output file:
       comment lines start with '#'
   * Use dynamic memory for reading + storing
     light curve
   * Added TIMEZERO and TIMEPIXR to time calculations
   * Included FRACEXP in timing/rate calculations
   * In hxdbstjudge.def, included HXDleapsecInit to
     handle user supplied (or HEASoft/CALDB) leap
     second file. Also added leapfile parameter.
   * Renamed module HXDbstJudge
   * Renamed integ_time parameter to bgd_integ_time
   * Added lots of comments

ver 0.0.6 C.Padgett 2011 Mar. 24
   * Added STEP (occultation) detection method
   * Added HETE2 burst detection algorithm - linear
     fit to background.
   * Added control parameters for fully specifying how
     burst/step detection is done
   * Added option to do FITS output table
   * Added T50/T90 calc.
   * Moved all actual burst calculations to
     functions/hxdBstJudgeUtil/

***************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <cli.h>
#include <com.h>
#include <bnk.h>
#include <evs.h>
#include <anl.h>
#include <atFunctions.h>
#include <aste_time.h>
#include <fitsio.h>
#include <cfortran.h>
#include <hbook.h>
#include "hxd/HXD.h"
#include "hxdFitsHeaderUtil.h"
#include "hxdBstJudgeUtil.h"
#include "pil.h"

#define DEBUG 0

/*
** task name and version
*/
static char pname[ ]        = "HXDbstJudge";
char HXDbstJudge_version[ ] = "version 0.0.6";

/*
** input parameters
*/
static char filename[ FLEN_FILENAME ];
static char newfilename[ FLEN_FILENAME + 1 ];
static char det_alg_str[ PIL_LINESIZE ];
static char trigger_set_str[ PIL_LINESIZE ];

static int use_trigger_set = 0;
static int trigger_set     = 0;
static int clobber         = 0;
static int outtype         = 0;
static int bgd_integ_time  = 0;
static int bgd_early_gap   = 0;
static int bgd_late_gap    = 0;
static int step_window     = 0;
static int durest          = 0;

static double sigma         = 0.0;
static double gaptol        = 0.0;
static double overlaptol    = 0.0;
static double maxdur        = 0.0;
static double judge_time    = 0.0;
static double step_delchi   = 0.0;

static hxdBstJudge_DetAlg det_alg = GINGA;

/*
** HETE-2 trigger sets - based loosely on plots in paper:
**   "The Effectiveness of the HETE-2 Triggering Algorithm"
**   Tevenner, T. et al. 2001
*/
#define TRIGGER_SET_SHORT       0
#define TRIGGER_SET_MEDIUM      1
#define TRIGGER_SET_LONG        2
#define TRIGGER_SET_LENGTH      11

static double trigger_set_bkg1_start[3][TRIGGER_SET_LENGTH] =
    { { -12.0, -14.0, -20.0, -19.0, -19.0, -20.0, -20.0, -20.0, -21.0, -21.0, -21.0 },
      {  -8.0,  -8.0, -15.0, -19.0, -25.0, -35.0, -38.0, -40.0, -45.0, -45.0, -45.0 },
      {  -8.0,  -9.0, -14.0, -20.0, -28.0, -41.0, -43.0, -43.0, -44.0, -44.0, -44.0 } };
static double trigger_set_bkg1_stop[3][TRIGGER_SET_LENGTH] =
    { {  -2.0,  -3.0,  -4.0,  -3.0,  -3.0,  -3.0,  -3.0,  -4.0,  -4.0,  -4.0,  -4.0 },
      {  -2.0,  -2.0,  -6.0,  -6.0,  -7.0,  -9.0,  -7.0,  -9.0, -17.0, -17.0, -17.0 },
      {  -3.0,  -2.0,  -5.0,  -6.0,  -9.0, -15.0, -14.0, -13.0, -15.0, -15.0, -15.0 } };
static double trigger_set_bkg2_start[3][TRIGGER_SET_LENGTH] =
    { {   2.0,   4.0,   4.0,   4.0,   4.0,   5.0,   8.0,  10.0,  13.0,  20.0,  40.0 },
      {   3.0,   4.0,   5.0,   7.0,   8.0,   9.0,  13.0,  16.0,  16.0,  25.0,  50.0 },
      {   2.0,   4.0,   7.0,  11.0,  14.0,  19.0,  22.0,  25.0,  28.0,  40.0,  80.0 } };
static double trigger_set_bkg2_stop[3][TRIGGER_SET_LENGTH] =
    { {   5.0,   7.0,   8.0,   8.0,   9.0,  10.0,  13.0,  15.0,  18.0,  28.0,  50.0 },
      {   6.0,   7.0,   9.0,  11.0,  13.0,  15.0,  20.0,  24.0,  25.0,  40.0,  70.0 },
      {   7.0,   9.0,  17.0,  24.0,  34.0,  46.0,  52.0,  55.0,  58.0,  70.0,  110.0 } };
static double trigger_set_fg_stop[3][TRIGGER_SET_LENGTH] =
    { {   1.0,   1.0,   2.0,   2.0,   3.0,   4.0,   6.0,   8.0,  10.0,  15.0,  25.0 },
      {   1.0,   2.0,   2.0,   3.0,   3.0,   4.0,   8.0,  11.0,  12.0,  20.0,  40.0 },
      {   1.0,   2.0,   2.0,   3.0,   5.0,   5.0,   8.0,  11.0,  14.0,  28.0,  50.0 } };

/*
** format for output FITS table
*/
#define HXDBSTJUDGE_OUTFITS_BST_NUM_COL 30
enum {
    HBJ_OUTCOL_BST_TIMESTART = 1,
    HBJ_OUTCOL_BST_TIMESTOP,
    HBJ_OUTCOL_BST_DATESTART,
    HBJ_OUTCOL_BST_DATESTOP,
    HBJ_OUTCOL_BST_METHOD,
    HBJ_OUTCOL_BST_RATE,
    HBJ_OUTCOL_BST_ERROR,
    HBJ_OUTCOL_BST_RATESNR,
    HBJ_OUTCOL_BST_FORERATE,
    HBJ_OUTCOL_BST_FOREERROR,
    HBJ_OUTCOL_BST_BKGRATE,
    HBJ_OUTCOL_BST_BKGERROR,
    HBJ_OUTCOL_BST_BKGSTART,
    HBJ_OUTCOL_BST_BKGSTOP,
    HBJ_OUTCOL_BST_BKGCOEFFS,
    HBJ_OUTCOL_BST_CHANBIN,
    HBJ_OUTCOL_BST_CHANMIN,
    HBJ_OUTCOL_BST_CHANMAX,
    HBJ_OUTCOL_BST_T50,
    HBJ_OUTCOL_BST_T50ERR,
    HBJ_OUTCOL_BST_T50START,
    HBJ_OUTCOL_BST_T50STOP,
    HBJ_OUTCOL_BST_T90,
    HBJ_OUTCOL_BST_T90ERR,
    HBJ_OUTCOL_BST_T90START,
    HBJ_OUTCOL_BST_T90STOP,
    HBJ_OUTCOL_BST_DATAMODE,
    HBJ_OUTCOL_BST_DETNAM,
    HBJ_OUTCOL_BST_LC_FILE,
    HBJ_OUTCOL_BST_TLM_FILE
};

#define HXDBSTJUDGE_OUTFITS_STP_NUM_COL 19
enum {
    HBJ_OUTCOL_STP_TIMESTART = 1,
    HBJ_OUTCOL_STP_TIMESTOP,
    HBJ_OUTCOL_STP_DATESTART,
    HBJ_OUTCOL_STP_DATESTOP,
    HBJ_OUTCOL_STP_METHOD,
    HBJ_OUTCOL_STP_STEPTIME,
    HBJ_OUTCOL_STP_STEPWIDTH,
    HBJ_OUTCOL_STP_STEPHEIGHT,
    HBJ_OUTCOL_STP_STEPSNR,
    HBJ_OUTCOL_STP_STEPCOEFFS,
    HBJ_OUTCOL_STP_BKGSTART,
    HBJ_OUTCOL_STP_BKGSTOP,
    HBJ_OUTCOL_STP_CHANBIN,
    HBJ_OUTCOL_STP_CHANMIN,
    HBJ_OUTCOL_STP_CHANMAX,
    HBJ_OUTCOL_STP_DATAMODE,
    HBJ_OUTCOL_STP_DETNAM,
    HBJ_OUTCOL_STP_LC_FILE,
    HBJ_OUTCOL_STP_TLM_FILE
};

#define HXDBSTJUDGE_OUTFITS_NUM_COMM 40
static char* comm[HXDBSTJUDGE_OUTFITS_NUM_COMM] = {
    " ",
    "This file contains special events detected in WAM lightcurves obtained",
    "from the BST and/or TRN data modes. The special events are either burst",
    "type events (e.g. GRBs) or step events due to Earth occultation of",
    "bright sources.",
    " ",
    "The columns in this file are as follows:",
    " ",
    "TIMESTART - Start time of the event in MET [s]. It is calculated as the",
    "            start of the first bin where the signal to noise ratio (SNR)",
    "            is higher than the input threshold.",
    "TIMESTOP  - Stop time of the event in MET [s]. It is calculated by one",
    "            of two methods:",
    "             a) A fixed offset from TIMESTART (METHOD='GINGA' or",
    "                METHOD='STEP').",
    "             b) The time when the SNR no longer increases when more",
    "                rate bins are added (METHOD='HETE2').",
    "DATESTART - Start date of the event in UTC.",
    "DATESTOP  - Stop date of the event in UTC.",
    "METHOD    - Indicates the method used to detect bursts and calculate",
    "            TIMESTOP. It can be one of 'HETE2', 'GINGA', or 'STEP'.",
    "DATAMODE  - Data mode for the light curve in which the event occurs,",
    "            either 'TRANSIENT' for TRN mode or 'BURST' for BST mode.",
    "DETNAM    - Name of the WAM unit for the light curve in which the",
    "            event occurs - 'WAM_ANTI0', 'WAM_ANTI1', 'WAM_ANTI2' or",
    "            'WAM_ANTI3'.",
    "LC_FILE   - Name of the light curve file containing this event.",
    "TLM_FILE  - Name of the RPT file that contains this event.",
    "CHANBIN   - Channel binning type used in CHANMIN and CHANMAX columns.",
    "            Can be either 'TH', 'PH' or 'UN' (unknown). The standard",
    "            values for channels are:",
    "              TH 0 = PH 2-3   = 50-110 keV",
    "              TH 1 = PH 4-7   = 110-240 keV",
    "              TH 2 = PH 8-16  = 240-520 keV",
    "              TH 3 = PH 17-54 = 520-5000 keV",
    "            where TH(0,1,2,3) are the BST mode channels. The TRN mode",
    "            has 54 channels that are typically grouped to match the BST",
    "            channels.",
    "CHANMIN   - Minimum channel number used to derive the input light curve.",
    "CHANMAX   - Maximum channel number used to derive the input light curve."
};

#define HXDBSTJUDGE_OUTFITS_BST_NUM_COMM 30
static char* comm_bst[HXDBSTJUDGE_OUTFITS_BST_NUM_COMM] = {
    "RATE      - Average background subtrated rate [c/s] calculated over the",
    "            interval that starts with TIMSTART and ends with TIMESTOP.",
    "ERROR     - Error in the RATE column [c/s].",
    "RATESNR   - Signal to noise ratio of the event. It is equivalent to the",
    "            ratio of the RATE column to the ERROR column.",
    "FORERATE  - Event average rate [c/s] not background subtracted. This is",
    "            the average light curve rate between TIMESTART and TIMESTOP.",
    "FOREERROR - Error [c/s] in the FORERATE column.",
    "BKGRATE   - Average background rate [c/s]. This is calculated by one of",
    "            two methods:",
    "             a) The mean count rate in the fixed interval between",
    "                BKGSTART and BKGSTOP (METHOD='GINGA' or METHOD='STEP').",
    "             b) The result of interpolating a linear fit of the count",
    "                rate in the intervals BKGSTART(1) to BKGSTOP(1) and",
    "                BKGSTART(2) to BKGSTOP(2) onto the time interval between",
    "                TIMESTART and TIMESTOP (METHOD='HETE2').",
    "BKGERROR  - Error [c/s] in the BKGRATE column.",
    "BKGSTART  - Start time(s) of the background interval(s) in MET [s].",
    "BKGSTOP   - Stop time(s) of the background interval(s) in MET [s].",
    "T50       - Standard 50 percent burst duration measure calculated",
    "            between TIMESTART and TIMESTOP.",
    "T50ERR    - Error estimate for T50.",
    "T50START  - Start time of the T50 interval in MET [s].",
    "T50STOP   - Stop time of the T50 interval in MET [s].",
    "T90       - Standard 90 percent burst duration measure calculated",
    "            between TIMESTART and TIMESTOP.",
    "T90ERR    - Error estimate for T90.",
    "T90START  - Start time of the T90 interval in MET [s].",
    "T90STOP   - Stop time of the T90 interval in MET [s].",
    " "
};

#define HXDBSTJUDGE_OUTFITS_STP_NUM_COMM 11
static char* comm_stp[HXDBSTJUDGE_OUTFITS_STP_NUM_COMM] = {
    "STEPTIME  - Midpoint of fitted step in MET [s]",
    "STEPWIDTH - Step width, calculated as:",
    "              atanh( 0.99 ) / STPCOEFFS[5]",
    "STEPHEIGHT- Step height in count/s",
    "STEPSIGMA - Significance of step. Calculated as:",
    "              STPHEIGHT / 90% confidence interval",
    "STEPCOEFFS- Fitted coefficients for the template step function as:",
    "              A[1] + A[2]*(TIME-A[3]) + A[4]*tanh((TIME-A[3])*A[5])",
    "BKGSTART  - Start times of the background intervals in MET [s].",
    "BKGSTOP   - Stop times of the background intervals in MET [s].",
    " "
};


static char *ttype_bst[] = {
    "TIMESTART", "TIMESTOP", "DATESTART", "DATESTOP", "METHOD", "RATE",
    "ERROR", "RATESNR", "FORERATE", "FOREERROR", "BKGRATE", "BKGERROR",
    "BKGSTART", "BKGSTOP", "BKGCOEFFS", "CHANBIN", "CHANMIN", "CHANMAX",
    "T50", "T50ERR", "T50START", "T50STOP", "T90", "T90ERR", "T90START",
    "T90STOP", "DATAMODE", "DETNAM", "LC_FILE", "TLM_FILE"
};

static char *tform_bst[] = {
    "1D", "1D", "23A", "23A", "5A", "1D", "1D", "1D", "1D", "1D", "1D", "1D",
    "2D", "2D", "2D", "2A", "1J", "1J", "1D", "1D", "1D", "1D", "1D", "1D",
    "1D", "1D", "9A", "9A", "50A", "30A"
};

static char *tunit_bst[] = {
    "s", "s", "UTC", "UTC", "", "count/s", "count/s", "", "count/s", "count/s",
    "count/s", "count/s", "s", "s", "", "", "channel", "channel", "s", "s",
    "s", "s", "s", "s", "s", "s", "", "", "", ""
};

static char *ttype_stp[] = {
    "TIMESTART", "TIMESTOP", "DATESTART", "DATESTOP", "METHOD", "STEPTIME",
    "STEPWIDTH", "STEPHEIGHT", "STEPSIGMA", "STEPCOEFFS", "BKGSTART", "BKGSTOP",
    "CHANBIN", "CHANMIN", "CHANMAX", "DATAMODE", "DETNAM", "LC_FILE",
    "TLM_FILE"
};

static char *tform_stp[] = {
    "1D", "1D", "23A", "23A", "5A", "1D", "1D", "1D", "1D", "5D", "2D", "2D",
    "2A", "1J", "1J", "9A", "9A", "50A", "30A"
};

static char *tunit_stp[] = {
    "s", "s", "UTC", "UTC", "", "s", "s", "count/s", "", "", "s", "s", "",
    "channel", "channel", "", "", "", ""
};

static int HXDbstJudge_writeHistory ( fitsfile* fp ) {

    int istat = 0;
    int status = ANL_OK;
    char history [PIL_LINESIZE];
    return istat;

    /*sprintf( history, "    ------- parameters in %s -------   ", pname );
    if ( fits_write_history( fp, history, &istat ) ) {
        fprintf( stderr, "fits_write_history failed (%d)\n", istat );
        status = ANL_NG; return status;
    }

    sprintf(history, "         outroot = %s", com.newfilename);
    fits_write_history(fp, history, &istat);
    if(istat) {
        fprintf(stderr, "fits_write_history failed (%d)\n", istat);
        status = ANL_NG; return status;
    }

    if(trnfits->tpu_board==-1){
        sprintf(history, "         tpu_board = all (0,1,2,3)");
    } else {
        sprintf(history, "         tpu_board = %d", trnfits->tpu_board);
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
        fprintf(stderr, "fits_write_history failed (%d)\n", istat);
        status = ANL_NG; return status;
    }

    if (trnfits->ph_mode == 0){
        sprintf(history, "         ph_mode = others");
    } else if (trnfits->ph_mode == 1){
        sprintf(history, "         ph_mode = PH");
    } else {
        sprintf(history, "         ph_mode = unknown");
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
        fprintf(stderr, "fits_write_history failed (%d)\n", istat);
        status = ANL_NG; return status;
    }

    sprintf(history, "         channel = (%d ch -- %d ch)",
            trnfits->chmin, trnfits->chmax);
    fits_write_history(fp, history, &istat);
    if(istat) {
        fprintf(stderr, "fits_write_history failed (%d)\n", istat);
        status = ANL_NG; return status;
    }

    if(trnfits->dtcor){
        sprintf(history, "         dt_cor = Perform dead time correction");
    } else {
        sprintf(history, "         dt_cor = No dead time correction");
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
        fprintf(stderr, "fits_write_history failed (%d)\n", istat);
        status = ANL_NG; return status;
    }

    if(trnfits->dtcor){
        sprintf(history, "         dt_clk = %f (sec) dead time clock", trnfits->dt_clk);
        fits_write_history(fp, history, &istat);
        if(istat) {
            fprintf(stderr, "fits_write_history failed (%d)\n", istat);
            status = ANL_NG; return status;
        }
    }

    return status;*/
}

static int write_fits_row_bst( fitsfile* fp, int i, hxdBstJudge_Trigger* trig,
                               hxdBstJudge_LC* lc, HXD_STD_KEYS stdkeys,
                               long chanmin, long chanmax, char* chanbin,
                               char* infilep, int* istat ) {
    char datestr[24];
    char* charp;

    double ddum;

    AtTimeD burstStart;
    AtTimeD burstStop;



    charp = datestr;

    /* write start/stop in AETIME and date start/stop in UTC */
    ddum = trig->fstart - lc->timedel / 2.0;
    aste2attimeD( ddum, &burstStart );
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_TIMESTART, i, 1, 1,
                             &ddum, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_TIMESTART ], *istat );
        return ANL_NG;
    }
    ddum = trig->fstop + lc->timedel / 2.0;
    aste2attimeD( ddum, &burstStop );
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_TIMESTOP, i, 1, 1,
                             &ddum, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_TIMESTOP ], *istat );
        return ANL_NG;
    }

    sprintf( datestr, "%04d-%02d-%02dT%02d:%02d:%06.3f", burstStart.yr,
             burstStart.mo, burstStart.dy, burstStart.hr, burstStart.mn,
             burstStart.sc + burstStart.ss );
    if ( fits_write_col_str( fp, HBJ_OUTCOL_BST_DATESTART, i, 1, 1,
                             &charp, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_DATESTART ], *istat );
        return ANL_NG;
    }
    
    sprintf( datestr, "%04d-%02d-%02dT%02d:%02d:%06.3f", burstStop.yr,
             burstStop.mo, burstStop.dy, burstStop.hr, burstStop.mn,
             burstStop.sc + burstStop.ss );
    if ( fits_write_col_str( fp, HBJ_OUTCOL_BST_DATESTOP, i, 1, 1,
                             &charp, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_DATESTOP ], *istat );
        return ANL_NG;
    }



    /* write method */
    charp = datestr;
    if ( det_alg == HETE2 ) {
        sprintf( datestr, "HETE2" );
    } else if ( det_alg == GINGA ) {
        sprintf( datestr, "GINGA" );
    }
    if ( fits_write_col_str( fp, HBJ_OUTCOL_BST_METHOD, i, 1, 1,
                             &charp, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_METHOD ], *istat );
        return ANL_NG;
    }



    /* write rate, error, snr, bkg, bkg, net and net error */
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_RATE, i, 1, 1,
                             &( trig->net_rate ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_RATE ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_ERROR, i, 1, 1,
                             &( trig->net_rateerr ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_ERROR ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_RATESNR, i, 1, 1,
                             &( trig->sigma ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_RATESNR ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_FORERATE, i, 1, 1,
                             &( trig->fore_rate ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_FORERATE ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_FOREERROR, i, 1, 1,
                             &( trig->fore_rateerr ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_FOREERROR ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_BKGRATE, i, 1, 1,
                             &( trig->back_rate ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_BKGRATE ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_BKGERROR, i, 1, 1,
                             &( trig->back_rateerr ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_BKGERROR ], *istat );
        return ANL_NG;
    }



    /* write vector columns for background start/stop/fit coefficients */
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_BKGSTART, i, 1, 1,
                             &( trig->b1start ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_BKGSTART ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_BKGSTOP, i, 1, 1,
                             &( trig->b1stop ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_BKGSTOP ], *istat );
        return ANL_NG;
    }
    if ( det_alg == HETE2 ) {
        if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_BKGSTART, i, 2, 1,
                                 &( trig->b2start ), istat ) ) {
            fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                     pname, ttype_bst[ HBJ_OUTCOL_BST_BKGSTART ], *istat );
            return ANL_NG;
        }
        if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_BKGSTOP, i, 2, 1,
                                 &( trig->b2stop ), istat ) ) {
            fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                     pname, ttype_bst[ HBJ_OUTCOL_BST_BKGSTOP ], *istat );
            return ANL_NG;
        }
        if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_BKGCOEFFS, i, 1, 2,
                                 trig->bgcoeffs, istat ) ) {
            fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                     pname, ttype_bst[ HBJ_OUTCOL_BST_BKGCOEFFS ], *istat );
            return ANL_NG;
        }
    } else {
        ddum = -1.0;
        if ( fits_write_colnull_dbl( fp, HBJ_OUTCOL_BST_BKGSTART, i, 2, 1,
                                     &ddum, ddum, istat ) ) {
            fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                     pname, ttype_bst[ HBJ_OUTCOL_BST_BKGSTART ], *istat );
            return ANL_NG;
        }
        if ( fits_write_colnull_dbl( fp, HBJ_OUTCOL_BST_BKGSTOP, i, 2, 1,
                                     &ddum, ddum, istat ) ) {
            fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                     pname, ttype_bst[ HBJ_OUTCOL_BST_BKGSTOP ], *istat );
            return ANL_NG;
        }
        if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_BKGCOEFFS, i, 1, 1,
                                 &( trig->back_rate ), istat ) ) {
            fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                     pname, ttype_bst[ HBJ_OUTCOL_BST_BKGCOEFFS ], *istat );
            return ANL_NG;
        }
        ddum = 0.0;
        if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_BKGCOEFFS, i, 2, 1,
                                 &ddum, istat ) ) {
            fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                     pname, ttype_bst[ HBJ_OUTCOL_BST_BKGCOEFFS ], *istat );
            return ANL_NG;
        }
    }



    /* write chanbin/min/max cols */
    charp = chanbin;
    if ( fits_write_col_str( fp, HBJ_OUTCOL_BST_CHANBIN, i, 1, 1,
                             &charp, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_CHANBIN ], *istat );
        return ANL_NG;
    }
    if ( fits_write_colnull_lng( fp, HBJ_OUTCOL_BST_CHANMIN, i, 1, 1,
                                 &chanmin, -1, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_lng(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_CHANMIN ], *istat );
        return ANL_NG;
    }
    if ( fits_write_colnull_lng( fp, HBJ_OUTCOL_BST_CHANMAX, i, 1, 1,
                                 &chanmax, -1, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_lng(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_CHANMAX ], *istat );
        return ANL_NG;
    }



    /* write T50/T50ERR/T90/T90ERR */
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_T50, i, 1, 1,
                             &( trig->t50 ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_T50 ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_T50ERR, i, 1, 1,
                             &( trig->t50err ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_T50ERR ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_T50START, i, 1, 1,
                             &( trig->t50start ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_T50START ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_T50STOP, i, 1, 1,
                             &( trig->t50stop ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_T50STOP ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_T90, i, 1, 1,
                             &( trig->t90 ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_T90 ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_T90ERR, i, 1, 1,
                             &( trig->t90err ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_T90ERR ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_T90START, i, 1, 1,
                             &( trig->t90start ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_T90START ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_BST_T90STOP, i, 1, 1,
                             &( trig->t90stop ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_T90STOP ], *istat );
        return ANL_NG;
    }



    /* write some filenames and such */
    if ( fits_write_col_str( fp, HBJ_OUTCOL_BST_DATAMODE, i, 1, 1,
                             &( stdkeys.datamode ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_DATAMODE ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_str( fp, HBJ_OUTCOL_BST_DETNAM, i, 1, 1,
                             &( stdkeys.detnam ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_DETNAM ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_str( fp, HBJ_OUTCOL_BST_LC_FILE, i, 1, 1,
                             &infilep, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_LC_FILE ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_str( fp, HBJ_OUTCOL_BST_TLM_FILE, i, 1, 1,
                             &( stdkeys.tlmfile ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_bst[ HBJ_OUTCOL_BST_TLM_FILE ], *istat );
        return ANL_NG;
    }



    return ANL_OK;
}

static int write_fits_row_stp( fitsfile* fp, int i, hxdBstJudge_Trigger* trig,
                               hxdBstJudge_LC* lc, HXD_STD_KEYS stdkeys,
                               long chanmin, long chanmax, char* chanbin,
                               char* infilep, int* istat ) {
    char datestr[24];
    char* charp;

    double ddum;

    AtTimeD burstStart;
    AtTimeD burstStop;

    charp = datestr;

    /* write start/stop in AETIME and date start/stop in UTC */
    ddum = trig->fstart - lc->timedel / 2.0;
    aste2attimeD( ddum, &burstStart );
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_STP_TIMESTART, i, 1, 1,
                             &ddum, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_TIMESTART ], *istat );
        return ANL_NG;
    }
    ddum = trig->fstop + lc->timedel / 2.0;
    aste2attimeD( ddum, &burstStop );
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_STP_TIMESTOP, i, 1, 1,
                             &ddum, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_TIMESTOP ], *istat );
        return ANL_NG;
    }
    sprintf( datestr, "%04d-%02d-%02dT%02d:%02d:%06.3f", burstStart.yr,
             burstStart.mo, burstStart.dy, burstStart.hr, burstStart.mn,
             burstStart.sc + burstStart.ss );
    if ( fits_write_col_str( fp, HBJ_OUTCOL_STP_DATESTART, i, 1, 1,
                             &charp, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_DATESTART ], *istat );
        return ANL_NG;
    }
    sprintf( datestr, "%04d-%02d-%02dT%02d:%02d:%06.3f", burstStop.yr,
             burstStop.mo, burstStop.dy, burstStop.hr, burstStop.mn,
             burstStop.sc + burstStop.ss );
    if ( fits_write_col_str( fp, HBJ_OUTCOL_STP_DATESTOP, i, 1, 1,
                             &charp, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_DATESTOP ], *istat );
        return ANL_NG;
    }

    /* write method */
    charp = datestr;
    sprintf( datestr, "STEP" );
    if ( fits_write_col_str( fp, HBJ_OUTCOL_STP_METHOD, i, 1, 1,
                             &charp, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_METHOD ], *istat );
        return ANL_NG;
    }

    /* write step params */
    ddum = ( trig->fstart + trig->fstop ) / 2.0;
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_STP_STEPTIME, i, 1, 1,
                             &ddum, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_STEPTIME ], *istat );
        return ANL_NG;
    }
    ddum = trig->fstop - trig->fstart;
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_STP_STEPWIDTH, i, 1, 1,
                             &ddum, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_STEPWIDTH ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_STP_STEPHEIGHT, i, 1, 1,
                             &( trig->bgcoeffs[ 3 ] ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_STEPHEIGHT ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_STP_STEPSNR, i, 1, 1,
                             &( trig->sigma ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_STEPSNR ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_STP_STEPCOEFFS, i, 1, 5,
                             trig->bgcoeffs, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_STEPCOEFFS ], *istat );
        return ANL_NG;
    }

    /* write vector columns for background start/stop */
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_STP_BKGSTART, i, 1, 1,
                             &( trig->b1start ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_BKGSTART ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_STP_BKGSTOP, i, 1, 1,
                             &( trig->b1stop ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_BKGSTOP ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_STP_BKGSTART, i, 2, 1,
                             &( trig->b2start ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_BKGSTART ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_dbl( fp, HBJ_OUTCOL_STP_BKGSTOP, i, 2, 1,
                             &( trig->b2stop ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_dbl(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_BKGSTOP ], *istat );
        return ANL_NG;
    }

    /* write chanbin/min/max cols */
    charp = chanbin;
    if ( fits_write_col_str( fp, HBJ_OUTCOL_STP_CHANBIN, i, 1, 1,
                             &charp, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_CHANBIN ], *istat );
        return ANL_NG;
    }
    if ( fits_write_colnull_lng( fp, HBJ_OUTCOL_STP_CHANMIN, i, 1, 1,
                                 &chanmin, -1, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_lng(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_CHANMIN ], *istat );
        return ANL_NG;
    }
    if ( fits_write_colnull_lng( fp, HBJ_OUTCOL_STP_CHANMAX, i, 1, 1,
                                 &chanmax, -1, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_lng(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_CHANMAX ], *istat );
        return ANL_NG;
    }

    /* write some filenames and such */
    if ( fits_write_col_str( fp, HBJ_OUTCOL_STP_DATAMODE, i, 1, 1,
                             &( stdkeys.datamode ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_DATAMODE ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_str( fp, HBJ_OUTCOL_STP_DETNAM, i, 1, 1,
                             &( stdkeys.detnam ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_DETNAM ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_str( fp, HBJ_OUTCOL_STP_LC_FILE, i, 1, 1,
                             &infilep, istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_LC_FILE ], *istat );
        return ANL_NG;
    }
    if ( fits_write_col_str( fp, HBJ_OUTCOL_STP_TLM_FILE, i, 1, 1,
                             &( stdkeys.tlmfile ), istat ) ) {
        fprintf( stderr, "%s:fits_write_col_str(%s) failed (%d)\n",
                 pname, ttype_stp[ HBJ_OUTCOL_STP_TLM_FILE ], *istat );
        return ANL_NG;
    }
    return ANL_OK;
}

static int
HXDbstJudge_writebstFitsTable( char* inlc, char* outname,
                               hxdBstJudge_LC* lc,
                               hxdBstJudge_Trigger** trigs, int ntrigs ) {

    int istat = 0;
    int simple = 1;
    int bitpix = 8;
    int naxis = 0;
    long naxes[1];
    long pcount = 0;
    long gcount = 1;
    int extend = 1; /** Extension is included**/
    int nc;
    int i = 0;
    long chanmin, chanmax;
    char chanbin[80];
    char comment[80];
    char datestr[24];
    char infile[FLEN_FILENAME];
    char* infilep;
    char basefile[FLEN_FILENAME];
    char* charp = datestr;
    char origin[30];
    double ddum;

    fitsfile* fp;
    fitsfile* infp;

    HXD_STD_KEYS stdkeys;

    chanmin = chanmax = -1;

    /* calculate the basename of the light curve file */
    /* but including any CFITSIO filtering */
    fits_parse_rootname( inlc, basefile, &istat );
    infilep = &basefile[ 0 ];
    charp = infilep;
    while( ( infilep = index( infilep, '/' ) ) ) {
        charp = ++infilep;
    }
    strncpy( infile, inlc, FLEN_FILENAME );
    i = strlen( charp );
    infilep = &infile[ 0 ];
    while ( infilep && strncmp( charp, infilep, i ) != 0 ) {
        infilep = index( infilep, '/' );
        infilep++;
    }
    if ( !infilep ) {
        fprintf( stderr, "%s: failed to parse input filename\n",
                 pname );
        return ANL_NG;
    }



    /* open input file for reading keywords */
    if ( fits_open_data( &infp, inlc, READONLY, &istat ) ) {
        fprintf( stderr, "%s:fits_open_data(%s) failed (%d)\n",
                 pname, inlc, istat );
        return ANL_NG;
    }



    /* store origin in the bank so we don't fail later */
    sprintf( origin, "%s", pname );
    BnkDef( "ASTE:FFF_ORIGIN", sizeof( origin ) );
    BnkPut( "ASTE:FFF_ORIGIN", sizeof( origin ), origin );



    /* create output file */
    if ( fits_create_file( &fp, outname, &istat ) ) {
      fprintf( stderr, "%s:fits_create_file %s failed (%d)\n", pname, outname, istat );
        return ANL_NG;
    }
    if ( fits_write_grphdr( fp, simple, bitpix, naxis, naxes,
                            pcount, gcount, extend, &istat ) ) {
        fprintf( stderr, "%s:fits_write_grphdr failed (%d)\n", pname, istat );
        return ANL_NG;
    }



    /* allocate and setup keywords to write */
    hxdFitsHeader_mallocSTDKEYS( &stdkeys );
    hxdFitsHeader_setDefaultKeywordValues( &stdkeys );

    stdkeys.hduclass_ev_hk = -1;
    stdkeys.use_detnam     = 1;
    stdkeys.write_radec    = 1;

    sprintf( stdkeys.creator, "ANL: %s: %s", pname, HXDbstJudge_version );



    /* Credits, from ASTE_ANL (later than version 1.40)*/
    sprintf( stdkeys.credits,      anl_task_credits( ) );
    sprintf( stdkeys.task_name,    anl_task_name( )    );
    sprintf( stdkeys.task_version, anl_task_version( ) );

    /* read keywords from input file */
    fits_read_key_str( infp, "INSTRUME", stdkeys.instrument,
                       comment, &istat );
    fits_read_key_str( infp, "DETNAM", stdkeys.detnam,
                       comment, &istat );
    fits_read_key_str( infp, "DATAMODE", stdkeys.datamode,
                       comment, &istat );
    if ( istat ) { sprintf( stdkeys.datamode, "  " ); istat = 0; }

    fits_read_key_str( infp, "TLM_FILE", stdkeys.tlmfile,
                       comment, &istat );
    if ( istat ) { sprintf( stdkeys.tlmfile, "  " ); istat = 0; }
    fits_read_key_str( infp, "TIM_FILE", stdkeys.timfile,
                       comment, &istat);
    if ( istat ) { sprintf( stdkeys.timfile, "  " ); istat = 0; }
    fits_read_key_str( infp, "ATT_FILE", stdkeys.attfile,
                       comment, &istat );
    if ( istat ) { sprintf( stdkeys.attfile, "  " ); istat = 0; }
    fits_read_key_str( infp, "ORB_FILE", stdkeys.orbfile,
                       comment, &istat );
    if ( istat ) { sprintf( stdkeys.orbfile, "  " ); istat = 0; }
    fits_read_key_str( infp, "LEAPFILE", stdkeys.leapfile,
                       comment, &istat );
    if ( istat ) { sprintf( stdkeys.leapfile, "  " ); istat = 0; }
    fits_read_key_str( infp, "TELDEF", stdkeys.teldef,
                       comment, &istat );
    if ( istat ) { sprintf( stdkeys.teldef, "  " ); istat = 0; }

    fits_read_key_flt( infp, "TIERRELA", &stdkeys.tierrela,
                       comment, &istat );
    if ( istat ) { stdkeys.tierrela = -999; istat = 0; }
    fits_read_key_flt( infp, "TIERABSO",  &stdkeys.tierabso,
                       comment, &istat );
    if ( istat ) { stdkeys.tierabso = -999; istat = 0; }

    fits_read_key_dbl( infp, "TSTART", &stdkeys.tstart,
                       comment, &istat );
    fits_read_key_dbl( infp, "TSTOP",  &stdkeys.tstop,
                       comment, &istat );
    fits_read_key_dbl( infp, "TELAPSE", &stdkeys.telapse,
                       comment, &istat );
    fits_read_key_dbl( infp, "ONTIME",  &stdkeys.ontime,
                       comment, &istat );

    fits_read_key_str( infp, "OBJECT", stdkeys.object,
                       comment, &istat );
    fits_read_key_str( infp, "OBSERVER", stdkeys.observer,
                       comment, &istat );
    fits_read_key_str( infp, "OBS_MODE", stdkeys.obs_mode,
                       comment, &istat );
    fits_read_key_str( infp, "OBS_ID", stdkeys.obs_id,
                       comment, &istat );
    fits_read_key_str( infp, "DATE-OBS", stdkeys.date_obs,
                       comment, &istat );
    fits_read_key_str( infp, "TIME-OBS", stdkeys.time_obs,
                       comment, &istat );
    fits_read_key_str( infp, "DATE-END", stdkeys.date_end,
                       comment, &istat );
    fits_read_key_str( infp, "TIME-END", stdkeys.time_end,
                       comment, &istat );

    fits_read_key_dbl( infp, "RA_OBJ", &stdkeys.ra_obj,
                       comment, &istat );
    if ( istat ) { stdkeys.ra_obj = 0.0; istat = 0; }
    fits_read_key_dbl( infp, "DEC_OBJ", &stdkeys.dec_obj,
                       comment, &istat );
    if ( istat ) { stdkeys.dec_obj = 0.0; istat = 0; }
    fits_read_key_dbl( infp, "RA_PNT", &stdkeys.ra_pnt,
                       comment, &istat );
    if ( istat ) { stdkeys.ra_pnt = 0.0; istat = 0; }
    fits_read_key_dbl( infp, "DEC_PNT", &stdkeys.dec_pnt,
                       comment, &istat );
    if ( istat ) { stdkeys.dec_pnt = 0.0; istat = 0; }
    fits_read_key_dbl( infp, "RA_NOM", &stdkeys.ra_nom,
                       comment, &istat );
    if ( istat ) { stdkeys.ra_nom = 0.0; istat = 0; }
    fits_read_key_dbl( infp, "DEC_NOM", &stdkeys.dec_nom,
                       comment, &istat );
    if ( istat ) { stdkeys.dec_nom = 0.0; istat = 0; }
    fits_read_key_dbl( infp, "PA_NOM", &stdkeys.roll_nom,
                       comment, &istat );
    if ( istat ) { stdkeys.roll_nom = 0.0; istat = 0; }
    fits_read_key_dbl( infp, "MEAN_EA1", &stdkeys.mean_ea1,
                       comment, &istat );
    if ( istat ) { stdkeys.mean_ea1 = 0.0; istat = 0; }
    fits_read_key_dbl( infp, "MEAN_EA2", &stdkeys.mean_ea2,
                       comment, &istat );
    if ( istat ) { stdkeys.mean_ea2 = 0.0; istat = 0; }
    fits_read_key_dbl( infp, "MEAN_EA3", &stdkeys.mean_ea3,
                       comment, &istat );
    if ( istat ) { stdkeys.mean_ea3 = 0.0; istat = 0; }



    /*
    ** read the chanbin/chanmin/chanmax keywords to use in the table
    ** but don't put into stdkeys
    */
    charp = chanbin;
    fits_read_key_str( infp, "CHANBIN", charp,
                       comment, &istat );
    if ( istat ) { sprintf( chanbin, "UN" ); istat = 0; }
    fits_read_key_lng( infp, "CHANMIN", &chanmin,
                       comment, &istat );
    if ( istat ) { chanmin = -1; istat = 0; }
    fits_read_key_lng( infp, "CHANMAX", &chanmax,
                       comment, &istat );
    if ( istat ) { chanmax = -1; istat = 0; }

    fits_close_file( infp, &istat );



    /* write standard keywords */
    hxdFitsHeader_writeHXDStdKeys( fp, stdkeys, 1, &istat );
    if ( istat ) {
        fprintf( stderr, "%s:hxdFitsHeader_writeHXDStdKeys failed (%d)\n ",
                 pname, istat);
        return ANL_NG;
    }

    istat = HXDbstJudge_writeHistory( fp );
    if ( istat != ANL_OK ) {
        fprintf( stderr, "%s: Error in writing history\n", pname );
        return istat;
    } else {
        istat = 0;
    }

    hxdFitsHeader_updateStdTimeKeys( fp, stdkeys, 1, &istat );
    if ( istat ) {
        fprintf( stderr, "%s:hxdFitsHeader_updateStdTimeKeys failed (%d)\n ",
                 pname, istat );
        return ANL_NG;
    }

    if ( fits_write_date( fp, &istat ) ) {
        fprintf( stderr, "%s:fits_write_date for header failed (%d)\n",
                 pname, istat );
        return ANL_NG;
    }
    if ( fits_delete_key( fp, "CLOCKAPP", &istat ) ) { istat = 0; }
    if ( fits_delete_key( fp, "TIMEDEL",  &istat ) ) { istat = 0; }
    if ( fits_delete_key( fp, "TIMEPIXR", &istat ) ) { istat = 0; }
    if ( fits_delete_key( fp, "TIERRELA", &istat ) ) { istat = 0; }
    if ( fits_delete_key( fp, "TIERABSO", &istat ) ) { istat = 0; }



    /* create table */
    if ( fits_create_hdu( fp, &istat ) ) {
        fprintf( stderr, "%s:fits_creat_hdu failed (%d)\n", pname, istat );
        return ANL_NG;
    }
    if ( det_alg == STEP ) {
        nc = HXDBSTJUDGE_OUTFITS_STP_NUM_COL;
        if ( fits_write_btblhdr( fp, ntrigs, nc, ttype_stp, tform_stp,
                                 tunit_stp, "EVTTABLE", pcount, &istat ) ) {
            fprintf( stderr, "%s:fits_write_btblhdr failed (%d)\n",
                     pname, istat );
            return ANL_NG;
        }
    } else {
        nc = HXDBSTJUDGE_OUTFITS_BST_NUM_COL;
        if ( fits_write_btblhdr( fp, ntrigs, nc, ttype_bst, tform_bst,
                                 tunit_bst, "EVTTABLE", pcount, &istat ) ) {
            fprintf( stderr, "%s:fits_write_btblhdr failed (%d)\n",
                     pname, istat );
            return ANL_NG;
        }
    }

    hxdFitsHeader_writeHXDStdKeys( fp, stdkeys, 2, &istat );
    if ( istat ) {
        fprintf( stderr, "%s:hxdFitsHeader_writeHXDStdKeys failed (%d)\n ",
                 pname, istat );
        return ANL_NG;
    }

    istat = HXDbstJudge_writeHistory( fp );
    if ( istat != ANL_OK ) {
        fprintf( stderr, "%s: Error in writing history\n", pname );
        return istat;
    } else {
        istat = 0;
    }



    /* write comments */
    for ( i = 0; i < HXDBSTJUDGE_OUTFITS_NUM_COMM; i++ ) {
        if ( fits_write_comment( fp, comm[ i ], &istat ) ) {
            fprintf(stderr, "%s:fits_write_comment() failed (%d)\n",
                    pname, istat );
            return ANL_NG;
        }
    }
    if ( det_alg == STEP ) {
        for ( i = 0; i < HXDBSTJUDGE_OUTFITS_STP_NUM_COMM; i++ ) {
            if ( fits_write_comment( fp, comm_stp[ i ], &istat ) ) {
                fprintf(stderr, "%s:fits_write_comment() failed (%d)\n",
                        pname, istat );
                return ANL_NG;
            }
        }
    } else {
        for ( i = 0; i < HXDBSTJUDGE_OUTFITS_BST_NUM_COMM; i++ ) {
            if ( fits_write_comment( fp, comm_bst[ i ], &istat ) ) {
                fprintf(stderr, "%s:fits_write_comment() failed (%d)\n",
                        pname, istat );
                return ANL_NG;
            }
        }
    }

    hxdFitsHeader_updateStdTimeKeys(fp, stdkeys, 2, &istat);
    if(istat){
        fprintf(stderr, "%s:hxdFitsHeader_updateStdTimeKeys failed (%d)\n ",
                pname, istat );
        return ANL_NG;
    }

    if ( fits_write_date( fp, &istat ) ) {
        fprintf( stderr, "%s:fits_write_date for 1st extension failed (%d)\n",
                 pname, istat );
        return ANL_NG;
    }
    if ( fits_delete_key( fp, "CLOCKAPP", &istat ) ) { istat = 0; }
    if ( fits_delete_key( fp, "TIMEDEL",  &istat ) ) { istat = 0; }
    if ( fits_delete_key( fp, "TIMEPIXR", &istat ) ) { istat = 0; }
    if ( fits_delete_key( fp, "TIERRELA", &istat ) ) { istat = 0; }
    if ( fits_delete_key( fp, "TIERABSO", &istat ) ) { istat = 0; }



    /* now write the table */
    for ( i = 0; i < ntrigs; i++ ) {


        if ( det_alg == STEP ) {
            if ( write_fits_row_stp( fp, i + 1, trigs[ i ], lc, stdkeys, chanmin, chanmax, chanbin, infilep, &istat ) ) {
                return ANL_NG;
            }
        } else {
            if ( write_fits_row_bst( fp, i + 1, trigs[ i ], lc, stdkeys, chanmin, chanmax, chanbin, infilep, &istat ) ) {
                return ANL_NG;
            }
        }
        if ( istat ) {
            return ANL_NG;
        }
    }



    fits_close_file( fp, &istat );
    return ANL_OK;



}
/*
** dummy
*/
void HXDbstJudge_startup( int *status ) {
    *status = ANL_OK;
}

/*
** common startup - read parameters
*/
void HXDbstJudge_com( int *status ) {


    if ( ( *status = PILGetFname( "input_name", filename ) ) ) {
        anl_msg_error( "%s: PILGetFname input_name error (%d)\n",
                       pname, *status );
        *status = ANL_ERROR;
        return;
    }
    if ( ( *status = PILGetString( "outfile", newfilename ) ) ) {
        anl_msg_error( "%s: PILGetString outfile error (%d)\n",
                       pname, *status );
        *status = ANL_ERROR;
        return;
    }

    /*
    ** check with detection algorithm to use
    */
    if ( ( *status = PILGetString( "det_alg", det_alg_str ) ) ) {
        anl_msg_error( "%s: PILGetReal det_alg error (%d)\n",
                       pname, *status );
        *status = ANL_ERROR;
        return;
    }
    if ( !strncmp( det_alg_str, "0", PIL_LINESIZE - 1 ) ||
         !strncmp( det_alg_str, "GINGA", PIL_LINESIZE - 1 ) ) {
        det_alg = GINGA;
    } else if ( !strncmp( det_alg_str, "1", PIL_LINESIZE - 1 ) ||
                !strncmp( det_alg_str, "HETE2", PIL_LINESIZE - 1 ) ) {
        det_alg = HETE2;
    } else if ( !strncmp( det_alg_str, "2", PIL_LINESIZE - 1 ) ||
                !strncmp( det_alg_str, "STEP", PIL_LINESIZE - 1 ) ) {
        det_alg = STEP;
    } else {
        anl_msg_error( "%s: Invalid value for det_alg parameter: %s\n",
                       pname, det_alg_str );
        *status = ANL_ERROR;
        return;
    }

    /*
    ** check if we're to use a pre-defined "trigger set"
    */
    if ( ( *status = PILGetBool( "use_trigger_set", &use_trigger_set ) ) ) {
        anl_msg_error( "%s: PILGetBool use_trigger_set error (%d)\n",
                       pname, *status );
        *status = ANL_ERROR;
        return;
    }

    /*
    ** don't allow it for anything but HETE2 though
    */
    if ( det_alg != HETE2 ) {
        use_trigger_set = 0;
    }
    if ( !use_trigger_set && det_alg != STEP ) {
        if ( ( *status = PILGetInt( "bgd_integ_time", &bgd_integ_time ) ) ) {
            anl_msg_error( "%s: PILGetInt bgd_integ_time error (%d)\n",
                           pname, *status );
            *status = ANL_ERROR;
            return;
        }
        if ( ( *status = PILGetInt( "bgd_early_gap", &bgd_early_gap ) ) ) {
            anl_msg_error( "%s: PILGetInt bgd_early_gap error (%d)\n",
                           pname, *status );
            *status = ANL_ERROR;
            return;
        }
        if ( ( *status = PILGetInt( "bgd_late_gap", &bgd_late_gap ) ) ) {
            anl_msg_error( "%s: PILGetInt bgd_late_gap error (%d)\n",
                           pname, *status );
            *status = ANL_ERROR;
            return;
        }
        if ( ( *status = PILGetReal( "delta_t", &judge_time ) ) ) {
            anl_msg_error( "%s: PILGetReal delta_t error (%d)\n",
                           pname, *status );
            *status = ANL_ERROR;
            return;
        }
    }
    if ( det_alg == STEP ) {
        if ( ( *status = PILGetInt( "step_window", &step_window ) ) ) {
            anl_msg_error( "%s: PILGetInt step_window error (%d)\n",
                           pname, *status );
            *status = ANL_ERROR;
            return;
        }
        if ( ( *status = PILGetReal( "step_delchi", &step_delchi ) ) ) {
            anl_msg_error( "%s: PILGetReal step_delchi error (%d)\n",
                           pname, *status );
            *status = ANL_ERROR;
            return;
        }
    }

    if ( ( *status = PILGetReal( "sigma", &sigma ) ) ) {
        anl_msg_error( "%s: PILGetReal sigma error (%d)\n",
                       pname, *status );
        *status = ANL_ERROR;
        return;
    }
    if ( ( *status = PILGetInt( "outtype", &outtype ) ) ) {
        anl_msg_error( "%s: PILGetInt outtype error (%d)\n",
                       pname, *status );
        *status = ANL_ERROR;
        return;
    }
    if ( ( *status = PILGetString( "trigger_set", trigger_set_str ) ) ) {
        anl_msg_error( "%s: PILGetReal trigger_set error (%d)\n",
                       pname, *status );
        *status = ANL_ERROR;
        return;
    }
    if ( !strncmp( trigger_set_str, "0", PIL_LINESIZE - 1 ) ||
         !strncmp( trigger_set_str, "SHORT", PIL_LINESIZE - 1 ) ) {
        trigger_set = TRIGGER_SET_SHORT;
    } else if ( !strncmp( trigger_set_str, "1", PIL_LINESIZE - 1 ) ||
                !strncmp( trigger_set_str, "MEDIUM", PIL_LINESIZE - 1 ) ) {
        trigger_set = TRIGGER_SET_MEDIUM;
    } else if ( !strncmp( trigger_set_str, "2", PIL_LINESIZE - 1 ) ||
                !strncmp( trigger_set_str, "LONG", PIL_LINESIZE - 1 ) ) {
        trigger_set = TRIGGER_SET_LONG;
    } else {
        anl_msg_error( "%s: Invalid value for trigger_set parameter: %s\n",
                       pname, trigger_set_str );
        *status = ANL_ERROR;
        return;
    }

    if ( ( *status = PILGetReal( "gaptol", &gaptol ) ) ) {
        anl_msg_error( "%s: PILGetReal gaptol error (%d)\n",
                       pname, *status );
        *status = ANL_ERROR;
        return;
    }
    if ( ( *status = PILGetReal( "overlaptol", &overlaptol ) ) ) {
        anl_msg_error( "%s: PILGetReal overlaptol error (%d)\n",
                       pname, *status );
        *status = ANL_ERROR;
        return;
    }
    if ( ( *status = PILGetReal( "maxdur", &maxdur ) ) ) {
        anl_msg_error( "%s: PILGetReal maxdur error (%d)\n",
                       pname, *status );
        *status = ANL_ERROR;
        return;
    }
    if ( ( *status = PILGetBool( "durest", &durest ) ) ) {
        anl_msg_error( "%s: PILGetBool durest error (%d)\n",
                       pname, *status );
        *status = ANL_ERROR;
        return;
    }
    if ( ( *status = PILGetBool( "clobber", &clobber ) ) ) {
        anl_msg_error( "%s: PILGetBool clobber error (%d)\n",
                       pname, *status );
        *status = ANL_ERROR;
        return;
    }
    *status = ANL_OK;

}

/*
** dummies
*/
void HXDbstJudge_init( int *status ) {
    *status = ANL_OK;
}

void HXDbstJudge_his( int *status ) {
    *status = ANL_OK;
}

/*
** begin run - reads light curve and calculates "background"
*/
void HXDbstJudge_bgnrun( int *status ) {

    int istat  = 0;
    int exists = 0;
    int hasexcl = 0;

    char newfilename2[ FLEN_FILENAME + 1 ];
    char* newfilenamep;

    /*
    ** strip off '!' from beginning of filename if there
    */
    if ( newfilename[ 0 ] == '!' ) {
        newfilenamep = &newfilename[ 1 ];
        strncpy( &newfilename2[ 0 ], newfilenamep, strlen( newfilename ) + 1 );
        newfilenamep = &newfilename2[ 0 ];
        strncpy( &newfilename[ 0 ], newfilenamep, strlen( newfilename2 ) + 1 );
        hasexcl = 1;
    }

    /*
    ** check if we need to clobber
    */
    if ( fits_file_exists( newfilename, &exists, &istat ) ) {
        anl_msg_error( "%s: could not determine if %s exists!\n",
                       pname, newfilename );
        *status = ANL_ERROR;
        return;
    }

    /* check if clobber set */
    if ( exists == 1 && !clobber && !hasexcl ) {
        anl_msg_error( "%s: %s exists and clobber not set!\n",
                       pname, newfilename );
        *status = ANL_ERROR;
        return;
    }

    /*
    ** set the correct clobbering output file name for the output type
    */
    if ( outtype == 0 ) {
        /* FITS output, clobber filename has ! in front */
        sprintf( newfilename2, "!%s", newfilename );
        newfilenamep = &newfilename2[ 0 ];
        strncpy( &newfilename[ 0 ], newfilenamep, strlen( newfilename2 ) + 1 );
    }

    *status = ANL_OK;
}

/*
** analysis loop
*/
void HXDbstJudge_ana( int nevent, int eventid, int *status ) {

    int i;
    int ntrigs = 0;

    double bgd_integ_time1;
    double bgd_integ_time2;
    double bgd_early_gapd;
    double bgd_late_gapd;

    FILE *text_fp = ( FILE* ) 0;

    AtTimeD burstStart;
    AtTimeD burstStop;

    hxdBstJudge_LC lc;
    hxdBstJudge_Trigger* trig;
    hxdBstJudge_Trigger** det_trigs = ( hxdBstJudge_Trigger** ) 0;
    hxdBstJudge_Trigger* intrigs[TRIGGER_SET_LENGTH];


    /*
    ** read the light curve
    */
    if ( hxdBstJudge_LC_read( filename, &lc ) ) {
        anl_msg_error( "%s: failed to read light curve from %s\n",
                       pname, filename );
        *status = ANL_ERROR;
        return;
    }


    /*
    ** calculate basic light curve stats
    */
    hxdBstJudge_LC_stats_basic( &lc, 1.0, 1.0e20 );

    if ( det_alg != HETE2 || !use_trigger_set ) {

        /*
        ** setup input "trigger" based on user inputs
        */
        bgd_early_gapd = det_alg == HETE2 ? ( double ) bgd_early_gap : 0.0;
        bgd_late_gapd =  det_alg == HETE2 ? ( double ) bgd_late_gap : 0.0;
        trig = hxdBstJudge_Trigger_new( 0.0, judge_time, ( double ) bgd_integ_time,
                                        bgd_early_gapd, ( double ) bgd_integ_time,
                                        bgd_late_gapd, sigma, -1 );
        if ( !trig ) {
            anl_msg_error( "%s: Could not allocate memory\n", pname );
            *status = ANL_NG;
            return;
        }

        /* disable the late bkg interval for GINGA mode */
        trig->b2stop   = det_alg == HETE2
                            ? trig->b2stop
                            : trig->b2start - 2.0 * lc.timedel;

        /* for STEP mode, use the fore_rateerr
           member to hold delta chi-squared, among others */
        if ( det_alg == STEP ) {
            trig->fore_rateerr = step_delchi;
            trig->b1start = 0.0;
            trig->b1stop  = ( double ) step_window;
        }

        intrigs[0] = trig;
        ntrigs = 1;
    } else {

        /* 
        ** setup a bunch of triggers for the trigger set chosen
        */
        for ( i = 0; i < TRIGGER_SET_LENGTH; i++ ) {
            bgd_early_gapd = -1.0 * trigger_set_bkg1_stop[trigger_set][i];
            bgd_late_gapd  = trigger_set_bkg2_start[trigger_set][i] -
                             trigger_set_fg_stop[trigger_set][i];
            bgd_integ_time1 = trigger_set_bkg1_stop[trigger_set][i] -
                              trigger_set_bkg1_start[trigger_set][i];
            bgd_integ_time2 = trigger_set_bkg2_stop[trigger_set][i] -
                              trigger_set_bkg2_start[trigger_set][i];
            trig = hxdBstJudge_Trigger_new( 0.0, trigger_set_fg_stop[trigger_set][i],
                                            bgd_integ_time1, bgd_early_gapd,
                                            bgd_integ_time2, bgd_late_gapd, sigma, -1 );
            if ( !trig ) {
                anl_msg_error( "%s: Could not allocate memory\n", pname );
                *status = ANL_NG;
                return;
            }
            intrigs[i] = trig;
        }
        ntrigs = TRIGGER_SET_LENGTH;
    }



    /*
    ** detect bursts
    */
    hxdBstJudge_LC_bst_judge( &lc, intrigs, ntrigs, &det_trigs,
                              &ntrigs, det_alg, gaptol, overlaptol,
                              maxdur, durest, status );
    if ( *status != ANL_OK ) {
        anl_msg_error( "%s: Burst detection failed\n", pname );
        return;
    }



    /*
    ** write bursts to output file
    */
    if ( ntrigs > 0 && det_trigs ) {
        if ( outtype == 0 ) {

            *status = HXDbstJudge_writebstFitsTable( filename, newfilename, &lc,
                                                     det_trigs, ntrigs );
        } else {

            /*
            ** open output file
            */
            text_fp = fopen( newfilename, "w" );
            if ( text_fp == NULL ) {
                anl_msg_error( "%s: failed to open file %s\n",
                               pname, newfilename );
                *status = ANL_ERROR;
                return;
            }

            /*
            ** print header into output file
            */
            fprintf( text_fp, "\
#\n\
# %s %s\n\
# Detected bursts in %s\n\
#\n\
#   START DATE         TSTART (AETIME)        TSTOP (AETIME)       DURATION (s)    COUNTS   SIGMA\n\
#------------------------------------------------------------------------------------------------\n",
                     pname, HXDbstJudge_version, filename );

            for( i = 0; i < ntrigs; i++ ) {

                /*
                ** calculate the attime
                */
                aste2attimeD( det_trigs[ i ]->fstart, &burstStart );
                aste2attimeD( det_trigs[ i ]->fstop, &burstStop );

                /*
                ** write a line to the output file
                */
                fprintf( text_fp, "%04d-%02d-%02d %02d:%02d:%02d %.16E %.16E %.8E %8d %7.1f\n",
                         burstStart.yr, burstStart.mo, burstStart.dy, burstStart.hr,
                         burstStart.mn, burstStart.sc,
                         det_trigs[ i ]->fstart - lc.timedel / 2.0,
                         det_trigs[ i ]->fstop + lc.timedel / 2.0,
                         det_trigs[ i ]->fstop - det_trigs[ i ]->fstart + lc.timedel,
                         ( int )det_trigs[ i ]->net_cnts, det_trigs[ i ]->sigma );
            }
            fclose( text_fp );
        }
    } else {
        anl_msg_warning( "%s: No bursts detected. Not creating output file.\n",
                         pname );
    }



    /*
    ** free memory
    */
    if ( use_trigger_set ) {
        for ( i = 0; i < TRIGGER_SET_LENGTH; i++ ) {
            if ( intrigs[ i ] ) {
                hxdBstJudge_Trigger_free( &( intrigs[ i ] ) );
                intrigs[ i ] = ( hxdBstJudge_Trigger* ) 0;
            }
        }
    } else {
        if ( intrigs[ 0 ] ) {
            hxdBstJudge_Trigger_free( &( intrigs[ 0 ] ) );
            intrigs[ 0 ] = ( hxdBstJudge_Trigger* ) 0;
        }
    }
    if ( ntrigs > 0 && det_trigs ) {
        for( i = 0; i < ntrigs; i++ ) {
            if ( det_trigs[ i ] ) {
                hxdBstJudge_Trigger_free( &( det_trigs[ i ] ) );
                det_trigs[ i ] = ( hxdBstJudge_Trigger* ) 0;
            }
        }
        free( det_trigs );
    }
    /*hxdBstJudge_Trigger_free( &trig );*/
    hxdBstJudge_LC_free( &lc );

    *status = ANL_QUIT;

}

/*
** cleanup
*/
void HXDbstJudge_endrun( int *status ) {

    *status = ANL_OK;
}

/*
** dummy
*/
void HXDbstJudge_exit( int *status ) {
    *status = ANL_OK;
}

