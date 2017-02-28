/*
 *   hxdcaldbUtil_gsoght.c
 *         2006-08-30, created by Y.Terada
 *                     for version 2.0.x.x process
 *         2007-04-24, modified by Y.Terada
 *                     add reserve space
 *         2007-05-11, modified by M.K (v0.6.8)
 *                     "ghf" -> "ght", row search algorithm
 *         2007-05-27, modified by M.K (v0.7.0)
 *                     nrow[3]
 */
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "hxdcaldbUtil.h"
#include "hxdFitsCommentUtil.h"
#include "atFunctions.h"
#include "aste_time.h"

static FILE*     hxd_gsoght_ascii_fp;
static fitsfile* hxd_gsoght_fits_fp;
static char tool_name[] = "hxdcaldbUtil_gsoght";

/* ===================================================================
 *   ASCII File I/O
 * ===================================================================*/
int hxdcaldbUtil_gsoght_open_ASCII (char* gsoght_ascii_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  hxd_gsoght_ascii_fp = fopen(gsoght_ascii_fname, "r");
  if (hxd_gsoght_ascii_fp == NULL){
    fprintf(stderr, "%s: Cannot open ASCII file(%s)\n",
            tool_name, gsoght_ascii_fname);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

int hxdcaldbUtil_gsoght_read_ASCII (GSO_GHT* gsoght_data){
  int status = HXDCALDBUTIL_STATUS_OK;

  /**------------ parameters in ASCII ------------**/
  double start_time;
  double end_time;
  int unit_id;  /** not in GSO_GHT **/
  int start_yyyymmdd; /* not in ASCII, make from start_time */
  int start_hhmmss;   /* not in ASCII, make from end_time */
  int model_id;
  double slow_param_a;
  double slow_param_b;
  double slow_param_c;
  double slow_longterm;
  double fast_param_a;
  double fast_param_b;
  double fast_param_c;
  double fast_longterm;
  /**-----------------------------------------------**/
  
  int N_row;

  AtTimeD attime;
  double prev_start_time = 0.0;
  double prev_end_time = 0.0;
  int    prev_unit_id  = 0xFFFF;
  int    the_unit_id;

  if (gsoght_data == NULL){
    fprintf(stderr, "hxdcaldbUtil: Invalid data pointer(gsoght_data)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if (hxd_gsoght_ascii_fp == NULL){
    fprintf(stderr, "hxdcaldbUtil: Invalid file pointer(hxd_gsogh)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  N_row = -1;
  while(!feof(hxd_gsoght_ascii_fp))   {
    fscanf(hxd_gsoght_ascii_fp,
	   "%lf %lf %d %d %lf %lf %lf %lf %lf %lf %lf %lf\n",
	   &start_time, &end_time, &unit_id, &model_id,
	   &slow_param_a,&slow_param_b,&slow_param_c,&slow_longterm,
	   &fast_param_a,&fast_param_b,&fast_param_c,&fast_longterm);

    /*** (1) check the format ***/
    if ( ((int)start_time) != ((int)prev_start_time) ) {
      /** (1-1) end of the time zone **/
      if (prev_unit_id != 0xFFFF){
        fprintf(stderr, "%s:No.%d: Not All Unit ID defined. 0x%04X\n",
                tool_name, N_row, prev_unit_id);
      }
      prev_unit_id = 0;
      prev_start_time = start_time;
      prev_end_time   = end_time;
      N_row ++;
    } else {
      /** (1-2) same time zone: check **/
      if ( ((int)end_time) != ((int)prev_end_time) ){
        fprintf(stderr, 
                "%s:No.%d: same start time (%f), diffrent end time (%f->%f)\n",
                tool_name, N_row, start_time, prev_end_time, end_time);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }
      if ( prev_unit_id & (0x0001<<unit_id) ){
        fprintf(stderr, "%s:No.%d: same unit_id=%d\n",
                tool_name, N_row, unit_id);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
      }
    }
    prev_unit_id |= (0x0001<<unit_id);
    /** (1-3) check **/
    if (unit_id > HXD_WEL_N_UNIT){
        fprintf(stderr, "%s:No.%d: undefined unit_id=%d\n",
                tool_name, N_row, unit_id);
        status = HXDCALDBUTIL_STATUS_NG;
        return status;
    }

    /*** (2) write the data into the structure ***/
    aste2attimeD(start_time,&attime);
    start_yyyymmdd = attime.yr*10000 + attime.mo*100 + attime.dy;
    start_hhmmss = attime.hr*10000 + attime.mn*100 + attime.sc;
    gsoght_data->data[N_row].start_time     = start_time;
    gsoght_data->data[N_row].end_time       = end_time;
    gsoght_data->data[N_row].start_yyyymmdd = start_yyyymmdd;
    gsoght_data->data[N_row].start_hhmmss   = start_hhmmss;
    gsoght_data->data[N_row].model_id       = model_id;
    gsoght_data->data[N_row].slow_param_a[unit_id]  = slow_param_a;
    gsoght_data->data[N_row].slow_param_b[unit_id]  = slow_param_b;
    gsoght_data->data[N_row].slow_param_c[unit_id]  = slow_param_c;
    gsoght_data->data[N_row].slow_longterm[unit_id] = slow_longterm;
    gsoght_data->data[N_row].fast_param_a[unit_id]  = fast_param_a;
    gsoght_data->data[N_row].fast_param_b[unit_id]  = fast_param_b;
    gsoght_data->data[N_row].fast_param_c[unit_id]  = fast_param_c;
    gsoght_data->data[N_row].fast_longterm[unit_id] = fast_longterm;
  } /** end of feof **/

  gsoght_data->nrow = N_row+1;

  return status;
}

int hxdcaldbUtil_gsoght_close_ASCII(void){
  int status = HXDCALDBUTIL_STATUS_OK;
  if (fclose(hxd_gsoght_ascii_fp) ){
    fprintf(stderr, "%s: Cannot close ASCII file\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

/* ===================================================================
 *   Fits File  Create
 * ===================================================================*/
int hxdcaldbUtil_gsoght_create_FITS(char* gsoght_fits_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
  int bitpix = 8;
  long naxis2 = 1;
  long nrow=0;
  char detname[16] = "WELL_GSO";
  char code_name[16] = "GAIN_HIST_PARAM";
  char *extension_name[HXDGSOGHT_PMT_N_LINE]={
    "GHT_152GD_350KEV",
    "GHT_ANNIHILATION_511KEV",
    "GHT_153GD_150KEV"
  };
  long naxes[1];
  int unit_id;

#define HXDPMT_GSOGAINHIST_TBL_N_KEYWORD 37

static char *ttype[HXDPMT_GSOGAINHIST_TBL_N_KEYWORD] = {
    "START_TIME", "YYYYMMDD", "HHMMSS", 
    "END_TIME", "MODEL_ID", 
    "W00_SLOW", "W00_FAST",
    "W01_SLOW", "W01_FAST",
    "W02_SLOW", "W02_FAST",
    "W03_SLOW", "W03_FAST",
    "W10_SLOW", "W10_FAST",
    "W11_SLOW", "W11_FAST",
    "W12_SLOW", "W12_FAST",
    "W13_SLOW", "W13_FAST",
    "W20_SLOW", "W20_FAST",
    "W21_SLOW", "W21_FAST",
    "W22_SLOW", "W22_FAST",
    "W23_SLOW", "W23_FAST",
    "W30_SLOW", "W30_FAST",
    "W31_SLOW", "W31_FAST",
    "W32_SLOW", "W32_FAST",
    "W33_SLOW", "W33_FAST"
};
static char *tform[HXDPMT_GSOGAINHIST_TBL_N_KEYWORD] = {
    "1D", "1J", "1J", 
    "1D", "1I", 
    /** Fit Results **/
    "8D", "8D", "8D", "8D", 
    "8D", "8D", "8D", "8D", 
    "8D", "8D", "8D", "8D", 
    "8D", "8D", "8D", "8D",
    "8D", "8D", "8D", "8D", 
    "8D", "8D", "8D", "8D", 
    "8D", "8D", "8D", "8D", 
    "8D", "8D", "8D", "8D"
};

 static char *tunit[HXDPMT_GSOGAINHIST_TBL_N_KEYWORD] = {
    "s", "", "",  
    "s", "",
    "","","","",
    "","","","",
    "","","","",
    "","","","",
    "","","","",
    "","","","",
    "","","","",
    "","","",""
  };
  char *keyword[HXDPMT_GSOGAINHIST_TBL_N_KEYWORD]={
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  ", 
    "TTYPE6  ", "TTYPE7  ", "TTYPE8  ","TTYPE9  ", "TTYPE10  ", 
    "TTYPE11  ", "TTYPE12  ", "TTYPE13  ", "TTYPE14  ", "TTYPE15  ", 
    "TTYPE16  ", "TTYPE17  ", "TTYPE18  ", "TTYPE19  ", "TTYPE20  ",
    "TTYPE21  ", "TTYPE22  ", "TTYPE23  ", "TTYPE24  ", "TTYPE25  ", 
    "TTYPE26  ", "TTYPE27  ", "TTYPE28  ", "TTYPE29  ", "TTYPE30  ",
    "TTYPE31  ", "TTYPE32  ", "TTYPE33  ", "TTYPE34  ", "TTYPE35  ", 
    "TTYPE36  ", "TTYPE37  "
  };
  char *comment[HXDPMT_GSOGAINHIST_TBL_N_KEYWORD]={
    "Start time",  "date of START_TIME", "time of START_TIME",
    "End time", "Model ID",
    "Gain parameters of W00 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W00 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W01 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W01 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W02 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W02 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W03 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W03 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W10 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W10 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W11 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W11 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W12 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W12 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W13 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W13 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W20 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W20 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W21 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W21 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W22 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W22 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W23 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W23 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W30 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W30 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W31 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W31 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W32 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W32 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W33 PI_SLOW                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage.",
    "Gain parameters of W33 PI_FAST                                        01: temperature dependence,             (A)                           02: time constant after High Voltage ON (B)                           03: gain variability after HV ON,       (C)                           04: long term trend of PMT gain     (gain0)                           05-08: reserved                                                       when gain is described by                                              gain0 * [1+A*T] * [1+Cexp(-tsaa/b)]/[1+C],                            where T is time after launch, tsaa is time                            after SAA passage."
  };
  int ncol = sizeof(ttype)/sizeof(*ttype);
  char *description[HXDGSOGHT_PMT_N_LINE] ={
    "Gain parameters of the intrinsic Gd line on 348 keV.",
    "Gain parameters of the Annihilation line on 511 keV.",
    "Gain parameters of the line from 153Gd on 152 keV."
  };
  int  use_boundary = 1;
  char *ext_meanning[HXDGSOGHT_PMT_N_LINE] = {
    "ENERG(348,348)",
    "ENERG(511,511)",
    "ENERG(148,148)"
  };
  int hdu_num;

  if (fits_create_file(&hxd_gsoght_fits_fp, gsoght_fits_fname, &istat)) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n", tool_name, 
            gsoght_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } 

  if( fits_write_grphdr(hxd_gsoght_fits_fp,
                        1, 8, 0, naxes, 0, 1, 1, &istat) ){
    fprintf(stderr, "%s:fits_write_grphdr failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  for (hdu_num=0; hdu_num < HXDGSOGHT_PMT_N_LINE; hdu_num++){
    if( fits_create_hdu(hxd_gsoght_fits_fp, &istat) ){
      fprintf(stderr, "%s:fits_create_hdu failed (%d)\n", tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    if ( fits_write_btblhdr(hxd_gsoght_fits_fp, naxis2, ncol, 
                            ttype, tform, tunit, 
                            extension_name[hdu_num], 0, &istat) ) {
      fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    if (hxdcaldbUtil_write_fits_header(hxd_gsoght_fits_fp, 
                                       detname, code_name, 
                                       description[hdu_num],
                                       gsoght_fits_fname,
                                       use_boundary,ext_meanning[hdu_num])
        != HXDCALDBUTIL_STATUS_OK){
      fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    
    hxdFitsComment_write(hxd_gsoght_fits_fp,HXDPMT_GSOGAINHIST_TBL_N_KEYWORD,
                         keyword, comment, &istat);
    if (istat){
      fprintf(stderr, "%s: fits write comment failed.\n", tool_name);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
  }

  
  if(fits_flush_file(hxd_gsoght_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int hxdcaldbUtil_gsoght_write_row_FITS(int irow, GSO_GHT_TBL* ght_tbl){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
  int colnum;
  long firstelem = 1;
  long nelements = 1;
  enum{
    GSOGHT_START_TIME = 1, 
      GSOGHT_YYYYMMDD, GSOGHT_HHMMSS, 
      GSOGHT_END_TIME, GSOGHT_MODEL_ID, 
      GSOGHT_W00_SLOW, GSOGHT_W00_FAST,
      GSOGHT_W01_SLOW, GSOGHT_W01_FAST,
      GSOGHT_W02_SLOW, GSOGHT_W02_FAST,
      GSOGHT_W03_SLOW, GSOGHT_W03_FAST,
      GSOGHT_W10_SLOW, GSOGHT_W10_FAST,
      GSOGHT_W11_SLOW, GSOGHT_W11_FAST,
      GSOGHT_W12_SLOW, GSOGHT_W12_FAST,
      GSOGHT_W13_SLOW, GSOGHT_W13_FAST,
      GSOGHT_W20_SLOW, GSOGHT_W20_FAST,
      GSOGHT_W21_SLOW, GSOGHT_W21_FAST,
      GSOGHT_W22_SLOW, GSOGHT_W22_FAST,
      GSOGHT_W23_SLOW, GSOGHT_W23_FAST,
      GSOGHT_W30_SLOW, GSOGHT_W30_FAST,
      GSOGHT_W31_SLOW, GSOGHT_W31_FAST,
      GSOGHT_W32_SLOW, GSOGHT_W32_FAST,
      GSOGHT_W33_SLOW, GSOGHT_W33_FAST
      };
  double param[HXDGSOGHT_N_PARAM];
  int unit_id;

  /*** START_TIME ***/
  colnum = GSOGHT_START_TIME;
  fits_write_col_dbl(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements, 
                    &ght_tbl->start_time, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl start_time failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** YYYYMMDD ***/
  colnum = GSOGHT_YYYYMMDD;
  fits_write_col_int(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements, 
                    &ght_tbl->start_yyyymmdd, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl start_yyyymmdd failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** HHMMSS ***/
  colnum = GSOGHT_HHMMSS;
  fits_write_col_int(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements, 
                    &ght_tbl->start_hhmmss, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl start_hhmmss failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** END_TIME ***/
  colnum = GSOGHT_END_TIME;
  fits_write_col_dbl(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements, 
                    &ght_tbl->end_time, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl end_time failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** MODEL_ID ***/
  colnum = GSOGHT_MODEL_ID;
  fits_write_col_int(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements, 
		     &ght_tbl->model_id, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl model_id failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** Parameters ***/
  for(unit_id=0; unit_id<HXD_WEL_N_UNIT; unit_id++){
    /*- slow -*/
    param[0] = ght_tbl->slow_param_a[unit_id];
    param[1] = ght_tbl->slow_param_b[unit_id];
    param[2] = ght_tbl->slow_param_c[unit_id];
    param[3] = ght_tbl->slow_longterm[unit_id];
    param[4] = 0.0; /** reserve **/
    param[5] = 0.0; /** reserve **/
    param[6] = 0.0; /** reserve **/
    param[7] = 0.0; /** reserve **/

    colnum = GSOGHT_W00_SLOW + (unit_id * 2);
    nelements = HXDGSOGHT_N_PARAM;
    fits_write_col_dbl(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements,
		       param, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl W%d%d_SLOW failed (%d)\n",
	      tool_name, unit_id>>2, unit_id&0x03, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    /*- fast -*/
    param[0] = ght_tbl->fast_param_a[unit_id];
    param[1] = ght_tbl->fast_param_b[unit_id];
    param[2] = ght_tbl->fast_param_c[unit_id];
    param[3] = ght_tbl->fast_longterm[unit_id];
    param[4] = 0.0; /** reserve **/
    param[5] = 0.0; /** reserve **/
    param[6] = 0.0; /** reserve **/
    param[7] = 0.0; /** reserve **/

    colnum = GSOGHT_W00_FAST + (unit_id * 2);
    nelements = HXDGSOGHT_N_PARAM;
    fits_write_col_dbl(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements,
		       param, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_dbl W%d%d_FAST failed (%d)\n",
	      tool_name, unit_id>>2, unit_id&0x03, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
  }
  return status;
}

int hxdcaldbUtil_gsoght_write_FITS(GSO_GHT* gdalpha_ght_data,
				   GSO_GHT* annihi_ght_data,
				   GSO_GHT* gd153_ght_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
 
  GSO_GHT* ght_data[HXDGSOGHT_PMT_N_LINE];
  long naxis2;
  int hdu_num, hdutype;
  int irow;
  char detname[16] = "WELL_GSO"; /** pri header **/

  /** assign lines **/
  ght_data[0] = (GSO_GHT*) gdalpha_ght_data;
  ght_data[1] = (GSO_GHT*) annihi_ght_data;
  ght_data[2] = (GSO_GHT*) gd153_ght_data;

  for (hdu_num=0; hdu_num < HXDGSOGHT_PMT_N_LINE; hdu_num++){
    /*** (1) move to the corresponding extension ***/
    fits_movabs_hdu(hxd_gsoght_fits_fp, (hdu_num+2), &hdutype, &istat);
    if (istat){
      fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
              tool_name, hdu_num+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
   
    /** (2) update fits header **/
    naxis2= (long) ght_data[hdu_num]->nrow;
    if (fits_modify_key_lng(hxd_gsoght_fits_fp, "NAXIS2", naxis2, "&", 
			    &istat)){
      fprintf(stderr, "%s:fits_update_key NAXIS2=%d failed HDU%d (%d)\n", 
              tool_name, ght_data[hdu_num]->nrow, hdu_num+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    /** (3) write data **/
    for(irow=1; irow<=ght_data[hdu_num]->nrow; irow++){
      status = 
	hxdcaldbUtil_gsoght_write_row_FITS(irow, 
					   &ght_data[hdu_num]->data[irow-1]);
      if(status == HXDCALDBUTIL_STATUS_NG){
	fprintf(stderr, "%s: gsoght write fits failed (row=%d)\n", 
		tool_name, irow);
	return status;
      }
    } /** end of irow **/  

    /** (4) update fits header **/
    if (fits_write_date(hxd_gsoght_fits_fp, &istat)) {
      fprintf(stderr,"%s: fits_write_date failed (%d)\n", tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    
    if (fits_write_chksum(hxd_gsoght_fits_fp, &istat)) {
      fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", tool_name, 
              istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
  } /** end of hdu_num **/

  if(fits_flush_file(hxd_gsoght_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /******** Add Primary Header keywords ********/
  if (hxdcaldbUtil_write_fits_priheader(hxd_gsoght_fits_fp, detname)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

/* ===================================================================
 *   Fits File  Read
 * ===================================================================*/
int hxdcaldbUtil_gsoght_open_FITS(char* gsoght_fits_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  if (fits_open_file(&hxd_gsoght_fits_fp,gsoght_fits_fname, READONLY, 
		     &istat)) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
            gsoght_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  return status;
}

int hxdcaldbUtil_gsoght_read_row_FITS(int irow, GSO_GHT_TBL* ght_tbl){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  int colnum, anynul;
  long firstelem = 1;
  long nelements = 1;
  int    int_nulval = 0;
  double dbl_nulval = 0.0;
  enum{
    GSOGHT_START_TIME = 1, 
      GSOGHT_YYYYMMDD, GSOGHT_HHMMSS, 
      GSOGHT_END_TIME, GSOGHT_MODEL_ID, 
      GSOGHT_W00_SLOW, GSOGHT_W00_FAST,
      GSOGHT_W01_SLOW, GSOGHT_W01_FAST,
      GSOGHT_W02_SLOW, GSOGHT_W02_FAST,
      GSOGHT_W03_SLOW, GSOGHT_W03_FAST,
      GSOGHT_W10_SLOW, GSOGHT_W10_FAST,
      GSOGHT_W11_SLOW, GSOGHT_W11_FAST,
      GSOGHT_W12_SLOW, GSOGHT_W12_FAST,
      GSOGHT_W13_SLOW, GSOGHT_W13_FAST,
      GSOGHT_W20_SLOW, GSOGHT_W20_FAST,
      GSOGHT_W21_SLOW, GSOGHT_W21_FAST,
      GSOGHT_W22_SLOW, GSOGHT_W22_FAST,
      GSOGHT_W23_SLOW, GSOGHT_W23_FAST,
      GSOGHT_W30_SLOW, GSOGHT_W30_FAST,
      GSOGHT_W31_SLOW, GSOGHT_W31_FAST,
      GSOGHT_W32_SLOW, GSOGHT_W32_FAST,
      GSOGHT_W33_SLOW, GSOGHT_W33_FAST
      };
  double param[HXDGSOGHT_N_PARAM];
  int unit_id;

  /*** START_TIME ***/
  colnum = GSOGHT_START_TIME;
  fits_read_col_dbl(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements,
		    dbl_nulval, &ght_tbl->start_time, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl start_time failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** YYYYMMDD ***/
  colnum = GSOGHT_YYYYMMDD;
  fits_read_col_int(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements,
		    int_nulval, &ght_tbl->start_yyyymmdd, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl start_yyyymmdd failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** HHMMSS ***/
  colnum = GSOGHT_HHMMSS;
  fits_read_col_int(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements,
		    dbl_nulval, &ght_tbl->start_hhmmss, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl start_hhmmss failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** END_TIME ***/
  colnum = GSOGHT_END_TIME;
  fits_read_col_dbl(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements,
		    dbl_nulval, &ght_tbl->end_time, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl end_time failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** MODEL_ID ***/
  colnum = GSOGHT_MODEL_ID;
  fits_read_col_int(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements,
		    dbl_nulval, &ght_tbl->model_id, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl model_id failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** Parameters ***/
  for(unit_id=0; unit_id<HXD_WEL_N_UNIT; unit_id++){
    /*- slow -*/
    colnum = GSOGHT_W00_SLOW + (unit_id * 2);
    nelements = HXDGSOGHT_N_PARAM;
    fits_read_col_dbl(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements,
		      dbl_nulval, param, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl W%d%d_SLOW failed (%d)\n",
              tool_name, unit_id>>2, unit_id&0x03, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    ght_tbl->slow_param_a[unit_id]  = param[0];
    ght_tbl->slow_param_b[unit_id]  = param[1];
    ght_tbl->slow_param_c[unit_id]  = param[2];
    ght_tbl->slow_longterm[unit_id] = param[3];
    /** param[4-8] are reserved space**/

    /*- fast -*/
    colnum = GSOGHT_W00_FAST + (unit_id * 2);
    nelements = HXDGSOGHT_N_PARAM;
    fits_read_col_dbl(hxd_gsoght_fits_fp, colnum, irow, firstelem, nelements,
		      dbl_nulval, param, &anynul, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_col_dbl W%d%d_FAST failed (%d)\n",
              tool_name, unit_id>>2, unit_id&0x03, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    ght_tbl->fast_param_a[unit_id]  = param[0];
    ght_tbl->fast_param_b[unit_id]  = param[1];
    ght_tbl->fast_param_c[unit_id]  = param[2];
    ght_tbl->fast_longterm[unit_id] = param[3];
    /** param[4-8] are reserved space**/
  }

  return status;
}


int hxdcaldbUtil_gsoght_read_FITS(double tstart, double tstop,
				  GSO_GHT* gdalpha_ght_data,
				  GSO_GHT* annihi_ght_data,
				  GSO_GHT* gd153_ght_data, 
				  int* numrow){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  int hdu_num, hdutype;
  char comment[80];
  GSO_GHT* ght_data[HXDGSOGHT_PMT_N_LINE];
  /** for row search **/
  double ght_start_time;
  int startrow, stoprow;
  /** read start time **/
  int colnum,anynul;
  long irow, nrow;
  long firstelem = 1;
  long nelements = 1;
  enum {
    GSOGHT_START_TIME=1
  };
  double  double_nulval = 0.0;
  int i;

  /** assign lines **/
  ght_data[0] = (GSO_GHT*) gdalpha_ght_data;
  ght_data[1] = (GSO_GHT*) annihi_ght_data;
  ght_data[2] = (GSO_GHT*) gd153_ght_data;

  for (hdu_num=0; hdu_num < HXDGSOGHT_PMT_N_LINE; hdu_num++){
    /** (1) move to the HDU **/
    fits_movabs_hdu(hxd_gsoght_fits_fp, (hdu_num+2), &hdutype, &istat);
    if (istat){
      fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
              tool_name, hdu_num+1, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }

    /** (2) read number of rows **/
    fits_read_key_lng(hxd_gsoght_fits_fp, "NAXIS2", &nrow, comment, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_read_key NAXIS2 failed (%d)\n", 
              tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    numrow[hdu_num] = nrow ;

    /** (3) search row region, using tstart and tstop (only hdu_num==0) **/
    /*******    This  section is skipped in case of ght *****/

    /** (4) read the data **/
    for(irow=1;irow<=nrow;irow++){
      status = 
	hxdcaldbUtil_gsoght_read_row_FITS(irow, &ght_data[hdu_num]->data[irow-1]);
      if(status == HXDCALDBUTIL_STATUS_NG){
	fprintf(stderr, "%s: gsoght read fits failed (row=%d)\n", 
		tool_name, irow);
	return status;
      }
    } /** end of irow **/

  } /** end of hdu_num **/

  return status;
}

int hxdcaldbUtil_gsoght_close_FITS(void){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
  
  if(fits_close_file(hxd_gsoght_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  return status;
}

/* ========================= EOF =========================== */
