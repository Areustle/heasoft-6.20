 /*
 *   hxdcaldbUtil_bstidt.c
 *         2006-08-29, created by Y.Terada
 *         2006-09-15, format changed by Y.Terada
 *         2006-09-29, usr 'BURST_ID' by Y.Terada
 *         2007-04-24, format changed by Y.Terada
 *         2007-05-01, add functions.
 *         2008-11-02, update checksum, by Y.Terada
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "hxdcaldbUtil.h"
#include "hxdFitsCommentUtil.h"
#include "atFunctions.h"
#include "aste_time.h"

static FILE*     hxd_bstidt_ascii_fp;
static fitsfile* hxd_bstidt_fits_fp;
static char tool_name[] = "hxdcaldbUtil_bstidt";

/* ===================================================================
 *   ASCII File I/O
 * ===================================================================*/
int hxdcaldbUtil_bstidt_open_ASCII ( char *bstidt_ascii_fname ){
  int status = HXDCALDBUTIL_STATUS_OK;
  hxd_bstidt_ascii_fp = fopen(bstidt_ascii_fname, "r");
  if (hxd_bstidt_ascii_fp == NULL){
    fprintf(stderr, "%s: Cannot open ASCII file(%s)\n",
            tool_name, bstidt_ascii_fname);
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

int hxdcaldbUtil_bstidt_read_ASCII ( HxdBstidt *bstidt_data ){
  int status = HXDCALDBUTIL_STATUS_OK;
  int fscanf_stat;
  int N_row;

  /*** ----------------- ascii format ----------------------------  ***/
  int trg_id;                                /** (w) trigger id **/
  char burst_id[HXDBSTIDT_MAX_CHAR_LNGTH];   /** (w) burst_id   **/
  char trn_id[HXDBSTIDT_MAX_CHAR_LNGTH];     /** (w) trn_id     **/
  int yyyymmdd;                              /** (w) event date **/
  int hhmmss;                                /** (w) event time **/
  int classification;                        /** (w) event classification **/
  int trg_src;                               /** (t) trigger source **/
  int trg_src_soft;                          /** (b) trigger source, soft **/
  int frzd_yyyymmdd[HXDBSTIDT_TPU_N_BOARD];  /** (b) frzd time (yyyy/mm/dd)**/
  int frzd_hhmmss[HXDBSTIDT_TPU_N_BOARD];    /** (b) frzd time (hh:mm:ss)  **/
  double frzd_aetime[HXDBSTIDT_TPU_N_BOARD]; /** (b) frzd time (aetime)    **/
  int simul_sa;        /** (w) satellites simultaneous observation **/
  int peak_countrate[HXDBSTIDT_TPU_N_BOARD];  /** (w) raw peak count rate**/
  double euler_angle[HXDBSTIDT_N_EULER];     /** (w) Euler angle of Suzaku **/
  double inci_theta;   /** (w) incident theta **/
  double inci_phi;     /** (w) incident phi   **/
  double inci_ra;      /** (w) incident R.A.  **/
  double inci_dec;     /** (w) incident DEC   **/
  char  inci_origin[HXDBSTIDT_MAX_CHAR_LNGTH];   /** (w) INCI_ORIGIN    **/
  double sat_alt;      /** (w) Satellite altitude   **/
  double sat_lon;      /** (w) Satellite longitude **/
  double sat_lat;      /** (w) Satellite latitude   **/
  int time_mode[HXDBSTIDT_TPU_N_BOARD];      /** (t) time mode **/
  int frzon_gbread;                          /** (t) GB READ count **/
  double frzon_aetime[HXDBSTIDT_TPU_N_BOARD];/** (t) aetime    @ freeze on **/
  int frzon_ti[HXDBSTIDT_TPU_N_BOARD];       /** (t) TI        @ freeze on **/
  int frzon_t_latch_ti;                      /** (t) T_LATCH_TI@ freeze on **/
  int frzon_acu_t_latch;                     /** (t) T_LATCH   @ freeze on **/
  int frzof_gbread;                          /** (t) GB READ count **/
  double frzof_aetime[HXDBSTIDT_TPU_N_BOARD];/** (t) aetime    @ freeze off **/
  int  bstid_revision; /** revision of this row (0=created, 1,2,3,...) **/
  /*** ----------------- ascii format ----------------------------  ***/

  int board;
  int axis;

  if (bstidt_data == NULL){
    fprintf(stderr, "hxdcaldbUtil: Invalid data pointer(bstidt_data)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (hxd_bstidt_ascii_fp == NULL){
    fprintf(stderr, "hxdcaldbUtil: Invalid file pointer(hxd_bstidt)\n");
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  N_row = 0;
  while( !feof(hxd_bstidt_ascii_fp) ){
    fscanf(hxd_bstidt_ascii_fp,
	   "%d %s %s %d %d %d %d %d %d %d %d %d %d %d %d %d %lf %lf %lf %lf %d %d %d %d %d %lf %lf %lf %lf %lf %lf %lf %s %lf %lf %lf %d %d %d %d %d %lf %lf %lf %lf %d %d %d %d %d %d %d %lf %lf %lf %lf %d\n",
	   &trg_id, burst_id, trn_id, &yyyymmdd, &hhmmss, &classification, 
	   &trg_src, &trg_src_soft,
	   &frzd_yyyymmdd[0], &frzd_hhmmss[0], 
	   &frzd_yyyymmdd[1], &frzd_hhmmss[1], 
	   &frzd_yyyymmdd[2], &frzd_hhmmss[2], 
	   &frzd_yyyymmdd[3], &frzd_hhmmss[3], 
	   &frzd_aetime[0],&frzd_aetime[1],&frzd_aetime[2],&frzd_aetime[3],
	   &simul_sa, 
	   &peak_countrate[0], &peak_countrate[1], &peak_countrate[2], &peak_countrate[3], 
	   &euler_angle[0], &euler_angle[1], &euler_angle[2], 
	   &inci_theta, &inci_phi, &inci_ra, &inci_dec, inci_origin,
	   &sat_alt, &sat_lon, &sat_lat,
	   &time_mode[0],&time_mode[1],
	   &time_mode[2],&time_mode[3], &frzon_gbread,
	   &frzon_aetime[0], &frzon_aetime[1], 
	   &frzon_aetime[2], &frzon_aetime[3], 
	   &frzon_ti[0], &frzon_ti[1], &frzon_ti[2], &frzon_ti[3], 
	   &frzon_t_latch_ti, &frzon_acu_t_latch, &frzof_gbread,
	   &frzof_aetime[0], &frzof_aetime[1], 
	   &frzof_aetime[2], &frzof_aetime[3], &bstid_revision);

    bstidt_data->data[N_row].trg_id = trg_id;
    sprintf(bstidt_data->data[N_row].burst_id, burst_id);
    /*
    printf("BURST_ID=%s\n", burst_id);
    printf("BURST_ID=%s\n", bstidt_data->data[N_row].burst_id);
    */
    sprintf(bstidt_data->data[N_row].trn_id, trn_id);
    bstidt_data->data[N_row].yyyymmdd = yyyymmdd;
    bstidt_data->data[N_row].hhmmss = hhmmss;
    bstidt_data->data[N_row].classification = classification;
    bstidt_data->data[N_row].trg_src = trg_src;
    bstidt_data->data[N_row].trg_src_soft = trg_src_soft;
    bstidt_data->data[N_row].simul_sa = simul_sa;
    bstidt_data->data[N_row].inci_theta = inci_theta;
    bstidt_data->data[N_row].inci_phi   = inci_phi;
    bstidt_data->data[N_row].inci_ra    = inci_ra;
    bstidt_data->data[N_row].inci_dec   = inci_dec;
    sprintf(bstidt_data->data[N_row].inci_origin, inci_origin);
    bstidt_data->data[N_row].sat_alt    = sat_alt;
    bstidt_data->data[N_row].sat_lon    = sat_lon;
    bstidt_data->data[N_row].sat_lat    = sat_lat;
    bstidt_data->data[N_row].frzon_gbread = frzon_gbread;
    bstidt_data->data[N_row].frzon_t_latch_ti = frzon_t_latch_ti;
    bstidt_data->data[N_row].frzon_acu_t_latch = frzon_acu_t_latch;
    bstidt_data->data[N_row].frzof_gbread = frzof_gbread;
    bstidt_data->data[N_row].bstid_revision = bstid_revision;

    for(board=0; board<HXDBSTIDT_TPU_N_BOARD; board++){
      bstidt_data->data[N_row].frzd_yyyymmdd[board] = frzd_yyyymmdd[board];
      bstidt_data->data[N_row].frzd_hhmmss[board] = frzd_hhmmss[board];
      bstidt_data->data[N_row].frzd_aetime[board] = frzd_aetime[board];
      bstidt_data->data[N_row].time_mode[board] = time_mode[board];
      bstidt_data->data[N_row].peak_countrate[board] = peak_countrate[board];
      bstidt_data->data[N_row].frzon_aetime[board] = frzon_aetime[board];
      bstidt_data->data[N_row].frzon_ti[board] = frzon_ti[board];
      bstidt_data->data[N_row].frzof_aetime[board] = frzof_aetime[board];
    }
    for(axis=0;axis<HXDBSTIDT_N_EULER;axis++){
      bstidt_data->data[N_row].euler_angle[axis] = euler_angle[axis];
    }

    N_row ++;
    printf("N_row=%d\n",N_row);

  } /** end of ascii_fp **/

  bstidt_data->n_row = N_row;
  
  return status;
}

int hxdcaldbUtil_bstidt_close_ASCII( void){
  int status = HXDCALDBUTIL_STATUS_OK;
  
  if (fclose(hxd_bstidt_ascii_fp) ){
    fprintf(stderr, "%s: Cannot close ASCII file\n",tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
  }

  return status;
}

/* ===================================================================
 *   Fits File  Create
 * ===================================================================*/
int hxdcaldbUtil_bstidt_create_FITS( char* bstidt_fits_fname ){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;

#define HXDBSTIDT_NKEYWORD 33
  char *ttype[HXDBSTIDT_NKEYWORD] = {
    "TRG_ID", "BURST_ID", "TRN_ID","YYYYMMDD", "HHMMSS", "CLASSIFICATION", 
    "TRG_SRC", "TRG_SRC_SOFT",
    "FRZN_YYYYMMDD", "FRZN_HHMMSS", "FRZN_AETIME",
    "SIMUL_SA", "PEAK_CNTRATE", "EULER", 
    "INCI_THETA","INCI_PHI","INCI_RA","INCI_DEC","INCI_ORIGIN",
    "SAT_ALT","SAT_LON","SAT_LAT",
    "TIME_MODE", "FRZON_GBREAD", 
    "FRZON_AETIME", "FRZON_TI", "FRZON_T_LATCH_TI", "FRZON_ACU_T_LATCH",
    "FRZOF_GBREAD", "FRZOF_AETIME", 
    "VALID_YYYYMMDD", "VALID_HHMMSS", "BSTID_REV"
  };

  char *tform[HXDBSTIDT_NKEYWORD] = {
    "1J", "64A", "64A", "1J", "1J", "1J", 
    "5X", "4X", 
    "4J", "4J", "4D", 
    "32X", "4J", "3D", 
    "1D", "1D", "1D", "1D", "64A", 
    "1E", "1E", "1E", 
    "4J", "1J", 
    "4D", "4J", "1J", "1J", 
    "1J", "4D", 
    "1J", "1J", "1J"
  };

  char *tunit[HXDBSTIDT_NKEYWORD] = {
    "", "", "", "", "", "",
    "", "", 
    "", "", "s", 
    "", "count/s", "deg",
    "deg","deg","deg","deg","",
    "km","deg","deg",
    "", "", 
    "", "", "", "", 
    "", "s", 
    "", "", ""
  };

  char *keyword[HXDBSTIDT_NKEYWORD] = {
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  ", 
    "TTYPE6  ", "TTYPE7  ", "TTYPE8  ", "TTYPE9  ", "TTYPE10 ",
    "TTYPE11 ", "TTYPE12 ", "TTYPE13 ", "TTYPE14 ", "TTYPE15 ", 
    "TTYPE16 ", "TTYPE17 ", "TTYPE18 ", "TTYPE19 ", "TTYPE20 ",
    "TTYPE21 ", "TTYPE22 ", "TTYPE23 ", "TTYPE24 ", "TTYPE25 ",
    "TTYPE26 ", "TTYPE27 ", "TTYPE28 ", "TTYPE29 ", "TTYPE30 ",
    "TTYPE31 ", "TTYPE32 ", "TTYPE33 "
   
  };
  char *comment[HXDBSTIDT_NKEYWORD] = {
    "Trigger ID of HXD-WAM Burst data", 
    "BURST_ID in HXD BST FITS",
    "Corresponding ID of HXD-WAM Trn Data",
    "Date for the event [yyyy/mm/dd],                                      which is one of FRZN_DATE",
    "Time for the event [hh:mm:ss],                                        which is one of FRZN_TIME",
    "Event classification of BST data                                      [0x00:unID, 0x01:confirmed gamma-ray burst,                            0x02:possible  gamma-ray burst,                                       0x03:solar flare,                                                     0x04:soft gamma-ray repeaters,                                        0x05:particle event, 0x06:noise, 0x07:SAA,                            0x08:others, 0xFF:not classified yet]",
    "trigger source of BST data                                            [1:HXD-DE on-board software, 2:WAM-3, 3:WAM-2,                         4:WAM-1, 5:WAM-0]",
    "trigger mode of on-board software                                     [1:TPU Trigger Mode, 2:PH Monitor Mode,                                3:WANTI (Well Slow LD) Monitor Mode,                                  4:Pseudo Mode]", 
    "date of bst freeze time [yyyy/mm/dd]                                  for WAM-n (n=0,1,2,3)", 
    "time of bst freeze time [hh:mm:ss]                                    for WAM-n (n=0,1,2,3)", 
    "spacecraft time at bst freeze time                                    for WAM-n (n=0,1,2,3)", 
    "Satellites simultaneously detected                                    [0:nothing, 1:HETE-2, 2:Swift BAT,                                     3:Konus Wind, 4:Ulysses, 5:Messenger,                                 6:Mars Odyssey, 7:INTEGRAL SPI-ACS,                                   8:INTEGRAL IBIS, 9:AGILE, 10:GLAST, 11:Goes,                          12:RHESSI, 13:Trace, 14:Hinode,                                       15-32:others reserved]. This information is provided by Interplanetary Network (Kevin Hurley, http://ssl.berkeley.edu/ipn3/index.html and http://heasarc.gsfc.nasa.gov/W3Browse/gamma-ray-bursts/ipngrb.html.)", 
    "Peak count rate in the whole energy band                              (TH 0 to 3) without dead time correction,                              [0:WAM-0, 1:WAM-1, 2:WAM-2, 3:WAM-3]",
    "Satellite Z-Y-Z euler angles [deg]",
    "Incident angle, 90.0-Elevation (deg)",
    "Incident angle, Azimuths (deg)",
    "Object Position, R.A (deg)",
    "Object Position, DEC (deg)",
    "Origin of incident information, INCI_THETA,                           and INCI_PHI. (GCN ID, ATEL ID etc)",
    "altitude of satellite orbit from earth (km)",
    "longitude of satellite orbit (deg)",
    "latitude of satellite orbit (deg)",
    "TPU Time mode for WAM-n (n=0,1,2,3).                                  [0x00:1/2 sec, 0x01:1 sec, 0x02:2 sec,                                 0x03:4 sec]", 
    "HXD_GB_RD_CNT at HXD HK FITS, SYS extension,                          when GB freeze flag is turned on",
    "spacecraft time of TRN FITS, when GB freeze                           flag for WAM-n (n=0,1,2,3) is turned on",
    "TI of TRN FITS, when GB freeze flag for WAM-n                         (n=0,1,2,3) is turned on",
    "HXD_AE_TM_LATCH_TM at HXD HK FITS, HK                                 extension, when GB freeze flag is turned on",
    "HXD_TLATCH_TIME at HXD HK FITS, SYS extension,                        when GB freeze flag is turned on",
    "HXD_GB_RD_CNT at HXD HK FITS, SYS extension,                          when GB freeze flag is turned off",
    "spacecraft time of TRN FITS, when GB freeze                           flag for WAM-n (n=0,1,2,3) is turned off",
    "date of final access of the row [yyyy/mm/dd]",
    "time of final access of the row [hh:mm:ss]",
    "revision of the row [0: created, 1, 2, 3, ...]"
  };

  int ncol = sizeof(ttype)/sizeof(*ttype);
  int bitpix = 8;
  int naxis = 0;
  long nrow=0;
  char detname[16] = "WAM_ANTI";
  char code_name[16] = "BURST_ID_TABLE";
  char extention_name[]="BST_IDT";
  char description[81] = "ID table of HXD-WAM Burst data";
  int use_boundary = 0;
  char *boundary = "";

  if (fits_create_file(&hxd_bstidt_fits_fp, bstidt_fits_fname, &istat)) {
    fprintf(stderr, "%s:fits_create_file %s failed (%d)\n", tool_name, 
            bstidt_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  } 

  if(fits_create_img(hxd_bstidt_fits_fp, bitpix, naxis, &nrow, &istat)) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if( fits_create_tbl(hxd_bstidt_fits_fp, BINARY_TBL, nrow, ncol,
                      ttype, tform, tunit, extention_name, &istat) ){
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (hxdcaldbUtil_write_fits_header(hxd_bstidt_fits_fp, 
                                     detname, code_name, description,
                                     bstidt_fits_fname,
                                     use_boundary, boundary)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxdFitsComment_write(hxd_bstidt_fits_fp,HXDBSTIDT_NKEYWORD,
                       keyword, comment, &istat);
  if (istat){
    fprintf(stderr, "%s: fits write comment failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if(fits_flush_file(hxd_bstidt_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int hxdcaldbUtil_bstidt_write_row_FITS (int irow, HxdBstidt_Tbl *bstidt_tbl){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
  int colnum;
  long firstelem = 1;
  long nelements = 1;
  enum{
    HXDBSTIDT_TRG_ID = 1, 
      HXDBSTIDT_BURST_ID, 
      HXDBSTIDT_TRN_ID, 
      HXDBSTIDT_YYYYMMDD, 
      HXDBSTIDT_HHMMSS, 
      HXDBSTIDT_CLASSIFICATION, 
      HXDBSTIDT_TRG_SRC, 
      HXDBSTIDT_TRG_SRC_SOFT,
      HXDBSTIDT_FRZN_YYYYMMDD, 
      HXDBSTIDT_FRZN_HHMMSS, 
      HXDBSTIDT_FRZN_AETIME,
      HXDBSTIDT_SIMUL_SA, 
      HXDBSTIDT_PEAK_CNTRATE,
      HXDBSTIDT_EULER,
      HXDBSTIDT_INCI_THETA,
      HXDBSTIDT_INCI_PHI,
      HXDBSTIDT_INCI_RA,
      HXDBSTIDT_INCI_DEC,
      HXDBSTIDT_INCI_ORIGIN,
      HXDBSTIDT_SAT_ALT,
      HXDBSTIDT_SAT_LON,
      HXDBSTIDT_SAT_LAT,
      HXDBSTIDT_TIME_MODE, 
      HXDBSTIDT_FRZON_GBREAD, 
      HXDBSTIDT_FRZON_AETIME, 
      HXDBSTIDT_FRZON_TI, 
      HXDBSTIDT_FRZON_T_LATCH_TI, 
      HXDBSTIDT_FRZON_ACU_T_LATCH,
      HXDBSTIDT_FRZOF_GBREAD, 
      HXDBSTIDT_FRZOF_AETIME, 
      HXDBSTIDT_VALID_YYYYMMDD, 
      HXDBSTIDT_VALID_HHMMSS, 
      HXDBSTIDT_BSTID_REV
  }; /** FOR COLUMN NUMBER**/

  int bitpos;
  char larray[32]; /** for 32X format **/
  char *fpp[1];
  float float_sat_alt,float_sat_lon,float_sat_lat;

  /** (w) trigger id **/
  colnum = HXDBSTIDT_TRG_ID;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->trg_id, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int trg_id failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*
  colnum = HXDBSTIDT_BURST_ID;
  fits_write_col_str(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->burst_id, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_str burst_id failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  */

  colnum = HXDBSTIDT_TRN_ID;
  nelements = 1;
  fpp[0] = bstidt_tbl->trn_id;
  fits_write_col_str(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     fpp, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_str trn_id failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) event date, from FRZN_DATE **/
  colnum = HXDBSTIDT_YYYYMMDD;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->yyyymmdd, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int yyyymmdd failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) event time, from FRZN_TIME **/
  colnum = HXDBSTIDT_HHMMSS;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->hhmmss, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int hhmmss failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) event classification **/
  colnum = HXDBSTIDT_CLASSIFICATION;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->classification, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int classification failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (t) trigger source **/
  colnum = HXDBSTIDT_TRG_SRC;
  nelements = 5; /* 5X*/
  for(bitpos=0; bitpos<5; bitpos++)
    larray[4-bitpos] = (bstidt_tbl->trg_src >> bitpos) & 0x1;
  fits_write_col_bit(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, &larray[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_bit trg_src failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (b) trigger source, soft **/
  colnum = HXDBSTIDT_TRG_SRC_SOFT;
  nelements = 4; /* 4X*/
  for(bitpos=0; bitpos<4; bitpos++)
    larray[3-bitpos] = (bstidt_tbl->trg_src_soft >> bitpos) & 0x1;
  fits_write_col_bit(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, &larray[0], &istat);

  if(istat){
    fprintf(stderr, "%s:fits_write_col_bit trg_src_soft failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (b) frzd time (yyyy/mm/dd)**/
  colnum = HXDBSTIDT_FRZN_YYYYMMDD;
  nelements = 4;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->frzd_yyyymmdd[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int frzd_yyyymmdd failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (b) frzd time (hh:mm:ss)**/
  colnum = HXDBSTIDT_FRZN_HHMMSS;
  nelements = 4;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->frzd_hhmmss[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int frzd_hhmmss failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (b) frzd time (aetime)    **/
  colnum = HXDBSTIDT_FRZN_AETIME;
  nelements = 4;
  fits_write_col_dbl(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->frzd_aetime[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl frzd_aetime failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) satellites simultaneous observation **/
  colnum = HXDBSTIDT_SIMUL_SA;
  nelements = 32; /* 16X*/
  for(bitpos=0; bitpos<32; bitpos++)
    larray[31-bitpos] = (bstidt_tbl->simul_sa >> bitpos) & 0x1;
  fits_write_col_bit(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, &larray[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_bit simul_sa failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
      return status;
  }

  /** (w) raw peak count **/
  colnum = HXDBSTIDT_PEAK_CNTRATE;
  nelements = HXDBSTIDT_TPU_N_BOARD;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->peak_countrate[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int peak_countrate failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) Euler angle of Suzaku **/
  colnum = HXDBSTIDT_EULER;
  nelements = HXDBSTIDT_N_EULER;
  fits_write_col_dbl(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->euler_angle[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl euler_angle failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) incident theta **/
  colnum = HXDBSTIDT_INCI_THETA;
  nelements = 1;
  fits_write_col_dbl(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->inci_theta, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl INCI_THETA failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) incident phi **/
  colnum = HXDBSTIDT_INCI_PHI;
  nelements = 1;
  fits_write_col_dbl(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->inci_phi, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl INCI_PHI failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) incident R.A. **/
  colnum = HXDBSTIDT_INCI_RA;
  nelements = 1;
  fits_write_col_dbl(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->inci_ra, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl INCI_RA failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) incident DEC **/
  colnum = HXDBSTIDT_INCI_DEC;
  nelements = 1;
  fits_write_col_dbl(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->inci_dec, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl INCI_DEC failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  colnum = HXDBSTIDT_INCI_ORIGIN;
  nelements = 1;
  fpp[0] = bstidt_tbl->inci_origin;
  fits_write_col_str(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     fpp, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_str inci_origin failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }


  /** (w) Satellite altitude  **/
  colnum = HXDBSTIDT_SAT_ALT;
  nelements = 1;
  float_sat_alt = (float) bstidt_tbl->sat_alt;
  fits_write_col_flt(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &float_sat_alt, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_flt SAT_ALT failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) Satellite longitude **/
  colnum = HXDBSTIDT_SAT_LON;
  nelements = 1;
  float_sat_lon = (float) bstidt_tbl->sat_lon;
  fits_write_col_flt(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &float_sat_lon, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl SAT_LON failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) Satellite latitude **/
  colnum = HXDBSTIDT_SAT_LAT;
  nelements = 1;
  float_sat_lat = (float) bstidt_tbl->sat_lat;
  fits_write_col_flt(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &float_sat_lat, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl SAT_LAT failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (t) time mode **/
  colnum = HXDBSTIDT_TIME_MODE;
  nelements = 4;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->time_mode[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int time_mode failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }


  /** (t) GB READ count **/
  colnum = HXDBSTIDT_FRZON_GBREAD;
  nelements = 1;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->frzon_gbread, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int frzon_gbreadfailed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }


  /** (t) aetime    @ freeze on **/
  colnum = HXDBSTIDT_FRZON_AETIME;
  nelements = 4;
  fits_write_col_dbl(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->frzon_aetime[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl frzon_aetime failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (t) TI        @ freeze on **/
  colnum = HXDBSTIDT_FRZON_TI;
  nelements = 4;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->frzon_ti[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int frzon_ti failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (t) T_LATCH_TI@ freeze on **/
  colnum = HXDBSTIDT_FRZON_T_LATCH_TI;
  nelements = 1;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->frzon_t_latch_ti, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int frzon_t_latch_ti failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (t) T_LATCH   @ freeze on **/
  colnum = HXDBSTIDT_FRZON_ACU_T_LATCH;
  nelements = 1;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->frzon_acu_t_latch, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int frzon_acu_t_latch failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (t) GB READ count **/
  colnum = HXDBSTIDT_FRZOF_GBREAD;
  nelements = 1;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->frzof_gbread, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int frzof_gbread failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (t) aetime    @ freeze off **/
  colnum = HXDBSTIDT_FRZOF_AETIME;
  nelements = 4;
  fits_write_col_dbl(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->frzof_aetime[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl frzof_aetime failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** file name of BST **/
  colnum = HXDBSTIDT_BURST_ID;
  if(bstidt_tbl->burst_id == NULL){
    fprintf(stderr, "%s: write irow=%s, BURST_ID null pointer\n",
	    tool_name, irow);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  nelements = 1;
  fpp[0]=bstidt_tbl->burst_id;
  fits_write_col_str(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     fpp, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_str burst_id failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** date on final access (yyyy/mm/dd) **/
  colnum = HXDBSTIDT_VALID_YYYYMMDD;
  nelements = 1;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->valid_yyyymmdd, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int valid_yyyymmdd failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** time on final access (hh:mm:ss)   **/
  colnum = HXDBSTIDT_VALID_HHMMSS;
  nelements = 1;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->valid_hhmmss, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int valid_hhmmss failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** revision of this row (0=created, 1,2,3,...) **/
  colnum = HXDBSTIDT_BSTID_REV;
  nelements = 1;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstidt_tbl->bstid_revision, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int bstid_revision failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int hxdcaldbUtil_bstidt_write_FITS ( HxdBstidt *bstidt_data ){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
  int irow;
  char detname[16] = "WAM_ANTI"; /** pri header **/

  for(irow=1; irow<=bstidt_data->n_row; irow++){
    status = hxdcaldbUtil_bstidt_write_row_FITS(irow, 
						&bstidt_data->data[irow-1]);
    if(status == HXDCALDBUTIL_STATUS_NG){
      fprintf(stderr, "%s: bstidt wite fits failed (row=%d)\n", 
	      tool_name, irow);
      return status;
    }
  } 

  if (fits_modify_key_lng(hxd_bstidt_fits_fp, "NAXIS2", 
                          bstidt_data->n_row, "&", &istat)){
    fprintf(stderr, "%s:fits_update_key NAXIS2 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
 
  fits_write_key_log(hxd_bstidt_fits_fp, "CLOCKAPP", 1, "If Clock corrections are applied (F/T)", &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_key_log CLCOKAPP failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_dbl(hxd_bstidt_fits_fp, "MJDREFF", 0.00074287037037037, 13, "Reference MJD, fractional part", &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_key_flt MJDREFF failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  fits_write_key_lng(hxd_bstidt_fits_fp, "MJDREFI", 51544, "Reference MJD, Integer part", &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_key_int MJDREFI failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  fits_write_key_str(hxd_bstidt_fits_fp,"TIMESYS","TT","Time system", &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_key_str TIMESYS failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }


  if (fits_write_date(hxd_bstidt_fits_fp, &istat)) {
    fprintf(stderr,"%s: fits_write_date failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if (fits_write_chksum(hxd_bstidt_fits_fp, &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if(fits_flush_file(hxd_bstidt_fits_fp, &istat)){
    fprintf(stderr, "%s: fits_flush_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /******** Add Primary Header keywords ********/
  if (hxdcaldbUtil_write_fits_priheader(hxd_bstidt_fits_fp, detname)
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
int hxdcaldbUtil_bstidt_open_FITS ( char* bstidt_fits_fname ){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;

  if (fits_open_file(&hxd_bstidt_fits_fp, bstidt_fits_fname, READONLY,
                     &istat)) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
            bstidt_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  return status;
}

int hxdcaldbUtil_bstidt_read_row_FITS (int irow, HxdBstidt_Tbl *bstidt_tbl ){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  int colnum;
  long firstelem = 1;
  long nelements = 1;
  int    int_nulval = 0;
  double dbl_nulval = 0.0;
  char*  chr_p_nulval= NULL;

  int anynul;
  enum{
    HXDBSTIDT_TRG_ID = 1, 
      HXDBSTIDT_BURST_ID, 
      HXDBSTIDT_TRN_ID, 
      HXDBSTIDT_YYYYMMDD, 
      HXDBSTIDT_HHMMSS, 
      HXDBSTIDT_CLASSIFICATION, 
      HXDBSTIDT_TRG_SRC, 
      HXDBSTIDT_TRG_SRC_SOFT,
      HXDBSTIDT_FRZN_YYYYMMDD, 
      HXDBSTIDT_FRZN_HHMMSS, 
      HXDBSTIDT_FRZN_AETIME,
      HXDBSTIDT_SIMUL_SA, 
      HXDBSTIDT_PEAK_CNTRATE,
      HXDBSTIDT_EULER,
      HXDBSTIDT_INCI_THETA,
      HXDBSTIDT_INCI_PHI,
      HXDBSTIDT_INCI_RA,
      HXDBSTIDT_INCI_DEC,
      HXDBSTIDT_INCI_ORIGIN,
      HXDBSTIDT_SAT_ALT,
      HXDBSTIDT_SAT_LON,
      HXDBSTIDT_SAT_LAT,
      HXDBSTIDT_TIME_MODE, 
      HXDBSTIDT_FRZON_GBREAD, 
      HXDBSTIDT_FRZON_AETIME, 
      HXDBSTIDT_FRZON_TI, 
      HXDBSTIDT_FRZON_T_LATCH_TI, 
      HXDBSTIDT_FRZON_ACU_T_LATCH,
      HXDBSTIDT_FRZOF_GBREAD, 
      HXDBSTIDT_FRZOF_AETIME, 
      HXDBSTIDT_VALID_YYYYMMDD, 
      HXDBSTIDT_VALID_HHMMSS, 
      HXDBSTIDT_BSTID_REV
  }; /** FOR COLUMN NUMBER**/

  int bitpos;
  char larray[32]; /** for 32X format **/
  char *fpp[1];
  float float_sat_alt,float_sat_lon,float_sat_lat;

  /** (w) trigger id **/
  colnum = HXDBSTIDT_TRG_ID;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->trg_id, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int trg_id failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) burst id **/
  /*
  colnum = HXDBSTIDT_BURST_ID;
  nelements = 1;
  fpp[0] = (char *) malloc(sizeof(char)*HXDBSTIDT_MAX_CHAR_LNGTH);
  fits_read_col_str(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, chr_p_nulval, 
		    fpp, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_str burst_id failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  sprintf(bstidt_tbl->burst_id, "%s", fpp[0]);
  free(fpp[0]);
  */

  /** (w) trn id **/
  colnum = HXDBSTIDT_TRN_ID;
  nelements = 1;
  fpp[0] = (char *) malloc(sizeof(char)*HXDBSTIDT_MAX_CHAR_LNGTH);
  fits_read_col_str(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, chr_p_nulval, 
		    fpp, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_str burst_id failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  sprintf(bstidt_tbl->trn_id, "%s", fpp[0]);
  free(fpp[0]);
  
  /** (w) event date, from FRZN_DATE **/
  colnum = HXDBSTIDT_YYYYMMDD;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->yyyymmdd, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int yyyymmdd failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (w) event time, from FRZN_TIME **/
  colnum = HXDBSTIDT_HHMMSS;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->hhmmss, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int hhmmss failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (w) event classification **/
  colnum = HXDBSTIDT_CLASSIFICATION;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->classification, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int classification failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (t) trigger source **/
  colnum = HXDBSTIDT_TRG_SRC;
  nelements = 5; /* 5X*/
  fits_read_col_bit(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, &larray[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_bit trg_src failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  bstidt_tbl->trg_src = 0x00;
  for(bitpos=0; bitpos<5; bitpos++)
    bstidt_tbl->trg_src  |= ( (larray[bitpos]) << (4-bitpos) );
  
  /** (b) trigger source, soft **/
  colnum = HXDBSTIDT_TRG_SRC_SOFT;
  nelements = 4; /* 4X*/
  fits_read_col_bit(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, &larray[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_bit trg_src_soft failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  bstidt_tbl->trg_src_soft = 0x00;
  for(bitpos=0; bitpos<4; bitpos++)
    bstidt_tbl->trg_src_soft  |= ( (larray[bitpos]) << (3-bitpos) );
  
  /** (b) frzd time (yyyy/mm/dd)**/
  colnum = HXDBSTIDT_FRZN_YYYYMMDD;
  nelements = HXDBSTIDT_TPU_N_BOARD;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->frzd_yyyymmdd[0], &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int frzd_yyyymmdd failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (b) frzd time (hh:mm:ss)**/
  colnum = HXDBSTIDT_FRZN_HHMMSS;
  nelements = 4;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->frzd_hhmmss[0], &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int frzd_hhmmss failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (b) frzd time (aetime)    **/
  colnum = HXDBSTIDT_FRZN_AETIME;
  nelements = HXDBSTIDT_TPU_N_BOARD;
  fits_read_col_dbl(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, dbl_nulval, 
		    &bstidt_tbl->frzd_aetime[0], &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl frzd_aetime failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (w) satellites simultaneous observation **/
  colnum = HXDBSTIDT_SIMUL_SA;
  nelements = 32; /* 32X*/
  fits_read_col_bit(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, &larray[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_bit simul_sa failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  bstidt_tbl->simul_sa = 0x00;
  for(bitpos=0; bitpos<32; bitpos++)
    bstidt_tbl->simul_sa   |= ( (larray[bitpos]) << (31-bitpos) );

  /** (w) raw peak count rate**/
  colnum = HXDBSTIDT_PEAK_CNTRATE;
  nelements = HXDBSTIDT_TPU_N_BOARD;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->peak_countrate[0], &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int peak_countrate failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) Euler angle of Suzaku **/
  colnum = HXDBSTIDT_EULER;
  nelements = HXDBSTIDT_N_EULER;
  fits_read_col_dbl(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, dbl_nulval, 
		    &bstidt_tbl->euler_angle[0], &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl euler_angle failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (w) incident theta **/
  colnum = HXDBSTIDT_INCI_THETA;
  nelements = 1;
  fits_read_col_dbl(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, dbl_nulval, 
		    &bstidt_tbl->inci_theta, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl INCI_THETA failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (w) incident phi **/
  colnum = HXDBSTIDT_INCI_PHI;
  nelements = 1;
  fits_read_col_dbl(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, dbl_nulval, 
		    &bstidt_tbl->inci_phi, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl INCI_PHI failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (w) incident R.A. **/
  colnum = HXDBSTIDT_INCI_RA;
  nelements = 1;
  fits_read_col_dbl(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, dbl_nulval, 
		    &bstidt_tbl->inci_ra, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl INCI_RA failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (w) incident DEC **/
  colnum = HXDBSTIDT_INCI_DEC;
  nelements = 1;
  fits_read_col_dbl(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, dbl_nulval, 
		    &bstidt_tbl->inci_dec, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl INCI_DEC failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (w) incident origin **/
  colnum = HXDBSTIDT_INCI_ORIGIN;
  nelements = 1;
  fpp[0] = (char *) malloc(sizeof(char)*HXDBSTIDT_MAX_CHAR_LNGTH);
  fits_read_col_str(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, chr_p_nulval, 
		    fpp, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_str inci_origin failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  sprintf(bstidt_tbl->inci_origin, "%s", fpp[0]);
  free(fpp[0]);
  
  /** (w) Satellite altitude  **/
  colnum = HXDBSTIDT_SAT_ALT;
  nelements = 1;
  fits_read_col_flt(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, dbl_nulval, 
		    &float_sat_alt, &anynul, &istat);
  bstidt_tbl->sat_alt = (double) float_sat_alt;
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl SAT_ALT failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (w) Satellite longitude **/
  colnum = HXDBSTIDT_SAT_LON;
  nelements = 1;
  fits_read_col_flt(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, dbl_nulval, 
		    &float_sat_lon, &anynul, &istat);
  bstidt_tbl->sat_lon = (double) float_sat_lon;
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl SAT_LON failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (w) Satellite latitude **/
  colnum = HXDBSTIDT_SAT_LAT;
  nelements = 1;
  fits_read_col_flt(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, dbl_nulval, 
		    &float_sat_lat, &anynul, &istat);
  bstidt_tbl->sat_lat = float_sat_lat;
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl SAT_LAT failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /** (t) time mode **/
  colnum = HXDBSTIDT_TIME_MODE;
  nelements = 4;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->time_mode[0], &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int time_mode failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (t) GB READ count **/
  colnum = HXDBSTIDT_FRZON_GBREAD;
  nelements = 1;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->frzon_gbread, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int frzon_gbreadfailed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (t) aetime    @ freeze on **/
  colnum = HXDBSTIDT_FRZON_AETIME;
  nelements = 4;
  fits_read_col_dbl(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, dbl_nulval, 
		    &bstidt_tbl->frzon_aetime[0], &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl frzon_aetime failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (t) TI        @ freeze on **/
  colnum = HXDBSTIDT_FRZON_TI;
  nelements = 4;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->frzon_ti[0], &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int frzon_ti failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (t) T_LATCH_TI@ freeze on **/
  colnum = HXDBSTIDT_FRZON_T_LATCH_TI;
  nelements = 1;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->frzon_t_latch_ti, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int frzon_t_latch_ti failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (t) T_LATCH   @ freeze on **/
  colnum = HXDBSTIDT_FRZON_ACU_T_LATCH;
  nelements = 1;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->frzon_acu_t_latch, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int frzon_acu_t_latch failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (t) GB READ count **/
  colnum = HXDBSTIDT_FRZOF_GBREAD;
  nelements = 1;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->frzof_gbread, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int frzof_gbread failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** (t) aetime    @ freeze off **/
  colnum = HXDBSTIDT_FRZOF_AETIME;
  nelements = 4;
  fits_read_col_dbl(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, dbl_nulval, 
		    &bstidt_tbl->frzof_aetime[0], &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_dbl frzof_aetime failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** file name of BST **/
  colnum = HXDBSTIDT_BURST_ID;
  nelements = 1;
  fpp[0] = (char *) malloc(sizeof(char)*HXDBSTIDT_MAX_CHAR_LNGTH);
  fits_read_col_str(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, chr_p_nulval, 
		    fpp, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_str burst_id failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  sprintf(bstidt_tbl->burst_id, "%s", fpp[0]);
  free (fpp[0]);
  
  /** date on final access (yyyy/mm/dd) **/
  colnum = HXDBSTIDT_VALID_YYYYMMDD;
  nelements = 1;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->valid_yyyymmdd, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int valid_yyyymmdd failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** time on final access (hh:mm:ss)   **/
  colnum = HXDBSTIDT_VALID_HHMMSS;
  nelements = 1;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->valid_hhmmss, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int valid_hhmmss failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  /** revision of this row (0=created, 1,2,3,...) **/
  colnum = HXDBSTIDT_BSTID_REV;
  nelements = 1;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstidt_tbl->bstid_revision, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int bstid_revision failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}


int hxdcaldbUtil_bstidt_read_FITS ( HxdBstidt *bstidt_data ){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  long irow, nrow;
  char comment[80];
  int hdutype;

  /*** move to BSTIDT extension ***/
  fits_movabs_hdu(hxd_bstidt_fits_fp, HXDBSTIDT_IDTABLE_HDU_ID,
                  &hdutype,&istat);
  if(istat){
    fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
            tool_name, HXDBSTIDT_IDTABLE_HDU_ID, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** read nrow (number of rows) ***/
  fits_read_key_lng(hxd_bstidt_fits_fp, "NAXIS2", &nrow, 
                    comment, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_key NAXIS2 failed (%d)\n", 
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  bstidt_data->n_row = nrow;

  for (irow = 1; irow <= nrow; irow ++){
    status = hxdcaldbUtil_bstidt_read_row_FITS(irow, 
					       &bstidt_data->data[irow-1]);
    if(status == HXDCALDBUTIL_STATUS_NG){
      fprintf(stderr, "%s: bstidt read fits failed (row=%d)\n", 
              tool_name, irow);
      return status;
    }
  }

  return status;
}

int hxdcaldbUtil_bstidt_close_FITS( void ){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat = 0;
  
  if(fits_close_file(hxd_bstidt_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;
}

/* ===================================================================
 *   Utilities 
 * ===================================================================*/

int
hxdcaldbUtil_bstidt_init_a_bstidtbl(HxdBstidt_Tbl *bstidt){
  int status = HXDCALDBUTIL_STATUS_OK;
  int board;

  bstidt->trg_id    = HXDBSTIDT_UNDEF_TRGID;
  sprintf(bstidt->burst_id,"Not Assigned");
  sprintf(bstidt->trn_id,  "Not Assigned");
  bstidt->yyyymmdd = HXDBSTIDT_UNDEF_YYYYMMDD;
  bstidt->hhmmss   = HXDBSTIDT_UNDEF_HHMMSS;
  bstidt->classification    = 0;
  bstidt->trg_src           = 0x00;
  bstidt->trg_src_soft      = 0x00;

  for(board=0; board<4; board++){
    bstidt->frzd_yyyymmdd[board] = HXDBSTIDT_UNDEF_YYYYMMDD;
    bstidt->frzd_hhmmss[board]   = HXDBSTIDT_UNDEF_HHMMSS;
    bstidt->frzd_aetime[board]   = HXDBSTIDT_UNDEF_AETIME;
    bstidt->peak_countrate[board]    = 0.00;
  }

  bstidt->euler_angle[0] = HXDBSYIDT_UNDEF_DBLVAL;
  bstidt->euler_angle[1] = HXDBSYIDT_UNDEF_DBLVAL;
  bstidt->euler_angle[2] = HXDBSYIDT_UNDEF_DBLVAL;

  bstidt->simul_sa   = 0x00;
  bstidt->inci_theta = HXDBSYIDT_UNDEF_DBLVAL;
  bstidt->inci_phi   = HXDBSYIDT_UNDEF_DBLVAL;
  bstidt->inci_ra    = HXDBSYIDT_UNDEF_DBLVAL;
  bstidt->inci_dec   = HXDBSYIDT_UNDEF_DBLVAL;
  sprintf(bstidt->inci_origin, "NONE");
  bstidt->sat_alt    = HXDBSYIDT_UNDEF_DBLVAL;
  bstidt->sat_lon    = HXDBSYIDT_UNDEF_DBLVAL;
  bstidt->sat_lat    = HXDBSYIDT_UNDEF_DBLVAL;

  for(board=0; board<4; board++){
    bstidt->time_mode[board] = HXDBSYIDT_UNDEF_INTVAL;
  }
  bstidt->frzon_gbread       = HXDBSYIDT_UNDEF_INTVAL;

  for(board=0; board<4; board++){
    bstidt->frzon_aetime[board] = HXDBSTIDT_UNDEF_AETIME;
    bstidt->frzon_ti[board]     = HXDBSYIDT_UNDEF_INTVAL;
  } 
  bstidt->frzon_t_latch_ti      = HXDBSYIDT_UNDEF_INTVAL;
  bstidt->frzon_acu_t_latch     = HXDBSYIDT_UNDEF_INTVAL;
  bstidt->frzof_gbread          = HXDBSYIDT_UNDEF_INTVAL;
  for(board=0; board<4; board++){
    bstidt->frzof_aetime[board] = HXDBSTIDT_UNDEF_AETIME;
  }

  bstidt->valid_yyyymmdd        = HXDBSTIDT_UNDEF_YYYYMMDD;
  bstidt->valid_hhmmss          = HXDBSTIDT_UNDEF_HHMMSS;

  bstidt->bstid_revision = 1;

  return status;
}

/* ===================================================================
 *   Utilities for hxdbsttime
 * ===================================================================*/
int
hxdcaldbUtil_bstidt_read_frzonTimeInfo(HxdBstidt *bstidt_data, 
				       int board, double tlm_aetime,
				       double *frzon_aetime, 
				       unsigned int *frzon_telm_time,
				       unsigned int *latch_AE_time,
				       unsigned int *latch_DE_time,
				       int *bsttime_valid,
				       int *bstidt_rowid){
  int status = HXDCALDBUTIL_STATUS_OK;
  int row_id;
  double a_frzon_aetime = 0.0000;

  for (row_id = 1; row_id <= bstidt_data->n_row; row_id ++){
    a_frzon_aetime = bstidt_data->data[row_id-1].frzon_aetime[board];
    if (a_frzon_aetime > tlm_aetime) break;
  }
  row_id --;

  *bstidt_rowid = row_id;
  *frzon_aetime    = bstidt_data->data[row_id-1].frzon_aetime[board];
  *frzon_telm_time = bstidt_data->data[row_id-1].frzon_ti[board];
  *latch_AE_time   = bstidt_data->data[row_id-1].frzon_acu_t_latch;
  *latch_DE_time   = bstidt_data->data[row_id-1].frzon_t_latch_ti;

  if(*frzon_aetime <= TIME_EPSILON){
    *bsttime_valid = HXDBSTTIME_ASGN_INVALID;
  } else {
    *bsttime_valid = HXDBSTTIME_ASGN_VALID;
  }

  return status;
}


int
hxdcaldbUtil_bstidt_update_by_hxdbsttime (char* bstidt_fits_fname,
					 int bstidt_rowid,
					 double *bst_frzd_tm){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  int hdutype;
  int colnum;
/*int irow   = bstidt_rowid -1 ;*/
  int irow   = bstidt_rowid;
  int anynul;
  long firstelem = 1;
  long nelements = 1;
  int  int_nulval = 0;
  int  yyyymmdd[HXDBSTIDT_TPU_N_BOARD];
  int  hhmmss[HXDBSTIDT_TPU_N_BOARD];
  AtTimeD bst_attimed[HXDBSTIDT_TPU_N_BOARD];

  enum{
    HXDBSTIDT_TRG_ID = 1, 
      HXDBSTIDT_BURST_ID, 
      HXDBSTIDT_TRN_ID, 
      HXDBSTIDT_YYYYMMDD, 
      HXDBSTIDT_HHMMSS, 
      HXDBSTIDT_CLASSIFICATION, 
      HXDBSTIDT_TRG_SRC, 
      HXDBSTIDT_TRG_SRC_SOFT,
      HXDBSTIDT_FRZN_YYYYMMDD, 
      HXDBSTIDT_FRZN_HHMMSS, 
      HXDBSTIDT_FRZN_AETIME,
      HXDBSTIDT_SIMUL_SA, 
      HXDBSTIDT_PEAK_CNTRATE,
      HXDBSTIDT_EULER,
      HXDBSTIDT_INCI_THETA,
      HXDBSTIDT_INCI_PHI,
      HXDBSTIDT_INCI_RA,
      HXDBSTIDT_INCI_DEC,
      HXDBSTIDT_INCI_ORIGIN,
      HXDBSTIDT_SAT_ALT,
      HXDBSTIDT_SAT_LON,
      HXDBSTIDT_SAT_LAT,
      HXDBSTIDT_TIME_MODE, 
      HXDBSTIDT_FRZON_GBREAD, 
      HXDBSTIDT_FRZON_AETIME, 
      HXDBSTIDT_FRZON_TI, 
      HXDBSTIDT_FRZON_T_LATCH_TI, 
      HXDBSTIDT_FRZON_ACU_T_LATCH,
      HXDBSTIDT_FRZOF_GBREAD, 
      HXDBSTIDT_FRZOF_AETIME, 
      HXDBSTIDT_VALID_YYYYMMDD, 
      HXDBSTIDT_VALID_HHMMSS, 
      HXDBSTIDT_BSTID_REV
  }; /** FOR COLUMN NUMBER**/

  int bstid_revision;
  int board;

  /*** (1) Open FITS ***/
  if (fits_open_file(&hxd_bstidt_fits_fp, bstidt_fits_fname, READWRITE,
                     &istat)) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
            bstidt_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** (2) move to BSTIDT extension ***/
  fits_movabs_hdu(hxd_bstidt_fits_fp, HXDBSTIDT_IDTABLE_HDU_ID,
                  &hdutype, &istat);
  if(istat){
    fprintf(stderr, "%s: fits_movabs_hdu %d failed (%d)\n", 
            tool_name, HXDBSTIDT_IDTABLE_HDU_ID, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** (3) read version of the row ***/
  colnum = HXDBSTIDT_BSTID_REV;
  nelements = 1;
  fits_read_col_int(hxd_bstidt_fits_fp, colnum, 
		    irow, firstelem, nelements, int_nulval, 
		    &bstid_revision, &anynul, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_read_col_int bstid_revision failed (%d)\n",
            tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** (4) write version of the row ***/
  bstid_revision ++;
  colnum = HXDBSTIDT_BSTID_REV;
  nelements = 1;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &bstid_revision, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int bstid_revision failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** (5) write frzn_time of the row ***/
  colnum = HXDBSTIDT_FRZN_AETIME;
  nelements = 4;
  fits_write_col_dbl(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     bst_frzd_tm, &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_dbl frzd_aetime failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }


  for (board=0; board<HXDBSTIDT_TPU_N_BOARD; board++){
    aste2attimeD(bst_frzd_tm[board], &bst_attimed[board]);
    yyyymmdd[board]  = bst_attimed[board].yr * 10000;
    yyyymmdd[board] += bst_attimed[board].mo * 100;
    yyyymmdd[board] += bst_attimed[board].dy;
    hhmmss[board]    = bst_attimed[board].hr * 10000;
    hhmmss[board]   += bst_attimed[board].mn * 100;
    hhmmss[board]   += bst_attimed[board].sc;
  }

  colnum = HXDBSTIDT_FRZN_YYYYMMDD;
  nelements = 4;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &yyyymmdd[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int frzd_yyyymmdd failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  colnum = HXDBSTIDT_FRZN_HHMMSS;
  nelements = 4;
  fits_write_col_int(hxd_bstidt_fits_fp, colnum, 
		     irow, firstelem, nelements, 
		     &hhmmss[0], &istat);
  if(istat){
    fprintf(stderr, "%s:fits_write_col_int frzd_hhmmss failed (%d)\n",
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** (6) update checksum ***/
  fits_write_chksum(hxd_bstidt_fits_fp, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*** (7) close FITS ***/
  if(fits_close_file(hxd_bstidt_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  return status;
}
