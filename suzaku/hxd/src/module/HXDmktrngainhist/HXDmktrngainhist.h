#ifndef _HXD_TRN_GAIN_HIST_UTIL_H_
#define _HXD_TRN_GAIN_HIST_UTIL_H_

#define HXD_TPU_N_BOARD 4
#define HXD_ANTI_N_UNIT 5

#define TRN_EXPOSURE_TIME 1000
#define TRN_EXPOSURE_COUNTS 10000

#define STATUS_ADD    1
#define STATUS_CREATE 2 

#define HXD_GAIN_HIST_KEY_NUM 20

enum {
  TIME, YYYYMMDD, HHMMSS, EXPOSURE_TIME, 
  GAIN0, GAIN_ERROR0,  OFFSET0, OFFSET_ERROR0,
  GAIN1, GAIN_ERROR1,  OFFSET1, OFFSET_ERROR1,
  GAIN2, GAIN_ERROR2,  OFFSET2, OFFSET_ERROR2,
  GAIN3, GAIN_ERROR3,  OFFSET3, OFFSET_ERROR3
};

static char *hxd_trn_gainhistory_keyword[] = {
  "TIME", "YYYYMMDD" ,"HHMMSS", "EXPOSURE_TIME",
  "TRN_GAIN0", "TRN_GAIN_ERROR0",
  "TRN_OFFSET0", "TRN_OFFSET_ERROR0",
  "TRN_GAIN1", "TRN_GAIN_ERROR1",
  "TRN_OFFSET1", "TRN_OFFSET_ERROR1",
  "TRN_GAIN2", "TRN_GAIN_ERROR2",
  "TRN_OFFSET2", "TRN_OFFSET_ERROR2",
  "TRN_GAIN3", "TRN_GAIN_ERROR3",
  "TRN_OFFSET3", "TRN_OFFSET_ERROR3"
};

static char *hxd_trn_gainhistory_format[] = {
  "1D", "1J", "1J", "1D",
  "5D", "5D", "5D", "5D",
  "5D", "5D", "5D", "5D",
  "5D", "5D", "5D", "5D",
  "5D", "5D", "5D", "5D"
};

static char *hxd_trn_gainhistory_unit[] = {
  "s","","","s",
  "", "", "", "",
  "", "", "", "",
  "", "", "", "",
  "", "", "", ""
};

char *hxd_trn_gainhistory_comment[] = {
  "Start time",
  "Year, Month, Day",
  "Hour, Minute, Second",
  "Exposure time",
  "Gain on TRN_PHA => TRN_PI  (TPU0)",
  "Error of TRN_gain          (TPU0)",
  "Offset on TRN_PHA => TRN_PI(TPU0)",
  "Error of TRN_offset        (TPU0)",
  "Gain on TRN_PHA => TRN_PI  (TPU1)",
  "Error of TRN_gain          (TPU1)",
  "Offset on TRN_PHA => TRN_PI(TPU1)",
  "Error of TRN_offset        (TPU1)",
  "Gain on TRN_PHA => TRN_PI  (TPU2)",
  "Error of TRN_gain          (TPU2)",
  "Offset on TRN_PHA => TRN_PI(TPU2)",
  "Error of TRN_offset        (TPU2)",
  "Gain on TRN_PHA => TRN_PI  (TPU3)",
  "Error of TRN_gain          (TPU3)",
  "Offset on TRN_PHA => TRN_PI(TPU3)",
  "Error of TRN_offset        (TPU3)"
};

typedef struct {
  double start_time;
  double exposure_time;
  double gain        [HXD_ANTI_N_UNIT];
  double gain_error  [HXD_ANTI_N_UNIT];
  double offset      [HXD_ANTI_N_UNIT];
  double offset_error[HXD_ANTI_N_UNIT];
}HxdTrnGainHist;

typedef struct {
  char * telescope;
  char * instrument;
  char * creator;
  double tstart;
  double tstop;
  float mjdref;
  char * timeref;
  float timesys;
  char * timeunit;
} HxdGainHistKey;

static void HXDmktrngainhist_fits_add_bgnrun( int *istat );
static void HXDmktrngainhist_fits_create_bgnrun( int *istat );
static void HXDmktrngainhist_create_tbl( fitsfile *fp, int *istat );
static void HXDmktrngainhist_col_num(  fitsfile *fp, int *colnum, int *istat );
static void HXDmktrngainhist_col_write(fitsfile *fp, long irow, int *colnum,
			   HxdTrnGainHist *fits, double time, int *istat );
static void HXDmktrngainhist_timeUpdate (HxdGainHistKey *stdkeys,
					 double aetime, int *istat);
static void HXDmktrngainhist_col_null (fitsfile *fp, long irow, int *colnum,
				       double time, int *istat );
static void HXDmktrngainhist_setDefaultKeywordValues(HxdGainHistKey *stdkeys);
static void HXDmktrngainhist_updateStdTimeKeys(fitsfile *fp,
					       HxdGainHistKey stdkeys,
					       int hdunum, int add_flag,
					       int *status);
static void HXDmktrngainhist_writeHXDStdKeys(fitsfile * fp,
					     HxdGainHistKey stdkeys,
					     int hdunum, int *istat);
#endif
