#ifndef _HXD_FITS_HEADER_UTIL_OLD_H_
#define _HXD_FITS_HEADER_UTIL_OLD_H_
#include "aeFitsHeaderUtil.h"

typedef struct {
  char * telescope;
  char * instrument;
  char * detnam;
  int    use_detnam;
  char * object;
  char * obs_id;
  char * observer;
  char * obs_mode;
  char * datamode;
  char * date_obs;
  char * time_obs;
  char * date_end;
  char * time_end;
  double tstart;
  double tstop;
  double ontime;
  double telapse;
  int nevents;
  char * radecsys;
  int equinox;
  int    write_radec; /** 0 in fff **/
  double ra_obj;   /** new **/
  double dec_obj;  /** new **/
  double ra_nom;
  double dec_nom;
  double roll_nom; /** new **/
  double ra_pnt;   /** new **/
  double dec_pnt;  /** new **/
  double mean_ea1;  /** new **/
  double mean_ea2;  /** new **/
  double mean_ea3;  /** new **/
  int   mjd_refi; 
  double mjd_reff;
  char * timeref;
  char * timesys; 
  char * timeunit;
  char * tassign;
  int  clockapp;
  float  timedel;
  float timepixr;
  float tierrela;
  float tierabso;
  char * creator;
  char * origin;
  char * hduclass;
  int  hduclass_ev_hk; /* add */
  char * tlmfile;
  char * timfile;  /* add */
  char * attfile;  /* add */
  char * orbfile;  /* add */
  char * leapfile;
  char * teldef;
  char * credits;
  char * task_name;
  char * task_version;
} HXD_STD_KEYS;

#define HXD_FITS_HEADER_UTIL_HDUCLASS_EVENT   1
#define HXD_FITS_HEADER_UTIL_HDUCLASS_HK      2
#define HXD_FITS_HEADER_UTIL_HDUCLASS_PHA     3
#define HXD_FITS_HEADER_UTIL_HDUCLASS_PHA_PRI 4

#define HXD_FITS_HEADER_UTIL_CREDIT_CREATE    0
#define HXD_FITS_HEADER_UTIL_CREDIT_DUPLICATE 1
#define HXD_FITS_HEADER_UTIL_CREDIT_OVERWRITE 2

#define FITS_KEY_MAX_LENGTH 80
#define CREDITS_MAX_LINES   64

#define HXDFITSHEADER_LINESIZE 256

typedef struct {
  int number;
  char comments[CREDITS_MAX_LINES][256];
} HXD_FITS_HEADER_CREDITS_BUF;

#define HXD_FITS_HEADER_UTIL_OK 1
#define HXD_FITS_HEADER_UTIL_NG 99

/** header size, version 2005-11-09**/
#define AESPECT_HEADER_RESERVE 35
#define HXD_FITS_HEADER_RESERVE_HK_PRI      (120+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_HK       (907+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_ACU      (427+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_SYS      (284+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_SCL      (247+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_PWH      (151+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_RHK      (168+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_STM      (788+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_PPR      (1914+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_PST      (1271+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_AET_HC   (151+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_AET_SC   (575+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_MEM_DMP  (123+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_ECC_DMP  (125+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_IO_DMP   (125+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_RECC_DMP (125+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_RIO_DMP  (125+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_SFC      (155+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_SFF1     (155+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_SFF2     (155+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_DLT      (154+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_SP_PMT   (154+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_HK_SP_PIN   (154+AESPECT_HEADER_RESERVE)
  
#define HXD_FITS_HEADER_RESERVE_WEL_PRI (216+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_WEL_EVT (429+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_WEL_GTI (229+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_TRN_PRI (207+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_TRN_EVT (400+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_TRN_GTI (220+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_BST_PRI (134+AESPECT_HEADER_RESERVE)
#define HXD_FITS_HEADER_RESERVE_BST_EVT (354+AESPECT_HEADER_RESERVE)

void hxdFitsHeader_old_mallocSTDKEYS(HXD_STD_KEYS *stdkeys);
void hxdFitsHeader_old_freeSTDKEYS(HXD_STD_KEYS *stdkeys);

void hxdFitsHeader_old_setDefaultKeywordValues(HXD_STD_KEYS *);

int hxdFitsHeader_old_updateStdTimeKeys(fitsfile*, HXD_STD_KEYS, int hdunum,
				    int *status );

int hxdFitsHeader_old_writeHXDStdKeys (fitsfile*, HXD_STD_KEYS, int hdunum,
				   int *status);

int hxdFitsHeader_old_timeUpdate (HXD_STD_KEYS*, double aetime);

void hxdFitsHeader_old_writeCredits(fitsfile * fp, char * task_name,
				char * task_version, char * credits, 
				int write_mode, int *status);

int hxdFitsHeader_old_writeTIMEDEL(fitsfile * fp, int extension_id, 
			       double timedel_val);

int hxdFitsHeader_old_readTIME(fitsfile* fp,
			   double* tstart, double* tstop,
			   double* telapse, double* ontime);

int hxdFitsHeader_old_writeParamer_wel(fitsfile *fp, int extension_id);
int hxdFitsHeader_old_writeParamer_scl(fitsfile *fp, int extension_id);
int hxdFitsHeader_old_writeParamer_trn(fitsfile *fp, int extension_id);
int hxdFitsHeader_old_writeParamer_bst(fitsfile *fp, int extension_id);
int hxdFitsHeader_old_writeParamer_arf(fitsfile *fp, int extension_id);

static int hxdFitsHeader_old_extruct_credits( char* credits );

#endif
