#ifndef _HXD_TRN_FITS_UTIL_H_
#define _HXD_TRN_FITS_UTIL_H_

#include <fitsio.h>

#define HXD_TRN_FITS_PRIMARY_HDU        1
#define HXD_TRN_FITS_EVENT_EXTENTION    2
#define HXD_TRN_FITS_GTI_EXTENTION      3

#define HXD_TRN_FITS_KEY_NUM 39
#define HXD_TRN_MAX_ENE_BIN 54

#define HXD_GB_TRIG_NBIT 5

typedef struct {
  double time;
  double aetime;
  double s_time;
  int trntime;
  unsigned int ti;
  short trn_iblock;
  short reserve01;
  unsigned char trn_ae_de_length_chk;
  short reserve02;
  char  reserve03;
  unsigned char trn_de_module;
  short reserve04;
  char  reserve05;
  unsigned char trn_de_block;
  short reserve06;
  char  reserve07;
  unsigned char trn_rdbin;
  short reserve08;
  char  reserve09;
  unsigned char trn_tblid;
  short reserve10;
  char  reserve11;
  unsigned char trn_ae_block;
  short reserve12;
  char  reserve13;
  unsigned short trn_data_size;
  short reserve14;
  unsigned char trn_sys_err_code;
  short reserve15;
  char  reserve16;
  int trn_gb_time;
  unsigned char trn_gb_flg;
  short reserve17;
  char  reserve18;
  unsigned char trn_time_mode;
  short reserve19;
  char  reserve20;
  unsigned char trn_rbm;
  short reserve21;
  char  reserve22;
  unsigned char trn_gb_frz;
  short reserve23;
  char  reserve24;
  unsigned char trn_dt_mode;
  short reserve25;
  char  reserve26;
  unsigned char trn_sumld_mode;
  short reserve27;
  char  reserve28;
  unsigned char trn_ae_module;
  short reserve29;
  char  reserve30;
  unsigned char trn_gb_trg;
  short reserve31;
  char  reserve32;
  unsigned short trn_pi[HXD_TRN_MAX_ENE_BIN];
  short reserve33;
  unsigned short trn_ph[HXD_TRN_MAX_ENE_BIN];
  short reserve34;
  unsigned short trn_over_flow;
  short reserve35;
  unsigned short trn_pseudo;
  short reserve36;
  unsigned short trn_t_ant0;
  short reserve37;
  unsigned short trn_t_ant1;
  short reserve38;
  unsigned short trn_t_ant2;
  short reserve39;
  unsigned short trn_t_ant3;
  short reserve40;
  unsigned short trn_t_ant4;
  short reserve41;
  unsigned short trn_ud;
  short reserve42;
  unsigned short trn_dead_time;
  short reserve43;
  unsigned short trn_sum_ld;
  short reserve44;
  unsigned short trn_w_ant0;
  short reserve45;
  unsigned short trn_w_ant1;
  short reserve46;
  unsigned short trn_w_ant2;
  short reserve47;
  unsigned short trn_w_ant3;    
  short reserve48;
  unsigned short trn_quality;
  short reserve49;
/*unsigned char  trn_packtbl_id;*/
} HxdTrnFits;

#define HXD_TRNTIME_MODE_05SEC  0
#define HXD_TRNTIME_MODE_1SEC   1
#define HXD_TRNTIME_MODE_2SEC   2
#define HXD_TRNTIME_MODE_4SEC   3

void hxdtrnFits_create_tbl( fitsfile *fp, int *istat );

void hxdtrnFits_col_write( fitsfile *fp, long irow, int *colnum,
			  HxdTrnFits *fits, int *istat );
/* int colnum[HXD_TRN_FITS_KEY_NUM] */

void hxdtrnFits_col_read( fitsfile *fp, long irow, int *colnum,
			  HxdTrnFits *fits, int *istat );
/* int colnum[HXD_TRN_FITS_KEY_NUM] */

void hxdtrnFits_col_num( fitsfile *fp, int *colnum, int *istat );
/* int colnum[HXD_TRN_FITS_KEY_NUM] */

void hxdtrnFits_add_tlminmax( fitsfile *fp, int *istat );

void hxdtrnFits_add_comment( fitsfile *fp, int *istat );

void hxdtrnFits_modify_trntime_unit (fitsfile *fp, int time_mode, int *istat);

#endif
