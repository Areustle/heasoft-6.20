#ifndef _HXD_BST_FITS_UTIL_H_
#define _HXD_BST_FITS_UTIL_H_

#include <fitsio.h>

#define HXD_BST_FITS_PRIMARY_HDU        1
#define HXD_BST_FITS_EVENT_EXTENTION      2
#define HXD_BST_FITS_GTI_EXTENTION      3

#define HXD_BST_FITS_KEY_NUM 34
#define HXD_BST_PH_ENE_BIN 54
#define HXD_BST_TH_TIME_BIN 32
#define HXD_BST_SOFT_FLG_BIT 4

#define HXD_BST_INVALID_INIT  0
#define HXD_BST_INVALID_TRGID 99999

typedef struct {
  double aetime;
  double s_time;
  unsigned int ti; 
  short bst_iblock;
  short reserve00; /** for memory allignment **/
  unsigned char bst_sys_err_cod;
  short reserve01; /** for memory allignment **/
  char  reserve02; /** for memory allignment **/
  unsigned char bst_soft_flg;
  short reserve03; /** for memory allignment **/
  char  reserve04; /** for memory allignment **/
  unsigned char bst_current_tpu;
  short reserve05; /** for memory allignment **/
  char  reserve06; /** for memory allignment **/
  unsigned char bst_rest_req;
  short reserve07; /** for memory allignment **/
  char  reserve08; /** for memory allignment **/
  unsigned char bst_data_size;
  short reserve09; /** for memory allignment **/
  char  reserve10; /** for memory allignment **/
  unsigned char bst_read_cnt;
  short reserve11; /** for memory allignment **/
  char  reserve12; /** for memory allignment **/
  unsigned char bst_de_module;
  short reserve13; /** for memory allignment **/
  char  reserve14; /** for memory allignment **/
  unsigned char bst_ae_module;
  short reserve15; /** for memory allignment **/
  char  reserve16; /** for memory allignment **/
  unsigned char bst_data_seq;
  short reserve17; /** for memory allignment **/
  char  reserve18; /** for memory allignment **/
  unsigned int  bst_frzd_time;
  unsigned short bst_th0[HXD_BST_TH_TIME_BIN];
  unsigned short bst_th1[HXD_BST_TH_TIME_BIN];
  unsigned short bst_th2[HXD_BST_TH_TIME_BIN];
  unsigned short bst_th3[HXD_BST_TH_TIME_BIN];
  unsigned short bst_ph[HXD_BST_PH_ENE_BIN];  
  unsigned short bst_pi[HXD_BST_PH_ENE_BIN];  
  unsigned short bst_over_flow;
  short reserve25; /** for memory allignment **/
  unsigned short bst_pseudo;
  short reserve26; /** for memory allignment **/
  unsigned short bst_t_ant0;
  short reserve27; /** for memory allignment **/
  unsigned short bst_t_ant1;
  short reserve28; /** for memory allignment **/
  unsigned short bst_t_ant2;
  short reserve29; /** for memory allignment **/
  unsigned short bst_t_ant3;
  short reserve30; /** for memory allignment **/
  unsigned short bst_t_ant4;
  short reserve31; /** for memory allignment **/
  unsigned short bst_ud;
  short reserve32; /** for memory allignment **/
  unsigned short bst_dead_time;
  short reserve33; /** for memory allignment **/
  unsigned short bst_sum_ld;
  short reserve34; /** for memory allignment **/
  unsigned short bst_w_ant0;
  short reserve35; /** for memory allignment **/
  unsigned short bst_w_ant1;
  short reserve36; /** for memory allignment **/
  unsigned short bst_w_ant2;
  short reserve37; /** for memory allignment **/
  unsigned short bst_w_ant3;    
  short reserve38; /** for memory allignment **/
  unsigned short bst_quality;    
  short reserve39; /** for memory allignment **/
} HxdBstFits;

void hxdbstFits_create_tbl( fitsfile *fp, int *istat );

void hxdbstFits_col_write( fitsfile *fp, long irow, int *colnum,
			  HxdBstFits *fits, int *istat );
/* int colnum[HXD_BST_FITS_KEY_NUM] */

void hxdbstFits_col_read( fitsfile *fp, long irow, int *colnum,
			  HxdBstFits *fits, int *istat );
/* int colnum[HXD_BST_FITS_KEY_NUM] */

void hxdbstFits_col_num( fitsfile *fp, int *colnum, int form_ver, int *istat );
/* int colnum[HXD_BST_FITS_KEY_NUM] */

void hxdbstFits_add_tlminmax( fitsfile *fp, int *istat );

void hxdbstFits_add_comment( fitsfile *fp, int *istat );

void hxdbstFits_add_key(fitsfile *fp, unsigned long *bst_freezed_time_ct,
			int *tup_timemode, int *istat );

void hxdbstFits_modify_key(fitsfile *fp, unsigned long *bst_freezed_time_ct,
			   double *bst_freezed_time, long *tpu_time_mode,
			   int form_ver, int *istat );

void hxdbstFits_read_key(fitsfile *fp, unsigned long *bst_freezed_time_ct,
			 double *bst_freezed_time, long *tpu_time_mode,
			 int form_ver, int *istat );  

void hxdbstFits_add_trgid( fitsfile *fp, int *trg_id, int *istat );

void hxdbstFits_add_bstid( fitsfile *fp, int *istat );

void hxdbstFits_add_trnid( fitsfile *fp, int *istat );

void hxdbstFits_add_timecorr( fitsfile *fp, int *istat );

void hxdbstFits_modify_trgid( fitsfile *fp, int *trg_id, int *istat );

void hxdbstFits_modify_timecorr( fitsfile *fp, int *timecorr, int *istat );

#endif
