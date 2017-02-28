/*
   v0.0.1 test version created by M. sugiho
   v0.0.8 mod Y.Terada 1999-12-23	      
   v0.0.9 mod Y.Terada 2003-04-07
   v0.1.1 mod Y.Terada 2003-05-03
   v0.1.2 mod Y.Terada 2003-10-27
   v0.1.3 mod Y.Terada 2003-12-20
   v0.1.4 mod Y.Terada 2003-12-23
   v0.1.6 mod Y.Terada 2004-03-12
   v0.1.7 mod Y.Terada 2004-04-27
   v0.1.8 mod Y.Terada 2004-06-01
   v0.2.0 mod Y.Terada 2005-05-04
   v0.2.1 mod Y.Terada 2005-05-17
   v0.2.2 mod Y.Terada 2005-05-18
   v0.2.3 mod Y.Terada 2005-05-24
   v0.2.4 mod Y.Terada 2005-11-02
   --> v1.0.0
   v2.0.0 mod Y.Terada 2006-09-13
   v2.0.1 mod Y.Terada 2006-09-15
   v2.0.2 mod Y.Terada 2006-09-15
   v2.0.3 mod Y.Terada 2006-09-21
   v2.0.4 mod Y.Terada 2006-09-28
   v2.0.5 mod Y.Terada 2007-04-13
   v2.0.6 mod Y.Terada 2007-05-01
   v2.0.7/8 mod Y.Terada 2007-05-24
       fits_read_key_lng --> fits_read_key_lnglng
   v2.0.9 mod Y.Terada 2007-06-19 debug
   v2.1.1 mod Y.Terada 2007-10-30
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"
#include "fitsio.h"

#include "hxdbstFitsUtil.h"
#include "hxdeventFitsUtil.h"
#include "hxdFitsCommentUtil.h"

#include "atFunctions.h"
#include "aste_time.h"

#define HXD_TPU_BOARD_NUM 4
#define DEBUG  0
#define DEBUG2 0

static char *pname = "hxdbstFitsUtil";

enum{
  S_TIME, TI, BST_IBLOCK,
  BST_ERR_COD, BST_SOFT_FLG, BST_CURRENT_TPU, BST_REST_REQ, 
  BST_DATA_SIZE, BST_READ_CNT, BST_DE_MODULE, BST_AE_MODULE, BST_DATA_SEQ, 
  BST_FRZ_TIME,
  BST_TH0,BST_TH1,BST_TH2,BST_TH3, BST_PH,BST_PI,
  BST_OVER_FLOW, BST_PSEUDO,
  BST_T_ANT0, BST_T_ANT1, BST_T_ANT2, BST_T_ANT3, BST_T_ANT4,
  BST_UD, BST_DEAD_TIME, BST_SUM_LD,
  BST_W_ANT0, BST_W_ANT1, BST_W_ANT2, BST_W_ANT3,
  BST_QUALITY
};

static char *hxd_bst_fits_keyword_v1[HXD_BST_FITS_KEY_NUM] = {
  "S_TIME", "TI", "BST_IBLOCK",
  "BST_SYS_ERR_CODE", "BST_SOFT_FLG", "BST_CURRENT_TPU", "BST_REST_REQ", 
  "BST_DATA_SIZE", "BST_READ_CNT", "BST_DE_MODULE", "BST_AE_MODULE", 
  "BST_DATA_SEQ",  "BST_FRZD_TIME",
  "BST_TH0","BST_TH1","BST_TH2","BST_TH3", "BST_PH", "BST_PI",
  "BST_OVER_FLOW", "BST_PSEUDO",
  "BST_T_ANT0", "BST_T_ANT1", "BST_T_ANT2", "BST_T_ANT3", "BST_T_ANT4",
  "BST_UD", "BST_DEAD_TIME", "BST_SUM_LD",
  "BST_W_ANT0", "BST_W_ANT1", "BST_W_ANT2", "BST_W_ANT3",
  "BST_QUALITY"
};

static char *hxd_bst_fits_keyword[HXD_BST_FITS_KEY_NUM] = {
  "S_TIME", "TI", "BST_IBLOCK",
  "BST_SYS_ERR_CODE", "BST_SOFT_FLG", "BST_CURRENT_TPU", "BST_REST_REQ", 
  "BST_DATA_SIZE", "BST_READ_CNT", "BST_DE_MODULE", "BST_AE_MODULE", 
  "BST_DATA_SEQ",  "BST_FRZ_TIME",
  "BST_TH0","BST_TH1","BST_TH2","BST_TH3", "BST_PH", "BST_PI",
  "BST_OVER_FLOW", "BST_PSEUDO",
  "BST_T_ANT0", "BST_T_ANT1", "BST_T_ANT2", "BST_T_ANT3", "BST_T_ANT4",
  "BST_UD", "BST_DEAD_TIME", "BST_SUM_LD",
  "BST_W_ANT0", "BST_W_ANT1", "BST_W_ANT2", "BST_W_ANT3",
  "BST_QUALITY"
};

static char *hxd_bst_fits_format[HXD_BST_FITS_KEY_NUM] = {
  "1D", "1V", "1I",
  "1B", "4X", "1B", "1B", "1B", "1B", "1B", "1B", 
  "1B", "1V",
  "32U", "32U", "32U", "32U", "54U",  "54U",  
  "1U", "1U",
  "1U", "1U", "1U", "1U", "1U",
  "1U", "1U", "1U",
  "1U", "1U", "1U", "1U",
  "1U"
};

static char *hxd_bst_fits_unit[HXD_BST_FITS_KEY_NUM] = {
  "s", "1/4096 s", "",
  "", "", "", "",
  "byte", "", "", "",
  "", "1/64 s",
  "cnts per 1/32s", "cnts per 1/32s", "cnts per 1/32s", "cnts per 1/32s", "cnts/s", "cnts/s",
  "C", "C",
  "C", "C", "C", "C", "C",
  "C", "C", "C",
  "C", "C", "C", "C",
  ""
};

void hxdbstFits_create_tbl( fitsfile *fp, int *istat ){
  
  long naxis = 0;
  
  static char extention_name[]="EVENTS";
  
  fits_create_tbl( fp, BINARY_TBL, naxis, HXD_BST_FITS_KEY_NUM,
		  hxd_bst_fits_keyword, hxd_bst_fits_format,
		  hxd_bst_fits_unit, extention_name, istat);
  
  if ( *istat ) {
    fprintf(stderr, "%s:fits_create_tbl failed (%)\n", pname, *istat);
  }
  
}


void hxdbstFits_col_num( fitsfile *fp, int *colnum, int form_ver, int *istat ){
  
  int i;
  
  int casesen = TRUE;
  
  for( i=0;i<HXD_BST_FITS_KEY_NUM;i++ ){
    if (form_ver == 1 ){
      fits_get_colnum( fp, casesen, hxd_bst_fits_keyword_v1[i],
		       &colnum[i], istat);
    } else {
      fits_get_colnum( fp, casesen, hxd_bst_fits_keyword[i],
		       &colnum[i], istat);
    }
    if ( *istat ) {
      fprintf(stderr, "%s: fits_get_colnum('%s') failed (%d)\n",
	      pname, hxd_bst_fits_keyword[i], *istat);
      return;
    }
  }
  
}


void hxdbstFits_col_read( fitsfile *fp, long irow, int *colnum,
			 HxdBstFits *fits, int *istat ){
  
  long firstelem = 1;
  long nelements = 1;
  
  int anynul;
  
  {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[S_TIME], irow, firstelem, nelements,
		      nulval, &fits->s_time, &anynul, istat); 
    fits->aetime = fits->s_time; /** No information for AETIME **/
 }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('S_TIME') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned int nulval = 0x0;
    fits_read_col_uint(fp, colnum[TI], irow, firstelem, nelements,
		       nulval, &fits->ti, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('TI') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    short nulval = 0x0;
    fits_read_col_sht(fp, colnum[BST_IBLOCK], irow, firstelem, nelements,
		      nulval, &fits->bst_iblock, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_IBLOCK') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned char nulval = 0x0;
    fits_read_col_byt(fp, colnum[BST_ERR_COD], irow, firstelem, nelements,
		      nulval, &fits->bst_sys_err_cod, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_SYS_ERR_COD') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    long firstbit = 1;           /* first bit to read (1 = 1st) */
    int  nbits = HXD_BST_SOFT_FLG_BIT;/*number of bits to read (<= 32) */
    char larray[HXD_BST_SOFT_FLG_BIT];
    int i;

    fits_read_col_bit(fp, colnum[BST_SOFT_FLG], irow,
                      firstbit, nbits,  &larray[0], istat);
    fits->bst_soft_flg = 0x00;
    for(i=0;i<HXD_BST_SOFT_FLG_BIT; i++) {
      fits->bst_soft_flg |= ( (larray[i]) << (HXD_BST_SOFT_FLG_BIT - 1 -i) );
    }

  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_SOFT_FLG') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned char nulval = 0x0;
    fits_read_col_byt(fp, colnum[BST_CURRENT_TPU], irow, firstelem, nelements,
		      nulval, &fits->bst_current_tpu, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_CURRENT_TPU') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned char nulval = 0x0;
    fits_read_col_byt(fp, colnum[BST_REST_REQ], irow, firstelem, nelements,
		      nulval, &fits->bst_rest_req, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_REST_REQ') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned char nulval = 0x0;
    fits_read_col_byt(fp, colnum[BST_DATA_SIZE], irow, firstelem, nelements,
		      nulval, &fits->bst_data_size, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_DATA_SIZE') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned char nulval = 0x0;
    fits_read_col_byt(fp, colnum[BST_READ_CNT], irow, firstelem, nelements,
		      nulval, &fits->bst_read_cnt, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_READ_CNT') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned char nulval = 0x0;
    fits_read_col_byt(fp, colnum[BST_DE_MODULE], irow, firstelem, nelements,
		      nulval, &fits->bst_de_module, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_DE_MODULE') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned char nulval = 0x0;
    fits_read_col_byt(fp, colnum[BST_AE_MODULE], irow, firstelem, nelements,
		      nulval, &fits->bst_ae_module, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_AE_MODULE') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned char nulval = 0x0;
    fits_read_col_byt(fp, colnum[BST_DATA_SEQ], irow, firstelem, nelements,
		      nulval, &fits->bst_data_seq, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_DATA_SEQ') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned int nulval = 0x0;
    fits_read_col_uint(fp, colnum[BST_FRZ_TIME], irow, firstelem, nelements,
		       nulval, &fits->bst_frzd_time, &anynul, istat);
    /*
    unsigned short nulval = 0x0;    
    fits_read_col_usht(fp, colnum[BST_FRZ_TIME], irow, firstelem, nelements,
		       nulval, &fits->bst_frzd_time, &anynul, istat);
    */
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_FRZ_TIME') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_TH0], irow, firstelem,
		       HXD_BST_TH_TIME_BIN, nulval, fits->bst_th0, &anynul,
		       istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col BST_TH0 failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_TH1], irow, firstelem,
		       HXD_BST_TH_TIME_BIN, nulval, fits->bst_th1, &anynul,
		       istat);
  }
  if ( *istat ) {    
    fprintf(stderr,"%s:fits_read_col BST_TH1 failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_TH2], irow, firstelem,
		       HXD_BST_TH_TIME_BIN, nulval, fits->bst_th2, &anynul,
		       istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col BST_TH2 failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_TH3], irow, firstelem,
		       HXD_BST_TH_TIME_BIN, nulval, fits->bst_th3, &anynul,
		       istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col BST_TH3 failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_PH], irow, firstelem,
		       HXD_BST_PH_ENE_BIN, nulval, fits->bst_ph, &anynul,
		       istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col BST_PH failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_PI], irow, firstelem,
		       HXD_BST_PH_ENE_BIN, nulval, fits->bst_pi, &anynul,
		       istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col BST_PI failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_OVER_FLOW], irow, firstelem,
		       nelements, nulval, &fits->bst_over_flow, &anynul,
		       istat); 
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_OVER_FLOW)' failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_PSEUDO], irow, firstelem,
		       nelements, nulval, &fits->bst_pseudo, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_PSEUDO') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_T_ANT0], irow, firstelem,
		       nelements, nulval, &fits->bst_t_ant0, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_T_ANT0') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_T_ANT1], irow, firstelem,
		       nelements, nulval, &fits->bst_t_ant1, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_T_ANT1') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_T_ANT2], irow, firstelem,
		       nelements, nulval, &fits->bst_t_ant2, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_T_ANT2') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_T_ANT3], irow, firstelem,
		       nelements, nulval, &fits->bst_t_ant3, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_T_ANT3') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_T_ANT4], irow, firstelem,
		       nelements, nulval, &fits->bst_t_ant4, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_T_ANT4') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_UD], irow, firstelem,
		       nelements, nulval, &fits->bst_ud, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_UD') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_DEAD_TIME], irow, firstelem,
		       nelements, nulval, &fits->bst_dead_time, &anynul,
		       istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_DEAD_TIME') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_SUM_LD], irow, firstelem,
		       nelements, nulval, &fits->bst_sum_ld, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_SUM_LD') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_W_ANT0], irow, firstelem,
		       nelements, nulval, &fits->bst_w_ant0, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_W_ANT0') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_W_ANT1], irow, firstelem,
		       nelements, nulval, &fits->bst_w_ant1, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_W_ANT1') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_W_ANT2], irow, firstelem,
		       nelements, nulval, &fits->bst_w_ant2, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_W_ANT2') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_W_ANT3], irow, firstelem,
		       nelements, nulval, &fits->bst_w_ant3, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_W_ANT3') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    unsigned short nulval = 0x0;
    fits_read_col_usht(fp, colnum[BST_QUALITY], irow, firstelem,
		       nelements, nulval, &fits->bst_quality, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col ('BST_QUALITY') failed (%d)\n",
            pname, *istat);
    return;
  }

}

void hxdbstFits_col_write( fitsfile *fp, long irow, int *colnum,
			    HxdBstFits *fits, int *istat ){
  
  long firstelem = 1;
  long nelements = 1;

  if (DEBUG2){
    int i;
    for( i=0;i<HXD_BST_FITS_KEY_NUM;i++ )
      fprintf(stdout, "%s:colnum[%d]=%d\n",pname,i,colnum[i]);
  }
  
  {
    fits_write_col_dbl(fp, colnum[S_TIME], irow, firstelem, nelements,
		     &fits->s_time, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col S_TIME=%f failed (%d)\n",
	    pname, fits->s_time, *istat);
    return;
  } else {
    fits_write_col_uint(fp, colnum[TI], irow, firstelem, nelements,
			&fits->ti, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TI=%u failed (%d)\n",
	    pname, fits->ti, *istat);
    return;
  } else {
    fits_write_col_sht(fp, colnum[BST_IBLOCK], irow, firstelem, nelements,
                        &fits->bst_iblock, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_IBLOCK=%d failed (%d)\n",
            pname, fits->bst_iblock, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[BST_ERR_COD], irow, firstelem, nelements,
		       &fits->bst_sys_err_cod, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_SYS_ERR_COD=%d failed (%d)\n",
            pname, fits->bst_sys_err_cod, *istat);
    return;
  } else {
    /*
    fits_write_col_byt(fp, colnum[BST_SOFT_FLG], irow, firstelem, nelements,
		       &fits->bst_soft_flg, istat);
    */
    long  fbit = 1;        
    long  nbit = HXD_BST_SOFT_FLG_BIT; 
    char  larray[HXD_BST_SOFT_FLG_BIT];
    int i;    unsigned char soft_flg;

    soft_flg = fits->bst_soft_flg;
    for(i=0;i<HXD_BST_SOFT_FLG_BIT; i++) {          
      larray[HXD_BST_SOFT_FLG_BIT-i-1] = (soft_flg>> i) & 0x1;
    }
    fits_write_col_bit(fp, colnum[BST_SOFT_FLG], irow, fbit, nbit,
                       &larray[0], istat); 
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_SOFT_FLG=%d failed (%d)\n",
            pname, fits->bst_soft_flg, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[BST_CURRENT_TPU], irow, firstelem, nelements,
		       &fits->bst_current_tpu, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_CURRENT_TPU=%d failed (%d)\n",
            pname, fits->bst_current_tpu, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[BST_REST_REQ], irow, firstelem, nelements,
		       &fits->bst_rest_req, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_REST_REQ=%d failed (%d)\n",
            pname, fits->bst_rest_req, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[BST_DATA_SIZE], irow, firstelem, nelements,
		       &fits->bst_data_size, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_DATA_SIZE=%d failed (%d)\n",
            pname, fits->bst_data_size, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[BST_READ_CNT], irow, firstelem, nelements,
		       &fits->bst_read_cnt, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_READ_CNT=%d failed (%d)\n",
            pname, fits->bst_read_cnt, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[BST_DE_MODULE], irow, firstelem, nelements,
		       &fits->bst_de_module, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_DE_MODULE=%d failed (%d)\n",
            pname, fits->bst_de_module, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[BST_AE_MODULE], irow, firstelem, nelements,
		       &fits->bst_ae_module, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_AE_MODULE=%d failed (%d)\n",
            pname, fits->bst_ae_module, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[BST_DATA_SEQ], irow, firstelem, nelements,
		       &fits->bst_data_seq, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_DATA_SEQ=%d failed (%d)\n",
            pname, fits->bst_data_seq, *istat);
    return;
  } else {
    fits_write_col_uint(fp, colnum[BST_FRZ_TIME], irow, firstelem, nelements,
			&fits->bst_frzd_time, istat);
    /*
    fits_write_col_usht(fp, colnum[BST_FRZ_TIME], irow, firstelem, nelements,
			&fits->bst_frzd_time, istat);
    */
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_FRZ_TIME=%d failed (%d)\n",
            pname, fits->bst_frzd_time, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_TH0], irow, firstelem,
			HXD_BST_TH_TIME_BIN, fits->bst_th0, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_TH0 failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_TH1], irow, firstelem,
			HXD_BST_TH_TIME_BIN, fits->bst_th1, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_TH1 failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_TH2], irow, firstelem,
			HXD_BST_TH_TIME_BIN, fits->bst_th2, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_TH2 failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_TH3], irow, firstelem,
			HXD_BST_TH_TIME_BIN, fits->bst_th3, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_TH3 failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_PH], irow, firstelem,
			HXD_BST_PH_ENE_BIN, fits->bst_ph, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_PH failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_PI], irow, firstelem,
			HXD_BST_PH_ENE_BIN, fits->bst_pi, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_PI failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_OVER_FLOW], irow, firstelem,
                        nelements, &fits->bst_over_flow, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_OVER_FLOW=%d failed (%d)\n",
            pname, fits->bst_over_flow, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_PSEUDO], irow, firstelem,
                        nelements, &fits->bst_pseudo, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_PSEUDO=%d failed (%d)\n",
            pname, fits->bst_pseudo, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_T_ANT0], irow, firstelem,
                        nelements, &fits->bst_t_ant0, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_T_ANT0=%d failed (%d)\n",
            pname, fits->bst_t_ant0, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_T_ANT1], irow, firstelem,
                        nelements, &fits->bst_t_ant1, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_T_ANT1=%d failed (%d)\n",
            pname, fits->bst_t_ant1, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_T_ANT2], irow, firstelem,
                        nelements, &fits->bst_t_ant2, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_T_ANT2=%d failed (%d)\n",
            pname, fits->bst_t_ant2, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_T_ANT3], irow, firstelem,
                        nelements, &fits->bst_t_ant3, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_T_ANT3=%d failed (%d)\n",
            pname, fits->bst_t_ant3, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_T_ANT4], irow, firstelem,
                        nelements, &fits->bst_t_ant4, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_T_ANT4=%d failed (%d)\n",
            pname, fits->bst_t_ant4, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_UD], irow, firstelem,
                        nelements, &fits->bst_ud, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_UD=%d failed (%d)\n",
            pname, fits->bst_ud, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_DEAD_TIME], irow, firstelem,
                        nelements, &fits->bst_dead_time, istat);
    if (DEBUG) fprintf(stderr, "%s:fits_write_col BST_DEAD_TIME=%d (status %d) col=%d, irow=%d, firstelem=%d, nelem=%d\n",
            pname, fits->bst_dead_time, *istat, colnum[BST_DEAD_TIME], irow, firstelem, nelements);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_DEAD_TIME=%d failed (%d)\n",
            pname, fits->bst_dead_time, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_SUM_LD], irow, firstelem,
                        nelements, &fits->bst_sum_ld, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_SUM_LD=%d failed (%d)\n",
            pname, fits->bst_sum_ld, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_W_ANT0], irow, firstelem,
                        nelements, &fits->bst_w_ant0, istat);
    if (DEBUG) fprintf(stderr, "%s:fits_write_col BST_W_ANT0=%d (status %d) col=%d, irow=%d, firstelem=%d, nelem=%d\n",
            pname, fits->bst_w_ant0, *istat, colnum[BST_W_ANT0], irow, firstelem, nelements);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_W_ANT0=%d failed (%d)\n",
            pname, fits->bst_w_ant0, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_W_ANT1], irow, firstelem,
                        nelements, &fits->bst_w_ant1, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_W_ANT1=%d failed (%d)\n",
            pname, fits->bst_w_ant1, *istat);
    if (DEBUG) fprintf(stderr,
	    "%s:fits_write_col BST_W_ANT1=%d failed (%d) col=%d, irow=%d, firstelem=%d, nelem=%d\n",
            pname, fits->bst_w_ant1, *istat, colnum[BST_W_ANT1], irow, firstelem, nelements);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_W_ANT2], irow, firstelem,
                        nelements, &fits->bst_w_ant2, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_W_ANT2=%d failed (%d)\n",
            pname, fits->bst_w_ant2, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_W_ANT3], irow, firstelem,
                        nelements, &fits->bst_w_ant3, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_W_ANT3=%d failed (%d)\n",
            pname, fits->bst_w_ant3, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[BST_QUALITY], irow, firstelem,
                        nelements, &fits->bst_quality, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col BST_QUALITY=%d failed (%d)\n",
            pname, fits->bst_quality, *istat);
    return;
  }
  
}

void hxdbstFits_add_tlminmax(fitsfile *fp, int *istat){
  int i,j=0;
  
  char tmp_card[81];

  char *card[]={
    "TLMIN3  =                    0 / minimum legal value",
    "TLMAX3  =                    1 / maximum legal value",
    "TLMIN6  =                    0 / minimum legal value",
    "TLMAX6  =                    3 / maximum legal value",
    "TLMIN7  =                    0 / minimum legal value",
    "TLMAX7  =                  129 / maximum legal value",
    "TLMIN8  =                    1 / minimum legal value",
    "TLMAX8  =                 1024 / maximum legal value",
    "TLMIN9  =                    0 / minimum legal value",
    "TLMAX9  =                  255 / maximum legal value",
    "TLMIN10 =                    0 / minimum legal value",
    "TLMAX10 =                    3 / maximum legal value",
    "TLMIN11 =                    0 / minimum legal value",
    "TLMAX11 =                    3 / maximum legal value",
    "TLMIN14 =                    0 / minimum legal value",
    "TLMAX14 =                65535 / maximum legal value",
    "TLMIN15 =                    0 / minimum legal value",
    "TLMAX15 =                65535 / maximum legal value",
    "TLMIN16 =                    0 / minimum legal value",
    "TLMAX16 =                65535 / maximum legal value",
    "TLMIN17 =                    0 / minimum legal value",
    "TLMAX17 =                65535 / maximum legal value",
    "TLMIN18 =                    0 / minimum legal value",
    "TLMAX18 =                65535 / maximum legal value",
    "TLMIN19 =                    0 / minimum legal value",
    "TLMAX19 =                65535 / maximum legal value",
    "TLMIN20 =                    0 / minimum legal value",
    "TLMAX20 =                65535 / maximum legal value",
    "TLMIN21 =                    0 / minimum legal value",
    "TLMAX21 =                65535 / maximum legal value",
    "TLMIN22 =                    0 / minimum legal value",
    "TLMAX22 =                65535 / maximum legal value",
    "TLMIN23 =                    0 / minimum legal value",
    "TLMAX23 =                65535 / maximum legal value",
    "TLMIN24 =                    0 / minimum legal value",
    "TLMAX24 =                65535 / maximum legal value",
    "TLMIN25 =                    0 / minimum legal value",
    "TLMAX25 =                65535 / maximum legal value",
    "TLMIN26 =                    0 / minimum legal value",
    "TLMAX26 =                65535 / maximum legal value",
    "TLMIN27 =                    0 / minimum legal value",
    "TLMAX27 =                65535 / maximum legal value",
    "TLMIN28 =                    0 / minimum legal value",
    "TLMAX28 =                65535 / maximum legal value",
    "TLMIN29 =                    0 / minimum legal value",
    "TLMAX29 =                65535 / maximum legal value",
    "TLMIN30 =                    0 / minimum legal value",
    "TLMAX30 =                65535 / maximum legal value",
    "TLMIN31 =                    0 / minimum legal value",
    "TLMAX31 =                65535 / maximum legal value",
    "TLMIN32 =                    0 / minimum legal value",
    "TLMAX32 =                65535 / maximum legal value",
    "TLMIN33 =                    0 / minimum legal value",
    "TLMAX33 =                65535 / maximum legal value"
  };
  
  for(i=1;;i++){
    if (fits_flush_file(fp, istat)){
      fprintf(stderr, "%s:fits_flush_file failed (%d)\n", pname, *istat);
    }
    
    if (fits_read_record(fp, i, tmp_card, istat)) {
      /* fprintf(stderr, "%s:fits_read_record failed (%d)\n",
	 pname, *istat); */
      *istat = 0;
      break;
    }

    if(0 == strncmp("TFORM3 ", tmp_card, 7)){
/*    fprintf(stdout, "TFORM3: come here j=%d\n", j); */
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM6 ", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM7 ", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM8 ", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM9 ", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM10", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM11", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM14", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM15", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM16", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM17", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM18", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM19", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM20", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM21", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM22", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM23", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM24", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM25", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM26", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM27", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM28", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM29", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM30", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM31", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM32", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM33", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }

    if ( *istat ) {
      fprintf(stderr, "%s:fits_insert_record failed (%d)\n",
              pname, *istat);
      break;
    }
  }  
}

void hxdbstFits_add_comment( fitsfile *fp, int *istat ){
  
  char *keyword[]={
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  ", "TTYPE6  ",
    "TTYPE7  ", "TTYPE8  ", "TTYPE9  ", "TTYPE10 ", "TTYPE11 ", "TTYPE12 ",
    "TTYPE13 ", "TTYPE14 ", "TTYPE15 ", "TTYPE16 ", "TTYPE17 ", "TTYPE18 ",
    "TTYPE19 ", "TTYPE20 ", "TTYPE21 ", "TTYPE22 ", "TTYPE23 ", "TTYPE24 ",
    "TTYPE25 ", "TTYPE26 ", "TTYPE27 ", "TTYPE28 ", "TTYPE29 ", "TTYPE30 ",
    "TTYPE31 ", "TTYPE32 ", "TTYPE33 ", "TTYPE34 "
  };
  
  char *comment[]={
    "Gamma Burst event data packet edit time",
    "Secondary header time",
    "This block number of a packet",
    "Gamma Burst data error code",
    "Gamma Burst data soft trigger                                         1:TPU Trigger Mode, 2: PH Monitor Mode,                               3:WANTI (Well Slow LD) Monitor Mode,                                  4: Pseudo Mode ",
    "Current TPU module ID of Auto-Request mode",
    "Rest Req-count of current TPU in Auto-Request",
    "Data Size of HXD-AE-TPU output",
    "Read Count of Burst data to HXD-DE",
    "Tpu module ID (with DE)",
    "Tpu module ID (with AE)",
    "Gamma Burst data sequence number",
    "Gamma Burst data freeze time",
    "Light curve counter of 0ch",
    "Light curve counter of 1ch",
    "Light curve counter of 2ch",
    "Light curve counter of 3ch",
    "54 channel PH counter",
    "54 channel PI counter",
    "Over flow counter",
    "Pseudo data counter",
    "T0 LD hit counter",
    "T1 LD hit counter",
    "T2 LD hit counter",
    "T3 LD hit counter",
    "T4 LD hit counter",
    "Sum UD counter",
    "Dead time counter                                                     (Unit is defined by TRN_DT_MODE)",
    "Sum LD counter                                                        (Unit is defined by TRN_SUMLD_MODE)",
    "W0 Anti counter",
    "W1 Anti counter",
    "W2 Anti counter",
    "W3 Anti counter",
    "BST Quality flag"
  };
  
  hxdFitsComment_write ( fp, HXD_BST_FITS_KEY_NUM, keyword, comment, istat);
  
  if(*istat){
    fprintf(stderr, "%s: hxdFitsComment_write failed(%d)\n",
	    pname, istat);
  }
  
}

void hxdbstFits_add_key(fitsfile *fp, unsigned long *bst_freezed_time_ct,
			int *tpu_timemode, int *istat ){
  
  int keynum;
  int i,j;
  char tmp_card[81];
#define HXDBSTFITS_BST_CARD_NUM 20
#define HXDBSTFITS_TPU_NBOARD    4
  static char bst_card[HXDBSTFITS_BST_CARD_NUM][81] = {
    "FRZN_TM0=                      / Gamma Burst data freeze time Tpu 0",
    "FRTM_CT0=                      / Gamma Burst data freeze time Tpu 0 in telm",
    "TM_MODE0=                      / Tpu0 time mode [0x00:1/2 sec][0x01:1 sec]",
    "COMMENT                          [0x02:2 sec][0x03:4 sec]",
    "DATE_FZ0= '        '           / DATE freeze_time T0(YYYY-MM-DDThh:mm:ss UT)",
    "FRZN_TM1=                      / Gamma Burst data freeze time Tpu 0",
    "FRTM_CT1=                      / Gamma Burst data freeze time Tpu 0 in telm",
    "TM_MODE1=                      / Tpu0 time mode [0x00:1/2 sec][0x01:1 sec]",
    "COMMENT                          [0x02:2 sec][0x03:4 sec]",
    "DATE_FZ1= '        '           / DATE freeze_time T0(YYYY-MM-DDThh:mm:ss UT)",
    "FRZN_TM2=                      / Gamma Burst data freeze time Tpu 0",
    "FRTM_CT2=                      / Gamma Burst data freeze time Tpu 0 in telm",
    "TM_MODE2=                      / Tpu0 time mode [0x00:1/2 sec][0x01:1 sec]",
    "COMMENT                          [0x02:2 sec][0x03:4 sec]",
    "DATE_FZ2= '        '           / DATE freeze_time T0(YYYY-MM-DDThh:mm:ss UT)",
    "FRZN_TM3=                      / Gamma Burst data freeze time Tpu 0",
    "FRTM_CT3=                      / Gamma Burst data freeze time Tpu 0 in telm",
    "TM_MODE3=                      / Tpu0 time mode [0x00:1/2 sec][0x01:1 sec]",
    "COMMENT                          [0x02:2 sec][0x03:4 sec]",
    "DATE_FZ3= '        '           / DATE freeze_time T0(YYYY-MM-DDThh:mm:ss UT)"
  };
  double bst_freezed_time[HXDBSTFITS_TPU_NBOARD];
  int form_ver = HXD_EVENT_FITS_FORMAT_VERSION;
  long tpu_timemode_lng[HXDBSTFITS_TPU_NBOARD];

  for(keynum=1;;keynum++){
    if (fits_flush_file(fp, istat)){
      fprintf(stderr,
              "Error doing fits_flush_file() (status=%d)\n", *istat);
    }
    if (fits_read_record(fp, keynum, tmp_card, istat)) {
      *istat = 0; break;
    }
    
    if(0 == strncmp("HISTORY", tmp_card, 7)){
      if (DEBUG2) fprintf(stdout, "card found.\n");
      break;
    }
  } /** end of keynum **/

  /***** write keys *****/
  for(i=0; i<HXDBSTFITS_BST_CARD_NUM; i++){
    fits_insert_record(fp, keynum+i, bst_card[i], istat);
    if(*istat){
      fprintf(stderr,"Error in writing keyword: %s (status=%d)\n",
	      bst_card[i], *istat);
        break;
      }
  }

  /***** update keys *****/
  for(j=0;j<HXDBSTFITS_TPU_NBOARD; j++){
    bst_freezed_time[j] = 0.0;
    tpu_timemode_lng[j] = (long) tpu_timemode[j];
  }

  hxdbstFits_modify_key(fp, bst_freezed_time_ct, bst_freezed_time,
			tpu_timemode_lng, form_ver, istat);
  return;  
}

void
hxdbstFits_modify_key(fitsfile *fp, unsigned long *bst_freezed_time_ct,
		      double *bst_freezed_time, long *tpu_time_mode,
		      int form_ver,  int *istat ){
  int board;
  int decimal   =  8;
  char *comment = "&";
  static char *bst_key_tm_v1[]={
    "FRZD_TM0", "FRZD_TM1", "FRZD_TM2", "FRZD_TM3"
  };
  static char *bst_key_tm[]={
    "FRZN_TM0", "FRZN_TM1", "FRZN_TM2", "FRZN_TM3"
  };
  static char *bst_key_ct[]={
    "FRTM_CT0", "FRTM_CT1", "FRTM_CT2", "FRTM_CT3"
  };
  static char *bst_key_mode[]={
    "TM_MODE0", "TM_MODE1", "TM_MODE2", "TM_MODE3"
  };
  static char *bst_key_date[]={
    "DATE_FZ0", "DATE_FZ1", "DATE_FZ2", "DATE_FZ3"
  };
  AtTimeD attime;
  char date[80];

  for(board=0;board<HXD_TPU_BOARD_NUM ;board++){ 
    if(form_ver == 1){
      fits_modify_key_fixdbl(fp, bst_key_tm_v1[board], bst_freezed_time[board],
			     decimal, comment, istat);
    } else {
      fits_modify_key_fixdbl(fp, bst_key_tm[board], bst_freezed_time[board],
			     decimal, comment, istat);
    }
      if ( *istat ) {
	  fprintf(stderr, "%s:fits_modify_key (%s) failed (%d)\n",
		  pname, bst_key_tm[board], istat);
	  return ; 
      }

      fits_modify_key_lng (fp, bst_key_ct[board], bst_freezed_time_ct[board],
			   comment, istat);
      if ( *istat ) {
	  fprintf(stderr, "%s:fits_modify_key (%s) failed (%d)\n",
		  pname, bst_key_ct[board], istat);
	  return ;
      }

      fits_modify_key_lng(fp, bst_key_mode[board], tpu_time_mode[board],
			  comment, istat); 
      if ( *istat ) {
	  fprintf(stderr, "%s:fits_modify_key (%s) failed (%d)\n",
		  pname,  bst_key_mode[board], istat);
	  return ; 
      }

      aste2attimeD(bst_freezed_time[board], &attime);
      sprintf(date, "%04d-%02d-%02dT%02d:%02d:%02d",
	      attime.yr, attime.mo, attime.dy,
	      attime.hr, attime.mn, attime.sc );

      fits_modify_key_str(fp, bst_key_date[board], date, 
			  comment, istat); 
      if ( *istat ) {
	  fprintf(stderr, "%s:fits_modify_str (%s) failed (%d): %s\n",
		  pname,  bst_key_date[board], istat, date);
	  return ;
      }

  }
  return;
}

void hxdbstFits_read_key(fitsfile *fp, unsigned long *bst_freezed_time_ct,
			 double *bst_freezed_time, long *tpu_time_mode,
			 int form_ver, int *istat ){

  char comment[80] ;
  
  int board;
  
  static char *bst_key_v1[]={
    "FRZD_TM0", "FRZD_TM1", "FRZD_TM2", "FRZD_TM3", 
    "FRTM_CT0", "FRTM_CT1", "FRTM_CT2", "FRTM_CT3",
    "TM_MODE0", "TM_MODE1", "TM_MODE2", "TM_MODE3"
  };

  static char *bst_key[]={
    "FRZN_TM0", "FRZN_TM1", "FRZN_TM2", "FRZN_TM3", 
    "FRTM_CT0", "FRTM_CT1", "FRTM_CT2", "FRTM_CT3",
    "TM_MODE0", "TM_MODE1", "TM_MODE2", "TM_MODE3"
  };

  if (form_ver == 1 ){
    for (board=0; board<HXD_TPU_BOARD_NUM; board++){
      fits_read_key_dbl(fp, bst_key_v1[board], &bst_freezed_time[board],
			comment, istat);
      if ( *istat ) {
	fprintf(stderr, "%s:fits_read_key %s failed (%d)\n", pname, 
		bst_key_v1[board], *istat);
	return ;      
      }
      
      fits_read_key (fp, TULONG, bst_key_v1[board+HXD_TPU_BOARD_NUM],
		     &bst_freezed_time_ct[board], comment, istat);
      if ( *istat ) {
	fprintf(stderr, "%s:fits_read_key %s failed (%d)\n", pname, 
		bst_key_v1[board+HXD_TPU_BOARD_NUM],*istat);
	return ;
      }
      
      fits_read_key(fp, TLONG, bst_key_v1[board+HXD_TPU_BOARD_NUM*2],
			&tpu_time_mode[board], comment, istat); 
      if ( *istat ) {
	fprintf(stderr, "%s:fits_read_key %s failed (%d)\n", pname, 
		bst_key_v1[board+HXD_TPU_BOARD_NUM*2], *istat);
	return ;      
      }    
    }
  } else {
    for (board=0; board<HXD_TPU_BOARD_NUM; board++){
      fits_read_key_dbl(fp, bst_key[board], &bst_freezed_time[board],
			comment, istat);
      if ( *istat ) {
	fprintf(stderr, "%s:fits_read_key %s failed (%d)\n", pname, 
		bst_key[board], *istat);
	return ;      
      }
      
      fits_read_key (fp, TULONG, bst_key[board+HXD_TPU_BOARD_NUM],
		     &bst_freezed_time_ct[board], comment, istat);
      if ( *istat ) {
	fprintf(stderr, "%s:fits_read_key %s failed (%d)\n", pname, 
		bst_key[board+HXD_TPU_BOARD_NUM], *istat);
	return ;
      }
      
      fits_read_key(fp, TLONG, bst_key[board+HXD_TPU_BOARD_NUM*2],
		    &tpu_time_mode[board], comment, istat); 
      if ( *istat ) {
	fprintf(stderr, "%s:fits_read_key %s failed (%d)\n", pname, 
		bst_key[board+HXD_TPU_BOARD_NUM*2], *istat);
	return ;      
      }    
    }
  }

}

void hxdbstFits_add_trgid( fitsfile *fp, int *trg_id, int *istat ){
  int i;
  char tmp_card[81];
  char card[81];
  char comment[81];

  sprintf(card,
          "TRG_ID  =                    %d / Sequential Trigger ID of HXD-WAM Burst data,",
	  *trg_id);
  sprintf(comment,
	  "COMMENT                          listed in CALDB, ae_hxd_bstidt_yyyymmdd.fits.");

  for(i=1;;i++){
    if (fits_flush_file(fp, istat)){
      fprintf(stderr,
              "Error doing fits_flush_file() (status=%d)\n", *istat);
    }
    
    if (fits_read_record(fp, i, tmp_card, istat)) {
      *istat = 0;
      break;
    }

    if (DEBUG2) fprintf(stdout, "card = %s\n",tmp_card);
    if(0 == strncmp("HISTORY", tmp_card, 7)){
      if (DEBUG2) fprintf(stdout, "found HISTORY, write %s", card);
      fits_insert_record(fp, i, card,    istat); /** before HISTORY **/
      if(*istat){
        fprintf(stderr,"Error in writing keyword: TRG_ID (status=%d)\n",
                *istat);
        break;
      }
      fits_insert_record(fp, i+1, comment, istat);
      if(*istat){
        fprintf(stderr,"Error in writing comment, TRG_ID (status=%d)\n",
                *istat);
        break;
      }
      break;
    }
  }

  return;
}

/* modify? read? Is it OK? */
void hxdbstFits_modify_trgid( fitsfile *fp, int *trg_id, int *istat ){
  long trgid;
  char comment[80];
  
  fits_read_key_lnglng(fp, "TRG_ID", &trgid, comment, istat);
  if(*istat){
    fprintf(stderr,"Error in reading TRG_ID (status=%d)\n", *istat);
    *trg_id = HXD_BST_INVALID_TRGID;
    return;
  }

  *trg_id = (int) trgid;
  return;

}


void hxdbstFits_add_bstid( fitsfile *fp, int *istat ){
  int i;
  char tmp_card[81];
  char card[81] 
    = "BURST_ID= '        '           / Trigger ID of HXD-WAM Burst data, which is";
  char comment[81]
    = "COMMENT                          tagged in the pipe line processing.";

  for(i=1;;i++){
    if (fits_flush_file(fp, istat)){
      fprintf(stderr,
              "Error doing fits_flush_file() (status=%d)\n", *istat);
    }
    
    if (fits_read_record(fp, i, tmp_card, istat)) {
      *istat = 0;
      break;
    }

    if(0 == strncmp("HISTORY", tmp_card, 7)){
      fits_insert_record(fp, i, card,    istat); /** before HISTORY **/
      if(*istat){
        fprintf(stderr,"Error in writing keyword: BURST_ID (status=%d)\n",
                *istat);
        break;
      }
      fits_insert_record(fp, i+1, comment,    istat);
      if(*istat){
        fprintf(stderr,"Error in writing keyword: BURST_ID (status=%d)\n",
                *istat);
        break;
      }
      break;
    }
  }

  return;
}

void hxdbstFits_add_trnid( fitsfile *fp, int *istat ){
  int i;
  char tmp_card[81];
  char card[81] 
    = "TRN_ID  = '        '           / Corresponding Event ID of HXD-WAM Trn data. ";
  char comment[81]
    = "COMMENT                          See ae_hxd_bstidt_yyyymmdd.fits in CALDB area.";

  for(i=1;;i++){
    if (fits_flush_file(fp, istat)){
      fprintf(stderr,
              "Error doing fits_flush_file() (status=%d)\n", *istat);
    }
    
    if (fits_read_record(fp, i, tmp_card, istat)) {
      *istat = 0;
      break;
    }

    if(0 == strncmp("HISTORY", tmp_card, 7)){
      fits_insert_record(fp, i, card,    istat); /** before HISTORY **/
      if(*istat){
        fprintf(stderr,"Error in writing keyword: BURST_ID (status=%d)\n",
                *istat);
        break;
      }
      fits_insert_record(fp, i+1, comment,    istat);
      if(*istat){
        fprintf(stderr,"Error in writing keyword: BURST_ID (status=%d)\n",
                *istat);
        break;
      }
      break;
    }
  }

  return;
}

void
hxdbstFits_add_timecorr( fitsfile *fp, int *istat ){
  int i;
  char tmp_card[81];
  char card[81] 
    = "TIMECORR=                      / FRZN_TMn, DATE_FZn (n=0,1,2,3) assigned by the ";
  char comment[81]
    = "COMMENT                          hxdbsttime is valid or not.(0:valid, 1:invalid)";

  for(i=1;;i++){
    if (fits_flush_file(fp, istat)){
      fprintf(stderr,
              "Error doing fits_flush_file() (status=%d)\n", *istat);
    }
    
    if (fits_read_record(fp, i, tmp_card, istat)) {
      *istat = 0;
      break;
    }

    if(0 == strncmp("HISTORY", tmp_card, 7)){
      fits_insert_record(fp, i, card,    istat); /** before HISTORY **/
      if(*istat){
        fprintf(stderr,"Error in writing keyword: TIMECORR (status=%d)\n",
                *istat);
        break;
      }
      fits_insert_record(fp, i+1, comment,    istat);
      if(*istat){
        fprintf(stderr,"Error in writing keyword: TIMECORR,comment1 (status=%d)\n",
                *istat);
        break;
      }
      break;
    }
  }

  return;
}

void
hxdbstFits_modify_timecorr( fitsfile *fp, int *timecorr, int *istat ){
  int datatype = TLOGICAL;
  char commenta[81]                  = "FRZN_TMn, DATE_FZn (n=0,1,2,3) assigned by the";
  char commentb[81]
    = "COMMENT                          hxdbsttime is valid or not.(T:valid, F:invalid)";
  int val;

  int i;
  char tmp_card[81];

  int hdunum;
  int hdutype;

  /** defined in hxdcaldbUtil.h **/
#define HXDBSTTIME_ASGN_VALID   1
#define HXDBSTTIME_ASGN_INVALID 0

  if (*timecorr == HXDBSTTIME_ASGN_VALID) {
    val = 1;
  } else {
    val = 0;
  }

  for(hdunum=HXD_BST_FITS_PRIMARY_HDU; hdunum<=HXD_BST_FITS_EVENT_EXTENTION;
      hdunum++){

    fits_movabs_hdu(fp, hdunum, &hdutype, istat);
    if (*istat) {
      fprintf(stderr, 
	      "hxdbstFits_modify_timecorr:fits_movabs_hdu %d failed (%d)\n",
              pname, hdunum, *istat);
      return;
    }
    
    fits_update_key_log(fp, "TIMECORR", val, commenta, istat);
    if(*istat){
      fprintf(stderr,"hxdbstFits_modify_timecorr: failed (%d)\n",*istat);
      return;
    }
    
    
    /*** modify comment (for old mkhxd1stfits)***/
    for(i=1;;i++){
      if (fits_flush_file(fp, istat)){
	fprintf(stderr,
		"Error doing fits_flush_file() (status=%d)\n", *istat);
      }
      
      if (fits_read_record(fp, i, tmp_card, istat)) {
	*istat = 0;
	break;
      }
      
      /** delete comment **/
      if(0 == strncmp("TIMECORR", tmp_card, 8)){
	if (fits_read_record(fp, i+1, tmp_card, istat)) {
	  *istat = 0;
	  break;
	}
	if(0 == strncmp("COMMENT", tmp_card, 7)){
	  fits_delete_record(fp, i+1, istat); /** COMMENT **/
	  if(*istat){
	    fprintf(stderr,"Error in deleting comments: TIMECORR (status=%d)\n",
		    *istat);
	    break;
	  }
	}
	
	
	if (fits_read_record(fp, i+1, tmp_card, istat)) {
	  *istat = 0;
	  break;
	}
	if(0 == strncmp("COMMENT", tmp_card, 7)){
	  fits_delete_record(fp, i+1, istat); /** COMMENT **/
	  if(*istat){
	    fprintf(stderr,"Error in deleting comments: TIMECORR (status=%d)\n",
		    *istat);
	    break;
	  }
	}
	


	if (fits_read_record(fp, i+1, tmp_card, istat)) {
	  *istat = 0;
	  break;
	}
	if(0 == strncmp("COMMENT", tmp_card, 7)){
	  fits_delete_record(fp, i+1, istat); /** COMMENT **/
	  if(*istat){
	    fprintf(stderr,"Error in deleting comments: TIMECORR (status=%d)\n",
		    *istat);
	    break;
	  }
	}
	
	/** insert comment **/
	fits_insert_record(fp, i+1, commentb,    istat);
	if(*istat){
	  fprintf(stderr,"Error in writing keyword: TIMECORR,comment (status=%d)\n",
		  *istat);
	  break;
	}
	
	break;
      }
    }
  } /* end of hdunum*/
  return;
}
