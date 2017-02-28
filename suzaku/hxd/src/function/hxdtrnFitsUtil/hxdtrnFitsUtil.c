/*
   v0.0.1 test version created by M. sugiho, 1999-09-05
   v0.0.8 Astro-E2 version Y.Terada 2003-04-07
   v0.1.0 maintenanced by Y.Terada 2003-05-18
   v0.1.1 change format of FLG info, Y.Terada 2003-10-27
   v0.1.2 add ETI, and change TRN_TI to TI Y.Terada 2003-12-20
   v0.1.3 1B -> 1X, FLG/FRZ etc.. M.Suzuki, Y.Terada 2003-12-23
   v0.1.4 Add DATA_SIZE for illigal data, M.Suzuki, Y.Terada 2003-12-23
   v0.1.5 change ETI format                 Y.Terada 2004-03-12
   v0.1.6 debug GB_TRG (5X)                 Y.Terada 2004-06-01
   v0.2.0 change AETIME to S_TIME           Y.Terada 2005-05-04
   v0.2.1 delete ETI                        Y.Terada 2005-05-17
   v0.2.2 debug TLMAX/TLMIN                 Y.Terada 2005-05-18
   v0.2.3 aetime and s_time                 Y.Terada 2005-05-24
   v0.2.4 delete unit of TRNTIME            Y.Terada 2005-06-13
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <anl.h>
#include <bnk.h>
#include <fitsio.h>
#include "hxdtrnFitsUtil.h"
#include "hxdFitsCommentUtil.h"

static char *pname = "hxdtrnFitsUtil";

enum{
  TIME, S_TIME, TRNTIME, TI, TRN_IBLOCK,
  TRN_AE_DE_LENGTH_CHK, TRN_DE_MODULE, TRN_TBL_ID, TRN_DE_BLOCK,
  TRN_RDBIN, TRN_AE_BLOCK, TRN_DATA_SIZE, TRN_SYS_ERR_CODE,
  TRN_GB_TIME, TRN_GB_FLG, TRN_TIME_MODE, TRN_RBM,
  TRN_GB_FRZ, TRN_DT_MODE, TRN_SUMLD_MODE, TRN_AE_MODULE, 
  TRN_GB_TRG,
  TRN_PI, TRN_PH,
  TRN_OVER_FLOW, TRN_PSEUDO,
  TRN_T_ANT0, TRN_T_ANT1, TRN_T_ANT2, TRN_T_ANT3, TRN_T_ANT4,
  TRN_UD, TRN_DEAD_TIME, TRN_SUM_LD,
  TRN_W_ANT0, TRN_W_ANT1, TRN_W_ANT2, TRN_W_ANT3,
  TRN_QUALITY /*, TRN_PACKTBL_ID */
};

static char *hxd_trn_fits_keyword[HXD_TRN_FITS_KEY_NUM] = {
  "TIME", "S_TIME", "TRNTIME", "TI", "TRN_IBLOCK",
  "TRN_LENGTH_CHK", "TRN_DE_MODULE" , "TRN_TBL_ID", "TRN_DE_BLOCK",
  "TRN_RDBIN", "TRN_AE_BLOCK", "TRN_DATA_SIZE", "TRN_SYS_ERR_CODE", 
  "TRN_GB_TIME", "TRN_GB_FLG", "TRN_TIME_MODE", "TRN_RBM",
  "TRN_GB_FRZ", "TRN_DT_MODE", "TRN_SUMLD_MODE", "TRN_AE_MODULE", 
  "TRN_GB_TRG",
  "TRN_PI", "TRN_PH",
  "TRN_OVER_FLOW", "TRN_PSEUDO",
  "TRN_T_ANT0", "TRN_T_ANT1", "TRN_T_ANT2", "TRN_T_ANT3", "TRN_T_ANT4",
  "TRN_UD", "TRN_DEAD_TIME", "TRN_SUM_LD",
  "TRN_W_ANT0", "TRN_W_ANT1", "TRN_W_ANT2", "TRN_W_ANT3",
  "TRN_QUALITY" /* , "TRN_PACKTBL_ID" */
};

static char *hxd_trn_fits_format[HXD_TRN_FITS_KEY_NUM] = {
  "1D", "1D", "1J", "1V", "1I",
  "1X", "1B", "1B", "1B",
  "1B", "1B", "1U", "1B",
  "1J", "1X", "1B", "1X",
  "1X", "1X", "1X", "1B",
  "5X",
  "54U", "54U",
  "1U", "1U",
  "1U", "1U", "1U", "1U", "1U",
  "1U", "1U", "1U",
  "1U", "1U", "1U", "1U",
  "1I" /* , "1B" */
};

static char *hxd_trn_fits_unit[HXD_TRN_FITS_KEY_NUM] = {
  "s", "s", "", "1/4096 s", "",
  "", "", "", "",
  "ch", "", "byte", "",
  "1/64 s", "", "", "",
  "", "", "", "", 
  "",
  "cnts", "cnts",
  "cnts", "cnts",
  "C", "C", "C", "C", "C",
  "C", "C", "C",
  "C", "C", "C", "C",
  "" /* , "" */
};

void hxdtrnFits_create_tbl( fitsfile *fp, int *istat ){
  
  long naxis = 0;
  
  static char extention_name[]="EVENTS";

  fits_create_tbl( fp, BINARY_TBL, naxis, HXD_TRN_FITS_KEY_NUM,
		  hxd_trn_fits_keyword, hxd_trn_fits_format,
		  hxd_trn_fits_unit, extention_name, istat);
  
  if ( *istat ) {
    fprintf(stderr, "%s:fits_create_tbl failed (%)\n", pname, *istat);
  }
  
}


void hxdtrnFits_col_num( fitsfile *fp, int *colnum, int *istat ){
  
  int i;
  
  int casesen = TRUE;
  
  for( i=0;i<HXD_TRN_FITS_KEY_NUM;i++ ){
    fits_get_colnum( fp, casesen, hxd_trn_fits_keyword[i],
		    &colnum[i], istat);
    
    if ( *istat ) {
      fprintf(stderr, "%s: fits_get_colnum('%s') failed (%d)\n",
	      pname, hxd_trn_fits_keyword[i], *istat);
      return;
    }
  }
  
}


void hxdtrnFits_col_read( fitsfile *fp, long irow, int *colnum,
			 HxdTrnFits *fits, int *istat ){
  
  long firstelem = 1;
  long nelements = 1;
  
  int anynul;
  {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[TIME], irow, firstelem, nelements,
                      nulval, &fits->time, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('TIME') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[S_TIME], irow, firstelem, nelements,
		      nulval, &fits->s_time, &anynul, istat);
    fits->aetime = fits->s_time; /** There is no information for AETIME.**/
  }
  if ( *istat ) {    
    fprintf(stderr,"%s:fits_read_col('S_TIME') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned int nulval = 0;
    fits_read_col_uint(fp, colnum[TI], irow, firstelem, nelements,
		       nulval, &fits->ti, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TI') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    short nulval = 0;
    fits_read_col_sht(fp, colnum[TRN_IBLOCK], irow, firstelem, nelements,
		      nulval, &fits->trn_iblock, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_IBLOCK') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    /*
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_AE_DE_LENGTH_CHK], irow, firstelem,
		      nelements, nulval, &fits->trn_ae_de_length_chk, &anynul,
		      istat);
    */
    long firstbit = 1;         /* first bit to read (1 = 1st) */
    int  nbits = 1;            /* number of bits to read (<= 32) */
    fits_read_col_bit(fp, colnum[TRN_AE_DE_LENGTH_CHK], irow,
                      firstbit, nbits,  &fits->trn_ae_de_length_chk, istat);

  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_LENGTH_CHK') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_DE_MODULE], irow, firstelem,
		      nelements, nulval, &fits->trn_de_module, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_DE_MODULE') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_TBL_ID], irow, firstelem,
		      nelements, nulval, &fits->trn_tblid, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_TBL_ID') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_DE_BLOCK], irow, firstelem,
		      nelements, nulval, &fits->trn_de_block, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_DE_BLOCK') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_RDBIN], irow, firstelem,
		      nelements, nulval, &fits->trn_rdbin, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_RDBIN') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_AE_BLOCK], irow, firstelem,
		      nelements, nulval, &fits->trn_ae_block, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_AE_BLOCK') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_DATA_SIZE], irow, firstelem,
		       nelements, nulval, &fits->trn_data_size, &anynul,istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_DATA_SIZE') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_SYS_ERR_CODE], irow, firstelem,
		      nelements, nulval, &fits->trn_sys_err_code, 
		      &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_SYS_ERR_CODE') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    int nulval = 0;
    fits_read_col_int(fp, colnum[TRNTIME], irow, firstelem,
		      nelements, nulval, &fits->trntime, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRNTIME') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    int nulval = 0;
    fits_read_col_int(fp, colnum[TRN_GB_TIME], irow, firstelem,
		      nelements, nulval, &fits->trn_gb_time, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_GB_TIME') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    /*
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_GB_FLG], irow, firstelem,
		      nelements, nulval, &fits->trn_gb_flg, &anynul, istat);
    */
    long firstbit = 1;         /* first bit to read (1 = 1st) */
    int  nbits = 1;            /* number of bits to read (<= 32) */
    fits_read_col_bit(fp, colnum[TRN_GB_FLG], irow,
                      firstbit, nbits,  &fits->trn_gb_flg, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_GB_FLG') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_TIME_MODE], irow, firstelem,
		      nelements, nulval, &fits->trn_time_mode, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_TIME_MODE') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    /*
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_RBM], irow, firstelem,
		      nelements, nulval, &fits->trn_rbm, &anynul, istat);
    */
    long firstbit = 1;      /* first bit to read (1 = 1st) */
    int  nbits = 1;         /* number of bits to read (<= 32) */
    fits_read_col_bit(fp, colnum[TRN_RBM], irow,
                      firstbit, nbits,  &fits->trn_rbm, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col_bit('TRN_RBM') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    /*
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_GB_FRZ], irow, firstelem,
		      nelements, nulval, &fits->trn_gb_frz, &anynul, istat);
    */
    long firstbit = 1;            /* first bit to read (1 = 1st) */
    int  nbits = 1;  /* number of bits to read (<= 32) */
    fits_read_col_bit(fp, colnum[TRN_GB_FRZ], irow,
                      firstbit, nbits,  &fits->trn_gb_frz, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_GB_FRZ') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    /*
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_DT_MODE], irow, firstelem,
		      nelements, nulval, &fits->trn_dt_mode, &anynul, istat);
    */
    long firstbit = 1;            /* first bit to read (1 = 1st) */
    int  nbits = 1;  /* number of bits to read (<= 32) */
    fits_read_col_bit(fp, colnum[TRN_DT_MODE], irow,
                      firstbit, nbits,  &fits->trn_dt_mode, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_DT_MODE') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    /*
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_SUMLD_MODE], irow, firstelem,
		      nelements, nulval, &fits->trn_sumld_mode, &anynul,istat);
    */
    long firstbit = 1;            /* first bit to read (1 = 1st) */
    int  nbits = 1;  /* number of bits to read (<= 32) */
    fits_read_col_bit(fp, colnum[TRN_SUMLD_MODE], irow,
                      firstbit, nbits,  &fits->trn_sumld_mode, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_SUMLD_MODE') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_AE_MODULE], irow, firstelem,
		      nelements, nulval, &fits->trn_ae_module, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_AE_MODULE') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    long firstbit = 1;            /* first bit to read (1 = 1st) */
    int nbits = HXD_GB_TRIG_NBIT; /* number of bits to read (<= 32) */
    char larray[HXD_GB_TRIG_NBIT];/* array of logicals corresponding to bits */
    int i;

    fits_read_col_bit(fp, colnum[TRN_GB_TRG], irow,
                      firstbit, nbits,  &larray[0], istat);
    fits->trn_gb_trg = 0x00;
    for(i=0;i<HXD_GB_TRIG_NBIT; i++) {
      fits->trn_gb_trg |= ( (larray[i]) << (HXD_GB_TRIG_NBIT - 1 -i) );
    }
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_GB_TRG') failed (%d)\n",
	    pname, *istat);
    return;    
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_PH], irow, firstelem,
		       HXD_TRN_MAX_ENE_BIN, nulval, fits->trn_ph, &anynul,
		       istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_PH')failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_PI], irow, firstelem,
		       HXD_TRN_MAX_ENE_BIN, nulval, fits->trn_pi, &anynul,
		       istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_PI') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_OVER_FLOW], irow, firstelem,
		       nelements, nulval, &fits->trn_over_flow, &anynul,
		       istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_OVER_FLOW') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_PSEUDO], irow, firstelem,
		       nelements, nulval, &fits->trn_pseudo, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_PSEUDO') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_T_ANT0], irow, firstelem,
		       nelements, nulval, &fits->trn_t_ant0, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_T_ANT0') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_T_ANT1], irow, firstelem,
		       nelements, nulval, &fits->trn_t_ant1, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_T_ANT1') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_T_ANT2], irow, firstelem,
		       nelements, nulval, &fits->trn_t_ant2, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_T_ANT2') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_T_ANT3], irow, firstelem,
		       nelements, nulval, &fits->trn_t_ant3, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_T_ANT3') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_T_ANT4], irow, firstelem,
		       nelements, nulval, &fits->trn_t_ant4, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_T_ANT4') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_UD], irow, firstelem,
		       nelements, nulval, &fits->trn_ud, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_UD') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_DEAD_TIME], irow, firstelem,
		       nelements, nulval, &fits->trn_dead_time, &anynul,
		       istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_DEAD_TIME') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_SUM_LD], irow, firstelem,
		       nelements, nulval, &fits->trn_sum_ld, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_SUM_LD') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_W_ANT0], irow, firstelem,
		       nelements, nulval, &fits->trn_w_ant0, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_W_ANT0') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_W_ANT1], irow, firstelem,
		       nelements, nulval, &fits->trn_w_ant1, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_W_ANT1') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_W_ANT2], irow, firstelem,
		       nelements, nulval, &fits->trn_w_ant2, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_W_ANT2') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_W_ANT3], irow, firstelem,
		       nelements, nulval, &fits->trn_w_ant3, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_W_ANT3') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned short nulval = 0;
    fits_read_col_usht(fp, colnum[TRN_QUALITY], irow, firstelem,
		       nelements, nulval, &fits->trn_quality, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_QUALITY') failed (%d)\n",
	    pname, *istat);
    return;
  }  /* else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[TRN_PACKTBL_ID], irow, firstelem,
		      nelements, nulval, &fits->trn_packtbl_id, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_read_col('TRN_PACKTBL_ID') failed (%d)\n",
	    pname, *istat);
    return;
  }    */
  
}

void hxdtrnFits_col_write( fitsfile *fp, long irow, int *colnum,
			    HxdTrnFits *fits, int *istat ){
  
  long firstelem = 1;
  long nelements = 1;
  
  fits_write_col_dbl(fp, colnum[TIME], irow, firstelem, nelements,
		     &fits->time, istat);
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TIME=%f failed (%d)\n",
	    pname, fits->time, *istat);
    return;
  } else {
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
    fits_write_col_sht(fp, colnum[TRN_IBLOCK], irow, firstelem, nelements,
			&fits->trn_iblock, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_IBLOCK=%d failed (%d)\n",
	    pname, fits->trn_iblock, *istat);
    return;
  } else {
    /**
    fits_write_col_byt(fp, colnum[TRN_AE_DE_LENGTH_CHK], irow, firstelem,
                       nelements, &fits->trn_ae_de_length_chk, istat);
    **/
    long  fbit = 1;     /* first bit of 7X to write(1=1st) */
    long  nbit = 1;     /* number of bits to write   */
    char  larray[1];    /* array of logicals corresponding to bits */
    larray[0] = fits->trn_ae_de_length_chk &0x01;
    fits_write_col_bit(fp, colnum[TRN_AE_DE_LENGTH_CHK], irow, fbit, nbit,
                       &larray[0], istat); 
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col_bit TRN_LENGTH_CHK=%d failed (%d)\n",
	    pname, fits->trn_ae_de_length_chk, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[TRN_DE_MODULE], irow, firstelem,
		       nelements, &fits->trn_de_module, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_DE_MODULE=%d failed (%d)\n",
	    pname, fits->trn_de_module, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[TRN_TBL_ID], irow, firstelem,
		       nelements, &fits->trn_tblid, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_TBL_ID=%d failed (%d)\n",
	    pname, fits->trn_tblid, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[TRN_DE_BLOCK], irow, firstelem,
		       nelements, &fits->trn_de_block, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_DE_BLOCK=%d failed (%d)\n",
	    pname, fits->trn_de_block, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[TRN_RDBIN], irow, firstelem,
		       nelements, &fits->trn_rdbin, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_RDBIN=%d failed (%d)\n",
	    pname, fits->trn_rdbin, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[TRN_AE_BLOCK], irow, firstelem,
		       nelements, &fits->trn_ae_block, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_AE_BLOCK=%d failed (%d)\n",
	    pname, fits->trn_ae_block, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_DATA_SIZE], irow, firstelem,
			nelements, &fits->trn_data_size, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_DATA_SIZE=%d failed (%d)\n",
	    pname, fits->trn_data_size, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[TRN_SYS_ERR_CODE], irow, firstelem,
		       nelements, &fits->trn_sys_err_code, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_SYS_ERR_CODE=%d failed (%d)\n",
	    pname, fits->trn_sys_err_code, *istat);
    return;
  } else {
    fits_write_col_int(fp, colnum[TRNTIME], irow, firstelem,
		       nelements, &fits->trntime, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRNTIME=%d failed (%d)\n",
	    pname, fits->trntime, *istat);
    return;
  } else {
    fits_write_col_int(fp, colnum[TRN_GB_TIME], irow, firstelem,
		       nelements, &fits->trn_gb_time, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_GB_TIME=%d failed (%d)\n",
	    pname, fits->trn_gb_time, *istat);
    return;
  } else {
    /*
    fits_write_col_byt(fp, colnum[TRN_GB_FLG], irow, firstelem,
		       nelements, &fits->trn_gb_flg, istat);
    */
    long  fbit = 1;     /* first bit of 7X to write(1=1st) */
    long  nbit = 1;     /* number of bits to write   */
    char  larray[1];    /* array of logicals corresponding to bits */
    larray[0] = fits->trn_gb_flg &0x01;
    fits_write_col_bit(fp, colnum[TRN_GB_FLG], irow, fbit, nbit,
                       &larray[0], istat); 
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col_bit TRN_GB_FLG=%d failed (%d)\n",
	    pname, fits->trn_gb_flg, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[TRN_TIME_MODE], irow, firstelem,
		       nelements, &fits->trn_time_mode, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_TIME_MODE=%d failed (%d)\n",
	    pname, fits->trn_time_mode, *istat);
    return;
  } else {
    /*
    fits_write_col_byt(fp, colnum[TRN_RBM], irow, firstelem,
		       nelements, &fits->trn_rbm, istat);
    */
    long  fbit = 1;     /* first bit of 7X to write(1=1st) */
    long  nbit = 1;     /* number of bits to write   */
    char  larray[1];    /* array of logicals corresponding to bits */
    larray[0] =  fits->trn_rbm & 0x01;
    fits_write_col_bit(fp, colnum[TRN_RBM], irow, fbit, nbit,
                       &larray[0], istat); 
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col_bit TRN_RBM=%d failed (%d)\n",
	    pname, fits->trn_rbm, *istat);
    return;
  } else {
    /*
    fits_write_col_byt(fp, colnum[TRN_GB_FRZ], irow, firstelem,
		       nelements, &fits->trn_gb_frz, istat);
    */
    long  fbit = 1;     /* first bit of 7X to write(1=1st) */
    long  nbit = 1;     /* number of bits to write   */
    char  larray[1];    /* array of logicals corresponding to bits */
    larray[0] = fits->trn_gb_frz &0x01;
    fits_write_col_bit(fp, colnum[TRN_GB_FRZ], irow, fbit, nbit,
                       &larray[0], istat); 
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col_bit TRN_GB_FRZ=%d failed (%d)\n",
	    pname, fits->trn_gb_frz, *istat);
    return;
  } else {
    /*
    fits_write_col_byt(fp, colnum[TRN_DT_MODE], irow, firstelem,
		       nelements, &fits->trn_dt_mode, istat);
    */
    long  fbit = 1;     /* first bit of 7X to write(1=1st) */
    long  nbit = 1;     /* number of bits to write   */
    char  larray[1];    /* array of logicals corresponding to bits */
    larray[0] = fits->trn_dt_mode &0x01;
    fits_write_col_bit(fp, colnum[TRN_DT_MODE], irow, fbit, nbit,
                       &larray[0], istat); 
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_co_bitl TRN_DT_MOD=%d failed (%d)\n",
	    pname, fits->trn_dt_mode, *istat);
    return;
  } else {
    /*
    fits_write_col_byt(fp, colnum[TRN_SUMLD_MODE], irow, firstelem,
		       nelements, &fits->trn_sumld_mode, istat);
    */
    long  fbit = 1;     /* first bit of 7X to write(1=1st) */
    long  nbit = 1;     /* number of bits to write   */
    char  larray[1];    /* array of logicals corresponding to bits */
    larray[0] = fits->trn_sumld_mode & 0x01;
    fits_write_col_bit(fp, colnum[TRN_SUMLD_MODE], irow, fbit, nbit,
                       &larray[0], istat); 
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col_bit TRN_SUMLD_MODE=%d failed (%d)\n",
	    pname, fits->trn_sumld_mode, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[TRN_AE_MODULE], irow, firstelem,
		       nelements, &fits->trn_ae_module, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_AE_MODULE=%d failed (%d)\n",
	    pname, fits->trn_ae_module, *istat);
    return;
  } else {
    /*
    fits_write_col_byt(fp, colnum[TRN_GB_TRG], irow, firstelem,
		       nelements, &fits->trn_gb_trg, istat);
    */
    long  fbit = 1;                     /* first bit of 7X to write(1=1st) */
    long  nbit = HXD_GB_TRIG_NBIT;         /* number of bits to write   */
    char  larray[HXD_GB_TRIG_NBIT];/*array of logicals corresponding to bits */
    int i;    unsigned char trig;

    trig = fits->trn_gb_trg;
    for(i=0;i<HXD_GB_TRIG_NBIT; i++) {          
      larray[HXD_GB_TRIG_NBIT-i-1] = (trig >> i) & 0x1;
    }
    fits_write_col_bit(fp, colnum[TRN_GB_TRG], irow, fbit, nbit,
                       &larray[0], istat); 
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col_bit TRN_GB_TRG=0x%x failed (%d)\n",
	    pname, fits->trn_gb_trg, *istat);
    return;    
  } else {
    fits_write_col_usht(fp, colnum[TRN_PH], irow, firstelem,
			HXD_TRN_MAX_ENE_BIN, fits->trn_ph, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_PH failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_PI], irow, firstelem,
			HXD_TRN_MAX_ENE_BIN, fits->trn_pi, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_PI failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_OVER_FLOW], irow, firstelem,
			nelements, &fits->trn_over_flow, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_OVER_FLOW=%d failed (%d)\n",
	    pname, fits->trn_over_flow, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_PSEUDO], irow, firstelem,
			nelements, &fits->trn_pseudo, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_PSEUDO=%d failed (%d)\n",
	    pname, fits->trn_pseudo, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_T_ANT0], irow, firstelem,
			nelements, &fits->trn_t_ant0, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_T_ANT0=%d failed (%d)\n",
	    pname, fits->trn_t_ant0, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_T_ANT1], irow, firstelem,
			nelements, &fits->trn_t_ant1, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_T_ANT1=%d failed (%d)\n",
	    pname, fits->trn_t_ant1, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_T_ANT2], irow, firstelem,
			nelements, &fits->trn_t_ant2, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_T_ANT2=%d failed (%d)\n",
	    pname, fits->trn_t_ant2, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_T_ANT3], irow, firstelem,
			nelements, &fits->trn_t_ant3, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_T_ANT3=%d failed (%d)\n",
	    pname, fits->trn_t_ant3, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_T_ANT4], irow, firstelem,
			nelements, &fits->trn_t_ant4, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_T_ANT4=%d failed (%d)\n",
	    pname, fits->trn_t_ant4, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_UD], irow, firstelem,
			nelements, &fits->trn_ud, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_UD=%d failed (%d)\n",
	    pname, fits->trn_ud, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_DEAD_TIME], irow, firstelem,
			nelements, &fits->trn_dead_time, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_DEAD_TIME=%d failed (%d)\n",
	    pname, fits->trn_dead_time, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_SUM_LD], irow, firstelem,
			nelements, &fits->trn_sum_ld, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_SUM_LD=%d failed (%d)\n",
	    pname, fits->trn_sum_ld, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_W_ANT0], irow, firstelem,
			nelements, &fits->trn_w_ant0, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_W_ANT0=%d failed (%d)\n",
	    pname, fits->trn_w_ant0, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_W_ANT1], irow, firstelem,
			nelements, &fits->trn_w_ant1, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_W_ANT1=%d failed (%d)\n",
	    pname, fits->trn_w_ant1, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_W_ANT2], irow, firstelem,
			nelements, &fits->trn_w_ant2, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_W_ANT2=%d failed (%d)\n",
	    pname, fits->trn_w_ant2, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_W_ANT3], irow, firstelem,
			nelements, &fits->trn_w_ant3, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_W_ANT3=%d failed (%d)\n",
	    pname, fits->trn_w_ant3, *istat);
    return;
  } else {
    fits_write_col_usht(fp, colnum[TRN_QUALITY], irow, firstelem,
			nelements, &fits->trn_quality, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_QUALITY=%d failed (%d)\n",
	    pname, fits->trn_w_ant3, *istat);
    return;
  } /* else {
    fits_write_col_byt(fp, colnum[TRN_PACKTBL_ID], irow, firstelem,
		       nelements, &fits->trn_packtbl_id, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRN_PACKTBL_ID=%d failed (%d)\n",
	    pname, fits->trn_w_ant3, *istat);
    return;
  } */

}


void hxdtrnFits_modify_trntime_unit (fitsfile *fp, int time_mode, int *istat){
    char unit[64];
    int hdutype;
    char tmp_card[81];
    int i;

    fits_movabs_hdu(fp, HXD_TRN_FITS_EVENT_EXTENTION, &hdutype, istat);
    if (*istat) {
      fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
	      pname, HXD_TRN_FITS_EVENT_EXTENTION, *istat);
      return;
    }

    switch (time_mode){
    case HXD_TRNTIME_MODE_05SEC:
      sprintf(unit, 
	      "TUNIT3  = '1/2^16 s'           / physical unit of field");
      break;
    case HXD_TRNTIME_MODE_1SEC:
      sprintf(unit, 
	      "TUNIT3  = '1/2^15 s'           / physical unit of field");
      break;
    case HXD_TRNTIME_MODE_2SEC:
      sprintf(unit, 
	      "TUNIT3  = '1/2^14 s'           / physical unit of field");
      break;
    case HXD_TRNTIME_MODE_4SEC:
      sprintf(unit, 
	      "TUNIT3  = '1/2^13 s'           / physical unit of field");
      break;
    default:
      sprintf(unit, 
	      "TUNIT3  = '1/2^15 s'           / physical unit of field");
      break;
    }

/*  fits_write_key_unit(fp, "TRNTIME ", unit, istat);*/
/*  fits_write_key_unit(fp, "TTYPE3", unit, istat); */
    for(i=1;;i++){
      if (fits_read_record(fp, i, tmp_card, istat)) {
	*istat = 0;
	break;
      }
      if(0 == strncmp("TFORM3 ", tmp_card, 7)){
	fits_insert_record(fp, i+1, unit, istat);
	if ( *istat ) {
	  fprintf(stderr,"%s:fits_write_key_unit TRNTIME failed (%d)\n",
		  pname, *istat);
	  return;
	}
	break;
      }
    }

    return;

}


void hxdtrnFits_add_tlminmax(fitsfile *fp, int *istat){
  int i,j=0;
  
  char tmp_card[81];
  
  char *card[]={
    "TLMIN7  =                    0 / minimum legal value",
    "TLMAX7  =                    3 / maximum legal value",
    "TLMIN8  =                    0 / minimum legal value",
    "TLMAX8  =                  255 / maximum legal value",
    "TLMIN9  =                    1 / minimum legal value",
    "TLMAX9  =                    7 / maximum legal value",
    "TLMIN10 =                    1 / minimum legal value",
    "TLMAX10 =                   54 / maximum legal value",
    "TLMIN11 =                    1 / minimum legal value",
    "TLMAX11 =                    7 / maximum legal value",
    "TLMIN12 =                    0 / minimum legal value",
    "TLMAX12 =                10399 / maximum legal value",
    "TLMIN21 =                    0 / minimum legal value",
    "TLMAX21 =                    3 / maximum legal value",
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
    "TLMAX33 =                65535 / maximum legal value",
    "TLMIN34 =                    0 / minimum legal value",
    "TLMAX34 =                65535 / maximum legal value",
    "TLMIN35 =                    0 / minimum legal value",
    "TLMAX35 =                65535 / maximum legal value",
    "TLMIN36 =                    0 / minimum legal value",
    "TLMAX36 =                65535 / maximum legal value",
    "TLMIN37 =                    0 / minimum legal value",
    "TLMAX37 =                65535 / maximum legal value",
    "TLMIN38 =                    0 / minimum legal value",
    "TLMAX38 =                65535 / maximum legal value",
    "TLMIN39 =                    0 / minimum legal value",
    "TLMAX39 =                65535 / maximum legal value" 
  };
   
  for(i=1;;i++){
    if (fits_flush_file(fp, istat)){
      fprintf(stderr, "%s:fits_flush_file failed (%d)\n", pname, *istat);
    }
    
    if (fits_read_record(fp, i, tmp_card, istat)) {
      /*fprintf(stderr, "%s:fits_read_record failed (%d)\n",
	 pname, *istat);*/
      *istat = 0;
      break;
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
    if(0 == strncmp("TFORM12", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM21", tmp_card, 7)){
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
    if(0 == strncmp("TFORM34", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM35", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM36", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM37", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM38", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TFORM39", tmp_card, 7)){
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

void hxdtrnFits_add_comment( fitsfile *fp, int *istat ){
  
  char *keyword[]={
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  ", "TTYPE6  ",
    "TTYPE7  ", "TTYPE8  ", "TTYPE9  ", "TTYPE10 ", "TTYPE11 ", "TTYPE12 ",
    "TTYPE13 ", "TTYPE14 ", "TTYPE15 ", "TTYPE16 ", "TTYPE17 ", "TTYPE18 ",
    "TTYPE19 ", "TTYPE20 ", "TTYPE21 ", "TTYPE22 ", "TTYPE23 ", "TTYPE24 ",
    "TTYPE25 ", "TTYPE26 ", "TTYPE27 ", "TTYPE28 ", "TTYPE29 ", "TTYPE30 ",
    "TTYPE31 ", "TTYPE32 ", "TTYPE33 ", "TTYPE34 ", "TTYPE35 ", "TTYPE36 ",
    "TTYPE37 ", "TTYPE38 ", "TTYPE39 "
  };
  
  char *comment[]={
    "Event arrival time",
    "Transient event data packet edit time",
    "Original 24bit time counter in the telemetry",
    "Secondary header time", 
    "This block number of a packet",
    "Length Check Bit by HXD-DE (1: OK / 0: NG)",
    "Tpu module ID (I/F address between DE and TPU)",   
    "ID of PH format defined by hxd_trn_ph_tbl.fits",
    "Number of blocks of a packet counted by DE",
    "Number of reduces PHA/PI bin",
    "Number of blocks of a packet counted by AE",
    "Information of Data Size in Illegal packet",
    "DE Error Code, captured by System Software",
    "Gamma burst judged time counter",
    "Gamma burst flag",
    "Tpu time mode                                  [0x00:1/2 sec][0x01:1 sec][0x02:2 sec][0x03:4 sec]",
    "Radiation Belt Monitor flag",
    "GB data memory freezed flag",
    "Dead time format of 19-bit counter             (0: Lower 16-bit, 1: Upper 16-bit)",
    "SumLD format of 19-bit counter                 (0: Lower 16-bit, 1: Upper 16-bit)",
    "Tpu module ID named by AE",
    "GB Trigger                                     [0b00010000:TPU0][0b00001000:TPU1][0b00000100:TPU2]                   [0b00000010:TPU3][0b00000001:SOFT]",
    "(54 - TRN_RDBIN)channel PI counter",
    "(54 - TRN_RDBIN)channel PH counter",
    "Over flow counter",
    "Pseudo data counter",
    "T0 LD hit counter",
    "T1 LD hit counter",
    "T2 LD hit counter",
    "T3 LD hit counter",
    "T4 LD hit counter",
    "Sum UD counter",
    "Dead time counter",
    "Sum LD counter",
    "W0 Anti counter",
    "W1 Anti counter",
    "W2 Anti counter",
    "W3 Anti counter",
    "Quality grade"
    /*    "Packig table ID for trn PH-histogram" */
  };
  
  hxdFitsComment_write ( fp, HXD_TRN_FITS_KEY_NUM, keyword, comment, istat);

  if(*istat){
    fprintf(stderr, "%s: hxdFitsComment_write failed(%d)\n",
	    pname, istat);
  }
  
}
