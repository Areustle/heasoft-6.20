/*
 *  hxdcaldbUtil_trnphtbl.c
 *
 *  version 0.1,  created by Y.Terada, 2003-04-30
 *  version 0.2,  use hxdcaldbUtil, by Y.Terada, 2005-02-04
 *  version 0.2.4; 2005-05-17,
 *                 change INSTRUME and DETNAM by Y.Terada
 *                 INSTRUME is fixed to be a value of 'HXD'
 *                 DETNAM is, 'WELL_GSO' 'WELL_PIN' 'WAM_ANTI'
 *  version 0.4.0; 2005-06-13 Y.Terada
 *                 GSFC comment
 *  version 0.4.1; 2005-06-25 Y.Terada, pri herder
 *  version 0.4.3; 2005-09-28 Y.Terada,  add FILENAME
 *  version 0.4.9; 2005-09-28 Y.Terada,  boundary
 *  version 0.6.2; 2006-08-31 Y.Terada
 *                 format changed (as version 1.2.2.3 caldb)
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "hxdcaldbUtil.h"
#include "hxdFitsCommentUtil.h"

static char tool_name[] = "hxdcaldbUtil_wampht";

static FILE*     hxdtrnphtbl_ascii_fp;
static fitsfile* hxdtrnphtbl_fits_fp;

int 
hxdcaldbUtil_trnphtbl_open_ASCII (char* trnphtbl_ascii_fname){
  int status = HXDCALDBUTIL_STATUS_OK;
  
  hxdtrnphtbl_ascii_fp = fopen(trnphtbl_ascii_fname,"r");
  if (hxdtrnphtbl_ascii_fp == NULL){
    fprintf(stderr, 
            "hxdcaldbUtil: Cannot open hxdtrnphtbl ASCII file(%s)\n",
	    trnphtbl_ascii_fname);
    status = HXDCALDBUTIL_STATUS_NG;
  }

  return status;
}

static int 
hxdcaldbUtil_trnphtbl_generate_addflg ( HxdTrnPh *hxdtrnph_data) {
  int status = HXDCALDBUTIL_STATUS_OK;
  int ph;
  int bin_id, shift;

  for (ph=0; ph<HXD_TRNTBL_PH_NBIN; ph++) {
    bin_id = ph / 8;
    shift  = ph % 8;
    if ( (hxdtrnph_data->trn_bin[bin_id] << shift) & 0x80 ) {
      hxdtrnph_data->add_flg[ph] = 1;
    } else {
      hxdtrnph_data->add_flg[ph] = 0;
    }
    /* fprintf(stderr, "%d ", hxdtrnphtbl_data->add_flg[ph]); */
  }

  return status;
}

int 
hxdcaldbUtil_trnphtbl_read_ASCII (HxdTrnPhTbl *hxdtrnphtbl_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  int table_id;
  int data[HXD_TRNTBL_NBIN];
  int ph;
  static int prev_table_id = 0;
  int irow;
  hxdtrnphtbl_data->ntable = 0;

  while (1){
    /** read **/
    fscanf(hxdtrnphtbl_ascii_fp, "%d %x %x %x %x %x %x %x",
	   &table_id, &data[0], &data[1], &data[2], 
	   &data[3],  &data[4], &data[5], &data[6], &data[7]);
    if (table_id < prev_table_id) {
      fprintf(stderr, "Table ID (%d) is smaller than previous one (%d)\n",
	      table_id, prev_table_id);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    } else if (table_id > HXD_TRNTBL_NTBL_MAX) {
      fprintf(stderr, "Read EOF (table id = %d; total %d table)\n", 
	      table_id, hxdtrnphtbl_data->ntable);
      break;
    }
    /** save **/
    hxdtrnphtbl_data->trnphtbl[table_id].table_id = table_id;
    for (ph=0; ph<HXD_TRNTBL_NBIN; ph++)
      hxdtrnphtbl_data->trnphtbl[table_id].trn_bin[ph] = data[ph];
    prev_table_id = table_id;
    hxdtrnphtbl_data->ntable ++;
  }

  /******** calc add flag **********/
  for (irow = 0; irow < hxdtrnphtbl_data->ntable; irow ++) {
    status = 
      hxdcaldbUtil_trnphtbl_generate_addflg(&hxdtrnphtbl_data->trnphtbl[irow]);
    if (status != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr, "%s: Generate ADDFLG (tbl=%d) Failed\n", 
	      tool_name, irow);
      return 0;
    }
  }

  return status;
}

int 
hxdcaldbUtil_trnphtbl_close_ASCII(void){
  int status = HXDCALDBUTIL_STATUS_OK;
  if (fclose(hxdtrnphtbl_ascii_fp) ){
    fprintf(stderr, 
            "hxdcaldbUtil: hxdtrnphtbl_ascii close failed\n");
    status = HXDCALDBUTIL_STATUS_NG;
  }
  return status;
}

int 
hxdcaldbUtil_trnphtbl_create_FITS(char* trnphtbl_fits_fname){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;

  char *ttype[HXD_TRNTBL_NKEYWORD] = {
    "TABLE_ID", 
    "TRN_BIN0",  "TRN_BIN1",  "TRN_BIN2",  "TRN_BIN3", 
    "TRN_BIN4",  "TRN_BIN5",  "TRN_BIN6",
    "ADD_FLG0",  "ADD_FLG1",  "ADD_FLG2",  "ADD_FLG3",  "ADD_FLG4",
    "ADD_FLG5",  "ADD_FLG6",  "ADD_FLG7",  "ADD_FLG8",  "ADD_FLG9",
    "ADD_FLG10", "ADD_FLG11", "ADD_FLG12", "ADD_FLG13", "ADD_FLG14",
    "ADD_FLG15", "ADD_FLG16", "ADD_FLG17", "ADD_FLG18", "ADD_FLG19",
    "ADD_FLG20", "ADD_FLG21", "ADD_FLG22", "ADD_FLG23", "ADD_FLG24",
    "ADD_FLG25", "ADD_FLG26", "ADD_FLG27", "ADD_FLG28", "ADD_FLG29",
    "ADD_FLG30", "ADD_FLG31", "ADD_FLG32", "ADD_FLG33", "ADD_FLG34",
    "ADD_FLG35", "ADD_FLG36", "ADD_FLG37", "ADD_FLG38", "ADD_FLG39",
    "ADD_FLG40", "ADD_FLG41", "ADD_FLG42", "ADD_FLG43", "ADD_FLG44",
    "ADD_FLG45", "ADD_FLG46", "ADD_FLG47", "ADD_FLG48", "ADD_FLG49",
    "ADD_FLG50", "ADD_FLG51", "ADD_FLG52", "ADD_FLG53"
  };

  char *tform[HXD_TRNTBL_NKEYWORD] = {
    "1B",
    "1B", "1B", "1B", "1B",
    "1B", "1B", "1B", 
    "1X", "1X", "1X", "1X", "1X",
    "1X", "1X", "1X", "1X", "1X",
    "1X", "1X", "1X", "1X", "1X",
    "1X", "1X", "1X", "1X", "1X",
    "1X", "1X", "1X", "1X", "1X",
    "1X", "1X", "1X", "1X", "1X",
    "1X", "1X", "1X", "1X", "1X",
    "1X", "1X", "1X", "1X", "1X",
    "1X", "1X", "1X", "1X", "1X",
    "1X", "1X", "1X", "1X", "1X",
    "1X", "1X", "1X", "1X"
  };

  char *tunit[HXD_TRNTBL_NKEYWORD] = {
    "", 
    "", "", "", "",
    "", "", "", 
    "", "", "", "", "",
    "", "", "", "", "",
    "", "", "", "", "",
    "", "", "", "", "",
    "", "", "", "", "",
    "", "", "", "", "",
    "", "", "", "", "",
    "", "", "", "", "",
    "", "", "", "", "",
    "", "", "", "", "",
    "", "", "", "",
  };

  char *minmax_card[] = {
    "TLMIN1  =                    0 / minimum legal value for TABLE_ID",
    "TLMAX1  =                  255 / maximum legal value for TABLE_ID",
    "TLMIN2  =                    0 / minimum legal value for TRN_BIN ",
    "TLMAX2  =                  255 / maximum legal value for TRN_BIN ",
    "TLMIN3  =                    0 / minimum legal value for TRN_BIN ",
    "TLMAX3  =                  255 / maximum legal value for TRN_BIN ",
    "TLMIN4  =                    0 / minimum legal value for TRN_BIN ",
    "TLMAX4  =                  255 / maximum legal value for TRN_BIN ",
    "TLMIN5  =                    0 / minimum legal value for TRN_BIN ",
    "TLMAX5  =                  255 / maximum legal value for TRN_BIN ",
    "TLMIN6  =                    0 / minimum legal value for TRN_BIN ",
    "TLMAX6  =                  255 / maximum legal value for TRN_BIN ",
    "TLMIN7  =                    0 / minimum legal value for TRN_BIN ",
    "TLMAX7  =                  255 / maximum legal value for TRN_BIN ",
    "TLMIN8  =                    0 / minimum legal value for TRN_BIN ",
    "TLMAX8  =                  255 / maximum legal value for TRN_BIN ",
  };

  char *keyword[HXD_TRNTBL_NKEYWORD] = {
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  ", 
    "TTYPE6  ", "TTYPE7  ", "TTYPE8  ", "TTYPE9  ", "TTYPE10 ",
    "TTYPE11 ", "TTYPE12 ", "TTYPE13 ", "TTYPE14 ", "TTYPE15 ", 
    "TTYPE16 ", "TTYPE17 ", "TTYPE18 ", "TTYPE19 ", "TTYPE20 ",
    "TTYPE21 ", "TTYPE22 ", "TTYPE23 ", "TTYPE24 ", "TTYPE25 ", 
    "TTYPE26 ", "TTYPE27 ", "TTYPE28 ", "TTYPE29 ", "TTYPE30 ",
    "TTYPE31 ", "TTYPE32 ", "TTYPE33 ", "TTYPE34 ", "TTYPE35 ", 
    "TTYPE36 ", "TTYPE37 ", "TTYPE38 ", "TTYPE39 ", "TTYPE40 ",
    "TTYPE41 ", "TTYPE42 ", "TTYPE43 ", "TTYPE44 ", "TTYPE45 ", 
    "TTYPE46 ", "TTYPE47 ", "TTYPE48 ", "TTYPE49 ", "TTYPE50 ",
    "TTYPE51 ", "TTYPE52 ", "TTYPE53 ", "TTYPE54 ", "TTYPE55 ", 
    "TTYPE56 ", "TTYPE57 ", "TTYPE58 ", "TTYPE59 ", "TTYPE60 ",
    "TTYPE61 ", "TTYPE62 "
  };

  char *comment[HXD_TRNTBL_NKEYWORD] = {
    "table ID of TRN PH TABLE",
    "TRN PH BIN Compression Setting 1/7",
    "TRN PH BIN Compression Setting 2/7",
    "TRN PH BIN Compression Setting 3/7",
    "TRN PH BIN Compression Setting 4/7",
    "TRN PH BIN Compression Setting 5/7",
    "TRN PH BIN Compression Setting 6/7",
    "TRN PH BIN Compression Setting 7/7",
    "The add flag for PH0 (0=no add / 1=add                                to the previous ch; 0 nesessary)",
    "The add flag for PH1 (1=add to the prev ch)",
    "The add flag for PH2 (1=add to the prev ch)",
    "The add flag for PH3 (1=add to the prev ch)",
    "The add flag for PH4 (1=add to the prev ch)",
    "The add flag for PH5 (1=add to the prev ch)",
    "The add flag for PH6 (1=add to the prev ch)",
    "The add flag for PH7 (1=add to the prev ch)",
    "The add flag for PH8 (1=add to the prev ch)",
    "The add flag for PH9 (1=add to the prev ch)",
    "The add flag for PH10 (1=add to the prev ch)",
    "The add flag for PH11 (1=add to the prev ch)",
    "The add flag for PH12 (1=add to the prev ch)",
    "The add flag for PH13 (1=add to the prev ch)",
    "The add flag for PH14 (1=add to the prev ch)",
    "The add flag for PH15 (1=add to the prev ch)",
    "The add flag for PH16 (1=add to the prev ch)",
    "The add flag for PH17 (1=add to the prev ch)",
    "The add flag for PH18 (1=add to the prev ch)",
    "The add flag for PH19 (1=add to the prev ch)",
    "The add flag for PH20 (1=add to the prev ch)",
    "The add flag for PH21 (1=add to the prev ch)",
    "The add flag for PH22 (1=add to the prev ch)",
    "The add flag for PH23 (1=add to the prev ch)",
    "The add flag for PH24 (1=add to the prev ch)",
    "The add flag for PH25 (1=add to the prev ch)",
    "The add flag for PH26 (1=add to the prev ch)",
    "The add flag for PH27 (1=add to the prev ch)",
    "The add flag for PH28 (1=add to the prev ch)",
    "The add flag for PH29 (1=add to the prev ch)",
    "The add flag for PH30 (1=add to the prev ch)",
    "The add flag for PH31 (1=add to the prev ch)",
    "The add flag for PH32 (1=add to the prev ch)",
    "The add flag for PH33 (1=add to the prev ch)",
    "The add flag for PH34 (1=add to the prev ch)",
    "The add flag for PH35 (1=add to the prev ch)",
    "The add flag for PH36 (1=add to the prev ch)",
    "The add flag for PH37 (1=add to the prev ch)",
    "The add flag for PH38 (1=add to the prev ch)",
    "The add flag for PH39 (1=add to the prev ch)",
    "The add flag for PH40 (1=add to the prev ch)",
    "The add flag for PH41 (1=add to the prev ch)",
    "The add flag for PH42 (1=add to the prev ch)",
    "The add flag for PH43 (1=add to the prev ch)",
    "The add flag for PH44 (1=add to the prev ch)",
    "The add flag for PH45 (1=add to the prev ch)",
    "The add flag for PH46 (1=add to the prev ch)",
    "The add flag for PH47 (1=add to the prev ch)",
    "The add flag for PH48 (1=add to the prev ch)",
    "The add flag for PH49 (1=add to the prev ch)",
    "The add flag for PH50 (1=add to the prev ch)",
    "The add flag for PH51 (1=add to the prev ch)",
    "The add flag for PH52 (1=add to the prev ch)",
    "The add flag for PH53 (1=add to the prev ch)"
  };

  int bitpix = 8;
  int naxis = 0;
  long nrow=0;
  int ncol = sizeof(ttype)/sizeof(*ttype);
  static char extention_name[]="TRN_PH";

  int hdutype;
  int hdunum = 1;
  int minmax_id;

  char detname[16] = "WAM_ANTI";
  char code_name[16] = "TRNPHTBL";
  char description[81] = "REBIN Settings for WAM PHA spectrum";
  int use_boundary = 0;
  char *boundary = "";

  if (fits_create_file(&hxdtrnphtbl_fits_fp, trnphtbl_fits_fname, &istat)) {
    fprintf(stderr, "%s:fits_create_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if(fits_create_img(hxdtrnphtbl_fits_fp, bitpix, naxis, &nrow, &istat)) {  
    fprintf(stderr, "%s:fits_create_img failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if( fits_create_tbl( hxdtrnphtbl_fits_fp, BINARY_TBL, nrow, ncol,
		       ttype, tform, tunit, extention_name, &istat) ){
    fprintf(stderr, "%s: fits_create_tbl failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  /*  
  if( fits_write_grphdr(hxdtrnphtbl_fits_fp, 
			1, 8, 0, naxis, 0, 1, 1, &istat) ){
    fprintf(stderr, "%s:fits_write_grphdr failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if( fits_create_hdu(hxdtrnphtbl_fits_fp, &istat) ){
    fprintf(stderr, "%s:fits_create_hdu failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if ( fits_write_btblhdr(hxdtrnphtbl_fits_fp, nrow, ncol, 
                          ttype, tform, tunit, extention_name, 0, &istat) ) {
    fprintf(stderr, "%s:fits_write_btblhdr failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  */

  if (hxdcaldbUtil_write_fits_header(hxdtrnphtbl_fits_fp, 
                                     detname, code_name, description,
				     trnphtbl_fits_fname,
				     use_boundary, boundary)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  hxdFitsComment_write(hxdtrnphtbl_fits_fp, HXD_TRNTBL_NKEYWORD,
                       keyword, comment, &istat);
  if (istat){
    fprintf(stderr, "%s: fits write comment failed (%d).\n", tool_name,istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;

}

int hxdcaldbUtil_trnphtbl_write_FITS(HxdTrnPhTbl *trnphtbl_data){
  int istat = 0;
  int status = HXDCALDBUTIL_STATUS_OK;
  long firstelem = 1;
  long nelements = 1;
  int colnum;

  unsigned char fits_table_id;
  unsigned char fits_trn_bin;
  char          fits_add_flg;

  char detname[16] = "WAM_ANTI";

  int irow;
  int trnbin_id, addflg_id;
  
  for (irow=1;irow<=trnphtbl_data->ntable;irow++){
    
    colnum = 1;
    fits_table_id 
      = (unsigned char) trnphtbl_data->trnphtbl[irow-1].table_id;
    fits_write_col_byt(hxdtrnphtbl_fits_fp, colnum, irow, 
		       firstelem, nelements, 
		       &fits_table_id, &istat);
    if(istat){
      fprintf(stderr, "%s:fits_write_col_byt failed (%d)\n", tool_name, istat);
      status = HXDCALDBUTIL_STATUS_NG;
      return status;
    }
    
    for(trnbin_id=0; trnbin_id<HXD_TRNTBL_NBIN; trnbin_id ++){
      colnum ++;
      fits_trn_bin 
	= (unsigned char) trnphtbl_data->trnphtbl[irow-1].trn_bin[trnbin_id];
      fits_write_col_byt(hxdtrnphtbl_fits_fp, colnum, irow, 
			 firstelem, nelements, 
			 &fits_trn_bin, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_byt failed (%d)\n", 
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
  }
    
    for(addflg_id=0; addflg_id<HXD_TRNTBL_PH_NBIN; addflg_id ++){
      colnum ++;
      fits_add_flg 
	= (char) trnphtbl_data->trnphtbl[irow-1].add_flg[addflg_id];
      fits_write_col_bit(hxdtrnphtbl_fits_fp, colnum, irow, 
			 firstelem, nelements, 
			 &fits_add_flg, &istat);
      if(istat){
	fprintf(stderr, "%s:fits_write_col_bit failed (%d)\n", 
		tool_name, istat);
	status = HXDCALDBUTIL_STATUS_NG;
	return status;
      }
    }
    
  }
  
  if (fits_modify_key_lng(hxdtrnphtbl_fits_fp, "NAXIS2", irow-1,
			  "&", &istat)){
    fprintf(stderr, "%s:fits_update_key NAXIS2 failed (%d)\n", 
	    tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  if (fits_write_date(hxdtrnphtbl_fits_fp, &istat)) {
    fprintf(stderr,"%s: fits_write_date failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  if (fits_write_chksum(hxdtrnphtbl_fits_fp, &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }


  /******** Add Primary Header keywords ********/
  if (hxdcaldbUtil_write_fits_priheader(hxdtrnphtbl_fits_fp, detname)
      != HXDCALDBUTIL_STATUS_OK){
    fprintf(stderr, "%s: write_fits_header failed.\n", tool_name);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  
  return status;

}

int 
hxdcaldbUtil_trnphtbl_close_FITS(void){
  int status = HXDCALDBUTIL_STATUS_OK;
  int istat  = 0;
  if(fits_close_file(hxdtrnphtbl_fits_fp, &istat)){
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", tool_name, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }

  return status;

}

int hxdcaldbUtil_trnphtbl_open_FITS(char* trnphtbl_fits_fname){
  int status = HXDCALDBUTIL_STATUS_OK;

  int istat  = 0;

  if (fits_open_file(&hxdtrnphtbl_fits_fp, trnphtbl_fits_fname, 
		     READONLY, &istat)) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name, 
            trnphtbl_fits_fname, istat);
    status = HXDCALDBUTIL_STATUS_NG;
    return status;
  }
  return status;
}

int hxdcaldbUtil_trnphtbl_read_FITS(HxdTrnPhTbl *trnphtbl_data){
  int status = HXDCALDBUTIL_STATUS_OK;
  return status;
}
