/*
 * TYPE: ASTE_ANL(ftool)
 * NAME: HXDmktrngainhist
 *
 * file: HXDmktrngainhist
 *
 * PURPOSE:
 *   Make Gain history table for HXD-Anti Counters per one side.
 *
 * HISTORY:
 *       version 0.0.1  1999.10.17 created  by Y. Terada
 *             original is "HXDmkgainhist version 0.0.3" M. Sugiho
 *             install for anti counter calibration.
 *       version 0.0.2  1999.10.31 modefied by Y. Terada
 *             change #include
 *       version 0.0.3  1999.12.24 modefied by Y. Terada
 *             Change param name for the third release of HXD-FTOOLS.
 *       version 0.1.0             Y.Terada  2003-07-23 
 *         for HEADAS, PIL
 *       version 0.1.1             Y.Terada  2004-03-13
 *         change pfiles, Boolian logic
 *       version 0.1.2             Y.Terada  2006-08-25
 *         debug
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "atFunctions.h"
#include "aste_rand.h"
#include "fitsio.h"
#include "cfortran.h"
#include "hbook.h"
#include "HXD.h"

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"

#include "HXDmktrngainhist.h"
#include "hxdFitsHeaderUtil.h"

#define MAX_LENGTH PIL_LINESIZE
#define END_TIME_PLUS 100.0

char HXDmktrngainhist_version[] = "version 0.1.2";

static char pname[] = "HXDmktrngainhist";

static struct {
  char fitlog_filename[MAX_LENGTH];
  char gainhistory_filename[MAX_LENGTH];
  int add_status;
  fitsfile *trn_fitlog_fp;
  fitsfile *trn_gainhistory_fp;
  int colnum[HXD_GAIN_HIST_KEY_NUM];
  long irow;
  double prev_time;
  int first_flag;
}com;

static HxdTrnGainHist trn;

static HxdGainHistKey stdkeys;

void
HXDmktrngainhist_startup(int *status){ *status = ANL_OK; }

void
HXDmktrngainhist_com(int *status) {
    
    char add_string[MAX_LENGTH];
    int gh_iomode;
    
    if ( *status ) { /* ftools */
	
	*status = 0;
	/*
	UCLGST("trn_fitlog_name", com.fitlog_filename, *status);
	
	UCLGST("trn_gainhist_name", com.gainhistory_filename,
	       *status);
	
	UCLGST("gainhistory_iomode", add_string, *status);
	*/

	*status = PILGetFname("trn_fitlog_name", com.fitlog_filename);
	if ( *status ) {
	  fprintf(stderr, "%s:PILGetFname trn_fitlog_name err (%d)\n",
                  pname, *status);
	    *status = ANL_QUIT;
	    exit(-1);
	}
	
	*status = PILGetFname("trn_gainhist_name", com.gainhistory_filename);
	if ( *status ) {
	  fprintf(stderr, "%s:PILGetFname trn_gainhist_name err (%d)\n",
                  pname, *status);
	    *status = ANL_QUIT;
	    exit(-1);
	}
	/*
	*status = PILGetString("gainhistory_iomode", add_string);
	*/
	*status = PILGetBool("gainhistory_iomode", &gh_iomode);
	if ( *status ) {
	  fprintf(stderr, "%s:PILGetString gainhistory_iomode err (%d)\n",
                  pname, *status);
	    *status = ANL_QUIT;
	    exit(-1);
	}

	if( gh_iomode == 1 ) {
	    com.add_status = STATUS_ADD;
	} else if( gh_iomode == 0 ) {
	    com.add_status = STATUS_CREATE;
	} else {
	    fprintf(stderr, "%s : no such answer : yes or no\n",
		    add_string);
	    *status = ANL_QUIT;
	    exit(-1);
	}
	
	*status = ANL_OK;
	return;
    }
    /* ANL */
    
    com.gainhistory_filename[0] = '\0';
    com.fitlog_filename[0] = '\0';
    add_string[0] = '\0';
    
    CLtxtrd("HXD TRN input fit log file name",
	    com.fitlog_filename, sizeof(com.fitlog_filename) );
    
    CLtxtrd("HXD TRN gain history file name",
	    com.gainhistory_filename, sizeof(com.gainhistory_filename) );
    
    CLtxtrd("Add output to existing file",
	    add_string, sizeof(add_string) );
    
    if(!strcmp("yes", add_string)){
	com.add_status = STATUS_ADD;
    } else if(!strcmp("no", add_string)){
	com.add_status = STATUS_CREATE;
    } else {
	fprintf(stderr, "%s : no such answer : yes or no\n",
		add_string);
	*status = ANL_QUIT;
	exit(-1);
    }
  
    *status = ANL_OK;
  return;
  
}

void
HXDmktrngainhist_init(int *status) {
  int unit;

  for (unit = 0; unit < HXD_ANTI_N_UNIT; unit++){
    trn.gain       [unit] = 1.000;
    trn.gain_error [unit] = 0.0;
    trn.offset     [unit] = 0.000; 
    trn.offset_error[unit]= 0.0;
  }
  
  *status = ANL_OK;
  return;
}

void
HXDmktrngainhist_his(int *status){ *status = ANL_OK; }

void
HXDmktrngainhist_bgnrun(int *status){
  int istat = 0;

  if( com.add_status == STATUS_ADD ){
      HXDmktrngainhist_fits_add_bgnrun(&istat);
        if(istat == ANL_NG) {
            fprintf(stderr, "%s: fits_add_bgnrun failed\n",pname);
            *status = ANL_QUIT;
            return;
        }
  } else if( com.add_status == STATUS_CREATE ) {
      HXDmktrngainhist_fits_create_bgnrun(&istat);
        if(istat == ANL_NG) {
            fprintf(stderr, "%s: fits_create_bgnrun failed\n",pname);
            *status = ANL_QUIT;
            return;
        }
  } else {
    *status = ANL_QUIT;
    exit(-1);
  }
  
  HXDmktrngainhist_col_num( com.trn_gainhistory_fp, com.colnum, &istat );
  
  if ( istat ) {
    *status = ANL_QUIT;
    return;
  }
  
  com.first_flag = 1;
  
  *status = ANL_OK;
  return;
  
}

void
HXDmktrngainhist_ana(int nevent, int eventid, int *status){
  
  int size;
  int istat = 0;
  
  double time;
  static int counts;
  
  BnkfGetM ( "HXD:TRN:EV_TIME", sizeof(double) , &size, &time );

  if(time <= 0.0000){
    *status = ANL_OK;
    return;
  }

  if( com.first_flag ){    
    counts = 0;
    com.prev_time = time;

    if( com.add_status == STATUS_CREATE ){
      com.prev_time -=END_TIME_PLUS;
    }
    
    com.first_flag = 0;
    *status = ANL_OK;
    return;
  }

  counts++;
  
  if(   time   > com.prev_time + TRN_EXPOSURE_TIME
     && counts > TRN_EXPOSURE_COUNTS              ){
    
    trn.start_time = com.prev_time;
    trn.exposure_time = time - com.prev_time;
    
    com.irow++;
    HXDmktrngainhist_col_write( com.trn_gainhistory_fp, com.irow, com.colnum,
			       &trn, com.prev_time, &istat );
    if( istat ){
      fprintf(stderr,"%s:  col_write failed (%d)\n",
	      pname, istat);
      *status = ANL_QUIT;
      return;
    }
    
    counts = 0;
    com.prev_time = time;    
  }
  
  HXDmktrngainhist_timeUpdate (&stdkeys, time, &istat);
  if( istat ){
      fprintf(stderr,"%s:  Time Update failed (%d)\n",
	      pname, istat);
      *status = ANL_QUIT;
      return;
    }
  
  *status = ANL_OK;
  return;
  
}

void
HXDmktrngainhist_endrun(int *status){
  
  int istat=0;
  
  trn.start_time = com.prev_time;
  trn.exposure_time = stdkeys.tstop - com.prev_time;
  
  com.irow++;
  
  HXDmktrngainhist_col_write( com.trn_gainhistory_fp, com.irow, com.colnum,
			     &trn, com.prev_time, &istat );  
  if(istat){
    fprintf(stderr,"%s: col_write failed (%d)\n",
	    pname, istat);
    *status = ANL_QUIT;
    return;
  }
  
  stdkeys.tstop  += END_TIME_PLUS;
  stdkeys.tstart -= END_TIME_PLUS;

  /*  
  com.irow++;
  
  HXDmktrngainhist_col_null( com.trn_gainhistory_fp, com.irow, com.colnum,
			    stdkeys.tstop, &istat );
  if(istat){
    fprintf(stderr,"%s: col_null failed (%d)\n",
	    pname, istat);
    *status = ANL_QUIT;
    return;
  }
  */

  *status = ANL_OK;
  return;
  
}

void
HXDmktrngainhist_exit(int *status){
  int istat = 0;
  
  HXDmktrngainhist_updateStdTimeKeys(com.trn_gainhistory_fp, stdkeys, 1,
				     com.add_status, &istat);
  if( istat ) {
      fprintf(stderr,"%s: Error in updateStdTimeKeys() (status=%d)\n",
	      pname, istat);
      *status = ANL_QUIT;
      return;
  }
  
  if (fits_write_date(com.trn_gainhistory_fp, &istat)) {
    fprintf(stderr,"%s: fits_write_date() failed (status=%d)\n", pname,
            istat);
    *status = ANL_QUIT;
    return;
  }
  
  if (fits_write_chksum(com.trn_gainhistory_fp, &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }
  
  HXDmktrngainhist_updateStdTimeKeys(com.trn_gainhistory_fp, stdkeys, 2,
				     com.add_status, &istat);
  if( istat ){
    fprintf(stderr,"%s: Error in updateStdTimeKeys() (status=%d)\n",
            pname, istat);
    *status = ANL_QUIT;
    return;
  }

  if (fits_modify_key_lng(com.trn_gainhistory_fp, "NAXIS2", com.irow, "&",
			  &istat)) {
    fprintf(stderr, "%s:fits_update_key NAXIS2 failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }
  
  if (fits_write_date(com.trn_gainhistory_fp, &istat)) {
    fprintf(stderr,"%s: fits_write_date() failed (status=%d)\n", pname,
            istat);
    *status = ANL_QUIT;
    return;
  }
  
  if (fits_write_chksum(com.trn_gainhistory_fp, &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }
  
  fits_close_file(com.trn_gainhistory_fp, &istat);
  
  if ( istat ) {
    fprintf(stderr, "%s:fits_close_file failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }
  
  *status = ANL_OK;
  return;
}

/** =============== END OF HXDmktrngainhist.c ====================== **/

static void
HXDmktrngainhist_fits_add_bgnrun( int *istat ){
    int hdutype, hdunum;
    char comment[81];
    
    fits_open_file(&com.trn_gainhistory_fp, com.gainhistory_filename,
		   READWRITE, istat);
    if( istat ){
	fprintf(stderr, "%s: fits_open_file %s failed (%d)\n", pname,
		com.gainhistory_filename, *istat);
	*istat = ANL_QUIT;
	exit(-1);
    }

    hdunum = 2;
    fits_movabs_hdu(com.trn_gainhistory_fp, hdunum, &hdutype, istat);
    if ( istat ) {
      fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, *istat);
      *istat = ANL_QUIT;
      return;
    } else {
      fits_read_key_lng(com.trn_gainhistory_fp, "NAXIS2", &com.irow,
			comment, istat);
    }    
    if ( istat ) {
      fprintf(stderr, "%s: fits_read_key('NAXIS2') failed (%d)\n",
	      pname, istat);
      *istat = ANL_QUIT;
      return;
    }

    com.irow--;

}

static void
HXDmktrngainhist_fits_create_bgnrun( int *istat ){
    int primary_bitpix = HXD_GAIN_HIST_KEY_NUM;
    int primary_naxis = 0;
    long primary_nrow=0;
    char buff[80];
    int hdunum;
    
    HXDmktrngainhist_setDefaultKeywordValues(&stdkeys);
    sprintf(buff, "ANL: %s: %s", pname, HXDmktrngainhist_version);
    stdkeys.creator=strdup(buff);
    
    fits_create_file(&com.trn_gainhistory_fp, com.gainhistory_filename,
		     istat);
    if ( *istat ) {
      fprintf(stderr, "%s: fits_create_file %s failed (%d)\n", pname,
	      com.gainhistory_filename, *istat);
      *istat = ANL_QUIT;
      exit(-1);
    }
    
    fits_create_img(com.trn_gainhistory_fp,
		    primary_bitpix, primary_naxis, &primary_nrow, istat);
    if ( *istat ) {
      fprintf(stderr, "%s:fits_create_file failed (%d)\n", pname, *istat);
      *istat = ANL_QUIT;
      return;
    }
    
    hdunum = 1;
    HXDmktrngainhist_writeHXDStdKeys(com.trn_gainhistory_fp, stdkeys,
				     hdunum, istat);
    if( *istat ) {
	fprintf(stderr,"%s: Error in writeHXDStdKey() (istat=%d)\n",
		pname, *istat);
	*istat = ANL_QUIT;
	return;
    }
    
    HXDmktrngainhist_create_tbl( com.trn_gainhistory_fp, istat );
    
    if ( *istat ) {
	fprintf(stderr,"%s: Error in HXDmktrngainhist_create_tbl(istat=%d)\n",
		pname, *istat);
	*istat = ANL_QUIT;
      return;
    }

    hdunum = 2;
    HXDmktrngainhist_writeHXDStdKeys(com.trn_gainhistory_fp,
				     stdkeys, hdunum, istat);
    if( *istat ) {
	fprintf(stderr,"%s: Error in writeHXDStdKey() (istat=%d)\n",
		pname, *istat);
	*istat = ANL_QUIT;
	return;
    }
    
    com.irow = 0;
    
}

static void
HXDmktrngainhist_create_tbl( fitsfile *fp, int *istat ){
  
  long naxis = 0;
  
  static char extention_name[]="TRN_GAIN_HISTORY";
  
  char *keyword[]={
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  ", "TTYPE6  ",
    "TTYPE7  ", "TTYPE8  ", "TTYPE9  ", "TTYPE10 ", "TTYPE11 ", "TTYPE12 ",
    "TTYPE13 ", "TTYPE14 ", "TTYPE15 ", "TTYPE16 ", "TTYPE17 ", "TTYPE18 ",
    "TTYPE19 ", "TTYPE20 "
  };

  fits_create_tbl( fp, BINARY_TBL, naxis, HXD_GAIN_HIST_KEY_NUM,
                  hxd_trn_gainhistory_keyword, hxd_trn_gainhistory_format,
                  hxd_trn_gainhistory_unit, extention_name, istat);
  
  if ( *istat ) {
    fprintf(stderr, "%s:fits_create_tbl failed (%)\n", pname, *istat);
    return;
  }
  
  hxdFitsComment_write ( fp, HXD_GAIN_HIST_KEY_NUM, keyword,
                        hxd_trn_gainhistory_comment, istat);
  
  if(*istat){
    fprintf(stderr, "%s: hxdFitsComment_write failed(%d)\n",pname, *istat);
    return;
  }
  
}

static void
HXDmktrngainhist_col_num(  fitsfile *fp, int *colnum, int *istat ){
  int i;
  int casesen = TRUE;
  
  for( i=0;i<HXD_GAIN_HIST_KEY_NUM;i++ ){
      fits_get_colnum( fp, casesen, hxd_trn_gainhistory_keyword[i],
		      &colnum[i], istat);
      
      if ( *istat ) {
	  fprintf(stderr, "%s: fits_get_colnum('%s') failed (%d)\n",
		  pname, hxd_trn_gainhistory_keyword[i], *istat);
	  return;
      }
  }
  
}

static void
HXDmktrngainhist_col_write(fitsfile *fp, long irow, int *colnum,
			   HxdTrnGainHist *fits, double time,
			   int *istat ){
  long firstelem = 1;
  long nelements = 1;
  
  int yyyymmdd,hhmmss;
  AtTime attime;
  
  aste2attime(time, &attime);
  yyyymmdd = attime.yr*10000 + attime.mo*100 + attime.dy;
  hhmmss = attime.hr*10000 + attime.mn*100 + attime.sc;
    
  fits_write_col_dbl(fp, colnum[TIME], irow, firstelem, nelements,
                     &fits->start_time, istat);
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TIME=%f failed (%d)\n",
            pname, fits->start_time, *istat);
    return;
  } else {
    fits_write_col_int(fp, colnum[YYYYMMDD], irow, firstelem, nelements,
                       &yyyymmdd, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col YYYYMMDD=%d failed (%d)\n",
            pname, yyyymmdd, *istat);
    return;
  } else {
    fits_write_col_int(fp, colnum[HHMMSS], irow, firstelem, nelements,
                       &hhmmss, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col HHMMSS=%d failed (%d)\n",
            pname, hhmmss, *istat);
    return;
  } else { 
    fits_write_col_dbl(fp, colnum[EXPOSURE_TIME], irow, firstelem, nelements,
                       &fits->exposure_time, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col EXPOSURE_TIME=%f failed (%d)\n",
            pname, fits->exposure_time, *istat);
    return;
  } else {
      /************** TPU0 *********************/
    fits_write_col_dbl(fp, colnum[GAIN0], irow, firstelem, HXD_TPU_N_BOARD,
                       &fits->gain[0], istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN0 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[GAIN_ERROR0], irow, firstelem,
                       HXD_ANTI_N_UNIT, &fits->gain_error[0], istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN_ERROR0 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[OFFSET0], irow, firstelem,
                       HXD_ANTI_N_UNIT, &fits->offset[0], istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET0 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[OFFSET_ERROR0], irow, firstelem,
                       HXD_ANTI_N_UNIT, &fits->offset_error[0], istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET_ERROR0 failed (%d)\n",
            pname, *istat);
    return;
  } else {
      /************** TPU0 *********************/
    fits_write_col_dbl(fp, colnum[GAIN0], irow, firstelem, HXD_ANTI_N_UNIT,
                       &fits->gain[0], istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN0 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[GAIN_ERROR0], irow, firstelem,
                       HXD_ANTI_N_UNIT, fits->gain_error, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN_ERROR0 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[OFFSET0], irow, firstelem,
                       HXD_ANTI_N_UNIT, fits->offset, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET0 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[OFFSET_ERROR0], irow, firstelem,
                       HXD_ANTI_N_UNIT, fits->offset_error, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET_ERROR0 failed (%d)\n",
            pname, *istat);
    return;
  } else {
      /************** TPU1 *********************/
    fits_write_col_dbl(fp, colnum[GAIN1], irow, firstelem, HXD_ANTI_N_UNIT,
                       fits->gain, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN1 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[GAIN_ERROR1], irow, firstelem,
                       HXD_ANTI_N_UNIT, fits->gain_error, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN_ERROR1 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[OFFSET1], irow, firstelem,
                       HXD_ANTI_N_UNIT, fits->offset, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET1 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[OFFSET_ERROR1], irow, firstelem,
                       HXD_ANTI_N_UNIT, fits->offset_error, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET_ERROR1 failed (%d)\n",
            pname, *istat);
    return;
  } else {
      /************** TPU2 *********************/
    fits_write_col_dbl(fp, colnum[GAIN2], irow, firstelem, HXD_ANTI_N_UNIT,
                       fits->gain, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN2 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[GAIN_ERROR2], irow, firstelem,
                       HXD_ANTI_N_UNIT, fits->gain_error, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN_ERROR2 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[OFFSET2], irow, firstelem,
                       HXD_ANTI_N_UNIT, fits->offset, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET2 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[OFFSET_ERROR2], irow, firstelem,
                       HXD_ANTI_N_UNIT, fits->offset_error, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET_ERROR2 failed (%d)\n",
            pname, *istat);
    return;
  } else {
      /************** TPU3 *********************/
    fits_write_col_dbl(fp, colnum[GAIN3], irow, firstelem, HXD_ANTI_N_UNIT,
                       fits->gain, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN3 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[GAIN_ERROR3], irow, firstelem,
                       HXD_ANTI_N_UNIT, fits->gain_error, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN_ERROR3 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[OFFSET3], irow, firstelem,
                       HXD_ANTI_N_UNIT, fits->offset, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET3 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[OFFSET_ERROR3], irow, firstelem,
                       HXD_ANTI_N_UNIT, fits->offset_error, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET_ERROR3 failed (%d)\n",
            pname, *istat);
    return;
  }

  *istat = ANL_OK;
}

static void
HXDmktrngainhist_timeUpdate (HxdGainHistKey *stdkeys, double aetime,
			     int *istat){
    
  if ( 0 == (int) stdkeys->tstart || aetime < stdkeys->tstart ) {
    stdkeys->tstart = aetime;
  }
  
  if ( 0 == (int) stdkeys->tstop || stdkeys->tstop < aetime ) {    
    stdkeys->tstop = aetime;
  }
  
  *istat = ANL_OK;  
}

/*
static void
HXDmktrngainhist_col_null (fitsfile *fp, long irow, int *colnum,
			   double time,   int *istat ){
  
  long firstelem = 1;
  long nelements = 1;

  double exposure_time = 0;
  
  int yyyymmdd,hhmmss;
  AtTime attime;
  
  aste2attime(time, &attime);
  yyyymmdd = attime.yr*10000 + attime.mo*100 + attime.dy;
  hhmmss = attime.hr*10000 + attime.mn*100 + attime.sc;
  
  fits_write_col_dbl(fp, colnum[TIME], irow, firstelem, nelements,
                     &time, istat);
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TIME=%f failed (%d)\n",
            pname, time, *istat);
    return;
  } else {
    fits_write_col_int(fp, colnum[YYYYMMDD], irow, firstelem, nelements,
                       &yyyymmdd, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col YYYYMMDD=%d failed (%d)\n",
            pname, yyyymmdd, *istat);
    return;
  } else {
    fits_write_col_int(fp, colnum[HHMMSS], irow, firstelem, nelements,
                       &hhmmss, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col HHMMSS=%d failed (%d)\n",
            pname, hhmmss, *istat);
    return;
  } else { 
    fits_write_col_dbl(fp, colnum[EXPOSURE_TIME], irow, firstelem, nelements,
                       &exposure_time, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col EXPOSURE_TIME failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[GAIN0], irow, firstelem,
                        HXD_ANTI_N_UNIT, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN0 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[GAIN_ERROR0], irow, firstelem,
                       HXD_ANTI_N_UNIT, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN_ERROR0 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[OFFSET0], irow, firstelem,
                        HXD_ANTI_N_UNIT, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET0 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[OFFSET_ERROR0], irow, firstelem,
                        HXD_ANTI_N_UNIT, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET_ERROR0 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[GAIN1], irow, firstelem,
                        HXD_ANTI_N_UNIT, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN1 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[GAIN_ERROR1], irow, firstelem,
                       HXD_ANTI_N_UNIT, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN_ERROR1 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[OFFSET1], irow, firstelem,
                        HXD_ANTI_N_UNIT, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET1 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[OFFSET_ERROR1], irow, firstelem,
                        HXD_ANTI_N_UNIT, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET_ERROR1 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[GAIN2], irow, firstelem,
                        HXD_ANTI_N_UNIT, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN2 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[GAIN_ERROR2], irow, firstelem,
                       HXD_ANTI_N_UNIT, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN_ERROR2 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[OFFSET2], irow, firstelem,
                        HXD_ANTI_N_UNIT, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET2 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[OFFSET_ERROR2], irow, firstelem,
                        HXD_ANTI_N_UNIT, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET_ERROR2 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[GAIN3], irow, firstelem,
                        HXD_ANTI_N_UNIT, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN3 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[GAIN_ERROR3], irow, firstelem,
                       HXD_ANTI_N_UNIT, istat);
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GAIN_ERROR3 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[OFFSET3], irow, firstelem,
                        HXD_ANTI_N_UNIT, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET3 failed (%d)\n",
            pname, *istat);
    return;
  } else {
    fits_write_col_null(fp, colnum[OFFSET_ERROR3], irow, firstelem,
                        HXD_ANTI_N_UNIT, istat);    
  }
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col OFFSET_ERROR3 failed (%d)\n",
            pname, *istat);
    return;
  } 
  
}
*/

static void
HXDmktrngainhist_setDefaultKeywordValues(HxdGainHistKey *stdkeys){
  
  stdkeys->telescope=strdup("ASTRO-E");
  stdkeys->instrument=strdup("HXD");
  stdkeys->tstart=0.0;
  stdkeys->tstop=0.0;
  stdkeys->mjdref=51544;
  stdkeys->timeref=strdup("LOCAL");
  stdkeys->timesys=2000.0;
  stdkeys->timeunit=strdup("s");
  
}

static void
HXDmktrngainhist_writeHXDStdKeys(fitsfile * fp,  HxdGainHistKey stdkeys,
                                 int hdunum, int *istat){
  int hdutype;
  
  char comment[73];

  if (hdunum != 0) {
    if (fits_movabs_hdu(fp, hdunum, &hdutype, istat)) {
      fprintf(stderr, "%s: fits_movabs_hdu failed (istat=%d)\n",pname,
              *istat);
      *istat=ANL_NG;
    }
  }
  
  if (fits_write_key_str(fp, "TELESCOP", stdkeys.telescope, 
                         "Telescope (mission) name", istat)) {
    fprintf(stderr,"Error in writing keyword: TELESCOP (istat=%d)\n",*istat);
    *istat=ANL_NG;
  }
  if (fits_write_key_str(fp, "INSTRUME", stdkeys.instrument, 
                         "instrument name", istat)) {
    fprintf(stderr,"Error in writing keyword: INSTRUME (istat=%d)\n",*istat);
    *istat=ANL_NG;
  }
  if (fits_write_key_dbl(fp, "TSTART",   stdkeys.tstart, 15, 
                         "time start", istat)){
    fprintf(stderr,"Error in writing keyword: TSTART (istat=%d)\n",*istat);
    *istat=ANL_NG;
  }
  if (fits_write_key_dbl(fp, "TSTOP",    stdkeys.tstop, 15,  
                         "time stop", istat)){
    fprintf(stderr,"Error in writing keyword: TSTOP (istat=%d)\n",*istat);
    *istat=ANL_NG;
  }
   if (fits_write_key_fixflt(fp, "MJDREF",   stdkeys.mjdref, 1, 
                            "MJD corresponding to SC clock start (2000.0)",
                            istat)){
    fprintf(stderr,"Error in writing keyword: OBJECT (istat=%d)\n",*istat);
    *istat=ANL_NG;
  }
  if (fits_write_key_str(fp, "TIMEREF",  stdkeys.timeref, 
                         "Barycentric correction not applied to times",
                         istat)){
    fprintf(stderr,"Error in writing keyword: TIMEREF (istat=%d)\n",*istat);
    *istat=ANL_NG;
  }
  if (fits_write_key_flt(fp, "TIMESYS",  stdkeys.timesys, 1, 
                         "Time measured from 2000 Jan 1 00:00 UT", istat)){
    fprintf(stderr,"Error in writing keyword: TIMESYS (istat=%d)\n",*istat);
    *istat=ANL_NG;
  }
  if (fits_write_key_str(fp, "TIMEUNIT", stdkeys.timeunit, 
                         "unit for time related keywords", istat)){
    fprintf(stderr,"Error in writing keyword: TIMEUNIT (istat=%d)\n",*istat);
    *istat=ANL_NG;
  }
  if (fits_write_key_str(fp, "CREATOR",  stdkeys.creator, 
                         "by HXD hardware/software team",istat)){
    fprintf(stderr,"Error in writing keyword: CREATOR (istat=%d)\n",*istat);
    *istat=ANL_NG;
  }
  
  *istat=ANL_OK;
  
}


static void
HXDmktrngainhist_updateStdTimeKeys(fitsfile *fp, HxdGainHistKey stdkeys,
				   int hdunum, int add_flag, int *istat){
    int hdutype;
    char comment[] = "&";
    
    if (hdunum != 0) {
	if (fits_movabs_hdu(fp, hdunum, &hdutype, istat)) {
	    fprintf(stderr, "%s: fits_movabs_hdu failed (istat=%d)\n",pname,
		    *istat);
	    *istat=ANL_NG;
	}
    }
    
    if(add_flag == ANL_FALSE){
	if (fits_update_key_dbl (fp, "TSTART", stdkeys.tstart,
				 15, comment, istat)){
	    fprintf(stderr, "%s: ffmkys TSTART failed (%d)\n", pname, *istat);
	    *istat=ANL_NG;
	}
    }
    if (fits_update_key_dbl (fp, "TSTOP", stdkeys.tstop,
			     15, comment, istat)){
	fprintf(stderr, "%s: ffmkys TSTOP failed (%d)\n", pname, *istat);
	*istat=ANL_NG;
    }
    
    *istat=ANL_OK;
    
}
