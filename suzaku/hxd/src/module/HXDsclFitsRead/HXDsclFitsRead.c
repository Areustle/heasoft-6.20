/*
   HXDsclFitsRead:
   
   v0.0.1 created by Y. Terada 1999-12-13
   only for SCL time assignment.
   
   *  Not to read ALL columns in _scl.hk, only TIME, AETIME, SCL_TIME.
     (if it is nesessary to read all columns, use HXDHKFitsRead)
   *  Only OVERWRITE Mode is supported.
     (READONLY, CREATE Mode is not supported.)

   v0.0.2 mode by Y.Terada 1999-12-24
    * change param name for the third release of hxd-ftools.
 		  
   v0.1.0 mode by Y.Terada 2000-05-03
    * for HXD-II. SCL is included in HK file.

   v0.2.0 mode by Y.Terada 2003-07-23
    * for HEADAS, PIL.

   v0.3.0 modified by Y.Terada, 2004-03-11
   *  for new format (HXD_SCL_BOARD --> HXD_SCL_MODULE)

   v0.3.1 modified by Y.Terada, 2004-03-12
   *  closeFITS.
   v0.4.0 modified by Y.Terada, 2005-02-16
   *  don't use HXD_SCL_MODULE, use HXD_SCL_PI_WPU_ID.

   v0.4.1 modified by Y.Terada, 2005-06-03
   *  use PACKET_S_TIME instead of PACKET_AETIME

   v0.4.2 modified by Y.Terada, 2005-11-02
   *  memory locate in Credit

   v0.4.3 modified by Y.Terada, 2005-11-04
   *  put PIL parameters in Bnk

   v0.4.4 modified by Y.Terada, 2005-11-04
   *  delete strndup.

   v0.4.5 modified by Y.Terada, 2005-11-08
   *  shorten Bnk Name

   v0.4.6 modified by Y.Terada, 2006-08-23
   *  read TSTART/TSTOP

   v0.4.7 modified by Y.Terada, 2006-08-25
   *  Bnk size of HXD:SCL:BOARD changed

   v0.4.7 modified by Y.Terada, 2011-08-13
   *  debug CHECKSUM 
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
#include "aste_time.h"
#include "aste_coord.h"
#include "aste_rand.h"
#include "fitsio.h"
#include "cfortran.h"
#include "hbook.h"
#include "hxd/HXD.h"

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"
#include "hxdFitsHeaderUtil.h"

#include "HXDsclFitsRead.h"
/* #include "hxdgtiFitsUtil.h" */

#define FILENAME_MAX_LENGTH PIL_LINESIZE
#define DEBUG 0

char HXDsclFitsRead_version[] = "version 0.4.7";

static char pname[] = "HXDsclFitsRead";

static struct {
    char filename[FILENAME_MAX_LENGTH];
    fitsfile *fp;
    int irow;
    char time[32];
} com;

static struct{
    int colnum[HXD_SCL_FITS_KEY_READ_NUM];
    long nrow;
} fits;

void
HXDsclFitsRead_startup(int *status){
  int ftools_no = 0;
  BnkDef( "HXD:PIL:input_name", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:ftools:hxdscltime_yn",       sizeof(int));
  BnkPut( "HXD:ftools:hxdscltime_yn",       sizeof(int), &ftools_no);

  *status = ANL_OK; 
}

void
HXDsclFitsRead_init( int *status ) {
    int i;
    
    /****** SCL Bnk *********************/
    BnkDef("HXD:SCL:PACKET_AETIME",   sizeof(double) ); /* SCL_AETIME */
    BnkDef("HXD:SCL:PACKET_S_TIME",   sizeof(double) ); /* SCL_S_TIME */
    BnkDef("HXD:SCL:PACKET_SEC_HEADER",   sizeof(int)); /* TI         */
    BnkDef("HXD:SCL:EV_TIME", sizeof(double) );         /* TIME       */
    BnkDef("HXD:SCL:TIME",    sizeof(int)    );         /* SCL_TIME   */
    BnkDef("HXD:SCL:BOARD",   sizeof(int)    );         /* SCL_BOARD  */
    
    /******* Fits column ****************/
    BnkDef( "HXDsclFitsRead:IROW", sizeof(long) );
    
    BnkDef( "HXDsclFitsRead:FILE_P", sizeof(fitsfile *) );
    BnkDef( "HXDsclFitsRead:FILE_NAME", sizeof(com.filename) -1);
    BnkDef( "HXDsclFitsRead:NROW", sizeof(long) );
    BnkDef( "HXDsclFitsRead:TSTART", sizeof(double) );
    BnkDef( "HXDsclFitsRead:TSTOP", sizeof(double) );
    BnkDef( "HXDsclFitsRead:HK:TBEGIN", sizeof(double) );
    BnkDef( "HXDsclFitsRead:HK:TEND", sizeof(double) );
    BnkDef( "HXDsclFitsRead:SYS:TBEGIN", sizeof(double) );
    BnkDef( "HXDsclFitsRead:SYS:TEND", sizeof(double) );

    BnkDef( "HXDsclFitsRead:EV_TIME:CHANGE", sizeof(int) );
    BnkDef( "HXDsclFitsRead:GRADE:CHANGE", sizeof(int) );
    BnkDef( "HXDsclFitsRead:PI_PMT:CHANGE", sizeof(int) );
    BnkDef( "HXDsclFitsRead:PI_PIN:CHANGE", sizeof(int) );
    
    BnkDef( "HXD:ALL:EVENTTYPE", sizeof(int) );
    
    BnkDef( "HXD:ALL:UPDATE" ,   sizeof(int) );
    
    *status = ANL_OK;
}

void
HXDsclFitsRead_com( int *status ) {
    int i;
    
    if ( *status ) { /* ftools */
	*status = 0;

	*status = PILGetFname("input_name", com.filename);
	if ( *status ) {
	  fprintf(stderr, "%s: PILGetFname input_name error (%d)\n",
                pname, *status );
	    *status = ANL_QUIT;
	    exit(-1);
	} else {
	  int string_size = strlen(com.filename);
	  if  (string_size>HXDFITSHEADER_LINESIZE) {
	    fprintf(stderr, "%s: warning input_name is long\n", pname);
	    string_size = HXDFITSHEADER_LINESIZE;
	  }
	  BnkPut("HXD:PIL:input_name", string_size, com.filename);
	}
	
	*status = ANL_OK;
	return;
    }
    
    /* ANL */
    com.filename[0] = '\0';
    
    CLtxtrd("HXD scl fits file name (= HK fits)",
	    com.filename, sizeof(com.filename));
    *status = ANL_OK;
}

void
HXDsclFitsRead_his(int *status){  *status = ANL_OK; }

static void
HXDsclFitsRead_col_num(fitsfile *fp , int *colnum , int *istat ){
    int i;
    int casesen = TRUE;
    
    for(i=0; i<HXD_SCL_FITS_KEY_READ_NUM; i++){
	fits_get_colnum(fp, casesen, hxd_scl_fits_keyword[i],
			&colnum[i], istat);
	if(*istat){
	    fprintf(stderr, "%s: fits_get_colnum('%s') failed (%d)\n",
		    pname, hxd_scl_fits_keyword[i], *istat);
	}
    }
}

static int
openFITS(){
    int istat = 0;
    
    int casesen = TRUE;
    int hdutype;
    char comment[80];
    
    fits_open_file(&com.fp, com.filename, READWRITE, &istat);
    
    if ( istat ) {
	fprintf(stderr, "%s: fits_open_file('%s') failed (%d)\n",
		pname, com.filename, istat);
	/*return ANL_NG;*/
	exit(-1);
    } else {
	fits_movabs_hdu(com.fp, HXD_SCL_FITS_EVENT_EXTENTION , &hdutype,
			&istat);
    }    
    if ( istat ) {
	fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
	return ANL_NG;
    } else {
	fits_read_key_lng(com.fp, "NAXIS2", &fits.nrow, comment,  &istat);
    }    
    if ( istat ) {
	fprintf(stderr, "%s: fits_read_key('NAXIS2') failed (%d)\n",
		pname, istat);
	return ANL_NG;
    } else {
	HXDsclFitsRead_col_num(com.fp , fits.colnum , &istat );
    } if( istat ){
	return ANL_NG;
    }
    
    return ANL_OK;
    
}

static int
reopenFITS(){
    
    int istat = 0;
    int hdutype;
    double tstart_scl, tstop_scl;
    char comment[80];
    double tbegin_hk, tend_hk, tbegin_sys, tend_sys;
    int time_col = 1;
    long nrow_hk, nrow_sys;
    int  anynul;

    fits_open_file(&com.fp, com.filename, READWRITE, &istat);
    
    if ( istat ) {
	fprintf(stderr, "%s: fits_open_file('%s') failed (%d)\n",
		pname, com.filename, istat);
	return ANL_NG;
    } else {
	fits_movabs_hdu(com.fp, HXD_SCL_FITS_EVENT_EXTENTION,
			&hdutype, &istat);
    }
    
    if ( istat ) {
	fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
	return ANL_NG;
    } else {
      fits_read_key_dbl(com.fp, "TSTART", &tstart_scl, comment, &istat);
    }

    if( istat ){
        fprintf(stderr, "%s: fits_read_key_dbl TSTART failed (%d)\n", 
	        pname, istat);
        return ANL_QUIT;
    } else {
      fits_read_key_dbl(com.fp, "TSTOP", &tstop_scl, comment, &istat);
    }

    if( istat ){
        fprintf(stderr, "%s: fits_read_key_dbl TSTOP failed (%d)\n", 
	        pname, istat);
        return ANL_QUIT;
    } 
    
    BnkfPutM("HXDsclFitsRead:NROW", sizeof(long), &fits.nrow );
    BnkfPutM("HXDsclFitsRead:FILE_NAME", strlen(com.filename)+1, com.filename);
    BnkfPutM("HXDsclFitsRead:FILE_P", sizeof(com.fp), &com.fp);
    BnkfPutM("HXDsclFitsRead:TSTART", sizeof(double), &tstart_scl );
    BnkfPutM("HXDsclFitsRead:TSTOP", sizeof(double), &tstop_scl );

    /** read HK time begin and end **/
    fits_movabs_hdu(com.fp, HXD_SCL_FITS_HK_EXTENTION, &hdutype, &istat);
    if ( istat ) {
      fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
      return ANL_NG;
    } 

    fits_read_col_dbl(com.fp, time_col, 1, 1, 1, 0.0,
		      &tbegin_hk, &anynul, &istat);
    if ( istat ) {
      fprintf(stderr, "%s: fits_read_col_dbl, TIME failed (%d)\n", 
	      pname, istat);
      return ANL_NG;
    } 

    fits_read_key_lng(com.fp, "NAXIS2", &nrow_hk, comment, &istat);
    if ( istat ) {
      fprintf(stderr, "%s: fits_read_key_lng, NAXIS2 failed (%d)\n", 
	      pname, istat);
      return ANL_NG;
    } 

    fits_read_col_dbl(com.fp, time_col, nrow_hk, 1, 1, 0.0,
		      &tend_hk, &anynul, &istat);
    if ( istat ) {
      fprintf(stderr, "%s: fits_read_col_dbl, TIME failed (%d)\n", 
	      pname, istat);
      return ANL_NG;
    } 
    BnkfPutM("HXDsclFitsRead:HK:TBEGIN", sizeof(double), &tbegin_hk );
    BnkfPutM("HXDsclFitsRead:HK:TEND", sizeof(double), &tend_hk );

    /** read SYS time begin and end **/
    fits_movabs_hdu(com.fp, HXD_SCL_FITS_SYS_EXTENTION, &hdutype, &istat);
    if ( istat ) {
      fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
      return ANL_NG;
    } 

    fits_read_col_dbl(com.fp, time_col, 1, 1, 1, 0.0,
		      &tbegin_sys, &anynul, &istat);
    if ( istat ) {
      fprintf(stderr, "%s: fits_read_col_dbl, TIME failed (%d)\n", 
	      pname, istat);
      return ANL_NG;
    } 

    fits_read_key_lng(com.fp, "NAXIS2", &nrow_sys, comment, &istat);
    if ( istat ) {
      fprintf(stderr, "%s: fits_read_key_lng, NAXIS2 failed (%d)\n", 
	      pname, istat);
      return ANL_NG;
    } 

    fits_read_col_dbl(com.fp, time_col, nrow_sys, 1, 1, 0.0,
		      &tend_sys, &anynul, &istat);
    if ( istat ) {
      fprintf(stderr, "%s: fits_read_col_dbl, TIME failed (%d)\n", 
	      pname, istat);
      return ANL_NG;
    } 
    BnkfPutM("HXDsclFitsRead:SYS:TBEGIN", sizeof(double), &tbegin_sys );
    BnkfPutM("HXDsclFitsRead:SYS:TEND", sizeof(double), &tend_sys );
    
    /** move SCL extention **/
    fits_movabs_hdu(com.fp, HXD_SCL_FITS_EVENT_EXTENTION,
		    &hdutype, &istat);
    
    if ( istat ) {
      fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
      return ANL_NG;
    }
    
    return ANL_OK;
}

static int
closeFITS(){
    int istat = 0;
    int hdutype;
    int extid;

    for(extid=1;extid<=HXD_SCL_FITS_NUM_EXTENTION;extid++){
      fits_movabs_hdu(com.fp, extid, &hdutype, &istat);
      if( istat ){
	fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
	return ANL_NG;
      } 
      
      fits_write_chksum(com.fp, &istat);
      if ( istat ) {
	fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
	return ANL_NG;
      }
    }

    fits_close_file(com.fp, &istat);
    if ( istat ) {
	fprintf(stderr, "%s: fits_close_file failed (%d)\n", pname, istat);
	return ANL_NG;
    }
    
    return ANL_OK;
}

void
HXDsclFitsRead_bgnrun(int *status) {
    int istat = 0;
    int evtyp = HXD_EVTYP_SCL;
    
    if( openFITS() ){
	*status = ANL_QUIT;
	return;
    }
    
    closeFITS();
    
    com.irow = 1;
    
    BnkfPutM("HXD:ALL:EVENTTYPE", sizeof(int), &evtyp );
    
    if( reopenFITS() ){
	*status = ANL_QUIT;
	return;
    }
    
    *status = ANL_OK;
}

void
HXDsclFitsRead_ana(int nevent, int eventid, int *status){
    int istat = 0;
    int update = HXD_UPDATE_SCL;
    long firstelem = 1;
    long nelements = 1;
    int anynul;
    
    double  time ; /* assigned time */
    double aetime; /* packet aetime */
    double s_time; /* packet s_time */
    int hxd_scl_time; /* 24-bit cnt */
    unsigned int ti;  /* packet cnt */
    unsigned char hxd_scl_board;
    
    if ( fits.nrow < com.irow ) {
	*status = ANL_QUIT;
	return;
    }
    
    {
	double nulval = 0.0;
	fits_read_col_dbl(com.fp, fits.colnum[HXD_SCL_AETIME], com.irow,
			  firstelem, nelements, nulval, &aetime, &anynul,
			  &istat);
	if ( istat ) {
	    fprintf(stderr, "%s: fits_read_col failed (%d)\n",	pname, istat);
	    *status = ANL_QUIT;
	    return;
	}
	BnkfPutM ("HXD:SCL:PACKET_AETIME", sizeof(double), &aetime);
    }

    {
	double nulval = 0.0;
	fits_read_col_dbl(com.fp, fits.colnum[HXD_SCL_S_TIME], com.irow,
			  firstelem, nelements, nulval, &s_time, &anynul,
			  &istat);
	if ( istat ) {
	    fprintf(stderr, "%s: fits_read_col failed (%d)\n",	pname, istat);
	    *status = ANL_QUIT;
	    return;
	}
	BnkfPutM ("HXD:SCL:PACKET_S_TIME", sizeof(double), &s_time);
    }

    {
	unsigned int nulval = 0.0;
	fits_read_col_uint(com.fp, fits.colnum[TI], com.irow, firstelem,
			   nelements, nulval, &ti, &anynul, &istat);
	BnkfPutM ("HXD:SCL:PACKET_SEC_HEADER", sizeof(int), &ti);
    }
    {
	int nulval = 0.0;	
	fits_read_col_int(com.fp, fits.colnum[HXD_SCL_TIME], com.irow,
			  firstelem, nelements, nulval, &hxd_scl_time,
			  &anynul, &istat);
	if ( istat ) {
	    fprintf(stderr, "%s: fits_read_col failed (%d)\n",	pname, istat);
	    *status = ANL_QUIT;
	    return;
	}
	BnkfPutM ("HXD:SCL:TIME", sizeof(int), &hxd_scl_time);
    }
    {
	unsigned char nulval = 0.0;
	int scl_board_int;
	fits_read_col_byt(com.fp, fits.colnum[HXD_SCL_PI_WPU_ID], com.irow,
			  firstelem, nelements, nulval, &hxd_scl_board,
			  &anynul, &istat);
	if ( istat ) {
	    fprintf(stderr, "%s: fits_read_col failed (%d)\n",	pname, istat);
	    *status = ANL_QUIT;
	    return;
	}
	scl_board_int = (int) hxd_scl_board;
	BnkfPutM ("HXD:SCL:BOARD", sizeof(int), &scl_board_int);
    }
    
    BnkfPutM("HXDsclFitsRead:IROW", sizeof(int), &com.irow);
    com.irow ++;
    
    BnkfPutM("HXD:ALL:UPDATE", sizeof(int), &update);
    
    *status = ANL_OK;
}

void
HXDsclFitsRead_endrun( int *status ) {
  *status = ANL_OK;
}

void
HXDsclFitsRead_exit(int *status){
    int istat = 0;
    int hdutype;
    char taskname[FITS_KEY_MAX_LENGTH];
    char taskver[FITS_KEY_MAX_LENGTH];
    char credits[FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES];
    char *buf;

    fits_movabs_hdu(com.fp, HXD_SCL_FITS_PRIMARY_HDU, &hdutype, &istat);
    if( istat ){
	fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
	*status = ANL_QUIT;
	return;
    }

    hxdFitsHeader_writeCredits(com.fp, anl_task_name(), 
			       anl_task_version(), anl_task_credits(), 
			       HXD_FITS_HEADER_UTIL_CREDIT_DUPLICATE, &istat);
    if ( istat ) {
      fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (%d)\n",
	      pname, istat);
      *status = ANL_NG;
      return;
    }

    if (hxdFitsHeader_writeParamer_scl( com.fp, HXD_SCL_FITS_PRIMARY_HDU) 
        != HXD_FITS_HEADER_UTIL_OK){
      fprintf(stderr, "%s: error in writing parameters\n", pname);
      return;
    }
    
    fits_movabs_hdu(com.fp, HXD_SCL_FITS_EVENT_EXTENTION, &hdutype,	&istat);
    if( istat ){
	fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
	*status = ANL_QUIT;
	return;
    }
    /*
    sprintf( taskname, "%s", 
	     strndup(anl_task_name()   , FITS_KEY_MAX_LENGTH));
    sprintf( taskver, "%s",  
	     strndup(anl_task_version(), FITS_KEY_MAX_LENGTH));
    sprintf( credits, "%s",  
	     strndup(anl_task_credits(),
		     FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES));
    */
    buf = (char *)strdup(anl_task_name());
    if(strlen(buf) > FITS_KEY_MAX_LENGTH) buf[FITS_KEY_MAX_LENGTH] = '\0';
    sprintf( taskname, "%s", buf);
    
    buf = (char *)strdup(anl_task_version());
    if(strlen(buf) > FITS_KEY_MAX_LENGTH) buf[FITS_KEY_MAX_LENGTH] = '\0';
    sprintf( taskver, "%s", buf);
    
    buf = (char *)strdup(anl_task_credits());
    if(strlen(buf) > FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES) 
      buf[FITS_KEY_MAX_LENGTH] = '\0';
    sprintf( credits, "%s", buf);

    hxdFitsHeader_writeCredits(com.fp, taskname, taskver, credits,
			       HXD_FITS_HEADER_UTIL_CREDIT_DUPLICATE, &istat);
    if ( istat ) {
      fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (%d)\n",
	      pname, istat);
      *status = ANL_NG;
      return;
    }
    
    if (hxdFitsHeader_writeParamer_scl( com.fp, HXD_SCL_FITS_EVENT_EXTENTION) 
        != HXD_FITS_HEADER_UTIL_OK){
      fprintf(stderr, "%s: error in writing parameters\n", pname);
      return;
    }
    
   if(closeFITS()){
    *status = ANL_QUIT;
    return;
  }
}
/*************************** EOF ******************************************/
