/*
   HXDeventFitsRead
   
   v0.0.1 test version for ftools
          created by M.Sugiho
   v0.0.5 for HXDeventFitsWrite v0.5.8
   v0.0.6 BnkDef "HXD:WEL:***:old"
          BnkDef "HXDeventFitsRead:***:COLNUM"
   v0.0.7 WPU_CLOCK_RATE 3bit -> 1byte
          hbook test
          READONLY support
   v0.0.8 support gti 
   v0.1.0 BnkDef "HXD:ALL:RPT"
          BnkDef "HXDeventFitsRead:FILE_NAME"
	  & debug
   v0.1.1 BnkDef "HXDeventFitsRead:TIME"
   v0.1.2 EVTIME -> EV_TIME
   v0.1.3 delete "HXD:ALL:RPT" "HXD:ALL:EVENTTYPE"
   v0.1.4 for HXDeventFitsWrite v0.6.5
   v0.1.5 TIME GRADE PI_PMT PI_PIN to com
   v0.1.6 for HXDeventFitsWrite v0.6.6
          iomode support
   v0.1.7 nfits -> 1
   v0.1.8 delete HXD:ALL:TIME , support CREATE
   v0.1.9 use function hxdeventFitsUtil
          add BnkDef( "HXDeventFitsRead:NROW", sizeof(long) );
   v0.2.0 colnum => time_colnum, grade_colnum, pi_fast_colnum pi_slow_colnum
                    pi_pin0_colnum pi_pin1_colnum pi_pin2_colnum pi_pin3_colnum

   v0.2.1 GTI => TIME or AETIME
          delete COLNUM         corresponding to HXD2ndeventFitsWrire 0.1.2
	  
   v0.2.2 LNGTH_OK => LENGTH_CHK corresponding to HXD2ndeventFitsWrire 0.1.3
   v0.2.3 hxd_eventFits -> hxdeventFits : hxdeventFitsUtil v0.0.4
   v0.2.4 for hxdeventFitsUtil v0.0.5
   v0.2.5 use hxdgtiFitsUtil
   v0.2.6 debug!! in CREATE mode
   v0.2.7 *.par change : hxd_evebt_fits_gti_time -> hxd_event_fits_gti_time
          delete Evs
   v0.2.8 hxdeventFitsToBnkUtil v0.0.1
   v0.2.9 chksum
   v0.3.0 1999-10-31 change include for ftool-release   by Y.Terada
   v0.3.1 1999-12-24 shorten param-name                 by Y.Terada
                     add new_fp for CREATE_MODE.
		     add PI_PIN CHANGE
   v0.3.2 2003-05-03 for HXD-II (use PWH)               by Y.Terada
   v0.4.0 2003-07-23 for HEADAS, by Y Terada, H Takahashi, M Suzuki
   v0.4.1 2004-03-11 write Credits  by Y.Terada
   v0.4.2 2004-03-12 change pfile (yes_no --> Boolian logic) by Y.Terada
   v0.4.3 2005-01-18 Put EventFits structure 
                             by S.Hirakuri, T.Kisisita, I.taka, Y.Terada
   v0.5.0 2005-05-04 change AETIME to S_TIME by Y.Terada
   v0.5.1 2005-10-11 debug                              by T.Kitaguchi
   v0.5.2 2005-10-22 read T_START etc..                 by Y.Terada
   v0.5.3 2005-11-04 Put PIL value into BNK             by Y.Terada
   ---> v1.0.0
   v2.0.0 2006-09-10 Version 2.0 format                 by Y.Terada
   v2.0.2 2007-05-07 debug                              by Y.Terada
   v2.0.3 2007-06-22 irow data size int -> long         by T.Kitaguchi
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
/*#include "pfile.h"*/

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"

#include "hxdeventFitsUtil.h"
#include "hxdeventFitsToBnkUtil.h"
#include "hxdgtiFitsUtil.h"
#include "hxdFitsHeaderUtil.h"

#define FILENAME_MAX_LENGTH PIL_LINESIZE

#define DEBUG 0
 
enum{
  HXD_READONLY,
  HXD_OVERWRITE,
  HXD_CREATE
};

char HXDeventFitsRead_version[] = "version 2.0.2";

static char pname[] = "HXDeventFitsRead";

static struct {
  char filename[FILENAME_MAX_LENGTH];
  char newfilename[FILENAME_MAX_LENGTH];
  fitsfile *fp;
  fitsfile *new_fp;
  long irow;
  int iomode;
  int gtimode;
  char time[32];
  int time_change;
  int grade_change;
  int pi_pmt_change;
  int pi_pin_change;
  int use_pwh;
} com;

static HXD_GTI gti;

static struct{
  int colnum[HXD_EVENT_FITS_KEY_NUM];
  int gti_time_colnum;
  long nrow;
  long version;
} fits;

static int gsooldpi = 0;

void
HXDeventFitsRead_startup(int *status)
{
  int ftools_no = 0;
  BnkDef( "HXD:PIL:read_iomode",   
	  sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:time_change",   sizeof(int));
  BnkDef( "HXD:PIL:grade_change",  sizeof(int));
  BnkDef( "HXD:PIL:pi_pmt_change", sizeof(int));
  BnkDef( "HXD:PIL:pi_pin_change", sizeof(int));
  BnkDef( "HXD:PIL:gtimode",       sizeof(int));
  BnkDef( "HXD:PIL:gti_time",      sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:input_name",    sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:create_name",   sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:use_pwh_mode",  sizeof(char)*HXDFITSHEADER_LINESIZE);

  BnkDef( "HXD:ftools:hxdtime_yn",       sizeof(int));
  BnkDef( "HXD:ftools:hxdmkgainhist_yn", sizeof(int));
  BnkDef( "HXD:ftools:hxdpi_yn",         sizeof(int));
  BnkDef( "HXD:ftools:hxdgrade_yn",      sizeof(int));

  BnkPut( "HXD:ftools:hxdtime_yn",       sizeof(int), &ftools_no);
  BnkPut( "HXD:ftools:hxdmkgainhist_yn", sizeof(int), &ftools_no);
  BnkPut( "HXD:ftools:hxdpi_yn",         sizeof(int), &ftools_no);
  BnkPut( "HXD:ftools:hxdgrade_yn",      sizeof(int), &ftools_no);
  
  *status = ANL_OK;
  return; /** need return **/
}

void
HXDeventFitsRead_init(int *status)
{
  int i;
  
  BnkDef( "HXDeventFitsRead:IROW", sizeof(long) );
  hxdeventFitsToBnk_init();
  BnkDef( "HXDeventFitsRead:FILE_P", sizeof(fitsfile *) );
  BnkDef( "HXDeventFitsRead:FILE_NAME", sizeof(com.filename) -1);
  BnkDef( "HXDeventFitsRead:NEW_FILE_NAME", sizeof(com.newfilename) -1);
  BnkDef( "HXDeventFitsRead:NEW_FILE_P", sizeof(fitsfile *));
  BnkDef( "HXDeventFitsRead:IOMODE", sizeof(int) );
  BnkDef( "HXDeventFitsRead:GTIMODE", sizeof(int) );
  BnkDef( "HXDeventFitsRead:NROW", sizeof(long) );
  BnkDef( "HXDeventFitsRead:GSOOLDPI", sizeof(int) );
  
  BnkDef( "HXDeventFitsRead:EV_TIME:CHANGE", sizeof(int) );
  BnkDef( "HXDeventFitsRead:GRADE:CHANGE", sizeof(int) );
  BnkDef( "HXDeventFitsRead:PI_PMT:CHANGE", sizeof(int) );
  BnkDef( "HXDeventFitsRead:PI_PIN:CHANGE", sizeof(int) );
  
  BnkDef( "HXDeventFitsRead:USE_PWH", sizeof(int) );

  BnkDef( "HXDeventFitsRead:TSTART",  sizeof(double) );
  BnkDef( "HXDeventFitsRead:TSTOP",   sizeof(double) );
  BnkDef( "HXDeventFitsRead:TELAPSE", sizeof(double) );
  BnkDef( "HXDeventFitsRead:ONTIME",  sizeof(double) );

  BnkDef( "HXD:ALL:EVENTTYPE", sizeof(int) );
  
  BnkDef( "HXD:ALL:UPDATE" ,   sizeof(int) );

  BnkDef( "HXD:WEL:EVENT" ,   sizeof(HxdEventFits02) );
  
  *status = ANL_OK;
  return; /** need return **/
}

void
HXDeventFitsRead_com(int *status)
{

  char iomode_strings[PIL_LINESIZE];
  char gtimode_strings[PIL_LINESIZE];
  
  char time_change_strings[PIL_LINESIZE];
  char grade_change_strings[PIL_LINESIZE];
  char pi_pmt_change_strings[PIL_LINESIZE];
  char pi_pin_change_strings[PIL_LINESIZE];

  int gtimode, pwhmode;
  int time_change, grade_change, pi_pmt_change, pi_pin_change;

  char pwhmode_strings[PIL_LINESIZE];

  int i;
  
  if ( *status ) { /* ftools */
    
    *status = 0;
    
/*  UCLGST("read_iomode", iomode_strings, *status); */
    *status = PILGetString("read_iomode", iomode_strings);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetString read_iomode error (%d)\n", 
	      pname, *status);
      *status = ANL_QUIT;
      exit(-1);
    } else {
      int string_size = strlen(iomode_strings);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
	fprintf(stderr, "%s: warning iomode_string is long\n", pname);
	string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:read_iomode", string_size,
	     iomode_strings);
    }
    
    if(!strcmp("readonly",iomode_strings)){
      com.iomode = HXD_READONLY;
    } else if(!strcmp("overwrite",iomode_strings)){
      com.iomode = HXD_OVERWRITE;
    } else if(!strcmp("create",iomode_strings)){
      com.iomode = HXD_CREATE;	    
    } else {
      fprintf(stderr,"%s : no such I/O mode\n",iomode_strings);
      *status = ANL_QUIT;
      exit(-1);
    }


    
    if ( com.iomode == HXD_OVERWRITE ){
       *status = PILGetBool("time_change", &time_change);
       if ( *status ) {
	 fprintf(stderr, "%s: PILGetBool time_change error (%d)\n", 
		 pname, *status);
	 *status = ANL_QUIT;
	 exit(-1);
       } else {
	 BnkPut("HXD:PIL:time_change", sizeof(int),
		&time_change);
       }
       if( time_change == 1){
	 com.time_change = TRUE;
       } else if(time_change == 0){
	 com.time_change = FALSE;
       } else {
	 fprintf(stderr, "%s : no such answer (%d, yes=1/no=0)\n",
		 pname, time_change);
	 *status = ANL_QUIT;
	 exit(-1);
       }

      *status = PILGetBool("grade_change", &grade_change);
       if ( *status ) {
	 fprintf(stderr, "%s: PILGetBool grade_change error (%d)\n", 
		 pname, *status);
	 *status = ANL_QUIT;
	 exit(-1);
       } else {
	 BnkPut("HXD:PIL:grade_change", sizeof(int),
		&grade_change);
       }
       if( grade_change == 1 ){
	 com.grade_change = TRUE;
       } else if( grade_change == 0 ){
	 com.grade_change = FALSE;
       } else {
	 fprintf(stderr, "%s : no such answer (%d, yes=1/no=0)\n",
		 pname, grade_change);
	 *status = ANL_QUIT;
	 exit(-1);
       }

       *status = PILGetBool("pi_pmt_change", &pi_pmt_change);
       if ( *status ) {
	 fprintf(stderr, "%s: PILGetBool pi_pmt_change error (%d)\n", 
		 pname, *status);
	 *status = ANL_QUIT;
	 exit(-1);
       } else {
	 BnkPut("HXD:PIL:pi_pmt_change", sizeof(int),
		&pi_pmt_change);
       }
       if( pi_pmt_change == 1 ){
	 com.pi_pmt_change = TRUE;
       } else if( pi_pmt_change == 0){
	 com.pi_pmt_change = FALSE;
       } else {
	 fprintf(stderr, "%s : no such answer (%d, yes=1/no=0)\n",
		 pname, pi_pmt_change);
	 *status = ANL_QUIT;
	 exit(-1);
       }


      *status = PILGetBool("pi_pin_change", &pi_pin_change);
      if ( *status ) {
	fprintf(stderr, "%s: PILGetBool pi_pin_change error (%d)\n", 
		pname, *status);
	*status = ANL_QUIT;
	exit(-1);
      } else {
	 BnkPut("HXD:PIL:pi_pin_change", sizeof(int),
		&pi_pin_change);
      }

      if( pi_pin_change == 1){
	com.pi_pin_change = TRUE;
      } else if( pi_pin_change == 0){
	com.pi_pin_change = FALSE;
      } else {
	fprintf(stderr, "%s : no such answer (%d, yes=1/no=0)\n",
		pname, pi_pin_change);
	*status = ANL_QUIT;
	exit(-1);
      }
      
    }

    *status = PILGetBool("gtimode", &gtimode);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetBool gtimode error (%d)\n", 
	      pname, *status);
      *status = ANL_QUIT;
      exit(-1);
    } else {
      BnkPut("HXD:PIL:gtimode", sizeof(int), &gtimode);
    }
    if( gtimode == 1){
      com.gtimode = TRUE;
    } else if( gtimode == 0){
      com.gtimode = FALSE;
    } else {
      fprintf(stderr,"%s : no such answer (%d, yes=1/no=0)\n" , pname, 
		    gtimode);
      *status = ANL_QUIT;
      exit(-1);
    }
    
    if( com.gtimode == TRUE ){
      /*      
      UCLGST("gti_time", com.time , *status);
      */
      *status = PILGetString("gti_time", com.time );
      if ( *status ) {
	fprintf(stderr, "%s: PILGetString gti_time error (%d)\n", 
		pname, *status);
	*status = ANL_QUIT;
	exit(-1);
      } else {
	int string_size = strlen(com.time);
	if  (string_size>HXDFITSHEADER_LINESIZE) {
	  fprintf(stderr, "%s: warning gti_time is long\n", pname);
	  string_size = HXDFITSHEADER_LINESIZE;
	}
	BnkPut("HXD:PIL:gti_time", string_size, com.time);
      }
      if( strcmp("TIME",com.time) && strcmp("S_TIME",com.time)){
	fprintf(stderr,"%s : no such time\n",com.time);
	*status = ANL_QUIT;
		exit(-1);
      }
    }
    /*    
    UCLGST("input_name", com.filename, *status);	
    */
    *status = PILGetFname("input_name", com.filename);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetFname INPUT error (%d)\n", pname, *status);
      *status = ANL_QUIT;
      exit(-1);
    } else {
      int string_size = strlen(com.filename);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
	fprintf(stderr, "%s: warning input_name is long\n", pname);
	string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:input_name", string_size, 
	     com.filename);
    }
    
    if ( com.iomode == HXD_CREATE ){
      *status = PILGetFname("create_name", com.newfilename);
      
      if ( *status ) { 
	  *status = ANL_QUIT;
	  exit(-1);
      } else {
	int string_size = strlen(com.newfilename);
	if  (string_size>HXDFITSHEADER_LINESIZE) {
	  fprintf(stderr, "%s: warning create_name is long\n", pname);
	  string_size = HXDFITSHEADER_LINESIZE;
	}
	BnkPut("HXD:PIL:create_name", string_size,
	       com.newfilename);
      }
    } else {
	com.newfilename[0] = '\0';
    }
    *status = PILGetBool("use_pwh_mode", &pwhmode);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetString use_pwh_mode error (%d)\n", 
	      pname, *status);
      *status = ANL_QUIT;
      exit(-1);
    } else {
      BnkPut("HXD:PIL:use_pwh_mode", sizeof(int), &pwhmode);
    }
    if( pwhmode == 1){
      com.use_pwh = TRUE;
    } else if( pwhmode == 0){
      com.use_pwh = FALSE;
    } else {
      fprintf(stderr,"%s : no such answer (%d, yes=1/no=0)\n" ,
		    pname, pwhmode);
      *status = ANL_QUIT;
      exit(-1);
    }
    
    *status = ANL_OK;
    if (DEBUG)  fprintf(stdout, "%s_com: ANL OK\n", pname);
    return;
  } /*** end of ftools ***/
  
  iomode_strings[0]='\0';
  gtimode_strings[0]='\0';    
  time_change_strings[0]='\0';
  grade_change_strings[0]='\0';
  pi_pmt_change_strings[0]='\0';
  pi_pin_change_strings[0]='\0';
  pwhmode_strings[0]='\0';
  
  CLtxtrd("HXD event fits input I/O mode : readonly or overwrite or create",
	  iomode_strings, sizeof(iomode_strings) );
  
  if(!strcmp("readonly",iomode_strings)){
    com.iomode = HXD_READONLY;
  } else if(!strcmp("overwrite",iomode_strings)){
    com.iomode = HXD_OVERWRITE;
  } else if(!strcmp("create",iomode_strings)){
    com.iomode = HXD_CREATE;
  } else {
    fprintf(stderr,"%s : no such I/O mode",iomode_strings);
    *status = ANL_QUIT;
    return;
  }
  
  if ( com.iomode == HXD_OVERWRITE ){
    
    CLtxtrd("HXD event fits update TIME : yes or no",
	    time_change_strings, sizeof(time_change_strings) );
    
    if(!strcmp("yes", time_change_strings)){
      com.time_change = TRUE;
    } else if(!strcmp("no", time_change_strings)){
      com.time_change = FALSE;
    } else {
      fprintf(stderr, "%s : no such answer : yes or no",
	      time_change_strings);
      *status = ANL_QUIT;	
    }
    
    CLtxtrd("HXD event fits update GRADE : yes or no",
	    grade_change_strings, sizeof(grade_change_strings) );
    
    if(!strcmp("yes", grade_change_strings)){
      com.grade_change = TRUE;
    } else if(!strcmp("no", grade_change_strings)){
      com.grade_change = FALSE;
    } else {
      fprintf(stderr, "%s : no such answer : yes or no",
	      grade_change_strings);
      *status = ANL_QUIT;
    }
    
    CLtxtrd("HXD event fits update PI_FAST PI_SLOW : yes or no",
	    pi_pmt_change_strings, sizeof(pi_pmt_change_strings) );
    
    if(!strcmp("yes", pi_pmt_change_strings)){
      com.pi_pmt_change = TRUE;
    } else if(!strcmp("no", pi_pmt_change_strings)){
      com.pi_pmt_change = FALSE;
    } else {
      fprintf(stderr, "%s : no such answer : yes or no",
	      pi_pmt_change_strings);
      *status = ANL_QUIT;
    }
    
    CLtxtrd("\
HXD event fits update PI_PIN0 PI_PIN1 PI_PIN2 PI_PIN3: yes or no",
	    pi_pin_change_strings, sizeof(pi_pin_change_strings) );
    
    if(!strcmp("yes", pi_pin_change_strings)){
      com.pi_pin_change = TRUE;
    } else if(!strcmp("no", pi_pin_change_strings)){
      com.pi_pin_change = FALSE;
    } else {
      fprintf(stderr, "%s : no such answer : yes or no",
	      pi_pin_change_strings);
      *status = ANL_QUIT;
    }
    
  }
  
  CLtxtrd("HXD event fits using GTI : yes or no", gtimode_strings,
	  sizeof(gtimode_strings) );
    
  if(!strcmp("yes",gtimode_strings)){
    com.gtimode = TRUE;
  } else if(!strcmp("no",gtimode_strings)){
    com.gtimode = FALSE;
  } else {
    fprintf(stderr,"%s : no such answer : yes or no",gtimode_strings);
    *status = ANL_QUIT;
	return;
  }

  com.time[0] = '\0';
  
  if( com.gtimode == TRUE ){
    
    CLtxtrd("HXD event fits time using TIME or S_TIME", com.time,
	    sizeof(com.time) );
    
    if( strcmp("TIME",com.time) && strcmp("S_TIME",com.time)){
      fprintf(stderr,"%s : no such time",com.time);
      *status = ANL_QUIT;
      return;
    }
  }
  
  com.filename[0] = '\0';
  
  CLtxtrd("HXD event fits file name",
	  com.filename, sizeof(com.filename));
  
  if ( com.iomode == HXD_CREATE ){
    
    com.newfilename[0] = '\0';
    
    CLtxtrd("HXD event fits created file name",
	    com.newfilename, sizeof(com.newfilename));
    
  }
  
  CLtxtrd("HXD event fits using PWH in time assignment: yes or no", 
	  pwhmode_strings, sizeof(pwhmode_strings) );
    
  if(!strcmp("yes",pwhmode_strings)){
    com.use_pwh = TRUE;
  } else if(!strcmp("no",pwhmode_strings)){
    com.use_pwh = FALSE;
  } else {
    fprintf(stderr,"%s : no such answer : yes or no",pwhmode_strings);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
  return; /** need return **/
}

void
HXDeventFitsRead_his(int *status)
{
  *status = ANL_OK;
  return; /** need return **/
}

static int
openFITS(){
  
  int istat = 0;
  
  int casesen = TRUE;
  int hdutype;
  char comment[80];
  
  if( com.iomode==HXD_OVERWRITE ){
    fits_open_file(&com.fp, com.filename, READWRITE, &istat);
    com.new_fp = NULL;
  } else if( com.iomode==HXD_READONLY ) {
    fits_open_file(&com.fp, com.filename, READONLY, &istat);
    com.new_fp = NULL;
  } else if( com.iomode==HXD_CREATE ) {
    fits_open_file(&com.fp, com.filename, READONLY, &istat);
    
/*  fits_create_file(&com.new_fp, com.newfilename, &istat);*/
    /*
     * (!!) create file is in HXD2ndeventFitsWrite.c
     *      So new_fp set to NULL, and BnkPut NULL_POINTER
     */
    com.new_fp = NULL; 
  } 
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_open_file('%s') failed (%d)\n",
	    pname, com.filename, istat);
    /*return ANL_NG;*/
    exit(-1);
  } else {
    fits_movabs_hdu(com.fp, HXD_EVENT_FITS_EVENT_EXTENTION , &hdutype,
		    &istat);
  }    
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_read_key_lng(com.fp, "HXD_FVER", &fits.version, comment,&istat);
  } 
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_key('HXD_FVER') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    if (fits.version != HXD_EVENT_FITS_FORMAT_VERSION){
      fprintf(stderr, "%s: HXD Fits Format Version (%d) is inconsistent.",
	      pname, fits.version);
      fprintf(stderr, "%s: Version %d is required. Stop the process.",
	      pname, HXD_EVENT_FITS_FORMAT_VERSION);
      return ANL_NG;
    }

    fits_read_key_lng(com.fp, "NAXIS2", &fits.nrow, comment,
		      &istat);
  }    
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_key('NAXIS2') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    hxdeventFits_col_num(com.fp , fits.colnum , &istat );
  } if( istat ){
    return ANL_NG;
  }

  /** read GSOOLDPI keyword **/
  fits_read_key_log(com.fp, "GSOOLDPI", &gsooldpi, comment, &istat);
  if ( istat == KEY_NO_EXIST ) {

      /** if key not extant, assume hxdpi_old was used **/
      gsooldpi = 1;
      istat = 0;
  }

  if( com.gtimode == TRUE ){
    fits_get_colnum( com.fp, casesen, com.time, &fits.gti_time_colnum, &istat);
    
    if ( istat ) {
      fprintf(stderr, "%s: fits_get_colnum('%s') failed (%d)\n",
	      pname, com.time, istat);
      return ANL_NG;    
    }
    
    hxdgtiFits_readGTI ( com.fp, HXD_EVENT_FITS_GTI_EXTENTION, &gti, &istat );
    
    if( istat ){
      return ANL_NG;
    }
    
  }
  
  return ANL_OK;
    
}

static int
reopenFITS(){
  
  int istat = 0;
  int hdutype;
  
  if( com.iomode==HXD_OVERWRITE ){
    fits_open_file(&com.fp, com.filename, READWRITE, &istat);
  } else {
    fits_open_file(&com.fp, com.filename, READONLY, &istat);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_open_file('%s') failed (%d)\n",
		pname, com.filename, istat);
    return ANL_NG;
  } else {
    fits_movabs_hdu(com.fp, HXD_EVENT_FITS_EVENT_EXTENTION , &hdutype,
		    &istat);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  }
  
  BnkfPutM( "HXDeventFitsRead:NROW", sizeof(long), &fits.nrow );
  
  BnkfPutM("HXDeventFitsRead:FILE_NAME", strlen(com.filename)+1,
	   com.filename);
  
  BnkfPutM("HXDeventFitsRead:FILE_P", sizeof(com.fp), &com.fp);
  BnkfPutM("HXDeventFitsRead:GSOOLDPI", sizeof(int), &gsooldpi);
  
  return ANL_OK;
}

static int
closeFITS(){
  
  int istat = 0;
  
  fits_close_file(com.fp, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_close_file failed (%d)\n", pname, istat);
    return ANL_NG;
  }
  
  return ANL_OK;
  
}

void
HXDeventFitsRead_bgnrun(int *status)
{
  
  int istat = 0;
  double tstart, tstop, telapse, ontime;
  int stat;
  
  int evtyp = HXD_EVTYP_WEL;
  
  if( openFITS() ){
    *status = ANL_QUIT;
    return;
  }

  stat = hxdFitsHeader_readTIME(com.fp, &tstart, &tstop, &telapse, &ontime);
  if (stat != HXD_FITS_HEADER_UTIL_OK){
    fprintf(stderr, "%s: error in reading HEADER\n", pname);
    *status = ANL_NG;
    return;
  }

  closeFITS();
  
  com.irow = 1;
  
  BnkfPutM("HXDeventFitsRead:IOMODE", sizeof(int), &com.iomode );
  
  BnkfPutM("HXDeventFitsRead:GTIMODE", sizeof(int), &com.gtimode );

  BnkfPutM("HXDeventFitsRead:USE_PWH", sizeof(int), &com.use_pwh );
  
  BnkfPutM("HXDeventFitsRead:EV_TIME:CHANGE", sizeof(int),
	   &com.time_change );
  BnkfPutM("HXDeventFitsRead:GRADE:CHANGE", sizeof(int),
	   &com.grade_change );
  BnkfPutM("HXDeventFitsRead:PI_PMT:CHANGE", sizeof(int),
	   &com.pi_pmt_change );
  BnkfPutM("HXDeventFitsRead:PI_PIN:CHANGE", sizeof(int),
	   &com.pi_pin_change );

  BnkfPutM("HXDeventFitsRead:TSTART", sizeof(double), &tstart);
  BnkfPutM("HXDeventFitsRead:TSTOP",  sizeof(double), &tstop);
  BnkfPutM("HXDeventFitsRead:TELAPSE",sizeof(double), &telapse);
  BnkfPutM("HXDeventFitsRead:ONTIME", sizeof(double), &ontime);

  
  BnkfPutM("HXDeventFitsRead:NEW_FILE_NAME", strlen(com.newfilename)+1,
	   com.newfilename);
  BnkfPutM("HXDeventFitsRead:NEW_FILE_P", sizeof(com.new_fp), &com.new_fp);
  
  BnkfPutM("HXD:ALL:EVENTTYPE", sizeof(int), &evtyp );
  
  if( reopenFITS() ){
    *status = ANL_QUIT;
    return;
  }
  
  *status = ANL_OK;
  return; /** need return **/
}

static int
judgeGTI(double time){
  
  int i;
  
  for(i=0;i<gti.row;i++){
    if( gti.start[i] <= time && time <= gti.stop[i]){
      return ANL_TRUE;
    }
  }
  
  return ANL_FALSE;
  
}

void
HXDeventFitsRead_ana(int nevent, int eventid, int *status)
{
    
  int istat = 0;
  
  HxdEventFits02 data;
  
  int update = HXD_UPDATE_WEL;
  
  if ( fits.nrow < com.irow ) {

    *status = ANL_QUIT;
    return;
    
  }
    
  if ( com.gtimode == TRUE ) {
    
    long firstelem = 1;
    long nelements = 1;
    
    double nulval = 0.0;
    double  time ;
    
    int anynul;
    
    fits_read_col_dbl(com.fp, fits.gti_time_colnum, com.irow,
		      firstelem,nelements, nulval, &time, &anynul, &istat);
    
    if ( istat ) {
      fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	      pname, istat);
      *status = ANL_QUIT;
      return;
    }
    
    if( judgeGTI( time ) == ANL_FALSE  ){
      update = 0;
    }
    
  }
  
  BnkfPutM("HXD:ALL:UPDATE", sizeof(int), &update);
  
  hxdeventFits_col_read( com.fp, com.irow, fits.colnum, &data, &istat );
  
  if( istat ){
    *status = ANL_QUIT;
    return;	
  }

  hxdeventFitsToBnk_put( &data );
  BnkfPutM("HXD:WEL:EVENT", sizeof(HxdEventFits02), &data);
  
  BnkfPutM("HXDeventFitsRead:IROW", sizeof(long), &com.irow);
  
  com.irow ++;
  
  *status = ANL_OK;
  return; /** need return **/
}

void
HXDeventFitsRead_endrun(int *status)
{

  int istat = 0;

  int hdutype;

  /** change Header in HXD2ndeventFitsWrite for CREATE Mode **/
  if (com.iomode == HXD_OVERWRITE) {

    fits_movabs_hdu(com.fp, HXD_EVENT_FITS_PRIMARY_HDU, &hdutype, &istat);
    if( istat ){
      fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
      *status = ANL_QUIT;
      return;
      
    } else {
      hxdFitsHeader_writeCredits(com.fp, anl_task_name(), 
				 anl_task_version(), anl_task_credits(), 
				 HXD_FITS_HEADER_UTIL_CREDIT_OVERWRITE,
				 &istat);
    }
    if ( istat ) {
      fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (primary: %d)\n",
	      pname, istat);
      return;
    } 
    if (hxdFitsHeader_writeParamer_wel( com.fp, 
					HXD_EVENT_FITS_PRIMARY_HDU) 
	!= HXD_FITS_HEADER_UTIL_OK){
      fprintf(stderr, "%s: error in writing parameters\n", pname);
      return;
    } else {
      fits_write_chksum(com.fp, &istat);
    }
    if ( istat ) {
      fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
      *status = ANL_QUIT;
      return;
    } 
    
    fits_movabs_hdu(com.fp, HXD_EVENT_FITS_EVENT_EXTENTION, &hdutype, &istat);
    if( istat ){
      fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
      *status = ANL_QUIT;
      return;
    } else {
      hxdFitsHeader_writeCredits(com.fp, anl_task_name(), 
				 anl_task_version(), anl_task_credits(), 
				 HXD_FITS_HEADER_UTIL_CREDIT_OVERWRITE,
				 &istat);
    } if ( istat ) {
      fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (primary: %d)\n",
	      pname, istat);
      return;
    }
    if (hxdFitsHeader_writeParamer_wel( com.fp, 
					HXD_EVENT_FITS_EVENT_EXTENTION) 
	!= HXD_FITS_HEADER_UTIL_OK){
      fprintf(stderr, "%s: error in writing parameters\n", pname);
      return;
    } else {
      fits_write_chksum(com.fp, &istat);
    }
    if ( istat ) {
      fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
      *status = ANL_QUIT;
      return;
    } 
    
    fits_movabs_hdu(com.fp, HXD_EVENT_FITS_GTI_EXTENTION, &hdutype, &istat);
    if( istat ){
      fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
      *status = ANL_QUIT;
      return;
    } else {
      hxdFitsHeader_writeCredits(com.fp, anl_task_name(), 
				 anl_task_version(), anl_task_credits(), 
				 HXD_FITS_HEADER_UTIL_CREDIT_OVERWRITE,
				 &istat);
    }
    if ( istat ) {
      fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (primary: %d)\n",
	      pname, istat);
      return;
    } 
    if (hxdFitsHeader_writeParamer_wel( com.fp, 
					HXD_EVENT_FITS_GTI_EXTENTION) 
	!= HXD_FITS_HEADER_UTIL_OK){
      fprintf(stderr, "%s: error in writing parameters\n", pname);
      return;
    } else {
      fits_write_chksum(com.fp, &istat);
    }
    if ( istat ) {
      fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
      *status = ANL_QUIT;
      return;
    } 
  } 
  
  *status = ANL_OK;
  return; /** need return **/
}

void
HXDeventFitsRead_exit(int *status)
{

  /* if ((com.iomode == HXD_READONLY) || (com.iomode == HXD_OVERWRITE)) { */
    /** close new_fp in HXD2ndeventFitsWrite for CREATE MODE **/
    if(closeFITS()){
      *status = ANL_QUIT;
      return;
    }
  /*}*/  
  *status = ANL_OK;
  return; /** need return **/

}
