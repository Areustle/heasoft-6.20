/*
   HXDtrnFitsRead
     v0.0.1 created Y.Terada
     v0.1.0 support QUALITY Change   2003-05-03 Y.Terada
     v0.2.0 for HEADAS,              2003-07-24, Y Terada
     v0.3.0 for new format (AETIME), 2004-03-11, Y Terada
     v0.3.1 change closeFITS,        2004-03-12, Y Terada
     v0.3.2 change pfiles, Boolian   2004-03-13, Y Terada
     v0.4.0 change AETIME to S_TIME  2005-05-04, Y Terada
     v0.4.1 put PIL values into Bnk  2005-11-04, Y Terada
     v0.4.2 debug                    2005-11-08, Y Terada
     v0.4.3 save GTI in Bnk          2011-03-24, C Padgett
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

#include "hxdtrnFitsUtil.h"
#include "hxdtrnFitsToBnkUtil.h"
#include "hxdgtiFitsUtil.h"

#define FILENAME_MAX_LENGTH PIL_LINESIZE

enum{
  HXD_READONLY,
  HXD_OVERWRITE,
  HXD_CREATE
};

char HXDtrnFitsRead_version[] = "version 0.4.3";

static char pname[] = "HXDtrnFitsRead";

static struct {
  char filename[FILENAME_MAX_LENGTH];
  char newfilename[FILENAME_MAX_LENGTH];
  fitsfile *fp;
  int irow;
  int iomode;
  int gtimode;
  char time[PIL_LINESIZE];
  int time_change;
  int pi_change;
  int quality_change;
} com;

static HXD_GTI gti;

static struct{
  int colnum[HXD_TRN_FITS_KEY_NUM];
  int gti_time_colnum;
  long nrow;
} fits;

void
HXDtrnFitsRead_startup(int *status)
{
  int ftools_no = 0;

  BnkDef( "HXD:ftools:hxdwamtime_yn",       sizeof(int));
  BnkDef( "HXD:ftools:hxdmkwamgainhist_yn", sizeof(int));
  BnkDef( "HXD:ftools:hxdwampi_yn",         sizeof(int));
  BnkDef( "HXD:ftools:hxdwamgrade_yn",      sizeof(int));

  BnkPut( "HXD:ftools:hxdwamtime_yn",       sizeof(int), &ftools_no);
  BnkPut( "HXD:ftools:hxdmkwamgainhist_yn", sizeof(int), &ftools_no);
  BnkPut( "HXD:ftools:hxdwampi_yn",         sizeof(int), &ftools_no);
  BnkPut( "HXD:ftools:hxdwamgrade_yn",      sizeof(int), &ftools_no);

  BnkDef( "HXD:PIL:read_iomode",   sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:time_change",   sizeof(int));
  BnkDef( "HXD:PIL:pi_change",     sizeof(int));
  BnkDef( "HXD:PIL:quality_change",sizeof(int));
  BnkDef( "HXD:PIL:gtimode",       sizeof(int));
  BnkDef( "HXD:PIL:gti_time",      sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:input_name",    sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:create_name",   sizeof(char)*HXDFITSHEADER_LINESIZE);

  *status = ANL_OK;
}

void
HXDtrnFitsRead_init(int *status)
{
  hxdtrnFitsToBnk_init();
  
  BnkDef( "HXDtrnFitsRead:IROW", sizeof(long) );
  
  BnkDef( "HXDtrnFitsRead:FILE_P", sizeof(fitsfile) );
  BnkDef( "HXDtrnFitsRead:FILE_NAME", sizeof(com.filename) -1);
  BnkDef( "HXDtrnFitsRead:NEW_FILE_NAME", sizeof(com.newfilename) -1);
  BnkDef( "HXDtrnFitsRead:IOMODE", sizeof(int) );
  BnkDef( "HXDtrnFitsRead:GTIMODE", sizeof(int) );
  BnkDef( "HXDtrnFitsRead:GTI", sizeof(HXD_GTI) );
  BnkDef( "HXDtrnFitsRead:NROW", sizeof(long) );
  
  BnkDef( "HXDtrnFitsRead:EV_TIME:CHANGE", sizeof(int) );
  BnkDef( "HXDtrnFitsRead:PI:CHANGE", sizeof(int) );
  BnkDef( "HXDtrnFitsRead:QUALITY:CHANGE", sizeof(int) );

  BnkDef( "HXD:ALL:EVENTTYPE", sizeof(int) );
  
  BnkDef( "HXD:ALL:UPDATE" ,   sizeof(int) );
  
  *status = ANL_OK;
}

void
HXDtrnFitsRead_com(int *status)
{

  char iomode_strings[PIL_LINESIZE];
  char gtimode_strings[PIL_LINESIZE];
  
  char time_change_strings[PIL_LINESIZE];
  char pi_change_strings[PIL_LINESIZE];
  char quality_change_strings[PIL_LINESIZE];

  int gtimode, time_change, pi_change, quality_change;

  if ( *status ) { /* ftools */
    
    *status = 0;
    /*    
    UCLGST("read_iomode", iomode_strings, *status);
    */
    *status = PILGetString("read_iomode", iomode_strings);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetString read_iomode error (%d)\n",
	      pname, *status );
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
      /*
      UCLGST("time_change", time_change_strings,
	     *status);
	     */
      *status = PILGetBool("time_change", &time_change);
      if ( *status ) {
	fprintf(stderr, "%s: PILGetString time_change error (%d)\n",
		pname, *status );
	*status = ANL_QUIT;
	exit(-1);
      } else {
         BnkPut("HXD:PIL:time_change", sizeof(int),
                &time_change);
      }

      if( time_change == 1 ){
	com.time_change = TRUE;
      } else if( time_change == 0 ){
	com.time_change = FALSE;
      } else {
	fprintf(stderr, "%s : no such answer : yes or no\n",
		time_change_strings);
	*status = ANL_QUIT;
	exit(-1);
      }
      /*
      UCLGST("pi_change", pi_change_strings,
	     *status);
	     */
      *status = PILGetBool("pi_change", &pi_change);
      if ( *status ) {
	fprintf(stderr, "%s: PILGetString pi_change error (%d)\n",
		pname, *status );
	*status = ANL_QUIT;
	exit(-1);
      } else {
	BnkPut("HXD:PIL:pi_change", sizeof(int),&pi_change);
      }

      if( pi_change == 1){
	com.pi_change = TRUE;
      } else if( pi_change == 0){
	com.pi_change = FALSE;
      } else {
	fprintf(stderr, "%s : no such answer : yes or no\n",
		pi_change_strings);
	*status = ANL_QUIT;
	exit(-1);
      }
      
      /*
      UCLGST("quality_change", quality_change_strings,
	     *status);
	     */
      *status = PILGetBool("quality_change", &quality_change );
      if ( *status ) {
	fprintf(stderr, "%s: PILGetString quality_change error (%d)\n",
		pname, *status );
	*status = ANL_QUIT;
	exit(-1);
      } else {
	BnkPut("HXD:PIL:quality_change", sizeof(int),
	       &quality_change);
      }

      if( quality_change == 1 ){
	com.quality_change = TRUE;
      } else if( quality_change == 0 ){
	com.quality_change = FALSE;
      } else {
	fprintf(stderr, "%s : no such answer : yes or no\n",
		quality_change_strings);
	*status = ANL_QUIT;
	exit(-1);
      }
      
    }
    /*
    UCLGST("gtimode", gtimode_strings, *status);
    */
    *status = PILGetBool("gtimode", &gtimode);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetString gtimode error (%d)\n",
	      pname, *status );
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
      fprintf(stderr,"%s : no such answer : yes or no\n" ,
	      gtimode_strings);
      *status = ANL_QUIT;
      exit(-1);
    }
    
    if( com.gtimode == TRUE ){
      /*
      UCLGST("gti_time", com.time , *status);
      */
      *status = PILGetString("gti_time", com.time);
      if ( *status ) {
	fprintf(stderr, "%s: PILGetString gti_time error (%d)\n",
		pname, *status );
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


    if ( com.iomode == HXD_CREATE ){
      /*
      UCLGST("create_name", com.newfilename, *status);
      */
      *status = PILGetFname("create_name", com.newfilename);
      if ( *status ) {
	fprintf(stderr, "%s: PILGetFname create_name error (%d)\n",
		pname, *status );
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
    }
    
    *status = ANL_OK;
    return;
  } /** Ftools End **/

  iomode_strings[0]='\0';
  gtimode_strings[0]='\0';    
  time_change_strings[0]='\0';  
  pi_change_strings[0]='\0';
  quality_change_strings[0]='\0';
  
  CLtxtrd("HXD trn fits input I/O mode : readonly or overwrite or create",
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
    
    CLtxtrd("HXD trn fits update TIME : yes or no",
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
        
    CLtxtrd("HXD trn fits update PI : yes or no",
	    pi_change_strings, sizeof(pi_change_strings) );
    
    if(!strcmp("yes", pi_change_strings)){
      com.pi_change = TRUE;
    } else if(!strcmp("no", pi_change_strings)){
      com.pi_change = FALSE;
    } else {
      fprintf(stderr, "%s : no such answer : yes or no",
	      pi_change_strings);
      *status = ANL_QUIT;
    }
    
    CLtxtrd("HXD trn fits update QUALITY : yes or no",
	    quality_change_strings, sizeof(quality_change_strings) );
    
    if(!strcmp("yes", quality_change_strings)){
      com.quality_change = TRUE;
    } else if(!strcmp("no", quality_change_strings)){
      com.quality_change = FALSE;
    } else {
      fprintf(stderr, "%s : no such answer : yes or no",
	      quality_change_strings);
      *status = ANL_QUIT;
    }
    
  }
  
  CLtxtrd("HXD trn fits using GTI : yes or no", gtimode_strings,
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
    
    CLtxtrd("HXD trn fits time using TIME or S_TIME", com.time,
	    sizeof(com.time) );
    
    if( strcmp("TIME",com.time) && strcmp("S_TIME",com.time)){
      fprintf(stderr,"%s : no such time",com.time);
      *status = ANL_QUIT;
      return;
    }
  }
  
  com.filename[0] = '\0';
  
  CLtxtrd("HXD trn fits file name",
	  com.filename, sizeof(com.filename));
  
  if ( com.iomode == HXD_CREATE ){
    
    com.newfilename[0] = '\0';
    
    CLtxtrd("HXD trn fits created file name",
	    com.newfilename, sizeof(com.newfilename));
    
  }
  
  *status = ANL_OK;
}

void
HXDtrnFitsRead_his(int *status)
{
  *status = ANL_OK;
}

static int
openFITS(){
  
  int istat = 0;
  
  int casesen = TRUE;
  int hdutype;
  char comment[80];
  
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
    fits_movabs_hdu(com.fp, HXD_TRN_FITS_EVENT_EXTENTION , &hdutype,
		    &istat);
  }    
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } else {
    fits_read_key_lng(com.fp, "NAXIS2", &fits.nrow, comment,
		      &istat);
  }    
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_key('NAXIS2') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    hxdtrnFits_col_num(com.fp , fits.colnum , &istat );
  } if( istat ){
    return ANL_NG;
  }

  if( com.gtimode == TRUE ){
    fits_get_colnum( com.fp, casesen, com.time, &fits.gti_time_colnum, &istat);
    
    if ( istat ) {
      fprintf(stderr, "%s: fits_get_colnum('%s') failed (%d)\n",
	      pname, com.time, istat);
      return ANL_NG;    
    }
    
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
    hxdgtiFits_readGTI ( com.fp, HXD_TRN_FITS_GTI_EXTENTION, &gti, &istat );
    BnkfPutM( "HXDtrnFitsRead:GTI", sizeof(HXD_GTI), &gti );
  }
    
  if ( istat ) {
    return ANL_NG;
  } else {
    fits_movabs_hdu(com.fp, HXD_TRN_FITS_EVENT_EXTENTION , &hdutype,
		    &istat);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_movrel_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  }
  
  BnkfPutM( "HXDtrnFitsRead:NROW", sizeof(long), &fits.nrow );
  
  BnkfPutM("HXDtrnFitsRead:FILE_NAME", strlen(com.filename)+1,
	   com.filename);
  
  BnkfPutM("HXDtrnFitsRead:FILE_P", sizeof(com.fp), &com.fp);
  
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
HXDtrnFitsRead_bgnrun(int *status)
{
  
  int evtyp = HXD_EVTYP_TRB;
  
  if( openFITS() ){
    *status = ANL_QUIT;
    return;
  }
  
  closeFITS();
  
  com.irow = 1;
  
  BnkfPutM("HXDtrnFitsRead:IOMODE", sizeof(int), &com.iomode );
  
  BnkfPutM("HXDtrnFitsRead:GTIMODE", sizeof(int), &com.gtimode );
  
  BnkfPutM("HXDtrnFitsRead:EV_TIME:CHANGE", sizeof(int),
	   &com.time_change );
  BnkfPutM("HXDtrnFitsRead:PI:CHANGE", sizeof(int),
	   &com.pi_change );
  
  BnkfPutM("HXDtrnFitsRead:QUALITY:CHANGE", sizeof(int),
	   &com.quality_change );
  
  BnkfPutM("HXDtrnFitsRead:NEW_FILE_NAME", strlen(com.newfilename)+1,
	   com.newfilename);
  
  BnkfPutM("HXD:ALL:EVENTTYPE", sizeof(int), &evtyp );
  
  if( reopenFITS() ){
    *status = ANL_QUIT;
    return;
  }
  
  *status = ANL_OK;
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
HXDtrnFitsRead_ana(int nevent, int eventid, int *status)
{
    
  int istat = 0;
  
  HxdTrnFits data;
  
  int update = HXD_UPDATE_TRN;
  
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
  
  hxdtrnFits_col_read( com.fp, com.irow, fits.colnum, &data, &istat );
  
  if( istat ){
    *status = ANL_QUIT;
    return;	
  }
  
  hxdtrnFitsToBnk_put( &data );
  
  BnkfPutM("HXDtrnFitsRead:IROW", sizeof(int), &com.irow);
  
  com.irow ++;
  
  *status = ANL_OK;
  
}

void
HXDtrnFitsRead_endrun(int *status)
{
  int istat = 0;
  int hdutype;
  int hdunum;

  if (com.iomode != HXD_OVERWRITE) {
    *status = ANL_OK;
    return;
  }

  /** 1st extension **/
  fits_movabs_hdu(com.fp, HXD_TRN_FITS_PRIMARY_HDU, &hdutype, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, istat);
    *status = ANL_NG;
    return;
  } else {/** write credits into HISTORY **/
    hxdFitsHeader_writeCredits(com.fp, anl_task_name(), 
			       anl_task_version(), anl_task_credits(), 
			       HXD_FITS_HEADER_UTIL_CREDIT_OVERWRITE, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (primary: %d)\n",
	    pname, istat);
    *status = ANL_NG;
    return;
  }     
  if (hxdFitsHeader_writeParamer_trn( com.fp, HXD_TRN_FITS_PRIMARY_HDU)
      != HXD_FITS_HEADER_UTIL_OK){
    fprintf(stderr, "%s: error in writing parameters\n", pname);
    return;
  } else {
    fits_write_chksum(com.fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
    *status = ANL_NG;
    return;
  } 

  /** 2nd extension **/
  fits_movabs_hdu(com.fp, HXD_TRN_FITS_EVENT_EXTENTION, &hdutype, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, istat);
    *status = ANL_NG;
    return;
  } else {/** write credits into HISTORY **/
    hxdFitsHeader_writeCredits(com.fp, anl_task_name(), 
			       anl_task_version(), anl_task_credits(), 
			       HXD_FITS_HEADER_UTIL_CREDIT_OVERWRITE, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (secondary: %d)\n",
	    pname, istat);
    *status = ANL_NG;
    return;
  } 

  if (hxdFitsHeader_writeParamer_trn( com.fp, HXD_TRN_FITS_EVENT_EXTENTION)
      != HXD_FITS_HEADER_UTIL_OK){
    fprintf(stderr, "%s: error in writing parameters\n", pname);
    return;
  } else {
    fits_write_chksum(com.fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
    *status = ANL_NG;
    return;
  } 
  
  /** 3rd extension **/
  fits_movabs_hdu(com.fp, HXD_TRN_FITS_GTI_EXTENTION, &hdutype, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, istat);
    *status = ANL_NG;
    return;
  } else {
    hxdFitsHeader_writeCredits(com.fp, anl_task_name(), 
			       anl_task_version(), anl_task_credits(), 
			       HXD_FITS_HEADER_UTIL_CREDIT_OVERWRITE, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: hxdFitsHeader_writeCredits failed (gti: %d)\n", 
	    pname, istat);
    *status = ANL_NG;
    return;
  } 
  if (hxdFitsHeader_writeParamer_trn( com.fp, HXD_TRN_FITS_GTI_EXTENTION)
      != HXD_FITS_HEADER_UTIL_OK){
    fprintf(stderr, "%s: error in writing parameters\n", pname);
    return;
  } else {
    fits_write_chksum(com.fp, &istat);
  }
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_chksum failed (%d)\n", pname, istat);
    *status = ANL_NG;
    return;
  } 

  *status = ANL_OK;
  return;
}

void
HXDtrnFitsRead_exit(int *status)
{

  if(closeFITS()){
    *status = ANL_QUIT;
    return;
  }
  
  *status = ANL_OK;
}
