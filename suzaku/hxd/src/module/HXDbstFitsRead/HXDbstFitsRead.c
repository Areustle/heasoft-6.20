/*
 *  HXDbstFitsRead
 *    v0.0.1  created 1999.10.13  By Y Terada
 *    v0.0.3  add error message 1999-12-23 Y.Terada
 *    v0.0.4  modefied 1999.12.23 By Y. Teada
 *       Change param name for the third release of HXD-FTOOLS.
 *    v0.0.5  modefied 2003.04.14 By Y. Teada
 *       For HXD-II format (hxdbstFitsUtil v0.1.0).
 *    v0.0.6  modefied 2003.05.03 By Y. Teada
 *    v0.1.0  modefied 2003-07-23 by Y.Terada 
 *          for HEADAS, PIL.
 *    v0.1.2  modefied 2003-11-04 by Y.Terada 
 *          put PIL value
 *    v0.1.3  modefied 2003-11-08 by Y.Terada 
 *    v0.1.4  modefied 2006-07-26 by Y.Terada 
 *       ---> v1.0.0
 *    v2.0.0  modefied 2006-09-15 by Y.Terada 
 *       for version 2.0 format
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

#include "hxdbstFitsUtil.h"
#include "hxdbstFitsToBnkUtil.h"
#include "hxdgtiFitsUtil.h"
#include "hxdeventFitsUtil.h"

#define FILENAME_MAX_LENGTH PIL_LINESIZE
#define HXD_TPU_BOARD_NUM   4

enum{
  HXD_READONLY,
  HXD_OVERWRITE,
  HXD_CREATE
};

char HXDbstFitsRead_version[] = "version 2.0.0";

static char pname[] = "HXDbstFitsRead";

static struct {
  char filename[FILENAME_MAX_LENGTH];
  char newfilename[FILENAME_MAX_LENGTH];
  fitsfile *fp;
  int irow;
  int iomode;
} com;

static HXD_GTI gti;

/*static long freezed_time[HXD_TPU_BOARD_NUM];*/
/* why long? double? (only 3 Byte) */

static struct{
  int colnum[HXD_BST_FITS_KEY_NUM];
  long nrow;
  long version;
} fits;

void
HXDbstFitsRead_startup(int *status)
{
  int ftools_no = 0;
  BnkDef( "HXD:ftools:hxdbsttime_yn",       sizeof(int));
  BnkPut( "HXD:ftools:hxdbsttime_yn",       sizeof(int), &ftools_no);

  BnkDef( "HXD:PIL:read_iomode",      sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:input_name",       sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:create_name",      sizeof(char)*HXDFITSHEADER_LINESIZE);

  BnkDef( "HXD:BST:format_version",    sizeof(int));

  *status = ANL_OK;
}

void
HXDbstFitsRead_init(int *status)
{
  int i;
  
  hxdbstFitsToBnk_init();
  
  BnkDef( "HXDbstFitsRead:IROW", sizeof(long) );
  
  BnkDef( "HXDbstFitsRead:FILE_P", sizeof(fitsfile) );
  BnkDef( "HXDbstFitsRead:FILE_NAME", sizeof(com.filename) -1);
  BnkDef( "HXDbstFitsRead:NEW_FILE_NAME", sizeof(com.newfilename) -1);
  BnkDef( "HXDbstFitsRead:IOMODE", sizeof(int) );
  BnkDef( "HXDbstFitsRead:NROW", sizeof(long) );
  
  BnkDef( "HXD:ALL:EVENTTYPE", sizeof(int) );
  
  BnkDef( "HXD:ALL:UPDATE" ,   sizeof(int) );
  
  *status = ANL_OK;
}

void
HXDbstFitsRead_com(int *status)
{

  char iomode_strings[PIL_LINESIZE];

  int i;
  
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
      fprintf(stderr,"%s : no such I/O mode\n",pname);
      *status = ANL_QUIT;
      exit(-1);
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

    if ( *status ) {
      fprintf(stderr,"%s : Cannot get input file name\n",pname);
      *status = ANL_QUIT;
      exit(-1);
    }
    
    if ( com.iomode == HXD_CREATE ){
      /*
      UCLGST("create_name", com.newfilename, *status);
      */
      *status = PILGetFname("create_name", com.newfilename);
      if ( *status ) {
	  fprintf(stderr,"%s: PILGetFname create_name error (%d)\n",
		  pname, *status);
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
  }
  
  iomode_strings[0]='\0';

  CLtxtrd("HXD bst fits input I/O mode : readonly or overwrite or create",
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
  
  com.filename[0] = '\0';
  
  CLtxtrd("HXD bst fits file name", com.filename, sizeof(com.filename));
  
  if ( com.iomode == HXD_CREATE ){
    
    com.newfilename[0] = '\0';
    
    CLtxtrd("HXD bst fits created file name",
	    com.newfilename, sizeof(com.newfilename));
    
  }
  
  *status = ANL_OK;
}

void
HXDbstFitsRead_his(int *status)
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
    fits_movabs_hdu(com.fp, HXD_BST_FITS_EVENT_EXTENTION , &hdutype,
		    &istat);
  }    
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  } 


  fits_read_key_lng(com.fp, "HXD_FVER", &fits.version, comment,&istat);

  if ( istat ) {
    /** continue **/
    fprintf(stdout, "%s: fits_read_key('HXD_FVER') failed (%d)\n",
	    pname, istat);
    fprintf(stdout, "%s: Continue. Set HXD FORMAT Version = 1\n",  pname);
    fits.version = 1;
    istat = 0; 
  } else {
    if (fits.version != HXD_EVENT_FITS_FORMAT_VERSION){
      fprintf(stderr, "%s: HXD Fits Format Version (%d) is inconsistent.",
              pname, fits.version);
      fprintf(stderr, "%s: Version %d is required. Stop the process.",
              pname, HXD_EVENT_FITS_FORMAT_VERSION);
      return ANL_NG;
    }
  }

  {
    int version_id = (int) fits.version;
    BnkPut("HXD:BST:format_version",    sizeof(int), &version_id);
  }

  fits_read_key_lng(com.fp, "NAXIS2", &fits.nrow, comment, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_key('NAXIS2') failed (%d)\n",
	    pname, istat);
    return ANL_NG;
  } else {
    int form_version = (int) fits.version;
    hxdbstFits_col_num(com.fp , fits.colnum , form_version, &istat );
  } if( istat ){
    return ANL_NG;
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
    fits_movabs_hdu(com.fp, HXD_BST_FITS_EVENT_EXTENTION , &hdutype,
		    &istat);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n", pname, istat);
    return ANL_NG;
  }
  
  BnkfPutM( "HXDbstFitsRead:NROW", sizeof(long), &fits.nrow );
  
  BnkfPutM("HXDbstFitsRead:FILE_NAME", strlen(com.filename)+1,
	   com.filename);
  
  BnkfPutM("HXDbstFitsRead:FILE_P", sizeof(com.fp), &com.fp);
  
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
HXDbstFitsRead_bgnrun(int *status)
{
  
  int istat = 0;
  
  int evtyp = HXD_EVTYP_BST;

  long bst_freezed_time_ct[4];
  long tpu_time_mode[4];
  double bst_freezed_time[4];
  int bst_freezed_time_ct_int[4];
  int tpu_time_mode_int[4];

  int i;
  
  if( openFITS() ){
    fprintf(stderr,"%s_bgnrun : Open fits failed.\n",pname);
    *status = ANL_QUIT;
    return;
  }
  
  closeFITS();
  
  com.irow = 1;
  
  BnkfPutM("HXDbstFitsRead:IOMODE", sizeof(int), &com.iomode );  
  BnkfPutM("HXDbstFitsRead:NEW_FILE_NAME", strlen(com.newfilename)+1,
	   com.newfilename);  
  BnkfPutM("HXD:ALL:EVENTTYPE", sizeof(int), &evtyp );
  
  if( reopenFITS() ){
    fprintf(stderr,"%s_bgnrun : Re-open fits failed.\n",pname);
    *status = ANL_QUIT;
    return;
  }
  {
    int form_version = (int) fits.version;
    hxdbstFits_read_key(com.fp, bst_freezed_time_ct, bst_freezed_time,
			tpu_time_mode, form_version, &istat );
  }
  if(istat){
    fprintf(stderr,"%s_bgnrun : Cannot get freezed time.\n",pname);
    *status = ANL_QUIT;
  }

  for(i=0;i<4;i++){
    bst_freezed_time_ct_int[i] = (int)bst_freezed_time_ct[i];
    tpu_time_mode_int[i] = (int)tpu_time_mode[i];    
  }
  
  BnkfPutM("HXD:BST:FRZD_TM", sizeof(double)*4, bst_freezed_time);
  BnkfPutM("HXD:BST:FRTM_CT", sizeof(int)*4, bst_freezed_time_ct_int);
  BnkfPutM("HXD:BST:TM_MODE", sizeof(int)*4, tpu_time_mode_int);
    
  *status = ANL_OK;
  
}

void
HXDbstFitsRead_ana(int nevent, int eventid, int *status)
{
    
  int istat = 0;
  
  HxdBstFits data;
  
  int update = HXD_UPDATE_BST;
  
  if ( fits.nrow < com.irow ) {
    /* fprintf(stderr,"%s_ana : End of File\n",pname); */
    *status = ANL_QUIT;
    return;
    
  }
  
  BnkfPutM("HXD:ALL:UPDATE", sizeof(int), &update);
  
  hxdbstFits_col_read( com.fp, com.irow, fits.colnum, &data, &istat );

  if( istat ){
    fprintf(stderr,"%s_ana : Cannot read burst date\n",pname);
    *status = ANL_QUIT;
    return;	
  }
  
  hxdbstFitsToBnk_put( &data );
  
  /*
  BnkfPutM("HXD:BST:FREEZED_TIME", sizeof(long),
	   &freezed_time[data.bst_board]);
  
  BnkfPutM("HXDbstFitsRead:IROW", sizeof(int), &com.irow);
  */
  
  com.irow ++;
  
  *status = ANL_OK;
  
}

void
HXDbstFitsRead_endrun(int *status)
{
  *status = ANL_OK;
}

void
HXDbstFitsRead_exit(int *status)
{

  if(closeFITS()){
    *status = ANL_QUIT;
    return;
  }
  
  *status = ANL_OK;
}
