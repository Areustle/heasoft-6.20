
/*
 *  HXDmkgainhist
 *  v0.1.0 2003-07-23 for HEADAS, by Y Terada, H Takahashi, M Suzuki
 *  v0.2.0 2005-02-05, new gainhistory method, by Y.Terada
 *         with hxdcaldbUtil and hxdgainhistry (script).
 *         delete hxdgainhistUtil.
 *  v0.2.1 2005-02-07, debug, by Y.Terada
 *  v0.2.2 2005-05-30, new format GHF (support FAST) by Y.Terada
 *  v0.3.0 2005-06-28, add WritePHA. (renamed) by Y.Terada
 *  v0.4.0 2005-10-24, new format (add new lines in orbit), by Y.Terada
 *  v0.4.1 2005-11-02, delete debug message, by Y.Terada
 *  v0.4.2 2005-11-04, BnkPut PIL parameters,   by Y.Terada
 *  v0.4.3 2005-11-08, BnkPut PIL parameters,   by Y.Terada
 *  v0.4.4 2006-08-18, Save memory   by Y.Terada
 *  v0.4.5 2006-08-24, Save memory 2 by Y.Terada
 *  --> v1.0.0
 *  v2.0.0 2006-09-10, for version 2.0 by Y.Terada
 *  v2.0.1 2007-04-27, with caldbUtil v0.6.6
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
#include "fitsio.h"
#include "HXD.h"
#include "hxdcaldbUtil.h"

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"

#include "hxdFitsHeaderUtil.h"

#define MAX_LENGTH PIL_LINESIZE

char HXDmkgainhistWriteGHF_version[] = "version 0.4.5";

static char pname[] = "HXDmkgainhistWriteGHF";

static struct {
  int  ghf_mode;
  char pin_fitlog_filename[MAX_LENGTH];
  char gso_fitlog_file_list[MAX_LENGTH];
  int  gso_flist_valid;
  char gso_fitlog_gda_filename[MAX_LENGTH];/* listed in gso_fitlog_file_list */
  char gso_fitlog_ani_filename[MAX_LENGTH];/* listed in gso_fitlog_file_list */
  char gso_fitlog_150_filename[MAX_LENGTH];/* listed in gso_fitlog_file_list */
  char pin_gainhistory_filename[MAX_LENGTH];
  char gso_gainhistory_filename[MAX_LENGTH];
  char fits_date[MAX_LENGTH];
  char fits_time[MAX_LENGTH];
  int add;
}com;

FILE* gso_fitlog_file_list_fp;

/*
static PIN_GHF pinghf_data;
static GSO_GHF gsoghf_gda_slow_data;
static GSO_GHF gsoghf_gda_fast_data;
static GSO_GHF gsoghf_ani_slow_data;
static GSO_GHF gsoghf_ani_fast_data;
static GSO_GHF gsoghf_150_slow_data;
static GSO_GHF gsoghf_150_fast_data;
*/

static PIN_GHF *pinghf_data = NULL;
static GSO_GHF *gsoghf_gda_slow_data = NULL; /** Gd alpha, 340 keV **/
static GSO_GHF *gsoghf_gda_fast_data = NULL;
static GSO_GHF *gsoghf_ani_slow_data = NULL; /** Annihilation, 511 keV **/
static GSO_GHF *gsoghf_ani_fast_data = NULL;
static GSO_GHF *gsoghf_150_slow_data = NULL; /** Gd, 150 keV **/
static GSO_GHF *gsoghf_150_fast_data = NULL;

void
HXDmkgainhistWriteGHF_startup(int *status){ 
  int used = 1;
  BnkPut( "HXD:ftools:hxdmkgainhist_yn", sizeof(int), &used);

  BnkDef( "HXD:PIL:ghfwrite_mode",   sizeof(int));
  BnkDef( "HXD:PIL:pin_fitlog_name", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:gso_fitlog_list", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:gso_gdalph_name", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:gso_annihi_name", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:gso_gd150k_name", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:valid_date",      sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:valid_time",      sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:pin_gainhist_name", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:gso_gainhist_name", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:gainhistory_iomode",sizeof(char)*HXDFITSHEADER_LINESIZE);

  /****** Memory Allocation ******/
  if(pinghf_data == NULL){
    pinghf_data = (PIN_GHF*) malloc( sizeof(PIN_GHF) );
  } else {
    fprintf(stderr,"hxdpiUtil: Memory, pinghf_data, is already allocated\n");
    *status = ANL_NG;;
    return;
  }

  if(gsoghf_gda_slow_data == NULL){
    gsoghf_gda_slow_data = (GSO_GHF*) malloc( sizeof(GSO_GHF) );
  } else {
    fprintf(stderr,"hxdpiUtil: Memory, gsoghf_gda_slow_data, is already allocated\n");
    *status = ANL_NG;;
    return;
  }

  if(gsoghf_gda_fast_data == NULL){
    gsoghf_gda_fast_data = (GSO_GHF*) malloc( sizeof(GSO_GHF) );
  } else {
    fprintf(stderr,"hxdpiUtil: Memory, gsoghf_gda_fast_data, is already allocated\n");
    *status = ANL_NG;;
    return;
  }

  if(gsoghf_ani_slow_data == NULL){
    gsoghf_ani_slow_data = (GSO_GHF*) malloc( sizeof(GSO_GHF) );
  } else {
    fprintf(stderr,"hxdpiUtil: Memory, gsoghf_ani_slow_data, is already allocated\n");
    *status = ANL_NG;;
    return;
  }

  if(gsoghf_ani_fast_data == NULL){
    gsoghf_ani_fast_data = (GSO_GHF*) malloc( sizeof(GSO_GHF) );
  } else {
    fprintf(stderr,"hxdpiUtil: Memory, gsoghf_ani_fast_data, is already allocated\n");
    *status = ANL_NG;;
    return;
  }

  if(gsoghf_150_slow_data == NULL){
    gsoghf_150_slow_data = (GSO_GHF*) malloc( sizeof(GSO_GHF) );
  } else {
    fprintf(stderr,"hxdpiUtil: Memory, gsoghf_150_slow_data, is already allocated\n");
    *status = ANL_NG;;
    return;
  }

  if(gsoghf_150_fast_data == NULL){
    gsoghf_150_fast_data = (GSO_GHF*) malloc( sizeof(GSO_GHF) );
  } else {
    fprintf(stderr,"hxdpiUtil: Memory, , is already allocated\n");
    *status = ANL_NG;;
    return;
  }
  
  *status = ANL_OK; 
}

void
HXDmkgainhistWriteGHF_com(int *status) {
  
  char add_string[MAX_LENGTH];
  char mod_string[MAX_LENGTH];
  static char flist[MAX_LENGTH];
  
  if ( *status ) { /* ftools */
    
    *status = 0;

    *status = PILGetBool("ghfwrite_mode", &com.ghf_mode);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    } else {
      BnkPut("HXD:PIL:ghfwrite_mode",   sizeof(int), &com.ghf_mode);
    }

    if (! com.ghf_mode){
      *status = ANL_OK;
      return;
    }

    *status = PILGetFname("pin_fitlog_name", com.pin_fitlog_filename);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    } else {
      int string_size = strlen(com.pin_fitlog_filename);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning pin_fitlog_name is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:pin_fitlog_name", string_size,
	     com.pin_fitlog_filename);
    }
    

    *status = PILGetFname("gso_fitlog_name", flist);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    } else {
      int string_size = strlen(flist);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning pin_fitlog_name is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:gso_fitlog_list", string_size, flist);
    }
    if (flist[0] == '@'){
      /** gso_fitlog_name is a file list. **/
      com.gso_flist_valid=1;
      sprintf(com.gso_fitlog_file_list, "%s", flist+1);
      gso_fitlog_file_list_fp = fopen(com.gso_fitlog_file_list, "r");
      if (gso_fitlog_file_list_fp == NULL){
	fprintf(stderr, "%s: file %s open error\n", pname,
		com.gso_fitlog_file_list);
	*status = ANL_QUIT;
	exit(-1);
      }
      fscanf(gso_fitlog_file_list_fp, "%s\n", com.gso_fitlog_gda_filename);
      fscanf(gso_fitlog_file_list_fp, "%s\n", com.gso_fitlog_ani_filename);
      fscanf(gso_fitlog_file_list_fp, "%s\n", com.gso_fitlog_150_filename);
    } else {
      /** gso_fitlog_name is not a file list. OLD version. **/
      /*
      fprintf(stderr, "%s: gso_fitlog_name does not start at @",
              pname);
      */
      com.gso_flist_valid=0;
      sprintf(com.gso_fitlog_gda_filename, "%s", flist);
      sprintf(com.gso_fitlog_ani_filename, "NULL");
      sprintf(com.gso_fitlog_150_filename, "NULL");
    }
    
    {
      int string_size;
      string_size = strlen(com.gso_fitlog_gda_filename);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
	fprintf(stderr, "%s: warning gso_fitlog_gda_filename is long\n", 
		pname);
	string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:gso_gdalph_name", string_size,
	     com.gso_fitlog_gda_filename);
      
      string_size = strlen(com.gso_fitlog_ani_filename);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
	fprintf(stderr, "%s: warning gso_fitlog_anni_filename is long\n", 
		pname);
	string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:gso_annihi_name", string_size,
	     com.gso_fitlog_ani_filename);
      
      string_size = strlen(com.gso_fitlog_150_filename);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
	  fprintf(stderr, "%s: warning gso_fitlog_gd150_filename is long\n", 
		  pname);
	  string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:gso_gd150k_name", string_size,
	     com.gso_fitlog_150_filename);
    }
    
    *status = PILGetString("valid_date", com.fits_date);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    } else {
      int string_size = strlen(com.fits_date);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning valid_date is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:valid_date", string_size,
	     com.fits_date);
    }

    *status = PILGetString("valid_time", com.fits_time);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    } else {
      int string_size = strlen(com.fits_time);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning valid_time is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:valid_date", string_size, com.fits_time);
    }

    *status = PILGetFname("pin_gainhist_name", com.pin_gainhistory_filename);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    } else {
      int string_size = strlen(com.pin_gainhistory_filename);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning pin_gainhist_name is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:pin_gainhist_name", string_size, 
	     com.pin_gainhistory_filename);
    }
    
    *status = PILGetFname("gso_gainhist_name", com.gso_gainhistory_filename);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    } else {
      int string_size = strlen(com.gso_gainhistory_filename);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning gso_gainhist_name is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:gso_gainhist_name", string_size, 
	     com.pin_gainhistory_filename);
    }
    
    *status = PILGetString("gainhistory_iomode", add_string);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    } else {
      int string_size = strlen(add_string);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning gainhistory_iomode is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXD:PIL:gainhistory_iomode", string_size, add_string);
    }
    
    if(!strcmp("yes", add_string)){
      com.add = ANL_TRUE;
    } else if(!strcmp("no", add_string)){
      com.add = ANL_FALSE;
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

  com.gso_gainhistory_filename[0] = '\0';
  com.gso_fitlog_file_list[0] = '\0';
  com.pin_gainhistory_filename[0] = '\0';
  com.pin_fitlog_filename[0] = '\0';
  com.fits_date[0]='\0';
  com.fits_time[0]='\0';
  add_string[0] = '\0';
  mod_string[0] = '\0';
  
  CLtxtrd("GHF Write Mode? (yes or no)",
	  mod_string, sizeof(mod_string) );
  if(!strcmp("yes", mod_string)){
    com.ghf_mode=1;
  } else if(!strcmp("no", mod_string)){
    com.ghf_mode=0;
  } else {
    fprintf(stderr, "%s : no such answer : yes or no\n",
	    mod_string);
    *status = ANL_QUIT;
    exit(-1);
  }

  if (! com.ghf_mode){
    *status = ANL_OK;
    return;
  }

  CLtxtrd("HXD PIN input fit log file name",
	  com.pin_fitlog_filename, sizeof(com.pin_fitlog_filename) );
  
  CLtxtrd("HXD GSO input fit log file name",
	  com.gso_fitlog_file_list, sizeof(com.gso_fitlog_file_list) );
  
  CLtxtrd("Valid Date (YYYY-MM-DD)for Gain History Files",
	  com.fits_date, sizeof(com.fits_date) );

  CLtxtrd("Valid Time (hh:mm:ss) for Gain History Files",
	  com.fits_time, sizeof(com.fits_time) );

  CLtxtrd("HXD PIN gain history file name",
	  com.pin_gainhistory_filename, sizeof(com.pin_gainhistory_filename) );
  
  CLtxtrd("HXD GSO gain history file name",
	  com.gso_gainhistory_filename, sizeof(com.gso_gainhistory_filename) );
  
  CLtxtrd("Add output to existing file",
	  add_string, sizeof(add_string) );
  
  if(!strcmp("yes", add_string)){
    com.add = ANL_TRUE;
  } else if(!strcmp("no", add_string)){
    com.add = ANL_FALSE;
  } else {
    fprintf(stderr, "%s : no such answer : yes or no\n",
	    add_string);
    *status = ANL_QUIT;
    exit(-1);
  }
  
  *status = ANL_OK;
  
}

void
HXDmkgainhistWriteGHF_init(int *status) {
  int stat;
  int ghf_version = 0;
  if (! com.ghf_mode){
    *status = ANL_OK;
    return;
  }

  stat = hxdcaldbUtil_set_date(com.fits_date, com.fits_time,ghf_version);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: set date failed \n", pname);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

void
HXDmkgainhistWriteGHF_his(int *status){
  *status = ANL_OK; 
}

void
HXDmkgainhistWriteGHF_bgnrun(int *status){
  int stat;

  if (! com.ghf_mode){
    *status = ANL_OK;
    return;
  }

  /** Read ASCII table for GSO GHF (1) **/
  stat = hxdcaldbUtil_gsogainhist_open_ASCII(com.gso_fitlog_gda_filename);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: gso log file open error\n", pname);
    *status = ANL_QUIT;
    return;
  }
  stat = hxdcaldbUtil_gsogainhist_read_ASCII(gsoghf_gda_slow_data, 
					     gsoghf_gda_fast_data);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: gso log file read error\n", pname);
    *status = ANL_QUIT;
    return;
  }
  stat = hxdcaldbUtil_gsogainhist_close_ASCII();
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: gso log file close error\n", pname);
    *status = ANL_QUIT;
    return;
  }
  
  if(com.gso_flist_valid){
    /** Read ASCII table for GSO GHF (2) **/
    stat = hxdcaldbUtil_gsogainhist_open_ASCII(com.gso_fitlog_ani_filename);
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: gso log file open error\n", pname);
      *status = ANL_QUIT;
      return;
    }
    stat = hxdcaldbUtil_gsogainhist_read_ASCII(gsoghf_ani_slow_data, 
					       gsoghf_ani_fast_data);
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: gso log file read error\n", pname);
      *status = ANL_QUIT;
      return;
    }
    stat = hxdcaldbUtil_gsogainhist_close_ASCII();
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: gso log file close error\n", pname);
      *status = ANL_QUIT;
      return;
    }
    
    /** Read ASCII table for GSO GHF (3) **/
    stat = hxdcaldbUtil_gsogainhist_open_ASCII(com.gso_fitlog_150_filename);
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: gso log file open error\n", pname);
      *status = ANL_QUIT;
      return;
    }
    stat = hxdcaldbUtil_gsogainhist_read_ASCII(gsoghf_150_slow_data, 
					       gsoghf_150_fast_data);
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: gso log file read error\n", pname);
      *status = ANL_QUIT;
      return;
    }
    stat = hxdcaldbUtil_gsogainhist_close_ASCII();
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: gso log file close error\n", pname);
      *status = ANL_QUIT;
      return;
    }
  } 

  /** Read ASCII table for PIN GHF **/
  stat = hxdcaldbUtil_pingainhist_open_ASCII(com.pin_fitlog_filename);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: pin log file open error\n", pname);
    *status = ANL_QUIT;
    return;
  }
  stat = hxdcaldbUtil_pingainhist_read_ASCII(pinghf_data);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: pin log file read error\n", pname);
    *status = ANL_QUIT;
    return;
  }
  stat = hxdcaldbUtil_pingainhist_close_ASCII();
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: pin log file close error\n", pname);
    *status = ANL_QUIT;
    return;
  }

  /** Create Fits **/
  stat = hxdcaldbUtil_gsogainhist_create_FITS(com.gso_gainhistory_filename);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: file create error\n", pname);
    *status = ANL_QUIT;
    return;
  }

  stat = hxdcaldbUtil_pingainhist_create_FITS(com.pin_gainhistory_filename);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: file create error\n", pname);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

void
HXDmkgainhistWriteGHF_ana(int nevent, int eventid, int *status){
  if (! com.ghf_mode){
    *status = ANL_OK;
    return;
  }

  *status = ANL_QUIT;
}

void
HXDmkgainhistWriteGHF_endrun(int *status){
  int stat;
  int n_line;

  if (! com.ghf_mode){
    *status = ANL_OK;
    return;
  }

  if(com.gso_flist_valid){
    n_line = 3;
  } else {
    n_line = 1;
  }

  /** write and close gso ghf **/
  stat = hxdcaldbUtil_gsogainhist_write_FITS(n_line,
					     gsoghf_gda_slow_data,
					     gsoghf_gda_fast_data,
					     gsoghf_ani_slow_data,
					     gsoghf_ani_fast_data,
					     gsoghf_150_slow_data,
					     gsoghf_150_fast_data);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: file write error GSO-DNL\n", pname);
    *status = ANL_QUIT;
    return;
  }
  
  stat = hxdcaldbUtil_gsogainhist_close_FITS();
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: file close error\n", pname);
    *status = ANL_QUIT;
    return;
  }

  /** write and close pin ghf **/
  stat = hxdcaldbUtil_pingainhist_write_FITS(pinghf_data);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: file write error PIN-GHF\n", pname);
    *status = ANL_QUIT;
    return;
  }

  stat = hxdcaldbUtil_pingainhist_close_FITS();
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: file close error\n", pname);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

void
HXDmkgainhistWriteGHF_exit(int *status){
  *status = ANL_OK;
}
