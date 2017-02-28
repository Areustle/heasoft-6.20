/*
   HXDgradFITS build v0.1.2
           2005-0?-??  by Y.Terada
   v0.1.3  2005-05-27  by T.Kitaguchi  support PSDSEL curve table I/O
   v0.1.4  2005-08-30  by T.Kitaguchi  support PIN threshold table I/O
   v0.2.0  2005-11-04  by Y.Terada,    put PIL parameters
   v0.2.1  2005-11-08  by Y.Terada,    shorten Bnk nameby
   v0.2.3  2006-06-08  by T.Kitaguchi, input PSDSEL criteria with PIL
   v2.0.2  2007-04-27  by Y.Terada,    support CALDB access function
   v2.0.3  2007-06-22  by T.Kitaguchi, input time info to CALDB access function
   v2.0.4  2007-06-26  by T.Kitaguchi, HXDGRADE_TIMEINFO_MAX_LENGTH 10 -> 16
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "fitsio.h"
#include "cfortran.h"
#include "hxd/HXD.h"
#include "atFunctions.h"

#include "pil.h"
/* #include "headas.h" */
/* #include <pfile.h>    */
/* #include "uclpario.h" */
/* #include "hxdgradeUtil.h" */
#include "hxdFitsHeaderUtil.h"
#include "aste_caldb.h"
#include "aste_time.h"

#define DEBUG 0
#define FILELIST_MAX_LENGTH PIL_LINESIZE
#define HXDGRADE_TIMEINFO_MAX_LENGTH 16

char HXDgradeFITS_version[] = "version 2.0.4";

static char pname[] = "HXDgradeFITS";


static struct{
  char   hxdgrade_psdsel_fname[FILELIST_MAX_LENGTH];
  char   hxdgrade_pinthres_fname[FILELIST_MAX_LENGTH];
  char   o_hxdgrade_psdsel_fname[FILELIST_MAX_LENGTH];
  char   o_hxdgrade_pinthres_fname[FILELIST_MAX_LENGTH];
  int    caldb_type_psdsel;
  int    caldb_type_pinthr;
  double hxdgrade_psdsel_criteria;
} com;


void HXDgradeFITS_startup( int *status ) {
  BnkDef("HXD:PIL:hxdgrade_psdsel_fname",
	 sizeof(char)*HXDFITSHEADER_LINESIZE);

  BnkDef("HXD:PIL:hxdgrade_pinthres_fname",
	 sizeof(char)*HXDFITSHEADER_LINESIZE);

  BnkDef("HXD:PIL:hxdgrade_psdsel_criteria", sizeof(double));

  BnkDef("HXD:PIL:CALDB_TYPE:pinthr", sizeof(int));
  BnkDef("HXD:PIL:CALDB_TYPE:psdsel", sizeof(int));
  com.caldb_type_psdsel = 0;
  com.caldb_type_pinthr = 0;

  *status = ANL_OK;
}



void HXDgradeFITS_com( int *status ) {
  char *k;

  if ( *status ) { /* ftools */
    *status = 0;

    /*** Input PSDSEL (PSD selection) caldb file name ***/
    *status = PILGetFname(k="hxdgrade_psdsel_fname",
			  com.o_hxdgrade_psdsel_fname);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetFname HXDgrade PSD error (%d)\n",
	      pname, *status);
      *status = ANL_QUIT;
      exit(-1);
    }

    /*** Input PINTHR (PIN lower threshold) caldb file name ***/
    *status = PILGetFname(k="hxdgrade_pinthres_fname",
			  com.o_hxdgrade_pinthres_fname);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetFname HXDgrade PIN threshold error (%d)\n",
	      pname, *status);
      *status = ANL_QUIT;
      exit(-1);
    }

    /*** Input PSD selection criteria ***/
    *status = PILGetReal("hxdgrade_psdsel_criteria", &com.hxdgrade_psdsel_criteria);


    if (DEBUG) fprintf(stdout, "%s: PILGetReal HXDgrade PSD selection criteria (%lf)\n",
		       pname, com.hxdgrade_psdsel_criteria);

    if ( *status ) {
      fprintf(stderr, "%s: PILGetReal HXDgrade PSD selection criteria error (%d)\n",
	      pname, *status);
      *status = ANL_QUIT;
      exit(-1);
    } else {
      if  ( com.hxdgrade_psdsel_criteria < 0.0 ) {
        fprintf(stderr, "%s: error hxdgrade_psdsel_criteria is negative\n", pname);
	*status = ANL_QUIT;
	exit(-1);
      }
      BnkPut("HXD:PIL:hxdgrade_psdsel_criteria", sizeof(double),
             &com.hxdgrade_psdsel_criteria);
    }

    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }

    *status = ANL_OK;
    return;
  }

  com.hxdgrade_psdsel_fname[0]   = '\0';
  com.hxdgrade_pinthres_fname[0] = '\0';
  com.hxdgrade_psdsel_criteria   = 2.1;

  CLtxtrd("HXDgrade PSD selection file name", com.hxdgrade_psdsel_fname,
	  sizeof(com.hxdgrade_psdsel_fname));

  CLtxtrd("HXDgrade PIN threshold file name", com.hxdgrade_pinthres_fname,
	  sizeof(com.hxdgrade_pinthres_fname));

  CLfdprd("HXDgrade PSD selection criteria", &com.hxdgrade_psdsel_criteria);

  *status = ANL_OK;
}

void HXDgradeFITS_init( int *status ) {
  BnkDef("HXDgrade:PSDSEL_FILE_NAME",   sizeof(com.hxdgrade_psdsel_fname));
  BnkDef("HXDgrade:PINTHRES_FILE_NAME", sizeof(com.hxdgrade_pinthres_fname));
  BnkDef("HXDgrade:PSDSEL_CRITERIA",    sizeof(com.hxdgrade_psdsel_criteria));

  *status = ANL_OK;
}


void HXDgradeFITS_bgnrun( int *status ) {
  int size, string_size, gsooldpi;
  CALDB_INFO caldb;
  char yyyymmdd_tstart[HXDGRADE_TIMEINFO_MAX_LENGTH];
  char hhmmss_tstart[HXDGRADE_TIMEINFO_MAX_LENGTH];
  /* char yyyymmdd_tstop[HXDGRADE_TIMEINFO_MAX_LENGTH];
     char hhmmss_tstop[HXDGRADE_TIMEINFO_MAX_LENGTH]; */
  AtTimeD attime_tstart;
  double  aetime_tstart;
  /* AtTimeD attime_tstop;
     double  aetime_tstop; */


  BnkfGetM("HXDeventFitsRead:TSTART", sizeof(double), &size, &aetime_tstart);
  /* BnkfGetM("HXDeventFitsRead:TSTOP",  sizeof(double), &size, &aetime_tstop); */

  /** see if the GSOOLDPI keyword is set **/
  BnkfGetM("HXDeventFitsRead:GSOOLDPI", sizeof(int), &size, &gsooldpi);

  aste2attimeD(aetime_tstart, &attime_tstart);
  /* aste2attimeD(aetime_tstop,  &attime_tstop); */

  if ( DEBUG ) {
    printf("TSTART: %f, YYMMDD: %04d-%02d-%02d, HHMMSS: %02d:%02d:%02d\n",
	   attime_tstart.yr, attime_tstart.mo, attime_tstart.dy,
	   attime_tstart.hr, attime_tstart.mn, attime_tstart.sc);
    printf("        %f YYMMDD: %s, HHMMSS: %s\n",
	   aetime_tstart, yyyymmdd_tstart, hhmmss_tstart);
  }

  sprintf(yyyymmdd_tstart, "%04d-%02d-%02d",
	  attime_tstart.yr, attime_tstart.mo, attime_tstart.dy);
  sprintf(hhmmss_tstart, "%02d:%02d:%02d",
	  attime_tstart.hr, attime_tstart.mn, attime_tstart.sc);



  /*-- PSDSEL (PSD selection) caldb access --*/
  if (0==CLstricmp("CALDB", com.o_hxdgrade_psdsel_fname) ){
    aste_caldb_init(&caldb);

    com.caldb_type_psdsel = 1;
    caldb.telescop = "SUZAKU";        /* TELESCOP */
    caldb.instrume = "HXD";           /* INSTRUME */
    caldb.detnam   = "WELL_GSO";      /* DETNAM   */
    if ( gsooldpi ) {
      caldb.codename = "PSDSEL";        /* CCNM0001 */
    } else {
      caldb.codename = "PSDSEL2";        /* CCNM0001 */
    }
    caldb.date0    = yyyymmdd_tstart; /* YYYY-MM-DD in UTC */
    caldb.time0    = hhmmss_tstart;   /* hh:mm:ss.uuuuuu in UTC */


    aste_caldb_get(&caldb);

    if (caldb.status != 0 || caldb.nfound == 0){
      anl_msg_error("%s: no CALDB entry for '%s-%s' (status=%d)\n",
		    pname, caldb.detnam, caldb.codename, caldb.status);
      *status = ANL_QUIT;
      return;
    }
    if (caldb.nfound != 1){
      anl_msg_warning("%s: multiple CALDB entry for '%s' (nfound=%d)\n",
		      pname, caldb.codename, caldb.nfound);
    }
    strcpy(com.hxdgrade_psdsel_fname, caldb.filename);
  } else{
    strcpy(com.hxdgrade_psdsel_fname, com.o_hxdgrade_psdsel_fname);
  }

  /*-- Put filename --*/
  string_size = strlen(com.hxdgrade_psdsel_fname);
  if  (string_size>HXDFITSHEADER_LINESIZE) {
    fprintf(stderr, "%s: warning hxdgrade_psdsel_fname is long\n", pname);
    string_size = HXDFITSHEADER_LINESIZE;
  }
  BnkPut("HXD:PIL:hxdgrade_psdsel_fname", string_size,
	 com.hxdgrade_psdsel_fname);
  BnkPut("HXD:PIL:CALDB_TYPE:psdsel",sizeof(int),&com.caldb_type_psdsel);




  /*-- PINTHR (PIN lower threshold) caldb access --*/
  if (0==CLstricmp("CALDB", com.o_hxdgrade_pinthres_fname) ){
    aste_caldb_init(&caldb);

    com.caldb_type_pinthr = 1;
    caldb.telescop = "SUZAKU";        /* TELESCOP */
    caldb.instrume = "HXD";           /* INSTRUME */
    caldb.detnam   = "WELL_PIN";      /* DETNAM   */
    caldb.codename = "PINTHRES";      /* CCNM0001 */
    caldb.date0    = yyyymmdd_tstart; /* YYYY-MM-DD in UTC */
    caldb.time0    = hhmmss_tstart;   /* hh:mm:ss.uuuuuu in UTC */

    aste_caldb_get(&caldb);

    if (caldb.status != 0 || caldb.nfound == 0){
      anl_msg_error("%s: no CALDB entry for '%s-%s' (status=%d)\n",
		    pname, caldb.detnam, caldb.codename, caldb.status);
      *status = ANL_QUIT;
      return;
    }
    if (caldb.nfound != 1){
      anl_msg_warning("%s: multiple CALDB entry for '%s' (nfound=%d)\n",
		      pname, caldb.codename, caldb.nfound);
    }
    strcpy(com.hxdgrade_pinthres_fname, caldb.filename);
  } else{
    strcpy(com.hxdgrade_pinthres_fname, com.o_hxdgrade_pinthres_fname);
  }

  /*-- Put filename --*/
  string_size = strlen( com.hxdgrade_pinthres_fname );
  if  (string_size>HXDFITSHEADER_LINESIZE) {
    fprintf(stderr, "%s: warning hxdgrade_pinthres_fname is long\n",pname);
    string_size = HXDFITSHEADER_LINESIZE;
  }

  BnkPut("HXD:PIL:hxdgrade_pinthres_fname", string_size,
	 com.hxdgrade_pinthres_fname);
  BnkPut("HXD:PIL:CALDB_TYPE:pinthr",sizeof(int),&com.caldb_type_pinthr);



  BnkfPutM("HXDgrade:PSDSEL_FILE_NAME",   sizeof(com.hxdgrade_psdsel_fname),
	   com.hxdgrade_psdsel_fname);
  BnkfPutM("HXDgrade:PINTHRES_FILE_NAME", sizeof(com.hxdgrade_pinthres_fname),
	   com.hxdgrade_pinthres_fname);
  BnkfPutM("HXDgrade:PSDSEL_CRITERIA",    sizeof(com.hxdgrade_psdsel_criteria),
	   &com.hxdgrade_psdsel_criteria);



 *status = ANL_OK;

}

void HXDgradeFITS_his( int *status ) { *status = ANL_OK; }

void HXDgradeFITS_ana( int nevent, int eventid, int *status ) {
    *status = ANL_OK;
}

void HXDgradeFITS_endrun( int *status ) { *status = ANL_OK; }

void HXDgradeFITS_exit( int *status ) { *status = ANL_OK; }
