/*
 *  HXDleapsecInit
 *
 *  v0.2.0 2003-07-23 for HEADAS, by Y Terada, H Takahashi, M Suzuki
 *  v0.2.1 2005-06-13 by Y.Terada
 *  v0.2.2 2005-11-02 by Y.Terada ---> v1.0.0
 *  v2.0.0 2007-04-27 by Y.Terada for v2.0 / CALDB access function
 *  v2.0.1 2007-05-29 by Y.Terada change parameter name
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
/* #include "hbook.h"*/
#include "HXD.h"

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"

#include "hxdtableFitsUtil.h"
#include "aste_caldb.h"

#define MAX_LENGTH PIL_LINESIZE

char HXDleapsecInit_version[] = "version 2.0.1";
static char pname[] = "HXDleapsecInit";

char *leapsec_filename;
char o_leapsec_filename[MAX_LENGTH];
int  piltype_caldb_leapsec;

void
HXDleapsecInit_startup(int *status){ 
  piltype_caldb_leapsec = 0;
  leapsec_filename = o_leapsec_filename;
  *status = ANL_OK; 
}

void
HXDleapsecInit_com(int *status) {
        char *k;

  if ( *status ) { /* ftools */
    *status = 0;

    *status = PILGetString (k="leapfile", o_leapsec_filename); 

    if(0==CLstricmp("CALDB", o_leapsec_filename)){
      piltype_caldb_leapsec = 1;
    }

    leapsec_filename = aste_caldb_find_leapfile(o_leapsec_filename);

    if(leapsec_filename == NULL) {
      anl_msg_error("%s: cannot get leapsec file. Quit\n", pname);
      *status = ANL_QUIT; return;
    }

    if ( *status ) {
      fprintf(stderr, "%s: leapsec file name (%s) err (%d)\n",
	      pname, leapsec_filename, *status);
      *status = ANL_QUIT;
      exit(-1);
    }
    
    *status = ANL_OK;
    return;
  }
  /* ANL */
  
  o_leapsec_filename[0] = '\0';
  
  CLtxtrd("leapsec file name",
	  o_leapsec_filename, sizeof(o_leapsec_filename) );
  
  leapsec_filename = o_leapsec_filename;
  
  *status = ANL_OK;
  
}

void
HXDleapsecInit_init(int *status) {
  static char leapsec_name[MAX_LENGTH];

  BnkDef("HXD:PIL:CALDB_TYPE:LEAPSEC", sizeof(int));
  BnkDef("HXD:PIL:leapsec_name", sizeof(char)*MAX_LENGTH);
  BnkPut("HXD:PIL:CALDB_TYPE:LEAPSEC", sizeof(int), &piltype_caldb_leapsec);

  if (NULL == (char*) mission_time_init(NULL)) {
    mission_time_init(leapsec_filename);
    BnkPut("HXD:PIL:leapsec_name", sizeof(char)*MAX_LENGTH, leapsec_filename);
  } 

  *status = ANL_OK;
  
}

void
HXDleapsecInit_his(int *status){ *status = ANL_OK; }

void
HXDleapsecInit_bgnrun(int *status){
  *status = ANL_OK;
  
}

void
HXDleapsecInit_ana(int nevent, int eventid, int *status){
    
    *status = ANL_OK;
    return;
    
}

void
HXDleapsecInit_endrun(int *status){ *status = ANL_OK; }

void
HXDleapsecInit_exit(int *status){ *status = ANL_OK; }
