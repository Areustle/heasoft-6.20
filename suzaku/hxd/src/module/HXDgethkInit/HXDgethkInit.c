/*
 *   version 0.1.0 2003-07-23
 *   for HEADAS by Y Terada, H Takahashi, M Suzuki
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
/* #include "hbook.h" */
#include "hxd/HXD.h"

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"


#include "aste_gethk.h"

#define FILELIST_MAX_LENGTH PIL_LINESIZE

char HXDgethkInit_version[] = "version 0.1.0";
static char pname[] = "HXDgethkInit";

static char hk_filelist[FILELIST_MAX_LENGTH];
static ASTE_HK *aste_hk;

void
HXDgethkInit_startup(int *status){ *status = ANL_OK; }

void
HXDgethkInit_com(int *status) {
    
  if ( *status ) { /* ftools */
    
    *status = 0;
    /*    
    UCLGST("hklist_name", hk_filelist, *status);
    */
    *status = PILGetFname("hklist_name", hk_filelist);

    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }
    
    *status = ANL_OK;
    return;
  }
  /* ANL */
  
  hk_filelist[0] = '\0';
  
  CLtxtrd("HXD HK fits file list name",
	  hk_filelist, sizeof(hk_filelist) );
  
  *status = ANL_OK;
  
}

void
HXDgethkInit_init(int *status) {

  BnkDef ("HXDgethkInit:ASTE_HK", sizeof(ASTE_HK *));
  
  aste_hk = aste_gethk_init ( hk_filelist );
  
  BnkfPutM ("HXDgethkInit:ASTE_HK",  sizeof(ASTE_HK *), &aste_hk);
  
  *status = ANL_OK;
  
}

void
HXDgethkInit_his(int *status){ *status = ANL_OK; }

void
HXDgethkInit_bgnrun(int *status){

  *status = ANL_OK;
  
}

void
HXDgethkInit_ana(int nevent, int eventid, int *status){
    
    *status = ANL_OK;
    return;
    
}

void
HXDgethkInit_endrun(int *status){ *status = ANL_OK; }

void
HXDgethkInit_exit(int *status){ *status = ANL_OK; }
