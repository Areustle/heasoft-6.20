/*
 *  HXDtableFitsInit
 * v0.1.0 2003-07-23 for HEADAS, by Y Terada, H Takahashi, M Suzuki
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
#include "HXD.h"

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"


#include "hxdtableFitsUtil.h"

#define FILELIST_MAX_LENGTH PIL_LINESIZE

char HXDtableFitsInit_version[] = "version 0.1.0";
static char pname[] = "HXDtableFitsInit";

static char table_fits_filelist[FILELIST_MAX_LENGTH];
static HxdTableFits table_fits;

void
HXDtableFitsInit_startup(int *status){ *status = ANL_OK; }

void
HXDtableFitsInit_com(int *status) {
    
  if ( *status ) { /* ftools */
    
    *status = 0;
    /*    
    UCLGST("table_list_name", table_fits_filelist, *status);
    */
    *status = PILGetFname("table_list_name", table_fits_filelist);

    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }
    
    *status = ANL_OK;
    return;
  }
  /* ANL */
  
  table_fits_filelist[0] = '\0';
  
  CLtxtrd("HXD table fits file list name",
	  table_fits_filelist, sizeof(table_fits_filelist) );
  
  *status = ANL_OK;
  
}

void
HXDtableFitsInit_init(int *status) {

  int istat = 0;
  
  BnkDef ("HXDtableFitsInit:TABLE_FITS", sizeof(HxdTableFits));
  
  hxdtableFits_Init( table_fits_filelist, &table_fits, &istat);
  
  if (istat){
    *status = ANL_QUIT;
    return;
  }
  
  BnkfPutM ("HXDtableFitsInit:TABLE_FITS",  sizeof(HxdTableFits),
	    &table_fits);
  
  *status = ANL_OK;
  
}

void
HXDtableFitsInit_his(int *status){ *status = ANL_OK; }

void
HXDtableFitsInit_bgnrun(int *status){

  *status = ANL_OK;
  
}

void
HXDtableFitsInit_ana(int nevent, int eventid, int *status){
    
    *status = ANL_OK;
    return;
    
}

void
HXDtableFitsInit_endrun(int *status){ *status = ANL_OK; }

void
HXDtableFitsInit_exit(int *status){ *status = ANL_OK; }
