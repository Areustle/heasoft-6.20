/*
 *   HXDtrnpiFITS for FITS
 *         
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
#include "HXD.h"

#include "hxdtableFitsUtil.h"
#include "hxdtrnpiUtil.h"

char HXDtrnpiFITS_version[] = "version 2.0.0";

static char pname[] = "HXDtrnpiFITS";

void
HXDtrnpiFITS_startup(int *status)
{
    *status = ANL_OK;
}

void
HXDtrnpiFITS_com(int *status)
{
        
    if ( *status ) { /* ftools */
        
        *status = 0;
	
	if ( *status ) {
            *status = ANL_QUIT;
            exit(-1);
        }
	
        *status = ANL_OK;
        return;
    }
    
    *status = ANL_OK;
}

void
HXDtrnpiFITS_init(int *status)
{
    *status = ANL_OK;
}

void
HXDtrnpiFITS_his(int *status)
{
    *status = ANL_OK;
}

void
HXDtrnpiFITS_bgnrun(int *status)
{
    *status = ANL_OK;       
}

void
HXDtrnpiFITS_ana(int nevent, int eventid, int *status)
{
    *status = ANL_OK;
}

void
HXDtrnpiFITS_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDtrnpiFITS_exit(int *status)
{
    *status = ANL_OK;
}


