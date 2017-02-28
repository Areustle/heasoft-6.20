/*
 *   HXDmktrnspecFITS for FITS
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
#include "hxdmktrnspecUtil.h"

char HXDmktrnspecFITS_version[] = "version 0.2.1";

static char pname[] = "HXDmktrnspecFITS";

void
HXDmktrnspecFITS_startup(int *status)
{
    *status = ANL_OK;
}

void
HXDmktrnspecFITS_com(int *status)
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
HXDmktrnspecFITS_init(int *status)
{
    *status = ANL_OK;
}

void
HXDmktrnspecFITS_his(int *status)
{
    *status = ANL_OK;
}

void
HXDmktrnspecFITS_bgnrun(int *status)
{
    *status = ANL_OK;       
}

void
HXDmktrnspecFITS_ana(int nevent, int eventid, int *status)
{
    *status = ANL_OK;
}

void
HXDmktrnspecFITS_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDmktrnspecFITS_exit(int *status)
{
    *status = ANL_OK;
}


