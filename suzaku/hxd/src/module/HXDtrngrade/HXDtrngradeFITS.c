
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cli.h>
#include <com.h>
#include <bnk.h>
#include <evs.h>
#include <anl.h>
#include <fitsio.h>
#include <cfortran.h>
#include "hxd/HXD.h"

char HXDtrngradeFITS_version[] = "version 0.0.3";

static char pname[] = "HXDtrngradeFITS";

void
HXDtrngradeFITS_startup(int *status)
{
    *status = ANL_OK;
}

void
HXDtrngradeFITS_com(int *status)
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
HXDtrngradeFITS_init(int *status)
{
    *status = ANL_OK;
}

void
HXDtrngradeFITS_his(int *status)
{
    *status = ANL_OK;
}

void
HXDtrngradeFITS_bgnrun(int *status)
{
    *status = ANL_OK;       
}

void
HXDtrngradeFITS_ana(int nevent, int eventid, int *status)
{
    *status = ANL_OK;
}

void
HXDtrngradeFITS_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDtrngradeFITS_exit(int *status)
{
    *status = ANL_OK;
}
