#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cli.h>
#include <com.h>
#include <bnk.h>
#include <evs.h>
#include <anl.h>
#include <atFunctions.h>
#include <aste_time.h>
#include <fitsio.h>
#include <cfortran.h>
#include <hbook.h>
#include "hxd/HXD.h"

#include "hxdgradeUtil.h"
#define DEBUG 0

char HXDtrngrade_version[] = "version 0.1.0";

static char pname[] = "HXDtrngrade";

void
HXDtrngrade_startup(int *status)
{
  int used = 1;
  BnkPut( "HXD:ftools:hxdwamgrade_yn",      sizeof(int), &used);
  *status = ANL_OK;
}

void
HXDtrngrade_com(int *status)
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
HXDtrngrade_init(int *status)
{
    *status = ANL_OK;
}

void
HXDtrngrade_his(int *status)
{
    *status = ANL_OK;
}

void
HXDtrngrade_bgnrun(int *status)
{
    *status = ANL_OK;       
}

void
HXDtrngrade_ana(int nevent, int eventid, int *status)
{

    int size;
    static int trn_quality;
    
    if (DEBUG) fprintf(stdout,"%s_ana Start\n",pname);

    hxdtrn_gradeUtil( &trn_quality );
    if (DEBUG) fprintf(stdout,"%s_ana hxdtrngrade done\n",pname);

    BnkfPutM( "HXD:TRN:TRN_QUALITY", sizeof(int), &trn_quality );
    
    *status = ANL_OK;
    if (DEBUG) fprintf(stdout,"%s_ana ANL OK\n",pname);
    return;
}

void
HXDtrngrade_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDtrngrade_exit(int *status)
{
    *status = ANL_OK;
}
