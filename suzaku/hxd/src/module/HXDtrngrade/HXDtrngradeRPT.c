
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cli.h>
#include <com.h>
#include <bnk.h>
#include <evs.h>
#include <anl.h>
#include <fitsio.h>
#include "hxd/HXD.h"

char HXDtrngradeRPT_version[] = "version 0.0.3";

static char pname[] = "HXDtrngradeRPT";

void
HXDtrngradeRPT_startup(int *status)
{
    *status = ANL_OK;
}

void
HXDtrngradeRPT_com(int *status)
{
    *status = ANL_OK;
}

void
HXDtrngradeRPT_init(int *status)
{
    *status = ANL_OK;
}

void
HXDtrngradeRPT_his(int *status)
{
    *status = ANL_OK;
}

void
HXDtrngradeRPT_bgnrun(int *status)
{
    *status = ANL_OK;       
}

void
HXDtrngradeRPT_ana(int nevent, int eventid, int *status)
{
    *status = ANL_OK;
}

void
HXDtrngradeRPT_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDtrngradeRPT_exit(int *status)
{
    *status = ANL_OK;
}
