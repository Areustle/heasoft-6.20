
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "fitsio.h"
#include "hxd/HXD.h"

char HXDgradeRPT_version[] = "version 0.0.3";

static char pname[] = "HXDgradeRPT";

void
HXDgradeRPT_startup(int *status)
{
    *status = ANL_OK;
}

void
HXDgradeRPT_com(int *status)
{
    *status = ANL_OK;
}

void
HXDgradeRPT_init(int *status)
{
    *status = ANL_OK;
}

void
HXDgradeRPT_his(int *status)
{
    *status = ANL_OK;
}

void
HXDgradeRPT_bgnrun(int *status)
{
    *status = ANL_OK;
}

void
HXDgradeRPT_ana(int nevent, int eventid, int *status)
{
    *status = ANL_OK;
}

void
HXDgradeRPT_endrun(int *status)
{
    *status = ANL_OK;
}

void
HXDgradeRPT_exit(int *status)
{
    *status = ANL_OK;
}
