/*
 *   HXDftrnTimeRPT v0.0.1 
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
#include "hxd/HXD.h"

char HXDftrnTimeRPT_version[] = "version 0.2.6";

static char pname[] = "HXDftrnTimeRPT";

void
HXDftrnTimeRPT_startup(int *status)
{
    *status = ANL_OK;
}

void
HXDftrnTimeRPT_com(int *status)
{
    *status = ANL_OK;
}

void
HXDftrnTimeRPT_init(int *status)
{

    BnkDef( "HXDftrnTime:HXD_SYS_LATCH_TI" , sizeof(int) );
    /* "HXD:SYS:TLTIME" */
    BnkDef( "HXDftrnTime:HXD_AE_TM_LATCH_TM", sizeof(int) );
    /* "HXD:HKA:LATCH_TIM" */
    
    BnkDef( "HXDftrnTime:HXD_SYS_TIME", sizeof(int) );
    BnkDef( "HXDftrnTime:HXD_HK_TIME", sizeof(int) );
    
    *status = ANL_OK;

}

void
HXDftrnTimeRPT_his(int *status)
{
    *status = ANL_OK;
}

void
HXDftrnTimeRPT_bgnrun(int *status)
{
    *status = ANL_OK;       
}

void
HXDftrnTimeRPT_ana(int nevent, int eventid, int *status)
{

    int size;
    
    int update;
    unsigned int data;
    double time;

    BnkfGetM("HXD:ALL:UPDATE", sizeof(int), &size, &update );
    
    if( update & HXD_UPDATE_SYS ){

        BnkfGetM ("HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size, &time);
        BnkfPutM ("HXDftrnTime:HXD_SYS_TIME", sizeof(int), &time);

	BnkfGetM ("HXD:SYS:TLTIME", sizeof(int), &size, &data);	
	BnkfPutM ("HXDftrnTime:HXD_SYS_LATCH_TI", sizeof(int), &data);
	
    }
    
    if( update & HXD_UPDATE_HK ){

        BnkfGetM ("HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size, &time);
        BnkfPutM ("HXDftrnTime:HXD_HK_TIME", sizeof(int), &time);

	BnkfGetM ("HXD:HKA:LATCH_TIM", sizeof(int), &size, &data);
	BnkfPutM ("HXDftrnTime:HXD_AE_TM_LATCH_TM", sizeof(int), &data);
	
    }
    
    *status = ANL_OK;
}

void
HXDftrnTimeRPT_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDftrnTimeRPT_exit(int *status)
{
    *status = ANL_OK;
}
