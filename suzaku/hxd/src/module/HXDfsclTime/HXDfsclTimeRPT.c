/*
 *   HXDfsclTimeRPT v0.0.1 
 *                  v0.0.4 2000/02/03 Y.Fukazawa get WPU_CLOCK
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

char HXDfsclTimeRPT_version[] = "version 0.2.9";

static char pname[] = "HXDfsclTimeRPT";

void
HXDfsclTimeRPT_startup(int *status){    *status = ANL_OK;}

void
HXDfsclTimeRPT_com(int *status){    *status = ANL_OK;}

void
HXDfsclTimeRPT_init(int *status){

    BnkDef( "HXDfsclTime:HXD_SYS_LATCH_TI" , sizeof(int) );
    /* "HXD:SYS:TLTIME" */
    BnkDef( "HXDfsclTime:HXD_AE_TM_LATCH_TM", sizeof(int) );
    /* "HXD:HKA:LATCH_TIM" */
    
    BnkDef( "HXDfsclTime:HXD_SYS_TIME", sizeof(int) );
    BnkDef( "HXDfsclTime:HXD_HK_TIME", sizeof(int) );
    BnkDef( "HXDfsclTime:HXD_WPU_CLK_RATE", sizeof(int) );
    
    *status = ANL_OK;

}

void
HXDfsclTimeRPT_his(int *status){    *status = ANL_OK; }

void
HXDfsclTimeRPT_bgnrun(int *status){    *status = ANL_OK;       }

void
HXDfsclTimeRPT_ana(int nevent, int eventid, int *status){

    int size;
    
    int update;
    unsigned int data;
    double time;

    BnkfGetM("HXD:ALL:UPDATE", sizeof(int), &size, &update );
    
    if( update & HXD_UPDATE_SYS ){

        BnkfGetM ("HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size, &time);
        BnkfPutM ("HXDfsclTime:HXD_SYS_TIME", sizeof(int), &time);
		
	BnkfGetM ("HXD:SYS:TLTIME", sizeof(int), &size, &data);	
	BnkfPutM ("HXDfsclTime:HXD_SYS_LATCH_TI", sizeof(int), &data);
	
    }
    
    if( update & HXD_UPDATE_HK ){

        BnkfGetM ("HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size, &time);
        BnkfPutM ("HXDfsclTime:HXD_HK_TIME", sizeof(int), &time);

	BnkfGetM ("HXD:HKA:LATCH_TIM", sizeof(int), &size, &data);
	BnkfPutM ("HXDfsclTime:HXD_AE_TM_LATCH_TM", sizeof(int), &data);
	
	BnkfGetM ("HXD:HKA:WPU_CLK_RATE", sizeof(int), &size, &data);
	BnkfPutM ("HXDfsclTime:HXD_WPU_CLK_RATE", sizeof(int), &data);
	
    }
    
    *status = ANL_OK;
}

void
HXDfsclTimeRPT_endrun(int *status){        *status = ANL_OK; }

void
HXDfsclTimeRPT_exit(int *status){    *status = ANL_OK; }
/******************** EOF **********************************************/
