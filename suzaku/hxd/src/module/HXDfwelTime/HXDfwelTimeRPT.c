/*
 *   HXDfwelTimeRPT v0.0.1 test version for ftools
 *                  v0.0.2 first fix version (this module need translator)
 *                  v0.1.6 read and put PWH
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

char HXDfwelTimeRPT_version[] = "version 0.3.8";

static char pname[] = "HXDfwelTimeRPT";
static int hxdfweltime_use_pwh;

void
HXDfwelTimeRPT_startup(int *status)
{
    *status = ANL_OK;
}

void
HXDfwelTimeRPT_com(int *status)
{
    *status = ANL_OK;
}

void
HXDfwelTimeRPT_init(int *status)
{

    BnkDef( "HXDfwelTime:HXD_SYS_LATCH_TI" , sizeof(int) );
    /* "HXD:SYS:TLTIME" */
    BnkDef( "HXDfwelTime:HXD_AE_TM_LATCH_TM", sizeof(int) );
    /* "HXD:HKA:LATCH_TIM" */
    BnkDef( "HXDfwelTime:HXD_WPU_CLK_RATE", sizeof(int) );
    /* "HXD:HKA:WPU_CLK_RATE" */

    BnkDef( "HXDfwelTime:HXD_SYS_TIME", sizeof(int) );
    BnkDef( "HXDfwelTime:HXD_HK_TIME", sizeof(int) );

    BnkDef( "HXDfwelTime:PWH", sizeof(int)*HXD_PWH_DATANUM);
    
    *status = ANL_OK;

}

void
HXDfwelTimeRPT_his(int *status)
{
    *status = ANL_OK;
}

void
HXDfwelTimeRPT_bgnrun(int *status)
{
  BnkfGetM("HXDeventFitsRead:USE_PWH", sizeof(int),   
	   &size, &hxdfweltime_use_pwh);
  
    *status = ANL_OK;       
}

void
HXDfwelTimeRPT_ana(int nevent, int eventid, int *status)
{

    int size;
    
    int update;
    unsigned int data;
    double time;
    int pwh[HXD_PWH_DATANUM];

    BnkfGetM("HXD:ALL:UPDATE", sizeof(int), &size, &update );
    
    if( update & HXD_UPDATE_SYS ){

        BnkfGetM ("HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size, &time);
        BnkfPutM ("HXDfwelTime:HXD_SYS_TIME", sizeof(int), &time);

	BnkfGetM ("HXD:SYS:TLTIME", sizeof(int), &size, &data);	
	BnkfPutM ("HXDfwelTime:HXD_SYS_LATCH_TI", sizeof(int), &data);
	
    }
    
    if( update & HXD_UPDATE_HK ){

        BnkfGetM ("HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size, &time);
        BnkfPutM ("HXDfwelTime:HXD_HK_TIME", sizeof(int), &time);

	BnkfGetM ("HXD:HKA:LATCH_TIM", sizeof(int), &size, &data);
	BnkfPutM ("HXDfwelTime:HXD_AE_TM_LATCH_TM", sizeof(int), &data);
	
	BnkfGetM ("HXD:HKA:WPU_CLK_RATE", sizeof(int), &size, &data);
	BnkfPutM ("HXDfwelTime:HXD_WPU_CLK_RATE", sizeof(int), &data);
    }
    
    /*** USE PWH or not ***/
    if (hxdfweltime_use_pwh == TRUE) {  
      *status = ANL_OK;
      return;
    } 
    if( update & HXD_UPDATE_WEL ){
	BnkfGetM ("HXD:WEL:PWH", sizeof(int)*HXD_PWH_DATANUM, &size, &pwh);
	BnkfPutM ("HXDfwelTime:PWH", sizeof(int)*HXD_PWH_DATANUM, &pwh);
    }

    *status = ANL_OK;
}

void
HXDfwelTimeRPT_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDfwelTimeRPT_exit(int *status)
{
    *status = ANL_OK;
}
