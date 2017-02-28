/*
 *   HXDtransRPTtoeventFITS v0.0.1 test version for ftools
 *                          v0.0.2 no need HXDread We0,We1,We2,We3
 *                          v0.0.3 for HXDeventExtract 2.0.9
 *                          v0.0.4 "HXD:ALL:APDATE" & HXD_UPDATE_WEL = 0
 *                                 if( HK & SYS first packet not come )
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

char HXDtransRPTtoeventFITS_version[] = "version 0.0.5";

static char pname[] = "HXDtransRPTtoeventFITS";

static int pre_event_flag;
static int HK_Packet_First_Flag = 0;
static int SYS_Packet_First_Flag = 0;

void
HXDtransRPTtoeventFITS_startup(int *status)
{
    *status = ANL_OK;
}

void
HXDtransRPTtoeventFITS_com(int *status)
{
    *status = ANL_OK;
}

void
HXDtransRPTtoeventFITS_init(int *status)
{
    
    BnkDef( "HXD:ALL:UPDATE" ,   sizeof(int));
    
    BnkDef( "HXD:WEL:UNITID", sizeof(int) );
    BnkDef( "HXD:WEL:LENGTH_CHK", sizeof(int) );
    BnkDef( "HXD:WEL:WELTIME", sizeof(int) );
    BnkDef( "HXD:WEL:QUALITY_FLAGS", sizeof(int) );
    BnkDef( "HXD:WEL:TRIG", sizeof(int) );
    BnkDef( "HXD:WEL:HIT_PATTERN_WELL", sizeof(int) );
    BnkDef( "HXD:WEL:HIT_PATTERN_ANTI", sizeof(int) );
    BnkDef( "HXD:WEL:PHA_FAST", sizeof(int) );
    BnkDef( "HXD:WEL:PHA_SLOW", sizeof(int) );
    BnkDef( "HXD:WEL:PHA_PIN", sizeof(int)*4 ); 
  
    BnkDef( "HXD:TRB:EV_TIME", sizeof(int) );
    BnkDef( "HXD:TRB:PI", sizeof(int)*54 );
    
    BnkDef( "HXD:AET_SC:PACKET_AETIME", sizeof(double) );
    BnkDef( "HXD:AET_HC:PACKET_AETIME", sizeof(double) );
    BnkDef( "HXD:PPR:PACKET_AETIME", sizeof(double) );
    BnkDef( "HXD:PST:PACKET_AETIME", sizeof(double) );
    BnkDef( "HXD:STM:PACKET_AETIME", sizeof(double) );
    BnkDef( "HXD:WEL:PACKET_AETIME", sizeof(double) );
    
    BnkDef( "HXD:TRN:PACKET_SEC_HEADER", sizeof(int) );
    BnkDef( "HXD:BST:PACKET_SEC_HEADER", sizeof(int) );
    BnkDef( "HXD:WEL:PACKET_SEC_HEADER",  sizeof(int) );

    pre_event_flag = 1;
    
    *status = ANL_OK;
}

void
HXDtransRPTtoeventFITS_his(int *status)
{
    *status = ANL_OK;
}

void
HXDtransRPTtoeventFITS_bgnrun(int *status)
{    
    *status = ANL_OK;
}


static void HXDtransRPTtoeventFITS_WE0(){
    
    int size;
    
    char data[16];

    double time;
    unsigned int sec_header;

    int unitid;
    int length_chk;
    int weltime;
    int flag;
    int trig;
    int hit_pat_well;
    int hit_pat_anti;
    int pha_fast;
    int pha_slow;
    int pin_ph[4];
    
    int update;

    BnkfGetM("HXD:WE0:PACKET_AETIME", sizeof(double), &size, &time);
    BnkfPutM("HXD:WEL:PACKET_AETIME", sizeof(double), &time);
    
    BnkfGetM("HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size,
	     &sec_header);
    BnkfPutM("HXD:WEL:PACKET_SEC_HEADER", sizeof(int), &sec_header);
    
    BnkGet( "HXD:WE0:ALL", sizeof(char)*16, &size, data );
    
    unitid = 0 + ((data[3] & 0xC0) >> 6);    
    length_chk = ((data[0] &0x80) >> 7);
    weltime = ((data[0] & 0x7F )<<12) + ((data[0+1] & 0xFF )<< 4)
        + ((data[0+2] & 0xF0 )>> 4);
    flag = ((data[2] & 0x07) << 4) + ((data[4] & 0xF0) >> 4);
    trig = ((data[2] & 0x08) << 3) + (data[3] & 0x3F);
    hit_pat_well = ((data[4] & 0x0F) << 12) + ((data[5] & 0xFF) << 4)
        + ((data[6] & 0xF0) >> 4);
    hit_pat_anti = ((data[6] & 0x0F) << 16) + ((data[7] & 0xFF) << 8)
        + (data[8] & 0xFF);    
    pha_fast = ((data[9] & 0xFF) << 4) + ((data[10] & 0xF0) >> 4);
    pha_slow = ((data[10] & 0x0F) << 8) + (data[10+1] & 0xFF);
    pin_ph[0] = (data[12] & 0xFF);
    pin_ph[1] = (data[13] & 0xFF);
    pin_ph[2] = (data[14] & 0xFF);
    pin_ph[3] = (data[15] & 0xFF);
    
    BnkfPutM( "HXD:WEL:UNITID", sizeof(int), &unitid );
    BnkfPutM( "HXD:WEL:LENGTH_CHK", sizeof(int), &length_chk );
    BnkfPutM( "HXD:WEL:WELTIME", sizeof(int), &weltime );
    BnkfPutM( "HXD:WEL:QUALITY_FLAGS", sizeof(int), &flag );
    BnkfPutM( "HXD:WEL:TRIG", sizeof(int), &trig );
    BnkfPutM( "HXD:WEL:HIT_PATTERN_WELL", sizeof(int), &hit_pat_well );
    BnkfPutM( "HXD:WEL:HIT_PATTERN_ANTI", sizeof(int), &hit_pat_anti );
    BnkfPutM( "HXD:WEL:PHA_FAST", sizeof(int), &pha_fast );
    BnkfPutM( "HXD:WEL:PHA_SLOW", sizeof(int), &pha_slow );
    BnkfPutM( "HXD:WEL:PHA_PIN", sizeof(int)*4, pin_ph );

    if( HK_Packet_First_Flag && SYS_Packet_First_Flag ){
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_WEL;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
    }
    
}

static void HXDtransRPTtoeventFITS_WE1(){
    
    int size;
    
    char data[16];

    double time;
    unsigned int sec_header;

    int unitid;
    int length_chk;
    int weltime;
    int flag;
    int trig;
    int hit_pat_well;
    int hit_pat_anti;
    int pha_fast;
    int pha_slow;
    int pin_ph[4];
    
    int update;

    BnkfGetM("HXD:WE1:PACKET_AETIME", sizeof(double), &size, &time);
    BnkfPutM("HXD:WEL:PACKET_AETIME", sizeof(double), &time);
    
    BnkfGetM("HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size,
	     &sec_header);
    BnkfPutM("HXD:WEL:PACKET_SEC_HEADER", sizeof(int), &sec_header);
    
    BnkGet( "HXD:WE1:ALL", sizeof(char)*16, &size, data );
    
    unitid = 4 + ((data[3] & 0xC0) >> 6);    
    length_chk = ((data[0] &0x80) >> 7);
    weltime = ((data[0] & 0x7F )<<12) + ((data[0+1] & 0xFF )<< 4)
        + ((data[0+2] & 0xF0 )>> 4);
    flag = ((data[2] & 0x07) << 4) + ((data[4] & 0xF0) >> 4);
    trig = ((data[2] & 0x08) << 3) + (data[3] & 0x3F);
    hit_pat_well = ((data[4] & 0x0F) << 12) + ((data[5] & 0xFF) << 4)
        + ((data[6] & 0xF0) >> 4);
    hit_pat_anti = ((data[6] & 0x0F) << 16) + ((data[7] & 0xFF) << 8)
        + (data[8] & 0xFF);    
    pha_fast = ((data[9] & 0xFF) << 4) + ((data[10] & 0xF0) >> 4);
    pha_slow = ((data[10] & 0x0F) << 8) + (data[10+1] & 0xFF);
    pin_ph[0] = (data[12] & 0xFF);
    pin_ph[1] = (data[13] & 0xFF);
    pin_ph[2] = (data[14] & 0xFF);
    pin_ph[3] = (data[15] & 0xFF);
    
    BnkfPutM( "HXD:WEL:UNITID", sizeof(int), &unitid );
    BnkfPutM( "HXD:WEL:LENGTH_CHK", sizeof(int), &length_chk );
    BnkfPutM( "HXD:WEL:WELTIME", sizeof(int), &weltime );
    BnkfPutM( "HXD:WEL:QUALITY_FLAGS", sizeof(int), &flag );
    BnkfPutM( "HXD:WEL:TRIG", sizeof(int), &trig );
    BnkfPutM( "HXD:WEL:HIT_PATTERN_WELL", sizeof(int), &hit_pat_well );
    BnkfPutM( "HXD:WEL:HIT_PATTERN_ANTI", sizeof(int), &hit_pat_anti );
    BnkfPutM( "HXD:WEL:PHA_FAST", sizeof(int), &pha_fast );
    BnkfPutM( "HXD:WEL:PHA_SLOW", sizeof(int), &pha_slow );
    BnkfPutM( "HXD:WEL:PHA_PIN", sizeof(int)*4, pin_ph );

    if( HK_Packet_First_Flag && SYS_Packet_First_Flag ){
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_WEL;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
    }
    
}

static void HXDtransRPTtoeventFITS_WE2(){
    
    int size;
    
    char data[16];

    double time;
    unsigned int sec_header;

    int unitid;
    int length_chk;
    int weltime;
    int flag;
    int trig;
    int hit_pat_well;
    int hit_pat_anti;
    int pha_fast;
    int pha_slow;
    int pin_ph[4];
    
    int update;

    BnkfGetM("HXD:WE2:PACKET_AETIME", sizeof(double), &size, &time);
    BnkfPutM("HXD:WEL:PACKET_AETIME", sizeof(double), &time);
    
    BnkfGetM("HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size,
	     &sec_header);
    BnkfPutM("HXD:WEL:PACKET_SEC_HEADER", sizeof(int), &sec_header);
    
    BnkGet( "HXD:WE2:ALL", sizeof(char)*16, &size, data );
    
    unitid = 8 + ((data[3] & 0xC0) >> 6);    
    length_chk = ((data[0] &0x80) >> 7);
    weltime = ((data[0] & 0x7F )<<12) + ((data[0+1] & 0xFF )<< 4)
        + ((data[0+2] & 0xF0 )>> 4);
    flag = ((data[2] & 0x07) << 4) + ((data[4] & 0xF0) >> 4);
    trig = ((data[2] & 0x08) << 3) + (data[3] & 0x3F);
    hit_pat_well = ((data[4] & 0x0F) << 12) + ((data[5] & 0xFF) << 4)
        + ((data[6] & 0xF0) >> 4);
    hit_pat_anti = ((data[6] & 0x0F) << 16) + ((data[7] & 0xFF) << 8)
        + (data[8] & 0xFF);    
    pha_fast = ((data[9] & 0xFF) << 4) + ((data[10] & 0xF0) >> 4);
    pha_slow = ((data[10] & 0x0F) << 8) + (data[10+1] & 0xFF);
    pin_ph[0] = (data[12] & 0xFF);
    pin_ph[1] = (data[13] & 0xFF);
    pin_ph[2] = (data[14] & 0xFF);
    pin_ph[3] = (data[15] & 0xFF);
    
    BnkfPutM( "HXD:WEL:UNITID", sizeof(int), &unitid );
    BnkfPutM( "HXD:WEL:LENGTH_CHK", sizeof(int), &length_chk );
    BnkfPutM( "HXD:WEL:WELTIME", sizeof(int), &weltime );
    BnkfPutM( "HXD:WEL:QUALITY_FLAGS", sizeof(int), &flag );
    BnkfPutM( "HXD:WEL:TRIG", sizeof(int), &trig );
    BnkfPutM( "HXD:WEL:HIT_PATTERN_WELL", sizeof(int), &hit_pat_well );
    BnkfPutM( "HXD:WEL:HIT_PATTERN_ANTI", sizeof(int), &hit_pat_anti );
    BnkfPutM( "HXD:WEL:PHA_FAST", sizeof(int), &pha_fast );
    BnkfPutM( "HXD:WEL:PHA_SLOW", sizeof(int), &pha_slow );
    BnkfPutM( "HXD:WEL:PHA_PIN", sizeof(int)*4, pin_ph );
    
    if( HK_Packet_First_Flag && SYS_Packet_First_Flag ){	
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_WEL;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
    }
    
}

static void HXDtransRPTtoeventFITS_WE3(){
    
    int size;
    
    char data[16];

    double time;
    unsigned int sec_header;

    int unitid;
    int length_chk;
    int weltime;
    int flag;
    int trig;
    int hit_pat_well;
    int hit_pat_anti;
    int pha_fast;
    int pha_slow;
    int pin_ph[4];
    
    int update;

    BnkfGetM("HXD:WE3:PACKET_AETIME", sizeof(double), &size, &time);
    BnkfPutM("HXD:WEL:PACKET_AETIME", sizeof(double), &time);
    
    BnkfGetM("HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size,
	     &sec_header);
    BnkfPutM("HXD:WEL:PACKET_SEC_HEADER", sizeof(int), &sec_header);
    
    BnkGet( "HXD:WE3:ALL", sizeof(char)*16, &size, data );
    
    unitid = 12 + ((data[3] & 0xC0) >> 6);    
    length_chk = ((data[0] &0x80) >> 7);
    weltime = ((data[0] & 0x7F )<<12) + ((data[0+1] & 0xFF )<< 4)
        + ((data[0+2] & 0xF0 )>> 4);
    flag = ((data[2] & 0x07) << 4) + ((data[4] & 0xF0) >> 4);
    trig = ((data[2] & 0x08) << 3) + (data[3] & 0x3F);
    hit_pat_well = ((data[4] & 0x0F) << 12) + ((data[5] & 0xFF) << 4)
        + ((data[6] & 0xF0) >> 4);
    hit_pat_anti = ((data[6] & 0x0F) << 16) + ((data[7] & 0xFF) << 8)
        + (data[8] & 0xFF);    
    pha_fast = ((data[9] & 0xFF) << 4) + ((data[10] & 0xF0) >> 4);
    pha_slow = ((data[10] & 0x0F) << 8) + (data[10+1] & 0xFF);
    pin_ph[0] = (data[12] & 0xFF);
    pin_ph[1] = (data[13] & 0xFF);
    pin_ph[2] = (data[14] & 0xFF);
    pin_ph[3] = (data[15] & 0xFF);
    
    BnkfPutM( "HXD:WEL:UNITID", sizeof(int), &unitid );
    BnkfPutM( "HXD:WEL:LENGTH_CHK", sizeof(int), &length_chk );
    BnkfPutM( "HXD:WEL:WELTIME", sizeof(int), &weltime );
    BnkfPutM( "HXD:WEL:QUALITY_FLAGS", sizeof(int), &flag );
    BnkfPutM( "HXD:WEL:TRIG", sizeof(int), &trig );
    BnkfPutM( "HXD:WEL:HIT_PATTERN_WELL", sizeof(int), &hit_pat_well );
    BnkfPutM( "HXD:WEL:HIT_PATTERN_ANTI", sizeof(int), &hit_pat_anti );
    BnkfPutM( "HXD:WEL:PHA_FAST", sizeof(int), &pha_fast );
    BnkfPutM( "HXD:WEL:PHA_SLOW", sizeof(int), &pha_slow );
    BnkfPutM( "HXD:WEL:PHA_PIN", sizeof(int)*4, pin_ph );

    if( HK_Packet_First_Flag && SYS_Packet_First_Flag ){
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_WEL;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
    }
    
}


void
HXDtransRPTtoeventFITS_ana(int *nevent, int *eventid, int *status)
{
    
    int size;
    int evtyp;
    int update;
    
    double aetime;
    unsigned int sec_header;
    
    if ( pre_event_flag ){
	update = 0;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	pre_event_flag = 0;
    }
    
    BnkfGetM( "HXD:ALL:EVENTTYPE", sizeof(int), &size, &evtyp );
    switch ( evtyp ) {
	
    case HXD_EVTYP_WE0:
        HXDtransRPTtoeventFITS_WE0();
	pre_event_flag = 1;
	*status = ANL_OK;
	return;
    case HXD_EVTYP_WE1:
	HXDtransRPTtoeventFITS_WE1();
	pre_event_flag = 1;
	*status = ANL_OK;
	return;
    case HXD_EVTYP_WE2:
	HXDtransRPTtoeventFITS_WE2();
	pre_event_flag = 1;
	*status = ANL_OK;
	return;
    case HXD_EVTYP_WE3:
	HXDtransRPTtoeventFITS_WE3();
	pre_event_flag = 1;
	*status = ANL_OK;
	return;
    case HXD_EVTYP_TRB:
	BnkfGetM( "HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size,
		 &sec_header );
	BnkfPutM( "HXD:TRN:PACKET_SEC_HEADER", sizeof(int), &sec_header );
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_TRN;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;
	return;
    case HXD_EVTYP_BST:
	BnkfGetM( "HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size,
		 &sec_header );
	BnkfPutM( "HXD:BST:PACKET_SEC_HEADER", sizeof(int), &sec_header );
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_BST;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );	
	*status = ANL_SKIP;
	return;
    case HXD_EVTYP_STM:
	BnkfGetM( "HXD:PPS:PACKET_AETIME", sizeof(double), &size, &aetime);
	BnkfPutM( "HXD:STM:PACKET_AETIME", sizeof(double), &aetime);	
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_STM;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_PST:
	BnkfGetM( "HXD:PPS:PACKET_AETIME", sizeof(double), &size, &aetime);
	BnkfPutM( "HXD:PST:PACKET_AETIME", sizeof(double), &aetime);	
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_PST;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;
	return;
    case HXD_EVTYP_PPR:
	BnkfGetM( "HXD:PPS:PACKET_AETIME", sizeof(double), &size, &aetime);
	BnkfPutM( "HXD:PPR:PACKET_AETIME", sizeof(double), &aetime);	
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_PPR;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_AET_HC:
	BnkfGetM( "HXD:AET:PACKET_AETIME", sizeof(double), &size, &aetime);
	BnkfPutM( "HXD:AET_HC:PACKET_AETIME", sizeof(double), &aetime);	
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_AET_HC;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_AET_SC:
	BnkfGetM( "HXD:AET:PACKET_AETIME", sizeof(double), &size, &aetime);
	BnkfPutM( "HXD:AET_SC:PACKET_AETIME", sizeof(double), &aetime);	
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_AET_SC;	
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_HK:
	if(!HK_Packet_First_Flag){
            HK_Packet_First_Flag = 1;
        }
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_HK;	
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
       	return;
    case HXD_EVTYP_SYS:
	if(!SYS_Packet_First_Flag){
            SYS_Packet_First_Flag = 1;
        }
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_SYS;	
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_SCL:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_SCL;	
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_ACU:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_ACU;	
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_RHK:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_RHK;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_SFC:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_SFC;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_SFF1:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_SFF1;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_SFF2:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_SFF2;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_DLT:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_DLT;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_SP_PIN:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_SP_PIN;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_SP_PMT:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_SP_PMT;	
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_MEM_DMP:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_MEM_DMP;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_ECC_DMP:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_ECC_DMP;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_IO_DMP:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_IO_DMP;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_RECC_DMP:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_RECC_DMP;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    case HXD_EVTYP_RIO_DMP:
	BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
	update |= HXD_UPDATE_RIO_DMP;
	BnkfPutM( "HXD:ALL:UPDATE", sizeof(int), &update );
	*status = ANL_SKIP;	
	return;
    default:
	*status = ANL_SKIP;
	return;
    }    
    
}

void
HXDtransRPTtoeventFITS_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDtransRPTtoeventFITS_exit(int *status)
{
    *status = ANL_OK;
}
