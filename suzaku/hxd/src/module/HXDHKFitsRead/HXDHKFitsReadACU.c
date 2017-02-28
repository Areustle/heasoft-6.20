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

enum {
  HK=2,SYS,ACU,SCL,PWH,RHK
};
enum {
  STM=2,PPR,PST,AET_HC,AET_SC
};
enum {
  MEM_DMP=2,ECC_DMP,IO_DMP,RECC_DMP,RIO_DMP
};
enum {
  SFC=2,SFF1,SFF2,DLT,SP_PMT,SP_PIN
};

enum {
  HXD_ACU_LNGTH_OK,
  HXD_ACU_PI_SEQ_NO,
  HXD_ACU_TLATCH_TIME,
  HXD_ACU_PSU_VOLT,
  HXD_ACU_PROC_TIME,
  HXD_ACU_WPU_ON_OF1,
  HXD_ACU_WPU_ON_OF0,
  HXD_ACU_WPU_ON_OF3,
  HXD_ACU_WPU_ON_OF2,
  HXD_ACU_TPU_ON_OF1,
  HXD_ACU_TPU_ON_OF0,
  HXD_ACU_TPU_ON_OF3,
  HXD_ACU_TPU_ON_OF2,
  HXD_ACU_DATA_ID,
  HXD_ACU_HV_ENA_DIS,
  HXD_ACU_HVW_ON_OF1,
  HXD_ACU_HVW_ON_OF0,
  HXD_ACU_HVW_ON_OF3,
  HXD_ACU_HVW_ON_OF2,
  HXD_ACU_HVT_ON_OF1,
  HXD_ACU_HVT_ON_OF0,
  HXD_ACU_HVT_ON_OF3,
  HXD_ACU_HVT_ON_OF2,
  HXD_ACU_HVP_ON_OF1,
  HXD_ACU_HVP_ON_OF0,
  HXD_ACU_HVP_ON_OF3,
  HXD_ACU_HVP_ON_OF2,
  HXD_ACU_WPU_CLK_RATE,
  HXD_ACU_HVREF_W0,
  HXD_ACU_HVREF_W1,
  HXD_ACU_HVREF_W2,
  HXD_ACU_HVREF_W3,
  HXD_ACU_HVREF_T0,
  HXD_ACU_HVREF_T1,
  HXD_ACU_HVREF_T2,
  HXD_ACU_HVREF_T3,
  HXD_ACU_HVREF_P0,
  HXD_ACU_HVREF_P1,
  HXD_ACU_HVREF_P2,
  HXD_ACU_HVREF_P3,
  HXD_ACU_HVREF_DATA,
  HXD_ACU_TEMP0,
  HXD_ACU_TEMP1,
  HXD_ACU_TEMP2,
  HXD_ACU_TEMP3,
  HXD_ACU_TEMP4,
  HXD_ACU_TEMP5,
  HXD_ACU_TEMP6,
  HXD_ACU_TEMP7,
  HXD_ACU_TEMP8,
  HXD_ACU_TEMP9,
  HXD_ACU_TEMPA,
  HXD_ACU_TEMPB,
  HXD_ACU_TEMPC,
  HXD_ACU_TEMPD,
  HXD_ACU_TEMPE,
  HXD_ACU_TEMPF,
  HXD_ACU_HV_W0,
  HXD_ACU_HV_W1,
  HXD_ACU_HV_W2,
  HXD_ACU_HV_W3,
  HXD_ACU_HV_T0,
  HXD_ACU_HV_T1,
  HXD_ACU_HV_T2,
  HXD_ACU_HV_T3,
  HXD_ACU_HV_P0,
  HXD_ACU_HV_P1,
  HXD_ACU_HV_P2,
  HXD_ACU_HV_P3,
};

static char pname[] = "HXDHKFitsReadACU";

static int colnum[69];
static int time_colnum;

void
HXDHKFitsReadACU_init()
{
  BnkDef( "HXD:ACU:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:ACU:LNGTH_OK", sizeof(int) );
  BnkDef( "HXD:ACU:PI_SEQ_NO", sizeof(int) );
  BnkDef( "HXD:ACU:TIME_LATCH_TIME", sizeof(int) );
  BnkDef( "HXD:ACU:PSU_VOLT", sizeof(int) );
  BnkDef( "HXD:ACU:PROC_TIME", sizeof(int) );
  BnkDef( "HXD:ACU:WPU_ON/OF", sizeof(int)*4 );
  BnkDef( "HXD:ACU:TPU_ON/OF", sizeof(int)*4 );
  BnkDef( "HXD:ACU:DATA_ID", sizeof(int) );
  BnkDef( "HXD:ACU:HV_ENA/DIS", sizeof(int) );
  BnkDef( "HXD:ACU:HVW_ON/OF", sizeof(int)*4 );
  BnkDef( "HXD:ACU:HVT_ON/OF", sizeof(int)*4 );
  BnkDef( "HXD:ACU:HVP_ON/OF", sizeof(int)*4 );
  BnkDef( "HXD:ACU:WPU_CLK_RATE", sizeof(int) );
  BnkDef( "HXD:ACU:HVREF_W", sizeof(int)*4 );
  BnkDef( "HXD:ACU:HVREF_T", sizeof(int)*4 );
  BnkDef( "HXD:ACU:HVREF_P", sizeof(int)*4 );
  BnkDef( "HXD:ACU:HVREF_DATA", sizeof(int) );
  BnkDef( "HXD:ACU:TEMP", sizeof(int)*16 );
  BnkDef( "HXD:ACU:HV_W", sizeof(int)*4 );
  BnkDef( "HXD:ACU:HV_T", sizeof(int)*4 );
  BnkDef( "HXD:ACU:HV_P", sizeof(int)*4 );
  
}


int
HXDHKFitsReadACU_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, ACU, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, istat);
    return istat;
  } else {
    if( fits_get_colnum(fp, casesen, "TIME", &time_colnum, &istat) ){
      fprintf(stderr, "%s: fits_get_colnum('TIME') failed (%d)\n",
	      pname, istat);
      return istat;
    }
  }
  
  if( fits_get_colnum(fp, casesen, "HXD_ACU_LNGTH_OK",
                      &colnum[HXD_ACU_LNGTH_OK], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_LNGTH_OK') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_PI_SEQ_NO",
                      &colnum[HXD_ACU_PI_SEQ_NO], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_PI_SEQ_NO') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TLATCH_TIME",
                      &colnum[HXD_ACU_TLATCH_TIME], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TLATCH_TIME') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_PSU_VOLT",
                      &colnum[HXD_ACU_PSU_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_PSU_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_PROC_TIME",
                      &colnum[HXD_ACU_PROC_TIME], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_PROC_TIME') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_WPU_ON_OF1",
                      &colnum[HXD_ACU_WPU_ON_OF1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_WPU_ON_OF1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_WPU_ON_OF0",
                      &colnum[HXD_ACU_WPU_ON_OF0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_WPU_ON_OF0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_WPU_ON_OF3",
                      &colnum[HXD_ACU_WPU_ON_OF3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_WPU_ON_OF3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_WPU_ON_OF2",
                      &colnum[HXD_ACU_WPU_ON_OF2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_WPU_ON_OF2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TPU_ON_OF1",
                      &colnum[HXD_ACU_TPU_ON_OF1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TPU_ON_OF1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TPU_ON_OF0",
                      &colnum[HXD_ACU_TPU_ON_OF0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TPU_ON_OF0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TPU_ON_OF3",
                      &colnum[HXD_ACU_TPU_ON_OF3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TPU_ON_OF3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TPU_ON_OF2",
                      &colnum[HXD_ACU_TPU_ON_OF2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TPU_ON_OF2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_DATA_ID",
                      &colnum[HXD_ACU_DATA_ID], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_DATA_ID') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_ENA_DIS",
                      &colnum[HXD_ACU_HV_ENA_DIS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_ENA_DIS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVW_ON_OF1",
                      &colnum[HXD_ACU_HVW_ON_OF1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVW_ON_OF1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVW_ON_OF0",
                      &colnum[HXD_ACU_HVW_ON_OF0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVW_ON_OF0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVW_ON_OF3",
                      &colnum[HXD_ACU_HVW_ON_OF3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVW_ON_OF3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVW_ON_OF2",
                      &colnum[HXD_ACU_HVW_ON_OF2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVW_ON_OF2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVT_ON_OF1",
                      &colnum[HXD_ACU_HVT_ON_OF1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVT_ON_OF1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVT_ON_OF0",
                      &colnum[HXD_ACU_HVT_ON_OF0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVT_ON_OF0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVT_ON_OF3",
                      &colnum[HXD_ACU_HVT_ON_OF3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVT_ON_OF3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVT_ON_OF2",
                      &colnum[HXD_ACU_HVT_ON_OF2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVT_ON_OF2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVP_ON_OF1",
                      &colnum[HXD_ACU_HVP_ON_OF1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVP_ON_OF1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVP_ON_OF0",
                      &colnum[HXD_ACU_HVP_ON_OF0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVP_ON_OF0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVP_ON_OF3",
                      &colnum[HXD_ACU_HVP_ON_OF3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVP_ON_OF3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVP_ON_OF2",
                      &colnum[HXD_ACU_HVP_ON_OF2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVP_ON_OF2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_WPU_CLK_RATE",
                      &colnum[HXD_ACU_WPU_CLK_RATE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_WPU_CLK_RATE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_W0",
                      &colnum[HXD_ACU_HVREF_W0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_W0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_W1",
                      &colnum[HXD_ACU_HVREF_W1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_W1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_W2",
                      &colnum[HXD_ACU_HVREF_W2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_W2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_W3",
                      &colnum[HXD_ACU_HVREF_W3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_W3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_T0",
                      &colnum[HXD_ACU_HVREF_T0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_T0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_T1",
                      &colnum[HXD_ACU_HVREF_T1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_T1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_T2",
                      &colnum[HXD_ACU_HVREF_T2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_T2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_T3",
                      &colnum[HXD_ACU_HVREF_T3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_T3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_P0",
                      &colnum[HXD_ACU_HVREF_P0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_P0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_P1",
                      &colnum[HXD_ACU_HVREF_P1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_P1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_P2",
                      &colnum[HXD_ACU_HVREF_P2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_P2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_P3",
                      &colnum[HXD_ACU_HVREF_P3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_P3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HVREF_DATA",
                      &colnum[HXD_ACU_HVREF_DATA], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HVREF_DATA') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMP0",
                      &colnum[HXD_ACU_TEMP0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMP0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMP1",
                      &colnum[HXD_ACU_TEMP1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMP1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMP2",
                      &colnum[HXD_ACU_TEMP2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMP2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMP3",
                      &colnum[HXD_ACU_TEMP3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMP3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMP4",
                      &colnum[HXD_ACU_TEMP4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMP4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMP5",
                      &colnum[HXD_ACU_TEMP5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMP5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMP6",
                      &colnum[HXD_ACU_TEMP6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMP6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMP7",
                      &colnum[HXD_ACU_TEMP7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMP7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMP8",
                      &colnum[HXD_ACU_TEMP8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMP8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMP9",
                      &colnum[HXD_ACU_TEMP9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMP9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMPA",
                      &colnum[HXD_ACU_TEMPA], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMPA') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMPB",
                      &colnum[HXD_ACU_TEMPB], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMPB') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMPC",
                      &colnum[HXD_ACU_TEMPC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMPC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMPD",
                      &colnum[HXD_ACU_TEMPD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMPD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMPE",
                      &colnum[HXD_ACU_TEMPE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMPE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_TEMPF",
                      &colnum[HXD_ACU_TEMPF], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_TEMPF') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_W0",
                      &colnum[HXD_ACU_HV_W0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_W0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_W1",
                      &colnum[HXD_ACU_HV_W1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_W1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_W2",
                      &colnum[HXD_ACU_HV_W2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_W2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_W3",
                      &colnum[HXD_ACU_HV_W3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_W3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_T0",
                      &colnum[HXD_ACU_HV_T0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_T0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_T1",
                      &colnum[HXD_ACU_HV_T1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_T1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_T2",
                      &colnum[HXD_ACU_HV_T2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_T2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_T3",
                      &colnum[HXD_ACU_HV_T3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_T3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_P0",
                      &colnum[HXD_ACU_HV_P0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_P0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_P1",
                      &colnum[HXD_ACU_HV_P1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_P1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_P2",
                      &colnum[HXD_ACU_HV_P2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_P2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HV_P3",
                      &colnum[HXD_ACU_HV_P3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HV_P3') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadACU_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, ACU, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, ACU, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:ACU:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_acu_lngth_ok;
    fits_read_col_byt(fp, colnum[HXD_ACU_LNGTH_OK], irow, firstelem,
                      nelements, nulval, &hxd_acu_lngth_ok, &anynul, &istat);
    data[0] = hxd_acu_lngth_ok;
    BnkfPutM ("HXD:ACU:LNGTH_OK", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_acu_pi_seq_no;
    fits_read_col_usht(fp, colnum[HXD_ACU_PI_SEQ_NO], irow, firstelem,
                      nelements, nulval, &hxd_acu_pi_seq_no, &anynul, &istat);
    data[0] = hxd_acu_pi_seq_no;
    BnkfPutM ("HXD:ACU:PI_SEQ_NO", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_acu_tlatch_time;
    fits_read_col_int(fp, colnum[HXD_ACU_TLATCH_TIME], irow, firstelem,
                      nelements, nulval, &hxd_acu_tlatch_time, &anynul, &istat);
    data[0] = hxd_acu_tlatch_time;
    BnkfPutM ("HXD:ACU:TIME_LATCH_TIME", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_acu_psu_volt;
    fits_read_col_byt(fp, colnum[HXD_ACU_PSU_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_acu_psu_volt, &anynul, &istat);
    data[0] = hxd_acu_psu_volt;
    BnkfPutM ("HXD:ACU:PSU_VOLT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_acu_proc_time;
    fits_read_col_byt(fp, colnum[HXD_ACU_PROC_TIME], irow, firstelem,
                      nelements, nulval, &hxd_acu_proc_time, &anynul, &istat);
    data[0] = hxd_acu_proc_time;
    BnkfPutM ("HXD:ACU:PROC_TIME", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_acu_wpu_on_of1;
    unsigned char hxd_acu_wpu_on_of0;
    unsigned char hxd_acu_wpu_on_of3;
    unsigned char hxd_acu_wpu_on_of2;
    fits_read_col_byt(fp, colnum[HXD_ACU_WPU_ON_OF1], irow, firstelem,
                      nelements, nulval, &hxd_acu_wpu_on_of1, &anynul, &istat);
    data[0] = hxd_acu_wpu_on_of1;
    fits_read_col_byt(fp, colnum[HXD_ACU_WPU_ON_OF0], irow, firstelem,
                      nelements, nulval, &hxd_acu_wpu_on_of0, &anynul, &istat);
    data[1] = hxd_acu_wpu_on_of0;
    fits_read_col_byt(fp, colnum[HXD_ACU_WPU_ON_OF3], irow, firstelem,
                      nelements, nulval, &hxd_acu_wpu_on_of3, &anynul, &istat);
    data[2] = hxd_acu_wpu_on_of3;
    fits_read_col_byt(fp, colnum[HXD_ACU_WPU_ON_OF2], irow, firstelem,
                      nelements, nulval, &hxd_acu_wpu_on_of2, &anynul, &istat);
    data[3] = hxd_acu_wpu_on_of2;
    BnkfPutM ("HXD:ACU:WPU_ON/OF", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_acu_tpu_on_of1;
    unsigned char hxd_acu_tpu_on_of0;
    unsigned char hxd_acu_tpu_on_of3;
    unsigned char hxd_acu_tpu_on_of2;
    fits_read_col_byt(fp, colnum[HXD_ACU_TPU_ON_OF1], irow, firstelem,
                      nelements, nulval, &hxd_acu_tpu_on_of1, &anynul, &istat);
    data[0] = hxd_acu_tpu_on_of1;
    fits_read_col_byt(fp, colnum[HXD_ACU_TPU_ON_OF0], irow, firstelem,
                      nelements, nulval, &hxd_acu_tpu_on_of0, &anynul, &istat);
    data[1] = hxd_acu_tpu_on_of0;
    fits_read_col_byt(fp, colnum[HXD_ACU_TPU_ON_OF3], irow, firstelem,
                      nelements, nulval, &hxd_acu_tpu_on_of3, &anynul, &istat);
    data[2] = hxd_acu_tpu_on_of3;
    fits_read_col_byt(fp, colnum[HXD_ACU_TPU_ON_OF2], irow, firstelem,
                      nelements, nulval, &hxd_acu_tpu_on_of2, &anynul, &istat);
    data[3] = hxd_acu_tpu_on_of2;
    BnkfPutM ("HXD:ACU:TPU_ON/OF", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_acu_data_id;
    fits_read_col_byt(fp, colnum[HXD_ACU_DATA_ID], irow, firstelem,
                      nelements, nulval, &hxd_acu_data_id, &anynul, &istat);
    data[0] = hxd_acu_data_id;
    BnkfPutM ("HXD:ACU:DATA_ID", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_acu_hv_ena_dis;
    fits_read_col_byt(fp, colnum[HXD_ACU_HV_ENA_DIS], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_ena_dis, &anynul, &istat);
    data[0] = hxd_acu_hv_ena_dis;
    BnkfPutM ("HXD:ACU:HV_ENA/DIS", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_acu_hvw_on_of1;
    unsigned char hxd_acu_hvw_on_of0;
    unsigned char hxd_acu_hvw_on_of3;
    unsigned char hxd_acu_hvw_on_of2;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVW_ON_OF1], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvw_on_of1, &anynul, &istat);
    data[0] = hxd_acu_hvw_on_of1;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVW_ON_OF0], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvw_on_of0, &anynul, &istat);
    data[1] = hxd_acu_hvw_on_of0;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVW_ON_OF3], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvw_on_of3, &anynul, &istat);
    data[2] = hxd_acu_hvw_on_of3;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVW_ON_OF2], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvw_on_of2, &anynul, &istat);
    data[3] = hxd_acu_hvw_on_of2;
    BnkfPutM ("HXD:ACU:HVW_ON/OF", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_acu_hvt_on_of1;
    unsigned char hxd_acu_hvt_on_of0;
    unsigned char hxd_acu_hvt_on_of3;
    unsigned char hxd_acu_hvt_on_of2;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVT_ON_OF1], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvt_on_of1, &anynul, &istat);
    data[0] = hxd_acu_hvt_on_of1;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVT_ON_OF0], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvt_on_of0, &anynul, &istat);
    data[1] = hxd_acu_hvt_on_of0;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVT_ON_OF3], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvt_on_of3, &anynul, &istat);
    data[2] = hxd_acu_hvt_on_of3;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVT_ON_OF2], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvt_on_of2, &anynul, &istat);
    data[3] = hxd_acu_hvt_on_of2;
    BnkfPutM ("HXD:ACU:HVT_ON/OF", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_acu_hvp_on_of1;
    unsigned char hxd_acu_hvp_on_of0;
    unsigned char hxd_acu_hvp_on_of3;
    unsigned char hxd_acu_hvp_on_of2;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVP_ON_OF1], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvp_on_of1, &anynul, &istat);
    data[0] = hxd_acu_hvp_on_of1;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVP_ON_OF0], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvp_on_of0, &anynul, &istat);
    data[1] = hxd_acu_hvp_on_of0;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVP_ON_OF3], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvp_on_of3, &anynul, &istat);
    data[2] = hxd_acu_hvp_on_of3;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVP_ON_OF2], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvp_on_of2, &anynul, &istat);
    data[3] = hxd_acu_hvp_on_of2;
    BnkfPutM ("HXD:ACU:HVP_ON/OF", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_acu_wpu_clk_rate;
    fits_read_col_byt(fp, colnum[HXD_ACU_WPU_CLK_RATE], irow, firstelem,
                      nelements, nulval, &hxd_acu_wpu_clk_rate, &anynul, &istat);
    data[0] = hxd_acu_wpu_clk_rate;
    BnkfPutM ("HXD:ACU:WPU_CLK_RATE", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_acu_hvref_w0;
    unsigned char hxd_acu_hvref_w1;
    unsigned char hxd_acu_hvref_w2;
    unsigned char hxd_acu_hvref_w3;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_W0], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_w0, &anynul, &istat);
    data[0] = hxd_acu_hvref_w0;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_W1], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_w1, &anynul, &istat);
    data[1] = hxd_acu_hvref_w1;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_W2], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_w2, &anynul, &istat);
    data[2] = hxd_acu_hvref_w2;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_W3], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_w3, &anynul, &istat);
    data[3] = hxd_acu_hvref_w3;
    BnkfPutM ("HXD:ACU:HVREF_W", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_acu_hvref_t0;
    unsigned char hxd_acu_hvref_t1;
    unsigned char hxd_acu_hvref_t2;
    unsigned char hxd_acu_hvref_t3;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_T0], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_t0, &anynul, &istat);
    data[0] = hxd_acu_hvref_t0;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_T1], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_t1, &anynul, &istat);
    data[1] = hxd_acu_hvref_t1;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_T2], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_t2, &anynul, &istat);
    data[2] = hxd_acu_hvref_t2;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_T3], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_t3, &anynul, &istat);
    data[3] = hxd_acu_hvref_t3;
    BnkfPutM ("HXD:ACU:HVREF_T", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_acu_hvref_p0;
    unsigned char hxd_acu_hvref_p1;
    unsigned char hxd_acu_hvref_p2;
    unsigned char hxd_acu_hvref_p3;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_P0], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_p0, &anynul, &istat);
    data[0] = hxd_acu_hvref_p0;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_P1], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_p1, &anynul, &istat);
    data[1] = hxd_acu_hvref_p1;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_P2], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_p2, &anynul, &istat);
    data[2] = hxd_acu_hvref_p2;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_P3], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_p3, &anynul, &istat);
    data[3] = hxd_acu_hvref_p3;
    BnkfPutM ("HXD:ACU:HVREF_P", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_acu_hvref_data;
    fits_read_col_byt(fp, colnum[HXD_ACU_HVREF_DATA], irow, firstelem,
                      nelements, nulval, &hxd_acu_hvref_data, &anynul, &istat);
    data[0] = hxd_acu_hvref_data;
    BnkfPutM ("HXD:ACU:HVREF_DATA", sizeof(int)*1, data);
  }
  {
    unsigned int data[16];
    unsigned char nulval=1;
    unsigned char hxd_acu_temp0;
    unsigned char hxd_acu_temp1;
    unsigned char hxd_acu_temp2;
    unsigned char hxd_acu_temp3;
    unsigned char hxd_acu_temp4;
    unsigned char hxd_acu_temp5;
    unsigned char hxd_acu_temp6;
    unsigned char hxd_acu_temp7;
    unsigned char hxd_acu_temp8;
    unsigned char hxd_acu_temp9;
    unsigned char hxd_acu_tempa;
    unsigned char hxd_acu_tempb;
    unsigned char hxd_acu_tempc;
    unsigned char hxd_acu_tempd;
    unsigned char hxd_acu_tempe;
    unsigned char hxd_acu_tempf;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMP0], irow, firstelem,
                      nelements, nulval, &hxd_acu_temp0, &anynul, &istat);
    data[0] = hxd_acu_temp0;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMP1], irow, firstelem,
                      nelements, nulval, &hxd_acu_temp1, &anynul, &istat);
    data[1] = hxd_acu_temp1;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMP2], irow, firstelem,
                      nelements, nulval, &hxd_acu_temp2, &anynul, &istat);
    data[2] = hxd_acu_temp2;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMP3], irow, firstelem,
                      nelements, nulval, &hxd_acu_temp3, &anynul, &istat);
    data[3] = hxd_acu_temp3;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMP4], irow, firstelem,
                      nelements, nulval, &hxd_acu_temp4, &anynul, &istat);
    data[4] = hxd_acu_temp4;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMP5], irow, firstelem,
                      nelements, nulval, &hxd_acu_temp5, &anynul, &istat);
    data[5] = hxd_acu_temp5;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMP6], irow, firstelem,
                      nelements, nulval, &hxd_acu_temp6, &anynul, &istat);
    data[6] = hxd_acu_temp6;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMP7], irow, firstelem,
                      nelements, nulval, &hxd_acu_temp7, &anynul, &istat);
    data[7] = hxd_acu_temp7;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMP8], irow, firstelem,
                      nelements, nulval, &hxd_acu_temp8, &anynul, &istat);
    data[8] = hxd_acu_temp8;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMP9], irow, firstelem,
                      nelements, nulval, &hxd_acu_temp9, &anynul, &istat);
    data[9] = hxd_acu_temp9;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMPA], irow, firstelem,
                      nelements, nulval, &hxd_acu_tempa, &anynul, &istat);
    data[10] = hxd_acu_tempa;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMPB], irow, firstelem,
                      nelements, nulval, &hxd_acu_tempb, &anynul, &istat);
    data[11] = hxd_acu_tempb;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMPC], irow, firstelem,
                      nelements, nulval, &hxd_acu_tempc, &anynul, &istat);
    data[12] = hxd_acu_tempc;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMPD], irow, firstelem,
                      nelements, nulval, &hxd_acu_tempd, &anynul, &istat);
    data[13] = hxd_acu_tempd;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMPE], irow, firstelem,
                      nelements, nulval, &hxd_acu_tempe, &anynul, &istat);
    data[14] = hxd_acu_tempe;
    fits_read_col_byt(fp, colnum[HXD_ACU_TEMPF], irow, firstelem,
                      nelements, nulval, &hxd_acu_tempf, &anynul, &istat);
    data[15] = hxd_acu_tempf;
    BnkfPutM ("HXD:ACU:TEMP", sizeof(int)*16, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_acu_hv_w0;
    unsigned short hxd_acu_hv_w1;
    unsigned short hxd_acu_hv_w2;
    unsigned short hxd_acu_hv_w3;
    fits_read_col_usht(fp, colnum[HXD_ACU_HV_W0], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_w0, &anynul, &istat);
    data[0] = hxd_acu_hv_w0;
    fits_read_col_usht(fp, colnum[HXD_ACU_HV_W1], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_w1, &anynul, &istat);
    data[1] = hxd_acu_hv_w1;
    fits_read_col_usht(fp, colnum[HXD_ACU_HV_W2], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_w2, &anynul, &istat);
    data[2] = hxd_acu_hv_w2;
    fits_read_col_usht(fp, colnum[HXD_ACU_HV_W3], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_w3, &anynul, &istat);
    data[3] = hxd_acu_hv_w3;
    BnkfPutM ("HXD:ACU:HV_W", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_acu_hv_t0;
    unsigned short hxd_acu_hv_t1;
    unsigned short hxd_acu_hv_t2;
    unsigned short hxd_acu_hv_t3;
    fits_read_col_usht(fp, colnum[HXD_ACU_HV_T0], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_t0, &anynul, &istat);
    data[0] = hxd_acu_hv_t0;
    fits_read_col_usht(fp, colnum[HXD_ACU_HV_T1], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_t1, &anynul, &istat);
    data[1] = hxd_acu_hv_t1;
    fits_read_col_usht(fp, colnum[HXD_ACU_HV_T2], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_t2, &anynul, &istat);
    data[2] = hxd_acu_hv_t2;
    fits_read_col_usht(fp, colnum[HXD_ACU_HV_T3], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_t3, &anynul, &istat);
    data[3] = hxd_acu_hv_t3;
    BnkfPutM ("HXD:ACU:HV_T", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_acu_hv_p0;
    unsigned short hxd_acu_hv_p1;
    unsigned short hxd_acu_hv_p2;
    unsigned short hxd_acu_hv_p3;
    fits_read_col_usht(fp, colnum[HXD_ACU_HV_P0], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_p0, &anynul, &istat);
    data[0] = hxd_acu_hv_p0;
    fits_read_col_usht(fp, colnum[HXD_ACU_HV_P1], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_p1, &anynul, &istat);
    data[1] = hxd_acu_hv_p1;
    fits_read_col_usht(fp, colnum[HXD_ACU_HV_P2], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_p2, &anynul, &istat);
    data[2] = hxd_acu_hv_p2;
    fits_read_col_usht(fp, colnum[HXD_ACU_HV_P3], irow, firstelem,
                      nelements, nulval, &hxd_acu_hv_p3, &anynul, &istat);
    data[3] = hxd_acu_hv_p3;
    BnkfPutM ("HXD:ACU:HV_P", sizeof(int)*4, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
