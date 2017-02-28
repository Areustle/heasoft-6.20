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
  HXD_AET_SC_TABLE_TYPE,
  HXD_AET_SC_TABLE_NO,
  HXD_AET_SC_W_INTERRUPT0,
  HXD_AET_SC_W_INTERRUPT1,
  HXD_AET_SC_W_INTERRUPT2,
  HXD_AET_SC_W_INTERRUPT3,
  HXD_AET_SC_W_PINGAIN0,
  HXD_AET_SC_W_PINGAIN1,
  HXD_AET_SC_W_PINGAIN2,
  HXD_AET_SC_W_PINGAIN3,
  HXD_AET_SC_W_PINGAIN4,
  HXD_AET_SC_W_PINGAIN5,
  HXD_AET_SC_W_PINGAIN6,
  HXD_AET_SC_W_PINGAIN7,
  HXD_AET_SC_W_PINGAIN8,
  HXD_AET_SC_W_PINGAIN9,
  HXD_AET_SC_W_PINGAIN10,
  HXD_AET_SC_W_PINGAIN11,
  HXD_AET_SC_W_PINGAIN12,
  HXD_AET_SC_W_PINGAIN13,
  HXD_AET_SC_W_PINGAIN14,
  HXD_AET_SC_W_PINGAIN15,
  HXD_AET_SC_W_SLWGAIN0,
  HXD_AET_SC_W_SLWGAIN1,
  HXD_AET_SC_W_SLWGAIN2,
  HXD_AET_SC_W_SLWGAIN3,
  HXD_AET_SC_W_SLWGAIN4,
  HXD_AET_SC_W_SLWGAIN5,
  HXD_AET_SC_W_SLWGAIN6,
  HXD_AET_SC_W_SLWGAIN7,
  HXD_AET_SC_W_SLWGAIN8,
  HXD_AET_SC_W_SLWGAIN9,
  HXD_AET_SC_W_SLWGAIN10,
  HXD_AET_SC_W_SLWGAIN11,
  HXD_AET_SC_W_SLWGAIN12,
  HXD_AET_SC_W_SLWGAIN13,
  HXD_AET_SC_W_SLWGAIN14,
  HXD_AET_SC_W_SLWGAIN15,
  HXD_AET_SC_W_FSTGAIN0,
  HXD_AET_SC_W_FSTGAIN1,
  HXD_AET_SC_W_FSTGAIN2,
  HXD_AET_SC_W_FSTGAIN3,
  HXD_AET_SC_W_FSTGAIN4,
  HXD_AET_SC_W_FSTGAIN5,
  HXD_AET_SC_W_FSTGAIN6,
  HXD_AET_SC_W_FSTGAIN7,
  HXD_AET_SC_W_FSTGAIN8,
  HXD_AET_SC_W_FSTGAIN9,
  HXD_AET_SC_W_FSTGAIN10,
  HXD_AET_SC_W_FSTGAIN11,
  HXD_AET_SC_W_FSTGAIN12,
  HXD_AET_SC_W_FSTGAIN13,
  HXD_AET_SC_W_FSTGAIN14,
  HXD_AET_SC_W_FSTGAIN15,
  HXD_AET_SC_W_PMT_ATTEN0,
  HXD_AET_SC_W_PMT_ATTEN1,
  HXD_AET_SC_W_PMT_ATTEN2,
  HXD_AET_SC_W_PMT_ATTEN3,
  HXD_AET_SC_W_PMT_ATTEN4,
  HXD_AET_SC_W_PMT_ATTEN5,
  HXD_AET_SC_W_PMT_ATTEN6,
  HXD_AET_SC_W_PMT_ATTEN7,
  HXD_AET_SC_W_PMT_ATTEN8,
  HXD_AET_SC_W_PMT_ATTEN9,
  HXD_AET_SC_W_PMT_ATTEN10,
  HXD_AET_SC_W_PMT_ATTEN11,
  HXD_AET_SC_W_PMT_ATTEN12,
  HXD_AET_SC_W_PMT_ATTEN13,
  HXD_AET_SC_W_PMT_ATTEN14,
  HXD_AET_SC_W_PMT_ATTEN15,
  HXD_AET_SC_W_PMTLD0,
  HXD_AET_SC_W_PMTLD1,
  HXD_AET_SC_W_PMTLD2,
  HXD_AET_SC_W_PMTLD3,
  HXD_AET_SC_W_PMTLD4,
  HXD_AET_SC_W_PMTLD5,
  HXD_AET_SC_W_PMTLD6,
  HXD_AET_SC_W_PMTLD7,
  HXD_AET_SC_W_PMTLD8,
  HXD_AET_SC_W_PMTLD9,
  HXD_AET_SC_W_PMTLD10,
  HXD_AET_SC_W_PMTLD11,
  HXD_AET_SC_W_PMTLD12,
  HXD_AET_SC_W_PMTLD13,
  HXD_AET_SC_W_PMTLD14,
  HXD_AET_SC_W_PMTLD15,
  HXD_AET_SC_W_PINLD0,
  HXD_AET_SC_W_PINLD1,
  HXD_AET_SC_W_PINLD2,
  HXD_AET_SC_W_PINLD3,
  HXD_AET_SC_W_TRGSW0,
  HXD_AET_SC_W_TRGSW1,
  HXD_AET_SC_W_TRGSW2,
  HXD_AET_SC_W_TRGSW3,
  HXD_AET_SC_W_TRGSW4,
  HXD_AET_SC_W_TRGSW5,
  HXD_AET_SC_W_TRGSW6,
  HXD_AET_SC_W_TRGSW7,
  HXD_AET_SC_W_TRGSW8,
  HXD_AET_SC_W_TRGSW9,
  HXD_AET_SC_W_TRGSW10,
  HXD_AET_SC_W_TRGSW11,
  HXD_AET_SC_W_TRGSW12,
  HXD_AET_SC_W_TRGSW13,
  HXD_AET_SC_W_TRGSW14,
  HXD_AET_SC_W_TRGSW15,
  HXD_AET_SC_W_PMTUD0,
  HXD_AET_SC_W_PMTUD1,
  HXD_AET_SC_W_PMTUD2,
  HXD_AET_SC_W_PMTUD3,
  HXD_AET_SC_W_PINUD0,
  HXD_AET_SC_W_PINUD1,
  HXD_AET_SC_W_PINUD2,
  HXD_AET_SC_W_PINUD3,
  HXD_AET_SC_W_PINUD4,
  HXD_AET_SC_W_PINUD5,
  HXD_AET_SC_W_PINUD6,
  HXD_AET_SC_W_PINUD7,
  HXD_AET_SC_W_PINUD8,
  HXD_AET_SC_W_PINUD9,
  HXD_AET_SC_W_PINUD10,
  HXD_AET_SC_W_PINUD11,
  HXD_AET_SC_W_PINUD12,
  HXD_AET_SC_W_PINUD13,
  HXD_AET_SC_W_PINUD14,
  HXD_AET_SC_W_PINUD15,
  HXD_AET_SC_W_HPT_WDT0,
  HXD_AET_SC_W_HPT_WDT1,
  HXD_AET_SC_W_HPT_WDT2,
  HXD_AET_SC_W_HPT_WDT3,
  HXD_AET_SC_W_PSD_LVL0,
  HXD_AET_SC_W_PSD_LVL1,
  HXD_AET_SC_W_PSD_LVL2,
  HXD_AET_SC_W_PSD_LVL3,
  HXD_AET_SC_W_PSD_LVL4,
  HXD_AET_SC_W_PSD_LVL5,
  HXD_AET_SC_W_PSD_LVL6,
  HXD_AET_SC_W_PSD_LVL7,
  HXD_AET_SC_W_PSD_LVL8,
  HXD_AET_SC_W_PSD_LVL9,
  HXD_AET_SC_W_PSD_LVL10,
  HXD_AET_SC_W_PSD_LVL11,
  HXD_AET_SC_W_PSD_LVL12,
  HXD_AET_SC_W_PSD_LVL13,
  HXD_AET_SC_W_PSD_LVL14,
  HXD_AET_SC_W_PSD_LVL15,
  HXD_AET_SC_W_INHIBIT0,
  HXD_AET_SC_W_INHIBIT1,
  HXD_AET_SC_W_INHIBIT2,
  HXD_AET_SC_W_INHIBIT3,
  HXD_AET_SC_W_INHIBIT4,
  HXD_AET_SC_W_INHIBIT5,
  HXD_AET_SC_W_INHIBIT6,
  HXD_AET_SC_W_INHIBIT7,
  HXD_AET_SC_W_INHIBIT8,
  HXD_AET_SC_W_INHIBIT9,
  HXD_AET_SC_W_INHIBIT10,
  HXD_AET_SC_W_INHIBIT11,
  HXD_AET_SC_W_INHIBIT12,
  HXD_AET_SC_W_INHIBIT13,
  HXD_AET_SC_W_INHIBIT14,
  HXD_AET_SC_W_INHIBIT15,
  HXD_AET_SC_W_PSDOFF0,
  HXD_AET_SC_W_PSDOFF1,
  HXD_AET_SC_W_PSDOFF2,
  HXD_AET_SC_W_PSDOFF3,
  HXD_AET_SC_T_PMTGAIN0,
  HXD_AET_SC_T_PMTGAIN1,
  HXD_AET_SC_T_PMTGAIN2,
  HXD_AET_SC_T_PMTGAIN3,
  HXD_AET_SC_T_PMTGAIN4,
  HXD_AET_SC_T_PMTGAIN5,
  HXD_AET_SC_T_PMTGAIN6,
  HXD_AET_SC_T_PMTGAIN7,
  HXD_AET_SC_T_PMTGAIN8,
  HXD_AET_SC_T_PMTGAIN9,
  HXD_AET_SC_T_PMTGAIN10,
  HXD_AET_SC_T_PMTGAIN11,
  HXD_AET_SC_T_PMTGAIN12,
  HXD_AET_SC_T_PMTGAIN13,
  HXD_AET_SC_T_PMTGAIN14,
  HXD_AET_SC_T_PMTGAIN15,
  HXD_AET_SC_T_PMTGAIN16,
  HXD_AET_SC_T_PMTGAIN17,
  HXD_AET_SC_T_PMTGAIN18,
  HXD_AET_SC_T_PMTGAIN19,
  HXD_AET_SC_T_PMTLD0,
  HXD_AET_SC_T_PMTLD1,
  HXD_AET_SC_T_PMTLD2,
  HXD_AET_SC_T_PMTLD3,
  HXD_AET_SC_T_PMTLDSUM0,
  HXD_AET_SC_T_PMTLDSUM1,
  HXD_AET_SC_T_PMTLDSUM2,
  HXD_AET_SC_T_PMTLDSUM3,
  HXD_AET_SC_T_TRGSW0,
  HXD_AET_SC_T_TRGSW1,
  HXD_AET_SC_T_TRGSW2,
  HXD_AET_SC_T_TRGSW3,
  HXD_AET_SC_T_PMTUDSW0,
  HXD_AET_SC_T_PMTUDSW1,
  HXD_AET_SC_T_PMTUDSW2,
  HXD_AET_SC_T_PMTUDSW3,
  HXD_AET_SC_T_HPT_WDT0,
  HXD_AET_SC_T_HPT_WDT1,
  HXD_AET_SC_T_HPT_WDT2,
  HXD_AET_SC_T_HPT_WDT3,
  HXD_AET_SC_T_GBTH_TIM0,
  HXD_AET_SC_T_GBTH_TIM1,
  HXD_AET_SC_T_GBTH_TIM2,
  HXD_AET_SC_T_GBTH_TIM3,
  HXD_AET_SC_T_GBENA0,
  HXD_AET_SC_T_GBENA1,
  HXD_AET_SC_T_GBENA2,
  HXD_AET_SC_T_GBENA3,
  HXD_AET_SC_T_GBTRG_DE0,
  HXD_AET_SC_T_GBTRG_DE1,
  HXD_AET_SC_T_GBTRG_DE2,
  HXD_AET_SC_T_GBTRG_DE3,
  HXD_AET_SC_T_GBTRG_SD0,
  HXD_AET_SC_T_GBTRG_SD1,
  HXD_AET_SC_T_GBTRG_SD2,
  HXD_AET_SC_T_GBTRG_SD3,
  HXD_AET_SC_T_GB_THRES0,
  HXD_AET_SC_T_GB_THRES1,
  HXD_AET_SC_T_GB_THRES2,
  HXD_AET_SC_T_GB_THRES3,
  HXD_AET_SC_T_RBM_TRG0,
  HXD_AET_SC_T_RBM_TRG1,
  HXD_AET_SC_T_RBM_TRG2,
  HXD_AET_SC_T_RBM_TRG3,
};

static char pname[] = "HXDHKFitsReadAET_SC";

static int colnum[230];
static int time_colnum;

void
HXDHKFitsReadAET_SC_init()
{
  BnkDef( "HXD:AET_SC:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:AET:SC:TABLE_TYPE", sizeof(int) );
  BnkDef( "HXD:AET:SC:TABLE_NO", sizeof(int) );
  BnkDef( "HXD:AET:SC:W_INTERRUPT", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:W_PINGAIN", sizeof(int)*16 );
  BnkDef( "HXD:AET:SC:W_SLWGAIN", sizeof(int)*16 );
  BnkDef( "HXD:AET:SC:W_FSTGAIN", sizeof(int)*16 );
  BnkDef( "HXD:AET:SC:W_PMT_ATTEN", sizeof(int)*16 );
  BnkDef( "HXD:AET:SC:W_PMTLD", sizeof(int)*16 );
  BnkDef( "HXD:AET:SC:W_PINLD", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:W_TRGSW", sizeof(int)*16 );
  BnkDef( "HXD:AET:SC:W_PMTUD", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:W_PINUD", sizeof(int)*16 );
  BnkDef( "HXD:AET:SC:W_HPT_WDT", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:W_PSD_LVL", sizeof(int)*16 );
  BnkDef( "HXD:AET:SC:W_INHIBIT", sizeof(int)*16 );
  BnkDef( "HXD:AET:SC:W_PSDOFF", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:T_PMTGAIN", sizeof(int)*20 );
  BnkDef( "HXD:AET:SC:T_PMTLD", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:T_PMTLDSUM", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:T_TRGSW", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:T_PMTUDSW", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:T_HPT_WDT", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:T_GBTH_TIM", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:T_GBENA", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:T_GBTRG_DE", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:T_GBTRG_SD", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:T_GB_THRES", sizeof(int)*4 );
  BnkDef( "HXD:AET:SC:T_RBM_TRG", sizeof(int)*4 );
  
}


int
HXDHKFitsReadAET_SC_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, AET_SC, &hdutype, &istat );
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
  
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_TABLE_TYPE",
                      &colnum[HXD_AET_SC_TABLE_TYPE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_TABLE_TYPE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_TABLE_NO",
                      &colnum[HXD_AET_SC_TABLE_NO], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_TABLE_NO') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INTERRUPT0",
                      &colnum[HXD_AET_SC_W_INTERRUPT0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INTERRUPT0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INTERRUPT1",
                      &colnum[HXD_AET_SC_W_INTERRUPT1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INTERRUPT1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INTERRUPT2",
                      &colnum[HXD_AET_SC_W_INTERRUPT2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INTERRUPT2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INTERRUPT3",
                      &colnum[HXD_AET_SC_W_INTERRUPT3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INTERRUPT3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN0",
                      &colnum[HXD_AET_SC_W_PINGAIN0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN1",
                      &colnum[HXD_AET_SC_W_PINGAIN1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN2",
                      &colnum[HXD_AET_SC_W_PINGAIN2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN3",
                      &colnum[HXD_AET_SC_W_PINGAIN3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN4",
                      &colnum[HXD_AET_SC_W_PINGAIN4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN5",
                      &colnum[HXD_AET_SC_W_PINGAIN5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN6",
                      &colnum[HXD_AET_SC_W_PINGAIN6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN7",
                      &colnum[HXD_AET_SC_W_PINGAIN7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN8",
                      &colnum[HXD_AET_SC_W_PINGAIN8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN9",
                      &colnum[HXD_AET_SC_W_PINGAIN9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN10",
                      &colnum[HXD_AET_SC_W_PINGAIN10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN11",
                      &colnum[HXD_AET_SC_W_PINGAIN11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN12",
                      &colnum[HXD_AET_SC_W_PINGAIN12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN13",
                      &colnum[HXD_AET_SC_W_PINGAIN13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN14",
                      &colnum[HXD_AET_SC_W_PINGAIN14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINGAIN15",
                      &colnum[HXD_AET_SC_W_PINGAIN15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINGAIN15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN0",
                      &colnum[HXD_AET_SC_W_SLWGAIN0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN1",
                      &colnum[HXD_AET_SC_W_SLWGAIN1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN2",
                      &colnum[HXD_AET_SC_W_SLWGAIN2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN3",
                      &colnum[HXD_AET_SC_W_SLWGAIN3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN4",
                      &colnum[HXD_AET_SC_W_SLWGAIN4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN5",
                      &colnum[HXD_AET_SC_W_SLWGAIN5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN6",
                      &colnum[HXD_AET_SC_W_SLWGAIN6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN7",
                      &colnum[HXD_AET_SC_W_SLWGAIN7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN8",
                      &colnum[HXD_AET_SC_W_SLWGAIN8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN9",
                      &colnum[HXD_AET_SC_W_SLWGAIN9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN10",
                      &colnum[HXD_AET_SC_W_SLWGAIN10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN11",
                      &colnum[HXD_AET_SC_W_SLWGAIN11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN12",
                      &colnum[HXD_AET_SC_W_SLWGAIN12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN13",
                      &colnum[HXD_AET_SC_W_SLWGAIN13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN14",
                      &colnum[HXD_AET_SC_W_SLWGAIN14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_SLWGAIN15",
                      &colnum[HXD_AET_SC_W_SLWGAIN15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_SLWGAIN15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN0",
                      &colnum[HXD_AET_SC_W_FSTGAIN0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN1",
                      &colnum[HXD_AET_SC_W_FSTGAIN1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN2",
                      &colnum[HXD_AET_SC_W_FSTGAIN2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN3",
                      &colnum[HXD_AET_SC_W_FSTGAIN3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN4",
                      &colnum[HXD_AET_SC_W_FSTGAIN4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN5",
                      &colnum[HXD_AET_SC_W_FSTGAIN5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN6",
                      &colnum[HXD_AET_SC_W_FSTGAIN6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN7",
                      &colnum[HXD_AET_SC_W_FSTGAIN7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN8",
                      &colnum[HXD_AET_SC_W_FSTGAIN8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN9",
                      &colnum[HXD_AET_SC_W_FSTGAIN9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN10",
                      &colnum[HXD_AET_SC_W_FSTGAIN10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN11",
                      &colnum[HXD_AET_SC_W_FSTGAIN11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN12",
                      &colnum[HXD_AET_SC_W_FSTGAIN12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN13",
                      &colnum[HXD_AET_SC_W_FSTGAIN13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN14",
                      &colnum[HXD_AET_SC_W_FSTGAIN14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_FSTGAIN15",
                      &colnum[HXD_AET_SC_W_FSTGAIN15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_FSTGAIN15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN0",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN1",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN2",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN3",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN4",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN5",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN6",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN7",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN8",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN9",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN10",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN11",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN12",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN13",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN14",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMT_ATTEN15",
                      &colnum[HXD_AET_SC_W_PMT_ATTEN15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMT_ATTEN15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD0",
                      &colnum[HXD_AET_SC_W_PMTLD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD1",
                      &colnum[HXD_AET_SC_W_PMTLD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD2",
                      &colnum[HXD_AET_SC_W_PMTLD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD3",
                      &colnum[HXD_AET_SC_W_PMTLD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD4",
                      &colnum[HXD_AET_SC_W_PMTLD4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD5",
                      &colnum[HXD_AET_SC_W_PMTLD5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD6",
                      &colnum[HXD_AET_SC_W_PMTLD6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD7",
                      &colnum[HXD_AET_SC_W_PMTLD7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD8",
                      &colnum[HXD_AET_SC_W_PMTLD8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD9",
                      &colnum[HXD_AET_SC_W_PMTLD9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD10",
                      &colnum[HXD_AET_SC_W_PMTLD10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD11",
                      &colnum[HXD_AET_SC_W_PMTLD11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD12",
                      &colnum[HXD_AET_SC_W_PMTLD12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD13",
                      &colnum[HXD_AET_SC_W_PMTLD13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD14",
                      &colnum[HXD_AET_SC_W_PMTLD14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTLD15",
                      &colnum[HXD_AET_SC_W_PMTLD15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTLD15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINLD0",
                      &colnum[HXD_AET_SC_W_PINLD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINLD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINLD1",
                      &colnum[HXD_AET_SC_W_PINLD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINLD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINLD2",
                      &colnum[HXD_AET_SC_W_PINLD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINLD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINLD3",
                      &colnum[HXD_AET_SC_W_PINLD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINLD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW0",
                      &colnum[HXD_AET_SC_W_TRGSW0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW1",
                      &colnum[HXD_AET_SC_W_TRGSW1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW2",
                      &colnum[HXD_AET_SC_W_TRGSW2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW3",
                      &colnum[HXD_AET_SC_W_TRGSW3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW4",
                      &colnum[HXD_AET_SC_W_TRGSW4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW5",
                      &colnum[HXD_AET_SC_W_TRGSW5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW6",
                      &colnum[HXD_AET_SC_W_TRGSW6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW7",
                      &colnum[HXD_AET_SC_W_TRGSW7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW8",
                      &colnum[HXD_AET_SC_W_TRGSW8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW9",
                      &colnum[HXD_AET_SC_W_TRGSW9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW10",
                      &colnum[HXD_AET_SC_W_TRGSW10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW11",
                      &colnum[HXD_AET_SC_W_TRGSW11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW12",
                      &colnum[HXD_AET_SC_W_TRGSW12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW13",
                      &colnum[HXD_AET_SC_W_TRGSW13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW14",
                      &colnum[HXD_AET_SC_W_TRGSW14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_TRGSW15",
                      &colnum[HXD_AET_SC_W_TRGSW15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_TRGSW15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTUD0",
                      &colnum[HXD_AET_SC_W_PMTUD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTUD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTUD1",
                      &colnum[HXD_AET_SC_W_PMTUD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTUD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTUD2",
                      &colnum[HXD_AET_SC_W_PMTUD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTUD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PMTUD3",
                      &colnum[HXD_AET_SC_W_PMTUD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PMTUD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD0",
                      &colnum[HXD_AET_SC_W_PINUD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD1",
                      &colnum[HXD_AET_SC_W_PINUD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD2",
                      &colnum[HXD_AET_SC_W_PINUD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD3",
                      &colnum[HXD_AET_SC_W_PINUD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD4",
                      &colnum[HXD_AET_SC_W_PINUD4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD5",
                      &colnum[HXD_AET_SC_W_PINUD5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD6",
                      &colnum[HXD_AET_SC_W_PINUD6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD7",
                      &colnum[HXD_AET_SC_W_PINUD7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD8",
                      &colnum[HXD_AET_SC_W_PINUD8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD9",
                      &colnum[HXD_AET_SC_W_PINUD9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD10",
                      &colnum[HXD_AET_SC_W_PINUD10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD11",
                      &colnum[HXD_AET_SC_W_PINUD11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD12",
                      &colnum[HXD_AET_SC_W_PINUD12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD13",
                      &colnum[HXD_AET_SC_W_PINUD13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD14",
                      &colnum[HXD_AET_SC_W_PINUD14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PINUD15",
                      &colnum[HXD_AET_SC_W_PINUD15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PINUD15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_HPT_WDT0",
                      &colnum[HXD_AET_SC_W_HPT_WDT0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_HPT_WDT0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_HPT_WDT1",
                      &colnum[HXD_AET_SC_W_HPT_WDT1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_HPT_WDT1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_HPT_WDT2",
                      &colnum[HXD_AET_SC_W_HPT_WDT2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_HPT_WDT2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_HPT_WDT3",
                      &colnum[HXD_AET_SC_W_HPT_WDT3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_HPT_WDT3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL0",
                      &colnum[HXD_AET_SC_W_PSD_LVL0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL1",
                      &colnum[HXD_AET_SC_W_PSD_LVL1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL2",
                      &colnum[HXD_AET_SC_W_PSD_LVL2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL3",
                      &colnum[HXD_AET_SC_W_PSD_LVL3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL4",
                      &colnum[HXD_AET_SC_W_PSD_LVL4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL5",
                      &colnum[HXD_AET_SC_W_PSD_LVL5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL6",
                      &colnum[HXD_AET_SC_W_PSD_LVL6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL7",
                      &colnum[HXD_AET_SC_W_PSD_LVL7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL8",
                      &colnum[HXD_AET_SC_W_PSD_LVL8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL9",
                      &colnum[HXD_AET_SC_W_PSD_LVL9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL10",
                      &colnum[HXD_AET_SC_W_PSD_LVL10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL11",
                      &colnum[HXD_AET_SC_W_PSD_LVL11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL12",
                      &colnum[HXD_AET_SC_W_PSD_LVL12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL13",
                      &colnum[HXD_AET_SC_W_PSD_LVL13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL14",
                      &colnum[HXD_AET_SC_W_PSD_LVL14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSD_LVL15",
                      &colnum[HXD_AET_SC_W_PSD_LVL15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSD_LVL15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT0",
                      &colnum[HXD_AET_SC_W_INHIBIT0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT1",
                      &colnum[HXD_AET_SC_W_INHIBIT1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT2",
                      &colnum[HXD_AET_SC_W_INHIBIT2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT3",
                      &colnum[HXD_AET_SC_W_INHIBIT3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT4",
                      &colnum[HXD_AET_SC_W_INHIBIT4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT5",
                      &colnum[HXD_AET_SC_W_INHIBIT5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT6",
                      &colnum[HXD_AET_SC_W_INHIBIT6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT7",
                      &colnum[HXD_AET_SC_W_INHIBIT7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT8",
                      &colnum[HXD_AET_SC_W_INHIBIT8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT9",
                      &colnum[HXD_AET_SC_W_INHIBIT9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT10",
                      &colnum[HXD_AET_SC_W_INHIBIT10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT11",
                      &colnum[HXD_AET_SC_W_INHIBIT11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT12",
                      &colnum[HXD_AET_SC_W_INHIBIT12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT13",
                      &colnum[HXD_AET_SC_W_INHIBIT13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT14",
                      &colnum[HXD_AET_SC_W_INHIBIT14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_INHIBIT15",
                      &colnum[HXD_AET_SC_W_INHIBIT15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_INHIBIT15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSDOFF0",
                      &colnum[HXD_AET_SC_W_PSDOFF0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSDOFF0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSDOFF1",
                      &colnum[HXD_AET_SC_W_PSDOFF1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSDOFF1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSDOFF2",
                      &colnum[HXD_AET_SC_W_PSDOFF2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSDOFF2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_W_PSDOFF3",
                      &colnum[HXD_AET_SC_W_PSDOFF3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_W_PSDOFF3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN0",
                      &colnum[HXD_AET_SC_T_PMTGAIN0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN1",
                      &colnum[HXD_AET_SC_T_PMTGAIN1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN2",
                      &colnum[HXD_AET_SC_T_PMTGAIN2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN3",
                      &colnum[HXD_AET_SC_T_PMTGAIN3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN4",
                      &colnum[HXD_AET_SC_T_PMTGAIN4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN5",
                      &colnum[HXD_AET_SC_T_PMTGAIN5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN6",
                      &colnum[HXD_AET_SC_T_PMTGAIN6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN7",
                      &colnum[HXD_AET_SC_T_PMTGAIN7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN8",
                      &colnum[HXD_AET_SC_T_PMTGAIN8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN9",
                      &colnum[HXD_AET_SC_T_PMTGAIN9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN10",
                      &colnum[HXD_AET_SC_T_PMTGAIN10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN11",
                      &colnum[HXD_AET_SC_T_PMTGAIN11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN12",
                      &colnum[HXD_AET_SC_T_PMTGAIN12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN13",
                      &colnum[HXD_AET_SC_T_PMTGAIN13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN14",
                      &colnum[HXD_AET_SC_T_PMTGAIN14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN15",
                      &colnum[HXD_AET_SC_T_PMTGAIN15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN16",
                      &colnum[HXD_AET_SC_T_PMTGAIN16], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN16') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN17",
                      &colnum[HXD_AET_SC_T_PMTGAIN17], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN17') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN18",
                      &colnum[HXD_AET_SC_T_PMTGAIN18], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN18') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTGAIN19",
                      &colnum[HXD_AET_SC_T_PMTGAIN19], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTGAIN19') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTLD0",
                      &colnum[HXD_AET_SC_T_PMTLD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTLD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTLD1",
                      &colnum[HXD_AET_SC_T_PMTLD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTLD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTLD2",
                      &colnum[HXD_AET_SC_T_PMTLD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTLD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTLD3",
                      &colnum[HXD_AET_SC_T_PMTLD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTLD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTLDSUM0",
                      &colnum[HXD_AET_SC_T_PMTLDSUM0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTLDSUM0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTLDSUM1",
                      &colnum[HXD_AET_SC_T_PMTLDSUM1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTLDSUM1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTLDSUM2",
                      &colnum[HXD_AET_SC_T_PMTLDSUM2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTLDSUM2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTLDSUM3",
                      &colnum[HXD_AET_SC_T_PMTLDSUM3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTLDSUM3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_TRGSW0",
                      &colnum[HXD_AET_SC_T_TRGSW0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_TRGSW0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_TRGSW1",
                      &colnum[HXD_AET_SC_T_TRGSW1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_TRGSW1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_TRGSW2",
                      &colnum[HXD_AET_SC_T_TRGSW2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_TRGSW2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_TRGSW3",
                      &colnum[HXD_AET_SC_T_TRGSW3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_TRGSW3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTUDSW0",
                      &colnum[HXD_AET_SC_T_PMTUDSW0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTUDSW0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTUDSW1",
                      &colnum[HXD_AET_SC_T_PMTUDSW1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTUDSW1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTUDSW2",
                      &colnum[HXD_AET_SC_T_PMTUDSW2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTUDSW2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_PMTUDSW3",
                      &colnum[HXD_AET_SC_T_PMTUDSW3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_PMTUDSW3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_HPT_WDT0",
                      &colnum[HXD_AET_SC_T_HPT_WDT0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_HPT_WDT0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_HPT_WDT1",
                      &colnum[HXD_AET_SC_T_HPT_WDT1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_HPT_WDT1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_HPT_WDT2",
                      &colnum[HXD_AET_SC_T_HPT_WDT2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_HPT_WDT2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_HPT_WDT3",
                      &colnum[HXD_AET_SC_T_HPT_WDT3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_HPT_WDT3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBTH_TIM0",
                      &colnum[HXD_AET_SC_T_GBTH_TIM0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBTH_TIM0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBTH_TIM1",
                      &colnum[HXD_AET_SC_T_GBTH_TIM1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBTH_TIM1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBTH_TIM2",
                      &colnum[HXD_AET_SC_T_GBTH_TIM2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBTH_TIM2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBTH_TIM3",
                      &colnum[HXD_AET_SC_T_GBTH_TIM3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBTH_TIM3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBENA0",
                      &colnum[HXD_AET_SC_T_GBENA0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBENA0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBENA1",
                      &colnum[HXD_AET_SC_T_GBENA1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBENA1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBENA2",
                      &colnum[HXD_AET_SC_T_GBENA2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBENA2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBENA3",
                      &colnum[HXD_AET_SC_T_GBENA3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBENA3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBTRG_DE0",
                      &colnum[HXD_AET_SC_T_GBTRG_DE0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBTRG_DE0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBTRG_DE1",
                      &colnum[HXD_AET_SC_T_GBTRG_DE1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBTRG_DE1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBTRG_DE2",
                      &colnum[HXD_AET_SC_T_GBTRG_DE2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBTRG_DE2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBTRG_DE3",
                      &colnum[HXD_AET_SC_T_GBTRG_DE3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBTRG_DE3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBTRG_SD0",
                      &colnum[HXD_AET_SC_T_GBTRG_SD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBTRG_SD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBTRG_SD1",
                      &colnum[HXD_AET_SC_T_GBTRG_SD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBTRG_SD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBTRG_SD2",
                      &colnum[HXD_AET_SC_T_GBTRG_SD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBTRG_SD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GBTRG_SD3",
                      &colnum[HXD_AET_SC_T_GBTRG_SD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GBTRG_SD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GB_THRES0",
                      &colnum[HXD_AET_SC_T_GB_THRES0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GB_THRES0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GB_THRES1",
                      &colnum[HXD_AET_SC_T_GB_THRES1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GB_THRES1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GB_THRES2",
                      &colnum[HXD_AET_SC_T_GB_THRES2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GB_THRES2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_GB_THRES3",
                      &colnum[HXD_AET_SC_T_GB_THRES3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_GB_THRES3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_RBM_TRG0",
                      &colnum[HXD_AET_SC_T_RBM_TRG0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_RBM_TRG0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_RBM_TRG1",
                      &colnum[HXD_AET_SC_T_RBM_TRG1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_RBM_TRG1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_RBM_TRG2",
                      &colnum[HXD_AET_SC_T_RBM_TRG2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_RBM_TRG2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_SC_T_RBM_TRG3",
                      &colnum[HXD_AET_SC_T_RBM_TRG3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_SC_T_RBM_TRG3') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadAET_SC_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, AET_SC, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, AET_SC, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:AET_SC:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_table_type;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_TABLE_TYPE], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_table_type, &anynul, &istat);
    data[0] = hxd_aet_sc_table_type;
    BnkfPutM ("HXD:AET:SC:TABLE_TYPE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_table_no;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_TABLE_NO], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_table_no, &anynul, &istat);
    data[0] = hxd_aet_sc_table_no;
    BnkfPutM ("HXD:AET:SC:TABLE_NO", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_interrupt0;
    unsigned char hxd_aet_sc_w_interrupt1;
    unsigned char hxd_aet_sc_w_interrupt2;
    unsigned char hxd_aet_sc_w_interrupt3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INTERRUPT0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_interrupt0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_interrupt0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INTERRUPT1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_interrupt1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_interrupt1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INTERRUPT2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_interrupt2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_interrupt2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INTERRUPT3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_interrupt3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_interrupt3;
    BnkfPutM ("HXD:AET:SC:W_INTERRUPT", sizeof(int)*4, data);
  }
  {
    unsigned int data[16];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_pingain0;
    unsigned char hxd_aet_sc_w_pingain1;
    unsigned char hxd_aet_sc_w_pingain2;
    unsigned char hxd_aet_sc_w_pingain3;
    unsigned char hxd_aet_sc_w_pingain4;
    unsigned char hxd_aet_sc_w_pingain5;
    unsigned char hxd_aet_sc_w_pingain6;
    unsigned char hxd_aet_sc_w_pingain7;
    unsigned char hxd_aet_sc_w_pingain8;
    unsigned char hxd_aet_sc_w_pingain9;
    unsigned char hxd_aet_sc_w_pingain10;
    unsigned char hxd_aet_sc_w_pingain11;
    unsigned char hxd_aet_sc_w_pingain12;
    unsigned char hxd_aet_sc_w_pingain13;
    unsigned char hxd_aet_sc_w_pingain14;
    unsigned char hxd_aet_sc_w_pingain15;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_pingain0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_pingain1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_pingain2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_pingain3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN4], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain4, &anynul, &istat);
    data[4] = hxd_aet_sc_w_pingain4;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN5], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain5, &anynul, &istat);
    data[5] = hxd_aet_sc_w_pingain5;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN6], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain6, &anynul, &istat);
    data[6] = hxd_aet_sc_w_pingain6;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN7], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain7, &anynul, &istat);
    data[7] = hxd_aet_sc_w_pingain7;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN8], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain8, &anynul, &istat);
    data[8] = hxd_aet_sc_w_pingain8;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN9], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain9, &anynul, &istat);
    data[9] = hxd_aet_sc_w_pingain9;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN10], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain10, &anynul, &istat);
    data[10] = hxd_aet_sc_w_pingain10;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN11], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain11, &anynul, &istat);
    data[11] = hxd_aet_sc_w_pingain11;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN12], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain12, &anynul, &istat);
    data[12] = hxd_aet_sc_w_pingain12;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN13], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain13, &anynul, &istat);
    data[13] = hxd_aet_sc_w_pingain13;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN14], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain14, &anynul, &istat);
    data[14] = hxd_aet_sc_w_pingain14;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINGAIN15], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pingain15, &anynul, &istat);
    data[15] = hxd_aet_sc_w_pingain15;
    BnkfPutM ("HXD:AET:SC:W_PINGAIN", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_slwgain0;
    unsigned char hxd_aet_sc_w_slwgain1;
    unsigned char hxd_aet_sc_w_slwgain2;
    unsigned char hxd_aet_sc_w_slwgain3;
    unsigned char hxd_aet_sc_w_slwgain4;
    unsigned char hxd_aet_sc_w_slwgain5;
    unsigned char hxd_aet_sc_w_slwgain6;
    unsigned char hxd_aet_sc_w_slwgain7;
    unsigned char hxd_aet_sc_w_slwgain8;
    unsigned char hxd_aet_sc_w_slwgain9;
    unsigned char hxd_aet_sc_w_slwgain10;
    unsigned char hxd_aet_sc_w_slwgain11;
    unsigned char hxd_aet_sc_w_slwgain12;
    unsigned char hxd_aet_sc_w_slwgain13;
    unsigned char hxd_aet_sc_w_slwgain14;
    unsigned char hxd_aet_sc_w_slwgain15;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_slwgain0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_slwgain1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_slwgain2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_slwgain3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN4], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain4, &anynul, &istat);
    data[4] = hxd_aet_sc_w_slwgain4;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN5], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain5, &anynul, &istat);
    data[5] = hxd_aet_sc_w_slwgain5;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN6], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain6, &anynul, &istat);
    data[6] = hxd_aet_sc_w_slwgain6;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN7], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain7, &anynul, &istat);
    data[7] = hxd_aet_sc_w_slwgain7;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN8], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain8, &anynul, &istat);
    data[8] = hxd_aet_sc_w_slwgain8;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN9], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain9, &anynul, &istat);
    data[9] = hxd_aet_sc_w_slwgain9;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN10], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain10, &anynul, &istat);
    data[10] = hxd_aet_sc_w_slwgain10;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN11], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain11, &anynul, &istat);
    data[11] = hxd_aet_sc_w_slwgain11;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN12], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain12, &anynul, &istat);
    data[12] = hxd_aet_sc_w_slwgain12;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN13], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain13, &anynul, &istat);
    data[13] = hxd_aet_sc_w_slwgain13;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN14], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain14, &anynul, &istat);
    data[14] = hxd_aet_sc_w_slwgain14;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_SLWGAIN15], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_slwgain15, &anynul, &istat);
    data[15] = hxd_aet_sc_w_slwgain15;
    BnkfPutM ("HXD:AET:SC:W_SLWGAIN", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_fstgain0;
    unsigned char hxd_aet_sc_w_fstgain1;
    unsigned char hxd_aet_sc_w_fstgain2;
    unsigned char hxd_aet_sc_w_fstgain3;
    unsigned char hxd_aet_sc_w_fstgain4;
    unsigned char hxd_aet_sc_w_fstgain5;
    unsigned char hxd_aet_sc_w_fstgain6;
    unsigned char hxd_aet_sc_w_fstgain7;
    unsigned char hxd_aet_sc_w_fstgain8;
    unsigned char hxd_aet_sc_w_fstgain9;
    unsigned char hxd_aet_sc_w_fstgain10;
    unsigned char hxd_aet_sc_w_fstgain11;
    unsigned char hxd_aet_sc_w_fstgain12;
    unsigned char hxd_aet_sc_w_fstgain13;
    unsigned char hxd_aet_sc_w_fstgain14;
    unsigned char hxd_aet_sc_w_fstgain15;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_fstgain0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_fstgain1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_fstgain2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_fstgain3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN4], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain4, &anynul, &istat);
    data[4] = hxd_aet_sc_w_fstgain4;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN5], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain5, &anynul, &istat);
    data[5] = hxd_aet_sc_w_fstgain5;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN6], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain6, &anynul, &istat);
    data[6] = hxd_aet_sc_w_fstgain6;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN7], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain7, &anynul, &istat);
    data[7] = hxd_aet_sc_w_fstgain7;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN8], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain8, &anynul, &istat);
    data[8] = hxd_aet_sc_w_fstgain8;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN9], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain9, &anynul, &istat);
    data[9] = hxd_aet_sc_w_fstgain9;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN10], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain10, &anynul, &istat);
    data[10] = hxd_aet_sc_w_fstgain10;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN11], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain11, &anynul, &istat);
    data[11] = hxd_aet_sc_w_fstgain11;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN12], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain12, &anynul, &istat);
    data[12] = hxd_aet_sc_w_fstgain12;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN13], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain13, &anynul, &istat);
    data[13] = hxd_aet_sc_w_fstgain13;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN14], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain14, &anynul, &istat);
    data[14] = hxd_aet_sc_w_fstgain14;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_FSTGAIN15], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_fstgain15, &anynul, &istat);
    data[15] = hxd_aet_sc_w_fstgain15;
    BnkfPutM ("HXD:AET:SC:W_FSTGAIN", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_pmt_atten0;
    unsigned char hxd_aet_sc_w_pmt_atten1;
    unsigned char hxd_aet_sc_w_pmt_atten2;
    unsigned char hxd_aet_sc_w_pmt_atten3;
    unsigned char hxd_aet_sc_w_pmt_atten4;
    unsigned char hxd_aet_sc_w_pmt_atten5;
    unsigned char hxd_aet_sc_w_pmt_atten6;
    unsigned char hxd_aet_sc_w_pmt_atten7;
    unsigned char hxd_aet_sc_w_pmt_atten8;
    unsigned char hxd_aet_sc_w_pmt_atten9;
    unsigned char hxd_aet_sc_w_pmt_atten10;
    unsigned char hxd_aet_sc_w_pmt_atten11;
    unsigned char hxd_aet_sc_w_pmt_atten12;
    unsigned char hxd_aet_sc_w_pmt_atten13;
    unsigned char hxd_aet_sc_w_pmt_atten14;
    unsigned char hxd_aet_sc_w_pmt_atten15;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_pmt_atten0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_pmt_atten1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_pmt_atten2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_pmt_atten3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN4], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten4, &anynul, &istat);
    data[4] = hxd_aet_sc_w_pmt_atten4;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN5], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten5, &anynul, &istat);
    data[5] = hxd_aet_sc_w_pmt_atten5;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN6], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten6, &anynul, &istat);
    data[6] = hxd_aet_sc_w_pmt_atten6;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN7], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten7, &anynul, &istat);
    data[7] = hxd_aet_sc_w_pmt_atten7;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN8], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten8, &anynul, &istat);
    data[8] = hxd_aet_sc_w_pmt_atten8;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN9], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten9, &anynul, &istat);
    data[9] = hxd_aet_sc_w_pmt_atten9;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN10], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten10, &anynul, &istat);
    data[10] = hxd_aet_sc_w_pmt_atten10;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN11], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten11, &anynul, &istat);
    data[11] = hxd_aet_sc_w_pmt_atten11;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN12], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten12, &anynul, &istat);
    data[12] = hxd_aet_sc_w_pmt_atten12;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN13], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten13, &anynul, &istat);
    data[13] = hxd_aet_sc_w_pmt_atten13;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN14], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten14, &anynul, &istat);
    data[14] = hxd_aet_sc_w_pmt_atten14;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMT_ATTEN15], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmt_atten15, &anynul, &istat);
    data[15] = hxd_aet_sc_w_pmt_atten15;
    BnkfPutM ("HXD:AET:SC:W_PMT_ATTEN", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_pmtld0;
    unsigned char hxd_aet_sc_w_pmtld1;
    unsigned char hxd_aet_sc_w_pmtld2;
    unsigned char hxd_aet_sc_w_pmtld3;
    unsigned char hxd_aet_sc_w_pmtld4;
    unsigned char hxd_aet_sc_w_pmtld5;
    unsigned char hxd_aet_sc_w_pmtld6;
    unsigned char hxd_aet_sc_w_pmtld7;
    unsigned char hxd_aet_sc_w_pmtld8;
    unsigned char hxd_aet_sc_w_pmtld9;
    unsigned char hxd_aet_sc_w_pmtld10;
    unsigned char hxd_aet_sc_w_pmtld11;
    unsigned char hxd_aet_sc_w_pmtld12;
    unsigned char hxd_aet_sc_w_pmtld13;
    unsigned char hxd_aet_sc_w_pmtld14;
    unsigned char hxd_aet_sc_w_pmtld15;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_pmtld0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_pmtld1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_pmtld2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_pmtld3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD4], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld4, &anynul, &istat);
    data[4] = hxd_aet_sc_w_pmtld4;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD5], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld5, &anynul, &istat);
    data[5] = hxd_aet_sc_w_pmtld5;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD6], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld6, &anynul, &istat);
    data[6] = hxd_aet_sc_w_pmtld6;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD7], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld7, &anynul, &istat);
    data[7] = hxd_aet_sc_w_pmtld7;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD8], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld8, &anynul, &istat);
    data[8] = hxd_aet_sc_w_pmtld8;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD9], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld9, &anynul, &istat);
    data[9] = hxd_aet_sc_w_pmtld9;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD10], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld10, &anynul, &istat);
    data[10] = hxd_aet_sc_w_pmtld10;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD11], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld11, &anynul, &istat);
    data[11] = hxd_aet_sc_w_pmtld11;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD12], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld12, &anynul, &istat);
    data[12] = hxd_aet_sc_w_pmtld12;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD13], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld13, &anynul, &istat);
    data[13] = hxd_aet_sc_w_pmtld13;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD14], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld14, &anynul, &istat);
    data[14] = hxd_aet_sc_w_pmtld14;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTLD15], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtld15, &anynul, &istat);
    data[15] = hxd_aet_sc_w_pmtld15;
    BnkfPutM ("HXD:AET:SC:W_PMTLD", sizeof(int)*16, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_pinld0;
    unsigned char hxd_aet_sc_w_pinld1;
    unsigned char hxd_aet_sc_w_pinld2;
    unsigned char hxd_aet_sc_w_pinld3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINLD0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinld0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_pinld0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINLD1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinld1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_pinld1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINLD2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinld2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_pinld2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINLD3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinld3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_pinld3;
    BnkfPutM ("HXD:AET:SC:W_PINLD", sizeof(int)*4, data);
  }
  {
    unsigned int data[16];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_trgsw0;
    unsigned char hxd_aet_sc_w_trgsw1;
    unsigned char hxd_aet_sc_w_trgsw2;
    unsigned char hxd_aet_sc_w_trgsw3;
    unsigned char hxd_aet_sc_w_trgsw4;
    unsigned char hxd_aet_sc_w_trgsw5;
    unsigned char hxd_aet_sc_w_trgsw6;
    unsigned char hxd_aet_sc_w_trgsw7;
    unsigned char hxd_aet_sc_w_trgsw8;
    unsigned char hxd_aet_sc_w_trgsw9;
    unsigned char hxd_aet_sc_w_trgsw10;
    unsigned char hxd_aet_sc_w_trgsw11;
    unsigned char hxd_aet_sc_w_trgsw12;
    unsigned char hxd_aet_sc_w_trgsw13;
    unsigned char hxd_aet_sc_w_trgsw14;
    unsigned char hxd_aet_sc_w_trgsw15;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_trgsw0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_trgsw1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_trgsw2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_trgsw3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW4], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw4, &anynul, &istat);
    data[4] = hxd_aet_sc_w_trgsw4;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW5], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw5, &anynul, &istat);
    data[5] = hxd_aet_sc_w_trgsw5;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW6], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw6, &anynul, &istat);
    data[6] = hxd_aet_sc_w_trgsw6;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW7], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw7, &anynul, &istat);
    data[7] = hxd_aet_sc_w_trgsw7;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW8], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw8, &anynul, &istat);
    data[8] = hxd_aet_sc_w_trgsw8;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW9], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw9, &anynul, &istat);
    data[9] = hxd_aet_sc_w_trgsw9;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW10], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw10, &anynul, &istat);
    data[10] = hxd_aet_sc_w_trgsw10;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW11], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw11, &anynul, &istat);
    data[11] = hxd_aet_sc_w_trgsw11;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW12], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw12, &anynul, &istat);
    data[12] = hxd_aet_sc_w_trgsw12;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW13], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw13, &anynul, &istat);
    data[13] = hxd_aet_sc_w_trgsw13;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW14], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw14, &anynul, &istat);
    data[14] = hxd_aet_sc_w_trgsw14;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_TRGSW15], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_trgsw15, &anynul, &istat);
    data[15] = hxd_aet_sc_w_trgsw15;
    BnkfPutM ("HXD:AET:SC:W_TRGSW", sizeof(int)*16, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_pmtud0;
    unsigned char hxd_aet_sc_w_pmtud1;
    unsigned char hxd_aet_sc_w_pmtud2;
    unsigned char hxd_aet_sc_w_pmtud3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTUD0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtud0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_pmtud0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTUD1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtud1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_pmtud1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTUD2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtud2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_pmtud2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PMTUD3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pmtud3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_pmtud3;
    BnkfPutM ("HXD:AET:SC:W_PMTUD", sizeof(int)*4, data);
  }
  {
    unsigned int data[16];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_pinud0;
    unsigned char hxd_aet_sc_w_pinud1;
    unsigned char hxd_aet_sc_w_pinud2;
    unsigned char hxd_aet_sc_w_pinud3;
    unsigned char hxd_aet_sc_w_pinud4;
    unsigned char hxd_aet_sc_w_pinud5;
    unsigned char hxd_aet_sc_w_pinud6;
    unsigned char hxd_aet_sc_w_pinud7;
    unsigned char hxd_aet_sc_w_pinud8;
    unsigned char hxd_aet_sc_w_pinud9;
    unsigned char hxd_aet_sc_w_pinud10;
    unsigned char hxd_aet_sc_w_pinud11;
    unsigned char hxd_aet_sc_w_pinud12;
    unsigned char hxd_aet_sc_w_pinud13;
    unsigned char hxd_aet_sc_w_pinud14;
    unsigned char hxd_aet_sc_w_pinud15;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_pinud0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_pinud1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_pinud2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_pinud3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD4], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud4, &anynul, &istat);
    data[4] = hxd_aet_sc_w_pinud4;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD5], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud5, &anynul, &istat);
    data[5] = hxd_aet_sc_w_pinud5;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD6], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud6, &anynul, &istat);
    data[6] = hxd_aet_sc_w_pinud6;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD7], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud7, &anynul, &istat);
    data[7] = hxd_aet_sc_w_pinud7;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD8], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud8, &anynul, &istat);
    data[8] = hxd_aet_sc_w_pinud8;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD9], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud9, &anynul, &istat);
    data[9] = hxd_aet_sc_w_pinud9;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD10], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud10, &anynul, &istat);
    data[10] = hxd_aet_sc_w_pinud10;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD11], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud11, &anynul, &istat);
    data[11] = hxd_aet_sc_w_pinud11;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD12], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud12, &anynul, &istat);
    data[12] = hxd_aet_sc_w_pinud12;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD13], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud13, &anynul, &istat);
    data[13] = hxd_aet_sc_w_pinud13;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD14], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud14, &anynul, &istat);
    data[14] = hxd_aet_sc_w_pinud14;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PINUD15], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_pinud15, &anynul, &istat);
    data[15] = hxd_aet_sc_w_pinud15;
    BnkfPutM ("HXD:AET:SC:W_PINUD", sizeof(int)*16, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_hpt_wdt0;
    unsigned char hxd_aet_sc_w_hpt_wdt1;
    unsigned char hxd_aet_sc_w_hpt_wdt2;
    unsigned char hxd_aet_sc_w_hpt_wdt3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_HPT_WDT0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_hpt_wdt0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_hpt_wdt0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_HPT_WDT1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_hpt_wdt1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_hpt_wdt1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_HPT_WDT2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_hpt_wdt2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_hpt_wdt2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_HPT_WDT3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_hpt_wdt3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_hpt_wdt3;
    BnkfPutM ("HXD:AET:SC:W_HPT_WDT", sizeof(int)*4, data);
  }
  {
    unsigned int data[16];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_psd_lvl0;
    unsigned char hxd_aet_sc_w_psd_lvl1;
    unsigned char hxd_aet_sc_w_psd_lvl2;
    unsigned char hxd_aet_sc_w_psd_lvl3;
    unsigned char hxd_aet_sc_w_psd_lvl4;
    unsigned char hxd_aet_sc_w_psd_lvl5;
    unsigned char hxd_aet_sc_w_psd_lvl6;
    unsigned char hxd_aet_sc_w_psd_lvl7;
    unsigned char hxd_aet_sc_w_psd_lvl8;
    unsigned char hxd_aet_sc_w_psd_lvl9;
    unsigned char hxd_aet_sc_w_psd_lvl10;
    unsigned char hxd_aet_sc_w_psd_lvl11;
    unsigned char hxd_aet_sc_w_psd_lvl12;
    unsigned char hxd_aet_sc_w_psd_lvl13;
    unsigned char hxd_aet_sc_w_psd_lvl14;
    unsigned char hxd_aet_sc_w_psd_lvl15;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_psd_lvl0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_psd_lvl1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_psd_lvl2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_psd_lvl3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL4], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl4, &anynul, &istat);
    data[4] = hxd_aet_sc_w_psd_lvl4;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL5], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl5, &anynul, &istat);
    data[5] = hxd_aet_sc_w_psd_lvl5;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL6], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl6, &anynul, &istat);
    data[6] = hxd_aet_sc_w_psd_lvl6;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL7], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl7, &anynul, &istat);
    data[7] = hxd_aet_sc_w_psd_lvl7;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL8], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl8, &anynul, &istat);
    data[8] = hxd_aet_sc_w_psd_lvl8;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL9], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl9, &anynul, &istat);
    data[9] = hxd_aet_sc_w_psd_lvl9;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL10], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl10, &anynul, &istat);
    data[10] = hxd_aet_sc_w_psd_lvl10;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL11], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl11, &anynul, &istat);
    data[11] = hxd_aet_sc_w_psd_lvl11;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL12], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl12, &anynul, &istat);
    data[12] = hxd_aet_sc_w_psd_lvl12;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL13], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl13, &anynul, &istat);
    data[13] = hxd_aet_sc_w_psd_lvl13;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL14], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl14, &anynul, &istat);
    data[14] = hxd_aet_sc_w_psd_lvl14;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSD_LVL15], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psd_lvl15, &anynul, &istat);
    data[15] = hxd_aet_sc_w_psd_lvl15;
    BnkfPutM ("HXD:AET:SC:W_PSD_LVL", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_inhibit0;
    unsigned char hxd_aet_sc_w_inhibit1;
    unsigned char hxd_aet_sc_w_inhibit2;
    unsigned char hxd_aet_sc_w_inhibit3;
    unsigned char hxd_aet_sc_w_inhibit4;
    unsigned char hxd_aet_sc_w_inhibit5;
    unsigned char hxd_aet_sc_w_inhibit6;
    unsigned char hxd_aet_sc_w_inhibit7;
    unsigned char hxd_aet_sc_w_inhibit8;
    unsigned char hxd_aet_sc_w_inhibit9;
    unsigned char hxd_aet_sc_w_inhibit10;
    unsigned char hxd_aet_sc_w_inhibit11;
    unsigned char hxd_aet_sc_w_inhibit12;
    unsigned char hxd_aet_sc_w_inhibit13;
    unsigned char hxd_aet_sc_w_inhibit14;
    unsigned char hxd_aet_sc_w_inhibit15;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_inhibit0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_inhibit1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_inhibit2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_inhibit3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT4], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit4, &anynul, &istat);
    data[4] = hxd_aet_sc_w_inhibit4;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT5], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit5, &anynul, &istat);
    data[5] = hxd_aet_sc_w_inhibit5;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT6], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit6, &anynul, &istat);
    data[6] = hxd_aet_sc_w_inhibit6;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT7], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit7, &anynul, &istat);
    data[7] = hxd_aet_sc_w_inhibit7;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT8], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit8, &anynul, &istat);
    data[8] = hxd_aet_sc_w_inhibit8;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT9], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit9, &anynul, &istat);
    data[9] = hxd_aet_sc_w_inhibit9;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT10], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit10, &anynul, &istat);
    data[10] = hxd_aet_sc_w_inhibit10;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT11], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit11, &anynul, &istat);
    data[11] = hxd_aet_sc_w_inhibit11;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT12], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit12, &anynul, &istat);
    data[12] = hxd_aet_sc_w_inhibit12;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT13], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit13, &anynul, &istat);
    data[13] = hxd_aet_sc_w_inhibit13;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT14], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit14, &anynul, &istat);
    data[14] = hxd_aet_sc_w_inhibit14;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_INHIBIT15], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_inhibit15, &anynul, &istat);
    data[15] = hxd_aet_sc_w_inhibit15;
    BnkfPutM ("HXD:AET:SC:W_INHIBIT", sizeof(int)*16, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_w_psdoff0;
    unsigned char hxd_aet_sc_w_psdoff1;
    unsigned char hxd_aet_sc_w_psdoff2;
    unsigned char hxd_aet_sc_w_psdoff3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSDOFF0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psdoff0, &anynul, &istat);
    data[0] = hxd_aet_sc_w_psdoff0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSDOFF1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psdoff1, &anynul, &istat);
    data[1] = hxd_aet_sc_w_psdoff1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSDOFF2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psdoff2, &anynul, &istat);
    data[2] = hxd_aet_sc_w_psdoff2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_W_PSDOFF3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_w_psdoff3, &anynul, &istat);
    data[3] = hxd_aet_sc_w_psdoff3;
    BnkfPutM ("HXD:AET:SC:W_PSDOFF", sizeof(int)*4, data);
  }
  {
    unsigned int data[20];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_t_pmtgain0;
    unsigned char hxd_aet_sc_t_pmtgain1;
    unsigned char hxd_aet_sc_t_pmtgain2;
    unsigned char hxd_aet_sc_t_pmtgain3;
    unsigned char hxd_aet_sc_t_pmtgain4;
    unsigned char hxd_aet_sc_t_pmtgain5;
    unsigned char hxd_aet_sc_t_pmtgain6;
    unsigned char hxd_aet_sc_t_pmtgain7;
    unsigned char hxd_aet_sc_t_pmtgain8;
    unsigned char hxd_aet_sc_t_pmtgain9;
    unsigned char hxd_aet_sc_t_pmtgain10;
    unsigned char hxd_aet_sc_t_pmtgain11;
    unsigned char hxd_aet_sc_t_pmtgain12;
    unsigned char hxd_aet_sc_t_pmtgain13;
    unsigned char hxd_aet_sc_t_pmtgain14;
    unsigned char hxd_aet_sc_t_pmtgain15;
    unsigned char hxd_aet_sc_t_pmtgain16;
    unsigned char hxd_aet_sc_t_pmtgain17;
    unsigned char hxd_aet_sc_t_pmtgain18;
    unsigned char hxd_aet_sc_t_pmtgain19;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain0, &anynul, &istat);
    data[0] = hxd_aet_sc_t_pmtgain0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain1, &anynul, &istat);
    data[1] = hxd_aet_sc_t_pmtgain1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain2, &anynul, &istat);
    data[2] = hxd_aet_sc_t_pmtgain2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain3, &anynul, &istat);
    data[3] = hxd_aet_sc_t_pmtgain3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN4], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain4, &anynul, &istat);
    data[4] = hxd_aet_sc_t_pmtgain4;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN5], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain5, &anynul, &istat);
    data[5] = hxd_aet_sc_t_pmtgain5;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN6], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain6, &anynul, &istat);
    data[6] = hxd_aet_sc_t_pmtgain6;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN7], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain7, &anynul, &istat);
    data[7] = hxd_aet_sc_t_pmtgain7;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN8], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain8, &anynul, &istat);
    data[8] = hxd_aet_sc_t_pmtgain8;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN9], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain9, &anynul, &istat);
    data[9] = hxd_aet_sc_t_pmtgain9;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN10], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain10, &anynul, &istat);
    data[10] = hxd_aet_sc_t_pmtgain10;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN11], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain11, &anynul, &istat);
    data[11] = hxd_aet_sc_t_pmtgain11;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN12], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain12, &anynul, &istat);
    data[12] = hxd_aet_sc_t_pmtgain12;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN13], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain13, &anynul, &istat);
    data[13] = hxd_aet_sc_t_pmtgain13;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN14], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain14, &anynul, &istat);
    data[14] = hxd_aet_sc_t_pmtgain14;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN15], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain15, &anynul, &istat);
    data[15] = hxd_aet_sc_t_pmtgain15;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN16], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain16, &anynul, &istat);
    data[16] = hxd_aet_sc_t_pmtgain16;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN17], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain17, &anynul, &istat);
    data[17] = hxd_aet_sc_t_pmtgain17;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN18], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain18, &anynul, &istat);
    data[18] = hxd_aet_sc_t_pmtgain18;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTGAIN19], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtgain19, &anynul, &istat);
    data[19] = hxd_aet_sc_t_pmtgain19;
    BnkfPutM ("HXD:AET:SC:T_PMTGAIN", sizeof(int)*20, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_t_pmtld0;
    unsigned char hxd_aet_sc_t_pmtld1;
    unsigned char hxd_aet_sc_t_pmtld2;
    unsigned char hxd_aet_sc_t_pmtld3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTLD0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtld0, &anynul, &istat);
    data[0] = hxd_aet_sc_t_pmtld0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTLD1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtld1, &anynul, &istat);
    data[1] = hxd_aet_sc_t_pmtld1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTLD2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtld2, &anynul, &istat);
    data[2] = hxd_aet_sc_t_pmtld2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTLD3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtld3, &anynul, &istat);
    data[3] = hxd_aet_sc_t_pmtld3;
    BnkfPutM ("HXD:AET:SC:T_PMTLD", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_t_pmtldsum0;
    unsigned char hxd_aet_sc_t_pmtldsum1;
    unsigned char hxd_aet_sc_t_pmtldsum2;
    unsigned char hxd_aet_sc_t_pmtldsum3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTLDSUM0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtldsum0, &anynul, &istat);
    data[0] = hxd_aet_sc_t_pmtldsum0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTLDSUM1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtldsum1, &anynul, &istat);
    data[1] = hxd_aet_sc_t_pmtldsum1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTLDSUM2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtldsum2, &anynul, &istat);
    data[2] = hxd_aet_sc_t_pmtldsum2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTLDSUM3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtldsum3, &anynul, &istat);
    data[3] = hxd_aet_sc_t_pmtldsum3;
    BnkfPutM ("HXD:AET:SC:T_PMTLDSUM", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_t_trgsw0;
    unsigned char hxd_aet_sc_t_trgsw1;
    unsigned char hxd_aet_sc_t_trgsw2;
    unsigned char hxd_aet_sc_t_trgsw3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_TRGSW0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_trgsw0, &anynul, &istat);
    data[0] = hxd_aet_sc_t_trgsw0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_TRGSW1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_trgsw1, &anynul, &istat);
    data[1] = hxd_aet_sc_t_trgsw1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_TRGSW2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_trgsw2, &anynul, &istat);
    data[2] = hxd_aet_sc_t_trgsw2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_TRGSW3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_trgsw3, &anynul, &istat);
    data[3] = hxd_aet_sc_t_trgsw3;
    BnkfPutM ("HXD:AET:SC:T_TRGSW", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_t_pmtudsw0;
    unsigned char hxd_aet_sc_t_pmtudsw1;
    unsigned char hxd_aet_sc_t_pmtudsw2;
    unsigned char hxd_aet_sc_t_pmtudsw3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTUDSW0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtudsw0, &anynul, &istat);
    data[0] = hxd_aet_sc_t_pmtudsw0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTUDSW1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtudsw1, &anynul, &istat);
    data[1] = hxd_aet_sc_t_pmtudsw1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTUDSW2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtudsw2, &anynul, &istat);
    data[2] = hxd_aet_sc_t_pmtudsw2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_PMTUDSW3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_pmtudsw3, &anynul, &istat);
    data[3] = hxd_aet_sc_t_pmtudsw3;
    BnkfPutM ("HXD:AET:SC:T_PMTUDSW", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_t_hpt_wdt0;
    unsigned char hxd_aet_sc_t_hpt_wdt1;
    unsigned char hxd_aet_sc_t_hpt_wdt2;
    unsigned char hxd_aet_sc_t_hpt_wdt3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_HPT_WDT0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_hpt_wdt0, &anynul, &istat);
    data[0] = hxd_aet_sc_t_hpt_wdt0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_HPT_WDT1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_hpt_wdt1, &anynul, &istat);
    data[1] = hxd_aet_sc_t_hpt_wdt1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_HPT_WDT2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_hpt_wdt2, &anynul, &istat);
    data[2] = hxd_aet_sc_t_hpt_wdt2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_HPT_WDT3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_hpt_wdt3, &anynul, &istat);
    data[3] = hxd_aet_sc_t_hpt_wdt3;
    BnkfPutM ("HXD:AET:SC:T_HPT_WDT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_t_gbth_tim0;
    unsigned char hxd_aet_sc_t_gbth_tim1;
    unsigned char hxd_aet_sc_t_gbth_tim2;
    unsigned char hxd_aet_sc_t_gbth_tim3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBTH_TIM0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbth_tim0, &anynul, &istat);
    data[0] = hxd_aet_sc_t_gbth_tim0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBTH_TIM1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbth_tim1, &anynul, &istat);
    data[1] = hxd_aet_sc_t_gbth_tim1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBTH_TIM2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbth_tim2, &anynul, &istat);
    data[2] = hxd_aet_sc_t_gbth_tim2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBTH_TIM3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbth_tim3, &anynul, &istat);
    data[3] = hxd_aet_sc_t_gbth_tim3;
    BnkfPutM ("HXD:AET:SC:T_GBTH_TIM", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_t_gbena0;
    unsigned char hxd_aet_sc_t_gbena1;
    unsigned char hxd_aet_sc_t_gbena2;
    unsigned char hxd_aet_sc_t_gbena3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBENA0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbena0, &anynul, &istat);
    data[0] = hxd_aet_sc_t_gbena0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBENA1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbena1, &anynul, &istat);
    data[1] = hxd_aet_sc_t_gbena1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBENA2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbena2, &anynul, &istat);
    data[2] = hxd_aet_sc_t_gbena2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBENA3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbena3, &anynul, &istat);
    data[3] = hxd_aet_sc_t_gbena3;
    BnkfPutM ("HXD:AET:SC:T_GBENA", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_t_gbtrg_de0;
    unsigned char hxd_aet_sc_t_gbtrg_de1;
    unsigned char hxd_aet_sc_t_gbtrg_de2;
    unsigned char hxd_aet_sc_t_gbtrg_de3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBTRG_DE0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbtrg_de0, &anynul, &istat);
    data[0] = hxd_aet_sc_t_gbtrg_de0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBTRG_DE1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbtrg_de1, &anynul, &istat);
    data[1] = hxd_aet_sc_t_gbtrg_de1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBTRG_DE2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbtrg_de2, &anynul, &istat);
    data[2] = hxd_aet_sc_t_gbtrg_de2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBTRG_DE3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbtrg_de3, &anynul, &istat);
    data[3] = hxd_aet_sc_t_gbtrg_de3;
    BnkfPutM ("HXD:AET:SC:T_GBTRG_DE", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_t_gbtrg_sd0;
    unsigned char hxd_aet_sc_t_gbtrg_sd1;
    unsigned char hxd_aet_sc_t_gbtrg_sd2;
    unsigned char hxd_aet_sc_t_gbtrg_sd3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBTRG_SD0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbtrg_sd0, &anynul, &istat);
    data[0] = hxd_aet_sc_t_gbtrg_sd0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBTRG_SD1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbtrg_sd1, &anynul, &istat);
    data[1] = hxd_aet_sc_t_gbtrg_sd1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBTRG_SD2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbtrg_sd2, &anynul, &istat);
    data[2] = hxd_aet_sc_t_gbtrg_sd2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GBTRG_SD3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gbtrg_sd3, &anynul, &istat);
    data[3] = hxd_aet_sc_t_gbtrg_sd3;
    BnkfPutM ("HXD:AET:SC:T_GBTRG_SD", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_t_gb_thres0;
    unsigned char hxd_aet_sc_t_gb_thres1;
    unsigned char hxd_aet_sc_t_gb_thres2;
    unsigned char hxd_aet_sc_t_gb_thres3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GB_THRES0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gb_thres0, &anynul, &istat);
    data[0] = hxd_aet_sc_t_gb_thres0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GB_THRES1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gb_thres1, &anynul, &istat);
    data[1] = hxd_aet_sc_t_gb_thres1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GB_THRES2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gb_thres2, &anynul, &istat);
    data[2] = hxd_aet_sc_t_gb_thres2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_GB_THRES3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_gb_thres3, &anynul, &istat);
    data[3] = hxd_aet_sc_t_gb_thres3;
    BnkfPutM ("HXD:AET:SC:T_GB_THRES", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_sc_t_rbm_trg0;
    unsigned char hxd_aet_sc_t_rbm_trg1;
    unsigned char hxd_aet_sc_t_rbm_trg2;
    unsigned char hxd_aet_sc_t_rbm_trg3;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_RBM_TRG0], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_rbm_trg0, &anynul, &istat);
    data[0] = hxd_aet_sc_t_rbm_trg0;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_RBM_TRG1], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_rbm_trg1, &anynul, &istat);
    data[1] = hxd_aet_sc_t_rbm_trg1;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_RBM_TRG2], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_rbm_trg2, &anynul, &istat);
    data[2] = hxd_aet_sc_t_rbm_trg2;
    fits_read_col_byt(fp, colnum[HXD_AET_SC_T_RBM_TRG3], irow, firstelem,
                      nelements, nulval, &hxd_aet_sc_t_rbm_trg3, &anynul, &istat);
    data[3] = hxd_aet_sc_t_rbm_trg3;
    BnkfPutM ("HXD:AET:SC:T_RBM_TRG", sizeof(int)*4, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
