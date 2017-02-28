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
  HXD_PST_PI_VER,
  HXD_PST_ERR_CNT,
  HXD_PST_RET_ERR,
  HXD_PST_ERR_SYS,
  HXD_PST_ERR_PI,
  HXD_PST_ERR_PICD,
  HXD_PST_WEV_TOUT,
  HXD_PST_GST_CALL,
  HXD_PST_GST_PARA,
  HXD_PST_PST_CALL,
  HXD_PST_PST_DISC,
  HXD_PST_PPS_CALL,
  HXD_PST_GCM_CALL,
  HXD_PST_PCM_CALL,
  HXD_PST_GWE_CALL,
  HXD_PST_GWE_SIZE,
  HXD_PST_PWE_RJCT,
  HXD_PST_PWE_CALL,
  HXD_PST_GWE_LNER,
  HXD_PST_GMN_CALL,
  HXD_PST_GMN_CNT,
  HXD_PST_PMN_CALL,
  HXD_PST_PMN_DISC,
  HXD_PST_GHK_CALL,
  HXD_PST_GHK_CNT,
  HXD_PST_PHK_CALL,
  HXD_PST_PHK_DISC,
  HXD_PST_GBST_CALL,
  HXD_PST_GBST_CNT,
  HXD_PST_PBST_CALL,
  HXD_PST_PBST_DISC,
  HXD_PST_BST_CNT0,
  HXD_PST_BST_CNT1,
  HXD_PST_BST_CNT2,
  HXD_PST_BST_CNT3,
  HXD_PST_BST_CURMD,
  HXD_PST_CMD_RJCT,
  HXD_PST_GTR_CALL,
  HXD_PST_GTR_LNER,
  HXD_PST_PTR_CALL,
  HXD_PST_PTR_DISC,
  HXD_PST_TRN_CNT0,
  HXD_PST_TRN_CNT1,
  HXD_PST_TRN_CNT2,
  HXD_PST_TRN_CNT3,
  HXD_PST_GB_FLG,
  HXD_PST_GB_FRZ,
  HXD_PST_PHSF_SEQ,
  HXD_PST_PHSF_CALL,
  HXD_PST_PHSF_DISC,
  HXD_PST_PHSF_C0,
  HXD_PST_PHSF_C1,
  HXD_PST_PHSF_PS0,
  HXD_PST_PHSF_PS1,
  HXD_PST_UNI_DIS0,
  HXD_PST_UNI_DIS1,
  HXD_PST_UNI_DIS2,
  HXD_PST_UNI_DIS3,
  HXD_PST_2D_DIS0,
  HXD_PST_2D_DIS1,
  HXD_PST_2D_DIS2,
  HXD_PST_2D_DIS3,
  HXD_PST_2D_DIS4,
  HXD_PST_2D_DIS5,
  HXD_PST_2D_DIS6,
  HXD_PST_2D_DIS7,
  HXD_PST_2D_DIS8,
  HXD_PST_2D_DIS9,
  HXD_PST_2D_DIS10,
  HXD_PST_2D_DIS11,
  HXD_PST_2D_DIS12,
  HXD_PST_2D_DIS13,
  HXD_PST_2D_DIS14,
  HXD_PST_2D_DIS15,
  HXD_PST_FL_DIS0,
  HXD_PST_EVSEL_FL_DIS1,
  HXD_PST_FL_DIS2,
  HXD_PST_FL_DIS3,
  HXD_PST_FL_DIS4,
  HXD_PST_FL_DIS5,
  HXD_PST_FL_DIS6,
  HXD_PST_FLG_DIS7,
  HXD_PST_FL_DIS8,
  HXD_PST_FL_DIS9,
  HXD_PST_FL_DIS10,
  HXD_PST_FL_DIS11,
  HXD_PST_FL_DIS12,
  HXD_PST_FL_DIS13,
  HXD_PST_FL_DIS14,
  HXD_PST_FL_DIS15,
  HXD_PST_TR_DIS0,
  HXD_PST_TR_DIS1,
  HXD_PST_TR_DIS2,
  HXD_PST_TR_DIS3,
  HXD_PST_TR_DIS4,
  HXD_PST_TR_DIS5,
  HXD_PST_TR_DIS6,
  HXD_PST_TR_DIS7,
  HXD_PST_TR_DIS8,
  HXD_PST_TR_DIS9,
  HXD_PST_TR_DIS10,
  HXD_PST_TR_DIS11,
  HXD_PST_TR_DIS12,
  HXD_PST_TR_DIS13,
  HXD_PST_TR_DIS14,
  HXD_PST_TR_DIS15,
  HXD_PST_HT_DIS0,
  HXD_PST_HT_DIS1,
  HXD_PST_HT_DIS2,
  HXD_PST_HT_DIS3,
  HXD_PST_HT_DIS4,
  HXD_PST_HT_DIS5,
  HXD_PST_HT_DIS6,
  HXD_PST_HT_DIS7,
  HXD_PST_HT_DIS8,
  HXD_PST_HT_DIS9,
  HXD_PST_HT_DIS10,
  HXD_PST_HT_DIS11,
  HXD_PST_HT_DIS12,
  HXD_PST_HT_DIS13,
  HXD_PST_HT_DIS14,
  HXD_PST_HT_DIS15,
  HXD_PST_HP_DIS0,
  HXD_PST_HP_DIS1,
  HXD_PST_HP_DIS2,
  HXD_PST_HP_DIS3,
  HXD_PST_HP_DIS4,
  HXD_PST_HP_DIS5,
  HXD_PST_HP_DIS6,
  HXD_PST_HP_DIS7,
  HXD_PST_HP_DIS8,
  HXD_PST_HP_DIS9,
  HXD_PST_HP_DIS10,
  HXD_PST_HP_DIS11,
  HXD_PST_HP_DIS12,
  HXD_PST_HP_DIS13,
  HXD_PST_HP_DIS14,
  HXD_PST_HP_DIS15,
  HXD_PST_DT_DIS0,
  HXD_PST_DT_DIS1,
  HXD_PST_DT_DIS2,
  HXD_PST_DT_DIS3,
  HXD_PST_DT_DIS4,
  HXD_PST_DT_DIS5,
  HXD_PST_DT_DIS6,
  HXD_PST_DT_DIS7,
  HXD_PST_DT_DIS8,
  HXD_PST_DT_DIS9,
  HXD_PST_DT_DIS10,
  HXD_PST_DT_DIS11,
  HXD_PST_DT_DIS12,
  HXD_PST_DT_DIS13,
  HXD_PST_DT_DIS14,
  HXD_PST_DT_DIS15,
  HXD_PST_DT_SUM0,
  HXD_PST_DT_SUM1,
  HXD_PST_DT_SUM2,
  HXD_PST_DT_SUM3,
  HXD_PST_DT_SUM4,
  HXD_PST_DT_SUM5,
  HXD_PST_DT_SUM6,
  HXD_PST_DT_SUM7,
  HXD_PST_DT_SUM8,
  HXD_PST_DT_SUM9,
  HXD_PST_DT_SUM10,
  HXD_PST_DT_SUM11,
  HXD_PST_DT_SUM12,
  HXD_PST_DT_SUM13,
  HXD_PST_DT_SUM14,
  HXD_PST_DT_SUM15,
  HXD_PST_PL_DIS0,
  HXD_PST_PL_DIS1,
  HXD_PST_PL_DIS2,
  HXD_PST_PL_DIS3,
  HXD_PST_PL_DIS4,
  HXD_PST_PL_DIS5,
  HXD_PST_PL_DIS6,
  HXD_PST_PL_DIS7,
  HXD_PST_PL_DIS8,
  HXD_PST_PL_DIS9,
  HXD_PST_PL_DIS10,
  HXD_PST_PL_DIS11,
  HXD_PST_PL_DIS12,
  HXD_PST_PL_DIS13,
  HXD_PST_PL_DIS14,
  HXD_PST_PL_DIS15,
  HXD_PST_PT_DIS0,
  HXD_PST_PT_DIS1,
  HXD_PST_PT_DIS2,
  HXD_PST_PT_DIS3,
  HXD_PST_PT_DIS4,
  HXD_PST_PT_DIS5,
  HXD_PST_PT_DIS6,
  HXD_PST_PT_DIS7,
  HXD_PST_PT_DIS8,
  HXD_PST_PT_DIS9,
  HXD_PST_PT_DIS10,
  HXD_PST_PT_DIS11,
  HXD_PST_PT_DIS12,
  HXD_PST_PT_DIS13,
  HXD_PST_PT_DIS14,
  HXD_PST_PT_DIS15,
  HXD_PST_PREV_W0,
  HXD_PST_PREV_W1,
  HXD_PST_PREV_W2,
  HXD_PST_PREV_W3,
  HXD_PST_PREV_W4,
  HXD_PST_PREV_W5,
  HXD_PST_PREV_W6,
  HXD_PST_PREV_W7,
  HXD_PST_PREV_W8,
  HXD_PST_PREV_W9,
  HXD_PST_PREV_W10,
  HXD_PST_PREV_W11,
  HXD_PST_PREV_W12,
  HXD_PST_PREV_W13,
  HXD_PST_PREV_W14,
  HXD_PST_PREV_W15,
  HXD_PST_PREV_N0,
  HXD_PST_PREV_N1,
  HXD_PST_PREV_N2,
  HXD_PST_PREV_N3,
  HXD_PST_PREV_N4,
  HXD_PST_PREV_N5,
  HXD_PST_PREV_N6,
  HXD_PST_PREV_N7,
  HXD_PST_PREV_N8,
  HXD_PST_PREV_N9,
  HXD_PST_PREV_N10,
  HXD_PST_PREV_N11,
  HXD_PST_PREV_N12,
  HXD_PST_PREV_N13,
  HXD_PST_PREV_N14,
  HXD_PST_PREV_N15,
  HXD_PST_PREV_I0,
  HXD_PST_PREV_I1,
  HXD_PST_PREV_I2,
  HXD_PST_PREV_I3,
  HXD_PST_PREV_I4,
  HXD_PST_PREV_I5,
  HXD_PST_PREV_I6,
  HXD_PST_PREV_I7,
  HXD_PST_PREV_I8,
  HXD_PST_PREV_I9,
  HXD_PST_PREV_I10,
  HXD_PST_PREV_I11,
  HXD_PST_PREV_I12,
  HXD_PST_PREV_I13,
  HXD_PST_PREV_I14,
  HXD_PST_PREV_I15,
  HXD_PST_DT_PS0,
  HXD_PST_DT_PS1,
  HXD_PST_DT_PS2,
  HXD_PST_DT_PS3,
  HXD_PST_DT_PS4,
  HXD_PST_DT_PS5,
  HXD_PST_DT_PS6,
  HXD_PST_DT_PS7,
  HXD_PST_DT_PS8,
  HXD_PST_DT_PS9,
  HXD_PST_DT_PS10,
  HXD_PST_DT_PS11,
  HXD_PST_DT_PS12,
  HXD_PST_DT_PS13,
  HXD_PST_DT_PS14,
  HXD_PST_DT_PS15,
  HXD_PST_ACU_TI0,
  HXD_PST_ACU_TI1,
  HXD_PST_ACU_TI2,
  HXD_PST_ACU_TI3,
  HXD_PST_ACU_TI4,
  HXD_PST_ACU_TI5,
  HXD_PST_ACU_TI6,
  HXD_PST_ACU_TI7,
  HXD_PST_ACU_TI8,
  HXD_PST_ACU_TI9,
  HXD_PST_ACU_TI10,
  HXD_PST_ACU_TI11,
  HXD_PST_ACU_TI12,
  HXD_PST_ACU_TI13,
  HXD_PST_ACU_TI14,
  HXD_PST_ACU_TI15,
  HXD_PST_BST_REQ0,
  HXD_PST_BST_REQ1,
  HXD_PST_BST_REQ2,
  HXD_PST_BST_REQ3,
  HXD_PST_TPUTMOD0,
  HXD_PST_TPUTMOD1,
  HXD_PST_TPUTMOD2,
  HXD_PST_TPUTMOD3,
  HXD_PST_BST_TRGF,
  HXD_PST_BST_PH_F,
  HXD_PST_BST_WLDF,
  HXD_PST_BST_PSEF,
  HXD_PST_GB_F_PAT,
  HXD_PST_GB_ENA,
};

static char pname[] = "HXDHKFitsReadPST";

static int colnum[296];
static int time_colnum;

void
HXDHKFitsReadPST_init()
{
  BnkDef( "HXD:PST:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:PST:PI_VERSION", sizeof(int) );
  BnkDef( "HXD:PST:ERROR_COUNT", sizeof(int) );
  BnkDef( "HXD:PST:RETURNED_ERROR", sizeof(int) );
  BnkDef( "HXD:PST:ERR_SYS", sizeof(int) );
  BnkDef( "HXD:PST:ERR_PI", sizeof(int) );
  BnkDef( "HXD:PST:ERR_PICOD", sizeof(int) );
  BnkDef( "HXD:PST:WEVT_TIMEOUT", sizeof(int) );
  BnkDef( "HXD:PST:GSTS_CALL", sizeof(int) );
  BnkDef( "HXD:PST:GSTS_PARAM", sizeof(int) );
  BnkDef( "HXD:PST:PSTS_CALL", sizeof(int) );
  BnkDef( "HXD:PST:PSTS_DISCARD", sizeof(int) );
  BnkDef( "HXD:PST:PPST_CALL", sizeof(int) );
  BnkDef( "HXD:PST:GCMD_CALL", sizeof(int) );
  BnkDef( "HXD:PST:PCMD_CALL", sizeof(int) );
  BnkDef( "HXD:PST:GWEL_CALL", sizeof(int) );
  BnkDef( "HXD:PST:GWEL_SIZE", sizeof(int) );
  BnkDef( "HXD:PST:PWEL_OVERWRITE", sizeof(int) );
  BnkDef( "HXD:PST:PWEL_CALL", sizeof(int) );
  BnkDef( "HXD:PST:GWEL_LENERROR", sizeof(int) );
  BnkDef( "HXD:PST:GMON_CALL", sizeof(int) );
  BnkDef( "HXD:PST:GMON_COUNT", sizeof(int) );
  BnkDef( "HXD:PST:PMON_CALL", sizeof(int) );
  BnkDef( "HXD:PST:PMON_DISCARD", sizeof(int) );
  BnkDef( "HXD:PST:GHK_CALL", sizeof(int) );
  BnkDef( "HXD:PST:GHK_COUNT", sizeof(int) );
  BnkDef( "HXD:PST:PHK_CALL", sizeof(int) );
  BnkDef( "HXD:PST:PHK_DISCARD", sizeof(int) );
  BnkDef( "HXD:PST:GBST_CALL", sizeof(int) );
  BnkDef( "HXD:PST:GBST_COUNT", sizeof(int) );
  BnkDef( "HXD:PST:PBST_CALL", sizeof(int) );
  BnkDef( "HXD:PST:PBST_DISCARD", sizeof(int) );
  BnkDef( "HXD:PST:BST_CNT_TPU", sizeof(int)*4 );
  BnkDef( "HXD:PST:BST_CURR_BD", sizeof(int) );
  BnkDef( "HXD:PST:CMD_RJT_CNT", sizeof(int) );
  BnkDef( "HXD:PST:GTRN_CALL", sizeof(int) );
  BnkDef( "HXD:PST:GTRN_LENERROR", sizeof(int) );
  BnkDef( "HXD:PST:PTRN_CALL", sizeof(int) );
  BnkDef( "HXD:PST:PTRN_DISCARD", sizeof(int) );
  BnkDef( "HXD:PST:TRN_COUNT", sizeof(int)*4 );
  BnkDef( "HXD:PST:GB_FLG", sizeof(int) );
  BnkDef( "HXD:PST:GB_FRZ", sizeof(int) );
  BnkDef( "HXD:PST:PHSF_SEQ", sizeof(int) );
  BnkDef( "HXD:PST:PHSF_CALL", sizeof(int) );
  BnkDef( "HXD:PST:PHSF_DISCARD", sizeof(int) );
  BnkDef( "HXD:PST:PHSF_COUNTER", sizeof(int)*2 );
  BnkDef( "HXD:PST:PHSF_PSEUD", sizeof(int)*2 );
  BnkDef( "HXD:PST:EVSEL_UNI_DISCD", sizeof(int)*4 );
  BnkDef( "HXD:PST:EVSEL_2D_DISCD", sizeof(int)*16 );
  BnkDef( "HXD:PST:EVSEL_FLG_DISCD", sizeof(int)*16 );
  BnkDef( "HXD:PST:EVSEL_TRG_DISCD", sizeof(int)*16 );
  BnkDef( "HXD:PST:EVSEL_HIT_DISCD", sizeof(int)*16 );
  BnkDef( "HXD:PST:EVSEL_HITPIN_DISCD", sizeof(int)*16 );
  BnkDef( "HXD:PST:EVSEL_DT_DISCD", sizeof(int)*16 );
  BnkDef( "HXD:PST:EVSEL_DT_SUMT", sizeof(int)*16 );
  BnkDef( "HXD:PST:EVSEL_PINLD_DISCD", sizeof(int)*16 );
  BnkDef( "HXD:PST:EVSEL_PINTRG_DISCD", sizeof(int)*16 );
  BnkDef( "HXD:PST:PREV_PWEL", sizeof(int)*16 );
  BnkDef( "HXD:PST:PREV_NON_EXIST", sizeof(int)*16 );
  BnkDef( "HXD:PST:PREV_INVAL", sizeof(int)*16 );
  BnkDef( "HXD:PST:EVSEL_DT_PSEUD", sizeof(int)*16 );
  BnkDef( "HXD:PST:EVSL_ACU_CNT", sizeof(int)*16 );
  BnkDef( "HXD:PST:BST_RQUEST_CNT", sizeof(int)*4 );
  BnkDef( "HXD:PST:TPU_TIM_MOD", sizeof(int)*4 );
  BnkDef( "HXD:PST:BST_TRG_FLG", sizeof(int) );
  BnkDef( "HXD:PST:BST_PH_FLG", sizeof(int) );
  BnkDef( "HXD:PST:BST_WLD_FLG", sizeof(int) );
  BnkDef( "HXD:PST:BST_PSE_FLG", sizeof(int) );
  BnkDef( "HXD:PST:GB_FLG_PAT", sizeof(int) );
  BnkDef( "HXD:PST:GB_ENA", sizeof(int) );
  
}


int
HXDHKFitsReadPST_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, PST, &hdutype, &istat );
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
  
  if( fits_get_colnum(fp, casesen, "HXD_PST_PI_VER",
                      &colnum[HXD_PST_PI_VER], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PI_VER') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ERR_CNT",
                      &colnum[HXD_PST_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_RET_ERR",
                      &colnum[HXD_PST_RET_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_RET_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ERR_SYS",
                      &colnum[HXD_PST_ERR_SYS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ERR_SYS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ERR_PI",
                      &colnum[HXD_PST_ERR_PI], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ERR_PI') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ERR_PICD",
                      &colnum[HXD_PST_ERR_PICD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ERR_PICD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_WEV_TOUT",
                      &colnum[HXD_PST_WEV_TOUT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_WEV_TOUT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GST_CALL",
                      &colnum[HXD_PST_GST_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GST_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GST_PARA",
                      &colnum[HXD_PST_GST_PARA], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GST_PARA') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PST_CALL",
                      &colnum[HXD_PST_PST_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PST_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PST_DISC",
                      &colnum[HXD_PST_PST_DISC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PST_DISC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PPS_CALL",
                      &colnum[HXD_PST_PPS_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PPS_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GCM_CALL",
                      &colnum[HXD_PST_GCM_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GCM_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PCM_CALL",
                      &colnum[HXD_PST_PCM_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PCM_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GWE_CALL",
                      &colnum[HXD_PST_GWE_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GWE_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GWE_SIZE",
                      &colnum[HXD_PST_GWE_SIZE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GWE_SIZE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PWE_RJCT",
                      &colnum[HXD_PST_PWE_RJCT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PWE_RJCT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PWE_CALL",
                      &colnum[HXD_PST_PWE_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PWE_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GWE_LNER",
                      &colnum[HXD_PST_GWE_LNER], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GWE_LNER') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GMN_CALL",
                      &colnum[HXD_PST_GMN_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GMN_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GMN_CNT",
                      &colnum[HXD_PST_GMN_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GMN_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PMN_CALL",
                      &colnum[HXD_PST_PMN_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PMN_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PMN_DISC",
                      &colnum[HXD_PST_PMN_DISC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PMN_DISC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GHK_CALL",
                      &colnum[HXD_PST_GHK_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GHK_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GHK_CNT",
                      &colnum[HXD_PST_GHK_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GHK_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PHK_CALL",
                      &colnum[HXD_PST_PHK_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PHK_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PHK_DISC",
                      &colnum[HXD_PST_PHK_DISC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PHK_DISC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GBST_CALL",
                      &colnum[HXD_PST_GBST_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GBST_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GBST_CNT",
                      &colnum[HXD_PST_GBST_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GBST_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PBST_CALL",
                      &colnum[HXD_PST_PBST_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PBST_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PBST_DISC",
                      &colnum[HXD_PST_PBST_DISC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PBST_DISC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_CNT0",
                      &colnum[HXD_PST_BST_CNT0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_CNT0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_CNT1",
                      &colnum[HXD_PST_BST_CNT1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_CNT1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_CNT2",
                      &colnum[HXD_PST_BST_CNT2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_CNT2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_CNT3",
                      &colnum[HXD_PST_BST_CNT3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_CNT3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_CURMD",
                      &colnum[HXD_PST_BST_CURMD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_CURMD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_CMD_RJCT",
                      &colnum[HXD_PST_CMD_RJCT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_CMD_RJCT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GTR_CALL",
                      &colnum[HXD_PST_GTR_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GTR_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GTR_LNER",
                      &colnum[HXD_PST_GTR_LNER], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GTR_LNER') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PTR_CALL",
                      &colnum[HXD_PST_PTR_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PTR_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PTR_DISC",
                      &colnum[HXD_PST_PTR_DISC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PTR_DISC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TRN_CNT0",
                      &colnum[HXD_PST_TRN_CNT0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TRN_CNT0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TRN_CNT1",
                      &colnum[HXD_PST_TRN_CNT1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TRN_CNT1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TRN_CNT2",
                      &colnum[HXD_PST_TRN_CNT2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TRN_CNT2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TRN_CNT3",
                      &colnum[HXD_PST_TRN_CNT3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TRN_CNT3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GB_FLG",
                      &colnum[HXD_PST_GB_FLG], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GB_FLG') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GB_FRZ",
                      &colnum[HXD_PST_GB_FRZ], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GB_FRZ') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PHSF_SEQ",
                      &colnum[HXD_PST_PHSF_SEQ], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PHSF_SEQ') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PHSF_CALL",
                      &colnum[HXD_PST_PHSF_CALL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PHSF_CALL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PHSF_DISC",
                      &colnum[HXD_PST_PHSF_DISC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PHSF_DISC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PHSF_C0",
                      &colnum[HXD_PST_PHSF_C0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PHSF_C0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PHSF_C1",
                      &colnum[HXD_PST_PHSF_C1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PHSF_C1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PHSF_PS0",
                      &colnum[HXD_PST_PHSF_PS0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PHSF_PS0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PHSF_PS1",
                      &colnum[HXD_PST_PHSF_PS1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PHSF_PS1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_UNI_DIS0",
                      &colnum[HXD_PST_UNI_DIS0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_UNI_DIS0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_UNI_DIS1",
                      &colnum[HXD_PST_UNI_DIS1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_UNI_DIS1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_UNI_DIS2",
                      &colnum[HXD_PST_UNI_DIS2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_UNI_DIS2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_UNI_DIS3",
                      &colnum[HXD_PST_UNI_DIS3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_UNI_DIS3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS0",
                      &colnum[HXD_PST_2D_DIS0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS1",
                      &colnum[HXD_PST_2D_DIS1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS2",
                      &colnum[HXD_PST_2D_DIS2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS3",
                      &colnum[HXD_PST_2D_DIS3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS4",
                      &colnum[HXD_PST_2D_DIS4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS5",
                      &colnum[HXD_PST_2D_DIS5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS6",
                      &colnum[HXD_PST_2D_DIS6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS7",
                      &colnum[HXD_PST_2D_DIS7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS8",
                      &colnum[HXD_PST_2D_DIS8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS9",
                      &colnum[HXD_PST_2D_DIS9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS10",
                      &colnum[HXD_PST_2D_DIS10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS11",
                      &colnum[HXD_PST_2D_DIS11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS12",
                      &colnum[HXD_PST_2D_DIS12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS13",
                      &colnum[HXD_PST_2D_DIS13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS14",
                      &colnum[HXD_PST_2D_DIS14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_2D_DIS15",
                      &colnum[HXD_PST_2D_DIS15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_2D_DIS15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS0",
                      &colnum[HXD_PST_FL_DIS0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_EVSEL_FL_DIS1",
                      &colnum[HXD_PST_EVSEL_FL_DIS1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_EVSEL_FL_DIS1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS2",
                      &colnum[HXD_PST_FL_DIS2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS3",
                      &colnum[HXD_PST_FL_DIS3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS4",
                      &colnum[HXD_PST_FL_DIS4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS5",
                      &colnum[HXD_PST_FL_DIS5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS6",
                      &colnum[HXD_PST_FL_DIS6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FLG_DIS7",
                      &colnum[HXD_PST_FLG_DIS7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FLG_DIS7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS8",
                      &colnum[HXD_PST_FL_DIS8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS9",
                      &colnum[HXD_PST_FL_DIS9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS10",
                      &colnum[HXD_PST_FL_DIS10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS11",
                      &colnum[HXD_PST_FL_DIS11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS12",
                      &colnum[HXD_PST_FL_DIS12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS13",
                      &colnum[HXD_PST_FL_DIS13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS14",
                      &colnum[HXD_PST_FL_DIS14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_FL_DIS15",
                      &colnum[HXD_PST_FL_DIS15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_FL_DIS15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS0",
                      &colnum[HXD_PST_TR_DIS0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS1",
                      &colnum[HXD_PST_TR_DIS1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS2",
                      &colnum[HXD_PST_TR_DIS2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS3",
                      &colnum[HXD_PST_TR_DIS3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS4",
                      &colnum[HXD_PST_TR_DIS4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS5",
                      &colnum[HXD_PST_TR_DIS5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS6",
                      &colnum[HXD_PST_TR_DIS6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS7",
                      &colnum[HXD_PST_TR_DIS7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS8",
                      &colnum[HXD_PST_TR_DIS8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS9",
                      &colnum[HXD_PST_TR_DIS9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS10",
                      &colnum[HXD_PST_TR_DIS10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS11",
                      &colnum[HXD_PST_TR_DIS11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS12",
                      &colnum[HXD_PST_TR_DIS12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS13",
                      &colnum[HXD_PST_TR_DIS13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS14",
                      &colnum[HXD_PST_TR_DIS14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TR_DIS15",
                      &colnum[HXD_PST_TR_DIS15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TR_DIS15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS0",
                      &colnum[HXD_PST_HT_DIS0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS1",
                      &colnum[HXD_PST_HT_DIS1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS2",
                      &colnum[HXD_PST_HT_DIS2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS3",
                      &colnum[HXD_PST_HT_DIS3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS4",
                      &colnum[HXD_PST_HT_DIS4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS5",
                      &colnum[HXD_PST_HT_DIS5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS6",
                      &colnum[HXD_PST_HT_DIS6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS7",
                      &colnum[HXD_PST_HT_DIS7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS8",
                      &colnum[HXD_PST_HT_DIS8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS9",
                      &colnum[HXD_PST_HT_DIS9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS10",
                      &colnum[HXD_PST_HT_DIS10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS11",
                      &colnum[HXD_PST_HT_DIS11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS12",
                      &colnum[HXD_PST_HT_DIS12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS13",
                      &colnum[HXD_PST_HT_DIS13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS14",
                      &colnum[HXD_PST_HT_DIS14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HT_DIS15",
                      &colnum[HXD_PST_HT_DIS15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HT_DIS15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS0",
                      &colnum[HXD_PST_HP_DIS0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS1",
                      &colnum[HXD_PST_HP_DIS1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS2",
                      &colnum[HXD_PST_HP_DIS2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS3",
                      &colnum[HXD_PST_HP_DIS3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS4",
                      &colnum[HXD_PST_HP_DIS4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS5",
                      &colnum[HXD_PST_HP_DIS5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS6",
                      &colnum[HXD_PST_HP_DIS6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS7",
                      &colnum[HXD_PST_HP_DIS7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS8",
                      &colnum[HXD_PST_HP_DIS8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS9",
                      &colnum[HXD_PST_HP_DIS9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS10",
                      &colnum[HXD_PST_HP_DIS10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS11",
                      &colnum[HXD_PST_HP_DIS11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS12",
                      &colnum[HXD_PST_HP_DIS12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS13",
                      &colnum[HXD_PST_HP_DIS13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS14",
                      &colnum[HXD_PST_HP_DIS14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_HP_DIS15",
                      &colnum[HXD_PST_HP_DIS15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_HP_DIS15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS0",
                      &colnum[HXD_PST_DT_DIS0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS1",
                      &colnum[HXD_PST_DT_DIS1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS2",
                      &colnum[HXD_PST_DT_DIS2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS3",
                      &colnum[HXD_PST_DT_DIS3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS4",
                      &colnum[HXD_PST_DT_DIS4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS5",
                      &colnum[HXD_PST_DT_DIS5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS6",
                      &colnum[HXD_PST_DT_DIS6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS7",
                      &colnum[HXD_PST_DT_DIS7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS8",
                      &colnum[HXD_PST_DT_DIS8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS9",
                      &colnum[HXD_PST_DT_DIS9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS10",
                      &colnum[HXD_PST_DT_DIS10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS11",
                      &colnum[HXD_PST_DT_DIS11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS12",
                      &colnum[HXD_PST_DT_DIS12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS13",
                      &colnum[HXD_PST_DT_DIS13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS14",
                      &colnum[HXD_PST_DT_DIS14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_DIS15",
                      &colnum[HXD_PST_DT_DIS15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_DIS15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM0",
                      &colnum[HXD_PST_DT_SUM0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM1",
                      &colnum[HXD_PST_DT_SUM1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM2",
                      &colnum[HXD_PST_DT_SUM2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM3",
                      &colnum[HXD_PST_DT_SUM3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM4",
                      &colnum[HXD_PST_DT_SUM4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM5",
                      &colnum[HXD_PST_DT_SUM5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM6",
                      &colnum[HXD_PST_DT_SUM6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM7",
                      &colnum[HXD_PST_DT_SUM7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM8",
                      &colnum[HXD_PST_DT_SUM8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM9",
                      &colnum[HXD_PST_DT_SUM9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM10",
                      &colnum[HXD_PST_DT_SUM10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM11",
                      &colnum[HXD_PST_DT_SUM11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM12",
                      &colnum[HXD_PST_DT_SUM12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM13",
                      &colnum[HXD_PST_DT_SUM13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM14",
                      &colnum[HXD_PST_DT_SUM14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_SUM15",
                      &colnum[HXD_PST_DT_SUM15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_SUM15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS0",
                      &colnum[HXD_PST_PL_DIS0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS1",
                      &colnum[HXD_PST_PL_DIS1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS2",
                      &colnum[HXD_PST_PL_DIS2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS3",
                      &colnum[HXD_PST_PL_DIS3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS4",
                      &colnum[HXD_PST_PL_DIS4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS5",
                      &colnum[HXD_PST_PL_DIS5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS6",
                      &colnum[HXD_PST_PL_DIS6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS7",
                      &colnum[HXD_PST_PL_DIS7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS8",
                      &colnum[HXD_PST_PL_DIS8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS9",
                      &colnum[HXD_PST_PL_DIS9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS10",
                      &colnum[HXD_PST_PL_DIS10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS11",
                      &colnum[HXD_PST_PL_DIS11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS12",
                      &colnum[HXD_PST_PL_DIS12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS13",
                      &colnum[HXD_PST_PL_DIS13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS14",
                      &colnum[HXD_PST_PL_DIS14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PL_DIS15",
                      &colnum[HXD_PST_PL_DIS15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PL_DIS15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS0",
                      &colnum[HXD_PST_PT_DIS0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS1",
                      &colnum[HXD_PST_PT_DIS1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS2",
                      &colnum[HXD_PST_PT_DIS2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS3",
                      &colnum[HXD_PST_PT_DIS3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS4",
                      &colnum[HXD_PST_PT_DIS4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS5",
                      &colnum[HXD_PST_PT_DIS5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS6",
                      &colnum[HXD_PST_PT_DIS6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS7",
                      &colnum[HXD_PST_PT_DIS7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS8",
                      &colnum[HXD_PST_PT_DIS8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS9",
                      &colnum[HXD_PST_PT_DIS9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS10",
                      &colnum[HXD_PST_PT_DIS10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS11",
                      &colnum[HXD_PST_PT_DIS11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS12",
                      &colnum[HXD_PST_PT_DIS12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS13",
                      &colnum[HXD_PST_PT_DIS13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS14",
                      &colnum[HXD_PST_PT_DIS14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PT_DIS15",
                      &colnum[HXD_PST_PT_DIS15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PT_DIS15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W0",
                      &colnum[HXD_PST_PREV_W0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W1",
                      &colnum[HXD_PST_PREV_W1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W2",
                      &colnum[HXD_PST_PREV_W2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W3",
                      &colnum[HXD_PST_PREV_W3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W4",
                      &colnum[HXD_PST_PREV_W4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W5",
                      &colnum[HXD_PST_PREV_W5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W6",
                      &colnum[HXD_PST_PREV_W6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W7",
                      &colnum[HXD_PST_PREV_W7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W8",
                      &colnum[HXD_PST_PREV_W8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W9",
                      &colnum[HXD_PST_PREV_W9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W10",
                      &colnum[HXD_PST_PREV_W10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W11",
                      &colnum[HXD_PST_PREV_W11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W12",
                      &colnum[HXD_PST_PREV_W12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W13",
                      &colnum[HXD_PST_PREV_W13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W14",
                      &colnum[HXD_PST_PREV_W14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_W15",
                      &colnum[HXD_PST_PREV_W15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_W15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N0",
                      &colnum[HXD_PST_PREV_N0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N1",
                      &colnum[HXD_PST_PREV_N1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N2",
                      &colnum[HXD_PST_PREV_N2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N3",
                      &colnum[HXD_PST_PREV_N3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N4",
                      &colnum[HXD_PST_PREV_N4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N5",
                      &colnum[HXD_PST_PREV_N5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N6",
                      &colnum[HXD_PST_PREV_N6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N7",
                      &colnum[HXD_PST_PREV_N7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N8",
                      &colnum[HXD_PST_PREV_N8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N9",
                      &colnum[HXD_PST_PREV_N9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N10",
                      &colnum[HXD_PST_PREV_N10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N11",
                      &colnum[HXD_PST_PREV_N11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N12",
                      &colnum[HXD_PST_PREV_N12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N13",
                      &colnum[HXD_PST_PREV_N13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N14",
                      &colnum[HXD_PST_PREV_N14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_N15",
                      &colnum[HXD_PST_PREV_N15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_N15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I0",
                      &colnum[HXD_PST_PREV_I0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I1",
                      &colnum[HXD_PST_PREV_I1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I2",
                      &colnum[HXD_PST_PREV_I2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I3",
                      &colnum[HXD_PST_PREV_I3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I4",
                      &colnum[HXD_PST_PREV_I4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I5",
                      &colnum[HXD_PST_PREV_I5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I6",
                      &colnum[HXD_PST_PREV_I6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I7",
                      &colnum[HXD_PST_PREV_I7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I8",
                      &colnum[HXD_PST_PREV_I8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I9",
                      &colnum[HXD_PST_PREV_I9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I10",
                      &colnum[HXD_PST_PREV_I10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I11",
                      &colnum[HXD_PST_PREV_I11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I12",
                      &colnum[HXD_PST_PREV_I12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I13",
                      &colnum[HXD_PST_PREV_I13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I14",
                      &colnum[HXD_PST_PREV_I14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_PREV_I15",
                      &colnum[HXD_PST_PREV_I15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_PREV_I15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS0",
                      &colnum[HXD_PST_DT_PS0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS1",
                      &colnum[HXD_PST_DT_PS1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS2",
                      &colnum[HXD_PST_DT_PS2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS3",
                      &colnum[HXD_PST_DT_PS3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS4",
                      &colnum[HXD_PST_DT_PS4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS5",
                      &colnum[HXD_PST_DT_PS5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS6",
                      &colnum[HXD_PST_DT_PS6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS7",
                      &colnum[HXD_PST_DT_PS7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS8",
                      &colnum[HXD_PST_DT_PS8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS9",
                      &colnum[HXD_PST_DT_PS9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS10",
                      &colnum[HXD_PST_DT_PS10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS11",
                      &colnum[HXD_PST_DT_PS11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS12",
                      &colnum[HXD_PST_DT_PS12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS13",
                      &colnum[HXD_PST_DT_PS13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS14",
                      &colnum[HXD_PST_DT_PS14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_DT_PS15",
                      &colnum[HXD_PST_DT_PS15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_DT_PS15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI0",
                      &colnum[HXD_PST_ACU_TI0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI1",
                      &colnum[HXD_PST_ACU_TI1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI2",
                      &colnum[HXD_PST_ACU_TI2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI3",
                      &colnum[HXD_PST_ACU_TI3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI4",
                      &colnum[HXD_PST_ACU_TI4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI5",
                      &colnum[HXD_PST_ACU_TI5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI6",
                      &colnum[HXD_PST_ACU_TI6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI7",
                      &colnum[HXD_PST_ACU_TI7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI8",
                      &colnum[HXD_PST_ACU_TI8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI9",
                      &colnum[HXD_PST_ACU_TI9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI10",
                      &colnum[HXD_PST_ACU_TI10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI11",
                      &colnum[HXD_PST_ACU_TI11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI12",
                      &colnum[HXD_PST_ACU_TI12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI13",
                      &colnum[HXD_PST_ACU_TI13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI14",
                      &colnum[HXD_PST_ACU_TI14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_ACU_TI15",
                      &colnum[HXD_PST_ACU_TI15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_ACU_TI15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_REQ0",
                      &colnum[HXD_PST_BST_REQ0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_REQ0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_REQ1",
                      &colnum[HXD_PST_BST_REQ1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_REQ1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_REQ2",
                      &colnum[HXD_PST_BST_REQ2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_REQ2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_REQ3",
                      &colnum[HXD_PST_BST_REQ3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_REQ3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TPUTMOD0",
                      &colnum[HXD_PST_TPUTMOD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TPUTMOD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TPUTMOD1",
                      &colnum[HXD_PST_TPUTMOD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TPUTMOD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TPUTMOD2",
                      &colnum[HXD_PST_TPUTMOD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TPUTMOD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_TPUTMOD3",
                      &colnum[HXD_PST_TPUTMOD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_TPUTMOD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_TRGF",
                      &colnum[HXD_PST_BST_TRGF], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_TRGF') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_PH_F",
                      &colnum[HXD_PST_BST_PH_F], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_PH_F') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_WLDF",
                      &colnum[HXD_PST_BST_WLDF], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_WLDF') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_BST_PSEF",
                      &colnum[HXD_PST_BST_PSEF], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_BST_PSEF') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GB_F_PAT",
                      &colnum[HXD_PST_GB_F_PAT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GB_F_PAT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PST_GB_ENA",
                      &colnum[HXD_PST_GB_ENA], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PST_GB_ENA') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadPST_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, PST, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, PST, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:PST:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pst_pi_ver;
    fits_read_col_byt(fp, colnum[HXD_PST_PI_VER], irow, firstelem,
                      nelements, nulval, &hxd_pst_pi_ver, &anynul, &istat);
    data[0] = hxd_pst_pi_ver;
    BnkfPutM ("HXD:PST:PI_VERSION", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_err_cnt;
    fits_read_col_usht(fp, colnum[HXD_PST_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_pst_err_cnt, &anynul, &istat);
    data[0] = hxd_pst_err_cnt;
    BnkfPutM ("HXD:PST:ERROR_COUNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_ret_err;
    fits_read_col_usht(fp, colnum[HXD_PST_RET_ERR], irow, firstelem,
                      nelements, nulval, &hxd_pst_ret_err, &anynul, &istat);
    data[0] = hxd_pst_ret_err;
    BnkfPutM ("HXD:PST:RETURNED_ERROR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_err_sys;
    fits_read_col_usht(fp, colnum[HXD_PST_ERR_SYS], irow, firstelem,
                      nelements, nulval, &hxd_pst_err_sys, &anynul, &istat);
    data[0] = hxd_pst_err_sys;
    BnkfPutM ("HXD:PST:ERR_SYS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pst_err_pi;
    fits_read_col_byt(fp, colnum[HXD_PST_ERR_PI], irow, firstelem,
                      nelements, nulval, &hxd_pst_err_pi, &anynul, &istat);
    data[0] = hxd_pst_err_pi;
    BnkfPutM ("HXD:PST:ERR_PI", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pst_err_picd;
    fits_read_col_byt(fp, colnum[HXD_PST_ERR_PICD], irow, firstelem,
                      nelements, nulval, &hxd_pst_err_picd, &anynul, &istat);
    data[0] = hxd_pst_err_picd;
    BnkfPutM ("HXD:PST:ERR_PICOD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_wev_tout;
    fits_read_col_usht(fp, colnum[HXD_PST_WEV_TOUT], irow, firstelem,
                      nelements, nulval, &hxd_pst_wev_tout, &anynul, &istat);
    data[0] = hxd_pst_wev_tout;
    BnkfPutM ("HXD:PST:WEVT_TIMEOUT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_gst_call;
    fits_read_col_usht(fp, colnum[HXD_PST_GST_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_gst_call, &anynul, &istat);
    data[0] = hxd_pst_gst_call;
    BnkfPutM ("HXD:PST:GSTS_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_gst_para;
    fits_read_col_usht(fp, colnum[HXD_PST_GST_PARA], irow, firstelem,
                      nelements, nulval, &hxd_pst_gst_para, &anynul, &istat);
    data[0] = hxd_pst_gst_para;
    BnkfPutM ("HXD:PST:GSTS_PARAM", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_pst_call;
    fits_read_col_usht(fp, colnum[HXD_PST_PST_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_pst_call, &anynul, &istat);
    data[0] = hxd_pst_pst_call;
    BnkfPutM ("HXD:PST:PSTS_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_pst_disc;
    fits_read_col_usht(fp, colnum[HXD_PST_PST_DISC], irow, firstelem,
                      nelements, nulval, &hxd_pst_pst_disc, &anynul, &istat);
    data[0] = hxd_pst_pst_disc;
    BnkfPutM ("HXD:PST:PSTS_DISCARD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_pps_call;
    fits_read_col_usht(fp, colnum[HXD_PST_PPS_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_pps_call, &anynul, &istat);
    data[0] = hxd_pst_pps_call;
    BnkfPutM ("HXD:PST:PPST_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_gcm_call;
    fits_read_col_usht(fp, colnum[HXD_PST_GCM_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_gcm_call, &anynul, &istat);
    data[0] = hxd_pst_gcm_call;
    BnkfPutM ("HXD:PST:GCMD_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_pcm_call;
    fits_read_col_usht(fp, colnum[HXD_PST_PCM_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_pcm_call, &anynul, &istat);
    data[0] = hxd_pst_pcm_call;
    BnkfPutM ("HXD:PST:PCMD_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned int nulval=1;
    unsigned int hxd_pst_gwe_call;
    fits_read_col_uint(fp, colnum[HXD_PST_GWE_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_gwe_call, &anynul, &istat);
    data[0] = hxd_pst_gwe_call;
    BnkfPutM ("HXD:PST:GWEL_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_gwe_size;
    fits_read_col_usht(fp, colnum[HXD_PST_GWE_SIZE], irow, firstelem,
                      nelements, nulval, &hxd_pst_gwe_size, &anynul, &istat);
    data[0] = hxd_pst_gwe_size;
    BnkfPutM ("HXD:PST:GWEL_SIZE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_pwe_rjct;
    fits_read_col_usht(fp, colnum[HXD_PST_PWE_RJCT], irow, firstelem,
                      nelements, nulval, &hxd_pst_pwe_rjct, &anynul, &istat);
    data[0] = hxd_pst_pwe_rjct;
    BnkfPutM ("HXD:PST:PWEL_OVERWRITE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned int nulval=1;
    unsigned int hxd_pst_pwe_call;
    fits_read_col_uint(fp, colnum[HXD_PST_PWE_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_pwe_call, &anynul, &istat);
    data[0] = hxd_pst_pwe_call;
    BnkfPutM ("HXD:PST:PWEL_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned int nulval=1;
    unsigned int hxd_pst_gwe_lner;
    fits_read_col_uint(fp, colnum[HXD_PST_GWE_LNER], irow, firstelem,
                      nelements, nulval, &hxd_pst_gwe_lner, &anynul, &istat);
    data[0] = hxd_pst_gwe_lner;
    BnkfPutM ("HXD:PST:GWEL_LENERROR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_gmn_call;
    fits_read_col_usht(fp, colnum[HXD_PST_GMN_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_gmn_call, &anynul, &istat);
    data[0] = hxd_pst_gmn_call;
    BnkfPutM ("HXD:PST:GMON_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_gmn_cnt;
    fits_read_col_usht(fp, colnum[HXD_PST_GMN_CNT], irow, firstelem,
                      nelements, nulval, &hxd_pst_gmn_cnt, &anynul, &istat);
    data[0] = hxd_pst_gmn_cnt;
    BnkfPutM ("HXD:PST:GMON_COUNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_pmn_call;
    fits_read_col_usht(fp, colnum[HXD_PST_PMN_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_pmn_call, &anynul, &istat);
    data[0] = hxd_pst_pmn_call;
    BnkfPutM ("HXD:PST:PMON_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_pmn_disc;
    fits_read_col_usht(fp, colnum[HXD_PST_PMN_DISC], irow, firstelem,
                      nelements, nulval, &hxd_pst_pmn_disc, &anynul, &istat);
    data[0] = hxd_pst_pmn_disc;
    BnkfPutM ("HXD:PST:PMON_DISCARD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_ghk_call;
    fits_read_col_usht(fp, colnum[HXD_PST_GHK_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_ghk_call, &anynul, &istat);
    data[0] = hxd_pst_ghk_call;
    BnkfPutM ("HXD:PST:GHK_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_ghk_cnt;
    fits_read_col_usht(fp, colnum[HXD_PST_GHK_CNT], irow, firstelem,
                      nelements, nulval, &hxd_pst_ghk_cnt, &anynul, &istat);
    data[0] = hxd_pst_ghk_cnt;
    BnkfPutM ("HXD:PST:GHK_COUNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_phk_call;
    fits_read_col_usht(fp, colnum[HXD_PST_PHK_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_phk_call, &anynul, &istat);
    data[0] = hxd_pst_phk_call;
    BnkfPutM ("HXD:PST:PHK_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_phk_disc;
    fits_read_col_usht(fp, colnum[HXD_PST_PHK_DISC], irow, firstelem,
                      nelements, nulval, &hxd_pst_phk_disc, &anynul, &istat);
    data[0] = hxd_pst_phk_disc;
    BnkfPutM ("HXD:PST:PHK_DISCARD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_gbst_call;
    fits_read_col_usht(fp, colnum[HXD_PST_GBST_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_gbst_call, &anynul, &istat);
    data[0] = hxd_pst_gbst_call;
    BnkfPutM ("HXD:PST:GBST_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_gbst_cnt;
    fits_read_col_usht(fp, colnum[HXD_PST_GBST_CNT], irow, firstelem,
                      nelements, nulval, &hxd_pst_gbst_cnt, &anynul, &istat);
    data[0] = hxd_pst_gbst_cnt;
    BnkfPutM ("HXD:PST:GBST_COUNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_pbst_call;
    fits_read_col_usht(fp, colnum[HXD_PST_PBST_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_pbst_call, &anynul, &istat);
    data[0] = hxd_pst_pbst_call;
    BnkfPutM ("HXD:PST:PBST_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_pbst_disc;
    fits_read_col_usht(fp, colnum[HXD_PST_PBST_DISC], irow, firstelem,
                      nelements, nulval, &hxd_pst_pbst_disc, &anynul, &istat);
    data[0] = hxd_pst_pbst_disc;
    BnkfPutM ("HXD:PST:PBST_DISCARD", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_pst_bst_cnt0;
    unsigned short hxd_pst_bst_cnt1;
    unsigned short hxd_pst_bst_cnt2;
    unsigned short hxd_pst_bst_cnt3;
    fits_read_col_usht(fp, colnum[HXD_PST_BST_CNT0], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_cnt0, &anynul, &istat);
    data[0] = hxd_pst_bst_cnt0;
    fits_read_col_usht(fp, colnum[HXD_PST_BST_CNT1], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_cnt1, &anynul, &istat);
    data[1] = hxd_pst_bst_cnt1;
    fits_read_col_usht(fp, colnum[HXD_PST_BST_CNT2], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_cnt2, &anynul, &istat);
    data[2] = hxd_pst_bst_cnt2;
    fits_read_col_usht(fp, colnum[HXD_PST_BST_CNT3], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_cnt3, &anynul, &istat);
    data[3] = hxd_pst_bst_cnt3;
    BnkfPutM ("HXD:PST:BST_CNT_TPU", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_bst_curmd;
    fits_read_col_usht(fp, colnum[HXD_PST_BST_CURMD], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_curmd, &anynul, &istat);
    data[0] = hxd_pst_bst_curmd;
    BnkfPutM ("HXD:PST:BST_CURR_BD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_cmd_rjct;
    fits_read_col_usht(fp, colnum[HXD_PST_CMD_RJCT], irow, firstelem,
                      nelements, nulval, &hxd_pst_cmd_rjct, &anynul, &istat);
    data[0] = hxd_pst_cmd_rjct;
    BnkfPutM ("HXD:PST:CMD_RJT_CNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_gtr_call;
    fits_read_col_usht(fp, colnum[HXD_PST_GTR_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_gtr_call, &anynul, &istat);
    data[0] = hxd_pst_gtr_call;
    BnkfPutM ("HXD:PST:GTRN_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_gtr_lner;
    fits_read_col_usht(fp, colnum[HXD_PST_GTR_LNER], irow, firstelem,
                      nelements, nulval, &hxd_pst_gtr_lner, &anynul, &istat);
    data[0] = hxd_pst_gtr_lner;
    BnkfPutM ("HXD:PST:GTRN_LENERROR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_ptr_call;
    fits_read_col_usht(fp, colnum[HXD_PST_PTR_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_ptr_call, &anynul, &istat);
    data[0] = hxd_pst_ptr_call;
    BnkfPutM ("HXD:PST:PTRN_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_ptr_disc;
    fits_read_col_usht(fp, colnum[HXD_PST_PTR_DISC], irow, firstelem,
                      nelements, nulval, &hxd_pst_ptr_disc, &anynul, &istat);
    data[0] = hxd_pst_ptr_disc;
    BnkfPutM ("HXD:PST:PTRN_DISCARD", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_pst_trn_cnt0;
    unsigned short hxd_pst_trn_cnt1;
    unsigned short hxd_pst_trn_cnt2;
    unsigned short hxd_pst_trn_cnt3;
    fits_read_col_usht(fp, colnum[HXD_PST_TRN_CNT0], irow, firstelem,
                      nelements, nulval, &hxd_pst_trn_cnt0, &anynul, &istat);
    data[0] = hxd_pst_trn_cnt0;
    fits_read_col_usht(fp, colnum[HXD_PST_TRN_CNT1], irow, firstelem,
                      nelements, nulval, &hxd_pst_trn_cnt1, &anynul, &istat);
    data[1] = hxd_pst_trn_cnt1;
    fits_read_col_usht(fp, colnum[HXD_PST_TRN_CNT2], irow, firstelem,
                      nelements, nulval, &hxd_pst_trn_cnt2, &anynul, &istat);
    data[2] = hxd_pst_trn_cnt2;
    fits_read_col_usht(fp, colnum[HXD_PST_TRN_CNT3], irow, firstelem,
                      nelements, nulval, &hxd_pst_trn_cnt3, &anynul, &istat);
    data[3] = hxd_pst_trn_cnt3;
    BnkfPutM ("HXD:PST:TRN_COUNT", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pst_gb_flg;
    fits_read_col_byt(fp, colnum[HXD_PST_GB_FLG], irow, firstelem,
                      nelements, nulval, &hxd_pst_gb_flg, &anynul, &istat);
    data[0] = hxd_pst_gb_flg;
    BnkfPutM ("HXD:PST:GB_FLG", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pst_gb_frz;
    fits_read_col_byt(fp, colnum[HXD_PST_GB_FRZ], irow, firstelem,
                      nelements, nulval, &hxd_pst_gb_frz, &anynul, &istat);
    data[0] = hxd_pst_gb_frz;
    BnkfPutM ("HXD:PST:GB_FRZ", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_phsf_seq;
    fits_read_col_usht(fp, colnum[HXD_PST_PHSF_SEQ], irow, firstelem,
                      nelements, nulval, &hxd_pst_phsf_seq, &anynul, &istat);
    data[0] = hxd_pst_phsf_seq;
    BnkfPutM ("HXD:PST:PHSF_SEQ", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_phsf_call;
    fits_read_col_usht(fp, colnum[HXD_PST_PHSF_CALL], irow, firstelem,
                      nelements, nulval, &hxd_pst_phsf_call, &anynul, &istat);
    data[0] = hxd_pst_phsf_call;
    BnkfPutM ("HXD:PST:PHSF_CALL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pst_phsf_disc;
    fits_read_col_usht(fp, colnum[HXD_PST_PHSF_DISC], irow, firstelem,
                      nelements, nulval, &hxd_pst_phsf_disc, &anynul, &istat);
    data[0] = hxd_pst_phsf_disc;
    BnkfPutM ("HXD:PST:PHSF_DISCARD", sizeof(int)*1, data);
  }
  {
    unsigned int data[2];
    unsigned int nulval=1;
    unsigned int hxd_pst_phsf_c0;
    unsigned int hxd_pst_phsf_c1;
    fits_read_col_uint(fp, colnum[HXD_PST_PHSF_C0], irow, firstelem,
                      nelements, nulval, &hxd_pst_phsf_c0, &anynul, &istat);
    data[0] = hxd_pst_phsf_c0;
    fits_read_col_uint(fp, colnum[HXD_PST_PHSF_C1], irow, firstelem,
                      nelements, nulval, &hxd_pst_phsf_c1, &anynul, &istat);
    data[1] = hxd_pst_phsf_c1;
    BnkfPutM ("HXD:PST:PHSF_COUNTER", sizeof(int)*2, data);
  }
  {
    unsigned int data[2];
    unsigned int nulval=1;
    unsigned int hxd_pst_phsf_ps0;
    unsigned int hxd_pst_phsf_ps1;
    fits_read_col_uint(fp, colnum[HXD_PST_PHSF_PS0], irow, firstelem,
                      nelements, nulval, &hxd_pst_phsf_ps0, &anynul, &istat);
    data[0] = hxd_pst_phsf_ps0;
    fits_read_col_uint(fp, colnum[HXD_PST_PHSF_PS1], irow, firstelem,
                      nelements, nulval, &hxd_pst_phsf_ps1, &anynul, &istat);
    data[1] = hxd_pst_phsf_ps1;
    BnkfPutM ("HXD:PST:PHSF_PSEUD", sizeof(int)*2, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_pst_uni_dis0;
    unsigned short hxd_pst_uni_dis1;
    unsigned short hxd_pst_uni_dis2;
    unsigned short hxd_pst_uni_dis3;
    fits_read_col_usht(fp, colnum[HXD_PST_UNI_DIS0], irow, firstelem,
                      nelements, nulval, &hxd_pst_uni_dis0, &anynul, &istat);
    data[0] = hxd_pst_uni_dis0;
    fits_read_col_usht(fp, colnum[HXD_PST_UNI_DIS1], irow, firstelem,
                      nelements, nulval, &hxd_pst_uni_dis1, &anynul, &istat);
    data[1] = hxd_pst_uni_dis1;
    fits_read_col_usht(fp, colnum[HXD_PST_UNI_DIS2], irow, firstelem,
                      nelements, nulval, &hxd_pst_uni_dis2, &anynul, &istat);
    data[2] = hxd_pst_uni_dis2;
    fits_read_col_usht(fp, colnum[HXD_PST_UNI_DIS3], irow, firstelem,
                      nelements, nulval, &hxd_pst_uni_dis3, &anynul, &istat);
    data[3] = hxd_pst_uni_dis3;
    BnkfPutM ("HXD:PST:EVSEL_UNI_DISCD", sizeof(int)*4, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_2d_dis0;
    unsigned int hxd_pst_2d_dis1;
    unsigned int hxd_pst_2d_dis2;
    unsigned int hxd_pst_2d_dis3;
    unsigned int hxd_pst_2d_dis4;
    unsigned int hxd_pst_2d_dis5;
    unsigned int hxd_pst_2d_dis6;
    unsigned int hxd_pst_2d_dis7;
    unsigned int hxd_pst_2d_dis8;
    unsigned int hxd_pst_2d_dis9;
    unsigned int hxd_pst_2d_dis10;
    unsigned int hxd_pst_2d_dis11;
    unsigned int hxd_pst_2d_dis12;
    unsigned int hxd_pst_2d_dis13;
    unsigned int hxd_pst_2d_dis14;
    unsigned int hxd_pst_2d_dis15;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS0], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis0, &anynul, &istat);
    data[0] = hxd_pst_2d_dis0;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS1], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis1, &anynul, &istat);
    data[1] = hxd_pst_2d_dis1;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS2], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis2, &anynul, &istat);
    data[2] = hxd_pst_2d_dis2;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS3], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis3, &anynul, &istat);
    data[3] = hxd_pst_2d_dis3;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS4], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis4, &anynul, &istat);
    data[4] = hxd_pst_2d_dis4;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS5], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis5, &anynul, &istat);
    data[5] = hxd_pst_2d_dis5;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS6], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis6, &anynul, &istat);
    data[6] = hxd_pst_2d_dis6;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS7], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis7, &anynul, &istat);
    data[7] = hxd_pst_2d_dis7;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS8], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis8, &anynul, &istat);
    data[8] = hxd_pst_2d_dis8;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS9], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis9, &anynul, &istat);
    data[9] = hxd_pst_2d_dis9;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS10], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis10, &anynul, &istat);
    data[10] = hxd_pst_2d_dis10;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS11], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis11, &anynul, &istat);
    data[11] = hxd_pst_2d_dis11;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS12], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis12, &anynul, &istat);
    data[12] = hxd_pst_2d_dis12;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS13], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis13, &anynul, &istat);
    data[13] = hxd_pst_2d_dis13;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS14], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis14, &anynul, &istat);
    data[14] = hxd_pst_2d_dis14;
    fits_read_col_uint(fp, colnum[HXD_PST_2D_DIS15], irow, firstelem,
                      nelements, nulval, &hxd_pst_2d_dis15, &anynul, &istat);
    data[15] = hxd_pst_2d_dis15;
    BnkfPutM ("HXD:PST:EVSEL_2D_DISCD", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_fl_dis0;
    unsigned int hxd_pst_evsel_fl_dis1;
    unsigned int hxd_pst_fl_dis2;
    unsigned int hxd_pst_fl_dis3;
    unsigned int hxd_pst_fl_dis4;
    unsigned int hxd_pst_fl_dis5;
    unsigned int hxd_pst_fl_dis6;
    unsigned int hxd_pst_flg_dis7;
    unsigned int hxd_pst_fl_dis8;
    unsigned int hxd_pst_fl_dis9;
    unsigned int hxd_pst_fl_dis10;
    unsigned int hxd_pst_fl_dis11;
    unsigned int hxd_pst_fl_dis12;
    unsigned int hxd_pst_fl_dis13;
    unsigned int hxd_pst_fl_dis14;
    unsigned int hxd_pst_fl_dis15;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS0], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis0, &anynul, &istat);
    data[0] = hxd_pst_fl_dis0;
    fits_read_col_uint(fp, colnum[HXD_PST_EVSEL_FL_DIS1], irow, firstelem,
                      nelements, nulval, &hxd_pst_evsel_fl_dis1, &anynul, &istat);
    data[1] = hxd_pst_evsel_fl_dis1;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS2], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis2, &anynul, &istat);
    data[2] = hxd_pst_fl_dis2;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS3], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis3, &anynul, &istat);
    data[3] = hxd_pst_fl_dis3;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS4], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis4, &anynul, &istat);
    data[4] = hxd_pst_fl_dis4;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS5], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis5, &anynul, &istat);
    data[5] = hxd_pst_fl_dis5;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS6], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis6, &anynul, &istat);
    data[6] = hxd_pst_fl_dis6;
    fits_read_col_uint(fp, colnum[HXD_PST_FLG_DIS7], irow, firstelem,
                      nelements, nulval, &hxd_pst_flg_dis7, &anynul, &istat);
    data[7] = hxd_pst_flg_dis7;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS8], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis8, &anynul, &istat);
    data[8] = hxd_pst_fl_dis8;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS9], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis9, &anynul, &istat);
    data[9] = hxd_pst_fl_dis9;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS10], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis10, &anynul, &istat);
    data[10] = hxd_pst_fl_dis10;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS11], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis11, &anynul, &istat);
    data[11] = hxd_pst_fl_dis11;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS12], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis12, &anynul, &istat);
    data[12] = hxd_pst_fl_dis12;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS13], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis13, &anynul, &istat);
    data[13] = hxd_pst_fl_dis13;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS14], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis14, &anynul, &istat);
    data[14] = hxd_pst_fl_dis14;
    fits_read_col_uint(fp, colnum[HXD_PST_FL_DIS15], irow, firstelem,
                      nelements, nulval, &hxd_pst_fl_dis15, &anynul, &istat);
    data[15] = hxd_pst_fl_dis15;
    BnkfPutM ("HXD:PST:EVSEL_FLG_DISCD", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_tr_dis0;
    unsigned int hxd_pst_tr_dis1;
    unsigned int hxd_pst_tr_dis2;
    unsigned int hxd_pst_tr_dis3;
    unsigned int hxd_pst_tr_dis4;
    unsigned int hxd_pst_tr_dis5;
    unsigned int hxd_pst_tr_dis6;
    unsigned int hxd_pst_tr_dis7;
    unsigned int hxd_pst_tr_dis8;
    unsigned int hxd_pst_tr_dis9;
    unsigned int hxd_pst_tr_dis10;
    unsigned int hxd_pst_tr_dis11;
    unsigned int hxd_pst_tr_dis12;
    unsigned int hxd_pst_tr_dis13;
    unsigned int hxd_pst_tr_dis14;
    unsigned int hxd_pst_tr_dis15;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS0], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis0, &anynul, &istat);
    data[0] = hxd_pst_tr_dis0;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS1], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis1, &anynul, &istat);
    data[1] = hxd_pst_tr_dis1;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS2], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis2, &anynul, &istat);
    data[2] = hxd_pst_tr_dis2;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS3], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis3, &anynul, &istat);
    data[3] = hxd_pst_tr_dis3;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS4], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis4, &anynul, &istat);
    data[4] = hxd_pst_tr_dis4;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS5], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis5, &anynul, &istat);
    data[5] = hxd_pst_tr_dis5;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS6], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis6, &anynul, &istat);
    data[6] = hxd_pst_tr_dis6;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS7], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis7, &anynul, &istat);
    data[7] = hxd_pst_tr_dis7;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS8], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis8, &anynul, &istat);
    data[8] = hxd_pst_tr_dis8;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS9], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis9, &anynul, &istat);
    data[9] = hxd_pst_tr_dis9;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS10], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis10, &anynul, &istat);
    data[10] = hxd_pst_tr_dis10;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS11], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis11, &anynul, &istat);
    data[11] = hxd_pst_tr_dis11;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS12], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis12, &anynul, &istat);
    data[12] = hxd_pst_tr_dis12;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS13], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis13, &anynul, &istat);
    data[13] = hxd_pst_tr_dis13;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS14], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis14, &anynul, &istat);
    data[14] = hxd_pst_tr_dis14;
    fits_read_col_uint(fp, colnum[HXD_PST_TR_DIS15], irow, firstelem,
                      nelements, nulval, &hxd_pst_tr_dis15, &anynul, &istat);
    data[15] = hxd_pst_tr_dis15;
    BnkfPutM ("HXD:PST:EVSEL_TRG_DISCD", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_ht_dis0;
    unsigned int hxd_pst_ht_dis1;
    unsigned int hxd_pst_ht_dis2;
    unsigned int hxd_pst_ht_dis3;
    unsigned int hxd_pst_ht_dis4;
    unsigned int hxd_pst_ht_dis5;
    unsigned int hxd_pst_ht_dis6;
    unsigned int hxd_pst_ht_dis7;
    unsigned int hxd_pst_ht_dis8;
    unsigned int hxd_pst_ht_dis9;
    unsigned int hxd_pst_ht_dis10;
    unsigned int hxd_pst_ht_dis11;
    unsigned int hxd_pst_ht_dis12;
    unsigned int hxd_pst_ht_dis13;
    unsigned int hxd_pst_ht_dis14;
    unsigned int hxd_pst_ht_dis15;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS0], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis0, &anynul, &istat);
    data[0] = hxd_pst_ht_dis0;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS1], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis1, &anynul, &istat);
    data[1] = hxd_pst_ht_dis1;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS2], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis2, &anynul, &istat);
    data[2] = hxd_pst_ht_dis2;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS3], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis3, &anynul, &istat);
    data[3] = hxd_pst_ht_dis3;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS4], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis4, &anynul, &istat);
    data[4] = hxd_pst_ht_dis4;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS5], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis5, &anynul, &istat);
    data[5] = hxd_pst_ht_dis5;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS6], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis6, &anynul, &istat);
    data[6] = hxd_pst_ht_dis6;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS7], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis7, &anynul, &istat);
    data[7] = hxd_pst_ht_dis7;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS8], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis8, &anynul, &istat);
    data[8] = hxd_pst_ht_dis8;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS9], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis9, &anynul, &istat);
    data[9] = hxd_pst_ht_dis9;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS10], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis10, &anynul, &istat);
    data[10] = hxd_pst_ht_dis10;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS11], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis11, &anynul, &istat);
    data[11] = hxd_pst_ht_dis11;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS12], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis12, &anynul, &istat);
    data[12] = hxd_pst_ht_dis12;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS13], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis13, &anynul, &istat);
    data[13] = hxd_pst_ht_dis13;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS14], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis14, &anynul, &istat);
    data[14] = hxd_pst_ht_dis14;
    fits_read_col_uint(fp, colnum[HXD_PST_HT_DIS15], irow, firstelem,
                      nelements, nulval, &hxd_pst_ht_dis15, &anynul, &istat);
    data[15] = hxd_pst_ht_dis15;
    BnkfPutM ("HXD:PST:EVSEL_HIT_DISCD", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_hp_dis0;
    unsigned int hxd_pst_hp_dis1;
    unsigned int hxd_pst_hp_dis2;
    unsigned int hxd_pst_hp_dis3;
    unsigned int hxd_pst_hp_dis4;
    unsigned int hxd_pst_hp_dis5;
    unsigned int hxd_pst_hp_dis6;
    unsigned int hxd_pst_hp_dis7;
    unsigned int hxd_pst_hp_dis8;
    unsigned int hxd_pst_hp_dis9;
    unsigned int hxd_pst_hp_dis10;
    unsigned int hxd_pst_hp_dis11;
    unsigned int hxd_pst_hp_dis12;
    unsigned int hxd_pst_hp_dis13;
    unsigned int hxd_pst_hp_dis14;
    unsigned int hxd_pst_hp_dis15;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS0], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis0, &anynul, &istat);
    data[0] = hxd_pst_hp_dis0;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS1], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis1, &anynul, &istat);
    data[1] = hxd_pst_hp_dis1;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS2], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis2, &anynul, &istat);
    data[2] = hxd_pst_hp_dis2;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS3], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis3, &anynul, &istat);
    data[3] = hxd_pst_hp_dis3;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS4], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis4, &anynul, &istat);
    data[4] = hxd_pst_hp_dis4;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS5], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis5, &anynul, &istat);
    data[5] = hxd_pst_hp_dis5;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS6], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis6, &anynul, &istat);
    data[6] = hxd_pst_hp_dis6;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS7], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis7, &anynul, &istat);
    data[7] = hxd_pst_hp_dis7;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS8], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis8, &anynul, &istat);
    data[8] = hxd_pst_hp_dis8;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS9], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis9, &anynul, &istat);
    data[9] = hxd_pst_hp_dis9;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS10], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis10, &anynul, &istat);
    data[10] = hxd_pst_hp_dis10;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS11], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis11, &anynul, &istat);
    data[11] = hxd_pst_hp_dis11;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS12], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis12, &anynul, &istat);
    data[12] = hxd_pst_hp_dis12;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS13], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis13, &anynul, &istat);
    data[13] = hxd_pst_hp_dis13;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS14], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis14, &anynul, &istat);
    data[14] = hxd_pst_hp_dis14;
    fits_read_col_uint(fp, colnum[HXD_PST_HP_DIS15], irow, firstelem,
                      nelements, nulval, &hxd_pst_hp_dis15, &anynul, &istat);
    data[15] = hxd_pst_hp_dis15;
    BnkfPutM ("HXD:PST:EVSEL_HITPIN_DISCD", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_dt_dis0;
    unsigned int hxd_pst_dt_dis1;
    unsigned int hxd_pst_dt_dis2;
    unsigned int hxd_pst_dt_dis3;
    unsigned int hxd_pst_dt_dis4;
    unsigned int hxd_pst_dt_dis5;
    unsigned int hxd_pst_dt_dis6;
    unsigned int hxd_pst_dt_dis7;
    unsigned int hxd_pst_dt_dis8;
    unsigned int hxd_pst_dt_dis9;
    unsigned int hxd_pst_dt_dis10;
    unsigned int hxd_pst_dt_dis11;
    unsigned int hxd_pst_dt_dis12;
    unsigned int hxd_pst_dt_dis13;
    unsigned int hxd_pst_dt_dis14;
    unsigned int hxd_pst_dt_dis15;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS0], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis0, &anynul, &istat);
    data[0] = hxd_pst_dt_dis0;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS1], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis1, &anynul, &istat);
    data[1] = hxd_pst_dt_dis1;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS2], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis2, &anynul, &istat);
    data[2] = hxd_pst_dt_dis2;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS3], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis3, &anynul, &istat);
    data[3] = hxd_pst_dt_dis3;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS4], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis4, &anynul, &istat);
    data[4] = hxd_pst_dt_dis4;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS5], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis5, &anynul, &istat);
    data[5] = hxd_pst_dt_dis5;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS6], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis6, &anynul, &istat);
    data[6] = hxd_pst_dt_dis6;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS7], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis7, &anynul, &istat);
    data[7] = hxd_pst_dt_dis7;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS8], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis8, &anynul, &istat);
    data[8] = hxd_pst_dt_dis8;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS9], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis9, &anynul, &istat);
    data[9] = hxd_pst_dt_dis9;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS10], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis10, &anynul, &istat);
    data[10] = hxd_pst_dt_dis10;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS11], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis11, &anynul, &istat);
    data[11] = hxd_pst_dt_dis11;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS12], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis12, &anynul, &istat);
    data[12] = hxd_pst_dt_dis12;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS13], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis13, &anynul, &istat);
    data[13] = hxd_pst_dt_dis13;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS14], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis14, &anynul, &istat);
    data[14] = hxd_pst_dt_dis14;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_DIS15], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_dis15, &anynul, &istat);
    data[15] = hxd_pst_dt_dis15;
    BnkfPutM ("HXD:PST:EVSEL_DT_DISCD", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_dt_sum0;
    unsigned int hxd_pst_dt_sum1;
    unsigned int hxd_pst_dt_sum2;
    unsigned int hxd_pst_dt_sum3;
    unsigned int hxd_pst_dt_sum4;
    unsigned int hxd_pst_dt_sum5;
    unsigned int hxd_pst_dt_sum6;
    unsigned int hxd_pst_dt_sum7;
    unsigned int hxd_pst_dt_sum8;
    unsigned int hxd_pst_dt_sum9;
    unsigned int hxd_pst_dt_sum10;
    unsigned int hxd_pst_dt_sum11;
    unsigned int hxd_pst_dt_sum12;
    unsigned int hxd_pst_dt_sum13;
    unsigned int hxd_pst_dt_sum14;
    unsigned int hxd_pst_dt_sum15;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM0], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum0, &anynul, &istat);
    data[0] = hxd_pst_dt_sum0;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM1], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum1, &anynul, &istat);
    data[1] = hxd_pst_dt_sum1;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM2], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum2, &anynul, &istat);
    data[2] = hxd_pst_dt_sum2;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM3], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum3, &anynul, &istat);
    data[3] = hxd_pst_dt_sum3;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM4], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum4, &anynul, &istat);
    data[4] = hxd_pst_dt_sum4;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM5], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum5, &anynul, &istat);
    data[5] = hxd_pst_dt_sum5;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM6], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum6, &anynul, &istat);
    data[6] = hxd_pst_dt_sum6;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM7], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum7, &anynul, &istat);
    data[7] = hxd_pst_dt_sum7;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM8], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum8, &anynul, &istat);
    data[8] = hxd_pst_dt_sum8;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM9], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum9, &anynul, &istat);
    data[9] = hxd_pst_dt_sum9;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM10], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum10, &anynul, &istat);
    data[10] = hxd_pst_dt_sum10;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM11], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum11, &anynul, &istat);
    data[11] = hxd_pst_dt_sum11;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM12], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum12, &anynul, &istat);
    data[12] = hxd_pst_dt_sum12;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM13], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum13, &anynul, &istat);
    data[13] = hxd_pst_dt_sum13;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM14], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum14, &anynul, &istat);
    data[14] = hxd_pst_dt_sum14;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_SUM15], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_sum15, &anynul, &istat);
    data[15] = hxd_pst_dt_sum15;
    BnkfPutM ("HXD:PST:EVSEL_DT_SUMT", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_pl_dis0;
    unsigned int hxd_pst_pl_dis1;
    unsigned int hxd_pst_pl_dis2;
    unsigned int hxd_pst_pl_dis3;
    unsigned int hxd_pst_pl_dis4;
    unsigned int hxd_pst_pl_dis5;
    unsigned int hxd_pst_pl_dis6;
    unsigned int hxd_pst_pl_dis7;
    unsigned int hxd_pst_pl_dis8;
    unsigned int hxd_pst_pl_dis9;
    unsigned int hxd_pst_pl_dis10;
    unsigned int hxd_pst_pl_dis11;
    unsigned int hxd_pst_pl_dis12;
    unsigned int hxd_pst_pl_dis13;
    unsigned int hxd_pst_pl_dis14;
    unsigned int hxd_pst_pl_dis15;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS0], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis0, &anynul, &istat);
    data[0] = hxd_pst_pl_dis0;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS1], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis1, &anynul, &istat);
    data[1] = hxd_pst_pl_dis1;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS2], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis2, &anynul, &istat);
    data[2] = hxd_pst_pl_dis2;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS3], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis3, &anynul, &istat);
    data[3] = hxd_pst_pl_dis3;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS4], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis4, &anynul, &istat);
    data[4] = hxd_pst_pl_dis4;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS5], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis5, &anynul, &istat);
    data[5] = hxd_pst_pl_dis5;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS6], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis6, &anynul, &istat);
    data[6] = hxd_pst_pl_dis6;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS7], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis7, &anynul, &istat);
    data[7] = hxd_pst_pl_dis7;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS8], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis8, &anynul, &istat);
    data[8] = hxd_pst_pl_dis8;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS9], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis9, &anynul, &istat);
    data[9] = hxd_pst_pl_dis9;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS10], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis10, &anynul, &istat);
    data[10] = hxd_pst_pl_dis10;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS11], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis11, &anynul, &istat);
    data[11] = hxd_pst_pl_dis11;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS12], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis12, &anynul, &istat);
    data[12] = hxd_pst_pl_dis12;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS13], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis13, &anynul, &istat);
    data[13] = hxd_pst_pl_dis13;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS14], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis14, &anynul, &istat);
    data[14] = hxd_pst_pl_dis14;
    fits_read_col_uint(fp, colnum[HXD_PST_PL_DIS15], irow, firstelem,
                      nelements, nulval, &hxd_pst_pl_dis15, &anynul, &istat);
    data[15] = hxd_pst_pl_dis15;
    BnkfPutM ("HXD:PST:EVSEL_PINLD_DISCD", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_pt_dis0;
    unsigned int hxd_pst_pt_dis1;
    unsigned int hxd_pst_pt_dis2;
    unsigned int hxd_pst_pt_dis3;
    unsigned int hxd_pst_pt_dis4;
    unsigned int hxd_pst_pt_dis5;
    unsigned int hxd_pst_pt_dis6;
    unsigned int hxd_pst_pt_dis7;
    unsigned int hxd_pst_pt_dis8;
    unsigned int hxd_pst_pt_dis9;
    unsigned int hxd_pst_pt_dis10;
    unsigned int hxd_pst_pt_dis11;
    unsigned int hxd_pst_pt_dis12;
    unsigned int hxd_pst_pt_dis13;
    unsigned int hxd_pst_pt_dis14;
    unsigned int hxd_pst_pt_dis15;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS0], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis0, &anynul, &istat);
    data[0] = hxd_pst_pt_dis0;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS1], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis1, &anynul, &istat);
    data[1] = hxd_pst_pt_dis1;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS2], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis2, &anynul, &istat);
    data[2] = hxd_pst_pt_dis2;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS3], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis3, &anynul, &istat);
    data[3] = hxd_pst_pt_dis3;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS4], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis4, &anynul, &istat);
    data[4] = hxd_pst_pt_dis4;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS5], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis5, &anynul, &istat);
    data[5] = hxd_pst_pt_dis5;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS6], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis6, &anynul, &istat);
    data[6] = hxd_pst_pt_dis6;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS7], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis7, &anynul, &istat);
    data[7] = hxd_pst_pt_dis7;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS8], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis8, &anynul, &istat);
    data[8] = hxd_pst_pt_dis8;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS9], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis9, &anynul, &istat);
    data[9] = hxd_pst_pt_dis9;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS10], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis10, &anynul, &istat);
    data[10] = hxd_pst_pt_dis10;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS11], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis11, &anynul, &istat);
    data[11] = hxd_pst_pt_dis11;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS12], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis12, &anynul, &istat);
    data[12] = hxd_pst_pt_dis12;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS13], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis13, &anynul, &istat);
    data[13] = hxd_pst_pt_dis13;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS14], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis14, &anynul, &istat);
    data[14] = hxd_pst_pt_dis14;
    fits_read_col_uint(fp, colnum[HXD_PST_PT_DIS15], irow, firstelem,
                      nelements, nulval, &hxd_pst_pt_dis15, &anynul, &istat);
    data[15] = hxd_pst_pt_dis15;
    BnkfPutM ("HXD:PST:EVSEL_PINTRG_DISCD", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_prev_w0;
    unsigned int hxd_pst_prev_w1;
    unsigned int hxd_pst_prev_w2;
    unsigned int hxd_pst_prev_w3;
    unsigned int hxd_pst_prev_w4;
    unsigned int hxd_pst_prev_w5;
    unsigned int hxd_pst_prev_w6;
    unsigned int hxd_pst_prev_w7;
    unsigned int hxd_pst_prev_w8;
    unsigned int hxd_pst_prev_w9;
    unsigned int hxd_pst_prev_w10;
    unsigned int hxd_pst_prev_w11;
    unsigned int hxd_pst_prev_w12;
    unsigned int hxd_pst_prev_w13;
    unsigned int hxd_pst_prev_w14;
    unsigned int hxd_pst_prev_w15;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W0], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w0, &anynul, &istat);
    data[0] = hxd_pst_prev_w0;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W1], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w1, &anynul, &istat);
    data[1] = hxd_pst_prev_w1;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W2], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w2, &anynul, &istat);
    data[2] = hxd_pst_prev_w2;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W3], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w3, &anynul, &istat);
    data[3] = hxd_pst_prev_w3;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W4], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w4, &anynul, &istat);
    data[4] = hxd_pst_prev_w4;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W5], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w5, &anynul, &istat);
    data[5] = hxd_pst_prev_w5;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W6], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w6, &anynul, &istat);
    data[6] = hxd_pst_prev_w6;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W7], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w7, &anynul, &istat);
    data[7] = hxd_pst_prev_w7;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W8], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w8, &anynul, &istat);
    data[8] = hxd_pst_prev_w8;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W9], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w9, &anynul, &istat);
    data[9] = hxd_pst_prev_w9;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W10], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w10, &anynul, &istat);
    data[10] = hxd_pst_prev_w10;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W11], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w11, &anynul, &istat);
    data[11] = hxd_pst_prev_w11;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W12], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w12, &anynul, &istat);
    data[12] = hxd_pst_prev_w12;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W13], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w13, &anynul, &istat);
    data[13] = hxd_pst_prev_w13;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W14], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w14, &anynul, &istat);
    data[14] = hxd_pst_prev_w14;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_W15], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_w15, &anynul, &istat);
    data[15] = hxd_pst_prev_w15;
    BnkfPutM ("HXD:PST:PREV_PWEL", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_prev_n0;
    unsigned int hxd_pst_prev_n1;
    unsigned int hxd_pst_prev_n2;
    unsigned int hxd_pst_prev_n3;
    unsigned int hxd_pst_prev_n4;
    unsigned int hxd_pst_prev_n5;
    unsigned int hxd_pst_prev_n6;
    unsigned int hxd_pst_prev_n7;
    unsigned int hxd_pst_prev_n8;
    unsigned int hxd_pst_prev_n9;
    unsigned int hxd_pst_prev_n10;
    unsigned int hxd_pst_prev_n11;
    unsigned int hxd_pst_prev_n12;
    unsigned int hxd_pst_prev_n13;
    unsigned int hxd_pst_prev_n14;
    unsigned int hxd_pst_prev_n15;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N0], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n0, &anynul, &istat);
    data[0] = hxd_pst_prev_n0;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N1], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n1, &anynul, &istat);
    data[1] = hxd_pst_prev_n1;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N2], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n2, &anynul, &istat);
    data[2] = hxd_pst_prev_n2;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N3], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n3, &anynul, &istat);
    data[3] = hxd_pst_prev_n3;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N4], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n4, &anynul, &istat);
    data[4] = hxd_pst_prev_n4;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N5], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n5, &anynul, &istat);
    data[5] = hxd_pst_prev_n5;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N6], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n6, &anynul, &istat);
    data[6] = hxd_pst_prev_n6;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N7], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n7, &anynul, &istat);
    data[7] = hxd_pst_prev_n7;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N8], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n8, &anynul, &istat);
    data[8] = hxd_pst_prev_n8;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N9], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n9, &anynul, &istat);
    data[9] = hxd_pst_prev_n9;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N10], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n10, &anynul, &istat);
    data[10] = hxd_pst_prev_n10;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N11], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n11, &anynul, &istat);
    data[11] = hxd_pst_prev_n11;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N12], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n12, &anynul, &istat);
    data[12] = hxd_pst_prev_n12;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N13], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n13, &anynul, &istat);
    data[13] = hxd_pst_prev_n13;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N14], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n14, &anynul, &istat);
    data[14] = hxd_pst_prev_n14;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_N15], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_n15, &anynul, &istat);
    data[15] = hxd_pst_prev_n15;
    BnkfPutM ("HXD:PST:PREV_NON_EXIST", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_prev_i0;
    unsigned int hxd_pst_prev_i1;
    unsigned int hxd_pst_prev_i2;
    unsigned int hxd_pst_prev_i3;
    unsigned int hxd_pst_prev_i4;
    unsigned int hxd_pst_prev_i5;
    unsigned int hxd_pst_prev_i6;
    unsigned int hxd_pst_prev_i7;
    unsigned int hxd_pst_prev_i8;
    unsigned int hxd_pst_prev_i9;
    unsigned int hxd_pst_prev_i10;
    unsigned int hxd_pst_prev_i11;
    unsigned int hxd_pst_prev_i12;
    unsigned int hxd_pst_prev_i13;
    unsigned int hxd_pst_prev_i14;
    unsigned int hxd_pst_prev_i15;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I0], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i0, &anynul, &istat);
    data[0] = hxd_pst_prev_i0;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I1], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i1, &anynul, &istat);
    data[1] = hxd_pst_prev_i1;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I2], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i2, &anynul, &istat);
    data[2] = hxd_pst_prev_i2;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I3], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i3, &anynul, &istat);
    data[3] = hxd_pst_prev_i3;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I4], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i4, &anynul, &istat);
    data[4] = hxd_pst_prev_i4;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I5], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i5, &anynul, &istat);
    data[5] = hxd_pst_prev_i5;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I6], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i6, &anynul, &istat);
    data[6] = hxd_pst_prev_i6;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I7], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i7, &anynul, &istat);
    data[7] = hxd_pst_prev_i7;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I8], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i8, &anynul, &istat);
    data[8] = hxd_pst_prev_i8;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I9], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i9, &anynul, &istat);
    data[9] = hxd_pst_prev_i9;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I10], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i10, &anynul, &istat);
    data[10] = hxd_pst_prev_i10;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I11], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i11, &anynul, &istat);
    data[11] = hxd_pst_prev_i11;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I12], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i12, &anynul, &istat);
    data[12] = hxd_pst_prev_i12;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I13], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i13, &anynul, &istat);
    data[13] = hxd_pst_prev_i13;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I14], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i14, &anynul, &istat);
    data[14] = hxd_pst_prev_i14;
    fits_read_col_uint(fp, colnum[HXD_PST_PREV_I15], irow, firstelem,
                      nelements, nulval, &hxd_pst_prev_i15, &anynul, &istat);
    data[15] = hxd_pst_prev_i15;
    BnkfPutM ("HXD:PST:PREV_INVAL", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_pst_dt_ps0;
    unsigned int hxd_pst_dt_ps1;
    unsigned int hxd_pst_dt_ps2;
    unsigned int hxd_pst_dt_ps3;
    unsigned int hxd_pst_dt_ps4;
    unsigned int hxd_pst_dt_ps5;
    unsigned int hxd_pst_dt_ps6;
    unsigned int hxd_pst_dt_ps7;
    unsigned int hxd_pst_dt_ps8;
    unsigned int hxd_pst_dt_ps9;
    unsigned int hxd_pst_dt_ps10;
    unsigned int hxd_pst_dt_ps11;
    unsigned int hxd_pst_dt_ps12;
    unsigned int hxd_pst_dt_ps13;
    unsigned int hxd_pst_dt_ps14;
    unsigned int hxd_pst_dt_ps15;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS0], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps0, &anynul, &istat);
    data[0] = hxd_pst_dt_ps0;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS1], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps1, &anynul, &istat);
    data[1] = hxd_pst_dt_ps1;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS2], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps2, &anynul, &istat);
    data[2] = hxd_pst_dt_ps2;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS3], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps3, &anynul, &istat);
    data[3] = hxd_pst_dt_ps3;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS4], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps4, &anynul, &istat);
    data[4] = hxd_pst_dt_ps4;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS5], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps5, &anynul, &istat);
    data[5] = hxd_pst_dt_ps5;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS6], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps6, &anynul, &istat);
    data[6] = hxd_pst_dt_ps6;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS7], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps7, &anynul, &istat);
    data[7] = hxd_pst_dt_ps7;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS8], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps8, &anynul, &istat);
    data[8] = hxd_pst_dt_ps8;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS9], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps9, &anynul, &istat);
    data[9] = hxd_pst_dt_ps9;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS10], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps10, &anynul, &istat);
    data[10] = hxd_pst_dt_ps10;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS11], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps11, &anynul, &istat);
    data[11] = hxd_pst_dt_ps11;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS12], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps12, &anynul, &istat);
    data[12] = hxd_pst_dt_ps12;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS13], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps13, &anynul, &istat);
    data[13] = hxd_pst_dt_ps13;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS14], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps14, &anynul, &istat);
    data[14] = hxd_pst_dt_ps14;
    fits_read_col_uint(fp, colnum[HXD_PST_DT_PS15], irow, firstelem,
                      nelements, nulval, &hxd_pst_dt_ps15, &anynul, &istat);
    data[15] = hxd_pst_dt_ps15;
    BnkfPutM ("HXD:PST:EVSEL_DT_PSEUD", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned short nulval=1;
    unsigned short hxd_pst_acu_ti0;
    unsigned short hxd_pst_acu_ti1;
    unsigned short hxd_pst_acu_ti2;
    unsigned short hxd_pst_acu_ti3;
    unsigned short hxd_pst_acu_ti4;
    unsigned short hxd_pst_acu_ti5;
    unsigned short hxd_pst_acu_ti6;
    unsigned short hxd_pst_acu_ti7;
    unsigned short hxd_pst_acu_ti8;
    unsigned short hxd_pst_acu_ti9;
    unsigned short hxd_pst_acu_ti10;
    unsigned short hxd_pst_acu_ti11;
    unsigned short hxd_pst_acu_ti12;
    unsigned short hxd_pst_acu_ti13;
    unsigned short hxd_pst_acu_ti14;
    unsigned short hxd_pst_acu_ti15;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI0], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti0, &anynul, &istat);
    data[0] = hxd_pst_acu_ti0;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI1], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti1, &anynul, &istat);
    data[1] = hxd_pst_acu_ti1;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI2], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti2, &anynul, &istat);
    data[2] = hxd_pst_acu_ti2;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI3], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti3, &anynul, &istat);
    data[3] = hxd_pst_acu_ti3;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI4], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti4, &anynul, &istat);
    data[4] = hxd_pst_acu_ti4;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI5], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti5, &anynul, &istat);
    data[5] = hxd_pst_acu_ti5;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI6], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti6, &anynul, &istat);
    data[6] = hxd_pst_acu_ti6;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI7], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti7, &anynul, &istat);
    data[7] = hxd_pst_acu_ti7;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI8], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti8, &anynul, &istat);
    data[8] = hxd_pst_acu_ti8;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI9], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti9, &anynul, &istat);
    data[9] = hxd_pst_acu_ti9;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI10], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti10, &anynul, &istat);
    data[10] = hxd_pst_acu_ti10;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI11], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti11, &anynul, &istat);
    data[11] = hxd_pst_acu_ti11;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI12], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti12, &anynul, &istat);
    data[12] = hxd_pst_acu_ti12;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI13], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti13, &anynul, &istat);
    data[13] = hxd_pst_acu_ti13;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI14], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti14, &anynul, &istat);
    data[14] = hxd_pst_acu_ti14;
    fits_read_col_usht(fp, colnum[HXD_PST_ACU_TI15], irow, firstelem,
                      nelements, nulval, &hxd_pst_acu_ti15, &anynul, &istat);
    data[15] = hxd_pst_acu_ti15;
    BnkfPutM ("HXD:PST:EVSL_ACU_CNT", sizeof(int)*16, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_pst_bst_req0;
    unsigned short hxd_pst_bst_req1;
    unsigned short hxd_pst_bst_req2;
    unsigned short hxd_pst_bst_req3;
    fits_read_col_usht(fp, colnum[HXD_PST_BST_REQ0], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_req0, &anynul, &istat);
    data[0] = hxd_pst_bst_req0;
    fits_read_col_usht(fp, colnum[HXD_PST_BST_REQ1], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_req1, &anynul, &istat);
    data[1] = hxd_pst_bst_req1;
    fits_read_col_usht(fp, colnum[HXD_PST_BST_REQ2], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_req2, &anynul, &istat);
    data[2] = hxd_pst_bst_req2;
    fits_read_col_usht(fp, colnum[HXD_PST_BST_REQ3], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_req3, &anynul, &istat);
    data[3] = hxd_pst_bst_req3;
    BnkfPutM ("HXD:PST:BST_RQUEST_CNT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_pst_tputmod0;
    unsigned char hxd_pst_tputmod1;
    unsigned char hxd_pst_tputmod2;
    unsigned char hxd_pst_tputmod3;
    fits_read_col_byt(fp, colnum[HXD_PST_TPUTMOD0], irow, firstelem,
                      nelements, nulval, &hxd_pst_tputmod0, &anynul, &istat);
    data[0] = hxd_pst_tputmod0;
    fits_read_col_byt(fp, colnum[HXD_PST_TPUTMOD1], irow, firstelem,
                      nelements, nulval, &hxd_pst_tputmod1, &anynul, &istat);
    data[1] = hxd_pst_tputmod1;
    fits_read_col_byt(fp, colnum[HXD_PST_TPUTMOD2], irow, firstelem,
                      nelements, nulval, &hxd_pst_tputmod2, &anynul, &istat);
    data[2] = hxd_pst_tputmod2;
    fits_read_col_byt(fp, colnum[HXD_PST_TPUTMOD3], irow, firstelem,
                      nelements, nulval, &hxd_pst_tputmod3, &anynul, &istat);
    data[3] = hxd_pst_tputmod3;
    BnkfPutM ("HXD:PST:TPU_TIM_MOD", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pst_bst_trgf;
    fits_read_col_byt(fp, colnum[HXD_PST_BST_TRGF], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_trgf, &anynul, &istat);
    data[0] = hxd_pst_bst_trgf;
    BnkfPutM ("HXD:PST:BST_TRG_FLG", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pst_bst_ph_f;
    fits_read_col_byt(fp, colnum[HXD_PST_BST_PH_F], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_ph_f, &anynul, &istat);
    data[0] = hxd_pst_bst_ph_f;
    BnkfPutM ("HXD:PST:BST_PH_FLG", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pst_bst_wldf;
    fits_read_col_byt(fp, colnum[HXD_PST_BST_WLDF], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_wldf, &anynul, &istat);
    data[0] = hxd_pst_bst_wldf;
    BnkfPutM ("HXD:PST:BST_WLD_FLG", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pst_bst_psef;
    fits_read_col_byt(fp, colnum[HXD_PST_BST_PSEF], irow, firstelem,
                      nelements, nulval, &hxd_pst_bst_psef, &anynul, &istat);
    data[0] = hxd_pst_bst_psef;
    BnkfPutM ("HXD:PST:BST_PSE_FLG", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pst_gb_f_pat;
    fits_read_col_byt(fp, colnum[HXD_PST_GB_F_PAT], irow, firstelem,
                      nelements, nulval, &hxd_pst_gb_f_pat, &anynul, &istat);
    data[0] = hxd_pst_gb_f_pat;
    BnkfPutM ("HXD:PST:GB_FLG_PAT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pst_gb_ena;
    fits_read_col_byt(fp, colnum[HXD_PST_GB_ENA], irow, firstelem,
                      nelements, nulval, &hxd_pst_gb_ena, &anynul, &istat);
    data[0] = hxd_pst_gb_ena;
    BnkfPutM ("HXD:PST:GB_ENA", sizeof(int)*1, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
