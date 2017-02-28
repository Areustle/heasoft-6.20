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
  HXD_DMA_BF_SET,
  HXD_DMA_BUFFS,
  HXD_DMA_TOUTS,
  HXD_MON_RDTMS,
  HXD_MON_TOUTS,
  HXD_TRN_RDTMS,
  HXD_TRN_TOUTS,
  HXD_HK_RDTMS,
  HXD_HK_TOUTS,
  HXD_BST_TOUTS,
  HXD_PI_CMD_CNT,
  HXD_PIAE_CMD_CNT,
  HXD_TLATCH_TIME,
  HXD_WEVT0_BREC_CNT,
  HXD_WEVT1_BREC_CNT,
  HXD_WEVT2_BREC_CNT,
  HXD_WEVT3_BREC_CNT,
  HXD_WEVT0_RJC_CNT,
  HXD_WEVT1_RJC_CNT,
  HXD_WEVT2_RJC_CNT,
  HXD_WEVT3_RJC_CNT,
  HXD_GW_CNT,
  HXD_PW_CNT0,
  HXD_PW_CNT1,
  HXD_PW_CNT2,
  HXD_PW_CNT3,
  HXD_W0EVTLMRJCT,
  HXD_W1EVTLMRJCT,
  HXD_W2EVTLMRJCT,
  HXD_W3EVTLMRJCT,
  HXD_W0TLMBFSH,
  HXD_W1TLMBFSH,
  HXD_W2TLMBFSH,
  HXD_W3TLMBFSH,
  HXD_GB_RD_CNT,
  HXD_WEVT0_TOUT_CNT,
  HXD_WEVT1_TOUT_CNT,
  HXD_WEVT2_TOUT_CNT,
  HXD_WEVT3_TOUT_CNT,
  HXD_MON0_REC_CNT,
  HXD_MON1_REC_CNT,
  HXD_MON2_REC_CNT,
  HXD_MON3_REC_CNT,
  HXD_MON0_TOUT_CNT,
  HXD_MON1_TOUT_CNT,
  HXD_MON2_TOUT_CNT,
  HXD_MON3_TOUT_CNT,
  HXD_TRN0_REC_CNT,
  HXD_TRN1_REC_CNT,
  HXD_TRN2_REC_CNT,
  HXD_TRN3_REC_CNT,
  HXD_TR0_TOUT_CNT,
  HXD_TR1_TOUT_CNT,
  HXD_TR2_TOUT_CNT,
  HXD_TR3_TOUT_CNT,
  HXD_GB0_TOUT_CNT,
  HXD_GB1_TOUT_CNT,
  HXD_GB2_TOUT_CNT,
  HXD_GB3_TOUT_CNT,
  HXD_HK_REC_CNT,
  HXD_HK_TOUT_CNT,
  HXD_WEVT_REV_PORT,
  HXD_W0_ERR_CNT,
  HXD_W1_ERR_CNT,
  HXD_W2_ERR_CNT,
  HXD_W3_ERR_CNT,
  HXD_W0_ERR_CODE,
  HXD_W1_ERR_CODE,
  HXD_W2_ERR_CODE,
  HXD_W3_ERR_CODE,
  HXD_MN0_ERR_CNT,
  HXD_MN1_ERR_CNT,
  HXD_MN2_ERR_CNT,
  HXD_MN3_ERR_CNT,
  HXD_MN0_ERR_CODE,
  HXD_MN1_ERR_CODE,
  HXD_MN2_ERR_CODE,
  HXD_MN3_ERR_CODE,
  HXD_TR0_ERR_CNT,
  HXD_TR1_ERR_CNT,
  HXD_TR2_ERR_CNT,
  HXD_TR3_ERR_CNT,
  HXD_TR0_ERR_CODE,
  HXD_TR1_ERR_CODE,
  HXD_TR2_ERR_CODE,
  HXD_TR3_ERR_CODE,
  HXD_HK_ERR_CNT,
  HXD_HK_ERR_CODE,
  HXD_GB0_ERR_CNT,
  HXD_GB1_ERR_CNT,
  HXD_GB2_ERR_CNT,
  HXD_GB3_ERR_CNT,
  HXD_GB0_ERR_CODE,
  HXD_GB1_ERR_CODE,
  HXD_GB2_ERR_CODE,
  HXD_GB3_ERR_CODE,
  HXD_DTCP_ER_CNT,
  HXD_DTCP_ER_CODE,
  HXD_DTCP_ER_DTYP,
  HXD_DTCP_ER_MD,
  HXD_DTCP_ER_SIZE,
  HXD_AEHK_ER_CNT,
  HXD_AEHK_ER_CODE,
  HXD_AEHK_ER_DAT_TYP,
  HXD_AEHK_ER_MD,
};

static char pname[] = "HXDHKFitsReadSYS";

static int colnum[105];
static int time_colnum;

void
HXDHKFitsReadSYS_init()
{
  BnkDef( "HXD:SYS:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:SYS:DMA_BLCKS", sizeof(int) );
  BnkDef( "HXD:SYS:DMA_BUFFS", sizeof(int) );
  BnkDef( "HXD:SYS:DMA_TOUTS", sizeof(int) );
  BnkDef( "HXD:SYS:MON_RDTMS", sizeof(int) );
  BnkDef( "HXD:SYS:MON_TOUTS", sizeof(int) );
  BnkDef( "HXD:SYS:TRN_RDTMS", sizeof(int) );
  BnkDef( "HXD:SYS:TRN_TOUTS", sizeof(int) );
  BnkDef( "HXD:SYS:HK_RDTMS", sizeof(int) );
  BnkDef( "HXD:SYS:HK_TOUTS", sizeof(int) );
  BnkDef( "HXD:SYS:BST_TOUTS", sizeof(int) );
  BnkDef( "HXD:SYS:PICMD_CNT", sizeof(int) );
  BnkDef( "HXD:SYS:AECMD_CNT", sizeof(int) );
  BnkDef( "HXD:SYS:TLTIME", sizeof(int) );
  BnkDef( "HXD:SYS:WEVT_CNT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:WEVT_RCNT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:GET_WCNT", sizeof(int) );
  BnkDef( "HXD:SYS:PUT_WCNT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:WEVTTRCT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:WEVTBFSH", sizeof(int)*4 );
  BnkDef( "HXD:SYS:GB_CNT", sizeof(int) );
  BnkDef( "HXD:SYS:WEVT_TOUT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:MON_CNT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:MON_TOUT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:TRN_CNT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:TRN_TOUT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:GB_TOUT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:HK_CNT", sizeof(int) );
  BnkDef( "HXD:SYS:HK_TOUT", sizeof(int) );
  BnkDef( "HXD:SYS:RCVPRTST", sizeof(int) );
  BnkDef( "HXD:SYS:W_ERRCNT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:W_ERRCD", sizeof(int)*4 );
  BnkDef( "HXD:SYS:MN_ERRCNT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:MN_ERRCD", sizeof(int)*4 );
  BnkDef( "HXD:SYS:TN_ERRCNT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:TN_ERRCD", sizeof(int)*4 );
  BnkDef( "HXD:SYS:HK_ERRCNT", sizeof(int) );
  BnkDef( "HXD:SYS:HK_ERRCD", sizeof(int) );
  BnkDef( "HXD:SYS:GB_ERRCNT", sizeof(int)*4 );
  BnkDef( "HXD:SYS:GB_ERRCD", sizeof(int)*4 );
  BnkDef( "HXD:SYS:DTCP_ERRCNT", sizeof(int) );
  BnkDef( "HXD:SYS:DTCP_ERRCD", sizeof(int) );
  BnkDef( "HXD:SYS:DTCP_ERRDT", sizeof(int) );
  BnkDef( "HXD:SYS:DTCP_ERRBD", sizeof(int) );
  BnkDef( "HXD:SYS:DTCP_DSIZE", sizeof(int) );
  BnkDef( "HXD:SYS:AEHK_ERRCNT", sizeof(int) );
  BnkDef( "HXD:SYS:AEHK_ERRCD", sizeof(int) );
  BnkDef( "HXD:SYS:AEHK_ERRDT", sizeof(int) );
  BnkDef( "HXD:SYS:AEHK_ERRBD", sizeof(int) );
  
}


int
HXDHKFitsReadSYS_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, SYS, &hdutype, &istat );
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
  
  if( fits_get_colnum(fp, casesen, "HXD_DMA_BF_SET",
                      &colnum[HXD_DMA_BF_SET], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DMA_BF_SET') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DMA_BUFFS",
                      &colnum[HXD_DMA_BUFFS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DMA_BUFFS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DMA_TOUTS",
                      &colnum[HXD_DMA_TOUTS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DMA_TOUTS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MON_RDTMS",
                      &colnum[HXD_MON_RDTMS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MON_RDTMS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MON_TOUTS",
                      &colnum[HXD_MON_TOUTS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MON_TOUTS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TRN_RDTMS",
                      &colnum[HXD_TRN_RDTMS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TRN_RDTMS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TRN_TOUTS",
                      &colnum[HXD_TRN_TOUTS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TRN_TOUTS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HK_RDTMS",
                      &colnum[HXD_HK_RDTMS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HK_RDTMS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HK_TOUTS",
                      &colnum[HXD_HK_TOUTS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HK_TOUTS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_BST_TOUTS",
                      &colnum[HXD_BST_TOUTS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_BST_TOUTS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PI_CMD_CNT",
                      &colnum[HXD_PI_CMD_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PI_CMD_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PIAE_CMD_CNT",
                      &colnum[HXD_PIAE_CMD_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PIAE_CMD_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TLATCH_TIME",
                      &colnum[HXD_TLATCH_TIME], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TLATCH_TIME') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT0_BREC_CNT",
                      &colnum[HXD_WEVT0_BREC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT0_BREC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT1_BREC_CNT",
                      &colnum[HXD_WEVT1_BREC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT1_BREC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT2_BREC_CNT",
                      &colnum[HXD_WEVT2_BREC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT2_BREC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT3_BREC_CNT",
                      &colnum[HXD_WEVT3_BREC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT3_BREC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT0_RJC_CNT",
                      &colnum[HXD_WEVT0_RJC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT0_RJC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT1_RJC_CNT",
                      &colnum[HXD_WEVT1_RJC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT1_RJC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT2_RJC_CNT",
                      &colnum[HXD_WEVT2_RJC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT2_RJC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT3_RJC_CNT",
                      &colnum[HXD_WEVT3_RJC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT3_RJC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GW_CNT",
                      &colnum[HXD_GW_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GW_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PW_CNT0",
                      &colnum[HXD_PW_CNT0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PW_CNT0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PW_CNT1",
                      &colnum[HXD_PW_CNT1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PW_CNT1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PW_CNT2",
                      &colnum[HXD_PW_CNT2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PW_CNT2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PW_CNT3",
                      &colnum[HXD_PW_CNT3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PW_CNT3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W0EVTLMRJCT",
                      &colnum[HXD_W0EVTLMRJCT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W0EVTLMRJCT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W1EVTLMRJCT",
                      &colnum[HXD_W1EVTLMRJCT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W1EVTLMRJCT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W2EVTLMRJCT",
                      &colnum[HXD_W2EVTLMRJCT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W2EVTLMRJCT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W3EVTLMRJCT",
                      &colnum[HXD_W3EVTLMRJCT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W3EVTLMRJCT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W0TLMBFSH",
                      &colnum[HXD_W0TLMBFSH], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W0TLMBFSH') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W1TLMBFSH",
                      &colnum[HXD_W1TLMBFSH], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W1TLMBFSH') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W2TLMBFSH",
                      &colnum[HXD_W2TLMBFSH], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W2TLMBFSH') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W3TLMBFSH",
                      &colnum[HXD_W3TLMBFSH], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W3TLMBFSH') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB_RD_CNT",
                      &colnum[HXD_GB_RD_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB_RD_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT0_TOUT_CNT",
                      &colnum[HXD_WEVT0_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT0_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT1_TOUT_CNT",
                      &colnum[HXD_WEVT1_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT1_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT2_TOUT_CNT",
                      &colnum[HXD_WEVT2_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT2_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT3_TOUT_CNT",
                      &colnum[HXD_WEVT3_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT3_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MON0_REC_CNT",
                      &colnum[HXD_MON0_REC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MON0_REC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MON1_REC_CNT",
                      &colnum[HXD_MON1_REC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MON1_REC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MON2_REC_CNT",
                      &colnum[HXD_MON2_REC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MON2_REC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MON3_REC_CNT",
                      &colnum[HXD_MON3_REC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MON3_REC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MON0_TOUT_CNT",
                      &colnum[HXD_MON0_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MON0_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MON1_TOUT_CNT",
                      &colnum[HXD_MON1_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MON1_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MON2_TOUT_CNT",
                      &colnum[HXD_MON2_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MON2_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MON3_TOUT_CNT",
                      &colnum[HXD_MON3_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MON3_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TRN0_REC_CNT",
                      &colnum[HXD_TRN0_REC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TRN0_REC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TRN1_REC_CNT",
                      &colnum[HXD_TRN1_REC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TRN1_REC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TRN2_REC_CNT",
                      &colnum[HXD_TRN2_REC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TRN2_REC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TRN3_REC_CNT",
                      &colnum[HXD_TRN3_REC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TRN3_REC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TR0_TOUT_CNT",
                      &colnum[HXD_TR0_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TR0_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TR1_TOUT_CNT",
                      &colnum[HXD_TR1_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TR1_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TR2_TOUT_CNT",
                      &colnum[HXD_TR2_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TR2_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TR3_TOUT_CNT",
                      &colnum[HXD_TR3_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TR3_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB0_TOUT_CNT",
                      &colnum[HXD_GB0_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB0_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB1_TOUT_CNT",
                      &colnum[HXD_GB1_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB1_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB2_TOUT_CNT",
                      &colnum[HXD_GB2_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB2_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB3_TOUT_CNT",
                      &colnum[HXD_GB3_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB3_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HK_REC_CNT",
                      &colnum[HXD_HK_REC_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HK_REC_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HK_TOUT_CNT",
                      &colnum[HXD_HK_TOUT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HK_TOUT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEVT_REV_PORT",
                      &colnum[HXD_WEVT_REV_PORT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEVT_REV_PORT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W0_ERR_CNT",
                      &colnum[HXD_W0_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W0_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W1_ERR_CNT",
                      &colnum[HXD_W1_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W1_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W2_ERR_CNT",
                      &colnum[HXD_W2_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W2_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W3_ERR_CNT",
                      &colnum[HXD_W3_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W3_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W0_ERR_CODE",
                      &colnum[HXD_W0_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W0_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W1_ERR_CODE",
                      &colnum[HXD_W1_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W1_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W2_ERR_CODE",
                      &colnum[HXD_W2_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W2_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_W3_ERR_CODE",
                      &colnum[HXD_W3_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_W3_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MN0_ERR_CNT",
                      &colnum[HXD_MN0_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MN0_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MN1_ERR_CNT",
                      &colnum[HXD_MN1_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MN1_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MN2_ERR_CNT",
                      &colnum[HXD_MN2_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MN2_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MN3_ERR_CNT",
                      &colnum[HXD_MN3_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MN3_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MN0_ERR_CODE",
                      &colnum[HXD_MN0_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MN0_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MN1_ERR_CODE",
                      &colnum[HXD_MN1_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MN1_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MN2_ERR_CODE",
                      &colnum[HXD_MN2_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MN2_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MN3_ERR_CODE",
                      &colnum[HXD_MN3_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MN3_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TR0_ERR_CNT",
                      &colnum[HXD_TR0_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TR0_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TR1_ERR_CNT",
                      &colnum[HXD_TR1_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TR1_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TR2_ERR_CNT",
                      &colnum[HXD_TR2_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TR2_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TR3_ERR_CNT",
                      &colnum[HXD_TR3_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TR3_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TR0_ERR_CODE",
                      &colnum[HXD_TR0_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TR0_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TR1_ERR_CODE",
                      &colnum[HXD_TR1_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TR1_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TR2_ERR_CODE",
                      &colnum[HXD_TR2_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TR2_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TR3_ERR_CODE",
                      &colnum[HXD_TR3_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TR3_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HK_ERR_CNT",
                      &colnum[HXD_HK_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HK_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HK_ERR_CODE",
                      &colnum[HXD_HK_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HK_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB0_ERR_CNT",
                      &colnum[HXD_GB0_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB0_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB1_ERR_CNT",
                      &colnum[HXD_GB1_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB1_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB2_ERR_CNT",
                      &colnum[HXD_GB2_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB2_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB3_ERR_CNT",
                      &colnum[HXD_GB3_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB3_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB0_ERR_CODE",
                      &colnum[HXD_GB0_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB0_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB1_ERR_CODE",
                      &colnum[HXD_GB1_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB1_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB2_ERR_CODE",
                      &colnum[HXD_GB2_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB2_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GB3_ERR_CODE",
                      &colnum[HXD_GB3_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GB3_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DTCP_ER_CNT",
                      &colnum[HXD_DTCP_ER_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DTCP_ER_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DTCP_ER_CODE",
                      &colnum[HXD_DTCP_ER_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DTCP_ER_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DTCP_ER_DTYP",
                      &colnum[HXD_DTCP_ER_DTYP], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DTCP_ER_DTYP') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DTCP_ER_MD",
                      &colnum[HXD_DTCP_ER_MD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DTCP_ER_MD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DTCP_ER_SIZE",
                      &colnum[HXD_DTCP_ER_SIZE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DTCP_ER_SIZE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AEHK_ER_CNT",
                      &colnum[HXD_AEHK_ER_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AEHK_ER_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AEHK_ER_CODE",
                      &colnum[HXD_AEHK_ER_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AEHK_ER_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AEHK_ER_DAT_TYP",
                      &colnum[HXD_AEHK_ER_DAT_TYP], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AEHK_ER_DAT_TYP') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AEHK_ER_MD",
                      &colnum[HXD_AEHK_ER_MD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AEHK_ER_MD') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadSYS_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, SYS, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, SYS, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:SYS:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_dma_bf_set;
    fits_read_col_byt(fp, colnum[HXD_DMA_BF_SET], irow, firstelem,
                      nelements, nulval, &hxd_dma_bf_set, &anynul, &istat);
    data[0] = hxd_dma_bf_set;
    BnkfPutM ("HXD:SYS:DMA_BLCKS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_dma_buffs;
    fits_read_col_byt(fp, colnum[HXD_DMA_BUFFS], irow, firstelem,
                      nelements, nulval, &hxd_dma_buffs, &anynul, &istat);
    data[0] = hxd_dma_buffs;
    BnkfPutM ("HXD:SYS:DMA_BUFFS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_dma_touts;
    fits_read_col_byt(fp, colnum[HXD_DMA_TOUTS], irow, firstelem,
                      nelements, nulval, &hxd_dma_touts, &anynul, &istat);
    data[0] = hxd_dma_touts;
    BnkfPutM ("HXD:SYS:DMA_TOUTS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_mon_rdtms;
    fits_read_col_byt(fp, colnum[HXD_MON_RDTMS], irow, firstelem,
                      nelements, nulval, &hxd_mon_rdtms, &anynul, &istat);
    data[0] = hxd_mon_rdtms;
    BnkfPutM ("HXD:SYS:MON_RDTMS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_mon_touts;
    fits_read_col_byt(fp, colnum[HXD_MON_TOUTS], irow, firstelem,
                      nelements, nulval, &hxd_mon_touts, &anynul, &istat);
    data[0] = hxd_mon_touts;
    BnkfPutM ("HXD:SYS:MON_TOUTS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_trn_rdtms;
    fits_read_col_byt(fp, colnum[HXD_TRN_RDTMS], irow, firstelem,
                      nelements, nulval, &hxd_trn_rdtms, &anynul, &istat);
    data[0] = hxd_trn_rdtms;
    BnkfPutM ("HXD:SYS:TRN_RDTMS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_trn_touts;
    fits_read_col_byt(fp, colnum[HXD_TRN_TOUTS], irow, firstelem,
                      nelements, nulval, &hxd_trn_touts, &anynul, &istat);
    data[0] = hxd_trn_touts;
    BnkfPutM ("HXD:SYS:TRN_TOUTS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_hk_rdtms;
    fits_read_col_byt(fp, colnum[HXD_HK_RDTMS], irow, firstelem,
                      nelements, nulval, &hxd_hk_rdtms, &anynul, &istat);
    data[0] = hxd_hk_rdtms;
    BnkfPutM ("HXD:SYS:HK_RDTMS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_hk_touts;
    fits_read_col_byt(fp, colnum[HXD_HK_TOUTS], irow, firstelem,
                      nelements, nulval, &hxd_hk_touts, &anynul, &istat);
    data[0] = hxd_hk_touts;
    BnkfPutM ("HXD:SYS:HK_TOUTS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_bst_touts;
    fits_read_col_byt(fp, colnum[HXD_BST_TOUTS], irow, firstelem,
                      nelements, nulval, &hxd_bst_touts, &anynul, &istat);
    data[0] = hxd_bst_touts;
    BnkfPutM ("HXD:SYS:BST_TOUTS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pi_cmd_cnt;
    fits_read_col_byt(fp, colnum[HXD_PI_CMD_CNT], irow, firstelem,
                      nelements, nulval, &hxd_pi_cmd_cnt, &anynul, &istat);
    data[0] = hxd_pi_cmd_cnt;
    BnkfPutM ("HXD:SYS:PICMD_CNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_piae_cmd_cnt;
    fits_read_col_byt(fp, colnum[HXD_PIAE_CMD_CNT], irow, firstelem,
                      nelements, nulval, &hxd_piae_cmd_cnt, &anynul, &istat);
    data[0] = hxd_piae_cmd_cnt;
    BnkfPutM ("HXD:SYS:AECMD_CNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned int nulval=1;
    unsigned int hxd_tlatch_time;
    fits_read_col_uint(fp, colnum[HXD_TLATCH_TIME], irow, firstelem,
                      nelements, nulval, &hxd_tlatch_time, &anynul, &istat);
    data[0] = hxd_tlatch_time;
    BnkfPutM ("HXD:SYS:TLTIME", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_wevt0_brec_cnt;
    unsigned short hxd_wevt1_brec_cnt;
    unsigned short hxd_wevt2_brec_cnt;
    unsigned short hxd_wevt3_brec_cnt;
    fits_read_col_usht(fp, colnum[HXD_WEVT0_BREC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_wevt0_brec_cnt, &anynul, &istat);
    data[0] = hxd_wevt0_brec_cnt;
    fits_read_col_usht(fp, colnum[HXD_WEVT1_BREC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_wevt1_brec_cnt, &anynul, &istat);
    data[1] = hxd_wevt1_brec_cnt;
    fits_read_col_usht(fp, colnum[HXD_WEVT2_BREC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_wevt2_brec_cnt, &anynul, &istat);
    data[2] = hxd_wevt2_brec_cnt;
    fits_read_col_usht(fp, colnum[HXD_WEVT3_BREC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_wevt3_brec_cnt, &anynul, &istat);
    data[3] = hxd_wevt3_brec_cnt;
    BnkfPutM ("HXD:SYS:WEVT_CNT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_wevt0_rjc_cnt;
    unsigned short hxd_wevt1_rjc_cnt;
    unsigned short hxd_wevt2_rjc_cnt;
    unsigned short hxd_wevt3_rjc_cnt;
    fits_read_col_usht(fp, colnum[HXD_WEVT0_RJC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_wevt0_rjc_cnt, &anynul, &istat);
    data[0] = hxd_wevt0_rjc_cnt;
    fits_read_col_usht(fp, colnum[HXD_WEVT1_RJC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_wevt1_rjc_cnt, &anynul, &istat);
    data[1] = hxd_wevt1_rjc_cnt;
    fits_read_col_usht(fp, colnum[HXD_WEVT2_RJC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_wevt2_rjc_cnt, &anynul, &istat);
    data[2] = hxd_wevt2_rjc_cnt;
    fits_read_col_usht(fp, colnum[HXD_WEVT3_RJC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_wevt3_rjc_cnt, &anynul, &istat);
    data[3] = hxd_wevt3_rjc_cnt;
    BnkfPutM ("HXD:SYS:WEVT_RCNT", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_gw_cnt;
    fits_read_col_usht(fp, colnum[HXD_GW_CNT], irow, firstelem,
                      nelements, nulval, &hxd_gw_cnt, &anynul, &istat);
    data[0] = hxd_gw_cnt;
    BnkfPutM ("HXD:SYS:GET_WCNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_pw_cnt0;
    unsigned short hxd_pw_cnt1;
    unsigned short hxd_pw_cnt2;
    unsigned short hxd_pw_cnt3;
    fits_read_col_usht(fp, colnum[HXD_PW_CNT0], irow, firstelem,
                      nelements, nulval, &hxd_pw_cnt0, &anynul, &istat);
    data[0] = hxd_pw_cnt0;
    fits_read_col_usht(fp, colnum[HXD_PW_CNT1], irow, firstelem,
                      nelements, nulval, &hxd_pw_cnt1, &anynul, &istat);
    data[1] = hxd_pw_cnt1;
    fits_read_col_usht(fp, colnum[HXD_PW_CNT2], irow, firstelem,
                      nelements, nulval, &hxd_pw_cnt2, &anynul, &istat);
    data[2] = hxd_pw_cnt2;
    fits_read_col_usht(fp, colnum[HXD_PW_CNT3], irow, firstelem,
                      nelements, nulval, &hxd_pw_cnt3, &anynul, &istat);
    data[3] = hxd_pw_cnt3;
    BnkfPutM ("HXD:SYS:PUT_WCNT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_w0evtlmrjct;
    unsigned short hxd_w1evtlmrjct;
    unsigned short hxd_w2evtlmrjct;
    unsigned short hxd_w3evtlmrjct;
    fits_read_col_usht(fp, colnum[HXD_W0EVTLMRJCT], irow, firstelem,
                      nelements, nulval, &hxd_w0evtlmrjct, &anynul, &istat);
    data[0] = hxd_w0evtlmrjct;
    fits_read_col_usht(fp, colnum[HXD_W1EVTLMRJCT], irow, firstelem,
                      nelements, nulval, &hxd_w1evtlmrjct, &anynul, &istat);
    data[1] = hxd_w1evtlmrjct;
    fits_read_col_usht(fp, colnum[HXD_W2EVTLMRJCT], irow, firstelem,
                      nelements, nulval, &hxd_w2evtlmrjct, &anynul, &istat);
    data[2] = hxd_w2evtlmrjct;
    fits_read_col_usht(fp, colnum[HXD_W3EVTLMRJCT], irow, firstelem,
                      nelements, nulval, &hxd_w3evtlmrjct, &anynul, &istat);
    data[3] = hxd_w3evtlmrjct;
    BnkfPutM ("HXD:SYS:WEVTTRCT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_w0tlmbfsh;
    unsigned short hxd_w1tlmbfsh;
    unsigned short hxd_w2tlmbfsh;
    unsigned short hxd_w3tlmbfsh;
    fits_read_col_usht(fp, colnum[HXD_W0TLMBFSH], irow, firstelem,
                      nelements, nulval, &hxd_w0tlmbfsh, &anynul, &istat);
    data[0] = hxd_w0tlmbfsh;
    fits_read_col_usht(fp, colnum[HXD_W1TLMBFSH], irow, firstelem,
                      nelements, nulval, &hxd_w1tlmbfsh, &anynul, &istat);
    data[1] = hxd_w1tlmbfsh;
    fits_read_col_usht(fp, colnum[HXD_W2TLMBFSH], irow, firstelem,
                      nelements, nulval, &hxd_w2tlmbfsh, &anynul, &istat);
    data[2] = hxd_w2tlmbfsh;
    fits_read_col_usht(fp, colnum[HXD_W3TLMBFSH], irow, firstelem,
                      nelements, nulval, &hxd_w3tlmbfsh, &anynul, &istat);
    data[3] = hxd_w3tlmbfsh;
    BnkfPutM ("HXD:SYS:WEVTBFSH", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_gb_rd_cnt;
    fits_read_col_usht(fp, colnum[HXD_GB_RD_CNT], irow, firstelem,
                      nelements, nulval, &hxd_gb_rd_cnt, &anynul, &istat);
    data[0] = hxd_gb_rd_cnt;
    BnkfPutM ("HXD:SYS:GB_CNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_wevt0_tout_cnt;
    unsigned short hxd_wevt1_tout_cnt;
    unsigned short hxd_wevt2_tout_cnt;
    unsigned short hxd_wevt3_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_WEVT0_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_wevt0_tout_cnt, &anynul, &istat);
    data[0] = hxd_wevt0_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_WEVT1_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_wevt1_tout_cnt, &anynul, &istat);
    data[1] = hxd_wevt1_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_WEVT2_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_wevt2_tout_cnt, &anynul, &istat);
    data[2] = hxd_wevt2_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_WEVT3_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_wevt3_tout_cnt, &anynul, &istat);
    data[3] = hxd_wevt3_tout_cnt;
    BnkfPutM ("HXD:SYS:WEVT_TOUT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_mon0_rec_cnt;
    unsigned short hxd_mon1_rec_cnt;
    unsigned short hxd_mon2_rec_cnt;
    unsigned short hxd_mon3_rec_cnt;
    fits_read_col_usht(fp, colnum[HXD_MON0_REC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_mon0_rec_cnt, &anynul, &istat);
    data[0] = hxd_mon0_rec_cnt;
    fits_read_col_usht(fp, colnum[HXD_MON1_REC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_mon1_rec_cnt, &anynul, &istat);
    data[1] = hxd_mon1_rec_cnt;
    fits_read_col_usht(fp, colnum[HXD_MON2_REC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_mon2_rec_cnt, &anynul, &istat);
    data[2] = hxd_mon2_rec_cnt;
    fits_read_col_usht(fp, colnum[HXD_MON3_REC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_mon3_rec_cnt, &anynul, &istat);
    data[3] = hxd_mon3_rec_cnt;
    BnkfPutM ("HXD:SYS:MON_CNT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_mon0_tout_cnt;
    unsigned short hxd_mon1_tout_cnt;
    unsigned short hxd_mon2_tout_cnt;
    unsigned short hxd_mon3_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_MON0_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_mon0_tout_cnt, &anynul, &istat);
    data[0] = hxd_mon0_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_MON1_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_mon1_tout_cnt, &anynul, &istat);
    data[1] = hxd_mon1_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_MON2_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_mon2_tout_cnt, &anynul, &istat);
    data[2] = hxd_mon2_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_MON3_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_mon3_tout_cnt, &anynul, &istat);
    data[3] = hxd_mon3_tout_cnt;
    BnkfPutM ("HXD:SYS:MON_TOUT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_trn0_rec_cnt;
    unsigned short hxd_trn1_rec_cnt;
    unsigned short hxd_trn2_rec_cnt;
    unsigned short hxd_trn3_rec_cnt;
    fits_read_col_usht(fp, colnum[HXD_TRN0_REC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_trn0_rec_cnt, &anynul, &istat);
    data[0] = hxd_trn0_rec_cnt;
    fits_read_col_usht(fp, colnum[HXD_TRN1_REC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_trn1_rec_cnt, &anynul, &istat);
    data[1] = hxd_trn1_rec_cnt;
    fits_read_col_usht(fp, colnum[HXD_TRN2_REC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_trn2_rec_cnt, &anynul, &istat);
    data[2] = hxd_trn2_rec_cnt;
    fits_read_col_usht(fp, colnum[HXD_TRN3_REC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_trn3_rec_cnt, &anynul, &istat);
    data[3] = hxd_trn3_rec_cnt;
    BnkfPutM ("HXD:SYS:TRN_CNT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_tr0_tout_cnt;
    unsigned short hxd_tr1_tout_cnt;
    unsigned short hxd_tr2_tout_cnt;
    unsigned short hxd_tr3_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_TR0_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_tr0_tout_cnt, &anynul, &istat);
    data[0] = hxd_tr0_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_TR1_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_tr1_tout_cnt, &anynul, &istat);
    data[1] = hxd_tr1_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_TR2_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_tr2_tout_cnt, &anynul, &istat);
    data[2] = hxd_tr2_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_TR3_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_tr3_tout_cnt, &anynul, &istat);
    data[3] = hxd_tr3_tout_cnt;
    BnkfPutM ("HXD:SYS:TRN_TOUT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_gb0_tout_cnt;
    unsigned short hxd_gb1_tout_cnt;
    unsigned short hxd_gb2_tout_cnt;
    unsigned short hxd_gb3_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_GB0_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_gb0_tout_cnt, &anynul, &istat);
    data[0] = hxd_gb0_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_GB1_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_gb1_tout_cnt, &anynul, &istat);
    data[1] = hxd_gb1_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_GB2_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_gb2_tout_cnt, &anynul, &istat);
    data[2] = hxd_gb2_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_GB3_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_gb3_tout_cnt, &anynul, &istat);
    data[3] = hxd_gb3_tout_cnt;
    BnkfPutM ("HXD:SYS:GB_TOUT", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_hk_rec_cnt;
    fits_read_col_usht(fp, colnum[HXD_HK_REC_CNT], irow, firstelem,
                      nelements, nulval, &hxd_hk_rec_cnt, &anynul, &istat);
    data[0] = hxd_hk_rec_cnt;
    BnkfPutM ("HXD:SYS:HK_CNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_hk_tout_cnt;
    fits_read_col_usht(fp, colnum[HXD_HK_TOUT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_hk_tout_cnt, &anynul, &istat);
    data[0] = hxd_hk_tout_cnt;
    BnkfPutM ("HXD:SYS:HK_TOUT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_wevt_rev_port;
    fits_read_col_byt(fp, colnum[HXD_WEVT_REV_PORT], irow, firstelem,
                      nelements, nulval, &hxd_wevt_rev_port, &anynul, &istat);
    data[0] = hxd_wevt_rev_port;
    BnkfPutM ("HXD:SYS:RCVPRTST", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_w0_err_cnt;
    unsigned char hxd_w1_err_cnt;
    unsigned char hxd_w2_err_cnt;
    unsigned char hxd_w3_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_W0_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_w0_err_cnt, &anynul, &istat);
    data[0] = hxd_w0_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_W1_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_w1_err_cnt, &anynul, &istat);
    data[1] = hxd_w1_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_W2_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_w2_err_cnt, &anynul, &istat);
    data[2] = hxd_w2_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_W3_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_w3_err_cnt, &anynul, &istat);
    data[3] = hxd_w3_err_cnt;
    BnkfPutM ("HXD:SYS:W_ERRCNT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_w0_err_code;
    unsigned char hxd_w1_err_code;
    unsigned char hxd_w2_err_code;
    unsigned char hxd_w3_err_code;
    fits_read_col_byt(fp, colnum[HXD_W0_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_w0_err_code, &anynul, &istat);
    data[0] = hxd_w0_err_code;
    fits_read_col_byt(fp, colnum[HXD_W1_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_w1_err_code, &anynul, &istat);
    data[1] = hxd_w1_err_code;
    fits_read_col_byt(fp, colnum[HXD_W2_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_w2_err_code, &anynul, &istat);
    data[2] = hxd_w2_err_code;
    fits_read_col_byt(fp, colnum[HXD_W3_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_w3_err_code, &anynul, &istat);
    data[3] = hxd_w3_err_code;
    BnkfPutM ("HXD:SYS:W_ERRCD", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_mn0_err_cnt;
    unsigned char hxd_mn1_err_cnt;
    unsigned char hxd_mn2_err_cnt;
    unsigned char hxd_mn3_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_MN0_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_mn0_err_cnt, &anynul, &istat);
    data[0] = hxd_mn0_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_MN1_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_mn1_err_cnt, &anynul, &istat);
    data[1] = hxd_mn1_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_MN2_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_mn2_err_cnt, &anynul, &istat);
    data[2] = hxd_mn2_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_MN3_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_mn3_err_cnt, &anynul, &istat);
    data[3] = hxd_mn3_err_cnt;
    BnkfPutM ("HXD:SYS:MN_ERRCNT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_mn0_err_code;
    unsigned char hxd_mn1_err_code;
    unsigned char hxd_mn2_err_code;
    unsigned char hxd_mn3_err_code;
    fits_read_col_byt(fp, colnum[HXD_MN0_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_mn0_err_code, &anynul, &istat);
    data[0] = hxd_mn0_err_code;
    fits_read_col_byt(fp, colnum[HXD_MN1_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_mn1_err_code, &anynul, &istat);
    data[1] = hxd_mn1_err_code;
    fits_read_col_byt(fp, colnum[HXD_MN2_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_mn2_err_code, &anynul, &istat);
    data[2] = hxd_mn2_err_code;
    fits_read_col_byt(fp, colnum[HXD_MN3_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_mn3_err_code, &anynul, &istat);
    data[3] = hxd_mn3_err_code;
    BnkfPutM ("HXD:SYS:MN_ERRCD", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_tr0_err_cnt;
    unsigned char hxd_tr1_err_cnt;
    unsigned char hxd_tr2_err_cnt;
    unsigned char hxd_tr3_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_TR0_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_tr0_err_cnt, &anynul, &istat);
    data[0] = hxd_tr0_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_TR1_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_tr1_err_cnt, &anynul, &istat);
    data[1] = hxd_tr1_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_TR2_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_tr2_err_cnt, &anynul, &istat);
    data[2] = hxd_tr2_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_TR3_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_tr3_err_cnt, &anynul, &istat);
    data[3] = hxd_tr3_err_cnt;
    BnkfPutM ("HXD:SYS:TN_ERRCNT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_tr0_err_code;
    unsigned char hxd_tr1_err_code;
    unsigned char hxd_tr2_err_code;
    unsigned char hxd_tr3_err_code;
    fits_read_col_byt(fp, colnum[HXD_TR0_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_tr0_err_code, &anynul, &istat);
    data[0] = hxd_tr0_err_code;
    fits_read_col_byt(fp, colnum[HXD_TR1_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_tr1_err_code, &anynul, &istat);
    data[1] = hxd_tr1_err_code;
    fits_read_col_byt(fp, colnum[HXD_TR2_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_tr2_err_code, &anynul, &istat);
    data[2] = hxd_tr2_err_code;
    fits_read_col_byt(fp, colnum[HXD_TR3_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_tr3_err_code, &anynul, &istat);
    data[3] = hxd_tr3_err_code;
    BnkfPutM ("HXD:SYS:TN_ERRCD", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_hk_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_HK_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_hk_err_cnt, &anynul, &istat);
    data[0] = hxd_hk_err_cnt;
    BnkfPutM ("HXD:SYS:HK_ERRCNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_hk_err_code;
    fits_read_col_byt(fp, colnum[HXD_HK_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_hk_err_code, &anynul, &istat);
    data[0] = hxd_hk_err_code;
    BnkfPutM ("HXD:SYS:HK_ERRCD", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_gb0_err_cnt;
    unsigned char hxd_gb1_err_cnt;
    unsigned char hxd_gb2_err_cnt;
    unsigned char hxd_gb3_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_GB0_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_gb0_err_cnt, &anynul, &istat);
    data[0] = hxd_gb0_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_GB1_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_gb1_err_cnt, &anynul, &istat);
    data[1] = hxd_gb1_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_GB2_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_gb2_err_cnt, &anynul, &istat);
    data[2] = hxd_gb2_err_cnt;
    fits_read_col_byt(fp, colnum[HXD_GB3_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_gb3_err_cnt, &anynul, &istat);
    data[3] = hxd_gb3_err_cnt;
    BnkfPutM ("HXD:SYS:GB_ERRCNT", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_gb0_err_code;
    unsigned char hxd_gb1_err_code;
    unsigned char hxd_gb2_err_code;
    unsigned char hxd_gb3_err_code;
    fits_read_col_byt(fp, colnum[HXD_GB0_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_gb0_err_code, &anynul, &istat);
    data[0] = hxd_gb0_err_code;
    fits_read_col_byt(fp, colnum[HXD_GB1_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_gb1_err_code, &anynul, &istat);
    data[1] = hxd_gb1_err_code;
    fits_read_col_byt(fp, colnum[HXD_GB2_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_gb2_err_code, &anynul, &istat);
    data[2] = hxd_gb2_err_code;
    fits_read_col_byt(fp, colnum[HXD_GB3_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_gb3_err_code, &anynul, &istat);
    data[3] = hxd_gb3_err_code;
    BnkfPutM ("HXD:SYS:GB_ERRCD", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_dtcp_er_cnt;
    fits_read_col_byt(fp, colnum[HXD_DTCP_ER_CNT], irow, firstelem,
                      nelements, nulval, &hxd_dtcp_er_cnt, &anynul, &istat);
    data[0] = hxd_dtcp_er_cnt;
    BnkfPutM ("HXD:SYS:DTCP_ERRCNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_dtcp_er_code;
    fits_read_col_byt(fp, colnum[HXD_DTCP_ER_CODE], irow, firstelem,
                      nelements, nulval, &hxd_dtcp_er_code, &anynul, &istat);
    data[0] = hxd_dtcp_er_code;
    BnkfPutM ("HXD:SYS:DTCP_ERRCD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_dtcp_er_dtyp;
    fits_read_col_byt(fp, colnum[HXD_DTCP_ER_DTYP], irow, firstelem,
                      nelements, nulval, &hxd_dtcp_er_dtyp, &anynul, &istat);
    data[0] = hxd_dtcp_er_dtyp;
    BnkfPutM ("HXD:SYS:DTCP_ERRDT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_dtcp_er_md;
    fits_read_col_byt(fp, colnum[HXD_DTCP_ER_MD], irow, firstelem,
                      nelements, nulval, &hxd_dtcp_er_md, &anynul, &istat);
    data[0] = hxd_dtcp_er_md;
    BnkfPutM ("HXD:SYS:DTCP_ERRBD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_dtcp_er_size;
    fits_read_col_byt(fp, colnum[HXD_DTCP_ER_SIZE], irow, firstelem,
                      nelements, nulval, &hxd_dtcp_er_size, &anynul, &istat);
    data[0] = hxd_dtcp_er_size;
    BnkfPutM ("HXD:SYS:DTCP_DSIZE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aehk_er_cnt;
    fits_read_col_byt(fp, colnum[HXD_AEHK_ER_CNT], irow, firstelem,
                      nelements, nulval, &hxd_aehk_er_cnt, &anynul, &istat);
    data[0] = hxd_aehk_er_cnt;
    BnkfPutM ("HXD:SYS:AEHK_ERRCNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aehk_er_code;
    fits_read_col_byt(fp, colnum[HXD_AEHK_ER_CODE], irow, firstelem,
                      nelements, nulval, &hxd_aehk_er_code, &anynul, &istat);
    data[0] = hxd_aehk_er_code;
    BnkfPutM ("HXD:SYS:AEHK_ERRCD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aehk_er_dat_typ;
    fits_read_col_byt(fp, colnum[HXD_AEHK_ER_DAT_TYP], irow, firstelem,
                      nelements, nulval, &hxd_aehk_er_dat_typ, &anynul, &istat);
    data[0] = hxd_aehk_er_dat_typ;
    BnkfPutM ("HXD:SYS:AEHK_ERRDT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aehk_er_md;
    fits_read_col_byt(fp, colnum[HXD_AEHK_ER_MD], irow, firstelem,
                      nelements, nulval, &hxd_aehk_er_md, &anynul, &istat);
    data[0] = hxd_aehk_er_md;
    BnkfPutM ("HXD:SYS:AEHK_ERRBD", sizeof(int)*1, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
