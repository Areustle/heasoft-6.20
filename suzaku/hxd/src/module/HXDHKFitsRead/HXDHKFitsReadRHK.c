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
  HXD_ROM_DEST,
  HXD_ROM_CMD_ANS_BK0,
  HXD_ROM_CMD_ANS_BK1,
  HXD_ROM_CMD_ANS_BK2,
  HXD_ROM_CMD_ANS_BK3,
  HXD_ROM_CMD_ANS_BK4,
  HXD_ROM_CMD_ANS_BK5,
  HXD_ROM_CMD_ANS_BK6,
  HXD_ROM_RSV_CMD_LEN,
  HXD_ROM_RSV_CMD_CNT,
  HXD_ROM_CMD_REJ_COD,
  HXD_ROM_CMD_REJ_CNT,
  HXD_ROM_HXD_DIS,
  HXD_ROM_DTRATE,
  HXD_ROM_SW_VERSION,
  HXD_ROM_DP_CNTL_ERR,
  HXD_ROM_TLM_DMA_ERR,
  HXD_ROM_CMD_DMA_ERR,
  HXD_ROM_ERR_COD,
  HXD_ROM_ERR_LOG_CNT,
  HXD_ROM_ERR_TSK_INF,
  HXD_ROM_ILL_INT_CNT,
};

static char pname[] = "HXDHKFitsReadRHK";

static int colnum[22];
static int time_colnum;

void
HXDHKFitsReadRHK_init()
{
  BnkDef( "HXD:RHK:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:RHK:DEST", sizeof(int) );
  BnkDef( "HXD:RHK:CABK", sizeof(int)*7 );
  BnkDef( "HXD:RHK:RCLENG", sizeof(int) );
  BnkDef( "HXD:RHK:RCCNT", sizeof(int) );
  BnkDef( "HXD:RHK:CREJCOD", sizeof(int) );
  BnkDef( "HXD:RHK:CRECNT", sizeof(int) );
  BnkDef( "HXD:RHK:HXDDIS", sizeof(int) );
  BnkDef( "HXD:RHK:DTRATE", sizeof(int) );
  BnkDef( "HXD:RHK:SW_VER", sizeof(int) );
  BnkDef( "HXD:RHK:CNTL_DMAERR", sizeof(int) );
  BnkDef( "HXD:RHK:TLM_DMAERR", sizeof(int) );
  BnkDef( "HXD:RHK:CMD_DMAERR", sizeof(int) );
  BnkDef( "HXD:RHK:ERRCOD", sizeof(int) );
  BnkDef( "HXD:RHK:ERRLOGC", sizeof(int) );
  BnkDef( "HXD:RHK:ERRTINF", sizeof(int) );
  BnkDef( "HXD:RHK:ILINTC", sizeof(int) );
  
}


int
HXDHKFitsReadRHK_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, RHK, &hdutype, &istat );
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
  
  if( fits_get_colnum(fp, casesen, "HXD_ROM_DEST",
                      &colnum[HXD_ROM_DEST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_DEST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_CMD_ANS_BK0",
                      &colnum[HXD_ROM_CMD_ANS_BK0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_CMD_ANS_BK0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_CMD_ANS_BK1",
                      &colnum[HXD_ROM_CMD_ANS_BK1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_CMD_ANS_BK1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_CMD_ANS_BK2",
                      &colnum[HXD_ROM_CMD_ANS_BK2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_CMD_ANS_BK2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_CMD_ANS_BK3",
                      &colnum[HXD_ROM_CMD_ANS_BK3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_CMD_ANS_BK3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_CMD_ANS_BK4",
                      &colnum[HXD_ROM_CMD_ANS_BK4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_CMD_ANS_BK4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_CMD_ANS_BK5",
                      &colnum[HXD_ROM_CMD_ANS_BK5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_CMD_ANS_BK5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_CMD_ANS_BK6",
                      &colnum[HXD_ROM_CMD_ANS_BK6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_CMD_ANS_BK6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_RSV_CMD_LEN",
                      &colnum[HXD_ROM_RSV_CMD_LEN], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_RSV_CMD_LEN') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_RSV_CMD_CNT",
                      &colnum[HXD_ROM_RSV_CMD_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_RSV_CMD_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_CMD_REJ_COD",
                      &colnum[HXD_ROM_CMD_REJ_COD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_CMD_REJ_COD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_CMD_REJ_CNT",
                      &colnum[HXD_ROM_CMD_REJ_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_CMD_REJ_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_HXD_DIS",
                      &colnum[HXD_ROM_HXD_DIS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_HXD_DIS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_DTRATE",
                      &colnum[HXD_ROM_DTRATE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_DTRATE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_SW_VERSION",
                      &colnum[HXD_ROM_SW_VERSION], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_SW_VERSION') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_DP_CNTL_ERR",
                      &colnum[HXD_ROM_DP_CNTL_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_DP_CNTL_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_TLM_DMA_ERR",
                      &colnum[HXD_ROM_TLM_DMA_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_TLM_DMA_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_CMD_DMA_ERR",
                      &colnum[HXD_ROM_CMD_DMA_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_CMD_DMA_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_ERR_COD",
                      &colnum[HXD_ROM_ERR_COD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_ERR_COD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_ERR_LOG_CNT",
                      &colnum[HXD_ROM_ERR_LOG_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_ERR_LOG_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_ERR_TSK_INF",
                      &colnum[HXD_ROM_ERR_TSK_INF], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_ERR_TSK_INF') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_ILL_INT_CNT",
                      &colnum[HXD_ROM_ILL_INT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_ILL_INT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadRHK_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, RHK, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, RHK, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:RHK:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_dest;
    fits_read_col_byt(fp, colnum[HXD_ROM_DEST], irow, firstelem,
                      nelements, nulval, &hxd_rom_dest, &anynul, &istat);
    data[0] = hxd_rom_dest;
    BnkfPutM ("HXD:RHK:DEST", sizeof(int)*1, data);
  }
  {
    unsigned int data[7];
    unsigned char nulval=1;
    unsigned char hxd_rom_cmd_ans_bk0;
    unsigned char hxd_rom_cmd_ans_bk1;
    unsigned char hxd_rom_cmd_ans_bk2;
    unsigned char hxd_rom_cmd_ans_bk3;
    unsigned char hxd_rom_cmd_ans_bk4;
    unsigned char hxd_rom_cmd_ans_bk5;
    unsigned char hxd_rom_cmd_ans_bk6;
    fits_read_col_byt(fp, colnum[HXD_ROM_CMD_ANS_BK0], irow, firstelem,
                      nelements, nulval, &hxd_rom_cmd_ans_bk0, &anynul, &istat);
    data[0] = hxd_rom_cmd_ans_bk0;
    fits_read_col_byt(fp, colnum[HXD_ROM_CMD_ANS_BK1], irow, firstelem,
                      nelements, nulval, &hxd_rom_cmd_ans_bk1, &anynul, &istat);
    data[1] = hxd_rom_cmd_ans_bk1;
    fits_read_col_byt(fp, colnum[HXD_ROM_CMD_ANS_BK2], irow, firstelem,
                      nelements, nulval, &hxd_rom_cmd_ans_bk2, &anynul, &istat);
    data[2] = hxd_rom_cmd_ans_bk2;
    fits_read_col_byt(fp, colnum[HXD_ROM_CMD_ANS_BK3], irow, firstelem,
                      nelements, nulval, &hxd_rom_cmd_ans_bk3, &anynul, &istat);
    data[3] = hxd_rom_cmd_ans_bk3;
    fits_read_col_byt(fp, colnum[HXD_ROM_CMD_ANS_BK4], irow, firstelem,
                      nelements, nulval, &hxd_rom_cmd_ans_bk4, &anynul, &istat);
    data[4] = hxd_rom_cmd_ans_bk4;
    fits_read_col_byt(fp, colnum[HXD_ROM_CMD_ANS_BK5], irow, firstelem,
                      nelements, nulval, &hxd_rom_cmd_ans_bk5, &anynul, &istat);
    data[5] = hxd_rom_cmd_ans_bk5;
    fits_read_col_byt(fp, colnum[HXD_ROM_CMD_ANS_BK6], irow, firstelem,
                      nelements, nulval, &hxd_rom_cmd_ans_bk6, &anynul, &istat);
    data[6] = hxd_rom_cmd_ans_bk6;
    BnkfPutM ("HXD:RHK:CABK", sizeof(int)*7, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_rsv_cmd_len;
    fits_read_col_byt(fp, colnum[HXD_ROM_RSV_CMD_LEN], irow, firstelem,
                      nelements, nulval, &hxd_rom_rsv_cmd_len, &anynul, &istat);
    data[0] = hxd_rom_rsv_cmd_len;
    BnkfPutM ("HXD:RHK:RCLENG", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_rsv_cmd_cnt;
    fits_read_col_byt(fp, colnum[HXD_ROM_RSV_CMD_CNT], irow, firstelem,
                      nelements, nulval, &hxd_rom_rsv_cmd_cnt, &anynul, &istat);
    data[0] = hxd_rom_rsv_cmd_cnt;
    BnkfPutM ("HXD:RHK:RCCNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_cmd_rej_cod;
    fits_read_col_byt(fp, colnum[HXD_ROM_CMD_REJ_COD], irow, firstelem,
                      nelements, nulval, &hxd_rom_cmd_rej_cod, &anynul, &istat);
    data[0] = hxd_rom_cmd_rej_cod;
    BnkfPutM ("HXD:RHK:CREJCOD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_cmd_rej_cnt;
    fits_read_col_byt(fp, colnum[HXD_ROM_CMD_REJ_CNT], irow, firstelem,
                      nelements, nulval, &hxd_rom_cmd_rej_cnt, &anynul, &istat);
    data[0] = hxd_rom_cmd_rej_cnt;
    BnkfPutM ("HXD:RHK:CRECNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_rom_hxd_dis;
    fits_read_col_usht(fp, colnum[HXD_ROM_HXD_DIS], irow, firstelem,
                      nelements, nulval, &hxd_rom_hxd_dis, &anynul, &istat);
    data[0] = hxd_rom_hxd_dis;
    BnkfPutM ("HXD:RHK:HXDDIS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_dtrate;
    fits_read_col_byt(fp, colnum[HXD_ROM_DTRATE], irow, firstelem,
                      nelements, nulval, &hxd_rom_dtrate, &anynul, &istat);
    data[0] = hxd_rom_dtrate;
    BnkfPutM ("HXD:RHK:DTRATE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_rom_sw_version;
    fits_read_col_usht(fp, colnum[HXD_ROM_SW_VERSION], irow, firstelem,
                      nelements, nulval, &hxd_rom_sw_version, &anynul, &istat);
    data[0] = hxd_rom_sw_version;
    BnkfPutM ("HXD:RHK:SW_VER", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_dp_cntl_err;
    fits_read_col_byt(fp, colnum[HXD_ROM_DP_CNTL_ERR], irow, firstelem,
                      nelements, nulval, &hxd_rom_dp_cntl_err, &anynul, &istat);
    data[0] = hxd_rom_dp_cntl_err;
    BnkfPutM ("HXD:RHK:CNTL_DMAERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_tlm_dma_err;
    fits_read_col_byt(fp, colnum[HXD_ROM_TLM_DMA_ERR], irow, firstelem,
                      nelements, nulval, &hxd_rom_tlm_dma_err, &anynul, &istat);
    data[0] = hxd_rom_tlm_dma_err;
    BnkfPutM ("HXD:RHK:TLM_DMAERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_cmd_dma_err;
    fits_read_col_byt(fp, colnum[HXD_ROM_CMD_DMA_ERR], irow, firstelem,
                      nelements, nulval, &hxd_rom_cmd_dma_err, &anynul, &istat);
    data[0] = hxd_rom_cmd_dma_err;
    BnkfPutM ("HXD:RHK:CMD_DMAERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_err_cod;
    fits_read_col_byt(fp, colnum[HXD_ROM_ERR_COD], irow, firstelem,
                      nelements, nulval, &hxd_rom_err_cod, &anynul, &istat);
    data[0] = hxd_rom_err_cod;
    BnkfPutM ("HXD:RHK:ERRCOD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_rom_err_log_cnt;
    fits_read_col_usht(fp, colnum[HXD_ROM_ERR_LOG_CNT], irow, firstelem,
                      nelements, nulval, &hxd_rom_err_log_cnt, &anynul, &istat);
    data[0] = hxd_rom_err_log_cnt;
    BnkfPutM ("HXD:RHK:ERRLOGC", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_err_tsk_inf;
    fits_read_col_byt(fp, colnum[HXD_ROM_ERR_TSK_INF], irow, firstelem,
                      nelements, nulval, &hxd_rom_err_tsk_inf, &anynul, &istat);
    data[0] = hxd_rom_err_tsk_inf;
    BnkfPutM ("HXD:RHK:ERRTINF", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_ill_int_cnt;
    fits_read_col_byt(fp, colnum[HXD_ROM_ILL_INT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_rom_ill_int_cnt, &anynul, &istat);
    data[0] = hxd_rom_ill_int_cnt;
    BnkfPutM ("HXD:RHK:ILINTC", sizeof(int)*1, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
