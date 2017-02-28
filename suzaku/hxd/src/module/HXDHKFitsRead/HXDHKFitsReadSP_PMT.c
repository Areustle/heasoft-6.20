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
  HXD_HIS_TYPE,
  HXD_SP_PMT_IBLOCK,
  HXD_SP_PMT_BUFF_ID,
  HXD_HIS_SFF1_UNIT,
  HXD_HIS_SFF2_UNIT,
  HXD_SP_PMT_BLOCK,
  HXD_HIS_PI_SEQNO,
  HXD_HIS_DISCARD,
  HXD_SP_PMT_HIS_MODE,
  HXD_SP_PMT_PRESET,
  HXD_SP_PMT_HST_COUNT,
  HXD_SP_PMT_HST_PSEUD,
  HXD_SP_PMT_UNITID,
  HXD_SP_PMT_HISTOGRAM,
};

static char pname[] = "HXDHKFitsReadSP_PMT";

static int colnum[14];
static int time_colnum;

void
HXDHKFitsReadSP_PMT_init()
{
  BnkDef( "HXD:SP_PMT:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:SP_PMT:TYPE", sizeof(int) );
  BnkDef( "HXD:SP_PMT:IBLOCK", sizeof(int) );
  BnkDef( "HXD:SP_PMT:BUFF_ID", sizeof(int) );
  BnkDef( "HXD:SP_PMT:SFF1_UNIT", sizeof(int) );
  BnkDef( "HXD:SP_PMT:SFF2_UNIT", sizeof(int) );
  BnkDef( "HXD:SP_PMT:BLOCK", sizeof(int) );
  BnkDef( "HXD:SP_PMT:PI_SEQNO", sizeof(int) );
  BnkDef( "HXD:SP_PMT:DISCARD", sizeof(int) );
  BnkDef( "HXD:SP_PMT:HIS_MODE", sizeof(int) );
  BnkDef( "HXD:SP_PMT:PRESET", sizeof(int) );
  BnkDef( "HXD:SP_PMT:HST_COUNT", sizeof(int) );
  BnkDef( "HXD:SP_PMT:HST_PSEUD", sizeof(int) );
  BnkDef( "HXD:SP_PMT:UNIT", sizeof(int) );
  BnkDef( "HXD:SP_PMT:HISTOGRAM", sizeof(int)*256 );
  
}


int
HXDHKFitsReadSP_PMT_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, SP_PMT, &hdutype, &istat );
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
  
  if( fits_get_colnum(fp, casesen, "HXD_HIS_TYPE",
                      &colnum[HXD_HIS_TYPE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_TYPE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PMT_IBLOCK",
                      &colnum[HXD_SP_PMT_IBLOCK], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PMT_IBLOCK') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PMT_BUFF_ID",
                      &colnum[HXD_SP_PMT_BUFF_ID], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PMT_BUFF_ID') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HIS_SFF1_UNIT",
                      &colnum[HXD_HIS_SFF1_UNIT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_SFF1_UNIT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HIS_SFF2_UNIT",
                      &colnum[HXD_HIS_SFF2_UNIT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_SFF2_UNIT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PMT_BLOCK",
                      &colnum[HXD_SP_PMT_BLOCK], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PMT_BLOCK') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HIS_PI_SEQNO",
                      &colnum[HXD_HIS_PI_SEQNO], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_PI_SEQNO') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HIS_DISCARD",
                      &colnum[HXD_HIS_DISCARD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_DISCARD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PMT_HIS_MODE",
                      &colnum[HXD_SP_PMT_HIS_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PMT_HIS_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PMT_PRESET",
                      &colnum[HXD_SP_PMT_PRESET], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PMT_PRESET') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PMT_HST_COUNT",
                      &colnum[HXD_SP_PMT_HST_COUNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PMT_HST_COUNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PMT_HST_PSEUD",
                      &colnum[HXD_SP_PMT_HST_PSEUD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PMT_HST_PSEUD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PMT_UNITID",
                      &colnum[HXD_SP_PMT_UNITID], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PMT_UNITID') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PMT_HISTOGRAM",
                      &colnum[HXD_SP_PMT_HISTOGRAM], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PMT_HISTOGRAM') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadSP_PMT_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, SP_PMT, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, SP_PMT, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:SP_PMT:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_his_type;
    fits_read_col_byt(fp, colnum[HXD_HIS_TYPE], irow, firstelem,
                      nelements, nulval, &hxd_his_type, &anynul, &istat);
    data[0] = hxd_his_type;
    BnkfPutM ("HXD:SP_PMT:TYPE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    short nulval=1;
    short hxd_sp_pmt_iblock;
    fits_read_col_sht(fp, colnum[HXD_SP_PMT_IBLOCK], irow, firstelem,
                      nelements, nulval, &hxd_sp_pmt_iblock, &anynul, &istat);
    data[0] = hxd_sp_pmt_iblock;
    BnkfPutM ("HXD:SP_PMT:IBLOCK", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_sp_pmt_buff_id;
    fits_read_col_byt(fp, colnum[HXD_SP_PMT_BUFF_ID], irow, firstelem,
                      nelements, nulval, &hxd_sp_pmt_buff_id, &anynul, &istat);
    data[0] = hxd_sp_pmt_buff_id;
    BnkfPutM ("HXD:SP_PMT:BUFF_ID", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_his_sff1_unit;
    fits_read_col_byt(fp, colnum[HXD_HIS_SFF1_UNIT], irow, firstelem,
                      nelements, nulval, &hxd_his_sff1_unit, &anynul, &istat);
    data[0] = hxd_his_sff1_unit;
    BnkfPutM ("HXD:SP_PMT:SFF1_UNIT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_his_sff2_unit;
    fits_read_col_byt(fp, colnum[HXD_HIS_SFF2_UNIT], irow, firstelem,
                      nelements, nulval, &hxd_his_sff2_unit, &anynul, &istat);
    data[0] = hxd_his_sff2_unit;
    BnkfPutM ("HXD:SP_PMT:SFF2_UNIT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_sp_pmt_block;
    fits_read_col_byt(fp, colnum[HXD_SP_PMT_BLOCK], irow, firstelem,
                      nelements, nulval, &hxd_sp_pmt_block, &anynul, &istat);
    data[0] = hxd_sp_pmt_block;
    BnkfPutM ("HXD:SP_PMT:BLOCK", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_his_pi_seqno;
    fits_read_col_usht(fp, colnum[HXD_HIS_PI_SEQNO], irow, firstelem,
                      nelements, nulval, &hxd_his_pi_seqno, &anynul, &istat);
    data[0] = hxd_his_pi_seqno;
    BnkfPutM ("HXD:SP_PMT:PI_SEQNO", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_his_discard;
    fits_read_col_usht(fp, colnum[HXD_HIS_DISCARD], irow, firstelem,
                      nelements, nulval, &hxd_his_discard, &anynul, &istat);
    data[0] = hxd_his_discard;
    BnkfPutM ("HXD:SP_PMT:DISCARD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_sp_pmt_his_mode;
    fits_read_col_byt(fp, colnum[HXD_SP_PMT_HIS_MODE], irow, firstelem,
                      nelements, nulval, &hxd_sp_pmt_his_mode, &anynul, &istat);
    data[0] = hxd_sp_pmt_his_mode;
    BnkfPutM ("HXD:SP_PMT:HIS_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_sp_pmt_preset;
    fits_read_col_usht(fp, colnum[HXD_SP_PMT_PRESET], irow, firstelem,
                      nelements, nulval, &hxd_sp_pmt_preset, &anynul, &istat);
    data[0] = hxd_sp_pmt_preset;
    BnkfPutM ("HXD:SP_PMT:PRESET", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned int nulval=1;
    unsigned int hxd_sp_pmt_hst_count;
    fits_read_col_uint(fp, colnum[HXD_SP_PMT_HST_COUNT], irow, firstelem,
                      nelements, nulval, &hxd_sp_pmt_hst_count, &anynul, &istat);
    data[0] = hxd_sp_pmt_hst_count;
    BnkfPutM ("HXD:SP_PMT:HST_COUNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned int nulval=1;
    unsigned int hxd_sp_pmt_hst_pseud;
    fits_read_col_uint(fp, colnum[HXD_SP_PMT_HST_PSEUD], irow, firstelem,
                      nelements, nulval, &hxd_sp_pmt_hst_pseud, &anynul, &istat);
    data[0] = hxd_sp_pmt_hst_pseud;
    BnkfPutM ("HXD:SP_PMT:HST_PSEUD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_sp_pmt_unitid;
    fits_read_col_byt(fp, colnum[HXD_SP_PMT_UNITID], irow, firstelem,
                      nelements, nulval, &hxd_sp_pmt_unitid, &anynul, &istat);
    data[0] = hxd_sp_pmt_unitid;
    BnkfPutM ("HXD:SP_PMT:UNIT", sizeof(int)*1, data);
  }
  {
    int i;
    unsigned int data[256];
    unsigned short nulval=1;
    unsigned short hxd_sp_pmt_histogram[256];
    fits_read_col_usht(fp, colnum[HXD_SP_PMT_HISTOGRAM], irow, firstelem,
                      256, nulval, hxd_sp_pmt_histogram, &anynul, &istat);
    for(i=0;i<256;i++){
      data[i] = hxd_sp_pmt_histogram[i];
    }
    BnkfPutM ("HXD:SP_PMT:HISTOGRAM", sizeof(int)*256, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}