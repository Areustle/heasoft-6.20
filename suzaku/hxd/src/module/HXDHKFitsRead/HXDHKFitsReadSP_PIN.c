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
  HXD_SP_PIN_IBLOCK,
  HXD_SP_PIN_BUFF_ID,
  HXD_HIS_SFF1_UNIT,
  HXD_HIS_SFF2_UNIT,
  HXD_SP_PIN_BLOCK,
  HXD_HIS_PI_SEQNO,
  HXD_HIS_DISCARD,
  HXD_SP_PIN_HIS_MODE,
  HXD_SP_PIN_PRESET,
  HXD_SP_PIN_HST_COUNT,
  HXD_SP_PIN_HST_PSEUD,
  HXD_SP_PIN_UNITID,
  HXD_SP_PIN_HISTOGRAM,
};

static char pname[] = "HXDHKFitsReadSP_PIN";

static int colnum[14];
static int time_colnum;

void
HXDHKFitsReadSP_PIN_init()
{
  BnkDef( "HXD:SP_PIN:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:SP_PIN:TYPE", sizeof(int) );
  BnkDef( "HXD:SP_PIN:IBLOCK", sizeof(int) );
  BnkDef( "HXD:SP_PIN:BUFF_ID", sizeof(int) );
  BnkDef( "HXD:SP_PIN:SFF1_UNIT", sizeof(int) );
  BnkDef( "HXD:SP_PIN:SFF2_UNIT", sizeof(int) );
  BnkDef( "HXD:SP_PIN:BLOCK", sizeof(int) );
  BnkDef( "HXD:SP_PIN:PI_SEQNO", sizeof(int) );
  BnkDef( "HXD:SP_PIN:DISCARD", sizeof(int) );
  BnkDef( "HXD:SP_PIN:HIS_MODE", sizeof(int) );
  BnkDef( "HXD:SP_PIN:PRESET", sizeof(int) );
  BnkDef( "HXD:SP_PIN:HST_COUNT", sizeof(int) );
  BnkDef( "HXD:SP_PIN:HST_PSEUD", sizeof(int) );
  BnkDef( "HXD:SP_PIN:UNIT", sizeof(int) );
  BnkDef( "HXD:SP_PIN:HISTOGRAM", sizeof(int)*256 );
  
}


int
HXDHKFitsReadSP_PIN_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, SP_PIN, &hdutype, &istat );
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
  if( fits_get_colnum(fp, casesen, "HXD_SP_PIN_IBLOCK",
                      &colnum[HXD_SP_PIN_IBLOCK], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PIN_IBLOCK') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PIN_BUFF_ID",
                      &colnum[HXD_SP_PIN_BUFF_ID], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PIN_BUFF_ID') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HIS_SFF1_UNIT",
                      &colnum[HXD_HIS_SFF1_UNIT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_SFF1_UNIT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HIS_SFF2_UNIT",
                      &colnum[HXD_HIS_SFF2_UNIT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_SFF2_UNIT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PIN_BLOCK",
                      &colnum[HXD_SP_PIN_BLOCK], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PIN_BLOCK') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HIS_PI_SEQNO",
                      &colnum[HXD_HIS_PI_SEQNO], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_PI_SEQNO') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HIS_DISCARD",
                      &colnum[HXD_HIS_DISCARD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_DISCARD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PIN_HIS_MODE",
                      &colnum[HXD_SP_PIN_HIS_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PIN_HIS_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PIN_PRESET",
                      &colnum[HXD_SP_PIN_PRESET], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PIN_PRESET') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PIN_HST_COUNT",
                      &colnum[HXD_SP_PIN_HST_COUNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PIN_HST_COUNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PIN_HST_PSEUD",
                      &colnum[HXD_SP_PIN_HST_PSEUD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PIN_HST_PSEUD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PIN_UNITID",
                      &colnum[HXD_SP_PIN_UNITID], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PIN_UNITID') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SP_PIN_HISTOGRAM",
                      &colnum[HXD_SP_PIN_HISTOGRAM], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SP_PIN_HISTOGRAM') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadSP_PIN_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, SP_PIN, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, SP_PIN, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:SP_PIN:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_his_type;
    fits_read_col_byt(fp, colnum[HXD_HIS_TYPE], irow, firstelem,
                      nelements, nulval, &hxd_his_type, &anynul, &istat);
    data[0] = hxd_his_type;
    BnkfPutM ("HXD:SP_PIN:TYPE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    short nulval=1;
    short hxd_sp_pin_iblock;
    fits_read_col_sht(fp, colnum[HXD_SP_PIN_IBLOCK], irow, firstelem,
                      nelements, nulval, &hxd_sp_pin_iblock, &anynul, &istat);
    data[0] = hxd_sp_pin_iblock;
    BnkfPutM ("HXD:SP_PIN:IBLOCK", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_sp_pin_buff_id;
    fits_read_col_byt(fp, colnum[HXD_SP_PIN_BUFF_ID], irow, firstelem,
                      nelements, nulval, &hxd_sp_pin_buff_id, &anynul, &istat);
    data[0] = hxd_sp_pin_buff_id;
    BnkfPutM ("HXD:SP_PIN:BUFF_ID", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_his_sff1_unit;
    fits_read_col_byt(fp, colnum[HXD_HIS_SFF1_UNIT], irow, firstelem,
                      nelements, nulval, &hxd_his_sff1_unit, &anynul, &istat);
    data[0] = hxd_his_sff1_unit;
    BnkfPutM ("HXD:SP_PIN:SFF1_UNIT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_his_sff2_unit;
    fits_read_col_byt(fp, colnum[HXD_HIS_SFF2_UNIT], irow, firstelem,
                      nelements, nulval, &hxd_his_sff2_unit, &anynul, &istat);
    data[0] = hxd_his_sff2_unit;
    BnkfPutM ("HXD:SP_PIN:SFF2_UNIT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_sp_pin_block;
    fits_read_col_byt(fp, colnum[HXD_SP_PIN_BLOCK], irow, firstelem,
                      nelements, nulval, &hxd_sp_pin_block, &anynul, &istat);
    data[0] = hxd_sp_pin_block;
    BnkfPutM ("HXD:SP_PIN:BLOCK", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_his_pi_seqno;
    fits_read_col_usht(fp, colnum[HXD_HIS_PI_SEQNO], irow, firstelem,
                      nelements, nulval, &hxd_his_pi_seqno, &anynul, &istat);
    data[0] = hxd_his_pi_seqno;
    BnkfPutM ("HXD:SP_PIN:PI_SEQNO", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_his_discard;
    fits_read_col_usht(fp, colnum[HXD_HIS_DISCARD], irow, firstelem,
                      nelements, nulval, &hxd_his_discard, &anynul, &istat);
    data[0] = hxd_his_discard;
    BnkfPutM ("HXD:SP_PIN:DISCARD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_sp_pin_his_mode;
    fits_read_col_byt(fp, colnum[HXD_SP_PIN_HIS_MODE], irow, firstelem,
                      nelements, nulval, &hxd_sp_pin_his_mode, &anynul, &istat);
    data[0] = hxd_sp_pin_his_mode;
    BnkfPutM ("HXD:SP_PIN:HIS_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_sp_pin_preset;
    fits_read_col_usht(fp, colnum[HXD_SP_PIN_PRESET], irow, firstelem,
                      nelements, nulval, &hxd_sp_pin_preset, &anynul, &istat);
    data[0] = hxd_sp_pin_preset;
    BnkfPutM ("HXD:SP_PIN:PRESET", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned int nulval=1;
    unsigned int hxd_sp_pin_hst_count;
    fits_read_col_uint(fp, colnum[HXD_SP_PIN_HST_COUNT], irow, firstelem,
                      nelements, nulval, &hxd_sp_pin_hst_count, &anynul, &istat);
    data[0] = hxd_sp_pin_hst_count;
    BnkfPutM ("HXD:SP_PIN:HST_COUNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned int nulval=1;
    unsigned int hxd_sp_pin_hst_pseud;
    fits_read_col_uint(fp, colnum[HXD_SP_PIN_HST_PSEUD], irow, firstelem,
                      nelements, nulval, &hxd_sp_pin_hst_pseud, &anynul, &istat);
    data[0] = hxd_sp_pin_hst_pseud;
    BnkfPutM ("HXD:SP_PIN:HST_PSEUD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_sp_pin_unitid;
    fits_read_col_byt(fp, colnum[HXD_SP_PIN_UNITID], irow, firstelem,
                      nelements, nulval, &hxd_sp_pin_unitid, &anynul, &istat);
    data[0] = hxd_sp_pin_unitid;
    BnkfPutM ("HXD:SP_PIN:UNIT", sizeof(int)*1, data);
  }
  {
    int i;
    unsigned int data[256];
    unsigned short nulval=1;
    unsigned short hxd_sp_pin_histogram[256];
    fits_read_col_usht(fp, colnum[HXD_SP_PIN_HISTOGRAM], irow, firstelem,
                      256, nulval, hxd_sp_pin_histogram, &anynul, &istat);
    for(i=0;i<256;i++){
      data[i] = hxd_sp_pin_histogram[i];
    }
    BnkfPutM ("HXD:SP_PIN:HISTOGRAM", sizeof(int)*256, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
