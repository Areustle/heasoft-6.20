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
  HXD_SFF2_IBLOCK,
  HXD_SFF2_BUFF_ID,
  HXD_HIS_SFF1_UNIT,
  HXD_HIS_SFF2_UNIT,
  HXD_SFF2_BLOCK,
  HXD_HIS_PI_SEQNO,
  HXD_HIS_DISCARD,
  HXD_SFF2_HIS_MODE,
  HXD_SFF2_PRESET,
  HXD_SFF2_HST_COUNT,
  HXD_SFF2_HST_PSEUD,
  HXD_SFF2_UNITID,
  HXD_SFF2_HISTOGRAM,
};

static char pname[] = "HXDHKFitsReadSFF2";

static int colnum[14];
static int time_colnum;

void
HXDHKFitsReadSFF2_init()
{
  BnkDef( "HXD:SFF2:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:SFF2:TYPE", sizeof(int) );
  BnkDef( "HXD:SFF2:IBLOCK", sizeof(int) );
  BnkDef( "HXD:SFF2:BUFF_ID", sizeof(int) );
  BnkDef( "HXD:SFF2:SFF1_UNIT", sizeof(int) );
  BnkDef( "HXD:SFF2:SFF2_UNIT", sizeof(int) );
  BnkDef( "HXD:SFF2:BLOCK", sizeof(int) );
  BnkDef( "HXD:SFF2:PI_SEQNO", sizeof(int) );
  BnkDef( "HXD:SFF2:DISCARD", sizeof(int) );
  BnkDef( "HXD:SFF2:HIS_MODE", sizeof(int) );
  BnkDef( "HXD:SFF2:PRESET", sizeof(int) );
  BnkDef( "HXD:SFF2:HST_COUNT", sizeof(int) );
  BnkDef( "HXD:SFF2:HST_PSEUD", sizeof(int) );
  BnkDef( "HXD:SFF2:SFF2_UNIT", sizeof(int) );
  BnkDef( "HXD:SFF2:HISTOGRAM", sizeof(int)*256 );
  
}


int
HXDHKFitsReadSFF2_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, SFF2, &hdutype, &istat );
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
  if( fits_get_colnum(fp, casesen, "HXD_SFF2_IBLOCK",
                      &colnum[HXD_SFF2_IBLOCK], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SFF2_IBLOCK') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SFF2_BUFF_ID",
                      &colnum[HXD_SFF2_BUFF_ID], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SFF2_BUFF_ID') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HIS_SFF1_UNIT",
                      &colnum[HXD_HIS_SFF1_UNIT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_SFF1_UNIT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HIS_SFF2_UNIT",
                      &colnum[HXD_HIS_SFF2_UNIT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_SFF2_UNIT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SFF2_BLOCK",
                      &colnum[HXD_SFF2_BLOCK], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SFF2_BLOCK') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HIS_PI_SEQNO",
                      &colnum[HXD_HIS_PI_SEQNO], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_PI_SEQNO') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HIS_DISCARD",
                      &colnum[HXD_HIS_DISCARD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HIS_DISCARD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SFF2_HIS_MODE",
                      &colnum[HXD_SFF2_HIS_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SFF2_HIS_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SFF2_PRESET",
                      &colnum[HXD_SFF2_PRESET], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SFF2_PRESET') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SFF2_HST_COUNT",
                      &colnum[HXD_SFF2_HST_COUNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SFF2_HST_COUNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SFF2_HST_PSEUD",
                      &colnum[HXD_SFF2_HST_PSEUD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SFF2_HST_PSEUD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SFF2_UNITID",
                      &colnum[HXD_SFF2_UNITID], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SFF2_UNITID') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SFF2_HISTOGRAM",
                      &colnum[HXD_SFF2_HISTOGRAM], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SFF2_HISTOGRAM') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadSFF2_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, SFF2, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, SFF2, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:SFF2:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_his_type;
    fits_read_col_byt(fp, colnum[HXD_HIS_TYPE], irow, firstelem,
                      nelements, nulval, &hxd_his_type, &anynul, &istat);
    data[0] = hxd_his_type;
    BnkfPutM ("HXD:SFF2:TYPE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    short nulval=1;
    short hxd_sff2_iblock;
    fits_read_col_sht(fp, colnum[HXD_SFF2_IBLOCK], irow, firstelem,
                      nelements, nulval, &hxd_sff2_iblock, &anynul, &istat);
    data[0] = hxd_sff2_iblock;
    BnkfPutM ("HXD:SFF2:IBLOCK", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_sff2_buff_id;
    fits_read_col_byt(fp, colnum[HXD_SFF2_BUFF_ID], irow, firstelem,
                      nelements, nulval, &hxd_sff2_buff_id, &anynul, &istat);
    data[0] = hxd_sff2_buff_id;
    BnkfPutM ("HXD:SFF2:BUFF_ID", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_his_sff1_unit;
    fits_read_col_byt(fp, colnum[HXD_HIS_SFF1_UNIT], irow, firstelem,
                      nelements, nulval, &hxd_his_sff1_unit, &anynul, &istat);
    data[0] = hxd_his_sff1_unit;
    BnkfPutM ("HXD:SFF2:SFF1_UNIT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_his_sff2_unit;
    fits_read_col_byt(fp, colnum[HXD_HIS_SFF2_UNIT], irow, firstelem,
                      nelements, nulval, &hxd_his_sff2_unit, &anynul, &istat);
    data[0] = hxd_his_sff2_unit;
    BnkfPutM ("HXD:SFF2:SFF2_UNIT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_sff2_block;
    fits_read_col_byt(fp, colnum[HXD_SFF2_BLOCK], irow, firstelem,
                      nelements, nulval, &hxd_sff2_block, &anynul, &istat);
    data[0] = hxd_sff2_block;
    BnkfPutM ("HXD:SFF2:BLOCK", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_his_pi_seqno;
    fits_read_col_usht(fp, colnum[HXD_HIS_PI_SEQNO], irow, firstelem,
                      nelements, nulval, &hxd_his_pi_seqno, &anynul, &istat);
    data[0] = hxd_his_pi_seqno;
    BnkfPutM ("HXD:SFF2:PI_SEQNO", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_his_discard;
    fits_read_col_usht(fp, colnum[HXD_HIS_DISCARD], irow, firstelem,
                      nelements, nulval, &hxd_his_discard, &anynul, &istat);
    data[0] = hxd_his_discard;
    BnkfPutM ("HXD:SFF2:DISCARD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_sff2_his_mode;
    fits_read_col_byt(fp, colnum[HXD_SFF2_HIS_MODE], irow, firstelem,
                      nelements, nulval, &hxd_sff2_his_mode, &anynul, &istat);
    data[0] = hxd_sff2_his_mode;
    BnkfPutM ("HXD:SFF2:HIS_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_sff2_preset;
    fits_read_col_usht(fp, colnum[HXD_SFF2_PRESET], irow, firstelem,
                      nelements, nulval, &hxd_sff2_preset, &anynul, &istat);
    data[0] = hxd_sff2_preset;
    BnkfPutM ("HXD:SFF2:PRESET", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned int nulval=1;
    unsigned int hxd_sff2_hst_count;
    fits_read_col_uint(fp, colnum[HXD_SFF2_HST_COUNT], irow, firstelem,
                      nelements, nulval, &hxd_sff2_hst_count, &anynul, &istat);
    data[0] = hxd_sff2_hst_count;
    BnkfPutM ("HXD:SFF2:HST_COUNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned int nulval=1;
    unsigned int hxd_sff2_hst_pseud;
    fits_read_col_uint(fp, colnum[HXD_SFF2_HST_PSEUD], irow, firstelem,
                      nelements, nulval, &hxd_sff2_hst_pseud, &anynul, &istat);
    data[0] = hxd_sff2_hst_pseud;
    BnkfPutM ("HXD:SFF2:HST_PSEUD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_sff2_unitid;
    fits_read_col_byt(fp, colnum[HXD_SFF2_UNITID], irow, firstelem,
                      nelements, nulval, &hxd_sff2_unitid, &anynul, &istat);
    data[0] = hxd_sff2_unitid;
    BnkfPutM ("HXD:SFF2:SFF2_UNIT", sizeof(int)*1, data);
  }
  {
    int i;
    unsigned int data[256];
    unsigned short nulval=1;
    unsigned short hxd_sff2_histogram[256];
    fits_read_col_usht(fp, colnum[HXD_SFF2_HISTOGRAM], irow, firstelem,
                      256, nulval, hxd_sff2_histogram, &anynul, &istat);
    for(i=0;i<256;i++){
      data[i] = hxd_sff2_histogram[i];
    }
    BnkfPutM ("HXD:SFF2:HISTOGRAM", sizeof(int)*256, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
