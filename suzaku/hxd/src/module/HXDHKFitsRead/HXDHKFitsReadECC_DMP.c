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
  HXD_ECC_MEM_AD,
  HXD_ECC_MEM_DAT,
  HXD_ECC_CHK_BIT,
  HXD_ECC_SYN_BIT,
};

static char pname[] = "HXDHKFitsReadECC_DMP";

static int colnum[4];
static int time_colnum;

void
HXDHKFitsReadECC_DMP_init()
{
  BnkDef( "HXD:ECC_DMP:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:ECC_DMP:DADRS", sizeof(int) );
  BnkDef( "HXD:ECC_DMP:MEMDT", sizeof(int) );
  BnkDef( "HXD:ECC_DMP:CHKBIT", sizeof(int) );
  BnkDef( "HXD:ECC_DMP:SYDBIT", sizeof(int) );
  
}


int
HXDHKFitsReadECC_DMP_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, ECC_DMP, &hdutype, &istat );
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
  
  if( fits_get_colnum(fp, casesen, "HXD_ECC_MEM_AD",
                      &colnum[HXD_ECC_MEM_AD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ECC_MEM_AD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ECC_MEM_DAT",
                      &colnum[HXD_ECC_MEM_DAT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ECC_MEM_DAT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ECC_CHK_BIT",
                      &colnum[HXD_ECC_CHK_BIT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ECC_CHK_BIT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ECC_SYN_BIT",
                      &colnum[HXD_ECC_SYN_BIT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ECC_SYN_BIT') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadECC_DMP_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, ECC_DMP, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, ECC_DMP, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:ECC_DMP:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_ecc_mem_ad;
    fits_read_col_int(fp, colnum[HXD_ECC_MEM_AD], irow, firstelem,
                      nelements, nulval, &hxd_ecc_mem_ad, &anynul, &istat);
    data[0] = hxd_ecc_mem_ad;
    BnkfPutM ("HXD:ECC_DMP:DADRS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned int nulval=1;
    unsigned int hxd_ecc_mem_dat;
    fits_read_col_uint(fp, colnum[HXD_ECC_MEM_DAT], irow, firstelem,
                      nelements, nulval, &hxd_ecc_mem_dat, &anynul, &istat);
    data[0] = hxd_ecc_mem_dat;
    BnkfPutM ("HXD:ECC_DMP:MEMDT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_ecc_chk_bit;
    fits_read_col_byt(fp, colnum[HXD_ECC_CHK_BIT], irow, firstelem,
                      nelements, nulval, &hxd_ecc_chk_bit, &anynul, &istat);
    data[0] = hxd_ecc_chk_bit;
    BnkfPutM ("HXD:ECC_DMP:CHKBIT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_ecc_syn_bit;
    fits_read_col_byt(fp, colnum[HXD_ECC_SYN_BIT], irow, firstelem,
                      nelements, nulval, &hxd_ecc_syn_bit, &anynul, &istat);
    data[0] = hxd_ecc_syn_bit;
    BnkfPutM ("HXD:ECC_DMP:SYDBIT", sizeof(int)*1, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
