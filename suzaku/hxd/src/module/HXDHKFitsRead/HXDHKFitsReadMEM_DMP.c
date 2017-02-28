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
  HXD_MEM_DUMP_ADRS,
  HXD_MEM_DMP_LNG,
  HXD_MEM_DMP_DAT_N,
};

static char pname[] = "HXDHKFitsReadMEM_DMP";

static int colnum[3];
static int time_colnum;

void
HXDHKFitsReadMEM_DMP_init()
{
  BnkDef( "HXD:MEM_DMP:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:MEM_DMP:STARTAD", sizeof(int) );
  BnkDef( "HXD:MEM_DMP:DTLEN", sizeof(int) );
  BnkDef( "HXD:MEM_DMP:DMP_DATA", sizeof(int)*1536 );
  
}


int
HXDHKFitsReadMEM_DMP_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, MEM_DMP, &hdutype, &istat );
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
  
  if( fits_get_colnum(fp, casesen, "HXD_MEM_DUMP_ADRS",
                      &colnum[HXD_MEM_DUMP_ADRS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MEM_DUMP_ADRS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MEM_DMP_LNG",
                      &colnum[HXD_MEM_DMP_LNG], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MEM_DMP_LNG') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MEM_DMP_DAT_N",
                      &colnum[HXD_MEM_DMP_DAT_N], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MEM_DMP_DAT_N') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadMEM_DMP_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, MEM_DMP, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, MEM_DMP, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:MEM_DMP:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_mem_dump_adrs;
    fits_read_col_int(fp, colnum[HXD_MEM_DUMP_ADRS], irow, firstelem,
                      nelements, nulval, &hxd_mem_dump_adrs, &anynul, &istat);
    data[0] = hxd_mem_dump_adrs;
    BnkfPutM ("HXD:MEM_DMP:STARTAD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_mem_dmp_lng;
    fits_read_col_usht(fp, colnum[HXD_MEM_DMP_LNG], irow, firstelem,
                      nelements, nulval, &hxd_mem_dmp_lng, &anynul, &istat);
    data[0] = hxd_mem_dmp_lng;
    BnkfPutM ("HXD:MEM_DMP:DTLEN", sizeof(int)*1, data);
  }
  {
    int i;
    unsigned int data[1536];
    unsigned char nulval=1;
    unsigned char hxd_mem_dmp_dat_n[1536];
    fits_read_col_byt(fp, colnum[HXD_MEM_DMP_DAT_N], irow, firstelem,
                      1536, nulval, hxd_mem_dmp_dat_n, &anynul, &istat);
    for(i=0;i<1536;i++){
      data[i] = hxd_mem_dmp_dat_n[i];
    }
    BnkfPutM ("HXD:MEM_DMP:DMP_DATA", sizeof(int)*1536, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
