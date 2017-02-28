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
  HXD_WEL_PWH1,
  HXD_WEL_PWH2,
  HXD_WEL_PWH3,
  HXD_WEL_PWH4,
  HXD_WEL_PWH5,
  HXD_WEL_PWH6,
  HXD_WEL_PWH7,
  HXD_WEL_PWH8,
  HXD_WEL_PWH9,
  HXD_WEL_PWH10,
  HXD_WEL_PWH11,
  HXD_WEL_PWH12,
  HXD_WEL_PWH13,
  HXD_WEL_PWH14,
  HXD_WEL_PWH15,
  HXD_WEL_PWH16,
};

static char pname[] = "HXDHKFitsReadPWH";

static int colnum[16];
static int time_colnum;

void
HXDHKFitsReadPWH_init()
{
  BnkDef( "HXD:PWH:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:WEL:PWH", sizeof(int)*16 );
  BnkDef( "", sizeof(int)*0 );
  
}


int
HXDHKFitsReadPWH_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, PWH, &hdutype, &istat );
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
  
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH1",
                      &colnum[HXD_WEL_PWH1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH2",
                      &colnum[HXD_WEL_PWH2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH3",
                      &colnum[HXD_WEL_PWH3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH4",
                      &colnum[HXD_WEL_PWH4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH5",
                      &colnum[HXD_WEL_PWH5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH6",
                      &colnum[HXD_WEL_PWH6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH7",
                      &colnum[HXD_WEL_PWH7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH8",
                      &colnum[HXD_WEL_PWH8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH9",
                      &colnum[HXD_WEL_PWH9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH10",
                      &colnum[HXD_WEL_PWH10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH11",
                      &colnum[HXD_WEL_PWH11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH12",
                      &colnum[HXD_WEL_PWH12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH13",
                      &colnum[HXD_WEL_PWH13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH14",
                      &colnum[HXD_WEL_PWH14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH15",
                      &colnum[HXD_WEL_PWH15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WEL_PWH16",
                      &colnum[HXD_WEL_PWH16], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WEL_PWH16') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadPWH_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, PWH, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, PWH, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:PWH:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[16];
    int nulval=1;
    int hxd_wel_pwh1;
    int hxd_wel_pwh2;
    int hxd_wel_pwh3;
    int hxd_wel_pwh4;
    int hxd_wel_pwh5;
    int hxd_wel_pwh6;
    int hxd_wel_pwh7;
    int hxd_wel_pwh8;
    int hxd_wel_pwh9;
    int hxd_wel_pwh10;
    int hxd_wel_pwh11;
    int hxd_wel_pwh12;
    int hxd_wel_pwh13;
    int hxd_wel_pwh14;
    int hxd_wel_pwh15;
    int hxd_wel_pwh16;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH1], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh1, &anynul, &istat);
    data[0] = hxd_wel_pwh1;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH2], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh2, &anynul, &istat);
    data[1] = hxd_wel_pwh2;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH3], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh3, &anynul, &istat);
    data[2] = hxd_wel_pwh3;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH4], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh4, &anynul, &istat);
    data[3] = hxd_wel_pwh4;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH5], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh5, &anynul, &istat);
    data[4] = hxd_wel_pwh5;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH6], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh6, &anynul, &istat);
    data[5] = hxd_wel_pwh6;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH7], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh7, &anynul, &istat);
    data[6] = hxd_wel_pwh7;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH8], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh8, &anynul, &istat);
    data[7] = hxd_wel_pwh8;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH9], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh9, &anynul, &istat);
    data[8] = hxd_wel_pwh9;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH10], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh10, &anynul, &istat);
    data[9] = hxd_wel_pwh10;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH11], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh11, &anynul, &istat);
    data[10] = hxd_wel_pwh11;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH12], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh12, &anynul, &istat);
    data[11] = hxd_wel_pwh12;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH13], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh13, &anynul, &istat);
    data[12] = hxd_wel_pwh13;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH14], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh14, &anynul, &istat);
    data[13] = hxd_wel_pwh14;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH15], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh15, &anynul, &istat);
    data[14] = hxd_wel_pwh15;
    fits_read_col_int(fp, colnum[HXD_WEL_PWH16], irow, firstelem,
                      nelements, nulval, &hxd_wel_pwh16, &anynul, &istat);
    data[15] = hxd_wel_pwh16;
    BnkfPutM ("HXD:WEL:PWH", sizeof(int)*16, data);
  }
  {
    unsigned int data[0];
    BnkfPutM ("", sizeof(int)*0, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
