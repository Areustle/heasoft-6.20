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
  HXD_SCL_LNGTH_OK,
  HXD_SCL_PI_WPU_ID,
  HXD_SCL_PI_SEQ_NO,
  HXD_SCL_PMT_LD0,
  HXD_SCL_PMT_LD1,
  HXD_SCL_PMT_LD2,
  HXD_SCL_PMT_LD3,
  HXD_SCL_PIN_LD0,
  HXD_SCL_PIN_LD1,
  HXD_SCL_PIN_LD2,
  HXD_SCL_PIN_LD3,
  HXD_SCL_PMT_UD0,
  HXD_SCL_PMT_UD1,
  HXD_SCL_PMT_UD2,
  HXD_SCL_PMT_UD3,
  HXD_SCL_PIN_UD0,
  HXD_SCL_PIN_UD1,
  HXD_SCL_PIN_UD2,
  HXD_SCL_PIN_UD3,
  HXD_SCL_TIME,
  HXD_SCL_STMP_TIM,
  HXD_SCL_DT0,
  HXD_SCL_DT1,
  HXD_SCL_DT2,
  HXD_SCL_DT3,
  HXD_SCL_STMPSQ_DUMMY,
  HXD_SCL_MODULE_DUMMY,
  HXD_SCL_PUDMD_DUMMY,
};

static char pname[] = "HXDHKFitsReadSCL";

static int colnum[28];
static int time_colnum;

void
HXDHKFitsReadSCL_init()
{
  BnkDef( "HXD:SCL:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:SCL:LNGTH_OK", sizeof(int) );
  BnkDef( "HXD:SCL:PI_WPU_ID", sizeof(int) );
  BnkDef( "HXD:SCL:PI_SEQ_NO", sizeof(int) );
  BnkDef( "HXD:SCL:PMT_LD", sizeof(int)*4 );
  BnkDef( "HXD:SCL:PIN_LD", sizeof(int)*4 );
  BnkDef( "HXD:SCL:PMT_UD", sizeof(int)*4 );
  BnkDef( "HXD:SCL:PIN_UD", sizeof(int)*4 );
  BnkDef( "HXD:SCL:TIME", sizeof(int) );
  BnkDef( "HXD:SCL:STMP_TIM", sizeof(int) );
  BnkDef( "HXD:SCL:DEAD_TIME", sizeof(int)*4 );
  BnkDef( "HXD:SCL:STMP_SEQ", sizeof(int) );
  BnkDef( "HXD:SCL:BOARD", sizeof(int) );
  BnkDef( "HXD:SCL:PUD_MODE", sizeof(int) );
  
}


int
HXDHKFitsReadSCL_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, SCL, &hdutype, &istat );
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
  
  if( fits_get_colnum(fp, casesen, "HXD_SCL_LNGTH_OK",
                      &colnum[HXD_SCL_LNGTH_OK], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_LNGTH_OK') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PI_WPU_ID",
                      &colnum[HXD_SCL_PI_WPU_ID], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PI_WPU_ID') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PI_SEQ_NO",
                      &colnum[HXD_SCL_PI_SEQ_NO], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PI_SEQ_NO') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PMT_LD0",
                      &colnum[HXD_SCL_PMT_LD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PMT_LD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PMT_LD1",
                      &colnum[HXD_SCL_PMT_LD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PMT_LD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PMT_LD2",
                      &colnum[HXD_SCL_PMT_LD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PMT_LD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PMT_LD3",
                      &colnum[HXD_SCL_PMT_LD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PMT_LD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PIN_LD0",
                      &colnum[HXD_SCL_PIN_LD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PIN_LD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PIN_LD1",
                      &colnum[HXD_SCL_PIN_LD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PIN_LD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PIN_LD2",
                      &colnum[HXD_SCL_PIN_LD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PIN_LD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PIN_LD3",
                      &colnum[HXD_SCL_PIN_LD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PIN_LD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PMT_UD0",
                      &colnum[HXD_SCL_PMT_UD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PMT_UD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PMT_UD1",
                      &colnum[HXD_SCL_PMT_UD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PMT_UD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PMT_UD2",
                      &colnum[HXD_SCL_PMT_UD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PMT_UD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PMT_UD3",
                      &colnum[HXD_SCL_PMT_UD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PMT_UD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PIN_UD0",
                      &colnum[HXD_SCL_PIN_UD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PIN_UD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PIN_UD1",
                      &colnum[HXD_SCL_PIN_UD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PIN_UD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PIN_UD2",
                      &colnum[HXD_SCL_PIN_UD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PIN_UD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PIN_UD3",
                      &colnum[HXD_SCL_PIN_UD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PIN_UD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_TIME",
                      &colnum[HXD_SCL_TIME], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_TIME') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_STMP_TIM",
                      &colnum[HXD_SCL_STMP_TIM], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_STMP_TIM') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_DT0",
                      &colnum[HXD_SCL_DT0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_DT0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_DT1",
                      &colnum[HXD_SCL_DT1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_DT1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_DT2",
                      &colnum[HXD_SCL_DT2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_DT2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_DT3",
                      &colnum[HXD_SCL_DT3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_DT3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_STMPSQ_DUMMY",
                      &colnum[HXD_SCL_STMPSQ_DUMMY], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_STMPSQ_DUMMY') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_MODULE_DUMMY",
                      &colnum[HXD_SCL_MODULE_DUMMY], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_MODULE_DUMMY') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SCL_PUDMD_DUMMY",
                      &colnum[HXD_SCL_PUDMD_DUMMY], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SCL_PUDMD_DUMMY') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadSCL_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, SCL, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, SCL, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:SCL:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_scl_lngth_ok;
    fits_read_col_byt(fp, colnum[HXD_SCL_LNGTH_OK], irow, firstelem,
                      nelements, nulval, &hxd_scl_lngth_ok, &anynul, &istat);
    data[0] = hxd_scl_lngth_ok;
    BnkfPutM ("HXD:SCL:LNGTH_OK", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_scl_pi_wpu_id;
    fits_read_col_byt(fp, colnum[HXD_SCL_PI_WPU_ID], irow, firstelem,
                      nelements, nulval, &hxd_scl_pi_wpu_id, &anynul, &istat);
    data[0] = hxd_scl_pi_wpu_id;
    BnkfPutM ("HXD:SCL:PI_WPU_ID", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_scl_pi_seq_no;
    fits_read_col_usht(fp, colnum[HXD_SCL_PI_SEQ_NO], irow, firstelem,
                      nelements, nulval, &hxd_scl_pi_seq_no, &anynul, &istat);
    data[0] = hxd_scl_pi_seq_no;
    BnkfPutM ("HXD:SCL:PI_SEQ_NO", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_scl_pmt_ld0;
    unsigned short hxd_scl_pmt_ld1;
    unsigned short hxd_scl_pmt_ld2;
    unsigned short hxd_scl_pmt_ld3;
    fits_read_col_usht(fp, colnum[HXD_SCL_PMT_LD0], irow, firstelem,
                      nelements, nulval, &hxd_scl_pmt_ld0, &anynul, &istat);
    data[0] = hxd_scl_pmt_ld0;
    fits_read_col_usht(fp, colnum[HXD_SCL_PMT_LD1], irow, firstelem,
                      nelements, nulval, &hxd_scl_pmt_ld1, &anynul, &istat);
    data[1] = hxd_scl_pmt_ld1;
    fits_read_col_usht(fp, colnum[HXD_SCL_PMT_LD2], irow, firstelem,
                      nelements, nulval, &hxd_scl_pmt_ld2, &anynul, &istat);
    data[2] = hxd_scl_pmt_ld2;
    fits_read_col_usht(fp, colnum[HXD_SCL_PMT_LD3], irow, firstelem,
                      nelements, nulval, &hxd_scl_pmt_ld3, &anynul, &istat);
    data[3] = hxd_scl_pmt_ld3;
    BnkfPutM ("HXD:SCL:PMT_LD", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_scl_pin_ld0;
    unsigned short hxd_scl_pin_ld1;
    unsigned short hxd_scl_pin_ld2;
    unsigned short hxd_scl_pin_ld3;
    fits_read_col_usht(fp, colnum[HXD_SCL_PIN_LD0], irow, firstelem,
                      nelements, nulval, &hxd_scl_pin_ld0, &anynul, &istat);
    data[0] = hxd_scl_pin_ld0;
    fits_read_col_usht(fp, colnum[HXD_SCL_PIN_LD1], irow, firstelem,
                      nelements, nulval, &hxd_scl_pin_ld1, &anynul, &istat);
    data[1] = hxd_scl_pin_ld1;
    fits_read_col_usht(fp, colnum[HXD_SCL_PIN_LD2], irow, firstelem,
                      nelements, nulval, &hxd_scl_pin_ld2, &anynul, &istat);
    data[2] = hxd_scl_pin_ld2;
    fits_read_col_usht(fp, colnum[HXD_SCL_PIN_LD3], irow, firstelem,
                      nelements, nulval, &hxd_scl_pin_ld3, &anynul, &istat);
    data[3] = hxd_scl_pin_ld3;
    BnkfPutM ("HXD:SCL:PIN_LD", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_scl_pmt_ud0;
    unsigned short hxd_scl_pmt_ud1;
    unsigned short hxd_scl_pmt_ud2;
    unsigned short hxd_scl_pmt_ud3;
    fits_read_col_usht(fp, colnum[HXD_SCL_PMT_UD0], irow, firstelem,
                      nelements, nulval, &hxd_scl_pmt_ud0, &anynul, &istat);
    data[0] = hxd_scl_pmt_ud0;
    fits_read_col_usht(fp, colnum[HXD_SCL_PMT_UD1], irow, firstelem,
                      nelements, nulval, &hxd_scl_pmt_ud1, &anynul, &istat);
    data[1] = hxd_scl_pmt_ud1;
    fits_read_col_usht(fp, colnum[HXD_SCL_PMT_UD2], irow, firstelem,
                      nelements, nulval, &hxd_scl_pmt_ud2, &anynul, &istat);
    data[2] = hxd_scl_pmt_ud2;
    fits_read_col_usht(fp, colnum[HXD_SCL_PMT_UD3], irow, firstelem,
                      nelements, nulval, &hxd_scl_pmt_ud3, &anynul, &istat);
    data[3] = hxd_scl_pmt_ud3;
    BnkfPutM ("HXD:SCL:PMT_UD", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_scl_pin_ud0;
    unsigned short hxd_scl_pin_ud1;
    unsigned short hxd_scl_pin_ud2;
    unsigned short hxd_scl_pin_ud3;
    fits_read_col_usht(fp, colnum[HXD_SCL_PIN_UD0], irow, firstelem,
                      nelements, nulval, &hxd_scl_pin_ud0, &anynul, &istat);
    data[0] = hxd_scl_pin_ud0;
    fits_read_col_usht(fp, colnum[HXD_SCL_PIN_UD1], irow, firstelem,
                      nelements, nulval, &hxd_scl_pin_ud1, &anynul, &istat);
    data[1] = hxd_scl_pin_ud1;
    fits_read_col_usht(fp, colnum[HXD_SCL_PIN_UD2], irow, firstelem,
                      nelements, nulval, &hxd_scl_pin_ud2, &anynul, &istat);
    data[2] = hxd_scl_pin_ud2;
    fits_read_col_usht(fp, colnum[HXD_SCL_PIN_UD3], irow, firstelem,
                      nelements, nulval, &hxd_scl_pin_ud3, &anynul, &istat);
    data[3] = hxd_scl_pin_ud3;
    BnkfPutM ("HXD:SCL:PIN_UD", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_scl_time;
    fits_read_col_int(fp, colnum[HXD_SCL_TIME], irow, firstelem,
                      nelements, nulval, &hxd_scl_time, &anynul, &istat);
    data[0] = hxd_scl_time;
    BnkfPutM ("HXD:SCL:TIME", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_scl_stmp_tim;
    fits_read_col_int(fp, colnum[HXD_SCL_STMP_TIM], irow, firstelem,
                      nelements, nulval, &hxd_scl_stmp_tim, &anynul, &istat);
    data[0] = hxd_scl_stmp_tim;
    BnkfPutM ("HXD:SCL:STMP_TIM", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_scl_dt0;
    unsigned short hxd_scl_dt1;
    unsigned short hxd_scl_dt2;
    unsigned short hxd_scl_dt3;
    fits_read_col_usht(fp, colnum[HXD_SCL_DT0], irow, firstelem,
                      nelements, nulval, &hxd_scl_dt0, &anynul, &istat);
    data[0] = hxd_scl_dt0;
    fits_read_col_usht(fp, colnum[HXD_SCL_DT1], irow, firstelem,
                      nelements, nulval, &hxd_scl_dt1, &anynul, &istat);
    data[1] = hxd_scl_dt1;
    fits_read_col_usht(fp, colnum[HXD_SCL_DT2], irow, firstelem,
                      nelements, nulval, &hxd_scl_dt2, &anynul, &istat);
    data[2] = hxd_scl_dt2;
    fits_read_col_usht(fp, colnum[HXD_SCL_DT3], irow, firstelem,
                      nelements, nulval, &hxd_scl_dt3, &anynul, &istat);
    data[3] = hxd_scl_dt3;
    BnkfPutM ("HXD:SCL:DEAD_TIME", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_scl_stmpsq_dummy;
    fits_read_col_byt(fp, colnum[HXD_SCL_STMPSQ_DUMMY], irow, firstelem,
                      nelements, nulval, &hxd_scl_stmpsq_dummy, &anynul, &istat);
    data[0] = hxd_scl_stmpsq_dummy;
    BnkfPutM ("HXD:SCL:STMP_SEQ", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_scl_module_dummy;
    fits_read_col_byt(fp, colnum[HXD_SCL_MODULE_DUMMY], irow, firstelem,
                      nelements, nulval, &hxd_scl_module_dummy, &anynul, &istat);
    data[0] = hxd_scl_module_dummy;
    BnkfPutM ("HXD:SCL:BOARD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_scl_pudmd_dummy;
    fits_read_col_byt(fp, colnum[HXD_SCL_PUDMD_DUMMY], irow, firstelem,
                      nelements, nulval, &hxd_scl_pudmd_dummy, &anynul, &istat);
    data[0] = hxd_scl_pudmd_dummy;
    BnkfPutM ("HXD:SCL:PUD_MODE", sizeof(int)*1, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
