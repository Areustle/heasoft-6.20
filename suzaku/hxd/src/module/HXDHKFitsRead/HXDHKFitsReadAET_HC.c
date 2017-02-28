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
  HXD_AET_HC_TABLE_TYPE,
  HXD_AET_HC_TABLE_NO,
  HXD_AET_HC_HV_W_SET0,
  HXD_AET_HC_HV_W_SET1,
  HXD_AET_HC_HV_W_SET2,
  HXD_AET_HC_HV_W_SET3,
  HXD_AET_HC_HV_T_SET0,
  HXD_AET_HC_HV_T_SET1,
  HXD_AET_HC_HV_T_SET2,
  HXD_AET_HC_HV_T_SET3,
  HXD_AET_HC_HV_P_SET0,
  HXD_AET_HC_HV_P_SET1,
  HXD_AET_HC_HV_P_SET2,
  HXD_AET_HC_HV_P_SET3,
  HXD_AET_HC_TIMER_RESET,
  HXD_AET_HC_TIMER_CLOCK,
  HXD_AET_HC_PEVENT_RATE,
  HXD_AET_HC_RBM_ENA,
};

static char pname[] = "HXDHKFitsReadAET_HC";

static int colnum[18];
static int time_colnum;

void
HXDHKFitsReadAET_HC_init()
{
  BnkDef( "HXD:AET_HC:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:AET:HC:TABLE_TYPE", sizeof(int) );
  BnkDef( "HXD:AET:HC:TABLE_NO", sizeof(int) );
  BnkDef( "HXD:AET:HC:HV_W_SET", sizeof(int)*4 );
  BnkDef( "HXD:AET:HC:HV_T_SET", sizeof(int)*4 );
  BnkDef( "HXD:AET:HC:HV_P_SET", sizeof(int)*4 );
  BnkDef( "HXD:AET:HC:TIMER_RESET", sizeof(int) );
  BnkDef( "HXD:AET:HC:TIMER_CLOCK", sizeof(int) );
  BnkDef( "HXD:AET:HC:PEVENT_RATE", sizeof(int) );
  BnkDef( "HXD:AET:HC:RBM_ENA", sizeof(int) );
  
}


int
HXDHKFitsReadAET_HC_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, AET_HC, &hdutype, &istat );
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
  
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_TABLE_TYPE",
                      &colnum[HXD_AET_HC_TABLE_TYPE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_TABLE_TYPE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_TABLE_NO",
                      &colnum[HXD_AET_HC_TABLE_NO], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_TABLE_NO') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_HV_W_SET0",
                      &colnum[HXD_AET_HC_HV_W_SET0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_HV_W_SET0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_HV_W_SET1",
                      &colnum[HXD_AET_HC_HV_W_SET1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_HV_W_SET1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_HV_W_SET2",
                      &colnum[HXD_AET_HC_HV_W_SET2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_HV_W_SET2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_HV_W_SET3",
                      &colnum[HXD_AET_HC_HV_W_SET3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_HV_W_SET3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_HV_T_SET0",
                      &colnum[HXD_AET_HC_HV_T_SET0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_HV_T_SET0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_HV_T_SET1",
                      &colnum[HXD_AET_HC_HV_T_SET1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_HV_T_SET1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_HV_T_SET2",
                      &colnum[HXD_AET_HC_HV_T_SET2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_HV_T_SET2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_HV_T_SET3",
                      &colnum[HXD_AET_HC_HV_T_SET3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_HV_T_SET3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_HV_P_SET0",
                      &colnum[HXD_AET_HC_HV_P_SET0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_HV_P_SET0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_HV_P_SET1",
                      &colnum[HXD_AET_HC_HV_P_SET1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_HV_P_SET1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_HV_P_SET2",
                      &colnum[HXD_AET_HC_HV_P_SET2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_HV_P_SET2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_HV_P_SET3",
                      &colnum[HXD_AET_HC_HV_P_SET3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_HV_P_SET3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_TIMER_RESET",
                      &colnum[HXD_AET_HC_TIMER_RESET], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_TIMER_RESET') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_TIMER_CLOCK",
                      &colnum[HXD_AET_HC_TIMER_CLOCK], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_TIMER_CLOCK') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_PEVENT_RATE",
                      &colnum[HXD_AET_HC_PEVENT_RATE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_PEVENT_RATE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AET_HC_RBM_ENA",
                      &colnum[HXD_AET_HC_RBM_ENA], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AET_HC_RBM_ENA') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadAET_HC_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, AET_HC, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, AET_HC, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:AET_HC:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aet_hc_table_type;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_TABLE_TYPE], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_table_type, &anynul, &istat);
    data[0] = hxd_aet_hc_table_type;
    BnkfPutM ("HXD:AET:HC:TABLE_TYPE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aet_hc_table_no;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_TABLE_NO], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_table_no, &anynul, &istat);
    data[0] = hxd_aet_hc_table_no;
    BnkfPutM ("HXD:AET:HC:TABLE_NO", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_hc_hv_w_set0;
    unsigned char hxd_aet_hc_hv_w_set1;
    unsigned char hxd_aet_hc_hv_w_set2;
    unsigned char hxd_aet_hc_hv_w_set3;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_HV_W_SET0], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_hv_w_set0, &anynul, &istat);
    data[0] = hxd_aet_hc_hv_w_set0;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_HV_W_SET1], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_hv_w_set1, &anynul, &istat);
    data[1] = hxd_aet_hc_hv_w_set1;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_HV_W_SET2], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_hv_w_set2, &anynul, &istat);
    data[2] = hxd_aet_hc_hv_w_set2;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_HV_W_SET3], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_hv_w_set3, &anynul, &istat);
    data[3] = hxd_aet_hc_hv_w_set3;
    BnkfPutM ("HXD:AET:HC:HV_W_SET", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_hc_hv_t_set0;
    unsigned char hxd_aet_hc_hv_t_set1;
    unsigned char hxd_aet_hc_hv_t_set2;
    unsigned char hxd_aet_hc_hv_t_set3;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_HV_T_SET0], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_hv_t_set0, &anynul, &istat);
    data[0] = hxd_aet_hc_hv_t_set0;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_HV_T_SET1], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_hv_t_set1, &anynul, &istat);
    data[1] = hxd_aet_hc_hv_t_set1;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_HV_T_SET2], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_hv_t_set2, &anynul, &istat);
    data[2] = hxd_aet_hc_hv_t_set2;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_HV_T_SET3], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_hv_t_set3, &anynul, &istat);
    data[3] = hxd_aet_hc_hv_t_set3;
    BnkfPutM ("HXD:AET:HC:HV_T_SET", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_aet_hc_hv_p_set0;
    unsigned char hxd_aet_hc_hv_p_set1;
    unsigned char hxd_aet_hc_hv_p_set2;
    unsigned char hxd_aet_hc_hv_p_set3;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_HV_P_SET0], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_hv_p_set0, &anynul, &istat);
    data[0] = hxd_aet_hc_hv_p_set0;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_HV_P_SET1], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_hv_p_set1, &anynul, &istat);
    data[1] = hxd_aet_hc_hv_p_set1;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_HV_P_SET2], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_hv_p_set2, &anynul, &istat);
    data[2] = hxd_aet_hc_hv_p_set2;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_HV_P_SET3], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_hv_p_set3, &anynul, &istat);
    data[3] = hxd_aet_hc_hv_p_set3;
    BnkfPutM ("HXD:AET:HC:HV_P_SET", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aet_hc_timer_reset;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_TIMER_RESET], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_timer_reset, &anynul, &istat);
    data[0] = hxd_aet_hc_timer_reset;
    BnkfPutM ("HXD:AET:HC:TIMER_RESET", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aet_hc_timer_clock;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_TIMER_CLOCK], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_timer_clock, &anynul, &istat);
    data[0] = hxd_aet_hc_timer_clock;
    BnkfPutM ("HXD:AET:HC:TIMER_CLOCK", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aet_hc_pevent_rate;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_PEVENT_RATE], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_pevent_rate, &anynul, &istat);
    data[0] = hxd_aet_hc_pevent_rate;
    BnkfPutM ("HXD:AET:HC:PEVENT_RATE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aet_hc_rbm_ena;
    fits_read_col_byt(fp, colnum[HXD_AET_HC_RBM_ENA], irow, firstelem,
                      nelements, nulval, &hxd_aet_hc_rbm_ena, &anynul, &istat);
    data[0] = hxd_aet_hc_rbm_ena;
    BnkfPutM ("HXD:AET:HC:RBM_ENA", sizeof(int)*1, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
