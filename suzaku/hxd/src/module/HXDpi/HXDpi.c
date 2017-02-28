/*
 *  HXDpi
 *       version 0.1.0          99/8/6    C.Tanihata
 *         for AE linearity correction only,
 *       version 0.2.0
 *         HXDpiRPT  HXDpiFITS
 *         use function hxdpiUtil
 *       version 0.2.2
 *         for fits.tble format
 *         hxdpiUtil v0.0.3
 *       version 0.3.0
 *         for hxdpiUtil v0.1.0
 *       version 0.3.1
 *         for hxdpiUtil v0.1.1
 *       version 0.3.2
 *         for HXDtableFitsInit
 *       version 0.3.3
 *         for hxdpiUtil v0.1.3
 *       version 0.3.4
 *
 *       version 0.3.5 (M.Sugiho)
 *         version down form 0.3.4 : not use aste_get_hk
 *       version 0.3.6 (Y.Terada)
 *         change #include
 *       version 0.3.7 (Y.Terada) 1999-12-24
 *         change param name for the third release of hxd-ftools.
 *	 version 0.5.0 (T.Kishishita,S.hirakuri) 2005-01-18
 *	 version 0.5.1 (T.Kishishita,S.hirakuri, Y.Terada) 2005-01-25
 *              read HK temperature,
 *              read ASCII file, hxdpiUtil v0.2.1
 *              put PI information.
 *       version 0.5.2 (T.Kishishita, Y.Terada) 2005-02-02
 *              support FITS I/O
 *       version 0.5.4 (S.Hirakuri) 2005-05-23
 *              PI(GSO) int -> double 
 *       version 0.5.5 (Y.Terada) 2005-06-03
 *              PI(PIN) int -> double 
 *       version 0.5.6 (T.Kishishita) 2005-06-03
 *              match with hxdpiUtil 0.2.9
 *       version 0.5.7 (Y.Terada) 2005-06-09
 *              new aste_gethk version 2.3
 *       version 0.5.8 (S.Hirakuri) 2005-06-14
 *              WEL:TIME = 0.0
 *       version 0.5.9 (S.Hirakuri) 2005-10-21
 *              read TSTART
 *       version 0.6.0 (M.K) 2005-10-27
 *              gso/pin_init() moved from HXDpi_init() to HXDpi_bgnrun()
 *              ( consistency with "TSTART" access in HXDeventFitsRead )
 *       version 0.6.1 (Y.Terada) 2005-11-03
 *              put PIL parameters in Bnk
 *       version 0.6.2 (Y.Terada) 2005-11-05
 *              put PIL parameters in Bnk, debug
 *       version 0.6.3 (Y.Terada) 2005-11-08
 *              shorten Bnk name
 *       version 0.6.4 (H.Takahashi) 2006-02-03
 *              Skip BnkGet before BnkPut
 *       version 0.6.5 (H.Takahashi) 2006-02-04
 *              Invalid value 0.0 -> define
 *       version 0.7.0 (M.K) 2006-05-26
 *              change hxdpiUtil_gso_init()
 *       version 2.0.0 (Y.Terada)    2006-09-10
 *              new format for version 2.0.x.x process
 *       version 2.0.1 (Y.Terada)    2007-04-27
 *              CALDB auto access function
 *       version 2.1.0 (M.Kokubun)   2007-05-08
 *              GHF -> GHT replacement
 *       version 2.1.1 (Y.Terada)    2007-05-13
 *              debug
 *       version 2.1.2 (M.Kokubun)   2007-05-20
 *              HXDpiFITS.c debug
 *       version 2.1.3 (M.Kokubun)   2007-06-12
 *              HXDpiFITS.c debug
 *       version 2.2.0 (S.Yamada)   2009-07-17
 *              inc. orbit
 *       version 2.3.1 (Y.Terada)    2009-10-19
 *              delete // comment mark for ftools release.
 *       version 2.4.0 (Y.Terada)    2009-10-20
 *              GSOGHT --> GSOGHP (new caldb file).
 *       version 2.4.1 (Y.Terada)    2009-10-21
 *              GSOGHP --> GSOGPT.
 *       version 2.4.2 (Y.Terada)    2010-01-05
 *              GSOGPT_F and GSOGPT_V.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "atFunctions.h"
#include "fitsio.h"
#include "cfortran.h"
#include "hbook.h"
#include "hxd/HXD.h"
#include "hxdpiUtil.h"
#include "hxdeventFitsToBnkUtil.h"
#include "hxdFitsHeaderUtil.h"


/* #include "atFunctions.h" */
/* #include "aste_orbit.h" */


#define FILELIST_MAX_LENGTH 256


#define HXD_PIN_PER_WEL_NUM 4
#define DEBUG 0

#define HXD_TIME_INVALID 0.0
#define HXD_PI_INVALID 0
#define HXD_UPI_INVALID 0.0

char HXDpi_version[] = "version 2.4.2";
static char pname[] = "HXDpi";

void
HXDpi_startup(int *status){ 
  int used = 1;
  BnkPut( "HXD:ftools:hxdpi_yn", sizeof(int), &used);

  *status = ANL_OK; 
}

void
HXDpi_com(int *status) {
        
    if ( *status ) { /* ftools */
        
        *status = 0;
        
        if ( *status ) {
            *status = ANL_QUIT;
            exit(-1);
        }
        
        *status = ANL_OK;
        return;
    }
    
    *status = ANL_OK;
}

void
HXDpi_init(int *status) {

    *status = ANL_OK;

}

void	
HXDpi_his(int *status){ *status = ANL_OK; }

void
HXDpi_bgnrun(int *status){

  int size;
  int istat;
  double tstart, tstop;

  char hxd_gsogpt_fname[FILELIST_MAX_LENGTH];
  char hxd_gsolin_fname[FILELIST_MAX_LENGTH];
  char hxd_pinghf_fname[FILELIST_MAX_LENGTH];
  char hxd_pinlin_fname[FILELIST_MAX_LENGTH];
  int  hxd_gsogpt_version = HXDGSOGPT_VERSION_UNDEFINED;

  char orbit[FILELIST_MAX_LENGTH];/* add SY */

  BnkfGetM("HXDpi:GSO_GPT_NAME", sizeof(hxd_gsogpt_fname),
	   &size, hxd_gsogpt_fname);
  BnkfGetM("HXDpi:GSO_LIN_NAME", sizeof(hxd_gsolin_fname),
	   &size, hxd_gsolin_fname);
  BnkfGetM("HXDpi:PIN_GHF_NAME", sizeof(hxd_pinghf_fname),
	   &size, hxd_pinghf_fname);
  BnkfGetM("HXDpi:PIN_LIN_NAME", sizeof(hxd_pinlin_fname),
	   &size, hxd_pinlin_fname);

  BnkfGetM("HXDpi:ORB_NAME", sizeof(orbit), &size, orbit);

  /*** Note: TSTART/TSTOP is BnkPut-ed in HXDeventFitsRead_bgnrun() ***/
  BnkfGetM("HXDeventFitsRead:TSTART", sizeof(double), &size, &tstart);
  BnkfGetM("HXDeventFitsRead:TSTOP",  sizeof(double), &size, &tstop);
  /* printf(" HXDpi_init: time duration = %lf -- %lf\n", tstart, tstop ); */

  hxdpiUtil_gso_init(hxd_gsogpt_fname, hxd_gsolin_fname, 
		     tstart, tstop,  &istat);
  if(istat != HXDPIUTIL_OK){
    fprintf(stderr, "%s: GSO Init Error\n", pname);
    *status = ANL_QUIT;
    return;
  }

  hxdpiUtil_gso_read_gsogpt_version( &hxd_gsogpt_version, &istat);
  if(istat != HXDPIUTIL_OK){
    fprintf(stderr, "%s: GSO Init Error (read VERSION of GSOGPT)\n", 
	    pname);
    *status = ANL_QUIT;
    return;
  }
  BnkDef  ("HXDpi:GSOGPT_VERSION", sizeof(int));
  BnkfPutM("HXDpi:GSOGPT_VERSION", sizeof(int), &hxd_gsogpt_version);

  hxdpiUtil_pin_init(hxd_pinghf_fname, hxd_pinlin_fname, &istat);
  if(istat != HXDPIUTIL_OK){
    fprintf(stderr, "%s: PIN Init Error\n", pname);
    *status = ANL_QUIT;
    return;
  }

  hxdpiUtil_orb_init(orbit, &istat);
    
  *status = ANL_OK;
    
}

void
HXDpi_ana(int nevent, int eventid, int *status){
  int size;
  int pi_slow, pi_fast;
  int pi_pin0, pi_pin1, pi_pin2, pi_pin3;
  int pi_pin[4];
  double upi_slow, upi_fast;
  double upi_pin0, upi_pin1, upi_pin2, upi_pin3;
  double upi_pin[4];
  HxdEventFits02 event_data;
  hxdpiUtil_HK  hk_data;
  hxdpiUtil_EHK ehk_data;
  int temp;
  
  int saapass;

  BnkfGetM("HXD:WEL:EVENT", sizeof(HxdEventFits02), &size,&event_data);



  if(event_data.time == HXD_TIME_INVALID){
    pi_fast = HXD_PI_INVALID; /* INVARID CH DATA */
    pi_slow = HXD_PI_INVALID; /* INVARID CH DATA */
    pi_pin0 = HXD_PI_INVALID;  /* INVARID CH DATA */
    pi_pin1 = HXD_PI_INVALID;  /* INVARID CH DATA */
    pi_pin2 = HXD_PI_INVALID;  /* INVARID CH DATA */
    pi_pin3 = HXD_PI_INVALID;  /* INVARID CH DATA */
    upi_fast = HXD_UPI_INVALID; /* INVARID CH DATA */
    upi_slow = HXD_UPI_INVALID; /* INVARID CH DATA */
    upi_pin0 = HXD_UPI_INVALID;  /* INVARID CH DATA */
    upi_pin1 = HXD_UPI_INVALID;  /* INVARID CH DATA */
    upi_pin2 = HXD_UPI_INVALID;  /* INVARID CH DATA */
    upi_pin3 = HXD_UPI_INVALID;  /* INVARID CH DATA */
  }else{
      BnkfGetM("HXDpi:HKDATA",  sizeof(hxdpiUtil_HK),  &size, &hk_data);
      BnkfGetM("HXDpi:EHKDATA", sizeof(hxdpiUtil_EHK), &size, &ehk_data);
      saapass=hxdpiUtil_orb_calcsaa(&event_data);
      /* printf("aa-hajime saapass %d \n",saapass); */
      hxdpiUtil_pin_correct(&event_data,
			    &upi_pin0,&upi_pin1,&upi_pin2,&upi_pin3,
			    &pi_pin0,&pi_pin1,&pi_pin2,&pi_pin3);

      hxdpiUtil_gso_correct(&event_data, &hk_data, &ehk_data,
			    &upi_fast,&upi_slow, &pi_fast,&pi_slow,&saapass);



  }

  hxdpiUtil_modify_eventdata(upi_fast, upi_slow, upi_pin0, upi_pin1,
			     upi_pin2, upi_pin3, 
			     pi_fast, pi_slow, pi_pin0, pi_pin1,
			     pi_pin2, pi_pin3, &event_data);

  upi_pin[0] = upi_pin0;
  upi_pin[1] = upi_pin1;
  upi_pin[2] = upi_pin2;
  upi_pin[3] = upi_pin3;
  pi_pin[0] = pi_pin0;
  pi_pin[1] = pi_pin1;
  pi_pin[2] = pi_pin2;
  pi_pin[3] = pi_pin3;

  BnkfPutM("HXD:WEL:EVENT", sizeof(HxdEventFits02),&event_data);
  BnkfPutM("HXD:WEL:UPI_FAST", sizeof(double),  &upi_fast);
  BnkfPutM("HXD:WEL:UPI_SLOW", sizeof(double),  &upi_slow);
  BnkfPutM("HXD:WEL:UPI_PIN", sizeof(double)*4, upi_pin);
  BnkfPutM("HXD:WEL:PI_FAST", sizeof(int),  &pi_fast);
  BnkfPutM("HXD:WEL:PI_SLOW", sizeof(int),  &pi_slow);
  BnkfPutM("HXD:WEL:PI_PIN", sizeof(int)*4, pi_pin);
    
  *status = ANL_OK;
    
}
	
void	
HXDpi_endrun(int *status){ *status = ANL_OK; }

void
HXDpi_exit(int *status){ *status = ANL_OK; }

