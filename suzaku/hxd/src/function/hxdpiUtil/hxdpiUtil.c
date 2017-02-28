/*
   hxdpiUtil v0.0.1 <= HXDpi 0.1.2
             v0.0.3 for HXDpi 0.2.2
             v0.0.4 for HXDpi 0.2.3
	     v0.1.0 for HXDpi 0.3.0
	     v0.1.1 for HXDpi 0.3.1
	     v0.1.2 for hxdtableFitsUtil 0.0.8
	     v0.1.3 add GainUtil for HXDpi 0.3.3
	     v0.1.5 version doen from v0.1.3 : not use aste_get_hk
	            for HXDpi 0.3.5
	     v0.2.0 for HXD-II PI Util (PIN+GSO),
	            by Y.Terada, S.Hirakuri, T.Kisisita
	     v0.2.1 by Y.Terada, S.Hirakuri, T.Kisisita
	            support digitalization, modify_eventdata 
		    read HK temperature
	     v0.2.2; 2005-02-03, support fits io, by T.Kisisita.
	     v0.2.3; 2005-02-05, read gain history by Y.Terada
	     v0.2.4; 2005-02-07, debug, by Y.Terada
	     v0.7.1; 2006-08-04, debug hxdpiUtil_modify_eventdata, 
                              by terada, tanaka, watanabe, kokubun
	     v2.0.0; 2006-09-10, by yterada for v2 format
	     v2.0.1; 2006-09-13, debug by kokubun
	     v2.3.0; 2009-07-19, S.Yamada new ght
             v2.3.2: 2009-10-19, delete // mark by Y.Terada
	     v2.4.3: 2013-10-09, add return value, by Y.Terada
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hxdpiUtil.h"

#include "atFunctions.h"
#include "aste_orbit.h"
#include "aste_time.h"
#include "anl_msg.h"

#include "hxdeventFitsUtil.h"
#include "pil.h"


static ORBIT *orbitp;
static int first_flag;


int hxdpiUtil_check_version( char* hxdpi_module_version){
    return HXDPIUTIL_OK;
}


void hxdpiUtil_orb_init(char* orbitfile, int* istat){
  *istat = aste_orbit_init(&orbitp, orbitfile);
  first_flag=1;
}


                                
int hxdpiUtil_orb_calcsaa(HxdEventFits02* eventdata){


  static int saapass_new;
  static double t_old, t_new,t_old_print;
  static int saa_hxd_new, saa_hxd_old; 
  double shift_t; /* to jump previous saapass */ 
  double mjd_now; /* obtained from event file */
  double t0,dt,dt_print,mjd0; /* variable */ 
  double last_saa_hxd_time,next_saa_hxd_time,dt_saa,tmp_last_saa_hxd_time;
  int saa_hxd0;
  int back_counter;/* to connt saapass for the first event */
  
  AtVect vSat0, vSatG0;
  AtPolarVect pvSatG0;

  int istat; /* int init;*/
  int calc_flag, first_saaremote_flag;

  shift_t=2100; /* 2100 sec (35min) */
  saa_hxd0=-1;
  calc_flag=0;
  first_saaremote_flag=0;
  back_counter=0;

  /* printf("why calc_flag stand (0) %d %d\n",saa_hxd_new,saa_hxd_old); */

  t_new=eventdata->time;
  
  if( t_new < 173491200.)
    {
      anl_msg_error("before 2005-07-01T00:00:00 t=%f \n", t_new);
      return -1;
    }


  if( first_flag == 1) {
    t_old=t_new;
  }


  dt=t_new - t_old;
  if( dt < 0) {
    /* anl_msg_error("minus time  t=%f \n", dt); */
      return saapass_new;    
  }



  mjd_now = aste2mjd(t_new);
  /* calc saa_hxd_new */
  istat = aste_orbit(orbitp, t_new, vSat0, NULL);
  if ( istat ) { anl_msg_error("aste_orbit(0), no valid interval for t=%f mjd=%f \n", t_new,mjd_now);
    return -1;}
  atGeodetic(mjd_now, vSat0, vSatG0);
  atVectToPol(vSatG0, &pvSatG0);
  atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd_new);

  
  /* check weather or not to recalc saapass is needed. */
  if( first_flag == 1) {
    calc_flag = 1;
    if ( saa_hxd_new == 0 ){
      first_saaremote_flag=1;}
    else{/* inside saa for the first event */
      first_saaremote_flag=0;}
  }
  else if( 0 == saa_hxd_new && saa_hxd_old ) {/* saa out */
    calc_flag = 1;
    /* printf("why calc_flag stand (1) %d %d\n",saa_hxd_new,saa_hxd_old); */
  }
  else { /* in saa or still outside saa */
    calc_flag = 0;
  }
  

  if (calc_flag){

    /* printf("stand cal_flag %d \n",calc_flag); */

    if(first_saaremote_flag == 1 || first_flag == 0){/* firstevent or non-first event during saaremote */
      /* printf("stand first_saaremote or nonfirst event %d \n",first_saaremote_flag); */
      t0 = t_new;
      saa_hxd0=saa_hxd_new;
      while ( ! saa_hxd0 )  {
	/* go past until SAA */
	t0 = t0 - 60.0;
	mjd0 = aste2mjd(t0);
	istat = aste_orbit(orbitp, t0, vSat0, NULL);
	if ( istat ) { anl_msg_error("aste_orbit(1), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	  return -1;}
	atGeodetic(mjd0, vSat0, vSatG0);
	atVectToPol(vSatG0, &pvSatG0);
	atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
      }
      while ( saa_hxd0 ) {
	/* go future until SAA out*/
	t0 = t0 + 1.0;
	mjd0 = aste2mjd(t0);
	istat = aste_orbit(orbitp, t0, vSat0, NULL);
	if ( istat ) { anl_msg_error("aste_orbit(2), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	  return -1;}
	atGeodetic(mjd0, vSat0, vSatG0);
	atVectToPol(vSatG0, &pvSatG0);
	atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
      }
      last_saa_hxd_time = t0;
      
      t0 = t_new;
      saa_hxd0 = saa_hxd_new;
      while ( ! saa_hxd0 )  {
	t0 = t0 + 60.0;
    mjd0 = aste2mjd(t0);
    istat = aste_orbit(orbitp, t0, vSat0, NULL);
    if ( istat ) { anl_msg_error("aste_orbit(3), no valid interval for t=%f mjd=%f \n", t0,mjd0);
      return -1;}
    atGeodetic(mjd0, vSat0, vSatG0);
    atVectToPol(vSatG0, &pvSatG0);
    atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
      }
      while ( saa_hxd0 ) {
	t0 = t0 - 1.0;
	mjd0 = aste2mjd(t0);
	istat = aste_orbit(orbitp, t0, vSat0, NULL);
	if ( istat ) { anl_msg_error("aste_orbit(4), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	  return -1;}
	atGeodetic(mjd0, vSat0, vSatG0);
	atVectToPol(vSatG0, &pvSatG0);
	atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
      }
      
  next_saa_hxd_time = t0;

  dt_saa = next_saa_hxd_time - last_saa_hxd_time;
    }
    else{/* first_saaremote_flag == 0 (in saa for the first event) */
      /* printf("not stand first_saaremote %d \n",first_saaremote_flag); */
      t0 = t_new;
      t0 = t0 - shift_t;
      mjd0 = aste2mjd(t0);
      istat = aste_orbit(orbitp, t0, vSat0, NULL);
      if ( istat ) { anl_msg_error("aste_orbit(5), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	return -1;}
      atGeodetic(mjd0, vSat0, vSatG0);
      atVectToPol(vSatG0, &pvSatG0);
      atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);

      while ( saa_hxd0 ) {
	t0 = t0 - 500.0;
	mjd0 = aste2mjd(t0);
	istat = aste_orbit(orbitp, t0, vSat0, NULL);
	if ( istat ) { anl_msg_error("aste_orbit(11a), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	  return -1;}
	atGeodetic(mjd0, vSat0, vSatG0);
	atVectToPol(vSatG0, &pvSatG0);
	atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
      }

      
      while ( ! saa_hxd0 )  {
	/* go past until SAA */
	t0 = t0 - 60.0;
	mjd0 = aste2mjd(t0);
	istat = aste_orbit(orbitp, t0, vSat0, NULL);
	if ( istat ) { anl_msg_error("aste_orbit(6), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	  return -1;}
	atGeodetic(mjd0, vSat0, vSatG0);
	atVectToPol(vSatG0, &pvSatG0);
	atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
      }
      
      while ( saa_hxd0 ) {
	/* go future until SAA out*/
	t0 = t0 + 1.0;
	mjd0 = aste2mjd(t0);
	istat = aste_orbit(orbitp, t0, vSat0, NULL);
	if ( istat ) { anl_msg_error("aste_orbit(7), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	  return -1;}
	atGeodetic(mjd0, vSat0, vSatG0);
	atVectToPol(vSatG0, &pvSatG0);
	atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
      }
      last_saa_hxd_time = t0;
  
      t0 = t_new;
      t0 = t0 - shift_t;
      mjd0 = aste2mjd(t0);
      istat = aste_orbit(orbitp, t0, vSat0, NULL);
      if ( istat ) { anl_msg_error("aste_orbit(8), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	return -1;}
      atGeodetic(mjd0, vSat0, vSatG0);
      atVectToPol(vSatG0, &pvSatG0);
      atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);

      while ( saa_hxd0 ) {
	t0 = t0 - 500.0;
	mjd0 = aste2mjd(t0);
	istat = aste_orbit(orbitp, t0, vSat0, NULL);
	if ( istat ) { anl_msg_error("aste_orbit(11a), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	  return -1;}
	atGeodetic(mjd0, vSat0, vSatG0);
	atVectToPol(vSatG0, &pvSatG0);
	atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
      }

      
      while ( ! saa_hxd0 )  {
	t0 = t0 + 60.0;
	mjd0 = aste2mjd(t0);
	istat = aste_orbit(orbitp, t0, vSat0, NULL);
	if ( istat ) { anl_msg_error("aste_orbit(9), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	  return -1;}
	atGeodetic(mjd0, vSat0, vSatG0);
	atVectToPol(vSatG0, &pvSatG0);
	atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
      }
      while ( saa_hxd0 ) {
	t0 = t0 - 1.0;
	mjd0 = aste2mjd(t0);
	istat = aste_orbit(orbitp, t0, vSat0, NULL);
	if ( istat ) { anl_msg_error("aste_orbit(10), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	  return -1;}
	atGeodetic(mjd0, vSat0, vSatG0);
	atVectToPol(vSatG0, &pvSatG0);
	atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
      }
      next_saa_hxd_time = t0;
      dt_saa = next_saa_hxd_time - last_saa_hxd_time;
    }
 
   
    /* end dt_saa */
  
    /* printf("why calc_flag stand (2) %d %d\n",saa_hxd_new,saa_hxd_old); */

    /* printf("show dt_saa %f \n",dt_saa); */

     if ( dt_saa < 0 ) { anl_msg_error("aste_orbit(m1), calc saapass failed. t=%f mjd=%f \n", t0,mjd0);
       return -1;}


    if(dt_saa > 10000) {/* saa remote */
      saapass_new=0;
    } else if( first_flag == 1) {
      
      while(dt_saa < 10000) {
	back_counter++;
	/* calc to get saa remote */

	tmp_last_saa_hxd_time=last_saa_hxd_time;
	
	t0 = last_saa_hxd_time - shift_t;
	mjd0 = aste2mjd(t0);
	istat = aste_orbit(orbitp, t0, vSat0, NULL);
	if ( istat ) { anl_msg_error("aste_orbit(11), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	  return -1;}
	atGeodetic(mjd0, vSat0, vSatG0);
	atVectToPol(vSatG0, &pvSatG0);
	atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);

	while ( saa_hxd0 ) {
	  t0 = t0 - 500.0;
	  mjd0 = aste2mjd(t0);
	  istat = aste_orbit(orbitp, t0, vSat0, NULL);
	  if ( istat ) { anl_msg_error("aste_orbit(11a), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	    return -1;}
	  atGeodetic(mjd0, vSat0, vSatG0);
	  atVectToPol(vSatG0, &pvSatG0);
	  atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
	}


	
	while ( ! saa_hxd0 )  {
	  /* go past until SAA */
	  t0 = t0 - 60.0;
	  mjd0 = aste2mjd(t0);
	  istat = aste_orbit(orbitp, t0, vSat0, NULL);
	  if ( istat ) { anl_msg_error("aste_orbit(12), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	    return -1;}
	  atGeodetic(mjd0, vSat0, vSatG0);
	  atVectToPol(vSatG0, &pvSatG0);
	  atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
	}
	while ( saa_hxd0 ) {
	  /* go future until SAA out*/
	  t0 = t0 + 1.0;
	  mjd0 = aste2mjd(t0);
	  istat = aste_orbit(orbitp, t0, vSat0, NULL);
	  if ( istat ) { anl_msg_error("aste_orbit(13), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	    return -1;}
	  atGeodetic(mjd0, vSat0, vSatG0);
	  atVectToPol(vSatG0, &pvSatG0);
	  atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
	}
	last_saa_hxd_time = t0;
	
	
	t0 = tmp_last_saa_hxd_time - shift_t;
	mjd0 = aste2mjd(t0);
	istat = aste_orbit(orbitp, t0, vSat0, NULL);
	if ( istat ) { anl_msg_error("aste_orbit(14), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	  return -1;}
	atGeodetic(mjd0, vSat0, vSatG0);
	atVectToPol(vSatG0, &pvSatG0);
	atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);

	while ( saa_hxd0 ) {
	  t0 = t0 - 500.0;
	  mjd0 = aste2mjd(t0);
	  istat = aste_orbit(orbitp, t0, vSat0, NULL);
	  if ( istat ) { anl_msg_error("aste_orbit(11a), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	    return -1;}
	  atGeodetic(mjd0, vSat0, vSatG0);
	  atVectToPol(vSatG0, &pvSatG0);
	  atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
	}

		

	while ( ! saa_hxd0 )  {
	  t0 = t0 + 60.0;
	  mjd0 = aste2mjd(t0);
	  istat = aste_orbit(orbitp, t0, vSat0, NULL);
	  if ( istat ) { anl_msg_error("aste_orbit(15), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	    return -1;}
	  atGeodetic(mjd0, vSat0, vSatG0);
	  atVectToPol(vSatG0, &pvSatG0);
	  atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
	}
	while ( saa_hxd0 ) {
	  t0 = t0 - 1.0;
	  mjd0 = aste2mjd(t0);
	  istat = aste_orbit(orbitp, t0, vSat0, NULL);
	  if ( istat ) { anl_msg_error("aste_orbit(16), no valid interval for t=%f mjd=%f \n", t0,mjd0);
	    return -1;}
	  atGeodetic(mjd0, vSat0, vSatG0);
	  atVectToPol(vSatG0, &pvSatG0);
	  atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
	}


	next_saa_hxd_time = t0;
	dt_saa = next_saa_hxd_time - last_saa_hxd_time;
	/* printf("try backing(dt_saa,next,last,back_counter) %f %f %f %d \n",dt_saa,next_saa_hxd_time,last_saa_hxd_time,back_counter); */
	
	if ( dt_saa < 0 ) { anl_msg_error("aste_orbit(m1), calc saapass failed. t=%f mjd=%f \n", t0,mjd0);
	  return -1;}

      }


      saapass_new=back_counter;
    }

    else{
      /* printf("why calc_flag stand (2) %d %d\n",saa_hxd_new,saa_hxd_old);*/
      /* printf("just increse 1pass (dt_saa,next,last,back_counter) %f %f %f %d %d \n",dt_saa,next_saa_hxd_time,last_saa_hxd_time,back_counter,saapass_new); */
      saapass_new++;
    }
    
  }
  
  /* printf("why calc_flag stand (l1) %d %d\n",saa_hxd_new,saa_hxd_old);*/
  saa_hxd_old=saa_hxd_new;
  /* printf("why calc_flag stand (l2) %d %d\n",saa_hxd_new,saa_hxd_old);*/

  first_flag=0;

  /*  *saapass=saapass_new; */
  
  dt_print = t_new - t_old_print;

  if( dt_print > 60){ 
    /*    printf("xyz vector \n"); */
    /*    printf("allvalue %10.5f %d %d %d %d \n",t_new,saa_hxd0,*saapass,first_flag,saapass_new); */
    t_old_print=t_new;
  }  

  t_old=t_new;

  return  saapass_new;
  
}      

void hxdpiUtil_modify_eventdata(double upi_fast, double upi_slow, 
				double upi_pin0, double upi_pin1,
				double upi_pin2, double upi_pin3, 
				int pi_fast, int pi_slow,
				int pi_pin0, int pi_pin1,
				int pi_pin2, int pi_pin3,
				HxdEventFits02 *event_data){
  event_data->pi_fast =  pi_fast;
  event_data->pi_slow =  pi_slow;
  event_data->pi_pin0 =  pi_pin0;
  event_data->pi_pin1 =  pi_pin1;
  event_data->pi_pin2 =  pi_pin2;
  event_data->pi_pin3 =  pi_pin3;
  event_data->upi_fast =  upi_fast;
  event_data->upi_slow =  upi_slow;
  event_data->upi_pin0 =  upi_pin0;
  event_data->upi_pin1 =  upi_pin1;
  event_data->upi_pin2 =  upi_pin2;
  event_data->upi_pin3 =  upi_pin3;
}
/*
#define HXD_PIN_PER_WEL_NUM 4

#define FAST_MAX_CH 4096
#define SLOW_MAX_CH 4096
#define PIN_MAX_CH 256

void hxdpi_check( double pi_fast, double pi_slow, double *pi_pin,
                 int *check_pi_fast, int *check_pi_slow, int *check_pi_pin ){

    int i;
    
    *check_pi_fast = (int)(pi_fast+0.5);
    if ( *check_pi_fast < 0 ){
        *check_pi_fast = 0;
    } else if ( *check_pi_fast > FAST_MAX_CH ){
        *check_pi_fast = FAST_MAX_CH;
    }
    
    *check_pi_slow = (int)(pi_slow+0.5);
    if ( *check_pi_slow < 0 ){
        *check_pi_slow = 0;
    } else if ( *check_pi_slow > SLOW_MAX_CH ){
        *check_pi_slow = SLOW_MAX_CH;
    }
    
    for(i=0;i<HXD_PIN_PER_WEL_NUM;i++){
        check_pi_pin[i] = (int)(pi_pin[i]+0.5);
        if( check_pi_pin[i]<0 ){
            check_pi_pin[i] = 0;            
        } else if ( check_pi_pin[i] > PIN_MAX_CH ){
            check_pi_pin[i] = PIN_MAX_CH;
        }
    }
    
}
*/

int hxdpiUtil_half_adjust( double *double_value ){
  int int_val;
  double delta;

  int_val = (int) *double_value;

  delta = *double_value - (double) int_val;

  if(delta >= 0.5) int_val ++;

  return int_val;

}
