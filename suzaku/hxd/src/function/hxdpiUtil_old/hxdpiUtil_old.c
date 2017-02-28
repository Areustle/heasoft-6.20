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
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hxdpiUtil_old.h"

int hxdpiUtil_old_check_version( char* hxdpi_module_version){
    return HXDPIUTIL_OK;
}

void hxdpiUtil_old_modify_eventdata(double upi_fast, double upi_slow, 
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

int hxdpiUtil_old_half_adjust( double *double_value ){
  int int_val;
  double delta;

  int_val = (int) *double_value;

  delta = *double_value - (double) int_val;

  if(delta >= 0.5) int_val ++;

  return int_val;

}
