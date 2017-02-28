#ifndef _HXD_PI_UTIL_H_
#define _HXD_PI_UTIL_H_

#define HXDPIUTIL_OK 1
#define HXDPIUTIL_NG 0
#include "hxdeventFitsUtil.h"

int   hxdpiUtil_check_version( char* hxdpi_module_version );
void  hxdpiUtil_modify_eventdata(double upi_fast, double upi_slow, 
				 double upi_pin0, double upi_pin1,
				 double upi_pin2, double upi_pin3, 
				 int pi_fast, int pi_slow,
				 int pi_pin0, int pi_pin1,
				 int pi_pin2, int pi_pin3,
				 HxdEventFits02 *event_data);
int hxdpiUtil_half_adjust( double *double_value );


void hxdpiUtil_orb_init(char* orbit, int* istat);

#define HXDPIUTIL_HK_COLNUM 48
#define HXDPIUTIL_EHK_COLNUM 33

typedef struct { 
  double hxd_temp_w10_cal;
  double hxd_temp_w11_cal;
  double hxd_temp_w12_cal;
  double hxd_temp_w13_cal;
  double hxd_temp_w20_cal;
  double hxd_temp_w21_cal;
  double hxd_temp_w22_cal;
  double hxd_temp_w23_cal;
  double hxd_temp_w00_cal;
  double hxd_temp_w01_cal;
  double hxd_temp_w02_cal;
  double hxd_temp_w03_cal;
  double hxd_temp_w30_cal;
  double hxd_temp_w31_cal;
  double hxd_temp_w32_cal;
  double hxd_temp_w33_cal;
  double hxd_temp_t10_cal;
  double hxd_temp_t12_cal;
  double hxd_temp_t14_cal;
  double hxd_temp_t21_cal;
  double hxd_temp_t23_cal;
  double hxd_temp_hv_w2_cal;
  double hxd_temp_hv_p1_cal;
  double hxd_temp_hv_t1_cal;
  double hxd_temp_t00_cal;
  double hxd_temp_t02_cal;
  double hxd_temp_t04_cal;
  double hxd_temp_t31_cal;
  double hxd_temp_t33_cal;
  double hxd_temp_hv_w0_cal;
  double hxd_temp_hv_p0_cal;
  double hxd_temp_hv_t3_cal;
  double hxd_temp_cap4_cal;
  double hxd_temp_cap3_cal;
  double hxd_temp_body4_cal;
  double hxd_temp_body3_cal;
  double hxd_temp_btm3_cal;
  double hxd_temp_btm4_cal;
  double hxd_temp_bar3_cal;
  double hxd_temp_center_cal;
  double hxd_temp_cap2_cal;
  double hxd_temp_cap1_cal;
  double hxd_temp_body2_cal;
  double hxd_temp_body1_cal;
  double hxd_temp_btm1_cal;
  double hxd_temp_btm2_cal;
  double hxd_temp_bar1_cal;
  double hxd_temp_bar2_cal;
} hxdpiUtil_HK;


typedef struct {
  double time;
  int yyyymmdd;
  int hhmmss;
  float euler1;
  float euler2;
  float euler3;
  float foc_ra;
  float foc_dec;
  float foc_roll;
  float dlt_ra;
  float dlt_dec;
  float dlt_roll;
  float ang_dist;
  float sat_alt;
  float sat_lon;
  float sat_lat;
  float elv;
  float dye_elv;
  float nte_elv;
  float sun_alt;
  float t_dy_nt;
  float tn_dy_nt;
  float cor;
  int saa;
  float t_saa;
  float tn_saa;
  int saa_hxd;
  float t_saa_hxd;
  float tn_saa_hxd;
  float zgmag_ang;
  float zgmag_phi;
  float ze_ang;
  float ze_phi;
} hxdpiUtil_EHK;


int hxdpiUtil_orb_calcsaa(HxdEventFits02* eventdata);
/*void hxdpiUtil_orb_calcsaa(HxdEventFits02* eventdata,int* saapass);*/

#include "hxdpiUtil_gso.h"
#include "hxdpiUtil_pin.h"

#endif
