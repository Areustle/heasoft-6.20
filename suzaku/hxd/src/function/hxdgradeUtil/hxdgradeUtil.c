/* hxdgradeUtil:
 * v0.1.0 2005-01-18, created by T.Kitaguchi
 * v0.1.2 2005-02-11, debug
 * v0.1.3 2005-05-10, change comment out
 * v0.1.4 2005-05-10, BnkPut pin_id
 * v0.1.5 2005-05-26, support psdsel() ASCII I/O, pi_pin : int -> double
 * v0.1.6 2005-06-02, support psdsel() FITS  I/O
 * v0.1.7 debug
 * v0.1.8 2005-06-11, double PSDSEL (Y.Terada)
 * v0.1.9 2005-06-14, GRADE_PMTTRG = 0 changed
 * v0.2.0 2005-07-01, debug in hxdgradeUtil_calc_psdsel function
 * v0.2.1 2005-08-30, support PIN PI threshold in GRADE_PINTRG
 * v0.2.2 2005-09-12, change GRADE_HITPAT_DEFAULT = 1
 *                     and change GRADE_PSDSEL_DEFAULT = 3.0
 * v0.2.3 2005-11-16, correspond to gsopsd caldb format revision
 *                     and change GRADE_PSDSEL_DEFAULT = 4.0
 * v0.2.4 2006-01-19, change GRADE_PSDSEL_DEFAULT = 2.1
 * v0.2.5 2006-05-01, change hit-multiplicity definition
 * v0.2.6 2006-06-08, input PSDSEL criteria with PIL
 * v0.2.7 2006-08-18, save Memory, by Y.Terada
 * v0.2.7 2006-08-23, check row_num is not negative in hxdgradeUtil_calc_psdsel
 * v2.0.0 2006-09-10, new format v2 by Y.Terada
 * v2.0.1 2006-09-12, debug for new format v2 by T.Kitaguchi
 * v2.0.2 2006-09-13  add DET_TYPE = 2 (Pseudo Event) by T.Kitaguchi
 * v2.0.3 2007-04-04  set PI_PIN when sole PIN trigger by T.Kitaguchi
 * v2.0.4 2013-10-09  add return value by Y.Terada
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hxdgradeUtil.h"
#include "hxdcaldbUtil.h"

#define HXD_GRADE_SUD_TRG             0x40
#define HXD_GRADE_ANODE_TRG           0x20
#define HXD_GRADE_PIN0_TRG            0x10
#define HXD_GRADE_PIN1_TRG            0x08
#define HXD_GRADE_PIN2_TRG            0x04
#define HXD_GRADE_PIN3_TRG            0x02
#define HXD_GRADE_PSEUDO_TRG          0x01

#define HXD_GRADE_PIN_LD_FLG          0x40
#define HXD_GRADE_PIN_DOUBLE_FLG      0x20
#define HXD_GRADE_PIN_UD_FLG          0x10
#define HXD_GRADE_RESET_FLG           0x08
#define HXD_GRADE_PMT_DOUBLE_FLG      0x04
#define HXD_GRADE_PMT_UD_FLG          0x02
#define HXD_GRADE_PSD_OUT_FLG         0x01

#define HXD_GRADE_XY_NUM              6
#define HXD_GRADE_WEL_UNIT_NUM        16
#define HXD_GRADE_ANTI_UNIT_NUM       20
#define HXD_GRADE_WEL_HITPAT_BIT_POS  0x8000
#define HXD_GRADE_ANTI_HITPAT_BIT_POS 0x80000

#define DEBUG 0

static char *pname = "hxdgradeUtil";

#define HXDGRADE_INIT_NO 0
#define HXDGRADE_INIT    0xff

/*static double pin_ld_threshold[HXD_GRADE_PIN_UNIT_NUM];*/
static double *pin_ld_threshold = NULL;
/*static Hxdgrade_Psdsel psdsel;*/
static Hxdgrade_Psdsel* psdsel = NULL;
static int init = HXDGRADE_INIT_NO;
static double psdsel_criteria;

int hxdgradeUtil_init( char *pin_ld_thes_fname, char *psdsel_fname,
		       double *psdsel_criteria_init, char* psdsel_type) {
  int status = HXDGRADEUTIL_OK;
  int stat;

  /*** memory allocation ***/
  if ( pin_ld_threshold == NULL ) {
    pin_ld_threshold = malloc( sizeof(double)*HXD_GRADE_PIN_UNIT_NUM );
  } else {
    fprintf(stderr, "%s: Memory, pin_ld_threshold, is already allocated.\n",pname);
    status = HXDGRADEUTIL_NG;
    return status;
  }
  if ( psdsel == NULL ) {
    /* printf(" %d bytes\n", sizeof(Hxdgrade_Psdsel)); */
    psdsel = (Hxdgrade_Psdsel*) malloc( sizeof(Hxdgrade_Psdsel) );
  } else {
    fprintf(stderr, "%s: Memory, psdsel, is already allocated.\n",pname);
    status = HXDGRADEUTIL_NG;
    return status;
  }

  /*** caldb access  ***/
  stat = hxdcaldbUtil_pinthres_open_FITS(pin_ld_thes_fname);
  if ( stat != HXDCALDBUTIL_STATUS_OK ) {
    status = HXDGRADEUTIL_NG;
    return status;
  }

  stat = hxdcaldbUtil_pinthres_read_FITS(pin_ld_threshold);
  if ( stat != HXDCALDBUTIL_STATUS_OK ) {
    status = HXDGRADEUTIL_NG;
    return status;
  }

  stat = hxdcaldbUtil_pinthres_close_FITS();
  if ( stat != HXDCALDBUTIL_STATUS_OK ) {
    status = HXDGRADEUTIL_NG;
    return status;
  }


  /*
  stat = hxdcaldbUtil_psdsel_open_ASCII(psdsel_fname);
  if ( stat != HXDCALDBUTIL_STATUS_OK ) {
    status = HXDGRADEUTIL_NG;
    return status;
  }

  stat = hxdcaldbUtil_psdsel_read_ASCII(&psdsel);
  if ( stat != HXDCALDBUTIL_STATUS_OK ) {
    status = HXDGRADEUTIL_NG;
    return status;
  }

  stat = hxdcaldbUtil_psdsel_close_ASCII();
  if ( stat != HXDCALDBUTIL_STATUS_OK ) {
    status = HXDGRADEUTIL_NG;
    return status;
  }
  */


  stat = hxdcaldbUtil_psdsel_open_FITS(psdsel_fname);
  if ( stat != HXDCALDBUTIL_STATUS_OK ) {
    status = HXDGRADEUTIL_NG;
    return status;
  }

  stat = hxdcaldbUtil_psdsel_read_FITS(psdsel);
  if ( stat != HXDCALDBUTIL_STATUS_OK ) {
    status = HXDGRADEUTIL_NG;
    return status;
  }
  if (strncmp(psdsel_type,psdsel->ccnm,8)){
    fprintf(stderr, "%s: %s required, but %s found in %s\n",pname,
            psdsel_type, psdsel->ccnm, psdsel_fname);
    status = HXDGRADEUTIL_NG;
    return status;
  }

  stat = hxdcaldbUtil_psdsel_close_FITS();
  if ( stat != HXDCALDBUTIL_STATUS_OK ) {
    status = HXDGRADEUTIL_NG;
    return status;
  }

  psdsel_criteria = *psdsel_criteria_init;

  init = HXDGRADE_INIT;
  return status;
}

int hxdgradeUtil_endrun( void ){
  int status = HXDGRADEUTIL_OK;
  /**do nothing **/
  return status;
}

#define HXD_GRADE_QULITY_0 0
#define HXD_GRADE_QULITY_1 1

int hxdgradeUtil_calc_qualty( HxdEventFits02 *event,
			      int *grade_qualty ) {
  int status = HXDGRADEUTIL_OK;

  /*
  static double previous_time[HXD_GRADE_WEL_UNIT_NUM] = { 0.0, 0.0, 0.0, 0.0,
							  0.0, 0.0, 0.0, 0.0,
							  0.0, 0.0, 0.0, 0.0,
							  0.0, 0.0, 0.0, 0.0 };
  */

  *grade_qualty = HXD_GRADE_QULITY_0;

  if ( !event->length_chk
       || !(event->quality_flags & HXD_GRADE_PSD_OUT_FLG)
       || (event->quality_flags &
	   (HXD_GRADE_PIN_DOUBLE_FLG + HXD_GRADE_PIN_UD_FLG + HXD_GRADE_RESET_FLG
	    + HXD_GRADE_PMT_DOUBLE_FLG + HXD_GRADE_PMT_UD_FLG)) ) {
    *grade_qualty = HXD_GRADE_QULITY_1;
  }

  /* reverse time */
  /*
  if ( event->time < previous_time[event->unitid] ) {
    *grade_qualty = HXD_GRADE_QULITY_1;
  }

  previous_time[event->unitid] = event->time;
  */

  return status;

}

#define HXD_GRADE_PMTTRG_0       0
#define HXD_GRADE_PMTTRG_1       1
#define HXD_GRADE_PMTTRG_2       2

#define HXD_GRADE_SELF_HIT       1
#define HXD_GRADE_NO_SELF_HIT    0

int hxdgradeUtil_calc_pmttrg( HxdEventFits02 *event,
			      int *grade_pmttrg ) {

  int status = HXDGRADEUTIL_OK;
  int self_hit;

  /** check initicalization, gradeinfo **/
  if (init != HXDGRADE_INIT){
    fprintf(stderr, "%s: Please Init First (calc_pmttrg)\n",pname);
    status = HXDGRADEUTIL_NG;
    return status;
  }

  if ( event->hit_pattern_well
       & (HXD_GRADE_WEL_HITPAT_BIT_POS >> event->unitid) )
    self_hit = HXD_GRADE_SELF_HIT;
  else
    self_hit = HXD_GRADE_NO_SELF_HIT;

  /*
      if ( (event->trig == HXD_GRADE_ANODE_TRG)
      && (self_hit == HXD_GRADE_SELF_HIT) 
      && (event->pha_pin0 < gradeinfo.pin_ld_threshold[event->unitid*4+0]
      && event->pha_pin1 < gradeinfo.pin_ld_threshold[event->unitid*4+1]
      && event->pha_pin2 < gradeinfo.pin_ld_threshold[event->unitid*4+2]
      && event->pha_pin3 < gradeinfo.pin_ld_threshold[event->unitid*4+3]) ) {
  */

  if ( (event->trig == HXD_GRADE_ANODE_TRG)
       && !(event->quality_flags & HXD_GRADE_PIN_LD_FLG)
       && (self_hit == HXD_GRADE_SELF_HIT) )
    *grade_pmttrg = HXD_GRADE_PMTTRG_0;     /* pure PMT event */

  else if ( (event->trig == (HXD_GRADE_ANODE_TRG + HXD_GRADE_PIN0_TRG)
	     || event->trig == (HXD_GRADE_ANODE_TRG + HXD_GRADE_PIN1_TRG)
	     || event->trig == (HXD_GRADE_ANODE_TRG + HXD_GRADE_PIN2_TRG)
	     || event->trig == (HXD_GRADE_ANODE_TRG + HXD_GRADE_PIN3_TRG))
	    && !(event->quality_flags & HXD_GRADE_PIN_LD_FLG)
	    && (self_hit == HXD_GRADE_SELF_HIT) )
    *grade_pmttrg = HXD_GRADE_PMTTRG_1;     /* Compton event */

  else
    *grade_pmttrg = HXD_GRADE_PMTTRG_2;     /* others */

  return status;

}


#define HXD_GRADE_PIN_NUM_OF_1UNIT    4

#define HXD_GRADE_PINTRG_0    0
#define HXD_GRADE_PINTRG_1    1
#define HXD_GRADE_PINTRG_2    2

int hxdgradeUtil_calc_pintrg(HxdEventFits02 *event,
			     int *pin_id, int *grade_pintrg) {

  int status = HXDGRADEUTIL_OK;
  int self_hit;

  if ( event->hit_pattern_well
       & (HXD_GRADE_WEL_HITPAT_BIT_POS >> event->unitid) ) {
    self_hit = HXD_GRADE_SELF_HIT;
  } else {
    self_hit = HXD_GRADE_NO_SELF_HIT;
  }

  if ( (event->quality_flags & HXD_GRADE_PIN_LD_FLG)
       && (self_hit == HXD_GRADE_NO_SELF_HIT) )  {

    switch ( event->trig ) {

    case HXD_GRADE_PIN0_TRG:
      *pin_id = HXDGRADEUTIL_PINID_PIN0;

      if ( pin_ld_threshold[event->unitid*HXD_GRADE_PIN_NUM_OF_1UNIT+0]
	   < event->upi_pin0 )
	*grade_pintrg = HXD_GRADE_PINTRG_0;     /* pure PIN0 event */
      else
	*grade_pintrg = HXD_GRADE_PINTRG_1;     /* PIN0 event below LD */
      break;

    case HXD_GRADE_PIN1_TRG:
      *pin_id = HXDGRADEUTIL_PINID_PIN1;

      if ( pin_ld_threshold[event->unitid*HXD_GRADE_PIN_NUM_OF_1UNIT+1]
	   < event->upi_pin1 )
	*grade_pintrg = HXD_GRADE_PINTRG_0;     /* pure PIN1 event */
      else
	*grade_pintrg = HXD_GRADE_PINTRG_1;     /* PIN1 event below LD */
      break;

    case HXD_GRADE_PIN2_TRG:
      *pin_id = HXDGRADEUTIL_PINID_PIN2;

      if ( pin_ld_threshold[event->unitid*HXD_GRADE_PIN_NUM_OF_1UNIT+2]
	   < event->upi_pin2 )
	*grade_pintrg = HXD_GRADE_PINTRG_0;     /* pure PIN2 event */
      else
	*grade_pintrg = HXD_GRADE_PINTRG_1;     /* PIN2 event below LD */
      break;

    case HXD_GRADE_PIN3_TRG:
      *pin_id = HXDGRADEUTIL_PINID_PIN3;
      if ( pin_ld_threshold[event->unitid*HXD_GRADE_PIN_NUM_OF_1UNIT+3]
	   < event->upi_pin3 )
	*grade_pintrg = HXD_GRADE_PINTRG_0;     /* pure PIN3 event */
      else
	*grade_pintrg = HXD_GRADE_PINTRG_1;     /* PIN3 event below LD */
      break;

    default:
      *grade_pintrg = HXD_GRADE_PINTRG_2;     /* others */
      *pin_id = HXDGRADEUTIL_PINID_UNDEFINED;
      break;
    }

  } else {
    *grade_pintrg = HXD_GRADE_PINTRG_2;
    *pin_id = HXDGRADEUTIL_PINID_UNDEFINED;     /* others */
  }

  return status;

}


#define HXD_GRADE_PSETRG_0 0    /* Pseudo event */
#define HXD_GRADE_PSETRG_1 1    /*    others    */

/***   add v2.0.2   ***/
int hxdgradeUtil_calc_psetrg(HxdEventFits02 *event,
			     int *grade_psetrg) {
  int status = HXDGRADEUTIL_OK;

  *grade_psetrg = HXD_GRADE_PSETRG_1;

  if ( event->trig & HXD_GRADE_PSEUDO_TRG )
    *grade_psetrg = HXD_GRADE_PSETRG_0;

  return status;

}


#define HXD_GRADE_PSDSEL_0 0
#define HXD_GRADE_PSDSEL_1 1
#define HXD_GRADE_PSDSEL_2 2
#define HXD_GRADE_PSDSEL_3 3

#define HXD_GRADE_PSDSEL_0_CUT 2.0   /* sigma */
#define HXD_GRADE_PSDSEL_1_CUT 2.5   /* sigma */
#define HXD_GRADE_PSDSEL_2_CUT 3.0   /* sigma */

#define HXD_GRADE_PSDSEL_BIN         0.1
#define HXD_GRADE_PSDSEL_INVERSE_BIN  10

#define HXD_GRADE_PSDSEL_ROUND_ERROR 1e-5

#define HXD_GRADE_PSDSEL_RPI_FAST_LD         0.01
#define HXD_GRADE_PSDSEL_DISPERSION_INVALID  10.0

#define SIN45     0.70710678118654752440   /* sqrt(2)/2 */
#define COS45     0.70710678118654752440   /* sqrt(2)/2 */
#define NORM      1.41421356237309504880   /* sqrt(2) */

int hxdgradeUtil_calc_psdsel(HxdEventFits02 *event,
			     double *grade_psdsel) {
  int status = HXDGRADEUTIL_OK;

  /*** RPI is PI in 45-degree counterclockwise rotation ***/
  double rpi_fast, rpi_slow, rpi_fast_decimal;
  double rpi_slow_center, rpi_slow_1sigma_upper, rpi_slow_1sigma_lower;

  int row_num, row_1unit;
  double dispersion_over_sigma;

  row_1unit = psdsel->n_row / HXD_GRADE_WEL_UNIT_NUM;


  /*** -45 degree rotation ***/
  rpi_fast = (+ event->upi_fast * COS45 + event->upi_slow * SIN45) / NORM;
  rpi_slow = (- event->upi_fast * SIN45 + event->upi_slow * COS45) / NORM;

  rpi_fast_decimal = rpi_fast - (int)rpi_fast;

  row_num = (int)(rpi_fast*HXD_GRADE_PSDSEL_INVERSE_BIN) + row_1unit*event->unitid;


  /*** check data format of PSDSEL caldb ***/
  if ( (row_num + 1) > psdsel->n_row ) {
    fprintf(stderr,
            "%s: %d-th row is above maximum (%d) of PSD caldb file\n",pname,
            row_num+1, psdsel->n_row);
    status = HXDGRADEUTIL_NG;
    return status;
  } else if ( row_num < 0 ) {
    fprintf(stderr,
            "%s: row number (%d) is negative.\n",pname,
	    row_num);
    status = HXDGRADEUTIL_NG;
    return status;
  } else if ( ((rpi_fast + HXD_GRADE_PSDSEL_ROUND_ERROR)
	       < psdsel->data[row_num].rpi_fast)
	      || ((rpi_fast - HXD_GRADE_PSDSEL_ROUND_ERROR)
		  >= psdsel->data[row_num+1].rpi_fast)) {
    fprintf(stderr,
            "%s: Data format of this module conflicts with that of PSD caldb file.\n\
hxdgradeUtil: Does not satisfy the following condition.\n\
hxdgradeUtil: psdsel->rpi_fast[%d]      rpi_fast     psdsel->rpi_fast[%d]\n\
hxdgradeUtil:        %3.1f          <=  %f   <         %3.1f\n",pname,
	    row_num, row_num+1,
	    psdsel->data[row_num].rpi_fast, rpi_fast,
	    psdsel->data[row_num+1].rpi_fast);

    status = HXDGRADEUTIL_NG;
    return status;
  }


  /*** interpolation of PSD selection curve ***/
  rpi_slow_center =
    psdsel->data[row_num].rpi_slow_center * (1.0 - rpi_fast_decimal)
    + psdsel->data[row_num+1].rpi_slow_center * rpi_fast_decimal;
  rpi_slow_1sigma_upper =
    psdsel->data[row_num].rpi_slow_1sigma_upper * (1.0 - rpi_fast_decimal)
    + psdsel->data[row_num+1].rpi_slow_1sigma_upper * rpi_fast_decimal;
  rpi_slow_1sigma_lower =
    psdsel->data[row_num].rpi_slow_1sigma_lower * (1.0 - rpi_fast_decimal)
    + psdsel->data[row_num+1].rpi_slow_1sigma_lower * rpi_fast_decimal;

  if ( rpi_fast < HXD_GRADE_PSDSEL_RPI_FAST_LD )    /* Rotated PI_FAST LD */
    dispersion_over_sigma = HXD_GRADE_PSDSEL_DISPERSION_INVALID;

  else if ( rpi_slow > rpi_slow_center )
    dispersion_over_sigma
      = (rpi_slow - rpi_slow_center) / rpi_slow_1sigma_upper;

  else
    dispersion_over_sigma
      = (rpi_slow_center - rpi_slow) / rpi_slow_1sigma_lower;

  /*
  if ( dispersion_over_sigma <  HXD_GRADE_PSDSEL_0_CUT ) {
    *grade_psdsel = HXD_GRADE_PSDSEL_0;
  } else if ( dispersion_over_sigma < HXD_GRADE_PSDSEL_1_CUT ) {
    *grade_psdsel = HXD_GRADE_PSDSEL_1;
  } else if ( dispersion_over_sigma < HXD_GRADE_PSDSEL_2_CUT ) {
    *grade_psdsel = HXD_GRADE_PSDSEL_2;
  } else {
    *grade_psdsel = HXD_GRADE_PSDSEL_3;
  }
  */

  /*
  printf("unit:%d S/F=%3.2f/%3.2f\n\
%3.2f <= %3.8f < %3.2f Row:%d\n\
%3.2f c:%3.2f u:%3.2f l:%3.2f s:%1.2e\n\n",
	 event->unitid, event->upi_fast, event->upi_slow,
	 psdsel->data[row_num].rpi_fast, rpi_fast, psdsel->data[row_num+1].rpi_fast, row_num,
	 rpi_slow, rpi_slow_center, rpi_slow_1sigma_upper, rpi_slow_1sigma_lower, *grade_psdsel);
  */

  *grade_psdsel = dispersion_over_sigma;

  return status;
}


#define HXD_GRADE_HITPAT_35           0
#define HXD_GRADE_HITPAT_8            1
#define HXD_GRADE_HITPAT_4            2
#define HXD_GRADE_HITPAT_others       3
#define HXD_GRADE_HITPAT_OVER_MULTI   4

#define MULTIPLICITY                  1

#define HXD_GRADE_HIT                 1
#define HXD_GRADE_NO_HIT              0
 
int hxdgradeUtil_calc_hitpat(HxdEventFits02 *event,
			     int *grade_hitpat) {
  int status = HXDGRADEUTIL_OK;
  int wel_x[] = { 1,2,2,1,3,4,4,3,3,4,4,3,1,2,2,1 };
  int wel_y[] = { 4,4,3,3,4,4,3,3,2,2,1,1,2,2,1,1 };
  int ant_x[] = { 0,1,2,3,4,5,5,5,5,5,5,4,3,2,1,0,0,0,0,0 };
  int ant_y[] = { 5,5,5,5,5,5,4,3,2,1,0,0,0,0,0,0,1,2,3,4 };

  int wel_unit;
  int ant_unit;
  int hitpattern[HXD_GRADE_XY_NUM][HXD_GRADE_XY_NUM];
  int hit_count = 0;
  int self_hit;
  int hit_multiplicity;

  int i,j;

  if ( event->hit_pattern_well
       & (HXD_GRADE_WEL_HITPAT_BIT_POS >> event->unitid) )
    self_hit = HXD_GRADE_SELF_HIT;
  else
    self_hit = HXD_GRADE_NO_SELF_HIT;

  for ( i=0; i<HXD_GRADE_XY_NUM; i++ )
    for ( j=0; j<HXD_GRADE_XY_NUM; j++ )
      hitpattern[i][j] = HXD_GRADE_NO_HIT;

  for( wel_unit = 0; wel_unit < HXD_GRADE_WEL_UNIT_NUM; wel_unit++ ){
    if( event->hit_pattern_well & ( HXD_GRADE_WEL_HITPAT_BIT_POS >> wel_unit ) ){
      hitpattern[wel_x[wel_unit]][wel_y[wel_unit]] = HXD_GRADE_HIT;
      hit_count++;
    }
  }

  for( ant_unit = 0; ant_unit < HXD_GRADE_ANTI_UNIT_NUM; ant_unit++ ){
    if( event->hit_pattern_anti & ( HXD_GRADE_ANTI_HITPAT_BIT_POS >> ant_unit ) ){
      hitpattern[ant_x[ant_unit]][ant_y[ant_unit]] = HXD_GRADE_HIT;
      hit_count++;
    }
  }

  hit_multiplicity = hit_count - self_hit;

  if ( hit_multiplicity > MULTIPLICITY ) {
    *grade_hitpat = HXD_GRADE_HITPAT_OVER_MULTI;
    return  HXDGRADEUTIL_OK;
  }

  if ( !(event->hit_pattern_well & (~(HXD_GRADE_WEL_HITPAT_BIT_POS >> event->unitid)))
       && !event->hit_pattern_anti )
    *grade_hitpat = HXD_GRADE_HITPAT_35;

  else if ( hitpattern[wel_x[event->unitid] - 1][wel_y[event->unitid]    ] == HXD_GRADE_NO_HIT
	    && hitpattern[wel_x[event->unitid] + 1][wel_y[event->unitid]    ] == HXD_GRADE_NO_HIT
	    && hitpattern[wel_x[event->unitid]    ][wel_y[event->unitid] - 1] == HXD_GRADE_NO_HIT
	    && hitpattern[wel_x[event->unitid]    ][wel_y[event->unitid] + 1] == HXD_GRADE_NO_HIT
	    && hitpattern[wel_x[event->unitid] - 1][wel_y[event->unitid] - 1] == HXD_GRADE_NO_HIT
	    && hitpattern[wel_x[event->unitid] - 1][wel_y[event->unitid] + 1] == HXD_GRADE_NO_HIT
	    && hitpattern[wel_x[event->unitid] + 1][wel_y[event->unitid] - 1] == HXD_GRADE_NO_HIT
	    && hitpattern[wel_x[event->unitid] + 1][wel_y[event->unitid] + 1] == HXD_GRADE_NO_HIT )
    *grade_hitpat = HXD_GRADE_HITPAT_8;

  else if ( hitpattern[wel_x[event->unitid] - 1][wel_y[event->unitid]    ] == HXD_GRADE_NO_HIT
	    && hitpattern[wel_x[event->unitid] + 1][wel_y[event->unitid]    ] == HXD_GRADE_NO_HIT
	    && hitpattern[wel_x[event->unitid]    ][wel_y[event->unitid] - 1] == HXD_GRADE_NO_HIT
	    && hitpattern[wel_x[event->unitid]    ][wel_y[event->unitid] + 1] == HXD_GRADE_NO_HIT )
    *grade_hitpat = HXD_GRADE_HITPAT_4;

  else
    *grade_hitpat = HXD_GRADE_HITPAT_others;

  return status;

}



#define GRADE_QUALTY_DEFAULT    0
#define GRADE_PMTTRG_DEFAULT    0
#define GRADE_PINTRG_DEFAULT    0
#define GRADE_PSETRG_DEFAULT    0                  /* add    v2.0.2 */
#define GRADE_PSDSEL_DEFAULT    psdsel_criteria    /* change v0.2.5 */
#define GRADE_HITPAT_DEFAULT    1                  /* change v0.2.2 */

int hxdgradeUtil_calc_dettype( int grade_qualty, int grade_pmttrg,
			       int grade_pintrg, int grade_psetrg,
			       double grade_psdsel,
			       int grade_hitpat, int grade_reserv,
			       int *det_type ){
  int status = HXDGRADEUTIL_OK;

  if ( grade_qualty == GRADE_QUALTY_DEFAULT
       && grade_pmttrg == GRADE_PMTTRG_DEFAULT
       && grade_psdsel <= GRADE_PSDSEL_DEFAULT
       && grade_hitpat <= GRADE_HITPAT_DEFAULT )
    *det_type = HXDGRADEUTIL_DETTYPE_GSO;

  else if ( grade_qualty == GRADE_QUALTY_DEFAULT
	    && grade_pintrg == GRADE_PINTRG_DEFAULT
	    && grade_hitpat <= GRADE_HITPAT_DEFAULT )
    *det_type = HXDGRADEUTIL_DETTYPE_PIN;

  else if ( grade_qualty == GRADE_QUALTY_DEFAULT
	    && grade_psetrg == GRADE_PSETRG_DEFAULT
	    && grade_hitpat <= GRADE_HITPAT_DEFAULT )
    *det_type = HXDGRADEUTIL_DETTYPE_PSEUDO;

  else
    *det_type = HXDGRADEUTIL_DETTYPE_UNDEFINED;


  /* fprintf(stderr, "util DET_TYPE=%d\n\n", *det_type); */

  return status;

}

#define HXD_GRADE_PIPIN_UNDEFINED    255.0

int
hxdgradeUtil_copy_pipin( HxdEventFits02 *event, int det_type, int pin_id,
			 double *upi_pin, int *pi_pin) {
  int status = HXDGRADEUTIL_OK;

    switch ( pin_id ) {

    case HXDGRADEUTIL_PINID_PIN0:
      *pi_pin  = event->pi_pin0;
      *upi_pin = event->upi_pin0;
      break;

    case HXDGRADEUTIL_PINID_PIN1:
      *pi_pin  = event->pi_pin1;
      *upi_pin = event->upi_pin1;
      break;

    case HXDGRADEUTIL_PINID_PIN2:
      *pi_pin  = event->pi_pin2;
      *upi_pin = event->upi_pin2;
      break;

    case HXDGRADEUTIL_PINID_PIN3:
      *pi_pin  = event->pi_pin3;
      *upi_pin = event->upi_pin3;
      break;

    default:
      *pi_pin  = (int) HXD_GRADE_PIPIN_UNDEFINED;
      *upi_pin =       HXD_GRADE_PIPIN_UNDEFINED;
      break;

    }

  return status;
}

int
hxdgradeUtil_modify_eventdata( int grade_qualty, int grade_pmttrg,
			       int grade_pintrg, double grade_psdsel,
			       int grade_hitpat, int grade_reserv,
			       int det_type, double upi_pin, int pi_pin,
			       int pin_id, HxdEventFits02 *event ) {
  int status = HXDGRADEUTIL_OK;

  event->grade_qualty = (unsigned short) grade_qualty;
  event->grade_pmttrg = (unsigned char)  grade_pmttrg;
  event->grade_pintrg = (unsigned char)  grade_pintrg;
  event->grade_psdsel = grade_psdsel;
  event->grade_hitpat = (unsigned int)   grade_hitpat;
  event->grade_reserv = (unsigned int)   grade_reserv;
  event->det_type     = (unsigned char)  det_type;
  event->upi_pin      = upi_pin;
  event->pi_pin       = pi_pin;
  event->pin_id       = (unsigned char)  pin_id;

  return status;
}
