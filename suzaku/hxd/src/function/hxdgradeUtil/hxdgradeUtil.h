#ifndef _HXD_GRADE_UTIL_H_
#define _HXD_GRADE_UTIL_H_

#include "hxdeventFitsUtil.h"

/* ==========================
 *      version check
 * ==========================*/
int hxdgradeUtil_check_version( char* hxdgrade_module_version );

/* ==========================
 *      calc GRADE
 * ==========================*/
#define HXDGRADEUTIL_OK 1
#define HXDGRADEUTIL_NG 0

int hxdgradeUtil_init( char *pin_ld_thes_fname, char *psdsel_ascii_fname,
		       double *psdsel_criteria_init, char* psdsel_type );
int hxdgradeUtil_endrun( void );

int hxdgradeUtil_calc_qualty(HxdEventFits02 *event,
			     int *grade_qualty);


#define HXD_GRADE_PIN_UNIT_NUM  64

int hxdgradeUtil_calc_pmttrg(HxdEventFits02 *event, int *grade_pmttrg);

#define HXDGRADEUTIL_PINID_PIN0        0
#define HXDGRADEUTIL_PINID_PIN1        1
#define HXDGRADEUTIL_PINID_PIN2        2
#define HXDGRADEUTIL_PINID_PIN3        3
#define HXDGRADEUTIL_PINID_UNDEFINED 255

int hxdgradeUtil_calc_pintrg(HxdEventFits02 *event,
			     int *pin_id, int *grade_pintrg);

int hxdgradeUtil_calc_psetrg(HxdEventFits02 *event,
			     int *grade_psetrg);

int hxdgradeUtil_calc_psdsel(HxdEventFits02 *event,
			     double *grade_psdsel);

int hxdgradeUtil_calc_hitpat(HxdEventFits02 *event,
			     int *grade_hitpat);

int hxdgradeUtil_calc_reserv(HxdEventFits02 *event,
			     int *grade_reserv);

/* ==========================
 *      calc DET_TYPE
 * ==========================*/
/* (memo)
 * DET_TYPE:          1999-12-10 HXD meeting
 *                    2005-01-18 mod, for HXD-II
 *                    2006-06-08 mod,
 *                    2006-09-13 add det_type = 2 (cleaned Pseudo events)
 *     gso     event   det_type = 0;
 *           GRADE_QUALTY = 0    && GRADE_PMTTRG = 0
 *        && GRADE_PSDSEL <= 2.1 && GRADE_HITPAT <= 2
 *     pin     event   det_type = 1;
 *           GRADE_QUALTY = 0 && GRADE_PINTRG = 0 && GRADE_HITPAT <= 2
 *     pseudo  event   det_type = 2;
 *           GRADE_QUALTY = 0 && GRADE_PSETRG = 0 && GRADE_HITPAT <= 2
 *     others       det_type = 255;
 */
#define HXDGRADEUTIL_DETTYPE_GSO         0
#define HXDGRADEUTIL_DETTYPE_PIN         1
#define HXDGRADEUTIL_DETTYPE_PSEUDO      2 /* add v2.0.2 */
#define HXDGRADEUTIL_DETTYPE_UNDEFINED 255

int hxdgradeUtil_calc_dettype( int    grade_qualty,
			       int    grade_pmttrg,
			       int    grade_pintrg,
			       int    grade_psetrg, /* add v2.0.2 */
			       double grade_psdsel,
			       int    grade_hitpat,
			       int    grade_reserv,
			       int    *det_type );

int hxdgradeUtil_copy_pipin( HxdEventFits02 *event, int det_type, int pin_id,
			     double *upi_pin, int *pi_pin);

int hxdgradeUtil_modify_eventdata( int    grade_qualty,
				   int    grade_pmttrg,
				   int    grade_pintrg,
				   double grade_psdsel,
				   int    grade_hitpat,
				   int    grade_reserv,
				   int    det_type,
				   double upi_pin, int pi_pin,
				   int    pin_id,
				   HxdEventFits02 *event );

#endif
