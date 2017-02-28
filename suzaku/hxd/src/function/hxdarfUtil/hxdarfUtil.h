#ifndef _HXD_ARF_UTIL_H_
#define _HXD_ARF_UTIL_H_
/*
 *    hxdarfUtil, for critical ftool named hxdarfgen.
 *
 *      version 0.1.x  originally developed by Y.Matsumoto 2000-02-04
 *              Not Used for the HXD-II
 *
 *      version 0.4.0  created by Y.Terada for HXD-II      2005-05-10
 *              with hxdcaldbUtil version 0.2.2
 *                   hxdrspUtil   version 0.1.0
 *      version 0.4.1  change teldef format by Y.Terada    2005-05-10
 *              with hxdcaldbUtil version 0.2.3
 *                   hxdrspUtil   version 0.1.0
 *      version 0.4.2  add (dummy) attitude I/O 
 *                   read header of PI file by K.Tamura, Y.Terada 2005-05-23
 *      version 0.4.3  usr aste_cood and atFunctions by Y.Terada 2005-06-06
 *
 *      version 0.4.4  modifiy effective area to 2.55 x 2.55 cm2
 *                    by K.Tamura 2005-06-15
 *
 *      version 0.5.0  move declaration of hxdarfUtil_read_pifile and
 *                      hxdarfUtil_conv_radec_detdeg from hxdarfUtil.c to here
 *                      by T.Kitaguchi 2005-09-07
 */

#include "hxdcaldbUtil.h"
#include "hxdrspUtil.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_teldef.h"

typedef struct{
  double angle; /** arcmin **/
  double specrsp[HXD_ARFDB_U_N_EBIN_MAX];
  int    e_ch;
} HxdArfAngle;

typedef struct {
  TELDEF *teldef;
  /*  TELDEF_ASTROE  teldef; */
  /*ATTFILE *attfile; */
  HxdArfdbUnit arfdb_data;
  AtEulerAng eulerang;
  SKYREF     skyref;
  /*HxdTelDef    teldef_data; */
  int arfdb_data_type; /** PIN or GSO **/
} hxdarfUtil_common;

#define HXDARFUTIL_WMAP_MAX_BIN 64
typedef struct {
  double exposure_map[HXDARFUTIL_WMAP_MAX_BIN];
  double ra_obj;
  double dec_obj;
  double ra_pnt;
  double dec_pnt;
  double ra_nom;
  double dec_nom;
  double roll_nom;
} hxdarfUtil_pifile;

#define HXDARFUTIL_ARFDB_TYPE_PIN   10
#define HXDARFUTIL_ARFDB_TYPE_GSO   20
#define HXDARFUTIL_ARFDB_TYPE_UNDEF 99

/** ----------------------------------------------- **/
/***           public functions                    ***/
/** ----------------------------------------------- **/
#define HXDARFUTIL_STATUS_OK 1
#define HXDARFUTIL_STATUS_NG 0

int hxdarfUtil_init(int  arfdbunit_type, /** PIN or GSO **/
		    char *arfdbunit_fits_fname,
		    char *teldef_fits_fname,
		    char *attitude_fits_fname,
		    char *pi_file_name);

int hxdarfUtil_ascii_init(int  arfdbunit_type, /** PIN or GSO **/
			  char *arfdbUnit_ascii_list_fname,
			  char *arfdbUnit_ascii_edef_fname,
			  char *teldef_fits_fname,
			  char *attitude_fits_fname,
			  char *pi_file_name);

int hxdarfUtil_GSO( int unit_id,
		    double target_ra, double target_dec,
		    HxdArfData *arf);

int hxdarfUtil_PIN( int pin_id,
		    double target_ra, double target_dec,
		    HxdArfData *arf);

int hxdarfUtil_end( void );

#define HXDARFUTIL_UNIT_NUMBER_PIN 64
#define HXDARFUTIL_UNIT_NUMBER_GSO 16

#define HXDARFUTIL_GEOMETRICAL_AREA_PIN 6.5025 /** in cm2, 25.5mm x 25.5mm **/
#define HXDARFUTIL_GEOMETRICAL_AREA_GSO 26.01  /** in cm2, 25.5mm x 25.5mm x 4 **/

void hxdarfUtil_clear_HxdArfData(      HxdArfData *arf, int nrow);

void hxdarfUtil_merge_HxdArfData(      HxdArfData *arf_in_a,
				       HxdArfData *arf_in_b,
				       HxdArfData *arf_ans,
				       double ratio_a_b);
void hxdarfUtil_add_HxdArfData(        HxdArfData *arf_in_a,
				       HxdArfData *arf_in_b,
				       HxdArfData *arf_ans,
				       double core_a, double core_b);
void hxdarfUtil_copy_HxdArfData(       HxdArfData *arf_in,
				       HxdArfData *arf_ans);

#define HXDARFUTIL_PIFILE_TYPE_PIN  0
#define HXDARFUTIL_PIFILE_TYPE_GSO  1

int hxdarfUtil_get_Exp_Ratio( int dettype, double *exp_ratio );

int hxdarfUtil_read_pifile( char *pi_file_name,
			    hxdarfUtil_pifile *pifile);

int hxdarfUtil_conv_radec_detdeg( double ra, double dec,
				  double *det_x_deg,
				  double *det_y_deg,
				  hxdarfUtil_pifile *pifile);


/******************** EOF ********************/
#endif
