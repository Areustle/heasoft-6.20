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
 *      version 0.4.3  usr aste_cood and atFunctions by Y.Terada 2005-06-06
 *
 *      version 0.4.4  modify exposure ratio definition
 *                                             by K.Tamrua 2005-06-15
 *
 *      version 0.4.8  delete C++ Style comments     by Y.Terada 2005-10-27
 *
 *      version 0.4.9  full debug              by K.Tamrua 2005-11-04
 *
 *      version 0.5.0  remove static at declaration of
 *                      hxdarfUtil_read_pifile and hxdarfUtil_conv_radec_detdeg
 *                      by T.Kitaguchi 2006-09-07
 *
 *      version 0.5.1  add return value by Y.Terada 2013-10-09
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "hxdarfUtil.h"
#include "hxdcaldbUtil.h"
#include "hxdrspUtil.h"

#include "aste_coord.h"
#include "aste_teldef.h"

static char tool_name[] = "hxdarfUtil";
/*
static char version[]   = "version 0.5.1";
*/

#define DEBUG 0
#define ARFDB_FILE_TYPE 0

static hxdarfUtil_common com;
static hxdarfUtil_pifile pifile;

static void hxdarfUtil_search_HxdArfdbUnit(    double angle,
					      HxdArfdbUnit *arfdb_data,
					      HxdArfAngle  *arfangle_ans_prev,
					      HxdArfAngle *arfangle_ans_after);
static void hxdarfUtil_interpolate_HxdArfAngle(double angle,
					      HxdArfAngle *arfangle_prev,
					      HxdArfAngle *arfangle_after,
					      HxdArfAngle *arfangle_ans);
static void hxdarfUtil_extract_HxdArfData(    HxdArfAngle  *arfangle_ans,
					      HxdArfdbUnit *arfdb_data,
					      HxdArfData *arf);
static double hxdarfUtil_interpolate_calc(double x0, double y0, 
					  double x1, double y1, double x);
static int hxd_arfUtil_calc_arf(int det_id, double target_x_AIM,
				double target_y_AIM, HxdArfData *arf);
static int hxdarfUtil_get_ratio_target_hxdnom(HxdArfAngle *angle_target,
					      HxdArfAngle *angle_nom,
					      HxdArfAngle *angle_out);
static int hxdarfUtil_angle_target_satnom(double target_ra, double target_dec,
					  double satnom_ra, double satnom_dec,
					  double *x_min, double *y_min);



/** ----------------------------------------------- **/
/***     public functions for FITS ARFDB           ***/
/** ----------------------------------------------- **/

  int hxdarfUtil_init(int  arfdbunit_type, /** PIN or GSO **/
		      char *arfdbunit_fits_fname, 
		      char *teldef_fits_fname,
		      char *attitude_fits_fname,
		      char *pi_file_name){
    int status = HXDARFUTIL_STATUS_OK;
    int stat;
    char *telescop = "SUZAKU";
    /*   char *telescop = "Astro-E2"; */
    char *instrume = "HXD";

    /**==== read ARFDB  ====**/
    if ( arfdbunit_type == HXDARFUTIL_ARFDB_TYPE_PIN ||
	 arfdbunit_type == HXDARFUTIL_ARFDB_TYPE_GSO ) {
      com.arfdb_data_type = arfdbunit_type;
    } else {
      fprintf(stderr,"%s_init: Invalid arfdb type.\n", tool_name);
      com.arfdb_data_type = HXDARFUTIL_ARFDB_TYPE_UNDEF;
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }

    stat = hxdcaldbUtil_arfdbUnit_open_FITS(arfdbunit_fits_fname);
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: file open error\n", tool_name);
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }

    stat = hxdcaldbUtil_arfdbUnit_read_FITS(&com.arfdb_data);
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: read error\n", tool_name);
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }

    stat = hxdcaldbUtil_arfdbUnit_close_FITS();
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: close error\n", tool_name);
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }


    com.teldef = aste_coord_init(telescop, instrume, teldef_fits_fname);
    if (com.teldef == NULL){
      fprintf(stderr,"%s: teldef init error \n", tool_name);
      status = HXDARFUTIL_STATUS_NG;
      return status;
  }


  /**==== read TELDEF  ====**/
    /*
  stat = hxdcaldbUtil_teldef_open_FITS(teldef_fits_fname);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: file open error\n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  stat = hxdcaldbUtil_teldef_read_FITS(&com.teldef);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: read error\n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  stat = hxdcaldbUtil_teldef_close_FITS ();
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: close error\n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }
    */
 


  /**==== read ATTITUDE  ====**/
  /*
  com.attfile = openAttFile(attitude_fits_fname);
  if (com.attfile  == NULL){
    fprintf(stderr,"%s: attitide open error \n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }
  */

  /**=== read PI File ===**/
  stat = hxdarfUtil_read_pifile( pi_file_name, &pifile);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: read pifile error\n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  /** === convert into Euler Angle === **/


  com.skyref.alpha = pifile.ra_nom;
  com.skyref.delta = pifile.dec_nom;
  com.skyref.roll  = 90.0 - pifile.roll_nom; /* roll angle = 90 - MEAN_EA3 */
  stat = aste_skyref2euler(com.teldef, &com.skyref, &com.eulerang);
  if (stat != ASTE_COORD_NORMAL_END){
    fprintf(stderr,"%s: astecood aste_skyref2euler failed(%d) \n", 
	    tool_name, stat);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }
  if (DEBUG)
    fprintf(stderr, "%s: (RA,DEC,ROL)=(%f, %f, %f) --> Eul(%f, %f, %f)\n",
	    tool_name, pifile.ra_nom, pifile.dec_nom, pifile.roll_nom,
	    com.eulerang.phi, com.eulerang.theta, com.eulerang.psi);


  return status;
}

/** ----------------------------------------------- **/
/***     public functions for ASCII ARFDB          ***/
/** ----------------------------------------------- **/


  int hxdarfUtil_ascii_init(int  arfdbunit_type, /** PIN or GSO **/
			    char *arfdbUnit_ascii_list_fname,
			    char *arfdbUnit_ascii_edef_fname,
			    char *teldef_fits_fname,
			    char *attitude_fits_fname,
			    char *pi_file_name){

    int status = HXDARFUTIL_STATUS_OK;
    int stat;
    char *telescop = "SUZAKU";
    char *instrume = "HXD";

    /**==== read ARFDB  ====**/

    if ( arfdbunit_type == HXDARFUTIL_ARFDB_TYPE_PIN ||
	 arfdbunit_type == HXDARFUTIL_ARFDB_TYPE_GSO ) {
      com.arfdb_data_type = arfdbunit_type;
    } else {
      fprintf(stderr,"%s_init: Invalid arfdb type.\n", tool_name);
      com.arfdb_data_type = HXDARFUTIL_ARFDB_TYPE_UNDEF;
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }

    stat = hxdcaldbUtil_arfdbUnit_open_ASCII(arfdbUnit_ascii_list_fname,
					     arfdbUnit_ascii_edef_fname);
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: file open error\n", tool_name);
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }

    stat = hxdcaldbUtil_arfdbUnit_read_ASCII(&com.arfdb_data);
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: file open error\n", tool_name);
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }

    hxdcaldbUtil_arfdbUnit_close_ASCII();
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: file open error\n", tool_name);
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }

    com.teldef = aste_coord_init(telescop, instrume, teldef_fits_fname);
    if (com.teldef == NULL){
      fprintf(stderr,"%s: teldef init error \n", tool_name);
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }

  /**=== read PI File ===**/
  stat = hxdarfUtil_read_pifile( pi_file_name, &pifile);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: read pifile error\n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  /** === convert into Euler Angle === **/
  com.skyref.alpha = pifile.ra_nom;
  com.skyref.delta = pifile.dec_nom;
  com.skyref.roll  = 90.0 - pifile.roll_nom; /* roll angle = 90 - MEAN_EA3 */
  stat = aste_skyref2euler(com.teldef, &com.skyref, &com.eulerang);
  if (stat != ASTE_COORD_NORMAL_END){
    fprintf(stderr,"%s: astecood aste_skyref2euler failed(%d) \n",
	    tool_name, stat);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }
  if (DEBUG)
    fprintf(stderr, "%s: (RA,DEC,ROL)=(%f, %f, %f) --> Eul(%f, %f, %f)\n",
	    tool_name, pifile.ra_nom, pifile.dec_nom, pifile.roll_nom,
	    com.eulerang.phi, com.eulerang.theta, com.eulerang.psi);

  return status;
  }

int hxdarfUtil_GSO( int unit_id, 
		    double target_ra,
		    double target_dec,
		    HxdArfData *arf){
  int status = HXDARFUTIL_STATUS_OK;
  double target_x;
  double target_y;
  /* double satnom_theta; */
  /* double satnom_phi;   */
  /* double target_theta; */
  /* double target_phi;   */

  /** check status **/
  if (com.arfdb_data_type != HXDARFUTIL_ARFDB_TYPE_GSO){
    fprintf(stderr,"%s_GSO: Invalid Initialization.\n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  /** Calc difference between SatNominal and Target  **/
  status = hxdarfUtil_angle_target_satnom(target_ra, target_dec,
					  pifile.ra_nom, pifile.dec_nom,
					  &target_x, &target_y);
  if (status != HXDARFUTIL_STATUS_OK){
    fprintf(stderr,"%s_GSO: Calc Angle failed \n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  if(DEBUG) fprintf(stdout, "before target_x = %f\n", target_x);
  if(DEBUG) fprintf(stdout, "before target_y = %f\n", target_y);

  /** Calc arf  **/
  status = hxd_arfUtil_calc_arf(unit_id, target_x, target_y, arf);
  if (status != HXDARFUTIL_STATUS_OK){
    fprintf(stderr,"%s_GSO: Calc arf failed \n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int hxdarfUtil_PIN( int pin_id,
		    double target_ra,
		    double target_dec,
		    HxdArfData *arf){
  int status = HXDARFUTIL_STATUS_OK;
  double target_x;
  double target_y;
  /* double satnom_theta; */
  /* double satnom_phi;   */
  /* double target_theta; */
  /* double target_phi;   */

  /** check status **/
  if (com.arfdb_data_type != HXDARFUTIL_ARFDB_TYPE_PIN){
    fprintf(stderr,"%s_PIN: Invalid Initialization.\n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  /** Calc difference between SatNominal and Target  **/
  status = hxdarfUtil_angle_target_satnom(target_ra, target_dec, 
					  pifile.ra_nom, pifile.dec_nom,
					  &target_x, &target_y);
  if (status != HXDARFUTIL_STATUS_OK){
    fprintf(stderr,"%s_PIN: Calc Angle failed \n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  if(DEBUG) fprintf(stdout, "before target_x = %f\n", target_x);
  if(DEBUG) fprintf(stdout, "before target_y = %f\n", target_y);

  /** just a wrapper of hxd_arfUtil_calc_arf() ***/
  status = hxd_arfUtil_calc_arf(pin_id, target_x, target_y, arf);
  if (status != HXDARFUTIL_STATUS_OK){
    fprintf(stderr,"%s_PIN: Calc arf failed \n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int hxdarfUtil_end( void ){
  int status = HXDARFUTIL_STATUS_OK;

  if (aste_coord_free(com.teldef) != ASTE_COORD_NORMAL_END){
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }
  /*
  closeAttFile(com.attfile);
  */

  return status;
}

/** ----------------------------------------------- **/
/***           static function, arf calc body      ***/
/** ----------------------------------------------- **/
static int
hxd_arfUtil_calc_arf(int det_id,
		     double target_x, double target_y,
		     HxdArfData *arf){
  int status = HXDARFUTIL_STATUS_OK;
  double hxdnom_x_UNIT, hxdnom_y_UNIT;
  double target_x_UNIT, target_y_UNIT;
  HxdArfAngle arfangle_target_prev, arfangle_target_after;
  HxdArfAngle arfangle_hxdnom_prev, arfangle_hxdnom_after;
  HxdArfAngle arfangle_target_x, arfangle_target_y;
  HxdArfAngle arfangle_hxdnom_x, arfangle_hxdnom_y;
  HxdArfAngle arfangle_x, arfangle_y;
  HxdArfData arf_x, arf_y;
  double focx = com.teldef->mission.aste->foc_xoff;
  double focy = com.teldef->mission.aste->foc_yoff;
  double scal  = com.teldef->mission.aste->det_scal;

  double intx;  /*float to double*/
  double inty;

  double merge_ratio = 0.0;

  if(DEBUG){
  fprintf(stdout, "Input teldef file: %s\n",com.teldef->filename);
  fprintf(stdout, "pin int x = %f\n",com.teldef->mission.aste->pin[det_id].intx);
  fprintf(stdout, "pin int y = %f\n",com.teldef->mission.aste->pin[det_id].inty);
  fprintf(stdout, "target_x = %f\n", target_x);
  fprintf(stdout, "target_y = %f\n", target_y);
  }

  if (com.arfdb_data_type == HXDARFUTIL_ARFDB_TYPE_PIN) { 
  intx = com.teldef->mission.aste->pin[det_id].intx;
  inty = com.teldef->mission.aste->pin[det_id].inty;
  }
  else if (com.arfdb_data_type == HXDARFUTIL_ARFDB_TYPE_GSO) { 
  intx = com.teldef->mission.aste->gso[det_id].intx;
  inty = com.teldef->mission.aste->gso[det_id].inty;
  }
  else{
    fprintf(stderr, "hxd_arfUtil_calc_arf: invalid det type\n");
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  /*
  intx = 0;
  inty = 0;
  */

  target_x_UNIT = - target_x + intx;
  target_y_UNIT = - target_y + inty;
  hxdnom_x_UNIT = 1.0*intx;
  hxdnom_y_UNIT = 1.0*inty;

  /*
  target_x_UNIT = target_x_UNIT - 3.5;
  target_y_UNIT = target_y_UNIT - 0.0;
  hxdnom_x_UNIT = hxdnom_x_UNIT - 3.5;
  hxdnom_y_UNIT = hxdnom_y_UNIT - 0.0;
  */


  target_x_UNIT = target_x_UNIT + focx * scal;
  target_y_UNIT = target_y_UNIT + focy * scal;
  /*
  hxdnom_x_UNIT = hxdnom_x_UNIT + focx * scal;
  hxdnom_y_UNIT = hxdnom_y_UNIT + focy * scal;
  */

  if(DEBUG) fprintf(stdout, "\n focx * scal = %f\n focy * scal = %f \n \n",
		    focx * scal, focy * scal);

  if(target_x_UNIT<0.0) target_x_UNIT = -1.0*target_x_UNIT;
  if(target_y_UNIT<0.0) target_y_UNIT = -1.0*target_y_UNIT;
  if(hxdnom_x_UNIT<0.0) hxdnom_x_UNIT = -1.0*hxdnom_x_UNIT;
  if(hxdnom_y_UNIT<0.0) hxdnom_y_UNIT = -1.0*hxdnom_y_UNIT;

  if(DEBUG){
    fprintf(stdout, "after target_x_UNIT = %f\n", target_x_UNIT);
    fprintf(stdout, "after target_y_UNIT = %f\n", target_y_UNIT);
    fprintf(stdout, "after hxdnom_x_UNIT = %f\n", hxdnom_x_UNIT);
    fprintf(stdout, "after hxdnom_y_UNIT = %f\n", hxdnom_y_UNIT);
   }

  /** calc arf (with arfdb) X-Axis **/
  hxdarfUtil_search_HxdArfdbUnit( target_x_UNIT, &com.arfdb_data,
				  &arfangle_target_prev, &arfangle_target_after);

  hxdarfUtil_interpolate_HxdArfAngle(target_x_UNIT,
				     &arfangle_target_prev, &arfangle_target_after,
				     &arfangle_target_x);

  hxdarfUtil_search_HxdArfdbUnit( hxdnom_x_UNIT, &com.arfdb_data,
				  &arfangle_hxdnom_prev, &arfangle_hxdnom_after);

  hxdarfUtil_interpolate_HxdArfAngle(hxdnom_x_UNIT,
				     &arfangle_hxdnom_prev, &arfangle_hxdnom_after,
				     &arfangle_hxdnom_x);

  hxdarfUtil_get_ratio_target_hxdnom(&arfangle_target_x, &arfangle_hxdnom_x, &arfangle_x);

  hxdarfUtil_extract_HxdArfData(&arfangle_x, &com.arfdb_data, &arf_x);

  /** calc arf (with arfdb) Y-Axis **/

  hxdarfUtil_search_HxdArfdbUnit( target_y_UNIT, &com.arfdb_data,
				  &arfangle_target_prev, &arfangle_target_after);

  hxdarfUtil_interpolate_HxdArfAngle(target_y_UNIT, 
				     &arfangle_target_prev, &arfangle_target_after,
				     &arfangle_target_y);

  hxdarfUtil_search_HxdArfdbUnit( hxdnom_y_UNIT, &com.arfdb_data,
				  &arfangle_hxdnom_prev, &arfangle_hxdnom_after);

  hxdarfUtil_interpolate_HxdArfAngle(hxdnom_y_UNIT, 
				     &arfangle_hxdnom_prev, &arfangle_hxdnom_after,
				     &arfangle_hxdnom_y);

  hxdarfUtil_get_ratio_target_hxdnom(&arfangle_target_y, &arfangle_hxdnom_y, &arfangle_y);

  hxdarfUtil_extract_HxdArfData(&arfangle_y, &com.arfdb_data, &arf_y);

  /** merge arf **/

  if(com.arfdb_data_type == HXDARFUTIL_ARFDB_TYPE_PIN){
    merge_ratio = 1.0 / HXDARFUTIL_UNIT_NUMBER_PIN;
  }
  else if(com.arfdb_data_type == HXDARFUTIL_ARFDB_TYPE_GSO){
    merge_ratio = 1.0 / HXDARFUTIL_UNIT_NUMBER_GSO;
  }


  hxdarfUtil_merge_HxdArfData(&arf_x, &arf_y, arf, merge_ratio);

  if(DEBUG) fprintf(stdout, "merge_ratio = %f", merge_ratio);

  if(DEBUG) fprintf(stdout, "\n Merged RSP[10] = %f\n \n", arf->specresp[10] /  HXDARFUTIL_GEOMETRICAL_AREA_PIN);

  return status;
}

/** ----------------------------------------------- **/
/***           static function utilities           ***/
/** ----------------------------------------------- **/
static void
hxdarfUtil_search_HxdArfdbUnit(double angle,                   /* input 1 */
			       HxdArfdbUnit *arfdb_data,       /* input 2 */
			       HxdArfAngle  *arfangle_ans_prev,/* output  */
			       HxdArfAngle  *arfangle_ans_after){
  int angle_id;
  int energy_bin;
  int angle_range = 0;

  /** search **/
  for(angle_id=0; angle_id <= (arfdb_data->n_angle-1); angle_id ++){
    if ( (arfdb_data->angle[angle_id] <= angle) && 
	 (angle <= arfdb_data->angle[angle_id+1])	 ){
      angle_range = 1;
      break;
    }
  }

  if(angle_range == 0 ) {
    fprintf(stderr, "Warning! Input (RA,DEC) is too far from the satellite attitude. \n");
  }

  if (DEBUG) fprintf(stderr, "found %3.2f arcmin = %3.2f -- %3.2f\n",
		     angle, arfdb_data->angle[angle_id],
		     arfdb_data->angle[angle_id+1]);
  /** copy **/
  arfangle_ans_prev->angle  = arfdb_data->angle[angle_id];
  arfangle_ans_after->angle = arfdb_data->angle[angle_id+1];
  arfangle_ans_prev->e_ch   = arfdb_data->n_energy_bin;
  arfangle_ans_after->e_ch  = arfdb_data->n_energy_bin;

  for (energy_bin=0; energy_bin<arfdb_data->n_energy_bin; energy_bin++ ){
    arfangle_ans_prev->specrsp[energy_bin] 
      = arfdb_data->specrsp[angle_id][energy_bin];
    arfangle_ans_after->specrsp[energy_bin] 
      = arfdb_data->specrsp[angle_id+1][energy_bin];
  }

}

static double
hxdarfUtil_interpolate_calc(double x0, double y0, double x1, double y1,
			    double x){
  double y = y0 + ( (x-x0)/(x1-x0) )*(y1-y0);
  return y;
}

static void
hxdarfUtil_interpolate_HxdArfAngle(double angle,               /* input 1 */
				   HxdArfAngle *arfangle_prev, /* input 2 */
				   HxdArfAngle *arfangle_after,/* input 3 */
				   HxdArfAngle *arfangle_ans){ /* output  */
  /* double angle_prev, angle_after; */
  int en;

  if (arfangle_prev->e_ch > MKDMYARF_MAX_NAXIS) {
    fprintf(stderr, "%s_interpolate_HxdArfAngle: e_ch (%d) is over %d\n",
	    tool_name, arfangle_prev->e_ch, MKDMYARF_MAX_NAXIS);
    return;
  }
  if (arfangle_prev->e_ch != arfangle_after->e_ch) {
    fprintf(stderr, "%s_interpolate_HxdArfAngle: invalid input\n", tool_name);
    return;
  }
  arfangle_ans->angle = angle;
  arfangle_ans->e_ch  = arfangle_prev->e_ch;

  for (en = 0; en <arfangle_ans->e_ch; en ++) {
    arfangle_ans->specrsp[en] 
      = hxdarfUtil_interpolate_calc(arfangle_prev->angle,
				    arfangle_prev->specrsp[en],
				    arfangle_after->angle,
				    arfangle_after->specrsp[en], angle);
  }
  return;
}

static void
hxdarfUtil_extract_HxdArfData(HxdArfAngle  *arfangle,      /* input 1 */
			      HxdArfdbUnit *arfdb_data,        /* input 2 */
			      HxdArfData *arf){                /* output  */
  int irow;

  for (irow = 0; irow <= arfangle->e_ch; irow ++ ){
    arf->row_num[irow]     = arfdb_data->energy_bin[irow];
    arf->energy_low[irow]  = arfdb_data->energy_low[irow];
    arf->energy_high[irow] = arfdb_data->energy_high[irow];
    arf->specresp[irow]    = arfangle->specrsp[irow];
  }

  arf->irow              = arfangle->e_ch;

}

void hxdarfUtil_clear_HxdArfData( HxdArfData *arf, int nrow){
  int irow;

  for (irow = 0; irow <= nrow; irow ++ ){
    arf->row_num[irow]     = irow;
    arf->energy_low[irow]  = 0.0;
    arf->energy_high[irow] = 0.0;
    arf->specresp[irow]    = 0.0;
  }

  arf->irow              = nrow;
}

void
hxdarfUtil_merge_HxdArfData(HxdArfData *arf_in_a,       /* input 1 */
			    HxdArfData *arf_in_b,       /* input 2 */
			    HxdArfData *arf_ans,        /* output  */
			    double ratio_a_b   ){
  int irow;
  double geom_area;

  if (com.arfdb_data_type == HXDARFUTIL_ARFDB_TYPE_PIN) {
   } else if (com.arfdb_data_type == HXDARFUTIL_ARFDB_TYPE_GSO) {
    geom_area = HXDARFUTIL_GEOMETRICAL_AREA_GSO;
  } else {
    fprintf(stderr, "hxdarfUtil_merge_HxdArfData: invalid det type\n");
    return;
  }

  for (irow = 0; irow <= arf_in_a->irow; irow ++ ){
    arf_ans->row_num[irow]     = arf_in_a->row_num[irow];
    arf_ans->energy_low[irow]  = arf_in_a->energy_low[irow];
    arf_ans->energy_high[irow] = arf_in_a->energy_high[irow];
    arf_ans->specresp[irow]    = ratio_a_b * 
      arf_in_a->specresp[irow] * arf_in_b->specresp[irow];
  }

  arf_ans->irow              = arf_in_a->irow;

  return;
}

void
hxdarfUtil_add_HxdArfData(HxdArfData *arf_in_a,       /* input 1 */
			  HxdArfData *arf_in_b,       /* input 2 */
			  HxdArfData *arf_ans,        /* output  */
			  double core_a, double core_b){
  int irow;

  for (irow = 0; irow <= arf_in_a->irow; irow ++ ){
    arf_ans->row_num[irow]     = arf_in_a->row_num[irow];
    arf_ans->energy_low[irow]  = arf_in_a->energy_low[irow];
    arf_ans->energy_high[irow] = arf_in_a->energy_high[irow];
    arf_ans->specresp[irow]    = core_a * arf_in_a->specresp[irow]
      + core_b * arf_in_b->specresp[irow];
  }

  arf_ans->irow              = arf_in_a->irow;
}

void
hxdarfUtil_copy_HxdArfData(HxdArfData *arf_in,       /* input 1 */
			   HxdArfData *arf_ans){     /* output  */
  int irow;

  for (irow = 0; irow <= arf_in->irow; irow ++ ){
    arf_ans->row_num[irow]     = arf_in->row_num[irow];
    arf_ans->energy_low[irow]  = arf_in->energy_low[irow];
    arf_ans->energy_high[irow] = arf_in->energy_high[irow];
    arf_ans->specresp[irow]    = arf_in->specresp[irow];
  }

  arf_ans->irow              = arf_in->irow;
}

int
hxdarfUtil_read_pifile( char *pi_file_name,
			hxdarfUtil_pifile *pifile){
  int status = HXDARFUTIL_STATUS_OK;
  int istat = 0;
  char comment[256];
  fitsfile *hxdarfUtil_pifile_fp;
  int detid;

  /*****  Open Fits ******/
  if (fits_open_file(&hxdarfUtil_pifile_fp, pi_file_name, READONLY, &istat)) {
    fprintf(stderr, "%s:fits_open_file %s failed (%d)\n", tool_name,
            pi_file_name, istat);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  /***** Read Fits *****/
  fits_read_key_dbl(hxdarfUtil_pifile_fp, "RA_OBJ", &pifile->ra_obj, comment,
		    &istat);
  if (istat) {
    fprintf(stderr, "%s:fits_readkey RA_OBJ failed (%d)\n", tool_name, istat);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdarfUtil_pifile_fp, "DEC_OBJ", &pifile->dec_obj, comment,
		    &istat);
  if (istat) {
    fprintf(stderr, "%s:fits_readkey DEC_OBJ failed (%d)\n", tool_name, istat);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  /*
  fits_read_key_dbl(hxdarfUtil_pifile_fp, "RA_PNT", &pifile->ra_pnt, comment, 
		    &istat);
  if (istat) {
    fprintf(stderr, "%s:fits_readkey RA_PNT failed (%d)\n", tool_name, istat);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdarfUtil_pifile_fp, "DEC_PNT", &pifile->dec_pnt, comment,
		    &istat);
  if (istat) {
    fprintf(stderr, "%s:fits_readkey DEC_PNT failed (%d)\n", tool_name, istat);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }
  */

  fits_read_key_dbl(hxdarfUtil_pifile_fp, "RA_NOM", &pifile->ra_nom, comment,
		    &istat);
  if (istat) {
    fprintf(stderr, "%s:fits_readkey RA_NOM failed (%d)\n", tool_name, istat);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdarfUtil_pifile_fp, "DEC_NOM", &pifile->dec_nom, comment,
		    &istat);
  if (istat) {
    fprintf(stderr, "%s:fits_readkey DEC_NOM failed (%d)\n", tool_name, istat);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  fits_read_key_dbl(hxdarfUtil_pifile_fp, "MEAN_EA3", &pifile->roll_nom,
		    comment, &istat);
  if (istat) {
    fprintf(stderr, "%s:fits_readkey MEAN_EA3 failed (%d)\n", tool_name, istat);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }
  /*
  fits_read_key_dbl(hxdarfUtil_pifile_fp, "ROLL_NOM", &pifile->roll_nom,
		    comment, &istat);
  if (istat) {
    fprintf(stderr, "%s:fits_readkey ROLL_NOM failed (%d)\n", tool_name, istat);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }
  /*

  /** read WMAP **/
  /****** temporal ******/
  for (detid=0; detid<HXDARFUTIL_WMAP_MAX_BIN; detid++)
    pifile->exposure_map[detid] = 1.0;

  if (DEBUG)
    fprintf(stderr, "PI RA,DEC OBJ(%f %f), PNT (%f %f) NOM (%f %f %f)\n",
	    pifile->ra_obj, pifile->dec_obj,
	    pifile->ra_pnt, pifile->dec_pnt,
	    pifile->ra_nom, pifile->dec_nom, pifile->roll_nom);

  return status;
}

int hxdarfUtil_conv_radec_detdeg( double ra, double dec,
				  double *det_x_deg,
				  double *det_y_deg,
				  hxdarfUtil_pifile *pifile){
  int status = HXDARFUTIL_STATUS_OK;
  double delta_ra, delta_dec;
  double rot_ang;

  delta_ra  = ra  - pifile->ra_pnt;
  delta_dec = dec - pifile->dec_pnt;

  /** Are these correct ?? Not used now **/
  rot_ang = pifile->roll_nom;
  *det_x_deg = -cos(rot_ang) * delta_ra + sin(rot_ang) * delta_dec;
  *det_y_deg =  sin(rot_ang) * delta_ra + cos(rot_ang) * delta_dec;

  if (DEBUG)
    fprintf(stderr, "RA, DEC(%f %f) - PNT(%f %f) = (%f %f) --> det (%f %f)\n",
	    ra, dec, pifile->ra_pnt, pifile->dec_pnt,
	    delta_ra, delta_dec, *det_x_deg, *det_y_deg);

  return status;
}

int hxdarfUtil_get_Exp_Ratio( int dettype, double *exp_ratio ){
  int status = HXDARFUTIL_STATUS_OK;
  int detid  = 0;
  int total_detnum = 0;
  double total_val = 0.0;

  /** PIN or GSO **/
  if (dettype == HXDARFUTIL_PIFILE_TYPE_GSO){
    total_detnum = 16;
  } else if (dettype == HXDARFUTIL_PIFILE_TYPE_PIN){
    total_detnum = 64;
  }

  /** calc total exposure **/
  for (detid=0; detid < total_detnum; detid ++){
    total_val += pifile.exposure_map[detid];
  }

  for (detid=0; detid < total_detnum; detid ++){
    exp_ratio[detid] = total_detnum * pifile.exposure_map[detid] / total_val;
    /* 2005.06.14 modified
       exp_ratio[detid] = pifile.exposure_map[detid] / total_val;
    */
  }

  return status;
}

static int 
hxdarfUtil_get_ratio_target_hxdnom(HxdArfAngle *angle_target,
				       HxdArfAngle *angle_nom,
				       HxdArfAngle *angle_out){
 
  int status = HXDARFUTIL_STATUS_OK;
  int en;
  for (en = 0; en <angle_target->e_ch; en++) {
    if(angle_nom->specrsp[en]==0.0){
      angle_out->specrsp[en]=0.0;
    }
    else{
      angle_out->specrsp[en] = angle_target->specrsp[en] / angle_nom->specrsp[en];
    }
  }

  if(DEBUG){
    fprintf(stdout, "RSP[10]: %f\n", angle_out->specrsp[10]);
  }

  /** copy **/
  angle_out->e_ch   = angle_target->e_ch;

  return status;
}


/* not correct */
int hxdarfUtil_conv_euler_detmin_x(double theta, double phi){

  double x_min;
  /** x_min = d(phi) * cos(theta) if d(theta) << 1 and d(phi) << 1  **/
  x_min = 60.0 * 180.0 / PI * phi * cos(theta*180.0/PI);
  return x_min;
}

int hxdarfUtil_conv_euler_detmin_y(double theta, double phi){

  double y_min;
  /** y_min = d(theta) * cos(phi) if d(theta) << 1 and d(phi) << 1  **/
  y_min = 60.0 * 180.0 / PI * theta * cos(phi*180.0/PI);
  return y_min;
}

static int 
hxdarfUtil_angle_target_satnom(double target_ra, double target_dec,
				   double satnom_ra, double satnom_dec,
				   double *x_min, double *y_min){

  int status = HXDARFUTIL_STATUS_OK;
  int tmp_status;
  /* int stat; */
  double target_focx_ch ;
  double target_focy_ch ;
  double satnom_focx_ch ;
  double satnom_focy_ch ;

  if(DEBUG) fprintf(stdout, "target_ra = %f, target_dec = %f \n",target_ra, target_dec);
  if(DEBUG) fprintf(stdout, "satnom_ra = %f, satnom_dec = %f \n",satnom_ra, satnom_dec);

  tmp_status = aste_ecs2foc(com.teldef, &com.eulerang, 
  			    target_ra, target_dec, &target_focx_ch, &target_focy_ch);

  tmp_status = aste_ecs2foc(com.teldef, &com.eulerang, 
			    satnom_ra, satnom_dec, &satnom_focx_ch, &satnom_focy_ch);

  if(DEBUG) fprintf(stdout, "target_focx_ch = %f, target_focy_ch = %f\n",
		    target_focx_ch, target_focy_ch);
  if(DEBUG) fprintf(stdout, "satnom_focx_ch = %f, satnom_focy_ch = %f\n",
		    satnom_focx_ch, satnom_focy_ch);

  *x_min = (satnom_focx_ch - target_focx_ch) * com.teldef->mission.aste->det_scal;
  *y_min = -1.0*(satnom_focy_ch - target_focy_ch) * com.teldef->mission.aste->det_scal;

  if(DEBUG) fprintf(stdout, "\n x_min = %f, y_min = %f\n \n", *x_min, *y_min);

  return status;
}


/************************************************************************/
/************ Functions just for tests. Should be deleted. **************/
/************************************************************************/

int hxdarfUtil_PIN_forPlot( int pin_id,
			    double target_ra,
			    double target_dec,
			    double satnom_ra,
			    double satnom_dec,
			    HxdArfData *arf){
  int status = HXDARFUTIL_STATUS_OK;
  double target_x;
  double target_y;

  /** check status **/
  if (com.arfdb_data_type != HXDARFUTIL_ARFDB_TYPE_PIN){
    fprintf(stderr,"%s_PIN: Invalid Initialization.\n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  /** Calc difference between SatNominal and Target  **/
  status = hxdarfUtil_angle_target_satnom(target_ra, target_dec,
					  satnom_ra, satnom_dec,
					  &target_x, &target_y);
  if (status != HXDARFUTIL_STATUS_OK){
    fprintf(stderr,"%s_PIN: Calc Angle failed \n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  if(DEBUG) fprintf(stdout, "before target_x = %f\n", target_x);
  if(DEBUG) fprintf(stdout, "before target_y = %f\n", target_y);

  /** just a wrapper of hxd_arfUtil_calc_arf() ***/
  status = hxd_arfUtil_calc_arf(pin_id, target_x, target_y, arf);
  if (status != HXDARFUTIL_STATUS_OK){
    fprintf(stderr,"%s_PIN: Calc arf failed \n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  return status;
}

int hxdarfUtil_init_forPlot(int  arfdbunit_type, /** PIN or GSO **/
			    char *arfdbunit_fits_fname,
			    char *teldef_fits_fname,
			    char *attitude_fits_fname,
			    char *pi_file_name,
			    double satnom_ra_in, double satnom_dec_in){
    int status = HXDARFUTIL_STATUS_OK;
    int stat;
    char *telescop = "SUZAKU";
    /*char *telescop = "Astro-E2";*/
    char *instrume = "HXD";

    /**==== read ARFDB  ====**/
    if ( arfdbunit_type == HXDARFUTIL_ARFDB_TYPE_PIN ||
	 arfdbunit_type == HXDARFUTIL_ARFDB_TYPE_GSO ) {
      com.arfdb_data_type = arfdbunit_type;
    } else {
      fprintf(stderr,"%s_init: Invalid arfdb type.\n", tool_name);
      com.arfdb_data_type = HXDARFUTIL_ARFDB_TYPE_UNDEF;
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }

    stat = hxdcaldbUtil_arfdbUnit_open_FITS(arfdbunit_fits_fname);
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: file open error\n", tool_name);
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }

    stat = hxdcaldbUtil_arfdbUnit_read_FITS(&com.arfdb_data);
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: read error\n", tool_name);
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }

    stat = hxdcaldbUtil_arfdbUnit_close_FITS();
    if (stat != HXDCALDBUTIL_STATUS_OK) {
      fprintf(stderr,"%s: close error\n", tool_name);
      status = HXDARFUTIL_STATUS_NG;
      return status;
    }


    com.teldef = aste_coord_init(telescop, instrume, teldef_fits_fname);
    if (com.teldef == NULL){
      fprintf(stderr,"%s: teldef init error \n", tool_name);
      status = HXDARFUTIL_STATUS_NG;
      return status;
  }


   /**=== read PI File ===**/
  stat = hxdarfUtil_read_pifile( pi_file_name, &pifile);
  if (stat != HXDCALDBUTIL_STATUS_OK) {
    fprintf(stderr,"%s: read pifile error\n", tool_name);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }

  /** === convert into Euler Angle === **/
  com.skyref.alpha = satnom_ra_in;
  com.skyref.delta = satnom_dec_in;
  com.skyref.roll  = 90.0 - pifile.roll_nom; /* roll angle = 90 - MEAN_EA3 */
  stat = aste_skyref2euler(com.teldef, &com.skyref, &com.eulerang);
  if (stat != ASTE_COORD_NORMAL_END){
    fprintf(stderr,"%s: astecood aste_skyref2euler failed(%d) \n",
	    tool_name, stat);
    status = HXDARFUTIL_STATUS_NG;
    return status;
  }
  if (DEBUG)
    fprintf(stderr, "%s: (RA,DEC,ROL)=(%f, %f, %f) --> Eul(%f, %f, %f)\n",
	    tool_name, pifile.ra_nom, pifile.dec_nom, pifile.roll_nom,
	    com.eulerang.phi, com.eulerang.theta, com.eulerang.psi);


  return status;
}
