/*
   HXDgrade renewal v0.1.0
              Kitaguchi Takao 2005-01-20
   v0.1.1     by Y.Terada,    2005-02-07
   v0.1.4     by T.Kitaguchi, 2005-05-17   BnkPut pin_id
   v0.1.5     by Y.Terada,    2005-05-26   change BNK name for PI_PIN
   v0.1.6     by T.Kitaguchi, 2005-05-26   support psdsel()
   v0.1.7     by Y.Terada,    2005-06-11   double PSDSEL
   v0.1.8     by T.Kitaguchi, 2005-06-14   change in ( ) of HXDgrade_ana( )
   v0.1.9     by T.Kitaguchi, 2005-08-30   support PIN PI threshold
   v0.2.0     by Y.Terada,    2005-11-04   put PIL parameters
   v0.2.1     by Y.Terada,    2005-11-08   shorten Bnk name
   v0.2.2     by T.Kitaguchi, 2006-05-01   correct error messages
   v0.2.3     by T.Kitaguchi, 2006-05-01   input PSDSEL criteria with PIL
   v2.0.0     by Y.Terada,    2006-09-10   new format ver2
   v2.0.1     by T.Kitaguchi, 2006-09-13   add DET_TYPE = 2 (Pseudo Event)
   v2.0.2     by Y.Terada,    2007-04-27   support CALDB access function
   v2.0.3     by T.Kitaguchi, 2007-06-22   move hxdgradeUtil_init func
                                           from HXDgrade_init to HXDgrade_bgnrun
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/*#include <pfile.h>*/

#include "anl.h"
#include "bnk.h"

#include "hxdgradeUtil.h"

#define FILELIST_MAX_LENGTH 256

char HXDgrade_version[] = "version 2.0.3";

static char pname[] = "HXDgrade";

void HXDgrade_startup( int *status ) {
  int used = 1;
  BnkPut("HXD:ftools:hxdgrade_yn",      sizeof(int), &used);

  *status = ANL_OK;
}

void HXDgrade_com( int *status ) {
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

void HXDgrade_init( int *status ) {
  *status = ANL_OK;
}

void HXDgrade_his( int *status ) {
  *status = ANL_OK;
}

void HXDgrade_bgnrun( int *status ) {
  int stat, size, gsooldpi;
  char hxdgrade_psdsel_fname[FILELIST_MAX_LENGTH];
  char hxdgrade_pinthres_fname[FILELIST_MAX_LENGTH];
  char psdsel_type[71];
  double hxdgrade_psdsel_criteria;

  BnkfGetM("HXDgrade:PSDSEL_FILE_NAME", sizeof(hxdgrade_psdsel_fname),
           &size, hxdgrade_psdsel_fname);

  BnkfGetM("HXDgrade:PINTHRES_FILE_NAME", sizeof(hxdgrade_pinthres_fname),
           &size, hxdgrade_pinthres_fname);

  BnkfGetM("HXDgrade:PSDSEL_CRITERIA", sizeof(hxdgrade_psdsel_criteria),
           &size, &hxdgrade_psdsel_criteria);

  BnkfGetM("HXDeventFitsRead:GSOOLDPI", sizeof(int), &size, &gsooldpi);

  if ( gsooldpi ) {
    strncpy( psdsel_type, "PSDSEL", 7 );
  } else {
    strncpy( psdsel_type, "PSDSEL2", 8 );
  }
  stat = hxdgradeUtil_init(hxdgrade_pinthres_fname, hxdgrade_psdsel_fname,
			   &hxdgrade_psdsel_criteria, psdsel_type);
  if ( stat != HXDGRADEUTIL_OK ) {
    fprintf(stderr, "%s: Init failed\n", pname);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

void HXDgrade_ana( int nevent, int eventid, int *status ) {
  int stat;
  int size;
  HxdEventFits02 event;

  int    grade_qualty;
  int    grade_pmttrg;
  int    grade_pintrg;
  int    grade_psetrg; /* add v2.0.1 */
  double grade_psdsel;
  int    grade_hitpat;
  int    grade_reserv;

  int    det_type;
  int    pin_id;
  int    pi_pin;
  double upi_pin;

  /** need to be listed in caldb?? **/
  /*
  double pin_ld_threshold[HXD_GRADE_PIN_UNIT_NUM] = {
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
  };
  */

  BnkfGetM( "HXD:WEL:EVENT", sizeof(HxdEventFits02), &size, &event );


  /* ------------------------
   * Calculate GRADE
   * ------------------------*/
  stat = hxdgradeUtil_calc_qualty( &event, &grade_qualty );
  if (stat != HXDGRADEUTIL_OK) {
    fprintf(stderr, "%s: calc QUALITY failed\n", pname);
    *status = ANL_QUIT;
    return;
  }

  /* stat = hxdgradeUtil_calc_pmttrg( &event, pin_ld_threshold,
                                      &grade_pmttrg );*/
  stat = hxdgradeUtil_calc_pmttrg( &event, &grade_pmttrg );
  if (stat != HXDGRADEUTIL_OK) {
    fprintf(stderr, "%s: calc PMTRG failed\n", pname);
    *status = ANL_QUIT;
    return;
  }

  stat = hxdgradeUtil_calc_pintrg( &event, &pin_id, &grade_pintrg );
  if (stat != HXDGRADEUTIL_OK) {
    fprintf(stderr, "%s: calc PINTRG failed\n", pname);
    *status = ANL_QUIT;
    return;
  }

  stat = hxdgradeUtil_calc_psetrg( &event, &grade_psetrg );
  if (stat != HXDGRADEUTIL_OK) {
    fprintf(stderr, "%s: calc PSETRG failed\n", pname);
    *status = ANL_QUIT;
    return;
  }

  stat = hxdgradeUtil_calc_psdsel( &event, &grade_psdsel );
  if (stat != HXDGRADEUTIL_OK) {
    fprintf(stderr, "%s: calc PSDSEL failed\n", pname);
    *status = ANL_QUIT;
    return;
  }

  stat = hxdgradeUtil_calc_hitpat( &event, &grade_hitpat );
  if (stat != HXDGRADEUTIL_OK) {
    fprintf(stderr, "%s: calc HITPAT failed\n", pname);
    *status = ANL_QUIT;
    return;
  }

  grade_reserv = 255;

  /* ------------------------
   * Calculate DET_TYPE
   * ------------------------*/
  stat = hxdgradeUtil_calc_dettype( grade_qualty, grade_pmttrg, grade_pintrg,
				    grade_psetrg, grade_psdsel, grade_hitpat,
				    grade_reserv,
				    &det_type );
  if (stat != HXDGRADEUTIL_OK) {
    fprintf(stderr, "%s: calc DET_TYPE failed\n", pname);
    *status = ANL_QUIT;
    return;
  }


  /* ------------------------
   * Select PI_PIN
   * ------------------------*/
  stat = hxdgradeUtil_copy_pipin( &event, det_type, pin_id, &upi_pin, &pi_pin);
  if (stat != HXDGRADEUTIL_OK) {
    fprintf(stderr, "%s: calc PI_PIN failed\n", pname);
    *status = ANL_QUIT;
    return;
  }


  /* ------------------------
   *   Bnk Put
   * ------------------------*/
  BnkfPutM( "HXD:WEL:GRADE_QUALTY", sizeof(int),    &grade_qualty );
  BnkfPutM( "HXD:WEL:GRADE_PMTTRG", sizeof(int),    &grade_pmttrg );
  BnkfPutM( "HXD:WEL:GRADE_PINTRG", sizeof(int),    &grade_pintrg );
  BnkfPutM( "HXD:WEL:GRADE_PSDSEL", sizeof(double), &grade_psdsel );
  BnkfPutM( "HXD:WEL:GRADE_HITPAT", sizeof(int),    &grade_hitpat );
  BnkfPutM( "HXD:WEL:GRADE_RESERV", sizeof(int),    &grade_reserv );
  BnkfPutM( "HXD:WEL:DET_TYPE",     sizeof(int),    &det_type );
  BnkfPutM( "HXD:WEL:GRADE_PI_PIN", sizeof(int),    &pi_pin );
  BnkfPutM( "HXD:WEL:GRADE_UPI_PIN",sizeof(double), &upi_pin );
  BnkfPutM( "HXD:WEL:PIN_ID",       sizeof(int),    &pin_id );

  /* --------------------------------
   *   Write back to event structure
   * --------------------------------*/
  stat = hxdgradeUtil_modify_eventdata( grade_qualty, grade_pmttrg,
					grade_pintrg, grade_psdsel,
					grade_hitpat, grade_reserv,
					det_type, upi_pin, pi_pin,
					pin_id, &event );
  if (stat != HXDGRADEUTIL_OK) {
    fprintf(stderr, "%s: write GRADE failed\n", pname);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;

}

void HXDgrade_endrun( int *status ) {
  int stat;
  stat = hxdgradeUtil_endrun();
  if (stat != HXDGRADEUTIL_OK){
    fprintf(stderr, "%s: Init failed\n", pname);
    *status = ANL_QUIT;
    return;
  }
  *status = ANL_OK;
}

void HXDgrade_exit( int *status ) {
  *status = ANL_OK;
}
