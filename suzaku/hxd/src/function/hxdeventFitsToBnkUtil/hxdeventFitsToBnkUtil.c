/*
   v0.0.1 test version created by M. sugiho
   v0.0.2 M.Sugiho
   v0.0.3 add DET_TYPE by Y.Terada
   v0.1.0 add MTI      by Y.Terada
   v0.1.1 add ETI      by Y.Terada, 
   v0.1.2 debug        by Y.Terada, 2004-05-02
   v0.1.3 debug        by Y.Terada, 2004-06-01
   v0.2.0 Bnk->Fits    by Y.Terada, 2004-06-01
   v0.3.0 new Grade    by Y.Terada, 2005-01-15
   v0.3.1 delete ETI   
          add PIN_ID   by Y.Terada, 2005-05-17
   v0.3.2 aetime and s_time 
          input aetime to TIME,  by Y.Terada, 2005-05-24
   v0.3.3 double PI_   by Y.Terada  2005-05-26
          change Bnk Name of PI_PIN
   v0.3.4 change format of GRADE_PSDSEL  by Y.Terada 2005-06-11
   v0.3.5 format check, HIT_PATTERN      by Y.Terada 2005-12-02
   ---> v1.0.0
   v2.0.0 Format for Version 2.0 process by Y.Terada 2006-09-08
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"
#include "fitsio.h"

#include "hxdeventFitsUtil.h"
#include "hxdeventFitsToBnkUtil.h"

static char *pname = "hxdeventFitsToBnkUtil";

void hxdeventFitsToBnk_init(){
  
  BnkDef( "HXD:WEL:EV_TIME", sizeof(double) );
  BnkDef( "HXD:WEL:MTI", sizeof(int) );
  BnkDef( "HXD:WEL:GRADE_QUALTY", sizeof(int) );
  BnkDef( "HXD:WEL:GRADE_PMTTRG", sizeof(int) );
  BnkDef( "HXD:WEL:GRADE_PINTRG", sizeof(int) );
  BnkDef( "HXD:WEL:GRADE_PSDSEL", sizeof(double) );
  BnkDef( "HXD:WEL:GRADE_HITPAT", sizeof(int) );
  BnkDef( "HXD:WEL:GRADE_RESERV", sizeof(int) );
  BnkDef( "HXD:WEL:GRADE_PI_PIN", sizeof(int));     /** v2.0 **/
  BnkDef( "HXD:WEL:GRADE_UPI_PIN", sizeof(double)); /** v2.0 **/
  BnkDef( "HXD:WEL:DET_TYPE", sizeof(int) );
  BnkDef( "HXD:WEL:PI_FAST", sizeof(int) );         /** v2.0 **/
  BnkDef( "HXD:WEL:PI_SLOW", sizeof(int) );         /** v2.0 **/
  BnkDef( "HXD:WEL:PI_PIN", sizeof(int)*4 );        /** v2.0 **/
  BnkDef( "HXD:WEL:UPI_FAST", sizeof(double) );     /** v2.0 **/
  BnkDef( "HXD:WEL:UPI_SLOW", sizeof(double) );     /** v2.0 **/
  BnkDef( "HXD:WEL:UPI_PIN", sizeof(double)*4 );    /** v2.0 **/
  BnkDef( "HXD:WEL:PIN_ID", sizeof(int)   );
  
  BnkDef( "HXD:WEL:UNITID", sizeof(int) );
  BnkDef( "HXD:WEL:LENGTH_CHK", sizeof(int) );
  BnkDef( "HXD:WEL:WELTIME", sizeof(int) );
  BnkDef( "HXD:WEL:QUALITY_FLAGS", sizeof(int) );
  BnkDef( "HXD:WEL:TRIG", sizeof(int) );
  BnkDef( "HXD:WEL:HIT_PATTERN_WELL", sizeof(int) );
  BnkDef( "HXD:WEL:HIT_PATTERN_ANTI", sizeof(int) );
  BnkDef( "HXD:WEL:PHA_FAST", sizeof(int) );
  BnkDef( "HXD:WEL:PHA_SLOW", sizeof(int) );
  BnkDef( "HXD:WEL:PHA_PIN", sizeof(int)*4 );
  
  BnkDef( "HXD:WEL:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:WEL:PACKET_S_TIME", sizeof(double) );
  BnkDef( "HXD:WEL:PACKET_SEC_HEADER", sizeof(int) );
 
}

void hxdeventFitsToBnk_put( HxdEventFits02 *fits ){
  
  int int_trig;
  int int_unitid;
  int int_pin_id;
  int int_grade_qualty;
  int int_grade_pmttrg;
  int int_grade_pintrg;
/*int int_grade_psdsel;*/
  int int_grade_hitpat;
  int int_grade_reserv;
  int int_det_type;
  int int_length_chk;
  int int_pha_fast;
  int int_pha_slow;
  int int_pha_pin[4];
  int pi_pin[4];
  double upi_pin[4];
  
  int_trig = (int)fits->trig;    
  int_unitid = (int)fits->unitid;    
  int_pin_id = (int)fits->pin_id;    
  int_grade_qualty = (int)fits->grade_qualty;
  int_grade_pmttrg = (int)fits->grade_pmttrg;
  int_grade_pintrg = (int)fits->grade_pintrg;
/*int_grade_psdsel = (int)fits->grade_psdsel; */
  int_grade_hitpat = (int)fits->grade_hitpat;
  int_grade_reserv = (int)fits->grade_reserv;
  int_det_type = (int)fits->det_type;
  int_length_chk = (int)fits->length_chk;
/*int_quality_flags = (int)fits->quality_flags;*/
/*int_pi_fast = (int)fits->pi_fast;
  int_pi_slow = (int)fits->pi_slow; */
  int_pha_fast = (int)fits->pha_fast;
  int_pha_slow = (int)fits->pha_slow;
/*int_pi_pin[0] = (int)fits->pi_pin0;*/
  int_pha_pin[0] = (int)fits->pha_pin0;
/*int_pi_pin[1] = (int)fits->pi_pin1; */
  int_pha_pin[1] = (int)fits->pha_pin1;
/*int_pi_pin[2] = (int)fits->pi_pin2; */
  int_pha_pin[2] = (int)fits->pha_pin2;
/*int_pi_pin[3] = (int)fits->pi_pin3;*/
  int_pha_pin[3] = (int)fits->pha_pin3;
/*int_hit_pattern_well = (int) fits->hit_pattern_well;
  int_hit_pattern_anti = (int) fits->hit_pattern_anti; */

  pi_pin[0] = fits->pi_pin0;
  pi_pin[1] = fits->pi_pin1;
  pi_pin[2] = fits->pi_pin2;
  pi_pin[3] = fits->pi_pin3;
  upi_pin[0] = fits->upi_pin0;
  upi_pin[1] = fits->upi_pin1;
  upi_pin[2] = fits->upi_pin2;
  upi_pin[3] = fits->upi_pin3;

/* BnkfPutM( "HXD:WEL:ETI",  sizeof(int)*2, fits->eti ); */
  BnkfPutM( "HXD:WEL:EV_TIME", sizeof(double), &fits->time );
  BnkfPutM( "HXD:WEL:MTI", sizeof(int), &fits->mti );
  BnkfPutM( "HXD:WEL:GRADE_QUALTY", sizeof(int), &int_grade_qualty );
  BnkfPutM( "HXD:WEL:GRADE_PMTTRG", sizeof(int), &int_grade_pmttrg );
  BnkfPutM( "HXD:WEL:GRADE_PINTRG", sizeof(int), &int_grade_pintrg );
  BnkfPutM( "HXD:WEL:GRADE_PSDSEL", sizeof(double), &fits->grade_psdsel );
  BnkfPutM( "HXD:WEL:GRADE_HITPAT", sizeof(int), &int_grade_hitpat );
  BnkfPutM( "HXD:WEL:GRADE_RESERV", sizeof(int), &int_grade_reserv );
  BnkfPutM( "HXD:WEL:GRADE_PI_PIN", sizeof(int), &fits->pi_pin );
  BnkfPutM( "HXD:WEL:GRADE_UPI_PIN", sizeof(double), &fits->upi_pin );
  BnkfPutM( "HXD:WEL:DET_TYPE", sizeof(int), &int_det_type );
  BnkfPutM( "HXD:WEL:PI_FAST", sizeof(int), &fits->pi_fast );
  BnkfPutM( "HXD:WEL:PI_SLOW", sizeof(int), &fits->pi_slow );
  BnkfPutM( "HXD:WEL:PI_PIN", sizeof(int)*4, &pi_pin[0] );
  BnkfPutM( "HXD:WEL:UPI_FAST", sizeof(double), &fits->upi_fast );
  BnkfPutM( "HXD:WEL:UPI_SLOW", sizeof(double), &fits->upi_slow );
  BnkfPutM( "HXD:WEL:UPI_PIN", sizeof(double)*4, &upi_pin[0] );
  BnkfPutM( "HXD:WEL:PIN_ID", sizeof(int), &int_pin_id );
  
  BnkfPutM( "HXD:WEL:UNITID", sizeof(int), &int_unitid );
  BnkfPutM( "HXD:WEL:LENGTH_CHK", sizeof(int), &int_length_chk );
  BnkfPutM( "HXD:WEL:WELTIME", sizeof(int), &fits->weltime );
  BnkfPutM( "HXD:WEL:QUALITY_FLAGS", sizeof(int), &fits->quality_flags );
  BnkfPutM( "HXD:WEL:TRIG", sizeof(int), &int_trig );
  BnkfPutM( "HXD:WEL:HIT_PATTERN_WELL", sizeof(int), &fits->hit_pattern_well); 
  BnkfPutM( "HXD:WEL:HIT_PATTERN_ANTI", sizeof(int), &fits->hit_pattern_anti);
  BnkfPutM( "HXD:WEL:PHA_FAST", sizeof(int), &int_pha_fast );
  BnkfPutM( "HXD:WEL:PHA_SLOW", sizeof(int), &int_pha_slow );
  BnkfPutM( "HXD:WEL:PHA_PIN", sizeof(int)*4, &int_pha_pin[0] );
  
  BnkfPutM( "HXD:WEL:PACKET_AETIME", sizeof(double), &fits->aetime);
  BnkfPutM( "HXD:WEL:PACKET_S_TIME", sizeof(double), &fits->s_time);
  BnkfPutM( "HXD:WEL:PACKET_SEC_HEADER", sizeof(int), &fits->ti );  
}

void hxdeventFitsToBnk_get(int board, HxdEventFits02 *fits ){
  int size;
  char data[HXD_WEL_LENGTH];


  switch(board){
  case 0:
    BnkGet( "HXD:WE0:ALL", sizeof(char)*HXD_WEL_LENGTH, &size, data );
    BnkfGetM("HXD:WE0:PACKET_S_TIME", sizeof(double), &size, &fits->s_time);
    break;        
  case 1:
    BnkGet( "HXD:WE1:ALL", sizeof(char)*HXD_WEL_LENGTH, &size, data );
    BnkfGetM("HXD:WE1:PACKET_S_TIME", sizeof(double), &size, &fits->s_time);
    break;        
  case 2:
    BnkGet( "HXD:WE2:ALL", sizeof(char)*HXD_WEL_LENGTH, &size, data );
    BnkfGetM("HXD:WE2:PACKET_S_TIME", sizeof(double), &size, &fits->s_time);
    break;
  case 3:
    BnkGet( "HXD:WE3:ALL", sizeof(char)*HXD_WEL_LENGTH, &size, data );
    BnkfGetM("HXD:WE3:PACKET_S_TIME", sizeof(double), &size, &fits->s_time);
    break;
  }
  
  BnkfGetM("HXD:ALL:PACKET_SEC_HEADER", sizeof(int), &size, &fits->ti);
/*  BnkfGetM("HXD:ALL:ETI", sizeof(int)*2, &size, fits->eti); */

  fits->unitid  = ( board << 2 ) + ((data[3] & 0xC0) >> 6);
  fits->length_chk = ((data[0] &0x80) >> 7);
  fits->weltime = ((data[0] & 0x7F )<<12) + ((data[1] & 0xFF )<< 4)
    + ((data[2] & 0xF0 )>> 4);
  fits->quality_flags = ((data[2] & 0x07) << 4) + ((data[4] & 0xF0) >> 4);
  fits->trig = ((data[2] & 0x08) << 3) + (data[3] & 0x3F);
  fits->hit_pattern_well = ((data[4] & 0x0F) << 12) + ((data[5] & 0xFF) << 4)
    + ((data[6] & 0xF0) >> 4);
  fits->hit_pattern_anti = ((data[6] & 0x0F) << 16) + ((data[7] & 0xFF) << 8)
    + (data[8] & 0xFF);    
  fits->pha_fast = ((data[9] & 0xFF) << 4) + ((data[10] & 0xF0) >> 4);
  fits->pha_slow = ((data[10] & 0x0F) << 8) + (data[11] & 0xFF);
  fits->pha_pin0 = (data[12] & 0xFF);
  fits->pha_pin1 = (data[13] & 0xFF);
  fits->pha_pin2 = (data[14] & 0xFF);
  fits->pha_pin3 = (data[15] & 0xFF);

  /********* FFF default value *********/
  fits->mti = fits->ti;
  fits->time = 0.0;     /** mk1stfits **/
  fits->aetime = fits->s_time;
  fits->grade_qualty = 255;
  fits->grade_pmttrg = 255;
  fits->grade_pintrg = 255;
  fits->grade_psdsel = 255.0;
  fits->grade_hitpat = 255;
  fits->grade_reserv = 255;
  fits->det_type = 255; /* 255 = NOT_DEFINED */
  fits->pin_id   = 255; /* 255 = NOT_DEFINED */
  fits->pi_fast = 0;
  fits->pi_slow = 0;
  fits->pi_pin0 = 0;
  fits->pi_pin1 = 0;
  fits->pi_pin2 = 0;
  fits->pi_pin3 = 0;
  fits->pi_pin  = 0; 
  fits->upi_fast = 0.0;
  fits->upi_slow = 0.0;
  fits->upi_pin0 = 0.0;
  fits->upi_pin1 = 0.0;
  fits->upi_pin2 = 0.0;
  fits->upi_pin3 = 0.0;
  fits->upi_pin  = 0.0; 
}






