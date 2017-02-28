#ifndef _HXD_EVENT_FITS_UTIL_H_
#define _HXD_EVENT_FITS_UTIL_H_

#include <fitsio.h>

#define HXD_EVENT_FITS_FORMAT_VERSION     2

#define HXD_EVENT_FITS_PRIMARY_HDU        1
#define HXD_EVENT_FITS_EVENT_EXTENTION    2
#define HXD_EVENT_FITS_GTI_EXTENTION      3

#define HXD_EVENT_FITS_KEY_NUM_V1 32
#define HXD_EVENT_FITS_KEY_NUM_V2 39
#define HXD_EVENT_FITS_KEY_NUM HXD_EVENT_FITS_KEY_NUM_V2

#define HXD_GRADE_NBIT  16
#define HXD_TRIG_NBIT    7
#define HXD_QFLAG_NBIT   7
#define HXD_HITWEL_NBIT 16
#define HXD_HITANT_NBIT 20
#define HXD_INT_TOTBIT  32
#define HXD_SHRT_TOTBIT 16
#define HXD_CHAR_TOTBIT  8

/*******************************************************************
 **   Format on Rev 0.x  Version 1.0, 1.1, 1.2 process
 *******************************************************************/
typedef struct {
    double time;
    double aetime;
    double s_time;
    int weltime;
    unsigned int ti;
    /* unsigned int eti[2]; */
    unsigned int mti;
    unsigned char trig;
    char  reserve01;
    short reserve02;
    unsigned char unitid;
    char  reserve03a;
    short reserve04a;
    unsigned char pin_id;
    char  reserve03b;
    short reserve04b;
    unsigned char length_chk;
    char  reserve05;
    short reserve06;
    unsigned int quality_flags;
/*  char  reserve07;
    short reserve08; */
    unsigned int hit_pattern_well;
/*  short reserve09; */
    unsigned int hit_pattern_anti;
    short pha_fast;
    short reserve10;
    short pha_slow;
    short reserve11;
    unsigned char pha_pin0;
    char  reserve12;
    short reserve13;
    unsigned char pha_pin1;
    char  reserve14;
    short reserve15;
    unsigned char pha_pin2;
    char  reserve16;
    short reserve17;
    unsigned char pha_pin3;
    char  reserve18;
    short reserve19;
    /** for 2nd fits **/
    double pi_fast;
    double pi_slow;
    double pi_pin0;
    char  reserve20;
    short reserve21;
    double pi_pin1;
    char  reserve22;
    short reserve23;
    double pi_pin2;
    char  reserve24;
    short reserve25;
    double pi_pin3;
    char  reserve26;
    short reserve27;
    double pi_pin;
    char  reserve28;
    short reserve29;
    short grade_qualty;
    short reserve30;
    unsigned char  grade_pmttrg;
    char  reserve31;
    short reserve32;
    unsigned char  grade_pintrg;
    char  reserve33;
    short reserve34;
/*  unsigned short grade_psdsel; */
    double grade_psdsel;
    short reserve35;
    unsigned int   grade_hitpat;
    unsigned int   grade_reserv;
    unsigned char det_type;
    char  reserve36;
    short reserve37;
} HxdEventFits01;
/** oldr name is HxdEventFits: to detect incompatibility at compile**/

/*******************************************************************
 **   Format on Version 2.0                                       **
 **   This Util only supports the following format after 0.6      **
 *******************************************************************/
typedef struct {
  double time;
  double aetime;
  double s_time;
  int weltime;
  unsigned int ti;
  unsigned int mti;
  unsigned char trig;
  char  reserve01;
  short reserve02;
  unsigned char unitid;
  char  reserve03;
  short reserve04;
  unsigned char pin_id;
  char  reserve05;
  short reserve06;
  unsigned char length_chk;
  char  reserve07;
  short reserve08;
  unsigned int quality_flags;
  unsigned int hit_pattern_well;
  unsigned int hit_pattern_anti;
  short pha_fast;
  short reserve09;
  short pha_slow;
  short reserve10;
  unsigned char pha_pin0;
  char  reserve11;
  short reserve12;
  unsigned char pha_pin1;
  char  reserve13;
  short reserve14;
  unsigned char pha_pin2;
  char  reserve15;
  short reserve16;
  unsigned char pha_pin3;
  char  reserve17;
  short reserve18;
  /** for 2nd fits **/
  int pi_fast;        /** changed v1 --> v2 **/
  int pi_slow;        /** changed v1 --> v2 **/
  int pi_pin0;        /** changed v1 --> v2 **/
  int pi_pin1;        /** changed v1 --> v2 **/
  int pi_pin2;        /** changed v1 --> v2 **/
  int pi_pin3;        /** changed v1 --> v2 **/
  int pi_pin;         /** changed v1 --> v2 **/
  short grade_qualty;
  short reserve19;
  unsigned char  grade_pmttrg;
  char  reserve20;
  short reserve21;
  unsigned char  grade_pintrg;
  char  reserve22;
  short reserve23;
  double grade_psdsel;
  unsigned int   grade_hitpat;
  unsigned int   grade_reserv;
  unsigned char det_type;
  char  reserve24;
  short reserve25;
  double upi_fast;    /** add v1 --> v2 **/
  double upi_slow;    /** add v1 --> v2 **/
  double upi_pin;     /** add v1 --> v2 **/
  double upi_pin0;    /** add v1 --> v2 **/
  double upi_pin1;    /** add v1 --> v2 **/
  double upi_pin2;    /** add v1 --> v2 **/
  double upi_pin3;    /** add v1 --> v2 **/
} HxdEventFits02;

#define HXD_WELTIME_CLKRATE_NORM    0
#define HXD_WELTIME_CLKRATE_FINE    1
#define HXD_WELTIME_CLKRATE_SFINE_A 2
#define HXD_WELTIME_CLKRATE_SFINE_B 3

void hxdeventFits_create_tbl( fitsfile *fp, int *istat );

void hxdeventFits_col_write( fitsfile *fp, long irow, int *colnum,
			     HxdEventFits02 *fits, int *istat );

void hxdeventFits_col_read( fitsfile *fp, long irow, int *colnum,
			    HxdEventFits02 *fits, int *istat );

void hxdeventFits_col_num( fitsfile *fp, int *colnum, int *istat );

void hxdeventFits_add_tlminmax( fitsfile *fp, int *istat );

void hxdeventFits_add_comment( fitsfile *fp, int *istat );

void hxdeventFits_modify_weltime_unit(fitsfile *fp, int time_mode, int *istat);

void hxdeventFits_add_formatversion(fitsfile *fp, int *istat);

void hxdeventFits_add_clkrate( fitsfile *fp, int *istat );

#endif
