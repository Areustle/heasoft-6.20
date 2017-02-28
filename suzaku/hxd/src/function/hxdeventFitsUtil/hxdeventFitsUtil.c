/*
   v0.0.1 test version created by M. sugiho
   v0.0.2 support hxd_eventFITS_col_write by M. sugiho
   v0.0.5 for HXDeventFitsWrite 0.7.4     by M. sugiho 
              HXDeventFitsRead 0.2.4
	      HXD2ndeventFitsWrite 0.1.5
   v0.0.9 add DET_TYPE and PI_PIN column 1999-12-11 by Y.Terada
   v0.1.0 GSFC version 1999-12-24
   v0.1.1 change PI format (4096ch -> 1024ch)       by Y.Terada
   v0.1.2 change HIT_PATTERN_WELL, HIT_PATTERN_ANTI format
          change TRIG, QUALITY_FLAGS format
          change PI format (4096ch -> 800 ch)
	  Add GRADE_A,GRADE_B,GRADE_C,GRADE_D       by Y.Terada
   v0.2.0 add MTI                                   by Y.Terada
   v0.2.2 debug thanks to Y Ishisaki 2003-07023
   v0.2.3 MTI to ETI, change order  by Y.Terada, 2003-12-20
   v0.2.4 debug fits_write_col_bit, larray (8-bit) 
          change format (length_ok = 1X)
          add comments on QUALITY_FLAGS
                            by H.Takahashi, Y.Terada, 2003-12-22
   v0.2.6 change ETI format by Y.Terada, 2004-03-12
   v0.2.7 debug  ETI format by Y.Terada, 2004-04-27
   v0.2.8 read nX format    by Y.Terada, 2004-06-01
   v0.2.9 read swap fits comment 'Slow<->Fast' by M.Suzuki, 2004-09-07
   v0.3.0 Pre-Flight Format, change Grade      by Y.Terada, 2005-01-14
   v0.3.1 Add comments, PI / Grade definitions by Y.Terada, 2005-02-07
   v0.3.2 change TLMAX/TLMIN,                  by Y.Terada, 2005-02-14
   v0.4.0 change AETIME to S_TIME              by Y.Terada, 2005-05-04
   v0.4.1 detele ETI, only TI                  by Y.Terada, 2005-05-17
          add PIN_ID (0,1,2,3)
   v0.4.2 debug TLMAX/TLMIN                    by Y.Terada, 2005-05-18
   v0.4.3 change PI_*** format, Int --> Dbl    by Y.Terada, 2005-05-19
   v0.4.4 debug in TLMAX/TLMIN                 by Y.Terada, 2005-05-24
   v0.4.5 aetime and s_time                    by Y.Terada, 2005-05-24
   v0.4.6 double cast in PI_***  by Y.Terada and S.Hirakuri 2005-05-26
   v0.4.7 PSD_OUT comment change            by T. Kitaguchi 2005-06-11
   v0.4.8 change format, double GRADE_PSDSEL   by Y.Terada, 2005-06-11
   v0.4.9 WELTIME unit                         by Y.Terada, 2005-06-13
   v0.5.0 update comments                      by Y.Terada, 2005-10-12
   v0.5.1 GRADE_xxx inconsistent I/O, reported by T.Kitaguchi,
                                      reviced  by Y.Terada, 2005-11-05
   v0.5.2 type check (I/O)                     by Y.Terada, 2005-11-08
   v0.5.3 type check (I/O)                     by Y.Terada, 2005-12-02
   v2.0.0 New format for Version 2.0 process   by Y.Terada, 2006-09-08
   v2.0.1 add CLK_RATE                         by Y.Terada, 2006-09-10
   v2.0.2 add DETTYPE=2, expand range GRADE_PSDSEL
                                               by Y.Terada, 2006-09-13
   v2.0.3 debug TLMAX
   v2.0.4 change GRADE_PINTRG comment       by T.Kitaguchi, 2007-04-10
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"
#include "fitsio.h"
#include "hxdeventFitsUtil.h"
#include "hxdFitsCommentUtil.h"

static char *pname = "hxdeventFitsUtil";

enum{
  TIME, S_TIME, WELTIME, TI, MTI, 
  LENGTH_CHK, UNITID, DET_TYPE, PIN_ID, 
  GRADE_QUALTY, GRADE_PMTTRG, GRADE_PINTRG, 
  GRADE_PSDSEL,GRADE_HITPAT, GRADE_RESERV,
  TRIG,  QUALITY_FLAGS,
  HIT_PATTERN_WELL, HIT_PATTERN_ANTI,
  PI_FAST,  PI_SLOW,  PI_PIN,  PI_PIN0,  PI_PIN1,  PI_PIN2,  PI_PIN3,
  PHA_FAST, PHA_SLOW,          PHA_PIN0, PHA_PIN1, PHA_PIN2, PHA_PIN3, 
  UPI_FAST, UPI_SLOW, UPI_PIN, UPI_PIN0, UPI_PIN1, UPI_PIN2, UPI_PIN3
};

static char *hxd_event_fits_keyword[HXD_EVENT_FITS_KEY_NUM] = {
  "TIME", "S_TIME", "WELTIME", "TI", "MTI", 
  "LENGTH_CHK", "UNITID",  "DET_TYPE", "PIN_ID",
  "GRADE_QUALTY", "GRADE_PMTTRG", "GRADE_PINTRG", 
  "GRADE_PSDSEL", "GRADE_HITPAT", "GRADE_RESERV",
  "TRIG", "QUALITY_FLAGS",
  "HIT_PATTERN_WELL", "HIT_PATTERN_ANTI",
  "PI_FAST", "PI_SLOW", "PI_PIN", "PI_PIN0", "PI_PIN1", "PI_PIN2", "PI_PIN3",
  "PHA_FAST", "PHA_SLOW", 
  "PHA_PIN0", "PHA_PIN1", "PHA_PIN2", "PHA_PIN3", 
  "UPI_FAST", "UPI_SLOW", 
  "UPI_PIN", "UPI_PIN0", "UPI_PIN1", "UPI_PIN2", "UPI_PIN3"
};

static char *hxd_event_fits_format[HXD_EVENT_FITS_KEY_NUM] = {
  "1D", "1D", "1J", "1V", "1V",
  "1X", "1B", "1B", "1B",
  "1I", "1B", "1B", "1D", "1V", "1V",
  "7X", "7X",
  "16X", "20X",
  "1I", "1I", "1B", "1B", "1B", "1B", "1B", 
  "1I", "1I", "1B", "1B", "1B", "1B",
  "1D", "1D", "1D", "1D", "1D", "1D", "1D"
};

static char *hxd_event_fits_unit[HXD_EVENT_FITS_KEY_NUM] = {
  "s", "s", "", "1/4096 s", "1/4096 s",
  "", "", "",  "", "", "", "", "",
  "", "", "", "", 
  "", "",
  "chan", "chan", "chan", "chan", "chan", "chan", "chan",
  "chan", "chan", "chan", "chan", "chan", "chan",
  "chan", "chan", "chan", "chan", "chan", "chan", "chan"
};

void hxdeventFits_create_tbl( fitsfile *fp, int *istat ){
  
  long naxis = 0;
  
  static char extention_name[]="EVENTS";
  
  fits_create_tbl( fp, BINARY_TBL, naxis, HXD_EVENT_FITS_KEY_NUM,
		  hxd_event_fits_keyword, hxd_event_fits_format,
		  hxd_event_fits_unit, extention_name, istat);
  
  if ( *istat ) {
    fprintf(stderr, "%s:fits_create_tbl failed (%)\n", pname, *istat);
  }
  
}


void hxdeventFits_col_num( fitsfile *fp, int *colnum, int *istat ){
  
  int i;
  
  int casesen = TRUE;
  
  for( i=0;i<HXD_EVENT_FITS_KEY_NUM;i++ ){
    fits_get_colnum( fp, casesen, hxd_event_fits_keyword[i],
		    &colnum[i], istat);
    
    if ( *istat ) {
      fprintf(stderr, "%s: fits_get_colnum('%s') failed (%d)\n",
	      pname, hxd_event_fits_keyword[i], *istat);
      return;
    }
  }
  
}

void hxdeventFits_col_read( fitsfile *fp, long irow, int *colnum,
			    HxdEventFits02 *fits, int *istat ){
  
  long firstelem = 1;
  long nelements = 1;
  
  int anynul;
  
  {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[TIME], irow, firstelem, nelements,
		      nulval, &fits->time, &anynul, istat);
  }    
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('TIME') failed (%d)\n",
	    pname, *istat);   
    return;
  } else {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[S_TIME], irow, firstelem, nelements,
		      nulval, &fits->s_time, &anynul, istat);
    fits->aetime = fits->s_time; /** There is no information of AETIME.**/
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('S_TIME') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    int nulval = 0;
    fits_read_col_int(fp, colnum[WELTIME], irow, firstelem,nelements,
		      nulval, &fits->weltime, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('WELTIME') failed  (%d)\n",
	    pname, *istat);	
    return;
  } else {
    unsigned int nulval = 0;
    fits_read_col_uint(fp, colnum[TI], irow, firstelem, nelements, nulval,
		       &fits->ti, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('TI') failed (%d)\n", pname,
	    *istat);
    return;
  } else {
    unsigned int nulval = 0;
    fits_read_col_uint(fp, colnum[MTI], irow, firstelem, nelements, nulval,
		       &fits->mti, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('MTI') failed (%d)\n", pname,
	    *istat);
    return;
  } else {
    long firstbit = 1;            /* first bit to read (1 = 1st) */
    int  nbits = 1;  /* number of bits to read (<= 32) */
    fits_read_col_bit(fp, colnum[LENGTH_CHK], irow,
		      firstbit, nbits,  &fits->length_chk, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('LENGTH_CHK') failed (%d)\n",
	    pname, *istat);        
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[UNITID], irow, firstelem, nelements,
		      nulval, &fits->unitid, &anynul, istat);	
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('UNITID') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[DET_TYPE], irow, firstelem, nelements,
		      nulval, &fits->det_type, &anynul, istat);	
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('DET_TYPE') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[PIN_ID], irow, firstelem, nelements,
		      nulval, &fits->pin_id, &anynul, istat);	
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PIN_ID') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    short nulval = 0;
    fits_read_col_sht(fp, colnum[GRADE_QUALTY], irow, firstelem, nelements,
		      nulval, &fits->grade_qualty, &anynul,istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('GRADE_QUALTY') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[GRADE_PMTTRG], irow, firstelem, nelements,
		      nulval, &fits->grade_pmttrg, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('GRADE_PMTTRG') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[GRADE_PINTRG], irow, firstelem, nelements,
		      nulval, &fits->grade_pintrg, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('GRADE_PINTRG') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[GRADE_PSDSEL], irow, firstelem, nelements,
		      nulval, &fits->grade_psdsel, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('GRADE_PSDSEL') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    int nulval = 0;
    fits_read_col_uint(fp, colnum[GRADE_HITPAT], irow, firstelem, nelements,
		       nulval, &fits->grade_hitpat, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('GRADE_HITPAT') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    int nulval = 0;
    fits_read_col_uint(fp, colnum[GRADE_RESERV], irow, firstelem, nelements,
		       nulval, &fits->grade_reserv, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('GRADE_RESERV') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    long firstbit = 1;            /* first bit to read (1 = 1st) */
    int nbits = HXD_TRIG_NBIT;  /* number of bits to read (<= 32) */
    char  larray[HXD_TRIG_NBIT];/* array of logicals corresponding to bits */
    int i;
    fits_read_col_bit(fp, colnum[TRIG], irow,
		      firstbit, nbits,  &larray[0], istat);

    fits->trig = 0x00;
    for(i=0;i<HXD_TRIG_NBIT; i++) {
      /** TRIG:   None, sud, anode, p0, p1, p2, p3, pse **/
      /** larray: None, 0,   1,     2,  3,  4,  5,  6   **/
      fits->trig |= ( (larray[i])<<(HXD_TRIG_NBIT - 1 -i) );
    }

  } if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('TRIG') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    long firstbit = 1;            /* first bit to read (1 = 1st) */
    int  nbits = HXD_QFLAG_NBIT;  /* number of bits to read (<= 32) */
    char larray[HXD_QFLAG_NBIT];  /* array of logicals corresponding to bits */
    int i;

    fits_read_col_bit(fp, colnum[QUALITY_FLAGS], irow,
		      firstbit, nbits,  &larray[0], istat);

    fits->quality_flags = 0x00;
    for(i=0;i<HXD_QFLAG_NBIT; i++) {
      /** Flag:   None,  pin_ld, pin_dbl, pin_ud, rst, pmt_dbl, pmt_ud, psd**/
      /** larray: None,  0,      1,       2,      3,   4,       5,      6  **/
      fits->quality_flags |= ( (larray[i])<<(HXD_QFLAG_NBIT - 1 -i) );
    }

  }    
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('QUALITY_FLAGS') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    long firstbit = 1;            /* first bit to read (1 = 1st) */
    int nbits = HXD_HITWEL_NBIT;  /* number of bits to read (<= 32) */
    char larray[HXD_HITWEL_NBIT]; /* array of logicals corresponding to bits */
    int i;
    fits_read_col_bit(fp, colnum[HIT_PATTERN_WELL], irow,
		      firstbit, nbits, &larray[0], istat);

    fits->hit_pattern_well = 0x00;
    for(i=0;i<HXD_HITWEL_NBIT; i++) {
      /** Hit_W: None, None, ..., W00, W01, ,,, W32, W33 **/
      fits->hit_pattern_well |= ( (larray[i]) << (HXD_HITWEL_NBIT - 1 -i) );
    }

  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('HIT_PATTERN_WELL') failed (%d)\n",
	    pname, *istat);
    
    return;
  } else {
    long firstbit = 1;            /* first bit to read (1 = 1st) */
    int nbits = HXD_HITANT_NBIT;  /* number of bits to read (<= 32) */
    char larray[HXD_HITANT_NBIT]; /* array of logicals corresponding to bits */
    int i;
    fits_read_col_bit(fp, colnum[HIT_PATTERN_ANTI], irow,
		      firstbit, nbits, &larray[0], istat);

    fits->hit_pattern_anti = 0x00;
    for(i=0;i<HXD_HITANT_NBIT; i++) {
      /** Hit_W: None, None, ..., T00, T01, ,,, T33, T34 **/
      fits->hit_pattern_anti |= ( (larray[i]) << (HXD_HITANT_NBIT - 1 -i) );
    }

  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('HIT_PATTERN_ANTI') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    short nulval = 0;
    short short_val; 
    fits_read_col_sht(fp, colnum[PI_FAST], irow, firstelem, nelements,
		      nulval, &short_val, &anynul, istat);
    fits->pi_fast = short_val;
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PI_FAST') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    short nulval = 0;
    short short_val;
    fits_read_col_sht(fp, colnum[PI_SLOW], irow, firstelem, nelements,
		      nulval, &short_val, &anynul,istat);
    fits->pi_slow = short_val;
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PI_SLOW') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    short nulval = 0;
    fits_read_col_sht(fp, colnum[PHA_FAST], irow, firstelem, nelements,
		      nulval, &fits->pha_fast, &anynul, istat);
    }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PHA_FAST') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    short nulval = 0;
    fits_read_col_sht(fp, colnum[PHA_SLOW], irow, firstelem, nelements,
		      nulval, &fits->pha_slow, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PHA_SLOW') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    unsigned char char_val;
    fits_read_col_byt(fp, colnum[PI_PIN0], irow, firstelem, nelements,
		      nulval, &char_val, &anynul, istat);
    fits->pi_pin0 = char_val;
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PI_PIN0') failed (%d)\n",
	    pname, *istat);
        return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[PHA_PIN0], irow, firstelem, nelements,
		      nulval, &fits->pha_pin0, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PHA_PIN0') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    unsigned char char_val;
    fits_read_col_byt(fp, colnum[PI_PIN1], irow, firstelem, nelements,
		      nulval, &char_val, &anynul, istat);
    fits->pi_pin1 = char_val;
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PI_PIN1') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[PHA_PIN1], irow, firstelem, nelements,
		      nulval, &fits->pha_pin1, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PHA_PIN1') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    unsigned char char_val; 
    fits_read_col_byt(fp, colnum[PI_PIN2], irow, firstelem, nelements,
		      nulval, &char_val, &anynul, istat);
    fits->pi_pin2 = char_val;
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PI_PIN2') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[PHA_PIN2], irow, firstelem, nelements,
		      nulval, &fits->pha_pin2, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PHA_PIN2') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    unsigned char char_val;
    fits_read_col_byt(fp, colnum[PI_PIN3], irow, firstelem, nelements,
		      nulval, &char_val, &anynul, istat);
    fits->pi_pin3 = char_val;
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PI_PIN3') failed (%d)\n",
                pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    fits_read_col_byt(fp, colnum[PHA_PIN3], irow, firstelem, nelements,
		      nulval, &fits->pha_pin3, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PHA_PIN3') failed (%d)\n",
	    pname, *istat);
    return;
  } else {
    unsigned char nulval = 0;
    unsigned char char_val;
    fits_read_col_byt(fp, colnum[PI_PIN], irow, firstelem, nelements,
		      nulval, &char_val, &anynul, istat);
    fits->pi_pin = char_val;
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('PI_PIN') failed (%d)\n",
	    pname, *istat);
    return;
  }else {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[UPI_FAST], irow, firstelem, nelements,
                      nulval, &fits->upi_fast, &anynul, istat);
  } 
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('UPI_FAST') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[UPI_SLOW], irow, firstelem, nelements,
                      nulval, &fits->upi_slow, &anynul,istat);
  } 
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('UPI_SLOW') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[UPI_PIN0], irow, firstelem, nelements,
                      nulval, &fits->upi_pin0, &anynul, istat);
  } 
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('UPI_PIN0') failed (%d)\n",
            pname, *istat);
        return;
  } else {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[UPI_PIN1], irow, firstelem, nelements,
                      nulval, &fits->upi_pin1, &anynul, istat);
  } 
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('UPI_PIN1') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[UPI_PIN2], irow, firstelem, nelements,
                      nulval, &fits->upi_pin2, &anynul, istat);
  } 
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('UPI_PIN2') failed (%d)\n",
            pname, *istat);
    return;
  } else {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[UPI_PIN3], irow, firstelem, nelements,
                      nulval, &fits->upi_pin3, &anynul, istat);
  } 
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('UPI_PIN3') failed (%d)\n",
                pname, *istat);
    return;
  } else {
    double nulval = 0.0;
    fits_read_col_dbl(fp, colnum[UPI_PIN], irow, firstelem, nelements,
                      nulval, &fits->upi_pin, &anynul, istat);
  }
  if ( *istat ) {
    fprintf(stderr, "%s: fits_read_col('UPI_PIN') failed (%d)\n",
            pname, *istat);
    return;
  }

  return;
}

void hxdeventFits_col_write( fitsfile *fp, long irow, int *colnum,
			    HxdEventFits02 *fits, int *istat ){
  
  long firstelem = 1;
  long nelements = 1;
  
  fits_write_col_dbl(fp, colnum[TIME], irow, firstelem, nelements,
		     &fits->time, istat);
  if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TIME=%f failed (%d)\n",
	    pname, fits->time, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[S_TIME], irow, firstelem, nelements,
		       &fits->s_time, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col S_TIME=%f failed (%d)\n",
		pname, fits->s_time, *istat);
    return;
  } else {
    fits_write_col_int(fp, colnum[WELTIME], irow, firstelem, nelements,
		       &fits->weltime, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col WELTIME=%f failed (%d)\n",
	    pname, fits->weltime, *istat);
    return;
  } else {
    fits_write_col_uint(fp, colnum[TI], irow, firstelem, nelements,
			&fits->ti, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TI=%u failed (%d)\n",
	    pname, fits->ti, *istat);
    return;
  } else {
    fits_write_col_uint(fp, colnum[MTI], irow, firstelem, nelements,
			&fits->mti, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col MTI=%u failed (%d)\n",
	    pname, fits->mti, *istat);
    return;
  } else {
    /*
    fits_write_col_byt(fp, colnum[LENGTH_CHK], irow, firstelem, nelements,
		       &fits->length_chk, istat);
    */
    long  fbit = 1;     /* first bit of 7X to write(1=1st) */
    long  nbit = 1;     /* number of bits to write   */
    char  larray[1];    /* array of logicals corresponding to bits */
    larray[0] = fits->length_chk &0x01;
    fits_write_col_bit(fp, colnum[LENGTH_CHK], irow, fbit, nbit,
		       &larray[0], istat); 
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col_bit LENGTH_CHK=%d failed (%d)\n",
	    pname, fits->length_chk, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[UNITID], irow, firstelem, nelements,
		       &fits->unitid, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col UNITID=%d failed (%d)\n",
	    pname, fits->unitid, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[DET_TYPE], irow, firstelem, nelements,
		       &fits->det_type, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col DET_TYPE=%d failed (%d)\n",
	    pname, fits->det_type, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[PIN_ID], irow, firstelem, nelements,
		       &fits->pin_id, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PIN_ID=%d failed (%d)\n",
	    pname, fits->det_type, *istat);
    return;
  } else { /* 16X */
    fits_write_col_sht(fp, colnum[GRADE_QUALTY], irow, firstelem, nelements,
		       &fits->grade_qualty, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GRADE_QUALTY=0x%x failed (%d)\n",
	    pname, fits->grade_qualty, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[GRADE_PMTTRG], irow, firstelem, nelements,
		       &fits->grade_pmttrg, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GRADE_PMTTRG=%d failed (%d)\n",
	    pname, fits->grade_pmttrg, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[GRADE_PINTRG], irow, firstelem, nelements,
		       &fits->grade_pintrg, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GRADE_PINTRG=%d failed (%d)\n",
	    pname, fits->grade_pintrg, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[GRADE_PSDSEL], irow, firstelem, nelements,
		       &fits->grade_psdsel, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GRADE_PSDSEL=%d failed (%d)\n",
	    pname, fits->grade_psdsel, *istat);
    return;
  } else {
    fits_write_col_uint(fp, colnum[GRADE_HITPAT], irow, firstelem, nelements,
			&fits->grade_hitpat, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GRADE_HITPAT=%d failed (%d)\n",
	    pname, fits->grade_hitpat, *istat);
    return;
  } else {
    fits_write_col_uint(fp, colnum[GRADE_RESERV], irow, firstelem, nelements,
			&fits->grade_reserv, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col GRADE_RESERV=%d failed (%d)\n",
	    pname, fits->grade_reserv, *istat);
    return;
  } else { /* 7X */
    long  fbit = 1;                     /* first bit of 7X to write(1=1st) */
    long  nbit = HXD_TRIG_NBIT;         /* number of bits to write   */
    char  larray[HXD_TRIG_NBIT];/* array of logicals corresponding to bits */
    int i;    unsigned char trig;

    trig = fits->trig;
    for(i=0;i<HXD_TRIG_NBIT; i++) {          
      larray[HXD_TRIG_NBIT-i-1] = (trig >> i) & 0x1;
    }

    fits_write_col_bit(fp, colnum[TRIG], irow, fbit, nbit,
		       &larray[0], istat); 

  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col TRIG=%d failed (%d)\n",
	    pname, fits->trig, *istat);
    return;
  } else {
    long  fbit = 1;                    /* first bit to write(1=1st) */
    long  nbit = HXD_QFLAG_NBIT;       /* number of bits to write   */
    char larray[HXD_QFLAG_NBIT];   /* array of logicals corresponding to bits */
    int i;    

/*  qflags=fits->quality_flags;*/ /** Put in HXDeventFitsWrite **/
    for(i=0;i<HXD_QFLAG_NBIT; i++) {
      larray[HXD_QFLAG_NBIT-i-1] = (fits->quality_flags >> i) & 0x1;
    }
    fits_write_col_bit(fp, colnum[QUALITY_FLAGS], irow, fbit, nbit,
		       &larray[0], istat); 

  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col QUALITY_FLAGS=%d failed (%d)\n",
	    pname, fits->quality_flags, *istat);
    return;
  } else { /* 16X */
    long  fbit = 1;               /* first bit to write(1=1st) */
    long  nbit = HXD_HITWEL_NBIT; /* number of bits to write   */
    char larray[HXD_HITWEL_NBIT]; /* array of logicals corresponding to bits */
    int i;

    for(i=0;i<HXD_HITWEL_NBIT; i++) {
      larray[HXD_HITWEL_NBIT-1-i] = (fits->hit_pattern_well >> i) & 0x1;
    }

    fits_write_col_bit(fp, colnum[HIT_PATTERN_WELL], irow, fbit, nbit,
		       &larray[0], istat); 

  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col HIT_PATTERN_WELL=%d failed (%d)\n",
	    pname, fits->hit_pattern_well, *istat);
    return;
  } else { /* 20X */
    long  fbit = 1;               /* first bit to write(1=1st) */
    long  nbit = HXD_HITANT_NBIT; /* number of bits to write   */
    char larray[HXD_HITANT_NBIT]; /* array of logicals corresponding to bits */
    int i; 

    for(i=0;i<HXD_HITANT_NBIT; i++) {
      larray[HXD_HITANT_NBIT-1-i] = (fits->hit_pattern_anti >> i) & 0x1;
    }

    fits_write_col_bit(fp, colnum[HIT_PATTERN_ANTI], irow, fbit, nbit,
		       &larray[0], istat); 

  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col HIT_PATTERN_ANTI=%d failed (%d)\n",
	    pname, fits->hit_pattern_anti, *istat);
    return;
  } else {
    short short_val = (short) fits->pi_fast;
    fits_write_col_sht(fp, colnum[PI_FAST], irow, firstelem, nelements,
		       &short_val, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PI_FAST=%d failed (%d)\n",
	    pname, fits->pi_fast, *istat);
    return;
  } else {
    short short_val = (short) fits->pi_slow;
    fits_write_col_sht(fp, colnum[PI_SLOW], irow, firstelem, nelements,
		       &short_val, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PI_SLOW=%d failed (%d)\n",
	    pname, fits->pi_slow, *istat);
    return;
  } else {
    fits_write_col_sht(fp, colnum[PHA_FAST], irow, firstelem, nelements,
		       &fits->pha_fast, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PHA_FAST=%d failed (%d)\n",
	    pname, fits->pha_fast, *istat);
	return;
  } else {
    fits_write_col_sht(fp, colnum[PHA_SLOW], irow, firstelem, nelements,
		       &fits->pha_slow, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PHA_SLOW=%d failed (%d)\n",
	    pname, fits->pha_slow, *istat);
    return;
  } else {
    unsigned char char_val = (char) fits->pi_pin0;
    fits_write_col_byt(fp, colnum[PI_PIN0], irow, firstelem, nelements,
		       &char_val, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PI_PIN0=%d failed (%d)\n",
	    pname, fits->pi_pin0, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[PHA_PIN0], irow, firstelem, nelements,
		       &fits->pha_pin0, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PHA_PIN0=%d failed (%d)\n",
	    pname, fits->pha_pin0, *istat);
    return;
  } else {
    unsigned char char_val = (char) fits->pi_pin1;
    fits_write_col_byt(fp, colnum[PI_PIN1], irow, firstelem, nelements,
		       &char_val, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PI_PIN1=%d failed (%d)\n",
	    pname, fits->pi_pin1, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[PHA_PIN1], irow, firstelem, nelements,
		       &fits->pha_pin1, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PHA_PIN1=%d failed (%d)\n",
	    pname, fits->pha_pin1, *istat);
    return;
  } else {
    unsigned char char_val = (char) fits->pi_pin2;
    fits_write_col_byt(fp, colnum[PI_PIN2], irow, firstelem, nelements,
		       &char_val, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PI_PIN2=%d failed (%d)\n",
	    pname, fits->pi_pin2, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[PHA_PIN2], irow, firstelem, nelements,
		       &fits->pha_pin2, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PHA_PIN2=%d failed (%d)\n",
	    pname, fits->pha_pin2, *istat);
    return;
  } else {
    unsigned char char_val = (char) fits->pi_pin3;
    fits_write_col_byt(fp, colnum[PI_PIN3], irow, firstelem, nelements,
		       &char_val, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PI_PIN3=%d failed (%d)\n",
	    pname, fits->pi_pin3, *istat);
    return;
  } else {
    fits_write_col_byt(fp, colnum[PHA_PIN3], irow, firstelem, nelements,
		       &fits->pha_pin3, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PHA_PIN3=%d failed (%d)\n",
	    pname, fits->pha_pin3, *istat);
    return;
  } else {
    unsigned char char_val = (char) fits->pi_pin;
    fits_write_col_byt(fp, colnum[PI_PIN], irow, firstelem, nelements,
		       &char_val, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col PI_PIN=%d failed (%d)\n",
	    pname, fits->pi_pin, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[UPI_FAST], irow, firstelem, nelements,
                       &fits->upi_fast, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col UPI_FAST=%f failed (%d)\n",
            pname, fits->upi_fast, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[UPI_SLOW], irow, firstelem, nelements,
                       &fits->upi_slow, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col UPI_SLOW=%f failed (%d)\n",
            pname, fits->upi_slow, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[UPI_PIN0], irow, firstelem, nelements,
                       &fits->upi_pin0, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col UPI_PIN0=%f failed (%d)\n",
            pname, fits->upi_pin0, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[UPI_PIN1], irow, firstelem, nelements,
                       &fits->upi_pin1, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col UPI_PIN1=%f failed (%d)\n",
            pname, fits->upi_pin1, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[UPI_PIN2], irow, firstelem, nelements,
                       &fits->upi_pin2, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col UPI_PIN2=%f failed (%d)\n",
            pname, fits->upi_pin2, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[UPI_PIN3], irow, firstelem, nelements,
                       &fits->upi_pin3, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col UPI_PIN3=%f failed (%d)\n",
            pname, fits->upi_pin3, *istat);
    return;
  } else {
    fits_write_col_dbl(fp, colnum[UPI_PIN], irow, firstelem, nelements,
                       &fits->upi_pin, istat);
  } if ( *istat ) {
    fprintf(stderr,"%s:fits_write_col UPI_PIN=%f failed (%d)\n",
            pname, fits->upi_pin, *istat);
    return;
  } 
  return;
}


void hxdeventFits_modify_weltime_unit(fitsfile *fp, int time_mode, int *istat){
    char unit[64];
    int hdutype;
    char tmp_card[81];
    int i;

    if (fits_movabs_hdu(fp, HXD_EVENT_FITS_EVENT_EXTENTION,
                        &hdutype, istat)) {
      fprintf(stderr, "%s: fits_movabs_hdu failed To HDU %d (status=%d)\n",
              pname, HXD_EVENT_FITS_EVENT_EXTENTION, *istat);
      return;
    }
    
    switch (time_mode){
    case HXD_WELTIME_CLKRATE_NORM:
      sprintf(unit, 
              "TUNIT3  = '1/2^13 s'           / physical unit of field");
      break;
    case HXD_WELTIME_CLKRATE_SFINE_A:
    case HXD_WELTIME_CLKRATE_SFINE_B:
      sprintf(unit, 
              "TUNIT3  = '1/2^15 s'           / physical unit of field");
      break;
    case HXD_WELTIME_CLKRATE_FINE:
    default:
      sprintf(unit, 
              "TUNIT3  = '1/2^14 s'           / physical unit of field");
      break;
    }
    /*
    fits_write_key_unit(fp, "WELTIME", unit, istat);
    */

    for(i=1;;i++){
      if (fits_read_record(fp, i, tmp_card, istat)) {
        *istat = 0;
        break;
      }
      if(0 == strncmp("TFORM3 ", tmp_card, 7)){
        fits_insert_record(fp, i+1, unit, istat);
        if ( *istat ) {
          fprintf(stderr,"%s:fits_write_key_unit TRNTIME failed (%d)\n",
                  pname, *istat);
          return;
        }
        break;
      }
    }
    return;
}


void hxdeventFits_add_tlminmax(fitsfile *fp, int *istat){
  
  int i,j=0;
  
  char tmp_card[81];
  
  char *card[]={
    "TLMIN7  =                    0 / minimum legal value", /* UNITID */
    "TLMAX7  =                   15 / maximum legal value",
    "TLMIN8  =                    0 / minimum legal value", /* DET_TYPE */
    "TLMAX8  =                    9 / maximum legal value",
    "TLMIN9  =                    0 / minimum legal value", /* PIN_ID */
    "TLMAX9  =                    3 / maximum legal value",
    "TLMIN10 =                    0 / minimum legal value", /* GRADE_QUALTY */
    "TLMAX10 =                    1 / maximum legal value",
    "TLMIN11 =                    0 / minimum legal value", /* GRADE_PMTTRG */
    "TLMAX11 =                    2 / maximum legal value",
    "TLMIN12 =                    0 / minimum legal value", /* GRADE_PINTRG */
    "TLMAX12 =                    2 / maximum legal value",
    "TLMIN13 =               -100.0 / minimum legal value", /* GRADE_PSDSEL */
    "TLMAX13 =                100.0 / maximum legal value",
    "TLMIN14 =                    0 / minimum legal value", /* GRADE_HITPAT */
    "TLMAX14 =                    4 / maximum legal value",
    "TLMIN15 =                    0 / minimum legal value", /* GRADE_RESERV */
    "TLMAX15 =                  255 / maximum legal value",
    "TLMIN20 =                    0 / minimum legal value", /* PI_FAST */
    "TLMAX20 =                  511 / maximum legal value",
    "TLMIN21 =                    0 / minimum legal value", /* PI_SLOW */
    "TLMAX21 =                  511 / maximum legal value",
    "TLMIN22 =                    0 / minimum legal value", /* PI_PIN */
    "TLMAX22 =                  255 / maximum legal value",
    "TLMIN23 =                    0 / minimum legal value", /* PI_PIN0 */
    "TLMAX23 =                  255 / maximum legal value",
    "TLMIN24 =                    0 / minimum legal value", /* PI_PIN1 */
    "TLMAX24 =                  255 / maximum lagal value",
    "TLMIN25 =                    0 / minimum legal value", /* PI_PIN2 */
    "TLMAX25 =                  255 / maximum legal value",
    "TLMIN26 =                    0 / minimum legal value", /* PI_PIN3 */
    "TLMAX26 =                  255 / maximum legal value",
    "TLMIN27 =                    0 / minimum legal value", /* PHA_FAST */ 
    "TLMAX27 =                 4095 / maximum legal value",
    "TLMIN28 =                    0 / minimum legal value", /* PHA_SLOW */ 
    "TLMAX28 =                 4095 / maximum legal value",
    "TLMIN29 =                    0 / minimum legal value", /* PHA_PIN0 */ 
    "TLMAX29 =                  255 / maximum legal value",
    "TLMIN30 =                    0 / minimum legal vlaue", /* PHA_PIN1 */ 
    "TLMAX30 =                  255 / maximum legal value",
    "TLMIN31 =                    0 / minimum legal value", /* PHA_PIN2 */ 
    "TLMAX31 =                  255 / maximum legal value",
    "TLMIN32 =                    0 / minimum legal value", /* PHA_PIN3 */ 
    "TLMAX32 =                  255 / maximum legal value",
    "TLMIN33 =                    0 / minimum legal value", /* UPI_FAST */ 
    "TLMAX33 =                  511 / maximum legal value",
    "TLMIN34 =                    0 / minimum legal value", /* UPI_SLOW */ 
    "TLMAX34 =                  511 / maximum legal value",
    "TLMIN35 =                    0 / minimum legal value", /* UPI_PIN  */ 
    "TLMAX35 =                  255 / maximum legal value",
    "TLMIN36 =                    0 / minimum legal value", /* UPI_PIN0 */ 
    "TLMAX36 =                  255 / maximum legal value",
    "TLMIN37 =                    0 / minimum legal value", /* UPI_PIN1 */ 
    "TLMAX37 =                  255 / maximum legal value",
    "TLMIN38 =                    0 / minimum legal value", /* UPI_PIN2 */ 
    "TLMAX38 =                  255 / maximum legal value",
    "TLMIN39 =                    0 / minimum legal value", /* UPI_PIN3 */ 
    "TLMAX39 =                  255 / maximum legal value"
  };
  
  for(i=1;;i++){
    if (fits_flush_file(fp, istat)){
      fprintf(stderr,
	      "Error doing fits_flush_file() (status=%d)\n", *istat);
    }
    
    if (fits_read_record(fp, i, tmp_card, istat)) {
      /*fprintf(stderr, "%s:fits_read_record failed (%d)\n",
	pname, *istat);*/
      *istat = 0;
      break;
    }
    
    if(0 == strncmp("TFORM7 ", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }    
    if(0 == strncmp("TFORM8 ", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }    
    if(0 == strncmp("TFORM9 ", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }    
    if(0 == strncmp("TFORM10", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }    
    if(0 == strncmp("TFORM11", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }    
    if(0 == strncmp("TFORM12", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }    
    if(0 == strncmp("TFORM13", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }    
    if(0 == strncmp("TFORM14", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }    
    if(0 == strncmp("TFORM15", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }    
    if(0 == strncmp("TUNIT20", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT21", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT22", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT23", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT24", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT25", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
	    fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT26", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT27", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
	    fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT28", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT29", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT30", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT31", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT32", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT33", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT34", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT35", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT36", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT37", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT38", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    if(0 == strncmp("TUNIT39", tmp_card, 7)){
      fits_insert_record(fp, i+1, card[j++], istat);
      fits_insert_record(fp, i+2, card[j++], istat);
    }
    
    if ( *istat ) {
      fprintf(stderr, "%s:fits_insert_record failed (%d)\n",
	      pname, *istat);
      break;
    }
    }
}

void hxdeventFits_add_comment( fitsfile *fp, int *istat ){
  
  char *keyword[]={
    "TTYPE1  ", "TTYPE2  ", "TTYPE3  ", "TTYPE4  ", "TTYPE5  ", "TTYPE6  ",
    "TTYPE7  ", "TTYPE8  ", "TTYPE9  ", "TTYPE10 ", "TTYPE11 ", "TTYPE12 ",
    "TTYPE13 ", "TTYPE14 ", "TTYPE15 ", "TTYPE16 ", "TTYPE17 ", "TTYPE18 ",
    "TTYPE19 ", "TTYPE20 ", "TTYPE21 ", "TTYPE22 ", "TTYPE23 ", "TTYPE24 ",
    "TTYPE25 ", "TTYPE26 ", "TTYPE27 ", "TTYPE28 ", "TTYPE29 ", "TTYPE30 ",
    "TTYPE31 ", "TTYPE32 ", "TTYPE33 ", "TTYPE34 ", "TTYPE35 ", "TTYPE36 ",
    "TTYPE37 ", "TTYPE38 ", "TTYPE39 "
  };
  
  char *comment[]={
    "Event arrival time",
    "Well event data packet edit time",
    "Original 19-bit time in the telemetry                                 Time resolution is determined by                                      HXD_WPU_CLK_RATE in HK FITS.",
    "Secondary header time", 
    "Secondary header time modefied with PWH",
    "Length check (ok=1)",
    "Well unit ID from 0 to 15",
    "Detector type                                                         0:   GSO event, which HXD team recommended                            1:   PIN event, which HXD team recommended                            2:   Cleaned Pseudo event (for hxddtcor)                              255: Other events",
    "PIN ID in Unit (0-3) valid for DET_TPYE = 1                           255: undefined ID number (DET_TYPE != 1).",
    "Event grade (Data Quality)                                            1:Dirty event caused by some hardware trouble.                        0:Other event (selection recommended)",
    "Event grade (Triggerd by PMT)                                         0:Trigged only by Anode (recommended)                                 1:Trigged by Anode and single PIN                                     2:Others, Multiple or No triggered",
    "Event grade (Triggerd by PIN)                                         0:Trigged by single PIN with PIN_LD quality                             above offline PIN LD (recommended)                                  1:Trigged by single PIN with PIN_LD quality                             below offline PIN LD                                                2:Others",
    "Event grade (PSD Selection)                                           Likelihood of GSO event in sigme, estimated                           by SLOW_PI vs. FAST_PI diagram.",
    "Event grade (Hit Pattern)                                             0:No hit in other 35 units                                            1:No hit in 8 surrounding neighber units                              2:No hit in 4 surrounding neighber units                              3:Low multiplicity                                                    4:Others                                                              The recommendation is grade_hitpat = 1.",
    "Event grade (reserved)",
    "Trigger type flag,                                                    b1000000:SUD(trigd by SuperUpper Discriminator)                       b0100000:ANODE                                                        b0010000:PIN0 b0001000:PIN1 b0000100:PIN2                             b0000010:PIN3 b0000001:PSEUDO",
    "7 kinds of the quality information                                    b1000000:PIN_LD_TRG [1:PIN event, 0:others],                          b0100000:PIN_DBL [1:Double-trigged PIN event],                        b0010000:PIN_UD [1:PIN Upper Discriminator event],                    b0001000:Reset [1: Incomplete acquisition],                           b0000100:PMT_DBL [Double-trigged PMT event],                          b0000010:PMT_UD [1:PMT Upper Discri. event],                          b0000001:PSD_OUT [0:BGO background, 1:GSO event]",
    "Well hit pattern (W00, W01, ... W32, W33)",
    "Anti-conunter hit pattern (T00, T01.., T34)",
    "FAST PI channel                                                       In the PHA to PI conversion process by hxdpi,                         the following effects are included;                                    (1) Differencial ADC non linearity                                    (2) Integrated ADC non linearity                                      (3) PMT Gain variability.                                            The Gd edge effect is not included but in                             the response matrix. The definition is,                                   PI = E  /2.0 - 0.50,                                              where E is the GSO channel energy (keV) at                            the beginning of the each PI channel.",
    "Slow PI channel                                                         see the comments on FAST_PI.",
    "PIN  PI channel, copied by the task hxdgrade.                         The recommended PIN PI value (ch), copied from                        PINn_PI(n=0,1,2,3) selected by the DET_TYPE=1                         and by GRADE_PINTRG.",
    "PIN0 PI channel                                                       In the conversion process from PHA to PI,                             the following effects are included;                                     (1) Integrated ADC non linearity                                      (2) PIN individual gain variation.                                  The definition is,                                                       PI = E (keV) /0.375 - 1.00.                                        ",
    "PIN1 PI channel                                                        see the comments on PIN0_PI.",
    "PIN2 PI channel                                                        see the comments on PIN0_PI.",
    "PIN3 PI channel                                                        see the comments on PIN0_PI.",
    "FAST PHA channel",
    "Slow PHA channel",
    "PIN0 PHA channel",
    "PIN1 PHA channel",
    "PIN2 PHA channel",
    "PIN3 PHA channel",
    "FAST PI channel(floating format, see PI_FAST)",
    "SLOW PI channel(floating format, see PI_SLOW)",
    "PIN  PI channel(floating format, see PI_PIN )",
    "PIN0 PI channel(floating format, see PI_PIN0)",
    "PIN1 PI channel(floating format, see PI_PIN1)",
    "PIN2 PI channel(floating format, see PI_PIN2)",
    "PIN3 PI channel(floating format, see PI_PIN3)"
  };

  /** see QUALITY_FLAGS, in HXDeventFitsWrite **/
  /** fits.quality_flags = ((data[2] & 0x07) << 4) 
                         + ((data[4] & 0xF0) >> 4); **/  
  hxdFitsComment_write ( fp, HXD_EVENT_FITS_KEY_NUM, keyword, comment, istat);

  if(*istat){
    fprintf(stderr, "%s: hxdFitsComment_write failed(%d)\n",
	    pname, istat);
  }
  
}

void hxdeventFits_add_formatversion(fitsfile *fp, int *istat){
  int i;
  char tmp_card[81];
  char card[81];

  sprintf(card,
	  "HXD_FVER=                    %d / Format Version of HXD FITS File",
	  HXD_EVENT_FITS_FORMAT_VERSION);

  for(i=1;;i++){
    if (fits_flush_file(fp, istat)){
      fprintf(stderr,
	      "Error doing fits_flush_file() (status=%d)\n", *istat);
    }

    if (fits_read_record(fp, i, tmp_card, istat)) {
      *istat = 0;
      break;
    }

    if(0 == strncmp("CREATOR", tmp_card, 7)){
      fits_insert_record(fp, i, card,	 istat); /** before CREATOR key **/
      if(*istat){
	fprintf(stderr,"Error in writing keyword: HXD_FVER (status=%d)\n",
		*istat);
	break;
      }
      break;
    }
  }

}

void hxdeventFits_add_clkrate( fitsfile *fp, int *istat ){
  /*-- Insert CLK_RATE after DATAMODE --*/
  int i;
  char tmp_card[81];

  for(i=1;;i++){
    if (fits_flush_file(fp, istat)){
      fprintf(stderr,
	      "Error doing fits_flush_file() (status=%d)\n", *istat);
    }

    if (fits_read_record(fp, i, tmp_card, istat)) {
      *istat = 0;
      break;
    }

    if(0 == strncmp("DATAMODE", tmp_card, 8)){

      /* fits_write_key_str(fp, "CLK_RATE", "", "HXD clock rate", istat); */
      fits_insert_record(fp, i+1, 
			 "CLK_RATE= '        '           / HXD clock rate", 
			 istat);
      if(*istat){
	fprintf(stderr,"Error in writing keyword: CLK_RATE (status=%d)\n",
		*istat);
	break;
      }
      break;
    }
  }

  return;
}
