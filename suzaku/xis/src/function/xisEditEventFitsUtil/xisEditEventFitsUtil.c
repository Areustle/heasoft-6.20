/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:53 1999 by E. Miyata*/
/**************************************
 *
 *  xisEditEventFitsUtil
 *	1999/07/17 created by miyata
 *	version 0.0 1999.07.17		Emi Miyata
 *	version 0.1 1999.07.21		Emi Miyata
 *	version 0.3 1999.07.25		Emi Miyata
 *	version 0.4 1999.08.31		Emi Miyata
 *		added skip keywords for XISEFITV=2
 *	version 0.5 1999.10.20		Emi Miyata
 *		for new c-compiler
 *	version 0.6 2000.02.05		Emi Miyata
 *		add read_event_extension
 *	version 0.7 2000.02.08		Emi Miyata
 *              treating unit in hairly way for poor fitsio
 *	version 0.8 2003.07.03		OZAKI Masanobu
 *              strlen off-by-one error
 *	version 1.0 2005.07.04		Hiroshi Nakajima
 *              change how to read sensor ID (XIS-0 --> XIS0)
 *              fix fits2bank and bank2fits
 *	version 1.1 2005.07.05		Hiroshi Nakajima
 *              escape from double definition of bank
 *	version 1.2 2005.07.20		Hiroshi Nakajima
 *              eliminate the sources of compile warnings
 *	version 1.21 2005.08.29		Kiyoshi Hayashida
 *              unsigned long constant is explicitely defined
 *              2147483648 -> 2147483648UL
 *	version 1.3  2005.09.17		Kiyoshi Hayashida
 *              skip_key updated for new fff file (2005-09-10-)
 *	version 1.4  2005.10.11		Hiroshi Nakajima
 *              skip_key updated for new fff file
 *	version 1.5  2005.10.30		Kiyoshi Hayashida
 *              skip_key updated for new fff file (OBS_ID,OBS_REM,LEAPFILE)
 *              ttype=TULONG is set evenif TSCALn value is not included
 *              ttype=TUINT is set properly
 *	version 1.6  2005.12.22		Kiyoshi Hayashida
 *              ttype=TBIT, TUSHORT are implemented
 *	version 1.7  2006.08.22		Y.ISHISAKI
 *              check with BnkIsDef() before BnkDef() in fits2bank_keyword()
 *	version 1.8  2006.08.24		Y.ISHISAKI
 *              use index in BnkPut, BnkGet in bank2fits(), fits2bank()
 *	version 1.9  2006.10.31		Y.ISHISAKI
 *		check SIMPLE, BITPIX, NAXIS, EXTEND in fits2bank_keyword()
 *		remove format_char()
 *		use fits_read_key_str() to get string in fits2bank_keyword()
 **************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"
#include "fitsio.h"
#include "xisEventFitsUtil.h"
#include "xisNamingFunc.h"
#include "xisread.h"
#include "xisEditEventFitsUtil.h"

#define DEBUG 0

static char pname[] = "xisEditEventFitsUtil";

/*************************************
	fits の column list をつくる。
*************************************/
int
list_up_column (fitsfile *fitsd, char *head, COLUMN_INF **column_inf,
		int *column_num, int *max_value_size)
{
  int icol, colnum;
  int fitsStatus=0;
  char comment[FLEN_COMMENT], keyword[FLEN_KEYWORD+10];
  char bnkname[128], bnk[128];

  int typecode;		/* TSTRING/TSHORT/TINT/TLONG etc. */
  long repeat;		/* if TFORM=4J, typecode=TLONG,repeat=4,width=8 */
  long width;		/* if TFORM=60A12, repeat=60,width=12 */
                        /* width == byte */
  unsigned long tzero;
  int tscal;
  FITS_GET_MEMORY;

  *max_value_size = 0;
  /* read column number */
  fits_read_key(fitsd, TINT, "TFIELDS", &colnum, comment, &fitsStatus);

  if (((*column_inf) = calloc (colnum, sizeof (COLUMN_INF))) == NULL) {
    fprintf (stderr,"%s: memory allocation error\n", pname);
    return ANL_FALSE;
  }

  /* read column */
  for (icol=1; icol<=colnum; icol++) {
    /* read column name */
    sprintf (keyword, "TTYPE%d", icol);
    fits_read_key_str(fitsd, keyword, bnkname, comment, &fitsStatus);
    /* check data type */
    fits_get_coltype (fitsd, icol, &typecode, &repeat, &width, &fitsStatus);
    /* TLONG is 4byte integer in CFITSIO */
    /* short is used as integer in bank */
    if ((width == 2) && (typecode==TSHORT || typecode==TUSHORT ||
			 typecode==TINT || typecode==TUINT) ) {
      width = 4;
    } else if ( typecode == TBYTE || typecode == TSBYTE ) {
      width = 4;
    } else if ( typecode == TFLOAT ) {
      width = 8;
    }
    (*column_inf)[icol-1].datatype = typecode;

    /* check unsigned long or signed long */
    if (typecode == TLONG) {
      sprintf (keyword, "TZERO%d", icol);
      fits_read_key (fitsd, TULONG, keyword, &tzero, comment, &fitsStatus);
      if (fitsStatus == 0 && tzero == 2147483648UL) {
	sprintf (keyword, "TSCAL%d", icol);
	fits_read_key (fitsd, TINT, keyword, &tscal, comment, &fitsStatus);
	if ( (fitsStatus == 0 && tscal == 1) || fitsStatus != 0 ) {
           /* "|| fitsStatus != 0" is added by K.Hayashida 2005/10/30 */
	  (*column_inf)[icol-1].datatype = TULONG;
	}
      }
    }
    fitsStatus=0;

    /* check unsigned int or signed int (added by K.Hayashida 2005/10/30)*/
    if (typecode == TINT) {
      sprintf (keyword, "TZERO%d", icol);
      fits_read_key (fitsd, TULONG, keyword, &tzero, comment, &fitsStatus);
      if (fitsStatus == 0 && tzero == 32768UL) {
	sprintf (keyword, "TSCAL%d", icol);
	fits_read_key (fitsd, TINT, keyword, &tscal, comment, &fitsStatus);
	if ( (fitsStatus == 0 && tscal == 1) || fitsStatus != 0 ) {
	  (*column_inf)[icol-1].datatype = TUINT;
	}
      }
    }
    fitsStatus=0;

    /* check unsigned short or signed short (added by K.Hayashida 2005/12/22)*/
    if (typecode == TSHORT) {
      sprintf (keyword, "TZERO%d", icol);
      fits_read_key (fitsd, TULONG, keyword, &tzero, comment, &fitsStatus);
      if (fitsStatus == 0 && tzero == 32768UL) {
	sprintf (keyword, "TSCAL%d", icol);
	fits_read_key (fitsd, TINT, keyword, &tscal, comment, &fitsStatus);
	if ( (fitsStatus == 0 && tscal == 1) || fitsStatus != 0 ) {
	  (*column_inf)[icol-1].datatype = TUSHORT;
	}
      }
    }
    fitsStatus=0;

    /* pack column information */
    sprintf (bnk, "XIS:%s%s", head, bnkname);
    (*column_inf)[icol-1].name = strdup (bnk);
    (*column_inf)[icol-1].size = repeat*width;
    (*column_inf)[icol-1].index = 0;
    (*column_inf)[icol-1].nelem = repeat;
    (*column_inf)[icol-1].colnum = icol;
    BnkDef (bnk, repeat*width);

    *max_value_size = (*max_value_size > repeat*width)
      ? *max_value_size : repeat*width;

    FITS_CHECK_ERROR(fitsStatus);

  }

  *column_num = colnum;

  return ANL_TRUE;
}

/*************************************
	fits のキーワード情報を読んできて bank にコピーする。
	primary header のときは skip_flag=ANL_FALSE として
	全ての keywords を bnk に落す。
*************************************/
int
fits2bank_keyword (fitsfile *fitsd, int skip)
{
  int i, ikey=1, sensor, skipflag;
  int fitsStatus=0, ivalue;
  double dvalue;
  char _key[FLEN_KEYWORD+5], keyname[FLEN_KEYWORD+5], value[FLEN_VALUE];
  char head[FLEN_VALUE];

  /* skip keywords listed below */
  static char *skip_key[] = {
    "SIMPLE", "BITPIX", "NAXIS", "EXTEND", "PCOUNT", "GCOUNT",
    "EXTNAME", "EXTEND", "XTENSION",
    "TFIELDS", "TLMIN", "TLMAX", "TTYPE", "TFORM", "TUNIT",
    "TZERO", "TSCAL",
    /* added after XISEFITV 2 */
    "INSTRUME", "XIS-AEID",
    "MTYPE", "MFORM",
    "SENSOR", "TELESCOP",
    "OBS_MODE", "DATAMODE",
    "XIS-APID", "EDITMODE",
    "CLK_MODE", "OBJECT",
    "OBSERVER", "DATE-OBS",
    "TIME-OBS", "DATE-END",
    "TIME-END", "TSTART",
    "TSTOP", "TELAPSE",
    "ONTIME", "RADECSYS",
    "EQUINOX", "RA_NOM",
    "DEC_NOM", "MJDREF",
    "TIMEREF", "TIMESYS",
    "TIMEUNIT", "TASSIGN",
    "CLOCKAPP", "TIMEDEL",
    "TIMEPIXR", "TIERRELA",
    "TIERABSO", "TLM_FILE",
    "CREATOR", "HDUCLAS",
    "ORIGIN", "DATE",
    "CHECKSUM", "DATASUM",
    /* Added by H. Nakajima 5 July 2005*/
    "RA_OBJ", "DEC_OBJ",
    "RA_PNT", "DEC_PNT",
    "PA_NOM", "MEAN_EA1",
    "MEAN_EA2", "MEAN_EA3",
    "TIM_FILE", "ATT_FILE",
    "ORB_FILE", "TELDEF",
    /* Added by K. Hayashida 16 Sep 2005 */
    "CODE_ID","WINOPT",
    "WIN_ST","WIN_SIZ",
    "SNAPTI1","DELAY1",
    "PSUM_L","CI",
    "BINNING","SRAM_VER",
    "TIMEZERO","EXPOSURE",
    "LIVETIME","MJD-OBS",
    "USER","FILIN001",
    /* Added by H. Nakajima 11 Oct 2005*/
    "SNAPTI2","DELAY2",
    "SNAPTI3","DELAY3",
    "SNAPTI4","DELAY4",
    /* Added by K. Hayashida 30 Oct 2005 */
    "OBS_ID","OBS_REM","LEAPFILE"
  };
  static int nkey = sizeof(skip_key) / sizeof(skip_key[0]);

  FITS_GET_MEMORY;

  /* read extension name for bank name */
  fits_read_key_str(fitsd, "EXTNAME", value, NULL, &fitsStatus);
  if (fitsStatus == 0 && strstr(value, "EVENT") == NULL) {
    sprintf (head, "XIS:%s:", value);
  } else {
    strcpy (head, "XIS:");
  }
  fitsStatus = 0;

  while (1) {
    fits_read_keyn(fitsd, ikey++, _key, value, NULL, &fitsStatus);
if(DEBUG) printf ("keyword - '%s'\n", _key);
    skipflag=0;
    if (strcmp (_key, "END") == 0) break;
    if ( 0 == strcmp(_key, "SIMPLE") ||
	 0 == strcmp(_key, "BITPIX") ||
	 0 == strcmp(_key, "NAXIS")  ||
	 0 == strcmp(_key, "EXTEND") ||
	 0 == strcmp(_key, "HISTORY")||
	 0 == strcmp(_key, "COMMENT") ) {
      continue;
    }
    if ( skip ) {
      for (i=0; i < nkey; i++) {
	if ( strstr(_key, skip_key[i]) != NULL) {
	    skipflag=1;
	    break;
	}
      }
      if ( skipflag ) continue;
    }
    /*printf ("on\n");*/
    sprintf(keyname, "%s%s", head, _key);
    if ( ANL_OK == BnkIsDef(keyname) ) {
      anl_msg_debug("\
%s: BNK keyword '%s' already defined\n", pname, keyname);
      continue;
    }
if(DEBUG) printf ("lets write  '%s'\n", keyname);
    if ( '\'' == value[0]  ) {
      /* keep beginning "'" */
      fits_read_key_str(fitsd, _key, value+1, NULL, &fitsStatus);
      if ( 0 == strcmp(_key, "EDITMODE") ) {
	ivalue = getEditModeNum(value);
	BnkDef(keyname, sizeof(int));
	BnkPut(keyname, sizeof(int), &ivalue);
      } else if ( 0 == strcmp(_key, "CLK_MODE") ) {
	ivalue = getClockModeNum(value);
	BnkDef(keyname, sizeof(int));
	BnkPut(keyname, sizeof(int), &ivalue);
      } else {
	BnkDef(keyname, strlen(value)+1);
	BnkPut(keyname, strlen(value)+1, value);
      }
    } else if ( strchr(value, '.') != NULL ) {
      dvalue = atof (value);
      BnkDef(keyname, sizeof(double));
      BnkPut(keyname, sizeof(double), &dvalue);
    } else {
      ivalue = atoi (value);
      BnkDef(keyname, sizeof(int));
      BnkPut(keyname, sizeof(int), &ivalue);
    }
    if ( 0 == strcmp(_key, "INSTRUME") ) {
      BnkDef("XIS:SENSOR", sizeof(int));
      if ( sscanf(value+1, "XIS%d", &sensor) > 0 ||
	   sscanf(value+1, "XIS-EM%d", &sensor) > 0 ||
	   sscanf(value+1, "XIS-FM%d", &sensor) > 0 ) {
	;
      } else {
	fprintf(stderr, "\
%s: Unknown sensor ID: '%s'\n", pname, value);
	sensor = -1;
      }
      BnkPut ("XIS:SENSOR", sizeof(int), &sensor);
    }
    FITS_CHECK_ERROR(fitsStatus);
  }

  return ANL_TRUE;
}

/*************************************
	fits の コラム情報を読んできて bank にコピーする。
*************************************/
int
fits2bank (fitsfile *fitsd, COLUMN_INF *column_inf, int column_num,
	   long irow, int max_value_size)
{
  static long firstelem=1L;
  static double dnull=0.0;
  static int    inull=0;
  static unsigned int unull=0;

  static int in_max=0;
  static double *dvalue=NULL;
  static int    *ivalue;
  static unsigned int *uvalue;
  static char   *bitarray;

  int icol, anynul;
  COLUMN_INF *cp;

  int fitsStatus=0;

  FITS_GET_MEMORY;

/* mem alloc */
  if (dvalue != NULL && in_max < max_value_size) {
    in_max = max_value_size;
    free (dvalue);
    dvalue = NULL;
  }
  if (dvalue == NULL) {
    dvalue  = malloc(max_value_size);
    ivalue = (int *)dvalue;
    uvalue = (unsigned int *)dvalue;
    bitarray = (char *)dvalue;
    if ( dvalue == NULL ) {
      fprintf (stderr, "\
%s:fits2bank: memory allocation error\n", pname);
      return ANL_FALSE;
    }
    in_max = max_value_size;
  }

#ifdef _SAKI_
/* check row number match to given row */
  if (getTimeMatch (fitsd, timematch, &row) != ANL_TRUE) {
    return ANL_FALSE;
  }
#endif

/* read column */
  for (icol = 1; icol <= column_num; icol++) {
    cp = &column_inf[icol-1];
    switch ( cp->datatype ) {
/* convert to double (64bit real) */
    case TFLOAT:
    case TDOUBLE:
      fits_read_col_dbl(fitsd, icol, irow, firstelem, cp->nelem,
			dnull, dvalue, &anynul, &fitsStatus);
      BnkfPut(cp->name, &cp->index, cp->size, dvalue);
      break;
/* convert to int (32bit signed integer) */
    case TBYTE:
    case TSBYTE:
    case TSHORT:
    case TUSHORT:
    case TINT:
    case TLONG:
    case TLOGICAL:
      fits_read_col_int(fitsd, icol, irow, firstelem, cp->nelem,
			inull, ivalue, &anynul, &fitsStatus);
      BnkfPut(cp->name, &cp->index, cp->size, ivalue);
      break;
/* convert to unsigned int (32bit unsigned integer) */
    case TUINT:
    case TULONG:
      fits_read_col_uint(fitsd, icol, irow, firstelem, cp->nelem,
			 unull, uvalue, &anynul, &fitsStatus);
      BnkfPut(cp->name, &cp->index, cp->size, uvalue);
      break;
    case TBIT:
      fits_read_col_bit(fitsd, icol, irow, firstelem, cp->nelem,
			bitarray, &fitsStatus);
      BnkfPut(cp->name, &cp->index, cp->size, bitarray);
      break;
    default:
      fprintf(stderr, "\
%s:fits2bank: datatype=%d not supported, at icol=%d\n",
	pname, cp->datatype, icol);
    }

    FITS_CHECK_ERROR(fitsStatus);
  }

  return ANL_TRUE;
}

/*************************************
	bank の情報を読んできて fits に書き出す。
*************************************/
int
bank2fits(fitsfile *fitsd, COLUMN_INF *column_inf,
	  int column_num, long irow, int max_value_size)
{
  static long firstelem = 1L;
  static int in_max=0;
  static double *dvalue=NULL;
  static int    *ivalue;
  static unsigned int    *uvalue;
  static char   *bitarray;

  int icol, size;
  COLUMN_INF *cp;

  int fitsStatus=0;

  FITS_GET_MEMORY;

  /* mem alloc */
  if ( dvalue != NULL && in_max < max_value_size ) {
    in_max = max_value_size;
    free (dvalue);
    dvalue = NULL;
  }
  if (dvalue == NULL) {
    dvalue = (double *) calloc (max_value_size, 1);
    ivalue = (int *)dvalue;
    uvalue = (unsigned int *)dvalue;
    bitarray = (char *)dvalue;
    if ( dvalue == NULL ) {
      fprintf (stderr, "\
%s:bank2fits: memory allocation error\n", pname);
      return ANL_FALSE;
    }
    in_max = max_value_size;
  }

  for (icol = 1; icol <= column_num; icol++) {
    cp = &column_inf[icol-1];
    switch ( cp->datatype ) {
/* convert to double (64bit real) */
    case TFLOAT:
    case TDOUBLE:
      BnkfGet(cp->name, &cp->index, cp->size, &size, dvalue);
      fits_write_col_dbl(fitsd, icol, irow, firstelem, cp->nelem,
			 dvalue, &fitsStatus);
      break;
/* convert to int (32bit signed integer) */
    case TBYTE:
    case TSBYTE:
    case TSHORT:
    case TUSHORT:
    case TINT:
    case TLONG:
    case TLOGICAL:
      BnkfGet(cp->name, &cp->index, cp->size, &size, ivalue);
      fits_write_col_int(fitsd, icol, irow, firstelem, cp->nelem,
			 ivalue, &fitsStatus);
      break;
/* convert to unsigned int (32bit unsigned integer) */
    case TUINT:
    case TULONG:
      BnkfGet(cp->name, &cp->index, cp->size, &size, uvalue);
      fits_write_col_uint(fitsd, icol, irow, firstelem, cp->nelem,
			  uvalue, &fitsStatus);
      break;
    case TBIT:
      BnkfGet(cp->name, &cp->index, cp->size, &size, bitarray);
      fits_write_col_bit(fitsd, icol, irow, firstelem, cp->nelem,
			 bitarray, &fitsStatus);
      break;
    default:
      fprintf(stderr, "\
%s:bank2fits: datatype=%d not supported, at icol=%d\n",
	pname, cp->datatype, icol);
    }

    FITS_CHECK_ERROR(fitsStatus);
  }

  return ANL_TRUE;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; End: ***
*/
