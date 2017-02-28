/*************************************
  XISputPixelQuality
     Put Pixel Quality for each event

  ver 0.0    2005.10.03 Kiyoshi Hayashida
	initial version
  ver 0.1    2005.10.18 Kiyoshi Hayashida
	cmpatible both with PIXEL_QUALITY and STATUS colum
  ver 0.2    2005.10.24 Masanobu Ozaki
	fix FITS_GET_MEMORY location in XISputPixelQuality_init();
	it must be at the beginning of a block because it is a
	macro of variables declaration.
	    2005.11.10 Kiyoshi Hayashida
	version no is corrected to be 0.2
  ver 0.3    2005.12.20 Kiyoshi Hayashida
	fix bug for segment boundaries
  ver 0.4    2005.12.22 Kiyoshi Hayashida
	compatible with new version (ver 2) caldb file
         ( ae_xi?_badcolum_20051122.fits ) with backward compatibility
  ver 0.5    2005.12.24 Kiyoshi Hayashida
	reading bitfield from caldb file is corrected
  ver 0.6    2006.02.03 Kiyoshi Hayashida
	buf[80]->buf[PIL_LINESIZE]
  ver 0.7    2006.02.7 Kiyoshi Hayashida
	BnkfGetM aetime sizeof(int)->double
  ver 0.8    2006.04.10 Ken Ebisawa
	EXTAME of badcolumn file can be either
	XIS?_BADCOLUMNS (old format) or BADCOLUMNS
	(new format).
  ver 0.9    2006.08.24 Y.ISHISAKI
	increase bitarray[2 -> NBITS_BCCODE];
	check if NBITS_BCCODE == caldb_bccode_nbits in _init()
	remove redundant parentheses in _ana()
		fix bug in calculation of pixel_quality
	define SETBIT() macro
	use BnkIsDef() to check PIXEL_QUALITY or STATUS in _init()
	call aefits_write_name_vers() to write history in _endrun()
  ver 1.0    2006.10.31 Y.ISHISAKI
	change parameter name, badcolumn_file -> badcolumfile
	change parameter name, calmask_file -> calmaskfile
	support CALDB for badcolumfile, calmaskfile
	use xisrsp_get_caldb_file()
  ver 1.1    2007.01.31 Y.ISHISAKI
	use aefits_write_module_history() in XISeditEventFits
  ver 1.2    2007.05.04 Y.ISHISAKI
	use xisPixelQualityInit(), xisPixelQuality() functions
	bug fix of checking bccode by using TESTBIT() macro
	use fits_movabs_hdu() instead of fits_movnam_hdu() for old badcolumfile
  ver 2.0    2007.05.05 Y.ISHISAKI
	xisPixelQualityInit(), xisPixelQuality() moved to xisPixelQuality.c
	add 'enable_scipixq' parameter
*************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "com.h"
#include "cli.h"
#include "pil.h"
#include "fitsio.h"
#include "xisTelemFormat.h"
#include "xisSciUtil.h"
#include "xisPixelQuality.h"
#include "xisRespUtil.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "XISputPixelQuality";
char XISputPixelQuality_version[] = "version 2.0";

#define _PRINT_OUT_

/* parameter = CALDB file name */
static struct {
  int enable_scipixq;
  char *badcolumfile, o_badcolumfile[PIL_LINESIZE];
  char *calmaskfile, o_calmaskfile[PIL_LINESIZE];
} com;

static int exist_pixel_quality;
static int exist_status;

static PIXQ_INFO pixq;
static PIXQ_STAT statistics;

static void
showParam(void)
{
  printf("\nANL:  *** %s show parameter ***\n\n", pname);
  printf("%20s   %s\n", "ENABLE_SCIPIXQ", com.enable_scipixq ? "YES":"NO");
  printf("%20s   '%s'%s\n", "BADCOULMFILE", com.badcolumfile,
	 (com.badcolumfile == com.o_badcolumfile) ? "" : " (CALDB)");
  printf("%20s   '%s'%s\n", "CALMASKFILE", com.calmaskfile,
	 (com.calmaskfile == com.o_calmaskfile) ? "" : " (CALDB)");
  printf("\n");
}

void
XISputPixelQuality_startup(int *status)
{
  com.badcolumfile = strcpy(com.o_badcolumfile, "CALDB");
  com.calmaskfile = strcpy(com.o_calmaskfile, "CALDB");

  *status = ANL_OK;
}

void
XISputPixelQuality_com(int *status)
{
  static char *keytbl[] = {
    "SHOW",
    "ENABLE_SCIPIXQ",
    "BADCOLUMFILE",
    "CALMASKFILE",
    "EXIT"
  };
  static char *help[] = {
    "Show current setting",
    "Flag to enable SCI pixel quality bits",
    "badcolumn file in CALDB",
    "calmask file in CALDB",
    "Exit from this menu"
  };
  static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
  char *k, *key;
  int ans[2];

  if ( *status ) {	/* ftools */
    if (
PILGetBool (k="enable_scipixq", &com.enable_scipixq) ||
PILGetFname(k="badcolumfile", com.o_badcolumfile) ||
PILGetFname(k="calmaskfile", com.o_calmaskfile) ||
	 0 ) {
      anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
      *status = ANL_QUIT;
      return;
    }
    *status = ANL_OK;
    return;
  }

  for(;;) {
    CMinquir(pname, nkey, keytbl, help, 1, ans);
    key = keytbl[ans[1]-1];
    if (strcmp (key, "SHOW") == 0) {
      showParam ();
    } else if ( 0 == strcmp("ENABLE_SCIPIXQ", key) ) {
      CLlogrd(key, &com.enable_scipixq);
    } else if ( 0 == strcmp("BADCOLUMFILE", key) ) {
      CLtxtrd(key, com.badcolumfile, sizeof(com.o_badcolumfile));
    } else if ( 0 == strcmp("CALMASKFILE", key) ) {
      CLtxtrd(key, com.calmaskfile, sizeof(com.o_calmaskfile));
    } else if (strcmp (key, "EXIT") == 0) {
      return;
    }
  }

  *status = ANL_OK;
}

void
XISputPixelQuality_init(int *status)
{
  char *code, *instrume, o_instrume[FLEN_VALUE];
  int used, istat;

  EvsDef("XISputPixelQuality:BEGIN");
  EvsDef("XISputPixelQuality:ENTRY");
  EvsDef("XISputPixelQuality:OK");

  BnkGet("XIS:INSTRUME", sizeof(o_instrume), &used, &o_instrume);
  instrume = o_instrume + 1;	/* remove beginning "'" */
  code = "BADPIX";
  com.badcolumfile = xisrsp_get_caldb_file(instrume, code, com.o_badcolumfile);
  code = "CALMASK";
  com.calmaskfile = xisrsp_get_caldb_file(instrume, code, com.o_calmaskfile);
  if ( NULL == com.badcolumfile || NULL == com.calmaskfile ) {
    goto quit;
  }
  showParam();

  pixq.badcol_filename = com.badcolumfile;
  pixq.calmask_filename = com.calmaskfile;

  istat = xisSciBnkGetKeys("XIS:FITS:PTR", &pixq.sci);
  if ( istat ) {
    goto quit;
  }
  if ( 0 == com.enable_scipixq ) {
    pixq.sci.ci = 0;		/* force disable SCI pixel quality bits */
    pixq.sci.period_rawy = pixq.sci.nrow = pixq.sci.ap4n = pixq.sci.ap256n = 0;
  }

  istat = xisPixelQualityInit(instrume, &pixq);
  if ( istat ) {
    goto quit;
  }

  exist_pixel_quality = BnkIsDef("XIS:PIXEL_QUALITY");
  exist_status = BnkIsDef("XIS:STATUS");
#ifdef _PRINT_OUT_
  anl_msg_debug("\
%s: pixel_quality %d, status %d\n", pname, exist_pixel_quality, exist_status);
#endif

  if ( ANL_OK != exist_status && ANL_OK != exist_pixel_quality ) {
    anl_msg_warning("\
%s: WARNING: PIXEL_QUALITY and STATUS column is not found\n",pname);
  }

  xisPixqStatInit(&statistics);

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XISputPixelQuality_his(int *status)
{
  *status = ANL_OK;
}

void
XISputPixelQuality_bgnrun(int *status)
{
  EvsfSetM("XISputPixelQuality:BEGIN");

  *status = ANL_OK;
}

void
XISputPixelQuality_ana(int nevent, int eventid, int *status)
{
  double aetime;
  int used, actx, acty, rawy;
  unsigned int pixel_quality;

  EvsfSetM("XISputPixelQuality:ENTRY");

  BnkfGetM("XIS:TIME", sizeof(double), &used, &aetime);
  BnkfGetM("XIS:ACTX", sizeof(int), &used, &actx);
  BnkfGetM("XIS:ACTY", sizeof(int), &used, &acty);
  BnkfGetM("XIS:RAWY", sizeof(int), &used, &rawy);

  pixel_quality = xisPixelQuality(&pixq, aetime, actx, acty, rawy);

  xisPixqStatAdd(&statistics, pixel_quality);

  if ( ANL_OK == exist_pixel_quality ) {
    BnkfPutM("XIS:PIXEL_QUALITY", sizeof(int), &pixel_quality);
  }
  if ( ANL_OK == exist_status ) {
    BnkfPutM("XIS:STATUS", sizeof(int), &pixel_quality);
  }

  EvsfSetM("XISputPixelQuality:OK");
  *status = ANL_OK;
}

void
XISputPixelQuality_endrun(int *status)
{
  fitsfile* fp;					/* for output evt fits */
  char history[FLEN_COMMENT+PIL_LINESIZE];
  int used = 0, istat = 0;

  xisPixqStatWrite(&statistics, stdout);	/* show statistics */

  /*************************************************/
  /* Write a HISTORY keyword into output fits file */
  /*************************************************/

  BnkGet("XIS:OUTFITS:EVENTS:PTR", sizeof(fp), &used, &fp);
  if ( used != sizeof(fp) ) {
    *status = ANL_OK;
    return;
  }

  sprintf(history, "\
  enable_scipixq=%s", com.enable_scipixq ? "yes" : "no");
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  badcolumfile='%s'%s", com.badcolumfile,
	  (com.badcolumfile == com.o_badcolumfile) ? "" : " (CALDB)");
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  calmaskfile='%s'%s", com.calmaskfile,
	  (com.calmaskfile == com.o_calmaskfile) ? "" : " (CALDB)");
  fits_write_history(fp, history, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }

  istat = xisPixqStatWriteFits(&statistics, fp);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

void
XISputPixelQuality_exit(int *status)
{
  *status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; End: ***
*/
