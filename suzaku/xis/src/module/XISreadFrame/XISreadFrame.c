/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:50:18 1999 by E. Miyata*/
/* xis_module/XISreadFrame/0.7_fast/XISreadFrame_v0.7.c: modified by M.Ozaki on Fri Aug 13 21:04:55 1999:
 *   - replace BnkGet, BnkPut, EvsGet, ...
 *     to BnkfGetM, BnkfPutM, EvsfGetM, ... .
 */
/**************************************

  XISreadFrame

	XIS Fits-format event file を読んでバンクに流すモジュール

	version 0.0	98.04.03	Emi Miyata
	従来の event だけの流れをやめて

		frame-----exp-----event----end
		  ↑       ↑       ↑      ↓
		  ↑       ↑       ↑------←	(event 単位)
		  ↑	   ↑		    ↓
		  ↑       ↑---------------←	(exposure 単位)
		  ↑			    ↓
		  ↑------------------------←	(frame 単位)

	というように FITS の構造をそのまま保存したような形で
	ループをまわすこととする。

    version 0.1	98.04.27	Emi Miyata
	TSTART/TSTOP を bnkput し忘れていたので bnkput するようにした
	_com のインターフェースを改良した。

    version 0.2	98.10.21	Emi Miyata
	DE の FM 仕様にあわせた

    version 0.3	1999.07.01	Emi Miyata
	mkxis1stfits 1999-05-27 taiou

    version 0.4	1999.07.17	Emi Miyata
	ftools 化にむけて xisread 対応とした。

    version 0.5	1999.07.23	Emi Miyata
	bank name を変えた。

    version 0.7	1999.07.25	Emi Miyata
	read keywords in frame extension

    version 0.8	1999.08.31	Emi Miyata

    version 0.9	2003.07.03	Hironori Matsumoto
         add "+1" after the strlen function at
         BnkfPutM ("XIS:FILENAME", strlen(input_file)+1, input_file);
    version 1.0     ?
    version 1.01	2005.09.16	Kiyoshi Hayashida
                       fits_movnam_hdu is used instead of fits_movabs_hdu
    version 1.02	2005.11.05	Masanobu Ozaki
                       correcting a message...
    version 1.1	2006.08.24	Y.ISHISAKI
	TLONG -> TINT in getEventNum()
    version 1.2	2007.01.30	Y.ISHISAKI
	add OUTFILE parameter
	add BNK XIS:FRAMES, XIS:FILENAME:PTR, XIS:OUTFITS:FILENAME:PTR
	run without FRAMES and EXPOSURES extensions
	check PILGet() error in _com()
    version 1.6	2007.04.30	Y.ISHISAKI
	add ignore_frames parameter
**************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "pil.h"
#include "fitsio.h"
#include "xisTelemFormat.h"
#include "xisEventFitsUtil.h"
#include "xisEditEventFitsUtil.h"

static char pname[] = "XISreadFrame";
char XISreadFrame_version[] = "version 1.6";

static fitsfile *_infitsd = NULL;
static fitsfile *infitsd = NULL;
static char infile_buffer[PIL_LINESIZE] = "frf.fits";
static char outfile_buffer[PIL_LINESIZE] = "frfout.fits";
static char *infile = infile_buffer;
static char *outfile = outfile_buffer;
static int ignore_frames = 0;
static long framenum = 0;
static char BNKhead[] = "FRAMES:";

/* fits column information */
static COLUMN_INF *column_inf;
static int column_num;
static int max_value_size;
static long irow;

static void
showParam(void)
{
  printf("\nANL:  *** %s show parameter ***\n\n", pname);
  printf("%20s   %s\n", "INFILE", infile);
  printf("%20s   %s\n", "OUTFILE", outfile);
  printf("%20s   %s\n", "IGNORE_FRAMES", ignore_frames ? "YES" : "NO");
  printf("\n");
}

void
XISreadFrame_startup(int *status)
{
  *status = ANL_OK;
}

void
XISreadFrame_com(int *status)
{
  static char *keytbl[] = {
    "INFILE",
    "OUTFILE",
    "IGNORE_FRAMES",
    "SHOW",
    "EXIT"
  };
  static char *help[] = {
    "Input event FITS file name",
    "Output event FITS file name",
    "Flag to ignore FRAMES extension",
    "Show current setting",
    "Exit from this menu"
  };
  static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
  char *k, *key;
  int ans[2];

  if ( *status ) {	/* ftools */
    if (
PILGetFname(k="infile", infile) ||
PILGetFname(k="outfile", outfile) ||
PILGetBool (k="ignore_frames", &ignore_frames) ||
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
    if ( 0 == strcmp("INFILE", key) ) {
      CLtxtrd(key, infile, sizeof(infile));
    } else if ( 0 == strcmp("OUTFILE", key) ) {
      CLtxtrd(key, infile, sizeof(outfile));
    } else if ( 0 == strcmp("IGNORE_FRAMES", key) ) {
      CLlogrd(key, &ignore_frames);
    } else if ( 0 == strcmp ("SHOW", key) ) {
      showParam();
    } else if ( 0 == strcmp ("EXIT", key) ) {
      break;
    }
  }

  *status = ANL_OK;
}

void
XISreadFrame_init(int *status)
{
  int hdutype, skip, extver;
  int fitsStatus = 0;
  FITS_GET_MEMORY;

  showParam();

  EvsDef("XISreadFrame:BEGIN");
  EvsDef("XISreadFrame:ENTRY");
  EvsDef("XISreadFrame:OK");

  BnkDef("XIS:FILENAME", FLEN_FILENAME);
  BnkDef("XIS:FILENAME:PTR", sizeof(char *));
  BnkDef("XIS:OUTFITS:FILENAME:PTR", sizeof(char *));
  BnkDef("XIS:FITS:PTR", sizeof(char *));

/* open input event file */
  FITS_CHECK_ERROR_ANL(
fits_open_file(&_infitsd, infile, READONLY, &fitsStatus));
  anl_msg_info("\
%s: succeeded in opening '%s'\n", pname, infile);

/* list up column for primary header */
  FITS_CHECK_ERROR_ANL(
fits_movabs_hdu(_infitsd, 1, &hdutype, &fitsStatus));
  skip = ANL_FALSE;
  if ( fits2bank_keyword(_infitsd, skip) != ANL_TRUE ) {
    *status = ANL_QUIT;
    return;
  }

  BnkDef("XIS:FRAMES", sizeof(framenum));

  if ( ignore_frames ) {
    framenum = 0;	/* NO FRAMES EXTENSION */
    goto no_frames;
  }

  FITS_CHECK_ERROR_ANL(
fits_reopen_file(_infitsd, &infitsd, &fitsStatus));

/* move to FRAMES extension */
  extver = 0;		/* EXTVER check ignored */
  hdutype = BINARY_TBL;
fits_movnam_hdu(infitsd, hdutype, FRAME_EXTENSION_NAME, extver, &fitsStatus);
  if ( fitsStatus ) {
    framenum = 0;	/* NO FRAMES EXTENSION */
    anl_msg_warning("\
%s: WARNING: no %s extension, ignore it\n", pname, FRAME_EXTENSION_NAME);
    goto no_frames;
  }

/* list up column for FRAMES extension */
  if ( list_up_column(infitsd, BNKhead, &column_inf, &column_num,
		      &max_value_size) != ANL_TRUE) {
    *status = ANL_QUIT;
    return;
  }

  skip = ANL_TRUE;
  if ( fits2bank_keyword(infitsd, skip) != ANL_TRUE ) {
    *status = ANL_QUIT;
    return;
  }

  BnkDef("XIS:FRAMES:LIST:PTR", sizeof(char *));
  BnkDef("XIS:FRAMES:LIST:NUM", sizeof(int));
  BnkDef("XIS:FRAMES:LIST:MAXSIZE", sizeof(int));
  BnkPut("XIS:FRAMES:LIST:PTR", sizeof(char *), &column_inf);
  BnkPut("XIS:FRAMES:LIST:NUM", sizeof(int), &column_num);
  BnkPut("XIS:FRAMES:LIST:MAXSIZE", sizeof(int), &max_value_size);

  FITS_CHECK_ERROR_ANL(
fits_read_key_lng(infitsd, "NAXIS2", &framenum, NULL, &fitsStatus));

  irow = 1L;

 no_frames:

  BnkPut("XIS:FILENAME", strlen(infile)+1, infile);
  BnkPut("XIS:FILENAME:PTR", sizeof(infile), &infile);
  BnkPut("XIS:OUTFITS:FILENAME:PTR", sizeof(outfile), &outfile);
  BnkPut("XIS:FITS:PTR", sizeof(char *), &_infitsd);
  BnkPut("XIS:FRAMES", sizeof(long), &framenum);

  *status = ANL_OK;
}

void
XISreadFrame_his(int *status)
{
  *status = ANL_OK;
}

void
XISreadFrame_bgnrun(int *status)
{
  EvsfSetM("XISreadFrame:BEGIN");

  *status = ANL_OK;
}

void
XISreadFrame_ana(int nevent, int eventid, int *status)
{
  int used, allprocessed;

  EvsfSetM("XISreadFrame:ENTRY");

  if ( 0 == framenum ) {	/* NO FRAMES EXTENSION */
    allprocessed = ANL_FALSE;
    BnkfGetM("XIS:READEVENT:ALLPROCESSED", sizeof(int), &used, &allprocessed);
    if ( ANL_TRUE == allprocessed ) {
      *status = ANL_QUIT;
      return;
    }
    *status = ANL_OK;
    return;
  }

  if ( framenum < irow ) {
    *status = ANL_QUIT;
    return;
  }

  if ( fits2bank(infitsd, column_inf, column_num, irow, max_value_size)
       != ANL_TRUE) {
    *status = ANL_QUIT;
    return;
  }

  irow++;

  EvsfSetM("XISreadFrame:OK");
  *status = ANL_OK;
}

void
XISreadFrame_endrun(int *status)
{
  *status = ANL_OK;
}

void
XISreadFrame_exit(int *status)
{
  int fitsStatus = 0;

  if ( NULL != _infitsd ) {
    fits_close_file(_infitsd, &fitsStatus);
  }
  if ( NULL != infitsd ) {
    fits_close_file(infitsd, &fitsStatus);
  }
  if ( fitsStatus ) {
    anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, fitsStatus);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; End: ***
*/
