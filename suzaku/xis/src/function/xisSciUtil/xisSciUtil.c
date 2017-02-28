/*************************************************
    xisSciUtil.c

    Version 1.0   2007.04.30    Y.ISHISAKI

    Version 1.1   2007.05.03    Y.ISHISAKI
	increase ap4y, ap256y size 16 -> 32
	add window option parameters needed for RAWY -> ACTY conversion
	check SCI_START_RAWY <= 0 in xisSciCalcDeltaACTY()
	only use SCI_START_RAWY, SCI_PERIOD_RAWY for window option

    Version 1.2   2007.05.05    Y.ISHISAKI
	add hidden_acty for strict treatment for hidden SCI row

    Version 1.3   2007.05.28    Y.ISHISAKI
	add xisSciWriteKeys()
	accept fp == NULL in xisSciReadKeys()

****************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"
#include "fitsio.h"
#include "xisSciUtil.h"

static char pname[] = "xisSciUtil-1.3";

int
xisSciCalcDeltaACTY(SCI_PARAM *sci)
{
  short i, rawy, dacty, dy;

  short start_rawy = sci->start_rawy;
  short period = sci->period_rawy;
  short nrow = sci->nrow;
  short *sciy = sci->rawy;
  short *dacty_map = sci->dacty;

  if ( period <= 0 ) {
    anl_msg_error("\
%s: illegal SCI_PERIOD_RAWY=%d\n", pname, period);
    return -1;
  }

  while ( 0 <= start_rawy ) {		/* start_rawy must be < 0 */
    start_rawy -= period;
  }

  sci->hidden_acty = sci->win_st + start_rawy;

  for (rawy = 0; rawy < 1024; rawy++) {
    dacty = rawy - start_rawy;
    for (i = 0; i < nrow; i++) {
      dy = rawy - sciy[i];
      if ( 0 <= dy && dy < dacty ) {
	dacty = dy;
      }
    }
    dacty_map[rawy] = dacty;
  }

  return 0;
}

int
xisSciReadKeys(fitsfile *fp, SCI_PARAM *sci)
{
  int i;
  char *k, keyname[FLEN_KEYWORD];

  int istat = 0;

  memset(sci, 0, sizeof(*sci));
  if ( NULL == fp ) {
    sci->code_id = -999;	/* unknown CODE_ID */
    sci->winopt = 0;		/* window off */
    sci->win_st = 0;
    sci->win_siz = 1024;
    sci->ci = 0;		/* no charge injection */
    return 0;
  }

  if (
fits_read_key(fp, TSHORT, k="CODE_ID", &sci->code_id, NULL, &istat) ||
fits_read_key(fp, TSHORT, k="WINOPT",  &sci->winopt,  NULL, &istat) ||
fits_read_key(fp, TSHORT, k="WIN_ST",  &sci->win_st,  NULL, &istat) ||
fits_read_key(fp, TSHORT, k="WIN_SIZ", &sci->win_siz, NULL, &istat) ||
fits_read_key(fp, TSHORT, k="CI", &sci->ci, NULL, &istat) ||
       0 ) {
    goto read_error;
  }

  if ( 2 != sci->ci && 3 != sci->ci ) {
    return 0;	/* not SCI-54 rows or SCI-108 rows */
  }

  if (
fits_read_key(fp, TSHORT, k="SCIPERIY", &sci->period_rawy, NULL, &istat) ||
fits_read_key(fp, TSHORT, k="SCISTATY", &sci->start_rawy, NULL, &istat) ||
fits_read_key(fp, TSHORT, k="SCISPEED", &sci->ispeed, NULL, &istat) ||
fits_read_key(fp, TSHORT, k="SCIN", &sci->nrow, NULL, &istat) ||
fits_read_key(fp, TSHORT, k="AP4N", &sci->ap4n, NULL, &istat) ||
fits_read_key(fp, TSHORT, k="AP256N", &sci->ap256n, NULL, &istat) ||
       0 ) {
    goto read_error;
  }

  if ( sci->nrow < 0 || 32 < sci->nrow ||
       sci->ap4n < 0 || 32 < sci->ap4n ||
       sci->ap256n < 0 || 32 < sci->ap256n ) {
    anl_msg_error("\
%s: too many SCI rows: SCIN=%d, AP4N=%d, AP256N=%d\n",
	pname, sci->nrow, sci->ap4n, sci->ap256n);
    return -1;
  }

  for (i = 0; i < sci->nrow; i++) {
    sprintf(keyname, "SCIY%d", i+1);
    if (
fits_read_key(fp, TSHORT, k=keyname, &sci->rawy[i], NULL, &istat) ) {
      goto read_error;
    }
  }

  for (i = 0; i < sci->ap4n; i++) {
    sprintf(keyname, "AP4Y%d", i+1);
    if (
fits_read_key(fp, TSHORT, k=keyname, &sci->ap4y[i], NULL, &istat) ) {
      goto read_error;
    }
  }

  for (i = 0; i < sci->ap256n; i++) {
    sprintf(keyname, "AP256Y%d", i+1);
    if (
fits_read_key(fp, TSHORT, k=keyname, &sci->ap256y[i], NULL, &istat) ) {
      goto read_error;
    }
  }

  istat = xisSciCalcDeltaACTY(sci);
  return istat;

 read_error:
  anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
  return istat;
}

int
xisSciBnkGetKeys(char *bnkname_fitsptr, SCI_PARAM *sci)
{
  char *k;
  int istat, used, hdutype, extver;
  fitsfile *fp0, *fp;

  /* open input event fits file */
  fp0 = NULL;
  used = 0;
  BnkGet(bnkname_fitsptr, sizeof(fp0), &used, &fp0);
  if ( used != sizeof(fp0) || NULL == fp0 ) {
    anl_msg_error("\
%s: BnkGet('%s') failed\n", pname, bnkname_fitsptr);
    return -1;
  }

  istat = 0;
  fits_reopen_file(fp0, &fp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_reopen_file() failed (%d)\n", pname, istat);
    return istat;
  }

  hdutype = BINARY_TBL;
  extver = 0;		/* extver check is ignored */
  fits_movnam_hdu(fp, hdutype, k="EVENTS", extver, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_movnam_hdu('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }

  istat = xisSciReadKeys(fp, sci);
  if ( istat ) {
    return istat;
  }

  fits_close_file(fp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
    return istat;
  }

  return 0;
}

int
xisSciWriteKeys(fitsfile *fp, SCI_PARAM *sci)
{
  static struct {
    char *code_id, *winopt, *win_st, *win_siz, *ci;
    char *sciperiy, *scistaty, *scispeed, *scin, *ap4n, *ap256n;
    char *sciy, *ap4y, *ap256y;
  } cm = {
    "micro-codel ID",
    "window option (0:Off, 1:1/4, 2:1/8, 3:1/16)",
    "window start address",
    "window size",
    "0:no CI,1:diagn.CI,2:SCI-54rows,3:SCI-108rows",
    "SCI periodicity",
    "SCI start rawy address",
    "SCI speed",
    "number of SCI rows",
    "number of SCI AP4 rows",
    "number of SCI AP256 rows",
    "list of SCI rows",
    "list of SCI AP4 rows",
    "list of SCI AP256 rows"
  };

  int i;
  char *k, keyname[FLEN_KEYWORD];

  int istat = 0;

  if (
fits_write_key_lng(fp, k="CODE_ID", sci->code_id, cm.code_id, &istat) ||
fits_write_key_lng(fp, k="WINOPT",  sci->winopt,  cm.winopt, &istat) ||
fits_write_key_lng(fp, k="WIN_ST",  sci->win_st,  cm.win_st, &istat) ||
fits_write_key_lng(fp, k="WIN_SIZ", sci->win_siz, cm.win_siz, &istat) ||
fits_write_key_lng(fp, k="CI", sci->ci, cm.ci, &istat) ||
       0 ) {
    goto write_error;
  }

  if ( 2 != sci->ci && 3 != sci->ci ) {
    return 0;	/* not SCI-54 rows or SCI-108 rows */
  }

  if (
fits_write_key_lng(fp, k="SCIPERIY", sci->period_rawy, cm.sciperiy, &istat) ||
fits_write_key_lng(fp, k="SCISTATY", sci->start_rawy, cm.scistaty, &istat) ||
fits_write_key_lng(fp, k="SCISPEED", sci->ispeed, cm.scispeed, &istat) ||
fits_write_key_lng(fp, k="SCIN", sci->nrow, cm.scin, &istat) ||
fits_write_key_lng(fp, k="AP4N", sci->ap4n, cm.ap4n, &istat) ||
fits_write_key_lng(fp, k="AP256N", sci->ap256n, cm.ap256n, &istat) ||
       0 ) {
    goto write_error;
  }

  if ( sci->nrow < 0 || 32 < sci->nrow ||
       sci->ap4n < 0 || 32 < sci->ap4n ||
       sci->ap256n < 0 || 32 < sci->ap256n ) {
    anl_msg_error("\
%s: too many SCI rows: SCIN=%d, AP4N=%d, AP256N=%d\n",
	pname, sci->nrow, sci->ap4n, sci->ap256n);
    return -1;
  }

  for (i = 0; i < sci->nrow; i++) {
    sprintf(keyname, "SCIY%d", i+1);
    if (
fits_write_key_lng(fp, k=keyname, sci->rawy[i], cm.sciy, &istat) ) {
      goto write_error;
    }
  }

  for (i = 0; i < sci->ap4n; i++) {
    sprintf(keyname, "AP4Y%d", i+1);
    if (
fits_write_key_lng(fp, k=keyname, sci->ap4y[i], cm.ap4y, &istat) ) {
      goto write_error;
    }
  }

  for (i = 0; i < sci->ap256n; i++) {
    sprintf(keyname, "AP256Y%d", i+1);
    if (
fits_write_key_lng(fp, k=keyname, sci->ap256y[i], cm.ap256y, &istat) ) {
      goto write_error;
    }
  }

  return 0;

 write_error:
  anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
  return istat;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; End: ***
*/
