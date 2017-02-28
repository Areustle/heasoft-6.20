/*************************************

  XIStrailCorrection
        Correction against charge trail from the center pixel

  ver 0.0 2004/12/28 	K. Hayashida
  ver 0.1 2005/01/05	K. Hayashida
	compatible with both xisdl and xisread
  ver 0.2 2005/01/12	K. Hayashida
	make random number factor as an option
	(default = 0.0 =not to use random number)
  ver 0.3 2005/01/27	K. Hayashida
	BNK "frame_event data" is considered for xisdl
  ver 0.4 2005/06/14	H. Nakajima
	input gain parameters from fits file
  ver 0.5 2005/06/21	H. Nakajima
	add an explicit branch about editmode
  ver 0.6 2005/06/21	H. Nakajima
	insure the filename length of PIL_LINESIZE
  ver 0.7 2005/07/08      H. Nakajima
	delete XISDESim.h from the list of include files
  ver 0.8 2005/07/28	H. Nakajima
	fraction_h and fraction_v is zero
	when rawx and acty equal zero, respectively
  ver 0.81 2005/08/16     K. Hayashida
	"read from caldb" part was corrected and moved into *ini
	compatible with 3x3 mode
  ver 0.9 2005/10/04	H. Nakajima
	change the format of CALDB file
	use PHASCORR if Evs("XISpreparePHASCORR:OK")
	leave history comments to the output file
  ver 1.0 2005/12/05 Hiroshi Nakajima
	change how to leave HISTORY (utilize aeFitsHeaderUtil)
	transparent when Frame, DarkInit, DarkUpdate, and DarkFrame mode
  ver 1.1 2005/12/07 Hiroshi Nakajima
	seek proper caltime correctly
  ver 1.2 2006/01/25 Hiroshi Nakajima
	set buffer length to "PIL_LINESIZE" in endrun
  ver 1.3 2006/02/06 Hiroshi Nakajima
	set buffer length to "2*PIL_LINESIZE" in endrun
  ver 1.4 2006.08.22 Y.ISHISAKI
	use XIS:TSTART,TSTOP instead of XIS:DATE-OBS,TIME-OBS
	use anl_msg_***() functions for messages
  ver 2.0 2006.10.31 Y.ISHISAKI
	change parameter name, trcor_rand_seed/skip -> rand_seed/skip
	change parameter name, trcor_flag_random (i) -> trcor_flag_rand (b)
	change parameter name, trcor_caldbfile -> makepifile
	support CALDB for makepifile
	use xisrsp_get_caldb_file(), xisrsp_seek_valid_row()
	static declarations of trailfraction_v(), trailfraction_v()
  ver 2.1 2007.01.31 Y.ISHISAKI
	use aefits_write_module_history() in XISeditEventFits
  ver 3.0 2007.04.30 Y.ISHISAKI
	add enable_trcor parameter
  ver 3.1 2008.04.10 Y.ISHISAKI
	bug fix in check_caltime_scti/pcti(), where rejecting obstime == 0.0
	set initial obstime = tstart in _init()
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
#include "aste_rand.h"
#include "xisTelemFormat.h"
#include "xisRespUtil.h"
#include "aeFitsHeaderUtil.h"

#define  CHTR_EXTENSION_NAME	"CHARGETRAIL"

char XIStrailCorrection_version[] = "version 3.1";
static char pname[] = "XIStrailCorrection";

/* array parameters for charge trail parameters */
#define ASIZE	(XIStotalSegNo * XISactiveSegmentHsize)
typedef struct {
  int format_version;
  char *extname;
  double last_obstime;
  double caltime, caltime_prev, caltime_next;
  int off_rawx[ASIZE];
  int off_acty[ASIZE];
  double trailh[ASIZE];
  double trailv[ASIZE];
  double alphah[ASIZE];
  double alphav[ASIZE];
} TRCOR_PARAM;

static struct {
  int enable_trcor;
  char *makepifile, o_makepifile[PIL_LINESIZE];
} com;

static char *instrume, o_instrume[FLEN_VALUE];
static TRCOR_PARAM trcor;

/***********************************************************
  ChargeTrail_h = cti_ph1500_h *(Ph[0]/1500)^(alpha-1)
  delta_ph_h = CTI_h * (rawx+offset_rawx)
 ***********************************************************/
static double
trailfraction_h(double cti_ph1500_h, double alpha_h,
		int offset_rawx, double ph_preceding, int rawx)
{
  double cti_h, fraction_h;

  if ( 0.0 < ph_preceding ) {
    cti_h = cti_ph1500_h * pow(ph_preceding/1500.0, alpha_h - 1.0);
    fraction_h = cti_h * (rawx + offset_rawx);
  } else {
    fraction_h = 0.0;
  }

  return fraction_h;
}

static double
trailfraction_v(double cti_ph1500_v, double alpha_v,
		int offset_acty, double ph_preceding, int acty)
{
  double cti_v, fraction_v;

  if ( 0.0 < ph_preceding ) {
    cti_v =  cti_ph1500_v * pow(ph_preceding/1500.0, alpha_v - 1.0);
    fraction_v = cti_v *  (acty + offset_acty);
  } else {
    fraction_v = 0.0;
  }

  return fraction_v;
}

static void
xis_trcor(TRCOR_PARAM *tp, int segment, int rawx, int acty, double *ph)
{
  int ipos;
  int off_rawx, off_acty;
  double phtrail_h, phtrail_v;
  double trailh, trailv, alphah, alphav;

  segment &= 3;
  rawx &= 255;
  acty &= 1023;

  ipos = segment * XISactiveSegmentHsize + rawx;
  trailh = tp->trailh[ipos];
  trailv = tp->trailv[ipos];
  alphah = tp->alphah[ipos];
  alphav = tp->alphav[ipos];
  off_rawx = tp->off_rawx[ipos];
  off_acty = tp->off_acty[ipos];

  anl_msg_debug("%e %e %e %e %d %d\n",
	trailh, trailv, alphah, alphav, off_rawx, off_acty);

                   /**********************************************************/
                   /* Charge trail from the preceding pixels to center pixel */
                   /**********************************************************/
  phtrail_h = ph[4] * trailfraction_h(trailh, alphah, off_rawx, ph[4], rawx-1);
  phtrail_v = ph[2] * trailfraction_v(trailv, alphav, off_acty, ph[2], acty-1);
  ph[4] = ph[4] + phtrail_h;
  ph[2] = ph[2] + phtrail_v;
  ph[0] = ph[0] - phtrail_h - phtrail_v;

                   /**********************************************************/
                   /* Charge trail from the center pixel to following pixels */
                   /**********************************************************/
  phtrail_h = ph[0] * trailfraction_h(trailh, alphah, off_rawx, ph[0], rawx);
  phtrail_v = ph[0] * trailfraction_v(trailv, alphav, off_acty, ph[0], acty);
  ph[0] = ph[0] + phtrail_h + phtrail_v;
  ph[5] = ph[5] - phtrail_h;
  ph[7] = ph[7] - phtrail_v;
}

static int
read_trcor_param(char *extname, double obstime, TRCOR_PARAM *tp)
{
  int i, a;
  int col_time, col_th, col_tv, col_ah, col_av, col_x, col_y;
  long irow, nrows;
  char *k, caldb_instrume[FLEN_VALUE];

  fitsfile *fp = NULL;
  int istat = 0;

  if ( fits_open_file(&fp, com.makepifile, READONLY, &istat) ) {
    anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, com.makepifile, istat);
    goto quit;
  }
  if ( fits_movnam_hdu(fp, BINARY_TBL, extname, 0, &istat) ) {
    anl_msg_error("\
%s: fits_movnam_hdu('%s') failed (%d)\n", pname, extname, istat);
    goto quit;
  }
  if (
fits_read_key_lng(fp, k="NAXIS2", &nrows, NULL, &istat) ||
fits_read_key_str(fp, k="INSTRUME", caldb_instrume, NULL, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  if ( 0 != strcmp(instrume, caldb_instrume) ) {
    anl_msg_error("\
%s: INSTRUME keyword (%s) is inconsistent with CALDB (%s).\n",
	pname, instrume, caldb_instrume);
    istat = -1;
    goto quit;
  }
  istat = xisrsp_read_format_version(fp, extname, &tp->format_version);
  if ( istat ) {
    goto quit;
  }

  if ( 1 != tp->format_version ) {
    anl_msg_error("\
%s: unknown FORMAT_VERSION=%d for %s\n", pname, tp->format_version, extname);
    istat = -1;
    goto quit;
  }

  irow = xisrsp_seek_valid_row(fp, obstime);
  anl_msg_info("\
reading %s at %ld-th row\n", extname, irow);

  if (
fits_get_colnum(fp, CASESEN, k="TIME",       &col_time, &istat) ||
fits_get_colnum(fp, CASESEN, k="TrailH",     &col_th,   &istat) ||
fits_get_colnum(fp, CASESEN, k="TrailV",     &col_tv,   &istat) ||
fits_get_colnum(fp, CASESEN, k="AlphaH",     &col_ah,   &istat) ||
fits_get_colnum(fp, CASESEN, k="AlphaV",     &col_av,   &istat) ||
fits_get_colnum(fp, CASESEN, k="OffsetRAWX", &col_x,    &istat) ||
fits_get_colnum(fp, CASESEN, k="OffsetACTY", &col_y,    &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum(%s) failed (%d)\n", pname, k, istat);
    goto quit;
  }

  if (
fits_read_col_dbl(fp, i=col_time, irow, 1, 1, 0.0, &tp->caltime, &a, &istat) ||
fits_read_col_dbl(fp, i=col_th, irow, 1, ASIZE, 0.0, tp->trailh, &a, &istat) ||
fits_read_col_dbl(fp, i=col_tv, irow, 1, ASIZE, 0.0, tp->trailv, &a, &istat) ||
fits_read_col_dbl(fp, i=col_ah, irow, 1, ASIZE, 0.0, tp->alphah, &a, &istat) ||
fits_read_col_dbl(fp, i=col_av, irow, 1, ASIZE, 0.0, tp->alphav, &a, &istat) ||
fits_read_col_int(fp, i=col_x, irow, 1, ASIZE, 0, tp->off_rawx, &a, &istat) ||
fits_read_col_int(fp, i=col_y, irow, 1, ASIZE, 0, tp->off_acty, &a, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n",
	pname, i, irow, istat);
    goto quit;
  }

  tp->caltime_prev = tp->caltime_next = tp->caltime;

  if ( 1 < irow ) {
    fits_read_col_dbl(fp, col_time, irow-1, 1, 1, 0.0,
	&tp->caltime_prev, &a, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col('TIME') failed at irow=%ld (%d)\n", pname, irow, istat);
      goto quit;
    }
  }

  if ( irow < nrows ) {
    fits_read_col_dbl(fp, col_time, irow+1, 1, 1, 0.0,
	&tp->caltime_next, &a, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col('TIME') failed at irow=%ld (%d)\n", pname, irow, istat);
      goto quit;
    }
  }

  if ( fits_close_file(fp, &istat) ) {
    anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
    goto quit;
  }

  tp->last_obstime = obstime;
  tp->extname = extname;

  return 0;

 quit:
  if ( NULL != fp ) {
    int istat2 = 0;
    fits_close_file(fp, &istat2);	/* ignore error */
  }
  return istat;
}

static int
check_caltime(double obstime, TRCOR_PARAM *p)
{
  double t0, t1;

  if ( obstime == p->last_obstime ) {
    return 0;
  }

  if ( 0.0 == obstime ) {	/* ignore invalid TIME assignment */
    return 0;
  }

  p->last_obstime = obstime;
  t0 = (p->caltime + p->caltime_prev) / 2;	/* previous boundary */
  t1 = (p->caltime + p->caltime_next) / 2;	/* next boundary */

  if ( t0 < obstime && obstime <= t1 ) {
    return 0;	/* valid row */
  }

  if ( obstime <= t0 && p->caltime == p->caltime_prev ) {
    return 0;	/* no previous row */
  }

  if ( t1 < obstime && p->caltime == p->caltime_next ) {
    return 0;	/* no next row */
  }

  return read_trcor_param(p->extname, obstime, p);
}

static void
showParam(void)
{
  printf("\nANL:  *** %s show parameter ***\n\n", pname);
  printf("%20s   %s\n", "ENABLE_TRCOR", com.enable_trcor ? "YES" : "NO");
  printf("%20s   '%s'%s\n", "MAKEPIFILE", com.makepifile,
	com.makepifile == com.o_makepifile ? "" : " (CALDB)");
  printf("\n");
}

void
XIStrailCorrection_startup(int *status)
{
  com.makepifile = strcpy(com.o_makepifile, "CALDB");

  *status = ANL_OK;
}

void
XIStrailCorrection_com(int *status)
{
  static char *keytbl[] = {
    "SHOW",
    "ENABLE_TRCOR",
    "MAKEPIFILE",
    "EXIT"
  };
  static char *help[] = {
    "Show current setting",
    "flag to enable charge trail correction",
    "flag to use random number",
    "CALDB file for charge trail parameters",
    "Exit from this menu"
  };
  static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
  char *k, *key;
  int ans[2];

  if ( *status ) {	/* ftools */
    if (
PILGetBool (k="enable_trcor", &com.enable_trcor) ||
PILGetFname(k="makepifile", com.o_makepifile) ||
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
    if ( 0 == strcmp("MAKEPIFILE", key) ) {
      CLtxtrd(key, com.makepifile, sizeof(com.o_makepifile));
    } else if ( 0 == strcmp("ENABLE_TRCOR", key) ) {
      CLlogrd(key, &com.enable_trcor);
    } else if ( 0 == strcmp("SHOW", key) ) {
      showParam ();
    } else if ( 0 == strcmp("EXIT", key) ) {
      break;
    }
  }

  *status = ANL_OK;
}

void
XIStrailCorrection_init(int *status)
{
  static char *extname = CHTR_EXTENSION_NAME;

  int used;
  double obstime, tstart, tstop;

  int istat = 0;

  BnkGet("XIS:INSTRUME", sizeof(o_instrume), &used, &o_instrume);
  instrume = o_instrume + 1;	/* remove beginning "'" */
  com.makepifile = xisrsp_get_caldb_file(instrume, extname, com.o_makepifile);
  if ( NULL == com.makepifile ) {
    goto quit;
  }
  showParam();

  BnkfGetM("XIS:TSTART", sizeof(tstart), &used, &tstart);
  BnkfGetM("XIS:TSTOP", sizeof(tstop), &used, &tstop);
  obstime = tstart;

  istat = read_trcor_param(extname, obstime, &trcor);
  if ( istat ) {
    goto quit;
  }

  EvsDef("XIStrailCorrection:BEGIN");
  EvsDef("XIStrailCorrection:ENTRY");
  EvsDef("XIStrailCorrection:OK");

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XIStrailCorrection_his(int *status)
{
  *status = ANL_OK;
}

void
XIStrailCorrection_bgnrun(int *status)
{
  EvsfSetM("XIStrailCorrection:BEGIN");

  *status = ANL_OK;
}

void
XIStrailCorrection_ana(int nevent, int eventid, int *status)
{
  double obstime, ph[XISoneEventPixelTotNo5x5];
  int istat, used, num_pixel, editmode, segment, rawx, acty, j;

  EvsfSetM("XIStrailCorrection:ENTRY");

  if ( 0 == com.enable_trcor ) {	/* do nothing */
    *status = ANL_OK;
    return;
  }

  BnkfGetM("XIS:EDITMODE", sizeof(int), &used, &editmode);

  switch (editmode) {
  case XISedit5x5:
    num_pixel = XISoneEventPixelTotNo5x5;

/* 5x5 pixel order / top left = readout node
        9  10  11  12   13
       14   1   2   3   15
       16   4   C   5   17
       18   6   7   8   19
       20  21  22  23   24
*/
    break;

  case XISedit3x3:
    num_pixel = XISoneEventPixelTotNo3x3;

/* 3x3 pixel order / top left = readout node
       1   2   3
       4   C   5
       6   7   8
*/
      break;

  default:
    *status = ANL_OK;
    return;
  }

  BnkfGetM("XIS:TIME", sizeof(double), &used, &obstime);
  BnkfGetM("XIS:RAWX", sizeof(int), &used, &rawx);
  BnkfGetM("XIS:ACTY", sizeof(int), &used, &acty);
  BnkfGetM("XIS:SEGMENT", sizeof(int), &used, &segment);
  if ( segment < 0 || XIStotalSegNo <= segment ) {
    anl_msg_error("\
%s: illegal segment id %d\n", pname, segment);
    *status = ANL_QUIT;
    return;
  }

  BnkfGetM("XIS:PHASCORR", sizeof(double) * num_pixel, &used, ph);
  if ( ANL_MSG_DEBUG <= anl_msg_chatter() ) {
    anl_msg_debug("before TrailCorr");
    for (j = 0; j < num_pixel; j++) {
      anl_msg_debug(" %.1f", ph[j]);
    }
    anl_msg_debug("\n");
  }

  istat = check_caltime(obstime, &trcor);
  if ( istat ) {
    *status = ANL_ERROR;
    return;
  }
  xis_trcor(&trcor, segment, rawx, acty, ph);

  if ( ANL_MSG_DEBUG <= anl_msg_chatter() ) {
    anl_msg_debug("after TrailCorr");
    for (j = 0; j < num_pixel; j++) {
      anl_msg_debug(" %.1f", ph[j]);
    }
    anl_msg_debug("\n");
  }

  BnkfPutM("XIS:PHASCORR", sizeof(double) * num_pixel, ph);

  EvsfSetM("XIStrailCorrection:OK");
  *status = ANL_OK;
}

void
XIStrailCorrection_endrun(int *status)
{
  fitsfile *fp;			/* for output evt fits */
  char buf[2*PIL_LINESIZE];
  int used = 0, istat = 0;

                          /*************************************************/
                          /* Write a HISTORY keyword into output fits file */
                          /*************************************************/
  BnkGet("XIS:OUTFITS:EVENTS:PTR", sizeof(fp), &used, &fp);
  if ( used != sizeof(fp) ) {
    *status = ANL_OK;
    return;
  }

  sprintf(buf, "\
  enable_trcor=%s", com.enable_trcor ? "yes" : "no");
  fits_write_history(fp, buf, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

void
XIStrailCorrection_exit(int *status)
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
