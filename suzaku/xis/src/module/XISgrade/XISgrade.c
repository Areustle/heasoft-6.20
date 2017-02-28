/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:50:14 1999 by E. Miyata*/
/* xis_module/XISgrade/0.0_fast/XISgrade_v0.0.c: modified by M.Ozaki on Fri Aug 13 21:04:53 1999:
 *   - replace BnkGet, BnkPut, EvsGet, ...
 *     to BnkfGetM, BnkfPutM, EvsfGetM, ... .
 */
/*************************************

  XISgrade

  1999/07/24	Emi Miyata
	based on XISascaGrade
	BNKput XIS:PHA, XIS:GRADE
  ver 0.1    1999.10.20 Emi Miyata
	include fast version
  ver0.2      ?
  ver 0.3    2005.01.05 K. Hayashida
	compatible with gcc ver3 in Linux
  ver 0.4    2005.03.08 K. Hayashida
	New Feature: Energy dependent spth for XIS-BI is introduced
  ver 0.5    2005.03.09 K. Hayashida
	spth = spth_min + slope * log10(PHA) + offset
  ver 0.6    2005.06.14 H. Nakajima
	input gain parameters from fits file
  ver 0.7    2005.06.21 H. Nakajima
	add an explicit branch about editmode
  ver 0.8    2005.06.22 H. Nakajima
	insure the filename length of PIL_LINESIZE
  ver 0.9    2005.08.03 K.Hayashida
	1) sensor,segment are defined not only for 5x5, but also 3x3 2x2.
	2) constant_spth is introduced as an option.
	if flag_constant_spth is ANL_TRUE constant_spth is used,
	otherwise phadependent spth is employed according to
	the parameters written in spthfile.
  ver 0.91    2005.08.16 H.Yamaguchi
	2x2 mode BNK name corrected
  ver 1.0    2005.10.04 H. Nakajima
	change the format of CALDB file
	in 2x2 mode, set *ph to be zero and BnkGet PHAS
	before classifying Grade
	use PHASCORR if Evs("XISpreparePHASCORR:OK")
	leave history comments to the output file
  ver 1.1    2005.12.05 Hiroshi Nakajima
	change how to leave HISTORY (utilize aeFitsHeaderUtil)
	transparent when the edit mode is Frame, DarkInit,
	DarkUpdate, and DarkFrame mode
  ver 1.2    2005.12.07 Hiroshi Nakajima
	seek proper caltime correctly
  ver 1.3    2006.01.25 Hiroshi Nakajima
	set buffer length to "PIL_LINESIZE" in endrun
  ver 1.4    2006.02.06 Hiroshi Nakajima
	set buffer length to "2*PIL_LINESIZE" in endrun
  ver 1.5    2006.08.22 Y.ISHISAKI
	use XIS:TSTART,TSTOP instead of XIS:DATE-OBS,TIME-OBS
	use anl_msg_***() functions for messages
  ver 2.0    2006.10.31 Y.ISHISAKI
	change parameter type, flag_constant_spth (i) -> (b)
	change parameter name, grade_caldbfile -> makepifile
	support CALDB for makepifile
	use xisrsp_get_caldb_file(), xisrsp_seek_valid_row()
  ver 2.1 2007.01.31 Y.ISHISAKI
	use aefits_write_module_history() in XISeditEventFits
  ver 2.2 2007.04.20 Y.ISHISAKI
	truncate -1000 <= PHA && PHA <= 4095 in _ana()
  ver 3.0 2007.04.30 Y.ISHISAKI
	calculate in floating point, fill "XIS:PHA:DOUBLE"
  ver 3.1 2007.05.14 Y.ISHISAKI
	use PHAS_INFO, add xis_sumph(), read_spth_param(), check_caltime()
  ver 3.2 2007.05.30 Y.ISHISAKI
	BnkGet "XIS:SEGMENT" in XISgrade_ana()
  ver 3.3 2008.04.10 Y.ISHISAKI
	bug fix in check_caltime(), where rejecting obstime == 0.0
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
#include "xisGradeUtil.h"
#include "xisTelemFormat.h"
#include "xisRespUtil.h"
#include "aeFitsHeaderUtil.h"

#define  SPTH_EXTENSION_NAME	"SPTH_PARAM"

static char pname[] = "XISgrade";
char XISgrade_version[] = "version 3.3";

/* array parameters for PHA dependent spth */
#define ASIZE	XIStotalSegNo
typedef struct {
  int format_version;
  char *extname;
  double last_obstime;
  double caltime, caltime_prev, caltime_next;
  double offset[ASIZE];
  double slope[ASIZE];
  double minimum[ASIZE];
} SPTH_PARAM;

typedef struct {
/* input */
  double phas[XISoneEventPixelTotNo5x5];
  int segment, editmode;
  int p_outer, sum_outer;	/* 3x3 specific */
  int pos2x2, p_adj;		/* 2x2 specific */
/* output */
  double sumph;
  int type, above;
} PHAS_INFO;

static struct {
  int flag_constant_spth;	/* if true, constant_spth is used */
  int constant_spth;
  char *makepifile, o_makepifile[PIL_LINESIZE];
} com;

static char *instrume, o_instrume[FLEN_VALUE];
static SPTH_PARAM spth;

      /**********************************************************************/
      /* Split-threshold as a function of log10(PHA) 2005/03/09 K.Hayashida */
      /**********************************************************************/
static double
split_threshold(double spth_min,	/* minimum allowed value of spth */
		double spth_slope,	/* slope of spth vs log10(PHA[ADU]) */
		double spth_offset,	/* offset of spth vs log10(PHA[ADU]) */
		double pha) 		/* PHA [ADU] */
{
  double spth = 0.0;

  if ( 0 < pha ) {
    spth = spth_min + spth_slope * log10(pha) + spth_offset;
  }

  if ( spth < spth_min ) {
    spth = spth_min;
  }

  return spth;
}

static void
xis_sumph(SPTH_PARAM *sp, PHAS_INFO *pp)
{
  double sumph, *ph;
  double spth_slope, spth_offset, spth_min;
  double spth_tmp, spth_use;
  int editmode, segment, type, above;
  int p_outer, sum_outer;	/* 3x3 specific */
  int pos2x2, p_adj;		/* 2x2 specific */

  editmode = pp->editmode;
  ph = pp->phas;

  if ( com.flag_constant_spth ) {
    spth_use = com.constant_spth;
    switch ( editmode ) {
    case XISedit5x5:
      classify_5x5(ph, spth_use, &sumph, &type, &above);
      break;
    case XISedit3x3:
      p_outer = pp->p_outer;
      sum_outer = pp->sum_outer;
      classify_3x3(ph, p_outer, sum_outer, spth_use, &sumph, &type, &above);
      break;
    case XISedit2x2:
      pos2x2 = pp->pos2x2;
      p_adj = pp->p_adj;
      classify_2x2(ph, pos2x2, p_adj, spth_use, &sumph, &type, &above);
      break;
    default:
      ;
    }
  } else {
    segment = pp->segment;
    spth_slope = sp->slope[segment];
    spth_offset = sp->offset[segment];
    spth_min = sp->minimum[segment];

               /*************************************************************/
               /* 1st trial to evaluate sumph for split thresold to be used */
               /*************************************************************/
    spth_tmp = spth_min;
    switch ( editmode ) {
    case XISedit5x5:
      classify_5x5(ph, spth_tmp, &sumph, &type, &above);
      spth_use = split_threshold(spth_min, spth_slope, spth_offset, sumph);
      classify_5x5(ph, spth_use, &sumph, &type, &above);
      break;
    case XISedit3x3:
      p_outer = pp->p_outer;
      sum_outer = pp->sum_outer;
      classify_3x3(ph, p_outer, sum_outer, spth_tmp, &sumph, &type, &above);
      spth_use = split_threshold(spth_min, spth_slope, spth_offset, sumph);
      classify_3x3(ph, p_outer, sum_outer, spth_use, &sumph, &type, &above);
      break;
    case XISedit2x2:
      pos2x2 = pp->pos2x2;
      p_adj = pp->p_adj;
      classify_2x2(ph, pos2x2, p_adj, spth_tmp, &sumph, &type, &above);
      spth_use = split_threshold(spth_min, spth_slope, spth_offset, sumph);
      classify_2x2(ph, pos2x2, p_adj, spth_use, &sumph, &type, &above);
      break;
    default:
      ;
    }
  }

  pp->type = type;
  pp->above = above;
  pp->sumph = sumph;
}

static int
read_spth_param(char *extname, double obstime, SPTH_PARAM *sp)
{
  int icol, anul;
  int col_time, col_offset, col_slope, col_minimum;
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
  istat = xisrsp_read_format_version(fp, extname, &sp->format_version);
  if ( istat ) {
    goto quit;
  }
  if ( 1 != sp->format_version ) {
    anl_msg_error("\
%s: unknown FORMAT_VERSION=%d for %s\n", pname, sp->format_version, extname);
    istat = -1;
    goto quit;
  }

  irow = xisrsp_seek_valid_row(fp, obstime);
  anl_msg_info("\
reading %s at %ld-th row\n", extname, irow);

  if (
fits_get_colnum(fp, CASESEN, k="TIME", &col_time, &istat) ||
fits_get_colnum(fp, CASESEN, k="OFFSET", &col_offset, &istat) ||
fits_get_colnum(fp, CASESEN, k="SLOPE", &col_slope, &istat) ||
fits_get_colnum(fp, CASESEN, k="MINIMUM", &col_minimum, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum(%s) failed (%d)\n", pname, k, istat);
    goto quit;
  }

  if (
fits_read_col_dbl(fp, icol=col_time, irow, 1,
		  1, 0.0, &sp->caltime, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_offset, irow, 1,
		  ASIZE, 0.0, sp->offset, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_slope, irow, 1,
		  ASIZE, 0.0, sp->slope, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_minimum, irow, 1,
		  ASIZE, 0.0, sp->minimum, &anul, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n",
	pname, icol, irow, istat);
    goto quit;
  }

  sp->caltime_prev = sp->caltime_next = sp->caltime;

  if ( 1 < irow ) {
    fits_read_col_dbl(fp, col_time, irow-1, 1, 1, 0.0,
	&sp->caltime_prev, &anul, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col('TIME') failed at irow=%ld (%d)\n", pname, irow, istat);
      goto quit;
    }
  }

  if ( irow < nrows ) {
    fits_read_col_dbl(fp, col_time, irow+1, 1, 1, 0.0,
	&sp->caltime_next, &anul, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col('TIME') failed at irow=%ld (%d)\n", pname, irow, istat);
      goto quit;
    }
  }

  fits_close_file(fp, &istat);
  fp = NULL;
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
    goto quit;
  }

  sp->last_obstime = obstime;
  sp->extname = extname;

  return 0;

 quit:
  if ( NULL != fp ) {
    int istat2 = 0;
    fits_close_file(fp, &istat2);	/* ignore error */
  }
  return istat;
}

static int
check_caltime(double obstime, SPTH_PARAM *p)
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

  return read_spth_param(p->extname, obstime, p);
}

static void
showParam(void)
{
  printf ("\nANL:  *** %s show parameter ***\n\n", pname);
  printf("%20s   %s\n", "FLAG_CONSTANT_SPTH",
	 com.flag_constant_spth ? "YES" : "NO");
  if( com.flag_constant_spth ) {
    printf("%20s   %d\n", "CONSTANT_SPTH", com.constant_spth);
  } else {
    printf("%20s   '%s'%s\n", "MAKEPIFILE", com.makepifile,
	com.makepifile == com.o_makepifile ? "" : " (CALDB)");
  }
  printf("\n");
}

void
XISgrade_startup(int *status)
{
  com.flag_constant_spth = ANL_FALSE;  /* pha dependent spth as default */
  com.constant_spth = 20; /* default contant_spth = 20 (ADU) */
  com.makepifile = strcpy(com.o_makepifile, "CALDB");

  *status = ANL_OK;
}

void
XISgrade_com(int *status)
{
  static char *keytbl[] = {
    "FLAG_CONSTANT_SPTH",
    "CONSTANT_SPTH",
    "MAKEPIFILE",
    "SHOW",
    "EXIT"
  };
  static char *help[] = {
    "Flag to use constant Spth",
    "Constant Spth Value ",
    "CALDB file for Spth parameters",
    "Show current setting",
    "Exit from this menu"
  };
  static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
  char *k, *key;
  int ans[2];

  if ( *status ) {
    if (
PILGetBool (k="flag_constant_spth", &com.flag_constant_spth) ||
PILGetInt  (k="constant_spth", &com.constant_spth) ||
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
    if ( 0 == strcmp("FLAG_CONSTANT_SPTH", key) ) {
      CLlogrd(key, &com.flag_constant_spth);
    } else if ( 0 == strcmp("CONSTANT_SPTH", key) ) {
      CLintrd(key, &com.constant_spth);
    } else if ( 0 == strcmp("MAKEPIFILE", key) ) {
      CLtxtrd(key, com.makepifile, sizeof(com.o_makepifile));
    } else if ( 0 == strcmp("SHOW", key) ) {
      showParam();
    } else if ( 0 == strcmp("EXIT", key) ) {
      return;
    }
  }

  *status = ANL_OK;
}

void
XISgrade_init(int *status)
{
  static char extname[] = SPTH_EXTENSION_NAME;

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

  istat = read_spth_param(extname, obstime, &spth);
  if ( istat ) {
    goto quit;
  }

  EvsDef("XISgrade:BEGIN");
  EvsDef("XISgrade:ENTRY");
  EvsDef("XISgrade:OK");

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XISgrade_his(int *status)
{
  *status = ANL_OK;
}

void
XISgrade_bgnrun(int *status)
{

  EvsfSetM("XISgrade:BEGIN");

  *status = ANL_OK;
}

void
XISgrade_ana(int nevent, int eventid, int *status)
{
  double obstime;
  PHAS_INFO info;
  int used, num_pixel;

  EvsfSetM("XISgrade:ENTRY");
  BnkfGetM("XIS:SEGMENT", sizeof(int), &used, &info.segment);
  BnkfGetM("XIS:EDITMODE", sizeof(int), &used, &info.editmode);

  switch (info.editmode) {

  case XISedit5x5:
    num_pixel = XISoneEventPixelTotNo5x5;
    break;

  case XISedit3x3:
    num_pixel = XISoneEventPixelTotNo3x3;
    BnkfGetM("XIS:P_OUTER_MOST", sizeof(int), &used, &info.p_outer);
    BnkfGetM("XIS:SUM_OUTER_MOST", sizeof(int), &used, &info.sum_outer);
    break;

  case XISedit2x2:
    num_pixel = XISoneEventPixelTotNo2x2;
    BnkfGetM("XIS:POS2x2", sizeof(int), &used, &info.pos2x2);
    BnkfGetM("XIS:PADJACENT", sizeof(int), &used, &info.p_adj);
    break;

  default:
    *status = ANL_OK;
    return;
  }

  BnkfGetM("XIS:TIME", sizeof(double), &used, &obstime);

  if ( check_caltime(obstime, &spth) ) {
    *status = ANL_ERROR;
    return;
  }

  BnkfGetM("XIS:PHASNOCTI", sizeof(double) * num_pixel, &used, info.phas);
  xis_sumph(&spth, &info);
  BnkfPutM("XIS:PHANOCTI:DOUBLE", sizeof(double), &info.sumph);

  BnkfGetM("XIS:PHASCORR", sizeof(double) * num_pixel, &used, info.phas);
  xis_sumph(&spth, &info);
  BnkfPutM("XIS:PHA:DOUBLE", sizeof(double), &info.sumph);
  BnkfPutM("XIS:GRADE", sizeof(int), &info.type);

  EvsfSetM("XISgrade:OK");
  *status = ANL_OK;
}

void
XISgrade_endrun(int *status)
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

  if ( com.flag_constant_spth ) {
    sprintf(buf, "\
  flag_constant_spth=yes  constant_spth=%d", com.constant_spth);
    fits_write_history(fp, buf, &istat);
  } else {
    sprintf(buf, "\
  flag_constant_spth=no");
    fits_write_history(fp, buf, &istat);
  }
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

void
XISgrade_exit(int *status)
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
