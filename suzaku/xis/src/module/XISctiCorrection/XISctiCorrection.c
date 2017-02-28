/*************************************

  XISctiCorrection
     Correction charge loss due to charge tranfer inefficiency

  ver 0.0    2005.06.06 Hiroshi Nakajima
	input gain parameters from fits file
  ver 0.1    2005.06.21 Hiroshi Nakajima
	add an explicit branch about editmode
  ver 0.2    2005.06.21 Hiroshi Nakajima
	insure the filename length of PIL_LINESIZE
  ver 0.3    2005.07.27 Hiroshi Nakajima
	change the format of CALDB file
  ver 0.4    2005.10.04 Hiroshi Nakajima
	change the format of CALDB file
	use RAWX and ACTY
	use PHASCORR if Evs("XISpreparePHASCORR:OK")
	leave history comments to the output file
  ver 0.5    2005.11.03 Hiroshi Nakajima
	change how to leave HISTORY (utilize aeFitsHeaderUtil)
	transparent when the edit mode is Frame, DarkInit,
	DarkUpdate, and DarkFrame mode
  ver 0.6    2005.12.06 Hiroshi Nakajima
	change xis_corcti
  ver 0.7    2006.01.25 Hiroshi Nakajima
	set buffer length to "PIL_LINESIZE" in endrun
  ver 0.8    2006.02.06 Hiroshi Nakajima
	set buffer length to "2*PIL_LINESIZE" in endrun
  ver 0.9    2006.04.20 Hiroshi Nakajima
	correct the position of radamizing
  ver 1.0    2006.06.09 Hironori Matsumoto
	include <time.h>
  ver 1.1    2006.07.22 Hiroshi Nakajima
	introduce Frame-Store transfer component
	in parallel transfer
  ver 1.2    2006.08.22 Y.ISHISAKI
	use XIS:TSTART,TSTOP instead of XIS:DATE-OBS,TIME-OBS
	use anl_msg_***() functions for messages
  ver 1.3    2006.08.25 Hiroshi Nakajima
	terminate with error except for the following cases:
	ther's no CBD10001 keyword, or
	CBD10001 is "FORMAT_VERSION(1)", or
	CBD10001 is "FORMAT_VERSION(2)"
  ver 2.0    2006.10.31 Y.ISHISAKI
	change parameter name, cticor_rand_seed/skip -> rand_seed/skip
	change parameter name, cticor_flag_random (i) -> cticor_flag_rand (b)
	change parameter name, cticor_caldbfile -> makepifile
	support CALDB for makepifile
	use xisrsp_get_caldb_file(), xisrsp_seek_valid_row()
	add corph argument to xis_corcti()
  ver 2.1 2007.01.31 Y.ISHISAKI
	use aefits_write_module_history() in XISeditEventFits
  ver 2.2 2007.04.11 H.Nakajima
	adopt to the new CALDB file which has extensions for Window option
  ver 3.0 2007.04.30 Y.ISHISAKI
	add 'enable_cticor', 'enable_scicti' parameter
	support 2x2, 4WIN, 8WIN, SCI makepi extensions
	SCI CTI correction, in xis_cticor()
  ver 3.1 2007.05.03 Y.ISHISAKI
	fix CTI correction when window option in xis_cticor()
	copy "XIS:PHASCORR" to "XIS:PHASNOCTI" in _ana()
  ver 3.2 2007.05.05 Y.ISHISAKI
	use rawy information in xis_cticor() for strict treatment
  ver 3.3 2007.05.30 Y.ISHISAKI
	bug fix of handling pos2x2 in xis_cticor()
  ver 3.4 2008.04.10 Y.ISHISAKI
	bug fix in check_caltime(), where rejecting obstime == 0.0
	set initial obstime = tstart in _init()
  ver 3.5 2008.06.06 Y.ISHISAKI
	bug fix in setting pp->caltime in read_pcti_param()
  ver 3.6 2008.10.06 H.Uchiyama
	Trial Version
	change the number of transfers in window mode
		Parallel CTI:  iacty - win_st + 1
		Frame Store CTI: 1026 + win_st
	simplify SCI-on CTI correction
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
#include "aste_time.h"
#include "aste_rand.h"
#include "xisTelemFormat.h"
#include "xisRespUtil.h"
#include "xisSciUtil.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "XISctiCorrection";
char XISctiCorrection_version[] = "version 3.6";

#define  SCTI_EXT_NAME	        "SERIAL_CTI"
#define  PCTI_EXT_NAME	        "PARALLEL_CTI"
#define  PCTI_4WI_EXT_NAME	"PARALLEL_CTI_4WI"
#define  PCTI_8WI_EXT_NAME	"PARALLEL_CTI_8WI"
#define  PCTI_SCI_EXT_NAME	"PARALLEL_CTI_SCI"
#define  PCTI_4WI_SCI_EXT_NAME	"PARALLEL_CTI_4WI_SCI"
#define  PCTI_8WI_SCI_EXT_NAME	"PARALLEL_CTI_8WI_SCI"

/* array parameters for CTI parameters */
#define SSIZE	(XIStotalSegNo)
typedef struct {
  int format_version;
  char *extname;
  double last_obstime;
  double caltime, caltime_prev, caltime_next;
  double cti_const[SSIZE];
  double cti_norm[SSIZE];
  double cti_pow[SSIZE];
} SCTI_PARAM;

#define PSIZE	(XIStotalSegNo * XISactiveSegmentHsize)
typedef struct {
  int format_version;
  int sci_period;		/* used for format_version = 3 */
  char *extname;
  double last_obstime;
  double caltime, caltime_prev, caltime_next;
  double cti_pow[PSIZE];	/* used for format_version = 1, 2, 3 */
  double cti_const[PSIZE];	/* used for format_version = 1, 2 */
  double cti_norm[PSIZE];	/* used for format_version = 1, 2 */
  double ctifs_norm[PSIZE];	/* used for format_version = 2, 3 */
  double *cti_norm_u;		/* used for format_version = 3 */
  double *cti_norm_l;		/* used for format_version = 3 */
  double dq_norm[PSIZE];	/* used for format_version = 3 */
} PCTI_PARAM;

static struct {
  int enable_cticor;
  int enable_scicti;
  char *makepifile, o_makepifile[PIL_LINESIZE];
} com;

static char *instrume, o_instrume[FLEN_VALUE];
static char *scti_extname = SCTI_EXT_NAME;
static char *pcti_extname = PCTI_EXT_NAME;
static SCTI_PARAM scti;
static PCTI_PARAM pcti;
static SCI_PARAM sci;

static char *
decide_pcti_extname(int winopt, int ci)
{
  char *extname = NULL;

  if ( 0 == ci ) {	/* CI is off */

    if ( winopt == XISwindowOff ) {
      extname = PCTI_EXT_NAME;
      anl_msg_debug("\
Winsow option is %s: reading %s extension\n", XISwindowOffSName, extname);
    } else if ( winopt == XISwindow4 ) {
      extname = PCTI_4WI_EXT_NAME;
      anl_msg_debug("\
%s window observation: reading %s extension\n", XISwindow4SName, extname);
    } else if ( winopt == XISwindow8 ) {
      extname = PCTI_8WI_EXT_NAME;
      anl_msg_debug("\
%s window observation: reading %s extension\n", XISwindow8SName, extname);
    } else if ( winopt == XISwindow16 ) {
      anl_msg_error("\
%s: non supported window option (%s window).\n", pname, XISwindow16SName);
    } else {
      anl_msg_error("\
%s: illegal window option %d\n", pname, winopt);
    }

  } else if ( 2 == ci || 3 == ci ) {	/* SCI is on */

    if ( winopt == XISwindowOff ) {
      extname = PCTI_SCI_EXT_NAME;
      anl_msg_debug("\
SCI is on, Winsow option is %s: reading %s extension\n",
	XISwindowOffSName, extname);
    } else if ( winopt == XISwindow4 ) {
      extname = PCTI_4WI_SCI_EXT_NAME;
      anl_msg_debug("\
SCI is on, %s window observation: reading %s extension\n",
	XISwindow4SName, extname);
    } else if ( winopt == XISwindow8 ) {
      extname = PCTI_8WI_SCI_EXT_NAME;
      anl_msg_debug("\
SCI is on, %s window observation: reading %s extension\n",
	XISwindow8SName, extname);
    } else if ( winopt == XISwindow16 ) {
      anl_msg_error("\
%s: non supported window option (%s window).\n", pname, XISwindow16SName);
    } else {
      anl_msg_error("\
%s: illegal window option %d\n", pname, winopt);
    }

  } else if ( 1 == ci ) {	/* diagnostic CI is on */
    extname = PCTI_EXT_NAME;
    anl_msg_debug("\
%s: diagnostic CI mode: reading %s extension\n", pname, extname);

  } else {
    anl_msg_error("\
%s: illegal CI mode %d\n", pname, ci);
  }

  return extname;
}

static int
read_scti_param(char *extname, double obstime, SCTI_PARAM *sp)
{
  int icol, anul;
  int col_time, col_const, col_norm, col_pow;
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
fits_get_colnum(fp, CASESEN, k="CTI_CONST", &col_const, &istat) ||
fits_get_colnum(fp, CASESEN, k="CTI_NORM", &col_norm, &istat) ||
fits_get_colnum(fp, CASESEN, k="CTI_POW", &col_pow, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum(%s) failed (%d)\n", pname, k, istat);
    goto quit;
  }

  if (
fits_read_col_dbl(fp, icol=col_time, irow, 1,
		  1, 0.0, &sp->caltime, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_const, irow, 1,
		  SSIZE, 0.0, sp->cti_const, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_norm, irow, 1,
		  SSIZE, 0.0, sp->cti_norm, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_pow, irow, 1,
		  SSIZE, 0.0, sp->cti_pow, &anul, &istat) ||
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
read_pcti_param(char *extname, int sci_period, double obstime, PCTI_PARAM *pp)
{
  int i, icol, anul, period;
  int col_time, col_const, col_norm, col_pow, col_fs;
  int col_period, col_norm_u, col_norm_l, col_dq_norm;
  long irow, nrows, validrow, prevrow, nextrow;
  double caltime, refgap, timegap;
  char *k, caldb_instrume[FLEN_VALUE];

  fitsfile *fp = NULL;
  int istat = 0;

  icol = 0;	/* dummy initialization for gcc warning */
  pp->cti_norm_u = pp->cti_const;
  pp->cti_norm_l = pp->cti_norm;

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
  istat = xisrsp_read_format_version(fp, extname, &pp->format_version);
  if ( istat ) {
    goto quit;
  }

  if ( 1 == pp->format_version || 2 == pp->format_version ) {
    irow = xisrsp_seek_valid_row(fp, obstime);
    anl_msg_info("\
reading %s at %ld-th row\n", extname, irow);

    if (
fits_get_colnum(fp, CASESEN, k="TIME", &col_time, &istat) ||
fits_get_colnum(fp, CASESEN, k="CTI_CONST", &col_const, &istat) ||
fits_get_colnum(fp, CASESEN, k="CTI_NORM", &col_norm, &istat) ||
fits_get_colnum(fp, CASESEN, k="CTI_POW", &col_pow, &istat) ||
	0 ) {
      anl_msg_error("\
%s: fits_get_colnum(%s) failed (%d)\n", pname, k, istat);
      goto quit;
    }

    if (
fits_read_col_dbl(fp, icol=col_time, irow, 1,
		  1, 0.0, &pp->caltime, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_const, irow, 1,
		  PSIZE, 0.0, pp->cti_const, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_norm, irow, 1,
		  PSIZE, 0.0, pp->cti_norm, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_pow, irow, 1,
		  PSIZE, 0.0, pp->cti_pow, &anul, &istat) ||
	0 ) {
      anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n",
	pname, icol, irow, istat);
      goto quit;
    }

    if ( 1 == pp->format_version ) {
/* FORMAT_VERSION = 1, no CTIFS_NORM column */
      for (i = 0; i < PSIZE; i++) {
	pp->ctifs_norm[i] = 0.0;
      }
    } else {
      if (
fits_get_colnum(fp, CASESEN, k="CTIFS_NORM", &col_fs, &istat) ||
	   0 ) {
	anl_msg_error("\
%s: fits_get_colnum(%s) failed (%d)\n", pname, k, istat);
	goto quit;
      }
      if (
fits_read_col_dbl(fp, icol=col_fs, irow, 1,
		  PSIZE, 0.0, pp->ctifs_norm, &anul, &istat) ||
	   0 ) {
	anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n",
		pname, icol, irow, istat);
	goto quit;
      }
    }

    pp->caltime_prev = pp->caltime_next = pp->caltime;

    if ( 1 < irow ) {
      fits_read_col_dbl(fp, col_time, irow-1, 1, 1, 0.0,
	&pp->caltime_prev, &anul, &istat);
      if ( istat ) {
	anl_msg_error("\
%s: fits_read_col('TIME') failed at irow=%ld (%d)\n", pname, irow, istat);
	goto quit;
      }
    }

    if ( irow < nrows ) {
      fits_read_col_dbl(fp, col_time, irow+1, 1, 1, 0.0,
	&pp->caltime_next, &anul, &istat);
      if ( istat ) {
	anl_msg_error("\
%s: fits_read_col('TIME') failed at irow=%ld (%d)\n", pname, irow, istat);
	goto quit;
      }
    }
  } else if ( 3 == pp->format_version ) {
    if (
fits_get_colnum(fp, CASESEN, k="TIME", &col_time, &istat) ||
fits_get_colnum(fp, CASESEN, k="SCI_PERIOD_RAWY", &col_period, &istat) ||
fits_get_colnum(fp, CASESEN, k="CTI_NORM_U", &col_norm_u, &istat) ||
fits_get_colnum(fp, CASESEN, k="CTI_NORM_L", &col_norm_l, &istat) ||
fits_get_colnum(fp, CASESEN, k="dQ_NORM", &col_dq_norm, &istat) ||
fits_get_colnum(fp, CASESEN, k="CTI_POW", &col_pow, &istat) ||
fits_get_colnum(fp, CASESEN, k="CTIFS_NORM", &col_fs, &istat) ||
	0 ) {
      anl_msg_error("\
%s: fits_get_colnum(%s) failed (%d)\n", pname, k, istat);
      goto quit;
    }

    validrow = prevrow = nextrow = -1;
    refgap = 0.0;		/* dummy initialization for gcc warning */

    for (irow = 1; irow <= nrows; irow++) {
      if (
fits_read_col_dbl(fp, col_time, irow, 1, 1, 0.0, &caltime, &anul, &istat) ||
fits_read_col_int(fp, col_period, irow, 1, 1, 0, &period, &anul, &istat) ||
	   0 ) {
	anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n",
		pname, icol, irow, istat);
      }
      if ( sci_period != period ) {
	continue;
      }
      if ( -1 == validrow ) {
	validrow = irow;
	pp->caltime = caltime;
	refgap = fabs(obstime - caltime);
      } else {
	timegap = fabs(obstime - caltime);
	if ( timegap < refgap ) {
	  prevrow = validrow;
	  pp->caltime_prev = caltime;
	  nextrow = -1;
	  validrow = irow;
	  pp->caltime = caltime;
	  refgap = timegap;
	} else if ( timegap == refgap ) {
	  if ( caltime <= obstime ) {
	    prevrow = validrow;
	    pp->caltime_prev = caltime;
	    nextrow = -1;
	    validrow = irow;
	    pp->caltime = caltime;
	    refgap = timegap;
	  } else if ( -1 == nextrow ) {
	    nextrow = irow;
	    pp->caltime_next = caltime;
	  }
	} else if ( -1 == nextrow ) {
	  nextrow = irow;
	  pp->caltime_next = caltime;
	}
      }
    }
    if ( -1 == validrow ) {
      anl_msg_error("\
%s: no valid row for SCI_PERIOD_RAWY=%d\n", pname, sci_period);
      istat = -1;
      goto quit;
    }
    irow = validrow;
    if ( -1 == prevrow ) {
      pp->caltime_prev = pp->caltime;
    }
    if ( -1 == nextrow ) {
      pp->caltime_next = pp->caltime;
    }
    anl_msg_info("\
reading %s at %ld-th row\n", extname, irow);

    if (
fits_read_col_dbl(fp, icol=col_norm_u, irow, 1,
		  PSIZE, 0.0, pp->cti_norm_u, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_norm_l, irow, 1,
		  PSIZE, 0.0, pp->cti_norm_l, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_dq_norm, irow, 1,
		  PSIZE, 0.0, pp->dq_norm, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_pow, irow, 1,
		  PSIZE, 0.0, pp->cti_pow, &anul, &istat) ||
fits_read_col_dbl(fp, icol=col_fs, irow, 1,
		  PSIZE, 0.0, pp->ctifs_norm, &anul, &istat) ||
	0 ) {
      anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n",
	pname, icol, irow, istat);
      goto quit;
    }
  } else {
    anl_msg_error("\
%s: unknown FORMAT_VERSION=%d for %s\n", pname, pp->format_version, extname);
    istat = -1;
    goto quit;
  }

  fits_close_file(fp, &istat);
  fp = NULL;
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
    goto quit;
  }

  pp->last_obstime = obstime;
  pp->extname = extname;
  pp->sci_period = sci_period;

  return 0;

 quit:
  if ( NULL != fp ) {
    int istat2 = 0;
    fits_close_file(fp, &istat2);	/* ignore error */
  }
  return istat;
}

static int
xis_cticor(SCTI_PARAM *sp, PCTI_PARAM *pp, SCI_PARAM *sci,
	int segment, int rawx, int rawy, int acty, int pos2x2, int num_pixel,
	double phas[], double corph[])
{
  static int map_2x2[4][4] = {
    { 0, 1, 2, 4 },	/* pos2x2 = 0 */
    { 0, 2, 3, 5 },	/* pos2x2 = 1 */
    { 0, 4, 6, 7 },	/* pos2x2 = 2 */
    { 0, 5, 7, 8 }	/* pos2x2 = 3 */
  };
  static int map_rawx[25] = {
     0, -1,  0, +1, -1,
     1, -1,  0, +1, -2,
    -1,  0, +1, +2, -2,
    +2, -2, +2, -2, +2,
    -2, -1,  0, +1, +2
  };
  static int map_acty[25] = {
     0, -1, -1, -1,  0,
     0, +1, +1, +1, -2,
    -2, -2, -2, -2, -1,
    -1,  0,  0, +1, +1,
    +2, +2, +2, +2, +2
  };
  static int map_segment[4] = {
    1, 0, 3, 2
  };

  int i, j, ipos, iseg, irawx, irawy, iacty, dacty, bacty, space;
  int ns, np, nfs, win_st, win_siz;
  double pha, cor_pha, ctis, ctip, ctifs, dq, dq0, ctip0, spow;
  double scti_const, scti_norm, scti_pow;	/* Serial CTI parameters */
  double pcti_const, pcti_norm, pcti_pow;	/* Parallel CTI parameters */
  double ctifs_norm;				/* Frame Store CTI parameter */
  double norm_u, norm_l, dq_norm;		/* SCI CTI parameters */

    /* 5x5 pixel order / top left = readout node
       9   10  11  12   13
       14   1   2   3   15      =====>RAWX
       16   4   C   5   17      ||
       18   6   7   8   19      ||
       20  21  22  23   24      \/ACTY
    */

  segment &= 3;
  rawx &= 255;
  acty &= 1023;
  space = sci->period_rawy;
  win_st = sci->win_st;		/* window start address */
  win_siz = sci->win_siz;	/* window size */

  for (i = 0; i < num_pixel; i++) {
    if ( pos2x2 < 0 ) {
      j = i;			/* 5x5 or 3x3 */
    } else {
      pos2x2 &= 3;
      j = map_2x2[pos2x2][i];	/* 2x2 */
    }

                              /**********************************************/
                              /* calculate CTI of particular Column and Row */
                              /**********************************************/

/****************************************************************************
>> On Sun, 22 Apr 2007 18:35:59 +0900,
>> Hiroshi Nakajima <nakajima@ess.sci.osaka-u.ac.jp> said:

 - SEGMENT = 1, 2, RAWX = -1, -2 の時は、それぞれ
   SEGMENT = 2, 1, RAWX = 0, 1 の CTI パラメータを使うべき

 - SEGMENT = 0, 1, 2, 3, RAWX = 256, 257 の時は、それぞれ
   SEGMENT = 1, 0, 3, 2, RAWX = 255, 254 の CTI パラメータを使うべき
****************************************************************************/

    iseg  = segment;
    irawx = rawx + map_rawx[j];
    irawy = rawy + map_acty[j];
    iacty = acty + map_acty[j];
    if ( irawx < 0 ) {
      if ( 1 == segment || 2 == segment ) {
	iseg = segment + 1;
	irawx = - (1 + irawx);
      } else {
	corph[i] = phas[i];
	continue;
      }
    } else if ( 255 < irawx ) {
      iseg = map_segment[segment];
      irawx = 511 - irawx;
    }

    ipos = XISactiveSegmentHsize * iseg + irawx;

    ns = irawx + 1;		/* number of serial transfer */
    np = 0;

    scti_const = sp->cti_const[iseg];
    scti_norm = sp->cti_norm[iseg];
    scti_pow = sp->cti_pow[iseg];

    ctifs_norm = pp->ctifs_norm[ipos];

    cor_pha = phas[i];

    switch ( pp->format_version ) {
    case 1:	/* nominal CTI correction, with CTIFS_NORM = 0.0 */
    case 2:	/* nominal CTI correction, with CTIFS_NORM column */
      if ( win_st <= iacty && iacty < win_st + win_siz ) {
	np = iacty - win_st + 1;	/* number of parallel slow transfer */
	nfs = 1026 + win_st;		/* number of fast transfer for imaging to frame-store region */
      }
      pcti_const = pp->cti_const[ipos];
      pcti_norm = pp->cti_norm[ipos];
      pcti_pow = pp->cti_pow[ipos];

      if ( 0.0 < cor_pha ) {
	ctis = scti_const + scti_norm * pow(cor_pha, -scti_pow);
	cor_pha = cor_pha / pow(1-ctis, ns);
	ctip = pcti_const + pcti_norm * pow(cor_pha, -pcti_pow);
	cor_pha = cor_pha / pow(1-ctip, np);
	if ( 0.0 < ctifs_norm ) {
	  ctifs = ctifs_norm * pow(cor_pha, -pcti_pow);
	  cor_pha = cor_pha / pow(1-ctifs, nfs);
	}
      }
      break;

    case 3:	/* SCI CTI correction */
      if ( win_st <= iacty && iacty < win_st + win_siz ) {
	np = iacty - win_st + 1;	/* number of parallel slow transfer */
        nfs = 1026 + win_st;
      }
      dacty = ( 0 <= irawy && irawy < 1024 ) ? sci->dacty[irawy] : 0;
      bacty = space - dacty;

      norm_u = pp->cti_norm_u[ipos];
      norm_l = pp->cti_norm_l[ipos];
      dq_norm = pp->dq_norm[ipos];
      pcti_pow = pp->cti_pow[ipos];

      if ( 0.0 < cor_pha ) {
	pha = cor_pha;
	ctis = scti_const + scti_norm * pow(pha, -scti_pow);
	spow = pow(1-ctis, ns);
	ctip0 = (norm_u * bacty + norm_l * dacty) / space;
	ctip = ctip0 * pow(pha, - pcti_pow);
	dq0 =  dq_norm * dacty / space;
	dq = dq0 * pow(pha, 1.0 - pcti_pow);
	cor_pha = ( pha / pow(1-ctip, np)  + dq ) / spow;
	if ( 0.0 < ctifs_norm ) {
	  ctifs = ctifs_norm * pow(cor_pha, -pcti_pow);
	  cor_pha = cor_pha / pow(1-ctifs, nfs);
	}
      }
      break;

    default:	/* not supported */
      ;
    }

    corph[i] = cor_pha;
  }

  return 0;
}

static int
check_caltime_scti(double obstime, SCTI_PARAM *p)
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

  return read_scti_param(p->extname, obstime, p);
}

static int
check_caltime_pcti(double obstime, PCTI_PARAM *p)
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

  return read_pcti_param(p->extname, p->sci_period, obstime, p);
}

static void
showParam(void)
{
  printf("\nANL:  *** %s show parameter ***\n\n", pname);
  printf("%20s   %s\n", "ENABLE_CTICOR", com.enable_cticor ? "YES":"NO");
  printf("%20s   %s\n", "ENABLE_SCICTI", com.enable_scicti ? "YES":"NO");
  printf("%20s   '%s'%s\n", "MAKEPIFILE", com.makepifile,
	com.makepifile == com.o_makepifile ? "" : " (CALDB)");
  printf("\n");
}

void
XISctiCorrection_startup(int *status)
{
  com.makepifile = strcpy(com.o_makepifile, "CALDB");

  *status = ANL_OK;
}

void
XISctiCorrection_com(int *status)
{
  static char *keytbl[] = {
    "ENABLE_CTICOR",
    "ENABLE_SCICTI",
    "MAKEPIFILE",
    "SHOW",
    "EXIT"
  };
  static char *help[] = {
    "flag to enable CTI correction",
    "flag to enable CTI correction for SCI",
    "CALDB file for CTI parameters",
    "Show current setting",
    "Exit from this menu"
  };
  static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
  char *k;

  if ( *status ) {	/* ftools */
    if (
PILGetBool (k="enable_cticor", &com.enable_cticor) ||
PILGetBool (k="enable_scicti", &com.enable_scicti) ||
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
    char *key;
    int ans[2];
    CMinquir(pname, nkey, keytbl, help, 1, ans);
    key = keytbl[ans[1]-1];
    if ( 0 == strcmp("SHOW", key) ) {
      showParam();
    } else if ( 0 == strcmp("ENABLE_CTICOR", key) ) {
      CLlogrd(key, &com.enable_cticor);
    } else if ( 0 == strcmp("ENABLE_SCICTI", key) ) {
      CLlogrd(key, &com.enable_scicti);
    } else if ( 0 == strcmp("MAKEPIFILE", key) ) {
      CLtxtrd(key, com.makepifile, sizeof(com.o_makepifile));
    } else if ( 0 == strcmp("EXIT", key) ) {
      return;
    }
  }

  *status = ANL_OK;
}

void
XISctiCorrection_init(int *status)
{
  static char *extname = SCTI_EXT_NAME;

  int used, winopt, ci, sci_period;
  double obstime, tstart, tstop;

  int istat = 0;

  BnkGet("XIS:INSTRUME", sizeof(o_instrume), &used, &o_instrume);
  instrume = o_instrume + 1;	/* remove beginning "'" */
  com.makepifile = xisrsp_get_caldb_file(instrume, extname, com.o_makepifile);
  if ( NULL == com.makepifile ) {
    goto quit;
  }
  showParam();

  if ( com.enable_scicti ) {
    istat = xisSciBnkGetKeys("XIS:FITS:PTR", &sci);
    if ( istat ) {
      goto quit;
    }
    ci = sci.ci;
    sci_period = sci.period_rawy;
  } else {
    ci = 0;	/* force disable SCI CTI correction */
    sci_period = 0;
    BnkPut("XIS:CI", sizeof(ci), &ci);	/* override CI keyword for XISpha2pi */
  }

  BnkfGetM("XIS:TSTART", sizeof(tstart), &used, &tstart);
  BnkfGetM("XIS:TSTOP", sizeof(tstop), &used, &tstop);
  obstime = tstart;

  scti_extname = SCTI_EXT_NAME;
  istat = read_scti_param(scti_extname, obstime, &scti);
  if ( istat ) {
    goto quit;
  }

  BnkfGetM("XIS:WINOPT", sizeof(winopt), &used, &winopt);

  pcti_extname = decide_pcti_extname(winopt, ci);
  if ( NULL == pcti_extname ) {
    goto quit;
  }

  istat = read_pcti_param(pcti_extname, sci_period, obstime, &pcti);
  if ( istat ) {
    if ( BAD_HDU_NUM == istat && (XISwindowOff != winopt || 0 != ci) ) {
      anl_msg_warning("\
%s: WARNING: %s not found, assume SCI-off, WIN-off\n", pname, pcti_extname);
      pcti_extname = decide_pcti_extname(XISwindowOff, 0);
      istat = read_pcti_param(pcti_extname, sci_period, obstime, &pcti);
    }
    if ( istat ) {
      goto quit;
    }
  }

  EvsDef("XISctiCorrection:BEGIN");
  EvsDef("XISctiCorrection:ENTRY");
  EvsDef("XISctiCorrection:OK");

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XISctiCorrection_his(int *status)
{
  *status = ANL_OK;
}

void
XISctiCorrection_bgnrun(int *status)
{

  EvsfSetM("XISctiCorrection:BEGIN");

  *status = ANL_OK;
}

void
XISctiCorrection_ana(int nevent, int eventid, int *status)
{
  int j, used, num_pixel, editmode, pos2x2, segment, rawx, rawy, acty;
  double obstime;
  double ph[XISoneEventPixelTotNo5x5];
  double corph[XISoneEventPixelTotNo5x5];

  EvsfSetM("XISctiCorrection:ENTRY");

  pos2x2 = -1;
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

  case XISedit2x2:
    num_pixel = XISoneEventPixelTotNo2x2;
    BnkfGetM("XIS:POS2x2", sizeof(int), &used, &pos2x2);
    break;

  default:
    *status = ANL_OK;
    return;
  }

  BnkfGetM("XIS:PHASCORR", sizeof(double) * num_pixel, &used, ph);
  BnkfPutM("XIS:PHASNOCTI", sizeof(double) * num_pixel, ph);

  if ( 0 == com.enable_cticor ) {	/* do nothing */
    *status = ANL_OK;
    return;
  }

  BnkfGetM("XIS:TIME", sizeof(double), &used, &obstime);
  BnkfGetM("XIS:SEGMENT", sizeof(int), &used, &segment);
  BnkfGetM("XIS:RAWX", sizeof(int), &used, &rawx);
  BnkfGetM("XIS:RAWY", sizeof(int), &used, &rawy);
  BnkfGetM("XIS:ACTY", sizeof(int), &used, &acty);
  if ( segment < 0 || XIStotalSegNo <= segment ) {
    anl_msg_error("\
%s: illegal segment id %d\n", pname,segment);
    *status = ANL_QUIT;
    return;
  }

  if ( ANL_MSG_DEBUG <= anl_msg_chatter() ) {
    anl_msg_debug("before CTICorr");
    for (j = 0; j < num_pixel; j++) {
      anl_msg_debug(" %.1f", ph[j]);
      anl_msg_debug(" RAWX=%d, ACTY=%d, seg=%d\n", rawx, acty, segment);
    }
  }

  if ( check_caltime_scti(obstime, &scti) ||
       check_caltime_pcti(obstime, &pcti) ) {
    *status = ANL_ERROR;
    return;
  }

  xis_cticor(&scti, &pcti, &sci,
	segment, rawx, rawy, acty, pos2x2, num_pixel, ph, corph);

  if ( ANL_MSG_DEBUG <= anl_msg_chatter() ) {
    anl_msg_debug("after CTICorr");
    for (j = 0; j < num_pixel; j++) {
      anl_msg_debug(" %.1f", corph[j]);
    }
    anl_msg_debug("\n");
  }

  BnkfPutM("XIS:PHASCORR", sizeof(double) * num_pixel, corph);

  EvsfSetM("XISctiCorrection:OK");
  *status = ANL_OK;
}

void
XISctiCorrection_endrun(int *status)
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
  enable_cticor=%s  enable_scicti=%s",
	com.enable_cticor ? "yes" : "no", com.enable_scicti ? "yes" : "no");
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
XISctiCorrection_exit(int *status)
{
  *status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; c-basic-offset:2  ***
;;; End: ***
*/
