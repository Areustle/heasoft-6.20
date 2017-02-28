/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:50:17 1999 by E. Miyata*/
/* xis_module/XISpha2pi/0.0_fast/XISpha2pi_v0.0.c: modified by M.Ozaki on Fri Aug 13 21:04:54 1999:
 *   - replace BnkGet, BnkPut, EvsGet, ...
 *     to BnkfGetM, BnkfPutM, EvsfGetM, ... .
 */
/*************************************

  XISpha2pi

  1999/07/25	Emi Miyata
	fill PI column
	BNKget XIS:PHA
	BNKput XIS:PI
  ver 0.1    1999.10.20 Emi Miyata
	include fast version
  ver 0.2    1999.12.31 Emi Miyata
	add history keywords
  ver 0.3    2000.01.03 Emi Miyata
	fixed bug for treatment random number .... difficult
  ver 0.6    2005.06.15 Hiroshi Nakajima
        input gain parameters from fits file
	consider AE Temperature
  ver 0.7    2005.06.20 Hiroshi Nakajima
	add a branch about editmode
  ver 0.8    2005.06.22 Hiroshi Nakajima
        insure the filename length of PIL_LINESIZE
  ver 0.9    2005.10.04 Hiroshi Nakajima
	1. change the way how to read AE Temp. in an HK FITS file
	   (consider the AETime when we get HK packets)
        2. change the format of CALDB file
        3. redefine PI
            pi = (int)(pi_d+10000.5) - 10000;  ->

            pi = (int)(pi_d+10000) - 10000
        4. leave history comments to the output file
        5. restrict PI up to 4095
  ver 1.0    2005.10.28 Hiroshi Nakajima
	fix a bug at xis_pha2pi
	     if (tmp_pi <= Si_Kedge){                           ->
	     if (tmp_pi <= Si_Kedge_keV * 1000 / XIS_PI_UNIT){
	  and
	     #define Si_Kedge                1.839  ->
	     #define Si_Kedge_keV            1.839
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
  ver 1.5    2006.02.24 Hiroshi Nakajima
	set the length of variable comment as FLEN_COMMENT
  ver 1.6    2006.08.22 Y.ISHISAKI
	use XIS:TSTART,TSTOP instead of XIS:DATE-OBS,TIME-OBS
	use anl_msg_***() functions for messages
  ver 2.0    2006.10.31 Y.ISHISAKI
	change parameter name, pha2pi_rand_seed/skip -> rand_seed/skip
	change parameter name, pha2pi_hkfile -> hkfile
	change parameter name, pha2pi_caldbfile -> makepifile
	support CALDB for makepifile
	use xisrsp_get_caldb_file(), xisrsp_seek_valid_row()
	static declaration of xis_pha2pi()
  ver 2.1 2007.01.31 Y.ISHISAKI
	use aefits_write_module_history() in XISeditEventFits
  ver 2.2 2007.04.11 H.Nakajima
	adopt to the new CALDB file which has extensions for Window option,
	2x2 mode, and SCI
	adopt to the new gain table with 3 lines
  ver 3.0 2007.04.30 Y.ISHISAKI
	add 'hk_time_margin', 'hk_aetemp_min', 'hk_aetemp_max' parameters
  ver 3.1 2007.05.03 Y.ISHISAKI
	read FWHM_LM, FWHM_MH columns from makepi CALDB
	add 'enable_edge_smooth' parameter
	calculate "XIS:PHANOCTI" in _ana()
  ver 3.2 2008.04.10 Y.ISHISAKI
	bug fix in check_caltime_aegain/gain(), where rejecting obstime == 0.0
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
#include "aste_time.h"
#include "aste_rand.h"
#include "xisTelemFormat.h"
#include "xisRespUtil.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "XISpha2pi";
char XISpha2pi_version[] = "version 3.2";

#define XIS_PI_UNIT             3.65                  /* energy(eV) per 1ch */
#define FE55_ENE_keV            5.89507
                              /* See J.A.Bearden, 1967, Rev.Mod.Phys. 39, 78*/
#define Si_Kedge_keV            1.8389
         /* See B.L.Henke et al., 1993, Atom. Data & Nuc. Data Tab., 54, 181*/
#define Default_FWHM_AT_Si_Kedge_eV     110.0

#define G_AET_EXT_NAME		       "GAIN-AETEMP"
#define G_NORM_EXT_NAME	               "GAIN_NORMAL"
#define G_NORM_4WI_EXT_NAME            "GAIN_4WI"
#define G_NORM_8WI_EXT_NAME            "GAIN_8WI"
#define G_NORM_2x2_EXT_NAME            "GAIN_2x2"
#define G_NORM_2x2_4WI_EXT_NAME        "GAIN_2x2_4WI"
#define G_NORM_2x2_8WI_EXT_NAME        "GAIN_2x2_8WI"
#define G_NORM_SCI_EXT_NAME	       "GAIN_NORMAL_SCI"
#define G_NORM_4WI_SCI_EXT_NAME        "GAIN_4WI_SCI"
#define G_NORM_8WI_SCI_EXT_NAME        "GAIN_8WI_SCI"
#define G_NORM_2x2_SCI_EXT_NAME        "GAIN_2x2_SCI"
#define G_NORM_2x2_4WI_SCI_EXT_NAME    "GAIN_2x2_4WI_SCI"
#define G_NORM_2x2_8WI_SCI_EXT_NAME    "GAIN_2x2_8WI_SCI"
#define G_PSUM_EXT_NAME	               "GAIN_PSUM"

#define ASIZE	XIStotalSegNo

/* array parameters for AE-Temp dependent gain parameters */
/* PH(MnKalpha) = Co + aegain_norm * (To - aegain_offset ) ^ aegain_pow */
typedef struct {
  int format_version;
  char *extname;
  double last_obstime;
  double caltime, caltime_prev, caltime_next;
  double offs[ASIZE];
  double norm[ASIZE];
  double gpow[ASIZE];
} AEGAIN_PARAM;

/* array parameters for gain parameters */
typedef struct {
  int format_version;
  char *extname;
  double last_obstime;
  double caltime, caltime_prev, caltime_next;
  double l2[ASIZE];	/* used for format_version = 1, 2 */
  double l1[ASIZE];	/* used for format_version = 1, 2 */
  double l0[ASIZE];	/* used for format_version = 1, 2 */
  double m2[ASIZE];	/* used for format_version = 2 */
  double m1[ASIZE];	/* used for format_version = 2 */
  double m0[ASIZE];	/* used for format_version = 2 */
  double h2[ASIZE];	/* used for format_version = 1, 2 */
  double h1[ASIZE];	/* used for format_version = 1, 2 */
  double h0[ASIZE];	/* used for format_version = 1, 2 */
  double blm[ASIZE];	/* used for format_version = 2 */
  double bmh[ASIZE];	/* used for format_version = 2 */
  double wlm[ASIZE];	/* used for format_version = 2 */
  double wmh[ASIZE];	/* used for format_version = 2 */
  double gt[ASIZE];	/* used for format_version = 1, 2 */
  double pho[ASIZE];	/* pulse height at 5.9keV at gtemp */

  double b1_ph_lo[ASIZE], b1_ph_up[ASIZE];
  double b1_pi_lo[ASIZE], b1_pi_up[ASIZE];
  double b2_ph_lo[ASIZE], b2_ph_up[ASIZE];
  double b2_pi_lo[ASIZE], b2_pi_up[ASIZE];
  double slope1[ASIZE], slope2[ASIZE];

} GAIN_PARAM;

/* parameters for AE temperature from HK file */
typedef struct {
  double last_obstime;
  int last_ipos;
  int nrows;
  int nrej;
  struct aetime_aetemp {
    double aetime;
    double aetemp;
  } *hk;

/* HK statistics filled in calc_hk_statistics() */
  double aetemp_avg;
  double aetemp_min;
  double aetemp_max;
  double aetemp_sgm;

  double last_warning_time;
  int evnum_exceed_time_margin;

} HKTEMP_PARAM;

static struct {
  char hkfile[PIL_LINESIZE];
  char *makepifile, o_makepifile[PIL_LINESIZE];
  double hk_time_margin;
  double hk_aetemp_min;
  double hk_aetemp_max;
  int enable_edge_smooth;
} com;

static char *instrume, o_instrume[FLEN_VALUE];
static char *aegain_extname = G_AET_EXT_NAME;
static char *gain_extname = G_NORM_EXT_NAME;
static HKTEMP_PARAM hktemp;
static AEGAIN_PARAM aegain;
static GAIN_PARAM gain;

static int
precalc_gain_param(GAIN_PARAM *gp)
{
  static double fe55ev = 1000*FE55_ENE_keV;

  int seg;
  double l2, l1, l0;
  double m2, m1, m0;
  double h2, h1, h0;
  double blm, bmh;		/* Boundary_LM, Boundary_MH */
  double hlm, hmh;		/* FWHM_LM/2, FWHM_MH/2 */
  double b1_ph_lo, b1_ph_up;
  double b1_pi_lo, b1_pi_up;
  double b2_ph_lo, b2_ph_up;
  double b2_pi_lo, b2_pi_up;
  double slope1, slope2;	/* slope_LM, slope_MH */

  for (seg = 0; seg < ASIZE; seg++) {
    h2 = gp->h2[seg];
    h1 = gp->h1[seg];
    h0 = gp->h0[seg];
    if ( 0.0 == h2 ) {
      gp->pho[seg] = ( fe55ev - h0 ) / h1;
    } else {
      gp->pho[seg] = ( -h1 + sqrt(h1*h1 - 4 * h2 * ( h0 - fe55ev)) ) / (2*h2);
    }
  }

  if ( 2 != gp->format_version ) {
    return 0;
  }

  for (seg = 0; seg < ASIZE; seg++) {
    l2  = gp->l2[seg];  l1  = gp->l1[seg];  l0  = gp->l0[seg];
    m2  = gp->m2[seg];  m1  = gp->m1[seg];  m0  = gp->m0[seg];
    h2  = gp->h2[seg];  h1  = gp->h1[seg];  h0  = gp->h0[seg];
    blm = gp->blm[seg]; bmh = gp->bmh[seg];
    hlm = hmh = 0.0;
    if ( com.enable_edge_smooth ) {
      hlm = gp->wlm[seg] / 2;
      hmh = gp->wmh[seg] / 2;
    }

    /*  Define 3rd gain table around edge 1 (Si K-edge)*/
    if ( l2 == 0.0 ) {
      b1_ph_lo = ( (blm - hlm) - l0 ) / l1;
    } else {
      b1_ph_lo = ( -l1 + sqrt(l1*l1 - 4*l2*(l0 - (blm - hlm))) ) / (2*l2);
    }

    if ( m2 == 0.0 ) {
      b1_ph_up = ( (blm + hlm ) - m0 ) / m1;
    } else {
      b1_ph_up = ( -m1 + sqrt(m1*m1 - 4*m2*(m0 - (blm + hlm))) ) / (2*m2);
    }

    b1_pi_lo = ( blm - hlm ) / XIS_PI_UNIT;
    b1_pi_up = ( blm + hlm ) / XIS_PI_UNIT;
    slope1 = 0.0;
    if ( b1_ph_up != b1_ph_lo ) {
      slope1 = (b1_pi_up - b1_pi_lo) / (b1_ph_up - b1_ph_lo);
    }

    /*  Define 3rd gain table around edge 2 (reserved) */
    if ( m2 == 0.0 ) {
      b2_ph_lo = ( (bmh - hmh) - m0 ) / m1;
    } else {
      b2_ph_lo = ( -m1 + sqrt(m1*m1 - 4*m2*(m0 - (bmh - hmh))) ) / (2*m2);
    }

    if ( h2 == 0.0 ) {
      b2_ph_up = ( (bmh + hmh) - h0 ) / h1;
    } else {
      b2_ph_up = ( -h1 + sqrt(h1*h1 - 4*h2*(h0 - (bmh + hmh))) ) / (2*h2);
    }

    b2_pi_lo = ( bmh - hmh ) / XIS_PI_UNIT;
    b2_pi_up = ( bmh + hmh ) / XIS_PI_UNIT;
    slope2 = 0.0;
    if ( b2_ph_up != b2_ph_lo ) {
      slope2 = (b2_pi_up - b2_pi_lo) / (b2_ph_up - b2_ph_lo);
    }

    gp->b1_ph_lo[seg] = b1_ph_lo;
    gp->b1_ph_up[seg] = b1_ph_up;
    gp->b1_pi_lo[seg] = b1_pi_lo;
    gp->b1_pi_up[seg] = b1_pi_up;

    gp->b2_ph_lo[seg] = b2_ph_lo;
    gp->b2_ph_up[seg] = b2_ph_up;
    gp->b2_pi_lo[seg] = b2_pi_lo;
    gp->b2_pi_up[seg] = b2_pi_up;

    gp->slope1[seg] = slope1;
    gp->slope2[seg] = slope2;
  }

  return 0;
}


static double
xis_pha2pi(AEGAIN_PARAM *ap, GAIN_PARAM *gp, int seg, double atemp, double pha)
{
  double l2, l1, l0;
  double m2, m1, m0;
  double h2, h1, h0;
  double gtemp, pho, norm, offs, gpow, gfact;

  double pha_d, pi_d;

/* gfact = 1.0 + (PH - PHo) / PHo
               PHo: pulse height at 5.9keV at gtemp (given in CALDB)
               PHo = Constant + aegainnorm*(T - aegainoffset)^aegainpow
               PH:  pulse height at 5.9keV in this observation
	       PH  = Constant + norm*(T' - offset)^pow */

  gtemp = gp->gt[seg];
  pho   = gp->pho[seg];
  norm  = ap->norm[seg];
  offs  = ap->offs[seg];
  gpow  = ap->gpow[seg];
  gfact = 1 + norm * ( pow(atemp-offs, gpow) - pow(gtemp-offs, gpow) ) / pho;
  pha_d = pha / gfact;

  l2  = gp->l2[seg];  l1  = gp->l1[seg];  l0  = gp->l0[seg];
  h2  = gp->h2[seg];  h1  = gp->h1[seg];  h0  = gp->h0[seg];

  if ( 1 == gp->format_version ) {
/* FORMAT_VERSION=1 */
    pi_d = ( pha_d * ( pha_d * l2 + l1 ) + l0 ) / XIS_PI_UNIT;
    if ( Si_Kedge_keV * 1000 / XIS_PI_UNIT < pi_d ) {
      pi_d = ( pha_d * ( pha_d * h2 + h1 ) + h0 ) / XIS_PI_UNIT;
    }
    return pi_d;
  }

/* FORMAT_VERSION=2 */

  m2  = gp->m2[seg];  m1  = gp->m1[seg];  m0  = gp->m0[seg];

  if ( pha_d <= gp->b1_ph_lo[seg] ) {
    pi_d = ( pha_d * ( pha_d * l2 + l1 ) + l0 ) / XIS_PI_UNIT;

  } else if ( pha_d < gp->b1_ph_up[seg] ) {
    pi_d = gp->slope1[seg] * ( pha_d - gp->b1_ph_lo[seg] ) + gp->b1_pi_lo[seg];

  } else if ( pha_d <= gp->b2_ph_lo[seg] ) {
    pi_d = ( pha_d * ( pha_d * m2 + m1 ) + m0 ) / XIS_PI_UNIT;

  } else if ( pha_d < gp->b2_ph_up[seg] ) {
    pi_d = gp->slope2[seg] * ( pha_d - gp->b2_ph_lo[seg] ) + gp->b2_pi_lo[seg];

  } else {
    pi_d = ( pha_d * ( pha_d * h2 + h1 ) + h0 ) / XIS_PI_UNIT;
  }

  return pi_d;
}

static char *
decide_gain_extname(int editmode, int winopt, int ci)
{
  char *extname = NULL;

  if ( editmode == XISeditTiming ) {
    extname = G_PSUM_EXT_NAME;
	anl_msg_debug("\
Psum mode: reading %s extension\n", extname);
    return extname;
  }

    /**************************************************************/
    /**************** Normal Mode SCI off *************************/
    /**************************************************************/

  if ( 0 == ci || 1 == ci ) {	/* CI is off or diagnostic CI */

    if ( winopt == XISwindowOff ) {
      if ( editmode == XISedit2x2 ){
	extname = G_NORM_2x2_EXT_NAME;
	anl_msg_debug("\
%s mode and Window option is %s: reading %s extension\n",
		     XISedit2x2SName, XISwindowOffSName, extname);
      } else {
	extname = G_NORM_EXT_NAME;
	anl_msg_debug("\
Window option is %s: reading %s extension\n", XISwindowOffSName, extname);
      }
    } else if ( winopt == XISwindow4 ) {
      if ( editmode == XISedit2x2 ){
	extname = G_NORM_2x2_4WI_EXT_NAME;
	anl_msg_debug("\
%s mode and %s window observation: reading %s extension\n",
		     XISedit2x2SName, XISwindow4SName, extname);
      } else {
	extname = G_NORM_4WI_EXT_NAME;
	anl_msg_debug("\
%s window observation: reading %s extension\n", XISwindow4SName, extname);
      }
    } else if ( winopt == XISwindow8 ) {
      if ( editmode == XISedit2x2 ) {
	extname = G_NORM_2x2_8WI_EXT_NAME;
	anl_msg_debug("\
%s mode and %s window observation: reading %s extension\n",
		     XISedit2x2SName, XISwindow8SName, extname);
      } else {
	extname = G_NORM_8WI_EXT_NAME;
	anl_msg_debug("\
%s window observation: reading %s extension\n", XISwindow8SName, extname);
      }
    } else if ( winopt == XISwindow16 ) {
      anl_msg_error("\
%s: non supported window option (%s window).\n", pname, XISwindow16SName);
    } else {
      anl_msg_error("\
%s: illegal window option %d\n", pname, winopt);
    }

    /**************************************************************/
    /**************** Normal Mode SCI on  *************************/
    /**************************************************************/
  } else if ( 2 == ci || 3 == ci ) {	/* SCI is on */

    if ( winopt == XISwindowOff ) {
      if ( editmode == XISedit2x2 ){
	extname = G_NORM_2x2_SCI_EXT_NAME;
	anl_msg_debug("\
SCI is on, %s mode and Window option is %s: reading %s extension\n",
		     XISedit2x2SName, XISwindowOffSName, extname);
      } else {
	extname = G_NORM_SCI_EXT_NAME;
	anl_msg_debug("\
SCI is on, Window option is %s: reading %s extension\n",
		     XISwindowOffSName, extname);
      }
    } else if ( winopt == XISwindow4 ) {
      if ( editmode == XISedit2x2 ) {
	extname = G_NORM_2x2_4WI_SCI_EXT_NAME;
	anl_msg_debug("\
SCI is on, %s mode and %s window observation: reading %s extension\n",
		     XISedit2x2SName, XISwindow4SName, extname);
      } else {
	extname = G_NORM_4WI_SCI_EXT_NAME;
	anl_msg_debug("\
SCI is on, %s window observation: reading %s extension\n",
		     XISwindow4SName, extname);
      }
    } else if ( winopt == XISwindow8 ) {
      if ( editmode == XISedit2x2 ) {
	extname = G_NORM_2x2_8WI_SCI_EXT_NAME;
	anl_msg_debug("\
SCI is on, %s mode and %s window observation: reading %s extension\n",
		     XISedit2x2SName, XISwindow8SName, extname);
      } else {
	extname = G_NORM_8WI_SCI_EXT_NAME;
	anl_msg_debug("\
SCI is on, %s window observation: reading %s extension\n",
		     XISwindow8SName, extname);
      }
    } else if ( winopt == XISwindow16 ) {
      anl_msg_error("\
%s: non supported window option (%s window).\n", pname, XISwindow16SName);
    } else {
      anl_msg_error("\
%s: illegal window option %d\n", pname, winopt);
    }

  } else {
    anl_msg_error("\
%s: illegal ci option %d\n", pname, ci);
  }

  return extname;
}

static int
read_aegain_param(char *extname, double obstime, AEGAIN_PARAM *ap)
{
  int i, a;
  int col_time, col_offs, col_norm, col_gpow;
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
  istat = xisrsp_read_format_version(fp, extname, &ap->format_version);
  if ( istat ) {
    goto quit;
  }
  if ( 1 < ap->format_version ) {
    anl_msg_error("\
%s: unknown FORMAT_VERSION=%d for %s\n", pname, ap->format_version, extname);
    istat = -1;
    goto quit;
  }

  irow = xisrsp_seek_valid_row(fp, obstime);
  anl_msg_info("\
reading %s at %ld-th row\n", extname, irow);

  if (
fits_get_colnum(fp, CASESEN, k="TIME", &col_time, &istat) ||
fits_get_colnum(fp, CASESEN, k="NORM", &col_norm, &istat) ||
fits_get_colnum(fp, CASESEN, k="OFFSET", &col_offs, &istat) ||
fits_get_colnum(fp, CASESEN, k="POW", &col_gpow, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum(%s) failed (%d)\n", pname, k, istat);
    goto quit;
  }

  if (
fits_read_col_dbl(fp, i=col_time, irow, 1, 1, 0.0, &ap->caltime, &a, &istat) ||
fits_read_col_dbl(fp, i=col_norm, irow, 1, ASIZE, 0.0, ap->norm, &a, &istat) ||
fits_read_col_dbl(fp, i=col_offs, irow, 1, ASIZE, 0.0, ap->offs, &a, &istat) ||
fits_read_col_dbl(fp, i=col_gpow, irow, 1, ASIZE, 0.0, ap->gpow, &a, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n",
	pname, i, irow, istat);
    goto quit;
  }

  ap->caltime_prev = ap->caltime_next = ap->caltime;

  if ( 1 < irow ) {
    fits_read_col_dbl(fp, col_time, irow-1, 1, 1, 0.0,
	&ap->caltime_prev, &a, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col('TIME') failed at irow=%ld (%d)\n", pname, irow, istat);
      goto quit;
    }
  }

  if ( irow < nrows ) {
    fits_read_col_dbl(fp, col_time, irow+1, 1, 1, 0.0,
	&ap->caltime_next, &a, &istat);
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

  ap->last_obstime = obstime;
  ap->extname = extname;

  return 0;

 quit:
  if ( NULL != fp ) {
    int istat2 = 0;
    fits_close_file(fp, &istat2);	/* ignore error */
  }
  return istat;
}

static int
read_gain_param(char *extname, double obstime, GAIN_PARAM *gp)
{
  int i, a;
  int col_time;
  int col_l2, col_l1, col_l0;
  int col_m2, col_m1, col_m0;
  int col_h2, col_h1, col_h0;
  int col_blm, col_bmh, col_wlm, col_wmh, col_gt;
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
  istat = xisrsp_read_format_version(fp, extname, &gp->format_version);
  if ( istat ) {
    goto quit;
  }

  irow = xisrsp_seek_valid_row(fp, obstime);
  anl_msg_info("\
reading %s at %ld-th row\n", extname, irow);

  if (
fits_get_colnum(fp, CASESEN, k="TIME", &col_time, &istat) ||
fits_get_colnum(fp, CASESEN, k="QUAD_LOW", &col_l2, &istat) ||
fits_get_colnum(fp, CASESEN, k="LINR_LOW", &col_l1, &istat) ||
fits_get_colnum(fp, CASESEN, k="OFFSET_LOW", &col_l0, &istat) ||
fits_get_colnum(fp, CASESEN, k="QUAD_HIGH", &col_h2, &istat) ||
fits_get_colnum(fp, CASESEN, k="LINR_HIGH", &col_h1, &istat) ||
fits_get_colnum(fp, CASESEN, k="OFFSET_HIGH", &col_h0, &istat) ||
fits_get_colnum(fp, CASESEN, k="AETemp", &col_gt, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum(%s) failed (%d)\n", pname, k, istat);
    goto quit;
  }

  if (
fits_read_col_dbl(fp, i=col_time, irow, 1, 1, 0.0, &gp->caltime, &a, &istat) ||
fits_read_col_dbl(fp, i=col_l2, irow, 1, ASIZE, 0.0, gp->l2, &a, &istat) ||
fits_read_col_dbl(fp, i=col_l1, irow, 1, ASIZE, 0.0, gp->l1, &a, &istat) ||
fits_read_col_dbl(fp, i=col_l0, irow, 1, ASIZE, 0.0, gp->l0, &a, &istat) ||
fits_read_col_dbl(fp, i=col_h2, irow, 1, ASIZE, 0.0, gp->h2, &a, &istat) ||
fits_read_col_dbl(fp, i=col_h1, irow, 1, ASIZE, 0.0, gp->h1, &a, &istat) ||
fits_read_col_dbl(fp, i=col_h0, irow, 1, ASIZE, 0.0, gp->h0, &a, &istat) ||
fits_read_col_dbl(fp, i=col_gt, irow, 1, ASIZE, 0.0, gp->gt, &a, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n",
	pname, i, irow, istat);
    goto quit;
  }

  if ( 2 == gp->format_version ) {
    if (
fits_get_colnum(fp, CASESEN, k="QUAD_MED", &col_m2, &istat) ||
fits_get_colnum(fp, CASESEN, k="LINR_MED", &col_m1, &istat) ||
fits_get_colnum(fp, CASESEN, k="OFFSET_MED", &col_m0, &istat) ||
fits_get_colnum(fp, CASESEN, k="Boundary_LM", &col_blm, &istat) ||
fits_get_colnum(fp, CASESEN, k="Boundary_MH", &col_bmh, &istat) ||
       0 ) {
      anl_msg_error("\
%s: fits_get_colnum(%s) failed (%d)\n", pname, k, istat);
      goto quit;
    }
    if (
fits_read_col_dbl(fp, i=col_m2, irow, 1, ASIZE, 0.0, gp->m2, &a, &istat) ||
fits_read_col_dbl(fp, i=col_m1, irow, 1, ASIZE, 0.0, gp->m1, &a, &istat) ||
fits_read_col_dbl(fp, i=col_m0, irow, 1, ASIZE, 0.0, gp->m0, &a, &istat) ||
fits_read_col_dbl(fp, i=col_blm, irow, 1, ASIZE, 0.0, gp->blm, &a, &istat) ||
fits_read_col_dbl(fp, i=col_bmh, irow, 1, ASIZE, 0.0, gp->bmh, &a, &istat) ||
       0 ) {
      anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n",
	pname, i, irow, istat);
      goto quit;
    }
    if (
fits_get_colnum(fp, CASESEN, k="FWHM_LM", &col_wlm, &istat) ||
fits_get_colnum(fp, CASESEN, k="FWHM_MH", &col_wmh, &istat) ||
       0 ) {
      istat = 0;	/* ignore error */
      anl_msg_warning("\
%s: WARNING: FWHM_LM, FWHM_MH columns are not found, assume %.0f eV\n",
	pname, (double)Default_FWHM_AT_Si_Kedge_eV);
      for (i = 0; i < ASIZE; i++) {
	gp->wlm[i] = Default_FWHM_AT_Si_Kedge_eV;
	gp->wmh[i] = Default_FWHM_AT_Si_Kedge_eV;
      }
    } else if (
fits_read_col_dbl(fp, i=col_wlm, irow, 1, ASIZE, 0.0, gp->wlm, &a, &istat) ||
fits_read_col_dbl(fp, i=col_wmh, irow, 1, ASIZE, 0.0, gp->wmh, &a, &istat) ||
       0 ) {
      anl_msg_error("\
%s: fits_read_col(icol=%d) failed at irow=%ld (%d)\n",
	pname, i, irow, istat);
      goto quit;
    }

  } else if ( 2 < gp->format_version ) {
    anl_msg_error("\
%s: unknown FORMAT_VERSION=%d for %s\n", pname, gp->format_version, extname);
    istat = -1;
    goto quit;
  }

  precalc_gain_param(gp);

  gp->caltime_prev = gp->caltime_next = gp->caltime;

  if ( 1 < irow ) {
    fits_read_col_dbl(fp, col_time, irow-1, 1, 1, 0.0,
	&gp->caltime_prev, &a, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col('TIME') failed at irow=%ld (%d)\n", pname, irow, istat);
      goto quit;
    }
  }

  if ( irow < nrows ) {
    fits_read_col_dbl(fp, col_time, irow+1, 1, 1, 0.0,
	&gp->caltime_next, &a, &istat);
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

  gp->last_obstime = obstime;
  gp->extname = extname;

  return 0;

 quit:
  if ( NULL != fp ) {
    int istat2 = 0;
    fits_close_file(fp, &istat2);	/* ignore error */
  }
  return istat;
}

static int
check_caltime_aegain(double obstime, AEGAIN_PARAM *p)
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

  return read_aegain_param(p->extname, obstime, p);
}

static int
check_caltime_gain(double obstime, GAIN_PARAM *p)
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

  return read_gain_param(p->extname, obstime, p);
}

static int
compare_aetime(const void *p1, const void *p2)
{
  double aetime1, aetime2;
  aetime1 = ((struct aetime_aetemp *)p1)->aetime;
  aetime2 = ((struct aetime_aetemp *)p2)->aetime;
  if ( aetime1 < aetime2 ) {
    return -1;
  } else if ( aetime1 > aetime2 ) {
    return 1;
  }
  return 0;
}

static void
calc_hk_statistics(HKTEMP_PARAM *hp)
{
  int i, nrows;
  double aetemp, mi, ma, av, sg, dif;
  struct aetime_aetemp *hk = hp->hk;

  mi = ma = hk[0].aetemp;
  av = sg = 0.0;
  nrows = hp->nrows;

  if ( 0 < nrows ) {
    for (i = 0; i < nrows; i++) {
      aetemp = hk[i].aetemp;
      if ( aetemp < mi ) {
	mi = aetemp;
      } else if ( ma < aetemp ) {
	ma = aetemp;
      }
      av += aetemp;
    }
    av = av / nrows;

    if ( 1 < nrows ) {
      for (i = 0; i < nrows; i++) {
	dif = hk[i].aetemp - av;
	sg += dif * dif;
      }
      sg = sqrt(sg / (nrows - 1));
    }
  }

  hp->aetemp_min = mi;
  hp->aetemp_max = ma;
  hp->aetemp_avg = av;
  hp->aetemp_sgm = sg;

  hp->last_warning_time = 0.0;
  hp->evnum_exceed_time_margin = 0;
}

static int
reject_invalid_hk(HKTEMP_PARAM *hp)
{
  int i, j, nrows;
  double aetime, aetemp;
  struct aetime_aetemp *hk = hp->hk;

  nrows = hp->nrows;
  for (i = j = 0; i < nrows; i++) {
    aetime = hk[i].aetime;
    aetemp = hk[i].aetemp;
    if ( 0.0 < aetime &&	/* reject invalid time assignment */
	 com.hk_aetemp_min <= aetemp && aetemp <= com.hk_aetemp_max ) {
      hk[j] = hk[i];
      j++;
    }
  }
  hp->nrej = hp->nrows - j;
  hp->nrows = j;

  if ( 0 == j ) {
    return hp->nrej;
  }

  qsort(hk, j, sizeof(*hp->hk), compare_aetime);
  calc_hk_statistics(hp);

  return hp->nrej;
}

static int
read_hkfile(HKTEMP_PARAM *hp)
{
  static char *extname = "XIS_AE_TCE_I";

  char *k, hk_instrume[FLEN_VALUE], keyname[FLEN_VALUE];
  int col_time, col_temp, anul, nrej;
  long irow, nrows;;
  double aetime, aetemp;
  struct aetime_aetemp *hk;

  fitsfile *fp = NULL;
  int istat = 0;

  if ( fits_open_file(&fp, com.hkfile, READONLY, &istat) ) {
    anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, com.hkfile, istat);
    goto quit;
  }
  if ( fits_movnam_hdu(fp, BINARY_TBL, extname, 0, &istat) ) {
    anl_msg_error("\
%s: fits_movnam_hdu('%s') failed (%d)\n", pname, extname, istat);
    goto quit;
  }
  if ( fits_read_key_str(fp, k="INSTRUME", hk_instrume, NULL, &istat) ) {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  if ( 0 != strcmp(instrume, hk_instrume) ) {
    anl_msg_error("\
%s: INSTRUME keyword (%s) is inconsistent with HK (%s).\n",
	pname, instrume, hk_instrume);
    istat = -1;
    goto quit;
  }
  if ( fits_get_num_rows(fp, &nrows, &istat) ) {
    anl_msg_error("\
%s: fits_get_num_rows()  failed (%d)\n", pname, istat);
    goto quit;
  }
  if ( 0 == nrows ) {
    anl_msg_error("\
%s: nrows=0 for hkfile\n", pname);
    istat = -1;
    goto quit;
  }

  hp->hk = hk = malloc(nrows * sizeof(*hk));
  if ( NULL == hk ) {
    anl_msg_error("\
%s: can't allocate memory for hkfile\n", pname);
    istat = -1;
    goto quit;
  }

  sprintf(keyname, "%s_VDCHK18_CAL", instrume+2);	/* Sn_VDCHK18_CAL */
  anl_msg_info("\
reading %s, %s, nrows=%ld\n", com.hkfile, keyname, nrows);

  if (
fits_get_colnum(fp, CASESEN, k="TIME", &col_time, &istat) ||
fits_get_colnum(fp, CASESEN, k=keyname, &col_temp, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }

  for (irow = 1; irow <= nrows; irow++) {
    fits_read_col_dbl(fp, col_time, irow, 1, 1, 0.0, &aetime, &anul, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col('TIME') failed (%d)\n", pname, istat);
      goto quit;
    }
    fits_read_col_dbl(fp, col_temp, irow, 1, 1, 0.0, &aetemp, &anul, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col('%s') failed (%d)\n", pname, keyname, istat);
      goto quit;
    }
    hk[irow-1].aetime = aetime;
    hk[irow-1].aetemp = aetemp;
  }

  fits_close_file(fp, &istat);
  fp = NULL;
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
    goto quit;
  }

  hp->last_obstime = 0.0;
  hp->last_ipos = 0;
  hp->nrows = nrows;
  nrej = reject_invalid_hk(hp);
  if ( 0 == hp->nrows ) {
    anl_msg_error("\
%s: all of HK rows are rejected (nrej=%d)\n", pname, nrej);
    istat = -1;
    goto quit;
  }

  anl_msg_info("\
    nvalid=%d  nrej=%d  time=%.1lf - %.1lf [s]\n\
    AE-temp: average=%.3f  sigma=%.3f  min=%.3f  max=%.3f [degC]\n\
\n",	hp->nrows, nrej, hk[0].aetime, hk[hp->nrows-1].aetime,
	hp->aetemp_avg, hp->aetemp_sgm, hp->aetemp_min, hp->aetemp_max);

  return 0;

 quit:
  if ( NULL != fp ) {
    int istat2 = 0;
    fits_close_file(fp, &istat2);	/* ignore error */
  }
  return istat;
}

static void
find_aetemp(HKTEMP_PARAM *hp, double obstime,
	double *caltime_return, double *aetemp_return)
{
  int nrows, ipos, ipos_prev, ipos_next;
  double caltime, caltime_prev, caltime_next, t0, t1;
  struct aetime_aetemp *hk = hp->hk;

  nrows = hp->nrows;
  ipos = hp->last_ipos;
  if ( obstime == hp->last_obstime || 1 == nrows ) {
    goto end;
  }

  caltime = hk[ipos].aetime;
  ipos_prev = ipos - 1;
  if ( ipos_prev < 0 ) {
    ipos_prev = 0;
  }
  ipos_next = ipos + 1;
  if ( nrows <= ipos_next ) {
    ipos_next = nrows - 1;
  }
  caltime_prev = hk[ipos_prev].aetime;
  caltime_next = hk[ipos_next].aetime;
  t0 = (caltime + caltime_prev) / 2;		/* previous boundary */
  t1 = (caltime + caltime_next) / 2;		/* next boundary */

  if ( t0 == obstime ) {
    ipos = ipos_prev;
    goto end;
  }

  if ( t0 < obstime ) {

 again:
    for (;;) {
      if ( obstime <= t1 ) {
	goto end;
      }
      ipos++;
      if ( nrows <= ipos ) {
	ipos = nrows - 1;
	goto end;
      }
      caltime = caltime_next;
      ipos_next++;
      if ( nrows <= ipos_next ) {
	ipos_next = nrows - 1;
      }
      caltime_next = hk[ipos_next].aetime;
      t1 = (caltime + caltime_next) / 2;	/* next boundary */
    }
  }

  ipos = 0;
  ipos_next = 1;
  caltime = hk[ipos].aetime;
  caltime_next = hk[ipos_next].aetime;
  t1 = (caltime + caltime_next) / 2;		/* next boundary */

  if ( obstime <= t1 ) {
    goto end;
  }

  goto again;

 end:
  hp->last_ipos = ipos;
  hp->last_obstime = obstime;
  *caltime_return = hk[ipos].aetime;
  *aetemp_return = hk[ipos].aetemp;
}

static void
showParam(void)
{
  printf ("\nANL:  *** %s show parameter ***\n\n", pname);
  printf("%20s   '%s'\n", "HKFILE", com.hkfile);
  printf("%20s   '%s'%s\n", "MAKEPIFILE", com.makepifile,
	com.makepifile == com.o_makepifile ? "" : " (CALDB)");
  printf("\n");
}

void
XISpha2pi_startup(int *status)
{
  strcpy(com.hkfile, "none");
  com.makepifile = strcpy(com.o_makepifile,"CALDB");

  *status = ANL_OK;
}

void
XISpha2pi_com(int *status)
{
  static char *keytbl[] = {
    "HKFILE",
    "MAKEPIFILE",
    "HK_TIME_MARGIN",
    "HK_AETEMP_MIN",
    "HK_AETEMP_MAX",
    "ENABLE_EDGE_SMOOTH",
    "SHOW",
    "EXIT"
  };
  static char *help[] = {
    "HK file",
    "CALDB file for Gain parameters",
    "Time margin in second to consider AE-temp is valid",
    "Minimum value in degC to consider AE-temp is valid",
    "Maximum value in degC to consider AE-temp is valid",
    "Flag to enable smoothing the PHA to PI relation around edge",
    "Show current setting",
    "Exit from this menu"
  };
  static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
  char *k, *key;
  int ans[2];

  if ( *status ) {	/* ftools */
    if (
PILGetFname(k="hkfile", com.hkfile) ||
PILGetFname(k="makepifile", com.makepifile) ||
PILGetReal (k="hk_time_margin", &com.hk_time_margin) ||
PILGetReal (k="hk_aetemp_min", &com.hk_aetemp_min) ||
PILGetReal (k="hk_aetemp_max", &com.hk_aetemp_max) ||
PILGetBool (k="enable_edge_smooth", &com.enable_edge_smooth) ||
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
    if ( 0 == strcmp("SHOW", key) ) {
      showParam ();
    } else if ( 0 == strcmp("HKFILE", key) ) {
      CLtxtrd(key, com.hkfile, sizeof(com.hkfile));
    } else if ( 0 == strcmp("MAKEPIFILE", key) ) {
      CLtxtrd(key, com.makepifile, sizeof(com.o_makepifile));
    } else if ( 0 == strcmp("HK_TIME_MARGIN", key) ) {
      CLfdprd(key, &com.hk_time_margin);
    } else if ( 0 == strcmp("HK_AETEMP_MIN", key) ) {
      CLfdprd(key, &com.hk_aetemp_min);
    } else if ( 0 == strcmp("HK_AETEMP_MAX", key) ) {
      CLfdprd(key, &com.hk_aetemp_max);
    } else if ( 0 == strcmp("ENABLE_EDGE_SMOOTH", key) ) {
      CLlogrd(key, &com.enable_edge_smooth);
    } else if ( 0 == strcmp ("EXIT", key) ) {
      return;
    }
  }

  *status = ANL_OK;
}

void
XISpha2pi_init(int *status)
{
  static char *extname = G_AET_EXT_NAME;

  int used, winopt, ci;
  double obstime, tstart, tstop;
  int istat = 0, editmode = 0;

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

  istat = read_aegain_param(aegain_extname, obstime, &aegain);
  if ( istat ) {
    goto quit;
  }

  BnkfGetM("XIS:WINOPT", sizeof(winopt), &used, &winopt);
  BnkfGetM("XIS:CI", sizeof(ci), &used, &ci);
  BnkfGetM("XIS:EDITMODE", sizeof(editmode), &used, &editmode);

  gain_extname = decide_gain_extname(editmode, winopt, ci);
  if ( NULL == gain_extname ) {
    goto quit;
  }

  istat = read_gain_param(gain_extname, obstime, &gain);
  if ( istat ) {
    if ( BAD_HDU_NUM == istat &&
	 (XISedit2x2 == editmode || XISwindowOff != winopt || 0 != ci) ) {
      anl_msg_warning("\
%s: WARNING: %s not found, assume 5x5, SCI-off, WIN-off\n",
		pname, gain_extname);
      gain_extname = decide_gain_extname(XISedit5x5, XISwindowOff, 0);
      istat = read_gain_param(gain_extname, obstime, &gain);
    }
    if ( istat ) {
      goto quit;
    }
  }

  istat = read_hkfile(&hktemp);
  if ( istat ) {
    goto quit;
  }

  EvsDef("XISpha2pi:BEGIN");
  EvsDef("XISpha2pi:ENTRY");
  EvsDef("XISpha2pi:OK");

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
XISpha2pi_his(int *status)
{
  *status = ANL_OK;
}

void
XISpha2pi_bgnrun(int *status)
{

  EvsfSetM("XISpha2pi:BEGIN");

  *status = ANL_OK;
}

void
XISpha2pi_ana(int nevent, int eventid, int *status)
{
  int used, pha, pi, editmode, segment;
  double pha_d, pi_d;
  double aetemp, obstime, caltime;

  EvsfSetM("XISpha2pi:ENTRY");
  BnkfGetM("XIS:EDITMODE", sizeof(int), &used, &editmode);

  switch (editmode) {
  case XISedit5x5:
  case XISedit3x3:
  case XISedit2x2:
  case XISeditTiming:
    break;
  default:
    *status = ANL_OK;
    return;
  }

  BnkfGetM("XIS:SEGMENT", sizeof(int), &used, &segment);
  if ( segment < 0 || XIStotalSegNo <= segment ) {
    anl_msg_error("\
%s: illegal segment id %d\n", pname,segment);
    *status = ANL_QUIT;
    return;
  }

  BnkfGetM("XIS:TIME", sizeof(double), &used, &obstime);

  find_aetemp(&hktemp, obstime, &caltime, &aetemp);
  if ( 0 < com.hk_time_margin && com.hk_time_margin < fabs(caltime-obstime) ){
    hktemp.evnum_exceed_time_margin++;
    if ( hktemp.last_warning_time != obstime ) {
      hktemp.last_warning_time = obstime;
      anl_msg_warning("\
%s: WARNING: HK time=%.1f is too far from event time=%.1f\n",
	pname, caltime, obstime);
    }
  }

  if ( check_caltime_aegain(obstime, &aegain) ||
       check_caltime_gain(obstime, &gain) ) {
    *status = ANL_ERROR;
    return;
  }

  BnkfGetM("XIS:PHANOCTI:DOUBLE", sizeof(double), &used, &pha_d);
  pi_d = xis_pha2pi(&aegain, &gain, segment, aetemp, pha_d);
  pi = (int)floor(pi_d);	/* truncate here */
  if ( pi < -1000 ) {
    pi = -1000;
  } else if ( 4095 < pi ) {
    pi = 4095;
  }
  BnkfPutM("XIS:PHANOCTI", sizeof(int), &pi);

  BnkfGetM("XIS:PHA:DOUBLE", sizeof(double), &used, &pha_d);
  pi_d = xis_pha2pi(&aegain, &gain, segment, aetemp, pha_d);
  pi = (int)floor(pi_d);	/* truncate here */
  if ( pi < -1000 ) {
    pi = -1000;
  } else if ( 4095 < pi ) {
    pi = 4095;
  }
  BnkfPutM("XIS:PI", sizeof(int), &pi);

  pha = (int)floor(pha_d + 0.5);
  if ( pha < -1000 ) {
    pha = -1000;
  } else if ( 4095 < pha ) {
    pha = 4095;
  }
  BnkfPutM("XIS:PHA", sizeof(int), &pha);

  EvsfSetM("XISpha2pi:OK");

  *status = ANL_OK;
}

void
XISpha2pi_endrun(int *status)
{
  static long tlmin = 0, tlmax = 4095;

  fitsfile *fp;			/* for output evt fits */
  char history[2*PIL_LINESIZE];
  int icol;
  char key[FLEN_KEYWORD], comment[FLEN_COMMENT];

  int used = 0, istat = 0, editmode = 0;
  HKTEMP_PARAM *hp = &hktemp;
  struct aetime_aetemp *hk = hp->hk;

                          /*************************************************/
                          /* Write a HISTORY keyword into output fits file */
                          /*************************************************/
  BnkGet("XIS:OUTFITS:EVENTS:PTR", sizeof(fp), &used, &fp);
  if ( used != sizeof(fp) ) {
    *status = ANL_OK;
    return;
  }

  sprintf(history, "\
  enable_edge_smooth=%s", com.enable_edge_smooth ? "yes" : "no");
  sprintf(history, "\
  makepifile='%s'%s", com.makepifile,
	com.makepifile == com.o_makepifile ? "" : " (CALDB)");
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  hk_time_margin=%.1f  hk_aetemp_min=%.1f  hk_aetemp_max=%.1f",
	com.hk_time_margin, com.hk_aetemp_min, com.hk_aetemp_max);
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  hkfile='%s'", com.hkfile);
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
    nvalid=%d  nrej=%d  time=%.1lf - %.1lf [s]",
	hp->nrows, hp->nrej, hk[0].aetime, hk[hp->nrows-1].aetime);
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
    AE-temp: average=%.3f  sigma=%.3f  min=%.3f  max=%.3f [degC]",
	hp->aetemp_avg, hp->aetemp_sgm, hp->aetemp_min, hp->aetemp_max);
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
    evnum_exceed_time_margin=%d [events]", hp->evnum_exceed_time_margin);
  fits_write_history(fp, history, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
    *status = ANL_QUIT;
    return;
  }

  BnkfGetM("XIS:EDITMODE", sizeof(int), &used, &editmode);
/* throuth when editmode is darkinit,darkupdate */
  if ( editmode == XISeditDarkInit ||
       editmode == XISeditDarkUpdate ||
       editmode == XISeditFrame ||
       editmode == XISeditDarkFrame ) {
    *status = ANL_OK;
    return;
  }

  if ( fits_get_colnum(fp, CASESEN, "PI", &icol, &istat) ) {
    *status = ANL_OK;
    return;
  }
  anl_msg_info("\
%s: updating header keywords ...\n", pname);
  sprintf(key, "TLMIN%d", icol);
  fits_modify_key_lng(fp, key, tlmin, NULL, &istat);
  fits_read_key_lng(fp, key, &tlmin, comment, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_modify_key('%s') failed (%d)\n", pname, key, istat);
    *status = ANL_QUIT;
    return;
  }
  anl_msg_info("\
%-8s = %20ld / %s\n", key, tlmin, comment);

  sprintf(key, "TLMAX%d", icol);
  fits_modify_key_lng(fp, key, tlmax, NULL, &istat);
  fits_read_key_lng(fp, key, &tlmax, comment, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_modify_key('%s') failed (%d)\n", pname, key, istat);
    *status = ANL_QUIT;
    return;
  }
  anl_msg_info("\
%-8s = %20ld / %s\n", key, tlmax, comment);
  anl_msg_info("\n");

  *status = ANL_OK;
}

void
XISpha2pi_exit(int *status)
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
