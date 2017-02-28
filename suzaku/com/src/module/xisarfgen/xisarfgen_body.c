/*
    xisarfgen.c

    2009/10/14 Y.ISHISAKI	version 1.0
    2009/12/14 Y.ISHISAKI	version 1.1
    2010/03/30 Y.MAEDA	        version 1.2
    2011/04/21 Y.ISHISAKI	version 1.3
	consider y-flip, in making DET image from xrt_psf() in XRT coordinates
    2011/07/27 Y.ISHISAKI	version 1.4
        bug fix in allocating com.specresp in _init()
        use com.skyref instead of obs->skyref in _ana() & calc_eulinfo()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include "anl.h"
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "pil.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "aste_coord.h"
#include "aste_att.h"
#include "aste_gti.h"
#include "aste_caldb.h"
#include "aeFitsHeaderUtil.h"
#include "xis_contami.h"
#include "xisTelemFormat.h"
#include "xisSciUtil.h"
#include "xisPixelQuality.h"
#include "xis_effarea.h"
#include "xis_psf.h"

static char pname[] = "xisarfgen";
char xisarfgen_version[] = "version 1.4";

#define MAX_REGION		200
#define MAX_ENERGY		10000
#define MAX_TIME_STEP		1000000

#define TRANSMISSION_INDEX_SEARCH /* fast index search for transmission */
#define CONTAMI_FAST_CALC /* no convolution with PSF & XIS contami */

#define unless(a) if(!(a))

typedef struct {
  double gti_min_sec;
  double winexp;
  double t;
  int flag_end;
  long irow;
  long nrows;
  int col_start;
  int col_stop;
  int idiv;
  int ndiv;
  double t0;
  double *tbuf;
} EXPOSURE_INFO;

typedef struct {
  double t;
  AtEulerAng ea;
  int detx;
  int dety;
  int nexp;
} EULER_INFO;

typedef struct {
  double cor_lo;
  double cor_hi;
  double expo;
  double spec_count;
  double image_count;
  double *spec;
  double *serr;
  double *image;
} SORT_INFO;

typedef struct {
  int spec_siz;			/* 0 or 4096 */
  int image_siz;		/* 0 or 1024x1024 */
  int skyimage_siz;
  int det_xsiz, det_ysiz;
  int sky_xsiz, sky_ysiz;
  int win_st, win_siz, ci;	/* header keywords */
  int mjdrefi;
  double mjdreff;
  char areascal[FLEN_CARD], backscal[FLEN_CARD], corrscal[FLEN_CARD];
  int nexp;
  int neuler;
  EULER_INFO *eulinfo;
  SKYREF skyref;
  int nsort;
  EXPOSURE_INFO expinfo;
  long nrows, nrej_time, nrej_grade, nrej_pixq, nrej_detxy, nrej_pi;
  double aetime_start, aetime_stop, aetime_average;
  double exposure_before_time_min, exposure_after_time_max;
  double total_expo;
  double total_count;
  double *wmap;			/* WMAP in DET coordinates */
  double *skyimage;
  SORT_INFO *sp;		/* array of (nsort + 1) */
} SORT_DATA;

typedef struct {
  char telescop[FLEN_KEYWORD];
  char instrume[FLEN_KEYWORD];
  double equinox;
  char radecsys[FLEN_KEYWORD];
  char hduclas1[FLEN_KEYWORD], hduclas2[FLEN_KEYWORD];
  char mtype1[FLEN_KEYWORD], mform1[FLEN_KEYWORD];
  char ctype1[FLEN_KEYWORD], ctype2[FLEN_KEYWORD];
  double crpix1, crpix2;
  double cdelt1, cdelt2;
  double crval1, crval2, crota2; /* SKYREF */
  int wmrebin, x_offset, y_offset; /* WMAP keywords */
} FITS_IMAGE_KEY;

enum skyref_mode {
  SKYREF_MODE_AUTO,
  SKYREF_MODE_USER
};

enum source_mode {
  SOURCE_MODE_J2000,
  SOURCE_MODE_SKYXY,
};

enum region_mode {
  REGION_MODE_SKYFITS,
  REGION_MODE_SKYCIRC,
  REGION_MODE_SKYREG,
  REGION_MODE_SKYEXPR,
};

static PIXQ_INFO pixq;
static PIXQ_STAT statistics;

static struct {
  char telescop[FLEN_KEYWORD];
  char instrume[FLEN_KEYWORD];
  char *teldeffile, o_teldeffile[PIL_LINESIZE];
  char *leapfile, o_leapfile[PIL_LINESIZE];

  char *effareafile, o_effareafile[PIL_LINESIZE];
  double geomarea;

  char *psffile, o_psffile[PIL_LINESIZE];

  char *shieldfile, o_shieldfile[PIL_LINESIZE];
  struct {
    int nrow;
    double *en, *va;
#ifdef TRANSMISSION_INDEX_SEARCH
    struct trans_index {
      double norm, offs;
      int nbody;		/* in fact, sizeof(body)-1 */
      int *body;		/* 0-nbody */
    } index;
#endif
  } trans;

  enum skyref_mode pointing;	/* auto/user */
  SKYREF skyref;

  enum source_mode source_mode;
  char source_image[PIL_LINESIZE];
  FITS_IMAGE_KEY source_kp;
  double source_x, source_y;
  double source_ra, source_dec;
  AtRotMat sky_rm;

  int num_region;
  enum region_mode region_mode;
  double region_x[MAX_REGION], region_y[MAX_REGION];
  double region_rmin[MAX_REGION], region_rmax[MAX_REGION];
  char regfile[MAX_REGION][PIL_LINESIZE];
  char arffile[MAX_REGION][PIL_LINESIZE];
  fitsfile *arffp[MAX_REGION];
  int region_nx, region_ny;
  float *regmap[MAX_REGION];
  double *wmap[MAX_REGION];
  FITS_IMAGE_KEY region_kp[MAX_REGION];
  double sum_source_reg[MAX_REGION];
  double sum_detmask_reg[MAX_REGION];
  double tot_ccd_pix_reg[MAX_REGION];

  char detmask[PIL_LINESIZE];
  FITS_IMAGE_KEY detmask_kp;
  int ndetx, ndety;
  float *detmaskmap;
  double sum_detmask;
  double tot_ccd_pix;

  char phafile[PIL_LINESIZE];

  char gtifile[PIL_LINESIZE];
  char date_obs[PIL_LINESIZE];
  double photon_time;
  double gti_min_sec;

  char attitude[PIL_LINESIZE];
  AtEulerAng ea_deg, ea;
  int aberration;

  char *rmffile, o_rmffile[PIL_LINESIZE];
  int num_energ;
  double *energ_lo;		/* ENERG_LO in RMF/ARF */
  double *energ_hi;		/* ENERG_HI in RMF/ARF */
  double *energ_cen;		/* (ENERG_HI+ENERG_HI)/2.0 */

  char *contamifile, o_contamifile[PIL_LINESIZE];
  XIS_CONTAMI *xcp;

  int enable_pixq;
  char hotpixfiles[PIL_LINESIZE];
  char *badcolumfile, o_badcolumfile[PIL_LINESIZE];
  char *calmaskfile, o_calmaskfile[PIL_LINESIZE];
  unsigned int pixq_min;
  unsigned int pixq_max;
  unsigned int pixq_and;
  unsigned int pixq_eql;

  int clobber;			/* overwrite arf files or not */

  TELDEF *teldef;
  SORT_DATA obs;

  double *specresp[MAX_REGION];
  double *xrt_effarea[MAX_REGION];
  double *shield_trans[MAX_REGION];
  double *contami_trans[MAX_REGION];

} com;

static int
write_history(fitsfile *fp)
{
  int i;
  char history[PIL_LINESIZE + FLEN_VALUE];
  AtRightAscension ra;
  AtDeclination dec;
  HOTPIXFILE_INFO *hp;

  int istat = 0;
  PIXQ_INFO *p = &pixq;

  sprintf(history, "\
  phafile='%s'", com.phafile);
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  teldef='%s'%s", com.teldeffile,
	(com.teldeffile == com.o_teldeffile) ? "" : " (CALDB)");
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  leapfile='%s'%s", com.leapfile,
	(com.leapfile == com.o_leapfile) ? "" : " (CALDB)");
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  pointing=%s", (SKYREF_MODE_AUTO == com.pointing) ? "AUTO" : "USER");
  fits_write_history(fp, history, &istat);
  atDegToRA(com.skyref.alpha, &ra);
  sprintf(history, "\
  ref_alpha=%.4f (%02dh%02dm%06.3fs)", com.skyref.alpha,
	ra.hour, ra.min, ra.sec);
  fits_write_history(fp, history, &istat);
  atDegToDec(com.skyref.delta, &dec);
  sprintf(history, "\
  ref_delta=%.4f (%s%02dd%02dm%05.2fs)", com.skyref.delta,
	(dec.sign < 0) ? "-" : "+", dec.deg, dec.min, dec.sec);
  fits_write_history(fp, history, &istat);
  sprintf(history, "\
  ref_roll=%.4f", com.skyref.roll);
  fits_write_history(fp, history, &istat);

  sprintf(history, "\
  aberration=%s", com.aberration ? "yes" : "no");
  fits_write_history(fp, history, &istat);

  switch ( com.source_mode ) {
  case SOURCE_MODE_J2000:
    sprintf(history, "\
  source_mode=J2000  source_ra=%.6f  source_dec=%.6f",
	com.source_ra, com.source_dec);
    fits_write_history(fp, history, &istat);
    break;
  case SOURCE_MODE_SKYXY:
    sprintf(history, "\
  source_mode=SKYXY  source_x=%.1f  source_y=%.1f",
	com.source_x, com.source_y);
    fits_write_history(fp, history, &istat);
    break;
  default:
    ;
  }

  switch ( com.region_mode ) {
  case REGION_MODE_SKYFITS:
    sprintf(history, "\
  region_mode=SKYFITS  num_region=%d", com.num_region);
    fits_write_history(fp, history, &istat);
    for (i = 0; i < com.num_region; i++) {
      sprintf(history, "\
  regfile%d='%s'", i+1, com.regfile[i]);
      fits_write_history(fp, history, &istat);
      sprintf(history, "\
  arffile%d='%s'", i+1, com.arffile[i]);
      fits_write_history(fp, history, &istat);
    }
    break;
  case REGION_MODE_SKYCIRC:
    sprintf(history, "\
  region_mode=SKYCIRC  num_region=%d", com.num_region);
    fits_write_history(fp, history, &istat);
    for (i = 0; i < com.num_region; i++) {
      sprintf(history, "\
  %d: ( %.1f , %.1f )  %.1f - %.1f", i+1,
	com.region_x[i], com.region_y[i],
	com.region_rmin[i], com.region_rmax[i]);
      fits_write_history(fp, history, &istat);
      sprintf(history, "\
  arffile%d='%s'", i+1, com.arffile[i]);
      fits_write_history(fp, history, &istat);
    }
    break;
  case REGION_MODE_SKYREG:
    sprintf(history, "\
  region_mode=SKYREG  num_region=%d", com.num_region);
    fits_write_history(fp, history, &istat);
    for (i = 0; i < com.num_region; i++) {
      sprintf(history, "\
  regfile%d='%s'", i+1, com.regfile[i]);
      fits_write_history(fp, history, &istat);
      sprintf(history, "\
  arffile%d='%s'", i+1, com.arffile[i]);
      fits_write_history(fp, history, &istat);
    }
    break;
  case REGION_MODE_SKYEXPR:
    sprintf(history, "\
  region_mode=SKYEXPR  num_region=%d", com.num_region);
    fits_write_history(fp, history, &istat);
    for (i = 0; i < com.num_region; i++) {
      sprintf(history, "\
  regfile%d='%s'", i+1, com.regfile[i]);
      fits_write_history(fp, history, &istat);
      sprintf(history, "\
  arffile%d='%s'", i+1, com.arffile[i]);
      fits_write_history(fp, history, &istat);
    }
    break;
  default:
    ;
  }

  sprintf(history, "\
  detmask='%s'", com.detmask);
  fits_write_history(fp, history, &istat);
  if ( 0 != CLstricmp("none", com.detmask) ) {
    sprintf(history, "\
    MASK_RATIO_CCD = %.1f / %.0f = %.6f",
	    com.sum_detmask, com.tot_ccd_pix, com.sum_detmask/com.tot_ccd_pix);
    fits_write_history(fp, history, &istat);
  }

  if ( 0 == CLstricmp("NONE", com.gtifile) ||
       0 == CLstricmp("USER", com.gtifile) ||
       0 ) {
    sprintf(history, "\
  gtifile='%s'  date_obs='%s' (t=%.1f)",
	com.gtifile, com.date_obs, com.photon_time);
  } else {
    sprintf(history, "\
  gtifile='%s'", com.gtifile);
    fits_write_history(fp, history, &istat);
    sprintf(history, "\
  gti_min_sec=%.3f", com.gti_min_sec);
  }
  fits_write_history(fp, history, &istat);

  if ( 0 == CLstricmp("NONE", com.attitude) ||
       0 == CLstricmp("USER", com.attitude) ||
       0 ) {
    sprintf(history, "\
  attitude='%s'  ea1=%.4f  ea2=%.4f  ea3=%.4f", com.attitude,
	com.ea.phi*RAD2DEG, com.ea.theta*RAD2DEG, com.ea.psi*RAD2DEG);
  } else {
    sprintf(history, "\
  attitude='%s'", com.attitude);
  }
  fits_write_history(fp, history, &istat);

  sprintf(history, "\
  rmffile='%s'%s", com.rmffile,
	(com.rmffile == com.o_rmffile) ? "" : " (CALDB)");
  fits_write_history(fp, history, &istat);

  sprintf(history, "\
  contamifile='%s'%s", com.contamifile,
	(com.contamifile == com.o_contamifile) ? "" : " (CALDB)");
  fits_write_history(fp, history, &istat);

  sprintf(history, "\
  shieldfile='%s'%s", com.shieldfile,
	(com.shieldfile == com.o_shieldfile) ? "" : " (CALDB)");
  fits_write_history(fp, history, &istat);

  sprintf(history, "\
  effareafile='%s'%s", com.effareafile,
	(com.effareafile == com.o_effareafile) ? "" : " (CALDB)");
  fits_write_history(fp, history, &istat);

  sprintf(history, "\
  psffile='%s'%s", com.psffile,
	(com.psffile == com.o_psffile) ? "" : " (CALDB)");
  fits_write_history(fp, history, &istat);

  sprintf(history, "\
  enable_pixq=%s  clobber=%s",
	com.enable_pixq ? "yes" : "no", com.clobber ? "yes" : "no");
  fits_write_history(fp, history, &istat);

  if ( com.enable_pixq ) {
    sprintf(history, "\
  hotpixfiles='%s'", com.hotpixfiles);
    fits_write_history(fp, history, &istat);
    for (i = 0; i < p->num_hotpixfile; i++) {
      hp = &p->hotpixfile_list[i];
      sprintf(history, "\
    %d: '%s'", i+1, hp->filename);
      fits_write_history(fp, history, &istat);
      sprintf(history, "\
       num_hotpix=%ld  num_added=%ld  num_dupli=%ld",
	hp->num_hotpix, hp->num_added, hp->num_dupli);
      fits_write_history(fp, history, &istat);
    }
    sprintf(history, "\
  badcolumfile='%s'%s", com.badcolumfile,
	(com.badcolumfile == com.o_badcolumfile) ? "" : " (CALDB)");
    fits_write_history(fp, history, &istat);
    sprintf(history, "\
  calmaskfile='%s'%s", com.calmaskfile,
	(com.calmaskfile == com.o_calmaskfile) ? "" : " (CALDB)");
    fits_write_history(fp, history, &istat);
    sprintf(history, "\
  pixq_min=%u (0x%08x)  pixq_max=%u (0x%08x)",
	com.pixq_min, com.pixq_min, com.pixq_max, com.pixq_max);
    fits_write_history(fp, history, &istat);
    sprintf(history, "\
  pixq_and=%u (0x%08x)  pixq_eql=%u (0x%08x)",
	com.pixq_and, com.pixq_and, com.pixq_eql, com.pixq_eql);
    fits_write_history(fp, history, &istat);
  }

  if ( com.enable_pixq ) {
    istat = xisPixqStatWriteFits(&statistics, fp);
  }

  return istat;
}

static int
write_arf_wmap(fitsfile *fp, int ireg)
{
  char *k;
  double *wmap;
  int istat, hdutype;
  double /*source_ratio_reg, */mask_ratio_ccd, mask_ratio_reg;

  istat = 0;

  fits_movabs_hdu(fp, 1, &hdutype, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: move to primary array failed (%d)\n", pname, istat);
    return istat;
  }
  wmap = com.wmap[ireg];
  fits_write_img_dbl(fp, 1, 1, com.region_nx * com.region_ny, wmap, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: writing wmap failed (%d)\n", pname, istat);
    return istat;
  }

  fits_movabs_hdu(fp, 2, &hdutype, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: move to 1st extension failed (%d)\n", pname, istat);
    return istat;
  }

/*
  source_ratio_reg = com.sum_source_reg[ireg] / com.n_photon;
  if ( 0.1 <= source_ratio_reg ) {
    fits_modify_key_fixdbl(fp, k="SOURCE_RATIO_REG", source_ratio_reg, 6, "\
source image ratio inside selected region", &istat);
  } else {
    fits_modify_key_dbl(fp, k="SOURCE_RATIO_REG", source_ratio_reg, 6, "\
source image ratio inside selected region", &istat);
  }
  if ( istat ) {
    anl_msg_error("\
%s: fits_modify_key('%s') failed (%d)\n", pname, k, istat);
    return istat;
  }
*/

  if ( NULL != com.detmaskmap ) {
    mask_ratio_ccd = com.sum_detmask / com.tot_ccd_pix;
    mask_ratio_reg = com.sum_detmask_reg[ireg] / com.tot_ccd_pix_reg[ireg];
    if (
	fits_modify_key_fixdbl(fp, k="MASK_RATIO_CCD", mask_ratio_ccd, 6, "\
detmask ratio in whole CCD area", &istat) ||
	fits_modify_key_fixdbl(fp, k="MASK_RATIO_REG", mask_ratio_reg, 6, "\
detmask ratio inside selected region", &istat) ||
	0 ) {
      anl_msg_error("\
%s: fits_modify_key('%s') failed (%d)\n", pname, k, istat);
      return istat;
    }
  }

  return istat;
}

static int
create_wmap(fitsfile *fp, TELDEF *teldef)
{
  static struct {
    char *key, *value, *comment;
  } *p, keys[] = {
    { "TELESCOP", "SUZAKU",	"mission/satellite name" },
    { "INSTRUME", "XIS",	"instrument/detector name" },
    { "HDUCLASS", "OGIP",	"format conforms to OGIP standard" },
    { "HDUCLAS1", "IMAGE",	"dataset relates to spectral response" },
    { "HDUVERS1", "1.0.0",	"Version of family of formats" },
    { "HDUCLAS2", "WMAP",	"dataset contains Weighted Map Image" },
    { "HDUVERS2", "1.0.0",	"Version of format" },
    { "WCSNAMEP", "PHYSICAL",	"" },
    { "WCSTY1P",  "PHYSICAL",	"" },
    { "WCSTY2P",  "PHYSICAL",	"" },
    { "CTYPE1P",  "",		"Source of X-axis" },
    { "CTYPE2P",  "",		"Source of Y-axis" },
  };
  int istat = 0;
  int naxis = 2;
  long naxes[2];
  int ikey, nkey = sizeof(keys) / sizeof(*keys);
  TELDEF_ASTROE *aste = com.teldef->mission.aste;
  double cdelt1 = - (aste->sky.xscl / aste->focallen) * RAD2DEG;
  double cdelt2 = + (aste->sky.yscl / aste->focallen) * RAD2DEG;

  keys[0].value = teldef->telescop;
  keys[1].value = teldef->instrume;

  switch ( com.region_mode ) {
  case REGION_MODE_SKYFITS:
  case REGION_MODE_SKYCIRC:
  case REGION_MODE_SKYREG:
  case REGION_MODE_SKYEXPR:
    keys[nkey-2].value = "X";	/* CTYPE1P */
    keys[nkey-1].value = "Y";	/* CTYPE2P */
    naxes[0] = aste->sky.xsiz;
    naxes[1] = aste->sky.ysiz;
    break;
  }

  fits_create_img(fp, FLOAT_IMG, naxis, naxes, &istat);

  for (ikey = 0; ikey < nkey; ikey++) {
    p = &keys[ikey];
    fits_write_key_str(fp, p->key, p->value, p->comment, &istat);
  }

  fits_write_key_lng(fp, "CRPIX1P", 1,
	"X axis ref pixel of physical coord", &istat);
  fits_write_key_lng(fp, "CRVAL1P", 1,
	"physical coord of X ref pixel", &istat);
  fits_write_key_lng(fp, "CDELT1P", 1,
	"X axis increment of physical coord", &istat);
  fits_write_key_lng(fp, "CRPIX2P", 1,
	"Y axis ref pixel of physical coord", &istat);
  fits_write_key_lng(fp, "CRVAL2P", 1,
	"physical coord of Y ref pixel", &istat);
  fits_write_key_lng(fp, "CDELT2P", 1,
	"Y axis increment of physical coord", &istat);

  switch ( com.region_mode ) {
  case REGION_MODE_SKYFITS:
  case REGION_MODE_SKYCIRC:
  case REGION_MODE_SKYREG:
  case REGION_MODE_SKYEXPR:
    fits_write_key_str(fp, "RADECSYS", "FK5",
	"celestial coord system", &istat);
    fits_write_key_fixdbl(fp, "EQUINOX", 2000.0, 1,
	"Equinox of celestial coord system", &istat);
    fits_write_key_str(fp, "MTYPE1", "SKY",
	"DM Keyword: Descriptor name", &istat);
    fits_write_key_str(fp, "MFORM1", "X,Y",
	"DM Keyword: Descriptor value", &istat);
    fits_write_key_str(fp, "CTYPE1", "RA---TAN",
	"X coordinate projection", &istat);
    fits_write_key_fixdbl(fp, "CRPIX1", aste->sky.xcen, 1,
	"X reference pixel", &istat);
    fits_write_key_fixdbl(fp, "CRVAL1", com.skyref.alpha, 5,
	"X reference pixel value (deg)", &istat);
    fits_write_key_fixdbl(fp, "CDELT1", cdelt1, 7,
	"X pixel scale (deg/pixel)", &istat);
    fits_write_key_str(fp, "CTYPE2", "DEC--TAN",
	"Y coordinate projection", &istat);
    fits_write_key_fixdbl(fp, "CRPIX2", aste->sky.ycen, 1,
	"Y reference pixel", &istat);
    fits_write_key_fixdbl(fp, "CRVAL2", com.skyref.delta, 5,
	"Y reference pixel value (deg)", &istat);
    fits_write_key_fixdbl(fp, "CDELT2", cdelt2, 7,
	"Y pixel scale (deg/pixel)", &istat);
    fits_write_key_fixdbl(fp, "CROTA2", com.skyref.roll, 5,
	"Sky coord rotation angle (deg)", &istat);
    break;
  }

  return istat;
}

static fitsfile *
create_arf_file(char *arffile)
{
#define NCOLS	6
  static int tfields = NCOLS;
  static char extname[] = "SPECRESP";
  static char *ttype[NCOLS] = {
    "ENERG_LO", "ENERG_HI",
    "SPECRESP", "XRT_EFFAREA", "SHIELD_TRANSMIS", "CONTAMI_TRANSMIS"
  };
  static char *tform[NCOLS] = {
    "1E", "1E",
    "1E", "1E", "1E", "1E"
  };
  static char *tunit[NCOLS] = {
    "keV", "keV",
    "cm**2", "cm**2", "", ""
  };
#undef NCOLS
  static struct {
    char *key, *value, *comment;
  } *p, keys[] = {
    { "TELESCOP",	"SUZAKU",	"mission/satellite name" },
    { "INSTRUME",	"XIS",		"instrument/detector name" },
    { "ARFVERSN",	"1992a",	"OGIP classification of FITS format" },
    { "HDUCLASS",	"OGIP",		"format conforms to OGIP standard" },
    { "HDUCLAS1",	"RESPONSE",	"dataset relates to spectral response" },
    { "HDUVERS1",	"1.0.0",	"Version of family of formats" },
    { "HDUCLAS2",	"SPECRESP",	"dataset contains spectral response" },
    { "HDUVERS2",	"1.1.0",	"Version of format (OGIP memo CAL/GEN/92-002a)" }
  };
  int ikey, nkey = sizeof(keys) / sizeof(*keys);

  fitsfile *fp;
  char *k, creator[PIL_LINESIZE];
  int istat;

  istat = 0;
  fits_create_file(&fp, arffile, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: creating ARF file '%s' failed (%d)\n", pname, arffile, istat);
    return NULL;
  }

  istat = create_wmap(fp, com.teldef);
  if ( istat ) {
    anl_msg_error("\
%s: creating WMAP failed for '%s' (%d)\n", pname, arffile, istat);
    return NULL;
  }

  fits_create_tbl(fp, BINARY_TBL,
	com.num_energ, tfields, ttype, tform, tunit, extname, &istat);

  keys[0].value = com.teldef->telescop;
  keys[1].value = com.teldef->instrume;

  for (ikey = 0; ikey < nkey; ikey++) {
    p = &keys[ikey];
    fits_write_key_str(fp, p->key, p->value, p->comment, &istat);
  }
  if ( istat ) {
    anl_msg_error("\
%s: header write error for '%s'\n", pname, arffile);
    return NULL;
  }

  if ( com.enable_pixq ) {
    istat = xisSciWriteKeys(fp, &pixq.sci);
    if ( istat ) return NULL;
  }

  fits_write_key_fixdbl(fp, "GEOMAREA", com.geomarea, 4,
	"geometrical area of XRT (cm2)", &istat);
  if ( istat ) {
    anl_msg_error("\
%s: failed to write ARF key GEOMAREA (%d)\n", pname, istat);
    return NULL;
  }

  fits_write_key_str(fp, "TELDEF", com.teldef->filename,
	"name of the telescope definition file", &istat);
  if ( istat ) {
    anl_msg_error("\
%s: failed to write ARF key TELDEF (%d)\n", pname, istat);
    return NULL;
  }

  fits_write_key_str(fp, k="LEAPFILE", aefits_basename(com.leapfile),
	"name of the leap second file", &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_key_str('%s') failed (%d)\n", pname, k, istat);
    return NULL;
  }

  sprintf(creator, "%s version %s", anl_task_name(), anl_task_version());
  fits_write_key_str(fp, "CREATOR", creator,
	"software that created this file", &istat);
  if ( istat ) {
    anl_msg_error("\
%s: failed to write ARF key CREATOR (%d)\n", pname, istat);
    return NULL;
  }

  istat = aefits_write_module_history(fp, pname);
  if ( istat ) return NULL;

  istat = write_history(fp);
  if ( istat ) return NULL;

/*
  if (
      fits_write_key_null(fp, k="SOURCE_RATIO_REG", NULL, &istat) ||
      0 ) {
    anl_msg_error("\
%s: fits_write_key_null('%s') failed (%d)\n", pname, k, istat);
    return NULL;
  }
*/

  if ( NULL != com.detmaskmap ) {
    if (
	 fits_write_key_null(fp, k="MASK_RATIO_CCD", NULL, &istat) ||
	 fits_write_key_null(fp, k="MASK_RATIO_REG", NULL, &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_write_key_null('%s') failed (%d)\n", pname, k, istat);
      return NULL;
    }
  }

  return fp;
}

static int
read_shield_file(char *shieldfile)
{
  int hdutype;
  fitsfile *fp;

  int istat = 0;

  fits_open_file(&fp, shieldfile, READONLY, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: XRT thermal shield transmission '%s' open failed\n", pname, shieldfile);
    return -1;
  }

  for (;;) {
#ifdef TRANSMISSION_INDEX_SEARCH
    int i, j, k, nbody, *body;
    double offs, norm;
#endif
    int ne, col_energy, col_trans, anynul;

    istat = 0;
    fits_movrel_hdu(fp, 1, &hdutype, &istat);
    if ( istat ) {
      fits_close_file(fp, &istat);
      anl_msg_error("\
%s: no transmission data in '%s'\n", pname, shieldfile);
      return -1;
    }
    fits_read_key(fp, TINT, "NAXIS2", &ne, NULL, &istat);
    fits_get_colnum(fp, CASEINSEN, "ENERGY", &col_energy, &istat);
    fits_get_colnum(fp, CASEINSEN, "TRANSMIS", &col_trans, &istat);
    if ( istat ) {
      istat = 0;
      fits_get_colnum(fp, CASEINSEN, "TRANSMISSION", &col_trans, &istat);
      if ( istat ) {
	anl_msg_error("\
%s: fits_get_colunm('TRANSMIS') failed (%d)\n", pname, istat);
	return istat;
      }
    }
    com.trans.en = malloc(2 * sizeof(double) * ne
#ifdef TRANSMISSION_INDEX_SEARCH
	+ sizeof(*com.trans.index.body) * (com.trans.index.nbody + 1)
#endif
	);
    com.trans.va = &com.trans.en[ne];
    if ( NULL == com.trans.en ) {
      anl_msg_error("\
%s: Energy/Transmission malloc failed for '%s'\n", pname, shieldfile);
      return -1;
    }
    ffgcvd(fp, col_energy, 1, 1, ne, 0.0, com.trans.en, &anynul, &istat);
    ffgcvd(fp, col_trans,  1, 1, ne, 0.0, com.trans.va, &anynul, &istat);
    fits_close_file(fp, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: Energy/Transmission column read error for '%s'\n", pname, shieldfile);
      return -1;
    }

#ifdef TRANSMISSION_INDEX_SEARCH
    nbody = com.trans.index.nbody;
    body = com.trans.index.body = (int *)( &com.trans.va[ne] );
    offs = com.trans.index.offs = com.trans.en[0];
    norm = com.trans.index.norm = com.trans.en[ne-1];
    for (i = j = 0; i < ne - 1; i++) {
      k = nbody * ( com.trans.en[i] - offs ) / norm;
      while ( j <= k ) body[j++] = i;
    }
    while ( j < nbody+1 ) body[j++] = i;
#endif

    return 0;
  }
}

static double
shield_transmission_at(double energy)
{
  double trans, x0, x1, y0, y1;

#ifdef TRANSMISSION_INDEX_SEARCH
  int ipos, ie, ne;
  struct trans_index *idx = &com.trans.index;

  ipos = idx->nbody * ( energy - idx->offs ) / idx->norm;
  if ( ipos < 0 || idx->nbody <= ipos ) return 0.0; /* out of bounds */
  ne = com.trans.nrow - 1;

  for (ie = idx->body[ipos]; ie < ne; ie++) {
    if ( energy < com.trans.en[ie] ) break;
  }

  if ( ie <= 0 ) return 0.0;

  x0 = com.trans.en[ie-1];
  x1 = com.trans.en[ie];
  y0 = com.trans.va[ie-1];
  y1 = com.trans.va[ie];

  trans = (y0 * (x1 - energy) + y1 * (energy - x0)) / (x1 - x0);
  /*
     if ( energy < x0 || x1 < energy ) {
     printf("transmission_at: index search failed !!!\n");
     }
     printf("\
     ie=%d, ipos=%d, nsearch=%d, x0=%f, energy=%f, x1=%f\n",
     ie, ipos, ie - idx->body[ipos], x0, energy, x1);
     */

#else

  int i, n;

  if ( energy < com.trans.en[0] ) {
    return 0.0;
  }

  trans = 0.0;
  n = com.trans.nrow - 1;
  for (i = 0; i < n; i++) {
    if ( energy <= com.trans.en[i+1] ) {
      x0 = com.trans.en[i];
      x1 = com.trans.en[i+1];
      y0 = com.trans.va[i];
      y1 = com.trans.va[i+1];
      trans = (y0 * (x1 - energy) + y1 * (energy - x0)) / (x1 - x0);
      break;
    }
  }

#endif

  return trans;
}

static int
get_next_exptime_gti(fitsfile *fp, EXPOSURE_INFO *p)
{
  char *k;
  int anul;
  double start, stop, span;

  int istat = 0;

  p->flag_end = 0;

  if ( 0 == p->irow ) {
    fits_get_num_rows(fp, &p->nrows, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_get_num_rows() failed (%d)\n", pname, istat);
      goto quit;
    }
    if ( p->nrows < 1 ) {
      anl_msg_error("\
%s: empty GTI extension\n", pname);
      istat = -1;
      goto quit;
    }
    if (
	 fits_get_colnum(fp, CASESEN, k="START", &p->col_start, &istat) ||
	 fits_get_colnum(fp, CASESEN, k="STOP", &p->col_stop, &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
      goto quit;
    }
    p->idiv = 0;
    anl_msg_info("\
%s: extracting time from GTI: ngti=%ld winexp=%.1f\n",
	pname, p->nrows,  p->winexp);
  }

  if ( 0 == p->idiv ) {
 again:
    p->irow++;
    if ( p->nrows < p->irow ) {
      p->flag_end = 1;
      return 0;
    }
    fits_read_col_dbl(fp, p->col_start, p->irow, 1, 1, 0.0,
	&start, &anul, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col('START') failed (%d)\n", pname, istat);
      goto quit;
    }
    fits_read_col_dbl(fp, p->col_stop, p->irow, 1, 1, 0.0,
	&stop, &anul, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_read_col('STOP') failed (%d)\n", pname, istat);
      goto quit;
    }
    span = stop - start;
    if ( span < p->gti_min_sec ) {
      anl_msg_warning("\
%s: WARNING: too short GTI %.1f - %.1f (%.3f) at irow=%ld\n",
	pname, start, stop, span, p->irow);
      goto again;
    }
    p->ndiv = (int)( (span + p->winexp - 0.1) / p->winexp );
    if ( p->ndiv <= 0 ) {
      anl_msg_warning("\
%s: WARNING: strange GTI at irow=%ld, %.0f - %.0f\n",
	pname, p->irow, start, stop);
      p->t = (start + stop) / 2;
      return 0;
    }
    p->t0 = start - (p->winexp * p->ndiv - span - p->winexp) / 2;
    anl_msg_debug("%.3f - %.3f span=%.3f t0=%.3f ndiv=%d\n",
	start, stop, span, p->t0, p->ndiv);
  }

  p->t = p->t0 + p->idiv * p->winexp;
  p->idiv++;
  if ( p->ndiv <= p->idiv ) {
    p->idiv = 0;		/* goto next line */
  }

  return 0;

 quit:
  return istat;
}

static int
compare_eulinfo(const void *v1, const void *v2)
{
#define detx1 (((EULER_INFO *)v1)->detx)
#define detx2 (((EULER_INFO *)v2)->detx)
#define dety1 (((EULER_INFO *)v1)->dety)
#define dety2 (((EULER_INFO *)v2)->dety)
  if ( detx1 < detx2 ) {
    return -1;
  } else if ( detx1 > detx2 ) {
    return +1;
  } else if ( dety1 < dety2 ) {
    return -1;
  } else if ( dety1 > dety2 ) {
    return +1;
  }
  return 0;
#undef detx1
#undef detx2
#undef dety1
#undef dety2
}

static void
EulerToVect(AtEulerAng *ea, AtVect v)
{
	double sin_t = sin(ea->theta);

	v[0] = sin_t * cos(ea->phi);
	v[1] = sin_t * sin(ea->phi);
	v[2] = cos(ea->theta);
}

static int
VectToEuler(AtVect v, AtEulerAng *ea)
{
#define x (v[0])
#define y (v[1])
#define z (v[2])

	double r2, x2y2, rxy;

	x2y2 = x*x + y*y;
	r2 = x2y2 + z*z;
	if ( 0.0 == r2 ) {
		return -1;
	}

	if ( 0.0 == x2y2 ) {
		ea->phi = 0.0;
	} else {
		ea->phi = atan2(y, x);	/* -PI <= atan2 <= PI */
		if ( ea->phi < 0.0 ) {
			ea->phi += TWO_PI;
		}
	}

	rxy = sqrt(x2y2);
	ea->theta = atan2(rxy, z);	/* 0 <= theta, because 0 <= rxy */

#undef x
#undef y
#undef z

	return 0;
}

static int
shrink_eulinfo(EULER_INFO *eulinfo, int n)
{
  int i, istat, nexp;
  double ax, x, ay, y, at;
  AtVect av_vec, vec;
  AtEulerAng *ea, av_ea;

  if ( 1 == n ) {
    return 0;		/* no need to shrink */
  }

  nexp = 0;
  ax = ay = at = 0.0;
  av_vec[0] = av_vec[1] = av_vec[2] = 0.0;
  for (i = 0; i < n; i++) {
    nexp += eulinfo[i].nexp;
    ea = &eulinfo[i].ea;
    EulerToVect(ea, vec);
    av_vec[0] += vec[0];
    av_vec[1] += vec[1];
    av_vec[2] += vec[2];
    x = cos(ea->psi);
    y = sin(ea->psi);
    ax += x;
    ay += y;
    at += eulinfo[i].t;
  }

  istat = VectToEuler(av_vec, &av_ea);
  if ( istat ) {
    av_ea = eulinfo->ea;
  }

  av_ea.psi = atan2(ay, ax);		/* -PI <= atan2 <= PI */
  if ( av_ea.psi < 0.0 ) {
    av_ea.psi += TWO_PI;
  }

  eulinfo->nexp = nexp;
  eulinfo->ea = av_ea;
  eulinfo->t = at / nexp;

  return 0;
}

static int
calc_eulinfo(TELDEF *teldef, ATTFILE *attfile, fitsfile *fp, double photon_time, SORT_DATA *obs)
{
  static EULER_INFO eulinfo_static;

  char *k;
  int iexp, nexp, neuler, nalloc, hdutype;
  int i, n, idetx, idety;
  EXPOSURE_INFO expinfo;
  int (*get_next_exptime)(fitsfile *fp, EXPOSURE_INFO *p);
  AtEulerAng ea;
  EULER_INFO *eulinfo, *ep;
  double detx, dety, alpha, delta;

  int istat = 0;

  if ( NULL == fp ) {
    obs->nexp = 1;
    obs->neuler = 1;
    obs->eulinfo = &eulinfo_static;
    obs->eulinfo->t = photon_time;
    if ( NULL == attfile ) {
      obs->eulinfo->ea = com.ea;
    } else {
      istat = aste_att_ea(attfile, photon_time, &obs->eulinfo->ea);
      if ( istat ) {
	anl_msg_error("\
%s: aste_att_ea(t=%.0f) failed (%d)\n", pname, photon_time, istat);
	goto quit;
      }
    }
    obs->eulinfo->nexp = 1;
    return 0;
  }

  fits_movabs_hdu(fp, 1, &hdutype, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_movabs_hdu('PRIMARY') failed (%d)\n", pname, istat);
    goto quit;
  }
  fits_read_key_dbl(fp, k="TIMEDEL", &expinfo.winexp, NULL, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  if ( expinfo.winexp <= 0.0 ) {
    anl_msg_error("\
%s: illegal keyword value of TIMEDEL=%.3f\n", pname, expinfo.winexp);
    istat = -1;
    goto quit;
  }
  expinfo.irow = 0;
  expinfo.gti_min_sec = com.gti_min_sec;

  get_next_exptime = get_next_exptime_gti;
  fits_movnam_hdu(fp, BINARY_TBL, "GTI", 0, &istat);
  if ( istat ) {
    istat = 0;			/* ignore error */
    fits_movnam_hdu(fp, BINARY_TBL, "STDGTI", 0, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: no EVENTS/GTI/STDGTI extension is found (%d)\n", pname, istat);
      goto quit;
    }
  }

  nexp = 0;
  nalloc = 0;
  eulinfo = NULL;
  for (;;) {
    istat = (*get_next_exptime)(fp, &expinfo);
    if ( istat ) goto quit;
    if ( expinfo.flag_end ) {
      break;
    }
    if ( NULL == attfile ) {
      ea = com.ea;
    } else {
      istat = aste_att_ea(attfile, expinfo.t, &ea);
      if ( istat ) {
	anl_msg_error("\
%s: aste_att_ea(t=%.0f) failed (%d)\n", pname, expinfo.t, istat);
	goto quit;
      }
    }
    if ( nalloc <= nexp ) {
      nalloc += 10000;
      eulinfo = realloc(eulinfo, sizeof(*eulinfo) * nalloc);;
      if ( NULL == eulinfo ) {
	anl_msg_error("\
%s: eulinfo realloc(size=%d) failed\n", pname, nalloc);
	istat = -1;
	goto quit;
      }
    }
    alpha = com.skyref.alpha;
    delta = com.skyref.delta;
    if ( com.aberration ) {
      aste_inv_aberration(expinfo.t, obs->mjdrefi, obs->mjdreff,
	&alpha, &delta);
    }
    aste_ecs2det(teldef, &ea, alpha, delta, &detx, &dety);
    eulinfo[nexp].t = expinfo.t;
    eulinfo[nexp].ea = ea;
    eulinfo[nexp].detx = (int)floor(detx);
    eulinfo[nexp].dety = (int)floor(dety);
    eulinfo[nexp].nexp = 1;
    nexp++;
  }

  qsort(eulinfo, nexp, sizeof(*eulinfo), compare_eulinfo);
  iexp = neuler = 0;
  while ( iexp < nexp ) {
    ep = &eulinfo[iexp];
    idetx = ep->detx;
    idety = ep->dety;
    for (i = iexp; i < nexp; i++) {
      if ( idetx != eulinfo[i].detx || idety != eulinfo[i].dety ) {
	break;
      }
      anl_msg_debug("\
  %4d: %d %d: (%.6f %.6f %.6f) t=%.1f\n",
	i+1, eulinfo[i].detx, eulinfo[i].dety,
	eulinfo[i].ea.phi*RAD2DEG, eulinfo[i].ea.theta*RAD2DEG,
	eulinfo[i].ea.psi*RAD2DEG, eulinfo[i].t);
    }
    n = i - iexp;
    shrink_eulinfo(ep, n);
    if ( neuler < iexp ) {
      eulinfo[neuler] = *ep;
    }
    iexp += n;
    neuler++;
    anl_msg_debug("\
%04d: n=%d: %d %d: (%.6f %.6f %.6f) t=%.1f\n",
	neuler, ep->nexp, ep->detx, ep->dety,
	ep->ea.phi*RAD2DEG, ep->ea.theta*RAD2DEG, ep->ea.psi*RAD2DEG,
	ep->t);
  }

  anl_msg_info("\
%s: nexp=%d shrink to neuler=%d\n", pname, nexp, neuler);

  obs->nexp = nexp;
  obs->neuler = neuler;
  obs->eulinfo = eulinfo;

  return 0;

 quit:
  return istat;
}

static int
check_keyword(fitsfile *fp, int flag_sky, int nx, int ny, FITS_IMAGE_KEY *kp)
{
  struct fits_key_list {
    char *key;
    int req, datatype;
    void *ptr;
  };

  static struct {
    struct fits_key_list
    telescop,
    instrume,
    object,
    exposure,
    equinox,
    radecsys,
    hduclas1,
    hduclas2,
    mtype1,
    mform1,
    ctype1,
    ctype2,
    cdelt1,
    cdelt2,
    crpix1,
    crpix2,
    crval1,
    crval2,
    crota2,
    wmrebin,
    x_offset,
    y_offset,
    end;
  } list = {
    { "TELESCOP",	0, TSTRING, NULL },
    { "INSTRUME",	0, TSTRING, NULL },
    { "OBJECT",		0, TSTRING, NULL },
    { "EXPOSURE",	0, TSTRING, NULL },
    { "EQUINOX",	1, TDOUBLE, NULL },
    { "RADECSYS",	1, TSTRING, NULL },
    { "HDUCLAS1",	0, TSTRING, NULL },
    { "HDUCLAS2",	0, TSTRING, NULL },
    { "MTYPE1",		0, TSTRING, NULL },
    { "MFORM1",		0, TSTRING, NULL },
    { "CTYPE1",		0, TSTRING, NULL },
    { "CTYPE2",		0, TSTRING, NULL },
    { "CDELT1",		1, TDOUBLE, NULL },
    { "CDELT2",		1, TDOUBLE, NULL },
    { "CRPIX1",		0, TDOUBLE, NULL },
    { "CRPIX2",		0, TDOUBLE, NULL },
    { "CRVAL1",		1, TDOUBLE, NULL },
    { "CRVAL2",		1, TDOUBLE, NULL },
    { "CROTA2",		0, TDOUBLE, NULL },
    { "WMREBIN",	0, TINT, NULL },
    { "X-OFFSET",	0, TINT, NULL },
    { "Y-OFFSET",	0, TINT, NULL },
    { NULL, 0, 0, NULL }
  };

  int i, nfound;
  char card[FLEN_CARD];

  struct fits_key_list *lp = &list.telescop;
  int istat = 0;

  if ( NULL == kp ) {
    return 0;
  }

  list.telescop.ptr = &kp->telescop;
  list.instrume.ptr = &kp->instrume;
  list.equinox.ptr  = &kp->equinox;
  list.radecsys.ptr = kp->radecsys;
  list.hduclas1.ptr = kp->hduclas1;
  list.hduclas2.ptr = kp->hduclas2;
  list.mtype1.ptr   = kp->mtype1;
  list.mform1.ptr   = kp->mform1;
  list.ctype1.ptr   = kp->ctype1;
  list.ctype2.ptr   = kp->ctype2;
  list.crpix1.ptr   = &kp->crpix1;
  list.crpix2.ptr   = &kp->crpix2;
  list.cdelt1.ptr   = &kp->cdelt1;
  list.cdelt2.ptr   = &kp->cdelt2;
  list.crval1.ptr   = &kp->crval1;
  list.crval2.ptr   = &kp->crval2;
  list.crota2.ptr   = &kp->crota2;
  list.wmrebin.ptr  = &kp->wmrebin;
  list.x_offset.ptr = &kp->x_offset;
  list.y_offset.ptr = &kp->y_offset;

  /* set initial values */
  kp->telescop[0] = '\0';
  kp->instrume[0] = '\0';
  kp->equinox     = 0.0;
  kp->radecsys[0] = '\0';
  kp->hduclas1[0] = '\0';
  kp->hduclas2[0] = '\0';
  kp->mtype1[0]   = '\0';
  kp->mform1[0]   = '\0';
  if ( flag_sky ) {
    strcpy(kp->ctype1, "RA---TAN");
    strcpy(kp->ctype2, "DEC--TAN");
  } else {
    kp->ctype1[0] = '\0';
    kp->ctype2[0] = '\0';
  }
  kp->crpix1      = (nx + 1) / 2.0;
  kp->crpix2      = (ny + 1) / 2.0;
  kp->cdelt1      = 0.0;
  kp->cdelt2      = 0.0;
  kp->crval1      = 0.0;
  kp->crval2      = 0.0;
  kp->crota2      = 0.0;
  kp->wmrebin     = 1;
  kp->x_offset    = 1;
  kp->y_offset    = 1;

  for (i = nfound = 0; NULL != lp[i].key; i++) {
    istat = 0;
    fits_read_card(fp, lp[i].key, card, &istat);
    if ( 0 == istat ) {
      nfound++;
      anl_msg_info("%s\n", card);
      if ( NULL != lp[i].ptr ) {
	if ( TDOUBLE == lp[i].datatype ) {
	  fits_read_key_dbl(fp, lp[i].key, lp[i].ptr, NULL, &istat);
	} else if ( TSTRING == lp[i].datatype ) {
	  fits_read_key_str(fp, lp[i].key, lp[i].ptr, NULL, &istat);
	} else if ( TINT == lp[i].datatype ) {
	  fits_read_key(fp, TINT, lp[i].key, lp[i].ptr, NULL, &istat);
	}
      }
    }
    if ( istat && flag_sky && 1 == lp[i].req ) {
      anl_msg_error("\
%s: required keyword '%s' not found\n", pname, lp[i].key);
      return -1;
    }
  }

  if ( flag_sky && 0 != strcmp("RA---TAN", kp->ctype1) ) {
    anl_msg_error("\
%s: CTYPE1 = '%s' not supported\n", pname, kp->ctype1);
    return -1;
  }

  if ( flag_sky && 0 != strcmp("DEC--TAN", kp->ctype2) ) {
    anl_msg_error("\
%s: CTYPE2 = '%s' not supported\n", pname, kp->ctype1);
    return -1;
  }

  if ( nfound ) {
    anl_msg_info("\
   %d keywords detected\n", nfound);
  }

  return 0;
}

static float *
read_fits_image(char *fn, int flag_sky, int *nx, int *ny, FITS_IMAGE_KEY *kp)
{
  fitsfile *fp;
  float *image;
  int simple, bitpix, naxis, extd, anynul, istat, nelem;
  long naxes[2], pc, gc;

  istat = 0;
  fits_open_file(&fp, fn, READONLY, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: FITS image '%s' open failed (%d)\n", pname, fn, istat);
    return NULL;
  }

  fits_read_imghdr(fp,2,&simple,&bitpix,&naxis,naxes,&pc,&gc,&extd,&istat);
  *nx = naxes[0];
  *ny = naxes[1];
  if ( naxis < 2 ) {
    *ny = 1;
  }
  unless ( 0 == istat && naxis && nx && ny ) return NULL;

  nelem = (*nx) * (*ny);
  image = malloc( nelem * sizeof(*image) );
  if ( NULL == image ) {
    anl_msg_error("\
%s: image[%dx%d] malloc() failed\n", pname, *nx, *ny);
    return NULL;
  }

  if ( NULL != kp ) {
    istat = check_keyword(fp, flag_sky, *nx, *ny, kp);
    if ( istat ) {
      free(image);
      return NULL;
    }
  }

  fits_read_img_flt(fp, 1, 1, nelem, 0.0, image, &anynul, &istat);
  fits_close_file(fp, &istat);
  if ( istat ) {
    anl_msg_info("\
%s: FITS image '%s' read error\n", pname, fn);
    free(image);
    return NULL;
  }

  return image;
}

static int
convert_act_to_det(TELDEF *teldef, int nx, int ny, float *act_image)
{
  int ix, iy, iactx, iacty, idetx, idety, iactpos, idetpos;
  float *det_image;
  TELDEF_ASTROE *aste = teldef->mission.aste;

  anl_msg_warning("\
%s: WARNING: image type is ACT, converting to DET\n", pname);

  if ( aste->det.xsiz != aste->act.xsiz ||
      aste->det.ysiz != aste->act.ysiz ) {
    anl_msg_error("\
%s: invalid teldef file, ACT_X|YSIZ != DET_X|YSIZ\n", pname);
    return -1;
  }

  if ( nx != aste->det.xsiz || ny != aste->det.ysiz ) {
    anl_msg_error("\
%s: invalid image size %dx%d\n", pname, nx, ny);
    return -1;
  }

  det_image = malloc( sizeof(*det_image) * nx * ny );
  if ( NULL == det_image ) {
    anl_msg_error("\
%s: det_image[%dx%d] malloc() failed\n", pname, nx, ny);
    return -1;
  }

  for (iy = 0; iy < ny; iy++) {
    iacty = iy + aste->act.xpix1;
    for (ix = 0; ix < nx; ix++) {
      iactx = ix + aste->act.ypix1;
      xis_act2det(teldef, iactx, iacty, &idetx, &idety);
      iactpos = iy * nx + ix;
      idetpos = (idety - aste->det.ypix1) * nx + idetx - aste->det.xpix1;
      det_image[idetpos] = act_image[iactpos];
    }
  }

  memcpy(act_image, det_image, nx * ny * sizeof(*det_image));
  free(det_image);
  return 0;
}

static int
read_detmask(char *detmask)
{
  int nx, ny, ipos, nxny;
  float *image;
  FITS_IMAGE_KEY *kp = &com.detmask_kp;

  com.ndetx = com.teldef->mission.aste->det.xsiz;
  com.ndety = com.teldef->mission.aste->det.ysiz;

  if ( 0 == CLstricmp("none", detmask) ) {
    com.detmaskmap = NULL;
    return 0;
  }

  anl_msg_info("\
%s: reading detmask '%s'\n", pname, detmask);

  image = read_fits_image(detmask, 0, &nx, &ny, kp);
  if ( NULL == image ) {
    return -1;
  } else if ( com.ndetx != nx || com.ndety != ny ) {
    anl_msg_error("\
%s: detmask size mismatch (nx=%d, ny=%d)\n", pname, nx, ny);
    free(image);
    return -1;
  } else if ( '\0' == *kp->ctype1 && '\0' == *kp->ctype2 ) {
    anl_msg_warning("\
%s: WARNING: CTYPE1, CTYPE2 is not specified, assuming DETX, DETY\n", pname);
  } else if ( 0 == CLstricmp(kp->ctype1, "DETX") &&
	      0 == CLstricmp(kp->ctype2, "DETY") ) {
    ;
  } else if ( 0 == CLstricmp(kp->ctype1, "ACTX") &&
	      0 == CLstricmp(kp->ctype2, "ACTY") ) {
    if ( convert_act_to_det(com.teldef, nx, ny, image) ) {
      free(image);
      return -1;
    }
  } else {
    anl_msg_error("\
%s: invalid CTYPE1='%s', CTYPE2='%s'\n", pname, kp->ctype1, kp->ctype2);
    free(image);
    return -1;
  }
  if ( *kp->telescop && 0 != strcmp(kp->telescop, com.telescop) ) {
    anl_msg_warning("\
%s: WARNING: TELESCOP keyword mismatch\n", pname);
  }
  if ( *kp->instrume && 0 != strcmp(kp->instrume, com.instrume) ) {
    anl_msg_warning("\
%s: WARNING: INSTRUME keyword mismatch\n", pname);
  }

  anl_msg_info("\
   %dx%d image read\n", nx, ny);

  nxny = nx * ny;
  com.tot_ccd_pix = nxny;
  com.sum_detmask = 0.0;
  for (ipos = 0; ipos < nxny; ipos++) {
    com.sum_detmask += image[ipos];
  }
  com.detmaskmap = image;

  anl_msg_info("\
   mask_ratio_ccd = %.1f / %.0f = %.6f\n",
	com.sum_detmask, com.tot_ccd_pix, com.sum_detmask/com.tot_ccd_pix);

  return 0;
}

static int
read_energ_lo_hi(char *rmffile)
{
  fitsfile *fp;
  char comment[80];
  int istat, istat2, hdunum;
  int ie, ne, col_lo, col_hi, anul;
  int exact = 1;

  istat = istat2 = 0;
  fits_open_file(&fp, rmffile, READONLY, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: rmffile '%s' open failed (%d)\n", pname, rmffile, istat);
    return istat;
  }

  fits_get_hdu_num(fp, &hdunum);
  if ( 1 == hdunum ) {		/* primary HDU */
    fits_movnam_hdu(fp, BINARY_TBL, "MATRIX", 0, &istat);
    if ( istat ) {
      istat = 0;
      fits_movnam_hdu(fp, BINARY_TBL, "SPECRESP MATRIX", 0, &istat);
      if ( istat ) {
	fits_close_file(fp, &istat2);
	anl_msg_error("\
%s: no MATRIX extension in '%s'\n", pname, rmffile);
	return istat;
      }
    }
  }

  fits_read_key(fp, TINT, "NAXIS2", &ne, comment, &istat);
  fits_get_colnum(fp, exact, "ENERG_LO", &col_lo, &istat);
  fits_get_colnum(fp, exact, "ENERG_HI", &col_hi, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: ENERG_LO/HI column not found in '%s'\n", pname, rmffile);
    fits_close_file(fp, &istat2);
    return istat;
  }

  com.num_energ = ne;
  com.energ_lo = malloc(3 * sizeof(double) * ne);
  com.energ_hi = com.energ_lo + ne;
  com.energ_cen = com.energ_hi + ne;
  if ( NULL == com.energ_lo ) {
    anl_msg_error("\
%s: ENERG_LO/HI malloc() failed for '%s'\n", pname, rmffile);
    fits_close_file(fp, &istat2);
    return istat;
  }

  fits_read_col_dbl(fp, col_lo, 1, 1, ne, 0.0, com.energ_lo, &anul, &istat);
  fits_read_col_dbl(fp, col_hi, 1, 1, ne, 0.0, com.energ_hi, &anul, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: ENERG_LO/HI column read error for '%s'\n", pname, rmffile);
    fits_close_file(fp, &istat2);
    free(com.energ_lo);
    com.energ_lo = NULL;
    return istat;
  }

  for (ie = 0; ie < ne; ie++) {
    com.energ_cen[ie] = (com.energ_lo[ie] + com.energ_hi[ie]) / 2.0;
  }

  fits_close_file(fp, &istat);

  return 0;
}

static int
read_contamifile(char *contamifile)
{
  int istat;

  if ( 0 == CLstricmp("NONE", contamifile) ) {
    com.xcp = NULL;
    return 0;
  }
  istat = xis_contami_init(&com.xcp, contamifile);

  return istat;
}

static float *
make_ds9reg_image(char *regfile, char *regexpr, int nx, int ny)
{
  static char exptmpl[] = "regfilter(\"%s\")";
  static char tmpfile[] = "mem://tmp.fits";
  static char extname[] = "DUMP_IMAGE";
  static char *ttype[2] = { "X",  "Y" };
  static char *tform[2] = { "1I", "1I"};
  static char *tunit[2] = { "",   ""  };

  char *k;
  int ix, iy, ipos;
  float *regmap;
  long irow, nrows, n_good_rows;
  double tcdltx, tcdlty;

  fitsfile *fp = NULL;
  char *regstat = NULL;
  int istat = 0, ncol = 2;
  TELDEF_ASTROE *aste = com.teldef->mission.aste;

  nrows = nx * ny;
  regmap = malloc( nrows * sizeof(*regmap) );
  if ( NULL == regmap ) {
    anl_msg_error("\
%s: regmap[%d x %d] malloc() failed\n", pname, nx, ny);
    goto error;
  }

  fits_create_file(&fp, tmpfile, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: could not open tmpfil; may already exist?\n", pname);
    goto error;
  }

  /* construct binary table header */
  fits_create_tbl(fp, BINARY_TBL, 0, ncol, ttype, tform, tunit, extname, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_create_tbl(tmpfil) failed (%d)\n", pname, istat);
    goto error;
  }

  if ( nx == aste->sky.xsiz && ny == aste->sky.ysiz ) {
    tcdltx = - (aste->sky.xscl / aste->focallen) * RAD2DEG;
    tcdlty = + (aste->sky.yscl / aste->focallen) * RAD2DEG;
    if (
fits_write_key_str(fp, k="RADECSYS", "FK5",
	"celestial coord system", &istat) ||
fits_write_key_fixdbl(fp, k="EQUINOX", 2000.0, 1,
	"Equinox of celestial coord system", &istat) ||
fits_write_key_fixdbl(fp, k="TCRPX1", aste->sky.xcen, 1,
	"X reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="TCRVL1", com.skyref.alpha, 5,
	"X reference pixel value (deg)", &istat) ||
fits_write_key_fixdbl(fp, k="TCDLT1", tcdltx, 7,
	"X pixel scale (deg/pixel)", &istat) ||
fits_write_key_fixdbl(fp, k="TCROT1", com.skyref.roll, 5,
	"X pixel rotation (deg)", &istat) ||
fits_write_key_str(fp, k="TCTYP1", "RA---TAN",
	"X coordinate projection method", &istat) ||
fits_write_key_fixdbl(fp, k="TCRPX2", aste->sky.ycen, 1,
	"Y reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="TCRVL2", com.skyref.delta, 5,
	"Y reference pixel value (deg)", &istat) ||
fits_write_key_fixdbl(fp, k="TCDLT2", tcdlty, 7,
	"Y pixel scale (deg/pixel)", &istat) ||
fits_write_key_fixdbl(fp, k="TCROT2", com.skyref.roll, 5,
	"Y pixel rotation (deg)", &istat) ||
fits_write_key_str(fp, k="TCTYP2", "DEC--TAN",
	"Y coordinate projection method", &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
      goto error;
    }
  }

  irow = 1L;
  for (iy = 1; iy <= ny; iy++) {
    for (ix = 1; ix <= nx; ix++) {
      fits_write_col_int(fp, 1, irow, 1, 1, &ix, &istat);
      fits_write_col_int(fp, 2, irow, 1, 1, &iy, &istat);
      irow++;
    }
  }
  if ( istat ) {
    anl_msg_error("\
%s: fits_write_col() failed (%d)\n", pname, istat);
    goto error;
  }

  fits_flush_file(fp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_flush_file() failed (%d)\n", pname, istat);
    goto error;
  }

  if ( NULL == regexpr ) {
    regstat = malloc( nrows + strlen(regfile) + sizeof(exptmpl));
    if ( NULL != regstat ) {
      regexpr = &regstat[nrows];
      sprintf(regexpr, exptmpl, regfile);
    }
  } else {
    regstat = malloc( nrows );
  }

  if ( NULL == regstat ) {
    anl_msg_error("\
%s: regstat[%ld] malloc() failed\n", pname, nrows);
    goto error;
  }

  fits_find_rows(fp, regexpr, 1, nrows, &n_good_rows, regstat, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_find_rows('%s') failed (%d)\n", pname, regexpr, istat);
    goto error;
  }
  anl_msg_info("\
   regexpr=%s, n_good_rows=%ld\n", regexpr, n_good_rows);

  for (ipos = 0; ipos < nrows; ipos++) {
    regmap[ipos] = regstat[ipos] ? 1.0 : 0.0;
  }

  free(regstat);
  fits_close_file(fp, &istat);

  return regmap;

 error:
  if ( NULL != regstat ) {
    free(regstat);
  }

  if ( NULL != fp ) {
    istat = 0;
    fits_close_file(fp, &istat);
  }

  if ( NULL != regmap ) {
    free(regmap);
  }

  return NULL;
}

static float *
read_skyfits(TELDEF *teldef, char *regfile, int i, FITS_IMAGE_KEY *kp)
{
  float *image, *image2;
  int nx, ny, nx2, ny2;
  int ix, iy, ipos, ix2, iy2, ipos2;

  nx = teldef->mission.aste->sky.xsiz;
  ny = teldef->mission.aste->sky.ysiz;

  image = read_fits_image(regfile, 0, &nx2, &ny2, kp);
  if ( NULL == image ) {
    return NULL;
  }

  if ( 0 == strcmp(kp->hduclas1, "IMAGE") &&
      0 == strcmp(kp->hduclas2, "WMAP") ) {
    if ( 0 != strcmp(kp->mtype1, "SKY") ||
	0 != strcmp(kp->mform1, "X,Y") ) {
      anl_msg_error("\
%s: source_image is WMAP, but not in SKY coordinates\n", pname);
      free(image);
      return NULL;
    }
    image2 = image;
    image = malloc( nx * ny * sizeof(*image) );
    if ( NULL == image ) {
      anl_msg_error("\
%s: image[%dx%d] malloc() failed\n", pname, nx, ny);
      free(image2);
      return NULL;
    }
    anl_msg_info("\
   converting WMAP [%dx%d*%d+%d+%d] into SKY image\n",
		 nx2, ny2, kp->wmrebin, kp->x_offset, kp->y_offset);
    for (ipos = 0, iy = 1; iy <= ny; iy++) {
      iy2 = ((iy - 1) / kp->wmrebin + 1) - kp->y_offset;
      for (ix = 1; ix <= nx; ix++, ipos++) {
	ix2 = ((ix - 1) / kp->wmrebin + 1) - kp->x_offset;
	if ( 0 <= iy2 && iy2 < ny2 && 0 <= ix2 && ix2 < nx2 ) {
	  ipos2 = iy2 * nx2 + ix2;
	  image[ipos] = (image2[ipos2] < 0.0) ? 0.0 : 1.0;
	} else {
	  image[ipos] = 0.0;
	}
      }
    }
    free(image2);

  } else if ( nx != nx2 || ny != ny2 ) {
    anl_msg_error("\
%s: regfile%d size mismatch (nx=%d, ny=%d)\n", pname, i, nx2, ny2);
    free(image);
    return NULL;
  } else if ( SKYREF_MODE_USER == com.pointing ) {
    if ( com.skyref.alpha != com.region_kp[i].crval1 ||
	 com.skyref.delta != com.region_kp[i].crval2 ||
	 com.skyref.roll  != com.region_kp[i].crota2 ) {
      anl_msg_warning("\
%s: WARNING: SKYREF is different from the user specification\n", pname);
    }
  }

  if ( *kp->telescop && 0 != strcmp(kp->telescop, teldef->telescop) ) {
    anl_msg_warning("\
%s: WARNING: TELESCOP keyword mismatch\n", pname);
  }
  if ( *kp->instrume && 0 != strcmp(kp->instrume, teldef->instrume) ) {
    anl_msg_warning("\
%s: WARNING: INSTRUME keyword mismatch\n", pname);
  }

  return image;
}

static int
make_regmap(void)
{
  int i, ix, iy, ipos, nx, ny, npos;
  double x2, y2, r2, r2min, r2max, region_x, region_y;
  float *image, *rp, *regmap;
  double *wmap;
  FITS_IMAGE_KEY *kp;

  anl_msg_info("\
%s: making region maps\n", pname);
  image = NULL;			/* initialize for gcc warning */

  switch ( com.region_mode ) {
 case REGION_MODE_SKYFITS:
 case REGION_MODE_SKYCIRC:
 case REGION_MODE_SKYREG:
 case REGION_MODE_SKYEXPR:
    nx = com.region_nx = com.teldef->mission.aste->sky.xsiz;
    ny = com.region_ny = com.teldef->mission.aste->sky.ysiz;
    npos = nx * ny;
    break;

 default:
    anl_msg_error("\
%s: unknonw region_mode in make_regmap()\n", pname);
    return -1;
  }

  for (i = 0; i < com.num_region; i++) {
    kp = &com.region_kp[i];

    switch ( com.region_mode ) {
  case REGION_MODE_SKYFITS:
      anl_msg_info("\
   reading regfile%d '%s'\n", i+1, com.regfile[i]);
      image = read_skyfits(com.teldef, com.regfile[i], i, kp);
      if ( NULL == image ) return -1;
      break;

  case REGION_MODE_SKYCIRC:
      region_x = com.region_x[i];
      region_y = com.region_y[i];
      r2min = com.region_rmin[i] * com.region_rmin[i];
      r2max = com.region_rmax[i] * com.region_rmax[i];
      image = malloc( npos * sizeof(*image) );
      if ( NULL == image ) {
	anl_msg_error("\
%s: com.regmap[%d][%dx%d] malloc() failed\n", pname, i, nx, ny);
	return -1;
      }
      for (iy = 0; iy < ny; iy++) {
	rp = &image[iy*nx];
	y2 = (iy+1) - region_y;
	y2 *= y2;
	for (ix = 0; ix < nx; ix++) {
	  x2 = (ix+1) - region_x;
	  x2 *= x2;
	  r2 = x2 + y2;
	  rp[ix] = ( r2min <= r2 && r2 <= r2max ) ? 1.0 : 0.0;
	}
      }
      break;

  case REGION_MODE_SKYREG:
      image = make_ds9reg_image(com.regfile[i], NULL, nx, ny);
      if ( NULL == image ) {
	return -1;
      }
      break;

  case REGION_MODE_SKYEXPR:
      image = make_ds9reg_image(NULL, com.regfile[i], nx, ny);
      if ( NULL == image ) {
	return -1;
      }
      break;

  default:
      ;
    }

    com.regmap[i] = image;
  }

  if ( REGION_MODE_SKYFITS == com.region_mode &&
       SKYREF_MODE_AUTO == com.pointing ) {
    for (i = 0; i < com.num_region; i++) {
      if ( com.skyref.alpha != com.region_kp[i].crval1 ||
	   com.skyref.delta != com.region_kp[i].crval2 ||
	   com.skyref.roll  != com.region_kp[i].crota2 ) {
	anl_msg_error("\
%s: SKYREF different for regfile%d\n", pname, i+1);
	return -1;
      }
    }
  }

/* initialize wmap, sum_source_reg, sum_detmask_reg, tot_ccd_pix_reg */
  for (i = 0; i < com.num_region; i++) {
    wmap = com.wmap[i] = malloc( npos * sizeof(*wmap) );
    if ( NULL == wmap ) {
      anl_msg_error("\
%s: com.wmap[%d][%dx%d] malloc() failed\n", pname, i, nx, ny);
      return -1;
    }
    regmap = com.regmap[i];
    for (ipos = 0; ipos < npos; ipos++) {
      wmap[ipos] = ( 0.0 < regmap[ipos] ) ? 0.0 : -1.0;
    }

    com.sum_source_reg[i]  = 0.0;
    com.sum_detmask_reg[i] = 0.0;
    com.tot_ccd_pix_reg[i] = 0.0;
  }

  return 0;
}

static int
eval_pixq(double obstime)
{
  char *k;
  int istat, ipos, nxny, actx, acty, detx, dety;
  fitsfile *ifp;
  float *image;
  unsigned char *actexpo;

  if ( 0 == com.enable_pixq ) {
    return 0;
  }

  anl_msg_info("\
%s: evaluating pixel quality selection\n", pname);

/* initialize pixel quality function */
  pixq.hotpix_filenames = com.hotpixfiles;
  pixq.badcol_filename  = com.badcolumfile;
  pixq.calmask_filename = com.calmaskfile;
  if ( 0 == CLstricmp("none", com.phafile) ) {
    istat = xisSciReadKeys(NULL, &pixq.sci);
    if ( istat ) goto quit;
  } else {
    istat = 0;
    fits_open_file(&ifp, k=com.phafile, READONLY, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, k, istat);
      goto quit;
    }
    istat = xisSciReadKeys(ifp, &pixq.sci);
    if ( istat ) {
      int istat2 = 0;
      fits_close_file(ifp, &istat2); /* ignore error */
      goto quit;
    }
    fits_close_file(ifp, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, k, istat);
      goto quit;
    }
  }
  istat = xisPixelQualityInit(com.instrume, &pixq);
  if ( istat ) goto quit;

/* initialize pixel quality statistics */
  xisPixqStatInit(&statistics);
  statistics.flag_sel = 1;
  statistics.pixq_min = com.pixq_min;
  statistics.pixq_max = com.pixq_max;
  statistics.pixq_and = com.pixq_and;
  statistics.pixq_eql = com.pixq_eql;

/* generate exposure map in ACT coordinates */
  istat = xisPixqExpMapGenACT(&pixq, &statistics, obstime);
  if ( istat ) goto quit;

/* free memory for badcol_cache & calmask */
  if ( NULL != pixq.badcol_cache ) {
    free(pixq.badcol_cache);
    pixq.badcol_cache = NULL;
  }
  if ( NULL != pixq.calmask ) {
    free(pixq.calmask);
    pixq.calmask = NULL;
  }

  anl_msg_info("\
%s: merging with detmask\n", pname);

  nxny = com.ndetx * com.ndety;
  image = com.detmaskmap;
  if ( NULL == image ) {
    image = malloc(sizeof(*image) * nxny);
    if ( NULL == image ) {
      anl_msg_error("\
%s: image[%dx%d] malloc() failed\n", pname, com.ndetx, com.ndety);
      istat = -1;
      goto quit;
    }
    for (ipos = 0; ipos < nxny; ipos++) {
      image[ipos] = 1.0;
    }
  }

  actexpo = pixq.actexpo;
  for (acty = 0; acty < com.ndety; acty++) {
    for (actx = 0; actx < com.ndetx; actx++) {
      xis_act2det(com.teldef, actx, acty, &detx, &dety);
      ipos = (dety - 1) * com.ndetx + (detx - 1);
      image[ipos] *= (*actexpo) * pixq.bscale;
      actexpo++;
    }
  }

/* free memory for actexpo */
  free(pixq.actexpo);
  pixq.actexpo = NULL;

  com.tot_ccd_pix = nxny;
  com.sum_detmask = 0.0;
  for (ipos = 0; ipos < nxny; ipos++) {
    com.sum_detmask += image[ipos];
  }
  com.detmaskmap = image;

  anl_msg_info("\
   mask_ratio_ccd = %.1f / %.0f = %.6f\n",
	com.sum_detmask, com.tot_ccd_pix, com.sum_detmask/com.tot_ccd_pix);

  return 0;

 quit:
  return istat;
}

void
xisarfgen_startup(int *status)
{
  com.leapfile = com.o_leapfile;
  com.teldeffile = com.o_teldeffile;
  com.shieldfile = com.o_shieldfile;
  com.effareafile = com.o_effareafile;
  com.psffile = com.o_psffile;
  com.rmffile = com.o_rmffile;
  com.contamifile = com.o_contamifile;
  com.badcolumfile = com.o_badcolumfile;
  com.calmaskfile  = com.o_calmaskfile;
#ifdef TRANSMISSION_INDEX_SEARCH
  com.trans.index.norm = 0.0;
  com.trans.index.offs = 0.0;
  com.trans.index.nbody = 10000;
  com.trans.index.body = NULL;
#endif

  *status = ANL_OK;
}

static void
show_parameter(void)
{
  int i;
  char buf[80];
  AtRightAscension ra;
  AtDeclination dec;

#define MSG	anl_msg_always

  MSG("\n");
  MSG("%s: *** show parameter ***\n", pname);
  MSG("\n");
  MSG("%4s%-20s%s\n", "", "PHAFILE", com.phafile);
  MSG("%4s%-20s%s%s\n", "", "LEAPFILE", com.leapfile,
      (com.leapfile == com.o_leapfile) ? "" : " (CALDB)");
  MSG("%4s%-20s%s%s\n", "", "TELDEF", com.teldeffile,
      (com.teldeffile == com.o_teldeffile) ? "" : " (CALDB)");
  MSG("%4s%-20s%s%s\n", "", "SHIELDFILE", com.shieldfile,
      (com.shieldfile == com.o_shieldfile) ? "" : " (CALDB)");
  MSG("%4s%-20s%s%s\n", "", "EFFAREAFILE", com.effareafile,
      (com.effareafile == com.o_effareafile) ? "" : " (CALDB)");
  MSG("%4s%-20s%s%s\n", "", "PSFFILE", com.psffile,
      (com.psffile == com.o_psffile) ? "" : " (CALDB)");

  switch ( com.pointing ) {
 case SKYREF_MODE_AUTO:
    MSG("\
%4s%-20s%s\n", "", "POINTING", "AUTO");
    break;
 case SKYREF_MODE_USER:
    MSG("\
%4s%-20s%s\n", "", "POINTING", "USER");
    break;
 default:
    MSG("\
%4s%-20s%s\n", "", "POINTING", "UNKNOWN");
  }

  atDegToRA(com.skyref.alpha, &ra);
  sprintf(buf, "%.4f (%02dh%02dm%06.3fs)", com.skyref.alpha,
	  ra.hour, ra.min, ra.sec);
  MSG("\
  %4s%-20s%s\n", "", "REF_ALPHA (J2000)", buf);
  atDegToDec(com.skyref.delta, &dec);
  sprintf(buf, "%.4f (%s%02dd%02dm%05.2fs)", com.skyref.delta,
	  (dec.sign < 0) ? "-" : "+", dec.deg, dec.min, dec.sec);
  MSG("\
  %4s%-20s%s\n", "", "REF_DELTA (J2000)", buf);
  MSG("\
  %4s%-20s%.4f\n", "", "REF_ROLL (deg)", com.skyref.roll);

  MSG("\
%4s%-20s%s\n", "", "ABERRATION", com.aberration ? "YES" : "NO");

  switch ( com.source_mode ) {
 case SOURCE_MODE_J2000:
    MSG("\
%4s%-20s%s\n", "", "SOURCE_MODE", "J2000");
    MSG("\
  %4s%-20s( %9.4f , %9.4f ) (deg)\n", "", "SOURCE_RA,DEC",
	com.source_ra, com.source_dec);
    break;
 case SOURCE_MODE_SKYXY:
    MSG("\
%4s%-20s%s\n", "", "SOURCE_MODE", "SKYXY");
    MSG("\
  %4s%-20s( %6.1f , %6.1f )\n", "", "SOURCE_X,Y",
	com.source_x, com.source_y);
    break;
 default:
    MSG("\
%4s%-20s%s\n", "", "SOURCE_MODE", "UNKNOWN");
  }

  switch ( com.region_mode ) {
 case REGION_MODE_SKYFITS:
    MSG("\
%4s%-20s%s\n", "", "REGION_MODE", "SKYFITS");
    for (i = 0; i < com.num_region; i++) {
      MSG("\
%4s%3d: REGFILE %s\n", "", i+1, com.regfile[i]);
      MSG("\
%4s     ARFFILE %s\n", "", com.arffile[i]);
    }
    break;
 case REGION_MODE_SKYCIRC:
    MSG("\
%4s%-20s%s\n", "", "REGION_MODE", "SKYCIRC");
    for (i = 0; i < com.num_region; i++) {
      MSG("\
%4s%3d%-19s( %6.1f , %6.1f ) %6.1f - %6.1f\n",
	  "", i+1, ": X,Y,RMIN,RMAX",
	  com.region_x[i], com.region_y[i],
	  com.region_rmin[i], com.region_rmax[i]);
      MSG("\
%4s     ARFFILE %s\n", "", com.arffile[i]);
    }
    break;
 case REGION_MODE_SKYREG:
    MSG("\
%4s%-20s%s\n", "", "REGION_MODE", "SKYREG");
    for (i = 0; i < com.num_region; i++) {
      MSG("\
%4s%3d: REGFILE %s\n", "", i+1, com.regfile[i]);
      MSG("\
%4s     ARFFILE %s\n", "", com.arffile[i]);
    }
    break;
 case REGION_MODE_SKYEXPR:
    MSG("\
%4s%-20s%s\n", "", "REGION_MODE", "SKYEXPR");
    for (i = 0; i < com.num_region; i++) {
      MSG("\
%4s%3d: REGFILE %s\n", "", i+1, com.regfile[i]);
      MSG("\
%4s     ARFFILE %s\n", "", com.arffile[i]);
    }
    break;
 default:
    MSG("\
%4s%-20s%s\n", "", "REGION_MODE", "UNKNOWN");
  }

  MSG("\
%4s%-20s%s\n", "", "DETMASK", com.detmask);
  MSG("\
%4s%-20s%s\n", "", "GTIFILE", com.gtifile);
  if ( 0 == CLstricmp("NONE", com.gtifile) ||
       0 == CLstricmp("USER", com.gtifile) ||
       0 ) {
    MSG("\
  %4s%-20s%s\n", "", "DATE_OBS", com.date_obs);
  }
  MSG("\
%4s%-20s%s\n", "", "ATTITUDE", com.attitude);
  if ( 0 == CLstricmp("NONE", com.attitude) ||
       0 == CLstricmp("USER", com.attitude) ||
       0 ) {
    MSG("\
  %4s%-20s%.4f %.4f %.4f\n", "", "EA1/2/3",
	com.ea.phi*RAD2DEG, com.ea.theta*RAD2DEG, com.ea.psi*RAD2DEG);
  }
  MSG("\
%4s%-20s%s%s\n", "", "RMFFILE", com.rmffile,
      (com.rmffile == com.o_rmffile) ? "" : " (CALDB)");
  MSG("\
%4s%-20s%s%s\n", "", "CONTAMIFILE", com.contamifile,
      (com.contamifile == com.o_contamifile) ? "" : " (CALDB)");
  MSG("\
%4s%-20s%s\n", "", "ENABLE_PIXQ", com.enable_pixq ? "YES" : "NO");
  if ( com.enable_pixq ) {
    MSG("\
  %4s%-20s%s\n", "", "HOTPIXFILES", com.hotpixfiles);
    MSG("\
  %4s%-20s%s%s\n", "", "BADCOLUMFILE", com.badcolumfile,
	(com.badcolumfile == com.o_badcolumfile) ? "" : " (CALDB)");
    MSG("\
  %4s%-20s%s%s\n", "", "CALMASKFILE", com.calmaskfile,
	(com.calmaskfile == com.o_calmaskfile) ? "" : " (CALDB)");
    MSG("\
  %4s%-20s%u (0x%08x)\n", "", "PIXQ_MIN", com.pixq_min, com.pixq_min);
    MSG("\
  %4s%-20s%u (0x%08x)\n", "", "PIXQ_MAX", com.pixq_max, com.pixq_max);
    MSG("\
  %4s%-20s%u (0x%08x)\n", "", "PIXQ_AND", com.pixq_and, com.pixq_and);
    MSG("\
  %4s%-20s%u (0x%08x)\n", "", "PIXQ_EQL", com.pixq_eql, com.pixq_eql);
  }
  MSG("\
%4s%-20s%.1f\n", "", "GTI_MIN_SEC", com.gti_min_sec);
  MSG("\
%4s%-20s%s\n", "", "CLOBBER", com.clobber ? "YES" : "NO");
  MSG("\n");
}

void
xisarfgen_com(int *status)
{
  int i;
  char *k, buf[PIL_LINESIZE];

  unsigned int uint_min = 0;
  unsigned int uint_max = 4294967295u;
  double pixq_min = 0.0;
  double pixq_max = 524287.0;
  double pixq_and = 0.0;
  double pixq_eql = 0.0;

  if ( *status ) {		/* ftools */
    if (
PILGetFname(k="phafile", com.phafile) ||
PILGetFname (k="teldef", com.teldeffile) ||
PILGetFname (k="leapfile", com.leapfile) ||
PILGetFname (k="shieldfile", com.shieldfile) ||
PILGetFname (k="effareafile", com.effareafile) ||
PILGetFname (k="psffile", com.psffile) ||
PILGetBool  (k="aberration", &com.aberration) ||
PILGetReal  (k="gti_min_sec", &com.gti_min_sec) ||
PILGetBool  (k="clobber", &com.clobber) ||
	 0 ) {
      goto pil_error;
    }

    if ( PILGetString(k="pointing", buf) ) goto pil_error;
    if ( 0 == CLstricmp("AUTO", buf) ) {
      com.pointing = SKYREF_MODE_AUTO;
    } else if ( 0 == CLstricmp("USER", buf) ) {
      com.pointing = SKYREF_MODE_USER;
      if ( PILGetReal(k="ref_alpha", &com.skyref.alpha) ||
	   PILGetReal(k="ref_delta", &com.skyref.delta) ||
	   PILGetReal(k="ref_roll",  &com.skyref.roll) ) {
	goto pil_error;
      }
    }

    if ( PILGetString(k="source_mode", buf) ) goto pil_error;
    if ( 0 == CLstricmp("J2000", buf) ) {
      com.source_mode = SOURCE_MODE_J2000;
      if ( PILGetReal(k="source_ra", &com.source_ra) ||
	   PILGetReal(k="source_dec", &com.source_dec) ) {
	goto pil_error;
      }
    } else if ( 0 == CLstricmp("SKYXY", buf) ) {
      com.source_mode = SOURCE_MODE_SKYXY;
      if ( PILGetReal(k="source_x", &com.source_x) ||
	   PILGetReal(k="source_y", &com.source_y) ) {
	goto pil_error;
      }
    } else {
      anl_msg_error("\
%s: unknown source_mode=%s\n", pname, buf);
      goto pil_error;
    }

    if ( PILGetInt(k="num_region", &com.num_region) ) goto pil_error;
    if ( MAX_REGION < com.num_region ) {
      anl_msg_error("\
%s: too many num_region=%d\n", pname, com.num_region);
      goto pil_error;
    }

    if ( PILGetString(k="region_mode", buf) ) goto pil_error;
    if ( 0 == CLstricmp("SKYFITS", buf) ) {
      com.region_mode = REGION_MODE_SKYFITS;
      for (i = 0; i < com.num_region; i++) {
	sprintf(buf, "regfile%d", i+1);
	if ( PILGetFname(k=buf, com.regfile[i]) ) goto pil_error;
	sprintf(buf, "arffile%d", i+1);
	if ( PILGetFname(k=buf, com.arffile[i]) ) goto pil_error;
      }
    } else if ( 0 == CLstricmp("SKYCIRC", buf) ) {
      com.region_mode = REGION_MODE_SKYCIRC;
      for (i = 0; i < com.num_region; i++) {
	sprintf(buf, "region_x%d", i+1);
	if ( PILGetReal(k=buf, &com.region_x[i]) ) goto pil_error;
	sprintf(buf, "region_y%d", i+1);
	if ( PILGetReal(k=buf, &com.region_y[i]) ) goto pil_error;
	sprintf(buf, "region_rmin%d", i+1);
	if ( PILGetReal(k=buf, &com.region_rmin[i]) ) goto pil_error;
	sprintf(buf, "region_rmax%d", i+1);
	if ( PILGetReal(k=buf, &com.region_rmax[i]) ) goto pil_error;
	sprintf(buf, "arffile%d", i+1);
	if ( PILGetFname(k=buf, com.arffile[i]) ) goto pil_error;
      }
    } else if ( 0 == CLstricmp("SKYREG", buf) ) {
      com.region_mode = REGION_MODE_SKYREG;
      for (i = 0; i < com.num_region; i++) {
	sprintf(buf, "regfile%d", i+1);
	if ( PILGetFname(k=buf, com.regfile[i]) ) goto pil_error;
	sprintf(buf, "arffile%d", i+1);
	if ( PILGetFname(k=buf, com.arffile[i]) ) goto pil_error;
      }
    } else if ( 0 == CLstricmp("SKYEXPR", buf) ) {
      com.region_mode = REGION_MODE_SKYEXPR;
      for (i = 0; i < com.num_region; i++) {
	sprintf(buf, "regfile%d", i+1);
	if ( PILGetFname(k=buf, com.regfile[i]) ) goto pil_error;
	sprintf(buf, "arffile%d", i+1);
	if ( PILGetFname(k=buf, com.arffile[i]) ) goto pil_error;
      }
    } else {
      anl_msg_error("\
%s: unknown region_mode=%s\n", pname, buf);
      goto pil_error;
    }

    if ( PILGetFname(k="detmask", com.detmask) ) goto pil_error;
    if ( PILGetFname(k="gtifile", com.gtifile) ) goto pil_error;
    if ( 0 == CLstricmp("NONE", com.gtifile) ||
	 0 == CLstricmp("USER", com.gtifile) ||
	 0 ) {
      if ( PILGetString(k="date_obs", com.date_obs) ) goto pil_error;
    }
    if ( PILGetFname(k="attitude", com.attitude) ) goto pil_error;
    if ( 0 == CLstricmp("NONE", com.attitude) ||
	 0 == CLstricmp("USER", com.attitude) ||
	 0 ) {
      if ( PILGetReal(k="ea1", &com.ea_deg.phi) ||
	   PILGetReal(k="ea2", &com.ea_deg.theta) ||
	   PILGetReal(k="ea3", &com.ea_deg.psi) ) {
	goto pil_error;
      }
      com.ea.phi = com.ea_deg.phi * DEG2RAD;
      com.ea.theta = com.ea_deg.theta * DEG2RAD;
      com.ea.psi = com.ea_deg.psi * DEG2RAD;
    }
    if ( PILGetFname(k="rmffile", com.rmffile) ) goto pil_error;
    if ( PILGetFname(k="contamifile", com.contamifile) ) goto pil_error;

    if ( PILGetBool(k="enable_pixq", &com.enable_pixq) ) goto pil_error;
    if ( com.enable_pixq ) {
      if ( PILGetFname(k="hotpixfiles", com.hotpixfiles) ||
	  PILGetFname(k="badcolumfile", com.badcolumfile) ||
	  PILGetFname(k="calmaskfile", com.calmaskfile) ||
	  PILGetReal (k="pixq_min", &pixq_min) ||
	  PILGetReal (k="pixq_max", &pixq_max) ||
	  PILGetReal (k="pixq_and", &pixq_and) ||
	  PILGetReal (k="pixq_eql", &pixq_eql) ||
	  0 ) {
	goto pil_error;
      }
      if ( pixq_min < uint_min || uint_max < pixq_min ||
	  pixq_max < uint_min || uint_max < pixq_max ||
	  pixq_and < uint_min || uint_max < pixq_and ||
	  pixq_eql < uint_min || uint_max < pixq_eql ||
	  0 ) {
	anl_msg_error("\
%s: 'pixq_***' parameter out of range\n", pname);
	*status = ANL_QUIT;
	return;
      }

      com.pixq_min = (unsigned int)pixq_min;
      com.pixq_max = (unsigned int)pixq_max;
      com.pixq_and = (unsigned int)pixq_and;
      com.pixq_eql = (unsigned int)pixq_eql;
    }

    *status = ANL_OK;;
    return;

 pil_error:
    anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
    *status = ANL_QUIT;
    return;
  }

  *status = ANL_OK;
}

static int
find_key(fitsfile *fp, char *keyname)
{
  int i;
  char card[FLEN_CARD];

  int istat = 0;

  fits_read_card(fp, keyname, card, &istat);
  if ( istat ) return istat;

  card[FLEN_CARD-1] = '\0';
  for (i = FLEN_CARD - 1; 0 <= i && ' ' == card[i]; i--) {
    card[i] = '\0';
  }
  anl_msg_info("%s\n", card);

  return 0;
}

void
xisarfgen_init(int *status)
{
  int sensor, xrt_id, ireg, nalloc_bytes, verbose;
  char *xrt_inst;

  int det_xsiz, det_ysiz, sky_xsiz, sky_ysiz;
  double tstart, tstop, obstime, ra_nom, dec_nom;
  char *k;
  TELDEF_ASTROE *aste;
  AtTimeD attime;

  char *telescop = com.telescop;
  char *instrume = com.instrume;
  SORT_DATA *obs = &com.obs;

  fitsfile *ifp = NULL;
  fitsfile *gti_fp = NULL;
  ATTFILE *attfile = NULL;
  int istat = 0;

/* open input pha file to read keywords for CALDB */
  anl_msg_info("\
%s: opening phafile '%s' ...\n", pname, com.phafile);
  fits_open_file(&ifp, k=com.phafile, READONLY, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  if (
find_key(ifp, k="TELESCOP") ||
fits_read_key_str(ifp, k, telescop, NULL, &istat) ||
find_key(ifp, k="INSTRUME") ||
fits_read_key_str(ifp, k, instrume, NULL, &istat) ||
find_key(ifp, k="TSTART") ||
fits_read_key_dbl(ifp, k, &tstart, NULL, &istat) ||
find_key(ifp, k="TSTOP") ||
fits_read_key_dbl(ifp, k, &tstop, NULL, &istat) ||
find_key(ifp, k="RA_NOM") ||
fits_read_key_dbl(ifp, k, &ra_nom, NULL, &istat) ||
find_key(ifp, k="DEC_NOM") ||
fits_read_key_dbl(ifp, k, &dec_nom, NULL, &istat) ||
find_key(ifp, k="WIN_ST") ||
fits_read_key(ifp, TINT, k, &obs->win_st, NULL, &istat) ||
find_key(ifp, k="WIN_SIZ") ||
fits_read_key(ifp, TINT, k, &obs->win_siz, NULL, &istat) ||
find_key(ifp, k="CI") ||
fits_read_key(ifp, TINT, k, &obs->ci, NULL, &istat) ||
fits_read_key(ifp, TINT, k="MJDREFI", &obs->mjdrefi, NULL, &istat) ||
fits_read_key_dbl(ifp, k="MJDREFF", &obs->mjdreff, NULL, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }

  obstime = (tstart + tstop) / 2;
  obs->skyref.alpha = ra_nom;
  obs->skyref.delta = dec_nom;
  obs->skyref.roll = 0.0;
  if ( SKYREF_MODE_AUTO == com.pointing ) {
    com.skyref = obs->skyref;
  }

/* find CALDB files */
  com.teldeffile = aste_caldb_find(instrume, "TELDEF", com.o_teldeffile);
  if ( NULL == com.teldeffile ) goto quit;
  com.leapfile = aste_caldb_find_leapfile(com.o_leapfile);
  if ( NULL == com.leapfile ) goto quit;

  sensor = aste_instrume_id(instrume);
  if ( ASTE_XRS_ID == sensor ) {
    xrt_id = ASTE_XRTS_ID;
  } else if ( ASTE_XIS0_ID <= sensor && sensor <= ASTE_XIS3_ID ) {
    xrt_id = ASTE_XRT0_ID + sensor - ASTE_XIS0_ID;
  } else if ( ASTE_XRT0_ID <= sensor && sensor <= ASTE_XRTS_ID ) {
    xrt_id = sensor;
  } else {
    anl_msg_error("\
%s: invalid instrume='%s'\n", pname, com.instrume);
    goto quit;
  }

  xrt_inst = aste_instrume(xrt_id);
  com.shieldfile = aste_caldb_find("XRT", "FTRANS", com.o_shieldfile);
  if ( NULL == com.shieldfile ) goto quit;
  com.effareafile = aste_caldb_find(xrt_inst, "EFFAREA", com.o_effareafile);
  if ( NULL == com.effareafile ) goto quit;
  com.psffile = aste_caldb_find(xrt_inst, "IMAGE_PSF_TABLE", com.o_psffile);
  if ( NULL == com.psffile ) goto quit;

#define get_caldb_file(CODE,O_FILE)	aste_caldb_find(instrume, CODE, O_FILE)
  com.contamifile = get_caldb_file("CONTAMI_TRANS", com.o_contamifile);
  if ( NULL == com.contamifile ) goto quit;
  com.rmffile = get_caldb_file("SPECRESP MATRIX", com.o_rmffile);
  if ( NULL == com.rmffile ) goto quit;
  if ( com.enable_pixq ) {
    com.badcolumfile = get_caldb_file("BADPIX", com.o_badcolumfile);
    if ( NULL == com.badcolumfile ) goto quit;
    com.calmaskfile = get_caldb_file("CALMASK", com.o_calmaskfile);
    if ( NULL == com.calmaskfile ) goto quit;
  }
#undef get_caldb_file

/* show current parameters */
  show_parameter();

/* initialize aste_time */
  verbose = -1; /*( 0 == CLstricmp("none", com.leapfile) ) ? -1 : -2;*/
  if ( NULL == atMissionTimeInit(com.leapfile, verbose) ) {
    anl_msg_error("\
%s: atMissionTimeInit('%s') failed\n", pname, com.leapfile);
    goto quit;
  }
  if ( -1 == verbose ) printf("\n");

/* initialize aste_coord */
  com.teldef = aste_coord_init(NULL, instrume, com.teldeffile);
  if ( NULL == com.teldef ) {
    anl_msg_error("\
%s: aste_coord_init('%s') failed\n", pname, com.teldeffile);
    goto quit;
  }
  aste = com.teldef->mission.aste;
  det_xsiz = aste->det.xsiz;
  det_ysiz = aste->det.ysiz;
  sky_xsiz = aste->sky.xsiz;
  sky_ysiz = aste->sky.ysiz;

/* open attitude file */
  if ( 0 == CLstricmp("NONE", com.attitude) ||
       0 == CLstricmp("USER", com.attitude) ||
       0 ) {
    attfile = NULL;
  } else {
    anl_msg_info("\
%s: opening attitude '%s' ...\n", pname, com.attitude);
    attfile = aste_att_init(com.attitude);
    if ( NULL == attfile ) {
      anl_msg_error("\
%s: could not find attitude '%s'\n", pname, com.attitude);
      goto quit;
    }
  }

/* open gti file */
  if ( 0 == CLstricmp("AUTO", com.gtifile) ) {
    fits_reopen_file(ifp, &gti_fp, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_reopen_file() failed (%d)\n", pname, istat);
      goto quit;
    }
    anl_msg_info("\
%s: reading GTI of phafile '%s' ...\n", pname, com.phafile);
  } else if ( 0 == CLstricmp("NONE", com.gtifile) ||
	      0 == CLstricmp("USER", com.gtifile) ||
	      0 ) {
    istat = aefits_datestr2attimeD(com.date_obs, &attime);
    if ( istat ) {
      anl_msg_error("\
%s: invalid format of date_obs='%s'\n", pname, com.date_obs);
      goto quit;
    }
    com.photon_time = attimeD2aste(&attime);
    anl_msg_info("\
%s: using fixed time of date_obs=%s (t=%.1f)\n",
	pname, com.date_obs, com.photon_time);
    gti_fp = NULL;
  } else {
    fits_open_file(&gti_fp, k=com.gtifile, READONLY, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, k, istat);
      goto quit;
    }
    anl_msg_info("\
%s: reading gtifile '%s' ...\n", pname, com.gtifile);
  }
  fits_close_file(ifp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, com.phafile, istat);
    goto quit;
  }

/* calculate eulinfo for sky region */
  if ( 0 != calc_eulinfo(com.teldef, attfile, gti_fp, com.photon_time, obs) ) {
    goto quit;
  }

  if ( 0 != read_detmask(com.detmask) ) {
    goto quit;
  }

  if ( 0 != read_energ_lo_hi(com.rmffile) ) {
    goto quit;
  }
  nalloc_bytes = 4 * com.num_region * com.num_energ * sizeof(double);
  com.specresp[0]= malloc(nalloc_bytes);
  if ( NULL == com.specresp[0] ) {
    anl_msg_error("\
%s: com.specresp[] malloc() failed\n", pname);
    goto quit;
  }
  memset(com.specresp[0], 0, nalloc_bytes);
  for (ireg = 0; ireg < com.num_region; ireg++) {
    com.specresp[ireg] = &com.specresp[0][4*ireg*com.num_energ];
    com.xrt_effarea[ireg] = &com.specresp[ireg][com.num_energ];
    com.shield_trans[ireg] = &com.xrt_effarea[ireg][com.num_energ];
    com.contami_trans[ireg] = &com.shield_trans[ireg][com.num_energ];
  }

  if ( 0 != read_contamifile(com.contamifile) ) {
    goto quit;
  }

  if ( 0 != eval_pixq(obstime) ) {
    goto quit;
  }

  if ( 0 != make_regmap() ) {
    goto quit;
  }

  anl_msg_info("\
%s: reading shield file ...\n", pname);
  if ( 0 != read_shield_file(com.shieldfile) ) {
    goto quit;
  }

  anl_msg_info("\
%s: reading effarea file ...\n", pname);
  if ( 0 != xis_effarea_init(com.effareafile) ) {
    goto quit;
  }
  fits_open_file(&ifp, k=com.effareafile, READONLY, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  fits_movnam_hdu(ifp, BINARY_TBL, "EFFAREA", 0, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: no EFFAREA extension is found (%d)\n", pname, istat);
    goto quit;
  }
  if (
find_key(ifp, k="GEOMAREA") ||
fits_read_key_dbl(ifp, k, &com.geomarea, NULL, &istat) ||
       0 ) {
    anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
    goto quit;
  }
  fits_close_file(ifp, &istat);
  if ( istat ) {
    anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, com.effareafile, istat);
    goto quit;
  }

  anl_msg_info("\
%s: reading psf file ...\n", pname);
  if ( 0 != xis_psf_init(com.psffile) ) {
    goto quit;
  }

  anl_msg_info("\
%s: creating arf file(s) ...\n", pname);
  for (ireg = 0; ireg < com.num_region; ireg++) {
    if ( com.clobber ) unlink(com.arffile[ireg]);
    com.arffp[ireg] = create_arf_file(com.arffile[ireg]);
    if ( NULL == com.arffp[ireg] ) goto quit;
  }

  anl_msg_info("\
%s: initialization finished\n", pname);

  *status = ANL_OK;
  return;

 quit:
  *status = ANL_QUIT;
  return;
}

void
xisarfgen_his(int *status)
{
  *status = ANL_OK;
}

void
xisarfgen_bgnrun(int *status)
{
  *status = ANL_OK;
}

void
xisarfgen_ana(int *nevent, int *eventid, int *status)
{
  static double detimage[1024][1024];
  static double psf_frac[MAX_REGION];
#ifndef CONTAMI_FAST_CALC
  static double psf_contami_frac[MAX_REGION];
#endif

  int istat;
  int ieul, ie, iene, nene;
  int ipos, ireg, ix, iy, idetx, idety, ixoff, iyoff, ipsfx, ipsfy;
  double aetime, alpha, delta, detx, dety, skyx, skyy;
  /* YM */
  double detxcen, detycen;
  /* YM */
  double xrtx, xrty, offaxis, azimuth;
  double vpsf, expo_frac, e_cen, s, t, delta_ene;
  double xrt_effarea, shield_trans, contami_trans;
  double *ene, *effarea, *psf, *psfx;
  AtEulerAng *ea;

  SORT_DATA *obs = &com.obs;
  SKYREF *skyref = &com.skyref;
  EULER_INFO *eulinfo = obs->eulinfo;
  int mjdrefi = obs->mjdrefi;
  double mjdreff = obs->mjdreff;

  ene = effarea = NULL;

  for (ieul = 0; ieul < obs->neuler; ieul++) {
    aetime = eulinfo[ieul].t;
    expo_frac = (double)eulinfo[ieul].nexp / obs->nexp;
    ea = &eulinfo[ieul].ea;
    switch ( com.source_mode ) {
    case SOURCE_MODE_J2000:
      /** YM **/
      alpha = com.source_ra;
      delta = com.source_dec;
      if ( com.aberration ) {
	/** YM **/
	aste_inv_aberration(aetime, mjdrefi, mjdreff, &alpha, &delta);
	aste_ecs2det(com.teldef, ea, alpha, delta, &detx, &dety);
      } else {
	aste_ecs2det(com.teldef, ea, alpha, delta, &detx, &dety);
      }
      break;

    case SOURCE_MODE_SKYXY:
      skyx = com.source_x;
      skyy = com.source_y;
      aste_sky2ecs(com.teldef, skyref, skyx, skyy, &alpha, &delta);
      if ( com.aberration ) {
	aste_inv_aberration(aetime, mjdrefi, mjdreff, &alpha, &delta);
      }
      aste_ecs2det(com.teldef, ea, alpha, delta, &detx, &dety);
      break;

    }

    detxcen = detx;
    detycen = dety;

    aste_det2xrt(com.teldef, detx, dety, &xrtx, &xrty);
    aste_xrt_rec2pol(com.teldef, xrtx, xrty, &offaxis, &azimuth);
    /* consider XRT (look-down) -> XIS DET (look-up) */
    /* when (detx,dety)=(1,1), (ixoff,iyoff)=(1535,1537) */
    /* when (ix,iy)=(detx,dety), (ipsfx,ipsfy)=(1536,1536) */
    ixoff = (int)floor((FASTARF_PSF_NAXIS1 + 1)/2.0 - detx + 0.5) - 1;
    iyoff = (int)floor((FASTARF_PSF_NAXIS2 + 1)/2.0 + dety + 0.5) - 1;

    printf("\n\
%d/%d: t=%.1f  expo_frac=%.6f  EA=( %.4f , %.4f , %.4f )\n\
    offaxis=%.6f [arcmin]  azimuth=%.3f [deg]\n",
	ieul+1, obs->neuler, aetime, expo_frac,
	ea->phi*RAD2DEG, ea->theta*RAD2DEG, ea->psi*RAD2DEG,
	offaxis, azimuth);

    if ( NULL != ene ) free(ene);
    if ( NULL != effarea ) free(effarea);
    nene = xis_effarea(offaxis, azimuth, &ene, &effarea);
    if ( nene < 0 ) goto error;

    istat = xis_psf(offaxis, azimuth, &psf);
    if ( istat ) goto error;

    ipos = 0;
    for (iy = 1; iy <= com.ndety; iy++) {
      ipsfy = iyoff - iy;	/* y-flip for look-down -> look-up */
      if ( 0 <= ipsfy && ipsfy < FASTARF_PSF_NAXIS2 ) {
	psfx = &psf[ipsfy * FASTARF_PSF_NAXIS1];
      } else {
	psfx = NULL;
      }
      for (ix = 1; ix <= com.ndetx; ix++) {
	ipsfx = ixoff + ix;
	if ( NULL != psfx && 0 <= ipsfx && ipsfx < FASTARF_PSF_NAXIS1 ) {
	  vpsf = psfx[ipsfx];
	} else {
	  vpsf = 0.0;
	}
	detimage[0][ipos] = vpsf * com.detmaskmap[ipos];
	ipos++;
      }
    }

    for (ireg = 0; ireg < com.num_region; ireg++) {
      psf_frac[ireg] = 0.0;
    }
    ipos = 0;
    for (iy = 1; iy <= com.region_ny; iy++) {
      for (ix = 1; ix <= com.region_nx; ix++) {
	/** YM **/
	/**	aste_sky2det(com.teldef, ea, skyref, ix, iy, &detx, &dety); **/
	aste_sky2ecs(com.teldef, skyref, ix, iy, &alpha, &delta);
	if ( com.aberration ) {
	  aste_inv_aberration(aetime, mjdrefi, mjdreff, &alpha, &delta);
	}
	aste_ecs2det(com.teldef, ea, alpha, delta, &detx, &dety);
	/** YM **/

	idetx = (int)(detx + 0.5);
	idety = (int)(dety + 0.5);
	if ( 1 <= idetx && idetx <= com.ndetx &&
	     1 <= idety && idety <= com.ndety ) {
	  for (ireg = 0; ireg < com.num_region; ireg++) {
	    if ( 0 < com.regmap[ireg][ipos] ) {
	      com.tot_ccd_pix_reg[ireg] += expo_frac * com.regmap[ireg][ipos];
	      com.sum_detmask_reg[ireg] += expo_frac * com.detmaskmap[(idety-1)*com.ndetx + idetx - 1];
	      vpsf = com.regmap[ireg][ipos] * detimage[idety-1][idetx-1];
	      psf_frac[ireg] += vpsf;
	      com.wmap[ireg][ipos] += 1e6 * expo_frac * vpsf;
	    }
	  }
	}
	ipos++;
      }
    }

    for (ireg = 0; ireg < com.num_region; ireg++) {
      anl_msg_info("\
psf_frac[%d]=%.6f\n", ireg, psf_frac[ireg]);
    }

    iene = 0;
    for (ie = 0; ie < com.num_energ; ie++) {
      e_cen = com.energ_cen[ie];
      while ( iene + 2 < nene ) {
	if ( ene[iene] <= e_cen && e_cen < ene[iene+1] ) {
	  break;
	}
	iene++;
      }
/*      printf("%d: e_cen=%.3f : %.3f - %.3f\n",
	     ie+1, e_cen, ene[iene], ene[iene+1]);*/
      delta_ene = ene[iene+1] - ene[iene];
      s = (e_cen - ene[iene]) / delta_ene;
      t = (ene[iene+1] - e_cen) / delta_ene;

#ifndef CONTAMI_FAST_CALC

      for (ireg = 0; ireg < com.num_region; ireg++) {
	psf_contami_frac[ireg] = 0.0;
      }
      ipos = 0;
      for (iy = 1; iy <= com.region_ny; iy++) {
	for (ix = 1; ix <= com.region_nx; ix++) {
	/** YM **/
	/**	aste_sky2det(com.teldef, ea, skyref, ix, iy, &detx, &dety); **/

      aste_sky2ecs(com.teldef, skyref, ix, iy, &alpha, &delta);
      if ( com.aberration ) {
	aste_inv_aberration(aetime, mjdrefi, mjdreff, &alpha, &delta);
      }
      aste_ecs2det(com.teldef, ea, alpha, delta, &detx, &dety);
	/** YM **/

	  idetx = (int)(detx + 0.5);
	  idety = (int)(dety + 0.5);
	  if ( 1 <= idetx && idetx <= com.ndetx &&
	       1 <= idety && idety <= com.ndety ) {
	    for (ireg = 0; ireg < com.num_region; ireg++) {
	      if ( 0 < com.regmap[ireg][ipos] ) {
		vpsf = com.regmap[ireg][ipos] * detimage[idety-1][idetx-1];
		vpsf *= xis_contami(com.xcp, e_cen, aetime, idetx, idety, NULL, NULL);
		psf_contami_frac[ireg] += vpsf;
	      }
	    }
	  }
	  ipos++;
	}
      }

#else

      xrt_effarea = t * effarea[iene] + s * effarea[iene+1];
      shield_trans = shield_transmission_at(e_cen);
      /** YM
      contami_trans = xis_contami(com.xcp, e_cen, aetime, detx, dety, NULL, NULL);
       YM **/
      contami_trans = xis_contami(com.xcp, e_cen, aetime, detxcen, detycen, NULL, NULL);
      for (ireg = 0; ireg < com.num_region; ireg++) {
	com.specresp[ireg][ie] += expo_frac * xrt_effarea * psf_frac[ireg] * shield_trans * contami_trans;
	com.xrt_effarea[ireg][ie] += expo_frac * xrt_effarea * psf_frac[ireg];
	com.shield_trans[ireg][ie] += expo_frac * shield_trans;
	com.contami_trans[ireg][ie] += expo_frac * contami_trans;
      }

#endif

    }

  }

  *status = ANL_QUIT;
  return;

 error:
  *status = ANL_ERROR;
  return;
}

void
xisarfgen_endrun(int *status)
{
  *status = ANL_OK;
}

void
xisarfgen_exit(int *status)
{
  int ireg, icol;
  fitsfile *ofp;
  int istat = 0;

  anl_msg_info("\n");
  for (ireg = 0; ireg < com.num_region; ireg++) {
    ofp = com.arffp[ireg];
    istat = write_arf_wmap(ofp, ireg);
    if ( istat ) {
      goto error;
    }
    icol = 0;
    if (
fits_write_col_dbl(ofp, ++icol, 1, 1, com.num_energ, com.energ_lo, &istat) ||
fits_write_col_dbl(ofp, ++icol, 1, 1, com.num_energ, com.energ_hi, &istat) ||
fits_write_col_dbl(ofp, ++icol, 1, 1, com.num_energ, com.specresp[ireg], &istat) ||
fits_write_col_dbl(ofp, ++icol, 1, 1, com.num_energ, com.xrt_effarea[ireg], &istat) ||
fits_write_col_dbl(ofp, ++icol, 1, 1, com.num_energ, com.shield_trans[ireg], &istat) ||
fits_write_col_dbl(ofp, ++icol, 1, 1, com.num_energ, com.contami_trans[ireg], &istat) ||
	 0 ) {
      anl_msg_error("\
%s: fits_write_col() failed at icol=%d for '%s' (%d)\n",
	pname, icol, com.arffile[ireg], istat);
      goto error;
    }
    fits_close_file(ofp, &istat);
    if ( istat ) {
      anl_msg_error("\
%s: Cannot close ARF file %s (%d)\n", pname, com.arffile[ireg], istat);
      goto error;
    }

    anl_msg_info("\
Closed #%d: %s\n", ireg+1, com.arffile[ireg]);
  }

  *status = ANL_OK;
  return;

 error:
  *status = ANL_QUIT;
  return;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:8 ***
;;; c-indent-level:2  ***
;;; c-basic-offset:2  ***
;;; End: ***
*/
