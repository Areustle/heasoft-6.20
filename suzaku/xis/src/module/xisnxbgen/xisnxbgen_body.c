/*************************************

  xisnxbgen

	2007/10/31 Y.ISHISAKI	version 1.0
  		created by Y.ISHISAKI

	2007/11/10 Y.ISHISAKI	version 1.1
		add ignore_keys (e.g. TCTYP*) in copy_keys()

	2007/11/23 Y.ISHISAKI	version 1.2
		bug fix in memset() size for com.regmap, nxb.skyimage in _init()

	2008/03/08 Y.ISHISAKI	version 1.3
		add hduclas1, hduclas2, wmrebin, x_offset, y_offset to FITS_IMAGE_KEY
		WMAP image is usable for regfile when region_mode=DETFITS or SKYFITS

	2010/08/22 Y.ISHISAKI	version 1.4
		show warning if TSTART of phafile or time_max exceeds the date of the
		XIS0 anomany in 27 June 2009

*************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <errno.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "pil.h"
#include "anl.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_att.h"
#include "aste_time.h"
#include "aste_orbit.h"
#include "aste_gethk.h"
#include "aste_coord.h"
#include "aste_caldb.h"
#include "aeFitsHeaderUtil.h"
#include "xisTelemFormat.h"
#include "xisEventFitsUtil.h"

#define unless(a) if(!(a))

static char pname[] = "xisnxbgen";
char xisnxbgen_version[] = "version 1.4";

#define XIS_GRADE_MAX	11
#define XIS_PI_MIN		0
#define XIS_PI_MAX		4095
#define XIS_PI_NCH		(XIS_PI_MAX - XIS_PI_MIN + 1)

/* these keywords are always needed
#define DEFAULT_AREASCAL	"\
AREASCAL=                  1.0 / area scaling factor"
#define DEFAULT_BACKSCAL	"\
BACKSCAL=                  1.0 / background file scaling factor"
#define DEFAULT_CORRSCAL	"\
CORRSCAL=                  1.0 / correction file scaling factor"
*/

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
	int spec_siz;				/* 0 or 4096 */
	int image_siz;				/* 0 or 1024x1024 */
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
	double *wmap;				/* WMAP in DET coordinates */
	double *skyimage;
	SORT_INFO *sp;				/* array of (nsort + 1) */
} SORT_DATA;

enum region_mode {
	REGION_MODE_SKYFITS,
	REGION_MODE_DETFITS,
	REGION_MODE_SKYREG,
	REGION_MODE_DETREG,
	REGION_MODE_SKYEXPR,
	REGION_MODE_DETEXPR
};

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
	double crval1, crval2, crota2;		/* SKYREF */
	int wmrebin, x_offset, y_offset;	/* WMAP keywords */
} FITS_IMAGE_KEY;

static struct {
	char outfile[PIL_LINESIZE];
	char phafile[PIL_LINESIZE];	/* pha file */
	char orbit[PIL_LINESIZE];
	char attitude[PIL_LINESIZE];
	char ehkfile[PIL_LINESIZE];
	char gtifile[PIL_LINESIZE];

	char time_min[PIL_LINESIZE];
	char time_max[PIL_LINESIZE];
	double t0, t1;

	char sortkey[PIL_LINESIZE];	/* default 'COR2' */
	char sortstep[PIL_LINESIZE];/* default '0,4,5,6,7,8,9,10,11,12,13,99' */
	double gti_min_sec;
	double ehk_margin_sec;
	int pi_min;
	int pi_max;
	char grades[PIL_LINESIZE];	/* default '0,2,3,4,6' */
	int grademask;				/* grade mask (0-31) made from grades */
	int apply_xisftools;
	int enable_pixq;
	unsigned int pixq_min;
	unsigned int pixq_max;
	unsigned int pixq_and;
	unsigned int pixq_eql;
	int aberration;
	int clobber;

	char *teldeffile, o_teldeffile[PIL_LINESIZE];
	char *leapfile, o_leapfile[PIL_LINESIZE];
	char *rigidity, o_rigidity[PIL_LINESIZE];
	char *nxbevent, o_nxbevent[PIL_LINESIZE];
	char *nxborbit, o_nxborbit[PIL_LINESIZE];
	char *nxbvdchk, o_nxbvdchk[PIL_LINESIZE];
	char *nxbcorhk, o_nxbcorhk[PIL_LINESIZE];
	char *nxbevent_or_tmpfile;	/* point to com.nxbevnet or temporary file */

	char region_mode[PIL_LINESIZE];
	enum region_mode region_mode_id;
	char regfile[PIL_LINESIZE];
	FITS_IMAGE_KEY region_kp;
	float *regmap;

	char detmask[PIL_LINESIZE];
	FITS_IMAGE_KEY detmask_kp;
	int ndetx, ndety;
	float *detmaskmap;
	double sum_detmask;
	double tot_ccd_pix;

	ORBIT *obs_orbit;
	ORBIT *nxb_orbit;
	AtRigData2 *rdp;

	ASTE_HK *obs_hk;
	ASTE_HK *nxb_hk;
	int obs_hkid;
	int nxb_hkid;

	char instrume_phafile[FLEN_VALUE];
	double tstart_phafile;
	double tstop_phafile;

} com;

static int
show_xis0_anomaly_warning(void)
{
	static double xis0_anomay_aetime = 299030402.0;	/* 2009-06-23T00:00:00 */

	if ( ASTE_XIS0_ID == aste_instrume_id(com.instrume_phafile) ) {
		if ( xis0_anomay_aetime < com.tstop_phafile ||
			 xis0_anomay_aetime < com.t1 ) {
			anl_msg_warning("\
%s: WARNING: **********************************************************\n\
    After the XIS0 anomaly in 23 June 2009, we apply an area discrimination\n\
    (AD) to all the XIS0 data starting from June 27, 2009 16:27 (UT).\n\
    The discriminated area is in the segment A, covering about 1/8 of the\n\
    entire chip. If (a) the duration covers after the XIS0 anomaly event\n\
    and the subsequent AD operation, and (b) the source extraction region\n\
    includes the AD-ed region, the resultant NXB spectrum may become\n\
    inappropriate. See the web page of\n\
    http://www.astro.isas.jaxa.jp/suzaku/analysis/xis/xis0_area_discriminaion/\n\
    for details.\n\
******************************************************************************\n\
", pname);
		}
		return -1;
	}

	return 0;
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

static float *
make_ds9reg_image(TELDEF *teldef, SKYREF *skyref,
	char *regfile, char *regexpr, int nx, int ny)
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
	TELDEF_ASTROE *aste = teldef->mission.aste;

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
fits_write_key_fixdbl(fp, k="TCRVL1", skyref->alpha, 5,
	"X reference pixel value (deg)", &istat) ||
fits_write_key_fixdbl(fp, k="TCDLT1", tcdltx, 7,
	"X pixel scale (deg/pixel)", &istat) ||
fits_write_key_fixdbl(fp, k="TCROT1", skyref->roll, 5,
	"X pixel rotation (deg)", &istat) ||
fits_write_key_str(fp, k="TCTYP1", "RA---TAN",
	"X coordinate projection method", &istat) ||
fits_write_key_fixdbl(fp, k="TCRPX2", aste->sky.ycen, 1,
	"Y reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="TCRVL2", skyref->delta, 5,
	"Y reference pixel value (deg)", &istat) ||
fits_write_key_fixdbl(fp, k="TCDLT2", tcdlty, 7,
	"Y pixel scale (deg/pixel)", &istat) ||
fits_write_key_fixdbl(fp, k="TCROT2", skyref->roll, 5,
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

static float *
read_detfits(TELDEF *teldef, char *regfile, FITS_IMAGE_KEY *kp)
{
	float *image, *image2;
	int nx, ny, nx2, ny2;
	int ix, iy, ipos, ix2, iy2, ipos2;

	nx = teldef->mission.aste->det.xsiz;
	ny = teldef->mission.aste->det.ysiz;

	image = read_fits_image(regfile, 0, &nx2, &ny2, kp);
	if ( NULL == image ) {
		return NULL;
	}

	if ( 0 == strcmp(kp->hduclas1, "IMAGE") &&
		 0 == strcmp(kp->hduclas2, "WMAP") ) {
		if ( 0 != strcmp(kp->mtype1, "DET") ||
			 0 != strcmp(kp->mform1, "DETX,DETY") ) {
			anl_msg_error("\
%s: source_image is WMAP, but not in DET coordinates\n", pname);
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
   converting WMAP [%dx%d*%d+%d+%d] into DET image\n",
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
%s: regfile size mismatch (nx=%d, ny=%d)\n", pname, nx2, ny2);
		free(image);
		return NULL;
	} else if ( '\0' == *kp->ctype1 && '\0' == *kp->ctype2 ) {
		anl_msg_warning("\
%s: WARNING: CTYPE1, CTYPE2 is not specified, assuming DETX, DETY\n", pname);
	} else if ( 0 == CLstricmp(kp->ctype1, "DETX") &&
			    0 == CLstricmp(kp->ctype2, "DETY") ) {
		;
	} else if ( 0 == CLstricmp(kp->ctype1, "ACTX") &&
			    0 == CLstricmp(kp->ctype2, "ACTY") ) {
		if ( convert_act_to_det(teldef, nx, ny, image) ) {
			free(image);
			return NULL;
		}
	} else {
		anl_msg_error("\
%s: invalid CTYPE1='%s', CTYPE2='%s'\n", pname, kp->ctype1, kp->ctype2);
		free(image);
		return NULL;
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

static float *
read_skyfits(TELDEF *teldef, char *regfile, FITS_IMAGE_KEY *kp)
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
%s: regfile size mismatch (nx=%d, ny=%d)\n", pname, nx2, ny2);
		free(image);
		return NULL;
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
read_detmask_merge_regmap(TELDEF *teldef, char *detmask)
{
	int nx, ny, ipos, nxny;
	float *image;

	nx = com.ndetx = teldef->mission.aste->det.xsiz;
	ny = com.ndety = teldef->mission.aste->det.ysiz;

	if ( 0 == CLstricmp("none", detmask) ) {
		com.detmaskmap = NULL;
		return 0;
	}

	anl_msg_info("\
%s: reading detmask '%s'\n", pname, detmask);

	image = read_detfits(teldef, detmask, &com.detmask_kp);
	if ( NULL == image ) {
		return -1;
	}

	anl_msg_info("\
   %dx%d image read\n", nx, ny);

	nxny = nx * ny;
	com.tot_ccd_pix = nxny;
	com.sum_detmask = 0.0;
	for (ipos = 0; ipos < nxny; ipos++) {
		com.sum_detmask += image[ipos];
		com.regmap[ipos] *= image[ipos];
	}
	com.detmaskmap = image;

	anl_msg_info("\
   mask_ratio_ccd = %.1f / %.0f = %.6f\n",
		com.sum_detmask, com.tot_ccd_pix, com.sum_detmask/com.tot_ccd_pix);

	return 0;
}

static int
get_value_cor(double aetime, ORBIT *orbit, int hkid, double *cor)
{
	int istat;
	double mjd;
	AtVect vSat, vSatG;
	AtPolarVect pvSatG;

	istat = aste_orbit(orbit, aetime, vSat, NULL);
	if ( istat ) goto error;
	mjd = aste2mjd(aetime);
	atGeodetic(mjd, vSat, vSatG);
	atVectToPol(vSatG, &pvSatG);
	atRigidityD(&pvSatG, cor);

	return 0;

 error:
	return -1;
}

static int
get_value_cor2(double aetime, ORBIT *orbit, int hkid, double *cor2)
{
	int istat;
	double mjd;
	AtVect vSat, vSatG;
	AtPolarVect pvSatG;

	istat = aste_orbit(orbit, aetime, vSat, NULL);
	if ( istat ) goto error;
	mjd = aste2mjd(aetime);
	atGeodetic(mjd, vSat, vSatG);
	atVectToPol(vSatG, &pvSatG);
	atRigidity2(com.rdp, &pvSatG, cor2);

	return 0;

 error:
	return -1;
}

static int
get_value_ehk(double aetime0, ORBIT *orbit, int hkid, double *ehk)
{
	int istat, num_trial;
	double aetime, stime[3], dt0, dt1, dt2;
/* aste_gethk.h
	double *stime		output: time iroiro
   					stime[0]: time when value was set,
   					stime[1]: previous time when HK is put,
   					stime[2]: next time when HK is put
*/

	aetime = aetime0;
	num_trial = 0;

 again:
	num_trial++;
	if ( 4 < num_trial ) {
		anl_msg_error("\
%s: too many trials in get_value_ehk() at t=%.3f\n", pname, aetime0);
		goto error;
	}
	stime[0] = stime[1] = stime[2] = aetime;
	istat = aste_gethk(hkid, aetime, TDOUBLE, 1, ehk, stime);
	if ( ASTE_GETHK_GTI_ERROR == istat ) {
		if ( stime[2] != aetime ) {
			aetime = stime[2];
			goto again;
		} else if ( stime[1] != aetime ) {
			aetime = stime[1];
			goto again;
		}
	}
	dt0 = fabs(aetime - stime[0]);
	dt1 = fabs(aetime - stime[1]);
	dt2 = fabs(aetime - stime[2]);
	if ( dt1 < dt0 ) {
		aetime = stime[0];
		goto again;
	} else if ( dt2 < dt0 ) {
		aetime = stime[2];
		goto again;
	}

	if ( com.ehk_margin_sec < dt0 ) {
		anl_msg_error("\
%s: EHK not found within ehk_margin_sec=%.1f (%.1f) at t=%.1f\n",
			pname, com.ehk_margin_sec, dt0, aetime0);
		goto error;
	}

	return 0;

 error:
	return -1;
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
		p->idiv = 0;			/* goto next line */
	}

	return 0;

 quit:
	return istat;
}

#if 0	/* not used */
static int
compare_double(const void *v1, const void *v2)
{
	if ( *(double *)v1 < *(double *)v2 ) {
		return -1;
	} else if ( *(double *)v1 > *(double *)v2 ) {
		return +1;
	}
	return 0;
}

static int
get_next_exptime_events(fitsfile *fp, EXPOSURE_INFO *p)
{
	char *k;
	int i, n, nalloc, col_t, anul;
	long irow, nrows;
	double t, *tbuf;

	int istat = 0;

	if ( 0 == p->irow ) {
		fits_get_num_rows(fp, &nrows, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_get_num_rows() failed (%d)\n", pname, istat);
			goto quit;
		}
		if ( nrows < 1 ) {
			anl_msg_error("\
%s: empty EVENTS extension\n", pname);
			istat = -1;
			goto quit;
		}
		fits_get_colnum(fp, CASESEN, k="TIME", &col_t, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
			goto quit;
		}
		nalloc = 0;
		tbuf = NULL;
		n = 0;
		for (irow = 1; irow <= nrows; irow++) {
			fits_read_col_dbl(fp, col_t, irow, 1, 1, 0.0, &t, &anul, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_read_col('TIME') failed (%d)\n", pname, istat);
				goto quit;
			}
			for (i = n - 1; 0 <= i && t != tbuf[i]; i--) {
				;
			}
			if ( i < 0 ) {		/* not found */
				if ( nalloc <= n ) {
					nalloc += 10000;
					tbuf = realloc(tbuf, sizeof(*tbuf) * nalloc);
					if ( NULL == tbuf ) {
						anl_msg_error("\
%s: tbuf realloc(size=%d) failed\n", pname, nalloc);
						istat = -1;
						goto quit;
					}
				}
				tbuf[n] = t;
				n++;
			}
		}
		p->tbuf = realloc(tbuf, sizeof(*tbuf) * n);
		if ( NULL == p->tbuf ) {
			p->tbuf = tbuf;		/* tbuf is kept unchanged */
		}
		qsort(tbuf, n, sizeof(*tbuf), compare_double);
		p->nrows = n;
		p->tbuf = tbuf;
		anl_msg_info("\
%s: extracting time from EVENTS: n=%ld nexp=%d winexp=%.1f\n",
					 pname, nrows, n, p->winexp);
	}

	if ( p->irow < p->nrows ) {
		p->flag_end = 0;
		p->t = p->tbuf[p->irow];
		p->irow++;
	} else {
		p->flag_end = 1;
		free(p->tbuf);
	}

	return 0;

 quit:
	return istat;
}
#endif

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
calc_eulinfo(TELDEF *teldef, ATTFILE *attfile, fitsfile *fp, SORT_DATA *obs)
{
	char *k;
	int iexp, nexp, neuler, nalloc, hdutype;
	int i, n, idetx, idety;
	EXPOSURE_INFO expinfo;
	int (*get_next_exptime)(fitsfile *fp, EXPOSURE_INFO *p);
	AtEulerAng ea;
	EULER_INFO *eulinfo, *ep;
	double detx, dety, alpha, delta;

	int istat = 0;

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
		istat = 0;	/* ignore error */
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
		istat = aste_att_ea(attfile, expinfo.t, &ea);
		if ( istat ) {
			anl_msg_error("\
%s: aste_att_ea(t=%.0f) failed (%d)\n", pname, expinfo.t, istat);
			goto quit;
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
		alpha = obs->skyref.alpha;
		delta = obs->skyref.delta;
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
conv_skyreg(TELDEF *teldef, SORT_DATA *obs, float *skyreg, float *detreg)
{
	unsigned char *cormap;
	double *winmap;
	float *sp;
	int i, ipos, ieuler;
	int ix, iy, iskyx, iskyy;
	EULER_INFO *ep;
	AtEulerAng ea;
	struct { int x, y; } sky[4];
	struct { double x, y; } det[4];
	double x, x0, x10, x20, x3210;
	double y, y0, y10, y20, y3210;
	double t, detx, dety, alpha, delta, eulexp, sexp;

	SKYREF *skyref = &obs->skyref;
	TELDEF_ASTROE *aste = teldef->mission.aste;
	int det_xpix1 = aste->det.xpix1;
	int det_ypix1 = aste->det.ypix1;
	int sky_xpix1 = aste->sky.xpix1;
	int sky_ypix1 = aste->sky.ypix1;
	int nx = aste->det.xsiz;
	int ny = aste->det.ysiz;
	int nxny = nx * ny;
	int nskyx = aste->sky.xsiz;
	int nskyy = aste->sky.ysiz;
	int mjdrefi = obs->mjdrefi;
	double mjdreff = obs->mjdreff;

	sky[0].x = sky_xpix1;
	sky[0].y = sky_ypix1;
	sky[1].x = sky_xpix1 + nskyx - 1;
	sky[1].y = sky_ypix1;
	sky[2].x = sky_xpix1;
	sky[2].y = sky_ypix1 + nskyy - 1;
	sky[3].x = sky_xpix1 + nskyx - 1;
	sky[3].y = sky_ypix1 + nskyy - 1;

	winmap = malloc( sizeof(*winmap) * nxny + sizeof(*cormap) * nxny );
	if ( NULL == winmap ) {
		anl_msg_error("\
%s: winmap malloc(size=%d) failed\n", pname, nxny);
		return -1;
	}
	cormap = (unsigned char *)&winmap[nxny];

	for (ieuler = 0; ieuler < obs->neuler; ieuler++) {
		ep = &obs->eulinfo[ieuler];
		ea = ep->ea;
		t  = ep->t;
		for (ipos = 0; ipos < nxny; ipos++) {
			winmap[ipos] = 0.0;
			cormap[ipos] = 0;
		}
		anl_msg_info("\
%4d: n=%-6d [%d %d] (%.6f %.6f %.6f) t=%.1f\n",
			ieuler+1, ep->nexp, ep->detx, ep->dety,
			ea.phi*RAD2DEG, ea.theta*RAD2DEG, ea.psi*RAD2DEG, t);
		sp = skyreg;

		for (i = 0; i < 4; i++) {
			aste_sky2ecs(teldef, skyref, sky[i].x, sky[i].y, &alpha, &delta);
			if ( com.aberration ) {
				aste_inv_aberration(t, mjdrefi, mjdreff, &alpha, &delta);
			}
			aste_ecs2det(teldef, &ea, alpha, delta, &det[i].x, &det[i].y);
		}

		x0 = det[0].x;
		x10 = det[1].x - x0;
		x20 = det[2].x - x0;
		x3210 = det[3].x - det[2].x - det[1].x + x0;

		y0 = det[0].y;
		y10 = det[1].y - y0;
		y20 = det[2].y - y0;
		y3210 = det[3].y - det[2].y - det[1].y + y0;

		for (iskyy = 0; iskyy < nskyy; iskyy++) {
			for (iskyx = 0; iskyx < nskyx; iskyx++) {
				sexp = *sp++;

#define calc_winmap()	do {\
  detx = x0 + x*x10 + y*x20 + x*y*x3210;\
  dety = y0 + x*y10 + y*y20 + x*y*y3210;\
  ix = (int)(detx + 0.5) - det_xpix1;\
  if ( 0 <= ix && ix < nx ) {\
    iy = (int)(dety + 0.5) - det_ypix1;\
    if ( 0 <= iy && iy < ny ) {\
      ipos = iy * nx + ix;\
      winmap[ipos] += sexp;\
      cormap[ipos] += 1;\
    }\
  }\
} while (0)

				x = (iskyx - 0.25) / (nskyx - 1);
	  			y = (iskyy - 0.25) / (nskyy - 1);
          		calc_winmap();		/* [iskyx-0.25, iskyy-0.25] */
          		x = (iskyx + 0.25) / (nskyx - 1);
          		calc_winmap();		/* [iskyx+0.25, iskyy-0.25] */
	  			y = (iskyy + 0.25) / (nskyy - 1);
          		calc_winmap();		/* [iskyx+0.25, iskyy+0.25] */
          		x = (iskyx - 0.25) / (nskyx - 1);
          		calc_winmap();		/* [iskyx-0.25, iskyy+0.25] */
			}
		}

#undef calc_winmap

		eulexp = (double)ep->nexp / obs->nexp;
		for (ipos = 0; ipos < nxny; ipos++) {
			if ( 0 < cormap[ipos] ) {
				detreg[ipos] += eulexp * winmap[ipos] / cormap[ipos];
			}
		}

    }

	free(winmap);
	return 0;
}

static int
conv_detimage(TELDEF *teldef, SORT_DATA *obs, SORT_DATA *nxb)
{
	unsigned char *cormap;
	double *winmap;
	double *dp;
	int i, ipos, ieuler;
	int ix, iy, idetx, idety;
	EULER_INFO *ep;
	AtEulerAng ea;
	struct { int x, y; } det[4];
	struct { double x, y; } sky[4];
	double x, x0, x10, x20, x3210;
	double y, y0, y10, y20, y3210;
	double t, skyx, skyy, alpha, delta, eulexp, dexp;

	SKYREF *skyref = &obs->skyref;
	TELDEF_ASTROE *aste = teldef->mission.aste;
	int det_xpix1 = aste->det.xpix1;
	int det_ypix1 = aste->det.ypix1;
	int sky_xpix1 = aste->sky.xpix1;
	int sky_ypix1 = aste->sky.ypix1;
	int nx = aste->sky.xsiz;
	int ny = aste->sky.ysiz;
	int nxny = nx * ny;
	int ndetx = aste->det.xsiz;
	int ndety = aste->det.ysiz;
	int mjdrefi = obs->mjdrefi;
	double mjdreff = obs->mjdreff;

	det[0].x = det_xpix1;
	det[0].y = det_ypix1;
	det[1].x = det_xpix1 + ndetx - 1;
	det[1].y = det_ypix1;
	det[2].x = det_xpix1;
	det[2].y = det_ypix1 + ndety - 1;
	det[3].x = det_xpix1 + ndetx - 1;
	det[3].y = det_ypix1 + ndety - 1;

	winmap = malloc( sizeof(*winmap) * nxny + sizeof(*cormap) * nxny );
	if ( NULL == winmap ) {
		anl_msg_error("\
%s: winmap malloc(size=%d) failed\n", pname, nxny);
		return -1;
	}
	cormap = (unsigned char *)&winmap[nxny];

	for (ieuler = 0; ieuler < obs->neuler; ieuler++) {
		ep = &obs->eulinfo[ieuler];
		ea = ep->ea;
		t  = ep->t;
		for (ipos = 0; ipos < nxny; ipos++) {
			winmap[ipos] = 0.0;
			cormap[ipos] = 0;
		}
		anl_msg_info("\
%4d: n=%-6d [%d %d] (%.6f %.6f %.6f) t=%.1f\n",
			ieuler+1, ep->nexp, ep->detx, ep->dety,
			ea.phi*RAD2DEG, ea.theta*RAD2DEG, ea.psi*RAD2DEG, t);
		dp = nxb->sp[nxb->nsort].image;

		for (i = 0; i < 4; i++) {
			aste_det2ecs(teldef, &ea, det[i].x, det[i].y, &alpha, &delta);
			if ( com.aberration ) {
				aste_cor_aberration(t, mjdrefi, mjdreff, &alpha, &delta);
			}
			aste_ecs2sky(teldef, skyref, alpha, delta, &sky[i].x, &sky[i].y);
		}

		x0 = sky[0].x;
		x10 = sky[1].x - x0;
		x20 = sky[2].x - x0;
		x3210 = sky[3].x - sky[2].x - sky[1].x + x0;

		y0 = sky[0].y;
		y10 = sky[1].y - y0;
		y20 = sky[2].y - y0;
		y3210 = sky[3].y - sky[2].y - sky[1].y + y0;

		for (idety = 0; idety < ndety; idety++) {
			for (idetx = 0; idetx < ndetx; idetx++) {
				dexp = *dp++;

#define calc_winmap()	do {\
  skyx = x0 + x*x10 + y*x20 + x*y*x3210;\
  skyy = y0 + x*y10 + y*y20 + x*y*y3210;\
  ix = (int)(skyx + 0.5) - sky_xpix1;\
  if ( 0 <= ix && ix < nx ) {\
    iy = (int)(skyy + 0.5) - sky_ypix1;\
    if ( 0 <= iy && iy < ny ) {\
      ipos = iy * nx + ix;\
      winmap[ipos] += dexp;\
      cormap[ipos] += 1;\
    }\
  }\
} while (0)

				x = (idetx - 0.25) / (ndetx - 1);
	  			y = (idety - 0.25) / (ndety - 1);
          		calc_winmap();		/* [idetx-0.25, idety-0.25] */
          		x = (idetx + 0.25) / (ndetx - 1);
          		calc_winmap();		/* [idetx+0.25, idety-0.25] */
	  			y = (idety + 0.25) / (ndety - 1);
          		calc_winmap();		/* [idetx+0.25, idety+0.25] */
          		x = (idetx - 0.25) / (ndetx - 1);
          		calc_winmap();		/* [idetx-0.25, idety+0.25] */
			}
		}

#undef calc_winmap

		eulexp = (double)ep->nexp / obs->nexp;
		for (ipos = 0; ipos < nxny; ipos++) {
			if ( 0 < cormap[ipos] ) {
				nxb->skyimage[ipos] += eulexp * winmap[ipos] / cormap[ipos];
			}
		}

    }

	free(winmap);
	return 0;
}

static int
parse_time_min_max(char *param, char *ts, double obstime, double *aetime_ptr)
{
	int istat;
	double aetime, toffs;

	toffs = 0.0;
	if ( '+' == *ts || '-' == *ts ) {	/* days after or before observation */
		sscanf(ts, "%lf", &toffs);
		aetime = obstime + toffs * 24 * 60 * 60;
	} else {
		istat = aefits_datestr2aetime(ts, &aetime);
		if ( istat ) {
			if ( 1 != sscanf(ts, "%lf", &aetime) ) {
				anl_msg_error("\
%s: illegal time format of %s='%s'\n", pname, param, ts);
				return istat;
			}
		}
	}
	*aetime_ptr = aetime;

	return 0;
}

static int
parse_grades(char *grades, int *grademask)
{
	static char delimiters[] = " ,";

	char *p;
	int g;

	p = grades;
	*grademask = 0;
	for (;;) {
		if ( 1 != sscanf(p, "%d", &g) ) {
			break;
		}
		if ( g <= XIS_GRADE_MAX ) {
			*grademask |= (1 << g);
		}
		for (;;) {
			p++;
			if ( '\0' == *p ) {
				break;
			}
			if ( NULL == strchr(delimiters, *p) ) {
				continue;
			}
			p++;	/* skip delimiter */
			break;
		}
		if ( '\0' == *p ) {
			break;
		}
	}

	return 0;
}

static int
alloc_sort_data(char *sortstep, SORT_DATA *obs, SORT_DATA *nxb)
{
	static char delimiters[] = " ,";

	char *p;
	int i, nstep;
	double steps[PIL_LINESIZE];
	SORT_INFO *obs_sp, *nxb_sp;

	nstep = 0;
	p = sortstep;
	for (;;) {
		if ( 1 != sscanf(p, "%lf", &steps[nstep]) ) {
			break;
		}
		if ( 0 < nstep && steps[nstep] <= steps[nstep-1] ) {
			anl_msg_error("\
%s: sortstep='%s' is not ascending order\n", pname, sortstep);
			return -1;
		}
		nstep++;
		for (;;) {
			p++;
			if ( '\0' == *p ) {
				break;
			}
			if ( NULL == strchr(delimiters, *p) ) {
				continue;
			}
			p++;	/* skip delimiter */
			break;
		}
		if ( '\0' == *p ) {
			break;
		}
	}

	if ( nstep < 2 ) {
		anl_msg_error("\
%s: too small nstep=%d for sortstep='%s'\n", pname, nstep, sortstep);
		return -1;
	}

	obs->sp = malloc( 2 * sizeof(SORT_INFO) * nstep );
	if ( NULL == obs->sp ) {
		anl_msg_error("\
%s: malloc(nstep=%d) failed for SORT_INFO\n", pname, nstep);
		return -1;
	}

	nxb->sp = &obs->sp[nstep];
	obs->nsort = nxb->nsort = nstep - 1;
	obs->total_expo = nxb->total_expo = 0.0;

	for (i = 0; i < nstep; i++) {
		obs_sp = &obs->sp[i];
		nxb_sp = &nxb->sp[i];
		if ( i < nstep - 1 ) {
			obs_sp->cor_lo = nxb_sp->cor_lo = steps[i];
			obs_sp->cor_hi = nxb_sp->cor_hi = steps[i+1];
		} else {
			obs_sp->cor_lo = nxb_sp->cor_lo = steps[0];
			obs_sp->cor_hi = nxb_sp->cor_hi = steps[i];
		}
		obs_sp->expo = nxb_sp->expo = 0.0;
		obs_sp->spec_count = nxb_sp->spec_count = 0.0;
		obs_sp->image_count = nxb_sp->image_count = 0.0;
		obs_sp->spec = NULL;
		if ( 0 < obs->spec_siz ) {
			obs_sp->spec = malloc( 2 * sizeof(*obs_sp->spec) * obs->spec_siz );
			if ( NULL == obs_sp->spec ) {
				anl_msg_error("\
%s: malloc(spec_siz=%d) failed for obs[%d].spec\n",
					pname, obs->spec_siz, i);
				return -1;
			}
			memset(obs_sp->spec, 0, 2 * sizeof(*obs_sp->spec) * obs->spec_siz);
			obs_sp->serr = &obs_sp->spec[obs->spec_siz];
		}
		obs_sp->image = NULL;
		if ( 0 < obs->image_siz ) {
			obs_sp->image = malloc(sizeof(*obs_sp->image) * obs->image_siz);
			if ( NULL == obs_sp->image ) {
				anl_msg_error("\
%s: malloc(image_siz=%d) failed for obs[%d].image\n",
					pname, obs->image_siz, i);
				return -1;
			}
			memset(obs_sp->image, 0, sizeof(*obs_sp->image) * obs->image_siz);
		}
		nxb_sp->spec = NULL;
		if ( 0 < nxb->spec_siz ) {
			nxb_sp->spec = malloc( 2 * sizeof(*nxb_sp->spec) * nxb->spec_siz );
			if ( NULL == nxb_sp->spec ) {
				anl_msg_error("\
%s: malloc(spec_siz=%d) failed for nxb[%d].spec\n",
					pname, nxb->spec_siz, i);
				return -1;
			}
			memset(nxb_sp->spec, 0, 2 * sizeof(*nxb_sp->spec) * nxb->spec_siz);
			nxb_sp->serr = &nxb_sp->spec[nxb->spec_siz];
		}
		nxb_sp->image = NULL;
		if ( 0 < nxb->image_siz ) {
			nxb_sp->image = malloc(sizeof(*nxb_sp->image) * nxb->image_siz);
			if ( NULL == nxb_sp->image ) {
				anl_msg_error("\
%s: malloc(image_siz=%d) failed for nxb[%d].image\n",
					pname, nxb->image_siz, i);
				return -1;
			}
			memset(nxb_sp->image, 0, sizeof(*nxb_sp->image) * nxb->image_siz);
		}
	}

	return 0;
}

static int
read_obs_gti(fitsfile *fp, SORT_DATA *obs)
{
	char *k, datestr[80];
	int i, nsort, hdutype;
	double aetime, t0, t1, cor, sum_expo, total_expo;
	double aetime_start, aetime_stop, aetime_average;
	EXPOSURE_INFO expinfo;
	int (*get_next_exptime)(fitsfile *fp, EXPOSURE_INFO *p);
	int (*get_value)(double aetime, ORBIT *orbit, int hkid, double *cor);

	int istat = 0;

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
		istat = 0;	/* ignore error */
		fits_movnam_hdu(fp, BINARY_TBL, "STDGTI", 0, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: no GTI/STDGTI extension is found (%d)\n", pname, istat);
			goto quit;
		}
	}

	if ( 0 == CLstricmp("COR2", com.sortkey) ) {
		get_value = get_value_cor2;
	} else if ( 0 == CLstricmp("COR", com.sortkey) ) {
		get_value = get_value_cor;
	} else {
		get_value = get_value_ehk;
	}

	aetime_start = 1e99;
	aetime_stop = -1e99;
	aetime_average = 0.0;
	nsort = obs->nsort;
	total_expo = 0.0;
	for (;;) {
		istat = (*get_next_exptime)(fp, &expinfo);
		if ( istat ) goto quit;
		if ( expinfo.flag_end ) {
			break;
		}
		aetime = expinfo.t;
		t0 = aetime - 0.5 * expinfo.winexp;
		t1 = aetime + 0.5 * expinfo.winexp;
		if ( t0 < aetime_start ) {
			aetime_start = t0;
		}
		if ( aetime_stop < t1 ) {
			aetime_stop = t1;
		}
		aetime_average += expinfo.winexp * aetime;
		istat = (*get_value)(aetime, com.obs_orbit, com.obs_hkid, &cor);
		if ( istat ) goto quit;
		for (i = 0; i < nsort; i++) {
			if ( obs->sp[i].cor_lo <= cor && cor < obs->sp[i].cor_hi ) {
				obs->sp[i].expo += expinfo.winexp;
				break;
			}
		}
		total_expo += expinfo.winexp;
	}

	if ( 0.0 == total_expo ) {
		anl_msg_error("\
%s: invalid EXPOSURE=0.0\n", pname);
		istat = -1;
		goto quit;
	}
	aetime_average /= total_expo;

	anl_msg_info("\
   Target observation start : %.1f [%s]\n",
		aetime_start, aefits_aetime2datestr(aetime_start, datestr));
	anl_msg_info("\
   Average observation at   : %.1f [%s]\n",
		aetime_average, aefits_aetime2datestr(aetime_average, datestr));
	anl_msg_info("\
   Target observation stop  : %.1f [%s]\n",
		aetime_stop, aefits_aetime2datestr(aetime_stop, datestr));
	sum_expo = 0.0;
	anl_msg_info("\
===========================================\n\
%-14s:  EXPOSURE (s)  FRACTION (%%)\n\
-------------------------------------------\n", com.sortkey);
	for (i = 0; i < nsort; i++) {
		anl_msg_info("\
%5.1f - %5.1f : %12.1f  %12.3f\n",
			obs->sp[i].cor_lo, obs->sp[i].cor_hi,
			obs->sp[i].expo, 100 * obs->sp[i].expo / total_expo);
		sum_expo += obs->sp[i].expo;
	}
	anl_msg_info("\
-------------------------------------------\n\
 %12s : %12.1f  %12.3f\n", "SUM", sum_expo, 100 * sum_expo / total_expo);
	anl_msg_info("\
 %12s : %12.1f  %12.3f\n\
-------------------------------------------\n", "TOTAL", total_expo, 100.0);

	obs->expinfo = expinfo;
	obs->aetime_start = aetime_start;
	obs->aetime_stop = aetime_stop;
	obs->aetime_average = aetime_average;
	obs->total_expo = total_expo;

	return 0;

 quit:
	return istat;
}

static int
read_nxb_evt(fitsfile *fp, SORT_DATA *obs, SORT_DATA *nxb)
{
	char *k, datestr[80];
	long irow, nrows, nrej_time, nrej_grade, nrej_pixq, nrej_detxy, nrej_pi;
	int i, ic, ipos, nsort, hdutype, anul;
	int col_t, col_acty, col_detx, col_dety, col_pi, col_grade, col_pixq;
	int acty, detx, dety, pi, grade;
	unsigned int pixq;
	double aetime, t0, t1, last_aetime, cor;
	double sum_expo, total_expo, total_count, sum_spec_count, sum_image_count;
	double aetime_start, aetime_stop, aetime_average;
	double exposure_before_time_min, exposure_after_time_max;
	double fobs, expo, w, w2;
	EXPOSURE_INFO expinfo;
	int (*get_next_exptime)(fitsfile *fp, EXPOSURE_INFO *p);
	int (*get_value)(double aetime, ORBIT *orbit, int hkid, double *cor);

	int istat = 0;

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
		istat = 0;	/* ignore error */
		fits_movnam_hdu(fp, BINARY_TBL, "STDGTI", 0, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: no GTI/STDGTI extension is found (%d)\n", pname, istat);
			goto quit;
		}
	}

	if ( 0 == CLstricmp("COR2", com.sortkey) ) {
		get_value = get_value_cor2;
	} else if ( 0 == CLstricmp("COR", com.sortkey) ) {
		get_value = get_value_cor;
	} else {
		get_value = get_value_ehk;
	}

	aetime_start = 1e99;
	aetime_stop = -1e99;
	aetime_average = 0.0;
	exposure_before_time_min = exposure_after_time_max = 0.0;
	nsort = nxb->nsort;
	total_expo = 0.0;
	for (;;) {
		istat = (*get_next_exptime)(fp, &expinfo);
		if ( istat ) goto quit;
		if ( expinfo.flag_end ) {
			break;
		}
		aetime = expinfo.t;
		t0 = aetime - 0.5 * expinfo.winexp;
		t1 = aetime + 0.5 * expinfo.winexp;
		if ( t0 < com.t0 ) {
			exposure_before_time_min += expinfo.winexp;
			continue;
		}
		if ( com.t1 < t1 ) {
			exposure_after_time_max += expinfo.winexp;
			continue;
		}
		if ( t0 < aetime_start ) {
			aetime_start = t0;
		}
		if ( aetime_stop < t1 ) {
			aetime_stop = t1;
		}
		aetime_average += expinfo.winexp * aetime;
		istat = (*get_value)(aetime, com.nxb_orbit, com.nxb_hkid, &cor);
		if ( istat ) goto quit;
		for (i = 0; i < nsort; i++) {
			if ( nxb->sp[i].cor_lo <= cor && cor < nxb->sp[i].cor_hi ) {
				 nxb->sp[i].expo += expinfo.winexp;
				break;
			}
		}
		total_expo += expinfo.winexp;
	}

	if ( 0.0 == total_expo ) {
		anl_msg_error("\
%s: invalid EXPOSURE=0.0\n", pname);
		istat = -1;
		goto quit;
	}
	aetime_average /= total_expo;

	anl_msg_info("\
   exposure_before_time_min=%.1f  exposure_after_time_max=%.1f\n",
		exposure_before_time_min, exposure_after_time_max);
	anl_msg_info("\
   NXB database accumulation start : %.1f [%s]\n",
		aetime_start, aefits_aetime2datestr(aetime_start, datestr));
	anl_msg_info("\
   Average accumulation time at    : %.1f [%s]\n",
		aetime_average, aefits_aetime2datestr(aetime_average, datestr));
	anl_msg_info("\
   NXB database accumulation stop  : %.1f [%s]\n",
		aetime_stop, aefits_aetime2datestr(aetime_stop, datestr));

	fits_movnam_hdu(fp, BINARY_TBL, "EVENTS", 0, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: EVENTS extension is not found (%d)\n", pname, istat);
		goto quit;
	}

	fits_get_num_rows(fp, &nrows, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_num_rows() failed (%d)\n", pname, istat);
		goto quit;
	}
	if ( nrows < 1 ) {
		anl_msg_error("\
%s: empty EVENTS extension\n", pname);
		istat = -1;
		goto quit;
	}

	if (
fits_get_colnum(fp, CASESEN, k="TIME", &col_t, &istat) ||
fits_get_colnum(fp, CASESEN, k="ACTY", &col_acty, &istat) ||
fits_get_colnum(fp, CASESEN, k="DETX", &col_detx, &istat) ||
fits_get_colnum(fp, CASESEN, k="DETY", &col_dety, &istat) ||
fits_get_colnum(fp, CASESEN, k="PI", &col_pi, &istat) ||
fits_get_colnum(fp, CASESEN, k="GRADE", &col_grade, &istat) ||
fits_get_colnum(fp, CASESEN, k="STATUS", &col_pixq, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	nrej_time = nrej_grade = nrej_pixq = nrej_detxy = nrej_pi = 0;
	last_aetime = 0.0;
	total_count = 0.0;
	for (irow = 1; irow <= nrows; irow++) {
		if (
fits_read_col_dbl(fp, ic=col_t, irow, 1, 1, 0.0, &aetime, &anul, &istat) ||
fits_read_col_int(fp, ic=col_acty, irow, 1, 1, 0, &acty, &anul, &istat) ||
fits_read_col_int(fp, ic=col_detx, irow, 1, 1, 0, &detx, &anul, &istat) ||
fits_read_col_int(fp, ic=col_dety, irow, 1, 1, 0, &dety, &anul, &istat) ||
fits_read_col_int(fp, ic=col_pi, irow, 1, 1, 0, &pi, &anul, &istat) ||
fits_read_col_int(fp, ic=col_grade, irow, 1, 1, 0, &grade, &anul, &istat) ||
fits_read_col_uint(fp, ic=col_pixq, irow, 1, 1, 0, &pixq, &anul, &istat) ||
			 0 ) {
			anl_msg_error("\
%s: fits_read_col(col=%d) failed at irow=%ld (%d)\n", pname, ic, irow, istat);
			goto quit;
		}
		if ( 1 == irow || last_aetime != aetime ) {
			istat = (*get_value)(aetime, com.nxb_orbit, com.nxb_hkid, &cor);
			last_aetime = aetime;
		}
		t0 = aetime - 0.5 * expinfo.winexp;
		t1 = aetime + 0.5 * expinfo.winexp;
		if ( t0 < com.t0 || com.t1 < t1 ) {
			nrej_time++;
			continue;
		}
		if ( XIS_GRADE_MAX < grade ) {
			anl_msg_warning("\
%s: WARNING: GRADE=%d at irow=%ld is rejected\n", pname, grade, irow);
			nrej_grade++;
			continue;
		}
		if ( 0 == (com.grademask & (1 << grade)) ) {
			nrej_grade++;
			continue;
		}
		if ( com.enable_pixq ) {
			if ( pixq < com.pixq_min || com.pixq_max < pixq ||
				(pixq & com.pixq_and) != com.pixq_eql ) {
				nrej_pixq++;
				continue;
			}
		}
		if ( detx < 1 || com.ndetx < detx || dety < 1 || com.ndety < dety ) {
			nrej_detxy++;
			continue;
		}
		if ( acty < obs->win_st || obs->win_st + obs->win_siz <= acty ) {
			nrej_detxy++;
			continue;
		}
		if ( pi < XIS_PI_MIN || XIS_PI_MAX < pi ) {
			nrej_pi++;
			continue;
		}

		total_count++;
		for (i = 0; i < nsort; i++) {
			if ( nxb->sp[i].cor_lo <= cor && cor < nxb->sp[i].cor_hi ) {
				ipos = com.ndetx * (dety - 1) + (detx - 1);
				w = com.regmap[ipos];
				if ( 0.0 < w ) {
					w2 = w * w;
					fobs = obs->sp[i].expo / obs->total_expo;
					expo = nxb->sp[i].expo;
					nxb->sp[i].spec_count += w;
					nxb->sp[i].spec[pi - XIS_PI_MIN] += w;
					nxb->sp[i].serr[pi - XIS_PI_MIN] += w2;
					if ( 0.0 < expo ) {
						if ( nxb->wmap[ipos] < 0.0 ) {
							nxb->wmap[ipos] = 0.0;
						}
						nxb->wmap[ipos] += w * fobs / expo;
					} else {
						anl_msg_error("\
%s: GTI is not consistent with EVENTS for NXB database\n", pname);
						istat = -1;
						goto quit;
					}
				}
				if ( com.pi_min <= pi && pi <= com.pi_max ) {
					w = (NULL == com.detmaskmap) ? 1.0 : com.detmaskmap[ipos];
					nxb->sp[i].image_count += w;
					nxb->sp[i].image[ipos] += w;
				}
			}
		}
	}

	sum_expo = sum_spec_count = sum_image_count = 0.0;
	anl_msg_info("\
   nrows=%ld  nrej_time=%ld  nrej_grade=%ld  nrej_pixq=%ld\n\
   nrej_detxy=%ld  nrej_pi=%ld\n",
		nrows, nrej_time, nrej_grade, nrej_pixq, nrej_detxy, nrej_pi);
	anl_msg_info("\
===================================================================\n\
%-14s:  EXPOSURE (s)  FRACTION (%%)  SPEC (cts) IMAGE (cts)\n\
-------------------------------------------------------------------\n",
		com.sortkey);
	for (i = 0; i < nsort; i++) {
		anl_msg_info("\
%5.1f - %5.1f : %12.1f  %12.3f %11.1f %11.1f\n",
			nxb->sp[i].cor_lo, nxb->sp[i].cor_hi,
			nxb->sp[i].expo, 100 * nxb->sp[i].expo / total_expo,
			nxb->sp[i].spec_count, nxb->sp[i].image_count);
		sum_expo += nxb->sp[i].expo;
		sum_spec_count += nxb->sp[i].spec_count;
		sum_image_count += nxb->sp[i].image_count;
	}
	anl_msg_info("\
-------------------------------------------------------------------\n\
 %12s : %12.1f  %12.3f %11.1f %11.1f\n", "SUM",
		sum_expo, 100 * sum_expo / total_expo,
		sum_spec_count, sum_image_count);
	anl_msg_info("\
 %12s : %12.1f  %12.3f %11.1f %11.1f\n\
-------------------------------------------------------------------\n",
		"TOTAL", total_expo, 100.0, total_count, total_count);

	if ( sum_expo < 200e3 ) {
		anl_msg_warning("\
%s: WARNING: *************************************************\n\
%s: WARNING:  Exposure time is not sufficient.\n\
%s: WARNING:  Check the 'time_min' and 'time_max' parameters,\n\
%s: WARNING:  and/or CALDB is the lastest.\n\
%s: WARNING: *************************************************\n",
			pname, pname, pname, pname, pname);
	}

	nxb->expinfo = expinfo;
	nxb->nrows = nrows;
	nxb->nrej_time = nrej_time;
	nxb->nrej_grade = nrej_grade;
	nxb->nrej_pixq = nrej_pixq;
	nxb->nrej_detxy = nrej_detxy;
	nxb->nrej_pi = nrej_pi;
	nxb->aetime_start = aetime_start;
	nxb->aetime_stop = aetime_stop;
	nxb->aetime_average = aetime_average;
	nxb->exposure_before_time_min = exposure_before_time_min;
	nxb->exposure_after_time_max = exposure_after_time_max;
	nxb->total_expo = total_expo;
	nxb->total_count = total_count;

	return 0;

 quit:
	return istat;
}

static int
calc_weighted_nxb(SORT_DATA *obs, SORT_DATA *nxb)
{
	int i, ipos, spec_siz, image_siz, nsort;
	double fobs, expo, weight, weight2;
	double wexpo, wspec_sum, wserr_sum, wimage_sum;
	double *wspec, *wserr, *wimage, *spec, *serr, *image;

	nsort = nxb->nsort;
	wspec = nxb->sp[nsort].spec;
	wserr = nxb->sp[nsort].serr;
	wimage = nxb->sp[nsort].image;
	spec_siz = nxb->spec_siz;
	image_siz = nxb->image_siz;

	for (i = 0; i < nsort; i++) {
		fobs = obs->sp[i].expo / obs->total_expo;
		expo = nxb->sp[i].expo;
		if ( 0.0 == fobs || 0.0 == expo ) {
			continue;
		}
		spec = nxb->sp[i].spec;
		serr = nxb->sp[i].serr;
		image = nxb->sp[i].image;
		weight = fobs / expo;
		weight2 = weight * weight;
		for (ipos = 0; ipos < spec_siz; ipos++) {
			wspec[ipos] += weight * spec[ipos];
			wserr[ipos] += weight2 * serr[ipos];
		}
		for (ipos = 0; ipos < image_siz; ipos++) {
			wimage[ipos] += weight * image[ipos];
		}
	}

	wspec_sum = wserr_sum = 0.0;
	for (ipos = 0; ipos < spec_siz; ipos++) {
		wspec_sum += wspec[ipos];
		wserr_sum += wserr[ipos];
		wserr[ipos] = sqrt(wserr[ipos]);
	}
	wexpo = wspec_sum / wserr_sum;
	wspec_sum *= wexpo;

	wimage_sum = 0.0;
	for (ipos = 0; ipos < image_siz; ipos++) {
		wimage[ipos] *= wexpo;
		if ( 0.0 < nxb->wmap[ipos] ) {
			nxb->wmap[ipos] *= wexpo;
		}
		wimage_sum += wimage[ipos];
	}

	nxb->sp[nsort].expo = wexpo;
	nxb->sp[nsort].spec_count = wspec_sum;
	nxb->sp[nsort].image_count = wimage_sum;

	anl_msg_info("\
 %12s : %12.1f  %12.3f %11.1f %11.1f\n\
-------------------------------------------------------------------\n",
		"EFFECTIVE", wexpo, 100 * wexpo / nxb->total_expo,
		wspec_sum, wimage_sum);

	return 0;
}

static char *
mkfname(char *name, char *o_name)
{
	static char buf[2*PIL_LINESIZE];
	if ( name == o_name ) {
		return name;
	}
	sprintf(buf, "'%s' (%s)", name, o_name);
	return buf;
}

static void
show_parameter(void)
{
	char datestr[80];

	printf("\nANL:  *** %s show parameter ***\n\n", pname);
	printf("%20s   '%s'\n", "OUTFILE", com.outfile);
	printf("%20s   '%s'\n", "PHAFILE", com.phafile);
	printf("%20s   '%s'\n", "REGION_MODE", com.region_mode);
	printf("%20s   '%s'\n", "REGFILE", com.regfile);
	printf("%20s   '%s'\n", "DETMASK", com.detmask);
	printf("%20s   %d\n", "PI_MIN", com.pi_min);
	printf("%20s   %d\n", "PI_MAX", com.pi_max);
	printf("%20s   '%s'\n", "ORBIT", com.orbit);
	printf("%20s   '%s'\n", "ATTITUDE", com.attitude);
	printf("%20s   '%s'\n", "EHKFILE", com.ehkfile);
	printf("%20s   '%s'\n", "GTIFILE", com.gtifile);
	printf("%20s   %s\n", "TELDEF", mkfname(com.teldeffile, com.o_teldeffile));
	printf("%20s   %s\n", "LEAPFILE", mkfname(com.leapfile, com.o_leapfile));
	printf("%20s   %s\n", "RIGIDITY", mkfname(com.rigidity, com.o_rigidity));
	printf("%20s   %s\n", "NXBEVENT", mkfname(com.nxbevent, com.o_nxbevent));
	printf("%20s   %s\n", "NXBORBIT", mkfname(com.nxborbit, com.o_nxborbit));
	printf("%20s   %s\n", "NXBVDCHK", mkfname(com.nxbvdchk, com.o_nxbvdchk));
	printf("%20s   %s\n", "NXBCORHK", mkfname(com.nxbcorhk, com.o_nxbcorhk));
	printf("%20s   '%s'\n", "SORTKEY", com.sortkey);
	printf("%20s   '%s'\n", "SORTSTEP", com.sortstep);
	printf("%20s   %.1f\n", "GTI_MIN_SEC", com.gti_min_sec);
	printf("%20s   %.1f\n", "EHK_MARGIN_SEC", com.ehk_margin_sec);
	printf("%20s   '%s' [%s]\n", "TIME_MIN",
		com.time_min, aefits_aetime2datestr(com.t0, datestr));
	printf("%20s   '%s' [%s]\n", "TIME_MAX",
		com.time_max, aefits_aetime2datestr(com.t1, datestr));
	printf("%20s   '%s' [mask=0x%04x]\n", "GRADES", com.grades, com.grademask);
	printf("%20s   %s\n", "APPLY_XISFTOOLS", com.apply_xisftools ? "YES":"NO");
	printf("%20s   %s\n", "ENABLE_PIXQ", com.enable_pixq ? "YES":"NO");
	if ( com.enable_pixq ) {
		printf("%20s   %u (0x%08x)\n", "PIXQ_MIN", com.pixq_min, com.pixq_min);
		printf("%20s   %u (0x%08x)\n", "PIXQ_MAX", com.pixq_max, com.pixq_max);
		printf("%20s   %u (0x%08x)\n", "PIXQ_AND", com.pixq_and, com.pixq_and);
		printf("%20s   %u (0x%08x)\n", "PIXQ_EQL", com.pixq_eql, com.pixq_eql);
	}
	printf("%20s   %s\n", "ABERRATION", com.aberration ? "YES":"NO");
	printf("%20s   %s\n", "CLOBBER", com.clobber ? "YES":"NO");
	printf("\n");
}

void
xisnxbgen_startup(int *status)
{
	com.teldeffile = com.o_teldeffile;
	com.leapfile = com.o_leapfile;
	com.rigidity = com.o_rigidity;
	com.nxbevent = com.o_nxbevent;
	com.nxborbit = com.o_nxborbit;
	com.nxbvdchk = com.o_nxbvdchk;
	com.nxbcorhk = com.o_nxbcorhk;

	*status = ANL_OK;
}

void
xisnxbgen_com(int *status)
{
	static char *keytbl[] = {
		"OUTFILE",
		"PHAFILE",
		"CLOBBER",
		"LEAPFILE",
		"SHOW",
		"EXIT"
	};
	static char *help[] = {
		"Output NXB file name",
		"Input filename",
		"Overwrite output file",
		"leap-seconds file name",
		"Show current setting",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);

	char *k, *key;
	int ans[2];

	unsigned int uint_min = 0;
	unsigned int uint_max = 4294967295u;
	double pixq_min = 0.0;
	double pixq_max = 524287.0;
	double pixq_and = 0.0;
	double pixq_eql = 0.0;

	if ( *status ) {			/* ftools */
		if (
			PILGetFname(k="outfile", com.outfile) ||
			PILGetFname(k="phafile", com.phafile) ||
			PILGetString(k="region_mode", com.region_mode) ||
			PILGetFname(k="regfile", com.regfile) ||
			PILGetFname(k="detmask", com.detmask) ||
			PILGetInt  (k="pi_min", &com.pi_min) ||
			PILGetInt  (k="pi_max", &com.pi_max) ||
			PILGetFname(k="orbit", com.orbit) ||
			PILGetFname(k="attitude", com.attitude) ||
			PILGetFname(k="ehkfile", com.ehkfile) ||
			PILGetFname(k="gtifile", com.gtifile) ||
			PILGetFname(k="teldef", com.o_teldeffile) ||
			PILGetFname(k="leapfile", com.o_leapfile) ||
			PILGetFname(k="rigidity", com.o_rigidity) ||
			PILGetFname(k="nxbevent", com.o_nxbevent) ||
			PILGetFname(k="nxborbit", com.o_nxborbit) ||
			PILGetFname(k="nxbvdchk", com.o_nxbvdchk) ||
			PILGetFname(k="nxbcorhk", com.o_nxbcorhk) ||
			PILGetString(k="sortkey", com.sortkey) ||
			PILGetString(k="sortstep", com.sortstep) ||
			PILGetReal (k="gti_min_sec", &com.gti_min_sec) ||
			PILGetReal (k="ehk_margin_sec", &com.ehk_margin_sec) ||
			PILGetString(k="time_min", com.time_min) ||
			PILGetString(k="time_max", com.time_max) ||
			PILGetString(k="grades", com.grades) ||
			PILGetBool (k="apply_xisftools", &com.apply_xisftools) ||
			PILGetBool (k="enable_pixq", &com.enable_pixq) ||
			PILGetReal (k="pixq_min", &pixq_min) ||
			PILGetReal (k="pixq_max", &pixq_max) ||
			PILGetReal (k="pixq_and", &pixq_and) ||
			PILGetReal (k="pixq_eql", &pixq_eql) ||
			PILGetBool (k="aberration", &com.aberration) ||
			PILGetBool (k="clobber", &com.clobber) ||
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

		*status = ANL_OK;
		return;

	pil_error:
		anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
		*status = ANL_QUIT;
		return;
	}

	for(;;) {
		CMinquir(pname, nkey, keytbl, help, 1, ans);
		key = keytbl[ans[1]-1];
		if ( 0 == strcmp("PHAFILE", key) ) {
			CLtxtrd(key, com.phafile, sizeof(com.phafile));
		} else if ( 0 == strcmp("OUTFILE", key) ) {
			CLtxtrd(key, com.outfile, sizeof(com.outfile));
		} else if ( 0 == strcmp("CLOBBER", key) ) {
			CLlogrd(key, &com.clobber);
		} else if ( 0 == strcmp("LEAPFILE", key) ) {
			CLtxtrd(key, com.o_leapfile, sizeof(com.o_leapfile));
		} else if (strcmp (key, "SHOW") == 0) {
			show_parameter();
		} else if (strcmp (key, "EXIT") == 0) {
			return;
		}
	}

	*status = ANL_OK;
}

int
apply_xisftools(void)
{
	static char tmpfile1[] = "tmp.xisnxbgen.XXXXXX";
	static char tmpfile2[] = "tmp.xisnxbgen.XXXXXX";
	static char tmpfile3[] = "tmp.xisnxbgen.XXXXXX";

	int fd, istat, length;
	char *command;

	com.nxbevent_or_tmpfile = com.nxbevent;

	if ( com.apply_xisftools ) {
		fd = mkstemp(tmpfile1);
		if ( fd < 0 ) {
			anl_msg_error("\
%s: mkstemp() failed (errno=%d)\n", pname, errno);
			return -1;
		}
		close(fd);

		length = 1024 + strlen(com.nxbevent) + strlen(com.nxbvdchk);
		command = malloc( length );
		if ( NULL == command ) {
			anl_msg_error("\
%s: command buffer malloc(length=%d) failed\n", pname, length);
			return -1;
		}
		sprintf(command, "\
xispi infile='%s' outfile='%s' hkfile='%s' mode=h",
				com.nxbevent_or_tmpfile, tmpfile1, com.nxbvdchk);
		anl_msg_info("\n\
%s: running \"%s\"\n", pname, command);
		istat = system(command);
		free(command);
		if ( istat ) goto quit;

		if ( com.nxbevent_or_tmpfile != com.nxbevent ) {
			unlink(com.nxbevent_or_tmpfile);
		}
		com.nxbevent_or_tmpfile = tmpfile1;
	}

	if ( com.apply_xisftools && com.enable_pixq ) {
		fd = mkstemp(tmpfile2);
		if ( fd < 0 ) {
			anl_msg_error("\
%s: mkstemp() failed (%d)\n", pname, errno);
			return -1;
		}
		close(fd);

		length = 1024 + strlen(com.nxbevent);
		command = malloc( length );
		if ( NULL == command ) {
			anl_msg_error("\
%s: command buffer malloc(length=%d) failed\n", pname, length);
			return -1;
		}

		sprintf(command, "\
xisputpixelquality infile='%s' outfile='%s' mode=h",
			com.nxbevent_or_tmpfile, tmpfile2);
		anl_msg_info("\n\
%s: running \"%s\"\n", pname, command);
		istat = system(command);
		free(command);
		if ( istat ) goto quit;

		if ( com.nxbevent_or_tmpfile != com.nxbevent ) {
			unlink(com.nxbevent_or_tmpfile);
		}
		com.nxbevent_or_tmpfile = tmpfile2;
	}

	if ( com.apply_xisftools ) {
		fd = mkstemp(tmpfile3);
		if ( fd < 0 ) {
			anl_msg_error("\
%s: mkstemp() failed (errno=%d)\n", pname, errno);
			return -1;
		}
		close(fd);

		length = 1024 + strlen(com.nxbevent);
		command = malloc( length );
		if ( NULL == command ) {
			anl_msg_error("\
%s: command buffer malloc(length=%d) failed\n", pname, length);
			return -1;
		}
		sprintf(command, "\
cleansis datafile='%s' outfile='%s' cellsize=5 logprob=-5.3 bthresh=3 \
phamin=%d phamax=%d chipcol=SEGMENT clobber=yes mode=h",
				com.nxbevent_or_tmpfile, tmpfile3, XIS_PI_MIN, XIS_PI_MAX);
		anl_msg_info("\n\
%s: running \"%s\"\n", pname, command);
		istat = system(command);
		free(command);
		if ( istat ) goto quit;

		if ( com.nxbevent_or_tmpfile != com.nxbevent ) {
			unlink(com.nxbevent_or_tmpfile);
		}
		com.nxbevent_or_tmpfile = tmpfile3;
	}

	return 0;

 quit:
	return istat;
}

static int
write_history(fitsfile *ofp, SORT_DATA *obs, SORT_DATA *nxb)
{
	char *k, *task_name, *task_version, creator[FLEN_VALUE], history[1024];
	char datestr[80];
	double sum_expo, sum_spec_count, sum_image_count;
	int i;

	int istat = 0;

	task_name = anl_task_name();
	task_version = anl_task_version();

	sprintf(creator, "%s version %s", task_name, task_version);
	if (
fits_update_key_str(ofp, k="CREATOR", creator,
	"software that created this file", &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}
	fits_write_date(ofp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_date() failed (%d)\n", pname, istat);
		return istat;
	}
	fits_write_chksum(ofp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
		return istat;
	}

	istat = aefits_write_name_vers(ofp, task_name, task_version);
	if ( istat ) {
		return istat;
	}

	sprintf(history, "\
  outfile='%s'", com.outfile);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  phafile='%s'", com.phafile);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  region_mode='%s'", com.region_mode);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  regfile='%s'", com.regfile);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  detmask='%s'", com.detmask);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  pi_min=%d  pi_max=%d", com.pi_min, com.pi_max);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  orbit='%s'", com.orbit);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  attitude='%s'", com.attitude);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  ehkfile='%s'", com.ehkfile);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  gtifile='%s'", com.gtifile);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  teldef=%s", mkfname(com.teldeffile, com.o_teldeffile));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  leapfile=%s", mkfname(com.leapfile, com.o_leapfile));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  rigidity=%s", mkfname(com.rigidity, com.o_rigidity));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  nxbevent=%s", mkfname(com.nxbevent, com.o_nxbevent));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  nxborbit=%s", mkfname(com.nxborbit, com.o_nxborbit));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  nxbvdchk=%s", mkfname(com.nxbvdchk, com.o_nxbvdchk));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  nxbcorhk=%s", mkfname(com.nxbcorhk, com.o_nxbcorhk));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  sortkey='%s'  sortstep='%s'", com.sortkey, com.sortstep);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  gti_min_sec=%.3f  ehk_margin_sec=%.3f", com.gti_min_sec, com.ehk_margin_sec);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  time_min='%s' [%s]", com.time_min, aefits_aetime2datestr(com.t0, datestr));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  time_max='%s' [%s]", com.time_max, aefits_aetime2datestr(com.t1, datestr));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  grades='%s' [mask=0x%04x]", com.grades, com.grademask);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
  apply_xisftools=%s  enable_pixq=%s",
		com.apply_xisftools ? "yes":"no",
		com.enable_pixq ? "yes":"no");
	fits_write_history(ofp, history, &istat);
	if ( com.enable_pixq ) {
		sprintf(history, "\
  pixq_min=%u (0x%08x)  pixq_max=%u (0x%08x)",
			com.pixq_min, com.pixq_min, com.pixq_max, com.pixq_max);
		fits_write_history(ofp, history, &istat);
		sprintf(history, "\
  pixq_and=%u (0x%08x)  pixq_eql=%u (0x%08x)",
			com.pixq_and, com.pixq_and, com.pixq_eql, com.pixq_eql);
		fits_write_history(ofp, history, &istat);
	}
	sprintf(history, "\
  abberation=%s  clobber=%s",
		com.aberration ? "yes":"no",
		com.clobber ? "yes":"no");
	fits_write_history(ofp, history, &istat);

	fits_write_history(ofp, " ", &istat);
	sprintf(history, "\
OBS GTI: ngti=%ld  winexp=%.1f", obs->expinfo.nrows, obs->expinfo.winexp);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
Target observation start : %.1f [%s]",
		obs->aetime_start, aefits_aetime2datestr(obs->aetime_start, datestr));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
Average observation at   : %.1f [%s]", obs->aetime_average,
		aefits_aetime2datestr(obs->aetime_average, datestr));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
Target observation stop  : %.1f [%s]",
		obs->aetime_stop, aefits_aetime2datestr(obs->aetime_stop, datestr));
	fits_write_history(ofp, history, &istat);
	sum_expo = 0.0;
	sprintf(history, "\
===========================================");
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
%-14s:  EXPOSURE (s)  FRACTION (%%)", com.sortkey);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
-------------------------------------------");
	fits_write_history(ofp, history, &istat);
	for (i = 0; i < obs->nsort; i++) {
		sprintf(history, "\
%5.1f - %5.1f : %12.1f  %12.3f",
			obs->sp[i].cor_lo, obs->sp[i].cor_hi,
			obs->sp[i].expo, 100 * obs->sp[i].expo / obs->total_expo);
		fits_write_history(ofp, history, &istat);
		sum_expo += obs->sp[i].expo;
	}
	sprintf(history, "\
-------------------------------------------");
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
 %12s : %12.1f  %12.3f", "SUM", sum_expo, 100 * sum_expo / obs->total_expo);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
 %12s : %12.1f  %12.3f", "TOTAL", obs->total_expo, 100.0);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
-------------------------------------------");
	fits_write_history(ofp, history, &istat);

	fits_write_history(ofp, " ", &istat);
	sprintf(history, "\
NXB GTI: ngti=%ld  winexp=%.1f", nxb->expinfo.nrows, nxb->expinfo.winexp);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
exposure_before_time_min=%.1f  exposure_after_time_max=%.1f",
		nxb->exposure_before_time_min, nxb->exposure_after_time_max);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
NXB accumulation start : %.1f [%s]", nxb->aetime_start,
		aefits_aetime2datestr(nxb->aetime_start, datestr));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
Average time at        : %.1f [%s]", nxb->aetime_average,
		aefits_aetime2datestr(nxb->aetime_average, datestr));
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
NXB accumulation stop  : %.1f [%s]", nxb->aetime_stop,
		aefits_aetime2datestr(nxb->aetime_stop, datestr));
	fits_write_history(ofp, history, &istat);
	sum_expo = sum_spec_count = sum_image_count = 0.0;
	sprintf(history, "\
nrows=%ld  nrej_time=%ld  nrej_grade=%ld  nrej_pixq=%ld",
		nxb->nrows, nxb->nrej_time, nxb->nrej_grade, nxb->nrej_pixq);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
nrej_detxy=%ld  nrej_pi=%ld", nxb->nrej_detxy, nxb->nrej_pi);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
===================================================================");
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
%-14s:  EXPOSURE (s)  FRACTION (%%)  SPEC (cts) IMAGE (cts)", com.sortkey);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
-------------------------------------------------------------------");
	fits_write_history(ofp, history, &istat);
	for (i = 0; i < nxb->nsort; i++) {
		sprintf(history, "\
%5.1f - %5.1f : %12.1f  %12.3f %11.1f %11.1f",
			nxb->sp[i].cor_lo, nxb->sp[i].cor_hi,
			nxb->sp[i].expo, 100 * nxb->sp[i].expo / nxb->total_expo,
			nxb->sp[i].spec_count, nxb->sp[i].image_count);
		fits_write_history(ofp, history, &istat);
		sum_expo += nxb->sp[i].expo;
		sum_spec_count += nxb->sp[i].spec_count;
		sum_image_count += nxb->sp[i].image_count;
	}
	sprintf(history, "\
-------------------------------------------------------------------");
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
 %12s : %12.1f  %12.3f %11.1f %11.1f", "SUM",
		sum_expo, 100 * sum_expo / nxb->total_expo,
		sum_spec_count, sum_image_count);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
 %12s : %12.1f  %12.3f %11.1f %11.1f",
		"TOTAL", nxb->total_expo, 100.0, nxb->total_count, nxb->total_count);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
-------------------------------------------------------------------");
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
 %12s : %12.1f  %12.3f %11.1f %11.1f",
		"EFFECTIVE", nxb->sp[i].expo, 100 * nxb->sp[i].expo / nxb->total_expo,
		nxb->sp[i].spec_count, nxb->sp[i].image_count);
	fits_write_history(ofp, history, &istat);
	sprintf(history, "\
-------------------------------------------------------------------");
	fits_write_history(ofp, history, &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
		return istat;
	}

	return istat;
}

static int
copy_keys(fitsfile *ofp, fitsfile *nxb_fp, SORT_DATA *obs, SORT_DATA *nxb)
{
	static char skip_to_key[] = "TELESCOP";
	static char *ignore_keys[] = {
		"BITPIX", "NAXIS*", "PCOUNT", "GCOUNT", "TFIELDS", "THEAP",
		"TBCOL*", "TTYPE*", "TFORM*", "TUNIT*", "TSCAL*", "TZERO*",
		"TDISP*", "TDIM*", "TNULL*",
		"TLMIN*", "TLMAX*", "TDMIN*", "TDMAX*",
		"TCDLT*", "TCRPX*", "TCRVL*", "TCTYP*", "TCUNI*", "TCROT*",
		"CRPIX*", "CRVAL*", "CDELT*", "CROTA*", "PC*", "CD*", "PV*",
		"CRDER*", "CSYER*", "CTYPE*", "CUNIT*", "PS*",
		"BZERO", "DATAMAX", "DATAMIN", "LONPOLE", "LATPOLE",
		"BUNIT", "WCSNAME",
		"CHECKSUM", "DATASUM", "NPIXSOU", NULL
	};
	static char *accept_keys[] = {
		"COMMENT", "HISTORY", NULL
	};

	int i, len, keysexist, keynum;
	char *k, keyname[FLEN_KEYWORD], card[FLEN_CARD], card2[FLEN_CARD];
	double tstart, tstop, telapse, ontime;

	int istat = 0;

	fits_read_card(nxb_fp, skip_to_key, card, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_card('%s') failed (%d)\n", pname, skip_to_key, istat);
		goto quit;
	}
	fits_get_hdrpos(nxb_fp, &keysexist, &keynum, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_hdrpos() failed (%d)\n", pname, istat);
		goto quit;
	}

	for (keynum = keynum - 1; keynum <= keysexist; keynum++) {
		fits_read_record(nxb_fp, keynum, card, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_record() failed at keynum=%d (%d)\n", pname, keynum, istat);
			goto quit;
		}
		strncpy(keyname, card, 8);
		keyname[8] = '\0';
		for (i = 7; 0 <= i; i--) {
			if ( ' ' == keyname[i] ) {
				keyname[i] = '\0';
			}
		}
		for (i = 0; NULL != ignore_keys[i]; i++) {
			if ( 0 == strcmp(keyname, ignore_keys[i]) ) {
				break;
			}
			len = strlen(ignore_keys[i]);
			if ( 0 < len && '*' == ignore_keys[i][len-1] ) {
				if ( 0 == strncmp(keyname, ignore_keys[i], len-1) ) {
					break;
				}
			}
		}
		if ( NULL != ignore_keys[i] ) {
			continue;
		}
		for (i = 0; NULL != accept_keys[i]; i++) {
			if ( 0 == strcmp(keyname, accept_keys[i]) ) {
				break;
			}
		}
		if ( NULL == accept_keys[i] ) {
			fits_read_card(ofp, keyname, card2, &istat);
			if ( 0 == istat ) {
				continue;
			} else if ( KEY_NO_EXIST == istat ) {
				istat = 0;
			} else {
				anl_msg_error("\
%s: fits_read_card('%s') failed (%d)\n", pname, keyname, istat);
				goto quit;
			}
		}
		fits_write_record(ofp, card, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_card('%s') failed (%d)\n", pname, keyname, istat);
			goto quit;
		}
	}

	tstart = nxb->aetime_start;
	tstop = nxb->aetime_stop;
	telapse = tstop - tstart;
	ontime = nxb->sp[nxb->nsort].expo;
	if (
fits_update_key_fixdbl(ofp, k="TSTART", tstart, 6, NULL, &istat) ||
fits_update_key_fixdbl(ofp, k="TSTOP", tstop, 6, NULL, &istat) ||
fits_update_key_fixdbl(ofp, k="TELAPSE", telapse, 6, NULL, &istat) ||
fits_update_key_fixdbl(ofp, k="ONTIME", ontime, 6, NULL, &istat) ||
fits_update_key_fixdbl(ofp, k="EXPOSURE", ontime, 6, NULL, &istat) ||
fits_update_key_fixdbl(ofp, k="LIVETIME", ontime, 6, NULL, &istat) ||
fits_update_key_lng(ofp, k="WIN_ST", obs->win_st, NULL, &istat) ||
fits_update_key_lng(ofp, k="WIN_SIZ", obs->win_siz, NULL, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_update_key('%s') failed (%d)\n", pname, k, istat);
	}

	istat = write_history(ofp, obs, nxb);
	if ( istat ) goto quit;

	return 0;

 quit:
	return istat;
}

static int
write_wmap(fitsfile *ofp, fitsfile *nxb_fp, SORT_DATA *obs, SORT_DATA *nxb)
{
	static struct {
		char *key, *value, *comment;
	} *p, keys[] = {
 { "HDUCLASS",	"OGIP",		"format conforms to OGIP standard" },
 { "HDUCLAS1",	"IMAGE",	"Extension contains an image" },
 { "HDUVERS1",	"1.0.0",	"Version of family of formats" },
 { "HDUCLAS2",	"WMAP",		"Extension contains a weighted map" },
 { "HDUVERS2",	"1.0.0",	"Version of format" },
 { "WCSNAMEP",	"PHYSICAL",	"" },
 { "WCSTY1P",	"PHYSICAL",	"" },
 { "WCSTY2P",	"PHYSICAL",	"" },
 { "CTYPE1P",	"DETX",		"Source of X-axis" },
 { "CTYPE2P",	"DETY",		"Source of Y-axis" },
};
	int istat = 0;
	int naxis = 2;
	long naxes[2];
	char *k;
	int ikey, nkey = sizeof(keys) / sizeof(*keys);

	naxes[0] = nxb->det_xsiz;
	naxes[1] = nxb->det_ysiz;

fits_create_img(ofp, FLOAT_IMG, naxis, naxes, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_img() failed (%d)\n", pname, istat);
		goto quit;
	}

	for (ikey = 0; ikey < nkey; ikey++) {
		p = &keys[ikey];
fits_write_key_str(ofp, k=p->key, p->value, p->comment, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
			goto quit;
		}
	}

	if (
fits_write_key_lng(ofp, k="CRPIX1P", 1,
	"X axis ref pixel of physical coord", &istat) ||
fits_write_key_lng(ofp, k="CRVAL1P", 1,
	"physical coord of X ref pixel", &istat) ||
fits_write_key_lng(ofp, k="CDELT1P", 1,
	"X axis increment of physical coord", &istat) ||
fits_write_key_lng(ofp, k="CRPIX2P", 1,
	"Y axis ref pixel of physical coord", &istat) ||
fits_write_key_lng(ofp, k="CRVAL2P", 1,
	"physical coord of Y ref pixel", &istat) ||
fits_write_key_lng(ofp, k="CDELT2P", 1,
	"Y axis increment of physical coord", &istat) ||
fits_write_key_str(ofp, k="MTYPE1", "DET",
	"DM Keyword: Descriptor name", &istat) ||
fits_write_key_str(ofp, k="MFORM1", "DETX,DETY",
	"DM Keyword: Descriptor value", &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	istat = copy_keys(ofp, nxb_fp, obs, nxb);
	if ( istat ) goto quit;

fits_write_img_dbl(ofp, 1, 1, naxes[0] * naxes[1], nxb->wmap, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_img() failed (%d)\n", pname, istat);
		goto quit;
	}

	fits_write_chksum(ofp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
		goto quit;
	}

	return istat;

 quit:
	return istat;
}

static int
write_spec(fitsfile *ofp, fitsfile *nxb_fp, SORT_DATA *obs, SORT_DATA *nxb)
{
	static char extname[] = "SPECTRUM";
	static char *ttype[4] = { "CHANNEL",  "RATE",  "STAT_ERR", "QUALITY" };
	static char *tform[4] = { "1I", "1E", "1E", "1I" };
	static char *tunit[4] = { "",   "count/s", "count/s", "" };
	static char *tcomm[4] = {
		"Pulse Invarient (PI) Channel",
		"Counts per second per channel",
		"Statistical error on RATE",
		"Quality flag of this channel (0=good)"
	};

	static struct {
		char *key, *value, *comment;
	} *p, keys[] = {
 { "HDUCLASS",	"OGIP",		"format conforms to OGIP standard" },
 { "HDUCLAS1",	"SPECTRUM",	"PHA dataset (OGIP memo OGIP-92-007)" },
 { "HDUVERS1",	"1.2.0",	"Obsolete - included for backwards compatibility"},
 { "HDUVERS",	"1.2.0",	"Version of format (OGIP memo OGIP-92-007)" },
 { "HDUCLAS2",	"BKG",		"Background PHA spectrum" },
 { "HDUCLAS3",	"RATE",		"PHA data stored in count/s" },
 { "TLMIN1",	"_l:0",		"Lowest legal channel number" },
 { "TLMAX1",	"_l:4095",	"Highest legal channel number" },
 { "AREASCAL",	"_l:1",		"area scaling factor" },
 { "BACKFILE",	"none",		"associated background filename" },
 { "BACKSCAL",	"_l:1",		"background file scaling factor" },
 { "CORRFILE",	"none",		"associated correction filename" },
 { "CORRSCAL",	"_l:1",		"correction file scaling factor" },
 { "RESPFILE",	"none",		"associated redistrib matrix filename" },
 { "ANCRFILE",	"none",		"associated ancillary response filename" },
 { "PHAVERSN",	"1992a",	"obsolete" },
 { "DETCHANS",	"_l:4096",	"total number possible channels" },
 { "CHANTYPE",	"PI",		"channel type (PHA, PI etc)" },
 { "POISSERR",	"_b:F",		"Poissonian errors not applicable" },
 { "SYS_ERR",	"_l:0",		"no systematic error specified" },
 { "GROUPING",	"_l:0",		"no grouping of the data has been defined" },
};

	short chan, quality;
	int ic, ikey, irow, bval;
	long lval;
	char *k, keyname[FLEN_KEYWORD];
	double *spec, *serr;

	int nc = sizeof(ttype) / sizeof(*ttype);
	int nkey = sizeof(keys) / sizeof(*keys);
	int istat = 0;

/* construct binary table header */
fits_create_tbl(ofp, BINARY_TBL, 0, nc, ttype, tform, tunit, extname, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_tbl(tmpfil) failed (%d)\n", pname, istat);
		goto quit;
	}
	for (ic = 0; ic < nc; ic++) {
		sprintf(keyname, "TTYPE%d", ic+1);
		fits_modify_comment(ofp, k=keyname, tcomm[ic], &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_modify_comment('%s') failed (%d)\n", pname, k, istat);
			goto quit;
		}
	}

	for (ikey = 0; ikey < nkey; ikey++) {
		p = &keys[ikey];
		if ( '_' == *p->value ) {
			switch ( p->value[1] ) {
			case 'b':	/* bool */
				bval = ( 'T' == p->value[3] ) ? TRUE : FALSE;
fits_write_key_log(ofp, k=p->key, bval, p->comment, &istat);
				break;
			case 'l':	/* long integer */
				lval = atol(p->value + 3);
fits_write_key_lng(ofp, k=p->key, lval, p->comment, &istat);
				break;
			default:
				;
			}
		} else {
fits_write_key_str(ofp, k=p->key, p->value, p->comment, &istat);
		}
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
			goto quit;
		}
	}

	if (
fits_update_key_lng(ofp, k="TLMIN1", XIS_PI_MIN, NULL, &istat) ||
fits_update_key_lng(ofp, k="TLMAX1", XIS_PI_MAX, NULL, &istat) ||
fits_update_key_lng(ofp, k="DETCHANS", XIS_PI_NCH, NULL, &istat) ||
fits_update_card(ofp, k="AREASCAL", obs->areascal, &istat) ||
fits_update_card(ofp, k="BACKSCAL", obs->backscal, &istat) ||
fits_update_card(ofp, k="CORRSCAL", obs->corrscal, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_update_key('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	istat = copy_keys(ofp, nxb_fp, obs, nxb);
	if ( istat ) goto quit;

	quality = 0;
	spec = nxb->sp[nxb->nsort].spec;
	serr = nxb->sp[nxb->nsort].serr;
	for (irow = 1; irow <= XIS_PI_NCH; irow++) {
		ic = 0;
		chan = XIS_PI_MIN + irow - 1;
		if (
fits_write_col_sht(ofp, ++ic, irow, 1, 1, &chan, &istat) ||
fits_write_col_dbl(ofp, ++ic, irow, 1, 1, &spec[chan-XIS_PI_MIN], &istat) ||
fits_write_col_dbl(ofp, ++ic, irow, 1, 1, &serr[chan-XIS_PI_MIN], &istat) ||
fits_write_col_sht(ofp, ++ic, irow, 1, 1, &quality, &istat) ||
			 0 ) {
			anl_msg_error("\
%s: fits_write_col('%s') failed at irow=%d (%d)\n",
				pname, ttype[ic-1], irow, istat);
			goto quit;
		}
	}

	fits_write_chksum(ofp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
		goto quit;
	}

	return istat;

 quit:
	return istat;
}

static int
write_detimage(fitsfile *ofp, fitsfile *nxb_fp, SORT_DATA *obs, SORT_DATA *nxb)
{
	static struct {
		char *key, *value, *comment;
	} *p, keys[] = {
 { "EXTNAME",	"IMAGE_DET","name of this binary table extension" },
 { "HDUCLASS",	"OGIP",		"format conforms to OGIP standard" },
 { "HDUCLAS1",	"IMAGE",	"Extension contains an image" },
 { "HDUCLAS2",	"BKG",		"Extension contains a background image" },
 { "WCSNAMEP",	"PHYSICAL",	"" },
 { "WCSTY1P",	"PHYSICAL",	"" },
 { "WCSTY2P",	"PHYSICAL",	"" },
 { "CTYPE1P",	"DETX",		"Source of X-axis" },
 { "CTYPE2P",	"DETY",		"Source of Y-axis" },
 { "CHANMIN",	"",			"Low PI channel" },
 { "CHANMAX",	"",			"High PI channel" },
 { "CHANTYPE",	"PI",		"Energy channel name" },
};

	long naxes[2];
	char *k;
	double *image;

	int istat = 0;
	int naxis = 2;
	int ikey, nkey = sizeof(keys) / sizeof(*keys);

	naxes[0] = nxb->det_xsiz;
	naxes[1] = nxb->det_ysiz;

fits_create_img(ofp, FLOAT_IMG, naxis, naxes, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_img() failed (%d)\n", pname, istat);
		goto quit;
	}

	for (ikey = 0; ikey < nkey; ikey++) {
		p = &keys[ikey];
fits_write_key_str(ofp, k=p->key, p->value, p->comment, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
			goto quit;
		}
	}

	if (
fits_write_key_lng(ofp, k="CRPIX1P", 1,
	"X axis ref pixel of physical coord", &istat) ||
fits_write_key_lng(ofp, k="CRVAL1P", 1,
	"physical coord of X ref pixel", &istat) ||
fits_write_key_lng(ofp, k="CDELT1P", 1,
	"X axis increment of physical coord", &istat) ||
fits_write_key_lng(ofp, k="CRPIX2P", 1,
	"Y axis ref pixel of physical coord", &istat) ||
fits_write_key_lng(ofp, k="CRVAL2P", 1,
	"physical coord of Y ref pixel", &istat) ||
fits_write_key_lng(ofp, k="CDELT2P", 1,
	"Y axis increment of physical coord", &istat) ||
fits_write_key_str(ofp, k="MTYPE1", "DET",
	"DM Keyword: Descriptor name", &istat) ||
fits_write_key_str(ofp, k="MFORM1", "DETX,DETY",
	"DM Keyword: Descriptor value", &istat) ||
fits_update_key_lng(ofp, k="CHANMIN", com.pi_min, NULL, &istat) ||
fits_update_key_lng(ofp, k="CHANMAX", com.pi_max, NULL, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	istat = copy_keys(ofp, nxb_fp, obs, nxb);
	if ( istat ) goto quit;

	image = nxb->sp[nxb->nsort].image;
fits_write_img_dbl(ofp, 1, 1, naxes[0] * naxes[1], image, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_img() failed (%d)\n", pname, istat);
		goto quit;
	}

	fits_write_chksum(ofp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
		goto quit;
	}

	return istat;

 quit:
	return istat;
}

static int
write_skyimage(fitsfile *ofp, fitsfile *nxb_fp,
			   TELDEF *teldef, SORT_DATA *obs, SORT_DATA *nxb)
{
	static struct {
		char *key, *value, *comment;
	} *p, keys[] = {
 { "EXTNAME",	"IMAGE_SKY","name of this binary table extension" },
 { "HDUCLASS",	"OGIP",		"format conforms to OGIP standard" },
 { "HDUCLAS1",	"IMAGE",	"Extension contains an image" },
 { "HDUCLAS2",	"BKG",		"Extension contains a background image" },
 { "WCSNAMEP",	"PHYSICAL",	"" },
 { "WCSTY1P",	"PHYSICAL",	"" },
 { "WCSTY2P",	"PHYSICAL",	"" },
 { "CTYPE1P",	"X",		"Source of X-axis" },
 { "CTYPE2P",	"Y",		"Source of Y-axis" },
 { "CHANMIN",	"",			"Low PI channel" },
 { "CHANMAX",	"",			"High PI channel" },
 { "CHANTYPE",	"PI",		"Energy channel name" },
};

	long naxes[2];
	char *k;
	double *image;

	int istat = 0;
	int naxis = 2;
	int ikey, nkey = sizeof(keys) / sizeof(*keys);
	TELDEF_ASTROE *aste = teldef->mission.aste;
	double cdelt1 = - (aste->sky.xscl / aste->focallen) * RAD2DEG;
	double cdelt2 = + (aste->sky.yscl / aste->focallen) * RAD2DEG;

	naxes[0] = nxb->sky_xsiz;
	naxes[1] = nxb->sky_ysiz;

fits_create_img(ofp, FLOAT_IMG, naxis, naxes, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_img() failed (%d)\n", pname, istat);
		goto quit;
	}

	for (ikey = 0; ikey < nkey; ikey++) {
		p = &keys[ikey];
fits_write_key_str(ofp, k=p->key, p->value, p->comment, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
			goto quit;
		}
	}

	if (
fits_write_key_lng(ofp, k="CRPIX1P", 1,
	"X axis ref pixel of physical coord", &istat) ||
fits_write_key_lng(ofp, k="CRVAL1P", 1,
	"physical coord of X ref pixel", &istat) ||
fits_write_key_lng(ofp, k="CDELT1P", 1,
	"X axis increment of physical coord", &istat) ||
fits_write_key_lng(ofp, k="CRPIX2P", 1,
	"Y axis ref pixel of physical coord", &istat) ||
fits_write_key_lng(ofp, k="CRVAL2P", 1,
	"physical coord of Y ref pixel", &istat) ||
fits_write_key_lng(ofp, k="CDELT2P", 1,
	"Y axis increment of physical coord", &istat) ||
fits_write_key_str(ofp, "RADECSYS", "FK5",
	"celestial coord system", &istat) ||
fits_write_key_fixdbl(ofp, "EQUINOX", 2000.0, 1,
	"Equinox of celestial coord system", &istat) ||
fits_write_key_str(ofp, "MTYPE1", "SKY",
	"DM Keyword: Descriptor name", &istat) ||
fits_write_key_str(ofp, "MFORM1", "X,Y",
	"DM Keyword: Descriptor value", &istat) ||
fits_write_key_str(ofp, "CTYPE1", "RA---TAN",
	"X coordinate projection", &istat) ||
fits_write_key_fixdbl(ofp, "CRPIX1", aste->sky.xcen, 1,
	"X reference pixel", &istat) ||
fits_write_key_fixdbl(ofp, "CRVAL1", obs->skyref.alpha, 5,
	"X reference pixel value (deg)", &istat) ||
fits_write_key_fixdbl(ofp, "CDELT1", cdelt1, 7,
	"X pixel scale (deg/pixel)", &istat) ||
fits_write_key_str(ofp, "CTYPE2", "DEC--TAN",
	"Y coordinate projection", &istat) ||
fits_write_key_fixdbl(ofp, "CRPIX2", aste->sky.ycen, 1,
	"Y reference pixel", &istat) ||
fits_write_key_fixdbl(ofp, "CRVAL2", obs->skyref.delta, 5,
	"Y reference pixel value (deg)", &istat) ||
fits_write_key_fixdbl(ofp, "CDELT2", cdelt2, 7,
	"Y pixel scale (deg/pixel)", &istat) ||
fits_write_key_fixdbl(ofp, "CROTA2", obs->skyref.roll, 5,
	"Sky coord rotation angle (deg)", &istat) ||
fits_update_key_lng(ofp, k="CHANMIN", com.pi_min, NULL, &istat) ||
fits_update_key_lng(ofp, k="CHANMAX", com.pi_max, NULL, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	istat = copy_keys(ofp, nxb_fp, obs, nxb);
	if ( istat ) goto quit;

	image = nxb->skyimage;
fits_write_img_dbl(ofp, 1, 1, naxes[0] * naxes[1], image, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_img() failed (%d)\n", pname, istat);
		goto quit;
	}

	fits_write_chksum(ofp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
		goto quit;
	}

	return istat;

 quit:
	return istat;
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
xisnxbgen_init(int *status)
{
	int verbose, hdutype;
	int ipos, det_xsiz, det_ysiz, sky_xsiz, sky_ysiz;
	float *sky_regmap;
	double tstart, tstop, ra_nom, dec_nom;
	char *k, *nxbevtkey, telescop[FLEN_VALUE], instrume[FLEN_VALUE];
	TELDEF *teldef;
	TELDEF_ASTROE *aste;
	SORT_DATA obs, nxb;

	fitsfile *ifp = NULL;
	fitsfile *ofp = NULL;
	fitsfile *gti_fp = NULL;
	fitsfile *nxb_fp = NULL;
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
fits_read_key(ifp, TINT, k, &obs.win_st, NULL, &istat) ||
find_key(ifp, k="WIN_SIZ") ||
fits_read_key(ifp, TINT, k, &obs.win_siz, NULL, &istat) ||
find_key(ifp, k="CI") ||
fits_read_key(ifp, TINT, k, &obs.ci, NULL, &istat) ||
fits_read_key(ifp, TINT, k="MJDREFI", &obs.mjdrefi, NULL, &istat) ||
fits_read_key_dbl(ifp, k="MJDREFF", &obs.mjdreff, NULL, &istat) ||
		0 ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}
	strcpy(com.instrume_phafile, instrume);
	com.tstart_phafile = tstart;
	com.tstop_phafile  = tstop;

/* move to 1st extension */
	fits_movabs_hdu(ifp, 2, &hdutype, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: move to 1st extension failed (%d)\n", pname, istat);
		goto quit;
	}
	if (
find_key(ifp, k="AREASCAL") ||
fits_read_card(ifp, k, obs.areascal, &istat) ||
find_key(ifp, k="BACKSCAL") ||
fits_read_card(ifp, k, obs.backscal, &istat) ||
find_key(ifp, k="CORRSCAL") ||
fits_read_card(ifp, k, obs.corrscal, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: WARNING: fits_read_card('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	obs.skyref.alpha = ra_nom;
	obs.skyref.delta = dec_nom;
	obs.skyref.roll = 0.0;

/* check CI */
	switch ( obs.ci ) {
	case 0:	/* no CI */
		anl_msg_info("\
%s: header keyword CI=%d found, using SCI-off NXB database\n", pname, obs.ci);
		nxbevtkey = "NXB_SCIOF_EVT";
		break;
	case 1:	/* diagnostic CI */
		anl_msg_error("\
%s: CI=%d (diagnostic CI) is not supported\n", pname, obs.ci);
		goto quit;
	case 2:	/* SCI-54rows */
		anl_msg_info("\
%s: header keyword CI=%d found, using SCI-on NXB database\n", pname, obs.ci);
		nxbevtkey = "NXB_SCION_EVT";
		break;
	case 3:	/* SCI-108rows */
		anl_msg_error("\
%s: CI=%d (SCI-108rows) is not supported\n", pname, obs.ci);
		goto quit;
	default:
		anl_msg_error("\
%s: CI=%d (unknown mode) is not supported\n", pname, obs.ci);
		goto quit;
	}

/* find CALDB files */
	com.teldeffile = aste_caldb_find(instrume, "TELDEF", com.o_teldeffile);
	if ( NULL == com.teldeffile ) goto quit;
	com.leapfile = aste_caldb_find_leapfile(com.o_leapfile);
	if ( NULL == com.leapfile ) goto quit;
	com.rigidity = aste_caldb_find_rigidity(com.o_rigidity);
	if ( NULL == com.rigidity ) goto quit;
	com.nxbevent = aste_caldb_find(instrume, nxbevtkey, com.o_nxbevent);
	if ( NULL == com.nxbevent ) goto quit;
	com.nxborbit = aste_caldb_find("XIS", "NXB_ORBIT", com.o_nxborbit);
	if ( NULL == com.nxborbit ) goto quit;
	com.nxbvdchk = aste_caldb_find(instrume, "NXB_VDC_HK", com.o_nxbvdchk);
	if ( NULL == com.nxbvdchk ) goto quit;
	com.nxbcorhk = aste_caldb_find("XIS", "NXB_COR_HK", com.o_nxbcorhk);
	if ( NULL == com.nxbcorhk ) goto quit;

/* initialize aste_time */
	verbose = ( 0 == CLstricmp("none", com.leapfile) ) ? -1 : -2;
	if ( NULL == atMissionTimeInit(com.leapfile, verbose) ) {
		anl_msg_error("\
%s: atMissionTimeInit('%s') failed\n", pname, com.leapfile);
		goto quit;
	}
	if ( -1 == verbose ) printf("\n");


/* parse time_min & time_max */
	if ( parse_time_min_max("time_min", com.time_min, tstart, &com.t0) ||
		 parse_time_min_max("time_max", com.time_max, tstop, &com.t1) ||
		 0 ) {
		goto quit;
	}

/* parse grades */
	if ( parse_grades(com.grades, &com.grademask) ) {
		goto quit;
	}

/* show parameter */
	show_parameter();

/* show XIS0 anomaly warning if needed */
	if ( show_xis0_anomaly_warning() ) {
		anl_msg_warning("\n");
	}

/* initialize rigidity for COR2 */
	istat = atRigSet2(&com.rdp, com.rigidity);
	if ( istat ) {
		anl_msg_error("\
%s: atRigSet2('%s') failed (%d)\n", pname, com.rigidity, istat);
		goto quit;
	}

/* initialize aste_coord */
	teldef = aste_coord_init(NULL, instrume, com.teldeffile);
	if ( NULL == teldef ) {
		anl_msg_error("\
%s: aste_coord_init('%s') failed\n", pname, com.teldeffile);
		goto quit;
	}
	aste = teldef->mission.aste;
	det_xsiz = aste->det.xsiz;
	det_ysiz = aste->det.ysiz;
	sky_xsiz = aste->sky.xsiz;
	sky_ysiz = aste->sky.ysiz;

/* initialize orbit file for OBS */
	istat = aste_orbit_init(&com.obs_orbit, com.orbit);
	if ( istat ) {
		anl_msg_error("\
%s: error in orbit file '%s'\n", pname, com.orbit);
		goto quit;
	}

/* initialize orbit file for NXB */
	istat = aste_orbit_init(&com.nxb_orbit, com.nxborbit);
	if ( istat ) {
		anl_msg_error("\
%s: error in orbit file '%s'\n", pname, com.nxborbit);
		goto quit;
	}

/* open attitude file */
	if ( 0 == CLstricmp("none", com.attitude) ) {
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

/* allocate SORT_DATA */
	obs.spec_siz = 0;
	obs.image_siz = 0;
	nxb.spec_siz = XIS_PI_NCH;
	nxb.image_siz = det_xsiz * det_ysiz;
	obs.det_xsiz = nxb.det_xsiz = det_xsiz;
	obs.det_ysiz = nxb.det_ysiz = det_ysiz;
	nxb.skyimage_siz = 0;
	obs.sky_xsiz = nxb.sky_xsiz = sky_xsiz;
	obs.sky_ysiz = nxb.sky_ysiz = sky_ysiz;
	istat = alloc_sort_data(com.sortstep, &obs, &nxb);
	if ( istat ) goto quit;

/* open EHK file for OBS */
	if ( 0 == CLstricmp("none", com.ehkfile) ) {
		com.obs_hk = NULL;
	} else {
		anl_msg_info("\
%s: opening ehkfile '%s' ...\n", pname, com.ehkfile);
		com.obs_hk = aste_gethk_init(com.ehkfile);
		if ( NULL == com.obs_hk ) goto quit;
		com.obs_hkid = aste_gethk_id(com.obs_hk, com.sortkey);
		if ( com.obs_hkid < 0 ) goto quit;
	}

/* open EHK file for NXB */
	anl_msg_info("\
%s: opening nxbcorhk '%s' ...\n", pname, com.nxbcorhk);
	com.nxb_hk = aste_gethk_init(com.nxbcorhk);
	if ( NULL == com.nxb_hk ) goto quit;
	com.nxb_hkid = aste_gethk_id(com.nxb_hk, com.sortkey);
	if ( com.nxb_hkid < 0 ) goto quit;

/* apply xis ftools */
	istat = apply_xisftools();
	if ( istat ) goto quit;

/* open gti file */
	if ( 0 == CLstricmp("none", com.gtifile) ) {
		fits_reopen_file(ifp, &gti_fp, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_reopen_file() failed (%d)\n", pname, istat);
			goto quit;
		}
		anl_msg_info("\
%s: reading phafile '%s' ...\n", pname, com.phafile);
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

/* check region_mode */
	if ( 0 == CLstricmp("DETFITS", com.region_mode) ) {
		com.region_mode_id = REGION_MODE_DETFITS;
	} else if ( 0 == CLstricmp("DETREG", com.region_mode) ) {
		com.region_mode_id = REGION_MODE_DETREG;
	} else if ( 0 == CLstricmp("DETEXPR", com.region_mode) ) {
		com.region_mode_id = REGION_MODE_DETEXPR;
	} else if ( 0 == CLstricmp("SKYFITS", com.region_mode) ) {
		com.region_mode_id = REGION_MODE_SKYFITS;
	} else if ( 0 == CLstricmp("SKYREG", com.region_mode) ) {
		com.region_mode_id = REGION_MODE_SKYREG;
	} else if ( 0 == CLstricmp("SKYEXPR", com.region_mode) ) {
		com.region_mode_id = REGION_MODE_SKYEXPR;
	} else {
		anl_msg_error("\
%s: region_mode=%s is not supported\n", pname, com.region_mode);
	}

/* create region_map */
	switch ( com.region_mode_id ) {
	case REGION_MODE_DETFITS:
		anl_msg_info("\
%s: reading regfile '%s' ...\n", pname, com.regfile);
		com.regmap = read_detfits(teldef, com.regfile, &com.region_kp);
		if ( NULL == com.regmap ) goto quit;
		break;
	case REGION_MODE_DETREG:
		anl_msg_info("\
%s: reading regfile '%s' ...\n", pname, com.regfile);
		com.regmap = make_ds9reg_image(teldef, &obs.skyref,
			com.regfile, NULL, det_xsiz, det_ysiz);
		if ( NULL == com.regmap ) goto quit;
		break;
	case REGION_MODE_DETEXPR:
		anl_msg_info("\
%s: reading regfile '%s' ...\n", pname, com.regfile);
		com.regmap = make_ds9reg_image(teldef, &obs.skyref,
			NULL, com.regfile, det_xsiz, det_ysiz);
		if ( NULL == com.regmap ) goto quit;
		break;
	case REGION_MODE_SKYFITS:
		anl_msg_info("\
%s: reading regfile '%s' ...\n", pname, com.regfile);
		sky_regmap = read_skyfits(teldef, com.regfile, &com.region_kp);
		if ( NULL == sky_regmap ) goto quit;
		break;
	case REGION_MODE_SKYREG:
		anl_msg_info("\
%s: reading regfile '%s' ...\n", pname, com.regfile);
		sky_regmap = make_ds9reg_image(teldef, &obs.skyref,
			com.regfile, NULL, sky_xsiz, sky_ysiz);
		if ( NULL == sky_regmap ) goto quit;
		break;
	case REGION_MODE_SKYEXPR:
		anl_msg_info("\
%s: reading regfile '%s' ...\n", pname, com.regfile);
		sky_regmap = make_ds9reg_image(teldef, &obs.skyref,
			NULL, com.regfile, sky_xsiz, sky_ysiz);
		if ( NULL == sky_regmap ) goto quit;
		break;
	default:
		;
	}

/* calculate eulinfo for sky region */
	switch ( com.region_mode_id ) {
	case REGION_MODE_SKYFITS:
	case REGION_MODE_SKYREG:
	case REGION_MODE_SKYEXPR:
		if ( NULL == attfile ) {
			anl_msg_error("\
%s: attitude file is required for region_mode=%s\n", pname, com.region_mode);
			goto quit;
		}
		istat = calc_eulinfo(teldef, attfile, gti_fp, &obs);
		if ( istat ) goto quit;
		com.regmap = malloc( sizeof(*com.regmap) * nxb.image_siz );
		if ( NULL == com.regmap ) {
			anl_msg_error("\
%s: regmap malloc(image_size=%d) failed\n", pname, nxb.image_siz);
			goto quit;
		}
		memset(com.regmap, 0, sizeof(*com.regmap) * nxb.image_siz);
		anl_msg_info("\
%s: converting SKY coordinate region into DET coordinate ...\n", pname);
		istat = conv_skyreg(teldef, &obs, sky_regmap, com.regmap);
		free(sky_regmap);
		if ( istat ) goto quit;
		break;
	default:
		;
	}

/* allocate & initialize wmap */
	nxb.wmap = malloc( sizeof(*nxb.wmap) * nxb.image_siz );
	if ( NULL == nxb.wmap ) {
		anl_msg_error("\
%s: wmap malloc(image_siz=%d) failed\n", pname, nxb.image_siz);
		goto quit;
	}
	for (ipos = 0; ipos < nxb.image_siz; ipos++) {
		nxb.wmap[ipos] = (0.0 < com.regmap[ipos]) ? 0.0 : -1.0;
	}

/* read & merge detmask */
	if ( 0 != read_detmask_merge_regmap(teldef, com.detmask) ) {
		goto quit;
	}

/* read OBS GTI */
	istat = read_obs_gti(gti_fp, &obs);
	if ( istat ) goto quit;

/* open NXB database */
	fits_open_file(&nxb_fp, k=com.nxbevent_or_tmpfile, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}
	anl_msg_info("\
%s: reading nxbevent '%s' ...\n", pname, com.nxbevent_or_tmpfile);
	istat = read_nxb_evt(nxb_fp, &obs, &nxb);
	if ( istat ) goto quit;

/* calculate weighted NXB */
	istat = calc_weighted_nxb(&obs, &nxb);
	if ( istat ) goto quit;

/* convert to skyimage */
	switch ( com.region_mode_id ) {
	case REGION_MODE_SKYFITS:
	case REGION_MODE_SKYREG:
	case REGION_MODE_SKYEXPR:
		nxb.skyimage_siz = sky_xsiz * sky_ysiz;
		nxb.skyimage = malloc( sizeof(*nxb.skyimage) * nxb.skyimage_siz );
		if ( NULL == nxb.skyimage ) {
			anl_msg_error("\
%s: regmap malloc(image_size=%d) failed\n", pname, nxb.skyimage_siz);
			goto quit;
		}
		memset(nxb.skyimage, 0, sizeof(*nxb.skyimage) * nxb.skyimage_siz);
		anl_msg_info("\
%s: converting DET coordinate image into SKY coordinate ...\n", pname);
		istat = conv_detimage(teldef, &obs, &nxb);
		if ( istat ) goto quit;
		break;
	default:
		;
	}

/* create output file */
	anl_msg_info("\
%s: creating outfile '%s' ...\n", pname, com.outfile);
	if ( com.clobber ) unlink(com.outfile);
	fits_create_file(&ofp, com.outfile, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: creating outfile '%s' failed (%d)\n", pname, com.outfile, istat);
		goto quit;
	}

/* copy regmap to wmap for test purpose */
/*	for (ipos = 0; ipos < nxb.image_siz; ipos++) {
		nxb.wmap[ipos] = com.regmap[ipos];
	}*/

/* write wmap */
	anl_msg_info("\
   writing WMAP ...\n");
	istat = write_wmap(ofp, nxb_fp, &obs, &nxb);
	if ( istat ) goto quit;

/* write spectrum */
	anl_msg_info("\
   writing SPECTRUM ...\n");
	istat = write_spec(ofp, nxb_fp, &obs, &nxb);
	if ( istat ) goto quit;

/* write image */
	anl_msg_info("\
   writing IMAGE_DET ...\n");
	istat = write_detimage(ofp, nxb_fp, &obs, &nxb);
	if ( istat ) goto quit;

/* write image */
	switch ( com.region_mode_id ) {
	case REGION_MODE_SKYFITS:
	case REGION_MODE_SKYREG:
	case REGION_MODE_SKYEXPR:
		anl_msg_info("\
   writing IMAGE_SKY ...\n");
		istat = write_skyimage(ofp, nxb_fp, teldef, &obs, &nxb);
		if ( istat ) goto quit;
		anl_msg_warning("\
%s: WARNING: *************************************************\n\
%s: WARNING:  Region mode in SKY coordinates is EXPERIMENTAL.\n\
%s: WARNING:  Please check the output carefully by yourslef.\n\
%s: WARNING: *************************************************\n",
			pname, pname, pname, pname);
		break;
	default:
		;
	}

/* free memory */
	free(nxb.wmap);
	free(com.regmap);
	for (ipos = 0; ipos <= nxb.nsort; ipos++) {
		free(nxb.sp[ipos].spec);
		free(nxb.sp[ipos].image);
	}
	free(obs.sp);

/* close fits files */
	fits_close_file(ofp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file() failed for outfile (%d)\n", pname, istat);
		goto quit;
	}

	fits_close_file(ifp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file() failed for phafile (%d)\n", pname, istat);
		goto quit;
	}

	fits_close_file(gti_fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file() failed for gtifile (%d)\n", pname, istat);
		goto quit;
	}

	fits_close_file(nxb_fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file() failed for nxbevent (%d)\n", pname, istat);
		goto quit;
	}
	if ( com.nxbevent_or_tmpfile != com.nxbevent ) {
		anl_msg_info("\
%s: remove temporary file '%s'\n", pname, com.nxbevent_or_tmpfile);
		unlink(com.nxbevent_or_tmpfile);
	}

	aste_att_close(attfile);
	if ( aste_orbit_free(com.obs_orbit) ) goto quit;
	if ( aste_orbit_free(com.nxb_orbit) ) goto quit;

/* show XIS0 anomaly warning if needed */
	show_xis0_anomaly_warning();

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
xisnxbgen_his(int *status)
{
	*status = ANL_OK;
}

void
xisnxbgen_bgnrun(int *status)
{
	*status = ANL_OK;
}

void
xisnxbgen_ana(int nevent, int eventid, int *status)
{
	*status = ANL_QUIT;
}

void
xisnxbgen_endrun(int *status)
{
	*status = ANL_OK;
}

void
xisnxbgen_exit(int *status)
{
	*status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; c-basic-offset:4  ***
;;; End: ***
*/
