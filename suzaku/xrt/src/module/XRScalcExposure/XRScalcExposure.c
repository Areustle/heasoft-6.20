/* $Id: XRScalcExposure.c,v 1.4 2006/11/01 00:27:39 ishisaki Exp $

	XRScalcExposure.c

	2003/02/16	Y.ISHISAKI	version 1.00

	2003/09/08	Y.ISHISAKI	version 1.10
		Change BNK keyword, "XRSarf" -> "ASTEARF"
		Change enum pos_type -> xrs_pos_type

 	2003/09/30	Y.ISHISAKI	version 1.20
		modified for HEADAS
		Initialize wmap by -1.0, scale wmap 1.0e6

	2003/11/10	Y.ISHISAKI	version 1.30
		read FW & GV transmission file

	2005/07/30	Y.ISHISAKI	version 2.0
		read bf & qe files
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "pil.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "xrsarf.h"

static char pname[] = "XRScalcExposure";
char XRScalcExposure_version[] = "version 2.00";

/* XRScalcExposure parameters */
static struct {
	char image_file[PIL_LINESIZE];
	double pixel_size;
	int nx, ny;
	float *image;
	TELDEF *teldef;
	int detxsiz, detysiz;
	double wmap_scale;
	float *wmap;
	struct {
		enum xrs_pos_type type;
		double xrs_xmm, xrs_ymm;
	} pos;
	char pixel_select[32];
	double exposure;
	struct transmission {
		char trans_file[PIL_LINESIZE];
		int nrow;
		double *energy, *trans;
	} fw, gv, bf, qe;
} com = {
	"none",		/* image_file */
	0.024,		/* pixel_size */
	0, 0,		/* nx, ny */
	NULL,		/* image */
	NULL,		/* teldef */
	0, 0,		/* detxsiz, detysiz */
	1.0e6,		/* wmap_scale */
	NULL,		/* wmap */
	{
		PT_POINT_XRSXY,					/* pos.type */
		0.0, 0.0						/* pos.xrs_xmm, xrs_ymm */
	},
	{1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,	/* pixel_select */
	 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},
	0.0,		/* exposure */
	{
		"none",	/* fw.trans_file */
		0,		/* fw.nrow */
		NULL, NULL	/* fw.energy, fw.trans */
	}, {
		"none",	/* gv.trans_file */
		0,		/* gv.nrow */
		NULL, NULL	/* gv.energy, gv.trans */
	}, {
		"none",	/* bf.trans_file */
		0,		/* bf.nrow */
		NULL, NULL	/* bf.energy, bf.trans */
	}, {
		"none",	/* qe.trans_file */
		0,		/* qe.nrow */
		NULL, NULL	/* qe.energy, qe.trans */
	}
};

static void
MSG(char *format, ...)
{
	FILE *fp = stdout;
	va_list args;
	va_start(args, format);
	if ( '!' == *format ) {
		vfprintf(fp, format+1, args);
	} else {
		vfprintf(fp, format, args);
		fputc('\n', fp);
	}
	va_end(args);
	if ( isatty(fileno(fp)) ) fflush(fp);
}

static void
show_parameter(char *title)
{
	MSG("");
	MSG(title, pname);
	MSG("");
	MSG("%4s%-20s'%s'", "", "IMAGE_file", com.image_file);
	MSG("%4s%-20s%.6f (mm)", "", "PIXEL_SIZE", com.pixel_size);
	MSG("%4s%-20s'%s'", "", "FW_FILE", com.fw.trans_file);
	MSG("%4s%-20s'%s'", "", "GV_FILE", com.gv.trans_file);
	MSG("%4s%-20s'%s'", "", "BF_FILE", com.bf.trans_file);
	MSG("%4s%-20s'%s'", "", "QE_FILE", com.qe.trans_file);
}

static int
write_fits_header(fitsfile *fp)
{
	char history[FLEN_FILENAME+FLEN_KEYWORD];
	int status = 0;

	sprintf(history, "%s %s", pname, XRScalcExposure_version);
	fits_write_history(fp, history, &status);
	sprintf(history, "  psf_file=%s", com.image_file);
	fits_write_history(fp, history, &status);
	sprintf(history, "  psf_pixel_size=%.6f (mm)", com.pixel_size);
	fits_write_history(fp, history, &status);
	sprintf(history, "  fw_file=%s", com.fw.trans_file);
	fits_write_history(fp, history, &status);
	sprintf(history, "  gv_file=%s", com.gv.trans_file);
	fits_write_history(fp, history, &status);
	sprintf(history, "  bf_file=%s", com.bf.trans_file);
	fits_write_history(fp, history, &status);
	sprintf(history, "  qe_file=%s", com.qe.trans_file);
	fits_write_history(fp, history, &status);

	return status;
}

static int
write_wmap(fitsfile *fp)
{
	int hdutype;
	int status = 0;

#if 0
	int ix, iy, pixel;
	for (iy = 0; iy < com.detysiz; iy++) {
		for (ix = 0; ix < com.detxsiz; ix++) {
			xrs_det2pixel(com.teldef, ix, iy, &pixel);
			com.wmap[iy*com.detxsiz+ix] = pixel;
		}
	}
#endif

	fits_movrel_hdu(fp, -1, &hdutype, &status);
	fits_write_img(fp, TFLOAT, 1, com.detxsiz*com.detysiz, com.wmap, &status);
	fits_movrel_hdu(fp, 1, &hdutype, &status);

	return status;
}

static int
read_image_file(char *image_file)
{
	int status;
	int bitpix, naxis, nx, ny, anynul;
	long naxes[2];
	fitsfile *fp;

	status = 0;
	fits_open_image(&fp, image_file, READONLY, &status);
	if ( status ) {
		fprintf(stderr, "\
%s: XRT image file '%s' open failed\n", pname, image_file);
		return -1;
	}

	fits_get_img_param(fp, 2, &bitpix, &naxis, naxes, &status);
	if ( status || 2 != naxis ) {
		fprintf(stderr, "\
%s: no image data in '%s'\n", pname, image_file);
		return -1;
	}
	nx = com.nx = naxes[0];
	ny = com.ny = naxes[1];
	com.image = malloc(sizeof(*com.image) * nx * ny);
	if ( NULL == com.image ) {
		fprintf(stderr, "\
%s: %dx%d image malloc failed for '%s'\n", pname, nx, ny, image_file);
		return -1;
	}
	fits_read_img(fp, TFLOAT, 1, nx*ny, NULL, com.image, &anynul, &status);
	fits_close_file(fp, &status);
	if ( status ) {
		fprintf(stderr, "\
%s: %dx%d image read error for '%s'\n", pname, nx, ny, image_file);
		return -1;
	}

	return 0;
}

static int
read_trans_file(struct transmission *p, int qe)
{
	int status, hdutype;
	fitsfile *fp;

	status = 0;
	fits_open_file(&fp, p->trans_file, READONLY, &status);
	if ( status ) {
		fprintf(stderr, "\
%s: XRS FW/GV transmission '%s' open failed\n", pname, p->trans_file);
		return -1;
	}

	for (;;) {
		int ny, col_energy, col_trans, anynul;
		char comment[80];
		int exact = 1;

		status = 0;
		fits_movrel_hdu(fp, 1, &hdutype, &status);
		if ( status ) {
			fits_close_file(fp, &status);
			fprintf(stderr, "\
%s: no transmission data in '%s'\n", pname, p->trans_file);
			return -1;
		}
		fits_read_key(fp, TINT, "NAXIS2", &ny, comment, &status);
		ffgcno(fp, exact, "ENERGY", &col_energy, &status);
		if ( 0 == qe ) {
			ffgcno(fp, exact, "TRANSMIS", &col_trans, &status);
		} else {
			ffgcno(fp, exact, "QE", &col_trans, &status);
		}
		p->energy = malloc(2 * sizeof(double) * ny);
		p->trans = p->energy + ny;
		if ( NULL == p->energy ) {
			fprintf(stderr, "\
%s: ENERGY/TRANSMIS/QE malloc failed for '%s'\n", pname, p->trans_file);
			return -1;
		}
		ffgcvd(fp, col_energy, 1, 1, ny, 0.0, p->energy, &anynul, &status);
		ffgcvd(fp, col_trans,  1, 1, ny, 0.0, p->trans,  &anynul, &status);
		fits_close_file(fp, &status);
		if ( status ) {
			fprintf(stderr, "\
%s: ENERGY/TRANSMIS/QE column read error for '%s'\n", pname, p->trans_file);
			return -1;
		}
		return ny;
	}
}

static double
integ_linlin(int n, double x[], double y[], double xlo, double xhi)
{
	int i;
	double integ = 0.0;

	for (i = 0; i < n-1; i++) {
		double s, ylo, yhi;
		double x0 = x[i];
		double x1 = x[i+1];
		double y0 = y[i];
		double y1 = y[i+1];

		if ( xhi <= x0 ) break;
		if ( x1 <= xlo ) continue;

		s = (y0 + y1) * (x1 - x0) / 2;
		if ( x0 < xlo ) {
			ylo = ( (xlo - x0) * y1 + (x1 - xlo) * y0 ) / (x1 - x0);
			s -= (y0 + ylo) * (xlo - x0) / 2;
		}
		if ( xhi < x1 ) {
			yhi = ( (xhi - x0) * y1 + (x1 - xhi) * y0 ) / (x1 - x0);
			s -= (yhi + y1) * (x1 - xhi) / 2;
		}
		integ += s;
	}

	return integ;
}

static int
calc_exposure_at(double img_xmm, double img_ymm, double img_val, int nstep)
{
	int ix, iy, ipos, ipix, npix, num_inside, ixch, iych;
	double xmm, ymm, xch, ych, img_val_frac;

	npix = 32;
	num_inside = 0;
	img_val_frac = com.wmap_scale * img_val / nstep / nstep;

	for (iy = 0; iy < nstep; iy++) {
		ymm = img_ymm + com.pixel_size * ( (iy + 0.5) / nstep - 0.5 );
		for (ix = 0; ix < nstep; ix++) {
			xmm = img_xmm + com.pixel_size * ( (ix + 0.5) / nstep - 0.5 );
			aste_det_mm2ch(com.teldef, xmm, ymm, &xch, &ych);
			xrs_det2pixel(com.teldef, xch, ych, &ipix);
			if ( 0 <= ipix && ipix < npix && com.pixel_select[ipix] ) {
				num_inside++;
				ixch = (int)(xch + 0.5);
				iych = (int)(ych + 0.5);
				if ( 0 <= ixch && ixch < com.detxsiz &&
					 0 <= iych && iych < com.detysiz ) {
					ipos = iych*com.detxsiz + ixch;
					if ( com.wmap[ipos] < 0.0 ) {
						com.wmap[ipos] = 0.0;
					}
					com.wmap[ipos] += img_val_frac;
				}
			}
		}
	}

	return num_inside;
}

static double
calc_exposure(void)
{
	int ix, iy, ipix, npix, nstep, num_corner_inside, cpix[4];
	double xl_mm, xh_mm, yl_mm, yh_mm;
	double xl_ch, xh_ch, yl_ch, yh_ch;
	double img_xcen, img_ycen, img_xmm, img_ymm, img_val;
	double exposure;

	if ( PT_POINT_XRSXY != com.pos.type ) {
		fprintf(stderr, "\
%s: POSITION_TYPE (%d) not supported\n", pname, com.pos.type);
		return -1.0;
	}

	img_xcen = (com.nx + 1) / 2.0;
	img_ycen = (com.ny + 1) / 2.0;
	npix = 32;
	exposure = 0.0;
	nstep = (int)(1000 * com.pixel_size + 0.5);		/* 1 um step */
	if ( 0 == nstep ) {
		nstep = 1;
	}

	for (iy = 0; iy < com.ny; iy++) {
		img_ymm = (iy + 1 - img_ycen) * com.pixel_size + com.pos.xrs_ymm;
		yl_mm = img_ymm - com.pixel_size/2;
		yh_mm = img_ymm + com.pixel_size/2;

		for (ix = 0; ix < com.nx; ix++) {
			img_val = com.image[iy*com.nx + ix];

			if ( img_val < 0.0 ) {
				continue;
			}

			img_xmm = (ix + 1 - img_xcen) * com.pixel_size + com.pos.xrs_xmm;
			xl_mm = img_xmm - com.pixel_size/2;
			xh_mm = img_xmm + com.pixel_size/2;

			aste_det_mm2ch(com.teldef, xl_mm, yl_mm, &xl_ch, &yl_ch);
			aste_det_mm2ch(com.teldef, xh_mm, yh_mm, &xh_ch, &yh_ch);
			num_corner_inside = 0;

			if ( xh_ch < 0 || com.detxsiz < xl_ch ||
				 yh_ch < 0 || com.detysiz < yl_ch ) {
				continue;
			}

			xrs_det2pixel(com.teldef, xl_ch, yl_ch, &ipix);
			if ( 0 <= ipix && ipix < npix && com.pixel_select[ipix] ) {
				num_corner_inside++;
			}
			cpix[0] = ipix;

			xrs_det2pixel(com.teldef, xh_ch, yl_ch, &ipix);
			if ( 0 <= ipix && ipix < npix && com.pixel_select[ipix] ) {
				num_corner_inside++;
			}
			cpix[1] = ipix;

			xrs_det2pixel(com.teldef, xh_ch, yh_ch, &ipix);
			if ( 0 <= ipix && ipix < npix && com.pixel_select[ipix] ) {
				num_corner_inside++;
			}
			cpix[2] = ipix;

			xrs_det2pixel(com.teldef, xl_ch, yh_ch, &ipix);
			if ( 0 <= ipix && ipix < npix && com.pixel_select[ipix] ) {
				num_corner_inside++;
			}
			cpix[3] = ipix;

			if ( 0 == num_corner_inside ) {
				/* completely outside pixel */
			} else if ( 4 == num_corner_inside &&
					    cpix[0] == cpix[1] &&
					    cpix[1] == cpix[2] &&
					    cpix[2] == cpix[3] ) {
				/* completely inside pixel */
				exposure += img_val;
				calc_exposure_at(img_xmm, img_ymm, img_val, nstep);
			} else {
				/* partially inside pixel */
				int num = calc_exposure_at(img_xmm, img_ymm, img_val, nstep);
				exposure += img_val * num / nstep / nstep;
			}
		}
	}

	return exposure;
}

void
XRScalcExposure_startup(int *status)
{
	;
}

void
XRScalcExposure_com(int *status)
{
	static char *keytbl[] = {
		"SHOW",
		"IMAGE_file",
		"PIXEL_SIZE",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"XRT image file name",
		"XRT image pixel size (mm)",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);

	if ( *status ) {	/* HEADAS */
		if (
PILGetFname("psf_file", com.image_file) ||
PILGetReal ("psf_pixel_size", &com.pixel_size) ||
PILGetFname("fw_file", com.fw.trans_file) ||
PILGetFname("gv_file", com.gv.trans_file) ||
PILGetFname("bf_file", com.bf.trans_file) ||
PILGetFname("qe_file", com.qe.trans_file) ||
			 0 ) {
			*status = ANL_QUIT;
			return;
		}

		*status = ANL_OK;;
		return;
	}

	for (;;) {
		char *key;
		int ans[2];

		CMinquir(pname, nkey, keytbl, help, 1, ans);
		key = keytbl[ans[1]-1];
		if ( 0 == strcmp("SHOW", key) ) {
			show_parameter("%s:  *** show parameter ***");
		} else if ( 0 == strcmp("IMAGE_file", key) ) {
			CLtxtrd(key, com.image_file, sizeof(com.image_file));
		} else if ( 0 == strcmp("PIXEL_SIZE", key) ) {
			CLfdprd(key, &com.pixel_size);
		} else if ( 0 == strcmp("FW_file", key) ) {
			CLtxtrd(key, com.fw.trans_file, sizeof(com.fw.trans_file));
		} else if ( 0 == strcmp("GV_file", key) ) {
			CLtxtrd(key, com.gv.trans_file, sizeof(com.gv.trans_file));
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
XRScalcExposure_init(int *status)
{
	int i, used, wmapsize;
	fitsfile *fp;
	char *arf_file;

	EvsDef("XRScalcExposure:BEGIN");
	EvsDef("XRScalcExposure:ENTRY");
	EvsDef("XRScalcExposure:OK");

	show_parameter("%s:  *** show parameter ***");
	MSG("");

	fflush(NULL); printf("\
%s: reading IMAGE file '%s'\n", pname, com.image_file);
	fflush(NULL);
	*status = read_image_file(com.image_file);
	if ( *status ) {
		*status = ANL_QUIT;
		return;
	}

	if ( 0 != CLstricmp("none", com.fw.trans_file) ) {
		fflush(NULL); printf("\
%s: reading FW file '%s'\n", pname, com.fw.trans_file);
		fflush(NULL);
		com.fw.nrow = read_trans_file(&com.fw, 0);
		if ( com.fw.nrow < 0 ) {
			*status = ANL_QUIT;
			return;
		}
	}

	if ( 0 != CLstricmp("none", com.gv.trans_file) ) {
		fflush(NULL); printf("\
%s: reading GV file '%s'\n", pname, com.gv.trans_file);
		fflush(NULL);
		com.gv.nrow = read_trans_file(&com.gv, 0);
		if ( com.gv.nrow < 0 ) {
			*status = ANL_QUIT;
			return;
		}
	}

	if ( 0 != CLstricmp("none", com.bf.trans_file) ) {
		fflush(NULL); printf("\
%s: reading BF file '%s'\n", pname, com.bf.trans_file);
		fflush(NULL);
		com.bf.nrow = read_trans_file(&com.bf, 0);
		if ( com.bf.nrow < 0 ) {
			*status = ANL_QUIT;
			return;
		}
	}

	if ( 0 != CLstricmp("none", com.qe.trans_file) ) {
		fflush(NULL); printf("\
%s: reading QE file '%s'\n", pname, com.qe.trans_file);
		fflush(NULL);
		com.qe.nrow = read_trans_file(&com.qe, 1);
		if ( com.qe.nrow < 0 ) {
			*status = ANL_QUIT;
			return;
		}
	}

	BnkGet("ASTEARF:FITS_PTR", sizeof(fp), &used, &fp);
	BnkGet("ASTEARF:ARF_FILE", sizeof(arf_file), &used, &arf_file);
	*status = write_fits_header(fp);
	if ( *status ) {
		fprintf(stderr, "\
%s: writing history failed for '%s'\n", pname, arf_file);
		*status = ANL_QUIT;
		return;
	}

	BnkGet("ASTEARF:TELDEF", sizeof(com.teldef), &used, &com.teldef);
	com.detxsiz = com.teldef->mission.aste->det.xsiz;
	com.detysiz = com.teldef->mission.aste->det.ysiz;
	wmapsize = com.detxsiz * com.detysiz;
	com.wmap = malloc(sizeof(*com.wmap) * wmapsize);
	if ( NULL == com.wmap ) {
		fprintf(stderr, "\
%s: WMAP malloc failed for '%s'\n", pname, arf_file);
		*status = ANL_QUIT;
		return;
	}
	for (i = 0; i < wmapsize; i++) {
		com.wmap[i] = -1.0;
	}

	BnkGet("ASTEARF:POS:TYPE", sizeof(com.pos.type),
		   &used, &com.pos.type);
	BnkGet("ASTEARF:POS:XRS_XMM", sizeof(com.pos.xrs_xmm),
		   &used, &com.pos.xrs_xmm);
	BnkGet("ASTEARF:POS:XRS_YMM", sizeof(com.pos.xrs_ymm),
		   &used, &com.pos.xrs_ymm);
	BnkGet("ASTEARF:PIXEL_SELECT", sizeof(com.pixel_select),
		   &used, com.pixel_select);

	fflush(NULL); printf("\
%s: calculating exposure\n", pname);
	fflush(NULL);
	com.exposure = calc_exposure();
	if ( com.exposure < 0.0 ) {
		*status = ANL_QUIT;
		return;
	}

	*status = write_wmap(fp);
	if ( *status ) {
		fprintf(stderr, "\
%s: writing WMAP failed for '%s'\n", pname, arf_file);
		*status = ANL_QUIT;
		return;
	}

	*status = ANL_OK;
}

void
XRScalcExposure_his(int *status)
{
	*status = ANL_OK;
}

void
XRScalcExposure_bgnrun(int *status)
{
	EvsSet("XRScalcExposure:BEGIN");

	*status = ANL_OK;
}

void
XRScalcExposure_ana(int nevent, int eventid, int *status)
{
	int used;
	struct transmission *p;
	double energ_lo, energ_hi, specresp, exposure, frac;

	EvsfSetM("XRScalcExposure:ENTRY");

	BnkfGetM("ASTEARF:ENERG_LO", sizeof(energ_lo), &used, &energ_lo);
	BnkfGetM("ASTEARF:ENERG_HI", sizeof(energ_hi), &used, &energ_hi);
	BnkfGetM("ASTEARF:SPECRESP", sizeof(specresp), &used, &specresp);
	BnkfGetM("ASTEARF:EXPOSURE", sizeof(exposure), &used, &exposure);

	frac = com.exposure;

	p = &com.fw;
	if ( p->nrow ) {
		frac *= integ_linlin(p->nrow, p->energy, p->trans, energ_lo, energ_hi);
		frac /= energ_hi - energ_lo;
	}

	p = &com.gv;
	if ( p->nrow ) {
		frac *= integ_linlin(p->nrow, p->energy, p->trans, energ_lo, energ_hi);
		frac /= energ_hi - energ_lo;
	}

	p = &com.bf;
	if ( p->nrow ) {
		frac *= integ_linlin(p->nrow, p->energy, p->trans, energ_lo, energ_hi);
		frac /= energ_hi - energ_lo;
	}

	p = &com.qe;
	if ( p->nrow ) {
		frac *= integ_linlin(p->nrow, p->energy, p->trans, energ_lo, energ_hi);
		frac /= energ_hi - energ_lo;
	}

	specresp *= frac;
	exposure *= frac;

	BnkfPutM("ASTEARF:SPECRESP", sizeof(specresp), &specresp);
	BnkfPutM("ASTEARF:EXPOSURE", sizeof(exposure), &exposure);

	EvsfSetM("XRScalcExposure:OK");
}

void
XRScalcExposure_endrun(int *status)
{
	*status = ANL_OK;
}

void
XRScalcExposure_exit(int *status)
{
	*status = ANL_OK;
}
