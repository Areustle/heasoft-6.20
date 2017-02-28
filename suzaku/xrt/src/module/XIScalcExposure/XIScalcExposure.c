/* $Id: XIScalcExposure.c,v 1.4 2006/11/01 00:27:38 ishisaki Exp $ */
/****************************************************
*
* XIScalcExposure.c
*
* 2003-09-30	version 1.20	Y.ISHISAKI
*	modified for HEADAS
*	scale wmap 1.0e6
*
* 2003-09-08	version 1.10	Y.ISHISAKI
*	Made from XIScalcExposure.c version 1.10
*
****************************************************/
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
#include "xisarf.h"

static char pname[] = "XIScalcExposure";
char XIScalcExposure_version[] = "version 1.20";

/* XIScalcExposure parameters */
static struct {
	char image_file[PIL_LINESIZE];
	double pixel_size;
	int nx, ny;
	float *image;
	TELDEF *teldef;
	int detxsiz, detysiz, detxpix1, detypix1;
	double detxscl, detyscl;
	double wmap_scale;
	float *wmap;
	struct {
		enum xis_pos_type type;
		double xis_xmm, xis_ymm;
	} pos;
	struct {
		enum xis_reg_type type;
		double xis_xmm, xis_ymm, xis_rmi, xis_rma;
	} reg;
	double exposure;
} com = {
	"none",		/* image_file */
	0.024,		/* pixel_size */
	0, 0,		/* nx, ny */
	NULL,		/* image */
	NULL,		/* teldef */
	0, 0,		/* detxsiz, detysiz */
	1, 1, 		/* detxpix1, detypix1 */
	0.0, 0.0, 	/* detxscl, detyscl */
	1.0e6,		/* wmap_scale */
	NULL,		/* wmap */
	{
		PT_POINT_XISXY_MM,		/* pos.type */
		0.0, 0.0,				/* pos.xis_xmm, xis_ymm */
	},
	{
		RT_XISXYRmiRma_MM,		/* reg.type */
		0.0, 0.0, 0.0, 0.0		/* reg.xis_xmm, xis_ymm, xis_rmi, xis_rma */
	},
	0.0			/* exposure */
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
}

static int
write_fits_header(fitsfile *fp)
{
	char history[FLEN_FILENAME+FLEN_KEYWORD];
	int status = 0;

	sprintf(history, "%s %s", pname, XIScalcExposure_version);
	fits_write_history(fp, history, &status);
	sprintf(history, "  psf_file=%s", com.image_file);
	fits_write_history(fp, history, &status);
	sprintf(history, "  psf_pixel_size=%.6f (mm)", com.pixel_size);
	fits_write_history(fp, history, &status);

	return status;
}

static int
write_wmap(fitsfile *fp)
{
	int hdutype;
	int status = 0;

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

	if ( com.pixel_size <= 0.0 ) {
		fprintf(stderr, "\
%s: invalid pixel size (%.6f mm)\n", pname, com.pixel_size);
		return -1;
	}

	return 0;
}

static double
calc_exposure(void)
{
	float *wmap;
	int ix, iy, ixpos, iypos;
	double dx, dy, rmm2, rmi2, rma2;
	double xis_xch, xis_ych, xis_xmm, xis_ymm;
	double xrt_xmm, xrt_ymm, pos_xrt_xmm, pos_xrt_ymm;
	double img_xcen, img_ycen, img_xch, img_ych, img_val;
	double exposure, corr_factor;

	if ( PT_POINT_XISXY_MM != com.pos.type ) {
		fprintf(stderr, "\
%s: POSITION_TYPE (%d) not supported\n", pname, com.pos.type);
		return -1.0;
	}

	xis_xmm = com.pos.xis_xmm;
	xis_ymm = com.pos.xis_ymm;
	aste_det_mm2ch(com.teldef, xis_xmm, xis_ymm, &xis_xch, &xis_ych);
	aste_det2xrt(com.teldef, xis_xch, xis_ych, &pos_xrt_xmm, &pos_xrt_ymm);

	if ( RT_XISXYRmiRma_MM != com.reg.type ) {
		fprintf(stderr, "\
%s: REGION_TYPE (%d) not supported\n", pname, com.reg.type);
		return -1.0;
	}

	rmi2 = com.reg.xis_rmi * com.reg.xis_rmi;
	rma2 = com.reg.xis_rma * com.reg.xis_rma;

	img_xcen = (com.nx + 1) / 2.0;
	img_ycen = (com.ny + 1) / 2.0;
	exposure = 0.0;
	corr_factor = com.detxscl * com.detyscl / com.pixel_size / com.pixel_size;
	if ( corr_factor < 0.0 ) {
		corr_factor = - corr_factor;
	}

	for (iy = 0; iy < com.detysiz; iy++) {
		xis_ych = com.detypix1 + iy;
		for (ix = 0; ix < com.detxsiz; ix++) {
			xis_xch = com.detxpix1 + ix;
			/* initialize wmap */
			wmap = &com.wmap[iy*com.detxsiz + ix];
			*wmap = 0.0;
			aste_det_ch2mm(com.teldef, xis_xch, xis_ych, &xis_xmm, &xis_ymm);
			dx = xis_xmm - com.reg.xis_xmm;
			dy = xis_ymm - com.reg.xis_ymm;
			rmm2 = dx*dx + dy*dy;
			if ( rmm2 < rmi2 || rma2 < rmm2 ) {
				/* out of integration region */
				*wmap = -1.0;
				continue;
			}
			aste_det2xrt(com.teldef, xis_xch, xis_ych, &xrt_xmm, &xrt_ymm);
			dx = xrt_xmm - pos_xrt_xmm;
			dy = xrt_ymm - pos_xrt_ymm;
			img_xch = img_xcen + dx / com.pixel_size;
			img_ych = img_ycen + dy / com.pixel_size;
			ixpos = (int)(img_xch + 0.5) - 1;
			iypos = (int)(img_ych + 0.5) - 1;
			if ( ixpos < 0 || com.nx <= ixpos ) {
				/* no PSF data */
				continue;
			}
			if ( iypos < 0 || com.ny <= iypos ) {
				/* no PSF data */
				continue;
			}
			img_val = corr_factor * com.image[iypos*com.nx + ixpos];
			exposure += img_val;
			*wmap = com.wmap_scale * img_val;
		}
	}

	return exposure;
}

void
XIScalcExposure_startup(int *status)
{
	;
}

void
XIScalcExposure_com(int *status)
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
		*status = PILGetFname("psf_file", com.image_file);
		if ( *status ) goto quit;
		*status = PILGetReal("psf_pixel_size", &com.pixel_size);
		if ( *status ) goto quit;

		*status = ANL_OK;;
		return;

	quit:
		*status = ANL_QUIT;
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
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
XIScalcExposure_init(int *status)
{
	int used;
	fitsfile *fp;
	char *arf_file;

	EvsDef("XIScalcExposure:BEGIN");
	EvsDef("XIScalcExposure:ENTRY");
	EvsDef("XIScalcExposure:OK");

	show_parameter("%s:  *** show parameter ***");

	printf("\n\
%s: reading IMAGE file '%s'\n", pname, com.image_file);
	*status = read_image_file(com.image_file);
	if ( *status ) {
		*status = ANL_QUIT;
		return;
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
	com.detxpix1 = com.teldef->mission.aste->det.xpix1;
	com.detypix1 = com.teldef->mission.aste->det.ypix1;
	com.detxscl = com.teldef->mission.aste->det.xscl;
	com.detyscl = com.teldef->mission.aste->det.yscl;
	com.wmap = malloc(sizeof(*com.wmap) * com.detxsiz * com.detysiz);
	if ( NULL == com.wmap ) {
		fprintf(stderr, "\
%s: WMAP malloc failed for '%s'\n", pname, arf_file);
		*status = ANL_QUIT;
		return;
	}
	memset(com.wmap, 0, sizeof(*com.wmap) * com.detxsiz * com.detysiz);

	BnkGet("ASTEARF:POS:TYPE", sizeof(com.pos.type),
		   &used, &com.pos.type);
	BnkGet("ASTEARF:POS:XIS_XMM", sizeof(com.pos.xis_xmm),
		   &used, &com.pos.xis_xmm);
	BnkGet("ASTEARF:POS:XIS_YMM", sizeof(com.pos.xis_ymm),
		   &used, &com.pos.xis_ymm);
	BnkGet("ASTEARF:REG:TYPE", sizeof(com.reg.type),
		   &used, &com.reg.type);
	BnkGet("ASTEARF:REG:XIS_XMM", sizeof(com.reg.xis_xmm),
		   &used, &com.reg.xis_xmm);
	BnkGet("ASTEARF:REG:XIS_YMM", sizeof(com.reg.xis_ymm),
		   &used, &com.reg.xis_ymm);
	BnkGet("ASTEARF:REG:XIS_RMI", sizeof(com.reg.xis_rmi),
		   &used, &com.reg.xis_rmi);
	BnkGet("ASTEARF:REG:XIS_RMA", sizeof(com.reg.xis_rma),
		   &used, &com.reg.xis_rma);

	printf("\
%s: calculating exposure\n", pname);
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
XIScalcExposure_his(int *status)
{
	*status = ANL_OK;
}

void
XIScalcExposure_bgnrun(int *status)
{
	EvsSet("XIScalcExposure:BEGIN");

	*status = ANL_OK;
}

void
XIScalcExposure_ana(int nevent, int eventid, int *status)
{
	int used;
	double energ_lo, energ_hi, specresp, exposure;

	EvsfSetM("XIScalcExposure:ENTRY");

	BnkfGetM("ASTEARF:ENERG_LO", sizeof(energ_lo), &used, &energ_lo);
	BnkfGetM("ASTEARF:ENERG_HI", sizeof(energ_hi), &used, &energ_hi);
	BnkfGetM("ASTEARF:SPECRESP", sizeof(specresp), &used, &specresp);
	BnkfGetM("ASTEARF:EXPOSURE", sizeof(exposure), &used, &exposure);

	specresp *= com.exposure;
	exposure *= com.exposure;

	BnkfPutM("ASTEARF:SPECRESP", sizeof(specresp), &specresp);
	BnkfPutM("ASTEARF:EXPOSURE", sizeof(exposure), &exposure);

	EvsfSetM("XIScalcExposure:OK");
}

void
XIScalcExposure_endrun(int *status)
{
	*status = ANL_OK;
}

void
XIScalcExposure_exit(int *status)
{
	*status = ANL_OK;
}
