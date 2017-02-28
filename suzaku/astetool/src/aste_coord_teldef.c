/* aste_coord_teldef.c,v 1.1 1999/12/20 03:45:08 ishisaki Exp */
/************************************************************************
  aste_coord_teldef.c

  aste_coord_init	aste_coord initialization function
  aste_coord_free	free allocated memory for TELDEF structure
  aste_coord_teldef	return already allocated TELDEF structure

  1999-12-19	Y.ISHISAKI

  2000-01-28	Y.ISHISAKI
	fix of memory alignemnt for Solaris

  2003-02-15	Y.ISHISAKI
	also accepts "Astro-E2" in aste_coord_init()

  2003-01-28	Y.ISHISAKI
	bug fix of fprintf()
	bug fix in aste_hxd_v1_init(), ipin -> igso

  2005-04-25	Y.ISHISAKI	version 1.40
  	bug fix in reading COORDDEF cdef->yunit in aste_coord_init()

  2005-05-25,29	A.BAMBA, Y.ISHISAKI	version 1.50
	support XIS TELDEF VERSION 2, which have PPUX/Y definitions
	add aste_xis_v1_init() -> aste_xis_v1or2_init()
	static COORDDEF coord[5] -> coord[6] in aste_coord_init()
	use aste_telescop() in aste_coord_init() to check TELESCOP keyword
	read CALxx keywords in aste_xrs_v1_init()
	support for teldef="none" for XIS, with aste_xis_none_init()

  2005-06-15	Y.ISHISAKI	version 1.51
	accept both 'XISn' & XIS-n' for XIS

  2005-06-16	Y.ISHISAKI	version 1.52
	use aste_instrume_id() to check INSTRUME keyword

  2005-10-09	Y.ISHISAKI	version 1.70
	DET_ROTD = -180 supported for XIS teldef 20050922 in aste_xis_v1or2_init()
	initialized values for "none" are changed in aste_xis_none_init()

  2006-04-06	Y.ISHISAKI	version 1.73
	search 'FORMAT_VERSION(n)' string in CBD10001 instead of VERSION keyword
************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_coord.h"

#ifndef fits_read_key_int
#define fits_read_key_int(fptr, keyname, value, comment, status)  \
	fits_read_key(fptr, TINT, keyname, value, comment, status)
#endif

static char pname[] = "aste_coord";

static TELDEF *xrs_teldef, *xis_teldef[4], *hxd_teldef;

/************************************************************************
	local functions
************************************************************************/
static int
aste_xrs_v1_init(fitsfile *fp, TELDEF *teldef, char *filename)
{
	long irow, nrow;
	int ihdu, hdutype, pixel, p_col, x_col, y_col, anynul;
	char *key, buf[80], comment[80], flag_pixel[32];
	struct xrs_pixel_map *p_map;
	int istat = 0;
	TELDEF_ASTROE *p = teldef->mission.aste;

	if ( 4 != teldef->ncoords ) {
		fprintf(stderr, "\
%s: NCOORDS=%d not valid for XRS TELDEF ver.1 in '%s'\n",
			pname, teldef->ncoords, filename);
		return -1;
	}

	p->raw = teldef->coord[0];
	p->det = teldef->coord[1];
	p->foc = teldef->coord[2];
	p->sky = teldef->coord[3];

	if (
		fits_read_key_str(fp, key="SEG_COL",   p->seg_col,  comment, &istat) ||
		fits_read_key_int(fp, key="SEG_NUM",  &p->seg_num,  comment, &istat) ||
		fits_read_key_dbl(fp, key="INT_XCEN", &p->int_xcen, comment, &istat) ||
		fits_read_key_dbl(fp, key="INT_YCEN", &p->int_ycen, comment, &istat) ||
		fits_read_key_dbl(fp, key="DET_SCAL", &p->det_scal, comment, &istat) ||
		fits_read_key_dbl(fp, key="DET_XOFF", &p->det_xoff, comment, &istat) ||
		fits_read_key_dbl(fp, key="DET_YOFF", &p->det_yoff, comment, &istat) ||
		fits_read_key_int(fp, key="DETXFLIP", &p->detxflip, comment, &istat) ||
		fits_read_key_int(fp, key="DETYFLIP", &p->detyflip, comment, &istat) ||
		fits_read_key_dbl(fp, key="FOC_XOFF", &p->foc_xoff, comment, &istat) ||
		fits_read_key_dbl(fp, key="FOC_YOFF", &p->foc_yoff, comment, &istat) ||
		fits_read_key_dbl(fp, key="FOC_ROTD", &p->foc_rotd, comment, &istat) ||
		fits_read_key_dbl(fp, key="FOCALLEN", &p->focallen, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM11", &p->alignm11, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM12", &p->alignm12, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM13", &p->alignm13, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM21", &p->alignm21, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM22", &p->alignm22, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM23", &p->alignm23, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM31", &p->alignm31, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM32", &p->alignm32, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM33", &p->alignm33, comment, &istat) ||
		fits_read_key_str(fp, key="SKY_FROM",  p->sky_from, comment, &istat) ||
		fits_read_key_str(fp, key="OPTCOORD",  p->optcoord, comment, &istat) ||
		fits_read_key_dbl(fp, key="OPTAXISX", &p->optaxisx, comment, &istat) ||
		fits_read_key_dbl(fp, key="OPTAXISY", &p->optaxisy, comment, &istat) ||
		0 ) {
		fprintf(stderr, "\
%s: %s keyword not found in '%s' (%d)\n", pname, key, filename, istat);
		return istat;
	}

	if (
		fits_read_key_int(fp, key="CALPIXEL", &p->calpixel, comment, &istat)
		) {
		istat = 0;
		p->calpixel = -1;	/* no CAL pixel */
		p->cal_detx = 0;
		p->cal_dety = 0;
		p->cal_focx = 0;
		p->cal_focy = 0;
		p->cal_x    = 0;
		p->cal_y    = 0;
		p->cal_roll = 0.0;
	} else if (
		fits_read_key_int(fp, key="CAL_DETX", &p->cal_detx, comment, &istat) ||
		fits_read_key_int(fp, key="CAL_DETY", &p->cal_dety, comment, &istat) ||
		fits_read_key_int(fp, key="CAL_FOCX", &p->cal_focx, comment, &istat) ||
		fits_read_key_int(fp, key="CAL_FOCY", &p->cal_focy, comment, &istat) ||
		fits_read_key_int(fp, key="CAL_X",    &p->cal_x,    comment, &istat) ||
		fits_read_key_int(fp, key="CAL_Y",    &p->cal_y,    comment, &istat) ||
		fits_read_key_dbl(fp, key="CAL_ROLL", &p->cal_roll, comment, &istat) ||
		0 ) {
		fprintf(stderr, "\
%s: %s keyword not found in '%s' (%d)\n", pname, key, filename, istat);
		return istat;
	}

	if ( 32 != p->seg_num ) {
		fprintf(stderr, "\
%s: SEG_NUM=%d invalid for XRS in '%s'\n", pname, p->seg_num, filename);
		return -1;
	}

	if ( 1 != p->detxflip ) {
		fprintf(stderr, "\
%s: DETXFLIP=%d invalid for XRS in '%s'\n", pname, p->detxflip, filename);
		return -1;
	}

	if ( -1 != p->detyflip ) {
		fprintf(stderr, "\
%s: DETYFLIP=%d invalid for XRS in '%s'\n", pname, p->detyflip, filename);
		return -1;
	}

	for (ihdu = 1; ; ihdu++) {
		istat = 0;
		fits_movabs_hdu(fp, ihdu, &hdutype, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: PIXEL_MAP extension not found in '%s' (%d)\n", pname, filename, istat);
			return istat;
		}
		if (
			BINARY_TBL == hdutype &&
			0 == fits_read_key_str(fp, "EXTNAME", buf, comment, &istat) &&
			0 == strcmp("PIXEL_MAP", buf) &&
			0 == fits_read_key_str(fp, "TELESCOP", buf, comment, &istat) &&
			0 == strcmp(teldef->telescop, buf) &&
			0 == fits_read_key_str(fp, "INSTRUME", buf, comment, &istat) &&
			0 == strcmp(teldef->instrume, buf) &&
			0 == fits_get_colnum(fp, CASESEN, "PIXEL",  &p_col, &istat) &&
			0 == fits_get_colnum(fp, CASESEN, "PIXELX", &x_col, &istat) &&
			0 == fits_get_colnum(fp, CASESEN, "PIXELY", &y_col, &istat) &&
			0 == fits_read_key_lng(fp, "NAXIS2", &nrow, comment, &istat) ) {
			break;
		}
	}

	if ( sizeof(flag_pixel) != nrow ) {
		fprintf(stderr, "\
%s: NAXIS2=%ld in PIXEL_MAP invalid for XRS in '%s'\n", pname, nrow, filename);
		return -1;
	}
	memset(flag_pixel, 0, sizeof(flag_pixel));

	for (irow = 1; irow <= nrow; irow++) {
		fits_read_col_int(fp, p_col, irow, 1, 1, 0, &pixel, &anynul, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: PIXEL_MAP read error in '%s' (%d)\n", pname, filename, istat);
			return istat;
		}
		if ( pixel < 0 || sizeof(flag_pixel) <= pixel ) {
			fprintf(stderr, "\
%s: PIXEL=%d in PIXEL_MAP invalid for XRS in '%s'\n", pname, pixel, filename);
			return -1;
		}
		if ( flag_pixel[pixel] ) {
			fprintf(stderr, "\
%s: PIXEL=%d in PIXEL_MAP duplicated in '%s'\n", pname, pixel, filename);
			return -1;
		}
		flag_pixel[pixel] = 1;
		p_map = &p->pixel_map[pixel];
		fits_read_col_dbl(fp, x_col, irow, 1, 4, 0, p_map->x, &anynul, &istat);
		fits_read_col_dbl(fp, y_col, irow, 1, 4, 0, p_map->y, &anynul, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: PIXEL_MAP read error in '%s' (%d)\n", pname, filename, istat);
			return istat;
		}
		p_map->xc = (p_map->x[0] + p_map->x[1] + p_map->x[2] + p_map->x[3]) /4;
		p_map->yc = (p_map->y[0] + p_map->y[1] + p_map->y[2] + p_map->y[3]) /4;
	}

	return 0;
}

static void
aste_xis_none_init(int id, TELDEF *teldef)
{
	TELDEF_ASTROE *p = teldef->mission.aste;

	strcpy(teldef->filename, "none");
/* this initialization is based on xisN_teldef_2005-09-22.fits */
	strcpy(teldef->date, "2005-09-22");
	teldef->version = 2;
	teldef->ncoords = 6;
	teldef->id = id;

	p->ppu.xsiz = 260;
	p->ppu.ysiz = 1024;
	p->ppu.xpix1 = 0;
	p->ppu.ypix1 = 0;
	p->ppu.xcen = p->ppu.xpix1 + (p->ppu.xsiz - 1.0) / 2.0;
	p->ppu.ycen = p->ppu.ypix1 + (p->ppu.ysiz - 1.0) / 2.0;
	p->ppu.xscl = 0.024;
	p->ppu.yscl = 0.024;
	strcpy(p->ppu.xcol, "PPUX");
	strcpy(p->ppu.ycol, "PPUY");
	strcpy(p->ppu.xunit, "mm");
	strcpy(p->ppu.yunit, "mm");

	p->seg_num = 4;
	strcpy(p->seg_col, "SEGMENT");
	p->raw.xsiz = 256;
	p->raw.ysiz = 1024;
	p->raw.xpix1 = 0;
	p->raw.ypix1 = 0;
	p->raw.xcen = p->raw.xpix1 + (p->raw.xsiz - 1.0) / 2.0;
	p->raw.ycen = p->raw.ypix1 + (p->raw.ysiz - 1.0) / 2.0;
	p->raw.xscl = 0.024;
	p->raw.yscl = 0.024;
	strcpy(p->raw.xcol, "RAWX");
	strcpy(p->raw.ycol, "RAWY");
	strcpy(p->raw.xunit, "mm");
	strcpy(p->raw.yunit, "mm");
	p->raw_xoff = 2;
	p->raw_yoff = 0;

	p->act.xsiz = 1024;
	p->act.ysiz = 1024;
	p->act.xpix1 = 0;
	p->act.ypix1 = 0;
	p->act.xcen = p->act.xpix1 + (p->act.xsiz - 1.0) / 2.0;
	p->act.ycen = p->act.ypix1 + (p->act.ysiz - 1.0) / 2.0;
	p->act.xscl = 0.024;
	p->act.yscl = 0.024;
	strcpy(p->act.xcol, "ACTX");
	strcpy(p->act.ycol, "ACTY");
	strcpy(p->act.xunit, "mm");
	strcpy(p->act.yunit, "mm");

	p->coe.x[0].a = 0;
	p->coe.x[0].b = 1;
	p->coe.x[0].c = 0;
	p->coe.y[0].a = 0;
	p->coe.y[0].b = 0;
	p->coe.y[0].c = 1;

	p->coe.x[1].a = 511;
	p->coe.x[1].b = -1;
	p->coe.x[1].c = 0;
	p->coe.y[1].a = 0;
	p->coe.y[1].b = 0;
	p->coe.y[1].c = 1;

	p->coe.x[2].a = 512;
	p->coe.x[2].b = 1;
	p->coe.x[2].c = 0;
	p->coe.y[2].a = 0;
	p->coe.y[2].b = 0;
	p->coe.y[2].c = 1;

	p->coe.x[3].a = 1023;
	p->coe.x[3].b = -1;
	p->coe.x[3].c = 0;
	p->coe.y[3].a = 0;
	p->coe.y[3].b = 0;
	p->coe.y[3].c = 1;

	p->actxflip = 1;
	p->actyflip = 1;

	p->det.xsiz = 1024;
	p->det.ysiz = 1024;
	p->det.xpix1 = 1;
	p->det.ypix1 = 1;
	p->det.xcen = p->det.xpix1 + (p->det.xsiz - 1.0) / 2.0;
	p->det.ycen = p->det.ypix1 + (p->det.ysiz - 1.0) / 2.0;
	p->det.xscl = 0.024;
	p->det.yscl = 0.024;
	strcpy(p->det.xcol, "DETX");
	strcpy(p->det.ycol, "DETY");
	strcpy(p->det.xunit, "mm");
	strcpy(p->det.yunit, "mm");

	if ( ASTE_XIS0_ID == id ) {
		p->detxflip = 1;
		p->detyflip = -1;
		p->det_rotd = 180.0;
	} else if ( ASTE_XIS1_ID == id ) {
		p->detxflip = 1;
		p->detyflip = -1;
		p->det_rotd = -90.0;
	} else if ( ASTE_XIS2_ID == id ) {
		p->detxflip = 1;
		p->detyflip = -1;
		p->det_rotd = 90.0;
	} else if ( ASTE_XIS3_ID == id ) {
		p->detxflip = 1;
		p->detyflip = -1;
		p->det_rotd = 180.0;
	}

	if ( 0.0 == p->det_rotd ) {
		p->detx_a = p->det.xpix1 - p->act.xpix1;
		p->detx_b = 1;
		p->detx_c = 0;
		p->dety_a = p->det.ypix1 + p->act.ypix1 + p->det.ysiz - 1;
		p->dety_b = 0;
		p->dety_c = -1;
	} else if ( 90.0 == p->det_rotd ) {
		p->detx_a = p->det.xpix1 + p->act.ypix1 + p->det.xsiz - 1;
		p->detx_b = 0;
		p->detx_c = -1;
		p->dety_a = p->det.ypix1 + p->act.xpix1 + p->det.ysiz - 1;
		p->dety_b = -1;
		p->dety_c = 0;
	} else if ( -90.0 == p->det_rotd ) {
		p->detx_a = p->det.xpix1 - p->act.ypix1;
		p->detx_b = 0;
		p->detx_c = 1;
		p->dety_a = p->det.ypix1 - p->act.xpix1;
		p->dety_b = 1;
		p->dety_c = 0;
	} else if ( 180.0 == p->det_rotd ) {
		p->detx_a = p->det.xpix1 + p->act.xpix1 + p->det.xsiz - 1;
		p->detx_b = -1;
		p->detx_c = 0;
		p->dety_a = p->det.ypix1 - p->act.ypix1;
		p->dety_b = 0;
		p->dety_c = 1;
	}

	p->foc.xsiz = 1536;
	p->foc.ysiz = 1536;
	p->foc.xpix1 = 1;
	p->foc.ypix1 = 1;
	p->foc.xcen = p->foc.xpix1 + (p->foc.xsiz - 1.0) / 2.0;
	p->foc.ycen = p->foc.ypix1 + (p->foc.ysiz - 1.0) / 2.0;
	p->foc.xscl = 0.024;
	p->foc.yscl = 0.024;
	strcpy(p->foc.xcol, "FOCX");
	strcpy(p->foc.ycol, "FOCY");
	strcpy(p->foc.xunit, "mm");
	strcpy(p->foc.yunit, "mm");

	p->foc_xoff = 0.0;
	p->foc_yoff = 0.0;
	p->foc_rotd = 0.0;

	p->sky.xsiz = 1536;
	p->sky.ysiz = 1536;
	p->sky.xpix1 = 1;
	p->sky.ypix1 = 1;
	p->sky.xcen = p->sky.xpix1 + (p->sky.xsiz - 1.0) / 2.0;
	p->sky.ycen = p->sky.ypix1 + (p->sky.ysiz - 1.0) / 2.0;
	p->sky.xscl = 0.024;
	p->sky.yscl = 0.024;
	strcpy(p->sky.xcol, "X");
	strcpy(p->sky.ycol, "Y");
	strcpy(p->sky.xunit, "mm");
	strcpy(p->sky.yunit, "mm");

	p->focallen = 4750.0;
	p->alignm11 = 1.0;
	p->alignm12 = 0.0;
	p->alignm13 = 0.0;
	p->alignm21 = 0.0;
	p->alignm22 = 1.0;
	p->alignm23 = 0.0;
	p->alignm31 = 0.0;
	p->alignm32 = 0.0;
	p->alignm33 = 1.0;
	strcpy(p->sky_from, "FOC");

	strcpy(p->optcoord, "DET");
	p->optaxisx = 512.5;
	p->optaxisy = 512.5;

	teldef->coord[0] = p->ppu;
	teldef->coord[1] = p->raw;
	teldef->coord[2] = p->act;
	teldef->coord[3] = p->det;
	teldef->coord[4] = p->foc;
	teldef->coord[5] = p->sky;
}

static int
aste_xis_v1or2_init(fitsfile *fp, TELDEF *teldef, char *filename)
{
	int iseg;
	char *key, comment[80];

	int istat = 0;
	TELDEF_ASTROE *p = teldef->mission.aste;

	if ( 1 == teldef->version ) {

		if ( 5 != teldef->ncoords ) {
			fprintf(stderr, "\
%s: NCOORDS=%d not valid for XIS TELDEF ver.1 in '%s'\n",
				pname, teldef->ncoords, filename);
			return -1;
		}
		p->raw = teldef->coord[0];
		p->act = teldef->coord[1];
		p->det = teldef->coord[2];
		p->foc = teldef->coord[3];
		p->sky = teldef->coord[4];

/* initialize PPU coord definitions, as a slight derivation of RAW */

		p->ppu = p->raw;
		strcpy(p->ppu.name, "PPU");
		p->ppu.xsiz = p->raw.xsiz + 4;
		p->ppu.xcen = p->raw.xcen + 2;
		strcpy(p->ppu.xcol, "PPUX");
		strcpy(p->ppu.ycol, "PPUY");

/* initialize raw_xoff & raw_yoff to nominal value */

		p->raw_xoff = 2;
		p->raw_yoff = 0;

	}  else if ( 2 == teldef->version ) {

		if ( 6 != teldef->ncoords ) {
			fprintf(stderr, "\
%s: NCOORDS=%d not valid for XIS TELDEF ver.2 in '%s'\n",
				pname, teldef->ncoords, filename);
			return -1;
		}
		p->ppu = teldef->coord[0];
		p->raw = teldef->coord[1];
		p->act = teldef->coord[2];
		p->det = teldef->coord[3];
		p->foc = teldef->coord[4];
		p->sky = teldef->coord[5];

/* read raw_xoff & raw_yoff from TELDEF file */

		if (
		fits_read_key_int(fp, key="RAW_XOFF", &p->raw_xoff, comment, &istat) ||
		fits_read_key_int(fp, key="RAW_YOFF", &p->raw_yoff, comment, &istat) ||
		0 ) {
			fprintf(stderr, "\
%s: %s keyword not found in '%s' (%d)\n", pname, key, filename, istat);
			return istat;
		}

	}

	if (
		fits_read_key_str(fp, key="SEG_COL",   p->seg_col,  comment, &istat) ||
		fits_read_key_int(fp, key="SEG_NUM",  &p->seg_num,  comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_X0_A",&p->coe.x[0].a,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_X0_B",&p->coe.x[0].b,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_X0_C",&p->coe.x[0].c,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_Y0_A",&p->coe.y[0].a,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_Y0_B",&p->coe.y[0].b,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_Y0_C",&p->coe.y[0].c,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_X1_A",&p->coe.x[1].a,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_X1_B",&p->coe.x[1].b,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_X1_C",&p->coe.x[1].c,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_Y1_A",&p->coe.y[1].a,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_Y1_B",&p->coe.y[1].b,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_Y1_C",&p->coe.y[1].c,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_X2_A",&p->coe.x[2].a,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_X2_B",&p->coe.x[2].b,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_X2_C",&p->coe.x[2].c,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_Y2_A",&p->coe.y[2].a,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_Y2_B",&p->coe.y[2].b,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_Y2_C",&p->coe.y[2].c,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_X3_A",&p->coe.x[3].a,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_X3_B",&p->coe.x[3].b,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_X3_C",&p->coe.x[3].c,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_Y3_A",&p->coe.y[3].a,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_Y3_B",&p->coe.y[3].b,comment, &istat) ||
		fits_read_key_dbl(fp, key="COE_Y3_C",&p->coe.y[3].c,comment, &istat) ||
		fits_read_key_int(fp, key="ACTXFLIP", &p->actxflip, comment, &istat) ||
		fits_read_key_int(fp, key="ACTYFLIP", &p->actyflip, comment, &istat) ||
		fits_read_key_int(fp, key="DETXFLIP", &p->detxflip, comment, &istat) ||
		fits_read_key_int(fp, key="DETYFLIP", &p->detyflip, comment, &istat) ||
		fits_read_key_dbl(fp, key="DET_ROTD", &p->det_rotd, comment, &istat) ||
		fits_read_key_dbl(fp, key="FOC_XOFF", &p->foc_xoff, comment, &istat) ||
		fits_read_key_dbl(fp, key="FOC_YOFF", &p->foc_yoff, comment, &istat) ||
		fits_read_key_dbl(fp, key="FOC_ROTD", &p->foc_rotd, comment, &istat) ||
		fits_read_key_dbl(fp, key="FOCALLEN", &p->focallen, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM11", &p->alignm11, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM12", &p->alignm12, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM13", &p->alignm13, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM21", &p->alignm21, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM22", &p->alignm22, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM23", &p->alignm23, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM31", &p->alignm31, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM32", &p->alignm32, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM33", &p->alignm33, comment, &istat) ||
		fits_read_key_str(fp, key="SKY_FROM",  p->sky_from, comment, &istat) ||
		fits_read_key_str(fp, key="OPTCOORD",  p->optcoord, comment, &istat) ||
		fits_read_key_dbl(fp, key="OPTAXISX", &p->optaxisx, comment, &istat) ||
		fits_read_key_dbl(fp, key="OPTAXISY", &p->optaxisy, comment, &istat) ||
		0 ) {
		fprintf(stderr, "\
%s: %s keyword not found in '%s' (%d)\n", pname, key, filename, istat);
		return istat;
	}

	if ( 4 != p->seg_num ) {
		fprintf(stderr, "\
%s: SEG_NUM=%d invalid for XIS in '%s'\n", pname, p->seg_num, filename);
		return -1;
	}

	for (iseg = 0; iseg < p->seg_num; iseg++) {
		struct xis_coefficient_xy coe_x = p->coe.x[iseg];
		struct xis_coefficient_xy coe_y = p->coe.y[iseg];

		if ( 0 != coe_x.c ) {
			fprintf(stderr, "\
%s: COE_X%d_C=%.1f invalid for XIS in '%s'\n", pname, iseg, coe_x.c, filename);
			return -1;
		}
		if ( 0 != coe_y.a ) {
			fprintf(stderr, "\
%s: COE_Y%d_A=%.1f invalid for XIS in '%s'\n", pname, iseg, coe_y.a, filename);
			return -1;
		}
		if ( 0 != coe_y.b ) {
			fprintf(stderr, "\
%s: COE_Y%d_B=%.1f invalid for XIS in '%s'\n", pname, iseg, coe_y.b, filename);
			return -1;
		}
		if ( 1 != coe_y.c ) {
			fprintf(stderr, "\
%s: COE_Y%d_C=%.1f invalid for XIS in '%s'\n", pname, iseg, coe_y.c, filename);
			return -1;
		}
	}

	if ( 1 != p->detxflip ) {
		fprintf(stderr, "\
%s: DETXFLIP=%d invalid for XIS in '%s'\n", pname, p->detxflip, filename);
		return -1;
	}

	if ( -1 != p->detyflip ) {
		fprintf(stderr, "\
%s: DETYFLIP=%d invalid for XIS in '%s'\n", pname, p->detyflip, filename);
		return -1;
	}

	if ( p->det.xsiz != p->act.xsiz ) {
		fprintf(stderr, "\
%s: ACT_XSIZ != DET_XSIZ invalid for XIS in '%s'\n", pname, filename);
		return -1;
	}

	if ( p->det.ysiz != p->act.ysiz ) {
		fprintf(stderr, "\
%s: ACT_XSIZ != DET_XSIZ invalid for XIS in '%s'\n", pname, filename);
		return -1;
	}

	if ( p->det.xsiz != p->det.ysiz ) {
		fprintf(stderr, "\
%s: DET_XSIZ != DET_YSIZ invalid for XIS in '%s'\n", pname, filename);
		return -1;
	}

	if ( 0.0 == p->det_rotd ) {
		p->detx_a = p->det.xpix1 - p->act.xpix1;
		p->detx_b = 1;
		p->detx_c = 0;
		p->dety_a = p->det.ypix1 + p->act.ypix1 + p->det.ysiz - 1;
		p->dety_b = 0;
		p->dety_c = -1;
	} else if ( 90.0 == p->det_rotd ) {
		p->detx_a = p->det.xpix1 + p->act.ypix1 + p->det.xsiz - 1;
		p->detx_b = 0;
		p->detx_c = -1;
		p->dety_a = p->det.ypix1 + p->act.xpix1 + p->det.ysiz - 1;
		p->dety_b = -1;
		p->dety_c = 0;
	} else if ( -90.0 == p->det_rotd ) {
		p->detx_a = p->det.xpix1 - p->act.ypix1;
		p->detx_b = 0;
		p->detx_c = 1;
		p->dety_a = p->det.ypix1 - p->act.xpix1;
		p->dety_b = 1;
		p->dety_c = 0;
	} else if ( 180.0 == p->det_rotd ) {
		p->detx_a = p->det.xpix1 + p->act.xpix1 + p->det.xsiz - 1;
		p->detx_b = -1;
		p->detx_c = 0;
		p->dety_a = p->det.ypix1 - p->act.ypix1;
		p->dety_b = 0;
		p->dety_c = 1;
	} else {
		fprintf(stderr, "\
%s: DET_ROTD=%.1f invalid for XIS in '%s'\n", pname, p->det_rotd, filename);
		return -1;
	}

	return 0;
}

static int
aste_hxd_v1_init(fitsfile *fp, TELDEF *teldef, char *filename)
{
	long irow, nrow;
	int ihdu, hdutype, det_col, x_col, y_col, anynul, ipin, igso;
	char *key, buf[80], comment[80], flag_pin[64], flag_gso[16], detector[8];
	double intx, inty;
	int istat = 0;
	TELDEF_ASTROE *p = teldef->mission.aste;

	if ( 4 != teldef->ncoords ) {
		fprintf(stderr, "\
%s: NCOORDS=%d not valid for HXD TELDEF ver.1 in '%s'\n",
			pname, teldef->ncoords, filename);
		return -1;
	}

	p->raw = teldef->coord[0];
	p->det = teldef->coord[1];
	p->foc = teldef->coord[2];
	p->sky = teldef->coord[3];

	if (
		fits_read_key_str(fp, key="SEG_COL",   p->seg_col,  comment, &istat) ||
		fits_read_key_int(fp, key="SEG_NUM",  &p->seg_num,  comment, &istat) ||
		fits_read_key_dbl(fp, key="INT_XCEN", &p->int_xcen, comment, &istat) ||
		fits_read_key_dbl(fp, key="INT_YCEN", &p->int_ycen, comment, &istat) ||
		fits_read_key_dbl(fp, key="DET_SCAL", &p->det_scal, comment, &istat) ||
		fits_read_key_dbl(fp, key="DET_XOFF", &p->det_xoff, comment, &istat) ||
		fits_read_key_dbl(fp, key="DET_YOFF", &p->det_yoff, comment, &istat) ||
		fits_read_key_int(fp, key="DETXFLIP", &p->detxflip, comment, &istat) ||
		fits_read_key_int(fp, key="DETYFLIP", &p->detyflip, comment, &istat) ||
		fits_read_key_dbl(fp, key="FOC_XOFF", &p->foc_xoff, comment, &istat) ||
		fits_read_key_dbl(fp, key="FOC_YOFF", &p->foc_yoff, comment, &istat) ||
		fits_read_key_dbl(fp, key="FOC_ROTD", &p->foc_rotd, comment, &istat) ||
		fits_read_key_dbl(fp, key="FOCALLEN", &p->focallen, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM11", &p->alignm11, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM12", &p->alignm12, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM13", &p->alignm13, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM21", &p->alignm21, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM22", &p->alignm22, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM23", &p->alignm23, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM31", &p->alignm31, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM32", &p->alignm32, comment, &istat) ||
		fits_read_key_dbl(fp, key="ALIGNM33", &p->alignm33, comment, &istat) ||
		fits_read_key_str(fp, key="SKY_FROM",  p->sky_from, comment, &istat) ||
		fits_read_key_str(fp, key="OPTCOORD",  p->optcoord, comment, &istat) ||
		fits_read_key_dbl(fp, key="OPTAXISX", &p->optaxisx, comment, &istat) ||
		fits_read_key_dbl(fp, key="OPTAXISY", &p->optaxisy, comment, &istat) ||
		0 ) {
		fprintf(stderr, "\
%s: %s keyword not found in '%s' (%d)\n", pname, key, filename, istat);
		return istat;
	}

	if ( 80 != p->seg_num ) {
		fprintf(stderr, "\
%s: SEG_NUM=%d invalid for HXD in '%s'\n", pname, p->seg_num, filename);
		return -1;
	}

	for (ihdu = 1; ; ihdu++) {
		istat = 0;
		fits_movabs_hdu(fp, ihdu, &hdutype, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: HXD_ALIGNMENT extension not found in '%s' (%d)\n", pname, filename, istat);
			return istat;
		}
		if (
			BINARY_TBL == hdutype &&
			0 == fits_read_key_str(fp, "EXTNAME", buf, comment, &istat) &&
			0 == strcmp("HXD_ALIGNMENT", buf) &&
			0 == fits_read_key_str(fp, "TELESCOP", buf, comment, &istat) &&
			0 == strcmp(teldef->telescop, buf) &&
			0 == fits_read_key_str(fp, "INSTRUME", buf, comment, &istat) &&
			0 == strcmp(teldef->instrume, buf) &&
			0 == fits_get_colnum(fp, CASESEN, "DETECTOR", &det_col, &istat) &&
			0 == fits_get_colnum(fp, CASESEN, "INTX", &x_col, &istat) &&
			0 == fits_get_colnum(fp, CASESEN, "INTY", &y_col, &istat) &&
			0 == fits_read_key_lng(fp, "NAXIS2", &nrow, comment, &istat) ) {
			break;
		}
	}

	if ( sizeof(flag_gso) + sizeof(flag_pin) != nrow ) {
		fprintf(stderr, "\
%s: NAXIS2=%ld in HXD_ALIGNMENT invalid in '%s'\n", pname, nrow, filename);
		return -1;
	}
	memset(flag_gso, 0, sizeof(flag_gso));
	memset(flag_pin, 0, sizeof(flag_pin));

	for (irow = 1; irow <= nrow; irow++) {
		char *s = detector;
		fits_read_col_str(fp,det_col, irow, 1, 1, "", &s, &anynul, &istat);
		fits_read_col_dbl(fp,x_col, irow, 1, 1, 0, &intx, &anynul, &istat);
		fits_read_col_dbl(fp,y_col, irow, 1, 1, 0, &inty, &anynul, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: HXD_ALIGNMENT read error in '%s' (%d)\n", pname, filename, istat);
			return istat;
		}
		if ( 0 == strncmp("GSO", detector, 3) ) {
			igso = atoi(detector+3);
			if ( igso < 0 || sizeof(flag_gso) <= igso ) {
				fprintf(stderr, "\
%s: %s invalid in '%s'\n", pname, detector, filename);
				return -1;
			}
			if ( flag_gso[igso] ) {
				fprintf(stderr, "\
%s: %s duplicated in '%s'\n", pname, detector, filename);
				return -1;
			}
			flag_gso[igso] = 1;
			p->gso[igso].intx = intx;
			p->gso[igso].inty = inty;
		}
		if ( 0 == strncmp("PIN", detector, 3) ) {
			ipin = atoi(detector+3);
			if ( ipin < 0 || sizeof(flag_pin) <= ipin ) {
				fprintf(stderr, "\
%s: %s invalid in '%s'\n", pname, detector, filename);
				return -1;
			}
			if ( flag_pin[ipin] ) {
				fprintf(stderr, "\
%s: %s duplicated in '%s'\n", pname, detector, filename);
				return -1;
			}
			flag_pin[ipin] = 1;
			p->pin[ipin].intx = intx;
			p->pin[ipin].inty = inty;
		}
	}

	return 0;
}

int
match_name(char *name, char *pattern)
{
	if ( NULL == pattern || '\0' == *pattern || 0 == strcmp("*", pattern) ) {
		return 0;
	}
	return strcmp(name, pattern);
}

/************************************************************************
  aste_coord_init	aste_coord initialization function

  INPUT
	char *telescop
		telescope name, which must match TELESCOP header keyword
		in the specified teldef file.
		If NULL or "" or "*", first occurence is used.
	char *instrume
		instrument name, which must match INSTRUME header keyword
		in the specified teldef file.
		If NULL or "" or "*", first occurence is used.
	char *filename
		teldef file name to read

  OUTPUT
	TELDEF *aste_coord_init
		allocated TELDEF structure.
		If this function fails, it returns NULL pointer and set errno
		(basically CFITSIO error number) to indicate the error
************************************************************************/
TELDEF *
aste_coord_init(char *telescop, char *instrume, char *filename)
{
/* temporary buffer to read teldef file */
	static TELDEF teldef;
	static COORDDEF coord[6];	/* 'PPU', 'RAW', 'ACT', 'DET', 'FOC', 'SKY' */
	static TELDEF_ASTROE aste;

	TELDEF *teldef_ptr;
	COORDDEF *cdef;
	fitsfile *fp;
	char *p, *name, key[64], value[80], comment[80];
	int i, id, version, istat, hdutype, align;

/* clear temporary TELDEF structure */

	memset(&teldef, 0, sizeof(teldef));
	memset(coord, 0, sizeof(coord));
	memset(&aste, 0, sizeof(aste));
	teldef.coord = coord;
	teldef.mission.aste = &aste;

/* check for special filename "none" for XIS */
	if ( 0 == strcmp("none", filename) ) {
		if ( 0 == match_name("ASTRO-E", telescop) ||
			 0 == match_name("Astro-E2", telescop) ||
			 0 == match_name(aste_telescop(), telescop) ) {
			if ( NULL != instrume ) {
				id = aste_instrume_id(instrume);
				if ( ASTE_XIS0_ID <= id && id <= ASTE_XIS3_ID ) {
					strcpy(teldef.telescop, aste_telescop());
					strcpy(teldef.instrume, instrume);
					aste_xis_none_init(id, &teldef);
					goto skip;
				}
			}
		}
	}

/* open teldef file */

	istat = 0;
	fits_open_file(&fp, filename, READONLY, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_open_file('%s') failed (%d)\n", pname, filename, istat);
		errno = istat;
		return NULL;
	}

/* read TELESCOP & INSTRUME keywords */

	for (;;) {
		istat = 0;
		fits_read_key_str(fp, "TELESCOP", teldef.telescop, comment, &istat);
		fits_read_key_str(fp, "INSTRUME", teldef.instrume, comment, &istat);
		if ( 0 == istat &&
			 0 == match_name(teldef.telescop, telescop) &&
			 0 == match_name(teldef.instrume, instrume) ) {
			break;
		}
		istat = 0;
		fits_movrel_hdu(fp, 1, &hdutype, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: TELESCOP and INSTRUME keywords not found in '%s' (%d)\n",
					pname, filename, istat);
			errno = istat;
			fits_close_file(fp, &istat);
			return NULL;
		}
	}

/* check whether telescop & instrume is supported by this function */

	if ( 0 != strcmp("ASTRO-E", teldef.telescop) &&
		 0 != strcmp("Astro-E2", teldef.telescop) &&
		 0 != strcmp(aste_telescop(), teldef.telescop) ) {
		fprintf(stderr, "\
%s: TELESCOP='%s' not supported\n", pname, teldef.telescop);
		errno = -1;
		return NULL;
	}

	teldef.id = aste_instrume_id(teldef.instrume);
	if ( ASTE_XRS_ID != teldef.id && ASTE_HXD_ID != teldef.id &&
		 (teldef.id < ASTE_XIS0_ID || ASTE_XIS3_ID < teldef.id ) ) {
		fprintf(stderr, "\
%s: INSTRUME='%s' not supported\n", pname, teldef.instrume);
		errno = -1;
		return NULL;
	}

/* read other general keywords */

	fits_read_key_str(fp, "FILENAME", teldef.filename, comment, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: FILENAME keyword not found in '%s' (%d), ignored\n",
				pname, filename, istat);
		istat = 0;
		*teldef.filename = '\0';
	}

	fits_read_key_str(fp, "DATE", teldef.date, comment, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: DATE keyword not found in '%s' (%d), ignored\n",
				pname, filename, istat);
		istat = 0;
		*teldef.date = '\0';
	}

	{
		static char key[] = "CBD10001";
		static char f[] = "FORMAT_VERSION(";
		static int len = sizeof(f) - 1;

		fits_read_key_str(fp, key, value, comment, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: %s keyword not found in '%s' (%d)\n", pname, key, filename, istat);
			errno = istat;
			fits_close_file(fp, &istat);
			return NULL;
		}
		if ( 0 != strncmp(value, f, len) ||
			 1 != sscanf(value+len, "%d", &teldef.version) ) {
			fprintf(stderr, "\
%s: 'FORMAT_VERSION(n)' not found in %s keyword\n", pname, key);
			errno = -1;
			fits_close_file(fp, &istat);
			return NULL;
		}
	}

/* read coordinates definitions */

	fits_read_key_int(fp, "NCOORDS", &teldef.ncoords, comment, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: NCOORDS keyword not found in '%s' (%d)\n", pname, filename, istat);
		errno = istat;
		fits_close_file(fp, &istat);
		return NULL;
	}
	if ( sizeof(coord)/sizeof(*coord) < teldef.ncoords ) {
		fprintf(stderr, "\
%s: NCOORDS=%d too many in '%s'\n", pname, teldef.ncoords, filename);
		errno = istat;
		fits_close_file(fp, &istat);
		return NULL;
	}
	for (i = 0; i < teldef.ncoords; i++) {
		cdef = &coord[i];
		sprintf(key, "COORD%d", i);
		fits_read_key_str(fp, key, cdef->name, comment, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: %s keyword not found in '%s' (%d)\n", pname, key, filename, istat);
			errno = istat;
			fits_close_file(fp, &istat);
			return NULL;
		}
	}

	for (i = 0; i < teldef.ncoords; i++) {
		cdef = &coord[i];
		name = cdef->name;
		sprintf(key, "%s_XSIZ", name);
		fits_read_key_int(fp, key, &cdef->xsiz, comment, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: %s keyword not found in '%s' (%d)\n", pname, key, filename, istat);
			errno = istat;
			fits_close_file(fp, &istat);
			return NULL;
		}
		sprintf(key, "%s_YSIZ", name);
		fits_read_key_int(fp, key, &cdef->ysiz, comment, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: %s keyword not found in '%s' (%d)\n", pname, key, filename, istat);
			errno = istat;
			fits_close_file(fp, &istat);
			return NULL;
		}
		sprintf(key, "%sXPIX1", name);
		fits_read_key_int(fp, key, &cdef->xpix1, comment, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: %s keyword not found in '%s' (%d)\n", pname, key, filename, istat);
			errno = istat;
			fits_close_file(fp, &istat);
			return NULL;
		}
		sprintf(key, "%sYPIX1", name);
		fits_read_key_int(fp, key, &cdef->ypix1, comment, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: %s keyword not found in '%s' (%d)\n", pname, key, filename, istat);
			errno = istat;
			fits_close_file(fp, &istat);
			return NULL;
		}
		sprintf(key, "%s_XCEN", name);
		fits_read_key_dbl(fp, key, &cdef->xcen, comment, &istat);
		if ( istat ) {
			istat = 0;
			cdef->xcen = cdef->xpix1 + (cdef->xsiz - 1.0) / 2.0;
		}
		sprintf(key, "%s_YCEN", name);
		fits_read_key_dbl(fp, key, &cdef->ycen, comment, &istat);
		if ( istat ) {
			istat = 0;
			cdef->ycen = cdef->ypix1 + (cdef->ysiz - 1.0) / 2.0;
		}
		sprintf(key, "%s_XSCL", name);
		fits_read_key_dbl(fp, key, &cdef->xscl, comment, &istat);
		if ( istat ) {
			istat = 0;		/* assume no scaling */
			cdef->xscl = 1.0;
		}
		sprintf(key, "%s_YSCL", name);
		fits_read_key_dbl(fp, key, &cdef->yscl, comment, &istat);
		if ( istat ) {
			istat = 0;		/* assume no scaling */
			cdef->yscl = 1.0;
		}
		sprintf(key, "%s_XCOL", name);
		fits_read_key_str(fp, key, cdef->xcol, comment, &istat);
		if ( istat ) {
			istat = 0;		/* assume default definition */
			sprintf(cdef->xcol, "%sX", 0 == strcmp("SKY", name) ? "" : name);
		}
		sprintf(key, "%s_YCOL", name);
		fits_read_key_str(fp, key, cdef->ycol, comment, &istat);
		if ( istat ) {
			istat = 0;		/* assume default definition */
			sprintf(cdef->ycol, "%sY", 0 == strcmp("SKY", name) ? "" : name);
		}
		sprintf(key, "%sXUNIT", name);
		fits_read_key_str(fp, key, cdef->xunit, comment, &istat);
		if ( istat ) {
			istat = 0;		/* assume no unit */
			*cdef->xunit = '\0';
		}
		sprintf(key, "%sYUNIT", name);
		fits_read_key_str(fp, key, cdef->yunit, comment, &istat);
		if ( istat ) {
			istat = 0;		/* assume no unit */
			*cdef->yunit = '\0';
		}
	}

/* begin setting ASTRO-E specific teldef structure */

	id = teldef.id;
	version = teldef.version;

	if ( ASTE_XIS0_ID <= id && id <= ASTE_XIS3_ID &&
		 (1 == version || 2 == version) ) {
		istat = aste_xis_v1or2_init(fp, &teldef, filename);
	} else if ( ASTE_XRS_ID == id && 1 == version ) {
		istat = aste_xrs_v1_init(fp, &teldef, filename);
	} else if ( ASTE_HXD_ID == id && 1 == version ) {
		istat = aste_hxd_v1_init(fp, &teldef, filename);
	} else {
		fprintf(stderr, "\
%s: '%s' '%s' TELDEF version %d not supported in '%s'\n",
				pname, teldef.telescop, teldef.instrume, version, filename);
		errno = -1;
		fits_close_file(fp, &istat);
		return NULL;
	}

	if ( istat ) {
		errno = istat;
		fits_close_file(fp, &istat);
		return NULL;
	}

	fits_close_file(fp, &istat);

 skip:

/* check & calculate several values in advance */

	if ( 0.0 == aste.det.xscl ) {
		fprintf(stderr, "\
%s: DET_XSCL=0.0 must be non-zero in '%s'\n", pname, filename);
		errno = -1;
		return NULL;
	}

	if ( 0.0 == aste.det.yscl ) {
		fprintf(stderr, "\
%s: DET_YSCL=0.0 must be non-zero in '%s'\n", pname, filename);
		errno = -1;
		return NULL;
	}

	if ( 0.0 == aste.focallen ) {
		fprintf(stderr, "\
%s: FOCALLEN=0.0 must be non-zero in '%s'\n", pname, filename);
		errno = -1;
		return NULL;
	}

	if ( 0.0 == aste.foc.xscl ) {
		fprintf(stderr, "\
%s: FOC_XSCL=0.0 must be non-zero in '%s'\n", pname, filename);
		errno = -1;
		return NULL;
	}

	if ( 0.0 == aste.foc.yscl ) {
		fprintf(stderr, "\
%s: FOC_YSCL=0.0 must be non-zero in '%s'\n", pname, filename);
		errno = -1;
		return NULL;
	}

	if ( 0.0 == aste.sky.xscl ) {
		fprintf(stderr, "\
%s: SKY_XSCL=0.0 must be non-zero in '%s'\n", pname, filename);
		errno = -1;
		return NULL;
	}

	if ( 0.0 == aste.sky.yscl ) {
		fprintf(stderr, "\
%s: SKY_YSCL=0.0 must be non-zero in '%s'\n", pname, filename);
		errno = -1;
		return NULL;
	}

	if ( 0 != strcmp("FOC", aste.sky_from) ) {
		fprintf(stderr, "\
%s: SKY_FROM='%s' must be 'FOC' in '%s'\n", pname, aste.sky_from, filename);
		errno = -1;
		return NULL;
	}

	if ( 0 != strcmp("DET", aste.optcoord) ) {
		fprintf(stderr, "\
%s: OPTCOORD='%s' must be 'DET' in '%s'\n", pname, aste.optcoord, filename);
		errno = -1;
		return NULL;
	}

	aste.Mij[0][0] = aste.alignm11;
	aste.Mij[0][1] = aste.alignm12;
	aste.Mij[0][2] = aste.alignm13;
	aste.Mij[1][0] = aste.alignm21;
	aste.Mij[1][1] = aste.alignm22;
	aste.Mij[1][2] = aste.alignm23;
	aste.Mij[2][0] = aste.alignm31;
	aste.Mij[2][1] = aste.alignm32;
	aste.Mij[2][2] = aste.alignm33;
	istat = atInvRotMat(aste.Mij, aste.invMij);

	if ( istat ) {
		fprintf(stderr, "\
%s: strange alignment matrix in '%s'\n", pname, filename);
		errno = -1;
		return NULL;
	}

	aste.cos_foc_rotd = cos(aste.foc_rotd);
	aste.sin_foc_rotd = sin(aste.foc_rotd);

/* allocate memory and copy temporary buffer into it */

	align = sizeof(double);	/* check floating point alignment for Solaris */
	p = malloc( (sizeof(teldef) + align - 1) / align * align +
				teldef.ncoords * sizeof(*coord) +
				sizeof(aste) +
				strlen(filename) + 1 );

	if ( NULL == p ) {
		fprintf(stderr, "\
%s: malloc() faild for TELDEF in '%s'\n", pname, filename);
		errno = -1;
		return NULL;
	}

	teldef_ptr = (void *)p;
	teldef.coord = (void *)(p + (sizeof(teldef) + align - 1) / align * align);
	teldef.mission.aste = (void *)&teldef.coord[teldef.ncoords];
	teldef.actual_filename = (void *)&teldef.mission.aste[1];

	*teldef_ptr = teldef;
	memcpy(teldef.coord, coord, teldef.ncoords * sizeof(*coord));
	*teldef.mission.aste = aste;
	strcpy(teldef.actual_filename, filename);

	if ( ASTE_XRS_ID == id ) {
		xrs_teldef = teldef_ptr;
	} else if ( ASTE_XIS0_ID <= id && id <= ASTE_XIS3_ID ) {
		xis_teldef[id - ASTE_XIS0_ID] = teldef_ptr;
	} else if ( ASTE_HXD_ID == id ) {
		hxd_teldef = teldef_ptr;
	}

	return teldef_ptr;
}

/************************************************************************
  aste_coord_free	free allocated memory for TELDEF structure
************************************************************************/
int
aste_coord_free(TELDEF *teldef)
{
	int id;

	if ( NULL == teldef ) {
		return ASTE_COORD_NOT_INITIALIZED;
	}

	id = teldef->id;

	if ( ASTE_XRS_ID == id ) {
		xrs_teldef = NULL;
	} else if ( ASTE_XIS0_ID <= id && id <= ASTE_XIS3_ID ) {
		xis_teldef[id - ASTE_XIS0_ID] = NULL;
	} else if ( ASTE_HXD_ID == id ) {
		hxd_teldef = NULL;
	} else {
		fprintf(stderr, "\
%s: unknown sensor ID=%d\n", pname, id);
		return ASTE_COORD_INVALID_SENSOR;
	}

	free(teldef);

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_coord_teldef	return already allocated TELDEF structure

  INPUT
	char *telescop
		telescope name, which must match TELESCOP header keyword in TELDEF.
		If NULL or "" or "*", first occurence is used.
	char *instrume
		instrument name, which must match INSTRUME header keyword in TELDEF.
		If NULL or "" or "*", first occurence is used.

  OUTPUT
	TELDEF *aste_coord_teldef
		already allocated TELDEF structure.
		If no TELDEF is found, it returns NULL.
************************************************************************/
TELDEF *
aste_coord_teldef(char *telescop, char *instrume)
{
	int i;
	TELDEF *teldef[6];

	teldef[0] = xis_teldef[0];
	teldef[1] = xis_teldef[1];
	teldef[2] = xis_teldef[2];
	teldef[3] = xis_teldef[3];
	teldef[4] = xrs_teldef;
	teldef[5] = hxd_teldef;

	for (i = 0; i < sizeof(teldef)/sizeof(*teldef); i++) {
		if ( NULL != teldef[i] &&
			 0 == match_name(teldef[i]->telescop, telescop) &&
			 0 == match_name(teldef[i]->instrume, instrume) ) {
			return teldef[i];
		}
	}

	return NULL;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
