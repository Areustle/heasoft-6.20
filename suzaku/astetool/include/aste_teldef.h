/* $Id: aste_teldef.h,v 1.12 2007/05/07 16:45:31 ishisaki Exp $ */
/* aste_teldef.h

	1999-12-19	Y.ISHISAKI	version 1.0

	2005-05-26	Y.ISHISAKI	version 1.50
		add PPUX/Y definitions & raw_x/yoff for XIS
		add cal** definitions for XRS
*/

#ifndef _ASTE_TELDEF_H_
#define _ASTE_TELDEF_H_

/* teldef definition of ASTRO-E */
typedef struct teldef_astroe {
/* copy of each coordinates */
	COORDDEF ppu;		/* XRS: none, XIS: 'PPUX'-'PPUY' */
	COORDDEF raw;		/* XRS: none, XIS: 'RAWX'-'RAWY' */
	COORDDEF act;		/* XRS: none, XIS: 'ACTX'-'ACTY' */
	COORDDEF det;		/* 'DETX'-'DETY' */
	COORDDEF foc;		/* 'FOCX'-'FOCY' */
	COORDDEF sky;		/* 'X'-'Y' */

/* definition of PPU -> RAW transformation, copy of teldef header */
	int raw_xoff;		/* [pixel] */
	int raw_yoff;		/* [pixel] */

/* miscellaneous definition of the RAW coordinates */
	char seg_col[64];	/* segment column name, 'PIXEL' or 'SEGMENT' */
	int seg_num;		/* number of segments */

/* definition of RAW -> ACT transformation, copy of teldef header */
	struct xis_coefficient {
		struct xis_coefficient_xy {
			double a, b, c;
		} x[4], y[4];
	} coe;
	int actxflip;		/* 1 or -1 */
	int actyflip;		/* 1 or -1 */

/* definition of ACT -> DET transformation, copy of teldef header */
	double int_xcen;
	double int_ycen;
	double det_scal;
	double det_xoff;
	double det_yoff;
	double det_rotd;	/* [deg] */
	int detxflip;		/* 1 or -1 */
	int detyflip;		/* 1 or -1 */

/* definition of DET -> FOC transformation, copy of teldef header */
	double foc_xoff;	/* [mm] */
	double foc_yoff;	/* [mm] */
	double foc_rotd;	/* [deg] */

/* definition of FOC -> SKY transformation, copy of teldef header */
	char sky_from[64];	/* must be 'FOC' */
	double focallen;	/* [mm], defines mm -> arcmin conversion */
	double alignm11, alignm12, alignm13;
	double alignm21, alignm22, alignm23;
	double alignm31, alignm32, alignm33;

/* definition of the optical axis, copy of teldef header */
	char optcoord[64];	/* must be 'DET' */
	double optaxisx;	/* [ch] */
	double optaxisy;	/* [ch] */

/* below is only for internal use */
	AtRotMat Mij, invMij;	/* alignment matrix & inversion of it */
	double cos_foc_rotd;	/* cos(foc_rotd) */
	double sin_foc_rotd;	/* sin(foc_rotd) */

	/* for XRS pixel map */
	struct xrs_pixel_map {
		/* [mm], lower left, upper right, upper left, lower left */
		double x[4], y[4];
		double xc, yc;	/* center */
	} pixel_map[32];
	int calpixel;
	int cal_detx, cal_dety;
	int cal_focx, cal_focy;
	int cal_x, cal_y;
	double cal_roll;
	/* XIS ACT -> DET */
	int detx_a, detx_b, detx_c;
	int dety_a, dety_b, dety_c;
	/* for HXD */
	struct gso_alignment {
		double intx;
		double inty;
	} gso[16];
	struct pin_alignment {
		double intx;
		double inty;
	} pin[64];
} TELDEF_ASTROE;

#endif
