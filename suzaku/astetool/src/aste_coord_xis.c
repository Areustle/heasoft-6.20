/* aste_coord_xis.c,v 1.1 1999/12/20 03:45:08 ishisaki Exp */
/************************************************************************
  aste_coord_xis.c

  xis_ppu2raw	convert XIS PPUX/Y[ch] -> RAWX/Y[ch]
  xis_raw2ppu	convert XIS RAWX/Y[ch] -> PPUX/Y[ch]
  xis_raw2act	convert XIS RAWX/Y[ch] -> ACTX/Y[ch]
  xis_act2raw	convert XIS ACTX/Y[ch] -> RAWX/Y[ch]
  xis_act2det	convert XIS ACTX/Y[ch] -> DETX/Y[ch]
  xis_det2act	convert XIS DETX/Y[ch] -> ACTX/Y[ch]

  1999-12-19	Y.ISHISAKI

  2003-01-28	Y.ISHISAKI
	bug fix of fprintf(), in xis_act2raw(), etc

  2005-05-25	A.BAMBA, Y.ISHISAKI		version 1.50
	support XIS TELDEF VERSION 2, which have PPUX/Y definitions
	add xis_ppu2raw(), xis_raw2ppu()
	accept -2<=RAWX<=257, check ACTX range in xis_raw2act()

************************************************************************/

#include <stdio.h>
#include <math.h>
#include "atFunctions.h"
#include "aste_coord.h"

#define	XISwindowOff	0
#define	XISwindow4		1
#define	XISwindow8		2
#define	XISwindow16		3

/************************************************************************
  xis_ppu2raw	convert XIS PPUX/Y[ch] -> RAWX/Y[ch]
************************************************************************/
int
xis_ppu2raw(
    TELDEF *teldef, /* input: teldef file */
    int ppux,       /* input: PPUX [0-259] */
    int ppuy,       /* input: PPUY [0-1023] */
    int *rawx,      /* output: RAWX [-2-257] */
    int *rawy       /* output: RAWY [0-1023] */
)
{
    static char pname[] = "xis_ppu2raw";

	TELDEF_ASTROE *p = teldef->mission.aste;

	if ( teldef->id < ASTE_XIS0_SENSOR || ASTE_XIS3_SENSOR < teldef->id ) {
		fprintf(stderr, "\
%s: TELDEF is not XIS (%d)\n", pname, teldef->id);
		return ASTE_COORD_INVALID_SENSOR;
	}

	if ( ppux < p->ppu.xpix1 || p->ppu.xpix1 + p->ppu.xsiz <= ppux ) {
		fprintf(stderr, "\
%s: illegal PPUX value -- %d\n", pname, ppux);
		return ASTE_COORD_INVALID_PIXEL;
	}

	if ( ppuy < p->ppu.ypix1 || p->ppu.ypix1 + p->ppu.ysiz <= ppuy ) {
		fprintf(stderr, "\
%s: illegal PPUY value -- %d\n", pname, ppuy);
		return ASTE_COORD_INVALID_PIXEL;
	}

	*rawx = ppux - p->raw_xoff;
	*rawy = ppuy - p->raw_yoff;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  xis_raw2ppu	convert XIS RAWX/Y[ch] -> PPUX/Y[ch]
************************************************************************/
int
xis_raw2ppu(
			TELDEF *teldef, /* input: teldef file */
			int rawx,       /* input: RAWX [-2-257] */
			int rawy,       /* input: RAWY [0-1023] */
			int *ppux,      /* output: PPUX [0-259] */
			int *ppuy       /* output: PPUY [0-1023] */
)
{
    static char pname[] = "xis_raw2ppu";

	int tmpx, tmpy;

	TELDEF_ASTROE *p = teldef->mission.aste;

	if ( teldef->id < ASTE_XIS0_SENSOR || ASTE_XIS3_SENSOR < teldef->id ) {
		fprintf(stderr, "\
%s: TELDEF is not XIS (%d)\n", pname, teldef->id);
		return ASTE_COORD_INVALID_SENSOR;
	}

	tmpx = rawx + p->raw_xoff;
	tmpy = rawy + p->raw_yoff;

	if ( tmpx < p->ppu.xpix1 || p->ppu.xpix1 + p->ppu.xsiz <= tmpx ) {
		fprintf(stderr, "\
%s: illegal RAWX value -- %d\n", pname, rawx);
		return ASTE_COORD_INVALID_PIXEL;
	}

	if ( tmpy < p->ppu.ypix1 || p->ppu.ypix1 + p->ppu.ysiz <= tmpy ) {
		fprintf(stderr, "\
%s: illegal RAWY value -- %d\n", pname, rawy);
		return ASTE_COORD_INVALID_PIXEL;
	}

    *ppux = tmpx;
    *ppuy = tmpy;

    return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  xis_raw2act	convert XIS RAWX/Y[ch] -> ACTX/Y[ch]

  RETURN
	ASTE_COORD_INVALID_PIXEL: invalid pixel range
	ASTE_COORD_INVALID_SEGMENT: invalid segment id
	ASTE_COORD_INVALID_WIN_OPT: invalid window option
************************************************************************/
int
xis_raw2act(
	TELDEF *teldef,	/* input: teldef file */
	int segid,		/* input: Segment ID [0-3] */
	int rawx,		/* input: RAWX [-2-257] */
	int rawy,		/* input: RAWY [0-1023] */
	int win_opt,	/* input: window option [0-3] */
	int win_pos,	/* input: window position */
	int *actx,		/* output: ACTX [0-1023] */
	int *acty		/* output: ACTY [0-1023] */
)
{
	static char pname[] = "xis_raw2act";
	struct xis_coefficient_xy coe_x/*, coe_y*/;
	int intx, inty, ppux, ppuy;

	TELDEF_ASTROE *p = teldef->mission.aste;

	if ( teldef->id < ASTE_XIS0_SENSOR || ASTE_XIS3_SENSOR < teldef->id ) {
		fprintf(stderr, "\
%s: TELDEF is not XIS (%d)\n", pname, teldef->id);
		return ASTE_COORD_INVALID_SENSOR;
	}

	if ( segid < 0 || p->seg_num <= segid ) {
		fprintf(stderr, "\
%s: unknown segment ID -- %d\n", pname, segid);
		return ASTE_COORD_INVALID_SEGMENT;
	}

	ppux = rawx + p->raw_xoff;
	ppuy = rawy + p->raw_yoff;

	if ( ppux < p->ppu.xpix1 || p->ppu.xpix1 + p->ppu.xsiz <= ppux ) {
		fprintf(stderr, "\
%s: illegal RAWX value -- %d\n", pname, rawx);
		return ASTE_COORD_INVALID_PIXEL;
	}

	if ( ppuy < p->ppu.ypix1 || p->ppu.ypix1 + p->ppu.ysiz <= ppuy ) {
		fprintf(stderr, "\
%s: illegal RAWY value -- %d\n", pname, rawy);
		return ASTE_COORD_INVALID_PIXEL;
	}

	coe_x = p->coe.x[segid];
/*	coe_y = p->coe.y[segid];*/
	intx = coe_x.a + coe_x.b * rawx;
	if ( intx < p->act.xpix1 || p->act.xpix1 + p->act.xsiz <= intx ) {
		fprintf(stderr, "\
%s: illegal SEG_ID/RAWX value -- %d/%d\n", pname, segid, rawx);
		return ASTE_COORD_INVALID_PIXEL;
	}

	switch (win_opt) {
	case XISwindowOff:
		inty = rawy;
		break;
	case XISwindow4:
		inty = rawy % (p->raw.ysiz/4) + win_pos;
		break;
	case XISwindow8:
		inty = rawy % (p->raw.ysiz/8) + win_pos;
		break;
	case XISwindow16:
		inty = rawy % (p->raw.ysiz/16) + win_pos;
		break;
	default:
		fprintf(stderr, "\
%s: unknown window option -- %d\n", pname, win_opt);
		return ASTE_COORD_INVALID_WIN_OPT;
	}

	*actx = intx;
	*acty = inty;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  xis_act2raw	convert XIS ACTX/Y[ch] -> RAWX/Y[ch]

  RETURN
	ASTE_COORD_INVALID_PIXEL: invalid pixel range
	ASTE_COORD_INVALID_SEGMENT: invalid segment id
	ASTE_COORD_INVALID_WIN_OPT: invalid window option
************************************************************************/
int
xis_act2raw(
	TELDEF *teldef,	/* input: teldef file */
	int actx,		/* input: ACTX [0-1023] */
	int acty,		/* input: ACTY [0-1023] */
	int win_opt,	/* input: window option [0-3] */
	int win_pos,	/* input: window position */
	int *segid_ptr,	/* output: Segment ID [0-3] */
	int *rawx_ptr,	/* output: RAWX [0-255] */
	int *rawy_ptr	/* output: RAWY [0-1023] */
)
{
	static char pname[] = "xis_act2raw";
	struct xis_coefficient_xy coe_x/*, coe_y*/;
	int segid, rawx, rawy, inty, ypix1, ysiz;

	TELDEF_ASTROE *p = teldef->mission.aste;

	if ( teldef->id < ASTE_XIS0_SENSOR || ASTE_XIS3_SENSOR < teldef->id ) {
		fprintf(stderr, "\
%s: TELDEF is not XIS (%d)\n", pname, teldef->id);
		return ASTE_COORD_INVALID_SENSOR;
	}

	if ( actx < p->act.xpix1 || p->act.xpix1 + p->act.xsiz <= actx ) {
		fprintf(stderr, "\
%s: illegal ACTX value -- %d\n", pname, actx);
		return ASTE_COORD_INVALID_PIXEL;
	}

	if ( acty < p->act.ypix1 || p->act.ypix1 + p->act.ysiz <= acty ) {
		fprintf(stderr, "\
%s: illegal ACTY value -- %d\n", pname, acty);
		return ASTE_COORD_INVALID_PIXEL;
	}

	segid = (actx - p->act.xpix1) / (p->act.xsiz/4);

	coe_x = p->coe.x[segid];
/*	coe_y = p->coe.y[segid];*/
	rawx = (actx - coe_x.a) / coe_x.b;

	inty = acty;
	ypix1 = p->raw.ypix1;

	switch (win_opt) {
	case XISwindowOff:
		rawy = inty;
		break;
	case XISwindow4:
		ysiz = p->raw.ysiz / 4;
		if ( ypix1 <= inty - win_pos && inty - win_pos < ypix1 + ysiz ) {
			rawy = inty - win_pos + ysiz;
		} else {
			fprintf(stderr, "\
%s: ACTY out of range -- %d\n", pname, acty);
			return ASTE_COORD_INVALID_PIXEL;
		}
		break;
	case XISwindow8:
		ysiz = p->raw.ysiz / 8;
		if ( ypix1 <= inty - win_pos && inty - win_pos < ypix1 + ysiz ) {
			rawy = inty - win_pos + ysiz;
		} else {
			fprintf(stderr, "\
%s: ACTY out of range -- %d\n", pname, acty);
			return ASTE_COORD_INVALID_PIXEL;
		}
		break;
	case XISwindow16:
		ysiz = p->raw.ysiz / 16;
		if ( ypix1 <= inty - win_pos && inty - win_pos < ypix1 + ysiz ) {
			rawy = inty - win_pos + ysiz;
		} else {
			fprintf(stderr, "\
%s: ACTY out of range -- %d\n", pname, acty);
			return ASTE_COORD_INVALID_PIXEL;
		}
		break;
	default:
		fprintf(stderr, "\
%s: unknown window option -- %d\n", pname, win_opt);
		return ASTE_COORD_INVALID_WIN_OPT;
	}

	*segid_ptr = segid;
	*rawx_ptr = rawx;
	*rawy_ptr = rawy;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  xis_act2det	convert XIS ACTX/Y[ch] -> DETX/Y[ch]
************************************************************************/
int
xis_act2det(
	TELDEF *teldef,	/* input: teldef file */
	int actx_ch,	/* input: ACTX [0-1023] */
	int acty_ch,	/* input: ACTY [0-1023] */
	int *detx_ch,	/* output: DETX [1-1024] */
	int *dety_ch	/* output: DETY [1-1024] */
)
{
	static char pname[] = "xis_act2det";
	TELDEF_ASTROE *p = teldef->mission.aste;

	if ( teldef->id < ASTE_XIS0_SENSOR || ASTE_XIS3_SENSOR < teldef->id ) {
		fprintf(stderr, "\
%s: TELDEF is not XIS (%d)\n", pname, teldef->id);
		return ASTE_COORD_INVALID_SENSOR;
	}

	*detx_ch = p->detx_a + p->detx_b * actx_ch + p->detx_c * acty_ch;
	*dety_ch = p->dety_a + p->dety_b * actx_ch + p->dety_c * acty_ch;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  xis_det2act	convert XIS DETX/Y[ch] -> ACTX/Y[ch]
************************************************************************/
int
xis_det2act(
	TELDEF *teldef,	/* input: teldef file */
	int detx_ch,	/* input: DETX [1-1024] */
	int dety_ch,	/* input: DETY [1-1024] */
	int *actx_ch,	/* output: ACTX [0-1023] */
	int *acty_ch	/* output: ACTY [0-1023] */
)
{
	static char pname[] = "xis_det2act";
	TELDEF_ASTROE *p = teldef->mission.aste;

	if ( teldef->id < ASTE_XIS0_SENSOR || ASTE_XIS3_SENSOR < teldef->id ) {
		fprintf(stderr, "\
%s: TELDEF is not XIS (%d)\n", pname, teldef->id);
		return ASTE_COORD_INVALID_SENSOR;
	}

	if ( p->detx_b && p->dety_c ) {
		*actx_ch = (detx_ch - p->detx_a) / p->detx_b;
		*acty_ch = (dety_ch - p->dety_a) / p->dety_c;
	} else if ( p->detx_c && p->dety_b ) {
		*actx_ch = (dety_ch - p->dety_a) / p->dety_b;
		*acty_ch = (detx_ch - p->detx_a) / p->detx_c;
	} else {
		return ASTE_COORD_NOT_INITIALIZED;
	}

	return ASTE_COORD_NORMAL_END;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
