/*
	aecoordcalc.c

	2005/06/28 Y.ISHISAKI	version 1.0

	2006/06/08 Y.ISHISAKI	version 1.1
		bug fix in handling dec.sign in degToDECstr()
		consider decimal overflow of last digit in degToRAstr(), degToDECstr()
		print AETIME only when mission time is initialized in show_parameter()

	2006/07/02 Y.ISHISAKI	version 1.2
		support of CALDB for leapfile & teldef

	2006/07/12 Y.ISHISAKI	version 1.3
		bug fix in parseRAstrToDeg() & parseDECstrToDeg()

	2006/07/24 Y.ISHISAKI	version 1.4
		further bug fix in parseRAstrToDeg() & parseDECstrToDeg()
		static declarations of parseRAstrToDeg() & parseDECstrToDeg()
		static declarations of degToRAstr() & degToDECstr()

	2006/07/31 Y.ISHISAKI	version 1.5
		read mjdrefi, mjdreff from parameter file
		use aste_cor_aberration(), aste_inv_aberration() in astetool-1.80
		caldb access moved from _com() to _init()
		print error when PILGet() failes
		use aste_mjdrefi(), aste_mjdreff() in _startup()

	2007/05/14 Y.ISHISAKI	version 1.6
		use aste_caldb_find_leapfile()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "anl.h"
#include "cli.h"
#include "com.h"
#include "pil.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "aste_coord.h"
#include "aste_att.h"
#include "aste_caldb.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "aecoordcalc";
char aecoordcalc_version[] = "version 1.6";

enum attitude_type {
	ATTITUDE_FILE,		/* specify attitude file */
	ATTITUDE_KEY,		/* read event file MEAN_EA1/2/3 */
	ATTITUDE_USER		/* set manually by user */
};

enum pointing_type {
	POINTING_KEY,
	POINTING_USER
};

static struct {
/* aecoordcalc */
	char instrume[PIL_LINESIZE];
	int inst_id;
	char coord[PIL_LINESIZE];
	char output[PIL_LINESIZE];
	int output_inst_id;

	char attitude[PIL_LINESIZE];
	enum attitude_type  attitude_type;
	char pointing[PIL_LINESIZE];
	enum pointing_type  pointing_type;
	char infile[PIL_LINESIZE];

	double ea1, ea2, ea3;	/* deg */
	AtEulerAng ea;			/* radian */

	char ref_alpha_string[PIL_LINESIZE];
	char ref_delta_string[PIL_LINESIZE];
	SKYREF sref;

	int aberration;

	char time_string[PIL_LINESIZE];
	double aetime;
	char *leapfile;
	char o_leapfile[PIL_LINESIZE];
	int    mjdrefi;
	double mjdreff;

} com;

struct {
	char alpha_string[PIL_LINESIZE];
	char delta_string[PIL_LINESIZE];
	double alpha, delta;
	double alphaJ2000, deltaJ2000;
	double alphaB1950, deltaB1950;
	double l, b;
	double aber_alpha, aber_delta, aber_arcsec;
	double skyx, skyy;
	double focx, focy;
	double detx, dety;
	double theta, phi;
	int pixel, corner;
	int actx, acty;
	int rawx, rawy;
	int ppux, ppuy;
	int segment;
} pos;

struct {
	char *teldef_file;
	char o_teldef_file[PIL_LINESIZE];
	TELDEF *teldef;
	double skyx, skyy;
	double focx, focy;
	double detx, dety;
	double theta, phi;
	int pixel;
} xrs;

struct {
	char *teldef_file;
	char o_teldef_file[PIL_LINESIZE];
	TELDEF *teldef;
	double skyx, skyy;
	double focx, focy;
	double detx, dety;
	double theta, phi;
} hxd;

struct {
	char *teldef_file;
	char o_teldef_file[PIL_LINESIZE];
	TELDEF *teldef;
	double skyx, skyy;
	double focx, focy;
	double detx, dety;
	double theta, phi;
	int actx, acty;
	int rawx, rawy;
	int ppux, ppuy;
	int segment;
} xis[4];


/*
   convert date string '2000-01-01T00:00:00.000' into aetime
*/
static double
datestr2aetime(char *datestrIN)
{
	char *p;
	char datestr[PIL_LINESIZE];
	AtTimeD attime;
	double aetime;
	int num_read;

	strcpy(datestr, datestrIN);

/* ignore non-number chars */
	for (p = datestr; *p; p++) {
		if ( (*p < '0' || '9' < *p) && '.' != *p ) {
			*p = ' ';
		}
	}

/* initialize attime */
	attime.yr = 2000;
	attime.mo = 1;
	attime.dy = 1;
	attime.hr = 0;
	attime.mn = 0;
	attime.sc = 0;
	attime.ss = 0.0;

	num_read = sscanf(datestr, "%d %d %d %d %d %lf",
		&attime.yr, &attime.mo, &attime.dy,
		&attime.hr, &attime.mn, &attime.ss);

	if ( num_read < 3 ) {

		aetime = atof(datestrIN);

	} else {

		if ( 6 == num_read ) {
			attime.sc = (int)floor(attime.ss);
			attime.ss = attime.ss - attime.sc;
		}

		aetime = attimeD2aste(&attime);

	}

	return aetime;
}

/*
   convert aetime into date string '2000-01-01T00:00:00.000'
*/
static char *
aetime2datestr(double aetime, char *datestrOUT)
{
	int us;
	AtTimeD attime;

	aste2attimeD(aetime, &attime);

	if ( 0.0 == attime.ss ) {
		sprintf(datestrOUT, "%04d-%02d-%02dT%02d:%02d:%02d",
			attime.yr, attime.mo, attime.dy,
			attime.hr, attime.mn, attime.sc);
	} else {

		us = floor( 1e6 * attime.ss );
		while ( 1000000 < us ) {
			us -= 1000000;
			attime.sc += 1;
		}
		while ( us < 0 ) {
			us += 1000000;
			attime.sc -= 1;
		}

		sprintf(datestrOUT, "%04d-%02d-%02dT%02d:%02d:%02d.%06d",
			attime.yr, attime.mo, attime.dy,
			attime.hr, attime.mn, attime.sc, us);
	}

	return datestrOUT;
}

/*
   parse R.A. string 'NNhNNmNN.Ns' into deg, where 0 <= deg < 360.0
*/
static double
parseRAstrToDeg(char *expression)
{
	char *p, *q;
	AtRightAscension ra;
	double deg;

	ra.hour = ra.min = 0;
	ra.sec = 0.0;
	p = q = expression;

	if ( '.' == *q ) { p = expression; goto skip; }
	while ( *q++ ) {
		if ( '.' == *q ) { p = expression; goto skip; }
		if ( '+' != *q && '-' != *q && ( *q < '0' || '9' < *q ) ) {
			ra.hour = atoi(p);
			p = q + 1;
			break;
		}
	}

	if ( '\0' == *(q-1) || '.' == *q ) { p = expression; goto skip; }
	while ( *q++ ) {
		if ( '.' == *q ) { p = expression; goto skip; }
		if ( '+' != *q && '-' != *q && ( *q < '0' || '9' < *q ) ) {
			ra.min = atoi(p);
			p = q + 1;
			break;
		}
	}

	if ( '\0' == *(q-1) ) { p = "0.0"; }

 skip:
	ra.sec = atof(p);

	if ( p == expression ) {
		deg = ra.sec;
	} else {
		deg = ra.hour*15.0 + 0.25 * (ra.min + ra.sec/60.0);
	}

	while ( deg < 0.0 ) {
		deg += 360.0;
	}

	while ( 360.0 <= deg ) {
		deg -= 360.0;
	}

	return deg;
}

/*
   parse DEC. string 'NNdNNmNN.Ns' into deg, where -90 <= deg <= 90
*/
static double
parseDECstrToDeg(char *expression)
{
	char *p, *q;
	AtDeclination dec;
	double deg;

	dec.sign = 1;
	dec.deg = dec.min = 0;
	dec.sec = 0.0;

	p = q = expression;

	if ( '-' == *p ) {
		dec.sign = -1;
	}

	if ( '.' == *q ) { p = expression; goto skip; }
	while ( *q++ ) {
		if ( '.' == *q ) { p = expression; goto skip; }
		if ( '+' != *q && '-' != *q && ( *q < '0' || '9' < *q ) ) {
			dec.deg = abs(atoi(p));
			p = q + 1;
			break;
		}
	}

	if ( '\0' == *(q-1) || '.' == *q ) { p = expression; goto skip; }
	while ( *q++ ) {
		if ( '.' == *q ) { p = expression; goto skip; }
		if ( '+' != *q && '-' != *q && ( *q < '0' || '9' < *q ) ) {
			dec.min = atoi(p);
			p = q + 1;
			break;
		}
	}

	if ( '\0' == *(q-1) ) { p = "0.0"; }

 skip:
	dec.sec = atof(p);

	if ( p == expression ) {
		deg = dec.sec;
	} else {
		deg = dec.sign * (dec.deg + (dec.min + dec.sec/60) / 60.);
	}

	while ( deg < -180.0 ) {
		deg += 180.0;
	}

	while ( +180.0 < deg ) {
		deg -= 180.0;
	}

	if ( deg < -90.0 ) {
		deg = - 180.0 - deg;	/* -91.0 -> -89.0 */
	}

	if ( +90.0 < deg ) {
		deg = 180.0 - deg;		/* 91.0 -> 89.0 */
	}

	return deg;
}

/*
   convert deg into R.A. string 'NNhNNmNN.Ns'
*/
static char *
degToRAstr(double deg, char *outstr)
{
	AtRightAscension ra;
	int subsec;

	while ( deg < 0.0 ) {
		deg += 360.0;
	}

	while ( 360.0 <= deg ) {
		deg -= 360.0;
	}

	atDegToRA(deg, &ra);
	subsec = (int)( ra.sec * 10 + 0.5 );
	if ( 600 <= subsec ) {
		subsec = subsec - 600;
		ra.min = ra.min + 1;
		if ( 60 <= ra.min ) {
			ra.min = ra.min - 60;
			ra.hour = ra.hour + 1;
			if ( 24 <= ra.hour ) {
				ra.hour = ra.hour - 24;
			}
		}
	}
	sprintf(outstr, "%dh%02dm%04.1fs", ra.hour, ra.min, subsec/10.0);

	return outstr;
}

/*
   convert deg into DEC. string 'NNdNNmNN.Ns'
*/
static char *
degToDECstr(double deg, char *outstr)
{
	int sig, sec;
	AtDeclination dec;

	while ( deg < -180.0 ) {
		deg += 180.0;
	}

	while ( +180.0 < deg ) {
		deg -= 180.0;
	}

	if ( deg < -90.0 ) {
		deg = - 180.0 - deg;	/* -91.0 -> -89.0 */
	}

	if ( +90.0 < deg ) {
		deg = 180.0 - deg;		/* 91.0 -> 89.0 */
	}

	atDegToDec(deg, &dec);
	sig = ( 0 < dec.sign ) ? '+' : '-';
	sec = (int)(dec.sec + 0.5);
	if ( 60 <= sec ) {
		sec = sec - 60;
		dec.min = dec.min + 1;
		if ( 60 <= dec.min ) {
			dec.min = dec.min - 60;
			dec.deg = dec.deg + 1;
		}
	}
	sprintf(outstr, "%c%dd%02dm%02ds", sig, dec.deg, dec.min, sec);

	return outstr;
}

static int
read_infile(void)
{
	int hdunum, hdutype;
	fitsfile *fp;
	char *k;

	int istat = 0;

	fits_open_file(&fp, com.infile, READONLY, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_open_file('%s') failed\n", pname, com.infile);
		return istat;
	}

	fits_get_hdu_num(fp, &hdunum);
	if ( 1 == hdunum ) {	/* primary */
		fits_movabs_hdu(fp, 2, &hdutype, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_movabs_hdu() to 1st extension failed\n", pname);
			return istat;
		}
	}

	if ( ATTITUDE_KEY == com.attitude_type ) {
		if (
fits_read_key_dbl(fp, k="MEAN_EA1", &com.ea1, NULL, &istat) ||
fits_read_key_dbl(fp, k="MEAN_EA2", &com.ea2, NULL, &istat) ||
fits_read_key_dbl(fp, k="MEAN_EA3", &com.ea3, NULL, &istat) ||
			 0 ) {
			fprintf(stderr, "\
%s: fits_read_key('%s') failed\n", pname, k);
			return istat;
		}
		com.ea.phi   = DEG2RAD * com.ea1;
		com.ea.theta = DEG2RAD * com.ea2;
		com.ea.psi   = DEG2RAD * com.ea3;
	}

	if ( POINTING_KEY == com.pointing_type ) {
		com.sref.roll = 0.0;
		if (
fits_read_key_dbl(fp, k="RA_NOM", &com.sref.alpha, NULL, &istat) ||
fits_read_key_dbl(fp, k="DEC_NOM", &com.sref.delta, NULL, &istat) ||
			 0 ) {
			fprintf(stderr, "\
%s: fits_read_key('%s') failed\n", pname, k);
			return istat;
		}
	}

	fits_close_file(fp, &istat);

	return 0;
}

static void
cor_aberration(double t, double *raInOut, double *decInOut)
{
	if ( com.aberration ) {
		aste_cor_aberration(t, com.mjdrefi, com.mjdreff, raInOut, decInOut);
	}
}

static void
inv_aberration(double t, double *raInOut, double *decInOut)
{
	if ( com.aberration ) {
		aste_inv_aberration(t, com.mjdrefi, com.mjdreff, raInOut, decInOut);
	}
}

static int
calc_ecs(void)
{
	char *k;
	int idetx, idety, actx, acty, rawx, rawy;
	double xrtx_mm, xrty_mm, detx, dety, alpha, delta;
	AtVect v0, v1;

	TELDEF *teldef = NULL;

	if ( ASTE_XRS_ID == com.inst_id ) {
		teldef = xrs.teldef;
	} else if ( ASTE_HXD_ID == com.inst_id ) {
		teldef = xrs.teldef;
	} else if ( ASTE_XIS0_ID <= com.inst_id && com.inst_id <= ASTE_XIS3_ID ) {
		teldef = xis[com.inst_id - ASTE_XIS0_ID].teldef;
	} else {
		fprintf(stderr, "\
%s: calc_coord(): invalid INSTRUME=%s\n", pname, com.instrume);
		return -1;
	}

	if ( 0 == strcmp("ECS", com.coord) ||
		 0 == strcmp("J2000", com.coord) ) {
		pos.alphaJ2000 = pos.alpha;
		pos.deltaJ2000 = pos.delta;
	} else if ( 0 == strcmp("B1950", com.coord) ) {
		atB1950toJ2000(pos.alpha, pos.delta,
			&pos.alphaJ2000, &pos.deltaJ2000);
	} else if ( 0 == strcmp("GAL", com.coord) ||
			    0 == strcmp("GALACTIC", com.coord) ) {
		atGaltoJ2000(pos.l, pos.b,
			&pos.alphaJ2000, &pos.deltaJ2000);
	} else if ( 0 == strcmp("SKY", com.coord) ) {
		aste_sky2ecs(teldef, &com.sref, pos.skyx, pos.skyy,
			&pos.alphaJ2000, &pos.deltaJ2000);
	} else if ( 0 == strcmp("FOC", com.coord) ) {
		aste_foc2ecs(teldef, &com.ea, pos.focx, pos.focy,
			&pos.alphaJ2000, &pos.deltaJ2000);
		cor_aberration(com.aetime, &pos.alphaJ2000, &pos.deltaJ2000);
	} else if ( 0 == strcmp("DET", com.coord) ) {
		aste_det2ecs(teldef, &com.ea, pos.detx, pos.dety,
			&pos.alphaJ2000, &pos.deltaJ2000);
		cor_aberration(com.aetime, &pos.alphaJ2000, &pos.deltaJ2000);
	} else if ( 0 == strcmp("OPTIC", com.coord) ) {
		aste_pol2rec(teldef, pos.theta, pos.phi, &xrtx_mm, &xrty_mm);
		aste_xrt2det(teldef, xrtx_mm, xrty_mm, &detx, &dety);
		aste_det2ecs(teldef, &com.ea, detx, dety,
			&pos.alphaJ2000, &pos.deltaJ2000);
		cor_aberration(com.aetime, &pos.alphaJ2000, &pos.deltaJ2000);
	} else if ( 0 == strcmp("PIXEL", com.coord) ) {
		xrs_pixel2det(teldef, pos.pixel, pos.corner, &detx, &dety);
		aste_det2ecs(teldef, &com.ea, detx, dety,
			&pos.alphaJ2000, &pos.deltaJ2000);
		cor_aberration(com.aetime, &pos.alphaJ2000, &pos.deltaJ2000);
	} else if ( 0 == strcmp("ACT", com.coord) ) {
		xis_act2det(teldef, pos.actx, pos.acty, &idetx, &idety);
		detx = idetx;
		dety = idety;
		aste_det2ecs(teldef, &com.ea, detx, dety,
			&pos.alphaJ2000, &pos.deltaJ2000);
		cor_aberration(com.aetime, &pos.alphaJ2000, &pos.deltaJ2000);
	} else if ( 0 == strcmp("RAW", com.coord) ) {
		xis_raw2act(teldef, pos.segment, pos.rawx, pos.rawy,
			0, 0, &actx, &acty);
		xis_act2det(teldef, pos.actx, pos.acty, &idetx, &idety);
		detx = idetx;
		dety = idety;
		aste_det2ecs(teldef, &com.ea, detx, dety,
			&pos.alphaJ2000, &pos.deltaJ2000);
		cor_aberration(com.aetime, &pos.alphaJ2000, &pos.deltaJ2000);
	} else if ( 0 == strcmp("PPU", com.coord) ) {
		xis_ppu2raw(teldef, pos.ppux, pos.ppuy, &rawx, &rawy);
		xis_raw2act(teldef, pos.segment, rawx, rawy, 0, 0, &actx, &acty);
		xis_act2det(teldef, pos.actx, pos.acty, &idetx, &idety);
		detx = idetx;
		dety = idety;
		aste_det2ecs(teldef, &com.ea, detx, dety,
			&pos.alphaJ2000, &pos.deltaJ2000);
		cor_aberration(com.aetime, &pos.alphaJ2000, &pos.deltaJ2000);
	} else {
		fprintf(stderr, "\
%s: calc_coord(): invalid COORD=%s\n", pname, com.coord);
		return -1;
	}

	atJ2000toB1950(pos.alphaJ2000, pos.deltaJ2000,
			&pos.alphaB1950, &pos.deltaB1950);
	atJ2000toGal(pos.alphaJ2000, pos.deltaJ2000,
			&pos.l, &pos.b);

	alpha = pos.alphaJ2000;
	delta = pos.deltaJ2000;
	inv_aberration(com.aetime, &alpha, &delta);
	pos.aber_alpha = aefits_delta_phi(pos.alphaJ2000, alpha);
	pos.aber_delta = pos.deltaJ2000 - delta;
	atPolDegToVect(1.0, pos.alphaJ2000, pos.deltaJ2000, v0);
	atPolDegToVect(1.0, alpha, delta, v1);
	atAngDistance(v0, v1, &pos.aber_arcsec);
	pos.aber_arcsec *= RAD2DEG * 60 * 60;

	if (
PILPutReal(k="alphaJ2000", pos.alphaJ2000) ||
PILPutReal(k="deltaJ2000", pos.deltaJ2000) ||
PILPutReal(k="alphaB1950", pos.alphaB1950) ||
PILPutReal(k="deltaB1950", pos.deltaB1950) ||
PILPutReal(k="l", pos.l) ||
PILPutReal(k="b", pos.b) ||
PILPutReal(k="aber_alpha", pos.aber_alpha) ||
PILPutReal(k="aber_delta", pos.aber_delta) ||
PILPutReal(k="aber_arcsec", pos.aber_arcsec) ||
		 0 ) {
		fprintf(stderr, "\
%s: PILPut('%s') failed\n", pname, k);
		return -1;
	}

	return 0;
}

void
print_ecs(void)
{
	char s1[80], s2[80];

	printf("\
%12s   ( %9.4f , %9.4f ) [deg] / (%12s ,%11s )\n", "J2000",
		pos.alphaJ2000, pos.deltaJ2000,
		degToRAstr(pos.alphaJ2000, s1), degToDECstr(pos.deltaJ2000, s2));
	printf("\
%12s   ( %9.4f , %9.4f ) [deg] / (%12s ,%11s )\n", "B1950",
		pos.alphaB1950, pos.deltaB1950,
		degToRAstr(pos.alphaB1950, s1), degToDECstr(pos.deltaB1950, s2));
	printf("\
%12s   ( %9.4f , %9.4f ) [deg]\n", "Galactic", pos.l, pos.b);
	printf("\
%12s   ( %9.4f , %9.4f ) [arcsec],  Ang.Distance =%9.4f\n", "Aberration",
		pos.aber_alpha*60*60, pos.aber_delta*60*60, pos.aber_arcsec);
}

static int
calc_xrs(void)
{
	char *k;
	TELDEF *teldef;
	double alpha, delta, xrtx_mm, xrty_mm;

	teldef = xrs.teldef;
	alpha = pos.alphaJ2000;
	delta = pos.deltaJ2000;

	aste_ecs2sky(teldef, &com.sref, alpha, delta, &xrs.skyx, &xrs.skyy);
	inv_aberration(com.aetime, &alpha, &delta);
	aste_ecs2foc(teldef, &com.ea, alpha, delta, &xrs.focx, &xrs.focy);
	aste_ecs2det(teldef, &com.ea, alpha, delta, &xrs.detx, &xrs.dety);
	aste_det2xrt(teldef, xrs.detx, xrs.dety, &xrtx_mm, &xrty_mm);
	aste_xrt_rec2pol(teldef, xrtx_mm, xrty_mm, &xrs.theta, &xrs.phi);
	xrs_det2pixel(teldef, xrs.detx, xrs.dety, &xrs.pixel);

	if (
PILPutReal(k="xrs_skyx", xrs.skyx) ||
PILPutReal(k="xrs_skyy", xrs.skyy) ||
PILPutReal(k="xrs_focx", xrs.focx) ||
PILPutReal(k="xrs_focy", xrs.focy) ||
PILPutReal(k="xrs_detx", xrs.detx) ||
PILPutReal(k="xrs_dety", xrs.dety) ||
PILPutReal(k="xrs_theta", xrs.theta) ||
PILPutReal(k="xrs_phi", xrs.phi) ||
PILPutInt(k="xrs_pixel", xrs.pixel) ||
		 0 ) {
		fprintf(stderr, "\
%s: PILPut('%s') failed\n", pname, k);
		return -1;
	}

	return 0;
}

static void
print_xrs(void)
{
	printf("\
%12s   ( %9.4f , %9.4f ) [pixel]\n", "XRS SKY", xrs.skyx, xrs.skyy);
	printf("\
%12s   ( %9.4f , %9.4f ) [pixel]\n", "XRS FOC", xrs.focx, xrs.focy);
	printf("\
%12s   ( %9.4f , %9.4f ) [pixel]\n", "XRS DET", xrs.detx, xrs.dety);
	printf("\
%s    %9.4f [arcmin] / %9.4f [deg]\n", "XRS THETA/PHI", xrs.theta, xrs.phi);
	printf("\
%12s   = %2d\n", "XRS PIXEL", xrs.pixel);
}

static int
calc_hxd(void)
{
	char *k;
	TELDEF *teldef;
	double alpha, delta, xrtx_mm, xrty_mm;

	teldef = hxd.teldef;
	alpha = pos.alphaJ2000;
	delta = pos.deltaJ2000;

	aste_ecs2sky(teldef, &com.sref, alpha, delta, &hxd.skyx, &hxd.skyy);
	inv_aberration(com.aetime, &alpha, &delta);
	aste_ecs2foc(teldef, &com.ea, alpha, delta, &hxd.focx, &hxd.focy);
	aste_ecs2det(teldef, &com.ea, alpha, delta, &hxd.detx, &hxd.dety);
	aste_det2xrt(teldef, hxd.detx, hxd.dety, &xrtx_mm, &xrty_mm);
	aste_xrt_rec2pol(teldef, xrtx_mm, xrty_mm, &hxd.theta, &hxd.phi);

	if (
PILPutReal(k="hxd_skyx", hxd.skyx) ||
PILPutReal(k="hxd_skyy", hxd.skyy) ||
PILPutReal(k="hxd_focx", hxd.focx) ||
PILPutReal(k="hxd_focy", hxd.focy) ||
PILPutReal(k="hxd_detx", hxd.detx) ||
PILPutReal(k="hxd_dety", hxd.dety) ||
PILPutReal(k="hxd_theta", hxd.theta) ||
PILPutReal(k="hxd_phi", hxd.phi) ||
		 0 ) {
		fprintf(stderr, "\
%s: PILPut('%s') failed\n", pname, k);
		return -1;
	}

	return 0;
}

static void
print_hxd(void)
{
	printf("\
%12s   ( %9.4f , %9.4f ) [pixel]\n", "HXD SKY", hxd.skyx, hxd.skyy);
	printf("\
%12s   ( %9.4f , %9.4f ) [pixel]\n", "HXD FOC", hxd.focx, hxd.focy);
	printf("\
%12s   ( %9.4f , %9.4f ) [pixel]\n", "HXD DET", hxd.detx, hxd.dety);
	printf("\
%s    %9.4f [arcmin] / %9.4f [deg]\n", "HXD THETA/PHI", hxd.theta, hxd.phi);
}

static int
calc_xis(int i)
{
	char k[80];
	TELDEF *teldef;
	double alpha, delta, xrtx_mm, xrty_mm;
	int idetx, idety;

	teldef = xis[i].teldef;
	alpha = pos.alphaJ2000;
	delta = pos.deltaJ2000;

	aste_ecs2sky(teldef, &com.sref, alpha, delta, &xis[i].skyx, &xis[i].skyy);
	inv_aberration(com.aetime, &alpha, &delta);
	aste_ecs2foc(teldef, &com.ea, alpha, delta, &xis[i].focx, &xis[i].focy);
	aste_ecs2det(teldef, &com.ea, alpha, delta, &xis[i].detx, &xis[i].dety);
	aste_det2xrt(teldef, xis[i].detx, xis[i].dety, &xrtx_mm, &xrty_mm);
	aste_xrt_rec2pol(teldef, xrtx_mm, xrty_mm, &xis[i].theta, &xis[i].phi);
	idetx = (int)floor(xis[i].detx + 0.5);
	idety = (int)floor(xis[i].dety + 0.5);
	xis_det2act(teldef, idetx, idety, &xis[i].actx, &xis[i].acty);
	xis_act2raw(teldef, xis[i].actx, xis[i].acty, 0, 0,
		&xis[i].segment, &xis[i].rawx, &xis[i].rawy);
	xis_raw2ppu(teldef, xis[i].rawx, xis[i].rawy, &xis[i].ppux, &xis[i].ppuy);

	if (
(sprintf(k, "xis%d_skyx", i), PILPutReal(k, xis[i].skyx)) ||
(sprintf(k, "xis%d_skyy", i), PILPutReal(k, xis[i].skyy)) ||
(sprintf(k, "xis%d_focx", i), PILPutReal(k, xis[i].focx)) ||
(sprintf(k, "xis%d_focy", i), PILPutReal(k, xis[i].focy)) ||
(sprintf(k, "xis%d_detx", i), PILPutReal(k, xis[i].detx)) ||
(sprintf(k, "xis%d_dety", i), PILPutReal(k, xis[i].dety)) ||
(sprintf(k, "xis%d_theta",i), PILPutReal(k, xis[i].theta))||
(sprintf(k, "xis%d_phi",  i), PILPutReal(k, xis[i].phi))  ||
(sprintf(k, "xis%d_actx", i), PILPutInt(k, xis[i].actx)) ||
(sprintf(k, "xis%d_acty", i), PILPutInt(k, xis[i].acty)) ||
(sprintf(k, "xis%d_segment", i), PILPutInt(k, xis[i].segment)) ||
(sprintf(k, "xis%d_rawx", i), PILPutInt(k, xis[i].rawx)) ||
(sprintf(k, "xis%d_rawy", i), PILPutInt(k, xis[i].rawy)) ||
(sprintf(k, "xis%d_ppux", i), PILPutInt(k, xis[i].ppux)) ||
(sprintf(k, "xis%d_ppuy", i), PILPutInt(k, xis[i].ppuy)) ||
		 0 ) {
		fprintf(stderr, "\
%s: PILPut('%s') failed\n", pname, k);
		return -1;
	}

	return 0;
}

static void
print_xis(int i)
{
	char item[80];

	sprintf(item, "XIS%d SKY", i); printf("\
%12s   ( %9.4f , %9.4f ) [pixel]\n", item, xis[i].skyx, xis[i].skyy);
	sprintf(item, "XIS%d FOC", i); printf("\
%12s   ( %9.4f , %9.4f ) [pixel]\n", item, xis[i].focx, xis[i].focy);
	sprintf(item, "XIS%d DET", i); printf("\
%12s   ( %9.4f , %9.4f ) [pixel]\n", item, xis[i].detx, xis[i].dety);
	sprintf(item, "XIS%d ACT", i); printf("\
%12s   ( %4d      , %4d      ) [pixel]\n", item, xis[i].actx, xis[i].acty);
	sprintf(item, "XIS%d RAW", i); printf("\
%12s   ( %4d      , %4d      ) [pixel] at SEGMENT = %d\n", item,
		xis[i].rawx, xis[i].rawy, xis[i].segment);
	sprintf(item, "XIS%d PPU", i); printf("\
%12s   ( %4d      , %4d      ) [pixel]\n", item, xis[i].ppux, xis[i].ppuy);
	sprintf(item, "XIS%d THETA/PHI", i); printf("\
%s   %9.4f [arcmin] / %9.4f [deg]\n", item, xis[i].theta, xis[i].phi);
}

void
aecoordcalc_startup(int *status)
{
	com.leapfile = com.o_leapfile;
	xrs.teldef_file = xrs.o_teldef_file;
	hxd.teldef_file = hxd.o_teldef_file;
	xis[0].teldef_file = xis[0].o_teldef_file;
	xis[1].teldef_file = xis[1].o_teldef_file;
	xis[2].teldef_file = xis[2].o_teldef_file;
	xis[3].teldef_file = xis[3].o_teldef_file;

	com.mjdrefi = 51544;
	com.mjdreff = 0.00074287037037037;

	*status = ANL_OK;
}

static void
show_parameter(void)
{
	static char *corner_string[] = {
		"center", "left-low", "right-low", "right-up", "left-up"
	};

	char s[80];

	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%20s   %s\n", "INSTRUME", com.instrume);
	printf("%20s   %s\n", "COORD", com.coord);
	if ( 0 == strcmp("ECS", com.coord) ||
		 0 == strcmp("J2000", com.coord) ||
		 0 == strcmp("B1950", com.coord) ) {
		printf("%20s   %.4f (deg) / %s\n", "ALPHA",
			pos.alpha, degToRAstr(pos.alpha, s) );
		printf("%20s   %.4f (deg) / %s\n", "DELTA",
			pos.delta, degToDECstr(pos.delta, s) );
	} else if ( 0 == strcmp("GAL", com.coord) ||
			    0 == strcmp("GALACTIC", com.coord) ) {
		printf("%20s   %.4f (deg)\n", "L", pos.l);
		printf("%20s   %.4f (deg)\n", "B", pos.b);
	} else if ( 0 == strcmp("SKY", com.coord) ) {
		printf("%20s   %.3f (pixel)\n", "X", pos.skyx);
		printf("%20s   %.3f (pixel)\n", "Y", pos.skyy);
	} else if ( 0 == strcmp("FOC", com.coord) ) {
		printf("%20s   %.3f (pixel)\n", "FOCX", pos.focx);
		printf("%20s   %.3f (pixel)\n", "FOCY", pos.focy);
	} else if ( 0 == strcmp("DET", com.coord) ) {
		printf("%20s   %.3f (pixel)\n", "DETX", pos.detx);
		printf("%20s   %.3f (pixel)\n", "DETY", pos.dety);
	} else if ( 0 == strcmp("OPTIC", com.coord) ) {
		printf("%20s   %.3f (arcmin)\n", "THETA", pos.theta);
		printf("%20s   %.3f (deg)\n", "PHI", pos.phi);
	} else if ( 0 == strcmp("PIXEL", com.coord) ) {
		printf("%20s   %d\n", "PIXEL", pos.pixel);
		printf("%20s   %d (%s)\n", "CORNER",
			pos.corner, corner_string[pos.corner]);
	} else if ( 0 == strcmp("ACT", com.coord) ) {
		printf("%20s   %d (pixel)\n", "ACTX", pos.actx);
		printf("%20s   %d (pixel)\n", "ACTY", pos.acty);
	} else if ( 0 == strcmp("RAW", com.coord) ) {
		printf("%20s   %d\n", "SEGMENT", pos.segment);
		printf("%20s   %d (pixel)\n", "RAWX", pos.rawx);
		printf("%20s   %d (pixel)\n", "RAWY", pos.rawy);
	} else if ( 0 == strcmp("PPU", com.coord) ) {
		printf("%20s   %d\n", "SEGMENT", pos.segment);
		printf("%20s   %d (pixel)\n", "PPUX", pos.ppux);
		printf("%20s   %d (pixel)\n", "PPUY", pos.ppuy);
	}

	printf("%20s   %s\n", "OUTPUT", com.output);
	if ( ATTITUDE_FILE == com.attitude_type ) {
		printf("%20s   '%s'\n", "ATTITUDE", com.attitude);
	} else {
		printf("%20s   %s\n", "ATTITUDE", com.attitude);
	}
	printf("%20s   %s\n", "POINTING", com.pointing);
	if ( ATTITUDE_KEY == com.attitude_type ||
		 POINTING_KEY == com.pointing_type ) {
		printf("%20s   '%s'\n", "INFILE", com.infile);
	}
	printf("%20s   %.12f (deg)\n", "EA1", com.ea1);
	printf("%20s   %.12f (deg)\n", "EA2", com.ea2);
	printf("%20s   %.12f (deg)\n", "EA3", com.ea3);
	printf("%20s   %.4f (deg) / %s\n", "REF_ALPHA",
		com.sref.alpha, degToRAstr(com.sref.alpha, s) );
	printf("%20s   %.4f (deg) / %s\n", "REF_DELTA",
		com.sref.delta, degToDECstr(com.sref.delta, s) );
	printf("%20s   %.4f (deg)\n", "REF_ROLL",  com.sref.roll);

	printf("%20s   %s\n", "ABERRATION", com.aberration ? "YES" : "NO");
	if ( com.aberration || ATTITUDE_FILE == com.attitude ) {
		printf("%20s   %.3f / %s in UTC\n", "AETIME",
			   com.aetime, aetime2datestr(com.aetime, s) );
		if ( com.leapfile == com.o_leapfile ) {
			printf("%20s   '%s'\n", "LEAPFILE", com.leapfile);
		} else {
			printf("%20s   '%s' (%s)\n", "LEAPFILE",
				com.leapfile, com.o_leapfile);
		}
	}
	if ( com.aberration ) {
		printf("%20s   %d\n", "MJDREFI", com.mjdrefi);
		printf("%20s   %.17f\n", "MJDREFF", com.mjdreff);
	}

	printf("%20s   '%s'%s\n", "XRS_TELDEF", xrs.teldef_file,
		(xrs.teldef_file == xrs.o_teldef_file) ? "" : " (CALDB)");
	printf("%20s   '%s'%s\n", "HXD_TELDEF", hxd.teldef_file,
		(hxd.teldef_file == hxd.o_teldef_file) ? "" : " (CALDB)");
	printf("%20s   '%s'%s\n", "XIS0_TELDEF", xis[0].teldef_file,
		(xis[0].teldef_file == xis[0].o_teldef_file) ? "" : " (CALDB)");
	printf("%20s   '%s'%s\n", "XIS1_TELDEF", xis[1].teldef_file,
		(xis[1].teldef_file == xis[1].o_teldef_file) ? "" : " (CALDB)");
	printf("%20s   '%s'%s\n", "XIS2_TELDEF", xis[2].teldef_file,
		(xis[2].teldef_file == xis[2].o_teldef_file) ? "" : " (CALDB)");
	printf("%20s   '%s'%s\n", "XIS3_TELDEF", xis[3].teldef_file,
		(xis[3].teldef_file == xis[3].o_teldef_file) ? "" : " (CALDB)");
}

void
aecoordcalc_com(int *status)
{
#define NVAL	3
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"LEAPFILE",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"leap seconds table file name",
		"exit from this menu"
	};

	char *k;
	int answer[2];
	int nreply = 1;

	if ( *status ) {	/* ftools */

		*status = ANL_QUIT;

		if (
PILGetString(k="instrume", com.instrume) ) {
			goto pil_error;
		}
		CLstrupc(com.instrume);
		com.inst_id = aste_instrume_id(com.instrume);
		if ( ASTE_XRS_ID != com.inst_id &&
			 ASTE_HXD_ID != com.inst_id &&
			 ASTE_XIS0_ID != com.inst_id &&
			 ASTE_XIS1_ID != com.inst_id &&
			 ASTE_XIS2_ID != com.inst_id &&
			 ASTE_XIS3_ID != com.inst_id ) {
			anl_msg_error("\
%s: non-supported instrument name '%s'\n", pname, com.instrume);
			goto quit;
		}

		if (
PILGetString(k="coord", com.coord) ) {
			goto pil_error;
		}
		CLstrupc(com.instrume);
		if ( 0 == CLstricmp("ECS", com.coord) ||
			 0 == CLstricmp("J2000", com.coord) ) {
			if (
PILGetString(k="alpha", pos.alpha_string) ||
PILGetString(k="delta", pos.delta_string) ) {
				goto pil_error;
			}
			pos.alpha = parseRAstrToDeg(pos.alpha_string);
			pos.delta = parseDECstrToDeg(pos.delta_string);
		} else if ( 0 == CLstricmp("B1950", com.coord) ) {
			if (
PILGetString(k="alpha", pos.alpha_string) ||
PILGetString(k="delta", pos.delta_string) ) {
				goto pil_error;
			}
			pos.alpha = parseRAstrToDeg(pos.alpha_string);
			pos.delta = parseDECstrToDeg(pos.delta_string);
		} else if ( 0 == CLstricmp("GAL", com.coord) ||
			 0 == strcmp("GALACTIC", com.coord) ) {
			if (
PILGetReal(k="l", &pos.l) ||
PILGetReal(k="b", &pos.b) ) {
				goto pil_error;
			}
		} else if ( 0 == CLstricmp("SKY", com.coord) ) {
			if (
PILGetReal(k="x", &pos.skyx) ||
PILGetReal(k="y", &pos.skyy) ) {
				goto pil_error;
			}
		} else if ( 0 == CLstricmp("FOC", com.coord) ) {
			if (
PILGetReal(k="focx", &pos.focx) ||
PILGetReal(k="focy", &pos.focy) ) {
				goto pil_error;
			}
		} else if ( 0 == CLstricmp("DET", com.coord) ) {
			if (
PILGetReal(k="detx", &pos.detx) ||
PILGetReal(k="dety", &pos.dety) ) {
				goto pil_error;
			}
		} else if ( 0 == CLstricmp("OPTIC", com.coord) ) {
			if (
PILGetReal(k="theta", &pos.theta) ||
PILGetReal(k="phi", &pos.phi) ) {
				goto pil_error;
			}
		} else if ( 0 == CLstricmp("PIXEL", com.coord) &&
			  ASTE_XRS_ID == com.inst_id ) {
			if (
PILGetInt(k="pixel", &pos.pixel) ) {
				goto pil_error;
			}
			if ( pos.pixel < 0 || 31 < pos.pixel ) {
				anl_msg_error("\
%s: invalid PIXEL value %d\n", pname, pos.pixel);
				goto quit;
			}
			if (
PILGetInt(k="corner", &pos.corner) ) {
				goto pil_error;
			}
			if ( pos.corner < 0 || 4 < pos.corner ) {
				anl_msg_error("\
%s: invalid CORNER value %d\n", pname, pos.corner);
				goto quit;
			}
		} else if ( 0 == CLstricmp("ACT", com.coord) &&
			  ASTE_XIS0_ID <= com.inst_id && com.inst_id <= ASTE_XIS3_ID ) {
			if (
PILGetInt(k="actx", &pos.actx) ||
PILGetInt(k="acty", &pos.acty) ) {
				goto pil_error;
			}
		} else if ( 0 == CLstricmp("RAW", com.coord) &&
			  ASTE_XIS0_ID <= com.inst_id && com.inst_id <= ASTE_XIS3_ID ) {
			if (
PILGetInt(k="segment", &pos.segment) ) {
				goto pil_error;
			}
			if ( pos.segment < 0 || 3 < pos.segment ) {
				anl_msg_error("\
%s: invalid SEGMENT value %d\n", pname, pos.segment);
				goto quit;
			}
			if (
PILGetInt(k="rawx", &pos.rawx) ||
PILGetInt(k="rawy", &pos.rawy) ) {
				goto pil_error;
			}
		} else if ( 0 == CLstricmp("PPU", com.coord) &&
			  ASTE_XIS0_ID <= com.inst_id && com.inst_id <= ASTE_XIS3_ID ) {
			if (
PILGetInt(k="segment", &pos.segment) ) {
				goto pil_error;
			}
			if ( pos.segment < 0 || 3 < pos.segment ) {
				anl_msg_error("\
%s: invalid SEGMENT value %d\n", pname, pos.segment);
				goto quit;
			}
			if (
PILGetInt(k="ppux", &pos.ppux) ||
PILGetInt(k="ppuy", &pos.ppuy) ) {
				goto pil_error;
			}
		} else {
			anl_msg_error("\
%s: non-supported coordinates name '%s'\n", pname, com.coord);
			goto quit;
		}

		if (
PILGetString(k="output", com.output) ) {
			goto pil_error;
		}
		CLstrupc(com.output);
		com.output_inst_id = aste_instrume_id(com.output);
		if ( 0 == CLstricmp("FULL", com.output) ) {
			com.output_inst_id = 0;
		} else if (
			 ASTE_XRS_ID != com.inst_id &&
			 ASTE_HXD_ID != com.inst_id &&
			 ASTE_XIS0_ID != com.inst_id &&
			 ASTE_XIS1_ID != com.inst_id &&
			 ASTE_XIS2_ID != com.inst_id &&
			 ASTE_XIS3_ID != com.inst_id ) {
			anl_msg_error("\
%s: non-supported instrument name '%s'\n", pname, com.instrume);
			goto quit;
		}

		if (
PILGetFname(k="attitude", com.attitude) ) {
			goto pil_error;
		}
		if ( 0 == CLstricmp("USER", com.attitude) ) {
			com.attitude_type = ATTITUDE_USER;
			CLstrupc(com.attitude);
		} else if ( 0 == CLstricmp("KEY", com.attitude) ) {
			com.attitude_type = ATTITUDE_KEY;
			CLstrupc(com.attitude);
		} else {
			com.attitude_type = ATTITUDE_FILE;
		}

		if (
PILGetString(k="pointing", com.pointing) ) {
			goto pil_error;
		}
		CLstrupc(com.pointing);
		if ( 0 == CLstricmp("USER", com.pointing) ) {
			com.pointing_type = POINTING_USER;
		} else if ( 0 == CLstricmp("KEY", com.pointing) ) {
			com.pointing_type = POINTING_KEY;
		} else {
			anl_msg_error("\
%s: non-supported pointing type '%s'\n", pname, com.pointing);
			goto quit;
		}

		if ( ATTITUDE_KEY == com.attitude_type ||
			 POINTING_KEY == com.pointing_type ) {
			if (
PILGetFname(k="infile", com.infile) ) {
				goto pil_error;
			}
		}

		if ( ATTITUDE_USER == com.attitude_type ) {
			if (
PILGetReal(k="ea1", &com.ea1) ||
PILGetReal(k="ea2", &com.ea2) ||
PILGetReal(k="ea3", &com.ea3) ) {
				goto pil_error;
			}
			com.ea.phi   = DEG2RAD * com.ea1;
			com.ea.theta = DEG2RAD * com.ea2;
			com.ea.psi   = DEG2RAD * com.ea3;
		}

		if ( POINTING_USER == com.pointing_type ) {
			if (
PILGetString(k="ref_alpha", com.ref_alpha_string) ||
PILGetString(k="ref_delta", com.ref_delta_string) ||
PILGetReal  (k="ref_roll", &com.sref.roll) ) {
				goto pil_error;
			}
			com.sref.alpha = parseRAstrToDeg(com.ref_alpha_string);
			com.sref.delta = parseDECstrToDeg(com.ref_delta_string);
		}

		if (
PILGetBool(k="aberration", &com.aberration) ) {
			goto pil_error;
		}
		if ( com.aberration || ATTITUDE_FILE == com.attitude ) {
			if (
PILGetString(k="t", com.time_string) ||
PILGetFname (k="leapfile", com.o_leapfile) ) {
				goto pil_error;
			}
		}
		if ( com.aberration ) {
			if (
PILGetInt (k="mjdrefi", &com.mjdrefi) ||
PILGetReal(k="mjdreff", &com.mjdreff) ) {
				goto pil_error;
			}
		}

		if (
PILGetFname(k="xrs_teldef", xrs.o_teldef_file) ||
PILGetFname(k="hxd_teldef", hxd.o_teldef_file) ||
PILGetFname(k="xis0_teldef", xis[0].o_teldef_file) ||
PILGetFname(k="xis1_teldef", xis[1].o_teldef_file) ||
PILGetFname(k="xis2_teldef", xis[2].o_teldef_file) ||
PILGetFname(k="xis3_teldef", xis[3].o_teldef_file) ||
			 0 ) {
			goto pil_error;
		}

		*status = ANL_OK;;
		return;

	pil_error:
		anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
	quit:
		*status = ANL_QUIT;
		return;
	}

	for (;;) {
		char *p;
		CMinquir(pname, NVAL, names, help, nreply, answer);
		p = names[answer[1]-1];
		if ( 0 == strcmp("SHOW_PARAMETER", p) ) {
			show_parameter();
		} else if ( 0 == strcmp("LEAPFILE", p) ) {
			CLtxtrd(p, com.o_leapfile, sizeof(com.o_leapfile));
		} else if ( 0 == strcmp("EXIT", p) ) {
			break;
		}
	}
#undef NVAL

	*status = ANL_OK;
}

void
aecoordcalc_init(int *status)
{
	static struct CALDB_INDEX {
		char *telescop;
		char *instrume;
		char *codename;
		char *origname;
		char **realname_ptr;
	} caldb_index[] = {
		{ NULL, "XRS",  "TELDEF", xrs.o_teldef_file,    &xrs.teldef_file },
		{ NULL, "HXD",  "TELDEF", hxd.o_teldef_file,    &hxd.teldef_file },
		{ NULL, "XIS0", "TELDEF", xis[0].o_teldef_file, &xis[0].teldef_file },
		{ NULL, "XIS1", "TELDEF", xis[1].o_teldef_file, &xis[1].teldef_file },
		{ NULL, "XIS2", "TELDEF", xis[2].o_teldef_file, &xis[2].teldef_file },
		{ NULL, "XIS3", "TELDEF", xis[3].o_teldef_file, &xis[3].teldef_file },
		{ NULL, NULL, NULL, NULL, NULL }
	};

	CALDB_INFO caldb;
	ATTFILE *attfile;
	char *k, s1[80], s2[80];
	int i, istat, verbose;

	com.leapfile = aste_caldb_find_leapfile(com.o_leapfile);
	if ( NULL == com.leapfile ) {
		goto quit;
	}

	for (i = 0; NULL != caldb_index[i].realname_ptr; i++) {
		struct CALDB_INDEX *p = &caldb_index[i];
		aste_caldb_init(&caldb);
		if ( 0 == CLstricmp("CALDB", p->origname) ) {
			caldb.telescop = p->telescop;
			caldb.instrume = p->instrume;
			caldb.codename = p->codename;
			aste_caldb_get(&caldb);
			if ( 0 != caldb.status || 0 == caldb.nfound ) {
				anl_msg_error("\
%s: no CALDB entry for '%s' (status=%d)\n",
					pname, caldb.codename, caldb.status);
				goto quit;
			}
			if ( 1 != caldb.nfound ) {
				anl_msg_warning("\
%s: WARNING: multiple CALDB entry (nfound=%d) for '%s'\n",
					pname, caldb.nfound, caldb.codename);
			}
			*p->realname_ptr = caldb.filename;
		}
	}

	if ( com.aberration || ATTITUDE_FILE == com.attitude_type ) {

/* initialize aste_time */
		verbose = ( 0 == CLstricmp("none", com.leapfile) ) ? -1 : -2;
		if ( -1 == verbose ) printf("\n");
		if ( NULL == atMissionTimeInit(com.leapfile, verbose) ) {
			anl_msg_error("\
%s: atMissionTimeInit('%s') failed\n", pname, com.leapfile);
			goto quit;
		}

/* convert time string into aetime */
		com.aetime = datestr2aetime(com.time_string);
		sprintf(s1, "%.6f", com.aetime);
		if (
PILPutString(k="t", s1) ||
			 0 ) {
			anl_msg_error("\
%s: PILPutString('%s') failed\n", pname, k);
			goto quit;
		}

	}

	if ( ATTITUDE_FILE == com.attitude_type ) {

		attfile = aste_att_init(com.attitude);
		if ( NULL == attfile ) {
			anl_msg_error("\
%s: aste_att_init('%s') failed\n", pname, com.attitude);
			goto quit;
		}

		istat = aste_att_euler(com.aetime, &com.ea);
		if ( istat ) {
			anl_msg_error("\
%s: t=%.3f out of range of attfile '%s'\n", pname, com.aetime, com.attitude);
			goto quit;
		}
		com.ea1 = RAD2DEG * com.ea.phi;
		com.ea2 = RAD2DEG * com.ea.theta;
		com.ea3 = RAD2DEG * com.ea.psi;

		aste_att_close(attfile);

	}

	if ( ATTITUDE_KEY == com.attitude_type ||
		 POINTING_KEY == com.pointing_type ) {
		istat = read_infile();
		if ( istat ) {
			goto quit;
		}
	}

	sprintf(s1, "%.4f", com.sref.alpha);
	sprintf(s2, "%.4f", com.sref.delta);
	if (
PILPutString(k="ref_alpha", s1) ||
PILPutString(k="ref_delta", s2) ||
PILPutReal  (k="ref_roll", com.sref.roll) ||
PILPutReal  (k="ea1", com.ea1) ||
PILPutReal  (k="ea2", com.ea2) ||
PILPutReal  (k="ea3", com.ea3) ||
		 0 ) {
		anl_msg_error("\
%s: PILPut('%s') failed\n", pname, k);
		goto quit;
	}

/* read teldef files */
	xrs.teldef = aste_coord_init(NULL, "XRS", xrs.teldef_file);
	if ( NULL == xrs.teldef ) goto quit;
	hxd.teldef = aste_coord_init(NULL, "HXD", hxd.teldef_file);
	if ( NULL == hxd.teldef ) goto quit;
	xis[0].teldef = aste_coord_init(NULL, "XIS0", xis[0].teldef_file);
	if ( NULL == xis[0].teldef ) goto quit;
	xis[1].teldef = aste_coord_init(NULL, "XIS1", xis[1].teldef_file);
	if ( NULL == xis[0].teldef ) goto quit;
	xis[2].teldef = aste_coord_init(NULL, "XIS2", xis[2].teldef_file);
	if ( NULL == xis[0].teldef ) goto quit;
	xis[3].teldef = aste_coord_init(NULL, "XIS3", xis[3].teldef_file);
	if ( NULL == xis[0].teldef ) goto quit;

/* show current parameters */
	show_parameter();
	printf("\n"); fflush(NULL);

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
aecoordcalc_his(int *status)
{
	*status = ANL_OK;
}

void
aecoordcalc_bgnrun(int *status)
{
	*status = ANL_OK;
}

void
aecoordcalc_ana(int *nevent, int *eventid, int *status)
{
	int i, istat;

	*status = ANL_QUIT;

	istat = calc_ecs();
	if ( istat ) {
		anl_put_exit_status(istat);
		return;
	}

	istat = calc_xrs();
	if ( istat ) {
		anl_put_exit_status(istat);
		return;
	}

	istat = calc_hxd();
	if ( istat ) {
		anl_put_exit_status(istat);
		return;
	}

	for (i = 0; i < 4; i++) {
		istat = calc_xis(i);
		if ( istat ) {
			anl_put_exit_status(istat);
			return;
		}
	}

	print_ecs();
	printf("\n"); fflush(NULL);

	if ( 0 == com.output_inst_id || ASTE_XRS_ID == com.output_inst_id ) {
		print_xrs();
		printf("\n"); fflush(NULL);
	}

	if ( 0 == com.output_inst_id || ASTE_HXD_ID == com.output_inst_id ) {
		print_hxd();
		printf("\n"); fflush(NULL);
	}

	for (i = 0; i < 4; i++) {
		if ( 0 == com.output_inst_id ||
			 ASTE_XIS0_ID + i == com.output_inst_id ) {
			print_xis(i);
			printf("\n"); fflush(NULL);
		}
	}

}

void
aecoordcalc_endrun(int *status)
{
	*status = ANL_OK;
}

void
aecoordcalc_exit(int *status)
{
	*status = ANL_OK;
}
