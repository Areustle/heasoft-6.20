/*
 SimASTE_ECStoXRTIN.c
   SimASTE module : convert RA,DEC -> XRTINtheta/phi

  1998/02/24 version 1.00	Y.ISHISAKI
	coded first

  1998/04/14 version 1.01	Y.ISHISAKI
	SimASTE_DRNDTS -> aste_drndts

  1998/09/10 version 1.10	Y.ISHISAKI
	work with ftools parameter interface

  2003-09-13 version 1.20	Y.ISHISAKI
	use latest aste_coord in astetool-1.24

  2006-04-09 version 1.3	Y.ISHISAKI
	change version number only

  2006-08-05 version 2.2	Y.ISHISAKI
	add aberration, aperture_cosine parameter, EVS SimASTE:APERTURE_COSINE
	pre-calculate r2min, r2max
	add write_history(), BnkGet/Put SimASTE:WRITE_HISTORY:FUNC in _init()
	BnkGet/Put SimASTE:GEOMAREA in _init()

  2006-10-21 version 2.3	Y.ISHISAKI
	fix min/max|radius/angle in HISTORY
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <unistd.h>
#include "fitsio.h"
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "com.h"
#include "pil.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_rand.h"
#include "aste_time.h"
#include "SimASTE.h"

static char pname[] = "SimASTE_ECStoXRTIN";
char SimASTE_ECStoXRTIN_version[] = "version 2.2";

static struct {
	int aberration;
	int mjdrefi;
	double mjdreff;
	int aperture_cosine;
	double minRadius, maxRadius;
	double minAngle, maxAngle;
	double r2min, r2max, geomarea;
	TELDEF *teldef;
	int (*prev_write_history)(fitsfile *);
} com = {
	ANL_YES,		/* aberration */
	0,				/* mjdrefi */
	0.0,			/* mjdreff */
	ANL_YES,		/* aperture_cosine */
	59.0, 200.0,	/* minRadius, maxRadius */
	0.0, 360.0,		/* minAngle, maxAngle */
	0.0, 0.0, 0.0,	/* r2min, r2max, geomarea */
	NULL,			/* teldef */
	NULL			/* prev_write_history */
};

static int
write_history(fitsfile *fp)
{
	int istat;
	char history[PIL_LINESIZE + FLEN_VALUE];

	if ( NULL != com.prev_write_history &&
		 write_history != com.prev_write_history ) {
		istat = (*com.prev_write_history)(fp);
		if ( istat ) {
			return istat;
		}
	}

	istat = SimASTE_write_history_pname(fp, pname);

	if ( com.aberration ) {
		sprintf(history, "\
  aberration=yes  mjdrefi=%d  mjdreff=%.17f", com.mjdrefi, com.mjdreff);
	} else {
		sprintf(history, "\
  aberration=no");
	}
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  aperture_cosine=%s", com.aperture_cosine ? "yes" : "no");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  minradius=%.1f  maxradius=%.1f  minangle=%.5f  maxangle=%.5f",
		com.minRadius, com.maxRadius, com.minAngle, com.maxAngle);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
    GEOMAREA=%.4f cm2", com.geomarea);
	fits_write_history(fp, history, &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
		return istat;
	}

	return istat;
}

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

	MSG("%4s%-20s%s", "", "ABERRATION", com.aberration ? "YES" : "NO");
	MSG("%4s%-20s%s", "", "APERTURE_COSINE",
		com.aperture_cosine ? "YES" : "NO");
	MSG("%4s%-20s%.3f (deg)", "", "MINANGLE", com.minAngle);
	MSG("%4s%-20s%.3f (deg)", "", "MAXANGLE", com.maxAngle);
	MSG("%4s%-20s%.5f (mm)", "", "MINRADIUS", com.minRadius);
	MSG("%4s%-20s%.5f (mm)", "", "MAXRADIUS", com.maxRadius);
	MSG("%4s%-20s%.4f (cm2)", "", "GEOMAREA", com.geomarea);
}

void
SimASTE_ECStoXRTIN_startup(int *status)
{
	com.mjdrefi = aste_mjdrefi();
	com.mjdreff = aste_mjdreff();
}

void
SimASTE_ECStoXRTIN_com(int *status)
{
	static char *keytbl[] = {
		"SHOW",
		"minRadius", "maxRadius",
		"minAngle", "maxAngle",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"Set inner radius [mm]",
		"Set outer radius [mm]",
		"Set minimum angle [deg]",
		"Set maximum angle [deg]",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);

	char *k;

	if ( *status ) {	/* ftools */
		if ( PILGetBool(k="aperture_cosine", &com.aperture_cosine) ||
			 PILGetBool(k="aberration", &com.aberration) ||
			 PILGetReal(k="minangle", &com.minAngle) ||
			 PILGetReal(k="maxangle", &com.maxAngle) ||
			 PILGetReal(k="minradius", &com.minRadius) ||
			 PILGetReal(k="maxradius", &com.maxRadius) ||
			 0 ) {
			anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
			*status = ANL_QUIT;
			return;
		}
		*status = ANL_OK;
		return;
	}

	for (;;) {
		char *key;
		int ans[2];

		CMinquir(pname, nkey, keytbl, help, 1, ans);
		key = keytbl[ans[1]-1];
		if ( 0 == strcmp("SHOW", key) ) {
			show_parameter("%s:  *** show parameter ***");
		} else if ( 0 == strcmp("minRadius", key) ) {
			CLfdprd(key, &com.minRadius);
		} else if ( 0 == strcmp("maxRadius", key) ) {
			CLfdprd(key, &com.maxRadius);
		} else if ( 0 == strcmp("minAngle", key) ) {
			CLfdprd(key, &com.minAngle);
		} else if ( 0 == strcmp("maxAngle", key) ) {
			CLfdprd(key, &com.maxAngle);
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
SimASTE_ECStoXRTIN_init(int *status)
{
	static int (*func)(fitsfile *fp) = write_history;
	int used;
	double geomarea;

	EvsDef("SimASTE_ECStoXRTIN:BEGIN");
	EvsDef("SimASTE_ECStoXRTIN:ENTRY");
	EvsDef("SimASTE_ECStoXRTIN:OK");
	EvsDef("SimASTE:APERTURE_COSINE");

	com.prev_write_history = NULL;
	BnkGet("SimASTE:WRITE_HISTORY:FUNC", sizeof(com.prev_write_history),
		&used, &com.prev_write_history);
	BnkPut("SimASTE:WRITE_HISTORY:FUNC", sizeof(func), &func);

	BnkfGetM("SimASTE:TELDEF", sizeof(com.teldef), &used, &com.teldef);

	com.r2min = com.minRadius * com.minRadius;
	com.r2max = com.maxRadius * com.maxRadius;
	com.geomarea = M_PI * (com.r2max - com.r2min) / 100;
	com.geomarea *= (com.maxAngle - com.minAngle) / 360;

	show_parameter("%s:  *** show parameter ***");

	geomarea = 0.0;
	BnkGet("SimASTE:GEOMAREA", sizeof(geomarea), &used, &geomarea);
	if ( 0.0 == geomarea ) {
		anl_msg_warning("\n\
%s: WARNING: assumed GEOMAREA not known, old photon file.\n", pname);
	} else if ( 0.01 <= fabs(com.geomarea - geomarea)  ) {
		anl_msg_warning("\n\
%s: WARNING: GEOMAREA does not match assumed value of %.4f cm2\n",
			pname, geomarea);
	}
	BnkPut("SimASTE:GEOMAREA", sizeof(com.geomarea), &com.geomarea);

	*status = ANL_OK;
}

void
SimASTE_ECStoXRTIN_his(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_ECStoXRTIN_bgnrun(int *status)
{
	EvsSet("SimASTE_ECStoXRTIN:BEGIN");
	*status = ANL_OK;
}

void
SimASTE_ECStoXRTIN_ana(int nevent, int eventid, int *status)
{
	int used;
	double ra, dec, detx_ch, dety_ch, xrtx_mm, xrty_mm, photon_time;
	AtEulerAng ea;
	double radius, angle, x_in, y_in, theta_in, phi_in;

	EvsfSetM("SimASTE_ECStoXRTIN:ENTRY");

	BnkfGetM("SimASTE:EULER", sizeof(ea), &used, &ea);
	BnkfGetM("SimASTE:RA", sizeof(ra), &used, &ra);
	BnkfGetM("SimASTE:DEC", sizeof(dec), &used, &dec);
	if ( com.aberration ) {
	BnkfGetM("SimASTE:PHOTON_TIME", sizeof(photon_time), &used, &photon_time);
		aste_inv_aberration(photon_time, com.mjdrefi, com.mjdreff, &ra, &dec);
	}
	aste_ecs2det(com.teldef, &ea, ra, dec, &detx_ch, &dety_ch);
	aste_det2xrt(com.teldef, detx_ch, dety_ch, &xrtx_mm, &xrty_mm);
	aste_xrt_rec2pol(com.teldef, xrtx_mm, xrty_mm, &theta_in, &phi_in);

/* consider aperture decrease by cosine factor */
	if ( com.aperture_cosine ) {
		if ( cos(theta_in*ARCMIN2RAD) < aste_drndts() ) {
			EvsfSetM("SimASTE:APERTURE_COSINE");
			*status = ANL_SKIP;
			return;
		}
	}

/* randomize x_in, y_in */
	radius = sqrt( aste_drndts() * ( com.r2max - com.r2min ) + com.r2min );
	angle = com.minAngle + (com.maxAngle - com.minAngle) * aste_drndts();
	angle = angle * DEG2RAD;
	x_in = radius * cos(angle);
	y_in = radius * sin(angle);

/* put data to BNK */
	BnkfPutM("SimASTE:XRTINtheta", sizeof(theta_in), &theta_in);
	BnkfPutM("SimASTE:XRTINphi", sizeof(phi_in), &phi_in);
	BnkfPutM("SimASTE:XRTINX", sizeof(x_in), &x_in);
	BnkfPutM("SimASTE:XRTINY", sizeof(y_in), &y_in);

	EvsfSetM("SimASTE_ECStoXRTIN:OK");

	*status = ANL_OK;
}

void
SimASTE_ECStoXRTIN_endrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_ECStoXRTIN_exit(int *status)
{
	*status = ANL_OK;
}
