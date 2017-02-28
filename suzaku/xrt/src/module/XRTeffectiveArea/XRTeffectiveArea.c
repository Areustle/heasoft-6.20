/* $Id: XRTeffectiveArea.c,v 1.4 2006/11/01 00:27:39 ishisaki Exp $ */
/****************************************************
*
* XRTeffectiveArea.c
*
* 2003-09-30	version 1.20	Y.ISHISAKI
*	modified for HEADAS
*
* 2003-09-08	version 1.10	Y.ISHISAKI
*	Change BNK keyword, "XRSarf" -> "ASTEARF"
*
* 2003-02-16	version 1.00	Y.ISHISAKI
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

static char pname[] = "XRTeffectiveArea";
char XRTeffectiveArea_version[] = "version 1.20";

/* XRTeffectiveArea parameters */
static struct {
	char area_file[PIL_LINESIZE];
	int nrow;
	double *energy, *area;
} com = {
	"none",		/* area_file */
	0,			/* nrow */
	NULL, NULL	/* energy, area */
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
	MSG("%4s%-20s'%s'", "", "AREA_file", com.area_file);
}

static int
write_fits_header(fitsfile *fp)
{
	char history[FLEN_FILENAME+FLEN_KEYWORD];
	int status = 0;

	sprintf(history, "%s %s", pname, XRTeffectiveArea_version);
	fits_write_history(fp, history, &status);
	sprintf(history, "  ea_file=%s", com.area_file);
	fits_write_history(fp, history, &status);

	return status;
}

static int
read_area_file(char *area_file)
{
	int status, hdutype;
	fitsfile *fp;

	status = 0;
	fits_open_file(&fp, area_file, READONLY, &status);
	if ( status ) {
		fprintf(stderr, "\
%s: XRT effective area table '%s' open failed\n", pname, area_file);
		return -1;
	}

	for (;;) {
		int ny, col_energy, col_area, anynul;
		char comment[80];
		int exact = 1;

		status = 0;
		fits_movrel_hdu(fp, 1, &hdutype, &status);
		if ( status ) {
			fits_close_file(fp, &status);
			fprintf(stderr, "\
%s: no effective area data in '%s'\n", pname, area_file);
			return -1;
		}
		fits_read_key(fp, TINT, "NAXIS2", &ny, comment, &status);
		ffgcno(fp, exact, "Energy", &col_energy, &status);
		ffgcno(fp, exact, "Area", &col_area, &status);
		com.energy = malloc(2 * sizeof(double) * ny);
		com.area = com.energy + ny;
		if ( NULL == com.energy ) {
			fprintf(stderr, "\
%s: Energy/Area malloc failed for '%s'\n", pname, area_file);
			return -1;
		}
		ffgcvd(fp, col_energy, 1, 1, ny, 0.0, com.energy, &anynul, &status);
		ffgcvd(fp, col_area,   1, 1, ny, 0.0, com.area,   &anynul, &status);
		fits_close_file(fp, &status);
		if ( status ) {
			fprintf(stderr, "\
%s: Energy/Area column read error for '%s'\n", pname, area_file);
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

void
XRTeffectiveArea_startup(int *status)
{
	;
}

void
XRTeffectiveArea_com(int *status)
{
	static char *keytbl[] = {
		"SHOW",
		"AREA_file",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"XRT effective area table file name",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);

	if ( *status ) {	/* HEADAS */
		*status = PILGetFname("ea_file", com.area_file);
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
		} else if ( 0 == strcmp("AREA_file", key) ) {
			CLtxtrd(key, com.area_file, sizeof(com.area_file));
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
XRTeffectiveArea_init(int *status)
{
	int used;
	fitsfile *fp;
	char *arf_file;

	EvsDef("XRTeffectiveArea:BEGIN");
	EvsDef("XRTeffectiveArea:ENTRY");
	EvsDef("XRTeffectiveArea:OK");

	show_parameter("%s:  *** show parameter ***");

	printf("\n\
%s: reading AREA file '%s'\n", pname, com.area_file);
	com.nrow = read_area_file(com.area_file);
	if ( com.nrow < 0 ) {
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

	*status = ANL_OK;
}

void
XRTeffectiveArea_his(int *status)
{
	*status = ANL_OK;
}

void
XRTeffectiveArea_bgnrun(int *status)
{
	EvsSet("XRTeffectiveArea:BEGIN");

	*status = ANL_OK;
}

void
XRTeffectiveArea_ana(int nevent, int eventid, int *status)
{
	int used;
	double area;
	double energ_lo, energ_hi, specresp, effarea;

	EvsfSetM("XRTeffectiveArea:ENTRY");

	BnkfGetM("ASTEARF:ENERG_LO", sizeof(energ_lo), &used, &energ_lo);
	BnkfGetM("ASTEARF:ENERG_HI", sizeof(energ_hi), &used, &energ_hi);
	BnkfGetM("ASTEARF:SPECRESP", sizeof(specresp), &used, &specresp);
	BnkfGetM("ASTEARF:EFFAREA",  sizeof(effarea),  &used, &effarea);

	area = integ_linlin(com.nrow, com.energy, com.area, energ_lo, energ_hi);
	area /= energ_hi - energ_lo;

	specresp *= area;
	effarea *= area;

	BnkfPutM("ASTEARF:SPECRESP", sizeof(specresp), &specresp);
	BnkfPutM("ASTEARF:EFFAREA",  sizeof(effarea),  &effarea);

	EvsfSetM("XRTeffectiveArea:OK");
}

void
XRTeffectiveArea_endrun(int *status)
{
	*status = ANL_OK;
}

void
XRTeffectiveArea_exit(int *status)
{
	*status = ANL_OK;
}
