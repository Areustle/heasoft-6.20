/* $Id: XRTthermalShield.c,v 1.4 2006/11/01 00:27:39 ishisaki Exp $ */
/************************************************************************

 XRTthermalShield.c

 2003-02-16	version 1.00	Y.ISHISAKI

 2003-09-08	version 1.10	Y.ISHISAKI
	Change BNK keyword, "XRSarf" -> "ASTEARF"

 2003-09-30	version 1.20	Y.ISHISAKI
	modified for HEADAS

 2005-12-03	version 1.30	Y.ISHISAKI
	change column name: "Energy" -> "ENERGY", "Transmission" -> "TRANSMIS"

 2006-07-24	version 1.4		Y.ISHISAKI
	support for CALDB
	use anl_msg_xxx() functions

************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include "anl.h"
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "pil.h"
#include "fitsio.h"
#include "aste_caldb.h"

static char pname[] = "XRTthermalShield";
char XRTthermalShield_version[] = "version 1.4";

/* XRTthermalShield parameters */
static struct {
	char *shieldfile, o_shieldfile[PIL_LINESIZE];
	int nrow;
	double *energy, *trans;
} com = {
	NULL, "none",	/* shieldfile, o_shieldfile */
	0,				/* nrow */
	NULL, NULL		/* energy, trans */
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
	MSG("%4s%-20s'%s'%s", "", "SHIELDFILE", com.shieldfile,
		(com.shieldfile == com.o_shieldfile) ? "" : " (CALDB)");
}

static int
write_fits_header(fitsfile *fp)
{
	char history[FLEN_FILENAME+FLEN_KEYWORD];
	int status = 0;

	sprintf(history, "%s %s", pname, XRTthermalShield_version);
	fits_write_history(fp, history, &status);
	sprintf(history, "  shieldfile=%s%s", com.shieldfile,
		(com.shieldfile == com.o_shieldfile) ? "" : " (CALDB)");
	fits_write_history(fp, history, &status);

	return status;
}

static int
read_shieldfile(char *shieldfile)
{
	int hdutype;
	fitsfile *fp;
	int nrow, col_en, col_tr, anul;
	char comment[FLEN_COMMENT];

	int istat = 0;

	fits_open_file(&fp, shieldfile, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: XRT thermal shield transmission '%s' open failed\n", pname, shieldfile);
		return istat;
	}

	fits_movrel_hdu(fp, 1, &hdutype, &istat);
	if ( istat ) {
		int istat2 = 0;
		fits_close_file(fp, &istat2);
		anl_msg_error("\
%s: no transmission data in '%s'\n", pname, shieldfile);
		return istat;
	}

	fits_read_key(fp, TINT, "NAXIS2", &nrow, comment, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('NAXIS2') failed (%d)\n", pname, istat);
		return istat;
	}
	fits_get_colnum(fp, CASEINSEN, "ENERGY", &col_en, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_colunm('ENERGY') failed (%d)\n", pname, istat);
		return istat;
	}
	fits_get_colnum(fp, CASEINSEN, "TRANSMIS", &col_tr, &istat);
	if ( istat ) {
		istat = 0;
		fits_get_colnum(fp, CASEINSEN, "TRANSMISSION", &col_tr, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_get_colunm('TRANSMIS') failed (%d)\n", pname, istat);
			return istat;
		}
	}

	com.energy = malloc(2 * sizeof(double) * nrow);
	com.trans = com.energy + nrow;
	if ( NULL == com.energy ) {
		anl_msg_error("\
%s: ENERGY/TRANSMIS malloc() failed for '%s'\n", pname, shieldfile);
		return -1;
	}

	if (
fits_read_col_dbl(fp, col_en, 1, 1, nrow, 0.0, com.energy, &anul, &istat) ||
fits_read_col_dbl(fp, col_tr, 1, 1, nrow, 0.0, com.trans,  &anul, &istat) ||
		0 ) {
		anl_msg_error("\
%s: ENERGY/TRANSMIS read error in '%s' (%d)\n", pname, shieldfile, istat);
		return istat;
	}

	fits_close_file(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, shieldfile, istat);
		return istat;
	}

	com.nrow = nrow;

	return 0;
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
XRTthermalShield_startup(int *status)
{
	com.shieldfile = com.o_shieldfile;
}

void
XRTthermalShield_com(int *status)
{
	static char *keytbl[] = {
		"SHOW",
		"SHIELDFILE",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"XRT thermal shield transmission file name",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
	char *k;

	if ( *status ) {	/* HEADAS */
		if ( PILGetFname(k="shieldfile", com.o_shieldfile) ) {
			goto quit;
		}
		*status = ANL_OK;;
		return;

	quit:
		anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
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
		} else if ( 0 == strcmp("SHIELDFILE", key) ) {
			CLtxtrd(key, com.shieldfile, sizeof(com.shieldfile));
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
XRTthermalShield_init(int *status)
{
	int istat, used;
	fitsfile *fp;
	char *arf_file;
	CALDB_INFO caldb;

	if ( 0 == CLstricmp("CALDB", com.o_shieldfile) ) {
		aste_caldb_init(&caldb);
		caldb.instrume = "XRT";
		caldb.codename = "FTRANS";
		aste_caldb_get(&caldb);
		if ( 0 != caldb.status || 0 == caldb.nfound ) {
			anl_msg_error("\
%s: no CALDB entry for '%s' (status=%d)\n",
				pname, caldb.codename, caldb.status);
			*status = ANL_QUIT;
			return;
		}
		if ( 1 != caldb.nfound ) {
			anl_msg_warning("\
%s: WARNING: multiple CALDB entry (nfound=%d) for '%s'\n",
				pname, caldb.nfound, caldb.codename);
		}
		com.shieldfile = caldb.filename;
	}

	EvsDef("XRTthermalShield:BEGIN");
	EvsDef("XRTthermalShield:ENTRY");
	EvsDef("XRTthermalShield:OK");

	show_parameter("%s:  *** show parameter ***");

	if ( 0 == CLstricmp("NONE", com.o_shieldfile) ) {
		anl_msg_info("\n\
%s: reading shield file '%s'\n", pname, com.shieldfile);
		istat = read_shieldfile(com.shieldfile);
		if ( istat ) {
			*status = ANL_QUIT;
			return;
		}
	}

	BnkGet("ASTEARF:FITS_PTR", sizeof(fp), &used, &fp);
	BnkGet("ASTEARF:ARF_FILE", sizeof(arf_file), &used, &arf_file);

	*status = write_fits_header(fp);
	if ( *status ) {
		anl_msg_error("\
%s: writing history failed for '%s'\n", pname, arf_file);
		*status = ANL_QUIT;
		return;
	}

	*status = ANL_OK;
}

void
XRTthermalShield_his(int *status)
{
	*status = ANL_OK;
}

void
XRTthermalShield_bgnrun(int *status)
{
	EvsSet("XRTthermalShield:BEGIN");

	*status = ANL_OK;
}

void
XRTthermalShield_ana(int nevent, int eventid, int *status)
{
	int used;
	double trans;
	double energ_lo, energ_hi, specresp, effarea;

	EvsfSetM("XRTthermalShield:ENTRY");

	if ( 0 == com.nrow ) {	/* shieldfile = 'none' */
		goto skip;
	}

	BnkfGetM("ASTEARF:ENERG_LO", sizeof(energ_lo), &used, &energ_lo);
	BnkfGetM("ASTEARF:ENERG_HI", sizeof(energ_hi), &used, &energ_hi);
	BnkfGetM("ASTEARF:SPECRESP", sizeof(specresp), &used, &specresp);
	BnkfGetM("ASTEARF:EFFAREA",  sizeof(effarea),  &used, &effarea);

	trans = integ_linlin(com.nrow, com.energy, com.trans, energ_lo, energ_hi);
	trans /= energ_hi - energ_lo;
/*	printf("%.4f %.4f %.6f\n", energ_lo, energ_hi, trans);*/

	specresp *= trans;
	effarea *= trans;

	BnkfPutM("ASTEARF:SPECRESP", sizeof(specresp), &specresp);
	BnkfPutM("ASTEARF:EFFAREA",  sizeof(effarea),  &effarea);

 skip:

	EvsfSetM("XRTthermalShield:OK");
}

void
XRTthermalShield_endrun(int *status)
{
	*status = ANL_OK;
}

void
XRTthermalShield_exit(int *status)
{
	*status = ANL_OK;
}
