/* $Id: XISarfBuild.c,v 1.4 2006/11/01 00:27:38 ishisaki Exp $ */
/****************************************************
*
* XISarfBuild.c
*
* 2003-09-08	version 1.10	Y.ISHISAKI
*	Made from XISarfBuild.c version 1.10
*
****************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "fitsio.h"

static char pname[] = "XISarfBuild";
char XISarfBuild_version[] = "version 1.10";

static int
write_fits_header(fitsfile *fp)
{
	char history[FLEN_FILENAME+FLEN_KEYWORD];
	int status = 0;

	sprintf(history, "%s %s", pname, XISarfBuild_version);
	fits_write_history(fp, history, &status);

	return status;
}

void
XISarfBuild_startup(int *status)
{
	;
}

void
XISarfBuild_init(int *status)
{
	int used;
	fitsfile *fp;
	char *arf_file;

	EvsDef("XISarfBuild:BEGIN");
	EvsDef("XISarfBuild:ENTRY");
	EvsDef("XISarfBuild:OK");

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
XISarfBuild_com(int *status)
{
	*status = ANL_OK;
}

void
XISarfBuild_his(int *status)
{
	*status = ANL_OK;
}

void
XISarfBuild_bgnrun(int *status)
{
	EvsSet("XISarfBuild:BEGIN");

	*status = ANL_OK;
}

void
XISarfBuild_ana(int nevent, int eventid, int *status)
{
	fitsfile *fp;
	char *arf_file;
	int irow, used;
	double energ_lo, energ_hi, specresp, effarea, exposure;

	EvsfSetM("XISarfBuild:ENTRY");

	BnkfGetM("ASTEARF:ROW", sizeof(irow), &used, &irow);
	BnkfGetM("ASTEARF:ENERG_LO", sizeof(energ_lo), &used, &energ_lo);
	BnkfGetM("ASTEARF:ENERG_HI", sizeof(energ_hi), &used, &energ_hi);
	BnkfGetM("ASTEARF:SPECRESP", sizeof(specresp), &used, &specresp);
	BnkfGetM("ASTEARF:EFFAREA",  sizeof(effarea),  &used, &effarea);
	BnkfGetM("ASTEARF:EXPOSURE", sizeof(exposure), &used, &exposure);
	BnkfGetM("ASTEARF:FITS_PTR", sizeof(fp), &used, &fp);

	*status = 0;
	fits_write_col_dbl(fp, 1, irow, 1, 1, &energ_lo, status);
	fits_write_col_dbl(fp, 2, irow, 1, 1, &energ_hi, status);
	fits_write_col_dbl(fp, 3, irow, 1, 1, &specresp, status);
	fits_write_col_dbl(fp, 4, irow, 1, 1, &effarea,  status);
	fits_write_col_dbl(fp, 5, irow, 1, 1, &exposure, status);
	if ( *status ) {
		BnkGet("ASTEARF:ARF_FILE", sizeof(arf_file), &used, &arf_file);
		fprintf(stderr, "\
%s: writing data failed for '%s'\n\
    at ROW=%d, ENERG_LO=%.6f, ENERG_HI=%.6f\n",
				pname, arf_file, irow, energ_lo, energ_hi);
		*status = ANL_QUIT;
		return;
	}

	EvsfSetM("XISarfBuild:OK");
	*status = ANL_OK;
}

void
XISarfBuild_endrun(int *status)
{
	*status = ANL_OK;
}

void
XISarfBuild_exit(int *status)
{
	*status = ANL_OK;
}
