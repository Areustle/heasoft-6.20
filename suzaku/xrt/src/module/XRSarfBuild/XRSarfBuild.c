/* $Id: XRSarfBuild.c,v 1.4 2006/11/01 00:27:38 ishisaki Exp $ */
/****************************************************
*
* XRSarfBuild.c
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
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "fitsio.h"

static char pname[] = "XRSarfBuild";
char XRSarfBuild_version[] = "version 1.10";

static int
write_fits_header(fitsfile *fp)
{
	char history[FLEN_FILENAME+FLEN_KEYWORD];
	int status = 0;

	sprintf(history, "%s %s", pname, XRSarfBuild_version);
	fits_write_history(fp, history, &status);

	return status;
}

void
XRSarfBuild_startup(int *status)
{
	;
}

void
XRSarfBuild_init(int *status)
{
	int used;
	fitsfile *fp;
	char *arf_file;

	EvsDef("XRSarfBuild:BEGIN");
	EvsDef("XRSarfBuild:ENTRY");
	EvsDef("XRSarfBuild:OK");

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
XRSarfBuild_com(int *status)
{
	*status = ANL_OK;
}

void
XRSarfBuild_his(int *status)
{
	*status = ANL_OK;
}

void
XRSarfBuild_bgnrun(int *status)
{
	EvsSet("XRSarfBuild:BEGIN");

	*status = ANL_OK;
}

void
XRSarfBuild_ana(int nevent, int eventid, int *status)
{
	fitsfile *fp;
	char *arf_file;
	int irow, used;
	double energ_lo, energ_hi, specresp, effarea, exposure;

	EvsfSetM("XRSarfBuild:ENTRY");

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

	EvsfSetM("XRSarfBuild:OK");
	*status = ANL_OK;
}

void
XRSarfBuild_endrun(int *status)
{
	*status = ANL_OK;
}

void
XRSarfBuild_exit(int *status)
{
	*status = ANL_OK;
}
