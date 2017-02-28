/*
  aste_caldb: GTI support routines for SUZAKU

	2006-07-25	Y.ISHISAKI	version 1.0

	2006-08-12	Y.ISHISAKI	version 1.1
		support text format goodtime file written by xselect in aste_gti_read()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "fitsio.h"
#include "aste_gti.h"

static char pname[] = "aste_gti";
char aste_gti_version[] = "1.1";

/************************************************************************
int aste_gti_zero()		: initialize GTI as zero

Input/Output:
	GTI_DATA *gti_ptr	: pointer to GTI

Return_Values:
	0					: success
	others  			: CFITSIO errors (defined in fitsio.h)
************************************************************************/
int
aste_gti_zero(GTI_DATA *gp)
{
	gp->ngti = 0;
	gp->tstart = 0.0;
	gp->tstop = 0.0;
	gp->telapse = 0.0;
	gp->ontime = 0.0;
	gp->start = NULL;
	gp->stop = NULL;

	return 0;
}

static int
check_line_empty(char *line)
{
	while ( *line ) {
		if ( ' ' < *line ) {
			return ANL_FALSE;
		}
		line++;
	}
	return ANL_TRUE;
}

static int
aste_gti_read_as_text(GTI_DATA *gp, FILE *fp)
{
	int i, n, nalloc, istat;
	double t0, t1;
	char line[1024];
	GTI_DATA g;

	n = 0;
	while ( NULL != fgets(line, sizeof(line), fp) ) {
		if ( strlen(line) == sizeof(line) - 1 ) {
			anl_msg_error("\
%s: too long line in GTI file\n", pname);
			istat = BAD_ROW_WIDTH;	/* sum of column widths not = NAXIS1 */
			goto error;
		}
		if ( check_line_empty(line) ) {
			continue;
		}
		if ( 2 != sscanf(line, "%lf%lf", &t0, &t1) ) {
			break;
		}
		n++;
	}

	rewind(fp);
	nalloc = 2 * n;
	g.ngti = n;
	g.start = malloc( nalloc * sizeof(*g.start) );
	g.stop = &g.start[n];
	if ( NULL == g.start ) {
		anl_msg_error("\
%s: malloc( nalloc=%d ) failed for GTI_DATA\n", pname, nalloc);
		istat = NGP_NO_MEMORY;
		goto error;
	}

	g.ontime = 0.0;
	for (i = 0; i < n; i++) {
		if ( NULL == fgets(line, sizeof(line), fp) ) {
			anl_msg_error("\
%s: GTI file read error at line=%d\n", pname, i+1);
			istat = READ_ERROR;
			goto error;
		}
		if ( strlen(line) == sizeof(line) - 1 ) {
			anl_msg_error("\
%s: too long line in GTI file\n", pname);
			istat = BAD_ROW_WIDTH;	/* sum of column widths not = NAXIS1 */
			goto error;
		}
		if ( check_line_empty(line) ) {
			continue;
		}
		if ( 2 != sscanf(line, "%lf%lf", &t0, &t1) ) {
			anl_msg_error("\
%s: GTI file read error at line=%d\n", pname, i+1);
			istat = READ_ERROR;
			goto error;
		}
		if ( t1 < t0 ) {
			anl_msg_error("\
%s: STOP:%.3f < START:%.3f in GTI row:%d\n", pname, t1, t0, i+1);
			istat = BAD_ORDER;
			goto error;
		}
		g.start[i] = t0;
		g.stop[i] = t1;
		g.ontime += t1 - t0;
	}

	g.tstart = g.start[0];
	g.tstop  = g.stop[g.ngti-1];
	g.telapse = g.tstop - g.tstart;

	*gp = g;
	return 0;

 error:
	if ( NULL != g.start ) {
		free(g.start);
	}

	aste_gti_zero(gp);

	return istat;
}

/************************************************************************
int aste_gti_read()		: read GTI_DATA from FITS or text file

Input:
	char *gtifile		: file name to read

Input/Output:
	GTI_DATA *gti_ptr	: pointer to GTI

Return_Values:
	0					: success
	others  			: CFITSIO errors (defined in fitsio.h)
************************************************************************/
int
aste_gti_read(GTI_DATA *gp, char *gtifile)
{
	fitsfile *fp;
	int hdunum, hdutype, nalloc, anul, igti;
	struct { int START, STOP, TIME; } c;
	char *k, card[FLEN_CARD];
	double t0, t1;
	GTI_DATA g;
	FILE *fq;

	int istat = 0, istat2 = 0;

	fp = NULL;
	g.start = NULL;

	fits_open_file(&fp, gtifile, READONLY, &istat);
	if ( istat ) {
		fq = fopen(gtifile, "r");
		if ( NULL == fq ) {
			anl_msg_error("\
%s: GTI file '%s' open failed (%d)\n", pname, gtifile, istat);
			goto error;
		}
		anl_msg_warning("\
%s: WARNING: fits_open_file('%s') failed (%d),\n\
%s: WARNING: try to read as a text file\n", pname, gtifile, istat, pname);
		istat = aste_gti_read_as_text(gp, fq);
		fclose(fq);
		return istat;
	}

	fits_get_hdu_num(fp, &hdunum);
	if ( 1 == hdunum ) {	/* primary HDU */
		fits_movnam_hdu(fp, BINARY_TBL, "GTI", 0, &istat);
		if ( istat ) {
			istat = 0;
			fits_movnam_hdu(fp, BINARY_TBL, "STDGTI", 0, &istat);
			if ( istat ) {
				anl_msg_warning("\
%s: WARNING: no GTI or STDGTI extension found, using 1st extension\n", pname);
				istat = 0;
				fits_movabs_hdu(fp, 2, &hdutype, &istat);
				if ( istat ) {
					anl_msg_error("\
%s: moving to 1st extension failed\n", pname);
					goto error;
				}
			}
		}
	}

	fits_read_card(fp, k="NAXIS2", card, &istat);
	fits_read_key(fp, TINT, k, &g.ngti, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed\n", pname, k);
		goto error;
	}

	nalloc = 2 * g.ngti;
	fits_get_colnum(fp, CASEINSEN, "START", &c.START, &istat);
	fits_get_colnum(fp, CASEINSEN, "STOP", &c.STOP, &istat);
	if ( istat ) {
		istat = 0;
		fits_get_colnum(fp, CASEINSEN, "TIME", &c.TIME, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: no START/STOP/TIME column found\n", pname);
			goto error;
		}
		anl_msg_warning("\
%s: WARNING: no START/STOP column, using TIME column\n", pname);
		c.START = c.TIME;
		c.STOP = 0;
		nalloc = g.ngti;
	}

	g.start = malloc( nalloc * sizeof(*g.start) );
	if ( NULL == g.start ) {
		istat = NGP_NO_MEMORY;
		anl_msg_error("\
%s: malloc( nalloc=%d ) failed for GTI_DATA\n", pname, nalloc);
		goto error;
	}
	g.stop = ( 0 == c.STOP ) ? g.start : &g.start[g.ngti];

	fits_read_col_dbl(fp, c.START, 1, 1, g.ngti, 0., g.start, &anul, &istat);
	if ( g.start != g.stop ) {
		fits_read_col_dbl(fp, c.STOP, 1, 1, g.ngti, 0., g.stop, &anul, &istat);
	}

	if ( istat ) {
		anl_msg_error("\
%s: fits_read_col('START/STOP/TIME') failed\n", pname);
		goto error;
	}

	fits_close_file(fp, &istat);
	fp = NULL;
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file() failed for GTI file\n", pname);
		goto error;
	}

	g.tstart = g.start[0];
	g.tstop  = g.stop[g.ngti-1];
	g.telapse = g.tstop - g.tstart;
	g.ontime = 0.0;

	if ( g.start != g.stop ) {
		for (igti = 0; igti < g.ngti; igti++) {
			t0 = g.start[igti];
			t1 = g.stop[igti];
			if ( t1 < t0 ) {
				anl_msg_error("\
%s: STOP:%.3f < START:%.3f in GTI row:%d\n", pname, t1, t0, igti+1);
				istat = BAD_ORDER;
				goto error;
			}
			g.ontime += t1 - t0;
		}
	}

	*gp = g;

	return 0;

 error:
	if ( NULL != g.start ) {
		free(g.start);
	}

	if ( NULL != fp ) {
		fits_close_file(fp, &istat2);
	}

	aste_gti_zero(gp);

	return istat;
}

/************************************************************************
int aste_gti_shrink()	: shrink redundant GTI

Input:
	GTI_DATA *gp		: pointer to GTI

Return_Values:
	0					: success
	others  			: CFITSIO errors (defined in fitsio.h)
************************************************************************/
int
aste_gti_shrink(GTI_DATA *gp)
{
	return 0;
}

/************************************************************************
int aste_gti_and()		: make AND of two GTIs

Input:
	GTI_DATA *g1p		: pointer to 1st GTI
	GTI_DATA *g2p		: pointer to 2nd GTI
	GTI_DATA *g3p		: resultant GTI of (1st GTI .AND. 2nd GTI)

Return_Values:
	0					: success
	others  			: CFITSIO errors (defined in fitsio.h)
************************************************************************/
int
aste_gti_and(GTI_DATA *g1p, GTI_DATA *g2p, GTI_DATA *g3p)
{
	return 0;
}

/************************************************************************
int aste_gti_or()		: make OR of two GTIs

Input:
	GTI_DATA *gti1		: pointer to 1st GTI
	GTI_DATA *gti2		: pointer to 2nd GTI
	GTI_DATA *gti_new	: resultant GTI of (1st GTI .OR. 2nd GTI)

Return_Values:
	0					: success
	others  			: CFITSIO errors (defined in fitsio.h)
************************************************************************/
int
aste_gti_or(GTI_DATA *g1p, GTI_DATA *g2p, GTI_DATA *g3p)
{
	return 0;
}

/************************************************************************
int aste_gti_free()		: free allocated memory for GTI

Input:
	GTI_DATA *gp		: pointer to GTI

Return_Values:
	0					: success
	others  			: CFITSIO errors (defined in fitsio.h)
************************************************************************/
int
aste_gti_free(GTI_DATA *gp)
{
	if ( NULL != gp->start && gp->start != &gp->tstart ) {
		free(gp->start);
	}
	aste_gti_zero(gp);
	return 0;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
