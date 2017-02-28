/*
	xrs_gain.c		XRS gain releated functions

	2005/05/09 Y.ISHISAKI	version 1.6
		created for XRSgainhistMake-1.6

	2005/07/05 Y.ISHISAKI	version 1.7
		moved from module/XRSgainhistMake/1.6/
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "xrs_gain.h"

static char *pname = "xrs_gain";
char xrs_gain_version[] = "1.7";

#define DEFAULT_PI_ESCAL	0.5		/* PI_ESCAL value for text file */

static XRS_GAINS *
read_text_file(char *filename, int *status)
{
	FILE *fp;
	int i, n, ip, istat;
	XRS_GAINS *gs;
	XRS_GAIN_PARAMS *g;
	char linebuf[1024];

	fp = fopen(filename, "r");
	if ( NULL == fp ) {
		fprintf(stderr, "\
%s: fopen('%s') failed (%d)\n", pname, filename, errno);
		istat = errno;
		goto error;
	}

	n = 32;
	gs = malloc( sizeof(*gs) + n * sizeof(gs->g[0]) + strlen(filename)+1 );
	if ( NULL == gs ) {
		fprintf(stderr, "\
%s: malloc() failed for XRS_GAINS (n=%d)\n", pname, n);
		istat = -1;
		goto error;
	}
	gs->g = (XRS_GAIN_PARAMS *)&gs[1];
	gs->filename = (char *)&gs->g[n];
	strcpy(gs->filename, filename);
	gs->pi_escal = DEFAULT_PI_ESCAL;
	gs->ng = n;

	for (i = 0; i < n; i++) {
		g = &gs->g[i];

		if ( NULL == fgets(linebuf, sizeof(linebuf), fp) ) {
			fprintf(stderr, "\
%s: fgets() failed for PIXEL=%d\n", pname, i);
			free(gs);
			istat = -1;
			goto error;
		}
		if ( '\0' == linebuf[0] ||
			 '#' == linebuf[0] ||
			 ';' == linebuf[0] ||
			 '!' == linebuf[0] ||
			 '%' == linebuf[0] ) {
			continue;		/* ignore this line as a comment */
		}
		g->start = 0.0;
		g->stop = 0.0;
		g->method = 0;
		g->p = g->pbuf;

		for (ip = 0; ip < XRS_GAIN_POLY_MAX; ip++) {
			g->p[ip] = 0.0;
		}

		g->np = sscanf(linebuf, "%d%lf%lf%lf%lf",
			&g->pixel, &g->p[1], &g->p[2], &g->p[3], &g->p[4]);
		g->np--;
		if ( g->np < 0 || i != g->pixel ) {
			fprintf(stderr, "\
%s: invalid gain parameters at\n%s", pname, linebuf);
			free(gs);
			istat = -1;
			goto error;
		}
	}

	return gs;

 error:
	if ( NULL != status ) {
		*status = istat;
	}
	return NULL;
}

XRS_GAINS *
xrs_gain_file_read(char *filename, int *status)
{
	fitsfile *fp;
	char *k, colname[8];
	int i, ip, nrow, hdunum, hdutype, anul;
	double pi_escal;
	XRS_GAINS *gs;
	XRS_GAIN_PARAMS *g;
	struct { int START, STOP, PIXEL, METHOD, NP, P[XRS_GAIN_POLY_MAX]; } co;

	int istat = 0;

	fits_open_file(&fp, filename, READONLY, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_open_file('%s') failed (%d)\n\
    reading as a text file ...\n", pname, filename, istat);
		gs = read_text_file(filename, status);
		return gs;
	}

	if ( 1 == fits_get_hdu_num(fp, &hdunum) ) {
		fits_movabs_hdu(fp, 2, &hdutype, &istat);
	}
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_movabs_hdu() failed (%d)\n", pname, istat);
		goto error;
	}

	fits_read_key(fp, TINT, "NAXIS2", &nrow, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key('NAXIS2') failed (%d)\n", pname, istat);
		goto error;
	}

	fits_read_key_dbl(fp, "PI_ESCAL", &pi_escal, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key('PI_ESCAL') failed (%d)\n", pname, istat);
		goto error;
	}

	if (
fits_get_colnum(fp, TRUE, k="START", &co.START, &istat) ||
fits_get_colnum(fp, TRUE, k="STOP", &co.STOP, &istat) ||
fits_get_colnum(fp, TRUE, k="PIXEL", &co.PIXEL, &istat) ||
fits_get_colnum(fp, TRUE, k="METHOD", &co.METHOD, &istat) ||
		0 ) {
		fprintf(stderr, "\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
		goto error;
	}

fits_get_colnum(fp, TRUE, "NP", &co.NP, &istat);
	if ( istat ) {
		istat = 0;		/* ignore this column */
		co.NP = -1;
	}

	for (i = 0; i < XRS_GAIN_POLY_MAX; i++) {
		sprintf(colname, "P%d", i);
fits_get_colnum(fp, TRUE, colname, &co.P[i], &istat);
		if ( istat ) {
			istat = 0;		/* ignore this column */
			co.P[i] = -1;
		}
	}

	gs = malloc( sizeof(*gs) + nrow * sizeof(gs->g[0]) + strlen(filename)+1 );
	if ( NULL == gs ) {
		fprintf(stderr, "\
%s: malloc() failed for XRS_GAINS (nrow=%d)\n", pname, nrow);
		istat = -1;
		goto error;
	}
	gs->g = (XRS_GAIN_PARAMS *)&gs[1];
	gs->filename = (char *)&gs->g[nrow];
	strcpy(gs->filename, filename);
	gs->pi_escal = pi_escal;
	gs->ng = nrow;

	for (i = 0; i < nrow; i++) {
		g = &gs->g[i];

		if (
fits_read_col_dbl(fp, co.START, i+1, 1, 1, 0.0, &g->start, &anul, &istat) ||
fits_read_col_dbl(fp, co.STOP, i+1, 1, 1, 0.0, &g->stop, &anul, &istat) ||
fits_read_col_int(fp, co.PIXEL, i+1, 1, 1, 0, &g->pixel, &anul, &istat) ||
fits_read_col_int(fp, co.METHOD, i+1, 1, 1, 0, &g->method, &anul, &istat) ||
			0 ) {
			fprintf(stderr, "\
%s: fits_read_col() failed at irow=%d (%d)\n", pname, i+1, istat);
			free(gs);
			goto error;
		}

		if ( 0 == g->method && -1 == co.NP ) {
			fprintf(stderr, "\
%s: fits_get_colnum('NP') failed (%d)\n", pname, istat);
			istat = COL_NOT_FOUND;
			free(gs);
			goto error;
		}

		if ( -1 != co.NP ) {
fits_read_col_int(fp, co.NP, i+1, 1, 1, 0, &g->np, &anul, &istat);
			if ( istat ) {
				fprintf(stderr, "\
%s: fits_read_col('NP') failed at irow=%d (%d)\n", pname, i+1, istat);
				free(gs);
				goto error;
			}
			if ( g->np < 0 || XRS_GAIN_POLY_MAX <= g->np ) {
				fprintf(stderr, "\
%s: invalid NP=%d at irow=%d (%d)\n", pname, g->np, i+1, istat);
				istat = -1;
				free(gs);
				goto error;
			}
		}

		g->p = g->pbuf;
		for (ip = 0; ip < XRS_GAIN_POLY_MAX; ip++) {
			if ( -1 == co.P[ip] ) {
				g->p[ip] = 0.0;
				continue;
			}
fits_read_col_dbl(fp, co.P[ip], i+1, 1, 1, 0, &g->p[ip], &anul, &istat);
			if ( istat ) {
				fprintf(stderr, "\
%s: fits_read_col('Pn') failed at irow=%d (%d)\n", pname, i+1, istat);
				free(gs);
				goto error;
			}
		}

	}

	fits_close_file(fp, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_close_file() failed (%d)\n", pname, istat);
		free(gs);
		goto error;
	}

	return gs;

 error:
	if ( NULL != status ) {
		*status = istat;
	}
	return NULL;
}

XRS_GAIN_PARAMS *
xrs_gain_param_get(XRS_GAINS *gs, int pixel, double t, int *status)
{
	int i, istat;
	XRS_GAIN_PARAMS *g;

	if ( NULL == gs ) {
		fprintf(stderr, "\
%s: param_get(): XRS_GAINS gs == NULL\n", pname);
		istat = -1;
		goto error;
	}

	for (i = 0; i < gs->ng; i++) {
		g = &gs->g[i];

		if ( pixel != g->pixel ) {
			continue;
		}

		if ( 0.0 == g->start && 0.0 == g->stop ) {
			return g;
		}

		if ( g->start <= t && t <= g->stop ) {
			return g;
		}
	}

	fprintf(stderr, "\
%s: param_get(): no valid entry for pixel=%d, t=%.3f\n", pname, pixel, t);
	istat = 1;
	goto error;

 error:
	if ( NULL != status ) {
		*status = istat;
	}
	return NULL;
}
