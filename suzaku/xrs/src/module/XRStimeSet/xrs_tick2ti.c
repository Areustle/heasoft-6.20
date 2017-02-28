/* $Id: xrs_tick2ti.c,v 1.4 2005/08/08 14:19:50 irby Exp $ */
/*
  xrs_tick2ti.c

    TICK [s] -> TI [1/4096 s] conversion for XRS

	2005/02/16 Y.ISHISAKI	version 1.0

	2005/04/25 Y.ISHISAKI	version 1.2
		TIME -> RECV_TIME

	2005/04/30 Y.ISHISAKI	version 1.3
		REASON -> TIME_QUALITY
		time region are much more strictly judged using TIME_QUALITY
		changed to use margin_sec to check continuity between two TIs

	2005/05/03 Y.ISHISAKI	version 1.4
		rename all RECV_TIME -> S_TIME

	2005/07/04 Y.ISHISAKI	version 1.7
		use fits_movnam_hdu() instead of fits_movabs_hdu() in read_tick_hk_file

	2005/07/26 Y.ISHISAKI	version 1.8
		use continuity of previous event for close events

	2005/07/27 Y.ISHISAKI	version 1.9
		sort by TICK2TI in compare_time()
		remove duplicated rows in read_tick_hk_file()
		check tick_bits & tick2ti in xrs_tick2ti() to use continuity
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "xrs_tick2ti.h"

static char *pname = "xrs_tick2ti";
char xrs_tick2ti_version[] = "1.9";

/* check continuity between two TIs */
static int
contTI(
	unsigned int ti1, double tz1,
	unsigned int ti2, double tz2,
	double margin_sec)
{
	int dti;
	double dtz;

	dti = ti1 - ti2;
	dtz = tz1 - tz2;

	if ( dti/4096 < dtz - margin_sec || dtz + margin_sec < dti/4096 ) {
		return -1;	/* judge as discontinuous */
	}

	return 0;		/* judge as continuous */
}

/* compare two TIs */
static int
cmpTI(
	unsigned int ti1, double tz1,
	unsigned int ti2, double tz2,
	double margin_sec)
{
	static double one_day = 24 * 60 * 60;

	int dti;
	double dtz;

	dti = ti1 - ti2;
	dtz = tz1 - tz2;

	if ( dti/4096 < dtz - margin_sec || dtz + margin_sec < dti/4096 ||
		 dtz < - one_day || one_day < dtz ) {
/* do comparison based on S_TIME */
		if ( 0.0 < dtz ) {
			return +1;
		} else if ( dtz < 0.0 ) {
			return -1;
		}
		return 0;
	}

/* do comparison based on TI */
	if ( 0 < dti ) {
		return +1;
	} else if ( dti < 0 ) {
		return -1;
	}

	return 0;
}

/* time comparison function for qsort */
static int
compare_time(struct xrs_tick_hk_struct *p, struct xrs_tick_hk_struct *q)
{
	if ( p->S_TIME < q->S_TIME ) {
		return -1;
	} else if ( p->S_TIME > q->S_TIME ) {
		return +1;
	} else if ( p->PIXEL < q->PIXEL ) {
		return -1;
	} else if ( p->PIXEL > q->PIXEL ) {
		return +1;
	} else if ( p->TICK2TI < q->TICK2TI ) {
		return -1;
	} else if ( p->TICK2TI > q->TICK2TI ) {
		return +1;
	}
	return 0;
}

static int
read_tick_hk_file(XRS_TICK2TI *xtp)
{
	static char extname[] = "XRS_CDP_TICK";

	fitsfile *fp;
	int i, j, n_dup, n_fix, hdunum, anul;
	char *key;
	struct { int S_TIME, TI, TICK, TICK2TI, PIXEL, TIME_QUALITY; } co;
	struct xrs_tick_hk_struct *p, *q;

	char found[32];
	int tick2ti[32];
	unsigned char time_quality[32];

	int istat = 0;

/* print information message */
	fflush(NULL); printf("\
%s: reading '%s[%s]' ...\n", pname, xtp->tick_hk_file, extname);
	fflush(NULL);

/* open file & move to XRS_CDP_TICK extension */
	fits_open_file(&fp, xtp->tick_hk_file, READONLY, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: tick_hk_file '%s' open failed (%d)\n", pname, xtp->tick_hk_file, istat);
		goto error;
	}
	if ( 1 == fits_get_hdu_num(fp, &hdunum) ) {
		fits_movnam_hdu(fp, BINARY_TBL, extname, 0, &istat);
	}
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_movnam_hdu('%s') failed (%d)\n", pname, extname, istat);
		goto error;
	}

/* get number of rows */
	fits_read_key(fp, TINT, "NAXIS2", &xtp->ntk, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key('NAXIS2') failed (%d)\n", pname, istat);
		goto error;
	}

/* allocate memory */
	xtp->tk = malloc(sizeof(*xtp->tk) * xtp->ntk);

	if ( NULL == xtp->tk ) {
		fprintf(stderr, "\
%s: malloc() failed for xtp->tk (ntk=%d)\n", pname, xtp->ntk);
		istat = -1;		/* malloc error */
		goto error;
	}

/* read column number */
	if (
fits_get_colnum(fp, CASESEN, key="S_TIME", &co.S_TIME, &istat) ||
fits_get_colnum(fp, CASESEN, key="TI", &co.TI, &istat) ||
fits_get_colnum(fp, CASESEN, key="TICK", &co.TICK, &istat) ||
fits_get_colnum(fp, CASESEN, key="TICK2TI", &co.TICK2TI, &istat) ||
fits_get_colnum(fp, CASESEN, key="PIXEL", &co.PIXEL, &istat) ||
fits_get_colnum(fp, CASESEN, key="TIME_QUALITY", &co.TIME_QUALITY, &istat) ||
		 0 ) {
		fprintf(stderr, "\
%s: fits_get_colnum '%s' failed (%d)\n", pname, key, istat);
		free(xtp->tk);
		goto error;
	}

/* read table contents */
	for (i = 1; i <= xtp->ntk; i++) {
		p = &xtp->tk[i-1];

fits_read_col_dbl (fp, co.S_TIME, i,1,1,0, &p->S_TIME, &anul, &istat);
fits_read_col_uint(fp, co.TI, i,1,1,0, &p->TI, &anul, &istat);
fits_read_col_uint(fp, co.TICK, i,1,1,0, &p->TICK, &anul, &istat);
fits_read_col_int (fp, co.TICK2TI, i,1,1,0, &p->TICK2TI, &anul, &istat);
fits_read_col_byt (fp, co.PIXEL, i,1,1,0, &p->PIXEL, &anul, &istat);
fits_read_col_byt (fp, co.TIME_QUALITY,i,1,1,0, &p->TIME_QUALITY,&anul,&istat);

		if ( istat ) {
			fprintf(stderr, "\
%s: fits_read_col() failed at irow=%d\n", pname, i);
			free(xtp->tk);
			goto error;
		}

		if ( p->PIXEL < 0 || 32 <= p->PIXEL ) {
			fprintf(stderr, "\
%s: invalid value of PIXEL=%d\n", pname, p->PIXEL);
			free(xtp->tk);
			goto error;
		}

	}

/* close file */
	fits_close_file(fp, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_close_file failed (%d)\n", pname, istat);
		free(xtp->tk);
		goto error;
	}

/* print information message */
	fflush(NULL); printf("\
   ntk=%d, sorting ...", xtp->ntk);
	fflush(NULL);

/* sort table */
	qsort(xtp->tk, xtp->ntk, sizeof(*xtp->tk), compare_time);
	printf(" done\n");
	fflush(NULL);

/* check duplicated rows */
	n_dup = 0;
	for (i = 0; i < xtp->ntk - 1; i++) {
		p = &xtp->tk[i];
		q = &xtp->tk[i+1];
		if ( p->S_TIME == q->S_TIME &&
			 p->TI == q->TI &&
			 p->PIXEL == q->PIXEL ) {
/* use smaller TIME_QUALITY */
			if ( q->TIME_QUALITY % 10 < p->TIME_QUALITY % 10 ) {
				*p = *q;
			}
/* this case, usually smaller TICK2TI seems to be correct */
			for (j = i + 1; j < xtp->ntk - 1; j++) {
				xtp->tk[j] = xtp->tk[j + 1];
			}
			n_dup++;
			xtp->ntk--;
			i--;
		}
	}

	if ( 0 < n_dup ) {
		fprintf(stderr, "\
%s: WARNING: %d duplicated lines are found, deleted\n", pname, n_dup);

/* check table again */
		n_fix = 0;
		memset(found, 0, sizeof(found));
		for (i = 0; i < xtp->ntk; i++) {
			p = &xtp->tk[i];
			if ( 0 == found[p->PIXEL] ) {
				found[p->PIXEL] = 1;
				time_quality[p->PIXEL] = p->TIME_QUALITY;
				tick2ti[p->PIXEL] = p->TICK2TI;
				continue;
			}
			if ( 4 <= p->TIME_QUALITY % 10 ) {
				if ( abs(p->TICK2TI - tick2ti[p->PIXEL]) <= 2 ) {
					p->TICK2TI = tick2ti[p->PIXEL];
					n_fix++;
				}
			}
		}

		if ( 0 < n_fix ) {
			fprintf(stderr, "\
%s: WARNING: TICK2TI is fixed for %d lines\n", pname, n_fix);
		}

/* print table contents */
		fflush(NULL); printf("\
S_TIME TI PIXEL TIME_QUALITY TICK2TI\n");
		for (i = 0; i < xtp->ntk; i++) {
			p = &xtp->tk[i];
			printf("\
%.3f %11d %2d %2d %d\n",
				p->S_TIME, p->TI, p->PIXEL, p->TIME_QUALITY, p->TICK2TI);
		}
		printf("\n");
		fflush(NULL);
	}

	return 0;

 error:

	xtp->ntk = 0;
	xtp->tk = NULL;
	return istat;
}


/************************************************************************
int xrs_tick2ti_init()	: initialize XRS_TICK2TI information

Input:
	char *tick_hk_file	: xrs_tick.hk FITS file name

Output:
	XRS_TICK2TI **xtpp	: pointer to XRS_TICK2TI pointer after initialization

Return_Values:
	0					: success
	-1					: malloc() error
	others				: CFITSIO error
************************************************************************/
int
xrs_tick2ti_init(XRS_TICK2TI **xtpp, char *tick_hk_file)
{
	XRS_TICK2TI *xtp;
	int i, istat, len_tick_hk_file;

	len_tick_hk_file = strlen(tick_hk_file);

	xtp = malloc(sizeof(*xtp) + (len_tick_hk_file+1));
	if ( NULL == xtp ) {
		fprintf(stderr, "\
%s: malloc() failed for XRS_TICK2TI *xtp\n", pname);
		return -1;
	}

	xtp->tick_hk_file = (char *)(xtp + 1);
	strcpy(xtp->tick_hk_file, tick_hk_file);

	istat = read_tick_hk_file(xtp);
	if ( istat ) {
		return istat;
	}

/* initialize previous conversion for each pixel */
	for (i = 0; i < 32; i++) {
		xtp->prev_s_time[i] = -1e22;
		xtp->prev_pkt_ti[i] = 0;
		xtp->prev_evt_ti[i] = 0;
		xtp->prev_tick[i] = 0;
	}

	*xtpp = xtp;
	return 0;
}


/************************************************************************
int xrs_tick2ti_free()	: free memory of XRS_TICK2TI

Input:
	XRS_TICK2TI *xtp	: XRS_TICK2TI pointer to free

Return_Values:
	0					: success
	-1					: XRS_TICK2TI *xtp is not allocated
************************************************************************/
int
xrs_tick2ti_free(XRS_TICK2TI *xtp)
{
	if ( NULL == xtp ) {
		return -1;
	}

	if ( NULL != xtp->tk ) {
		free(xtp->tk);
	}

	free(xtp);

	return 0;
}


/************************************************************************
int xrs_tick2ti()		: convert XRS-TICK [s] -> TI [1/4096 s]

Input:
	XRS_TICK2TI *xtp	: XRS_TICK2TI pointer used for the time correction
	unsigned int pkt_ti	: TI [1/4096 s] of the packet
	double s_time	    : Astro-E time [s] of the packet (S_TIME column)
	int pixel			: XRS PIXEL number (PIXEL column, 0-31)
	unsigned int tick	: XRS TICK [s] (TICK_COUNTER column), usually 0-15
	int tick_bits		: usable number of bit of tick, usually 4
	double margin_sec	: time margin [s] to judge TI continuity, usually 60

Output:
	unsigned int *ti_return	: calculated TI [1/4096 s]
	int *time_quality_return: quality of time assignment (0-8)

		0: time is determined by SET_TICK
		1: sole errTimeLost happened after time_quality=0
		2: by CDP-HK preceded by errRebooting
		3: sole errTimeLost happened after time_quality=2
		4: by CDP-HK not preceded by errRebooting
		5: sole errTimeLost happened after time_quality=4
		6: next is time_quality=4, and determined by previous CDP-HK
		7: next is time_quality=4, and determined by next CDP-HK
		8: TICK2TI is not known, so that pkt_ti itself was used

Return_Values:
	0					: success
	-1					: no valid TICK2TI information in XRS_TICK2TI *xtp
************************************************************************/
int
xrs_tick2ti(XRS_TICK2TI *xtp, unsigned int pkt_ti, double s_time,
		int pixel, unsigned int tick, int tick_bits, double margin_sec,
		unsigned int *ti_return, int *time_quality_return)
{
	int i, time_quality, tick2ti, c0, c1;
	struct xrs_tick_hk_struct *p, *q, *p_ok, *p_next;
	int tick_mask, tick_sign, tick_roll;
	int pkt_ti_sec, delta_tick, pkt_tick, evt_tick, diff_ti, diff_tick;
	unsigned int evt_ti, evt_ti_sec;
	double dt0, dt1;

	if ( NULL == xtp || xtp->tk == NULL || 0 == xtp->ntk ) {
		return -1;
	}

	p_ok = p_next = NULL;
	time_quality = 9;	/* dummy initialization for gcc warning */
	tick2ti = 0;

/* find most recent valid entry in xtp->tk */
	for (i = 0; i < xtp->ntk; i++) {
		p = &xtp->tk[i];
		if ( pixel != p->PIXEL ) {
			continue;
		}
		if ( 0 <= cmpTI(pkt_ti, s_time, p->TI, p->S_TIME, margin_sec) ) {
/* tick was determined in past */
			p_ok = p;
			p_next = NULL;
			time_quality = p->TIME_QUALITY % 10;
			tick2ti = p->TICK2TI;
		} else if ( NULL == p_ok && 4 / 2 == (p->TIME_QUALITY % 10) / 2 ) {
/* tick-jump will happen in future */
			p_ok = p;
			p_next = NULL;
			time_quality = p->TIME_QUALITY % 10;
			tick2ti = p->TICK2TI;
		} else if ( NULL == p_next ) {
			p_next = p;
		}
	}

	p = p_ok;
	q = p_next;

	if ( NULL == p_ok ) {
/* no valid entry, use pkt_ti itself */
		time_quality = 8;
		tick2ti = 0;
	} else if ( 6 == time_quality ) {
/* gray zone, exact time of tick-jump is not known */
		if ( NULL == q ) {
/* this should not happen, but keep this code for check purpose */
			fprintf(stderr, "\
%s: ERROR: next entry for time_quality=6 not found\n", pname);
			return -1;
		}
		c0 = contTI(pkt_ti, s_time, p->TI, p->S_TIME, margin_sec);
		c1 = contTI(pkt_ti, s_time, q->TI, q->S_TIME, margin_sec);
		if ( 0 == c0 && 0 != c1 ) {
			/* continuous only to p */
			time_quality = 6;
			tick2ti = p->TICK2TI;
		} else if ( 0 != c0 && 0 == c1 ) {
			/* continuous only to q */
			time_quality = 7;
			tick2ti = q->TICK2TI;
		} else if ( 0 == c0 && 0 == c1 ) {
			/* continuous to both side */
			dt0 = s_time - p->S_TIME;
			dt1 = q->S_TIME - s_time;
			if ( dt0 <= dt1 ) {
				/* closer to p */
				time_quality = 6;
				tick2ti = p->TICK2TI;
			} else {
				/* closer to q */
				time_quality = 7;
				tick2ti = q->TICK2TI;
			}
		} else {
			/* discontinuous to both side */
			time_quality = 8;
			tick2ti = 0;
		}
	}

	tick_sign = 1 << (tick_bits - 1);
	tick_mask = (tick_sign << 1) - 1;
	tick_roll = tick_mask + 1;

	pkt_ti_sec = pkt_ti / 4096;

/* use continuity of previous event for close events */
	diff_ti = pkt_ti - xtp->prev_pkt_ti[pixel];
	diff_tick = (tick - xtp->prev_tick[pixel]) & 15;
	if ( 4 == tick_bits &&
		 fabs(s_time - xtp->prev_s_time[pixel] - diff_tick) < 4.0 &&
		 abs(diff_ti - 4096*diff_tick) < 4 * 4096 ) {
		evt_ti = xtp->prev_evt_ti[pixel] + 4096 * diff_tick;
		evt_ti_sec = evt_ti / 4096;
		delta_tick = evt_ti_sec - pkt_ti_sec;
		if ( -20 <= delta_tick && delta_tick <= 2 ) {
			if ( tick2ti == xtp->prev_tick2ti[pixel] ) {
				goto skip;
			} else if ( 0 != ((tick2ti - xtp->prev_tick2ti[pixel]) & 15) ) {
				evt_tick = evt_ti_sec + xtp->prev_tick2ti[pixel];
				if ( tick == (evt_tick & tick_mask) ) {
					tick2ti = xtp->prev_tick2ti[pixel];
					goto skip;
				}
			}
		}
	}

	pkt_tick = pkt_ti_sec - tick2ti;
	delta_tick = tick - pkt_tick;
	delta_tick &= tick_mask;

	if ( 0 < delta_tick ) {
		delta_tick -= tick_roll;
	}

	if ( tick != ((pkt_tick + delta_tick) & tick_mask) ) {
/* this should not happen, but keep this code for check purpose */
		fprintf(stderr, "\
%s: ERROR: tick != (pkt_tick + delta_tick) & tick_mask\n\
    at PIXEL=%d, pkt_tick+delta_tick=%xh, tick=%xh, bits=%d, mask=%x\n",
		   pname, pixel, pkt_tick+delta_tick, tick, tick_bits, tick_mask);
	}

	evt_ti = ( (pkt_ti_sec + delta_tick) * 4096 ) & 0xffffffff;
	diff_ti = pkt_ti - evt_ti;
	if ( diff_ti <= -4096 ) {
		delta_tick -= tick_roll;
		evt_ti -= tick_roll * 4096;
	}

 skip:

/*	if ( 3 == pixel ) {
		fflush(NULL); printf("\
s_t=%.1f, pixel=%d, pkt_ti=%u, evt_ti=%u, tick=%d, dlt_tick=%d, tick2ti=%d\n", s_time, pixel, pkt_ti, evt_ti, tick, delta_tick, tick2ti);
		fflush(NULL);
	}*/

	if ( 4 == tick_bits ) {
		xtp->prev_s_time[pixel] = s_time;
		xtp->prev_pkt_ti[pixel] = pkt_ti;
		xtp->prev_evt_ti[pixel] = evt_ti;
		xtp->prev_tick[pixel] = tick;
		xtp->prev_tick2ti[pixel] = tick2ti;
	}

	*ti_return = evt_ti;
	*time_quality_return = time_quality;

	return 0;
}
