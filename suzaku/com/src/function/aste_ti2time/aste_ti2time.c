/*

  aste_ti2time: TI -> Astro-E time conversion

	2005-02-07	Y.ISHISAKI	version 1.0

	2005-02-15	Y.ISHISAKI	version 1.31
		add simplified version without initialization
		use printf() instead of fprintf(stderr, ..) for non-Error messages
		return -1 on malloc() error in read_time_pkt_file/dp_timc_file()
		bug fix in aste_ti2time_dbl(), formerly always assumed DHU-TI
		add functions of aste_ti2time_dp_dbl(), aste_ti2time_dhu_dbl()

	2005-04-13,25	Y.ISHISAKI	version 1.40
		check DP_TIMC discontinuity
		add tpk->flag_discon, dpk->add_no, base_jump, base_discon in TI2TIME
		aste_ti2time_dhu() revised to give continuous conversion for DHU-TI
		changes on debug information for DHU-TI

	2005-05-03	Y.ISHISAKI	version 2.5
		move to AEpacketTimeSet/2.5, remove from astetool
		rename all RECV_TIME -> S_TIME

	2005-05-04	Y.ISHISAKI	version 2.6
		check TI continuity in aste_ti2time_dp()
		allow ttp->tpk[].t0 == t1 in read_time_pkt_file(), aste_ti2time_init()

	2005-05-15	Y.ISHISAKI	version 2.7
		aste_ti2time.[ch] moved from com/module/AEpacketTimeSet/VERSION/
		change aste_ti2time_init() arguments

	2005-06-21	Y.ISHISAKI	version 2.8
		set ttp->tim_file = "none" for simplified version
		add aste_ti2time_version[]

	2005-07-10,12	Y.ISHISAKI	version 3.0
		add rough_ti2time_dp(), rough_ti2time_dhu() for ttp->rough
		initialize ttp->rough = 0 in aste_ti2time_init()

	2005-07-26	Y.ISHISAKI	version 3.1
		refer cache in get_timc_val(), about 5 times faster than before

	2005-08-04	Y.ISHISAKI	version 3.2
		choose aetime closer to tz for extrapolation in rough_ti2time_dhu()

	2005-08-05	Y.ISHISAKI	version 3.3
		char flag_tc -> signed char flag_tc, for Linux PPC
		remove double tc, char flag_tc in struct dp_timc_struct {}, not used

	2005-08-14	Y.ISHISAKI	version 3.4
		use flag_cont for DHU-TI continuity check in rough_ti2time_dhu()
		use H0 & H1 information in rough_ti2time_dhu(), aste_ti2time_dhu()
		add function check_H_between()

	2005-10-15	Y.ISHISAKI	version 3.5
		threshold for too large correction 10 -> 100 ms in aste_ti2time_dp()

	2005-10-21	Y.ISHISAKI	version 3.6
		initialize ttp->avg = NULL in aste_ti2time_init()
		add free(ttp->avg) in aste_ti2time_free()

	2005-11-30	Y.ISHISAKI	version 3.7
		check 0 < npos in accessing ttp->dpk[npos-1] in get_timc_val()
		change message "freq=" -> "f=" in aste_ti2time_init()

	2006-01-31	Y.ISHISAKI	version 3.8
		consider Y0->Y1 drift in checking for too large correction
		warning only for extrapolation error or too large correction
		use fabs(us) in checking large correction in aste_ti2time_dp()

	2006-02-23	Y.ISHISAKI	version 3.9
		allow 1 week gap (formerly 3 days) in aste/rough_ti2time_dp()
		check whether N0 < 1 week, when flag_jump in aste/rough_ti2time_dp()
		strict comparison of dt & dtmin in aste_ti2time_dp()
		remove dtmin in rough_ti2time_dp()

	2007-05-12	Y.ISHISAKI	version 4.0
		add counter_between() function
		use counter_between() in get_timc_val() for counter wrap around

	2007-07-16	Y.ISHISAKI	version 4.1
		check 32bit integer wrap around between Y0 -> Y1 in get_timc_val()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "aste_ti2time.h"

static char pname[] = "aste_ti2time";
char aste_ti2time_version[] = "4.1";

static int
simple_ti2time_dp(TI2TIME *ttp, unsigned int N, double tz, double *aetime)
{
	int dN;
	double dt;

	if ( 0 == ttp->dp.init_ref ) {
		ttp->dp.init_ref = 1;
		goto skip;
	}

	dN = N - ttp->dp.ref_ti;
	dt = tz - ttp->dp.ref_time;
	if ( fabs(dt - dN/4096.0) < 30.0 ) {	/* judge as no TI jump */
		ttp->dp.ref_ti = N;
		ttp->dp.ref_time += dN / 4096.0;
		*aetime = ttp->dp.ref_time;
		return 0;
	}
	fflush(stdout); fflush(stderr); printf("\
%s (simplified version): found DP-TI jump\n\
    old: t=%.1f, TI=%08x\n\
    new: t=%.1f, TI=%08x\n",
		pname, ttp->dp.ref_time, ttp->dp.ref_ti, tz, N);
	fflush(stdout); fflush(stderr);

 skip:
	ttp->dp.ref_ti = N;
	ttp->dp.ref_time = tz;
	*aetime = tz;

	return 0;
}

static int
simple_ti2time_dhu(TI2TIME *ttp, unsigned int N, double tz, double *aetime)
{
	int dN;
	double dt;

	if ( 0 == ttp->dhu.init_ref ) {
		ttp->dhu.init_ref = 1;
		goto skip;
	}

	dN = N - ttp->dhu.ref_ti;
	dt = tz - ttp->dhu.ref_time;
	if ( fabs(dt - dN/32.0) < 30.0 ) {		/* judge as no TI jump */
		ttp->dhu.ref_ti = N;
		ttp->dhu.ref_time += dN / 32.0;
		*aetime = ttp->dhu.ref_time;
		return 0;
	}
	fflush(stdout); fflush(stderr); printf("\
%s (simplified version): found DHU-TI jump\n\
    old: t=%.1f, TI=%08x\n\
    new: t=%.1f, TI=%08x\n",
		pname, ttp->dhu.ref_time, ttp->dhu.ref_ti, tz, N);
	fflush(stdout); fflush(stderr);

 skip:
	ttp->dhu.ref_ti = N;
	ttp->dhu.ref_time = tz;
	*aetime = tz;

	return 0;
}

static int
read_time_pkt_file(TI2TIME *ttp)
{
	static char extname[] = "TIME_PACKETS_SEL";
	static int extver = 0;
	static int hdutype = BINARY_TBL;

	fitsfile *fp;
	long irow;
	int i, naxis2, anul;
	char *key;
	struct { int t, N; } co;
	double t0, t1, freq;
	unsigned int N0, N1;
	struct time_pkt_struct *p;

	int istat = 0;

/* print information message */
	fflush(stdout); fflush(stderr); printf("\
%s: reading '%s[%s]' ...\n", pname, ttp->tim_file, extname);
	fflush(stdout); fflush(stderr);

/* open file */
	fits_open_file(&fp, ttp->tim_file, READONLY, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: tim_file '%s' open failed (%d)\n", pname, ttp->tim_file, istat);
		goto error;
	}
	fits_movnam_hdu(fp, hdutype, extname, extver, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_movnam_hdu('%s') failed (%d)\n", pname, extname, istat);
		goto error;
	}
	fits_read_key(fp, TINT, "NAXIS2", &naxis2, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key('NAXIS2') failed (%d)\n", pname, istat);
		goto error;
	} else if ( 0 == naxis2 ) {
		fprintf(stderr, "\
%s: no rows in tim_file '%s'\n", pname, ttp->tim_file);
		istat = -1;		/* no rows */
		goto error;
	}

/* allocate memory */
	ttp->ntpk = naxis2;
	ttp->tpk = malloc(sizeof(*ttp->tpk) * naxis2);

	if ( NULL == ttp->tpk ) {
		fprintf(stderr, "\
%s: malloc() failed for ttp->tpk (naxis2=%d)\n", pname, naxis2);
		istat = -1;		/* malloc error */
		goto error;
	}

/* get column numbers */
	if (
fits_get_colnum(fp, CASESEN, key="SAT_TIME", &co.t, &istat) ||
fits_get_colnum(fp, CASESEN, key="TI", &co.N, &istat) ||
		 0 ) {
		fprintf(stderr, "\
%s: fits_get_colnum('%s') failed (%d)\n", pname, key, istat);
		free(ttp->tpk);
		goto error;
	}

/* read table contents */
	ttp->tpk[0].flag_jump = 1;
	for (i = 0, irow = 1; irow < naxis2; irow++) {
		p = &ttp->tpk[i];

		if (
fits_read_col_dbl (fp, co.t, irow,  1, 1, 0.0, &t0, &anul, &istat) ||
fits_read_col_dbl (fp, co.t, irow+1,1, 1, 0.0, &t1, &anul, &istat) ||
fits_read_col_uint(fp, co.N, irow,  1, 1, 0,   &N0, &anul, &istat) ||
fits_read_col_uint(fp, co.N, irow+1,1, 1, 0,   &N1, &anul, &istat) ||
			0 ) {
			fprintf(stderr, "\
%s: fits_read_col() failed at irow=%ld\n", pname, irow);
			free(ttp->tpk);
			goto error;
		}

		if ( t0 < t1 ) {
			freq = (N1 - N0) / (t1 - t0);
			if ( 4095.5 < freq && freq < 4096.5 ) {
			/* this time region is continuous */
				p->t0 = t0;
				p->t1 = t1;
				p->N0 = N0;
				p->N1 = N1;
				i++;
				ttp->tpk[i].flag_jump = 0;
			} else if ( p->flag_jump ) {
			/* this region is not continuous, and also previous */
				p->t0 = t0;
				p->t1 = t0;
				p->N0 = N0;
				p->N1 = N0;
				i++;
				ttp->tpk[i].flag_jump = 1;
			} else {
			/* this region is not continuous, but previous is continuous */
				p->flag_jump = 1;
			}
		} else {
			/* time reversion, just ignore it */
			p->flag_jump = 1;
		}

	}

	p = &ttp->tpk[i];

	if (
fits_read_col_dbl (fp, co.t, irow,  1, 1, 0.0, &t0, &anul, &istat) ||
fits_read_col_uint(fp, co.N, irow,  1, 1, 0,   &N0, &anul, &istat) ||
		0 ) {
		fprintf(stderr, "\
%s: fits_read_col() failed at irow=%ld\n", pname, irow);
		free(ttp->tpk);
		goto error;
	} else if ( p->flag_jump ) {
	/* this region is not continuous, and also previous */
		p->t0 = t0;
		p->t1 = t0;
		p->N0 = N0;
		p->N1 = N0;
		i++;
	}

	ttp->ntpk = i;

/* print information message */
	fflush(stdout); fflush(stderr); printf("\
   ntpk=%d\n", ttp->ntpk);
	fflush(stdout); fflush(stderr);

/* close file */
	fits_close_file(fp, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_close_file() failed (%d)\n", pname, istat);
		free(ttp->tpk);
		goto error;
	}

	return 0;

 error:

	ttp->ntpk = 0;
	ttp->tpk = NULL;
	return istat;
}

static int
read_dp_timc_file(TI2TIME *ttp)
{
	static char extname1[] = "DP_TIMC";
	static char extname2[] = "DP_DHU_AVG";
	static int extver = 0;
	static int hdutype = BINARY_TBL;

	fitsfile *fp;
	long irow;
	int k, nrow, anul;
	char *key;
	struct { int tz, N, Y, add_no; } c1;
	struct { int tz, N, H, tz0, tz1, N0, N1, H0, H1, flag_cont; } c2;
	int istat = 0;

/* print information message */
	fflush(stdout); fflush(stderr); printf("\
%s: reading '%s[%s]' ...\n", pname, ttp->tim_file, extname1);
	fflush(stdout); fflush(stderr);

/* open file */
	fits_open_file(&fp, ttp->tim_file, READONLY, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: tim_file '%s' open failed (%d)\n", pname, ttp->tim_file, istat);
		goto error;
	}
	fits_movnam_hdu(fp, hdutype, extname1, extver, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_movnam_hdu('%s') failed (%d)\n", pname, extname1, istat);
		goto error;
	}
	fits_read_key(fp, TINT, "NAXIS2", &nrow, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key('NAXIS2') failed (%d)\n", pname, istat);
		goto error;
	}

/* allocate memory */
	ttp->ndpk = nrow;
	ttp->dpk = malloc(sizeof(*ttp->dpk) * nrow);

	if ( NULL == ttp->dpk ) {
		fprintf(stderr, "\
%s: malloc() failed for ttp->dpk (naxis2=%d)\n", pname, nrow);
		istat = -1;		/* malloc error */
		goto error;
	}

/* get column numbers */
	if (
fits_get_colnum(fp, CASESEN, key="S_TIME", &c1.tz, &istat) ||
fits_get_colnum(fp, CASESEN, key="TI", &c1.N, &istat) ||
fits_get_colnum(fp, CASESEN, key="DP_TIMC_DAT", &c1.Y, &istat) ||
fits_get_colnum(fp, CASESEN, key="DP_TIMC_ADD_NO", &c1.add_no, &istat) ||
		 0 ) {
		fprintf(stderr, "\
%s: fits_get_colnum('%s') failed (%d)\n", pname, key, istat);
		free(ttp->dpk);
		goto error;
	}

/* read table contents */
	for (irow = 1; irow <= nrow; irow++) {
		struct dp_timc_struct *p = &ttp->dpk[irow-1];
		if (
fits_read_col_dbl (fp, c1.tz, irow, 1, 1, 0.0, &p->tz, &anul, &istat) ||
fits_read_col_uint(fp, c1.N,  irow, 1, 1, 0,   &p->N,  &anul, &istat) ||
fits_read_col_int (fp, c1.Y,  irow, 1, 1, 0,   &p->Y,  &anul, &istat) ||
fits_read_col_usht(fp, c1.add_no, irow, 1, 1, 0, &p->add_no, &anul, &istat) ||
			0 ) {
			fprintf(stderr, "\
%s: fits_read_col() failed at irow=%ld\n", pname, irow);
			free(ttp->dpk);
			goto error;
		}
	}

/* print information message */
	fflush(stdout); fflush(stderr); printf("\
   ndpk=%d, t=%.3f - %.3f\n", ttp->ndpk, ttp->dpk[0].tz, ttp->dpk[nrow-1].tz);
	fflush(stdout); fflush(stderr);

/* print information message */
	fflush(stdout); fflush(stderr); printf("\
%s: reading '%s[%s]' ...\n", pname, ttp->tim_file, extname2);
	fflush(stdout); fflush(stderr);
/* move to next extension */
	fits_movnam_hdu(fp, hdutype, extname2, extver, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_movnam_hdu('%s') failed (%d)\n", pname, extname2, istat);
		goto error;
	}
	fits_read_key(fp, TINT, "NAXIS2", &nrow, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key('NAXIS2') failed (%d)\n", pname, istat);
		goto error;
	}

/* allocate memory */
	ttp->navg = nrow;
	ttp->avg = malloc(sizeof(*ttp->avg) * nrow);

	if ( NULL == ttp->dpk ) {
		fprintf(stderr, "\
%s: malloc() failed for ttp->avg (naxis2=%d)\n", pname, nrow);
		free(ttp->dpk);
		istat = -1;		/* malloc error */
		goto error;
	}

/* get column numbers */
	if (
fits_get_colnum(fp, CASESEN, key="AVG_S_TIME", &c2.tz, &istat) ||
fits_get_colnum(fp, CASESEN, key="S_TIME_0", &c2.tz0, &istat) ||
fits_get_colnum(fp, CASESEN, key="S_TIME_1", &c2.tz1, &istat) ||
fits_get_colnum(fp, CASESEN, key="AVG_N", &c2.N, &istat) ||
fits_get_colnum(fp, CASESEN, key="N0", &c2.N0, &istat) ||
fits_get_colnum(fp, CASESEN, key="N1", &c2.N1, &istat) ||
fits_get_colnum(fp, CASESEN, key="AVG_H", &c2.H, &istat) ||
fits_get_colnum(fp, CASESEN, key="H0", &c2.H0, &istat) ||
fits_get_colnum(fp, CASESEN, key="H1", &c2.H1, &istat) ||
fits_get_colnum(fp, CASESEN, key="FLAG_CONT", &c2.flag_cont, &istat) ||
		 0 ) {
		fprintf(stderr, "\
%s: fits_get_colnum('%s') failed (%d)\n", pname, key, istat);
		free(ttp->avg);
		free(ttp->dpk);
		goto error;
	}

/* read table contents */
	for (irow = 1; irow <= nrow; irow++) {
		struct dp_dhu_avg_struct *p = &ttp->avg[irow-1];
		p->flag_tc = 0;		/* p->tc unset */
		if (
fits_read_col_dbl (fp,k=c2.tz,  irow, 1, 1, 0.0, &p->tz,  &anul, &istat) ||
fits_read_col_dbl (fp,k=c2.tz0, irow, 1, 1, 0.0, &p->tz0, &anul, &istat) ||
fits_read_col_dbl (fp,k=c2.tz1, irow, 1, 1, 0.0, &p->tz1, &anul, &istat) ||
fits_read_col_dbl (fp,k=c2.N,   irow, 1, 1, 0.0, &p->N,   &anul, &istat) ||
fits_read_col_uint(fp,k=c2.N0,  irow, 1, 1, 0,   &p->N0,  &anul, &istat) ||
fits_read_col_uint(fp,k=c2.N1,  irow, 1, 1, 0,   &p->N1,  &anul, &istat) ||
fits_read_col_dbl (fp,k=c2.H,   irow, 1, 1, 0.0, &p->H,   &anul, &istat) ||
fits_read_col_int (fp,k=c2.H0,  irow, 1, 1, 0,   &p->H0,  &anul, &istat) ||
fits_read_col_int (fp,k=c2.H1,  irow, 1, 1, 0,   &p->H1,  &anul, &istat) ||
fits_read_col_log (fp,k=c2.flag_cont,irow,1,1,0, &p->flag_cont,&anul,&istat) ||
			0 ) {
			fprintf(stderr, "\
%s: fits_read_col() failed at irow=%ld\n", pname, irow);
			free(ttp->avg);
			free(ttp->dpk);
			goto error;
		}
	}

/* close file */
	fits_close_file(fp, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_close_file() failed (%d)\n", pname, istat);
		free(ttp->avg);
		free(ttp->dpk);
		goto error;
	}

	return 0;

 error:

	ttp->ndpk = 0;
	ttp->dpk = NULL;
	return istat;
}

static double
calc_dY(double Y1, double Y0)
{
	double dY;

	dY = Y1 - Y0;
	if ( dY < -2147483648.0 ) {			/* -2^31 */
		dY += 4294967296.0;				/* +2^32 */
	} else if ( 2147483648.0 <= dY ) {	/* +2^31 */
		dY -= 4294967296.0;				/* -2^32 */
	}

	return dY;
}

static int
counter_between(unsigned int N, unsigned int N0, unsigned int N1)
{
	unsigned int dN0, dN1;

	N = N - N0;
	N1 = N1 - N0;

	if ( N <= N1 ) {
		return 0;		/* N0 <= N <= N1 */
	}
	dN0 = N0 - N;
	dN1 = N - N1;
	if ( dN0 <= dN1 ) {
		return -1;		/* N < N0 */
	}
	return 1;			/* N1 < N */
}

static int
get_timc_val(TI2TIME *ttp, unsigned int N, double tz, double *Y, int *ipos_ptr)
{
	int i, i0, i_min, ipos, npos;
	double prev_tz, dt, dt_min;
	int nmatch, match_flag, matchpos[32];
	unsigned int prev_N, N0, N1;
	double Y0, Y1;

/* check for previous cache value */
	i0 = 0;
	if ( 0 <= ttp->prev_ipos ) {
		prev_N = ttp->prev_N;
		prev_tz = ttp->prev_tz;
		if ( N == prev_N ) {
			if ( fabs(tz - prev_tz) < 16.0 ) {
				if ( NULL != ipos_ptr ) {
					*ipos_ptr = ttp->prev_ipos;
				}
				*Y = ttp->prev_Y;
				return 0;
			}
		} else if ( 0 == counter_between(N, prev_N, prev_N + 24*60*60*4096) &&
				    prev_tz <= tz && tz <= prev_tz + 24*60*60 ) {
			/* allow 1 day gap, here */
			i0 = ttp->prev_ipos;
		} else if ( 0 == counter_between(N, prev_N - 60*4096, prev_N) &&
				    prev_tz - 60.0 <= tz ) {
			/* allow 1 min back, search down to 5 min, here */
			while ( 0 < i0 ) {
				i0--;
				if ( ttp->dpk[i0].tz < tz - 300.0 ) {
					break;
				}
			}
		}
	}

	nmatch = 0;

	for (i = i0; i < ttp->ndpk - 1; i++) {
		match_flag = 0;
		if ( tz + 300.0 < ttp->dpk[i].tz ) {	/* search up to 5 min */
			break;		/* assume DP packet is in time order */
		}
		N0 = ttp->dpk[i].N;
		N1 = ttp->dpk[i+1].N;
		if ( N0 <= N && N <= N1 ) {
			match_flag = 1;
		} else if ( N1 < N0 && ( N0 <= N || N <= N1 ) ) {
			match_flag = 1;
		}
		if ( match_flag ) {
			matchpos[nmatch] = i;
			if ( nmatch < sizeof(matchpos)/sizeof(*matchpos) ) {
				nmatch++;
			} else {
				fprintf(stderr, "\
%s: too many match positions (%d)\n", pname, nmatch);
				goto error;
			}
		}
	}
	npos = i;

	if ( 0 == nmatch ) {
		unsigned int deltaN0, deltaN1;
		double dt0, dt1;

		fprintf(stderr, "\
%s: can't interpolate TIMC for TI=%u, t=%.0f,\n\
    trying extrapolation.\n", pname, N, tz);

		deltaN0 = (ttp->dpk[0].N - N) & 0xffffffff;
		if ( 0 < npos ) {
			deltaN1 = (N - ttp->dpk[npos-1].N) & 0xffffffff;
		}
		if ( npos <= 0 || deltaN0 <= deltaN1 ) {
			ipos = 0;
			dt0 = ttp->dpk[0].tz - tz;
			if ( 300*4096 < deltaN0 || dt0 < -300.0 || 300.0 < dt0 ) {
				fprintf(stderr, "\
%s: too far extrapolation of deltaN=%u, dt=%.0f\n", pname, deltaN0, dt0);
				goto error;
			}
		} else {
			ipos = npos - 1;
			dt1 = tz - ttp->dpk[npos].tz;
			if ( 300*4096 < deltaN1 || dt1 < -300.0 || 300.0 < dt1 ) {
				fprintf(stderr, "\
%s: too far extrapolation of deltaN=%u, dt=%.0f\n", pname, deltaN1, dt1);
				goto error;
			}
		}
	} else if ( 1 < nmatch ) {
		i_min = matchpos[0];
		dt_min = fabs(tz - ttp->dpk[matchpos[0]].tz);
		for (i = 1; i < nmatch; i++) {
			dt = fabs(tz - ttp->dpk[matchpos[i]].tz);
			if ( dt < dt_min ) {
				i_min = matchpos[i];
				dt_min = dt;
			}
		}
		ipos = i_min;
	} else {
		ipos = matchpos[0];
	}

	N0 = ttp->dpk[ipos].N;
	N1 = ttp->dpk[ipos+1].N;
	Y0 = ttp->dpk[ipos].Y;
	Y1 = ttp->dpk[ipos+1].Y;
	if ( N1 < N0 ) {
		if ( N0 <= N ) {
			*Y = Y0;
		} else {
			*Y = Y1;
		}
	} else if ( N0 == N1 ) {
		*Y = (Y0 + Y1) / 2;
	} else {
		if ( (Y0 * Y1) < 0 && 2147483648.0 < fabs(Y0 - Y1) ) {
			/* 32bit integer wrap around between Y0 -> Y1 */
			*Y = Y0 + calc_dY(Y1, Y0) * (N - N0) / (N1 - N0);
		} else {
			*Y = (Y1*(N-N0) + Y0*(N1-N)) / (N1 - N0);
		}
	}

	if ( NULL != ipos_ptr ) {
		*ipos_ptr = ipos;
	}

	ttp->prev_ipos = ipos;
	ttp->prev_N = N;
	ttp->prev_tz = tz;
	ttp->prev_Y = *Y;

	return 0;

 error:

	return -1;
}

static int
check_timc_discontinuity(TI2TIME *ttp, int ipos0, int ipos1)
{
	int ipos, Y, dY;
	unsigned short da, add_no;

	if ( ipos0 < 0 ) {
		ipos0 = 0;
	} else if ( ttp->ndpk <= ipos0 ) {
		ipos0 = ttp->ndpk - 1;
	}

	ipos1 = ipos1 + 1;	/* interpolate b/w (ipos,ipos+1) in get_timc_val() */
	if ( ipos1 < 0 ) {
		ipos1 = 0;
	} else if ( ttp->ndpk <= ipos1 ) {
		ipos1 = ttp->ndpk - 1;
	}

	Y = ttp->dpk[ipos0].Y;
	add_no = ttp->dpk[ipos0].add_no;
	for (ipos = ipos0 + 1; ipos <= ipos1; ipos++) {
		dY = ttp->dpk[ipos].Y - Y;
		da = ttp->dpk[ipos].add_no - add_no;
		if ( 300 < da ||						/* allow 300-sec gap */
			 1e-6*4096*4096*100*da < fabs(dY)	/* allow 1us/1s correction */
			) {
			struct dp_timc_struct *p;
			ipos--;
			p = &ttp->dpk[ipos];
			fprintf(stderr, "\
%s: found DP_TIMC (= Y) discontinuity at:\n", pname);
			fprintf(stderr, "\
   i=%d: t=%.3f TI=%u Y=%d a=%d\n", ipos, p->tz, p->N, p->Y, p->add_no);
			ipos++;
			p = &ttp->dpk[ipos];
			fprintf(stderr, "\
   i=%d: t=%.3f TI=%u Y=%d a=%d\n", ipos, p->tz, p->N, p->Y, p->add_no);
			return -1;	/* found discontinuity */
		}
		Y = ttp->dpk[ipos].Y;
		add_no = ttp->dpk[ipos].add_no;
	}

	return 0;	/* no discontinuity */
}


/************************************************************************
int aste_ti2time_init()	: initialize TI2TIME information

	When tim_file == NULL or "none", use simplified version of aste_ti2time()

Input:
	char *tim_file		: .tim FITS file name

Output:
	TI2TIME **ttpp		: pointer to TI2TIME pointer after initialization

Return_Values:
	0					: success
	-1					: malloc() error
	-2					: extrapolation error in get_timc_val()
	-3					: too large base freq drift, something is wrong
	others				: CFITSIO error
************************************************************************/
int
aste_ti2time_init(TI2TIME **ttpp, char *tim_file)
{
	TI2TIME *ttp;
	int i, istat, ipos0, ipos1, len_tim_file;

	if ( NULL == tim_file || 0 == strcmp("none", tim_file) ) {
/* use simplified version */
		ttp = malloc(sizeof(*ttp));
		if ( NULL == ttp ) {
			fprintf(stderr, "\
%s: malloc() failed for TI2TIME *ttp\n", pname);
			return -1;
		}
		ttp->simple = 1;
		ttp->tim_file = "none";
		ttp->dp.init_ref = 0;
		ttp->dhu.init_ref = 0;
		ttp->tpk = NULL;
		ttp->dpk = NULL;
		ttp->avg = NULL;
		*ttpp = ttp;
		return 0;
	}

	len_tim_file = strlen(tim_file);

	ttp = malloc( sizeof(*ttp) + (len_tim_file+1) );
	if ( NULL == ttp ) {
		fprintf(stderr, "\
%s: malloc() failed for TI2TIME *ttp\n", pname);
		return -1;
	}

/* use full version */
	ttp->simple = 0;
	ttp->rough = 0;
	ttp->tim_file = (char *)(ttp + 1);
	strcpy(ttp->tim_file, tim_file);
	ttp->tpk = NULL;
	ttp->dpk = NULL;
	ttp->avg = NULL;
	ttp->prev_ipos = -1;
	ttp->prev_N = 0;
	ttp->prev_tz = 0.0;
	ttp->prev_Y = 0.0;

	istat = read_time_pkt_file(ttp);
	if ( istat ) {
		aste_ti2time_free(ttp);
		return istat;
	}

	istat = read_dp_timc_file(ttp);
	if ( istat ) {
		aste_ti2time_free(ttp);
		return istat;
	}

	for (i = 0; i < ttp->ntpk; i++) {
		double freq, Y0, Y1, dY;
		struct time_pkt_struct *p = &ttp->tpk[i];
		double t0 = p->t0;
		double t1 = p->t1;
		unsigned int N0 = p->N0;
		unsigned int N1 = p->N1;
		unsigned int deltaN = (N1 - N0) & 0xffffffff;

		if ( get_timc_val(ttp, N0, t0, &Y0, &ipos0) ) {
			fprintf(stderr, "\
%s: get_timc_val(N0=%u, t0=%.0f) failed\n", pname, N0, t0);
			aste_ti2time_free(ttp);
			return -2;
		}
		if ( t0 == t1 ) {
			p->Y0 = Y0;
			p->Y1 = Y0;
			p->freq = 4096.0 * 4096.0;
			p->flag_discon = 1;
			fflush(stdout); fflush(stderr); printf("\
%d: t0=t1=%.0f,N0=N1=%u,Y=%.0f,f=%.3f,j=%d,d=%d\n",
			i+1, t0, N0, Y0, p->freq, p->flag_jump, p->flag_discon);
			fflush(stdout); fflush(stderr);
			continue;
		}
		if ( get_timc_val(ttp, N1, t1, &Y1, &ipos1) ) {
			fprintf(stderr, "\
%s: get_timc_val(N1=%u, t1=%.0f) failed\n", pname, N1, t1);
			aste_ti2time_free(ttp);
			return -2;
		}
		p->flag_discon = check_timc_discontinuity(ttp, ipos0, ipos1);
		if ( p->flag_discon ) {
			fprintf(stderr, "\
   ignore Y between t:%.1f - %.1f, TI:%u - %u\n", t0, t1, N0, N1);
			dY = 0;
		} else {
			dY = calc_dY(Y1, Y0);
		}
		freq = (4096.0 * deltaN - dY/100) / (t1 - t0);
		fflush(stdout); fflush(stderr); printf("\
%d: t0=%.0f,N0=%u,Y=%.0f/%.0f,f=%.3f,j=%d,d=%d\n",
			i+1, t0, N0, Y0, Y1, freq, p->flag_jump, p->flag_discon);
		fflush(stdout); fflush(stderr);
		if ( freq < 16777216*(1.0 - 1e-3) || (1.0 + 1e-3)*16777216 < freq ) {
			fprintf(stderr, "\
%s: base freq drift (%f Hz) from 16,777,216 Hz is too large\n", pname, freq);
			aste_ti2time_free(ttp);
			return -3;
		}
		p->Y0 = Y0;
		p->Y1 = Y1;
		p->freq = freq;
	}

	*ttpp = ttp;
	return 0;
}


/************************************************************************
int aste_ti2time_free()	: free memory of TI2TIME

Input:
	TI2TIME *ttp		: TI2TIME pointer to free

Return_Values:
	0					: success
************************************************************************/
int
aste_ti2time_free(TI2TIME *ttp)
{
	if ( NULL == ttp ) {
		return -1;
	}

	if ( NULL != ttp->tpk ) {
		free(ttp->tpk);
	}

	if ( NULL != ttp->dpk ) {
		free(ttp->dpk);
	}

	if ( NULL != ttp->avg ) {
		free(ttp->avg);
	}

	free(ttp);

	return 0;
}


static int
rough_ti2time_dp(TI2TIME *ttp, unsigned int N, double tz, double *aetime)
{
	int i, imin, dN;
	unsigned int NN, N0, N1, NL, NH, dNunsig, dNmin, margin;
	double t, dt, f, Y, dY;

	if ( 0 == ttp->ntpk ) {
		return -1;
	}

	imin = -1;
	margin = 3 * 24 * 60 * 60 * 4096;	/* 3 days */
	dNunsig = dNmin = 0;		/* dummy initialization for gcc warning */

	for (i = 0; i < ttp->ntpk; i++) {

		NN = N;
		N0 = ttp->tpk[i].N0;
		NL = (N0 - margin) & 0xffffffff;
		N1 = ttp->tpk[i].N1;
		NH = (N1 + margin) & 0xffffffff;

		if ( ttp->tpk[i].flag_jump && N < N0 && N0/4096 < 7*24*60*60 ) {
			/* probably, TI was reset within 1 week */
			dNunsig = 0;
			goto skip;
		}

		NN = (NN - NL) & 0xffffffff;
		N0 = (N0 - NL) & 0xffffffff;
		N1 = (N1 - NL) & 0xffffffff;
		NH = (NH - NL) & 0xffffffff;
		NL = 0;

		if ( NL < NN && NN < NH ) {

			if ( NN < N0 ) {
				dNunsig = N0 - NN;
			} else if ( NN <= N1 ) {
				dNunsig = 0;
			} else {
				dNunsig = NN - N1;
			}

		skip:

			if ( tz < ttp->tpk[i].t0 ) {
				dt = ttp->tpk[i].t0 - tz;
				if ( 60*60 < dt ) {			/* more than 1 hour */
					continue;
				}
			} else {
				dt = tz - ttp->tpk[i].t1;
				if ( 7*24*60*60 < dt ) {	/* more than 1 week */
					continue;
				}
			}

			if ( -1 == imin ) {
				imin = i;
				dNmin = dNunsig;
			} else if ( dNunsig <= dNmin ) {
				imin = i;
				dNmin = dNunsig;
			}
		}
	}

	if ( -1 == imin ) {
		fprintf(stderr, "\
%s: no valid time interval for N=%u, tz=%.0f\n", pname, N, tz);
		return -1;
	}

	NN = N;
	N0 = ttp->tpk[imin].N0;
	NL = (N0 - margin) & 0xffffffff;
	NN = (NN - NL) & 0xffffffff;
	N0 = (N0 - NL) & 0xffffffff;
	dN = NN - N0;

	f = ttp->tpk[imin].freq;

	Y = 0.0;
	dY = 0.0;

	t = ttp->tpk[imin].t0 + (dN * 4096.0 - dY / 100) / f;

/* set debug information */
	ttp->s_time = tz;
	ttp->calc_time = t;
	ttp->delt_time = ttp->s_time - ttp->calc_time;
	ttp->N = N;
	ttp->Y = Y;
	ttp->base_t0 = ttp->tpk[imin].t0;
	ttp->base_t1 = ttp->tpk[imin].t1;
	ttp->base_Y0 = ttp->tpk[imin].Y0;
	ttp->base_Y1 = ttp->tpk[imin].Y1;
	ttp->base_freq = ttp->tpk[imin].freq;
	ttp->base_N0 = ttp->tpk[imin].N0;
	ttp->base_N1 = ttp->tpk[imin].N1;
	ttp->base_jump = ttp->tpk[imin].flag_jump;
	ttp->base_discon = ttp->tpk[imin].flag_discon;
	ttp->dN = dN;
	ttp->dY = dY;

/* set output value */
	*aetime = t;

	return 0;
}

/************************************************************************
int aste_ti2time_dp()	: convert DP-TI -> Astro-E time

Input:
	TI2TIME *ttp		: TI2TIME pointer used for the time correction
	unsigned int N		: DP-TI (time indicator) of the packet, 1/4096 s
	double tz;			: Astro-E time when the packet was received

Output:
	double *aetime		: Astro-E time of packet creation after time correction

Return_Values:
	0					: success
	-1					: no valid time interval in TI2TIME *ttp
************************************************************************/
int
aste_ti2time_dp(TI2TIME *ttp, unsigned int N, double tz, double *aetime)
{
	int i, imin, dN, dN0, flag_cont, flag_cont_min, flag_update;
	unsigned int NN, N0, N1, NL, NH, dNunsig, dNmin, margin, N1N0;
	double t, dt, dt0, dtmin, f, Y, dY, Y1Y0, us, margin_sec_cont;
	struct time_pkt_struct *tpk;

	if ( ttp->simple ) {
		return simple_ti2time_dp(ttp, N, tz, aetime);
	}

	if ( ttp->rough ) {
		return rough_ti2time_dp(ttp, N, tz, aetime);
	}

	if ( 0 == ttp->ntpk ) {
		return -1;
	}

	imin = -1;
	margin = 30 * 60 * 4096;	/* 30 min */
	dNunsig = dNmin = 0;		/* dummy initialization for gcc warning */
	dtmin = 0.0;				/* dummy initialization for gcc warning */
	flag_cont_min = 0;			/* dummy initialization for gcc warning */
	margin_sec_cont = 60.0;		/* time margin in sec to judge continuity */

	for (i = 0; i < ttp->ntpk; i++) {

		NN = N;
		N0 = ttp->tpk[i].N0;
		NL = (N0 - margin) & 0xffffffff;
		N1 = ttp->tpk[i].N1;
		NH = (N1 + margin) & 0xffffffff;

		if ( ttp->tpk[i].flag_jump && N < N0 && N0/4096 < 7*24*60*60 ) {
			/* probably, TI was reset within 1 week */
			dNunsig = 0;
			goto skip;
		}

		NN = (NN - NL) & 0xffffffff;
		N0 = (N0 - NL) & 0xffffffff;
		N1 = (N1 - NL) & 0xffffffff;
		NH = (NH - NL) & 0xffffffff;
		NL = 0;

		if ( NL < NN && NN < NH ) {

			if ( NN < N0 ) {
				dNunsig = N0 - NN;
			} else if ( NN <= N1 ) {
				dNunsig = 0;
			} else {
				dNunsig = NN - N1;
			}

		skip:

/*			if ( 87077732 == N ) {
				printf("\
N=%u, i=%d, N0=%u, N1=%u, dN=%u, t0=%f, t0=%f, t1=%f\n",
N, i, com.tpk[i].N0, com.tpk[i].N1, dNunsig, t0, com.tpk[i].t0, com.tpk[i].t1);
				printf("\
  NN=%u, NL=%u, N0=%u, N1=%u, NH=%u\n",
NN, NL, N0, N1, NH);
			}*/

			if ( tz < ttp->tpk[i].t0 ) {
				dt = ttp->tpk[i].t0 - tz;
				if ( 60*60 < dt ) {			/* more than 1 hour */
					continue;
				}
			} else {
				dt = tz - ttp->tpk[i].t1;
				if ( 7*24*60*60 < dt ) {	/* more than 1 week */
					continue;
				}
			}

/*			if ( 87077732 == N ) {
				printf("\
  dt=%f\n", dt);
			}*/

			dN0 = N - N0;
			dN0 /= 4096;
			dt0 = tz - ttp->tpk[i].t0;
			if ( dN0 < dt0 - margin_sec_cont || dt0 + margin_sec_cont < dN0 ) {
				flag_cont = 0;		/* judge as discontinuous */
			} else {
				flag_cont = 1;		/* judge as continuous */
			}

			if ( -1 == imin ) {
				imin = i;
				dNmin = dNunsig;
				dtmin = dt;
				flag_cont_min = flag_cont;
			} else if ( dNunsig <= dNmin ) {
				flag_update = 0;
				if ( 0 <= dt ) {
					if ( dt < dtmin ) {
						flag_update = 1;	/* 0 <= dt < dtmin */
					}
				} else {
					if ( 0 <= dtmin ) {
						flag_update = 1;	/* dt < 0 <= dtmin */
					} else if ( dtmin < dt ) {
						flag_update = 1;	/* dtmin < dt < 0 */
					}
				}
				if ( 0 == flag_cont && flag_cont_min ) {
				/* here, do not replace imin,
				   when previous is continuous && current is discontinuous */
					flag_update = 0;
				}
				if ( flag_update ) {
					imin = i;
					dNmin = dNunsig;
					dtmin = dt;
					flag_cont_min = flag_cont;
				}
			}
		}
	}

	if ( -1 == imin ) {
		fprintf(stderr, "\
%s: no valid time interval for N=%u, tz=%.0f\n", pname, N, tz);
		return -1;
	}

	tpk = &ttp->tpk[imin];

	NN = N;
	N0 = tpk->N0;
	NL = (N0 - margin) & 0xffffffff;
	NN = (NN - NL) & 0xffffffff;
	N0 = (N0 - NL) & 0xffffffff;
	dN = NN - N0;

	f = tpk->freq;
	if ( tpk->flag_discon ) {

		Y = 0.0;
		dY = 0.0;

	} else {

		if ( get_timc_val(ttp, N, tz, &Y, NULL) ) {
			fprintf(stderr, "\
%s: WARNING: get_timc_val(N=%u, tz=%.0f) failed, set dY=0\n", pname, N, tz);
			Y = 0.0;
			dY = 0.0;
		} else {
			dY = calc_dY(Y, tpk->Y0);
			Y1Y0 = calc_dY(tpk->Y1, tpk->Y0);
			N1N0 = tpk->N1 - tpk->N0;
			us = (dY - dN * Y1Y0 / N1N0) / 100 / f * 1.0e6;
			if ( 10e3 < fabs(us) ) {
				fprintf(stderr, "\
%s: WARNING: too large dY of %.0f us for N=%u, tz=%.0f\n", pname, us, N, tz);
			}
		}

	}

	t = tpk->t0 + (dN * 4096.0 - dY / 100) / f;

/* set debug information */
	ttp->s_time = tz;
	ttp->calc_time = t;
	ttp->delt_time = ttp->s_time - ttp->calc_time;
	ttp->N = N;
	ttp->Y = Y;
	ttp->base_t0 = tpk->t0;
	ttp->base_t1 = tpk->t1;
	ttp->base_Y0 = tpk->Y0;
	ttp->base_Y1 = tpk->Y1;
	ttp->base_freq = tpk->freq;
	ttp->base_N0 = tpk->N0;
	ttp->base_N1 = tpk->N1;
	ttp->base_jump = tpk->flag_jump;
	ttp->base_discon = tpk->flag_discon;
	ttp->dN = dN;
	ttp->dY = dY;

/* set output value */
	*aetime = t;

	return 0;
}


static int
get_avg_tc(TI2TIME *ttp, int i, double *tc0)
{
	int istat;
	double n0, tz0;

	if ( 0 == ttp->avg[i].flag_tc ) {
		n0 = ttp->avg[i].N + 0.5;
		tz0 = ttp->avg[i].tz;
		istat = aste_ti2time_dp_dbl(ttp, n0, tz0, &ttp->avg[i].tc);
		if ( istat ) {
			ttp->avg[i].flag_tc = -1;
			return istat;
		}
		ttp->avg[i].flag_tc = 1;
	}

	if ( ttp->avg[i].flag_tc < 0 ) {
		return -1;
	}

	*tc0 = ttp->avg[i].tc;
	return 0;
}

static int
check_H_between(int H, double H0, double H1)
{
	if ( H0 <= H1 ) {	/* no roll over */
		if ( H0 <= H && H <= H1 ) {
			return 1;
		}
	} else {			/* roll over exists */
		if ( H <= H1 || H0 <= H ) {
			return 1;
		}
	}
	return 0;
}

static int
rough_ti2time_dhu(TI2TIME *ttp, unsigned int DHU, double tz, double *aetime)
{
	int i, istat, H;
	double Hf, hv, h0, h1, nv, n0, n1, tz0, tz1, tc0, tc1;
	int i1, i2, found1, found2;
	double aetime1, aetime2;

	if ( 0 == ttp->navg ) {
		return -1;
	}

	H = (DHU/4) & 0xffffff;		/* 1/32 s -> 1/8 s */
	Hf = (DHU&3) / 4.0;			/* fractional part */

/* dummy initializations for gcc warning */
	i1 = i2 = 0;
	aetime1 = aetime2 = 0.0;

/* search valid entry between DHU-TI continuous time regions */
	for (i = 0; i < ttp->navg - 1; i++) {
		if ( 0 == ttp->avg[i].flag_cont ) {
			continue;	/* skip DHU-TI discontinuous interval */
		}
		if ( check_H_between(H, ttp->avg[i].H, ttp->avg[i+1].H ) ) {
			break;
		}
	}

	if ( i < ttp->navg - 1 ) {
/* found valid entry */
		hv = H + Hf;
		h0 = ttp->avg[i].H + 0.5;		/* add 0.5 for truncated bits */
		h1 = ttp->avg[i+1].H + 0.5;
		n0 = ttp->avg[i].N + 0.5;
		n1 = ttp->avg[i+1].N + 0.5;
		tz0 = ttp->avg[i].tz;
		tz1 = ttp->avg[i+1].tz;

		if ( hv < h0 - 1.0 ) {
			hv += 16777216.0;		/* 2^24 */
		}
		if ( h1 < h0 ) {
			h1 += 16777216.0;		/* 2^24 */
		}

		if ( 0 == ttp->avg[i].flag_cont ) {
			/* DHU-TI is continuous, but DP-TI is not */
			nv = 0.0;

		interpolation:

			istat = get_avg_tc(ttp, i, &tc0);
			if ( istat ) goto finish;
			istat = get_avg_tc(ttp, i+1, &tc1);
			if ( istat ) goto finish;
			*aetime = tc0 + (hv - h0) * (tc1 - tc0) / (h1 - h0);
			goto finish;
		}

		if ( n1 < n0 ) {
			n1 += 4294967296.0;		/* 2^32 */
		}
		nv = n0 + (hv - h0) * (n1 - n0) / (h1 - h0);
		if ( 4294967296.0 <= nv ) {
			nv -= 4294967296.0;
		}

		goto interpolation;

	}

	found1 = found2 = 0;

/* search entry at smaller side of discontinuous time regions */
	for (i = 0; i < ttp->navg - 1; i++) {
		if ( 0 < i && ttp->avg[i-1].flag_cont ) {
			continue;		/* skip continuous interval */
		}
		if ( H <= ttp->avg[i].H &&
			 ttp->avg[i].tz - ttp->avg[i].H/8.0 - 300.0 <= tz ) {
			break;
		}
	}

	if ( i < ttp->navg - 1 ) {
/* found valid entry */
		found1 = 1;
		i1 = i;
		hv = H + Hf;
		h0 = h1 = ttp->avg[i].H + 0.5;
		n0 = n1 = ttp->avg[i].N + 0.5;
		nv = n0 - (h0 - hv)*(4096/8);
		tz0 = ttp->avg[i].tz0;
		tz1 = ttp->avg[i].tz;
		while ( nv < 0.0 ) {
			nv += 4294967296.0;
		}
		istat = get_avg_tc(ttp, i, &tc0);
		aetime1 = tc0 - (h0 - hv)/8.0;
	}

/* search entry at larger side of discontinuous time regions */
	for (i = 0; i < ttp->navg; i++) {
		if ( ttp->avg[i].flag_cont ) {
			continue;		/* skip continuous interval */
		}
		if ( ttp->avg[i].H < H &&
			 ttp->avg[i].tz - 300.0 <= tz ) {
			break;
		}
	}

	if ( i < ttp->navg ) {
/* found valid entry */
		found2 = 1;
		i2 = i;
		hv = H + Hf;
		h0 = h1 = ttp->avg[i].H + 0.5;
		n0 = n1 = ttp->avg[i].N + 0.5;
		nv = n0 + (hv - h0)*(4096/8);
		tz0 = ttp->avg[i].tz;
		tz1 = ttp->avg[i].tz1;
		while ( 4294967296.0 <= nv ) {
			nv -= 4294967296.0;
		}
		istat = get_avg_tc(ttp, i, &tc0);
		aetime2 = tc0 + (hv - h0)/8.0;
	}

	if ( found1 && found2 ) {
		if ( check_H_between(H, ttp->avg[i1].H0, ttp->avg[i1].H1) ) {
			goto use_1;
		}
		if ( check_H_between(H, ttp->avg[i2].H0, ttp->avg[i2].H1) ) {
			goto use_2;
		}
		if ( fabs(tz - aetime2) < fabs(tz - aetime1) ) {
			goto use_2;
		}
		goto use_1;
	}

	if ( found1 ) {
	use_1:
		i = i1;
		hv = H + Hf;
		h0 = h1 = ttp->avg[i].H + 0.5;
		n0 = n1 = ttp->avg[i].N + 0.5;
		nv = n0 - (h0 - hv)*(4096/8);
		tz0 = ttp->avg[i].tz0;
		tz1 = ttp->avg[i].tz;
		while ( nv < 0.0 ) {
			nv += 4294967296.0;
		}
		istat = get_avg_tc(ttp, i, &tc0);
		*aetime = aetime1;
		goto finish;
	}

	if ( found2 ) {
	use_2:
		i = i2;
		hv = H + Hf;
		h0 = h1 = ttp->avg[i].H + 0.5;
		n0 = n1 = ttp->avg[i].N + 0.5;
		nv = n0 + (hv - h0)*(4096/8);
		tz0 = ttp->avg[i].tz;
		tz1 = ttp->avg[i].tz1;
		while ( 4294967296.0 <= nv ) {
			nv -= 4294967296.0;
		}
		istat = get_avg_tc(ttp, i, &tc0);
		*aetime = aetime2;
		goto finish;
	}

	return -1;

/* set debug information */

 finish:

	ttp->s_time = tz;
	ttp->calc_time = *aetime;
	ttp->delt_time = ttp->s_time - ttp->calc_time;
	ttp->DHU = DHU;
	ttp->H = H;
	ttp->Hf = Hf;
	ttp->nv = nv;
	ttp->n0 = n0;
	ttp->n1 = n1;
	ttp->hv = hv;
	ttp->h0 = h0;
	ttp->h1 = h1;
	ttp->tz0 = tz0;
	ttp->tz1 = tz1;

	return istat;
}


/************************************************************************
int aste_ti2time_dhu()	: convert DHU-TI -> Astro-E time

Input:
	TI2TIME *ttp		: TI2TIME pointer used for the time correction
	unsigned int DHU	: DHU-TI (time indicator) of the packet, 1/32 s
	double tz;			: Astro-E time when the packet was received

Output:
	double *aetime		: Astro-E time of packet creation after time correction

Return_Values:
	0					: success
	-1					: no valid time interval in TI2TIME *ttp
************************************************************************/
int
aste_ti2time_dhu(TI2TIME *ttp, unsigned int DHU, double tz, double *aetime)
{
	int i, istat, H;
	double dt, dH, Hf, hv, h0, h1, nv, n0, n1, tz0, tz1, tc0, tc1;
	int i1, i2, found1, found2;
	double aetime1, aetime2;

	if ( ttp->simple ) {
		return simple_ti2time_dhu(ttp, DHU, tz, aetime);
	}

	if ( ttp->rough ) {
		return rough_ti2time_dhu(ttp, DHU, tz, aetime);
	}

	if ( 0 == ttp->navg ) {
		return -1;
	}

	H = (DHU/4) & 0xffffff;		/* 1/32 s -> 1/8 s */
	Hf = (DHU&3) / 4.0;			/* fractional part */

/* dummy initializations for gcc warning */
	i1 = i2 = 0;
	aetime1 = aetime2 = 0.0;

/* search valid entry between DHU-TI continuous time regions */
	for (i = 0; i < ttp->navg - 1; i++) {
		if ( 0 == ttp->avg[i].flag_cont ) {
			dt = ttp->avg[i+1].tz - ttp->avg[i].tz;
			dH = ttp->avg[i+1].H - ttp->avg[i].H;
			if ( dH < 0.0 ) {
				dH += 16777216.0;		/* 2^24 */
			}
			if ( dt < 0.99 * dH/8 || 1.01 * dH/8 < dt ) {
/* found discontinuity in DHU-TI. Here, +-1% difference is allowed */
				continue;	/* skip DHU-TI discontinuous interval */
			}
		}
		if ( tz < ttp->avg[i].tz - 300.0 || ttp->avg[i+1].tz + 300.0 < tz ) {
			continue;	/* skip when S_TIME outside with +-5 min margin */
		}
		if ( check_H_between(H, ttp->avg[i].H, ttp->avg[i+1].H ) ) {
			break;
		}
	}

	if ( i < ttp->navg - 1 ) {
/* found valid entry */
		hv = H + Hf;
		h0 = ttp->avg[i].H + 0.5;		/* add 0.5 for truncated bits */
		h1 = ttp->avg[i+1].H + 0.5;
		n0 = ttp->avg[i].N + 0.5;
		n1 = ttp->avg[i+1].N + 0.5;
		tz0 = ttp->avg[i].tz;
		tz1 = ttp->avg[i+1].tz;

		if ( hv < h0 - 1.0 ) {
			hv += 16777216.0;		/* 2^24 */
		}
		if ( h1 < h0 ) {
			h1 += 16777216.0;		/* 2^24 */
		}

		if ( 0 == ttp->avg[i].flag_cont ) {
			/* DHU-TI is continuous, but DP-TI is not */
			nv = 0.0;

		interpolation:

			istat = get_avg_tc(ttp, i, &tc0);
			if ( istat ) goto finish;
			istat = get_avg_tc(ttp, i+1, &tc1);
			if ( istat ) goto finish;
			*aetime = tc0 + (hv - h0) * (tc1 - tc0) / (h1 - h0);
			goto finish;
		}

		if ( n1 < n0 ) {
			n1 += 4294967296.0;		/* 2^32 */
		}
		nv = n0 + (hv - h0) * (n1 - n0) / (h1 - h0);
		if ( 4294967296.0 <= nv ) {
			nv -= 4294967296.0;
		}
#if 0
/* 2005/04/25 Y.ISHISAKI
	This method might be accurate, but valid entry for nv does not always
	found, which produces a number of error messages, like

aste_ti2time: no valid time interval for N=4195041791, tz=166109644
aste_ti2time: no valid time interval for N=4195042047, tz=166109644
...

	Practically, DHU time assignment does not need such accuracy.
*/
		istat = aste_ti2time_dp_dbl(ttp, nv, tz, aetime);
		goto finish;
#else
		goto interpolation;
#endif

	}

	found1 = found2 = 0;

/* search entry at smaller side of discontinuous time regions */
	for (i = 0; i < ttp->navg - 1; i++) {
		if ( 0 < i && ttp->avg[i-1].flag_cont ) {
			continue;		/* skip continuous interval */
		}
		if ( H <= ttp->avg[i].H &&
			 tz <= ttp->avg[i].tz + 300.0 &&
			 ttp->avg[i].tz - ttp->avg[i].H/8.0 - 300.0 <= tz ) {
			break;
		}
	}

	if ( i < ttp->navg - 1 ) {
/* found valid entry */
		found1 = 1;
		i1 = i;
		hv = H + Hf;
		h0 = h1 = ttp->avg[i].H + 0.5;
		n0 = n1 = ttp->avg[i].N + 0.5;
		nv = n0 - (h0 - hv)*(4096/8);
		tz0 = ttp->avg[i].tz0;
		tz1 = ttp->avg[i].tz;
		while ( nv < 0.0 ) {
			nv += 4294967296.0;
		}
		istat = get_avg_tc(ttp, i, &tc0);
		aetime1 = tc0 - (h0 - hv)/8.0;
	}

/* search entry at larger side of discontinuous time regions */
	for (i = 0; i < ttp->navg; i++) {
		if ( ttp->avg[i].flag_cont ) {
			continue;		/* skip continuous interval */
		}
		if ( ttp->avg[i].H < H &&
			 tz <= ttp->avg[i].tz1 + 300.0 &&
			 ttp->avg[i].tz - 300.0 <= tz ) {
			break;
		}
	}

	if ( i < ttp->navg ) {
/* found valid entry */
		found2 = 1;
		i2 = i;
		hv = H + Hf;
		h0 = h1 = ttp->avg[i].H + 0.5;
		n0 = n1 = ttp->avg[i].N + 0.5;
		nv = n0 + (hv - h0)*(4096/8);
		tz0 = ttp->avg[i].tz;
		tz1 = ttp->avg[i].tz1;
		while ( 4294967296.0 <= nv ) {
			nv -= 4294967296.0;
		}
		istat = get_avg_tc(ttp, i, &tc0);
		aetime2 = tc0 + (hv - h0)/8.0;
	}

	if ( found1 && found2 ) {
		if ( check_H_between(H, ttp->avg[i1].H0, ttp->avg[i1].H1) ) {
			goto use_1;
		}
		if ( check_H_between(H, ttp->avg[i2].H0, ttp->avg[i2].H1) ) {
			goto use_2;
		}
		if ( fabs(tz - aetime2) < fabs(tz - aetime1) ) {
			goto use_2;
		}
		goto use_1;
	}

	if ( found1 ) {
	use_1:
		i = i1;
		hv = H + Hf;
		h0 = h1 = ttp->avg[i].H + 0.5;
		n0 = n1 = ttp->avg[i].N + 0.5;
		nv = n0 - (h0 - hv)*(4096/8);
		tz0 = ttp->avg[i].tz0;
		tz1 = ttp->avg[i].tz;
		while ( nv < 0.0 ) {
			nv += 4294967296.0;
		}
		istat = get_avg_tc(ttp, i, &tc0);
		*aetime = aetime1;
		goto finish;
	}

	if ( found2 ) {
	use_2:
		i = i2;
		hv = H + Hf;
		h0 = h1 = ttp->avg[i].H + 0.5;
		n0 = n1 = ttp->avg[i].N + 0.5;
		nv = n0 + (hv - h0)*(4096/8);
		tz0 = ttp->avg[i].tz;
		tz1 = ttp->avg[i].tz1;
		while ( 4294967296.0 <= nv ) {
			nv -= 4294967296.0;
		}
		istat = get_avg_tc(ttp, i, &tc0);
		*aetime = aetime2;
		goto finish;
	}

	return -1;

/* set debug information */

 finish:

	ttp->s_time = tz;
	ttp->calc_time = *aetime;
	ttp->delt_time = ttp->s_time - ttp->calc_time;
	ttp->DHU = DHU;
	ttp->H = H;
	ttp->Hf = Hf;
	ttp->nv = nv;
	ttp->n0 = n0;
	ttp->n1 = n1;
	ttp->hv = hv;
	ttp->h0 = h0;
	ttp->h1 = h1;
	ttp->tz0 = tz0;
	ttp->tz1 = tz1;

	return istat;
}


/************************************************************************
int aste_ti2time()		: convert TI -> Astro-E time

Input:
	TI2TIME *ttp		: TI2TIME pointer used for the time correction
	int apid			: APID (application process id) of the packet
	unsigned int N		: TI (time indicator) of the packet,
							1/4096 s for DP, 1/32 s for DHU
	double tz;			: Astro-E time when the packet was received

Output:
	double *aetime		: Astro-E time of packet creation after time correction

Return_Values:
	0					: success
	others				: error, see aste_ti2time_dp/dhu()
************************************************************************/
int
aste_ti2time(TI2TIME *ttp, int apid, unsigned int N, double tz, double *aetime)
{
	int istat;
	int component_id;

	component_id = (apid >> 5) & 0x0f;

	if ( 1 <= component_id && component_id <= 5 ) {
		/* DHU-TI */
		istat = aste_ti2time_dhu(ttp, N, tz, aetime);
	} else {
		/* DP-TI */
		istat = aste_ti2time_dp(ttp, N, tz, aetime);
	}

	ttp->apid = apid;
	return istat;
}


/************************************************************************
int aste_ti2time_dbl()	: convert TI (double) -> Astro-E time

Input:
	TI2TIME *ttp		: TI2TIME pointer used for the time correction
	int apid			: APID (application process id) of the packet
	double N			: TI (time indicator) of the packet,
							1/4096 s for DP, 1/32 s for DHU
	double tz;			: Astro-E time when the packet was received

Output:
	double *aetime		: Astro-E time of packet creation after time correction

Return_Values:
	0					: success
	others				: error, see aste_ti2time_dp/dhu()
************************************************************************/
int
aste_ti2time_dbl(TI2TIME *ttp, int apid, double N, double tz, double *aetime)
{
	int istat;
	unsigned int N0, N1;
	double t0, t1;

	N0 = floor(N);
	N1 = N0 + 1;

	istat = aste_ti2time(ttp, apid, N0, tz, &t0);
	if ( istat ) {
		return istat;
	}

	if ( (double)N0 == N ) {
		*aetime = t0;
		return 0;
	}

	istat = aste_ti2time(ttp, apid, N1, tz, &t1);
	if ( istat ) {
		return istat;
	}

	*aetime = t0 * (N1 - N) + t1 * (N - N0);

	return 0;
}

int
aste_ti2time_dp_dbl(TI2TIME *ttp, double N, double tz, double *aetime)
{
	int istat;

	istat = aste_ti2time_dbl(ttp, 0, N, tz, aetime);

	return istat;
}

int
aste_ti2time_dhu_dbl(TI2TIME *ttp, double N, double tz, double *aetime)
{
	int istat;

	istat = aste_ti2time_dbl(ttp, 32, N, tz, aetime);

	return istat;
}
