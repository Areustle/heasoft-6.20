/*

  aste_orbit: calculate satellite position for Astro-E2

	2005-08-30	Y.ISHISAKI	version 1.0

	2005-10-24	Y.ISHISAKI	version 1.2 (to match with AEorbitFitsWrite 1.2)
		add int version; in ORBIT
		change EXTNAME="ORBIT"->"PAR_ORBIT", EXTVER=2->0 in read_orbit_file()
		call atSetElement2() & atSatPos() if "PAR_ORBIT" not found, for USC
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "aste_orbit.h"

static char pname[] = "aste_orbit";
char aste_orbit_version[] = "1.2";

struct row_data {
	double w;
	KEPLERIAN k;
};

static void
KeplerianToCartesian(KEPLERIAN *kp, AtVect xyz, AtVect vxyz)
{
/* Gravitational constant times mass of Earth [ÃÏ¿´½ÅÎÏÄê¿ô] (km^3/s^3) */
#define GM	3.986005e5

	double a;	/* semi-major axis [µ°Æ»Ä¹È¾·Â] (km) */
	double b;	/* semi-minor axis [µ°Æ»Ã»È¾·Â] (km) */
	double e;	/* eccentricity [µ°Æ»Î¥¿´Î¨] */
	double u;	/* eccentric anomaly [Î¥¿´¶áÅÀÎ¥³Ñ] (rad) */
	double cosu, sinu;
	double M_dot;	/* 2 pi / T, where T = 2 pi a**1.5 / sqrt(GM) */
	double u_dot;	/* u_dot * (1 - e cos(u)) = M_dot, from Kepler equation */
    AtVect xys, vxys;

	double ai;	/* I:  Inclination (rad) */
	double ob;	/* AN: Right Ascension of Ascending Node (rad) */
	double ol;	/* AP: Argument of Perigee, angle from ascending node (rad) */
    double q1, q2, q3, q4, q5, q6, qq1, qq2;

	a = kp->A;
	e = kp->E;
	b = a * sqrt(1 - e*e);
    atKepler(kp->MA * DEG2RAD, e, &u);
	cosu = cos(u);
	sinu = sin(u);

	ai = kp->I  * DEG2RAD;
	ob = kp->AN * DEG2RAD;
	ol = kp->AP * DEG2RAD;

	xys[0] = a * (cosu - e);
	xys[1] = b * sinu;
/*	xys[2] = 0.0;*/

/* copied from atOrbPlane() */
	q1 = cos(ob);
	q2 = sin(ob);
	q3 = cos(ai);
	q4 = sin(ai);
    q5 = cos(ol);
    q6 = sin(ol);
    qq1 = xys[0] * q5 - xys[1] * q6;
    qq2 = xys[0] * q6 + xys[1] * q5;
    xyz[0] = q1 * qq1 - q2 * (qq2 * q3 /*- q4 * xys[2]*/);
    xyz[1] = q2 * qq1 + q1 * (qq2 * q3 /*- q4 * xys[2]*/);
    xyz[2] = q4 * qq2 /*+ q3 * xys[2]*/;
/* end of atOrbPlane() */

	if ( NULL != vxyz ) {
		M_dot = sqrt(GM / a) / a;
		u_dot = M_dot / (1 - e*cosu);
		vxys[0] = - a * sinu * u_dot;
		vxys[1] = b * cosu * u_dot;
/*		vxys[2] = 0.0;*/
		qq1 = vxys[0] * q5 - vxys[1] * q6;
		qq2 = vxys[0] * q6 + vxys[1] * q5;
		vxyz[0] = q1 * qq1 - q2 * (qq2 * q3 /*- q4 * vxys[2]*/);
		vxyz[1] = q2 * qq1 + q1 * (qq2 * q3 /*- q4 * vxys[2]*/);
		vxyz[2] = q4 * qq2 /*+ q3 * vxys[2]*/;
	}
}

static double
weight_ang(double s, double a0, double a1)
{
	double da, a;

	if ( 0.0 == s ) {
		return a0;
	}

	if ( 1.0 == s ) {
		return a1;
	}

	da = a1 - a0;
	while ( da < -180.0 ) {			/* e.g. a0=359, a1=1 */
		da = da + 360.0;
	}
	while ( 180.0 < da ) {		/* e.g. a0=1, a1=359 */
		da = da - 360.0;
	}

	a = a0 + s * da;
	if ( 360.0 < a ) {
		a -= 360.0;
	} else if ( a < 0.0 ) {
		a += 360.0;
	}

	return a;
}

static void
weight_kepler(double s, KEPLERIAN *k0, KEPLERIAN *k1, KEPLERIAN *kp)
{
	if ( 0.0 == s ) {
		*kp = *k0;
	} else if ( 1.0 == s ) {
		*kp = *k1;
	} else {
		kp->t = k0->t;	/* k0->t == k1->t */
		kp->A = k0->A + s * (k1->A - k0->A);
		kp->E = k0->E + s * (k1->E - k0->E);
		kp->I = weight_ang(s, k0->I, k1->I);
		kp->AN = weight_ang(s, k0->AN, k1->AN);
		kp->AP = weight_ang(s, k0->AP, k1->AP);
		kp->MA = weight_ang(s, k0->AP+k0->MA, k1->AP+k1->MA) - kp->AP;
	}
}

static int
compare_row_data(struct row_data *p1, struct row_data *p2)
{
	if ( p1->k.t < p2->k.t ) {
		return -1;
	} else if ( p1->k.t > p2->k.t ) {
		return +1;
	}
	return 0;
}

static int
read_orbit_file_v1(double aetime, ORBIT *obp)
{
	static int kchk = 0;	/* 0: calculate 2nd derivative of MA from A-dot */

	int istat, days;
	double mjd;
	AtElement elem;

/* read current orbital element */
	mjd = aste2mjd(aetime);
	istat = atSetElement2(obp->orbit_file, mjd, kchk);
	if ( istat ) {
		return istat;
	}

/* save current orbital element */
	elem = atElement;

/* search next orbital element update */
	for (days = 1; days <= 14; days++) {
		atSetElement2(obp->orbit_file, elem.mjdz + days, kchk);
		if ( elem.mjdz != atElement.mjdz ) {
			break;
		}
	}

/* remember valid time interval in tstart, tstop */
	obp->tstart = mjd2aste(elem.mjdz);
	obp->tstop  = mjd2aste(atElement.mjdz);

/* restore current orbital element */
	atElement = elem;

/* set version = 1 */
	obp->version = 1;

	return 0;
}

static int
read_orbit_file_v2(ORBIT *obp)
{
	static char extname[] = "PAR_ORBIT";
	static int extver = 0;
	static int hdutype = BINARY_TBL;
	static int cs = CASEINSEN;

	fitsfile *fp;
	long irow;
	int i, j, nkp, nrow, anul, hdunum;
	char *key;
	struct { int w, t, A, E, I, AN, AP, MA; } co;
	struct row_data *row, *rp;
	KEPLERIAN *kp;
	int istat, istat2;
	double s, w0, w1, ws;

	fp = NULL;
	kp = NULL;
	istat = istat2 = 0;

/* print information message */
	fflush(stdout); fflush(stderr); printf("\
%s: reading '%s[EXTNAME=%s,EXTVER=%d]' ...\n",
		pname, obp->orbit_file, extname, extver);
	fflush(stdout); fflush(stderr);

/* open file */
	fits_open_file(&fp, obp->orbit_file, READONLY, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: orbit file '%s' open failed (%d)\n", pname, obp->orbit_file, istat);
		goto try_v1;
	}
	fits_get_hdu_num(fp, &hdunum);
	if ( 1 == hdunum ) {
		fits_movnam_hdu(fp, hdutype, extname, extver, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_movnam_hdu('%s',%d) failed (%d)\n", pname, extname, extver, istat);
			fits_close_file(fp, &istat2);
			fp = NULL;
			istat2 = 0;		/* ignore error */
			goto try_v1;
		}
	}
	fits_read_key(fp, TINT, "NAXIS2", &nrow, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key('NAXIS2') failed (%d)\n", pname, istat);
		goto error;
	} else if ( 0 == nrow ) {
		fprintf(stderr, "\
%s: no rows in orbit file '%s'\n", pname, obp->orbit_file);
		istat = -1;		/* no rows */
		goto error;
	}

/* allocate memory */
	kp = malloc( sizeof(*kp) * nrow + sizeof(*row) * nrow );
	/* dummy realloc for purify
	kp = realloc( kp, sizeof(*kp) * nrow + sizeof(*row) * nrow ); */
	row = (struct row_data *)&kp[nrow];
	if ( NULL == kp ) {
		fprintf(stderr, "\
%s: malloc() failed for KEPLERIAN *kp (nrow=%d)\n", pname, nrow);
		istat = -1;		/* malloc error */
		goto error;
	}

/* get column numbers */
	if (
fits_get_colnum(fp, cs, key="WEIGHT", &co.w,  &istat) ||
		 0 ) {
		fprintf(stderr, "\
%s: WARNING: fits_get_colnum('%s') failed, ignored (%d)\n", pname, key, istat);
		istat = 0;
		co.w = -1;
	}

	if (
fits_get_colnum(fp, cs, key="TIME", &co.t,  &istat) ||
fits_get_colnum(fp, cs, key="A",	&co.A,  &istat) ||
fits_get_colnum(fp, cs, key="E",	&co.E,  &istat) ||
fits_get_colnum(fp, cs, key="I",	&co.I,  &istat) ||
fits_get_colnum(fp, cs, key="AN",	&co.AN, &istat) ||
fits_get_colnum(fp, cs, key="AP",	&co.AP, &istat) ||
fits_get_colnum(fp, cs, key="MA",	&co.MA, &istat) ||
		 0 ) {
		fprintf(stderr, "\
%s: fits_get_colnum('%s') failed (%d)\n", pname, key, istat);
		goto error;
	}

/* read table contents */
	for (irow = 1; irow <= nrow; irow++) {
		rp = &row[irow-1];

		rp->w = 1.0;
		if ( 0 < co.w ) {
fits_read_col_dbl(fp, co.w,  irow, 1, 1, 0.0, &rp->w,  &anul, &istat);
			if ( istat ) {
				fprintf(stderr, "\
%s: fits_read_col() failed at irow=%ld\n", pname, irow);
				goto error;
			}
		}

		if (
fits_read_col_dbl(fp, co.t,  irow, 1, 1, 0.0, &rp->k.t,  &anul, &istat) ||
fits_read_col_dbl(fp, co.A,  irow, 1, 1, 0.0, &rp->k.A,  &anul, &istat) ||
fits_read_col_dbl(fp, co.E,  irow, 1, 1, 0.0, &rp->k.E,  &anul, &istat) ||
fits_read_col_dbl(fp, co.I,  irow, 1, 1, 0.0, &rp->k.I,  &anul, &istat) ||
fits_read_col_dbl(fp, co.AN, irow, 1, 1, 0.0, &rp->k.AN, &anul, &istat) ||
fits_read_col_dbl(fp, co.AP, irow, 1, 1, 0.0, &rp->k.AP, &anul, &istat) ||
fits_read_col_dbl(fp, co.MA, irow, 1, 1, 0.0, &rp->k.MA, &anul, &istat) ||
			0 ) {
			fprintf(stderr, "\
%s: fits_read_col() failed at irow=%ld\n", pname, irow);
			goto error;
		}

	}

/* close file */
	fits_close_file(fp, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_close_file() failed (%d)\n", pname, istat);
		goto error;
	}

/* sort by time */
	qsort(row, nrow, sizeof(*row), compare_row_data);

/* set tstart, tstop */
	obp->tstart = row[0].k.t;
	obp->tstop = row[nrow-1].k.t;
	obp->prev_ipos = 0;

/* check duplicated rows */
	i = 0;
	nkp = 0;
	while ( i < nrow ) {
		for (j = i + 1; j < nrow; j++) {
			if ( row[i].k.t != row[j].k.t ) {
				break;
			}
		}
		if ( 2 < j - i ) {
			fprintf(stderr, "\
%s: more than two duplicated rows at irow=%d\n", pname, i+1);
			istat = -1;		/* too many duplicated rows */
			goto error;
		} else if ( 1 < j - i ) {
			w0 = row[i].w;
			w1 = row[i+1].w;
			ws = w0 + w1;
			if ( 0.0 == ws ) {
				s = 0.0;
			} else {
				s = w1 / ws;
			}
			weight_kepler(s, &row[i].k, &row[i+1].k, &kp[nkp]);
		} else {
			kp[nkp] = row[i].k;
		}
		nkp++;
		i = j;
	}

/* shrink memory */
	obp->kp = kp = realloc(kp, sizeof(*kp) * nkp);
	obp->nkp = nkp;

	if ( NULL == kp ) {
		fprintf(stderr, "\
%s: realloc() failed for obp->kp (nkp=%d)\n", pname, nkp);
		istat = -1;		/* malloc error */
		goto error;
	}

/* print information message */
	fflush(stdout); fflush(stderr); printf("\
   nrow=%d, nkp=%d, tstart=%.1f, tstop=%.1f\n",
		nrow, nkp, obp->tstart, obp->tstop);
	fflush(stdout); fflush(stderr);

/* set version = 2 */
	obp->version = 2;

	return 0;

 try_v1:
	istat = read_orbit_file_v1(0.0, obp);
	if ( istat ) {
		fprintf(stderr, "\
%s: trying with atSetElement() failed\n", pname);
		return istat;
	}
	fflush(stdout); fflush(stderr); printf("\
%s: trying with atSetElement() success\n", pname);
	fflush(stdout); fflush(stderr);
	return 0;

 error:

	if ( NULL != kp ) {
		free(kp);
	}

	if ( NULL != fp ) {
		fits_close_file(fp, &istat2);
	}

	obp->nkp = 0;
	obp->kp = NULL;

	return istat;
}


/************************************************************************
int aste_orbit_init()	: initialize ORBIT information

Input:
	char *orbit_file	: orbit FITS file name

Output:
	ORBIT **obpp		: pointer to ORBIT pointer after initialization

Return_Values:
	0					: success
	-1					: malloc() error
	others				: CFITSIO error
************************************************************************/
int
aste_orbit_init(ORBIT **obpp, char *orbit_file)
{
	ORBIT *obp;
	int istat, len_orbit_file;

	len_orbit_file = strlen(orbit_file);

	obp = malloc( sizeof(*obp) + (len_orbit_file+1) );
	if ( NULL == obp ) {
		fprintf(stderr, "\
%s: malloc() failed for ORBIT *obp\n", pname);
		return -1;
	}

	obp->orbit_file = (char *)(obp + 1);
	strcpy(obp->orbit_file, orbit_file);
	obp->kp = NULL;

	istat = read_orbit_file_v2(obp);
	if ( istat ) {
		aste_orbit_free(obp);
		return istat;
	}

	*obpp = obp;
	return 0;
}


/************************************************************************
int aste_orbit_free()	: free memory of ORBIT

Input:
	ORBIT *obp		: ORBIT pointer to free

Return_Values:
	0					: success
************************************************************************/
int
aste_orbit_free(ORBIT *obp)
{
	if ( NULL == obp ) {
		return -1;
	}

	if ( NULL != obp->kp ) {
		free(obp->kp);
	}

	free(obp);

	return 0;
}


/************************************************************************
int aste_orbit()		: get sidereal vector to the satellite

Input:
	ORBIT *obp			: ORBIT pointer used for the calculation
	double aetime		: Astro-E time when the packet was received

Output:
	AtVect xyz			: sidereal vector to the satellite (km)
	AtVect vxyz			: sidereal vector of the satellite velocity (km/s)

Return_Values:
	0					: success
	-1					: no valid time interval in ORBIT *obp
************************************************************************/
int
aste_orbit(ORBIT *obp, double aetime, AtVect xyz, AtVect vxyz)
{
	int i, nkp, istat;
	double dt, s, mjd;
	KEPLERIAN kp, *k0, *k1;

	if ( NULL == obp ) {
		fprintf(stderr, "\
%s: ORBIT obp == NULL\n", pname);
		return -1;
	}

	if ( 1 == obp->version ) {

		if ( aetime < obp->tstart ||
			 (obp->tstop != obp->tstart && obp->tstop <= aetime ) ) {
			istat = read_orbit_file_v1(aetime, obp);
			if ( istat ) {
				fprintf(stderr, "\
%s: read_orbit_file_v1() failed (%d)", pname, istat);
				return istat;
			}
		}

		mjd = aste2mjd(aetime);
		istat = atSatPos(mjd, xyz);
		if ( NULL != vxyz ) {	/* not supported with v1 */
			vxyz[0] = 0.0;
			vxyz[1] = 0.0;
			vxyz[2] = 0.0;
		}

		return istat;

	} else if ( 2 != obp->version ) {
		fprintf(stderr, "\
%s: unknown obp->version = %d\n", pname, obp->version);
		return -1;
	}

	k0 = k1 = NULL;		/* dummy initilization for gcc warning */

	i = obp->prev_ipos;
	k0 = &obp->kp[i];
	k1 = &obp->kp[i+1];
	if ( k0->t <= aetime && aetime <= k1->t ) {
		goto skip;
	} else if ( k1->t < aetime ) {
		nkp = obp->nkp;
		for (i = 0; i < nkp - 1; i++) {
			k1 = &obp->kp[i+1];
			if ( aetime <= k1->t ) {
				k0 = &obp->kp[i];
				goto skip;
			}
		}
	} else {
		while ( 0 < i ) {
			i--;
			k0 = &obp->kp[i];
			if ( k0->t <= aetime ) {
				k1 = &obp->kp[i+1];
				goto skip;
			}
		}
	}

	fprintf(stderr, "\
%s: aetime=%.1f out of range\n", pname, aetime);
	return -1;

 skip:

	obp->prev_ipos = i;

	dt = k1->t - k0->t;
	if ( 0.0 == dt ) {
		s = 0.0;
	} else {
		s = (aetime - k0->t) / dt;
	}

	weight_kepler(s, k0, k1, &kp);

	KeplerianToCartesian(&kp, xyz, vxyz);

	return 0;
}
