/*

  xis_contami: calculate transmission of the Suzaku XIS OBF contamination

	2006-05-24	Y.ISHISAKI	version 1.0

	2006-08-26	Y.ISHISAKI	version 1.1
		char *contami_file -> contamifile in XIS_CONTAMI structure
		add last_irow to cache irow in contamifile

	2006-08-26	Y.ISHISAKI	version 1.2
		read coratio in read_contami_file_v1()
		return carbon, oxygen column density in xis_contami()

	2009-12-01	Y.ISHISAKI	version 1.3
		modified for FORMAT_VERSION=2, variable elemental ratios
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "anl_msg.h"
#include "xis_contami.h"

static char pname[] = "xis_contami";
char xis_contami_version[] = "1.3";

static void
transmission_at(XIS_CONTAMI *xcp, double energy, double cohx_trans[4])
{
	int ipos, ie, ne;
	double trans, *en, *va, x0, x1, y0, y1, d, s, t;

	XIS_CONTAMI_TRANS_INDEX *idx = &xcp->index;

	cohx_trans[0] = 0.0;
	cohx_trans[1] = 1.0;
	cohx_trans[2] = 1.0;
	cohx_trans[3] = 1.0;

	if ( energy <= 0.0 ) {
		return;
	}

	ipos = idx->nbody * ( energy - idx->offs ) / idx->norm;
	if ( ipos < 0 ) {
		ipos = 0;
	} else if ( idx->nbody <= ipos ) {
		ipos = idx->nbody - 1;
	}
	ne = xcp->nen - 1;
	en = xcp->en;

	for (ie = idx->body[ipos]; ie < ne; ie++) {
		if ( energy < en[ie] ) break;
	}

	if ( ie <= 0 ) {
		ie = 1;
	}

	x0 = en[ie-1];
	x1 = en[ie];
	d = (x1 - x0);
	s = (x1 - energy) / d;
	t = (energy - x0) / d;

	va = xcp->vc;
	y0 = va[ie-1];
	y1 = va[ie];
	trans = s * y0 + t * y1;
	if ( trans < 0.0 ) {
		trans = 0.0;
	}
	cohx_trans[0] = trans;

	va = xcp->vo;
	if ( NULL != va ) {
		y0 = va[ie-1];
		y1 = va[ie];
		trans = s * y0 + t * y1;
		if ( trans < 0.0 ) {
			trans = 0.0;
		}
	}
	cohx_trans[1] = trans;

	va = xcp->vh;
	if ( NULL != va ) {
		y0 = va[ie-1];
		y1 = va[ie];
		trans = s * y0 + t * y1;
		if ( trans < 0.0 ) {
			trans = 0.0;
		}
	}
	cohx_trans[2] = trans;

	va = xcp->vx;
	if ( NULL != va ) {
		y0 = va[ie-1];
		y1 = va[ie];
		trans = s * y0 + t * y1;
		if ( trans < 0.0 ) {
			trans = 0.0;
		}
	}
	cohx_trans[3] = trans;

	return;
}

static int
get_format_version(fitsfile *fp, int *status)
{
	static char key[] = "CBD10001";
	static char f[] = "FORMAT_VERSION(";
	static int len = sizeof(f) - 1;

	char value[80];
	int format_version = -1;

	fits_read_key_str(fp, key, value, NULL, status);
	if ( *status ) {
		anl_msg_error("\
%s: %s keyword not found (%d)\n", pname, key, *status);
		return -1;
	}
	if ( 0 != strncmp(value, f, len) ||
		 1 != sscanf(value+len, "%d", &format_version) ) {
		anl_msg_error("\
%s: 'FORMAT_VERSION(n)' not found in %s keyword\n", pname, key);
		*status = KEY_NO_EXIST;
		return -1;
	}

	return format_version;
}

static int
read_contami_file_v1or2(XIS_CONTAMI *xcp)
{
	static char extname_growth[] = "CONTAMI_GROWTH";
	static char extname_trans[] = "CONTAMI_TRANS";
	static int extver = 0;
	static int hdutype = BINARY_TBL;
	static int cs = CASEINSEN;

	char *key;
	fitsfile *fp;
	long irow;
	int i, j, k, anul, ngp, nen;
	XIS_CONTAMI_GROWTH *gp;
	struct { int t, A, B, C, O, H, X, E, T; } co;
	double *en, *vc, *vo, *vh, *vx, t0, t1, offs, norm;
	int nbody, *body;
	int istat, istat2, format_version;

	fp = NULL;
	gp = NULL;
	en = NULL;
	istat = istat2 = 0;

/* print information message */
	anl_msg_info("\
%s: reading '%s' ...\n", pname, xcp->contamifile);

/* open file */
	fits_open_file(&fp, xcp->contamifile, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: contami file '%s' open failed (%d)\n", pname, xcp->contamifile, istat);
		goto error;
	}

/* move to "XIS_CONTAMI_GROWTH" */
	fits_movnam_hdu(fp, hdutype, extname_growth, extver, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_movnam_hdu('%s',%d) failed (%d)\n",
			pname, extname_growth, extver, istat);
		goto error;
	}
/* check "FORMAT_VERSION(n)" */
	format_version = get_format_version(fp, &istat);
	if ( format_version < 0 ) {
		goto error;
	} else if ( 1 != format_version && 2 != format_version ) {
		anl_msg_error("\
%s: FORMAT_VERSION(%d) not supported\n", pname, format_version);
		goto error;
	}
/* get "NAXIS2" */
	fits_read_key(fp, TINT, "NAXIS2", &ngp, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('NAXIS2') failed (%d)\n", pname, istat);
		goto error;
	} else if ( 0 == ngp ) {
		anl_msg_error("\
%s: no rows in contami file '%s'\n", pname, xcp->contamifile);
		istat = -1;		/* no rows */
		goto error;
	}

/* allocate memory */
	xcp->ngp = ngp;
	xcp->gp = gp = malloc( sizeof(*gp) * ngp );
	if ( NULL == gp ) {
		anl_msg_error("\
%s: malloc() failed for XIS_CONTAMI_GROWTH *gp (ngp=%d)\n", pname, ngp);
		istat = -1;		/* malloc error */
		goto error;
	}

/* get column numbers */
	if (
fits_get_colnum(fp, cs, key="TIME", &co.t, &istat) ||
fits_get_colnum(fp, cs, key="A",	&co.A, &istat) ||
fits_get_colnum(fp, cs, key="B",	&co.B, &istat) ||
fits_get_colnum(fp, cs, key="C",	&co.C, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, key, istat);
		goto error;
	}
	if ( 2 == format_version ) {
		if (
fits_get_colnum(fp, cs, key="O",	&co.O, &istat) ||
fits_get_colnum(fp, cs, key="H",	&co.H, &istat) ||
fits_get_colnum(fp, cs, key="X",	&co.X, &istat) ||
			 0 ) {
			anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, key, istat);
			goto error;
		}
	}

/* read table contents */
	for (irow = 1; irow <= ngp; irow++) {

		if (
fits_read_col_dbl(fp, co.t, irow, 1, 1, 0.0, &gp[irow-1].t, &anul, &istat) ||
fits_read_col_dbl(fp, co.A, irow, 1, 1, 0.0, &gp[irow-1].A, &anul, &istat) ||
fits_read_col_dbl(fp, co.B, irow, 1, 1, 0.0, &gp[irow-1].B, &anul, &istat) ||
fits_read_col_dbl(fp, co.C, irow, 1, 1, 0.0, &gp[irow-1].C, &anul, &istat) ||
			 0 ) {
			anl_msg_error("\
%s: fits_read_col() failed at irow=%ld\n", pname, irow);
			goto error;
		}

		if ( 1 == format_version ) {

			gp[irow-1].O = 0;
			gp[irow-1].H = 0;
			gp[irow-1].X = 0;

		} else {
			if (
fits_read_col_dbl(fp, co.O, irow, 1, 1, 0.0, &gp[irow-1].O, &anul, &istat) ||
fits_read_col_dbl(fp, co.H, irow, 1, 1, 0.0, &gp[irow-1].H, &anul, &istat) ||
fits_read_col_dbl(fp, co.X, irow, 1, 1, 0.0, &gp[irow-1].X, &anul, &istat) ||
				 0 ) {
				anl_msg_error("\
%s: fits_read_col() failed at irow=%ld\n", pname, irow);
				goto error;
			}
		}

	}

	t0 = xcp->gp[0].t;
	t1 = xcp->gp[ngp-1].t;

/* move to "XIS_CONTAMI_TRANS" */
	fits_movnam_hdu(fp, hdutype, extname_trans, extver, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_movnam_hdu('%s',%d) failed (%d)\n",
			pname, extname_trans, extver, istat);
		goto error;
	}
/* check "FORMAT_VERSION(n)" */
	if ( format_version < 0 ) {
		goto error;
	} else if ( 1 != format_version && 2 != format_version ) {
		anl_msg_error("\
%s: FORMAT_VERSION(%d) not supported\n", pname, format_version);
		goto error;
	}
/* get "NAXIS2" */
	fits_read_key(fp, TINT, "NAXIS2", &nen, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('NAXIS2') failed (%d)\n", pname, istat);
		goto error;
	} else if ( 0 == nen ) {
		anl_msg_error("\
%s: no rows in contami file '%s'\n", pname, xcp->contamifile);
		istat = -1;		/* no rows */
		goto error;
	}
/* get "CORATIO" */
	if ( 1 == format_version ) {
		fits_read_key_dbl(fp, "CORATIO", &xcp->coratio, NULL, &istat);
		if ( istat ) {
			anl_msg_warning("\
%s: WARNING: fits_read_key('CORATIO') failed (%d), CORATIO=6.0 assumed\n",
				pname, istat);
			xcp->coratio = 6.0;
			istat = 0;		/* ignore error */
		}
	} else {
		xcp->coratio = -1.0;
	}

/* allocate memory */
	xcp->nen = nen;
	xcp->index.nbody = nbody = nen - 1;
	if ( 1 == format_version ) {
		xcp->en = en = malloc( 2*sizeof(*en)*nen + sizeof(*body)*nen);
		xcp->vc = vc = en + nen;
		xcp->vo = xcp->vh = xcp->vx = vo = vh = vx = NULL;
		xcp->index.body = body = (int *)(vc + nen);
	} else {
		xcp->en = en = malloc( 5*sizeof(*en)*nen + sizeof(*body)*nen);
		xcp->vc = vc = en + nen;
		xcp->vo = vo = vc + nen;
		xcp->vh = vh = vo + nen;
		xcp->vx = vx = vh + nen;
		xcp->index.body = body = (int *)(vx + nen);
	}
	if ( NULL == en ) {
		anl_msg_error("\
%s: malloc() failed for CONTAMI_TRANS (nen=%d)\n", pname, nen);
		istat = -1;		/* malloc error */
		goto error;
	}

/* get column numbers */
	if ( 1 == format_version ) {

		if (
fits_get_colnum(fp, cs, key="ENERGY",   &co.E, &istat) ||
fits_get_colnum(fp, cs, key="TRANSMIS",	&co.T, &istat) ||
			 0 ) {
			anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, key, istat);
			goto error;
		}

	} else {

		if (
fits_get_colnum(fp, cs, key="ENERGY",   	&co.E, &istat) ||
fits_get_colnum(fp, cs, key="C_TRANSMIS",	&co.C, &istat) ||
fits_get_colnum(fp, cs, key="O_TRANSMIS",	&co.O, &istat) ||
fits_get_colnum(fp, cs, key="H_TRANSMIS",	&co.H, &istat) ||
fits_get_colnum(fp, cs, key="X_TRANSMIS",	&co.X, &istat) ||
			 0 ) {
			anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, key, istat);
			goto error;
		}

	}

/* read table contents */
	for (irow = 1; irow <= nen; irow++) {

		if ( 1 == format_version ) {

			if (
fits_read_col_dbl(fp, co.E, irow, 1, 1, 0.0, &en[irow-1], &anul, &istat) ||
fits_read_col_dbl(fp, co.T, irow, 1, 1, 0.0, &vc[irow-1], &anul, &istat) ||
				 0 ) {
				anl_msg_error("\
%s: fits_read_col() failed at irow=%ld\n", pname, irow);
				goto error;
			}

		} else {

			if (
fits_read_col_dbl(fp, co.E, irow, 1, 1, 0.0, &en[irow-1], &anul, &istat) ||
fits_read_col_dbl(fp, co.C, irow, 1, 1, 0.0, &vc[irow-1], &anul, &istat) ||
fits_read_col_dbl(fp, co.O, irow, 1, 1, 0.0, &vo[irow-1], &anul, &istat) ||
fits_read_col_dbl(fp, co.H, irow, 1, 1, 0.0, &vh[irow-1], &anul, &istat) ||
fits_read_col_dbl(fp, co.X, irow, 1, 1, 0.0, &vx[irow-1], &anul, &istat) ||
				 0 ) {
				anl_msg_error("\
%s: fits_read_col() failed at irow=%ld\n", pname, irow);
				goto error;
			}

		}
	}

/* close file */
	fits_close_file(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
		goto error;
	}

/* initialize index */
	en = xcp->en;
	xcp->index.offs = offs = en[0];
	xcp->index.norm = norm = en[nbody];
	for (i = j = 0; i < nbody; i++) {
		k = nbody * ( en[i] - offs ) / norm;
		while ( j <= k ) body[j++] = i;
	}
	while ( j < nbody + 1 ) body[j++] = i;

/* initialize cache */
	xcp->last_irow = -1;
	xcp->last_radius = 0.0;
	xcp->last_energy = 0.0;
	xcp->last_trans[0] = 0.0;
	xcp->last_trans[1] = 1.0;
	xcp->last_trans[2] = 1.0;
	xcp->last_trans[3] = 1.0;

/* set version = format_version */
	xcp->version = format_version;

/* print information message */
	anl_msg_info("\
   v%d, ngp=%d, t0=%.1f, t1=%.1f, nen=%d, e0=%.3f, e1=%.3f\n",
		format_version, ngp, t0, t1, nen, offs, norm);

	return 0;

 error:

	if ( NULL != en ) {
		free(en);
	}

	if ( NULL != gp ) {
		free(gp);
	}

	if ( NULL != fp ) {
		fits_close_file(fp, &istat2);
	}

	xcp->ngp = 0;
	xcp->gp = NULL;

	xcp->nen = 0;
	xcp->en = NULL;

	return istat;
}


/************************************************************************
int xis_contami_init()	: initialize XIS_CONTAMI information

Input:
	char *contamifile	: XIS contami FITS file name

Output:
	ORBIT **xcpp		: pointer to XIS_CONTAMI pointer after initialization

Return_Values:
	0					: success
	-1					: malloc() error
	others				: CFITSIO error
************************************************************************/
int
xis_contami_init(XIS_CONTAMI **xcpp, char *contamifile)
{
	XIS_CONTAMI *xcp;
	int istat, len_contamifile;

	len_contamifile = strlen(contamifile);

	xcp = malloc( sizeof(*xcp) + (len_contamifile+1) );
	if ( NULL == xcp ) {
		anl_msg_error("\
%s: malloc() failed for XIS_CONTAMI *xcp\n", pname);
		return -1;
	}

	xcp->contamifile = (char *)(xcp + 1);
	strcpy(xcp->contamifile, contamifile);

	istat = read_contami_file_v1or2(xcp);
	if ( istat ) {
		xis_contami_free(xcp);
		return istat;
	}

	*xcpp = xcp;
	return 0;
}


/************************************************************************
int xis_contami_free()	: free memory of XIS_CONTAMI

Input:
	XIS_CONTAMI *xcp	: XIS_CONTAMI pointer to free

Return_Values:
	0					: success
************************************************************************/
int
xis_contami_free(XIS_CONTAMI *xcp)
{
	if ( NULL == xcp ) {
		return -1;
	}

	if ( NULL != xcp->gp ) {
		free(xcp->gp);
	}

	if ( NULL != xcp->en ) {
		free(xcp->en);
	}

	free(xcp);

	return 0;
}


/************************************************************************
double xis_contami()	: get transmission of XIS contamination

Input:
	XIS_CONTAMI *xcp	: XIS_CONTAMI pointer used for the calculation
	double energy		: X-ray photon energy in keV
	double aetime		: Astro-E time when X-ray photon was arrived
	double detx, dety	: DETX, DETY coordinates of the X-ray incident

Output:
	double *carbon		: contamination in 1e18 cm-2 of carbon column density
	double *oxygen		: contamination in 1e18 cm-2 of oxygen column density

Return_Values:
	0.0 -- 1.0			: calculated transmission
	-1.0				: error in XIS_CONTAMI *xcp
************************************************************************/
double
xis_contami(XIS_CONTAMI *xcp,
	double energy, double aetime, double detx, double dety,
	double *carbon, double *oxygen)
{
	static double pixel2arcmin = 0.0002895 * 60.0;
	static double detxcen = 512.5;
	static double detycen = 512.5;

	int i, ngp;
	XIS_CONTAMI_GROWTH *gp;
	double trans[4], contami[4], contami_trans, dx, dy, dt, t, s, r, frac;
	double A, B, C, O, H, X;

	if ( NULL == xcp ) {
		anl_msg_error("\
%s: XIS_CONTAMI *xcp == NULL\n", pname);
		return -1;
	}

	if ( 1 != xcp->version && 2 != xcp->version ) {
		anl_msg_error("\
%s: unknown xcp->version = %d\n", pname, xcp->version);
		return -1;
	}

	ngp = xcp->ngp;
	if ( ngp <= 0 ) {
		anl_msg_error("\
%s: invalid xcp->ngp = %d\n", pname, ngp);
		return -1;
	}

	dx = detx - detxcen;
	dy = dety - detycen;
	r = sqrt(dx*dx + dy*dy) * pixel2arcmin;
	xcp->last_radius = r;

	gp = xcp->gp;
	if ( aetime <= gp[0].t ) {		/* no contamination */
		return 1.0;
	}

	if ( 1 == ngp ) {
		A = gp[0].A;
		B = gp[0].B;
		C = gp[0].C;
		O = gp[0].O;
		H = gp[0].H;
		X = gp[0].X;
	} else {

		i = xcp->last_irow;
		if ( gp[ngp-1].t <= aetime ) {
			i = ngp - 1;
		} else if ( 0 < i && i < ngp - 1 &&
				    gp[i-1].t <= aetime && aetime < gp[i].t ) {
			;
		} else {
			for (i = 1; i < ngp - 1; i++) {
				if ( aetime < gp[i].t ) {
					break;
				}
			}
		}

		xcp->last_irow = i;
		dt = gp[i].t - gp[i-1].t;
		t = (aetime - gp[i-1].t) / dt;
		s = (gp[i].t - aetime) / dt;
		A = s*gp[i-1].A + t*gp[i].A;
		B = s*gp[i-1].B + t*gp[i].B;
		C = s*gp[i-1].C + t*gp[i].C;
		O = s*gp[i-1].O + t*gp[i].O;
		H = s*gp[i-1].H + t*gp[i].H;
		X = s*gp[i-1].X + t*gp[i].X;

	}

	if ( C <= 0.0 ) {				/* no contamination */
		return 1.0;
	}

	frac = 1.0 / ( 1 + pow(r/A, B) );
	contami[0] = C * frac;
	contami[1] = O * frac;
	contami[2] = H * frac;
	contami[3] = X * frac;
	if ( xcp->last_energy == energy ) {
		trans[0] = xcp->last_trans[0];
		trans[1] = xcp->last_trans[1];
		trans[2] = xcp->last_trans[2];
		trans[3] = xcp->last_trans[3];
	} else {
		transmission_at(xcp, energy, trans);
		xcp->last_energy = energy;
		xcp->last_trans[0] = trans[0];
		xcp->last_trans[1] = trans[1];
		xcp->last_trans[2] = trans[2];
		xcp->last_trans[3] = trans[3];
	}
	contami_trans = pow(trans[0], contami[0]);
	if ( 1.0 != trans[1] && 0.0 != contami[1] ) {
		contami_trans *= pow(trans[1], contami[1]);
	}
	if ( 1.0 != trans[2] && 0.0 != contami[2] ) {
		contami_trans *= pow(trans[2], contami[2]);
	}
	if ( 1.0 != trans[3] && 0.0 != contami[3] ) {
		contami_trans *= pow(trans[3], contami[3]);
	}

	if ( NULL != carbon ) {
		*carbon = contami[0];
	}
	if ( NULL != oxygen ) {
		if ( 1 == xcp->version ) {
			*oxygen = contami[0] / xcp->coratio;
		} else {
			*oxygen = contami[1];
		}
	}

	return contami_trans;
}
