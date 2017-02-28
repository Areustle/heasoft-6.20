/* $Id: hrndmrmf.c,v 1.8 2007/04/06 06:55:29 ishisaki Exp $ */
/*******************************************************************
*
*	hrndmrmf.c:	Random number following specified RMF
*
*	struct HrndmRMF *HrndmRMF_init(char *rmffile);
*		Initialize routine
*
*	void HrndmRMF_free(struct HrndmRMF *hmf);
*		Free allocated memory
*
*	int HrndmRMF(
*		double energy, double pos, struct HrndmRMF *hmf,	// input
*		double *pi, double *efficiency						// output
*	);
*		Specify energy & pos (usually, random number of 0 <= pos < 1),
*		Return PI (defined in 0 <= PI < hmf->detchans) & efficiency.
*
*
*	1998/03/01	Y.ISHISAKI
*		Extracted from GISRMFsim for SimASTE version 1998-03-27
*
*	1998/03/25 Ning Gan  GSFC
*		Change all the fc series to ff series
*			Take out cwfitsio.h and add fitsio.h
*			Use CASESEN and BINARY_TBL constant in fitsio.h
*
*	1999/05/13	Y.ISHISAKI
*		fprintf() bug fix & slight modification for IRIX
*
*	2003/09/14	Y.ISHISAKI
*		Read ebounds in HrndmRMF_init()
*		Tuning of PI using ebounds.e_min[0] in HrndmRMF()
*		Change type of body from short to int in struct HrndmRMFindex
*		Subtract offs from norm in HrndmRMF_init()
*		Add int ne, detchans; in struct HrndmRMF
*
*	2004/08/05	Y.ISHISAKI
*		check (rsp-mat != NULL) in HrndmRMF()
*
*	2006/08/22	Y.ISHISAKI	version 1.82
*		static declaration of read_rmf()
*		check malloc() & cfitsio error in read_rmf()
*		change float -> double for ENERG_LO, ENERG_HI in read_rmf()
*
*	2007/04/06	Y.ISHISAKI	version 1.85
*		fix hmf->index.norm in HrndmRMF_init()
*		[ no harm without slight speed loss in previous versions, if 0 < offs ]
*
********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include "fitsio.h"
#include "aste_rand.h"

static char pname[] = "HrndmRMF";

static void
rsp_free(float *mat, struct HrndmRMFrmf *rsp, int ie)
{
	int i;
	free(mat);
	ie--;
	for (i = 0; i < ie; i++) {
		if ( NULL != rsp[i].mat ) free(rsp[i].mat);
	}
	free(rsp);
}

static int
read_rmf(fitsfile *fp, double rsplimit, struct HrndmRMF *hmf)
{
	static char *ttype[] = {
		"ENERG_LO", "ENERG_HI", "N_GRP", "F_CHAN", "N_CHAN", "MATRIX"
	};
	union {
		int num[6];
		struct { int lo, hi, n_grp, f_chan, n_chan, mat; } s;
	} col;
	int i, ie,  tfields, anul, istat;
	long ne, detchans;
	int n_grp, f_chan[10], n_chan[10];
	int igrp, ichan, ipos, icol;
	double energ_lo, energ_hi;
	float *mat;
	struct HrndmRMFrmf *rsp, *rsq;

	istat = 0;
	hmf->rmf = NULL;
	tfields = sizeof(col)/sizeof(col.s.lo);
	ffgkyj(fp, "NAXIS2", &ne, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: 'NAXIS2' keyword not found\n", pname);
		return istat;
	}
	hmf->ne = ne;
	ffgkyj(fp, "DETCHANS", &detchans, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: 'DETCHANS' keyword not found\n", pname);
		return istat;
	}
	if ( detchans != hmf->detchans ) {
		fprintf(stderr, "\
%s: DETCHANS (%ld) != DETCHANS (%d) in EBOUNDS\n",
				pname, detchans, hmf->detchans);
		return -1;
	}
	mat = malloc(sizeof(*mat)*detchans);
	if ( NULL == mat ) {
		fprintf(stderr, "\
%s: malloc() failed in allocating mat[%ld]\n", pname, detchans);
		return -1;
	}
	rsp = malloc(sizeof(*rsp)*(ne+1));
	if ( NULL == rsp ) {
		fprintf(stderr, "\
%s: malloc() failed in allocating rsp[%ld]\n", pname, ne+1);
		free(mat);
		return -1;
	}
	for (i = 0; i <= ne; i++) rsp[i].mat = NULL;
	for (i = 0; i < tfields; i++) {
		ffgcno(fp, CASESEN, ttype[i], &col.num[i], &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: '%s' keyword not found\n", pname, ttype[i]);
			return istat;
		}
	}
	for (ie = 1; ie <= ne; ie++) {
		if (
ffgcvd(fp, icol=col.s.lo, ie, 1, 1, 0, &energ_lo, &anul, &istat) ||
ffgcvd(fp, icol=col.s.hi, ie, 1, 1, 0, &energ_hi, &anul, &istat) ||
ffgcvk(fp, icol=col.s.n_grp, ie, 1, 1, 0, &n_grp, &anul, &istat) ||
			 0 ) {
			goto read_error;
		}
		if ( !(n_grp < sizeof(f_chan)/sizeof(*f_chan)) ) {
			fprintf(stderr, "\
%s: too big N_GRP(%d) at line %d\n", pname, n_grp, ie);
			rsp_free(mat, rsp, ie);
			return -1;
		}
		if (
ffgcvk(fp, icol=col.s.f_chan, ie, 1, n_grp, 0, f_chan, &anul, &istat) ||
ffgcvk(fp, icol=col.s.n_chan, ie, 1, n_grp, 0, n_chan, &anul, &istat) ||
			 0 ) {
			goto read_error;
		}
		for (igrp = 0, ipos = detchans; igrp < n_grp; igrp++) {
			ipos -= n_chan[igrp];
		}
		if ( ipos < 0 ) {
			fprintf(stderr, "\
%s: too big N_CHAN at line %d\n", pname, ie);
			rsp_free(mat, rsp, ie);
			return -1;
		}
		if (
ffgcve(fp, icol=col.s.mat, ie, 1, detchans-ipos, 0, &mat[ipos], &anul, &istat)
			 ) {
			goto read_error;
		}
		for (igrp = ichan = 0; igrp < n_grp; igrp++) {
			while ( ichan + 1 < f_chan[igrp] ) mat[ichan++] = 0.0;
			if ( ichan == ipos ) {
				ichan += n_chan[igrp];
				ipos += n_chan[igrp];
			} else {
				while ( n_chan[igrp]-- ) mat[ichan++] = mat[ipos++];
			}
		}
		while ( ichan < detchans ) mat[ichan++] = 0.0;
		n_grp = 1;
		for (i = 0; i < detchans && mat[i] <= rsplimit; i++) ;
		if ( i < detchans ) {
			f_chan[0] = i + 1;
			for (i = detchans; f_chan[0] <= i && mat[i-1] <= rsplimit; i--) ;
			n_chan[0] = i + 1 - f_chan[0];
		} else {
			f_chan[0] = 1;
			n_chan[0] = 0;
		}
		rsq = &rsp[ie-1];
		rsq->e.lo = energ_lo;
		rsq->e.hi = energ_hi;
		rsq->f_chan = f_chan[0];
		rsq->n_chan = n_chan[0];
		if ( n_chan[0] ) {
			rsq->mat = malloc(sizeof(*rsq->mat)*n_chan[0]);
			if ( NULL == rsq->mat ) {
				fprintf(stderr, "\
%s: malloc() failed in allocating rsq->mat[%d]\n", pname, n_chan[0]);
				rsp_free(mat, rsp, ie);
				return -1;
			}
			memcpy(rsq->mat, mat+f_chan[0]-1, sizeof(*mat)*n_chan[0]);
		}
	}
	free(mat);
	hmf->rmf = rsp;
	return 0;

 read_error:
	fprintf(stderr, "\
%s: fits_read_col('%s') failed at irow=%d (%d)\n",
		pname, ttype[icol-1], ie, istat);
	rsp_free(mat, rsp, ie);
	return istat;
}

struct HrndmRMF *
HrndmRMF_init(char *rmffile)
{
	struct HrndmRMF *hmf;
	struct HrndmRMFrmf *rsp;
	struct HrndmRMFebounds ebounds;
	char extname[32];
	int hdutype, istat, istat2;
	long naxis2, detchans;
	int *body;
	int i, j, k, ne, nbody, anynul;
	double norm, offs;
	fitsfile* fp;
	struct { int ch, e_min, e_max; } col;

	istat = istat2 = 0;
	ffopen(&fp, rmffile, READONLY, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: RMF file '%s' open error\n", pname, rmffile);
		return NULL;
	}

	do {
		ffmrhd(fp, 1, &hdutype, &istat);
		if ( istat ) {
			ffclos(fp, &istat2);
			fprintf(stderr, "\
%s: EBOUNDS extension not found\n", pname);
			return NULL;
		}
		ffgkys(fp, "EXTNAME", extname, NULL, &istat);
	} while ( 0 != strcmp("EBOUNDS", extname) );
	ffgkyj(fp, "NAXIS2", &naxis2, NULL, &istat);
	if ( istat ) {
		ffclos(fp, &istat2);
		fprintf(stderr, "\
%s: NAXIS2 keyword not found in EBOUNDS\n", pname);
		return NULL;
	}
	ffgkyj(fp, "DETCHANS", &detchans, NULL, &istat);
	if ( istat ) {
		ffclos(fp, &istat2);
		fprintf(stderr, "\
%s: DETCHANS keyword not found in EBOUNDS\n", pname);
		return NULL;
	}
	if ( detchans != naxis2 ) {
		fprintf(stderr, "\
%s: DETCHANS (%ld) != NAXIS2 (%ld) in EBOUNDS\n",
				pname, detchans, naxis2);
		return NULL;
	}
	ffgcno(fp, CASESEN, "CHANNEL", &col.ch, &istat);
	if ( istat ) {
		ffclos(fp, &istat2);
		fprintf(stderr, "\
%s: CHANNEL column not found in EBOUNDS\n", pname);
		return NULL;
	}
	ffgcno(fp, CASESEN, "E_MIN", &col.e_min, &istat);
	if ( istat ) {
		ffclos(fp, &istat2);
		fprintf(stderr, "\
%s: E_MIN column not found in EBOUNDS\n", pname);
		return NULL;
	}
	ffgcno(fp, CASESEN, "E_MAX", &col.e_max, &istat);
	if ( istat ) {
		ffclos(fp, &istat2);
		fprintf(stderr, "\
%s: E_MAX column not found in EBOUNDS\n", pname);
		return NULL;
	}
	ebounds.e_min = malloc((  sizeof(*ebounds.e_min)
							+ sizeof(*ebounds.e_max)
							+ sizeof(ebounds.channel)) * naxis2);
	if ( NULL == ebounds.e_min ) {
		fprintf(stderr, "\
%s: malloc() failed in allocating ebounds.emin\n", pname);
		return NULL;
	}
	ebounds.e_max = ebounds.e_min + naxis2;
	ebounds.channel = (int *)(ebounds.e_max + naxis2);
	ffgcvk(fp, col.ch, 1, 1, naxis2, 0, ebounds.channel, &anynul, &istat);
	ffgcvd(fp, col.e_min, 1, 1, naxis2, 0, ebounds.e_min, &anynul, &istat);
	ffgcvd(fp, col.e_max, 1, 1, naxis2, 0, ebounds.e_max, &anynul, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: error in reading EBOUNDS\n", pname);
		free(ebounds.e_min);
		return NULL;
	}

	ffmahd(fp, 1, &hdutype, &istat);
	do {
		ffmrhd(fp, 1, &hdutype, &istat);
		if ( istat || BINARY_TBL != hdutype ) {
			ffclos(fp, &istat2);
			fprintf(stderr, "\
%s: [SPECRESP] MATRIX extension not found\n", pname);
			free(ebounds.e_min);
			return NULL;
		}
		ffgkys(fp, "EXTNAME", extname, NULL, &istat);
	} while (strcmp("MATRIX", extname) && strcmp("SPECRESP MATRIX", extname));

	ffgkyj(fp, "NAXIS2", &naxis2, NULL, &istat);
	if ( istat ) {
		ffclos(fp, &istat2);
		fprintf(stderr, "\
%s: NAXIS2 keyword not found\n", pname);
		free(ebounds.e_min);
		return NULL;
	}
	nbody = 2 * naxis2;
	if ( 0xffff < nbody ) {	/* larger than unsigned short */
		nbody = 0xffff;
	}
	hmf = malloc(sizeof(*hmf) + (nbody+1)*sizeof(*hmf->index.body));
	if ( NULL == hmf ) {
		fprintf(stderr, "\
%s: malloc() failed in allocating hmf\n", pname);
		free(ebounds.e_min);
		return NULL;
	}
	hmf->index.nbody = nbody;
	hmf->index.body = (int *)(hmf + 1);
	hmf->detchans = detchans;
	hmf->ebounds = ebounds;
	istat = read_rmf(fp, 0.0, hmf);
	ffclos(fp, &istat2);
	if ( istat || NULL == hmf->rmf ) {
		free(hmf);
		free(ebounds.e_min);
		return NULL;
	}

	for (i = 0, ne = hmf->ne, rsp = hmf->rmf; i < ne; i++, rsp++) {
		if ( NULL != rsp->mat ) {
			struct Hrndm1 *hm;
			hm = Hrndm1_init(rsp->n_chan, NULL, rsp->mat, 101);
			free(rsp->mat);
			rsp->mat = (float *)hm;
		}
	}

	rsp--;
	norm = rsp->e.hi;
	rsp = hmf->rmf;
	body = hmf->index.body;
	hmf->index.offs = offs = rsp->e.lo;
	hmf->index.norm = norm = norm - offs;
	for (i = j = 0; i < ne; i++) {
		k = nbody * ( rsp[i].e.hi - offs ) / norm;
		while ( j <= k ) body[j++] = i;
	}
	while ( j < nbody+1 ) body[j++] = i;

	return hmf;
}

int
HrndmRMF(double energy, double pos, struct HrndmRMF *hmf, double *pi, double *efficiency)
{
	int ipos, ie, ne;
	struct HrndmRMFrmf *rsp;
	struct Hrndm1 *hm0, *hm1;
	double en0, en1, ef0, ef1;

	if ( NULL == hmf ) return -1;
	rsp = hmf->rmf;
	if ( NULL == rsp ) return -1;

	*pi = *efficiency = 0.0;

	if ( pos < 0.0 || 1.0 <= pos ) return 0;
	ipos = hmf->index.nbody * ( energy - hmf->index.offs ) / hmf->index.norm;
	if ( ipos < 0 || hmf->index.nbody <= ipos ) return 0;
	ne = hmf->ne;

	for (ie = hmf->index.body[ipos], rsp = rsp + ie; ie < ne; ie++, rsp++) {
		if ( energy < rsp->e.hi ) break;
	}
	if ( ne <= ie || NULL == rsp->mat ) return 0;

	en0 = en1 = (rsp->e.lo + rsp->e.hi) / 2;
	hm0 = hm1 = (struct Hrndm1 *)rsp->mat;
	ef0 = ef1 = hm0->model[hm0->ne - 1];
	*pi = (rsp->f_chan - 1) + Hrndm1(pos, hm0);
	*pi *= (energy - hmf->ebounds.e_min[0]) / (en0 - hmf->ebounds.e_min[0]);

	if ( energy < en0 ) {
		rsp--;
		if ( hmf->rmf <= rsp && NULL != rsp->mat ) {
			en1 = (rsp->e.lo + rsp->e.hi) / 2;
			hm1 = (struct Hrndm1 *)rsp->mat;
			ef1 = hm1->model[hm1->ne - 1];
		}
	} else {
		rsp++;
		if ( NULL != rsp->mat ) {
			en1 = (rsp->e.lo + rsp->e.hi) / 2;
			hm1 = (struct Hrndm1 *)rsp->mat;
			ef1 = hm1->model[hm1->ne - 1];
		}
	}
	*efficiency = ((en0 - energy)*ef1 + (energy - en1)*ef0) / (en0 - en1);

	return 0;
}

void
HrndmRMF_free(struct HrndmRMF *hmf)
{
	int i;

	if ( NULL == hmf ) return;
	for (i = 0; NULL != hmf->rmf[i].mat; i++) {
		Hrndm1_free((struct Hrndm1 *)hmf->rmf[i].mat);
	}
	free(hmf->ebounds.e_min);
	free(hmf);
}
