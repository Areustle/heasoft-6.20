/* hrandom.c,v 1.1.1.1 1995/04/03 09:08:48 ishisaki Exp */
/*******************************************************************
*
*	hrndm.c: Random number following specified distribution
*
*	1994/09/11	Y.ISHISAKI
*		First coded for SimASCA
*
*	1998/04/16	Y.ISHISAKI
*		Ported from SimASCA-3.1 hrandom.c
*
*	2003/09/14	Y.ISHISAKI
*		Change type of index from short to unsigned short in struct Hrndm1
*		Check the range of ne in Hrndm1_init()
*
********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "aste_rand.h"

#define unless(a)	if(!(a))
#define until(a)	while(!(a))
#define null(p)		((p)==NULL)

struct Hrndm1 *
Hrndm1_init(int ne, float *ear/*[ne+1]*/, float *photar/*[ne]*/, int ni)
{
	int i, j, k;
	double norm;
	struct Hrndm1 *hm;

	if ( 0xffff < ne ) {	/* larger than unsigned short */
		return NULL;
	}

	if ( null(ear) ) {
		hm = malloc( sizeof(*hm) + sizeof(*hm->model)*(ne-1) );
	} else {
		hm = malloc( sizeof(*hm) + sizeof(*hm->model)*2*ne );
	}
	if ( null(hm) ) return NULL;

	hm->ne = ne;
	hm->ni = 0;
	hm->index = NULL;

	if ( null(ear) ) {
		hm->ear = NULL;
		hm->model[0] = ( 0.0 < photar[0] ) ? photar[0] : 0.0;
		for (i = 1; i < ne; i++) {
			double dm = ( 0.0 < photar[i] ) ? photar[i] : 0.0;
			hm->model[i] = hm->model[i-1] + dm;
		}
	} else {
		hm->ear = &hm->model[ne];
		for (i = 0; i <= ne; i++) {
			hm->ear[i] = ear[i];
		}
		hm->model[0] = ( 0.0 < photar[0] ) ? (ear[1]-ear[0])*photar[0] : 0.0;
		for (i = 1; i < ne; i++) {
			double dm = ( 0.0 < photar[i] ) ? photar[i] : 0.0;
			hm->model[i] = hm->model[i-1] + (ear[i+1]-ear[i]) * dm;
		}
	}

	norm = hm->model[ne-1];
	unless ( 0.0 < norm ) {
		free(hm);
		return NULL;
	}

	if ( 2 < ni ) {
		hm->index = malloc(sizeof(*hm->index) * ni);
		unless ( null(hm->index) ) {
			for (i = j = 0; i < ne-1; i++) {
				k = (ni-1) * hm->model[i] / norm;
				while ( j <= k ) hm->index[j++] = i;
			}
			while ( j < ni ) hm->index[j++] = i;
			hm->ni = ni;
		}
	}

	return hm;
}

void
Hrndm1_free(struct Hrndm1 *hm)
{
	unless ( null(hm) ) {
		unless ( null(hm->index) ) free(hm->index);
		free(hm);
	}
}

double
Hrndm1(double pos, struct Hrndm1 *hm)
{
	double m0, m1, e0, e1;
	int ne, ibin, imid, imax;

	ne = hm->ne;
	if ( null(hm->index) ) {
		if ( 1 <= pos ) {
			hm->ipos = ne-1;
			return null(hm->ear) ? ne : hm->ear[ne];
		}
		ibin = 0;
		imax = ne - 1;
	} else {
		int nidx, ipos;
		nidx = hm->ni - 1;
		ipos = nidx * pos;
		if ( nidx <= ipos ) {
			hm->ipos = ne-1;
			return null(hm->ear) ? ne : hm->ear[ne];
		}
		ibin = hm->index[ipos];
		imax = hm->index[ipos+1];
	}
	pos *= hm->model[ne-1];
#if 0
	m0 = ibin ? hm->model[ibin-1] : 0.0;
	m1 = hm->model[imax];
	unless ( m0 <= pos && pos < m1 ) {
		printf("m(%d-1)=%lf, pos=%lf, m(%d)=%lf\n", ibin, m0, pos, imax, m1);
		printf("error !!!!!!!!");
		exit(1);
	}
#endif
	until ( ibin == imax ) {
		imid = ( ibin + imax ) / 2;
		if ( pos < hm->model[imid] ) {
			imax = imid;
		} else {
			ibin = imid + 1;
		}
	}
	if ( null(hm->ear) ) {
		e0 = ibin;
		e1 = ibin + 1;
	} else {
		e0 = hm->ear[ibin];
		e1 = hm->ear[ibin+1];
	}
	m0 = ibin ? hm->model[ibin-1] : 0.0;
	m1 = hm->model[ibin];
#if 0
	unless ( m0 <= pos && pos < m1 ) {
		printf("m0=%lf, pos=%lf, m1=%lf\n", m0, pos, m1);
		printf("error !!!!!!!!");
		exit(1);
	}
#endif
	hm->ipos = ibin;
	return e0 + ( e1 - e0 ) * ( pos - m0 ) / ( m1 - m0 );
}

struct Hrndm2 *
Hrndm2_init(float *image/*[nx*ny]*/, int nx, double xmi, double xma, int ny, double ymi, double yma)
{
	int ix, iy;
	float *xsum;
	double sum, thres;
	struct Hrndm2 *hm2;

	xsum = malloc(sizeof(*xsum) * ny);
	if ( null(xsum) ) goto err0;
	hm2 = malloc( sizeof(*hm2) + sizeof(hm2->xhm)*(ny-1) );
	if ( null(hm2) ) goto err1;

	hm2->nx = nx;
	hm2->ny = ny;
	hm2->xmi = xmi;
	hm2->xma = xma;
	hm2->ymi = ymi;
	hm2->yma = yma;

	sum = 0;
	for (iy = 0; iy < ny; iy++) {
		double s = 0.0;
		float *p = image + iy*nx;
		for (ix = 0; ix < nx; ix++) {
			if ( 0.0 < *p ) s += *p;
			p++;
		}
		sum += ( xsum[iy] = s );
	}
	hm2->yhm = Hrndm1_init(ny, NULL, xsum, 1001);
	if ( null(hm2->yhm) ) goto err2;

	thres = sum / ny;
	for (iy = 0; iy < ny; iy++) {
		struct Hrndm1 *hm = NULL;
		if ( 0.0 < xsum[iy] ) {
			int nidx = ( xsum[iy] < thres ) ? 0 : nx;
			hm = Hrndm1_init(nx, NULL, &image[iy*nx], nidx);
			if ( null(hm) ) {
				while ( iy ) {
					iy--;
					Hrndm1_free(hm2->xhm[iy]);
				}
				Hrndm1_free(hm2->yhm);
 err2:			free(hm2);
 err1:			free(xsum);
 err0:			return NULL;
			}
		}
		hm2->xhm[iy] = hm;
	}

	return hm2;
}

void
Hrndm2_free(struct Hrndm2 *hm2)
{
	int iy;
	unless ( null(hm2) ) {
		int ny = hm2->ny;
		Hrndm1_free(hm2->yhm);
		for (iy = 0; iy < ny; iy++) Hrndm1_free(hm2->xhm[iy]);
		free(hm2);
	}
}

void
Hrndm2(double pos[2], struct Hrndm2 *hm2)
{
	pos[1] = Hrndm1(pos[1], hm2->yhm);
	pos[0] = Hrndm1(pos[0], hm2->xhm[hm2->yhm->ipos]);
	pos[0] = hm2->xmi + pos[0] * ( hm2->xma - hm2->xmi ) / hm2->nx;
	pos[1] = hm2->ymi + pos[1] * ( hm2->yma - hm2->ymi ) / hm2->ny;
}
