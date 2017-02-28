/* hrandomb.c,v 1.1.1.1 1995/04/03 09:08:48 ishisaki Exp */
/*******************************************************************
*
*	hrndmb.c: Random number following specified HBOOK histogram
*
*	double HrndmB(int hid);					// [1-dim historgram]
*		return random number following 1-dim HBOOK of id=hid
*
*	double HrndmB(int hid, &xpos, &ypos);	// [2-dim histogram]
*		return random number following 2-dim HBOOK of id=hid
*
*	void HrndmB_free(int hid);
*		explicitly free allocated memory
*
*
*	1994/09/11	Y.ISHISAKI
*		First coded for SimASCA
*
*	1998/04/16	Y.ISHISAKI
*		Ported from SimASCA-3.1 hrandomb.c
*
*	2002/11/20	Y.ISHISAKI
*		redefine HUNPAK for cernlib v2000
*
*	2003/07/28	Y.ISHISAKI
*		change SimASTE -> aste
*
*	2004/09/22	Y.ISHISAKI
*		static declaration of choice for HUNPAK() in HrndmB()
*
********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "cfortran.h"
#include "hbook.h"
#include "aste_rand.h"

#undef HUNPAK
#define HUNPAK(A1,A2,A3,A4)  \
	CCALLSFSUB4(HUNPAK,hunpak,INT,FLOATV,STRING,INT,A1,A2,A3,A4)

#define until(a)	while(!(a))
#define unless(a)	if(!(a))
#define null(p)		((p)==NULL)
#define elif		else if

static char pname[] = "HrndmB";

typedef struct {
	int hid, dim;
	void *hm;
} HMBUF;

static HMBUF*
get_hmbuf(int hid)
{
	static int nbuf = 0;
	static HMBUF *hmbuf;
	int i;
	for (i = 0; i < nbuf; i++) {
		if ( hid == hmbuf[i].hid ) return &hmbuf[i];
	}
	for (i = 0; i < nbuf; i++) {
		if ( 0 == hmbuf[i].hid ) return &hmbuf[i];
	}
	nbuf += 100;
	if ( nbuf ) {
		hmbuf = realloc(hmbuf, sizeof(*hmbuf)*nbuf);
	} else {
		hmbuf = malloc(sizeof(*hmbuf)*nbuf);
	}
	if ( NULL == hmbuf ) {
		nbuf = 0;
		return NULL;
	}
	while ( i < nbuf ) hmbuf[i++].hid = 0;
	return &hmbuf[nbuf-100];
}

double
HrndmB(int hid, ...)
{
	static char choice[] = "HIST";
	HMBUF *hmp;;

	hmp = get_hmbuf(hid);
	if ( null(hmp) ) {
		printf("HBrndm1: internal table full\n");
		return -9999.0;
	}
	if ( 0 == hmp->hid ) {
		char chtit[81];
		float *ear, *photar, *image;
		int nx, ny, nwt, loc;
		float xmi, xma, ymi, yma;

		chtit[0] = '\0';
		xmi = xma = ymi = yma = 0.0;
		HGIVE(hid, chtit, nx, xmi, xma, ny, ymi, yma, nwt, loc);
		if ( 0 == nx && 0 == ny ) {
			printf("%s: HID=%d is NULL histogram\n", pname, hid);
			return -9999.0;
		} elif ( 0 == ny ) {
			int i;

			ear = malloc(sizeof(*ear)*(2*nx+1));
			if ( null(ear) ) {
				printf("%s: out of memory\n", pname);
				return -9999.0;
			}
			for (i = 0; i < nx; i++) {
				ear[i] = 0.0;
				HIX(hid, i+1, ear[i]);
			}
			ear[i] = xma;
			photar = ear + nx + 1;
			photar[0] = 0.0;
			HUNPAK(hid, photar, choice, 0);
			hmp->hm = Hrndm1_init(nx, ear, photar, 256);
			if ( null(hmp->hm) ) {
				printf("%s: Error in Hrndm1_init\n", pname);
				return -9999.0;
			}
			free(ear);
			hmp->hid = hid;
			hmp->dim = 1;
		} else {
			image = malloc(sizeof(*image)*nx*ny);
			if ( null(image) ) {
				printf("%s: out of memory\n", pname);
				return -9999.0;
			}
			image[0] = 0.0;
			HUNPAK(hid, image, choice, 0);
			hmp->hm = Hrndm2_init(image, nx, xmi, xma, ny, ymi, yma);
			if ( null(hmp->hm) ) {
				printf("%s: Error in Hrndm2_init\n", pname);
				return -9999.0;
			}
			free(image);
			hmp->hid = hid;
			hmp->dim = 2;
		}
	}
	if ( 1 == hmp->dim ) {
		return Hrndm1(aste_drndts(), hmp->hm);
	} else {
		va_list ap;
		double pos[2];
		va_start(ap, hid);
		aste_drndtsn(hmp->dim, pos);
		Hrndm2(pos, hmp->hm);
		*va_arg(ap, double*) = pos[0];
		*va_arg(ap, double*) = pos[1];
		va_end(ap);
		return 0.0;
	}
}

void
HrndmB_free(int hid)
{
	HMBUF *hmp;;
	hmp = get_hmbuf(hid);
	if ( null(hmp) || 0 == hmp->hid ) return;
	if ( 1 == hmp->dim ) {
		Hrndm1_free(hmp->hm);
	} else {
		Hrndm2_free(hmp->hm);
	}
	hmp->hid = hmp->dim = 0;
	hmp->hm = NULL;
}
