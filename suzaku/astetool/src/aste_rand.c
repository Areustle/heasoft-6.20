/* SimASCArand.c,v 1.1.1.1 1995/04/03 09:08:48 ishisaki Exp */
/*******************************************************************

	aste_rand.c:	Random number generator based on KEISAN-BUTSURI,
					R.Hayano & T.Takahashi, Kyoritsu-shuppan (1992)

	int aste_rndseed(void);
		Return random number seed. When not initialized, return 0.

	void aste_rndlcini(int irseed);
		Initialize random number seed for aste_irndlc().
		The seed number must be odd to obtain good results.

	void aste_irndlc(int np, int *iran);
		Simple random number generator,
		using linear congruential method (SENKEI-GOUDOU-HOU, in Japanese).
		This returns np-set of integers in the range of 0 - 0x7fffffff.
		Returned values are always odd, be carefull !!!
		It's period is about 2^30 = 1073741824.

	void aste_rndtsini(int irseed);
		Initialize random number seed for aste_drndts().

	double aste_drndts(void);
		62-bit precision random number generator, using Tausworthe method.
		This returns one floating point value in the range of 0 <= r < 1.0.
		It's period is about 2^250 \simeq 10^75.

	void aste_drndtsn(int np, double *dran);
		Return np-set of random numbers using aste_drndts().

	void aste_drndtsn_skip(int np);
		Skip random number generation by np (integer).

	void aste_drndtsn_skipd(double np);
		Skip random number generation by np (double precision floating point).

	double aste_drndtsn_gen(void);
		Return number of random numbers generated so far.

	double aste_drndtsg(void);
		Return gaussian distribution random number with mean=0.0, sigma=1.0,
		using 2-dim conversion method.


	1994-09-11	Y.ISHISAKI		First coded for SimASCA
	1998-04-16	Y.ISHISAKI		Ported to astetool
	1999-09-02	Y.ISHISAKI		add aste_rndseed()

************************************************************************/

#include <stdio.h>
#include <math.h>
#include "aste_rand.h"

static int iseed = 0;

/* return random number seed. when not initialized, return 0 */
int
aste_rndseed(void)
{
	return iseed;
}

void
aste_rndlcini(int irseed)
{
	iseed = irseed | 1;		/* must be odd */
}

void
aste_irndlc(int np, int *iran)
{
	int n;
	n = np;
	while ( 0 < n ) {
		iseed *= 48828125;		/* 5^11 */
		iseed &= 0x7fffffff;
		*iran = iseed;
		iran++;
		n--;
	}
}

#define IP	250
#define IQ	103
#define IRBIT	62
#define ILCBIT	20
#define DCONST	(1.0/4611686018427387904.0)

static int ipos;
static int irq[IP], inext[IP];
static union { unsigned long l; unsigned int i[2]; } ir[IP], ngen;

/* when irseed == 0, return current seed */
void
aste_rndtsini(int irseed)
{
	int i, j;
	int work[IP];
	for (i = 0; i < IP; i++) {
#ifdef __alpha
		ir[i].l = 0;
		ngen.l = 0;
#else
		ir[i].i[0] = ir[i].i[1] = 0;
		ngen.i[0] = ngen.i[1] = 0;
#endif
		irq[i] = ( i + IP - IQ ) % IP;
		inext[i] = i+1;
	}
	inext[IP-1] = 0;
	aste_rndlcini(irseed);
	for (i = 0; i < IRBIT; i++) {
		int ip = IP;
		aste_irndlc(ip, work);
		for (j = 0; j < IP; j++) {
			int ilc = work[j];
			ilc >>= ILCBIT - 1;
			ilc &= 1;
#ifdef __alpha
			ir[j].l <<= 1;
			ir[j].l |= ilc;
#else
			ir[j].i[1] <<= 1;
			if ( ir[j].i[0] & 0x80000000 ) ir[j].i[1]++;
			ir[j].i[0] <<= 1;
			ir[j].i[0] |= ilc;
#endif
		}
	}
	ipos = 0;
}

double
aste_drndts(void)
{
	double dran;
#ifdef __alpha
	dran = DCONST * ( ir[ipos].l ^= ir[irq[ipos]].l );
	ngen.l++;
#else
	dran = ( ir[ipos].i[0] ^= ir[irq[ipos]].i[0] );
	dran += 4294967296.0 * ( ir[ipos].i[1] ^= ir[irq[ipos]].i[1] );
	dran *= DCONST;
	ngen.i[0]++;
	if ( 0 == ngen.i[0] ) ngen.i[1]++;
#endif
	ipos = inext[ipos];
	return dran;
}

void
aste_drndtsn(int np, double *dran)
{
	int n;
	n = np;
#ifdef __alpha
	ngen.l += n;
#else
	{
		unsigned int  sav = ngen.i[0];
		ngen.i[0] += n;
		if ( ngen.i[0] < sav ) ngen.i[1]++;
	}
#endif
	while ( 0 < n ) {
#ifdef __alpha
		*dran = DCONST * ( ir[ipos].l ^= ir[irq[ipos]].l );
#else
		*dran = ( ir[ipos].i[0] ^= ir[irq[ipos]].i[0] );
		*dran += 4294967296.0 * ( ir[ipos].i[1] ^= ir[irq[ipos]].i[1] );
		*dran *= DCONST;
#endif
		ipos = inext[ipos];
		dran++;
		n--;
	}
}

#ifdef __alpha
#define skip1		(ir[ipos].l ^= ir[irq[ipos]].l, ipos = inext[ipos])
#else
#define skip1		(ir[ipos].i[0] ^= ir[irq[ipos]].i[0],\
					 ir[ipos].i[1] ^= ir[irq[ipos]].i[1],\
					 ipos = inext[ipos])
#endif

void
aste_drndtsn_skip(int np)
{
	int n;
	n = np;
#ifdef __alpha
	ngen.l += n;
#else
	{
		unsigned int  sav = ngen.i[0];
		ngen.i[0] += n;
		if ( ngen.i[0] < sav ) ngen.i[1]++;
	}
#endif
	while ( 32 <= n ) {
		skip1; skip1; skip1; skip1; skip1; skip1; skip1; skip1;
		skip1; skip1; skip1; skip1; skip1; skip1; skip1; skip1;
		skip1; skip1; skip1; skip1; skip1; skip1; skip1; skip1;
		skip1; skip1; skip1; skip1; skip1; skip1; skip1; skip1;
		n -= 32;
	}
	while ( 1 <= n ) {
		skip1;
		n--;
	}
}

void
aste_drndtsn_skipd(double np)
{
	double skip = np;
	while ( 1.0 <= skip ) {
		int n = 2147483647;
		if ( skip < 2147483647.0 ) n = (int)skip;
		skip -= (double)n;
		aste_drndtsn_skip(n);
	}
}

double
aste_drndtsn_gen(void)
{
#ifdef __alpha
	return (double)ngen.l;
#else
	return 4294967296.0 * ngen.i[1] + ngen.i[0];
#endif
}

double
aste_drndtsg(void)
{
	static int ncall = 0;
	static int n = 2;
	static double save;
	double fact, theta, ran[2];
	ncall++;
	if ( ncall & 1 ) {
		aste_drndtsn(n, ran);
		while ( 0.0 == ran[0] ) {
			ran[0] = aste_drndts();
		}
		fact = sqrt(-2*log(ran[0]));
		theta = 2 * M_PI * ran[1];
		save = fact * sin(theta);
		return fact * cos(theta);
	}
	return save;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
