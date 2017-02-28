/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/sisrmg/rddsub.c,v 3.8 1996/07/15 13:27:50 miket Exp $   */
/*                   */

/*
 filename:	rddsub.c
 purpose:	RDD subroutines
 author:	Geoffrey B. Crew
 date:		July 1995
 modified:      Jeff Guerber, GSFC 664/HSTX, 1996/3/27.  _ABS() for VMS.
 */

extern void	rdd_qzero();
extern double	rdd_G(), rdd_P();

/*
 *  derived from:
 *
 *  resp_gaus2rdd_gaus.c           arasmus@space.mit.edu 24-Feb-95
 */

static float	sig,x1,f; /* these are necessary to communicate with: */
static float	peak_g(), xg(), g(), erfcc(), qtrap(), trapzd();

#define RDD_TINY	1.e-5
#define RDD_HUGE	1.e+30
#define RDD_OBS_WIN	40
#define RDD_MAX_ITS	30

#ifndef M_PI
/*
 *  Standalone debugging front end:
 */
# include <math.h>
# define M_IS2PI	(M_2_SQRTPI*M_SQRT2*0.25)	/* (2 pi)^-1/2  */
#endif /* M_PI */

/* MJT 15July96 -- define macro (vms, g77/linux and ??) */
#ifndef _ABS
#define _ABS(x) ((x) < 0 ? -(x) : (x))
#endif

#ifndef CFITSIO_VERSION_STRING
# include <stdio.h>
/* and for output: */
# define DUMPVALS
main(argc, argv)
	int	argc;
	char	**argv;
{
	void	rdd_qzero();
	float   s_now, xone, frac;
	float	qpeak, qmean;
	double	x;
/* */
	s_now = atof(argv[1]);
	xone = atof(argv[2]);
	frac = atof(argv[3]);
	rdd_qzero(s_now, xone, frac, &qpeak, &qmean);
	fprintf(stderr, "s_now = %g\n", s_now);
	fprintf(stderr, "xone = %g\n", xone);
	fprintf(stderr, "frac = %g\n", frac);
	fprintf(stderr, "qpeak = %g\n", qpeak);
	fprintf(stderr, "qmean = %g\n", qmean);
	for (x = -RDD_OBS_WIN; x < RDD_OBS_WIN; x += .25) {
		fprintf(stdout, "%lg no %lg %lg\n",
			x,
			rdd_G(x,0.0,(double)s_now),
			rdd_P(x,0.0,(double)s_now, (double)xone) );
	}
}
#endif

/*
 *  Two shots at a value of qzero.
 */
void
rdd_qzero(s_now, xone, frac, qpeak, qmean)
	float	s_now, xone, frac;
	float	*qpeak, *qmean;
{
	float	bias, nbias;

	sig = s_now;
	x1 = (xone < 1.0) ? 1.0 : xone;
	f = frac;
	*qpeak = -peak_g();	/* faintdfe ON ?? */
	nbias = f*x1;
	bias = 0;
	while (_ABS(nbias-bias) > RDD_TINY) {
#ifdef DUMPVALS
	    fprintf(stdout, "b\t%g\t%g\n", nbias, bias);
#endif
		bias = nbias;
		nbias=qtrap(xg, bias - RDD_OBS_WIN, bias + RDD_OBS_WIN);
	}
	*qmean = -nbias;	/* faintdfe OFF ? */
}

static float
xg(x)
	float	x;
{
	return(x*g(x));
}

#define DG(X,H)	((g(X+H/2)-g(X-H/2))/H)
/* dg/dx is increasing then decreasing : lo++++++++x-------hi */
static float
peak_g()
{
	float	lo, hi, ldg, hdg, x, dg;

	lo = 0;		/* lower bound */
	hi = f*x1;	/* upper bound */
	do { lo -= sig; } while ((ldg = DG(lo,.05))<0.0);
	do { hi += sig; } while ((hdg = DG(hi,.05))>0.0);

	while (ldg - hdg > RDD_TINY) {
#ifdef DUMPVALS
	    fprintf(stdout, "x\t%g\t%g\t%g\t%g\n", lo, ldg, hi, hdg);
#endif
		x = (lo+hi)/2.0;
		dg = DG(x,.05);
		if (dg > 0) { lo = x; ldg = dg; }
		else        { hi = x; hdg = dg; }
	}
	return(x);
}
#undef DG

/* CF rdd_1() */
static float
g(x)
	float x;
{
	float g, h;

	g=(1.0-f)/(sqrt(2*M_PI)*sig)*exp(-0.5*x*x/(sig*sig));

	h = erfcc(sqrt(0.5)*(sig/x1-x/sig));
	if (h > 0.) g += f*0.5/x1*exp(0.5*sig*sig/(x1*x1)-x/x1)*h;
#ifdef DUMPVALS
	fprintf(stdout, "%g %g\n", x, g);
#endif
	return(g);
}

/*
 *  The RDD G(q,q0,sig) function.
 */
double
rdd_G(q, q0, s)
	double	q, q0, s;
{
	q -= q0;
	q0 = q/s;
	return(M_IS2PI * exp(-0.5 * q0 * q0) / s);
}

/*
 *  To support the x->0 limit, since rdd_2 requires cancellation.
 *
 *  x == 2 * rdd_xone
 *  y == (1.0/M_SQRT2)( sigma / rdd_xone - (q-q0)/sigma )
 *  z == ( q - q0 ) / sigma
 */
static double
rdd_Plim(x, s, z)
	double	x, s, z;
{
	x  = exp(-0.5 * z * z) / (x * s);
	x *= (M_SQRT2 * M_IS2PI);
	s  = - 1.0 / (2.0 * s * s);
	z  = 1.0 + s * ( 1.0 + 3*s * ( 1.0 + 5*s * ( 1.0 + 7*s )));
	x *= z;
	return(x);
}

/*
 *  The RDD P(q,q0,sig,x1) function.
 */
double
rdd_P(q, q0, s, x)
	double	q, q0, s, x;
{
	if (x <= 0.0) return(0.0);

	q -= q0;
	q0 = q/s;			/* (q - q0) / sigma */

	q  = s/x;			/* sigma / x */
	x *= 2.0;

	s  = (q-q0)*(1.0/M_SQRT2);
	if (s > 8.0) return(rdd_Plim(x, s, q0));

	/* return(exp(q*(0.5*q-q0)) * erfcc((q-q0)*(1.0/M_SQRT2)) / x); */
	/* ... except that exp() might overflow, so ... */

	/* s  = erfcc((float)((q-q0)*(1.0/M_SQRT2))) / x; */
	s = erfc( (q-q0)*(1.0/M_SQRT2) ) / x;
	if (s == 0.0) return(0.0);

	/* since erfcc > 0 it is safe to compute exp(). */
	s *= exp(q*(0.5*q-q0));

	return(s);
}

/*
 *  Use library call if available.
 */
static float
erfcc(x)
	float x;
{
/*
	float t,z,ans;
 */
	return((float)erfc((double)x));
/*
	z=_ABS(x);
	t=1.0/(1.0+0.5*z);
	ans=t*exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196+t*(0.09678418+
		t*(-0.18628806+t*(0.27886807+t*(-1.13520398+t*(1.48851587+
		t*(-0.82215223+t*0.17087277)))))))));
	return((x >= 0.0) ? ans : 2.0-ans);
 */
}

static float
qtrap(func,a,b)
	float a,b;
	float (*func)();
{
	int j;
	float s,olds,trapzd();

	olds = -RDD_HUGE;
	for (j=1;j<=RDD_MAX_ITS;j++) {
		s=trapzd(func,a,b,j);
		if (_ABS(s-olds) < RDD_TINY*_ABS(olds)) return(s);
		olds=s;
	}
	fputs("Too many steps in routine QTRAP\n", stderr);
	return(s);
}


#define FUNC(x) ((*func)(x))

static float
trapzd(func,a,b,n)
	float a,b;
	float (*func)();	/* ANSI: float (*func)(float); */
	int n;
{
	float x,tnm,sum,del;
	static float s;
	static int it;
	int j;

	if (n == 1) {
		it=1;
		return (s=0.5*(b-a)*(FUNC(a)+FUNC(b)));
	} else {
		tnm=it;
		del=(b-a)/tnm;
		x=a+0.5*del;
		for (sum=0.0,j=1;j<=it;j++,x+=del) sum += FUNC(x);
		it *= 2;
		s=0.5*(s+(b-a)*sum/tnm);
		return s;
	}
}

#undef FUNC
