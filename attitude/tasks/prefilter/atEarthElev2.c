/*
  atEarthElev.c 	   return value : elevation in radian

  Originally coded                  by C.Otani   1993.2.12
  Modified to use for sisAdSet.c    by T.Dotani
  Modified to use for dp10DyeElv.c  by C.Otani
  Modified                          by C.Otani   1993.12.14
  Modified to atEarthElev           by Y.Ishisaki  94.03.24
*/

#include <stdio.h>
#include <math.h>
#include "atFunctions.h"

/*
 *  Replacement routine for duvmif derived from Numerical Recipes...
 *
 * From dotani@astro.isas.ac.jp Thu Nov 11 06:02:17 1993
 * Subject: numcal.f
 *
 * Dear Geoff,
 *
 * Otani-san forwarded your mail to me.  The subroutine DUVMIF called
 * in numcal.f is a library subroutine in IMSL.  IMSL is a fortran
 * subroutine library for mathmatical applications.  You need license to
 * use this library.  But the subroutine DUVMIF is a relatively simple
 * one and I think it is not difficult to code the subroutine by yourself.
 * Following is the description of the subroutine taken from the
 * manual of the IMSL.
 *
 * UVMIF / DUVMIF (single/double precision)
 * Purpose		Find the minimum point of a smooth function of a single
 * 		variable using only function evaluations.
 * Usage		CALL UVMIF(F,XGUESS,STEP,BOUND,XACC,MAXFN,X)
 * Arguments
 * 	F	User-supplied FUNCTION to compute the value of the
 * 		function to be minimized.  The form is
 * 		F(x), where
 * 		X	- The point at which the function is evaluated.
 * 			  X should not be changed by F.
 * 		F	- The computed function value at the point X.
 * 	XGUESS	An initial guess of the minimum point of F. (input)
 * 	STEP	An order of magnitude estimate of the required change
 * 		in X. (input)
 * 	BOUND	A positive number that limits the amount by which X may
 * 		be changed from its initial value. (input)
 * 	XACC	The required absolute accuracy in the final value of X.
 * 		On a normal return there are points on either side of X
 * 		within a distance XACC at which F is no less than F(X).
 * 		(input)
 * 	MAXFN	Maximum number of function evaluations allowed. (input)
 * 	X	The point at which a minimum value of F is found. (output)
 */

/*
 *  The original intent here was to use the routine mnbrak() which is
 *  supposed to try to bracket the absolute minimum, and then pass along
 *  that location to golden() which does a brute-force golden-section
 *  search for the local minimum.  However, mnbrak() apparently didn't
 *  do it's job too well, so we replaced that with a brute-force step
 *  through the bounded search region.
 *
 *	{gbc,arasmus,jww}@space.mit.edu
 */
/*
 *  Maybe the generality of this function is slightly lost, because this
 *  function is modified so as to be suitable for the calculation of
 *  elevation angle from the bright earth. If you use this function for
 *  the general purposes, be careful.
 *
 *  STEP    This value must be specified to have at most one peak within
 *          X +- STEP for all allowed X value ( | X - XGUESS | <= BOUND ).
 *
 *  1993.12.20     Modified by  C.Otani ( otani@astro.isas.ac.jp )
 *  2003.7.14      Modified by  B.Wiegand (bob.wiegand@nasa.gov)
 *      (the last step before guess + bound was sometimes ignored)
 */

/* Copyright (C) 1987,1988 Numerical Recipes Software -- GOLDEN */
/* Modified for DOUBLE PRECISION data types */

#define R 0.61803398874989484820
#define C (1.0-R)
#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);
#define SHFU(b,c,d) (b)=(c);(c)=(d);

static double
golden (double ax, double bx, double cx, double (*f)(double),
		double tol, double * xmin, int maxiter)
{
	double f1,f2,x0,x1,x2,x3;
	int	count;

	x0=ax;
	x3=cx;
	if (fabs(cx-bx) > fabs(bx-ax)) {
		x1=bx;
		x2=bx+C*(cx-bx);
	}
	else {
		x2=bx;
		x1=bx-C*(bx-ax);
	}
	f1=(*f)(x1);
	f2=(*f)(x2);

	count = 2;		/* impose max of f evals */

	while (fabs(x3 - x0) + fabs(f1 - f2) > tol) {
		if (f2 < f1) {
			SHFT(x0,x1,x2,R*x1+C*x3)
			SHFU(f1,f2,(*f)(x2))
		}
		else {
			SHFT(x3,x2,x1,R*x2+C*x0)
			SHFU(f2,f1,(*f)(x1))
		}
		if (++count > maxiter) {
			(void)fprintf(stdout, "Duvmif: too many evals\n");
			break;	/* here */
		}
	}
	if (f1 < f2) {
		*xmin=x1;
		return f1;
	} else {
		*xmin=x2;
		return f2;
	}
}
#undef C
#undef R
#undef SHFT

static int
duvmif2_(double (*f)(double), double xguess, int steps,
			double bound, double tol, double maxfn, double * out)
{
	int i;
	double x, delta, bestx, bestf;
	double a, b;

	maxfn -= steps + 1;

	x = xguess - bound;
	delta = 2 * bound / steps;
	bestx = x;
	bestf = (*f)(bestx);

	for (i = 0; i < steps; ++i) {
		double q;
		x += delta;
		q = (*f)(x);
		if (q < bestf) {
			bestx = x;
			bestf = q;
		}
	}

	if      ( bestx < xguess - bound + delta) {
		a = xguess - bound;
		b = xguess - bound + 2 * delta;
	}
	else if ( bestx > xguess + bound - delta) {
		a = xguess + bound - 2 * delta;
		b = xguess + bound;
	}
	else {
		a = bestx - delta;
		b = bestx + delta;
	}

	/* do golden section search  */
	golden(a, bestx, b, f, tol, out, maxfn);

	if ( fabs( xguess - *out ) > bound + 1e-10) {
		fprintf(stdout,"DUVMIV: MINIMUM BRACKETED OUTSIDE BOUNDS [%e]\n",
				fabs(xguess - *out) - bound);
	}

	return(0);
}

#if 0
C     fortran subroutine for "sisAdSet"  coded    by T.Dotani
C                           1993.12.20   modified by C.Otani (numcal2)
C
      SUBROUTINE numcal2( sat, sis, erad, dang)
      DOUBLE PRECISION sat(3), sis(3), erad, dang
      DOUBLE PRECISION xguess/0.0/, x/0.0/, bound/0.0/, step/0.0/, fact
      DOUBLE PRECISION f,  pie/3.1415926535897932385/
      DOUBLE PRECISION satw(3), sisw(3), eradw
      EXTERNAL f
      COMMON /fcom/satw,sisw,eradw

      DO 10 I=1,3
         satw(I) = sat(I)
         sisw(I) = sis(I)
 10   CONTINUE
      eradw = erad
C                                     Check input arguments
C      IF( ABS(sat(2)).GE.1.0E-6) THEN
C         WRITE(*,*) 'SAT IS NOT ON THE XZ-PLANE; call dotani'
C      ENDIF
C                    Calculate boundary
C      IF( ABS(sat(1)).LT.erad) THEN
C         WRITE(*,*) 'SAT(1) IS LESS THAN ERAD; call dotani'
C         WRITE(*,*) 'SAT(1)=',sat(1),' ERAD=',erad
C         RETURN
C      ENDIF
      bound = ACOS(erad/sat(1))
C      IF ( bound.LT.0 )   bound = -bound
      xguess = 0.0
      step = bound/100.0
      CALL DUVMIF2( f, xguess, step, bound, 1.0D-4, 100, x)
C                                             limit x within the bound
C      IF( x-xguess.GT.bound) THEN
C         x = xguess + bound
C      ELSE IF( x-xguess.LT. -bound) THEN
C         x = xguess - bound
C      ENDIF
C                                              estimate ang. distance
      fact = SQRT( sis(1)**2 + sis(2)**2 + sis(3)**2 )
      dang = ACOS(-f(x)/fact)
      RETURN
      END

C     << f >>
C     this function returns the inner product of sis and the direction
C     from the satellite to the boundary of the day earth, but the signe
C     is changed.
C     x : angle which define a point on Earth, (R*cos(x), R*sin(x), 0)
C
      DOUBLE PRECISION FUNCTION f(x)
      DOUBLE PRECISION sat(3), sis(3), erad, x
      DOUBLE PRECISION nvect(3), fact
      COMMON /fcom/sat,sis,erad
      nvect(1) = erad*COS(x) - sat(1)
      nvect(2) = erad*SIN(x)
      nvect(3) = -sat(3)
      fact = SQRT(nvect(1)**2 + nvect(2)**2 + nvect(3)**2)
      DO 10 I=1,3
         nvect(I) = nvect(I)/fact
 10   CONTINUE
      f = -(sis(1)*nvect(1) + sis(2)*nvect(2) + sis(3)*nvect(3))
      RETURN
      END
#endif

#if 0
static double
func(double *x)
{
	AtVect vect, nvect;
	double fact;
	vect[0] = eradw * cos(*x) - satw[0];
	vect[1] = eradw * sin(*x);
	vect[2] = -satw[2];
	atNormVect(vect, nvect);
	return - atScalProd(sisw, nvect);
}
#endif

static double funcA;	/* sat[0]* sis[0] + sat[2]*sis[2] */
static double funcB;	/* erad * sis[0] */
static double funcC;	/* erad * sis[1] */
static double funcD;	/* erad**2 + sat[0]**2 + sat[2]**2 */
static double funcE;	/* 2 * erad * sat[0] */

static double
fastfunc(double x)
{
	double cosx = cos(x);
	double sinx = sin(x);
	return (funcA - funcB*cosx - funcC*sinx) / sqrt(funcD - funcE*cosx);
}

static double
numcal2(AtVect sat, AtVect sis, double erad)
{
	int steps, maxfn;
	double xguess, bound, tol, x;
	funcA = sat[0]*sis[0] + sat[2]*sis[2];
	funcB = erad * sis[0];
	funcC = erad * sis[1];
	funcD = erad*erad + sat[0]*sat[0] + sat[2]*sat[2];
	funcE = 2 * erad * sat[0];
	bound = acos(erad/sat[0]);
	xguess = 0.0;
	steps = 40;
	tol = 1.0e-6;
	maxfn = steps + 50;
	x = 0;
	duvmif2_(&fastfunc, xguess, steps, bound, tol, maxfn, &x);
	return acos(-fastfunc(x)/atNorm(sis));
}

#define DYEDIRECT -70.0
#define CANNOTSEE 120.0

int
atEarthElev2(
	AtVect vSat,	/*  (in) sidereal   */
	AtVect nvTgt,	/*  (in) sidereal   */
	AtVect vSun,	/*  (in) sidereal   */
	int *earth_occult,	/* (out) earth occultation */
	double elevation[3]	/* (out) from earth, DE, NE */
)
{
	AtVect vEarth, vSat2, vTgt2, vD;
	AtRotMat RM;
	double elv, angEarth, dAng, xDist, cross, dot, satDist;
	double xyradius;
	double erad = EARTH_RADIUS;

	/*   elevation angle   */
	atInvVect(vSat, vEarth);
	satDist = atNorm(vEarth);
	angEarth = asin(erad/satDist);
	atAngDistance(nvTgt, vEarth, &xDist);
	elevation[0] = elv = xDist - angEarth;

	/*   coordinate transformation   */
	atSetRotMatZX(vSun, vSat, RM);
	atRotVect(RM, vSat, vSat2);
	atRotVect(RM, nvTgt, vTgt2);

	/* If xyradius <= eradius, the result is very simple. */
	xyradius = vSat2[0];       /*  always vSat2[1]==0.0 */
	if ( xyradius <= erad ) {
		if ( vSat2[2] <= 0.0  ) {
			/*  night earth */
			elevation[1] = CANNOTSEE*DEG2RAD;
			elevation[2] = elv;
		} else {
			/* bright earth */
			elevation[1] = elv;
			elevation[2] = CANNOTSEE*DEG2RAD;
		}
	} else if ( elv <= 0.0 ) {
		/* satellite is looking at th Earth */
		dot = atScalProd(vEarth, nvTgt);
		cross = vSat2[2];
		cross += vTgt2[2] * (dot - sqrt(erad*erad-satDist*satDist+dot*dot));
		if( 0.0 <= cross ) {
			/* looking at the bright earth */
			vSat2[2] = -vSat2[2];
			vTgt2[2] = -vTgt2[2];
			dAng = numcal2(vSat2, vTgt2, erad);
			elevation[1] = -dAng;
			elevation[2] = dAng;
		} else {
			/* looking at the dark earth */
			dAng = numcal2(vSat2, vTgt2, erad);
			elevation[1] = dAng;
			elevation[2] = -dAng;
		}
	} else {
		/* looking at the space */
		dot = fabs(atScalProd(vSat2,vTgt2));
		vD[0] = vSat2[0] + dot*vTgt2[0];
		vD[1] =            dot*vTgt2[1];
		vD[2] = vSat2[2] + dot*vTgt2[2];
		cross = ( vD[2] / atNorm(vD) ) * erad;
		if( cross >= 0.0 ) {
			/* near the bright earth */
			vSat2[2] = -vSat2[2];
			vTgt2[2] = -vTgt2[2];
			dAng = numcal2(vSat2, vTgt2, erad);
			elevation[1] = elv;
			elevation[2] = dAng;
		} else {
			/* near the dark earth */
			dAng = numcal2(vSat2, vTgt2, erad);
			elevation[1] = dAng;
			elevation[2] = elv;
		}
	}
	if ( 0 < elv ) {
		*earth_occult = 0;	/* sky */
	} else if ( 0 < elevation[1] ) {
		*earth_occult = 1;	/* night earth */
	} else {
		*earth_occult = 2;	/* bright earth */
	}
	return 0;
}
