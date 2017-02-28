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
 *  1998/01/16     Jeff Guerber, RSTX/GSFC.  Replaced fprintf with c_fcerr.
 */

#ifdef vms
#  define duvmif2 duvmif2_
#endif vms

#include <math.h>
#include <stdio.h>
#include "cfortran.h"
#include "cftools.h"

static double	golden();

int
duvmif2(f, xguess, step, bound, xacc, maxfn, x)
     double	(*f)(), *xguess, *step, *bound, *xacc, *x;
     int	*maxfn;
{
  double   a, b, tol, temp1, temp2, bestguess=0.0;

  temp2=10000.0;
  for (temp1=(*xguess)-(*bound);temp1<=(*xguess)+(*bound);temp1+=(*step)){
    if (temp2 > (*f)(&temp1)){
      temp2=(*f)(&temp1);
      bestguess=temp1;
    }
  }
  if      ( bestguess == (*xguess)-(*bound) ){
    a = (*xguess) - (*bound);
    b = (*xguess) - (*bound) + (*step);
  }
  else if ( bestguess > (*xguess)+(*bound)-(*step) ){
    a = (*xguess) + (*bound) - (*step)*2.0;
    b = (*xguess) + (*bound);
  }
  else {
    a = bestguess - (*step);
    b = bestguess + (*step);
  }

  /* do golden section search  */
  tol = *xacc / *step;
  golden(a, bestguess, b, f, tol, x, maxfn);
  if ( fabs( *xguess - *x ) > *bound ) {
    c_fcerr( "DUVMIV: MINIMUM BRACKETED OUTSIDE BOUNDS!!\n");
  }

  return(0);
}

FCALLSCSUB7(duvmif2,DUVMIF2,duvmif2,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,PINT,PDOUBLE)


/* Copyright (C) 1987,1988 Numerical Recipes Software -- GOLDEN */
/* Modified for DOUBLE PRECISION data types */

#define R 0.61803398874989484820
#define C (1.0-R)
#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);
#define SHFU(b,c,d) (b)=(c);(c)=(d);

static double
golden(ax,bx,cx,f,tol,xmin,max)
     double ax,bx,cx,tol,*xmin;
     double (*f)();
     int	*max;
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
  f1=(*f)(&x1);
  f2=(*f)(&x2);

  count = 2;		/* impose max of f evals */

  while (fabs(x3-x0) > tol*(fabs(x1)+fabs(x2))) {
    if (f2 < f1) {
      SHFT(x0,x1,x2,R*x1+C*x3)
      SHFU(f1,f2,(*f)(&x2))
    }
    else {
      SHFT(x3,x2,x1,R*x2+C*x0)
      SHFU(f2,f1,(*f)(&x1))
    }
    if (++count > *max) {
      c_fcerr( "Duvmif: too many evals\n");
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
