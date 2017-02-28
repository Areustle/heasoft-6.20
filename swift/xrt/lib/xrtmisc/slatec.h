/*
 *	slatec.h: --- C-Wrappers to subset of FORTRAN SLATEC routines ---
 *
 *	DESCRIPTION:
 *      This header files provides macros to create C-wrapper functions for
 *      a subset of numerical routines from the public-domain FORTRAN library
 *      SLATEC. The general C-Fortran interface `cfortran.h' is used.
 *      For a description of the functionality and parameters of the various
 *      functions, see file `slatec.f'.
 *
 *	MACROS:
 *      SLATEC_DPCHIM --- compute derivatives of cubic Hermite interpolant
 *      SLATEC_DPCHFE --- evaluate cubic Hermite fun. using outout from DPCHIM
 *      SLATEC_DPOLFT --- least-square polynomial fit of discrete data
 *      SLATEX_DP1VLU --- evaluate polynomial computed by DPOLFT
 *
 *	AUTHOR:
 *      UL, Thu Feb 29 12:03:38 MET 1996
 */

#ifndef SLATEC_H
#define SLATEC_H

					/********************************/
                    /*        header files          */
                    /********************************/

#include "cfortran.h"

					/********************************/
					/*      defines / typedefs      */
					/********************************/

/*
 *	XERMSG: write error message to stdout
 */
#define SLATEC_XERMSG(S1, S2, S3, I1, I2)								\
	CCALLSFSUB5(XERMSG, xermsg,											\
				STRING, STRING, STRING, INT, INT,						\
				S1, S2, S3, I1, I2)

/*
 *	DPCHIM:	calculate derivatives needed to determine a monotone piecewise
 *          cubic Hermite interpolant to given data.
 *			Note: Original parameter INCFD (cmp. slatec.f) has been omitted
 *				  since we only need to do 1-dimensional interpolations
 */
#define SLATEC_DPCHIM(N, X, F, D, IERR)									\
    CCALLSFSUB6(DPCHIM, dpchim,                                         \
                INT, DOUBLEV, DOUBLEV, DOUBLEV, INT, PINT,				\
                N, X, F, D, 1, IERR)

/*
 *  DPCHFE: Evaluate a piecewise cubic Hermite function at an array of
 *          points using output from DPCHIM.
 *			Note: Original parameter INCFD (cmp. slatec.f) has been omitted
 *				  since we only need to do 1-dimensional interpolations
 */
#define SLATEC_DPCHFE(N, X, F, D, SKIP, NE, XE, FE, IERR)				\
    CCALLSFSUB10(DPCHFE, dpchfe,                                        \
                 INT, DOUBLEV, DOUBLEV, DOUBLEV, INT, PLOGICAL, INT,	\
                 DOUBLEV, DOUBLEV, PINT,                                \
                 N, X, F, D, 1, SKIP, NE, XE, FE, IERR)

/*
 *  DPOLFT: Fit discrete data in a least squares sense by polynomials
 *          in one variable.
 */
#define SLATEC_DPOLFT(N, X, Y, W, MAXDEG, NDEG, EPS, R, IERR, A)        \
    CCALLSFSUB10(DPOLFT, dpolft,                                        \
                 INT, DOUBLEV, DOUBLEV, DOUBLEV, INT, PINT, PDOUBLE,    \
                 DOUBLEV, PINT, DOUBLEV,                                \
                 N, X, Y, W, MAXDEG, NDEG, EPS, R, IERR, A)

/*
 *  DP1VLU: Use the coefficients generated by DPOLFT to evaluate the
 *          polynomial fit at a specified point
 */
#define SLATEC_DP1VLU(L, NDER, X, YFIT, YP, A)                          \
    CCALLSFSUB6(DP1VLU, dp1vlu,                                         \
                INT, INT, DOUBLE, PDOUBLE, DOUBLEV, DOUBLEV,            \
                L, NDER, X, YFIT, YP, A)

/*
 *	DNLS1E: An easy-to-use code which minimizes the sum of the squares
 *			of M nonlinear functions in N variables by a modification
 *			of the Levenberg-Marquardt algorithm.
 */
#define SLATEC_DNLS1E(FCN, IOPT, M, N, X, FVEC, TOL, NPRINT, INFO,		\
					  IW, WA, LWA)                                      \
    CCALLSFSUB12(DNLS1E, dnls1e,                                        \
				 ROUTINE, INT, INT, INT, DOUBLEV, DOUBLEV, DOUBLE,      \
				 INT, PINT, INTV, DOUBLEV, INT,                         \
                 FCN, IOPT, M, N, X, FVEC, TOL, NPRINT, INFO,           \
                 IW, WA, LWA)

/*
 *	DRAND: Random number generatr returning uniformally distributed number
 *		   in interval [0.0, 1.0]
 */
PROTOCCALLSFFUN1(DOUBLE, DRAND, drand, DOUBLE)
#define SLATEC_DRAND(R)													\
	CCALLSFFUN1(DRAND, drand, DOUBLE, R)

#endif