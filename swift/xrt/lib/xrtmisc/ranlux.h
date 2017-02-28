/*
 *	ranlux.h: --- C-Wrappers to random number generator in ranlux.f ---
 *
 *	DESCRIPTION:
 *      This header files provides macros to create C-wrapper functions for
 *      the Fortran routines in ranluf.f The general C-Fortran interface
 *		`cfortran.h' is used. For a description of the functionality and
 *		parameters of the various functions, see file `ranlux.f'.
 *
 *	MACROS:
 *		RANLUX_RLUX   --- returns a vector of random numbers in range (0,1)
 *      RANLUX_RLUXGO --- initialization of RLUX
 *
 *	AUTHOR:
 *    UL, Fri Sep  6 16:41:12 METDST 1996
 */

#ifndef RANLUX_H
#define RANLUX_H

					/********************************/
                    /*        header files          */
                    /********************************/

#include "cfortran.h"

					/********************************/
					/*      defines / typedefs      */
					/********************************/

/*
 *	RLUX: returns a vector RVEC of LEN 32-bit random floating point numbers
 *		  between zero (not included) and one (also not incl.)
 */
#define RANLUX_RLUX(RVEC, LEN)											\
	CCALLSFSUB2(RANLUX, ranlux, FLOATV, INT, RVEC, LEN)

/*
 *	RLUXGO: initializes the generator from one 32-bit integer INT and sets
 *			Luxury Level
 */
#define RANLUX_RLUXGO(LUX, SEED)										\
	CCALLSFSUB4(RLUXGO, rluxgo, INT, INT, INT, INT, LUX, SEED, 0, 0)

#endif

