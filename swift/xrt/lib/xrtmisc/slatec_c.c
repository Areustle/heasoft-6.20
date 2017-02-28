/*
 *	slatec.c: --- C-part of subset of SLATEC routines in SAXDAS ---
 *
 *	DESCRIPTION:
 *		This module contains a Fortran callable function 'D1MACH' which
 *		replaces the original SLATEC routines which the same name.
 *
 *	CHANGE HISTORY:
 *		UL, 04/03/96: 1.0.0: - first version
 *
 *	AUTHOR:
 *		UL, Mon Mar  4 11:47:52 MET 1996
 */

#define SLATEC_C
#define SLATEC_VERSION		1.0.0

					/********************************/
                    /*        header files          */
                    /********************************/

#include <float.h>
#include <math.h>

#include "cfortran.h"
#include "basic.h"
#include "slatec.h"

					/********************************/
					/*       external functions     */
					/********************************/

/*-----------------------------------------------------------------------------
 *	SYNOPSIS:
 *		double D1mach(int i);
 *
 *	LOGIC:
 *		derive required constants from <float.h>
 *
 *---------------------------------------------------------------------------*/
double D1mach(int i)
{
	static BOOL		FirstCall = TRUE;
	static double	DoubleConsts[5];

	if (!FirstCall)
		if (i >= 1 && i <= NUM_OF(DoubleConsts))
			return DoubleConsts[i-1];
		else
			SLATEC_XERMSG("SLATEC", "d1mach", "Index out of bounds", 1, 1);
	else {
		DoubleConsts[0] = DBL_MIN;		/* the smallest positive magnitude	*/ 
		DoubleConsts[1] = DBL_MAX;		/* the largest magnitude			*/
		DoubleConsts[2] = .5 * DBL_EPSILON;
										/* the smallest relative spacing	*/
		DoubleConsts[3] = DBL_EPSILON;	/* the largest relative spacing		*/
		DoubleConsts[4] = log10((double)FLT_RADIX);	/* log10(Base)			*/

		FirstCall = FALSE;

		return D1mach(i);
	}
} /* D1mach */

/*
 *	the Fotran-callable wrapper for the above function
 */
FCALLSCFUN1(DOUBLE, D1mach, D1MACH, d1mach, INT)





