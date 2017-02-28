/*
 *	astro.h: --- collection of some basic astronomical routines ---
 *
 *	DESCRIPTION:
 *
 *	The module contains an (uncomplete) collection of functions with an
 *	astronomical "flavor".
 *
 *	EXPORTED FUNCTIONS
 *
 *		Julday		--- compute modified Julian date
 *
 *	AUTHOR:
 *		UL, Fri Jun  3 14:45:38 METDST 1994
 */

#ifndef ASTRO_H
#define ASTRO_H

						/********************************/
						/*           includes           */
						/********************************/

#include <time.h>
#include "basic.h"

						/********************************/
						/*      function prototypes     */
						/********************************/

/*
 *	SYNOPSIS:
 *		double Julday(const double day, const unsigned month,
 *					  const unsigned year)
 *
 *	DESCRIPTION:
 *		Find Julian date at 0 hours UT using standard algorithm.
 *
 *	PARAMETERS:
 *		 I	:	day		: the day of the month (1-31)
 *				month	: the month of the year (1-12)
 *				year	: the year after BC
 *	RETURNS:
 *		The Julian date expressed as a double.
 *
 *	DOCUMENTATION:
 *		Fundamental Astronomy, H. Karttunen et al. (Edt), Springer Verlag 1987
 *		pp. 39
 */
extern double Julday		(const double day,
							 const unsigned month,
							 const unsigned year);

/*
 *	SYNOPSIS:
 *		double ModJulday(const double day, const unsigned month,
 *						 const unsigned year)
 *
 *	DESCRIPTION:
 *		Find Modified Julian date at 0 hours UT using standard algorithm.
 *
 *	PARAMETERS:
 *	 I	:	day		: the day of the month (1-31)
 *			month	: the month of the year (1-12)
 *			year	: the year after BC
 *
 *	RETURNS:
 *		The modified Julian date expressed as a double.
 */
extern double ModJulday		(const double day,
							 const unsigned month,
							 const unsigned year);



/*
 *	SYNOPSIS:
 *		double ModJulday2(const struct tm Date, const double FracSecs)
 *
 *	DESCRIPTION:
 *		Find modified Julian date (MJD) of certain point in time;
 *		
 *	PARAMETERS:
 *	 I	:	Date				: time structure containing date for which to
 *								  calculate MJD
 *			FracSecs			: fractional part of UTC seconds field of
 *								  <Date>
 *
 *	RETURNS:
 *		MJD corresponding to <Date>
 */
extern double ModJulday2(const struct tm Date,
						 const double FracSecs);



/*
 *	SYNOPSIS:
 *		void ModJulday2Inv(double MJD, struct tm *Date, double *FracSecs)
 *
 *	DESCRIPTION:
 *		Convert MJD value to day/month/year hour:min:sec.ddd format
 *		
 *	PARAMETERS:
 *	 I	:	MJD		: the MJD number
 *	 O	:	Date	: pointer to <struct tm> to be filled with the computed
 *					  values
 *			FracSecs: fractional part of Date.tm_sec field
 *
 *	RETURNS:
 *	<none>
 */
extern void ModJulday2Inv(double MJD,
						  struct tm *Date,
						  double *FracSecs);

extern void NormUTC(struct tm *UTC);



/*
 *	SYNOPSIS:
 *		void Aberration(const double Time,
 *						const double TrueRa, const double TrueDec,
 *						double *DeltaRa, double *DeltaDec,
 *						double *SunLong);
 *
 *	DESCRIPTION:
 *
 *	Calculate annual aberration correction. Adapted from FTOOLS routine
 *	library/ascasubs/aberration.f
 *
 *	PARAMETERS:
 *
 *	 I	:	Time		: time tag - seconds since mission reference time
 *			TrueRa		: geocentric right acension in radians
 *			TrueDec		: geocentric declination in radians
 *
 *	 O	:	*DeltaRa	: calculated correction to <TrueRa> in radians:
 *								*DeltaRa = ApparentRa - TrueRa
 *			*DeltaDec	: calculated correction to <TrueDec> in radians:
 *								*DeltaDec = ApparentDec - TrueDec
 *			*SunLong	: calculated solar longitude in radians
 *
 *	 I/O:	<none>
 *
 *	RETURNS:
 *	<none>
 */
extern void Aberration(const double Time,
					   const double TrueRa,
					   const double TrueDec,
					   double *DeltaRa,
					   double *DeltaDec,
					   double *SunLong);

extern char *AlphaStr (double alpha);
extern char *DeltaStr (double delta);
extern double Alpha2DecimalDeg(char *AlphaStr, BOOL *ConvOK);
extern double Delta2DecimalDeg(char *DeltaStr, BOOL *ConvOK);


#endif
