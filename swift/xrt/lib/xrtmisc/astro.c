/*
 *	astro.c: --- collection of some basic astronomical routines ---
 *
 *	DESCRIPTION:
 *
 *	The module contains a collection of functions with an astronomical
 *	"flavor".
 *
 *	CHANGE HISTORY:
 *		26/03/02: 1.0.4: - First Working version from Uwe Lammer
 *
 *	AUTHOR:
 *		UL, Fri Jun  3 14:45:38 METDST 1994
 */

#define ASTRO_C
#define ASTRO_VERSION	1.0.4

						/********************************/
						/*        header files          */
						/********************************/

#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "astro.h"
#include "calculus.h"

						/********************************/
						/*      external functions      */
						/********************************/

/*
 *	SYNOPSIS:
 *		double Julday(const double day, const unsigned month,
 *					  const unsigned year)
 *
 *	DESCRIPTION:
 *
 *		see astro.h
 *
 *	DEPENDENCIES:
 *
 *		<none>
 */
double Julday(const double day, const unsigned month, const unsigned year)
{
	double	A, f, g;

	if (month < 3) {
		f = (double)(year-1);
		g = (double)(month+12);
	} else {
		f = (double)year;
		g = (double)month;
	}
	A = 2. - floor(f/100.) + floor(f/400.);
	return
		floor(365.25*f) + floor(30.6001*(g+1.)) + day + A + 1720994.5;
} /* Julday */



/*
 *	SYNOPSIS:
 *		double ModJulday(const double day, const unsigned month,
 *						 const unsigned year)
 *
 *	DESCRIPTION:
 *
 *		see astro.h
 *
 *	DEPENDENCIES:
 *
 *		- Julday
 */
double ModJulday(const double day, const unsigned month, const unsigned year)
{
	return Julday(day, month, year) - 2400000.5;
} /* ModJulday */



double ModJulday2(const struct tm Date, const double FracSecs)
{
	return ModJulday((double)Date.tm_mday +
					 (3600.*Date.tm_hour + 60.*Date.tm_min +
					 (double)Date.tm_sec + FracSecs) / 86400.,
					 Date.tm_mon + 1, Date.tm_year + 1900);
} /* ModJulday2 */



/*
 *	SYNOPSIS:
 *		void ModJulday2Inv(double MJD, struct tm *Date, double *FracSecs)
 *
 *	DESCRIPTION:
 *
 *		see astro.h 
 */
void ModJulday2Inv(double MJD, struct tm *Date, double *FracSecs)
{
	double			JD, MJD_frac, dum;
	unsigned long	IntTime, JD_int, A, B, C, D, E;

	MJD_frac = modf(MJD, &JD);
	JD_int = (unsigned long)(JD + 2400000.5 + .5 + .5);

	*FracSecs = modf(MJD_frac * 86400., &dum);
	IntTime = (unsigned long)dum;
	Date->tm_hour = IntTime / 3600;
	IntTime %= 3600;
	Date->tm_min = IntTime / 60;
	Date->tm_sec = IntTime % 60;

	if (JD_int < 2299161)
		A = JD_int;
	else {
		unsigned long x;

		x = (unsigned long)(((double)JD_int - 1867216.25)/36524.25);
		A = JD_int + 1 + x - x/4;
	}
	B = A + 1524;
	C = (unsigned long)(((double)B - 122.1)/365.25);
	D = (unsigned long)(365.25 * (double)C);
	E = (unsigned long)((double)(B - D)/30.6001);

	Date->tm_mday = B - D - (unsigned long)(30.6001 * E);
	Date->tm_mon = (E <= 13 ? E-1 : E-13) - 1;
	Date->tm_year = (Date->tm_mon >= 2 ? C-4716 : C-4715) - 1900;

	Date->tm_isdst = 0;	/* no daylight saving time */
} /* ModJulday2Inv */



void NormUTC(struct tm *UTC)
{
	double	FracSecs, MJD;

	MJD = ModJulday2(*UTC, .5); /* .5 to avoid rounding error problems */

	ModJulday2Inv(MJD, UTC, &FracSecs);
} /* NormUTC */



/*
 *	SYNOPSIS:
 *		void Aberration(const double Time, const double TrueRa,
 *						const double TrueDec, double *DeltaRa,
 *						double *DeltaDec, double *SunLong);
 *
 *	DESCRIPTION:
 *
 *	see astro.h
 *
 *	REFERENCE:
 *
 *	Adapted from FTOOLS routine library/ascasubs/aberration.f
 *	Original algorithm: Nautical almanac 1993, Secs. B + C
 */
void Aberration(const double Time, const double TrueRa,
				const double TrueDec, double *DeltaRa,
				double *DeltaDec, double *SunLong)
{
	#define	EPSILON			1.e-12
	#define	JD_ZERO			-2557.5
	#define COS_ECLIPTIC	.9174820620768960
	#define SIN_ECLIPTIC	.3977771559141213
	#define KCONST			(20.49552 / 3600.0 * DEG_TO_RAD)

	double	CosRa, SinRa, CosDec, SinDec, T, g, L, CosLong, SinLong;
 
	CosRa	= cos(TrueRa);
	SinRa	= sin(TrueRa);
	CosDec	= cos(TrueDec);
	SinDec	= sin(TrueDec);
      
	T		= Time/86400. + JD_ZERO;
	g		= M_TWO_PI + fmod(357.528 + 0.9856003 * T, 360.) * DEG_TO_RAD;
	L		= 360. + fmod(280.460 + 0.9856474 * T, 360.);

	*SunLong= fmod(L + 1.915*sin(g) + .020*sin(2.*g), 360.) * DEG_TO_RAD;

	CosLong	= cos(*SunLong);
	SinLong	= sin(*SunLong);

	if (fabs(CosDec) > EPSILON)
		*DeltaRa = -KCONST/CosDec * (SinLong * SinRa +
									 CosLong * CosRa * COS_ECLIPTIC);
	else
		*DeltaRa = 0.;

	*DeltaDec = KCONST * (-SinLong * CosRa * SinDec + CosLong *
					(COS_ECLIPTIC * SinRa * SinDec - SIN_ECLIPTIC * CosDec));
} /* Aberration */



char *AlphaStr(double alpha)
{
	static char s[12];
	double		Hours, Minutes, Secs, FracSecs;

	/*
	 *	normalize alpha: 0<= alpha < 360
	 */
	alpha = fmod(alpha, 360.);
	if (alpha < 0.)
		alpha += 360.;

	FracSecs	= 10. * modf(86400./360. * alpha, &Secs);
	Secs		= 3600. * modf(Secs/3600., &Hours);
	Secs		= 60. * modf(Secs/60., &Minutes);

	sprintf(s, "%02.0fh%02.0fm%02.0f.%1.0fs", Hours, Minutes, Secs, FracSecs);

	return s;
} /* AlphaStr */



char *DeltaStr(double delta)
{
	static char s[12];
	int			Secs;
	double		Degrees;

	Secs = (int)(3600. * fabs(modf(delta, &Degrees)) + .5);
	sprintf(s, "%+03.0fo%02d'%02d\"", Degrees, Secs / 60, Secs % 60);

	return s;
} /* DeltaStr */



double Alpha2DecimalDeg(char *AlphaStr, BOOL *ConvOK)
{
	int		Hours, Minutes;
	double	Seconds;

	if (!AlphaStr || !*AlphaStr) {
		*ConvOK = FALSE;

		return 0.;
	}
	*ConvOK = TRUE;

	if (!strpbrk(AlphaStr, "hms"))
		/*
		 *	is already in decimal degrees
		 */
		return atof(AlphaStr);

	if (sscanf(AlphaStr, "%d%*[h ]%d%*[m ]%lf", &Hours, &Minutes, &Seconds)
		!= 3) {
		*ConvOK = FALSE;
		return 0.;
	} else
		return 15. * Hours + .25 * Minutes + Seconds / 240.;
} /* Alpha2DecimalDeg */



double Delta2DecimalDeg(char *DeltaStr, BOOL *ConvOK)
{
	int		Degrees, ArcMins;
	double	ArcSecs;

	if (!DeltaStr || !*DeltaStr) {
		*ConvOK = FALSE;

		return 0.;
	}
	*ConvOK = TRUE;

	if (!strpbrk(DeltaStr, "o'\""))
		/*
		 *	is already in decimal degrees
		 */
		return atof(DeltaStr);

	if (sscanf(DeltaStr, "%d%*[o ]%d%*[' ]%lf", &Degrees, &ArcMins, &ArcSecs)
		!= 3) {
		*ConvOK = FALSE;
		return 0.;
	} else {
		double	Sign = (double)Degrees / fabs((double)Degrees);
		return Sign * (fabs((double)Degrees) + ArcMins/60. + ArcSecs/3600.);
	}
} /* Delta2DecimalDeg */
