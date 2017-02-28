/* aste_time.c,v 1.3 1999/06/03 09:11:13 ishisaki Exp */
/************************************************************************
  aste_time_tt.c

    conversion between MJD-TT (Terrestrial Time) and Mission Time

  int aste_mjdrefi(void);
  double aste_mjdreff(void);
  double mjdtt2aste(double mjd_tt, int mjdrefi, double mjdreff);
  double aste2mjdtt(double mission_time, int mjdrefi, double mjdreff);

	2006/08/01	Y.ISHISAKI	version 1.80
		add aste_mjdrefi(), aste_mjdreff()
		add aste2mjdtt(), mjdtt2aste() functions, in fact they are generic

************************************************************************/
#include "aste_time.h"

/************************************************************************
int aste_mjdrefi()		: integer part of the MJD reference (2000.0 UT)

Return_Values:
	int mjdrefi			: MJDREFI for SUZAKU
************************************************************************/
int
aste_mjdrefi(void)
{
	return ASTE_MJDREFI;
}

/************************************************************************
int aste_mjdrefi()		: fractional part of the MJD reference (64.184 s)

Return_Values:
	double mjdreff		: MJDREFF for SUZAKU
************************************************************************/
double
aste_mjdreff(void)
{
	return ASTE_MJDREFF;
}

/************************************************************************
double mjdtt2aste()	: convert MJD-TT into mission time

Input:
	double mjd_tt		: Modified Julain Date (dy) in TT (Terrestrial Time)
	int    mjdrefi		: integer part of the MJD reference
	double mjdreff		: fractional part of the MJD reference

Return_Values:
	double mission_time	: mission time (s)
************************************************************************/
double
mjdtt2aste(double mjd_tt, int mjdrefi, double mjdreff)
{
	static double DAYSEC = 24 * 60 * 60;
	double mission_time;

	mission_time = (mjd_tt - mjdrefi) * DAYSEC;	/* must be calculated first */
	mission_time -= mjdreff * DAYSEC;

	return mission_time;
}

/************************************************************************
double aste2mjdtt()		: convert mission time int MJD-TT

Input:
	double mission_time	: mission time (s)
	int    mjdrefi		: integer part of the MJD reference
	double mjdreff		: fractional part of the MJD reference

Return_Values:
	double mjd_tt		: Modified Julain Date (dy) in TT (Terrestrial Time)
************************************************************************/
double
aste2mjdtt(double mission_time, int mjdrefi, double mjdreff)
{
	static double DAYSEC = 24 * 60 * 60;
	double mjd_tt;

	mjd_tt = mission_time / DAYSEC + mjdreff;	/* must be calculated first */
	mjd_tt += mjdrefi;

	return mjd_tt;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
