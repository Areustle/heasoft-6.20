/* aste_time.c,v 1.3 1999/06/03 09:11:13 ishisaki Exp */
/************************************************************************
  aste_time.c
    convert AtTimeD structure into mission time (Astro-E2|ASCA time)
    [AtTimeD とミッションタイム (Astro-E time) を変換するルーチン]

	This routine uses atMissionTimeInit(), atMJDToMission(), atMissionToMJD()
	in atFunctions-2.2 or after. MJD_BASE = 48988.0 (1993/01/01 00:00:00 0.0)
	for ASCA, and MJD_BASE = 51544.0 (2000/01/01 00:00:00 0.0) for Astro-E2.

  char *mission_time_init(char *leaptable);
	Initialize leapsec table reading file leaptable[].
	Return current leaptable file on returning.
	Just return leaptable file name if leaptable == NULL.

  double mjd2aste(double mjd);		// mjd -> astetime
  double aste2mjd(double astetime);	// astetime -> mjd
  double mjd2asca(double mjd);		// mjd -> ascatime
  double asca2mjd(double ascatime);	// ascatime -> mjd

  double attimeD2aste(AtTimeD *attime);	// AtTimeD -> astetime
  int aste2attimeD(double astetime, AtTimeD *attime);	// astetime -> AtTimeD
  double attimeD2asca(AtTimeD *attime);	// AtTimeD -> ascatime
  int asca2attimeD(double ascatime, AtTimeD *attime);	// ascatime -> AtTimeD

  double attime2aste(AtTime attime);		// AtTime -> astetime (obsolete)
  int aste2attime(double astetime, AtTime *attime);	// -> AtTime (obsolete)
  double attime2asca(AtTime attime);		// AtTime -> ascatime (obsolete)
  int asca2attime(double ascatime, AtTime *attime);	// -> AtTime (obsolete)

	2004/03/10	Y.ISHISAKI	version 1.26
		now, main code are moved to atFunctions-2.2

************************************************************************

 astroe_time の定義: 2000/01/01 00:00:00 (UT) (以下、基準点) からの経過秒数。
                     但し、基準点以前の閏秒は考慮しない。

************************************************************************/
#include "atFunctions.h"
#include "aste_time.h"

/* static AtTime asca_attime0 = {1993, 1, 1, 0, 0, 0, 0.0}; */
static double asca_mjd0 = ASCA_MJD_BASE;

/* static AtTime aste_attime0 = {2000, 1, 1, 0, 0, 0, 0.0}; */
static double aste_mjd0 = ASTE_MJD_BASE;

char *
mission_time_init(char *leaptable)
{
	char *fn;
	int verbose = 1;
	fn = atMissionTimeInit(leaptable, verbose);
	return fn;
}

double
mjd2aste(double mjd)
{
	double astetime;
	atMJDToMission(aste_mjd0, mjd, &astetime);
	return astetime;
}

double
aste2mjd(double astetime)
{
	double mjd;
	atMissionToMJD(aste_mjd0, astetime, &mjd);
	return mjd;
}

double
attimeD2aste(AtTimeD *attime)
{
	double astetime;
	atAtTimeDToMission(aste_mjd0, attime, &astetime);
	return astetime;
}

int
aste2attimeD(double astetime, AtTimeD *attime)
{
	int step;
	step = atMissionToAtTimeD(aste_mjd0, astetime, attime);
	return step;
}

double
attime2aste(AtTime attime)
{
	AtTimeD attimeD;
	double astetime;
	atAtTimeToAtTimeD(&attime, &attimeD);
	atAtTimeDToMission(aste_mjd0, &attimeD, &astetime);
	return astetime;
}

int
aste2attime(double astetime, AtTime *attime)
{
	int step;
	AtTimeD attimeD;
	step = atMissionToAtTimeD(aste_mjd0, astetime, &attimeD);
	atAtTimeDToAtTime(&attimeD, attime);
	return step;
}


double
mjd2asca(double mjd)
{
	double ascatime;
	atMJDToMission(asca_mjd0, mjd, &ascatime);
	return ascatime;
}

double
asca2mjd(double ascatime)
{
	double mjd;
	atMissionToMJD(asca_mjd0, ascatime, &mjd);
	return mjd;
}

double
attimeD2asca(AtTimeD *attime)
{
	double ascatime;
	atAtTimeDToMission(asca_mjd0, attime, &ascatime);
	return ascatime;
}

int
asca2attimeD(double ascatime, AtTimeD *attime)
{
	int step;
	step = atMissionToAtTimeD(asca_mjd0, ascatime, attime);
	return step;
}

double
attime2asca(AtTime attime)
{
	AtTimeD attimeD;
	double ascatime;
	atAtTimeToAtTimeD(&attime, &attimeD);
	atAtTimeDToMission(asca_mjd0, &attimeD, &ascatime);
	return ascatime;
}

int
asca2attime(double ascatime, AtTime *attime)
{
	int step;
	AtTimeD attimeD;
	step = atMissionToAtTimeD(asca_mjd0, ascatime, &attimeD);
	atAtTimeDToAtTime(&attimeD, attime);
	return step;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
