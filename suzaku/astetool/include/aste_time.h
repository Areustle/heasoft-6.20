/* aste_time.h,v 1.2 1999/06/03 09:10:59 ishisaki Exp */
/****************************************************************
  aste_time.h

	convert AtTimeD structure into mission time (Astro-E2|ASCA time)
    [AtTimeD とミッションタイム (Astro-E time) を変換するルーチン]

	2006/08/01	Y.ISHISAKI	version 1.80
		add aste_mjdrefi(), aste_mjdreff()
		add aste2mjdtt(), mjdtt2aste() functions, in fact they are generic

*****************************************************************/

#ifndef _ASTE_TIME_H_
#define _ASTE_TIME_H_

/* 1993/01/01 00:00:00 0.0 */
#define ASCA_MJD_BASE	48988.0

/* 2000/01/01 00:00:00 0.0 */
#define ASTE_MJD_BASE	51544.0

/* for conversion with Terrestrial Time */
#define ASTE_MJDREFI	51544				/* 2000.0 UT */
#define ASTE_MJDREFF	0.00074287037037037	/* (64.184/86400) */

#ifdef __cplusplus
extern "C"
{
#endif

/* aste_time.c */

char *mission_time_init(char *leaptable);

double mjd2aste(double mjd);
double aste2mjd(double astetime);
double mjd2asca(double mjd);
double asca2mjd(double ascatime);

#ifdef MJD_J2000	/* check atFunctions */
double attime2aste(AtTime attime);
int aste2attime(double astetime, AtTime *attime);
double attime2asca(AtTime attime);
int asca2attime(double ascatime, AtTime *attime);

double attimeD2aste(AtTimeD *attime);
int aste2attimeD(double astetime, AtTimeD *attime);
double attimeD2asca(AtTimeD *attime);
int asca2attimeD(double ascatime, AtTimeD *attime);
#endif

/* aste_time_tt.c */

int aste_mjdrefi(void);
double aste_mjdreff(void);
double mjdtt2aste(double mjd_tt, int mjdrefi, double mjdreff);
double aste2mjdtt(double mission_time, int mjdrefi, double mjdreff);

#ifdef __cplusplus
}
#endif

#endif	/* _ASTE_TIME_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
