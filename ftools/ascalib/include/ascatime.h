/****************************************************************/
/*  ascatime
/*    AtTimeとミッションタイムを変換するルーチン
/****************************************************************/

#ifndef _ASCATIME_H_
#define _ASCATIME_H_

void readLeapTable(char *leaptable);
double mjd2asca(double mjd);
double asca2mjd(double asca);
double round(double x);

#ifdef MJD_J2000	/* check atFunctions */
double attime2asca(AtTime attime);
int asca2attime(double ascatime, AtTime *attime);
#endif

#endif	/* _ASCATIME_H_ */
