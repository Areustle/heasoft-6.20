/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:52 1999 by E. Miyata
 * 2003/08/20 T.Tsuru
 *   add #include <sys/time.h>
 */
#include <stdio.h>
#include <sys/types.h>
#include <time.h>
#include <sys/time.h>
#include "atFunctions.h"
#include "aste_time.h"

#ifndef AE_EPOCH		/* 2000/1/1 0:0:0 の、1970/1/1 0:0:0 からの */
#define AE_EPOCH 946684800	/* 経過秒数 (うるう秒含まず)		    */
#endif

/*****************************************************************
  timeval をもらって aetime返す関数
******************************************************************/
double
timeval2aetime(struct timeval t)
{
	struct tm *ut;
	AtTimeD attime;
	double aetime;

	ut = gmtime(&t.tv_sec);
	attime.yr = ut->tm_year+1900;
	attime.mo = ut->tm_mon+1;
	attime.dy = ut->tm_mday;
	attime.hr = ut->tm_hour;
	attime.mn = ut->tm_min;
	attime.sc = ut->tm_sec;
	attime.ss = t.tv_usec/1.e6;
	aetime = attimeD2aste(&attime);

	return aetime;
}

/*****************************************************************
  time_t をもらって aetime返す関数
******************************************************************/
double
time2aetime(time_t t)
{
	struct tm *ut;
	AtTimeD attime;
	double aetime;

	ut = gmtime(&t);
	attime.yr = ut->tm_year+1900;
	attime.mo = ut->tm_mon+1;
	attime.dy = ut->tm_mday;
	attime.hr = ut->tm_hour;
	attime.mn = ut->tm_min;
	attime.sc = ut->tm_sec;
	attime.ss = 0.0;
	aetime = attimeD2aste(&attime);

	return aetime;
}

/*****************************************************************
  time_t をもらって localtime の aetime 返す関数
  つまり、localtime で 1992/01/01 からの経過時刻、と言う意味
******************************************************************/
double
time2aetimeLocal(time_t t)
{
	struct tm *ut;
	AtTimeD attime;
	double aetime;

	ut = localtime(&t);
	attime.yr = ut->tm_year+1900;
	attime.mo = ut->tm_mon+1;
	attime.dy = ut->tm_mday;
	attime.hr = ut->tm_hour;
	attime.mn = ut->tm_min;
	attime.sc = ut->tm_sec;
	attime.ss = 0.0;
	aetime = attimeD2aste(&attime);

	return aetime;
}

#define	MAX_DATE_CHAR_SIZE		32

/*****************************************************************
  time_t をもらって日付を返す関数
     UT で返すのは time2utCtime
     local 時間 で返すのは time2localCtime
******************************************************************/
char *
time2utCtime(time_t t, char *c)
{
	struct tm *ut;
	ut = gmtime(&t);
	strftime(c, MAX_DATE_CHAR_SIZE, "%Y/%m/%d %H:%M:%S", ut);
	return c;
}

char *
time2localCtime(time_t t, char *c)
{
	struct tm *local;
	local = localtime(&t);
	strftime(c, MAX_DATE_CHAR_SIZE, "%Y/%m/%d %H:%M:%S", local);
	return c;
}

/*****************************************************************
  timeval をもらって日付を返す関数
    UT で返すのは timeval2utCtime
    local 時間で返すのは timeval2localCtime
******************************************************************/
char *
timeval2utCtime(struct timeval t, char *c)
{
	struct tm *ut;
	ut = gmtime (&t.tv_sec);
	strftime (c, MAX_DATE_CHAR_SIZE, "%Y/%m/%d %H:%M:%S", ut);
	return c;
}

char *
timeval2localCtime(struct timeval t, char *c)
{
	struct tm *local;
	local = localtime (&t.tv_sec);
	strftime (c, MAX_DATE_CHAR_SIZE, "%Y/%m/%d %H:%M:%S", local);
	return c;
}

/*****************************************************************
  aetime をもらって日付を返す関数群
    UT で返すのは aetime2utCtime
    local 時間で返すのは aetime2localCtime
******************************************************************/
char *
aetime2utCtime(double t, char *c)
{
	AtTimeD attime;
	aste2attimeD(t, &attime);
	sprintf (c, "%4d/%02d/%02d %02d:%02d:%02d",
			 attime.yr, attime.mo, attime.dy,
			 attime.hr, attime.mn, attime.sc);
	return c;
}

char *
aetime2localCtime(double t, char *c)
{
	AtTimeD attime;
	t += 9. * 3600.;
	aste2attimeD(t, &attime);
	sprintf (c, "%4d/%02d/%02d %02d:%02d:%02d",
			 attime.yr, attime.mo, attime.dy,
			 attime.hr, attime.mn, attime.sc);
	return c;
}

int
aetime2aedate(double aetime)
{
	return (aetime/3600/24);
}
