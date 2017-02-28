/*
 * $Source: /headas/headas/attitude/tasks/prefilter/datetime.c,v $
 * $Revision: 1.6 $
 * $Date: 2016/10/25 20:00:22 $
 *
 * $Log: datetime.c,v $
 * Revision 1.6  2016/10/25 20:00:22  rwiegand
 * Add timeadj parameter. Swift is the instigater: timeadj is a mechanism to address its clock drift. Satellite positions in particular are sensitive to propagating the right amount of time.
 *
 * Revision 1.5  2005/09/14 21:41:12  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.4  2002/05/14 14:51:57  rwiegand
 * Reworked parameter interface with Ed Pier's suggestions.
 *
 * Revision 1.3  2002/01/31 13:51:17  rwiegand
 * Corrected conversion from seconds of day to hours, minutes, seconds
 *
 * Revision 1.2  2002/01/29 20:21:22  rwiegand
 * Use datetime_ as prefix of functions
 *
 * Revision 1.1  2001/11/01 14:44:10  rwiegand
 * Initial revision
 *
 */

#include <stdio.h>
#include <math.h>

#include "datetime.h"
#include "report.h"


#define KSKelso 0

#define SECONDS_PER_DAY 86400

const double MJD_BASE = 2400000.5;


int datetime_set_julian (datetime_t *t, double julian)
{
  int code = 0;
  t->julian = julian;
  return code;
}


int datetime_get_julian (const datetime_t *t, double *pjulian)
{
  int code = 0;
  *pjulian = t->julian;
  return code;
}


int datetime_set_mjd (datetime_t *t, double mjd)
{
  int code = 0;
  t->julian = MJD_BASE + mjd;
  return code;
}


/*
 TODO validate year/doy/sod
 */

int datetime_set_year_doy_sod (datetime_t *t, int year, int doy, double sod)
{
  int code;
  double julian, base;
  code = 0;
  base = julian_date_of_year(year);
  julian = base + doy + sod / SECONDS_PER_DAY;
  t->julian = julian;
  return code;
}


int is_leap_year (int year)
{
  int leap = 0;
  if ((year % 4) == 0) {
    if ((year % 100) == 0) {
      if ((year % 400) == 0)
        leap = 1;
    }
    else
      leap = 1;
  }
  return leap;
}


int days_in_year (int year)
{
  int days = is_leap_year(year) ? 366 : 365;
  return days;
}


/*
 TODO validate year/month/day
 */

int year_month_day_to_doy (int year, int month, int day, int *pdoy)
{
static int days_before[] = {
	0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
	};

  int code, leap, leap_day;
  code = 0;
  leap = is_leap_year(year);
  leap_day = (leap && (month > 2)) ? 1 : 0;
  *pdoy = days_before[month] + leap_day + day;
  return code;
}


int doy_from_year_month_day (int year, int month, int day)
{
  int code, doy;
  code = year_month_day_to_doy(year, month, day, &doy);
  if (code) {
    doy =  0;
    report_error("invalid input %d/%d/%d\n", year, month, day);
  }
  return doy;
}


int year_doy_to_month_day (int year, int doy, int *pmonth, int *pday)
{
static int days_in_month[] = {
	31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
	};
static int leap_day[] = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        };
  int code = 0;
  leap_day[1] = is_leap_year(year);
  if ((doy < 1) || (doy > days_in_year(year)))
    code = 1;
  else {
    int i, tmp;
    i = 0;
    tmp = doy;
    for (i = 0; i < 12; ++i) {
      int days = days_in_month[i] + leap_day[i];
      if (tmp <= days)
        break;
      tmp -= days;
    }
    *pmonth = i + 1;
    *pday = tmp;
  }
  return code;
}


/*
  Fliegel and Van Flandern algorithm for converting between Julian
  and Gregorian dates
*/

int julian_day_to_year_month_day (
        int jd, int *pyear, int *pmonth, int *pday)
{
  int code;
  int l,n,i,j,d,m,y;
  code = 0;
  l = jd + 68569;
  n = (4 * l) / 146097;
  l = l - (146097 * n + 3) / 4;
  i = (4000 * (l + 1)) / 1461001;
  l = l - (1461 * i) / 4 + 31;
  j = (80 * l) / 2447;
  d = l - (2447 * j) / 80;
  l = j / 11;
  m = j + 2 - (12 * l);
  y = 100 * (n - 49) + i + l;
  *pyear = y;
  *pmonth = m;
  *pday = d;
  return code;
}


int datetime_get_year_doy_sod (
        const datetime_t *t, int *pyear, int *pdoy, double *psod)
{
  int code = 0;
  double julian, remainder;
  int jd, month, day;
  remainder = modf(t->julian + 0.5, &julian);
  jd = (int) julian;
  julian_day_to_year_month_day(jd, pyear, &month, &day);
  *pdoy = doy_from_year_month_day(*pyear, month, day);
  *psod = remainder * SECONDS_PER_DAY;
  return code;
}


/*
    TODO: validate year/month/day
*/

int datetime_set_year_month_day_sod (
	datetime_t *t, int year, int month, int day, double sod)
{
  int doy = doy_from_year_month_day(year, month, day);
  return datetime_set_year_doy_sod(t, year, doy, sod);
}


int datetime_get_year_month_day_sod (
        const datetime_t *t, int *pyear, int *pmonth, int *pday, double *psod)
{
  int code;
  double julian, remainder;
  int jd;
  code = 0;
  remainder = modf(t->julian + 0.5, &julian);
  jd = (int) julian;
  julian_day_to_year_month_day(jd, pyear, pmonth, pday);
  *psod = remainder * SECONDS_PER_DAY;
  return code;
}



#if KSKelso
Function Julian_Date_of_Year(year : double) : double;
  { Astronomical Formulae for Calculators, Jean Meeus, pages 23-25 }
  { Calculate Julian Date of 0.0 Jan year }
  var
    A,B   : longint;
  begin
  year := year - 1;
  A := Trunc(year/100);
  B := 2 - A + Trunc(A/4);
  Julian_Date_of_Year := Trunc(365.25 * year)
                       + Trunc(30.6001 * 14)
                       + 1720994.5 + B;
  end; {Function Julian_Date_of_Year}
#endif

double julian_date_of_year (int year)
{
  double julian;
  int A, B;
  year -= 1;
  A = year / 100;
  B = 2 - A + A/4;
  julian = floor(365.25 * year)
         + floor(30.6001 * 14)   /* constant */
         + 1720994.5 + B;
  return julian;
}


void datetime_print (const datetime_t *t, FILE *file, const char *header)
{
  double julian, sod;
  int year, doy, month, day;

  datetime_get_julian(t, &julian);

  datetime_get_year_doy_sod(t, &year, &doy, &sod);
  year_doy_to_month_day(year, doy, &month, &day);

  fprintf(file,
		"%s\n"
			"\tobject\t%p\n"
			"\tjulian\t%f\n"
			"\tyear\t%d\n"
			"\tdoy\t%d\n"
			"\tmonth\t%d\n"
			"\tday\t%d\n"
			"\tsod\t%f\n",
		header,
		(void *) t,
		julian,
		year, doy, month, day, sod);

}


double seconds_from_hours_minutes_seconds (
		double hours, double minutes, double seconds)
{
	double tmp = 3600 * hours + 60 * minutes + seconds;
	return tmp;
}


/* no leap second support */
int sod_to_hours_minutes_seconds (double sod,
		int *phours, int *pminutes, double *pseconds)
{
	int code = 0;

	double remainder;

	if (sod > SECONDS_PER_DAY)
		code = 1;
	remainder = sod;

	*phours = (int) (remainder / 3600);
	remainder -= 3600 * *phours;

	*pminutes = (int) (remainder / 60);
	remainder -= 60 * *pminutes;

	*pseconds = remainder;

	return code;
}

