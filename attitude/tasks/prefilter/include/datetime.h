#ifndef DATETIME_H
#define DATETIME_H

/*
 * $Source: /headas/headas/attitude/tasks/prefilter/include/datetime.h,v $
 * $Revision: 1.2 $
 * $Date: 2016/10/25 20:00:22 $
 *
 *
 * $Log: datetime.h,v $
 * Revision 1.2  2016/10/25 20:00:22  rwiegand
 * Add timeadj parameter. Swift is the instigater: timeadj is a mechanism to address its clock drift. Satellite positions in particular are sensitive to propagating the right amount of time.
 *
 * Revision 1.1  2002/02/20 14:39:17  miket
 * Merging Bob's original RCS files with HEAdas revisions
 *
 * Revision 1.4  2002/01/28 19:18:54  rwiegand
 * Switched to datetime_ prefix
 *
 * Revision 1.3  2002/01/17 19:57:21  rwiegand
 * Added datetime comparison, offset, printing
 *
 * Revision 1.2  2001/11/01 14:43:50  rwiegand
 * date/time interface
 *
 * Revision 1.1  2001/10/26 16:31:16  rwiegand
 * Initial revision
 */


#include <stdio.h>


typedef enum
{
  DATETIME_OK,
  DATETIME_BAD_INPUT,
  DATETIME_CODE_DUMMY
} datetime_code;


typedef struct
{
  /* only datetime.c should access members of this struct */
  /* there are no guarantees that the fields will not change */
  double julian;
} datetime_t; 


int datetime_set_object (datetime_t *t, const datetime_t *other);

int datetime_set_julian (datetime_t *t, double julian);
int datetime_get_julian (const datetime_t *t, double *pjulian);
int datetime_set_mjd (datetime_t *t, double mjd);
int datetime_set_year_doy_sod (
        datetime_t *t, int year, int doy, double sod);
int datetime_get_year_doy_sod (
        const datetime_t *t, int *pyear, int *pdoy, double *psod);
int datetime_set_year_month_day_sod (
        datetime_t *t, int year, int month, int day, double sod);
int datetime_get_year_month_day_sod (
        const datetime_t *t, int *pyear, int *pmonth, int *pday, double *psod);

double datetime_get_offset (const datetime_t *t);

int is_leap_year (int year);

int year_doy_to_month_day (int year, int doy, int *pmonth, int *pday);
int year_month_day_to_doy (int year, int month, int day, int *pdoy);
int doy_from_year_month_day (int year, int month, int day);

double seconds_from_hours_minutes_seconds (
	double hours, double minutes, double seconds);
int sod_to_hours_minutes_seconds (
	double sod, int *phours, int *pminutes, double *pseconds);

int julian_day_to_year_doy (int jd, int *pyear, int *pdoy);
int julian_day_to_year_month_day (int jd, int *pyear, int *pmonth, int *pday);

double julian_date_of_year (int year);

void datetime_print (const datetime_t *t, FILE *file, const char *header);

int datetime_delta_seconds (datetime_t *t, double seconds);
int datetime_is_after (datetime_t *t, const datetime_t *other);
int datetime_format_year_doy_hhmmss (
	const datetime_t *t, char *buffer, const char *delimiters);


#endif

