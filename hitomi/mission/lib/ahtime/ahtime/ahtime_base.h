/// \file ahtime_base.h
/// \brief General time constants and functions.
/// \author Mike Witthoeft
/// \date $Date: 2014/09/10 02:41:18 $
 
#ifndef AHTIME_AHTIME_BASE_H
#define AHTIME_AHTIME_BASE_H

#include "ahgen/ahversion.h"
AHVERSION(AHTIME_AHTIME_BASE,"$Id: ahtime_base.h,v 1.25 2014/09/10 02:41:18 mwitthoe Exp $")

#include <string>
#include <vector>

/// \brief functions related to time or time-assignment
/// \ingroup mod_ahtime
namespace ahtime {

/** \addtogroup mod_ahtime
 *  @{
 */

/// \brief number of seconds in a minute
const long SEC_IN_MIN=60;

/// \brief number of minutes in an hour
const long MIN_IN_HOUR=60;

/// \brief number of seconds in an hour
const long SEC_IN_HOUR=SEC_IN_MIN*MIN_IN_HOUR;

/// \brief hours in day
const long HOUR_IN_DAY=24;

/// \brief number of seconds in a day
const long SEC_IN_DAY=SEC_IN_HOUR*HOUR_IN_DAY;

/// \brief number of days in a (non-leap) year
const long DAY_IN_YEAR=365;

/// \brief number of months in year
const long MON_IN_YEAR=12;

/// \brief number of seconds in a (non-leap) year
const long SEC_IN_YEAR=SEC_IN_DAY*DAY_IN_YEAR;

// if a subsecond is within this threshold of 0/1, then it will be
// rounded down/up (to take care of some numerical precision issues)
const double SUBSECOND_THRESHOLD=0.;    // if =0, then no rounding performed

/// \brief GPS epoch in calendar format (UTC)
const std::string GPS_EPOCH_CAL="1980-01-06T00:00:00";

/// \brief GPS epoch in MJD format (UTC)
const double GPS_EPOCH_MJD=44244;

/// \brief year of minimum UTC date
const long MINDATE_YEAR=1972;

/// \brief month of minimum UTC date
const long MINDATE_MON=1;

/// \brief day of minimum UTC date
const long MINDATE_DAY=1;


/// \brief Return true if given string is a positive integer number.
/// \param[in] val string to check
/// \return true if value is a integer-type number 
bool isNumber(const std::string& val);

/// \brief Resolve date string into year, month, and day.
/// \param[in] datestr date string
/// \param[out] year
/// \param[out] month
/// \param[out] day
///
/// Supported formats: YYYY-MM-DD and YYYY:DDD.  The separator characters 
/// ('-', ':') are flexible; all can be replaced by a space or any other
/// character.  It is important that there is a single character separating
/// one value from the next.  This function does not check the validity of
/// the component values, e.g. a month of 37 will not cause an error.
/// An exception is thrown if the format is not understood.
void parseDateString(const std::string& datestr, long& year, long& month,
                     long& day);

/// \brief Resolve time string into hour, minute, and second.
/// \param[in] datestr date string
/// \param[out] hour
/// \param[out] minute
/// \param[out] second
///
/// Supported formats: hh:mm:ss.xxx where ss.xxxx represent seconds with a
/// fractional component (the fractional part is optional)..  The separator
/// character (':') is flexible; it can be replaced by a space or any other
/// character.  It is important that there is a single character separating
/// one value from the next.  This function does not check the validity of
/// the component values, e.g. a minute of 37 will not cause an error.
/// An exception is thrown if the format is not understood.
void parseTimeString(const std::string& timestr, long& hour, long& minute,
                     double& second);

/// \brief Resolve date/time string into year, month, day, hour, minute,
///  and second.
/// \param[in] dt date/time string
/// \param[out] year
/// \param[out] month
/// \param[out] day
/// \param[out] hour
/// \param[out] minute
/// \param[out] second
///
/// Supported formats: YYYY-MM-DDThh:mm:ss.xxxx and YYYY:DDD:hh:mm:ss.xxxx
/// where ss.xxxx represent seconds with a fractional component (the 
/// fractional part is optional).  The separator characters ('-', ':', 'T') 
/// are flexible; all can be replaced by a space or any other character. 
/// It is important that there is a single character separating one value
/// to the next.  This function does not check the validity of the component
/// values, e.g. a month of 37 will not cause an error.  An exception is
/// thrown if the format is not understood.
void parseDateTimeString(const std::string& dt, long& year, long& month,
                         long& day, long& hour, long& minute, double& second);

/// \brief Return true if given year is a leap year.
/// \param[in] year integer year, YYYY
/// \return true if given a leap year, false otherwise
bool isLeapYear(long year); 

/// \brief Return number of days in given month index (1 = January); does not
/// account for leap years.
/// \param[in] imon index of month (1=January)
/// \return number of days (28, 30, or 31)
long daysInMonth(long imon); 

/// \brief Return number of days in given month index (1 = January); does
/// account for leap years.
/// \param[in] imon index of month (1=January)
/// \param[in] year year
/// \return number of days
long daysInMonth(long imon, long year); 

/// \brief Convert a day number (1-365/366) into the month and day of month.
/// \param[in] daynumber day of the year (1-365 or 366 if leap year)
/// \param[in] year need year to know if leap day is present
/// \param[out] month number of month (1-12)
/// \param[out] day number of day in month (1-31)
void convertDayNumberIntoMonthAndDay(long daynumber, long year, long& month,
                                     long& day);

/// \brief Convert a month and day into the day number (1-365/366).
/// \param[in] month number of month (1-12)
/// \param[in] day number of day in month (1-31)
/// \param[in] year need year to know if leap day is present
/// \param[out] daynumber day of the year (1-365 or 366 if leap year)
void convertMonthAndDayIntoDayNumber(long month, long day, long year, 
                                     long& daynumber);

/// \brief break up whole number of seconds into seconds, minutes, hours, and 
///  days where the output seconds and minutes are less than 60 and the hours
///  are less than 24.
/// \param[in] seconds whole number of seconds 
/// \param[out] nsec output seconds (< 60)
/// \param[out] nmin output minutes (< 60)
/// \param[out] nhour output hours (< 24)
/// \param[out] nday output days
void secondsBreakUp(long long seconds, long & nsec, long & nmin, long & nhour,
                    long & nday);

/// \brief combine time string with fractional part of seconds
/// \param[in] time time string
/// \param[in] subseconds fractional part of seconds: [0.:1.)
/// \param[in] digits number of decimals to retain
/// \return time string with fractional part of seconds appended
///
/// This function will combine a time string (e.g. 12:34:10) with a fractional
/// number of seconds (e.g. 0.234) to get a new string with both pieces of
/// information (e.g. 12:34:10.234).  The fractional part is rounded to 
/// conform to the value of the digits parameter.
std::string catTimeSubseconds(const std::string & time, 
                              double subseconds, long digits);

/// \brief split from given time string the fractional part of the seconds
///   count; this is the reverse of catTimeSubseconds()
/// \param[in] instr input time string with fractional number of seconds
/// \param[out] time time string with whole number of seconds
/// \param[out] subseconds fractional number of seconds
/// \return false if no fractional part is found in input string
bool uncatTimeSubseconds(const std::string & instr, std::string & time,
                         double & subseconds);

/** @} */

}  // namespace ahtime

#endif /* AHTIME_AHTIME_BASE_H */

/* Revision Log
 $Log: ahtime_base.h,v $
 Revision 1.25  2014/09/10 02:41:18  mwitthoe
 ahtime library: move timfile CALDB library to ahmission; move leapsecond CALDB library from ahmission; update AhDateTime & AhMJDTime classes and functions in ahtimeconv to support new version of ahtimeconv

 Revision 1.24  2013/04/10 14:47:59  mwitthoe
 remove unused constants: ASTROH_EPOCH, ASTROH_EPOCH_OFFSET, and ASTROH_L32TI_MAXCYCLE from ahtime_base.h in ahtime library

 Revision 1.23  2013/01/19 17:29:53  mwitthoe
 change TI variable to unsigned int (using a typedef); change make_hktim.cxx to produce test data with proper range of L32TI; add conversion from U32TI to L32TI in ahtimeassign for relevant instruments

 Revision 1.22  2012/11/26 21:23:20  mwitthoe
 add brief descriptions to namespaces in mission

 Revision 1.21  2012/10/26 14:23:07  mwitthoe
 add ASTROH_EPOCH constant to ahtime_base

 Revision 1.20  2012/10/25 01:38:00  mwitthoe
 remove ahtimfile library from ahtime; its functions have been generalized and put into the ahlookup library in ahmath

 Revision 1.19  2012/10/24 17:18:48  mwitthoe
 add cycling constant for L32TI to ahtime_base

 Revision 1.18  2012/10/22 21:11:42  mwitthoe
 add constant giving number of seconds between GPS and Astro-H epochs; add name of localhk file to time assignment function

 Revision 1.17  2012/10/18 17:35:40  mwitthoe
 ahtime library: created single header file, ahtime.h, to include entire ahtime library (old ahtime -> ahtime_base); separated CAMS data from instrument delay CALDB file into new file: associated libraries created

 Revision 1.16  2012/09/13 17:58:59  mwitthoe
 implement new version method in the ahtime library

 Revision 1.15  2012/09/11 18:52:07  mwitthoe
 restructuring time assignment library to prepare for science data (not finished)

 Revision 1.14  2012/08/29 22:12:24  mwitthoe
 in ahtime library, move unit test functions from src to test

 Revision 1.13  2012/08/28 21:34:44  mwitthoe
 changes to ahtime library: clean up; change leap second value from double to int; fix memory leak

 Revision 1.12  2012/08/24 18:53:21  mwitthoe
 clean up argument lists in ahtime library

 Revision 1.11  2012/08/22 15:13:37  mwitthoe
 finish converting ahtime library tests to new format

 Revision 1.10  2012/08/21 19:08:43  mwitthoe
 partial switchover to new testing regime to see if ahtest works

 Revision 1.9  2012/08/17 20:43:18  mwitthoe
 apply standards to ahtime library

 Revision 1.8  2012/08/15 16:11:52  mwitthoe
 fix to ahfits, ahlog, ahtime versioning

 Revision 1.7  2012/08/15 15:24:12  mwitthoe
 add versioning macros to ahtime header and source files

 Revision 1.6  2012/07/16 21:02:53  mwitthoe
 update ahtime library/tool header comments

 Revision 1.5  2012/07/13 19:17:22  mwitthoe
 set 1972-01-01 as the minimum date supported by AhTimeUTC

 Revision 1.4  2012/07/13 18:32:07  mwitthoe
 clean up ahtime code

 Revision 1.3  2012/07/12 12:12:50  mwitthoe
 standardize and consolidate ahtime unit tests

 Revision 1.2  2012/07/11 16:19:05  mwitthoe
 remove obsolete libraries from ahtime

 Revision 1.1  2012/06/19 21:31:34  mwitthoe
 move core/lib/ahtimeconv to core/lib/ahtime

 Revision 1.6  2012/06/07 11:11:39  mwitthoe
 ahtime: add ahcolumndef, ahdelay, and ahtimeassign with test programs and HK FITS file for testing

 Revision 1.5  2012/05/29 14:47:47  mwitthoe
 created class AhDateTime to replace functions in ahdatetime.h; moved leap-second functions into ahleapsec.h

 Revision 1.4  2012/05/23 23:27:46  mwitthoe
 add ahdatetime defining a structure to hold a date and time (and functions operating on it); add functions to ahtime to determine number of leap seconds in a date/time interval; remove AhTimeElem from ahtime (but class still present)

 Revision 1.3  2012/04/24 19:16:18  mwitthoe
 documentation for ahtime

*/
