/// \file AhDateTime.h
/// \brief Represent a date and time as year, month, day, hour, minute, second,
///  and fraction of a second
/// \author Mike Witthoeft
/// \date $Date: 2014/09/10 02:41:18 $
 
#ifndef AHTIME_AHDATETIME_H
#define AHTIME_AHDATETIME_H

#include "ahgen/ahversion.h"
AHVERSION(AHTIME_AHDATETIME,"$Id: AhDateTime.h,v 1.7 2014/09/10 02:41:18 mwitthoe Exp $")

#include "ahtime/ahtime_base.h"

#include <string>
#include <ctime>

/// \ingroup mod_ahtime
namespace ahtime {

/** \addtogroup mod_ahtime
 *  @{
 */

/// \brief alias for date/time container type
typedef struct tm typ_datetime;

/// \brief container storing date and time as YYYY-MM-DDThh:mm:ss.xxxxxx
///
/// This class uses the ctime struct tm as a container for the following: 
/// years, months, days, hours, minutes, and seconds.  A separate variable
/// exists to hold a fractional second in the range [0,1).  The class imposes
/// (a rather arbitrary) lower limit on the date defined by MINDATE_YEAR,
/// MINDATE_MON, and MINDATE_DAY defined in ahtime_base.h.
class AhDateTime {
  public:

    /// \brief initialize with GPS epoch
    AhDateTime();

    /// \brief initialize with separate year, month, day, etc
    /// \param[in] year 4-digit year
    /// \param[in] month index (1 = January)
    /// \param[in] day
    /// \param[in] hour (0-23)
    /// \param[in] minute (0-59)
    /// \param[in] second (0-60; 60 if leap second)
    /// \param[in] subsecond fraction of second [0.0:1.0)
    AhDateTime(long year, long month, long day, long hour, long minute,
               long second, double subsecond);

    /// \brief initialize with date and time as separate strings
    /// \param[in] date date string: "YYYY-MM-DD"
    /// \param[in] time time string: "hh:mm:ss.xxxx"
    AhDateTime(const std::string & date, const std::string & time);

    /// \brief initialize with date and time as a single string
    /// \param[in] datetime date/time string: "YYYY-MM-DD hh:mm:ss.xxxx"
    AhDateTime(const std::string & datetime);

    /// \brief initialize with date and time as separate integers
    /// \param[in] date integer date: 10000*year+100*month+day
    /// \param[in] time integer time: 10000*hour+100*minute+second
    /// \param[in] subsecond fraction of second [0.0:1.0)
    AhDateTime(long date, long time, double subsecond);

    /// \brief initialize with separate year, month, day, etc
    /// \param[in] year 4-digit year
    /// \param[in] month index (1 = January)
    /// \param[in] day
    /// \param[in] hour (0-23)
    /// \param[in] minute (0-59)
    /// \param[in] second (0-60; 60 if leap second)
    /// \param[in] subsecond fraction of second [0.0:1.0)
    void set(long year, long month, long day, long hour, long minute,
             long second, double subsecond);

    /// \brief set to given date and time strings
    /// \param[in] date date string: "YYYY-MM-DD"
    /// \param[in] time time string: "hh:mm:ss.xxxx"
    void set(const std::string & date, const std::string & time);

    /// \brief set date and time using separate integers
    /// \param[in] date integer date: 10000*year+100*month+day
    /// \param[in] time integer time: 10000*hour+100*minute+second
    /// \param[in] subsecond fraction of second [0.0:1.0)
    void set(long date, long time, double subsecond);

    /// \brief set to given date/time string with decimal values allowed for
    ///  the seconds
    /// \param[in] datetime date/time string: "YYYY-MM-DD hh:mm:ss.xxxx"
    void set(const std::string & datetime);

    /// \brief return year
    /// \return year
    long year() const;

    /// \brief return month
    /// \return month
    long month() const;

    /// \brief return day
    /// \return day
    long day() const;

    /// \brief return hour
    /// \return hour
    long hour() const;

    /// \brief return minute
    /// \return minute
    long minute() const;

    /// \brief return second
    /// \return second
    long second() const;

    /// \brief return fractional part of second
    /// \return fractional part of second
    double subsecond() const;

    /// \brief return the number of days elapsed for year (DDD)
    /// \return number of days (1-365 [366 on leap year])
    long daysInYear() const;

    /// \brief return the number of days remaining for year
    /// \return number of days (0-364 [365 on leap year])
    long daysLeftInYear() const;

    /// \brief compare with another instance; +1 means this > dt
    /// \param[in] dt AhDateTime instance to compare to
    /// \return +1,0,-1 based on order
    ///
    /// Compare stored date and time with those in another AhDateTime
    /// instance. Return values are +1, 0, or -1 depending on the 
    /// current time being later than, concurrent to, or earlier than the 
    /// given time.
    int compare(const AhDateTime & dt) const;

    /// \brief return date/time as a string: "YYYY-MM-DDTHH:MM:SS"
    /// \param[in] ndigits number of fractional second digits to include
    ///  (default: 0)
    /// \return formatted string
    std::string getDateTimeAsStr(int ndigits=0) const;

    /// \brief return date as a string: "YYYY-MM-DD"
    /// \return formatted string
    std::string getDateAsStr() const;

    /// \brief return time as a string: "HH:MM:SS"
    /// \param[in] ndigits number of fractional second digits to include
    ///  (default: 0)
    /// \return formatted string
    std::string getTimeAsStr(int ndigits=0) const;

    /// \brief return date as an integer: 10000*year+100*month+day
    /// \return date as integer
    long getDateAsInt() const;

    /// \brief return time as an integer: 10000*hour+100*minute+second
    /// \return time as integer
    long getTimeAsInt() const;

    /// \brief return subsecond count to given precision (will round values
    /// of finer precision)
    /// \param[in] ndigits number of digits to return
    /// \return 10^n * subsecond
    long getSubsecondAsInt(int ndigits) const;

    /// \brief return number of seconds elapsed since start of current year
    /// (not including possible leap second for UTC times)
    /// \param[out] (optional) subseconds fill with number of subseconds if not
    ///   NULL
    /// \return number of seconds
    long _secIntoYear(double* subsecond=0) const;

    /// \brief return number of seconds left in current year (not including
    /// possible leap second for UTC; _secLeftInYear()+_secIntoYear() = 86400)
    /// \param[out] (optional) subseconds fill with number of subseconds if not 
    ///   NULL
    /// \return number of seconds
    long _secLeftInYear(double* subseconds=0) const;

    /// \brief add the given number of seconds (and subseconds) to time
    ///   (does not account for leap seconds for UTC times)
    /// \param[in] seconds integer number of seconds
    /// \param[in] subseconds fractional number of seconds
    void _addSeconds(long long seconds, double subseconds);

    /// \brief substract the given number of seconds (and subseconds) from
    ///   time (does not account for leap seconds for UTC times)
    /// \param[in] seconds integer number of seconds
    /// \param[in] subseconds fractional number of seconds
    void _subtractSeconds(long long seconds, double subseconds);

    /// \brief add one second to time without checking if minutes (and so on)
    ///   need to be incremented; this is used to allow for the number of 
    ///   seconds to be 60 when time is at a leap second time.
    void _addOneSecondForce();

    /// \brief if the subsecond is near zero or one within SUBSECOND_THRESHOLD
    ///  then assume the subsecond should be zero and fix the second count
    ///  appropriately.  This is to adjust for imprecision of floating-point
    ///  calculations
    void _adjustSubsecondNearZero();

  private:
    typ_datetime m_datetime;    ///< struct storing all date/time components
    double m_subsecond;         ///< fraction of second not in m_datetime

};


/** @} */

}  // namespace ahtime

#endif /* AHTIME_AHDATETIME_H */

/* Revision Log
 $Log: AhDateTime.h,v $
 Revision 1.7  2014/09/10 02:41:18  mwitthoe
 ahtime library: move timfile CALDB library to ahmission; move leapsecond CALDB library from ahmission; update AhDateTime & AhMJDTime classes and functions in ahtimeconv to support new version of ahtimeconv

 Revision 1.6  2013/12/04 16:09:22  mwitthoe
 ahtime library: add ability to set AhDateTime with a single date-time string including fractional seconds, i.e. YYYY-MM-DDThh:mm:ss.xxxxxx

 Revision 1.5  2013/10/15 17:51:49  mwitthoe
 ahtime library: add function to the AhDateTime class, _adjustSubsecondNearZero, which will look for a subsecond count near to 0. or 1. and adjust the time so that the subsecond is exactly zero; this helps to make sure that the integer number of seconds is consistent across different architectures

 Revision 1.4  2013/09/18 14:09:13  mwitthoe
 ahtime library: add classes to store times in the DateTime and MJD formats


*/
