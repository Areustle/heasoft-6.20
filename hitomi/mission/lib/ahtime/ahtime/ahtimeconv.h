/// \file ahtimeconv.h
/// \brief Functions to act on time objects: UTC, TT, and TAI.
/// \author Mike Witthoeft
/// \date $Date: 2015/07/06 15:52:41 $
 
#ifndef AHTIME_AHTIMECONV_H
#define AHTIME_AHTIMECONV_H

#include "ahgen/ahversion.h"
AHVERSION(AHTIME_AHTIMECONV,"$Id: ahtimeconv.h,v 1.23 2015/07/06 15:52:41 klrutkow Exp $")

#include "ahtime/AhDateTime.h"
#include "ahtime/AhMJDTime.h"
#include "ahtime/leapsec.h"

/// \ingroup mod_ahtime
namespace ahtime {

/** \addtogroup mod_ahtime
 *  @{
 */

/// \brief Return the number of leap seconds between two UTC calendar dates
///  (inclusive).
/// \param[in] t1 smaller UTC as AhDateTime
/// \param[in] t2 larger UTC as AhDateTime
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \return number of leap seconds
long numLeapSecInInterval(const AhDateTime & t1, const AhDateTime & t2,
                          ahtime::leapsec::LeapSecTable & leapsecdat);

/// \brief Return the number of leap seconds between two UTC MJD dates
///  (inclusive).
/// \param[in] t1 smaller UTC as AhMJDTime
/// \param[in] t2 larger UTC as AhMJDTime
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \return number of leap seconds
long numLeapSecInInterval(const AhMJDTime & t1, const AhMJDTime & t2,
                          ahtime::leapsec::LeapSecTable & leapsecdat);

/// \brief Return the number of leap seconds occurring before the given
///  UTC calendar date.
/// \param[in] time UTC date as AhDateTime
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \return number of leap seconds
long numLeapSecBefore(const AhDateTime & time,
                      ahtime::leapsec::LeapSecTable & leapsecdat);

/// \brief Return the number of leap seconds occurring before the given
///  UTC MJD date.
/// \param[in] time UTC date as AhMJDTime
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \return number of leap seconds
long numLeapSecBefore(const AhMJDTime & time,
                      ahtime::leapsec::LeapSecTable & leapsecdat);

/// \brief Calculate the number of seconds in an interval of two AhDateTime
///  values.
/// \param[in] t1 lower limit
/// \param[in] t2 upper limit
/// \param[out] (optional) subseconds if not NULL, fill in subseconds
/// \return whole number of seconds (not rounded)
///
/// This function neglects any leap seconds that may occur between UTC times.
long long numSecInInterval(const AhDateTime & t1, const AhDateTime & t2,
                           double* subseconds);

/// \brief Calculate the number of seconds in an interval of two AhMJDTime
///  values.
/// \param[in] t1 lower limit
/// \param[in] t2 upper limit
/// \param[out] (optional) subseconds if not NULL, fill in subseconds
/// \return whole number of seconds (not rounded)
///
/// This function neglects any leap seconds that may occur between UTC times.
long long numSecInInterval(const AhMJDTime & t1, const AhMJDTime & t2,
                           double* subseconds);

/// \brief Calculate number of seconds in interval of two UTC times in the
///  calendar format including leap seconds.
/// \param[in] t1 lower limit
/// \param[in] t2 upper limit
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \param[out] (optional) subseconds if not NULL, fill in subseconds
/// \return whole number of seconds (not rounded)
long long numSecInUTCInterval(const AhDateTime & t1, const AhDateTime & t2, 
                              ahtime::leapsec::LeapSecTable & leapsecdat,
                              double* subseconds);

/// \brief Calculate number of seconds in interval of two UTC times in the
///  MJD format including leap seconds.
/// \param[in] t1 lower limit
/// \param[in] t2 upper limit
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \param[out] (optional) subseconds if not NULL, fill in subseconds
/// \return whole number of seconds (not rounded)
long long numSecInUTCInterval(const AhMJDTime & t1, const AhMJDTime & t2, 
                              ahtime::leapsec::LeapSecTable & leapsecdat,
                              double* subseconds);

/// \brief Reformat a number of seconds since the given epoch to MJD.
/// \param[in] seconds number of seconds
/// \param[in] epoch epoch that input seconds is relative to
/// \param[out] mjd output MJD-formatted time
void reformatSecondsAsMJD(double seconds, const AhMJDTime& epoch,
                          AhMJDTime& mjd);

/// \brief Reformat a number of seconds since the given epoch to AhDateTime.
/// \param[in] seconds number of seconds
/// \param[in] epoch epoch that input seconds is relative to
/// \param[out] mjd output AhDateTime-formatted time
void reformatSecondsAsDateTime(double seconds, const AhDateTime& epoch,
                               AhDateTime& dt);

/// \brief Reformat an MJD time to a number of seconds since the given epoch.
/// \param[in] mjd input MJD-formatted time
/// \param[in] epoch epoch that output seconds is relative to
/// \param[out] seconds number of seconds relative to epoch
void reformatMJDAsSeconds(const AhMJDTime& mjd, const AhMJDTime& epoch,
                          double& seconds);

/// \brief Reformat an AhDateTime time to a number of seconds since the given
///  epoch.
/// \param[in] dt input AhDateTime-formatted time
/// \param[in] epoch epoch that output seconds is relative to
/// \param[out] seconds number of seconds relative to epoch
void reformatDateTimeAsSeconds(const AhDateTime& dt, const AhDateTime& epoch,
                               double& seconds);

/// \brief Reformat a number of days since the given epoch to MJD.
/// \param[in] days number of days
/// \param[in] epoch epoch that input days is relative to
/// \param[out] mjd output MJD-formatted time
void reformatDaysAsMJD(double days, const AhMJDTime& epoch, AhMJDTime& mjd);

/// \brief Reformat a number of days since the given epoch to AhDateTime.
/// \param[in] days number of days
/// \param[in] epoch epoch that input days is relative to
/// \param[out] dt output DateTime-formatted time
void reformatDaysAsDateTime(double days, const AhDateTime& epoch,
                            AhDateTime& dt);

/// \brief Reformat an MJD time to a number of days since the given epoch.
/// \param[in] mjd input MJD-formatted time
/// \param[in] epoch epoch that output days is relative to
/// \param[out] days number of days relative to epoch
void reformatMJDAsDays(const AhMJDTime& mjd, const AhMJDTime& epoch,
                       double& days);

/// \brief Reformat an AhDateTime time to a number of days since the given epoch.
/// \param[in] dt input AhDateTime-formatted time
/// \param[in] epoch epoch that output days is relative to
/// \param[out] days number of days relative to epoch
void reformatDateTimeAsDays(const AhDateTime& dt, const AhDateTime& epoch,
                            double& days);

/// \brief Reformat an JD time to MJD.
/// \param[in] jd input Julian Day time
/// \param[out] mjd output MJD-formatted time
void reformatJDAsMJD(double jd, AhMJDTime& mjd);

/// \brief Reformat an JD time to AhDateTime.
/// \param[in] jd input Julian Day time
/// \param[out] dt output AhDateTime-formatted time
void reformatJDAsDateTime(double jd, AhDateTime& dt);

/// \brief Reformat a MJD time to JD.
/// \param[in] mjd input MJD-formatted time
/// \param[out] jd output Julian Day time
void reformatMJDAsJD(const AhMJDTime& mjd, double& jd);

/// \brief Reformat an AhDateTime time to JD.
/// \param[in] dt input AhDateTime-formatted time
/// \param[out] jd output Julian Day time
void reformatDateTimeAsJD(const AhDateTime& dt, double& jd);

/// \brief convert time from MJD format to DateTime
/// \param[in] mjd MJD-formatted time
/// \param[out] dt DateTime-formatted time
void reformatMJDAsDateTime(const AhMJDTime& mjd, AhDateTime& dt);

/// \brief convert time from DateTime format to MJD
/// \param[in] dt DateTime-formatted time
/// \param[out] mjd MJD-formatted time
void reformatDateTimeAsMJD(const AhDateTime& dt, AhMJDTime& mjd);

/// \brief convert mission time to UTC using AhDateTime format
/// \param[in] mt_in mission time in seconds
/// \param[in] utcepoch mission epoch as UTC DateTime
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \param[out] dt_out output UTC as DateTime
void convertMissionTimeToUTC(double mt_in, const AhDateTime& utcepoch, 
                             ahtime::leapsec::LeapSecTable & leapsecdat,
                             AhDateTime& dt_out);

/// \brief convert mission time to UTC using AhMJDTime format
/// \param[in] mt_in mission time in seconds
/// \param[in] utcepoch mission epoch as UTC in the MJD format
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \param[out] mjd_out output UTC as AhMJDTime value
void convertMissionTimeToUTC(double mt_in, const AhMJDTime& utcepoch, 
                             ahtime::leapsec::LeapSecTable & leapsecdat,
                             AhMJDTime& mjd_out);

/// \brief convert UTC time in AhDateTime format to mission time
/// \param[in] dt_in input UTC as DateTime
/// \param[in] utcepoch mission epoch as UTC DateTime
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \param[out] mt_out mission time in seconds
void convertUTCToMissionTime(const AhDateTime& dt_in, const AhDateTime& utcepoch, 
                             ahtime::leapsec::LeapSecTable & leapsecdat,
                             double& mt_out);

/// \brief convert UTC time in AhMJDTime format to mission time
/// \param[in] dt_in input UTC as AhMJDTime
/// \param[in] utcepoch mission epoch as UTC AhMJDTime
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \param[out] mt_out mission time in seconds
void convertUTCToMissionTime(const AhMJDTime& mjd_in, const AhMJDTime& utcepoch, 
                             ahtime::leapsec::LeapSecTable & leapsecdat,
                             double& mt_out);

/// \brief convert UTC time to terrestrial time in DateTime format
/// \param[in] dt_in UTC time as DateTime 
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \param[out] dt_out terrestrial time as DateTime
void convertUTCToTT(const AhDateTime& dt_in, 
                    ahtime::leapsec::LeapSecTable & leapsecdat, 
                    AhDateTime& dt_out);

/// \brief convert UTC time to terrestrial time in MJD format
/// \param[in] mjd_in UTC time as MJD 
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \param[out] mjd_out terrestrial time as MJD
void convertUTCToTT(const AhMJDTime& mjd_in, 
                    ahtime::leapsec::LeapSecTable & leapsecdat, 
                    AhMJDTime& mjd_out);

/// \brief convert terrestrial time to UTC in DateTime format
/// \param[in] dt_in terrestrial time as DateTime 
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \param[out] dt_out UTC time as DateTime
void convertTTToUTC(const AhDateTime& dt_in, 
                    ahtime::leapsec::LeapSecTable & leapsecdat, 
                    AhDateTime& dt_out);

/// \brief convert terrestrial time to UTC in MJD format
/// \param[in] mjd_in terrestrial time as AhMJDTime
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \param[out] mjd_out UTC time as DateTime
///
/// This function will do the conversion via an AhDateTime instance in order to
/// properly account for leapseconds.
void convertTTToUTC(const AhMJDTime& mjd_in, 
                    ahtime::leapsec::LeapSecTable & leapsecdat, 
                    AhMJDTime& mjd_out);

/// \brief convert mission time to terrestrial time in AhDateTime format
/// \param[out] mt_in mission time as seconds since epoch
/// \param[in] ttepoch mission epoch as TT in AhDateTime format
/// \param[in] dt_out terrestrial time as AhDateTime 
void convertMissionTimeToTT(double mt_in, const AhDateTime& ttepoch,
                            AhDateTime& dt_out);

/// \brief convert mission time to terrestrial time in AhMJDTime format
/// \param[out] mt_in mission time as seconds since epoch
/// \param[in] ttepoch mission epoch as TT in AhMJDTime format
/// \param[in] mjd_out terrestrial time as AhMJDTime 
void convertMissionTimeToTT(double mt_in, const AhMJDTime& ttepoch, 
                            AhMJDTime& mjd_out);

/// \brief convert terrestrial time to mission time in AhDateTime format
/// \param[in] dt_in terrestrial time as AhDateTime 
/// \param[in] ttepoch mission epoch as TT in AhDateTime format
/// \param[out] mt_out mission time as seconds since epoch
void convertTTToMissionTime(const AhDateTime& dt_in, const AhDateTime& ttepoch,
                            double& mt_out);

/// \brief convert terrestrial time to mission time in AhMJDTime format
/// \param[in] mjd_in terrestrial time as AhMJDTime 
/// \param[in] ttepoch mission epoch as TT in AhMJDTime format
/// \param[out] mt_out mission time as seconds since epoch
void convertTTToMissionTime(const AhMJDTime& mjd_in, const AhMJDTime& ttepoch, 
                            double& mt_out);

/// \brief calculate UTC string to write as FITS DATE-OBS or DATE-END
///  keyword from the TSTART/TSTOP value
/// \param[in] time TSTART or TSTOP value (seconds from epoch)
/// \param[in] epoch UTC epoch as AhDateTime instance
/// \param[in] leapsecdat leap second data loaded from CALDB file
/// \return UTC time written as string: YYYY-MM-DDTHH:MM:SS.uuuuuu
std::string calcDateObs(double time, const AhDateTime & epoch,
                        ahtime::leapsec::LeapSecTable & leapsecdat);

/// \brief calculate the years, months, days, etc elapsed between two
///  AhDateTime values
/// \param[in] start beginning of interval
/// \param[in] stop end of interval
/// \param[out] dyear number of whole years
/// \param[out] dmonth number of whole months
/// \param[out] dday number of whole days
/// \param[out] dhour number of whole hours
/// \param[out] dminute number of whole minutes
/// \param[out] dsecond number of whole seconds
/// \param[out] dsubsecond number of fractional seconds
void getDeltaCalPieces(const ahtime::AhDateTime& start,
                       const ahtime::AhDateTime& stop,
                       int& dyear, int& dmonth, int& dday, int& dhour,
                       int& dminute, int& dsecond, double& dsubsecond);


/** @} */

}  // namespace ahtime

#endif /* AHTIME_AHTIMECONV_H */

/* Revision Log
 $Log: ahtimeconv.h,v $
 Revision 1.23  2015/07/06 15:52:41  klrutkow
 changed DATE_OBS to DATE-OBS and DATE_END to DATE-END

 Revision 1.22  2014/09/11 14:02:20  mwitthoe
 ahtime library: add reformatting functions to go back and forth from AhDateTime to seconds, days, and JD

 Revision 1.21  2014/09/10 02:41:18  mwitthoe
 ahtime library: move timfile CALDB library to ahmission; move leapsecond CALDB library from ahmission; update AhDateTime & AhMJDTime classes and functions in ahtimeconv to support new version of ahtimeconv

 Revision 1.20  2013/10/16 02:30:41  mwitthoe
 ahtime library: remove references to AhTimeUTC, AhTimeTT, and AhTimeTAI from ahtimeconv.h

 Revision 1.19  2013/10/14 17:29:43  mwitthoe
 ahtime library: remove references to obsolete time classes: AhTimeUTC, AhTimeTT, and AhTimeTAI; change time conversion routines to use new version of leap second structure (see ahmission:leapsec); in make_hktim.cxx, change S_TIME column to TIME and add STATUS column in TIM file

 Revision 1.18  2013/09/19 14:01:32  mwitthoe
 ahtime library: added function to convert from UTC to Mission time in DateTime format; added function to convert from UTC to TT in MJD format; these functions were needed for the new version of the ahtimeconv tool

 Revision 1.17  2013/09/17 19:53:49  mwitthoe
 ahtime library: add function, convertTTToUTC(), to directly convert TT to UTC using times in the MJD format; this function is overloaded with the existing one using AhDateTime arguments

 Revision 1.16  2013/09/17 19:28:05  mwitthoe
 ahtime library: change how time conversion/reformatting is done between time systems and formats (see issue 290 for details); the old method is still intact, but can be removed once all tools/libraries have been switched over to the new method

 Revision 1.15  2013/04/18 20:56:27  mwitthoe
 add function to ahtime library which will calculate the DATE_OBS or DATE_END FITS keyword value from the given TSTART/TSTOP value; the function name is ahtime::calcDateObs()

 Revision 1.14  2013/04/11 21:18:16  mwitthoe
 update ahtime library to use new version of leap second library

 Revision 1.13  2012/11/15 02:54:05  mwitthoe
 change ahtime library to use new ahcaldb library

 Revision 1.12  2012/09/13 17:58:59  mwitthoe
 implement new version method in the ahtime library

 Revision 1.11  2012/08/29 22:12:24  mwitthoe
 in ahtime library, move unit test functions from src to test

 Revision 1.10  2012/08/24 18:53:21  mwitthoe
 clean up argument lists in ahtime library

 Revision 1.9  2012/08/22 15:13:37  mwitthoe
 finish converting ahtime library tests to new format

 Revision 1.8  2012/08/17 20:43:18  mwitthoe
 apply standards to ahtime library

 Revision 1.7  2012/08/15 16:11:52  mwitthoe
 fix to ahfits, ahlog, ahtime versioning

 Revision 1.6  2012/08/15 15:24:12  mwitthoe
 add versioning macros to ahtime header and source files

 Revision 1.5  2012/08/01 15:57:24  mwitthoe
 use local leap second file for testahtime

 Revision 1.4  2012/07/16 21:02:53  mwitthoe
 update ahtime library/tool header comments

 Revision 1.3  2012/07/13 18:32:07  mwitthoe
 clean up ahtime code

 Revision 1.2  2012/07/12 12:12:50  mwitthoe
 standardize and consolidate ahtime unit tests

 Revision 1.1  2012/07/11 16:26:19  mwitthoe
 new ahtime libraries and test codes: ahleapsec - new library to read leap second data from CALDB; AhTimeTAI - library to store number of seconds relative to an epoch; AhTimeTT - library to store modified Julian date (MJD); AhTimeUTC - library to store UTC times; ahtimeconv - conversion functions between 3 AhTime* classes


*/
