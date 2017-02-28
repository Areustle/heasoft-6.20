/// \file AhDateTime.cxx
/// \brief Represent a date and time as year, month, day, hour, minute, second,
///  and fraction of a second
/// \author Mike Witthoeft
/// \date $Date: 2014/09/10 02:41:19 $

#define AHLABEL ahtime_AhDateTime
#define AHCVSID "$Id: AhDateTime.cxx,v 1.10 2014/09/10 02:41:19 mwitthoe Exp $"

#include "ahlog/ahlog.h"
#include "ahtime/AhDateTime.h"

#include <sstream>
#include <iomanip>
#include <stdlib.h>
#include <cmath>

namespace ahtime {

// ---------------------------------------------------------------------------

AhDateTime::AhDateTime() {
  set(GPS_EPOCH_CAL);
}

// ---------------------------------------------------------------------------

AhDateTime::AhDateTime(long year, long month, long day, long hour, long minute, 
                       long second, double subsecond) {
  set(year,month,day,hour,minute,second,subsecond);
}

// ---------------------------------------------------------------------------

AhDateTime::AhDateTime(const std::string & date, const std::string & time) {
  set(date,time);
}

// ---------------------------------------------------------------------------

AhDateTime::AhDateTime(const std::string & datetime) {
  set(datetime);
}

// ---------------------------------------------------------------------------

AhDateTime::AhDateTime(long date, long time, double subsecond) {
  set(date,time,subsecond);
}

// ---------------------------------------------------------------------------

void AhDateTime::set(long year, long month, long day, long hour, long minute,
                     long second, double subsecond) {

  // Simple check of input ranges
  if (month < 1 || month > ahtime::MON_IN_YEAR) {
    std::stringstream msg;
    msg << "month value out of range: " << month;
    AH_THROW_RUNTIME(msg.str());
  }
  if (day < 1 || day > 31) {
    std::stringstream msg;
    msg << "day value out of range: " << day;
    AH_THROW_RUNTIME(msg.str());
  }
  if (hour < 0 || hour > ahtime::HOUR_IN_DAY) {
    std::stringstream msg;
    msg << "hour value out of range: " << hour;
    AH_THROW_RUNTIME(msg.str());
  }
  if (minute < 0 || minute > ahtime::MIN_IN_HOUR) {
    std::stringstream msg;
    msg << "minute value out of range: " << minute;
    AH_THROW_RUNTIME(msg.str());
  }
  if (second < 0 || second > ahtime::SEC_IN_MIN) {
    std::stringstream msg;
    msg << "second value out of range: " << second;
    AH_THROW_RUNTIME(msg.str());
  }
  if (subsecond < 0. || subsecond >= 1.) {
    std::stringstream msg;
    msg << "subsecond value out of range: " << subsecond;
    AH_THROW_RUNTIME(msg.str());
  }

  // reject if date is before MINDATE_YEAR/MON/DAY (see ahtime_base.h)
  bool reject=false;
  if (year < MINDATE_YEAR) reject=true;
  if (year == MINDATE_YEAR && month == MINDATE_MON && day < MINDATE_DAY)
    reject=true;
  if (reject) {
    std::stringstream tstr;
    tstr << MINDATE_YEAR << "-" << MINDATE_MON << "-" << MINDATE_DAY;
    AH_THROW_RUNTIME("times before "+tstr.str()+" not supported");
  }

  m_datetime.tm_year=year-1900;    // counted from 1900
  m_datetime.tm_mon=month-1;       // month range 0-11
  m_datetime.tm_mday=day;          // day range 1-31
  m_datetime.tm_hour=hour;         // range: 0-23
  m_datetime.tm_min=minute;        // range: 0-59
  m_datetime.tm_sec=second;        // range: 0-61  (account for leap seconds)
  m_subsecond=subsecond;

  // to avoid problems with rounding, subseconds close enough to zero will be
  // set to zero
  _adjustSubsecondNearZero();
}

// ---------------------------------------------------------------------------

void AhDateTime::set(const std::string & date, const std::string & time) {

  // get year, month, and day
  long year=0, month=0, day=0;
  ahtime::parseDateString(date,year,month,day);

  // get hour, min, and sec
  long hour=0,minute=0;
  double second=0.;
  ahtime::parseTimeString(time,hour,minute,second);

  // divide second into whole and fractional parts
  long wholesec=(long)second;
  double subsecond=second-wholesec;

  // fill structure
  set(year,month,day,hour,minute,wholesec,subsecond);
}

// ---------------------------------------------------------------------------

void AhDateTime::set(const std::string& datetime) {

  // get components
  long year=0, month=0, day=0;
  long hour=0,minute=0;
  double second=0.;
  ahtime::parseDateTimeString(datetime,year,month,day,hour,minute,second);

  // divide second into whole and fractional parts
  long wholesec=(long)second;
  double subsecond=second-wholesec;

  // fill structure
  set(year,month,day,hour,minute,wholesec,subsecond);
}

// ---------------------------------------------------------------------------

void AhDateTime::set(long date, long time, double subsecond) {
  // split date
  long year,mon,mday;
  year=date/10000;           // drop last 4 digits of integer
  long tmp=date-year*10000;
  mon=tmp/100;               // drop last 2 digits of integer
  mday=tmp-mon*100;

  // split time
  long hour,min,sec;
  hour=time/10000;
  tmp=time-hour*10000;
  min=tmp/100;
  sec=tmp-min*100;

  set(year,mon,mday,hour,min,sec,subsecond);
}

// ---------------------------------------------------------------------------

long AhDateTime::year() const {
  return m_datetime.tm_year+1900;
}

// ---------------------------------------------------------------------------

long AhDateTime::month() const {
  return m_datetime.tm_mon+1;
}

// ---------------------------------------------------------------------------

long AhDateTime::day() const {
  return m_datetime.tm_mday;
}

// ---------------------------------------------------------------------------

long AhDateTime::hour() const {
  return m_datetime.tm_hour;
}

// ---------------------------------------------------------------------------

long AhDateTime::minute() const {
  return m_datetime.tm_min;
}

// ---------------------------------------------------------------------------

long AhDateTime::second() const {
  return m_datetime.tm_sec;
}

// ---------------------------------------------------------------------------

double AhDateTime::subsecond() const {
  return m_subsecond;
}

// ---------------------------------------------------------------------------

long AhDateTime::daysInYear() const {
  long out=0;
  for (long imonth=1; imonth < month(); imonth++) {
    out+=ahtime::daysInMonth(imonth,year());
  }
  out+=day();
  return out;
}

// ---------------------------------------------------------------------------

long AhDateTime::daysLeftInYear() const {
  long out=ahtime::daysInMonth(month())-day();
  for (long imonth=month()+1; imonth <= ahtime::MON_IN_YEAR; imonth++) {
    out+=ahtime::daysInMonth(imonth,year());
  }
  return out;
}

// ---------------------------------------------------------------------------

/// \internal
/// \note alternate implementation, could just compare string order
int AhDateTime::compare(const AhDateTime & dt) const {
  if (year() < dt.year()) return -1;
  if (year() > dt.year()) return +1;

  if (month() < dt.month()) return -1;
  if (month() > dt.month()) return +1;

  if (day() < dt.day()) return -1;
  if (day() > dt.day()) return +1;

  if (hour() < dt.hour()) return -1;
  if (hour() > dt.hour()) return +1;

  if (minute() < dt.minute()) return -1;
  if (minute() > dt.minute()) return +1;

  if (second() < dt.second()) return -1;
  if (second() > dt.second()) return +1;

  if (subsecond() < dt.subsecond()) return -1;
  if (subsecond() > dt.subsecond()) return +1;

  return 0;
}

// ---------------------------------------------------------------------------

std::string AhDateTime::getDateAsStr() const {
  std::stringstream out;
  out << std::setw(4) << std::setfill('0') << year();
  out << "-";
  out << std::setw(2) << std::setfill('0') << month();
  out << "-";
  out << std::setw(2) << std::setfill('0') << day();
  return out.str();
}

// ---------------------------------------------------------------------------

std::string AhDateTime::getTimeAsStr(int ndigits) const {
  std::stringstream out;
  out << std::setw(2) << std::setfill('0') << hour();
  out << ":";
  out << std::setw(2) << std::setfill('0') << minute();
  out << ":";
  out << std::setw(2) << std::setfill('0') << second();
  if (ndigits > 0) {
    out << "." << std::setw(ndigits) << std::setfill('0')
        <<getSubsecondAsInt(ndigits);
  }
  return out.str();
}

// ---------------------------------------------------------------------------

std::string AhDateTime::getDateTimeAsStr(int ndigits) const {
  return getDateAsStr()+"T"+getTimeAsStr(ndigits);
}

// ---------------------------------------------------------------------------

long AhDateTime::getDateAsInt() const {
  return 10000*year()+100*month()+day();
}

// ---------------------------------------------------------------------------

long AhDateTime::getTimeAsInt() const {
  return 10000*hour()+100*minute()+second();
}

// ---------------------------------------------------------------------------

long AhDateTime::getSubsecondAsInt(int ndigits) const {
  // round to the first ndigits of fractional subsecond
  long long out=(long long)floor(0.5+pow(10.,ndigits)*subsecond());
  if (out == (long long)pow(10.,ndigits)) out--;   // in case rounding yields 1 second
  return out;
}

// ---------------------------------------------------------------------------

long AhDateTime::_secIntoYear(double* subseconds) const {

  // include leap day?
  bool leap=false;
  if (isLeapYear(year())) {
    if (month() > 2) leap=true;
  }

  // number of days before current day
  long ndays=day()-1;
  for (long imon=1; imon < month(); imon++) {
    ndays+=daysInMonth(imon);
  }
  if (leap) ndays++;

  // prepare output
  if (subseconds != NULL) *subseconds=subsecond();
  long out=ndays*SEC_IN_DAY
          +hour()*SEC_IN_HOUR+minute()*SEC_IN_MIN+second();
  return out;
}

// ---------------------------------------------------------------------------

long AhDateTime::_secLeftInYear(double* subseconds) const {

  // include leap day?
  bool leap=false;
  if (isLeapYear(year())) leap=true;

  // use _secIntoYear() to get result
  long out=SEC_IN_YEAR;
  if (leap) out+=SEC_IN_DAY;
  out-=_secIntoYear(subseconds);
  if (subseconds != NULL && *subseconds > 0.0) {
    out--;
    *subseconds=1.0-*subseconds;
  }
  return out;
}

// ---------------------------------------------------------------------------

void AhDateTime::_addSeconds(long long seconds, double subseconds) {
  if (seconds < 0 || subseconds < 0.) 
    AH_THROW_RUNTIME("can only add positive seconds/subseconds");

  // add in subseconds
  m_subsecond+=subseconds;
  while (m_subsecond >= 1.0) {
    m_subsecond-=1.0;
    m_datetime.tm_sec++;
  }

  // break up whole number of seconds in parts
  long nsec, nmin, nhour, nday;
  secondsBreakUp(seconds,nsec,nmin,nhour,nday);

  // find change in number of seconds
  m_datetime.tm_sec+=nsec;
  if (m_datetime.tm_sec >= SEC_IN_MIN) {
    m_datetime.tm_sec-=SEC_IN_MIN;
    nmin++;
  }

  // find change in number of hours
  m_datetime.tm_min+=nmin;
  if (m_datetime.tm_min >= MIN_IN_HOUR) {
    m_datetime.tm_min-=MIN_IN_HOUR;
    nhour++;
  }

  // find change in number of days
  m_datetime.tm_hour+=nhour;
  if (m_datetime.tm_hour >= HOUR_IN_DAY) {
    m_datetime.tm_hour-=HOUR_IN_DAY;
    nday++;
  }
  m_datetime.tm_mday+=nday;

  // find change in number of months/years
  while (m_datetime.tm_mday > daysInMonth(m_datetime.tm_mon+1,
                                          m_datetime.tm_year+1900)) {
    m_datetime.tm_mday-=daysInMonth(m_datetime.tm_mon+1,
                                    m_datetime.tm_year+1900);
    m_datetime.tm_mon++;
    if (m_datetime.tm_mon+1 > MON_IN_YEAR) {
      m_datetime.tm_mon-=MON_IN_YEAR;
      m_datetime.tm_year++;
    }
  }

  // to avoid problems with rounding, subseconds close enough to zero will be
  // set to zero
  _adjustSubsecondNearZero();
}

// ---------------------------------------------------------------------------

void AhDateTime::_subtractSeconds(long long seconds, double subseconds) {
  if (seconds < 0 || subseconds < 0.) 
    AH_THROW_RUNTIME("can only subtract positive seconds/subseconds");

  // subtract subseconds
  m_subsecond-=subseconds;
  while (m_subsecond < 0.0) {
    m_subsecond+=1.0;
    m_datetime.tm_sec--;
  }

  // break up whole number of seconds in parts
  long nsec, nmin, nhour, nday;
  secondsBreakUp(seconds,nsec,nmin,nhour,nday);

  // get change in number of seconds
  m_datetime.tm_sec-=nsec;
  if (m_datetime.tm_sec < 0) {
    m_datetime.tm_sec+=SEC_IN_MIN;
    nmin++;        // increase the number to subtract
  }

  // get change in number of minutes
  m_datetime.tm_min-=nmin;
  if (m_datetime.tm_min < 0) {
    m_datetime.tm_min+=MIN_IN_HOUR;
    nhour++;
  }

  // get change in number of hours
  m_datetime.tm_hour-=nhour;
  if (m_datetime.tm_hour < 0) {
    m_datetime.tm_hour+=HOUR_IN_DAY;
    nday++;
  }

  // get change in rest
  m_datetime.tm_mday-=nday;
  while (m_datetime.tm_mday < 1) {
    m_datetime.tm_mon--;
    m_datetime.tm_mday+=daysInMonth(m_datetime.tm_mon+1,
                                    m_datetime.tm_year+1900);
    if (m_datetime.tm_mon+1 < 1) {
      m_datetime.tm_mon+=MON_IN_YEAR;
      m_datetime.tm_year--;
    }
  }
  if (m_datetime.tm_year < 0)
    AH_THROW_RUNTIME("subtraction results in negative time");

  // to avoid problems with rounding, subseconds close enough to zero will be
  // set to zero
  _adjustSubsecondNearZero();
}

// ---------------------------------------------------------------------------

void AhDateTime::_addOneSecondForce() {
  m_datetime.tm_sec++;
}

// ---------------------------------------------------------------------------

// note: using _addSeconds so that rounding is done properly (i.e. if going from
// 59 seconds to 0 second + minute increment; _addSeconds calls this function
// as well, but since m_subsecond is being set to zero here, there will be no
// infinite loop of calls
void AhDateTime::_adjustSubsecondNearZero() {
  if ((1.-m_subsecond) <= SUBSECOND_THRESHOLD) {
    m_subsecond=0.0;
    _addSeconds(1,0.);
  }
  if (m_subsecond < SUBSECOND_THRESHOLD) m_subsecond=0.0;
}

// ---------------------------------------------------------------------------

}

/* Revision Log
 $Log: AhDateTime.cxx,v $
 Revision 1.10  2014/09/10 02:41:19  mwitthoe
 ahtime library: move timfile CALDB library to ahmission; move leapsecond CALDB library from ahmission; update AhDateTime & AhMJDTime classes and functions in ahtimeconv to support new version of ahtimeconv

 Revision 1.9  2014/08/05 12:08:40  mwitthoe
 ahtime library: make a couple implicit type conversions explicit to avoid compiler error

 Revision 1.8  2013/12/04 16:23:34  mwitthoe
 ahtime library: fix bug in the new AhDateTime::set() function which includes subseconds with the datetime string; the previous version chopped off the last digit of the subsecond value

 Revision 1.7  2013/12/04 16:09:22  mwitthoe
 ahtime library: add ability to set AhDateTime with a single date-time string including fractional seconds, i.e. YYYY-MM-DDThh:mm:ss.xxxxxx

 Revision 1.6  2013/10/15 17:51:49  mwitthoe
 ahtime library: add function to the AhDateTime class, _adjustSubsecondNearZero, which will look for a subsecond count near to 0. or 1. and adjust the time so that the subsecond is exactly zero; this helps to make sure that the integer number of seconds is consistent across different architectures

 Revision 1.5  2013/09/18 14:09:13  mwitthoe
 ahtime library: add classes to store times in the DateTime and MJD formats


*/
