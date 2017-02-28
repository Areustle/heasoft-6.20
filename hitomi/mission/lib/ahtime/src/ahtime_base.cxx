/// \file ahtime_base.cxx
/// \brief General time constants and functions.
/// \author Mike Witthoeft
/// \date $Date: 2014/09/10 02:41:19 $
 
#define AHLABEL ahtime_ahtime_base
#define AHCVSID "$Id: ahtime_base.cxx,v 1.21 2014/09/10 02:41:19 mwitthoe Exp $"

#include "ahtime/ahtime_base.h"
#include "ahlog/ahlog.h"

#include <stdlib.h>
#include <stdio.h>
#include <cmath>

namespace ahtime {

// ---------------------------------------------------------------------------

bool isNumber(const std::string& val) {
  std::string::const_iterator it=val.begin();
  while (it != val.end() && std::isdigit(*it)) ++it;
  return !val.empty() && it == val.end();
}

// ---------------------------------------------------------------------------

void parseDateString(const std::string& datestr, long& year, long& month,
                     long& day) {

  // Try format: YYYY-MM-DD
  if (datestr.size() == 10) {
    bool bad=false;
    if (!ahtime::isNumber(datestr.substr(0,4))) bad=true;     // year
    if (!ahtime::isNumber(datestr.substr(5,2))) bad=true;     // month
    if (!ahtime::isNumber(datestr.substr(8,2))) bad=true;     // day

    if (!bad) {
      year=atoi(datestr.substr(0,4).c_str());
      month=atoi(datestr.substr(5,2).c_str());
      day=atoi(datestr.substr(8,2).c_str());
      return;
    }
  }

  // try format: YYYY:DDD
  if (datestr.size() == 8) {
    bool bad=false;
    if (!ahtime::isNumber(datestr.substr(0,4))) bad=true;     // year
    if (!ahtime::isNumber(datestr.substr(5,3))) bad=true;     // day number

    if (!bad) {
      year=atoi(datestr.substr(0,4).c_str());
      long daynumber=atoi(datestr.substr(5,3).c_str());
      ahtime::convertDayNumberIntoMonthAndDay(daynumber,year,month,day);
      return;
    }
  }

  AH_THROW_RUNTIME("date format not understood");

}

// ---------------------------------------------------------------------------

void parseTimeString(const std::string& timestr, long& hour, long& minute,
                     double& second) {

  // Try format: hh:mm:ss.xxxx
  if (timestr.size() >= 8) {           // minimum length is 8 characters
    long ndecimal=timestr.size()-8;     // number of decimal digits for second
    bool bad=false;
    if (!ahtime::isNumber(timestr.substr(0,2))) bad=true;    // hour
    if (!ahtime::isNumber(timestr.substr(3,2))) bad=true;    // minute
    if (!ahtime::isNumber(timestr.substr(6,2))) bad=true;    // second
    if (ndecimal > 2) {       // fractional second present
      if (timestr.substr(8,1) != ".")
        bad=true;
      else
        if (!ahtime::isNumber(timestr.substr(9,ndecimal-1))) bad=true;
    }

    if (!bad) {
      hour=atoi(timestr.substr(0,2).c_str());
      minute=atoi(timestr.substr(3,2).c_str());
      second=atof(timestr.substr(6,2+ndecimal).c_str());
      return;
    }
  }

  AH_THROW_RUNTIME("time format not understood");

}

// ---------------------------------------------------------------------------

void parseDateTimeString(const std::string& dt, long& year, long& month,
                         long& day, long& hour, long& minute, double& second) {

  // Try format: YYYY-MM-DDThh:mm:ss.xxxx
  if (dt.size() >= 19) {          // minimum length is 19 characters
    long ndecimal=dt.size()-19;    // number of decimal digits for second
    bool bad=false;
    try {
      ahtime::parseDateString(dt.substr(0,10),year,month,day);
      ahtime::parseTimeString(dt.substr(11,8+ndecimal),hour,minute,second);
    } catch (...) {
      bad=true;
    }
    if (!bad) return;
  }

  // try format: YYYY:DDD:hh:mm:ss.xxxx
  if (dt.size() >= 17) {          // minimum length is 19 characters
    long ndecimal=dt.size()-17;    // number of decimal digits for second
    bool bad=false;
    try {
      ahtime::parseDateString(dt.substr(0,8),year,month,day);
      ahtime::parseTimeString(dt.substr(9,8+ndecimal),hour,minute,second);
    } catch (...) {
      bad=true;
    }
    if (!bad) return;
  }

  AH_THROW_RUNTIME("date/time format not understood");

}

// ---------------------------------------------------------------------------

bool isLeapYear(long year) {
  if (year%400 == 0) return true;
  if (year%100 == 0) return false;
  if (year%4 == 0) return true;
  return false;
} 

// ---------------------------------------------------------------------------

long daysInMonth(long imon) {
  switch (imon) {
    case 2:
      return 28;
    case 4:
    case 6:
    case 9:
    case 11:
      return 30;
  }
  return 31;
}

// ---------------------------------------------------------------------------

void secondsBreakUp(long long seconds, long & nsec, long & nmin, long & nhour,
                    long & nday) {

  // initialize output
  nsec=0;
  nmin=0;
  nhour=0;
  nday=0;

  // working variables
  long long tmp1,tmp2;

  // get nsec
  nsec=seconds%SEC_IN_MIN;
  tmp1=seconds/SEC_IN_MIN;     // number of minutes

  // get nmin
  nmin=tmp1%MIN_IN_HOUR;
  tmp2=tmp1/MIN_IN_HOUR;       // number of hours

  // get nhour
  nhour=tmp2%HOUR_IN_DAY;
  nday=tmp2/HOUR_IN_DAY;

}

// ---------------------------------------------------------------------------

long daysInMonth(long imon, long year) {
  long out=daysInMonth(imon);
  if (imon != 2) return out;
  if (isLeapYear(year)) out++;
  return out;
}

// ---------------------------------------------------------------------------

void convertDayNumberIntoMonthAndDay(long daynumber, long year, long& month,
                                     long& day) {

  day=daynumber;    // will decrease as months are subtracted
  month=1;          // start in january
  while (day > ahtime::daysInMonth(month,year)) {
    day-=ahtime::daysInMonth(month,year);
    month++;
    if (month > 12) break;
  }
  if (day > 31) AH_THROW_RUNTIME("too many days in year");
}

// ---------------------------------------------------------------------------

void convertMonthAndDayIntoDayNumber(long month, long day, long year, 
                                     long& daynumber) {
  daynumber=0;
  for (long imonth=1; imonth < month; imonth++) {
    daynumber+=ahtime::daysInMonth(imonth,year);
  }
  daynumber+=day;
}

// ---------------------------------------------------------------------------

std::string catTimeSubseconds(const std::string & time, 
                              double subseconds, 
                              long digits) {
  char tmp[99],fmt[9];
  sprintf(fmt,"%%.%if",(int)digits);
  sprintf(tmp,fmt,subseconds);
  std::string out=(std::string)tmp;
  out.erase(out.begin());  // get rid of leading zero
  return time+out;
}

// ---------------------------------------------------------------------------

bool uncatTimeSubseconds(const std::string & instr, std::string & time, 
                         double & subseconds) {
  size_t idx=instr.rfind(".");

  // no decimal point found
  if (idx == std::string::npos) {
    time=instr;
    subseconds=0.0;
    return false;
  }

  // split
  time=instr.substr(0,idx);
  subseconds=atof(instr.substr(idx).c_str());
  return true;
}

// ---------------------------------------------------------------------------

}

/* Revision Log
 $Log: ahtime_base.cxx,v $
 Revision 1.21  2014/09/10 02:41:19  mwitthoe
 ahtime library: move timfile CALDB library to ahmission; move leapsecond CALDB library from ahmission; update AhDateTime & AhMJDTime classes and functions in ahtimeconv to support new version of ahtimeconv

 Revision 1.20  2013/01/19 17:29:54  mwitthoe
 change TI variable to unsigned int (using a typedef); change make_hktim.cxx to produce test data with proper range of L32TI; add conversion from U32TI to L32TI in ahtimeassign for relevant instruments

 Revision 1.19  2012/10/18 17:35:40  mwitthoe
 ahtime library: created single header file, ahtime.h, to include entire ahtime library (old ahtime -> ahtime_base); separated CAMS data from instrument delay CALDB file into new file: associated libraries created

 Revision 1.18  2012/09/14 17:32:25  mwitthoe
 change pow(int,int) with shift operator

 Revision 1.17  2012/09/13 18:16:58  mwitthoe
 clean up of ahtime library source files

 Revision 1.16  2012/09/13 17:58:59  mwitthoe
 implement new version method in the ahtime library

 Revision 1.15  2012/09/11 18:52:08  mwitthoe
 restructuring time assignment library to prepare for science data (not finished)

 Revision 1.14  2012/09/07 17:39:30  mwitthoe
 remove obsolete include to ahtest in ahtime source files

 Revision 1.13  2012/08/29 22:12:24  mwitthoe
 in ahtime library, move unit test functions from src to test

 Revision 1.12  2012/08/28 21:34:44  mwitthoe
 changes to ahtime library: clean up; change leap second value from double to int; fix memory leak

 Revision 1.11  2012/08/24 18:53:22  mwitthoe
 clean up argument lists in ahtime library

 Revision 1.10  2012/08/22 18:27:44  mwitthoe
 testing does not require ahgen.h anymore

 Revision 1.9  2012/08/22 15:13:38  mwitthoe
 finish converting ahtime library tests to new format

 Revision 1.8  2012/08/21 19:08:44  mwitthoe
 partial switchover to new testing regime to see if ahtest works

 Revision 1.7  2012/08/17 20:43:18  mwitthoe
 apply standards to ahtime library

 Revision 1.6  2012/08/15 16:11:53  mwitthoe
 fix to ahfits, ahlog, ahtime versioning

 Revision 1.5  2012/08/15 15:24:13  mwitthoe
 add versioning macros to ahtime header and source files

 Revision 1.4  2012/07/13 18:32:08  mwitthoe
 clean up ahtime code


*/
