/// \file ahtimeconv.cxx
/// \brief Functions to act on time objects: UTC, TT, and TAI.
/// \author Mike Witthoeft
/// \date $Date: 2014/09/11 20:13:16 $
 
#define AHLABEL ahtime_ahtimeconv
#define AHCVSID "$Id: ahtimeconv.cxx,v 1.28 2014/09/11 20:13:16 mwitthoe Exp $"

#include "ahlog/ahlog.h"
#include "ahtime/ahtimeconv.h"

#include <cmath>
#include <iomanip>

namespace ahtime {

// ---------------------------------------------------------------------------

long numLeapSecInInterval(const AhDateTime & t1, const AhDateTime & t2,
                          ahtime::leapsec::LeapSecTable & leapsecdat) {

  // Note: leapsecdat contains two leap second tables.  The first maps the
  // calendar date to leap seconds and the second maps an MJD-formatted date
  // to leap seconds.  This function uses the first map: leapsecdat.cal.

  if (leapsecdat.cal.size() == 0) AH_THROW_RUNTIME("no leap second data loaded!");
  long nleap=0;
  ahtime::leapsec::LeapSecCal::iterator it;   // iterator over map of calendar date to leap seconds
  // note: it->first is the date as an integer and it->second is the number of leap seconds
  for (it=leapsecdat.cal.begin(); it != leapsecdat.cal.end(); it++) {
    if (t1.getDateAsInt() >= it->first) continue;
    if (t2.getDateAsInt() < it->first) continue;
    nleap+=it->second;
  }

  // if first time is before 1972-01-01, add 10s
  if (t1.year() < 1972) nleap+=10;

  return nleap;
}

// ---------------------------------------------------------------------------

long numLeapSecInInterval(const AhMJDTime & t1, const AhMJDTime & t2,
                          ahtime::leapsec::LeapSecTable & leapsecdat) {

  // Note: leapsecdat contains two leap second tables.  The first maps the
  // calendar date to leap seconds and the second maps an MJD-formatted date
  // to leap seconds.  This function uses the second map: leapsecdat.mjd.

  if (leapsecdat.mjd.size() == 0) AH_THROW_RUNTIME("no leap second data loaded!");
  long nleap=0;
  ahtime::leapsec::LeapSecMJD::iterator it;   // iterator over map of MJD date to leap seconds
  // note: it->first is the date as an integer and it->second is the number of leap seconds
  for (it=leapsecdat.mjd.begin(); it != leapsecdat.mjd.end(); it++) {
    if (t1.mjd() >= it->first) continue;
    if (t2.mjd() < it->first) continue;
    nleap+=it->second;
  }

  // if first time is before 1972-01-01, add 10s
  if (t1.mjd() < 41317.) nleap+=10;   // 41317 MJD = Jan 1, 1972

  return nleap;
}

// ---------------------------------------------------------------------------

long numLeapSecBefore(const AhDateTime & time,
                     ahtime::leapsec::LeapSecTable & leapsecdat) {

  // Note: leapsecdat contains two leap second tables.  The first maps the
  // calendar date to leap seconds and the second maps an MJD-formatted date
  // to leap seconds.  This function uses the first map: leapsecdat.cal.

  if (leapsecdat.cal.size() == 0) AH_THROW_RUNTIME("no leap second data loaded!");
  long nleap=0;
  ahtime::leapsec::LeapSecCal::iterator it;   // iterator over map of calendar date to leap seconds
  // note: it->first is the date as an integer and it->second is the number of leap seconds
  for (it=leapsecdat.cal.begin(); it != leapsecdat.cal.end(); it++) {
    if (time.getDateAsInt() < it->first) continue;
    nleap+=it->second;
  }
  nleap+=10;      // 10s shift at 1972-01-01
  return nleap;
}

// ---------------------------------------------------------------------------

long numLeapSecBefore(const AhMJDTime & time,
                      ahtime::leapsec::LeapSecTable & leapsecdat) {

  // Note: leapsecdat contains two leap second tables.  The first maps the
  // calendar date to leap seconds and the second maps an MJD-formatted date
  // to leap seconds.  This function uses the second map: leapsecdat.mjd.

  if (leapsecdat.mjd.size() == 0) AH_THROW_RUNTIME("no leap second data loaded!");
  long nleap=0;
  ahtime::leapsec::LeapSecMJD::iterator it;   // iterator over map of MJD date to leap seconds
  // note: it->first is the date as an integer and it->second is the number of leap seconds
  for (it=leapsecdat.mjd.begin(); it != leapsecdat.mjd.end(); it++) {
    if (time.mjd() < it->first) continue;
    nleap+=it->second;
  }
  nleap+=10;      // 10s shift at 1972-01-01 (41317 MJD)
  return nleap;
}

// ---------------------------------------------------------------------------

long long numSecInInterval(const AhDateTime & t1, const AhDateTime & t2,
                           double* subseconds) {

  // require that t2 > t1; check for t1 == t2
  int x=t1.compare(t2);
  if (x > 0) {
    AH_THROW_RUNTIME("first time is larger than second");
  } else if (x == 0) {
    if (subseconds != NULL) *subseconds=0.0;
    return 0;
  }

  // initialize output number of seconds
  long long out=0;

  // get number of seconds included in first and last years
  double sub_first, sub_last;
  long nsec_first=t1._secLeftInYear(&sub_first);
  long nsec_last=t2._secIntoYear(&sub_last);
  out+=nsec_first+nsec_last;

  // take care of sub-seconds
  double tsub=sub_first+sub_last;
  while (tsub > 1.0) {
    tsub-=1.0;
    out++;
  }

  // assign output subseconds if requested
  if (subseconds != NULL) *subseconds=tsub;

  // count rest of seconds
  if (t1.year() == t2.year()) {
    out-=SEC_IN_YEAR;
    if (isLeapYear(t1.year())) out-=SEC_IN_DAY;
  } else {
    for (long iyear=t1.year()+1; iyear < t2.year(); iyear++) {
      out+=SEC_IN_YEAR;
      if (isLeapYear(iyear)) out+=SEC_IN_DAY;
    }
  }
  return out;
}

// ---------------------------------------------------------------------------

long long numSecInInterval(const AhMJDTime & t1, const AhMJDTime & t2,
                           double* subseconds) {

  // require that t2 > t1; check for t1 == t2
  int x=t1.compare(t2);
  if (x > 0) {
    AH_THROW_RUNTIME("first time is larger than second");
  } else if (x == 0) {
    if (subseconds != NULL) *subseconds=0.0;
    return 0;
  }

  // calculation will separate whole days and the fractional days
  // until the last step to preserve precision
  long long wholeday=t2.mjdi()-t1.mjdi();    // whole days
  double subday=t2.mjdf()-t1.mjdf();         // fractional days

  // convert days into seconds
  long long out=wholeday*ahtime::SEC_IN_DAY;   // whole number of seconds
  double tsub=subday*ahtime::SEC_IN_DAY;       // fractional number of seconds
  out+=(long long)tsub;                        // add whole seconds from subday
  tsub=tsub-(long long)tsub;                   // left over sub seconds (< 1)

  // ensure that subsecond value is > 0
  if (tsub < 0.) {
    out--;
    tsub+=1.;
  }

  // assign output subseconds if requested
  if (subseconds != NULL) *subseconds=tsub;

  return out;

}

// ---------------------------------------------------------------------------

long long numSecInUTCInterval(const AhDateTime & t1, const AhDateTime & t2, 
                            ahtime::leapsec::LeapSecTable & leapsecdat,
                            double* subseconds) {

  // get time difference without leap seconds
  long long out=ahtime::numSecInInterval(t1,t2,subseconds);

  // add number of leap seconds in range
  out+=numLeapSecInInterval(t1,t2,leapsecdat);

  return out;
}

// ---------------------------------------------------------------------------

long long numSecInUTCInterval(const AhMJDTime & t1, const AhMJDTime & t2, 
                            ahtime::leapsec::LeapSecTable & leapsecdat,
                            double* subseconds) {

  // get time difference without leap seconds
  long long out=ahtime::numSecInInterval(t1,t2,subseconds);

  // add number of leap seconds in range
  out+=numLeapSecInInterval(t1,t2,leapsecdat);

  return out;
}

// ---------------------------------------------------------------------------

void reformatSecondsAsMJD(double seconds, const AhMJDTime& epoch,
                          AhMJDTime& mjd) {

  double days=seconds/SEC_IN_DAY;
  ahtime::reformatDaysAsMJD(days,epoch,mjd);
}

// ---------------------------------------------------------------------------

void reformatSecondsAsDateTime(double seconds, const AhDateTime& epoch,
                               AhDateTime& dt) {

  long long wholesec=(long long)seconds;
  double subseconds=seconds-wholesec;
  dt=epoch;
  dt._addSeconds(wholesec,subseconds);
}

// ---------------------------------------------------------------------------

void reformatMJDAsSeconds(const AhMJDTime& mjd, const AhMJDTime& epoch,
                          double& seconds) {

  double days=0.;
  ahtime::reformatMJDAsDays(mjd,epoch,days);
  seconds=SEC_IN_DAY*days;
}

// ---------------------------------------------------------------------------

void reformatDateTimeAsSeconds(const AhDateTime& dt, const AhDateTime& epoch,
                               double& seconds) {

  double subseconds=0.;
  long long wholesec=numSecInInterval(epoch,dt,&subseconds);
  seconds=(double)wholesec+subseconds;
}

// ---------------------------------------------------------------------------

void reformatDaysAsMJD(double days, const AhMJDTime& epoch, AhMJDTime& mjd) {

  long mjdi=epoch.mjdi()+(long)days;
  double mjdf=epoch.mjdf()+(days-(long)days);
  while (mjdf >= 1.) {
    mjdi++;
    mjdf-=1.;
  }
  mjd.set(mjdi,mjdf);
}

// ---------------------------------------------------------------------------

void reformatDaysAsDateTime(double days, const AhDateTime& epoch,
                            AhDateTime& dt) {

  double seconds=days*ahtime::SEC_IN_DAY;
  ahtime::reformatSecondsAsDateTime(seconds,epoch,dt);
}

// ---------------------------------------------------------------------------

void reformatMJDAsDays(const AhMJDTime& mjd, const AhMJDTime& epoch,
                       double& days) {
  long dmjdi=mjd.mjdi()-epoch.mjdi();
  double dmjdf=mjd.mjdf()-epoch.mjdf();
  days=(double)dmjdi+dmjdf;
}

// ---------------------------------------------------------------------------

void reformatDateTimeAsDays(const AhDateTime& dt, const AhDateTime& epoch,
                            double& days) {

  double seconds=0.;
  ahtime::reformatDateTimeAsSeconds(dt,epoch,seconds);
  days=seconds/ahtime::SEC_IN_DAY;
}

// ---------------------------------------------------------------------------

void reformatJDAsMJD(double jd, AhMJDTime& mjd) {
  mjd.set(jd-2400000.5);
}

// ---------------------------------------------------------------------------

void reformatJDAsDateTime(double jd, AhDateTime& dt) {
  AhMJDTime mjd;
  ahtime::reformatJDAsMJD(jd,mjd);
  ahtime::reformatMJDAsDateTime(mjd,dt);
}

// ---------------------------------------------------------------------------

void reformatMJDAsJD(const AhMJDTime& mjd, double& jd) {
  jd=mjd.mjd()+2400000.5;
}

// ---------------------------------------------------------------------------

void reformatDateTimeAsJD(const AhDateTime& dt, double& jd) {
  AhMJDTime mjd;
  ahtime::reformatDateTimeAsMJD(dt,mjd);
  ahtime::reformatMJDAsJD(mjd,jd);
}

// ---------------------------------------------------------------------------

void reformatMJDAsDateTime(const AhMJDTime& mjd, AhDateTime& dt) {

  // get Julian Day number from MJD date portion
  // 2400001 = 240000.5 + 0.5 (extra 0.5 since incoming date is relative to
  //                           midnight, not noon)
  long jd=mjd.mjdi()+2400001;
  
  // get date portion of MJD time
  // from Seidelmann, P. K., ed. 1992, Explanatory Supplement to the
  // Astronomical Almanac (Mill Valley, CA: University Science Books), p. 604
  long ll=jd+68569;
  long nn=(4*ll)/146097;
  ll=ll-(146097*nn+3)/4;
  long ii=(4000*(ll+1))/1461001;
  ll=ll-(1461*ii)/4+31;
  long jj=(80*ll)/2447;
  long day=ll-(2447*jj)/80;
  ll=jj/11;
  long month=jj+2-12*ll;
  long year=100*(nn-49)+ii+ll;

  // get time part
  long totalsec=(long)(SEC_IN_DAY*mjd.mjdf());            // total seconds into day
  double subsec=SEC_IN_DAY*mjd.mjdf()-(double)totalsec;   // remaining fractional second
  long hour=totalsec/SEC_IN_HOUR;
  long leftover=totalsec-SEC_IN_HOUR*hour;
  long minute=leftover/SEC_IN_MIN;
  long second=leftover-SEC_IN_MIN*minute;

  // set DateTime values
  dt.set(year,month,day,hour,minute,second,subsec);
}

// ---------------------------------------------------------------------------

void reformatDateTimeAsMJD(const AhDateTime& dt, AhMJDTime& mjd) {
  // convert date part to Julian Day number
  // from Seidelmann, P. K., ed. 1992, Explanatory Supplement to the
  // Astronomical Almanac (Mill Valley, CA: University Science Books), p. 604

  long jd=(1461*(dt.year()+4800-(14-dt.month())/12))/4+\
          (367*(dt.month()-2+12*((14-dt.month())/12)))/12-\
          (3*((dt.year()+4900-(14-dt.month())/12)/100))/4+\
          dt.day()-32075;

  // convert JD to MJD
  // 2400001 = 240000.5 + 0.5 (extra 0.5 since incoming date is relative to
  //                           midnight, not noon)
  long mjdi=jd-2400001;

  // if given time is 23:59:60 (indicating a leap second), set time manually
  double mjdf=0.;
  if (23 == dt.hour() && 59 == dt.minute() && 60 == dt.second()) {
    mjdi+=1;
    mjdf=0.;
  } else {    // normal operation
    mjdf=(SEC_IN_HOUR*dt.hour()+SEC_IN_MIN*dt.minute()+dt.second()+\
                 dt.subsecond())/SEC_IN_DAY;
    if (mjdf == 1.) {   // mjdf must be less than 1
      mjdi++;
      mjdf=0.;
    }
    if (mjdf < 0. || mjdf >= 1.) AH_THROW_RUNTIME("invalid time given: "+dt.getTimeAsStr());
  }

  // assign time to output
  mjd.set(mjdi,mjdf);
}

// ---------------------------------------------------------------------------

void convertMissionTimeToUTC(double mt_in, const AhDateTime& utcepoch, 
                             ahtime::leapsec::LeapSecTable & leapsecdat,
                             AhDateTime& dt_out) {

  // add seconds to epoch, store in dt_out
  dt_out=utcepoch;
  long long whole_seconds=(long long)mt_in;
  double subseconds=mt_in-(double)whole_seconds;
  dt_out._addSeconds(whole_seconds,subseconds);

  // get number of leap seconds between epoch and intermediate dt_out and
  // adjust output UTC time
  int nleap=numLeapSecInInterval(utcepoch,dt_out,leapsecdat);
  if (nleap > 0) dt_out._subtractSeconds(nleap,0.0);

  // check difference again, and add one leap second back as necessary
  int nleap2=numLeapSecInInterval(utcepoch,dt_out,leapsecdat);
  if (nleap2 != nleap) dt_out._addOneSecondForce();
}

// ---------------------------------------------------------------------------

void convertMissionTimeToUTC(double mt_in, const AhMJDTime& utcepoch, 
                             ahtime::leapsec::LeapSecTable & leapsecdat,
                             AhMJDTime& mjd_out) {

  // initialize output as the epoch and add input number of seconds
  mjd_out=utcepoch;
  mjd_out.addSeconds(mt_in);

  // get number of leap seconds between epoch and intermediate dt_out and
  // adjust output UTC time
  int nleap=numLeapSecInInterval(utcepoch,mjd_out,leapsecdat);
  if (nleap > 0) mjd_out.subtractSeconds((double)nleap);

  // check difference again, and add one leap second back as necessary
  int nleap2=numLeapSecInInterval(utcepoch,mjd_out,leapsecdat);
  if (nleap2 != nleap) mjd_out.addSeconds(1.);
}

// ---------------------------------------------------------------------------

void convertUTCToMissionTime(const AhDateTime& dt_in, const AhDateTime& utcepoch, 
                             ahtime::leapsec::LeapSecTable & leapsecdat,
                             double& mt_out) {

  long long sec_whole=0;      // whole number of seconds
  double sec_frac=0.;         // fractional number of seconds
  sec_whole=ahtime::numSecInUTCInterval(utcepoch,dt_in,leapsecdat,&sec_frac);
  mt_out=(double)sec_whole+sec_frac;
}

// ---------------------------------------------------------------------------

void convertUTCToMissionTime(const AhMJDTime& mjd_in, const AhMJDTime& utcepoch, 
                             ahtime::leapsec::LeapSecTable & leapsecdat,
                             double& mt_out) {

  long long sec_whole=0;      // whole number of seconds
  double sec_frac=0.;         // fractional number of seconds
  sec_whole=ahtime::numSecInUTCInterval(utcepoch,mjd_in,leapsecdat,&sec_frac);
  mt_out=(double)sec_whole+sec_frac;
}

// ---------------------------------------------------------------------------

void convertUTCToTT(const AhDateTime& dt_in, 
                    ahtime::leapsec::LeapSecTable & leapsecdat, 
                    AhDateTime& dt_out) {

  // the difference between UTC and TT is 32.184s + # of leapseconds
  int nleap=numLeapSecBefore(dt_in,leapsecdat);
  long long adj_whole=nleap+32;
  double adj_sub=0.184;

  dt_out=dt_in;
  dt_out._addSeconds(adj_whole,adj_sub);
}

// ---------------------------------------------------------------------------

void convertUTCToTT(const AhMJDTime& mjd_in, 
                    ahtime::leapsec::LeapSecTable & leapsecdat, 
                    AhMJDTime& mjd_out) {

  AhDateTime dt1, dt2;
  ahtime::reformatMJDAsDateTime(mjd_in,dt1);
  ahtime::convertUTCToTT(dt1,leapsecdat,dt2);
  ahtime::reformatDateTimeAsMJD(dt2,mjd_out);
}

// ---------------------------------------------------------------------------

void convertTTToUTC(const AhDateTime& dt_in, 
                    ahtime::leapsec::LeapSecTable & leapsecdat, 
                    AhDateTime& dt_out) {

  // the difference between UTC and TT is 32.184s + # of leapseconds
  int nleap=numLeapSecBefore(dt_in,leapsecdat);
  long adj_whole=nleap+32;
  double adj_sub=0.184;

  // apply preliminary adjustment
  dt_out=dt_in;
  dt_out._subtractSeconds(adj_whole,adj_sub);

  // check difference again, and add one leap second back as necessary
  int nleap2=numLeapSecBefore(dt_out,leapsecdat);
  if (nleap2 != nleap) dt_out._addOneSecondForce();
}

// ---------------------------------------------------------------------------

void convertTTToUTC(const AhMJDTime& mjd_in, 
                    ahtime::leapsec::LeapSecTable & leapsecdat, 
                    AhMJDTime& mjd_out) {

  // the difference between UTC and TT is 32.184s + # of leapseconds
  int nleap=numLeapSecBefore(mjd_in,leapsecdat);
  double adj_sec=32.184+nleap;

  // apply preliminary adjustment
  mjd_out=mjd_in;
  mjd_out.subtractSeconds(adj_sec);

  // check difference again, and add one leap second back as necessary
  int nleap2=numLeapSecBefore(mjd_out,leapsecdat);
  if (nleap2 != nleap) mjd_out.addSeconds(1.);

  // intermediate AhDateTime structures from which to do the conversion
  // (leap seconds are done via AhDateTime)
//  ahtime::AhDateTime dt_tt, dt_utc;
//  ahtime::reformatMJDAsDateTime(mjd_in,dt_tt);
//  ahtime::convertTTToUTC(dt_tt,leapsecdat,dt_utc);
//  ahtime::reformatDateTimeAsMJD(dt_utc,mjd_out);
}

// ---------------------------------------------------------------------------

void convertMissionTimeToTT(double mt_in, const AhDateTime& ttepoch,
                            AhDateTime& dt_out) {

  // add seconds to epoch, store in dt_out
  dt_out=ttepoch;
  long long whole_seconds=(long long)mt_in;
  double subseconds=mt_in-(double)whole_seconds;
  dt_out._addSeconds(whole_seconds,subseconds);
}

// ---------------------------------------------------------------------------

void convertMissionTimeToTT(double mt_in, const AhMJDTime& ttepoch, 
                            AhMJDTime& mjd_out) {

  // initialize output as the epoch and add input number of seconds
  mjd_out=ttepoch;
  mjd_out.addSeconds(mt_in);
}

// ---------------------------------------------------------------------------

void convertTTToMissionTime(const AhDateTime& dt_in, const AhDateTime& ttepoch,
                            double& mt_out) {

  long long sec_whole=0;      // whole number of seconds
  double sec_frac=0.;         // fractional number of seconds
  sec_whole=ahtime::numSecInInterval(ttepoch,dt_in,&sec_frac);
  mt_out=(double)sec_whole+sec_frac;
}

// ---------------------------------------------------------------------------

void convertTTToMissionTime(const AhMJDTime& mjd_in, const AhMJDTime& ttepoch, 
                            double& mt_out) {

  long long sec_whole=0;      // whole number of seconds
  double sec_frac=0.;         // fractional number of seconds
  sec_whole=ahtime::numSecInInterval(ttepoch,mjd_in,&sec_frac);
  mt_out=(double)sec_whole+sec_frac;
}

// ---------------------------------------------------------------------------

std::string calcDateObs(double time, const AhDateTime & epoch,
                        ahtime::leapsec::LeapSecTable & leapsecdat) {
  ahtime::AhDateTime tutc;
  ahtime::convertMissionTimeToUTC(time,epoch,leapsecdat,tutc);
  return ahtime::catTimeSubseconds(tutc.getDateTimeAsStr(),tutc.subsecond(),6);
}

// ---------------------------------------------------------------------------

void getDeltaCalPieces(const ahtime::AhDateTime& start,
                       const ahtime::AhDateTime& stop,
                       int& dyear, int& dmonth, int& dday, int& dhour,
                       int& dminute, int& dsecond, double& dsubsecond) {

  // require that stop > start
  if (stop.compare(start) < 0) {
    AH_THROW_LOGIC("cannot decompose difference between calendar times; start <= stop");
  } else if (stop.compare(start) == 0) {
    dyear=0;
    dmonth=0;
    dday=0;
    dhour=0;
    dminute=0;
    dsecond=0;
    dsubsecond=0.;
  }

  bool dyearset=false;
  bool dmonthset=false;
  bool ddayset=false;
  bool dhourset=false;
  bool dminuteset=false;
  bool dsecondset=false;
  bool dsubsecondset=false;

  while (1) {

    if (!dyearset) { dyear=stop.year()-start.year(); dyearset=true; }
    if (!dmonthset) { dmonth=stop.month()-start.month(); dmonthset=true; }
    if (dmonth < 0) {
      dyear--;
      dmonth+=ahtime::MON_IN_YEAR;
      continue;
    }
    if (!ddayset) { dday=stop.day()-start.day(); ddayset=true; }
    if (dday < 0) {
      dmonth--;
      dday+=daysInMonth(start.month()+dmonth,start.year()+dyear);
      continue;
    }
    if (!dhourset) { dhour=stop.hour()-start.hour(); dhourset=true; }
    if (dhour < 0) {
      dday--;
      dhour+=ahtime::HOUR_IN_DAY;
      continue;
    }
    if (!dminuteset) { dminute=stop.minute()-start.minute(); dminuteset=true; }
    if (dminute < 0) {
      dhour--;
      dminute+=ahtime::MIN_IN_HOUR;
      continue;
    }
    if (!dsecondset) { dsecond=stop.second()-start.second(); dsecondset=true; }
    if (dsecond < 0) {
      dminute--;
      dsecond+=ahtime::SEC_IN_MIN;
      continue;
    }
    if (!dsubsecondset) { dsubsecond=stop.subsecond()-start.subsecond(); dsubsecondset=true; }
    if (dsubsecond < 0.) {
      dsecond--;
      dsubsecond+=1.;
      continue;
    }
    break;
  }
}

// ---------------------------------------------------------------------------

}

/* Revision Log
 $Log: ahtimeconv.cxx,v $
 Revision 1.28  2014/09/11 20:13:16  mwitthoe
 ahtime library: remove some debugging print statements

 Revision 1.27  2014/09/11 14:02:21  mwitthoe
 ahtime library: add reformatting functions to go back and forth from AhDateTime to seconds, days, and JD

 Revision 1.26  2014/09/10 20:49:42  mwitthoe
 ahtime library: when reformatting from DateTime to MJD check if MJDF value is exactly equal to one, in which case set it to zero and add one to the MJDI value

 Revision 1.25  2014/09/10 02:41:19  mwitthoe
 ahtime library: move timfile CALDB library to ahmission; move leapsecond CALDB library from ahmission; update AhDateTime & AhMJDTime classes and functions in ahtimeconv to support new version of ahtimeconv

 Revision 1.24  2014/08/05 12:08:40  mwitthoe
 ahtime library: make a couple implicit type conversions explicit to avoid compiler error

 Revision 1.23  2013/10/14 17:29:43  mwitthoe
 ahtime library: remove references to obsolete time classes: AhTimeUTC, AhTimeTT, and AhTimeTAI; change time conversion routines to use new version of leap second structure (see ahmission:leapsec); in make_hktim.cxx, change S_TIME column to TIME and add STATUS column in TIM file

 Revision 1.22  2013/09/19 14:01:33  mwitthoe
 ahtime library: added function to convert from UTC to Mission time in DateTime format; added function to convert from UTC to TT in MJD format; these functions were needed for the new version of the ahtimeconv tool

 Revision 1.21  2013/09/17 19:53:49  mwitthoe
 ahtime library: add function, convertTTToUTC(), to directly convert TT to UTC using times in the MJD format; this function is overloaded with the existing one using AhDateTime arguments

 Revision 1.20  2013/09/17 19:28:05  mwitthoe
 ahtime library: change how time conversion/reformatting is done between time systems and formats (see issue 290 for details); the old method is still intact, but can be removed once all tools/libraries have been switched over to the new method

 Revision 1.19  2013/04/18 20:56:27  mwitthoe
 add function to ahtime library which will calculate the DATE_OBS or DATE_END FITS keyword value from the given TSTART/TSTOP value; the function name is ahtime::calcDateObs()

 Revision 1.18  2013/04/11 21:18:16  mwitthoe
 update ahtime library to use new version of leap second library

 Revision 1.17  2013/01/19 17:29:54  mwitthoe
 change TI variable to unsigned int (using a typedef); change make_hktim.cxx to produce test data with proper range of L32TI; add conversion from U32TI to L32TI in ahtimeassign for relevant instruments

 Revision 1.16  2012/09/13 18:16:58  mwitthoe
 clean up of ahtime library source files

 Revision 1.15  2012/09/13 17:58:59  mwitthoe
 implement new version method in the ahtime library

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

 Revision 1.8  2012/08/18 02:38:26  mwitthoe
 apply CALDB standards to ahleapsec library

 Revision 1.7  2012/08/17 20:43:18  mwitthoe
 apply standards to ahtime library

 Revision 1.6  2012/08/15 16:11:53  mwitthoe
 fix to ahfits, ahlog, ahtime versioning

 Revision 1.5  2012/08/15 15:24:13  mwitthoe
 add versioning macros to ahtime header and source files

 Revision 1.4  2012/08/01 15:57:25  mwitthoe
 use local leap second file for testahtime

 Revision 1.3  2012/07/13 18:32:08  mwitthoe
 clean up ahtime code


*/
