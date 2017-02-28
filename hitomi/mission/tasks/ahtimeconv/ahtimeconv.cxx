/// \file ahtimeconv.cxx
/// \brief Convert between UTC, Terrestrial Time and Mission Time with a
///  variety of format.
/// \author Mike Witthoeft
/// \date $Date: 2016/02/02 15:04:44 $
 
/** 

\defgroup tool_ahtimeconv Time Conversion (ahtimeconv)
@ingroup mod_mission_tasks

The ahtimeconv tool converts a single time from one system to another.  The 
supported systems are: Coordinated Universal Time (UTC), Terrestrial Time (TT),
and Mission Elapsed Time (MET). The latter is defined as a number of seconds
relative to an epoch (either UTC or TT).  Each time system can be represented
by the following formats: cal1 (YYYY-MM-DDThh:mm:ss), cal2 (YYYY:DDD:hh:mm:ss),
MJD, JD, seconds (from epoch), and days (from epoch).

This tool will accept a single time of the specified system and format and
convert the time to all supported formats of the provided output system.  These
output times are written to the log file.  In addition, the tool will write the
output time to the outtime parameter using the output format specified.  The
GPS time in seconds is always calculated and written to another output 
parameter.

There can be some confusion about the times represented by some combinations
of systems and formats.  The JD, MJD, and calendar formats do not rely on the
user-specified epoch.  The difference between the UTC and TT time systems in
these formats is always 32.184s + number of leap seconds.  However, the epoch
must be considered for the seconds and days formats.  Regardless of the output
system, the seconds format always reports the number of SI seconds elapsed 
since the epoch.  However, the number of days elapsed is system-dependent. 
In the TT system, a day is always 86400 seconds.  However, due to leap seconds,
the length of a UTC day can vary.  For example, the year 2012 had a single
leap second added (it is also a leap year).  In the UTC format, there are 366
days and 31622401 seconds.  In the TT format, there are still 31622401 seconds,
but since a TT day is always 86440s, the number of days becomes 366.000011574074
where the fractional part represents 1 second.

In order to increase the utility of this tool, the JD, MJD, and calendar formats
have been slightly redefined when the output system is MET (Mission Elapsed
Time).  Since MET is an elapsed time, the calendar format will give the number
of years, months, days, etc elapsed since the epoch (taken to be the start
time of the mission).  Similarly, the JD and MJD times will simply be the number
of days elapsed (1 day = 86400 seconds), meaning that the days, JD, and MJD
formats will have the same value.


\section Implementation notes

This tools works by converting the input time (regardless of system) to the
AhDateTime format (YYYY-MM-DDThh:mm:ss).  Conversion between systems is done
in this format, and then the output time is reformatted as needed.  Another
approach could be to use AhMJDTime as the intermediate format, but this has
less precision than AhDateTime.  In AhDateTime, the fraction of the second
less than 1 is stored in a double, whereas in AhMJDTime, the fraction of the
day less than 1 is stored in a double. 


\section ahtime_parfile Tool parameters

\verbatim

intime        , s, a, ""                      ,  ,  , "Input time"
insys         , s, a, "m"                     ,  ,  , "Input system ([M]ET, [T]T, [U]TC)"
inform        , s, a, "s"                     ,  ,  , "Input format ([s]ecs, mission [d]ays, [c]alendar, [j]ulian days, [m]odified julian days"
outsys        , s, a, "u"                     ,  ,  , "Output system ([M]ET, [T]T, [U]TC)"
outform       , s, a, "c"                     ,  ,  , "Output format ([s]ecs, mission [d]ays, [c]alendar, [j]ulian days, [m]odified julian days"
epochtime     , s, h, "2012-01-01T00:00:00"   ,  ,  , "Epoch time"
epochsys      , s, h, "u"                     ,  ,  , "Epoch system ([M]ET, [T]T, [U]TC)"
epochform     , s, h, "c"                     ,  ,  , "Epoch format ([s]ecs, mission [d]ays, [c]alendar, [j]ulian days, [m]odified julian days"
outtime       , s, hl, ""                     ,  ,  , "Output time in output system and format"
gpstime       , d, hl, ""                     ,  ,  , "Output time is GPS seconds"
leapsecfile   , s, h, "CALDB"                 ,  ,  , "Name of the leapsec file (or CALDB/REFDATA)"
numdigits     , i, h, 6                       , 0,  , "Number of decimal digits to use for sub-seconds (max: 18)"

\endverbatim

*/

#define AHLABEL tool_ahtimeconv
#define AHCVSID "$Id: ahtimeconv.cxx,v 1.37 2016/02/02 15:04:44 mwitthoe Exp $"

#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "ahapp/ahapp.h"
#include "ahlog/ahlog.h"
#include "ahtime/ahtime.h"
#include "ahmission/caldb.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <iostream>
#include <iomanip>     // std::setw, std::setfill
#include <cstdlib>     // std::strtod
#include <sstream>     // std::stringstream
#include <cmath>       // std::pow, std::min


/** \addtogroup tool_ahtimeconv
 *  @{
 */

struct Par {

  Par(): m_numdigits(0), m_gpstime(0.) {}        // constructor

  std::string m_intime;         // input time string
  std::string m_insys;          // time system of input time
  std::string m_inform;         // format of input time
  std::string m_outtime;        // output time string
  std::string m_outsys;         // time system of output time
  std::string m_outform;        // format of output time
  std::string m_epochtime;      // input time string
  std::string m_epochsys;       // time system of input time
  std::string m_epochform;      // format of input time
  std::string m_leapsecfile;    // name of leap second FITS file
  int m_numdigits;              // number of decimal digits to use for output seconds
  double m_gpstime;             // output GPS time in seconds
};


/// \brief Get parameter values
/// \param[out] par structure with parameter values
void getPar(Par& par);

/// \brief Read in leap second data.
/// \param[in] par structure with parameter values
/// \param[out] leapsecdat leap second data
void initialize(const Par& par, ahtime::leapsec::LeapSecTable & leapsecdat);

/// \brief Convert given time into other types
/// \param[in] par structure with parameter values
/// \param[in] leapsecdat leap second data
///
/// Note: the result of doWork() will be to update the values of the outtime
///       and gpstime parameters.
void doWork(Par& par, ahtime::leapsec::LeapSecTable & leapsecdat);

/// \brief Does nothing for this tool, but part of the standard tool layout.
void finalize();

/// \brief Check for proper format parameter and standard value.
/// \param[in] format value of format parameter
/// \return standardized value for input format parameter; empty string if bad
std::string getFormatFromParameter(const std::string& format);

/// \brief Check for proper system parameter and standard value.
/// \param[in] system value of system parameter
/// \return standardized value for input system parameter; empty string if bad
std::string getSystemFromParameter(const std::string& system);

/// \brief Convert a double to a string with the given number of decimal digits.
/// \param[in] value number to stringify
/// \param[in] numdigits number of decimal digits to keep
/// \return number as a string
///
/// Note: if the first numdigits digits of the decimal part of the value are
/// all zero, then the decimal portion is left out of the returned string.
/// For example, the tool will return "16" instead of "16.0000000".  The 
/// function does not perform any rounding.
std::string formatDoubleToPrecision(double value, int numdigits);

/// \brief Change a time quantity into the AhDateTime format.
/// \param[in] timestr time to reformat as a string
/// \param[in] format format of quantity in timestr
/// \param[in] epoch epoch of timestr; only needed if format is 's' or 'd'
/// \param[out] time quantity formatted as AhDateTime
void reformatToDateTime(const std::string& timestr, const std::string& format,
                        ahtime::AhDateTime* epoch, ahtime::AhDateTime* time);

/// \brief Convert a time from one system to another using AhDateTime as the
///  format.
/// \param[in] leapsecdat leap second table needed for converting between the
///  UTC and TT systems
/// \param[in] insys system to convert from
/// \param[in] intime time to be converted
/// \param[in] outsys system to convert to
/// \param[out] outtime final converted time
void convertDateTime(ahtime::leapsec::LeapSecTable & leapsecdat,
                     const std::string& insys, ahtime::AhDateTime* intime,
                     const std::string& outsys, ahtime::AhDateTime& outtime);

/// \brief Write a time as a string in the desired format.
/// \param[in] time time to be reformatted
/// \param[in] epoch epoch of time, only needed for output formats of seconds
///  or days
/// \param[in] format format of quantity to be written
/// \param[out] timestr string containing time in desired format
/// \param[in] numdigits number of decimal places to keep when outputting seconds;
///  for the days format, will keep this number + 6 digits
void reformatFromDateTime(ahtime::AhDateTime* time, ahtime::AhDateTime* epoch,
                          const std::string& format, std::string& timestr,
                          int numdigits);

/// \brief Convert a time to GPS time: seconds elapsed since 1980-01-06 UTC.
/// \param[in] leapsecdat leap second table needed if input is UTC
/// \param[in] insys system of input time
/// \param[in] intime time to convert to GPS
/// \param[out] gpstime number of seconds elapsed since GPS epoch
void convertToGPS(ahtime::leapsec::LeapSecTable & leapsecdat,
                  const std::string& insys, ahtime::AhDateTime* intime,
                  double& gpstime);


// ****************************************************************************

int main(int argc, char** argv) {

  Par par;                                      // container for parameters
  ahtime::AhDateTime epoch_utc;                 // epoch
  ahtime::leapsec::LeapSecTable leapsecdat;     // leap second data

  int status=ahapp::startUp(argc,argv,TOOLTAG);

  if (0 == status) {
    if (ahlog::get_debug()) {
      getPar(par);
      initialize(par,leapsecdat);
      doWork(par,leapsecdat);
      finalize();
      ahapp::shutDown();
    } else {
      try {
        getPar(par);
        initialize(par,leapsecdat);
        doWork(par,leapsecdat);
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize();
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
      try {
        ahapp::shutDown();
      } catch (const std::exception &x) {
        status=1;
        AH_ERR << "During shutdown: " << x.what() << std::endl;
      }
    }
  } else {
    std::cerr << "Unable to start up tool." << std::endl;
  }

  return status;
}

// ****************************************************************************

void getPar(Par& par) {

  // Declare variables
  bool badpar=false;       // flag if any parameter has a bad value

  // Read parameters
  par.m_intime=ahapp::getParString("intime");
  par.m_insys=getSystemFromParameter(ahapp::getParString("insys"));
  par.m_inform=getFormatFromParameter(ahapp::getParString("inform"));
  par.m_outsys=getSystemFromParameter(ahapp::getParString("outsys"));
  par.m_outform=getFormatFromParameter(ahapp::getParString("outform"));
  par.m_epochtime=ahapp::getParString("epochtime");
  par.m_epochsys=getSystemFromParameter(ahapp::getParString("epochsys"));
  par.m_epochform=getFormatFromParameter(ahapp::getParString("epochform"));
  par.m_leapsecfile=ahapp::getParString("leapsecfile");
  par.m_numdigits=ahapp::getParInt("numdigits");

  // Check parameter values.
  if (par.m_insys == "") {
    badpar=true;
    AH_OUT << "insys parameter has invalid value; must be M, T, or U" << std::endl;
  }
  if (par.m_inform == "") {
    badpar=true;
    AH_OUT << "inform parameter has invalid value; must be s, d, c1, c2, j, or m" << std::endl;
  }
  if (par.m_outsys == "") {
    badpar=true;
    AH_OUT << "outsys parameter has invalid value; must be M, T, or U" << std::endl;
  }
  if (par.m_outform == "") {
    badpar=true;
    AH_OUT << "outform parameter has invalid value; must be s, d, c1, c2, j, or m" << std::endl;
  }
  if (par.m_epochsys == "") {
    badpar=true;
    AH_OUT << "epochsys parameter has invalid value; must be M, T, or U" << std::endl;
  }
  if (par.m_epochform == "" || par.m_epochform == "s" || par.m_epochform == "d") {
    badpar=true;
    AH_OUT << "epochform parameter has invalid value; must be c1, c2, j, or m" << std::endl;
  }
  if (par.m_numdigits > 18) {
    badpar=true;
    AH_OUT << "numdigits cannot be more than 18 due to the range limit of the 'long long' data type" << std::endl;
  }
  if (badpar) AH_THROW_RUNTIME("invalid value for parameter");

  // Check for invalid combinations of input/output times.
  if (par.m_insys == "M") {
    if (par.m_inform != "s" && par.m_inform != "d")
      AH_THROW_RUNTIME("input MET times must be in (s)econds or (d)ays");
  }
}

// ****************************************************************************

void initialize(const Par& par, ahtime::leapsec::LeapSecTable & leapsecdat) {

  // Load leap second data
  std::string tmp=ahmission::caldb::resolve(par.m_leapsecfile, "leap second", "INS", "-", "LEAPSECS", "-", "-", "GEN");
  ahtime::leapsec::load(tmp,leapsecdat);
  // to record actual file path in par file, and in history keywords
  ape_trad_set_string("leapsecfile",tmp.c_str());

  // Write list of parameters to log file
  ahapp::writeParametersToLog(); 

}

// ****************************************************************************

void doWork(Par& par, ahtime::leapsec::LeapSecTable & leapsecdat) {

  // Declare variables
  bool outsys_is_met=false;           // flag if output system is MET
  ahtime::AhDateTime epoch_base;      // epoch time in epochsys
  ahtime::AhDateTime epoch_in;        // epoch time in insys
  ahtime::AhDateTime epoch_out;       // epoch time in outsys
  ahtime::AhDateTime time_in;         // time in input system
  ahtime::AhDateTime time_out;        // time converted to output system

  std::string time_sec;               // output time in seconds format
  std::string time_day;               // output time in days format
  std::string time_c1;                // output time in YYYY-MM-DDThh:mm:ss format
  std::string time_c2;                // output time in YYYY:DDD:hh:mm:ss format
  std::string time_jd;                // output time in JD format
  std::string time_mjd;               // output time in MJD format

  // Represent the time difference between the output time and the epoch
  // in the calendar format when the output system is MET:
  int dyear=0;                        // years elapsed
  int dmonth=0;                       // months elapsed
  int dday=0;                         // days elapsed
  int dhour=0;                        // hours elapsed
  int dminute=0;                      // minutes elapsed
  int dsecond=0;                      // seconds elapsed
  long long dsubsecint=0;             // subseconds elapsed as integer using numdigits parameter
  int dddd=0;                         // day of year elapsed 1-365(6)

  // If the output system is MET, then do all calculations in the TT system
  if (par.m_outsys == "M") {
    outsys_is_met=true;
    par.m_outsys="T";
  }

  // If the input system is MET, the want to convert input time into
  // the TT system.
//  bool insys_is_met=false;    // +++ 2014-09-09 MCW This will be needed if allowing c1, c2, JD, or MJD formatted input MET dates
  if (par.m_insys == "M") {
//    insys_is_met=true;
    par.m_insys="T";
  }


  // ==== process epoch ====

  // Reformat epoch into AhDateTime.
  reformatToDateTime(par.m_epochtime,par.m_epochform,0,&epoch_base);

  // Convert epoch to same system as input time.
  convertDateTime(leapsecdat,par.m_epochsys,&epoch_base,par.m_insys,
                  epoch_in);

  // Convert epoch into output system.
  convertDateTime(leapsecdat,par.m_epochsys,&epoch_base,par.m_outsys,
                  epoch_out);


  // ==== perform time conversion ====

  // Reformat input time into AhDateTime (calendar format).
  reformatToDateTime(par.m_intime,par.m_inform,&epoch_in,&time_in);

  // Convert input time into output system.
  convertDateTime(leapsecdat,par.m_insys,&time_in,par.m_outsys,time_out);


  // ==== prepare output for all formats ====

  // Reformat output time as seconds since epoch.  If the output system is UTC,
  // we need to include the number of leap seconds.
  if (par.m_outsys == "U") {
    long nleap=ahtime::numLeapSecInInterval(epoch_out,time_out,leapsecdat);
    double seconds=0;
    ahtime::reformatDateTimeAsSeconds(time_out,epoch_out,seconds);
    time_sec=formatDoubleToPrecision(seconds+nleap,par.m_numdigits);
  } else {
    reformatFromDateTime(&time_out,&epoch_out,"s",time_sec,par.m_numdigits);
  }
  if (par.m_outform == "s") par.m_outtime=time_sec;

  // Reformat output time as days since epoch.
  reformatFromDateTime(&time_out,&epoch_out,"d",time_day,par.m_numdigits);
  if (par.m_outform == "d") par.m_outtime=time_day;

  // When the output system is MET, we need the number of years, months,
  // etc elapsed since the epoch
  if (outsys_is_met) {
    double dsubsecond=0.;
    ahtime::getDeltaCalPieces(epoch_out,time_out,dyear,dmonth,dday,dhour,
                              dminute,dsecond,dsubsecond);
    dsubsecint=(long long)(std::pow(10.,par.m_numdigits)*dsubsecond);    // get subsecond to specified precision

    // Also need day number (1-365) between start and end dates.  This is
    // obtained by adding the number of days left in the start year with the
    // number of days into the last year.  Subtract the number of days in a
    // year to ensure that day number has the proper range.  Leap days are
    // ignored in this calculation.
    dddd=std::min(ahtime::DAY_IN_YEAR-1,epoch_out.daysLeftInYear())+    // ignoring leap days, the largest number of remaining days is 364
         std::min(ahtime::DAY_IN_YEAR-1,time_out.daysInYear());
    if (dddd >= ahtime::DAY_IN_YEAR) dddd-=ahtime::DAY_IN_YEAR;
  }

  // Reformat output time as YYYY-MM-DDThh:mm:ss.
  if (outsys_is_met) {
    // When the output system is MET, write the number of years, months,
    // etc elapsed since the epoch
    std::stringstream tstr;
    tstr << std::setw(4) << std::setfill('0') << dyear << "-";
    tstr << std::setw(2) << std::setfill('0') << dmonth << "-";
    tstr << std::setw(2) << std::setfill('0') << dday << "T";
    tstr << std::setw(2) << std::setfill('0') << dhour << ":";
    tstr << std::setw(2) << std::setfill('0') << dminute << ":";
    tstr << std::setw(2) << std::setfill('0') << dsecond;
    if (dsubsecint > 0) tstr << "." << std::setw(par.m_numdigits) << std::setfill('0') << dsubsecint;
    time_c1=tstr.str();
  } else {
    reformatFromDateTime(&time_out,0,"c1",time_c1,par.m_numdigits);   // epoch not needed
  }
  if (par.m_outform == "c1") par.m_outtime=time_c1;

  // Reformat output time as YYYY:DDD:hh:mm:ss.
  if (outsys_is_met) {
    // When the output system is MET, write the number of years, days,
    // etc elapsed since the epoch.
    std::stringstream tstr;
    tstr << std::setw(4) << std::setfill('0') << dyear << ":";
    tstr << std::setw(3) << std::setfill('0') << dddd << ":";
    tstr << std::setw(2) << std::setfill('0') << dhour << ":";
    tstr << std::setw(2) << std::setfill('0') << dminute << ":";
    tstr << std::setw(2) << std::setfill('0') << dsecond;
    if (dsubsecint > 0) tstr << "." << std::setw(par.m_numdigits) << std::setfill('0') << dsubsecint;
    time_c2=tstr.str();
  } else {
    reformatFromDateTime(&time_out,0,"c2",time_c2,par.m_numdigits);   // epoch not needed
  }
  if (par.m_outform == "c2") par.m_outtime=time_c2;

  // Reformat output time as JD.
  if (outsys_is_met) {
    time_jd="+"+time_day;     // for MET, output is number of days elasped from epoch
  } else {
    reformatFromDateTime(&time_out,0,"j",time_jd,par.m_numdigits);   // epoch not needed
  }
  if (par.m_outform == "j") par.m_outtime=time_jd;

  // Reformat output time as MJD.
  if (outsys_is_met) {
    time_mjd="+"+time_day;     // for MET, output is number of days elasped from epoch
  } else {
    reformatFromDateTime(&time_out,0,"m",time_mjd,par.m_numdigits);   // epoch not needed
  }
  if (par.m_outform == "m") par.m_outtime=time_mjd;

  // Get GPS time.
  convertToGPS(leapsecdat,par.m_insys,&time_in,par.m_gpstime);
  std::string time_gps=formatDoubleToPrecision(par.m_gpstime,par.m_numdigits);

  // Set output parameters.
  ape_trad_set_string("outtime",par.m_outtime.c_str());
  ape_trad_set_double("gpstime",par.m_gpstime);

  // Write all formats to log file.
  AH_INFO(ahlog::LOW) << "SECONDS: " << time_sec << std::endl;
  AH_INFO(ahlog::LOW) << "DAYS:    " << time_day << std::endl;
  AH_INFO(ahlog::LOW) << "CAL1:    " << time_c1 << std::endl;
  AH_INFO(ahlog::LOW) << "CAL2:    " << time_c2 << std::endl;
  AH_INFO(ahlog::LOW) << "JD:      " << time_jd << std::endl;
  AH_INFO(ahlog::LOW) << "MJD:     " << time_mjd << std::endl;
  AH_INFO(ahlog::LOW) << "GPS:     " << time_gps << " s" << std::endl;

  AH_OUT << "input time:  " << par.m_intime << "  input time system:  " 
         << par.m_insys << "  input format:  " << par.m_inform << std::endl;
  AH_OUT << "output time: " << par.m_outtime << "  output time system: " 
         << par.m_outsys << "  output format: " << par.m_outform << std::endl;

}

// ****************************************************************************

void finalize(){

  // Nothing needs to be done for this tool.

}

// ****************************************************************************

std::string getFormatFromParameter(const std::string& format) {
  if (format[0] == 's' || format[0] == 'S') {          // seconds
    return "s";
  } else if (format[0] == 'd' || format[0] == 'D') {   // days
    return "d";
  } else if (format == "c1" || format == "C1") {       // YYYY-MM-DDThh:mm:ss
    return "c1";
  } else if (format == "c2" || format == "C2") {       // YYYY:DDD:hh:mm:ss
    return "c2";
  } else if (format[0] == 'j' || format[0] == 'J') {   // Julian Day
    return "j";
  } else if (format[0] == 'm' || format[0] == 'M') {   // modified Julian day
    return "m";
  } else {
    return "";
  }
}

// ****************************************************************************

std::string getSystemFromParameter(const std::string& system) {
  if (system[0] == 'm' || system[0] == 'M') {          // MET
    return "M";
  } else if (system[0] == 't' || system[0] == 'T') {   // TT
    return "T";
  } else if (system[0] == 'u' || system[0] == 'U') {   // UTC
    return "U";
  } else {
    return "";
  }
}

// ****************************************************************************

std::string formatDoubleToPrecision(double value, int numdigits) {
  // The maximum number of decimal digits to display is 18 since that is
  // the range limit for the long long data type.  Normally this is checked,
  // in getPar(), but extra digits are displayed for days (e.g. MJD).  Here,
  // we ensure that we do not exceed the 18 digits.
  int n=std::min(numdigits,18);

  long long whole=(long long) value;
  std::stringstream out;
  out << whole;
  if (n > 0) {
    double frac=value-whole;
    long long fracint=(long long)(std::pow(10.,n)*frac);
    if (fracint > 0)    // do not need to display a bunch of zeros; e.g. 14.00000000
      out << "." << std::setw(n) << std::setfill('0') << fracint;
  }
  return out.str();
}

// ****************************************************************************

void reformatToDateTime(const std::string& timestr, const std::string& format,
                        ahtime::AhDateTime* epoch, ahtime::AhDateTime* time) {

  // Check if epoch is required.
  if (format == "s" || format == "d") {
    if (epoch == 0) AH_THROW_RUNTIME("epoch required if reformatting a time in seconds or days");
  }

  if (format == "s") {
    // Note: the reformat routine used here converts seconds into days assuming
    // a constant length of 86400s.  This is not true for UTC when a leap second
    // occurs.  However, when the input format is "s", this task assumes that
    // these are SI seconds, so the distinction between the UTC, TT, and MET
    // systems is not important.
    char* tmp=(char*)timestr.c_str();   // get reference to start of C-style string
    char* endptr=0;                     // used to check if number was found
    double seconds=std::strtod(tmp,&endptr);
    if (endptr == tmp)
      AH_THROW_RUNTIME("invalid input time; cannot convert "+timestr+" into seconds");
    ahtime::reformatSecondsAsDateTime(seconds,*epoch,*time);
  } else if (format == "d") {
    // Note: the reformat routine used here allows for a variable-length day
    // (as is the case when a leap second occurs in the UTC system).  So, the
    // input format of days will give a different result between the UTC and
    // TT/MET systems.  This differs than the treatment of "s" format (see
    // above).
    char* tmp=(char*)timestr.c_str();   // get reference to start of C-style string
    char* endptr=0;                     // used to check if number was found
    double days=std::strtod(timestr.c_str(),&endptr);
    if (endptr == tmp)
      AH_THROW_RUNTIME("invalid input time; cannot convert "+timestr+" into days");
    ahtime::reformatDaysAsDateTime(days,*epoch,*time);
  } else if (format == "c1" || format == "c2") {
    time->set(timestr);
  } else if (format == "j") {
    char* tmp=(char*)timestr.c_str();   // get reference to start of C-style string
    char* endptr=0;                     // used to check if number was found
    double jd=std::strtod(timestr.c_str(),&endptr);
    if (endptr == tmp)
      AH_THROW_RUNTIME("invalid input time; cannot convert "+timestr+" into JD");
    ahtime::reformatJDAsDateTime(jd,*time);
  } else if (format == "m") {
    char* tmp=(char*)timestr.c_str();   // get reference to start of C-style string
    char* endptr=0;                     // used to check if number was found
    double mjd=std::strtod(timestr.c_str(),&endptr);
    if (endptr == tmp)
      AH_THROW_RUNTIME("invalid input time; cannot convert "+timestr+" into MJD");
    ahtime::AhMJDTime intime(mjd);
    ahtime::reformatMJDAsDateTime(intime,*time);
  } else {
    AH_THROW_RUNTIME("invalid format: "+format+"; expecting s, d, c1, c2, j, or m");
  }
}

// ****************************************************************************

void convertDateTime(ahtime::leapsec::LeapSecTable & leapsecdat,
                     const std::string& insys, ahtime::AhDateTime* intime,
                     const std::string& outsys, ahtime::AhDateTime& outtime) {

  if (insys == "M" || outsys == "M")
    AH_THROW_LOGIC("trying to convert to/from MET system; this should be handled in doWork()");

  if (insys == "U" && outsys == "T") {
    ahtime::convertUTCToTT(*intime,leapsecdat,outtime);
  } else if (insys == "T" && outsys == "U") {
    ahtime::convertTTToUTC(*intime,leapsecdat,outtime);
  } else {   // input and output systems are the same
    outtime=*intime;
  }

}

// ****************************************************************************

void reformatFromDateTime(ahtime::AhDateTime* time, ahtime::AhDateTime* epoch,
                          const std::string& format, std::string& timestr,
                          int numdigits) {

  // Check if epoch is required.
  if (format == "s" || format == "d") {
    if (epoch == 0) AH_THROW_LOGIC("epoch required if reformatting a time in seconds or days");
  }

  std::stringstream tmp_timestr;                             // used to convert value to output string

  if (format == "s") {                                       // seconds since epoch
    double seconds=0;
    ahtime::reformatDateTimeAsSeconds(*time,*epoch,seconds);
    timestr=formatDoubleToPrecision(seconds,numdigits);

  } else if (format == "d") {                                // days since epoch
    double days=0;
    ahtime::reformatDateTimeAsDays(*time,*epoch,days);
    timestr=formatDoubleToPrecision(days,numdigits+6);         // use extra precision to get correct precision for seconds

  } else if (format == "c1") {                               // YYYY-MM-DDThh:mm:ss.xxxxxx
    long long subsecint=time->getSubsecondAsInt(numdigits);
    if (subsecint > 0)                                       // only display subseconds if all the digits are not zero
      timestr=time->getDateTimeAsStr(numdigits);
    else
      timestr=time->getDateTimeAsStr(0);

  } else if (format == "c2") {                               // YYYY:DDD:hh:mm:ss.xxxxxx
    std::stringstream dayofyear;
    dayofyear << std::setw(3) << std::setfill('0') << time->daysInYear();
    long long subsecint=time->getSubsecondAsInt(numdigits);
    if (subsecint > 0) {                                     // only display subseconds if all the digits are not zero
      tmp_timestr << time->year() << ":" << dayofyear.str() << ":"
                  << time->getTimeAsStr(numdigits);
    } else {
      tmp_timestr << time->year() << ":" << dayofyear.str() << ":"
                  << time->getTimeAsStr();
    }
    timestr=tmp_timestr.str();

  } else if (format == "j") {                                // JD
    double jd=0.;
    ahtime::reformatDateTimeAsJD(*time,jd);
    timestr=formatDoubleToPrecision(jd,numdigits+6);           // use extra precision to get correct precision for seconds

  } else if (format == "m") {                                // MJD
    ahtime::AhMJDTime mjd;
    ahtime::reformatDateTimeAsMJD(*time,mjd);
    timestr=formatDoubleToPrecision(mjd.mjd(),numdigits+6);    // use extra precision to get correct precision for seconds

  } else {
    AH_THROW_RUNTIME("invalid format; expecting s, d, c1, c2, j, or m");
  }
}

// ****************************************************************************

void convertToGPS(ahtime::leapsec::LeapSecTable & leapsecdat,
                  const std::string& insys, ahtime::AhDateTime* intime,
                  double& gpstime) {

  // Set up GPS epoch and convert from UTC to TT.
  ahtime::AhDateTime gpsepoch(ahtime::GPS_EPOCH_CAL);  // in UTC
  if (insys == "T") {    // insys will be set to "T" for MET also
    ahtime::AhDateTime gpsepoch_tt;
    convertDateTime(leapsecdat,"U",&gpsepoch,"T",gpsepoch_tt);
    gpsepoch=gpsepoch_tt;
  }

  // Calculate time difference between intime and GPS epoch.
  long long seconds=0;
  double subseconds=0.;
  seconds=ahtime::numSecInInterval(gpsepoch,*intime,&subseconds);
  gpstime=(double)seconds+subseconds;

  // If input is UTC, adjust for leap seconds.
  if (insys == "U") {     // input UTC system
    long numleapseconds=ahtime::numLeapSecBefore(*intime,leapsecdat);
    gpstime+=numleapseconds-19.;
  }
}

// ****************************************************************************

/** @} */


/* Revision Log
 $Log: ahtimeconv.cxx,v $
 Revision 1.37  2016/02/02 15:04:44  mwitthoe
 ahtimeconv: write input and output times to screen

 Revision 1.36  2015/07/30 16:26:59  mwitthoe
 ahtimeconv: add parameter stamping to log file

 Revision 1.35  2015/07/23 13:29:16  klrutkow
 removed commented line

 Revision 1.34  2015/07/22 14:06:52  klrutkow
 changed local CALDB query for leapsec to use ahmission caldb resolve query

 Revision 1.33  2015/07/07 14:08:16  klrutkow
 fixed error when calling ape_trad_set_str

 Revision 1.32  2015/07/01 16:53:44  klrutkow
 added ape call to store resolved leapsec filename in par file, and for logging

 Revision 1.31  2015/07/01 16:20:03  klrutkow
 remove comment to replace leapsec::resolve query for CALDB: that is correct way to call it

 Revision 1.30  2015/01/07 16:04:38  mdutka
 Updated parameter list see issue #472

 Revision 1.29  2014/12/17 16:20:21  mwitthoe
 ahtimeconv: remove debugging print statment

 Revision 1.28  2014/12/17 16:19:03  mwitthoe
 ahtimeconv: throw an error if the input time cannot be converted into a double value for the formats: seconds, days, JD, and MJD; issue 470

 Revision 1.27  2014/09/11 18:51:29  mwitthoe
 ahtimeconv tool: clean-up/conform to standards; add documentation

 Revision 1.26  2014/09/11 16:59:33  mwitthoe
 ahtimeconv tool: fix bug when trying to display more than 18 decimal digits for day quantities

 Revision 1.25  2014/09/11 16:54:49  mwitthoe
 ahtimeconv tool: add numdigits parameter which sets the number of decimal digits to include when displaying seconds

 Revision 1.24  2014/09/11 14:04:58  mwitthoe
 ahtimeconv tool: change intermediate format from AhMJDTime to AhDateTime in order to maximize precision (double value is seconds instead of days)

 Revision 1.23  2014/09/10 20:50:36  mwitthoe
 ahtimeconv tool: fix GPS calculation for TT times

 Revision 1.22  2014/09/10 02:50:39  mwitthoe
 ahtimeconv: change behavior according to new TRF from Sep 2014

 Revision 1.21  2014/01/03 21:34:17  mwitthoe
 ahtimeconv: update standard main, see issue 327

 Revision 1.20  2013/12/04 16:12:17  mwitthoe
 ahtimeconv: show microseconds with DateTime formatted times; allow fractional seconds to be input with UTC times

 Revision 1.19  2013/12/02 22:53:36  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.18  2013/10/16 02:18:01  mwitthoe
 ahtimeconv: update documentation to remove references to TAI times and add Mission Time instead

 Revision 1.17  2013/09/19 14:05:04  mwitthoe
 ahtimeconv tool: updated tool to match how ahtime library now treaw treats time systems and time formats; the input can be Mission Time as a number of seconds elapsed since a UTC epoch (in DateTime format), or UTC or TT times each which can be given in a DateTime or MJD format; the input is then converted to all other formats

 Revision 1.16  2013/04/12 19:35:40  mwitthoe
 update ahtimeconv to use new version of leap second library

 Revision 1.15  2013/04/03 16:48:00  mwitthoe
 ahtimeconv: removed ahmission from the Makefile; improved error message when given an empty time string to convert

 Revision 1.14  2013/03/26 14:37:33  mwitthoe
 updated standard main for ahtimeconv tool

 Revision 1.13  2013/02/19 22:15:47  mwitthoe
 ahtimeconv: make time type parameter (e.g. UTC) not case specific

 Revision 1.12  2013/02/13 23:11:28  mwitthoe
 fix typo in ahtimeconv Doxygen description (see issue 213)

 Revision 1.11  2012/11/15 03:01:38  mwitthoe
 change timing tools to use new ahcaldb libraries

 Revision 1.10  2012/10/18 17:38:49  mwitthoe
 align ahtimeconv tool with changes in ahtime library: namely, single ahtime header and new CAMS CALDB file

 Revision 1.9  2012/09/14 22:10:15  mwitthoe
 switch ahtimeconv tool to new version reporting scheme

 Revision 1.8  2012/08/28 21:36:21  mwitthoe
 change a function name in ahtimeconv tool to match change in ahtime library

 Revision 1.7  2012/08/23 21:31:21  mwitthoe
 change from ahgen to ahapp in ahtime and ahtimeconv tools

 Revision 1.6  2012/08/18 02:39:28  mwitthoe
 make ahtime and ahtimeconv tools work with new version of ahleapsec library

 Revision 1.5  2012/08/18 01:47:02  mwitthoe
 apply standards to ahtimeconv tool

 Revision 1.4  2012/08/15 16:55:54  mwitthoe
 put finalize() in try-block in main() of ahtime and ahtimeconv

 Revision 1.3  2012/08/15 16:25:03  mwitthoe
 add versioning to ahtime and ahtimeconv tools

 Revision 1.2  2012/07/20 14:58:11  mwitthoe
 add Doxygen to ahtimeconv

 Revision 1.1  2012/07/16 22:02:11  mwitthoe
 add ahtimeconv task to mission/tasks; converts times between UTC, TAI, and TT


*/

