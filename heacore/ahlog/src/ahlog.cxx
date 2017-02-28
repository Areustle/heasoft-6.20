/// \file ahlog.cxx
/// \brief AstroH logger
/// \author Mike Witthoeft
/// \date $Date: 2015/06/03 01:54:30 $

#define AHLABEL ahlog_ahlog
#define AHCVSID "$Id: ahlog.cxx,v 1.48 2015/06/03 01:54:30 rshill Exp $"

#include "ahlog/ahlog.h"

#ifdef HAVE_EXECINFO_H
#include <execinfo.h> 
#endif

#include <cstdarg>
#include <stdio.h>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <unistd.h>

namespace ahlog {

/// \brief Access global formatter; used by macros.
st_stream::StreamFormatter & logger();

/// \brief Pointer to logfile.
std::ofstream* & logfile();

/// \brief Answer to question: Is the log file being written?
bool & writelogfile();

/// \brief Return true/false whether DEBUG is on/off
bool debug();

/// \brief Return chatter level
int chatter();

/// \brief Overwrite (true) or append (false) log file
bool & clobber();

/// \brief Executable name
std::string& executable_name();

st_stream::OStream & logstream() {
  static st_stream::OStream s_logstream(false);
  return s_logstream;
}

/// \callgraph
st_stream::StreamFormatter & logger() {
  static st_stream::StreamFormatter s_format("", "", -1);
  return s_format;
}
// +++ check to see if anything is calling, if not, set as static, remove from header
std::string& executable_name() {
  static std::string s_execname;
  return s_execname;
}

// Create static OStream objects that will wrap the corresponding C++ streams before
// being connected to the "standard" streams sterr, stout, stlog.
static st_stream::OStream s_strm_cerr(true);
static st_stream::OStream s_strm_clog(true);
static st_stream::OStream s_strm_cout(true);

/// \callgraph
st_stream::OStream & macro_out(const std::string & func) {
  ahlog::logger().setMethod(func);
  return ahlog::logger().out();
}

/// \callgraph
st_stream::OStream & macro_info(const int & chatter, const std::string & func) {
  if (chatter == ahlog::OUT) return ahlog::macro_out(func);
  ahlog::logger().setMethod(func);
  return ahlog::logger().info(chatter);
}

/// \callgraph
st_stream::OStream & macro_warn(int chatter, const std::string & func) {
  ahlog::logger().setMethod(func);
  return ahlog::logger().warn(chatter);
}

/// \callgraph
st_stream::OStream & macro_err(const std::string & func) {
  ahlog::logger().setMethod(func);
  return ahlog::logger().err();
}

/// \callgraph
st_stream::OStream & macro_debug(const std::string & pfunc) {
  if (ahlog::get_debug()) ahlog::logger().setMethod(ahlog::strip_function(pfunc));
  return ahlog::logger().debug();
}

extern "C" {

int ahlog_macro_trace() {
  int status = 0;
  try {
    ahlog::macro_trace();
  } catch (const std::exception & x) {
    AH_INFO(ahlog::LOW) << "Problem calling ahlog::macro_trace: " << x.what() << std::endl;
    status = 1; // +++ 2013-11-04 JP Can we get the status from the message?
  }
  return status;
}

}

/// \callgraph
void macro_trace() {
  if(!ahlog::debug()) return;

#ifdef HAVE_EXECINFO_H
  const int bufsz = BUFSIZ;
  void *buff[bufsz];
  memset(buff, 0, bufsz);

  /* Backtrace returns addresses, each of which is presumably of type void *, so only
  ask for the number of addresses that would fit in the buffer. */
  int nb = backtrace(buff, bufsz/sizeof(void *));
  if( nb <= 0 ) return;

  char **text = backtrace_symbols(buff, nb);
  if( 0 == text ) return;

  ahlog::logger().err() << "*** STACK TRACE ***" << std::endl;
  for( int i = 1; i < nb; ++i ) {    // i=0 is always the macro function; skip
    if( 0 == text[i] ) continue;
    ahlog::logger().err() << i << ". " << text[i] << std::endl;
  }
  ahlog::logger().err() << "*** END TRACE ***" << std::endl;
  free(text); text = 0;
#else 
  ahlog::logger().err() << "*** STACK TRACE ***" << std::endl;
  ahlog::logger().err() << "*** STACK TRACE NOT SUPPORTED ON THIS SYSTEM ***" << std::endl;
  ahlog::logger().err() << "*** END TRACE ***" << std::endl;
#endif
}


namespace {
  
// write_to_stream: purely internal (in unnamed namespace) utility for handling the formatted
// output of the variable arguments passed from calling function. This function does not
// throw exceptions so it may safely be called from extern C and C++ functions.
// Note: the comments below deliberately do not use the third slash for doxygen. This is
// internal only.
// \param[in] os Stream destination for output.
// \param[in] format C-style (printf) format string.
// \param[in] ap Object holding variable arguments passed from calling function.
#define MAX_LINE_SIZE (4096u)
int write_to_stream(st_stream::OStream & os, const char * format, va_list ap) {
  int retval = -1;
  char str[MAX_LINE_SIZE] = "";
  // Use standard library function to handle the formatting into a string.
  retval = vsnprintf(str, MAX_LINE_SIZE, format, ap);
  // Pass the string to the stream selected by the calling code.
  os << str << std::flush;
  return retval;
}
#undef MAX_LINE_SIZE
}

/// \callgraph
std::ofstream* & logfile() {
  static std::ofstream* std_os;
  return std_os;
}

/// \callgraph
bool & writelogfile() {
  static bool logfileon;
  return logfileon;
}


extern "C" {

int ahlog_out(const char * func, const char * format, ...) {
  int retval = -1;
  if (0 != func and 0 != format) {
    va_list ap;
    va_start(ap, format);
    retval = write_to_stream(macro_out(func).prefix(), format, ap);
    va_end(ap);
  }
  return retval;
}

}

/// \callgraph
int out(const std::string & func, const std::string & format, ...) {
  int retval = -1;
  va_list ap;
  va_start(ap, format);
  retval = write_to_stream(macro_out(func).prefix(), format.c_str(), ap);
  va_end(ap);
  return retval;
}

extern "C" {

int ahlog_info(int chatter, const char * func, const char * format, ...) {
  int retval = -1;
  if (0 != func and 0 != format) {
    va_list ap;
    va_start(ap, format);
    retval = write_to_stream(macro_info(chatter, func).prefix(), format, ap);
    va_end(ap);
  }
  return retval;
}

}

/// \callgraph
int info(int chatter, const std::string & func, const std::string & format, ...) {
  int retval = -1;
  va_list ap;
  va_start(ap, format);
  retval = write_to_stream(macro_info(chatter, func).prefix(), format.c_str(), ap);
  va_end(ap);
  return retval;
}

extern "C" {

int ahlog_warn(int chatter, const char * func, const char * format, ...) {
  int retval = -1;
  if (0 != func and 0 != format) {
    va_list ap;
    va_start(ap, format);
    retval = write_to_stream(macro_warn(chatter, func).prefix() << ahlog::format_chatter(chatter), format, ap);
    va_end(ap);
  }
  return retval;
}

}

/// \callgraph
int warn(int chatter, const std::string & func, const std::string & format, ...) {
  int retval = -1;
  va_list ap;
  va_start(ap, format);
  retval = write_to_stream(macro_warn(chatter, func).prefix() << ahlog::format_chatter(chatter), format.c_str(), ap);
  va_end(ap);
  return retval;
}

extern "C" {

int ahlog_err(const char * func, const char * format, ...) {
  int retval = -1;
  if (0 != func and 0 != format) {
    va_list ap;
    va_start(ap, format);
    retval = write_to_stream(macro_err(func).prefix(), format, ap);
    va_end(ap);
  }
  return retval;
}

}

/// \callgraph
int err(const std::string & func, const std::string & format, ...) {
  int retval = -1;
  va_list ap;
  va_start(ap, format);
  retval = write_to_stream(macro_err(func).prefix(), format.c_str(), ap);
  va_end(ap);
  return retval;
}

extern "C" {

int ahlog_debug(const char * func, const char * file, const int line, const char * format, ...) {
  int retval = -1;
  if (0 != func and 0 != format) {
    va_list ap;
    va_start(ap, format);
    retval = write_to_stream(macro_debug(func).prefix() << "[" << std::string(file) << ":" << line << "] ", format, ap); 
    va_end(ap);
  }
  return retval;
}

}

// Overloaded function name
/// \callgraph

int debug(const std::string & func, const std::string & file, const int line, const std::string & format, ...) {
  int retval = -1;
  va_list ap;
  va_start(ap, format);
  retval = write_to_stream(macro_debug(func).prefix() << "[" << file << ":" << line << "] ", format.c_str(), ap); 
  va_end(ap);
  return retval;
}



extern "C" {

void ahlog_set_executable_name(const char* name) {
  executable_name() = name;
}

const char* ahlog_get_executable_name() {
  return executable_name().c_str();
}

}


/// \callgraph
void set_executable_name(const std::string& name) {
  executable_name() = name;
}

/// \callgraph
const std::string & get_executable_name() {
  return executable_name();
}

extern "C" {

void ahlog_set_debug(char val) {
  set_debug(0 != val);
}

char ahlog_get_debug() {
  return (debug() ? 1 : 0);
}

}

// Overloaded function name
/// \callgraph
bool debug() {
  return st_stream::GetDebugMode();
}

/// \callgraph
bool get_debug() {
  return debug();
}

/// \callgraph
void set_debug(bool val) {
  st_stream::SetDebugMode(val);
  ahlog::logger().setDebugMode(val);
}

/// \callgraph
bool & clobber() {
  static bool clobberlog;
  return clobberlog;
}

extern "C" {

void ahlog_set_clobber(char clob) {
  clobber() = clob;
}

char ahlog_get_clobber() {
  return (clobber() ? 1 : 0);
}

}

/// \callgraph
void set_clobber(bool clob) {
  clobber() = clob;
}

/// \callgraph
bool & get_clobber() {
  return clobber();
}

/// \callgraph
int chatter() {
  return st_stream::GetMaximumChatter();
}

extern "C" {

void ahlog_set_chatter(int chat) {
  set_chatter(chat);
}

int ahlog_get_chatter() {
  return chatter();
}

}

/// \callgraph
int get_chatter() {
  return chatter();
}

/// \callgraph
void set_chatter(int val) {
  if (val < 1) val=1;
  if (val > 3) val=3;
  st_stream::SetMaximumChatter(val);
  ahlog::logger().setDebugMode(val);
}


/// \callgraph
struct timeval & starttime() {
  static struct timeval logopentime;
  return logopentime;
}

extern "C" {

int ahlog_setup(const char * execname, const char * logfile, int chatter, char debug) {
  int status = 0;
  try {
    ahlog::setup(execname, logfile, chatter, 0 != debug);
  } catch (const std::exception & x) {
    AH_INFO(ahlog::LOW) << "Problem calling ahlog::setup: " << x.what() << std::endl;
    status = 1; // +++ 2013-11-04 JP Can we get the status from the message?
  }
  return status;
}

}

/// \callgraph
void setup(std::string execname, std::string logfile, int chatter, 
           bool debug) {
  // store start time
  gettimeofday(&starttime(),NULL);

  bool resetToMinChatter=false;
  bool resetToMaxChatter=false;
  if (chatter < ahlog::MINCHAT) {
    chatter=ahlog::MINCHAT;
    resetToMinChatter=true;
  }
  if (chatter > ahlog::MAXCHAT) {
    chatter=ahlog::MAXCHAT;
    resetToMaxChatter=true;
  }

  // set debug state
  ahlog::set_debug(debug);

  // strip path information from given executable name
  std::string execbase=ahlog::strip_path(execname);
  executable_name()=execbase;

  // initialize stream (if chatter=0, then write everything only to log file)
  st_stream::InitGlobal(execbase, chatter, debug);
  if (0 < chatter) {
    s_strm_cerr.connect(std::cerr);
    s_strm_clog.connect(std::clog);
    s_strm_cout.connect(std::cout);
    st_stream::sterr.connect(s_strm_cerr);
    st_stream::stlog.connect(s_strm_clog);
    st_stream::stout.connect(s_strm_cout);
  }

  // get clobber
  clobber()=false;
  if (logfile[0] == '!') {
    clobber()=true;
    logfile.erase(0,1);
  }

  // if empty logfile, base filename on execname
  writelogfile()=true;
  if ("" == logfile) {
    throw std::runtime_error("the name of the log file was set to an empty string (\"\"); this is an illegal value");
  } else if ("DEFAULT" == logfile) {
    std::stringstream tstr;
    tstr << execname << ".log";
    logfile=tstr.str();
  } else if ("NONE" == logfile) {
    writelogfile()=false;
  }

  ahlog::logfile()=0;   // this can cause memory leak if setup called twice
  if (writelogfile()) {
    if (clobber()) {
      ahlog::logfile()=new std::ofstream(logfile.c_str());
    } else {
      ahlog::logfile()=new std::ofstream(logfile.c_str(),std::fstream::app);
    }
    if (ahlog::logfile()->bad()) throw "could not open log file";

    // write date and time to top of header file
    ahlog::write_header(execbase);

    ahlog::logstream().connect(*ahlog::logfile());

    st_stream::sterr.connect(ahlog::logstream());
    st_stream::stlog.connect(ahlog::logstream());
    st_stream::stout.connect(ahlog::logstream());
  }

  // if chatter level was changed to set it within valid range, then report
  if (resetToMinChatter) {
    AH_INFO(ahlog::LOW) << "Chatter too small; resetting to " << ahlog::MINCHAT
                        << std::endl;
  }
  if (resetToMaxChatter) {
    AH_INFO(ahlog::LOW) << "Chatter too large; resetting to " << ahlog::MAXCHAT
                        << std::endl;
  }

}

extern "C" {
void ahlog_shutdown() { ahlog::shutdown(); }
}

/// \callgraph
void shutdown() {
  ahlog::write_footer(true);
  if (0 != ahlog::logfile()) {
    // Clean up the log file and its connections.
    st_stream::stout.disconnect(ahlog::logstream());
    st_stream::stlog.disconnect(ahlog::logstream());
    st_stream::sterr.disconnect(ahlog::logstream());
    ahlog::logfile()->close();
    delete ahlog::logfile();
    ahlog::logfile()=0;
  }
  st_stream::stout.disconnect(s_strm_cout);
  st_stream::stlog.disconnect(s_strm_clog);
  st_stream::sterr.disconnect(s_strm_cerr);
}

/// \callgraph
void write_header(const std::string & execname) {
  if (!writelogfile()) return;    // only write header to log file

  struct tm *current;
  time_t now;
  time(&now);
  current=localtime(&now);

  char buff[100];
  sprintf(buff,"%i/%02i/%02i %i:%02i:%02i",1900+current->tm_year,
          1+current->tm_mon,current->tm_mday,current->tm_hour,
          current->tm_min,current->tm_sec);

  char* runpath=getcwd(NULL,0);
  *ahlog::logfile() << "STARTLOG: " << buff << std::endl;
  *ahlog::logfile() << "EXECNAME: " << execname << std::endl;
  *ahlog::logfile() << "RUNPATH:  " << runpath << std::endl;
  *ahlog::logfile() << std::endl;
  free(runpath);
  runpath=0;
}

/// \callgraph
void write_footer(bool status) {
  if (!writelogfile()) return;    // only write footer to log file

  // get current time
  struct timeval endtime;
  gettimeofday(&endtime,NULL);

  // get program elapsed seconds
  double wtime=(endtime.tv_sec-starttime().tv_sec)+ // integral seconds
               (endtime.tv_usec-starttime().tv_usec)/1.e6; // fractional

  // convert to minutes if over 60s
  std::string units=" s";
  /// \internal
  /// \note uncomment below to allow for conversion of wall time to minutes
  /// if greater than 60s.
  //if (wtime > 60.) {
  //  wtime/=60.;
  //  units=" min";
  //}

  *ahlog::logfile() << std::endl; 
  /// \internal
  /// \note uncomment below to give exit states: normal or error
  //if (status) {
  //  *ahlog::logfile() << "EXIT:     NORMAL" << std::endl;
  //} else {
  //  *ahlog::logfile() << "EXIT:     ERROR" << std::endl;
  //}
  *ahlog::logfile() << "ENDLOG:   " << wtime << units << std::endl;
}

/// \callgraph
std::string format_chatter(unsigned int chat) {
  switch (chat) {
    case ahlog::HIGH:
      return "(HIGH) ";
    case ahlog::LOW:
      return "(LOW) ";
  }
  return "";
}

/// \callgraph
std::string strip_function(const std::string & instr) {
//return instr;
  std::string x=instr;

  // +++ 2013-11-05 JP: Bug: if no parentheses are present this hangs.
  // +++ 2013-11-05 JP; Work-around for now:
  if (std::string::npos == x.find("(")) x += "()";

  // for the last argument list, gut contents but leave parentheses;
  // remove other argument lists entirely including parentheses;
  // will misbehave for nested parentheses
  unsigned int i,istart,iend=0;
  while (iend < x.size()-1) {
    istart=-1;
    for (i=0; i<x.size(); i++) {
      if (x[i] == '(') istart=i;
      if (x[i] == ')') {
        iend=i;
        break;
      }
    }
    if (istart < 0) break;          // no parentheses found

    std::string tx=x.substr(0,istart);
    if (iend < x.size()-1) tx+=x.substr(iend+1,x.size()-1);
    if (iend == x.size()-1) tx+="()";
    x=tx;
  }

  // remove return type (this is done after removing argument lists
  // to avoid the problem of no return type and a space in an 
  // argument list)
  for (i=0; i < x.size(); i++) {
    if (x[i] == ' ') break;
  }
  istart=i+1;
  if (istart < x.size()) {        // be sure a space is found
    x=x.substr(istart,x.size()-1);
  }

  return x;
}

/// \callgraph
std::string strip_path(const std::string & instr) {
  int n=instr.size();
  while ( n > 0 && instr[n-1] != '/' && instr[n-1] != '\\') n--;
  return instr.substr(n,instr.size()-n);
}

/// \callgraph
std::string prepend(const std::string & msg, const std::string & func) {
  std::stringstream tstr;
  tstr << ahlog::strip_function(func) << " -> " << msg;
  return tstr.str();
}

}  // namespace ahlog

/* Revision Log
 $Log: ahlog.cxx,v $
 Revision 1.48  2015/06/03 01:54:30  rshill
 Corrected C-like ahlog::debug.

 Revision 1.47  2015/05/29 22:38:51  rshill
 Changed ahlog::debug C-style version - needs checking.

 Revision 1.46  2015/05/13 18:35:14  rshill
 Changed ahlog_debug (C call) to facilitate getting __FILE__ and __LINE__ from calling context.

 Revision 1.45  2014/08/29 12:15:51  peachey
 Use int not char for chatter.

 Revision 1.44  2014/05/28 12:31:35  peachey
 Always flush C-style output.

 Revision 1.43  2014/05/19 18:01:06  peachey
 Disconnect all connected streams in shutdown. Throughout code,
 when connecting/disconnecting error, log, and output streams, always connect in
 that order, and disconnect in the reverse order (output, log, error). Note that
 disconnecting streams in shutdown required moving the definition of some
 static stream objects. This now completely addresses Issue #386.

 Revision 1.42  2014/05/19 17:50:47  peachey
 Clean up the log file during shutdown (see issue #386). Add cvs log to
 testcahlog.c.

 Revision 1.41  2014/05/19 17:27:11  peachey
 Free some unfreed memory (fix small leaks) per issue #386.

 Revision 1.40  2014/04/09 14:38:01  irby
 Cygwin issue not resolved by previous revision.  Change <cstdio>
 include to <stdio.h> and change "std::vsnprintf" to "vsnprintf" to
 address original compiler complaint that "vsnprintf is not a member
 of std".

 Revision 1.39  2014/04/07 11:16:48  peachey
 Add missing header cstdio. Found because of Cygwin problem.

 Revision 1.38  2014/04/02 14:38:25  peachey
 See issue #370. Use #ifdef HAVE_EXECINFO_H macro to prevent compiling
 code that calls backtrace() function on systems that do not have it.
 Preserve STACKTRACE style messages, but make it clear the stacktrace
 is not supported on systems where it is not compiled.

 Revision 1.37  2014/03/06 22:18:09  rshill
 Change to macro_debug() suggested by Mike Witthoeft
 to solve timing issue with debug=no.

 Revision 1.36  2013/12/02 22:43:52  asargent
 debug() made to internal only

 Revision 1.35  2013/12/02 15:35:13  asargent
 debug() is no longer only internal

 Revision 1.34  2013/11/27 22:06:51  asargent
 Several new C-callable functions as well as get/set and printf style output, warn, info, err and debug functions.

 Revision 1.33  2013/11/18 15:24:04  peachey
 In C-callable ahlog_out function, check C-style strings (char * pointers) and signal error if passed.

 Revision 1.32  2013/11/05 21:27:43  peachey
 1) Rename ahlog::printout to simply ahlog::out.
 2) Add C-compatible header file with basic functionality needed to
    use ahlog to write to output stream (only).
 3) Various new comments and corrections.

 Revision 1.31  2013/11/04 15:01:57  peachey
 Added initial changes to support C calling code: 1) C-compatible header,
 2) printout function to do formatted output to the standard output stream.

 Revision 1.30  2013/10/24 20:55:57  peachey
 Add missing unistd.h for getcwd declaration. This broke the build on Mavericks.

 Revision 1.29  2013/10/04 15:20:22  mwitthoe
 ahlog library: ahlog now stores the name of the executable which called setup(); this information is needed by ahfits when logging buffering information to the header of the FITS file

 Revision 1.28  2013/08/19 15:26:39  mwitthoe
 ahlog: fix bug in chatter() function; return type was bool, should be int

 Revision 1.27  2013/08/09 19:59:03  mwitthoe
 ahlog library: reset chatter level if out-of-range

 Revision 1.26  2013/05/23 01:03:59  mwitthoe
 minor refactoring of setup() in ahlog

 Revision 1.25  2013/05/18 19:50:49  mwitthoe
 proposed changes to ahlog, see issue 214

 Revision 1.24  2013/04/04 15:37:05  mwitthoe
 ahlog.cxx: change delete to free for runpath variable which is allocated by the getcwd() C function

 Revision 1.23  2013/01/11 16:54:11  mwitthoe
 fix memory leaks in ahlog by deleting pointers or change pointers to non-pointers

 Revision 1.22  2012/09/14 23:56:56  mwitthoe
 apply version standards to ahlog

 Revision 1.21  2012/08/29 19:42:31  mwitthoe
 changes to ahlog: add set functions for chatter and debug states; remove static instaces of chatter and debug (just refer to those in st_stream); add tests to cycle through values of debug and chatter in testahlog; AH_INFO macro can now take ahlog::OUT as an argument to reproduce the AH_OUT macro output

 Revision 1.20  2012/08/24 19:41:33  mwitthoe
 fix to arguments of macro_info() in ahlog

 Revision 1.19  2012/08/24 19:15:19  mwitthoe
 clean up argument list in ahlog library

 Revision 1.18  2012/08/17 19:23:12  mwitthoe
 tweaks to ahlog

*/
