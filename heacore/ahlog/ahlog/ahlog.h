/**
    \file ahlog.h
    \brief AstroH logger, C++ interface only. For C access use cahlog.h.
    \author Mike Witthoeft
    \date $Date: 2015/06/03 01:58:04 $
   
    Initializes and provides a global formatter for st_stream.  Also defines 
    macros for compact logging syntax.
*/
#ifndef AHLOG_AHLOG_H
#define AHLOG_AHLOG_H

#ifdef __cplusplus
#include "ahlog/StreamFormatter.h"
#include "ahlog/st_stream.h"

#include <sys/time.h>
#include <stdexcept>
#endif

#ifdef __cplusplus
/** \ingroup mod_ahlog
 *  \brief macro to return the current function name (with processing)
 *  \param[in] x message
 * 
 *  This macro is used to prepend error messages before 'thrown'; therefore,
 *  when AH_ERR is ultimately used at a higher level in the application, the
 *  origin of the error is present in the message
 */
#define AH_LOCATE(x) ahlog::prepend(x,__PRETTY_FUNCTION__)

/** \ingroup mod_ahlog
 *  \brief macro to throw runtime error with trace information
 *  \param[in] x message
 */
#define AH_THROW_RUNTIME(x) (ahlog::macro_trace(), throw std::runtime_error(AH_LOCATE(x)))

/** \ingroup mod_ahlog
 *  \brief macro to throw logic error with trace information
 *  \param[in] x message
 */
#define AH_THROW_LOGIC(x) (ahlog::macro_trace(), throw std::logic_error(AH_LOCATE(x)))

/** \ingroup mod_ahlog
 *  \brief logger for DEBUG messages
 */
#define AH_DEBUG (ahlog::macro_debug(__PRETTY_FUNCTION__).prefix() << "[" << __FILE__ << ":" << __LINE__ << "] ")

/** \ingroup mod_ahlog
 *  \brief logger for OUTPUT messages
 */
#define AH_OUT (ahlog::macro_out(__func__).prefix())

/** \ingroup mod_ahlog
 *  \brief logger for INFO messages
 *  \param[in] x message priority: ahlog::LOW or ahlog::HIGH
 */
#define AH_INFO(x) (ahlog::macro_info(x,__func__).prefix())

/** \ingroup mod_ahlog
 *  \brief logger for WARN messages
 *  \param[in] x message priority: ahlog::LOW or ahlog::HIGH
 */
#define AH_WARN(x) (ahlog::macro_warn(x,__func__).prefix() << ahlog::format_chatter(x) )

/** \ingroup mod_ahlog
 *  \brief logger for ERR messages
 */
#define AH_ERR (ahlog::macro_err(__func__).prefix())

/** \brief Astro-H logger
 *  \ingroup mod_ahlog
 */
namespace ahlog {
#endif /* __cplusplus */

/** \addtogroup mod_ahlog
 *  @{
 */

/** \brief minimum and maximum chatter levels */
enum {
  MINCHAT=0,     /**< smallest legal chatter level */
  MAXCHAT=3      /**< largest legal chatter level */
};


/** \brief Message priority levels; small chatter triggers HIGH priority */
enum {
  LOW=3,      /**< low priority */
  HIGH=2,     /**< high priority */
  OUT=1       /**< duplicate output */
};

#ifdef __cplusplus

/** \brief behavior of AH_OUT macro
 *  \param[in] func result of __func__
 *  \return stream for log message
 *  +++ 2013-11-25: C version inserted
 */
st_stream::OStream & macro_out(const std::string & func);


/** \brief behavior of AH_INFO macro
 *  \param[in] chatter level ahlog::LOW or ahlog::HIGH
 *  \param[in] func result of __func__
 *  \return stream for log message
 */
st_stream::OStream & macro_info(const int & chatter, const std::string & func);


/** \brief behavior of AH_WARN macro
 *  \param[in] chatter level ahlog::LOW or ahlog::HIGH
 *  \param[in] func result of __func__
 *  \return stream for log message
 */
st_stream::OStream & macro_warn(int chatter, const std::string & func);


/** \brief behavior of AH_ERR macro
 *  \param[in] func result of __func__
 *  \return stream for log message
 */
st_stream::OStream & macro_err(const std::string & func);


/** \brief behavior of AH_DEBUG macro
 *  \param[in] pfunc result of __PRETTY_FUNCTION__
 *  \return stream for log message
 */
st_stream::OStream & macro_debug(const std::string & pfunc);


/** \brief print stacktrace if in DEBUG mode (used by AH_THROW_*)
 */
void macro_trace();


/** \brief printf-style output.
 *  \param[in] func Name of function, result of __func__
 *  \param[in] format Output C-style formatting string a la printf.
 */
int out(const std::string & func, const std::string & format, ...);


/** \brief printf-style info.
 *  \param[in] func Name of function, result of __func__
 *  \param[in] format Output C-style formatting string a la printf.
 */
int info(int chatter, const std::string & func, const std::string & format, ...);


/** \brief printf-style warn.
 *  \param[in] func Name of function, result of __func__
 *  \param[in] format Output C-style formatting string a la printf.
 */
int warn(int chatter, const std::string & func, const std::string & format, ...);


/** \brief printf-style err.
 *  \param[in] func Name of function, result of __func__
 *  \param[in] format Output C-style formatting string a la printf.
 */
int err(const std::string & func, const std::string & format, ...);


/** \brief printf-style debug.
 *  \param[in] func Name of function, result of __func__
 *  \param[in] format Output C-style formatting string a la printf.
 */
/* int debug(const std::string & pfunc, const std::string & format, ...); */
int debug(const std::string & func, const std::string & file, const int line, const std::string & format, ...);


/** \brief set name of executable
 *  \param[in] name string of desired executable name
 */
void set_executable_name(const std::string & name);


/** \brief get name of executable
 */
const std::string & get_executable_name();


/** \brief Return true/false whether DEBUG is on/off
 */
bool debug();

/** \brief Set the debug state (true/false = on/off)
 *  \param[in] val true to turn on debug statements
 */
void set_debug(bool val);


/** \brief Get the debug state (true/false = on/off)
 */
bool get_debug();


/** \brief Set the chatter level: 1 (low) - 3 (high)
 *  \param val chatter level 1, 2, or 3
 * 
 *  Note: A chatter level of zero (0) indicates that no messages will be
 *  written to screen (even output and errors).  If ahlog is set up with
 *  a chatter level of zero, this function will have no effect.  Conversely,
 *  a non-zero chatter level cannot be changed to zero with this function.
 */
void set_chatter(int val);


/** \brief Get the chatter level: 1 (low) - 3 (high)
 */
int get_chatter();


/** \brief Set the clobber state (true/false = on/off)
 *  \param[in] val true to turn on clobber statements
 */
void set_clobber(bool clob);


/** \brief Get the clobber state (true/false = on/off)
 */
bool & get_clobber();


/** \brief Initialize logger.
 *  \param[in] execname executable name
 *  \param[in] logfile name of log file; special values: NONE will prevent a
 *   log file from being written, DEFAULT will cause the log file to be named
 *   execname.log where execname is the first parameter value; an empty 
 *   string will result in an error; in addition, if the first character is 
 *   and exclamation point (!), then the given log file will be overwritten
 *  \param[in] chatter level; low -> less output
 *  \param[in] debug true if debug statements are to be printed 
 *  \internal
 *  \note if chatter is zero, then only the log file streams are opened
 *  \note input strings are copied since they are modified in setup()
 */
void setup(std::string execname, std::string logfile, int chatter, 
           bool debug);


/** \brief Shutdown logger.
 */
void shutdown();


/** \brief write header to log file
 *  \param[in] execname executable name to write to header
 */
void write_header(const std::string & execname);


/** \brief write footer to log file
 *  \param[in] status true if program ends with no error, false otherwise
 */
void write_footer(bool status);


/** \brief Return printable chatter level: "(HIGH) " or "(LOW) "
 *  \param chat chatter level
 */
std::string format_chatter(unsigned int chat);

/** \brief Strip return type and argument list from __PRETTY_FUNCTION__.
 *  \param[in] instr value of __PRETTY_FUNCTION__
 *  \return output string
 *  \internal
 *  \note will misbehave given nested parentheses
 */
std::string strip_function(const std::string & instr);


/** \brief Remove all path information from given string.
 *  \param[in] instr full filename including path
 *  \return output string without path
 * 
 *  This function is designed to take the value of argv[0] and return just
 *  the executable name.  It tests for forward- and backward-slashes as the
 *  path separator.
 */
std::string strip_path(const std::string & instr);


/** \brief prepend message message with function
 *  \param[in] msg message
 *  \param[in] func name of function
 * 
 *  This function is designed to be used by the AH_LOCATE macro to prepend
 *  a message with the name of the function originating the message.
 */
std::string prepend(const std::string & msg, const std::string & func);


/** @} */

}  /* namespace ahlog */
#endif /* __cplusplus */

#endif /* AHLOG_AHLOG_H */

/* Revision Log
 $Log: ahlog.h,v $
 Revision 1.41  2015/06/03 01:58:04  rshill
 Corrected C-like ahlog::debug.

 Revision 1.40  2015/05/29 22:39:23  rshill
 Changed ahlog::debug C-style version - needs checking.

 Revision 1.39  2015/05/13 18:30:56  rshill
 Converted C++-style comments to C-style comments.

 Revision 1.38  2013/12/09 15:16:24  peachey
 Merge facilities from st_stream into ahlog. Rename namespace st_stream
 to ahlog. Define a preprocessor macro for st_stream to avoid breaking
 code that refers to that namespace explicitly. Install and use the
 headers moved from st_stream. Do not link to st_stream.

 Revision 1.37  2013/12/02 22:43:29  asargent
 debug() made to internal only

 Revision 1.36  2013/12/02 15:35:04  asargent
 debug() is no longer only internal

 Revision 1.35  2013/11/27 22:05:12  asargent
 New c-callable functions including several set/get and printf style output, info, err, warn and debug functions

 Revision 1.34  2013/11/05 21:27:43  peachey
 1) Rename ahlog::printout to simply ahlog::out.
 2) Add C-compatible header file with basic functionality needed to
    use ahlog to write to output stream (only).
 3) Various new comments and corrections.

 Revision 1.33  2013/11/04 15:01:47  peachey
 Added initial changes to support C calling code: 1) C-compatible header,
 2) printout function to do formatted output to the standard output stream.

 Revision 1.32  2013/10/04 15:20:21  mwitthoe
 ahlog library: ahlog now stores the name of the executable which called setup(); this information is needed by ahfits when logging buffering information to the header of the FITS file

 Revision 1.31  2013/08/19 15:26:38  mwitthoe
 ahlog: fix bug in chatter() function; return type was bool, should be int

 Revision 1.30  2013/08/09 20:36:38  mwitthoe
 ahlog library: accidentally added version macro which requires ahgen; since ahgen requires ahlog, ahlog cannot depend on ahgen

 Revision 1.29  2013/08/09 19:59:03  mwitthoe
 ahlog library: reset chatter level if out-of-range

 Revision 1.28  2013/01/11 16:54:11  mwitthoe
 fix memory leaks in ahlog by deleting pointers or change pointers to non-pointers

 Revision 1.27  2012/11/26 21:22:05  mwitthoe
 add brief descriptions to namespaces in gen

 Revision 1.26  2012/09/15 00:24:36  mwitthoe
 ahlog cannot use the AHVERSION macro; removed

 Revision 1.25  2012/09/14 23:56:56  mwitthoe
 apply version standards to ahlog

 Revision 1.24  2012/08/29 19:42:31  mwitthoe
 changes to ahlog: add set functions for chatter and debug states; remove static instaces of chatter and debug (just refer to those in st_stream); add tests to cycle through values of debug and chatter in testahlog; AH_INFO macro can now take ahlog::OUT as an argument to reproduce the AH_OUT macro output

 Revision 1.23  2012/08/24 19:41:33  mwitthoe
 fix to arguments of macro_info() in ahlog

 Revision 1.22  2012/08/24 19:15:18  mwitthoe
 clean up argument list in ahlog library

 Revision 1.21  2012/08/17 19:23:12  mwitthoe
 tweaks to ahlog

 Revision 1.20  2012/08/17 18:31:08  mwitthoe
 apply standards to ahlog

 Revision 1.19  2012/08/15 16:11:52  mwitthoe
 fix to ahfits, ahlog, ahtime versioning

 Revision 1.18  2012/08/15 15:02:18  mwitthoe
 add versioning macros to ahlog header and source files

 Revision 1.17  2012/06/07 11:05:23  mwitthoe
 ahlog: rdynamic compiler option and missing include

 Revision 1.16  2012/05/31 18:24:27  mwitthoe
 tweaks to ahlog macros; incorporate traceback into main ahlog library

 Revision 1.15  2012/05/30 18:40:21  mwitthoe
 update ahlog Doxygen

 Revision 1.14  2012/05/30 17:38:10  mwitthoe
 improved the safety of the ahlog macros by creating macro_* functions to carry out the majority of the logic, so that the macros are now atomic; removed AH_CHAT_DEBUG

 Revision 1.13  2012/05/29 19:11:10  mwitthoe
 changed macros in ahlog to implement new method of throwing errors and stacktrace; added test program testthrow.cxx

 Revision 1.12  2012/05/29 17:47:22  mwitthoe
 update documentation for ahlog and ahtime

 Revision 1.11  2012/05/23 17:45:36  mwitthoe
 fixed namespace bug AH_ERR macro which prevented it working outside of ahlog

 Revision 1.10  2012/05/17 21:23:39  mwitthoe
 integrate ahtrace into ahlog's AH_ERR macro; trace is only printed when DEBUG is true

 Revision 1.9  2012/05/16 20:12:10  mwitthoe
 change input for DEFAULT log filename and implement clobber using excalamation point

 Revision 1.8  2012/05/10 19:46:45  mwitthoe
 improve header/footer for ahlog and other minor things

 Revision 1.7  2012/04/30 21:55:29  mwitthoe
 changes to ahlog: name of log file can be specified, new behavior for chatter level 0, standard log footer giving elapsed wall time

 Revision 1.6  2012/04/24 19:48:04  mwitthoe
 added documentation to ahlog


*/
