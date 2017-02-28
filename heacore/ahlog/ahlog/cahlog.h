/**
    \file cahlog.h
    \brief C accessible logging facility. For C++ access, use ahlog.h instead. This
      file will work from C++ but it is not needed and includes symbols outside
      any namespace.
    \author James Peachey
    \date $Date: 2015/05/13 18:32:44 $
   
*/
#ifndef AHLOG_CAHLOG_H
#define AHLOG_CAHLOG_H

#include "ahlog/ahlog.h"

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Initialize logger.
    \param[in] execname executable name
    \param[in] logfile name of log file; special values: NONE will prevent a
     log file from being written, DEFAULT will cause the log file to be named
     execname.log where execname is the first parameter value; an empty 
     string will result in an error; in addition, if the first character is 
     and exclamation point (!), then the given log file will be overwritten
    \param[in] chatter level; low -> less output
    \param[in] debug 1 (true) if debug statements are to be printed 
    \internal
    \note if chatter is zero, then only the log file streams are opened
    \note input strings are copied since they are modified in setup()
*/
int ahlog_setup(const char * execname, const char * logfile, int chatter, char debug);


/** \brief Shutdown logger. */
void ahlog_shutdown(void);


/* \brief print stacktrace if in DEBUG mode (used by AH_THROW_*) */
int ahlog_macro_trace();


/** \brief C printf-style output.
    \param[in] func Name of function, result of __func__
    \param[in] format Output C-style formatting string a la printf.
*/
int ahlog_out(const char * func, const char * format, ...);


/** \brief C printf-style info.
    \param[in] func Name of function, result of __func__
    \param[in] format Output C-style formatting string a la printf.
*/
int ahlog_info(int chatter, const char * func, const char * format, ...);


/** \brief C printf-style warn.
    \param[in] func Name of function, result of __func__
    \param[in] format Output C-style formatting string a la printf.
*/
int ahlog_warn(int chatter, const char * func, const char * format, ...);


/** \brief C printf-style err.
    \param[in] func Name of function, result of __func__
    \param[in] format Output C-style formatting string a la printf.
*/
int ahlog_err(const char * func, const char * format, ...);


/** \brief C printf-style debug.
    \param[in] func Name of function, result of __func__
    \param[in] file Name of file, result of __FILE__
    \param[in] line Line number, result of __LINE__
    \param[in] format Output C-style formatting string a la printf.
*/
int ahlog_debug(const char * func, const char * file, const int line, const char * format, ...);


/** \brief set name of executable
 \param[in] name string of desired executable name
*/
void ahlog_set_executable_name(const char* name);


/** \brief get name of executable */
const char* ahlog_get_executable_name();


/** \brief Set the debug state (true/false = on/off)
    \param[in] val true to turn on debug statements
*/
void ahlog_set_debug(char val);


/** \brief Get the debug state (true/false = on/off) */
char ahlog_get_debug();

/** \brief Set the chatter level: 1 (low) - 3 (high)
    \param val chatter level 1, 2, or 3

 Note: A chatter level of zero (0) indicates that no messages will be
 written to screen (even output and errors).  If ahlog is set up with
 a chatter level of zero, this function will have no effect.  Conversely,
 a non-zero chatter level cannot be changed to zero with this function.
*/
void ahlog_set_clobber(char clob);


/** \brief Get the clobber state (true/false = on/off) */
char ahlog_get_clobber();


/** \brief Set the clobber state (true/false = on/off)
    \param[in] val true to turn on clobber statements
*/
void ahlog_set_chatter(int chat);

/** \brief Get the chatter level: 1 (low) - 3 (high) */
int ahlog_get_chatter();

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* AHLOG_CAHLOG_H */

/* Revision Log
 $Log: cahlog.h,v $
 Revision 1.4  2015/05/13 18:32:44  rshill
 Changed ahlog_debug prototype; rearranged some blank records for readability.

 Revision 1.3  2014/08/29 12:15:51  peachey
 Use int not char for chatter.

 Revision 1.2  2013/11/27 22:05:22  asargent
 New c-callable functions including several set/get and printf style output, info, err, warn and debug functions

 Revision 1.1  2013/11/05 21:27:43  peachey
 1) Rename ahlog::printout to simply ahlog::out.
 2) Add C-compatible header file with basic functionality needed to
    use ahlog to write to output stream (only).
 3) Various new comments and corrections.

*/
