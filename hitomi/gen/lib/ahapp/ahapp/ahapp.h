/// \file ahapp.h
/// \brief Members and functions related to the general tool layout
/// \author James Peachey
/// \date $Date: 2016/04/07 16:42:58 $

#ifndef AHAPP_AHAPP_H
#define AHAPP_AHAPP_H

#include "ahgen/ahversion.h"
AHVERSION(AHAPP_AHAPP,"$Id: ahapp.h,v 1.19 2016/04/07 16:42:58 mwitthoe Exp $")

#include "ahlog/ahlog.h"

#include <string>

/// \brief general tool layout
/// \ingroup mod_ahapp
namespace ahapp {

/** \addtogroup mod_ahapp
 *  @{
 */

/// \brief Perform required standard start-up operations required for
///   all tools. Open log files, pass the command line to ape to open parameter
///   file, and handle global parameters such as clobber and chatter.
/// \param[in] argc The number of command line arguments.
/// \param[in] argv The command line arguments.
/// \param[in] tooltag version string assigned to tool
int startUp(int argc, char ** argv, const std::string& tooltag="");

/// \brief Perform required standard shut-down operations required for
///   all tools. Close log files, call ape to close parameter file, etc.
int shutDown(void);

/// \brief Get the global status variable, 0 for normal execution; other codes
///   denote errors.
int getStatus(void);

/// \brief Set the global status variable to the given value, and return the
///   new status. If the global status is non-0, the status will *not* be changed.
/// \param status The new status to set.
int setStatus(int status);

/// \brief Reset the global status variable to 0.
void resetStatus(void);

/// \brief Get string parameter
/// \param[in] parname name of parameter
std::string getParString(const std::string & parname);

/// \brief try to get parameter value using ahapp::getParString, but
///  if there is a problem, set a flag to false and print a warning
/// \param[in] parname parameter name
/// \param[in,out] okay flag to update with false if there is a problem
///  (note: the value can only change to false, it will never be set to
///  true)
/// \return parameter value read from par-file; upon error, return empty string
std::string getParString_warn(const std::string& parname, bool & okay);

/// \brief Get boolean parameter
/// \param[in] parname name of parameter
bool getParBool(std::string parname);

/// \brief try to get parameter value using ahapp::getParBool, but
///  if there is a problem, set a flag to false and print a warning
/// \param[in] parname parameter name
/// \param[in,out] okay flag to update with false if there is a problem
///  (note: the value can only change to false, it will never be set to
///  true)
/// \return parameter value read from par-file; upon error, return false
bool getParBool_warn(const std::string& parname, bool & okay);

/// \brief Get double parameter
/// \param[in] parname name of parameter
double getParDouble(std::string parname);

/// \brief try to get parameter value using ahapp::getParDouble, but
///  if there is a problem, set a flag to false and print a warning
/// \param[in] parname parameter name
/// \param[in,out] okay flag to update with false if there is a problem
///  (note: the value can only change to false, it will never be set to
///  true)
/// \return parameter value read from par-file; upon error, return 0.0
double getParDouble_warn(const std::string& parname, bool & okay);

/// \brief Get integer parameter
/// \param[in] parname name of parameter
int getParInt(std::string parname);

/// \brief try to get parameter value using ahapp::getParInt, but
///  if there is a problem, set a flag to false and print a warning
/// \param[in] parname parameter name
/// \param[in,out] okay flag to update with false if there is a problem
///  (note: the value can only change to false, it will never be set to
///  true)
/// \return parameter value read from par-file; upon error, return 0
int getParInt_warn(const std::string& parname, bool & okay);

/// \brief Write tool parameters to log file                                                                                                       
void writeParametersToLog();


} // namespace ahapp

/** @} */

#endif      /* AHAPP_AHAPP_h */

/* Revision Log
 $Log: ahapp.h,v $
 Revision 1.19  2016/04/07 16:42:58  mwitthoe
 ahapp: change writeParametersToLog() to write all parameters on a single line instead of one parameter per line

 Revision 1.18  2015/03/31 18:05:41  driethmi
 Added function writeParametersToLog() function, to allow parameter stamping
 for compiled tools.

 Revision 1.17  2014/01/10 16:20:45  peachey
 Per Redmine issue #313, make internal (remove from public header) several
 functions that are not for use outside the library.

 Revision 1.16  2014/01/03 16:44:30  peachey
 Change startUp and shutDown to behave in a more C-like way,
 handling lower exceptions and not throwing any, and returning int status.

 Revision 1.15  2013/11/08 21:35:01  mwitthoe
 ahapp: add alternative getPar functions (e.g. getParDouble_warn()) which will send a warning instead of throwing an error if there is a problem reading a parameter; the new functions also have a 2nd argument which returns true/false based on the success of reading the parameter; these fucntions were originally written in the sxsflagpix tool, but I decided to move them into ahapp since they seemed useful; test cases added for the new functions

 Revision 1.14  2013/11/01 21:28:23  peachey
 Add missing header file <string>.

 Revision 1.13  2013/09/09 14:06:52  mwitthoe
 ahapp library: redirect ape output and error messages through ahlog whenever possible using new ape_out_redirect() and ape_err_redirect() functions; move call to ahlog set up as early as possible in ahapp::startUp() (basically after chatter and debug are read from the parameter file

 Revision 1.12  2013/03/22 16:19:00  mwitthoe
 remove TOOLTAG macro from ahapp.h; this macro shall be defined in each tool

 Revision 1.11  2013/03/21 20:15:31  mwitthoe
 define TOOLTAG macro in ahapp.h instead of each tool; this macro must be updated manually instead of relying on a CVS keyword

 Revision 1.10  2013/03/20 15:35:54  mwitthoe
 assign tool version in startUp() using a new (optional) argument; version is used in the set_toolversion routine from HEAUTILS and will appear in the parameter dump in FITS files.

 Revision 1.9  2012/11/29 18:22:19  mwitthoe
 add getPar functions to ahapp for integers and doubles; standardize errors for these functions and report APE status

 Revision 1.8  2012/11/26 21:22:05  mwitthoe
 add brief descriptions to namespaces in gen

 Revision 1.7  2012/11/01 14:38:35  mwitthoe
 remove clobber state from ahapp; now uses clobber state added to ahgen

 Revision 1.6  2012/10/25 16:45:37  mwitthoe
 ahapp: add parameter retrieval functions (using APE) for string and boolean

 Revision 1.5  2012/10/12 22:48:16  mwitthoe
 revert untagged ahapp to match tagged version; turn on parameter history in startUp()

 Revision 1.3  2012/09/14 23:51:18  mwitthoe
 apply version standards to ahapp

 Revision 1.2  2012/08/29 19:47:30  mwitthoe
 remove static debug and chatter variables from ahapp (they are now only in ahlog); fix some warning messages in testahapp

 Revision 1.1  2012/08/22 21:18:45  mwitthoe
 add ahapp library under gen/lib


*/
