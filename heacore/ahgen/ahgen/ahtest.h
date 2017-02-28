/// \file ahtest.h
/// \brief Declarations of public members of the test support library.
/// \author James Peachey
/// \date $Date: 2013/09/17 17:57:45 $

/// \addtogroup mod_ahgen
/// \section ahgen_ahtest Library Test Macros - ahtest
///
/// This library provides macros to help with the testing of libraries.
/// The macros allow for individual tests to be isolated from one another
/// and keep track of an overall PASS/FAIL state.  See below for an example.
///
/// \code
/// LABEL_TEST("Provide a label for a series of tests.");
///
/// // check for success 
/// START_TEST("test 1") {
///  int x=1;
///  int y=2;
///  if (ahmath::add(x,y) != 3) FAIL;
/// } END_TEST
///
/// // another check for success with custom failure text
/// START_TEST("test 2") {
///  int x=4;
///  int y=6;
///  if (ahmath::add(x,y) != 10) FAILTEXT("addition failed!");
/// } END_TEST
///
/// // sometimes you expect something to fail
/// START_TEST_EXCEPTION("test 3") {
///  int x=3;
///  int y=0;
///  ahmath::divide(x,y);   // will throw exception
/// } END_TEST
/// \endcode
///
/// Running this test will give in the output:
///
/// \code
/// testcode: INFO: +++ Provide a label for a series of tests.
/// testcode: INFO: Passed: test 1
/// testcode: INFO: Passed: test 2
/// testcode: INFO: Passed: test 3
///   ahmath::divide() -> cannot divide by zero
/// \endcode
///
/// Note ahmath::add() and ahmath::divide() do not exist.
///


#ifndef AHGEN_AHTEST_H
#define AHGEN_AHTEST_H 

#include "ahgen/ahversion.h"
AHVERSION(AHGEN_AHTEST,"$Id: ahtest.h,v 1.13 2013/09/17 17:57:45 mwitthoe Exp $")

#include <string>
#include <list>

/// \ingroup mod_ahgen
/// \brief set test condition to PASS
#define PASS (localtestvars.m_ok=true)

/// \ingroup mod_ahgen
/// \brief set test condition to FAIL
#define FAIL (localtestvars.m_ok=false)

/// \ingroup mod_ahgen
/// \brief set test condition to FAIL
/// \param x message to append to message list
#define FAILTEXT(x) (localtestvars.m_msg.push_back(x),\
                     localtestvars.m_ok=false)

/// \ingroup mod_ahgen
/// \brief set label for test suite
/// \param x label
#define LABEL_TEST(x) (AH_INFO(ahlog::HIGH) << "+++ " << x << std::endl)

/// \ingroup mod_ahgen
/// \brief start a try-block for testing
/// \param x test section heading
#define START_TEST(x) { ahgen::LocalVars localtestvars(x,true); try {

/// \ingroup mod_ahgen
/// \brief start a try-block for testing an exception
/// \param x test section heading
#define START_TEST_EXCEPTION(x) { ahgen::LocalVars localtestvars(x,false); try {

/// \ingroup mod_ahgen
/// \brief end testing try-block
#define END_TEST } catch (const std::exception & x) { \
                   localtestvars.m_exception=x.what(); \
                   localtestvars.m_ok=localtestvars.m_testexc; } \
                 ahgen::testReport(localtestvars); \
                 if (!localtestvars.m_ok) ahgen::setTestStatus(1); }

/// \ingroup mod_ahgen
/// \brief tolerance used for comparing doubles in isEqual()
#define COMPTOL (1.e-10)


/// \ingroup mod_ahgen
namespace ahgen {

/** \addtogroup mod_ahgen
 *  @{
 */

/// \brief Structure to hold local variables for each test block; these are
///   only to be accessed by the test macros in this header.
struct LocalVars {

  /// \brief constructor
  /// \param section name of test block
  /// \param ok true for normal test; false if checking for exception
  LocalVars(std::string section,bool ok) : m_section(section),m_exception(""),
                                           m_ok(ok),m_testexc(!ok) {};

  std::string m_section;          ///< test section name
  std::string m_exception;        ///< exception message
  std::list<std::string> m_msg;   ///< list of optional messages
  bool m_ok;                      ///< true if all local test passed
  bool m_testexc;                 ///< true if looking for exception (default: false)
};

/// \brief write result of test to log streams
/// \param[in] lvars set of local variables
void testReport(const LocalVars & lvars);

/// \brief write if all tests passed or a failure occurred (using getTestStatus() 
///  from ahgen)
/// \return value of getTestStatus()
int finalReport();

/// \brief Get the test status variable, 0 for normal execution; other codes
///   denote errors.
int getTestStatus(void);

/// \brief Set the test status variable to the given value, and return the
///   new status. If the current status is non-0, then it will *not* be changed.
/// \param status The new status to set.
int setTestStatus(int status);

/// \brief Reset the global status variable to 0.
void resetTestStatus(void);

/// \brief Return true, if both values are the same within a set tolerance.
/// \param[in] x first value
/// \param[in] y second value
/// \param[in] tol optionally tolerance (default: COMPTOL)
/// \return true/false
bool isEqual(double x, double y, double tol=COMPTOL);

/** @} */

}  // namespace ahgen

#endif /* AHGEN_AHTEST_H */


/* Revision Log
 $Log: ahtest.h,v $
 Revision 1.13  2013/09/17 17:57:45  mwitthoe
 ahgen/ahtest: add optional argument to ahgen::isEqual() which allows a custom tolerance, default value is 1.e-10 (as before); if comparing against zero, change to absolute instead of relative difference, since the latter will always be 1. or undefined

 Revision 1.12  2013/01/24 17:44:30  mwitthoe
 update Doxygen for ahgen

 Revision 1.11  2012/10/16 14:42:21  mwitthoe
 remove old testing macros from ahtest.h

 Revision 1.10  2012/09/18 01:05:07  mwitthoe
 add isEqual() function to ahtest to help compare doubles

 Revision 1.9  2012/09/14 23:55:24  mwitthoe
 apply version standards to ahgen

 Revision 1.8  2012/08/29 18:55:39  mwitthoe
 in ahtest, include exception message, if caught

 Revision 1.7  2012/08/23 21:29:28  mwitthoe
 tweak LABEL_TEST macro in ahtest.h

 Revision 1.6  2012/08/22 18:23:28  mwitthoe
 give ahtest its own status variable instead of using the (defunct) status from ahgen; this change does not affect usage

 Revision 1.5  2012/08/21 19:06:28  mwitthoe
 implement testing method in ahgen/ahtest

 Revision 1.4  2012/04/13 17:30:30  peachey
 Use new ahlog macros (that use st_stream.

 Revision 1.3  2012/03/27 21:03:33  peachey
 Use AhLog::info() instead of AhLog::log() because this change was just
 made in ahlog.

 Revision 1.2  2012/02/03 15:17:36  peachey
 Remove unnecessary "using" statements from TRY and TRYEXCEPTION macros.
 Bad practice to include such things in macros that would be used in many
 places.

 Revision 1.1  2012/01/31 22:23:33  peachey
 Add first version of general support library.

*/
