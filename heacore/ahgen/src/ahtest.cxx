/// \file ahtest.cxx
/// \brief Declarations of public members of the test support library.
/// \author James Peachey
/// \date $Date: 2014/02/03 22:39:41 $

#define AHLABEL ahgen_ahtest
#define AHCVSID "$Id: ahtest.cxx,v 1.12 2014/02/03 22:39:41 mwitthoe Exp $"

#include "ahgen/ahtest.h"
#include "ahgen/ahgen.h"
#include "ahlog/ahlog.h"

#include <sstream>
#include <cmath>

namespace ahgen {

/// \callgraph
void testReport(const LocalVars & lvars) {
  std::stringstream exc;
  if (lvars.m_exception != "") exc << std::endl << "    " << lvars.m_exception;
  if(lvars.m_ok) {
    AH_INFO(ahlog::HIGH) << "Passed: " << lvars.m_section << exc.str() << std::endl;
  } else {
    AH_ERR << "Failed: " << lvars.m_section << exc.str() << std::endl;
  }
  std::list<std::string>::const_iterator it;
  for (it=lvars.m_msg.begin();it!=lvars.m_msg.end();it++)
    AH_INFO(ahlog::HIGH) << "   --- " << *it << std::endl;
}

/// \callgraph
int finalReport() {
  int out=ahgen::getTestStatus();
  if (0 == out)
    AH_OUT << "Unit test PASSED" << std::endl;
  else
    AH_OUT << "Unit test FAILED  (Status: " << out << ")" << std::endl;
  return out;
}

static int s_test_status = 0;

int getTestStatus(void) { return s_test_status; }

int setTestStatus(int status) { if (0 == s_test_status) s_test_status = status; return s_test_status; }

void resetTestStatus(void) { s_test_status = 0; }

bool isEqual(double x, double y, double tol) {
  // if x or y are zero, do absolute comparison...
  if (0. == x || 0. == y) {
    if (tol < std::abs(x-y)) return false;
  } else {   // else a relative comparison
    if (tol < (2.0*std::abs(x-y)/std::abs(x+y))) return false;
  }
  return true;
}


}  /* namespace ahgen */

/* Revision Log
 $Log: ahtest.cxx,v $
 Revision 1.12  2014/02/03 22:39:41  mwitthoe
 ahtest (ahgen): change test messages to more closely match aht output

 Revision 1.11  2013/09/17 17:57:46  mwitthoe
 ahgen/ahtest: add optional argument to ahgen::isEqual() which allows a custom tolerance, default value is 1.e-10 (as before); if comparing against zero, change to absolute instead of relative difference, since the latter will always be 1. or undefined

 Revision 1.10  2013/01/22 14:18:49  mwitthoe
 capitalize FAILURE in ahtest summary to ea

 Revision 1.9  2012/11/07 18:58:10  mwitthoe
 change output chatter level used by FAILTEXT (LOW -> HIGH)

 Revision 1.8  2012/10/23 19:39:13  peachey
 Fix typo: pased -> passed.

 Revision 1.7  2012/10/11 17:58:37  mwitthoe
 ahgen: make isEqual() test relative to magnitude instead of absolute

 Revision 1.6  2012/09/18 01:05:07  mwitthoe
 add isEqual() function to ahtest to help compare doubles

 Revision 1.5  2012/09/14 23:55:24  mwitthoe
 apply version standards to ahgen

 Revision 1.4  2012/08/29 18:55:39  mwitthoe
 in ahtest, include exception message, if caught

 Revision 1.3  2012/08/22 18:23:29  mwitthoe
 give ahtest its own status variable instead of using the (defunct) status from ahgen; this change does not affect usage

 Revision 1.2  2012/08/21 19:14:13  mwitthoe
 optional messages in ahtest are now printed as low-priority information instead of errors

 Revision 1.1  2012/08/21 19:06:29  mwitthoe
 implement testing method in ahgen/ahtest


*/
