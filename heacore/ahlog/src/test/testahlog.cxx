/** \file testahlog.cxx
    \brief Test program for ahlog library.
    \author Mike Witthoeft
    \date $Date: 2015/06/03 01:54:55 $
*/

#define AHLABEL test_testahlog
#define AHCVSID "$Id: testahlog.cxx,v 1.25 2015/06/03 01:54:55 rshill Exp $"

#include <string.h>
#include <cstdlib>
#include <stdexcept>

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include "ahlog/ahlog.h"
#include "sys/time.h"


/// \ingroup mod_log
namespace ahlog {

  void ut_cstyle();

  /** \addtogroup mod_ahlog
   *  @{
   */

  // ---------------------------------------------------------------------------

  void ut_sample_function() {

    AH_OUT << "This is an output message." << std::endl;

    AH_INFO(ahlog::LOW) << "This is LOW priority information." << std::endl;
    AH_INFO(ahlog::HIGH) << "This is HIGH priority information." << std::endl;
    AH_INFO(ahlog::OUT) << "This is output sent from AH_INFO." << std::endl;

    AH_WARN(ahlog::LOW) << "This is a LOW priority warning." << std::endl;
    AH_WARN(ahlog::HIGH) << "This is a HIGH priority warning." << std::endl;

    AH_DEBUG << "This is a debug message." << std::endl;

    ut_cstyle();

    AH_THROW_RUNTIME("This is a thrown error");
//    AH_ERR << "This is an error message." << std::endl;

    return;
  }

  // ---------------------------------------------------------------------------

  void ut_sample_class() {

    class TestClass {
      private:
        int m_x;
        int m_y;

      public:

        void sample_method() {
          AH_OUT << "This is an output message." << std::endl;

          AH_INFO(ahlog::LOW) << "This is LOW priority information." << std::endl;
          AH_INFO(ahlog::HIGH) << "This is HIGH priority information." << std::endl;
          AH_INFO(ahlog::OUT) << "This is output sent from AH_INFO." << std::endl;

          AH_WARN(ahlog::LOW) << "This is a LOW priority warning." << std::endl;
          AH_WARN(ahlog::HIGH) << "This is a HIGH priority warning." << std::endl;

          AH_DEBUG << "This is a debug message." << std::endl;

          AH_THROW_RUNTIME("This is a thrown error");
//          AH_ERR << "This is an error message." << std::endl;

          return;
        }

    };   // end TestClass

    TestClass object;
    object.sample_method();

  }  // end ut_sample_class

  // ---------------------------------------------------------------------------

  void ut_trace4() {
    AH_INFO(ahlog::HIGH) << "in traceback function4" << std::endl;
    AH_THROW_RUNTIME("throwing from function4");
    AH_INFO(ahlog::LOW) << "leaving function4 (should not see this)" 
                        << std::endl;
  }

  // ---------------------------------------------------------------------------

  void ut_trace3() {
    AH_INFO(ahlog::HIGH) << "in traceback function3" << std::endl;
    ut_trace4();
    AH_INFO(ahlog::LOW) << "leaving function3" << std::endl;
  }

  // ---------------------------------------------------------------------------

  void ut_trace2() {
    AH_INFO(ahlog::HIGH) << "in traceback function2" << std::endl;
    ut_trace3();
    AH_INFO(ahlog::LOW) << "leaving function2" << std::endl;
  }

  // ---------------------------------------------------------------------------

  void ut_trace1() {
    AH_INFO(ahlog::HIGH) << "in traceback function1" << std::endl;
    ut_trace2();
    AH_INFO(ahlog::LOW) << "leaving function1" << std::endl;
  }

  // ---------------------------------------------------------------------------

  bool ut_cycle_debug() {

    // debug test
    AH_OUT << "=== Test switching of debug state ===" << std::endl;
    AH_OUT << "Debug messages are initially turned off, then will" << std::endl;
    AH_OUT << "be turned on, and back off again.  For each of these" << std::endl;
    AH_OUT << "states, a debug message will be printed, only the" << std::endl;
    AH_OUT << "second message (#2) should be seen." << std::endl << std::endl;
    AH_OUT << "Start of test" << std::endl;

    AH_DEBUG << "Message #1: should not appear" << std::endl;
    ahlog::set_debug(true);
    AH_DEBUG << "Message #2: should appear" << std::endl;
    if(ahlog::get_debug() != true) return 1;
    ahlog::set_debug(false);
    AH_DEBUG << "Message #3: should not appear" << std::endl;
    if(ahlog::get_debug() != false) return 1;
    AH_OUT << "End of test" << std::endl;

    return 0;

  }
  
  // ---------------------------------------------------------------------------
 
  bool ut_cycle_clobber() {

    // debug test
    AH_OUT << "=== Test switching of clobber state ===" << std::endl;
    AH_OUT << "Clobber is initially turned off, then will" << std::endl;
    AH_OUT << "be turned on, and back off again.  For each of these" << std::endl;
    AH_OUT << "states, a clobber message will be printed, only the" << std::endl;
    AH_OUT << "second message (#2) should be seen." << std::endl << std::endl;
    AH_OUT << "Start of test" << std::endl;

    ahlog::set_clobber(true);
    AH_OUT << "Message #2: should appear" << std::endl;
    if(ahlog::get_clobber() != true) return 1;
    ahlog::set_clobber(false);
    if(ahlog::get_clobber() != false) {
      AH_OUT << "Message #3: should not appear" << std::endl;
      return 1;
    }
    AH_OUT << "End of test" << std::endl;

    return 0;
  }
 

  // ---------------------------------------------------------------------------

  bool ut_cycle_chatter() {


    std::string msg_yes="You should see this message";
    std::string msg_no="You should NOT see this message";

    AH_OUT << "=== Test switching of chatter level ===" << std::endl;
    AH_OUT << "This test will cycle through all non-zero chatter states" << std::endl;
    AH_OUT << "starting with 1 (minimal output to screen) to 3 (all" << std::endl;
    AH_OUT << "output to screen).  (All output is always sent to" << std::endl;
    AH_OUT << "the log file.)  For each chatter level, six messages" << std::endl;
    AH_OUT << "will be sent: output, low-priority info, high-priority" << std::endl;
    AH_OUT << "info, low-priority warning, high-priority warning, and" << std::endl;
    AH_OUT << "error." << std::endl;

    ahlog::set_chatter(1);
    if(ahlog::get_chatter() != 1) return 1;
    AH_OUT << std::endl;
    AH_OUT << "Chatter: 1" << std::endl;
    ahlog::set_chatter(1);
    if(ahlog::get_chatter() != 1) return 1;
    AH_OUT               << msg_yes << std::endl;
    AH_INFO(ahlog::LOW)  << msg_no << std::endl;
    AH_INFO(ahlog::HIGH) << msg_no << std::endl;
    AH_WARN(ahlog::LOW)  << msg_no << std::endl;
    AH_WARN(ahlog::HIGH) << msg_no << std::endl;
    AH_ERR               << msg_yes << std::endl;

    ahlog::set_chatter(1);
    if(ahlog::get_chatter() != 1) return 1;
    AH_OUT << std::endl;
    AH_OUT << "Chatter: 2" << std::endl;
    ahlog::set_chatter(2);
    if(ahlog::get_chatter() != 2) return 1;
    AH_OUT               << msg_yes << std::endl;
    AH_INFO(ahlog::LOW)  << msg_no << std::endl;
    AH_INFO(ahlog::HIGH) << msg_yes << std::endl;
    AH_WARN(ahlog::LOW)  << msg_no << std::endl;
    AH_WARN(ahlog::HIGH) << msg_yes << std::endl;
    AH_ERR               << msg_yes << std::endl;

    ahlog::set_chatter(1);
    if(ahlog::get_chatter() != 1) return 1;
    AH_OUT << std::endl;
    AH_OUT << "Chatter: 3" << std::endl;
    ahlog::set_chatter(3);
    if(ahlog::get_chatter() != 3) return 1;
    AH_OUT               << msg_yes << std::endl;
    AH_INFO(ahlog::LOW)  << msg_yes << std::endl;
    AH_INFO(ahlog::HIGH) << msg_yes << std::endl;
    AH_WARN(ahlog::LOW)  << msg_yes << std::endl;
    AH_WARN(ahlog::HIGH) << msg_yes << std::endl;
    AH_ERR               << msg_yes << std::endl;

    ahlog::set_chatter(1);
    if(ahlog::get_chatter() != 1) return 1;
    AH_OUT << std::endl;
    AH_OUT << "Chatter: 1 (again)" << std::endl;
    ahlog::set_chatter(1);
    if(ahlog::get_chatter() != 1) return 1;
    AH_OUT               << msg_yes << std::endl;
    AH_INFO(ahlog::LOW)  << msg_no << std::endl;
    AH_INFO(ahlog::HIGH) << msg_no << std::endl;
    AH_WARN(ahlog::LOW)  << msg_no << std::endl;
    AH_WARN(ahlog::HIGH) << msg_no << std::endl;
    AH_ERR               << msg_yes << std::endl;

    return 0;
    
  }

  int chatter = 1;
  void ut_cstyle() {
    ahlog::out("ut_cstyle", "C-style formatted output ahlog::out: %s %d\n", "output number", 0);
    ahlog::info(chatter, "ut_cstyle", "C-style formatted output ahlog::info: %s %d\n", "output number", 1);
    ahlog::warn(chatter, "ut_cstyle", "C-style formatted output ahlog::warn: %s %d\n", "output number", 2);
    ahlog::err("ut_cstyle", "C-style formatted output ahlog::err: %s %1.1f\n", "output number", 3.);
    ahlog::debug("ut_cstyle", __FILE__, __LINE__, "C-style formatted output ahlog::debug: %s %u\n", "output number", 4u);
  }

  bool ut_execname() {

    std::string ename = "";

    ahlog::set_executable_name("executable");
    ename = ahlog::get_executable_name();
    AH_OUT << "The executable name is: " << ename << std::endl;

    if(ename != "executable") return 1;

    return 0;

  }

  // ---------------------------------------------------------------------------

  /// \callgraph
  void ut_main(char* runtype, char* logfile, int chatter, bool debug) {

    bool status = 0;

    // start logger
    ahlog::setup("testahlog",logfile,chatter,debug);

    // do work
    try {
      if (!strcmp(runtype,"function")) {
        ut_sample_function();
      } else if (!strcmp(runtype,"class")) {
        ut_sample_class();
      } else if (!strcmp(runtype,"stacktrace")) {
        ut_trace1();
      } else if (!strcmp(runtype,"cycle_debug")) {
        ut_cycle_debug();
        if(eOK != status) {
          AH_ERR << "Error in debug cycle" << std::endl;
        }
      } else if (!strcmp(runtype,"cycle_chatter")) {
        status = ut_cycle_chatter();
        if(eOK != status) {
          AH_ERR << "Error in chatter cycle" << std::endl;
        }
      } else if (!strcmp(runtype,"cycle_clobber")) {
        status = ut_cycle_clobber();
        if(eOK != status) {
          AH_ERR << "Error in clobber cycle" << std::endl;
        }
      } else if (!strcmp(runtype,"execname")) {
        status = ut_execname();
        if(eOK != status) {
          AH_ERR << "Error in executable naming" << std::endl;
        }
      } else {
        AH_THROW_RUNTIME("invalid value for runtype");
      }
    } catch (const std::exception & x) {
      AH_ERR << x.what() << std::endl;
    }

    // close logger
    ahlog::shutdown();
  }

  /** @} */

}   // end namespace

// *****************************************************************************



int main(int argc, char* argv[]) {

  int status = ape_trad_init(argc, argv);
  if (eOK != status) {
    std::cout << "could not access parameter file" << std::endl;
    return 1;
  }

  // get parameter: chatter
  int chatter;
  status = ape_trad_query_int("chatter", &chatter);

  // get parameter: debug
  char t_debug=0;
  bool debug=false;
  status = ape_trad_query_bool("debug", &t_debug);
  if (eOK != status) {
    // Ignore error -- debug has default value.
  } else {
    debug = 0 != t_debug ? true : false;
  }

  // get parameter: logfile
  char* logfile;
  status = ape_trad_query_string("logfile",&logfile);

  // get parameter: runtype
  char* runtype;
  status = ape_trad_query_string("runtype", &runtype);

  int outval=0;
  try {
    ahlog::ut_main(runtype,logfile,chatter,debug);
  } catch (...) {
    outval=1;
  }

  free(logfile); logfile=0;
  free(runtype); runtype=0;

  return outval;
}
