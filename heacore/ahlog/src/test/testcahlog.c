/** \file testcahlog.c
    \brief C test program for ahlog library.
    \author James Peachey
    \date $Date: 2015/05/29 20:17:34 $
*/

#define AHLABEL test_testcahlog
#define AHCVSID "$Id: testcahlog.c,v 1.6 2015/05/29 20:17:34 rshill Exp $"

#include "ahlog/cahlog.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

/** \ingroup mod_log */

/** \callgraph */
int ut_main(int status, int chatter, int debug) {

  

    /* use logger for each stream */
    /* +++ 2013-11-05 JP: todo: implement parallel functions to ahlog_out
       +++ 2013-11-05 JP: for the other streams and then uncomment the these other test calls.
    */
    ahlog_out(__func__, "%s %d\n", "This is output, message number ", 0);
    ahlog_info(chatter, __func__, "%s\n", "This is info, message number 1");
    ahlog_warn(chatter, __func__, "%s%s %d\n", "This is warning, ", "message number ", 2);
    ahlog_err(__func__, "%s %1.1f\n", "This is error, message number ", 3.);
    
        
    /* Macro trace status */
    ahlog_out(__func__, "%s %d\n\n", "This is Macro Trace status: ", ahlog_macro_trace());
    
    
  return (status ? 1 : 0);
}

int ut_cycle_debug(int status, int debug) {

  /* Debug level tests */
  ahlog_out(__func__, "\n%s\n", "=== Test switching of debug state ===");
  ahlog_out("__func__", "%s\n", "Debug messages are initially turned off, then will");
  ahlog_out("__func__", "%s\n", "be turned on, and back off again.  For each of these");
  ahlog_out("__func__", "%s\n", "states, a debug message will be printed, only the");
  ahlog_out("__func__", "%s\n", "second message (#2) should be seen. The first (#1)");
  ahlog_out("__func__", "%s\n\n", "will only appear if debug mode is set at startup.");
  
  ahlog_out("__func__", "%s\n", "Start of test");
  ahlog_debug(__func__, __FILE__, __LINE__, "%s\n", "Debug message #1 should not appear unless debug is preset");
  ahlog_set_debug(1);
  ahlog_debug(__func__, __FILE__, __LINE__, "%s\n", "Debug message #2 should appear");
  if(ahlog_get_debug() != 1) status = 1;
  ahlog_set_debug(0);
  if(ahlog_get_debug() != 0) status = 1;
  ahlog_debug(__func__, __FILE__, __LINE__, "%s\n", "Debug message #3 should not appear");

  
  ahlog_out("__func__", "%s\n\n", "End of test.");
  
  return (status ? 1 : 0);
    
}

int ut_execname(int status) {
    
  /* Executable name tests */
  ahlog_set_executable_name("ahlog_exec");
  ahlog_out(__func__, "%s %s\n\n", "This is: ", ahlog_get_executable_name());
  
  return (status ? 1 : 0);

}

int ut_cycle_clobber(int status) {

  /* Clobber on/off tests */
  ahlog_out(__func__, "%s\n", "=== Test switching of clobber state ===");
  ahlog_out("__func__", "%s\n", "Clobber status is initially turned on, then will");
  ahlog_out("__func__", "%s\n", "be turned off again.");
  
  ahlog_out("__func__", "%s\n", "Start of test");
  ahlog_set_clobber(1);
  if(ahlog_get_clobber() != 1) status = 1;
  ahlog_out(__func__, "%s %d\n", "This is clobber on: ", ahlog_get_clobber());
  ahlog_set_clobber(0);
  if(ahlog_get_clobber() != 0) status = 1;
  ahlog_out(__func__, "%s %d\n", "This is clobber off: ", ahlog_get_clobber());
  ahlog_out("__func__", "%s\n\n", "End of test.");

  return (status ? 1 : 0);
}

int ut_cycle_chatter(int status) {

/* Chatter level tests */
  ahlog_out(__func__, "%s\n", "=== Test switching of chatter state ===");
  ahlog_out("__func__", "%s\n", "Chatter state is initially the default (2), then will");
  ahlog_out("__func__", "%s\n", "be switched to LOW (1) and HIGH (3). Only chatter levels ");
  ahlog_out("__func__", "%s\n", "1, 2, and 3 should be displayed.");


  ahlog_out("__func__", "%s\n", "Start of test");
  ahlog_set_chatter(2);
  if(ahlog_get_chatter() != 2) status = 1;
  ahlog_out(__func__, "%s %d\n", "This is the default chatter message level:", ahlog_get_chatter());
  ahlog_set_chatter(1);
  if(ahlog_get_chatter() != 1) status = 1;
  ahlog_out(__func__, "%s %d\n", "LOW chatter level set:", ahlog_get_chatter());
  ahlog_set_chatter(3);
  if(ahlog_get_chatter() != 3) status = 1;
  ahlog_out(__func__, "%s %d\n", "HIGH chatter level set", ahlog_get_chatter());
  ahlog_out("__func__", "%s\n\n", "End of test.");

  return (status ? 1 : 0);
}

/** @} */

int main(int argc, char* argv[]) {
  int chatter = 0;
  char debug = 0;
  char* logfile = 0;
  int retval = 0;
  int status = ape_trad_init(argc, argv);

  if (eOK != status) {
    fprintf(stderr, "Could not open parameter file. Status = %d\n", status);
  }

  if (eOK == status) {
    /* Get parameter: chatter */
    status = ape_trad_query_int("chatter", &chatter);
    if (eOK != status) {
      fprintf(stderr, "Could not get chatter parameter. Status = %d\n", status);
    }
  }

  if (eOK == status) {
    /* Get parameter: debug */
    status = ape_trad_query_bool("debug", &debug);
    if (eOK != status) {
      fprintf(stderr, "Could not get debug parameter. Status = %d\n", status);
    }
  }

  if (eOK == status) {
    /* Get parameter: logfile */
    status = ape_trad_query_string("logfile", &logfile);
    if (eOK != status) {
      fprintf(stderr, "Could not get logfile parameter. Status = %d\n", status);
    }
  }

  if (eOK == status) {
    /* start logger */
      status = ahlog_setup("testcahlog", logfile, chatter, debug);

      if (0 != status) {
        fprintf(stderr, "testcahlog: unable to set up logging.\n");
      }
      if (0 == status) {
        /* Test error handling case for null pointers. Should not throw an exception but just return -1. */
        retval = ahlog_out(__func__, 0, "This should not be printed\n", 0);
        if (-1 != retval) {
          fprintf(stderr, "testcahlog: attempt to output using a null format string returned %d, not -1 as expected.\n", retval);
          status = 1;
        }
        status = ut_main(status, chatter, debug);
        if (eOK != status) {
          fprintf(stderr, "Error from C unit test. Status = %d\n", status);
        }
        status = ut_cycle_debug(status, debug);
        if (eOK != status) {
          fprintf(stderr, "Error from debug test. Status = %d\n", status);
        }
        status = ut_execname(status);
        if (eOK != status) {
          fprintf(stderr, "Error from executable name test. Status = %d\n", status);
        }
        status = ut_cycle_clobber(status);
        if (eOK != status) {
          fprintf(stderr, "Error from clobber test. Status = %d\n", status);
        }
        status = ut_cycle_chatter(status);
        if (eOK != status) {
          fprintf(stderr, "Error from chatter test. Status = %d\n", status);
        }
        ahlog_shutdown();

        /* If shutdown is functioning correctly, none of these messages should appear. */
        ahlog_out("ut_main", "after ahlog_shutdown, this was written with ahlog_out. It should not appear anywhere.\n");
        ahlog_info(HIGH, "ut_main", "after ahlog_shutdown, this was written with ahloginfo. It should not appear anywhere.\n");
        ahlog_warn(HIGH, "ut_main", "after ahlog_shutdown, this was written with ahloginfo. It should not appear anywhere.\n");
        ahlog_err("ut_main", "after ahlog_shutdown, this was written with ahlog_err. It should not appear anywhere.\n");
      }
  }

  free(logfile); logfile = 0;

  return 0 != status ? 1 : 0;
}

/*
 * $Log: testcahlog.c,v $
 * Revision 1.6  2015/05/29 20:17:34  rshill
 * Updated calling sequence for ahlog_debug().
 *
 * Revision 1.5  2014/05/19 18:10:20  peachey
 * Clarify purpose of messages written after shutdown.
 *
 * Revision 1.4  2014/05/19 17:50:47  peachey
 * Clean up the log file during shutdown (see issue #386). Add cvs log to
 * testcahlog.c.
 *
 */
