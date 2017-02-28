/**
    \file testheaapp.c
    \brief Test heaapp facility for allowing client applications to select,
      initialize and use various available HEASoft libraries for standard
      support functions, such as parameter handling and output/error logging.
      From the client perspective, every HEASoft library is optional. No
      library depends on heaapp at compile time.
    \author James Peachey
    \date $Date: 2014/09/12 15:40:19 $
*/
#include "heaapp/heaapp.h"
#include "heaapp/heaapp_ahlog.h"
#include "heaapp/heaapp_ape.h"
#include "heaapp/heaapp_heautils.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Test cases. */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/** \brief Test setting up logging first, then parameter access. All output should
      appear, and chatter settings should be obeyed, even though the chatter
      parameter is set *after* logging is set up. */
int heaapp_test_log_par(void);

/** \brief Test setting up parameter access, then logging. All output should
      appear, and chatter settings should be obeyed, even though logging
      is set up *after* parameter access is set up. */
int heaapp_test_par_log(void);

/** \brief Test a common combination of ape, ahlog, and heautils
      initializations. */
int heaapp_test_ape_ahlog_heautils(void);

/** \brief Test caching and flushing messages. To allow logging to be set up at
      any time, heaapp provides a logging facility which caches output for display
      after logging is set up. */
int heaapp_test_msg(void);

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* "Fake" functions: functions provided as input to the test cases. */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/* Artificial functions that simulate the hooks a HEASoft library would need
   to be acceessible via heaapp. */
/* +++ CR 2014-09-05 JP need more comments in this file in general, but especially
what the fake functions are v. the functions used for test output etc. */

/* "Fake" logging library heaapp hooks: */
static int fakelog_heaapp_connect(HeaAppData * appdata);

static int fakelog_heaapp_initialize(HeaAppData * appdata);

static void fakelog_heaapp_finalize(HeaAppData * appdata);

/* "Fake" parameter file library heaapp hooks: */
static int fakepar_heaapp_connect(HeaAppData * appdata);

static int fakepar_heaapp_initialize(HeaAppData * appdata);

static void fakepar_heaapp_finalize(HeaAppData * appdata);

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Internal utilities used by the unit test. */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/* Format and report real error messages delivered by this test. */
int test_printerr(const char * func, const char * fmt, ...);

/* Format and report real messages delivered by this test. */
int test_printout(const char * func, const char * fmt, ...);

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Main code */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
int main(int testargc, char ** testargv) {
  int teststatus = 0;

  /* Test initializing fake libraries in order log, then par. This is the reverse
     of the usual order but it should work. */
  if (0 != heaapp_test_log_par()) teststatus = 1;

  /* Put a blank line between tests. */
  test_printout("", "\n");

  /* Test initializing fake libraries in order par, then log. */
  if (0 != heaapp_test_par_log()) teststatus = 1;

  /* Put a blank line between tests. */
  test_printout(0, "\n");

  test_printout(0, "Resetting PFILES to . to find heaapp_ape.par.\n");
  setenv("PFILES", ".", 1);

  /* Test initializing ahlog library. */
  if (0 != heaapp_ahlog_test()) teststatus = 1;

  /* Test initializing ape library. */
  if (0 != heaapp_ape_test()) teststatus = 1;

  /* Test initializing heautils library. */
  if (0 != heaapp_heautils_test()) teststatus = 1;

  /* Test initializing a common combination of libraries (. */
  if (0 != heaapp_test_ape_ahlog_heautils()) teststatus = 1;

  /* Put a blank line between tests. */
  test_printout(0, "\n");

  /* Test message caching facilities. This test must be last because it caches
     messages that should not be flushed, and other test cases flush messages. */
  if (0 != heaapp_test_msg()) teststatus = 1;

  /* Final message and error return. */
  if (0 == teststatus) printf("testheaapp: Unit test PASSED.\n");
  else fprintf(stderr, "testheaapp: Unit test FAILED.\n");

  return teststatus;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Test cases. */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
int heaapp_test_log_par(void) {
  int teststatus = 0;

  test_printout(__PRETTY_FUNCTION__, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  test_printout(__PRETTY_FUNCTION__, "about to run test case for log, then par initializations.\n");
  test_printout(__PRETTY_FUNCTION__, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  { int status = 0; int expected_status = 0; char * appname = "/path/to/testheaapp"; int argc = 1; char ** argv = &appname;
    HeaAppData appdata = heaapp_construct_appdata(argc, argv);
    if (0 == status) {
      /* Connect the logging functions first. Note this will trigger setting up (fake) logging without
         any log file, since the name of the log file is not known when the fake log initialize function
         is called. */
      status = fakelog_heaapp_connect(&appdata);
      if (expected_status != status) {
        teststatus = 1; test_printerr(__PRETTY_FUNCTION__,
          "fakelog_heaapp_connect returned %d, not %d as expected.\n", status, expected_status);
      }
    }
    if (0 == status) {
      /* Connect the par functions second. This will simulate getting the log file name from the user.
         In a real implementation, the log file name would presumably be ignored, because logging was
         set up before par functions. */
      status = fakepar_heaapp_connect(&appdata);
      if (expected_status != status) {
        teststatus = 1; test_printerr(__PRETTY_FUNCTION__,
          "fakepar_heaapp_connect returned %d, not %d as expected.\n", status, expected_status);
      }
    }
    if (0 == status) {
      /* Print a message before initializing. This is to demonstrate that messages go out in the same order
         as they come in, even though logging doesn't start until after initialize. */
      appdata.printout(__PRETTY_FUNCTION__, "Before heaapp_initialize, printed this with appdata.printout\n");

      /* Initialize should print some messages about what the fake intializers do. */
      status = heaapp_initialize(&appdata);
      if (expected_status != status) {
        teststatus = 1; test_printerr(__PRETTY_FUNCTION__,
          "heaapp_initialize returned %d, not %d as expected.\n", status, expected_status);
      }
    }
    if (0 == status) {
      appdata.printout(__PRETTY_FUNCTION__, "After heaapp_initialize, printed this with appdata.printout.\n");
      appdata.printerr(__PRETTY_FUNCTION__, "After heaapp_initialize, printed this with appdata.printerr.\n");
      appdata.printinfo(3, __PRETTY_FUNCTION__, "After heaapp_initialize, printed this with appdata.printinfo.\n");
      appdata.printwarn(3, __PRETTY_FUNCTION__, "After heaapp_initialize, printed this with appdata.printwarn.\n");
    }
    if (0 == status) {
      status = heaapp_finalize(&appdata);
    }
    if (expected_status != status) {
      teststatus = 1; test_printerr(__PRETTY_FUNCTION__,
        "heaapp_finalize returned %d, not %d as expected.\n", status, expected_status);
    }
  }
  test_printout(__PRETTY_FUNCTION__, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  test_printout(__PRETTY_FUNCTION__, "done with test case for log, then par initializations.\n");
  test_printout(__PRETTY_FUNCTION__, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  return teststatus;
}

int heaapp_test_par_log(void) {
  int teststatus = 0;

  test_printout(__PRETTY_FUNCTION__, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  test_printout(__PRETTY_FUNCTION__, "about to run test case for par, then log initializations.\n");
  test_printout(__PRETTY_FUNCTION__, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  { int status = 0; int expected_status = 0; char * appname = "/path/to/testheaapp"; int argc = 1; char ** argv = &appname;
    HeaAppData appdata = heaapp_construct_appdata(argc, argv);
    if (0 == status) {
      /* Connect the par functions first. Note this will get the log file name and print a message,
         which will be cached until logging is actually set up. */
      status = fakepar_heaapp_connect(&appdata);
      if (expected_status != status) {
        teststatus = 1; test_printerr(__PRETTY_FUNCTION__, "fakepar_heaapp_connect returned %d, not %d as expected.\n", status, expected_status);
      }
    }
    if (0 == status) {
      /* Connect the logging functions second. This will simulate opening the log file. A real
         implementation would be free to write to the log file, standard outputs or both. */
      status = fakelog_heaapp_connect(&appdata);
      if (expected_status != status) {
        teststatus = 1; test_printerr(__PRETTY_FUNCTION__, "fakelog_heaapp_connect returned %d, not %d as expected.\n", status, expected_status);
      }
    }
    if (0 == status) {
      /* Print a message before initializing. This is to demonstrate that messages go out in the same order
         as they come in, even though logging doesn't start until after initialize. */
      appdata.printout(__PRETTY_FUNCTION__, "Before heaapp_initialize, printed this with appdata.printout\n");
      status = heaapp_initialize(&appdata);
      if (expected_status != status) {
        teststatus = 1; test_printerr(__PRETTY_FUNCTION__, "heaapp_initialize returned %d, not %d as expected.\n", status, expected_status);
      }
    }
    if (0 == status) {
      appdata.printout(__PRETTY_FUNCTION__, "After heaapp_initialize, printed this with appdata.printout.\n");
      appdata.printerr(__PRETTY_FUNCTION__, "After heaapp_initialize, printed this with appdata.printerr.\n");
      appdata.printinfo(3, __PRETTY_FUNCTION__, "After heaapp_initialize, printed this with appdata.printinfo.\n");
      appdata.printwarn(3, __PRETTY_FUNCTION__, "After heaapp_initialize, printed this with appdata.printwarn.\n");
    }
    if (0 == status) {
      status = heaapp_finalize(&appdata);
    }
    if (expected_status != status) {
      teststatus = 1; test_printerr(__PRETTY_FUNCTION__, "heaapp_finalize returned %d, not %d as expected.\n", status, expected_status);
    }
  }
  test_printout(__PRETTY_FUNCTION__, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  test_printout(__PRETTY_FUNCTION__, "done with test case for par, then log initializations.\n");
  test_printout(__PRETTY_FUNCTION__, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  return teststatus;
}

/* Test combination of heaapp's ape and ahlog interfaces. */
int heaapp_test_ape_ahlog_heautils(void) {
  int teststatus = 0;
  int status = 0;
  { char * argv[] = { "testheaapp", "clobber=yes", "chatter=2", "debug=no", 0 };
    int argc = sizeof(argv)/sizeof(argv[0]) - 1; HeaAppData appdata = heaapp_construct_appdata(argc, argv);

    status = heaapp_ape_connect(&appdata);
    if (0 != status) {
      fprintf(stderr, "heaapp_test_ape_ahlog_heautils: heaapp_ape_connect returned %d, "
        "not 0 as expected.\n", status);
      teststatus = 1;
    }

    status = heaapp_ahlog_connect(&appdata);
    if (0 != status) {
      fprintf(stderr, "heaapp_test_ape_ahlog_heautils: heaapp_ahlog_connect returned %d, "
        "not 0 as expected.\n", status);
      teststatus = 1;
    }

    status = heaapp_heautils_connect(&appdata);
    if (0 != status) {
      fprintf(stderr, "heaapp_test_ape_ahlog_heautils: heaapp_heautils_connect returned %d, "
        "not 0 as expected.\n", status);
      teststatus = 1;
    }

    if (0 == teststatus) {
      status = heaapp_initialize(&appdata);
      if (0 != status) {
        fprintf(stderr, "heaapp_test_ape_ahlog_heautils: heaapp_initialize returned %d, "
        "not 0 as expected.\n", status);
        teststatus = 1;
      }
    }

    if (0 != teststatus) {
      fprintf(stderr, "heaapp_test_ape_ahlog_heautils: skipping remainder of test.\n");
    }

    if (0 == teststatus) {
      char toolname[HD_NAME_SIZE] = "";
      get_toolname(toolname);
      /* If all the above operations succeeded, ape should have retrieved all the required parameters
         and handed them off to ahlog and heautils. Everything should work now. */
      ape_msg_info(2, "heaapp_test_ape_ahlog_heautils: this message should appear both "
        "on screen and in the log file for %s.\n", toolname);
    }

    heaapp_finalize(&appdata);
  }
  
  return teststatus;
}

/* Test heaapp's message caching capabilities. */
int heaapp_test_msg(void) {
  int teststatus = 0;
  test_printout(__PRETTY_FUNCTION__, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  test_printout(__PRETTY_FUNCTION__, "about to run test case for message caching and flushing.\n");
  test_printout(__PRETTY_FUNCTION__, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  { int status = 0; int expected_status = 0; char * appname = "/path/to/testheaapp"; int argc = 1; char ** argv = &appname;
    /* Construct a HeaAppData object, capable of rudimentary output despite having
       no logging facilities set up. Note that the ruidmentary output function ingores
       chatter; all messages should be displayed when "flush" is called. */
    HeaAppData appdata = heaapp_construct_appdata(argc, argv);

    if (expected_status == status) {
      /* Write a few messages with different chatter levels to appdata's message handler.
         This should just cache these messages. */
      appdata.printinfo(0, __PRETTY_FUNCTION__, "printinfo message 0.\n");
      appdata.printwarn(100, __PRETTY_FUNCTION__, "printwarn message 1.\n");
      appdata.printerr(__PRETTY_FUNCTION__, "printerr message 2.\n");
      appdata.printout(__PRETTY_FUNCTION__, "printout message 3.\n");

      /* Flush the messages written so far to default output streams (stdout for info and stderr for
         err. Chatter is ignored. Flushall should clear the cache after flushing. */
      appdata.flushall(&appdata);

      /* Write a few more messages with different chatter levels to appdata's message handler.
         This is to confirm flushall uncached the messages it already displayed above. */
      appdata.printout(__PRETTY_FUNCTION__, "printout message 4.\n");
      appdata.printerr(__PRETTY_FUNCTION__, "printerr message 5.\n");
      appdata.printwarn(0, __PRETTY_FUNCTION__, "printwarn message 6.\n");
      appdata.printinfo(-7, __PRETTY_FUNCTION__, "printinfo message 7.\n");

      /* Write a few more messages, this time without the function name argument.
         They should appear the same as the others. */
      appdata.printout(0, "%s: printout message 8.\n", __PRETTY_FUNCTION__);
      appdata.printwarn(-7, "", "%s: printwarn message 9.\n", __PRETTY_FUNCTION__);
      appdata.printerr("", "%s: printerr message 10.\n", __PRETTY_FUNCTION__);
      appdata.printinfo(100, 0, "%s: printinfo message 11.\n", __PRETTY_FUNCTION__);

      /* Write a multi-line message, which should be properly formatted with the prefix
         (function name) on each line. */
      appdata.printinfo(7, __PRETTY_FUNCTION__, "printinfo multi-line message 12. This message is so long\n"
        "it is continued on the next line. Each line should be properly formatted\n"
        "with the prefix (function name) repeated at the beginning of each line.\n");

      /* Again, flush the messages written so far. */
      appdata.flushall(&appdata);

      /* Now cache some messages but don't flush them. These should never be seen. */
      appdata.printout(__PRETTY_FUNCTION__, "printout unflushed message (SHOULD NOT BE PRINTED!!!!)\n");
      appdata.printerr(__PRETTY_FUNCTION__, "printerr unflushed message (SHOULD NOT BE PRINTED!!!!)\n");
      appdata.printinfo(3, __PRETTY_FUNCTION__, "printinfo unflushed message (SHOULD NOT BE PRINTED!!!!)\n");
      appdata.printwarn(3, __PRETTY_FUNCTION__, "printwarn unflushed message (SHOULD NOT BE PRINTED!!!!)\n");

      /* Uncomment the following line to see the test print the messages it should not. */
      /* appdata.flushall(&appdata); */
    }
  }
  test_printout(__PRETTY_FUNCTION__, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  test_printout(__PRETTY_FUNCTION__, "done with test case for message caching and flushing.\n");
  test_printout(__PRETTY_FUNCTION__, "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  return teststatus;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* "Fake" functions: functions provided as input to the test cases. */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
static int fake_out(const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  va_start(ap, fmt);
  fprintf(stdout, "testheaapp: fake_out: %s: ", func);
  retval = vfprintf(stdout, fmt, ap);
  va_end(ap);
  fflush(stdout);
  return retval;
}

static int fake_err(const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "testheaapp: fake_err: %s: ", func);
  retval = vfprintf(stderr, fmt, ap);
  va_end(ap);
  return retval;
}

static int fake_info(int chatter, const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  va_start(ap, fmt);
  fprintf(stdout, "testheaapp: fake_info: chatter %d: %s: ", chatter, func);
  retval = vfprintf(stdout, fmt, ap);
  va_end(ap);
  fflush(stdout);
  return retval;
}

static int fake_warn(int chatter, const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "testheaapp: fake_warn: chatter %d: %s: ", chatter, func);
  retval = vfprintf(stderr, fmt, ap);
  va_end(ap);
  return retval;
}

static int fakelog_heaapp_connect(HeaAppData * appdata) {
  int status = 0;

  if (0 == appdata) {
    status = 1;
    fake_err(__PRETTY_FUNCTION__, "appdata is a NULL pointer.\n");
  }

  if (0 == status) {
    /* Add companion fakelib initialize function to the appdata. */
    status = appdata->add_init_func(appdata, fakelog_heaapp_initialize);
    if (0 != status) {
      appdata->printerr(__PRETTY_FUNCTION__, "add_init_func returned status %d, not 0 as expected", status);
    }
  }

  if (0 == status) {
    /* Add companion fakelib initialize function to the appdata. */
    status = appdata->add_final_func(appdata, fakelog_heaapp_finalize);
    if (0 != status) {
      appdata->printerr(__PRETTY_FUNCTION__, "add_final_func returned status %d, not 0 as expected", status);
    }
  }

  return status;
}

static int fakelog_heaapp_initialize(HeaAppData * appdata) {
  int status = 0;

  if (0 == appdata) {
    status = 1;
    fake_err(__PRETTY_FUNCTION__, "appdata is a NULL pointer.\n");
  }

  if (0 == status) {
    if (0 != appdata->logfileset)
      appdata->printout(__PRETTY_FUNCTION__, "logfile name is set; pretending to open file %s.\n", appdata->logfile);
    else
      appdata->printout(__PRETTY_FUNCTION__, "logfile name is not set; not even pretending to open logfile.\n");
  }

  if (0 == status) {
    /* Redirect all output functions to the same generic fake_err function. */
    appdata->printout(__PRETTY_FUNCTION__, "about to connect \"fake\" functions for logging.\n");

    appdata->printerr = &fake_err;
    appdata->printout = &fake_out;
    appdata->printinfo = &fake_info;
    appdata->printwarn = &fake_warn;

    appdata->flushall(appdata);

    appdata->printout(__PRETTY_FUNCTION__, "connected \"fake\" functions for logging.\n");

  }

  return status;
}

static void fakelog_heaapp_finalize(HeaAppData * appdata) {
  int status = 0;

  if (0 == appdata) {
    status = 1;
    fake_err(__PRETTY_FUNCTION__, "appdata is a NULL pointer.\n");
  }

  if (0 == status) {
    /* Create a fresh new appdata, from which to copy initial settings for logging functions. */
    HeaAppData appdatareset = heaapp_construct_appdata(0, 0);

    appdata->printout(__PRETTY_FUNCTION__, "resetting logging functions to the default.\n");
    appdata->flushall(appdata);

    /* Reset logging functions *only* from newly-constrcted appdata. */
    appdata->printwarn = appdatareset.printwarn;
    appdata->printinfo = appdatareset.printinfo;
    appdata->printout = appdatareset.printout;
    appdata->printerr = appdatareset.printerr;

    appdata->printout(__PRETTY_FUNCTION__, "reset logging functions to the default.\n");
  }
}

static int fakepar_heaapp_connect(HeaAppData * appdata) {
  int status = 0;

  if (0 == appdata) {
    status = 1;
    fake_err(__PRETTY_FUNCTION__, "appdata is a NULL pointer.\n");
  }

  if (0 == status) {
    /* Add companion fakelib initialize function to the appdata. */
    status = appdata->add_init_func(appdata, fakepar_heaapp_initialize);
    if (0 != status) {
      appdata->printerr(__PRETTY_FUNCTION__, "add_init_func returned status %d, not 0 as expected", status);
    }
  }

  if (0 == status) {
    /* Add companion fakelib initialize function to the appdata. */
    status = appdata->add_final_func(appdata, fakepar_heaapp_finalize);
    if (0 != status) {
      appdata->printerr(__PRETTY_FUNCTION__, "add_final_func returned status %d, not 0 as expected", status);
    }
  }

  return status;
}

static int fakepar_heaapp_initialize(HeaAppData * appdata) {
  int status = 0;

  if (0 == appdata) {
    status = 1;
    fake_err(__PRETTY_FUNCTION__, "appdata is a NULL pointer.\n");
  }

  if (0 == status) {
    /* Set the logfile name, as if the user supplied that to the library during initialize. */
    appdata->printout(__PRETTY_FUNCTION__, "simulating getting logfile name from user.\n");
    strcpy(appdata->logfile, "testheaapp.log"); appdata->logfileset = 1;
  }

  return status;
}

static void fakepar_heaapp_finalize(HeaAppData * appdata) {
  int status = 0;

  if (0 == appdata) {
    status = 1;
    fake_err(__PRETTY_FUNCTION__, "appdata is a NULL pointer.\n");
  }

  if (0 == status) {
    /* Set the logfile name, as if the user supplied that to the library during initialize. */
    appdata->printout(__PRETTY_FUNCTION__, "resetting logfile name to blank.\n");
    memset(appdata->logfile, '\0', strlen(appdata->logfile)); appdata->logfileset = 0;
  }
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Internal utilities used by the unit test. */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int test_printerr(const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  va_start(ap, fmt);
  if (0 != func && '\0' != *func) fprintf(stderr, "testheaapp: ERROR: %s: ", func);
  else fprintf(stderr, "testheaapp: ERROR: ");
  retval = vfprintf(stderr, fmt, ap);
  va_end(ap);
  return retval;
}

int test_printout(const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  va_start(ap, fmt);
  if (0 != func && '\0' != *func) fprintf(stdout, "testheaapp: %s: ", func);
  else fprintf(stdout, "testheaapp: ");
  retval = vfprintf(stdout, fmt, ap);
  va_end(ap);
  fflush(stdout);
  return retval;
}

/* Revision Log
 * $Log: testheaapp.c,v $
 * Revision 1.3  2014/09/12 15:40:19  peachey
 * Add comments from code review held on 2014-09-05.
 *
 * Revision 1.2  2014/08/06 18:28:30  peachey
 * Add remaining test code for ape, ahlog, and heautils. Initial version complete.
 *
 * Revision 1.1  2014/07/07 17:08:20  peachey
 * Initial version of heaapp library.
 *
 */
