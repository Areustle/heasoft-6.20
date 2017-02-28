/** \file heaapp_heautils.h
    \brief AstroH logger, heaapp interface
    \author James Peachey
    \date $Date: 2014/08/06 18:28:30 $
   
    Facilities for initializing heautils from the heaapp facilities.
*/
#ifndef heaapp_heaapp_heautils_h
#define heaapp_heaapp_heautils_h

#include "heaapp/heaapp.h"

#include "headas.h"
#include "headas_error.h"
#include "headas_utils.h"

/* These are defined privately in heautils source code, but should be in a public header file. */
#ifndef HD_NAME_SIZE 
#define HD_NAME_SIZE 128
#endif
#ifndef HD_VERS_SIZE 
#define HD_VERS_SIZE 128
#endif

#include <stdio.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

int heaapp_heautils_connect(HeaAppData * appdata);

int heaapp_heautils_initialize(HeaAppData * appdata);

void heaapp_heautils_finalize(HeaAppData * appdata);

int heaapp_heautils_test(void);

int heaapp_heautils_connect(HeaAppData * appdata) {
  int status = 0;
  if (0 == appdata) {
    status = 1;
  }
  if (0 == status) {
    appdata->add_init_func(appdata, heaapp_heautils_initialize);
    appdata->add_final_func(appdata, heaapp_heautils_finalize);
  }
  return status;
}

int heaapp_heautils_initialize(HeaAppData * appdata) {
  int status = 0;
  int clobber = 0;
  int history = 0;

  if (0 == appdata || 0 == appdata->argc || 0 == appdata->argv || 0 == appdata->argv[0]) {
    status = 1;
  }

  if (0 == status) {
    /* Register defaults for toolname/version, used in error reporting.
       These functions cannot create an error condition. */
    set_toolname(hdbasename(appdata->argv[0]));
    set_toolversion("0.0");

    /* Set up global error handler. */
    status = HDerror_init(status);
    if (0 != status) HDerror_throw(0, 0, 0, status);
  }

  if (0 == status) {
    if (0 != appdata->clobberset) clobber = appdata->clobber;
    if (0 != appdata->historyset) history = appdata->history;

    /* Do not set headas_chatpar here. This should be done in the heaapp
       functions to set up heaio, rather than heautils. The reason it is even
       mentioned here is that this code was largely lifted from heainit,
       which does this all at the same time in one block. */
    /* headas_chatpar = chatter; */
    headas_clobpar = clobber;
    set_history(history);
  }

  return status;
}

void heaapp_heautils_finalize(HeaAppData * appdata) {
  int status = 0;
  if (0 == appdata) {
    status = 1;
  }
  if (0 == status) {
  }
}

int heaapp_heautils_test(void) {
  int teststatus = 0;
  int status = 0;
  { int argc = 1; char * argv[] = { "/path/to/testheaapp", 0 }; HeaAppData appdata = heaapp_construct_appdata(argc, argv);
    const int expected_clobber = 42; /* Arbitrary value; hot realistic, but distinctive. */
    const int expected_history = 17; /* Arbitrary value; hot realistic, but distinctive. */

    status = heaapp_heautils_connect(&appdata);
    if (0 != status) {
      fprintf(stderr, "heautils test success case, heaapp_heautils_connect returned %d, "
        "not 0 as expected.\nSkipping remainder of test.\n", status);
      teststatus = 1;
    }

    if (0 == status) {
      /* Game the inputs to simulate other initializers that set inputs. */
      appdata.clobberset = 1; appdata.clobber = expected_clobber; /* Arbitrary but non-0 value. */
      appdata.historyset = 1; appdata.history = expected_history;
    }

    if (0 == status) {
      status = heaapp_initialize(&appdata);
      if (0 != status) {
        fprintf(stderr, "heautils test success case, heaapp_initialize returned %d, "
          "not 0 as expected.\nSkipping remainder of test.\n", status);
        teststatus = 1;
      }
    }

    /* Confirm values if initialize succeeded. */
    if (0 == status) {
      char toolname[HD_NAME_SIZE] = "";
      char toolversion[HD_VERS_SIZE] = "";
      int history;

      get_toolname(toolname);
      if (0 != strcmp(toolname, "testheaapp")) {
        fprintf(stderr, "heautils test success case, get_toolname returned %s, "
          "not %s as expected.\n", toolname, "testheaapp");
        teststatus = 1;
      }

      get_toolversion(toolversion);
      if (0 != strcmp(toolversion, "0.0")) {
        fprintf(stderr, "heautils test success case, get_toolversion returned %s, "
          "not %s as expected.\n", toolversion, "testheaapp");
        teststatus = 1;
      }

      if (expected_clobber != headas_clobpar) {
        fprintf(stderr, "heautils test success case, headas_clobpar is %d, "
          "not %d as expected.\n", headas_clobpar, expected_clobber);
        teststatus = 1;
      }

      get_history(&history);
      if (expected_history != history) {
        fprintf(stderr, "heautils test success case, headas_clobpar is %d, "
          "not %d as expected.\n", history, expected_history);
        teststatus = 1;
      }
    }

    heaapp_finalize(&appdata);
  }
  return teststatus;
}

#ifdef __cplusplus
}
#endif

#endif

/* Revision Log
 * $Log: heaapp_heautils.h,v $
 * Revision 1.1  2014/08/06 18:28:30  peachey
 * Add remaining test code for ape, ahlog, and heautils. Initial version complete.
 *
 */
