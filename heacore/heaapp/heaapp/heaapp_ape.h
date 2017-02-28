/** \file heaapp_ape.h
    \brief Declaration and definition of interface used for applications that use heaapp to
      initialize ape.
    \author James Peachey, HEASARC/EUD/GSFC.
*/
#ifndef heaapp_heaapp_ape_h
#define heaapp_heaapp_ape_h

#include "heaapp/heaapp.h"
/* +++ CR 2014-09-05 JP remove this recursive (albeit harmless) include */
#include "heaapp/heaapp_ape.h"

#include "ape/ape_error.h"
#include "ape/ape_msg.h"
#include "ape/ape_test.h"
#include "ape/ape_trad.h"

#include <stdarg.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Heaapp-compliant connect function, to be called by clients that
      wish to use ape.
    \param appdata The client's application data object, which must be
      set up previously using heaapp.
*/
int heaapp_ape_connect(HeaAppData * appdata);

/** \brief Heaapp-compliant initialize function, which will be called on behalf
      of the client by the heaapp initialize function. This will initialize ape,
      and set up ape functions to provide standard parameter access.
    \param appdata The client's application data object.
*/
int heaapp_ape_initialize(HeaAppData * appdata);

/** \brief Heaapp-compliant finalize function, which will be called on behalf
      of the client by the heaapp finalize function. This will clean up ape
      connections and shut down ape.
    \param appdata The client's application data object.
*/
void heaapp_ape_finalize(HeaAppData * appdata);

int heaapp_ape_test(void);

HeaAppData * s_appdata = 0;

static void s_heaapp_out(const char * msg) {
  /* Note that this function should only be connected when s_appdata is non-0. */
  s_appdata->printout("", "%s", msg);
}

static void s_heaapp_err(const char * msg) {
  /* Note that this function should only be connected when s_appdata is non-0. */
  s_appdata->printerr("", "%s", msg);
}

static void s_heaapp_info(int chatter, const char * msg) {
  /* Note that this function should only be connected when s_appdata is non-0. */
  s_appdata->printinfo(chatter, "", "%s", msg);
}

static void s_heaapp_warn(int chatter, const char * msg) {
  /* Note that this function should only be connected when s_appdata is non-0. */
  s_appdata->printwarn(chatter, "", "%s", msg);
}

int heaapp_ape_connect(HeaAppData * appdata) {
  int status = eOK;

  if (eOK == status && 0 == appdata) {
    status = eNullPointer;
  }

  if (eOK == status) {
    status = appdata->add_init_func(appdata, heaapp_ape_initialize);
  }

  if (eOK == status) {
    status = appdata->add_final_func(appdata, heaapp_ape_finalize);
  }

  return status;
}

static ApeMsgFuncPtrType s_save_out_handler = 0;
static ApeMsgFuncPtrType s_save_err_handler = 0;
static ApeMsgChatPtrType s_save_info_handler = 0;
static ApeMsgChatPtrType s_save_warn_handler = 0;

int heaapp_ape_initialize(HeaAppData * appdata) {
  int status = eOK;
  if (eOK == status && 0 == appdata) {
    status = eNullPointer;
  }

  /* This is subtle. It is necessary to redirect all ape output functions
     before calling any ape function, which may of course call ape output
     functions. The functions defined above (s_heaapp_err etc.) are set
     to use the functions in the appdata structure. This will allow future
     changes to appdata to affect ape's messages without further ape calls.
     Note that ape is not providing any message services. */
  if (eOK == status) {
    if (0 == s_save_err_handler) s_save_err_handler = ape_msg_get_err_handler();
    if (0 == s_save_warn_handler) s_save_warn_handler = ape_msg_get_warn_handler();
    if (0 == s_save_out_handler) s_save_out_handler = ape_msg_get_out_handler();
    if (0 == s_save_info_handler) s_save_info_handler = ape_msg_get_info_handler();
    s_appdata = appdata;
    ape_msg_set_err_handler(s_heaapp_err);
    ape_msg_set_warn_handler(s_heaapp_warn);
    ape_msg_set_out_handler(s_heaapp_out);
    ape_msg_set_info_handler(s_heaapp_info);
  }

  if (eOK == status) {
    status = ape_trad_init(appdata->argc, appdata->argv);
/* +++ CR 2014-09-05 JP add check and error message here. */
  }

  if (eOK == status && 0 == appdata->clobberset) {
    char clobber = 0;
    int optstatus = ape_trad_query_bool("clobber", &clobber);
    if (eOK == optstatus) {
      appdata->clobber = clobber;
      appdata->clobberset = 1;
    } else if (eParNotFound == optstatus) {
      ape_msg_warn(3, "ape found no clobber parameter.\n");
    } else {
      ape_msg_warn(3, "ape error reading clobber parameter.\n");
    }
  }

  if (eOK == status && 0 == appdata->chatterset) {
    int chatter = 0;
    int optstatus = ape_trad_query_int("chatter", &chatter);
    if (eOK == optstatus) {
      appdata->chatter = chatter;
      appdata->chatterset = 1;
    } else if (eParNotFound == optstatus) {
      ape_msg_warn(3, "ape found no chatter parameter.\n");
    } else {
      ape_msg_warn(3, "ape error reading chatter parameter.\n");
    }
  }

  if (eOK == status && 0 == appdata->logfileset) {
    char * logfile = 0;
    int optstatus = ape_trad_query_file_name("logfile", &logfile);
    if (eOK == optstatus) {
      strncpy(appdata->logfile, logfile, FILENAME_MAX);
      appdata->logfileset = 1;
    } else if (eParNotFound == optstatus) {
      ape_msg_warn(3, "ape found no logfile parameter.\n");
    } else {
      ape_msg_warn(3, "ape error reading logfile parameter.\n");
    }
    free(logfile); logfile = 0;
  }

  if (eOK == status && 0 == appdata->debugset) {
    char debug = 0;
    int optstatus = ape_trad_query_bool("debug", &debug);
    if (eOK == optstatus) {
      appdata->debug = debug;
      appdata->debugset = 1;
    } else if (eParNotFound == optstatus) {
      ape_msg_warn(3, "ape found no debug parameter.\n");
    } else {
      ape_msg_warn(3, "ape error reading debug parameter.\n");
    }
  }

  if (eOK == status && 0 == appdata->historyset) {
    char history = 0;
    int optstatus = ape_trad_query_bool("history", &history);
    if (eOK == optstatus) {
      appdata->history = history;
      appdata->historyset = 1;
    } else if (eParNotFound == optstatus) {
      ape_msg_warn(3, "ape found no history parameter.\n");
    } else {
      ape_msg_warn(3, "ape error reading history parameter.\n");
    }
  }

  return status;
}

void heaapp_ape_finalize(HeaAppData * appdata) {
  int status = eOK;
  if (eOK == status && 0 == appdata) {
    status = eNullPointer;
  }
  if (eOK == status) {
    ape_trad_close(1);
  }
  ape_msg_set_info_handler(s_save_info_handler);
  ape_msg_set_out_handler(s_save_out_handler);
  ape_msg_set_warn_handler(s_save_warn_handler);
  ape_msg_set_err_handler(s_save_err_handler);
  s_save_info_handler = 0;
  s_save_out_handler = 0;
  s_save_info_handler = 0;
  s_save_err_handler = 0;
  s_appdata = 0;
}

static int s_test_err(const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  if (0 != func && '\0' != *func) fprintf(ape_msg_get_err_stream(), "testheaapp: heaapp_ape_test: error: %s: ", func);
  else fprintf(ape_msg_get_err_stream(), "testheaapp: heaapp_ape_test: error: ");
  va_start(ap, fmt);
  retval = vfprintf(ape_msg_get_err_stream(), fmt, ap);
  va_end(ap);
  return retval;
}

static int s_test_warn(int chatter, const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  if (0 != func && '\0' != *func) fprintf(ape_msg_get_err_stream(), "testheaapp: heaapp_ape_test: warning: %s: ", func);
  else fprintf(ape_msg_get_err_stream(), "testheaapp: heaapp_ape_test: warning: ");
  va_start(ap, fmt);
  retval = vfprintf(ape_msg_get_err_stream(), fmt, ap);
  va_end(ap);
  return retval;
}

static int s_test_out(const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  if (0 != func && '\0' != *func) fprintf(ape_msg_get_out_stream(), "testheaapp: heaapp_ape_test: out: %s: ", func);
  else fprintf(ape_msg_get_out_stream(), "testheaapp: heaapp_ape_test: out: ");
  va_start(ap, fmt);
  retval = vfprintf(ape_msg_get_out_stream(), fmt, ap);
  va_end(ap);
  return retval;
}

static int s_test_info(int chatter, const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  if (0 != func && '\0' != *func) fprintf(ape_msg_get_out_stream(), "testheaapp: heaapp_ape_test: info: %s: ", func);
  else fprintf(ape_msg_get_out_stream(), "testheaapp: heaapp_ape_test: info: ");
  va_start(ap, fmt);
  retval = vfprintf(ape_msg_get_out_stream(), fmt, ap);
  va_end(ap);
  return retval;
}

static HeaAppData s_construct_test_appdata(int argc, char ** argv) {
  HeaAppData appdata = heaapp_construct_appdata(argc, argv);
  appdata.printerr = s_test_err;
  appdata.printwarn = s_test_warn;
  appdata.printout = s_test_out;
  appdata.printinfo = s_test_info;
  return appdata;
}

int heaapp_ape_test(void) {
  int status = eOK;

  { int argc = 1; char * argv[] = { "modeonly", 0 }; HeaAppData appdata = s_construct_test_appdata(argc, argv);

    status = heaapp_ape_connect(&appdata);
    if (eOK != status)
      ape_test_failed("heaapp test success case with ape_test parameter file, heaapp_ape_connect returned %d, "
        "not 0 as expected.\n", status);

    if (eOK == status) {
      status = heaapp_initialize(&appdata);
      if (eOK != status)
        ape_test_failed("heaapp test success case with ape_test parameter file, heaapp_initialize returned %d, "
          "not 0 as expected.\n", status);
    }

    if (eOK == status) {
      if (0 != appdata.clobberset) ape_test_failed("heaapp test success case with ape_test parameter file, "
        "appdata.clobberset was %d, not 0 as expected.\n", appdata.clobberset);
      
      if (0 != appdata.clobber)
        ape_test_failed("heaapp test success case with ape_test parameter file, "
          "appdata.clobber was %d, not 0 as expected.\n", appdata.clobber);
    }
    if (eOK == status) {
      if (0 != appdata.chatterset) ape_test_failed("heaapp test success case with ape_test parameter file, "
        "appdata.chatterset was %d, not 0 as expected.\n", appdata.chatterset);
      
      if (0 != appdata.chatter)
        ape_test_failed("heaapp test success case with ape_test parameter file, "
          "appdata.chatter was %d, not 0 as expected.\n", appdata.chatter);
    }
    if (eOK == status) {
      if (0 != appdata.logfileset) ape_test_failed("heaapp test success case with ape_test parameter file, "
        "appdata.logfileset was %d, not 0 as expected.\n", appdata.logfileset);
      
      if (0 != strcmp("", appdata.logfile))
        ape_test_failed("heaapp test success case with ape_test parameter file, "
          "appdata.logfile was \"%s\", not \"\" as expected.\n", appdata.logfile);
    }
    if (eOK == status) {
      if (0 != appdata.debugset) ape_test_failed("heaapp test success case with ape_test parameter file, "
        "appdata.debugset was %d, not 0 as expected.\n", appdata.debugset);
      
      if (0 != appdata.debug)
        ape_test_failed("heaapp test success case with ape_test parameter file, "
          "appdata.debug was %d, not 0 as expected.\n", appdata.debug);
    }
    if (eOK == status) {
      if (0 != appdata.historyset) ape_test_failed("heaapp test success case with ape_test parameter file, "
        "appdata.historyset was %d, not 0 as expected.\n", appdata.historyset);
      
      if (0 != appdata.history)
        ape_test_failed("heaapp test success case with ape_test parameter file, "
          "appdata.history was %d, not 0 as expected.\n", appdata.history);
    }
    heaapp_finalize(&appdata);
  }

  { int argc = 1; char * argv[] = { "testheaapp", 0 }; HeaAppData appdata = s_construct_test_appdata(argc, argv);

    status = heaapp_ape_connect(&appdata);
    if (eOK != status)
      ape_test_failed("heaapp test success case with testheaapp parameter file, heaapp_ape_connect returned %d, "
        "not 0 as expected.\n", status);

    if (eOK == status) {
      status = heaapp_initialize(&appdata);
      if (eOK != status)
        ape_test_failed("heaapp test success case with testheaapp parameter file, heaapp_initialize returned %d, "
          "not 0 as expected.\n", status);
    }

    if (eOK == status) {
      if (1 != appdata.clobberset) ape_test_failed("heaapp test success case with testheaapp parameter file, "
        "appdata.clobberset was %d, not 1 as expected.\n", appdata.clobberset);
      
      if (1 != appdata.clobber)
        ape_test_failed("heaapp test success case with testheaapp parameter file, "
          "appdata.clobber was %d, not 1 as expected.\n", appdata.clobber);
    }
    if (eOK == status) {
      if (1 != appdata.chatterset) ape_test_failed("heaapp test success case with testheaapp parameter file, "
        "appdata.chatterset was %d, not 1 as expected.\n", appdata.chatterset);
      
      if (1 != appdata.chatter)
        ape_test_failed("heaapp test success case with testheaapp parameter file, "
          "appdata.chatter was %d, not 1 as expected.\n", appdata.chatter);
    }
    if (eOK == status) {
      if (1 != appdata.debugset) ape_test_failed("heaapp test success case with testheaapp parameter file, "
        "appdata.debugset was %d, not 1 as expected.\n", appdata.debugset);
      
      if (1 != appdata.debug)
        ape_test_failed("heaapp test success case with testheaapp parameter file, "
          "appdata.debug was %d, not 1 as expected.\n", appdata.debug);
    }
    if (eOK == status) {
      if (1 != appdata.logfileset) ape_test_failed("heaapp test success case with testheaapp parameter file, "
        "appdata.logfileset was %d, not 1 as expected.\n", appdata.logfileset);
      
      if (0 != strcmp("!DEFAULT", appdata.logfile))
        ape_test_failed("heaapp test success case with testheaapp parameter file, "
          "appdata.logfile was \"%s\", not \"!DEFAULT\" as expected.\n", appdata.logfile);
    }
    if (eOK == status) {
      if (1 != appdata.historyset) ape_test_failed("heaapp test success case with testheaapp parameter file, "
        "appdata.historyset was %d, not 1 as expected.\n", appdata.historyset);
      
      if (1 != appdata.history)
        ape_test_failed("heaapp test success case with testheaapp parameter file, "
          "appdata.history was %d, not 1 as expected.\n", appdata.history);
    }
    heaapp_finalize(&appdata);
  }

  { int argc = 1; char * argv[] = { "non-existent", 0 }; HeaAppData appdata = s_construct_test_appdata(argc, argv);

    status = heaapp_ape_connect(&appdata);
    if (eOK != status)
      ape_test_failed("heaapp test failure case with non-existent parameter file, heaapp_ape_connect returned %d, "
        "not 0 as expected.\n", status);

    if (eOK == status) {
      status = heaapp_initialize(&appdata);
      if (eFileNotFound != status)
        ape_test_failed("heaapp test failure case with non-existent parameter file, heaapp_initialize returned %d, "
          "not %d as expected.\n", status, eFileNotFound);
    }

    heaapp_finalize(&appdata);
  }
  return ape_test_get_status();
}

#ifdef __cplusplus
}
#endif

#endif

/*
 * $Log: heaapp_ape.h,v $
 * Revision 1.2  2014/09/12 15:40:19  peachey
 * Add comments from code review held on 2014-09-05.
 *
 * Revision 1.1  2014/08/06 18:28:30  peachey
 * Add remaining test code for ape, ahlog, and heautils. Initial version complete.
 *
 */
