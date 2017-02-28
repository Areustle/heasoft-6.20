/** \file heaapp_ahlog.h
    \brief AstroH logger, heaapp interface
    \author James Peachey
    \date $Date: 2014/09/12 15:40:19 $
   
    Facilities for initializing ahlog from the heaapp facilities.
*/
#ifndef heaapp_heaapp_ahlog_h
#define heaapp_heaapp_ahlog_h

#include "heaapp/heaapp.h"

#include "ahlog/cahlog.h"

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

int heaapp_ahlog_connect(HeaAppData * appdata);

int heaapp_ahlog_initialize(HeaAppData * appdata);

void heaapp_ahlog_finalize(HeaAppData * appdata);

int heaapp_ahlog_test(void);

int heaapp_ahlog_connect(HeaAppData * appdata) {
  int status = 0;
  if (0 == appdata) {
    status = 1;
  }
  if (0 == status) {
/* +++ CR 2014-09-05 JP get and check the status: */
    appdata->add_init_func(appdata, heaapp_ahlog_initialize);
/* +++ CR 2014-09-05 JP get and check the status: */
    appdata->add_final_func(appdata, heaapp_ahlog_finalize);
  }
  return status;
}

/* +++ CR 2014-09-05 JP this is explained below. */
#ifdef RESET_IO
static HeaAppIoFuncType s_printerr_orig = 0;
static HeaAppChatIoFuncType s_printwarn_orig = 0;
static HeaAppIoFuncType s_printout_orig = 0;
static HeaAppChatIoFuncType s_printinfo_orig = 0;
#endif

int heaapp_ahlog_initialize(HeaAppData * appdata) {
  int status = 0;
/* +++ CR 2014-09-05 JP check how appname is set and handled and update this code appropriately.
The current code appears to be ignoring appdata->appname. */
  const char * appname = "";
  const char * logfile = "NONE";
  int chatter = 3;
  char debug = 1;

  if (0 == appdata) {
    status = 1;
  }

/* +++ CR 2014-09-05 JP this is explained below. */
#ifdef RESET_IO
  if (0 == status) {
    s_printerr_orig = appdata->printerr;
    s_printwarn_orig = appdata->printwarn;
    s_printout_orig = appdata->printout;
    s_printinfo_orig = appdata->printinfo;
  }
#endif

  if (0 == status) {
/* +++ CR 2014-09-05 JP check how appname is set and handled and update this code appropriately.
The current code appears to be ignoring appdata->appname. */
    if (0 < appdata->argc && 0 != appdata->argv && '\0' != *(appdata->argv[0])) appname = appdata->argv[0];
    if (0 != appdata->logfileset) logfile = appdata->logfile;
    if (0 != appdata->chatterset) chatter = appdata->chatter;
    if (0 != appdata->debugset) debug = appdata->debug;
    status = ahlog_setup(appname, logfile, chatter, debug);
  }

/* +++ CR 2014-09-05 JP for the benefit of anyone modeling another library set-up on this, explain
that most libraries should not mess with these, i.e., clarify that this library's job is to set up
appdata->print*. Also state what the ahlog_* functions are and where they are declared. */
  if (0 == status) {
    appdata->printerr = ahlog_err;
    appdata->printwarn = ahlog_warn;
    appdata->printout = ahlog_out;
    appdata->printinfo = ahlog_info;
  }
  return status;
}

void heaapp_ahlog_finalize(HeaAppData * appdata) {
  int status = 0;
  if (0 == appdata) {
    status = 1;
  }
  if (0 == status) {
    /* Note it is safe and preferable to leave ahlog functions connected in appdata even after shutdown. */
    /* +++ CR 2014-09-05 JP actually, the preceding comment is wrong. ahlog silently ignores further
       output because all the streams are detached. Instead need to connect functions that put
       further messages to C standard streams, or perhaps restore previous streams -- yet another flush at
       end of heaapp_finalize? Started to add code connected with this issue. Look for ifdef RESET_IO. */
    ahlog_shutdown();
  }
#ifdef RESET_IO
  if (0 == status) {
    appdata->printerr = s_printerr_orig;
    appdata->printwarn = s_printwarn_orig;
    appdata->printout = s_printout_orig;
    appdata->printinfo = s_printinfo_orig;
  }
#endif
}

int heaapp_ahlog_test(void) {
  int teststatus = 0;
  int status = 0;
  { int argc = 1; char * argv[] = { "testheaapp", 0 }; HeaAppData appdata = heaapp_construct_appdata(argc, argv);

    status = heaapp_ahlog_connect(&appdata);
    if (0 != status) {
      fprintf(stderr, "ahlog test success case, heaapp_ahlog_connect returned %d, "
        "not 0 as expected.\n", status);
      teststatus = 1;
    }

    if (0 == status) {
      /* Game the chatter level so messages written below appear. */
      appdata.chatter = 2; appdata.chatterset = 1;
    }

    if (0 == status) {
      status = heaapp_initialize(&appdata);
      if (0 != status) {
        fprintf(stderr, "ahlog test success case, heaapp_initialize returned %d, "
          "not 0 as expected.\n", status);
        teststatus = 1;
      }
    }

    appdata.printerr("heaapp_ahlog_test", "this was written with printerr.\n");
    appdata.printwarn(2, "heaapp_ahlog_test", "this was written with printwarn.\n");
    appdata.printwarn(3, "heaapp_ahlog_test", "this was written with printwarn with low chatter, should not appear.\n");
    appdata.printout("heaapp_ahlog_test", "this was written with printout.\n");
    appdata.printinfo(2, "heaapp_ahlog_test", "this was written with printinfo.\n");
    appdata.printinfo(3, "heaapp_ahlog_test", "this was written with printinfo with low chatter, should not appear.\n");

    heaapp_finalize(&appdata);

/* +++ CR 2014-09-05 JP this is explained below. */
#ifdef RESET_IO
    /* +++ Finish coding this: test that output following finalize gets printed, probably without requiring a flush. */
    appdata.printout("heaapp_ahlog_test", "this was written with printout after finalize; should appear only on screen.\n");
    /* Probably *don't* want to require the call below in order to see the output. */
    appdata.flushall(&appdata);
#endif
  }
  return teststatus;
}

#ifdef __cplusplus
}
#endif

#endif /* AHLOG_AHLOG_H */

/* Revision Log
 * $Log: heaapp_ahlog.h,v $
 * Revision 1.2  2014/09/12 15:40:19  peachey
 * Add comments from code review held on 2014-09-05.
 *
 * Revision 1.1  2014/08/06 18:28:30  peachey
 * Add remaining test code for ape, ahlog, and heautils. Initial version complete.
 *
 */
