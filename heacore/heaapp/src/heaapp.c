/**
    \file heaapp.c
    \brief Facility for allowing client applications to select, initialize and use
      various available HEASoft libraries for standard support functions, such as
      parameter handling and output/error logging.
    \author James Peachey
    \date $Date: 2014/09/12 15:40:19 $
*/

#include "heaapp/heaapp.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/* +++ CR 2014-09-05 JP add comment explaining the parenthesis, and describing this macro. */
#define MAX_MSG_SIZE (4096)

/* +++ CR 2014-09-05 JP add comment explaining the parenthesis, and describing this macro. */
#define MAX_NUM_MSG (512)
/* +++ CR 2014-09-05 JP add comment explaining the parenthesis, and describing this macro. */
#define AVG_MSG_SIZE (64)
/* +++ CR 2014-09-05 JP add comment explaining the parenthesis, and describing this macro. */
#define MSG_BUF_SIZE (512)

/* +++ CR 2014-09-05 JP add comments about these variables. */
static char s_msgbuf[MAX_NUM_MSG][MSG_BUF_SIZE];
static int s_msgstreamid[MAX_NUM_MSG];
static int s_msgchat[MAX_NUM_MSG];
static char s_msgfunc[MAX_NUM_MSG][MSG_BUF_SIZE];
static int s_msgid;
/* +++ CR 2014-09-05 JP use int rather than char to be more conventional? */
static char s_msgfull;
/* +++ CR 2014-09-05 JP use int rather than char to be more conventional? */
static char s_msgtrunc;
static int s_printerr(const char * func, const char * fmt, ...);
static int s_printout(const char * func, const char * fmt, ...);
static int s_printwarn(int chatter, const char * func, const char * fmt, ...);
static int s_printinfo(int chatter, const char * func, const char * fmt, ...);

enum {
  MSG_OUT = 1,
  MSG_ERR,
  MSG_INFO,
  MSG_WARN
};

static int s_msg_push(int streamid, int chatter, const char * func, const char * fmt, va_list ap) {
  int nchar = -1;
  if (0 == s_msgfull) {
    if (MAX_NUM_MSG <= s_msgid) {
      s_msgfull = 1;
    } else {
      int nremain = MSG_BUF_SIZE - (strlen(s_msgbuf[s_msgid]) + 1); /* Reserve room for terminator. */

      if (nremain <= 0) {
        s_msgfull = 1;
      } else {
        /* Buffer to hold just the current message being pushed. */
        char buf[MAX_MSG_SIZE] = "";

        /* Write the current message into the local buffer. */
        nchar = vsnprintf(buf, MAX_MSG_SIZE, fmt, ap);

        /* vsnprintf returns # of char that would have ideally been written if MAX_MSG_SIZE were large enough,
           which may be larger than MAX_MSG_SIZE - 1. buf thus contains at most MAX_MSG_SIZE - 1 characters
           (not counting terminator); however, nchar can exceed that. */
        if (MAX_MSG_SIZE <= nchar) { nchar = MAX_MSG_SIZE - 1; s_msgtrunc = 1; }

        /* Ensure message is not too long to fit in remainder of the big message buffer. */
        if (nchar > nremain) { nchar = nremain; s_msgfull = 1; }

        /* vsnprintf returns -1 to indicate problems. */
        if (nchar < 0) {
          s_msgtrunc = 1;
        } else {
          /* Copy what we can of the message to the big message buffer. */
          strncat(s_msgbuf[s_msgid], buf, nchar);

          /* Copy what we can of the function name to the function buffer. */
          if (0 != func) strncat(s_msgfunc[s_msgid], func, MSG_BUF_SIZE);

          /* Cache also the stream and chatter level for the message. */
          s_msgstreamid[s_msgid] = streamid;
          s_msgchat[s_msgid] = chatter;
          ++s_msgid;
        }
      }
    }
  }
  return nchar;
}

static int s_msg_err(const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  va_start(ap, fmt);
  retval = s_msg_push(MSG_ERR, 0, func, fmt, ap);
  va_end(ap);
  return retval;
}

static int s_msg_out(const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  va_start(ap, fmt);
  retval = s_msg_push(MSG_OUT, 0, func, fmt, ap);
  va_end(ap);
  return retval;
}

static int s_msg_info(int chatter, const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  if (MAX_NUM_MSG > s_msgid) s_msgchat[s_msgid] = chatter;
  va_start(ap, fmt);
  retval = s_msg_push(MSG_INFO, 0, func, fmt, ap);
  va_end(ap);
  return retval;
}

static int s_msg_warn(int chatter, const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  if (MAX_NUM_MSG > s_msgid) s_msgchat[s_msgid] = chatter;
  va_start(ap, fmt);
  retval = s_msg_push(MSG_WARN, 0, func, fmt, ap);
  va_end(ap);
  return retval;
}

static void s_msg_flush(HeaAppData * appdata) {
  if (0 != appdata) {
    HeaAppIoFuncType printout = (0 == appdata->printout || s_msg_out == appdata->printout) ? s_printout : appdata->printout;
    HeaAppIoFuncType printerr = (0 == appdata->printerr || s_msg_err == appdata->printerr) ? s_printerr : appdata->printerr;
    HeaAppChatIoFuncType printinfo =
      (0 == appdata->printinfo || s_msg_info == appdata->printinfo) ? s_printinfo : appdata->printinfo;
    HeaAppChatIoFuncType printwarn =
      (0 == appdata->printwarn || s_msg_warn == appdata->printwarn) ? s_printwarn : appdata->printwarn;
    int ii = 0;
    for (ii = 0; ii != s_msgid; ++ii) {
      if ('\0' != *s_msgbuf[ii]) {
        char * cp = strtok(s_msgbuf[ii], "\n");
        while (0 != cp) {
          switch (s_msgstreamid[ii]) {
            case MSG_OUT:
              if (s_printout == printout && 0 != appdata->appname) printout("", "%s: ", appdata->appname);
              printout(s_msgfunc[ii], "%s\n", cp);
              break;
            case MSG_ERR:
              if (s_printerr == printerr && 0 != appdata->appname) printerr("", "%s: ", appdata->appname);
              printerr(s_msgfunc[ii], "%s\n", cp);
              break;
            case MSG_INFO:
              if (s_printinfo == printinfo && 0 != appdata->appname) printinfo(s_msgchat[ii], "", "%s: ", appdata->appname);
              printinfo(s_msgchat[ii], s_msgfunc[ii], "%s\n", cp);
              break;
            case MSG_WARN:
              if (s_printwarn == printwarn && 0 != appdata->appname) printwarn(s_msgchat[ii], "", "%s: ", appdata->appname);
              printwarn(s_msgchat[ii], s_msgfunc[ii], "%s\n", cp);
              break;
          }
          cp = strtok(0, "\n");
        }
        *s_msgbuf[ii] = '\0';
        *s_msgfunc[ii] = '\0';
      }
    }
  }
  memset(s_msgstreamid, '\0', sizeof(s_msgstreamid));
  memset(s_msgchat, '\0', sizeof(s_msgchat));
  s_msgid= 0;
  s_msgfull = 0;
  s_msgtrunc = 0;
}

static int s_printerr(const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  if (0 != func && '\0' != *func) fprintf(stderr, "%s: ", func);
  va_start(ap, fmt);
  retval = vfprintf(stderr, fmt, ap);
  va_end(ap);
  return retval;
}

static int s_printout(const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  if (0 != func && '\0' != *func) fprintf(stdout, "%s: ", func);
  va_start(ap, fmt);
  retval = vfprintf(stdout, fmt, ap);
  va_end(ap);
  fflush(stdout);
  return retval;
}

static int s_printwarn(int chatter, const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  if (0 != func && '\0' != *func) fprintf(stderr, "%s: ", func);
  va_start(ap, fmt);
  retval = vfprintf(stderr, fmt, ap);
  va_end(ap);
  return retval;
}

static int s_printinfo(int chatter, const char * func, const char * fmt, ...) {
  int retval = 0;
  va_list ap;
  if (0 != func && '\0' != *func) fprintf(stdout, "%s: ", func);
  va_start(ap, fmt);
  retval = vfprintf(stdout, fmt, ap);
  va_end(ap);
  fflush(stdout);
  return retval;
}

/* Functions that cache output in buffers. This is in order to hold the output created
   during library initializations until some output facility is initialized. */
static int s_add_init_func(HeaAppData * appdata, HeaAppInitializeFuncType func) {
  int status = 0;
  if (0 == appdata || 0 == func) status = 1;
  if (0 == status) {
    int ii = 0;
    /* Go through all the slots in the HeaAppData object. */
    for (ii = 0; ii != HDAPP_NUM_LIB; ++ii) {
      /* Look for an available slot to add func (null pointer), and add the caller-supplied func. */
      if (0 == appdata->initializefunc[ii]) {
        appdata->initializefunc[ii] = func;
        break;
      }
    }
    /* Confirm that a slot was found; otherwise the caller is trying to include
       too many libraries. Flag this as an error. */
    if (HDAPP_NUM_LIB == ii) status = 1;
  }
  return status;
}

static int s_add_final_func(HeaAppData * appdata, HeaAppFinalizeFuncType func) {
  int status = 0;
  if (0 == appdata || 0 == func) status = 1;
  if (0 == status) {
    int ii = 0;
    /* Go through all the slots in the HeaAppData object. */
    for (ii = 0; ii != HDAPP_NUM_LIB; ++ii) {
      /* Look for an available slot to add func (null pointer), and add the caller-supplied func. */
      if (0 == appdata->finalizefunc[ii]) {
        appdata->finalizefunc[ii] = func;
        break;
      }
    }
    /* Confirm that a slot was found; otherwise the caller is trying to include
       too many libraries. Flag this as an error. */
    if (HDAPP_NUM_LIB == ii) status = 1;
  }
  return status;
}

HeaAppData heaapp_construct_appdata(int argc, char ** argv) {
  HeaAppData appdata = { 0 };
  appdata.argc = argc;
  appdata.argv = argv;
  if (0 != argv) {
    char * lastslash = strrchr(argv[0], '/');
    if (0 != lastslash) appdata.appname = lastslash + 1; else appdata.appname = argv[0];
  }
  appdata.printout = s_msg_out;
  appdata.printerr = s_msg_err;
  appdata.printinfo = s_msg_info;
  appdata.printwarn = s_msg_warn;
  appdata.flushall = s_msg_flush;
  appdata.add_init_func = s_add_init_func;
  appdata.add_final_func = s_add_final_func;
  return appdata;
}

int heaapp_initialize(HeaAppData * appdata) {
  int status = 0;
  if (0 == appdata) { status = 1; }
  if (0 == status) {
    int ii = 0;
    /* Stop the loop if non-0 status is returned (fatal error). */
    for (ii = 0; 0 == status && ii != HDAPP_NUM_LIB; ++ii) {
      int idx = ii; /* Initialize in array order. */
      if (0 != appdata->initializefunc[idx]) status = appdata->initializefunc[idx](appdata);
    }
    appdata->flushall(appdata);
  }
  return status;
}

int heaapp_finalize(HeaAppData * appdata) {
  int finalize_status = 0;
  if (0 == appdata) { finalize_status = 1; }
  if (0 == finalize_status) {
    int ii = 0;
/* +++ CR 2014-09-05 JP explain why there is flush before and after finalize. */
    appdata->flushall(appdata);
    /* Don't stop the loop for errors, but do signal them with a non-0 return. */
    for (ii = 0; ii != HDAPP_NUM_LIB; ++ii) {
      int idx = (HDAPP_NUM_LIB - 1) - ii; /* Finalize in reverse array order. */
      if (0 != appdata->finalizefunc[idx]) appdata->finalizefunc[idx](appdata);
    }
  }
/* +++ CR 2014-09-05 JP explain why there is flush before and after finalize. */
  if (0 != appdata) appdata->flushall(appdata);

/* +++ CR 2014-09-05 JP following flush, redirect streams to safe functions that don't cache, since no other flushes expected. */
  return finalize_status;
}

/*
Heaapp Development Notes
  Heaapp was developed to provide client applications (HEASoft tools) a
  flexible means of initializing support libraries, while minimizing
  coupling and dependencies. Previous initializers such as heainit
  are hard-wired to set up a particular set of libraries a particular
  way. This requires a tool needing just one of these libraries
  to initialize (and therefore link to) all the HEASoft libraries
  that heainit initializes. It also requires heaainit to be updated
  whenever one of the libraries it uses changes.

Heaapp Requirements
  R1 (interface) Libraries optional. Heaapp shall not require its client
     to use any other specific library.
     R1.1 (interface) This implies that heaapp shall be completely
          self-contained, that is, it shall not include, use, or refer
          explicitly to any function in other libraries.
  R2 (interface) heaapp optional. There shall be no requirement that a
     library use any part of heaapp in order to be compatible
     with heaapp.
     R2.1 (restatement) Heaapp shall be able to initialize a library
          without that library being modified to use or refer to heaapp.
     R2.2 This implies that whatever code is needed for heasapp
          to initialize a library need not reside in that library.
     R2.3 There shall be no requirement that heaapp be used to initialize
          any library.
  R8 (interface) Library support interface. Heaapp shall provide a lightweight
     and general means for library support functions (initializations and
     finalizations) to be developed, and for these library support functions
     to have access to client-level data members.
     R8.1 This implies that a common signature shall be used for initializations,
          and a common signature for finalizations.
     R8.2 This implies that the application data described in R4 shall be
          accessible in all library support functions.

  R3 (interface) Heaapp shall 0-initialize all variables.
  R4 (functional) Heaapp shall provide a lightweight means of passing
     application data between the client and the libraries the client
     chooses to use.
     R4.1 Heaapp shall accept and store command line arguments (argc, argv).
     R4.2 Heaapp shall pass command line arguments to library initializations,
          which may use them or modify their values.
     R4.3 Heaapp shall provide a means for storing, modifying and retrieving
          the following client-level data:
            int chatter;  parameter controlling the verbosity of output.
            char debug;   boolean parameter controlling whether the client
                          should enable and/or provide debugging support.
            char clobber; boolean parameter controlling whether existing output
                          files should be overwritten.
            char history; boolean parameter controlling whether parameters should
                          be written to output files.
          Other than storing them as the indicated intrinsic C type, heaapp
          shall not make any assumptions about, or restrictions upon the values
          or behavior associated with these parameters.
     R4.4 Heaapp shall provide a means for client and library code to
          determine whether any of the client-level data in R4.3 has been
          set (by client or library code).
  R5 (functional) Heaapp shall provide a lightweight and general means for
     the client to select at compile-time which libraries will be initialized
     by heaapp, and in what order.
  R6 (functional) Heaapp shall provide a means for the client to cause heaapp
     to call in sequence all the library initializations the client has selected.
     The order of the calls shall be client-defined.
  R7 (functional) Heaapp shall provide a means for the client to cause heaapp
     to call in sequence all the library finalizations the client has selected.
     The order of the calls shall be the reverse of the initialization sequence.

Heaapp Design
Heaapp factors into the following aspects: the client interface,
the library interface, support functions, default behaviors.

Client Interface
The client first calls a heaapp function (heaapp_construct_appdata), which
constructs a self-contained client data structure (HeaAppData). The client
passes command line arguments to heaapp_construct_appdata, which stores
them in the HeaAppData structure. The HeaAppData structure includes all
data members and/or function pointers needed to meet the requirements.

After calling heaapp_construct_appdata, the client selects which libraries
heaapp will initialize and/or finalize. The client does this by passing the
HeaAppData structure to one or more library set-up functions. The library
set-up functions do not directly call any library support functions, rather
they add the necessary library support functions to the HeaAppData's
container of initialization and/or finalization functions. (See the
section on the Library Interface for more information.)

The client then calls a heaapp function (heaapp_initialize), which calls each
initialization in the sequence in which the client called the library
set-up functions.

Following heaapp_initialize, the client is free to perform its own functions,
using any of the libraries that were set up by the heaapp_initialize step.

TODO: finish documenting the design. Do it as narrative? Probably
change the narrative to the form below.
D1 Client Interface
   D1.1 (
*/


#ifdef __cplusplus
}
#endif /* __cplusplus */

/* Revision Log
 * $Log: heaapp.c,v $
 * Revision 1.2  2014/09/12 15:40:19  peachey
 * Add comments from code review held on 2014-09-05.
 *
 * Revision 1.1  2014/07/07 17:08:20  peachey
 * Initial version of heaapp library.
 *
 */
