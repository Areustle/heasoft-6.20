/**
    \file heaapp.h
    \brief Facility for allowing client applications to select, initialize and use
      various available HEASoft libraries for standard support functions, such as
      parameter handling and output/error logging.
    \author James Peachey
    \date $Date: 2014/09/12 15:40:19 $

    The heaapp library provides a C-accessible framework for coordinating the
    initialization and clean-up of various other support libraries. This
    includes structures and data/function type definitions that allow support
    libraries to interoperate without explicitly depending on each other during
    compilation. The client application code can use heaapp to select from
    available support libraries to provide basic functionality such as user
    input and logging. Support libraries can use heaapp's interface to "publish"
    internal functions and data that have specific signatures and types. This
    allows the libraries to use each others features efficiently but indirectly,
    while remaining loosely coupled.

    The following are advantages to using heaapp rather than having client
    application code directly initialize the needed libraries.
      1. Application code is simplified because it does not need to know details of
         the libraries being used.
      2. Support libraries can work together without explicit interdependencies.
         For example, the parameter library ape can use ahlog for ape's error messages
         without explicitly using any part of ahlog.
      3. Application code has control over which libraries it uses. The division
         of labor between the application code, heaapp, and other support libraries
         allows heaapp to remain completely independent, which in turn means that
         applications using heaapp need not link to any library they do not
         actually need.

    How client application code can use heaapp:

    A. An application wishing to use the libraries "lib1" and "lib2" would
       bracket its main application code with heaapp code as follows:

         #include "heaapp/heaapp.h"

         int lib1_heaapp_connect(HeaAppData * appdata);
         int lib2_heaapp_connect(HeaAppData * appdata);

         int main(int argc, char ** argv) {
           int status = 0;
           HeaAppData appdata = { 0 };

           appdata = heaapp_construct_appdata(argc, argv);

           status = heaapp_lib1_connect(&appdata);

           status = heaapp_lib2_connect(&appdata);

           status = heaapp_initialize(&appdata);

           ...

           status = heaapp_finalize(&appdata);

           return status;
         }

       Note the following regarding the above sample code:
       1. Error checking was omitted for clarity. In production code, the status
          should be checked after each call, and execution should not continue if
          the status is non-0.
/* +++ CR 2014-09-05 JP reviewers requested this be changed to state finalize should be called in any case.
       2. The prototypes lib1_heaapp_connect, lib2_heaapp_connect, etc. should normally
/* +++ CR 2014-09-05 JP reviewers requested this should clarify that these headers are in heaapp and heaapp would need
    to be changed to add new libraries.
          be in library-specific header files. The client application chooses
          which libraries to use by calling the appropriate functions of this pattern.

    B. A support library named "fakelib" would be required to provide the following
       functions. At least the function fakelib_heaapp_connect should be prototyped
       in a public header file that may be included by client application code.

       i. A function with the name and signature:

          int fakelib_heaapp_connect(HeaAppData * appdata);

       This function must be named exactly as shown, with the word "fakelib" replaced
       by the actual library name. This function assigns to members of appdata the
       functions and data elements the library provides, as needed and appropriate for
       the library to be set up and used. Note that this should not in general
       perform any of the library-specific initialization work.

       ii. A function (name is a suggestion) with the signature:

           int fakelib_heaapp_initialize(HeaAppData * appdata);

       This function uses the members of appdata to perform whatever initialization
       the support library needs. A pointer to this function must be assigned to the
       appropriate element of appdata by the heaapp_fakelib_connect function described above.
       Use the static helper function heaapp_add_init_func (see below) for this purpose.

       iii. A function (name is a suggestion) with the signature:

            void fakelib_heaapp_finalize(HeaAppData * appdata);

       This function performs whatever finalization/shutdown/clean-up the support
       library needs. This function also clears all assignments to elements of appdata
       that were made by the function heaapp_fakelib_connect. A pointer to this function
       must be assigned to the appropriate element of appdata by the heaapp_fakelib_connect
       function described above. Use the static helper function heaapp_add_final_func (see below)
       for this purpose.
.
*/
/* +++ CR 2014-09-05 JP reviewers questioned whether this should be HEAAPP rather than HDAPP. Check heautils conventions. */
#ifndef HDAPP_HDAPP_H
#define HDAPP_HDAPP_H

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

/* +++ CR 2014-09-05 JP reviewers requested explanation of this construct, using typedef as well as struct so that C
usage can be C++-like wrt type name. */
struct HeaAppData;
typedef struct HeaAppData HeaAppData;

/* +++ CR 2014-09-05 JP reviewers requested explanation that these are function pointer types. */
typedef int (*HeaAppInitializeFuncType)(HeaAppData *);
typedef void (*HeaAppFinalizeFuncType)(HeaAppData *);
typedef int (*HeaAppIoFuncType)(const char * , const char *, ...);
typedef int (*HeaAppChatIoFuncType)(int, const char *, const char *, ...);

#define HDAPP_NUM_LIB (32)

struct HeaAppData {
  int argc;
  char ** argv;
  char * appname;
/* +++ CR 2014-09-05 JP add appvers and use heautils or other facility to handle the version appropriately. */
  HeaAppIoFuncType printout;
  HeaAppIoFuncType printerr;
  HeaAppChatIoFuncType printinfo;
  HeaAppChatIoFuncType printwarn;
  void (*flushall)(HeaAppData * appdata);
  int (*add_init_func)(HeaAppData * appdata, HeaAppInitializeFuncType func);
  int (*add_final_func)(HeaAppData * appdata, HeaAppFinalizeFuncType func);
  HeaAppInitializeFuncType initializefunc[HDAPP_NUM_LIB];
  HeaAppFinalizeFuncType finalizefunc[HDAPP_NUM_LIB];
/* +++ CR 2014-09-05 JP add this comment: */
  /* FILENAME_MAX is defined in stdio.h. */
  char logfile[FILENAME_MAX]; char logfileset;
  int chatter; char chatterset;
  char debug; char debugset;
  char clobber; char clobberset;
  char history; char historyset;
/* +++ CR 2014-09-05 JP add buffer so that ahfits can participate. Also add heaapp_ahfits.h header. */
};

/** \brief Construct an HeaAppData object with valid default behaviors.
    \param argc The number of command line arguments.
    \param argv Command line arguments.
/* +++ CR 2014-09-05 JP remove the following comment, this is no longer a parameter:
    \param appdata The HeaAppData object being constructed.
*/
HeaAppData heaapp_construct_appdata(int argc, char ** argv);

/** \brief Call all the intialize functions that are connected with this HeaAppData object.
    \param appdata The HeaAppData object.
*/
int heaapp_initialize(HeaAppData * appdata);

/** \brief Call all the finalize functions that are connected with this HeaAppData object.
    \param appdata The HeaAppData object.
*/
int heaapp_finalize(HeaAppData * appdata);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* HDAPP_HDAPP_H */

/* Revision Log
 * $Log: heaapp.h,v $
 * Revision 1.2  2014/09/12 15:40:19  peachey
 * Add comments from code review held on 2014-09-05.
 *
 * Revision 1.1  2014/07/07 17:08:20  peachey
 * Initial version of heaapp library.
 *
 */
