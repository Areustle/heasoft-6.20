/******************************************************************************
 *   File name: test_heainit.c                                                *
 *                                                                            *
 * Description: Test code for heainit library.                                *
 *                                                                            *
 *    Language: C or C++                                                      *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 ******************************************************************************/

/******************************************************************************
 * Header files.                                                              *
 ******************************************************************************/
#include "headas.h"

#include "ahlog/ahlog.h"
#include "ape/ape_error.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
/******************************************************************************/

/* C/C++ compatibility. */
#ifdef __cplusplus
extern "C" {
#endif

static int s_status = 0;
static void set_status(int status) { if (0 == s_status) s_status = status; }
static int get_status(void) { return s_status; }
/* Note: keep this tag split in two so CVS doesn't replace it. */
static const char s_tooltag [] = "$Name: heasoft6_20_20170113 $";

void test_special_conditions(void);

int main(int argc, char *argv[]) {
  /* First perform a standard start up and read the testmode parameter. */
  char * testmode = 0;
  int status = headas_start_up(argc, argv, s_tooltag);
  if (eOK != status) {
    fprintf(stderr, "test_heainit: headas_start_up returned status %d\n", status);
    set_status(status);
  }

  if (eOK == status) {
    status = ape_trad_query_string("testmode", &testmode);
    if (eOK != status) {
      fprintf(stderr, "test_heainit: ape_trad_query_string(testmode) returned status %d\n", status);
      set_status(status);
    }
  }

  if (eOK == status) {
    if (0 != testmode && 0 == strcmp(testmode, "SPECIAL")) {
      status = headas_shut_down();
      set_status(status);

      /* Test special input conditions of headas_start_up that
         cannot be done simply by setting command line arguments. */
      test_special_conditions();
    } else {
      /* The other mode of the test just calls headas_start_up using the supplied
         command line. This allows test cases to be constructed using the
         command line and/or parameter settings. */

      /* Report internal states. */
      printf("Clobber (from ahlog) is %s\n", 0 != ahlog_get_clobber()? "enabled" : "disabled");
      printf("Clobber (from heautils) is %s\n", 0 != headas_clobpar ? "enabled" : "disabled");
      printf("Chatter (from ahlog) is %d\n", ahlog_get_chatter());
      printf("Debug (from ahlog) is %s\n", 0 != ahlog_get_debug()? "enabled" : "disabled");
      printf("Executable name (from ahlog) is %s\n", ahlog_get_executable_name());
      { char name[128] = "Failed to get name"; get_toolname(name);
        printf("Executable name (from heautils) is %s\n", name);
      }
      { char version[128] = "Failed to get version"; get_toolversion(version);
        printf("Executable version (from heautils) is %s\n", version);
      }
      { int history = 0;
        get_history(&history);
        printf("Heautils history is %s\n", 0 != history ? "enabled" : "disabled");
      }
      /* printf("Logfile (from ahlog) is "); printf(0 != ahlog_get_logfile() ? "enabled\n" : "disabled\n"); */

      status = headas_shut_down();
      set_status(status);
    }
  } else {
    fprintf(stderr, "test_heainit: unable to start test -- aborting.\n");
    headas_shut_down(); /* Ignore status because this clause is error clean-up. */
  }

  return 0 != get_status() ? 1 : 0;
}

#define TESTBUFSIZE (512)

void test_special_conditions(void) {
  const char * tmp_pfiles = getenv("PFILES");
  char * orig_pfiles = (0 != tmp_pfiles) ? (char *) malloc((strlen(tmp_pfiles) + 1) * sizeof(char)): 0;
  if (0 != orig_pfiles) strcpy(orig_pfiles, tmp_pfiles);

  /* Verify behavior when headas_start_up is called for a tool that does not
     have a parmaeter file. */
  { char * argv[] = { "no_par_file" };
    int argc = sizeof(argv)/sizeof(argv[0]);
    int special_error = UCHAR_MAX;

    int status = headas_start_up(argc, argv, s_tooltag);
    if (eFileNotFound != status) {
      set_status(special_error);
      fprintf(stderr, "test_heainit (test_special_conditions): headas_start_up "
        "called for non existent parameter file returned status %d, not %d as expected\n",
        status, eFileNotFound);
    }
    headas_shut_down();
  }

  /* Verify behavior when headas_start_up is called for a tool that does
     have a parmaeter file, but which is missing all the "standard" parameters. */
  { FILE * fp = 0;
    fp = fopen("./empty.par", "w+");
    if (0 == fp) {
      fprintf(stderr, "test_heainit: unable to create file empty.par. Error is %d:\n", errno);
      perror(0);
    } else {
      fclose(fp);
    }
  }

  setenv("PFILES", ".", 1);
  { char * argv[] = { "./empty" };
    int argc = sizeof(argv)/sizeof(argv[0]);
    int special_error = UCHAR_MAX;
    int status = headas_start_up(argc, argv, "");
    if (eOK != status) {
      set_status(special_error);
      fprintf(stderr, "test_heainit (test_special_conditions): headas_start_up "
        "called for tool with empty par file returned status %d, not %d as expected\n",
        status, eOK);
    }
    if (eOK == status) {
      /* Confirm name and version setting. */
      char name[TESTBUFSIZE] = "Bogus tool name -- should be overwritten.";
      char version[TESTBUFSIZE] = "Bogus version string -- should be overwritten.";
      get_toolname(name); /* Should not include the ./ */
      if (0 != strcmp(name, "empty")) {
        set_status(special_error);
        fprintf(stderr, "test_heainit (test_special_conditions): get_toolname "
          "called for tool with empty par file gave name \"%s\", not \"empty\" as expected\n",
          name);
      }
      get_toolversion(version);
      if (0 != strcmp(version, "")) {
        set_status(special_error);
        fprintf(stderr, "test_heainit (test_special_conditions): get_toolversion "
          "called for tool with empty par file gave version \"%s\", not \"\" as expected\n",
          version);
      }
    }
    headas_shut_down();
  }
  if (0 != orig_pfiles) setenv("PFILES", orig_pfiles, 1);

  /* Test behavior when headas_start_up is called for a compliant tool, but with various
     version strings. */
  { char * argv[] = { "test_heainit" };
    int argc = sizeof(argv)/sizeof(argv[0]);
    int special_error = UCHAR_MAX;

    /* Test version string that should equate to empty. */
    /* Note: need to split up the two dollar signs here to defeat CVS expansion. */ 
    int status = headas_start_up(argc, argv, "$Name: " 
                                 "$");
    if (eOK != status) {
      set_status(special_error);
      fprintf(stderr, "test_heainit (test_special_conditions): headas_start_up "
        "called for compliant tool returned status %d, not %d as expected\n",
        status, eOK);
    }
    if (eOK == status) {
      /* Confirm version setting. Should have equated to empty string. */
      char name[TESTBUFSIZE] = "Bogus tool name -- should be overwritten.";
      char version[TESTBUFSIZE] = "Bogus version string -- should be overwritten.";
      get_toolname(name); /* Should not include the ./ */
      if (0 != strcmp(name, "test_heainit")) {
        set_status(special_error);
        fprintf(stderr, "test_heainit (test_special_conditions): get_toolname "
          "called for compliant tool gave name \"%s\", not \"test_heainit\" as expected\n",
          name);
      }
      get_toolversion(version);
      if (0 != strcmp(version, "")) {
        set_status(special_error);
        fprintf(stderr, "test_heainit (test_special_conditions): get_toolversion "
          "called for compliant tool with blank version gave version \"%s\", not \"\" as expected\n",
          version);
      }
    }
    headas_shut_down();

    /* Test version string with leading and trailing spaces. */
    /* Note: need to split up the two dollar signs here to defeat CVS expansion. */ 
    status = headas_start_up(argc, argv, "$Name:  \t" 
                                 "headas_fake_tag_string\t \t $");
    if (eOK != status) {
      set_status(special_error);
      fprintf(stderr, "test_heainit (test_special_conditions): headas_start_up "
        "called for compliant tool returned status %d, not %d as expected\n",
        status, eOK);
    }
    if (eOK == status) {
      /* Confirm version setting. Should have equated to empty string. */
      char version[TESTBUFSIZE] = "Bogus version string -- should be overwritten.";
      get_toolversion(version);
      if (0 != strcmp(version, "headas_fake_tag_string")) {
        set_status(special_error);
        fprintf(stderr, "test_heainit (test_special_conditions): get_toolversion "
          "called for compliant tool gave version \"%s\", not \"headas_fake_tag_string\" as expected\n",
          version);
      }
    }
    headas_shut_down();

    /* Test version string with no leading or trailing spaces. */
    /* Note: need to split up the two dollar signs here to defeat CVS expansion. */ 
    status = headas_start_up(argc, argv, "$Nameheadas no trailing/leading space"
                                 "$");
    if (eOK != status) {
      set_status(special_error);
      fprintf(stderr, "test_heainit (test_special_conditions): headas_start_up "
        "called for compliant tool returned status %d, not %d as expected\n",
        status, eOK);
    }
    if (eOK == status) {
      /* Confirm version setting. Should have equated to empty string. */
      char version[TESTBUFSIZE] = "Bogus version string -- should be overwritten.";
      get_toolversion(version);
      if (0 != strcmp(version, "headas no trailing/leading space")) {
        set_status(special_error);
        fprintf(stderr, "test_heainit (test_special_conditions): get_toolversion "
          "called for compliant tool gave version \"%s\", not \"headas no trailing/leading space\" as expected\n",
          version);
      }
    }
    headas_shut_down();
  }
}

/* C/C++ compatibility. */
#ifdef __cplusplus
}
#endif

/******************************************************************************
 * $Log: test_heainit.c,v $
 * Revision 1.1  2014/01/03 22:31:48  peachey
 * Add and test new functionality headas_start_up.
 *
 *
 ******************************************************************************/
