/******************************************************************************
 *   File name: test-xpi.c                                                    *
 *                                                                            *
 * Description: Test executable for XPI that performs a number of commonly    *
 *     needed parameter actions, first using direct Ape calls, then using     *
 *     XPI. Both sets of calls should have the same behavior. This test was   *
 *     developed during the transition between "Legacy XPI" (the original     *
 *     Fortran-based interface used with original Ftools and "XPI with Ape",  *
 *     which uses the more modern Ape implementation for the back-end.        *
 *     During the transition, expected differences between the two exist.     *
 *     When the transition is complete, identical behavior should occur.      *
 *                                                                            *
 *    Language: C or C++                                                      *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "hea_status.h"
#include "xpi.h"
#include "cfortran.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

typedef enum TestConstant {
  GaNumberValues = 5
} TestConstant;

/* TODO: Add prototypes for these to xpi.h so that these declarations are not necessary. */
int OpenDefaultPF(int argc, char ** argv);
int CloseDefaultPF(void);

/******************** Uclgsg *******************/
#define Uclgsg(parname,buffer,n,rmin,rmax,nr,status) \
  CCALLSFSUB7(UCLGSG,uclgsg,STRING,FLOATVV,PCINT,PCFLOAT,PCFLOAT,PCINT,PCINT,parname,buffer,n,rmin,rmax,nr,status)
#define xpisavepar(status) CCALLSFSUB1(XPISAVEPAR,xpisavepar,PCINT,status)

/* Macro that calls either the XPI or Ape version of a function. */
#define CALL(A, B) \
  if (0 != use_xpi) { \
    A; \
    printf("\n"); \
    fflush(stdout); \
    /* \
    if (eOK != status) fprintf(stdout, #A " returned status %d ***\n", status); \
    */ \
  } else { \
    status = B; \
    printf("\n"); \
    fflush(stdout); \
    /* \
    if (eOK != status) fprintf(stdout, #B " returned status %d ***\n", status); \
    */ \
  } \

/* Function to create a new set of command arguments with a different argv[0]. */
int create_cmd_line(const char * tool_name, int argc, char ** argv, char *** new_argv) {
  int status = eOK;
  int size_argv = 0;

  /* Check the arguments. */
  if (0 == tool_name || 0 == argv || 0 == new_argv) {
    status = eNullPointer;
  } else if (0 >= argc) {
    status = eInvalidArgument;
  }

  /* Be extra careful about the array of strings, and count up their total size. */
  if (eOK == status) {
    int ii = 0;
    for (ii = 0; ii < argc; ++ii) {
      if (0 == argv[ii]) status = eNullPointer;
      else size_argv += strlen(argv[ii]) + 1;
    }
  }

  /* Adjust the size of the array of strings to account for the new tool name. */
  if (eOK == status) {
    size_argv += strlen(tool_name) + 1;
    size_argv -= strlen(argv[0]) + 1;
    if (0 >= size_argv) {
      status = eInvalidArgument; /* Not really a bad argument, but this is a problem, so flag it. */
    }
  }

  /* Allocate space for the strings. */
  if (eOK == status) {
    *new_argv = (char **) calloc(argc, sizeof(char *));
    if (0 == *new_argv) {
      status = eDynAllocFailed;
    }

  }

  if (eOK == status) {
    (*new_argv)[0] = (char *) calloc(size_argv, sizeof(char));
    if (0 == (*new_argv)[0]) {
      status = eDynAllocFailed;
    }
  }

  /* Copy contents of the old strings to the new array of arguments. */
  if (eOK == status) {
    int ii = 0;

    /* First argument is the beginning of the string array. */
    strcpy((*new_argv)[ii], tool_name);

    for (ii = 1; ii < argc; ++ii) {
      (*new_argv)[ii] = (*new_argv)[ii-1] + strlen((*new_argv)[ii-1]) + 1;
      strcpy((*new_argv)[ii], argv[ii]);
    }
  }

  /* Clean up. */
  if (eOK != status) {
    if (0 != new_argv) {
      if (0 != *new_argv) free((*new_argv)[0]);
      free(*new_argv);
    }
  }

  return status;
}

int unit_test(int use_xpi, int argc, char ** argv) {
  int status = eOK;
  int xpistatus = 0;
  char ** new_argv = 0;

  /* Banner. */
  if (use_xpi) {
  	printf("\nRunning unit test using XPI ================================================:\n");
  } else {
  	printf("Running unit test using Ape ================================================:\n");
  }
  fflush(stdout);

  /* Clear error condition. */
  ResetHEAStatus();

  /* Copy and modify command line accordingly. */
  if (0 != use_xpi) {
    status = create_cmd_line("test-xpi", argc, argv, &new_argv);
  } else {
    status = create_cmd_line("test-ape", argc, argv, &new_argv);
  }

  if (eOK == status) {
    /* Initialize the system. */
    CALL(OpenDefaultPF(argc, new_argv), ape_trad_init(argc, new_argv));
    if (eOK == status) {
      char ape_ba = 0; int xpi_ba = 0;
      char ape_bh = 0; int xpi_bh = 0;
      double ape_da = 0.; double xpi_da = 0.;
      double ape_dh = 0.; double xpi_dh = 0.;
      char * ape_fa = 0; char xpi_fa[FILENAME_MAX] = "";
      char * ape_fh = 0; char xpi_fh[FILENAME_MAX] = "";
      char * ape_fra = 0; char xpi_fra[FILENAME_MAX] = "";
      char * ape_frh = 0; char xpi_frh[FILENAME_MAX] = "";
      char * ape_fwa = 0; char xpi_fwa[FILENAME_MAX] = "";
      char * ape_fwh = 0; char xpi_fwh[FILENAME_MAX] = "";
      int ape_ia = 0; int xpi_ia = 0;
      int ape_iia = 0; int xpi_iia = 0;
      int ape_ih = 0; int xpi_ih = 0;

#if 0
      short ape_sia = 0; short xpi_sia = 0;
      short ape_sih = 0; short xpi_sih = 0;
      long ape_lia = 0; long xpi_lia = 0;
      long ape_lih = 0; long xpi_lih = 0;
#else
      int ape_sia = 0; int xpi_sia = 0;
      int ape_sih = 0; int xpi_sih = 0;
      int ape_lia = 0; double xpi_lia = 0;
      int ape_lih = 0; double xpi_lih = 0;
#define ape_trad_query_short ape_trad_query_int
#endif
      float ape_ra = 0.; float xpi_ra = 0.;
      float ape_rh = 0.; float xpi_rh = 0.;
      char * ape_sa = 0; char xpi_sa[FILENAME_MAX] = "";
      char * ape_sh = 0; char xpi_sh[FILENAME_MAX] = "";

      char * ape_ga = 0; float xpi_ga[GaNumberValues][GaNumberValues];
      int max_number_elements = GaNumberValues;
      float rmin = 12.; float rmax = 15.;
      int number_elements = GaNumberValues;

      /* Wipe out the array content. */
      memset(xpi_ga, '\0', sizeof(xpi_ga));

      /* Get all the parameters. */
      CALL(Uclgsb("ba", &xpi_ba, &status), ape_trad_query_bool("ba", &ape_ba));
      SetHEAStatus(status); status = 0;

      CALL(Uclgsd("da", &xpi_da, &status), ape_trad_query_double("da", &ape_da));
      SetHEAStatus(status); status = 0;

      /* The following is needed for cfortran to pass the string to Fortran correctly. */
#define BufLen_2 (FILENAME_MAX - 1)
      CALL(Uclgst("fa", xpi_fa, &status), ape_trad_query_file_name("fa", &ape_fa));
      SetHEAStatus(status); status = eOK;

      CALL(Uclgst("fra", xpi_fra, &status), ape_trad_query_file_name("fra", &ape_fra));
      SetHEAStatus(status); status = eOK;

      CALL(Uclgst("fwa", xpi_fwa, &status), ape_trad_query_file_name("fwa", &ape_fwa));
      SetHEAStatus(status); status = eOK;

      CALL(Uclgsi("ia", &xpi_ia, &status), ape_trad_query_int("ia", &ape_ia));
      SetHEAStatus(status); status = 0;

      /* Try to get an INDEF value for an integer parameter.  XPI should
         return status = 3 and APE should return status = eUndefinedValue. */
      CALL(Uclgsi("iia", &xpi_iia, &status), ape_trad_query_int("iia", &ape_iia));
      if (0 != use_xpi) {
          if (3 == status) status = 0;
      } else {
          if (eUndefinedValue == status) status = 0;
      }
      SetHEAStatus(status); status = 0;

      CALL(Uclgss("sia", &xpi_sia, &status), ape_trad_query_short("sia", &ape_sia));
      SetHEAStatus(status); status = 0;

#if 0
      /* Legacy XPI does not support getting a long value. */
      CALL(Uclgsl("lia", &xpi_lia, &status), ape_trad_query_long("lia", &ape_lia));
      SetHEAStatus(status); status = 0;
#endif

      CALL(Uclgsr("ra", &xpi_ra, &status), ape_trad_query_float("ra", &ape_ra));
      SetHEAStatus(status); status = 0;

      CALL(Uclgst("sa", xpi_sa, &status), ape_trad_query_string("sa", &ape_sa));
      SetHEAStatus(status); status = 0;

      CALL(Uclgsg("ga", xpi_ga, &max_number_elements, &rmin, &rmax, &number_elements, &status), \
        ape_trad_query_string("ga", &ape_ga));
      if (0 != use_xpi || eUnknownType != status) SetHEAStatus(status); status = eOK;

      /* Hidden parameters */
      CALL(Uclgsb("bh", &xpi_bh, &status), ape_trad_query_bool("bh", &ape_bh));
      SetHEAStatus(status); status = 0;
      CALL(Uclgsd("dh", &xpi_dh, &status), ape_trad_query_double("dh", &ape_dh));
      SetHEAStatus(status); status = 0;
      CALL(Uclgst("fh", xpi_fh, &status), ape_trad_query_file_name("fh", &ape_fh));
      SetHEAStatus(status); status = eOK;
      CALL(Uclgst("frh", xpi_frh, &status), ape_trad_query_file_name("frh", &ape_frh));
      SetHEAStatus(status); status = eOK;
      CALL(Uclgst("fwh", xpi_fwh, &status), ape_trad_query_file_name("fwh", &ape_fwh));
      SetHEAStatus(status); status = eOK;
      CALL(Uclgsi("ih", &xpi_ih, &status), ape_trad_query_int("ih", &ape_ih));
      SetHEAStatus(status); status = 0;
      CALL(Uclgss("sih", &xpi_sih, &status), ape_trad_query_short("sih", &ape_sih));
      SetHEAStatus(status); status = 0;
#if 0
      CALL(Uclgsl("lih", &xpi_lih, &status), ape_trad_query_long("lih", &ape_lih));
      SetHEAStatus(status); status = 0;
#endif
      CALL(Uclgsr("rh", &xpi_rh, &status), ape_trad_query_float("rh", &ape_rh));
      SetHEAStatus(status); status = 0;
      CALL(Uclgst("sh", xpi_sh, &status), ape_trad_query_string("sh", &ape_sh));
      SetHEAStatus(status); status = 0;

      /* Save the par files before renaming so we can diff the 'get' pfiles. */
      CALL(xpisavepar(&status), ape_trad_save());

      if (0 != use_xpi) {
        status = rename("test-xpi.par","test-xpi-get.par");
      } else {
        status = rename("test-ape.par","test-ape-get.par");
      }

      if (eOK == status) {

        /* Put boolean */
        ape_ba = xpi_ba = 0;
        ape_bh = xpi_bh = 0;
        CALL(Uclpsb("ba", xpi_ba, &status), ape_trad_set_bool("ba", ape_ba));
        SetHEAStatus(status); status = 0;
        CALL(Uclpsb("bh", xpi_bh, &status), ape_trad_set_bool("bh", ape_bh));
        SetHEAStatus(status); status = 0;

        /* Put double */
        ape_da = ape_dh = 2.3456789012;
        xpi_da = xpi_dh = 2.3456789012;
        CALL(Uclpsd("da", xpi_da, &status), ape_trad_set_double("da", ape_da));
        SetHEAStatus(status); status = 0;
        CALL(Uclpsd("dh", xpi_dh, &status), ape_trad_set_double("dh", ape_dh));
        SetHEAStatus(status); status = 0;

#if 0
        /* Put filename */
        CALL(Uclpst("fa", "SPUD.FITS", &status), ape_trad_set_file_name("fa", "SPUD.FITS"));
        SetHEAStatus(status); status = 0;
        CALL(Uclpst("fh", "SPUD.FITS", &status), ape_trad_set_file_name("fh", "SPUD.FITS"));
        SetHEAStatus(status); status = 0;

        /* Put filename (read) */
        CALL(Uclpst("fra", ".", &status), ape_trad_set_file_name("fra", "."));
        SetHEAStatus(status); status = 0;
        CALL(Uclpst("frh", ".", &status), ape_trad_set_file_name("frh", "."));
        SetHEAStatus(status); status = 0;

        /* Put filename (write) */
        CALL(Uclpst("fwa", "SPUD.FITS", &status), ape_trad_set_file_name("fwa", "SPUD.FITS"));
        SetHEAStatus(status); status = 0;
        CALL(Uclpst("fwh", "SPUD.FITS", &status), ape_trad_set_file_name("fwh", "SPUD.FITS"));
        SetHEAStatus(status); status = 0;
#endif

        /* Put integer */
        ape_ia = xpi_ia = 2000000001;
        ape_ih = xpi_ih = 2000000001;
        CALL(Uclpsi("ia", xpi_ia, &status), ape_trad_set_int("ia", ape_ia));
        SetHEAStatus(status); status = 0;
        CALL(Uclpsi("ih", xpi_ih, &status), ape_trad_set_int("ih", ape_ih));
        SetHEAStatus(status); status = 0;

        /* Put an INDEF value into an integer parameter.  XPI requires that
           you send status = 3 in order for this to succeed. */
	xpistatus = 3;
        CALL(Uclpst("iia", "INDEF", &xpistatus), ape_trad_set_string("iia", "INDEF"));
        SetHEAStatus(status); status = 0;
	xpistatus = 0;

#if 0
        /* Put short int */
        ape_sia = xpi_sia = -16383;
        ape_sih = xpi_sih = -16383;
        CALL(Uclpss("sia", xpi_sia, &status), ape_trad_set_short("sia", ape_sia));
        SetHEAStatus(status); status = 0;
        CALL(Uclpss("sih", xpi_sih, &status), ape_trad_set_short("sih", ape_sih));
        SetHEAStatus(status); status = 0;

        /* Put long int */
        ape_lia = xpi_lia = 42424242;
        ape_lih = xpi_lih = 42424242;
        CALL(Uclpsl("lia", xpi_lia, &status), ape_trad_set_long("lia", ape_lia));
        SetHEAStatus(status); status = 0;
        CALL(Uclpsl("lih", xpi_lih, &status), ape_trad_set_long("lih", ape_lih));
        SetHEAStatus(status); status = 0;
#endif

        /* Put real/float */
        ape_ra = xpi_ra = 2.3456;
        ape_rh = xpi_rh = 2.3456;
        CALL(Uclpsr("ra", xpi_ra, &status), ape_trad_set_float("ra", ape_ra));
        SetHEAStatus(status); status = 0;
        CALL(Uclpsr("rh", xpi_rh, &status), ape_trad_set_float("rh", ape_rh));
        SetHEAStatus(status); status = 0;

        /* Put string */
        CALL(Uclpst("sa", "STRING", &status), ape_trad_set_string("sa", "STRING"));
        SetHEAStatus(status); status = 0;
        CALL(Uclpst("sh", "STRING", &status), ape_trad_set_string("sh", "STRING"));
        SetHEAStatus(status); status = 0;
      }

#undef BufLen_2

      /* Clean up parameters. */
      free(ape_ga); ape_ga = 0;
      free(ape_sa); ape_sa = 0;
      free(ape_fwa); ape_fwa = 0;
      free(ape_fra); ape_fra = 0;
      free(ape_fa); ape_fa = 0;

      free(ape_sh); ape_sa = 0;
      free(ape_fwh); ape_fwa = 0;
      free(ape_frh); ape_fra = 0;
      free(ape_fh); ape_fa = 0;

      status = GetHEAStatus(0);
    }

    /* Shut down the system. */
    ResetHEAStatus();
    CALL(CloseDefaultPF(), ape_trad_close(1));

    if (0 != use_xpi) {
      status = rename("test-xpi.par","test-xpi-put.par");
    } else {
      status = rename("test-ape.par","test-ape-put.par");
    }
  }

  /* Clean up copy of arguments. */
  if (0 != new_argv) {
    if (0 != *new_argv) free(*new_argv);
    free(new_argv);
  }

  return status;
}

int main(int argc, char ** argv) {
  int status = eOK;
  int ape_status = 0;
  int xpi_status = 0;

  /* The substance of the test is: first execute the test using ape explicitly and save the results,
     then repeat using xpi. This assumes that two identical parameter files are in the path, named
     test-ape.par and test-xpi.par. */
  ape_status = unit_test(0, argc, argv);

  xpi_status = unit_test(1, argc, argv);

  /* Report status back to caller. */
  if (ape_status != xpi_status) {
    fprintf(stderr, "Unit test using Ape returned status %d; using XPI returned status %d.\n", ape_status, xpi_status);
    status = 1;
  } else if (0 != ape_status) {
    status = 1;
  }

  return status;
}

/******************************************************************************
 * $Log: test-xpi.c,v $
 * Revision 1.6  2011/07/13 18:11:12  irby
 * Add tests for INDEF values.
 *
 * Revision 1.5  2010/12/10 21:21:14  irby
 * Save "get" par files before renaming them for "put" mode.
 *
 * Revision 1.4  2010/11/18 21:00:36  irby
 * Add uclgs* calls for hidden parameters.
 *
 * Revision 1.3  2010/11/12 21:00:09  irby
 * Add uclp* tests (also for hidden pars, for future use pending further
 * changes to ape), and remove status 2 override for file types which is
 * no longer needed.
 *
 * Revision 1.2  2010/06/25 15:19:19  irby
 * Don't use CALL macro for XPI/APE printf statements.
 *
 * Revision 1.1  2010/05/28 14:34:55  peachey
 * Add unit test for xpi.
 *
 *****************************************************************************/
