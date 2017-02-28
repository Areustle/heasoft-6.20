/******************************************************************************
 *   File name: heautils_test.c                                               *
 *                                                                            *
 * Description: Test program for heautils library.                            *
 *                                                                            *
 *    Language: C or C++                                                      *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/

/******************************************************************************
 * Header files.                                                              *
 ******************************************************************************/
#include <stdio.h>
#include <string.h>

#ifdef WIN32
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include "headas_utils.h"
/******************************************************************************/

/* C/C++ compatibility. */
#ifdef __cplusplus
extern "C" {
#endif

  /****************************************************************************
   * Constants.                                                               *
   ****************************************************************************/
  /****************************************************************************/

  /****************************************************************************
   * Type definitions.                                                        *
   ****************************************************************************/
  /****************************************************************************/

  /****************************************************************************
   * Global variable definitions.                                             *
   ****************************************************************************/
  /****************************************************************************/

  /****************************************************************************
   * Static variable definitions.                                             *
   ****************************************************************************/
  /****************************************************************************/

  /****************************************************************************
   * Static function definitions.                                             *
   ****************************************************************************/
  static int test_file_check(int status) {
    /* Save inherited status value. */
    int passed_status = status;

    /* Local variable to hold the return values of HDfile_check. */
    int retval = 0;

    /* Local variable to hold the expected return values of HDfile_check. */
    int expected_retval = 1;

    /* Name of test file. */
    char file_name[128] = "";

    /* Tests for a non-existent file. */
    strcpy(file_name, "non-existent");

    retval = HDfile_check(file_name, 0);
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", 0) returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "r");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"r\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "w");
    expected_retval = 0;
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"w\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    /* Tests for readable and writable file. */
    strcpy(file_name, "test_writable.fits");

    retval = HDfile_check(file_name, 0);
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", 0) returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "r");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"r\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "w");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"w\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    /* Tests for readable only file. */
    strcpy(file_name, "test_readonly.fits");

    /* Make sure it is only accessible for reading. */
#ifdef WIN32
    /* TODO: what does one put here to make the file read-only on Windows? */
#else
    chmod(file_name, S_IRUSR | S_IRGRP | S_IROTH);
#endif

    retval = HDfile_check(file_name, 0);
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", 0) returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "r");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"r\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    /* This is regrettably not how one would expect this to behave, but fits_file_exists only tells whether a file exists. */
    retval = HDfile_check(file_name, "w");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"w\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    /* Tests for compressed file. */
    strcpy(file_name, "test_compressed.fits");

    retval = HDfile_check(file_name, 0);
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", 0) returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "r");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"r\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "w");
    expected_retval = 1;
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"w\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    /* Tests for extended syntax, read-only. */
    strcpy(file_name, "test_writable.fits[1](CHANNEL>200 && CHANNEL<300)");

    retval = HDfile_check(file_name, 0);
    expected_retval = 0;
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", 0) returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    retval = HDfile_check(file_name, "r");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"r\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    /* This is regrettably not how one would expect this to behave, but fits_file_exists only tells whether a file
       exists. Also, cfitsio may actually let a user modify a gzipped file, even though it can't be saved. */
    retval = HDfile_check(file_name, "w");
    if (expected_retval != retval) {
      status = 1;
      fprintf(stderr, "test_file_check: HDfile_check(\"%s\", \"w\") returned %d, not %d.\n", file_name, retval, expected_retval);
    }

    /* Use inherited status if it was non-0. */
    if (0 != passed_status) status = passed_status;
    return status;
  }
  /****************************************************************************/

  /****************************************************************************
   * Function definitions.                                                    *
   ****************************************************************************/
  /****************************************************************************/

/* C/C++ compatibility. */
#ifdef __cplusplus
}
#endif

int main() {
  int status = 0;

  /* Test HDfile_check function. */
  status = test_file_check(status);

  return status;
}
/******************************************************************************
 * $Log: heautils_test.c,v $
 * Revision 1.2  2006/06/23 20:55:13  peachey
 * Rework file checking mechanism to be more careful about interpreting
 * output from system access checks and cfitsio's fits_file_exists function.
 *
 * Revision 1.1  2004/08/17 19:24:23  peachey
 * Add functions for testing for existence of files: HDfile_system_check,
 * which queries the file system, and HDfile_check, which calls
 * HDfile_system_check, but then also uses cfitsio's fits_file_exists function
 * to check for gzipped files, extended syntax, etc.
 *
 ******************************************************************************/
