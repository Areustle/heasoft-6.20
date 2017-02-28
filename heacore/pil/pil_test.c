#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include "pil.h"
#include "pil_error.h"

/* Get the status of this test. */
int get_status(void);

/* Set the status of this test. If already non-0, this has no effect. */
void set_status(int status);

void test_format_error(void);

void test_error(const char * fmt, ...);

void test_info(const char * fmt, ...);

int test_msg_logger(char * msg);

void test_command_line(int argc, char ** argv);

void test_one_command_line(int argc, char ** argv, int num_par, const char ** par_name, const char ** par_value,
  int pil_init_status, int pil_get_status);

void test_use_indef_float(void);

int main(int argc, char ** argv) {
  /* Enable error logging. */
  PILSetLoggerFunction(&test_msg_logger);

  /* Test command line argument parsing. */
  test_command_line(argc, argv);

  /* Test getting a floating point parameter as INDEF. */
  test_use_indef_float();

  /* Report errors. */
  test_format_error();

  return 0 != get_status() ? 1 : 0;
}

static int s_status = 0;

int get_status(void) { return s_status; }

void set_status(int status) { s_status = (0 != s_status) ? s_status : status; }

void test_format_error(void) {
  int status = get_status();
  if (PIL_OK != status) {
    const char * msg = PIL_err_handler(status);
    if (0 != msg) {
      test_info("unexpected PIL error %d: %s\n", status, msg);
    }
    test_error("TEST FAILED!\n");
  } else {
    test_info("Test Passed.\n");
  }
}

void test_error(const char * fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "pil_test: ERROR: ");
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}

void test_info(const char * fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "pil_test: INFO: ");
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}

int test_msg_logger(char * msg) {
  fprintf(stderr, "PIL error: %s\n", msg);
  return PIL_OK;
}

void cat_cmd_line(int argc, char ** argv, char * line) {
  if (0 >= argc) {
    *line = '\0';
  } else {
    int ii;

    strcpy(line, argv[0]);

    for (ii = 1; ii < argc; ++ii) {
      strcat(line, " \"");
      strcat(line, argv[ii]);
      strcat(line, "\"");
    }
  }
}

void test_command_line(int argc, char ** argv) {
  const char * par_name[] = { "par0", "par1", "par2", "par3" };
  const char * default_par_value[] = { "default-val0", "-1", "default-val2", "default-val3" };
  const char * par_value[sizeof(default_par_value)/sizeof(char *)];
  int status = PIL_OK;
  char line[PIL_LINESIZE];

  /* First test the command line supplied by the user, if any arguments were given. */
  if (argc > 1) {
    test_info("test_command_line: testing command line supplied by user.\n", line, status);
    cat_cmd_line(argc, argv, line);
    status = PILInit(argc, argv);
    test_info("test_command_line: for line '%s', PILInit returned status %d.\n", line, status);
    if (PIL_OK == status) {
      unsigned int ii;
      for (ii = 0; ii < sizeof(par_name) / sizeof(const char *); ++ii) {
        *line = '\0';
        status = PILGetAsString(par_name[ii], line);
        if (PIL_OK != status) {
          test_error("test_command_line: PILGetAsString(\"%s\", line) returned status %d.\n", status);
          set_status(status);
        } else {
          test_info("test_command_line: parameter \"%s\" has value \"%s\".\n", par_name[ii], line);
        }
      }
    }
    test_info("test_command_line: does this make sense? (Note: no parameters will be saved).\n");
    test_info("test_command_line: to run automated tests, run this again with no command line arguments.\n");
    /* Close without saving parameters. */
    PILClose(-1);
    return;
  } else {
    test_info("test_command_line: to test by hand, run this again with one or more command line arguments.\n");
  }

  test_info("\ntest_command_line: starting automated tests.\n");

  /* Disable prompts for all automated tests. */
  PILOverrideQueryMode(PIL_QUERY_OVERRIDE);

  /* Test nothing on command line. */
  { char * argv[] = { "pil_test" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test single positional parameter. */
  { char * argv[] = { "pil_test", "val0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test two positional parameters. */
  { char * argv[] = { "pil_test", "val0", "1" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one explicit parameter. */
  { char * argv[] = { "pil_test", "par0=val0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one explicit parameter which is not the first parameter in the file. */
  { char * argv[] = { "pil_test", "par1=1" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[1] = "1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test two explicit parameters. */
  { char * argv[] = { "pil_test", "par0=val0", "par1=1" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test two explicit parameters in reverse order. */
  { char * argv[] = { "pil_test", "par1=1", "par0=val0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one positional, one explicit parameter. */
  { char * argv[] = { "pil_test", "val0", "par1=1" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one positional, one explicit parameter in reverse order. */
  { char * argv[] = { "pil_test", "par1=1", "val0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one explicit parameter with simulated space after the =. */
  { char * argv[] = { "pil_test", "par0=", "val0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one explicit parameter with simulated space before the =. */
  { char * argv[] = { "pil_test", "par0", "=val0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one explicit parameter with simulated space before and after the =. */
  { char * argv[] = { "pil_test", "par0", "=", "val0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one explicit parameter with simulated space before and after the =, followed by positional parameter. */
  { char * argv[] = { "pil_test", "par1", "=", "1", "val0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one explicit parameter with space before and after the = (double quotes around whole argument). */
  { char * argv[] = { "pil_test", "par0  = \tval0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one positional, one explicit parameter with simulated space after the =. */
  { char * argv[] = { "pil_test", "val0", "par1=", "1" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one positional, one explicit parameter with simulated space after the =, positional parameter is not the first
     one in the file. */
  { char * argv[] = { "pil_test", "1", "par0=", "val0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one positional, one explicit parameter with simulated space before the =, positional parameter is not the first
     one in the file. */
  { char * argv[] = { "pil_test", "par1", "=1", "val0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test one positional, one explicit parameter with simulated space before the =, positional parameter is not the first
     one in the file and reverse order of arguments. */
  { char * argv[] = { "pil_test", "par0", "=val0", "1" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test explicit parameters with all blank values. */
  { char * argv[] = { "pil_test", "par0=", "par2=", "par3=" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "";
    par_value[2] = "";
    par_value[3] = "";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test explicit parameters with all values assigned, in variety of flavors, with extra spaces around =. */
  { char * argv[] = { "pil_test", "par1 = ", "1", "par3", " = ", "val3", "par0", " = val0", "par2 = val2" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    par_value[2] = "val2";
    par_value[3] = "val3";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test positional parameters whose values happen to be the same as parameter names. */
  { char * argv[] = { "pil_test", "par0", "par2", "=", "par2", "1", "par3" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "par0";
    par_value[1] = "1";
    par_value[2] = "par2";
    par_value[3] = "par3";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test explicit assignment from a value which happens to be the same as other parameter name. */
  { char * argv[] = { "pil_test", "par0", "=", "par1" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "par1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test explicit assignment from a value which happens to be an equals sign. */
  { char * argv[] = { "pil_test", "par0", "=", "=" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "=";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test parameters which happen to have equals signs in them. */
  { char * argv[] = { "pil_test", "par2 =", "= val2", "=", "1", "=val3" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "=";
    par_value[1] = "1";
    par_value[2] = "= val2";
    par_value[3] = "=val3";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test parameter duplicated positionally. This would be an error in XPI, but it's OK here, though perhaps surprising. */
  { char * argv[] = { "pil_test", "0", "par0=val0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "0";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test trailing = sign 1. */
  { char * argv[] = { "pil_test", "par0", "==" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "=";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test trailing = sign 2. */
  { char * argv[] = { "pil_test", "par0", "=", "=" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "=";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test trailing = sign 3. */
  { char * argv[] = { "pil_test", "=" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "=";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test = sign in value. */
  { char * argv[] = { "pil_test", "=val0" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "=val0";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test a common typo. Note that par2 is hidden, which is why par3 is assigned pr2=val2. Although this is
     really an error, there is no way for pil to detect it. */
  { char * argv[] = { "pil_test", "val0", "1", "pr2=val2" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    par_value[3] = "pr2=val2";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_OK, PIL_OK);
  }
  /* Test another common typo, which results in there being too many parameters because par2 is hidden,
     and does not take a positional parameter. */
  { char * argv[] = { "pil_test", "val0", "1", "val3", "pr2=val2" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0";
    par_value[1] = "1";
    par_value[3] = "pr2=val2";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_BOGUS_CMDLINE, PIL_OK);
  }
  /* Test another common mistake: parameter duplicated. */
  { char * argv[] = { "pil_test", "par0=val0A", "par1=1", "par0=", "val0B" };
    memcpy(par_value, default_par_value, sizeof(default_par_value));
    par_value[0] = "val0A";
    par_value[1] = "1";
    test_one_command_line(sizeof(argv) / sizeof(char *), argv, sizeof(par_name) / sizeof(char *), par_name, par_value,
      PIL_BOGUS_CMDLINE, PIL_OK);
  }
}

void test_one_command_line(int argc, char ** argv, int num_par, const char ** par_name, const char ** par_value,
  int pil_init_status, int pil_get_status) {
  int status = PIL_OK;
  char line[PIL_LINESIZE];

  cat_cmd_line(argc, argv, line);

  test_info("test_one_command_line: testing line '%s'.\n", line);
  status = PILInit(argc, argv);
  if (pil_init_status != status) {
    set_status(status != 0 ? status : 1);
    test_error("test_one_command_line: PILInit returned status %d, not %d as expected.\n", status, pil_init_status);
  }
  if (PIL_OK != status) {
    if (pil_init_status == status) {
      test_info("test_one_command_line: expected: PILInit failed; skipping comparison of parameter values.\n");
    } else {
      test_info("test_one_command_line: unexpected: PILInit failed; skipping comparison of parameter values.\n");
    }
  } else {
    int ii;
    char first_error = 1;
    for (ii = 0; ii < num_par; ++ii) {
      char value[PIL_LINESIZE];
      status = PILGetAsString(par_name[ii], value);
      if (pil_get_status != status) {
        set_status(status != 0 ? status : 1);
        if (0 != first_error) {
          test_error("test_one_command_line: after PILInit succeeded for line '%s':\n", line);
          first_error = 0;
        }
        test_error("test_one_command_line: PILGetAsString(\"%s\", value) returned %d, not %d as expected.\n", par_name[ii], status,
          pil_get_status);
      }
      if (0 != strcmp(value, par_value[ii])) {
        set_status(1);
        if (0 != first_error) {
          test_error("test_one_command_line: after PILInit succeeded for line '%s':\n", line);
          first_error = 0;
        }
        test_error("test_one_command_line: after applying command line arguments, parameter \"%s\" is \"%s\" not \"%s\" "
          "as expected.\n", par_name[ii], value, par_value[ii]);
      }
    }
  }

  /* Close with non-0 status so parameters will not be saved. */
  PILClose(-1);
}

void test_use_indef_float(void) {
  char * argv[] = { "pil_test", "par1=INDEF" };
  int argc = sizeof(argv) / sizeof(char *);
  int status = 0;
  char value[PIL_LINESIZE] = "";

  /* Enable prompts for this test. */
  PILOverrideQueryMode(PIL_QUERY_OVERRIDE);

  test_info("test_use_indef_float: setting par1=INDEF on the command line.\n");
  /* Punch in INDEF value. */
  status = PILInit(argc, argv);
  if (PIL_OK != status) {
    test_error("test_use_indef_float: unexpected: PILInit failed with status %d. Skipping rest of test.\n", status);
    set_status(status);
    return;
  }

  /* Get par1 as a string, so as to allow INDEF. */
  test_info("test_use_indef_float: testing PILGetAsString\n");
  status = PILGetAsString("par1", value);
  if (PIL_OK != status) {
    test_error("test_use_indef_float: unexpected: PILGetAsString failed with status %d. Skipping rest of test.\n", status);
    set_status(status);
  } else if (0 != strcmp(value, "INDEF")) {
    test_error("test_use_indef_float: unexpected: PILGetAsString returned a value of %s, not \"INDEF\" as expected.\n",
      value);
    set_status(1);
  }

  /* Close with non-0 status so parameters will not be saved. */
  PILClose(-1);
}
