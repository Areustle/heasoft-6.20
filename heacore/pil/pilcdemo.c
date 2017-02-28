/**************************************************************************************

	I N T E G R A L   S C I E N C E   D A T A   C E N T E R

	P A R A M E T E R   I N T E R F A C E   L I B R A R Y

Copyright:	(c) 1998 ISDC http://obswww.unige.ch/isdc/
File:		pilcdemo.c
Description:	Parameter Interface Library - sample C program
Authors:	Jerzy.Borkowski@obs.unige.ch (JB)
History:	17-Aug-98 (JB) : version 1.0 release

**************************************************************************************/

/*
 * this program demonstrates how to call PIL library from C code
 * note : this program uses only PIL library (calls PILInit())
 * so it is _NOT_ appropriate for ISDC executables which should
 * call CommonInit(). Should you wish to see sample ISDC compliant
 * program look for it in  Common library directory or see ISDCcopy.c
 * file in this directory.
 *
 * This program does _NOTHING_ useful, it simply displays all
 * parameters from parameter file prompting for new value, if
 * default value is out of range. Program bypasses official API
 * routines, so it is not recommended to follow it. It is included
 * for testing/debug purposes only.
 *
*/


#include <stdio.h>
#include <pil.h>

/* For SCREW 1498: This is a file access checker which returns 1 (file exists)
   for every file type (except a null pointer for file name). */
static int file_check(const char * file_name, const char * open_mode) {
  if (0 == file_name) return 0; /* Null file names do not exist. */
  if (0 == open_mode) {
    /* printf("Checking whether %s exists\n", file_name); */
  } else if (*open_mode == 'r') {
    /* printf("Checking whether %s is readable\n", file_name); */
  } else if (*open_mode == 'w') {
    /* printf("Checking whether %s is writable\n", file_name); */
  }
  return 1;
}

int	main(int argc, char **argv)
{ char minmaxstr[PIL_LINESIZE+100], modebuf[5];
  PIL_VALUE vmin, vmax;
  PIL_PARAM *pp;
  char buf[PIL_LINESIZE];
  int r, i;
  int value = 0;
  int save_mode = 0;
  int test_failed = 0; /* Used to determine whether this test program was successful. */

if (PIL_OK != (r = PILInit(argc, argv)))
  { printf("PILInit failed : %s\n", PIL_err_handler(r));
    return(10);
  } 

for (i=0; i<PILDefaultPFile.pcnt; i++)
 { pp = (PILDefaultPFile.pp)+i;
   switch (pp->format)
    { case PIL_FORMAT_BLANK:
      case PIL_FORMAT_COMMENT:
		printf("%s\n", pp->strline);
		break;
      case PIL_FORMAT_OK:
		/* The infile2 parameter must remain in an invalid state for the duration of this test.
		   It must not be prompted for, because in that case, PIL will only accept the name of
		   an existing file. This would break the test below to make sure the file access checking
		   function works properly. */
		if (0 == strcmp(pp->strname, "infile2")) break;
		minmaxstr[0] = 0;
		if (PIL_OK == PIL_get_range(&PILDefaultPFile, i, &vmin, &vmax))
		  sprintf(minmaxstr, "min=%s, max=%s", pp->strmin, pp->strmax);
		modebuf[0] = 0;
		PIL_mode2string(pp->mode, modebuf);
	        printf("%-16.16s %s %s %20s %s %s\n", pp->strname, PIL_type2string(pp->type),
				modebuf, pp->strvalue, minmaxstr, pp->strprompt);

		/* Test function which gets any parameter as a string, no matter
		   what its type. */
		if (PIL_OK != (r = PILGetAsString(pp->strname, buf)))
		 { test_failed = 1;
                   fprintf(stderr, "PILGetAsString failed with status %d for parameter %s\n", r, pp->strname);
                 }

		modebuf[0] = 0;
		PIL_mode2string(pp->mode, modebuf);
	        printf("%-16.16s %s %s %20s %s %s\n", pp->strname, PIL_type2string(pp->type),
				modebuf, buf, minmaxstr, pp->strprompt);
		break;
    }
 }

  /* Test for SCREW 1496: Set reprompt mode. */
  if (PIL_OK != (r = PILSetReprompt("reprompt", 1)))
   { test_failed = 1;
     fprintf(stderr, "Unexpected: First PILSetReprompt returned status %d\n", r);
   }

  /* Test for SCREW 1496: Prompt for one parameter again, which should be seen even if supplied on cmd line. */
  printf("Next you should see a prompt for reprompt, even if reprompt was supplied on the cmd line.\n");
  if (PIL_OK != (r = PILGetBool("reprompt", &value)))
   { test_failed = 1;
     fprintf(stderr, "Unexpected: PILGetBool(\"reprompt\", ...) returned status %d following PILSetReprompt\n", r);
   }

  /* Test for SCREW 1496: Prompting again should not cause the reprompt. */
  printf("Next you should not see any more prompts for reprompt\n");
  if (PIL_OK != (r = PILGetBool("reprompt", &value)))
   { test_failed = 1;
     fprintf(stderr, "Unexpected: PILGetBool(\"reprompt\", ...) returned status %d following first PILGetBool call\n", r);
   }

  /* Disable prompts from now on. */
  PILOverrideQueryMode(PIL_QUERY_OVERRIDE);

  /* Test for SCREW 1496: Set reprompt mode. */
  if (PIL_OK != (r = PILSetReprompt("reprompt", 1)))
   { test_failed = 1;
     fprintf(stderr, "Unexpected: Second PILSetReprompt returned status %d\n", r);
   }

  /* Test for SCREW 1496: OverrideQueryMode should trump SetReprompt. */
  if (PIL_OK != (r = PILGetBool("reprompt", &value)))
   { test_failed = 1;
     fprintf(stderr, "Unexpected: PILGetBool(\"reprompt\", ...) returned status %d following PILOverrideQueryMode\n", r);
   }

  /* Test for SCREW 1498: Verify that non-existent files fail PIL's default access check, but pass
     using the custom checker file_check (see above.) */
  /* Using the default file access checker with PIL should produce an error if one gets
     a file name for a file which doesn't exist. */
  if (PIL_OK == PILGetFname("infile2", buf))
   { test_failed = 1;
     fprintf(stderr, "Unexpected: PILGetFname succeeded for non-existent file when using PIL's default file checker.\n");
   }

  /* Override default file access checker. The one supplied claims any file name exists,
  even if it doesn't. */
  PILSetFileAccessFunction(&file_check);
  /* Now getting the same parameter should succeed. */
  if (PIL_OK != (r = PILGetFname("infile2", buf)))
   { test_failed = 1;
     fprintf(stderr, "Unexpected: PILGetFname returned status %d for non-existent file when using custom file checker.\n", r);
   }

  /* Reset the access checker just to be thorough. */
  PILSetFileAccessFunction(0);

  /* Test for pset mode correct function of PILSetFname. */
  save_mode = PILSpecialMode;
  PILSpecialMode |= PIL_PSET_MODE;
  if (PIL_OK != (r = PILPutFname("infile2", "another-non-existent-file")))
   { test_failed = 1;
     fprintf(stderr, "Unexpected: PILSetFname with PIL_PSET_MODE set returned status %d for non-existent file.\n", r);
   }
  if (PIL_OK != (r = PILGetFname("infile2", buf)))
   { test_failed = 1;
     fprintf(stderr, "Unexpected: PILGetFname infile2 with PIL_PSET_MODE set returned status %d.\n", r);
   }
  if (0 != strcmp(buf, "another-non-existent-file"))
   { test_failed = 1;
     fprintf(stderr, "Unexpected: PILGetFname infile2 got value %s, not another-non-existent-file.\n", buf);
   }
  PILSpecialMode = save_mode;

  if (PIL_OK != (r = PILPutFname("infile2", "pilcdemo.par+12345")))
   { test_failed = 1;
     fprintf(stderr, "Unexpected: PILPutFname failed for parameter file with fits-like extension of 5 digits.\n");
   }

PILClose(test_failed);
return(test_failed);
}
