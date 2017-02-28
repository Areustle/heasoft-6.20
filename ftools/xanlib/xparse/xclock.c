/*******************************************************************************

    This routine prints out a percentage string of two input long integer
    values with the specified step.

    AUTHOR: Song Yom (HSTX)
    DATE:   02/93

    MODIFICATION HISTORY:
    DATE    PROGRAMMER      DESCRIPTION
    ----    ----------      -----------

    93/04/23    Bruce O'Neel Get to compile on SunOS with a non-ansi compiler.
                             Add OUTDEVICE to write to the terminal rather
                             than stderr.

*******************************************************************************/

/*
    Include header file.
*/
#include <stdio.h>

#include <errno.h>

#ifdef unix
#if defined(sun) || defined(__hpux)
void xclock_(count,total,step)
long *count, *total, *step;
#else
void xclock_(int *count,int *total,int *step)
#endif
#define OUTDEV "/dev/tty"
#endif

#ifdef vms
void xclock(long int *count,long int *total,long int *step)
#endif
{
  static long int start = 0;
  static long int pvalue = 0;
  long int value;

  /*
    Make sure that step, total, and count arguments are non-zero positive
    integers.  Additionally, step must be in the range of 1 thru 100, and
    the count must be less than total.  If the arguments are valid, calculate
    the percentage, and print the percentage string to standard error in
    specified step.  Note that start variable (lower bound) is set to count
    for the initial call, and start and pvalue (previous percentage value) is
    reset at the final call.
    */
  if (*step <= 100 && *step > 0 && *count > 0 && *count <= *total)
    {
      if (!start) start = *count;
      value = (100 * (*count - start + 1)) / (*total - start + 1);
      if (value % *step == 0 && value != pvalue && *count != *total)
	{
	  
	  fprintf(stdout,"%3d%% completed\r",pvalue = value);
	  fflush(stdout);
	}
      else if (*count == *total)
        {	
          fprintf (stdout,"%3d%% completed\n",100);
	  fflush(stdout);
	  
	  start = pvalue = 0;
        }
    }
  
  return;
}

