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
  /* This routine does jack all at the moment, because I can't figure out
     how to do it in IRAF - LB */

  return;
}

