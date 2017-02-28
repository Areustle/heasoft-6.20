/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ofaintdfe/oerror.c,v 3.8 2003/08/27 15:39:50 irby Exp $   */
/*                   */
/*
syserr
 print out message and system error and terminate process.
ffatal
 print out message and terminate process..


*/

#include <stdio.h>
#include <stdarg.h>

void cleanup()
{}

void syserr(msg)
     char *msg;
{

/* James Peachey, HEASARC/GSFC/NASA, Raytheon STX, 19 February, 1998
   Commented out the guts of this routine, because sys_errlist is
   declared differently in some implementations, causing builds to break.
   The function syserr is not called by anything, so there is no harm
   in eviscerating it, and nothing in Ftools should be writing directly
   to stderr anyway.
#ifndef vms
  extern int errno, sys_nerr;
  extern char *sys_errlist[];

  fprintf(stderr,"ERROR: %s (%d",msg,errno);
  if(errno>0 && errno < sys_nerr)
    fprintf(stderr,"; %s",sys_errlist[errno]);
  fprintf(stderr,")\n");
#endif
*/

/*clean up temp. files etc. if necessary */
  cleanup();

  exit(1);
}


void ffatal(const char* format, ...)
{
  va_list args;

/* initialize varargs */
  va_start(args, format);

  fprintf(stderr, "ERROR: ");
/* print onto the buffer string */
  vfprintf(stderr, format, args);
/* did it */
  va_end(args);

/*clean up temp. files etc. if necessary */
  cleanup();

  exit(1);
}
