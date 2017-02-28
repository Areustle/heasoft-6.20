/* This set of routines handles the basic terminal IO on a NeXT
 * system.  These routines are all designed to be called from
 * Fortran.  Thus ttinit can be called using the Fortran line
 *      CALL TTINIT()
 */

#include <sys/ioctl.h>
#include <stdio.h>

static struct sgttyb term, saveterm;

ttinit_()

/* Sets the terminal flags to CBREAK and NOECHO mode.  This means that
 * most things the user types is passed back to the calling program.
 */
{
   if ( isatty(0) ) {
      int ier;

      ier=ioctl(0,TIOCGETP,&term);
      saveterm = term;

      if (ier==0) {
         term.sg_flags |=CBREAK;
         term.sg_flags &=~ECHO;
         ioctl(0,TIOCSETP,&term);
      }
   }
   return;
}

ttrset_()

/* Reset the terminal flags to state when CSTTY was last called.
 */
{
   if ( isatty(0) ) {
      ioctl(0,TIOCSETP,&saveterm);
   }
   return;
}

rdchr_(chr_ptr, chr_len)
char *chr_ptr;
int   chr_len;

/* Reads a single byte from the current terminal.  The read waits
 * until the user types something.
 *
 * chr_ptr   In/Ret  The character read
 * chr_len   Input   The Fortran size of the character array
 */
{
   return (read(0,chr_ptr,1));
}

cwrite_(chr_ptr, chr_len)
char *chr_ptr;
int   chr_len;

/* Write a single character to the terminal.
 *
 * chr_ptr   Input   The character to be written
 * chr_len   Input   The Fortran size of the character array
 */
{
   return (write(0,chr_ptr,chr_len));
}

cpgsze_(irow_ptr, icol_ptr)
int  *irow_ptr;
int  *icol_ptr;

/* Return the size of the current window.
 *
 * irow_ptr  Return  The number of rows in current window
 * icol_ptr  Return  The number of columns in current window
 */

{
   struct winsize actsize;

   ioctl(0, TIOCGWINSZ, &actsize);
   *irow_ptr=actsize.ws_row;
   *icol_ptr=actsize.ws_col;
   return;
}

#include <sys/time.h>

cgtod_(itime)
struct timeval *itime;

/* Returns the UNIX system time.
 *
 * itime     O  An array 2 elements long containing the timeval data
 */
{
   gettimeofday(itime, 0);
   return;
}

cgruse_(iruse)
struct rusage *iruse;

/* Returns the UNIX system rusage structure.
 *
 * iruse     O  An array 18 elements long containing the rusage data
 */
{
   getrusage(0, iruse);
   return;
}
