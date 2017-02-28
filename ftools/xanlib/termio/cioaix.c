/* This set of routines handles the basic terminal IO on an IBM AIX
 * system.  These routines are all designed to be called from
 * Fortran.  Thus TTINIT can be called using the Fortran line
 *      CALL TTINIT()
 */

#include <termios.h>
#include <stdio.h>

static struct termios term, saveterm;

ttinit()

/* Turn off echo, and canonical processing.  This means that most
 * things that the user types is passed into the calling program.
 * Uses POSIX terminal control.
 */
{
   if ( isatty(0) ) {
      int ier;

      ier = tcgetattr(0, &term);
      saveterm = term;

      if (ier==0) {
         term.c_iflag &= ~( ICRNL );
         term.c_lflag &= ~( ICANON );
         term.c_lflag &= ~( ECHO | ECHOK | ECHOE | ECHONL );
         term.c_cc[VMIN] = 1;
         tcsetattr(0, TCSADRAIN, &term);
      }
   }
   return;
}

ttrset()

/* Reset the terminal flags to state when CSTTY was last called.
 */
{
   if ( isatty(0) ) {
      tcsetattr(0, TCSADRAIN, &saveterm);
   }
   return;
}

rdchr(chr_ptr, chr_len)
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

cwrite(chr_ptr, chr_len)
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

cpgsze(irow_ptr, icol_ptr)
int  *irow_ptr;
int  *icol_ptr;

/* Return the size of the current window.
 *
 * irow_ptr  Return  The number of rows in current window
 * icol_ptr  Return  The number of columns in current window
 */

{
#include <sys/ioctl.h>
   struct winsize actsize;

   ioctl(0, TIOCGWINSZ, &actsize);
   *irow_ptr=actsize.ws_row;
   *icol_ptr=actsize.ws_col;
   return;
}

#include <sys/time.h>
#include <time.h>

cgtod(itime)
struct timeval *itime;

/* Returns the UNIX system time.
 *
 * itime     O  An array 2 elements long containing the timeval data
 */
{
   gettimeofday(itime, 0);
   return;
}

cgruse(iruse)
struct rusage *iruse;

/* Returns the UNIX system rusage structure.
 *
 * iruse     O  An array 18 elements long containing the rusage data
 */
{
   getrusage(0, iruse);
   return;
}

cloctim(itzone)
struct tm *itzone;

/* Returns the localtime structure in a Fortran readable array.
 *
 * itzone    O  An array  9 elements long containing the localtime data
 */
{
   struct timeval tp;
   struct tm *localtime();

   gettimeofday( &tp, 0);
   *itzone= *localtime( &tp );
   return;
}

cputs(chr_ptr, chr_len)
char *chr_ptr;
int *chr_len;
{
  FILE *file;
  int i;
  char *ftoolsoutput;
  int stdout_opened = 0;
  int tty_opened = 0;

  ftoolsoutput = getenv("FTOOLSOUTPUT");

  /* if the environment FTOOLSOUTPUT is not defined then open /dev/tty
     if FTOOLSOUTPUT is defined and it is stdout then assign file to stdout
     else, use the value of FTOOLSOUTPUT as a file name */

  if (NULL != ftoolsoutput) {
    if (!strcmp("/dev/tty",ftoolsoutput))
      tty_opened++;
    if (strcmp("stdout",ftoolsoutput)) {
      file = fopen(ftoolsoutput,"a+");
    } else {
      file = stdout;
      stdout_opened++;
    }
  } else {
    file = stdout;
    stdout_opened++;
  }
  if (NULL != file) {
    for (i=0;i<*chr_len;i++) {
      fputc(chr_ptr[i],file);
    }
    if (tty_opened) {
      fputc('\015',file);
      fputc('\012',file);
    } else {
      fputc('\n',file);
    }
    if (!stdout_opened) 
      fclose (file);
  }
}
