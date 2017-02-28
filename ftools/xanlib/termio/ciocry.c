/* This set of routines handles the basic terminal IO on an UNICOS Cray
 * system.  These routines are all designed to be called from
 * Fortran.  Thus cstty can be called using the Fortran line
 *      CALL CSTTY()
 */

#include <termios.h>
#include <stdio.h>

static struct termios term, saveterm;

CSTTY()

/* Turn off echo, canonical, and signal processing.  This means that
 * everything the user types is passed back to the calling program.
 * Uses POSIX terminal control.
 */
{
   if ( isatty(0) ) {
      int ier;

      ier = tcgetattr(0, &term);
      saveterm = term;

      if (ier==0) {
         term.c_iflag &= ~( ICRNL );
         term.c_lflag &= ~( ICANON | ISIG );
         term.c_lflag &= ~( ECHO | ECHOK | ECHOE | ECHONL );
         term.c_cc[VMIN] = 1;
         tcsetattr(0, TCSADRAIN, &term);
      }
   }
   return;
}

CRTTY()

/* Reset the terminal flags to state when CSTTY was last called.
 */
{
   if ( isatty(0) ) {
      tcsetattr(0, TCSADRAIN, &saveterm);
   }
   return;
}

RDCHR(chr_ptr, chr_len)
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

CWRITE(chr_ptr, lchr, chr_len)
char *chr_ptr;
int  *lchr;
int   chr_len;

/* Write a single character to the terminal.  For some strange reason
 * if lchr>50 then the Cray doesn't output anything.  For this reason
 * characters are written out one at a time.  Note, it would be more
 * efficient if characters were written in blocks of 50.
 *
 * chr_ptr   Input   The character to be written
 * lchr      Input   The number of valid characters in the CHR array
 * chr_len   Input   The Fortran size of the character array
 */
{
   int i;

   for( i=0; i < *lchr; i++) write(0,&chr_ptr[i],1);
   return;
}

CWINSIZ(irow_ptr, icol_ptr)
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

SYSTEM(cfile)
char *cfile;
{
   system(cfile);
   return;
}

CPUTS(chr_ptr, chr_len)
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
