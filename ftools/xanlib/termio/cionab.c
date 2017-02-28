/* This set of routines handles the basic terminal IO on a NeXT
 * system.  These routines are all designed to be called from
 * Fortran.  Thus TTINIT can be called using the Fortran line
 *      CALL TTINIT()
 */

#include <sys/ioctl.h>
#include <stdio.h>

static struct sgttyb term, saveterm;

ttinit()

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

ttrset()

/* Reset the terminal flags to state when CSTTY was last called.
 */
{
   if ( isatty(0) ) {
      ioctl(0,TIOCSETP,&saveterm);
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
   struct winsize actsize;

   ioctl(0, TIOCGWINSZ, &actsize);
   *irow_ptr=actsize.ws_row;
   *icol_ptr=actsize.ws_col;
   return;
}

void cgtenv(chr_ptr, cbuf_ptr, chr_len, cbuf_len)
char *chr_ptr, *cbuf_ptr;
int   *chr_len,  *cbuf_len;

/* Return the value of an environment variable.
 *
 * chr_ptr   Input   The environment variable
 * cbuf_ptr  Return  The translation (blanked filled)
 * chr_len   Input   The Fortran size of chr
 * cbuf_len  Input   The Fortran size of cbuf
 */
{
   int i;
   char *getenv (), *itmp, *iloc;

   itmp= cbuf_ptr;
   iloc= getenv(chr_ptr);
   if (iloc == 0)
     {
      *itmp= ' ';
      *cbuf_len = 0;
     }
   else
     {
      strcpy(cbuf_ptr,iloc);
      i = *cbuf_len = strlen(iloc);
      *(cbuf_ptr+i) = ' ';
     }
}

cargs(cline, cline_len)
char *cline;         /* pointer to fortran character array  */
int   cline_len;     /* Fortran size of character array     */

/* Reads the command line arguments into an array that a
 * FORTRAN program can use.
 */

{
   extern int NXArgc;         /* argc saved in crt0          */
   extern char **NXArgv;      /* argv saved in crt0          */
   int i,j;
   char *c, *iloc;

   c     = *NXArgv;           /* get pointer to start of args */
   iloc  = cline;

   i=1;
   for (j=1; j<=cline_len; j++) {
      if (c==0)
         *iloc++ = ' ';
      else if (*c==0) {
         *iloc++ = ' ';
         c++;
         if (i>=NXArgc)
            c=0;
         else
            i++;
      }
      else
         *iloc++ = *c++;
   }
   return;
}

#include <sys/time.h>

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
 * itzone    O  An array 11 elements long containing the localtime data
 */
{
   struct timeval tp;
   struct tm *localtime();

   gettimeofday( &tp, 0);
   *itzone= *localtime(&tp.tv_sec);
   return;
}

cputs_(chr_ptr, chr_len)
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