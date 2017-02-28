/* This set of routines handles the basic terminal IO on an Ultrix
 * system.  These routines are all designed to be called from
 * Fortran.  Thus copen_(icon_ptr) can be called using the
 * Fortran line
 *      CALL COPEN(ICON)
 */

#include <sys/ioctl.h>
#include <sys/file.h>
#include <stdio.h>

struct sgttyb cioblk;
int  ichan;

copen_(icon_ptr)
int  *icon_ptr;

/* Open a channel to the current terminal.  This routine opens device
 * /dev/tty unless icon<>0, in which case /dev/console is opened.  This
 * is done to avoid problems with the screenblank program running on
 * the console.
 *
 * icon_ptr   Input   <>0 if on console device, =0 otherwise
 */
{
      if ( *icon_ptr == 0 )
         ichan =open("/dev/tty", O_RDWR);
      else
         ichan =open("/dev/console", O_RDWR);
      return;
}

cstty_(iflag_ptr)
int  *iflag_ptr;

/* Sets the terminal flags to CBREAK and NOECHO mode.  The original
 * set of flags is returned in iflag_ptr, to allow the original
 * state to be restored with a call to crtty.
 *
 * iflag_ptr Return  The original state of the terminal flags
 */
{
      int ier;

      ier=ioctl(ichan,TIOCGETP,&cioblk);
      if (ier==0) {
         *iflag_ptr=cioblk.sg_flags;
         cioblk.sg_flags |=CBREAK;
         cioblk.sg_flags &=~ECHO;
         ier=ioctl(ichan,TIOCSETP,&cioblk);
         }
      return (ier);
}

crtty_(iflag_ptr)
int  *iflag_ptr;

/* Reset the terminal flags to the state given by iflag_ptr
 *
 * iflag_ptr Input   The flag state to restore
 */
{
      cioblk.sg_flags= *iflag_ptr;
      return (ioctl(ichan,TIOCSETP,&cioblk));
}

cpgsze_(row, col)
int *row;
int *col;

/* Get the size of the current window
*/
{
      struct winsize window;
      int ier;

      ier = ioctl(ichan, TIOCGWINSZ, &window);
      if (ier == 0) {
         *row = window.ws_row;
         *col = window.ws_col;
       }
       return(ier);
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
