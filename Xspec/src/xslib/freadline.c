/* freadline, fortran interface to gnu readline */

/* $Id: freadline.c,v 3.15 2013/05/30 21:37:28 kaa Exp $

   $Log: freadline.c,v $
   Revision 3.15  2013/05/30 21:37:28  kaa
   Tidying up compiler warnings. Mainly variable declarations but also a
   couple of cases of arithmetic GOTOs.

   Revision 3.14  2006/05/10 16:39:55  irby
   Move definition of xpitaskname to freadline.c and declare as extern in
   opdefpf.c (the opposite of the current scenario) so that we can use
   freadline.c in the Xspec library xslib without also adding opdefpf.c.

   Revision 3.13  2006/05/09 19:55:04  irby
   Apply strlen checking done in Xspec version of freadline.c so we can
   replace it with this one.  For unknown reasons (perhaps soon to find out?),
   the Xspec version also had the xpi_rl_help function commented out of the
   code entirely.  I'm leaving it in here for now, and hopefully Xspec won't
   choke on it.  Also changed if (!use_readline) free(tmp); to just free(tmp);

   Revision 3.12  2002/07/24 15:28:20  miket
   fixing bug introduced by rev 3.10 (fgets keeps terminal newline; gets does not)

   Revision 3.11  1999/11/02 21:57:57  elwin
   Fixing up the xpi_rl_help prototype some more.

   Revision 3.10  1999/11/01 16:58:12  dorman
   Modified call to gets(char*) to fgets(char*, size_t, FILE). This is
   to remove gcc compiler warnings about unsafe functions.

   Also, freadline and xpi_rl_help are now protoized and correctly typed (void).

   Revision 3.9  1997/05/16 20:49:08  oneel
   Fixed loop problem with eof

   Revision 3.8  1997/03/12 19:52:17  oneel
   fix a bug with strcatting a " " to the end of a possibly readonly string

   Revision 3.7  1997/01/17 21:09:39  oneel
   Add in ? help

 * Revision 3.6  1997/01/17  18:13:18  oneel
 * Let the user turn readline on and off
 *
 * Revision 3.5.1.1  1996/04/16  01:38:59  dunfee
 * Start of pristine ftools CVS...
 *
 * Revision 1.3  1995/12/06  19:59:45  oneel
 * finished up Mike's HP/SUX changes
 *
 * Revision 1.2  1995/12/06  19:58:57  oneel
 * Mike's HP/SUX changes
 *

   */


#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#if defined(vms)
#define freadline freadline_
#include <types.h>
#include <stat.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif



#include "cfortran.h"

char* readline(char* );
void add_history(char* );

char xpitaskname[1000];
void xpi_rl_help(int,int);

void 
freadline(char* cprom,int lprom,char* cbuf,int* lbuf,int* ios)
{
  char *tmp;
  char *prompt;
  int MAXCBUF = 1000;

  int use_readline = 1;

  if (getenv("XPINOREADLINE")) use_readline = 0;
#if defined(vms)
  use_readline = 0;
#endif

  *ios = 0;


  if (use_readline) 
    {
#if defined(vms)
      fprintf (stderr,"This should not happen, vms and use_readline!\n");
#else
      /*      rl_bind_key ('?',xpi_rl_help); */
      prompt = malloc(strlen(cprom)+5); /* yes, I know, +5 is sloppy */
      strcpy(prompt,cprom);
      strcat(prompt," ");
      tmp = readline(prompt);
      free(prompt);
      if (tmp == NULL) {
	*ios = 1;
	return;
      }
      /* rl_unbind_key('?'); */
#endif
    } 
  else 
    {
      static const int BUFSIZE = 1025;
      tmp = (char*)malloc(BUFSIZE*sizeof(char));
      if (!tmp) 
	{
	  *ios = 1;
	  return;
	}
      printf ("%s ",cprom);
      tmp = fgets(tmp,BUFSIZE,stdin);
      /* replace terminal newline with NULL */
      if (strchr(tmp,012) && (strcspn(tmp,"\n") == strlen(tmp)-1)) tmp[strlen(tmp)-1]='\0';
    }
  
  if (!tmp) 
    {
      *ios = 1;
      return;
    }
  if (strlen(tmp) >= MAXCBUF)
    {
     fprintf(stderr, "***Error:  Input is limited to %d chars\n", MAXCBUF);
     free(tmp);
     *ios = -1;
     return;
    }
  add_history(tmp);
  
  strcpy(cbuf,tmp);
  *lbuf = strlen(cbuf);

  free(tmp);
}



      
FCALLSCSUB5 (freadline,FREADLINE,freadline,STRING,INT,PSTRING,PINT,PINT)

void
xpi_rl_help (int count, int key)
{
  /* give help */
  char tmpstr[1000];
  strcpy (tmpstr,"fhelp ");
  strcat (tmpstr,xpitaskname);
  system (tmpstr);
}
