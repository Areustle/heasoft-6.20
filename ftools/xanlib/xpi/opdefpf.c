/* XPI <=> IRAF interface routine */

/* 
   $Id: opdefpf.c,v 3.22 2015/06/11 20:02:35 kaa Exp $

   $Log: opdefpf.c,v $
   Revision 3.22  2015/06/11 20:02:35  kaa
   Added the history command to the set of standard gtcom2 commands. This toggles
   writing the history file on and off. The immediate use is in xselect to avoid
   problems when running multiple instances of the program.

   Note that adding a new command required putting it in lists in both ldcmds1.f
   and xpitbldcm.f - one of these ought to be redundant.

   Tidied up a bunch of compiler warnings.

   Revision 3.21  2013/08/28 15:19:28  irby
   Non-void functions should return a value.  These were flagged as
   compile errors under Mac OSX 10.9 (Mavericks).

   Revision 3.20  2011/07/13 20:42:47  irby
   Add calls to a) ape_trad_init, to supersede old XPI file input function(s)
   and b) ape_par_register_get_text, to register the new custom text-getter
   function xpi_get_text which, when registered, tells APE to call back to
   it to get input text.

   Revision 3.19  2007/04/05 19:18:24  irby
   Do not use f_setarg on Darwin with the gfortran compiler.

   Revision 3.18  2007/03/29 21:06:44  irby
   Do not use f_setarg on Darwin with the Intel compilers.

   Revision 3.17  2006/05/16 14:39:15  irby
   When using g95 on Darwin, call g95_runtime_start (instead of f_setarg)
   and g95_runtime_stop.

   Revision 3.15  2006/05/10 16:39:55  irby
   Move definition of xpitaskname to freadline.c and declare as extern in
   opdefpf.c (the opposite of the current scenario) so that we can use
   freadline.c in the Xspec library xslib without also adding opdefpf.c.

   Revision 3.14  2005/08/02 14:59:45  zpan
   Add "_",":" and ";" as special characters list so that the xpi treat parameter values that
   begin with the "_",":" and ";" character no different than the other special
   characters in the list.

   Revision 3.13  2002/01/30 23:21:09  irby
   Initialize argc/argv with a call to f_setarg under Darwin.

   Revision 3.12  2000/04/27 13:33:13  peachey
   Moved function call downstream of variable declaration, so that this
   file will actually compile.

   Revision 3.11  2000/04/25 22:29:00  ngan
   Defined the global status variable.

   Revision 3.10  1999/12/16 18:19:38  peachey
   Method lheasoft_version_info was changed to get_lheasoft_version_info.

   Revision 3.9  1999/12/07 22:45:12  peachey
   Added mechanism for giving version number specific to the release
   version of which a given tool is part. Of course, this only works
   for tools which call xpi.

   Revision 3.8  1997/01/17 21:09:40  oneel
   Add in ? help

 * Revision 3.7  1996/07/24  20:16:51  oneel
 * Fix for g77/linux, not initializing variables
 *
 * Revision 3.6  1996/07/11  15:53:16  miket
 * MJT 11July96 g77/linux-related changes
 *
   Revision 3.5.1.1  1996/04/16 01:39:07  dunfee
   Start of pristine ftools CVS...

 * Revision 1.2  1995/12/06  16:17:17  oneel
 * Got Mike's changes
 *

/* This also holds the cucl* routines to interface between C and fortran for
   the parameter interface */

#define LOGFILE "/ftools/.heasarc.log"
#define LOCKFILE "/ftools/lock/heasarc.lock"
#define TIMEOUT 10

#include <string.h>

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#ifdef sysV88
#include <sys/unistd.h>
#endif

#ifdef HEASARC_LOG
#include <fcntl.h>
#include <sys/types.h>
#endif

#include "cfortran.h"
#include "ape/ape_trad.h"
#include "ape/ape_par.h"

#define TBLDSTANDIRAF(file) \
  CCALLSFSUB1(TBLDSTANDIRAF,tbldstandiraf,STRING,file)

#define GTBUFSETCMDLINE(cmdline) \
  CCALLSFSUB1(GTBUFSETCMDLINE,gtbufsetcmdline,STRING,cmdline)

#define UCLGSB(parname,buffer,status) \
  CCALLSFSUB3(UCLGSB,uclgsb,STRING,PINT,PINT,parname,buffer,status)

#define UCLGSD(parname,buffer,status) \
  CCALLSFSUB3(UCLGSD,uclgsd,STRING,PDOUBLE,PINT,parname,buffer,status)

#define UCLGSI(parname,buffer,status) \
  CCALLSFSUB3(UCLGSI,uclgsi,STRING,PINT,PINT,parname,buffer,status)

#define UCLGSL(parname,buffer,status) \
  CCALLSFSUB3(UCLGSL,uclgsl,STRING,PLONG,PINT,parname,buffer,status)

#define UCLGSR(parname,buffer,status) \
  CCALLSFSUB3(UCLGSR,uclgsr,STRING,PFLOAT,PINT,parname,buffer,status)

#define UCLGSS(parname,buffer,status) \
  CCALLSFSUB3(UCLGSS,uclgss,STRING,PINT,PINT,parname,buffer,status)

#define UCLGST(P,B,S) \
  CCALLSFSUB3(UCLGST,uclgst,STRING,PSTRING,PINT,P,B,S)


#define UCLPSB(parname,buffer,status) \
  CCALLSFSUB3(UCLPSB,uclpsb,STRING,INT,PINT,parname,buffer,status)

#define UCLPSD(parname,buffer,status) \
  CCALLSFSUB3(UCLPSD,uclpsd,STRING,DOUBLE,PINT,parname,buffer,status)

#define UCLPSI(parname,buffer,status) \
  CCALLSFSUB3(UCLPSI,uclpsi,STRING,INT,PINT,parname,buffer,status)

#define UCLPSL(parname,buffer,status) \
  CCALLSFSUB3(UCLPSL,uclpsl,STRING,LONG,PINT,parname,buffer,status)

#define UCLPSR(parname,buffer,status) \
  CCALLSFSUB3(UCLPSR,uclpsr,STRING,FLOAT,PINT,parname,buffer,status)

#define UCLPSS(parname,buffer,status) \
  CCALLSFSUB3(UCLPSS,uclpss,STRING,INT,PINT,parname,buffer,status)

#define UCLPST(parname,buffer,status) \
  CCALLSFSUB3(UCLPST,uclpst,STRING,STRING,PINT,parname,buffer,status)

#define YINIT \
  CCALLSFSUB0(YINIT,yinit)



#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* Necessary declarations for release-specific version information */
#include "lheasoft-version-info.h"

/* Global status */
#include "hea_status.h"
int xargc;
char **xargv;

static int allalnum(str)
     char *str;
{
  int true = 1;
  int t1;
  
#ifdef DEBUG
  fprintf(stdout,"str is %s",str);
  fflush(stdout);
#endif
  
  while (*str)
    {
#ifdef DEBUG
      fprintf (stdout,":%c-",*str);
      fflush(stdout);
#endif
      t1 = isalnum(*str);
#ifdef DEBUG
      fprintf (stdout,"%d:",t1);
      fflush (stdout);
#endif
      true = t1 && true;
      str++;
    }
#ifdef DEBUG
  fprintf (stdout," and value is %d\n",true);
  fflush(stdout);
#endif
  return true;
}

static int specchar(str)
     char *str;
{
  int true = 0;
  int t1;
  char *strnxt;
  
  
#ifdef DEBUG
  fprintf(stdout,"str is %s",str);
  fflush(stdout);
#endif
  
  while (*str)
    {
#ifdef DEBUG
      fprintf (stdout,":%c-",*str);
      fflush(stdout);
#endif
      switch (*str) 
	{
	case '\'':
	case '!':
	case '@':
	case '#':
	case '$':
	case '%':
	case '^':
	case '&':
	case '*':
	case '(':
	case ')':
	case '\\':
	case '|':
	case '{':
	case '}':
	case '[':
	case ']':
	case '~':
	case '`':
	case '<':
	case '>':
	case '?':
	case ',':
	case '.':
	case ' ':
	case '_': 
	case ':': 
	case ';': 
	  t1 = 1;
	  break;
	case '=':
	  strnxt = str;
	  strnxt++;
	  if (*strnxt == '=')
	    {
	      t1 = 1;
	    }
	  else
	    {
	      t1 = 0;
	    }
	  break;
	default:
	  t1 = 0;
	  break;
	}
      
#ifdef DEBUG
      fprintf (stdout,"%d:",t1);
      fflush (stdout);
#endif
      true = t1 || true;
      str++;
    }
#ifdef DEBUG
  fprintf (stdout," and value is %d\n",true);
  fflush(stdout);
#endif
  return true;
}

extern char xpitaskname[1000];


int xpi_get_text(const char * prompt, const char * name, char ** text);

OpenDefaultPF (argc, argv)
int argc;
char **argv;
{
  char pfname1[1000];
  char pfname2[1000];
  char cmdline[1000];
  char tmp[1000];
  char *tmp1;
  int i;
  int j = 0;

/* message for universal release version information */
  char version_msg[LHEASOFT_BUFSIZE] = "";

/* APE: */
  ape_trad_init(argc,argv);
  ape_par_register_get_text(xpi_get_text);

/* Darwin */
#ifdef __APPLE__
#if defined(g95Fortran)
  g95_runtime_start(argc,argv);
#else
#if !defined(INTEL_COMPILER) && !defined(gFortran)
  f_setarg(argc,argv);
#endif
#endif
#endif

/* Reset the global status to 0 */
  ResetHEAStatus();

/* if tool was invoked with a -V, -v or --version, print version
 * information and exit */
  if(argc > 1 &&
     !(strcmp(argv[1], "-V") &&
       strcmp(argv[1], "-v") &&
       strcmp(argv[1], "--version"))
    ) {
    get_lheasoft_version_info(argv[0], version_msg);
    fprintf(stdout, "%s\n", version_msg);
    exit(0);
  }
  YINIT;

  xargc = argc;
  xargv = argv;

  strcpy(pfname1,argv[0]);
  strcpy(xpitaskname,argv[0]);
#ifdef DEBUG
  fprintf (stdout,"argv[0] is %s\n",argv[0]);
  fflush (stdout);
#endif
  
  cmdline[0] = '\0';
  
  for (i=1;i<argc;i++)
    {
#ifdef DEBUG
      fprintf (stdout,"argv[%d] is %s\n",i,argv[i]);
      fflush (stdout);
#endif
      strcat(cmdline," ");
/*       if (!allalnum(argv[i]) || (0 == strcmp("-",argv[i]))) */
/*      if (strchr(argv[i],' ') || (0 == strcmp("-",argv[i]))) */
       if (specchar(argv[i]) || (0 == strcmp("-",argv[i]))) 
	{
	  strcpy(tmp,argv[i]);
	  if (tmp[0] == '=')
	    {
	      tmp1 = tmp;
	      *tmp1++ = '\0';
	      strcat(cmdline," = ");
	      strcat(cmdline,"\"");
	      strcat(cmdline,tmp1);
	      strcat(cmdline,"\"");
	    }
	  else
	    {
	      strcat(cmdline,"\"");
	      strcat(cmdline,argv[i]);
	      strcat(cmdline,"\"");
	    }
	}
      else 
	{
	  strcat(cmdline,argv[i]);
	}
      
    }

#ifdef DEBUG
  fprintf (stdout,"Cmdline: %s\n",cmdline);
  fflush (stdout);
#endif

  xpidebuglog(pfname1,cmdline);
  

#ifndef vms
  GTBUFSETCMDLINE(cmdline);
#endif
/*  fprintf(stdout,"%s\n",pfname1); */
  for (i=strlen(pfname1)-1;i>0;i--)
    {
      if (pfname1[i] == '.') pfname1[i] = '\0';
      if (pfname1[i] == ']')
	{
	  j = i;
	  j++;
	  break;
	}
      if (pfname1[i] == '/')
	{
	  j = i;
	  j++;
	  break;
	}
    }
  i = 0;
  while (pfname1[j])
    {
      pfname2[i++] = pfname1[j++];
    }
  pfname2[i++] = '\0';
  if (!strstr(pfname2,".par"))
    {
      strcat(pfname2,".par");
    }
#ifdef DEBUG
  fprintf(stdout,"Call tbldstandiraf with %s\n",pfname2); 
  fflush(stdout); 
#endif
  TBLDSTANDIRAF(pfname2);
	
}



void cuclgsb(parname,buffer,status)
     char *parname;
     int *buffer;
     int *status;
     
{
  int tmpbuffer;
  int tmpstatus;
  
  tmpstatus = 0;
  tmpbuffer = *buffer;
  UCLGSB(parname,tmpbuffer,tmpstatus);
  *buffer = tmpbuffer;
  *status = tmpstatus;
}

void cuclgsd(parname,buffer,status)
     char *parname;
     double *buffer;
     int *status;
     
{
  double tmpbuffer;
  int tmpstatus;
  
  tmpstatus = 0;
  tmpbuffer = *buffer;
  UCLGSD(parname,tmpbuffer,tmpstatus);
  *buffer = tmpbuffer;
  *status = tmpstatus;
}

void cuclgsi(parname,buffer,status)
     char *parname;
     int *buffer;
     int *status;
     
{
  int tmpbuffer;
  int tmpstatus;
  
  tmpstatus = 0;
  tmpbuffer = *buffer;
  UCLGSI(parname,tmpbuffer,tmpstatus);
  *buffer = tmpbuffer;
  *status = tmpstatus;
}

void cuclgsl(parname,buffer,status)
     char *parname;
     long *buffer;
     int *status;
     
{
  long tmpbuffer;
  int tmpstatus;
  
  tmpstatus = 0;
  tmpbuffer = *buffer;
  UCLGSL(parname,tmpbuffer,tmpstatus);
  *buffer = tmpbuffer;
  *status = tmpstatus;
}

void cuclgsr(parname,buffer,status)
     char *parname;
     float *buffer;
     int *status;
     
{
  float tmpbuffer;
  int tmpstatus;
  
  tmpstatus = 0;
  tmpbuffer = *buffer;
  UCLGSR(parname,tmpbuffer,tmpstatus);
  *buffer = tmpbuffer;
  *status = tmpstatus;
}

void cuclgss(parname,buffer,status)
     char *parname;
     int *buffer;
     int *status;
     
{
  int tmpbuffer;
  int tmpstatus;
  
  tmpstatus = 0;
  tmpbuffer = *buffer;
  UCLGSS(parname,tmpbuffer,tmpstatus);
  *buffer = tmpbuffer;
  *status = tmpstatus;
}

void cuclgst(parname,buffer,status)
     char *parname;
     char *buffer;
     int *status;
     
{
  char tmp[255];
  char tparname[50];
  int tmpstatus;
  
  tmpstatus = 0;
  strcpy(tmp,"                                                  ");
  strcpy(tparname,parname);
  UCLGST(tparname,tmp,*status);
  strcpy(buffer,tmp);
  *status = tmpstatus;
  
}




void cuclpsb(parname,buffer,status)
     char *parname;
     int *buffer;
     int *status;
     
{
  int tmpstatus;
  int tmpbuffer;
  
  tmpbuffer = *buffer;
  tmpstatus = 0;
  UCLPSB(parname,tmpbuffer,tmpstatus);
  *status = tmpstatus;
}

void cuclpsd(parname,buffer,status)
     char *parname;
     double *buffer;
     int *status;
     
{
  int tmpstatus;
  double tmpbuffer;
  
  tmpstatus = 0;
  tmpbuffer = *buffer;
  UCLPSD(parname,tmpbuffer,tmpstatus);
  *status = tmpstatus;
}

void cuclpsi(parname,buffer,status)
     char *parname;
     int *buffer;
     int *status;
     
{
  int tmpbuffer;
  int tmpstatus;
  
  tmpstatus = 0;
  tmpbuffer = *buffer;
  UCLPSI(parname,tmpbuffer,tmpstatus);
  *status = tmpstatus;
  
}

void cuclpsl(parname,buffer,status)
     char *parname;
     long *buffer;
     int *status;
     
{
  int tmpstatus;
  long tmpbuffer;
  
  tmpstatus = 0;
  tmpbuffer = *buffer;
  UCLPSL(parname,tmpbuffer,tmpstatus);
  *status = tmpstatus;
}

void cuclpsr(parname,buffer,status)
     char *parname;
     float *buffer;
     int *status;
     
{
  float tmpbuffer;
  int tmpstatus;
  
  tmpbuffer = *buffer;
  tmpstatus = 0;
  
  UCLPSR(parname,tmpbuffer,tmpstatus);
  *status = tmpstatus;
  *buffer = tmpbuffer;
}

void cuclpss(parname,buffer,status)
     char *parname;
     int *buffer;
     int *status;
     
{
  int tmpstatus;
  int tmpbuffer;
  
  tmpstatus = 0;
  tmpbuffer = *buffer;
  UCLPSS(parname,tmpbuffer,tmpstatus);
  *status = tmpstatus;
}

void cuclpst(parname,buffer,status)
     char *parname;
     char *buffer;
     int *status;
     
{
  char tmp[255];
  int tmpstatus;
  
  strcpy(tmp,buffer);
  tmpstatus = 0;
  UCLPST(parname,tmp,tmpstatus);
  *status = tmpstatus;
}


int xpidebuglog(command,line)
     char *command;
     char *line;
     
     
{

#ifndef HEASARC_LOG
  return 0;
#else

  FILE *outfile;
  char host[1000];
  char userid[1000];
  time_t clock;
  char *cp, *ctime();

  int lock;
  int i;
  int value;


  
/* Open the lock file, spin if you can't create it */

  i = 0;

#ifdef DEBUG
  puts("Trying lockfile");
#endif
  while ((lock = open(LOCKFILE,O_EXCL | O_CREAT)) == -1)
    {
      i++;
#ifdef DEBUG
      printf("Waiting on lock file %d\n",i);
#endif
      if (i > TIMEOUT) 
	{
#ifdef DEBUG
	  puts("Exiting, can't get lockfile");
#endif
	  return;
	}
      sleep(1);
    }

#ifdef DEBUG
  puts("Got lockfile");
#endif


  
  
  if (NULL == (outfile = fopen(LOGFILE,"a")))
    {
      /* couldn't open file, so return */
      return;
    }
  gethostname(host,1000);
  time (&clock);
  cp = ctime(&clock);

  cp[strlen(cp)-1] = '\0';
  cuserid(userid);
  
  fprintf(outfile,"%s|%s|%s|%s|%s\n",
	  command,userid,host,cp,line);
  
  fclose(outfile);

  close(lock);
  unlink(LOCKFILE);

#endif
}

