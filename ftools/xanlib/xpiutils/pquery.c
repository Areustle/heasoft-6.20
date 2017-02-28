/* set parameters in a par file */
/* $Id: pquery.c,v 3.14 2002/04/30 13:20:00 miket Exp $ */
/* $Log: pquery.c,v $
/* Revision 3.14  2002/04/30 13:20:00  miket
/* forcing pquery to use /dev/tty for prompting (FTOOLSOUTPUT not used here)
/*
/* Revision 3.13  2002/01/30 23:35:08  irby
/* Declare xargc & xargv as extern - already declared in opdefpf.c.
/*
/* Revision 3.12  2001/10/15 19:23:51  zpan
/*
/* fix uninitialized variable's warning
/*
/* Revision 3.11  2000/05/16 16:12:17  miket
/* Changing behavior of pquery2 only to match documented behavior on hidden params
/*
/* Revision 3.10  1997/07/10 18:18:32  oneel
/* Fix the bug reported by Uwe in the ftools 4.0 release of pquery.  This
/* fixes the case where
/*
/* pquery fdump skip
/*
/* returned nothing.  It should return the value of skip (a hidden
/* parameter) from the fdump par file.
/*
 * Revision 3.9  1997/03/03 22:11:14  oneel
 * Added - as a parameter name to query for.  If so, pquery goes into a
 * loop reading from stdin a parameter name, prompting the user (on
 * /dev/tty) if necessary, and writing to stdout the value of the
 * parameter.  Note that we don't use readline in this mode.
 *
 * Revision 3.8  1997/03/03  16:56:32  oneel
 * Added readline to pquery
 *
 * Revision 3.7  1996/07/18  15:43:45  miket
 * MJT 18July96 (g77/linux) double-underscores now handled via g77 flag
 *
 * Revision 3.6  1996/07/11  15:53:21  miket
 * MJT 11July96 g77/linux-related changes
 *
 * Revision 3.5.1.1  1996/04/16 01:39:13  dunfee
 * Start of pristine ftools CVS...
 *
 * Revision 1.5  1995/12/06  20:31:10  oneel
 * pquery now does value=+ and value=- parameters
 *
 * Revision 1.4  1995/12/06  16:17:57  oneel
 * Got Mike/Uwe's changes
 *
 *
 *	CHANGE HISTORY:
 *
 * Revision 1.3  1995/09/21  19:42:15  oneel
 * exit 0 at end
 *
 * Revision 1.2  1995/04/17  17:30:28  oneel
 * Pquery now prompts for hidden parameters if they are on the command
 * line.
 *
 * UL, 17/11/95: 1.3: - added feature: hidden-mode parameters are only
 *			"learned" when program was invoked as 'pquery'
 *			(nominal behavior in FTOOLS >= 3.4), otherwise,
 *                      hidden-mode parameters always remain unchanged
 *                      (nominal pre-3.4 behavior);
 *
 *
 * MJT 16May2000: when called as pquery2 *no* parameters were being updated
 *                at all. Modified code to work as documented, ie, pquery2
 *                won't update hidden parameters but will update learned ones.
 *                No change at all to the default, pquery, which updates all
 *                parameters regardless of whether they're hidden. Also added
 *                some more info to the DEBUG messages.
 */

#include <string.h>

#include "cfortran.h"
#ifdef VMS
#define tbfdpriraf tbfdpriraf_
#endif

#define PQUERY_NAME		"pquery"

#define TBLDPR(file,istat) \
  CCALLSFSUB2(TBLDPR,tbldpr,STRING,PINT,file,istat)

#define IRAFPARGETNPARS(i) \
  CCALLSFSUB1(IRAFPARGETNPARS,irafpargetnpars,PINT,i)

#define IRAFPARGETPFNAME(i) \
  CCALLSFSUB1(IRAFPARGETPFNAME,irafpargetpfname,PSTRING,i)

#define APPLY_MODE \
  CCALLSFSUB0(APPLY_MODE,apply_mode)

#define DUMPPAR \
  CCALLSFSUB0(DUMPPAR,dumppar)

#define IRAFPARGETLINE(i,name,desc,type,minp,maxp,defl,mode) \
  CCALLSFSUB8(IRAFPARGETLINE,irafpargetline,INT,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,i,name,desc,type,minp,maxp,defl,mode)

#define IRAFPARPUTLINE(i,defl) \
  CCALLSFSUB2(IRAFPARPUTLINE,irafparputline,INT,STRING,i,defl)

#include <stdio.h>
#include <string.h>

#include "readline/readline.h"

extern int xargc;     /* to avoid loading main.o from libf2c.a */
extern char **xargv;  /* to avoid loading main.o from libf2c.a */

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
     char *argv[];
     int argc;
     

{

  int i,j, k;
  int npars;
  int ierr;
  int found;
  int notdone = 1;
  int dohidden = 0;
  int updatehidden;
  char pfname[1000];
  char name[1000],desc[1000],type[1000],minp[1000],maxp[1000],defl[1000];
  char mode[1000], filename[1000], tmpstr[1000];
  char *parname, *deflnew;
  char saveparname[1000], savedefl[1000];
  char prompt[1000];
  int lprompt, ldefl, ios;
  int interactive = 0;
  char inpar[1000];
  FILE *TTY=0;
  
  xargv=argv;
  xargc=argc;

  /* 
   * We want all prompts to go to terminal since pquery/pquery2
   * is typically called via backticks from Perl scripts
   */
  rl_outstream = fopen("/dev/tty","w");

  if (argc < 3)
    {
      fprintf (stderr,"No parameter file and/or parameter specified\n");
      exit(1);
    }

  /*
   *	UL: check whether program name is 'pquery'
   */
  updatehidden = strcmp(argv[0], PQUERY_NAME) ? 0 : 1;


  /* do we want special hidden processing */

  if (0 == strcmp("-h",argv[1]))
    {
      dohidden++;
    }

  /* save the parameter file name */

  strcpy(pfname,argv[1+dohidden]);
  if (NULL == strstr(pfname,".par"))
    {
      strcat(pfname,".par");
    }

  /* are we in interactive mode */

  strcpy(saveparname,argv[2+dohidden]);
  if (!strcmp(saveparname,"-")) interactive++;

  /* load the parameter file */

#ifdef DEBUG
  fprintf(stderr,"Loading %s\n",pfname);
  fflush(stderr);
  fprintf(stderr,"updatehidden is %d\n",updatehidden);
  fflush(stderr);
#endif
#ifdef vms
  if (strstr(pfname,"]") || strstr(pfname,":"))
#else
  if (strstr(pfname,"/"))
#endif
    {
      TBLDPR(pfname,ierr);
    }
  else
    {
      ierr = 1;
    }
  if (ierr != 0)
    {
#ifdef vms
	  if (!strstr(pfname,".PAR") && !strstr(pfname,".par"))
#else
	  if (!strstr(pfname,".par"))
#endif

	{
	  strcat(pfname,".par");
	}
      tbfdpriraf(pfname,filename,&ierr);
      TBLDPR(filename,ierr);
    }

  /* apply the mode parameter */
  APPLY_MODE;

  IRAFPARGETPFNAME(filename);
  
  IRAFPARGETNPARS(npars);
#ifdef DEBUG
  fprintf (stderr,"Num pars %d\n",npars);
  fflush (stderr);
#endif

  /* Open the terminal if we are going to be in interactive mode */

  if (interactive) {
    if ((TTY = fopen("/dev/tty","r+")) == NULL) {
      fprintf (stderr,"can't open /dev/tty\n");
      exit(1);
    }
  }

  /* we loop to here if we are in interactive mode */
top:

  /* get the parameter name to work with */
  if (interactive) {
    if (NULL == fgets(saveparname,1000,stdin)) {
      exit(0);
    }
    if (saveparname[strlen(saveparname)-1] == '\n')
      saveparname[strlen(saveparname)-1] = '\0';  
  }

  notdone = 1;
  if (argc > 3)
    {
      /* we have more parameter values on the command line */
#ifdef DEBUG
      puts ("argc > 3, ");
#endif
      for (i=3;i<argc-dohidden;i++)
	{
	  strcpy(tmpstr,argv[i+dohidden]);
	  /* parname = value */
	  if (strchr(tmpstr,'='))
	    {
	      parname = tmpstr;
	      deflnew = strchr(tmpstr,'=');
	      *deflnew = '\0';
	      deflnew++;
	      if (0 == strcmp(deflnew,"+"))
		{
		  strcpy (deflnew,"yes");
		}
	      if (0 == strcmp(deflnew,"-"))
		{
		  strcpy (deflnew,"no");

		}
	      /* we have a valid name = value, find and save that value */

#ifdef DEBUG
	      fprintf (stderr,"changing %s to %s\n",parname,deflnew);
	      fflush (stderr);
#endif
	      found = 0;
	      for (k=1;k<=npars;k++)
		{
		  IRAFPARGETLINE(k,name,desc,type,minp,maxp,defl,mode);
#ifdef DEBUG
		  fprintf(stderr,"name is %s, mode is %s\n",name,mode);
		  fflush(stderr);
#endif		  

		  if (!strcmp(name,parname))
		    {
		      found = 1;
		      /* update the parameter, if necessary */
		      /* logically, this is wrong.  Modify at your
			 own risk though, people depend on wrong logic :-) */
		      /* 16May00 (MJT) taking that risk to make behavior match docs */
		      if (!strchr(mode,'h') ||
			  (strchr(mode,'h') && updatehidden))
			IRAFPARPUTLINE(k,deflnew);
			  
#ifdef DEBUG
		      fprintf (stderr,"Saving %s\n",deflnew);
		      fflush (stderr);
#endif
		      if (!strcmp(name,saveparname))
			{
			  notdone = 0;
			  strcpy(savedefl,deflnew);
			}
		    }
		}
	      if (!found)
		{
		  fprintf (stderr,"Parameter %s not found\n",parname);
		  exit(1);
		}
	    }
	  else if (tmpstr[strlen(tmpstr)-1] == '!')
	    /* Ohh, this isn't documented.  It seems to force a query 
	       even if it's on the command line */
	    {
	      tmpstr[strlen(tmpstr)-1] = '\0';
	      found = 0;
	      for (k=1;k<=npars;k++)
		{
		  IRAFPARGETLINE(k,name,desc,type,minp,maxp,defl,mode);
#ifdef DEBUG
		  fprintf(stderr,"name is %s, mode is %s\n",name,mode);
		  fflush(stderr);
#endif		  
		  if (!strcmp(name,tmpstr))
		    {
		      /* prompt the user */
		      if (dohidden || !strchr(mode,'h'))
			{
			  found=1;
			  if (!strcmp(desc,""))
			    {
			      sprintf (prompt,"%s (%s):",name,defl);
			    }
			  else
			    {
			      sprintf (prompt,"%s (%s): ",desc,defl);
			    }
			  lprompt = strlen(prompt);
			  if (interactive) {
			    printf(prompt);
			    if (NULL == fgets(defl,1000,TTY)) exit(1);
			    if (defl[strlen(defl)-1] == '\n')
			      defl[strlen(defl)-1] = '\0';
			    ldefl = strlen(defl);
			  } else {
			    freadline(prompt,lprompt,defl,&ldefl,&ios);
			    if (ios) exit (1);
			  }
#ifdef DEBUG
			  printf ("defl is now %s\n",defl);
#endif
			}
		      
		      if (ldefl)
			{
			  if (defl[strlen(defl)-1] == '\n')
			    defl[strlen(defl)-1] = '\0';
			  /* update the parameter, if necessary */
			  /* this is also wrong logically, see above */
			  /* 16May00 (MJT) taking that risk to make behavior match docs */
			  if (!strchr(mode,'h') ||
			      (strchr(mode,'h') && updatehidden))
			      IRAFPARPUTLINE(k,defl);
			  
			  if (!strcmp(name,saveparname))
			    {
			      notdone = 0;
			      strcpy(savedefl,defl);
			    }
			}
		    }
		}
	      if (!found)
		{
		  fprintf (stderr,"Parameter %s not found\n",saveparname);
		  exit(1);
		}
	    }
	  else if ((i+3 <= argc-dohidden) && 
		   (!strcmp(argv[i+1+dohidden],"=")))
	    {
	      /* now deal with the parametners of the form par+ and par- */
	      parname = argv[i+dohidden];
	      deflnew = argv[i+2+dohidden];
	      if (0 == strcmp(deflnew,"+"))
		{
		  strcpy (deflnew,"yes");
		}
	      if (0 == strcmp(deflnew,"-"))
		{
		  strcpy (deflnew,"no");
		}
#ifdef DEBUG
	      fprintf (stderr,"changing %s to %s\n",parname,deflnew);
	      fflush (stderr);
#endif
	      found = 0;	      
	      for (k=1;k<=npars;k++)
		{
		  IRAFPARGETLINE(k,name,desc,type,minp,maxp,defl,mode);
#ifdef DEBUG
		  fprintf(stderr,"name is %s, mode is %s\n",name,mode);
		  fflush(stderr);
#endif		  
		  if (!strcmp(name,parname))
		    {
		      found = 1;
		      /* update the parameter, if necessary */
		      /* this is also wrong, see above */
		      /* 16May00 (MJT) taking that risk to make behavior match docs */
		      if (!strchr(mode,'h') ||
			  (strchr(mode,'h') && updatehidden))
			  IRAFPARPUTLINE(k,deflnew);

		      if (!strcmp(name,saveparname))
			{
			  notdone = 0;
			  strcpy(savedefl,deflnew);
			}
		    }
		  if (!found)
		    {
		      fprintf (stderr,"Parameter %s not found\n",parname);
		      exit(1);
		    }

		}
	      i+=2;
	    }
	  else
	    /* default to catch + and -?  I'm not sure this is ever run */
	    {
	      
#ifdef DEBUG
	      puts ("Hmm, we actually reached here ");
	      fprintf (stderr,"changing parameter number %d to %s\n",
		       i-2,tmpstr);
	      fflush(stderr);
#endif
	      if (0 == strcmp(tmpstr,"+"))
		{
		  strcpy (tmpstr,"yes");
		}
	      if (0 == strcmp(tmpstr,"-"))
		{
		  strcpy (tmpstr,"no");
		}
	      IRAFPARGETLINE(i-2,name,desc,type,minp,maxp,defl,mode);
#ifdef DEBUG
		  fprintf(stderr,"name is %s, mode is %s\n",name,mode);
		  fflush(stderr);
#endif		  
	      if (strchr(mode,'l'))
                  {
                      IRAFPARPUTLINE(i-2,tmpstr);
                  }

	      if (!strcmp(name,saveparname))
		{
		  notdone = 0;
		  strcpy(savedefl,tmpstr);
		}
	    }
	}
    }
  if (notdone)
    {
      /* prompt for all */
#ifdef DEBUG
      puts ("final notdone ");
#endif
      for (i=1;i<=npars;i++)
	{
	  IRAFPARGETLINE(i,name,desc,type,minp,maxp,defl,mode);
#ifdef DEBUG
	  fprintf(stderr,"name is %s, mode is %s\n",name,mode);
	  fflush(stderr);
#endif		  
	    
	  if (!strcmp(name,saveparname))
	    {
	      
	      ldefl = strlen(defl);
	      strcpy(savedefl,defl);
	      if (dohidden || !strchr(mode,'h'))
		{
		  strcpy(savedefl,defl);
		  if (!strcmp(desc,""))
		    {
		      sprintf (prompt,"%s (%s):",name,defl);
		    }
		  else
		    {
		      sprintf (prompt,"%s (%s): ",desc,defl);
		    }
		  lprompt = strlen(prompt);
		  if (interactive) {
		    printf(prompt);
		    if (NULL == fgets(defl,1000,TTY)) exit(1);
		    if (defl[strlen(defl)-1] == '\n')
		      defl[strlen(defl)-1] = '\0';
		    ldefl = strlen(defl);
		  } else {
		    freadline(prompt,lprompt,defl,&ldefl,&ios);
		    if (ios) exit (1);
		  }
#ifdef DEBUG
		  printf ("defl is now %s\n",defl);
#endif

		}
	      if (ldefl)
		{
		  if (defl[strlen(defl)-1] == '\n')
		    defl[strlen(defl)-1] = '\0';
/* Debate this JI/KB	  if (strchr(mode,'l')) */
		  /* 16May00 (MJT) taking that risk to make behavior match docs */
		  if (!strchr(mode,'h') ||
		      (strchr(mode,'h') && updatehidden))
		      IRAFPARPUTLINE(i,defl);
		  strcpy(savedefl,defl);
		}
	    }
	}
    }
  printf("%s\n",savedefl);
  if (interactive) goto top;
  CloseDefaultPF ();
  exit (0);
}

