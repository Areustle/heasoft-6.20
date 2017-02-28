/* List a par file */


#include <string.h>

#include "cfortran.h"

#ifdef VMS
#define tbfdpriraf tbfdpriraf_
#endif

#define TBLDPR(file,istat) \
  CCALLSFSUB2(TBLDPR,tbldpr,STRING,PINT,file,istat)

#define IRAFPARGETNPARS(i) \
  CCALLSFSUB1(IRAFPARGETNPARS,irafpargetnpars,PINT,i)

#define IRAFPARGETPFNAME(i) \
  CCALLSFSUB1(IRAFPARGETPFNAME,irafpargetpfname,PSTRING,i)

#define IRAFPARGETLINE(i,name,desc,type,minp,maxp,defl,mode) \
  CCALLSFSUB8(IRAFPARGETLINE,irafpargetline,INT,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,i,name,desc,type,minp,maxp,defl,mode)

#include <stdio.h>
#include <string.h>

extern int xargc;     /* to avoid loading main.o from libf2c.a */
extern char **xargv;  /* to avoid loading main.o from libf2c.a */

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
     char *argv[];
     int argc;
{
  
  int i,j;
  int npars;
  int ierr;
    
  char pfname[1000];
  char name[1000],desc[1000],type[1000],minp[1000],maxp[1000],defl[1000];
  char mode[1000], filename[1000];

  xargc=argc;
  xargv=argv;

  if (argc < 2)
    {
      fprintf (stderr,"No parameter files to list\n");
      exit(1);
    }
  for (j=1;j<argc;j++)
    {
      strcpy(pfname,argv[j]);
      if (NULL == strstr(pfname,".par"))
	{
	  strcat(pfname,".par");
	}
#ifdef DEBUG
      fprintf(stderr,"Loading %s\n",pfname);
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
      
      IRAFPARGETPFNAME(name);
      
      printf ("Parameters for %s\n",name);
      
      IRAFPARGETNPARS(npars);
#ifdef DEBUG
      fprintf (stderr,"Num pars %d\n",npars);
      fflush (stderr);
#endif
      for (i=1;i<=npars;i++)
	{
	  IRAFPARGETLINE(i,name,desc,type,minp,maxp,defl,mode);
	  if (strchr(mode,'h'))
	    {
	      strcpy(minp,name);
	      strcpy(name,"(");
	      strcat(name,minp);
	      strcat(defl,")");
	      
	    }
	  printf ("%13s = %-16s %s\n",name,defl,desc);
	  
	}
      printf ("\n");
    }
  exit (0);

}
