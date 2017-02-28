/* unlearn a par file */


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

#define TRLOG(cbuf,lbuf,cret,lret) \
  CCALLSFSUB4(TRLOG,trlog,STRING,INT,PSTRING,PINT,cbuf,lbuf,cret,lret)

#include <stdio.h>
#include <string.h>

extern int xargc;    /* to avoid loading main.o from libf2c.a */
extern char **xargv; /* to avoid loading main.o from libf2c.a */

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
     char *argv[];
     int argc;
{
  
  int j;
  int ierr;
  
    
  char pfname[1000], name[1000], pfclobber[1000];
  FILE *infile;
  
  char filename[1000];

  xargc=argc;
  xargv=argv;

  TRLOG("PFCLOBBER",9,pfclobber,j);
  if (!j)
      {
	  fprintf (stderr,"You don't have PFCLOBBER set, punlearn is unable to do anything\n");  
	  exit (1);
      }
  if (argc < 2)
    {
      fprintf (stderr,"No parameter files to unlearn\n");
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
	  if (!strstr(pfname,".PAR") && !strstr(pfname,".par"))
#else
	  if (!strstr(pfname,".par"))
#endif
	{
	  strcat(pfname,".par");
	}
      
      tbfdpriraf(pfname,filename,&ierr);
      TBLDPR(filename,ierr);
      
      IRAFPARGETPFNAME(name);

      /* Nuke the par file */
      if (NULL != (infile = fopen(name,"w")))
	{
	  fprintf(infile," ");
	  fclose(infile);
	}
      else
	{
	  fprintf (stderr,"Couldn't open %s\n",name);
	}
      tbfdpriraf(pfname,filename,&ierr);
    }
  exit (0);
}
