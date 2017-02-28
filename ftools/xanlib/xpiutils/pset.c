/* set parameters in a par file */


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

#define IRAFPARPUTLINE(i,defl) \
  CCALLSFSUB2(IRAFPARPUTLINE,irafparputline,INT,STRING,i,defl)

#include <stdio.h>
#include <string.h>

extern int xargc;    /* to avoid loading main.o from libf2c.a */
extern char **xargv; /* to avoid loading main.o from libf2c.a */

int MAIN_; /* work around SunOS 4.1.3 bug */

main (argc,argv)
     char *argv[];
     int argc;
     

{
  
  int i,j, k;
  int npars;
  int ierr;
  int found;
  
    
  char pfname[1000];
  char name[1000],desc[1000],type[1000],minp[1000],maxp[1000],defl[1000];
  char mode[1000], filename[1000], tmpstr[1000];
  char prompt[1000];
  int  lprompt, ldefl, ios;
  char *parname, *deflnew;
  
  xargc=argc;
  xargv=argv;

  if (argc < 2)
    {
      fprintf (stderr,"No parameter files to modify\n");
      exit(1);
    }
  
  strcpy(pfname,argv[1]);
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
  
  IRAFPARGETPFNAME(filename);
  
  IRAFPARGETNPARS(npars);
#ifdef DEBUG
  fprintf (stderr,"Num pars %d\n",npars);
  fflush (stderr);
#endif
  if (argc == 2)
    {
      /* prompt for all */
      for (i=1;i<=npars;i++)
	{
	  IRAFPARGETLINE(i,name,desc,type,minp,maxp,defl,mode);
	  if (!strcmp(desc,""))
	    {
	      sprintf (prompt,"%s (%s):",name,defl);
	    }
	  else
	    {
	      sprintf (prompt,"%s (%s): ",desc,defl);
	    }
	  lprompt = strlen(prompt);
	  freadline(prompt,lprompt,defl,&ldefl,&ios);
	  if (ios) exit(1);
/*
          if (NULL == fgets(defl,1000,stdin))
	    {
	      exit(1);
	    }
*/
	  if (ldefl > 0)
	    {
	      defl[strlen(defl)] = '\0';
	      IRAFPARPUTLINE(i,defl);
	    }
	}
    }
  else
    {
      for (i=2;i<argc;i++)
	{
	  strcpy(tmpstr,argv[i]);
	  if (strchr(tmpstr,'='))
	    {
	      parname = tmpstr;
	      deflnew = strchr(tmpstr,'=');
	      *deflnew = '\0';
	      deflnew++;
#ifdef DEBUG
	      fprintf (stderr,"changing %s to %s\n",parname,deflnew);
	      fflush (stderr);
#endif
	      found = 0;
	      
	      for (k=1;k<=npars;k++)
		{
		  
		  IRAFPARGETLINE(k,name,desc,type,minp,maxp,defl,mode);
		  if (!strcmp(name,parname))
		    {
		      found = 1;
		      IRAFPARPUTLINE(k,deflnew);
		    }
		}
	      if (!found)
		{
		  fprintf (stderr,"Parameter %s not found\n",parname);
		  exit(1);
		  
		}

	    }
	  else if (tmpstr[strlen(tmpstr)-1] == '!')
	    {
	      tmpstr[strlen(tmpstr)-1] = '\0';
	      found = 0;
	      for (k=1;k<=npars;k++)
		{
		  IRAFPARGETLINE(k,name,desc,type,minp,maxp,defl,mode);
		  if (!strcmp(name,tmpstr))
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
		      freadline(prompt,lprompt,defl,&ldefl,&ios);
/*
		      if (NULL == fgets(defl,1000,stdin))
			{
			  exit(1);
			}
*/
		      if (ldefl > 0)
			{
			  defl[strlen(defl)] = '\0';
			  IRAFPARPUTLINE(k,defl);
			}
		    }
		}
	      if (!found)
		{
		  fprintf (stderr,"Parameter %s not found\n",tmpstr);
		  exit(1);
		  
		}
	    }
	  else if ((i+3 <= argc) && (!strcmp(argv[i+1],"=")))
	    {
	      parname = argv[i];
	      deflnew = argv[i+2];
#ifdef DEBUG
	      fprintf (stderr,"changing %s to %s\n",parname,deflnew);
	      fflush (stderr);
#endif
	      found = 0;	      
	      for (k=1;k<=npars;k++)
		{
		  
		  IRAFPARGETLINE(k,name,desc,type,minp,maxp,defl,mode);
		  if (!strcmp(name,parname))
		    {
		      found = 1;
		      IRAFPARPUTLINE(k,deflnew);
		    }
		}
	      if (!found)
		{
		  fprintf (stderr,"Parameter %s not found\n",parname);
		  exit(1);
		}

	      i+=2;
	    }
	  else
	    {
#ifdef DEBUG
	      fprintf (stderr,"changing parameter number %d to %s\n",
		       i-1,tmpstr);
	      fflush(stderr);
#endif
	      IRAFPARPUTLINE(i-1,tmpstr);
	    }
	}
    }
  CloseDefaultPF ();
  exit(0);
}
