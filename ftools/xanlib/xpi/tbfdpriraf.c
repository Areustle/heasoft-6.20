
/** tbfdpriraf.c
*
* This is a routine to find where the paramter file (.par) file is stored.
*

$Id: tbfdpriraf.c,v 3.12 2015/06/11 20:02:35 kaa Exp $

$Log: tbfdpriraf.c,v $
Revision 3.12  2015/06/11 20:02:35  kaa
Added the history command to the set of standard gtcom2 commands. This toggles
writing the history file on and off. The immediate use is in xselect to avoid
problems when running multiple instances of the program.

Note that adding a new command required putting it in lists in both ldcmds1.f
and xpitbldcm.f - one of these ought to be redundant.

Tidied up a bunch of compiler warnings.

Revision 3.11  2000/06/29 19:35:49  peachey
Do not include blank lines when counting lines.

Revision 3.10  1998/07/14 19:05:01  peachey
Cygwin port

 * Revision 3.9  1997/05/22  16:01:20  oneel
 * Changed such that if a user .par file is found and pfclobber isn't set
 * then you're still good to go.  Previously if you didn't have a system
 * .par file you halted.
 *
Revision 3.8  1997/05/20 17:35:20  oneel
Fixed a bug where the default value stored in the parameter file was
way big (> 255 chars) and I'd put the length in a character(silly)

Added path searching to tbldpriraf which seems to work like the SAO
Host interface

Revision 3.7  1997/05/16 17:04:59  oneel
added log and id cvs fields


* It takes apart 

*/

#define LINESIZE 1000
#define XPIASSERT(EXP,STR) (void)((EXP) || (xpicerror(STR,__LINE__,__FILE__), 0))

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#if defined(vms)
#define tbfdpriraf tbfdpriraf_
#define xpiparsepar xpiparsepar_
#include <types.h>
#include <stat.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif


#include "cfortran.h"

static int nlines(char *str);
static void copyfile(char *ifile, char *ofile);
void updateit(char *basename,char *pfname,int *ierr);
void tbfdpriraf(char *basename,char *pfname,int *ierr);
int findpf(char *basename,char *pfname,char *pfpath);
int copypf(char *basename,char *pfname,char *pfpath, char *syspf);
void xpiparsepar(char *inln,char *name,char *type,char *upd,char *defl,
		char *min,char *max,char *desc);
void xpicerror(char *str,int lineno, char *filename);


/* pfname will hold the resulting .par file name */
/* basename will hold the base name for the parfile without the directory */
/* ierr will be 0 if everything is ok, else it'll be not 0 */
/*      1 means the file couldn't be found, and 2 means it couldn't be
        copied
*/


void tbfdpriraf(char *basename,char *pfname,int *ierr)
{
  char *work;
  char *tmpstr;
  char *user;
  char *sys;
  char *syspfname;
  char *userpfname;
  struct stat usepf;
  struct stat syspf;
  int syspfstat;


  /* set return value */
  *ierr = 1;

  /* alloc space */
  tmpstr = malloc(LINESIZE);
  XPIASSERT(tmpstr,"Out of Memory");
  user = malloc(LINESIZE);
  XPIASSERT(user,"Out of Memory");
  sys = malloc(LINESIZE);
  XPIASSERT(sys,"Out of Memory");
  syspfname = malloc(LINESIZE);
  XPIASSERT(syspfname,"Out of Memory");
  userpfname = malloc(LINESIZE);
  XPIASSERT(userpfname,"Out of Memory");




/* Get pfiles and split the user and system parts off of it */  
  work = getenv("PFILES");
  if (NULL == work) {
    fprintf (stderr,"ERROR: PFILES not defined\n");
    exit(1);
  }

  strcpy(tmpstr,work);

  if (getenv("XPIDEBUG") != NULL) {
    fprintf (stdout,"PFILES is %s\n",tmpstr);
    fflush (stdout);
  }



  if (NULL == strchr(tmpstr,';')) {
    strcpy(user,tmpstr);
    strcpy(sys,tmpstr);
  }
  else {
    work = strtok(tmpstr,";");
    strcpy(user,work);
    work = strtok(NULL,";");
    strcpy(sys,work);
  }
  

  if (getenv("XPIDEBUG") != NULL) {
    fprintf (stdout,"user %s system %s\n",user,sys);
    fflush (stdout);
  }

  syspfstat = findpf(basename,syspfname,sys);


  /* we got a system par file now in syspfname, go get the user parfile */

  if (findpf(basename,userpfname,user)) {
    /* we didn't find it, we need to copy it */
    if (getenv("XPIDEBUG") != NULL) {
      fprintf(stdout,"\nno user .par file found\n");
    }
    if (syspfstat) {
      fprintf(stderr,"No user parameter file found in path %s\n",user);
      fprintf(stderr,
	      "Unable to copy a parameter file for %s from the path \n%s\n",
	      basename,sys);
      exit(1);
    }
    if (copypf(basename,userpfname,user,syspfname)) {
      fprintf(stderr,"Unable to copy par file for %s into the path\n%s\n",
	      basename,user);
      exit(1);
    }
  }

  /* now we have a system parameter file in syspfname and a user one in
     userpfname */

  strcpy (pfname,userpfname);
  *ierr = 0;

  /* If pfclobber is set then check times and lines */

  if (getenv("PFCLOBBER") != NULL) {
    if (syspfstat) {
      fprintf(stderr,
	      "Unable to find a system parameter file but PFCLOBBER set\n%s\n",
	      sys);
      exit(1);
    }
    if (nlines(userpfname) != nlines(syspfname)) {
      if (getenv("XPIDEBUG") != NULL) {
	fprintf(stdout,"files have different line counts sys - %d user %d\n",
		nlines(syspfname),nlines(userpfname));
      }
      copyfile(userpfname,syspfname);
    }
    if (stat(syspfname,&syspf)) {
      /* really bad, it should be there still */
      fprintf(stderr,"Can't stat system parameter file %s\n",syspfname);
      exit(1);
    }
    if (stat(userpfname,&usepf)) {
      /* really bad, should be there by now */
      fprintf(stderr,"Can't stat user parameter file %s\n",userpfname);
    }
    /* CYGWIN32 doesn't do inode stuff correctly, so use st_mtime */
#ifdef __CYGWIN32__
    if (syspf.st_mtime > usepf.st_mtime) {
      /* system parameter file is newer */
      if (getenv("XPIDEBUG") != NULL) {
	fprintf(stdout,"syspfile is newer than userpfile sys - %d user - %d\n",
		(int)syspf.st_mtime,(int)usepf.st_mtime);
      }
      copyfile(userpfname,syspfname);
    }
#else
    if (syspf.st_ctime > usepf.st_ctime) {
      /* system parameter file is newer */
      if (getenv("XPIDEBUG") != NULL) {
	fprintf(stdout,"syspfile is newer than userpfile sys - %d user - %d\n",
		(int)syspf.st_ctime,(int)usepf.st_ctime);
      }
      copyfile(userpfname,syspfname);
    }
#endif
  }

  free(tmpstr);
  free(user);
  free(sys);
  free(syspfname);
  free(userpfname);

}

/* findpf finds a parameter file named basename.par in pfpath and returns the
   result in pfname.  pfname will be a 0 length string if nothing was found
   and the function will return a 1, else it returns 0*/

int findpf(char *basename, char *pfname, char *pfpath) {
  
  char *work;
  char *tmpstr;
  char *tpfpath;
  int retval = 1;
  FILE *tfile;

  /* allocate memory */
  tmpstr = malloc(LINESIZE);
  XPIASSERT(tmpstr,"Out of Memory");
  tpfpath = malloc(LINESIZE);
  XPIASSERT(tpfpath,"Out of Memory");

  *pfname = '\0';

  /* find which directory has the file */
  strcpy(tpfpath,pfpath);
  if (getenv("XPIDEBUG") != NULL) {
    fprintf(stdout,"searching parfile path %s\n",tpfpath);
  }

  work = strtok(tpfpath,":");
  do {
    strcpy(tmpstr,work);  /* directory */
#ifdef unix
    strcat(tmpstr,"/"); /* optional separator */
#endif
    strcat(tmpstr,basename); /* filename */
    if (getenv("XPIDEBUG") != NULL) {
      fprintf(stdout,"looking for file %s",tmpstr);
    }
    /* can we open it for read */
    if (NULL != (tfile = fopen(tmpstr,"r"))) {
      /* found the right file */
      if (getenv("XPIDEBUG") != NULL) {
	fprintf(stdout,"- OK\n");
      }
      fclose(tfile);
      strcpy(pfname,tmpstr);
      retval = 0;
      break;
    }
    
  } while (NULL != (work = strtok(NULL,":")));
  
  free(tmpstr);
  free(tpfpath);
  
  return retval;
  
}

/* copypf copies a parameter file for basename into the first place it can 
   in pfpath and returns the value in pfname and a retval of 0.  If it is 
   unsucessful then it returns 1 and sets pfname to an empty string */

int copypf(char *basename, char *pfname, char *pfpath, char *syspf) {
  
  char *work;
  char *tmpstr;
  char *tpfpath;
  int retval = 1;
  FILE *tfile;

  /* allocate memory */
  tmpstr = malloc(LINESIZE);
  XPIASSERT(tmpstr,"Out of Memory");
  tpfpath = malloc(LINESIZE);
  XPIASSERT(tpfpath,"Out of Memory");

  *pfname = '\0';

  /* find which directory has the file */
  strcpy(tpfpath,pfpath);
  if (getenv("XPIDEBUG") != NULL) {
    fprintf(stdout,"trying to copy into parfile path %s\n",tpfpath);
  }

  work = strtok(tpfpath,":");
  do {
    strcpy(tmpstr,work);  /* directory */
#ifdef unix
    strcat(tmpstr,"/"); /* optional separator */
#endif
    strcat(tmpstr,basename); /* filename */
    if (getenv("XPIDEBUG") != NULL) {
      fprintf(stdout,"trying to copy into file %s",tmpstr);
    }
    /* can we open it for write */
    if (NULL != (tfile = fopen(tmpstr,"w"))) {
      /* found the right file */
      if (getenv("XPIDEBUG") != NULL) {
	fprintf(stdout,"- OK\n");
      }
      
      fclose(tfile);
      strcpy(pfname,tmpstr);
      copyfile(pfname,syspf);
      retval = 0;
      break;
    }
    
  } while (NULL != (work = strtok(NULL,":")));
  

  free(tmpstr);
  free(tpfpath);

  return retval;
  
}


static void copyfile(char *outfile, char *infile)
{
  FILE *ifile;
  FILE *ofile;
  char *inline1;

  
  if (NULL == (ifile = fopen(infile,"r"))) {
    /* really bad, we could just read it a few secs ago! */
    fprintf (stderr,"Unable to read system par file %s\n",infile);
    exit(1);
  }

  if (NULL == (ofile = fopen(outfile,"w"))) {
    /* really bad, we could just write it a few secs ago! */
    fprintf (stderr,"Unable to write user par file %s\n",outfile);
    exit(1);
  }
  
  inline1 = malloc(LINESIZE);
  XPIASSERT(inline1,"Out of Memory");
  
  while (fgets(inline1,LINESIZE,ifile)) {
    fputs(inline1,ofile);
  }
  fclose(ifile);
  fclose(ofile);
  free(inline1);
}

/* This returns the number of lines in a file */
static int nlines(char *str)

{
  char *inline1;
  int i = 0;
  FILE *infile;

  /* get memory for the input line */
  inline1 = malloc(LINESIZE);
  XPIASSERT(inline1,"Out of Memory");

  /* debugging on? */
  if (getenv("XPIDEBUG") != NULL) {
    fprintf (stdout,"par file %s ",str);
  }
  
  /* Open file */
  if (NULL == (infile = fopen(str,"r"))) {
    free(inline1);
    return -1;
  }
  /* count the number of lines, # is a comment, \n implies a blank line */
  while (fgets(inline1,LINESIZE,infile)) {
    if (inline1[0] != '#' && inline1[0] != '\n') i++; 
  }

  /* close the file and return */
  fclose(infile);
  if (getenv("XPIDEBUG") != NULL) {
    fprintf (stdout,"has %d lines\n",i);
  }
  free(inline1);
  return i;
}
      
/* Gets the comma delimited value from inline and stores it in value 
   static char *xpiparseparnext(char *inln)
   {
   char *value;
   
   if (*inln == ',')
   {
   value = "";
   }
   else 
   {
   if (*inln == '"' && *(inln+1) == '"')
   {
   value = "";
   }
   else
   {
   if (*inln == '"')
   {
   value = strtok(inln+1,"\"");
   if (value) value--;
   } 
   else 
   {
   value = strtok(inln,",");
   }
   }
   }
   
   return value;
   }
   */


/* This has a bit of a trick where the first character of name, upd, defl,
   min, max, and desc have the maximum length of the fortran strings */

void xpiparsepar(char *inln,char *name,char *type,char *upd,char *defl,
		char *min,char *max,char *desc)
{

    char tmp1[LINESIZE];
    char parsed[LINESIZE];
    char *parsed1;
    char *tmp;
    int i, j;

    strcpy(tmp1,inln);
    tmp = tmp1;

    parsed1 = parsed;

    /* parse off the name, this always ends with a , */

    while (*tmp && *tmp != ',')
	{
	    *parsed1++ = *tmp++;
	}
    *parsed1 = '\0';
    if (strlen(parsed) > (int)*name)
	{
	    fprintf (stderr, "Length error name was %lu should have been %d on line %s\n",strlen(parsed),*name,inln);
	    return;
	}
    strcpy(name,parsed);
    parsed1 = parsed;
    
    if (!*tmp) 
	{
	    fprintf (stderr,"Early return on line %s\n",inln);
	    return;
	}
    if (*tmp == ',') tmp++;
    
    /* Now the type, this always ends with a , */

    while (*tmp && *tmp != ',')
	{
	    *parsed1++ = *tmp++;
	}
    *parsed1 = '\0';
    if (strlen(parsed) > (int)*type)
	{
	    fprintf (stderr, "Length error type was %lu should have been %d on line %s\n",strlen(parsed),*type,inln);
	    return;
	}
    strcpy(type,parsed);
    parsed1 = parsed;
    
    if (!*tmp) 
	{
	    fprintf (stderr,"Early return on line %s\n",inln);
	    return;
	}
    if (*tmp == ',') tmp++;
    
    /* Now the mode, this always ends with a , */

    while (*tmp && *tmp != ',')
	{
	    *parsed1++ = *tmp++;
	}
    *parsed1 = '\0';
    if (strlen(parsed) > (int)*upd)
	{
	    fprintf (stderr, "Length error upd was %lu should have been %d on line %s\n",strlen(parsed),*upd,inln);
	    return;
	}
    strcpy(upd,parsed);
    parsed1 = parsed;
    
    if (!*tmp) 		
	{
	    fprintf (stderr,"Early return on line %s\n",inln);
	    return;
	}
    if (*tmp == ',') tmp++;
    
    /* Now the default value, This can start with either a " or ' */

    switch (*tmp) 
	{
	case '\'' :
	    tmp++;
	    while (*tmp && *tmp != '\'')
		{
		    *parsed1++ = *tmp++;
		}
	    *parsed1 = '\0';
	    
	    if (!*tmp) 		
		{
		    fprintf (stderr,"Early return on line %s\n",inln);
		    return;
		}
	    if (*tmp == '\'') tmp++;
	    break;
	case '"':
	    tmp++;
	    while (*tmp && *tmp != '"')
		{
		    *parsed1++ = *tmp++;
		}
	    *parsed1 = '\0';
	    if (!*tmp) 
		{
		    fprintf (stderr,"Early return on line %s\n",inln);
		    return;
		}
	    if (*tmp == '"') tmp++;
	    break;
	default:
	    while (*tmp && *tmp != ',')
		{
		    *parsed1++ = *tmp++;
		}
	    *parsed1 = '\0';
	    if (!*tmp) 
		{
		    fprintf (stderr,"Early return on line %s\n",inln);
		    return;
		}
	}
    *parsed1 = '\0';
    i = (int) *defl;
    if (i < 0) 
	{
	    i += 256;
	}
    j = (int) *(defl+1);
    if (j < 0) j += 256;
    i = i + 256*j;
    if (strlen(parsed) > i)
	{
	    fprintf (stderr, "Length error defl was %lu should have been %d on line %s\n",strlen(parsed),i,inln);
	    return;
	}
    strcpy(defl,parsed);
    parsed1 = parsed;

    if (*tmp == ',') tmp++;

    /* Now the minimum value, This can start with either a " or ' */

    switch (*tmp) 
	{
	case '\'' :
	    tmp++;
	    while (*tmp && *tmp != '\'')
		{
		    *parsed1++ = *tmp++;
		}
	    *parsed1 = '\0';
	    
	    if (!*tmp) 
		{
		    fprintf (stderr,"Early return on line %s\n",inln);
		    return;
		}
	    if (*tmp == '\'') tmp++;
	    break;
	case '"':
	    tmp++;
	    while (*tmp && *tmp != '"')
		{
		    *parsed1++ = *tmp++;
		}
	    *parsed1 = '\0';
	    if (!*tmp) 		
		{
		    fprintf (stderr,"Early return on line %s\n",inln);
		    return;
		}
	    if (*tmp == '"') tmp++;
	    break;
	default:
	    while (*tmp && *tmp != ',')
		{
		    *parsed1++ = *tmp++;
		}
	    *parsed1 = '\0';
	    if (!*tmp) 
		{
		    fprintf (stderr,"Early return on line %s\n",inln);
		    return;
		}
	}
    *parsed1 = '\0';
    if (strlen(parsed) > (int)*min)
	{
	    fprintf (stderr, "Length error min was %lu should have been %d on line %s\n",strlen(parsed),*min,inln);
	    return;
	}
    strcpy(min,parsed);
    parsed1 = parsed;

    if (*tmp == ',') tmp++;

    /* Now the maximum value, This can start with either a " or ' */

    switch (*tmp) 
	{
	case '\'' :
	    tmp++;
	    while (*tmp && *tmp != '\'')
		{
		    *parsed1++ = *tmp++;
		}
	    *parsed1 = '\0';
	    
	    if (!*tmp) 
		{
		    fprintf (stderr,"Early return on line %s\n",inln);
		    return;
		}
	    if (*tmp == '\'') tmp++;
	    break;
	case '"':
	    tmp++;
	    while (*tmp && *tmp != '"')
		{
		    *parsed1++ = *tmp++;
		}
	    *parsed1 = '\0';
	    if (!*tmp) 
		{
		    fprintf (stderr,"Early return on line %s\n",inln);
		    return;
		}
	    if (*tmp == '"') tmp++;
	    break;
	default:
	    while (*tmp && *tmp != ',')
		{
		    *parsed1++ = *tmp++;
		}
	    *parsed1 = '\0';
	    if (!*tmp) 
		{
		    fprintf (stderr,"Early return on line %s\n",inln);
		    return;
		}
	}
    *parsed1 = '\0';
    if (strlen(parsed) > (int)*max)
	{
	    fprintf (stderr, "Length error max was %lu should have been %d on line %s\n",strlen(parsed),*max,inln);
	    return;
	}
    strcpy(max,parsed);
    parsed1 = parsed;

    if (*tmp == ',') tmp++;

    /* Now the desc parameter (prompt) */

    while (*tmp)
	{
	    *parsed1++ = *tmp++;
	}
    *parsed1 = '\0';
    if (strlen(parsed) > (int)*desc)
	{
	    fprintf (stderr, "Length error desc was %lu should have been %d on line %s\n",strlen(parsed),*desc,inln);
	    return;
	}
    strcpy(desc,parsed);
    parsed1 = parsed;
}

void xpicerror(char *str, int lineno, char *filename)
{
  fprintf(stderr,"XPICERROR : %s at line %d in file %s\n",str,lineno,filename);
  fflush(stderr);
  abort();
}

FCALLSCSUB3 (tbfdpriraf,TBFDPRIRAF,tbfdpriraf,STRING,PSTRING,PINT)


FCALLSCSUB8 (xpiparsepar,XPIPARSEPAR,xpiparsepar,STRING,
             PSTRING,
             PSTRING,
             PSTRING,
             PSTRING,
             PSTRING,
             PSTRING,
             PSTRING)
