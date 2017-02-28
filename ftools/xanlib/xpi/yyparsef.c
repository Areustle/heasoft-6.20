/* Fortran to C interface for the parser */


#include <string.h>

/*#ifdef VMS
*/

#include "cfortran.h"

/*#endif */

#if defined(vms)
#define yyparsef yyparsef_
#define xpiparsestr xpiparsestr_
#define setstandalone_ setstandalone
#endif 

#define MAXLEN 20000  /* maximum length of string for recall, et al */

static char xpiinchar[MAXLEN] = {'\0'};
static char stackbuf[MAXLEN] = {'\0'};
static int standalone = 0;

int setstandalone_()
{
  /* call this when you are running a standalone command */
  standalone = 1;
  return 0;
  
}

static 

int yyparsef() 
{ 
  return zaparse();
};


int xpiparsestr(str)
char *str;
{
  strcpy(xpiinchar,str);
  strcat(xpiinchar,"\n");
  return zaparse();
}


int xpistr(str)
char *str;
{
  if (xpiinchar[0] == '\0') return 0;
  strcpy(str,xpiinchar);
  xpiinchar[0] = '\0';
  return strlen(str);
}

int stackcmd(buf,nchars)
char *buf;
int nchars;
{
  char tmp [MAXLEN];
  int i,j;


  for (i=0;i<MAXLEN-1;i++)
    {
      if (stackbuf[i] == '\n') stackbuf[i+1] = '\0';
    }
/*  printf ("Entered stackcmd, stack is %s\n",stackbuf); */
  /* is there anything to return? */
  if (!strlen(stackbuf)) return 0;

  /* Is there a | character in the buffer? */
  memset(tmp,'\0',MAXLEN);
  strncpy(tmp,stackbuf,MAXLEN);
  tmp[MAXLEN-1] = '\0';
  if (!standalone && strchr(tmp,';'))
    {
      /* There is a | character, return everything before it */
      for (i=0;tmp[i] != ';';i++);
      /* i now holds the index into tmp (and stackbuf) where
	 the | character is */
      tmp[i] = '\0';
      strncpy(buf,tmp,nchars);
      strcat(buf,"\n");
      i++;
      j = 0;
      while(' ' == tmp[i]) i++;
      while(tmp[i])
	{
	  stackbuf[j++] = tmp[i++];
	}
      stackbuf[j] = '\0';
    }
  else
    {
      strncpy(buf,stackbuf,nchars);
      buf[nchars-1] = '\0';
      stackbuf[0] = '\0';
      memset(stackbuf,'\0',MAXLEN);
    }


  return(strlen(buf));
}

int stackcmdcap(buf)
char *buf;
{
  memset(stackbuf,'\0',MAXLEN);
  strncpy(stackbuf,buf,MAXLEN);
  stackbuf[MAXLEN-1] = '\0';
  return 0;
}



/*#ifdef VMS
*/
FCALLSCFUN0(INT,yyparsef,YYPARSEF,yyparsef)
FCALLSCFUN1(INT,xpiparsestr,XPIPARSESTR,xpiparsestr,STRING)

/*
#else

yyparsef_()
{
  return yyparsef();
}

xpiparsestr_(str,lstr)
char *str;
int lstr;
{
  char str1[10000];

  strncpy(str1,str,lstr);
  str1[lstr] = '\0';
  return xpiparsestr(str1);
}



#endif
*/
