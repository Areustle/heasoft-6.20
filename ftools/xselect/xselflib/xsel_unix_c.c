#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "cfortran.h"
#include <errno.h>

#define MAXPATHLEN 256

/* These are the prototypes, not for the SUN: */

#if ! defined sun || defined __STDC__
void xsl_chkdir(char *dirname,int *ierr);
void xsl_exist(char *filename,int *ierr);
void xsl_exit(int outstat);
void xsl_expand_dir(char *dirname,int *ierr);
size_t xsl_fcstln(char *string);
void xsl_list(char *file,char *lststr,char *datadir,char *curdir,int *status);
void xsl_nogtbuf_edit(int *status);
void xsl_rmfile(char *fname);
#endif



/********************** xsl_chkdir ********************/

#if defined sun && ! defined __STDC__
void xsl_chkdir(dirname,ierr)
     char *dirname;
     int *ierr;
#else
void xsl_chkdir(char *dirname,int *ierr)
#endif

{
  struct stat dirinfo;
  char command[256];
  size_t length;
  
  length = xsl_fcstln(dirname);
  if ( dirname[length-1] == '?' ) { /* If the dirname ends in a ? do an ls */
    strcpy(command,"/bin/ls ");
    dirname[length-1] = '\0';
    strcat(command,dirname);
    *ierr = system(command);
    *ierr = -10;                     /* return status = -10 as a flag */

  }
  else {
    if ( dirname[length-1] != '/' ) {/* if it doesn't end in a /, add one */
      dirname[length] = '/';
      dirname[length+1] = '\0';
      length++;
    }

    *ierr =  stat(dirname,&dirinfo);
  }
}

FCALLSCSUB2(xsl_chkdir,XSL_CHKDIR,xsl_chkdir,PSTRING,PINT)

/********************** xsl_exist ********************/



#if defined sun && ! defined __STDC__
void xsl_exist(filename,ierr)
char *filename;
int *ierr;
#else
void xsl_exist(char *filename,int *ierr)
#endif

{
  struct stat fileinfo;
  *ierr =  stat(filename,&fileinfo);
}

FCALLSCSUB2(xsl_exist,XSL_EXIST,xsl_exist,STRING,PINT)

/********************** xsl_exit ********************/



#if defined sun && ! defined __STDC__
void xsl_exit(outstat)
int outstat;
#else
void xsl_exit(int outstat)
#endif

{
  
  Xsl_tcl_end();
  if(outstat % 256 || outstat == 0) {
    exit(outstat);
  }
  else {
    exit(outstat+1);
  }
}

FCALLSCSUB1(xsl_exit,XSL_EXIT,xsl_exit,INT)

/********************** xsl_expand_dir ********************/

#if defined sun && ! defined __STDC__
void xsl_expand_dir(dirname,ierr)
     char *dirname;
     int *ierr;
#else
void xsl_expand_dir(char *dirname,int *ierr)
#endif

{
  
  FILE *strmpt;
  char command[256],result[256];

  strcpy(result,"");
  strcpy(command,"cd ");
  strcat(command,dirname);
#if defined sun && ! defined __STDC__
  strcat(command," 2> /dev/null;pwd"); 
#else
  strcat(command," ; pwd"); 
#endif
  strmpt = popen(command,"r");

  fscanf(strmpt,"%s",result); 

  *ierr = pclose(strmpt);

  if(*ierr == 0) {
    strcpy(dirname,result);
    strcat(dirname,"/");
  }
}

FCALLSCSUB2(xsl_expand_dir,XSL_EXPAND_DIR,xsl_expand_dir,PSTRING,PINT)

/********************** xsl_list ********************/

#if defined sun && ! defined __STDC__
void xsl_list(file,lststr,datadir,curdir,status)
char *file,*lststr,*datadir,*curdir;
int *status;
#else
void xsl_list(char *file,char *lststr,char *datadir,char *curdir,int *status)
#endif

{
  char comlin[512],*tmpstr;

  strcpy(comlin,"(find ");
  strcat(comlin,datadir);
  strcat(comlin," -name \"");
  strcat(comlin,lststr);
  strcat(comlin,"\" -print ");
  if(strcmp(file,"NONE") != 0){
    strcat(comlin," > ");
    tmpstr = strstr(file,curdir);
    if(tmpstr == NULL) {
      strcat(comlin,curdir);
      strcat(comlin,"/");
    }
    strcat(comlin,file);
    strcat(comlin," 2> /dev/null )");
  }
  else {
    strcat(comlin,")");
  }

  *status = system(comlin);
  return;

}

FCALLSCSUB5(xsl_list,XSL_LIST,xsl_list,STRING,STRING,STRING,STRING,PINT)

/********************* xsl_nogtb_edit *******************/

#if defined sun && ! defined __STDC__
void xsl_nogtbuf_edit(status)
int *status;
#else
void xsl_nogtbuf_edit(int *status)
#endif

{
  char *string="GTBUF_EDIT=OFF";

  *status = putenv(string);

  return;
}
FCALLSCSUB1(xsl_nogtbuf_edit,XSL_NOGTBUF_EDIT,xsl_nogtbuf_edit,PINT)


/********************** xsel_rmfile ********************/


#if defined sun && ! defined __STDC__
void xsl_rmfile(fname)
char *fname;
#else
void xsl_rmfile(char *fname)
#endif

{
int ierr;

ierr = unlink(fname);

return;

}

FCALLSCSUB1(xsl_rmfile,XSL_RMFILE,xsl_rmfile,STRING)


/********************** xsl_spawn ********************/



#if defined sun && ! defined __STDC__
void xsl_spawn(comlin,status)
int *status;
char *comlin;
#else
void xsl_spawn(char *comlin,int *status)
#endif

{

  *status = system(comlin);
  
  return;
}

FCALLSCSUB2(xsl_spawn,XSL_SPAWN,xsl_spawn,STRING,PINT)
     

/********************** xsl_fcstln ********************/

#if defined sun && ! defined __STDC__
size_t xsl_fcstln(string)
char *string;
#else
size_t xsl_fcstln(char *string)
#endif

{
  size_t lstring;
  int i;

  lstring = strlen(string);
  i = lstring-1;
  while(string[i] == ' '){
    i--;
    if(i<0){
      return 0;
    }
  }
  return i+1;
}
