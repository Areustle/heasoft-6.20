/** tbfdpr.c
*
* This is a routine to find where the paramter file (.par) file is stored.
*
* It used to do...
*
* First it looks in the current directory (VMS [], unix .)
* Then it looks one directory down in the directory xparm ([.xparm] xparm/
* Then it looks at your home directory (sys$login, $HOME)
* Then it looks at your home directory, down one in a directory xparm 
*				(sys$login:[.xparm] or $HOME/xparm)
* Then it looks at the system directory.
*
* If it finds the parameter file in the system directory then it copies it
* to someplace in the following order:
*
* One directory down from the current directory ([.xparm] or xparm/)
* Home directory down xparm (sys$login:[.xparm] or $HOME/xparm)
* current directory (if writable) ([] or .)
* Home directory (sys$login or $HOME)

*
* NOW it just calls tbfdpriraf

*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#if defined(vms)
#define tbfdpr tbfdpr_
#define xpicwd xpicwd_
#define tbfdpriraf tbfdpriraf_
#include <types.h>
#include <stat.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#endif


#include "cfortran.h"

#define PTEND(disk,dir,file) \
  CCALLSFSUB3(PTEND,ptend,STRING,STRING,PSTRING,disk,dir,file)

#define TRLOG(cbuf,lbuf,cret,lret) \
  CCALLSFSUB4(TRLOG,trlog,STRING,INT,PSTRING,PINT,cbuf,lbuf,cret,lret)

#define XERROR(cbuf,chatty) \
  CCALLSFSUB2(XERROR,xerror,STRING,INT,cbuf,chatty)


/* pfname will hold the resulting .par file name */
/* basename will hold the base name for the parfile  A parfile is called
   <basename>.par */
/* sysdir holds the system directory */
/* ierr will be 0 if everything is ok, else it'll be not 0 */
/*      1 means the file couldn't be found, and 2 means it couldn't be
        copied
*/


void tbfdpr(basename,sysdisk,sysdir,pfname,ierr)
char *pfname, *sysdisk, *sysdir, *basename;
int *ierr;
{
  char tbasename[1000];
  
  strcpy(tbasename,basename);
  
  strcat(tbasename,".par");
  tbfdpriraf(tbasename,pfname,ierr);
  xpidebuglog(pfname,"xselect or extractor command line");
}

void xpicwd(cwd)
     char *cwd;
{
  char tmpstr[1000];
  
#if defined(linux) || defined(__APPLE__) || defined(sysV88) || defined(vms) || defined(solaris) || defined(__hpux)
  getcwd(tmpstr,1000);
#else
  getwd(tmpstr);
#endif
  strcpy(cwd,tmpstr);
  strcat(cwd,"/");
}


FCALLSCSUB5 (tbfdpr,TBFDPR,tbfdpr,STRING,STRING,STRING,PSTRING,PINT)
FCALLSCSUB1 (xpicwd,XPICWD,xpicwd,PSTRING)

