#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "xselect.h"

void  Xsl_tcl_end() {
   return;
}

void xsl_runcc(char *comfil,int *status);



/****************xsl_runcc_ ********************/



#if defined sun && ! defined __STDC__
void xsl_runcc(comfil,status)
int *status;
char *comfil;
#else
void xsl_runcc(char *comfil,int *status)
#endif

{


  mode_t mode;
  
  mode = S_IRWXU | S_IRGRP | S_IROTH;
  

  chmod(comfil,mode);
  *status = system(comfil);
  
  return;
}

FCALLSCSUB2(xsl_runcc,XSL_RUNCC,xsl_runcc,STRING,PINT)


