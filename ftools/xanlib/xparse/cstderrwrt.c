#include "cfortran.h"
#include <stdio.h>

void cstderrwrt(str1)
     char *str1;
          
{
  fputs(str1,stderr);
  fputc('\n',stderr);
  fflush(stderr);
}

FCALLSCSUB1(cstderrwrt,CSTDERRWRT,cstderrwrt,STRING)
