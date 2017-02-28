#include "cfortran.h"

#ifdef VMS
#define xpigetenv xpigetenv_
#endif

void xpigetenv(env,value)
     char *env;
     char *value;
{
#ifdef VMS

  strcpy(value,"(UNDEF)");
  return;
  
#else

  char *tmp1;
  char *getenv();
  
  tmp1 = getenv(env);
  
  if (!tmp1) 
       strcpy(value,"(UNDEF)");
  else
       strcpy(value,tmp1);
  
#endif
  
}
    
FCALLSCSUB2(xpigetenv,XPIGETENV,xpigetenv,STRING,PSTRING)
