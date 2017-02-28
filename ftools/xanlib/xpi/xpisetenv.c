#include "cfortran.h"

#ifdef VMS
#define xpisetenv xpisetenv_
#endif

void xpisetenv(str1,str2)
     char *str1;
     char *str2;
          
{
#ifdef VMS
  return;
  
#else
  static char tmpstr[1000];
  char *tmp1;
  
  
  char *getenv();
  
  
  strcpy (tmpstr,str1);
  strcat (tmpstr,"=");
  strcat (tmpstr,str2);
  

  putenv(tmpstr);

  tmp1 = getenv(str1);
  if (!tmp1) 
    {
/*      puts ("Null pointer returned"); */
    }
  else
    {
/*      printf ("getenv returned %s for %s",tmp1,str1);  */
    }
  
#endif
  
}

    
FCALLSCSUB2 (xpisetenv,XPISETENV,xpisetenv,STRING,STRING)
