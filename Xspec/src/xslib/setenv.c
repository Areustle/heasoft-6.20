/*  the underscore in the function name needs to be there for */
/*    compatability with Sun Fortran                          */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static void xpisetenv(char* str1, char* str2)
{
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
}


void setenv_(char* instring)
{
/*   char *pltdev = "PGPLOT_DEVICE=                 ";*/
   char pltdev[] = {"PGPLOT_TYPE"};

   xpisetenv(pltdev,instring);

/* Below is the old version.  This worked in K&R c but won't work now */
/*   int  len2; 
   int  estatus;
   int  i;
   int  done;

   fprintf(stdout,"%s:instring\n",instring);
   done = 0;
   i = 0; 
   while (!done)
   {
     if (i > 0 && instring[i] == ' ')
     {
        len2 = i;
        done = 1;
     }
     i += 1;
     if (i == 254) done = 1;
    }
   if (len2 == 254) instring[254] = '\0';   

   fprintf (stdout,"len2 is %d\n",len2);
   for (i=0; i<len2; i++)
    {
       pltdev[12+i] = instring[i]; 
    }
   pltdev[14+len2] = '\0'; 
    
   fprintf(stdout,"%s:pltdev %s:scratch\n",pltdev,scratch);
   estatus = putenv(scratch);
   fprintf(stderr,"%d:estatus\n",estatus);
   estatus = putenv(instring);

   return;
   */
   
}     
