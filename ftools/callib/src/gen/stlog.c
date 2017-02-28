#include "cfortran.h"
/*****************************************************************************/
/* Description: Creates or redefines a logical or environment variable.      */
/*              WARNING: this routine has a memory leak on UNIX systems.     */
/*              It will allocate memory for the new environment variable     */
/*              without freeing the memory assigned to the old environment   */
/*              variable.  Calling this routine in a loop will eat up memory */
/*              160 bytes at a time.                                         */
/*                                                                           */
/* Arguments:   str1  (i): the name of the logical or environment            */
/*                         variable to be created/redefined                  */
/*              str2  (i): the value which the logical or env var will have  */
/*                                                                           */
/* Origin:      Original vers written by B Oneel, NASA/GSFC Hughes STX       */
/*              presumably for the XPI software package                      */
/*                                                                           */
/* Authors/Modification History:                                             */
/* Bruce Oneel sometime, 1994 -- Original Version                            */
/* Ron Zellar  Sep 28, 1994   -- Modified to include VMS specific code       */
/* Ron Zellar  Sep 29, 1994   -- Incorporated dynamic memory allocation      */
/*****************************************************************************/

#ifdef VMS 
/* This function is only needed on VMS machines */
/*****************************************************************************/
/*                                                                           */
/*      cdescrip.c                                                           */
/*                                                                           */
/*      "c_descrip" creates a character string descriptor block compatible   */
/*  with DEC VMS FORTRAN code that can be used for calling FORTRAN routines  */
/*  with character string arguments.                                         */
/*                                                                           */
/*              Carl W. Akerlof                                              */
/*              Randall Laboratory of Physics                                */
/*              500 East University                                          */
/*              University of Michigan                                       */
/*              Ann Arbor, Michigan  48109                                   */
/*                                                                           */
/*              November 17, 1993                                            */
/*                                                                           */
/*****************************************************************************/

#include <string.h>
#include <descrip.h>

  struct dsc$descriptor_s c_descrip(char *string)
  {
     struct dsc$descriptor_s s;
     s.dsc$b_dtype = DSC$K_DTYPE_T;   /* Type is CHARACTER */
     s.dsc$b_class = DSC$K_CLASS_S;   /* String descriptor */
     s.dsc$w_length = strlen(string); /* String length     */
     s.dsc$a_pointer = string;        /* String pointer    */
     return (s);
  }
#endif

#ifdef VMS
#define stlog stlog_
#endif

stlog(str1,str2)
     char *str1;           /* name of system variable to be created */
     char *str2;           /* value which the system variable will have */
          
{
#ifdef VMS                 /* This is the code for VMS machines */

     int LIB$SET_LOGICAL();
     struct dsc$descriptor_s des1,des2,des3;
     char *str3;

     str3 = "LNM$PROCESS"; /* Create a logical in the process table */

     des1 = c_descrip(str1); /* define descriptor for variable name  */
     des2 = c_descrip(str2); /* define descriptor for variable value */
     des3 = c_descrip(str3); /* define descriptor for process varble */

     LIB$SET_LOGICAL(&des1,&des2,&des3); /* create the logical */

                           /* End of VMS code */
#else                      /* This is the code for UNIX machines */

  char *tmpstr;
  char *tmp1;
  
  
  char *getenv();
  
                           /* get enough memory to hold the new env var */
  tmpstr = malloc(strlen(str1)+strlen(str2)+2);
  
  strcpy (tmpstr,str1);    /* create "keyword = value" string */
  strcat (tmpstr,"=");
  strcat (tmpstr,str2);
  

  putenv(tmpstr);          /* create the environment variable */

/* This code checks to see if the env var was created    */
/* successfully, but it has been commented out...        */
/*  tmp1 = getenv(str1);                                 */
/*  if (!tmp1)                                           */
/*    {                                                  */
/*      puts ("Null pointer returned");                  */
/*    }                                                  */
/*  else                                                 */
/*    {                                                  */
/*      printf ("getenv returned %s for %s",tmp1,str1);  */
/*    }                                                  */
  
                           /* End of UNIX code */
#endif
  
}

/* cfortran stuff */
FCALLSCSUB2 (stlog,STLOG,stlog,STRING,STRING)
