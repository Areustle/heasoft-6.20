#include <stdio.h>	/* pil.h uses FILE */
#include <string.h>

#include "headas.h"
#include "pil.h"
#include "att_fatal.h"

/****************************************************************************
* This file contains a number of routines for reading and writing to
* the parameter file. These are basicly just wrappers for the 
* "xpi" routines found in the xanlib FTOOLS library.
* These wrappers are used for several reasons:
*
* - if someone wanted to replace the xpi routines with private routines
*   the would only have to re-write the routines in this file.
*   [this version is implemented in terms of PIL]
*
* - the wrappers have less cryptic C-like names
*
* - they conveniently include error checking.
*
* - the xpi.h header file is very sensitive to namespace collisions with
*   other defines. These can have mysterious and unpleasant side-effects.
*   We avoid this problem by separating the xpi calls into this file
*   which has no pre-processor "includes" other than xpi.h
*
*****************************************************************************/

/*************************************************************************
***************************************************************************
* error handler for PIL routines
***************************************************************************/
void check_for_pil_errors(int status, char* doing, char* name) 
{
   if (!status)
      return;

   headas_chat(0, "Couldn't %s parameter %s status=%d\n",
           doing, name, status);

   att_fatal(status);
}

/*************************************************************************
***************************************************************************
*
***************************************************************************/
void read_string_param(char* name, char* value, int dimen) 
{
   int status;

#if 1

   char tmp[PIL_LINESIZE];
   int length = dimen - 1;
   status = PILGetString(name, tmp);
   check_for_pil_errors(status, "read", name);
   if (strlen(tmp) > length) {
     tmp[length] = 0;
     headas_chat(1, "Warning truncated %s to '%s'\n", name, tmp);
   }
   strcpy(value, tmp);

#else

   char tmp[PIL_LINESIZE];
   PIL_PARAM * pp;
   int idx, special;

   /* reprompt until a string no longer than dimen including NULL is entered */
   PIL_find_name(&PILDefaultPFile, name, &idx);
   pp = &PILDefaultPFile.pp[idx];
   special = PILSpecialMode;
   while (!(status = PILGetString(name, tmp)) && strlen(tmp) + 1 > dimen) {
      fprintf(stderr, "value too long [limit is %d]\n", dimen);
      PILSpecialMode |= PIL_PSET_MODE;
      pp->attrib &= ~PIL_VALUE_FROM_CMDLINE;
   }
   PILSpecialMode = special;

   check_for_pil_errors(status, "read", name);
   strcpy(value, tmp);

#endif
}

/*************************************************************************
***************************************************************************
*
***************************************************************************/
double read_double_param(char* name) 
{
   int status;
   double value = 0.;

   status = PILGetReal(name, &value);
   check_for_pil_errors(status, "read", name);

   return(value);
}

/*************************************************************************
***************************************************************************
*
***************************************************************************/
int read_int_param(char* name) 
{
   int status;
   int value = 0;

   status = PILGetInt(name, &value);
   check_for_pil_errors(status, "read", name);

   return(value);
}

/*************************************************************************
***************************************************************************
*
***************************************************************************/
int read_boolean_param(char* name) 
{
   int status;
   int value = 0;

   status = PILGetBool(name, &value);
   check_for_pil_errors(status, "read", name);

   return(value);
}

/*************************************************************************
***************************************************************************
*
***************************************************************************/
void write_int_param(char* name, int value) 
{
   int status = PILPutInt(name, value);
   check_for_pil_errors(status, "write", name);
}

/*************************************************************************
***************************************************************************
*
***************************************************************************/
void write_double_param(char* name, double value) 
{
   int status = PILPutReal(name, value);
   check_for_pil_errors(status, "write", name);
}

