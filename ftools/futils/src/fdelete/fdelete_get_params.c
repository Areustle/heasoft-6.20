/*******************************************************************************

Filename:
     fdelete_get_params.c

Function:
     fdelete_get_params

Description:
     Read parameter file
 
Author/Date: 
     toliver / May, 1999

Modification History:

$Log: fdelete_get_params.c,v $
Revision 1.1  1999/06/01 18:06:18  toliver
initial revision


Libraries referenced:
     cftoolslib
     ftoolslib
     xanlib

Usage:
     int fdelete_get_params (char *file, int *delete);

Primary Variables:

     --- Inputs ---
     file    : Name of file to be deleted

     --- Internal ---
     confirm : Prompt user to confirm file deletion?
     status  : FITSIO error code

     --- Return ---
     delete  : Delete file?
     int     : FITSIO error code

*******************************************************************************/

#include <xpi.h>        /* Required for parameter file operations. */

#include "fdelete.h"

int fdelete_get_params (char *file, int *delete)
{

   int status = OK;
   int confirm;

   int BufLen_2 = BUFSIZE;   /* Required for C calls to Uclgs* functions */

   Uclgst ("file", file, &status);
   if (OK != status)
   {
      c_fcerr ("Parameter 'file' not found in .par file");
      return (status);
   }

   Uclgsb ("confirm", &confirm, &status);
   if (OK != status)
   {
      c_fcerr ("Parameter 'confirm' not found in .par file");
      return (status);
   }

   if (confirm)
   {
      Uclgsb ("delete", delete, &status);
      if (OK != status)
      {
         c_fcerr ("Parameter 'delete' not found in .par file");
         return (status);
      }
   }
   else
   {
      *delete = 1;
   }

   return (status);
}
