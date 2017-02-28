/*******************************************************************************

Filename:
     fdelete.c

Function:
     fdelete

Description:
     "main" entry point of program.  

Author/Date: 
     toliver / May, 1999

Modification History:

$Log: fdelete.c,v $
Revision 1.1  1999/06/01 18:05:03  toliver
initial revision


Function(s) referenced:
     int fdelete_get_params     : Reads .par file

Libraries referenced:
     cftoolslib
     ftoolslib

Usage:
     int fdelete (void)

Primary Variables:

     --- Inputs ---
     none

     --- Internal ---
     file      : FITS file structure pointer
     filename  : Name of file to be deleted
     context   : Text buffer for output messages
     delete    : File deletion flag
     status    : FITSIO error code

     --- Return ---
     none

*******************************************************************************/

#include "fdelete.h"

int fdelete (void)
{

   fitsfile *file;

   char filename[CBUFSIZE];
   char context[CBUFSIZE];

   int delete;
   int status;

   c_ptaskn ("fdelete1.0");

   status = fdelete_get_params (filename, &delete);
   if (OK == status)
   {

      status = fits_open_file (&file, filename, READONLY, &status);
      if (OK != status)
      {
         sprintf (context, "...Error opening %s", filename);
         Fcerrm (status);
         c_fcerr (context);
         return (status);
      }

      if (delete)
      {
         status = fits_delete_file (file, &status);
         if (OK == status)
         {
            sprintf (context, "File %s sucessfully deleted", filename);
            c_fcecho (context);
         }
         else
         {
            sprintf (context, "...Error deleting %s", filename);
            Fcerrm (status);
            c_fcerr (context);
            return (status);
         }
      }
      else
      {
         sprintf (context, "File %s not deleted", filename);
         c_fcecho (context);
      }

   } /* parameter file read successfully */

   return (status);

}
