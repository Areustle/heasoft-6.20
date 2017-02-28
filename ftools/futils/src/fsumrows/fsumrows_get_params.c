/*******************************************************************************

Filename:
     fsumrows_get_params.c

Function:
     fsumrows_get_params

Description:
     Read parameter file
 
Author/Date: 
     toliver / April, 1999

Modification History: see log at bottom of file

Usage:
     int fsumrows_get_params (char *infile, char *outfile, char *cols,
                              char *rows, int *operation, int *nullval,
                              int *clobber);

Primary Variables:

     --- Inputs ---
     infile    : Filename (including extension) of input file
     outfile   : Filename (including extension) of output file
     cols      : List of input table column specifiers
     rows      : List of input table row ranges to be processed
     operation : Operation to perform on input data (SUM or AVG)
     nullval   : Numerical value of integer NULLS in output table
     clobber   : Overwrite existing files?
     sametype  : Convert output back to input type after operation?

     --- Internal ---
     opstr     : Operation code string from .par file
     status    : FITSIO error code

     --- Return ---
     int       : FITSIO error code

*******************************************************************************/

#include <xpi.h>        /* Required for parameter file operations. */

#include "fsumrows.h"

int fsumrows_get_params (char *infile, char *outfile, char *cols,
                         char *rows, int *operation, int *nullval,
                         int *clobber, int *sametype)
{

   char opstr[CBUFSIZE];
   int status = OK;

   int BufLen_2 = BUFSIZE;   /* Required for C calls to Uclgs* functions */

   Uclgst ("infile", infile, &status);
   if (OK != status)
   {
      c_fcerr ("Parameter 'infile' not found in .par file");
      return (status);
   }

   Uclgst("outfile", outfile, &status);
   if (OK != status)
   {
      c_fcerr("Parameter 'outfile' not found in .par file");
      return (status);
   }

   Uclgst("cols", cols, &status);
   if (OK != status)
   {
      c_fcerr("Parameter 'cols' not found in .par file");
      return (status);
   }

   Uclgst("rows", rows, &status);
   if (OK != status)
   {
      c_fcerr ("Parameter 'rows' not found in .par file");
      return (status);
   }

   Uclgst ("operation", opstr, &status);
   if (OK != status)
   {
      c_fcerr("Parameter 'operation' not found in .par file");
      return (status);
   }
   fits_uppercase (opstr);
   if (!strcmp (opstr, "AVG"))
   {
      *operation = AVG_OP;
   }
   else if (!strcmp (opstr, "SUM"))
   {
      *operation = SUM_OP;
   }
   else
   {
      status = NOT_OK;
      c_fcerr ("Bad operation parameter - only use SUM or AVG");
      return (status);
   }

   Uclgsi("nullval", nullval, &status);
   if(status != OK) {
      c_fcerr("Parameter 'nullval' not found in .par file");
      return status;
   }

   Uclgsb ("clobber", clobber, &status);
   if (OK != status)
   {
      c_fcerr ("Parameter 'clobber' not found in .par file");
      return (status);
   }

   Uclgsb ("sametype", sametype, &status);
   if (OK != status)
   {
      c_fcerr ("Parameter 'sametype' not found in .par file");
      return (status);
   }

   return (status);
}

/******************************************************************************

$Log: fsumrows_get_params.c,v $
Revision 1.2  1999/10/27 14:55:07  peachey
Use new boolean parameter "sametype" (default false) to allow user
to write the output with the same data type as the input.


******************************************************************************/
