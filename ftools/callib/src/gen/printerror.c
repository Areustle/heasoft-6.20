/* 
Function:

     Printerror


Description:

     This routine gets the error description associated with the input error
     status and writes the error messages on the stack to the screen.


Functions called:

   FITS functions:
     fits_get_errstatus  -- gets the error desctiption
     fits_read_errstatus -- reads the error messages from the error stack


Author and modification history:

     Dr. Bill Pence 


Usage:

     void Printerror(int status)

     Input:
        status    -- i -- error status
 
     Include Files:
        general.h
	

----------------------------------------------------------------------------- */

#include "general.h"

void Printerror(int status)
{
  char status_str[FLEN_STATUS], errmsg[FLEN_ERRMSG];

  if (status)
    fprintf(stderr, "\n*** Error occured during program execution ***\n");
  
  fits_get_errstatus(status, status_str);    /* get the error description */
  fprintf(stderr, "\nstatus = %d: %s\n", status, status_str);

  /* get first message; null if stack is empty */
  if (fits_read_errmsg(errmsg))
    {
      fprintf(stderr, "\nError message stack:\n");
      fprintf(stderr, " %s\n", errmsg);

      while (fits_read_errmsg(errmsg))   /* get remaining messages */
         fprintf(stderr, "%s\n", errmsg);
    }

  exit(status);    /* terminate the program, returning error status */
}

