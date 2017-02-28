#include "fitsio.h"

void printerror( int status)
{
    /*****************************************************/
    /* Print out cfitsio error messages and exit program */
    /*****************************************************/

    char status_str[FLEN_STATUS], errmsg[FLEN_ERRMSG], errorMsg[256];

    if (status)
    {
      sprintf( errorMsg, "\n*** Error occurred during program execution ***");
      c_fcecho( errorMsg );
    }

    fits_get_errstatus(status, status_str);   /* get the error description */
    sprintf( errorMsg, "\nstatus = %d: %s", status, status_str);
    c_fcecho( errorMsg );

    /* get first message; null if stack is empty */
    if ( fits_read_errmsg(errmsg) ) 
    {
         sprintf( errorMsg, "\nError message stack:");
         c_fcecho( errorMsg );
         sprintf( errorMsg, " %s", errmsg);
         c_fcecho( errorMsg );

         while ( fits_read_errmsg(errmsg) )  /* get remaining messages */
         {
             sprintf( errorMsg, " %s", errmsg);
             c_fcecho( errorMsg );
         }
    }

    exit( status );       /* terminate the program, returning error status */
}
