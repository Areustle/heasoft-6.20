/* 
Function:

     PrintWarning


Description:

     This routine gets the error description associated with the input error
     status and displays it on the screen.  It also writes the input warning
     to the screen.


Functions called:

   FITS functions:
     fits_get_errstatus  -- gets the error desctiption
     fits_get_version    -- gets the version number of the FITSIO

   genereal utility functions:
     DispMsg -- writes the input message to the screen for a large enough
                value of the chatter flag

   macro:
     Min -- returns the minimum of the two input values


Author and modification history:

     Sandhia Bansal (July 1997) 


Usage:

     void PrintWarning(char *program, char *version, char *message, int chatter, 
                       int wtChatter, int status)

     Input:
        program   -- char * -- name of the calling program
	version   -- char * -- version number of the calling program
	message   -- char * -- warning to be printed to the screen
	chatter   -- int    -- chatter flag
	wtchatter -- int    -- if chatter > wtchatter, print the message
        status    -- int    -- error status
 
     Include Files:

        <stdio.h>
	<stdlib.h>
	<string.h>
        "general.h"
	

----------------------------------------------------------------------------- */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "general.h"



void PrintWarning(char *program, char *version, char *message, int chatter, 
		  int wtChatter, int status)
{
   float fversion;

   int   ilen;
   int   i;

   char  status_str[FLEN_STATUS], wrnstr[FLEN_ERRMSG];
   char  cversion[20];

   if (status != 0)
   {
      if (chatter >= wtChatter)
      {
	 strcpy(wrnstr, "** ");
	 strcat(wrnstr, program);
	 strcat(wrnstr, " ");
	 strcat(wrnstr, version);
	 strcat(wrnstr, " WARNING: **");

	 DispMsg(1, 1, wrnstr);

	 ilen = Min(strlen(message), (FLEN_ERRMSG-1));
	 while (ilen > 0)
	 {
	    for (i=0; i<FLEN_ERRMSG-1; i++)
	        wrnstr[i] = ' ';
	    wrnstr[FLEN_ERRMSG] = '\0';
	    strncpy(wrnstr, message, ilen);

	    if (strlen(message) >= FLEN_ERRMSG)
	       wrnstr[FLEN_ERRMSG-1] = '\0';

	    fprintf(stderr, "%s\n", wrnstr);
	    message = &message[ilen];
	    ilen = Min(strlen(message), (FLEN_ERRMSG-1));
	 }

	 if (status > 100)         /* do this only for FITSIO errors */
	 {
	    /* get the error description */
	    fits_get_errstatus(status, status_str);    
	    fits_get_version(&fversion);

	    strcpy(wrnstr, "fitsio ");
	    sprintf(cversion, "%5.2f", fversion);
	    strcat(wrnstr, cversion);
	    strcat(wrnstr, " error message: ");
	    fprintf(stderr, "\n%s", wrnstr);
	    fprintf(stderr, "\nstatus = %d: %s\n", status, status_str);
	 }
      }
   }
}

