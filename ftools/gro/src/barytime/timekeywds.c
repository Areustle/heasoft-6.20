/**************************************************************************************
 *                              TIMEKEYWDS.C
 *
 *  Program: TIMEKEYWDS v1.0
 *  Date:    February 20, 2002
 *
 *  Programmer: Sandhia Bansal, SSAI, EGRET/GRO Project
 *
 *  Content: Writes keywords to the specified extension of a FITS file.  Assumes that
 *           the file is already open and will be closed by the calling program.
 *
 *  Called by:  WriteHeader
 *
 *  External calls:
 *   FITS_WRITE_KEY:  Writes specified keyword to the extension
 *
 *  Method:
 *   Write Key
 *   If Error
 *      Report and return to the calling program
 *
 *  Modification History:
 *************************************************************************************/

#include "smdb.h"


int TimeKeywds(fitsfile *fptr, char *extname)
{
   double  value=0.0;

   int     status=0;

   char    *msg=(char *) malloc(80*sizeof(char));




   /* Write some new keys to the extension */
   status = fits_write_key(fptr, TFLOAT, "MJDREF", &value, 
   	                   "Baycenter corrected time of measurement", &status);
   if (status != 0)
      Fcerr(strcat(strcat(strcpy(msg, 
	    "Error While writing MJDREF keyword to "), extname), " extension"));

   if (status == 0)
   {
      status = fits_write_key(fptr, TSTRING, "TIMESYS", "JD", 
			         "The time system is Julian Days", &status);
      if (status != 0)
         Fcerr(strcat(strcat(strcpy(msg, 
 	       "Error While writing TIMESYS keyword to "), extname), " extension"));
   }
    
   if (status == 0)
   { 
      status = fits_write_key(fptr, TSTRING, "TIMEUNIT", "d", 
   	                         "Physical unit for TSTART and TSTOP", &status);
      if (status != 0)
         Fcerr(strcat(strcat(strcpy(msg,
               "Error While writing TIMEUNIT keyword to "), extname), " extension"));
   }

   if (status == 0)
   {
      /* TSTART and TSTOP will be updated later */
      status = fits_write_key(fptr, TDOUBLE, "TSTART", &value, 
			      "Observation Start Time", &status);
      if (status != 0)
         Fcerr(strcat(strcat(strcpy(msg, 
	       "Error While writing TSTART keyword to "), extname), " extension"));
   }

   if (status == 0)
   {
      status = fits_write_key(fptr, TDOUBLE, "TSTOP", &value, "Observation End Time", 
	  		      &status);
      if (status != 0)
         Fcerr(strcat(strcat(strcpy(msg, 
	       "Error While writing TSTOP keyword to "), extname), " extension"));
   }

   return status;
}
