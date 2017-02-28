/* 
-------- Function: ------------------------------------------------------------

     CreateEboundExt


-------- Description: ---------------------------------------------------------

     This function creates the EBOUNDS extension for an RMF/PHA file
     in one of the formats conforming to the HDUVERS2='1.*.*' family.
     Currently the following formats are supported (see OGIP/92-002a)
     HDUVERS2 = '1.0.0'
     HDUVERS2 = '1.1.0'
     HDUVERS2 = '1.2.0'
     but HDUVERS2 = '1.0.0' & '1.1.0' will be overridden such that '1.2.0' is
     written.

   Assumptions:
   
      The FITS is open and has had the Primary Header written
        !!! Note !!!! File is left open at the end  
        and  MUST BE CLOSED               by fits_close_file 
        or   ANOTHER EXTENSION ADDED      by fits_create_table
        in order to (automatically) write the mandatory END header keyword.


-------- Functions called: ----------------------------------------------------

   FITS functions:
      fits_create_table    : (FITSIO) Creates a new FITS extension file
      fits_write_col       : (FITSIO) Writes the data
      fits_write_comment   : (FITSIO) Writes a FITS comment keyword  
      fit_write_history    : (FITSIO) Writes a FITS history keyword
      fits_write_key       : (FITSIO) Writes a keyword
      fits_write_key_lng   : (FITSIO) Writes a keyword in LONG format
      fits_write_key_str   : (FITSIO) Writes a keyword in STRING format

   General Utility Functions:
      DispMsg      -- displays messages
      Printerror   -- prints FITSIO error messages
      PrintWarning -- prints FITSIO warnings


-------- Usage: ---------------------------------------------------------------
   int CreateEboundsExt1(fitsfile *fptr, int chatter, int nk_history, 
                         char **history, int nk_comm, char **comment, 
			 char *rmfversn, char *telescop, char *instrume, 
			 char *detnam, char *filter, float areascal, 
			 char *chantype, int fchan, int iebound, float *e_min, 
			 float *e_max)
   Input Parameters:
       FPTR          fitsfile : Pointer to the output FITS file
       CHATTER       int      : Chattiness flag for o/p (5 quite,10 normal,
                                >20 silly)
       NK_HISTORY    int      : No. records to be written as HISTORY records
       HISTORY       char     : Array of history strings to be written
       NK_COMM       int      : No. records to be written as COMMENT records
       COMMENT       char     : Array of comment strings to be written
       RMFVERSN      char     : String denoting OGIP HDUVERS2 family  
       TELESCOP      char     : String listing telescope/mission
       INSTRUME      char     : String listing instrument/detector
       DETNAM        char     : String listing specific detector name
       FILTER        char     : String listing instrument filter in use
       AREASCAL      float    : Area scaling factor
       CHANTYPE      char     : Type of detector channels in use (PHA, PI)
       FCHAN         int      : No. ("name") of first channel (usually 0 or 1)
       IEBOUND       int      : No. channels in the full array
       E_MIN         float    : Array containing min nominal energy bound to 
                                each chan
       E_MAX         float    : Array containing max nominal energy bound to 
                                each chan

   Output Parameters:
       STATUS        int         : 0=OK.


-------- User i/ps required (prompted for): -----------------------------------

   None


-------- Include files --------------------------------------------------------

   <stdio.h>
   <stdlib.h>
   <string.h>
   "general.h"


-------- Origin: --------------------------------------------------------------
   This function is basically a C version of wtebd3.f.


-------- Authors/Modification History: ----------------------------------------
   Sandhia Bansal  (1.0.0; Jul 97)

---------------------------------------------------------------------------- */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "general.h"


int CreateEboundsExt1(fitsfile *fptr, int chatter, int nk_history, 
		      char **history, int nk_comm, char **comment, 
		      char *rmfversn, char *telescop, char *instrume,
		      char *detnam, char *filter, float areascal, 
		      char *chantype, int fchan, int iebound, 
		      float *e_min, float *e_max)
{
   long nrows=(long) iebound; 
   long firstrow=1, firstelem=1;
   long *ichan = (long *) malloc(iebound*sizeof(long));

   int  status=0;
   int  tfields=3;
   int  tlmax=fchan+iebound-1;
   int  i=0, itemp=0;

   char *version="1.0.0";
   char errstr[FLEN_ERRMSG];
   char wrnstr[FLEN_ERRMSG];
   char message[FLEN_ERRMSG];
   char hduvers2[6];
   char *ttype[] = {"CHANNEL", "E_MIN", "E_MAX"};
   char *tform[] = {"J",       "E",     "E"    };
   char *tunit[] = {" ",       "keV",   "keV"  };
   char *extname = "EBOUNDS";
   char *progname = "CreateEboundsExt1";
   char comm[FLEN_COMMENT];

   /* Initializations */
   strcpy(errstr, "** ");
   strcat(strcat(strcat(strcat(errstr, progname), " "), version), " ERROR: ");

   strcpy(wrnstr, "** ");
   strcat(strcat(strcat(strcat(wrnstr, progname), " "), version), " WARNING: ");
   
   /* Give user info if requested */
   strcpy(message, " ... using ");
   strcat(strcat(strcat(message, progname), " "), version);
   DispMsg(chatter, 15, message);

   if (rmfversn[0] != '1')
   {
      strcpy(message, wrnstr);
      strcat(message, " Format/subroutine mismatch");
      DispMsg(1, 1, message);
      
      strcpy(message, 
	     " ...... This routine writes only the 1.*.* family of formats");
      DispMsg(chatter, 1, message);
      
      strcpy(message, " ...... Requested Format: ");
      strcat(message, rmfversn);
      DispMsg(chatter, 1, message);
      
      status = 15;
   }
   
   if (status == 0)
   {
      /* Check that we know the format, and override if an old format */
      if (strcmp(rmfversn, "1.2.0")==0)
	 strcpy(hduvers2, rmfversn);

      else if ((strcmp(rmfversn, "1.0.0")==0) || (strcmp(rmfversn, "1.1.0")==0))
      {
	 strcpy(hduvers2, "1.2.0");

	 strcpy(message, wrnstr);
	 strcat(strcat(message, " Old format requested: "), rmfversn);
	 DispMsg(chatter, 1, message);

	 strcpy(message, "Resetting format (HDUVERS2) to ");
	 strcat(message, hduvers2);
	 DispMsg(chatter, 1, message);
      }

      else
      {
	 strcpy(hduvers2, "1.2.0");

	 strcpy(message, wrnstr);
	 strcat(strcat(message, " Unknown format: "), rmfversn);
	 DispMsg(chatter, 1, message);

	 strcpy(message, " ...... Resetting format (HDUVERS2) to ");
	 strcat(message, hduvers2);
	 DispMsg(chatter, 1, message);
      }

      /* Create a new extension with required keywords */

      if (fits_create_tbl(fptr, BINARY_TBL, nrows, tfields, ttype, tform, tunit,
			  extname, &status))
      {
	 DispMsg(1, 1, wrnstr);
	 Printerror(status);
      }
	 
      if (status == 0)
      {
	 strcpy(message, " ... new extension created");
	 DispMsg(chatter, 15, message);

	 strcpy(message, " ... written the extension header keywords");
	 DispMsg(chatter, 15, message);

	 /* Write the HDUCLASn & HDUVERSn keywords */
	 if (fits_write_key(fptr, TSTRING, "HDUCLASS", "OGIP",
			    "Format confirms to OGIP standard", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing HDUCLASS keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (fits_write_key(fptr, TSTRING, "HDUCLAS1", "RESPONSE",
			    "dataset relates to spectral response", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing HDUCLAS1 keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (fits_write_key(fptr, TSTRING, "HDUVERS1", "1.0.0",
			    "Version of family of formats", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing HDUVERS1 keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (fits_write_key(fptr, TSTRING, "HDUCLAS2", "EBOUNDS",
			    "Nominal energies of PHA chan boundaries", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing HDUCLAS2 keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (fits_write_key(fptr, TSTRING, "HDUVERS2", hduvers2,
			    "Version of format (OGIP memo CAL/GEN/92-002a)", 
			    &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing HDUVERS2 keyword", 1, 1,
			 status);
	    status = 0;
	 }
	 
	 /* Add the other (passed) OGIP required keywords */
	 if (fits_write_key(fptr, TSTRING, "TELESCOP", telescop,
			    "Mission/Satellite name", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing TELESCOP keyword", 1, 1,
			 status);
	    status = 0;
	 }
	 
	 if (fits_write_key(fptr, TSTRING, "INSTRUME", instrume,
			    "Instrument/Detector name", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing INSTRUME keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (fits_write_key(fptr, TSTRING, "DETNAM", detnam,
			    "Specific detector name in use", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing DETNAM keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (fits_write_key(fptr, TSTRING, "FILTER", filter,
			    "filter in use", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing FILTER keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (fits_write_key_lng(fptr, "DETCHANS", iebound,
				"Total number of detector channels", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing DETCHANS keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if ((strcmp(chantype, "PHA")) && (strcmp(chantype, "PI")))
	    strcpy(comm, "WARNING This is NOT an OGIP-approved value");
	 else
	    strcpy(comm, "Detector Channel Type in use (PHA or PI)");

	 if (fits_write_key_str(fptr, "CHANTYPE", chantype, comm, &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing CHANTYPE keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (fits_write_key(fptr, TFLOAT, "EFFAREA", &areascal,
			    "Area scaling factor", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing ERRAREA keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (fits_write_key(fptr, TSTRING, "RMFVERSN", "1992a",
			    "OGIP classification of FITS format", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing RMFVERSN keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (fits_write_key_lng(fptr, "TLMIN1", fchan,
				"Minimum value legally allowed in column 1", 
				&status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing TLMIN1 keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (fits_write_key_lng(fptr, "TLMAX1", tlmax,
				"Maximum value legally allowed in column 1", 
				&status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing TLMAX1 keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 strcpy(message, " ... Written the OGIP required keywords");
	 DispMsg(chatter, 15, message);

	 /* Add the (passed) history cards, adding one related to this program */
	 i=0, itemp=0;
	 while (i<nk_history)
	 {
	    if (fits_write_history(fptr, history[i++], &status))
	    {
	       itemp = status;
	       strcpy(comm, " - (missing record) fitsio illegal character ?");
	       status = 0;
	       fits_write_history(fptr, comm, &status);
	    }
	 }

	 strcpy(comm, "FITS EBOUNDS extension written by ");
	 strcat(strcat(strcat(comm, progname), " "), version);
	 fits_write_history(fptr, comm, &status);

	 if ((itemp != 0) || (status != 0))
	    PrintWarning(progname, version, 
			 "Error while writing at least one History record", 
			 1, 1, status);
	 
	 status = 0;

	 strcpy(message, " ... Written the history keywords");
	 DispMsg(chatter, 15, message);

	 /* Add the (passed) comment cards */
	 i=0, itemp=0;
	 while (i<nk_comm)
	 {
	    if (fits_write_comment(fptr, comment[i++], &status))
	    {
	       itemp = status;
	       strcpy(comm, " - (missing record) fitsio illegal character ?");
	       status = 0;
	       fits_write_history(fptr, comm, &status);
	    }
	 }

	 if (itemp != 0)
	    PrintWarning(progname, version, 
			 "Error while writing at least one Comment record", 
			 1, 1, status);

	 strcpy(message, " ... Written the comment keywords");
	 DispMsg(chatter, 15, message);
      
	 /* Write the data */
	 for (i=0; i<iebound; i++)
	    ichan[i] = (long) (fchan + i);

	 if (fits_write_col(fptr, TLONG, 1, firstrow, firstelem, nrows,
			    ichan, &status))
	    Printerror(status);

	 if (fits_write_col(fptr, TFLOAT, 2, firstrow, firstelem, nrows,
			    e_min, &status))
	    Printerror(status);

	 if (fits_write_col(fptr, TFLOAT, 3, firstrow, firstelem, nrows,
			    e_max, &status))
	    Printerror(status); 

	 strcpy(message, " ... Written the data");
	 DispMsg(chatter, 15, message);
      }

      else
      {
	 strcpy(message, errstr);
	 DispMsg(1, 1, message);
	 strcpy(message, "FATAL: Extension not written");
	 DispMsg(1, 1, message);
      }
   }
   
   free(ichan);

   return(status); 
}






