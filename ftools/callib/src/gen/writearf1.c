/* 
-------- Function: -------------------------------------------------------------

     WriteArf1


-------- Description: ----------------------------------------------------------

     This subroutine Creates and Writes the SPECRESP extension for an ARF file 
     in one of the formats conforming to the HDUVERS2='1.*.*' family.

     Currently the following formats are supported (see OGIP/92-002a)
     HDUVERS2 = '1.0.0'
     HDUVERS2 = '1.1.0'

     The requested format is checked, and if belonging to the '1.*.*' family,
     but not included above, the extension is written in the last format listed.

   Assumptions:
   
      The FITS is open and has had the Primary Header written.
      The file is wound to the desired location.
        !!! Note !!!! File is left open at the end  
        and  MUST BE CLOSED               by fits_close_file 
        or   ANOTHER EXTENSION ADDED      by fits_create_table
        in order to (automatically) write the mandatory END header keyword.


-------- Functions called: -----------------------------------------------------

   FITS functions:
      fits_create_table    : (FITSIO) Creates a new FITS extension file
      fits_write_col       : (FITSIO) Writes the data
      fits_write_comment   : (FITSIO) Writes a FITS comment keyword  
      fit_write_history    : (FITSIO) Writes a FITS history keyword
      fits_write_key       : (FITSIO) Writes a keyword

   General Utility Functions:
      DispMsg      -- displays messages
      Printerror   -- prints FITSIO error messages
      PrintWarning -- prints FITSIO warnings


-------- Usage: ----------------------------------------------------------------

   int WriteArf1(fitsfile *fptr, int chatter, int nk_history, char **history, 
                 int nk_comm, char **comment, char *arfversn, char *phafil, 
		 char *telescop, char *instrume, char *detnam, char *filter, 
		 int ienerg, float *energ_lo, float *energ_hi, float *sprsp)

       FPTR          fitsfile  : Pointer to the output FITS file
       CHATTER       int       : Chattiness flag for o/p (5 quite,10 normal,
                                 >20 silly)
       NK_HISTORY    int       : No. records to be written as HISTORY records
       HISTORY       char      : Array of history strings to be written
       NK_COMM       int       : No. records to be written as COMMENT records
       COMMENT       char      : Array of comment strings to be written
       ARFVERSN      char      : String denoting OGIP HDUVERS2 family  
       PHAFIL        char      : Name of the PHA file for which this ARF is 
                                 created
       TELESCOP      char      : String listing telescope/mission
       INSTRUME      char      : String listing instrument/detector
       DETNAM        char      : String listing specific detector name
       FILTER        char      : String listing instrument filter in use
       IENERG        int       : No. energy bins
       ENERG_LO      float     : Array containing lower bound to each energy bin
       ENERG_HI      float     : Array containing upper bound to each energy bin
       SPRSP         float     : Array containing the specresp dataset

   Output Parameters:
       STATUS        int       : 0=OK.


-------- Include files ---------------------------------------------------------

   <stdio.h>
   <stdlib.h>
   <string.h>
   "general.h"


-------- Origin: ---------------------------------------------------------------

   This function is basically a C version of wtarf1.f.


-------- Authors/Modification History: -----------------------------------------

   Sandhia Bansal  (1.0.0; Jul 97)

----------------------------------------------------------------------------- */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "general.h"



int WriteArf1(fitsfile *fptr, int chatter, int nk_history, char **history, 
	      int nk_comm, char **comment, char *arfversn, char *phafil, 
	      char *telescop, char *instrume, char *detnam, char *filter, 
	      int ienerg, float *energ_lo, float *energ_hi, float *sprsp)
{

   float *values;

   long nrows=(long) ienerg; 
   long firstrow=1, firstelem=1;

   int  status=0;
   int  tfields=3;
   int  i=0, ie=0;
   int  itemp=0;

   char *progname = "WriteArf1";
   char *version="1.0.0";
   char errstr[FLEN_ERRMSG];
   char wrnstr[FLEN_ERRMSG];
   char message[FLEN_ERRMSG];
   char hduvers2[6];
   char *ttype[] = {"ENERG_LO", "ENERG_HI", "SPECRESP"};
   char *tform[] = {"E",        "E",        "E"       };
   char *tunit[] = {"keV",      "keV",      "cm**2"   };
   char *extname = "SPECRESP";
   char comm[FLEN_COMMENT];


   /* Initializations */
   strcpy(wrnstr, "** ");
   strcat(strcat(strcat(strcat(wrnstr, progname), " "), version), " WARNING: ");
   strcpy(errstr, "** ");
   strcat(strcat(strcat(strcat(errstr, progname), " "), version), " ERROR: ");

   /* Give user info if requested */
   strcat(strcat(strcat(strcpy(message, " ... using "), progname), " Version "),
	  version);
   DispMsg(chatter, 20, message);

   if (arfversn[0] != '1')
   {
      strcat(strcpy(message, wrnstr), " Format/subroutine mismatch");
      DispMsg(1, 1, message);
      
      strcpy(message, 
	     " ...... This routine writes only the 1.*.* family of formats");
      DispMsg(1, 1, message);
      
      strcat(strcpy(message, " ...... Requested Format: "), arfversn);
      DispMsg(1, 1, message);
      
      status = 15;
   }

   if (status == 0)
   {
      if (arfversn[0] != '1')
      {
	 strcpy(message, wrnstr);
	 strcat(message, " Format/subroutine mismatch");
	 DispMsg(1, 1, message);
      
	 strcpy(message, 
		" ...... This routine writes only the 1.*.* family of formats");
	 DispMsg(1, 1, message);
      
	 strcpy(message, " ...... Requested Format: ");
	 strcat(message, arfversn);
	 DispMsg(1, 1, message);
      
	 status = 15;
      }

      if (status == 0)
      {
	 if ((strcmp(arfversn, "1.0.0")==0) || (strcmp(arfversn, "1.1.0")==0))
	    strcpy(hduvers2, arfversn);
	 
	 else
	 {
	    strcpy(hduvers2, "1.1.0");
	    
	    strcpy(message, "Unknown format: ");
	    strcat(message, arfversn);
	    PrintWarning(progname, version, message, 1, 1, 1);

	    strcpy(message, "Resetting format (HDUVERS2) to ");
	    strcat(message, hduvers2);
	    DispMsg(1, 1, message);
	 }

	 /* Create a new extension */
	 if (fits_create_tbl(fptr, BINARY_TBL, nrows, tfields, ttype, tform, 
			     tunit, extname, &status))
	 {
	    DispMsg(1, 1, "Error while creating ARF extension");
	    Printerror(status);
	 }

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

	 if (fits_write_key(fptr, TSTRING, "HDUCLAS2", "SPECRESP",
			    "Dataset contains spectral response", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing HDUCLAS2 keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (fits_write_key(fptr, TSTRING, "HDUVERS2", "1.1.0",
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

	 if (fits_write_key(fptr, TSTRING, "ARFVERSN", "1992a",
			    "OGIP classification of FITS format", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing ARFVERSN keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 if (strcmp(phafil, " ") == 0)
	    strcpy(phafil, "UNKNOWN");
	 if (fits_write_key(fptr, TSTRING, "PHAFILE", phafil,
			    "PHA file for which this ARF is created", &status))
	 {
	    PrintWarning(progname, version, 
			 "Error while writing PHAFILE keyword", 1, 1,
			 status);
	    status = 0;
	 }

	 strcpy(message, " ... written the OGIP required keywords");
	 DispMsg(chatter, 20, message);

	 /* Add the (passed) history cards, adding one related to this 
	    program */
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

	 strcpy(comm, "FITS ARF extension written by ");
	 strcat(strcat(strcat(comm, progname), " "), version);
	 fits_write_history(fptr, comm, &status);

	 if ((itemp != 0) || (status != 0))
	    PrintWarning(progname, version, 
			 "Error while writing at least one History record", 
			 1, 1, status);
	 
	 status = 0;

	 strcpy(message, " ... Written the history keywords");
	 DispMsg(chatter, 20, message);

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
	 DispMsg(chatter, 20, message);
      
	 /* Write the data */
	 for (ie=0; ie<ienerg; ie++)
	 {
	    /* the energy bin */
	    if (fits_write_col(fptr, TFLOAT, 1, firstrow, firstelem, 1,
			       &energ_lo[ie], &status))
	       Printerror(status);

	    if (fits_write_col(fptr, TFLOAT, 2, firstrow, firstelem, 1,
			       &energ_hi[ie], &status))
	       Printerror(status);

	    /* the spectral response */
	    if (fits_write_col(fptr, TFLOAT, 3, firstrow, firstelem, 1,
			       &sprsp[ie], &status))
	       Printerror(status);
	    
	    firstrow++;
	 }
  
	 DispMsg(chatter, 20, " ... Written the data");

	 if (status != 0)
	 {
	    strcpy(message, errstr);
	    DispMsg(1, 1, message);
	    strcpy(message, "FATAL - aborting");
	    DispMsg(1, 1, message);
	 }

	 else
	 {
	    strcpy(message, " ... Successfully written the RSP_MATRIX data");
	    DispMsg(chatter, 15, message);
	 }
      }
   }

   return(status);
}
