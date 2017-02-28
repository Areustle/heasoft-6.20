/* 
-------- Function: -------------------------------------------------------------

     WriteTypeIPha


-------- Description: ----------------------------------------------------------

     This subroutine writes the SPECTRUM extension for a PHA file of type I
     in one of the formats conforming to the HDUVERS1='1.*.*' family. 
     Currently the following formats are supported (see OGIP/92-007a)
     HDUVERS1 = '1.0.0' 
     HDUVERS1 = '1.1.0'
     HDUVERS  = '1.1.0'
     The requested format is checked, and if belonging to the '1.*.*' family,
     but not included above, the extension is written in the last format listed.

   Assumptions:
   
      The FITS is open and has had the Primary Header written.  Also the
      file is assumed to be wound to the desired location.  

      !!! Note !!!! File is left open at the end  
      and  MUST BE CLOSED               by fits_close_file 
      or   ANOTHER EXTENSION ADDED      by fits_create_table
      in order to (automatically) write the mandatory END header keyword.

      In all cases, the 1.*.* family of formats consists of a BINTABLE 
      extension, with the number of rows equal to the number of channels 
      passed (this does not have to be the total number of detector channels 
      the instrument is capable of).


-------- Functions called: -----------------------------------------------------
 
   FITS functions:
      fits_create_table       : (FITSIO) Creates a new FITS extension file
      fits_modify_comment     : (FITSIO) Modifies the comment field for an 
                                         existing column
      fits_write_key          : (FITSIO) Writes a keyword
      fits_uppercase          : (FITSIO) Converts the input key to uppercase
      fits_write_col          : (FITSIO) Writes the data
      fits_write_comment      : (FITSIO) Writes a FITS comment keyword  
      fit_write_history       : (FITSIO) Writes a FITS history keyword
      fits_write_key_dbl      : (FITSIO) Writes a keyword in DOUBLE format
      fits_write_key_fixflt   : (FITSIO) Writes a keyword in FIXED FLOAT forma
      fits_write_key_lng      : (FITSIO) Writes a keyword in LONG format
      fits_write_key_log      : (FITSIO) Writes a keyword in LOGICAL format

   General Utility Functions:
      Printerror   -- prints FITSIO error messages
      DispMsg      -- Prints the input error message to the screen


-------- Usage: ----------------------------------------------------------------

   int WriteTypeIPha(fitsfile *fptr, int chatter, int nk_history, 
                     char **history, int nk_comm, char **comment, 
		     char *telescop, char *instrume, char *detnam, 
		     char *filter, char *phaversn, char *hduclas2, 
		     long fchan, double texpos, float areascal, 
		     char *backfil, float backscal, char *corrfil, 
		     float corrscal, char *respfil, char *ancrfil, 
		     long detchans, char *chantyp, short *channel, 
		     float *counts, int dtype, int qerror, float *serr, 
		     int qsys, float *syserr, int qqual, short *quality, 
		     int qgroup, short *grping, long nchan)

       FPTR          fitsfile  : Pointer to the output FITS file
       CHATTER       int       : Chattiness flag for o/p (5 quite,10 normal,
                                 >20 silly)
       NK_HISTORY    int       : No. records to be written as HISTORY records
       HISTORY       char      : Array of history strings to be written
       NK_COMM       int       : No. records to be written as COMMENT records
       COMMENT       char      : Array of comment strings to be written
       TELESCOP      char      : String listing telescope/mission
       INSTRUME      char      : String listing instrument/detector
       DETNAM        char      : String listing specific detector name
       FILTER        char      : String listing instrument filter in use
       PHAVERSN      char      : String denoting OGIP HDUVERS family
       HDUCLAS2      char      : String containing HDUCLAS2 value
       FCHAN         long      : First legal channel number (ie 0, 1 etc)
       TEXPOS        double    : Exposure (Live) time
       AREASCAL      float     : Area scaling factor
       BACKFIL       char      : Associated background filename
       BACKSCAL      float     : Background scaling factor
       CORRFIL       char      : Associated correction filename
       CORRSCAL      float     : Correction scaling factor
       RESPFIL       char      : Detector redistribution matrix file (RMF)
       ANCRFIL       char      : Ancillary response file (ARF)
       DETCHANS      long      : Total number of possible detector channels 
       CHANTYP       char      : Type of detector channels (PHA, PI etc)
       CHANNEL       short     : Array of detector channel numbers
       COUNTS        float     : Array of obs'd counts (or count rate) per chan
       DTYPE         int       : Flag to denote counts (1) or count rates (2)
       QERROR        int       : Flag as to whether stat errors passed down
       SERR          float     : Array of statistical errors on COUNTS 
                                 (for QERROR=T)
       QSYS          int       : Flag as to whether systematic errors passed 
                                 down
       SYSERR        float     : Array of systematic errors on COUNTS 
                                 (for QSYS=T)
       QQUAL         int       : Flag as to whether quality array passed down
       QUALTY        short     : Array of quality flags (for QQUAL=T)
       QGROUP        int       : Flag as to whether grouping array passed down
       GRPING        short     : Array of quality flags (for QGROUP=T)
       NCHAN	     long      : No. channels actually passed down

   Output Parameters:
       STATUS        int       : 0=OK.


-------- Include files ---------------------------------------------------------

   <stdio.h>
   <stdlib.h>
   <string.h>
   "general.h"


-------- Origin: ---------------------------------------------------------------

   This function is basically a C version of wtpha1.f.


-------- Authors/Modification History: -----------------------------------------
   kaa             (1.0.1; Dec 4 98) Added support for 1.2.0 format.

   Sandhia Bansal  (1.0.0; Jul 97)

----------------------------------------------------------------------------- */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "general.h"


int WriteTypeIPha(fitsfile *fptr, int chatter, int nk_history, char **history, 
		  int nk_comm, char **comment, char *telescop, char *instrume, 
		  char *detnam, char *filter, char *phaversn, char *hduclas2, 
		  long fchan, double texpos, float areascal, char *backfil, 
		  float backscal, char *corrfil, float corrscal, char *respfil, 
		  char *ancrfil, long detchans, char *chantyp, short *channel, 
		  float *counts, int dtype, int qerror, float *serr, int qsys, 
		  float *syserr, int qqual, short *quality, int qgroup, 
		  short *grping, long nchan)
{
   long  nrows=nchan; 
   long  firstrow=1L, firstelem=1L;
   long  *conv4 = (long *) malloc(nchan*sizeof(long));
   long  lkeyval;
   long  tlmin=fchan;
   long  tlmax=fchan+detchans-1L;

   int   status=0;
   int   i;
   int   tfields=2;
   int   ikeyval;
   int   colnum;

   short *conv2 = (short *) malloc(nchan*sizeof(short));

   char  version[7];
   char  errstr[FLEN_ERRMSG];
   char  wrnstr[FLEN_ERRMSG];
   char  message[FLEN_ERRMSG];
   char  hduvers[6];
   char  *ttype[NFIELDS_I];
   char  *tform[NFIELDS_I];
   char  *tunit[NFIELDS_I];
   char  *tcomm[NFIELDS_I];
   char  extname[FLEN_VALUE];
   char  keyname[FLEN_KEYWORD];
   char  ckeyval[FLEN_KEYWORD];
   
   /* Initializations */
   strcpy(version, "1.0.0");
   strcpy(extname, "SPECTRUM");

   strcpy(errstr, "** WriteTypeIPha ");
   strcat(strcat(errstr, version), " ERROR: ");

   strcpy(wrnstr, "** WriteTypeIPha ");
   strcat(strcat(wrnstr, version), " WARNING: ");
   
   /* Give user info if requested */
   strcpy(message, " ... using WriteTypeIPha Version ");
   strcat(message, version);
   DispMsg(chatter, 15, message);

   if (phaversn[0] != '1')
   {
      strcpy(message, wrnstr);
      strcat(message, " Format/subroutine mismatch");
      DispMsg(1, 1, message);
   
      DispMsg(1, 1, 
	      " ...... This routine writes only the 1.*.* family of formats");
      
      strcpy(message, " ...... Requested Format: ");
      strcat(message, phaversn);
      DispMsg(1, 1, message);
      
      status = 15;
   }

   if (status == 0)
   {
      if ( !strcmp(phaversn, "1.0.0") || !strcmp(phaversn, "1.1.0") )
	 strcpy(hduvers, phaversn);
      else
      {
	 strcpy(hduvers, "1.1.0");
	 strcpy(message, wrnstr);
	 strcat(strcat(message, " Unknown format: "), phaversn);
	 DispMsg(1, 1, message);

	 strcpy(message, " ...... Resetting format (HDUVERS) to ");
	 strcat(message, hduvers);
	 DispMsg(1, 1, message);
     }

      /* Create a new extension with required keywords */
      for (i=0; i<NFIELDS_I; i++)
      {
	 ttype[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
	 tform[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
	 tunit[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
	 tcomm[i] = (char *) malloc(FLEN_CARD*sizeof(char));
      }

      strcpy(ttype[0], "CHANNEL");
      strcpy(tform[0], "I");
      strcpy(tunit[0], " ");
      if ((strcmp(chantyp, "PHA") == 0) || (strcmp(chantyp, "pha") == 0))
	 strcpy(tcomm[0], "Pulse Height Analyser (PHA) Channel ");
      else if ((strcmp(chantyp, "PI") == 0) || (strcmp(chantyp, "pi") == 0))
	 strcpy(tcomm[0], "Pulse Invarient (PI) Channel ");
      else
	 strcpy(tcomm[0], "Detector channel (type unknown)");

      if (dtype == 1)
      {
	 strcpy(ttype[1], "COUNTS");
	 strcpy(tform[1], "J");
	 strcpy(tunit[1], "count");
	 strcpy(tcomm[1], "Counts per channel");
      }
      else if (dtype == 2)
      {
	 strcpy(ttype[1], "RATE");
	 strcpy(tform[1], "E");
	 strcpy(tunit[1], "count/s");
	 strcpy(tcomm[1], "Counts per second per channel");
      }

      if (qerror != 0)
      {
	 strcpy(ttype[tfields], "STAT_ERR");
	 strcpy(tform[tfields], "E");
	 if (dtype == 1)
	 {
	    strcpy(tunit[tfields], "count");
	    strcpy(tcomm[tfields], "Statistical error on COUNTS");
	 }
	 else if (dtype == 2)
	 {
	    strcpy(tunit[tfields], "count/s");
	    strcpy(tcomm[tfields], "Statistical error on RATE");
	 }
	 tfields++;
      }

      if (qsys != 0)
      {
	 strcpy(ttype[tfields], "SYS_ERR");
	 strcpy(tform[tfields], "E");
	 strcpy(tunit[tfields], " ");
	 if (dtype == 1)
	 {
	    strcpy(tcomm[tfields], "Fractional systematic errors on COUNTS");
	 }
	 else if (dtype == 2)
	 {
	    strcpy(tcomm[tfields], "Fractional systematic errors on RATE");
	 }
	 tfields++;
      }

      if (qqual != 0)
      {
	 strcpy(ttype[tfields], "QUALITY");
	 strcpy(tform[tfields], "I");
	 strcpy(tunit[tfields], " ");
	 strcpy(tcomm[tfields], "Quality flag of this channel (0=good)");
	 tfields++;
      }

      if (qgroup != 0)
      {
	 strcpy(ttype[tfields], "GROUPING");
	 strcpy(tform[tfields], "I");
	 strcpy(tunit[tfields], " ");
	 strcpy(tcomm[tfields], "Grouping flag of this channel (0=undefined)");
	 tfields++;
      }

      if (fits_create_tbl(fptr, BINARY_TBL, nrows, tfields, ttype, tform, tunit,
			  extname, &status))
	 Printerror(status);

      DispMsg(chatter, 20, " ... new extension created");
      DispMsg(chatter, 20, " ... written the extension header keywords");

      /* Modify the comments in the TTYPE fields */
      for (i=0; i<tfields; i++)
      {
	 sprintf(keyname, "TTYPE%d", i+1);
	 if (fits_modify_comment(fptr, keyname, tcomm[i], &status))
	    Printerror(status);
      }

      for (i=0; i<NFIELDS_I; i++)
      {
	 free(ttype[i]);
	 free(tform[i]);
	 free(tunit[i]);
	 free(tcomm[i]);
      }

      /* Write the HDUCLASn and HDUVERS keywords */
      
      if (fits_write_key(fptr, TSTRING, "HDUCLASS", "OGIP",
			 "Format confirms to OGIP standard", &status))
	 Printerror(status);

      if (fits_write_key(fptr, TSTRING, "HDUCLAS1", "SPECTRUM",
			 "PHA dataset (OGIP memo OGIP-92-007)", &status))
	 Printerror(status);

      if ( !strcmp(hduvers,"1.0.0") ) {

         if (fits_write_key(fptr, TSTRING, "HDUVERS1", hduvers,
	                    "Version of format (OGIP memo OGIP-92-007a)", 
                            &status))
	 Printerror(status);

      } 
      else 
      {

         if (fits_write_key(fptr, TSTRING, "HDUVERS1", hduvers,
	                    "Obsolete - included for backwards compatibility", 
                            &status))
	 Printerror(status);

         if (fits_write_key(fptr, TSTRING, "HDUVERS", hduvers,
	                    "Version of format (OGIP memo OGIP-92-007a)", 
                            &status))
	 Printerror(status);

      }


      /* convert hduclas2 to uppercase */
      fits_uppercase(hduclas2);

      if (strcmp(hduclas2, "TOTAL") == 0)	 
	 strcpy(message, "Gross PHA Spectrum (source + bkgd)");

      else if (strcmp(hduclas2, "NET") == 0)
	 strcpy(message, "Bkgd-subtracted PHA Spectrum");

      else if (strcmp(hduclas2, "BKG") == 0)
	 strcpy(message, "Bkgd PHA Spectrum");

      else if (strcmp(hduclas2, "UNKNOWN") == 0)
	 strcpy(message, "Maybe TOTAL, NET or BKG Spectrum");

      else
	 strcpy(message, "WARNING: This is NOT an OGIP-approved value");

      if (fits_write_key(fptr, TSTRING, "HDUCLAS2", hduclas2, message, &status))
	 Printerror(status);

      switch (dtype)
      {
         case 1:
	    strcpy(ckeyval, "COUNT");
	    strcpy(message, "PHA data stored as Counts (not count/s)");
	    break;

         case 2:
	    strcpy(ckeyval, "RATE");
	    strcpy(message, "PHA data stored as Counts/s");
	    break;

         default:
	    strcpy(ckeyval, "UNKNOWN");
	    strcpy(message, "Unknown storage method for PHA data");
	    break;
      }
      if (fits_write_key(fptr, TSTRING, "HDUCLAS3", ckeyval, message, &status))
	 Printerror(status);

      /* Write additional keywords describing data */
      if (fits_write_key_lng(fptr, "TLMIN1", fchan,
			     "Lowest legal channel number", &status))
	 Printerror(status);

      if (fits_write_key_lng(fptr, "TLMAX1", tlmax,
			     "Highest legal channel number", &status))
	 Printerror(status);

      /* Add the other (passed) OGIP required keywords */
      if (fits_write_key(fptr, TSTRING, "TELESCOP", telescop,
			 "Mission/Satellite name", &status))
	 Printerror(status);

      if (fits_write_key(fptr, TSTRING, "INSTRUME", instrume,
			 "Instrument/Detector name", &status))
	 Printerror(status);

      if (fits_write_key(fptr, TSTRING, "DETNAM", detnam,
			 "Specific detector name in use", &status))
	 Printerror(status);

      if (fits_write_key(fptr, TSTRING, "FILTER", filter,
			 "filter in use", &status))
	 Printerror(status);

      if (fits_write_key_dbl(fptr, "EXPOSURE", texpos, DECIMALS,
			     "exposure (in seconds)", &status))
	 Printerror(status);

      if (fits_write_key_fixflt(fptr, "AREASCAL", areascal, DECIMALS,
				"Area scaling factor", &status))
	 Printerror(status);

      if (strcmp(backfil, "none") == 0)
	 strcpy(backfil, "NONE");
      if (fits_write_key(fptr, TSTRING, "BACKFILE", backfil,
			 "Associated background filename", &status))
	 Printerror(status);

      if (fits_write_key_fixflt(fptr, "BACKSCAL", backscal, DECIMALS,
				"Background file scaling factor", &status))
	 Printerror(status);

      if (strcmp(corrfil, "none") == 0)
	 strcpy(corrfil, "NONE");
      if (fits_write_key(fptr, TSTRING, "CORRFILE", corrfil,
			 "Associated correction filename", &status))
	 Printerror(status);

      if (fits_write_key_fixflt(fptr, "CORRSCAL", corrscal, DECIMALS,
				"Correction file scaling factor", &status))
	 Printerror(status);

      if (fits_write_key(fptr, TSTRING, "RESPFILE", respfil,
			 "Associated redistrib matrix filename", &status))
	 Printerror(status);

      if (fits_write_key(fptr, TSTRING, "ANCRFILE", ancrfil,
			 "Associated ancillary response filename", &status))
	 Printerror(status);

      if (fits_write_key(fptr, TSTRING, "PHAVERSN", "1992a",
			 "OGIP classification of FITS format", &status))
	 Printerror(status);

      if (fits_write_key_lng(fptr, "DETCHANS", detchans,
			     "Total number of detector channels", &status))
	 Printerror(status);

      if (fits_write_key(fptr, TSTRING, "CHANTYPE", chantyp,
			 "channel type (PHA, PI etc)", &status))
	 Printerror(status);

      if ((dtype == 1) && (qerror == 0))
      {
	 ikeyval = 1;
	 if (fits_write_key_log(fptr, "POISSERR", ikeyval,
				"Poissonian errors to be assumed", &status))
	   Printerror(status);
      }

      else
      {
	 ikeyval = 0;
	 if (fits_write_key_log(fptr, "POISSERR", ikeyval,
				"Poissonian errors not applicable", &status))
	   Printerror(status);
      }

      if (qerror == 0)
      {
	 lkeyval = 0L;
	 if (fits_write_key_lng(fptr, "STAT_ERR", lkeyval,
				"no statistical error specified", &status))
	   Printerror(status);
      }

      if (qsys == 0)
      {
	 lkeyval = 0L;
	 if (fits_write_key_lng(fptr, "SYS_ERR", lkeyval,
				"no systematic error specified", &status))
	   Printerror(status);
      }

      if (qgroup == 0)
      {
	 lkeyval = 0L;
	 if (fits_write_key_lng(fptr, "GROUPING", lkeyval,
				"no grouping of the data is defined", &status))
	   Printerror(status);
      }

      if (qqual == 0)
      {
	 lkeyval = 0L;
	 if (fits_write_key_lng(fptr, "QUALITY", lkeyval,
				"no data quality information specified", 
				&status))
	   Printerror(status);
      }

      /* Add the (passed) history cards, adding one related to this program */
      i=0;
      while ((i<nk_history) && (status==0))
      {
	 if (fits_write_history(fptr, history[i++], &status))
	    Printerror(status);
      }

      strcpy(message, "FITS SPECTRUM extension written by WriteTypeIPha ");
      strcat(message, version);
      if (fits_write_history(fptr, message, &status))
	 Printerror(status);

      DispMsg(chatter, 20, " ... Written the history keywords");

      /* Add the (passed) comment cards */
      i=0;
      while ((i<nk_comm) && (status==0))
      {
	 if (fits_write_comment(fptr, comment[i++], &status))
	    Printerror(status);
      }

      DispMsg(chatter, 20, " ... Written the comment header keywords");
      DispMsg(chatter, 20, " ... Written the OGIP required keywords");

      /* Write the data */
      for (i=0; i<nchan; i++)
	 conv2[i] = channel[i]; 

      if (fits_write_col(fptr, TSHORT, 1, firstrow, firstelem, nrows,
			 conv2, &status))
	 Printerror(status);

      if (dtype == 1)
      {
	 for (i=0; i<nchan; i++)
	    conv4[i] = (long) counts[i]; 
	 if (fits_write_col(fptr, TLONG, 2, firstrow, firstelem, nrows,
			    conv4, &status))
	    Printerror(status);
      }
      else
      {
	 if (fits_write_col(fptr, TFLOAT, 2, firstrow, firstelem, nrows,
			    counts, &status))
	    Printerror(status);      
      } 

      colnum = 2;
      /* Write errors if present */
      if (qerror != 0)
      {
	 if (fits_write_col(fptr, TFLOAT, ++colnum, firstrow, firstelem, nrows,
			    serr, &status))
	    Printerror(status);      
      }

      if (qsys != 0)
      {
	 if (fits_write_col(fptr, TFLOAT, ++colnum, firstrow, firstelem, nrows,
			    syserr, &status))
	    Printerror(status);      
      }

      if (qqual != 0)
      {	 
	 for (i=0; i<nchan; i++)
	    conv2[i] = quality[i]; 

	 if (fits_write_col(fptr, TSHORT, ++colnum, firstrow, firstelem, nrows,
			    conv2, &status))
	    Printerror(status);      
      }

      /* Write grouping if present */
      if (qgroup != 0)
      {
	 for (i=0; i<nchan; i++)
	    conv2[i] = grping[i]; 

	 if (fits_write_col(fptr, TSHORT, ++colnum, firstrow, firstelem, nrows,
			    conv2, &status))
	    Printerror(status);      
      }

      DispMsg(chatter, 20, " ... Written the data");
   }

   else
   {
      strcpy(message, errstr);
      strcat(message, " FATAL: Extension not written");
      DispMsg(1, 1, message);
   }

   free(conv2);
   free(conv4);

   return(status);
}
