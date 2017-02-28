/*
-------- Function: ------------------------------------------------------------

     WriteTypeIIPha


-------- Description: ---------------------------------------------------------

     This subroutine writes the SPECTRUM extension for a PHA file of type II
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


-------- Functions called: ----------------------------------------------------

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
      fits_modify_key_lng     : (FITSIO) Modifies a long keyword

   General Utility Functions:
      Printerror   -- prints FITSIO error messages
      DispMsg      -- Prints the input error message to the screen


-------- Usage: ---------------------------------------------------------------

   int WriteTypeIIPha(fitsfile *fptr, int chatter, int nk_history,
                      char **history, int nk_comm, char **comment,
		      char *telescop, char *instrume, char *detnam,
		      char *filter, char *phaversn, char *hduclas2, long fchan,
		      double texpos, float areascal, int nbackfil,
		      char **backfil, float *backscal, int ncorrfil,
		      char **corrfil, float *corrscal, int nrespfil,
		      char **respfil, int nancrfil, char **ancrfil,
		      int ntemplfil, char **templfil, long detchans,
		      char *chantyp, short **channel, float **counts, int dtype,
		      int qerror, float **serr, int qsys, float **syserr,
		      int qqual, short **quality, int qgroup, short **grping,
		      int nspec, long nchan)

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
       NBACKFIL      int       : Number of background files
       BACKFIL       char      : Associated background filename
       BACKSCAL      float     : Background scaling factor
       NCORRFIL      int       : Number of correction files
       CORRFIL       char      : Associated correction filename
       CORRSCAL      float     : Correction scaling factor
       NRESPFIL      int       : Number of redistribution matrix files (RMF)
       RESPFIL       char      : Detector redistribution matrix file
       NANCRFIL      int       : Number of ancillary response files (ARF)
       ANCRFIL       char      : Ancillary response file
       NTEMPLFIL     int       : Number of template files
       TEMPLFIL      char      : Template file(s)
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
       NSPEC         int       : No. of spectra
       NCHAN	     long      : No. channels actually passed down

   Output Parameters:
       STATUS        int       : 0=OK.


-------- Include files --------------------------------------------------------

   <stdio.h>
   <stdlib.h>
   <string.h>
   "general.h"


-------- Origin: --------------------------------------------------------------

   This function is basically a C version of wtpha1.f.


-------- Authors/Modification History: ----------------------------------------

   Sandhia Bansal  (1.0.3; Dec    98) Added two new arguments (ntemplfile and
                                      templfile) to accomodate BKGEN program.
   Sandhia Bansal  (1.0.2; Nov 10 98) Added a 3rd datatype (dtype) to handle
                                      the case when data is in counts/s/kev
				      units.
   kaa             (1.0.2; Dec 4 98)  Use HDUVERS not HDUVERS1 in format 1.1.0.
   Sandhia Bansal  (1.0.1; Oct 27 97) Use tlmin2 and tlmax2 instead of
                                      tlmin1 and tlmax1.
   Sandhia Bansal  (1.0.0; Jul 97)

---------------------------------------------------------------------------- */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "general.h"



int WriteTypeIIPha(fitsfile *fptr, int chatter, int nk_history, char **history,
		   int nk_comm, char **comment, char *telescop, char *instrume,
		   char *detnam, char *filter, char *phaversn, char *hduclas2,
		   long fchan, double texpos, float areascal, int nbackfil,
		   char **backfil, float *backscal, int ncorrfil,
		   char **corrfil, float *corrscal, int nrespfil,
		   char **respfil, int nancrfil, char **ancrfil, int ntemplfil,
		   char **templfil, long detchans, char *chantyp,
		   short **channel, float **counts, int dtype, int qerror,
		   float **serr, int qsys, float **syserr, int qqual,
		   short **quality, int qgroup, short **grping, int nspec,
		   long nchan)
{
   long  nrows=0;
   long  firstrow=1, firstelem=1;
   long  *conv4 = (long *) malloc(nchan*sizeof(long));
   long  lkeyval;
   long  tlmin=fchan;
   long  tlmax=fchan+detchans-1;

   int   status=0;
   int   tfields=4;
   int   i, j;
   int   ikeyval;
   int   colnum;

   short *conv2 = (short *) malloc(nchan*sizeof(short));

   char  version[7];
   char  errstr[FLEN_ERRMSG];
   char  wrnstr[FLEN_ERRMSG];
   char  message[FLEN_ERRMSG];
   char  hduvers[6];
   char  *ttype[NFIELDS_II];
   char  *tform[NFIELDS_II];
   char  *tunit[NFIELDS_II];
   char  *tcomm[NFIELDS_II];
   char  extname[FLEN_VALUE];
   char  keyname[FLEN_KEYWORD];
   char  ckeyval[FLEN_KEYWORD];
   char  *rowid[1];

   /* Initializations */
   strcpy(version, "1.0.3");
   strcpy(extname, "SPECTRUM");

   strcpy(errstr, "** WriteTypeIIPha ");
   strcat(strcat(errstr, version), " ERROR: ");

   strcpy(wrnstr, "** WriteTypeIIPha ");
   strcat(strcat(wrnstr, version), " WARNING: ");

   /* Give user info if requested */
   if (chatter >= 15)
   {
      strcpy(message, " ... using WriteTypeIIPha Version ");
      strcat(message, version);
      DispMsg(1, 1, message);
   }

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
      for (i=0; i<NFIELDS_II; i++)
      {
	 ttype[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
	 tform[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
	 tunit[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
	 tcomm[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
	 strcpy(tunit[i], " ");
	 strcpy(tcomm[i], " ");
      }

      strcpy(ttype[0], "SPEC_NUM");
      strcpy(tform[0], "1I");

      strcpy(ttype[1], "CHANNEL");
      sprintf(tform[1], "%dI", nchan);
      if ((strcmp(chantyp, "PHA") == 0) || (strcmp(chantyp, "pha") == 0))
	 strcpy(tcomm[1], "Pulse Height Analyser (PHA) Channel ");
      else if ((strcmp(chantyp, "PI") == 0) || (strcmp(chantyp, "pi") == 0))
	 strcpy(tcomm[1], "Pulse Invarient (PI) Channel ");
      else
	 strcpy(tcomm[1], "Detector channel (type unknown)");

      if (dtype == 1)
      {
	 strcpy(ttype[2], "COUNTS");
	 sprintf(tform[2], "%dJ", nchan);
	 strcpy(tunit[2], "count");
	 strcpy(tcomm[2], "Counts per channel");
      }

      else if (dtype == 2)
      {
	 strcpy(ttype[2], "RATE");
	 sprintf(tform[2], "%dE", nchan);
	 strcpy(tunit[2], "count/s");
	 strcpy(tcomm[2], "Counts per second per channel");
      }

      else if (dtype == 3)
      {
	 strcpy(ttype[2], "RATE");
	 sprintf(tform[2], "%dE", nchan);
	 strcpy(tunit[2], "count/s/keV");
	 strcpy(tcomm[2], "Counts per second per energy");
      }

      strcpy(ttype[3], "ROWID");
      strcpy(tform[3], "20A");

      if (qerror != 0)
      {
	 strcpy(ttype[tfields], "STAT_ERR");
	 sprintf(tform[tfields], "%dE", nchan);
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

	 else if (dtype == 3)
	 {
	    strcpy(tunit[tfields], "counts/s/keV");
	    strcpy(tcomm[tfields], "Statistical error on RATE/keV");
	 }

	 tfields++;
      }

      if (qsys != 0)
      {
	 strcpy(ttype[tfields], "SYS_ERR");
	 sprintf(tform[tfields], "%dE", nchan);
	 if (dtype == 1)
	 {
	    strcpy(tcomm[tfields], "Fractional systematic errors on COUNTS");
	 }

	 else if (dtype == 2)
	 {
	    strcpy(tcomm[tfields], "Fractional systematic errors on RATE");
	 }

	 else if (dtype == 3)
	 {
	    strcpy(tcomm[tfields], "Fractional systematic errors on RATE/keV");
	 }

	 tfields++;
      }

      if (qqual != 0)
      {
	 strcpy(ttype[tfields], "QUALITY");
	 sprintf(tform[tfields], "%dI", nchan);
	 strcpy(tcomm[tfields], "Quality flag of this channel (0=good)");
	 tfields++;
      }

      if (qgroup != 0)
      {
	 strcpy(ttype[tfields], "GROUPING");
	 sprintf(tform[tfields], "%dI", nchan);
	 strcpy(tcomm[tfields], "Grouping flag of this channel (0=undefined)");
	 tfields++;
      }

      if (nbackfil > 1)
      {
	 strcpy(ttype[tfields], "BACKFILE");
	 sprintf(tform[tfields], "%dA", strlen(backfil[0]));
	 strcpy(tcomm[tfields], "Associated Background Filename");
	 tfields++;

	 strcpy(ttype[tfields], "BACKSCAL");
	 strcpy(tform[tfields], "1E");
	 strcpy(tcomm[tfields], "Background File Scaling Factor");
	 tfields++;
     }

      if (ntemplfil > 1)
      {
	 strcpy(ttype[tfields], "TPL_FILE");
	 sprintf(tform[tfields], "%dA", strlen(templfil[0]));
	 strcpy(tcomm[tfields], "Associated Template Filename");
	 tfields++;
     }

      if (ncorrfil > 1)
      {
	 strcpy(ttype[tfields], "CORRFILE");
	 sprintf(tform[tfields], "%dA", strlen(corrfil[0]));
	 strcpy(tcomm[tfields], "Associated Correction Filename");
	 tfields++;

	 strcpy(ttype[tfields], "CORRSCAL");
	 strcpy(tform[tfields], "1E");
	 strcpy(tcomm[tfields], "Correction File Scaling Factor");
	 tfields++;
      }

      if (nrespfil > 1)
      {
	 strcpy(ttype[tfields], "RESPFILE");
	 sprintf(tform[tfields], "%dA", strlen(respfil[0]));
	 strcpy(tcomm[tfields], "Associated Redistrib Matrix Filename");
	 tfields++;
      }

      if (nancrfil > 1)
      {
	 strcpy(ttype[tfields], "ANCRFILE");
	 sprintf(tform[tfields], "%dA", strlen(ancrfil[0]));
	 strcpy(tcomm[tfields], "Associated Ancillary Response Filename");
	 tfields++;
      }

      /* Create binary table */
      if (fits_create_tbl(fptr, BINARY_TBL, nrows, tfields, ttype, tform, tunit,
			  extname, &status))
	 Printerror(status);

      DispMsg(chatter, 20, " ... new extension created");
      DispMsg(chatter, 20, " ... written the extension header keywords");

      /* Modify the comments in the TTYPE fields */
      for (i=0; i<tfields; i++)
      {
	 if (strcmp(tcomm[i], " ") != 0)
	 {
	    sprintf(keyname, "TTYPE%d", i+1);
	    if (fits_modify_comment(fptr, keyname, tcomm[i], &status))
	       Printerror(status);
	 }
      }

      for (i=0; i<NFIELDS_II; i++)
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

         case 3:
	    strcpy(ckeyval, "RATE");
	    strcpy(message, "PHA data stored as Counts/s/keV");
	    break;

         default:
	    strcpy(ckeyval, "UNKNOWN");
	    strcpy(message, "Unknown storage method for PHA data");
	    break;
      }
      if (fits_write_key(fptr, TSTRING, "HDUCLAS3", ckeyval, message, &status))
	 Printerror(status);

      if (fits_write_key(fptr, TSTRING, "HDUCLAS4", "TYPE:II",
			 "Multiple PHA datasets contained", &status))
	 Printerror(status);

      /* Write additional keywords describing data */
      if (fits_write_key_lng(fptr, "TLMIN2", fchan,
			     "Lowest legal channel number", &status))
	 Printerror(status);

      if (fits_write_key_lng(fptr, "TLMAX2", tlmax,
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

      if (nbackfil <= 1)
      {
	 if (strcmp(backfil[0], "none") == 0)
	    strcpy(backfil[0], "NONE");
	 if (fits_write_key(fptr, TSTRING, "BACKFILE", backfil[0],
			    "Associated background filename", &status))
	    Printerror(status);

	 if (fits_write_key_fixflt(fptr, "BACKSCAL", backscal[0], DECIMALS,
				   "Background file scaling factor", &status))
	    Printerror(status);
      }

      if (ntemplfil <= 1)
      {
	 if (strcmp(templfil[0], "none") == 0)
	    strcpy(templfil[0], "NONE");
	 if (fits_write_key(fptr, TSTRING, "TPL_FILE", templfil[0],
			    "Associated template filename", &status))
	    Printerror(status);
      }

      if (ncorrfil <= 1)
      {
	 if (strcmp(corrfil[0], "none") == 0)
	    strcpy(corrfil[0], "NONE");
	 if (fits_write_key(fptr, TSTRING, "CORRFILE", corrfil[0],
			    "Associated correction filename", &status))
	    Printerror(status);

	 if (fits_write_key_fixflt(fptr, "CORRSCAL", corrscal[0], DECIMALS,
				   "Correction file scaling factor", &status))
	  Printerror(status);
      }

      if (nrespfil <= 1)
      {
	 if (fits_write_key(fptr, TSTRING, "RESPFILE", respfil[0],
			    "Associated redistrib matrix filename", &status))
	    Printerror(status);
      }

      if (nancrfil <= 1)
      {
	 if (fits_write_key(fptr, TSTRING, "ANCRFILE", ancrfil[0],
			    "Associated ancillary response filename", &status))
	    Printerror(status);
      }

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

      strcpy(message, "FITS SPECTRUM extension written by WriteTypeIIPha ");
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
      for (i=0; i<nspec; i++)
      {
	 /* Write column 1: SPEC_NUM (1-element array) */
	 conv2[0] = (short) i;
	 if (fits_write_col(fptr, TSHORT, 1, firstrow, firstelem, 1, conv2,
			    &status))
	    Printerror(status);

	 /* Write column 2: CHANNEL (nchan-element array) */
	 for (j=0; j<nchan; j++)
	    conv2[j] = channel[i][j];
	 if (fits_write_col(fptr, TSHORT, 2, firstrow, firstelem, nchan, conv2,
			    &status))
	    Printerror(status);

	 /* Write column 3: COUNTS/RATE (nchan-element array) */
	 if (dtype == 1)
	 {
	    for (j=0; j<nchan; j++)
	       conv4[j] = (long) counts[i][j];
	    if (fits_write_col(fptr, TLONG, 3, firstrow, firstelem, nchan,
			       conv4, &status))
	       Printerror(status);
	 }

	 else if ((dtype == 2) || (dtype == 3))
	 {
	    if (fits_write_col(fptr, TFLOAT, 3, firstrow, firstelem, nchan,
			       counts[i], &status))
	       Printerror(status);
	 }

	 /* Write column 4: ROWID (1-element array) */
	 rowid[0] = (char *) malloc(20*sizeof(char));
	 sprintf(rowid[0], "SPEC_%d", i+1);
	 if (fits_write_col(fptr, TSTRING, 4, firstrow, firstelem, 1, rowid,
			    &status))
	    Printerror(status);
	 free(rowid[0]);

	 colnum = 4;

	 /* Write errors if present */
	 if (qerror != 0)
	 {
	    if (fits_write_col(fptr, TFLOAT, ++colnum, firstrow, firstelem,
			       nchan, serr[i], &status))
	       Printerror(status);
	 }

	 if (qsys != 0)
	 {
	    if (fits_write_col(fptr, TFLOAT, ++colnum, firstrow, firstelem,
			       nchan, syserr[i], &status))
	       Printerror(status);
	 }

	 if (qqual != 0)
	 {
	    for (j=0; j<nchan; j++)
	       conv2[j] = quality[i][j];

	    if (fits_write_col(fptr, TSHORT, ++colnum, firstrow, firstelem,
			       nchan, conv2, &status))
	       Printerror(status);
	 }

	 /* Write grouping if present */
	 if (qgroup != 0)
	 {
	    for (j=0; j<nchan; j++)
	       conv2[j] = grping[i][j];

	    if (fits_write_col(fptr, TSHORT, ++colnum, firstrow, firstelem,
			       nchan, conv2, &status))
	       Printerror(status);
	 }

	 if (nbackfil > 1)
	 {
	    rowid[0] = (char *) malloc(strlen(backfil[0])*sizeof(char));
	    strcpy(rowid[0], backfil[i]);
	    if (fits_write_col(fptr, TSTRING, ++colnum, firstrow, firstelem, 1,
			       rowid, &status))
	       Printerror(status);
	    free(rowid[0]);

	    if (fits_write_col(fptr, TFLOAT, ++colnum, firstrow, firstelem, 1,
			       &backscal[i], &status))
	       Printerror(status);
	 }

	 if (ntemplfil > 1)
	 {
	    rowid[0] = (char *) malloc(strlen(templfil[0])*sizeof(char));
	    strcpy(rowid[0], templfil[i]);
	    if (fits_write_col(fptr, TSTRING, ++colnum, firstrow, firstelem, 1,
			       rowid, &status))
	       Printerror(status);
	    free(rowid[0]);
	 }

	 if (ncorrfil > 1)
	 {
	    rowid[0] = (char *) malloc(strlen(corrfil[0])*sizeof(char));
	    strcpy(rowid[0], corrfil[i]);
	    if (fits_write_col(fptr, TSTRING, ++colnum, firstrow, firstelem, 1,
			       rowid, &status))
	       Printerror(status);
	    free(rowid[0]);

	    if (fits_write_col(fptr, TFLOAT, ++colnum, firstrow, firstelem, 1,
			       &corrscal[i], &status))
	       Printerror(status);
	 }

	 if (nrespfil > 1)
	 {
	    rowid[0] = (char *) malloc(strlen(respfil[0])*sizeof(char));
	    strcpy(rowid[0], respfil[i]);
	    if (fits_write_col(fptr, TSTRING, ++colnum, firstrow, firstelem, 1,
			       rowid, &status))
	       Printerror(status);
	    free(rowid[0]);
	 }

	 if (nancrfil > 1)
	 {
	    rowid[0] = (char *) malloc(strlen(ancrfil[0])*sizeof(char));
	    strcpy(rowid[0], ancrfil[i]);
	    if (fits_write_col(fptr, TSTRING, ++colnum, firstrow, firstelem, 1,
			       rowid, &status))
	       Printerror(status);
	    free(rowid[0]);
	 }

	 firstrow++;

	 strcpy(keyname, "NAXIS2");
	 if (fits_modify_key_lng(fptr, keyname, firstrow-1, "&", &status))
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
