/* 
-------- Function: -------------------------------------------------------------

     ReadType1Pha


-------- Description: ----------------------------------------------------------

     This subroutine reads the SPECTRUM extension of a PHA file in 
     the following formats
         PHAVERSN = 1992a
         HDUVERS1 = 1.0.0
         HDUVERS1 = 1.1.0
         HDUVERS  = 1.1.0

   Assumptions:
   
      The FITS is open and the file is at the desired SPECTRUM extension.
        !!! Note !!!! File is left open at the end  
        and  MUST BE CLOSED               by fits_close_file 
        or   ANOTHER EXTENSION READ       by fits_read_btblhdr


-------- Functions called: -----------------------------------------------------

   FITS functions:
      fits_read_btblhdr   : (FITSIO) Reads the header info from the CHU
      fits_read_key_lng   : (FITSIO) Reads a LONG keyword
      fits_read_key_str   : (FITSIO) Reads a STRING keyword
      fits_read_key_dbl   : (FITSIO) Reads a DOUBLE keyword
      fits_read_key_log   : (FITSIO) Reads a LOGICAL keyword
      fits_read_key_flt   : (FITSIO) Reads a FLOAT keyword
      fits_get_colnum     : (FITSIO) Rerieves the column number 
      fits_read_col_sht   : (FITSIO) Reads the column in SHORT format
      fits_read_col_lng   : (FITSIO) Reads the column in LONG format
      fits_read_col_flt   : (FITSIO) Reads the column in FLOAT format


   General Utility Functions:
      DispMsg      -- displays messages
      Printerror   -- prints FITSIO error messages
      PrintWarning -- prints FITSIO warnings


-------- Usage: ----------------------------------------------------------------

   int ReadTypeIPha(fitsfile *fptr, int chatter, char *telescop, char *instrume, 
                    char *detnam, char *filter, char *phaversn, char *hduclas2, 
		    long *fchan, double *texpos, float *areascal, char *backfil, 
		    float *backscal, char *corrfil, float *corrscal, 
		    char *rmffil, char *arffil, int *nXflt, char **xflt, 
		    char *dmode, long *detchans, char *chantyp, short **channel,
		    long **ipha, float **pha, int *dtype, int *qerror, 
		    float **error, int *qsys, float **sysfrc, int *qqual, 
		    short **quality, int *qgroup, short **grping, int *pois, 
		    long *nchan)
   Input Parameters:

        FPTR          fitsfile *  : Pointer to the input PHA file
        CHATTER       int         : chattiness flag for o/p (5 quite,10 normal,
	                            >20 silly)

   Output Parameters:

        TELESCOP   : Telescope name
        INSTRUME   : Instrument name
        DETNAM     : Specific detector name
	FILTER     : Filter in use
	PHAVERSN   : PHA file format version
	HDUCLAS2   : Describes SPECTRUM - BKG,TOTAL,NET
	FCHAN      : First possible value (TLMIN) of first column (Channel)
	             if TLMIN not found the default value 1 is assumed
	TEXPOS     : Exposure time
	AREASCAL   : Area scaling factor
	BACKFILE   : Associated background filename
	BACKSCAL   : Background scaling factor
	CORRFILE   : Associated correction filename
	CORRSCAL   : Correction scaling factor
	RMFFILE    : Associated response filename
	ARFFILE    : Associated ancillary response filename
	NXFLT      : Number of XSPEC filter keywords
	XFLT       : XSPEC filter description for each XSPEC filter keyword
	DMODE      : Datamode - for ASCA SIS - BRIGHT,FAINT etc 
	DETCHANS   : Number of possible detector channels
	CHANTYPE   : Channel type - PHA or PI
	CHANNEL    : Channel numbers in 1st column
        IPHA       : Data if written in the units of COUNTS
	PHA        : Data if written in the units of COUNTS/SEC
	DTYPE      : 1 if data is in COUNTS; 2 if data is written as RATES
	QERROR     : True if statistical errors are present in the input file
	ERROR      : Statistical errors if QERROR=TRUE
	QSYS       : True if systematic errors are present in the input file
	SYSFRC     : Systematic errors if QSYS=TRUE
	QQUAL      : True if quality info is present in a column in the input 
	             file
	QUALITY    : Channel quality flags if QQUAL=TRUE
	QGROUP     : True if grouping info is present in a column in the input 
	             file
	GRPING     : Channel grouping flags if QGROUP=TRUE
	POIS       : True if Poission errors apply
        NCHAN      : Total number of rows present in the extension


-------- Include files ---------------------------------------------------------

   <stdio.h>
   <stdlib.h>
   <string.h>
   "general.h"


-------- Origin: ---------------------------------------------------------------

   This function is basically a C version of rdpha1.f.


-------- Authors/Modification History: -----------------------------------------

   Sandhia Bansal  (1.0.0; Jul 97)

----------------------------------------------------------------------------- */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "general.h"


int ReadTypeIPha(fitsfile *fptr, int chatter, char *telescop, char *instrume, 
		 char *detnam, char *filter, char *phaversn, char *hduclas2, 
		 long *fchan, double *texpos, float *areascal, char *backfil, 
		 float *backscal, char *corrfil, float *corrscal, char *rmffil, 
		 char *arffil, int *nXflt, char **xflt, char *dmode, 
		 long *detchans, char *chantyp, short **channel, long **ipha, 
		 float **pha, int *dtype, int *qerror, float **error, int *qsys,
		 float **sysfrc, int *qqual, short **quality, int *qgroup, 
		 short **grping, int *pois, long *nchan)
{
   float rval=0.0;

   long  pcount;
   long  firstrow=1, firstelem=1;
   long  lnullval=0;
   long  lvar;
   long  repeat, width;

   float enullval=0.0;

   int   status=0;
   int   tfields;
   int   i;
   int   colnum;
   int   anynul=0;
   int   typecode;
   int   qnotint;

   short snullval = 0;

   char  version[7];
   char  extname[FLEN_VALUE];
   char  errstr[FLEN_ERRMSG];
   char  wrnstr[FLEN_ERRMSG];
   char  message[FLEN_ERRMSG];
   char  *ttype[NFIELDS_I];
   char  *tform[NFIELDS_I];
   char  *tunit[NFIELDS_I];
   char  *tcomm[NFIELDS_I];
   char  comment[FLEN_COMMENT];
   char  tlmin[FLEN_KEYWORD];
   
   /* Initializations */
   strcpy(version, "1.0.0");

   strcpy(errstr, "** ReadTypeIPha ");
   strcat(strcat(errstr, version), " ERROR: ");

   strcpy(wrnstr, "** ReadTypeIPha ");
   strcat(strcat(wrnstr, version), " WARNING: ");
   
   /* Give user info if requested */
   if (chatter >= 15)
   {
      strcpy(message, " ... using ReadTypeIPha Version ");
      strcat(message, version);
   }

   /* Allocate space for the table parameters and initialize */
   for (i=0; i<NFIELDS_I; i++)
   {
      ttype[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
      tform[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
      tunit[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
      tcomm[i] = (char *) malloc(FLEN_CARD*sizeof(char));
      strcpy(ttype[i], " ");
   }

   /* Read binary table keywords from the CHU */
   if (fits_read_btblhdr(fptr, NFIELDS_I, nchan, &tfields, ttype, tform, 
			 tunit, extname, &pcount, &status))
      Printerror(status);

   if (*nchan <= 0)
   {
      status = 1;
      DispMsg(1, 1, "Array size is zero in the input file");
   }
   
   else
   {
      /* Allocate space for channels
	 Other arrays will be allocated as needed */
      *channel = (short *) malloc(*nchan*sizeof(short)); 

      /* Read DETCHANS */
      if (fits_read_key_lng(fptr, "DETCHANS", detchans, comment, &status))
	 PrintWarning("ReadType1Pha", version, 
		      "Error while reading DETCHANS keyword", 
		      chatter, 10, status);
      status = 0;

      /* Determine which columns are present and set the appropriate flags */
      *dtype = *qerror = *qsys = *qqual = *qgroup = 0;

      for (i=0; i<NFIELDS_I; i++)
      {
	 if (strcmp(ttype[i], "COUNTS") == 0)
	    *dtype = 1;
	 else if (strcmp(ttype[i], "RATE") == 0)
	    *dtype = 2;
	 else if (strcmp(ttype[i], "STAT_ERR") == 0)
	    *qerror = 1;
	 else if (strcmp(ttype[i], "SYS_ERR") == 0)
	    *qsys = 1;
	 else if (strcmp(ttype[i], "QUALITY") == 0)
	    *qqual = 1;
	 else if (strcmp(ttype[i], "GROUPING") == 0)
	    *qgroup = 1;
      }

      if (*dtype == 0)
      {
	 status = 1;
	 DispMsg(1, 1, "Error while reading primary header");
      }

      else
      {
	 /* Read TELESCOPE */
	 strcpy(telescop, "UNKNOWN");
	 if (fits_read_key_str(fptr, "TELESCOP", telescop, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading TELESCOP keyword", 
			 chatter, 20, status);
	 if (status == 225)
	    strcpy(telescop, "UNKNOWN");
	 status = 0;

	 /* Read INSTRUMENT */
	 strcpy(instrume, "UNKNOWN");
	 if (fits_read_key_str(fptr, "INSTRUME", instrume, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading INSTRUME keyword", 
			 chatter, 20, status);
	 if (status == 225)
	    strcpy(instrume, "UNKNOWN");
	 status = 0;

	 /* Read PHAVERSN */
	 strcpy(phaversn, " ");
	 if (fits_read_key_str(fptr, "HDUVERS", phaversn, comment, &status))
	 {
	    PrintWarning("ReadType1Pha", version, 
			 "Could not find HDUVERS - trying HDUVERS1",
			 chatter, 30, status);
	    if (strcmp(phaversn, " ") == 0)
	    {
	       status = 0;
	       if (fits_read_key_str(fptr, "HDUVERS1", phaversn, comment, 
				     &status))
		  PrintWarning("ReadType1Pha", version, 
			       "Could not find HDUVERS1 - trying PHAVERSN",
			       chatter, 30, status);
	    }
	    if (strcmp(phaversn, " ") == 0)
	    {
	       status = 0;
	       if (fits_read_key_str(fptr, "PHAVERSN", phaversn, comment, 
				     &status))
		  PrintWarning("ReadType1Pha", version, 
			       "Error while reading PHAVERSN keyword", 
			       chatter, 30, status);
	    }
	 }
	 status = 0;

	 /* Read HDUCLAS2 */
	 strcpy(hduclas2, " ");
	 if (fits_read_key_str(fptr, "HDUCLAS2", hduclas2, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading HDUCLAS2 keyword", 
			 chatter, 30, status);
	 status = 0;

	 /* Read DETNAM */
	 strcpy(detnam, " ");
	 if (fits_read_key_str(fptr, "DETNAM", detnam, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading DETNAM keyword", 
			 chatter, 20, status);
	 status = 0;

	 /* Read FILTER */
	 strcpy(filter, " ");
	 if (fits_read_key_str(fptr, "FILTER", filter, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading FILTER keyword", 
			 chatter, 20, status);
	 status = 0;

	 /* Read TEXPOS */
	 *texpos = -1.0;
	 if (fits_read_key_dbl(fptr, "EXPOSURE", texpos, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading FILTER keyword", 
			 chatter, 10, status);
	 if (status == 225)
	    PrintWarning("ReadType1Pha", version, 
			 "Exposure time not found - exposure set to -1",
			 chatter, 10, status);
	 status = 0;

	 /* Read POISSERR */
	 *pois = 0;
	 if (fits_read_key_log(fptr, "POISSERR", pois, comment, &status))
	 {
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading POISSERR keyword", 
			 chatter, 15, status);
	    PrintWarning("ReadType1Pha", version, 
			 "POISSERR assumed to be false", 
			 chatter, 10, status);
	 }
	 status = 0;

	 /* Read AREASCAL */
	 *areascal = 1.0;
	 if (fits_read_key_flt(fptr, "AREASCAL", areascal, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading AREASCAL keyword", 
			 chatter, 10, status);
	 status = 0;

	 /* Read BACKSCAL */
	 *backscal = 1.0;
	 if (fits_read_key_flt(fptr, "BACKSCAL", backscal, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading BACKSCAL keyword", 
			 chatter, 10, status);
	 status = 0;

	 /* Read CORRSCAL */
	 *corrscal = 1.0;
	 if (fits_read_key_flt(fptr, "CORRSCAL", corrscal, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading CORRSCAL keyword", 
			 chatter, 10, status);
	 status = 0;

	 /* Read BACKFILE */
	 strcpy(backfil, "NONE");
	 if (fits_read_key_str(fptr, "BACKFILE", backfil, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading BACKFILE keyword", 
			 chatter, 10, status);
	 status = 0;

	 /* Read CORRFILE */
	 strcpy(corrfil, "NONE");
	 if (fits_read_key_str(fptr, "CORRFILE", corrfil, comment, &status))
 	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading CORRFILE keyword", 
			 chatter, 10, status);
	 status = 0;

	 /* Read RESPFILE */
	 strcpy(rmffil, "NONE");
	 if (fits_read_key_str(fptr, "RESPFILE", rmffil, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading RESPFILE keyword", 
			 chatter, 10, status);
	 status = 0;

	 /* Read ANCRFILE */
	 strcpy(arffil, "NONE");
	 if (fits_read_key_str(fptr, "ANCRFILE", arffil, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading ANCRFILE keyword", 
			 chatter, 10, status);
	 status = 0;

	 /* Read CHANTYPE */
	 strcpy(chantyp, "UNKNOWN");
	 if (fits_read_key_str(fptr, "CHANTYPE", chantyp, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading CHANTYPE keyword", 
			 chatter, 20, status);
	 status = 0;

	 /* Read XSPEC FILTER */
	 if (fits_read_keys_str(fptr, "XFLT", 1, 9999, xflt, nXflt, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading XFLT keyword", 
			 chatter, 30, status);
	 status = 0;
	
	 /* Read DATAMODE */
	 strcpy(dmode, " ");
	 if (fits_read_key_str(fptr, "DATAMODE", dmode, comment, &status))
	    PrintWarning("ReadType1Pha", version, 
			 "Error while reading DATAMODE keyword", 
			 chatter, 10, status);
	 status = 0;

	 /* READ DATA */

	 /* CHANNEL   */
	 if (fits_get_colnum(fptr, 0, "CHANNEL", &colnum, &status))
	 {
	    if (fits_get_colnum(fptr, 0, " CHANNEL", &colnum, &status))
	       Printerror(status);
	 }

	 if (fits_read_col_sht(fptr, colnum, firstrow, firstelem, *nchan, 
			       snullval, *channel, &anynul, &status))
	    Printerror(status);

	 /* Read TLMINx keyword, where x is channel column number to determine
	    first possible channel value */
	 sprintf(tlmin, "TLMIN%d", colnum);
	 if (fits_read_key_lng(fptr, tlmin, fchan, comment, &status))
	 {
	    if (status == 202)              /* keyword not found in header */
	    {
	       *fchan = (((*channel)[0] == 0) ? 0L : 1L);
	       PrintWarning("ReadType1Pha", version, 
			    "TLMIN keyword for FCHAN value not found", 
			    chatter, 15, status);
	       sprintf(message, "Default value of FCHAN is %d\n", fchan);
	       PrintWarning("ReadType1Pha", version, message, chatter, 15, 
			    status);
	    }
	 }
	 status = 0;

	 /* COUNTS or RATE */
	 if (*dtype == 1)
	 {
	    if (fits_get_colnum(fptr, 0, "COUNTS", &colnum, &status))
	       Printerror(status);

	    *pha = (float *) malloc(*nchan*sizeof(float)); 
	    if (fits_read_col_flt(fptr, colnum, firstrow, firstelem, *nchan, 
				  enullval, *pha, &anynul, &status))
	       Printerror(status);

	    *ipha = (long *) malloc(*nchan*sizeof(long));
            qnotint = 0;
	    for (i=0; i<*nchan; i++) {
	      *ipha[i] = (long)(*pha[i]);
	      if ( abs(*ipha[i] - *pha[i]) > 1e-5 ) qnotint = 1;
	    }
	    if ( qnotint == 1 ) {
	      PrintWarning("ReadType1Pha", version, "**WARNING: COUNTS column is non-integer and will be truncated\n", chatter, 0, status);
	    }

	 }
	 
	 else if (*dtype == 2)
	 {
	    if (fits_get_colnum(fptr, 0, "RATE", &colnum, &status))
	       Printerror(status);

	    *pha = (float *) malloc(*nchan*sizeof(float)); 
	    if (fits_read_col_flt(fptr, colnum, firstrow, firstelem, *nchan, 
				  enullval, *pha, &anynul, &status))
	       Printerror(status);
	 }
	 
	 /* STAT_ERR */
	 *error = (float *) malloc(*nchan*sizeof(float)); 
	 for (i=0; i<*detchans; i++)
	    (*error)[i] = 0.0;

	 if (*qerror == 1)	    
	 {
	    if (fits_get_colnum(fptr, 0, "STAT_ERR", &colnum, &status))
	       Printerror(status);

	    if (fits_read_col_flt(fptr, colnum, firstrow, firstelem, *nchan, 
				  enullval, *error, &anynul, &status))
	       Printerror(status);
	 }
	 
	 else if (*pois == 0)
	 {
	    if (fits_read_key_flt(fptr, "STAT_ERR", &rval, comment, &status))
	       status = 0;
	    else
	    {
	       for (i=0; i<*detchans; i++)
		  (*error)[i] = rval;
	       *qerror = 1;
	    }
	 }

	 /* SYS_ERR */
	 *sysfrc = (float *) malloc(*nchan*sizeof(float)); 
	 for (i=0; i<*detchans; i++)
	    (*sysfrc)[i] = 0.0;

	 if (*qsys == 1)
	 {
	    if (fits_get_colnum(fptr, 0, "SYS_ERR", &colnum, &status))
	       Printerror(status);

	    if (fits_read_col_flt(fptr, colnum, firstrow, firstelem, *nchan, 
				  enullval, *sysfrc, &anynul, &status))
	       Printerror(status);
	 }
	 
	 else
	 {
	    if (fits_read_key_flt(fptr, "SYS_ERR", &rval, comment, &status))
	       status = 0;
	    else
	    {
	       for (i=0; i<*detchans; i++)
		  (*sysfrc)[i] = rval;
	       *qsys = 1;
	    }
	 }

	 /* QUALITY */
	 *quality = (short *) malloc(*nchan*sizeof(short)); 
	 for (i=0; i<*detchans; i++)
	    (*quality)[i] = 0;

	 if (*qqual == 1)
	 {
	    if (fits_get_colnum(fptr, 0, "QUALITY", &colnum, &status))
	       Printerror(status);

	    if (fits_read_col_sht(fptr, colnum, firstrow, firstelem, *nchan, 
				  snullval, *quality, &anynul, &status))
	       Printerror(status);
	 }
	 
	 else
	 {
	    if (fits_read_key_lng(fptr, "QUALITY", &lvar, comment, &status))
	       status = 0;
	    else
	    {
	       for (i=0; i<*detchans; i++)
		  (*quality)[i] = (short) lvar;
	       *qqual = 1;
	    }
	 }

	 /* GROUPING */
	 *grping = (short *) malloc(*nchan*sizeof(short)); 
	 for (i=0; i<*detchans; i++)
	    (*grping)[i] = 0;

	 if (*qgroup == 1)
	 {
	    if (fits_get_colnum(fptr, 0, "GROUPING", &colnum, &status))
	       Printerror(status);

	    if (fits_read_col_sht(fptr, colnum, firstrow, firstelem, *nchan, 
				  snullval, *grping, &anynul, &status))
	       Printerror(status);
	 }

	 DispMsg(chatter, 20, "Data has been read");
      }
   }

   for (i=0; i<NFIELDS_I; i++)
   {
      free(ttype[i]);
      free(tform[i]);
      free(tunit[i]);
      free(tcomm[i]);
   }

   return(status);
}

