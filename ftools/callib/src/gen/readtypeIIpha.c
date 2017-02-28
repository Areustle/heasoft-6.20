/* 
-------- Function: -------------------------------------------------------------

     ReadTypeIIPha


-------- Description: ----------------------------------------------------------

     This subroutine reads the SPECTRUM extension of a PHA file of type II in
     the following formats 
            PHAVERSN = 1992a format
            HDUVERS1 = 1.0.0
            HDUVERS1 = 1.1.0
            HDUVERS = 1.1.0


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

   int ReadTypeIIPha(fitsfile *fptr, int chatter, char *telescop, 
                     char *instrume, char *detnam, char *filter, char *phaversn,
		     char *hduclass, char *hduclas1, char *hduclas2, 
		     char *hduclas3, char *hduclas4, char *hduvers, 
		     long *fchan, double *texpos, float *areascal, 
		     int *nbackfil, char ***backfil, float **backscal, 
		     int *ncorrfil, char ***corrfil, float **corrscal, 
		     int *nrespfil, char ***respfil, int *nancrfil, 
		     char ***ancrfil, int *nXflt, char **xflt, char *dmode, 
		     long *detchans, char *chantyp, short **spec_num, 
		     short **channel, long **ipha, float **pha, char ***rowid, 
		     int *dtype, int *qerror, float **serr, int *qsys, 
		     float **syserr, int *qqual, short **quality, int *qgroup, 
		     short **grping, int *pois, long *nspec)

   Input Parameters:

        FPTR          fitsfile *  : Pointer to the input PHA file
        CHATTER       int         : chattiness flag for o/p (5 quite,10 normal,
	                            >20 silly)

   Output Parameters:

        TELESCOP      char        : Telescope name
        INSTRUME      char        : Instrument name
        DETNAM        char        : Specific detector name
	FILTER        char        : Filter in use
	PHAVERSN      char        : PHA file format version
	HDUCLASS      char        : Indicates the file format
	HDUCLAS1      char        : Indicates major class in the heirarchy
	HDUVERS       char        : The phaversn to be written
	HDUCLAS2      char        : Whether the spectrum is srce, bkgd, or both
	HDUCLAS3      char        : Indicates whether the data are stored as 
	                            counts or count rates
	FCHAN         long        : First possible value (TLMIN) of first column
	                            (Channel).  If TLMIN not found the default 
				    value 1 is assumed
	TEXPOS       double       : Exposure time
	AREASCAL     float        : Area scaling factor
	NBACKFIL     int          : Number of background files
	BACKFIL      char         : Pointer to the list of associated background
	                            filenames
	BACKSCAL     float        : Background scaling factor
	NCORRFIL     int          : Number of correction files
	CORRFIL      char         : Pointer to the list of associated correction
	                            filenames
	CORRSCAL     float        : Correction scaling factor
	NRESPFIL     int          : Number of response files
	RESPFIL      char         : Pointer to the list of associated response 
	                            filenames
	NANCRFIL     int          : Number of ancillary response files
	ANCRFIL      char         : Pointer to the list of associated ancillary
	                            response filenames
	NXFLT        int          : Number of XSPEC filter keywords
	XFLT         char         : XSPEC filter description for each XSPEC 
	                            filter keyword
	DMODE        char         : Datamode - for ASCA SIS - BRIGHT,FAINT etc 
	DETCHANS     long         : Number of possible detector channels
	CHANTYPE     char         : Channel type - PHA or PI
	SPEC_NUM     short        : Index to a spectrum in the SPECTRUM 
	                            extension
	CHANNEL      short        : Channel numbers in 1st column
        IPHA         long         : Data if written in the units of COUNTS
	PHA          float        : Data if written in the units of COUNTS/SEC
	ROWID        char         : Comment associated with a given spectrum in
	                            SPECTRUM ext
	DTYPE        int          : 1 if data is in COUNTS; 2 if data is written
	                            as RATES
	QERROR       int          : True if statistical errors are present in 
	                            the input file
	SERR         float        : Statistical errors if QERROR=TRUE
	QSYS         int          : True if systematic errors are present in the
	                            input file
	SYSERR       float        : Systematic errors if QSYS=TRUE
	QQUAL        int          : True if quality info is present in a column
	                            in the input file
	QUALITY      short        : Channel quality flags if QQUAL=TRUE
	QGROUP       int          : True if grouping info is present in a column
	                            in the input 
	             file
	GRPING       short        : Channel grouping flags if QGROUP=TRUE
	POIS         int          : True if Poission errors apply
        NSPEC        long         : Total number of rows present in the 
	                            extension (number of spectra)


-------- Include files ---------------------------------------------------------

   <stdio.h>
   <stdlib.h>
   <string.h>
   "fitsio.h"
   "general.h"


-------- Origin: ---------------------------------------------------------------

   This function is basically a C version of rdpha1.f.


-------- Authors/Modification History: -----------------------------------------
   kaa             (1.0.2; Dec 98)  Added support for 1.2.0 file format
   Sandhia Bansal  (1.0.1; Oct 98)  Included the documentation.
                                    Deleted nk_history, history, nk_comm and
				    comm from the argument list.
   Sandhia Bansal  (1.0.0; Jul 97)

----------------------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "constdef.h"

int ReadTypeIIPha(fitsfile *fptr, int chatter, char *telescop, char *instrume, 
		  char *detnam, char *filter, char *phaversn, char *hduclass, 
		  char *hduclas1, char *hduclas2, char *hduclas3, 
		  char *hduclas4, char *hduvers, long *fchan, double *texpos, 
		  float *areascal, int *nbackfil, char ***backfil, 
		  float **backscal, int *ncorrfil, char ***corrfil, 
		  float **corrscal, int *nrespfil, char ***respfil, 
		  int *nancrfil, char ***ancrfil, int *nXflt, char **xflt, 
		  char *dmode, long *detchans, char *chantyp, short **spec_num,
		  short **channel, long **ipha, float **pha, char ***rowid, 
		  int *dtype, int *qerror, float **serr, int *qsys, 
		  float **syserr, int *qqual, short **quality, int *qgroup, 
		  short **grping, int *pois, long *nspec)
{
   float enullval=0.0;
   float rval=0.0;

   long  pcount=0L;
   long  fchan2=0L;
   long  firstrow=1, firstelem=1;
   long  lnullval=0L;
   long  lvar=0L;
   long  repeat, width;

   int   tfields=0;
   int   status=0;
   int   colnum=0;
   int   numSpec=0;
   int   nchan=0L;
   int   i=0, j=0;
   int   anynul=0;
   int   typecode;
   int   qnotint;

   short snullval=0;

   char  version[7];
   char  tmp[FLEN_VALUE];
   char  extname[FLEN_VALUE];
   char  errstr[FLEN_ERRMSG];
   char  wrnstr[FLEN_ERRMSG];
   char  message[FLEN_ERRMSG];
   char  *ttype[NFIELDS_II];
   char  *tform[NFIELDS_II];
   char  *tunit[NFIELDS_II];
   char  *tcomm[NFIELDS_II];
   char  tlmin[FLEN_KEYWORD];
   char  comment[FLEN_VALUE];
   char  stnullval[FLEN_VALUE]={" "};
   
   /* Initializations */
   strcpy(version, "1.0.0");

   strcpy(errstr, "** ReadTypeIIPha ");
   strcat(strcat(errstr, version), " ERROR: ");

   strcpy(wrnstr, "** ReadTypeIIPha ");
   strcat(strcat(wrnstr, version), " WARNING: ");
   
   /* Give user info if requested */
   if (chatter >= 15)
   {
      strcpy(message, " ... using ReadTypeIIPha Version ");
      strcat(message, version);
   }

   /* Allocate space for the table parameters and initialize */
   for (i=0; i<NFIELDS_II; i++)
   {
      ttype[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
      tform[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
      tunit[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
      tcomm[i] = (char *) malloc(FLEN_CARD*sizeof(char));
      strcpy(ttype[i], " ");
   }

   /* Read binary table keywords from the CHU */
   if (fits_read_btblhdr(fptr, NFIELDS_II, nspec, &tfields, ttype, tform, 
			 tunit, extname, &pcount, &status))
      Printerror(status);
   numSpec = (int) *nspec;

   if (*nspec <= 0)
   {
      status = 1;
      DispMsg(1, 1, "Array size is zero in the input file");
   }
   
   else
   {
      /* Allocate space for spec_num
	 Other arrays will be allocated as needed */
      *spec_num = (short *) malloc(numSpec*sizeof(short)); 

      /* Determine which columns are present and set the appropriate flags */
      *dtype = *qerror = *qsys = *qqual = *qgroup = 0;

      for (i=0; i<NFIELDS_II; i++)
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
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading TELESCOP keyword", 
			 chatter, 20, status);
	 if (status == 225)
	    strcpy(telescop, "UNKNOWN");
	 status = 0;

	 /* Read INSTRUMENT */
	 strcpy(instrume, "UNKNOWN");
	 if (fits_read_key_str(fptr, "INSTRUME", instrume, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading INSTRUME keyword", 
			 chatter, 20, status);
	 if (status == 225)
	    strcpy(instrume, "UNKNOWN");
	 status = 0;

	 /* Read PHAVERSN */
	 strcpy(phaversn, " ");
	 if (fits_read_key_str(fptr, "PHAVERSN", phaversn, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading PHAVERSN keyword", 
			 chatter, 30, status);
	 status = 0;

	 /* Read HDUCLASS */
	 strcpy(hduclass, " ");
	 if (fits_read_key_str(fptr, "HDUCLASS", hduclass, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading HDUCLASS keyword", 
			 chatter, 30, status);
	 status = 0;

	 /* Read HDUCLAS1 */
	 strcpy(hduclas1, " ");
	 if (fits_read_key_str(fptr, "HDUCLAS1", hduclas1, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading HDUCLAS1 keyword", 
			 chatter, 30, status);
	 status = 0;

	 /* Read HDUCLAS2 */
	 strcpy(hduclas2, " ");
	 if (fits_read_key_str(fptr, "HDUCLAS2", hduclas2, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading HDUCLAS2 keyword", 
			 chatter, 30, status);
	 status = 0;

	 /* Read HDUCLAS3 */
	 strcpy(hduclas3, " ");
	 if (fits_read_key_str(fptr, "HDUCLAS3", hduclas3, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading HDUCLAS3 keyword", 
			 chatter, 30, status);
	 status = 0;

	 /* Read HDUCLAS4 */
	 strcpy(hduclas4, " ");
	 if (fits_read_key_str(fptr, "HDUCLAS4", hduclas4, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading HDUCLAS4 keyword", 
			 chatter, 30, status);
	 status = 0;

	 /* Read HDUVERS/HDUVERS1 */
	 strcpy(hduvers, " ");
	 if (fits_read_key_str(fptr, "HDUVERS", hduvers, comment, &status))
	 {
	    PrintWarning("ReadTypeIIPha", version, 
			 "Could not find HDUVERS - trying HDUVERS1",
			 chatter, 30, status);
	    if (strcmp(hduvers, " ") == 0)
	    {
	       status = 0;
	       if (fits_read_key_str(fptr, "HDUVERS1", hduvers, comment, 
				     &status))
	          PrintWarning("ReadTypeIIPha", version, 
			       "Error while reading HDUVERS1 keyword", 
			       chatter, 30, status);
	    }
	 }
	 status = 0;

	 /* Read DETNAM */
	 strcpy(detnam, " ");
	 if (fits_read_key_str(fptr, "DETNAM", detnam, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading DETNAM keyword", 
			 chatter, 20, status);
	 status = 0;

	 /* Read FILTER */
	 strcpy(filter, " ");
	 if (fits_read_key_str(fptr, "FILTER", filter, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading FILTER keyword", 
			 chatter, 20, status);
	 status = 0;

	 /* Read TEXPOS */
	 *texpos = -1.0;
	 if (fits_read_key_dbl(fptr, "EXPOSURE", texpos, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading FILTER keyword", 
			 chatter, 10, status);
	 if (status == 225)
	    PrintWarning("ReadTypeIIPha", version, 
			 "Exposure time not found - exposure set to -1",
			 chatter, 10, status);
	 status = 0;

	 /* Read AREASCAL */
	 *areascal = 1.0;
	 if (fits_read_key_flt(fptr, "AREASCAL", areascal, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading AREASCAL keyword", 
			 chatter, 10, status);
	 status = 0;

	 /* Read BACKSCAL */
	 *backscal = (float *) malloc(numSpec*sizeof(float));
	 for (i=0; i<*nspec; i++)
	    (*backscal)[i] = 0.0;
	 if (fits_read_key_flt(fptr, "BACKSCAL", *backscal, comment, &status))
	    status = 0;

	 /* Read BACKFILE */
	 *nbackfil = 0;
	 *backfil = (char **) malloc(numSpec*sizeof(char *));
	 for (i=0; i<*nspec; i++)
	    (*backfil)[i] = (char *) malloc(SWGLEN*sizeof(char));
	 if (fits_read_key_str(fptr, "BACKFILE", (*backfil)[0], comment, 
			       &status))
	    status = 0;
	 else
	 {
	    if (strcmp((*backfil)[0], " ") > 0)
	       (*nbackfil)++;
	 }

	 /* Read corrSCAL */
	 *corrscal = (float *) malloc(numSpec*sizeof(float));
	 for (i=0; i<*nspec; i++)
	    (*corrscal)[i] = 0.0;
	 if (fits_read_key_flt(fptr, "CORRSCAL", *corrscal, comment, &status))
	    status = 0;

	 /* Read CORRFILE */
	 *ncorrfil = 0;
	 *corrfil = (char **) malloc(numSpec*sizeof(char *));
	 for (i=0; i<*nspec; i++)
	    (*corrfil)[i] = (char *) malloc(SWGLEN*sizeof(char));
	 if (fits_read_key_str(fptr, "CORRFILE", (*corrfil)[0], comment, 
			       &status))
	    status = 0;
	 else
	    if (strcmp((*corrfil)[0], " ") > 0)
	       (*ncorrfil)++;

	 /* Read RESPFILE */
	 *nrespfil = 0;
	 *respfil = (char **) malloc(numSpec*sizeof(char *));
	 for (i=0; i<*nspec; i++)
	    (*respfil)[i] = (char *) malloc(SWGLEN*sizeof(char));
	 if (fits_read_key_str(fptr, "RESPFILE", (*respfil)[0], comment, 
			       &status))
	    status = 0;
	 else
	 {
	    if (strcmp((*respfil)[0], " ") > 0)
	       (*nrespfil)++;
	 }

	 /* Read ANCRFILE */
	 *nancrfil = 0;
	 *ancrfil = (char **) malloc(numSpec*sizeof(char *));
	 for (i=0; i<*nspec; i++)
	    (*ancrfil)[i] = (char *) malloc(SWGLEN*sizeof(char));
	 if (fits_read_key_str(fptr, "ANCRFILE", (*ancrfil)[0], comment, 
			       &status))
	    status = 0;
	 else
	 {
	    if (strcmp((*ancrfil)[0], " ") > 0)
	       (*nancrfil)++;
	 }

	 /* Read DETCHANS */
	 if (fits_read_key_lng(fptr, "DETCHANS", detchans, comment, &status))
	    Printerror(status);

	 /* Read CHANTYPE */
	 if (fits_read_key_str(fptr, "CHANTYPE", chantyp, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading CHANTYPE keyword", 
			 chatter, 20, status);
	 status = 0;

	 /* Read POISSERR */
	 *pois = 0;
	 if (fits_read_key_log(fptr, "POISSERR", pois, comment, &status))
	 {
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading POISSERR keyword", 
			 chatter, 15, status);
	    PrintWarning("ReadTypeIIPha", version, 
			 "POISSERR assumed to be false", 
			 chatter, 10, status);
	 }
	 status = 0;

	 /* Read XSPEC FILTER */
	 if (fits_read_keys_str(fptr, "XFLT", 1, 9999, xflt, nXflt, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading XFLT keyword", 
			 chatter, 30, status);
	 status = 0;
	
	 /* Read DATAMODE */
	 strcpy(dmode, " ");
	 if (fits_read_key_str(fptr, "DATAMODE", dmode, comment, &status))
	    PrintWarning("ReadTypeIIPha", version, 
			 "Error while reading DATAMODE keyword", 
			 chatter, 10, status);
	 status = 0;

	 /* move to the beginning of the CHU
	 if (fits_read_record(fptr, 0, tmp, &status))
	    Printerror(status);*/

	 /* READ DATA */

	 /* SPEC_NUM */

	 if (fits_get_colnum(fptr, 0, "SPEC_NUM", &colnum, &status))
	 {
	    if (fits_get_colnum(fptr, 0, " SPEC_NUM", &colnum, &status))
	       Printerror(status);
	 }

	 if (fits_read_col_sht(fptr, colnum, firstrow, firstelem, *nspec, 
			       snullval, *spec_num, &anynul, &status))
	    Printerror(status);

	 if (fits_get_colnum(fptr, 0, "CHANNEL", &colnum, &status))
	 {
	    if (fits_get_colnum(fptr, 0, " CHANNEL", &colnum, &status))
	       Printerror(status);
	 }

	 /* Read TLMINx keyword, where x is channel column number 
	    to determine first possible channel value */
	 sprintf(tlmin, "TLMIN%d", colnum);
	 if (fits_read_key_lng(fptr, tlmin, fchan, comment, &status))
	    Printerror(status);

	 /* CHANNEL */
	 nchan = (int) (*nspec * (*detchans));
	 *channel  = (short *) malloc(nchan*sizeof(short)); 
	 if (fits_read_col_sht(fptr, colnum, firstrow, firstelem, nchan, 
			       snullval, *channel, &anynul, &status))
	    Printerror(status);

	 /* COUNTS or RATE */
	 if (*dtype == 1)
	 {
	    if (fits_get_colnum(fptr, 0, "COUNTS", &colnum, &status))
	       Printerror(status);

	    *pha = (float *) malloc(nchan*sizeof(float)); 
	    if (fits_read_col_flt(fptr, colnum, firstrow, firstelem, nchan, 
				  enullval, *pha, &anynul, &status))
	       Printerror(status);

	    *ipha = (long *) malloc(nchan*sizeof(long));  
            qnotint = 0;
	    for (i=0; i<nchan; i++) {
	      *ipha[i] = (long)(*pha[i]);
	      if ( abs(*ipha[i] - *pha[i]) > 1e-5 ) qnotint = 1;
	    }
	    if ( qnotint == 1 ) {
	      PrintWarning("ReadTypeIIPha", version, "**WARNING: COUNTS column is non-integer and will be truncated\n", chatter, 0, status);
	    }

	 }
	 
	 else if (*dtype == 2)
	 {
	    if (fits_get_colnum(fptr, 0, "RATE", &colnum, &status))
	       Printerror(status);

	    *pha = (float *) malloc(nchan*sizeof(float)); 
	    if (fits_read_col_flt(fptr, colnum, firstrow, firstelem, nchan, 
				  enullval, *pha, &anynul, &status))
	       Printerror(status);
	 }
	 
	 /* ROWID */
	 if (fits_get_colnum(fptr, 0, "ROWID", &colnum, &status))
	    Printerror(status);

	 *rowid = (char **) malloc(numSpec*sizeof(char *));
	 for (i=0; i<*nspec; i++)
	    (*rowid)[i] = (char *) malloc(SWGLEN*sizeof(char));
	 if (fits_read_col_str(fptr, colnum, firstrow, firstelem, *nspec, 
			        stnullval, *rowid, &anynul, &status))
	    Printerror(status);

	 /* STAT_ERR */
	 *serr = (float *) malloc(nchan*sizeof(float)); 
	 for (i=0; i<nchan; i++)
	    (*serr)[i] = 0.0;

	 if (*qerror == 1)	    
	 {
	    if (fits_get_colnum(fptr, 0, "STAT_ERR", &colnum, &status))
	       Printerror(status);

	    if (fits_read_col_flt(fptr, colnum, firstrow, firstelem, nchan, 
				  enullval, *serr, &anynul, &status))
	       Printerror(status);
	 }
	 
	 else if (*pois == 0)
	 {
	    if (fits_read_key_flt(fptr, "STAT_ERR", &rval, comment, &status))
	       status = 0;
	    else
	    {
	       for (i=0; i<nchan; i++)
		  (*serr)[i] = rval;
	       *qerror = 1;
	    }
	 }

	 /* SYS_ERR */
	 *syserr = (float *) malloc(nchan*sizeof(float)); 
	 for (i=0; i<nchan; i++)
	    (*syserr)[i] = 0.0;

	 if (*qsys == 1)
	 {
	    if (fits_get_colnum(fptr, 0, "SYS_ERR", &colnum, &status))
	       Printerror(status);

	    if (fits_read_col_flt(fptr, colnum, firstrow, firstelem, nchan, 
				  enullval, *syserr, &anynul, &status))
	       Printerror(status);
	 }
	 
	 else
	 {
	    if (fits_read_key_flt(fptr, "SYS_ERR", &rval, comment, &status))
	       status = 0;
	    else
	    {
	       for (i=0; i<nchan; i++)
		  (*syserr)[i] = rval;
	       *qsys = 1;
	    }
	 }

	 /* QUALITY */
	 *quality = (short *) malloc(nchan*sizeof(short)); 
	 for (i=0; i<nchan; i++)
	    (*quality)[i] = 0;

	 if (*qqual == 1)
	 {
	    if (fits_get_colnum(fptr, 0, "QUALITY", &colnum, &status))
	       Printerror(status);

	    if (fits_read_col_sht(fptr, colnum, firstrow, firstelem, nchan, 
				  snullval, *quality, &anynul, &status))
	       Printerror(status);
	 }
	 
	 else
	 {
	    if (fits_read_key_lng(fptr, "QUALITY", &lvar, comment, &status))
	       status = 0;
	    else
	    {
	       for (i=0; i<nchan; i++)
		  (*quality)[i] = (short) lvar;
	       *qqual = 1;
	    }
	 }
	 
	 /* GROUPING */
	 *grping = (short *) malloc(nchan*sizeof(short)); 
	 for (i=0; i<nchan; i++)
	    (*grping)[i] = 0;

	 if (*qgroup == 1)
	 {
	    if (fits_get_colnum(fptr, 0, "GROUPING", &colnum, &status))
	       Printerror(status);

	    if (fits_read_col_sht(fptr, colnum, firstrow, firstelem, nchan, 
				  snullval, *grping, &anynul, &status))
	       Printerror(status);
	 }
	 
	 else
	 {
	    if (fits_read_key_lng(fptr, "GROUPING", &lvar, comment, &status))
	       status = 0;
	    else
	    {
	       for (i=0; i<nchan; i++)
		  (*grping)[i] = (short) lvar;
	       *qgroup = 1;
	    }
	 }
	 
	 /* BKGFILE */
	 if (*nbackfil == 0)
	 {
	    if (fits_get_colnum(fptr, 0, "BACKFILE", &colnum, &status))
	       status = 0;
	    else
	    {
	       *nbackfil = *nspec;
	       if (fits_read_col_str(fptr, colnum, firstrow, firstelem,
				     *nspec, stnullval, *backfil, &anynul,
				     &status))
		  Printerror(status);
	    }
	 }

	 /* BACKSCAL */
	 if (fits_get_colnum(fptr, 0, "BACKSCAL", &colnum, &status))
	    status = 0;
	 else
	 {
	    if (fits_read_col_flt(fptr, colnum, firstrow, firstelem,
				  *nspec, enullval, *backscal, &anynul,
				  &status))
	       Printerror(status);
	 }

	 /* CORRFILE */
	 if (*ncorrfil == 0)
	 {
	    if (fits_get_colnum(fptr, 0, "CORRFILE", &colnum, &status))
	       status = 0;
	    else
	    {
	       *ncorrfil = *nspec;
	       if (fits_read_col_str(fptr, colnum, firstrow, firstelem,
				     *nspec, stnullval, *corrfil, &anynul,
				     &status))
		  Printerror(status);
	    }
	 }

	 /* CORRSCAL */
	 if (fits_get_colnum(fptr, 0, "CORRSCAL", &colnum, &status))
	    status = 0;
	 else
	 {
	    if (fits_read_col_flt(fptr, colnum, firstrow, firstelem,
				  *nspec, enullval, *corrscal, &anynul,
				  &status))
	       Printerror(status);
	 }

	 /* RESPFILE */
	 if (*nrespfil == 0)
	 {
	    if (fits_get_colnum(fptr, 0, "RESPFILE", &colnum, &status))
	       status = 0;
	    else
	    {
	       *nrespfil = *nspec;
	       if (fits_read_col_str(fptr, colnum, firstrow, firstelem,
				     *nspec, stnullval, *respfil, &anynul,
				     &status))
		  Printerror(status);
	    }
	 }

	 /* ANCRFILE */
	 if (*nancrfil == 0)
	 {
	    if (fits_get_colnum(fptr, 0, "ANCRFILE", &colnum, &status))
	       status = 0;
	    else
	    {
	       *nancrfil = *nspec;
	       if (fits_read_col_str(fptr, colnum, firstrow, firstelem,
				     *nspec, stnullval, *ancrfil, &anynul,
				     &status))
		  Printerror(status);
	    }
	 }
      }

      for (i=0; i<NFIELDS_II; i++)
      {
	 free(ttype[i]);
	 free(tform[i]);
	 free(tunit[i]);
	 free(tcomm[i]);
      }
   }

   return(status);
}

