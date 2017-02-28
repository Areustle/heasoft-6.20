#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "constdef.h"

int writephaIIkeywd(fitsfile *fptr, int chatter, int nk_history, 
                    char *history[], int nk_comm, char *com[], 
                    char *extname, char *telescop, char *instrume, 
                    char *detnam, char *filter, char *phaversn, char *hduclas2, 
	            long fchan, double texpos, double areascal, int nbackscal,
                    double backscal, int ncorrscal, double corrscal, 
                    int nbackfil, char *backfil, int ncorrfil, char *corrfil,
                    int nancrfil, char *ancrfil, int ancrfil_len, int nrespfil, 
                    char *respfil, int respfil_len, char *dmode, long detchans,
                    char *chantyp, int dtype, int qerror, int qsys, int qqual, 
                    int qgroup, long nchan, long nspec)
{
    int i, j;
    int status = 0;

    int tfields=0;
    int ikeyval;
    int colnum;

    short *conv2 = (short *) malloc(nchan*sizeof(short));

    long *conv4 = (long *) malloc(nchan*sizeof(long));
    long lkeyval;
    long nrows=0;
    long firstrow=1L, firstelem=1L;
    long tlmax = fchan+detchans-1L;
   
    char version[7];
    char errstr[FLEN_ERRMSG];   
    char wrnstr[FLEN_ERRMSG];
    char message[FLEN_ERRMSG];
    char hduvers1[6];   
    char *ttype[NFIELDS_II];
    char *tform[NFIELDS_II];
    char *tunit[NFIELDS_II];
    char *tcomm[NFIELDS_II];
    char keyname[FLEN_KEYWORD];
    char ckeyval[FLEN_KEYWORD];
    char *rowid[1];
    char tmp[FLEN_VALUE];
    char function[] = {"writephaIIkeywd"};


   /* Initializations */
   strcpy(version, "1.0.0");

   sprintf(errstr, "** %s ", function);
   strcat(strcat(errstr, version), " ERROR: ");

   sprintf(wrnstr, "** %s ", function);
   strcat(strcat(wrnstr, version), " WARNING: ");
   
   sprintf(message, " ... using %s Version ", function);
   strcat(message, version);
   DispMsg(chatter, 10, message);

   /* Create a new extension with required keywords */
   for (i=0; i<NFIELDS_II; i++)
   {
       ttype[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
       tform[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
       tunit[i] = (char *) malloc(FLEN_VALUE*sizeof(char));
       tcomm[i] = (char *) malloc(FLEN_CARD*sizeof(char));
       strcpy(ttype[i], "");
       strcpy(tform[i], "");
       strcpy(tunit[i], "");
       strcpy(tcomm[i], "");
   }

   strcpy(ttype[tfields], "SPEC_NUM");
   strcpy(tform[tfields], "1I");
   tfields++;

   strcpy(ttype[tfields], "ROWID");
   strcpy(tform[tfields], "20A");
   tfields++;

   strcpy(ttype[tfields], "CHANNEL");
   sprintf(tmp, "%dI", nchan);
   strcpy(tform[tfields], tmp);
   strcpy(tunit[tfields], " ");
   if ((strcmp(chantyp, "PHA") == 0) || (strcmp(chantyp, "pha") == 0))
      strcpy(tcomm[tfields], "Pulse Height Analyser (PHA) Channel ");
   else if ((strcmp(chantyp, "PI") == 0) || (strcmp(chantyp, "pi") == 0))
      strcpy(tcomm[tfields], "Pulse Invarient (PI) Channel ");
   else
      strcpy(tcomm[tfields], "Detector channel (type unknown)");

   tfields++;

   if (dtype == 1)
   {
       strcpy(ttype[tfields], "COUNTS");
       sprintf(tmp, "%dJ", nchan);
       strcpy(tform[tfields], tmp);
       strcpy(tunit[tfields], "count");
       strcpy(tcomm[tfields], "Counts per channel");
       tfields++;
   }
   else if (dtype == 2)
   {
       strcpy(ttype[tfields], "RATE");
       sprintf(tmp, "%dE", nchan);
       strcpy(tform[tfields], tmp);
       strcpy(tunit[tfields], "count/s");
       strcpy(tcomm[tfields], "Counts per second per channel");
       tfields++;
   }

   if (qerror != 0)
   {
	 strcpy(ttype[tfields], "STAT_ERR");
         sprintf(tmp, "%dE", nchan);
         strcpy(tform[tfields], tmp);
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
         sprintf(tmp, "%dE", nchan);
         strcpy(tform[tfields], tmp);
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
         sprintf(tmp, "%dI", nchan);
         strcpy(tform[tfields], tmp);
	 strcpy(tunit[tfields], " ");
	 strcpy(tcomm[tfields], "Quality flag of this channel (0=good)");
	 tfields++;
   }

   if (qgroup != 0)
   {
	 strcpy(ttype[tfields], "GROUPING");
         sprintf(tmp, "%dI", nchan);
         strcpy(tform[tfields], tmp);
	 strcpy(tunit[tfields], " ");
	 strcpy(tcomm[tfields], "Grouping flag of this channel (0=undefined)");
	 tfields++;
   }


   if (nbackfil > 1)
   {
      strcpy(ttype[tfields], "BACKFILE");
      sprintf(tform[tfields], "%dA", strlen(backfil));
      strcpy(tcomm[tfields], "Associated Background Filename");
      tfields++;

      strcpy(ttype[tfields], "BACKSCAL");
      sprintf(tmp, "%dE", nchan);
      strcpy(tform[tfields], tmp);
      strcpy(tcomm[tfields], "Background File Scaling Factor");
      tfields++;
   }

   if (ncorrfil > 1)
   {
      strcpy(ttype[tfields], "CORRFILE");
      sprintf(tform[tfields], "%dA", strlen(corrfil));
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
      /**sprintf(tform[tfields], "%dA", strlen(respfil));**/
      sprintf(tform[tfields], "%dA", respfil_len);
      strcpy(tcomm[tfields], "Associated Redistrib Matrix Filename");
      tfields++;
   }


   if (nancrfil > 1)
   {
      strcpy(ttype[tfields], "ANCRFILE");
      /**sprintf(tform[tfields], "%dA", strlen(ancrfil));**/
      sprintf(tform[tfields], "%dA", ancrfil_len);
      strcpy(tcomm[tfields], "Associated Ancillary Response Filename");
      tfields++;
   }

   /* Create binary table */
   if (fits_create_tbl(fptr, BINARY_TBL, nrows, tfields, ttype, tform, tunit,
                       extname, &status)) Printerror(status);

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

  /** if status ==0 then continue.  Otherwise fatal error **********/
  if (status == 0)
  {
     if (!strcmp(phaversn, "1.0.0") || !strcmp(phaversn, "1.1.0"))
        strcpy(hduvers1, phaversn);
     else
     {
        strcpy(hduvers1, "1.1.0");
        strcpy(message, wrnstr);
        strcat(strcat(message, " Unknown format for phaversn: "), phaversn);
        DispMsg(chatter, 10, message);

        strcpy(message, " ...... Resetting format (HDUVERS1) to ");
        strcat(message, hduvers1);
        DispMsg(chatter, 10, message);
     }

     /* Write the HDUCLASn and HDUVERSn keywords */

     if (fits_write_key(fptr, TSTRING, "HDUCLASS", "OGIP",
                        "Format confirms to OGIP standard", &status))
        Printerror(status);

     if (fits_write_key(fptr, TSTRING, "HDUCLAS1", "SPECTRUM",
                        "PHA dataset (OGIP memo OGIP-92-007)", &status))
        Printerror(status);

     if (fits_write_key(fptr, TSTRING, "HDUVERS1", hduvers1,
                        "Version of format (OGIP memo OGIP-92-007a)", &status))
        Printerror(status);

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

     if (fits_write_key_fixdbl(fptr, "AREASCAL", areascal, DECIMALS,
                               "Area scaling factor", &status))
        Printerror(status);

     if (nbackfil <= 1)
     {
         if (strcmp(backfil, "none") == 0) strcpy(backfil, "NONE");
         if (fits_write_key(fptr, TSTRING, "BACKFILE", backfil,
                        "Associated background filename", &status))
                Printerror(status);

         if (fits_write_key_fixdbl(fptr, "BACKSCAL", backscal, DECIMALS,
                               "Background file scaling factor", &status))
                Printerror(status);
     }

     if (ncorrfil <= 1)
     {
         if (strcmp(corrfil, "none") == 0) strcpy(corrfil, "NONE");
         if (fits_write_key(fptr, TSTRING, "CORRFILE", corrfil,
                            "Associated correction filename", &status))
                    Printerror(status);

         if (fits_write_key_fixdbl(fptr, "CORRSCAL", corrscal, DECIMALS,
                                "Correction file scaling factor", &status))
                    Printerror(status);
     }

     if (nrespfil <= 1)
     {
         if (strcmp(respfil, "none") == 0) strcpy(respfil, "NONE");
         if (fits_write_key(fptr, TSTRING, "RESPFILE", respfil,
                            "Associated redistrib matrix filename", &status))
                    Printerror(status);
     }

     if (nancrfil <= 1)
     {
         if (strcmp(ancrfil, "none") == 0) strcpy(ancrfil, "NONE");
         if (fits_write_key(fptr, TSTRING, "ANCRFILE", ancrfil,
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
        lkeyval = 1L;
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
     /* add one blank line of history */

     strcpy(message, "                            ");
     if (fits_write_history(fptr, message, &status)) Printerror(status);

     i=0;
     while ((i<nk_history) && (status==0))
     {
        if (fits_write_history(fptr, history[i], &status))
           Printerror(status);
        i++;
     }

     strcpy(message, "FITS SPECTRUM extension written by WritephaIIkeywd ");
     strcat(message, version);
     if (fits_write_history(fptr, message, &status))
        Printerror(status);

     strcpy(message, "               ");
     if (fits_write_history(fptr, message, &status)) Printerror(status);

     DispMsg(chatter, 20, " ... Written the history keywords");

     /* Add the (passed) comment cards */
     i=0;
     while ((i<nk_comm) && (status==0))
     {
        if (fits_write_comment(fptr, com[i], &status))
           Printerror(status);
        i++;
     }

     strcpy(message, "          "); 
     if (fits_write_comment(fptr, message, &status)) Printerror(status);

      /* put the date    */
     /*if (ffpdat(fptr, &status)) Printerror(status);*/

     DispMsg(chatter, 10, " ... Written the comment header keywords");
     DispMsg(chatter, 10, " ... Written the OGIP required keywords");

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


