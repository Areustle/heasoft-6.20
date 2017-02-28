#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "constdef.h"

int readphaIIkeywd(fitsfile *fptr, int chatter, char *telescop, char *instrume, 
          char *detnam, char *filter, char *phaversn, char *hduclass, 
          char *hduclas1, char *hduclas2, char *hduclas3, char *hduclas4, 
          char *hduvers1, long *fchan, double *texpos, 
          double *areascal, int *nbackscal, int *ncorrscal, int *nbackfil, 
          int *ncorrfil, int *nrespfil,  int *nancrfil, 
	  char *dmode, long *detchans, char *chantyp, 
	  int *dtype, int *qerror,  int *qsys, 
	  int *qqual, int *qgroup, 
	  int *pois, long *nspec)
{
   double enullval=0.0;
   double rval=0.0;
   double  tmpscal;

   long  pcount=0L;
   long  fchan2=0L;
   long  firstrow=1, firstelem=1;
   long  lnullval=0L;
   long  lvar=0L;
   long   rows=0;
   int   tfields=0;
   int   status=0;
   int   colnum=0;
   int   numSpec=0;
   int   nchan=0L;
   int   i=0, j=0;
   int   anynul=0;
   int   hdutype=0;
   int   hdunum = 0, chan_colnum=0;
   int   hdunumold=0, hdutot = 0;
   short snullval=0;
   int flagchan = 0;

   char version[7];
   char tmp[FLEN_VALUE];
   char extname[FLEN_VALUE];
   char keyname[FLEN_VALUE];
   char errstr[FLEN_ERRMSG];
   char wrnstr[FLEN_ERRMSG];
   char message[FLEN_ERRMSG];
   char *ttype[NFIELDS_II];
   char *tform[NFIELDS_II];
   char *tunit[NFIELDS_II];
   char *tcomm[NFIELDS_II];
   char tlmin[FLEN_KEYWORD];
   char comment[FLEN_VALUE];
   char stnullval[FLEN_VALUE]={" "};
   char tmpfil[FLEN_VALUE];
   char function[] = {"readphaIIkeywd"};

   /* Initializations */
   strcpy(version, "1.0.0");

   sprintf(errstr, "** %s ", function);
   strcat(strcat(errstr, version), " ERROR: ");

   sprintf(wrnstr, "** %s ", function);
   strcat(strcat(wrnstr, version), " WARNING: ");
   
   /* Give user info if requested */
   if (chatter >= 10)
   {
      sprintf(message, " ... using %s Version ", function);
      strcat(message, version);
   }

   /* Allocate space for the table parameters and initialize */
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
	    PrintWarning(function, version, " ... TELESCOP keyword not found ", 
			 chatter, 10, status);
         strcpy(message, " ...... setting TELESCOP to ");
         strcat(message, "UNKNOWN");
         DispMsg(1, 10, message);

	 if (status == 225) strcpy(telescop, "UNKNOWN");
	 status = 0;

	 /* Read INSTRUMENT */
	 strcpy(instrume, "UNKNOWN");
	 if (fits_read_key_str(fptr, "INSTRUME", instrume, comment, &status))
	    PrintWarning(function, version, " ... while reading INSTRUME keyword", 
			 chatter, 10, status);
         strcpy(message, " ...... setting INSTRUMENT to ");
         strcat(message, "UNKNOWN");
         DispMsg(1, 10, message);

	 if (status == 225) strcpy(instrume, "UNKNOWN");
	 status = 0;

	 /* Read PHAVERSN */
	 strcpy(phaversn, " ");
	 if (fits_read_key_str(fptr, "PHAVERSN", phaversn, comment, &status))
	    PrintWarning(function, version, " ... PHAVERSN keyword not found", 
			 chatter, 30, status);
      
	 status = 0;
         
	 /* Read HDUCLASS */
	 strcpy(hduclass, " ");
	 if (fits_read_key_str(fptr, "HDUCLASS", hduclass, comment, &status))
	    PrintWarning(function, version, " ... HDUCLASS keyword not found", 
			 chatter, 30, status);
	 status = 0;
 
	 /* Read HDUCLAS1 */
	 strcpy(hduclas1, " ");
	 if (fits_read_key_str(fptr, "HDUCLAS1", hduclas1, comment, &status))
	    PrintWarning(function, version, " ... HDUCLAS1 keyword not found", 
			 chatter, 30, status);
	 status = 0;

	 /* Read HDUCLAS2 */
	 strcpy(hduclas2, " ");
	 if (fits_read_key_str(fptr, "HDUCLAS2", hduclas2, comment, &status))
	    PrintWarning(function, version, " ... HDUCLAS2 keyword not found", 
			 chatter, 30, status);
	 status = 0;

	 /* Read HDUCLAS3 */
	 strcpy(hduclas3, " ");
	 if (fits_read_key_str(fptr, "HDUCLAS3", hduclas3, comment, &status))
	    PrintWarning(function, version, " ... HDUCLAS3 keyword not found", 
			 chatter, 30, status);
	 status = 0;

	 /* Read HDUCLAS4 */
	 strcpy(hduclas4, " ");
	 if (fits_read_key_str(fptr, "HDUCLAS4", hduclas4, comment, &status))
	    PrintWarning(function, version, " ... HDUCLAS4 keyword not found", 
			 chatter, 30, status);
	 status = 0;

	 /* Read HDUVERS1 */
	 strcpy(hduvers1, " ");
	 if (fits_read_key_str(fptr, "HDUVERS1", hduvers1, comment, &status))
	    PrintWarning(function, version, " ... HDUVERS1 keyword not found", 
			 chatter, 30, status);
	 status = 0;

	 /* Read DETNAM */
	 strcpy(detnam, " ");
	 if (fits_read_key_str(fptr, "DETNAM", detnam, comment, &status))
	    PrintWarning(function, version, " ... DETNAM keyword not found", 
			 chatter, 10, status);
	 status = 0;

	 /* Read FILTER */
	 strcpy(filter, " ");
	 if (fits_read_key_str(fptr, "FILTER", filter, comment, &status))
	    PrintWarning(function, version, " ... FILTER keyword not found", 
			 chatter, 10, status);
	 status = 0;

	 /* Read TEXPOS */
	 *texpos = -1.0;
	 if (fits_read_key_dbl(fptr, "EXPOSURE", texpos, comment, &status))
	    PrintWarning(function, version, " ... EXPOSURE keyword not found", 
			 chatter, 10, status);
	 if (status == 225)
	    PrintWarning(function, version, 
			 "Exposure time not found - exposure set to -1",
			 chatter, 10, status);
	 status = 0;

	 /* Read AREASCAL */
	 *areascal = 1.0;
	 if (fits_read_key_dbl(fptr, "AREASCAL", areascal, comment, &status))
	    PrintWarning(function, version, " ... AREASCAL keyword not found", 
			 chatter, 10, status);
	 status = 0;

	 /* Read BACKSCAL */
         *nbackscal = 0;
	 if (fits_read_key_dbl(fptr, "BACKSCAL", &tmpscal, comment, &status))
	    status = 0;
         else
            *nbackscal = 1;

	 /* Read BACKFILE */
	 *nbackfil = 0;
	 if (fits_read_key_str(fptr, "BACKFILE", tmpfil, comment, &status))
	    status = 0;
	 else
	    (*nbackfil)++;

	 /* Read CORRSCAL */
         *ncorrscal = 0;
	 if (fits_read_key_dbl(fptr, "CORRSCAL", &tmpscal, comment, &status))
	    status = 0;
         else
             *ncorrscal = 1;

	 /* Read CORRFILE */
	 *ncorrfil = 0;
	 if (fits_read_key_str(fptr, "CORRFILE", tmpfil, comment, &status))
	    status = 0;
	 else
	    (*ncorrfil)++;

	 /* Read RESPFILE */
	 *nrespfil = 0;
	 if (fits_read_key_str(fptr, "RESPFILE", tmpfil, comment, 
			       &status))
	    status = 0;
	 else
            (*nrespfil)++;

	 /* Read ANCRFILE */
	 *nancrfil = 0;
	 if (fits_read_key_str(fptr, "ANCRFILE", tmpfil, comment, &status))
	    status = 0;
	 else
	    (*nancrfil)++;

	 /* Read DETCHANS */
	 if (fits_read_key_lng(fptr, "DETCHANS", detchans, comment, &status))
	   {
	     printf("Warning... DETCHANS keyword not found!..trying read from other HDU...\n ");
             flagchan = 1;
	     status=0;
	   }

           
	 /* Read CHANTYPE */
         status = 0; 
	 if (fits_read_key_str(fptr, "CHANTYPE", chantyp, comment, &status))
	    printf( "Warning ... CHANTYPE keyword not found!\n");
	 status = 0;

	 /* Read POISSERR */
	 *pois = 0;
	 if (fits_read_key_log(fptr, "POISSERR", pois, comment, &status))
	 {
	    printf( "Warning ... POISSERR keyword not found!\n");
	    PrintWarning("Warning...POISSERR assumed to be false", 
			 chatter, 10, status);
	 }
	 status = 0;

	 /* Read DATAMODE */
	 strcpy(dmode, " ");
	 if (fits_read_key_str(fptr, "DATAMODE", dmode, comment, &status))
	    PrintWarning(function, version, " ... DATAMODE keyword not found", 
                         chatter, 10, status);

	 status = 0;

	 /* move to the beginning of the CHU
	 if (fits_read_record(fptr, 0, tmp, &status))
	    Printerror(status);*/

	 
	 /* BKGFILE */
	 if (*nbackfil == 0)
	 {
	    if (fits_get_colnum(fptr, 0, "BACKFILE", &colnum, &status))
	       status = 0;
	    else
	       *nbackfil = *nspec;
	 }

#ifdef UNDEF

	 /* BACKSCAL */
	 if (fits_get_colnum(fptr, 0, "BACKSCAL", &colnum, &status))
	    status = 0;
	 else
	 {
	    if (fits_read_col_dbl(fptr, colnum, firstrow, firstelem,
				  *nspec, enullval, *backscal, &anynul,
				  &status))
	       Printerror(status);
	 }
#endif

	 /* CORRFILE */
	 if (*ncorrfil == 0)
	 {
	    if (fits_get_colnum(fptr, 0, "CORRFILE", &colnum, &status))
	       status = 0;
	    else
	       *ncorrfil = *nspec;
	 }
#ifdef UNDEF

	 /* CORRSCAL */
	 if (fits_get_colnum(fptr, 0, "CORRSCAL", &colnum, &status))
	    status = 0;
	 else
	 {
	    if (fits_read_col_dbl(fptr, colnum, firstrow, firstelem,
				  *nspec, enullval, *corrscal, &anynul,
				  &status))
	       Printerror(status);
	 }

#endif

	 /* RESPFILE */
	 if (*nrespfil == 0)
	 {
	    if (fits_get_colnum(fptr, 0, "RESPFILE", &colnum, &status))
	       status = 0;
	    else
	       *nrespfil = *nspec;
	 }

	 /* ANCRFILE */
	 if (*nancrfil == 0)
	 {
	    if (fits_get_colnum(fptr, 0, "ANCRFILE", &colnum, &status))
	       status = 0;
	    else
	       *nancrfil = *nspec;
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

   if (flagchan == 1)
     {
       ffghdn(fptr, &hdunum);
       hdunumold = hdunum;
       if (ffthdu(fptr, &hdutot, &status)) 
	 {
	   printf("can't get the total number of HDU !\n");
	   hdutot = 4;
	 }
      
       for (i=0;i<hdutot;i++)
	 {
	   if (ffmahd(fptr, i, &hdutype, &status)) status=0 ; 
	   else
	     {
	       if (ffgkys(fptr, "EXTNAME", keyname, comment, &status))
		 status = 0;
	       else
		 {
		   if ((strcmp(keyname,"SPI.-EBDS-OAR")) == 0)
		     {
		       if (ffgnrw(fptr, detchans, &status))
			 {
			   printf("Warning... CHANNEL number not found!\n ");
			   exit(1);;
			 }
                       else 
			 {
		   
		       if (ffmahd(fptr, hdunumold, &hdutype, &status)) status=0 ;     
                        return(status);
			 }
		     }
		 }
	     }
	 }

     if (ffmahd(fptr, hdunumold, &hdutype, &status)) status=0 ; 
     }
   return(status);
}

