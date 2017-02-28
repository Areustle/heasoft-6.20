#include <string.h>
#include <stdio.h>
#include "bpulsarspec.h"
#include "general.h"
#include <stddef.h>
#include "fitsio2.h"

void  writeRates(char *infname, char *bpulsfn, char *filename, 
      long *snrows, double *sumRates, double *ratesErr,
      int *decnum, double *texpos)
{

    fitsfile *outfptr, *infptr;       /* pointer to the FITS file*/
    int status = 0, hdutype, dtype =1, i=0,j=0;
    char telescop[] = "CGRO", instrume[]="BATSE"; 
    char phaversn[] = "1.0.0", hduclas2[] = "TOTAL";
    int  fchan=0,  nrespfil=0,ii; 
    int  nacrfil=0, ntemplfil=0;
    int nbackfil=0, ncorrfil=0,nancrfil=0;
    float areascal = 1.0, backscal=1.0, corrscal=0.0, val;
    char *backfil[10], corrfil[20]="NONE", respfil[20]="NONE";
    char  ancrfil[20]="NONE",templfil[20]="NONE";
    char filter[10]="NONE";
    long detchans=16,nchan=16;
    char chantyp[] = "PHA", history[100] =" ";
    short quality[16],grping[16];
    float counts[16];
    int  qerror=0,qsys=0,qqual=0,qgroup=0,*chan=NULL;
   long  *conv4 = (long *) malloc(16*sizeof(long));
   long  lkeyval;
   long  tlmin=fchan;
   long  tlmax=fchan+detchans-1;
   int   tfields=3, ikeyval,innum,outnum,morekeys;
   short *conv2 = (short *) malloc(nchan*sizeof(short));
   char  hduvers[6],message[200], keyname[FLEN_KEYWORD];
   char  *sdecnum= (char *) malloc(20*sizeof(char));
   char  extname[] = "SPECTRUM";
   char valstring[100],comm[100],datecomm[100],ckeyval[FLEN_KEYWORD];
   char msg[100]="";

   /* define the name, datatype, and physical units for the 3 columns */
    char *ttype[] = { "CHANNEL", "RATES",    "STAT_ERR"      };
    char *tform[] = {  "I",        "1D",           "1E"        };
    char *tunit[] = {   "",       "counts/sec",       "counts/sec"    };
    char *tcomm[] = {   "Pulse Height Analyser (PHA) Channel ",       
                        "Counts per channel",   
                        "Counts Error per channel"    };
    
     int nkeys, keypos,jj, hdunum, timeref;
     char datestr[30], date[30];
     
    chan = (int *) malloc(16 * sizeof(int));
    for (i=0;i<16;i++) chan[i]=i+1;
    for (i=0;i<100;i++) valstring[i]='\0'; 
    for (i=0;i<100;i++) comm[i]='\0'; 
    for (i=0;i<100;i++) datecomm[i]='\0';
    for (i=0;i<30;i++) date[i]='\0';

     ffgstm(datestr, &timeref, &status);
     strncpy(date,datestr,10);
     strcat(datecomm,"FITs file creation date (yy-mm-dd)");

    /* Delete old file if it already exists */
       remove(filename);  
     
    /* open the FITS file containing a primary array and an ASCII table */
    status = 0;
    if ( fits_create_file(&outfptr, filename, &status)) 
	{
          printf ("Problem creating FITs file!");
          exit (1);
	}

         if (fits_create_tbl(outfptr, BINARY_TBL, nchan, tfields, ttype, tform,
          tunit,extname, &status))
         {
          printf ("Problem creating binary table!");
          exit (1);
	 }

    /* Modify the comments in the TTYPE fields */
      for (i=0; i<tfields; i++)
      {
         if (strcmp(tcomm[i], " ") != 0)
         {
            sprintf(keyname, "TTYPE%d", i+1);
            if (fits_modify_comment(outfptr, keyname, tcomm[i], &status))
               Printerror(status);
         }
      }

  /* Write the HDUCLASn and HDUVERS keywords */

      if (fits_write_key(outfptr, TSTRING, "HDUCLASS", "OGIP",
                         "Format confirms to OGIP standard", &status))
         Printerror(status);
      
      if (fits_write_key(outfptr, TSTRING, "HDUCLAS1", "SPECTRUM",
                         "PHA dataset (OGIP memo OGIP-92-007)", &status))
         Printerror(status);

         strcpy(hduvers, "1.1.0");
         if (fits_write_key(outfptr, TSTRING, "HDUVERS1", hduvers,
                            "Obsolete - included for backwards compatibility",
                            &status))
         Printerror(status);

         if (fits_write_key(outfptr, TSTRING, "HDUVERS", hduvers,
                            "Version of format (OGIP memo OGIP-92-007a)",
                            &status))
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

      if (fits_write_key(outfptr, TSTRING, "HDUCLAS2", hduclas2, message, &status))
  Printerror(status);

      /* Read kyw from in file */
       if ( fits_open_file(&infptr, infname, READONLY, &status) ) 
         Printerror( status );
    status =0;
    message[0]='\0'; 
    strcat(infname,"[BATSE_PSR_FOB_16]");
     
    /* get the hdunum */  
    ffextn (infname, &hdunum, &status);

    /* move to 1st HDU in infile*/
       
     if ( ffmahd(infptr, 1, &hdutype, &status) ) 
     Printerror( status );
     status = 0;
     if(fits_read_key(infptr, TSTRING, "TELESCOP", valstring,
                      comm, &status) == 0 )
	 {     
          if (fits_write_key(outfptr, TSTRING, "TELESCOP",valstring, comm,
                          &status))
          Printerror(status); 
	 }

         status = 0;
        if (fits_read_key(infptr, TSTRING,"OBJECT", valstring, 
                          comm, &status) == 0 )
          {
           if (fits_write_key(outfptr, TSTRING, "OBJECT",valstring, 
                   comm, &status))
           Printerror(status); 
	  }

         status = 0;
      if (fits_write_key(outfptr, TSTRING, "FILTER", filter,
                         "filter in use", &status))
         Printerror(status);

      status = 0;
      if (fits_write_key_dbl(outfptr, "EXPOSURE", *texpos, DECIMALS,
                             "exposure (in seconds)", &status))
         Printerror(status);
    
        status = 0;
        if (ffgky(infptr,TFLOAT,"OBS-TIME", &val, comm, &status) == 0 )
          {
           if (fits_write_key(outfptr, TFLOAT, "OBS-TIME", &val,
               comm, &status))
           Printerror(status); 
	  }

      status = 0;     
      if (ffgky(infptr, TFLOAT,"STRT-DAY", &val, comm, &status) == 0 )
          {
           if (fits_write_key(outfptr, TFLOAT, "STRT-DAY", &val,
               comm, &status))
           Printerror(status); 
	  }
      status = 0; 
      if (ffgky(infptr, TFLOAT,"STRT-TIM", &val, comm, &status) == 0 )
          {
           if (fits_write_key(outfptr,TFLOAT, "STRT-TIM", &val,
               comm, &status))
           Printerror(status); 
	  }

       status = 0;
       if (fits_read_key(infptr, TSTRING, "DATE-OBS", valstring,
           comm, &status) == 0 )
          {
           if(fits_write_key(outfptr, TSTRING, "DATE-OBS", valstring,
                             comm, &status))
           Printerror(status); 
	  }

       status = 0;
       if (fits_read_key(infptr, TSTRING, "TIME-OBS", valstring, 
                         comm, &status) == 0 )
	  {
            if (fits_write_key(outfptr, TSTRING, "TIME-OBS",valstring, 
                  comm,&status))
             Printerror(status); 
	  }
  
       status = 0;
       if (fits_read_key(infptr, TSTRING, "DATE-END", valstring, 
                         comm, &status) == 0 )
	  {
            if (fits_write_key(outfptr, TSTRING, "DATE-END",valstring, 
                  comm,&status))
             Printerror(status); 
	  }
       status = 0;
       if (fits_read_key(infptr, TSTRING, "TIME-END", valstring,
                          comm, &status) == 0 )
	  {
            if (fits_write_key(outfptr, TSTRING, "TIME-END",valstring, 
                  comm,&status))
             Printerror(status); 
	  }

       status = 0;
       if (ffgky(infptr,TFLOAT,"C-Z-RA" , &val, comm, &status) == 0 ) 
          {
           if (ffpkyf(outfptr,"C-Z-RA", val,1,comm, &status))
           Printerror(status); 
	  }  

       status = 0;
       if (ffgky(infptr,TFLOAT,"SC-Z-DEC" , &val, comm, &status) == 0 ) 
          {
           if (ffpkyf(outfptr,"SC-Z-DEC", val,1,comm, &status))
           Printerror(status); 
	  }  
      
       status = 0;
       if (ffgky(infptr,TFLOAT,"SC-X-RA",&val, comm, &status) == 0 ) 
          {
           if (ffpkyf(outfptr,"SC-X-RA" , val,1,comm, &status))
           Printerror(status); 
	  }  

        status = 0;
        if (ffgky(infptr, TFLOAT,"SC-X-DEC" , &val, comm, &status) == 0 ) 
          {  
           if (ffpkyf(outfptr,"SC-X-DEC" , val,1,comm, &status))
           Printerror(status); 
	  } 
 
        status = 0;
        if (ffgky(infptr,TFLOAT,"SC-Z-BII", &val, comm, &status) == 0 ) 
          {
           if (ffpkyf(outfptr,"SC-Z-BII" , val,1,comm, &status))
           Printerror(status); 
	  }  
        
        status = 0;
        if (ffgky(infptr,TFLOAT,"SC-Z-LII" , &val, comm, &status) == 0 ) 
          {
           if (ffpkyf(outfptr,"SC-Z-LII" , val,1,comm, &status))
           Printerror(status); 
	  } 
 
        status = 0;
        if (ffgky(infptr,TFLOAT,"SC-X-BII",&val, comm, &status) == 0 )
	  {         
           if (ffpkyf(outfptr,"SC-X-BII" , val,1,comm, &status))
           Printerror(status); 
	  }  

        status = 0;
        if (fits_read_key(infptr, TFLOAT, "SC-X-LII" , &val, comm, &status) == 0 ) 
          {
           if (fits_write_key(outfptr, TFLOAT, "SC-X-LII" , &val, comm, 
               &status))
           Printerror(status); 
	  }  

          status = 0;
          if (fits_read_key(infptr, TSTRING, "DSELECT", valstring,
            comm, &status) == 0 )
	  {
            for (i=0; i<8;i++) 
              if (strncmp(valstring+i,"Y",1) ==0 ) *decnum = 7-i;
	  }

         sprintf(sdecnum,"%d",*decnum);
         status = 0;
          if (fits_read_key(infptr, TSTRING, "DET-MODE", valstring, 
             comm, &status) == 0 )
	  {
            strcat(valstring,sdecnum);
            if (fits_write_key(outfptr, TSTRING,"DET-MODE",valstring, 
                  comm,&status))
             Printerror(status); 
	  }

        status = 0;
        if (fits_read_key(infptr, TSTRING, "INSTRUME", valstring, 
            comm, &status) == 0 )
	  {    
            if (fits_write_key(outfptr, TSTRING, "INSTRUME",valstring, 
                  comm,&status))
             Printerror(status); 
	  }
        status = 0;      
       if ( fits_close_file(infptr, &status) )
         Printerror( status );
      
       if (fits_write_key_fixflt(outfptr, "AREASCAL", areascal, DECIMALS,
                                "Area scaling factor", &status))
       Printerror(status);
       if (nbackfil <= 1)
         {
          if (fits_write_key(outfptr, TSTRING, "BACKFILE", "NONE",
                            "Associated background filename", &status))
            Printerror(status);

         if (fits_write_key_fixflt(outfptr, "BACKSCAL",backscal, DECIMALS,
                                   "Background file scaling factor", &status))
            Printerror(status);
      }

         if (fits_write_key(outfptr, TSTRING, "CORRFILE", "NONE",
                            "Associated correction filename", &status))
            Printerror(status);

         if (fits_write_key_fixflt(outfptr, "CORRSCAL", corrscal, DECIMALS,
                                   "Correction file scaling factor", &status))
          Printerror(status);
          
         if (fits_write_key(outfptr, TSTRING, "RESPFILE", "NONE",
                            "Associated redistrib matrix filename", &status))
            Printerror(status);

         if (fits_write_key(outfptr, TSTRING, "ANCRFILE", "NONE",
                            "Associated ancillary response filename", &status))
            Printerror(status);

      if (fits_write_key(outfptr, TSTRING, "PHAVERSN", "1992a",
                         "OGIP classification of FITS format", &status))
         Printerror(status);

      if (fits_write_key_lng(outfptr, "DETCHANS", detchans,
                             "Total number of detector channels", &status))
         Printerror(status);

      if (fits_write_key(outfptr, TSTRING, "CHANTYPE", chantyp,
                         "channel type (PHA, PI etc)", &status))
         Printerror(status);
 
      if ((dtype == 1) && (qerror == 0))
      {
         ikeyval = 0;
         if (fits_write_key_log(outfptr, "POISSERR", ikeyval,
                                "Poissonian errors to be assumed", &status))
           Printerror(status);
      }

     
     if (qsys == 0)
      {
         lkeyval = 0L;
         if (fits_write_key_lng(outfptr, "SYS_ERR", lkeyval,
                                "no systematic error specified", &status))
           Printerror(status);
      }

      if (qgroup == 0)
      {
         lkeyval = 0L;
         if (fits_write_key_lng(outfptr, "GROUPING", lkeyval,
                                "no grouping of the data is defined", &status))
           Printerror(status);
      }

      if (qqual == 0)
      {
         lkeyval = 0L;
         if (fits_write_key_lng(outfptr, "QUALITY", lkeyval,
                                "no data quality information specified",
                                &status))
           Printerror(status);
      }

      /* Add the (passed) history cards, adding one related to this program */
         
         strcat(history,bpulsfn);
         strcat(strcpy(comm,"Created by bpulsarspec from input "),bpulsfn);
         if (fits_write_key(outfptr, TSTRING, "HISTORY", history,
                            " The input FITs file", &status))
            Printerror(status);

       status = 0;
       if (fits_write_key(outfptr, TSTRING, "COMMENT", comm,"", &status))
       Printerror(status); 

      status = 0;
      if (fits_write_key(outfptr, TSTRING,"DATE", date,datecomm, &status))
           Printerror(status);

      /* Write the data */
         status = 0;
         fits_write_col(outfptr, TINT, 1, 1, 1, 16, chan,
                   &status);
         if (status !=0)
	   {
            printf("Error writting channel number!\n");
            exit(1);
	   }

          fits_write_col(outfptr, TDOUBLE, 2, 1, 1, 16, sumRates,
                   &status);
          if (status !=0)
	   {
            printf("Error writting channel rates!\n");
            exit(1);
	   }
 
          fits_write_col(outfptr, TDOUBLE, 3, 1, 1, 16, ratesErr,
                   &status);
          if (status !=0)
	   {
            printf("Error writting channel rates errors!\n");
            exit(1);
	   }
     
      status = 0;      
    if ( fits_movabs_hdu(outfptr,1, &hdutype, &status) )
         Printerror( status );

     status = 0;   
    /* write date, time keyword */
    if (fits_write_key(outfptr, TSTRING, "COMMENT", comm,"", &status))
    Printerror(status); 

       status = 0;      
     if (ffclos(outfptr, &status) > 0)
      {
        printf("ERROR in ffclos = %d", status);
        exit(1);
      }
   free(conv2);
   free(conv4);
   
   return;
}
