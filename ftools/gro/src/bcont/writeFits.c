#include <string.h>
#include <stdio.h>
#include "bcont.h"
#include "general.h"
#include <stddef.h>
#include "fitsio2.h"

void  writeFits(long *snrows, double *sbinTime, double *sumCts, 
      double *sumCtsErr, char *data_dir, int *det, int *ndet, 
      float *timeRes, char *rcFlg, char **infname, int *nfile,
      char *filename, int *chansta, int *chanstp, int *binsz)
{
    fitsfile *outfptr, *infptr;       /* pointer to the FITS file*/
    int status = 0, hdutype, dtype =1, i=0,j=0;
    char telescop[] = "CGRO", instrume[]="BATSE"; 
    char history[100] =" ";
    int  tfields=3, timeref;
    char  extname[] = "SPECTRUM";
    char valstring[100],comm[100],datecomm[100]="";
    char msg[100]="",sdecnum[8]="", tmpstr[1]="";
    char datestr[30], date[30];
    char *ttype[3], *tunit[3], *tcomm[3];
    char *tform[] = { "1D",  "1D", "1D"};
    int nkeys, keypos,jj, hdunum;
    char  message[200], keyname[FLEN_KEYWORD],*infn;
    float  val;
   
    /* define the name, datatype, and physical units for the 3 columns */
    tunit[0] = "TJD";
    tcomm[0] = "Mid Time in days from JD2440000.5";
    ttype[0] = "MID_TIME";
    ttype[2] = "STAT_ERR";
    if (strncmp(rcFlg,"c",1) == 0 ||
             strncmp(rcFlg,"C",1) == 0)
      {    
       ttype[1] = "COUNTS";
       tunit[1] = "counts";
       tunit[2] = "counts";
       tcomm[1] = " ";  
       tcomm[2] = " ";
      }
     else
       {
        ttype[1] = "RATES";
        tunit[1] = "counts/sec";
        tunit[2] = "counts/sec";
        tcomm[1] = " ";  
        tcomm[2] = " ";
       }  
    
    for (i=0;i<100;i++) valstring[i]='\0'; 
    for (i=0;i<100;i++) comm[i]='\0'; 
    for (i=0;i<100;i++) datecomm[i]='\0';
    for (i=0;i<30;i++) date[i]='\0';
    
     ffgstm(datestr, &timeref, &status);
     strncpy(date,datestr,10);
     strcpy(datecomm,"FITs file creation date (yy-mm-dd)");    
     
    /* Delete old file if it already exists */
    remove(filename);  
   
    /* open the FITS file containing a primary array and an ASCII table */
    status = 0;
    if ( fits_create_file(&outfptr, filename, &status)) 
	{
          printf ("Problem creating FITs file!\n");
          exit (1);
	}
      
         if (fits_create_tbl(outfptr, BINARY_TBL, *snrows, tfields, ttype, tform,
          tunit,extname, &status))
         {
          printf ("Problem creating binary table!\n");
          exit (1);
	 }
	
     status = 0;
     ffpkyf(outfptr,"TJDREF", 2440000.5,1,"TJD reference", &status);
     if (status != 0)
     {
          printf ("Problem writing keyword TJDREF!");
          exit (1);
	}
  
    /* Modify the comments in the TTYPE fields */
      for (i=0; i<3; i++)
      {
            sprintf(keyname, "TTYPE%d", i+1);
            status = 0;
            if (fits_modify_comment(outfptr, keyname, tcomm[i], &status))
               Printerror(status);
      }
        
            status = 0;
            if (fits_modify_comment(outfptr,"TFORM1",
                "of interval when data accumulated", &status))
               Printerror(status);
      

      /* Read kyw from in file */
       if ( fits_open_file(&infptr,infname[0], READONLY, &status) ) 
         Printerror( status );
    status =0;
    message[0]='\0'; 
     
    /* get the hdunum */  
    ffextn (infname[0], &hdunum, &status);

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
      if (fits_write_key_dbl(outfptr, "TIME_RES", *timeRes, DECIMALS,
                             "time Resolution (in seconds)", &status))
         Printerror(status);
   
      status = 0;
      if (fits_write_key(outfptr, TSTRING,"TIME_SYS", "UTC",
                             "the system use to the define time", &status))
         Printerror(status);
       
      status = 0;
      if (fits_write_key_dbl(outfptr, "TSTART",sbinTime[0], DECIMALS,
                             "start time (in TJD)", &status))
         Printerror(status);

      status = 0;
      if (fits_write_key_dbl(outfptr, "TSTOP", sbinTime[*snrows-1], 
          DECIMALS,"stop time (in TJD)", &status))
         Printerror(status);

      status = 0;
      if (fits_write_key(outfptr, TINT,"BIN_SIZ", binsz,
                             "time bin size", &status))
         Printerror(status);
       
      status = 0;
      if (fits_write_key(outfptr, TINT,"CH_START", chansta,
                             "start channel", &status))
         Printerror(status);

      status = 0;
      if (fits_write_key(outfptr, TINT,"CH_STOP", chanstp,
                             "stop channel", &status))
         Printerror(status);
      
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
       if (fits_read_key(infptr, TSTRING, "END-DAY", valstring, 
                         comm, &status) == 0 )
	  {
            if (fits_write_key(outfptr, TSTRING, "END-DAY",valstring, 
                  comm,&status))
             Printerror(status); 
	  }
       status = 0;
       if (fits_read_key(infptr, TSTRING, "END-TIM", valstring,
                          comm, &status) == 0 )
	  {
            if (fits_write_key(outfptr, TSTRING, "END-TIM",valstring, 
                  comm,&status))
             Printerror(status); 
	  }
      
       status = 0;
       if (ffgky(infptr,TFLOAT,"SC-Z-RA" , &val, comm, &status) == 0 ) 
          {
           if (ffpkyf(outfptr,"SC-Z-RA", val,1,comm, &status))
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
         for (i=0;i<100;i++) valstring[i]='\0';
         for (i=0;i<*ndet;i++)
	   {
            sprintf(sdecnum,"%d",det[i]);
            strcat(valstring,sdecnum);
	   }
            if (fits_write_key(outfptr, TSTRING,"DSELECT",valstring, 
                  "detectors used in statistical",&status))
             Printerror(status); 
	

        status = 0;
        for (i=0;i<100;i++) valstring[i]='\0';
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
      
      /* Add the (passed) history cards, adding one related to this program */
        
         for (i=0;i<*nfile;i++) 
         strcat(strcat(history,infname[i])," ");
         strcpy(comm,"Created by bcont from input: ");
 
       status = 0;
       if (fits_write_key(outfptr, TSTRING, "COMMENT",comm,"", &status))
       Printerror(status);
 
      if (fits_write_key(outfptr, TSTRING, "HISTORY", history,
                            " The input FITs file", &status))
            Printerror(status);

      status = 0;
      if (fits_write_key(outfptr, TSTRING,"DATE", date,datecomm, &status))
           Printerror(status);

      /* Write the data */
         status = 0;
         fits_write_col(outfptr, TDOUBLE, 1, 1, 1, *snrows, sbinTime,
                   &status);
         if (status !=0)
	   {
            printf("Error writting mid-time!\n");
            exit(1);
	   }

          fits_write_col(outfptr, TDOUBLE, 2, 1, 1,*snrows, sumCts,
                   &status);
          if (status !=0)
	   {
            printf("Error writting counts/rates!\n");
            exit(1);
	   }
 
          fits_write_col(outfptr, TDOUBLE, 3, 1, 1,*snrows, sumCtsErr,
                   &status);
          if (status !=0)
	   {
            printf("Error writting stat-errors!\n");
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
    if (fits_write_key(outfptr, TSTRING, "HISTORY", history,
                            " The input FITs file", &status))
    Printerror(status);
    status = 0;
      if (fits_write_key(outfptr, TSTRING,"DATE", date,datecomm, &status))
           Printerror(status);
    status = 0;       
     if (ffclos(outfptr, &status) > 0)
      {
        printf("ERROR in ffclos = %d", status);
        exit(1);
 }
 
   return;
}
