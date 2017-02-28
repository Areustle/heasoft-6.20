# include <string.h>
# include <stdlib.h>
# include <stdio.h>
# include <ctype.h>
# include <time.h>
# include <stddef.h>


/*  Description : Task to create an empty Calibration Index File with the name
                  given by the filename parameter.

    Passed parameters : none

    Origin : original

    Authors/modification History:
      Lorraine Breedon (1.0.0:97 Nov 14) Original Version 
      Lorraine Breedon (1.1.0:98 Jun 23) expand widths of date 
                                         columns for y2k */


    
# include "fitsio.h"
# include "xpi.h"
# include "ftools.h"
# include "ftoolstruct.h"
# include "cfitsio.h"
# include "cfortran.h"
# include "pfile.h"

#define MAX_FNAME        160             /* max length file name */

#define BufLen_2 ((unsigned)(MAX_FNAME-1))    /* Cf. Uclgst macro */

int c_crtf()
{   
   fitsfile *fptr;   /* pointer to the new FITS file (cif) */
   int errstat=0, status=0;
   char subname[]="C_CRTCIF";
   char *sptr=subname;
   char version[]="1.1.0";
   char *vptr=version;
   char context[25];
   char taskname[40];
   char filename[160];
   int bitpix = -32;
   long naxis = 0;
   long naxes[2]={0,0};
   long nrows=0;
   long pcount = 0;
   int tfields=18;
   char extname[]="CIF";
   char *ttype[] = {"TELESCOP","INSTRUME","DETNAM","FILTER","CAL_DEV",
                    "CAL_DIR", "CAL_FILE", "CAL_CLAS", "CAL_DTYP", "CAL_CNAM",
                    "CAL_CBD", "CAL_XNO", "CAL_VSD", "CAL_VST", "REF_TIME",
                    "CAL_QUAL", "CAL_DATE", "CAL_DESC"};
   char *tform[] = {"10A",      "10A",     "20A",   "10A",   "20A", 
                    "70A",      "40A",     "3A",    "4A",    "20A",
                    "630A70",   "I",       "10A",    "8A",     "D",
                    "I",        "10A",     "70A"};
   char *tunit[18];
   int i; 
   char comment[] = "Version of CIF format";
   char keyname[] = "CIFVERSN";
   char value[]="1.1";
   char output[80];



   for (i=0; i<18; i++) {
       tunit[i]=" ";
   }

/* Give user info */

   strcpy(output,""); 
   strcat(output,"** Using ");
   strcat(output,sptr);
   strcat(output," version ");
   strcat(output,vptr);

   printf(" \n");
   
   printf(output);
   printf(" \n");
   printf(" \n");




   /*cfortran call to initialize the TASK common block used by Fcerr*/
    C2FCBSTR("c_crtcif",TASK.taskname,0);


    filename[0]='\0';
 
   /*Open the par file and read any command line arguments
    OpenDefaultPF(argc, argv); */
        Uclgst("filename",filename,&errstat);
        if(errstat) {
            Fcerr("Error getting filename parameter");
            return(errstat);
        }
      
         
    /* CloseDefaultPF(); */

 

/* create new FITS file */
   fits_create_file(&fptr,filename, &status); 
   if (status) {
            Fcerr("Problem creating NEW caldb.indx file...already exists?");
            return(status);
   }

 
   
/* Write the mandatory Primary array keywords */
   fits_create_img(fptr,bitpix,naxis,naxes,&status);
   if (status) {
            Fcerr("Problem writing mandatory keywords");
            return(status);
   }

          


/* Append the date keyword into the CHU. The keyword value will contain 
   the current system date */

   fits_write_date(fptr, &status);
   if (status) {
            Fcerr("Problem appending date keyword");
            return(status);
   }


/* Create the cif extension */

   fits_create_tbl(fptr,BINARY_TBL,nrows,tfields,ttype,tform,tunit,
           extname,&status);
   if (status) {
            Fcerr("Problem creating CIF extension");
            return(status);
   }


/* Write the CIF version number */
   fits_write_key_str(fptr,keyname,value,comment,&status);
   if (status) {
            Fcerr("Problem writing CIF version number");
            return(status);
   }


/* Close the file */

   fits_close_file(fptr,&status);
   if (status) {
            Fcerr("Problem writing binary table keywords");
            return(status);
   }

   strcpy(output,"");
   strcat(output,"** "); 
   strcat(output,sptr);
   strcat(output," version ");
   strcat(output,vptr);
   strcat(output," finished ");
    
   printf(output);
   printf(" \n");




}


