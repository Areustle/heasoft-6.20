/******************************************************************************
* File name:   gtisynch.c
* 
* Task name:   gtisynch
* 
* Description: Modifies/Adjusts GTI START and END time stamps depending upon 
*              the values of the time frames in the input event file(s).   
* 
* Author/Date: Alex Muslimov / May, 1999
*              NASA/GSFC
*              Raytheon ITSS
* 
* Modification History:
* 
* Usage:       
* 
* Arguments:   From .par file
* 
* Functions implemented in this file:
*        void  gtisynch() :  Entry point of program
* 
* Library/CFITSIO routines called:
*
*        status=PILGetString(char* param name, char * param var, int* status);
*        Uclgsd(char* param name, double * param var, int* status);
*        c_fcerr(char* error_message);
*        fits_open_file( fitsfile *fptr, char *filename, 
*                          int iomode, > int *status ) ;
*        fits_create_file( fitsfile **fptr, char *filename, > int *status ) ;
*        fits_parse_extnum( fitsfile *fptr, int *extnum, int *status ) ;
*        fits_create_tbl( fitsfile *fptr, int tbltype, long naxis2, 
*                         int tfields, char *ttype[], char *tform[],
*                         char *tunit[], char *extname, > int *status ) ;
*        fits_get_colnum( fitsfile *fptr, 0, char *templt, 
*                         > int *colnum, int *status ) ;
*        fits_get_coltype( fitsfile *fptr, int colnum, > int *typecode, 
*                          long *repeat, long *width, int *status ) ;
*
*        fits_write_col_dbl( fitsfile *fptr, int colnum, long firstrow, 
*                            long firstelem, long nelements, 
*                            DTYPE *array, > int *status ) ;
*        fits_read_col( fitsfile *fptr, int datatype, int colnum, 
*                       long firstrow, long firstelem, long nelements, 
*                       DTYPE *nulval, DTYPE *array, int *anynul, 
*                       > int *status ) ;
*        ffclos( fitsfile *fptr, > int *status ) ;
*        fits_clear_errmsg( void );
*        c_fcerr( char * error_message ) ;
*        fits_movabs_hdu( fitsfile *fptr, int hdunum, > int * hdutype, 
*                         int *status );        
*****************************************************************************/

#include <stdio.h>      /* Note: for IRAF compatibility,
                        standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#define TOOLSUB gtisynch
#include "headas_main.c"

#if (defined __APPLE__) || (defined __CYGWIN__)
#include <float.h>
#include <limits.h>
#else
#include <values.h>
#endif

#define BUFSIZE  80
#define CBUFSIZE 81

typedef struct GTITIME { 
   double tstart;
   double tstop;
} gtitime; 


/******************************************************************************
* Function:
*       GetParam
* 
* Description:
*       Read parameter file
*  
* Author: 
*       Alex Muslimov / May 1999
*       NASA/GSFC
*       Raytheon ITSS
*  
* Modification history:
* 
* Usage:
*      int GetParam ( char *infile, 
*                     char *gtifile, 
*                     char *outfile,
*                     double *expofrac,
*                     char *begin,
*                     char *end,
*                     char *frames_ext,   
*                     char *start,
*                     char *stop,
*                     char *gti_ext,
*                     int *clobber );
* 
* Primary Variables:
*    --- Inputs ---
*      infile    :   Filename (including extension) of input file
*      gtifile   :   Filename of the GTI file
*      outfile   :   Filename (including extension) of output file
*      expofrac  :   Fraction of exposure to be included in a GTI
*      begin     :   Name of column containing frames begin time
*      end       :   Name of column containing frames end time
*      frames_ext:   Extension name containing frames
*      start     :   Name of column containing GTI start times 
*      stop      :   Name of column containing GTI end times 
*      gti_ext   :   Extension name containing GTIs
*      clobber   :   Optional overwriting of the existing file
*      status    :   FITSIO error code
* 
******************************************************************************/
int GetParam ( char *infile, char *gtifile, char *outfile, 
               double *expofrac, char *begin, char *end, 
               char *frames_ext, char *start, char *stop, 
               char *gti_ext, int *clobber )
{

   int status = 0;

   status=PILGetFname("infile", infile);
   if(status != 0) {
      fprintf(stderr,"\nParameter 'infile' not found in .par file.\n");
      return status;
   }

   status=PILGetFname("gtifile", gtifile);
   if(status != 0) {
      fprintf(stderr,"Parameter 'gtifile' not found in .par file.");
      return status;
   }

   status=PILGetFname("outfile", outfile);
   if(status != 0) {
      fprintf(stderr,"Parameter 'outfile' not found in .par file.");
      return status;
   }

   status=PILGetReal("expofrac", expofrac);
   if(status != 0) {
      fprintf(stderr,"Parameter 'expofrac' not found in .par file.");
      return status;
   }

   status=PILGetString("begin", begin);
   if(status != 0) {
      fprintf(stderr,"Parameter 'begin' not found in .par file.");
      return status;
   }

   status=PILGetString("end", end);
   if(status != 0) {
      fprintf(stderr,"Parameter 'end' not found in .par file.");
      return status;
   }

   status=PILGetString("frames_ext", frames_ext);
   if(status != 0) {
      fprintf(stderr,"Parameter 'frames_ext' not found in .par file.");
      return status;
   }

   status=PILGetString("start", start);
   if(status != 0) {
      fprintf(stderr,"Parameter 'start' not found in .par file.");
      return status;
   }

   status=PILGetString("stop", stop);
   if(status != 0) {
      fprintf(stderr,"Parameter 'stop' not found in .par file.");
      return status;
   }

   status=PILGetString("gti_ext", gti_ext);
   if(status != 0) {
      fprintf(stderr,"Parameter 'gti_ext' not found in .par file.");
      return status;
   }

   status=PILGetBool("clobber", clobber);
   if(status != 0) {
      fprintf(stderr,"Parameter 'clobber' not found in .par file.");
      return status;
   }

#ifdef TEST
printf("infile=%s, gtifile=%s, outfile=%s \n", infile, gtifile, outfile ) ;
printf("expofrac = %e \n", *expofrac ) ;
printf("begin=%s, end=%s, frames_ext=%s \n", begin,end,frames_ext) ;
printf("start=%s, stop=%s, gti_ext=%s \n", begin,end,gti_ext) ;
printf("clobber=%d \n", *clobber ) ;
#endif

   return status;
}

/******************************************************************************
* Function:
*      GtiSynchron
* 
* Description:
*
* 
* Author: 
*      Alex Muslimov / May 1999
*      NASA/GSFC
*      Raytheon ITSS
* 
* Modification history:
*
* 
* Usage:
*      void GtiSynchron ( char *infile, char *gtifile, char *outfile,
*                         double  expofrac, char *begin, char *end, 
*                         char *frames_ext, char *start, char *stop, 
*                         char *gti_ext, int clobber, int *status ) ;
* 
* Primary Variables:
*    --- Inputs ---
*      infile    :   Filename (including extension) of input file
*      gtifile   :   Filename of the GTI file
*      outfile   :   Filename (including extension) of output file
*      expofrac  :   Fraction of exposure to be included in a GTI
*      begin     :   Name of column containing frames begin time
*      end       :   Name of column containing frames end time
*      frames_ext:   Extension name containing frames
*      start     :   Name of column containing GTI start times 
*      stop      :   Name of column containing GTI end times 
*      gti_ext   :   Extension name containing GTIs
*      clobber   :   Optional overwriting of the existing file
*      status    :   FITSIO error code
*    --- Internal ---
*      in,gti,out:   Input and Output FITS files
*      inExt     :   FITS HDU extension to read from
*      outExt    :   FITS HDU extension to write to
*      hdutypeIn :   HDU type of input extension/primary array (0=Image)
*       
******************************************************************************/
void GtiSynchron ( char *infile, char *gtifile, char *outfile, 
                   double expofrac, char *begin, char *end, 
                   char *frames_ext, char *start, char *stop, 
                   char *gti_ext, int clobber, int *status )

{

   fitsfile *in, *gti, *out;
   char context[CBUFSIZE], ConText[CBUFSIZE] ;
   char KeyWord[CBUFSIZE] ;
   
   int i, j ;
   unsigned long nrows, M, N ;
   int inExt, outExt, hdutypeIn, hdutypeOut ;
   int SameGtiFile, anynul ;

   int begin_col, end_col, start_col, stop_col ;
   int ColNumStart, ColNumStop, NumCol ;

   double *StartTime, *StopTime, *BeginTime, *EndTime ;
   double nulval ;

   int BufLen_1  = BUFSIZE; 
   int BufLen_2  = BUFSIZE; 
   unsigned long NumTimes ;

   int len;
   char *p;


   /* Function prototype */
   void AdjustTimes ( unsigned long M, unsigned long N, double expofrac, 
                 double *StartTime, double *StopTime,double *BeginTime, 
                 double *EndTime, unsigned long *NumTimes, int *status ) ;

   /* take out the trailing space in infile */
   len = strlen(infile);
   p = infile + (len -1);
   for (i = len - 1; i >= 0 && isspace(*p); i--) {*p = '\0'; p--;}

   NumCol     = 2 ; /* number of columns to process */

   inExt      = 0 ;
   outExt     = 0 ;
   hdutypeIn  = 0 ;
   hdutypeOut = 0 ;
   KeyWord[0] = 0 ;
   context[0] = 0 ;
   ConText[0] = 0 ;

   begin_col  = 0 ;
   end_col    = 0 ;
   start_col  = 0 ;
   stop_col   = 0 ;
   nrows      = 0 ;

   nulval     = DOUBLENULLVALUE ;
   anynul     = 0 ;
   *status    = 0 ;

   /*********************************************************/
   /*   Open input files and move to the appropriate HDUs   */
   /*********************************************************/

   if( fits_open_file( &in, infile, READONLY, status) ) {
      fprintf(stderr,"...Error opening FITS file ." );
      goto Error;
   }

   if( fits_open_file( &gti, gtifile, READWRITE, status) ) {
      fprintf(stderr,"...Error opening GTI file ." );
      goto Error;
   }


   /*********************************************************************/
   /*  Read in HDU parameters and determine input data type and format  */
   /*********************************************************************/

   fits_get_hdu_num( in, &inExt );
   if( fits_get_hdu_type( in, &hdutypeIn, status ) ) {
      fprintf(stderr,"...Couldn't obtain hdu type in input file." );
      goto Error;
   }

   /* type of HDU: IMAGE_HDU (=0), ASCII_TBL (=1), BINARY_TBL (=2) */

#ifdef TEST
printf("GtiSynch: HDU TYPE=%d \n", hdutypeIn ) ;
printf("GtiSynch: Column name =%s \n", begin ) ;
printf("GtiSynch: Column name =%s \n", end ) ;
#endif

      fits_read_key( in, TSTRING, "EXTNAME", KeyWord, 0, status ) ;
      if( *status ) {
         fprintf(stderr,"...Error reading EXTNAME keyword.");
         goto Error;
      }

      strcpy(context,"FRAMES") ;

   if( (hdutypeIn == BINARY_TBL) || (!strcmp(KeyWord,context)) ) {

       fits_get_colnum( in, 0, begin, &begin_col, status );
       fits_get_colnum( in, 0, end, &end_col, status ) ;
       fits_read_key( in, TLONG, "NAXIS2", &nrows, 0, status );
       M = nrows ;    
     
       if( *status ) {
         fprintf(stderr,"...Error reading information for column.");
         goto Error;
       }

   }

   else {
         fprintf(stderr,"...Unsupported extension type.");
         goto Error;
        }

#ifdef TEST
printf("GtiSynchExtension =%s, number of rows =%d \n", KeyWord, M ) ;
#endif


   /* Allocate memory for 1-d data arrays */

   /* M - number of elements in BeginTime/EndTime arrays */

      BeginTime  = ( double *) malloc( M * sizeof( double ) ) ; 
      EndTime    = ( double *) malloc( M * sizeof( double ) ) ; 

      fits_read_col_dbl( in, begin_col, 1, 1, M, nulval, BeginTime, 
                           &anynul, status) ;
       if( *status ) {
         fprintf(stderr,"...Error reading BeginTime column.");
         goto Error;
       }

      fits_read_col_dbl( in, end_col, 1, 1, M, nulval, EndTime, 
                           &anynul, status) ;
       if( *status ) {
         fprintf(stderr,"...Error reading EndTime column.");
         goto Error;
       }

#ifdef TEST
 for ( i = 0; i < M; i++ ) {
 printf("GtiSynch: Row = %d BeginTime = %e EndTime = %e \n", 
                   i, BeginTime[i], EndTime[i] ) ;
 }
#endif

   /* Get GTI HDU  */

   fits_get_hdu_num( gti, &inExt );
   if( fits_get_hdu_type( gti, &hdutypeIn, status ) ) {
      fprintf(stderr,"...Couldn't obtain hdu type in GTI file." );
      goto Error;
   }
      fits_read_key( gti, TSTRING, "EXTNAME", KeyWord, 0, status ) ;
      if( *status ) {
         fprintf(stderr,"...Error reading EXTNAME keyword.");
         goto Error;
      }

   if( (hdutypeIn == BINARY_TBL) || (!strcmp(KeyWord,gti_ext)) ) {

       fits_get_colnum( gti, 0, start, &start_col, status );
       fits_get_colnum( gti, 0, stop, &stop_col, status ) ;
       fits_read_key( gti, TLONG, "NAXIS2", &nrows, 0, status );
       N = nrows ;
       ColNumStart = start_col ;
       ColNumStop  = stop_col ;    
     
       if( *status ) {
         fprintf(stderr,"...Error reading information for column.");
         goto Error;
       }

   }

   else {
         fprintf(stderr,"...Unsupported extension type.");
         goto Error;
   }

#ifdef TEST
printf("GtiSynch: Column number for start times = %d \n", start_col ) ;
printf("GtiSynch: Column number for stop times =%d \n", stop_col ) ;
printf("GtiSynchExtension =%s, number of rows =%d \n", KeyWord, N ) ;
#endif
 
   /* Allocate memory for 1-d data arrays */
   /* N - number of elements in StartTime/StopTime arrays */

   StartTime  = ( double *) malloc( N * sizeof( double ) ) ; 
   StopTime   = ( double *) malloc( N * sizeof( double ) ) ; 

      fits_read_col_dbl( gti, start_col, 1, 1, N, nulval, StartTime, 
                           &anynul, status) ;
       if( *status ) {
         fprintf(stderr,"...Error reading StartTime column.");
         goto Error;
       }

      fits_read_col_dbl( gti, stop_col, 1, 1, N, nulval, StopTime, 
                           &anynul, status) ;
       if( *status ) {
         fprintf(stderr,"...Error reading StopTime column.");
         goto Error;
       }

#ifdef TEST
 for ( i = 0; i < N; i++ ) {
 printf("GtiSynch: Row = %d StartTime = %e StopTime = %e \n", 
                   i, StartTime[i], StopTime[i] ) ;
 }
#endif

  NumTimes = 0 ;

  AdjustTimes( M, N, expofrac, StartTime, StopTime, BeginTime,
               EndTime, &NumTimes, status ) ;

#ifdef TEST
 for ( i = 0; i < NumTimes; i++ ) {
 printf("GtiSynch: Row = %d StartTime = %e StopTime = %e \n", 
                   i, StartTime[i], StopTime[i] ) ;
 }
#endif


   /* Find out if the output should be written to the same events file */

   SameGtiFile = 0 ;

   fits_parse_rootname(outfile,ConText,status );
   if(*status != 0) {
      fprintf(stderr,"... Error parsing the output file rootname.");
      goto Error;
   }
   context[0] = 0 ;
   fits_parse_rootname(gtifile,context,status );
   if(*status != 0) {
      fprintf(stderr,"... Error parsing the GTIs file rootname.");
      goto Error ;
   }
   if ( !strcmp(context,ConText) ) SameGtiFile = 1 ;

#ifdef TEST
 printf("GtiSynch: Same File ?  %d \n", SameGtiFile) ;
#endif


  if( !SameGtiFile ) {
   /* open the output file */
     fits_open_file( &out, outfile, READWRITE, status ) ;
     if(*status == FILE_NOT_OPENED ) {
       *status = 0 ;
       fits_clear_errmsg() ;
       fits_create_file( &out, outfile, status ) ;
     } else if( *status == 0 ) {
            if ( clobber ) {
              fits_delete_file( out, status ) ;
              fits_create_file( &out, outfile, status ) ;
            }
     } else {
	 fprintf(stderr,"Unable to open/create output file.");
	 goto Error;
     }


   /* Copy input GTI file HDUs to the output file */


     for (i = 1; fits_movabs_hdu(gti,i,NULL,status) == 0; i++) {
         if( fits_copy_hdu(gti,out,0,status) != 0 )
         break ;
     }
     if (*status == END_OF_FILE)
        *status = 0 ;
     else {
         fits_get_errstatus(*status,context) ;
         do fprintf(stderr,context) ; 
         while (fits_read_errmsg(context) != 0);
     }

    /* Modify the appropriate keywords in the output file */

     ffmkyj(out,"NAXIS2",NumTimes,0,status) ;
       if(*status != 0) {
         fprintf(stderr,"... Problem with modifying NAXIS2 keyword.");
         goto Error ;
       }
        

   /*********************************************************/
   /*             Write the output to the outfile           */
   /*********************************************************/

#ifdef TEST
 printf("GtiSynch: status= %d outfile = %s outextension = %d \n", *status, outfile, outExt ) ;
#endif

     ffpcl(out,TDOUBLE,start_col,1,1,NumTimes,StartTime,status) ;
        if(*status != 0) {
         fprintf(stderr,"... Problem writing StartTime column to Table.");
         goto Error ;
        }
     ffpcl(out,TDOUBLE,stop_col,1,1,NumTimes,StopTime,status) ;
        if(*status != 0) {
         fprintf(stderr,"... Problem writing StopTime column to Table.");
         goto Error ;
        }

     ffclos( out, status) ;
        if(*status != 0) {
         fprintf(stderr,"... Problem with closing the outfile.");
         goto Error ;
        }     

  }

   /**********************************************************/
   /*  Write the output to the same GTI file and extension   */
   /**********************************************************/


  if( SameGtiFile ) {

    /* Modify the appropriate keywords in the input GTI file */

     fits_movabs_hdu(gti,inExt,NULL,status) ; 
     ffmkyj(gti,"NAXIS2",NumTimes,0,status) ;
       if(*status != 0) {
         fprintf(stderr,"... Problem with modifying NAXIS2 keyword.");
         goto Error ;
       }


     ffpcl(gti,TDOUBLE,start_col,1,1,NumTimes,StartTime,status) ;
        if(*status != 0) {
         fprintf(stderr,"... Problem writing StartTime column to Table.");
         goto Error ;
        }

     ffpcl(gti,TDOUBLE,stop_col,1,1,NumTimes,StopTime,status) ;
        if(*status != 0) {
         fprintf(stderr,"... Problem writing StopTime column to Table.");
         goto Error ;
        }

     ffclos( gti, status) ;
        if(*status != 0) {
         fprintf(stderr,"... Problem with closing the GTI file.");
         goto Error ;
        }     

  }


     ffclos( in, status );
        if(*status != 0) {
         fprintf(stderr,"... Problem with closing the input event file.");
         goto Error ;
        }     

Error:
     if( *status != 0 ) {
       fprintf(stderr,context);   
     }

   /* Delete allocated memory */

   free ( BeginTime ) ;
   free ( EndTime ) ;  
   free ( StartTime ) ;
   free ( StopTime ) ;

   return;
}

/******************************************************************************
* Function:
*       AdjustTimes
* 
* Description:
*       Adjusts the start and stop time values of GTI using the time frames
*  
* Author: 
*       Alex Muslimov / May 1999
*       NASA/GSFC
*       Raytheon ITSS
*  
* Modification history:
* 
* Usage:
*      void AdjustTimes( unsigned long M, 
*                        unsigned long N, 
*                        double expofrac,
*                        double *StartTime,
*                        double *StopTime,
*                        double *BeginTime,
*                        double *EndTime,
*                        unsigned long *NumTimes,
*                        int *status );
* 
* Primary Variables:
*    --- Inputs ---
*      M         : input number of elements in BeginTime/EndTime arrays
*      N         : input number of elements in StartTime/StopTime arrays
*      expofrac  : input Fraction of exposure to be included in a GTI 
*      StartTime : start time GTI array
*      StopTime  : stop time GTI array
*      BeginTime : begin time FRAMES array
*      EndTime   : end time FRAMES array
*      NumTimes  : output number of elements in StartTime/StopTime arrays
*      status    : output FITSIO error code
* 
******************************************************************************/
void AdjustTimes ( unsigned long M, unsigned long N, double expofrac, 
        double *StartTime, double *StopTime,double *BeginTime, 
        double *EndTime, unsigned long *NumTimes, int *status ) 
{

   unsigned long n, m, counter ;
   double BadTime, denom, frac1, frac2 ;

   *NumTimes = 0 ;
   
   BadTime = -9.99999 ;
   

	for ( n = 0; n < N; n++ ) {

            for( m = 0; m < M; m++ ) {

		denom = *(EndTime + m) - *(BeginTime + m) ;
                frac1 = ( *(EndTime + m) - *(StartTime + n) )/denom ;
                frac2 = ( *(EndTime + m) - *(StopTime + n) )/denom ;

		if( ( *(StartTime + n) >= *(BeginTime + m) ) &&
                    ( *(StartTime + n) <= *(EndTime + m) ) ) {
	          if( frac1 > expofrac ) { 
                   *(StartTime + n) = *(BeginTime + m) ;
		  }
	          if( frac1 <= expofrac ) { 
                   *(StartTime + n) = *(EndTime + m) ;
		  }
                } else if( *(StartTime + n) < BeginTime[0] ) {
                   *(StartTime + n) = BeginTime[0] ;
		} else if( ( *(StopTime + n) >= *(BeginTime + m) ) &&
                         ( *(StopTime + n) <= *(EndTime + m) ) ) {
	           if( frac2 > expofrac ) {
                    *(StopTime + n) = *(BeginTime + m) ;
		   }
	           if( frac2 <= expofrac ) { 
                    *(StopTime + n) = *(EndTime + m) ;
		   }
                } else if( *(StopTime + n) > EndTime[M-1] ) {
                    *(StopTime + n) = EndTime[M-1] ;
		} else if( *(StartTime + n) >= EndTime[M-1] ) {
                  *(StartTime + n) = BadTime ;
                  *(StopTime + n)  = BadTime ;
                } else if( *(StopTime + n) <= BeginTime[0] ) {
                  *(StartTime + n) = BadTime ;
                  *(StopTime + n)  = BadTime ;
                }
                      
            }
         
        }


        /* Sort out the Good times */

        counter = 0 ;
        for ( n = 0; n < N; n++ ) {

         if( *(StartTime + n) != BadTime && 
              *(StopTime + n) != BadTime && 
             *(StartTime + n) != *(StopTime + n) ) {
           *(StartTime + counter) = *(StartTime + n) ;        
           *(StopTime + counter) = *(StopTime + n) ;
           counter++;         
	 }

        }

   *NumTimes = counter;   
   *status = 0 ;     

   return;
}

/* comparision function for the tstart */ 
int comptstart(const void *t1, const void *t2) 
{ 
    gtitime *gt1;
    gtitime *gt2;

    gt1 = (gtitime *)t1;
    gt2 = (gtitime *)t2;

    if(gt1->tstart < gt2->tstart ) return -1;
    if(gt1->tstart > gt2->tstart ) return 1;
    return 0; 
}


/******************************************************************************
* Function:
*      merge_or_gtis
* 
* Description:
*      Merge the tmp gti files with the desired output git file. 
* 
* Author: 
*      Ning Gan / June 1999
*      NASA/GSFC
*      Raytheon ITSS
* 
* Modification history:
* 
* Usage:
*     merge_or_gtis(tmpfile, outfile, int extnum, igti,status)
* 
******************************************************************************/
     void merge_or_gtis(char *tmpfile,   /* tmp gti file */
                        char *outfile,   /* output gti file */
                        char *extnam,    /* gti extension number */
                        char *startname, /*start column name */
                        char *stopname,  /*stop column name */
                        int igti,        /* index of the tmp gti file */
                        int clobber,     /* clobber constant */
                        int *status
                        ) 
{ 
   
    fitsfile *gti;
    fitsfile *out;
    static int ncolstart;
    static int ncolstop;
    long i; 
    gtitime *ptime;  

    long n1,n2; 
    int anynul;
    long j;


    /* initialize the output gti file */
    if (igti == 1) {
       *status = 0;
       fits_open_file( &gti, tmpfile, READONLY, status ) ;
       if(*status)  { 
            fits_report_error(stderr,*status);
            return;
       }

       fits_movnam_hdu(gti,BINARY_TBL,extnam, 0 , status);
       if(*status)  { 
            fits_report_error(stderr,*status);
            return;
       }

       fits_get_colnum(gti, CASEINSEN, startname, &ncolstart,status);
       if(*status)  { 
            fits_report_error(stderr,*status);
            return;
       }

       fits_get_colnum(gti, CASEINSEN, stopname, &ncolstop,status);
       if(*status)  { 
            fits_report_error(stderr,*status);
            return;
       }

       /* open the output file */
       fits_open_file( &out, outfile, READWRITE, status ) ;
       if(*status == FILE_NOT_OPENED ) {
           *status = 0 ;
           fits_clear_errmsg() ;
           fits_create_file( &out, outfile, status ) ;
        } else if( *status == 0 ) {
             if ( clobber ) {
                  fits_delete_file( out, status ) ;
                  fits_create_file( &out, outfile, status ) ;
             }
        } else {
	     fprintf(stderr,"Unable to open/create gti output file.");
             return;
        }
        for (i = 1; fits_movabs_hdu(gti,i,NULL,status) == 0; i++) {
             if(fits_copy_hdu(gti,out,0,status) != 0 ) { 
                 *status = 0;
                 break;
             }
        } 
        *status = 0;
        fits_close_file(gti,status); 
        fits_close_file(out,status); 
        return;
    }

    /* merge the output gti file */
    *status = 0;
    fits_open_file( &gti, tmpfile, READONLY, status ) ;
    if(*status)  { 
        fits_report_error(stderr,*status);
        return;
    } 
    fits_movnam_hdu(gti,BINARY_TBL,extnam, 0 , status);
    if(*status)  { 
        fits_report_error(stderr,*status);
        return;
    } 

    fits_open_file(&out, outfile, READWRITE, status ) ;
    if(*status)  { 
        fits_report_error(stderr,*status);
        return;
    } 
    fits_movnam_hdu(out,BINARY_TBL,extnam, 0 , status);
    if(*status)  { 
        fits_report_error(stderr,*status);
        return;
    } 

    /* get the total number of gtis from tempfile and gti outfile */  
    fits_get_num_rows(out,&n1,status);
    fits_get_num_rows(gti,&n2,status); 

    ptime = (gtitime*) calloc( (n1+n2), sizeof(gtitime));
    for (i = 0; i < n1; i++) { 
        fits_read_col (out, TDOUBLE, ncolstart,
            i+1, 1 ,1, NULL, &(ptime[i].tstart), &anynul, status);
        fits_read_col (out, TDOUBLE, ncolstop,
            i+1, 1 ,1, NULL, &(ptime[i].tstop), &anynul, status);
    } 
    for (i = 0; i < n2; i++) { 
        fits_read_col (gti, TDOUBLE, ncolstart,
            i+1, 1 ,1, NULL, &(ptime[i+n1].tstart), &anynul, status);
        fits_read_col (gti, TDOUBLE, ncolstop,
            i+1, 1 ,1, NULL, &(ptime[i+n1].tstop), &anynul, status);
    } 
    
    qsort((void*) ptime, n1+n2, sizeof(gtitime), comptstart);

    j = 0;
    for (i = 0; i < n1+n2-1; i++) {  
        if(ptime[i+1].tstart >= ptime[i].tstop) { 
            j++;
            fits_write_col (out, TDOUBLE, ncolstart,
                j, 1 ,1, &(ptime[i].tstart), status);
            fits_write_col (out, TDOUBLE, ncolstop,
                j, 1 ,1, &(ptime[i].tstop), status);
            if( i+2 == n1 + n2 ) {
                j++;
                fits_write_col (out, TDOUBLE, ncolstart,
                j, 1 ,1, &(ptime[i+1].tstart), status);
                fits_write_col (out, TDOUBLE, ncolstop,
                j, 1 ,1, &(ptime[i+1].tstop), status);
            }
        } else { 
            ptime[i+1].tstart = ptime[i].tstart; 
            if(ptime[i+1].tstop <= ptime[i].tstop ) 
                 ptime[i+1].tstop = ptime[i].tstop; 
            if(i+2 == n1 + n2 ) {
                j++;
                fits_write_col (out, TDOUBLE, ncolstart,
                j, 1 ,1, &(ptime[i+1].tstart), status);
                fits_write_col (out, TDOUBLE, ncolstop,
                j, 1 ,1, &(ptime[i+1].tstop), status);
            }
        }
    } 
    fits_close_file(gti,status); 
    fits_close_file(out,status); 
    free(ptime);
    return;
}

 
/******************************************************************************
* Function:
*      gtisynch
* 
* Description:
*      "main" entry point of program.  Calls GetParam to get parameters
*      then GtiSynchron to do the processing.
* 
* Author: 
*      Alex Muslimov / May 1999
*      NASA/GSFC
*      Raytheon ITSS
* 
* Modification history:
* 
* Usage:
*      void gtisynch( void )
* 
* Primary Variables:
*    --- Inputs ---
*      none
*    --- Internal ---
*      infile    :   Filename (including extension) of input file
*      gtifile   :   Filename of the GTI file
*      outfile   :   Filename (including extension) of output file
*      expofrac  :   Fraction of exposure to be included in a GTI
*      begin     :   Name of column containing frames begin time
*      end       :   Name of column containing frames end time
*      frames_ext:   Extension name containing frames
*      start     :   Name of column containing GTI start times 
*      stop      :   Name of column containing GTI end times 
*      gti_ext   :   Extension name containing GTIs
*      clobber   :   Optional overwriting of the existing file
*      status    :   FITSIO error code
*    --- Return ---
*      none
* 
******************************************************************************/

int gtisynch (void)
{
   static char taskname[80] = "gtisynch";
   static char version[8] = "2.1";

   char infile[PIL_LINESIZE], gtifile[PIL_LINESIZE], outfile[PIL_LINESIZE] ;   
   char *begin, *end, *start, *stop ;
   char *frames_ext, *gti_ext ;
   FILE *fp;
   char tmpin[FLEN_FILENAME];
   char tmpout[FLEN_FILENAME];
   char *lfile;
   char tmp[100];
   int nevtfile;
   int i;

   int clobber, status ;
   double expofrac ;

   set_toolname(taskname);
   set_toolversion(version);

   clobber  = 0 ;
   expofrac = 0 ;
   status   = 0 ;

   /* Allocate memory for the strings */

      begin      = ( char * )malloc( CBUFSIZE * sizeof( char ) ) ;
      end        = ( char * )malloc( CBUFSIZE * sizeof( char ) ) ;
      frames_ext = ( char * )malloc( CBUFSIZE * sizeof( char ) ) ;
      start      = ( char * )malloc( CBUFSIZE * sizeof( char ) ) ;
      stop       = ( char * )malloc( CBUFSIZE * sizeof( char ) ) ;
      gti_ext    = ( char * )malloc( CBUFSIZE * sizeof( char ) ) ;


   status = GetParam( infile, gtifile, outfile, &expofrac, begin, end, 
                      frames_ext, start, stop, gti_ext, &clobber ) ;

#ifdef TEST                 
printf("GetParam: status=%d \n", status ) ;
#endif
        
    
    if(status ) { 
         fprintf(stderr,"\nError getting the parameters for Gtisynchron\n");
         return status;
    }
           
    if (infile[0] != '@' ) { 
          GtiSynchron( infile, gtifile, outfile, expofrac, begin, end,
               frames_ext, start, stop, gti_ext, clobber, &status) ;
#ifdef TEST                 
          printf("GtiSynch: status=%d \n", status ) ;
#endif
    } 
    else {
          lfile = &infile[1];  
          if ( ( fp = fopen(lfile,"r") ) == NULL) { 
              sprintf(tmp,"Error open list file: %s. ",lfile);
              fprintf(stderr,tmp);
              return status; 
          } 
          nevtfile = 0;
          /* process each event file */
          while ( fgets(tmpin,FLEN_FILENAME,fp) != NULL) { 
              nevtfile++;
              sprintf(tmpout,"gtisynch_%d.tmp",nevtfile);
              GtiSynchron(tmpin, gtifile, tmpout, expofrac, begin, end,
               frames_ext, start, stop, gti_ext, clobber, &status);
              if(status != 0) {
                  sprintf(tmp,"Error in processing file: %s. ",tmpin);
                  fprintf(stderr,tmp);
                  return status;
              }
          }
          /* merge all the tmp files */
          for (i = 1; i <= nevtfile; i++) {
              sprintf(tmpout,"gtisynch_%d.tmp",i);
              merge_or_gtis(tmpout,outfile,gti_ext,start,stop,
                  i,clobber,&status);
              if(status != 0) {
                  sprintf(tmp,"Error in merge file: %s. ",tmpout);
                  fprintf(stderr,tmp);
                  return status;
              }
              remove(tmpout);
          } 
      }

        /* Delete allocated memory */

        free ( infile ) ;
        free ( gtifile ) ;
        free ( outfile ) ;
        free ( begin ) ;
        free ( end ) ;
        free ( frames_ext ) ;
        free ( start ) ;
        free ( stop ) ;
        free ( gti_ext ) ;
   return status;            
}
