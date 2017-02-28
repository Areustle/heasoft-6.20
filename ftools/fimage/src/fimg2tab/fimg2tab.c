/*
*****************************************************************************

File name: fimg2tab.c

Task name: fimg2tab

Description:  Copies the image pixel values (specified by the column and 
row ranges) to a table extension (BINARY/ASCII). 
	      

Author/Date: Alex Muslimov/ January 1999

Modification History:  
        March 1999 (AM)  -  Added null values in the image. Added the 
                            turning off any scaling before reading the 
                            image. Added copying the keywords BSCALE and 
                            BZERO (if they exist) to TSCALn and TZEROn for 
                            each column. Replaced the parameters binary_format
                            and ascii_format by out_format. Added TNULLn 
                            keyword(if it exists) for every column in the 
                            output table.
Notes:


Usage:	fimg2tab fitsfile outfile cols rows 
                (flipx = no) (flipy = no) (swapxy = no) 
                (rootname = Col_) (coordcol = no) 
                (tabletype = binary) (out_format = - )
                     
e.g.    

Arguments: none

Functions implemented in this file:


Library routines called:

	Uclgst(char* param name, 
               char* param var, 
               int* status);
	Uclpsd(char* param name, 
               double* param value, 
               int* status);
        fits_open_file( fitsfile *fptr, 
                        char *filename, 
                        int iomode, 
                      > int *status ) ;
        fits_create_file( fitsfile **fptr,
	                  char *filename,
			> int *status ) ;
        fits_create_tbl( fitsfile *fptr,
	                 int tbltype,
			 long naxis2,
			 int tfields,
                         char *ttype[],
			 char *tform[],
			 char *tunit[],
			 char *extname,
			 int *status ) ;
	fits_uppercase( char *string ) ;
        fits_read_img_[byt,sht,usht,int,uint,lng,ulng,flt,dbl]( 
	               fitsfile *fptr,
		       long group,
		       long firstelem,
		       long nelements,
		       DTYPE nulval,
		     > DTYPE *array,
		       int *anynul,
		       int *status );
                fftnul( fitsfile *fptr, 
                        int colnum, 
                        long nulval, 
                      > int *status ) ;
                ffpcnd( fitsfile *fptr,
			int column,
			long firstrow,
			long firstelem,
			long nelements,
			DTYPE *array,
                        DTYPE nulval,
		      > int *status ) ;
        fits_set_bscale( fitsfile *fptr,
                         double scale,
                         double zero,
                      >  int *status ) ; 	
	ffgky[ljedcm]( fitsfile *fptr, 
                       char *keyname, 
                     > DTYPE *numval, 
                       char *comment, 
                       int *status ) ;
        ffkeyn( char *keyroot,
	        int index,
	      > char *keyname,
	      > int *status ) ;
	ffclos( fitsfile *fptr, 
                int *status ) ;
        Fcgrgs( rows, 
                nrows, 
              > &numRowRanges, 
	      > startRow, 
	      > endRow );     				    	
	c_fcerr(char* error_message);
	c_fcecho(char* user_message);
	c_ptaskn(char* taskname);

Main Variables:

NAXIS     - Fits file image dimension
Naxis1    - Length of the x-coordinate axis
Naxis2    - Length of the y-coordinate axis
Xfirst[0] - First column of the image fragment to read
Xlast[0]  - Last column of the image fragment to read
Yfirst[0] - First row of the image fragment to read
Ylast[0]  - Last row of the image fragment to read
datatype  - Type of data to read/write (set to double ! )
tabltype  - Type of a Table extension (ascii/binary)
Format    - Format of the output data to write into the Table extension
NumX      - Number of the image columns to read
NumY      - Number of the image rows to read
NumPxls   - Total number of pixels in the image fragment to read

*****************************************************************************
*/

#define MaxImgDim    2    /* maximum image dimension */
#define NumRanges    2    /* maximum number of input coordinate ranges */
#include <stdio.h>        /* Note: for IRAF compatibility,
			     standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <cfitsio.h>
#include <fitsio.h>
#include <xpi.h>	/* Required for parameter file operations. */
#include <cftools.h>	/* Required to use c_fcerr, c_fcecho, etc. */
#include <ftools.h>
#if !defined(__APPLE__) && !defined(__CYGWIN__)
#include <values.h>
#include <malloc.h>
#endif


void fimg2tab()
{

        fitsfile *in , *out ;

	int BufLen_2 = FLEN_FILENAME - 1; /* Required for C calls to Uclxxx. */

	unsigned int Mem_Size_1, Mem_Size_2 ;

        int count, status , NumColRange, NumRowRange ; 
        int Xfirst[ NumRanges ], Yfirst[ NumRanges ] ;
        int Xlast[ NumRanges ], Ylast[ NumRanges ], FlipXopt ;
        int FlipYopt, SwapXYopt, CoordColOpt, tabltype ;
        int NumCols, indat=0, datatype, anynul ;
        int BscaleAvail, BzeroAvail, BlankAvail ;
        int CreateVector, VectorCols=1, CopyAll ;


        long i, j, NAXIS, Naxis[ MaxImgDim ], NAXISn, Naxis1 ; 
        long Naxis2, NumX, NumY=0L, NumPxls, MemBytes, SubMemBytes ;
        long membytes, FirstRow, FirstElem, NumElem, NumMax ; 
        long counter, BITPIX, BLANK ; 

	char **TType, **TForm, *InFile, *OutFile, *context ; 
        char *Cols, *Rows, *RootName, *ColName, *Dform, *TabType ; 
        char *KeyWord, *KeyRoot, *Format, *Vform, *TForm1 ;

	double a[ 1 ], *Array, *SubArray, *array ; 
        double nulval, BSCALE, BZERO ;


/* The following call must be used to set the taskname, for use by
   c_fcerr and other routines in the ftools libraries */

	c_ptaskn("fimg2tab");           /* used by c_fcerr */

/*

       A L L O C A T E   M E M O R Y  F O R  T H E  S T R I N G S
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*/
	context   = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	InFile    = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	OutFile   = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	Cols      = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	Rows      = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	RootName  = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	ColName   = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	KeyWord   = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	KeyRoot   = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	Format    = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	TabType   = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	Dform     = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	Vform     = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;
	TForm1    = ( char * )malloc( FLEN_FILENAME * sizeof( char ) ) ;


/*  
        G E T  I N P U T  P A R A M E T E R S  F R O M  
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               P A R A M E T E R   F I L E
               ~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/

        status = 0 ;
        BscaleAvail = BzeroAvail = BlankAvail = 0 ;

/* get the input FITS file name */

	Uclgst("fitsfile", InFile, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'fitsfile' not found in .par file.");
	  c_fcerr(context);
	  exit(-1)  ;
	}

/* get the output FITS file name */

	Uclgst("outfile", OutFile, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'outfile' not found in .par file.");
	  c_fcerr(context);
	  exit(-1)  ;
	}

/* get the input string containg columns */

	Uclgst("cols", Cols, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'cols' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}

/* get the input string containg rows */

	Uclgst("rows", Rows, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'rows' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}

/* get the input value for vector column output option */

	Uclgsb("vector", &CreateVector, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'vector' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}

/* get the input value for vector copyall option */

	Uclgsb("copyall", &CopyAll, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'copyall' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}

/* get the input value for 'flip X' option  */

	Uclgsb("flipx", &FlipXopt, &status); 
	if(status != 0)
	{
	  strcpy( context,"Parameter 'flipx' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}

/* get the input value for 'flip Y' option  */

	Uclgsb("flipy", &FlipYopt, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'flipy' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}

/* get the input value for 'swap XY' option  */

	Uclgsb("swapxy", &SwapXYopt, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'swapxy' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}

/* get the input string containing column root name  */

	Uclgst("rootname", RootName, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'rootname' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}

/* get the input value for 'coord column' option  */

	Uclgsb("coordcol", &CoordColOpt, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'coordcol' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}

/* get the input string containing table type  */

	Uclgst("tabletype", TabType, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'tabletype' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}

/* get the input string containing the output data format  */

	Uclgst("out_format", Dform, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'out_format' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}


/* encode table type */

        fits_uppercase( TabType ) ;
        tabltype = ASCII_TBL ;
    
	if ( strcmp( TabType, "ASCII" ) == 0 ) {
	  tabltype = ASCII_TBL ;
	  if ( CreateVector ) {
 	     strcpy(context,"Vector columns are not legal in ASCII tables.");
	     c_fcerr(context);
	     exit(-1) ;
	  }
	} else if ( strcmp( TabType, "BINARY" ) == 0 ) {
	  tabltype = BINARY_TBL ;
	} else {
 	          strcpy(context,"Unknown input table type.");
	          c_fcerr(context);
	          exit(-1) ;
	}


/*
    O P E N  T H E  I N P U T  F I T S  F I L E   W I T H  T H E 
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	         R E A D   O N L Y   A C C E S S
                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/



	ffopen( &in, InFile, READONLY, &status ) ;
        if ( status != 0 ) {
	  strcpy(context,"Unable to open the input FITS file");
	  c_fcerr(context);
          ffrprt( stdout, status) ;
	  exit(-1) ;
        }



/*
    O P E N  T H E  O U T P U T  F I T S  F I L E   W I T H  T H E 
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	         R E A D / W R I T E  A C C E S S
                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/

	fits_open_file( &out, OutFile, READWRITE, &status ) ;
        if ( status == FILE_NOT_OPENED ) {
	  status = 0 ;
	  fits_create_file( &out, OutFile, &status ) ;
           if ( status != 0 ) {
	      strcpy(context,"Unable to create the output FITS file");
	      c_fcerr(context);
              ffrprt( stdout, status) ;
	      exit(-1) ;
           }
        }

       /* Copy all previous extensions, if requested */

       if ( CopyAll ) {
           ffcpfl(in, out, 1, 0, 0, &status);
           if ( status != 0 ) {
	      strcpy(context,"Unable to copy previous HDUs to output.");
	      c_fcerr(context);
              ffrprt( stdout, status) ;
	      exit(-1) ;
           }
       }




        datatype = TDOUBLE ;   /*      ALWAYS DOUBLE     !!!     */

   
/* 
        G E T  T H E  V A L U E S  O F  K E Y  W O R D S 
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/



/* get the value of BITPIX keyword */

        
        BITPIX = 0 ;
        strcpy( KeyWord , "BITPIX" ) ;
        ffgkyj( in, KeyWord, &BITPIX, context, &status ) ;
        if ( status != 0 ) {
	  strcpy(context,"BITPIX keyword is missing");
	  c_fcerr(context);
          ffrprt( stdout, status) ;
	  exit(-1) ;
        }


	if ( BITPIX == 8 )   indat = 1 ;
	if ( BITPIX == 16 )  indat = 2 ;
	if ( BITPIX == 32 )  indat = 3 ;
	if ( BITPIX == -32 ) indat = 4 ;
	if ( BITPIX == -64 ) indat = 5 ;


/*
        D E C O D E  T H E  O U T P U T  D A T A  F O R M A T
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/

	fits_uppercase( Dform ) ;
        if ( !strcmp( Dform, "-" ) || !strcmp( Dform," ") ) 
	  {
          /* 
            Get the data type of the image and set the output 
            data format that matchs it
          */
           
           switch ( indat ) {

	    case 1 :

            if( tabltype == ASCII_TBL )
            strcpy( Dform, "I3" ) ;
            if( tabltype == BINARY_TBL )
            strcpy( Dform, "1B" ) ;
            strcpy( Vform, "B" ) ;
           
            break ;

	    case 2 :

            if( tabltype == ASCII_TBL )
            strcpy( Dform, "I6" ) ;
            if( tabltype == BINARY_TBL )
            strcpy( Dform, "1I" ) ;
            strcpy( Vform, "I" ) ;

	    break ;

	    case 3 :

            if( tabltype == ASCII_TBL )
            strcpy( Dform, "I11" ) ;
            if( tabltype == BINARY_TBL )
            strcpy( Dform, "1J" ) ;
            strcpy( Vform, "J" ) ;

	    break ;

	    case 4 :

            if( tabltype == ASCII_TBL )
            strcpy( Dform, "E14.6" ) ;
            if( tabltype == BINARY_TBL )
            strcpy( Dform, "1E" ) ;
            strcpy( Vform, "E" ) ;

	    break ;

	    case 5 :

            if( tabltype == ASCII_TBL )
            strcpy( Dform, "E23.15" ) ;
            if( tabltype == BINARY_TBL )
            strcpy( Dform, "1D" ) ;
            strcpy( Vform, "D" ) ;

	    break ;

            default :
	       strcpy( context, "Unknown output data format." ) ;
	       c_fcerr( context ) ;
               exit( -1 ) ;

	   }


	  }


/*             S E T  U P  T H E  D A T A  F O R M A T          
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*/

                      strcpy( Format, Dform ) ;


/* get the value of NAXIS */
 
	NAXIS = 0 ;
        strcpy( KeyWord , "NAXIS" ) ;
        ffgkyj( in, KeyWord, &NAXIS, 0, &status ) ;
        if ( status != 0 ) {
	  strcpy(context,"NAXIS keyword is missing");
	  c_fcerr(context);
          ffrprt( stdout, status) ;
	  exit(-1) ;
        }

	if ( NAXIS < 1 ) {
	      strcpy(context,"Can't handle image dimension less than 1."); 
	      c_fcerr(context);
	      exit(-1) ;
	}

	if ( NAXIS > MaxImgDim ) { 
	  strcpy( context, "Can't handle image dimension greater than 2." ) ;
	  c_fcerr( context ) ;
	  exit(-1) ;
	}

/* get the values of length of axes */

	strcpy( KeyRoot, KeyWord ) ;
        count = 1 ;
        
        while ( count <= NAXIS ) {
 
	  ffkeyn( KeyRoot, count , KeyWord, &status ) ;
           if ( status != 0 ) {
	      strcpy(context,"Can't produce indexed keyword");
	      c_fcerr(context);
              ffrprt( stdout, status) ;
	      exit(-1) ;
           }	
         
          NAXISn = 0 ;
           
	  ffgkyj( in, KeyWord, &NAXISn, 0, &status ) ;
           if ( status != 0 ) {
	      strcpy(context,"NAXISn keyword is missing");
	      c_fcerr(context);
              ffrprt( stdout, status) ;
	      exit(-1) ;
           }	
	   if ( status == 0 ) 
	     Naxis[ count - 1 ] = NAXISn ;

	count++ ;

	}


/* get the values of BSCALE, BZERO, and BLANK keyword */
 
	BSCALE = 0.0 ;
        strcpy( KeyWord , "BSCALE" ) ;
        ffgkyd( in, KeyWord, &BSCALE, 0, &status ) ;
        if ( status == 0 ) BscaleAvail = 1 ;
        if ( status != 0 ) BscaleAvail = 0 ; 
        status = 0 ;

	BZERO = 0.0 ;
        strcpy( KeyWord , "BZERO" ) ;
        ffgkyd( in, KeyWord, &BZERO, 0, &status ) ;
        if ( status == 0 ) BzeroAvail = 1 ;
        if ( status != 0 ) BzeroAvail = 0 ;
        status = 0 ;

	BLANK = 0 ;
        strcpy( KeyWord , "BLANK" ) ;
        ffgkyj( in, KeyWord, &BLANK, 0, &status ) ;
          if ( status == 0 ) BlankAvail = 1 ;
          if ( status != 0 ) BlankAvail = 0 ;
        status = 0 ;



        
/* 
                 G E T  T H E  C O L U M N  A N D  R O W  R A N G E S 
                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      U S I N G  T H E  I N P U T  S T R I N G S
                      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/

        if ( !strcmp( Cols, "-" ) ) {
	   Xfirst[0] = 1 ;
                if ( NAXIS == 1 ) Xlast[0]  = 999 ;
                if ( NAXIS == 2 ) Xlast[0]  = Naxis[0] ;
	}
        else 
	Fcgrgs( Cols, 10000000, &NumColRange, Xfirst, Xlast ) ;
         
	   NumX = (long int)( Xlast[0] - Xfirst[0] + 1) ;
                if ( NumX > 999 ) {
	           strcpy(context,"Number of columns should be < 1000."); 
	           c_fcerr(context);
	           exit(-1) ;
		}
	           Naxis1 = Naxis[0] ;
	              NumPxls = Naxis1 ;
                         NumMax  = NumX ;



	if ( NAXIS == 1 ) {

          if ( !strcmp( Rows, "-" ) || !strcmp( Rows, " " ) ) {
	        Yfirst[0] = 1 ;
                Ylast[0]  = 1 ;
                NumY      = 1 ;
                strcpy(context,"Warning: Number of rows is set to 1 (You have 1-D image.)") ;
                c_fcecho(context) ;
	  }
          else  
	  Fcgrgs( Rows, 10000000, &NumRowRange, Yfirst, Ylast) ;

	     NumY = (long int )( Ylast[0] - Yfirst[0] + 1 )  ;
	     if ( NumY > 1 ) {
             NumY = 1 ;
	       strcpy(context,"Warning: Number of rows is set to 1 (You have 1-D image.)") ;
               c_fcecho(context) ;
             } 
	 }
       

	if ( NAXIS == 2 ) {

          if ( !strcmp( Rows, "-" ) ) {
	        Yfirst[0] = 1 ;
                Ylast[0]  = Naxis[1] ;
	  }
          else  
	  Fcgrgs( Rows, 10000000, &NumRowRange, Yfirst, Ylast) ;

	     NumY = (long int )( Ylast[0] - Yfirst[0] + 1 )  ;
	        Naxis2 = Naxis[1] ;
	           NumPxls = NumX * NumY ;
	              if ( NumY > NumX ) NumMax = NumY ;

	 }
       
/* allocate memory for 1-d data arrays */

	MemBytes    = ( unsigned ) ( NumPxls * sizeof( double ) ) ;
        SubMemBytes = ( unsigned ) ( NumX * sizeof( double ) ) ;
        membytes    = ( unsigned ) ( NumMax * sizeof( double ) ) ;
        Array       = ( double *) malloc( MemBytes ) ; 
        SubArray    = ( double *) malloc( SubMemBytes ) ;
        array       = ( double *) malloc( membytes ) ;


/* turn off the scaling */

        fits_set_bscale( in, 1., 0., &status ) ;
	       if( status != 0 ) {
		 strcpy( context, "Can't turn off the scaling.") ;
		 c_fcerr( context ) ;
                 ffrprt( stdout, status) ;
		 exit( -1 ) ;
	       }

/* read the image pixels */

        nulval    = DOUBLENULLVALUE ;
        anynul    = 0 ;
        FirstElem = 0 ;

	if ( NAXIS == 1 ) { 
	  FirstElem = ( long int )( Xfirst[0] );
	  fits_read_img_dbl( in, 0 , FirstElem, NumX, nulval, SubArray, 
                    &anynul, &status ) ;
	       if( status != 0 ) {
		 strcpy( context, "Can't read 1-D image.") ;
		 c_fcerr( context ) ;
                 ffrprt( stdout, status) ;
		 exit( -1 ) ;
	       }
	}

	if ( NAXIS == 2 ) {

	  counter = 0 ;
	  for ( i = 0 ; i < NumY ; ++i ) {
	    FirstElem = ( long int )( Xfirst[0] + Naxis1 * 
                         ( Yfirst[0] - 1 + i ) ) ;
	    fits_read_img_dbl( in, 0 , FirstElem, NumX, nulval, 
                     SubArray, &anynul, &status ) ;
	       if ( status != 0 ) {
		 strcpy( context, "Can't read 2-D image." ) ;
		 c_fcerr( context ) ;
                 ffrprt( stdout, status) ;
		 exit( -1 ) ;
	       }
	         for ( j = 0 ; j < NumX ; ++j ) {
		   *(Array + counter) = *(SubArray + j ) ;
		   ++counter ;
		 }
	  }
	}


/*
   C R E A T E   A S C I I / B I N A R Y   T A B L E    E X T E N S I O N 
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/
        

/* allocate memory for 2-d array */

	Mem_Size_1 = ( unsigned ) ( NumX * sizeof( char * ) ) ;
 	Mem_Size_2 = ( unsigned ) ( FLEN_FILENAME * sizeof( char ) ) ; 
 	TType = ( char ** )malloc( Mem_Size_1 ) ;
	TForm = ( char ** )malloc( Mem_Size_1 ) ;

/* append the column index to the column root name */
	
	count = 1 ;
        strcpy( ColName, RootName ) ;

	for ( i = 0 ; i < NumX ; ++i ) {
	  *(TType + i ) = ( char * ) malloc( Mem_Size_2 ) ;
	  *(TForm + i ) = ( char * ) malloc( Mem_Size_2 ) ;
            ffkeyn( RootName, count , ColName, &status ) ;
	       if ( status != 0 ) {
		 strcpy( context, "Can't produce indexed column name." ) ;
		 c_fcerr( context ) ;
                 ffrprt( stdout, status) ;
		 exit( -1 ) ;
	       }
        strcpy( *(TType + i ) , ColName ) ;
        strcpy( *(TForm + i ) , Format ) ;
        ++count ;
        }

/* create BINARY/ASCII Table extension */

        if ( SwapXYopt == 0 ) {
        ffcrtb( out, tabltype, (int)NumY, (int)NumX , TType, TForm, 0 , 0 , &status ) ;
         if ( status != 0 ) {
	  strcpy(context,"Unable to create table extension");
	  c_fcerr(context);
	  exit(-1) ;
         }
        }

        if ( SwapXYopt != 0 ) {
        ffcrtb( out, tabltype, (int)NumX, (int)NumY , TType, TForm, 0 , 0 , &status ) ;
         if ( status != 0 ) {
	  strcpy(context,"Unable to create table extension");
	  c_fcerr(context);
	  exit(-1) ;
         }
        }

/*
       W R I T E   E L E M E N T S  I N T O   B I N A R Y / A S C I I
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     T A B L E   C O L U M N S
                     ~~~~~~~~~~~~~~~~~~~~~~~~~
*/


       switch ( NAXIS ) {

       case 1 :
	         if ( SwapXYopt == 0 ) {
		   if ( FlipXopt == 0 ) {
		     for ( i = 0 ; i < NumX ; ++i ) {
                        a[ 0 ] = *( SubArray + i ) ;
                        /* insert column (i+1) with 1 element, a[0] */
                          if ( BlankAvail && ( 
                               !strcmp( Dform, "1B" ) ||
                               !strcmp( Dform, "1I" ) || 
                               !strcmp( Dform, "1J" )  ) )
                             fftnul( out, (i+1), BLANK, &status ) ;
                             ffpcnd( out, ( ( int )i + 1 ), 1, 1, 1, a, nulval, &status ) ;
                           if ( status != 0 ) {
	                      strcpy(context,"Unable to write column into table extension");
	                      c_fcerr(context);
                              ffrprt( stdout, status) ;
	                      exit(-1) ;
                           }

		     }

                   }
		   else {
		     for ( i = ( NumX - 1 ) ; i >= 0 ; --i ) {
                        a[ 0 ] = *( SubArray + i ) ;
                        /* insert column (NumX-i) with 1 element, a[0] */
                          if ( BlankAvail && ( 
                               !strcmp( Dform, "1B" ) ||
                               !strcmp( Dform, "1I" ) || 
                               !strcmp( Dform, "1J" )  ) ) 
                             fftnul( out, (NumX-i), BLANK, &status ) ;
                             ffpcnd( out, ( int )( NumX - i ), 1, 1, 1, a, nulval, &status ) ;
                           if ( status != 0 ) {
	                      strcpy(context,"Unable to write column into table extension");
	                      c_fcerr(context);
                              ffrprt( stdout, status) ;
	                      exit(-1) ;
                           }

                     }
		   }
		 }
		 else {
		   if ( FlipXopt == 0 ) {
		     for ( i = 0 ; i < NumX ; ++i ) {
		        *( array + i ) = *( SubArray + i ) ;
		     }
		        /* insert column 1 (array) with NumX elements */
                          if ( BlankAvail && ( 
                               !strcmp( Dform, "1B" ) ||
                               !strcmp( Dform, "1I" ) || 
                               !strcmp( Dform, "1J" )  ) )
                             fftnul( out, 1, BLANK, &status ) ;
                             ffpcnd( out, 1, 1, 1, NumX, array, nulval, &status ) ; 
                           if ( status != 0 ) {
	                      strcpy(context,"Unable to write column into table extension");
	                      c_fcerr(context);
                              ffrprt( stdout, status) ;
	                      exit(-1) ;
                           }

		   }
		   else {
		     for ( i = ( NumX - 1 ) ; i >= 0 ; --i ) {
		       *( array + NumX - i - 1 ) = *( SubArray + i ) ;
		     }
		        /* insert column 1 (array) with NumX elements */
                          if ( BlankAvail && ( 
                               !strcmp( Dform, "1B" ) ||
                               !strcmp( Dform, "1I" ) || 
                               !strcmp( Dform, "1J" )  ) ) 
                             fftnul( out, 1, BLANK, &status ) ;
                             ffpcnd( out, 1, 1, 1, NumX, array, nulval, &status ) ;
                           if ( status != 0 ) {
	                      strcpy(context,"Unable to write column into table extension");
	                      c_fcerr(context);
                              ffrprt( stdout, status) ;
	                      exit(-1) ;
                           }

		   }
		 }
       break ;

       case 2 :
	         if ( SwapXYopt == 0 ) {
		   if ( FlipXopt == 0 && FlipYopt == 0 ) {
		     for ( i = 0 ; i < NumX ; ++i ) {
		       for ( j = 0 ; j < NumY ; ++j ) {
			 *( array + j ) = *( Array + i + NumX * j )  ;
		       }
       		       /* insert column (i+1) with NumY elements, array */

                          if ( BlankAvail && ( 
                               !strcmp( Dform, "1B" ) ||
                               !strcmp( Dform, "1I" ) || 
                               !strcmp( Dform, "1J" )  ) )
                             fftnul( out, (i+1), BLANK, &status ) ;
     ffpcnd( out, (1+(int)i), 1, 1, NumY, array, nulval, &status ) ;

                           if ( status != 0 ) {
	                      strcpy(context,"Unable to write column into table extension");
	                      c_fcerr(context);
                              ffrprt( stdout, status) ;
	                      exit(-1) ;
                           }

		     }
		   }
		   else if ( FlipXopt == 1 && FlipYopt == 0 ) {
		     for ( i = (NumX - 1) ; i >= 0 ; --i ) {
		       for ( j = 0 ; j < NumY ; ++j ) {
			 *( array + j ) = *( Array + i + NumX * j )  ;
		       }
       		       /* insert column (NumX-i) with NumY elements, array */
                          if ( BlankAvail && ( 
                               !strcmp( Dform, "1B" ) ||
                               !strcmp( Dform, "1I" ) || 
                               !strcmp( Dform, "1J" )  ) )
                             fftnul( out, (NumX-i), BLANK, &status ) ;
                             ffpcnd( out,( int )( NumX - i ), 1, 1, NumY, array, nulval, &status ) ; 
                           if ( status != 0 ) {
	                      strcpy(context,"Unable to write column into table extension");
	                      c_fcerr(context);
                              ffrprt( stdout, status) ;
	                      exit(-1) ;
                           }

		     }
		   } 
		   else if ( FlipXopt == 0 && FlipYopt == 1 ) {
		     for ( i = 0 ; i < NumX ; ++i ) {
		       for ( j = ( NumY - 1 ) ; j >= 0 ; --j ) {
			 *( array + NumY - j - 1 ) = *( Array + i + NumX * j ) ;
		       }
       		       /* insert column (i+1) with NumY elements, array */
                          if ( BlankAvail && ( 
                               !strcmp( Dform, "1B" ) ||
                               !strcmp( Dform, "1I" ) || 
                               !strcmp( Dform, "1J" )  ) ) 
                             fftnul( out, (i+1), BLANK, &status ) ;
                             ffpcnd( out, ( ( int )i + 1 ), 1, 1, NumY, array, nulval, &status ) ; 
                           if ( status != 0 ) {
	                      strcpy(context,"Unable to write column into table extension");
	                      c_fcerr(context);
                              ffrprt( stdout, status) ;
	                      exit(-1) ;
                           }

		     }
		   } 
		   else if ( FlipXopt == 1 && FlipYopt == 1 ) {
		     for ( i = (NumX - 1) ; i >= 0 ; --i ) {
		       for ( j = ( NumY - 1 ) ; j >= 0 ; --j ) {
			 *( array + NumY - j - 1 ) = *( Array + i + NumX * j ) ;
		       }
       		       /* insert column (NumX-i) with NumY elements, array */
                          if ( BlankAvail && ( 
                               !strcmp( Dform, "1B" ) ||
                               !strcmp( Dform, "1I" ) || 
                               !strcmp( Dform, "1J" )  ) )
                             fftnul( out, (NumX-i), BLANK, &status ) ;
                             ffpcnd( out, ( int )( NumX - i ), 1, 1, NumY, array, nulval, &status ) ;
                           if ( status != 0 ) {
	                      strcpy(context,"Unable to write column into table extension");
	                      c_fcerr(context);
                              ffrprt( stdout, status) ;
	                      exit(-1) ;
                           }

		     }
		   } 
		   else {
		     strcpy( context, "Unknown flipx(y) option.") ;
		     c_fcerr( context ) ;
		     exit( -1 ) ;
		   }
		 }

		 if ( SwapXYopt == 1 ) { 
		   if ( FlipXopt == 0 && FlipYopt == 0 ) {
		     for ( i = 0 ; i < NumY ; ++i ) {
		       for ( j = 0 ; j < NumX ; ++j ) {
			 *( array + NumX - j -1 ) = *( Array + j + NumX * i ) ;
		       }
       		       /* insert column (i+1) with NumX elements, array */
                          if ( BlankAvail && ( 
                               !strcmp( Dform, "1B" ) ||
                               !strcmp( Dform, "1I" ) || 
                               !strcmp( Dform, "1J" )  ) ) 
                             fftnul( out, (i+1), BLANK, &status ) ;
                             ffpcnd( out, ( ( int )i + 1 ), 1, 1, NumX, array, nulval, &status ) ;
                           if ( status != 0 ) {
	                      strcpy(context,"Unable to write column into table extension");
	                      c_fcerr(context);
                              ffrprt( stdout, status) ;
	                      exit(-1) ;
                           }

		     }
		   }
		   else if ( FlipXopt == 1 && FlipYopt == 0 ) {
		     for ( i = 0 ; i < NumY ; ++i ) {
		       for ( j = 0 ; j < NumX ; ++j ) {
			 *( array + j ) = *( Array + j + NumX * i ) ;
		       }
       		       /* insert column (i+1) with NumX elements, array */
                          if ( BlankAvail && ( 
                               !strcmp( Dform, "1B" ) ||
                               !strcmp( Dform, "1I" ) || 
                               !strcmp( Dform, "1J" )  ) )
                             fftnul( out, (i+1), BLANK, &status ) ;
                             ffpcnd( out, ( ( int )i + 1 ), 1, 1, NumX, array, nulval, &status ) ;
                           if ( status != 0 ) {
	                      strcpy(context,"Unable to write column into table extension");
	                      c_fcerr(context);
                              ffrprt( stdout, status) ;
	                      exit(-1) ;
                           }

		     }
		   } 
		   else if ( FlipXopt == 0 && FlipYopt == 1 ) {
		     for ( i = ( NumY -1 ) ; i >= 0 ; --i ) {
		       for ( j = ( NumX - 1 ) ; j >= 0 ; --j ) {
			 *( array + NumX - j - 1 ) = *( Array + j + NumX * i ) ;
		       }
       		       /* insert column (NumY-i) with NumX elements, array */
                          if ( BlankAvail && ( 
                               !strcmp( Dform, "1B" ) ||
                               !strcmp( Dform, "1I" ) || 
                               !strcmp( Dform, "1J" )  ) ) 
                             fftnul( out, (NumY-i), BLANK, &status ) ;
                             ffpcnd( out, ( int )( NumY - i ), 1, 1, NumX, array, nulval, &status ) ;
                           if ( status != 0 ) {
	                      strcpy(context,"Unable to write column into table extension");
	                      c_fcerr(context);
                              ffrprt( stdout, status) ;
	                      exit(-1) ;
                           }

		     }
		   } 

		   else if ( FlipXopt == 1 && FlipYopt == 1 ) {
		     for ( i = (NumY - 1) ; i >= 0 ; --i ) {
		       for ( j = 0 ; j < NumX ; ++j ) {
			 *( array + j ) = *( Array + j + NumX * i ) ;
		       }
       		       /* insert column (NumY-i) with NumX elements, array */
                          if ( BlankAvail && ( 
                               !strcmp( Dform, "1B" ) ||
                               !strcmp( Dform, "1I" ) || 
                               !strcmp( Dform, "1J" )  ) )
                             fftnul( out, (NumY-i), BLANK, &status ) ;
                             ffpcnd( out, ( int )( NumY - i ), 1, 1, NumX, array, nulval, &status ) ;
                           if ( status != 0 ) {
	                      strcpy(context,"Unable to write column into table extension");
	                      c_fcerr(context);
                              ffrprt( stdout, status) ;
	                      exit(-1) ;
                           }

		     }
		   }
		   else {
		     strcpy( context, "Unknown flipx(y) option.") ;
		     c_fcerr( context ) ;
		     exit( -1 ) ;
		   }
		 }
     break ;
		 
     default :

	 strcpy( context, "Unable to handle image dimension greater than 2.") ;
	 c_fcerr( context ) ;
	 exit( -1 ) ;

     }                    /* end of SWITCH block */
	       

/* get the number of columns from the created Table */

          ffrdef( out, &status );
          if ( status != 0 ) {
	     strcpy(context,"Unable to re-initialize FITSIO buffers.");
	     c_fcerr(context);
             ffrprt( stdout, status) ;
	     exit(-1) ;
          }
	  NumCols = 0 ;
	  fits_get_num_cols( out, &NumCols, &status ) ;
           if ( status != 0 ) {
	      strcpy(context,"Can't get the number of columns.");
	      c_fcerr(context);
              ffrprt( stdout, status) ;
	      exit(-1) ;
           }

/* If creating vector column output, reset TFIELDS=1, TFORM1=NumCols,
 * re-initialize FITSIO buffers, and delete all TFORMn & TTYPEn keywords 
 * for 1 < n < NumCols.  Afterwards set NumCols=1 so that only TSCAL1, 
 * TZERO1, and TNULL1 are written in the loop below. */

       if ( CreateVector ) {

          strcpy( KeyWord, "TFIELDS" ) ;
          ffukyj( out, KeyWord, VectorCols, "number of fields in each row", &status ) ;
          if ( status != 0 ) {
	     strcpy(context,"Unable to update TFIELDS keyword.");
	     c_fcerr(context);
             ffrprt( stdout, status) ;
	     exit(-1) ;
          }
          ffnkey( NumCols, Vform, TForm1, &status) ;
          if ( status != 0 ) {
	     strcpy(context,"Unable to create new TFORM1 value.");
	     c_fcerr(context);
             ffrprt( stdout, status) ;
	     exit(-1) ;
          }
          strcpy( KeyWord, "TFORM1" ) ;
          ffukys( out, KeyWord, TForm1, "data format of field", &status );
          if ( status != 0 ) {
	     strcpy(context,"Unable to update TFORM1 keyword.");
	     c_fcerr(context);
             ffrprt( stdout, status) ;
	     exit(-1) ;
          }
          ffrdef( out, &status );
          if ( status != 0 ) {
	     strcpy(context,"Unable to re-initialize FITSIO buffers.");
	     c_fcerr(context);
             ffrprt( stdout, status) ;
	     exit(-1) ;
          }
          for ( i = 2 ; i <= NumCols; i++ ) {

             strcpy( RootName , "TFORM" ) ;
             ffkeyn( RootName , i, KeyWord , &status) ;
             if ( status != 0 ) {
	        strcpy(context,"Unable to create indexed keyword names TFORMn.");
	        c_fcerr(context);
                ffrprt( stdout, status) ;
	        exit(-1) ;
             }
             ffdkey( out, KeyWord , &status) ;
             if ( status != 0 ) {
	        strcpy(context,"Unable to delete indexed keywords TFORMn.");
	        c_fcerr(context);
                ffrprt( stdout, status) ;
	        exit(-1) ;
             }
             strcpy( RootName , "TTYPE" ) ;
             ffkeyn( RootName , i, KeyWord , &status) ;
             if ( status != 0 ) {
	        strcpy(context,"Unable to create indexed keyword names TTYPEn.");
	        c_fcerr(context);
                ffrprt( stdout, status) ;
	        exit(-1) ;
             }
             ffdkey( out, KeyWord , &status) ;
             if ( status != 0 ) {
	        strcpy(context,"Unable to delete indexed keywords TTYPEn.");
	        c_fcerr(context);
                ffrprt( stdout, status) ;
	        exit(-1) ;
             }

          }
          NumCols = 1;

       } /* End CreateVector conditional */

/* write in the key words TSCALn,TZEROn, and TNULLn for each column */

       for ( i = 0 ; i < NumCols ; i++ ) {
          
          if ( BscaleAvail ) {          

          strcpy( RootName , "TSCAL" ) ;
          ffkeyn( RootName , i+1, KeyWord , &status) ;
           if ( status != 0 ) {
	      strcpy(context,"Can't create indexed keyword TSCALn.");
	      c_fcerr(context);
              ffrprt( stdout, status) ;
	      exit(-1) ;
           }
          ffpkyd( out, KeyWord, BSCALE, 15, 0 , &status ) ;
           if ( status != 0 ) {
	      strcpy(context,"Can't write in the value of TSCALn.");
	      c_fcerr(context);
              ffrprt( stdout, status) ;
	      exit(-1) ;
           }

	  }

          if ( BzeroAvail ) {

	  strcpy( RootName , "TZERO" ) ;
          ffkeyn( RootName, i+1, KeyWord , &status) ;
           if ( status != 0 ) {
	      strcpy(context,"Can't create indexed keyword TZEROn.");
	      c_fcerr(context);
              ffrprt( stdout, status) ;
	      exit(-1) ;
           }
          ffpkyd( out, KeyWord, BZERO, 15, 0, &status ) ;
           if ( status != 0 ) {
	      strcpy(context,"Can't write in the value of TZEROn.");
	      c_fcerr(context);
              ffrprt( stdout, status) ;
	      exit(-1) ;
           }

          }

          if ( BlankAvail ) { 
	       strcpy( RootName , "TNULL" ) ;
               ffkeyn( RootName, i+1, KeyWord , &status) ;
              if ( status != 0 ) {
	        strcpy(context,"Can't create indexed keyword TNULLn.");
	        c_fcerr(context);
                ffrprt( stdout, status) ;
	        exit(-1) ;
              }

             if ( tabltype == BINARY_TBL && 
                  ( !strcmp(Dform, "1B" ) || 
                    !strcmp(Dform, "1I" ) || 
                    !strcmp(Dform, "1J" ) ) )  
             {
               ffpkyj( out, KeyWord, BLANK, 0, &status ) ;
                if ( status != 0 ) {
	          strcpy(context,"Can't write in the value of TNULLn.");
	          c_fcerr(context);
                  ffrprt( stdout, status) ;
	          exit(-1) ;
                }
	     }
 
          }

       }   /* end of i-loop */

       /* Copy all remaining extensions, if requested */

       if ( CopyAll ) {
           ffcpfl(in, out, 0, 0, 1, &status);
           if ( status != 0 ) {
	      strcpy(context,"Unable to copy remaining HDUs to output.");
	      c_fcerr(context);
              ffrprt( stdout, status) ;
	      exit(-1) ;
           }
       }

/* 
              C L O S E   T H E   I N P U T  F I T S   F I L E 
              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/

	ffclos( in, &status ) ;
        if ( status != 0 ) {
	  strcpy( context, "Unable to close the input FITS file " ) ;
	  c_fcerr( context ) ;
          ffrprt( stdout, status) ;
	  exit(-1) ;
	}


/* 
              C L O S E   T H E   O U T P U T  F I T S   F I L E 
              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/

	ffclos( out, &status ) ;
        if ( status != 0 ) {
	  strcpy( context, "Unable to close the output FITS file " ) ;
	  c_fcerr( context ) ;
          ffrprt( stdout, status) ;
	  exit(-1) ;
	}


/* delete allocated memory */

	free( Array ) ;
        free( SubArray ) ;
        free( array ) ;

	for ( i = 0 ; i < NumX ; ++i ) {
	free( TType[ i ] ) ;
        free( TForm[ i ] ) ;
        }

	free( TType ) ;
        free( TForm ) ;
	free( context ) ;
	free( InFile ) ;
	free( OutFile ) ;
	free( Cols ) ;
	free( Rows ) ;
	free( RootName ) ;
        free( ColName ) ;
	free( KeyWord ) ;
	free( KeyRoot ) ;
        free( Format ) ;
	free( TabType ) ;
	free( Dform ) ;
	free( Vform ) ;
	free( TForm1) ;

/* memory deleted */
        
        return;
}


