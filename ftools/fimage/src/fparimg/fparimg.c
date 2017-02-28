/*
*****************************************************************************

File name: fparimg.c

Task name: fparimg

Description:  Writes a parameter value to an image pixel 
	      

Author/Date: Alex Muslimov/ December 1998

Modification History:
      1/25/99  AM  'infile' changed to 'fitsfile'. In .par file : 
                   non-hidden parameters don't contain default values 
     12/20/99  NG  Updated for reading compressed images.
		   
Usage:	fparimg <value> <fitsfile[ext#]> <pixel_coordinates>
                     
e.g.    fparimg 1998 MyImg.fits[2]  2,33,78

Arguments: none

Functions implemented in this file:


Library routines called:

	Uclgst(char* param name, 
               char* param var, 
               int* status);
	Uclgsd(char* param name, 
               double* param value, 
               int* status);
        fits_open_file( fitsfile *fptr, 
                        char *filename, 
                        int iomode, 
                      > int *status ) ;
	ffghdd( fitsfile *fptr, 
              > int *hdunum );
	ffgky[ljedcm]( fitsfile *fptr, 
                       char *keyname, 
                     > DTYPE *numval, 
                       char *comment, 
                       int *status ) ;

        ffppr[b,i,ui,k,uk,j,uj,e,d]( fitsfile *fptr, 
                                     long group, 
                                     long firstelem, 
                                     long nelements, 
                                     DTYPE *array, 
                                   > int *status ) ;
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

InValue   - parameter value to be written to the specified pixel
Pixel[]   - string containing the input pixel coordinates
X[i]      - X[0] - x pixel coordinate
          - X[1] - y pixel coordinate
          - ........................

NAXIS     - actual image dimension
ImgDim    - image dimension as implied by the number of input  x, y, ... 
             coordinates
Naxis[i]  - length of axis i (x, y , ... )
Fpixel    - "physical" coordinate of a pixel with the given x, y, ... 
            coordinates


*****************************************************************************
*/

#define MaxImgDim 10    /* maximum number of image dimensions */

#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include <fitsio.h>
#include <cfitsio.h>
#include <xpi.h>	/* Required for parameter file operations. */
#include <cftools.h>	/* Required to use c_fcerr, c_fcecho, etc. */
#include <ftools.h>
#ifndef __APPLE__
#include <malloc.h>
#endif



void fparimg()
{
	
	int BufLen_2 = FLEN_FILENAME - 1; /* Required for C calls to Uclxxx. */
        fitsfile *fptr ;

        int status, i,  ImgDim, HduNum , X[MaxImgDim] ;
        long int Naxis[MaxImgDim] , Fpixel ;
        int bitpix, NAXIS;
        double InValue ;
	char *context, *FitFile, *Pixel, *index ;

/* The following call must be used to set the taskname, for use by
   c_fcerr and other routines in the ftools libraries */

	c_ptaskn("fparimg");           /* used by c_fcerr */

/* allocate memory */

        context    = ( char * ) malloc( FLEN_FILENAME * sizeof( char ) ) ;
        FitFile    = ( char * ) malloc( FLEN_FILENAME * sizeof( char ) ) ;
        Pixel      = ( char * ) malloc( FLEN_FILENAME * sizeof( char ) ) ;
        index      = ( char * ) malloc( FLEN_FILENAME * sizeof( char ) ) ;        


/*
        G E T  I N P U T  P A R A M E T E R S      
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/
        status  = 0 ;
	InValue = 0.e0 ;

	/* get the input parameter value */
     
	Uclgsd("invalue", &InValue, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'InValue' not found in .par file.");
	  c_fcerr(context);
          exit( -1 ) ;	
	}
 
	/* get the input FITS file name */

	Uclgst("fitsfile", FitFile, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'FitFile' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}

	/* get the input string containg pixel coordinates */

	Uclgst("pixel", Pixel, &status); 
	if(status != 0)
	{
	  strcpy(context,"Parameter 'Pixel' not found in .par file.");
	  c_fcerr(context);
	  exit(-1) ;
	}

/*
        O P E N  T H E  F I T S  F I L E   W I T H  T H E 
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	         R E A D / W R I T E  A C C E S S
                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/

	ffopen( &fptr, FitFile, READWRITE, &status ) ;
        if ( status != 0 ) {
	  strcpy(context,"Unable to open the FITS file");
	  c_fcerr(context);
	  exit(-1) ;
        }


/* 
        G E T  T H E  V A L U E S  O F  K E Y  W O R D S 
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/


        if (ffgipr(fptr, MaxImgDim, &bitpix,&NAXIS, Naxis, &status)) {
          fits_report_error(stderr,status);
          exit(-1) ;
        }



/* 
        G E T  T H E  V A L U E S  O F  P I X E L   C O O R D I N A T E S
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         	U S I N G  T H E  I N P U T  S T R I N G
                ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/

        Fcgrgs( Pixel , 10000000, &ImgDim, X, X ) ;

	if ( ImgDim != NAXIS ) {
	  strcpy( context, "Image dimension and number of input pixel coordinates don't match" ) ;
	  c_fcerr( context ) ;
	}

 
/* 
       C A L C U L A T E  T H E  'P H Y S I C A L'  P I X E L  N U M B E R 
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 C O R R E S P O N D I N G  T O  T H E  I N P U T 
                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
                         P I X E L  C O O R D I N A T E S
                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/

        Fpixel = 0 ;

        for ( i = NAXIS - 1 ; i >= 0 ; i-- ) {
	  Fpixel = ( long int )( X[ i ] - 1 + Naxis[ i ] * Fpixel ) ;
        }

        Fpixel++ ;


/*
        W R I T E  T H E  I N P U T   P A R A M E T E R   V A L U E 
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       T O  T H E  'P H Y S I C A L'   P I X E L  C O O R D I N A T E
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
	             O F  T H E  I N P U T  I M A G E 
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*/


	ffpprd( fptr, 1, Fpixel, 1, &InValue, &status ) ;

        if ( status != 0 ) {
	  strcpy(context,"Can't write the input parameter value into the image");
	  c_fcerr(context);	
	  exit(-1) ;
        }
	
/* 
              C L O S E   T H E   F I T S   F I L E 
              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*/

	ffclos( fptr, &status ) ;
        if ( status != 0 ) {
	  strcpy( context, "Unable to close the FITS file " ) ;
	  c_fcerr( context ) ;
	  exit(-1) ;
	}



/* delete allocated memory */

        free ( context ) ;
        free ( FitFile ) ;
        free ( Pixel ) ;
        free ( index ) ;



	return;
}

/* The following code is needed to allow IRAF to call fparimg ftool
This extra subroutine layer is needed because of differences in
linkers between vms and unix. */


#ifdef vms
#define F77CALL fparimg
#endif
#ifdef unix
#define F77CALL fparimg_
#endif


void F77CALL()
{
	void fparimg();
	fparimg();
}
