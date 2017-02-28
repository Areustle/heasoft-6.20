/******************************************************************************
* File name:   fimgextract.c
* 
* Task name:   fimgextract
* 
* Description: Create an image/primary array from data extracted from either
*              a table extension or another image.  A single column of a table
*              (single valued or vector/array) or a contiguous subset of a
*              data cube/image serves as the input.  An arbitrary list of
*              rows of the table (or final dimension of a data cube/image)
*              can be selected to be STACKed/concatenated, SUMmed, or AVeraGed.
* 
* Author/Date: Peter D. Wilson/Oct 1997
*              NASA/GSFC
*              Hughes STX
* 
* Modification History:
*              10/06/97 1.0a PDW -- Creation (from cdummyftool.c)
*              06/17/98 1.1  PDW -- Modified file handling to support
*                                   new extended syntax.  Had to change
*                                   output file behavior a little
*              12/20/99 1.2  PDW -- Handle input data scaling (BZERO/TZERO..)
*              12/20/99 1.3  NG  -- Updated for reading the compressed
*                                   images
* 
* Usage:       fimgextract (host)
*              fimgextract (IRAF)
* 
* Arguments:   From .par file
* 
* Functions implemented in this file:
*        void  fimgextract() :  Entry point of program
*        int   gImgExtract() :  Reads .par file
*        void  ImgExtract()  :  Extracts/processes data cube
*        int   ReadImg()     :  Reads image data of arbitrary type
*        int   ReadCol()     :  Reads tabular data of arbitrary type
*        int   WriteImg()    :  Writes image file, including nulls
*        int   CopyKeys()    :  Copy relevant input keywords to output
*        char* Trunc()       :  Truncates str to given length
* 
* Library routines called:
*        Uclgsx(char* param name, void* param var, int* status);
*        c_fcerr(char* error_message);
*        Fcxxxx(...);
*        fits_xxxx_xxx(...);
*        sprintf(...), strcpy(...), strlen(...)
*****************************************************************************/

#include <stdio.h>      /* Note: for IRAF compatibility,
                        standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>

#include <xpi.h>        /* Required for parameter file operations. */
#include <ftools.h>
#include <cftools.h>    /* Required to use c_fcerr, c_fcecho, etc. */
#include <fitsio.h>
#if (defined __APPLE__) || (defined __CYGWIN__)
#include <float.h>
#include <limits.h>
#define MAXDOUBLE DBL_MAX
#define MAXFLOAT  FLT_MAX
#define MAXINT    INT_MAX
#else
#include <values.h>
#endif

#define MAXHDU 100000
#define MAXRANGES 15
#define BUFSIZE  255
#define CBUFSIZE 256

enum { STACK_OP, SUM_OP, AVG_OP };

/******************************************************************************
* Function:
*       gImgExtract
* 
* Description:
*       Read parameter file
*  
* Author: 
*       Peter D. Wilson/Sept 1997
*       NASA/GSFC
*       Hughes STX
*  
* Modification history:
* 
* Usage:
*      int gImgExtract ( char *infile,  char *outfile,   char *cols,
*                        char *rows,    int  *operation, int  *nullval,
*                        int  *clobber, int  *copykeys,  int  *history    );
* 
* Primary Variables:
*    --- Inputs ---
*      infile    :   Filename (including extension) of input file
*      outfile   :   Filename (including extension) of output file
*      cols      :   Name of table column or list of image bounds
*      rows      :   List of row ranges to be processed
*      operation :   Operation to perform on input data (STACK, SUM, or AVG)
*      nullval   :   Numerical value of integer NULLS in output image
*      clobber   :   Overwrite existing files?
*      copykeys  :   Copy relevant keywords to output?
*      history   :   Put history keywords into output file?
*    --- Internal ---
*      opstr     :   Operation code string from .par file
*      status    :   FITSIO error code
*    --- Return ---
*      int       :   FITSIO error code
* 
******************************************************************************/
int gImgExtract ( char *infile, char *outfile, char *cols,
                  char *rows, int *operation, int *nullval,
                  int *clobber, int *copykeys, int *history)
{
   char opstr[CBUFSIZE];
   int status = OK;

   int BufLen_2 = BUFSIZE;   /* Required for C calls to Uclxxx. */

   Uclgst("infile", infile, &status);
   if(status != OK) {
      c_fcerr("Parameter 'infile' not found in .par file.");
      return status;
   }

   Uclgst("outfile", outfile, &status);
   if(status != OK) {
      c_fcerr("Parameter 'outfile' not found in .par file.");
      return status;
   }

   Uclgst("cols", cols, &status);
   if(status != OK) {
      c_fcerr("Parameter 'cols' not found in .par file.");
      return status;
   }

   Uclgst("rows", rows, &status);
   if(status != OK) {
      c_fcerr("Parameter 'rows' not found in .par file.");
      return status;
   }

   Uclgst("operation", opstr, &status);
   if(status != OK) {
      c_fcerr("Parameter 'operation' not found in .par file.");
      return status;
   }
   fits_uppercase(opstr);
   if( !strcmp(opstr,"AVG") ) *operation = AVG_OP;
   else if( !strcmp(opstr,"SUM") ) *operation = SUM_OP;
   else if( ! strcmp(opstr,"STACK") ) *operation = STACK_OP;
   else {
      status = NOT_OK;
      c_fcerr("Bad operation parameter. Only use SUM, AVG, or STACK.");
      return status;
   }

   Uclgsi("nullval", nullval, &status);
   if(status != OK) {
      c_fcerr("Parameter 'nullval' not found in .par file.");
      return status;
   }

   Uclgsb("clobber", clobber, &status);
   if(status != OK) {
      c_fcerr("Parameter 'clobber' not found in .par file.");
      return status;
   }

   Uclgsb("copykeys", copykeys, &status);
   if(status != OK) {
      c_fcerr("Parameter 'copykeys' not found in .par file.");
      return status;
   }

   Uclgsb("history", history, &status);
   if(status != OK) {
      c_fcerr("Parameter 'history' not found in .par file.");
      return status;
   }

   return status;
}

/******************************************************************************
* Function:
*      ImgExtract
* 
* Description:
*      The workhorse!  Extract rows of data from either a table column
*      or an image data cube and either sum, average, or stack them,
*      outputting the results to an image extension or primary array.
* 
* Author: 
*      Peter D. Wilson/Oct 1997
*      NASA/GSFC
*      Hughes STX
* 
* Modification history:
*      06/17/98 PDW: New CFITSIO extended filenames require new extension
*                    handling.  No longer call fcpars; get extension number
*                    *after* opening file.
*      12/20/99 PDW: Turn off data scaling of BZERO/TZERO/etc when handling
*                    data.
* 
* Usage:
*      void ImgExtract ( char *infile,  char *outfile,   char *cols,
*                        char *rows,    int   operation, int   nullval, 
*                        int   clobber, int   copykeys,  int   history,
*                        int  *status                                     );
* 
* Primary Variables:
*    --- Inputs ---
*      infile    :   Filename (including extension) of input file
*      outfile   :   Filename (including extension) of output file
*      cols      :   Name of table column or list of image bounds
*      rows      :   List of row ranges to be processed
*      operation :   Operation to perfrom on input data (SUM, AVG, or STACK)
*      nullval   :   Numerical value of integer NULLs in output image
*      clobber   :   Overwrite existing files?
*      copykeys  :   Copy relevant keywords to output?
*      history   :   Put history keywords into output file?
*      status    :   FITSIO error code
*    --- Internal ---
*      in, out   :   Input and Output FITS files
*      inExt     :   FITS HDU extension to read from
*      outExt    :   FITS HDU extension to write to
*      hdutypeIn :   HDU type of input extension/primary array (0=Image)
*      naxis_in  :   Dimension of input data
*      naxes_in  :   Size of each dimension of input data
*      naxis_out :   Dimension of output data
*      naxes_out :   Size of each dimension of output data
*      startRow  :   Array of starting row numbers for each range in 'rows'
*      endRow    :   Array of ending row numbers for each range in 'rows'
*      nrows     :   Number of rows in input file
*      numrows   :   Number of rows to be processed (and perhaps written out)
*      colnum    :   Column number of the Binary or ASCII table to be extracted
*      startCol  :   Array of starting column numbers for each dimension
*                       of input image
*      endCol    :   Array of ending column numbers for each dimension
*                       of input image
*      buffer    :   Buffer holding current row of input data
*      bufferSum :   Buffer holding running sum of input data
*      Nulls     :   Array indicating which data in buffer are undefined
*      NullsSum  :   Array indicating which data in bufferSum are undefined
*      anynul    :   Flag indicating most recent read encountered a NULL
*      inputNull :   Flag indicating whether input data may have NULLs
*      typecode  :   FITS data type being processed within buffer and bufferSum
*      typesize  :   Byte count of data being used (ie, 1=char,..., 8=double)
*      bitpix    :   FITS image data type to be written out
*      buflen    :   Number of data to be read or written at a time
*                       (ie, size of buffer and bufferSum, not a byte count)
*      loc       :   Location within output HDU to write next row of data
*      context   :   Error message string
*    --- Return ---
*      none
* 
******************************************************************************/
void ImgExtract ( char *infile, char *outfile, char *cols,
                  char *rows, int operation, int nullval, 
                  int clobber, int copykeys, int history, int *status)
{
   fitsfile *in, *out;
   char context[CBUFSIZE];
   int i, j, inExt, outExt, hdutypeIn, hdutypeOut, anynul;
   int simple, bitpix, naxis_in, naxis_out, extend, typecode, typesize;
   long naxes_in[MAXRANGES], naxes_out[MAXRANGES], inc[MAXRANGES];
   long fpixels[MAXRANGES], lpixels[MAXRANGES], loc;
   long repeat, width, pcount, gcount;
   long row, nrows, numrows;
   int startRow[MAXRANGES], endRow[MAXRANGES], numRowRanges, direction;
   int colnum, startCol[MAXRANGES], endCol[MAXRANGES];
   int tmpNull, inputNull;

   void *buffer;
   double *bufferSum;
   char *Nulls, *NullsSum;
   size_t buflen;

   int BufLen_1 = BUFSIZE;    /*                                  */
   int BufLen_2 = BUFSIZE;    /*  Required for C calls to Fcxxxx. */ 
   int MaxElem_2 = MAXRANGES; /*                                  */

   /****************      Function Prototypes      ****************/
   char *Trunc( char *str, int len );
   int ReadSubset( fitsfile *fptr, int type, int naxis, long *naxes,
                   long *fpixels, long *lpixels, long *inc, void *buffer,
                   char *nularray, int *anynul, int *status );
   int ReadCol( fitsfile *fptr, int type, int col, long row,
                long felem, long nelem, void *buffer,
                char *nularray, int *anynul, int *status );
   int WriteImg( fitsfile *fptr, int type, long felem, long nelem,
                 void *buffer, char *nularray, int nullval, int *status );
   int CopyKeys( fitsfile *in, fitsfile *out, int hdu,
		 int colnum, int *status );

   context[0]=0;     /*  Clear Error String  */
   buffer=0;         /*    and pointers      */
   bufferSum=0;
   Nulls=NullsSum=0;

   /**********************************/
   /*   Open and move to input HDU   */
   /**********************************/

   if( fits_open_file( &in, infile, READONLY, status) ) {
      sprintf(context, "...Error opening %s", Trunc( infile, 60 ) );
      goto Error;
   }
   fits_get_hdu_num( in, &inExt );
   if( fits_get_hdu_type( in, &hdutypeIn, status ) ) {
      sprintf( context, "...Couldn't obtain hdu type in input file." );
      goto CleanUp1;
   }

   if( inExt==1 ) { /* Opened primary array... Check if anything there */
      if( fits_get_img_dim (in, &naxis_in, status)) {
         sprintf(context,"...Unable to get dimensions of the image in primary header.");
         goto CleanUp1;
      }
      if( naxis_in==0 ) {
	 if( fits_movrel_hdu( in, 1, &hdutypeIn, status ) ) {
	    sprintf( context, 
		     "...Couldn't move to first extension in input file." );
	    goto CleanUp1;
	 }
      }
   }

   /*********************************************************************/
   /*   Read in HDU parameters and determine dimensions of input data   */
   /*********************************************************************/

   switch( hdutypeIn ) {
   case IMAGE_HDU:
      if( fits_read_imghdr( in, MAXRANGES, &simple, &bitpix, &naxis_in,
                            naxes_in, &pcount, &gcount, &extend, status ) ) {
         strcpy(context,"...Input image header could not be read.");
         goto CleanUp1;
      }
      if( naxis_in==0 ) {
         strcpy( context, "...Input image header is empty." );
         *status=NOT_OK;
         goto CleanUp1;
      } else if( naxis_in==1 ) {
         naxis_in = 2;
         naxes_in[1] = naxes_in[0];
         naxes_in[0] = 1;
      } else if( naxis_in>MAXRANGES ) {
         strcpy(context,"...Input image header has too many axes.");
         *status = NOT_OK;
         goto CleanUp1;
      }
      nrows = naxes_in[naxis_in-1];

      if( cols[0]=='-' && cols[1]==0 ) {  /*  Process entire image  */
         naxis_out = naxis_in-1;
         for( i=0; i<naxis_out; i++ ) {
            startCol[i] = 1;
            naxes_out[i] = endCol[i] = naxes_in[i];
         }
      } else {
         Fcgrgs( cols, MAXINT, &naxis_out, startCol, endCol );
         if( (naxis_out != naxis_in-1) ) {
            sprintf( context, "%s has an improper number of ranges.",
                     Trunc( cols, 45 ) );
            *status=NOT_OK;
            goto CleanUp1;
         }
            /*  Check bounds and get output size  */
         for( i=0; i<naxis_out; i++ ) {
            if( endCol[i]>naxes_in[i] ) endCol[i] = naxes_in[i];
            else if( endCol[i]<1 ) endCol[i] = 1;
            if( startCol[i]>naxes_in[i] ) startCol[i] = naxes_in[i];
            else if( startCol[i]<1 ) startCol[i] = 1;
            if( (naxes_out[i] = endCol[i]-startCol[i]+1) < 1 ) {
               j=startCol[i]; startCol[i]=endCol[i]; endCol[i]=j;
               naxes_out[i] = endCol[i]-startCol[i]+1;
            }
         }
      }

      switch( bitpix ) {
      case BYTE_IMG:    typecode = TBYTE;    break;
      case SHORT_IMG:   typecode = TSHORT;   break;
      case LONG_IMG:    typecode = TLONG;    break;
      case FLOAT_IMG:   typecode = TFLOAT;   break;
      case DOUBLE_IMG:  typecode = TDOUBLE;  break;
      default:
         strcpy(context,"Bitpix of unknown type.");
         *status = NOT_OK;
         goto CleanUp1;
         break;
      }

      /* Unless the input data type is double, if we are averaging
         the input data, then the output image data type should be
         float to avoid truncating the averaged values */

      if ((operation == AVG_OP) && (bitpix != DOUBLE_IMG))
      {
         bitpix = FLOAT_IMG;
      }

      /*   Check whether NULLs are present.  If yes, set nullval   */
      if( fits_read_key( in, TINT, "BLANK", &tmpNull, 0, status ) ) {
	 *status = OK;
	 inputNull = FALSE;
      } else {
	 inputNull = TRUE;
	 if( bitpix>0 )
	    nullval = tmpNull;
      }

      break;
   case ASCII_TBL:
   case BINARY_TBL:
      fits_get_colnum( in, 0, cols, &colnum, status );
      fits_get_coltype( in, colnum, &typecode, &repeat, &width, status );
      fits_read_tdim( in, colnum, MAXRANGES, &naxis_in, naxes_in, status );
      fits_read_key( in, TLONG, "NAXIS2", &nrows, 0, status );
      if( *status ) {
         sprintf( context,"...Error reading information for column %s.",
                  Trunc( cols, 35 ) );
         goto CleanUp1;
      }

      for( i=0; i<naxis_in; i++)
	 naxes_out[i] = naxes_in[i];
      naxis_out = naxis_in;
      naxis_in++;
      naxes_in[naxis_in-1] = nrows;

      switch( typecode ) {
      case TBYTE:    bitpix = BYTE_IMG;    break;
      case TUSHORT:
      case TSHORT:   bitpix = SHORT_IMG;   break;
      case TULONG:
      case TLONG:    bitpix = LONG_IMG;    break;
      case TFLOAT:   bitpix = FLOAT_IMG;   break;
      case TDOUBLE:  bitpix = DOUBLE_IMG;  break;
      default:
         strcpy(context,"Column data type is not supported.");
         *status = NOT_OK;
         goto CleanUp1;
         break;
      }

      /* Unless the input data type is double, if we are averaging
         the input data, then the output image data type should be
         float to avoid truncating the averaged values */

      if ((operation == AVG_OP) && (typecode != TDOUBLE))
      {
         bitpix = FLOAT_IMG;
      }

      /*   Check whether NULLs are present.  If yes, set nullval   */
      tmpNull = nullval;
      sprintf( context, "TNULL%d",colnum );
      if( hdutypeIn==ASCII_TBL )
              fits_read_card( in, context, context, status );
      else fits_read_key( in, TINT, context, &tmpNull, 0, status );
      if( *status ) {
	 *status = OK;
	 inputNull = FALSE;
      } else {
	 inputNull = TRUE;
	 if( bitpix>0 )
	    nullval = tmpNull;
      }
      context[0]=0;

      break;
   default:
      strcpy(context,"Unsupported extension type.");
      *status = NOT_OK;
      goto CleanUp1;
   }

   /**********************************************************/
   /*   Setup data buffers for input and processing data--   */
   /*      If data is to be processed, do it all in double   */
   /*      format to prevent overflow.  FITSIO handles       */
   /*      all data conversion from and to the FITS file.    */
   /**********************************************************/

   if( operation!=STACK_OP ) {
      typecode = TDOUBLE;
      typesize = sizeof(double);
   } else
      typesize = (bitpix<0 ? -bitpix : bitpix)>>3;
   for( buflen=1,i=0; i<naxis_out; i++ ) buflen *= naxes_out[i];
   buffer = (void *)malloc( typesize*buflen );
   Nulls = (char *)malloc( sizeof(char)*buflen );
   if( buffer==0 || Nulls==0 ) {
      strcpy(context,"Couldn't allocate memory for input buffers.");
      *status = NOT_OK;
      goto CleanUp2;
   }
   if( operation!=STACK_OP ) {
      bufferSum = (double *)malloc( sizeof(double)*buflen );
      NullsSum = (char *)malloc( sizeof(char)*buflen );
      if( bufferSum==0 || NullsSum==0 ) {
         strcpy(context,"Couldn't allocate memory for summed input buffers.");
         *status = NOT_OK;
         goto CleanUp2;
      }
      for( i=0; i<buflen; i++ ) {
         NullsSum[i] = 0;
         bufferSum[i] = 0.0;
      }
   }

   /************************************/
   /*   Decode row ranges to be read   */
   /************************************/

   Fcgrgs( rows, nrows, &numRowRanges, startRow, endRow );
   for( numrows=0,i=0; i<numRowRanges; i++ ) {
      if( startRow[i]<1 ) startRow[i] = 1;
      if( endRow[i]<1 ) endRow[i] = 1;
      j = endRow[i]-startRow[i];
      if( j > 0 ) numrows += j+1;
      else numrows += 1-j;
   }
   if( operation==STACK_OP ) {
      naxis_out++;
      naxes_out[naxis_out-1] = numrows;
   }

   /************************************************************************/
   /*   Open/Create output file and image extension and/or primary array   */
   /************************************************************************/

   fits_parse_extnum( outfile, &outExt, status );
   fits_parse_rootname( outfile, context, status );
   strcpy( outfile, context );

   fits_open_file( &out, outfile, READWRITE, status );
   if( *status==FILE_NOT_OPENED ) {
      *status = OK;
      fits_clear_errmsg();
      fits_create_file( &out, outfile, status );
      if( outExt>1 ) fits_create_img( out, 8, 0, 0, status );
   } else if( *status!=OK ) {
      sprintf( context,"...Trouble opening output file %s.",
               Trunc( outfile, 45 ) );
      goto CleanUp3;
   } else if( clobber ) {
      fits_delete_file( out, status );
      fits_create_file( &out, outfile, status );
      if( outExt>1 ) fits_create_img( out, 8, 0, 0, status );
   } else if( outExt==1 ) {
      strcpy( context,"Primary array already exists. Cannot overwrite.");
      strcpy( context+strlen(context)," Use clobber=yes." );
      *status = NOT_OK;
      goto CleanUp3;
   } else {
      if( outExt==-99 ) outExt = MAXHDU;
      fits_movabs_hdu( out, outExt-1, &hdutypeOut, status );
      if( *status==END_OF_FILE ) {
         *status=OK;
         fits_clear_errmsg();
      } else if( *status ) {
	 strcpy( context,"Unable to move to proper extension.");
	 goto CleanUp3;
      }
   }
   fits_insert_img( out, bitpix, naxis_out, naxes_out, status );

   /******************************************/
   /*   Write additional keywords to header  */
   /******************************************/

   if( bitpix>0 && inputNull )
      fits_write_key( out, TINT, "BLANK", &nullval, 0, status );
   if( copykeys ) CopyKeys( in, out, hdutypeIn, colnum, status );
   if( history ) {
      sprintf( context, "TASK: " );
      c_gtaskn( context+6 );
      sprintf( context+21, " on %s", Trunc(infile,40) );
      fits_write_history( out, context, status );
      sprintf( context, " Image %sed from column(s) %s and row(s) %s.",
	       operation==STACK_OP ?  "STACK" : 
                          ( operation==SUM_OP ? "SUM" : "AVG" ),
               Trunc(cols,20), Trunc(rows,20) );
      fits_write_history( out, context, status );
   }
   fits_set_hdustruc( out, status );
   if( *status!=OK ) {
      sprintf( context, "...Trouble creating output file." );
      goto CleanUp3;
   }

   /*****************************************/
   /*   Zero-out the Scaling keywords...    */
   /*     deal with raw data directly.      */
   /*****************************************/

   if( hdutypeIn==IMAGE_HDU )
      fits_set_bscale(in, 1.0, 0.0, status);
   else
      fits_set_tscale(in, colnum, 1.0, 0.0, status);
   fits_set_bscale(out, 1.0, 0.0, status);

   /*****************************************/
   /*   Read in, process, and output data   */
   /*****************************************/

   if( hdutypeIn==IMAGE_HDU )
      for( i=0; i<naxis_in; i++ ) {
         fpixels[i] = startCol[i];
         lpixels[i] = endCol[i];
         inc[i]=1;
      }
   for( i=0, loc=1; i<numRowRanges; i++ ) {
      direction = (startRow[i]<=endRow[i] ? 1 : -1);
      for( row=startRow[i]; row != endRow[i]+direction;
           row+=direction, loc+=buflen ) {
         if( hdutypeIn==IMAGE_HDU ) {
            fpixels[naxis_in-1]=lpixels[naxis_in-1]=row;
            ReadImg( in, typecode, naxis_in, naxes_in, fpixels,
                     lpixels, inc, buffer, Nulls, &anynul, status );
         } else
            ReadCol( in, typecode, colnum, row, 1L, buflen,
                     buffer, Nulls, &anynul, status );

         if( operation==STACK_OP ) {

            if (!anynul) {  /* no null values */
	       WriteImg( out, typecode, loc, buflen, buffer,
                      0, nullval, status );
            } else {
	       WriteImg( out, typecode, loc, buflen, buffer,
                      Nulls, nullval, status );
            }
         } else {
            for( j=0; j<buflen; j++ )
               if( !NullsSum[j] )
                  if( Nulls[j] )
                     NullsSum[j] = 1;
                  else
                     bufferSum[j] += ( (double *)buffer )[j];
         }

         if( *status!=OK ) {
            sprintf( context,"...Trouble transfering data in row %ld.", row );
            goto CleanUp3;
         }
      }
   }
   if( operation!=STACK_OP ) {
      if( operation==AVG_OP )
         for( i=0; i<buflen; i++ ) 
            if( !NullsSum[i] ) bufferSum[i] /= numrows;
      WriteImg( out, typecode, 1L, buflen, bufferSum, NullsSum,
                nullval, status );
   }

   /***********************************************************/
   /*   Cleanup after ourselves... in reverse order to make   */
   /*        error processing easier                          */
   /***********************************************************/

CleanUp3:
   fits_close_file( out, status );

CleanUp2:
   if( Nulls ) free( Nulls );
   if( NullsSum ) free( NullsSum );
   if( buffer ) free( buffer );
   if( bufferSum ) free( bufferSum );

CleanUp1:
   fits_close_file( in, status );

Error:
   if( *status != OK ) {
      if( *status != NOT_OK ) Fcerrm(*status);
      c_fcerr(context);
   }

   return;
}

/******************************************************************************
* Function:
*     ReadImg
* 
* Description:
*     Read an arbitrary subset of a multidimensional image of an arbitrary
*     data type, flagging NULL values.
* 
* Author: 
*      Peter D. Wilson/Oct 1997
*      NASA/GSFC
*      Hughes STX
* 
* Modification history:
* 
* Usage:
*      int ReadImg ( fitsfile *fptr,   int   type,    int   naxis,
*                    long     *naxes,  long *fpixels, long *lpixels,
*                    long     *inc,    void *buffer,  char *nularray,
*                    int      *anynul, int  *status                   );
* 
* Primary Variables:
*    --- Inputs ---
*      fptr      :   Input FITS file pointer
*      type      :   FITS data type to be read into buffer
*      naxis     :   Number of dimensions of image
*      naxes     :   Size of each dimension of image
*      fpixels   :   First pixel of each dimension to be read
*      lpixels   :   Last pixel of each dimension to be read
*      inc       :   Stepsize for each dimension
*      buffer    :   Pointer to array to hold the data
*      nularray  :   Array holding the null flags
*      anynul    :   Flag indicating whether a null was read
*      status    :   FITSIO error code
*    --- Return ---
*      int       :   FITSIO error code
* 
******************************************************************************/
int ReadImg( fitsfile *fptr, int type, int naxis, long *naxes,
             long *fpixels, long *lpixels, long *inc, void *buffer,
             char *nularray, int *anynul, int *status )
{
   switch( type ) {
   case TBYTE:
      fits_read_subsetnull_byt( fptr, 1, naxis, naxes, fpixels,
                                lpixels, inc, (unsigned char *)buffer,
                                nularray, anynul, status );
      break;
   case TUSHORT:
      fits_read_subsetnull_usht( fptr, 1, naxis, naxes, fpixels,
                                 lpixels, inc, (unsigned short *)buffer,
                                 nularray, anynul, status );
      break;
   case TSHORT:
      fits_read_subsetnull_sht( fptr, 1, naxis, naxes, fpixels,
                                lpixels, inc, (short *)buffer, nularray,
                                anynul, status );
      break;
   case TULONG:
      fits_read_subsetnull_ulng( fptr, 1, naxis, naxes, fpixels,
                                 lpixels, inc, (unsigned long *)buffer,
                                 nularray, anynul, status );
      break;
   case TLONG:
      fits_read_subsetnull_lng( fptr, 1, naxis, naxes, fpixels,
                                lpixels, inc, (long *)buffer, nularray,
                                anynul, status );
      break;
   case TFLOAT:
      fits_read_subsetnull_flt( fptr, 1, naxis, naxes, fpixels,
                                lpixels, inc, (float *)buffer, nularray,
                                anynul, status );
      break;
   case TDOUBLE:
      fits_read_subsetnull_dbl( fptr, 1, naxis, naxes, fpixels,
                                lpixels, inc, (double *)buffer, nularray,
                                anynul, status );
      break;
   default:
      *status=NOT_OK;
      break;
   }
   return(*status);
}

/******************************************************************************
* Function:
*     ReadCol
* 
* Description:
*     Read one entry of a table (single value or vector column) of an
*     arbitrary data type, flagging NULL values.
* 
* Author: 
*      Peter D. Wilson/Oct 1997
*      NASA/GSFC
*      Hughes STX
* 
* Modification history:
* 
* Usage:
*      int ReadCol( fitsfile *fptr,   int   type,     int   col,
*                   long      row,    long  felem,    long  nelem,
*                   void     *buffer, char *nularray, int  *anynul,
*                   int      *status                                 );
* 
* Primary Variables:
*    --- Inputs ---
*      fptr      :   Input FITS file pointer
*      type      :   FITS data type to be read into buffer
*      col       :   Column number to read
*      row       :   Row number to read
*      felem     :   First element of vector column to be read
*      nelem     :   Number of elements to be read
*      buffer    :   Pointer to array to hold the data
*      nularray  :   Array holding the NULL flags
*      anynul    :   Flag indicating whether a NULL was read
*      status    :   FITSIO error code
*    --- Return ---
*      int       :   FITSIO error code
* 
******************************************************************************/
int ReadCol( fitsfile *fptr, int type, int col, long row,
                       long felem, long nelem, void *buffer,
                       char *nularray, int *anynul, int *status )
{
   switch( type ) {
   case TBYTE:
      fits_read_colnull_byt( fptr, col, row, felem, nelem, 
                             (unsigned char *)buffer, nularray,
                             anynul, status );
      break;
   case TUSHORT:
      fits_read_colnull_usht( fptr, col, row, felem, nelem, 
                             (unsigned short *)buffer, nularray,
                             anynul, status );
      break;
   case TSHORT:
      fits_read_colnull_sht( fptr, col, row, felem, nelem, 
                             (short *)buffer, nularray,
                             anynul, status );
      break;
   case TULONG:
      fits_read_colnull_ulng( fptr, col, row, felem, nelem, 
                              (unsigned long *)buffer, nularray,
                              anynul, status );
      break;
   case TLONG:
      fits_read_colnull_lng( fptr, col, row, felem, nelem, 
                             (long *)buffer, nularray,
                             anynul, status );
      break;
   case TFLOAT:
      fits_read_colnull_flt( fptr, col, row, felem, nelem, 
                             (float *)buffer, nularray,
                             anynul, status );
      break;
   case TDOUBLE:
      fits_read_colnull_dbl( fptr, col, row, felem, nelem, 
                             (double *)buffer, nularray,
                             anynul, status );
      break;
   default:
      *status=NOT_OK;
      break;
   }
   return(*status);
}

/******************************************************************************
* Function:
*     WriteImg
* 
* Description:
*     Write a row of data of an arbitrary data type to an image or primary
*     array, flagging NULL values.
* 
* Author: 
*      Peter D. Wilson/Oct 1997
*      NASA/GSFC
*      Hughes STX
* 
* Modification history:
* 
* Usage:
*      int WriteImg( fitsfile *fptr,    int   type,   long felem,
*                    long      nelem,   void *buffer, char *nularray,
*                    int       nullval, int  *status                  );
* 
* Primary Variables:
*    --- Inputs ---
*      fptr      :   Input FITS file pointer
*      type      :   FITS data type to be written out of buffer
*      felem     :   First element of image to be written (ie, 1-D location)
*      nelem     :   Number of elements to be written
*      buffer    :   Pointer to array holding the data
*      nularray  :   Array holding the NULL flags
*      nullval   :   Value to assign to integer NULL data
*      status    :   FITSIO error code
*    --- Internal ---
*      nul_xxx   :   NULL value properly converted to data type 'type'
*    --- Return ---
*      int       :   FITSIO error code
* 
******************************************************************************/
int WriteImg( fitsfile *fptr, int type, long felem, long nelem,
              void *buffer, char *nularray, int nullval, int *status )
{
   long elem;
   unsigned char nul_byt;
   unsigned short nul_usht;
   short nul_sht;
   unsigned long nul_ulng;
   long nul_lng;
   float nul_flt;
   double nul_dbl;

   switch( type ) {
   case TBYTE:
      nul_byt = (unsigned char)nullval;
      if (nularray) {
        for( elem=0;elem<nelem;elem++ )
         if( nularray[elem] ) ((unsigned char *)buffer)[elem] = nul_byt;
        fits_write_imgnull_byt( fptr, 1L, felem, nelem, 
                              (unsigned char *)buffer, nul_byt, status );
      } else {
        fits_write_img_byt( fptr, 1L, felem, nelem, 
                              (unsigned char *)buffer, status );
      }
      break;
   case TUSHORT:
      nul_usht = (unsigned short)nullval;
      if (nularray) {
        for( elem=0;elem<nelem;elem++ )
         if( nularray[elem] ) ((unsigned short *)buffer)[elem] = nul_usht;
        fits_write_imgnull_usht( fptr, 1L, felem, nelem, 
                              (unsigned short *)buffer, nul_usht, status );
      } else {
        fits_write_img_usht( fptr, 1L, felem, nelem, 
                              (unsigned short *)buffer, status );
      }
      break;
   case TSHORT:
      nul_sht = (short)nullval;
      if (nularray) {
        for( elem=0;elem<nelem;elem++ )
         if( nularray[elem] ) ((short *)buffer)[elem] = nul_sht;
        fits_write_imgnull_sht( fptr, 1L, felem, nelem, 
                              (short *)buffer, nul_sht, status );
       } else {
        fits_write_img_sht( fptr, 1L, felem, nelem, 
                              (short *)buffer, status );
      }
     break;
   case TULONG:
      nul_ulng = (unsigned long)nullval;
      if (nularray) {
        for( elem=0;elem<nelem;elem++ )
         if( nularray[elem] ) ((unsigned long *)buffer)[elem] = nul_ulng;
        fits_write_imgnull_ulng( fptr, 1L, felem, nelem, 
                              (unsigned long *)buffer, nul_ulng, status );
      } else {
        fits_write_img_ulng( fptr, 1L, felem, nelem, 
                              (unsigned long *)buffer, status );
      }
      break;
   case TLONG:
      nul_lng = (long)nullval;
      if (nularray) {
        for( elem=0;elem<nelem;elem++ )
         if( nularray[elem] ) ((long *)buffer)[elem] = nul_lng;
        fits_write_imgnull_lng( fptr, 1L, felem, nelem, 
                              (long *)buffer, nul_lng, status );
      } else {
        fits_write_img_lng( fptr, 1L, felem, nelem, 
                              (long *)buffer, status );
      }
      break;
   case TFLOAT:
      nul_flt = MAXFLOAT;
     if (nularray) {
       for( elem=0;elem<nelem;elem++ )
         if( nularray[elem] ) ((float *)buffer)[elem] = nul_flt;
        fits_write_imgnull_flt( fptr, 1L, felem, nelem, 
                              (float *)buffer, nul_flt, status );
      } else {
        fits_write_img_flt( fptr, 1L, felem, nelem, 
                              (float *)buffer, status );
      }
      break;
   case TDOUBLE:
      nul_dbl = MAXDOUBLE;
      if (nularray) {
        for( elem=0;elem<nelem;elem++ )
         if( nularray[elem] ) ((double *)buffer)[elem] = nul_dbl;
        fits_write_imgnull_dbl( fptr, 1L, felem, nelem, 
                              (double *)buffer, nul_dbl, status );
      } else {
        fits_write_img_dbl( fptr, 1L, felem, nelem, 
                              (double *)buffer, status );
      }
      break;
   default:
      *status=NOT_OK;
      break;
   }
   return(*status);
}

/******************************************************************************
* Function:
*      CopyKeys
* 
* Description:
*      Copy keywords from the input to output extension.
*
* Author: 
*      Peter D. Wilson/Oct 1997 (Based on copyhead by Lawrence E. Brown)
*      NASA/GSFC
*      Hughes STX
* 
* Modification history:
* 
* Usage:
*      int CopyKeys( fitsfile *in,     fitsfile *out,   int hdu
                     int       colnum, int      *status          );
* 
* Primary Variables:
*    --- Inputs ---
*      in, out   :   Input and Output FITS files
*      hdu       :   HDU type of input extension
*      colnum    :   Column number of table being extracted
*      status    :   FITSIO error code
*    --- Return ---
*      int       :   FITSIO error code
* 
******************************************************************************/
int CopyKeys( fitsfile *in, fitsfile *out, int hdu, int colnum, int *status )
{
   int i, j, copy, nkeys, nmore, kstatus[3];
   char record[CBUFSIZE], card[3][CBUFSIZE], key[10];
   char *ignore_keys[] = { 
                     "SIMPLE",   "BITPIX",   "NAXIS",   "EXTEND",
                     "XTENSION", "PCOUNT",   "GCOUNT",  "TFIELDS",
                     "TTYPE",    "TBCOL",    "TFORM",   "TUNIT",
                     "THEAP",    "TDIM",     "GROUPS",  "END",
                     "EXTNAME",  "CHECKSUM", "DATASUM", "BLANK",
                     "TSCAL",    "TZERO",    "TNULL",   "TDISP",
                     "CTYPE",    "CRPIX",    "CROTA",   "CRVAL",
                     "CDELT",    "TLMIN",    "TLMAX",   "OPTIC",
                     "TCRPX",    "TCRVL",    "TCDLT",   "TCTYP",
                     "TCROT",    "PLTSCL",   "" };

   /**************************************************/
   /*   Convert tabular keywords to image keywords   */
   /**************************************************/

   if( hdu==ASCII_TBL || hdu==BINARY_TBL ) {
      for( i=0; i<3; i++ ) kstatus[i]=0;
      sprintf( key, "TUNIT%d", colnum );
      fits_read_card( in, key, card[0], kstatus );
      strcpy( card[0], "BUNIT   " );
      sprintf( key, "TSCAL%d", colnum );
      fits_read_card( in, key, card[1], kstatus+1 );
      strcpy( card[1], "BSCALE  " );
      sprintf( key, "TZERO%d", colnum );
      fits_read_card( in, key, card[2], kstatus+2 );
      strcpy( card[2], "BZERO   " );
      for( i=0; i<3; i++ ) {
	 card[i][8] = '=';
	 if( !kstatus[i] ) fits_write_record( out, card[i], status );
      }
   }

   /***************************************************************/
   /*   Copy all the other keywords, other than those specified   */
   /***************************************************************/

   fits_get_hdrspace( in, &nkeys, &nmore, status );
   for( i=0; i<nkeys; i++ ) {
      fits_read_record( in, i+1, record, status );
      copy = TRUE;
      j = 0;
      do {
	 if( !strncmp( record, ignore_keys[j], strlen( ignore_keys[j] ) ) )
	    copy = FALSE;
      } while( ignore_keys[++j][0]!=0 && copy );
      if( !strncmp( record, "TCD", 3 ) ) {
	 for( j=6; j<CBUFSIZE && copy; j++ )
	    if( record[j]!=' ' ) copy = FALSE;
      }
      if( copy ) fits_write_record( out, record, status );
      if( *status!=OK ) return *status;
   }
   return( *status );
}

/******************************************************************************
* Function:
*      Trunc
* 
* Description:
*      Truncate a character string if longer than a given length,
*      placing ellipses at the end if too long.  Returns a pointer
*      to the string (can be used within another function call).
* 
* Author: 
*      Peter D. Wilson/Oct 1997
*      NASA/GSFC
*      Hughes STX
* 
* Modification history:
* 
* Usage:
*      char *Trunc( char *str, int len );
* 
* Primary Variables:
*    --- Inputs ---
*      str       :   Character string to be truncated
*      len       :   Truncation length for the string
*    --- Return ---
*      char *    :   Pointer to modified string
* 
******************************************************************************/
char *Trunc( char *str, int len )
{
   if( strlen(str) > len ) {
      str[len-3] = str[len-2] = str[len-1] = '.';
      str[len] = 0;
   }
   return( str );
}

/******************************************************************************
* Function:
*      fimgextract
* 
* Description:
*      "main" entry point of program.  Calls gImgExtract to get parameters
*      then ImgExtract to do the processing.
* 
* Author: 
*      Peter D. Wilson/Oct 1997
*      NASA/GSFC
*      Hughes STX
* 
* Modification history:
* 
* Usage:
*      void fimgextract( void )
* 
* Primary Variables:
*    --- Inputs ---
*      none
*    --- Internal ---
*      infile    :   Filename (including extension) of input file
*      outfile   :   Filename (including extension) of output file
*      cols      :   Name of table column or list of image bounds
*      rows      :   List of row ranges to be processed
*      operation :   Operation to perform on input data
*      nullval   :   Numerical value of NULLs in an integer output image
*      clobber   :   Overwrite existing files?
*      copykeys  :   Copy relevant keywords to output?
*      history   :   Put history keywords into output file?
*      status    :   FITSIO error code
*    --- Return ---
*      none
* 
******************************************************************************/
void fimgextract()
{
   char infile[CBUFSIZE], outfile[CBUFSIZE], cols[CBUFSIZE];
   char rows[CBUFSIZE];
   int operation;
   int nullval;
   int clobber, copykeys, history, status;

   c_ptaskn("fimgextract1.3");

   status = gImgExtract( infile, outfile, cols, rows, &operation,
			 &nullval, &clobber, &copykeys, &history);
   if( status==OK )
      ImgExtract( infile, outfile, cols, rows, operation,
		  nullval, clobber, copykeys, history, &status);

   return;
}

/****************************************************************
* The following code is needed to allow IRAF to call fimgextract.
* This extra subroutine layer is needed because of differences in
* linkers between vms and unix. 
****************************************************************/

#ifdef vms
#define F77CALL fimgextr
#endif
#ifdef unix
#define F77CALL fimgextr_
#endif

void F77CALL()
{
   void fimgextract();
   fimgextract();
}
