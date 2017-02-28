/******************************************************************************

Filename:
     fsumrows_process_rows.c

Function:
     fsumrows_process_rows

Description:
     Extract rows of data from a table column and either sum or average them,
     writing the results to a table cell in the output file.

Author/Date:
     toliver / April, 1999

Modification history:

$Log: fsumrows_process_rows.c,v $
Revision 1.6  2003/04/17 17:53:11  irby
Under Cygwin, use limits.h instead of values.h.

Revision 1.5  2002/12/23 17:13:17  irby
Define MAXHDU (=100000) since it was taken out of fitsio.h in cfitsio v2.430.

Revision 1.4  2002/02/07 17:20:33  irby
Under Darwin, use limits.h instead of values.h.

Revision 1.3  2001/10/15 19:18:59  zpan

fix uninitialized variable's warning

Revision 1.2  1999/10/27 14:55:07  peachey
Use new boolean parameter "sametype" (default false) to allow user
to write the output with the same data type as the input.

Revision 1.1  1999/05/19 19:32:31  toliver
initial version


Functions referenced:
       int fsumrows_get_params     : Reads .par file
       int fsumrows_process_rows   : Extracts/processes selected rows

Libraries referenced:
       cfitsio
       ftoolslib

Usage:
     int fsumrows_process_rows (char *infile, char *outfile, char *cols,
                                char *rows, int operation, int nullval,
                                int clobber);

Primary Variables:

     --- Inputs ---
     infile    : Filename (including extension) of input file
     outfile   : Filename (including extension) of output file
     cols      : List of input table column specifiers
     rows      : List of input table row ranges to be processed
     operation : Operation to perfrom on input data (SUM, AVG, or STACK)
     nullval   : Numerical value of integer NULLS in output table
     clobber   : Overwrite existing files?
     sametype  : Convert output back to input type after operation?

     --- Internal ---
     in, out   : Input and Output FITS files
     inExt     : FITS HDU extension to read from
     hdutypeIn : HDU type of input extension/primary array (0=Image)
     naxis_in  : Dimension of input data
     naxes_in  : Size of each dimension of input data
     naxis_out : Dimension of output data
     startRow  : Array of starting row numbers for each range in 'rows'
     endRow    : Array of ending row numbers for each range in 'rows'
     nrows     : Number of rows in input file
     numrows   : Number of rows to be processed (and perhaps written out)
     colnum    : Column number of the Binary or ASCII table to be extracted
     startCol  : Array of starting column numbers for each dimension
                    of input image
     endCol    : Array of ending column numbers for each dimension
                    of input image
     buffer    : Buffer holding current row of input data
     bufferSum : Buffer holding running sum of input data
     Nulls     : Array indicating which data in buffer are undefined
     NullsSum  : Array indicating which data in bufferSum are undefined
     anynul    : Flag indicating most recent read encountered a NULL
     inputNull : Flag indicating whether input data may have NULLs
     typecode  : FITS data type being processed within buffer and bufferSum
     buflen    : Number of data to be read or written at a time
                    (i.e., size of buffer and bufferSum, not a byte count)
     loc       : Location within output HDU to write next row of data
     context   : Error message string

     --- Return ---
     status    : FITSIO error code

*******************************************************************************/

#include <ftools.h>
#if (defined __APPLE__) || (defined __CYGWIN__)
#include <limits.h>
#else
#include <values.h>
#endif

#include "fsumrows.h"

#define MAXHDU 100000

int fsumrows_process_rows (char *infile,
                           char *outfile,
                           char *cols,
                           char *rows,
                           int operation,
                           int nullval,
                           int clobber,
                           int sametype)

{

   fitsfile *in,
            *out;

   int status = OK,
       i,
       j,
       inExt,
       nkeys,
       nmore,
       ncols,
       tfields,
       outExt,
       hdutypeIn,
       hdutypeOut,
       anynul,
       simple,
       naxis_in,
       naxis_out,
       naxis,
       typecode,
       startRow[MAXRANGES],
       endRow[MAXRANGES],
       numRowRanges,
       direction,
       colnum,
       startCol[MAXRANGES],
       endCol[MAXRANGES],
       column_selected[MAXDIM],
       tmpNull,
       inputNull=0,
       BufLen_1 = BUFSIZE,    /* Required for C calls to Fc* */
       BufLen_2 = BUFSIZE,    /* Required for C calls to Fc* */ 
       MaxElem_2 = MAXRANGES; /* Required for C calls to Fc* */

   long naxes_in[MAXRANGES],
        naxes_out[MAXRANGES],
        loc,
        repeat,
        width,
        naxes,
        naxis1,
        row,
        nrows,
        numrows,
        rowlen,
        tbcol,
        pcount;

   void *buffer=0;

   double *bufferSum=0;

   char context[CBUFSIZE],
        *ttype[MAXDIM],
        *tform[MAXDIM],
        *tunit[MAXDIM],
        extname[CBUFSIZE],
        record[CBUFSIZE],
        *Nulls=0,
        *NullsSum=0,
        keyword[CBUFSIZE],
        value[CBUFSIZE];

   size_t buflen;

   naxis = 0;
   naxes = 0;
   nmore = 0;

/*
** Open and move to input HDU.
*/

   if (fits_open_file (&in, infile, READONLY, &status))
   {
      sprintf (context, "...Error opening %s", infile);
      goto Error;
   }

   fits_get_hdu_num (in, &inExt);
   if (fits_get_hdu_type (in, &hdutypeIn, &status))
   {
      sprintf( context, "...Couldn't obtain hdu type in input file" );
      goto CleanUp1;
   }

/*
** Primary array open, check if anything there.
*/
   if (1 == inExt)
   {
      if (fits_read_key (in, TINT, "NAXIS", &naxis_in, 0, &status))
      {
         sprintf (context, "...Unable to read NAXIS keyword in primary header");
         goto CleanUp1;
      }

      if (0 == naxis_in)
      {
         if (fits_movrel_hdu (in, 1, &hdutypeIn, &status))
         {
	    sprintf( context,
		     "...Couldn't move to first extension in input file");
	    goto CleanUp1;
	 }
      }
   }

/*
** Only table extensions are supported.
*/

   if ((ASCII_TBL != hdutypeIn) && (BINARY_TBL != hdutypeIn))
   {
      sprintf (context, "Unsupported extension type: %d", hdutypeIn);
      status = NOT_OK;
      goto CleanUp1;
  }

/*
** From the input file extension, get the column names and range of rows.
*/
   for (i = 0; i < MAXDIM; i++)
   {
      ttype[i] = (char *)malloc (CBUFSIZE);
      tform[i] = (char *)malloc (CBUFSIZE);
      tunit[i] = (char *)malloc (CBUFSIZE);
      column_selected[i] = 0;
   }

   if (ASCII_TBL == hdutypeIn)
   {
      (void)fits_read_atblhdr (in, MAXDIM, &rowlen, &nrows, &tfields,
                               ttype, &tbcol, tform, tunit, extname, &status);
   }
   else
   {
      (void)fits_read_btblhdr (in, MAXDIM, &nrows, &tfields, ttype, tform,
                               tunit, extname, &pcount, &status);
   }

/*
** Prepare the column selected list if all columns are to be processed.
*/
   if ((' ' == cols[0]) || ('-' == cols[0]))
   {
      for (i = 0; i < tfields; i++)
      {
         column_selected[i] = 1;
      }
   }

/*
** Otherwise parse the column specifier list to determine which columns were
**    selected.
*/
   else
   {
      fsumrows_build_selected_columns_list (in, cols, column_selected,
                                            tfields, &status);
      if (OK != status)
      {
         strcpy (context, "...Column template not found in table");
         goto CleanUp1;
      }
   }

/*
** Open/create output file, create primary array if necessary.
*/

   fits_parse_extnum (outfile, &outExt, &status);
   fits_parse_rootname (outfile, context, &status);
   strcpy (outfile, context);

   if (outExt == 1)
   {
      strcpy (context, "Can't write table into primary array");
      status = NOT_OK;
      goto CleanUp2;
   }

   fits_open_file (&out, outfile, READWRITE, &status);
   if (FILE_NOT_OPENED == status)
   {
      status = OK;
      fits_clear_errmsg ();
      fits_create_file (&out, outfile, &status);
      fits_write_imghdr (out, 16L, naxis, &naxes, &status);
      if (OK != status)
      {
         sprintf (context, "...Could not create primary array in output file");
         goto CleanUp3;
      }
   }
   else if (OK != status)
   {
      sprintf (context, "...Trouble opening output file %s", outfile);
      goto CleanUp3;
   }
   else if (clobber)
   {
      fits_delete_file (out, &status);
      fits_create_file (&out, outfile, &status);
      fits_write_imghdr (out, 16L, naxis, &naxes, &status);
      if (OK != status)
      {
         sprintf (context, "...Could not create primary array in output file");
         goto CleanUp3;
      }
   }
   else
   {
      outExt = MAXHDU;
      fits_movabs_hdu (out, (outExt - 1), &hdutypeOut, &status);
      if (END_OF_FILE == status)
      {
         status = OK;
         fits_clear_errmsg ();
      }
      else if (status)
      {
	 strcpy (context, "...Unable to move to end of output file");
	 goto CleanUp3;
      }
   }

/*
** Create empty HDU for output table.
*/

   fits_create_hdu (out, &status);
   if (OK != status)
   {
      sprintf (context, "...Could not create empty HDU in output file");
      goto CleanUp3;
   }

/*
** Copy the input file header then write additional keywords to header.
*/

   fits_get_hdrspace (in, &nkeys, &nmore, &status);
   for (i = 0; i < nkeys; i++)
   {
      fits_read_record (in, i + 1, record, &status);
      fits_write_record (out, record, &status);
      if (OK != status)
      {
         sprintf (context, "...Error copying header from input file");
         goto CleanUp3;
      }
   }

   if (inputNull)
   {
      fits_write_key (out, TINT, "BLANK", &nullval, 0, &status);
   }
   sprintf (context, "TASK: ");
   c_gtaskn (context + 6);
   sprintf ((context + 21), " on %s", infile);
   fits_write_history (out, context, &status);
   sprintf (context, "Table %sed from column(s) %s and row(s) %s",
            (operation == SUM_OP ? "SUM" : "AVG"), cols, rows);
   fits_write_history (out, context, &status);
   if (OK != status)
   {
      sprintf (context, "...Trouble adding header keywords to output file");
      goto CleanUp3;
   }

/*
** Set the NAXIS2 keyword in the output file to zero.
*/
   fits_modify_key_lng (out, "NAXIS2", 0L, "&", &status);
   if (OK != status)
   {
      sprintf (context, "...Error setting NAXIS2 = 0");
      goto CleanUp3;
   }

/*
** Read in HDU parameters and determine dimensions of input data column.
*/
   fits_read_key (in, TLONG, "NAXIS1", &naxis1, 0, &status);
   fits_read_key (in, TLONG, "NAXIS2", &nrows, 0, &status);
   if (status)
   {
      strcpy (context, "...Error reading HDU NAXIS parameters");
      goto CleanUp3;
   }

/*
** Add the row to hold the processed column values.
*/
   (void)fits_insert_rows (out, 0L, 1L, &status);
   if (OK != status)
   {
      sprintf (context, "...Error adding processed column value row");
      goto CleanUp3;
   }

/*
** Loop for all possible columns, process only those selected.
*/

   for (colnum = 1; colnum <= tfields; colnum++)
   {
      if (column_selected[colnum - 1])
      {

/*
**       Determine dimensions of input data column.
*/

         fits_get_coltype (in, colnum, &typecode, &repeat, &width, &status);
         fits_read_tdim (in, colnum, MAXRANGES, &naxis_in, naxes_in, &status);
         if (status)
         {
            sprintf (context, "...Error reading information for column "
                     "number %d", colnum);
            goto CleanUp1;
         }

         for (i = 0; i < naxis_in; i++)
         { 
            naxes_out[i] = naxes_in[i];
         }
         naxis_out = naxis_in;
         naxis_in++;
         naxes_in[naxis_in - 1] = nrows;

/*
**       Process all numeric column types except complex
*/
         if ((TBYTE == typecode) || (TSHORT == typecode) ||
             (TUSHORT == typecode) || (TINT == typecode) ||
             (TUINT == typecode) || (TLONG == typecode) ||
             (TULONG == typecode) || (TFLOAT == typecode) ||
             (TDOUBLE == typecode))
         {

/*
**          Check whether NULLs are present - if yes, set nullval.
*/

            tmpNull = nullval;
            sprintf (context, "TNULL%d", colnum);
            if (ASCII_TBL == hdutypeIn)
            {
               fits_read_card (in, context, context, &status);
            }
            else
            {
               fits_read_key (in, TINT, context, &tmpNull, 0, &status);
            }

            if (status)
            {
               status = OK;
               inputNull = FALSE;
            }
            else
            {
               inputNull = TRUE;
               nullval = tmpNull;
            }

/*
**          Setup data buffers for input and processing of data -- do it all in
**             double format to prevent overflow.  CFITSIO handles all data
**             conversion from and to the FITS file
*/

            for (buflen = 1, i = 0; i < naxis_out; i++)
            {
               buflen *= naxes_out[i];
            }
            buffer = (void *)malloc (sizeof (double) * buflen);
            Nulls = (char *)malloc (sizeof (char) * buflen);
            if ((0 == buffer) || (0 == Nulls))
            {
               strcpy (context, "Couldn't allocate memory for input buffers");
               status = NOT_OK;
               goto CleanUp2;
            }
            bufferSum = (double *)malloc (sizeof (double) * buflen);
            NullsSum = (char *)malloc (sizeof (char) * buflen);
            if ((0 == bufferSum) || (0 == NullsSum))
            {
               strcpy(context, "Couldn't allocate memory for summed input "
                      "buffers");
               status = NOT_OK;
               goto CleanUp2;
            }
            for (i = 0; i < buflen; i++)
            {
               NullsSum[i] = 0;
               bufferSum[i] = 0.0;
            }

/*
**          Decode row ranges to be read.
*/

            Fcgrgs (rows, nrows, &numRowRanges, startRow, endRow);
            for (numrows = 0,i = 0; i < numRowRanges; i++ )
            {
               if (startRow[i] < 1)
               {
                  startRow[i] = 1;
               }
               if (endRow[i] < 1)
               {
                  endRow[i] = 1;
               }
               j = endRow[i] - startRow[i];
               if (j > 0)
               {
                  numrows += j + 1;
               }
               else
               {
                  numrows += 1 - j;
               }
            }

/*
**          If the current column type is integer change the type of the
**             corresponding output column to float.  Update the NAXIS1 keyword
**             appropriately
**          Only do this if sametype is false, indicating that output
**             should be left in the floating point format
*/
            if (!sametype &&((TBYTE == typecode) || (TSHORT == typecode) ||
                (TUSHORT == typecode) || (TINT == typecode) ||
                (TUINT == typecode) || (TLONG == typecode) ||
                (TULONG == typecode)))
            {
               sprintf (keyword, "TFORM%-d", colnum);
               if (BINARY_TBL == hdutypeIn)
               {
                  sprintf (value, "%-dE", repeat);
                  fits_modify_key_str (out, keyword, value,
                                       "data format of field: 4-byte REAL",
                                       &status);
                  switch (typecode)
	          {
                     case TBYTE:
                        naxis1 += (3 * buflen);
                        break;
                     case TSHORT:
                     case TUSHORT:
                     case TINT:
                     case TUINT:
                        naxis1 += (2 * buflen);
                        break;
                  }
               }
               else
               {
                  fits_modify_key_str (out, keyword, "F17.6",
                                       "format of field", &status);
                  naxis1 += (17 - width);
               }

               fits_modify_key_lng (out, "NAXIS1", naxis1, "&", &status);
               if (OK != status)
               {
                  sprintf (context, "...Error changing integer column "
                                    "to float");
                  goto CleanUp3;
               }
            }

/*
**          Rescan the current header.
*/
            fits_set_hdustruc (out, &status);
            if (OK != status)
            {
               sprintf (context, "...Error rescanning current header");
               goto CleanUp3;
            }

/*
**          Read in, process, and output data.
*/

            for (i = 0, loc = 1; i < numRowRanges; i++)
            {
               direction = (startRow[i] <= endRow[i] ? 1 : -1);
               for (row = startRow[i];
                    row != (endRow[i] + direction);
                    row += direction, loc += buflen)
               {
                  fits_read_colnull_dbl (in, colnum, row, 1L, buflen, 
                                         (double *)buffer, Nulls,
                                         &anynul, &status);

                  for (j = 0; j < buflen; j++)
                  {
                     if (!NullsSum[j])
                     {
                        if (Nulls[j])
	                {
                           NullsSum[j] = 1;
                        }
                        else
                        {
                           bufferSum[j] += ((double *)buffer)[j];
                        }
                     }
                  }

                  if (OK != status)
                  {
                     sprintf (context, "...Trouble transfering data in "
                                       "row %ld", row);
                     goto CleanUp3;
                  }
               }
            }

            if (AVG_OP == operation)
            {
               for (i = 0; i < buflen; i++)
               { 
                  if (!NullsSum[i])
                  {
                     bufferSum[i] /= numrows;
                  }
               }
            }

/*
**          Write result to output column
*/

            fsumrows_wrt_col_result (out, TDOUBLE, colnum, buflen, bufferSum,
                                     NullsSum, nullval, &status);

            if (OK != status)
            {
               sprintf (context, "...Error writing result to output column");
               goto CleanUp3;
            }

/*
**          Free up dynamic arrays for next column.
*/
            if (Nulls)
            {
               free (Nulls);
               Nulls = 0;
            }
            if (NullsSum)
            {
               free (NullsSum);
               NullsSum = 0;
            }
            if (buffer)
            {
               free (buffer);
               buffer = 0;
            }
            if (bufferSum)
            {
               free (bufferSum);
               bufferSum = 0;
            }

         }

/*
**       Non-numeric columns that are selected generate an information
**          message.
*/
         else
         {
            sprintf (context, "...Datatype %3d for column number %d not "
                     "supported,\n                 column will not appear "
                     "in output table", typecode, colnum);
            c_fcerr (context);
            column_selected[colnum - 1] = 0;
         }

      } /* if (column_selected[i]) */

   } /* for all columns */

/*
** Delete unneeded columns from output files.
*/
   for (i = tfields; i > 0; i--)
   {
      if (!column_selected[i - 1])
      {
         (void)fits_delete_col (out, i, &status);
         if (OK != status)
         {
            sprintf (context, "...Error deleting column number %d", i);
            goto CleanUp3;
         }
      }
   }

/*
** Cleanup... in reverse order to make error processing easier.
*/
CleanUp3:
   fits_close_file (out, &status);

CleanUp2:
   if (Nulls)
   {
      free (Nulls);
   }
   if (NullsSum)
   {
      free (NullsSum);
   }
   if (buffer)
   {
      free (buffer);
   }
   if (bufferSum)
   {
      free (bufferSum);
   }

CleanUp1:
   fits_close_file (in, &status);
   for (i = 0; i < MAXDIM; i++)
   {
      free (ttype[i]);
      free (tform[i]);
      free (tunit[i]);
   }

Error:
   if (OK != status)
   {
      if (NOT_OK != status)
      {
         Fcerrm (status);
      }
      c_fcerr (context);
   }

   return (status);

}
