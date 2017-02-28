/*******************************************************************************

Filename:
     fsumrows.c

Function:
     fsumrows

Description:
     "main" entry point of program.  Sum or average a subset of a (vector)
     column from a table extracted from an ASCII or  binary table extension.
     A set of columns of a table (single valued or vector/array) serves as
     the input.  An arbitrary list of rows of the selected columns can be
     selected to be SUMmed or AVeraGed.

Author/Date: 
     toliver / April, 1999 (borrowed heavily from the tool fimgextract,
                            by Peter B. Wilson)

Modification History:

$Log: fsumrows.c,v $
Revision 1.3  1999/10/27 14:55:07  peachey
Use new boolean parameter "sametype" (default false) to allow user
to write the output with the same data type as the input.

Revision 1.2  1999/05/19 19:29:58  toliver
added mutilple column support


Functions referenced:
       int fsumrows_get_params     : Reads .par file
       int fsumrows_process_rows   : Extracts/processes selected rows

Libraries referenced:
       cftoolslib

Usage:
     int fsumrows (void)

Primary Variables:

     --- Inputs ---
     none

     --- Internal ---
     infile    : Filename (including extension) of input file
     outfile   : Filename (including extension) of output file
     cols      : List of input table column specifiers
     rows      : List of input table row ranges to be processed
     operation : Operation to perform on input data
     nullval   : Numerical value of integer NULLS in output table
     clobber   : Overwrite existing files?
     sametype  : Convert output back to input type after operation?
     status    : FITSIO error code

     --- Return ---
     none

*******************************************************************************/

#include "fsumrows.h"

int fsumrows (void)
{

   char infile[CBUFSIZE],
        outfile[CBUFSIZE],
        cols[CBUFSIZE],
        rows[CBUFSIZE];

   int operation,
       nullval,
       clobber,
       sametype,
       status;

   c_ptaskn ("fsumrows1.0");

   status = fsumrows_get_params (infile, outfile, cols, rows,
                                 &operation, &nullval, &clobber,
                                 &sametype);
   if (OK == status)
   {
      status = fsumrows_process_rows (infile, outfile, cols, rows,
                                      operation, nullval, clobber,
                                      sametype);
   }

   return (status);
}
