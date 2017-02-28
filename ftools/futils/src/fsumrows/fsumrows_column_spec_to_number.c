/******************************************************************************

File name: fsumrows_column_spec_to_number.c

Function name: fsumrows_column_spec_to_number

Description: Parse a list of column specifiers (templates), then translate it
             into a list of flags corresponding to the  matching column
             numbers.  The input string contains either a list of column
             specifiers or a file spec containeing the column specifiers.

Author/Date: toliver, 5/99

Modification History: see log at EOF

Notes:  The calling routine is responsible for ensuring that the size of the
        column nmaes do not exceed the size of the strings in the column name
        list, since no size information is passed to this function.

Usage:
       #include "fsumrows.h"
       build_selected_columns_list (file, columns, column_selected, max_cols
                                    &status);

Arguments:
       fitsfile *file -- FITS file containing the columns (assumed to be
                         open at the proper extension)
       char *columns -- string containing list of column specifiers or a
                        filespec containing the column specifiers
       int *column_selected -- array of flags denoting if a column number
                               was selected for processing
       int max_cols -- maximum number of column names to store in list
       int *status -- status return

Significant local variables:

        char *token_start -- pointer to the start of a column name
        char *token_stop -- pointer to the end of a column name

Other library functions called:

     cftoolslib:
          c_fcecho (char *message); -- echo message to stdout

******************************************************************************/

#include <cftools.h>

void fsumrows_column_spec_to_number (fitsfile *file,
                                     char *token,
                                     int *column_selected,
                                     int max_cols,
                                     int *status)
{

   char message[C_FCERR_MSG];

   int colnum;

   (void)fits_get_colnum (file, CASEINSEN, token, &colnum, status);

   if (COL_NOT_UNIQUE == *status)
   {
      if (colnum > max_cols)
      {
         /* the following is safe, as long as you know you aren't writing
            more than C_FCERR_MSG characters */
         sprintf (message, "WARNING: function column_spec_to_number - "
                           "column number %d exceeds size of\n"
                           "                                          "
                           "column_selected list", colnum);
         c_fcecho (message);
         *status = 0;
         return;
      }

      column_selected[colnum-1]= 1;

      while (COL_NOT_UNIQUE == *status)
      {
         if (colnum > max_cols)
         {
            /* the following is safe, as long as you know you aren't writing
               more than C_FCERR_MSG characters */
            sprintf (message, "WARNING: function column_spec_to_number - "
                              "column number %d exceeds size of\n"
                              "                                          "
                              "column_selected list", colnum);
            c_fcecho (message);
            *status = 0;
            return;
         }
         column_selected[colnum-1]= 1;
         (void)fits_get_colnum (file, CASEINSEN, token, &colnum, status);
      }
      *status = 0;

   }

   else if (OK == *status)
   {
      if (colnum > max_cols)
      {
         /* the following is safe, as long as you know you aren't writing
            more than C_FCERR_MSG characters */
         sprintf (message, "WARNING: function column_spec_to_number - "
                           "column number %d exceeds size of\n"
                           "                                          "
                           "column_selected list", colnum);
         c_fcecho (message);
         *status = 0;
         return;
      }
      else
      {
         column_selected[colnum-1]= 1;
      }

   }

   else
   {
      /* the following is safe, as long as you know you aren't writing more
         than C_FCERR_MSG characters */
      sprintf (message, "ERROR: function column_spec_to_number - error "
               "matching column number to column\n                     "
               "                   template '%s'", token);
      c_fcecho (message);
   }

   return;
}

/******************************************************************************

$Log: fsumrows_column_spec_to_number.c,v $
Revision 1.3  1999/10/27 13:22:40  peachey
Column numbers start with 1, C arrays start with 0, so put in offset
when selecting columns. This is consistent with the other functions.

Revision 1.2  1999/06/03 20:54:30  toliver
removed ^M characters that were hosing IRIX cc build

 * Revision 1.1  1999/05/19  19:31:40  toliver
 * initial version
 *

******************************************************************************/
