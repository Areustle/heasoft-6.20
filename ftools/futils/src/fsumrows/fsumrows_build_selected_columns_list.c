/******************************************************************************

File name: fsumrows_build_selected_columns_list.c

Function name: fsumrows_build_selected_columns_list

Description: Parse a list of column specifiers (templates), then translate it
             into a list of flags corresponding to the matching column
             numbers.  The input string contains either a list of column
             specifiers or a file spec containeing the column specifiers.

Author/Date: toliver, April, 1999

Modification History:

$Log: fsumrows_build_selected_columns_list.c,v $
Revision 1.2  1999/06/03 20:54:44  toliver
removed ^M characters that were hosing IRIX cc build

 * Revision 1.1  1999/05/19  19:31:07  toliver
 * initial revision
 *

Functions referenced:
       int fsumrows_column_spec_to_number : Translate column specifier into
                                            column number(s), using them to
                                            update the column_selected list

Libraries referenced:
       cftoolslib

Usage:
       #include "fsumrows.h"
       fsumrows_build_selected_columns_list (file, columns, column_selected,
                                             max_cols, &status);

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

******************************************************************************/

#include <string.h>
#include <ctype.h>

#include "fsumrows.h"

/*
** Because fgets is used to read strings from file (and it requires a parameter
**    stating the maximum number of characters to read in) a large maximum
**    size was chose as to try and minimize the chance of truncation.  If the
**    size of the strings in the column name string list were available then
**    that would be the best value to use in the fgets function call.
*/

#define MAX_LINE 1024

/*****************************************************************************/

void fsumrows_build_selected_columns_list (fitsfile *file,
                                           char *columns,
                                           int *column_selected,
                                           int max_cols,
                                           int *status)

{

   int colnum;
   int i;

   char *token_start;
   char *token_stop;
   char token[MAX_LINE];

   FILE *fp;

/*
** Initializations.
*/

  token_start = columns;

/*
** Find the first non-whitespace character in the input buffer, return if
**    everything is whitespace.
*/
   while (isspace (*token_start) && (*token_start != '\0'))
   {
      token_start++;
   }
   if ('\0' == *token_start)
   {
      return;
   }

/*
** If the first non-whitespace character in the input buffer is '@', then the
**    column name are stored in a file, the filespec following the '@'
**    character
*/
   if ('@' == *token_start)
   {

/*
**    If the file does not open, display a message on stderr and fall through
**       to the end of the function.
*/
      token_start++;
      fp = fopen (token_start, "r");
      if (NULL == fp)
      {
         c_fcerr ("function build_selected_columns_list : file containing\n"
                  "                                                     "
                  "column list not found");
      }
      else
      {

/*
**       Read all non-whitespace lines from the file and translate the column
**          specifiers into the column selected list, exiting if a status
**          error is detected.  Because fgets is used to read the file, a
**          large maximum line size is specified to try to avoid truncating
**          the lines in the file.  The newline character must also be removed
**          from the end of the string (fgets leaves it there), and any
**          trailing whitespace is also removed.
*/
         while (fgets (token, MAX_LINE, fp) != NULL)
         {
            token[strlen (token) - 1] = '\0';
            (void)c_trimstr (token, strlen (token));
            if (strlen (token) > 0)
            {
               fsumrows_column_spec_to_number (file, token, column_selected,
                                               max_cols, status);
               if (OK != *status)
               {
                  return;
               }
            }
         }
         fclose (fp);
      }

   } /* column names are contained in a file */

/*
** If the column specifiers are not if a file, then parse the input string to
**    extract them.
*/
   else
   {

/*
**    Look for the column names until the end of the last column name in the
**       input string was found, the end of the input string was reached while
**       looking for the start of a new column name, or an error status is
**       detected.
*/
      do
      {

/*
**       Find the next column name delimiter.  Column names are delimited by
**          spaces, commas, quotation marks, or brackets.
*/
         token_stop = strpbrk (token_start, " ,\"[");

/*
**       If a delimiter was not found, then the last column name has been
**          found.  Translate the column specifier into the column selected
**          list, exiting if a status error is detected.
*/
         if (NULL == token_stop)
	 {
            strcpy (token, token_start);
            fsumrows_column_spec_to_number (file, token, column_selected,
                                            max_cols, status);
            if (OK != *status)
            {
               return;
            }
         }

/*
**       A column name delimiter was found, process according to type.
*/
         else
         {
            switch ((int)(*token_stop))
            {
/*
**             Spaces and commas are treated the same.
*/
               case ' ':
               case ',':
/*
**                If this delimiter immediately follows a delimiter, then
**                   simply advance to the next character in the input string
**                   and search for the next delimiter.
*/
                  if (token_start == token_stop)
		  {
                     token_start++;
                  }
/*
**                Otherwise, this delimiter marks the end of a column
**                   specifier.  Translate the column specifier into the
**                   column selected list, exiting if a status error is
**                   detected.  Advance the column name start pointer to the
**                   next character in the input string.
*/
                  else
		  {
                     strncpy (token, token_start, (token_stop - token_start));
                     token[token_stop - token_start] = '\0';
                     fsumrows_column_spec_to_number (file, token,
                                                     column_selected, max_cols,
                                                     status);
                     if (OK != *status)
                     {
                        return;
                     }
                     token_start = token_stop + 1;
                  }
                  break;
/*
**             For quotation marks, everything between the quotes is part of
**                the column specifier, but not the quotes themselves.
*/
               case '"':

/*
**                Search for the closing quotation mark.
*/
                  token_start++;
                  token_stop = strchr (token_start, '"');

/*
**                If this quote immediately follows a quote (""), then simply
**                   advance to the next character in the input string and
**                   search for the next delimiter.
*/
                  if (token_start == token_stop)
		  {
                     token_start++;
                  }

/*
**                If a closing quote was found, translate the column specifier
**                   into the column selected list, exiting if a status error
**                   is detected.  Advance the column name start pointer to
**                   the next character in the input string.
*/
                  else if (NULL != token_stop)
		  {
                     strncpy (token, token_start, (token_stop - token_start));
                     token[token_stop - token_start + 1] = '\0';
                     fsumrows_column_spec_to_number (file, token,
                                                     column_selected, max_cols,
                                                     status);
                     if (OK != *status)
                     {
                        return;
                     }
                     token_start = token_stop + 1;
                  }

/*
**                If no closing quote was found, then consider all characters
**                   from the opening quote to the end of the input string to
**                   be one column specifier.  Translate the column specifier
**                   into the column selected list, exiting if a status error
**                   is detected.  NULL the column name end pointer.
*/
                  else
		  {
                     strcpy (token, token_start);
                     fsumrows_column_spec_to_number (file, token,
                                                     column_selected, max_cols,
                                                     status);
                     if (OK != *status)
                     {
                        return;
                     }
                     token_stop = NULL;
                  }

                  break;

/*
**             For square brackets, everything between the brackets is part of
**                the column specifier, including the brackets themselves.
*/
               case '[':

/*
**                Search for the closing square bracket.
*/
                  token_stop = strchr (token_start, ']');

/*
**                If the closing bracket immediately follows the opening
**                   bracket ([]), then simply advance to the next character
**                   in the input string and search for the next delimiter.
*/
                  if (token_start == token_stop)
		  {
                     token_start++;
                  }

/*
**                If a closing bracket was found, translate the column
**                   specifier into the column selected list, exiting if a
**                   status error is detected.  Advance the column name start
**                   pointer to the next character in the input string;
**                   otherwise, set the list overflow flag.
*/
                  else if (NULL != token_stop)
		  {
                     strncpy (token, token_start,
                              (token_stop - token_start + 1));
                     token[token_stop - token_start + 2] = '\0';
                     fsumrows_column_spec_to_number (file, token,
                                                     column_selected, max_cols,
                                                     status);
                     if (OK != *status)
                     {
                        return;
                     }
                     token_start = token_stop + 1;
                  }

/*
**                If no closing bracket was found, then consider all characters
**                   from the opening quote to the end of the input string to
**                   be one column specifier.  Translate the column specifier
**                   into the column selected list, exiting if a status error
**                   is detected.  NULL the column name end pointer.
*/
                  else
		  {
                     strcpy (token, token_start);
                     strcat (token, "]");
                     fsumrows_column_spec_to_number (file, token,
                                                     column_selected, max_cols,
                                                     status);
                     if (OK != *status)
                     {
                        return;
                     }
                     token_stop = NULL;
                  }

                  break;

            } /* switch on column name delimiter type */

         } /* column delimiter was found */

/*
**    Continue until the end of the last column name in the input string was
**       found or the end of the input string was reached while looking for the
**       start of a new column name.
*/
      }
      while ((NULL != token_stop) && ('\0' != *token_start));

   } /* column names are contained in string */

   return;

}
