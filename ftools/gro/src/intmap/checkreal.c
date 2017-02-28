#include "intmap.h"


/*******************************************************************************
 * Validate real numbers
 * Get the real number string from the window widget, convert it to real. If an
 * error is found in conversion or if it is < than minimum passed, then write
 * a message on the window and on the screen and return the error code.
 ******************************************************************************/
int checkreal(float min, char msg[], char *strval, float *realval)
{
   int	num;
   char	rest[80], footer[80];
	
   /* Clear the window footer message */
   strcpy(rest, " ");

   /* Try to convert the string to real */
   num = sscanf(strval, "%f%s", realval, rest);

   /* Check for errors and notify the user if found */
   if ((num != 1) || (strcmp(rest," ") != 0) || (*realval < min)) {
      strcpy(footer, "Invalid value for ");
      strcat(footer, msg);
      fprintf(stderr,"\007");
      return 1;
   }
   else return 0;
}

