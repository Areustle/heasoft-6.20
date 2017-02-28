/*
 * xrgindefdbl.c
 */
#include "pil.h"
#include "fitsio.h"

int xrgindefdbl(char *parname, double defval, double *value) {
/*
 * Get double parameter
 * If user enters INDEF, substitute defval
 *
 *  I  parname (c) Parameter name
 *  I  defval  (d) Default value to use when INDEF is entered
 *  O  value   (d) Final value of parameter
 */
   int match, exact, query_mode, status;
   char buff[PIL_LINESIZE];

   status = PILGetAsString(parname, buff);
   if ( status == 0 ) {
      /* Empty entry corresponds to NULL */
      /* NOTE: Only relevant for hidden parameters */
      if ( strcmp(buff, "") == 0 ) {  
         *value = DOUBLENULLVALUE;
         return(status);
      }
      fits_compare_str("INDEF", buff, CASEINSEN, &match, &exact);
   } else {
      match = FALSE;
   }
   if ( match ) {
      *value = defval;
      status = 0;
   } else {
      query_mode = PILGetQueryMode();

      /* Suppress prompts temporarily so that you don't get a 
       * second prompt for the same parameter */
      PILOverrideQueryMode(PIL_QUERY_OVERRIDE);

      /* Get value as double */
      status = PILGetReal(parname, value);

      /* Restore original query mode */
      PILOverrideQueryMode(query_mode);
   }
   return(status);
}
