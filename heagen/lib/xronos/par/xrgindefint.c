/*
 * xrgindefint.c
 */
#include "pil.h"
#include "fitsio.h"

int xrgindefint(char *parname, int defval, int *value) {
/*
 * Get int parameter
 * If user enters INDEF, substitute defval
 *
 *  I  parname (c) Parameter name
 *  I  defval  (d) Default value to use when INDEF is entered
 *  O  value   (d) Final value of parameter
 */
   int match, exact, query_mode, status;
   char buff[PIL_LINESIZE];

   if ( PILGetAsString(parname, buff) ) buff[0] = '\0';
   fits_compare_str("INDEF", buff, CASEINSEN, &match, &exact);
   if ( match ) {
      *value = defval;
      status = 0;
   } else {
      query_mode = PILGetQueryMode();

      /* Suppress prompts temporarily so that you don't get a 
       * second prompt for the same parameter */
      PILOverrideQueryMode(PIL_QUERY_OVERRIDE);

      /* Get value as int */
      status = PILGetInt(parname, value);

      /* Restore original query mode */
      PILOverrideQueryMode(query_mode);
   }
   return(status);
}
