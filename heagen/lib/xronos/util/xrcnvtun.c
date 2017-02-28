/*
 * xrcnvtun.c
 */
#include "xronos.h"

int xrcnvtun(int inunit, double intime, int outunit, double *outtime,
             int *status) {
/*
 * Convert time from input unit to output unit
 *
 * At the moment, conversion into either seconds or days are the 
 * only two cases implemented.
 */
   char errmsg[MAXLEN_ERR];

   /* No conversion needed */
   if ( inunit == outunit ) {
      *outtime = intime;
      return(*status);
   }

   if ( outunit == TIMEUNIT_DAY ) {

      /* Convert to days */

      switch (inunit) {
         case TIMEUNIT_SEC: 
            *outtime = intime/8.64e4;
            break;
         case TIMEUNIT_MILSEC:
            *outtime = intime/8.64e7;
            break;
         case TIMEUNIT_MICSEC:
            *outtime = intime/8.64e10;
            break;
         case TIMEUNIT_HOUR:
            *outtime = intime/24.0;
            break;
         case TIMEUNIT_YEAR:
            *outtime = intime*365.25;  /* ! */
            break;
         default:
            *status = -1;
            sprintf(errmsg, "Conversion failed, bad input unit\n");
            HD_ERROR_THROW(errmsg, *status);
      }

   } else if ( outunit == TIMEUNIT_SEC ) {

     /* Convert to seconds */

      switch (inunit) {
         case TIMEUNIT_DAY: 
            *outtime = intime*8.64e4;
            break;
         case TIMEUNIT_MILSEC:
            *outtime = intime*1e-3;
            break;
         case TIMEUNIT_MICSEC:
            *outtime = intime*1e-6;
            break;
         case TIMEUNIT_HOUR:
            *outtime = intime*3600.0;
            break;
         case TIMEUNIT_YEAR:
            *outtime = intime*3.15576e7;  /* ! */
            break;
         default:
            *status = -1;
            sprintf(errmsg, "Conversion failed, bad input unit\n");
            HD_ERROR_THROW(errmsg, *status);
      }

   } else {
      *status = -1;
      sprintf(errmsg, "Conversion failed, bad output unit\n");
      HD_ERROR_THROW(errmsg, *status);
   }

   return(*status);
}
