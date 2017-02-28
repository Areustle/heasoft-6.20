/*
 * xrftgtunit.c
 */
#include "xronos.h"

int xrftgtunit(fitsfile *fptr, int timesys, int column,
               int *timeunit_header, int *timeunit_column, int *status) {
/*
 * XRonos FiTs routine to Get TimeUNIT
 * 
 * Looks for the TIMEUNIT keyword and tries to find what its value is.
 * If it is not there the routine will look for time unit of column
 * If the keyword is found but cannot be
 * interpreted, a fatal error is issued from xrdectun.
 * Finally, if no unit keyword is found, but timesys is TIMESYS_REF, 
 * the routine assumes time is written in days.
 * 
 *  I  fptr             -  Pointer to open fits file set to appropriate HDU
 *  I  timesys         (i) Time system
 *  I  column          (i) Column index
 *  O  timeunit_header (i) Time unit found in header
 *  O  timeunit_column (i) Time unit associated with column
 *  O  status          (i) Error flag (0=OK)
 */
   int idum, ftstat;
   char errmsg[MAXLEN_ERR];
   char keybuff[FLEN_VALUE];
   char *keyval;
   keyval = keybuff;

/* ---------------------------------------------------------------------*
 * Time units: establish what units are used in the file any way we can.
 * If units can not be deciphered, exit with fatal error status
 * ---------------------------------------------------------------------*/


   /* Units in header. */
   ftstat = 0;
   if ( fits_read_key_str(fptr, "TIMEUNIT", keyval, NULL, &ftstat) )
      keyval[0] = '\0';
   *timeunit_header = xrdectun(keyval);

   /* Units on TIME column */
   ftstat = 0;
   if ( fits_read_keys_str(fptr, "TUNIT", column, 1, &keyval, 
        &idum, &ftstat) ) keyval[0] = '\0';
   *timeunit_column = xrdectun(keyval);

   /*
    * If no time unit keywords found at all.
    * Last attempt is if timesys is JD, times are in days.
    */
   if ( *timeunit_header == TIMEUNIT_NONE &&
        *timeunit_column == TIMEUNIT_NONE ) {

      if ( timesys != TIMESYS_NONE && timesys != TIMESYS_REF ) {

         *timeunit_header = TIMEUNIT_DAY;
         *timeunit_column = TIMEUNIT_DAY;

      } else {

         *status = -1048;
         sprintf(errmsg, "No TIMEUNIT keyword and no TUNITnnn for column %d", 
                                                                     column);
         HD_ERROR_THROW(errmsg, *status);
         return(*status);

      }

   }

   /* 
    * If header time units were not found, use column units
    */
   if ( *timeunit_header == TIMEUNIT_NONE ) {
        *timeunit_header = *timeunit_column;
         headas_chat(VERBOSE, "WARNING: %s -- %s\n",
                      "Units not found for header timing keywords"
                      "Assuming column value");
   }
   /* 
    * If column time units were not found, use header units.
    */
   if ( *timeunit_column == TIMEUNIT_NONE ) {
        *timeunit_column = *timeunit_header;
         headas_chat(VERBOSE, "WARNING: %s -- %s\n",
                      "Units not found for column"
                      "Assuming TIMEUNIT keyword value");
   }

   return(*status);
}
