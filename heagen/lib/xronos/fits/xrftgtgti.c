/*
 * xrftgtgti.c
 */
#include "xronos.h"

int xrftgtgti(fitsfile *fptr, XRFile *file, int *status) {
/*
 * XRonos FiTs routine to Get Timing info on GTI
 * 
 * Gets overall GTI start and stop times.  Looks for TSTART and TSTOP
 * keywords first (or their double representations).  If successful, it
 * converts their values into days and adds on dtoffset.  If unsuccessful
 * and if a TIME column is present, it fetches the first and last
 * entries in the column, converts them into days, and adds on
 * dtzero and dtoffset 
 *
 *  I  fptr  - Pointer to open fits file (current HDU set to data)
 * I/O file  - Contains properties of file
 *  O  status - Error flag (0=OK)
 */
   int anynul, timeunit, timeunit_header, timeunit_column, ftstat;
   double gtizero, gtista, gtisto, ddum;
   char errmsg[MAXLEN_ERR];
   char keybuff[FLEN_VALUE];
   char *keyval;
   keyval = keybuff;

   /* -------- *
    * Columns
    * -------- */

   fits_get_num_rows(fptr, &file->ngtis, status);
   fits_get_colnum(fptr, CASEINSEN, "*START*", &file->col_gtista, status);
   fits_get_colnum(fptr, CASEINSEN, "*STOP*", &file->col_gtisto, status);
   if ( *status != 0 ) {
      sprintf(errmsg, "Error finding GTI START/STOP columns");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /* ----------- *
    * Time units
    * ----------- */

   xrftgtunit(fptr, file->timesys, file->col_gtista,
              &timeunit_header, &timeunit_column, status);
   if ( *status != 0 ) return(*status);


   /* ----------- *
    * Get dtzero  *
    * ----------- */

   ftstat = 0;
   xrftgdky(fptr, "TIMEZERI", "TIMEZERF", "TIMEZERO", &ddum, &ftstat);
   if ( ftstat == 0 ) {
      xrcnvtun(timeunit_header, ddum, TIMEUNIT_DAY, &gtizero, &ftstat);
   }
   if ( ftstat != 0 ) gtizero = 0.0;

   /* ------------------------ *
    * Get start and stop time  *
    * ------------------------ */

   /* First look in header for TSTART/TSTOP */
   ftstat = 0;
   timeunit = timeunit_header;
   xrftgdky(fptr, "TSTARTI", "TSTARTF", "TSTART", &gtista, &ftstat);
   xrftgdky(fptr, "TSTOPI",  "TSTOPF",  "TSTOP",  &gtisto, &ftstat);

   if ( ftstat == 0 && gtista < gtisto ) {

      xrcnvtun(timeunit, gtista, TIMEUNIT_DAY, &gtista, &ftstat);
      xrcnvtun(timeunit, gtisto, TIMEUNIT_DAY, &gtisto, &ftstat);
      gtista += file->dtoffset;
      gtisto += file->dtoffset;

   } else if ( file->col_gtista ) {

      /* If that fails, 
       * Use the first START and last STOP values.
       * No offset for this case. */
      ftstat = 0;
      fits_read_col_dbl(fptr, file->col_gtista, 1, 1, 1, DOUBLENULLVALUE, 
                        &gtista, &anynul, &ftstat);
      fits_read_col_dbl(fptr, file->col_gtisto, file->ngtis, 1, 1, 
                        DOUBLENULLVALUE, &gtisto, &anynul, &ftstat);
      timeunit = timeunit_column;
      xrcnvtun(timeunit, gtista, TIMEUNIT_DAY, &gtista, &ftstat);
      xrcnvtun(timeunit, gtisto, TIMEUNIT_DAY, &gtisto, &ftstat);
      gtista += (gtizero + file->dtoffset);
      gtisto += (gtizero + file->dtoffset);
      if ( XRONOS5_EMULATION ) {
         gtista = gtista - file->dtint/2./86400.;
         gtisto = gtisto + file->dtint/2./86400.;
      }

   }

   if ( ftstat != 0 ) {
      *status = -1046;
      sprintf(errmsg, "Failed to determine GTI start and stop time");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   if ( gtista > file->dtsta && !file->FR.set ) {
      headas_chat(VERBOSE, " Warning: start time: %17.10g\n", file->dtsta);
      headas_chat(VERBOSE, " Conflicts with GTI start time: %17.10g\n", gtista);
   }
   if ( gtisto < file->dtsto && !file->LR.set ) {
      headas_chat(VERBOSE, " Warning: stop time: %17.10g\n", file->dtsto);
      headas_chat(VERBOSE, " Conflicts with GTI stop time: %17.10g\n", gtisto);
   }

   /* Save values for later */
   file->gtiunit = timeunit_column;
   file->gtizero = gtizero;
   file->gtista = gtista;
   file->gtisto = gtisto;

   /* Not implemented - time shift options */

   return(*status);
}
