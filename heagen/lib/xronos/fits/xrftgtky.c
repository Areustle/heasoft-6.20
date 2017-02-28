/*
 * xrftgtky.c
 */
#include <string.h>
#include "xronos.h"

int xrftgtky(fitsfile *fptr, XRFile *file, int *status) {
/*
 * XRonos FiTs routine to Get Timing KeYwords.
 * The returned parameters contain all information necessary to reconstruct 
 * the full TIME for any row in the file (but not inside packets).
 * 
 * This routine does many things.
 * 
 * 1) Looks for the TIMESYS keyword.  If it exists and is equal to 
 *    JD, MJD or TJD, sets time system.
 *    Otherwise the routine looks for the MJDREF keyword (or the
 *    MJDREFI MJDREFF pair). 
 * 
 * 2) Looks up timeunit (see xrftgtunit)
 * 
 * 3) Looks for a TIME column, and if present, looks for its TUNITnnn
 *    keyword.  If not successful, the routine assumes that times in the
 *    TIME column are written in the same units as those in the header.
 * 
 * 4) If neither TIMEUNIT nor TUNITxxx for TIME column can be found,
 *    the routine issues a warning.  If the calling routine knows
 *    what the units are ahead of time.
 * 
 * 5) Looks for a TIMEZERO keyword, or its TIMEZERI TIMEZERF double.
 *    If successful, the value is stored in dtzero, converted into days,
 *    and returned.  All TIME column values should be offset by dtzero.
 * 
 * 6) Looks for the integration time by first searching for the TIMEDEL
 *    keyword and then the DELTIME keyword.  If successful, the value
 *    is converted to seconds and returned in dtint.  If unsuccessful, and
 *    if the file contains binned data (RATE_DATA), a warning is reported.
 * 
 * 7) Sets an internal offset value dtoffset that can be used to convert
 *    all times to TJD.  If timesys unknown this is not possible, and the
 *    routine returns dtoffset = 0.d0.  If however timesys is unknown and 
 *    a TIME column is present, and if the first value in the TIME column is
 *    negative, dtoffset is set so that TIME column values start from
 *    zero when dtoffset is added to them (and dtzero added on as well, 
 *    of course).
 * 
 * 8) Gets start and stop times.  It first looks for TSTART and TSTOP
 *    keywords (or their double representations).  If successful, it
 *    converts their values into days and adds on dtoffset.  If unsuccessful
 *    and if a TIME column is present, it fetches the first and last
 *    entries in the column, converts them into days, and adds on
 *    dtzero and dtoffset (see 5 and 7 above).  The start and stop
 *    are returned in dtsta and dtsto, respectively.
 * 
 * 9) Looks at FR and LR options for user-supplied first and last rows
 *    to read from the file.  If either one is not zero, the TIME value
 *    for respective rows is used to replace dtsta or dtsto, as appropriate.
 *    The input value of npts is adjusted to the numberof rows that will
 *    actually be read from the file.
 * 
 * 10) Finally, for the case of packet data, the routine looks for the
 *     TIMEPIXR keyword.  Its value should be between 0 and 1, and
 *     is stored in dfoffset (not to be confused with dtoffset).  The
 *     keyword indicates where in the packet bin the time is measured:
 *     0 for the beginning, 1 for end and 0.5 for half way through.  Xronos
 *     assumes halfway through by default, so this routine subtracts
 *     from 0.5 the dfoffset before returning it. The return value is 
 *     positive for any dfoffset less than 0.5 and negative if dfoffset
 *     is greater than 0.5.
 *
 *  I  fptr  - Pointer to open fits file (current HDU set to data)
 * I/O file  - Contains properties of file
 *  O  status - Error flag (0=OK)
 */
   int anynul, timeunit, frow, lrow, ftstat;
   double ddum, mjdref;
   char errmsg[MAXLEN_ERR];
   char keybuff[FLEN_VALUE];
   char *keyval;
   keyval = keybuff;

 /*--------------------------------*
  * Time system and reference time *
  *--------------------------------*/

 /*
  * Get TIMESYS / MJDREF.
  */
   ftstat = 0;
   if ( fits_read_key_str(fptr, "TIMESYS", keyval, NULL, &ftstat) )
      keyval[0] = '\0';
   if ( strcmp(keyval, "MJD") == 0 ) {
      file->timesys = TIMESYS_MJD;
   } else if ( strcmp(keyval, "JD") == 0 ) {
      file->timesys = TIMESYS_JD;
   } else if ( strcmp(keyval, "TJD") == 0 ) {
      file->timesys = TIMESYS_TJD;
   } else {
      /*
       * Ignore any other values of TIMESYS, and look for the MJDREF.
       */
      ftstat = 0;
      xrftgdky(fptr, "MJDREFI", "MJDREFF", "MJDREF", &mjdref, &ftstat);
      if ( ftstat == 0 ) {
         file->timesys = TIMESYS_REF;
      } else {
         file->timesys = TIMESYS_NONE;
      }
   }

   /* ----------- *
    * Time units
    * ----------- */

   xrftgtunit(fptr, file->timesys, file->col_time,
              &file->timeunit_header, &file->timeunit_column, status);
   if ( *status != 0 ) return(*status);

   /* ----------- *
    * Get dtzero  *
    * ----------- */

   ftstat = 0;
   xrftgdky(fptr, "TIMEZERI", "TIMEZERF", "TIMEZERO", &ddum, &ftstat);
   if ( ftstat == 0 ) {
      xrcnvtun(file->timeunit_header, ddum, TIMEUNIT_DAY, &file->dtzero,
               &ftstat);
   }
   if ( ftstat != 0 ) file->dtzero = 0.0;

   /* --------------------- *
    * Get integration time  *
    * --------------------- */

   ftstat = 0;
   fits_read_key_dbl(fptr, "TIMEDEL", &ddum, NULL, &ftstat);
   if ( ftstat != 0 ) {
      ftstat = 0;
      fits_read_key_dbl(fptr, "DELTIME", &ddum, NULL, &ftstat);
   }

   if ( ftstat == 0 ) {
      /* Success:  convert dtint to seconds. */
      xrcnvtun(file->timeunit_header, ddum, 
               TIMEUNIT_SEC, &file->dtint, &ftstat);
   }

   if ( ftstat != 0 ) {

      /* Find out if this is an events file.  If not, and there is no TIMEDEL
       * column, and if TIMEDEL was not found, report error (calling routine
       * decides what to do with it). */

      if ( file->type == EVENT_DATA ) {
         file->dtint = -1.0;
      } else if ( file->col_timedel ) {
         file->dtint = 0.0;
      } else {
         /* no TIMEDEL column or keyword and not EVENT LIST */
         *status = -1;
         sprintf(errmsg, "Could not decode integration time");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
   }

   /* ------------------- *
    * Set internal offset *
    * ------------------- */

   switch (file->timesys) { /* Convert to TJD */
      case TIMESYS_MJD:
         file->dtoffset = -40000.0;
         break;
      case TIMESYS_JD:
         file->dtoffset = -2440000.5;
         break;
      case TIMESYS_TJD:
         file->dtoffset = 0.;
         break;
      case TIMESYS_REF:
         if ( mjdref > 40000.0 ) {
            file->dtoffset = mjdref - 40000.0;
         } else {
            file->dtoffset = 0.;
         }
         break;
      default:
         /* Unsupported time system.  Take the times as they appear in the
          * file unless they are negative.  If a negative time value is
          * found, set dtoffset to offset times to start from zero.  This
          * must assume that all files in the series have the same time 
          * representation, thus this part only gets executed (dtoffset only 
          * gets set) on the first file in the series. */

         ddum = 0.0;
         if ( file->col_time ) {
            *status = 0;
            fits_read_col_dbl(fptr, file->col_time, 1, 1, 1, DOUBLENULLVALUE, 
                              &ddum, &anynul, status);
            xrcnvtun(file->timeunit_column, ddum, TIMEUNIT_DAY, &ddum,
                     status);
            if ( *status != 0 ) return(*status);
            ddum += file->dtzero;
         } else if ( file->dtzero < 0.0 ) {
            ddum = file->dtzero;
         }
         if ( ddum < 0.0 ) file->dtoffset = -ddum;
      /* end default case */
   }
   /* Avoid adding big offset automatically - OF option */
   if ( file->OF.set && file->timesys == TIMESYS_REF &&
        mjdref > 40000.0 ) file->dtoffset = 0.0;
      
   /* ------------------------ *
    * Get start and stop time  *
    * ------------------------ */

   /* First look in header for TSTART/TSTOP */
   ftstat = 0;
   timeunit = file->timeunit_header;
   xrftgdky(fptr, "TSTARTI", "TSTARTF", "TSTART", &file->dtsta, &ftstat);
   xrftgdky(fptr, "TSTOPI",  "TSTOPF",  "TSTOP",  &file->dtsto, &ftstat);

   if ( ftstat == 0 && file->dtsta < file->dtsto ) {

      xrcnvtun(timeunit, file->dtsta, TIMEUNIT_DAY, &file->dtsta, &ftstat);
      xrcnvtun(timeunit, file->dtsto, TIMEUNIT_DAY, &file->dtsto, &ftstat);
      file->dtsta += file->dtoffset;
      file->dtsto += file->dtoffset;

   } else if ( file->col_time ) {

      /* If that fails, 
       * Search the TIME column for first and last values.
       * No offset for this case. */
      ftstat = 0;
      fits_read_col_dbl(fptr, file->col_time, 1, 1, 1, DOUBLENULLVALUE, 
                        &file->dtsta, &anynul, &ftstat);
      fits_read_col_dbl(fptr, file->col_time, file->nrows, 1, 1, 
                        DOUBLENULLVALUE, &file->dtsto, &anynul, &ftstat);
      timeunit = file->timeunit_column;
      xrcnvtun(timeunit, file->dtsta, TIMEUNIT_DAY, &file->dtsta, &ftstat);
      xrcnvtun(timeunit, file->dtsto, TIMEUNIT_DAY, &file->dtsto, &ftstat);
      file->dtsta += (file->dtzero + file->dtoffset);
      file->dtsto += (file->dtzero + file->dtoffset);

      /* Account for bin bounds */
      if ( file->type != EVENT_DATA ) {
         file->dtsta -= (file->dtint/86400.0/2.0);
         file->dtsto += (file->dtint/86400.0/2.0);
      }
   }

   if ( ftstat != 0 ) {
      *status = -1046;
      sprintf(errmsg, "Failed to determine start and stop time");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /* Apply options for first and last rows to read from rate tables. */

   /* FR - first row */
   frow = 1;
   if ( file->FR.set && file->FR.value > 0 ) {
      frow = file->FR.value;
      if ( file->col_time ) {
         ftstat = 0;
         fits_read_col_dbl(fptr, file->col_time, frow, 1, 1, DOUBLENULLVALUE, 
                           &file->dtsta, &anynul, &ftstat);
         xrcnvtun(file->timeunit_column, file->dtsta, TIMEUNIT_DAY, 
                  &file->dtsta, &ftstat);
         file->dtsta += (file->dtzero + file->dtoffset);
         if ( file->type != EVENT_DATA ) {
            file->dtsta -= (file->dtint/86400.0/2.0);
         }
      } else {
         file->dtsta += (file->dtint*(frow - 1)/86400.0);
      }
   }
   file->frow = frow;

   /* LR - last row */
   lrow = file->nrows;
   if ( file->LR.set && file->LR.value > 0 ) {
      lrow = file->LR.value;
      if ( file->col_time ) {
         ftstat = 0;
         fits_read_col_dbl(fptr, file->col_time, lrow, 1, 1, DOUBLENULLVALUE, 
                           &file->dtsto, &anynul, &ftstat);
         xrcnvtun(file->timeunit_column, file->dtsto, TIMEUNIT_DAY, 
                  &file->dtsto, &ftstat);
         file->dtsto += (file->dtzero + file->dtoffset);
         if ( file->type != EVENT_DATA ) {
            file->dtsto += (file->dtint/86400.0/2.0);
         }
      } else {
         file->dtsto = file->dtsta + (file->dtint*(lrow - frow)/86400.0);
      }
   }
   file->lrow = lrow;

   /* 
    * Look for keyword to set dfoffset (for time centering on packet data)
    */
   ftstat = 0;
   fits_read_key_dbl(fptr, "TIMEPIXR", &ddum, NULL, &ftstat);
   if ( ftstat == 0 ) {
      file->dfoffset = 0.5 - ddum;
   } else {
      file->dfoffset = 0.0;
   }

   return(*status);
}
