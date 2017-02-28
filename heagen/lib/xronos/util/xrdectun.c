/*
 * xrdectun.c
 */
#include "xronos.h"

int xrdectun(char *unitstr) {
/*
 * DECode a keyword string for Time UNits.
 *
 * Strings are interpreted as follows:
 *
 * First, if the substring 'SEC' appears anywhere -> seconds
 * Second, if the substring 'DAY' appears anywhere -> days
 *    This is to cover cases like TIMEUNIT = 'MET DAY'.
 *
 * The routine then checks the following cases:
 *
 * 'MS' or 'MSEC' => milliseconds  (overwriting flag if 'SEC' found prior)
 * 'MMS' or 'US' => microseconds
 * ... + several other cases; see below.
 *
 *   I  unitstr  (c) Character string with physical time units
 *
 *  Returns unitflag based on input string
 */
   int match, exact, unitflag;

   unitflag = TIMEUNIT_NONE;

   fits_compare_str("SEC", unitstr, CASEINSEN, &match, &exact);
   if ( match ) unitflag = TIMEUNIT_SEC;
   fits_compare_str("DAY", unitstr, CASEINSEN, &match, &exact);
   if ( match ) unitflag = TIMEUNIT_DAY;
   fits_compare_str("MS*", unitstr, CASEINSEN, &match, &exact);
   if ( match ) unitflag = TIMEUNIT_MILSEC;
   fits_compare_str("MMS*", unitstr, CASEINSEN, &match, &exact);
   if ( match ) unitflag = TIMEUNIT_MICSEC;
   fits_compare_str("US", unitstr, CASEINSEN, &match, &exact);
   if ( match ) unitflag = TIMEUNIT_MICSEC;
   fits_compare_str("MILLI", unitstr, CASEINSEN, &match, &exact);
   if ( match ) unitflag = TIMEUNIT_MILSEC;
   fits_compare_str("MICRO", unitstr, CASEINSEN, &match, &exact);
   if ( match ) unitflag = TIMEUNIT_MICSEC;
   
/*
 * If no such string is found, pick off the first character.
 */

   if ( unitflag == TIMEUNIT_NONE ) {
      fits_compare_str("S*", unitstr, CASEINSEN, &match, &exact);
      if ( match ) unitflag = TIMEUNIT_SEC;
      fits_compare_str("D*", unitstr, CASEINSEN, &match, &exact);
      if ( match ) unitflag = TIMEUNIT_DAY;
      fits_compare_str("H*", unitstr, CASEINSEN, &match, &exact);
      if ( match ) unitflag = TIMEUNIT_HOUR;
      fits_compare_str("Y*", unitstr, CASEINSEN, &match, &exact);
      if ( match ) unitflag = TIMEUNIT_YEAR;
   }
   return(unitflag);
}
