/*
 * xrdaytostr.c
 */
#include "xronos.h"

int xrdaytostr(double day, int decimals, char *outstr, int *status) {
/*
 * Convert day number fraction to character hh:mm:ss.sss representation.
 *
 * I  day      (d) Day number fraction
 * I  decimals (i) Decimal accuracy for secs in string
 * O  outstr   (c) Day in ddd hh:mm:ss.sss format
 * O  status   (i) Error flag (0=OK)
 */
   int iday, ihr, imin;
   double hr, min, sec;
   char *timestr;

   iday = (int) day;
   hr   = (day - iday) * 24.0;
   ihr  = (int) hr;
   min  = (hr - ihr)   * 60.0;
   imin = (int) min;
   sec  = (min - imin) * 60.0;
   
   fits_time2str(0, 0, 0, ihr, imin, sec, decimals, outstr, status);
   timestr = stralloc(outstr);
   sprintf(outstr, "%d %s", iday, timestr);
   free(timestr);
   return(*status);
}
