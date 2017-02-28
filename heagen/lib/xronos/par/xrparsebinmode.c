/*
 * xrparsebinmode.c
 */
#include <string.h>
#include "fitsio.h"
#include "xronos.h"

int xrparsebinmode(char *binmodestr, int *binmode, int *status) {
/*
 * Parse binmode string
 *
 * I/O binmodestr - Binmode string 
 *  O  binmode    - BINMODE_ constant
 *  O  status     - Error flag (0 = OK)
 *
 *  Side effect: binmodestr is converted to uppercase
 */

   /* Convert string to uppercase */
   fits_uppercase(binmodestr);

   if ( strcmp(binmodestr,"LINEAR") == 0 || 
        strcmp(binmodestr,"1") == 0 ) {

      *binmode = BINMODE_LINEAR;

   } else if ( strcmp(binmodestr,"LOG") == 0 || 
               strcmp(binmodestr,"2") == 0 ) {

      *binmode = BINMODE_LOG;

   } else if ( strcmp(binmodestr,"COUNTS") == 0 || 
               strcmp(binmodestr,"3") == 0 ) {

      *binmode = BINMODE_COUNTS;

   } else if ( strcmp(binmodestr,"BAYES") == 0 || 
               strcmp(binmodestr,"4") == 0 ) {

      *binmode = BINMODE_BAYES;

   } else {

      *binmode = 0;
      *status = -1;

   }
   return(*status);
}
