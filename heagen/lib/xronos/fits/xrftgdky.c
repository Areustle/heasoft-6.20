/*
 * xrftgdky.c
 */
#include "fitsio.h"

int xrftgdky(fitsfile *fptr, char *keynami, char *keynamf, char *keynams,
             double *dval, int *status) {
/*
 * XRonos FiTs routine to Get a Double KeYword value.
 * 
 * This general-purpose routine could actually be a fitsio routine.
 * All it does is to allow the possibilty that a double keyword value
 * may be represented in a file as two keywords: one integer and one
 * double precision.  
 * 
 * First, the routine looks for the integer-double pair given by
 * keynami and keynamf, respectively.  If it finds those keywords,
 * it returns their sum in dval.  If the pair are not found, it looks
 * for the single keyword given by keynams and returns its value in dval.
 * 
 *  I  fptr          = Pointer to fitsfile
 *  I  keynami  (c)  = Name of the integer part of the keyword
 *  I  keynamf  (c)  = Name of the fractional part of the keyword
 *  I  keynams  (c)  = Name of the single keyword to use if double is not there
 *  O  dval     (d)  = Returned double precision value [dble(i) + f]
 *  O  status   (i)  = 0 if succesful.
 */
   double intpart, decpart;
/*
 * Search for paired keyword first.
 */
   *status = 0;
   fits_read_key_dbl(fptr, keynami, &intpart, NULL, status);
   if ( *status == 0 ) {
      fits_read_key_dbl(fptr, keynamf, &decpart, NULL, status);
      if ( *status == 0 ) {
         *dval = intpart + decpart;
         return(*status);
      }
   }
/*
 * Search for single keyword.
 */
   *status = 0;
   fits_read_key_dbl(fptr, keynams, dval, NULL, status);
   return(*status);
}
