/*
 * xrftbufclean.c
 */
#include "xronos.h"

int xrftbufclean(XRFITSBuffer *ftbuff, int *status) {
/*
 * Cleanup FITS data buffer 
 *
 * I/O ftbuff  - FITS data buffer
 *  O  status  - Error flag (0=OK)
 */
   int ftstat;

   ftstat = 0;
   if ( ftbuff->fptr ) fits_close_file(ftbuff->fptr, &ftstat);

   return(*status);
}
