/*
 * xrftexpopnt.c
 */
#include "xronos.h"

int xrftexpopnt(XRFITSBuffer *ftbuff, XRExpoPoint *pnt, int *status) {
/*
 * Get a exposure point (e.g. GTI) from FITS file
 *
 * I/O ftbuff  - FITS data buffer
 *  O  pnt     - Output exposure point
 *  O  status  - Error flag (0=OK)
 */
   int anynul;
   double ddum;
   XRFile *file;
   fitsfile *fptr;

   file = ftbuff->file;
   fptr = ftbuff->fptr;

   /* Start out assuming valid point */
   pnt->valid = TRUE;

   /* If end of buffer return non-valid point */
   if ( ftbuff->end ) {
      pnt->valid = FALSE;
      return(*status);
   }

   if ( file->col_gtista ) {

      /* START Column */
      fits_read_col_dbl(fptr, file->col_gtista, ftbuff->irow, 1, 1, 
                        DOUBLENULLVALUE, &ddum, &anynul, status);
      xrcnvtun(file->gtiunit, ddum, TIMEUNIT_DAY, &ddum, status);
      pnt->timesta = ddum;

      /* STOP Column */
      fits_read_col_dbl(fptr, file->col_gtisto, ftbuff->irow, 1, 1, 
                        DOUBLENULLVALUE, &ddum, &anynul, status);
      xrcnvtun(file->gtiunit, ddum, TIMEUNIT_DAY, &ddum, status);
      pnt->duration = ddum - pnt->timesta;

      /* Convert to standard time system */
      pnt->timesta += (file->dtoffset + file->gtizero);

      pnt->fracexp = 1.0;

   } else {
      /* Exposure information */
      /* not implemented */
      pnt->valid = FALSE;
   }

   return(*status);
}
