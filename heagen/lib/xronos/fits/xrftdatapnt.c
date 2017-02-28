/*
 * xrftdatapnt.c
 */
#include "xronos.h"

int xrftdatapnt(XRFITSBuffer *ftbuff, XRDataPoint *pnt, int *status) {
/*
 * Get a data point from FITS file
 *
 * I/O ftbuff  - FITS data buffer
 *  O  pnt     - Output data point
 *  O  status  - Error flag (0=OK)
 */
   int anynul;
   double coltime, pnttime, cor;
   XRFile *file;
   fitsfile *fptr;

   file = ftbuff->file;
   fptr = ftbuff->fptr;

   /* Start out assuming valid non-null point */
   pnt->valid = TRUE;
   pnt->null = FALSE;

   /* If end of buffer return non-valid point */
   if ( ftbuff->end ) {
      pnt->valid = FALSE;
      return(*status);
   }

   /* TIME Column */
   if ( file->col_time ) {
      fits_read_col_dbl(fptr, file->col_time, ftbuff->irow, 1, 1, 
                        DOUBLENULLVALUE, &coltime, &anynul, status);
      if ( pnt->time == DOUBLENULLVALUE ) pnt->valid = FALSE;
      xrcnvtun(file->timeunit_column, coltime, TIMEUNIT_DAY, &pnttime,
               status);
      pnt->time = pnttime;
   } else {
      pnt->time = (ftbuff->irow - 1) * file->dtint / 86400.0;
   }
   pnt->time += (file->dtzero + file->dtoffset + 
                 file->dfoffset*file->dtint/86400.0);

   /* RATE Column */
   if ( file->col_rate ) {
      fits_read_col_dbl(fptr, file->col_rate, ftbuff->irow, 1, 1, 
                        DOUBLENULLVALUE, &pnt->y, &anynul, status);
      if ( pnt->y == DOUBLENULLVALUE ) pnt->null = TRUE;
   } else {
      pnt->y = 1.0;
   }
   /* Correct for deadtime */
   cor = 1.0/file->deadc;
   pnt->y = pnt->y*cor;

   /* ERROR Column */
   if ( file->col_err ) {
      fits_read_col_dbl(fptr, file->col_err, ftbuff->irow, 1, 1,
                        DOUBLENULLVALUE, &pnt->sy, &anynul, status);
      if ( pnt->sy == DOUBLENULLVALUE ) pnt->sy = 0.0;
   } else {
      pnt->sy = 0.0;
   }
   /* Correct for deadtime */
   cor = cor*file->deaderr;
   pnt->sy = pnt->sy*cor;

   /* TIMEDELT Column or constant */
   if ( file->col_timedel ) {
      fits_read_col_dbl(fptr, file->col_timedel, ftbuff->irow, 1, 1,
                        0.0, &coltime, &anynul, status);
      xrcnvtun(file->timeunit_column, coltime, TIMEUNIT_SEC, &pnttime,
               status);
      pnt->expo = pnttime;
   } else {
      if ( file->dtint <= 0.0 ) {
         pnt->expo = 0.0;
      } else {
         pnt->expo = file->dtint;
      }
   }

   /*
    * Obtain total corrected exposure
    * Use FRACEXP column if exists,
    * Otherwise use DEADC column and vignetting correction,
    * Failing that, apply DEADC keyword and vignetting correction
    */
   if ( file->col_fracexp ) {
      fits_read_col_dbl(fptr, file->col_fracexp, ftbuff->irow, 1, 1,
                        0.0, &pnt->expcor, &anynul, status);
   } else {
      if ( file->col_deadc ) {
         fits_read_col_dbl(fptr, file->col_deadc, ftbuff->irow, 1, 1,
                           0.0, &pnt->expcor, &anynul, status);
      } else {
         pnt->expcor = file->deadc;
      }
      pnt->expcor = pnt->expcor*file->vignet;
   }
   pnt->expcor = pnt->expcor*pnt->expo;

   /* Event point setting */
   pnt->event = FALSE;
   if ( file->type == EVENT_DATA ) pnt->event = TRUE;

   /* Null point settings */
   if ( pnt->null ) {
      pnt->y = 0.0;
      pnt->sy = 0.0;
      pnt->expcor = 0.0;
   }

   return(*status);
}
