/*
 * xrftbufgetpnt.c
 */
#include "xronos.h"

int xrftbufgetpnt(XRFITSBuffer *ftbuff, void *pnt, int *status) {
/*
 * Get a point from FITS data buffer 
 *
 * I/O ftbuff  - FITS data buffer
 *  O  pnt     - Output data point
 *  O  status  - Error flag (0=OK)
 */
   bool needfile;
   int lrow, extnum, ftstat, hdutype;
   char errmsg[MAXLEN_ERR];

   /* If past last row, close file and move on */
   if ( ftbuff->file ) {
      lrow = ftbuff->file->lrow;
      if ( ftbuff->type == GTI_DATA ) lrow = ftbuff->file->ngtis;
      if ( ftbuff->irow > lrow ) {
         ftstat = 0;
         fits_close_file(ftbuff->fptr, &ftstat);
         ftbuff->fptr = NULL;
         ftbuff->file = NULL;
      }
   }

   /* Check if need to open new file */
   needfile = FALSE;
   if ( !ftbuff->file ) needfile = TRUE;

   /* If needed, increment to next file */
   if ( needfile ) {

      /* Check for end of data */
      if ( ftbuff->ifile >= ftbuff->nfiles ) {
         ftbuff->end = TRUE;
         needfile = FALSE;
      } else {
         ftbuff->file = ftbuff->files[ftbuff->ifile];
         ftbuff->ifile++;
         extnum = ftbuff->file->ext;
         if ( ftbuff->type == GTI_DATA ) extnum = ftbuff->file->gtiext;
     

         /* If no exposure extension, return */
         if ( extnum == 0 ) {
            ftbuff->end = TRUE;
            needfile = FALSE;
         }
      }
   }

   if ( needfile ) {

      /* Open file */
      fits_open_file(&ftbuff->fptr, ftbuff->file->name, READONLY, status);
      if ( *status != 0 ) {
         sprintf(errmsg, "Error opening file: %s", ftbuff->file->name);
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }

      /* Move to appropriate extension */
      fits_movabs_hdu(ftbuff->fptr, extnum+1, &hdutype, status);
      if ( *status != 0 ) {
         sprintf(errmsg, "Error moving to ext %d of file: %s", 
            ftbuff->file->ext, ftbuff->file->name);
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }

      /* Initialize row */
      ftbuff->irow = ftbuff->file->frow;
      if ( ftbuff->type == GTI_DATA ) ftbuff->irow = 1;
   }

   /* Read specific type of point */
   switch ( ftbuff->type ) {
      case EVENT_DATA: 
      case RATE_DATA:
         xrftdatapnt(ftbuff, (XRDataPoint *) pnt, status);
         break;
      case GTI_DATA:
         xrftexpopnt(ftbuff, (XRExpoPoint *) pnt, status);
         break;
      default:
         sprintf(errmsg, "Failed to read point of undefined type");
         HD_ERROR_THROW(errmsg, *status);
         *status = -1;
         return(*status);
   }

   /* Setup for next read */
   ftbuff->irow++;

   return(*status);
}
