/*
 * xrftgtcor.c
 */
#include "xronos.h"

int xrftgtcor(fitsfile *fptr, XRFile *file, int *status) {
/*
 * XRonos FiTs routine to Get Timing CORrection keywords 
 *
 *  I  fptr   - Pointer to open fits file (current HDU set to data)
 * I/O file   - Contains properties of file
 *  O  status - Error flag (0=OK)
 */
   int ftstat;

   /* Initialize */
   file->clockapp = FALSE;
   file->vignapp = FALSE;
   file->vignet = 1.0;
   file->deadapp = FALSE;
   file->deadc = 1.0;
   file->deaderr = 1.0;
   file->backapp = FALSE;
   file->backv = 1.0;

   if ( file->type == EVENT_DATA ) return(*status);

   /* Clock Correction */
   ftstat = 0;
   fits_read_key_log(fptr, "CLOCKAPP", &file->clockapp, NULL, &ftstat);

   /* Vignetting Correction */
   ftstat = 0;
   fits_read_key_log(fptr, "VIGNAPP", &file->vignapp, NULL, &ftstat);

   ftstat = 0;
   fits_read_key_dbl(fptr, "VIGNET", &file->vignet, NULL, &ftstat);
   if ( file->vignapp ) file->vignet = 1.0;

   /*  Always use <=1.0 convention */
   if ( file->vignet > 1.0 ) file->vignet = 1.0/file->vignet;

   /* Deadtime Correction */
   ftstat = 0;
   fits_read_key_log(fptr, "DEADAPP", &file->deadapp, NULL, &ftstat);

   ftstat = 0;
   fits_read_key_dbl(fptr, "DEADC", &file->deadc, NULL, &ftstat);
   if ( file->deadapp ) file->deadc = 1.0;

   /*  Always use <=1.0 convention */
   if ( file->deadc > 1.0 ) file->deadc = 1.0/file->deadc;

   /* Deadtime Correction Error for missions with sampling corrections */
   ftstat = 0;
   fits_read_key_dbl(fptr, "DEADERR", &file->deaderr, NULL, &ftstat);

   /* Background Correction */
   ftstat = 0;
   fits_read_key_log(fptr, "BACKAPP", &file->backapp, NULL, &ftstat);

   ftstat = 0;
   fits_read_key_dbl(fptr, "BACKV", &file->backv, NULL, &ftstat);
   if ( file->backapp ) file->backv = 1.0;

   return(*status);
}
