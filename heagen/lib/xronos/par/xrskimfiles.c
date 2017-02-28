/*
 * xrskimfiles.c
 */
#include "xronos.h"

int xrskimfiles(XRTask *task, XRParam *param, int *status) {
/*
 * Read some high-level information from files
 *
 *  I  task   - Contains properties of task
 *  O  param  - Contains user input parameters
 *  O  status - Error flag (0 = OK)
 */
   char errmsg[MAXLEN_ERR];
   int i, j, hdutype, ftstat;
   XRSeries *series;
   XRFile *file;
   fitsfile *fptr;

   long nrows;

   for ( i = 0; i < param->nser; i++ ) {

      series = param->series[i];

      for ( j = 0; j < param->series[i]->nfiles; j++ ) {

         file = series->files[j];

         headas_chat(NORMAL, " Series %d file %d: %s\n", i+1, j+1, file->name);
         fits_open_file(&fptr, file->name, READONLY, status);
         if ( *status != 0 ) {
            sprintf(errmsg, "Couldn't open file: %s", file->name);
            HD_ERROR_THROW(errmsg, *status);
            return(*status);
         }
         /* Find relevant extensions */
         if ( xrftgext(fptr, file, status) ) goto cleanup;

         /* Move to data extension */
         fits_movabs_hdu(fptr, (file->ext)+1, &hdutype, status);

         /* Get number of rows */
         nrows = 0;
         fits_get_num_rows(fptr, &nrows, status);
         if ( *status == 0 && nrows == 0 ) {
            *status = 102;
            sprintf(errmsg, "No data in extension %d of %s", file->ext, 
                    file->name);
            HD_ERROR_THROW(errmsg, *status);
         }
         if ( *status != 0 ) goto cleanup;
         file->nrows = nrows;
          
         /* Find relevant columns */
         xrftgcol(fptr, file, status);

         /* Get timing information */
         if ( xrftgtky(fptr, file, status) ) {
            sprintf(errmsg, "Trouble reading header timing information");
            HD_ERROR_THROW(errmsg, *status);
            goto cleanup;
         }
         /* Get energy bounds */
         /* xrphaopt - not implemented */

         /* Get descriptive keywords */
         xrftgdes(fptr, file, status);

         /* Get GTI information */
         if ( file->gtiext ) {
            fits_movabs_hdu(fptr, (file->gtiext)+1, &hdutype, status);
            xrftgtgti(fptr, file, status);
         }

         /* Get corrections */
         xrftgtcor(fptr, file, status);

         /* Write summary of file */
         xrsummfile(file, status);

         /* Update series properties */
         xrupdtser(file, param->series[i], status);
         
         ftstat = 0;
         fits_close_file(fptr, &ftstat);
      }

      /* Update global properties of all series */
      if ( param->dtrange > 0.0 ) {
         if ( series->dtsta < param->dtsta ) param->dtsta = series->dtsta;
         if ( series->dtsto > param->dtsto ) param->dtsto = series->dtsto;
      } else {
         param->dtsta = series->dtsta;
         param->dtsto = series->dtsto;
      }
      param->dtrange = param->dtsto - param->dtsta;
      if ( series->dtint > param->dtint ) param->dtint = series->dtint;
   }
   return(*status);

cleanup:
   ftstat = 0;
   if ( fptr ) fits_close_file(fptr, &ftstat);
   return(*status);

}
