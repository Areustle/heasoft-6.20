/*
 * xrupdtser.c
 */
#include "xronos.h"

int xrupdtser(XRFile *file, XRSeries *series, int *status) {
/*
 * Update series properties based on properties of new file
 *
 *  I  file   - Contains properties of file
 * I/O series - Contains properties of series
 *  O  status - Error flag (0=OK)
 */
   char errmsg[MAXLEN_ERR];
   double dtsta, dtsto;

   /*
    *  Set type of data represented in series
    */
   if ( series->type ) {
      if ( file->type != series->type ) {
         *status = 1022;
         sprintf(errmsg,
                 "Cannot mix binned data and event arrival time files");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
   } else {
      series->type = file->type;
   }

   /* 
    * Set start/stop
    */
   dtsta = file->dtsta;
   dtsto = file->dtsto;
    
   /* Use GTI start and stop when present */
   if ( file->type == EVENT_DATA && file->gtiext ) {
      dtsta = file->gtista;
      dtsto = file->gtisto;
   }

   if ( series->dtrange > 0.0 ) {

      if ( dtsta < series->dtsta ) {
         *status = 1021;
         sprintf(errmsg, "Error: infiles are not time-ordered");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      if ( dtsta < series->dtsto ) {
         headas_chat(TERSE, " Warning: files for series %d overlap in time\n",
                     series->id);
         headas_chat(TERSE, 
            " Having > 1 intv or using time winss. might cause data loss!\n");
      }
      if ( dtsto > series->dtsto ) {
         series->dtsto = dtsto;
      }

   } else {

      series->dtsta = dtsta;
      series->dtsto = dtsto;

   }
   series->dtrange = series->dtsto - series->dtsta;

   /*
    * Set integration time
    */
   if ( series->dtint > 0.0 && file->dtint != series->dtint ) {
      headas_chat(NORMAL, 
         " Warning: Different binning times in different infiles!\n");
      headas_chat(NORMAL, 
         " New Bin integ. time must be multiple of all bin times!\n");
   }
   /* Use longest bin as default */
   if ( file->dtint > series->dtint ) series->dtint = file->dtint;

   return(*status);
}
