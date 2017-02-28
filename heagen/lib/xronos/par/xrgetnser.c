/*
 * xrgetnser.c
 */
#include "pil.h"
#include "xronos.h"

int xrgetnser(XRTask *task, XRParam *param, int *status) {
/*
 *  Get number of series and allocates associated structs
 *   in param
 *
 *  I  task   - Contains properties of task
 *  I  param  - Contains parameters input by user
 *  O  status - Error flag (0=OK)
 */
   int nser, i;
   char errmsg[MAXLEN_ERR];
   XRSeries **series;
   XRSeries *serary;

   if ( (*status = PILGetInt("nser", &nser)) ) {
      sprintf(errmsg, "Error getting number of series");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   param->mxser = nser;
   param->nser = nser;

   series = (XRSeries **) calloc(nser, sizeof(XRSeries *));
   serary = (XRSeries *) calloc(nser, sizeof(XRSeries));
   if ( !series || !serary ) {
      *status = MEMORY_ALLOCATION;
      sprintf(errmsg, "Failed to allocate space for series");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   /*
    *  Maintain access to structs via '->' operator
    *  series is array of ptrs to Series structs
    */
   for ( i = 0; i < nser; i++ ) {
      series[i] = &serary[i];
      series[i]->id = i+1;
   }
   param->series = series;

   return(*status);
}
