/*
 * xrallocser.c
 */
#include <stdlib.h>
#include "xronos.h"

int xrallocser(int index, XRSeriesData *series, int *status) {
/*
 * Check that there is enough room for a particular array index
 *  in the series arrays.  If not, allocate a new block.
 *
 *  I  index  (i)  Array index
 * I/O series  -   Series data 
 *  O  status (i)  Error flag (0=OK)
 */
   const int bufsize = 100;
   int i;
   char errmsg[MAXLEN_ERR];

   if ( index < series->mxbint ) return(*status);

   series->mxbint += bufsize;

   if ( series->y ) {
      series->y = realloc(series->y, sizeof(float)*series->mxbint);
   } else {
      series->y = malloc(sizeof(float)*series->mxbint);
   }

   if ( series->sy ) {
      series->sy = realloc(series->sy, sizeof(float)*series->mxbint);
   } else {
      series->sy = malloc(sizeof(float)*series->mxbint);
   }

   if ( series->expcor ) {
      series->expcor = realloc(series->expcor, sizeof(float)*series->mxbint);
   } else {
      series->expcor = malloc(sizeof(float)*series->mxbint);
   }

   if ( series->expo ) {
      series->expo = realloc(series->expo, sizeof(double)*series->mxbint);
   } else {
      series->expo = malloc(sizeof(double)*series->mxbint);
   }

   if ( series->time ) {
      series->time = realloc(series->time, sizeof(double)*series->mxbint);
   } else {
      series->time = malloc(sizeof(double)*series->mxbint);
   }

   /* Check allocations */
   if ( !series->y || !series->sy   || !series->expcor || 
                      !series->expo || !series->time ) {
      *status = MEMORY_ALLOCATION;
      sprintf(errmsg, "Failed to allocate space for series data");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /* Initialize */
   for ( i = series->nbint; i < series->mxbint; i++ ) {
      series->y[i] = 0.;
      series->sy[i] = 0.;
      series->expcor[i] = 0.;
      series->expo[i] = 0.;
      series->time[i] = 0;
   }
   return(*status);
}
