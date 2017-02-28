/*
 * xrfreeser.c
 */
#include <stdlib.h>
#include "xronos.h"

int xrfreeser(XRSeriesData *series, int *status) {
/*
 *  Free series arrays
 *
 * I/O series  -   Series data 
 *  O  status (i)  Error flag (0=OK)
 */
   series->mxbint = 0;
   series->nbint  = 0;

   if ( series->y ) free(series->y);
   series->y = NULL;
   if ( series->sy ) free(series->sy);
   series->sy = NULL;
   if ( series->expcor ) free(series->expcor);
   series->expcor = NULL;
   if ( series->expo ) free(series->expo);
   series->expo = NULL;
   if ( series->time ) free(series->time);
   series->time = NULL;

   return(*status);
}
