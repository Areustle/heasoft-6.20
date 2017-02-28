/*
 * xrupdtexpo.c
 */
#include "xronos.h"

int xrupdtexpo(XRExpoPoint *pnt, XRExpoPoint *lastpnt, int *ibin,
               double *curtime, XRSeriesData *series, int *status) {
/*
 * Update expo array in series based on an exposure point
 *
 *  I  pnt     - Exposure point (time, duration, fracexp)
 *  I  lastpnt - Last exposure point
 * I/O ibin    - Bin index
 * I/O curtime - Current time
 * I/O series  - Series data
 *  O  status  - Error flag (0=OK)
 */
   double binsta, binsto, corsec;

   while ( *ibin < series->nbint ) {

      binsta = series->time[*ibin] - series->expo[*ibin]/2./86400.;
      binsto = binsta + series->expo[*ibin]/86400.;

      /* Check if curtime indicates a leftover partial bin */
      if ( *curtime > binsta ) {
         binsta = *curtime;
      } else {
         series->expcor[*ibin] = 0.;
      }

      /*
       * If exposure 'event' comes before current bin, 
       * exit to get next 'event'
       */
      if ( pnt->valid && pnt->timesta < binsta ) {
         lastpnt->timesta = pnt->timesta;
         lastpnt->fracexp = pnt->fracexp;
         return(*status);
      }

      if ( !pnt->valid || pnt->timesta > binsto ) {
         /*
          * If exposure 'event' comes after current bin,
          * calculate corrected exposure and move to next bin
          * Note: invalid point is effectively at timesta=infinity
          */
         corsec = (binsto-binsta)*86400.;
         series->expcor[*ibin] = series->expcor[*ibin] +
                                 corsec*lastpnt->fracexp;
         *ibin = *ibin + 1;

      } else {
          /*
           * Otherwise exposure 'event' splits bin
           * Calculate portion that is known and exit to get next 'event'
           */
         corsec = (pnt->timesta - binsta)*86400.;
         series->expcor[*ibin] = series->expcor[*ibin] +
                                 corsec*lastpnt->fracexp;
         *curtime = pnt->timesta;
         lastpnt->timesta = pnt->timesta;
         lastpnt->fracexp = pnt->fracexp;
         return(*status);
      }
   }
   return(*status);
}
