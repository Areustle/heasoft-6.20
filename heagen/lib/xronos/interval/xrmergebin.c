/*
 * xrmergebin.c
 */
#include <math.h>
#include "xronos.h"

int xrmergebin(XRBinState *bin, XRSeriesData *series, int *status) {
/*
 *  Merge current accumulated bin with last recorded bin
 *
 *  bin     - Binning state
 *  series  - Series data to merge bin
 *  status  i Error flag (0=OK)
 */
   int i;
   double sy, timesta, timesto;

   /* Do not add if bin has not information in it */
   if ( bin->npnts == 0 ) return(*status);

   i = bin->index - 1;
   timesta = series->time[i] - series->expo[i]/86400./2.;
   timesto = timesta + series->expo[i]/86400.;

   /* If no prior bin or last bin is not adjacent, use xraddnewbin */
   if ( i == 0 || fabs(bin->timesta - timesto) > EPSILON ) {
      xraddnewbin(bin, series, status);
      return *status;
   }

   series->y[i] += bin->sum;
   sy = series->sy[i]*series->sy[i];
   series->sy[i] = sqrt(sy + bin->err);
   series->expo[i] += bin->expo;
   series->expcor[i] += bin->expcor;
   series->time[i] = (timesta + bin->timesto)/2.0;

   bin->npnts = 0;
   bin->sum = 0.;
   bin->err = 0.;
   bin->expo = 0.;
   bin->expcor = 0.;
   bin->timesta = 0.;
   bin->timesto = 0.;

   return(*status);
}
