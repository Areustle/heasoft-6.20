/*
 * xraddnewbin.c
 */
#include <math.h>
#include "xronos.h"

int xraddnewbin(XRBinState *bin, XRSeriesData *series, int *status) {
/*
 *  Add newbin to series based on contents of bin state
 *
 *  bin     - Binning state
 *  series  - Series data to add bin
 *  status  i Error flag (0=OK)
 */
   int i;

   /* Do not add if bin has not information in it */
   if ( bin->npnts == 0 ) return(*status);

   i = bin->index;

   xrallocser(i, series, status);
   if ( *status != 0 ) return(*status);

   series->y[i] = bin->sum;
   series->sy[i] = sqrt(bin->err);
   if ( XRONOS5_EMULATION && bin->mode == BINMODE_LINEAR ) {
      series->expo[i] = bin->dtnb;
   } else {
      series->expo[i] = bin->expo;
   }
   series->expcor[i] = bin->expcor;
   series->time[i] = (bin->timesta + bin->timesto)/2.0;
   series->nbint = i + 1;

   bin->index++;
   bin->npnts = 0;
   bin->sum = 0.;
   bin->err = 0.;
   bin->expo = 0.;
   bin->expcor = 0.;
   bin->timesta = 0.;
   bin->timesto = 0.;

   return(*status);
}
