/*
 * xrgetintv.c
 */
#include <math.h>
#include "xronos.h"

int xrgetintv(XRTask *task, XRParam *param, XRFrame *frame, int *status) {
/*
 *  Set up frame based on user inputs
 *
 *  I  task   - Contains properties of task
 *  I  param  - Contains user inputs
 *  O  frame  - Contains data frame
 *  O  status - Error flag (0=OK)
 */
   int i;
   XRInterval *intv;
   XRBinState binstor;
   XRBinState *bin;

   bin = &binstor;            /* Always use -> syntax to access */
   intv = frame->intvs[frame->curintv]; /* Use current interval */

   /* Loop through series, reading data points */

   for ( i = 0; i < param->nser; i++ ) {

      headas_chat(VERBOSE, "(Processing Interval: %d Series: %d)\n", 
                           intv->id, i+1);

      xrbininit(bin, param, status);

      /* Bin series data */
      xrbinseries(bin, param->series[i], intv->series[i], status);

      /* Bayesian block calculation */
      if ( param->binmode == BINMODE_BAYES ) {
         xrbayesblock(param, intv->series[i], bin, status);
         xrbinseries(bin, param->series[i], intv->series[i], status);
      }

      /* Update interval-wide info (set nbins to max nbint)*/
      if ( intv->series[i]->nbint > intv->nbins ) {
         intv->nbins = intv->series[i]->nbint;
      }
   }

   return(*status);
}
