/*
 * xrgetwin.c
 */
#include "xronos.h"

int xrgetwin(XRTask *task, XRParam *param, int *status) {
/*
 * Read in windows and store in param
 *
 *  I  task   - Contains properties of task
 * I/O param  - Contains user input parameters
 *  O  status - Error flag (0 = OK)
 */
   int i;
   XRSeries *series;

/* Default windows hard-coded */
/* Delaying implementation as there has been talk of changing format of
 * input window files */

   
   for ( i = 0; i < param->nser; i++ ) {
      series = param->series[i];
      series->oriewia = 0.25;
      series->oriewio = 50.0;
      series->newewia = 9.9999998e-03;
      series->newewio = 50.0;
      series->intewia = 0.5;
      series->intewio = 50.0;
   }

   return(*status);
}
