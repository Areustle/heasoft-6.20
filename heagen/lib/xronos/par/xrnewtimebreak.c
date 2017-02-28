/*
 * xrnewtimebreak.c
 */
#include <stdlib.h>
#include "xronos.h"

XRTimeBreak *xrnewtimebreak(XRParam *param, double time, int *status) {
/*
 *  Allocate and set defaults in timebreak element for inclusion in linked list
 *
 *  I  param  - Contains user input parameters
 *  I  time   - Time of break
 *  O  status - Error flag (0 = OK)
 */
   XRTimeBreak *newtbrk;

   newtbrk = (XRTimeBreak *) malloc(sizeof(XRTimeBreak));
   if ( !newtbrk ) return(NULL);

   /* Set time */
   newtbrk->time = time;

   /* Fill with global binning parameters that can be overridden */
   newtbrk->binmode = param->binmode;
   newtbrk->cntbin  = param->cntbin;
   newtbrk->gapintv = param->gapintv;
   newtbrk->dtnb    = param->dtnb;
   newtbrk->logbase = param->logbase;

   newtbrk->next = NULL;

   return(newtbrk);
}
