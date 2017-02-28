/*
 * xrsetupfrm.c
 */
#include "xronos.h"

int xrsetupfrm(XRTask *task, XRParam *param, XRFrame *frame, int *status) {
/*
 *  Set up frame based on user inputs
 *
 *  I  task   - Contains properties of task
 *  I  param  - Contains user inputs
 *  O  frame  - Contains data frame
 *  O  status - Error flag (0=OK)
 */
   int i, j;
   char errmsg[MAXLEN_ERR];
   XRInterval **intvs;
   XRInterval *intvsary;
   XRInterval *intv;

   XRSeriesData **sdata;
   XRSeriesData *sdary;

   /* Allocate and setup intervals */
   frame->mxintv  = param->nintv;
   frame->nintv   = param->nintv;
   frame->curintv = 0;

   intvs = (XRInterval **) calloc(param->nintv, sizeof(XRInterval *));
   intvsary = (XRInterval *) calloc(param->nintv, sizeof(XRInterval));
   if ( !intvs || !intvsary ) {
      *status = MEMORY_ALLOCATION;
      sprintf(errmsg, "Failed to allocate space for intervals");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   for ( i = 0; i < frame->nintv; i++ ) {

      /* 
       * Maintain access to structs via '->' operator 
       * intvs is array of ptrs to XRInterval structs 
       */
      intvs[i]  = &intvsary[i];

      /* Allocate and setup series data */
      intv = intvs[i];
      intv->mxser = param->nser;
      intv->nser = param->nser;
      intv->id = i+1;

      sdata = (XRSeriesData **) calloc(intv->nser, sizeof(XRSeriesData *));
      sdary = (XRSeriesData *) calloc(intv->nser, sizeof(XRSeriesData));
      if ( !sdata || !sdary ) {
         *status = MEMORY_ALLOCATION;
         sprintf(errmsg, "Failed to allocate space for interval %d", i+1);
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      /* 
       * Maintain access to structs via '->' operator 
       * sdata is array of ptrs to XRSeriesData structs 
       */
      for ( j = 0; j < intv->nser; j++ ) {
         sdata[j]  = &sdary[j];

         /* Transfer some information from series inputs to series data */
         sdata[j]->dtrange = param->series[j]->dtrange;
         sdata[j]->dtsta = param->series[j]->dtsta;
         sdata[j]->dtsto = param->series[j]->dtsto;
         sdata[j]->dtint = param->series[j]->dtint;
      }
      intv->series = sdata;
   }
   frame->intvs = intvs;

   return(*status);
}
