/*
 *  grbcurve.c
 *
 *  C Implementation of Xronos lcurve
 *
 */

#define TOOLSUB grbcurve
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

#include "xronos.h"

int grbcurve () {

   static char taskname[] = "grbcurve";
   static char version[]  = "1.00";
   static char code[]     = "lc";

   /* Constant properties of task */

   XRTask task = { /* name            = */ taskname,
                   /* version         = */ version,
                   /* code            = */ code,
                   /* time_type       = */ TRUE,
                   /* fft_type        = */ FALSE,
                   /* search_type     = */ FALSE,
                   /* fold_type       = */ FALSE,
                   /* use_outfileroot = */ TRUE,
                   /* use_gapfill     = */ TRUE,
                   /* use_errorbars   = */ FALSE,
                   /* use_flatexpo    = */ FALSE,
                   /* use_nintfm      = */ FALSE,
                   /* use_rebin       = */ FALSE,
                   /* strict_newbin   = */ FALSE,
                   /* nbdf            = */ 512 };

   XRParam param = { 0 };
   XRFrame frdata = { 0 };
   XRFrame *frame;

   int status;

   status = 0;
   frame = &frdata; /* Always access frame with -> syntax */

   /* Task initialization */
   xrinit(&task, &status);
   
   /* Retrieve parameters */
   if ( xrgetparams(&task, &param, &status) ) return(status);

   /* Set up a data frame */
   if ( xrsetupfrm(&task, &param, frame, &status) ) return(status);

   /* Loop through intervals in frame */
   while ( frame->curintv < frame->nintv ) {

      /* Process interval */
      if ( xrgetintv(&task, &param, frame, &status) ) return(status);

      /* plot: not implemented */

      frame->curintv++;
   }

   /* Write output file */
   xrwroutf(&task, &param, frame, &status);

   return(status);
}
