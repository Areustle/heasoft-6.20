/*
 *  xrgetparams.c
 *
 */

#include "pil.h"
#include "xronos.h"

int xrgetparams(XRTask *task, XRParam *param, int *status) {
/*
 *  Central Xronos parameter fetching, rationalization,
 *   and file checkout routine
 *
 *  I  task   - Contains properties of task
 *  O  param  - Contains user input parameters
 *  O  status - Error flag (0 = OK)
 */
   char errmsg[MAXLEN_ERR];
   
   /*
    * Get the number of points to use to fill in gaps
    */
   if ( task->use_gapfill ) {
      if ( (*status = PILGetInt("gapfill", &param->gapfill)) ) {
         sprintf(errmsg, "Error reading the 'gapfill' parameter");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      if ( param->gapfill < 0 ) {
         sprintf(errmsg, "gapfill=%d must be greater than 0 newbins",
                         param->gapfill);
         *status = -1;
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
   }
   /*
    * Get whether to force the start at the beginning of the 1st window
    */
   if ( (*status = PILGetBool("forcestart", &param->forcestart)) ) {
      sprintf(errmsg, "Error reading the 'forcestart' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   /*
    * Get number of points to determine how to do error calculation
    */
   if ( task->use_errorbars ) {
      if ( (*status = PILGetInt("errorbars", &param->errorbars)) ) {
         sprintf(errmsg, "Error reading the 'errorbars' parameter");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      if ( param->errorbars < 2 ) {
         sprintf(errmsg, "errorbars=%d to small, Minimum number of points for sigma from scatter is 2", param->errorbars);
         *status = -1;
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
   }
   /*
    * Get whether to do an exposure profile or not
    */
   if ( (*status = PILGetBool("exposure", &param->exposure)) ) {
      sprintf(errmsg, "Error reading the 'exposure' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   /*
    * Get how to do normalization
    */
   if ( (*status = PILGetInt("normalization", &param->normalization)) ) {
      sprintf(errmsg, "Error reading the 'normalization' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   /*
    * Get whether to force strict simultaneity
    */
   if ( (*status = PILGetBool("simultaneous", &param->simultaneous)) ) {
      sprintf(errmsg, "Error reading the 'simultaneous' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   /*
    * Get number of seconds in special window before
    */
   if ( (*status = PILGetReal("spwinbefore", &param->spwinbefore)) ) {
      sprintf(errmsg, "Error reading the 'spwinbefore' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   if ( param->spwinbefore < 0.0 ) {
      sprintf(errmsg, "spwinbefore=%g must be greater than 0 seconds",
                      param->spwinbefore);
      *status = -1;
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   /*
    * Get number of seconds in special window after
    */
   if ( (*status = PILGetReal("spwinafter", &param->spwinafter)) ) {
      sprintf(errmsg, "Error reading the 'spwinafter' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   if ( param->spwinafter < 0.0 ) {
      sprintf(errmsg, "spwinafter=%g must be greater than 0 seconds",
                      param->spwinafter);
      *status = -1;
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   /*
    * Get rescaling factor on output
    */
   if ( (*status = PILGetReal("rescale", &param->rescale)) ) {
      sprintf(errmsg, "Error reading the 'rescale' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   /*
    * Get offset value for final output
    */
   if ( (*status = PILGetReal("offset", &param->offset)) ) {
      sprintf(errmsg, "Error reading the 'offset' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   /*
    * Get whether to use fast Fourier transform
    */
   if ( task->fft_type ) {
      if ( (*status = PILGetBool("fast", &param->fast)) ) {
         sprintf(errmsg, "Error reading the 'fast' parameter");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
   }
   /*
    * Get whether to apply the exposure calculation for 
    *  fast pulsar with event file
    */
   if ( task->use_flatexpo ) {
      if ( (*status = PILGetBool("flatexpo", &param->flatexpo)) ) {
         sprintf(errmsg, "Error reading the 'flatexpo' parameter");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
   }
   /*
    * Get number of series
    */
   if ( xrgetnser(task, param, status) ) {
      sprintf(errmsg, "Error getting number of series");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /*
    * Get input files
    */
   if ( xrgetfiles(task, param, status) ) {
      sprintf(errmsg, "Error getting input files");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /* Read some high-level information from files */
   if ( xrskimfiles(task, param, status) ) {
      sprintf(errmsg, "Error reading files");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /* Get windows */
   if ( xrgetwin(task, param, status) ) {
      sprintf(errmsg, "Error getting windows");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /* Get binning parameters */

   if ( xrgetbinmode(task, param, status) ) {
      sprintf(errmsg, "Error getting bin mode parameters");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /*  TIME task-specific */
   if ( task->time_type ) {

      if ( xrgettime(task, param, status) ) {
         sprintf(errmsg, "Error getting TIME task new bin parameters");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }

      if ( xrgettimebreak(task, param, status) ) {
         sprintf(errmsg, "Error getting timebreak parameter");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }

   }

   /*  FOLD task-specific */
   if ( task->fold_type ) {
   /* not implemented 
      if ( xrgetfold(task, param, status) ) {
         sprintf(errmsg, "Error getting FOLD task new bin parameters");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
   */
   }

   /*  All tasks */
   if ( xrgetnbin(task, param, status) ) {
      sprintf(errmsg, "Error getting new bin parameters");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /* Not implemented */
   /*  xrgetres (SEARCH) */
   /*  xrgettrend (TIME) */
   /*  xrgetlc (lcurve) tunits parameter */

   if ( xrgetfout(task, param, status) ) {
      sprintf(errmsg, "Error getting output filename");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /*  xrgetplto */

   return(*status);

}
