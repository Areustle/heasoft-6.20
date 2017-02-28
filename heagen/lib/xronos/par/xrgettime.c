/*
 * xrgettime.c
 */
#include <math.h>
#include <string.h>
#include "pil.h"
#include "xronos.h"

/* Macro to simplify syntax */
#define NINT(x) (x<0?(int)((x)-0.5):(int)((x)+0.5))

int xrgettime(XRTask *task, XRParam *param, int *status) {
/*
 * Get parameters specific to linear binning method
 *
 *  I  task   - Contains properties of task
 * I/O param  - Contains user input parameters
 *  O  status - Error flag (0 = OK)
 */
   char errmsg[MAXLEN_ERR];
   int iv;
   double dv, dtint, dtrange, dtnb_default; 
   int nbdf, nbint;

   /* Return without getting parameters for select modes */
   if ( param->binmode == BINMODE_COUNTS ||
        param->binmode == BINMODE_BAYES ) {

      param->dtnb = 0.;
      param->nbint = 0;
      return(*status);
   }

   /* Simplify expressions by making copy */
   dtint = param->dtint;
   dtrange = param->dtrange;

   /* Default to original binning */
   dtnb_default = dtint;

   /* Default to task-specific setting for newbins per interval */
   nbdf = task->nbdf;

   /* For 1 intv of nbdf newbin */
   dv = dtrange*86400.0/(nbdf - 0.6);

   if ( dtint <= 0.0 || !task->strict_newbin ) {

      /* Set default dtnb for nbdf newbin/intv of less */
      /* Note: works also if dtint < 0 */
      if ( dv > dtint ) dtnb_default = dv;

   } else if ( dv > dtint ) {

      dtnb_default = ceil(dv/dtint)*dtint;
   }

   /* Adjust the default number of newbins if necessary */

   /*  Number of new bins expected for dtnb */
   iv = ceil(dtrange*86400.0/dtnb_default);

   /*  To have nbdf at most as default */
   if ( nbdf > iv ) nbdf = iv;

   /* For FFT analysis */
   /* Set nbdf to the next power of 2 */
   if ( task->fft_type ) {
      if ( nbdf <= 0 ) nbdf = 1;
      nbdf = pow(2,ceil(log10(nbdf)/log10(2.0)));
   }
 
   if ( dtint > 0.0 && task->strict_newbin ) {
      headas_chat(NORMAL, " Note: Newbin Time must be an integer multiple of Minimum Newbin Time\n");
   }

   /*
    * Write bin duration
    */
   if ( dtint > 0.0 ) {
      iv = ceil(dtrange*86400.0/dtint);
      headas_chat(TERSE, " Minimum Newbin Time %17.8g (s)\n", dtint);
      headas_chat(TERSE, " for Maximum Newbin No.. %d\n", iv);
   } else {
      headas_chat(TERSE, "\n Arrival Time Input File(s)\n");
   }
   headas_chat(TERSE, "\n");

   /*
    * Write default newbin duration
    */
   headas_chat(TERSE, 
      " Default Newbin Time is: %16.8g (s) (to have 1 Intv. of %d Newbins)\n",
      dtnb_default, nbdf);
   headas_chat(TERSE, " Type INDEF to accept the default value\n\n");

   /* Get dtnb parameter */
   if ( (*status = xrgindefdbl("dtnb", dtnb_default, &param->dtnb)) ) {
      sprintf(errmsg, "Error reading the 'dtnb' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /* Validate dtnb parameter */
   if ( (param->dtnb > 0.0 && param->dtnb < dtint) &&
        abs(param->dtnb - dtint) > 1e-8 ) *status = 1002;
   if ( param->dtnb <= 0.0 && NINT(param->dtnb) == 0 ) *status = 1002;
   if ( param->dtnb < 0.0 ) {
      if ( dtint < 0.0 ) {
         param->dtnb = - NINT(param->dtnb)*dtint;
      } else {
         *status = 1041;
      }
   }
   if ( *status != 0 ) {
      sprintf(errmsg, "Invalid Newbin Time: %17.8g", param->dtnb);
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   if ( param->dtnb >= dtrange*86400.0 ) {
      *status = 1023;
      sprintf(errmsg, "Newbin duration is longer than total duration.");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   headas_chat(VERBOSE, "( dtnb (s) = %17.10g )\n", param->dtnb);
   
   /* Reset newbin duration if must be strict multiple of original */
   if ( dtint > 0.0 && task->strict_newbin && param->dtnb > dtint 
        && fmod(param->dtnb, dtint) != 0.0 ) {
      param->dtnb = floor(param->dtnb/dtint)*dtint;
      headas_chat(TERSE, " Warning: Newbin Time reset to %15.8g s\n\n",
                  param->dtnb);
   }

   /* Get number of newbins per interval */
   nbint = nbdf;

   /*  Number of newbins expected for dtnb */
   iv = ceil(dtrange*86400.0/param->dtnb);
   if ( nbint > iv ) nbint = iv;
       
   /* For FFT analysis */
   /* Set nbint to the next power of 2 */
   if ( task->fft_type ) {
      if ( nbint <= 0 ) nbint = 1;
      nbint = pow(2,ceil(log10(nbint)/log10(2.0)));
   }
   param->nbint = nbint;  /* Store default nbint */

   return(*status);
}
