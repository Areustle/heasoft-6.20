/*
 * xrgetnbin.c
 */
#include <math.h>
#include "pil.h"
#include "xronos.h"

int xrgetnbin(XRTask *task, XRParam *param, int *status) {
/*
 * Get binning parameters
 *
 *  I  task   - Contains properties of task
 * I/O param  - Contains user input parameters
 *  O  status - Error flag (0 = OK)
 */
   int iv, nintv, nbint_default, nintfm_default;
   char errmsg[MAXLEN_ERR];

   /* Not applicable to select binning modes */
   if ( param->binmode == BINMODE_COUNTS ||
        param->binmode == BINMODE_BAYES ) {
      param->nintv = 1;
      param->nintfm = 1;
      return(*status);
   }
   
   iv = ceil(param->dtrange*86400.0/param->dtnb);
   headas_chat(NORMAL, " Newbin Time ...... %17.8g (s)\n", param->dtnb);
   headas_chat(TERSE,  " Maximum Newbin No. %d\n\n", iv);

   /* Default nbint calculated in xrgettime or xrgetfold */
   nbint_default = param->nbint;

   headas_chat(TERSE,  " Default Newbins per Interval are: %d\n",
                       nbint_default);
   iv = (iv/(nbint_default+1)+1);
   headas_chat(TERSE,  " (giving %d Interval%s of %d Newbins%s)\n", iv, 
                       (iv == 1 ? "" : "s"), nbint_default,
                       (iv == 1 ? "" : " each"));
   headas_chat(NORMAL, " Type INDEF to accept the default value\n\n");

   /* Get Newbins per interval */
   if ( (*status = xrgindefint("nbint", nbint_default, &param->nbint)) ) {
      sprintf(errmsg, "Error getting 'nbint' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   if ( param->nbint == 0 || param->nbint == 1 || param->nbint < -31 ) {
      sprintf(errmsg, "Invalid Newbins/Interval: %d", param->nbint);
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /* Negative nbint implies 2**(-nbint) */
   if ( param->nbint < 0 ) param->nbint = pow(2, -param->nbint);

   if ( !task->fold_type ) {

      /* For FFT analysis */
      /* Set to next power of 2 */
      if ( task->fft_type ) {
         iv = pow(2,ceil(log10(param->nbint)/log10(2.0)));
         if ( iv != param->nbint ) {
            headas_chat(TERSE, " Warning: No. of Newbins/Intv. reset to %d\n",
                               param->nbint);
            param->nbint = iv;
         }
      }

      headas_chat(VERBOSE, "(nbint = %d)\n", param->nbint);
   }

   /* Calculate expected number of intervals */
   iv = ceil(param->dtrange*86400.0/param->dtnb);
   nintv = ceil( (float) iv / (float) param->nbint);

   headas_chat(VERBOSE, " (giving %d Interval%s of %d Newbins %s)", nintv, 
                       (nintv == 1 ? "" : "s"), param->nbint,
                       (nintv == 1 ? "" : "each"));

   headas_chat(NORMAL, " Maximum of %d Intvs. with %d Newbins of %17.6g (s)\n",
                       nintv, param->nbint, param->dtnb);
   param->nintv = nintv; /* Store number of intervals */

   /* Get intervals/frame if necessary */
   param->nintfm = 1;
   if ( task->use_nintfm ) {
      nintfm_default = nintv;
      headas_chat(TERSE, "(Default intervals per frame are: %d\n",
                          nintfm_default);

      if ( (*status = xrgindefint("nintfm", nintfm_default, &param->nintfm)) ) {
         sprintf(errmsg, "Error getting 'nintfm' parameter");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      if ( param->nintfm <= 0 ) {
         sprintf(errmsg, "Invalid Number of Intervals/Frame: %d",
                         param->nintfm);
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      headas_chat(VERBOSE, "( nintfm = %d )\n", param->nintfm);

      headas_chat(TERSE, 
         " Results from up to %d Intvs. will be averaged in a Frame\n", 
         param->nintfm);
   }

   /* Get result rebinning constant if necessary (>1 linear, <1 log) */
   param->rebin = 1.0;
   if ( task->use_rebin ) {
      if ( (*status = PILGetReal("rebin", &param->rebin)) ) {
         sprintf(errmsg, "Error getting 'rebin' parameter");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      if ( param->rebin < -2.0 ) {
         sprintf(errmsg, "Invalid Rebin: %9.2g\n", param->rebin);
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      headas_chat(VERBOSE, "( rebin = %14.8g )\n", param->rebin);

      if ( param->rebin > 1.0 ) {
         headas_chat(NORMAL, 
            " Results will be rebinned by a factor of %7.2g\n", param->rebin);
      } else if ( param->rebin < -1.0 ) {
         headas_chat(NORMAL, "%s %7.2g\n",
            " Results will be rebinned geometrically with a series of step",
            param->rebin);
      }
   }
         
   /*
    *  Write various messages
    */

   /* Force simultaneity */
   if ( param->simultaneous && param->nser > 1 ) {
      headas_chat(TERSE, 
         " Note: Simultaneousness of the %d series will be forced\n", 
         param->nser);
   }
   if ( param->rescale != 1.0 ) {
      headas_chat(TERSE, 
         " Note: All results and errors will be multiplied by %14.7g\n",
         param->rescale);
   }
   if ( param->offset != 0.0 ) {
      headas_chat(TERSE, 
         " Note: All results and errors will be added with %14.7g\n",
         param->offset);
   }
   

   /* Maximum number of frames expected */
   param->nframes = (param->nintv - 1)/abs(param->nintfm) + 1;

   return(*status);
} 
