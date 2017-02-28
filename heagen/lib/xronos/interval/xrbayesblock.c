/*
 * xrbayesblock.c
 */
#include <math.h>
#include "xronos.h"

/* Prototypes */
double xrbbcost_maxlike(double cellsize, double cellpop);
double xrbbcost_bininfpri(double cellsize, double cellpop);

int xrbayesblock(XRParam *param, XRSeriesData *series, 
                 XRBinState *bin, int *status) {
/*
 *  Set up frame based on user inputs
 *
 *  I  param  - Contains user inputs
 *  I  series - Series data
 *  O  bin    - Bin state initialized for second run through
 *  O  status - Error flag (0=OK)
 */
   int i, j, ista, isto, ncells;
   int *last_indices;
   double *best_optima;
   double cost_work, sumcellsize, sumcellpop;
   double (*costfcn) (double, double);
   double tsta, tsto;

   char errmsg[MAXLEN_ERR];
   XRTimeBreak *newtbrk;

   /* Allocate working arrays */
   ncells = series->nbint;
   last_indices = malloc(ncells*sizeof(int));
   best_optima  = malloc(ncells*sizeof(double));
   if ( !last_indices || !best_optima ) {
      *status = MEMORY_ALLOCATION;
      sprintf(errmsg, "Failed to allocate space for Bayesian blocks");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   
   /* Only one cost function implemented thus far */
   costfcn = xrbbcost_maxlike;
   costfcn = xrbbcost_bininfpri;

   /*
    * Algorithm adapted from Scargle
    * Studies in Astronomical Time Series Analysis VI.:
    * Optimal Segmentation: Blocks, Histograms and Triggers
    */
   for ( i = 0; i < ncells; i++ ) {

      /* Add cost of cumulative sum of cells starting from end */
      sumcellsize = 0.;
      sumcellpop = 0.;
      best_optima[i] = DOUBLENULLVALUE;
      for ( j = i; j >= 0; j-- ) {
	 /*
         sumcellsize += series->expo[j];
         sumcellsize += (series->expo[j]/100.0e-6);
	 */
         sumcellsize += (series->expo[j]/2.5073);
         sumcellpop  += series->y[j];
         cost_work = costfcn(sumcellsize, sumcellpop);
         cost_work = cost_work - param->logncp;
         if ( j > 0 ) cost_work += best_optima[j-1];

         /*
          * Save maximum value in best_optima array
          *  and maximum location in last_indices array
          */
         if ( best_optima[i] == DOUBLENULLVALUE ||
              cost_work > best_optima[i] ) {
            best_optima[i] = cost_work;
            last_indices[i] = j;
         }
      }
   }

   /* Initialize bin state for second runthrough */
   xrbininit(bin, param, status);

   /*  
    * Translate change points into timebreaks
    * Changepoints are encountered in reverse order
    */
   isto = ncells - 1;
   ista = last_indices[i-1];
   newtbrk = NULL;
   while ( ista > 0 ) {

      /* Save last timebreak */
      bin->timebreak = newtbrk; 

      /* New timebreak */
      tsta = series->time[ista] - series->expo[ista]/86400.0/2.0;
      tsto = series->time[isto] + series->expo[isto]/86400.0/2.0;
      newtbrk = xrnewtimebreak(param, tsta, status);
      newtbrk->binmode = BINMODE_LINEAR;
      newtbrk->dtnb = (tsto-tsta)*86400./param->binblock;

      /* Insert new timebreak in front of last timebreak */
      newtbrk->next = bin->timebreak;
      bin->timebreak = newtbrk;

      /* Next block */
      isto = ista - 1;
      ista = last_indices[isto];

   }
   /* Set binning for first block as initial bin state */
   bin->mode = BINMODE_LINEAR;
   tsta = series->time[ista] - series->expo[ista]/86400.0/2.0;
   tsto = series->time[isto] + series->expo[isto]/86400.0/2.0;
   bin->dtnb = (tsto-tsta)*86400./param->binblock;

   /* Deallocate memory no longer needed */
   free(last_indices);
   free(best_optima);
   xrfreeser(series, status);

   return(*status);
}

double xrbbcost_maxlike(double cellsize, double cellpop) {
/*
 *  Returns log of cost function
 *  Maximum likelihood
 *  Applicable to any Poissonian data
 *   e.g. TTE data where multiple events can be in same tick 
 *
 *  I  cellsize  - Width of cell in ticks
 *  I  cellpop   - Number of events in cell
 */

   static double epsilon = 2.2e-16;

   return ( cellpop * log(cellpop + epsilon) - log(cellsize) - 1 );
}

double xrbbcost_bininfpri(double cellsize, double cellpop) {
/*
 *  I  cellsize  - Width of cell in ticks
 *  I  cellpop   - Number of events in cell
 */

   return ( lgamma(cellpop+1) - (cellpop+1) * log(cellsize) );
}
