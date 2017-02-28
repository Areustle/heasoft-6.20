/*
 * xrbininit.c
 */
#include "xronos.h"

int xrbininit(XRBinState *bin, XRParam *param, int *status) {
/*
 * Initialize bin state
 *
 * I/O bin     - Bin state to initialize
 *  I  param   - User input parameters
 *  O  status  i Error flag (0=OK)
 */

   bin->mode = param->binmode;
   bin->index = 0;
   bin->npnts = 0;
   bin->timesta = 0.;
   bin->timesto = 0.;
   bin->sum = 0;
   bin->err = 0;
   bin->expo = 0.;
   bin->expcor = 0.;

   /* Binning properties */

   bin->mode = param->binmode;
   bin->dtnb = param->dtnb;
   bin->logbase = param->logbase;

   /* Binning by counts */
   bin->cntbin = param->cntbin;
   bin->gapintv = param->gapintv;

   /* Time break */
   bin->timebreak = param->timebreak;

   /* 
    * Bayesian Block Binning
    */
   if ( bin->mode == BINMODE_BAYES ) {

      /* First time through bin by counts to create cells */
      bin->mode = BINMODE_COUNTS;
      bin->cntbin = param->evtcell;
      bin->gapintv = DOUBLENULLVALUE;
      bin->timebreak = NULL;

   }

   return(*status);
}
