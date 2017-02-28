/*
 * xrbinpnt.c
 */
#include <math.h>
#include "xronos.h"

int xrbinpnt(XRBinState *bin, XRDataPoint *datapnt, XRExpoPoint *expopnt,
             double *cursta, double *cursto, bool *leftover, 
             XRSeriesData *series,  int *status) {
/*
 *  Put new data point into bin
 *
 *  I  bin      - Binning state
 *  I  datapnt  - Data point to insert
 *  I  expopnt  - Exposure point in effect (fracexp not used yet)
 * I/O cursta   - Start time of current data cell
 * I/O cursto   - Stop time of current data cell
 *  O  leftover - Whether there is leftover portion of data after binning
 *  O  series   - Series data to insert point into
 *  O  status   - Error flag (0=OK)
 */
   int index, binmode, logbase;
   double cntpnt, binduration, binoptimal, timediff, frac;
   double curexpo, curexpcor, expsta, expsto, datasta, datasto;
   bool delay_newbin = FALSE;
   bool at_timebreak = FALSE;

   *leftover = FALSE;
   curexpo = (*cursto - *cursta)*86400.;

   binmode = bin->mode;
   logbase = bin->logbase;

   /* Internally treat linear binning as logarithmic with logbase=1 */
   if ( binmode == BINMODE_LINEAR ) {
      binmode = BINMODE_LOG;
      logbase = 1;
   }

   /* Check if past timebreak */
   if ( bin->timebreak && *cursto > bin->timebreak->time ) {

      /* Protect against time values prior to data */
      if ( *cursta < bin->timebreak->time ) {
         *cursto = bin->timebreak->time;
         *leftover = TRUE;
         delay_newbin = TRUE;
      }

      at_timebreak = TRUE;
   }

   /* Check for full bin */
   if ( binmode == BINMODE_COUNTS ) {

      /* Use early edge of point as bin start */
      if ( bin->timesta == 0. ) { /* Store bin start time */
         bin->timesta = *cursta;
         bin->timesto = bin->timesta;
      }
      timediff = (*cursta - bin->timesto)*86400.;

      /* When sum hits counts/bin level, add newbin */
      if ( datapnt->event ) {
         cntpnt = datapnt->y;
      } else {
         cntpnt = datapnt->y*datapnt->expcor*curexpo/datapnt->expo;
      }
      if ( bin->gapintv != DOUBLENULLVALUE && timediff > bin->gapintv ) {
         if ( bin->sum + cntpnt < bin->cntbin ) {
            xrmergebin(bin, series, status);
         } else {
            xraddnewbin(bin, series, status);
         }
      } else if ( bin->sum + cntpnt >= bin->cntbin ) {
         delay_newbin = TRUE;  /* Add current point before adding newbin */
         if ( *status != 0 ) return(*status);
      }

   } else if ( binmode == BINMODE_LOG ) {

      /* When bin exposure hits duration of newbin, fill-in newbin */
      if ( bin->timesta == 0. ) {
         binduration = curexpo;
      } else {
         binduration = (*cursto - bin->timesta)*86400.;
      }
      binoptimal = bin->dtnb*pow(logbase, bin->index);
      if ( binduration > binoptimal ) {
         /* Change stop time of range to add to bin */
         *cursto = *cursto - (binduration-binoptimal)/86400.;
         *leftover = TRUE;
         delay_newbin = TRUE;
      }

   }

   /* If not set, use early edge of point as bin start */
   if ( bin->timesta == 0. ) { /* Store bin start time */
      bin->timesta = *cursta;
      bin->timesto = bin->timesta;
   }

   /* Place point in bin */

   curexpo = (*cursto - *cursta)*86400.;

   bin->npnts += 1;
   if ( datapnt->event ) {
      /*
       * Determine corrected exposure from intersecting 
       * portion of exposure point 
       */
      expsta = expopnt->timesta;
      expsto = expsta + expopnt->duration;
      if ( *cursta > expsta ) expsta = *cursta;
      if ( *cursto < expsto ) expsto = *cursto;
      if ( expsto > expsta ) {
         curexpcor = expopnt->fracexp*(expsto - expsta)*86400.;
      } else {
         curexpcor = 0.0;
      }

      /* Place entire event in bin if center of event in bin */
      if ( datapnt->time >= *cursta && datapnt->time <= *cursto ) {
         bin->sum += datapnt->y;
         bin->err += datapnt->y;
      }

   } else {

      /* Place intersecting portion of data point in bin */
      datasta = datapnt->time - datapnt->expo/2./86400.;
      datasto = datapnt->time + datapnt->expo/2./86400.;
      if ( *cursta > datasta ) datasta = *cursta;
      if ( *cursto < datasto ) datasto = *cursto;
      if ( datasto > datasta ) {
         frac = (datasto-datasta)*86400./datapnt->expo;
         if ( frac > 1.0 ) frac = 1.0;
         curexpcor = datapnt->expcor*frac;
         bin->sum += datapnt->y*curexpcor;
         bin->err += (datapnt->sy*datapnt->sy*curexpcor*curexpcor);
      } else {
         curexpcor = 0.0;
      }

   }
   /* Add gaps between data points to total exposure */
   timediff = (*cursta - bin->timesto)*86400.;
   if ( timediff > 0. ) {
      bin->expo += timediff;
      bin->timesto += timediff/86400.;
   }
   bin->timesto = *cursto;
   bin->expo += curexpo;
   bin->expcor += curexpcor;

   if ( delay_newbin ) xraddnewbin(bin, series, status);

   /* If timebreak encountered, update bin */
   if ( at_timebreak ) {
      bin->mode      = bin->timebreak->binmode;
      bin->dtnb      = bin->timebreak->dtnb;
      bin->logbase   = bin->timebreak->logbase;
      bin->cntbin    = bin->timebreak->cntbin;
      bin->gapintv   = bin->timebreak->gapintv;
      bin->timebreak = bin->timebreak->next;
   }

   /* Move on to next time */
   *cursta = *cursto;

   return(*status);
}
