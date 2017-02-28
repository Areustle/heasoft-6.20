/*
 * xrgetbinmode.c
 */
#include <string.h>
#include "pil.h"
#include "xronos.h"

int xrgetbinmode(XRTask *task, XRParam *param, int *status) {
/*
 * Get binmode parameter 
 *
 *  I  task   - Contains properties of task
 * I/O param  - Contains user input parameters
 *  O  status - Error flag (0 = OK)
 */
   char errmsg[MAXLEN_ERR];
   char binmodestr[PIL_LINESIZE];

   /* Binning mode */
   if ( (*status = PILGetString("binmode", binmodestr)) ) return(*status);

   *status = xrparsebinmode(binmodestr, &param->binmode, status);
   if ( *status ) return(*status);

   if ( param->binmode == BINMODE_LOG ) {

      /* Base of logarithmic binning */
      if ( (*status = PILGetInt("logbase", &param->logbase)) ) {
         sprintf(errmsg, "Error getting 'logbase' parameter");
         *status = -1;
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      if ( param->logbase <= 0 ) {
         sprintf(errmsg, "Invalid Logarithmic Base: %d", param->logbase);
         *status = -1;
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
 
   } else if ( param->binmode == BINMODE_COUNTS ) {

      /* Counts per bin */
      if ( (*status = PILGetReal("cntbin", &param->cntbin)) ) {
         sprintf(errmsg, "Error reading the 'cntbin' parameter");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }

      /* 
       * Gap interval (missing data > gapintv considered a gap)
       * No entry or INDEF entry assigns DOUBLENULLVALUE
       */
      if ( (*status = xrgindefdbl("gapintv", -1.0, &param->gapintv)) ) {
         sprintf(errmsg, "Error reading the 'gapintv' parameter");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      if ( param->gapintv < 0 ) param->gapintv = DOUBLENULLVALUE;

   } else if ( param->binmode == BINMODE_BAYES ) {

      /* Events per cell */
      if ( (*status = PILGetInt("evtcell", &param->evtcell)) ) {
         sprintf(errmsg, "Error getting 'evtcell' parameter");
         *status = -1;
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      if ( param->evtcell <= 0 ) {
         sprintf(errmsg, "Invalid events per cell: %d", param->evtcell);
         *status = -1;
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }

      /* Bins per block */
      if ( (*status = PILGetInt("binblock", &param->binblock)) ) {
         sprintf(errmsg, "Error getting 'binblock' parameter");
         *status = -1;
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      if ( param->binblock <= 0 ) {
         sprintf(errmsg, "Invalid bins per block: %d", param->binblock);
         *status = -1;
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }

      /* Log of number of changepoints (complexity parameter) */
      if ( (*status = PILGetReal("logncp", &param->logncp))) {
         sprintf(errmsg, "Error reading the 'logncp' parameter");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      
   }
   return(*status);
}
