/*
 * xrgettimebreak.c
 */
#include <stdlib.h>
#include <string.h>
#include "pil.h"
#include "xronos.h"

int xrgettimebreak(XRTask *task, XRParam *param, int *status) {
/*
 * Get timebreak parameter, parse and store 
 *
 *  I  task   - Contains properties of task
 * I/O param  - Contains user input parameters
 *  O  status - Error flag (0 = OK)
 */
   char errmsg[MAXLEN_ERR];
   char strval[PIL_LINESIZE];
   char **tbrklist;
   XRTimeBreak *newtbrk, **curtbrk;
   int i, nitems;
   double time;
   char *argstr, *valstr;

   param->timebreak = NULL;
   if ( (*status = PILGetString("timebreak", strval)) ) {
      sprintf(errmsg, "Error reading the 'timebreak' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   if ( strcmp(strval, "") == 0 ) return(*status);

   /*
    *  expand_item_list (headas_utils.h)
    *  Supports: @timebreak.txt
    *            T1 param=value param=value,T2 param=value
    */
   tbrklist = expand_item_list(strval, &nitems, ',', 1, 1, 1, status);
   if ( *status != 0 ) {
      sprintf(errmsg, "Failed to parse timebreak '%s'", strval);
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   curtbrk = &param->timebreak;
   for ( i = 0; i < nitems; i++ ) {

      /* Line format: time param=value param=value */
      argstr = fits_split_names(tbrklist[i]);
      time = atof(argstr);

      newtbrk = xrnewtimebreak(param, time, status);
      if ( !newtbrk ) {
         *status = MEMORY_ALLOCATION;
         sprintf(errmsg, "Failed to allocate space for timebreak");
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }

      argstr = fits_split_names(NULL);
      while ( argstr ) {
         valstr = strchr(argstr, '=');
         if ( valstr ) {
            *valstr = '\0';
            valstr++;
         } else {
            *status = -1;
            sprintf(errmsg, "Incorrect timebreak format");
            HD_ERROR_THROW(errmsg, *status);
            return(*status);
         }
         fits_uppercase(argstr);
         if ( strcmp(argstr,"BINMODE") == 0 ) {
            xrparsebinmode(valstr, &newtbrk->binmode, status);
         } else if ( strcmp(argstr, "CNTBIN") == 0 ) {
            newtbrk->cntbin = atof(valstr);
         } else if ( strcmp(argstr, "GAPINTV") == 0 ) {
            newtbrk->gapintv = atof(valstr);
         } else if ( strcmp(argstr, "DTNB") == 0 ) {
            newtbrk->dtnb = atof(valstr);
         } else if ( strcmp(argstr, "LOGBASE") == 0 ) {
            newtbrk->logbase = atoi(valstr);
         } else {
            *status = -1;
            sprintf(errmsg, "Unrecognized timebreak property: %s", argstr);
            HD_ERROR_THROW(errmsg, *status);
            return(*status);
         }
         argstr = fits_split_names(NULL);
      }

      newtbrk->next = NULL;
      *curtbrk = newtbrk;
      curtbrk = &newtbrk->next;
   }
   free(tbrklist);

   return(*status);
}
