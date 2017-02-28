/*
 * xrgetfout.c
 */
#include <string.h>
#include "pil.h"
#include "xronos.h"

int xrgetfout(XRTask *task, XRParam *param, int *status) {
/*
 *  Get output file parameters
 *
 *  I  task   - Contains properties of task
 *  I  param  - Contains user inputs
 *  O  status - Error flag (0=OK)
 */
   int match, exact;
   char strval[PIL_LINESIZE];
   char errmsg[MAXLEN_ERR];
   char ext[5];
   char *cptr;

   if ( !task->use_outfile ) return(*status);

   /*
    * Get output file root name
    */
   if ( (*status = PILGetString("outfileroot", strval)) ) {
      sprintf(errmsg, "Error reading the 'outfileroot' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   param->outfileroot = stralloc(strval);
   /*
    * Get output file
    */
   if ( (*status = PILGetString("outfile", strval)) ) {
      sprintf(errmsg, "Error getting 'outfile' parameter");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   fits_compare_str("default", strval, CASEINSEN, &match, &exact);

   if ( match ) { /* outfile is "default" */

      fits_compare_str("default*", param->outfileroot, CASEINSEN, 
                       &match, &exact);

      if ( match ) { /* outfileroot is "default*" */

         /* Use first input file */
         strcpy(strval, param->series[0]->files[0]->name);

         /* Strip off directory and extension */
         cptr = strval + strlen(strval) - 1;
         while ( cptr != strval && *cptr != '/' ) {
            if ( *cptr == '.' ) *cptr = '\0';
            cptr--;
         }
         if ( *cptr == '/' ) cptr++;
            
      } else { /* outfileroot not "default*" */

         cptr = param->outfileroot;

      }

   } else {

      cptr = strval;

   }

   if ( *cptr == ' ' || !*cptr ) {  /* if space, assume no outfile */

      strval[0] = '\0';
      param->outfile = stralloc(strval);

   } else if ( strchr(cptr, '.') ) { 

      param->outfile = stralloc(cptr);

   } else {  /* if no '.' add extension */

      sprintf(ext, ".f%2.2s", task->code); 
      param->outfile = strcatalloc(cptr, ext);

   }

   if ( *param->outfile ) {
      headas_chat(VERBOSE, "Using output file name: %s\n", param->outfile);
   } else {
      headas_chat(VERBOSE, "No output file name\n");
   }
   
   return(*status);
}
