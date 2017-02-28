/*
 * xrgetfiles.c
 */
#include <stdlib.h>
#include <string.h>
#include "pil.h"
#include "xronos.h"

/*
 *  Local helper function prototypes (implemented at end)
 */
int xrstorefilenames(XRSeries *files, char **list, int ibeg, int iend, 
                     int *status);

int xrgetfiles(XRTask *task, XRParam *param, int *status) {
/*
 *  Get filenames for indicated number of series (param->nser).
 *  If input begins with '@', indicates a text file containing filenames.
 *  In such a text file, a line with '///' separates multiple series.
 *
 *  I  task   - Contains properties of task
 *  O  param  - Contains user input parameters
 *  O  status - Error flag (0 = OK)
 */
   char errmsg[MAXLEN_ERR];
   char strval[PIL_LINESIZE];
   char parname[PIL_LINESIZE];
   char **filelist;
   int nitems, j;
   int iseries, seriesbeg, seriesend;

   /*
    *  No limit to param->nser
    *  'cfilen' parameter is used for all cfile# parameters that
    *  are not present in the par file.  
    *  Only limitation: In order to learn
    *  the queried input, par file must explicitly contain a line
    *  with a cfile# (e.g. 'cfile6', 'cfile13'). 
    */
   iseries = 1;
   while ( iseries <= param->nser ) {
      sprintf(parname, "cfile%d", iseries);
      if ( (*status = PILGetFname(parname, strval)) ) {
         if ( PIL_NOT_FOUND == *status ) {
            headas_chat(VERBOSE, "Could not find '%s' parameter\n", parname);
            if ( (*status = PILGetFname("cfilen", strval)) ) {
               sprintf(errmsg, "Error reading a 'cfile' parameter\n");
               HD_ERROR_THROW(errmsg, *status);
               return(*status);
            }
         } else {
            sprintf(errmsg, "Error reading the '%s' parameter", parname);
            HD_ERROR_THROW(errmsg, *status);
            return(*status);
         }
      }
      /*
       *  expand_item_list (headas_utils.h)
       *  Supports: @filelist.txt
       *            file1,file2,file3
       */
      filelist = expand_item_list(strval, &nitems, ',', 1, 1, 1, status);
      if ( *status != 0 ) {
         sprintf(errmsg, "Failed to parse input file '%s'", strval);
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      seriesbeg = 0;
      seriesend = -1;
      j = 0;
      while ( j < nitems && iseries <= param->nser ) {
         if ( strncmp(filelist[j], "///", 3) == 0 ) {
            xrstorefilenames(param->series[iseries-1], filelist, seriesbeg,
                             seriesend, status);
            iseries++;
            seriesbeg = j+1;
         } else {
            seriesend = j;
         }
         j++;
      }
      if ( iseries <= param->nser ) {
         xrstorefilenames(param->series[iseries-1], filelist, seriesbeg,
                          seriesend, status);
      }
      free(filelist);

      iseries++; /* next series */
   }
   return(*status);
}

int xrstorefilenames(XRSeries *series, char **list, int ibeg, int iend, 
                     int *status) {
/*
 *  Given an array of filenames and a range of indexes, 
 *  allocate a new array and copy the indicated strings
 *  and set in XRSeries data structure
 *
 *  I  series - XRSeries data structure to update
 *  I  list   - array of strings (e.g. return from expand_item_list)
 *  I  ibeg   - index of string to begin copy 
 *  I  iend   - index of string to end copy 
 *  O  status - Error flag (0 = OK)
 */
   int nfiles, i, j;
   char errmsg[MAXLEN_ERR];
   XRFile **files;
   XRFile *fileary;

   nfiles = iend - ibeg + 1;

   if ( nfiles <= 0 ) {
      *status = -1;
      sprintf(errmsg, "Invalid bounds in filename list");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   files = (XRFile **) calloc(nfiles, sizeof(XRFile *));
   fileary = (XRFile *) calloc(nfiles, sizeof(XRFile));
   if ( !files || !fileary ) {
      *status = MEMORY_ALLOCATION;
      sprintf(errmsg, "Failed to allocate space for input files");
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }
   /*
    *  Maintain access to structs via '->' operator
    *  files is array of ptrs to XRFile structs
    */
   for ( i = 0; i < nfiles; i++ ) {
      files[i] = &fileary[i];
      files[i]->id = i+1;
   }

   i = 0;
   for ( j = ibeg; j <= iend; j++ ) {
      xrparseopt(list[j], files[i], status);
      i++;
   }
   series->mxfiles = nfiles;
   series->nfiles = nfiles;
   series->files = files;

   return(*status);
}
