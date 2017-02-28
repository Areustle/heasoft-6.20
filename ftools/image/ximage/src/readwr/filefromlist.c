/*
 *  FORTRAN Usage:
 *
 *  call filefromlist(list, buffer, status)
 *
 *  list   I (c) Expression of a file list
 *               @filelist.txt or file1.fits,file2.fits,file3.fits
 *  buffer O (c) Where next filename is copied
 *  buflen I (i) Length of buffer
 *  status O (i) Error flag (0=OK)
 *  
 *  First execution, list should have some value.  Subsequent calls
 *   should use '' for list until buffer is returned as ''
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include "fitsio.h"
#include "cfortran.h"
#include "../include/xcommon.h"

static char *listbuf = NULL;

void filefromlist(char *list, char *buffer, int buflen, int *status) {
/*
 *  Gets next file from file list
 *
 *  I  list      (c)  File list expressed as @filelist.txt or
 *                     file1.fits,file2.fits,file3.fits
 *  O  buffer    (c)  Where next filename is copied
 *  I  buflen    (i)  Length of buffer
 *  O  status    (i)  Error flag (0=OK)
 *  
 *  First execution, list should have some non-null value.  
 *  Subsequent calls should use pointer to a null for list 
 *  until buffer is returned as empty
 */
   char *curfile;

/*
 *  If called with a bad status, zero out state and return
 */
   if ( *status && listbuf ) {
      strcpy(buffer,"");
      free(listbuf);
      listbuf = NULL;
      return;
   }

   if ( list && *list ) {  /* Initial run */
      if ( listbuf ) {
         cxwrite("filefromlist: Attempted to initialize while still in use", 10);
         *status = -1;
         return;
      }
      if ( *list == '@' ) {
         if ( ffimport_file( list+1, &listbuf, status) ) return;
      } else {
         listbuf = stralloc(list);
      }
      curfile = fits_split_names(listbuf);
   } else {
      if ( !listbuf ) {
         cxwrite("filefromlist: Not initialized", 10);
         *status = -1;
         return;
      }
      curfile = fits_split_names(NULL);
   }
   if ( !curfile ) {
      strcpy(buffer,"");
      free(listbuf);
      listbuf = NULL;
      return;
   }
   if ( strlen(curfile) > buflen ) {
      cxwrite("filefromlist: Buffer exceeded", 10);
      *status = -1;
      return;
   }
   strcpy(buffer, curfile);

   return;
}
FCALLSCSUB4(filefromlist,FILEFROMLIST,filefromlist,STRING,PSTRING,INT,PINT)
