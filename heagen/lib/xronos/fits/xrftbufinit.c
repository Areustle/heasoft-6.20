/*
 * xrftbufinit.c
 */
#include "xronos.h"

int xrftbufinit(XRFITSBuffer *ftbuff, int type, int nfiles, 
                XRFile **files, int *status) {
/*
 * Initialize FITS data buffer 
 *
 * I/O ftbuff   - FITS data buffer
 *  I  type     - Type of FITS data
 *  I  nfiles   - Number of files
 *  I  files    - List of files
 *  O  status   - Error flag (0=OK)
 */

   ftbuff->type = type;
   ftbuff->nfiles = nfiles;
   ftbuff->files = files;
   ftbuff->ifile = 0;
   ftbuff->file = NULL;
   ftbuff->fptr = NULL;
   ftbuff->end = FALSE;

   return(*status);
}
