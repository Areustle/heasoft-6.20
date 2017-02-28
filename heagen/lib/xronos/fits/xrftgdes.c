/*
 * xrftgdes.c
 */
#include "xronos.h"

int xrftgdes(fitsfile *fptr, XRFile *file, int *status) {
/*
 * Get DEScriptive keywords from a XRonos FiTs file
 *
 *  I  fptr  - Pointer to open fits file (Current HDU changes in routine)
 *  I  file  - Contains properties of file
 *  O  status - Error flag (0=OK)
 *
 * Note: status will always be 0, as found keywords get set and 
 *       keywords that are not found are assigned a blank string.  
 *       No fatal errors can be encountered in this routine.
 */
   int i, ftstat;
   char keyval[FLEN_VALUE];

   static char *rastr_list[] = { "RA", "RA_OBJ", "RA_SRC", "RA_NOM", "" };
   static char *decstr_list[] = { "DEC", "DEC_OBJ", "DEC_SRC", "DEC_NOM", "" };

   /* Mark CFITSIO errors from this routine for later clearing */
   fits_write_errmark();

   /* OBJECT */
   ftstat = 0;
   if ( fits_read_key_str(fptr, "OBJECT", keyval, NULL, &ftstat) ) 
      keyval[0] = '\0';
   file->object = stralloc(keyval);

   /* EXTNAME */
   ftstat = 0;
   if ( fits_read_key_str(fptr, "EXTNAME", keyval, NULL, &ftstat) )
      keyval[0] = '\0';
   file->extname = stralloc(keyval);

   /* RA */
   i = 0;
   keyval[0] = '\0';
   while ( !*keyval && *rastr_list[i] ) {
      ftstat = 0;
      if ( fits_read_key_str(fptr, rastr_list[i], keyval, NULL, &ftstat) )
         keyval[0] = '\0';
      i++;
   }
   file->rastr = stralloc(keyval);

   /* DEC */
   i = 0;
   keyval[0] = '\0';
   while ( !*keyval && *decstr_list[i] ) {
      ftstat = 0;
      if ( fits_read_key_str(fptr, decstr_list[i], keyval, NULL, &ftstat) )
         keyval[0] = '\0';
      i++;
   }
   file->decstr = stralloc(keyval);

   /* TELESCOP */
   ftstat = 0;
   if ( fits_read_key_str(fptr, "TELESCOP", keyval, NULL, &ftstat) )
      keyval[0] = '\0';
   file->telescop = stralloc(keyval);

   /* INSTRUME */
   ftstat = 0;
   if ( fits_read_key_str(fptr, "INSTRUME", keyval, NULL, &ftstat) )
      keyval[0] = '\0';
   file->instrume = stralloc(keyval);

   /* DETNAME */
   ftstat = 0;
   if ( fits_read_key_str(fptr, "DETNAME", keyval, NULL, &ftstat) )
      keyval[0] = '\0';
   file->detname = stralloc(keyval);

   /* FILTER */
   ftstat = 0;
   if ( fits_read_key_str(fptr, "FILTER", keyval, NULL, &ftstat) )
      keyval[0] = '\0';
   file->filter = stralloc(keyval);

   /* Clear CFITSIO errors from this routine */
   fits_clear_errmark();

   return(*status);
}
