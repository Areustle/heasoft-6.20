/*
 * xrftgcol.c
 */
#include "xronos.h"

int xrftgcol(fitsfile *fptr, XRFile *file, int *status) {
/*
 * Looks at column header keywords to find a match
 * with various strings.  The columns it searches for are:
 *
 *   Quantity               Search string        
 *-----------------------------------------------
 *   Time                   'TIME'               
 *   Rate                   'RATE' or 'COUNT'    
 *   Error                  any substring 'ERR'  
 *   Dead time              'DEADC'              
 *   Integration time       'TIMEDEL'            
 *   Fractional exposure    'FRACEXP'            
 *   PHA                    'PHA'                
 *
 * As an added requirement on the (unitless) fractional exposure the
 * routine checks that the value of the TUNIT keyword for that column is 
 *  blank, = 'none' or does not exist.
 *
 * These defaults can be overridden by the options VX (time)
 * VY (rate), VS (error), VE (dead time), VC (PHA). 
 *
 *  I  fptr  - Pointer to open fits file (current HDU set to data)
 * I/O file  - Contains properties of file
 *  O  status - Error flag (0=OK)
 *
 *  Note: status will always be 0, as found columns get set to their
 *        index and columns that are not found are 0.  No fatal errors
 *        can be encountered in this routine.
 */
   int idum, match, exact;
   char keybuff[FLEN_VALUE];
   char *keyval;
   keyval = keybuff;

   /* Mark CFITSIO errors from this routine for later clearing */
   fits_write_errmark();

   /* Time column*/
   if ( file->VX.set ) {
      file->col_time = file->VX.value;
   } else {
      fits_get_colnum(fptr, CASEINSEN, "TIME", &file->col_time, status);
   }

   /* Rate column (not applicable to event data) */
   if ( file->type == EVENT_DATA ) {
      file->col_rate = 0;
   } else if ( file->VY.set ) {
      file->col_rate = file->VY.value;
   } else {
      *status = 0;
      fits_get_colnum(fptr, CASEINSEN, "RATE*", &file->col_rate, status);
      if ( *status == COL_NOT_FOUND ) {
         *status = 0;
         fits_get_colnum(fptr, CASEINSEN, "COUNT*", &file->col_rate, 
                         status);
      }
   }

   /* Error column (not applicable to event data) */
   if ( file->type == EVENT_DATA ) {
      file->col_err = 0;
   } else if ( file->VS.set ) {
      file->col_err = file->VS.value;
   } else {
      *status = 0;
      fits_get_colnum(fptr, CASEINSEN, "*ERR*", &file->col_err, status);
   }

   /* Dead time correction column (not applicable to event data) */
   if ( file->type == EVENT_DATA ) {
      file->col_deadc = 0;
   } else if ( file->VE.set ) {
      file->col_deadc = file->VE.value;
   } else {
      *status = 0;
      fits_get_colnum(fptr, CASEINSEN, "DEADC*", &file->col_deadc, 
                      status);
   }

   /* Integration time column (TIMEDEL) */
   *status = 0;
   fits_get_colnum(fptr, CASEINSEN, "TIMEDEL*", &file->col_timedel, 
                   status);

   /* FRACEXP column */
   *status = 0;
   fits_get_colnum(fptr, CASEINSEN, "FRACEXP*", &file->col_fracexp, 
                   status);
   /* If FRACEXP exists, ignore DEADC */
   if ( file->col_fracexp > 0 ) file->col_deadc = 0;

   if ( file->col_fracexp ) {
      *status = 0;
      /* If fracexp column has blank or 'NONE' for TUNIT, ignore */
      keyval[0] = '\0';
      fits_read_keys_str(fptr, "TUNIT", file->col_fracexp, 1, &keyval,
                         &idum, status);
      fits_compare_str("NONE", keyval, CASEINSEN, &match, &exact);
      if ( *keyval && !match ) file->col_fracexp = 0;
   }
   
   /* PHA column */
   if ( file->VC.set ) {
      file->col_pha = file->VC.value;
   } else {
      *status = 0;
      fits_get_colnum(fptr, CASEINSEN, "PHA*", &file->col_pha, status);
      if ( *status == 0 ) { /* PHAS not valid */
         fits_get_colnum(fptr, CASEINSEN, "PHAS", &idum, status);
         if ( *status == 0 ) { file->col_pha = 0; }
      }
   }
   *status = 0;
 
   /* Clear CFITSIO errors from this routine */
   fits_clear_errmark();

   return(*status);
}
