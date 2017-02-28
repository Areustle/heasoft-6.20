/*
 * xrftgext.c
 */
#include "xronos.h"

int xrftgext(fitsfile *fptr, XRFile *file, int *status) {
/*
 * If extension has been specified in the RT file option,
 * The routine only tries to determine the type of data
 * in that extension (as well as checking that fitsio can move to it).
 *
 * Otherwise, searches for the rate table and gti extensions
 * and determines type of data: *EVENT_DATA if the string 'EVENT' 
 * shows up in one of the EXTNAME or HDUCLAS.  Otherwise the routine 
 * assumes RATE_DATA (binned light curve).
 *
 * If no extension is found, the routine defaults to the first
 * extension
 *
 *  I  fptr  - Pointer to open fits file 
 * I/O file  - Contains properties of file
 *  O  status - Error flag (0=OK)
 */
   int i, hdutype, match, exact, extnum, ftstat;
   char errmsg[MAXLEN_ERR];
   char keyval[FLEN_VALUE];

   /* Templates used to search for data */
   /* Terminated by "" string */
   static char *hduclas1[] = {"EVENT","EVENTS","LIGHTCURVE",""};
   static char *hduclas2[] = {"TOTAL","NET","BKG",""};
   static char *hduclas3[] = {"COUNTS","COUNT","RATE","FLUX",""};
   static char *extname[]  = {"EVENT","EVENTS","COUNTS","COUNT","RATE",""};
   static char *gtiname[]  = {"GTI","ALLGTI","STDGTI",""};

   /*
    *  Assuming RATE_DATA unless EVENT extension is found
    */
   file->type = RATE_DATA;
   /*
    *  If extension specified, verify
    */
   if ( file->RT.set ) {
      fits_movabs_hdu(fptr, (file->RT.value)+1, &hdutype, status);
      if ( hdutype != BINARY_TBL ) *status = -1;
      if ( *status != 0 ) {
         sprintf(errmsg, "Error in specified FITS extension %d",
                 file->RT.value);
         HD_ERROR_THROW(errmsg, *status);
         return(*status);
      }
      file->ext = file->RT.value;
      /*
       * Determine data type
       */
      ftstat = 0;
      if ( fits_read_key_str(fptr, "HDUCLAS1", keyval, NULL, &ftstat) )
         keyval[0] = '\0';
      fits_compare_str("EVENT*", keyval, CASEINSEN, &match, &exact);
      if ( match ) {
         file->type = EVENT_DATA;
      } else {
         ftstat = 0;
         if ( fits_read_key_str(fptr, "EXTNAME", keyval, NULL, &ftstat) )
            keyval[0] = '\0';
         fits_compare_str("EVENT*", keyval, CASEINSEN, &match, &exact);
         if ( match ) file->type = EVENT_DATA;
      }
   }

   /*
    * Loop over extensions.  This loop stops when it finds a match or
    * when it gets to the end of the file.
    */
   ftstat = 0;
   extnum = 1;
   while ( file->ext == 0 || file->gtiext == 0 ) {

      fits_movabs_hdu(fptr, extnum+1, &hdutype, &ftstat);
      if ( ftstat != 0 ) break;
      /*
       *  Test against the templates defined above to find data
       */

      /* HDUCLAS1 */
      if ( file->ext == 0 || file->gtiext == 0 ) {
         if ( fits_read_key_str(fptr, "HDUCLAS1", keyval, NULL, &ftstat)) {
            ftstat = 0;
            keyval[0] = '\0';
         }
         i = 0;
         while ( file->ext == 0 && *hduclas1[i] ) {
            fits_compare_str(hduclas1[i], keyval, CASEINSEN, &match, &exact);
            if ( match ) { 
               file->ext = extnum;
               fits_compare_str("EVENT*", keyval, CASEINSEN, &match, &exact);
               if ( match ) file->type = EVENT_DATA;
            }
            i++;
         }
         i = 0;
         while ( file->gtiext == 0 && *gtiname[i] ) {
            fits_compare_str(gtiname[i], keyval, CASEINSEN, &match, &exact);
            if ( match ) file->gtiext = extnum;
            i++;
         }
      }

      /* HDUCLAS2 */
      if ( file->ext == 0 ) {
         if ( fits_read_key_str(fptr, "HDUCLAS2", keyval, NULL, &ftstat)) {
            ftstat = 0;
            keyval[0] = '\0';
         }
         i = 0;
         while ( file->ext == 0 && *hduclas2[i] ) {
            fits_compare_str(hduclas2[i], keyval, CASEINSEN, &match, &exact);
            if ( match ) {
               file->ext = extnum;
               fits_compare_str("EVENT*", keyval, CASEINSEN, &match, &exact);
               if ( match ) file->type = EVENT_DATA;
            }
            i++;
         }
      }

      /* HDUCLAS3 */
      if ( file->ext == 0 ) {
         if ( fits_read_key_str(fptr, "HDUCLAS3", keyval, NULL, &ftstat)) {
            ftstat = 0;
            keyval[0] = '\0';
         }
         i = 0;
         while ( file->ext == 0 && *hduclas3[i] ) {
            fits_compare_str(hduclas3[i], keyval, CASEINSEN, &match, &exact);
            if ( match ) {
               file->ext = extnum;
               fits_compare_str("EVENT*", keyval, CASEINSEN, &match, &exact);
               if ( match ) file->type = EVENT_DATA;
            }
            i++;
         }
      }

      /* EXTNAME */
      if ( file->ext == 0 || file->gtiext == 0 ) {
         if ( fits_read_key_str(fptr, "EXTNAME", keyval, NULL, &ftstat)) {
            ftstat = 0;
            keyval[0] = '\0';
         }
         i = 0;
         while ( file->ext == 0 && *extname[i] ) {
            fits_compare_str(extname[i], keyval, CASEINSEN, &match, &exact);
            if ( match ) {
               file->ext = extnum;
               fits_compare_str("EVENT*", keyval, CASEINSEN, &match, &exact);
               if ( match ) file->type = EVENT_DATA;
            }
            i++;
         }
         i = 0;
         while ( file->gtiext == 0 && *gtiname[i] ) {
            fits_compare_str(gtiname[i], keyval, CASEINSEN, &match, &exact);
            if ( match ) file->gtiext = extnum;
            i++;
         }
      }
      extnum++;
   }
   if ( file->ext == 0 ) {
      headas_chat(TERSE, " Defaulting to first FITS extension");
      file->ext = 1;
   }
   /*
    *  If GTI specified, verify
    */
   if ( file->type == EVENT_DATA ) {
      if ( file->VE.set ) {
         if ( file->VE.value > 0 ) {
            fits_movabs_hdu(fptr, (file->VE.value)+1, &hdutype, status);
            if ( hdutype != BINARY_TBL ) *status = -1;
            if ( *status != 0 ) {
               sprintf(errmsg, "Error in specified GTI extension %d",
                       file->VE.value);
               HD_ERROR_THROW(errmsg, *status);
               return(*status);
            }
            file->gtiext = file->VE.value;
         } else if ( file->VE.value == 0 ) {
            /*
             *  If VE option set to 0, ignore GTI
             */
            if ( file->VE.set && file->VE.value == 0 ) {
               file->gtiext = 0;
               if ( file->gtiname ) free(file->gtiname);
               file->gtiname = NULL;
            }
         }
      }
   } else {
      /* If RATE_DATA, ignore GTI */
      file->gtiext = 0;
   }
   /*
    *  Get EXTNAME for GTI extension
    */
   if ( file->gtiext ) {
      ftstat = 0;
      fits_movabs_hdu(fptr, (file->gtiext)+1, &hdutype, &ftstat);
      fits_read_key_str(fptr, "EXTNAME", keyval, NULL, &ftstat);
      if ( ftstat != 0 ) *keyval = '\0';
      file->gtiname = stralloc(keyval);
   }

   return(*status);
}
