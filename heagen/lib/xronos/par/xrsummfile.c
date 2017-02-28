/*
 * xrsummfile.c
 */
#include <string.h>
#include "xronos.h"

int xrsummfile(XRFile *file, int *status) {
/*
 * WRite Xronos Fits file summary information 
 *
 *  I  file  - Contains properties of file
 *  O  status - Error flag (0=OK)
 */
   int ftstat;
   double gtista, gtisto;
   char buff1[100], buff2[100];

   /* Banner line: selected extensions */
   headas_chat(NORMAL, "\n"); /* Separating space */
   headas_chat(NORMAL, " Selected FITS extensions: ");
   headas_chat(NORMAL, "%d - RATE TABLE; ", file->ext);
   if ( file->gtiext ) {
      headas_chat(NORMAL, "%d - GTIS; ", file->gtiext);
   }
   headas_chat(NORMAL, "\n\n");

   /* Line 1: OBJECT and Start Time */
   headas_chat(NORMAL, " Source ............ %-18s", file->object);
   if ( file->timesys == TIMESYS_REF || file->timesys == TIMESYS_NONE ) {
      headas_chat(NORMAL, " Start Time (d) .... ");
   } else {
      headas_chat(NORMAL, " TJD Start Time .... ");
   }
   ftstat = 0;
   if ( xrdaytostr(file->dtsta, 3, buff1, &ftstat) ) buff1[0] = '\0';
   headas_chat(NORMAL, "%-18s\n", buff1);

   /* Line 2: FITS extension name and number, and Stop Time */
   sprintf(buff1, "%d - %s", file->ext, file->extname);
   headas_chat(NORMAL, " FITS Extension .... %-18s", buff1);

   if ( file->timesys == TIMESYS_REF || file->timesys == TIMESYS_NONE ) {
      headas_chat(NORMAL, " Stop Time (d) ..... ");
   } else {
      headas_chat(NORMAL, " TJD Stop Time ..... ");
   }
   ftstat = 0;
   if ( xrdaytostr(file->dtsto, 3, buff1, &ftstat) ) buff1[0] = '\0';
   headas_chat(NORMAL, "%-18s\n", buff1);

   /* Line 3: Number of FITS rows and integration time */
   sprintf(buff1, "%ld", file->lrow - file->frow + 1);
   if ( file->dtint > 0.0 ) {
      sprintf(buff2, "%-12.4g", file->dtint);
   } else {
      strcpy(buff2, "none");
   }
   headas_chat(NORMAL, 
 " No. of Rows ....... %-18s Bin Time (s) ...... %-18s\n", buff1, buff2);

   /* Line 4: RA and TIMESYS */
   headas_chat(NORMAL, " Right Ascension ... %-18s", file->rastr);
   if ( file->timesys == TIMESYS_NONE ) {
      strcpy(buff1, "Literal");
   } else {
      strcpy(buff1, "Converted to TJD");
   }
   headas_chat(NORMAL, " Internal time sys.. %-18s\n", buff1);

   /* Line 5: DEC and TELESCOP/INSTRUME */
   headas_chat(NORMAL, " Declination ....... %-18s", file->decstr);
   sprintf(buff1, "%s %s", file->telescop, file->instrume);
   headas_chat(NORMAL, " Experiment ........ %-18s\n", buff1);

   /* Line 6: DETNAME and/or FILTER, if present. */
   if ( *file->detname || *file->filter ) {
      headas_chat(NORMAL, " Detector .......... %-18s", file->detname);
      headas_chat(NORMAL, " Filter ............ %-18s\n", file->filter);
   }
   headas_chat(NORMAL, "\n");

   /* Line 7: Corrections */
   headas_chat(NORMAL, 
" Corrections applied: Vignetting - %3s; Deadtime - %3s; Bkgd - %3s; Clock - %3s\n",
 (file->vignapp ? "Yes" : "No"), (file->deadapp ? "Yes" : "No"),
 (file->backapp ? "Yes" : "No"), (file->clockapp ? "Yes" : "No"));
   if ( file->vignapp || file->deadapp || file->backapp ) {
      headas_chat(NORMAL,
"              values: %-14.9g    %-14.9g  %-14.9g\n", 
                file->vignet, file->deadc, file->backv);
   }
   headas_chat(NORMAL, "\n");
   
   /* Line 8: Selected columns */
   headas_chat(NORMAL, " Selected Columns: ");
   if ( file->col_time ) {
      headas_chat(NORMAL, "%d - Time; ", file->col_time);
   }
   if ( file->col_rate ) {
      headas_chat(NORMAL, "%d - Y-axis; ", file->col_rate);
   }
   if ( file->col_err ) {
      headas_chat(NORMAL, "%d - Y-error; ", file->col_err);
   }
   if ( file->col_deadc ) {
      headas_chat(NORMAL, "%d - Dead Time; ", file->col_deadc);
   }
   if ( file->col_timedel ) {
      headas_chat(NORMAL, "%d - Delta Time; ", file->col_timedel);
   }
   if ( file->col_fracexp ) {
      headas_chat(NORMAL, "%d - Fractional exposure; ", file->col_fracexp);
   }
   if ( file->col_pha ) {
      headas_chat(NORMAL, "%d - E-Channel; ", file->col_pha);
   }

   /* Line 9: blank */
   headas_chat(NORMAL, "\n\n");

   /* Line 10: Type of file */
   if ( file->type == EVENT_DATA ) {
      headas_chat(NORMAL, " File contains arrival-time data.");
   } else if ( file->type == RATE_DATA ) {
      headas_chat(NORMAL, " File contains binned data.");
   } else {
      headas_chat(NORMAL, " File data is of unknown type.");
   }
      
   /* Line 11: blank */
   headas_chat(NORMAL, "\n\n");

   /* If no GTI, done... */
   if ( !file->gtiext ) return(*status);

   /* GTI Info */
   /* Line 1: Number and name of extension, and Start Time */
   headas_chat(NORMAL, " FITS Extension ....  ");
   sprintf(buff1, "%d - %-12s", file->gtiext, file->gtiname);
   headas_chat(NORMAL, "%-18s", buff1);
   headas_chat(NORMAL, " First GTI Start ...  ");
   gtista = file->gtista;
   if ( xrdaytostr(gtista, 3, buff1, &ftstat) ) buff1[0] = '\0';
   headas_chat(NORMAL, "%-18s\n", buff1);

   /* Line 2: FITS # of rows and Stop Time */
   headas_chat(NORMAL, " No. of Rows .......  ");
   sprintf(buff1, "%ld", file->ngtis);
   headas_chat(NORMAL, "%-18s", buff1);
   headas_chat(NORMAL, " Last GTI Stop .....  ");
   gtisto = file->gtisto;
   if ( xrdaytostr(gtisto, 3, buff1, &ftstat) ) buff1[0] = '\0';
   headas_chat(NORMAL, "%-18s\n", buff1);

   /* Line 3: Selected columns */
   headas_chat(NORMAL, 
      " Selected columns: %d - GTI Start; %d - GTI Stop\n", 
      file->col_gtista, file->col_gtisto);

   headas_chat(NORMAL, "\n\n");

   return(*status);
}
