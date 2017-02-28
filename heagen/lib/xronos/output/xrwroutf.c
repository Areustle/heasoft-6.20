/*
 * xrwroutf.c
 */
#include <math.h>
#include "xronos.h"

#define MAX_DECIMAL 16

int xrwroutf(XRTask *task, XRParam *param, XRFrame *frame, int *status) {
/*
 *  Write frame to single output FITS file
 *  (Only implementing one interval per extension format)
 *
 *  I  task   - Contains properties of task
 *  I  param  - Contains user inputs
 *  I  frame  - Contains calculated frame results
 *  O  status - Error flag (0=OK)
 */
   int i, colnum, ftstat;
   double fbintime;
   fitsfile *fptr;
   char errmsg[MAXLEN_ERR];
   XRInterval *intv;
   XRSeriesData *series;

   const int tfields = 5;
   static char *ttype[] = { "TIME", "XAX_E", "RATE1", "ERROR1", "FRACEXP"};
   static char *tform[] = {    "D",     "D",     "E",      "E",       "E"};
   static char *tunit[] = {    "s",      "", "count/s", "count/s",    "" };

   /* If no outfile, write nothing */
   if ( !*param->outfile ) return(*status);

   headas_chat(NORMAL, "Writing output file: %s\n", param->outfile);
   headas_clobberfile(param->outfile);

   fits_create_file(&fptr, param->outfile, status);
   if ( *status != 0 ) {
      sprintf(errmsg, "Couldn't open output file %s", param->outfile);
      HD_ERROR_THROW(errmsg, *status);
      return(*status);
   }

   /* Not fully implemented */
   /* Simple writer for testing, only write first interval */
   intv = frame->intvs[0];

   /* Only first series */
   series = intv->series[0];

   fits_insert_btbl(fptr, intv->nbins, tfields, ttype,
                    tform, tunit, "RATE", 0, status);

   /* TMP CONVERSION  correct method not implemented */
   fbintime = series->dtsta + series->expo[0]/2./86400.;
   for ( i = 0; i < series->nbint; i++ ) {
      /* clobber time array... converting to seconds */
      series->time[i] = (series->time[i] - fbintime)*86400.;
      if ( series->expcor[i] == 0.0 ) {
         series->y[i] = FLOATNULLVALUE;
         series->sy[i] = FLOATNULLVALUE;
         series->expcor[i] = FLOATNULLVALUE;
      } else {
         /* from counts to counts/sec (corrected) */
         series->y[i] = series->y[i]/series->expcor[i];
         series->sy[i] = series->sy[i]/series->expcor[i];
         /* from corrected seconds to fractional exposure */
         series->expcor[i] = series->expcor[i]/series->expo[i];
      }
      /* from exposure to error bar */
      series->expo[i] = series->expo[i]/2.0;
   }

   colnum = 1;
   fits_write_col_dbl(fptr, colnum, 1, 1, series->nbint, series->time, status);
   colnum++;
   fits_write_col_dbl(fptr, colnum, 1, 1, series->nbint, series->expo, status);
   colnum++;
   fits_write_colnull_flt(fptr, colnum, 1, 1, series->nbint, series->y, 
                          FLOATNULLVALUE, status);
   colnum++;
   fits_write_colnull_flt(fptr, colnum, 1, 1, series->nbint, series->sy,
                          FLOATNULLVALUE, status);
   colnum++;
   fits_write_colnull_flt(fptr, colnum, 1, 1, series->nbint, series->expcor,
                          FLOATNULLVALUE, status);

   ftstat = 0;

   /* Write keywords */
   fits_write_key_str(fptr, "CREATOR", task->name, "Xronos task name", status);
   fits_write_key_str(fptr, "HDUCLASS", "OGIP", NULL, status);
   fits_write_key_str(fptr, "HDUCLAS1", "LIGHT CURVE", NULL, status);
   fits_write_key_str(fptr, "HDUCLAS2", "TOTAL", NULL, status);
   fits_write_key_str(fptr, "HDUCLAS3", "RATE", NULL, status);
   fits_write_key_str(fptr, "CONTENT", "XRONOS OUTPUT", NULL, status);
   fits_write_key_str(fptr, "ORIGIN", "HEASARC/GSFC", NULL, status);
   fits_write_key_str(fptr, "TIMVERSN", "OGIP/93-003", NULL, status);
   fits_write_key_dbl(fptr, "TSTART", series->dtsta, MAX_DECIMAL, 
                      "Start time for this extension", status);
   fits_write_key_dbl(fptr, "TSTOP", series->dtsto, MAX_DECIMAL, 
                      "Stop time for this extension", status);
   fits_write_key_str(fptr, "TIMEUNIT", "d",
                      "Units for header timing keywords", status);
   fits_write_key_dbl(fptr, "TIMEZERO", fbintime, MAX_DECIMAL,
                      "Zero-point offset for TIME column", status);

   fits_close_file(fptr, &ftstat);
   
   
   return(*status);
}
