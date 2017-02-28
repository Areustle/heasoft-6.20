/*
 * xrbinseries.c
 */
#include <math.h>
#include "xronos.h"

int xrbinseries(XRBinState *bin, XRSeries *series, XRSeriesData *seriesdata, 
                int *status) {
/*
 *  Fill series data by binning input files
 *
 *  I  bin        - Contains binning state
 *  I  series     - Contains series inputs
 *  O  seriesdata - Contains output series data 
 *  O  status     - Error flag (0=OK)
 */
   bool leftover;
   double cursta, cursto;
   XRFITSBuffer databufstor, expobufstor;
   XRFITSBuffer *databuf, *expobuf; 
   XRDataPoint datapntstor = { 0 };
   XRDataPoint nextdatapntstor = { 0 };
   XRDataPoint *datapnt, *nextdatapnt;
   XRExpoPoint expopntstor = { 0 };
   XRExpoPoint lastexpopntstor = { 0 };
   XRExpoPoint *expopnt, *lastexpopnt;

   databuf = &databufstor;  /* Always use -> syntax to access */
   expobuf = &expobufstor;
   datapnt = &datapntstor;
   nextdatapnt = &nextdatapntstor;
   expopnt = &expopntstor;
   lastexpopnt = &lastexpopntstor;

   /* Initializations */
   cursta = series->dtsta;
   cursto = cursta;
   expopnt->valid = FALSE;
   datapnt->valid = FALSE;
   nextdatapnt->valid = FALSE;
   leftover = FALSE;

   xrftbufinit(databuf, series->type, series->nfiles, series->files, status);
   xrftbufinit(expobuf, GTI_DATA, series->nfiles, series->files, status);

   /* Get initial data point */
   xrftbufgetpnt(databuf, nextdatapnt, status);
   if ( *status != 0 ) return(*status);

   /* Loop until FITS buffers are exhausted */
   while ( !databuf->end || !expobuf->end ) {

      /* If needed, get exposure point */
      if ( !expopnt->valid || 
           cursta > expopnt->timesta + expopnt->duration ||
           fabs(cursta - expopnt->timesta - expopnt->duration) < EPSILON ) {

         xrftbufgetpnt(expobuf, expopnt, status);
         if ( *status != 0 ) return(*status);
      }
      /* If needed, get data point */
      if ( !datapnt->valid || !leftover ) {

         /* Copy next data point to current */
         datapnt->valid  = nextdatapnt->valid;
         datapnt->null   = nextdatapnt->null;
         datapnt->event  = nextdatapnt->event;
         datapnt->y      = nextdatapnt->y;
         datapnt->sy     = nextdatapnt->sy;
         datapnt->expo   = nextdatapnt->expo;
         datapnt->expcor = nextdatapnt->expcor;
         datapnt->time   = nextdatapnt->time;

         /* Get next data point */
         xrftbufgetpnt(databuf, nextdatapnt, status);
         if ( *status != 0 ) return(*status);

         /* Collapse events at same time into one data point */
         while ( !databuf->end && 
                 datapnt->valid && nextdatapnt->valid &&
                 datapnt->time == nextdatapnt->time ) {
            datapnt->y = datapnt->y + nextdatapnt->y;
            xrftbufgetpnt(databuf, nextdatapnt, status);
            if ( *status != 0 ) return(*status);
         }
      }
      /* Find relevant time range to be binned */

      if ( !datapnt->valid && !nextdatapnt->valid ) {
         /*
          * If no data info, use end of exposure point
          */
         if ( !expopnt->valid ) break;
         cursto = expopnt->timesta + expopnt->duration;

      } else if ( !expopnt->valid ) {
         /*
          * If no exposure info, use end of data point
          */
         if ( !datapnt->valid ) break;
         cursto = datapnt->time + datapnt->expo/2./86400.;

      } else if ( expopnt->timesta + expopnt->duration <
                  datapnt->time + datapnt->expo/2./86400. ) {
         /*
          * If end of exposure point comes before end of
          * current data point, use edge of exposure point
          */
         cursto = expopnt->timesta + expopnt->duration;

      } else if ( datapnt->event ) {
         /*
          * For Events
          */
         if ( !nextdatapnt->valid || 
              expopnt->timesta + expopnt->duration <
              nextdatapnt->time - nextdatapnt->expo/2./86400. ) {
            /*
             * If edge of exposure point comes before next data point,
             * use edge of exposure point
             */
            cursto = expopnt->timesta + expopnt->duration;

         } else {
            /*
             * Otherwise, use midpoint between current and 
             * next data point
             */
             cursto = (datapnt->time + nextdatapnt->time)/2.;
         }

      } else {
         /*
          *  If no other constraints, use end of data point
          */
         cursto = datapnt->time + datapnt->expo/2./86400.;

      }
      /* Crop at start of exposure point */
      if ( expopnt->timesta > cursta ) {
         cursta = expopnt->timesta;
      }

      /* Bin portion of timeline */
      xrbinpnt(bin, datapnt, expopnt, &cursta, &cursto, &leftover,
               seriesdata, status);
   }
   /* Add last bin */
   xraddnewbin(bin, seriesdata, status);

   xrftbufclean(databuf, status);
   xrftbufclean(expobuf, status);

   return(*status);
}
