/*  
    Routines for initializing, utilizing and freeing CFITSIO
    regions in XIMAGE.

    Example Fortran Usage:
      
      CHARACTER*(*) REGFILE, WCSID
      INTEGER NUMREG, IREG, MODE, STATUS
c *1*
      REAL*8 XMIN, XMAX, YMIN, YMAX
      REAL*8 TXMIN, TXMAX, TYMIN, TYMAX
c *2*
      INTEGER COLOR, LWIDTH, LSTYLE, EXCOLOR, EXLWIDTH, EXLSTYLE
c *3*
      REAL*8 X, Y
      LOGICAL GOOD

c   Set WCS data for regions
      call SETREGWCS(WCSID, STATUS)

c   Initialize regions through file
      call XINITREG(REGFILE, NUMREG, STATUS)

c   Or create box regions internally
c   MODE:  0 = Start new region list, 1 = Append to region list
*     MODE = 0
*     call XBOXREG(MODE, XCEN, YCEN, XWID, YWID, ANGLE, NUMREG, STATUS)

c   Note: To get info on number of regions
*     call INFOREG(NUMREG)

c   Example Usage 1
c   ---------------
c   Find bounding box enclosing all regions in list

      TXMIN = +1e40
      TXMAX = -1e40
      TYMIN = +1e40
      TYMAX = -1e40
      DO IREG = 1, NUMREG
         call BBOXREG(IREG, XMIN, XMAX, YMIN, YMAX, STATUS)
         TXMIN = MIN(XMIN,TXMIN)
         TXMAX = MAX(XMAX,TXMAX)
         TYMIN = MIN(YMIN,TYMIN)
         TYMAX = MAX(YMAX,TYMAX)
      ENDDO
c   ---------------

c   Example Usage 2
c   ---------------
c   Plot regions

      DO IREG= 1, NUMREG
         call PLOTREG(IREG, COLOR, LWIDTH, LSTYLE, EXCOLOR, EXLWIDTH,
     &                EXLSTYLE, STATUS)
      ENDDO

c   Example Usage 3
c   ---------------
c   Loop through events
.
.
.

c   For each event, check if in any of the regions
      good = .FALSE.
      DO IREG = 1, NUMREG
         good = good.or.ISINREG(IREG, X, Y)
      ENDDO

c   Do something based on whether good or not, like construct image
c   ---------------

c   When done, free regions
      call XFREEREG(STATUS)
    
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "cpgplot.h"
#include "fitsio.h"
#include "region.h"
#include "cfortran.h"
#include "../include/maxvals.h"
#include "../include/null.h"
#include "../include/map.h"
#include "../include/wcsmanager.h"
#include "../include/astfits.h"
#include "../include/pgutil.h"
#include "../include/xcommon.h"

/* Required for xim_plot_region */

void jrncir(float, float, float, int, int, int);
void jrnbox(float, float, float, float, float, int, int, int);
void jrnpoly(float *, int, int, int, int);

static RgnWCS wcsbuf = { 0 };

static int nreglist = 0;          /* Number of regions                */
static int nregalloc = 0;         /* Number of region slots allocated */
static char **reglist = NULL;     /* Array of region filenames        */
static SAORegion **regobj = NULL; /* Array of region objects          */

const int chunkalloc = 10;        /* Chunk size to allocate           */

void setregwcs (char *wcsid, int *status) {
/* 
 * Set up for conversion needed by regions
 */
   RgnWCS *wcs;
   AstFrameSet *wcsinfo;
   AstFitsChan *fitschan;
   char mapid[MAX_IDSTR];
   char msg[100];
   int locstat;

   wcs = &wcsbuf;

   /* Set up conversion FrameSet */
   wcscnvbeg(wcsid, "SKY", "PIXEL", &wcs->cnvidx, status);
   if ( *status != 0 ) return;

   wcs->exists = 1;
   wcs->rot = 0.0;

   /* Extract CROTA2 from wcs information using internal header */
   locstat = 0;
   GETTMPMAP(mapid, locstat);
   if ( locstat != 0 ) {
      cxwrite(" Failed to set up region: no tmpmaps available", 10);
      *status = locstat;
      return;
   }
   wcstohdr(wcsid, mapid, status);
   if ( *status != 0 ) {
      cxwrite(" Failed to set up region: wcsid to header conversion", 10);
      return;
   }
   GHEADD(mapid, "CROTA2", wcs->rot, 0, locstat);
   if ( locstat != 0 ) {
      cxwrite(" Failed to set up region: internal header problem ", 10);
      *status = locstat;
      return;
   }
   FREETMPMAP(mapid, locstat);

   sprintf(msg, " Region interpreted using WCS rotation: %g\n", wcs->rot);
   cxwrite(msg, 20);
}
FCALLSCSUB2(setregwcs,SETREGWCS,setregwcs,STRING,PINT)

void frlist () {

   int i;

   for ( i = 0; i < nreglist; i++ ) {
      if ( reglist[i] ) free (reglist[i]);
      if ( regobj[i] )  xim_free_region(regobj[i]);
   }
   
   if ( !reglist ) free(reglist);
   reglist = NULL;
   if ( !regobj ) free(regobj);
   regobj = NULL;
   nreglist = 0;
   nregalloc = 0;
}

void inforeg (int *numreg) {
   *numreg = nreglist;
}
FCALLSCSUB1(inforeg,INFOREG,inforeg,PINT)


void xinitreg (char *regfile, int *numreg, int *status) {

   FILE *fp, *testfp;
   char linebuf[FLEN_FILENAME];
   char *linecpy, *begline, *endline;
   int i;
   char **tmplist;

   *status = 0;

   if ( reglist ) { frlist(); }
   cxwrite(regfile, 25);

   if ( regfile[0] == '@' ) {
      cxwrite(" Region list", 25);
/*
 *  Read region list, and build array of strings from it
 */
      fp = fopen (regfile+1, "r");
      if ( !fp ) {
         cxwrite(" Failed to open region list", 10);
         *status = -1;
         return;
      }

      nreglist = 0;
      reglist = (char **) malloc ((chunkalloc)*sizeof (char *));
      if ( !reglist ) {
         cxwrite(" Failed to allocate region list", 10);
         *status = -1;
         return;
      }
      nregalloc = chunkalloc;
      while ( fgets (linebuf, FLEN_FILENAME, fp) ) {

         begline = linebuf;  /* Strip spaces from beginning of filename */
         while ( isspace(*begline) ) {
            begline++;
         }
         if ( *begline ) {   /* If no non-space characters skip line */
            linecpy = stralloc(begline);
            if (! linecpy ) {
               cxwrite(" Failed to allocate for regionfile name", 10);
               *status = -1;
               frlist();
               return;
            }
            endline = linecpy;
            while ( !isspace(*endline) ) {
               endline++;
            }
            *endline = '\0';
            if ( nreglist+1 > nregalloc ) {
               tmplist = realloc(reglist, (nregalloc+chunkalloc)*sizeof(char *));
               if ( !tmplist ) {
                  cxwrite(" Failed to allocate for region list", 10);
                  frlist();
                  *status = -1;
                  return;
               }
               nregalloc += chunkalloc;
               reglist = tmplist;
            }
            reglist[nreglist] = linecpy;
            nreglist ++;
         }
      }
            
   } else {
/*
 *   Single region file
 */
      cxwrite(" Single region file", 25);
      reglist = (char **) malloc ((1)*sizeof (char *));
      if ( !reglist ) {
         cxwrite(" Failed to allocate region list of one", 10);
         *status = -1;
         return;
      }
      cxwrite(" Allocated one ptr in list", 25);
      linecpy = stralloc(regfile);
      if (! linecpy ) {
         cxwrite(" Failed to allocate for regionfile name", 10);
         *status = -1;
         frlist();
         return;
      }
      reglist[0] = linecpy;
      nreglist = 1;
   } 
      
/*
 *  Allocate space for SAORegion pointers
 */
   regobj = (SAORegion **) malloc (nreglist*(sizeof(SAORegion *)));
   if ( !regobj ) {
      cxwrite(" Failed to allocate SAOregion list", 10);
      *status = -1;
      frlist();
      return;
   }
   for ( i = 0; i < nreglist; i++ ) {
/*
 *  Check for region file existence.  If not there, add .reg
 */
      testfp = fopen (reglist[i], "r");
      if ( !testfp ) {
         sprintf(linebuf, "%s%s", reglist[i], ".reg");
         free (reglist[i]);
         reglist[i] = stralloc(linebuf);
      } else {
         fclose(testfp);
      }
      xim_read_rgnfile(reglist[i], &wcsbuf, &regobj[i], status);
      if ( *status ) {
          sprintf(linebuf, "Error reading region: %s", reglist[i]);
          cxwrite(linebuf, 10);
          regobj[i] = NULL;
      }
   }
   *numreg = nreglist;
}
FCALLSCSUB3(xinitreg,XINITREG,xinitreg,STRING,PINT,PINT)

void xboxreg(int mode, double xcen, double ycen, double xwid,
             double ywid, double angle, int *numreg, int *status) {
/*
 *  Create internal box region without reading region file
 *
 *  mode -> 0 = Clear all existing regions in list
 *          1 = Add to existing regions in list
 */
   char **tmplist;
   SAORegion **tmpobj, *newobj;
   RgnShape *newShape;
   
   if ( mode == 0 && reglist ) { frlist(); }

   /* Allocate space in region name and object arrays */

   if ( nreglist+1 > nregalloc ) {
      tmplist = realloc(reglist, (nregalloc+chunkalloc)*sizeof(char *));
      tmpobj = realloc(regobj, (nregalloc+chunkalloc)*sizeof(SAORegion *));
      if ( !tmplist || !tmpobj ) {
         cxwrite(" Failed to allocate for internal box region", 10);
         *status = -1;
         return;
      }
      nregalloc += chunkalloc;
      reglist = tmplist;
      regobj = tmpobj;
   }

   /* Allocate and set region SAORegion struct */

   newobj = (SAORegion *) malloc(sizeof(SAORegion));
   if ( !newobj ) {
      cxwrite(" Failed to allocate box region struct", 10);
      *status = -1;
      return;
   }
   newShape = (RgnShape *) malloc(sizeof(RgnShape));
   if ( !newShape ) {
      cxwrite(" Failed to allocate box region shape struct", 10);
      free (newobj);
      *status = -1;
      return;
   }
   newobj->nShapes = 1;
   newobj->Shapes = newShape;
   newobj->wcs = NULL;
   newShape->sign = 1;
   newShape->shape = box_rgn;
   newShape->param.gen.p[0] = xcen;
   newShape->param.gen.p[1] = ycen;
   newShape->param.gen.p[2] = xwid;
   newShape->param.gen.p[3] = ywid;
   newShape->param.gen.p[4] = angle;
   newShape->param.gen.sinT = sin( myPI * (angle / 180.0) );
   newShape->param.gen.cosT = cos( myPI * (angle / 180.0) );

   /* Add new region to arrays */

   regobj[nreglist] = newobj;
   reglist[nreglist] = stralloc("internalbox");
   nreglist ++;
   *numreg = nreglist;
}
FCALLSCSUB8(xboxreg,XBOXREG,xboxreg,INT,DOUBLE,DOUBLE,DOUBLE,DOUBLE,DOUBLE,PINT,PINT)

int isinreg(int ireg, double x, double y) {
/*
 * Returns whether x,y is in region #ireg
 * ireg goes from 1 to nreglist
 */
    if ( ireg < 1 || ireg > nreglist ) { 
       cxwrite(" ERROR: accessing out of region list bounds", 10);
       return(0); 
    }
    return (xim_in_region(x,y,regobj[ireg-1]));
}
FCALLSCFUN3(LOGICAL,isinreg,ISINREG,isinreg,INT,DOUBLE,DOUBLE)
      
void xfreereg(int *status) {

   RgnWCS *wcs;

   *status = 0;

   wcs = &wcsbuf;
   if ( wcs->exists ) wcscnvend(wcs->cnvidx);
   wcs->exists = 0;
   
   if ( !regobj || !reglist || !nreglist ) { *status = -1; }
   frlist ();
 
}
FCALLSCSUB1(xfreereg,XFREEREG,xfreereg,PINT)

void prlist (char **list, int nlist) {

   int i;

   for ( i = 0; i < nlist; i++ ) {
      printf ("%d - %s\n", i+1, list[i]);
   }
   
}

void prreglist () { prlist(reglist, nreglist); }
FCALLSCSUB0(prreglist,PRREGLIST,prreglist)

void xim_bbox_region(SAORegion *Rgn, double *xmin, double *xmax, 
                     double *ymin, double *ymax, int *status) {
/*
 *  Determines region's bounding box
 *  Use DNULL() values to indicate unbounded region
 *
 */
   RgnShape *Shapes;
   int i;
   double xcen, ycen, xwid, ywid, cosT, sinT;
   float xpg, ypg, rpg;
   float xpgmin=0.0, xpgmax=0.0, ypgmin=0.0, ypgmax=0.0;

   *status = 0;

   *xmin = +1e40;
   *xmax = -1e40;
   *ymin = +1e40;
   *ymax = -1e40;

   if ( !Rgn ) {
      *status = -1;
      return;
   }
   Shapes = Rgn->Shapes;

/*
 * If first region is excluded region, the region is not bounded
 */
   if ( Shapes->sign == 0 ) {
      *xmin = DNULL();
      *xmax = DNULL();
      *ymin = DNULL();
      *ymax = DNULL();
      return;
   }

   for ( i=0; i<Rgn->nShapes; i++, Shapes++ ) {

/* 
 * For now, we'll ignore Shapes->sign, as determining bounds of 
 * an intersection is nontrivial 
 */
      switch ( Shapes->shape ) {

         case box_rgn:
            xcen = Shapes->param.gen.p[0];
            ycen = Shapes->param.gen.p[1];
            xwid = Shapes->param.gen.p[2];
            ywid = Shapes->param.gen.p[3];
            sinT = Shapes->param.gen.sinT;
            cosT = Shapes->param.gen.cosT;

            xpg = -0.5*xwid*cosT -( 0.5*ywid)*sinT + xcen;
            ypg = -0.5*xwid*sinT +( 0.5*ywid)*cosT + ycen;
            xpgmin = xpg;
            xpgmax = xpg;
            ypgmin = ypg;
            ypgmax = ypg;
            xpg =  0.5*xwid*cosT -( 0.5*ywid)*sinT + xcen;
            ypg =  0.5*xwid*sinT +( 0.5*ywid)*cosT + ycen;
            if ( xpg>xpgmax ) xpgmax = xpg;
            if ( xpg<xpgmin ) xpgmin = xpg;
            if ( ypg>ypgmax ) ypgmax = ypg;
            if ( ypg<ypgmin ) ypgmin = ypg;
            xpg =  0.5*xwid*cosT -(-0.5*ywid)*sinT + xcen;
            ypg =  0.5*xwid*sinT +(-0.5*ywid)*cosT + ycen;
            if ( xpg>xpgmax ) xpgmax = xpg;
            if ( xpg<xpgmin ) xpgmin = xpg;
            if ( ypg>ypgmax ) ypgmax = ypg;
            if ( ypg<ypgmin ) ypgmin = ypg;
            xpg = -0.5*xwid*cosT -(-0.5*ywid)*sinT + xcen;
            ypg = -0.5*xwid*sinT +(-0.5*ywid)*cosT + ycen;
            if ( xpg>xpgmax ) xpgmax = xpg;
            if ( xpg<xpgmin ) xpgmin = xpg;
            if ( ypg>ypgmax ) ypgmax = ypg;
            if ( ypg<ypgmin ) ypgmin = ypg;
            break;

         case rectangle_rgn:
            xcen = Shapes->param.gen.p[5];
            ycen = Shapes->param.gen.p[6];
            xwid = Shapes->param.gen.a;
            ywid = Shapes->param.gen.b;
            sinT = Shapes->param.gen.sinT;
            cosT = Shapes->param.gen.cosT;

            xpg = -xwid*cosT -( ywid)*sinT + xcen;
            ypg = -xwid*sinT +( ywid)*cosT + ycen;
            xpgmin = xpg;
            xpgmax = xpg;
            ypgmin = ypg;
            ypgmax = ypg;
            xpg =  xwid*cosT -( ywid)*sinT + xcen;
            ypg =  xwid*sinT +( ywid)*cosT + ycen;
            if ( xpg>xpgmax ) xpgmax = xpg;
            if ( xpg<xpgmin ) xpgmin = xpg;
            if ( ypg>ypgmax ) ypgmax = ypg;
            if ( ypg<ypgmin ) ypgmin = ypg;
            xpg =  xwid*cosT -(-ywid)*sinT + xcen;
            ypg =  xwid*sinT +(-ywid)*cosT + ycen;
            if ( xpg>xpgmax ) xpgmax = xpg;
            if ( xpg<xpgmin ) xpgmin = xpg;
            if ( ypg>ypgmax ) ypgmax = ypg;
            if ( ypg<ypgmin ) ypgmin = ypg;
            xpg = -xwid*cosT -(-ywid)*sinT + xcen;
            ypg = -xwid*sinT +(-ywid)*cosT + ycen;
            if ( xpg>xpgmax ) xpgmax = xpg;
            if ( xpg<xpgmin ) xpgmin = xpg;
            if ( ypg>ypgmax ) ypgmax = ypg;
            if ( ypg<ypgmin ) ypgmin = ypg;
            break;

         case diamond_rgn:
            xcen = Shapes->param.gen.p[0];
            ycen = Shapes->param.gen.p[1];
            xwid = Shapes->param.gen.p[2];
            ywid = Shapes->param.gen.p[3];
            sinT = Shapes->param.gen.sinT;
            cosT = Shapes->param.gen.cosT;

            xpg = -0.5*xwid*cosT + xcen;
            ypg = -0.5*xwid*sinT + ycen;
            xpgmin = xpg;
            xpgmax = xpg;
            ypgmin = ypg;
            ypgmax = ypg;
            xpg =  -( 0.5*ywid)*sinT + xcen;
            ypg =   ( 0.5*ywid)*cosT + ycen;
            if ( xpg>xpgmax ) xpgmax = xpg;
            if ( xpg<xpgmin ) xpgmin = xpg;
            if ( ypg>ypgmax ) ypgmax = ypg;
            if ( ypg<ypgmin ) ypgmin = ypg;
            xpg =  0.5*xwid*cosT + xcen;
            ypg =  0.5*xwid*sinT + ycen;
            if ( xpg>xpgmax ) xpgmax = xpg;
            if ( xpg<xpgmin ) xpgmin = xpg;
            if ( ypg>ypgmax ) ypgmax = ypg;
            if ( ypg<ypgmin ) ypgmin = ypg;
            xpg = -(-0.5*ywid)*sinT + xcen;
            ypg =  (-0.5*ywid)*cosT + ycen;
            if ( xpg>xpgmax ) xpgmax = xpg;
            if ( xpg<xpgmin ) xpgmin = xpg;
            if ( ypg>ypgmax ) ypgmax = ypg;
            if ( ypg<ypgmin ) ypgmin = ypg;
            break;

         case circle_rgn:
            xpg = Shapes->param.gen.p[0];
            ypg = Shapes->param.gen.p[1];
            rpg = Shapes->param.gen.p[2];
            xpgmin = xpg-rpg;
            xpgmax = xpg+rpg;
            ypgmin = ypg-rpg;
            ypgmax = ypg+rpg;
            break;

         case annulus_rgn:
            xpg = Shapes->param.gen.p[0];
            ypg = Shapes->param.gen.p[1];
            if ( Shapes->param.gen.p[2] > Shapes->param.gen.p[3] ) {
               rpg = Shapes->param.gen.p[2];
            } else {
               rpg = Shapes->param.gen.p[3];
            }
            xpgmin = xpg-rpg;
            xpgmax = xpg+rpg;
            ypgmin = ypg-rpg;
            ypgmax = ypg+rpg;
            break;

         case sector_rgn:
            cxwrite(" Sector bounding box not implemented", 10);
            *status = -1;
            return;
/*
            xpg = Shapes->param.gen.p[0];
            ypg = Shapes->param.gen.p[1];
            sinT = Shapes->param.gen.p[2];
            cosT = Shapes->param.gen.p[3];
*/
            break;

         case ellipse_rgn:
/*
 *  Treat as simple circle for bounding box
 */
            xpg = Shapes->param.gen.p[0];
            ypg = Shapes->param.gen.p[1];
            if ( Shapes->param.gen.p[2] > Shapes->param.gen.p[3] ) {
               rpg = Shapes->param.gen.p[2];
            } else {
               rpg = Shapes->param.gen.p[3];
            }
            xpgmin = xpg-rpg;
            xpgmax = xpg+rpg;
            ypgmin = ypg-rpg;
            ypgmax = ypg+rpg;
            break;

         case elliptannulus_rgn:
/*
 *  Treat as simple circle for bounding box
 */
            xpg = Shapes->param.gen.p[0];
            ypg = Shapes->param.gen.p[1];
            rpg = Shapes->param.gen.p[2];
            if ( Shapes->param.gen.p[3] > rpg ) {
               rpg = Shapes->param.gen.p[3];
            } 
            if ( Shapes->param.gen.p[4] > rpg ) {
               rpg = Shapes->param.gen.p[4];
            } 
            if ( Shapes->param.gen.p[5] > rpg ) {
               rpg = Shapes->param.gen.p[5];
            } 
            xpgmin = xpg-rpg;
            xpgmax = xpg+rpg;
            ypgmin = ypg-rpg;
            ypgmax = ypg+rpg;
            break;

         case line_rgn:
            xpg = Shapes->param.gen.p[0];
            ypg = Shapes->param.gen.p[1];
            xpgmin = xpg;
            xpgmax = xpg;
            ypgmin = ypg;
            ypgmax = ypg;
            xpg = Shapes->param.gen.p[2];
            ypg = Shapes->param.gen.p[3];
            if ( xpg>xpgmax ) xpgmax = xpg;
            if ( xpg<xpgmin ) xpgmin = xpg;
            if ( ypg>ypgmax ) ypgmax = ypg;
            if ( ypg<ypgmin ) ypgmin = ypg;
            break;

         case point_rgn:
            xpg = Shapes->param.gen.p[0];
            ypg = Shapes->param.gen.p[1];
            xpgmin = xpg;
            xpgmax = xpg;
            ypgmin = ypg;
            ypgmax = ypg;
            break;

         case poly_rgn:

            xpgmin = Shapes->param.poly.xmin;
            xpgmax = Shapes->param.poly.xmax;
            ypgmin = Shapes->param.poly.ymin;
            ypgmax = Shapes->param.poly.ymax;

            break;
      }
      if ( xpgmax>*xmax ) *xmax = xpgmax;
      if ( xpgmin<*xmin ) *xmin = xpgmin;
      if ( ypgmax>*ymax ) *ymax = ypgmax;
      if ( ypgmin<*ymin ) *ymin = ypgmin;
   }

   /* Return null values, if not bounded */

   if ( *xmin>=+1e40 ) *xmin = DNULL();
   if ( *xmax<=-1e40 ) *xmax = DNULL();
   if ( *ymin>=+1e40 ) *ymin = DNULL();
   if ( *ymax<=-1e40 ) *ymax = DNULL();

}

void bboxreg(int ireg, double *xmin, double *xmax, double *ymin,
             double *ymax, int *status) {
/*
 * Get bounding box for region #ireg
 * ireg goes from 1 to nreglist
 */
    if ( ireg < 1 || ireg > nreglist ) { 
       cxwrite(" ERROR: accessing out of region list bounds", 10);
       *status = 1;
       return; 
    }
    xim_bbox_region(regobj[ireg-1],xmin,xmax,ymin,ymax,status);
    if ( *status != 0 ) {
       cxwrite(" ERROR: Failed to determine region bounds", 10);
    }
}
FCALLSCSUB6(bboxreg,BBOXREG,bboxreg,INT,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,PINT)


void xim_plot_region (SAORegion *Rgn, int color, int lwidth, int lstyle,
                      int excolor, int exlwidth, int exlstyle, int *status) {
/*
 *  Plots region in structure with PGPLOT
 */
   RgnShape *Shapes;
   int i, curcol, curlw, curls;
   double xcen, ycen, xwid, ywid, cosT, sinT;
   float xpg, ypg, rpg, rxpg, rypg;
   float bxcen, bycen, bxwid, bywid, angle;
   float *tmppts, plypts[8];

   *status = 0;
   cpgsave();

   if ( !Rgn ) {
      *status = -1;
      return;
   }
   Shapes = Rgn->Shapes;
   for ( i=0; i<Rgn->nShapes; i++, Shapes++ ) {

      if ( Shapes->sign ) {
         curcol = color;
         curlw = lwidth;
         curls = lstyle;
      } else {
         curcol = excolor;
         curlw = exlwidth;
         curls = exlstyle;
      }
      LINE_PGSTATE(color, lwidth, lstyle);

      switch ( Shapes->shape ) {

         case box_rgn:
            bxcen = Shapes->param.gen.p[0];
            bycen = Shapes->param.gen.p[1];
            bxwid = Shapes->param.gen.p[2];
            bywid = Shapes->param.gen.p[3];
            angle = Shapes->param.gen.p[4];

            jrnbox(bxcen,bycen,bxwid,bywid,angle,curcol,curlw,curls);
            break;

         case rectangle_rgn:
            xcen = Shapes->param.gen.p[5];
            ycen = Shapes->param.gen.p[6];
            xwid = Shapes->param.gen.a;
            ywid = Shapes->param.gen.b;
            sinT = Shapes->param.gen.sinT;
            cosT = Shapes->param.gen.cosT;

            plypts[0] = -xwid*cosT -( ywid)*sinT + xcen;
            plypts[1] = -xwid*sinT +( ywid)*cosT + ycen;
            plypts[2] =  xwid*cosT -( ywid)*sinT + xcen;
            plypts[3] =  xwid*sinT +( ywid)*cosT + ycen;
            plypts[4] =  xwid*cosT -(-ywid)*sinT + xcen;
            plypts[5] =  xwid*sinT +(-ywid)*cosT + ycen;
            plypts[6] = -xwid*cosT -(-ywid)*sinT + xcen;
            plypts[7] = -xwid*sinT +(-ywid)*cosT + ycen;
            jrnpoly(plypts,8,curcol,curlw,curls);
            break;

         case diamond_rgn:
            xcen = Shapes->param.gen.p[0];
            ycen = Shapes->param.gen.p[1];
            xwid = Shapes->param.gen.p[2];
            ywid = Shapes->param.gen.p[3];
            sinT = Shapes->param.gen.sinT;
            cosT = Shapes->param.gen.cosT;

            plypts[0] = -0.5*xwid*cosT + xcen;
            plypts[1] = -0.5*xwid*sinT + ycen;
            plypts[2] =  -( 0.5*ywid)*sinT + xcen;
            plypts[3] =   ( 0.5*ywid)*cosT + ycen;
            plypts[4] =  0.5*xwid*cosT + xcen;
            plypts[5] =  0.5*xwid*sinT + ycen;
            plypts[6] = -(-0.5*ywid)*sinT + xcen;
            plypts[7] =  (-0.5*ywid)*cosT + ycen;
            jrnpoly(plypts,8,curcol,curlw,curls);
            break;

         case circle_rgn:
            xpg = Shapes->param.gen.p[0];
            ypg = Shapes->param.gen.p[1];
            rpg = Shapes->param.gen.p[2];
            jrncir(xpg,ypg,rpg,curcol,curlw,curls);
            break;

         case annulus_rgn:
            xpg = Shapes->param.gen.p[0];
            ypg = Shapes->param.gen.p[1];
            rpg = Shapes->param.gen.p[2];
            jrncir(xpg,ypg,rpg,curcol,curlw,curls);
            rpg = Shapes->param.gen.p[3];
            jrncir(xpg,ypg,rpg,curcol,curlw,curls);
            break;

         case sector_rgn:
            cxwrite(" Sector plotting not implemented", 10);
/*
            xpg = Shapes->param.gen.p[0];
            ypg = Shapes->param.gen.p[1];
            sinT = Shapes->param.gen.p[2];
            cosT = Shapes->param.gen.p[3];
*/
            break;

         case ellipse_rgn:
            xpg = Shapes->param.gen.p[0];
            ypg = Shapes->param.gen.p[1];
            rxpg = Shapes->param.gen.p[2];
            rypg = Shapes->param.gen.p[3];
            sinT = Shapes->param.gen.sinT;
            cosT = Shapes->param.gen.cosT;
            DRWELP(xpg,ypg,rxpg,rypg,sinT,cosT);
            break;

         case elliptannulus_rgn:
            xpg = Shapes->param.gen.p[0];
            ypg = Shapes->param.gen.p[1];
            rxpg = Shapes->param.gen.p[2];
            rypg = Shapes->param.gen.p[3];
            sinT = Shapes->param.gen.a;
            cosT = Shapes->param.gen.b;
            DRWELP(xpg,ypg,rxpg,rypg,sinT,cosT);
            rxpg = Shapes->param.gen.p[4];
            rypg = Shapes->param.gen.p[5];
            sinT = Shapes->param.gen.sinT;
            cosT = Shapes->param.gen.cosT;
            DRWELP(xpg,ypg,rxpg,rypg,sinT,cosT);
            break;

         case line_rgn:
            plypts[0] = Shapes->param.gen.p[0];
            plypts[1] = Shapes->param.gen.p[1];
            plypts[2] = Shapes->param.gen.p[2];
            plypts[3] = Shapes->param.gen.p[3];
            jrnpoly(plypts,4,curcol,curlw,curls);
            break;

         case point_rgn:
            plypts[0] = Shapes->param.gen.p[0];
            plypts[1] = Shapes->param.gen.p[1];
            jrnpoly(plypts,2,curcol,curlw,curls);
            break;

         case poly_rgn:

            tmppts = (float *) malloc(sizeof(float)*Shapes->param.poly.nPts);
            if ( !tmppts ) {
               cxwrite(" Polygon allocation failure", 5);
               *status = -1;
               return;
            }
            for ( i = 0; i < Shapes->param.poly.nPts; i++ ) {
               tmppts[i] = Shapes->param.poly.Pts[i];
            }
            jrnpoly(tmppts,Shapes->param.poly.nPts,curcol,curlw,curls);
            free(tmppts);
            break;
      }
   }

   cpgunsa();
}

void plotreg(int ireg, int color, int lwidth, int lstyle,
             int excolor, int exlwidth, int exlstyle, int *status) {
/*
 * Plots region #ireg
 * ireg goes from 1 to nreglist
 */
    if ( ireg < 1 || ireg > nreglist ) { 
       cxwrite(" ERROR: accessing out of region list bounds", 10);
       *status = 1;
       return; 
    }
    xim_plot_region(regobj[ireg-1],color,lwidth,lstyle,
                    excolor,exlwidth,exlstyle,status);
}
FCALLSCSUB8(plotreg,PLOTREG,plotreg,INT,INT,INT,INT,INT,INT,INT,PINT)
