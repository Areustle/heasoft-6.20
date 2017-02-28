/*
 * This is a copy of
 *		headas/ftools/image/ximage/src/region/ximreg.c:xim_bbox_region
 */

#include "region.h"
#include "headas_stdio.h"


void xim_bbox_region(SAORegion *Rgn, double *xmin, double *xmax, 
                     double *ymin, double *ymax, int *status) {
/*
 *  Determines region's bounding box
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
            headas_chat(0, " Sector bounding box not implemented");
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

            xpgmin = Shapes->xmin;
            xpgmax = Shapes->xmax;
            ypgmin = Shapes->ymin;
            ypgmax = Shapes->ymax;

            break;
      }
      if ( xpgmax>*xmax ) *xmax = xpgmax;
      if ( xpgmin<*xmin ) *xmin = xpgmin;
      if ( ypgmax>*ymax ) *ymax = ypgmax;
      if ( ypgmin<*ymin ) *ymin = ypgmin;
   }

   /* Return "infinite" value, if not bounded */

   if ( *xmin>=+1e40 ) *xmin = -1e40;
   if ( *xmax<=-1e40 ) *xmax = +1e40;
   if ( *ymin>=+1e40 ) *ymin = -1e40;
   if ( *ymax<=-1e40 ) *ymax = +1e40;

}


