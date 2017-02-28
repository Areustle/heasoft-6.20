#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "cpgplot.h"
#include "../include/xmtcl.h"

#include "../include/maxvals.h"
#include "../include/xcommon.h"
#include "../include/cmddef.h"

#include "cfortran.h"
#include "../include/pgutil.h"

#define myPI  3.1415926535897932385

/*
 * drawcmd --
 * Tcl command to draw onto current PGPLOT device
 */
int
drawcmd(ClientData cdata,Tcl_Interp* interp,int objc, Tcl_Obj* CONST objv[] )
{
   int status;
   cmddef *curcmd;
   pardef *parlist;
   argdef *curarg;
   int i, iscircle, isbox, ispoly, isline, isarrow, isoarrow;
   int color, lwidth, lstyle;
   float x, y, r, xpg, ypg, xpg0, ypg0;
   double xcen, ycen, xwid, ywid, angle, cosT, sinT, dd;
   Tcl_Obj *tmpObj;
   double dbl[5];

   curcmd = cdata;

   if ( !CParseParm(interp, objc, objv, curcmd) ) {
      return TCL_ERROR;
   }
   parlist = curcmd->parlist;

   lwidth = -1;
   lstyle = -1;
   status = 0;
   iscircle = 0;
   isbox = 0;
   ispoly = 0;
   isline = 0;
   isarrow = 0;
   isoarrow = 0;

   color = 16;
   GET_COLOR(color);

   cgparl(parlist, "CIRCLE", &iscircle, &status);
   cgparl(parlist, "BOX", &isbox, &status);
   cgparl(parlist, "POLYGON", &ispoly, &status);
   cgparl(parlist, "LINE", &isline, &status);
   cgparl(parlist, "ARROW", &isarrow, &status);
   cgparl(parlist, "OARROW", &isoarrow, &status);
   cgpari(parlist, "COLOR", &color, &status);
   cgpari(parlist, "LWIDTH", &lwidth, &status);
   cgpari(parlist, "LSTYLE", &lstyle, &status);

   if ( !ISDISPLAY() ) {
      cxwarn("No display", 10);
      return TCL_ERROR;
   }

   if ( iscircle ) {

      if ( curcmd->cmdargc != 3 ) {
         cxwrite(" Usage: draw circle <x> <y> <r>", 10);
         return TCL_ERROR;
      }
      curarg = curcmd->cmdargs;
      i = 0;
      while ( curarg ) {
         tmpObj = Tcl_NewStringObj(curarg->name, -1);
         Tcl_GetDoubleFromObj(xm_interp, tmpObj, &dbl[i]);
         curarg = curarg->next;
         i++;
      }
      x = dbl[0];
      y = dbl[1];
      r = dbl[2];

      cpgsave();
      LINE_PGSTATE(color, lwidth, lstyle);
      DRWCIR(x, y, r);
      cpgunsa();

   } else if ( isbox ) {

      if ( curcmd->cmdargc != 5 && curcmd->cmdargc != 4 ) {
         cxwrite(" Usage: draw box <xcen> <ycen> <xwid> <ywid> <angle>", 10);
         return TCL_ERROR;
      }
      curarg = curcmd->cmdargs;
      i = 0;
      while ( curarg ) {
         tmpObj = Tcl_NewStringObj(curarg->name, -1);
         Tcl_GetDoubleFromObj(xm_interp, tmpObj, &dbl[i]);
         curarg = curarg->next;
         i++;
      }
      xcen = dbl[0];
      ycen = dbl[1];
      xwid = dbl[2];
      ywid = dbl[3];
      if ( i > 4 ) {
         angle = dbl[4];
      } else {
         angle = 0.;
      }
      sinT = sin( myPI * (angle / 180.0) );
      cosT = cos( myPI * (angle / 180.0) );

      cpgsave();
      LINE_PGSTATE(color, lwidth, lstyle);

      xpg = -0.5*xwid*cosT -( 0.5*ywid)*sinT + xcen;
      ypg = -0.5*xwid*sinT +( 0.5*ywid)*cosT + ycen;
      cpgmove(xpg, ypg);
      xpg0 = xpg;
      ypg0 = ypg;
      xpg =  0.5*xwid*cosT -( 0.5*ywid)*sinT + xcen;
      ypg =  0.5*xwid*sinT +( 0.5*ywid)*cosT + ycen;
      cpgdraw(xpg, ypg);
      xpg =  0.5*xwid*cosT -(-0.5*ywid)*sinT + xcen;
      ypg =  0.5*xwid*sinT +(-0.5*ywid)*cosT + ycen;
      cpgdraw(xpg, ypg);
      xpg = -0.5*xwid*cosT -(-0.5*ywid)*sinT + xcen;
      ypg = -0.5*xwid*sinT +(-0.5*ywid)*cosT + ycen;
      cpgdraw(xpg, ypg);
      cpgdraw(xpg0, ypg0);

      cpgunsa();

   } else if ( ispoly || isline ) {

      if ( curcmd->cmdargc == 0 || curcmd->cmdargc % 2 != 0 ) {
         if ( ispoly ) {
            cxwrite(" Usage: draw polygon <x1> <y1> ... <xn> <yn>", 10);
         } else {
            cxwrite(" Usage: draw line <x1> <y1> <x2> <y2>", 10);
         }
         return TCL_ERROR;
      }
      curarg = curcmd->cmdargs;

      cpgsave();
      LINE_PGSTATE(color, lwidth, lstyle);
/*
 *  Save first point to connect with at end
 */
      tmpObj = Tcl_NewStringObj(curarg->name, -1);
      Tcl_GetDoubleFromObj(xm_interp, tmpObj, &dd);
      curarg = curarg->next;
      xpg0 = dd;

      tmpObj = Tcl_NewStringObj(curarg->name, -1);
      Tcl_GetDoubleFromObj(xm_interp, tmpObj, &dd);
      curarg = curarg->next;
      ypg0 = dd;

      cpgmove(xpg0,ypg0);
/*
 *  Draw to each pair of coords in turn
 */
      while ( curarg ) {
         tmpObj = Tcl_NewStringObj(curarg->name, -1);
         Tcl_GetDoubleFromObj(xm_interp, tmpObj, &dd);
         curarg = curarg->next;
         xpg = dd;

         tmpObj = Tcl_NewStringObj(curarg->name, -1);
         Tcl_GetDoubleFromObj(xm_interp, tmpObj, &dd);
         curarg = curarg->next;
         ypg = dd;

         cpgdraw(xpg,ypg);
      }
/*
 *  Connect to first point if polygon
 */
      if ( ispoly ) cpgdraw(xpg0,ypg0);
      cpgunsa();

   } else if ( isarrow || isoarrow ) {

      if ( curcmd->cmdargc != 4 ) {
         if ( isarrow ) {
            cxwrite(" Usage: draw arrow <x1> <y1> <x2> <y2>", 10);
         } else {
            cxwrite(" Usage: draw oarrow <x1> <y1> <x2> <y2>", 10);
         }
         return TCL_ERROR;
      }
      curarg = curcmd->cmdargs;

      cpgsave();
      LINE_PGSTATE(color, lwidth, lstyle);
/*
 *  Start point
 */
      tmpObj = Tcl_NewStringObj(curarg->name, -1);
      Tcl_GetDoubleFromObj(xm_interp, tmpObj, &dd);
      curarg = curarg->next;
      xpg0 = dd;

      tmpObj = Tcl_NewStringObj(curarg->name, -1);
      Tcl_GetDoubleFromObj(xm_interp, tmpObj, &dd);
      curarg = curarg->next;
      ypg0 = dd;
/*
 *  End point
 */
      tmpObj = Tcl_NewStringObj(curarg->name, -1);
      Tcl_GetDoubleFromObj(xm_interp, tmpObj, &dd);
      curarg = curarg->next;
      xpg = dd;

      tmpObj = Tcl_NewStringObj(curarg->name, -1);
      Tcl_GetDoubleFromObj(xm_interp, tmpObj, &dd);
      curarg = curarg->next;
      ypg = dd;

      if ( isarrow ) {
         cpgsah(1,45.,0.3);
      } else {
         cpgsah(2,45.,0.3);
      }
      cpgarro(xpg0, ypg0, xpg, ypg);

      cpgunsa();

   } else {
      cxwrite(" No shape specified to draw", 10);
      return TCL_ERROR;
   }

   cjrncmd(curcmd, &status);
   return TCL_OK;
}
