#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "cfortran.h"

#include "ast.h"
#include "cpgplot.h"
#include "fitsio.h"
#include "../include/maxvals.h"
#include "../include/xcommon.h"
#include "../include/xmtcl.h"
#include "../include/cmddef.h"
#include "../include/pgutil.h"
#include "../include/map.h"
#include "../include/wcsmanager.h"

/*
 * curvecmd --
 * Plot geodesic curve using AST
 */
int
curvecmd(ClientData cdata,Tcl_Interp* interp,int objc, Tcl_Obj* CONST objv[] )
{
   int status;
   cmddef *curcmd;
   pardef *parlist;

   int color, lwidth, lstyle;
   double x1sky, y1sky, x2sky, y2sky;
   double start[2], finish[2];

   char wcsid[MAX_IDSTR];

   AstPlot *plot;
   AstFrame *frame;
   AstFrameSet *wcsinfo, *wcscopy;

   float detbox[4];
   double imgbox[4], xpix, ypix;

   x1sky = AST__BAD;
   y1sky = AST__BAD;
   x2sky = AST__BAD;
   y2sky = AST__BAD;
   lwidth = -1;
   lstyle = -1;

   color = 16;
   GET_COLOR(color);

   curcmd = cdata;
   status = 0;

   if ( !CParseParm(interp, objc, objv, curcmd) ) {
      return TCL_ERROR;
   }
   parlist = curcmd->parlist;

   if ( curcmd->cmdargc != 0 ) {
      cwrongargs(curcmd->cmdargs);
      return TCL_ERROR;
   }

   cgpard(parlist, "X1SKY", &x1sky, &status);
   cgpard(parlist, "Y1SKY", &y1sky, &status);
   cgpard(parlist, "X2SKY", &x2sky, &status);
   cgpard(parlist, "Y2SKY", &y2sky, &status);
   cgpari(parlist, "COLOR", &color, &status);
   cgpari(parlist, "LWIDTH", &lwidth, &status);
   cgpari(parlist, "LSTYLE", &lstyle, &status);
   if ( status != 0 ) return TCL_ERROR;


   if ( !ISDISPLAY() ) {
      cxwarn("No image displayed", 10);
      return TCL_ERROR;
   }

   cpgsave();
   LINE_PGSTATE(color, lwidth, lstyle);

   /*
    *  Get wcsid from current pgstate.  Map information unimportant
    */
   
   tclress("pgtk::curwcsid", wcsid, MAX_IDSTR, &status);
   wcsinfo = getwcs(wcsid);
   if ( wcsinfo == NULL ) {
      cxwrite("ERROR: Failed to get WCS frame", 5);
      cpgunsa();
      return TCL_ERROR;
   }

   cpgqwin(detbox, detbox+2, detbox+1, detbox+3);
   xpix = detbox[0];
   ypix = detbox[1];
   wcsimgpix(wcsid, imgbox, imgbox+1, &xpix, &ypix, 0, &status);
   xpix = detbox[2];
   ypix = detbox[3];
   wcsimgpix(wcsid, imgbox+2, imgbox+3, &xpix, &ypix, 0, &status);

   astBegin;
   wcscopy = (AstFrameSet *) astCopy(wcsinfo);
   frame = astGetFrame(wcscopy, AST__CURRENT);
   plot = astPlot(wcscopy, detbox, imgbox, "");
   if ( astIsASkyFrame(frame) ) {
      start[0] = x1sky*D2R;
      start[1] = y1sky*D2R;
      finish[0] = x2sky*D2R;
      finish[1] = y2sky*D2R;
   } else {
      start[0] = x1sky;
      start[1] = y1sky;
      finish[0] = x2sky;
      finish[1] = y2sky;
   }
   astCurve(plot, start, finish);
   astEnd;

   cpgunsa();
   cjrncmd(curcmd, &status);
   return TCL_OK;
}
