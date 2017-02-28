#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <tcl.h>
#include "cfortran.h"

#include "ast.h"
#include "cpgplot.h"
#include "fitsio.h"
#include "../include/maxvals.h"
#include "../include/astadd.h"
#include "../include/xcommon.h"
#include "../include/cmddef.h"
#include "../include/xmtcl.h"
#include "../include/pgutil.h"
#include "../include/map.h"
#include "../include/wcsmanager.h"

#define MAX_OPTS 200

/*
 * gridcmd --
 * Plot grid using AST
 */
int
gridcmd(ClientData cdata,Tcl_Interp* interp,int objc, Tcl_Obj* CONST objv[] )
{
   int status;
   cmddef *curcmd;
   pardef *parlist;

   int i, ichat;
   Tcl_Obj *tmpObj;
   argdef *curarg;

   int abbrev, radec, galactic, labcolor, color, lwidth, lstyle;
   int ticks_only, nolabel;
   double gridstep[2];
   float csize, gaprad[2];
   const char *label;

   char wcsid[MAX_IDSTR];

   AstPlot *plot;
   AstFrameSet *wcsinfo, *wcscopy;
   AstFrame *frame;
   AstFitsChan *fitschan;
   char *encode, *msg;
   int iframe;

   float detbox[4];
   double imgbox[4], xpix, ypix, pt1[2], pt2[2], dist;
   float equinox, gap, ticklen;

   int is_grid_astoptions;
   Tcl_Obj* valPtr;

   equinox = -1.;
   abbrev = 0;
   radec = 0;
   galactic = 0;
   labcolor = -1;
   csize = 0.75;
   lwidth = -1;
   lstyle = -1;
   ticks_only = 0;
   gridstep[0] = -1.0;
   gridstep[1] = -1.0;
   nolabel = 0;

   color = 16;
   GET_COLOR(color);

   curcmd = cdata;
   status = 0;

   if ( !CParseParm(interp, objc, objv, curcmd) ) {
      return TCL_ERROR;
   }
   parlist = curcmd->parlist;
/*
 * Expect one of the following:
 *    grid     [No args, default grid spacing]
 *    grid #   [1 arg, same spacing for both axes]
 *    grid # # [2 args, separate spacing for axes]
 *
 *  cwrongargs prints arguments which are unaccounted for
 */
   if ( curcmd->cmdargc > 2 ) {
      cwrongargs(curcmd->cmdargs);
      return TCL_ERROR;
   }
   curarg = curcmd->cmdargs;
   i = 0;
   while (curarg) {
      tmpObj = Tcl_NewStringObj(curarg->name,-1);
      Tcl_GetDoubleFromObj(xm_interp, tmpObj, &gridstep[i]);
      curarg = curarg->next;
      i++;
   }

   cgparl(parlist, "ABBREV", &abbrev, &status);
   cgparl(parlist, "RADEC", &radec, &status);
   cgparr(parlist, "EQUINOX", &equinox, &status);
   cgparl(parlist, "GALACTIC", &galactic, &status);
   cgpari(parlist, "COLOR", &color, &status);
   cgpari(parlist, "LABCOLOR", &labcolor, &status);
   cgparr(parlist, "CSIZE", &csize, &status);
   cgpari(parlist, "LWIDTH", &lwidth, &status);
   cgpari(parlist, "LSTYLE", &lstyle, &status);
   cgparl(parlist, "TICKS_ONLY", &ticks_only, &status);
   cgparl(parlist, "NOLABEL", &nolabel, &status);
   if ( status != 0 ) return TCL_ERROR;


   if ( !ISDISPLAY() ) {
      cxwarn("No image displayed", 10);
      return TCL_ERROR;
   }

   /*
    *  Get wcsid from current pgstate.  Map information unimportant
    */
   
   tclress("pgtk::curwcsid", wcsid, MAX_IDSTR, &status);
   wcsinfo = getwcs(wcsid);
   if ( wcsinfo == NULL ) {
      cxwrite("ERROR: Failed to get WCS frame", 5);
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

   /*
    *  If current frame not a skyframe, set to a sky frame in order
    *  to plot galactic grid
    */
   if ( galactic || radec || equinox > 0. ) {
      if ( !astIsASkyFrame(frame) ) {
         iframe = getframeidx(wcscopy, "SKY");
         if ( !iframe ) {
            cxwarn(" Image has no sky coordinates", 10);
            return TCL_ERROR;
         } else {
            astSetI(wcscopy, "Current", iframe);
            frame = astGetFrame(wcscopy, AST__CURRENT);
         }
      }
   }

   plot = astPlot(wcscopy, detbox, imgbox, "Grid=1,Border=1,Edge(1)=top,Edge(2)=right,TextLab=0,DrawTitle=0,MinTickLen=0");

   /*
    *  Override plot options with values from grid_astoptions Tcl var
    */
   is_grid_astoptions = 0;
   tclresl("info exists grid_astoptions", &is_grid_astoptions, &status);
   if ( is_grid_astoptions ) {
      tclresobj("set grid_astoptions", &valPtr, &status);
      astSet(plot, Tcl_GetString(valPtr));
   }

   astSetI(plot, "Abbrev", abbrev);
   if ( labcolor >= 0 ) astSetI(plot, "Color", labcolor);
   if ( color >= 0 ) astSetI(plot, "Color(Grid)", color);
   if ( csize >= 0. ) astSetF(plot, "Size(NumLab)", csize);
   if ( lwidth > 0 ) astSetI(plot, "Width(Grid)", lwidth);
   if ( lstyle > 0 ) astSetI(plot, "Style", lstyle);
   if ( galactic ) astSet(plot, "System=Galactic");

   /*
    *  If an FK system, use default equinox
    */
   if ( equinox < 0. ) {
      if ( strncmp(astGetC(plot, "System"), "FK", 2) == 0 ) {
         tclresr("set default(equinox)", &equinox, &status);
      }
   }
   if ( equinox > 0. ) {
      astSetF(plot, "Equinox", equinox);
      if ( strncmp(astGetC(plot, "System"), "FK", 2) == 0 ) {
         if ( (int) equinox == 1950 ) {
            astSet(plot, "System=FK4");
         } else if ( (int) equinox == 2000 ) {
            astSet(plot, "System=FK5");
         }
      }
   }
   if ( radec ) {
      if ( (int) equinox == 1950 ) {
         astSet(plot, "System=FK4");
      } else {
         astSet(plot, "System=FK5");
      }
   } 
   if ( ticks_only ) {
      astSet(plot, "Grid=0,TickAll=1");
      ticklen = astGetF(plot, "MajTickLen");
      astSetF(plot, "MajTickLen", 2.*ticklen);
   }
   if ( nolabel ) astSet(plot,"NumLab=0");
   msg = strcatalloc(" System: ", astGetC(plot, "System"));
   cxwrite(msg, 15);
   free(msg);
/*
 *  Determine gap between grid lines
 */
   gaprad[0] = -1.;
   gaprad[1] = -1.;
   if ( gridstep[0] > 0. ) {
      if ( gridstep[1] < 0. ) gridstep[1] = gridstep[0];
      if ( astIsASkyFrame(frame) ) {
         gaprad[0] = gridstep[0]*D2R; /* convert degrees to radians */
         gaprad[1] = gridstep[1]*D2R;
      } else {
         gaprad[0] = gridstep[0];     /* no conversion */
         gaprad[1] = gridstep[1];
      }
   }
   if ( gaprad[0] > 0. && gaprad[1] > 0. ) {
      astSetF(plot, "Gap(1)", gaprad[0]);
      astSetF(plot, "Gap(2)", gaprad[1]);
   }

   /* Use letters (hms) to delimit R.A. and degree to delimit Dec */
   label = astGetC(plot, "Label(1)");
   if ( strcmp(label, "Right ascension") == 0 ) {
      astSetC(plot, "Format(1)", "lhms.*g");
   } else if ( strcmp(label, "Declination") == 0 ) {
      astSetC(plot, "Format(1)", "+dms.*g");
   }
   label = astGetC(plot, "Label(2)");
   if ( strcmp(label, "Declination") == 0 ) {
      astSetC(plot, "Format(2)", "+dms.*g");
   } else if ( strcmp(label, "Right ascension") == 0 ) {
      astSetC(plot, "Format(2)", "lhms.*g");
   }
   astReporting_(0);
   ichat = getastchat();
   setastchat(20);    /* Show grid errors at higher chat */
   astGrid(plot);
   setastchat(ichat); /* Restore original chat level for ast errors */
   if ( !astOK ) astClearStatus;
   astEnd;

   cjrncmd(curcmd, &status);
   return TCL_OK;
}
