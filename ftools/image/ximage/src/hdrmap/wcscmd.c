#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../include/xmtcl.h"
#include "cfortran.h"
#include "fitsio.h"
#include "ast.h"
#include "../include/wcsmanager.h"
#include "../include/xcommon.h"
#include "../include/cmddef.h"
#include "../include/maxvals.h"
#include "../include/map.h"

#define INFOLEN 30
#define NFRMID 3

/*
 * wcscmd --
 * WCS data maintenance (primarily for Tcl scripting)
 */
int
wcscmd(ClientData cdata,Tcl_Interp* interp,int objc, Tcl_Obj* CONST objv[] )
{
   int status;
   cmddef *curcmd;
   pardef *parlist;

   int uphdr, upwcs, decrwcs, incrwcs, show, iframe, icurr;
   char wcsid[MAX_IDSTR], mapid[MAX_IDSTR], nwcsid[MAX_IDSTR];
   char frameid[MAX_IDSTR];
   char system[INFOLEN], projection[INFOLEN], xlab[INFOLEN];
   char ylab[INFOLEN], unit[INFOLEN];
   double imgref[2], detref[2], begzm[2];
   const char *domain;

   AstFrameSet *wcsinfo;
   AstFrame *frame;
   int i, j, found, skyfound, isglobal, isreadonly;
   double equinox;
   char buff[40];

   static char * framealias[NFRMID] = { "IMG", "DET", "SKY" };
   static char * framedomain[NFRMID] = { "GRID", "PIXEL", "SKY" };
   
   strcpy(wcsid, "");
   strcpy(mapid, "");
   strcpy(frameid, "");
   tclress("set curmap", mapid, MAX_IDSTR, &status);
   uphdr = 0;
   upwcs = 0;
   incrwcs = 0;
   decrwcs = 0;
   show = 0;

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

   cgpars(parlist, "WCSID", wcsid, MAX_IDSTR, &status);
   cgpars(parlist, "MAPID", mapid, MAX_IDSTR, &status);
   cgpars(parlist, "FRAMEID", frameid, MAX_IDSTR, &status);
   cgparl(parlist, "UPHDR", &uphdr, &status);
   cgparl(parlist, "UPWCS", &upwcs, &status);
   cgparl(parlist, "DECR", &decrwcs, &status);
   cgparl(parlist, "INCR", &incrwcs, &status);
   cgparl(parlist, "SHOW", &show, &status);

   if ( decrwcs && incrwcs ) {
      cxwrite("ERROR: incr and decr qualifiers are incompatible", 5);
      return TCL_ERROR;
   }
   if ( uphdr && upwcs ) {
      cxwrite("ERROR: uphdr and upwcs qualifiers are incompatible", 5);
      return TCL_ERROR;
   }
   /*
    *  Clear wcs array from Tcl environment
    */
   isglobal = 0;
   isreadonly = 0;
   tclunset("wcs", isglobal, &status);
   status = 0;

   if ( uphdr || upwcs ) {
      if ( !*wcsid ) strcpy(wcsid, mapid);
   }

/*
 *  If no wcsid is given, print status of all wcsid's
 */
   if ( ! *wcsid ) {
      prallwcs();
      GHEADS(mapid, "WCSID", wcsid, 0, status);
      if ( strcmp(wcsid,"") == 0 ) return TCL_OK;
   }

   wcsinfo = getmapwcs(wcsid);
   if ( !wcsinfo && !upwcs ) {
      cxwrite("ERROR: Specified wcsid is unused", 5);
      return TCL_ERROR;
   }
   if ( uphdr ) {
      wcstohdr(wcsid, mapid, &status);
   } else if ( upwcs ) {
      GHEADS(mapid, "WCSID", wcsid, 0, status);
      if ( *wcsid ) wcsdecref(wcsid);
      strcpy(wcsid,"");
      GHEADS(mapid, "WCSID", wcsid, 1, status);
      genhdrwcs(mapid, MAX_IDSTR, nwcsid, &status);
      GHEADD(mapid, "CRPIX1", imgref[0], 0, status);
      GHEADD(mapid, "CRPIX2", imgref[1], 0, status);
      GHEADD(mapid, "DRPIX1", detref[0], 0, status);
      GHEADD(mapid, "DRPIX2", detref[1], 0, status);
      GHEADD(mapid, "ZMX", begzm[0], 0, status);
      GHEADD(mapid, "ZMY", begzm[1], 0, status);
      wcssetdet(nwcsid, 0, imgref, detref, begzm, &status);
      GHEADS(mapid, "WCSID", nwcsid, 1, status);
   } else if ( incrwcs ) {
      wcsincref(wcsid);
   } else if ( decrwcs ) {
      wcsdecref(wcsid);
   } else if ( show ) {
      astShow(wcsinfo);
   } else if ( strcmp(frameid,"") != 0 ) {
      if ( *frameid == '?' ) goto frameopt;
      iframe = getframeidx(wcsinfo,frameid);
      if ( !iframe ) goto frameopt;
      astSetI(wcsinfo, "Current", iframe);
   } else {
      wcsfrminfo(wcsid, system, projection, xlab, ylab, unit, &equinox);
      tclvars("wcs(wcsid)", wcsid, isreadonly, isglobal, &status);
      tclvars("wcs(system)", system, isreadonly, isglobal, &status);
      tclvars("wcs(projection)", projection, isreadonly, isglobal, &status);
      tclvars("wcs(unit)", unit, isreadonly, isglobal, &status);
      tclvars("wcs(xlab)", xlab, isreadonly, isglobal, &status);
      tclvars("wcs(ylab)", ylab, isreadonly, isglobal, &status);
      if ( wcsinfo ) {
         tclvari("wcs(frame)", astGetI(wcsinfo, "Current"), isreadonly, 
                 isglobal, &status);
      }
   }

   return TCL_OK;

frameopt:
   cxwrite("", 10);
   cxwrite(" n   ID   System     Unit", 10);
   cxwrite(" --  ---  ---------- ------------", 10);
   skyfound = 0;
   icurr = astGetI(wcsinfo, "Current");
   for ( i = 1; i <= astGetI(wcsinfo, "Nframe"); i++ ) {
      frame = astGetFrame(wcsinfo, i);
      domain = astGetC(frame, "Domain");
      found = 0;
      j = 0;
      while ( !found && j < NFRMID ) {
         if ( strcmp(domain, framedomain[j]) == 0 ) {
            found = 1;
            if ( j == 2 ) { /* Insure only one SKY domain is printed */
               if ( skyfound ) found = 0;
               skyfound = 1; 
            }
         } else {
            j++;
         }
      }
      if ( found ) {
         sprintf(buff, " %2d  %3s  %-10s %s\n", i, framealias[j], 
                    astGetC(frame, "System"), getframeunit(frame));
      } else {
         sprintf(buff, " %2d  %-3s  %-10s %s\n", i, astGetC(frame, "Ident"), 
                    astGetC(frame, "System"), getframeunit(frame));
      }
      buff[strlen(buff)-1] = '\0';
      if ( i == icurr ) buff[0] = '*';
      cxwrite(buff, 10);
   }
   cxwrite("\n Frames are identified by index 'n' or 'ID'", 10);
   cxwrite(" * = Current frame", 10);

   return TCL_OK;
}
