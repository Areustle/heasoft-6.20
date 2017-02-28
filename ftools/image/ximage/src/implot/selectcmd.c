#include <stdlib.h>
#include "cpgplot.h"
#include "../include/xmtcl.h"
#include "cfortran.h"

#include "../include/xcommon.h"
#include "../include/cmddef.h"
#include "../include/null.h"
#include "../include/pgutil.h"

#define NORM  1
#define LINE  2
#define RECT  3
#define VLINE 4
#define XRNG  5
#define HLINE 6
#define YRNG  7
#define CROSS 8

Tcl_Obj *selectpt(Tcl_Interp *interp, int istk, int mode,
                  Tcl_Obj *inObj, char *button);
/*
 * selectcmd --
 * Tcl command to draw onto current PGPLOT device
 */
int
selectcmd(ClientData cdata,Tcl_Interp* interp,int objc, Tcl_Obj* CONST objv[] )
{
   int status, retcode;
   cmddef *curcmd;
   pardef *parlist;
   int i, isbox, isxrng, isyrng, iscross, istk, noerr;
   int color;
   float xpix, ypix, dr1, dr2;
   double dd;
   Tcl_Obj *cmdObj, *resObj, *tmpObj, *inObj, *colObj;
   char button;
   int clmode[2], nclick;

   curcmd = cdata;

   if ( !CParseParm(interp, objc, objv, curcmd) ) {
      return TCL_ERROR;
   }
   parlist = curcmd->parlist;

   color = -1;
   status = 0;
   isbox = 0;
   isxrng = 0;
   isyrng = 0;
   iscross = 0;
   xpix = RNULL();
   ypix = RNULL();
   noerr = 0;

   cgparl(parlist, "BOX", &isbox, &status);
   cgparl(parlist, "XRANGE", &isxrng, &status);
   cgparl(parlist, "YRANGE", &isyrng, &status);
   cgparl(parlist, "CROSS", &iscross, &status);
   cgparr(parlist, "XPIX", &xpix, &status);
   cgparr(parlist, "YPIX", &ypix, &status);
   cgpari(parlist, "COLOR", &color, &status);
   cgparl(parlist, "NOERR", &noerr, &status);

   Tcl_UnsetVar(interp, "select", 0); /* Ignore return value */

   if ( !ISDISPLAY() ) {
      cxwarn("No display", 10);
      return TCL_ERROR;
   }
   if ( !ISMOUSE() ) {
      cxwarn("Not interactive device", 10);
      return TCL_ERROR;
   }
   if ( curcmd->cmdargc != 0 ) {
      cwrongargs(curcmd->cmdargs);
      return TCL_ERROR;
   }

   istk = ISTK();
   button = ' ';
   inObj = NULL;
   resObj = NULL;
   cmdObj = NULL;
   dr1 = 0.;
   dr2 = 0.;

/*
 *  Invoke cursor based on parameters
 */
   if ( isbox ) {

      nclick = 2;
      clmode[0] = NORM;
      clmode[1] = RECT;

   } else if ( isxrng ) {

      nclick = 2;
      clmode[0] = VLINE;
      clmode[1] = XRNG;

   } else if ( isyrng ) {

      nclick = 2;
      clmode[0] = HLINE;
      clmode[1] = YRNG;

   } else if ( iscross ) {

      nclick = 1;
      clmode[0] = CROSS;

   } else if ( !ISRNULL(xpix) && !ISRNULL(ypix) ) {

     /* Draw line from anchor point (xpix,ypix) to current */

      nclick = 1;
      clmode[0] = LINE;

      /* Create xpix, ypix list for selectpt */

      inObj = Tcl_NewObj();
      dd = xpix;
      Tcl_ListObjAppendElement(interp, inObj, Tcl_NewDoubleObj(dd));
      dd = ypix;
      Tcl_ListObjAppendElement(interp, inObj, Tcl_NewDoubleObj(dd));

   } else {

     /* Simply select point */

      nclick = 1;
      clmode[0] = NORM;
   }
/*
 *  Change cursor color
 */
   cpgsave();
   colObj = NULL;
   if ( color != -1 ) {
      if ( istk ) {
         /* Save current color value */
         tmpObj = Tcl_ObjGetVar2(interp,
            Tcl_NewStringObj("pgtk::cursci", -1), NULL, 0);

         /* Effectively: set pgtk::cursci $color */
         cmdObj = Tcl_NewObj();
         Tcl_ListObjAppendElement(interp, cmdObj,
                                  Tcl_NewStringObj("set", -1));
         Tcl_ListObjAppendElement(interp, cmdObj,
                                  Tcl_NewStringObj("pgtk::cursci", -1));
         Tcl_ListObjAppendElement(interp, cmdObj,
                                  Tcl_NewIntObj(color));
         retcode = Tcl_EvalObjEx(interp, cmdObj, TCL_EVAL_GLOBAL);

         /* Construct color reset command */
         colObj = Tcl_NewObj();
         Tcl_ListObjAppendElement(interp, colObj,
                                  Tcl_NewStringObj("set", -1));
         Tcl_ListObjAppendElement(interp, colObj,
                                  Tcl_NewStringObj("pgtk::cursci", -1));
         Tcl_ListObjAppendElement(interp, colObj, tmpObj);
         Tcl_IncrRefCount(colObj);
      } else {
         cpgsci(color);
      }
   }

   /*
    *  Request necessary clicks
    */

   for ( i = 0; i < nclick; i++ ) {
      tmpObj = selectpt(interp, istk, clmode[i], inObj, &button);

      if ( !noerr && button == '3' ) {
         Tcl_SetObjResult(interp, 
             Tcl_NewStringObj("Cursor selection cancelled", -1));
         tmpObj = NULL;
      }
      if ( !tmpObj ) {
         cpgunsa();
         if ( colObj ) {
            Tcl_EvalObjEx(interp, colObj, TCL_EVAL_GLOBAL);
            Tcl_DecrRefCount(colObj);
         }
         return TCL_ERROR;
      }
      if ( i == 0 ) { 
         resObj = tmpObj;
         inObj = tmpObj;
         Tcl_IncrRefCount(resObj);
      } else { 
         Tcl_ListObjAppendList(interp, resObj, tmpObj);
      }
   }

   if ( button == '1' ) {
      Tcl_SetVar2(interp, "select", "btn", "lf", 0);
   } else if ( button == '2' ) {
      Tcl_SetVar2(interp, "select", "btn", "md", 0);
   } else if ( button == '3' ) {
      Tcl_SetVar2(interp, "select", "btn", "rt", 0);
   }

   /* Reset cursor color */

   cpgunsa();
   if ( colObj ) {
      Tcl_EvalObjEx(interp, colObj, TCL_EVAL_GLOBAL);
      Tcl_DecrRefCount(colObj);
   }
   Tcl_SetObjResult(interp, resObj);
   Tcl_DecrRefCount(resObj);

   return TCL_OK;
}

/*
 *  Helper function for selectcmd
 *  Select point using xtk-specific commands when necessary
 */
Tcl_Obj *selectpt(Tcl_Interp *interp, int istk, int mode,
                  Tcl_Obj *inObj, char *button) {

   Tcl_Obj *resObj, *cmdObj, *valObj;
   int retcode, pgstat;
   float dr1, dr2, xval, yval;
   double dd;
   char ch[2];

   pgstat = 1;
   retcode = TCL_OK;
   xval = 0.;
   yval = 0.;

   if ( istk ) {

   /* Start building pgtk::cursor command for tk case */

      cmdObj = Tcl_NewObj();
      Tcl_ListObjAppendElement(interp, cmdObj,
                              Tcl_NewStringObj("pgtk::cursor", -1));

   } else if ( inObj ) {

   /* Parse input point for non-tk case */

      Tcl_ListObjIndex(interp, inObj, 0, &valObj);
      Tcl_GetDoubleFromObj(interp, valObj, &dd);
      dr1 = dd;
      Tcl_ListObjIndex(interp, inObj, 1, &valObj);
      Tcl_GetDoubleFromObj(interp, valObj, &dd);
      dr2 = dd;
   }

   switch (mode) {

      case NORM:
         if ( istk ) {
            Tcl_ListObjAppendElement(interp, cmdObj,
                                     Tcl_NewStringObj("norm", -1));
            retcode = Tcl_EvalObjEx(interp, cmdObj, TCL_EVAL_GLOBAL);
         } else {
            pgstat = cpgband(0,0,dr1,dr2,&xval,&yval,ch);
         }
         break;

      case LINE:
         if ( istk ) {
            Tcl_ListObjAppendElement(interp, cmdObj,
                                     Tcl_NewStringObj("line", -1));
            Tcl_ListObjAppendList(interp, cmdObj, inObj);
            retcode = Tcl_EvalObjEx(interp, cmdObj, TCL_EVAL_GLOBAL);
         } else {
            pgstat = cpgband(1,0,dr1,dr2,&xval,&yval,ch);
         }
         break;

      case RECT:
         if ( istk ) {
            Tcl_ListObjAppendElement(interp, cmdObj,
                                     Tcl_NewStringObj("rect", -1));
            Tcl_ListObjAppendList(interp, cmdObj, inObj);
            retcode = Tcl_EvalObjEx(interp, cmdObj, TCL_EVAL_GLOBAL);
         } else {
            pgstat = cpgband(2,0,dr1,dr2,&xval,&yval,ch);
         }
         break;

      case VLINE:
         if ( istk ) {
            Tcl_ListObjAppendElement(interp, cmdObj,
                                     Tcl_NewStringObj("vline", -1));
            retcode = Tcl_EvalObjEx(interp, cmdObj, TCL_EVAL_GLOBAL);
         } else {
            pgstat = cpgband(6,0,dr1,dr2,&xval,&yval,ch);
         }
         break;

      case XRNG:
         if ( istk ) {
            Tcl_ListObjAppendElement(interp, cmdObj,
                                     Tcl_NewStringObj("xrng", -1));
            Tcl_ListObjAppendList(interp, cmdObj, inObj);
            retcode = Tcl_EvalObjEx(interp, cmdObj, TCL_EVAL_GLOBAL);
         } else {
            pgstat = cpgband(4,0,dr1,dr2,&xval,&yval,ch);
         }
         break;

      case HLINE:
         if ( istk ) {
            Tcl_ListObjAppendElement(interp, cmdObj,
                                     Tcl_NewStringObj("hline", -1));
            retcode = Tcl_EvalObjEx(interp, cmdObj, TCL_EVAL_GLOBAL);
         } else {
            pgstat = cpgband(5,0,dr1,dr2,&xval,&yval,ch);
         }
         break;

      case YRNG:
         if ( istk ) {
            Tcl_ListObjAppendElement(interp, cmdObj,
                                     Tcl_NewStringObj("yrng", -1));
            Tcl_ListObjAppendList(interp, cmdObj, inObj);
            retcode = Tcl_EvalObjEx(interp, cmdObj, TCL_EVAL_GLOBAL);
         } else {
            pgstat = cpgband(3,0,dr1,dr2,&xval,&yval,ch);
         }
         break;

      case CROSS:
         if ( istk ) {
            Tcl_ListObjAppendElement(interp, cmdObj,
                                     Tcl_NewStringObj("cross", -1));
            retcode = Tcl_EvalObjEx(interp, cmdObj, TCL_EVAL_GLOBAL);
         } else {
            pgstat = cpgband(7,0,dr1,dr2,&xval,&yval,ch);
         }
         break;

      default:
         return NULL;
   }

   if ( pgstat != 1 ) return NULL;
   if ( retcode == TCL_ERROR ) return NULL;

   if ( istk ) {
      *button = * Tcl_GetVar(interp, "pgtk::butpr", 0);
      resObj = Tcl_GetObjResult(interp);
   } else {
      if ( *ch == 'D' ) {
         *button = '2';
      } else if ( *ch == 'X' ) {
         *button = '3';
      } else {
         *button = '1';
      }
      resObj = Tcl_NewObj();
      dd = xval;
      Tcl_ListObjAppendElement(interp, resObj, Tcl_NewDoubleObj(dd));
      dd = yval;
      Tcl_ListObjAppendElement(interp, resObj, Tcl_NewDoubleObj(dd));
   }
   return resObj;
}
