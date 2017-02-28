/*
 *  Journalling: The recording of plot commands to 
 *               make sure nothing is lost on refresh
 */ 

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "cpgplot.h"
#include "../include/xmtcl.h"
#include "cfortran.h"

#include "../include/maxvals.h"
#include "../include/xcommon.h"
#include "../include/cmddef.h"
#include "../include/pgutil.h"

/*
 *  Start new journal
 */
void jrnnew(int *status) {

   if ( *status != 0 ) return;
   Tcl_Eval(xm_interp, "pgtk::journal {}");
}
FCALLSCSUB1(jrnnew,JRNNEW,jrnnew,PINT)

/*
 *  Journal (record) current command
 */ 

void cjrncmd(cmddef *curcmd, int *status) {
/*
 *  Journal command corresponding to cmdid (FORTRAN)
 */
   pardef *curpar;
   argdef *curarg;
   Tcl_Obj *cmdList, *tmpObj, *parObj;
   int svecho;
   
   if ( !curcmd ) {
      cxwrite(" cjrncmd: Command definition invalid", 10);
      *status = -1;
      return;
   }
   svecho = xm_echo_script;
   xm_echo_script = 0;
/*
 *  Start with command name
 */
   cmdList = Tcl_NewListObj(0, (Tcl_Obj **) NULL);
   tmpObj = Tcl_NewStringObj(curcmd->name, -1);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
   curpar = curcmd->parlist;
/*
 *  Build parameter entry
 */
   while ( curpar ) {
      parObj = (Tcl_Obj *) NULL;
      if ( curpar->value ) {
         switch (curpar->type) {
            case 'b': 
               if ( * (int *) curpar->value ) {
                  parObj = Tcl_NewStringObj(curpar->name, -1);
               }
               break;
            case 'i': 
               parObj = Tcl_NewStringObj(curpar->name, -1);
               Tcl_AppendToObj(parObj, "=", 1);
               tmpObj = Tcl_NewIntObj(* (int *) curpar->value);
               Tcl_AppendObjToObj(parObj, tmpObj);
               break; 
            case 'r': 
               parObj = Tcl_NewStringObj(curpar->name, -1);
               Tcl_AppendToObj(parObj, "=", 1);
               tmpObj = Tcl_NewDoubleObj(* (double *) curpar->value);
               Tcl_AppendObjToObj(parObj, tmpObj);
               break;
            case 's': 
               parObj = Tcl_NewStringObj(curpar->name, -1);
               Tcl_AppendToObj(parObj, "=", 1);
               Tcl_AppendToObj(parObj, (char *) curpar->value, -1);
               break;
            default: 
               break;
         }
      }
/*
 *  Append each parameter
 */
      if ( parObj ) {
         Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
      }
      curpar = curpar->next;
   }
/*
 *  Append each command argument
 */
   curarg = curcmd->cmdargs;
   while ( curarg ) {
      tmpObj = Tcl_NewStringObj(curarg->name, -1);
      Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
      curarg = curarg->next;
   }
/*
 *  Execute journal command with command list as argument
 */
   tmpObj = Tcl_NewStringObj("pgtk::journal", -1);
   Tcl_ListObjAppendElement(xm_interp, tmpObj, cmdList);
   Tcl_EvalObjEx(xm_interp, tmpObj, 0);
   /* Make sure display is updated after every plotting command */
   Tcl_EvalEx(xm_interp, "update idletasks", -1, TCL_EVAL_GLOBAL);
   Tcl_ResetResult(xm_interp);
   xm_echo_script = svecho;
}

/*
 *  Journal shortcuts - Constructing a Tcl command in FORTRAN
 *  is cumbersome and loses precision excessively, so routines for
 *  drawing box, circle, and label are implemented here
 *  for usage in that context
 */

void jrncir(float xcen, float ycen, float rad, int color, int lwidth,
            int lstyle) {
/*
 *  Construct and eval: 
 *  draw circle color=$color lwidth=$lwidth lstyle=$lstyle $xcen $ycen $rad
 *
 *  Note: If color, lwidth, or lstyle are < 0, they are omitted
 */
   Tcl_Obj *cmdList, *tmpObj, *parObj;
   int di, svecho;

   svecho = xm_echo_script;
   xm_echo_script = 0;
/*
 *  Start with command name
 *  {draw circle}
 */
   cmdList = Tcl_NewListObj(0, (Tcl_Obj **) NULL);
   tmpObj = Tcl_NewStringObj("draw", -1);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
   tmpObj = Tcl_NewStringObj("circle", -1);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
/*
 *  Build parameter entries and append them
 *  {color=$color lwidth=$lwidth lstyle=$lstyle}
 */
   if ( color < 0 ) {
      cpgqci(&di);
      tmpObj = Tcl_NewIntObj(di);
   } else {
      tmpObj = Tcl_NewIntObj(color);
   }
   parObj = Tcl_NewStringObj("color=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
   
   if ( lwidth < 0 ) {
      cpgqlw(&di);
      tmpObj = Tcl_NewIntObj(di);
   } else {
      tmpObj = Tcl_NewIntObj(lwidth);
   }
   parObj = Tcl_NewStringObj("lwidth=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
   
   if ( lstyle < 0 ) {
      cpgqls(&di);
      tmpObj = Tcl_NewIntObj(di);
   } else {
      tmpObj = Tcl_NewIntObj(lstyle);
   }
   parObj = Tcl_NewStringObj("lstyle=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
/*
 *  Append each command argument
 *  {$xcen $ycen $rad}
 */
   tmpObj = Tcl_NewDoubleObj(xcen);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
   tmpObj = Tcl_NewDoubleObj(ycen);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
   tmpObj = Tcl_NewDoubleObj(rad);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
/*
 *  Execute draw command (journalling-enabled)
 */
   Tcl_EvalObjEx(xm_interp, cmdList, 0);
   
   xm_echo_script = svecho;
}
FCALLSCSUB6(jrncir,JRNCIR,jrncir,FLOAT,FLOAT,FLOAT,INT,INT,INT)


void jrnbox(float xcen, float ycen, float xwid, float ywid, float angle,
            int color, int lwidth, int lstyle) {
/*
 *  Construct and eval: 
 *  draw box color=$color lwidth=$lwidth lstyle=$lstyle \
 *           $xcen $ycen $xwid $ywid $rad
 *
 *  Note: If color, lwidth, or lstyle are < 0, they are omitted
 */
   Tcl_Obj *cmdList, *tmpObj, *parObj;
   int di, svecho;

   svecho = xm_echo_script;
   xm_echo_script = 0;
/*
 *  Start with command name
 *  {draw box}
 */
   cmdList = Tcl_NewListObj(0, (Tcl_Obj **) NULL);
   tmpObj = Tcl_NewStringObj("draw", -1);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
   tmpObj = Tcl_NewStringObj("box", -1);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
/*
 *  Build parameter entries and append them
 *  {color=$color lwidth=$lwidth lstyle=$lstyle}
 */
   if ( color < 0 ) {
      cpgqci(&di);
      tmpObj = Tcl_NewIntObj(di);
   } else {
      tmpObj = Tcl_NewIntObj(color);
   }
   parObj = Tcl_NewStringObj("color=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);

   if ( lwidth < 0 ) {
      cpgqlw(&di);
      tmpObj = Tcl_NewIntObj(di);
   } else {
      tmpObj = Tcl_NewIntObj(lwidth);
   }
   parObj = Tcl_NewStringObj("lwidth=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);

   if ( lstyle < 0 ) {
      cpgqls(&di);
      tmpObj = Tcl_NewIntObj(di);
   } else {
      tmpObj = Tcl_NewIntObj(lstyle);
   }
   parObj = Tcl_NewStringObj("lstyle=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
/*
 *  Append each command argument
 *  {$xcen $ycen $xwid $ywid $angle}
 */
   tmpObj = Tcl_NewDoubleObj(xcen);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
   tmpObj = Tcl_NewDoubleObj(ycen);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
   tmpObj = Tcl_NewDoubleObj(xwid);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
   tmpObj = Tcl_NewDoubleObj(ywid);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
   tmpObj = Tcl_NewDoubleObj(angle);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
/*
 *  Execute draw command (journalling-enabled)
 */
   Tcl_EvalObjEx(xm_interp, cmdList, 0);
   xm_echo_script = svecho;
}
FCALLSCSUB8(jrnbox,JRNBOX,jrnbox,FLOAT,FLOAT,FLOAT,FLOAT,FLOAT,INT,INT,INT)

void jrnpoly(float *pts, int npts, int color, int lwidth, int lstyle) {
/*
 *  Construct and eval: 
 *  draw polygon color=$color lwidth=$lwidth lstyle=$lstyle \
 *           pts[0] pts[1] ... pts[npts-1]
 *
 *  Note: If color, lwidth, or lstyle are < 0, they are omitted
 */
   Tcl_Obj *cmdList, *tmpObj, *parObj;
   int i, di, svecho;

   svecho = xm_echo_script;
   xm_echo_script = 0;
/*
 *  Start with command name
 *  {draw polygon}
 */
   cmdList = Tcl_NewListObj(0, (Tcl_Obj **) NULL);
   tmpObj = Tcl_NewStringObj("draw", -1);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
   tmpObj = Tcl_NewStringObj("polygon", -1);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
/*
 *  Build parameter entries and append them
 *  {color=$color lwidth=$lwidth lstyle=$lstyle}
 */
   if ( color < 0 ) {
      cpgqci(&di);
      tmpObj = Tcl_NewIntObj(di);
   } else {
      tmpObj = Tcl_NewIntObj(color);
   }
   parObj = Tcl_NewStringObj("color=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);

   if ( lwidth < 0 ) {
      cpgqlw(&di);
      tmpObj = Tcl_NewIntObj(di);
   } else {
      tmpObj = Tcl_NewIntObj(lwidth);
   }
   parObj = Tcl_NewStringObj("lwidth=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);

   if ( lstyle < 0 ) {
      cpgqls(&di);
      tmpObj = Tcl_NewIntObj(di);
   } else {
      tmpObj = Tcl_NewIntObj(lstyle);
   }
   parObj = Tcl_NewStringObj("lstyle=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
/*
 *  Append all pts
 */
   for ( i = 0; i < npts; i++ ) {
      tmpObj = Tcl_NewDoubleObj(pts[i]);
      Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
   }
/*
 *  Execute draw command (journalling-enabled)
 */
   Tcl_EvalObjEx(xm_interp, cmdList, 0);
   xm_echo_script = svecho;
}

void jrnlab(float x, float y, char *text, char *font, int color, 
            float csize, int lwidth, int symbol, int symcolor, 
            float symcsize, int symlwidth, char *just, float angle) {
/*
 *  Construct and eval: 
 *  label xpix=$x ypix=$x font=$font csize=$csize color=$color
 *        lwidth=$lwidth symbol=$symbol symcsize=$symcsize
 *        symcolor=$symcolor symlwidth=$symlwidth just=$just
 *        angle=$angle clip
 *
 *  Note: If symcolor, symlwidth, symlstyle, symcsize,
 *  or symbol  are < 0, they are omitted.
 */
   Tcl_Obj *cmdList, *tmpObj, *parObj;
   int di, svecho;
   float dr;
   double dd;
   char ds[100];

   svecho = xm_echo_script;
   xm_echo_script = 0;
/*
 *  Start with command name
 *  {label}
 */
   cmdList = Tcl_NewListObj(0, (Tcl_Obj **) NULL);
   tmpObj = Tcl_NewStringObj("label", -1);
   Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
/*
 *  Build parameter entries and append them
 *  {xpix=$x ypix=$x font=$font csize=$csize color=$color
 *   lwidth=$lwidth symbol=$symbol symcsize=$symcsize
 *   symcolor=$symcolor symlwidth=$symlwidth just=$just
 *   angle=$angle}
 */
   parObj = Tcl_NewStringObj("xpix=", -1);
   dd = x;
   tmpObj = Tcl_NewDoubleObj(dd);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);

   parObj = Tcl_NewStringObj("ypix=", -1);
   dd = y;
   tmpObj = Tcl_NewDoubleObj(dd);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);

   
   if ( *font ) {
      tmpObj = Tcl_NewStringObj(font, -1);
   } else {
      cpgqcf(&di);
      sprintf(ds, "%d", di);
      tmpObj = Tcl_NewStringObj(ds, -1);
   }
   parObj = Tcl_NewStringObj("font=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
   
   if ( color < 0 ) {
      cpgqci(&di);
      tmpObj = Tcl_NewIntObj(di);
   } else {
      tmpObj = Tcl_NewIntObj(color);
   }
   parObj = Tcl_NewStringObj("color=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);

   if ( csize < 0. ) {
      cpgqch(&dr);
      dd = dr;
   } else { 
      dd = csize;
   }
   tmpObj = Tcl_NewDoubleObj(dd);
   parObj = Tcl_NewStringObj("csize=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
   
   if ( lwidth < 0 ) {
      cpgqlw(&di);
      tmpObj = Tcl_NewIntObj(di);
   } else {
      tmpObj = Tcl_NewIntObj(lwidth);
   }
   parObj = Tcl_NewStringObj("lwidth=", -1);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);

   if ( symbol != -1 ) {
      parObj = Tcl_NewStringObj("symbol=", -1);
      tmpObj = Tcl_NewIntObj(symbol);
      Tcl_AppendObjToObj(parObj, tmpObj);
      Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
   }
   if ( symcolor >= 0 ) {
      parObj = Tcl_NewStringObj("symcolor=", -1);
      tmpObj = Tcl_NewIntObj(symcolor);
      Tcl_AppendObjToObj(parObj, tmpObj);
      Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
   }
   if ( symcsize > 0. ) {
      parObj = Tcl_NewStringObj("symcsize=", -1);
      dd = symcsize;
      tmpObj = Tcl_NewDoubleObj(dd);
      Tcl_AppendObjToObj(parObj, tmpObj);
      Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
   }
   if ( symlwidth >= 0 ) {
      parObj = Tcl_NewStringObj("symlwidth=", -1);
      tmpObj = Tcl_NewIntObj(symlwidth);
      Tcl_AppendObjToObj(parObj, tmpObj);
      Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
   }
   if ( *just ) {
      parObj = Tcl_NewStringObj("just=", -1);
      tmpObj = Tcl_NewStringObj(just, -1);
      Tcl_AppendObjToObj(parObj, tmpObj);
      Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
   }
   parObj = Tcl_NewStringObj("angle=", -1);
   dd = angle;
   tmpObj = Tcl_NewDoubleObj(dd);
   Tcl_AppendObjToObj(parObj, tmpObj);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);

   parObj = Tcl_NewStringObj("clip", -1);
   Tcl_ListObjAppendElement(xm_interp, cmdList, parObj);
/*
 *  Append each command argument
 *  {$text}
 */
   if ( *text ) {
      tmpObj = Tcl_NewStringObj(text, -1);
      Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
   }
/*
 *  Execute label command (journalling-enabled)
 */
   Tcl_EvalObjEx(xm_interp, cmdList, 0);
   xm_echo_script = svecho;
}
FCALLSCSUB13(jrnlab,JRNLAB,jrnlab,FLOAT,FLOAT,STRING,STRING,INT,FLOAT,INT,INT,INT,FLOAT,INT,STRING,FLOAT)
