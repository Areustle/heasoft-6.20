/*
 *  Define Tcl commands that access PGPLOT directly
 */ 

#include "../include/xcommon.h"

#define MAXPLANES 8
#define MAX_COLORS 256

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/stat.h>
#include <X11/Xlib.h>

#include "cpgplot.h"
#include "../include/xmtcl.h"
#include <tk.h>
#include "cfortran.h"

#define SETDISMAP(A,B) CCALLSFSUB2(SETDISMAP,setdismap,STRING,PINT, A,B)

/*
 *  Tcl command which calls PGCLOS
 */
int PgclosCmd(ClientData clientData, Tcl_Interp *interp,
              int objc, Tcl_Obj *CONST objv[]) {

   if ( objc != 1 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "");
      return TCL_ERROR;
   }

   cpgclos();
   return TCL_OK;
}
/*
 *  Tcl command which calls PGSLCT
 */
int PgslctCmd(ClientData clientData, Tcl_Interp *interp,
              int objc, Tcl_Obj *CONST objv[]) {
   int id;

   if ( objc != 2 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "<id>");
      return TCL_ERROR;
   }

   Tcl_GetIntFromObj(interp, objv[1], &id);
   cpgslct(id);
   return TCL_OK;
}
/*
 *  Tcl command which calls PGQID and returns value
 */
int PgqidCmd(ClientData clientData, Tcl_Interp *interp,
             int objc, Tcl_Obj *CONST objv[]) {
   int id;

   if ( objc != 1 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "");
      return TCL_ERROR;
   }

   cpgqid(&id);
   Tcl_SetObjResult(interp,Tcl_NewIntObj(id));
   return TCL_OK;
}
/*
 *  Tcl command which calls PGERAS
 */
int PgerasCmd(ClientData clientData, Tcl_Interp *interp,
              int objc, Tcl_Obj *CONST objv[]) {

   if ( objc != 1 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "");
      return TCL_ERROR;
   }

   cpgeras();
   return TCL_OK;
}
/*
 *  Tcl command which calls PGPAGE
 */
int PgpageCmd(ClientData clientData, Tcl_Interp *interp,
              int objc, Tcl_Obj *CONST objv[]) {

   if ( objc != 1 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "");
      return TCL_ERROR;
   }

   cpgpage();
   return TCL_OK;
}
/*
 *  Tcl command which calls PGBBUF
 */
int PgbbufCmd(ClientData clientData, Tcl_Interp *interp,
              int objc, Tcl_Obj *CONST objv[]) {

   if ( objc != 1 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "");
      return TCL_ERROR;
   }

   cpgbbuf();
   return TCL_OK;
}
/*
 *  Tcl command which calls PGEBUF
 */
int PgebufCmd(ClientData clientData, Tcl_Interp *interp,
              int objc, Tcl_Obj *CONST objv[]) {

   if ( objc != 1 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "");
      return TCL_ERROR;
   }

   cpgebuf();
   return TCL_OK;
}

/*
 *  Tcl command which returns current viewport as list
 */
int GetvpCmd(ClientData clientData, Tcl_Interp *interp,
              int objc, Tcl_Obj *CONST objv[]) {

   int i;
   float vp[4];
   double dd;
   Tcl_Obj *listObj, *tmpObj;

   if ( objc != 1 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "");
      return TCL_ERROR;
   }

   cpgqvp(0,&vp[0],&vp[1],&vp[2],&vp[3]);

   listObj = Tcl_NewObj();
   for ( i = 0; i < 4; i++ ) {
      dd = vp[i];
      tmpObj = Tcl_NewDoubleObj(dd);
      Tcl_ListObjAppendElement(interp, listObj, tmpObj);
   }
   Tcl_SetObjResult(interp, listObj);

   return TCL_OK;
}

/*
 *  Tcl command which sets current viewport from list
 */
int SetvpCmd(ClientData clientData, Tcl_Interp *interp,
              int objc, Tcl_Obj *CONST objv[]) {

   int i, lobjc;
   float vp[4];
   double dd;
   Tcl_Obj **lobjv;

   if ( objc != 2 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "{v1 v2 v3 v4}");
      return TCL_ERROR;
   }

   Tcl_ListObjGetElements(interp, objv[1], &lobjc, &lobjv);
   if ( lobjc != 4 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "{v1 v2 v3 v4}");
      return TCL_ERROR;
   }

   for ( i = 0; i < 4; i++ ) {
      Tcl_GetDoubleFromObj(interp, lobjv[i], &dd);
      vp[i] = dd;
   }

   cpgsvp(vp[0],vp[1],vp[2],vp[3]);

   return TCL_OK;
}

/*
 *  Tcl command which returns current window limits as list
 */
int GetwinCmd(ClientData clientData, Tcl_Interp *interp,
              int objc, Tcl_Obj *CONST objv[]) {

   int i;
   float win[4];
   double dd;
   Tcl_Obj *listObj, *tmpObj;

   if ( objc != 1 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "");
      return TCL_ERROR;
   }

   cpgqwin(&win[0],&win[1],&win[2],&win[3]);

   listObj = Tcl_NewObj();
   for ( i = 0; i < 4; i++ ) {
      dd = win[i];
      tmpObj = Tcl_NewDoubleObj(dd);
      Tcl_ListObjAppendElement(interp, listObj, tmpObj);
   }
   Tcl_SetObjResult(interp, listObj);

   return TCL_OK;
}

/*
 *  Tcl command which sets current window limits from list
 */
int SetwinCmd(ClientData clientData, Tcl_Interp *interp,
              int objc, Tcl_Obj *CONST objv[]) {

   int i, lobjc;
   float win[4];
   double dd;
   Tcl_Obj **lobjv;

   if ( objc != 2 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "{x1 x2 y1 y2}");
      return TCL_ERROR;
   }

   Tcl_ListObjGetElements(interp, objv[1], &lobjc, &lobjv);
   if ( lobjc != 4 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "{x1 x2 y1 y2}");
      return TCL_ERROR;
   }

   for ( i = 0; i < 4; i++ ) {
      Tcl_GetDoubleFromObj(interp, lobjv[i], &dd);
      win[i] = dd;
   }

   cpgswin(win[0],win[1],win[2],win[3]);

   return TCL_OK;
}

/*
 *  Tcl command which sets display map (Tcl and FORTRAN internal)
 */
int SetdismapCmd(ClientData clientData, Tcl_Interp *interp,
                 int objc, Tcl_Obj *CONST objv[]) {
   char *mapid;
   int status;

   if ( objc != 2 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "<mapid>");
      return TCL_ERROR;
   }

   mapid = Tcl_GetString(objv[1]);
   status = 0;
   SETDISMAP(mapid, status);

   if ( status == 0 ) { 
      return TCL_OK;
   } else {
      return TCL_ERROR;
   }
}

/*
 *  Tcl command to see if certain number of color cells are available
 */
int IsFreeCmapCmd(ClientData clientData, Tcl_Interp *interp,
               int objc, Tcl_Obj *CONST objv[]) {
   char *msg;
   int len, retval, status;

   Tk_Window dotwin;
   Display *disp;
   int screenIndex;
   Colormap cmap;
   unsigned long *plane_masks;
   unsigned long *pixels;
   int ncolors;

   Tcl_Obj *resObj;

   dotwin = Tk_NameToWindow(interp,".",Tk_MainWindow(interp));
   if ( !dotwin ) return TCL_ERROR;
   disp = Tk_Display(dotwin);
   screenIndex = DefaultScreen(disp);
   cmap = DefaultColormap(disp, screenIndex);

   if ( objc != 2 ) {
      Tcl_WrongNumArgs(interp, 1, objv, "<no. of color cells>");
      return TCL_ERROR;
   }

   plane_masks = (unsigned long *) ckalloc(MAXPLANES*sizeof(unsigned long));
   pixels = (unsigned long *) ckalloc(MAX_COLORS*sizeof(unsigned long));
   if ( !plane_masks || !pixels ) {
      cxwrite(" Allocation for color cell check failed", 5);
      return TCL_ERROR;
   }

   retval = Tcl_GetIntFromObj(interp, objv[1], &ncolors);
   if ( retval == TCL_OK ) {
      status = XAllocColorCells(disp,cmap,True,plane_masks,0,pixels,ncolors);
      if ( status != 0 ) {
         resObj = Tcl_NewIntObj(1);  /* Success */
         XFreeColors (disp, cmap, pixels, ncolors, 0);
      } else {
         resObj = Tcl_NewIntObj(0);  /* Failure */
      }
      Tcl_SetObjResult(interp, resObj);
      return TCL_OK;
   } else {
      return TCL_ERROR;
   }
}

void load_tclimplot (Tcl_Interp *interp) {
/*
 *  Load PGPLOT-related Tcl commands
 */

  Tcl_CreateObjCommand(interp,"pgtk::pgclos",PgclosCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"pgtk::pgslct",PgslctCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"pgtk::pgqid",PgqidCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"pgtk::pgeras",PgerasCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"pgtk::pgpage",PgpageCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"pgtk::pgbbuf",PgbbufCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"pgtk::pgebuf",PgebufCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"pgtk::getvp",GetvpCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"pgtk::setvp",SetvpCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"pgtk::getwin",GetwinCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"pgtk::setwin",SetwinCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"pgtk::setdismap",SetdismapCmd,
                       (ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"pgtk::isfreecmap",IsFreeCmapCmd,
                       (ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);
}
