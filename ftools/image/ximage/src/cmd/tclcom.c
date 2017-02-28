/*
 *  Define common Tcl commands implemented in C
 */ 

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/stat.h>

#include "../include/xmtcl.h"
#include "../include/xcommon.h"
#include "cfortran.h"

/*
 *  Tcl command which calls XWRITE
 */
int TxwriteCmd(ClientData clientData, Tcl_Interp *interp,
                 int objc, Tcl_Obj *CONST objv[]) {
   char *msg;
   int ichat, len, retval;

   if ( objc != 3 ) {
      Tcl_WrongNumArgs(interp, 1, objv, 
           "Usage: txwrite <string> <chat level>");
      return TCL_ERROR;
   }

   msg = Tcl_GetStringFromObj(objv[1], &len);
   if ( !msg ) return TCL_ERROR;
   retval = Tcl_GetIntFromObj(interp, objv[2], &ichat);
   if ( retval == TCL_OK ) {
      cxwrite(msg, ichat);
      return TCL_OK;
   } else {
      return TCL_ERROR;
   }
}

/*
 *  Tcl command which calls XCREAD
 */
int TxreadCmd(ClientData clientData, Tcl_Interp *interp,
              int objc, Tcl_Obj *CONST objv[]) {
   char *prompt;
   int len, status;
   char nulstr[1];
   char buffer[256];
   Tcl_Obj *objPtr;

   nulstr[0] = '\0';

   if ( objc > 2 ) {
      Tcl_WrongNumArgs(interp, 1, objv, 
           "Usage: txread <prompt>");
      return TCL_ERROR;
   }

   if ( objc == 1 ) {
      prompt = nulstr;
   } else {
      prompt = Tcl_GetStringFromObj(objv[1], &len);
      if ( !prompt ) return TCL_ERROR;
   }
      
   status = 0;
   xm_tcl_read(prompt, buffer, &status);
   if ( status == 0) {
      objPtr = Tcl_NewStringObj(buffer, -1);
      Tcl_SetObjResult(interp, objPtr);
      return TCL_OK;
   } else {
      return TCL_ERROR;
   }
}
