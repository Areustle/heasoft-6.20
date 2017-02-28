#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../include/maxvals.h"
#include "../include/xcommon.h"
#include "../include/xmtcl.h"
#include "../include/cmddef.h"
#include "cfortran.h"

/*
 * cfortran prototypes for the needed fortran calls.
 */

#define XIMCMD(ID,CMDSTR,STATUS) CCALLSFSUB3(XIMCMD,ximcmd,INT,STRING,PINT, ID,CMDSTR,STATUS)

/*
 * procfcmd --
 * Wrapper routine to take TCL command and feed it to ximage.
 */
int
procfcmd(ClientData cdata,Tcl_Interp* interp,int objc, Tcl_Obj* CONST objv[] )
{
   int status;
   cmddef *curcmd;

   curcmd = cdata;
   if ( !curcmd ) {
      cxwrite(" procfcmd: Command definition invalid", 10);
      return TCL_ERROR;
   }
   if ( !CParseParm(interp, objc, objv, curcmd) ) {
      return TCL_ERROR;
   }

   /* Call FORTRAN commands */

   XIMCMD(curcmd->id, curcmd->name, status); 

   if ( status != 0 ) return TCL_ERROR;
   return TCL_OK;
}
