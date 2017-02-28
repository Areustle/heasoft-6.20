/*
 *  Evaluate 1 and 2 float arg proc returning a float
 *    for use in map operation (mopwork.f)
 *
 *  call mopproc1(procname, arg1, result, status)
 *  call mopproc2(procname, arg1, arg2, result, status)
 *
 */ 

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "../include/xmtcl.h"
#include "cfortran.h"

void mopproc1(char *procname, float arg1, float *result, int *status) {
/*
 *  Execute tcl proc, which takes one float argument and returns
 *    a float value
 */

   Tcl_Obj *cmdObj, *resObj;
   double dd;
   int svecho, retcode;

   svecho = xm_echo_script;
   xm_echo_script = 0;

   cmdObj = Tcl_NewObj();
   Tcl_ListObjAppendElement(xm_interp, cmdObj,
                            Tcl_NewStringObj(procname, -1));
   dd = arg1;
   Tcl_ListObjAppendElement(xm_interp, cmdObj, Tcl_NewDoubleObj(dd));
   retcode = Tcl_EvalObjEx(xm_interp, cmdObj, TCL_EVAL_GLOBAL);
   xm_echo_script = svecho;
   if ( retcode == TCL_ERROR ) {
      *status = -1;
      return;
   }
   resObj = Tcl_GetObjResult(xm_interp);
   Tcl_GetDoubleFromObj(xm_interp, resObj, &dd);
   *result = dd;
   Tcl_ResetResult(xm_interp);
}
FCALLSCSUB4(mopproc1,MOPPROC1,mopproc1,STRING,FLOAT,PFLOAT,PINT)

void mopproc2(char *procname, float arg1, float arg2, float *result, 
              int *status) {
/*
 *  Execute tcl proc, which takes two float arguments and returns
 *    a float value
 */

   Tcl_Obj *cmdObj, *resObj;
   double dd;
   int retcode, svecho;

   svecho = xm_echo_script;
   xm_echo_script = 0;

   cmdObj = Tcl_NewObj();
   Tcl_ListObjAppendElement(xm_interp, cmdObj,
                            Tcl_NewStringObj(procname, -1));
   dd = arg1;
   Tcl_ListObjAppendElement(xm_interp, cmdObj, Tcl_NewDoubleObj(dd));
   dd = arg2;
   Tcl_ListObjAppendElement(xm_interp, cmdObj, Tcl_NewDoubleObj(dd));
   retcode = Tcl_EvalObjEx(xm_interp, cmdObj, TCL_EVAL_GLOBAL);
   xm_echo_script = svecho;
   if ( retcode == TCL_ERROR ) {
      *status = -1;
      return;
   }
   resObj = Tcl_GetObjResult(xm_interp);
   Tcl_GetDoubleFromObj(xm_interp, resObj, &dd);
   *result = dd;
   Tcl_ResetResult(xm_interp);
}
FCALLSCSUB5(mopproc2,MOPPROC2,mopproc2,STRING,FLOAT,FLOAT,PFLOAT,PINT)
