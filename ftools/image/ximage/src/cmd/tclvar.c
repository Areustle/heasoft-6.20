/*
 *  Routines to allow Tcl variable manipulation from FORTRAN
 *
 *  tclunset(name, isglobal, status)
 *
 *     Unsets named variable
 *
 *  tclvar[ilsrd](name, value, isreadonly, isglobal, status)
 *
 *     Sets Tcl variable (name) to (value). If (isreadonly)
 *     unchangeable from Tcl environment.  If (isglobal)
 *     set as global variable otherwise local to current level
 *
 *     i = integer, l = logical, s = string, r = real, d = double
 *
 *  tclavar[ilsrd](name, value, isreadonly, isglobal, status)
 *
 *     Appends (value) to Tcl variable. If (isreadonly)
 *     unchangeable from Tcl environment.  If (isglobal)
 *     set as global variable otherwise local to current level
 *
 *     i = integer, l = logical, s = string, r = real, d = double
 *
 *  tclvarlr(name, valary, valnum, isreadonly, isglobal, status)
 *
 *     Sets Tcl list (name) as the real values in (valary) which is
 *     (valnum) in length.  If (isreadonly)
 *     unchangeable from Tcl environment.  If (isglobal)
 *     set as global variable otherwise local to current level
 *
 *  tclrun(cmd, status)
 *
 *     Execute (cmd) as Tcl command
 *
 *  tclres[ilrd](cmd, value, status)
 *
 *     Execute (cmd) as Tcl command, returning the result of
 *     the command as (value)
 *
 *     i = integer, l = logical, r = real, d = double
 *
 *  tclress(cmd, value, maxlen, status)
 *
 *     Execute (cmd) as Tcl command, returning the result of
 *     the command as string (value).  Length cannot exceed (maxlen)
 *
 *  tclreslr(cmd, valary, valnum, maxlen, status)
 *
 *     Execute (cmd) as Tcl command, returning the list result of
 *     the command as an array of reals in (valary) and its length
 *     in (valnum).  The maximum size of (valary) is (maxlen)
 *
 *  tclresld(cmd, valary, valnum, maxlen, status)
 *
 *     Execute (cmd) as Tcl command, returning the list result of
 *     the command as an array of doubles in (valary) and its length
 *     in (valnum).  The maximum size of (valary) is (maxlen)
 *
 *  tclret[ilsrd](value, status)
 *
 *     Set (value) to be the command's result (i.e. the value returned)
 *
 */ 

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "../include/xmtcl.h"
#include "cfortran.h"

#include "../include/maxvals.h"
#include "../include/xcommon.h"
#include "../include/cmddef.h"
#include "../include/null.h"

/*
 *  Tcl command which protects variable from being changed
 */
char *ProtectVar(ClientData clientData, Tcl_Interp *interp,
                 CONST char *name1, CONST char *name2, int flags) {

   Tcl_Obj *savObj;
   char *msg;
   int totlen;

   totlen = 25;  /* Size of "Variable () is read-only" + 1 */
   if ( name1 ) { totlen += strlen(name1); }
   if ( name2 ) { totlen += strlen(name2); }
   msg = malloc(sizeof(char)*totlen);

   if ( msg ) {
      if ( name2 ) {
         sprintf(msg, "Variable %s(%s) is read-only", name1, name2);
      } else {
         sprintf(msg, "Variable %s is read-only", name1);
      }
      cxwrite(msg, 10);
      free(msg);
   } else {
      cxwrite("Variable is read-only", 10);
   }
   savObj = clientData;
   Tcl_SetVar2Ex(xm_interp, name1, name2, savObj, flags);
   return NULL;
   
}

void tclunset(const char *name, int isglobal, int *status) {
/*
 * Unset named Tcl variable
 *
 *   char *name  = variable or array element name [e.g. ival or size(x)]
 *   int isglobal     = if true is global variable
 *   int *status = OK is zero
 */
   int varflag;

   *status = 0;

   varflag = 0;
   if ( isglobal ) {
      varflag = varflag|TCL_GLOBAL_ONLY; 
   } 
   if ( Tcl_UnsetVar(xm_interp, name, varflag) == TCL_ERROR ) {
      *status = -1;
      Tcl_ResetResult(xm_interp);
   }
}

void tclvarobj(const char *name, Tcl_Obj *valObj, int isreadonly, 
               int isglobal, int isappended, int *status) {
/*
 *  Set Tcl variable with object
 *
 *   char *name  = variable or array element name [e.g. ival or size(x)]
 *   TclObj *value = value to set
 *   int isreadonly   = if true is protected as read-only
 *   int isglobal     = if true is global variable
 *   int isappended   = if true value is appended to existing value
 *   int *status = OK is zero
 */
   char *tmpstr;
   Tcl_Obj *nameObj, *curObj;
   Tcl_Obj *resObj;
   ClientData cdata;
   int varflag;

   *status = 0;

   varflag = 0;
   if ( isglobal )   { varflag = varflag|TCL_GLOBAL_ONLY; }
   if ( isappended ) { varflag = varflag|TCL_APPEND_VALUE|TCL_LIST_ELEMENT; }
/*
 * Remove existing read-only protection
 */
   cdata = Tcl_VarTraceInfo(xm_interp, name, varflag, ProtectVar,
                            (ClientData) NULL);
   curObj = (Tcl_Obj *) cdata;
   if ( curObj ) {
      Tcl_UntraceVar(xm_interp, name, varflag|TCL_TRACE_WRITES, 
                     ProtectVar, curObj);
      Tcl_DecrRefCount(curObj);
   }

   nameObj = Tcl_NewStringObj(name, -1);
   resObj = Tcl_ObjSetVar2(xm_interp, nameObj, NULL, valObj, varflag);
/* This causes problems with Tcl 8.5.x:
   Tcl_DecrRefCount(nameObj);
*/
   if ( resObj ) {
      Tcl_ResetResult(xm_interp);
/*
 * Add read-only protection
 */
      if ( isreadonly ) {
         Tcl_IncrRefCount(valObj);
         Tcl_TraceVar(xm_interp, name, varflag|TCL_TRACE_WRITES, ProtectVar,
                      valObj);
      }
   } else {
      tmpstr = strcatalloc(" Failed to set Tcl variable: ", name);
      if ( tmpstr ) {
         cxwrite(tmpstr, 10);
         free(tmpstr);
      }
      *status = -1;
   }
}
FCALLSCSUB3(tclunset,TCLUNSET,tclunset,STRING,LOGICAL,PINT)

void tclvari(const char *name, int value, int isreadonly, int isglobal, 
             int *status) {
/*
 *  Call tclvarobj with integer object
 */
   Tcl_Obj *intObj;

   intObj = Tcl_NewIntObj(value);
   tclvarobj(name, intObj, isreadonly, isglobal, 0, status);
}
FCALLSCSUB5(tclvari,TCLVARI,tclvari,STRING,INT,LOGICAL,LOGICAL,PINT)

void tclvarl(const char *name, int value, int isreadonly, int isglobal, 
             int *status) {
/*
 *  Alias tclvarl to tclvari
 */
   tclvari(name, value, isreadonly, isglobal, status);
}
FCALLSCSUB5(tclvarl,TCLVARL,tclvarl,STRING,LOGICAL,LOGICAL,LOGICAL,PINT)

void tclvars(const char *name, const char *value, int isreadonly, 
             int isglobal, int *status) {
/*
 *  Call tclvarobj with string object
 */
   Tcl_Obj *strObj;

   strObj = Tcl_NewStringObj(value, -1);
   tclvarobj(name, strObj, isreadonly, isglobal, 0, status);
}
FCALLSCSUB5(tclvars,TCLVARS,tclvars,STRING,STRING,LOGICAL,LOGICAL,PINT)

void tclvarr(const char *name, float value, int isreadonly, int isglobal, 
             int *status) {
/*
 *  Call tclvarobj with double object, given float
 */
   Tcl_Obj *dblObj;
   double dd;

   dd = value;
   dblObj = Tcl_NewDoubleObj(dd);
   tclvarobj(name, dblObj, isreadonly, isglobal, 0, status);
}
FCALLSCSUB5(tclvarr,TCLVARR,tclvarr,STRING,FLOAT,LOGICAL,LOGICAL,PINT)

void tclvard(const char *name, double value, int isreadonly, int isglobal, 
             int *status) {
/*
 *  Call tclvarobj with double object, given double
 */
   Tcl_Obj *dblObj;
   double dd;

   /*
    *  Since Tcl environment is not strongly typed, use real/float
    *  null value to represent all nulls
    */
   if ( ISDNULL(value) ) {
      dd = RNULL();
   } else {
      dd = value;
   }
   dblObj = Tcl_NewDoubleObj(dd);
   tclvarobj(name, dblObj, isreadonly, isglobal, 0, status);
}
FCALLSCSUB5(tclvard,TCLVARD,tclvard,STRING,DOUBLE,LOGICAL,LOGICAL,PINT)

void tclavari(const char *name, int value, int isreadonly, int isglobal, 
              int *status) {
/*
 *  Call tclvarobj, appending integer object
 */
   Tcl_Obj *intObj;

   intObj = Tcl_NewIntObj(value);
   tclvarobj(name, intObj, isreadonly, isglobal, 1, status);
}
FCALLSCSUB5(tclavari,TCLAVARI,tclavari,STRING,INT,LOGICAL,LOGICAL,PINT)

void tclavarl(const char *name, int value, int isreadonly, int isglobal, 
              int *status) {
/*
 *  Alias tclavarl to tclavari
 */
   tclavari(name, value, isreadonly, isglobal, status);
}
FCALLSCSUB5(tclavarl,TCLAVARL,tclavarl,STRING,LOGICAL,LOGICAL,LOGICAL,PINT)

void tclavars(const char *name, const char *value, int isreadonly, 
              int isglobal, int *status) {
/*
 *  Call tclvarobj, appending string object
 */
   Tcl_Obj *strObj;

   strObj = Tcl_NewStringObj(value, -1);
   tclvarobj(name, strObj, isreadonly, isglobal, 1, status);
}
FCALLSCSUB5(tclavars,TCLAVARS,tclavars,STRING,STRING,LOGICAL,LOGICAL,PINT)

void tclavarr(const char *name, float value, int isreadonly, int isglobal, 
              int *status) {
/*
 *  Call tclvarobj, appending double object, given float
 */
   Tcl_Obj *dblObj;
   double dd;

   dd = value;
   dblObj = Tcl_NewDoubleObj(dd);
   tclvarobj(name, dblObj, isreadonly, isglobal, 1, status);
}
FCALLSCSUB5(tclavarr,TCLAVARR,tclavarr,STRING,FLOAT,LOGICAL,LOGICAL,PINT)

void tclavard(const char *name, double value, int isreadonly, int isglobal, 
              int *status) {
/*
 *  Call tclvarobj, appending double object, given double
 */
   Tcl_Obj *dblObj;
   double dd;

   /*
    *  Since Tcl environment is not strongly typed, use real/float
    *  null value to represent all nulls
    */
   if ( ISDNULL(value) ) {
      dd = RNULL();
   } else {
      dd = value;
   }
   dblObj = Tcl_NewDoubleObj(dd);
   tclvarobj(name, dblObj, isreadonly, isglobal, 1, status);
}
FCALLSCSUB5(tclavard,TCLAVARD,tclavard,STRING,DOUBLE,LOGICAL,LOGICAL,PINT)

void tclvarlr(const char *name, float *valary, int valnum, int isreadonly, 
              int isglobal, int *status) {
/*
 *  Call tclvarobj with double list object, given array of reals
 */
   Tcl_Obj *dlstObj, *dblObj;
   int i;
   double dd;

   dlstObj = Tcl_NewListObj(0, (Tcl_Obj **) NULL);
   for ( i = 0; i < valnum; i++ ) {
      dd = valary[i];
      dblObj = Tcl_NewDoubleObj(dd);
      Tcl_ListObjAppendElement(xm_interp, dlstObj, dblObj);
   }
   tclvarobj(name, dlstObj, isreadonly, isglobal, 0, status);
}
FCALLSCSUB6(tclvarlr,TCLVARLR,tclvarlr,STRING,FLOATV,INT,LOGICAL,LOGICAL,PINT)

void tclrun(const char *cmd, int *status) {
/*
 *  Execute tcl command from C or FORTRAN
 */
   char *tmp;
   int svecho;

   svecho = xm_echo_script;
   xm_echo_script = 0;

   if ( !Tcl_EvalEx(xm_interp, cmd, -1, TCL_EVAL_GLOBAL) == TCL_OK ) {
      tmp = strcatalloc(" Failed to run tcl: ", cmd);
      cxwrite(tmp, 5);
      free(tmp);
      tmp = strcatalloc(" Tcl error: ", Tcl_GetStringResult(xm_interp));
      cxwrite(tmp, 5);
      free(tmp);
      *status = -1;
   }
   xm_echo_script = svecho;
   Tcl_ResetResult(xm_interp);
   
}
FCALLSCSUB2(tclrun,TCLRUN,tclrun,STRING,PINT)

void tclresobj(const char *cmd, Tcl_Obj **valObjPtr, int *status) {
/*
 *  Execute tcl command and retrieve object result
 */
   char *tmp;
   int svecho;

   svecho = xm_echo_script;
   xm_echo_script = 0;

   if ( !Tcl_EvalEx(xm_interp, cmd, -1, TCL_EVAL_GLOBAL) == TCL_OK ) {
      tmp = strcatalloc(" Tcl command: ", cmd);
      cxwrite(tmp, 10);
      free(tmp);
      tmp = strcatalloc(" Return msg : ", Tcl_GetStringResult(xm_interp));
      cxwrite(tmp, 10);
      free(tmp);
      *status = -1;
   }
   xm_echo_script = svecho;
   if ( *status == 0 ) {
      *valObjPtr = Tcl_GetObjResult(xm_interp);
   }
   
}

void tclresi(const char *cmd, int *value, int *status) {
/*
 *  Execute tcl command and retrieve integer result
 */
   Tcl_Obj *resObj;

   *status = 0;

   tclresobj(cmd, &resObj, status);
   if ( *status == 0 ) {
      if ( Tcl_GetIntFromObj(xm_interp, resObj, value) != TCL_OK ) {
         cxwrite(" Failed to get integer in tclresi", 5);
         *status = -1;
      }
   }
   Tcl_ResetResult(xm_interp);
}
FCALLSCSUB3(tclresi,TCLRESI,tclresi,STRING,PINT,PINT)

void tclresl(const char *cmd, int *value, int *status) {
/*
 *  Execute tcl command and retrieve boolean logical result
 */
   tclresi(cmd, value, status);
}
FCALLSCSUB3(tclresl,TCLRESL,tclresl,STRING,PLOGICAL,PINT)

void tclress(const char *cmd, char *value, int maxlen, int *status) {
/*
 *  Execute tcl command and retrieve string result
 */
   Tcl_Obj *resObj;
   char *tmp;

   *status = 0;

   tclresobj(cmd, &resObj, status);
   if ( *status == 0 ) {
      tmp = Tcl_GetString(resObj);
      if ( tmp ) {
         safestrcpy(value, tmp, maxlen);
      } else {
         cxwrite(" Failed to get string in tclress", 5);
         *status = -1;
      }
   }
   Tcl_ResetResult(xm_interp);
}
FCALLSCSUB4(tclress,TCLRESS,tclress,STRING,PSTRING,INT,PINT)

void tclresr(const char *cmd, float *value, int *status) {
/*
 *  Execute tcl command and retrieve integer result
 */
   Tcl_Obj *resObj;
   double dd;

   *status = 0;

   tclresobj(cmd, &resObj, status);
   if ( *status == 0 ) {
      if ( Tcl_GetDoubleFromObj(xm_interp, resObj, &dd) != TCL_OK ) {
         cxwrite(" Failed to get real in tclresr", 5);
         *status = -1;
      }
      *value = dd;
   }
   Tcl_ResetResult(xm_interp);
}
FCALLSCSUB3(tclresr,TCLRESR,tclresr,STRING,PFLOAT,PINT)

void tclresd(const char *cmd, double *value, int *status) {
/*
 *  Execute tcl command and retrieve integer result
 */
   Tcl_Obj *resObj;

   *status = 0;

   tclresobj(cmd, &resObj, status);
   if ( *status == 0 ) {
      if ( Tcl_GetDoubleFromObj(xm_interp, resObj, value) != TCL_OK ) {
         cxwrite(" Failed to get double in tclresd", 5);
         *status = -1;
      }
   }
   Tcl_ResetResult(xm_interp);
}
FCALLSCSUB3(tclresd,TCLRESD,tclresd,STRING,PDOUBLE,PINT)

void tclreslr(const char *cmd, float *valary, int *valnum, int maxlen, 
              int *status) {
/*
 *  Execute tcl command and retrieve real list result
 */
   int i, objc;
   Tcl_Obj *resObj, **objv;
   double dd;

   *status = 0;

   tclresobj(cmd, &resObj, status);
   if ( *status == 0 ) {
      Tcl_ListObjGetElements(xm_interp, resObj, &objc, &objv);
      for ( i = 0; i < objc; i++ ) {
         if ( i+1 > maxlen ) {
            cxwrite(" Array dim exceeded in tclreslr", 5);
            *status = -1;
            return;
         }
         if ( Tcl_GetDoubleFromObj(xm_interp, objv[i], &dd) != TCL_OK ) {
            cxwrite(" Failed to get real value in tclreslr", 5);
            *status = -1;
            return;
         }
         valary[i] = dd;
      }
      *valnum = objc;
   }
   Tcl_ResetResult(xm_interp);
}
FCALLSCSUB5(tclreslr,TCLRESLR,tclreslr,STRING,FLOATV,PINT,INT,PINT)

void tclresld(const char *cmd, double *valary, int *valnum, int maxlen, 
              int *status) {
/*
 *  Execute tcl command and retrieve double list result
 */
   int i, objc;
   Tcl_Obj *resObj, **objv;
   double dd;

   *status = 0;

   tclresobj(cmd, &resObj, status);
   if ( *status == 0 ) {
      Tcl_ListObjGetElements(xm_interp, resObj, &objc, &objv);
      for ( i = 0; i < objc; i++ ) {
         if ( i+1 > maxlen ) {
            cxwrite(" Array dim exceeded in tclresld", 5);
            *status = -1;
            return;
         }
         if ( Tcl_GetDoubleFromObj(xm_interp, objv[i], &dd) != TCL_OK ) {
            cxwrite(" Failed to get double value in tclresld", 5);
            *status = -1;
            return;
         }
         valary[i] = dd;
      }
      *valnum = objc;
   }
   Tcl_ResetResult(xm_interp);
}
FCALLSCSUB5(tclresld,TCLRESLD,tclresld,STRING,DOUBLEV,PINT,INT,PINT)

void tclreti(int value, int *status) {
/*
 *  Call Tcl_SetObjResult integer object
 */
   Tcl_Obj *intObj;

   intObj = Tcl_NewIntObj(value);
   Tcl_SetObjResult(xm_interp, intObj);
}
FCALLSCSUB2(tclreti,TCLRETI,tclreti,INT,PINT)

void tclretl(int value, int *status) {
/*
 *  Alias tclretl to tclreti
 */
   tclreti(value, status);
}
FCALLSCSUB2(tclretl,TCLRETL,tclretl,LOGICAL,PINT)

void tclrets(char *value, int *status) {
/*
 *  Call Tcl_SetObjResult string object
 */
   Tcl_Obj *strObj;

   strObj = Tcl_NewStringObj(value, -1);
   Tcl_SetObjResult(xm_interp, strObj);
}
FCALLSCSUB2(tclrets,TCLRETS,tclrets,STRING,PINT)

void tclretr(float value, int *status) {
/*
 *  Call Tcl_SetObjResult double object, given float
 */
   Tcl_Obj *dblObj;
   double dd;

   dd = value;
   dblObj = Tcl_NewDoubleObj(dd);
   Tcl_SetObjResult(xm_interp, dblObj);
}
FCALLSCSUB2(tclretr,TCLRETR,tclretr,FLOAT,PINT)

void tclretd(double value, int *status) {
/*
 *  Call Tcl_SetObjResult double object, given double
 */
   Tcl_Obj *dblObj;
   double dd;

   /*
    *  Since Tcl environment is not strongly typed, use real/float
    *  null value to represent all nulls
    */
   if ( ISDNULL(value) ) {
      dd = RNULL();
   } else {
      dd = value;
   }

   dblObj = Tcl_NewDoubleObj(dd);
   Tcl_SetObjResult(xm_interp, dblObj);
}
FCALLSCSUB2(tclretd,TCLRETD,tclretd,DOUBLE,PINT)
