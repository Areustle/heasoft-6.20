/*
 *  Define tcl command that parses parameter file
 */ 

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "../include/xmtcl.h"
#include "../include/maxvals.h"
#include "../include/xcommon.h"
#include "../include/cmddef.h"


int parparse(int objc, Tcl_Obj * CONST objv[], pardef *parlist, 
             argdef **pcmdargs) {
/*
 *  Take command array and set parameter entries, returning
 *  remaining arguments (pcmdargs)
 */

   char *curstr, *tmpstr, **parary, *curptr;
   argdef *rootarg, *curarg, **setargptr;
   int i, curlen, retval, ibeg, iend=0, done, openbr, brpair, qucnt, mergeop;
   char lastchr, endchr=0;
   Tcl_Obj *curObj;

   rootarg = NULL;
   setargptr = &rootarg;

   i = 0;
   while ( i < objc ) {

      curstr = stralloc(Tcl_GetString(objv[i]));
      curlen = strlen(curstr);

      /*
       *  If form parameter=\"word word word\"
       *  Join    {              } {  } {    } into
       *          {                          }
       *
       *  If form parameter={word word word}       (Open w/o closed)
       *  Join    {             } {  } {   } into
       *          {                        }
       *
       *  or      parameter=" word"                (Odd # of quotes)
       *  Join    {         } {   } into
       *          {               }
       */
      curptr = curstr;
      brpair = 0;
      openbr = 0;
      qucnt = 0;
      lastchr = '\0';
      while ( *curptr ) {
         if ( lastchr != '\\' ) {
            if ( *curptr == '{' ) {
	       openbr++;
	    } else if ( *curptr == '}' ) {
	       openbr--;
               brpair++;
	    }
	    if ( !openbr && *curptr == '"' ) {
               qucnt++;
	    }
         }
         lastchr = *curptr;
         curptr++;
      }

      mergeop = 0;
      if ( openbr > 0 ) {
         mergeop = 1;
         endchr = '}';
      } else if ( qucnt > 0 ) {
         if ( curstr[curlen-1] != '"' || qucnt % 2 != 0 ) { 
            mergeop = 1;
            endchr = '"';
         }
      }

      if ( mergeop ) {
         free(curstr);
         ibeg = i;
         i++;
         done = 0;
         while ( i < objc && !done ) {

            tmpstr = Tcl_GetString(objv[i]);
            if ( tmpstr[strlen(tmpstr)-1] == endchr && *tmpstr != endchr ) {
               done = 1;
               iend = i;
            }
            i++;
         }
         i--;
         if ( !done ) iend = objc-1;
         curObj = Tcl_ConcatObj(iend-ibeg+1, objv+ibeg);
         curstr = stralloc(Tcl_GetString(curObj));
      }

      /* Add arg entry to list */

      curarg = arginit();
      if ( !curarg ) {
         cxwrite("Bad arg entry", 5);
         free(curstr);
         return 0;
      }

      /* Split each arguments by = */

      if ( parlist ) {
         tmpstr = stralloc(curstr);
         parary = quosplit(tmpstr, "=");
         if ( !parary ) {
            cxwrite("Bad parameter array", 5);
            free(tmpstr);
            free(curstr);
            return 0;
         }
         /* If exactly the form name=value, set name and value */
         /* Otherwise assign entire argument to name           */

         if ( parary[0] && parary[1] && !parary[2] ) {
/*
 *  If value is quoted at this point, strip
 */
            curarg->name = stralloc(parary[0]);
            curlen = strlen(parary[1]);
            if ( (*parary[1] == '"' && *(parary[1]+curlen-1) == '"') ||
                 (*parary[1] == '{' && *(parary[1]+curlen-1) == '}'
                   && brpair == 1 ) ) {
               *(parary[1]+curlen-1) = '\0';
               curarg->value = stralloc(parary[1]+1);
            } else {
               curarg->value = stralloc(parary[1]);
            }
         } else {
            curarg->name = stralloc(curstr);
         }
         free(parary);
         free(tmpstr);
      } else {
         curarg->name = stralloc(curstr);
      }
      *setargptr = curarg;
      setargptr = &curarg->next;

      free(curstr);
      i++;
   }

   retval = parassign(parlist, &rootarg);
   if ( !retval && rootarg ) {
      argfree(rootarg);
      rootarg = NULL;
   }

   *pcmdargs = rootarg;
   return(retval);
}

int CParseParm(Tcl_Interp* interp, int objc, Tcl_Obj* CONST objv[], 
               cmddef *curcmd) {
/*
 *  Parser to use with Tcl command implemented in C
 *  Take objects from Tcl command and parameter list, setting
 *  parameters and returning argument list
 *  Also check for help
 */
   pardef *foundpar;
   argdef *curarg;

   if ( !curcmd ) { 
      cxwrite(" CParseParm: Command definition invalid", 10);
      return 0;
   }
   /* 
    *  Free command args from last invocation of command
    */
   if ( curcmd->cmdargs ) {
      argfree(curcmd->cmdargs);
      curcmd->cmdargs = NULL;
      curcmd->cmdargc = 0;
      curcmd->curarg = NULL;
   }
    
   if ( parparse(objc-1, objv+1, curcmd->parlist, &curcmd->cmdargs) ) {
      if ( curcmd->parlist ) {
         foundpar = parfind(curcmd->parlist, "help");  /* Check for help */
         if ( foundpar && * (int *) foundpar->value ) {
            Tcl_VarEval(interp, "parmhelp ", curcmd->name, (char *)NULL);
            return 0;
         }
      }
      curarg = curcmd->cmdargs;   /* Count number of arguments */
      while ( curarg ) {
         curcmd->cmdargc ++;
         curarg = curarg->next;
      }
      curcmd->curarg = curcmd->cmdargs;
   } else {
      return 0;
   }
   return 1;
}

void parexport(Tcl_Interp *interp, pardef *parlist, argdef *arglist) {
/*
 *  Export parameters into Tcl environment as associative array.
 *  Values are accessed with $parval(parname)
 */
    static char parval[] = "parval";
    static char cmdargv[] = "cmdargv";
    static char cmdargc[] = "cmdargc";

    int listcnt;
    Tcl_Obj *nameObj, *parObj, *valObj, *listObj;
    pardef *curpar;
    argdef *curarg;

    /* Free existing parval array */

    Tcl_UnsetVar(interp, parval, 0);
    
    /* Set parval array */

    curpar = parlist;
    nameObj = Tcl_NewStringObj(parval, -1);
    Tcl_IncrRefCount(nameObj);
    while ( curpar ) {
       parObj = Tcl_NewStringObj((char *)curpar->name, -1);
       Tcl_IncrRefCount(parObj);
       if ( curpar->value ) {   
          valObj = NULL;
          switch ( curpar->type ) {
             case 'b':
                valObj = Tcl_NewBooleanObj(* (int *) curpar->value);
                break;
             case 'i':
                valObj = Tcl_NewIntObj(* (int *) curpar->value);
                break;
             case 'r':
                valObj = Tcl_NewDoubleObj(* (double *) curpar->value);
                break;
             case 's':
                valObj = Tcl_NewStringObj((char *) curpar->value, -1);
                break;
          }
       } else {   /* If no value, define as empty string */
          valObj = Tcl_NewStringObj("", -1);
       }
       Tcl_IncrRefCount(valObj);
       Tcl_ObjSetVar2(interp, nameObj, parObj, valObj, 0);
       Tcl_DecrRefCount(valObj);
       Tcl_DecrRefCount(parObj);
       curpar = curpar->next;
    }
    Tcl_DecrRefCount(nameObj);

    /* Free existing cmdargv list */

    Tcl_UnsetVar(interp, cmdargv, 0);
    
    /* Set cmdargv list */

    listcnt = 0;
    listObj = Tcl_NewListObj(listcnt, (Tcl_Obj **) NULL);
    Tcl_IncrRefCount(listObj);
    curarg = arglist;
    while ( curarg ) {
       valObj = Tcl_NewStringObj((char *) curarg->name, -1);
       Tcl_IncrRefCount(valObj);
       Tcl_ListObjAppendElement(interp, listObj, valObj);
       Tcl_DecrRefCount(valObj);
       curarg = curarg->next;
       listcnt++;
    }

    /* Set cmdargc (Number of arguments) */

    nameObj = Tcl_NewStringObj(cmdargc, -1);
    Tcl_IncrRefCount(nameObj);
    valObj = Tcl_NewIntObj(listcnt);
    Tcl_IncrRefCount(valObj);
    Tcl_ObjSetVar2(interp, nameObj, (Tcl_Obj *) NULL, valObj, 0);
    Tcl_DecrRefCount(nameObj);
    Tcl_DecrRefCount(valObj);

    /* Set cmdargv (Argument list) */

    nameObj = Tcl_NewStringObj(cmdargv, -1);
    Tcl_IncrRefCount(nameObj);
    Tcl_ObjSetVar2(interp, nameObj, (Tcl_Obj *) NULL, listObj, 0);
    Tcl_DecrRefCount(nameObj);
    Tcl_DecrRefCount(listObj);
}

/*
 *  Tcl command which assigns parvals array based on input
 */
int ParseParmCmd(ClientData clientData, Tcl_Interp *interp,
                 int objc, Tcl_Obj *CONST objv[]) {

   int len, helpval, retval;
   char *parfile;
   pardef *parlist, *foundpar;
   argdef *rootarg;
   Tcl_Obj **argv;
   int argc;

   rootarg = NULL;

   if ( objc != 3 ) {
      Tcl_WrongNumArgs(interp, 1, objv, 
           "Usage: parseparm parfile arglist");
      return TCL_ERROR;
   }

   parfile = Tcl_GetStringFromObj(objv[1], &len);
   if ( !parfile ) return TCL_ERROR;

   /* Load parameters for command and set defaults*/

   parlist = parload(parfile);

   
   Tcl_ListObjGetElements(interp, objv[2], &argc, &argv);

   if ( parparse(argc, argv, parlist, &rootarg) ) {
/*
 *  Check for help
 */
      helpval = 0;
      foundpar = parfind(parlist, "help");
      if ( foundpar && foundpar->value ) {
         helpval = * (int *) foundpar->value;
      }
      if ( helpval ) {
         Tcl_VarEval(interp, "parmhelp ", parfile, (char *)NULL);
         retval = TCL_ERROR;
      } else {
         parexport(interp, parlist, rootarg);
         retval = TCL_OK;
      }
   } else {
      cxwrite("Command parse failure in ParseParm", 20);
      retval = TCL_ERROR;
   }
   parfree(parlist);
   argfree(rootarg);
   
   return retval;

}
