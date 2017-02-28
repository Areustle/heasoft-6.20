
#ifdef __cplusplus
extern "C" {
#endif
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../include/xmtcl.h"
#include "../include/xcommon.h"

#ifdef __cplusplus
}
#endif

/*
 * ximunk --
 *   Handling for unknown commands. Anything beginning in '@' is assumed
 *   to be the name of a script, and execution of the file as a script is
 *   attempted. Anything else gets passed to tcl's standard unknown handling
 *   function, assuming there is one [unknown is renamed to xstcl_unknown by
 *   the startup procedure Xspec_Init]. If for some reason there isn't, nothing
 *   is executed at all.
 */
int
ximunk(ClientData cdata,Tcl_Interp * interp,int objc, Tcl_Obj* CONST objv[] )
{
  int i, svecho, quocnt, brcnt, done;
  char *command, *curchr, *tmpstr;
  int result = TCL_OK;
  char **qualary, **curstrptr;
  int oargc, abbr;
  Tcl_Obj *cmdObj, *fileObj, *strObj, **oargv, *abbrObj;
  FILE *fp;
  char cmdstr[1000];
 
  /*
   * Check for problems
   */
  if ( objc < 2 ) {
        return TCL_ERROR;
  }
  
  /*
   * Put all the arguments together into one string for processing.
   */
  tmpstr = Tcl_GetString(objv[1]);
  command = stralloc(tmpstr);
 
  /*
   *  If command begins with @, assume it is a script 
   *  (.xco automatically appended if file doesn't exist)
   */
  if ( *command == '@' ) {

        cmdObj = Tcl_NewStringObj("xsource", -1);

        /* Determine script file name */

        fileObj = Tcl_NewStringObj(command+1, -1);

        /* Prefer file with .xco extension */
        sprintf(cmdstr, "%s.xco", command+1);
        fp = fopen(cmdstr, "r");
        if ( fp ) {
           fclose(fp);
           Tcl_AppendToObj(fileObj, ".xco", -1);
        }
        Tcl_ListObjAppendElement(interp, cmdObj, fileObj);

        /* Append remaining arguments */

        for ( i = 2; i < objc; i++ ) {
           Tcl_ListObjAppendElement(interp, cmdObj, objv[i]);
        }
        Tcl_ListObjGetElements(interp, cmdObj, &oargc, &oargv);
        result = Tcl_EvalObjv(interp, oargc, oargv, 0);
  }
  /*
   *  If command begins with $, assume it is meant run a shell command
   *  NOTE: $ls on the command line will be seen as a variable
   *        The $ must be escaped or quoted to make it to this stage
   */
  else if ( *command == '$' ) {

        cmdObj = Tcl_NewStringObj("syscall", -1);

        /* Determine shell command */

        if ( *(command+1) ) {
           strObj = Tcl_NewStringObj(command+1, -1);
           Tcl_ListObjAppendElement(interp, cmdObj, strObj);
        }

        /* Append remaining arguments */

        for ( i = 2; i < objc; i++ ) {
           Tcl_ListObjAppendElement(interp, cmdObj, objv[i]);
        }
        Tcl_ListObjGetElements(interp, cmdObj, &oargc, &oargv);
        result = Tcl_EvalObjv(interp, oargc, oargv, 0);
  }
  /*
   *  Assume syntax ending in /? is asking for parameter help
   */
  else if ( Tcl_RegExpMatch(interp,command,"/\\?$") ) {

     command[strlen(command)-2] = '\0';
     result = Tcl_VarEval(interp, command, " help", (char *)NULL);
  }
  /*
   *  Allow prior 'command/qual1=val1/qual2=val2' syntax
   */
  else if ( Tcl_RegExpMatch(interp,command,"^[_A-Za-z]+\\/") ) {

     free(command);
/*
 *  Append together all arguments until quoting is matched
 */
     strObj = Tcl_NewStringObj("",0);
     i = 1;
     done = 0;
     quocnt = 0;
     brcnt = 0;
     while ( i < objc && !done ) {
        Tcl_AppendObjToObj(strObj, objv[i]);
        tmpstr = Tcl_GetString(objv[i]);
        curchr = tmpstr;
        while ( *curchr ) {
           if ( *curchr == '{' ) {
              brcnt++;
           } else if ( *curchr == '}' ) {
              brcnt--;
           }
           if ( *curchr == '"' ) {
              if ( quocnt > 0 ) {
                 quocnt--;
              } else {
                 quocnt++;
              }
           }
           curchr++;
        }
        if ( quocnt == 0 && brcnt == 0 ) {
           done = 1;
        } else {
           Tcl_AppendToObj(strObj, " ", 1);
        }
        i++;
     }

     /* Split first chunk (i.e. only quals) on / and build list object */

     tmpstr = Tcl_GetString(strObj);
     command = stralloc(tmpstr);
     qualary = quosplit(command, "/");

     cmdObj = Tcl_NewObj();
     curstrptr = qualary;
     while ( *curstrptr ) {
        strObj = Tcl_NewStringObj(*curstrptr, -1);
        Tcl_ListObjAppendElement(interp, cmdObj, strObj);
        curstrptr++;
     }

     /* Append remaining arguments */

     while ( i < objc ) {
        Tcl_ListObjAppendElement(interp, cmdObj, objv[i]);
        i++;
     }

     Tcl_ListObjGetElements(interp, cmdObj, &oargc, &oargv);
     result = Tcl_EvalObjv(interp, oargc, oargv, 0);

     free(qualary);
  }
  /*
   *  Allow prior case-insensitive syntax
   */
  else if ( Tcl_RegExpMatch(interp,command,"^[_A-Z]+$") ) {
 
     curchr = command;
     while ( *curchr ) {
        *curchr = tolower(*curchr);
        curchr++;
     }

     cmdObj = Tcl_NewStringObj(command, strlen(command));
     for ( i = 2; i < objc; i++ ) {
        Tcl_ListObjAppendElement(interp, cmdObj, objv[i]);
     }
     Tcl_ListObjGetElements(interp, cmdObj, &oargc, &oargv);
     result = Tcl_EvalObjv(interp, oargc, oargv, 0);
  }
  /*
   *  If command begins with !, assume it is an old-style comment
   */
  else if ( *command == '!' ) {
     /* Do nothing */
  }
  else 
  {
  /* 
   *  Match command abbreviations even if not in interactive mode
   */
     abbr = 0;
     sprintf(cmdstr, "info commands %s*", command);
     svecho = xm_echo_script;
     xm_echo_script = 0;
     result = Tcl_EvalEx(interp, cmdstr, -1, 0);
     if ( result == TCL_OK ) {
        abbrObj = Tcl_GetObjResult(interp);
        Tcl_ListObjGetElements(interp, abbrObj, &oargc, &oargv);
        if ( oargc == 1 ) {
           cmdObj = oargv[0];
           for ( i = 2; i < objc; i++ ) {
              Tcl_ListObjAppendElement(interp, cmdObj, objv[i]);
           }
           Tcl_IncrRefCount(cmdObj);
           Tcl_ListObjGetElements(interp, cmdObj, &oargc, &oargv);
           result = Tcl_EvalObjv(interp, oargc, oargv, 0);
           Tcl_DecrRefCount(cmdObj);
           abbr = 1;
        }
     }
     xm_echo_script = svecho;
  /*
  * Okay, so now try feeding it to the standard Tcl command and see if
  * it can process it.
  */
     if ( !abbr ) {
        cmdObj = Tcl_NewStringObj("tclunknown", 10);
        for ( i = 1; i < objc; i++ ) {
           Tcl_ListObjAppendElement(interp, cmdObj, objv[i]);
        }
        Tcl_ListObjGetElements(interp, cmdObj, &oargc, &oargv);
        result = Tcl_EvalObjv(interp, oargc, oargv, 0);
     }
  }

  free(command);

  return result;
}
