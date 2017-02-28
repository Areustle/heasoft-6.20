/*
 *  Define script executing command, xsource
 */ 

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "../include/xmtcl.h"
#include <sys/stat.h>

#include "cfortran.h"
#include "../include/maxvals.h"
#include "../include/xcommon.h"

/*
 * Max TCL level for echoing commands
 *  and maximum length of echoed command
 */
#define XM_MAX_LEV 100
#define XM_ECHOLEN 256

Tcl_CmdObjTraceProc xm_echo_command; /* Defined after XSourceCmd */
static int cmdlevel = -1;  /* Tracks first command's level for indentation*/

/*
 *----------------------------------------------------------------------
 *
 * XsourceCmd --
 *
 *	This procedure is invoked to process the "xsource" Tcl command.
 *      A modified version of source where arguments after the filename
 *      are used to replace instances of %1% in the script where 1 is
 *      the number of the argument.
 * 
 *      For example: xsource add2.xco 2 4
 *                   where add2.xco => output %1%+%2%; puts $output
 *                   results in 6 being printed
 *
 * Results:
 *	A standard Tcl object result.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
XsourceCmd(dummy, interp, objc, objv)
    ClientData dummy;		/* Not used. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int objc;			/* Number of arguments. */
    Tcl_Obj *CONST objv[];	/* Argument objects. */
{
    int result;
    Tcl_StatBuf statBuf;

    Tcl_Channel chan;
    Tcl_Obj *pathPtr, *objPtr, *cmdObj, *cpyObj;
    Tcl_RegExp regExpr;
    static char percpat[] = "%([1-9][0-9]*)%";
    Tcl_RegExpInfo info;
    int numargs, offset, i, wlen, intrace;
    int exprstart, exprend, numstart, numend;

    static Tcl_Trace echotrace = NULL;
    
    if (objc < 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "fileName <args>");
	return TCL_ERROR;
    }

    pathPtr = Tcl_DuplicateObj(objv[1]);
    Tcl_IncrRefCount(pathPtr);
    if ( Tcl_FSConvertToPathType(interp, pathPtr) != TCL_OK ) {
       Tcl_DecrRefCount(pathPtr);
       return TCL_ERROR;
    }
/*
 *  The following code is a modified version of Tcl_FSEvalFile
 */

    if (Tcl_FSGetNormalizedPath(interp, pathPtr) == NULL) {
        Tcl_DecrRefCount(pathPtr);
        return TCL_ERROR;
    }

    result = TCL_ERROR;
    objPtr = Tcl_NewObj();
    Tcl_IncrRefCount(objPtr);

    if (Tcl_FSStat(pathPtr, &statBuf) == -1) {
        Tcl_AppendResult(interp, "couldn't read file \"",
                Tcl_GetString(pathPtr),
                "\": ", Tcl_PosixError(interp), (char *) NULL);
        goto end;
    }
    chan = Tcl_FSOpenFileChannel(interp, pathPtr, "r", 0644);
    if (chan == (Tcl_Channel) NULL) {
        Tcl_ResetResult(interp);
        Tcl_AppendResult(interp, "couldn't read file \"",
                Tcl_GetString(pathPtr),
                "\": ", Tcl_PosixError(interp), (char *) NULL);
        goto end;
    }
    /*
     * The eofchar is \32 (^Z).  This is the usual on Windows, but we
     * effect this cross-platform to allow for scripted documents.
     * [Bug: 2040]
     */
    Tcl_SetChannelOption(interp, chan, "-eofchar", "\32");
    if (Tcl_ReadChars(chan, objPtr, -1, 0) < 0) {
        Tcl_Close(interp, chan);
        Tcl_AppendResult(interp, "couldn't read file \"",
                Tcl_GetString(pathPtr),
                "\": ", Tcl_PosixError(interp), (char *) NULL);
        goto end;
    }
    if (Tcl_Close(interp, chan) != TCL_OK) {
        goto end;
    }

/* BEGIN -- Modifications from Tcl_FSEvalFile
 * Replaces all instances of %1% with argument number 1, same for 2,
 * etc.
 */
    numargs = objc - 2;
    offset = 0;
    cmdObj = Tcl_NewStringObj("", 0);
    Tcl_IncrRefCount(cmdObj);

/*  Match number between percent signs */
/*  percpat = "%([1-9][0-9]*)%" */

    regExpr = Tcl_RegExpCompile(interp, percpat);
    while ( Tcl_RegExpExecObj(interp, regExpr, objPtr, offset, 2, 0) ) {
       Tcl_RegExpGetInfo(regExpr, &info);

       /* Copy up to match */
       
       exprstart = offset + info.matches[0].start;
       exprend = offset + info.matches[0].end;
       cpyObj = Tcl_GetRange(objPtr, offset, exprstart-1);
       Tcl_AppendObjToObj(cmdObj, cpyObj);
       Tcl_DecrRefCount(cpyObj);

       /* Get number between the percents */

       numstart = offset + info.matches[1].start;
       numend = offset + info.matches[1].end;
       cpyObj = Tcl_GetRange(objPtr, numstart, numend-1);
       Tcl_GetIntFromObj(interp, cpyObj, &i);
       Tcl_DecrRefCount(cpyObj);

       /* Copy the appropriate argument */

       if ( i <= numargs ) {
          Tcl_AppendObjToObj(cmdObj, objv[i+1]);
       } else {
          cpyObj = Tcl_GetRange(objPtr, exprstart, exprend-1);
          Tcl_AppendObjToObj(cmdObj, cpyObj);
          Tcl_DecrRefCount(cpyObj);
       }
       
       offset = exprend;  /* Set end of match for start of next search */
    }
    /* Copy remaining text */

    wlen = Tcl_GetCharLength(objPtr);
    cpyObj = Tcl_GetRange(objPtr, offset, wlen-1);
    Tcl_AppendObjToObj(cmdObj, cpyObj);
    Tcl_DecrRefCount(cpyObj);

    /* Echo commands that are being executed from script file */
          
    cmdlevel = -1;   /* Initialize */
    intrace = 0;
    if ( echotrace ) {
       intrace = 1;
    } else {
       echotrace = Tcl_CreateObjTrace(interp, XM_MAX_LEV, 0,
                   (Tcl_CmdObjTraceProc *)xm_echo_command, (ClientData)NULL,
                   (Tcl_CmdObjTraceDeleteProc *) NULL);
    }
    result = Tcl_EvalObjEx(interp, cmdObj, 0);
    if ( !intrace ) {
       Tcl_DeleteTrace(interp, echotrace);
       echotrace = NULL;
    }

/* END -- Modifications from Tcl_FSEvalFile */

    if (result == TCL_ERROR) {
	char msg[200 + TCL_INTEGER_SPACE];

	/*
	 * Record information telling where the error occurred.
	 */

	sprintf(msg, "\n    (file \"%.150s\" line %d)", 
                Tcl_GetString(pathPtr),
		interp->errorLine);
	Tcl_AddErrorInfo(interp, msg);
    }

    end:
    Tcl_DecrRefCount(pathPtr);
    Tcl_DecrRefCount(objPtr);
    return result;
}

/*
 * xm_echo_command --
 * Routine to write the entered commands to the output if running a script
 */
int xm_echo_command
(ClientData cdata,Tcl_Interp *interp,int level,CONST char* command, 
        Tcl_Command cmdtoken, int objc, Tcl_Obj *CONST objv[] )
{
  int len;
  int i, sv_echo;
  int result;
  int true_lev = -1;
  char* logcmd  = NULL;
  char* logcmdPtr = NULL;
  char* trunc = " ...\n";
  char* nl = "\n";
  char *stmp=NULL;
  Tcl_Channel logchan;
  static int unknown_level = XM_MAX_LEV;
  int history_add = 0;
  static char* xm_tcl_log_prompt = "puts -nonewline \"!$xm_tcl_prompt\"";

  if (command == NULL || !xm_echo_script) return TCL_OK;
  if ( cmdlevel == -1 ) cmdlevel = level; /* Set to first command level */
                                          /* Used in spacing of printout*/
  /*
   * Suppress echoing of the command to turn on/off echoing!
   */
  if( strstr(command, "xm_echo_script") ) return TCL_OK;

  /*
   *  Don't echo commands in the tk:: or pgtk:: namespaces
   */
  if ( strstr(command, "tk::") ) { return TCL_OK; }

  /*
   * Keep track of whether or not we are in the unknown proc.
   * Do this by storing its level.
   */
  if ( level <= unknown_level ) unknown_level = XM_MAX_LEV + 1;
  
  if( Tcl_RegExpMatchObj(interp, objv[0], Tcl_NewStringObj("unknown", -1)) 
      && *command !=  '@'  && unknown_level == XM_MAX_LEV + 1 ) 
      unknown_level = level;
  if( Tcl_RegExpMatchObj(interp, objv[0], Tcl_NewStringObj("history", -1)) )
  {
	if ( objc > 1 && 
           Tcl_RegExpMatchObj(interp, objv[1], Tcl_NewStringObj("add", -1)) )
              history_add = 1 ;
  }

  /*
   * Run TCL commands to find out what the execution level is for this
   * command.
   */

  sv_echo = xm_echo_script;
  xm_echo_script = 0;

  result = Tcl_VarEval(interp, "info level", (char *)NULL);
  if( result == TCL_OK ) sscanf(Tcl_GetStringResult(interp), "%d", &true_lev);

  xm_echo_script = sv_echo;

  /*
   * Echo command to stdout
   */

  if ( !history_add && true_lev == 0 && level <= unknown_level )
  {

        logchan = Tcl_GetStdChannel(TCL_STDOUT);


        logcmd = (char *)malloc((MAX_CMDLEN+1)*sizeof(char));
        /*
         * Precede command by `level-cmdlevel' spaces, 
         * to indicate what the level is.
         */
        strcpy(logcmd,"");
        for( i=0; i<level-cmdlevel; i++) strcat(logcmd," ");

        /*
         * If the command string is longer than 80 characters, truncate it.
         * Replace any newline characters with spaces.
         */
        strncat(logcmd, command, MAX_CMDLEN - strlen(logcmd));

        /*
	 * for scripting files, replace the "&" used as a line
         * delimiter with newline characters 
         */
        if (strchr(command,'&') != NULL) {
           logcmdPtr = logcmd;
           while ( (logcmdPtr <  logcmd + strlen(logcmd)) &&
                    ((logcmdPtr = strpbrk(logcmdPtr,"&")) != NULL )) {
              *logcmdPtr = '\n';
              logcmdPtr++;
           }    
        }
        /*
         * Truncate at first newline character
         */
        stmp = strchr(logcmd, *nl);
        if ( stmp ) *stmp = '\0';

        /*
         *  Return with no output, if all spaces or widget under .ximage
         *  toplevel is command or runs tkScreen*, tkScale*, tkMenu*
         *  (Events generated by GUI)
         */
        if ( Tcl_RegExpMatch(interp, logcmd, "^\\s*$") ||
             Tcl_RegExpMatch(interp, logcmd, "^\\s*\\.ximage") ||
             Tcl_RegExpMatch(interp, logcmd, "^\\s*tkMenu") ||
             Tcl_RegExpMatch(interp, logcmd, "^\\s*tkScale") ||
             Tcl_RegExpMatch(interp, logcmd, "^\\s*tkScreen") ||
             Tcl_RegExpMatch(interp, logcmd, "^\\s*\\$mainpg") ||
             strstr(logcmd, "update idletasks") ||
             strstr(logcmd, "[.ximage") ) {
           free(logcmd);
           return TCL_OK;
        }

        if( (len = strlen(logcmd)) >= XM_ECHOLEN ) {
           strcpy(logcmd+(XM_ECHOLEN-strlen(trunc)), trunc);
        }
        else strcpy(logcmd+len, nl);

        /*
        * Print the prompt, preceded by a `!', if we are executing a
        * script
        */
        sv_echo = xm_echo_script;
        xm_echo_script = 0;
        result = Tcl_Eval(interp, xm_tcl_log_prompt);
        xm_echo_script = sv_echo;
        if( Tcl_Write(logchan, logcmd, -1) < 1 )
        {
           Tcl_AppendResult(interp, "Error writing command to log file.", 
                    (char *)NULL);
        }
        free (logcmd);
   }
   else Tcl_ResetResult(interp);

   return TCL_OK;
}
