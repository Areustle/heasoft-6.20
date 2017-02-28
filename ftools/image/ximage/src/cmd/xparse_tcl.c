/*
 *  Replacements for xanlib i/o to reroute through Tcl channels
 */

#ifdef __cplusplus
extern "C" {
#endif

#       include <stdlib.h>
#       include <stdio.h>
#       include <string.h>
#       include <errno.h>
#       include "../include/xmtcl.h"
#       include <cfortran.h>

/* Token for command tracer which echos the commands. */
Tcl_Trace XM_Echo_Trace;


/* Tcl and link C variable as to whether to echo commands from a script.*/
int xm_echo_script;

/* Tcl and linked C variable to control whether or not commands return
 * Tcl results. Only included for backwards compatibility with v10.0 */
int xm_return_result = 0 ;


#ifdef __cplusplus
}
#endif

/*
 * xm_tcl_write --
 * Routine for sending output thru the TCL standard
 * output channel.  Emulates the xanlib routine FXWRITE
 */
int xm_tcl_write(char *wrtstr,int  idest)
{
  char *nl = "\n";
  Tcl_Channel OutChan;
  Tcl_Channel LogChan;
  int StdType=0;

  if( idest==0 ) return 0;

  if( idest & 1 ) StdType = TCL_STDOUT;
  else if( idest & 2 ) StdType = TCL_STDERR;

  if( idest==5 || idest==6 )
  {
        OutChan = Tcl_GetStdChannel(StdType);
  }
  else
  {
        LogChan = Tcl_GetChannel(xm_interp, xm_log_out, NULL);
        Tcl_ResetResult(xm_interp);
        if( !( idest&4 ) && LogChan == NULL )  OutChan = Tcl_GetStdChannel(StdType);
        else if( idest==1 && LogChan != NULL ) OutChan = XM_Stdout_Chan;
        else if( idest==2 && LogChan != NULL ) OutChan = XM_Stderr_Chan;
        else if( idest==4 && LogChan != NULL ) OutChan = XM_Logfile_Chan;
        else return TCL_OK;
  }

#ifdef __tk   /* if we are building for the GUI version */

  if ( OutChan==XM_Logfile_Chan ) 
  {
        Tcl_Write(OutChan, wrtstr, -1);
        Tcl_Write(OutChan, nl, 1);
        Tcl_Flush(OutChan);
  }
  else
  {
        Tcl_SetVar(xm_interp,"log_mess",wrtstr,
                   TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG|TCL_APPEND_VALUE);  
        Tcl_SetVar(xm_interp,"log_mess",nl,
                   TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG|TCL_APPEND_VALUE);
  }

#else  /* if we are building for the standard non-GUI version */

  Tcl_Write(OutChan, wrtstr, -1);
  Tcl_Write(OutChan, nl, 1);
  Tcl_Flush(OutChan);

#endif

  return TCL_OK;
}



/*
 * xm_tcl_read --
 *   Routine for read input from the TCL input channel.
 * Emulates XCREAD, etc, but assumes interactive input.
 */
int xm_tcl_read(char *prompt, char *buffer, int *ierr)

{
  Tcl_Channel OutChan, InChan;
  char *nl = "\n", *bk = "/*";
  CONST char *InName;
  Tcl_Obj *cmdList, *tmpObj, *resObj;

  OutChan = Tcl_GetStdChannel(TCL_STDOUT);
  InChan  = Tcl_GetStdChannel(TCL_STDIN);
  InName  = Tcl_GetChannelName(InChan);

#ifndef __CYGWIN__
/*
 *  Call "tclreadline::readline read" with prompt
 */

  cmdList = Tcl_NewObj();
  tmpObj = Tcl_NewStringObj("tclreadline::readline", -1);
  Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
  tmpObj = Tcl_NewStringObj("read", -1);
  Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
  tmpObj = Tcl_NewStringObj(prompt, -1);
  Tcl_ListObjAppendElement(xm_interp, cmdList, tmpObj);
  Tcl_EvalObjEx(xm_interp, cmdList, TCL_EVAL_GLOBAL);
#else
/*
 *  Use Tcl gets command to prompt user
 */

  cxwrite(prompt, 10);
  Tcl_EvalEx(xm_interp, "gets stdin", -1, TCL_EVAL_GLOBAL);
#endif

  resObj = Tcl_GetObjResult(xm_interp);
  strcpy(buffer, Tcl_GetString(resObj) );
  
  
  /*
   * If a log file is open, echo to it.
   */

  if( Tcl_GetChannel(xm_interp, xm_log_out, NULL) != NULL )
  {
        if( Tcl_Write(XM_Logfile_Chan, prompt, -1) < 0 || 
            Tcl_Write(XM_Logfile_Chan, buffer, -1) < 0 || 
            Tcl_Write(XM_Logfile_Chan, nl, -1) < 1 )
        {
                *ierr = Tcl_GetErrno();
                return TCL_OK;
        }
  }
  Tcl_ResetResult(xm_interp);

  /*
   * Check for the XPARSE EOF character
   */
  if( !strncmp(buffer, bk, strlen(bk)) )
  {
        *ierr = -1;
        return TCL_OK;
  }

  *ierr = 0;
  return TCL_OK;
}

/*
 * XANLIB replacements to route i/o through Tcl
 */

FCALLSCFUN2(INT,xm_tcl_write,FXWRITE,fxwrite,STRING,INT)

FCALLSCFUN3(INT,xm_tcl_read,XCREAD,xcread,STRING,PSTRING,PINT)

FCALLSCFUN3(INT,xm_tcl_read,XCREADH,xcreadh,STRING,PSTRING,PINT)

FCALLSCFUN3(INT,xm_tcl_read,XINIRD,xinird,STRING,PSTRING,PINT)
