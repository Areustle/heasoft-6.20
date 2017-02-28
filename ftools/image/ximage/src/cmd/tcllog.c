/*
 * Programs to allow for reading and writing of a log file which
 * will record all output from both XIMAGE and TCL.
 */
#include "../include/xmtcl.h"
#include "../include/maxvals.h"
 
#ifdef __cplusplus
extern "C" {
#endif
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

Tcl_Channel XM_Stdout_Chan;
Tcl_Channel XM_Stderr_Chan;
Tcl_Channel XM_Logfile_Chan;


char* xm_log_out = "LogOut";
char* xm_log_err = "ErrOut";


/*
 * Name for the TCL scripting channel.
 */
char *xm_script_file = (char *)NULL; 


/*
 * TCL Channel used for the script file.
 */
Tcl_Channel XM_Script_Chan;

#include <cfortran.h>
#ifdef __cplusplus
}
#endif

/*
 * xm_input_log -- Dummy input routine for data logging channel.
 */
int
xm_input_log(ClientData cdata,char * buf,int  bufsize,int * errcode )
{
        return TCL_OK;
}


/*
 * xm_close_log -- Procedure for closing the data logging channel.
 */
int
xm_close_log(ClientData cdata,Tcl_Interp * interp ) 
{
        return TCL_OK;
}


/*
 * xm_output_log -- 
 */
int xm_output_log(ClientData cdata, char* buf,int  bufsize,int* errcode )
{
  /*
   *  First write to the log file.
   */

  if( Tcl_Write(XM_Logfile_Chan, buf, bufsize) != bufsize )
  {
        *errcode = Tcl_GetErrno();
        return TCL_ERROR;
  }
  if( Tcl_Flush(XM_Logfile_Chan) != TCL_OK)
  {
        *errcode = Tcl_GetErrno();
        return TCL_ERROR;
  }
  /*
   *  Now write either to standard out or error, depending on outfile.
   */
  if( (char *)cdata == xm_log_out )
  {
        if( Tcl_Write(XM_Stdout_Chan, buf, bufsize) != bufsize )
        {
                *errcode = Tcl_GetErrno();
                return TCL_ERROR;
        }
        if( Tcl_Flush(XM_Stdout_Chan) != TCL_OK)
        {
                *errcode = Tcl_GetErrno();
                return TCL_ERROR;
        }

  }
  else if( (char *)cdata == xm_log_err )
  {
        if( Tcl_Write(XM_Stderr_Chan, buf, bufsize) != bufsize )
        {
                *errcode = Tcl_GetErrno();
                return TCL_ERROR;
        } 
        if( Tcl_Flush(XM_Stderr_Chan) != TCL_OK)
        {
                *errcode = Tcl_GetErrno();
                return TCL_ERROR;
        }
  }

  return bufsize;
}

/*
 * xm_watch_log --
 */
int xm_watch_log(ClientData cdata,int mask)
{
  int error=TCL_OK;
  if( mask | TCL_WRITABLE )
  {
        if( (char *)cdata == xm_log_out ) Tcl_NotifyChannel(XM_Stdout_Chan,mask);
        if( (char *)cdata == xm_log_err ) Tcl_NotifyChannel(XM_Stderr_Chan,mask);
        Tcl_NotifyChannel(XM_Logfile_Chan,mask);
  }

  return error;
}


/*
 * xm_handle_log --
 */
int xm_handle_log(ClientData cdata,int direct,ClientData* handler)
{
  if( (char *)cdata == xm_log_out )
  {
        return Tcl_GetChannelHandle(XM_Stdout_Chan, TCL_WRITABLE, handler);
  }
  else if ((char *)cdata == xm_log_err) 
  {
        return Tcl_GetChannelHandle(XM_Stderr_Chan, TCL_WRITABLE, handler);
  } 
  else return TCL_OK;
}


/*
 * xm_log --
 * Routine which sets everything up for writing a log file
 */
int xm_log(ClientData cdata,Tcl_Interp* interp,int objc,Tcl_Obj* CONST objv[] )
{
  Tcl_Channel Logging_Chan;
  char logfile[MAX_FILELEN];
  int objlen = 0;
  static Tcl_ChannelType *LogPtr;
  CONST char *logchanname;

  strcpy(logfile, "");

  if( objc > 1 )
  {
    /*
     * If none is requested then either close the current log file,
     * or if there is not one, tell the user they are being silly.
     */  
        if( !strcmp(Tcl_GetStringFromObj(objv[1],&objlen),"none") )
        {
                if( Tcl_GetChannel(interp, xm_log_out, NULL) == NULL )
                {
                        Tcl_SetResult(interp, "No log file currently active.", TCL_VOLATILE);
                        return TCL_OK;
                }
        }
        /*
        * Get the log file name if specified.
        */
        else
        {
                strcpy(logfile,Tcl_GetStringFromObj(objv[1],&objlen));
                if ( !strchr(logfile, '.') ) {
                      strcat(logfile, ".log");
                }
        }
  }
  else
  {
        strcpy(logfile,"ximage.log");
  }
  /*
   *  If logging enabled, close channel
   */
  if( Tcl_GetChannel(interp, xm_log_out, NULL) != NULL )
  {
        Logging_Chan = Tcl_GetChannel(interp, xm_log_out, NULL);
        Tcl_UnregisterChannel(interp, Logging_Chan);
        Logging_Chan = Tcl_GetChannel(interp, xm_log_err, NULL);
        Tcl_UnregisterChannel(interp, Logging_Chan);
        free(LogPtr);
        Tcl_SetStdChannel(XM_Stdout_Chan, TCL_STDOUT);
        Tcl_SetStdChannel(XM_Stderr_Chan, TCL_STDERR);
        Tcl_UnregisterChannel(interp, XM_Logfile_Chan);
        Tcl_UnsetVar(interp, "logchan", TCL_GLOBAL_ONLY);
  }
  /*
   *  If logfile is null, return
   */
  if ( !*logfile ) 
  {
        Tcl_SetResult(interp, "Logging disabled.", TCL_VOLATILE);
        return TCL_OK;
  }
  
  Tcl_ResetResult(interp);
  /*
   * Open the log file and register it.
   */
  XM_Logfile_Chan = Tcl_OpenFileChannel(interp, logfile, "w", 0664);
  if( XM_Logfile_Chan == NULL ) return TCL_ERROR;

  Tcl_RegisterChannel(interp, XM_Logfile_Chan);

  /*
   * Now create channel for writing to the log file and the
   * standard out simultaneously.
   */
  LogPtr = (Tcl_ChannelType *)malloc(sizeof(Tcl_ChannelType));

  LogPtr->typeName = (char *)NULL;
  LogPtr->blockModeProc = (Tcl_DriverBlockModeProc *)NULL;
  LogPtr->seekProc = (Tcl_DriverSeekProc *)NULL;
  LogPtr->setOptionProc = (Tcl_DriverSetOptionProc *)NULL;
  LogPtr->getOptionProc = (Tcl_DriverGetOptionProc *)NULL;

  LogPtr->closeProc = (Tcl_DriverCloseProc *)xm_close_log;
  LogPtr->inputProc = (Tcl_DriverInputProc *)xm_input_log;
  LogPtr->outputProc = (Tcl_DriverOutputProc *)xm_output_log;
  LogPtr->watchProc = (Tcl_DriverWatchProc *)xm_watch_log;
  LogPtr->getHandleProc = (Tcl_DriverGetHandleProc *)xm_handle_log;

  Logging_Chan = Tcl_CreateChannel( LogPtr, xm_log_out,  (ClientData)xm_log_out, TCL_WRITABLE);
  if( Logging_Chan == NULL ) return TCL_ERROR;

  Tcl_RegisterChannel(interp, Logging_Chan);

  if (Tcl_SetChannelOption(interp, Logging_Chan, "-buffering", "line") != TCL_OK ) return TCL_ERROR;

  /*
   * Now switch it around to be the standard channel for output,
   * after saving the value of the old channel so we can restore it.
   */
  XM_Stdout_Chan = Tcl_GetStdChannel(TCL_STDOUT);

  Tcl_SetStdChannel(Logging_Chan, TCL_STDOUT);

  /*
   * Now do the same for the standard error
   */

  Logging_Chan = Tcl_CreateChannel( LogPtr, xm_log_err,(ClientData)xm_log_err, TCL_WRITABLE);
  if( Logging_Chan == NULL ) return TCL_ERROR;

  Tcl_RegisterChannel(interp, Logging_Chan);

  if( Tcl_SetChannelOption(interp, Logging_Chan, "-buffering", "line") != TCL_OK ) return TCL_ERROR;

  XM_Stderr_Chan = Tcl_GetStdChannel(TCL_STDERR);

  Tcl_SetStdChannel(Logging_Chan, TCL_STDERR);

  /*
   * Finished
   */

  Tcl_AppendResult(interp, "Logging to file: ", logfile, (char *)NULL);
  logchanname = Tcl_GetChannelName(XM_Logfile_Chan);
  Tcl_SetVar(interp, "logchan", logchanname, TCL_GLOBAL_ONLY);

  return TCL_OK;
}


/*
 * xm_script --
 * Routine which sets everything up for writing a script file
 */
int xm_script(ClientData cdata,Tcl_Interp * interp,int  objc,Tcl_Obj* CONST objv[] )
{
  int objlen = 0;
  CONST char *scriptchanname;

  if( objc>1 && !strcmp(Tcl_GetStringFromObj(objv[1],&objlen),"none") )
  {
        /*
        * If none is requested then either close the current script file,
        * or if there is not one, tell the user they are being silly.
        */  
        if( xm_script_file == (char *)NULL )
        {
                Tcl_SetResult(interp, "No script file currently active.",TCL_VOLATILE);
        }
        else
        {
                Tcl_UnregisterChannel(interp, XM_Script_Chan);
                free(xm_script_file);
                xm_script_file = (char *)NULL;
                Tcl_UnsetVar(interp, "scriptchan", TCL_GLOBAL_ONLY);
        }
        return TCL_OK;
  }

  /*
   *  Make sure scripting isn't already enabled.
   */
  if( xm_script_file != (char *)NULL )
  {
        Tcl_SetResult(interp, "Script file is already active.", TCL_VOLATILE);
        return TCL_ERROR;
  }
  Tcl_ResetResult(interp);

  /*
   * Get the log file name if specified.
   */
  if( objc > 1 )
  {
        xm_script_file = (char *)malloc( (objlen + 1) * sizeof(char) );
        strcpy(xm_script_file,Tcl_GetStringFromObj(objv[1],&objlen));
  }
  else
  {
        xm_script_file = (char *)malloc( 10*sizeof(char) );
        strcpy(xm_script_file,"ximage.xco");
  }

  /*
   * Open the script file and register it.
   */
  XM_Script_Chan =  Tcl_OpenFileChannel(interp, xm_script_file, "w", 0664);
  if( XM_Script_Chan == NULL ) return TCL_ERROR;

  Tcl_RegisterChannel(interp, XM_Script_Chan);

  if( Tcl_SetChannelOption(interp, XM_Script_Chan, "-buffering", "line") != TCL_OK ) 
        return TCL_ERROR;

  /*
   * Finished
   */

  Tcl_AppendResult(interp, "Writing script file: ", xm_script_file,(char *)NULL);
  scriptchanname = Tcl_GetChannelName(XM_Script_Chan);
  Tcl_SetVar(interp, "scriptchan", scriptchanname, TCL_GLOBAL_ONLY);

  return TCL_OK;
}
