/*
 * Programs to allow for reading and writing of a log file which
 * will record all output from both XSPEC and TCL.
 */



#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Global/Global.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Utils/IosHolder.h>
#include <XSContainer.h>
#include <XSstreams.h>


Tcl_Channel xstcl::XS_Logfile_Chan;
Tcl_Channel xstcl::XS_Stdout_Chan;
Tcl_Channel xstcl::XS_Stderr_Chan;

#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cerrno>
#include <cassert>
#include <ctime>

/*
 * xs_input_log -- Dummy input routine for data logging channel.
 */
int
xstcl::xsInputLog(ClientData cdata,char * buf,int  bufsize,int * errcode )
{
        return TCL_OK;
}


/*
 * xs_close_log -- Procedure for closing the data logging channel.
 */
int
xstcl::xsCloseLog(ClientData cdata,Tcl_Interp * xsInterp )
{
        return TCL_OK;
}


/*
 * xs_output_log --
 */
int 
xstcl::xsOutputLog(ClientData cdata, char* buf,int  bufsize,int* errcode )
{

  if (bufsize <= 0)
     return bufsize;

  string testPrompt(buf, bufsize);
  const string cmdPrompt(string("!") + string(IosHolder::xsPrompt()));
  if (testPrompt.find(cmdPrompt) == 0)
  {
     // This is coming from an interactive command entry.
     // Add a newline so that it doesn't begin with a
     // previously leftover '#'. 
     char nl = '\n'; 
     if( Tcl_Write(XS_Logfile_Chan, &nl, 1) != 1 )
     {
           *errcode = Tcl_GetErrno();
           return TCL_ERROR;
     }

  }

  if( Tcl_Write(XS_Logfile_Chan, buf, bufsize) != bufsize )
  {
        *errcode = Tcl_GetErrno();
        return TCL_ERROR;
  }
  if (buf[bufsize-1] == '\n')
  {
     char pound = '#';
     if( Tcl_Write(XS_Logfile_Chan, &pound, 1) != 1 )
     {
           *errcode = Tcl_GetErrno();
           return TCL_ERROR;
     }
  }
  if( Tcl_Flush(XS_Logfile_Chan) != TCL_OK)
  {
        *errcode = Tcl_GetErrno();
        return TCL_ERROR;
  }
  /*
   *  Now write either to stadard out or error, depending on oufile.
   */
  if( (char *)cdata == xsLogOut )
  {
     if (XSparse::executingScript() || (bufsize && buf[0] != '!'))
     {
        if( Tcl_Write(XS_Stdout_Chan, buf, bufsize) != bufsize )
        {
                *errcode = Tcl_GetErrno();
                return TCL_ERROR;
        }
        if( Tcl_Flush(XS_Stdout_Chan) != TCL_OK)
        {
                *errcode = Tcl_GetErrno();
                return TCL_ERROR;
        }
     }
  }
  else if( (char *)cdata == xsLogErr )
  {
        if( Tcl_Write(XS_Stderr_Chan, buf, bufsize) != bufsize )
        {
                *errcode = Tcl_GetErrno();
                return TCL_ERROR;
        } 
        if( Tcl_Flush(XS_Stderr_Chan) != TCL_OK)
        {
                *errcode = Tcl_GetErrno();
                return TCL_ERROR;
        }
  }
  return bufsize;
}

/*
 * xs_watch_log --
 */
void 
xstcl::xsWatchLog(ClientData cdata,int mask)
{
        if( mask | TCL_WRITABLE )
        {
                if( (char *)cdata == xsLogOut ) Tcl_NotifyChannel(XS_Stdout_Chan,mask);
                if( (char *)cdata == xsLogErr ) Tcl_NotifyChannel(XS_Stderr_Chan,mask);
                Tcl_NotifyChannel(XS_Logfile_Chan,mask);
        }

}


/*
 * xs_handle_log --
 */
int 
xstcl::xsHandleLog(ClientData cdata,int direct,ClientData* handler)
{
  if( (char *)cdata == xsLogOut )
  {
        return Tcl_GetChannelHandle(XS_Stdout_Chan, TCL_WRITABLE, handler);
  }
  else if ((char *)cdata == xsLogErr)
  {
        return Tcl_GetChannelHandle(XS_Stderr_Chan, TCL_WRITABLE, handler);
  }
  else return TCL_OK;
}


/*
 * xs_log --
 * Routine which sets everything up for writing a log file
 */
int 
XSGlobal::xsLog(ClientData cdata,Tcl_Interp* xsInterp,int objc,Tcl_Obj *CONST objv[] )
{
  string logFile("xspec.log");

  if( objc > 1 )
  {
    /*
     * If none is requested then either close the current log file,
     * or if there is not one, tell the user they are being silly.
     */
     string firstArg(Tcl_GetString(objv[1]));
     if(XSutility::lowerCase(firstArg) == "none" )
     {
        try 
        {
           if (XSGlobal::globalData->logging())
           {
              tpout.closeLog();
              tpout << "Log file closed" << std::endl;
           }
           else
           {
              tpout << "No log file open" << std::endl;       
           }
           return globalData->autoSave(TCL_OK);
        }
        catch (...)
        {
           return globalData->autoSave(TCL_ERROR);       
        }
     }
     else if (XSutility::lowerCase(firstArg) == "stamp")
     {
        time_t calTime;
        time(&calTime);
        string timeStamp(ctime(&calTime));
        timeStamp = '_' + timeStamp;
        // Remove the trailing '/n'
        timeStamp.erase(timeStamp.length()-1);
        string::size_type loc = timeStamp.find(' ');
        while (loc != string::npos)
        {
           timeStamp.replace(loc,1,1,'_');
           loc = timeStamp.find(' ',loc+1);
        }
        if (objc == 2)
        {
           logFile = "xspec" + timeStamp + ".log";
        }
        else
        {
           string fileSuffix(Tcl_GetString(objv[2]));
           string::size_type dotLoc = fileSuffix.find_last_of('.');
           logFile = fileSuffix + timeStamp;
           if (dotLoc == string::npos)
           {
              logFile += ".log";
           }
        }
     }
     else logFile = Tcl_GetString(objv[1]);

  }
  /*
   *  Make sure logging isn't already enabled.
   */
  if( XSGlobal::globalData->logging() )
  {
        static char active[]  = "\nLog file is already open.";
        Tcl_SetResult(xsInterp,active, TCL_VOLATILE);
        return globalData->autoSave(TCL_ERROR);
  }

  Tcl_ResetResult(xsInterp);

  try 
  {
        tpout.setLogger(logFile, false);
        tperr.setLogger(logFile, true);

        tpout << "Logging to file:" << logFile << std::endl;

        XSGlobal::globalData->logging(true);  
        // For some reason this reset is required, else it
        // will print out "Logging to file" a second time.  I
        // can't quite figure out what Tcl is doing here.
        Tcl_ResetResult(xsInterp);

	string title, buildDate;
	XSutility::XSVersionString(title, buildDate);
	int chatter(tpout.consoleChatterLevel());
	tpout.consoleChatterLevel(0);
	tpout << "#" << title << std::endl;
	tpout << buildDate << std::endl;
	tpout.consoleChatterLevel(chatter);

        return globalData->autoSave(TCL_OK);
  }
  catch (...)
  {
        XSGlobal::globalData->logging(false);

        return globalData->autoSave(TCL_ERROR);  

  }

}

