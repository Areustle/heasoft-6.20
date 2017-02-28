//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// TclStream
#include <TclStream.h>
// xstcl
#include "xstcl.h"
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUtil/Utils/IosHolder.h>
#include "XSstreams.h"
#include "XSContainer.h"
#include <sstream>
#include <cstring>
//Tcl_ChannelType* xstcl::xsLogPtr = 0;


// Class TclIO 

TclIO::TclIO(const TclIO &right)
      : m_ioChannel(right.m_ioChannel),
        m_logChannel(right.m_logChannel),
        m_prompt(right.m_prompt)
{
}

TclIO::TclIO (Tcl_Channel channel)
      : m_ioChannel(channel),
        m_logChannel(0)
{
  //workaround for tcl8.4.4 bug: ensure that channel has linefeed not crlf
  Tcl_SetChannelOption(0,channel,"-translation","auto lf");
}


TclIO::~TclIO()
{
}


std::streamsize TclIO::read (char* s, std::streamsize n)
{


 // reimplemented 8/2003 using tclreadline module.
 // Call "tclreadline::readline read" with prompt " "
 // Based on code by Micah Johnson written for ximage, used with thanks. 8/27/03


  Tcl_Obj* cmdList (Tcl_NewObj());
  Tcl_Obj* tmpObj  (Tcl_NewStringObj("tclreadline::readline", -1));
  Tcl_ListObjAppendElement(interp, cmdList, tmpObj);

  tmpObj = Tcl_NewStringObj("read", -1);
  Tcl_ListObjAppendElement(interp, cmdList, tmpObj);

  tmpObj = Tcl_NewStringObj(m_prompt.c_str(), -1);
  Tcl_ListObjAppendElement(interp, cmdList, tmpObj);

  Tcl_EvalObjEx(interp, cmdList, TCL_EVAL_GLOBAL);
  Tcl_Obj* resObj (Tcl_GetObjResult(interp));
  Tcl_SetVar(interp, xstcl::xs_tcl_prompt, IosHolder::xsPrompt(),TCL_GLOBAL_ONLY);


  int mc (-1);
  std::streamsize chars (0);
  const char* TCLGET (Tcl_GetStringFromObj(resObj,&mc));

  if ( mc >= 0 )
  {
      chars = std::min(static_cast<std::streamsize>(mc),n-1);
      memcpy(s,TCLGET,chars);
      memset(s+chars,'\n',1);          
  }

  // test writing user input to log channel.
  if (m_logChannel)
  {
         static const char nl[] = "\n";
         string bangPrompt("!" + m_prompt);
         Tcl_Write(m_logChannel,bangPrompt.c_str(),-1);
         Tcl_Write(m_logChannel,TCLGET,-1);
         Tcl_Write(m_logChannel,nl,-1);
  }
  Tcl_ResetResult(interp);

  return chars + 1;
}

std::streamsize TclIO::write (const char* s, std::streamsize n)
{
  // require some handling here to catch the case where
  // output unsuccessful (nchars != n);
 std::streamsize nchars(n);
 int conTrivia = stream()->conVerbose();
 int logTrivia = stream()->logVerbose();

 // conTrivia is a measure of the "unimportance" of the string to
 // be written for console, and logTrivia for logging channel. This should
 // emulate the behavior of xwrite in xspec11.
 // strings are written to the channels whenever the string is 'less trivial'
 // than the chatter setting: so when the chatter setting is high, nothing
 // is trivial.

 // to make an output appear only for high chatter levels, raise its
 // triviality by using the XSstream::verbose call.


  if (conTrivia && (conTrivia <= stream()->consoleChatterLevel()) )      
  {      
        nchars = Tcl_WriteChars(ioChannel(),const_cast<char *>(s),n);
        Tcl_Flush(ioChannel());
  }
  if (logTrivia && logChannel() && XSGlobal::globalData->logging() 
       && logTrivia  <= stream()->logChatterLevel())
  {
        string ss(s,n);
        int nlPos = ss.find_first_of('\n');
        int nHash(0);
        while (nlPos >= 0)
        {
                ss = ss.insert(++nlPos,"#");
                ++nHash;
                nlPos = ss.find_first_of('\n',nlPos);       
        } 
//        if ( ss[ss.length()-1] == '#' )
//        {
//                ss += string("\n");
//                ++nHash;
//        }
        nlPos = Tcl_WriteChars(logChannel(),const_cast<char *>(ss.data()),n+nHash);
        //nchars = Tcl_WriteChars(logChannel(),s,n);
        Tcl_Flush(logChannel());
  }

  return nchars;
}

void TclIO::internalSetLogger (const std::string& name, bool isErr)
{
  using namespace xstcl;
  static char bfg[] = "-buffering";
  static char ln[] = "line";

  if (!isErr)
  {
     /*
      * Open the log file and register it.
      */

     char* lf = const_cast<char*>(name.c_str());

     char w[] = "w"; 
     if( (m_logChannel = Tcl_OpenFileChannel(interp, lf, w, 0664) )== 0 ) 
     {
             throw LogFileOpenFailure(name);
     }
     else
     {
           // the global symbol XS_Logfile_Chan is used in xsOutput.
           XS_Logfile_Chan = m_logChannel;
           // the need to register this is unclear, since tcl doesn't ever
           // directly write to the open file channel - it writes to
           // the logging channels through xsOutputLog which writes
           // to this file channel. Not the same thing.
           // Tcl_RegisterChannel(interp,m_logChannel);     
     }
     /*
     * Create the channel getting input from gnu readline.
     * Updated B.Dorman. 11/98 for compatibility with tcl8.0 
     *
     * Channel drivers revised in tcl8.3.2 8/2000.
     *
     */
     xsLogPtr = new Tcl_ChannelType;
     char typeN[] = "file";
     xsLogPtr->typeName = typeN;
     xsLogPtr->version  = (Tcl_ChannelTypeVersion) TCL_CHANNEL_VERSION_2;
     xsLogPtr->closeProc = (Tcl_DriverCloseProc *)xsCloseLog;
     xsLogPtr->inputProc = (Tcl_DriverInputProc *)xsInputLog;
     xsLogPtr->outputProc = (Tcl_DriverOutputProc *)xsOutputLog;
     xsLogPtr->seekProc = (Tcl_DriverSeekProc *)NULL;
     xsLogPtr->setOptionProc = (Tcl_DriverSetOptionProc *)NULL;
     xsLogPtr->getOptionProc = (Tcl_DriverGetOptionProc *)NULL;
     xsLogPtr->watchProc = (Tcl_DriverWatchProc *)xsWatchLog;
     xsLogPtr->getHandleProc = (Tcl_DriverGetHandleProc *)xsHandleLog;
     xsLogPtr->close2Proc = (Tcl_DriverClose2Proc *)NULL;                
     xsLogPtr->blockModeProc = (Tcl_DriverBlockModeProc *)NULL;
     xsLogPtr->flushProc     = (Tcl_DriverFlushProc *)NULL;
     xsLogPtr->handlerProc     = (Tcl_DriverHandlerProc *)NULL;


     Tcl_Channel 
          Logging_Chan(Tcl_CreateChannel( xsLogPtr, xsLogOut,  (ClientData)xsLogOut, TCL_WRITABLE));

     Tcl_SetVar(interp,"logchan",xsLogOut,TCL_GLOBAL_ONLY);
     Tcl_RegisterChannel(interp, Logging_Chan);

     if ( Tcl_SetChannelOption(interp, Logging_Chan, bfg, ln) != TCL_OK ) 
             throw LogFileOpenFailure(string(" setting channel options "));


     /*
      * Now switch it around to be the standard channel for output,
      * after saving the value of the old channel so we can restore it.
      */
     XS_Stdout_Chan = Tcl_GetStdChannel(TCL_STDOUT);

     Tcl_SetStdChannel(Logging_Chan, TCL_STDOUT);

     /*
      * Now do the same for the standard error
      */

     Logging_Chan = Tcl_CreateChannel( xsLogPtr, xsLogErr,(ClientData)xsLogErr, TCL_WRITABLE);

     Tcl_RegisterChannel(interp, Logging_Chan);

     // check- stderr is not buffered
     if ( Tcl_SetChannelOption(interp, Logging_Chan, bfg, ln) != TCL_OK ) 
             throw LogFileOpenFailure(string(" setting channel options "));

     //assert( Tcl_SetChannelOption(xsInterp, Logging_Chan, bfg, ln) == TCL_OK );

     XS_Stderr_Chan = Tcl_GetStdChannel(TCL_STDERR);

     Tcl_SetStdChannel(Logging_Chan, TCL_STDERR);

     Tcl_AppendResult(interp, "\nLogging to file: ", lf, (char *)NULL);
  }
  else
  {
     TclIO* tpoutChan = dynamic_cast<TclIO*>(tpout.getChannel());
     if (tpoutChan)
     {
        m_logChannel = tpoutChan->m_logChannel;
     }
  }
}

void TclIO::internalCloseLog ()
{
   using namespace xstcl;
   //ask tcl whether there is a log file open.
   static char notActive[] = "log file is not open";
   static char disabled[] = "logging switched off";   
   if( m_logChannel == 0 )
   {
           Tcl_SetResult(interp,notActive, TCL_VOLATILE);
   }
   else
   {
           // Tcl_UnregisterChannel(interp, m_logChannel);
           // also unregister stderr's direction to log file
           Tcl_Channel Logging_Chan(Tcl_GetChannel(interp, xsLogErr, 0));
           Tcl_UnregisterChannel(interp, Logging_Chan);
           Logging_Chan = Tcl_GetChannel(interp, xsLogOut, 0);
           Tcl_UnregisterChannel(interp, Logging_Chan);
           Tcl_SetResult(interp,disabled, TCL_VOLATILE);
           Tcl_SetStdChannel(XS_Stdout_Chan, TCL_STDOUT);
           Tcl_SetStdChannel(XS_Stderr_Chan, TCL_STDERR);
           Tcl_UnsetVar(interp,"logchan",TCL_GLOBAL_ONLY);
           XSGlobal::globalData->logging(false);
           delete xsLogPtr;
           xsLogPtr = 0;   
   } 
   TclIO* tperrChan = dynamic_cast<TclIO*>(tperr.getChannel());
   if (tperrChan)
   {
      tperrChan->m_logChannel = 0;
   }
}

void TclIO::setPrompt (const string& ps)
{
  m_prompt = ps;
}

// Additional Declarations

// Class TkIO 

TkIO::TkIO(const TkIO &right)
      : m_window(right.m_window),
        m_logChannel(right.m_logChannel)
{


  // I don't really think we should need a copy constructor anyway.
}

TkIO::TkIO (char* winName)
      : m_window(winName),
        m_logChannel(0)
{
}


TkIO::~TkIO()
{
}


std::streamsize TkIO::read (char* s, std::streamsize n)
{
  // note that s is  a pointer to the streambuf
  // internal buffer, and n is the size of that buffer.
  // prompt() is a string that contains either

  // a) a prompt string
  // b) a command that returns a string back to the stream.

  // prompt has a default set by the tk procedure.
  // BY CONVENTION in this code, a prompt string ends in ":" or ">".

  static char tkGet[] = "XSgetbox ";
  const size_t MAXCHAR = 256;
  static char* charPrompt = new char[MAXCHAR+1];

  charPrompt[MAXCHAR] = 0;
  int lastNonBlank = 0;
  int findZero = 0;
  if (prompt().length() > 0)
  {
        lastNonBlank = prompt().find_last_not_of(' ');
        if (  strchr(":>",m_prompt[lastNonBlank]) != 0 )
        {
       // The prompt string  ends in ':' or ">",
       // so it's a string literal prompt. add it to the "XSgetbox" command.
	        strncpy(charPrompt,tkGet,MAXCHAR);
                if ((strlen(charPrompt)+prompt().length()) <=MAXCHAR)
	           strcat(charPrompt,prompt().c_str());
        }
        // it's a script name with arguments.
        else 
           strncpy(charPrompt,prompt().c_str(),MAXCHAR);
  }
  else 
  {
        findZero = -1;
        strncpy(charPrompt,tkGet,MAXCHAR); 
  } 


  int status = Tcl_Eval(interp,charPrompt);
  if (!status)
  {
        strcpy(s,Tcl_GetStringResult(interp));   
	strcat(s,"\n");
        return strlen(s);
  }
  else return -1;
}

std::streamsize TkIO::write (const char* s, std::streamsize n)
{
  static char insert[] = " insert end {" ;
  static char end[] = "} \n";
  static char see[] = " see end\n";
  unsigned int nw = strlen(m_window);
  unsigned int lenStr = nw + n + 16;
  char* outStr = new char[lenStr+1];
  memcpy(outStr,m_window,nw);
  memcpy(outStr+nw,insert,13);
  memcpy(outStr+nw + 13,s,n);
  memcpy(outStr+nw + 13 + n,end,3);
  outStr[lenStr] = '\0';
  Tcl_EvalEx(interp,outStr,lenStr+1,0);
  strcat(strcpy(outStr,m_window),see); 
  Tcl_EvalEx(interp,outStr,strlen(outStr) + 1,0);
  delete [] outStr;

  return n;
}

void TkIO::setPrompt (const std::string& ps)
{
  m_prompt = ps;
}

void TkIO::prompts (const std::string& script, const std::vector<std::string>& prompts, const std::vector<std::string>& infos)
{
}

void TkIO::internalSetLogger (const std::string& name, bool isErr)
{
  using namespace xstcl;
  static char bfg[] = "-buffering";
  static char ln[] = "line";

  /*
   * Open the log file and register it.
   */

  char* lf = const_cast<char*>(name.c_str());

  char w[] = "w"; 
  if( (m_logChannel = Tcl_OpenFileChannel(interp, lf, w, 0664) )== 0 ) 
  {
          throw LogFileOpenFailure(name);
  }
  else
  {
        // the global symbol XS_Logfile_Chan is used in xsOutput.
        XS_Logfile_Chan = m_logChannel;
        // the need to register this is unclear, since tcl doesn't ever
        // directly write to the open file channel - it writes to
        // the logging channels through xsOutputLog which writes
        // to this file channel. Not the same thing.
        // Tcl_RegisterChannel(interp,m_logChannel);     
  }
  /*
  * Create the channel getting input from gnu readline.
  * Updated B.Dorman. 11/98 for compatibility with tcl8.0 
  *
  * Channel drivers revised in tcl8.3.2 8/2000.
  *
  */
  xsLogPtr = new Tcl_ChannelType;
  char typeN[] = "file";
  xsLogPtr->typeName = typeN;
  xsLogPtr->version  = (Tcl_ChannelTypeVersion) TCL_CHANNEL_VERSION_2;
  xsLogPtr->closeProc = (Tcl_DriverCloseProc *)xsCloseLog;
  xsLogPtr->inputProc = (Tcl_DriverInputProc *)xsInputLog;
  xsLogPtr->outputProc = (Tcl_DriverOutputProc *)xsOutputLog;
  xsLogPtr->seekProc = (Tcl_DriverSeekProc *)NULL;
  xsLogPtr->setOptionProc = (Tcl_DriverSetOptionProc *)NULL;
  xsLogPtr->getOptionProc = (Tcl_DriverGetOptionProc *)NULL;
  xsLogPtr->watchProc = (Tcl_DriverWatchProc *)xsWatchLog;
  xsLogPtr->getHandleProc = (Tcl_DriverGetHandleProc *)xsHandleLog;
  xsLogPtr->close2Proc = (Tcl_DriverClose2Proc *)NULL;                
  xsLogPtr->blockModeProc = (Tcl_DriverBlockModeProc *)NULL;
  xsLogPtr->flushProc     = (Tcl_DriverFlushProc *)NULL;
  xsLogPtr->handlerProc     = (Tcl_DriverHandlerProc *)NULL;


  Tcl_Channel 
       Logging_Chan(Tcl_CreateChannel( xsLogPtr, xsLogOut,  (ClientData)xsLogOut, TCL_WRITABLE));

  Tcl_RegisterChannel(interp, Logging_Chan);

  if ( Tcl_SetChannelOption(interp, Logging_Chan, bfg, ln) != TCL_OK ) 
          throw LogFileOpenFailure(string(" setting channel options "));


  /*
   * Now switch it around to be the standard channel for output,
   * after saving the value of the old channel so we can restore it.
   */
  XS_Stdout_Chan = Tcl_GetStdChannel(TCL_STDOUT);

  Tcl_SetStdChannel(Logging_Chan, TCL_STDOUT);

  /*
   * Now do the same for the standard error
   */

  Logging_Chan = Tcl_CreateChannel( xsLogPtr, xsLogErr,(ClientData)xsLogErr, TCL_WRITABLE);

  Tcl_RegisterChannel(interp, Logging_Chan);

  // check- stderr is not buffered
  if ( Tcl_SetChannelOption(interp, Logging_Chan, bfg, ln) != TCL_OK ) 
          throw LogFileOpenFailure(string(" setting channel options "));

  //assert( Tcl_SetChannelOption(xsInterp, Logging_Chan, bfg, ln) == TCL_OK );

  XS_Stderr_Chan = Tcl_GetStdChannel(TCL_STDERR);

  Tcl_SetStdChannel(Logging_Chan, TCL_STDERR);

  Tcl_AppendResult(interp, "\nLogging to file: ", lf, (char *)NULL);

}

void TkIO::internalCloseLog ()
{
   using namespace xstcl;
   //ask tcl whether there is a log file open.
   static char notActive[] = "log file is not open";
   static char disabled[] = "logging switched off";   
   if( m_logChannel == 0 )
   {
           Tcl_SetResult(interp,notActive, TCL_VOLATILE);
   }
   else
   {
           // Tcl_UnregisterChannel(interp, m_logChannel);
           // also unregister stderr's direction to log file
           Tcl_Channel Logging_Chan(Tcl_GetChannel(interp, xsLogErr, 0));
           Tcl_UnregisterChannel(interp, Logging_Chan);
           Logging_Chan = Tcl_GetChannel(interp, xsLogOut, 0);
           Tcl_UnregisterChannel(interp, Logging_Chan);
           Tcl_SetResult(interp,disabled, TCL_VOLATILE);
           Tcl_SetStdChannel(XS_Stdout_Chan, TCL_STDOUT);
           Tcl_SetStdChannel(XS_Stderr_Chan, TCL_STDERR);
           XSGlobal::globalData->logging(false);
           delete xsLogPtr;
           xsLogPtr = 0;
   }    
}

// Additional Declarations
