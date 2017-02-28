//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUser/UserInterface/xstcl.h>
#include <iostream>
#include <cstdlib>

// TclRedAlert
#include <XSUser/UserInterface/TclRedAlert.h>
// This is declared w/ extern in xstcl.h
Tcl_Interp* interp = 0;


// Class TclRedAlert 

TclRedAlert::TclRedAlert (const std::string& message, int returnCode)
  : RedAlert(message)
{
  reportAndExit(message,returnCode);
}


void TclRedAlert::reportAndExit (const std::string& message, const int returnCode)
{
    // use the global tcl interp pointer to communicate with tcl,
  // and shut down xspec with the tcl exit command. The point is 
  // to delegate most of the cleanup work if any to the tcl library.

  // xspec renames tcl's exit command to tclexit. We don't want to
  // call the renamed version, which prompts the user whether they
  // actually want to exit, on a redAlert error. But we do need to
  // check whether the exception has been raised before or after the
  // renaming is done.
  std::cerr << "\n*** XSPEC Fatal Error " << message << std::endl;

  // check whether this exception is thrown before or after the tcl interface
  // has started. If before, simply print message and exit with a return code.
  // if after, use tcl's exit call to shut down. Need to check whether exit
  // has been renamed (to tclexit) or not, which is done in Xspec_Init.
  if ( interp )
  {
        Tcl_CmdInfo* infoPtr = new Tcl_CmdInfo;
        char* retStr = new char[4];
        const string ex = "tclexit ";
        sprintf(retStr,"%3d",returnCode);
        int status = Tcl_GetCommandInfo(interp, const_cast<char*>(ex.c_str()), infoPtr);

        string exitCommand = "";

        if (!status) exitCommand = ex  + string(retStr);
        else exitCommand = "exit " + string(retStr);

        Tcl_Eval(interp,const_cast<char*>(exitCommand.c_str())); 
   }
   else exit(returnCode); 
}

// Class TclInitErr 

TclInitErr::TclInitErr (const string& message, const int returnCode)
  : TclRedAlert(message,returnCode)
{
}

