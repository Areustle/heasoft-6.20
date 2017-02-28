


/*
 * cfortran prototypes for the needed fortran calls.
 */


/*
 * Programs to cleanly exit from XSPEC. Re-written in C so we can do
 * things slightly different when running script given as arguments
 * to XSPEC.
 */
#define LOGICAL_STRICT

#include "XSUser/Global/Global.h"
#include "XSUser/Global/XSGlobal.h"
#include "XSUtil/Parse/XSparse.h"
#include "XSUtil/Utils/XSutility.h"
#include "XSContainer.h"
#include "XSstreams.h"
#include <iostream>

using namespace xstcl;


/*
 * xs_exit -- Exit xspec cleanly.
 */
int
XSGlobal::xsExit(ClientData cdata,Tcl_Interp* xsInterp,int objc, Tcl_Obj* CONST  objv[])
{
  int answer;
  Tcl_Channel OutChan;
  using namespace xstcl;
  /*
   * If we are not reading commands from a script, prompt for confirmation.
   */
   
  string userCmd(Tcl_GetString(objv[0]));
  if (userCmd == "quit")
  {
     // If user typed "exit", tclreadline::Exit will have already been called
     //  before it gets to here, and that would have written out the .hty file
     //  (also see scripts/xs_tclreadline.tcl).
     string flushHst("::tclreadline::readline write ");
     flushHst += globalData->userDir();
     flushHst += "/xspec.hty";
     Tcl_Eval(xsInterp, flushHst.c_str());
  } 
   
  char ti[] = "tcl_interactive";
  const char* tclInteractiveVar = Tcl_GetVar(xsInterp,ti,0);
  if ( XSparse::executingScript() || !tclInteractiveVar ||
                *tclInteractiveVar == '0' )
  {
        answer = 1;
  }
  else
  {
        answer = XSutility::yesToQuestion("Do you really want to exit? (y) ",1, tcin);
  }

  if (answer)
  {
        static char xq[] = " XSPEC: quit\n";
        static char ex[] = "tclexit";

	XSGlobal::cleanUp();
        OutChan = Tcl_GetStdChannel(TCL_STDOUT);
        Tcl_Write(OutChan,xq, -1);
        Tcl_Flush(OutChan);
        Tcl_Eval(xsInterp,ex);
        delete [] xs_tcl_prompt;
  }

  return globalData->autoSave(TCL_OK);
}
