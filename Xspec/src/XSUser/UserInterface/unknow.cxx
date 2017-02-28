#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSstreams.h>  // for streamio
#include <XSContainer.h>  // for streamio

#include <cstdlib> // for system call.
#include <cstring>

using namespace xstcl;

/*
 * xs_unknown --
 *   Handling for unknown commands. Anything beginning in '@' is assumed
 *   to be the name of a script, and execution of the file as a script is
 *   attempted. Anything else gets passed to tcl's standard unknown handling
 *   function, assuming there is one [unknown is renamed to xstcl_unknown by
 *   the startup procedure Xspec_Init]. If for some reason there isn't, nothing
 *   is executed at all.
 */
int
XSGlobal::xsUnknown(ClientData cdata,Tcl_Interp * xsInterp,int objc, Tcl_Obj *CONST  objv[] )
{
  int result = TCL_OK;
  int length = 0;
  /*
   * Put all the arguments together into one string for processing.
   */
   
  // This can happen if user enters "unknown" for some reason. 
  if (objc < 2)
     return TCL_OK;
     
  char* command = Tcl_GetStringFromObj(objv[1],&length);


  if ( strchr("@?" , *command ) != 0 && strlen(command) != 0)
  {
     if ( *command == '@')
        return xs_execute_script(xsInterp,command);
     else 
     {
        int len = strlen(command);
        if (len > 1)
        {
           // User entered "?<command>".  Assume they want a summary for
           // <command>, so just pass it directly to doComsum.
           string actualCmd(command+1);
           StringArray comsumArgs(2);
           comsumArgs[1] = actualCmd;           
           int retval = doComsum(comsumArgs);
           result = retval ? TCL_ERROR : TCL_OK;
        }
     } 
  }   
  /*
  * Okay, so now try feeding it to the standard Tcl command and see if
  * it can process it.
  */
  else if (strlen(command) == 0) result = TCL_OK;
  else
  {
     if (!XSGlobal::globalData->gui())
     {      
        string cmdStr(command);
        // NOTE: In Hera, calling tclunknown doesn't seem to have the desired 
        // effect of expanding command abbreviations.  This could be 
        // because Hera is a safe interpreter and therefore doesn't
        // implement a default unknown routine, but I'm not an expert
        // on this.  So, instead use this technique based on Micah
        // Johnson's code in ximage, which relies on "info commands" to
        // deal with abbreviations.  -CG 2/06

        //...but first, check if this is a system command.  "info
        // commands" won't find these, unlike "tclunknown".
        bool isFound = false;
        string testCom("auto_execok ");
        testCom += cmdStr;
        int saveEcho = xs_noecho_command;
        xs_noecho_command = 1;
        result = Tcl_EvalEx(xsInterp, testCom.c_str(), -1, 0);
        xs_noecho_command = saveEcho;
        if (result == TCL_OK)
        {
           Tcl_Obj* syscomObj = Tcl_GetObjResult(xsInterp);
           string syscomStr(Tcl_GetStringFromObj(syscomObj, 0));
           if (syscomStr.length())
           {
              isFound = true;
              for (int i = 2; i < objc; i++)
              {
                 cmdStr += string(" ") + Tcl_GetStringFromObj(objv[i],&length);
              }
              std::system(cmdStr.c_str());
           }
        }

        if (!isFound)
        {
           testCom = "info commands ";
           testCom += cmdStr + "*";
           xs_noecho_command = 1;
           result = Tcl_EvalEx(xsInterp, testCom.c_str(), -1, 0);
           xs_noecho_command = saveEcho;
           if (result == TCL_OK)
           {
              Tcl_Obj* abbrObj = Tcl_GetObjResult(xsInterp);
              Tcl_Obj** oargv = 0;
              int oargc = 0;
              // If abbrObj doesn't point to a list object, the following 
              // function will try to convert it to one.  After the call,
              // oargc will be the number of commands consistent with
              // the abbreviation.
              Tcl_ListObjGetElements(xsInterp, abbrObj, &oargc, &oargv);
              if (oargc == 1)
              {
                 // Success - an unambiguous match.  Replace abbreviated
                 // command string with full command.
                 cmdStr = string(Tcl_GetStringFromObj(oargv[0], &length));
                 for (int i = 2; i < objc; i++)
                 {
                    cmdStr += string(" ") + Tcl_GetStringFromObj(objv[i],&length);
                 }
                 Tcl_Obj* fullCmdObj = Tcl_NewStringObj(cmdStr.c_str(), cmdStr.length());
                 Tcl_IncrRefCount(fullCmdObj);
                 result = Tcl_EvalObj(xsInterp, fullCmdObj);
                 Tcl_DecrRefCount(fullCmdObj);
                 isFound = true;
              } 
           }
        }

        if (!isFound)
        {
           // If we're here, either command not found at all or it's
           // an ambiguous abbreviation.  Either way, send it to default
           // tcl unknown which will report the ambiguous case (except
           // on Hera).
           static char tclUnknown[] = "tclunknown";
           string unknownCmd("");
           for (int i = 2; i < objc; i++)
           {
                   cmdStr += string(" ") + Tcl_GetStringFromObj(objv[i],&length);
           }
           Tcl_CmdInfo *infoPtr = new Tcl_CmdInfo;
           if( Tcl_GetCommandInfo(xsInterp, tclUnknown, infoPtr) )
           {
                   unknownCmd = string(tclUnknown) + " " + cmdStr;
                   Tcl_Obj* unknown =
                      Tcl_NewStringObj(const_cast<char*>(unknownCmd.c_str()),
                            unknownCmd.length());
                   result = Tcl_EvalObj(xsInterp, unknown);
           }       
           delete infoPtr;
        }
     } // end if not gui
     // this is supposed to keep the tkcon unknown handler happy 
     else  result = TCL_CONTINUE;
  }

  return result;
}
