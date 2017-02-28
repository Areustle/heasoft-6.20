/*
 * Programs to allow for reading and writing of a script file which
 * will record all commands from both XSPEC and TCL.
 */


#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>



namespace xstcl
{
/*
 * Name for the TCL scripting channel.
 */
        string xsScriptFile("");
        Tcl_Channel XS_Script_Chan;
}
        using namespace xstcl;





/*
 * xs_script --
 * Routine which sets everything up for writing a script file
 */
int XSGlobal::xsScript(ClientData cdata,Tcl_Interp * xsInterp,int  objc, Tcl_Obj* CONST objv[] )
{

  string firstArg("xspec.xcm");

  if( objc > 1 )
  {
        //
        // If none is entered then either close the current script file,
        // or if there is not one, tell the user they are being silly.
        //
        firstArg = string(Tcl_GetString(objv[1]));
        if ( firstArg[0] == '?')
        {
              printDocs(static_cast<char*>(cdata),"?");
              return globalData->autoSave(TCL_OK);         
        }
        else if ( firstArg == "none")
        { 
                if( xsScriptFile.length() == 0 )
                {
                        char nonActive[] = "No script file currently active.";
                        Tcl_SetResult(xsInterp,nonActive,TCL_VOLATILE);
                }
                else
                {
                        Tcl_UnregisterChannel(xsInterp, XS_Script_Chan);
                        Tcl_UnsetVar(xsInterp,"scriptchan",TCL_GLOBAL_ONLY);
                        XS_Script_Chan = static_cast<Tcl_Channel>(NULL);
                        xsScriptFile = "";
                }
                return globalData->autoSave(TCL_OK);
        }

  }

  //
  // Make sure scripting isn't already enabled.
  // ,TCL_GLOBAL_ONLY);
  if ( XS_Script_Chan != static_cast<Tcl_Channel>(NULL))
  {
          char active[] = "Script file is already active.";
          Tcl_SetResult(xsInterp, active, TCL_VOLATILE);
          Tcl_ResetResult(xsInterp);
          return globalData->autoSave(TCL_ERROR);       
  }
  else
  {
          xsScriptFile = firstArg;
          char w[]  = "w";
          char bfg[] = "-buffering"; 
          char ln[] = "line"; 
          const char* scr (xsScriptFile.c_str());
          XS_Script_Chan =  Tcl_OpenFileChannel(xsInterp, scr, w, 0664);
          if( XS_Script_Chan == NULL ) 
          {
                  Tcl_AppendResult(xsInterp,"Could not open script file:", scr, (char*)NULL);
                  return globalData->autoSave(TCL_ERROR);
          }
          else
          {
                Tcl_RegisterChannel(xsInterp, XS_Script_Chan);
                Tcl_AppendResult(xsInterp, "Writing script file: ", scr ,(char *)NULL);
                if( Tcl_SetChannelOption(xsInterp, XS_Script_Chan, bfg,ln) != globalData->autoSave(TCL_OK) ) 
                {
                        return globalData->autoSave(TCL_ERROR); 
                }
                const char* scriptchanname (Tcl_GetChannelName(XS_Script_Chan));
                Tcl_SetVar(xsInterp,"scriptchan",scriptchanname,TCL_GLOBAL_ONLY);
          }
  }             
  return globalData->autoSave(TCL_OK);

}

