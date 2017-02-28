//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUser/Help/Help.h>
#include <XSstreams.h>
#include <xsTypes.h>

int
XSGlobal::xsHelp(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doHelp(rawArgs);
   if (!status)
      return TCL_OK;
   else
      return TCL_ERROR;
}

int
XSGlobal::doHelp(const StringArray& rawArgs)
{
    const size_t nArgs = rawArgs.size();
    if (nArgs > 1)
    {
       if (rawArgs[1] == "??")
       {
          std::map<string,string>::iterator m (summaryMap.begin());
          std::map<string,string>::iterator mEnd (summaryMap.end());
          while ( m != mEnd )
          {
             tcout << m->first << " " << m->second << '\n';
             ++m;
          }
          tcout << std::flush;
          return 0;
       }
    }
    Help::helpTree()->execute(rawArgs, 0);
    return 0;
}
