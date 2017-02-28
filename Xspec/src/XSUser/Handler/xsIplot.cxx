//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h> 
#include <XSUser/Global/XSGlobal.h>  
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Error/Error.h>
#include <xsTypes.h>

int
XSGlobal::xsIplot(ClientData cdata,Tcl_Interp* tclInterp, int objc, Tcl_Obj* CONST objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doIplot(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doIplot(const StringArray& rawArgs)
{
   int status = 0;
   const char* cmd = "iplot";
   const size_t nArgs = rawArgs.size();
   try
   {
      if (nArgs > 1 && rawArgs[1] == "?")
      {
         printDocs(cmd,"?");
      }
      else
      {
           StringArray args(std::max(1,static_cast<int>(nArgs)-1),"");
           for (size_t j = 1; j < nArgs; ++j)
           {
              args[j-1] = rawArgs[j];              
           }
           XSGlobal::commonPlotHandler(args, true);
      }
   }
   catch (YellowAlert&)
   {
      status = -1;
   }
   return globalData->autoSave(status);
}
