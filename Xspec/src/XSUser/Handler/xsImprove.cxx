#include <xsTypes.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/FitMethod.h>        
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Error/Error.h>

int
XSGlobal::xsImprove(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doImprove(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doImprove(const StringArray& rawArgs)
{
   using namespace XSContainer;
   const char* cmd = "improve";
   if (rawArgs.size() == 2 && rawArgs[1] == "?")
   {
      printDocs(cmd,"?");
      return 0;
   }

   FitMethod* method = fit->fitMethod();
   if (!fit->isStillValid())
   {
      tcout << " A valid fit is required before improve command can be run."
         << std::endl;
      return -1;          
   }

   try
   {
      method->improve(fit);
      fit->report();
   }
   catch (YellowAlert&)
   {
   }
   return 0;
}
